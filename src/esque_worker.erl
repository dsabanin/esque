%%%-------------------------------------------------------------------
%%% @author manuel@whisper.sh/chad@whisper.sh
%%% @copyright (C) 2013 WhisperText Inc 
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(esque_worker).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/1,
    noop/1,
    work_and_retry/6,
    work_and_track_failures/5
  ]).

-define(DEFAULT_WORKERS, 1).
-define(TIMER_HISTORY_COUNT, 100).

-record(state, {sub          :: pid(),
                queue_key    :: list(),
                ctl_key      :: atom(),
                module       :: atom(),
                method       :: atom(),
                queue_params :: list()
               }).

-include("queue.hrl").

-spec noop(term()) -> ok.
noop(_) ->
  ok.

start_link(QueueParams) ->
  gen_server:start_link({local, list_to_atom(?QUEUE_WORKER_PREFIX ++ ?QUEUE_NAME(QueueParams))}, ?MODULE, QueueParams, []).

init(QueueParams) ->
  QueueName = ?QUEUE_NAME(QueueParams),
  QueueKey = ?QUEUE_KEY(QueueName),
  SubKey = ?QUEUE_SUB_KEY(QueueName),
  CtlKey = list_to_atom(QueueKey),
  lager:debug("esque: starting worker for queue ~p", [QueueName]),
  {Module, Method} = proplists:get_value(action, QueueParams, {esque_worker, noop}),
  MaxConcurrentWorkers = proplists:get_value(workers, QueueParams, ?DEFAULT_WORKERS),
  cxy_ctl:init([{CtlKey, MaxConcurrentWorkers, ?TIMER_HISTORY_COUNT}]),

  {ok, Config} = application:get_env(esque, redis_pool),
  R = proplists:get_value(worker_config, Config),
  [Host, Port, Password] = [ proplists:get_value(X, R) || X <- [host, port, pwd]],
  {ok, Sub} = eredis_sub:start_link(Host, Port, Password),
  start_subscription(Sub, SubKey),

  {ok, #state{sub = Sub, queue_key = QueueKey, ctl_key = CtlKey, module = Module, method = Method, queue_params = QueueParams}, 0}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State = #state{ ctl_key = CtlKey, queue_key = QueueKey, module = Module, method = Method, queue_params = QueueParams }) ->
  Wait = pull_and_work(CtlKey, QueueKey, Module, Method, QueueParams),
  {noreply, State, Wait};
handle_info({eredis_disconnected, Client}, #state{ queue_key = QueueKey} = State) ->
  lager:debug("esque worker ~s lost connection to redis", [QueueKey]),
  eredis_sub:ack_message(Client),
  {noreply, State, ?IDLE_PERIOD_MS};
handle_info({eredis_connected, Client}, #state{ queue_key = QueueKey} = State) ->
  lager:debug("esque worker ~s regained connection to redis", [QueueKey]),
  eredis_sub:ack_message(Client),
  {noreply, State, ?IDLE_PERIOD_MS};
handle_info({subscribed, Channel, Client}, #state{ queue_key = QueueKey} = State) ->
  lager:debug("esque worker ~s subscribed to redis channel ~s", [QueueKey, Channel]),
  eredis_sub:ack_message(Client),
  {noreply, State, ?IDLE_PERIOD_MS};
handle_info({message, Channel, Message, Client}, #state{ ctl_key = CtlKey, queue_key = QueueKey, module = Module, method = Method, queue_params = QueueParams} = State) ->
  lager:debug("esque worker ~s message ~p from channel ~s ", [QueueKey, Message, Channel]),
  Wait = pull_and_work(CtlKey, QueueKey, Module, Method, QueueParams),
  eredis_sub:ack_message(Client),
  {noreply, State, Wait};
handle_info({dropped, NumMessages, Client}, #state{ ctl_key = CtlKey, queue_key = QueueKey, module = Module, method = Method, queue_params = QueueParams} = State) ->
  lager:debug("esque worker ~s lost ~s messages due to volume from redis", [QueueKey, NumMessages]),
  Wait = pull_and_work(CtlKey, QueueKey, Module, Method, QueueParams),
  eredis_sub:ack_message(Client),
  {noreply, State, Wait}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State, ?IDLE_PERIOD_MS}.

pull_and_work(CtlKey, QueueKey, Module, Method, Options) ->
  {ok, Result} = esque_redis_worker:q("lpop", [QueueKey]),
  case Result of
    undefined -> ?IDLE_PERIOD_MS;
    Binary ->
      Item = esque:deserialize(Binary),
      RetryOptions = proplists:get_value(retry_options, Options,[]),
      case {RetryOptions,proplists:get_value(retry_unless, RetryOptions)} of
        {[], _} ->  cxy_ctl:execute_task(CtlKey, ?MODULE, work_and_track_failures, [Module, Method, Item, QueueKey, Binary]);
        {_List, Expect} -> cxy_ctl:execute_task(CtlKey, ?MODULE, work_and_retry, [Module, Method, Item, Expect, QueueKey, Binary])
      end, 0
  end.


work_and_track_failures(Module, Method, Args, QueueKey, Binary) ->
  try 
    erlang:apply(Module, Method, Args) 
  catch 
    _:Exception ->
      StackTrace = erlang:get_stacktrace(),
      FailedQueueKey = ?QUEUE_KEY_FAILED(QueueKey),
      lager:error("~p:~p(~p) on queue ~p. got exception ~p and stacktrace ~p", [Module, Method, Args, QueueKey, Exception, StackTrace]),
      FailurePayload = [{params,Binary},{exception,Exception},{stacttrace,StackTrace}],
      esque_redis_worker:q("lpush", [FailedQueueKey, term_to_binary(FailurePayload)])
  end.


%% cty_ctl will call us
work_and_retry(Module, Method, Args, Expect, QueueKey, Binary) -> 
  case erlang:apply(Module, Method, Args) of
    Expect -> ok;
    Other -> lager:error("~p:~p(~p) expected ~p, got ~p, retrying.", [Module, Method, Args, Expect, Other]),
      {ok, _} = esque_redis_worker:q("lpush", [QueueKey, Binary])
  end.

start_subscription(Sub, SubName) ->
  eredis_sub:controlling_process(Sub, self()),
  eredis_sub:subscribe(Sub, [SubName]).
