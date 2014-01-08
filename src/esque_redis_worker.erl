%%%-------------------------------------------------------------------
%%% @author manuel@whisper.sh/chad@whisper.sh
%%% @copyright (C) 2013 WhisperText Inc 
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(esque_redis_worker).
-behaviour(gen_server).

-export([stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, expire/1]).

-export([q/2, qp/1]).

-record(state, {
    process_id :: pid()
  }).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
  gen_server:cast(Pid, stop).

-spec init(term()) -> {ok,#state{}}.
init(Params) ->
  [Host, Port, Database, Password, ReconnectSleep] = [ proplists:get_value(X, Params)
    || X <- [host, port, database, pwd, reconnect_sleep]], 
  {ok, C} = eredis:start_link(Host, Port, Database, Password, ReconnectSleep),
  lager:debug("Started Redis Worker with  ~p ",[C]),
  {ok, #state{process_id = C}}.


-spec q(binary() | string(),list()) -> {ok,binary()} | {ok,[]}.
q(Method,Values) ->
  wpool:call(esque_redis_pool,{q,Method,Values}).

%% Pipelining
-spec qp([list()]) -> [{ok,binary() | []}].
qp(Commands) ->
  wpool:call(esque_redis_pool,{qp, Commands}).

-spec expire(string()) -> {ok,binary()} | {ok,[]}.
expire(Key) ->
  wpool:call(esque_redis_pool,{q,"EXPIRE",[Key,1]}).

-spec handle_call(term(), {pid(),_}, #state{}) -> {reply, term(), #state{}}.
handle_call({q,Method,Values}, _From, State = #state{ process_id = P}) ->
  R = eredis:q(P, [Method | Values]),
  {reply,R,State};

handle_call({qp, Commands}, _From, State = #state{ process_id = P}) ->
  {reply, eredis:qp(P, Commands), State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, normal, #state{}}.
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Args, State) ->
  {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(Info, State) ->
  lager:info("unknown data ~p",[Info]),
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, _State) ->
  lager:error("esque_redis ~p terminating, StackTrace:~p. reason: ~p ~n",[self(),erlang:get_stacktrace(),Reason]),
  ok.

-spec code_change(term(),term(),term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

