%%%-------------------------------------------------------------------
%%% @author Chad DePue
%%% @copyright (C) 2013 WhisperText
%%% @doc Provides a simple ram or disk backed queuing system
%%%      using redis.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(esque).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-export([
    add_queue/1,
    stop_queue/1,
    queue/3,
    queue/2,
    failed_jobs_count/1,
    clean_queue_failed_jobs/1,
    failed_job/1,
    serialize/2,
    deserialize/1
    ]).

-include("include/queue.hrl").

%% This is called when the application is started by reltool
-spec start() -> ok | {error, term()}.
start() ->
  application:ensure_all_started(?MODULE),
  start(normal, []).

%% @hidden
%% This is called when this application is started by another application
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  esque_sup:start_link().

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(?MODULE).

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.

-spec add_queue(atom() | [tuple()]) -> ok.
add_queue(Name) when is_atom(Name) -> 
  add_queue([{name, atom_to_list(Name)}]);
add_queue(Q) ->
  Spec = sup_spec(Q),
  supervisor:start_child(esque_worker_sup, Spec),
  ok.

-spec stop_queue([tuple()]) -> ok.
stop_queue(QueueParams) -> 
  Name = ?QUEUE_NAME(QueueParams),
  Spec = list_to_atom(?QUEUE_WORKER_PREFIX ++ Name),
  supervisor:terminate_child(esque_worker_sup, Spec),
  supervisor:delete_child(esque_worker_sup, Spec).

-spec queue(atom() | string() , [tuple()], term()) -> integer().
queue(QueueName, QueueParams, Payload) when is_atom(QueueName) ->
  queue(atom_to_list(QueueName), QueueParams, Payload);
queue(QueueName, QueueParams, Payload) when is_list(QueueName) ->
  queue([{name, QueueName}] ++ proplists:delete(name, QueueParams), Payload).

%% Payload MUST be a list of arguments.
%% Payload will eventually be the Args parameter in
%% erlang:apply(Module, Function, Args).
-spec queue([tuple()], list()) -> integer().
queue(QueueParams, Payload) when is_list(QueueParams), is_list(Payload) ->
  Name = ?QUEUE_NAME(QueueParams),
  Key = ?QUEUE_KEY(Name),
  Command = [
    ["multi"],
    ["lpush", [Key], serialize(?QUEUE_FORMAT(QueueParams), Payload)],
    ["publish", ?QUEUE_SUB_KEY(Name), "new"],
    ["exec"]],
  [_, _, _, {ok, [Size, _]}] = esque_redis_worker:qp(Command),
  binary_to_integer(Size). 

serialize(erlang, Term) ->
  erlang:term_to_binary(Term);
serialize(json, Term) ->
  jsx:encode(Term).

deserialize(Binary) ->
  case jsx:is_json(Binary) of
    true -> deserialize(json, Binary);
    false -> deserialize(erlang, Binary)
  end.

deserialize(erlang, Binary) ->
  erlang:binary_to_term(Binary);
deserialize(json, Binary) ->
  jsx:decode(Binary).

-spec failed_jobs_count([tuple()] | atom()) -> integer().
failed_jobs_count(QueueName) when is_atom(QueueName) ->
  failed_jobs_count([{name, atom_to_list(QueueName)}]);
failed_jobs_count(QueueParams) when is_list(QueueParams) ->
  Name = ?QUEUE_NAME(QueueParams),
  Key = ?QUEUE_KEY(Name),
  FailedQueueKey = ?QUEUE_KEY_FAILED(Key),
  esque_redis_worker:q("llen", [FailedQueueKey]). 

-spec failed_job([tuple()] | atom()) -> integer().
failed_job(QueueName) when is_atom(QueueName) ->
  failed_job([{name, atom_to_list(QueueName)}]);
failed_job(QueueParams) when is_list(QueueParams) ->
  Name = ?QUEUE_NAME(QueueParams),
  Key = ?QUEUE_KEY(Name),
  FailedQueueKey = ?QUEUE_KEY_FAILED(Key),
  esque_redis_worker:q("lpop", [FailedQueueKey]).

-spec clean_queue_failed_jobs([tuple()] | atom()) -> integer().
clean_queue_failed_jobs(QueueName) when is_atom(QueueName) ->
  clean_queue_failed_jobs([{name, atom_to_list(QueueName)}]);
clean_queue_failed_jobs(QueueParams) when is_list(QueueParams) ->
  Name = ?QUEUE_NAME(QueueParams),
  Key = ?QUEUE_KEY(Name),
  FailedQueueKey = ?QUEUE_KEY_FAILED(Key),
  esque_redis_worker:q("del", [FailedQueueKey]). 

sup_spec(Q) ->
  Name = ?QUEUE_NAME(Q),
  {
    list_to_atom(?QUEUE_WORKER_PREFIX ++ Name),
    {esque_worker, start_link, [Q]},
    permanent,
    brutal_kill,
    worker,
    [esqueue_worker]}.

  
