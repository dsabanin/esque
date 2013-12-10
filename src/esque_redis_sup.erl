%%%-------------------------------------------------------------------
%%% @author manuel@whisper.sh/chad@whisper.sh
%%% @copyright (C) 2013 WhisperText Inc 
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(esque_redis_sup).
-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Children = [pool_def(redis_pool)],
  lager:info("Initializing esque redis supervisor ..."),
  {ok, {{one_for_one, 5, 10}, Children}}.

pool_def(Name) ->
  {ok, Config} = application:get_env(esque, redis_pool),
  WorkerDef = [Name,[
      {overrun_warning, proplists:get_value(overrun_warning, Config)},
      {workers, proplists:get_value(workers, Config)},
      {worker, {esque_redis_worker, proplists:get_value(worker_config, Config)}}
    ]],
  {Name,
    {wpool, start_pool, WorkerDef},
    permanent, brutal_kill, supervisor, [wpool]
  }.

