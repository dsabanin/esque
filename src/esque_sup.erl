%%%-------------------------------------------------------------------
%%% @author manuel@whisper.sh/chad@whisper.sh
%%% @copyright (C) 2013 WhisperText Inc 
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(esque_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([watch/3]).

%% Callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

watch(App, Field, Pid) ->
  supervisor:start_child(?MODULE,[App, Field, Pid]).

init([]) ->

  application:ensure_started(worker_pool),

  {ok, {{one_for_one, 100, 1},
      [
        {esque_redis_sup, {esque_redis_sup, start_link, []}, permanent, infinity, supervisor, [esque_redis_sup]},
        {esque_worker_sup, {esque_worker_sup, start_link, []}, permanent, infinity, supervisor, [esque_worker_sup]}
      ]}}.
