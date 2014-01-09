%%%-------------------------------------------------------------------
%%% @author manuel@whisper.sh/chad@whisper.sh
%%% @copyright (C) 2013 WhisperText Inc 
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(esque_worker_sup).

-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-export([start_link/0]).
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
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

