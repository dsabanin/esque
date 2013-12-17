-module(esque_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> 
  [
    start_test,
    add_queue,
    stop_queue
  ].

start_test(Config) ->
  application:start(esque),
  {ok,esque} = application:get_application(esque),
  Config.

add_queue(Config) ->
  start_test(Config),
  esque:add_queue([{name, "add_queue_test"}, {workers, 1}, {action, {esque_SUITE, add_queue_callback}}]),
  esque:queue([{name, "add_queue_test"}], [self()]),
  receive
    done -> ok
  after 2000 ->
    throw(queue_not_processed)
  end,
  Config.

stop_queue(Config) ->
  start_test(Config),
  esque:add_queue([{name, "delete"}]),
  esque:stop_queue([{name, "delete"}]),
  esque:add_queue([{name, "delete"}]),
  case whereis(esque_puller_delete) of
    undefined -> throw(failed);
    D when is_pid(D) -> ok
  end,
  Config.

add_queue_callback(Pid) ->
  Pid ! done.

