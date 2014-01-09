-module(esque_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> 
  [
    start_test,
    add_queue,
    stop_queue,
    multiple_workers,
    node_specific,
    retry,
    wrong_params_to_payload
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
    done -> ct:log("got done")
  after 2000 ->
    throw(queue_not_processed)
  end,
  Config.

stop_queue(Config) ->
  start_test(Config),
  esque:add_queue([{name, "delete"}]),
  esque:stop_queue([{name, "delete"}]),
  esque:add_queue([{name, "delete"}]),
  case whereis(esque_worker_delete) of
    undefined -> throw(failed);
    D when is_pid(D) -> ok
  end,
  Config.

multiple_workers(Config) ->
  start_test(Config),
  esque:add_queue([{name, "multiple_workers"}, {workers, 100}, {action, {esque_SUITE, add_queue_callback}}]),
  Pid = self(),
  lists:map(fun(_) -> spawn(fun() -> esque:queue(multiple_workers, [], [self()]),
                            receive
                              done -> ct:log("got done")
                            after 2000 ->
                              Pid ! error
                            end
                        end)
                  end, lists:seq(1,1000)),
  receive
    error -> throw(timeout_multiple_workers)
  after 3000 ->
    ok
  end,
  Config.

node_specific(Config) ->
  start_test(Config),
  Queue = [{name, "node_specific"}, {workers, 1}, {node_specific, true}, {action, {esque_SUITE, add_queue_callback}}],
  esque:add_queue(Queue),
  esque:queue(Queue, [self()]),
  receive
    done -> ct:log("got done")
  after 2000 ->
    throw(queue_not_processed)
  end,
  Config.

wrong_params_to_payload(Config) ->
  start_test(Config),
  Queue = [{name, "wrong_params_to_payload"}, {workers, 1}, {action, {foo, bar}}],
  esque:add_queue(Queue),
  try
    esque:queue(wrong_params_to_payload, [], atom)
  catch
    error:function_clause -> ct:log("~p ~p", [error, function_clause])
  end,
  Config.
  
retry(Config) -> 
  start_test(Config),
  Queue = [{name, "retry_esque_test"}, {workers, 1}, {retry_options, [{retry_unless, ok}]}, {action, {esque_SUITE, retry_callback}}],
  Pid = spawn(fun retry_waiter/0),
  esque:add_queue(Queue),
  esque:queue(Queue, [Pid, self()]),
  receive
    done -> ok
  after 2000 ->
    throw(retry_failed)
  end,
  Config.

%% ---- helpers

add_queue_callback(Pid) ->
  Pid ! done.

retry_callback(WaiterPid, TestPid) -> 
  WaiterPid ! called,
  case whereis(retry_waiter_called) of
    undefined -> not_ok;
    WaiterPid -> TestPid ! done, ok
  end.

retry_waiter() -> 
  receive
    called -> 
      ct:log("retry_waiter called once"),
      erlang:register(retry_waiter_called, self()),
      receive
        called -> ct:log("retry_waiter called again")
      end
  end.
