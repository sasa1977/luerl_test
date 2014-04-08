-module(runner).
-export([run/0]).

-define(RUNS, 100000).
-define(PARALLEL_WORKERS, 8).

program() ->
  <<"
  function fac(n)
      local i,r
      i = n
      r = 1
      while i > 0 do
        r = r * i
        i = i - 1
      end
      return r
    end
  ">>.


%% ------------------------------------------------------------------
%% Parallel setup
%% ------------------------------------------------------------------

runner(State) ->
  receive
    {exec, From, Code} ->
      From ! {response, Code(State)}
  end,
  runner(State).

setup_parallel_workers(State) ->
  list_to_tuple(
    [spawn(fun() -> runner(State) end) || _ <- lists:seq(1,?PARALLEL_WORKERS)]
  ).

exec_parallel(0, _RunnerPids, _Code) ->
  await(?RUNS);

exec_parallel(N, RunnerPids, Code) ->
  element(N rem ?PARALLEL_WORKERS + 1, RunnerPids) ! {exec, self(), Code},
  exec_parallel(N-1, RunnerPids, Code).

await(0) -> ok;
await(N) ->
  receive
    {response, _} -> await(N-1)
  end.


%% ------------------------------------------------------------------
%% Serial setup
%% ------------------------------------------------------------------

exec(0, _) -> ok;
exec(N, Code) ->
  Code(),
  exec(N-1, Code).


%% ------------------------------------------------------------------
%% Main measurement loop
%% ------------------------------------------------------------------

run() ->
  [code:add_path(Path) || Path <- filelib:wildcard("./deps/**/ebin")],
  code:add_path("./ebin/"),
  application:ensure_all_started(luerl_test),

  % test forking of state
  {_, State0} = luerl:do("a = 1"),
  {_, State1} = luerl:do("a = a + 2", State0),
  {_, State2} = luerl:do("a = a + 10", State0),
  io:format("~p~n", [element(2, luerl:eval("return a", State0))]),
  io:format("~p~n", [element(2, luerl:eval("return a", State1))]),
  io:format("~p~n", [element(2, luerl:eval("return a", State2))]),

  % measure precompiling
  {ok, Chunk} = luerl:load(program()),
  {_, InitialState} = luerl:do(Chunk),
  {Microsec, _} =
    timer:tc(
        fun() ->
          exec(
            ?RUNS,
            fun() -> luerl:call_function([fac], [50], InitialState) end
          )
        end
      ),
  io:format("~.2f runs/sec [sequential]~n", [?RUNS * 1000000 / Microsec]),
  RunnerPids = setup_parallel_workers(InitialState),
  {MicrosecParallel, _} =
    timer:tc(
        fun() ->
          exec_parallel(
            ?RUNS,
            RunnerPids,
            fun(State) ->
              element(1, luerl:call_function([fac], [50], State))
            end
          )
        end
      ),
  io:format("~.2f runs/sec [parallel]~n", [?RUNS * 1000000 / MicrosecParallel]).