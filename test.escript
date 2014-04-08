#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-define(RUNS, 100000).
-define(PARALLEL_WORKERS, 10).

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

runner(ReturnPid) ->
  receive
    {ok, Code} ->
      Code(),
      runner(ReturnPid);
    stop ->
      ReturnPid ! done
  end.

setup_parallel_workers() ->
  Me = self(),
  [spawn(fun() -> runner(Me) end) || _ <- lists:seq(1,?PARALLEL_WORKERS)].

exec_parallel(N, RunnerPids, Code) ->
  exec_parallel(N, RunnerPids, [], Code).

exec_parallel(N, [], RunnerPids, Code) ->
  exec_parallel(N, RunnerPids, [], Code);
exec_parallel(0, RunnerPids1, RunnerPids2, _Code) ->
  AllPids = RunnerPids1 ++ RunnerPids2,
  [Pid ! stop || Pid <- AllPids],
  await(length(AllPids));
exec_parallel(N, [Pid | Rest], AccPids, Code) ->
  Pid ! {ok, Code},
  exec_parallel(N-1, Rest, [Pid|AccPids], Code).

await(0) -> ok;
await(N) ->
  receive
    done -> await(N-1)
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

main(_Args) ->
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
  {_, State} = luerl:do(Chunk),
  {Microsec, _} =
    timer:tc(
        fun() ->
          exec(
            ?RUNS,
            fun() -> luerl:call_function([fac], [50], State) end
          )
        end
      ),
  io:format("~.2f runs/sec [sequential]~n", [?RUNS * 1000000 / Microsec]),
  RunnerPids = setup_parallel_workers(),
  {MicrosecParallel, _} =
    timer:tc(
        fun() ->
          exec_parallel(
            ?RUNS,
            RunnerPids,
            fun() -> luerl:call_function([fac], [50], State) end
          )
        end
      ),
  io:format("~.2f runs/sec [parallel]~n", [?RUNS * 1000000 / MicrosecParallel]).
