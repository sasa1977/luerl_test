#!/usr/bin/env escript

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

exec(0, _) -> ok;
exec(N, Code) ->
  Code(),
  exec(N-1, Code).

-define(RUNS, 100000).

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
  io:format("~.2f runs/sec~n", [?RUNS * 1000000 / Microsec]).