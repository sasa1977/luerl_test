#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

%% ------------------------------------------------------------------
%% Main measurement loop
%% ------------------------------------------------------------------

main(_Args) ->
  [code:add_path(Path) || Path <- filelib:wildcard("./deps/**/ebin")],
  code:add_path("./ebin/"),
  application:ensure_all_started(luerl_test),
  runner:run().