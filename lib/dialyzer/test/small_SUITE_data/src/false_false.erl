%%----------------------------------------------------------------------------
%% Mail from user (username: sauron!) via Dan Gudmundsson (17 Dec 2010).
%%
%%   tried this on:
%%     Erlang R14B (erts-5.8.1.2) [smp:2:2] [rq:2] [async-threads:0] ...
%%   and got funny diagnostics from dialyzer
%%
%%   false_false.erl:20: Function false_or/0 has no local return
%%   false_false.erl:25: The variable _ can never match since previous
%%		         clauses completely covered the type 'ok'
%%
%% Problem in the handling of 'or' with constant 'false' arguments
%% fixed by Stavros Aronis and Maria Christakis on the same day.
%%----------------------------------------------------------------------------
-module(false_false).

-export([false_or/0, wips/0]).

false_or() ->
  false or false.

wips() ->
  case new_execute_cmd(random:uniform(2)) of
    ok -> mostly_good;
    _ -> and_here_we_are
  end.

new_execute_cmd(1) ->
  ok;
new_execute_cmd(2) ->
  io:format("Surprise result is: ~p~n", [false or false]),
  false.
