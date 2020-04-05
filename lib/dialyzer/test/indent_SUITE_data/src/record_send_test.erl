%%-------------------------------------------------------------------
%% File    : record_send_test.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Description : A test inspired by a post of Mkcael Remond to the
%%		 Erlang mailing list suggesting thst Dialyzer should
%%		 be reporting sends to records rather than to pids.
%%		 Dialyzer v1.3.0 indeed reports one of the dicrepancies
%%		 (the one with the 4-tuple) but not the one where the
%%		 message is sent to a pair which is a record.
%%		 This should be fixed.
%%
%% Created : 10 Apr 2005 by Kostis Sagonas <kostis@it.uu.se>
%%-------------------------------------------------------------------
-module(record_send_test).

-export([t/0]).

-record(rec1, {a=a, b=b, c=c}).
-record(rec2, {a}).

t() ->
  t(#rec1{}).

t(Rec1 = #rec1{b=B}) ->
  Rec2 = some_mod:some_function(),
  if
    is_record(Rec2, rec2) ->
      Rec2 ! hello;	%% currently this one is not found
    true ->
      Rec1 ! hello_again
  end,
  B.
