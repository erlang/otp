%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% Date: Mon, 7 Jun 2004 13:07:39 +0300
%% From: Einar Karttunen
%% To: Erlang ML <erlang-questions@erlang.org>
%% Subject: Apparent binary matching bug with native compilation
%%
%% It seems that there is a problem with binary matching when
%% compiling native code. A length prefixed field matches one
%% byte too short in the native case.
%%
%% The test module works when compiled with no options, but
%% crashes with case_clause when compiled with [native].
%% This has been confirmed with R9C-0 and hipe snapshot 5/4/2004.
%%--------------------------------------------------------------------

-module(bs_bugs_R09).

-export([test/0]).

test() ->
  ["rei",".",[]] = pp(<<3,$r,$e,$i,0>>),
  ok.

pp(Bin) ->
  %% io:format("PP with ~p~n", [Bin]),
  case Bin of
    <<>> ->
      ["."];
    <<_:2, Len:6, Part:Len/binary>> ->
      [binary_to_list(Part)];
    <<_:2, Len:6, Part:Len/binary, Rest/binary>> ->
      %% io:format("Len ~p Part ~p Rest ~p~n", [Len,Part,Rest]),
      [binary_to_list(Part), "." | pp(Rest)]
  end.
