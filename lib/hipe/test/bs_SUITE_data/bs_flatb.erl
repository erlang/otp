%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------------
%% Program which resulted in a badarg crash when compiled to native code.
%% The problem was that hipe_icode_primops was stating that the primop
%% {bs_start_match, ok_matchstate} could not fail which made the icode_type
%% pass removing the third clause of flatb/1.
%%
%% (The program was working correctly with hipe option 'no_icode_type'.)
%%
%% Reported by Andreas Sandberg on 3/1/2011 and fixed by Kostis on 5/1/2011
%% with the help of Per Gustafsson.
%% --------------------------------------------------------------------------
-module(bs_flatb).

-export([hipe_options/0, test/0]).

hipe_options() ->
  [icode_type].

test() ->
  [] = flatb([<<>>], []),
  ok.

flatb(<<X:8, Rest/binary>>, Acc) ->
  flatb(Rest, [X|Acc]);
flatb(<<>>, Acc) ->
  Acc;
flatb([V], Acc) ->
  flatb(V, Acc).
