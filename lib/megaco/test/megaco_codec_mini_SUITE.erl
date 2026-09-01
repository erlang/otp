%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2026. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Test mini encoding/decoding (codec) module of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_codec_mini_SUITE).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([
 	 suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

	 otp7672_msg01/1,
 	 otp7672_msg02/1,

         otp16631_msg01/1,
         otp16631_msg02/1,
         otp16631_msg03/1,
         otp16631_msg04/1,
         otp16631_msg05/1,
         otp16631_msg06/1,
         otp16631_msg11/1,
         otp16631_msg12/1,
         otp16631_msg13/1,
         otp16631_msg14/1,
         otp16631_msg15/1,
         otp16631_msg16/1,
         otp16631_msg21/1,
         otp16631_msg22/1,
         otp16631_msg23/1,
         otp16631_msg24/1,
         otp16631_msg25/1,
         otp16631_msg26/1,
         otp16631_msg31/1,
         otp16631_msg32/1,
         otp16631_msg33/1,
         otp16631_msg34/1,
         otp16631_msg35/1,
         otp16631_msg36/1,

         otp20234_scan_oversized_binary/1,
         otp20234_scan_oversized_list/1,
         otp20234_oversized_integer/1,
         otp20234_boundary_integer/1

	]).  


%% ----

-define(SET_DBG(S,D), begin put(severity, S), put(dbg, D) end).
-define(RESET_DBG(),  begin erase(severity),  erase(dbg)  end).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [{group, tickets}].

groups() -> 
    [
     {tickets,  [], tickets_cases()},
     {otp7672,  [], otp7672_cases()},
     {otp16631, [], otp16631_cases()},
     {otp20234, [], otp20234_cases()}
    ].

tickets_cases() ->
    [
     {group, otp7672},
     {group, otp16631},
     {group, otp20234}
    ].

otp7672_cases() ->
    [
     otp7672_msg01,
     otp7672_msg02
    ].

otp16631_cases() ->
    [
     otp16631_msg01,
     otp16631_msg02,
     otp16631_msg03,
     otp16631_msg04,
     otp16631_msg05,
     otp16631_msg06,
     otp16631_msg11,
     otp16631_msg12,
     otp16631_msg13,
     otp16631_msg14,
     otp16631_msg15,
     otp16631_msg16,
     otp16631_msg21,
     otp16631_msg22,
     otp16631_msg23,
     otp16631_msg24,
     otp16631_msg25,
     otp16631_msg26,
     otp16631_msg31,
     otp16631_msg32,
     otp16631_msg33,
     otp16631_msg34,
     otp16631_msg35,
     otp16631_msg36
    ].

otp20234_cases() ->
    [
     otp20234_scan_oversized_binary,
     otp20234_scan_oversized_list,
     otp20234_oversized_integer,
     otp20234_boundary_integer
    ].



%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite([{sysmon, false} | Config0]) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),

            Config1
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(Case, Config) ->

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    put(verbosity,trc),
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config, erlang:nodes()]),

    erase(verbosity),
    megaco_test_lib:end_per_testcase(Case, Config).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
%% Ticket test cases:


%% --------------------------------------------------------------
%% 

otp7672_msg01(suite) ->
    [];
otp7672_msg01(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg01 -> entry", []),
    ok = otp7672( otp7672_msg01() ),
    %% ?RESET_DBG(), 
    ok.

otp7672_msg02(suite) ->
    [];
otp7672_msg02(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg02 -> entry", []),
    ok = otp7672( otp7672_msg02() ),
    %% ?RESET_DBG(), 
    ok.


otp7672_msg01() ->
    <<"!/1 <TEST> ">>.

otp7672_msg02() ->
    <<"!/1 <TE> ">>.

otp7672(Msg) ->
    case megaco_text_mini_decoder:decode_message([], Msg) of
	{ok, M} ->
	    t("mini decode successfull: ~n~p", [M]),
	    ok;
	Error ->
	    e("mini decode failed: ~n~p", [Error]),
	    {error, Error}
    end.



%% --------------------------------------------------------------
%% 

otp16631_msg01(suite) ->
    [];
otp16631_msg01(Config) when is_list(Config) ->
    d("otp16631_msg01 -> entry", []),
    ok = otp16631( otp16631_msg01() ),
    ok.

otp16631_msg01() ->
    otp16631_msg("a").


%% --

otp16631_msg02(suite) ->
    [];
otp16631_msg02(Config) when is_list(Config) ->
    d("otp16631_msg02 -> entry", []),
    ok = otp16631( otp16631_msg02() ),
    ok.

otp16631_msg02() ->
    otp16631_msg("b").


%% --

otp16631_msg03(suite) ->
    [];
otp16631_msg03(Config) when is_list(Config) ->
    d("otp16631_msg03 -> entry", []),
    ok = otp16631( otp16631_msg03() ),
    ok.

otp16631_msg03() ->
    otp16631_msg("c").


%% --

otp16631_msg04(suite) ->
    [];
otp16631_msg04(Config) when is_list(Config) ->
    d("otp16631_msg04 -> entry", []),
    ok = otp16631( otp16631_msg04() ),
    ok.

otp16631_msg04() ->
    otp16631_msg("d").


%% --

otp16631_msg05(suite) ->
    [];
otp16631_msg05(Config) when is_list(Config) ->
    d("otp16631_msg05 -> entry", []),
    ok = otp16631( otp16631_msg05() ),
    ok.

otp16631_msg05() ->
    otp16631_msg("e").


%% --

otp16631_msg06(suite) ->
    [];
otp16631_msg06(Config) when is_list(Config) ->
    d("otp16631_msg06 -> entry", []),
    ok = otp16631( otp16631_msg06() ),
    ok.

otp16631_msg06() ->
    otp16631_msg("f").


%% --

otp16631_msg11(suite) ->
    [];
otp16631_msg11(Config) when is_list(Config) ->
    d("otp16631_msg11 -> entry", []),
    ok = otp16631( otp16631_msg11() ),
    ok.

otp16631_msg11() ->
    otp16631_msg("000a").


%% --

otp16631_msg12(suite) ->
    [];
otp16631_msg12(Config) when is_list(Config) ->
    d("otp16631_msg12 -> entry", []),
    ok = otp16631( otp16631_msg12() ),
    ok.

otp16631_msg12() ->
    otp16631_msg("000b").


%% --

otp16631_msg13(suite) ->
    [];
otp16631_msg13(Config) when is_list(Config) ->
    d("otp16631_msg13 -> entry", []),
    ok = otp16631( otp16631_msg13() ),
    ok.

otp16631_msg13() ->
    otp16631_msg("000c").


%% --

otp16631_msg14(suite) ->
    [];
otp16631_msg14(Config) when is_list(Config) ->
    d("otp16631_msg14 -> entry", []),
    ok = otp16631( otp16631_msg14() ),
    ok.

otp16631_msg14() ->
    otp16631_msg("000d").


%% --

otp16631_msg15(suite) ->
    [];
otp16631_msg15(Config) when is_list(Config) ->
    d("otp16631_msg15 -> entry", []),
    ok = otp16631( otp16631_msg15() ),
    ok.

otp16631_msg15() ->
    otp16631_msg("000e").


%% --

otp16631_msg16(suite) ->
    [];
otp16631_msg16(Config) when is_list(Config) ->
    d("otp16631_msg16 -> entry", []),
    ok = otp16631( otp16631_msg16() ),
    ok.

otp16631_msg16() ->
    otp16631_msg("000f").


%% --

otp16631_msg21(suite) ->
    [];
otp16631_msg21(Config) when is_list(Config) ->
    d("otp16631_msg21 -> entry", []),
    ok = otp16631( otp16631_msg21() ),
    ok.

otp16631_msg21() ->
    otp16631_msg("0a12").


%% --

otp16631_msg22(suite) ->
    [];
otp16631_msg22(Config) when is_list(Config) ->
    d("otp16631_msg22 -> entry", []),
    ok = otp16631( otp16631_msg22() ),
    ok.

otp16631_msg22() ->
    otp16631_msg("0b12").


%% --

otp16631_msg23(suite) ->
    [];
otp16631_msg23(Config) when is_list(Config) ->
    d("otp16631_msg23 -> entry", []),
    ok = otp16631( otp16631_msg23() ),
    ok.

otp16631_msg23() ->
    otp16631_msg("0c12").


%% --

otp16631_msg24(suite) ->
    [];
otp16631_msg24(Config) when is_list(Config) ->
    d("otp16631_msg24 -> entry", []),
    ok = otp16631( otp16631_msg24() ),
    ok.

otp16631_msg24() ->
    otp16631_msg("0d12").


%% --

otp16631_msg25(suite) ->
    [];
otp16631_msg25(Config) when is_list(Config) ->
    d("otp16631_msg25 -> entry", []),
    ok = otp16631( otp16631_msg25() ),
    ok.

otp16631_msg25() ->
    otp16631_msg("0e12").


%% --

otp16631_msg26(suite) ->
    [];
otp16631_msg26(Config) when is_list(Config) ->
    d("otp16631_msg26 -> entry", []),
    ok = otp16631( otp16631_msg26() ),
    ok.

otp16631_msg26() ->
    otp16631_msg("0f12").


%% --

otp16631_msg31(suite) ->
    [];
otp16631_msg31(Config) when is_list(Config) ->
    d("otp16631_msg31 -> entry", []),
    ok = otp16631( otp16631_msg31() ),
    ok.

otp16631_msg31() ->
    otp16631_msg("a123").


%% --

otp16631_msg32(suite) ->
    [];
otp16631_msg32(Config) when is_list(Config) ->
    d("otp16631_msg32 -> entry", []),
    ok = otp16631( otp16631_msg32() ),
    ok.

otp16631_msg32() ->
    otp16631_msg("b123").


%% --

otp16631_msg33(suite) ->
    [];
otp16631_msg33(Config) when is_list(Config) ->
    d("otp16631_msg33 -> entry", []),
    ok = otp16631( otp16631_msg33() ),
    ok.

otp16631_msg33() ->
    otp16631_msg("c123").


%% --

otp16631_msg34(suite) ->
    [];
otp16631_msg34(Config) when is_list(Config) ->
    d("otp16631_msg34 -> entry", []),
    ok = otp16631( otp16631_msg34() ),
    ok.

otp16631_msg34() ->
    otp16631_msg("d123").


%% --

otp16631_msg35(suite) ->
    [];
otp16631_msg35(Config) when is_list(Config) ->
    d("otp16631_msg35 -> entry", []),
    ok = otp16631( otp16631_msg35() ),
    ok.

otp16631_msg35() ->
    otp16631_msg("e123").


%% --

otp16631_msg36(suite) ->
    [];
otp16631_msg36(Config) when is_list(Config) ->
    d("otp16631_msg36 -> entry", []),
    ok = otp16631( otp16631_msg36() ),
    ok.

otp16631_msg36() ->
    otp16631_msg("f123").


%% -----

otp16631( Msg ) ->
    Bin = erlang:list_to_binary(Msg),
    try megaco_compact_text_encoder:decode_mini_message([], dynamic, Bin) of
        {ok, _} ->
            ok;
        {error, _} = ERROR ->
            ERROR
    catch
        C:E:S ->
            {error, {C, E, S}}
    end.
               

otp16631_msg(X) when is_list(X) ->
    "!/1 [2409:8050:5005:1243:1011::" ++ X ++ 
        "] T=2523{C=-{SC=ROOT{SV{MT=RS,RE=901,PF=ETSI_BGF/2,V=3}}}}".


%% --------------------------------------------------------------
%% OTP-20234: Limit text codec integer parsing to prevent CPU
%% exhaustion via oversized digit strings.
%% --------------------------------------------------------------

%% Verify that the scanner rejects binary input exceeding MAX_SCAN_SIZE (100KB).
otp20234_scan_oversized_binary(suite) ->
    [];
otp20234_scan_oversized_binary(Config) when is_list(Config) ->
    d("otp20234_scan_oversized_binary -> entry", []),
    %% 100_001 bytes — just over the 100KB gate
    Bin = <<"!/1 ", (binary:copy(<<"0">>, 100001))/binary>>,
    case megaco_text_scanner:scan(Bin) of
        {error, {message_too_large, Size}, 0} when Size > 100000 ->
            d("otp20234_scan_oversized_binary -> rejected with size ~p", [Size]),
            ok;
        Other ->
            e("otp20234_scan_oversized_binary -> unexpected result: ~p", [Other]),
            ct:fail({unexpected_result, Other})
    end.

%% Verify that the scanner rejects list input exceeding MAX_SCAN_SIZE (100KB).
otp20234_scan_oversized_list(suite) ->
    [];
otp20234_scan_oversized_list(Config) when is_list(Config) ->
    d("otp20234_scan_oversized_list -> entry", []),
    %% 100_001 chars — just over the 100KB gate
    Chars = "!/1 " ++ lists:duplicate(100001, $0),
    case megaco_text_scanner:scan(Chars) of
        {error, {message_too_large, Size}, 0} when Size > 100000 ->
            d("otp20234_scan_oversized_list -> rejected with size ~p", [Size]),
            ok;
        Other ->
            e("otp20234_scan_oversized_list -> unexpected result: ~p", [Other]),
            ct:fail({unexpected_result, Other})
    end.

%% Verify that a 21-digit integer token is rejected by the parser
%% (exceeds MAX_DIGITS = 20) without triggering expensive bignum conversion.
%% Uses full decode_message (not mini) since the mini decoder doesn't
%% validate integers in the transaction body.
otp20234_oversized_integer(suite) ->
    [];
otp20234_oversized_integer(Config) when is_list(Config) ->
    d("otp20234_oversized_integer -> entry", []),
    %% 21-digit transactionID — exceeds ?MAX_DIGITS (20)
    TID = lists:duplicate(21, $9),
    Msg = list_to_binary(
            "!/1 [10.10.10.10]:2944\nT=" ++ TID ++
            "{C=-{SC=ROOT{SV{MT=RS,RE=\"901\"}}}}"),
    case megaco_compact_text_encoder:decode_message([], dynamic, Msg) of
        {error, [{reason, {_Line, _Mod, {too_large_integer, _Text, _Max}}} | _]} ->
            d("otp20234_oversized_integer -> "
              "correctly rejected by digit guard", []),
            ok;
        Other ->
            e("otp20234_oversized_integer -> "
              "unexpected result: ~p", [Other]),
            ct:fail({unexpected_result, Other})
    end.

%% Verify that a valid 10-digit uint32 (max value 4294967295) still
%% parses the integer successfully — boundary test ensuring the
%% MAX_DIGITS guard doesn't over-reject. The message may fail later
%% validation (e.g. missing parameters), but the integer parsing step
%% must succeed (no too_large_integer error).
otp20234_boundary_integer(suite) ->
    [];
otp20234_boundary_integer(Config) when is_list(Config) ->
    d("otp20234_boundary_integer -> entry", []),
    %% 10-digit transactionID at uint32 max — within ?MAX_DIGITS (20)
    Msg = <<"!/1 [10.10.10.10]:2944\nT=4294967295"
            "{C=-{SC=ROOT{SV{MT=RS,RE=\"901\"}}}}">>,
    case megaco_compact_text_encoder:decode_message([], dynamic, Msg) of
        {ok, _} ->
            d("otp20234_boundary_integer -> correctly accepted", []),
            ok;
        Other ->
            e("otp20234_boundary_integer -> "
              "unexpected result: ~p", [Other]),
            ct:fail({unexpected_result, Other})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F, A) ->
    io:format("*** [~s] ~p ***"
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).


t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

%% l(F,A) ->
%%     p(printable(get(severity),log),log,F,A).

e(F,A) ->
    p(printable(get(severity),err),err,F,A).


printable(trc,_) ->
    true;
printable(dbg,trc) ->
    false;
printable(dbg,_) ->
    true;
printable(log,log) ->
    true;
printable(log,err) ->
    true;
printable(err,err) ->
    true;
printable(_,_) ->
    false.


p(true,L,F,A) ->
    io:format("~s:" ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "TRC";
image_of(dbg) ->
    "DBG";
image_of(log) ->
    "LOG";
image_of(err) ->
    "ERR";
image_of(L) ->
    io_lib:format("~p",[L]).

