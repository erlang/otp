%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(bs_match_misc_SUITE).

-author('bjorn@erix.ericsson.se').
-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 bound_var/1,bound_tail/1,t_float/1,little_float/1,sean/1]).

-include("test_server.hrl").

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [bound_var,bound_tail,t_float,little_float,sean].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_all(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    ok.

finish_all(Config) when is_list(Config) ->
    ok.

bound_var(doc) -> "Test matching of bound variables.";
bound_var(Config) when list(Config) ->
    ?line ok = bound_var(42, 13, <<42,13>>),
    ?line nope = bound_var(42, 13, <<42,255>>),
    ?line nope = bound_var(42, 13, <<154,255>>),
    ok.

bound_var(A, B, <<A:8,B:8>>) -> ok;
bound_var(_, _, _) -> nope.

bound_tail(doc) -> "Test matching of a bound tail.";
bound_tail(Config) when list(Config) ->
    ?line ok = bound_tail(<<>>, <<13,14>>),
    ?line ok = bound_tail(<<2,3>>, <<1,1,2,3>>),
    ?line nope = bound_tail(<<2,3>>, <<1,1,2,7>>),
    ?line nope = bound_tail(<<2,3>>, <<1,1,2,3,4>>),
    ?line nope = bound_tail(<<2,3>>, <<>>),
    ok.

bound_tail(T, <<_:16,T/binary>>) -> ok;
bound_tail(_, _) -> nope.

t_float(Config) when list(Config) ->
    F = f1(),
    G = f_one(),

    ?line G = match_float(<<63,128,0,0>>, 32, 0),
    ?line G = match_float(<<63,240,0,0,0,0,0,0>>, 64, 0),

    ?line fcmp(F, match_float(<<F:32/float>>, 32, 0)),
    ?line fcmp(F, match_float(<<F:64/float>>, 64, 0)),
    ?line fcmp(F, match_float(<<1:1,F:32/float,127:7>>, 32, 1)),
    ?line fcmp(F, match_float(<<1:1,F:64/float,127:7>>, 64, 1)),
    ?line fcmp(F, match_float(<<1:13,F:32/float,127:3>>, 32, 13)),
    ?line fcmp(F, match_float(<<1:13,F:64/float,127:3>>, 64, 13)),
    ok.


fcmp(F1, F2) when (F1 - F2) / F2 < 0.0000001 -> ok.

match_float(Bin0, Fsz, I) ->
    Bin = make_sub_bin(Bin0),
    Bsz = size(Bin) * 8,
    Tsz = Bsz - Fsz - I,
    <<_:I,F:Fsz/float,_:Tsz>> = Bin,
    F.

little_float(Config) when list(Config) ->
    F = f2(),
    G = f_one(),

    ?line G = match_float_little(<<0,0,0,0,0,0,240,63>>, 64, 0),
    ?line G = match_float_little(<<0,0,128,63>>, 32, 0),

    ?line fcmp(F, match_float_little(<<F:32/float-little>>, 32, 0)),
    ?line fcmp(F, match_float_little(<<F:64/float-little>>, 64, 0)),
    ?line fcmp(F, match_float_little(<<1:1,F:32/float-little,127:7>>, 32, 1)),
    ?line fcmp(F, match_float_little(<<1:1,F:64/float-little,127:7>>, 64, 1)),
    ?line fcmp(F, match_float_little(<<1:13,F:32/float-little,127:3>>, 32, 13)),
    ?line fcmp(F, match_float_little(<<1:13,F:64/float-little,127:3>>, 64, 13)),

    ok.

match_float_little(Bin0, Fsz, I) ->
    Bin = make_sub_bin(Bin0),
    Bsz = size(Bin) * 8,
    Tsz = Bsz - Fsz - I,
    <<_:I,F:Fsz/float-little,_:Tsz>> = Bin,
    F.


make_sub_bin(Bin0) ->
    Sz = size(Bin0),
    Bin1 = <<37,Bin0/binary,38,39>>,
    <<_:8,Bin:Sz/binary,_:8,_:8>> = Bin1,
    Bin.

f1() ->
    3.1415.

f2() ->
    2.7133.

f_one() ->
    1.0.

sean(Config) when list(Config) ->
    ?line small = sean1(<<>>),
    ?line small = sean1(<<1>>),
    ?line small = sean1(<<1,2>>),
    ?line small = sean1(<<1,2,3>>),
    ?line large = sean1(<<1,2,3,4>>),

    ?line small = sean1(<<4>>),
    ?line small = sean1(<<4,5>>),
    ?line small = sean1(<<4,5,6>>),
    ?line {'EXIT',{function_clause,_}} = (catch sean1(<<4,5,6,7>>)),
    ok.

sean1(<<B/binary>>) when size(B) < 4 ->	small;
sean1(<<1, _B/binary>>) -> large.
