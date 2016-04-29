%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(line_number_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 line_numbers/1]).
-export([crash/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    cases().

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

cases() ->
    [line_numbers].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.





%%
%% === Make sure that this is always line 70 ===
%%
line1(Tag, X) ->				%Line 72
    case Tag of					%Line 73
	a ->
	    Y = X + 1,				%Line 75
	    Res = id({ok,Y}),			%Line 76
	    ?MODULE:crash({ok,42} = Res);	%Line 77
	b ->
	    x = id(x),				%Line 79
	    ok					%Line 80
    end.					%Line 81

crash(_) ->					%Line 83
    erlang:error(crash).			%Line 84

close_calls(Where) ->				%Line 86
    put(where_to_crash, Where),			%Line 87
    try
	call1(),				%Line 89
	call2(),				%Line 90
	call3(),				%Line 91
	no_crash				%Line 92
    catch error:crash ->
	    erlang:get_stacktrace()		%Line 94
    end.					%Line 95

call1() ->					%Line 97
    maybe_crash(call1),				%Line 98
    ok.						%Line 99

call2() ->					%Line 101
    maybe_crash(call2),				%Line 102
    ok.						%Line 103

call3() ->					%Line 105
    maybe_crash(call3),				%Line 106
    ok.						%Line 107

maybe_crash(Name) ->				%Line 109
    case get(where_to_crash) of			%Line 110
	Name ->
	    erlang:error(crash);		%Line 112
	_ ->
	    ok					%Line 114
    end.					%Line 115

build_binary1(Size) ->				%Line 117
    id(42),					%Line 118
    <<0:Size>>.					%Line 119

build_binary2(Size, Bin) ->			%Line 121
    id(0),					%Line 122
    <<7:Size,Bin/binary>>.			%Line 123

do_call_abs(x, Arg) ->				%Line 125
    abs(Arg).					%Line 126

do_call_unsafe_bif(x, Arg) ->			%Line 128
    link(Arg).					%Line 129


line_numbers(Config) when is_list(Config) ->
    File = ?MODULE_STRING ++ ".erl",
    {'EXIT',{{case_clause,bad_tag},
	     [{?MODULE,line1,2,
	       [{file,File},{line,73}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch line1(bad_tag, 0)),
    {'EXIT',{badarith,
	     [{?MODULE,line1,2,
	       [{file,File},{line,75}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch line1(a, not_an_integer)),
    {'EXIT',{{badmatch,{ok,1}},
	     [{?MODULE,line1,2,
	       [{file,File},{line,77}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch line1(a, 0)),
    {'EXIT',{crash,
	     [{?MODULE,crash,1,
	       [{file,File},{line,84}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch line1(a, 41)),

    [{?MODULE,maybe_crash,1,[{file,File},{line,112}]},
     {?MODULE,call1,0,[{file,File},{line,98}]},
     {?MODULE,close_calls,1,[{file,File},{line,89}]},
     {?MODULE,line_numbers,1,[{file,File},{line,_}]}|_] =
	close_calls(call1),
    [{?MODULE,maybe_crash,1,[{file,File},{line,112}]},
     {?MODULE,call2,0,[{file,File},{line,102}]},
     {?MODULE,close_calls,1,[{file,File},{line,90}]},
     {?MODULE,line_numbers,1,[{file,File},{line,_}]}|_] =
	close_calls(call2),
    [{?MODULE,maybe_crash,1,[{file,File},{line,112}]},
     {?MODULE,call3,0,[{file,File},{line,106}]},
     {?MODULE,close_calls,1,[{file,File},{line,91}]},
     {?MODULE,line_numbers,1,[{file,File},{line,_}]}|_] =
	close_calls(call3),
    no_crash = close_calls(other),

    <<0,0>> = build_binary1(16),
    {'EXIT',{badarg,
	     [{?MODULE,build_binary1,1,
	       [{file,File},{line,119}]},
	      {?MODULE,line_numbers,1,
	       [{file,ModFile},{line,_}]}|_]}} =
	(catch build_binary1(bad_size)),

    <<7,1,2,3>> = build_binary2(8, <<1,2,3>>),
    {'EXIT',{badarg,
	     [{?MODULE,build_binary2,2,
	       [{file,File},{line,123}]},
	      {?MODULE,line_numbers,1,
	       [{file,ModFile},{line,_}]}|_]}} =
	(catch build_binary2(bad_size, <<>>)),
    {'EXIT',{badarg,
	     [%% Beam has an extra here:
	      %%   {erlang,bit_size,[bad_binary],[]}
	      %% Since this is an artifact of the implementation,
	      %% we don't attempt to mimic it in the debugger.
	      {?MODULE,build_binary2,2,
	       [{file,File},{line,123}]},
	      {?MODULE,line_numbers,1,
	       [{file,ModFile},{line,_}]}|_]}} =
	(catch build_binary2(8, bad_binary)),

    {'EXIT',{function_clause,
	     [{?MODULE,do_call_abs,[y,y],
	       [{file,File},{line,125}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch do_call_abs(y, y)),
    {'EXIT',{badarg,
	     [{erlang,abs,[[]],[]},
	      {?MODULE,do_call_abs,2,
	       [{file,File},{line,126}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch do_call_abs(x, [])),

    {'EXIT',{badarg,
	     [{erlang,link,[[]],[]},
	      {?MODULE,do_call_unsafe_bif,2,
	       [{file,File},{line,129}]},
	      {?MODULE,line_numbers,1,_}|_]}} =
	(catch do_call_unsafe_bif(x, [])),

    ok.

id(I) ->
    I.
