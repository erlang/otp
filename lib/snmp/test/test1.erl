%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(test1).

-compile(export_all).

bits1(get) ->
    {value, [b0, b2]}.
bits1(set, _) ->
    noError.

bits2(get) ->
    {value, 2#11000000110}.
bits2(set, _) ->
    noError.

bits3(get) ->
    {value, [b0, b4]}. % error!

bits4(get) ->
    {value, 2#1000}. % error!

opaque_obj(get) ->
    {value, "opaque-data"}.

cnt64(get) ->
    {value, 18446744073709551615}.

multiStr(get) ->
    i("multiStr(get) -> entry"),
    global:re_register_name(snmp_multi_tester, self()),
    i("multiStr(get) -> registered, now await continue"),
    receive
	continue -> 
	    i("multiStr(get) -> received continue"),
	    ok
    end,
    {value, "ok"}.

multiStr(set, "block") ->
    global:re_register_name(snmp_multi_tester, self()),
    receive
	continue -> ok
    end,
    noError;
multiStr(set, _Value) ->
    noError.


i(F) ->
    i(F, []).

i(F, A) ->
    io:format("~p ~w:" ++ F ++ "~n", [self(), ?MODULE | A]).

