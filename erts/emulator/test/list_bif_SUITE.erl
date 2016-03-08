%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(list_bif_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
-export([hd_test/1,tl_test/1,t_length/1,t_list_to_pid/1,
	 t_list_to_float/1,t_list_to_integer/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].


all() -> 
    [hd_test, tl_test, t_length, t_list_to_pid,
     t_list_to_float, t_list_to_integer].

t_list_to_integer(suite) ->
    [];
t_list_to_integer(doc) ->
    ["tests list_to_integer and string:to_integer"];
t_list_to_integer(Config) when is_list(Config) ->
    ?line {'EXIT',{badarg,_}} = (catch list_to_integer("12373281903728109372810937209817320981321ABC")),
    ?line 12373281903728109372810937209817320981321 = (catch list_to_integer("12373281903728109372810937209817320981321")),
    ?line 12373 = (catch list_to_integer("12373")),
    ?line -12373 =  (catch list_to_integer("-12373")),
    ?line 12373 = (catch list_to_integer("+12373")),
    ?line {'EXIT',{badarg,_}} = ( catch list_to_integer(abc)),
    ?line {'EXIT',{badarg,_}} = (catch list_to_integer("")),
    ?line {12373281903728109372810937209817320981321,"ABC"} = string:to_integer("12373281903728109372810937209817320981321ABC"),
    ?line {-12373281903728109372810937209817320981321,"ABC"} = string:to_integer("-12373281903728109372810937209817320981321ABC"),
    ?line {12,[345]} = string:to_integer([$1,$2,345]),
    ?line {12,[a]} = string:to_integer([$1,$2,a]),
    ?line {error,no_integer} = string:to_integer([$A]),
    ?line {error,not_a_list} = string:to_integer($A),
    ok.

%% Test hd/1 with correct and incorrect arguments.
hd_test(Config) when is_list(Config) ->
    ?line $h = hd(id("hejsan")),
    ?line case catch hd(id($h)) of
	      {'EXIT', {badarg, _}} -> ok;
	      Res ->
		  ct:fail("hd/1 with incorrect args succeeded.~nResult: ~p", [Res])
	  end,
    ok.


%% Test tl/1 with correct and incorrect arguments.
tl_test(Config) when is_list(Config) ->
    ?line "ejsan" = tl(id("hejsan")),
    ?line case catch tl(id(104)) of
	      {'EXIT', {badarg, _}} ->
		  ok;
	      Res ->
		  ct:fail("tl/1 with incorrect args succeeded.~nResult: ~p", [Res])
	  end,
    ok.


%% Test length/1 with correct and incorrect arguments.

t_length(Config) when is_list(Config) ->
    ?line 0 = length(""),
    ?line 0 = length([]),
    ?line 1 = length([1]),
    ?line 2 = length([1,a]),
    ?line 2 = length("ab"),
    ?line 3 = length("abc"),
    ?line 4 = length(id([x|"abc"])),
    ?line 6 = length("hejsan"),
    ?line {'EXIT',{badarg,_}} = (catch length(id([a,b|c]))),
    ?line case catch length({tuple}) of
	      {'EXIT', {badarg, _}} ->
		  ok;
	      Res ->
		  ct:fail("length/1 with incorrect args succeeded.~nResult: ~p", [Res])
	  end,
    ok.
	      

%% Test list_to_pid/1 with correct and incorrect arguments.

t_list_to_pid(Config) when is_list(Config) ->
    ?line Me = self(),
    ?line MyListedPid = pid_to_list(Me),
    ?line Me = list_to_pid(MyListedPid),
    ?line case catch list_to_pid(id("Incorrect list")) of
	      {'EXIT', {badarg, _}} ->
		  ok;
	      Res ->
		  ct:fail("list_to_pid/1 with incorrect arg succeeded.~nResult: ~p", [Res])
	  end,
    ok.


%% Test list_to_float/1 with correct and incorrect arguments.

t_list_to_float(Config) when is_list(Config) ->
    ?line 5.89000 = list_to_float(id("5.89")),
    ?line 5.89898 = list_to_float(id("5.89898")),
    ?line case catch list_to_float(id("58")) of
	      {'EXIT', {badarg, _}} -> ok;
	      Res ->
		  ct:fail("list_to_float with incorrect arg succeeded.~nResult: ~p", [Res])
	  end,
    ok.

id(I) -> I.

    
