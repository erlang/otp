%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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
%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bincomp_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



byte_aligned(Config) when is_list(Config) ->
    ?line <<"abcdefg">> = << <<(X+32)>> || <<X>> <= <<"ABCDEFG">> >>,
    ?line <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>,
    ?line <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>,
  ok.

bit_aligned(Config) when is_list(Config) ->
    ?line <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	<< <<(X+32):7>> || <<X>> <= <<"ABCDEFG">> >>,
    ?line <<"ABCDEFG">> =
	<< <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>,
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>,
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>,
  ok.

extended_byte_aligned(Config) when is_list(Config) ->
    ?line <<"abcdefg">> = << <<(X+32)>> || X <- "ABCDEFG" >>,
    ?line "abcdefg" = [(X+32) || <<X>> <= <<"ABCDEFG">>],
    ?line <<1:32/little,2:32/little,3:32/little,4:32/little>> =
	<< <<X:32/little>> || X <- [1,2,3,4] >>,
    ?line [256,512,768,1024] =
	[X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>],
  ok.

extended_bit_aligned(Config) when is_list(Config) ->
    ?line <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> =
	<< <<(X+32):7>> || X <- "ABCDEFG" >>,
    ?line "ABCDEFG" = [(X-32) || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>],
    ?line <<1:31/little,2:31/little,3:31/little,4:31/little>> =
	<< <<X:31/little>> || X <- [1,2,3,4] >>,
    ?line [256,512,768,1024] =
	[X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>],
    ok.

mixed(Config) when is_list(Config) ->
    ?line <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>,
    ?line <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>,
    ?line <<2,3,3,4,4,5,5,6>> =
	<< <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>,
    ?line [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>],
    ?line [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]],
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>> >>,
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>,
    ?line <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =
	<< <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>,
    ?line [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>],
    ?line [2,3,3,4,4,5,5,6] =
	[(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2]],
    ok.
