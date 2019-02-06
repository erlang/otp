%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
-module(beam_jump_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 undefined_label/1,ambiguous_catch_try_state/1,
         build_tuple/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [undefined_label,
       ambiguous_catch_try_state,
       build_tuple
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

undefined_label(_Config) ->
    {'EXIT',{function_clause,_}} = (catch flights(0, [], [])),
    ok.

%% Would lose a label when compiled with no_copt.

flights(0, [], []) when [], 0; 0.0, [], false ->
    clark;
flights(_, Reproduction, introduction) when false, Reproduction ->
    responsible.

%% [ERL-209] beam_jump would share 'catch' blocks, causing an
%% ambiguous_catch_try_state error in beam_validator.

ambiguous_catch_try_state(_Config) ->
    {{'EXIT',{{case_clause,song},_}},{'EXIT',{{case_clause,song},_}}} =
	checks(42),
    ok.

river() -> song.

checks(Wanted) ->
    %% Must be one line to cause the unsafe optimization.
    {catch case river() of sheet -> begin +Wanted, if "da" -> Wanted end end end, catch case river() of sheet -> begin + Wanted, if "da" -> Wanted end end end}.

-record(message2, {id, p1}).
-record(message3, {id, p1, p2}).

build_tuple(_Config) ->
    {'EXIT',{{badrecord,message3},_}} = (catch do_build_tuple(#message2{})),
    ok.

do_build_tuple(Message) ->
    if is_record(Message, message2) ->
	    Res = {res, rand:uniform(100)},
	    {Message#message3.id, Res}
    end.
