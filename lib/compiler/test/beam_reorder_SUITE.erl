%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(beam_reorder_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 alloc/1,confused_beam_validator/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [alloc,
       confused_beam_validator
      ]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-record(alloc, {version}).

alloc(_Config) ->
    {ok,42} = alloc_a(1, 2, #alloc{version=42}),
    {a,b,c} = alloc_b(1, 2, #alloc{version={a,b,c}}),
    ok.

alloc_a(_U1, _U2, R) ->
    V = R#alloc.version,
    Res = id({ok,V}),
    _ = id(0),
    Res.

alloc_b(_U1, _U2, R) ->
    V = R#alloc.version,
    Res = id(V),
    _ = id(0),
    Res.

confused_beam_validator(_Config) ->
    {'EXIT',{{badmap,{any}},_}} = (catch efficient({any})),
    ok.

efficient({Var}=God) ->
    id(God#{}),
    catch
	receive _ ->
		Var
	end.

id(I) ->
    I.
