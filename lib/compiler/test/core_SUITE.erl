%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
-module(core_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 dehydrated_itracer/1,nested_tries/1,
	 make_effect_seq/1,eval_is_boolean/1,
	 unsafe_case/1,nomatch_shadow/1]).

-include_lib("test_server/include/test_server.hrl").

-define(comp(N),
	N(Config) when is_list(Config) -> try_it(N, Config)).

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(5)),
    [{watchdog,Dog}|Config].

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [dehydrated_itracer,nested_tries,make_effect_seq,
     eval_is_boolean,unsafe_case,nomatch_shadow].

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


?comp(dehydrated_itracer).
?comp(nested_tries).
?comp(make_effect_seq).
?comp(eval_is_boolean).
?comp(unsafe_case).
?comp(nomatch_shadow).

try_it(Mod, Conf) ->
    Src = filename:join(?config(data_dir, Conf), atom_to_list(Mod)),
    compile_and_load(Src, []),
    compile_and_load(Src, [no_copt]).

compile_and_load(Src, Opts) ->
    {ok,Mod,Bin} = compile:file(Src, [from_core,report,time,binary|Opts]),
    {module,Mod} = code:load_binary(Mod, Mod, Bin),
    ok = Mod:Mod().
