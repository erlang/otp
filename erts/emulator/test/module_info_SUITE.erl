%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(module_info_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 exports/1,functions/1,native/1]).

%%-compile(native).

%% Helper.
-export([native_proj/1,native_filter/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    modules().

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


modules() -> 
    [exports, functions, native].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%% Should return all functions exported from this module. (local)
all_exported() ->
    All = add_arity(modules()),
    lists:sort([{all,0},{suite,0},{groups,0},
		{init_per_suite,1},{end_per_suite,1},
		{init_per_group,2},{end_per_group,2},
		{init_per_testcase,2},{end_per_testcase,2},
		{module_info,0},{module_info,1},{native_proj,1},
		{native_filter,1}|All]).

%% Should return all functions in this module. (local)
all_functions() ->
    Locals = [{add_arity,1},{add_arity,2},{all_exported,0},{all_functions,0},
	      {modules,0}],
    lists:sort(Locals++all_exported()).

%% Test that the list of exported functions from this module is correct.
exports(Config) when is_list(Config) ->
    ?line All = all_exported(),
    ?line All = lists:sort(?MODULE:module_info(exports)),
    ?line (catch ?MODULE:foo()),
    ?line All = lists:sort(?MODULE:module_info(exports)),
    ok.

%% Test that the list of exported functions from this module is correct.
functions(Config) when is_list(Config) ->
    ?line All = all_functions(),
    ?line All = lists:sort(?MODULE:module_info(functions)),
    ok.

%% Test that the list of exported functions from this module is correct.
native(Config) when is_list(Config) ->
    ?line All = all_functions(),
    ?line case ?MODULE:module_info(native_addresses) of
	      [] ->
		  {comment,"no native functions"};
	      L ->
		  %% Verify that all functions have unique addresses.
		  ?line S0 = sofs:set(L, [{name,arity,addr}]),
		  ?line S1 = sofs:projection({external,fun ?MODULE:native_proj/1}, S0),
		  ?line S2 = sofs:relation_to_family(S1),
		  ?line S3 = sofs:family_specification(fun ?MODULE:native_filter/1, S2),
		  ?line 0 = sofs:no_elements(S3),
		  ?line S4 = sofs:range(S1),

		  %% Verify that the set of function with native addresses
		  %% is a subset of all functions in the module.
		  ?line AllSet = sofs:set(All, [{name,arity}]),
		  ?line true = sofs:is_subset(S4, AllSet),
		  
		  {comment,integer_to_list(sofs:no_elements(S0))++" native functions"}
	  end.

native_proj({Name,Arity,Addr}) ->
    {Addr,{Name,Arity}}.

native_filter(Set) ->
    sofs:no_elements(Set) =/= 1.

%% Helper functions (local).

add_arity(L) ->
    add_arity(L, []).

add_arity([H|T], Acc) ->
    add_arity(T, [{H,1}|Acc]);
add_arity([], Acc) -> lists:reverse(Acc).
