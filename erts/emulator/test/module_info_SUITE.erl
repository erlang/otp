%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(module_info_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
	 exports/1,functions/1,deleted/1,native/1,info/1]).

%%-compile(native).

%% Helper.
-export([native_proj/1,native_filter/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() -> 
    modules().

modules() ->
    [exports, functions, deleted, native, info].

%% Should return all functions exported from this module. (local)
all_exported() ->
    All = add_arity(modules()),
    lists:sort([{all,0},{suite,0},
                {module_info,0},{module_info,1},
                {native_proj,1},
                {native_filter,1}|All]).

%% Should return all functions in this module. (local)
all_functions() ->
    Locals = [{add_arity,1},{add_arity,2},{all_exported,0},{all_functions,0},
	      {modules,0}],
    lists:sort(Locals++all_exported()).

%% Test that the list of exported functions from this module is correct.
exports(Config) when is_list(Config) ->
    All = all_exported(),
    All = lists:sort(?MODULE:module_info(exports)),
    (catch ?MODULE:foo()),
    All = lists:sort(?MODULE:module_info(exports)),
    ok.

%% Test that the list of exported functions from this module is correct.
functions(Config) when is_list(Config) ->
    All = all_functions(),
    All = lists:sort(?MODULE:module_info(functions)),
    ok.

%% Test that deleted modules cause badarg
deleted(Config) when is_list(Config) ->
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "module_info_test"),
    {ok,module_info_test,Code} = compile:file(File, [binary]),
    {module,module_info_test} = erlang:load_module(module_info_test, Code),
    17 = module_info_test:f(),
    [_|_] = erlang:get_module_info(module_info_test, attributes),
    [_|_] = erlang:get_module_info(module_info_test),

    %% first delete it
    true = erlang:delete_module(module_info_test),
    {'EXIT',{undef, _}} = (catch module_info_test:f()),
    {'EXIT',{badarg, _}} = (catch erlang:get_module_info(module_info_test,attributes)),
    {'EXIT',{badarg, _}} = (catch erlang:get_module_info(module_info_test)),

    %% then purge it
    true = erlang:purge_module(module_info_test),
    {'EXIT',{undef, _}} = (catch module_info_test:f()),
    {'EXIT',{badarg, _}} = (catch erlang:get_module_info(module_info_test,attributes)),
    {'EXIT',{badarg, _}} = (catch erlang:get_module_info(module_info_test)),
    ok.

%% Test that the list of exported functions from this module is correct.
%% Verify that module_info(native) works.
native(Config) when is_list(Config) ->
    All = all_functions(),
    case ?MODULE:module_info(native_addresses) of
        [] ->
            false = ?MODULE:module_info(native),
            {comment,"no native functions"};
        L ->
            true = ?MODULE:module_info(native),
            %% Verify that all functions have unique addresses.
            S0 = sofs:set(L, [{name,arity,addr}]),
            S1 = sofs:projection({external,fun ?MODULE:native_proj/1}, S0),
            S2 = sofs:relation_to_family(S1),
            S3 = sofs:family_specification(fun ?MODULE:native_filter/1, S2),
            0 = sofs:no_elements(S3),
            S4 = sofs:range(S1),

            %% Verify that the set of function with native addresses
            %% is a subset of all functions in the module.
            AllSet = sofs:set(All, [{name,arity}]),
            true = sofs:is_subset(S4, AllSet),

            {comment,integer_to_list(sofs:no_elements(S0))++" native functions"}
    end.

native_proj({Name,Arity,Addr}) ->
    {Addr,{Name,Arity}}.

native_filter(Set) ->
    sofs:no_elements(Set) =/= 1.

%% Test that the module info of this module is correct. Use
%% erlang:get_module_info(?MODULE) to avoid compiler optimization tricks.
info(Config) when is_list(Config) ->
    Info = erlang:get_module_info(?MODULE),
    All = all_exported(),
    {ok,{?MODULE,MD5}} = beam_lib:md5(code:which(?MODULE)),
    {module, ?MODULE} = lists:keyfind(module, 1, Info),
    {md5, MD5} = lists:keyfind(md5, 1, Info),
    {exports, Exports} = lists:keyfind(exports, 1, Info),
    All = lists:sort(Exports),
    {attributes, Attrs} = lists:keyfind(attributes, 1, Info),
    {vsn,_} = lists:keyfind(vsn, 1, Attrs),
    {compile, Compile} = lists:keyfind(compile, 1, Info),
    {options,_} = lists:keyfind(options, 1, Compile),
    ok.

%% Helper functions (local).

add_arity(L) ->
    add_arity(L, []).

add_arity([H|T], Acc) ->
    add_arity(T, [{H,1}|Acc]);
add_arity([], Acc) -> lists:reverse(Acc).
