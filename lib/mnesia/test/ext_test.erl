%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-module(ext_test).

-include("mnesia_test_lib.hrl").
-include("ext_test_server.hrl").

%% Initializations
-export([init_backend/0, add_aliases/1, remove_aliases/1,
	 check_definition/4, semantics/2]).

-export([
	 create_table/3, load_table/4,
	 delete_table/2, close_table/2, sync_close_table/2,

	 sender_init/4,
	 receiver_first_message/4, receive_data/5, receive_done/4,

	 index_is_consistent/3, is_index_consistent/2,

	 real_suffixes/0, tmp_suffixes/0,

	 info/3,
	 fixtable/3,
	 validate_key/6, validate_record/6,

	 first/2, last/2, next/3, prev/3, slot/3,

	 insert/3, update_counter/4,
	 lookup/3,
	 delete/3, match_delete/3,
	 select/1, select/3, select/4,
	 select_reverse/3, select_reverse/4, repair_continuation/2
	]).

semantics(ext_ram_copies, storage) -> ram_copies;
semantics(ext_ram_copies, types  ) -> [set, ordered_set, bag];
semantics(ext_ram_copies, index_types) -> [ordered];
semantics(ext_disc_only_copies, storage) -> disc_only_copies;
semantics(ext_disc_only_copies, types  ) -> [set, bag];
semantics(ext_disc_only_copies, index_types) -> [bag];
semantics(_Alias, _) ->
    undefined.

init_backend() ->
    ?DBG(),
    %% cheat and stuff a marker in mnesia_gvar
    K = backend_init_marker(),
    case try ets:lookup_element(mnesia_gvar, K, 2) catch _:_ -> error end of
        error ->
            mnesia_lib:set(K, true),
            ok;
        Other ->
            {error, {backend_already_initialized, {?MODULE, Other}}}
    end.

backend_init_marker() ->
    {test, ?MODULE, backend_init}.

error_if_not_initialized() ->
    case try ets:lookup_element(mnesia_gvar, backend_init_marker(), 2) catch _:_ -> error end of
        error ->
            ?DBG({backend_not_initialized, {?MODULE, error}}),
            error({backend_not_initialized, {?MODULE, error}});
        _Other ->
            ok
    end.

add_aliases(_As) ->
    ?DBG(_As),
    case init_backend() of
        ok ->
            ok;
        _ ->
            ignore
    end,
    error_if_not_initialized(),
    true = mnesia_lib:val(backend_init_marker()),
    ok.

remove_aliases(_As) ->
    ?DBG(_As),
    error_if_not_initialized(),
    ok.


%% Table operations

check_definition(_Alias, _Tab, _Nodes, _Props) ->
    ?DBG({_Alias, ext_test_server:tab_to_list(_Tab), _Nodes, _Props}),
    ok.

create_table(Alias, Tab, Props) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Props}),
    try error_if_not_initialized() of
        ok ->
            call({?FUNCTION_NAME, Alias, Tab, Props})
    catch error : {backend_not_initialized, _} = Reason ->
        {aborted, Reason}
    end.

delete_table(Alias, Tab) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab)}),
    try error_if_not_initialized() of
        ok ->
            call({?FUNCTION_NAME, Alias, Tab})
    catch error : {backend_not_initialized, _} = Reason ->
        ok
    end.

load_table(Alias, Tab, LoadReason, Cs) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), LoadReason, Cs}),
    call({?FUNCTION_NAME, Alias, Tab, LoadReason, Cs}).

sender_init(Alias, Tab, RemoteStorage, Pid) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), RemoteStorage, Pid}),
    call({?FUNCTION_NAME, Alias, Tab, RemoteStorage, Pid, ?MODULE}).

receiver_first_message(Sender, {first, Size}, _Alias, Tab) ->
    ?DBG({Sender, {first, Size}, _Alias, ext_test_server:tab_to_list(Tab)}),
    error_if_not_initialized(),
    {Size, {Tab, Sender}}.

receive_data(Data, Alias, Name, Sender, State) ->
    ?DBG({Data, Alias, ext_test_server:tab_to_list(Name), Sender, State}),
    call({?FUNCTION_NAME, Data, Alias, Name, Sender, State}).

receive_done(_Alias, _Tab, _Sender, _State) ->
    ?DBG({_Alias, ext_test_server:tab_to_list(_Tab), _Sender, _State}),
    error_if_not_initialized(),
    ok.

close_table(Alias, Tab) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab)}),
    error_if_not_initialized(),
    sync_close_table(Alias, Tab).

sync_close_table(Alias, Tab) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab)}),
    call({?FUNCTION_NAME, Alias, Tab}).

fixtable(Alias, Tab, Bool) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Bool}),
    call({?FUNCTION_NAME, Alias, Tab, Bool}).

info(Alias, Tab, Type) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Type}),
    call({?FUNCTION_NAME, Alias, Tab, Type}).

real_suffixes() ->
    [".dat.ext"].

tmp_suffixes() ->
    [].

%% Index

index_is_consistent(Alias, Ix, Bool) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Ix), Bool}),
    call({?FUNCTION_NAME, Alias, Ix, Bool}).

is_index_consistent(Alias, Ix) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Ix)}),
    call({?FUNCTION_NAME, Alias, Ix}).

%% Record operations

validate_record(_Alias, _Tab, RecName, Arity, Type, _Obj) ->
    ?DBG({_Alias, ext_test_server:tab_to_list(_Tab), RecName, Arity, Type, _Obj}),
    error_if_not_initialized(),
    {RecName, Arity, Type}.

validate_key(_Alias, _Tab, RecName, Arity, Type, _Key) ->
    ?DBG({_Alias, ext_test_server:tab_to_list(_Tab), RecName, Arity, Type, _Key}),
    error_if_not_initialized(),
    {RecName, Arity, Type}.

insert(Alias, Tab, Obj) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Obj}),
    call({?FUNCTION_NAME, Alias, Tab, Obj}).

lookup(Alias, Tab, Obj) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Obj}),
    call({?FUNCTION_NAME, Alias, Tab, Obj}).

delete(Alias, Tab, Key) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Key}),
    call({?FUNCTION_NAME, Alias, Tab, Key}).

match_delete(Alias, Tab, Pat) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Pat}),
    call({?FUNCTION_NAME, Alias, Tab, Pat}).

first(Alias, Tab) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab)}),
    call({?FUNCTION_NAME, Alias, Tab}).

last(Alias, Tab) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab)}),
    error_if_not_initialized(),
    first(Alias, Tab).

next(Alias, Tab, Key) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Key}),
    call({?FUNCTION_NAME, Alias, Tab, Key}).

prev(Alias, Tab, Key) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Key}),
    error_if_not_initialized(),
    next(Alias, Tab, Key).

slot(Alias, Tab, Pos) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Pos}),
    call({?FUNCTION_NAME, Alias, Tab, Pos}).

update_counter(Alias, Tab, C, Val) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), C, Val}),
    call({?FUNCTION_NAME, Alias, Tab, C, Val}).

select(Continuation) ->
    ?DBG(Continuation),
    call({?FUNCTION_NAME, Continuation}).

select(Alias, Tab, Ms) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Ms}),
    call({?FUNCTION_NAME, Alias, Tab, Ms}).

select(Alias, Tab, Ms, Limit) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Ms, Limit}),
    call({?FUNCTION_NAME, Alias, Tab, Ms, Limit}).

select_reverse(Alias, Tab, Ms) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Ms}),
    call({?FUNCTION_NAME, Alias, Tab, Ms}).

select_reverse(Alias, Tab, Ms, Limit) ->
    ?DBG({Alias, ext_test_server:tab_to_list(Tab), Ms, Limit}),
    call({?FUNCTION_NAME, Alias, Tab, Ms, Limit}).

repair_continuation(Cont, Ms) ->
    ?DBG({Cont, Ms}),
    call({?FUNCTION_NAME, Cont, Ms}).

call(Req) ->
    error_if_not_initialized(),
    case gen_server:call({global, mnesia_test_lib:get_ext_test_server_name()}, Req) of
        #exception{c = Class, r = Reason, st = ST} = Ex ->
            ?DBG("call ~p resulted in an exception: ~p~n", [Req, Ex]),
            erlang:raise(Class, Reason, ST);
        Res ->
            Res
    end.
