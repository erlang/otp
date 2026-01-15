%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(ext_test_server).

-include("ext_test_server.hrl").

%% This process is supposed to emulate external database process, it should not be linked
%% to any mnesia process. It is expected that it survives mnesia restart.

-export([tab_to_list/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2, code_change/3]).

-record(state, {tables = maps:new()}).
-record(table, {state = undefined, tid = undefined, ix_consistent = false}).

-define(TRY(Operation), try Operation catch Class : Reason : ST -> #exception{c = Class, r = Reason, st = ST} end).

init(_) ->
    ?DBG(),
    {ok, #state{}}.

create_table(ext_ram_copies, Tab, Props, #state{tables = Tables} = State) when is_atom(Tab) ->
    case maps:get(Tab, Tables, undefined) of
        #table{state = opened, tid = _Tid} ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tab), _Tid]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tab)]),
            Tid = ets:new(Tab, [public, proplists:get_value(type, Props, set), {keypos, 2}]),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tab), Tid]),
            {ok, State#state{tables = maps:put(Tab, #table{state = opened, tid = Tid}, Tables)}}
    end;
create_table(ext_disc_only_copies, Tab, Props, #state{tables = Tables} = State) when is_atom(Tab) ->
    case maps:get(Tab, Tables, undefined) of
        #table{state = opened, tid = Tab} ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tab), Tab]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tab)]),
            File = tab_to_filename(Tab),
            {ok, Tab} = dets:open_file(Tab, [{type, proplists:get_value(type, Props, set)}, {keypos, 2}, {file, File}]),
            ?DBG("create_table Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tab)]),
            {ok, State#state{tables = maps:put(Tab, #table{state = opened, tid = Tab}, Tables)}}
    end;
create_table(ext_ram_copies, Tag={_Tab, index, {_Where, Type}}, _Opts, #state{tables = Tables} = State) ->
    case maps:get(Tag, Tables, undefined) of
        #table{state = opened, tid = _Tid} ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tag), _Tid]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            Tid = ets:new(tab_to_atom(Tag), [public, type_to_type_in_alias(ext_ram_copies, Type)]),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tag), Tid]),
            {ok, State#state{tables = maps:put(Tag, #table{state = opened, tid = Tid}, Tables)}}
    end;
create_table(ext_disc_only_copies, Tag={_Tab, index, {_Where, Type}}, _Opts, #state{tables = Tables} = State) ->
    case maps:get(Tag, Tables, undefined) of
        #table{state = opened, tid = Tag} ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tag), Tag]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            File = tab_to_filename(Tag),
            {ok, Tag} = dets:open_file(Tag, [{type, type_to_type_in_alias(ext_disc_only_copies, Type)}, {file, File}]),
            ?DBG("create_table Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tag)]),
            {ok, State#state{tables = maps:put(Tag, #table{state = opened, tid = Tag}, Tables)}}
    end;
create_table(ext_ram_copies, Tag={_Tab, retainer, _ChkPName}, _Opts, #state{tables = Tables} = State) ->
    case maps:get(Tag, Tables, undefined) of
        #table{state = opened, tid = _Tid} ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tag), _Tid]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_ram_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            Tid = ets:new(tab_to_atom(Tag), [set, public, {keypos, 2}]),
            ?DBG("create_table, Alias, ext_ram_copies, Tab: ~p(~p)~n", [tab_to_list(Tag), Tid]),
            {ok, State#state{tables = maps:put(Tag, #table{state = opened, tid = Tid}, Tables)}}
    end;
create_table(ext_disc_only_copies, Tag={_Tab, retainer, _ChkPName}, _Opts, #state{tables = Tables} = State) ->
    case maps:get(Tag, Tables, undefined) of
        #table{state = opened, tid = Tag} ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p(~p) is already opened~n", [tab_to_list(Tag), Tag]),
            {ok, State};
        _ ->
            ?DBG("create_table, Alias: ext_disc_only_copies, Tab: ~p~n", [tab_to_list(Tag)]),
            File = tab_to_filename(Tag),
            {ok, Tag} = dets:open_file(Tag, [{type, set}, {keypos, 2}, {file, File}]),
            ?DBG("create_table, Alias: ext_disc_only_copies after dets:open_file, Tab: ~p~n", [tab_to_list(Tag)]),
            {ok, State#state{tables = maps:put(Tag, #table{state = opened, tid = Tag}, Tables)}}
    end.

receive_data(Data, ext_ram_copies, Name, Sender, {Name, Tab, Sender} = _MnesiaState, State) ->
    ?DBG({Data, ext_ram_copies, Name, Sender, {Name, tab_to_list(Tab), Sender}}),
    true = ets:insert(tab_to_tid(Tab, State), Data),
    {more, State};
receive_data(Data, ext_disc_only_copies, Name, Sender, {Name, Tab, Sender} = _MnesiaState, State) ->
    ?DBG({Data, ext_disc_only_copies, Name, Sender, {Name, tab_to_list(Tab), Sender}}),
    ok = dets:insert(tab_to_tid(Tab, State), Data),
    {more, State};
receive_data(Data, Alias, Tab, Sender, {Name, Sender} = _MnesiaState, State) ->
    ?DBG({Data, Alias, tab_to_list(Tab), State}),
    receive_data(Data, Alias, Tab, Sender, {Name, Tab, Sender}, State).

select(Alias, Tab, Ms, State, Dir) ->
    Res = select(Alias, Tab, Ms, 100000, State, Dir),
    select_1(Alias, Res, Dir).

select_1(_Alias, '$end_of_table', _Dir) -> [];
select_1(ext_ram_copies, {Acc, C}, forward) ->
    case ets:select(C) of
        '$end_of_table' -> Acc;
        {New, Cont} ->
            select_1(ext_ram_copies, {New ++ Acc, Cont}, forward)
    end;
select_1(ext_ram_copies, {Acc, C}, reverse) ->
    case ets:select_reverse(C) of
        '$end_of_table' -> Acc;
        {New, Cont} ->
            select_1(ext_ram_copies, {New ++ Acc, Cont}, reverse)
    end;
select_1(ext_disc_only_copies, {Acc, C}, Dir) ->
    case dets:select(C) of
        '$end_of_table' -> Acc;
        {New, Cont} ->
            select_1(ext_disc_only_copies, {New ++ Acc, Cont}, Dir)
    end.

select(ext_ram_copies, Tab, Ms, Limit, State, forward) when is_integer(Limit); Limit =:= infinity ->
    ?DBG({ext_ram_copies, tab_to_list(Tab), Ms, Limit}),
    ets:select(tab_to_tid(Tab, State), Ms, Limit);
select(ext_ram_copies, Tab, Ms, Limit, State, reverse) when is_integer(Limit); Limit =:= infinity ->
    ?DBG({ext_ram_copies, tab_to_list(Tab), Ms, Limit}),
    ets:select_reverse(tab_to_tid(Tab, State), Ms, Limit);
select(ext_disc_only_copies, Tab, Ms, Limit, State, _Dir) when is_integer(Limit); Limit =:= infinity ->
    ?DBG({ext_disc_only_copies, tab_to_list(Tab), Ms, Limit}),
    dets:select(tab_to_tid(Tab, State), Ms, Limit).

handle_call({create_table, Alias, Tab, Props}, _From, State) ->
    ?DBG({create_table, Alias, tab_to_list(Tab), Props}),
    case ?TRY(create_table(Alias, Tab, Props, State)) of
        #exception{} = Res ->
            {reply, Res, State};
        {Res, NewState} ->
            {reply, Res, NewState}
    end;

handle_call({delete_table, ext_ram_copies, Tab}, _From, #state{tables = Tables} = State) ->
    ?DBG({delete_table, ext_ram_copies, tab_to_list(Tab)}),
    case maps:get(Tab, Tables, undefined) of
        #table{state = T, tid = Tid} when T == opened; T == created ->
            case ?TRY(ets:delete(Tid)) of
                #exception{} = Res ->
                    {reply, Res, State};
                _Res ->
                    NewState = State#state{tables = maps:remove(Tab, Tables)},
                    {reply, ok, NewState}
            end;
        undefined ->
            {reply, ok, State}
    end;
handle_call({delete_table, ext_disc_only_copies, Tab}, _From, #state{tables = Tables} = State) ->
    ?DBG({delete_table, ext_disc_only_copies, tab_to_list(Tab)}),
    case maps:get(Tab, Tables, undefined) of
        #table{state = opened, tid = Tid} ->
            Results = [?TRY(dets:sync(Tid)), ?TRY(dets:close(Tid)), ?TRY(file:delete(tab_to_filename(Tab)))],
            case lists:search(fun(Elem) -> is_record(Elem, exception) end, Results) of
                {value, Exception} ->
                    {reply, Exception, State};
                _ ->
                    case lists:search(fun({error, _}) -> true; (_) -> false end, Results) of
                        {value, Error} ->
                            {reply, Error, State};
                        _ ->
                            NewState = State#state{tables = maps:remove(Tab, Tables)},
                            {reply, ok, NewState}
                    end
                end;
        #table{state = created} ->
            case ?TRY(file:delete(tab_to_filename(Tab))) of
                ok ->
                    NewState = State#state{tables = maps:remove(Tab, Tables)},
                    {reply, ok, NewState};
                Res ->
                    {reply, Res, State}
            end;
        undefined ->
            {reply, ok, State}
    end;

handle_call({load_table, Alias, Tab, _LoadReason, Cs}, _From, State) ->
    ?DBG({load_table, Alias, tab_to_list(Tab), _LoadReason, Cs}),
    {Res, NewState} = create_table(Alias, Tab, mnesia_schema:cs2list(Cs), State),
    {reply, Res, NewState};

handle_call({sender_init, Alias, Tab, _RemoteStorage, _Pid, Module}, _From, State) ->
    ?DBG({sender_init, Alias, tab_to_list(Tab), _RemoteStorage, _Pid}),
    KeysPerTransfer = 100,
    Res = {standard,
        fun() -> mnesia_lib:db_init_chunk({ext, Alias, Module}, Tab, KeysPerTransfer) end,
        fun(Cont) -> mnesia_lib:db_chunk({ext, Alias, Module}, Cont) end},
    {reply, Res, State};

handle_call({receive_data, Data, Alias, Name, Sender, MnesiaState}, _From, State) ->
    ?DBG({receive_data, Data, Alias, Name, Sender, MnesiaState}),
    Res = ?TRY(receive_data(Data, Alias, Name, Sender, MnesiaState, State)),
    {reply, Res, State};

handle_call({sync_close_table, ext_ram_copies, Tab}, _From, #state{tables = Tables} = State) ->
    ?DBG({sync_close_table, ext_ram_copies, tab_to_list(Tab)}),
    NewState = State#state{tables = maps:remove(Tab, Tables)},
    {reply, ok, NewState};
handle_call({sync_close_table, ext_disc_only_copies, Tab}, _From, #state{tables = Tables} = State) ->
    ?DBG({sync_close_table, ext_disc_only_copies, tab_to_list(Tab)}),
    case maps:get(Tab, Tables, undefined) of
        #table{state = opened, tid = Tid} = Table ->
            Results = [?TRY(dets:sync(Tid)), ?TRY(dets:close(Tid))],
            case lists:search(fun(Elem) -> is_record(Elem, exception) end, Results) of
                {value, Exception} ->
                    {reply, Exception, State};
                _ ->
                    case lists:search(fun({error, _}) -> true; (_) -> false end, Results) of
                        {value, Error} ->
                            {reply, Error, State};
                        _ ->
                            NewState = State#state{tables = maps:put(Tab, Table#table{state = created}, Tables)},
                            {reply, ok, NewState}
                    end
            end;
        _ ->
            {reply, ok, State}
    end;

handle_call({fixtable, ext_ram_copies, Tab, Bool}, _From, State) ->
    ?DBG({fixtable, ext_ram_copies, tab_to_list(Tab), Bool}),
    Res = ?TRY(ets:safe_fixtable(tab_to_tid(Tab, State), Bool)),
    {reply, Res, State};
handle_call({fixtable, ext_disc_only_copies, Tab, Bool}, _From, State) ->
    ?DBG({fixtable, ext_disc_only_copies, tab_to_list(Tab), Bool}),
    Res = ?TRY(dets:safe_fixtable(tab_to_tid(Tab, State), Bool)),
    {reply, Res, State};

handle_call({info, ext_ram_copies, Tab, Type}, _From, State) ->
    ?DBG({info, ext_ram_copies, tab_to_list(Tab), Type}),
    Tid = tab_to_tid(Tab, State),
    Res = try ets:info(Tid, Type) of
        Val -> Val
    catch _:_ ->
        undefined
    end,
    {reply, Res, State};
handle_call({info, ext_disc_only_copies, Tab, Type}, _From, State) ->
    ?DBG({info, ext_disc_only_copies, tab_to_list(Tab), Type}),
    Tid = tab_to_tid(Tab, State),
    Res = try dets:info(Tid, Type) of
        Val -> Val
    catch _:_ ->
        undefined
    end,
    {reply, Res, State};

handle_call({index_is_consistent, _Alias, Ix, Bool}, _From, #state{tables = Tables} = State) ->
    ?DBG({index_is_consistent, _Alias, ext_test_server:tab_to_list(Ix), Bool}),
    Table = maps:get(Ix, Tables),
    NewTable = Table#table{ix_consistent = Bool},
    NewState = State#state{tables = maps:put(Ix, NewTable, Tables)},
    {reply, ok, NewState};

handle_call({is_index_consistent, _Alias, Ix}, _From, #state{tables = Tables} = State) ->
    ?DBG({is_index_consistent, _Alias, tab_to_list(Ix)}),
    case maps:get(Ix, Tables, undefined) of
        #table{ix_consistent = IxConsistent} ->
            {reply, IxConsistent, State};
        _ ->
            {reply, false, State}
    end;

handle_call({insert, ext_ram_copies, Tab, Obj}, _From, State) ->
    ?DBG({insert, ext_ram_copies, tab_to_list(Tab), Obj}),
    case ?TRY(ets:insert(tab_to_tid(Tab, State), Obj)) of
        true ->
            {reply, ok, State};
        Res ->
            {reply, Res, State}
    end;
handle_call({insert, ext_disc_only_copies, Tab, Obj}, _From, State) ->
    ?DBG({insert, ext_disc_only_copies, tab_to_list(Tab), Obj}),
    Res = ?TRY(dets:insert(tab_to_tid(Tab, State), Obj)),
    {reply, Res, State};

handle_call({lookup, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({lookup, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ?TRY(ets:lookup(tab_to_tid(Tab, State), Key)),
    {reply, Res, State};
handle_call({lookup, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({lookup, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = ?TRY(dets:lookup(tab_to_tid(Tab, State), Key)),
    {reply, Res, State};

handle_call({delete, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({delete, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ets:delete(tab_to_tid(Tab, State), Key),
    {reply, Res, State};
handle_call({delete, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({delete, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = ?TRY(dets:delete(tab_to_tid(Tab, State), Key)),
    {reply, Res, State};

handle_call({match_delete, ext_ram_copies, Tab, Pat}, _From, State) ->
    ?DBG({match_delete, ext_ram_copies, tab_to_list(Tab), Pat}),
    Res = ?TRY(ets:match_delete(tab_to_tid(Tab, State), Pat)),
    {reply, Res, State};
handle_call({match_delete, ext_disc_only_copies, Tab, Pat}, _From, State) ->
    ?DBG({match_delete, ext_disc_only_copies, tab_to_list(Tab), Pat}),
    Res = ?TRY(dets:match_delete(tab_to_tid(Tab, State), Pat)),
    {reply, Res, State};

handle_call({first, ext_ram_copies, Tab}, _From, State) ->
    ?DBG({first, ext_ram_copies, tab_to_list(Tab)}),
    Res = ?TRY(ets:first(tab_to_tid(Tab, State))),
    {reply, Res, State};
handle_call({first, ext_disc_only_copies, Tab}, _From, State) ->
    ?DBG({first, ext_disc_only_copies, tab_to_list(Tab)}),
    Res = ?TRY(dets:first(tab_to_tid(Tab, State))),
    {reply, Res, State};

handle_call({next, ext_ram_copies, Tab, Key}, _From, State) ->
    ?DBG({next, ext_ram_copies, tab_to_list(Tab), Key}),
    Res = ?TRY(ets:next(tab_to_tid(Tab, State), Key)),
    {reply, Res, State};
handle_call({next, ext_disc_only_copies, Tab, Key}, _From, State) ->
    ?DBG({next, ext_disc_only_copies, tab_to_list(Tab), Key}),
    Res = ?TRY(dets:next(tab_to_tid(Tab, State), Key)),
    {reply, Res, State};

handle_call({slot, ext_ram_copies, Tab, Pos}, _From, State) ->
    ?DBG({slot, ext_ram_copies, tab_to_list(Tab), Pos}),
    Res = ?TRY(ets:slot(tab_to_tid(Tab, State), Pos)),
    {reply, Res, State};
handle_call({slot, ext_disc_only_copies, Tab, Pos}, _From, State) ->
    ?DBG({slot, ext_disc_only_copies, tab_to_list(Tab), Pos}),
    Res = ?TRY(dets:slot(tab_to_tid(Tab, State), Pos)),
    {reply, Res, State};

handle_call({update_counter, ext_ram_copies, Tab, C, Val}, _From, State) ->
    ?DBG({update_counter, ext_ram_copies, tab_to_list(Tab), C, Val}),
    Res = ?TRY(ets:update_counter(tab_to_tid(Tab, State), C, Val)),
    {reply, Res, State};
handle_call({update_counter, ext_disc_only_copies, Tab, C, Val}, _From, State) ->
    ?DBG({update_counter, ext_disc_only_copies, tab_to_list(Tab), C, Val}),
    Res = ?TRY(dets:update_counter(tab_to_tid(Tab, State), C, Val)),
    {reply, Res, State};

handle_call({select, '$end_of_table' = End}, _From, State) ->
    ?DBG({select, End}),
    {reply, End, State};
handle_call({select, C}, _From, State) when element(1, C) == dets_cont ->
    ?DBG({select, {ext_disc_only_copies, C}}),
    Res = ?TRY(dets:select(C)),
    {reply, Res, State};
handle_call({select, C}, _From, State) ->
    ?DBG({select, {ext_ram_copies, C}}),
    Res = ?TRY(ets:select(C)),
    {reply, Res, State};

handle_call({select, Alias, Tab, Ms}, _From, State) ->
    ?DBG({select, Alias, tab_to_list(Tab), Ms}),
    Res = ?TRY(select(Alias, Tab, Ms, State, forward)),
    {reply, Res, State};

handle_call({select, Alias, Tab, Ms, Limit}, _From, State) ->
    ?DBG({select, Alias, tab_to_list(Tab), Ms, Limit}),
    Res = ?TRY(select(Alias, Tab, Ms, Limit, State, forward)),
    {reply, Res, State};

handle_call({select_reverse, Alias, Tab, Ms}, _From, State) ->
    ?DBG({select_reverse, Alias, tab_to_list(Tab), Ms}),
    Res = ?TRY(select(Alias, Tab, Ms, State, reverse)),
    {reply, Res, State};

handle_call({select_reverse, Alias, Tab, Ms, Limit}, _From, State) ->
    ?DBG({select_reverse, Alias, tab_to_list(Tab), Ms, Limit}),
    Res = ?TRY(select(Alias, Tab, Ms, Limit, State, reverse)),
    {reply, Res, State};

handle_call({repair_continuation, '$end_of_table' = Cont, _Ms}, _From, State) ->
    ?DBG({repair_continuation, Cont, _Ms}),
    {reply, Cont, State};
handle_call({repair_continuation, Cont, Ms}, _From, State) when element(1, Cont) == dets_cont ->
    ?DBG({repair_continuation, ext_disc_only_copies, Cont, Ms}),
    Res = ?TRY(dets:repair_continuation(Cont, Ms)),
    {reply, Res, State};
handle_call({repair_continuation, Cont, Ms}, _From, State) ->
    ?DBG({repair_continuation, ext_ram_copies, Cont, Ms}),
    Res = ?TRY(ets:repair_continuation(Cont, Ms)),
    {reply, Res, State}.

terminate(_Reason, _State) ->
    ?DBG(_Reason).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

tab_to_atom(Tab) when is_atom(Tab) ->
    Tab;
tab_to_atom(Tab) ->
    list_to_atom(tab_to_list(Tab)).
tab_to_list(Tab) when is_atom(Tab) ->
    atom_to_list(Tab);
tab_to_list({Tab, index, {Where, Type}}) ->
    atom_to_list(Tab) ++ "_index_" ++ integer_to_list(Where) ++ "_" ++ atom_to_list(Type);
tab_to_list({Tab, retainer, {ChkPNumber, Node}}) ->
    atom_to_list(Tab) ++ "_retainer_" ++ integer_to_list(ChkPNumber) ++ "_" ++ atom_to_list(Node);
tab_to_list({Tab, retainer, ChkPName}) ->
    atom_to_list(Tab) ++ "_retainer_" ++ atom_to_list(ChkPName).

type_to_type_in_alias(ext_ram_copies, ordered) ->
    ordered_set;
type_to_type_in_alias(_, Type) ->
    Type.

tab_to_filename(Tab) ->
    FName = tab_to_list(Tab) ++ ".dat.ext",
    mnesia_lib:dir(FName).

tab_to_tid(Tab, #state{tables = Tables}) ->
    Table = maps:get(Tab, Tables),
    Table#table.tid.
