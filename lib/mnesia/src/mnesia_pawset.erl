%%
%% This module implements the pure operation-based add-wins set (pawset) based
%% on the %% paper by Baquero et al. 2017. It is used to achieve eventual
%% consistency with the new (a)sync_ec API.

-module(mnesia_pawset).

-include("mnesia.hrl").

-export([db_put/3, db_erase/3, db_get/2, db_first/1, db_last/1, db_next_key/2,
         db_select/2, db_prev_key/2, db_all_keys/1, db_match_erase/3]).
-export([mktab/2, unsafe_mktab/2]).
-export([remote_match_object/2, remote_ec_select/2]).
-export([index_match_object/3, index_read/3]).

-type op() :: write | delete.
-type val() :: any().
-type ts() :: mnesia_causal:vclock().
-type element() :: {ts(), {op(), val()}}.
-type storage() :: ram_copies | disk_copies | disc_only_copies | {ext, atom(), atom()}.

-import(mnesia_lib, [important/2, dbg_out/2, verbose/2, warning/2]).

val(Var) ->
    case ?catch_val_and_stack(Var) of
        {'EXIT', Stacktrace} ->
            mnesia_lib:other_val(Var, Stacktrace);
        Value ->
            Value
    end.

has_var(Pat) ->
    mnesia:has_var(Pat).

mktab(Tab, [{keypos, 2}, public, named_table, Type | EtsOpts])
    when Type =:= pawset orelse Type =:= pawbag ->
    Args1 = [{keypos, 2}, public, named_table, bag | EtsOpts],
    mnesia_monitor:mktab(Tab, Args1).

unsafe_mktab(Tab, [{keypos, 2}, public, named_table, Type | EtsOpts])
    when Type =:= pawset orelse Type =:= pawbag ->
    Args1 = [{keypos, 2}, public, named_table, bag | EtsOpts],
    mnesia_monitor:unsafe_mktab(Tab, Args1).

%%==================== writes ====================
-spec effect(storage(), mnesia:table(), tuple()) -> ok.
effect(Storage, Tab, Tup) ->
    case causal_compact(Storage, Tab, obj2ele(Tup)) of
        true ->
            mnesia_lib:db_put(Storage, Tab, remove_op(Tup)),
            ok;
        false ->
            ok
    end.

db_put(Storage, Tab, Obj) when is_map(element(tuple_size(Obj), Obj)) ->
    try
        Tup = add_op(Obj, write),
        dbg_out("inserting ~p into ~p~n", [Tup, Tab]),
        effect(Storage, Tab, Tup)
    catch
        _:Reason ->
            warning("CRASH ~p ~p~n", [Reason, Tab])
    end;
db_put(_S, _T, Obj) ->
    error("bad tuple ~p, no ts at the end~n", [Obj]).

db_erase(Storage, Tab, Obj) when is_map(element(tuple_size(Obj), Obj)) ->
    Tup = add_op(Obj, delete),
    effect(Storage, Tab, Tup);
db_erase(_S, _T, Obj) ->
    error("bad tuple ~p, no ts at the end~n", [Obj]).

% used for delete_object, hence pattern cannot be a pattern
db_match_erase(Storage, Tab, Pat) ->
    db_erase(Storage, Tab, Pat).

%%==================== reads ====================
db_get(Tab, Key) ->
    Tups = mnesia_lib:db_get(Tab, Key),
    Res = [remove_ts(Tup) || Tup <- Tups],
    uniq(Tab, Res).

db_all_keys(Tab) when is_atom(Tab), Tab /= schema ->
    Pat0 = val({Tab, wild_pattern}),
    Pat = setelement(2, Pat0, '$1'),
    Keys = db_select(Tab, [{Pat, [], ['$1']}]),
    case val({Tab, setorbag}) of
        pawbag ->
            mnesia_lib:uniq(Keys);
        pawset ->
            Keys;
        Other ->
            error({incorrect_ec_table_type, Other})
    end.

db_match_object(Tab, Pat0) ->
    Pat = wild_ts(Pat0),
    Res = mnesia_lib:db_match_object(Tab, Pat),
    case val({Tab, setorbag}) of
        pawset ->
            Res1 = [remove_ts(Tup) || Tup <- Res],
            uniq(Tab, Res1);
        pawbag ->
            [remove_ts(Tup) || Tup <- Res];
        Other ->
            error({bad_val, Other})
    end.

db_first(Tab) ->
    mnesia_lib:db_first(Tab).

db_last(Tab) ->
    mnesia_lib:db_last(Tab).

db_next_key(Tab, Key) ->
    mnesia_lib:db_next_key(Tab, Key).

db_prev_key(Tab, Key) ->
    mnesia_lib:db_prev_key(Tab, Key).

db_select(Tab, Spec) ->
    Spec1 = [{wild_ts(MatchHead), Guards, Results} || {MatchHead, Guards, Results} <- Spec],
    mnesia_lib:db_select(Tab, Spec1).

index_read(Tab, Key, Attr) ->
    Tups = mnesia_index:dirty_read(Tab, Key, Attr),
    [remove_ts(Tup) || Tup <- Tups].

index_match_object(Tab, Pat, Pos) ->
    Pat2 = wild_ts(Pat),
    Tups = mnesia_index:dirty_match_object(Tab, Pat2, Pos),
    [remove_ts(Tup) || Tup <- Tups].

uniq(Tab, Res) ->
    case val({Tab, setorbag}) of
        pawset ->
            Res1 = mnesia_lib:uniq(Res),
            resolve_cc_add(Res1);
        pawbag ->
            Res;
        Other ->
            error({bad_val, Other})
    end.

-spec resolve_cc_add([element()]) -> [element()].
resolve_cc_add(Res) when length(Res) =< 1 ->
    Res;
resolve_cc_add(Res) ->
    dbg_out("selecting minimum element from ~p to resolve concurrent addition~n", [Res]),
    [lists:min(Res)].

remote_match_object(Tab, Pat) ->
    Key = element(2, Pat),
    case has_var(Key) of
        false ->
            db_match_object(Tab, Pat);
        true ->
            PosList = regular_indexes(Tab),
            remote_match_object(Tab, Pat, PosList)
    end.

remote_match_object(Tab, Pat, [Pos | Tail]) when Pos =< tuple_size(Pat) ->
    IxKey = element(Pos, Pat),
    case has_var(IxKey) of
        false ->
            Tups = mnesia_index:dirty_match_object(Tab, wild_ts(Pat), Pos),
            [remove_ts(Tup) || Tup <- Tups];
        true ->
            remote_match_object(Tab, Pat, Tail)
    end;
remote_match_object(Tab, Pat, []) ->
    db_match_object(Tab, Pat);
remote_match_object(Tab, Pat, _PosList) ->
    mnesia:abort({bad_type, Tab, Pat}).

remote_ec_select(Tab, Spec) ->
    case Spec of
        [{HeadPat, _, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
            Key = element(2, HeadPat),
            case has_var(Key) of
                false ->
                    db_select(Tab, Spec);
                true ->
                    PosList = regular_indexes(Tab),
                    remote_ec_select(Tab, Spec, PosList)
            end;
        _ ->
            db_select(Tab, Spec)
    end.

remote_ec_select(Tab, [{HeadPat, _, _}] = Spec, [Pos | Tail])
    when is_tuple(HeadPat), tuple_size(HeadPat) > 2, Pos =< tuple_size(HeadPat) ->
    Key = element(Pos, HeadPat),
    case has_var(Key) of
        false ->
            Recs = mnesia_index:dirty_select(Tab, wild_ts(HeadPat), Pos),
            Spec2 = [{wild_ts(MatchHead), Guards, Results} || {MatchHead, Guards, Results} <- Spec],
            CMS = ets:match_spec_compile(Spec2),
            ets:match_spec_run(Recs, CMS);
        true ->
            remote_ec_select(Tab, Spec, Tail)
    end;
remote_ec_select(Tab, Spec, _) ->
    db_select(Tab, Spec).

regular_indexes(Tab) ->
    PosList = val({Tab, index}),
    [P || P <- PosList, is_integer(P)].

%%% ==========pure op-based awset implementation==========

%% @returns whether this element should be added
-spec causal_compact(storage(), mnesia:table(), element()) -> boolean().
causal_compact(Storage, Tab, Ele) ->
    {Pid1, Mref1} =
        spawn_monitor(fun() ->
                         remove_obsolete(Storage, Tab, Ele),
                         receive {Parent, Mref} -> Parent ! {Mref, {obsolete, ok}} end
                      end),
    {Pid2, Mref2} =
        spawn_monitor(fun() ->
                         Red = redundant(Storage, Tab, Ele),
                         receive {Parent, Mref} -> Parent ! {Mref, {redundancy, Red}} end
                      end),
    Parent = self(),
    Pid1 ! {Parent, Mref1},
    Pid2 ! {Parent, Mref2},
    Red1 = wait_causal_compact(Mref1),
    Red2 = wait_causal_compact(Mref2),
    Red1 andalso Red2.

wait_causal_compact(Mref) ->
    receive
        {Mref, {obsolete, ok}} ->
            erlang:demonitor(Mref, [flush]),
            true;
        {Mref, {redundancy, Red}} ->
            erlang:demonitor(Mref, [flush]),
            not Red;
        {'DOWN', Mref, _, _, Reason} ->
            {error, Reason}
    end.

%% @doc checks whether the input element is redundant
%% i.e. if there are any other elements in the table that obsoletes this element
%% @end
-spec redundant(storage(), mnesia:table(), element()) -> boolean().
redundant(_Storage, _Tab, {_Ts, {delete, _Val}}) ->
    true;
redundant(Storage, Tab, Ele) ->
    Key = get_val_key(Tab, get_val_ele(Ele)),
    Tups = [add_op(Tup, write) || Tup <- mnesia_lib:db_get(Storage, Tab, Key)],
    Eles2 = lists:map(fun obj2ele/1, Tups),
    lists:any(fun(Ele2) -> obsolete(Tab, Ele, Ele2) end, Eles2).

%% @edoc removes elements that are obsoleted by Ele
-spec remove_obsolete(storage(), mnesia:table(), element()) -> ok.
remove_obsolete(Storage, Tab, Ele) ->
    Key = get_val_key(Tab, get_val_ele(Ele)),
    case mnesia_lib:db_get(Storage, Tab, Key) of
        [] ->
            ok;
        Tups when length(Tups) > 0 ->
            Tups2 = [add_op(Tup, write) || Tup <- Tups],
            Keep = lists:filter(fun(Tup) -> not obsolete(Tab, obj2ele(Tup), Ele) end, Tups2),
            Keep2 = [remove_op(Tup) || Tup <- Keep],
            mnesia_lib:db_erase(Storage, Tab, Key),
            mnesia_lib:db_put(Storage, Tab, Keep2),
            ok
    end.

key_pos(Tab) ->
    case val({Tab, storage_type}) of
        ram_copies ->
            ?ets_info(Tab, keypos);
        disc_copies ->
            ?ets_info(Tab, keypos);
        disc_only_copies ->
            dets:info(Tab, keypos);
        {ext, Mod, Alias} ->
            Mod:info(Alias, keypos)
    end.

%% @returns true if second element obsoletes the first one
-spec obsolete(mnesia:table(), {ts(), {op(), val()}}, {ts(), {op(), val()}}) -> boolean().
obsolete(Tab, {Ts1, {write, V1}}, {Ts2, {write, V2}}) ->
    equals(Tab, V1, V2) andalso mnesia_causal:compare_vclock(Ts1, Ts2) =:= lt;
obsolete(Tab, {Ts1, {write, V1}}, {Ts2, {delete, V2}}) ->
    equals(Tab, V1, V2) andalso mnesia_causal:compare_vclock(Ts1, Ts2) =:= lt;
obsolete(_Tab, {_Ts1, {delete, _V1}}, _X) ->
    true.

-spec equals(mnesia:table(), tuple(), tuple()) -> boolean().
equals(Tab, V1, V2) ->
    case val({Tab, setorbag}) of
        pawset ->
            element(key_pos(Tab), V1) =:= element(key_pos(Tab), V2);
        pawbag ->
            V1 =:= V2;
        Other ->
            error({bad_val, Other})
    end.

%% Some helper functions to deal convert between different formats

-spec obj2ele(tuple()) -> element().
obj2ele(Obj) ->
    {get_ts(Obj), {get_op(Obj), get_val_tup(Obj)}}.

get_val_ele({_Ts, {_Op, V}}) ->
    V.

%% @equiv delete_meta/1
get_val_tup(Obj) ->
    delete_meta(Obj).

-spec get_val_key(mnesia:table(), tuple()) -> term().
get_val_key(Tab, V) ->
    element(key_pos(Tab), V).

add_op(Obj, Op) ->
    erlang:append_element(Obj, Op).

remove_op(Obj) ->
    erlang:delete_element(tuple_size(Obj), Obj).

get_op(Obj) ->
    element(tuple_size(Obj), Obj).

get_ts(Obj) ->
    get_ts(tuple_size(Obj) - 1, Obj).

get_ts(Idx, Obj) ->
    element(Idx, Obj).

remove_ts(Obj) ->
    erlang:delete_element(tuple_size(Obj), Obj).

-spec delete_meta(tuple()) -> tuple().
delete_meta(Obj) ->
    Last = tuple_size(Obj),
    erlang:delete_element(Last - 1, erlang:delete_element(Last, Obj)).

wild_ts(Pat) ->
    erlang:append_element(Pat, '_').
