%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% Getting started:
%%
%% 1 Start one or more distributed Erlang nodes
%% 2a Connect the nodes, e.g. with net_adm:ping/1
%% 3a Run mnesia_meter:go()
%% 3b Run mnesia_meter:go(ReplicaType)
%% 3c Run mnesia_meter:go(ReplicaType, Nodes)

-module(mnesia_meter).
-author('hakan@erix.ericsson.se').
-export([
         go/0, 
         go/1, 
         go/2,
         repeat_meter/2
        ]).

-record(person, {name,        %% atomic, unique key
                 data,        %% compound structure
                 married_to,  %% name of partner or undefined
                 children}).  %% list of children

-record(meter, {desc, init, meter, micros}).

-record(result, {desc, list}).

-define(TIMES, 1000).

go() ->
    go(ram_copies).

go(ReplicaType) ->
    go(ReplicaType, [node() | nodes()]).

go(ReplicaType, Nodes) ->
    {ok, FunOverhead} = tc(fun(_) -> {atomic, ok} end, ?TIMES),
    Size = size(term_to_binary(#person{})),
    io:format("A fun apply costs ~p micro seconds. Record size is ~p bytes.~n",
              [FunOverhead, Size]),
    Res = go(ReplicaType, Nodes, [], FunOverhead, []),
    NewRes = rearrange(Res, []),
    DescHeader = lists:flatten(io_lib:format("~w on ~w", [ReplicaType, Nodes])),
    ItemHeader = lists:seq(1, length(Nodes)),
    Header = #result{desc = DescHeader, list = ItemHeader},
    SepList = ['--------' || _ <- Nodes],
    Separator = #result{desc = "", list = SepList},
    display([Separator, Header, Separator | NewRes] ++ [Separator]).

go(_ReplicaType, [], _Config, _FunOverhead, Acc) ->
    Acc;
go(ReplicaType, [H | T], OldNodes, FunOverhead, Acc) ->
    Nodes = [H | OldNodes],
    Config = [{ReplicaType, Nodes}],
    Res = run(Nodes, Config, FunOverhead),
    go(ReplicaType, T, Nodes, FunOverhead, [{ReplicaType, Nodes, Res} | Acc]).

rearrange([{_ReplicaType, _Nodes, Meters} | Tail], Acc) ->
    Acc2 = [add_meter(M, Acc) || M <- Meters],
    rearrange(Tail, Acc2);
rearrange([], Acc) ->
    Acc.

add_meter(M, Acc) ->
    case lists:keysearch(M#meter.desc, #result.desc, Acc) of
        {value, R} ->
            R#result{list = [M#meter.micros | R#result.list]};
        false ->
            #result{desc = M#meter.desc, list = [M#meter.micros]}
    end.

display(Res) ->
    MaxDesc = lists:max([length(R#result.desc) || R <- Res]),
    Format = lists:concat(["! ~-", MaxDesc, "s"]),
    display(Res, Format, MaxDesc).

display([R | Res], Format, MaxDesc) ->
    case R#result.desc of
	"" ->
	    io:format(Format, [lists:duplicate(MaxDesc, "-")]);
	Desc ->
	    io:format(Format, [Desc])
    end,
    display_items(R#result.list, R#result.desc),
    io:format(" !~n", []),
    display(Res, Format, MaxDesc);
display([], _Format, _MaxDesc) ->
    ok.
    
display_items([_Item | Items], "") ->
    io:format(" ! ~s", [lists:duplicate(10, $-)]),
    display_items(Items, "");
display_items([Micros | Items], Desc) ->
    io:format(" ! ~10w", [Micros]),
    display_items(Items, Desc);
display_items([], _Desc) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meters() ->
    [#meter{desc = "transaction update two records with read and write", 
            init = fun write_records/2,
            meter = fun update_records/1}, 
     #meter{desc = "transaction update two records with wread and write", 
            init = fun write_records/2,
            meter = fun w_update_records/1}, 
     #meter{desc = "transaction update two records with read and s_write", 
            init = fun s_write_records/2,
            meter = fun s_update_records/1}, 
     #meter{desc = "sync_dirty  update two records with read and write", 
            init = fun sync_dirty_write_records/2,
            meter = fun sync_dirty_update_records/1}, 
     #meter{desc = "async_dirty update two records with read and write", 
            init = fun async_dirty_write_records/2,
            meter = fun async_dirty_update_records/1}, 
     #meter{desc = "plain fun   update two records with dirty_read and dirty_write", 
            init = fun dirty_write_records/2,
            meter = fun dirty_update_records/1}, 
     #meter{desc = "ets         update two records with read and write (local only)", 
            init = fun ets_opt_write_records/2,
            meter = fun ets_update_records/1}, 
     #meter{desc = "plain fun   update two records with ets:lookup and ets:insert (local only)", 
            init = fun bif_opt_write_records/2,
            meter = fun bif_update_records/1}, 
     #meter{desc = "plain fun   update two records with dets:lookup and dets:insert (local only)", 
            init = fun dets_opt_write_records/2,
            meter = fun dets_update_records/1}, 
     
     #meter{desc = "transaction write two records with write", 
            init = fun write_records/2,
            meter = fun(X) -> write_records(X, 0-X) end}, 
     #meter{desc = "transaction write two records with s_write", 
            init = fun s_write_records/2,
            meter = fun(X) -> s_write_records(X, 0-X) end}, 
     #meter{desc = "sync_dirty  write two records with write", 
            init = fun sync_dirty_write_records/2,
            meter = fun(X) -> sync_dirty_write_records(X, 0-X) end}, 
     #meter{desc = "async_dirty write two records with write", 
            init = fun async_dirty_write_records/2,
            meter = fun(X) -> async_dirty_write_records(X, 0-X) end}, 
     #meter{desc = "plain fun   write two records with dirty_write", 
            init = fun dirty_write_records/2,
            meter = fun(X) -> dirty_write_records(X, 0-X) end}, 
     #meter{desc = "ets         write two records with write (local only)", 
            init = fun ets_opt_write_records/2,
            meter = fun(X) -> ets_write_records(X, 0-X) end}, 
     #meter{desc = "plain fun   write two records with ets:insert (local only)", 
            init = fun bif_opt_write_records/2,
            meter = fun(X) -> bif_write_records(X, 0-X) end}, 
     #meter{desc = "plain fun   write two records with dets:insert (local only)", 
            init = fun dets_opt_write_records/2,
            meter = fun(X) -> dets_write_records(X, 0-X) end}, 
     
     #meter{desc = "transaction read two records with read", 
            init = fun write_records/2,
            meter = fun(X) -> read_records(X, 0-X) end}, 
     #meter{desc = "sync_dirty  read two records with read", 
            init = fun sync_dirty_write_records/2,
            meter = fun(X) -> sync_dirty_read_records(X, 0-X) end}, 
     #meter{desc = "async_dirty read two records with read", 
            init = fun async_dirty_write_records/2,
            meter = fun(X) -> async_dirty_read_records(X, 0-X) end}, 
     #meter{desc = "plain fun   read two records with dirty_read", 
            init = fun dirty_write_records/2,
            meter = fun(X) -> dirty_read_records(X, 0-X) end}, 
     #meter{desc = "ets         read two records with read", 
            init = fun ets_opt_write_records/2,
            meter = fun(X) -> ets_read_records(X, 0-X) end},
     #meter{desc = "plain fun   read two records with ets:lookup", 
            init = fun bif_opt_write_records/2,
            meter = fun(X) -> bif_read_records(X, 0-X) end},
     #meter{desc = "plain fun   read two records with dets:lookup", 
            init = fun dets_opt_write_records/2,
            meter = fun(X) -> dets_read_records(X, 0-X) end}
    ].

update_fun(Name) ->
    fun() ->
            case mnesia:read({person, Name}) of
                [] -> 
                    mnesia:abort(no_such_person);
                [Pers] ->
                    [Partner] = mnesia:read({person, Pers#person.married_to}), 
                    mnesia:write(Pers#person{married_to = undefined}), 
                    mnesia:write(Partner#person{married_to = undefined})
            end
    end.
    
update_records(Name) ->
  mnesia:transaction(update_fun(Name)).

sync_dirty_update_records(Name) ->
  {atomic, mnesia:sync_dirty(update_fun(Name))}.

async_dirty_update_records(Name) ->
  {atomic, mnesia:async_dirty(update_fun(Name))}.

ets_update_records(Name) ->
  {atomic, mnesia:ets(update_fun(Name))}.

w_update_records(Name) ->
    F = fun() ->
                case mnesia:wread({person, Name}) of
                    [] -> 
                        mnesia:abort(no_such_person);
                    [Pers] ->
                        [Partner] = mnesia:wread({person, Pers#person.married_to}), 
                        mnesia:write(Pers#person{married_to = undefined}), 
                        mnesia:write(Partner#person{married_to = undefined})
                end
        end, 
    mnesia:transaction(F).

s_update_records(Name) ->
    F = fun() ->
                case mnesia:read({person, Name}) of
                    [] -> 
                        mnesia:abort(no_such_person);
                    [Pers] ->
                        [Partner] = mnesia:read({person, Pers#person.married_to}), 
                        mnesia:s_write(Pers#person{married_to = undefined}), 
                        mnesia:s_write(Partner#person{married_to = undefined})
                end
        end, 
    mnesia:transaction(F).

dirty_update_records(Name) ->
    case mnesia:dirty_read({person, Name}) of
        [] -> 
            mnesia:abort(no_such_person);
        [Pers] ->
            [Partner] = mnesia:dirty_read({person, Pers#person.married_to}), 
            mnesia:dirty_write(Pers#person{married_to = undefined}), 
            mnesia:dirty_write(Partner#person{married_to = undefined})
    end,
    {atomic, ok}.

bif_update_records(Name) ->
    case ets:lookup(person, Name) of
        [] -> 
            mnesia:abort(no_such_person);
        [Pers] ->
            [Partner] = ets:lookup(person, Pers#person.married_to), 
            ets:insert(person, Pers#person{married_to = undefined}), 
            ets:insert(person, Partner#person{married_to = undefined})
    end,
    {atomic, ok}.

dets_update_records(Name) ->
    case dets:lookup(person, Name) of
        [] -> 
            mnesia:abort(no_such_person);
        [Pers] ->
            [Partner] = dets:lookup(person, Pers#person.married_to), 
            dets:insert(person, Pers#person{married_to = undefined}), 
            dets:insert(person, Partner#person{married_to = undefined})
    end,
    {atomic, ok}.

write_records_fun(Pers, Partner) ->
    fun() ->
            P = #person{children = [ulla, bella]}, 
            mnesia:write(P#person{name = Pers, married_to = Partner}), 
            mnesia:write(P#person{name = Partner, married_to = Pers})
    end.

write_records(Pers, Partner) ->
    mnesia:transaction(write_records_fun(Pers, Partner)).

sync_dirty_write_records(Pers, Partner) ->
    {atomic, mnesia:sync_dirty(write_records_fun(Pers, Partner))}.

async_dirty_write_records(Pers, Partner) ->
    {atomic, mnesia:async_dirty(write_records_fun(Pers, Partner))}.

ets_write_records(Pers, Partner) ->
    {atomic, mnesia:ets(write_records_fun(Pers, Partner))}.

s_write_records(Pers, Partner) ->
    F = fun() ->
                P = #person{children = [ulla, bella]}, 
                mnesia:s_write(P#person{name = Pers, married_to = Partner}), 
                mnesia:s_write(P#person{name = Partner, married_to = Pers})
        end,
    mnesia:transaction(F).

dirty_write_records(Pers, Partner) ->
    P = #person{children = [ulla, bella]}, 
    mnesia:dirty_write(P#person{name = Pers, married_to = Partner}), 
    mnesia:dirty_write(P#person{name = Partner, married_to = Pers}),
    {atomic, ok}.

ets_opt_write_records(Pers, Partner) ->
    case mnesia:table_info(person, where_to_commit) of
        [{N, ram_copies}] when N == node() ->
            ets_write_records(Pers, Partner);
        _ ->
            throw(skipped)
    end.

bif_opt_write_records(Pers, Partner) ->
    case mnesia:table_info(person, where_to_commit) of
        [{N, ram_copies}] when N == node() ->
            bif_write_records(Pers, Partner);
        _ ->
            throw(skipped)
    end.

bif_write_records(Pers, Partner) ->
    P = #person{children = [ulla, bella]}, 
    ets:insert(person, P#person{name = Pers, married_to = Partner}), 
    ets:insert(person, P#person{name = Partner, married_to = Pers}),
    {atomic, ok}.

dets_opt_write_records(Pers, Partner) ->
    case mnesia:table_info(person, where_to_commit) of
        [{N, disc_only_copies}] when N == node() ->
            dets_write_records(Pers, Partner);
        _ ->
            throw(skipped)
    end.

dets_write_records(Pers, Partner) ->
    P = #person{children = [ulla, bella]}, 
    dets:insert(person, P#person{name = Pers, married_to = Partner}), 
    dets:insert(person, P#person{name = Partner, married_to = Pers}),
    {atomic, ok}.

read_records_fun(Pers, Partner) ->
    fun() ->
            case {mnesia:read({person, Pers}), 
                  mnesia:read({person, Partner})} of
                {[_], [_]} ->
                    ok;
                _ ->
                    mnesia:abort(no_such_person)
            end
    end.

read_records(Pers, Partner) ->
    mnesia:transaction(read_records_fun(Pers, Partner)).

sync_dirty_read_records(Pers, Partner) ->
    {atomic, mnesia:sync_dirty(read_records_fun(Pers, Partner))}.

async_dirty_read_records(Pers, Partner) ->
    {atomic, mnesia:async_dirty(read_records_fun(Pers, Partner))}.

ets_read_records(Pers, Partner) ->
    {atomic, mnesia:ets(read_records_fun(Pers, Partner))}.

dirty_read_records(Pers, Partner) ->
    case {mnesia:dirty_read({person, Pers}), 
          mnesia:dirty_read({person, Partner})} of
        {[_], [_]} ->
            {atomic, ok};
        _ ->
            mnesia:abort(no_such_person)
    end.

bif_read_records(Pers, Partner) ->
    case {ets:lookup(person, Pers), 
          ets:lookup(person, Partner)} of
        {[_], [_]} ->
            {atomic, ok};
        _ ->
            mnesia:abort(no_such_person)
    end.

dets_read_records(Pers, Partner) ->
    case {dets:lookup(person, Pers), 
          dets:lookup(person, Partner)} of
        {[_], [_]} ->
            {atomic, ok};
        _ ->
            mnesia:abort(no_such_person)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Nodes, Config, FunOverhead) ->
    Meters = meters(),
    io:format("Run ~w meters with table config: ~w~n", [length(Meters), Config]),
    rpc:multicall(Nodes, mnesia, lkill, []),
    start(Nodes, Config),
    Res = [run_meter(Data, Nodes, FunOverhead) || Data <- Meters],
    stop(Nodes),
    Res.

run_meter(M, Nodes, FunOverhead) when is_record(M, meter) ->
    io:format(".", []),
    case catch init_records(M#meter.init, ?TIMES) of
        {atomic, ok} ->
            rpc:multicall(Nodes, mnesia, dump_log, []),
            case tc(M#meter.meter, ?TIMES) of
                {ok, Micros} ->
                    M#meter{micros = lists:max([0, Micros - FunOverhead])};
                {error, Reason} ->
                    M#meter{micros = Reason}
            end;
        Res ->
            M#meter{micros = Res}
    end.

start(Nodes, Config) ->
    mnesia:delete_schema(Nodes), 
    ok = mnesia:create_schema(Nodes),
    Args = [[{dump_log_write_threshold, ?TIMES div 2},
             {dump_log_time_threshold, timer:hours(10)}]],
    lists:foreach(fun(Node) -> rpc:call(Node, mnesia, start, Args) end, Nodes), 
    Attrs = record_info(fields, person), 
    TabDef = [{attributes, Attrs} | Config], 
    {atomic, _} = mnesia:create_table(person, TabDef).
    
stop(Nodes) ->
    rpc:multicall(Nodes, mnesia, stop, []).

%% Generate some dummy persons    
init_records(_Fun, 0) ->
    {atomic, ok};
init_records(Fun, Times) ->
    {atomic, ok} = Fun(Times, 0 - Times),
    init_records(Fun, Times - 1).

tc(Fun, Times) ->
    case catch timer:tc(?MODULE, repeat_meter, [Fun, Times]) of
        {Micros, ok} ->
            {ok, Micros div Times};
        {_Micros, {error, Reason}} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

%% The meter must return {atomic, ok}
repeat_meter(Meter, Times) ->
    repeat_meter(Meter, {atomic, ok}, Times).

repeat_meter(_, {atomic, ok}, 0) ->
    ok;
repeat_meter(Meter, {atomic, _Result}, Times) when Times > 0 ->
    repeat_meter(Meter, Meter(Times), Times - 1);
repeat_meter(_Meter, Reason, _Times) ->
    {error, Reason}.

