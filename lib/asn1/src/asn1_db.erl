%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
%%
-module(asn1_db).

-export([dbstart/1,dbnew/1,dbsave/2,dbload/1,dbput/3,dbget/2,dbget_all/1]).
-export([dbget_all_mod/1,dbclear/0,dberase_module/1,dbstop/0]).

-record(state, {parent, monitor, includes, table}).

%% Interface
dbstart(Includes) ->
    Parent = self(),
    case get(?MODULE) of
        undefined ->
            put(?MODULE, spawn_link(fun() -> init(Parent, Includes) end)),
            true;
        _Pid ->
            req({new_includes, Includes})
    end.

dbnew(Module)              -> req({new, Module}).
dbsave(OutFile, Module)    -> req({save, OutFile, Module}).
dbload(Module)             -> req({load, Module}).
dbput(Module, K, V)        -> req({set, Module, K, V}).
dbget(Module, K)           -> req({get, Module, K}).
dbget_all(K)               -> req({get_all, K}).
dbget_all_mod(Mod)         -> req({all_mod, Mod}).
dbclear()                  -> req(clear).
dberase_module({module,M}) -> req({delete_mod, M}).
dbstop()                   -> Resp = req(stop), erase(?MODULE), Resp.

%% Internal functions
req(Request) ->
    DbPid = get(?MODULE),
    Ref = erlang:monitor(process,DbPid),
    get(?MODULE) ! {{Ref, self()}, Request},
    receive 
	{{Ref,?MODULE}, Reply} ->
	    erlang:demonitor(Ref,[flush]),
	    Reply; 
	{'DOWN',Ref,_,_,Info} -> 
	    exit({db_error,Info}) 
    end.

reply({Ref,From}, Response) ->
    From ! {{Ref,?MODULE}, Response}.

init(Parent, Includes) ->
    MRef = erlang:monitor(process, Parent),
    loop(#state{parent = Parent, monitor = MRef, includes = Includes,
                table = ets:new(?MODULE, [])}).

loop(#state{parent = Parent, monitor = MRef, table = Table,
            includes = Includes} = State) ->
    receive
        {From, {set, Mod, K2, V}} ->
            [{_, Modtab}] = ets:lookup(Table, Mod),
            ets:insert(Modtab, {K2, V}),
            reply(From, ok),
            loop(State);
        {From, {get, Mod, K2}} ->
            Result = case ets:lookup(Table, Mod) of
                         []            -> opentab(Table, Mod, Includes);
                         [{_, Modtab}] -> {ok, Modtab}
                     end,
            case Result of
                {ok, Newtab} -> reply(From, lookup(Newtab, K2));
                _Error       -> reply(From, undefined)
            end,
            loop(State);
        {From, {all_mod, Mod}} ->
            [{_, Modtab}] = ets:lookup(Table, Mod),
            reply(From, ets:tab2list(Modtab)),
            loop(State);
        {From, {delete_mod, Mod}} ->
            [{_, Modtab}] = ets:lookup(Table, Mod),
            ets:delete(Modtab),
            ets:delete(Table, Mod),
            reply(From, ok),
            loop(State);
        {From, {save, OutFile, Mod}} ->
            [{_,Mtab}] = ets:lookup(Table, Mod),
            reply(From, ets:tab2file(Mtab, OutFile)),
            loop(State);
        {From, {load, Mod}} ->
            Result = case ets:lookup(Table, Mod) of
                         []            -> opentab(Table, Mod, Includes);
                         [{_, Modtab}] -> {ok, Modtab}
                     end,
            reply(From, Result),
            loop(State);
        {From, {new, Mod}} ->
            case ets:lookup(Table, Mod) of
                [{_, Modtab}] -> ets:delete(Modtab);
                _             -> true
            end,
            ModTableId = ets:new(list_to_atom(lists:concat(["asn1_",Mod])), []),
            ets:insert(Table, {Mod, ModTableId}),
            reply(From, ok),
            loop(State);
        {From, clear} ->
            [ets:delete(Mt) || {_, Mt} <- ets:tab2list(Table)],
            ets:delete(Table),
            reply(From, cleared),
            loop(State#state{table = ets:new(asn1, [set])});
        {From, {new_includes, NewIncludes}} ->
            reply(From, true),
            loop(State#state{includes = NewIncludes});
        {From, stop} ->
            reply(From, stopped); %% Nothing to store
        {'DOWN', MRef, process, Parent, Reason} ->
            exit(Reason)
    end.

opentab(Tab, Mod, []) ->
    opentab(Tab, Mod, ["."]);
opentab(Tab, Mod, Includes) ->
    Base = lists:concat([Mod, ".asn1db"]),
    opentab2(Tab, Base, Mod, Includes, ok).

opentab2(_Tab, _Base, _Mod, [], Error) ->
    Error;
opentab2(Tab, Base, Mod, [Ih|It], _Error) ->
    File = filename:join(Ih, Base),
    case ets:file2tab(File) of
        {ok, Modtab} ->
            ets:insert(Tab, {Mod, Modtab}),
            {ok, Modtab};
        NewErr ->
            opentab2(Tab, Base, Mod, It, NewErr)
    end.

lookup(Tab, K) ->
    case ets:lookup(Tab, K) of
        []      -> undefined;
        [{K,V}] -> V
    end.
