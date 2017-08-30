%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(asn1_db).

-export([dbstart/1,dbnew/3,dbload/1,dbload/4,dbsave/2,dbput/2,
	 dbput/3,dbget/2]).
-export([dbstop/0]).

-record(state, {parent, monitor, includes, table}).

%% Interface
dbstart(Includes0) ->
    Includes = case Includes0 of
		   [] -> ["."];
		   [_|_] -> Includes0
	       end,
    Parent = self(),
    undefined = get(?MODULE),			%Assertion.
    put(?MODULE, spawn_link(fun() -> init(Parent, Includes) end)),
    ok.

dbload(Module, Erule, Maps, Mtime) ->
    req({load, Module, {Erule,Maps}, Mtime}).

dbload(Module) ->
    req({load, Module, any, {{0,0,0},{0,0,0}}}).

dbnew(Module, Erule, Maps) -> req({new, Module, {Erule,Maps}}).
dbsave(OutFile, Module)    -> cast({save, OutFile, Module}).
dbput(Module, K, V)        -> cast({set, Module, K, V}).
dbput(Module, Kvs)         -> cast({set, Module, Kvs}).
dbget(Module, K)           -> req({get, Module, K}).
dbstop()                   -> Resp = req(stop), erase(?MODULE), Resp.

%% Internal functions
-define(MAGIC_KEY, '__version_and_erule__').

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

cast(Request) ->
    get(?MODULE) ! Request,
    ok.

reply({Ref,From}, Response) ->
    From ! {{Ref,?MODULE}, Response},
    ok.

init(Parent, Includes) ->
    MRef = erlang:monitor(process, Parent),
    loop(#state{parent = Parent, monitor = MRef, includes = Includes,
                table = ets:new(?MODULE, [])}).

loop(#state{parent = Parent, monitor = MRef, table = Table,
            includes = Includes} = State) ->
    receive
        {set, Mod, K2, V} ->
            [{_, Modtab}] = ets:lookup(Table, Mod),
            ets:insert(Modtab, {K2, V}),
            loop(State);
        {set, Mod, Kvs} ->
            [{_, Modtab}] = ets:lookup(Table, Mod),
            ets:insert(Modtab, Kvs),
            loop(State);
        {From, {get, Mod, K2}} ->
	    %% XXX If there is no information for Mod, get_table/3
	    %% will attempt to load information from an .asn1db
	    %% file, without comparing its timestamp against the
	    %% source file. This is known to happen when check_*
	    %% functions for DER are generated, but it could possibly
	    %% happen in other circumstances. Ideally, this issue should
	    %% be rectified in some way, perhaps by ensuring that
	    %% the module has been loaded (using dbload/4) prior
	    %% to calling dbget/2.
            case get_table(Table, Mod, Includes) of
                {ok,Tab} -> reply(From, lookup(Tab, K2));
                error -> reply(From, undefined)
            end,
            loop(State);
        {save, OutFile, Mod} ->
            [{_,Mtab}] = ets:lookup(Table, Mod),
	    TempFile = OutFile ++ ".#temp",
            ok = ets:tab2file(Mtab, TempFile),
	    ok = file:rename(TempFile, OutFile),
            loop(State);
        {From, {new, Mod, EruleMaps}} ->
            [] = ets:lookup(Table, Mod),	%Assertion.
            ModTableId = ets:new(list_to_atom(lists:concat(["asn1_",Mod])), []),
            ets:insert(Table, {Mod, ModTableId}),
	    ets:insert(ModTableId, {?MAGIC_KEY, info(EruleMaps)}),
            reply(From, ok),
            loop(State);
	{From, {load, Mod, EruleMaps, Mtime}} ->
	    case ets:member(Table, Mod) of
		true ->
		    reply(From, ok);
		false ->
		    case load_table(Mod, EruleMaps, Mtime, Includes) of
			{ok, ModTableId} ->
			    ets:insert(Table, {Mod, ModTableId}),
			    reply(From, ok);
			error ->
			    reply(From, error)
		    end
	    end,
	    loop(State);
        {From, stop} ->
            reply(From, stopped); %% Nothing to store
        {'DOWN', MRef, process, Parent, Reason} ->
            exit(Reason)
    end.

get_table(Table, Mod, Includes) ->
    case ets:lookup(Table, Mod) of
	[{Mod,Tab}] ->
	    {ok,Tab};
	[] ->
	    load_table(Mod, any, {{0,0,0},{0,0,0}}, Includes)
    end.

lookup(Tab, K) ->
    case ets:lookup(Tab, K) of
        []      -> undefined;
        [{K,V}] -> V
    end.

info(EruleMaps) ->
    {asn1ct:vsn(),EruleMaps}.

load_table(Mod, EruleMaps, Mtime, Includes) ->
    Base = lists:concat([Mod, ".asn1db"]),
    case path_find(Includes, Mtime, Base) of
	error ->
	    error;
	{ok,ModTab} when EruleMaps =:= any ->
	    {ok,ModTab};
	{ok,ModTab} ->
	    Vsn = asn1ct:vsn(),
	    case ets:lookup(ModTab, ?MAGIC_KEY) of
		[{_,{Vsn,EruleMaps}}] ->
		    %% Correct version and encoding rule.
		    {ok,ModTab};
		_ ->
		    %% Missing key or wrong version/encoding rule.
		    ets:delete(ModTab),
		    error
	    end
    end.

path_find([H|T], Mtime, Base) ->
    File = filename:join(H, Base),
    case filelib:last_modified(File) of
	0 ->
	    path_find(T, Mtime, Base);
	DbMtime when DbMtime >= Mtime ->
	    case ets:file2tab(File) of
		{ok,_}=Ret ->
		    Ret;
		_ ->
		    path_find(T, Mtime, Base)
	    end;
	_ ->
	    path_find(T, Mtime, Base)
    end;
path_find([], _, _) -> error.
