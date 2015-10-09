%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1_db.erl,v 1.1 2008/12/17 09:53:29 mikpe Exp $
%%
-module(asn1_db).
%-compile(export_all).
-export([dbnew/1,dbsave/2,dbload/1,dbput/3,dbget/2,dbget_all/1]).
-export([dbget_all_mod/1,dbstop/0,dbclear/0,dberase_module/1,dbstart/1,stop_server/1]).
%% internal exports
-export([dbloop0/1,dbloop/2]).

%% Db stuff
dbstart(Includes) ->
    start_server(asn1db, asn1_db, dbloop0, [Includes]).

dbloop0(Includes) ->
    dbloop(Includes, ets:new(asn1, [set,named_table])).

opentab(Tab,Mod,[]) ->
    opentab(Tab,Mod,["."]);
opentab(Tab,Mod,Includes) ->
    Base = lists:concat([Mod,".asn1db"]),
    opentab2(Tab,Base,Mod,Includes,ok).

opentab2(_Tab,_Base,_Mod,[],Error) ->
    Error;
opentab2(Tab,Base,Mod,[Ih|It],_Error) ->
    File = filename:join(Ih,Base),
    case ets:file2tab(File) of
	{ok,Modtab} ->
	    ets:insert(Tab,{Mod, Modtab}),
	    {ok,Modtab};
	NewErr ->
	    opentab2(Tab,Base,Mod,It,NewErr)
    end.


dbloop(Includes, Tab) ->
    receive
	{From,{set, Mod, K2, V}} ->
	    [{_,Modtab}] = ets:lookup(Tab,Mod),
	    ets:insert(Modtab,{K2, V}),
	    From ! {asn1db, ok},
	    dbloop(Includes, Tab);
	{From, {get, Mod, K2}} ->
	    Result = case ets:lookup(Tab,Mod) of
			 [] ->
			     opentab(Tab,Mod,Includes);
			 [{_,Modtab}] -> {ok,Modtab}
		     end,
	    case Result of
		{ok,Newtab} ->
		    From ! {asn1db, lookup(Newtab, K2)};
		_Error ->
		    From ! {asn1db, undefined}
	    end,
	    dbloop(Includes, Tab);
	{From, {all_mod, Mod}} ->
	    [{_,Modtab}] = ets:lookup(Tab,Mod),
	    From ! {asn1db, ets:tab2list(Modtab)},
	    dbloop(Includes, Tab);
	{From, {delete_mod, Mod}} ->
	    [{_,Modtab}] = ets:lookup(Tab,Mod),
	    ets:delete(Modtab),
	    ets:delete(Tab,Mod),
	    From ! {asn1db, ok},
	    dbloop(Includes, Tab);
	{From, {save, OutFile,Mod}} ->
	    [{_,Mtab}] = ets:lookup(Tab,Mod),
	    {From ! {asn1db, ets:tab2file(Mtab,OutFile)}},
	    dbloop(Includes,Tab);
	{From, {load, Mod}} ->
	    Result = case ets:lookup(Tab,Mod) of
			 [] ->
			     opentab(Tab,Mod,Includes);
			 [{_,Modtab}] -> {ok,Modtab}
		     end,
	    {From, {asn1db,Result}},
	    dbloop(Includes,Tab);
	{From, {new, Mod}} ->
	    case ets:lookup(Tab,Mod) of
		[{_,Modtab}] ->
		    ets:delete(Modtab);
		_  ->
		    true
	    end,
	    Tabname = list_to_atom(lists:concat(["asn1_",Mod])),
	    ets:new(Tabname, [set,named_table]),
	    ets:insert(Tab,{Mod,Tabname}),
	    From ! {asn1db, ok},
	    dbloop(Includes,Tab);
	{From, stop} ->
		    From ! {asn1db, ok};  %% nothing to store
	{From, clear} ->
	    ModTabList = [Mt||{_,Mt} <- ets:tab2list(Tab)],
	    lists:foreach(fun(T) -> ets:delete(T) end,ModTabList),
	    ets:delete(Tab),
	    From ! {asn1db, cleared},
	    dbloop(Includes, ets:new(asn1, [set]))
    end.


%%all(Tab, K) ->
%%    pickup(K, ets:match(Tab, {{K, '$1'}, '$2'})).
%%pickup(K, []) -> [];
%%pickup(K, [[V1,V2] |T]) ->
%%    [{{K,V1},V2} | pickup(K, T)].

lookup(Tab, K) ->
    case ets:lookup(Tab, K) of
	[] -> undefined;
	[{K,V}] -> V
    end.


dbnew(Module) -> req({new,Module}).
dbsave(OutFile,Module) -> req({save,OutFile,Module}).
dbload(Module) -> req({load,Module}).

dbput(Module,K,V) -> req({set, Module, K, V}).
dbget(Module,K) ->   req({get, Module, K}).
dbget_all(K) ->   req({get_all, K}).
dbget_all_mod(Mod) -> req({all_mod,Mod}).
dbstop() ->       stop_server(asn1db).
dbclear() ->      req(clear).
dberase_module({module,M})->
    req({delete_mod, M}).

req(R) ->
    asn1db ! {self(), R},
    receive {asn1db, Reply} -> Reply end.

stop_server(Name) ->
    stop_server(Name, whereis(Name)).
stop_server(_, undefined) -> stopped;
stop_server(Name, _Pid) ->
    Name  ! {self(), stop},
    receive {Name, _} -> stopped end.


start_server(Name,Mod,Fun,Args) ->
    case whereis(Name) of
	undefined ->
	    register(Name, spawn(Mod,Fun, Args));
	_Pid ->
	    already_started
    end.
