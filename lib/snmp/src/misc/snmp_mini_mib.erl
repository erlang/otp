%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-module(snmp_mini_mib).

%% need definition of mib record
-include("snmp_types.hrl").

-export([
	 create/1, 
	 delete/1,
	 aliasname/2,	
	 oid/2, 
	 type/2
	]).


-record(mini_mib, {cache, db = []}).


%%%--------------------------------------------------
%%% The Mini MIB representation
%%%--------------------------------------------------

%% Returns a Mini MIB
create(Mibs) ->
    Loaded = lists:append([load_mib(Mib) || Mib <- Mibs]),
    Sorted = lists:keysort(1, Loaded),
    Db     = remove_dubbletts(Sorted), 
    Cache  = ets:new(snmp_mini_mib_cache, [set, {keypos, 1}]),
    #mini_mib{cache = Cache, 
	      db    = Db}.

delete(#mini_mib{cache = Cache}) ->
    ets:delete(Cache),
    ok.


%%----------------------------------------------------------------------
%% Returns: A list of {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
load_mib(MIB) ->
    F1 = snmp_misc:strip_extension_from_filename(MIB, ".bin"),
    ActualFileName = lists:append(F1, ".bin"),
    case snmp_misc:read_mib(ActualFileName) of
	{ok, #mib{mes = MEs, traps = Traps}} -> 
	    make_mini_mib_elem(MEs++Traps);
	{error, Reason} -> 
	    exit({error, {MIB, Reason}})
    end.


%%----------------------------------------------------------------------
%% Pre: List is sorted (dublettes are list neighbours)
%%----------------------------------------------------------------------
remove_dubbletts([])      -> [];
remove_dubbletts([X])     -> [X];
remove_dubbletts([X,X|T]) -> remove_dubbletts([X|T]);
remove_dubbletts([X|T])   -> [X|remove_dubbletts(T)].


%%----------------------------------------------------------------------
%% Args: A list if Mes
%% Returns: a list of {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
make_mini_mib_elem([]) -> [];
make_mini_mib_elem([#me{aliasname = N, 
			oid       = Oid, 
			entrytype = variable,
			asn1_type = #asn1_type{bertype = Type}} | T]) ->
    [{Oid, N, Type} | make_mini_mib_elem(T)];
make_mini_mib_elem([#me{aliasname = N, 
			oid       = Oid, 
			entrytype = table_column,
			asn1_type = ASN1}|T]) 
  when is_record(ASN1, asn1_type)->
    [{Oid, N, ASN1#asn1_type.bertype} | make_mini_mib_elem(T)];
make_mini_mib_elem([#me{aliasname = N, 
			oid       = Oid,
			asn1_type = undefined}|T]) ->
    [{Oid, N, undefined} | make_mini_mib_elem(T)];
make_mini_mib_elem([#notification{trapname = N, 
				  oid      = Oid}|T]) ->
    [{Oid, N, undefined} | make_mini_mib_elem(T)];
make_mini_mib_elem([_|T]) ->
    make_mini_mib_elem(T).


%%----------------------------------------------------------------------
%% Returns: false | {Oid, Aliasname, Type}
%%----------------------------------------------------------------------

aliasname(#mini_mib{cache = Cache, db = Db}, Oid) ->
    Key = {Oid, aliasname}, 
    case ets:lookup(Cache, Key) of
	[{_, Value}] ->
	    Value;
	_ ->
	    Value = aliasname(Db, Oid, false),
	    ets:insert(Cache, {Key, Value}),
	    Value
    end.

aliasname([], _Oid, Res) -> 
    Res;
aliasname([{Oid, _Aliasname, _Type} = OidData|T], OidX, Res) 
  when Oid =< OidX ->
    case lists:prefix(Oid, OidX) of
	true ->
	    aliasname(T, OidX, OidData);
	false ->
	    aliasname(T, OidX, Res)
    end;
aliasname([{_Oid, _Aliasname, _Type}|_T], _OidX, Res) ->
    Res.


oid(#mini_mib{db = Db}, AliasName) ->
    case lists:keysearch(AliasName, 2, Db) of
	{value, {Oid, _Aliasname, _Type}} -> 
	    Oid;
	false -> 
	    false
    end.

	    
type(MiniMIB, Oid) ->
    case aliasname(MiniMIB, Oid) of
	{_Oid, _AliasName, Type} ->
	    Type;
	Else ->
	    Else
    end.


