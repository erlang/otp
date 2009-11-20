%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

-module(dict_test_lib, [Mod,Equal]).

-export([module/0,equal/2,empty/0,size/1,to_list/1,from_list/1,
	 enter/3,delete/2,lookup/2]).

module() ->
    Mod.

equal(X, Y) ->
    Equal(X, Y).

empty() ->
    case erlang:function_exported(Mod, new, 0) of
	false -> Mod:empty();
	true -> Mod:new()
    end.

size(S) ->
    Mod:size(S).

to_list(S) ->
    Mod:to_list(S).

from_list(S) ->
    case erlang:function_exported(Mod, from_orddict, 1) of
	false ->
	    Mod:from_list(S);
	true ->
	    %% The gb_trees module has no from_list/1 function.
	    %%
	    %% The keys in S are not unique. To make sure
	    %% that we pick the same key/value pairs as
	    %% dict/orddict, first convert the list to an orddict.
	    Orddict = orddict:from_list(S),
	    Mod:from_orddict(Orddict)
    end.

%% Store new value into dictionary or update previous value in dictionary.
enter(Key, Val, Dict) ->
    case erlang:function_exported(Mod, store, 3) of
	false ->
	    Mod:enter(Key, Val, Dict);
	true ->
	    Mod:store(Key, Val, Dict)
    end.

%% Delete an EXISTING key.
delete(Key, Dict) ->
    case erlang:function_exported(Mod, delete, 2) of
	true -> Mod:delete(Key, Dict);
	false -> Mod:erase(Key, Dict)
    end.

%% -> none | {value,Value}
lookup(Key, Dict) ->
    case erlang:function_exported(Mod, lookup, 2) of
	false ->
	    case Mod:find(Key, Dict) of
		error -> none;
		{ok,Value} -> {value,Value}
	    end;
	true ->
	    Mod:lookup(Key, Dict)
    end.
