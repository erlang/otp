%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(dbg_idb).

%% External exports
-export([insert/3, lookup/2, match_object/2]).

%%====================================================================
%% External exports
%%====================================================================

insert(DbRef, Key, Value) ->
    case DbRef of
	{Node, ModDb} ->
	    rpc:block_call(Node, ets, insert, [ModDb, {Key, Value}]);
	ModDb ->
	    ets:insert(ModDb, {Key, Value})
    end.

lookup(DbRef, Key) ->
    Res = case DbRef of
	      {Node, ModDb} ->
		  rpc:block_call(Node, ets, lookup, [ModDb, Key]);
	      ModDb ->
		  ets:lookup(ModDb, Key)
	  end,
    case Res of
	[{Key, Value}] -> {ok, Value};
	_ -> not_found
    end.

match_object(DbRef, Key) ->
    case DbRef of
	{Node, ModDb} ->
	    rpc:block_call(Node, ets, match_object, [ModDb, Key]);
	ModDb ->
	    ets:match_object(ModDb, Key)
    end.
