%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
