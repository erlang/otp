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
-module(tv_mnesia_rpc).



-export([system_info/3,
	 table_info/4,
	 transaction/3
	]).






system_info(_Node, true, Key) ->
    chk(catch mnesia:system_info(Key));
system_info(Node, false, Key) ->
    chk(catch rpc:block_call(Node, mnesia, system_info, [Key])).




table_info(_Node, true, Tab, Item) ->
    chk(catch mnesia:table_info(Tab, Item));
table_info(Node, false, Tab, Item) ->
    chk(catch rpc:block_call(Node, mnesia, table_info, [Tab, Item])).




transaction(_Node, true, Fun) ->
    chk(catch mnesia:transaction(Fun));
transaction(Node, false, Fun) ->
    chk(catch rpc:block_call(Node, mnesia, transaction, [Fun])).




chk(Result) ->
    case Result of
	_Anything when is_list(Result) ->
	    Result;
	_Anything when is_atom(Result) ->
	    Result;
	_Anything when is_integer(Result) ->
	    Result;
	_Anything when is_pid(Result) ->
	    Result;

	{aborted, {bad_type, _Rec}} ->
	    throw(bad_format);

	{badrpc,nodedown} ->
	    throw(nodedown);
	{'EXIT', nodedown} ->
	    throw(nodedown);

	{'EXIT', {aborted, {no_exists, _Table, _Arg}}} ->
	    throw(no_table);
	
	{'EXIT', {aborted, {node_not_running, _Node}}} ->
	    throw(mnesia_not_started);
	{'EXIT', {{badarg, {gen, set_monitor_mode, _Data}}, _Info}} ->
	    throw(mnesia_not_started);
	{'EXIT', {'EXIT', {aborted, {node_not_running,_Node}}}} ->
	    throw(mnesia_not_started);
	{badrpc, {'EXIT', {aborted, {node_not_running,_Node}}}} ->
	    throw(mnesia_not_started);
	{badrpc, {'EXIT', {aborted, {no_exists,_Table,_Args}}}} ->
	    throw(mnesia_not_started);
	{badrpc, _Reason} ->
	    throw(mnesia_not_started);
	{'EXIT', {undef, {mnesia,_Fcn,_Args}}} ->
	    throw(mnesia_not_started);
	
	{'EXIT', Reason} ->
	    throw({unexpected_error, Reason});
	
	Other when is_tuple(Other) ->
	       %% For example wild_pattern requests return a tuple!
	    Other;
	
	Other ->
	    io:format("Unexpected return value: ~p~n", [Other])
    end.
	

