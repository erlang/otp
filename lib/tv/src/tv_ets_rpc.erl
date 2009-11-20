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
-module(tv_ets_rpc).



-export([all/2,
	 info/4,
	 new/4,
	 tab2list/3,
	 insert/4,
	 lookup/4,
	 delete/4
	]).




all(_Node, true) ->                  
    chk(catch ets:all());
all(Node, false) ->
    chk(catch rpc:block_call(Node, ets, all, [])).




info(_Node, true, TabId, What) ->
    chk(catch ets:info(TabId, What));
info(Node, false, TabId, What) ->
    chk(catch rpc:block_call(Node, ets, info, [TabId, What])).
    



new(_Node, true, TabName, Options) ->
    case catch ets:new(TabName, Options) of
	{TabName, Pid} when is_pid(Pid) ->
	    {TabName,Pid};
	{TabNo, Pid} when is_pid(Pid) ->
	    {TabNo,Pid};
	OtherResult ->
	    chk(OtherResult)
    end;
new(Node, false, TabName, Options) ->
    case catch rpc:block_call(Node, ets, new, [TabName, Options]) of
	{TabName, Pid} when is_pid(Pid) ->
	    {TabName,Pid};
	{TabNo, Pid} when is_pid(Pid) ->
	    {TabNo, Pid};
	OtherResult ->
	    chk(OtherResult)
    end.
    



tab2list(_Node, true, TabId) ->
    chk(catch ets:tab2list(TabId));
tab2list(Node, false, TabId) ->
    chk(catch rpc:call(Node, ets, tab2list, [TabId])).




insert(_Node, true, TabId, Object) ->
    chk(catch ets:insert(TabId, Object));
insert(Node, false, TabId, Object) ->
    chk(catch rpc:call(Node, ets, insert, [TabId, Object])).




lookup(_Node, true, TabId, Key) ->
    chk(catch ets:lookup(TabId, Key));
lookup(Node, false, TabId, Key) ->
    chk(catch rpc:call(Node, ets, lookup, [TabId, Key])).




delete(_Node, true, TabId, Key) ->
    chk(catch ets:delete(TabId, Key));
delete(Node, false, TabId, Key) ->
    chk(catch rpc:call(Node, ets, delete, [TabId, Key])).




chk(Result) ->
    case Result of
	undefined ->
	    throw(no_table);
	_Anything when is_list(Result) ->
	    Result;
	_Anything when is_atom(Result) ->
	    Result;
	_Anything when is_integer(Result) ->
	    Result;
	_Anything when is_pid(Result) ->
	    Result;

	%% Messages received when node is down.
	{badrpc, nodedown} ->
	    throw(nodedown);
	{'EXIT', nodedown} ->
	    throw(nodedown);
	{'EXIT', {{badarg, {gen, set_monitor_node, _Args}}, _Reason}} ->
	    throw(nodedown);

	%% Messages received when table doesn't exist.
	{'EXIT', {badarg, {ets,local_info,_Args}}} ->  
	       %% Due to inconsistencies in R2D and earlier versions:
	       %% ets:info/1 returned 'undefined' when table didn't
	       %% exist, while ets:info/2 returned the exit-signal 
	       %% above. This was corrected in R3A - now both functions 
	       %% return 'undefined'  :-)
	    throw(no_table);
	{badrpc, {'EXIT', {badarg,_Reason}}} ->
	    throw(no_table);
	{'EXIT', {badarg,_Reason}} ->
	    throw(no_table);
	Error when is_tuple(Error) ->   
	    throw({unexpected_error,Error})
    end.
	
