%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
-module(snmp_shadow_table).

-export([table_func/2, table_func/4]).

-include("snmpa_internal.hrl").

-record(time_stamp, {key, data}).

-define(verify(Expr, Error), verify(catch Expr, Error, ?FILE, ?LINE)).

verify(Res, Error, File, Line) ->
    case Res of
	{atomic, _} ->
	    Res;
	ok ->
	    Res;
	_ ->
	    error_msg("~s(~w): crashed ~p -> ~p ~p~n",
		      [File, Line, Error, Res, process_info(self())]),
	    Res
    end.


%%%-----------------------------------------------------------------
%%% This module contains generic functions for implementing an SNMP
%%% table as a 'shadow'-table in Mnesia.  This means that for the
%%% SNMP table, there exists one Mnesia table with all information
%%% of the table.
%%% The Mnesia table is updated whenever an SNMP request is issued
%%% for the table and a specified amount of time has past since the
%%% last update.
%%% This is implemented as instrumentation functions to be used
%%% for the table.
%%%-----------------------------------------------------------------

create_time_stamp_table() ->
    Props = [{type, set},
	     {attributes, record_info(fields, time_stamp)}],
    create_table(time_stamp, Props, ram_copies, false),
    NRef = 
	case mnesia:dirty_read({time_stamp, ref_count}) of
	    [] -> 1;
	    [#time_stamp{data = Ref}] -> Ref + 1
	end,
    ok = mnesia:dirty_write(#time_stamp{key = ref_count, data = NRef}).

delete_time_stamp_table() ->
    Tab = time_stamp,
    case catch mnesia:dirty_read({Tab, ref_count}) of
	{'EXIT', _Reason} ->
	    delete_table(Tab);
	[] ->
	    delete_table(Tab);
	[#time_stamp{data = 1}] ->
	    delete_table(Tab);
	[#time_stamp{data = Ref}] ->
	    ok = mnesia:dirty_write(#time_stamp{key = ref_count, data = Ref - 1})
    end.

update(Name, UpdateFunc, Interval) ->
    CurrentTime = snmp_misc:now(ms),
    case mnesia:dirty_read({time_stamp, Name}) of
	[#time_stamp{data = Expire}] when CurrentTime =< Expire -> ok;
	_ ->
	    UpdateFunc(),
	    ok = mnesia:dirty_write(#time_stamp{key = Name,
						data = CurrentTime + Interval})
    end.
	  

%%-----------------------------------------------------------------
%% Func: table_func(Op, Extra)
%%       table_func(Op, RowIndex, Cols, Extra)
%% Args: Extra = {Name, SnmpKey, Attributes, Interval, UpdateFunc}
%% Purpose: Instrumentation function for the table.
%%          Name is the name of the table
%%          SnmpKey is the snmpkey as it should be specifed in order
%%            to create the Mnesia table as an SNMP table
%%          Attributes is the attributes as it should be specifed in order
%%            to create the Mnesia table as an SNMP table
%%          Interval is the minimum time in milliseconds between two
%%            updates of the table
%%          UpdateFunc is a function with no arguments that is called
%%            whenever the table must be updated
%% Returns: As specified for an SNMP table instrumentation function.
%%-----------------------------------------------------------------
table_func(new, {Name, SnmpKey, Attribs, _Interval, _UpdateFunc}) ->
    create_time_stamp_table(),
    Props = [{type, set},
	     {snmp, [{key, SnmpKey}]},
	     {attributes, Attribs}],
    create_table(Name, Props, ram_copies, true);
table_func(delete, {Name, _SnmpKey, _Attribs, _Interval, _UpdateFunc}) ->
    delete_time_stamp_table(),
    delete_table(Name).

table_func(Op, RowIndex, Cols, 
	   {Name, _SnmpKey, _Attribs, Interval, UpdateFunc}) ->
    update(Name, UpdateFunc, Interval),
    snmp_generic:table_func(Op, RowIndex, Cols, {Name, mnesia}).


%%-----------------------------------------------------------------
%% Urrk.
%% We want named tables, without schema info; the tables should
%% be locally named, but if the node crashes, info about the
%% table shouldn't be kept.  We could use ets tables for this.
%% BUT, we also want the snmp functionality, therefore we must
%% use mnesia.
%% The problem arises when the node that implements these tables
%% crashes, and another node takes over the MIB-implementations.
%% That node cannot create the shadow tables again, because they
%% already exist (according to mnesia...).  Therefore, we must
%% check if we maybe must delete the table first, and then create
%% it again.
%%-----------------------------------------------------------------
create_table(Tab, Props, Storage, DeleteAll) ->
    case lists:member(Tab, mnesia:system_info(tables)) of
	true ->
	    case mnesia:table_info(Tab, storage_type) of
		unknown ->
		    ?verify(mnesia:add_table_copy(Tab, node(), Storage),
			    [add_table_copy, Tab, node(), Storage]);
		Storage when DeleteAll == true ->
		    delete_all(Tab);
		_ ->
		    ignore
	    end;
	false ->
	    Nodes = [node()],
	    Props2 = [{local_content, true}, {Storage, Nodes}] ++ Props,
	    ?verify(mnesia:create_table(Tab, Props2),
		    [create_table, Tab, Props2])
    end.

delete_all(Tab) ->
    delete_all(mnesia:dirty_first(Tab), Tab).

delete_all('$end_of_table', _Tab) ->
    ok;
delete_all(Key, Tab) ->
    ok = mnesia:dirty_delete({Tab, Key}),
    delete_all(mnesia:dirty_next(Tab, Key), Tab).

delete_table(Tab) ->
    case lists:member(Tab, mnesia:system_info(tables)) of
	true ->
	    case ?verify(mnesia:del_table_copy(Tab, node()),
			 [del_table_copy, Tab, node()]) of
		{atomic, ok} ->
		    ok;
		{aborted, _Reason} ->
		    catch delete_all(Tab),
		    ok
	    end;
	false ->
	    ok
    end.


%%-----------------------------------------------------------------

error_msg(F, A) ->
    ?snmpa_error(F, A).
