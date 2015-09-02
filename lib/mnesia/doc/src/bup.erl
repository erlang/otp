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
%%     $Id$
%%
-module(bup).
-export([
         change_node_name/5,
         view/2,
         test/0,
         test/1
        ]).

-export([
	 count/1,
	 display/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Management of backups, a few demos

%0
change_node_name(Mod, From, To, Source, Target) ->
    Switch =
        fun(Node) when Node == From -> To;
           (Node) when Node == To -> throw({error, already_exists});
           (Node) -> Node
        end,
    Convert =
        fun({schema, db_nodes, Nodes}, Acc) ->
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
           ({schema, version, Version}, Acc) ->
                {[{schema, version, Version}], Acc};
           ({schema, cookie, Cookie}, Acc) ->
                {[{schema, cookie, Cookie}], Acc};
           ({schema, Tab, CreateList}, Acc) ->
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->
                            case lists:member(Key, Keys) of
                                true -> {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
           (Other, Acc) ->
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, Mod, Target, Mod, Convert, switched).

view(Source, Mod) ->
    View = fun(Item, Acc) ->
                   io:format("~p.~n",[Item]),
                   {[Item], Acc + 1}
           end,
    mnesia:traverse_backup(Source, Mod, dummy, read_only, View, 0).
%0

-record(bup_rec, {key, val}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test change of node name
%%
%% Assume db_nodes to be current node and on all other nodes but one
%% Create new schema,  start Mnesia on all db_nodes
%% Create a table of disc_copies type which is replicated to all db_nodes
%% Perform a backup and change current node to unused node in backup
%% Start Mnesia on all nodes according to the new set of db_nodes
test() ->
    test(nodes()).

test(Nodes)->
    AllNodes = (Nodes -- [node()]) ++ [node()],
    case length(AllNodes)  of
        Length when Length > 1 ->
	    OldBup = "old.BUP",
	    NewBup = "new.BUP",
            Res = (catch test2(AllNodes, OldBup, NewBup)),
	    case Res of
                {'EXIT', Reason} ->
		    file:delete(OldBup),
		    file:delete(NewBup),
		    {error, Reason};
                ok ->
		    ok = count(NewBup),
		    file:delete(OldBup),
		    file:delete(NewBup),
		    ok
            end;
        _ ->
            {error,{"Must run on at least one other node",AllNodes}}
    end.
            
test2(AllNodes, OldBup, NewBup) ->
    ThisNode = node(),
    OtherNode = hd(AllNodes -- [ThisNode]),
    OldNodes = AllNodes -- [OtherNode],
    NewNodes = AllNodes -- [ThisNode],
    Mod = mnesia_backup, % Assume local files
    file:delete(OldBup),
    file:delete(NewBup),

    %% Create old backup
    rpc:multicall(AllNodes, mnesia, lkill, []),
    ok = mnesia:delete_schema(AllNodes),
    ok = mnesia:create_schema(OldNodes),
    rpc:multicall(OldNodes, mnesia, start, []),
    rpc:multicall(OldNodes, mnesia, wait_for_tables, [[schema], infinity]),
    
    CreateList = [{disc_copies, OldNodes},
                  {attributes, record_info(fields, bup_rec)}],
    {atomic, ok} = mnesia:create_table(bup_rec, CreateList),
    rpc:multicall(OldNodes, mnesia, wait_for_tables, [[bup_rec], infinity]),
    OldRecs = [#bup_rec{key = I, val = I * I} || I <- lists:seq(1, 10)],
    lists:foreach(fun(R) -> ok = mnesia:dirty_write(R) end,OldRecs),
    ok = mnesia:backup(OldBup, Mod),
    ok = mnesia:dirty_write(#bup_rec{key = 4711, val = 4711}),
    rpc:multicall(OldNodes, mnesia, stop, []),
    {ok,_} = view(OldBup, Mod),

    %% Change node name
    {ok,_} = change_node_name(Mod, ThisNode, OtherNode, OldBup, NewBup),
    ok = rpc:call(OtherNode, mnesia, install_fallback, [NewBup, Mod]),
    {_NewStartRes,[]} = rpc:multicall(NewNodes, mnesia, start, []),
    rpc:call(OtherNode, mnesia, wait_for_tables, [[bup_rec], infinity]),
    Wild = rpc:call(OtherNode, mnesia, table_info, [bup_rec, wild_pattern]),
    NewRecs = rpc:call(OtherNode, mnesia, dirty_match_object, [Wild]),
    rpc:multicall(NewNodes, mnesia, stop, []),
    {ok,_} = view(NewBup, Mod),

    %% Sanity test
    case {lists:sort(OldRecs), lists:sort(NewRecs)} of
        {Same, Same} -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {counter_tab, size_tab, acc_size = 0, n_records = 0}).


%% Iterates over a backup file and shows some statistics
%% The identity of ets table containing the counters is not removed
count(BupFile) ->
    CounterTab = ets:new(?MODULE, [set, public]),
    SizeTab = ets:new(?MODULE, [set, public]),
    Mod = mnesia:system_info(backup_module),
    State = #state{counter_tab = CounterTab, size_tab = SizeTab},
    case mnesia:traverse_backup(BupFile, Mod, dummy, read_only, fun incr/2, State) of
	{ok, State2} ->
	    Res = display(State2),
	    ets:delete(CounterTab),
	    ets:delete(SizeTab),
	    Res;
	{error, Reason} ->	
	    ets:delete(CounterTab),
	    ets:delete(SizeTab),
	    {error, Reason}
    end.

incr(Rec, State) ->
    Tab = element(1, Rec),
    Key = element(2, Rec),
    Oid = {Tab, Key},
    incr_counter(State#state.counter_tab, Oid),
    Size = size(term_to_binary(Rec)),
    max_size(State#state.size_tab, Tab, Key, Size),
    AccSize = State#state.acc_size,
    N = State#state.n_records,
    State2 = State#state{acc_size = AccSize + Size, n_records = N + 1},
    {[Rec], State2}.

incr_counter(T, Counter) ->
    case catch ets:update_counter(T, Counter, 1) of
	{'EXIT', _} ->
	    ets:insert(T, {Counter, 1});
	_ ->
	    ignore
    end.

max_size(T, Tab, Key, Size) ->
    case catch ets:lookup_element(T, Tab, 2) of
	{'EXIT', _} ->
	    ets:insert(T, {Tab, Size, Key});
	OldSize when OldSize < Size ->
	    ets:insert(T, {Tab, Size, Key});
	_ ->
	    ignore
    end.

%% Displays the statistics found in the ets table
display(State) ->
    CounterTab = State#state.counter_tab,
    Tabs = [T || {{_, T}, _} <- match_tab(CounterTab, schema)],
    io:format("~w tables with totally: ~w records, ~w keys, ~w bytes~n",
	      [length(Tabs),
	       State#state.n_records,
	       ets:info(CounterTab, size),
	       State#state.acc_size]),	       
    display(State, lists:sort(Tabs)).

display(State, [Tab | Tabs]) ->
    Counters = match_tab(State#state.counter_tab, Tab),
    io:format("~-10w     records in table ~w~n", [length(Counters), Tab]),
    Fun = fun({_Oid, Val}) when Val < 5 ->
		  ignore;
	     ({Oid, Val}) ->
		  io:format("~-10w *** records with key ~w~n", [Val, Oid])
	  end,
    lists:foreach(Fun, Counters),
    display_size(State#state.size_tab, Tab),
    display(State, Tabs);
display(_CounterTab, []) ->
    ok.
    
match_tab(T, Tab) ->
    ets:match_object(T, {{Tab, '_'}, '_'}).

display_size(T, Tab) ->
    case catch ets:lookup(T, Tab) of
	[] ->
	    ignore;
	[{_, Size, Key}] when Size > 1000 ->
	    io:format("~-10w --- bytes occupied by largest record ~w~n",
		      [Size, {Tab, Key}]);
	[{_, _, _}] ->
	    ignore
    end.
