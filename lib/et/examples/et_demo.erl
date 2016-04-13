%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% et_demo.erl - Provide some event trace filter examples.
%%----------------------------------------------------------------------

-module(et_demo).

-export([
         sim_trans/0, sim_trans/1,
         live_trans/0, live_trans/1,
         mgr_actors/1,
         start/0, start/1,
         filters/0,
         trace_mnesia/0
        ]).

%% Test
-export([s/0, t/0, t/1, init/0, gen/3]).

-include_lib("et/include/et.hrl").

%%----------------------------------------------------------------------

%sim_trans
sim_trans() ->
    sim_trans([]).

sim_trans(ExtraOptions) ->
    Options = [{dict_insert, {filter, mgr_actors}, fun mgr_actors/1}],
    {ok, Viewer} = et_viewer:start_link(Options ++ ExtraOptions),
    Collector = et_viewer:get_collector_pid(Viewer),
    et_collector:report_event(Collector, 60, my_shell, mnesia_tm, start_outer, 
                              "Start outer transaction"),
    et_collector:report_event(Collector, 40, mnesia_tm, my_shell, new_tid, 
                              "New transaction id is 4711"),
    et_collector:report_event(Collector, 20, my_shell, mnesia_locker, try_write_lock, 
                              "Acquire write lock for {my_tab, key}"),
    et_collector:report_event(Collector, 10, mnesia_locker, my_shell, granted,
                              "You got the write lock for {my_tab, key}"),
    et_collector:report_event(Collector, 60, my_shell, do_commit,
                              "Perform  transaction commit"),
    et_collector:report_event(Collector, 40, my_shell, mnesia_locker, release_tid,
                              "Release all locks for transaction 4711"),
    et_collector:report_event(Collector, 60, my_shell, mnesia_tm, delete_transaction,
                              "End of outer transaction"),
    et_collector:report_event(Collector, 20, my_shell, end_outer,
                              "Transaction returned {atomic, ok}"),
    {collector, Collector}.

%sim_trans

%mgr_actors
mgr_actors(E) when is_record(E, event) ->
    Actor = fun(A) ->
               case A of
                   mnesia_tm     -> trans_mgr;
                   mnesia_locker -> lock_mgr;
                   _             -> A
               end
            end,
    {true, E#event{from = Actor(E#event.from),
                   to = Actor(E#event.to),
                   contents = [{orig_from, E#event.from},
                               {orig_to,   E#event.to},
                               {orig_contents, E#event.contents}]}}.
%mgr_actors

%%----------------------------------------------------------------------

%start
start() ->
    start([]).

start(ExtraOptions) ->
    Options = [{trace_global, true},
               {parent_pid, undefined},
               {max_actors, infinity},
               {max_events, 1000},
               {active_filter, module_as_actor}],
    et_viewer:start_link(filters() ++ Options ++ ExtraOptions).
%start

%%----------------------------------------------------------------------

%live_trans
live_trans() ->
    live_trans([]).

live_trans(ExtraOptions) ->
    Options = [{title, "Mnesia tracer"},
	       {hide_actions, true},
	       {active_filter, named_process_info_nolink}],
    et_demo:start(Options ++ ExtraOptions),
    mnesia:start(),
    mnesia:create_table(my_tab, [{ram_copies, [node()]}]),
    et_demo:trace_mnesia(),
    register(my_shell, self()),

    mnesia:transaction(fun() -> mnesia:write({my_tab, key, val}) end).
%live_trans

%%----------------------------------------------------------------------

%trace_mnesia
trace_mnesia() ->
    Modules = mnesia:ms(),
    Spec = [{message, {caller}}, {return_trace}],
    Flags = [send, 'receive', procs, timestamp],
    dbg:p(all, [call, timestamp]),
    [dbg:tpl(M, [{'_', [], Spec}]) || M <- Modules],
    LocallyRunningServers = [M || M <- Modules, whereis(M) /= undefined],
    [dbg:p(whereis(RS), Flags) || RS <- LocallyRunningServers],
    dbg:p(self(), Flags),
    LocallyRunningServers.
%trace_mnesia

%%----------------------------------------------------------------------
%% Filter funs returns false, true or {true, NewEvent}
%%----------------------------------------------------------------------

%filters
filters() ->
    [{dict_insert, {filter, module_as_actor},
                   fun module_as_actor/1},
     {dict_insert, {filter, plain_process_info},
                   fun plain_process_info/1},
     {dict_insert, {filter, plain_process_info_nolink},
                   fun plain_process_info_nolink/1},
     {dict_insert, {filter, named_process_info},
                   fun named_process_info/1},
     {dict_insert, {filter, named_process_info_nolink},
                   fun named_process_info_nolink/1},
     {dict_insert, {filter, node_process_info},
                   fun node_process_info/1},
     {dict_insert, {filter, node_process_info_nolink},
                   fun node_process_info_nolink/1},
     {dict_insert, {filter, application_as_actor},
                   fun application_as_actor/1}
    ].
%filters

%module_as_actor
module_as_actor(E) when is_record(E, event) ->
    case lists:keysearch(mfa, 1, E#event.contents) of
        {value, {mfa, {M, F, _A}}} ->
            case lists:keysearch(pam_result, 1, E#event.contents) of
                {value, {pam_result, {M2, _F2, _A2}}} ->
                    {true, E#event{label = F, from = M2, to = M}};
                _ ->
                    {true, E#event{label = F, from = M, to = M}}
            end;
        _ ->
            false
    end.
%module_as_actor

%%----------------------------------------------------------------------

%plain_process_info
plain_process_info(E) when is_record(E, event) ->
    case E#event.label of
        send                          -> true;
        send_to_non_existing_process  -> true;
        'receive'                     -> true;
        spawn                         -> true;
        exit                          -> true;
        link                          -> true;
        unlink                        -> true;
        getting_linked                -> true;
        {seq_send, _Label}            -> true;
        {seq_receive, _Label}         -> true;
        {seq_print, _Label}           -> true;
        {drop, _N}                    -> true;
        _                             -> false
    end.
%plain_process_info

%plain_process_info_nolink
plain_process_info_nolink(E) when is_record(E, event) ->
    (E#event.label /= link) and
    (E#event.label /= unlink) and
    (E#event.label /= getting_linked) and
    plain_process_info(E). 
%plain_process_info_nolink

%%----------------------------------------------------------------------

named_process_info(E) when is_record(E, event) ->
    case plain_process_info(E) of
        true ->
            {true, E#event{to    = pid_to_name(E#event.to),
                           from  = pid_to_name(E#event.from),
                           label = msg_to_label(E)}};
        false ->
            false
    end.

named_process_info_nolink(E) when is_record(E, event) ->
    case plain_process_info_nolink(E) of
        true ->
            {true, E#event{to    = pid_to_name(E#event.to),
                           from  = pid_to_name(E#event.from),
                           label = msg_to_label(E)}};
        false ->
            false
    end.

pid_to_name(Pid) when is_pid(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} ->
            Name;
        _ ->
            Pid
    end;
pid_to_name({Name, Node}) when Node == node() ->
    Name;
pid_to_name(Other) ->
    Other.

%%----------------------------------------------------------------------

node_process_info(E) when is_record(E, event) ->
    case plain_process_info(E) of
        true ->
            {true, E#event{to    = pid_to_node(E#event.to),
                           from  = pid_to_node(E#event.from),
                           label = msg_to_label(E)}};
        false ->
            false
    end.
node_process_info_nolink(E) when is_record(E, event) ->
    case plain_process_info_nolink(E) of
        true ->
            {true, E#event{to    = pid_to_node(E#event.to),
                           from  = pid_to_node(E#event.from),
                           label = msg_to_label(E)}};
        false ->
            false
    end.

pid_to_node(Pid) when is_pid(Pid) ->
    node(Pid);
pid_to_node(Name) when is_atom(Name) ->
    node();
pid_to_node({_Name, Node}) when is_atom(Node) ->
    Node.

%%----------------------------------------------------------------------

application_as_actor(E) when is_record(E, event) ->
    {true, E#event{to    = pid_to_application(E#event.to),
                   from  = pid_to_application(E#event.from),
                   label = msg_to_label(E)}}.

pid_to_application(Pid) when is_pid(Pid) ->
    case application:get_application(Pid) of
        {ok, Name} ->
            Name;
        _ ->
            "UNKNOWN"
    end.

%%----------------------------------------------------------------------

msg_to_label(E) when is_record(E, event) ->
    case lists:keysearch(msg, 1, E#event.contents) of
        {value, {msg, Msg}} ->
            mnesia_msg_to_label(Msg, E#event.label);
        false ->
            E#event.label
    end.

mnesia_msg_to_label(Msg, Label) ->
    case Msg of
        {mnesia_tm, _Node, Reply} ->
            case Reply of
                ok                                 -> ok;
                store_erased                       -> store_erased;
                unblocked                          -> unblocked;
                {_From, _Ref, start_outer}         -> start_outer;
                {aborted, _Tid}                    -> aborted;
                {committed, _Tid}                  -> committed;
                {dirty_res, _Res}                  -> dirty_res;
                {error, _Reason}                   -> error;
                {info, _Part, _Coord}              -> info;
                {mnesia_tm, _Node, {'EXIT', _Reason}}   -> 'EXIT';
                {mnesia_tm, _Node, {dirty_res, _Reply}} -> dirty_res;
                {new_store, _Etab}                 -> new_store;
                {new_tid, _Tid, _Etab}             -> new_tid;
                {ok, _Name, _IgnoreNew, _Node}     -> ok;
                {restarted, _Tid}                  -> restarted;
                {vote_yes, _Tid, _Self}            -> vote_yes;
                {vote_yes, _Tid}                   -> vote_yes;
                {acc_pre_commit, _Tid, _Self, _Expect} -> acc_pre_commit;
                {schema_commit, _Tid, _Self}       -> schema_commit;
                {vote_no, _Tid, _Reason}           -> vote_no
            end;
        {'$gen_cast',allow_garb}                   -> allow_garb;
        {'$gen_cast', {release_schema_commit_lock, _Pid}} -> release_schema_commit_lock;
        {'$gen_call', _Ref, {mktab, _Name,_Args}}  -> mktab;
        {'$gen_call', _Ref, wait_for_schema_commit_lock} -> wait_for_schema_commit_lock;
        {'$gen_call', _Ref, {set_lock, _Key}}      -> set_global_lock;
        {'$gen_call', _Ref, {del_lock, _Key}}      -> del_global_lock;
        {'$gen_call', _Ref, {what_happened, _Decision, _Tid}} -> what_happened;
        {async_dump_log, _Reason}                  -> async_dump_log;
        check_overload                             -> check_overload;
        {dumper_done, _Pid, dumped}                -> dumper_done;
        garb_decisions                             -> garb_decisions;
        timeout                                    -> timeout;
        {mnesia_locker, _Node, granted}            -> granted;
        {mnesia_locker, _Node, {granted, _Lookup}} -> granted;
        {'EXIT', _Pid, _Reason}                    -> 'EXIT';
        {_From, info}                              -> info;
        {_From, start_outer}                       -> start_outer;
        {_From, {add_store, _Tid}}                 -> add_store;
        {_From, {ask_commit, _Prot, _Tid, _Commit, _DiscNs, _RamNs}} -> ask_commit;
        {_From, {async_dirty, _Tid, _Commit, _Tab}} -> async_dirty;
        {_From, {block_tab, _Tab}} -> block_tab;
        {_From, {del_store, _Tid, _Current, _Obsolete, _Prop}} -> del_store;
        {_From, {prepare_checkpoint, _Cp}}         ->prepare_checkpoint;
        {_From, {read, _Tid, _Oid}}                -> try_read_lock;
        {_From, {read_write, _Tid, _Oid}}          -> try_read_write_lock;
        {_From, {restart, _Tid, _Store}}           -> restart;
        {_From, {sync_dirty, _Tid, _Commit, _Tab}} -> sync_dirty;
        {_From, {unblock_me, _Tab}}                -> unblock_me;
        {_From, {unblock_tab, _Tab}}               -> unblock_tab;
        {_From, {write, _Tid, _Oid}}               -> try_write_lock;
        {_Tid, committed}                          -> committed;
        {_Tid, do_commit}                          -> do_commit;
        {_Tid, pre_commit}                         -> pre_commit;
        {_Tid, simple_commit}                      -> simple_commit;
        {_Tid, {do_abort, _Reason}}                -> do_abort;
        {activity_ended, _, _Pid}                  -> activity_ended;
        {ask_commit, _Protocol, _Tid, _Bin, _DiscNs, _RamNs} -> ask_commit;
        {delete_transaction, _Tid}                 -> delete_transaction;
        {mnesia_down, _Node}                       -> mnesia_down;
        {release_tid, _Tid}                        -> release_tid;
        {sync_trans_serial, _Tid}                  -> sync_trans_serial;
        {system, _From, _Msg}                      -> system;
        {transaction_done, _Res, _Pid}             -> transaction_done;
        {_Tid, {do_abort, _Reason}}                -> do_abort;
        {_Ref, granted}                            -> granted;
        _                                          -> Label
    end.

%%----------------------------------------------------------------------

s() ->
    spawn(fun() -> t(), timer:sleep(infinity) end).
		  
t() ->
    t(500).

t(N) ->
    Collector = init(),
    gen(Collector, 1, N),
    Collector.

init() ->
    EvenFilter =
	fun(#event{label = Label}) ->
		case catch (list_to_integer(Label) div 10) rem 2 of
		    0 ->
			false;
		    _ ->
			true
		end
	end,
    OddFilter = fun(E) -> not EvenFilter(E) end,
    {ok, Viewer} = et_viewer:start_link([{dict_insert, {filter, odd_tens}, EvenFilter},
					 {dict_insert, {filter, even_tens}, OddFilter},
					 {active_filter, odd_tens}]),
    et_viewer:get_collector_pid(Viewer).

gen(Collector, From, To) ->
    [et_collector:report_event(Collector, 20, from, to, integer_to_list(I), [I]) || I <- lists:seq(From, To)], 
    ok.
