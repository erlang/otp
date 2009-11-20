%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose : A front-end to the erlang:process_info() functions, that
%%           can handle processes on different nodes in a transparent
%%           way. 
%%	     Also some convenience functions for process info, as well
%%	     as some application specific functions for process 
%%           classification.
%%----------------------------------------------------------------------

-module(pman_process).

-export([pinfo/1, pinfo/2,
	 r_processes/1,
	 function_info/1,
	 get_name/1, msg/1, reds/1, psize/1,
	 is_running/1,
	 is_pid_or_shell/1,
	 get_pid/1,
	 is_system_process/1,
	 is_hidden_by_module/2
	]).

%% List of registered name that will make a prodcess a "SYSTEM"-process 
-define(SYSTEM_REG_NAMES,
	[
	 %% kernel
	 application_controller,
	 erl_reply,
	 auth,
	 boot_server,
	 code_server,
	 disk_log_server,
	 disk_log_sup,
	 erl_prim_loader,
	 error_logger,
	 file_server_2,
	 fixtable_server,
	 global_group,
	 global_name_server,
	 heart,
	 inet_gethost_native,
	 inet_gethost_native_sup,
	 init,
	 kernel_config,
	 kernel_safe_sup,
	 kernel_sup,
	 net_kernel,
	 net_sup,
	 rex,
	 user,
	 os_server,
	 ddll_server,
	 erl_epmd,
	 inet_db,
	 pg2,

	 %% stdlib
	 timer_server,
	 rsh_starter,
	 take_over_monitor,
	 pool_master,
	 dets,

	 %% sasl
	 sasl_safe_sup, sasl_sup, alarm_handler, overload,
	 release_handler,

	 %% gs
	 gs_frontend
	]).
	 
%% List of module:function/arity calls that will make the caller a 
%% "SYSTEM"-process.
%% 
-define(SYSTEM_INIT_CALLS,
	[{application_master,init,4},
	 {application_master,start_it,4},
	 {inet_tcp_dist,accept_loop,2},
	 {net_kernel,ticker,2},
	 {supervisor_bridge,user_sup,1},
	 {user_drv,server,2},
	 {group,server,3},
	 {kernel_config,init,1},
	 {inet_tcp_dist,do_accept,6},
	 {inet_tcp_dist,do_setup,6},
	 {pman_main,init,2},
	 {pman_buf_printer,init,2},
	 {pman_buf_converter,init,2},
	 {pman_buf_buffer,init,1},
	 {gstk,init,1},
	 {gstk_port_handler,init,2},
	 {gstk,worker_init,1}
	]).

%% List of module:function/arity calls that will make the executing
%% process a "SYSTEM"-process.
-define(SYSTEM_RUNNING_CALLS,
	[{file_io_server,server_loop,1},
	 {global,loop_the_locker,1},
	 {global,collect_deletions,2},
	 {global,loop_the_registrar,0},
	 {gs_frontend,request,2},
	 {shell,get_command1,5},
	 {shell,eval_loop,3},
	 {io,wait_io_mon_reply,2},
	 {pman_module_info,loop,1},
	 {pman_options,dialog,3},
	 {pman_options,loop,1},
	 {pman_relay_server,loop,1},
	 {pman_shell,monitor_loop,1},
	 {pman_shell,safe_loop,2}
	]).

%% pinfo(Pid) -> [{Item, Info}] | undefined
%% pinfo(Pid, Item) -> Info | undefined
%% A version of process_info/1 that handles pid on remote nodes as well.
pinfo({_, Pid}) -> % Handle internal process format
    pinfo(Pid);
pinfo(Pid) when node(Pid)==node() ->
    process_info(Pid);
pinfo(Pid) ->
    case rpc:call(node(Pid), erlang, process_info, [Pid]) of
	{badrpc, _} -> undefined;
	Res -> Res
    end.

pinfo({_, Pid}, Item) -> % Handle internal process format
    pinfo(Pid, Item);
pinfo(Pid, Item) when node(Pid)==node() ->
    case process_info(Pid, Item) of
	{Item, Info} -> Info;
	"" -> ""; % Item == registered_name
	undefined -> undefined
    end;
pinfo(Pid, Item) ->
    case rpc:call(node(Pid), erlang, process_info, [Pid, Item]) of
	{badrpc, _} -> undefined;
	{Item, Info} -> Info;
	"" -> ""; % Item == registered_name
	undefined -> undefined
    end.

%% function_info(Pid) -> {M, F, A}
%% Returns the initial function for the specified process. 
function_info(Pid) ->
    case pinfo(Pid, current_function) of
	{Module, Function, Arity} ->
	    {Module, Function, Arity};
	undefined ->
	    {unknown, unknown, 0}
    end.

%% r_processes(Node) -> Pids
%% Return a list of all processes at Node.
%%
%% If there is a problem with getting information from a remote 
%% node, an empty list is returned.
r_processes(Node) ->
    ordsets:from_list(r_processes1(Node)).

r_processes1(Node) ->
    if
	Node==node() ->
	    processes();
	true ->
	    case rpc:block_call(Node, erlang, processes, []) of
		{badrpc, _} ->
		    [];
		Pids -> Pids
	    end
    end.

%% is_running(Object) -> {true, {shell,Pid}} | {true, Pid} | false
%%   Object = {shell, Pid} | {link, Pid, ?} | Pid
is_running({shell,Pid}) -> 
    case is_running(Pid) of
	{true,Pid} ->
	    {true,{shell,Pid}};
	false ->
	    false
    end;
is_running({link,Pid,_}) -> 
    is_running(Pid);
is_running(Pid) ->
    case is_pid_or_shell(Pid) of
	true ->
	    case pinfo(Pid) of
		undefined -> false;
		_PInfo -> {true, Pid}
	    end;
	false ->
	    false
    end.

%% is_pid_or_shell(Object) -> bool()
%% Checks if the argument is an pid or a tuple {shell, Pid}.
is_pid_or_shell({shell,Pid}) when is_pid(Pid) ->
    true;
is_pid_or_shell(Pid) when is_pid(Pid) ->
    true;
is_pid_or_shell(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_pid/1 - returns the Pid of the object provided that 
%%   it is a proper process specifier.
%%
%% Arguments:
%%   Object	A process specifier
%%
%% Returns:
%%   The Pid.

get_pid({shell,Pid}) ->
    Pid;
get_pid(Pid) when is_pid(Pid) ->
    Pid.

%% is_system_process(Pid) -> bool()
%% Returns true if Pid is a "system process".
%% This is a prototype version, use file configuration later.
is_system_process(Pid) ->
    catch is_system_process2(Pid).

is_system_process2(Pid) ->
    
    %% Test if the registered name is a system registered name
    case pinfo(Pid, registered_name) of
	undefined -> ignore;
	"" -> ignore;
	Name ->
	    case lists:member(Name, ?SYSTEM_REG_NAMES) of
		true -> throw(true);
		false -> ignore
	    end
    end,
    
    %% Test if the start specification is a "system start function"
    MFAi = case pinfo(Pid, initial_call) of
	       {proc_lib, init_p, 5} ->
		   proc_lib:translate_initial_call(Pid); % {M,F,A} | Fun
	       Res -> Res % {M,F,A} | undefined
	   end,
    case lists:member(MFAi, ?SYSTEM_INIT_CALLS) of
	true -> throw(true);
	false -> ignore
    end,

    %% Test if the running specification is a "system running function"
    case pinfo(Pid, current_function) of
	undefined -> false;
	MFAc ->
	    lists:member(MFAc, ?SYSTEM_RUNNING_CALLS)
    end.

%% is_hidden_by_module(Pid, Modules) -> bool()
%% Checks if Pid is to be hidden because it executes code from one
%% of Modules
is_hidden_by_module(Pid, Modules) ->
    case pinfo(Pid, current_function) of
	{Module, _Function, _Arity} ->
	    lists:member(Module, Modules);
	undefined -> false
    end.

%% get_name(Pid) -> Name | " "
%% Returns the registered name of a process, if any, or " " otherwise.
get_name(Pid) ->
    case pinfo(Pid, registered_name) of
	undefined -> " ";
	"" -> " ";
	Name -> Name
    end.

%% msg(Pid) -> int()
msg(Pid) ->
    case pinfo(Pid, messages) of
	undefined -> 0;
	Msgs -> length(Msgs)
    end.

%% reds(Pid) -> int()
reds(Pid) ->
    case pinfo(Pid, reductions) of
	undefined -> 0;	    
	Reds -> Reds
    end.

%% psize(Pid) -> int()
%% Returns the total process size (stack + heap).
psize(Pid) ->
    Stack = pinfo(Pid, stack_size),
    Heap = pinfo(Pid, heap_size),
    case {Heap, Stack} of
	{undefined, undefined} -> 0;
	{undefined, Sz} -> Sz;
	{Sz, undefined} -> Sz;
	{Sz0, Sz1}  -> Sz0 + Sz1
    end.
