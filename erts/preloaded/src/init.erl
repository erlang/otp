%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
%%
%%           New initial version of init.
%% Booting from a script. The script is fetched either from
%% a local file or distributed from another erlang node.
%% 
%% Flags:
%%        -id Identity   : identity of the system.
%%        -boot File     : Absolute file name of the boot script.
%%        -boot_var Var Value
%%                       : $Var in the boot script is expanded to
%%                         Value.
%%        -loader LoaderMethod
%%                       : efile, inet, ose_inet
%%                         (Optional - default efile)
%%        -hosts [Node]  : List of hosts from which we can boot.
%%                         (Mandatory if -loader inet or ose_inet)
%%        -mode embedded : Load all modules at startup, no automatic loading
%%        -mode interactive : Auto load modules (default system behaviour).
%%        -path          : Override path in bootfile.
%%        -pa Path+      : Add my own paths first.
%%        -pz Path+      : Add my own paths last.
%%        -run           : Start own processes.
%%        -s             : Start own processes.
%% 
%% Experimental flags:
%%        -init_debug      : Activate debug printouts in init
%%        -loader_debug    : Activate debug printouts in erl_prim_loader
%%        -code_path_choice : strict | relaxed

-module(init).

-export([restart/0,reboot/0,stop/0,stop/1,
	 get_status/0,boot/1,get_arguments/0,get_plain_arguments/0,
	 get_argument/1,script_id/0]).

%% internal exports
-export([fetch_loaded/0,ensure_loaded/1,make_permanent/2,
	 notify_when_started/1,wait_until_started/0, 
	 objfile_extension/0, archive_extension/0,code_path_choice/0]).

-include_lib("kernel/include/file.hrl").

-type internal_status() :: 'starting' | 'started' | 'stopping'.

-record(state, {flags = [],
		args = [],
		start = [],
		kernel = []                   :: [{atom(), pid()}],
		bootpid                       :: pid(),
		status = {starting, starting} :: {internal_status(), term()},
		script_id = [],
		loaded = [],
		subscribed = []}).

-define(ON_LOAD_HANDLER, init__boot__on_load_handler).

debug(false, _) -> ok;
debug(_, T)     -> erlang:display(T).

-spec get_arguments() -> [{atom(), [string()]}].
get_arguments() ->
    request(get_arguments).

-spec get_plain_arguments() -> [string()].
get_plain_arguments() ->
    bs2ss(request(get_plain_arguments)).

-spec get_argument(atom()) -> 'error' | {'ok', [[string()]]}.
get_argument(Arg) ->
    request({get_argument, Arg}).

-spec script_id() -> term().
script_id() ->
    request(script_id).

bs2as(L0) when is_list(L0) ->
    map(fun b2a/1, L0);
bs2as(L) ->
    L.

bs2ss(L0) when is_list(L0) ->
    map(fun b2s/1, L0);
bs2ss(L) ->
    L.

-spec get_status() -> {internal_status(), term()}.
get_status() ->
    request(get_status).

-spec fetch_loaded() -> [atom()].
fetch_loaded() ->
    request(fetch_loaded).

%% Handle dynamic code loading until the
%% real code_server has been started.
-spec ensure_loaded(atom()) -> 'not_allowed' | {'module', atom()}.
ensure_loaded(Module) ->
    request({ensure_loaded, Module}).

-spec make_permanent(file:filename(), 'false' | file:filename()) ->
	'ok' | {'error', term()}.
make_permanent(Boot,Config) ->
    request({make_permanent,Boot,Config}).

-spec notify_when_started(pid()) -> 'ok' | 'started'.
notify_when_started(Pid) ->
    request({notify_when_started,Pid}).

-spec wait_until_started() -> 'ok'.
wait_until_started() ->
    receive 
	{init,started} -> ok 
    end.	    

request(Req) ->
    init ! {self(),Req},
    receive 
	{init,Rep} -> 
	    Rep
    end.

-spec restart() -> 'ok'.
restart() -> init ! {stop,restart}, ok.

-spec reboot() -> 'ok'.
reboot() -> init ! {stop,reboot}, ok.

-spec stop() -> no_return().
stop() -> init ! {stop,stop}, ok.

-spec stop(non_neg_integer() | string()) -> no_return().
stop(Status) -> init ! {stop,{stop,Status}}, ok.

-spec boot([binary()]) -> no_return().
boot(BootArgs) ->
    register(init, self()),
    process_flag(trap_exit, true),
    start_on_load_handler_process(),
    {Start0,Flags,Args} = parse_boot_args(BootArgs),
    Start = map(fun prepare_run_args/1, Start0),
    Flags0 = flags_to_atoms_again(Flags),
    boot(Start,Flags0,Args).

prepare_run_args({eval, [Expr]}) ->
    {eval,Expr};
prepare_run_args({_, L=[]}) ->
    bs2as(L);
prepare_run_args({_, L=[_]}) ->
    bs2as(L);
prepare_run_args({s, [M,F|Args]}) ->
    [b2a(M), b2a(F) | bs2as(Args)];
prepare_run_args({run, [M,F|Args]}) ->
    [b2a(M), b2a(F) | bs2ss(Args)].

b2a(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin));
b2a(A) when is_atom(A) ->
    A.

b2s(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
b2s(L) when is_list(L) ->
    L.

map(_F, []) ->
    [];
map(F, [X|Rest]) ->
    [F(X) | map(F, Rest)].

flags_to_atoms_again([]) ->
    [];
flags_to_atoms_again([{F0,L0}|Rest]) ->
    L = L0,
    F = b2a(F0),
    [{F,L}|flags_to_atoms_again(Rest)];
flags_to_atoms_again([{F0}|Rest]) ->
    F = b2a(F0),
    [{F}|flags_to_atoms_again(Rest)].

-spec code_path_choice() -> 'relaxed' | 'strict'.
code_path_choice() ->
    case get_argument(code_path_choice) of
	{ok,[["strict"]]} ->
	    strict;
	{ok,[["relaxed"]]} ->
	    relaxed;
	_Else ->
	    relaxed
    end.

boot(Start,Flags,Args) ->
    BootPid = do_boot(Flags,Start),
    State = #state{flags = Flags,
		   args = Args,
		   start = Start,
		   bootpid = BootPid},
    boot_loop(BootPid,State).

%%% Convert a term to a printable string, if possible.
to_string(X) when is_list(X) ->			% assume string
    F = flatten(X, []),
    case printable_list(F) of
	true ->  F;
	false -> ""
    end;
to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_pid(X) ->
    pid_to_list(X);
to_string(X) when is_float(X) ->
    float_to_list(X);
to_string(X) when is_integer(X) ->
    integer_to_list(X);
to_string(_X) ->
    "".						% can't do anything with it

%% This is an incorrect and narrow definition of printable characters.
%% The correct one is in io_lib:printable_list/1
%%
printable_list([H|T]) when is_integer(H), H >= 32, H =< 126 ->
    printable_list(T);
printable_list([$\n|T]) -> printable_list(T);
printable_list([$\r|T]) -> printable_list(T);
printable_list([$\t|T]) -> printable_list(T);
printable_list([]) -> true;
printable_list(_) ->  false.

flatten([H|T], Tail) when is_list(H) ->
    flatten(H, flatten(T, Tail));
flatten([H|T], Tail) ->
    [H|flatten(T, Tail)];
flatten([], Tail) ->
    Tail.

things_to_string([X|Rest]) ->
    " (" ++ to_string(X) ++ ")" ++ things_to_string(Rest);
things_to_string([]) ->
    "".

halt_string(String, List) ->
    HaltString = String ++ things_to_string(List),
    if
	length(HaltString)<199 -> HaltString;
	true -> first198(HaltString, 198)
    end.

first198([H|T], N) when N>0 ->
    [H|first198(T, N-1)];
first198(_, 0) ->
    [].

%% String = string()
%% List = [string() | atom() | pid() | number()]
%% Any other items in List, such as tuples, are ignored when creating
%% the string used as argument to erlang:halt/1.
crash(String, List) ->
    halt(halt_string(String, List)).

%% Status is {InternalStatus,ProvidedStatus}
-spec boot_loop(pid(), #state{}) -> no_return().
boot_loop(BootPid, State) ->
    receive
	{BootPid,loaded,ModLoaded} ->
	    Loaded = State#state.loaded,
	    boot_loop(BootPid,State#state{loaded = [ModLoaded|Loaded]});
	{BootPid,started,KernelPid} ->
	    boot_loop(BootPid, new_kernelpid(KernelPid, BootPid, State));
	{BootPid,progress,started} ->
            {InS,_} = State#state.status,
	    notify(State#state.subscribed),
	    boot_loop(BootPid,State#state{status = {InS,started},
					  subscribed = []});
	{BootPid,progress,NewStatus} ->
            {InS,_} = State#state.status,
	    boot_loop(BootPid,State#state{status = {InS,NewStatus}});
	{BootPid,{script_id,Id}} ->
	    boot_loop(BootPid,State#state{script_id = Id});
	{'EXIT',BootPid,normal} ->
            {_,PS} = State#state.status,
	    notify(State#state.subscribed),
	    loop(State#state{status = {started,PS},
			     subscribed = []});
	{'EXIT',BootPid,Reason} ->
	    erlang:display({"init terminating in do_boot",Reason}),
	    crash("init terminating in do_boot", [Reason]);
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
	    boot_loop(BootPid,State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->   %% Fetch and reset initially loaded modules.
	    case whereis(?ON_LOAD_HANDLER) of
		undefined ->
		    %% There is no on_load handler process,
		    %% probably because init:restart/0 has been
		    %% called and it is not the first time we
		    %% pass through here.
		    ok;
		Pid when is_pid(Pid) ->
		    Pid ! run_on_load,
		    receive
			{'EXIT',Pid,on_load_done} ->
			    ok;
			{'EXIT',Pid,Res} ->
			    %% Failure to run an on_load handler.
			    %% This is fatal during start-up.
			    exit(Res)
		    end
	    end,
	    From ! {init,State#state.loaded},
	    garb_boot_loop(BootPid,State#state{loaded = []});
	{From,{ensure_loaded,Module}} ->
	    {Res, Loaded} = ensure_loaded(Module, State#state.loaded),
	    From ! {init,Res},
	    boot_loop(BootPid,State#state{loaded = Loaded});
	Msg ->
	    boot_loop(BootPid,handle_msg(Msg,State))
    end.

ensure_loaded(Module, Loaded) ->
    File = concat([Module,objfile_extension()]),
    case catch load_mod(Module,File) of
	{ok, FullName} ->
	    {{module, Module}, [{Module, FullName}|Loaded]};
	Res ->
	    {Res, Loaded}
    end.

%% Tell subscribed processes the system has started.
notify(Pids) ->
    lists:foreach(fun(Pid) -> Pid ! {init,started} end, Pids).

%% Garbage collect all info about initially loaded modules.
%% This information is temporary stored until the code_server
%% is started.
%% We force the garbage collection as the init process holds
%% this information during the initialisation of the system and
%% it will be automatically garbed much later (perhaps not at all
%% if it is not accessed much).

garb_boot_loop(BootPid,State) ->
    garbage_collect(),
    boot_loop(BootPid,State).

new_kernelpid({Name,{ok,Pid}},BootPid,State) when is_pid(Pid) ->
    link(Pid),
    BootPid ! {self(),ok,Pid},
    Kernel = State#state.kernel,
    State#state{kernel = [{Name,Pid}|Kernel]};
new_kernelpid({_Name,ignore},BootPid,State) ->
    BootPid ! {self(),ignore},
    State;
new_kernelpid({Name,What},BootPid,State) ->
    erlang:display({"could not start kernel pid",Name,What}),
    clear_system(BootPid,State),
    crash("could not start kernel pid", [Name, What]).

%% Here is the main loop after the system has booted.

loop(State) ->
    receive
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt()!
	    loop(State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->           %% The Loaded info is cleared in
	    Loaded = State#state.loaded, %% boot_loop but is handled here 
	    From ! {init,Loaded},        %% anyway.
	    loop(State);
	{From, {ensure_loaded, _}} ->
	    From ! {init, not_allowed},
	    loop(State);
	Msg ->
	    loop(handle_msg(Msg,State))
    end.

handle_msg(Msg,State0) ->
    case catch do_handle_msg(Msg,State0) of
	{new_state,State} -> State;
	_                 -> State0
    end.

do_handle_msg(Msg,State) ->
    #state{flags = Flags,
	   status = Status,
	   script_id = Sid,
	   args = Args,
	   subscribed = Subscribed} = State,
    case Msg of
	{From,get_plain_arguments} ->
	    From ! {init,Args};
	{From,get_arguments} ->
	    From ! {init,get_arguments(Flags)};
	{From,{get_argument,Arg}} ->
	    From ! {init,get_argument(Arg,Flags)};
	{From,get_status} -> 
	    From ! {init,Status};
	{From,script_id} -> 
	    From ! {init,Sid};
	{From,{make_permanent,Boot,Config}} ->
	    {Res,State1} = make_permanent(Boot,Config,Flags,State),
	    From ! {init,Res},
	    {new_state,State1};
	{From,{notify_when_started,Pid}} ->
	    case Status of
		{InS,PS} when InS =:= started ; PS =:= started ->
		    From ! {init,started};
		_ ->
		    From ! {init,ok},
		    {new_state,State#state{subscribed = [Pid|Subscribed]}}
	    end;
	X ->
	    case whereis(user) of
		undefined ->
		    catch error_logger ! {info, self(), {self(), X, []}};
		User ->
		    User ! X,
		    ok
	    end
    end.		  

%%% -------------------------------------------------
%%% A new release has been installed and made
%%% permanent.
%%% Both restart/0 and reboot/0 shall startup using
%%% the new release. reboot/0 uses new boot script
%%% and configuration file pointed out externally.
%%% In the restart case we have to set new -boot and
%%% -config arguments.
%%% -------------------------------------------------

make_permanent(Boot,Config,Flags0,State) ->
    case set_flag('-boot',Boot,Flags0) of
	{ok,Flags1} ->
	    case set_flag('-config',Config,Flags1) of
		{ok,Flags} ->
		    {ok,State#state{flags = Flags}};
		Error ->
		    {Error,State}
	    end;
	Error ->
	    {Error,State}
    end.

set_flag(_Flag,false,Flags) ->
    {ok,Flags};
set_flag(Flag,Value,Flags) when is_list(Value) ->
    case catch list_to_binary(Value) of
	{'EXIT',_} ->
	    {error,badarg};
	AValue ->
	    {ok,set_argument(Flags,Flag,AValue)}
    end;
set_flag(_,_,_) ->
    {error,badarg}.

%%% -------------------------------------------------
%%% Stop the system. 
%%% Reason is: restart | reboot | stop
%%% According to reason terminate emulator or restart
%%% system using the same init process again.
%%% -------------------------------------------------

stop(Reason,State) ->
    BootPid = State#state.bootpid,
    {_,Progress} = State#state.status,
    State1 = State#state{status = {stopping, Progress}},
    clear_system(BootPid,State1),
    do_stop(Reason,State1).

do_stop(restart,#state{start = Start, flags = Flags, args = Args}) ->
    boot(Start,Flags,Args);
do_stop(reboot,_) ->
    halt();
do_stop(stop,State) ->
    stop_heart(State),
    halt();
do_stop({stop,Status},State) ->
    stop_heart(State),
    halt(Status).

clear_system(BootPid,State) ->
    Heart = get_heart(State#state.kernel),
    shutdown_pids(Heart,BootPid,State),
    unload(Heart).

stop_heart(State) ->
    case get_heart(State#state.kernel) of
	false ->
	    ok;
	Pid ->
	    %% As heart survives a restart the Parent of heart is init.
	    BootPid = self(),
	    %% ignore timeout
	    shutdown_kernel_pid(Pid, BootPid, self(), State) 
    end.

shutdown_pids(Heart,BootPid,State) ->
    Timer = shutdown_timer(State#state.flags),
    catch shutdown(State#state.kernel,BootPid,Timer,State),
    kill_all_pids(Heart), % Even the shutdown timer.
    kill_all_ports(Heart),
    flush_timout(Timer).

get_heart([{heart,Pid}|_Kernel]) -> Pid;
get_heart([_|Kernel])           -> get_heart(Kernel);
get_heart(_)                    -> false.


shutdown([{heart,_Pid}|Kernel],BootPid,Timer,State) ->
    shutdown(Kernel, BootPid, Timer, State);
shutdown([{_Name,Pid}|Kernel],BootPid,Timer,State) ->
    shutdown_kernel_pid(Pid, BootPid, Timer, State),
    shutdown(Kernel,BootPid,Timer,State);
shutdown(_,_,_,_) ->
    true.


%%
%% A kernel pid must handle the special case message
%% {'EXIT',Parent,Reason} and terminate upon it!
%%
shutdown_kernel_pid(Pid, BootPid, Timer, State) ->
    Pid ! {'EXIT',BootPid,shutdown},
    shutdown_loop(Pid, Timer, State, []).

%%
%% We have to handle init requests here in case a process
%% performs such a request and cannot shutdown (deadlock).
%% Keep all other EXIT messages in case it was another
%% kernel process. Resend these messages and handle later.
%%
shutdown_loop(Pid,Timer,State,Exits) ->
    receive
	{'EXIT',Pid,_} ->
	    resend(reverse(Exits)),
	    ok;
	{Timer,timeout} ->
	    erlang:display({init,shutdown_timeout}),
	    throw(timeout);
	{stop,_} ->
	    shutdown_loop(Pid,Timer,State,Exits);
	{From,fetch_loaded} ->
	    From ! {init,State#state.loaded},
	    shutdown_loop(Pid,Timer,State,Exits);
	{'EXIT',OtherP,Reason} ->
	    shutdown_loop(Pid,Timer,State,
			  [{'EXIT',OtherP,Reason}|Exits]);
	Msg ->
	    State1 = handle_msg(Msg,State),
	    shutdown_loop(Pid,Timer,State1,Exits)
    end.

resend([ExitMsg|Exits]) ->
    self() ! ExitMsg,
    resend(Exits);
resend(_) ->
    ok.

%%
%% Kill all existing pids in the system (except init and heart).
kill_all_pids(Heart) ->
    case get_pids(Heart) of
	[] ->
	    ok;
	Pids ->
	    kill_em(Pids),
	    kill_all_pids(Heart)  % Continue until all are really killed.
    end.
    
%% All except zombies.
alive_processes() ->
    [P || P <- processes(), erlang:is_process_alive(P)].

get_pids(Heart) ->
    Pids = alive_processes(),
    delete(Heart,self(),Pids).

delete(Heart,Init,[Heart|Pids]) -> delete(Heart,Init,Pids);
delete(Heart,Init,[Init|Pids])  -> delete(Heart,Init,Pids);
delete(Heart,Init,[Pid|Pids])   -> [Pid|delete(Heart,Init,Pids)];
delete(_,_,[])                  -> [].
    
kill_em([Pid|Pids]) ->
    exit(Pid,kill),
    kill_em(Pids);
kill_em([]) ->
    ok.

%%
%% Kill all existing ports in the system (except the heart port),
%% i.e. ports still existing after all processes have been killed.
%%
%% System ports like the async driver port will nowadays be immortal;
%% therefore, it is ok to send them exit signals...
%%
kill_all_ports(Heart) ->
    kill_all_ports(Heart,erlang:ports()).

kill_all_ports(Heart,[P|Ps]) ->
    case erlang:port_info(P,connected) of
	{connected,Heart} ->
	    kill_all_ports(Heart,Ps);
	_ ->
	    exit(P,kill),
	    kill_all_ports(Heart,Ps)
    end;
kill_all_ports(_,_) ->
    ok.

unload(false) ->
    do_unload(sub(erlang:pre_loaded(),erlang:loaded()));
unload(_) ->
    do_unload(sub([heart|erlang:pre_loaded()],erlang:loaded())).

do_unload([M|Mods]) ->
    catch erlang:purge_module(M),
    catch erlang:delete_module(M),
    catch erlang:purge_module(M),
    do_unload(Mods);
do_unload([]) ->
    ok.

sub([H|T],L) -> sub(T,del(H,L));
sub([],L)    -> L.
    
del(Item, [Item|T]) -> T;
del(Item, [H|T])    -> [H|del(Item, T)];
del(_Item, [])      -> [].

%%% -------------------------------------------------
%%% If the terminated Pid is one of the processes
%%% added to the Kernel, take down the system brutally.
%%% We are not sure that ANYTHING can work anymore,
%%% i.e. halt the system.
%%% Sleep awhile, it is thus possible for the
%%% error_logger (if it is still alive) to write errors
%%% using the simplest method.
%%% -------------------------------------------------

terminate(Pid,Kernel,Reason) ->
    case kernel_pid(Pid,Kernel) of
	{ok,Name} ->
	    sleep(500), %% Flush error printouts!
	    erlang:display({"Kernel pid terminated",Name,Reason}),
	    crash("Kernel pid terminated", [Name, Reason]);
	_ ->
	    false
    end.

kernel_pid(Pid,[{Name,Pid}|_]) ->
    {ok,Name};
kernel_pid(Pid,[_|T]) ->
    kernel_pid(Pid,T);
kernel_pid(_,_) ->
    false.

sleep(T) -> receive after T -> ok end.

%%% -------------------------------------------------
%%% Start the loader. 
%%% The loader shall run for ever!
%%% -------------------------------------------------

start_prim_loader(Init,Id,Pgm,Nodes,Path,{Pa,Pz}) ->
    case erl_prim_loader:start(Id,Pgm,Nodes) of
	{ok,Pid} when Path =:= false ->
	    InitPath = append(Pa,["."|Pz]),
	    erl_prim_loader:set_path(InitPath),
	    add_to_kernel(Init,Pid),
	    Pid;
	{ok,Pid} ->
	    erl_prim_loader:set_path(Path),
	    add_to_kernel(Init,Pid),
	    Pid;
	{error,Reason} ->
	    erlang:display({"cannot start loader",Reason}),
	    exit(Reason)
    end.

add_to_kernel(Init,Pid) ->
    Init ! {self(),started,{erl_prim_loader,{ok,Pid}}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),
	    ok
    end.

prim_load_flags(Flags) ->
    PortPgm = get_flag('-loader',Flags,<<"efile">>),
    Hosts = get_flag_list('-hosts', Flags, []),
    Id = get_flag('-id',Flags,none),
    Path = get_flag_list('-path',Flags,false),
    {PortPgm, Hosts, Id, Path}.

%%% -------------------------------------------------
%%% The boot process fetches a boot script and loads
%%% all modules specified and starts spec. processes.
%%% Processes specified with -s or -run are finally started.
%%% -------------------------------------------------

do_boot(Flags,Start) ->
    Self = self(),
    spawn_link(fun() -> do_boot(Self,Flags,Start) end).

do_boot(Init,Flags,Start) ->
    process_flag(trap_exit,true),
    {Pgm0,Nodes,Id,Path} = prim_load_flags(Flags),
    Root = b2s(get_flag('-root',Flags)),
    PathFls = path_flags(Flags),
    Pgm = b2s(Pgm0),
    _Pid = start_prim_loader(Init,b2a(Id),Pgm,bs2as(Nodes),
			     bs2ss(Path),PathFls),
    BootFile = bootfile(Flags,Root),
    BootList = get_boot(BootFile,Root),
    LoadMode = b2a(get_flag('-mode',Flags,false)),
    Deb = b2a(get_flag('-init_debug',Flags,false)),
    BootVars = get_flag_args('-boot_var',Flags),
    ParallelLoad = 
	(Pgm =:= "efile") and (erlang:system_info(thread_pool_size) > 0),

    PathChoice = code_path_choice(),
    eval_script(BootList,Init,PathFls,{Root,BootVars},Path,
		{true,LoadMode,ParallelLoad},Deb,PathChoice),

    %% To help identifying Purify windows that pop up,
    %% print the node name into the Purify log.
    (catch erlang:system_info({purify, "Node: " ++ atom_to_list(node())})),

    start_em(Start).

bootfile(Flags,Root) ->
    b2s(get_flag('-boot',Flags,concat([Root,"/bin/start"]))).

path_flags(Flags) ->
    Pa = append(reverse(get_flag_args('-pa',Flags))),
    Pz = append(get_flag_args('-pz',Flags)),
    {bs2ss(Pa),bs2ss(Pz)}.

get_boot(BootFile0,Root) ->
    BootFile = concat([BootFile0,".boot"]),
    case get_boot(BootFile) of
	{ok, CmdList} ->
	    CmdList;
	not_found -> %% Check for default.
	    BootF = concat([Root,"/bin/",BootFile]),
	    case get_boot(BootF)  of
		{ok, CmdList} ->
		    CmdList;
		not_found ->
		    exit({'cannot get bootfile',list_to_atom(BootFile)});
		_ ->
		    exit({'bootfile format error',list_to_atom(BootF)})
	    end;
	_ ->
	    exit({'bootfile format error',list_to_atom(BootFile)})
    end.
    
get_boot(BootFile) ->
    case erl_prim_loader:get_file(BootFile) of
	{ok,Bin,_} ->
	    case binary_to_term(Bin) of
		{script,Id,CmdList} when is_list(CmdList) ->
		    init ! {self(),{script_id,Id}}, % ;-)
		    {ok, CmdList};
		_ ->
		    error
	    end;
	_ ->
	    not_found
    end.

%%
%% Eval a boot script.
%% Load modules and start processes.
%% If a start command does not spawn a new process the
%% boot process hangs (we want to ensure syncronicity).
%%

eval_script([{progress,Info}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{progress,Info}),
    init ! {self(),progress,Info},
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{preLoaded,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{path,Path}|CfgL],Init,{Pa,Pz},Vars,false,Ph,Deb,PathChoice) ->
    RealPath0 = make_path(Pa, Pz, Path, Vars),
    RealPath = patch_path(RealPath0, PathChoice),
    erl_prim_loader:set_path(RealPath),
    eval_script(CfgL,Init,{Pa,Pz},Vars,false,Ph,Deb,PathChoice);
eval_script([{path,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    %% Ignore, use the command line -path flag.
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,embedded,Par},Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{true,embedded,Par},Deb,PathChoice);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,E,Par},Deb,PathChoice) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice);
eval_script([{primLoad,Mods}|CfgL],Init,PathFs,Vars,P,{true,E,Par},Deb,PathChoice)
  when is_list(Mods) ->
    if 
	Par =:= true ->
	    par_load_modules(Mods,Init);
	true ->
	    load_modules(Mods)
    end,
    eval_script(CfgL,Init,PathFs,Vars,P,{true,E,Par},Deb,PathChoice);
eval_script([{primLoad,_Mods}|CfgL],Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice) ->
    %% Do not load now, code_server does that dynamically!
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E,Par},Deb,PathChoice);
eval_script([{kernelProcess,Server,{Mod,Fun,Args}}|CfgL],Init,
	    PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{start,Server}),
    start_in_kernel(Server,Mod,Fun,Args,Init),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([{apply,{Mod,Fun,Args}}|CfgL],Init,PathFs,Vars,P,Ph,Deb,PathChoice) ->
    debug(Deb,{apply,{Mod,Fun,Args}}),
    apply(Mod,Fun,Args),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb,PathChoice);
eval_script([],_,_,_,_,_,_,_) ->
    ok;
eval_script(What,_,_,_,_,_,_,_) ->
    exit({'unexpected command in bootfile',What}).

load_modules([Mod|Mods]) ->
    File = concat([Mod,objfile_extension()]),
    {ok,Full} = load_mod(Mod,File),
    init ! {self(),loaded,{Mod,Full}},  %% Tell init about loaded module
    load_modules(Mods);
load_modules([]) ->
    ok.

%%% An optimization: erl_prim_loader gets the chance of loading many
%%% files in parallel, using threads. This will reduce the seek times,
%%% and loaded code can be processed while other threads are waiting
%%% for the disk. The optimization is not tried unless the loader is
%%% "efile" and there is a non-empty pool of threads.
%%%
%%% Many threads are needed to get a good result, so it would be
%%% beneficial to load several applications in parallel. However,
%%% measurements show that the file system handles one directory at a
%%% time, regardless if parallel threads are created for files on
%%% several directories (a guess: writing the meta information when
%%% the file was last read ('mtime'), forces the file system to sync
%%% between directories).

par_load_modules(Mods,Init) ->
    Ext = objfile_extension(),
    ModFiles = [{Mod,concat([Mod,Ext])} || Mod <- Mods,
					   not erlang:module_loaded(Mod)],
    Self = self(),
    Fun = fun(Mod, BinCode, FullName) ->
		  case catch load_mod_code(Mod, BinCode, FullName) of
		      {ok, _} ->
			  Init ! {Self,loaded,{Mod,FullName}},
			  ok;
		      _EXIT ->
			  {error, Mod}
		  end
	  end,
    case erl_prim_loader:get_files(ModFiles, Fun) of
	ok ->
	    ok;
	{error,Mod} ->
	    exit({'cannot load',Mod,get_files})
    end.

make_path(Pa, Pz, Path, Vars) ->
    append([Pa,append([fix_path(Path,Vars),Pz])]).

%% For all Paths starting with $ROOT add rootdir and for those
%% starting with $xxx/, expand $xxx to the value supplied with -boot_var!
%% If $xxx cannot be expanded this process terminates.

fix_path([Path|Ps], Vars) when is_atom(Path) ->
    [add_var(atom_to_list(Path), Vars)|fix_path(Ps, Vars)];
fix_path([Path|Ps], Vars) ->
    [add_var(Path, Vars)|fix_path(Ps, Vars)];
fix_path(_, _) ->
    [].

add_var("$ROOT/" ++ Path, {Root,_}) ->
    concat([Root, "/", Path]);
add_var([$$|Path0], {_,VarList}) ->
    {Var,Path} = extract_var(Path0,[]),
    Value = b2s(get_var_value(list_to_binary(Var),VarList)),
    concat([Value, "/", Path]);
add_var(Path, _)   ->
    Path.

extract_var([$/|Path],Var) -> {reverse(Var),Path};
extract_var([H|T],Var)     -> extract_var(T,[H|Var]);
extract_var([],Var)        -> {reverse(Var),[]}.

%% get_var_value(Var, [Vars]) where Vars == [atom()]
get_var_value(Var,[Vars|VarList]) ->
    case get_var_val(Var,Vars) of
	{ok, Value} ->
	    Value;
	_ ->
	    get_var_value(Var,VarList)
    end;
get_var_value(Var,[]) ->
    exit(list_to_atom(concat(["cannot expand \$", Var, " in bootfile"]))).

get_var_val(Var,[Var,Value|_]) -> {ok, Value};
get_var_val(Var,[_,_|Vars])    -> get_var_val(Var,Vars);
get_var_val(_,_)               -> false.

patch_path(Dirs, strict) ->
    Dirs;
patch_path(Dirs, relaxed) ->
    ArchiveExt = archive_extension(),
    [patch_dir(Dir, ArchiveExt) || Dir <- Dirs].

patch_dir(Orig, ArchiveExt) ->
    case funny_split(Orig, $/) of
	["nibe", RevApp, RevArchive | RevTop] ->
	    App = reverse(RevApp),
	    case funny_splitwith(RevArchive, $.) of
		{Ext, Base} when Ext =:= ArchiveExt, Base =:= App ->
		    %% Orig archive
		    Top = reverse([reverse(C) || C <- RevTop]),
		    Dir = join(Top ++ [App, "ebin"], "/"),
		    Archive = Orig;
		_ ->
		    %% Orig directory
		    Top = reverse([reverse(C) || C <- [RevArchive | RevTop]]),
		    Archive = join(Top ++ [App ++ ArchiveExt, App, "ebin"], "/"),
		    Dir = Orig
	    end,
	    %% First try dir, second try archive and at last use orig if both fails
	    case erl_prim_loader:read_file_info(Dir) of
		{ok, #file_info{type = directory}} ->
		    Dir;
		_ ->
		    case erl_prim_loader:read_file_info(Archive) of
			{ok, #file_info{type = directory}} ->
			    Archive;
			_ ->
			    Orig
		    end
	    end;
	_ ->
	    Orig
    end.

%% Returns all lists in reverse order
funny_split(List, Sep) ->
   funny_split(List, Sep, [], []).

funny_split([Sep | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [], [Path | Paths]);
funny_split([Head | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [Head | Path], Paths);
funny_split([], _Sep, Path, Paths) ->
    [Path | Paths].

%% Returns {BeforeSep, AfterSep} where BeforeSep is in reverse order
funny_splitwith(List, Sep) ->
    funny_splitwith(List, Sep, [], List).

funny_splitwith([Sep | Tail], Sep, Acc, _Orig) ->
    {Acc, Tail};
funny_splitwith([Head | Tail], Sep, Acc, Orig) ->
    funny_splitwith(Tail, Sep, [Head | Acc], Orig);
funny_splitwith([], _Sep, _Acc, Orig) ->
    {[], Orig}.

-spec join([string()], string()) -> string().
join([H1, H2 | T], S) ->
    H1 ++ S ++ join([H2 | T], S);
join([H], _) ->
    H.

%% Servers that are located in the init kernel are linked
%% and supervised by init.

start_in_kernel(Server,Mod,Fun,Args,Init) ->
    Res = apply(Mod,Fun,Args),
    Init ! {self(),started,{Server,Res}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),  %% Just for sure...
	    ok;
	{Init,ignore} ->
	    ignore
    end.

%% Do start all processes specified at command line using -s!
%% Use apply here instead of spawn to ensure syncronicity for
%% those servers that wish to have it so.
%% Disadvantage: anything started with -s that does not
%% eventually spawn will hang the startup routine.

%% We also handle -eval here. The argument is an arbitrary
%% expression that should be parsed and evaluated.

start_em([S|Tail]) ->
    case whereis(user) of
	undefined -> 
	    ok;
	P when is_pid(P) ->			%Let's set the group_leader()
	    erlang:group_leader(P, self())
    end,
    start_it(S),
    start_em(Tail);
start_em([]) -> ok.

start_it([]) ->
    ok;
start_it({eval,Bin}) ->
    Str = binary_to_list(Bin),
    {ok,Ts,_} = erl_scan:string(Str),
    Ts1 = case reverse(Ts) of
	      [{dot,_}|_] -> Ts;
	      TsR -> reverse([{dot,1} | TsR])
	  end,
    {ok,Expr} = erl_parse:parse_exprs(Ts1),
    erl_eval:exprs(Expr, []),
    ok;
start_it([_|_]=MFA) ->
    Ref = make_ref(),
    case catch {Ref,case MFA of
			[M]        -> M:start();
			[M,F]      -> M:F();
			[M,F|Args] -> M:F(Args)	% Args is a list
		    end} of
	{Ref,R} ->
	    R;
	{'EXIT',Reason} ->
	    exit(Reason);
	Other ->
	    throw(Other)
    end.

%%
%% Fetch a module and load it into the system.
%%
load_mod(Mod, File) ->
    case erlang:module_loaded(Mod) of
	false ->
	    case erl_prim_loader:get_file(File) of
		{ok,BinCode,FullName} ->
		    load_mod_code(Mod, BinCode, FullName);
		_ ->
		    exit({'cannot load',Mod,get_file})
	    end;
	_ -> % Already loaded.
	    {ok,File}
    end.

load_mod_code(Mod, BinCode, FullName) ->
    case erlang:module_loaded(Mod) of
	false ->
	    case erlang:load_module(Mod, BinCode) of
		{module,Mod} -> {ok,FullName};
		{error,on_load} ->
		    ?ON_LOAD_HANDLER ! {loaded,Mod},
		    {ok,FullName};
		Other ->
		    exit({'cannot load',Mod,Other})
	    end;
	_ -> % Already loaded.
	    {ok,FullName}
    end.

%% --------------------------------------------------------
%% If -shutdown_time is specified at the command line
%% this timer will inform the init process that it has to
%% force processes to terminate. It cannot be handled
%% softly any longer.
%% --------------------------------------------------------

shutdown_timer(Flags) ->
    case get_flag('-shutdown_time',Flags,infinity) of
	infinity ->
	    self();
	Time ->
	    case catch list_to_integer(binary_to_list(Time)) of
		T when is_integer(T) ->
		    Pid = spawn(fun() -> timer(T) end),
		    receive
			{Pid, started} ->
			    Pid
		    end;
		_ ->
		    self()
	    end
    end.
    
flush_timout(Pid) ->
    receive
	{Pid, timeout} -> true
    after 0            -> true
    end.

timer(T) ->
    init ! {self(), started},
    receive
    after T ->
	init ! {self(), timeout}
    end.
    
%% --------------------------------------------------------
%% Parse the command line arguments and extract things to start, flags
%% and other arguments. We keep the relative of the groups.
%% --------------------------------------------------------

parse_boot_args(Args) ->
    parse_boot_args(Args, [], [], []).

parse_boot_args([B|Bs], Ss, Fs, As) ->
    case check(B) of
	start_extra_arg ->
	    {reverse(Ss),reverse(Fs),lists:reverse(As, Bs)}; % BIF
	start_arg ->
	    {S,Rest} = get_args(Bs, []),
	    parse_boot_args(Rest, [{s, S}|Ss], Fs, As);
	start_arg2 ->
	    {S,Rest} = get_args(Bs, []),
	    parse_boot_args(Rest, [{run, S}|Ss], Fs, As);
	eval_arg ->
	    {Expr,Rest} = get_args(Bs, []),
	    parse_boot_args(Rest, [{eval, Expr}|Ss], Fs, As);
	flag ->
	    {F,Rest} = get_args(Bs, []),
	    Fl = case F of
		     []   -> [B];
		     FF   -> [B,FF]
		 end,
	    parse_boot_args(Rest, Ss,  
			    [list_to_tuple(Fl)|Fs], As);
	arg ->
	    parse_boot_args(Bs, Ss, Fs, [B|As]);
	end_args ->
	    parse_boot_args(Bs, Ss, Fs, As)
    end;
parse_boot_args([], Start, Flags, Args) ->
    {reverse(Start),reverse(Flags),reverse(Args)}.

check(<<"-extra">>) -> start_extra_arg;
check(<<"-s">>) -> start_arg;
check(<<"-run">>) -> start_arg2;
check(<<"-eval">>) -> eval_arg;
check(<<"--">>) -> end_args;
check(X) when is_binary(X) ->
    case binary_to_list(X) of
	[$-|_Rest] -> flag;
	_Chars     -> arg			%Even empty atoms
    end;
check(_X) -> arg.				%This should never occur

get_args([B|Bs], As) ->
    case check(B) of
	start_extra_arg -> {reverse(As), [B|Bs]};
	start_arg -> {reverse(As), [B|Bs]};
	start_arg2 -> {reverse(As), [B|Bs]};
	eval_arg -> {reverse(As), [B|Bs]};
	end_args -> {reverse(As), Bs};
	flag -> {reverse(As), [B|Bs]};
	arg ->
	    get_args(Bs, [B|As])
    end;
get_args([], As) -> {reverse(As),[]}.

%%
%% Internal get_flag function, with default value.
%% Return: true if flag given without args
%%         atom() if a single arg was given.
%%         list(atom()) if several args were given.
%%
get_flag(F,Flags,Default) ->
    case catch get_flag(F,Flags) of
	{'EXIT',_} ->
	    Default;
	Value ->
	    Value
    end.

get_flag(F,Flags) ->
    case search(F,Flags) of
	{value,{F,[V]}} ->
	    V;
	{value,{F,V}} ->
	    V;
	{value,{F}} -> % Flag given!
	    true;
	_ ->
	    exit(list_to_atom(concat(["no ",F," flag"])))
    end.

%%
%% Internal get_flag function, with default value.
%% Return: list(atom()) 
%%
get_flag_list(F,Flags,Default) ->
    case catch get_flag_list(F,Flags) of
	{'EXIT',_} ->
	    Default;
	Value ->
	    Value
    end.

get_flag_list(F,Flags) ->
    case search(F,Flags) of
	{value,{F,V}} ->
	    V;
	_ ->
	    exit(list_to_atom(concat(["no ",F," flag"])))
    end.

%%
%% Internal get_flag function.
%% Fetch all occurrences of flag.
%% Return: [Args,Args,...] where Args ::= list(atom())
%%
get_flag_args(F,Flags) -> get_flag_args(F,Flags,[]).

get_flag_args(F,[{F,V}|Flags],Acc) when is_list(V) ->
    get_flag_args(F,Flags,[V|Acc]);
get_flag_args(F,[{F,V}|Flags],Acc) ->
    get_flag_args(F,Flags,[[V]|Acc]);
get_flag_args(F,[_|Flags],Acc) ->
    get_flag_args(F,Flags,Acc);
get_flag_args(_,[],Acc) ->
    reverse(Acc).

get_arguments([{F,V}|Flags]) ->
    [$-|Fl] = atom_to_list(F),
    [{list_to_atom(Fl),to_strings(V)}|get_arguments(Flags)];
get_arguments([{F}|Flags]) ->
    [$-|Fl] = atom_to_list(F),
    [{list_to_atom(Fl),[]}|get_arguments(Flags)];
get_arguments([]) ->
    [].

to_strings([H|T]) when is_atom(H) -> [atom_to_list(H)|to_strings(T)];
to_strings([H|T]) when is_binary(H) -> [binary_to_list(H)|to_strings(T)];
to_strings([])    -> [].

get_argument(Arg,Flags) ->
    Args = get_arguments(Flags),
    case get_argument1(Arg,Args) of
	[] ->
	    error;
	Value ->
	    {ok,Value}
    end.

get_argument1(Arg,[{Arg,V}|Args]) ->
    [V|get_argument1(Arg,Args)];
get_argument1(Arg,[_|Args]) ->
    get_argument1(Arg,Args);
get_argument1(_,[]) ->
    [].

set_argument([{Flag,_}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([{Flag}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([Item|Flags],Flag,Value) ->
    [Item|set_argument(Flags,Flag,Value)];
set_argument([],Flag,Value) ->
    [{Flag,[Value]}].

concat([A|T]) when is_atom(A) ->
    atom_to_list(A) ++ concat(T);
concat([C|T]) when is_integer(C), 0 =< C, C =< 255 ->
    [C|concat(T)];
concat([Bin|T]) when is_binary(Bin) ->
    binary_to_list(Bin) ++ concat(T);
concat([S|T]) ->
    S ++ concat(T);
concat([]) ->
    [].

append(L, Z) -> L ++ Z.

append([E]) -> E;
append([H|T]) ->
    H ++ append(T);
append([]) -> [].

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]). % BIF
			
search(Key, [H|_T]) when is_tuple(H), element(1, H) =:= Key ->
    {value, H};
search(Key, [_|T]) ->
    search(Key, T);
search(_Key, []) ->
    false.

-spec objfile_extension() -> nonempty_string().
objfile_extension() ->
    ".beam".
%%    case erlang:system_info(machine) of
%%      "JAM" -> ".jam";
%%      "VEE" -> ".vee";
%%      "BEAM" -> ".beam"
%%    end.

-spec archive_extension() -> nonempty_string().
archive_extension() ->
    ".ez".

%%%
%%% Support for handling of on_load functions.
%%%

start_on_load_handler_process() ->
    register(?ON_LOAD_HANDLER,
	     spawn_link(fun on_load_handler_init/0)).

on_load_handler_init() ->
    on_load_loop([]).

on_load_loop(Mods) ->
    receive
	{loaded,Mod} ->
	    on_load_loop([Mod|Mods]);
	run_on_load ->
	    run_on_load_handlers(Mods),
	    exit(on_load_done)
    end.

run_on_load_handlers([M|Ms]) ->
    Fun = fun() ->
		  Res = erlang:call_on_load_function(M),
		  exit(Res)
	  end,
    {Pid,Ref} = spawn_monitor(Fun),
    receive
	{'DOWN',Ref,process,Pid,OnLoadRes} ->
	    Keep = OnLoadRes =:= ok,
	    erlang:finish_after_on_load(M, Keep),
	    case Keep of
		false ->
		    exit({on_load_function_failed,M});
		true ->
		    run_on_load_handlers(Ms)
	    end
    end;
run_on_load_handlers([]) -> ok.
