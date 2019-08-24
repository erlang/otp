%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%%
%%           New initial version of init.
%% Booting from a script. The script is fetched either from
%% a local file or distributed from another erlang node.
%% 
%% Flags:
%%        -boot File     : Absolute file name of the boot script.
%%        -boot_var Var Value
%%                       : $Var in the boot script is expanded to
%%                         Value.
%%        -loader LoaderMethod
%%                       : efile, inet
%%                         (Optional - default efile)
%%        -hosts [Node]  : List of hosts from which we can boot.
%%                         (Mandatory if -loader inet)
%%        -mode interactive : Auto load modules not needed at startup (default system behaviour).
%%        -mode embedded : Load all modules in the boot script, disable auto loading.
%%        -path          : Override path in bootfile.
%%        -pa Path+      : Add my own paths first.
%%        -pz Path+      : Add my own paths last.
%%        -run           : Start own processes.
%%        -s             : Start own processes.
%% 
%% Experimental flags:
%%        -profile_boot    : Use an 'eprof light' to profile boot sequence
%%        -init_debug      : Activate debug printouts in init
%%        -loader_debug    : Activate debug printouts in erl_prim_loader
%%        -code_path_choice : strict | relaxed

-module(init).

-export([restart/0,reboot/0,stop/0,stop/1,
	 get_status/0,boot/1,get_arguments/0,get_plain_arguments/0,
	 get_argument/1,script_id/0]).

%% for the on_load functionality; not for general use
-export([run_on_load_handlers/0]).

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
-type state() :: #state{}.

%% Data for eval_script/2.
-record(es,
	{init,
	 debug,
	 path,
	 pa,
	 pz,
	 path_choice,
	 prim_load,
	 load_mode,
	 vars
	}).

-define(ON_LOAD_HANDLER, init__boot__on_load_handler).


debug(false, _) -> ok;
debug(_, T)     -> erlang:display(T).

-spec get_arguments() -> Flags when
      Flags :: [{Flag :: atom(), Values :: [string()]}].
get_arguments() ->
    request(get_arguments).

-spec get_plain_arguments() -> [Arg] when
      Arg :: string().
get_plain_arguments() ->
    bs2ss(request(get_plain_arguments)).

-spec get_argument(Flag) -> {'ok', Arg} | 'error' when
      Flag :: atom(),
      Arg :: [Values :: [string()]].
get_argument(Arg) ->
    request({get_argument, Arg}).

-spec script_id() -> Id when
      Id :: term().
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

-spec get_status() -> {InternalStatus, ProvidedStatus} when
      InternalStatus :: internal_status(),
      ProvidedStatus :: term().
get_status() ->
    request(get_status).

-spec fetch_loaded() -> [{module(),file:filename()}].
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

-spec stop() -> 'ok'.
stop() -> init ! {stop,stop}, ok.

-spec stop(Status) -> 'ok' when
      Status :: non_neg_integer() | string().
stop(Status) when is_integer(Status), Status >= 0 ->
    stop_1(Status);
stop(Status) when is_list(Status) ->
    case is_bytelist(Status) of
        true ->
            stop_1(Status);
        false ->
            erlang:error(badarg)
    end;
stop(_) ->
    erlang:error(badarg).

is_bytelist([B|Bs]) when is_integer(B), B >= 0, B < 256 -> is_bytelist(Bs);
is_bytelist([]) -> true;
is_bytelist(_) -> false.

%% Note that we check the type of Status beforehand to ensure that
%% the call to halt(Status) by the init process cannot fail
stop_1(Status) -> init ! {stop,{stop,Status}}, ok.

-spec boot(BootArgs) -> no_return() when
      BootArgs :: [binary()].
boot(BootArgs) ->
    register(init, self()),
    process_flag(trap_exit, true),

    {Start0,Flags,Args} = parse_boot_args(BootArgs),
    %% We don't get to profile parsing of BootArgs
    case b2a(get_flag(profile_boot, Flags, false)) of
        false -> ok;
        true  -> debug_profile_start()
    end,
    Start = map(fun prepare_run_args/1, Start0),
    boot(Start, Flags, Args).

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
    list_to_atom(b2s(Bin));
b2a(A) when is_atom(A) ->
    A.

b2s(Bin) when is_binary(Bin) ->
    try
	unicode:characters_to_list(Bin,file:native_name_encoding())
    catch
	_:_ -> binary_to_list(Bin)
    end;
b2s(L) when is_list(L) ->
    L.

map(_F, []) ->
    [];
map(F, [X|Rest]) ->
    [F(X) | map(F, Rest)].

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
    start_on_load_handler_process(),
    BootPid = do_boot(Flags,Start),
    State = #state{flags = Flags,
		   args = Args,
		   start = Start,
		   bootpid = BootPid},
    boot_loop(BootPid,State).

%%% Convert a term to a printable string, if possible.
to_string(X, D) when is_list(X), D < 4 ->			% assume string
    F = flatten(X, []),
    case printable_list(F) of
	true when length(F) > 0 ->  F;
	_false ->
            List = [to_string(E, D+1) || E <- X],
            flatten(["[",join(List),"]"], [])
    end;
to_string(X, _D) when is_list(X) ->
    "[_]";
to_string(X, _D) when is_atom(X) ->
    atom_to_list(X);
to_string(X, _D) when is_pid(X) ->
    pid_to_list(X);
to_string(X, _D) when is_float(X) ->
    float_to_list(X);
to_string(X, _D) when is_integer(X) ->
    integer_to_list(X);
to_string(X, D) when is_tuple(X), D < 4 ->
    List = [to_string(E, D+1) || E <- tuple_to_list(X)],
    flatten(["{",join(List),"}"], []);
to_string(X, _D) when is_tuple(X) ->
    "{_}";
to_string(_X, _D) ->
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

join([] = T) ->
    T;
join([_Elem] = T) ->
    T;
join([Elem|T]) ->
    [Elem,","|join(T)].

flatten([H|T], Tail) when is_list(H) ->
    flatten(H, flatten(T, Tail));
flatten([H|T], Tail) ->
    [H|flatten(T, Tail)];
flatten([], Tail) ->
    Tail.

things_to_string([X|Rest]) ->
    " (" ++ to_string(X, 0) ++ ")" ++ things_to_string(Rest);
things_to_string([]) ->
    "".

halt_string(String, List) ->
    String ++ things_to_string(List).

%% String = string()
%% List = [string() | atom() | pid() | number() | list() | tuple()]
%% Items in List are truncated if found to be too large
-spec crash(_, _) -> no_return().
crash(String, List) ->
    halt(halt_string(String, List)).

%% Status is {InternalStatus,ProvidedStatus}
-spec boot_loop(pid(), state()) -> no_return().
boot_loop(BootPid, State) ->
    receive
	{BootPid,loaded,NewlyLoaded} ->
	    Loaded = NewlyLoaded ++ State#state.loaded,
	    boot_loop(BootPid, State#state{loaded = Loaded});
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
    case erlang:module_loaded(Module) of
	true ->
	    {{module, Module}, Loaded};
	false ->
	    do_ensure_loaded(Module, Loaded)
    end.

do_ensure_loaded(Module, Loaded) ->
    File = atom_to_list(Module) ++ objfile_extension(),
    case erl_prim_loader:get_file(File) of
	{ok,BinCode,FullName} ->
	    case do_load_module(Module, BinCode) of
		ok ->
		    {{module, Module}, [{Module, FullName}|Loaded]};
		error ->
		    {error, [{Module, FullName}|Loaded]}
	    end;
	Error ->
	    {Error, Loaded}
    end.

%% Tell subscribed processes the system has started.
notify(Pids) ->
    lists:foreach(fun(Pid) -> Pid ! {init,started} end, Pids).

%% Garbage collect all info about initially loaded modules.
%% This information is temporarily stored until the code_server
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
	{From, {ensure_loaded, _}} ->
	    From ! {init, not_allowed};
	X ->
            %% This is equal to calling logger:info/3 which we don't
            %% want to do from the init process, at least not during
            %% system boot. We don't want to call logger:timestamp()
            %% either.
	    case whereis(user) of
		undefined ->
                    catch logger ! {log, info, "init got unexpected: ~p", [X],
                                    #{pid=>self(),
                                      gl=>self(),
                                      time=>os:system_time(microsecond),
                                      error_logger=>#{tag=>info_msg}}};
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
    case set_flag(boot, Boot, Flags0) of
	{ok,Flags1} ->
	    case set_flag(config, Config, Flags1) of
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
    %% The flag here can be -boot or -config, which means the value is
    %% a file name! Thus the file name encoding is used when coverting.
    Encoding = file:native_name_encoding(),
    case catch unicode:characters_to_binary(Value,Encoding,Encoding) of
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
    %% Make sure we don't have any outstanding messages before doing the restart.
    flush(),
    erts_internal:erase_persistent_terms(),
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
    Logger = get_logger(State#state.kernel),
    shutdown_pids(Heart,Logger,BootPid,State),
    unload(Heart),
    kill_em([Logger]),
    do_unload([logger_server]).

flush() ->
    receive
        _M -> flush()
    after 0 ->
            ok
    end.

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

shutdown_pids(Heart,Logger,BootPid,State) ->
    Timer = shutdown_timer(State#state.flags),
    catch shutdown(State#state.kernel,BootPid,Timer,State),
    kill_all_pids(Heart,Logger), % Even the shutdown timer.
    kill_all_ports(Heart), % Logger has no ports
    flush_timout(Timer).

get_heart(Kernel) ->
    get_kernelpid(heart,Kernel).

get_logger(Kernel) ->
    get_kernelpid(logger,Kernel).

get_kernelpid(Name,[{Name,Pid}|_Kernel]) -> Pid;
get_kernelpid(Name,[_|Kernel])           -> get_kernelpid(Name,Kernel);
get_kernelpid(_,_)                    -> false.


shutdown([{Except,_Pid}|Kernel],BootPid,Timer,State)
  when Except==heart; Except==logger ->
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
kill_all_pids(Heart,Logger) ->
    case get_pids(Heart,Logger) of
	[] ->
	    ok;
	Pids ->
	    kill_em(Pids),
	    kill_all_pids(Heart,Logger)  % Continue until all are really killed.
    end.
    
%% All except system processes.
get_pids(Heart,Logger) ->
    Pids = [P || P <- processes(), not erts_internal:is_system_process(P)],
    delete(Heart,Logger,self(),Pids).

delete(Heart,Logger,Init,[Heart|Pids]) -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Logger|Pids])  -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Init|Pids])  -> delete(Heart,Logger,Init,Pids);
delete(Heart,Logger,Init,[Pid|Pids])   -> [Pid|delete(Heart,Logger,Init,Pids)];
delete(_,_,_,[])                  -> [].
    
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
    do_unload(sub([logger_server|erlang:pre_loaded()],erlang:loaded()));
unload(_) ->
    do_unload(sub([heart,logger_server|erlang:pre_loaded()],erlang:loaded())).

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

start_prim_loader(Init, Path0, {Pa,Pz}) ->
    Path = case Path0 of
	       false -> Pa ++ ["."|Pz];
	       _ -> Path0
	   end,
    case erl_prim_loader:start() of
	{ok,Pid} ->
	    erl_prim_loader:set_path(Path),
	    add_to_kernel(Init, Pid);
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
    Root = get_root(Flags),
    Path = get_flag_list(path, Flags, false),
    {Pa,Pz} = PathFls = path_flags(Flags),
    start_prim_loader(Init, bs2ss(Path), PathFls),
    BootFile = bootfile(Flags,Root),
    BootList = get_boot(BootFile,Root),
    LoadMode = b2a(get_flag(mode, Flags, false)),
    Deb = b2a(get_flag(init_debug, Flags, false)),
    catch ?ON_LOAD_HANDLER ! {init_debug_flag,Deb},
    BootVars = get_boot_vars(Root, Flags),

    PathChoice = code_path_choice(),
    Es = #es{init=Init,debug=Deb,path=Path,pa=Pa,pz=Pz,
	     path_choice=PathChoice,
	     prim_load=true,load_mode=LoadMode,
	     vars=BootVars},
    eval_script(BootList, Es),

    %% To help identifying Purify windows that pop up,
    %% print the node name into the Purify log.
    (catch erlang:system_info({purify, "Node: " ++ atom_to_list(node())})),

    start_em(Start),
    case b2a(get_flag(profile_boot,Flags,false)) of
        false -> ok;
        true  ->
            debug_profile_format_mfas(debug_profile_mfas()),
            debug_profile_stop()
    end,
    ok.

get_root(Flags) ->
    case get_argument(root, Flags) of
	{ok,[[Root]]} ->
	    Root;
	_ ->
	    exit(no_or_multiple_root_variables)
    end.

get_boot_vars(Root, Flags) ->
    BootVars = get_boot_vars_1(#{}, Flags),
    RootKey = <<"ROOT">>,
    BootVars#{RootKey=>Root}.

get_boot_vars_1(Vars, [{boot_var,[Key,Value]}|T]) ->
    get_boot_vars_1(Vars#{Key=>Value}, T);
get_boot_vars_1(_, [{boot_var,_}|_]) ->
    exit(invalid_boot_var_argument);
get_boot_vars_1(Vars, [_|T]) ->
    get_boot_vars_1(Vars, T);
get_boot_vars_1(Vars, []) ->
    Vars.

bootfile(Flags,Root) ->
    b2s(get_flag(boot, Flags, Root++"/bin/start")).

path_flags(Flags) ->
    Pa = append(reverse(get_flag_args(pa, Flags))),
    Pz = append(get_flag_args(pz, Flags)),
    {bs2ss(Pa),bs2ss(Pz)}.

get_boot(BootFile0,Root) ->
    BootFile = BootFile0 ++ ".boot",
    case get_boot(BootFile) of
	{ok, CmdList} ->
	    CmdList;
	not_found -> %% Check for default.
	    BootF = Root ++ "/bin/" ++ BootFile,
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

eval_script([{progress,Info}=Progress|T], #es{debug=Deb}=Es) ->
    debug(Deb, Progress),
    init ! {self(),progress,Info},
    eval_script(T, Es);
eval_script([{preLoaded,_}|T], #es{}=Es) ->
    eval_script(T, Es);
eval_script([{path,Path}|T], #es{path=false,pa=Pa,pz=Pz,
				 path_choice=PathChoice,
				 vars=Vars}=Es) ->
    RealPath0 = make_path(Pa, Pz, Path, Vars),
    RealPath = patch_path(RealPath0, PathChoice),
    erl_prim_loader:set_path(RealPath),
    eval_script(T, Es);
eval_script([{path,_}|T], #es{}=Es) ->
    %% Ignore, use the command line -path flag.
    eval_script(T, Es);
eval_script([{kernel_load_completed}|T], #es{load_mode=Mode}=Es0) ->
    Es = case Mode of
	     embedded -> Es0;
	     _ -> Es0#es{prim_load=false}
	 end,
    eval_script(T, Es);
eval_script([{primLoad,Mods}|T], #es{init=Init,prim_load=PrimLoad}=Es)
  when is_list(Mods) ->
    case PrimLoad of
	true ->
	    load_modules(Mods, Init);
	false ->
	    %% Do not load now, code_server does that dynamically!
	    ok
    end,
    eval_script(T, Es);
eval_script([{kernelProcess,Server,{Mod,Fun,Args}}|T],
	    #es{init=Init,debug=Deb}=Es) ->
    debug(Deb, {start,Server}),
    start_in_kernel(Server, Mod, Fun, Args, Init),
    eval_script(T, Es);
eval_script([{apply,{Mod,Fun,Args}}=Apply|T], #es{debug=Deb}=Es) ->
    debug(Deb, Apply),
    apply(Mod, Fun, Args),
    eval_script(T, Es);
eval_script([], #es{}) ->
    ok;
eval_script(What, #es{}) ->
    exit({'unexpected command in bootfile',What}).

load_modules(Mods0, Init) ->
    Mods = [M || M <- Mods0, not erlang:module_loaded(M)],
    F = prepare_loading_fun(),
    case erl_prim_loader:get_modules(Mods, F) of
	{ok,{Prep0,[]}} ->
	    Prep = [Code || {_,{prepared,Code,_}} <- Prep0],
	    ok = erlang:finish_loading(Prep),
	    Loaded = [{Mod,Full} || {Mod,{_,_,Full}} <- Prep0],
	    Init ! {self(),loaded,Loaded},
	    Beams = [{M,Beam,Full} || {M,{on_load,Beam,Full}} <- Prep0],
	    load_rest(Beams, Init);
	{ok,{_,[_|_]=Errors}} ->
	    Ms = [M || {M,_} <- Errors],
	    exit({load_failed,Ms})
    end.

load_rest([{Mod,Beam,Full}|T], Init) ->
    do_load_module(Mod, Beam),
    Init ! {self(),loaded,[{Mod,Full}]},
    load_rest(T, Init);
load_rest([], _) ->
    ok.

prepare_loading_fun() ->
    fun(Mod, FullName, Beam) ->
	    case erlang:prepare_loading(Mod, Beam) of
		{error,_}=Error ->
		    Error;
		Prepared ->
		    case erlang:has_prepared_code_on_load(Prepared) of
			true ->
			    {ok,{on_load,Beam,FullName}};
			false ->
			    {ok,{prepared,Prepared,FullName}}
		    end
	    end
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

add_var("$"++Path0, Vars) ->
    {Var,Path} = extract_var(Path0, []),
    Key = list_to_binary(Var),
    case Vars of
	#{Key:=Value0} ->
	    Value = b2s(Value0),
	    Value ++ "/" ++ Path;
	_ ->
	    Error0 = "cannot expand $" ++ Var ++ " in bootfile",
	    Error = list_to_atom(Error0),
	    exit(Error)
    end;
add_var(Path, _) ->
    Path.

extract_var([$/|Path],Var) -> {reverse(Var),Path};
extract_var([H|T],Var)     -> extract_var(T,[H|Var]);
extract_var([],Var)        -> {reverse(Var),[]}.

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
    Str = b2s(Bin),
    {ok,Ts,_} = erl_scan:string(Str),
    Ts1 = case reverse(Ts) of
	      [{dot,_}|_] -> Ts;
	      TsR -> reverse([{dot,erl_anno:new(1)} | TsR])
	  end,
    {ok,Expr} = erl_parse:parse_exprs(Ts1),
    {value, _Value, _Bs} = erl_eval:exprs(Expr, erl_eval:new_bindings()),
    ok;
start_it([_|_]=MFA) ->
    case MFA of
	[M]        -> M:start();
	[M,F]      -> M:F();
	[M,F|Args] -> M:F(Args)	% Args is a list
    end.

%% Load a module.

do_load_module(Mod, BinCode) ->
    case erlang:load_module(Mod, BinCode) of
	{module,Mod} ->
	    ok;
	{error,on_load} ->
	    ?ON_LOAD_HANDLER ! {loaded,Mod},
	    ok;
	_ ->
	    error
    end.

%% --------------------------------------------------------
%% If -shutdown_time is specified at the command line
%% this timer will inform the init process that it has to
%% force processes to terminate. It cannot be handled
%% softly any longer.
%% --------------------------------------------------------

shutdown_timer(Flags) ->
    case get_flag(shutdown_time, Flags, infinity) of
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
	{flag,A} ->
	    {F,Rest} = get_args(Bs, []),
	    Fl = {A,F},
	    parse_boot_args(Rest, Ss, [Fl|Fs], As);
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
check(<<"-",Flag/binary>>) -> {flag,b2a(Flag)};
check(_) -> arg.

get_args([B|Bs], As) ->
    case check(B) of
	start_extra_arg -> {reverse(As), [B|Bs]};
	start_arg -> {reverse(As), [B|Bs]};
	start_arg2 -> {reverse(As), [B|Bs]};
	eval_arg -> {reverse(As), [B|Bs]};
	end_args -> {reverse(As), Bs};
	{flag,_} -> {reverse(As), [B|Bs]};
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
get_flag(F, Flags, Default) ->
    case lists:keyfind(F, 1, Flags) of
	{F,[]} ->
	    true;
	{F,[V]} ->
	    V;
	{F,V} ->
	    V;
	_ ->
	    Default
    end.

%%
%% Internal get_flag function, with default value.
%% Return: list(atom()) 
%%
get_flag_list(F, Flags, Default) ->
    case lists:keyfind(F, 1, Flags) of
	{F,[_|_]=V} ->
	    V;
	_ ->
	    Default
    end.

%%
%% Internal get_flag function.
%% Fetch all occurrences of flag.
%% Return: [Args,Args,...] where Args ::= list(atom())
%%
get_flag_args(F,Flags) -> get_flag_args(F,Flags,[]).

get_flag_args(F,[{F,V}|Flags],Acc) ->
    get_flag_args(F,Flags,[V|Acc]);
get_flag_args(F,[_|Flags],Acc) ->
    get_flag_args(F,Flags,Acc);
get_flag_args(_,[],Acc) ->
    reverse(Acc).

get_arguments([{F,V}|Flags]) ->
    [{F,to_strings(V)}|get_arguments(Flags)];
get_arguments([]) ->
    [].

to_strings([H|T]) when is_atom(H) -> [atom_to_list(H)|to_strings(T)];
to_strings([H|T]) when is_binary(H) -> [b2s(H)|to_strings(T)];
to_strings([])    -> [].

get_argument(Arg, Flags) ->
    case get_argument1(Arg, Flags) of
	[] -> error;
	Value -> {ok,Value}
    end.

get_argument1(Arg, [{Arg,V}|Args]) ->
    [to_strings(V)|get_argument1(Arg, Args)];
get_argument1(Arg, [_|Args]) ->
    get_argument1(Arg, Args);
get_argument1(_, []) ->
    [].

set_argument([{Flag,_}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([Item|Flags],Flag,Value) ->
    [Item|set_argument(Flags,Flag,Value)];
set_argument([],Flag,Value) ->
    [{Flag,[Value]}].

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

run_on_load_handlers() ->
    Ref = monitor(process, ?ON_LOAD_HANDLER),
    catch ?ON_LOAD_HANDLER ! run_on_load,
    receive
	{'DOWN',Ref,process,_,noproc} ->
	    %% There is no on_load handler process,
	    %% probably because init:restart/0 has been
	    %% called and it is not the first time we
	    %% pass through here.
	    ok;
	{'DOWN',Ref,process,_,on_load_done} ->
	    ok;
	{'DOWN',Ref,process,_,Res} ->
	    %% Failure to run an on_load handler.
	    %% This is fatal during start-up.
	    exit(Res)
    end.

start_on_load_handler_process() ->
    register(?ON_LOAD_HANDLER,
	     spawn(fun on_load_handler_init/0)).

on_load_handler_init() ->
    on_load_loop([], false).

on_load_loop(Mods, Debug0) ->
    receive
	{init_debug_flag,Debug} ->
	    on_load_loop(Mods, Debug);
	{loaded,Mod} ->
	    on_load_loop([Mod|Mods], Debug0);
	run_on_load ->
	    run_on_load_handlers(Mods, Debug0),
	    exit(on_load_done)
    end.

run_on_load_handlers([M|Ms], Debug) ->
    debug(Debug, {running_on_load_handler,M}),
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
		    Error = {on_load_function_failed,M},
		    debug(Debug, Error),
		    exit(Error);
		true ->
		    debug(Debug, {on_load_handler_returned_ok,M}),
		    run_on_load_handlers(Ms, Debug)
	    end
    end;
run_on_load_handlers([], _) -> ok.


%% debug profile (light variant of eprof)
debug_profile_start() ->
    _ = erlang:trace_pattern({'_','_','_'},true,[call_time]),
    _ = erlang:trace_pattern(on_load,true,[call_time]),
    _ = erlang:trace(all,true,[call]),
    ok.

debug_profile_stop() ->
    _ = erlang:trace_pattern({'_','_','_'},false,[call_time]),
    _ = erlang:trace_pattern(on_load,false,[call_time]),
    _ = erlang:trace(all,false,[call]),
    ok.

debug_profile_mfas() ->
    _ = erlang:trace_pattern({'_','_','_'},pause,[call_time]),
    _ = erlang:trace_pattern(on_load,pause,[call_time]),
    MFAs = collect_loaded_mfas() ++ erlang:system_info(snifs),
    collect_mfas(MFAs,[]).

%% debug_profile_format_mfas should be called at the end of the boot phase
%% so all pertinent modules should be loaded at that point.
debug_profile_format_mfas(MFAs0) ->
    MFAs = lists:sort(MFAs0),
    lists:foreach(fun({{Us,C},{M,F,A}}) ->
                          Str = io_lib:format("~w:~w/~w", [M,F,A]),
                          io:format(standard_error,"~55s - ~6w : ~w us~n", [Str,C,Us])
                  end, MFAs),
    ok.

collect_loaded_mfas() ->
    Ms = [M || M <- [element(1, Mi) || Mi <- code:all_loaded()]],
    collect_loaded_mfas(Ms,[]).

collect_loaded_mfas([],MFAs) -> MFAs;
collect_loaded_mfas([M|Ms],MFAs0) ->
    MFAs = [{M,F,A} || {F,A} <- M:module_info(functions)],
    collect_loaded_mfas(Ms,MFAs ++ MFAs0).


collect_mfas([], Info) -> Info;
collect_mfas([MFA|MFAs],Info) ->
    case erlang:trace_info(MFA,call_time) of
        {call_time, []} ->
            collect_mfas(MFAs,Info);
        {call_time, false} ->
            collect_mfas(MFAs,Info);
        {call_time, Data} ->
            case collect_mfa(MFA,Data,0,0) of
                {{0,_},_} ->
                    %% ignore mfas with zero time
                    collect_mfas(MFAs,Info);
                MfaData ->
                    collect_mfas(MFAs,[MfaData|Info])
            end
    end.

collect_mfa(Mfa,[],Count,Time) -> {{Time,Count},Mfa};
collect_mfa(Mfa,[{_Pid,C,S,Us}|Data],Count,Time) ->
    collect_mfa(Mfa,Data,Count + C,Time + S * 1000000 + Us).
