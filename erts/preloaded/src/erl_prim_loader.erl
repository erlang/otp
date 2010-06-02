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

%% A primary loader, provides two different methods to fetch a file:
%% efile and inet. The efile method is simple communication with a
%% port program.
%%
%% The distribution loading was removed and replaced with
%% inet loading
%%
%% The start_it/4 function initializes a record with callback 
%% functions used to handle the interface functions.
%%

-module(erl_prim_loader).

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)

-include("inet_boot.hrl").

%% Public
-export([start/3, set_path/1, get_path/0, get_file/1, get_files/2,
         list_dir/1, read_file_info/1, get_cwd/0, get_cwd/1]).

%% Used by erl_boot_server
-export([prim_init/0, prim_get_file/2, prim_list_dir/2,
         prim_read_file_info/2, prim_get_cwd/2]).

%% Used by escript and code
-export([set_primary_archive/3, release_archives/0]).

-include_lib("kernel/include/file.hrl").

-type host() :: atom().

-record(prim_state, {debug :: boolean(),
		     cache,
		     primary_archive}).
-type prim_state() :: #prim_state{}.

-record(state, 
        {loader            :: 'efile' | 'inet',
         hosts = []        :: [host()], % hosts list (to boot from)
         id,                      % not used any more?
         data              :: 'noport' | port(), % data port etc
         timeout           :: timeout(),         % idle timeout
	 %% Number of timeouts before archives are released
	 n_timeouts        :: non_neg_integer(),
         multi_get = false :: boolean(),
         prim_state        :: prim_state()}).    % state for efile code loader

-define(IDLE_TIMEOUT, 60000).  %% tear inet connection after 1 minutes
-define(N_TIMEOUTS, 6).        %% release efile archive after 6 minutes

%% Defines for inet as prim_loader
-define(INET_FAMILY, inet).
-define(INET_ADDRESS, {0,0,0,0}).

-ifdef(DEBUG).
-define(dbg(Tag, Data), erlang:display({Tag,Data})).
-else.
-define(dbg(Tag, Data), true).
-endif.

-define(SAFE2(Expr, State), 
        fun() ->
                case catch Expr of
                    {'EXIT',XXXReason} -> {{error,XXXReason}, State};
                    XXXRes -> XXXRes
                end
        end()).

debug(#prim_state{debug = Deb}, Term) ->
    case Deb of
        false -> ok;
        true  -> erlang:display(Term)
    end.

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

-spec start(term(), atom() | string(), host() | [host()]) ->
	    {'ok', pid()} | {'error', term()}.
start(Id, Pgm, Hosts) when is_atom(Hosts) ->
    start(Id, Pgm, [Hosts]);
start(Id, Pgm0, Hosts) ->
    Pgm = if
              is_atom(Pgm0) ->
                  atom_to_list(Pgm0);
              true ->
                  Pgm0
          end,
    Self = self(),
    Pid = spawn_link(fun() -> start_it(Pgm, Id, Self, Hosts) end),
    register(erl_prim_loader, Pid),
    receive
        {Pid,ok} ->
            {ok,Pid};
        {'EXIT',Pid,Reason} ->
            {error,Reason}
    end.

start_it("ose_inet"=Cmd, Id, Pid, Hosts) ->
    %% Setup reserved port for ose_inet driver (only OSE)
    case catch erlang:open_port({spawn,Cmd}, [binary]) of
        {'EXIT',Why} ->
            ?dbg(ose_inet_port_open_fail, Why),
            Why;
        OseInetPort ->
            ?dbg(ose_inet_port, OseInetPort),
            OseInetPort
    end,
    start_it("inet", Id, Pid, Hosts);

%% Hosts must be a list on form ['1.2.3.4' ...]
start_it("inet", Id, Pid, Hosts) ->
    process_flag(trap_exit, true),
    ?dbg(inet, {Id,Pid,Hosts}),
    AL = ipv4_list(Hosts),
    ?dbg(addresses, AL),
    {ok,Tcp} = find_master(AL),
    init_ack(Pid),
    PS = prim_init(),
    State = #state {loader = inet,
                    hosts = AL,
                    id = Id,
                    data = Tcp,
                    timeout = ?IDLE_TIMEOUT,
		    n_timeouts = ?N_TIMEOUTS,
                    prim_state = PS},
    loop(State, Pid, []);

start_it("efile", Id, Pid, _Hosts) ->
    process_flag(trap_exit, true),
    {ok, Port} = prim_file:open([binary]),
    init_ack(Pid),
    MultiGet = case erlang:system_info(thread_pool_size) of
                   0 -> false;
                   _ -> true
               end,
    PS = prim_init(),
    State = #state {loader = efile,
                    id = Id,
                    data = Port,
                    timeout = infinity,
                    multi_get = MultiGet,
                    prim_state = PS},
    loop(State, Pid, []).

init_ack(Pid) ->
    Pid ! {self(),ok},
    ok.

-spec set_path([string()]) -> 'ok'.
set_path(Paths) when is_list(Paths) ->
    request({set_path,Paths}).

-spec get_path() -> {'ok', [string()]}.
get_path() ->
    request({get_path,[]}).

-spec get_file(atom() | string()) -> {'ok', binary(), string()} | 'error'.
get_file(File) when is_atom(File) ->
    get_file(atom_to_list(File));
get_file(File) ->
    check_file_result(get_file, File, request({get_file,File})).

-spec get_files([{atom(), string()}],
		fun((atom(),binary(),string()) -> 'ok' | {'error', atom()})) ->
			'ok' | {'error', atom()}.
get_files(ModFiles, Fun) ->
    case request({get_files,{ModFiles,Fun}}) of
        E = {error,_M} ->
            E;
        {error,Reason,M} ->
            check_file_result(get_files, M, {error,Reason}),
            {error,M};
        ok ->
            ok
    end.

-spec list_dir(string()) -> {'ok', [string()]} | 'error'.
list_dir(Dir) ->
    check_file_result(list_dir, Dir, request({list_dir,Dir})).

%% -> {ok,Info} | error
-spec read_file_info(string()) -> {'ok', tuple()} | 'error'.

read_file_info(File) ->
    check_file_result(read_file_info, File, request({read_file_info,File})).

-spec get_cwd() -> {'ok', string()} | 'error'.
get_cwd() ->
    check_file_result(get_cwd, [], request({get_cwd,[]})).

-spec get_cwd(string()) -> {'ok', string()} | 'error'.
get_cwd(Drive) ->
    check_file_result(get_cwd, Drive, request({get_cwd,[Drive]})).

-spec set_primary_archive(File :: string() | 'undefined', 
                          ArchiveBin :: binary() | 'undefined',
			  FileInfo :: #file_info{} | 'undefined')
			 -> {ok, [string()]} | {error,_}.

set_primary_archive(undefined, undefined, undefined) ->
    request({set_primary_archive, undefined, undefined, undefined});
set_primary_archive(File, ArchiveBin, FileInfo)
  when is_list(File), is_binary(ArchiveBin), is_record(FileInfo, file_info) ->
    request({set_primary_archive, File, ArchiveBin, FileInfo}).

-spec release_archives() -> 'ok' | {'error', _}.

release_archives() ->
    request(release_archives).

request(Req) ->
    Loader = whereis(erl_prim_loader),
    Loader ! {self(),Req},
    receive
        {Loader,Res} ->
            Res;
        {'EXIT',Loader,_What} ->
            error
    end.

check_file_result(_, _, {error,enoent}) ->
    error;
check_file_result(_, _, {error,enotdir}) ->
    error;
check_file_result(Func, Target, {error,Reason}) ->   
    case (catch atom_to_list(Reason)) of
        {'EXIT',_} ->                           % exit trapped
            error;
        Errno ->                                % errno
            Process = case process_info(self(), registered_name) of
                          {registered_name,R} -> 
                              "Process: " ++ atom_to_list(R) ++ ".";
                          _ -> 
                              ""
                      end,
            TargetStr =
                if is_atom(Target) -> atom_to_list(Target);
                   is_list(Target) -> Target;
                   true -> []
                end,
            Report = 
                case TargetStr of
                    [] ->
                        "File operation error: " ++ Errno ++ ". " ++
                        "Function: " ++ atom_to_list(Func) ++ ". " ++ Process;
                    _ ->
                        "File operation error: " ++ Errno ++ ". " ++
                        "Target: " ++ TargetStr ++ ". " ++
                        "Function: " ++ atom_to_list(Func) ++ ". " ++ Process
                end,
            %% this is equal to calling error_logger:error_report/1 which
            %% we don't want to do from code_server during system boot
            error_logger ! {notify,{error_report,group_leader(),
                                    {self(),std_error,Report}}},
            error
    end;
check_file_result(_, _, Other) ->
    Other.

%%% --------------------------------------------------------
%%% The main loop.
%%% --------------------------------------------------------

loop(State, Parent, Paths) ->
    receive
        {Pid,Req} when is_pid(Pid) ->
            %% erlang:display(Req),
            {Resp,State2,Paths2} =
                case Req of
                    {set_path,NewPaths} ->
                        {ok,State,to_strs(NewPaths)};
                    {get_path,_} ->
                        {{ok,Paths},State,Paths};
                    {get_file,File} ->
                        {Res,State1} = handle_get_file(State, Paths, File),
                        {Res,State1,Paths};
                    {get_files,{ModFiles,Fun}} ->
                        {Res,State1} = handle_get_files(State, ModFiles, Paths, Fun),
                        {Res,State1,Paths};
                    {list_dir,Dir} ->
                        {Res,State1} = handle_list_dir(State, Dir),
                        {Res,State1,Paths};
                    {read_file_info,File} ->
                        {Res,State1} = handle_read_file_info(State, File),
                        {Res,State1,Paths};
                    {get_cwd,[]} ->
                        {Res,State1} = handle_get_cwd(State, []),
                        {Res,State1,Paths};
                    {get_cwd,[_]=Args} ->
                        {Res,State1} = handle_get_cwd(State, Args),
                        {Res,State1,Paths};
                    {set_primary_archive,File,Bin,FileInfo} ->
                        {Res,State1} = handle_set_primary_archive(State, File, Bin, FileInfo),
                        {Res,State1,Paths};
                    release_archives ->
                        {Res,State1} = handle_release_archives(State),
                        {Res,State1,Paths};
                    _Other ->
                        {ignore,State,Paths}
                end,
            if Resp =:= ignore -> ok;
               true -> Pid ! {self(),Resp}, ok
            end,
            if 
                is_record(State2, state) ->
                    loop(State2, Parent, Paths2);
                true ->
                    exit({bad_state, Req, State2})          
            end;
        {'EXIT',Parent,W} ->
            _State1 = handle_stop(State),
            exit(W);
        {'EXIT',P,W} ->
            State1 = handle_exit(State, P, W),
            loop(State1, Parent, Paths);
        _Message ->
            loop(State, Parent, Paths)
    after State#state.timeout ->
            State1 = handle_timeout(State, Parent),
            loop(State1, Parent, Paths)
    end.

handle_get_files(State = #state{multi_get = true}, ModFiles, Paths, Fun) ->
    ?SAFE2(efile_multi_get_file_from_port(State, ModFiles, Paths, Fun), State);
handle_get_files(State, _ModFiles, _Paths, _Fun) ->     % no multi get
    {{error,no_multi_get},State}.
    
handle_get_file(State = #state{loader = efile}, Paths, File) ->
    ?SAFE2(efile_get_file_from_port(State, File, Paths), State);
handle_get_file(State = #state{loader = inet}, Paths, File) ->
    ?SAFE2(inet_get_file_from_port(State, File, Paths), State).

handle_set_primary_archive(State= #state{loader = efile}, File, Bin, FileInfo) ->
    ?SAFE2(efile_set_primary_archive(State, File, Bin, FileInfo), State).

handle_release_archives(State= #state{loader = efile}) ->
    ?SAFE2(efile_release_archives(State), State).

handle_list_dir(State = #state{loader = efile}, Dir) ->
    ?SAFE2(efile_list_dir(State, Dir), State);
handle_list_dir(State = #state{loader = inet}, Dir) ->
    ?SAFE2(inet_list_dir(State, Dir), State).

handle_read_file_info(State = #state{loader = efile}, File) ->
    ?SAFE2(efile_read_file_info(State, File), State);
handle_read_file_info(State = #state{loader = inet}, File) ->
    ?SAFE2(inet_read_file_info(State, File), State).

handle_get_cwd(State = #state{loader = efile}, Drive) ->
    ?SAFE2(efile_get_cwd(State, Drive), State);
handle_get_cwd(State = #state{loader = inet}, Drive) ->
    ?SAFE2(inet_get_cwd(State, Drive), State).
    
handle_stop(State = #state{loader = efile}) ->
    efile_stop_port(State);
handle_stop(State = #state{loader = inet}) ->
    inet_stop_port(State).

handle_exit(State = #state{loader = efile}, Who, Reason) ->
    efile_exit_port(State, Who, Reason);
handle_exit(State = #state{loader = inet}, Who, Reason) ->
    inet_exit_port(State, Who, Reason).

handle_timeout(State = #state{loader = efile}, Parent) ->
    efile_timeout_handler(State, Parent);
handle_timeout(State = #state{loader = inet}, Parent) ->
    inet_timeout_handler(State, Parent).

%%% --------------------------------------------------------
%%% Functions which handles efile as prim_loader (default).
%%% --------------------------------------------------------

%%% Reading many files in parallel is an optimization. 
%%% See also comment in init.erl.

%% -> {ok,State} | {{error,Module},State} | {{error,Reason,Module},State}
efile_multi_get_file_from_port(State, ModFiles, Paths, Fun) ->
    Ref = make_ref(),
    %% More than 200 processes is no gain.
    Max = erlang:min(200, erlang:system_info(thread_pool_size)),
    efile_multi_get_file_from_port2(ModFiles, 0, Max, State, Paths, Fun, Ref, ok).

efile_multi_get_file_from_port2([MF | MFs], Out, Max, State, Paths, Fun, Ref, Ret) when Out < Max ->
    Self = self(),
    _Pid = spawn(fun() -> efile_par_get_file(Ref, State, MF, Paths, Self, Fun) end),
    efile_multi_get_file_from_port2(MFs, Out+1, Max, State, Paths, Fun, Ref, Ret);
efile_multi_get_file_from_port2(MFs, Out, Max, _State, Paths, Fun, Ref, Ret) when Out > 0 ->
    receive 
        {Ref, ok, State1} ->
            efile_multi_get_file_from_port2(MFs, Out-1, Max, State1, Paths, Fun, Ref, Ret);
        {Ref, {error,_Mod} = Error, State1} ->
            efile_multi_get_file_from_port2(MFs, Out-1, Max, State1, Paths, Fun, Ref, Error);
        {Ref, MF, {error,emfile,State1}} ->
            %% Max can take negative values. Out cannot.
            efile_multi_get_file_from_port2([MF | MFs], Out-1, Max-1, State1, Paths, Fun, Ref, Ret);
        {Ref, {M,_F}, {error,Error,State1}} -> 
            efile_multi_get_file_from_port2(MFs, Out-1, 0, State1, Paths, Fun, Ref, {error,Error,M})
    end;
efile_multi_get_file_from_port2(_MFs, 0, _Max, State, _Paths, _Fun, _Ref, Ret) ->
    {Ret,State}.

efile_par_get_file(Ref, State, {Mod,File} = MF, Paths, Pid, Fun) ->
    %% One port for each file read in "parallel":
    case prim_file:open([binary]) of
        {ok, Port} ->
            Port0 = State#state.data,
            State1 = State#state{data = Port},
            R = case efile_get_file_from_port(State1, File, Paths) of
                    {{error,Reason},State2} -> 
                        {Ref,MF,{error,Reason,State2}};
                    {{ok,BinFile,Full},State2} -> 
                        %% Fun(...) -> ok | {error,Mod}
                        {Ref,Fun(Mod, BinFile, Full),State2#state{data=Port0}}
                end,
            prim_file:close(Port),
            Pid ! R;
        {error, Error} ->
            Pid ! {Ref,MF,{error,Error,State}}
    end.

%% -> {{ok,BinFile,File},State} | {{error,Reason},State}
efile_get_file_from_port(State, File, Paths) ->
    case is_basename(File) of
        false ->                        % get absolute file name.
            efile_get_file_from_port2(State, File);
        true when Paths =:= [] ->       % get plain file name.
            efile_get_file_from_port2(State, File);
        true ->                         % use paths.
            efile_get_file_from_port3(State, File, Paths)
    end.

efile_get_file_from_port2(#state{prim_state = PS} = State, File) ->
    {Res, PS2} = prim_get_file(PS, File),
    case Res of
        {error,port_died} ->
            exit('prim_load port died');
        {error,Reason} ->
            {{error,Reason},State#state{prim_state = PS2}};
        {ok,BinFile} ->
            {{ok,BinFile,File},State#state{prim_state = PS2}}
    end.

efile_get_file_from_port3(State, File, [P | Paths]) ->
    case efile_get_file_from_port2(State, concat([P,"/",File])) of
        {{error,Reason},State1} when Reason =/= emfile ->
            case Paths of
                [] ->                           % return last error
                    {{error,Reason},State1};
                _ ->                            % try more paths
                    efile_get_file_from_port3(State1, File, Paths)
            end;
        Result ->
            Result
    end;
efile_get_file_from_port3(State, _File, []) ->
    {{error,enoent},State}.

efile_set_primary_archive(#state{prim_state = PS} = State, File, Bin, FileInfo) ->
    {Res, PS2} = prim_set_primary_archive(PS, File, Bin, FileInfo),
    {Res,State#state{prim_state = PS2}}.

efile_release_archives(#state{prim_state = PS} = State) ->
    {Res, PS2} = prim_release_archives(PS),
    {Res,State#state{prim_state = PS2}}.

efile_list_dir(#state{prim_state = PS} = State, Dir) ->
    {Res, PS2} = prim_list_dir(PS, Dir),
    {Res, State#state{prim_state = PS2}}.

efile_read_file_info(#state{prim_state = PS} = State, File) ->
    {Res, PS2} = prim_read_file_info(PS, File),
    {Res, State#state{prim_state = PS2}}.

efile_get_cwd(#state{prim_state = PS} = State, Drive) ->
    {Res, PS2} = prim_get_cwd(PS, Drive),
    {Res, State#state{prim_state = PS2}}.

efile_stop_port(#state{data=Port}=State) ->
    prim_file:close(Port),
    State#state{data=noport}.

efile_exit_port(State, Port, Reason) when State#state.data =:= Port ->
    exit({port_died,Reason});
efile_exit_port(State, _Port, _Reason) ->
    State.

efile_timeout_handler(#state{n_timeouts = N} = State, _Parent) ->
    if
	N =< 0 ->
	    {_Res, State2} = efile_release_archives(State),
	    State2#state{n_timeouts = ?N_TIMEOUTS};
	true ->
	    State#state{n_timeouts = N - 1}
    end.

%%% --------------------------------------------------------
%%% Functions which handles inet prim_loader
%%% --------------------------------------------------------

%%
%% Connect to a boot master
%% return {ok, Socket}  TCP
%% AL is a list of boot servers (including broadcast addresses)
%%
find_master(AL) ->
    find_master(AL, ?EBOOT_RETRY, ?EBOOT_REQUEST_DELAY, ?EBOOT_SHORT_RETRY_SLEEP, 
               ?EBOOT_UNSUCCESSFUL_TRIES, ?EBOOT_LONG_RETRY_SLEEP).

find_master(AL, Retry, ReqDelay, SReSleep, Tries, LReSleep) ->
    {ok,U} = ll_udp_open(0),
    find_master(U, Retry, AL, ReqDelay, SReSleep, [], Tries, LReSleep).

%%
%% Master connect loop
%%
find_master(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, Tries, LReSleep) ->
    case find_loop(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, 
                   Tries, LReSleep) of
        [] ->   
            find_master(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, 
                        Tries, LReSleep);
        Servers ->
            ?dbg(servers, Servers),
            case connect_master(Servers) of
                {ok, Socket} -> 
                    ll_close(U),
                    {ok, Socket};
                _Error ->
                    find_master(U, Retry, AddrL, ReqDelay, SReSleep, 
                                Servers ++ Ignore, Tries, LReSleep)
            end
    end.

connect_master([{_Prio,IP,Port} | Servers]) ->
    case ll_tcp_connect(0, IP, Port) of
        {ok, S} -> {ok, S};
        _Error -> connect_master(Servers)
    end;
connect_master([]) ->
    {error, ebusy}.

%%
%% Always return a list of boot servers or hang.
%%
find_loop(U, Retry, AL, ReqDelay, SReSleep, Ignore, Tries, LReSleep) ->
    case find_loop(U, Retry, AL, ReqDelay, []) of
        [] ->                                   % no response from any server
            erlang:display({erl_prim_loader,'no server found'}), % lifesign
            Tries1 =
		if Tries > 0 ->
			sleep(SReSleep),
			Tries - 1;
		   true ->
			sleep(LReSleep),
			0
		end,
            find_loop(U, Retry, AL, ReqDelay, SReSleep, Ignore, Tries1, LReSleep);
        Servers ->
            keysort(1, Servers -- Ignore)
    end.

%% broadcast or send
find_loop(_U, 0, _AL, _Delay, Acc) ->
    Acc;
find_loop(U, Retry, AL, Delay, Acc) ->
    send_all(U, AL, [?EBOOT_REQUEST, erlang:system_info(version)]),
    find_collect(U, Retry-1, AL, Delay, Acc).

find_collect(U,Retry,AL,Delay,Acc) ->
    receive
        {udp, U, IP, _Port, [$E,$B,$O,$O,$T,$R,Priority,T1,T0 | _Version]} ->
            Elem = {Priority,IP,T1*256+T0},
            ?dbg(got, Elem),
            case member(Elem, Acc) of
                false  -> find_collect(U, Retry, AL, Delay, [Elem | Acc]);
                true -> find_collect(U, Retry, AL, Delay, Acc)
            end;
        _Garbage ->
            ?dbg(collect_garbage, _Garbage),
            find_collect(U, Retry, AL, Delay, Acc)
    after Delay ->
            ?dbg(collected, Acc),
            case keymember(0, 1, Acc) of  %% got high priority server?
                true -> Acc;
                false -> find_loop(U, Retry, AL, Delay, Acc)
            end
    end.

    
sleep(Time) ->
    receive after Time -> ok end.

inet_exit_port(State, Port, _Reason) when State#state.data =:= Port ->
    State#state{data = noport, timeout = infinity};
inet_exit_port(State, _, _) ->
    State.


inet_timeout_handler(State, _Parent) ->
    Tcp = State#state.data,
    if is_port(Tcp) -> ll_close(Tcp);
       true -> ok
    end,
    State#state{timeout = infinity, data = noport}.

%% -> {{ok,BinFile,Tag},State} | {{error,Reason},State}
inet_get_file_from_port(State, File, Paths) ->
    case is_basename(File) of
        false ->                        % get absolute file name.
            inet_send_and_rcv({get,File}, File, State);
        true when Paths =:= [] ->       % get plain file name.
            inet_send_and_rcv({get,File}, File, State);
        true ->                         % use paths.
            inet_get_file_from_port1(File, Paths, State)
    end.

inet_get_file_from_port1(File, [P | Paths], State) ->
    File1 = concat([P,"/",File]),
    case inet_send_and_rcv({get,File1}, File1, State) of
        {{error,Reason},State1} ->
            case Paths of
                [] ->                           % return last error
                    {{error,Reason},State1};
                _ ->                            % try more paths            
                    inet_get_file_from_port1(File, Paths, State1)
            end;
        Result -> Result
    end;
inet_get_file_from_port1(_File, [], State) ->
    {{error,file_not_found},State}.

inet_send_and_rcv(Msg, Tag, State) when State#state.data =:= noport ->
    {ok,Tcp} = find_master(State#state.hosts),     %% reconnect
    inet_send_and_rcv(Msg, Tag, State#state{data = Tcp,
					    timeout = ?IDLE_TIMEOUT});
inet_send_and_rcv(Msg, Tag, #state{data = Tcp, timeout = Timeout} = State) ->
    prim_inet:send(Tcp, term_to_binary(Msg)),
    receive
        {tcp,Tcp,BinMsg} ->
            case catch binary_to_term(BinMsg) of
                {get,{ok,BinFile}} ->
                    {{ok,BinFile,Tag},State};
                {_Cmd,Res={ok,_}} ->
                    {Res,State};
                {_Cmd,{error,Error}} ->
                    {{error,Error},State};
                {error,Error} ->
                    {{error,Error},State};
                {'EXIT',Error} ->
                    {{error,Error},State}
            end;
        {tcp_closed,Tcp} ->
            %% Ok we must reconnect
            inet_send_and_rcv(Msg, Tag, State#state{data = noport});
        {tcp_error,Tcp,_Reason} ->
            %% Ok we must reconnect
            inet_send_and_rcv(Msg, Tag, inet_stop_port(State));
        {'EXIT', Tcp, _} -> 
            %% Ok we must reconnect
            inet_send_and_rcv(Msg, Tag, State#state{data = noport})
    after Timeout ->
            %% Ok we must reconnect
            inet_send_and_rcv(Msg, Tag, inet_stop_port(State))
    end.

%% -> {{ok,List},State} | {{error,Reason},State}
inet_list_dir(State, Dir) ->
    inet_send_and_rcv({list_dir,Dir}, list_dir, State).

%% -> {{ok,Info},State} | {{error,Reason},State}
inet_read_file_info(State, File) ->
    inet_send_and_rcv({read_file_info,File}, read_file_info, State).

%% -> {{ok,Cwd},State} | {{error,Reason},State}
inet_get_cwd(State, []) ->
    inet_send_and_rcv(get_cwd, get_cwd, State);
inet_get_cwd(State, [Drive]) ->
    inet_send_and_rcv({get_cwd,Drive}, get_cwd, State).

inet_stop_port(#state{data=Tcp}=State) ->
    prim_inet:close(Tcp),
    State#state{data=noport}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Direct inet_drv access
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcp_options() ->
    [{mode,binary}, {packet,4}, {active, true}, {deliver,term}].

tcp_timeout() -> 
    15000.

%% options for udp  [list, {broadcast, true}, {active,true}]
udp_options() ->
    [{mode,list}, {active, true}, {deliver,term}, {broadcast,true}].
%%
%% INET version IPv4 addresses
%%
ll_tcp_connect(LocalPort, IP, RemotePort) ->
    case ll_open_set_bind(tcp, ?INET_FAMILY, tcp_options(),
                          ?INET_ADDRESS, LocalPort) of
        {ok,S} ->
            case prim_inet:connect(S, IP, RemotePort, tcp_timeout()) of
                ok -> {ok, S};
                Error -> port_error(S, Error)
            end;
        Error -> Error
    end.

%%
%% Open and initialize an udp port for broadcast
%%
ll_udp_open(P) ->
    ll_open_set_bind(udp, ?INET_FAMILY, udp_options(), ?INET_ADDRESS, P).


ll_open_set_bind(Protocol, Family, SOpts, IP, Port) ->
    case prim_inet:open(Protocol, Family) of
        {ok, S} ->
            case prim_inet:setopts(S, SOpts) of
                ok ->
                    case prim_inet:bind(S, IP, Port) of
                        {ok,_} ->
                            {ok, S};
                        Error -> port_error(S, Error)
                    end;
                Error -> port_error(S, Error)
            end;
        Error -> Error
    end.
                    

ll_close(S) ->
    unlink(S),
    exit(S, kill).

port_error(S, Error) ->
    unlink(S),
    prim_inet:close(S),
    Error.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec prim_init() -> prim_state().
prim_init() ->
    Deb =
        case init:get_argument(loader_debug) of
            {ok, _} -> true;
            error -> false
        end,
    cache_new(#prim_state{debug = Deb}).

prim_release_archives(PS) ->
    debug(PS, release_archives),
    {Res, PS2} = prim_do_release_archives(PS, get(), []),
    debug(PS2, {return, Res}),
    {Res, PS2}.

prim_do_release_archives(PS, [{ArchiveFile, DictVal} | KeyVals], Acc) ->
    Res = 
	case DictVal of
	    {primary, _PrimZip, _FI} ->
		ok; % Keep primary archive
	    {Cache, _FI} ->
		debug(PS, {release, cache, ArchiveFile}),
		erase(ArchiveFile),
		clear_cache(ArchiveFile, Cache)
	end,
    case Res of
	ok ->
	    prim_do_release_archives(PS, KeyVals, Acc);
	{error, Reason} ->
	    prim_do_release_archives(PS, KeyVals, [{ArchiveFile, Reason} | Acc])
    end;
prim_do_release_archives(PS, [], []) ->
    {ok, PS#prim_state{primary_archive = undefined}};
prim_do_release_archives(PS, [], Errors) ->
    {{error, Errors}, PS#prim_state{primary_archive = undefined}}.

prim_set_primary_archive(PS, undefined, undefined, undefined) ->
    debug(PS, {set_primary_archive, clean}),
    case PS#prim_state.primary_archive of
        undefined ->
            Res = {error, enoent},
            debug(PS, {return, Res}),
            {Res, PS};
        ArchiveFile ->
            {primary, PrimZip, _FI} = erase(ArchiveFile),
            ok = prim_zip:close(PrimZip),
            PS2 = PS#prim_state{primary_archive = undefined},
            Res = {ok, []},
            debug(PS2, {return, Res}),
            {Res, PS2}
    end;
prim_set_primary_archive(PS, ArchiveFile, ArchiveBin, #file_info{} = FileInfo)
  when is_list(ArchiveFile), is_binary(ArchiveBin) ->
    %% Try the archive file
    debug(PS, {set_primary_archive, ArchiveFile, byte_size(ArchiveBin)}),
    {Res3, PS3} =
        case PS#prim_state.primary_archive of
            undefined ->
                Fun =
                    fun({Funny, _GI, _GB}, A) ->
                            case Funny of
                                ["", "nibe", RevApp] -> % Reverse ebin
                                    %% Collect ebin directories in archive
                                    Ebin = reverse(RevApp) ++ "/ebin",
				    {true, [Ebin | A]};
                                _ ->
                                    {true, A}
                            end
                    end,
                Ebins0 = [ArchiveFile],
                case open_archive({ArchiveFile, ArchiveBin}, FileInfo, Ebins0, Fun) of
                    {ok, PrimZip, {RevEbins, FI, _}} ->
                        Ebins = reverse(RevEbins),
                        debug(PS, {set_primary_archive, Ebins}),
                        put(ArchiveFile, {primary, PrimZip, FI}),
                        {{ok, Ebins}, PS#prim_state{primary_archive = ArchiveFile}};
                    Error ->
                        debug(PS, {set_primary_archive, Error}),
                        {Error, PS}
                end;
            OldArchiveFile ->
                debug(PS, {set_primary_archive, clean}),
                {primary, PrimZip, _FI} = erase(OldArchiveFile),
                ok = prim_zip:close(PrimZip),
                PS2 = PS#prim_state{primary_archive = undefined},
                prim_set_primary_archive(PS2, ArchiveFile, ArchiveBin, FileInfo)
        end,
    debug(PS3, {return, Res3}),
    {Res3, PS3}.

-spec prim_get_file(prim_state(), file:filename()) -> {_, prim_state()}.
prim_get_file(PS, File) ->
    debug(PS, {get_file, File}),
    {Res2, PS2} =
        case name_split(PS#prim_state.primary_archive, File) of
            {file, PrimFile} ->
                Res = prim_file:read_file(PrimFile),
                {Res, PS};
            {archive, ArchiveFile, FileInArchive} ->
                debug(PS, {archive_get_file, ArchiveFile, FileInArchive}),
                FunnyFile = funny_split(FileInArchive, $/),
                Fun =
                    fun({Funny, _GetInfo, GetBin}, Acc) ->
                            if
                                Funny =:= FunnyFile ->
                                    {false, {ok, GetBin()}};
                                true ->
                                    {true, Acc}
                            end
                    end,
                apply_archive(PS, Fun, {error, enoent}, ArchiveFile)
        end,
    debug(PS, {return, Res2}),
    {Res2, PS2}.    

-spec prim_list_dir(prim_state(), file:filename()) ->
	 {{'ok', [file:filename()]}, prim_state()}
       | {{'error', term()}, prim_state()}.
prim_list_dir(PS, Dir) ->
    debug(PS, {list_dir, Dir}),
    {Res2, PS3} =
        case name_split(PS#prim_state.primary_archive, Dir) of
            {file, PrimDir} ->
                Res = prim_file:list_dir(PrimDir),
                {Res, PS};
            {archive, ArchiveFile, FileInArchive} ->
                debug(PS, {archive_list_dir, ArchiveFile, FileInArchive}),
                FunnyDir = funny_split(FileInArchive, $/),
                Fun =
                    fun({Funny, _GetInfo, _GetBin}, {Status, Names} = Acc) ->
                            case Funny of
                                [RevName | FD] when FD =:= FunnyDir ->
                                    case RevName of
                                        "" ->
                                            %% The listed directory
                                            {true, {ok, Names}};
                                        _ ->
                                            %% Plain file
                                            Name = reverse(RevName),
                                            {true, {Status, [Name | Names]}}
                                    end;
                                ["", RevName | FD] when FD =:= FunnyDir ->
                                    %% Directory
                                    Name = reverse(RevName),
                                    {true, {Status, [Name | Names]}};
                                [RevName] when FunnyDir =:= [""] ->
                                    %% Top file
                                    Name = reverse(RevName),
                                    {true, {ok, [Name | Names]}};
                                ["", RevName] when FunnyDir =:= [""] ->
                                    %% Top file
                                    Name = reverse(RevName),
                                    {true, {ok, [Name | Names]}};
                                _ ->
                                    %% No match
                                    {true, Acc}
                            end
                    end,
                {{Status, Names}, PS2} =
                    apply_archive(PS, Fun, {error, []}, ArchiveFile),
                case Status of
                    ok    -> {{ok, Names}, PS2};
                    error -> {{error, enotdir}, PS2}
                end
        end,
    debug(PS, {return, Res2}),
    {Res2, PS3}.

-spec prim_read_file_info(prim_state(), file:filename()) ->
	{{'ok', #file_info{}}, prim_state()}
      | {{'error', term()}, prim_state()}.
prim_read_file_info(PS, File) ->
    debug(PS, {read_file_info, File}),
    {Res2, PS2} =
        case name_split(PS#prim_state.primary_archive, File) of
            {file, PrimFile} ->
                Res = prim_file:read_file_info(PrimFile),
                {Res, PS};
            {archive, ArchiveFile, []} ->
                %% Fake top directory
                debug(PS, {archive_read_file_info, ArchiveFile}),
                case prim_file:read_file_info(ArchiveFile) of
                    {ok, FI} ->
                        {{ok, FI#file_info{type = directory}}, PS};
                    Other ->
                        {Other, PS}
                end;
            {archive, ArchiveFile, FileInArchive} ->
                debug(PS, {archive_read_file_info, File}),
                FunnyFile = funny_split(FileInArchive, $/),
                Fun =
                    fun({Funny, GetInfo, _GetBin}, Acc)  ->
			    case Funny of
				[H | T] when  H =:= "",
					      T =:= FunnyFile ->
                                    %% Directory
                                    {false, {ok, GetInfo()}};
                                F when F =:= FunnyFile ->
                                    %% Plain file
                                    {false, {ok, GetInfo()}};
                                _ ->
                                    %% No match
                                    {true, Acc}
                            end
                    end,
                apply_archive(PS, Fun, {error, enoent}, ArchiveFile)
        end,
    debug(PS2, {return, Res2}),
    {Res2, PS2}.

-spec prim_get_cwd(prim_state(), [file:filename()]) ->
        {{'error', term()} | {'ok', _}, prim_state()}.
prim_get_cwd(PS, []) ->
    debug(PS, {get_cwd, []}),
    Res = prim_file:get_cwd(),
    debug(PS, {return, Res}),
    {Res, PS};
prim_get_cwd(PS, [Drive]) ->
    debug(PS, {get_cwd, Drive}),
    Res = prim_file:get_cwd(Drive),
    debug(PS, {return, Res}),
    {Res, PS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_archive(PS, Fun, Acc, Archive) ->
    case get(Archive) of
        undefined ->
	    case open_archive(Archive, Acc, Fun) of
		{ok, PrimZip, {Acc2, FI, _}} ->
		    debug(PS, {cache, ok}),
		    put(Archive, {{ok, PrimZip}, FI}),
		    {Acc2, PS};
		Error ->
		    debug(PS, {cache, Error}),
		    %% put(Archive, {Error, FI}),
		    {Error, PS}
	    end;
        {primary, PrimZip, FI} ->
	    case prim_file:read_file_info(Archive) of
                {ok, FI2} 
		  when FI#file_info.mtime =:= FI2#file_info.mtime ->
		    case foldl_archive(PrimZip, Acc, Fun) of
			{ok, _PrimZip2, Acc2} ->
			    {Acc2, PS};
			Error ->
			    debug(PS, {primary, Error}),
			    {Error, PS}
		    end;
		Error ->
		    debug(PS, {cache, {clear, Error}}),
		    clear_cache(Archive, {ok, PrimZip}),
		    apply_archive(PS, Fun, Acc, Archive)
	    end;
        {Cache, FI} ->
            case prim_file:read_file_info(Archive) of
                {ok, FI2} 
		  when FI#file_info.mtime =:= FI2#file_info.mtime ->
                    case Cache of
                        {ok, PrimZip} ->
                            case foldl_archive(PrimZip, Acc, Fun) of
                                {ok, _PrimZip2, Acc2} ->
                                    {Acc2, PS};
                                Error ->
                                    debug(PS, {cache, {clear, Error}}),
                                    ok = clear_cache(Archive, Cache),
                                    debug(PS, {cache, Error}),
				    erase(Archive),
                                    %% put(Archive, {Error, FI}),
                                    {Error, PS}
                            end;
                        Error ->
                            debug(PS, {cache, Error}),
                            {Error, PS}
                    end;
                Error ->
                    debug(PS, {cache, {clear, Error}}),
                    ok = clear_cache(Archive, Cache),
                    apply_archive(PS, Fun, Acc, Archive)
            end
    end.

open_archive(Archive, Acc, Fun) ->
    case prim_file:read_file_info(Archive) of
	{ok, FileInfo} ->
	    open_archive(Archive, FileInfo, Acc, Fun);
	{error, Reason} ->
	    {error, Reason}
    end.

open_archive(Archive, FileInfo, Acc, Fun) ->
    FakeFI = FileInfo#file_info{type = directory},
    Wrapper =
	fun({N, GI, GB}, {A, I, FunnyDirs}) -> % Full iteration at open
		Funny = funny_split(N, $/),
		FunnyDirs2 =
		    case Funny of
			["" | FunnyDir] ->
			    [FunnyDir | FunnyDirs];
			_ ->
			    FunnyDirs
		    end,
		{Includes, FunnyDirs3, A2} = 
		    ensure_virtual_dirs(Funny, Fun, FakeFI, [{true, Funny}], FunnyDirs2, A),
		{_Continue, A3} = Fun({Funny, GI, GB}, A2),
		{true, Includes, {A3, I, FunnyDirs3}}
	end,
    prim_zip:open(Wrapper, {Acc, FakeFI, []}, Archive).

ensure_virtual_dirs(Funny, Fun, FakeFI, Includes, FunnyDirs, Acc) ->
    case Funny of
	[_ | FunnyDir] ->
	    case lists:member(FunnyDir, FunnyDirs) of % BIF
		false ->
		    GetInfo = fun() -> FakeFI end,
		    GetBin = fun() -> <<>> end,
		    VirtualDir = ["" | FunnyDir],
		    Includes2 = [{true, VirtualDir, GetInfo, GetBin} | Includes],
		    FunnyDirs2 = [FunnyDir | FunnyDirs],
		    {I, F, Acc2} = ensure_virtual_dirs(FunnyDir, Fun, FakeFI, Includes2, FunnyDirs2, Acc),
		    {_Continue, Acc3} = Fun({VirtualDir, GetInfo, GetBin}, Acc2),
		    {I, F, Acc3};
		true ->
		    {reverse(Includes), FunnyDirs, Acc}
	    end;
	[] ->
	    {reverse(Includes), FunnyDirs, Acc}
    end.

foldl_archive(PrimZip, Acc, Fun) ->
    Wrapper =
        fun({Funny, GI, GB}, A) ->
                %% Allow partial iteration at foldl
                {Continue, A2} = Fun({Funny, GI, GB}, A),
                {Continue, true, A2}
        end,                        
    prim_zip:foldl(Wrapper, Acc, PrimZip).

cache_new(PS) ->
    PS.

clear_cache(Archive, Cache) ->
    erase(Archive),
    case Cache of
        {ok, PrimZip} ->
            prim_zip:close(PrimZip);
        {error, _} ->
            ok
    end.

%%% --------------------------------------------------------
%%% Misc. functions.
%%% --------------------------------------------------------

%%% Look for directory separators
is_basename(File) ->
    case deep_member($/, File) of
        true -> 
            false;
        false ->
            case erlang:system_info(os_type) of
                {win32, _} ->
                    case File of
                        [_,$: | _] ->
			    false;
                        _ -> 
			    not deep_member($\\, File)
                    end;
                _ ->
                    true
            end
    end.

send_all(U, [IP | AL], Cmd) ->
    ?dbg(sendto, {U, IP, ?EBOOT_PORT, Cmd}),
    prim_inet:sendto(U, IP, ?EBOOT_PORT, Cmd),
    send_all(U, AL, Cmd);
send_all(_U, [], _) -> ok.

%%concat([A|T]) when is_atom(A) ->              %Atom
%%    atom_to_list(A) ++ concat(T);
concat([C|T]) when C >= 0, C =< 255 ->
    [C|concat(T)];
concat([S|T]) ->                                %String
    S ++ concat(T);
concat([]) ->
    [].

member(X, [X|_]) -> true;
member(X, [_|Y]) -> member(X, Y);
member(_X, [])   -> false.

deep_member(X, [X|_]) -> 
    true;
deep_member(X, [List | Y]) when is_list(List) ->
    deep_member(X, List) orelse deep_member(X, Y);
deep_member(X, [Atom | Y]) when is_atom(Atom) ->
    deep_member(X, atom_to_list(Atom)) orelse deep_member(X, Y);
deep_member(X, [_ | Y]) -> 
    deep_member(X, Y);
deep_member(_X, [])   ->
    false.

keymember(X, I, [Y | _]) when element(I,Y) =:= X -> true;
keymember(X, I, [_ | T]) -> keymember(X, I, T);
keymember(_X, _I, []) -> false.

keysort(I, L) -> keysort(I, L, []).

keysort(I, [X | L], Ls) ->
    keysort(I, L, keyins(X, I, Ls));
keysort(_I, [], Ls) -> Ls.

keyins(X, I, [Y | T]) when X < element(I,Y) -> [X,Y|T];
keyins(X, I, [Y | T]) -> [Y | keyins(X, I, T)];
keyins(X, _I, []) -> [X].

to_strs([P|Paths]) when is_atom(P) ->
    [atom_to_list(P)|to_strs(Paths)];
to_strs([P|Paths]) when is_list(P) ->
    [P|to_strs(Paths)];
to_strs([_|Paths]) ->
    to_strs(Paths);
to_strs([]) ->
    [].

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]). % BIF
                        
%% Returns all lists in reverse order
funny_split(List, Sep) ->
   funny_split(List, Sep, [], []).

funny_split([Sep | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [], [Path | Paths]);
funny_split([Head | Tail], Sep, Path, Paths) ->
    funny_split(Tail, Sep, [Head | Path], Paths);
funny_split([], _Sep, Path, Paths) ->
    [Path | Paths].

name_split(ArchiveFile, File0) ->
    File = absname(File0),
    do_name_split(ArchiveFile, File).
    
do_name_split(undefined, File) ->
    %% Ignore primary archive
    case string_split(File, init:archive_extension(), []) of
        no_split ->
            %% Plain file
            {file, File};
        {split, _RevArchiveBase, RevArchiveFile, []} ->
            %% Top dir in archive
            ArchiveFile = reverse(RevArchiveFile),
            {archive, ArchiveFile, []};
        {split, _RevArchiveBase, RevArchiveFile, [$/ | FileInArchive]} ->
            %% File in archive
            ArchiveFile = reverse(RevArchiveFile),
            {archive, ArchiveFile, FileInArchive};
        {split, _RevArchiveBase, _RevArchiveFile, _FileInArchive} ->
	    %% False match. Assume plain file
            {file, File}
    end;
do_name_split(ArchiveFile0, File) ->
    %% Look first in primary archive
    ArchiveFile = absname(ArchiveFile0),
    case string_match(File, ArchiveFile, []) of
        no_match ->
            %% Archive or plain file
            do_name_split(undefined, File);
        {match, _RevPrimArchiveFile, FileInArchive} ->
            %% Primary archive
            case FileInArchive of
                [$/ | FileInArchive2] ->
                    {archive, ArchiveFile, FileInArchive2};
                _ ->
                    {archive, ArchiveFile, FileInArchive}
            end
    end.

string_match([Char | File], [Char | Archive], RevTop) ->
    string_match(File, Archive, [Char | RevTop]);
string_match(File, [], RevTop) ->
    {match, RevTop, File};
string_match(_File, _Archive, _RevTop) ->
    no_match.

string_split([Char | File], [Char | Ext] = FullExt, RevTop) ->
    RevTop2 = [Char | RevTop],
    string_split2(File, Ext, RevTop, RevTop2, File, FullExt, RevTop2);
string_split([Char | File], Ext, RevTop) ->
    string_split(File, Ext, [Char | RevTop]);
string_split([], _Ext, _RevTop) ->
    no_split.

string_split2([Char | File], [Char | Ext], RevBase, RevTop, SaveFile, SaveExt, SaveTop) ->
    string_split2(File, Ext, RevBase, [Char | RevTop], SaveFile, SaveExt, SaveTop);
string_split2(File, [], RevBase, RevTop, _SaveFile, _SaveExt, _SaveTop) ->
    {split, RevBase, RevTop, File};
string_split2(_, _Ext, _RevBase, _RevTop, SaveFile, SaveExt, SaveTop) ->
    string_split(SaveFile, SaveExt, SaveTop).

%% Parse list of ipv4 addresses 
ipv4_list([H | T]) ->
    IPV = if is_atom(H) -> ipv4_address(atom_to_list(H));
             is_list(H) -> ipv4_address(H);
             true -> {error,einal}
          end,
    case IPV of
        {ok,IP} -> [IP | ipv4_list(T)];
        _ -> ipv4_list(T)
    end;
ipv4_list([]) -> [].
    
%%
%% Parse Ipv4 address: d1.d2.d3.d4 (from inet_parse)
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4_address(Cs) ->
    case catch ipv4_addr(Cs, []) of
        {'EXIT',_} -> {error,einval};
        Addr -> {ok,Addr}
    end.

ipv4_addr([C | Cs], IP) when C >= $0, C =< $9 -> ipv4_addr(Cs, C-$0, IP).

ipv4_addr([$.|Cs], N, IP) when N < 256 -> ipv4_addr(Cs, [N|IP]);
ipv4_addr([C|Cs], N, IP) when C >= $0, C =< $9 ->
    ipv4_addr(Cs, N*10 + (C-$0), IP);
ipv4_addr([], D, [C,B,A]) when D < 256 -> {A,B,C,D}.

%% A simplified version of filename:absname/1
absname(Name) ->
    Name2 = normalize(Name, []),
    case pathtype(Name2) of
	absolute ->
	    Name2;
	relative ->
	    case prim_file:get_cwd() of
		{ok, Cwd} ->
		    Cwd ++ "/" ++ Name2;
		{error, _} -> 
		    Name2
	    end;
	volumerelative ->
	    case prim_file:get_cwd() of
		{ok, Cwd} ->
		    absname_vr(Name2, Cwd);
		{error, _} -> 
		    Name2
	    end
    end.

%% Assumes normalized name
absname_vr([$/ | NameRest], [Drive, $\: | _]) ->
    %% Absolute path on current drive.
    [Drive, $\: | NameRest];
absname_vr([Drive, $\: | NameRest], [Drive, $\: | _] = Cwd) ->
    %% Relative to current directory on current drive.
    Cwd ++ "/" ++ NameRest;
absname_vr([Drive, $\: | NameRest], _) ->
    %% Relative to current directory on another drive.
    case prim_file:get_cwd([Drive, $\:]) of
	{ok, DriveCwd}  ->
	    DriveCwd ++ "/" ++ NameRest;
	{error, _} ->
	    [Drive, $\:, $/] ++ NameRest
    end.

%% Assumes normalized name
pathtype(Name) when is_list(Name) -> 
    case erlang:system_info(os_type) of
	{unix, _}  -> 
	    unix_pathtype(Name);
	{win32, _} ->
	    win32_pathtype(Name);
	{vxworks, _} ->
	    case vxworks_first(Name) of
		{device, _Rest, _Dev} ->
		    absolute;
		_ ->
		    relative
	    end;
	{ose,_} ->
	    unix_pathtype(Name)
    end.

unix_pathtype(Name) ->
    case Name of
	[$/|_] ->
	    absolute;
	[List|Rest] when is_list(List) ->
	    unix_pathtype(List++Rest);
	[Atom|Rest] when is_atom(Atom) ->
	    atom_to_list(Atom)++Rest;
	_ ->
	    relative
    end.

win32_pathtype(Name) ->
    case Name of
	[List|Rest] when is_list(List) ->
	    win32_pathtype(List++Rest);
	[Atom|Rest] when is_atom(Atom) ->
	    win32_pathtype(atom_to_list(Atom)++Rest);
	[Char, List | Rest] when is_list(List) ->
	    win32_pathtype([Char | List++Rest]);
	[$/, $/|_] -> 
	    absolute;
	[$\\, $/|_] -> 
	    absolute;
	[$/, $\\|_] ->
	    absolute;
	[$\\, $\\|_] -> 
	    absolute;
	[$/|_] -> 
	    volumerelative;
	[$\\|_] -> 
	    volumerelative;
	[C1, C2, List | Rest] when is_list(List) ->
	    pathtype([C1, C2|List ++ Rest]);
	[_Letter, $:, $/|_] -> 
	    absolute;
	[_Letter, $:, $\\|_] ->
	    absolute;
	[_Letter, $:|_] -> 
	    volumerelative;
	_ -> 
	    relative
    end.

vxworks_first(Name) ->
    case Name of
	[] ->
	    {not_device, [], []};
	[$/ | T] ->
	    vxworks_first2(device, T, [$/]);
	[$\\ | T] ->
	    vxworks_first2(device, T, [$/]);
	[H | T] when is_list(H) ->
	    vxworks_first(H ++ T);
	[H | T] ->
	    vxworks_first2(not_device, T, [H])
    end.

vxworks_first2(Devicep, Name, FirstComp) ->
    case Name of 
	[] ->
    {Devicep, [], FirstComp};
	[$/ |T ] ->
	    {Devicep, [$/ | T], FirstComp};
	[$\\ | T] ->
	    {Devicep, [$/ | T], FirstComp};
	[$: | T]->
	    {device, T, [$: | FirstComp]};
	[H | T] when is_list(H) ->
	    vxworks_first2(Devicep, H ++ T, FirstComp);
	[H | T] ->
	    vxworks_first2(Devicep, T, [H | FirstComp])
    end.

normalize(Name, Acc) ->
    case Name of
	[List | Rest] when is_list(List) ->
	    normalize(List ++ Rest, Acc);
	[Atom | Rest] when is_atom(Atom) ->
	    normalize(atom_to_list(Atom) ++ Rest, Acc);
	[$\\ | Chars] ->
	    normalize(Chars, [$/ | Acc]);
	[Char | Chars] ->
	    normalize(Chars, [Char | Acc]);
	[] ->
	    reverse(Acc)
    end.
