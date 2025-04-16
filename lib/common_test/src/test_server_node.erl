%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-module(test_server_node).
-moduledoc false.

%% Prior to OTP 26, maybe_expr used to require runtime support. As it's now
%% enabled by default, all modules are tagged with the feature even when they
%% don't use it. Therefore, we explicitly disable it until OTP 25 is out of
%% support.
-feature(maybe_expr, disable).
-compile(r25).

%% Test Controller interface
-export([is_release_available/1, find_release/1]).
-export([start_node/5, stop_node/1]).
-export([kill_nodes/0, nodedown/1]).
%% Internal export
-export([node_started/1]).

-include("test_server_internal.hrl").
-record(slave_info, {name,socket,client}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                  %%%
%%% All code in this module executes on the test_server_ctrl process %%%
%%% except for node_started/1  which execute on a new node.          %%%
%%%                                                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_release_available(Rel) when is_atom(Rel) ->
    is_release_available(atom_to_list(Rel));
is_release_available(Rel) ->
    case os:type() of
	{unix,_} ->
	    Erl = find_release(Rel),
	    case Erl of
		none -> false;
		_ -> filelib:is_regular(Erl)
	    end;
	_ ->
	    false
    end.

nodedown(Sock) ->
    Match = #slave_info{name='$1',socket=Sock,client='$2',_='_'},
    case ets:match(slave_tab,Match) of
	[[Node,_Client]] -> % Slave node died
	    gen_tcp:close(Sock),
	    ets:delete(slave_tab,Node),
	    slave_died;
	[] ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Start slave/peer nodes (initiated by test_server:start_node/5)
%%%
start_node(SlaveName, slave, Options, From, TI) when is_list(SlaveName) ->
    start_node_slave(list_to_atom(SlaveName), Options, From, TI);
start_node(SlaveName, slave, Options, From, TI) ->
    start_node_slave(SlaveName, Options, From, TI);
start_node(SlaveName, peer, Options, From, TI) when is_atom(SlaveName) ->
    start_node_peer(atom_to_list(SlaveName), Options, From, TI);
start_node(SlaveName, peer, Options, From, TI) ->
    start_node_peer(SlaveName, Options, From, TI);
start_node(_SlaveName, _Type, _Options, _From, _TI) ->
    not_implemented_yet.

%%
%% Peer nodes are always started on the same host as test_server_ctrl
%%
%% (Socket communication is used since in early days the test target
%% and the test server controller node could be on different hosts and
%% the target could not know the controller node via erlang
%% distribution)
%%
start_node_peer(SlaveName, OptList, From, TI) ->
    SuppliedArgs = start_node_get_option_value(args, OptList, []),
    Cleanup = start_node_get_option_value(cleanup, OptList, true),
    HostStr = test_server_sup:hoststr(),
    {ok,LSock} = gen_tcp:listen(0,[binary,
				   {reuseaddr,true},
				   {packet,2}]),
    {ok,WaitPort} = inet:port(LSock),
    NodeStarted = lists:concat([" -s ", ?MODULE, " node_started ",
				      HostStr, " ", WaitPort]),

    % Support for erl_crash_dump files..
    CrashDir = test_server_sup:crash_dump_dir(),
    CrashFile = filename:join([CrashDir,
			       "erl_crash_dump."++cast_to_list(SlaveName)]),
    CrashArgs = lists:concat([" -env ERL_CRASH_DUMP \"",CrashFile,"\" "]),
    FailOnError = start_node_get_option_value(fail_on_error, OptList, true),
    Prog0 = start_node_get_option_value(erl, OptList, default),
    {ClearAFlags, Prog1} = pick_erl_program(Prog0),
    Prog = quote_progname(Prog1),
    Args = 
	case string:find(SuppliedArgs,"-setcookie") of
	    nomatch ->
                "-setcookie " ++ TI#target_info.cookie ++ " " ++ SuppliedArgs;
	    _ ->
                SuppliedArgs
	end,
    Cmd = lists:concat([Prog,
			" -detached ",
			TI#target_info.naming, " ", SlaveName,
			NodeStarted,
			CrashArgs,
			" ", Args]),
    Opts = case {ClearAFlags, start_node_get_option_value(env, OptList, [])} of
	       {false, []} -> [];
	       {false, Env} -> [{env, Env}];
               {true, []} -> [{env, [{"ERL_AFLAGS", false}]}];
	       {true, Env} -> [{env, [{"ERL_AFLAGS", false} | Env]}]
	   end,
    %% peer is always started on localhost
    %%
    %% Bad environment can cause open port to fail. If this happens,
    %% we ignore it and let the testcase handle the situation...
    catch open_port({spawn, Cmd}, [stream|Opts]),

    Tmo = 60000 * test_server:timetrap_scale_factor(),
    
    case start_node_get_option_value(wait, OptList, true) of
	true ->
	    Ret = wait_for_node_started(LSock,Tmo,undefined,Cleanup,TI,self()),
	    case {Ret,FailOnError} of
		{{{ok, Node}, Warning},_} ->
		    gen_server:reply(From,{{ok,Node},HostStr,Cmd,[],Warning});
		{_,false} ->
		    gen_server:reply(From,{Ret, HostStr, Cmd});
		{_,true} ->
		    gen_server:reply(From,{fail,{Ret, HostStr, Cmd}})
	    end;
	false ->
	    Nodename = list_to_atom(SlaveName ++ "@" ++ HostStr),
	    I = "=== Not waiting for node",
	    gen_server:reply(From,{{ok, Nodename}, HostStr, Cmd, I, []}),
	    Self = self(),
	    spawn_link(wait_for_node_started_fun(LSock,Tmo,Cleanup,TI,Self)),
	    ok
    end.

-spec wait_for_node_started_fun(_, _, _, _, _) -> fun(() -> no_return()).
wait_for_node_started_fun(LSock, Tmo, Cleanup, TI, Self) ->
    fun() ->
            {{ok, _}, _} = wait_for_node_started(LSock,Tmo,undefined,
                                                 Cleanup,TI,Self),
            receive after infinity -> ok end
    end.

%%
%% Slave nodes are started on a remote host if
%% - the option remote is given when calling test_server:start_node/3
%%
start_node_slave(SlaveName, OptList, From, _TI) ->
    SuppliedArgs = start_node_get_option_value(args, OptList, []),
    Cleanup = start_node_get_option_value(cleanup, OptList, true),

    CrashDir = test_server_sup:crash_dump_dir(),
    CrashFile = filename:join([CrashDir,
			       "erl_crash_dump."++cast_to_list(SlaveName)]),
    CrashArgs = lists:concat([" -env ERL_CRASH_DUMP \"",CrashFile,"\" "]),
    Args = lists:concat([" ", SuppliedArgs, CrashArgs]),

    Prog0 = start_node_get_option_value(erl, OptList, default),
    {ClearAFlags, Prog} = pick_erl_program(Prog0),
    Ret = 
	case start_which_node(OptList) of
	    {error,Reason} -> {{error,Reason},undefined,undefined};
	    Host0 -> do_start_node_slave(Host0,SlaveName,Args,Prog,Cleanup,
                                         ClearAFlags)
	end,
    gen_server:reply(From,Ret).

%% Temporary suppression, to avoid a warning calling undocumented
%%  but deprecated function.
-compile([{nowarn_deprecated_function,[{slave,start,5}]}]).

do_start_node_slave(Host0, SlaveName, Args, Prog, Cleanup, ClearAFlags) ->
    Host =
	case Host0 of
	    local -> test_server_sup:hoststr();
	    _ -> cast_to_list(Host0)
	end,
    Cmd = Prog ++ " " ++ Args,
    SavedAFlags = save_clear_aflags(ClearAFlags),
    Res = case slave:start(Host, SlaveName, Args, no_link, Prog) of
              {ok,Nodename} ->
                  case Cleanup of
                      true -> ets:insert(slave_tab,#slave_info{name=Nodename});
                      false -> ok
                  end,
                  {{ok,Nodename}, Host, Cmd, [], []};
              Ret ->
                  {Ret, Host, Cmd}
          end,
    restore_aflags(SavedAFlags),
    Res.

%%
%% This saving/clearing/restoring is not free from races, but since
%% there are no slave:start() that has an option for setting environment
%% this is the best we can do without improving the slave module. Since
%% the slave module is about to be replaced by the new peer module, we
%% do not bother...
%%
save_clear_aflags(false) ->
    false;
save_clear_aflags(true) ->
    case os:getenv("ERL_AFLAGS") of
        false ->
            false;
        ErlAFlags ->
            os:unsetenv("ERL_AFLAGS"),
            ErlAFlags
    end.

restore_aflags(false) ->
    ok;
restore_aflags(ErlAFlags) ->
    true = os:putenv("ERL_AFLAGS", ErlAFlags),
    ok.

wait_for_node_started(LSock,Timeout,Client,Cleanup,TI,CtrlPid) ->
    case gen_tcp:accept(LSock,Timeout) of
	{ok,Sock} -> 
	    gen_tcp:close(LSock),
	    receive 
		{tcp,Sock,Started0} when is_binary(Started0) ->
		    case unpack(Started0) of
			error ->
			    gen_tcp:close(Sock),
			    {error, connection_closed};
			{ok,Started} ->
			    Version = TI#target_info.otp_release,
			    VsnStr = TI#target_info.system_version,
			    {ok,Nodename, W} = 
				handle_start_node_return(Version,
							 VsnStr,
							 Started),
			    case Cleanup of
				true ->
				    ets:insert(slave_tab,#slave_info{name=Nodename,
								     socket=Sock,
								     client=Client});
				false -> ok
			    end,
			    ok = gen_tcp:controlling_process(Sock,CtrlPid),
			    test_server_ctrl:node_started(Nodename),
			    {{ok,Nodename},W}
		    end;
		{tcp_closed,Sock} ->
		    gen_tcp:close(Sock),
		    {error, connection_closed}
	    after Timeout ->
		    gen_tcp:close(Sock),
		    {error, timeout}
	    end;
	{error,Reason} -> 
	    gen_tcp:close(LSock),
	    {error, {no_connection,Reason}}
    end.



handle_start_node_return(Version,VsnStr,{started, Node, Version, VsnStr}) ->
    {ok, Node, []};
handle_start_node_return(Version,VsnStr,{started, Node, OVersion, OVsnStr}) ->
    Str = io_lib:format("WARNING: Started node "
			"reports different system "
			"version than current node! "
			"Current node version: ~p, ~p "
			"Started node version: ~p, ~p",
			[Version, VsnStr, 
			 OVersion, OVsnStr]),
    Str1 = lists:flatten(Str),
    {ok, Node, Str1}.


%%
%% This function executes on the new node
%%
node_started([Host,PortAtom]) ->
    %% Must spawn a new process because the boot process should not 
    %% hang forever!!
    spawn(node_started_fun(Host,PortAtom)).

-spec node_started_fun(_, _) -> fun(() -> no_return()).
node_started_fun(Host,PortAtom) ->
    fun() -> node_started(Host,PortAtom) end.

%% This process hangs forever, just waiting for the socket to be
%% closed and terminating the node
node_started(Host,PortAtom) ->
    {_, Version} = init:script_id(),
    VsnStr = erlang:system_info(system_version),
    Port = list_to_integer(atom_to_list(PortAtom)),
    case catch gen_tcp:connect(Host,Port, [binary, 
				     {reuseaddr,true}, 
				     {packet,2}]) of
	
	{ok,Sock} -> 
	    Started = term_to_binary({started, node(), Version, VsnStr}),
	    ok = gen_tcp:send(Sock, tag_trace_message(Started)),
	    receive _Anyting ->
		    gen_tcp:close(Sock),
		    erlang:halt()
	    end;
	_else ->
	    erlang:halt()
    end.


-compile({inline, [tag_trace_message/1]}).
-dialyzer({no_improper_lists, tag_trace_message/1}).
tag_trace_message(M) ->
    [1|M].

% start_which_node(Optlist) -> hostname
start_which_node(Optlist) ->
    case start_node_get_option_value(remote, Optlist) of
	undefined ->
	    local;
	true ->
	    case find_remote_host() of
		{error, Other} ->
		    {error, Other};
		RHost ->
		    RHost
	    end
    end.
 
find_remote_host() ->
    HostList=test_server_ctrl:get_hosts(),
    case lists:delete(test_server_sup:hoststr(), HostList) of
	[] ->
	    {error, no_remote_hosts};
	[RHost|_Rest] ->
	    RHost
    end.

start_node_get_option_value(Key, List) ->
    start_node_get_option_value(Key, List, undefined).

start_node_get_option_value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    Default
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% stop_node(Name) -> ok | {error,Reason}
%%
%% Clean up - test_server will stop this node
stop_node(Name) ->
    case ets:lookup(slave_tab,Name) of
	[#slave_info{}] ->
	    ets:delete(slave_tab,Name),
	    ok;
	[] -> 
	    {error, not_a_slavenode}
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% kill_nodes() -> ok
%%
%% Brutally kill all slavenodes that were not stopped by test_server
kill_nodes() ->
    case ets:match_object(slave_tab,'_') of
	[] -> [];
	List ->
	    lists:map(fun(SI) -> kill_node(SI) end, List)
    end.

kill_node(SI) ->
    Name = SI#slave_info.name,
    ets:delete(slave_tab,Name),
    case SI#slave_info.socket of
	undefined ->
	    catch rpc:call(Name,erlang,halt,[]);
	Sock ->
	    gen_tcp:close(Sock)
    end,
    Name.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% cast_to_list(X) -> string()
%%% X = list() | atom() | void()
%%% Returns a string representation of whatever was input

cast_to_list(X) when is_list(X) -> X;
cast_to_list(X) when is_atom(X) -> atom_to_list(X);
cast_to_list(X) -> lists:flatten(io_lib:format("~tw", [X])).


%%% L contains elements of the forms
%%%  {prog, String}
%%%  {release, Rel} where Rel = String | latest | previous
%%%  this
%%%
%%% First element of returned tuple answers the question
%%% "Do we need to clear ERL_AFLAGS?":
%%% When starting a node with a previous release, options in
%%% ERL_AFLAGS could prevent the node from starting. For example,
%%% if ERL_AFLAGS is set to "-emu_type lcnt", the node will only
%%% start if the previous release happens to also have a lock
%%% counter emulator installed (unlikely).
pick_erl_program(default) ->
    {false, ct:get_progname()};
pick_erl_program(L) ->
    P = random_element(L),
    case P of
	{prog, S} ->
	    {false, S};
	{release, S} ->
	    {true, find_release(S)};
	this ->
	    {false, ct:get_progname()}
    end.

%% This is an attempt to distinguish between spaces in the program
%% path and spaces that separate arguments. The program is quoted to
%% allow spaces in the path.
%%
%% Arguments could exist either if the executable is explicitly given
%% ({prog,String}) or if the -program switch to beam is used and
%% includes arguments (typically done by cerl in OTP test environment
%% in order to ensure that slave/peer nodes are started with the same
%% emulator and flags as the test node. The return from ct:get_progname()
%% could then typically be "/<full_path_to>/cerl -gcov").
quote_progname(Progname) ->
    do_quote_progname(string:lexemes(Progname," ")).

do_quote_progname([Prog]) ->
    "\""++Prog++"\"";
do_quote_progname([Prog,Arg|Args]) ->
    case os:find_executable(Prog) of
	false ->
	    do_quote_progname([Prog++" "++Arg | Args]);
	_ ->
	    %% this one has an executable - we assume the rest are arguments
	    "\""++Prog++"\""++
		lists:flatten(lists:map(fun(X) -> [" ",X] end, [Arg|Args]))
    end.

random_element(L) ->
    lists:nth(rand:uniform(length(L)), L).

otp_release_path(RelPath) ->
    filename:join(otp_release_root(), RelPath).

otp_release_root() ->
    case get(test_server_release_root) of
        undefined ->
            Root = os:getenv("TEST_SERVER_RELEASE_ROOT",
                             "/usr/local/otp/releases"),
            put(test_server_release_root, Root),
            Root;
        Cached ->
            Cached
    end.

find_release(latest) ->
    otp_release_path("latest/bin/erl");
find_release(previous) ->
    "kaka";
find_release(Rel) ->
    case find_release(os:type(), Rel) of
        none ->
            case find_release_path(Rel) of
                none ->
                    case string:take(Rel,"_",true) of
                        {Rel,[]} ->
                            none;
                        {RelNum,_} ->
                            find_release_path(RelNum)
                    end;
                Release ->
                    Release
            end;
        Else ->
            Else
    end.

find_release_path(Rel) ->
    Paths = string:lexemes(os:getenv("PATH"), ":"),
    find_release_path(Paths, Rel).
find_release_path([Path|T], Rel) ->
    case os:find_executable("erl", Path) of
        false ->
            find_release_path(T, Rel);
        ErlExec ->
            QuotedExec = "\""++ErlExec++"\"",
            Release = os:cmd(QuotedExec ++ " -noinput -eval 'io:format(\"~ts\", [erlang:system_info(otp_release)])' -s init stop"),
            case Release =:= Rel of
                true ->
                    %% Check is the release is a source tree release,
                    %% if so we should not use it.
                    case os:cmd(QuotedExec ++ " -noinput -eval 'io:format(\"~p\",[filelib:is_file(filename:join([code:root_dir(),\"OTP_VERSION\"]))]).' -s init stop") of
                        "true" ->
                            find_release_path(T, Rel);
                        "false" ->
                            ErlExec
                    end;
                false -> find_release_path(T, Rel)
            end
    end;
find_release_path([], _) ->
    none.

find_release({unix,sunos}, Rel) ->
    case os:cmd("uname -p") of
	"sparc" ++ _ ->
            otp_release_path("otp_beam_solaris8_" ++ Rel ++ "/bin/erl");
	_ ->
	    none
    end;
find_release({unix,linux}, Rel) ->
    Candidates = find_rel_linux(Rel),
    case lists:dropwhile(fun(N) ->
				 not filelib:is_regular(N)
			 end, Candidates) of
	[] -> none;
	[Erl|_] -> Erl
    end;
find_release(_, _) -> none.

find_rel_linux(Rel) ->
    try
        case ubuntu_release() of
            none -> none;
            [UbuntuRel |_] -> throw(find_rel_ubuntu(Rel, UbuntuRel))
        end,
        case suse_release() of
            none -> none;
            SuseRel -> throw(find_rel_suse(Rel, SuseRel))
        end,
        []
    catch
        throw:Result ->
            Result
    end.

find_rel_suse(Rel, SuseRel) ->
    Root = otp_release_path("sles"),
    case SuseRel of
	"11" ->
	    %% Try both SuSE 11, SuSE 10 and SuSe 9 in that order.
	    find_rel_suse_1(Rel, Root++"11") ++
		find_rel_suse_1(Rel, Root++"10") ++
		find_rel_suse_1(Rel, Root++"9");
	"10" ->
	    %% Try both SuSE 10 and SuSe 9 in that order.
	    find_rel_suse_1(Rel, Root++"10") ++
		find_rel_suse_1(Rel, Root++"9");
	"9" ->
	    find_rel_suse_1(Rel, Root++"9");
	_ ->
	    []
    end.

find_rel_suse_1(Rel, RootWc) ->
    case erlang:system_info(wordsize) of
	4 ->
	    find_rel_suse_2(Rel, RootWc++"_32");
	8 ->
	    find_rel_suse_2(Rel, RootWc++"_64") ++
		find_rel_suse_2(Rel, RootWc++"_32")
    end.

find_rel_suse_2(Rel, RootWc) ->
    RelDir = filename:dirname(RootWc),
    Pat = filename:basename(RootWc ++ "_" ++ Rel) ++ ".*",
    case file:list_dir(RelDir) of
	{ok,Dirs} ->
	    case lists:filter(fun(Dir) ->
				      case re:run(Dir, Pat, [unicode]) of
					  nomatch -> false;
					  _       -> true
				      end
			      end, Dirs) of
		[] ->
		    [];
		[R|_] ->
		    [filename:join([RelDir,R,"bin","erl"])]
	    end;
	_ ->
	    []
    end.

%% suse_release() -> VersionString | none.
%%  Return the major SuSE version number for this platform or
%%  'none' if this is not a SuSE platform.
suse_release() ->
    case file:open("/etc/SuSE-release", [read]) of
	{ok,Fd} ->
	    try
		suse_release(Fd)
	    after
		file:close(Fd)
	    end;
	{error,_} -> none
    end.

suse_release(Fd) ->
    case io:get_line(Fd, '') of
	eof -> none;
	Line when is_list(Line) ->
	    case re:run(Line, "^VERSION\\s*=\\s*(\\d+)\s*",
			[{capture,all_but_first,list}]) of
		nomatch ->
		    suse_release(Fd);
		{match,[Version]} ->
		    Version
	    end
    end.

find_rel_ubuntu(_Rel, UbuntuRel) when is_integer(UbuntuRel), UbuntuRel < 16 ->
    [];
find_rel_ubuntu(_Rel, UbuntuRel) when is_integer(UbuntuRel), UbuntuRel < 20 ->
    find_rel_ubuntu(_Rel, 16, UbuntuRel);
find_rel_ubuntu(_Rel, UbuntuRel) when is_integer(UbuntuRel) ->
    find_rel_ubuntu(_Rel, 20, UbuntuRel).

find_rel_ubuntu(Rel, MinUbuntuRel, MaxUbuntuRel) when
      is_integer(MinUbuntuRel), is_integer(MaxUbuntuRel) ->
    Root = otp_release_path("ubuntu"),
    lists:foldl(fun (ChkUbuntuRel, Acc) ->
                        find_rel_ubuntu_aux1(Rel, Root++integer_to_list(ChkUbuntuRel))
                            ++ Acc
                end,
                [],
                lists:seq(MinUbuntuRel, MaxUbuntuRel)).

find_rel_ubuntu_aux1(Rel, RootWc) ->
    case erlang:system_info(wordsize) of
	4 ->
	    find_rel_ubuntu_aux2(Rel, RootWc++"_32");
	8 ->
	    find_rel_ubuntu_aux2(Rel, RootWc++"_64") ++
		find_rel_ubuntu_aux2(Rel, RootWc++"_32")
    end.

find_rel_ubuntu_aux2(Rel, RootWc) ->
    RelDir = filename:dirname(RootWc),
    Pat = filename:basename(RootWc ++ "_" ++ Rel) ++ ".*",
    case file:list_dir(RelDir) of
	{ok,Dirs} ->
	    case lists:filter(fun(Dir) ->
				      case re:run(Dir, Pat, [unicode]) of
					  nomatch -> false;
					  _       -> true
				      end
			      end, Dirs) of
		[] ->
		    [];
		[R|_] ->
		    [filename:join([RelDir,R,"bin","erl"])]
	    end;
	_ ->
	    []
    end.

ubuntu_release() ->
    case file:open("/etc/lsb-release", [read]) of
	{ok,Fd} ->
	    try
		ubuntu_release(Fd, undefined, undefined)
	    after
		file:close(Fd)
	    end;
	{error,_} -> none
    end.

ubuntu_release(_Fd, DistrId, Rel) when DistrId /= undefined,
                                      Rel /= undefined ->
    Ubuntu = case DistrId of
                 "Ubuntu" -> true;
                 "ubuntu" -> true;
                 _ -> false
             end,
    case Ubuntu of
        false -> none;
        true -> Rel
    end;
ubuntu_release(Fd, DistroId, Rel) ->
    case io:get_line(Fd, '') of
	eof ->
            none;
	Line when is_list(Line) ->
	    case re:run(Line, "^DISTRIB_ID=(\\w+)$",
                        [{capture,all_but_first,list}]) of
		{match,[NewDistroId]} ->
                    ubuntu_release(Fd, NewDistroId, Rel);
                nomatch ->
                    case re:run(Line, "^DISTRIB_RELEASE=(\\d+(?:\\.\\d+)*)$",
                                [{capture,all_but_first,list}]) of
                        {match,[RelList]} ->
                            NewRel = lists:map(fun (N) ->
                                                       list_to_integer(N)
                                               end,
                                               string:lexemes(RelList, ".")),
                            ubuntu_release(Fd, DistroId, NewRel);
                        nomatch ->
                            ubuntu_release(Fd, DistroId, Rel)
                    end
            end
    end.


unpack(Bin) ->
    {One,Term} = split_binary(Bin, 1),
    case binary_to_list(One) of
	[1] ->
	    case catch {ok,binary_to_term(Term)} of
		{'EXIT',_} -> error;
		{ok,_}=Res -> Res
	    end;
	_ -> error
    end.

