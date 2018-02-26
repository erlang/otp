%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2015. All Rights Reserved.
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
-module(inets_test_lib).

-include("inets_test_lib.hrl").
-include_lib("inets/src/http_lib/http_internal.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%% -- Misc os command and stuff

has_ipv6_support() ->
    tsp("has_ipv6_support -> no ipv6_hosts config"),
    {ok, Hostname} = inet:gethostname(),
    case inet:getaddrs(Hostname, inet6) of
	{ok, [Addr|_]} when is_tuple(Addr) andalso 
			    (element(1, Addr) =/= 0) ->
	    %% We actually need to test that the addr can be used, 
	    %% this is done by attempting to create a (tcp) 
	    %% listen socket
	    tsp("has_ipv6_support -> check Addr: ~p", [Addr]),
	    case (catch gen_tcp:listen(0, [inet6, {ip, Addr}])) of
		{ok, LSock} ->
		    tsp("has_ipv6_support -> we are ipv6 host"),
		    gen_tcp:close(LSock),
		    {ok, Addr};
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.
    
has_ipv6_support(Config) ->
    case lists:keysearch(ipv6_hosts, 1, Config) of
	false ->
	    %% Do a basic check to se if 
	    %% our own host has a working IPv6 address...
	    has_ipv6_support();

	{value, {_, Hosts}} when is_list(Hosts) ->
	    %% Check if our host is in the list of *known* IPv6 hosts
	    tsp("has_ipv6_support -> Hosts: ~p", [Hosts]),
	    {ok, Hostname} = inet:gethostname(),
	    case lists:member(list_to_atom(Hostname), Hosts) of
		true ->
		    tsp("has_ipv6_support -> we are known ipv6 host"),
		    {ok, [Addr|_]} = inet:getaddrs(Hostname, inet6),
		    {ok, Addr};
		false ->
		    undefined
	    end;
	
	_ ->
	    undefined

    end.

oscmd(Cmd) ->
  string:strip(os:cmd(Cmd), right, $\n).


print_system_info([]) ->
    do_print_system_info("System Info");
print_system_info(Prefix) when is_list(Prefix) ->
    NewPrefix = lists:flatten(io_lib:format("~s: System Info", [Prefix])), 
    do_print_system_info(NewPrefix).

do_print_system_info(Prefix) ->
    tsp("~s => "
	"~n"
	"~n   OS Type:            ~p"
	"~n   OS version:         ~p"
	"~n   Sys Arch:           ~p"
	"~n   CPU Topology:       ~p"
	"~n   Num logical procs:  ~p"
	"~n   SMP support:        ~p"
	"~n   Num schedulers:     ~p"
	"~n   Scheduler bindings: ~p"
	"~n   Wordsize:           ~p"
	"~n~n", [Prefix, 
		 os:type(), os:version(), 
		 erlang:system_info(system_architecture),
		 erlang:system_info(cpu_topology),
		 erlang:system_info(logical_processors),
		 erlang:system_info(smp_support),
		 erlang:system_info(schedulers),
		 erlang:system_info(scheduler_bindings),
		 erlang:system_info(wordsize)]),
    ok.
    
    
run_on_windows(Fun) ->
    run_on_os(windows, Fun).

run_on_os(windows, Fun) ->
    case os:type() of
	{win32, _} ->
	    Fun();
	_ ->
	    ok
    end.
	    
    
%% -- Misc node operation wrapper functions --

start_node(Name) ->
    Pa   = filename:dirname(code:which(?MODULE)),
    Args = case init:get_argument('CC_TEST') of
               {ok, [[]]} ->
                   " -pa /clearcase/otp/libraries/snmp/ebin ";
               {ok, [[Path]]} ->
                   " -pa " ++ Path;
               error ->
                      ""
              end,
    A = Args ++ " -pa " ++ Pa,
    Opts = [{cleanup,false}, {args, A}],
    case (catch test_server:start_node(Name, slave, Opts)) of
        {ok, Node} ->
            Node;
        Else ->
            exit({failed_starting_node, Name, Else})
    end.

stop_node(Node) ->
    rpc:cast(Node, erlang, halt, []),
    await_stopped(Node, 5).

await_stopped(_, 0) ->
    ok;
await_stopped(Node, N) ->
    Nodes = erlang:nodes(),
    case lists:member(Node, Nodes) of
        true ->
            sleep(1000),
            await_stopped(Node, N-1);
        false ->
            ok
    end.


%% ----------------------------------------------------------------
%% Ensure apps are started
%% This to ensure we dont attempt to run teatcases on platforms 
%% where there is no working ssl app.

ensure_started([]) ->
    ok;
ensure_started([App|Apps]) ->
    ensure_started(App),
    ensure_started(Apps);
ensure_started(crypto = App) ->
    %% We have to treat crypto in this special way because 
    %% only this function ensures that the NIF lib is actually
    %% loaded. And only by loading that lib can we know if it 
    %% is even possible to run crypto.
    do_ensure_started(App, fun() -> crypto:start() end);
ensure_started(App) when is_atom(App) ->
    do_ensure_started(App, fun() -> application:start(App) end).

do_ensure_started(App, Start) when is_function(Start) ->
    case (catch Start()) of
	ok ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	Error ->
	    throw({error, {failed_starting, App, Error}})
    end.


ensure_loaded(App) ->
    case application:load(App) of
	ok ->
	    ok;
	{error, {already_loaded,inets}} ->
	    ok;
	Error ->
	    Error
    end.



%% ----------------------------------------------------------------
%% HTTPD starter functions
%%

start_http_server(Conf) ->
    start_http_server(Conf, ?HTTP_DEFAULT_SSL_KIND).

start_http_server(Conf, essl = _SslTag) ->
    tsp("start_http_server(essl) -> try start crypto"),
    application:start(crypto), 
    tsp("start_http_server(essl) -> try start public_key"),
    application:start(public_key), 
    do_start_http_server(Conf);
start_http_server(Conf, SslTag) ->
    tsp("start_http_server(~w) -> entry", [SslTag]),
    do_start_http_server(Conf).

do_start_http_server(Conf) ->
    tsp("do_start_http_server -> entry with"
	"~n   Conf: ~p"
	"~n", [Conf]),
    tsp("do_start_http_server -> load inets"),
    case ensure_loaded(inets) of
	ok ->
	    tsp("do_start_http_server -> inets loaded - now set_env for httpd"),
	    case application:set_env(inets, services, [{httpd, Conf}]) of
		ok ->
		    tsp("do_start_http_server -> "
			"httpd conf stored in inets app env"),
		    case (catch application:start(inets)) of
			ok ->
			    tsp("do_start_http_server -> inets started"),
			    ok;
			Error1 ->
			    tsp("<ERROR> Failed starting application: "
				"~n   Error1: ~p", [Error1]),
			    tsf({failed_starting_inets, Error1})
		    end;
		Error2 ->
		    tsp("<ERROR> Failed set application env: "
			"~n   Error: ~p", [Error2]),
		    tsf({failed_set_env, Error2})
	    end;
	{error, Reason} ->
	    tsp("do_start_http_server -> failed loading inets"
		"~n   Reason: ~p", [Reason]),
	    tsf({failed_loading_inets, Reason})
    end.
	    
start_http_server_ssl(FileName) ->
    start_http_server_ssl(FileName, ?HTTP_DEFAULT_SSL_KIND).

start_http_server_ssl(FileName, essl = _SslTag) ->
    application:start(crypto), 
    do_start_http_server_ssl(FileName);
start_http_server_ssl(FileName, _SslTag) ->
    do_start_http_server_ssl(FileName).

do_start_http_server_ssl(FileName) ->
    tsp("start (ssl) http server with "
	"~n   FileName: ~p"
	"~n", [FileName]),
    application:start(ssl),	       
    catch do_start_http_server(FileName).



%% ----------------------------------------------------------------------
%% print functions
%%

info(F, A, Mod, Line) ->
    print("INF ", F, A, Mod, Line).

log(F, A, Mod, Line) ->
    print("LOG ", F, A, Mod, Line).

debug(F, A, Mod, Line) ->
    print("DBG ", F, A, Mod, Line).

print(P, F, A, Mod, Line) ->
    io:format("~s[~p:~p:~p] : " ++ F ++ "~n", [P, self(), Mod, Line| A]).

print(F, A, Mod, Line) ->
    print("", F, A, Mod, Line).

hostname() ->
   {ok, Name} = inet:gethostname(),
    Name.

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].


copy_file(File, From, To) ->
    file:copy(filename:join(From, File), filename:join(To, File)).

copy_files(FromDir, ToDir) -> 
    {ok, Files} = file:list_dir(FromDir),
    lists:foreach(fun(File) -> 
			  FullPath = filename:join(FromDir, File),
			  case filelib:is_file(FullPath) of
			      true ->
				  file:copy(FullPath,
					    filename:join(ToDir, File));
			      false ->
				  ok
			  end
		  end, Files).


copy_dirs(FromDirRoot, ToDirRoot) ->
    case file:list_dir(FromDirRoot) of
	{ok, Files}  ->
	    lists:foreach(
	      fun(FileOrDir) -> 
		      %% Check if it's a directory or a file
		      case filelib:is_dir(filename:join(FromDirRoot, 
							FileOrDir)) of
			  true ->
			      FromDir = filename:join([FromDirRoot, FileOrDir]),
			      ToDir   = filename:join([ToDirRoot, FileOrDir]),
			      case file:make_dir(ToDir) of
				  ok ->
				      copy_dirs(FromDir, ToDir);
				  {error, Reason} ->
				      tsp("<ERROR> Failed creating directory: "
					  "~n   ToDir:  ~p"
					  "~n   Reason: ~p"
					  "~nwhen"
					  "~n   ToDirRoot:           ~p"
					  "~n   ToDirRoot file info: ~p", 
					  [ToDir, 
					   Reason, 
					   ToDirRoot, 
					   file:read_file_info(ToDirRoot)]),
				      tsf({failed_copy_dir, ToDir, Reason})
			      end;
			  false ->
			      copy_file(FileOrDir, FromDirRoot, ToDirRoot)
		      end
	      end, Files);
	{error, Reason} ->
	    tsp("<ERROR> Failed get directory file list: "
		"~n   FromDirRoot: ~p"
		"~n   Reason:      ~p"
		"~nwhen"
		"~n   FromDirRoot file info: ~p", 
		[FromDirRoot, 
		 Reason, 
		 file:read_file_info(FromDirRoot)]),
	    tsf({failed_list_dir, FromDirRoot, Reason})
    end.
	    
		

del_dirs(Dir) ->
    case file:list_dir(Dir) of
	{ok, []} ->
	    file:del_dir(Dir);
	{ok, Files} ->
	    lists:foreach(fun(File) ->
				  FullPath = filename:join(Dir,File),
				  case filelib:is_dir(FullPath) of
				      true ->
					  del_dirs(FullPath),
					  file:del_dir(FullPath);	       
				      false ->
					  file:delete(FullPath)
				  end 
			  end, Files);
	_ ->
	    ok
    end.

check_body(Body) ->
    case string:rstr(Body, "</html>") of
	0 ->
	    case string:rstr(Body, "</HTML>") of
		0 ->
		    tsp("Body ~p", [Body]),
		    tsf(did_not_receive_whole_body);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

%% ----------------------------------------------------------------
%% Conditional skip of testcases
%%

non_pc_tc_maybe_skip(Config, Condition, File, Line)
  when is_list(Config) andalso is_function(Condition) ->
    %% Check if we shall skip the skip
    case os:getenv("TS_OS_BASED_SKIP") of
        "false" ->
            ok;
        _ ->
            case lists:keysearch(ts, 1, Config) of
                {value, {ts, inets}} ->
                    %% Always run the testcase if we are using our own
                    %% test-server...
                    ok;
                _ ->
                    case (catch Condition()) of
                        true ->
                            skip(non_pc_testcase, File, Line);
                        _ ->
                            ok
                    end
            end
    end.


os_based_skip(any) ->
    true;
os_based_skip(Skippable) when is_list(Skippable) ->
    {OsFam, OsName} =
        case os:type() of
            {_Fam, _Name} = FamAndName ->
                FamAndName;
            Fam ->
                {Fam, undefined}
        end,
    case lists:member(OsFam, Skippable) of
        true ->
            true;
        false ->
            case lists:keysearch(OsFam, 1, Skippable) of
                {value, {OsFam, OsName}} ->
                    true;
                {value, {OsFam, OsNames}} when is_list(OsNames) ->
                    lists:member(OsName, OsNames);
                _ ->
                    false
            end
    end;
os_based_skip(_) ->
    false.


%% ----------------------------------------------------------------------
%% Socket functions:
%% open(SocketType, Host, Port) -> {ok, Socket} | {error, Reason}
%% SocketType -> ssl | ip_comm
%% Host       -> atom() | string() | {A, B, C, D} 
%% Port       -> integer()

connect_bin(SockType, Host, Port) ->
    connect_bin(SockType, Host, Port, []).

connect_bin(ssl, Host, Port, Opts0) ->
    Opts = [binary, {packet,0} | Opts0], 
    connect(ssl, Host, Port, Opts);
connect_bin(essl, Host, Port, Opts0) ->
    Opts = [{ssl_imp, new}, binary, {packet,0}| Opts0], 
    connect(ssl, Host, Port, Opts);
connect_bin(ip_comm, Host, Port, Opts0) ->
    Opts = [binary, {packet, 0} | Opts0],
    connect(ip_comm, Host, Port, Opts);
connect_bin(Type, Host, Port, Opts) ->
    connect(Type, Host, Port, Opts).

connect_byte(SockType, Host, Port) ->
    connect_byte(SockType, Host, Port, []).
    
connect_byte(ssl, Host, Port, Opts0) ->
    Opts = [{packet,0} | Opts0], 
    connect(ssl, Host, Port, Opts);
connect_byte(essl, Host, Port, Opts0) ->
    Opts = [{ssl_imp, new}, {packet,0} | Opts0], 
    connect(ssl, Host, Port, Opts);
connect_byte(ip_comm, Host, Port, Opts0) ->
    Opts = [{packet,0} | Opts0],
    connect(ip_comm, Host, Port, Opts);
connect_byte(Type, Host, Port, Opts) ->
    connect(Type, Host, Port, Opts).

connect(ip_comm, Host, Port, Opts) ->
    gen_tcp:connect(Host, Port, Opts);
connect(ssl, Host, Port, Opts) ->
    ssl:connect(Host, Port, Opts);
connect(openssl_port, Host, Port, Opts) ->
    CaCertFile = proplists:get_value(cacertfile, Opts),
    Cmd = "openssl s_client -quiet -port " ++ integer_to_list(Port)  ++ " -host " ++ Host 
	++ " -CAfile " ++ CaCertFile,
    ct:log("openssl cmd: ~p~n", [Cmd]),
    OpensslPort =  open_port({spawn, Cmd}, [stderr_to_stdout]),
    read_junk(OpensslPort),
    {ok, OpensslPort}.

send(ssl, Socket, Data) ->
    ssl:send(Socket, Data);
send(essl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ip_comm,Socket,Data) ->
    gen_tcp:send(Socket,Data);
send(openssl_port, Port, Data) ->
    true = port_command(Port, Data),
    ok.
close(ssl,Socket) ->
    catch ssl:close(Socket);
close(essl,Socket) ->
    catch ssl:close(Socket);
close(ip_comm,Socket) ->
    catch gen_tcp:close(Socket);
close(openssl_port, Port) ->
    exit(Port, normal).


hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


sleep(infinity) ->
    receive
    after infinity ->
            ok
    end;
sleep(MSecs) ->
    receive
    after trunc(MSecs) ->
            ok
    end,
    ok.


skip(Reason, File, Line) ->
    exit({skipped, {Reason, File, Line}}).

fail(Reason, File, Line) ->
    String = lists:flatten(io_lib:format("Failure ~p(~p): ~p~n",
                                         [File, Line, Reason])),
    tsf(String).



flush() ->
    receive
        Msg ->
            [Msg | flush()]
    after 1000 ->
            []
    end.


tsp(F) ->
    tsp(F, []).
tsp(F, A) ->
    Timestamp = inets_lib:formated_timestamp(),
    ct:pal("*** ~s ~p ~p " ++ F ++ "~n", 
		       [Timestamp, node(), self() | A]).

tsf(Reason) ->
    ct:fail(Reason).

tss(Time) ->
    ct:sleep(Time).

timestamp() ->
    http_util:timestamp().

start_apps(Apps) ->
    lists:foreach(fun(App) ->
			  application:stop(App),
			  application:start(App)
		  end, Apps).
stop_apps(Apps) ->
    lists:foreach(fun(App) ->
			  application:stop(App)
		  end, Apps).

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
     rpc:call(Node, gen_tcp, close, [Socket]),
     Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.

read_junk(OpensslPort) ->
    receive
	{OpensslPort, _} ->
	    read_junk(OpensslPort)
    after 500 -> 
	    ok    
    end.
