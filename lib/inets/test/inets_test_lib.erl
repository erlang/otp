%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
-module(inets_test_lib).

-include("inets_test_lib.hrl").
-include_lib("inets/src/http_lib/http_internal.hrl").

%% Various small utility functions
-export([start_http_server/1, start_http_server/2]).
-export([start_http_server_ssl/1, start_http_server_ssl/2]).
-export([hostname/0]).
-export([connect_bin/3, connect_byte/3, send/3, close/2]).
-export([copy_file/3, copy_files/2, copy_dirs/2, del_dirs/1]).
-export([info/4, log/4, debug/4, print/4]).
-export([check_body/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([oscmd/1]).
-export([non_pc_tc_maybe_skip/4, os_based_skip/1]).
-export([flush/0]).
-export([start_node/1, stop_node/1]).

%% -- Misc os command and stuff

oscmd(Cmd) ->
  string:strip(os:cmd(Cmd), right, $\n).

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
%% HTTPD starter functions
%%

start_http_server(Conf) ->
    start_http_server(Conf, ?HTTP_DEFAULT_SSL_KIND).

start_http_server(Conf, essl = _SslTag) ->
    application:start(crypto), 
    do_start_http_server(Conf);
start_http_server(Conf, _SslTag) ->
    do_start_http_server(Conf).

do_start_http_server(Conf) ->
    tsp("start http server with "
	"~n   Conf: ~p"
	"~n", [Conf]),
    application:load(inets), 
    case application:set_env(inets, services, [{httpd, Conf}]) of
	ok ->
	    case application:start(inets) of
		ok ->
		    ok;
		Error1 ->
		    test_server:format("<ERROR> Failed starting application: "
				       "~n   Error: ~p"
				       "~n", [Error1]),
		    Error1
	    end;
	Error2 ->
	    test_server:format("<ERROR> Failed set application env: "
			       "~n   Error: ~p"
			       "~n", [Error2]),
	    Error2
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
    from($@, atom_to_list(node())).
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
    {ok, Files} = file:list_dir(FromDirRoot),
    lists:foreach(
      fun(FileOrDir) -> 
	      %% Check if it's a directory or a file
	      case filelib:is_dir(filename:join(FromDirRoot, FileOrDir)) of
		  true ->
		      FromDir = filename:join([FromDirRoot, FileOrDir]),
		      ToDir   = filename:join([ToDirRoot, FileOrDir]),
		      ok      = file:make_dir(ToDir),
		      copy_dirs(FromDir, ToDir);
		  false ->
		      copy_file(FileOrDir, FromDirRoot, ToDirRoot)
	      end
      end, Files).

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

connect_bin(ssl, Host, Port) ->
    connect(ssl, Host, Port, [binary, {packet,0}]);
connect_bin(ossl, Host, Port) ->
    connect(ssl, Host, Port, [{ssl_imp, old}, binary, {packet,0}]);
connect_bin(essl, Host, Port) ->
    connect(ssl, Host, Port, [{ssl_imp, new}, binary, {packet,0}, {reuseaddr, true}]);
connect_bin(ip_comm, Host, Port) ->
    Opts = [inet6, binary, {packet,0}],
    connect(ip_comm, Host, Port, Opts).

    
connect_byte(ssl, Host, Port) ->
    connect(ssl, Host, Port, [{packet,0}]);
connect_byte(ossl, Host, Port) ->
    connect(ssl, Host, Port, [{ssl_imp, old}, {packet,0}]);
connect_byte(essl, Host, Port) ->
    connect(ssl, Host, Port, [{ssl_imp, new}, {packet,0}]);
connect_byte(ip_comm, Host, Port) ->
    Opts = [inet6, {packet,0}],
    connect(ip_comm, Host, Port, Opts).


connect(ssl, Host, Port, Opts) ->
    ssl:start(),
    %% Does not support ipv6 in old ssl 
    case ssl:connect(Host, Port, Opts) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    Error
    end;
connect(ip_comm, Host, Port, Opts) ->
    case gen_tcp:connect(Host,Port, Opts) of
	{ok, Socket} ->
	    %% tsp("connect success"),
	    {ok, Socket};
	{error, nxdomain} ->
	    tsp("nxdomain opts: ~p", [Opts]),
	    connect(ip_comm, Host, Port, lists:delete(inet6, Opts));
	{error, eafnosupport}  ->
	    tsp("eafnosupport opts: ~p", [Opts]),
	    connect(ip_comm, Host, Port, lists:delete(inet6, Opts));
	{error, {enfile,_}} ->
	    tsp("Error enfile"),
	    {error, enfile};
	Error ->
	    tsp("Unexpected error: "
		"~n   Error: ~p"
		"~nwhen"
		"~n   Host:  ~p"
		"~n   Port:  ~p"
		"~n   Opts:  ~p"
		"~n", [Error, Host, Port, Opts]),
	    Error
    end.


send(ssl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ossl, Socket, Data) ->
    ssl:send(Socket, Data);
send(essl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ip_comm,Socket,Data) ->
    gen_tcp:send(Socket,Data).


close(ssl,Socket) ->
    catch ssl:close(Socket);
close(ossl,Socket) ->
    catch ssl:close(Socket);
close(essl,Socket) ->
    catch ssl:close(Socket);
close(ip_comm,Socket) ->
    catch gen_tcp:close(Socket).

millis() ->
    erlang:now().

millis_diff(A,B) ->
    T1 = (element(1,A)*1000000) + element(2,A) + (element(3,A)/1000000),
    T2 = (element(1,B)*1000000) + element(2,B) + (element(3,B)/1000000),
    T1 - T2.

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
    test_server:format("~p ~p:" ++ F ++ "~n", [self(), ?MODULE | A]).

tsf(Reason) ->
    test_server:fail(Reason).
