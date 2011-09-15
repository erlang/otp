%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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

%% Various small utility functions
-export([start_http_server/1, start_http_server_ssl/1]).
-export([hostname/0]).
-export([connect_bin/3, connect_byte/3, send/3, close/2]).
-export([copy_file/3, copy_files/2, copy_dirs/2, del_dirs/1]).
-export([info/4, log/4, debug/4, print/4]).
-export([check_body/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([non_pc_tc_maybe_skip/4, os_based_skip/1]).

start_http_server(Conf) ->
    ?DEBUG("start_http_server -> entry with"
	   "~n   Conf: ~p", [Conf]),
    inets_ensure_loaded(), 
    inets_set_env(services, [{httpd, Conf}]),
    inets_ensure_started(),
    ok.


start_http_server_ssl(FileName) ->
    application:start(ssl),	       
    catch start_http_server(FileName).

inets_ensure_loaded() ->
    ensure_loaded(inets).

ensure_loaded(App) ->
    case application:load(App) of
	ok ->
	    ok;
	{error, {already_loaded, _}} ->
	    ok;
	LoadRes ->
	    ?LOG("start_http_server -> failed loading ~p: ~p", [App, LoadRes]),
	    tsf({failed_loading, LoadRes})
    end.


inets_set_env(Service, Config) ->
    app_set_env(inets, Service, Config).

app_set_env(App, Param, Value) ->
    case application:set_env(App, Param, Value) of
	ok ->
	    ?DEBUG("start_http_server -> env set", []),
	    ok;
	SetEnvRes ->
	    ?LOG("start_http_server -> failed set env for ~p: ~p", 
		 [App, SetEnvRes]),
	    exit({failed_set_env, App, SetEnvRes})
    end.

inets_ensure_started() ->
    ensure_app_started(inets).

ensure_app_started(App) ->
    case application:start(App) of
	ok ->
	    ?DEBUG("start_http_server -> ~p started", [App]),
	    ok;
	{error, {already_started, _}} ->
	    ok;
	StartRes ->
	    ?LOG("start_http_server -> failed starting ~p: ~p", 
		 [App, StartRes]),
	    exit({failed_starting, App, StartRes})
    end.


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
%%     io:format("~w:copy_dirs -> entry with"
%% 	      "~n   FromDirRoot: ~p"
%% 	      "~n   ToDirRoot:   ~p"
%% 	      "~n", [?MODULE, FromDirRoot, ToDirRoot]), 
    {ok, Files} = file:list_dir(FromDirRoot),
    lists:foreach(
      fun(FileOrDir) -> 
	      %% Check if it's a directory or a file
%% 	      io:format("~w:copy_dirs -> check ~p"
%% 			"~n", [?MODULE, FileOrDir]), 
	      case filelib:is_dir(filename:join(FromDirRoot, FileOrDir)) of
		  true ->
%% 		      io:format("~w:copy_dirs -> ~p is a directory"
%% 				"~n", [?MODULE, FileOrDir]), 
		      FromDir = filename:join([FromDirRoot, FileOrDir]),
		      ToDir   = filename:join([ToDirRoot, FileOrDir]),
		      ok      = file:make_dir(ToDir),
		      copy_dirs(FromDir, ToDir);
		  false ->
%% 		      io:format("~w:copy_dirs -> ~p is a file"
%% 				"~n", [?MODULE, FileOrDir]), 
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
		    test_server:format("Body ~p~n", [Body]),
		    test_server:fail(did_not_receive_whole_body);
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
    ssl:start(),
    %% Does not support ipv6 in old ssl 
    case ssl:connect(Host, Port, [binary, {packet,0}]) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    Error
    end;
connect_bin(ip_comm, Host, Port) ->
    Opts = [inet6, binary, {packet,0}],
    connect(ip_comm, Host, Port, Opts).
    

connect(ip_comm, Host, Port, Opts) ->
    test_server:format("gen_tcp:connect(~p, ~p, ~p) ~n", [Host, Port, Opts]),
    case gen_tcp:connect(Host,Port, Opts) of
	{ok, Socket} ->
	    test_server:format("connect success~n", []),
	    {ok, Socket};
	{error, nxdomain} ->
	    test_server:format("nxdomain opts: ~p~n", [Opts]),
	    connect(ip_comm, Host, Port, lists:delete(inet6, Opts));
	{error, eafnosupport}  ->
	    test_server:format("eafnosupport opts: ~p~n", [Opts]),
	    connect(ip_comm, Host, Port, lists:delete(inet6, Opts));
	{error, {enfile,_}} ->
	    test_server:format("Error enfile~n", []),
	    {error, enfile};
	Error ->
	    test_server:format("Unexpected error: "
			       "~n   Error: ~p"
			       "~nwhen"
			       "~n   Host:  ~p"
			       "~n   Port:  ~p"
			       "~n   Opts:  ~p"
			       "~n", [Error, Host, Port, Opts]),
	    Error
    end.

connect_byte(ip_comm, Host, Port) ->
    Opts = [inet6, {packet,0}],
    connect(ip_comm, Host, Port, Opts);

connect_byte(ssl, Host, Port) ->
    ssl:start(),
    %% Does not support ipv6 in old ssl 
    case ssl:connect(Host,Port,[{packet,0}]) of
	{ok,Socket} ->
	    {ok,Socket};
	{error,{enfile,_}} ->
	    {error, enfile};
	Error ->
	    Error
    end.   

send(ssl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ip_comm,Socket,Data) ->
    gen_tcp:send(Socket,Data).


close(ssl,Socket) ->
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

tsf(Reason) ->
    test_server:fail(Reason).
