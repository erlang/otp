%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2015. All Rights Reserved.
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

-module(ftp_suite_lib).


-include_lib("test_server/include/test_server.hrl").
-include_lib("test_server/include/test_server_line.hrl").
-include("inets_test_lib.hrl").

%% Test server specific exports
% -export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).


-record(progress, {
	  current = 0,
	  total
	 }).



-define(FTP_USER, "anonymous").
-define(FTP_PASS, passwd()).
-define(FTP_PORT, 21).

-define(BAD_HOST, "badhostname").
-define(BAD_USER, "baduser").
-define(BAD_DIR,  "baddirectory").

-ifdef(ftp_debug_client).
-define(ftp_open(Host, Flags), 
	do_ftp_open(Host, [{debug, debug}, 
			   {timeout, timer:seconds(15)} | Flags])).
-else.
-ifdef(ftp_trace_client).
-define(ftp_open(Host, Flags), 
	do_ftp_open(Host, [{debug, trace}, 
			   {timeout, timer:seconds(15)} | Flags])).
-else.
-define(ftp_open(Host, Flags), 
	do_ftp_open(Host, [{verbose, true}, 
			   {timeout, timer:seconds(15)} | Flags])).
-endif.
-endif.

%% -- Tickets --

tickets(doc) ->
     "Test cases for reported bugs";
tickets(suite) ->
     [ticket_6035].

%% --

ftpd_init(FtpdTag, Config) ->
    %% Get the host name(s) of FTP server
    Hosts = 
	case ct:get_config(ftpd_hosts) of
	    undefined ->
		ftpd_hosts(data_dir(Config));
	    H ->
		H
	end,
    p("ftpd_init -> "
      "~n   Hosts:   ~p"
      "~n   Config:  ~p"
      "~n   FtpdTag: ~p", [Hosts, Config, FtpdTag]),
    %% Get the first host that actually have a running FTP server
    case lists:keysearch(FtpdTag, 1, Hosts) of
	{value, {_, TagHosts}} when is_list(TagHosts) ->
	    inets:start(),
	    case (catch get_ftpd_host(TagHosts)) of
		{ok, Host} ->
		    inets:stop(),
		    [{ftp_remote_host, Host}|Config];
		_ ->
		    inets:stop(),
		    Reason = lists:flatten(
			       io_lib:format("Could not find a valid "
					     "FTP server for ~p (~p)", 
					     [FtpdTag, TagHosts])),
		    {skip, Reason}
	    end;
	_ ->
	    Reason = lists:flatten(
		       io_lib:format("No host(s) running FTPD server "
				     "for ~p", [FtpdTag])),
	    {skip, Reason}
    end.

ftpd_fin(Config) ->
    lists:keydelete(ftp_remote_host, 1, Config).

get_ftpd_host([]) ->    
    {error, no_host};
get_ftpd_host([Host|Hosts]) -> 
    p("get_ftpd_host -> entry with"
      "~n   Host: ~p"
      "~n", [Host]),
    case (catch ftp:open(Host, [{port, ?FTP_PORT}, {timeout, 20000}])) of
	{ok, Pid} ->
	    (catch ftp:close(Pid)),
	    {ok, Host};
	_ ->
	    get_ftpd_host(Hosts)
    end.


%%--------------------------------------------------------------------

dirty_select_ftpd_host(Config) ->
    Hosts = 
	case ct:get_config(ftpd_hosts) of
	    undefined ->
		ftpd_hosts(data_dir(Config));
	    H ->
		H
	end,
    dirty_select_ftpd_host2(Hosts).

dirty_select_ftpd_host2([]) ->
    throw({error, not_found});
dirty_select_ftpd_host2([{PlatformTag, Hosts} | PlatformHosts]) ->
    case dirty_select_ftpd_host3(Hosts) of
	none ->
	    dirty_select_ftpd_host2(PlatformHosts);
	{ok, Host} ->
	    {PlatformTag, Host}
    end.

dirty_select_ftpd_host3([]) ->
    none;
dirty_select_ftpd_host3([Host|Hosts]) when is_list(Host) ->
    case dirty_select_ftpd_host4(Host) of
	true ->
	    {ok, Host};
	false ->
	    dirty_select_ftpd_host3(Hosts)
    end;
dirty_select_ftpd_host3([_|Hosts]) ->
    dirty_select_ftpd_host3(Hosts).

%% This is a very simple and dirty test that there is a 
%% (FTP) deamon on the other end.
dirty_select_ftpd_host4(Host) ->
    Port    = 21, 
    IpFam   = inet, 
    Opts    = [IpFam, binary, {packet, 0}, {active, false}],
    Timeout = ?SECS(5),
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, Sock} ->
	    gen_tcp:close(Sock),
	    true;
        _Error ->
	    false
    end.


%%--------------------------------------------------------------------

test_filenames() ->
    {ok, Host} = inet:gethostname(),
    File = Host ++ "_ftp_test.txt",
    NewFile = "new_" ++ File,
    {File, NewFile}.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(Case, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(Case, Config) 
  when (Case =:= open) orelse 
       (Case =:= open_port) ->
    put(ftp_testcase, Case), 
    io:format(user, "~n~n*** INIT ~w:~w ***~n~n", [?MODULE, Case]),
    inets:start(),
    NewConfig = data_dir(Config),
    watch_dog(NewConfig);

init_per_testcase(Case, Config)  ->
    put(ftp_testcase, Case), 
    do_init_per_testcase(Case, Config).

do_init_per_testcase(Case, Config)  
   when (Case =:= passive_user) ->
    io:format(user, "~n~n*** INIT ~w:~w ***~n~n", [?MODULE,Case]),
    inets:start(),
    NewConfig = close_connection(watch_dog(Config)),
    Host = ftp_host(Config), 
    case (catch ?ftp_open(Host, [{mode, passive}])) of
	{ok, Pid} ->
	    [{ftp, Pid} | data_dir(NewConfig)];
	{skip, _} = SKIP ->
	    SKIP
    end;

do_init_per_testcase(Case, Config)  
  when (Case =:= active_user) ->
    io:format(user, "~n~n*** INIT ~w:~w ***~n~n", [?MODULE, Case]),
    inets:start(),
    NewConfig = close_connection(watch_dog(Config)),
    Host = ftp_host(Config), 
    case (catch ?ftp_open(Host, [{mode, active}])) of
	{ok, Pid} ->
	    [{ftp, Pid} | data_dir(NewConfig)];
	{skip, _} = SKIP ->
	    SKIP
    end;

do_init_per_testcase(Case, Config) 
  when (Case =:= progress_report_send) orelse 
       (Case =:= progress_report_recv) ->
    inets:start(),
    io:format(user, "~n~n*** INIT ~w:~w ***~n~n", [?MODULE, Case]),
    NewConfig = close_connection(watch_dog(Config)),
    Host = ftp_host(Config), 
    Opts = [{port,     ?FTP_PORT},
	    {verbose,  true},
	    {progress, {?MODULE, progress, #progress{}}}], 
    case ftp:open(Host, Opts) of
	{ok, Pid} ->
	    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
	    [{ftp, Pid} | data_dir(NewConfig)];
	{skip, _} = SKIP ->
	    SKIP
    end;

do_init_per_testcase(Case, Config) ->
    io:format(user,"~n~n*** INIT ~w:~w ***~n~n", [?MODULE, Case]),
    inets:start(),
    NewConfig = close_connection(watch_dog(Config)),
    Host = ftp_host(Config), 
    Opts1 = 
	if 
	    ((Case =:= passive_ip_v6_disabled) orelse
	     (Case =:= active_ip_v6_disabled)) ->
 		[{ipfamily, inet}];
 	    true ->
 		[]
 	end,
    Opts2 = 
	case string:tokens(atom_to_list(Case), [$_]) of
	    ["active" | _] ->
		[{mode, active}  | Opts1];
	    _ ->
		[{mode, passive} | Opts1]
	end,
    case (catch ?ftp_open(Host, Opts2)) of
	{ok, Pid} ->
	    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
	    [{ftp, Pid} | data_dir(NewConfig)];
	{skip, _} = SKIP ->
	    SKIP
    end.


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_, Config) ->  
    NewConfig = close_connection(Config),
    Dog = ?config(watchdog, NewConfig),
    inets:stop(),
    test_server:timetrap_cancel(Dog),
    ok.


%%-------------------------------------------------------------------------
%% Suites similar for all hosts.
%%-------------------------------------------------------------------------

passive(suite) ->
    [
     passive_user, 
     passive_pwd, 
     passive_cd, 
     passive_lcd,
     passive_ls, 
     passive_nlist, 
     passive_rename, 
     passive_delete, 
     passive_mkdir, 
     passive_send, 
     passive_send_bin, 
     passive_send_chunk, 
     passive_append, 
     passive_append_bin,
     passive_append_chunk, 
     passive_recv, 
     passive_recv_bin, 
     passive_recv_chunk, 
     passive_type, 
     passive_quote, 
     passive_ip_v6_disabled
    ].

active(suite) ->
    [
     active_user, 
     active_pwd, 
     active_cd,
     active_lcd, 
     active_ls, 
     active_nlist, 
     active_rename, 
     active_delete, 
     active_mkdir, 
     active_send, 
     active_send_bin, 
     active_send_chunk, 
     active_append, 
     active_append_bin, 
     active_append_chunk, 
     active_recv, 
     active_recv_bin, 
     active_recv_chunk, 
     active_type, 
     active_quote, 
     active_ip_v6_disabled
    ].



%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

open(doc) ->
    ["Open an ftp connection to a host and close the connection."
     "Also check that !-messages does not disturbe the connection"];
open(suite) ->
    [];
open(Config) when is_list(Config) ->
    Host = ftp_host(Config), 
    (catch tc_open(Host)).


tc_open(Host) ->
    p("tc_open -> entry with"
      "~n   Host: ~p", [Host]),
    {ok, Pid} = ?ftp_open(Host, []),
    ok = ftp:close(Pid),
    p("tc_open -> try (ok) open 1"),
    {ok, Pid1} = 
	ftp:open({option_list, [{host,Host}, 
				{port, ?FTP_PORT}, 
				{flags, [verbose]}, 
				{timeout, 30000}]}),
    ok = ftp:close(Pid1),
    
    p("tc_open -> try (fail) open 2"),
    {error, ehost} = 
	ftp:open({option_list, [{port, ?FTP_PORT}, {flags, [verbose]}]}),
    {ok, Pid2} = ftp:open(Host),
    ok = ftp:close(Pid2),
    
    p("tc_open -> try (ok) open 3"),
    {ok, NewHost} = inet:getaddr(Host, inet),
    {ok, Pid3} = ftp:open(NewHost),
    ftp:user(Pid3, ?FTP_USER, ?FTP_PASS),
    Pid3 ! foobar,
    test_server:sleep(5000),
    {message_queue_len, 0} = process_info(self(), message_queue_len),
    ["200" ++ _] = ftp:quote(Pid3, "NOOP"),
    ok = ftp:close(Pid3),

    %% Bad input that has default values are ignored and the defult 
    %% is used.
    p("tc_open -> try (ok) open 4"),
    {ok, Pid4} = 
	ftp:open({option_list, [{host,    Host}, 
				{port,    badarg}, 
				{flags,   [verbose]}, 
				{timeout, 30000}]}),
    test_server:sleep(100),
    ok = ftp:close(Pid4),

    p("tc_open -> try (ok) open 5"),
    {ok, Pid5} = 
	ftp:open({option_list, [{host,    Host}, 
				{port,    ?FTP_PORT}, 
				{flags,   [verbose]}, 
				{timeout, -42}]}),
    test_server:sleep(100),
    ok = ftp:close(Pid5),

    p("tc_open -> try (ok) open 6"),
    {ok, Pid6} = 
	ftp:open({option_list, [{host,  Host}, 
				{port,  ?FTP_PORT}, 
				{flags, [verbose]}, 
				{mode,  cool}]}),
    test_server:sleep(100),
    ok = ftp:close(Pid6),

    p("tc_open -> try (ok) open 7"),
    {ok, Pid7} = 
	ftp:open(Host, [{port, ?FTP_PORT}, {verbose, true}, {timeout, 30000}]),
    ok = ftp:close(Pid7),

    p("tc_open -> try (ok) open 8"),
    {ok, Pid8} = 
	ftp:open(Host, ?FTP_PORT),
    ok = ftp:close(Pid8),

    p("tc_open -> try (ok) open 9"),
    {ok, Pid9} = 
	ftp:open(Host, [{port,     ?FTP_PORT}, 
			{verbose,  true}, 
			{timeout,  30000}, 
			{dtimeout, -99}]),
    ok = ftp:close(Pid9),

    p("tc_open -> try (ok) open 10"),
    {ok, Pid10} = 
	ftp:open(Host, [{port,     ?FTP_PORT}, 
			{verbose,  true}, 
			{timeout,  30000}, 
			{dtimeout, "foobar"}]),
    ok = ftp:close(Pid10),

    p("tc_open -> try (ok) open 11"),
    {ok, Pid11} = 
	ftp:open(Host, [{port,     ?FTP_PORT}, 
			{verbose,  true}, 
			{timeout,  30000}, 
			{dtimeout, 1}]),
    ok = ftp:close(Pid11),

    p("tc_open -> done"),
    ok.

    
%%-------------------------------------------------------------------------

open_port(doc) ->
    ["Open an ftp connection to a host with given port number "
     "and close the connection."]; % See also OTP-3892 
open_port(suite) ->
    [];
open_port(Config) when is_list(Config) ->
    Host = ftp_host(Config), 
    {ok, Pid} = ftp:open(Host, [{port, ?FTP_PORT}]),
    ok = ftp:close(Pid),
    {error, ehost} = ftp:open(?BAD_HOST, []),
    ok.


%%-------------------------------------------------------------------------

passive_user(doc) ->
    ["Open an ftp connection to a host, and logon as anonymous ftp."];
passive_user(suite) ->
    [];
passive_user(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    p("Pid: ~p",[Pid]),
    do_user(Pid).


%%-------------------------------------------------------------------------
    
passive_pwd(doc) ->
    ["Test ftp:pwd/1 & ftp:lpwd/1"];
passive_pwd(suite) ->
    [];
passive_pwd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_pwd(Pid).


%%-------------------------------------------------------------------------

passive_cd(doc) ->
    ["Open an ftp connection, log on as anonymous ftp, and cd to the"
     "directory \"/pub\" and the to the  non-existent directory."];
passive_cd(suite) ->
    [];
passive_cd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_cd(Pid).


%%-------------------------------------------------------------------------

passive_lcd(doc) ->
    ["Test api function ftp:lcd/2"];
passive_lcd(suite) ->
    [];
passive_lcd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    PrivDir = ?config(priv_dir, Config),
    do_lcd(Pid, PrivDir).


%%-------------------------------------------------------------------------

passive_ls(doc) ->
    ["Open an ftp connection; ls the current directory, and the "
     "\"incoming\" directory. We assume that ls never fails, since "
     "it's output is meant to be read by humans. "];
passive_ls(suite) ->
    [];
passive_ls(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_ls(Pid).


%%-------------------------------------------------------------------------

passive_nlist(doc) ->
    ["Open an ftp connection; nlist the current directory, and the "
     "\"incoming\" directory. Nlist does not behave consistenly over "
     "operating systems. On some it is an error to have an empty "
     "directory."];
passive_nlist(suite) ->
    [];
passive_nlist(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    WildcardSupport = ?config(wildcard_support, Config),
    do_nlist(Pid, WildcardSupport).


%%-------------------------------------------------------------------------

passive_rename(doc) ->
    ["Transfer a file to the server, and rename it; then remove it."];
passive_rename(suite) ->
    [];
passive_rename(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_rename(Pid, Config).
    

%%-------------------------------------------------------------------------

passive_delete(doc) ->
    ["Transfer a file to the server, and then delete it"];
passive_delete(suite) ->
    [];
passive_delete(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_delete(Pid, Config).
   

%%-------------------------------------------------------------------------

passive_mkdir(doc) ->
    ["Make a remote directory, cd to it, go to parent directory, and "
     "remove the directory."];
passive_mkdir(suite) ->
    [];
passive_mkdir(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_mkdir(Pid).
   

%%-------------------------------------------------------------------------

passive_send(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; send the file; get a directory listing and check that "
     "the file is on the list;, delete the remote file; get another listing "
     "and check that the file is not on the list; close the session; "
     "delete the local file."];
passive_send(suite) ->
    [];
passive_send(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send(Pid, Config).


%%-------------------------------------------------------------------------

passive_append(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; append the file to a file at the remote side that not exits"
     "this will create the file at the remote side. Then it append the file "
     "again. When this is done it recive the remote file and control that"
     "the content is doubled in it.After that it will remove the files"];
passive_append(suite) ->
    [];
passive_append(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append(Pid, Config).
   

%%------------------------------------------------------------------------- 

passive_send_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send a binary; remove file; close the connection."];
passive_send_bin(suite) ->
    [];
passive_send_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send_bin(Pid, Config).

%%-------------------------------------------------------------------------

passive_append_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append  a binary twice; get the file and compare the content"
     "remove file; close the connection."];
passive_append_bin(suite) ->
    [];
passive_append_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append_bin(Pid, Config).


%%-------------------------------------------------------------------------    

passive_send_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send chunks; remove file; close the connection."];
passive_send_chunk(suite) ->
    [];
passive_send_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send_chunk(Pid, Config).


%%-------------------------------------------------------------------------

passive_append_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append chunks;control content remove file; close the connection."];
passive_append_chunk(suite) ->
    [];
passive_append_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append_chunk(Pid, Config).


%%-------------------------------------------------------------------------

passive_recv(doc) ->
    ["Create a local file and transfer it to the remote host into the "
     "the \"incoming\" directory, remove "
     "the local file. Then open a new connection; cd to \"incoming\", "
     "lcd to the private directory; receive the file; delete the "
     "remote file; close connection; check that received file is in "
     "the correct directory; cleanup." ];
passive_recv(suite) ->
    [];
passive_recv(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv(Pid, Config).
 

%%-------------------------------------------------------------------------

passive_recv_bin(doc) ->
    ["Send a binary to the remote host; and retreive "
     "the file; then remove the file."];
passive_recv_bin(suite) ->
    [];
passive_recv_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv_bin(Pid, Config).


%%-------------------------------------------------------------------------

passive_recv_chunk(doc) ->
    ["Send a binary to the remote host; Connect again, and retreive "
     "the file; then remove the file."];
passive_recv_chunk(suite) ->
    [];
passive_recv_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv_chunk(Pid, Config).


%%-------------------------------------------------------------------------

passive_type(doc) ->
    ["Test that we can change btween ASCCI and binary transfer mode"];
passive_type(suite) ->
    [];
passive_type(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_type(Pid).


%%-------------------------------------------------------------------------

passive_quote(doc) ->
    [""];
passive_quote(suite) ->
    [];
passive_quote(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_quote(Pid).


%%-------------------------------------------------------------------------

passive_ip_v6_disabled(doc) ->
    ["Test ipv4 command PASV"];
passive_ip_v6_disabled(suite) ->
    [];
passive_ip_v6_disabled(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send(Pid, Config).


%%-------------------------------------------------------------------------

active_user(doc) ->
    ["Open an ftp connection to a host, and logon as anonymous ftp."];
active_user(suite) ->
    [];
active_user(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_user(Pid).


%%-------------------------------------------------------------------------

active_pwd(doc) ->
    ["Test ftp:pwd/1 & ftp:lpwd/1"];
active_pwd(suite) ->
    [];
active_pwd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_pwd(Pid).


%%-------------------------------------------------------------------------

active_cd(doc) ->
    ["Open an ftp connection, log on as anonymous ftp, and cd to the"
     "directory \"/pub\" and to a non-existent directory."];
active_cd(suite) ->
    [];
active_cd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_cd(Pid).


%%-------------------------------------------------------------------------

active_lcd(doc) ->
    ["Test api function ftp:lcd/2"];
active_lcd(suite) ->
    [];
active_lcd(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    PrivDir = ?config(priv_dir, Config),
    do_lcd(Pid, PrivDir).


%%-------------------------------------------------------------------------

active_ls(doc) ->
    ["Open an ftp connection; ls the current directory, and the "
     "\"incoming\" directory. We assume that ls never fails, since "
     "it's output is meant to be read by humans. "];
active_ls(suite) ->
    [];
active_ls(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_ls(Pid).


%%-------------------------------------------------------------------------

active_nlist(doc) ->
    ["Open an ftp connection; nlist the current directory, and the "
     "\"incoming\" directory. Nlist does not behave consistenly over "
     "operating systems. On some it is an error to have an empty "
     "directory."];
active_nlist(suite) ->
    [];
active_nlist(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    WildcardSupport = ?config(wildcard_support, Config),
    do_nlist(Pid, WildcardSupport).


%%-------------------------------------------------------------------------

active_rename(doc) ->
    ["Transfer a file to the server, and rename it; then remove it."];
active_rename(suite) ->
    [];
active_rename(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_rename(Pid, Config).
    

%%-------------------------------------------------------------------------

active_delete(doc) ->
    ["Transfer a file to the server, and then delete it"];
active_delete(suite) ->
    [];
active_delete(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_delete(Pid, Config).
   

%%-------------------------------------------------------------------------

active_mkdir(doc) ->
    ["Make a remote directory, cd to it, go to parent directory, and "
     "remove the directory."];
active_mkdir(suite) ->
    [];
active_mkdir(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_mkdir(Pid).
   

%%-------------------------------------------------------------------------

active_send(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; send the file; get a directory listing and check that "
     "the file is on the list;, delete the remote file; get another listing "
     "and check that the file is not on the list; close the session; "
     "delete the local file."];
active_send(suite) ->
    [];
active_send(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send(Pid, Config).


%%-------------------------------------------------------------------------

active_append(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; append the file to a file at the remote side that not exits"
     "this will create the file at the remote side. Then it append the file "
     "again. When this is done it recive the remote file and control that"
     "the content is doubled in it.After that it will remove the files"];
active_append(suite) ->
    [];
active_append(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append(Pid, Config).
   

%%------------------------------------------------------------------------- 

active_send_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send a binary; remove file; close the connection."];
active_send_bin(suite) ->
    [];
active_send_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send_bin(Pid, Config).


%%-------------------------------------------------------------------------

active_append_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append  a binary twice; get the file and compare the content"
     "remove file; close the connection."];
active_append_bin(suite) ->
    [];
active_append_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append_bin(Pid, Config).


%%-------------------------------------------------------------------------    

active_send_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send chunks; remove file; close the connection."];
active_send_chunk(suite) ->
    [];
active_send_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send_chunk(Pid, Config).


%%-------------------------------------------------------------------------

active_append_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append chunks;control content remove file; close the connection."];
active_append_chunk(suite) ->
    [];
active_append_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_append_chunk(Pid, Config).


%%-------------------------------------------------------------------------

active_recv(doc) ->
    ["Create a local file and transfer it to the remote host into the "
     "the \"incoming\" directory, remove "
     "the local file. Then open a new connection; cd to \"incoming\", "
     "lcd to the private directory; receive the file; delete the "
     "remote file; close connection; check that received file is in "
     "the correct directory; cleanup." ];
active_recv(suite) ->
    [];
active_recv(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv(Pid, Config).
 

%%-------------------------------------------------------------------------

active_recv_bin(doc) ->
    ["Send a binary to the remote host; and retreive "
     "the file; then remove the file."];
active_recv_bin(suite) ->
    [];
active_recv_bin(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv_bin(Pid, Config).


%%-------------------------------------------------------------------------

active_recv_chunk(doc) ->
    ["Send a binary to the remote host; Connect again, and retreive "
     "the file; then remove the file."];
active_recv_chunk(suite) ->
    [];
active_recv_chunk(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_recv_chunk(Pid, Config).


%%-------------------------------------------------------------------------

active_type(doc) ->
    ["Test that we can change btween ASCCI and binary transfer mode"];
active_type(suite) ->
    [];
active_type(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_type(Pid).


%%-------------------------------------------------------------------------

active_quote(doc) ->
    [""];
active_quote(suite) ->
    [];
active_quote(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_quote(Pid).


%%-------------------------------------------------------------------------

active_ip_v6_disabled(doc) ->
    ["Test ipv4 command PORT"];
active_ip_v6_disabled(suite) ->
    [];
active_ip_v6_disabled(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    do_send(Pid, Config).


%%-------------------------------------------------------------------------

api_missuse(doc)->
    ["Test that behaviour of the ftp process if the api is abused"];
api_missuse(suite) -> [];
api_missuse(Config) when is_list(Config) ->
    p("api_missuse -> entry"),
    Flag =  process_flag(trap_exit, true),
    Pid = ?config(ftp, Config),
    Host = ftp_host(Config), 
    
    %% Serious programming fault, connetion will be shut down 
    p("api_missuse -> verify bad call termination (~p)", [Pid]),
    case (catch gen_server:call(Pid, {self(), foobar, 10}, infinity)) of
	{error, {connection_terminated, 'API_violation'}} ->
	    ok;
	Unexpected1 ->
	    exit({unexpected_result, Unexpected1})
    end,
    test_server:sleep(500),
    undefined = process_info(Pid, status),

    p("api_missuse -> start new client"),
    {ok, Pid2} =  ?ftp_open(Host, []),
    %% Serious programming fault, connetion will be shut down 
    p("api_missuse -> verify bad cast termination"),
    gen_server:cast(Pid2, {self(), foobar, 10}),
    test_server:sleep(500),
    undefined = process_info(Pid2, status),

    p("api_missuse -> start new client"),
    {ok, Pid3} =  ?ftp_open(Host, []),
    %% Could be an innocent misstake the connection lives. 
    p("api_missuse -> verify bad bang"),
    Pid3 ! foobar, 
    test_server:sleep(500),
    {status, _} = process_info(Pid3, status),
    process_flag(trap_exit, Flag),
    p("api_missuse -> done"),
    ok.


%%-------------------------------------------------------------------------

not_owner(doc) ->
    ["Test what happens if a process that not owns the connection tries "
    "to use it"];
not_owner(suite) ->
    [];
not_owner(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    OtherPid = spawn_link(?MODULE, not_owner, [Pid, self()]),
    
    receive
	{OtherPid, ok} ->
	    {ok, _} = ftp:pwd(Pid)
    end,
    ok.

not_owner(FtpPid, Pid) ->
    {error, not_connection_owner} = ftp:pwd(FtpPid),
    ftp:close(FtpPid),
    test_server:sleep(100),
    Pid ! {self(), ok}.


%%-------------------------------------------------------------------------


progress_report(doc) ->
    ["Solaris 8 sparc test the option progress."];
progress_report(suite) ->
    [progress_report_send, progress_report_recv].


%% -- 

progress_report_send(doc) ->
    ["Test the option progress for ftp:send/[2,3]"];
progress_report_send(suite) ->
    [];
progress_report_send(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    ReportPid = 
	spawn_link(?MODULE, progress_report_receiver_init, [self(), 1]),
    do_send(Pid, Config),
    receive
	{ReportPid, ok} ->
	    ok
    end.


%% -- 

progress_report_recv(doc) ->
    ["Test the option progress for ftp:recv/[2,3]"];
progress_report_recv(suite) ->
    [];
progress_report_recv(Config) when is_list(Config) ->
    Pid = ?config(ftp, Config),
    ReportPid = 
 	spawn_link(?MODULE, progress_report_receiver_init, [self(), 3]),
    do_recv(Pid, Config),
    receive
 	{ReportPid, ok} ->
 	    ok
    end,
    ok.

progress(#progress{} = Progress , _File, {file_size, Total}) ->
    progress_report_receiver ! start,
    Progress#progress{total = Total};
progress(#progress{total = Total, current = Current} 
	 = Progress, _File, {transfer_size, 0}) ->
    progress_report_receiver ! finish,
    case Total of
	unknown ->
	    ok;
	Current ->
	    ok;
	_  ->
	    test_server:fail({error, {progress, {total, Total},
				      {current, Current}}})
    end,
    Progress;
progress(#progress{current = Current} = Progress, _File, 
	 {transfer_size, Size}) ->
    progress_report_receiver ! update,
    Progress#progress{current = Current + Size}.

progress_report_receiver_init(Pid, N) ->
    register(progress_report_receiver, self()),
    receive
	start ->
	    ok
    end,
    progress_report_receiver_loop(Pid, N-1).

progress_report_receiver_loop(Pid, N) ->
      receive
	  update ->
	    progress_report_receiver_loop(Pid, N);
	  finish when N =:= 0 ->
	      Pid ! {self(), ok};
	  finish  ->
	      Pid ! {self(), ok},
	      receive
		  start ->
		      ok
	      end,
	      progress_report_receiver_loop(Pid, N-1)
      end.


%%-------------------------------------------------------------------------
%% Ticket test cases
%%-------------------------------------------------------------------------

ticket_6035(doc) -> ["Test that owning process that exits with reason "
		     "'shutdown' does not cause an error message."];
ticket_6035(suite) -> [];
ticket_6035(Config) ->
    p("ticket_6035 -> entry with"
      "~n   Config: ~p", [Config]),
    PrivDir = ?config(priv_dir, Config),
    LogFile = filename:join([PrivDir,"ticket_6035.log"]),
    try
	begin
	    p("ticket_6035 -> select ftpd host"),
	    Host = dirty_select_ftpd_host(Config), 
	    p("ticket_6035 -> ftpd host selected (~p) => now spawn ftp owner", [Host]),
	    Pid  = spawn(?MODULE, open_wait_6035, [Host, self()]),
	    p("ticket_6035 -> waiter spawned: ~p => now open error logfile (~p)", 
	      [Pid, LogFile]),
	    error_logger:logfile({open, LogFile}),
	    p("ticket_6035 -> error logfile open => now kill waiter process"),
	    true = kill_ftp_proc_6035(Pid, LogFile),
	    p("ticket_6035 -> waiter process killed => now close error logfile"),
	    error_logger:logfile(close),
	    p("ticket_6035 -> done", []),
	    ok
	end
    catch 
	throw:{error, not_found} ->
	    {skip, "No available FTP servers"}
    end.

kill_ftp_proc_6035(Pid, LogFile) ->
    p("kill_ftp_proc_6035 -> entry"),
    receive
	open ->
	    p("kill_ftp_proc_6035 -> received open => now issue shutdown"),
	    exit(Pid, shutdown),
	    kill_ftp_proc_6035(Pid, LogFile);
	{open_failed, Reason} ->
	    p("kill_ftp_proc_6035 -> received open_failed"
	      "~n   Reason: ~p", [Reason]),
	    exit({skip, {failed_openening_server_connection, Reason}})
    after
	5000 ->
	    p("kill_ftp_proc_6035 -> timeout"),
	    is_error_report_6035(LogFile)
    end.

open_wait_6035({Tag, FtpServer}, From) ->
    p("open_wait_6035 -> try connect to [~p] ~s for ~p", [Tag, FtpServer, From]),
    case ftp:open(FtpServer, [{timeout, timer:seconds(15)}]) of
	{ok, Pid} ->
	    p("open_wait_6035 -> connected (~p), now login", [Pid]),
	    LoginResult = ftp:user(Pid,"anonymous","kldjf"),
	    p("open_wait_6035 -> login result: ~p", [LoginResult]),
	    From ! open,
	    receive
		dummy -> 
		    p("open_wait_6035 -> received dummy"),
		    ok
	    after
		10000 ->
		    p("open_wait_6035 -> timeout"),
		    ok
	    end,
	    p("open_wait_6035 -> done(ok)"),
	    ok;
	{error, Reason} ->
	    p("open_wait_6035 -> open failed"
	      "~n   Reason: ~p", [Reason]),
	    From ! {open_failed, {Reason, FtpServer}},
	    p("open_wait_6035 -> done(error)"),
	    ok
    end.

is_error_report_6035(LogFile) ->
    p("is_error_report_6035 -> entry"),
    Res =
	case file:read_file(LogFile) of
	    {ok, Bin} ->
		Txt = binary_to_list(Bin), 
		p("is_error_report_6035 -> logfile read: ~n~p", [Txt]),
		read_log_6035(Txt);
	    _ ->
		false
	end,
    p("is_error_report_6035 -> logfile read result: "
      "~n   ~p", [Res]),
    %% file:delete(LogFile),
    Res.

read_log_6035("=ERROR REPORT===="++_Rest) ->
    p("read_log_6035 -> ERROR REPORT detected"),
    true;
read_log_6035([H|T]) ->
    p("read_log_6035 -> OTHER: "
      "~p", [H]),
    read_log_6035(T);
read_log_6035([]) ->
    p("read_log_6035 -> done"),
    false.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_user(Pid) ->
    {error, euser} = ftp:user(Pid, ?BAD_USER, ?FTP_PASS),
    {error, euser} = ftp:user(Pid, ?FTP_USER++"\r\nPASS "++?FTP_PASS, ?FTP_PASS),
    {error, euser} = ftp:user(Pid, ?FTP_USER, ?FTP_PASS++"\r\nCWD ."),
    ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ok.

do_pwd(Pid) ->
    {ok, "/"} = ftp:pwd(Pid),
    {ok, Path} = ftp:lpwd(Pid),
    {ok, Path} = file:get_cwd(),
    ok.

do_cd(Pid) ->
    ok = ftp:cd(Pid, "/pub"),
    {error, epath} = ftp:cd(Pid, ?BAD_DIR),
    {error, efnamena} = ftp:cd(Pid, "/pub\r\nCWD ."),
    ok.

do_lcd(Pid, Dir) ->
    ok = ftp:lcd(Pid, Dir),
    {error, epath} = ftp:lcd(Pid, ?BAD_DIR),
    ok.


do_ls(Pid) ->
    {ok, _} = ftp:ls(Pid),
    {ok, _} = ftp:ls(Pid, "incoming"),
    %% neither nlist nor ls operates on a directory
    %% they operate on a pathname, which *can* be a 
    %% directory, but can also be a filename or a group 
    %% of files (including wildcards).
    {ok, _} = ftp:ls(Pid, "incom*"),
    %% but \r\n can't be in the wildcard
    {error, efnamena} = ftp:ls(Pid, "incoming\r\nCWD ."),
    ok.

do_nlist(Pid, WildcardSupport) ->
    {ok, _} = ftp:nlist(Pid),
    {ok, _} = ftp:nlist(Pid, "incoming"),
    {error, efnamena} = ftp:ls(Pid, "incoming\r\nCWD ."),
    %% neither nlist nor ls operates on a directory
    %% they operate on a pathname, which *can* be a 
    %% directory, but can also be a filename or a group 
    %% of files (including wildcards).
    case WildcardSupport of
	true ->
	    {ok, _} = ftp:nlist(Pid, "incom*"),
	    ok;
	_ ->
	    ok
    end.

do_rename(Pid, Config) ->
    PrivDir = ?config(priv_dir, Config),
    LFile  = ?config(file, Config),
    NewLFile  = ?config(new_file, Config),
    AbsLFile = filename:absname(LFile, PrivDir),
    Contents = "ftp_SUITE test ...",
    ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:lcd(Pid, PrivDir),
    ftp:delete(Pid, LFile),		% reset
    ftp:delete(Pid, NewLFile),		% reset
    ok = ftp:send(Pid, LFile), 
    {error, epath} = ftp:rename(Pid, NewLFile, LFile),
    {error, efnamena} = ftp:rename(Pid, NewLFile++"\r\nRNTO "++LFile++"\r\nRNFR "++NewLFile, LFile),
    {error, efnamena} = ftp:rename(Pid, NewLFile, LFile++"\r\nCWD ."),
    ok = ftp:rename(Pid, LFile, NewLFile),
    ftp:delete(Pid, LFile),		% cleanup
    ftp:delete(Pid, NewLFile),		% cleanup
    ok.

do_delete(Pid, Config) ->
    PrivDir = ?config(priv_dir, Config),
    LFile  = ?config(file, Config),
    AbsLFile = filename:absname(LFile, PrivDir),
    Contents = "ftp_SUITE test ...",
    ok = file:write_file(AbsLFile, list_to_binary(Contents)),    
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:lcd(Pid, PrivDir),
    ftp:delete(Pid,LFile),		% reset
    {error, efnamena} = ftp:delete(Pid,LFile++"\r\nCWD ."),
    ok = ftp:send(Pid, LFile),
    ok = ftp:delete(Pid,LFile),
    ok.

do_mkdir(Pid) ->
    NewDir = "earl_" ++
        integer_to_list(inets_time_compat:unique_integer([positive])),

    ok = ftp:cd(Pid, "incoming"),
    {ok, CurrDir} = ftp:pwd(Pid),
    {error, efnamena} = ftp:mkdir(Pid, NewDir++"\r\nCWD ."),
    {error, efnamena} = ftp:rmdir(Pid, NewDir++"\r\nCWD ."),
    ok = ftp:mkdir(Pid, NewDir),
    ok = ftp:cd(Pid, NewDir),
    ok = ftp:cd(Pid, CurrDir),
    ok = ftp:rmdir(Pid, NewDir),
    ok.

do_send(Pid, Config) ->
    PrivDir = ?config(priv_dir, Config),
    LFile  = ?config(file, Config),
    RFile = LFile ++ ".remote",
    AbsLFile = filename:absname(LFile, PrivDir),
    Contents = "ftp_SUITE test ...",
    ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:lcd(Pid, PrivDir),
    {error, efnamena} = ftp:send(Pid, LFile, RFile++"1\r\nCWD ."),
    ok = ftp:send(Pid, LFile, RFile),
    {ok, RFilesString} = ftp:nlist(Pid),
    RFiles = split(RFilesString),
    true = lists:member(RFile, RFiles),
    ok = ftp:delete(Pid, RFile),
    case ftp:nlist(Pid) of
	{error, epath} ->
	    ok;				% No files
	{ok, RFilesString1} ->
	    RFiles1 = split(RFilesString1),
	    false = lists:member(RFile, RFiles1)
    end,
    ok = file:delete(AbsLFile).

do_append(Pid, Config) ->
    PrivDir = ?config(priv_dir, Config),
    LFile  =  ?config(file, Config),
    RFile =  ?config(new_file, Config),
    AbsLFile = filename:absname(LFile, PrivDir),
    Contents = "ftp_SUITE test:appending\r\n",

    ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:lcd(Pid, PrivDir),

    %% remove files from earlier failed test case
    ftp:delete(Pid, RFile),
    ftp:delete(Pid, LFile),

    {error, efnamena} = ftp:append(Pid, LFile, RFile++"1\r\nCWD ."),
    ok = ftp:append(Pid, LFile, RFile),
    ok = ftp:append(Pid, LFile, RFile),
    ok = ftp:append(Pid, LFile),

    %% Control the contents of the file
    {ok, Bin1} = ftp:recv_bin(Pid, RFile),
    ok = ftp:delete(Pid, RFile),
    ok = file:delete(AbsLFile),
    ok = check_content(binary_to_list(Bin1), Contents, double),
    
    {ok, Bin2}  = ftp:recv_bin(Pid, LFile),
    ok = ftp:delete(Pid, LFile),
    ok = check_content(binary_to_list(Bin2), Contents, singel),
    ok.

do_send_bin(Pid, Config) ->
    File = ?config(file, Config),
    Contents = "ftp_SUITE test ...",
    Bin = list_to_binary(Contents),
    ok = ftp:cd(Pid, "incoming"),
    {error, enotbinary} = ftp:send_bin(Pid, Contents, File),
    {error, efnamena} = ftp:send_bin(Pid, Bin, File++"1\r\nCWD ."),
    ok = ftp:send_bin(Pid, Bin, File),
    {ok, RFilesString} = ftp:nlist(Pid),
    RFiles = split(RFilesString),
    true = lists:member(File, RFiles),
    ok = ftp:delete(Pid, File),
    ok.

do_append_bin(Pid, Config) ->
    File = ?config(file, Config),
    Contents = "ftp_SUITE test ...",
    Bin = list_to_binary(Contents),
    ok = ftp:cd(Pid, "incoming"),
    {error, enotbinary} = ftp:append_bin(Pid, Contents, File),
    {error, efnamena} = ftp:append_bin(Pid, Bin, File++"1\r\nCWD ."),
    ok = ftp:append_bin(Pid, Bin, File),
    ok = ftp:append_bin(Pid, Bin, File),
    %% Control the contents of the file
    {ok, Bin2} = ftp:recv_bin(Pid, File),
    ok = ftp:delete(Pid,File),
    ok = check_content(binary_to_list(Bin2),binary_to_list(Bin), double).

do_send_chunk(Pid, Config) ->
    File = ?config(file, Config),
    Contents = "ftp_SUITE test ...",
    Bin = list_to_binary(Contents),
    ok = ftp:cd(Pid, "incoming"),
    {error, efnamena} = ftp:send_chunk_start(Pid, File++"1\r\nCWD ."),
    ok = ftp:send_chunk_start(Pid, File),
    {error, echunk} = ftp:cd(Pid, "incoming"),
    {error, enotbinary} = ftp:send_chunk(Pid, Contents),
    ok = ftp:send_chunk(Pid, Bin),
    ok = ftp:send_chunk(Pid, Bin),
    ok = ftp:send_chunk_end(Pid),
    {ok, RFilesString} = ftp:nlist(Pid),
    RFiles = split(RFilesString),
    true = lists:member(File, RFiles),
    ok = ftp:delete(Pid, File),
    ok.

do_append_chunk(Pid, Config) ->
    File = ?config(file, Config),
    Contents = ["ER","LE","RL"],
    ok = ftp:cd(Pid, "incoming"),
    {error, efnamena} = ftp:append_chunk_start(Pid, File++"1\r\nCWD ."),
    ok = ftp:append_chunk_start(Pid, File),
    {error, enotbinary} = ftp:append_chunk(Pid, lists:nth(1,Contents)),
    ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(1,Contents))),
    ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(2,Contents))),
    ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(3,Contents))),
    ok = ftp:append_chunk_end(Pid),
    %%Control the contents of the file
    {ok, Bin2}  = ftp:recv_bin(Pid, File),
    ok = check_content(binary_to_list(Bin2),"ERL", double),
    ok = ftp:delete(Pid, File),
    ok.

do_recv(Pid, Config) ->
    PrivDir = ?config(priv_dir, Config),
    File  = ?config(file, Config),
    Newfile = ?config(new_file, Config),
    AbsFile = filename:absname(File, PrivDir),
    Contents = "ftp_SUITE:recv test ...",
    ok = file:write_file(AbsFile, list_to_binary(Contents)),
    ok = ftp:cd(Pid, "incoming"),
    ftp:delete(Pid, File),		% reset
    ftp:lcd(Pid, PrivDir),
    ok = ftp:send(Pid, File),
    ok = file:delete(AbsFile),		% cleanup
    test_server:sleep(100),
    ok = ftp:lcd(Pid, PrivDir),
    {error, efnamena} = ftp:recv(Pid, File++"\r\nCWD ."),
    ok = ftp:recv(Pid, File),
    {ok, Files} = file:list_dir(PrivDir),
    true = lists:member(File, Files),
    ok = file:delete(AbsFile), % cleanup
    ok = ftp:recv(Pid, File, Newfile), 
    ok = ftp:delete(Pid, File),		% cleanup
    ok.

do_recv_bin(Pid, Config) ->
    File = ?config(file, Config),
    Contents1 = "ftp_SUITE test ...",
    Bin1 = list_to_binary(Contents1),
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:send_bin(Pid, Bin1, File),
    test_server:sleep(100),
    {error, efnamena} = ftp:recv_bin(Pid, File++"\r\nCWD ."),
    {ok, Bin2}  = ftp:recv_bin(Pid, File),
    ok = ftp:delete(Pid, File),		% cleanup
    Contents2 = binary_to_list(Bin2),
    Contents1 = Contents2,
    ok.

do_recv_chunk(Pid, Config) ->
    File = ?config(file, Config),
    Data = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
	"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
	"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
	"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
	"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
	"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
	"GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG"
	"HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
	"IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII",

    Contents1 = lists:flatten(lists:duplicate(10, Data)),
    Bin1 = list_to_binary(Contents1),
    ok = ftp:cd(Pid, "incoming"),
    ok = ftp:type(Pid, binary),
    ok = ftp:send_bin(Pid, Bin1, File),
    test_server:sleep(100),
    {error, "ftp:recv_chunk_start/2 not called"} = recv_chunk(Pid, <<>>),
    {error, efnamena} = ftp:recv_chunk_start(Pid, File++"\r\nCWD ."),
    ok = ftp:recv_chunk_start(Pid, File),
    {ok, Contents2} = recv_chunk(Pid, <<>>),
    ok = ftp:delete(Pid, File),		% cleanup
    ok = find_diff(Contents2, Contents1, 1),
    ok.

do_type(Pid) ->
    ok = ftp:type(Pid, ascii),
    ok = ftp:type(Pid, binary),
    ok = ftp:type(Pid, ascii),
    {error, etype} = ftp:type(Pid, foobar),
    ok.

do_quote(Pid) ->
    ["257 \"/\""++_Rest] = ftp:quote(Pid, "pwd"), %% 257
    [_| _] = ftp:quote(Pid, "help"),
    %% This negativ test causes some ftp servers to hang. This test
    %% is not important for the client, so we skip it for now.
    %%["425 Can't build data connection: Connection refused."] 
    %% = ftp:quote(Pid, "list"), 
    ok.

 watch_dog(Config) ->
     Dog = test_server:timetrap(inets_test_lib:minutes(1)),
     NewConfig = lists:keydelete(watchdog, 1, Config),
     [{watchdog, Dog} | NewConfig].

 close_connection(Config) ->
     case ?config(ftp, Config) of
 	Pid when is_pid(Pid) ->
 	    ok = ftp:close(Pid),
 	    lists:delete({ftp, Pid}, Config);
 	_ ->
 	    Config
     end.
  
ftp_host(Config) ->
    case ?config(ftp_remote_host, Config) of
	undefined ->
	    exit({skip, "No host specified"});
	Host ->
	    Host   
    end.

check_content(RContent, LContent, Amount) ->
    LContent2 = case Amount of 
		    double ->
			LContent ++ LContent;
		    singel ->
			LContent
		end,
    case string:equal(RContent, LContent2) of
	true ->
	    ok;
	false ->
	    %% Find where the diff is
	    Where = find_diff(RContent, LContent2, 1),
	    Where
    end.

find_diff(A, A, _) ->
    ok;
find_diff([H|T1], [H|T2], Pos) ->
    find_diff(T1, T2, Pos+1);
find_diff(RC, LC, Pos) ->
    {error, {diff, Pos, RC, LC}}.

recv_chunk(Pid, Acc) ->
    case ftp:recv_chunk(Pid) of
	ok ->
	    {ok, binary_to_list(Acc)};
	{ok, Bin} ->
	    recv_chunk(Pid, <<Acc/binary, Bin/binary>>);
	Error ->
	    Error
    end.

split(Cs) ->
    split(Cs, [], []).

split([$\r, $\n| Cs], I, Is) ->
    split(Cs, [], [lists:reverse(I)| Is]); 
split([C| Cs], I, Is) ->
    split(Cs, [C| I], Is);
split([], I, Is) ->
    lists:reverse([lists:reverse(I)| Is]).

do_ftp_open(Host, Opts) ->
    p("do_ftp_open -> entry with"
      "~n   Host: ~p"
      "~n   Opts: ~p", [Host, Opts]), 
    case ftp:open(Host, Opts) of
	{ok, _} = OK ->
	    OK;
	{error, Reason} ->
	    Str = 
		lists:flatten(
		  io_lib:format("Unable to reach test FTP server ~p (~p)", 
				[Host, Reason])),
	    throw({skip, Str})
    end.
	 

passwd() ->
    Host = 
	case inet:gethostname() of
	    {ok, H} ->
		H;
	    _ ->
		"localhost"
	end,
    "ftp_SUITE@" ++ Host.

ftpd_hosts(Config) ->
    DataDir = ?config(data_dir, Config),
    FileName = filename:join([DataDir, "../ftp_SUITE_data/", ftpd_hosts]),
    p("FileName: ~p", [FileName]),
    case file:consult(FileName) of
	{ok, [Hosts]} when is_list(Hosts) ->
	    Hosts;
	_ -> 
	    []
    end.

wrapper(Prefix,doc,Func) ->
    Prefix++Func(doc);
wrapper(_,X,Func) ->
    Func(X).

data_dir(Config) ->
    case ?config(data_dir, Config) of
	List when (length(List) > 0) ->
	    PathList        = filename:split(List),
	    {NewPathList,_} = lists:split((length(PathList)-1), PathList),
	    DataDir   = filename:join(NewPathList ++ [ftp_SUITE_data]),
	    NewConfig = 
		lists:keyreplace(data_dir,1,Config, {data_dir,DataDir}),
	    NewConfig;
	_ -> Config
    end.
    
       
    
p(F) ->
    p(F, []).

p(F, A) ->
    case get(ftp_testcase) of
	undefined ->
	    io:format("~w [~w] " ++ F ++ "~n", [?MODULE, self() | A]);
	TC when is_atom(TC) ->
	    io:format("~w [~w] ~w:" ++ F ++ "~n", [?MODULE, self(), TC | A])
    end.
