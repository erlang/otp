%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
-module(ssh_upgrade_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").

-record(state, {
	  config,
	  root_dir,
	  server,
	  client,
	  connection,
	  soft
	 }).


%%%================================================================
%%%
%%% CommonTest callbacks
%%% 
suite() ->
    [{timetrap,{seconds,180}}].

all() -> 
    [
     minor_upgrade,
     major_upgrade
    ].

init_per_suite(Config0) ->
    ?CHECK_CRYPTO(
       case ct_release_test:init(Config0) of
	   {skip, Reason} ->
	       {skip, Reason};
	   Config ->
	       ssh:start(),
	       Config
       end
      ).

end_per_suite(Config) ->
    ct_release_test:cleanup(Config),
    ssh:stop(),
    UserDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_rsa(UserDir).

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    Config.

%%%================================================================
%%%
%%% Test cases
%%% 
major_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssh, major,{?MODULE, #state{config = Config}}, Config).

minor_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssh, minor,{?MODULE, #state{config = Config}}, Config).

%%%================================================================
%%%
%%% ct_release_test callbacks
%%% 

%%%----------------------------------------------------------------
%%% Initialyze system before upgrade test starts.
%%% Called by ct_release_test:upgrade/4
upgrade_init(CTData, State) -> 
    {ok, AppUp={_, _, Up, _Down}} = ct_release_test:get_appup(CTData, ssh),
    ct:log("AppUp: ~p", [AppUp]),
    ct:log("Up: ~p", [Up]),
    case Soft = is_soft(Up) of
	%% It is symmetrical, if upgrade is soft so is downgrade  
	true ->
	    setup_server_client(State#state{soft = Soft});
	false ->
	    State#state{soft = Soft}
    end.

%%%----------------------------------------------------------------
%%% Check that upgrade was successful
%%% Called by ct_release_test:upgrade/4
upgrade_upgraded(_, #state{soft=false} = State) ->
    test_hard(State, "upgrade");

upgrade_upgraded(_, State) -> 
    test_soft(State, "upgrade1").

%%%----------------------------------------------------------------
%%% Check that downgrade was successful.
%%% Called by ct_release_test:upgrade/4
upgrade_downgraded(_, #state{soft=false} = State) -> 
    test_hard(State, "downgrade");

upgrade_downgraded(_, #state{soft=true} = State) -> 
    test_soft(State, "downgrade1").
 
%%%================================================================
%%%
%%% Private functions
%%% 

is_soft([{restart_application, ssh}]) -> 
    false;
is_soft(_) ->
    true.


test_hard(State0, FileName) ->
    ct:log("test_hard State0=~p, FileName=~p",[State0, FileName]),
    State = setup_server_client(State0),
    test_connection(FileName, random_contents(), State).

test_soft(State0, FileName) ->
    ct:log("test_soft State0=~p, FileName=~p",[State0, FileName]),
    State = test_connection(FileName, random_contents(), State0),
    setup_server_client( close(State) ).


setup_server_client(#state{config=Config} = State) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
	    
    FtpRootDir = filename:join(PrivDir, "ftp_root"),
    catch file:make_dir(FtpRootDir),
	
    SFTP = ssh_sftpd:subsystem_spec([{root,FtpRootDir},{cwd,FtpRootDir}]),

    {Server,Host,Port} = ssh_test_lib:daemon(ssh_test_lib:inet_port(), % when lower rel is 18.x
					     [{system_dir,DataDir},
					      {user_passwords,[{"hej","hopp"}]},
					      {subsystems,[SFTP]}]),
    
    {ok, ChannelPid, Connection} = 
	ssh_sftp:start_channel(Host, Port, [{user_interaction,false},
					    {silently_accept_hosts,true},
					    {user_dir,DataDir},
					    {user,"hej"},
					    {password,"hopp"}]),
    State#state{server = Server,
		client = ChannelPid,
		connection = Connection}.


test_connection(FileName, FileContents,
		#state{client = ChannelPid,
		       root_dir = FtpRootDir} = State) ->
    ct:log("test_connection Writing with ssh_sftp:write_file",[]),
    case ssh_sftp:write_file(ChannelPid, FileName, FileContents) of
	ok ->
	    case ssh_sftp:read_file(ChannelPid, FileName) of
		{ok,FileContents} ->
			    State;
		{ok,Unexpected} ->
		    ct:fail("Expected ~p but got ~p from sftp:read_file(~p,..) in RootDir ~p",
			    [FileContents,Unexpected,FileName,FtpRootDir]
			   );
		Other ->
		    ct:fail("ssh_sftp:read_file(~p,~p) -> ~p~n" 
			    "ssh_sftp:list_dir(~p,\".\") -> ~p", 
			    [ChannelPid,FileName,Other,
			     ChannelPid, catch ssh_sftp:list_dir(ChannelPid, ".")])
	    end;

	Other ->
	    ct:fail("ssh_sftp:write_file(~p,~p,~p) -> ~p",[ChannelPid,FileName,FileContents,Other])
    end.


close(#state{server = Server,
	     connection = Connection} = State) ->
    ssh:close(Connection),
    ssh:stop_daemon(Server),
    State#state{server = undefined,
		client = undefined,
		connection = undefined}.


random_contents() -> list_to_binary( ssh_test_lib:random_chars(3) ).
