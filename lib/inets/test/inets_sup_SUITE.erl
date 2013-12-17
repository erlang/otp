%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

-module(inets_sup_SUITE).

-include_lib("common_test/include/ct.hrl").


%% Note: This directive should only be used in test suites.
-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [default_tree, ftpc_worker, tftpd_worker, httpd_subtree,
     httpc_subtree].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_) ->
    inets:stop(),
    ok.

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
init_per_testcase(httpd_subtree, Config) ->
    io:format("init_per_testcase(httpd_subtree) -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),
    Dog = test_server:timetrap(?t:minutes(1)),
    NewConfig = lists:keydelete(watchdog, 1, Config),

    DataDir = ?config(data_dir, Config), 
    PrivDir = ?config(priv_dir, Config), 			   
    ServerROOT = filename:join(PrivDir, "server_root"),
    DocROOT = filename:join(PrivDir, "htdocs"),
    ConfDir = filename:join(ServerROOT, "conf"),

    io:format("init_per_testcase(httpd_subtree) -> create dir(s)"
	      "~n", []),
    file:make_dir(ServerROOT), %% until http_test is cleaned up!
    ok = file:make_dir(DocROOT),
    ok = file:make_dir(ConfDir),    

    io:format("init_per_testcase(httpd_subtree) -> copy file(s)"
	      "~n", []),
    {ok, _} = inets_test_lib:copy_file("simple.conf", DataDir, PrivDir),
    {ok, _} = inets_test_lib:copy_file("mime.types", DataDir, ConfDir),
    
    io:format("init_per_testcase(httpd_subtree) -> write file(s)"
	      "~n", []),
    ConfFile = filename:join(PrivDir, "simple.conf"),
    {ok, Fd} = file:open(ConfFile, [append]),
    ok = file:write(Fd, "ServerRoot " ++ ServerROOT ++ "\n"),
    ok = file:write(Fd, "DocumentRoot " ++ DocROOT ++ "\n"),
    ok = file:close(Fd),
    
    %% To make sure application:set_env is not overwritten by any
    %% app-file settings.
    io:format("init_per_testcase(httpd_subtree) -> load inets app"
	      "~n", []),
    application:load(inets),
    io:format("init_per_testcase(httpd_subtree) -> update inets env"
	      "~n", []),
    ok = application:set_env(inets, services, [{httpd, ConfFile}]),
    
    try
	io:format("init_per_testcase(httpd_subtree) -> start inets app"
		  "~n", []),
	ok = inets:start(),
	io:format("init_per_testcase(httpd_subtree) -> done"
		  "~n", []),
	[{watchdog, Dog}, {server_root, ServerROOT}, {doc_root, DocROOT},
	 {conf_dir, ConfDir}| NewConfig]
    catch
	_:Reason ->
	    io:format("init_per_testcase(httpd_subtree) -> "
		      "failed starting inets - cleanup"
		      "~n   Reason: ~p"
		      "~n", [Reason]),
	    application:unset_env(inets, services),
	    application:unload(inets),
	    exit({failed_starting_inets, Reason})
    end;
    

init_per_testcase(Case, Config) ->
    io:format("init_per_testcase(~p) -> entry with"
	      "~n   Config: ~p"
	      "~n", [Case, Config]),
    Dog = test_server:timetrap(?t:minutes(5)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    Stop = inets:stop(),
    io:format("init_per_testcase(~p) -> Stop: ~p"
	      "~n", [Case, Stop]),
    ok = inets:start(),
    [{watchdog, Dog} | NewConfig].


%%--------------------------------------------------------------------
%% Function: end_per_testcase(Case, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(httpd_subtree, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    PrivDir = ?config(priv_dir, Config), 			   
    inets_test_lib:del_dirs(PrivDir),
    ok;

end_per_testcase(_, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    inets:stop(),
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%% default_tree
%%-------------------------------------------------------------------------
default_tree(doc) ->
    ["Makes sure the correct processes are started and linked," 
     "in the default case."];
default_tree(suite) ->
    [];
default_tree(Config) when is_list(Config) ->
    TopSupChildren = supervisor:which_children(inets_sup),
    4 = length(TopSupChildren),
    {value, {httpd_sup, _, supervisor,[httpd_sup]}} =
	lists:keysearch(httpd_sup, 1, TopSupChildren),
    {value, {httpc_sup, _,supervisor,[httpc_sup]}} = 
	lists:keysearch(httpc_sup, 1, TopSupChildren),
    {value, {ftp_sup,_,supervisor,[ftp_sup]}} = 
	lists:keysearch(ftp_sup, 1, TopSupChildren),
    {value, {tftp_sup,_,supervisor,[tftp_sup]}} = 
	lists:keysearch(tftp_sup, 1, TopSupChildren),

    HttpcSupChildren = supervisor:which_children(httpc_sup),
    {value, {httpc_profile_sup,_, supervisor, [httpc_profile_sup]}} =
	lists:keysearch(httpc_profile_sup, 1, HttpcSupChildren),
    {value, {httpc_handler_sup,_, supervisor, [httpc_handler_sup]}} =
	lists:keysearch(httpc_handler_sup, 1, HttpcSupChildren),
    
    [] = supervisor:which_children(ftp_sup),

    [] = supervisor:which_children(httpd_sup),
 
    %% Default profile
    [{httpc_manager, _, worker,[httpc_manager]}]
	= supervisor:which_children(httpc_profile_sup),
    
    [] = supervisor:which_children(httpc_handler_sup),
     
    [] = supervisor:which_children(tftp_sup),

    ok.


%%-------------------------------------------------------------------------
%% ftpc_worker
%%-------------------------------------------------------------------------
ftpc_worker(doc) ->
    ["Makes sure the ftp worker processes are added and removed "
     "appropriatly to/from the supervison tree."]; 
ftpc_worker(suite) ->
    [];
ftpc_worker(Config) when is_list(Config) ->
    [] = supervisor:which_children(ftp_sup),
    try
	begin
	    {_Tag, FtpdHost} = ftp_suite_lib:dirty_select_ftpd_host(Config),
	    case inets:start(ftpc, [{host, FtpdHost}]) of
		{ok, Pid} ->
		    case supervisor:which_children(ftp_sup) of
			[{_,_, worker, [ftp]}] ->
			    inets:stop(ftpc, Pid), 
			    test_server:sleep(5000),
			    [] = supervisor:which_children(ftp_sup),
			    ok;
			Children ->
			    exit({unexpected_children, Children})
		    end;
		_ ->
		    {skip, "Unable to reach test FTP server"}
	    end
	end
    catch
	throw:{error, not_found} ->
	    {skip, "No available FTP servers"}
    end.


%%-------------------------------------------------------------------------
%% tftpd_worker
%%-------------------------------------------------------------------------
tftpd_worker(doc) ->
    ["Makes sure the tftp sub tree is correct."]; 
tftpd_worker(suite) ->
    [];
tftpd_worker(Config) when is_list(Config) ->
    [] = supervisor:which_children(tftp_sup),   
    {ok, Pid0} = inets:start(tftpd, [{host, "localhost"}, 
				     {port, inet_port()}]),
    {ok, _Pid1} = inets:start(tftpd, [{host, "localhost"}, 
				      {port, inet_port()}], stand_alone),
    
    [{_,Pid0, worker, _}] = supervisor:which_children(tftp_sup),
    inets:stop(tftpd, Pid0),
    test_server:sleep(5000),
    [] = supervisor:which_children(tftp_sup),
    ok.


%%-------------------------------------------------------------------------
%% httpd_subtree
%%-------------------------------------------------------------------------
httpd_subtree(doc) ->
    ["Makes sure the httpd sub tree is correct."]; 
httpd_subtree(suite) ->
    [];
httpd_subtree(Config) when is_list(Config) ->
    io:format("httpd_subtree -> entry with"
	      "~n   Config: ~p"
	      "~n", [Config]),

    %% Check that we have the httpd top supervisor
    io:format("httpd_subtree -> verify inets~n", []),
    {ok, _} = verify_child(inets_sup, httpd_sup, supervisor),

    %% Check that we have the httpd instance supervisor
    io:format("httpd_subtree -> verify httpd~n", []),
    {ok, Id} = verify_child(httpd_sup, httpd_instance_sup, supervisor),
    {httpd_instance_sup, Addr, Port} = Id,
    Instance = httpd_util:make_name("httpd_instance_sup", Addr, Port),
    
    %% Check that we have the expected httpd instance children
    io:format("httpd_subtree -> verify httpd instance children "
	      "(acceptor, misc and manager)~n", []),
    {ok, _} = verify_child(Instance, httpd_connection_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_acceptor_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_misc_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_manager, worker),

    %% Check that the httpd instance acc supervisor has children
    io:format("httpd_subtree -> verify acc~n", []),
    InstanceAcc = httpd_util:make_name("httpd_acceptor_sup", Addr, Port),
    case supervisor:which_children(InstanceAcc) of
	[_ | _] -> 
	    ok;
	InstanceAccUnexpectedChildren ->
	    exit({unexpected_children, 
		  InstanceAcc, InstanceAccUnexpectedChildren})
    end,
    
    %% Check that the httpd instance misc supervisor has no children
    io:format("httpd_subtree -> verify misc~n", []),
    InstanceMisc = httpd_util:make_name("httpd_misc_sup", Addr, Port),
    case supervisor:which_children(InstanceMisc) of
	[] ->
	    ok;
	InstanceMiscUnexpectedChildren ->
	    exit({unexpected_children, 
		  InstanceMisc, InstanceMiscUnexpectedChildren})
    end,
    io:format("httpd_subtree -> done~n", []),
    ok.


verify_child(Parent, Child, Type) ->
%%     io:format("verify_child -> entry with"
%% 	      "~n   Parent: ~p"
%% 	      "~n   Child:  ~p"
%% 	      "~n   Type:   ~p"
%% 	      "~n", [Parent, Child, Type]),
    Children = supervisor:which_children(Parent),
%%     io:format("verify_child -> which children"
%% 	      "~n   Children:   ~p"
%% 	      "~n", [Children]),
    verify_child(Children, Parent, Child, Type).

verify_child([], Parent, Child, _Type) ->
    {error, {child_not_found, Child, Parent}};
verify_child([{Id, _Pid, Type2, Mods}|Children], Parent, Child, Type) ->
    case lists:member(Child, Mods) of
	true when (Type2 =:= Type) ->
%% 	    io:format("verify_child -> found with expected type"
%% 		      "~n   Id: ~p"
%% 		      "~n", [Id]),
	    {ok, Id};
	true when (Type2 =/= Type) ->
%% 	    io:format("verify_child -> found with unexpected type"
%% 		      "~n   Type2: ~p"
%% 		      "~n   Id:    ~p"
%% 		      "~n", [Type2, Id]),
	    {error, {wrong_type, Type2, Child, Parent}};
	false ->
	    verify_child(Children, Parent, Child, Type)
    end.
					  


%%-------------------------------------------------------------------------
%% httpc_subtree
%%-------------------------------------------------------------------------
httpc_subtree(doc) ->
    ["Makes sure the httpc sub tree is correct."]; 
httpc_subtree(suite) ->
    [];
httpc_subtree(Config) when is_list(Config) ->
    tsp("httpc_subtree -> entry with"
	"~n   Config: ~p", [Config]),

    tsp("httpc_subtree -> start inets service httpc with profile foo"),
    {ok, _Foo} = inets:start(httpc, [{profile, foo}]),

    tsp("httpc_subtree -> "
	"start stand-alone inets service httpc with profile bar"),
    {ok, _Bar} = inets:start(httpc, [{profile, bar}], stand_alone),

    tsp("httpc_subtree -> retreive list of httpc instances"),
    HttpcChildren = supervisor:which_children(httpc_profile_sup),
    tsp("httpc_subtree -> HttpcChildren: ~n~p", [HttpcChildren]),
    
    tsp("httpc_subtree -> verify httpc stand-alone instances"),
    {value, {httpc_manager, _, worker, [httpc_manager]}} =
	lists:keysearch(httpc_manager, 1, HttpcChildren),

    tsp("httpc_subtree -> verify httpc (named) instances"),
    {value,{{httpc,foo}, Pid, worker, [httpc_manager]}} = 
	lists:keysearch({httpc, foo}, 1, HttpcChildren),
    false = lists:keysearch({httpc, bar}, 1, HttpcChildren),
    
    tsp("httpc_subtree -> stop inets"),
    inets:stop(httpc, Pid),

    tsp("httpc_subtree -> done"),
    ok.

inet_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.


tsp(F) ->
    tsp(F, []).
tsp(F, A) ->
    test_server:format("~p ~p:" ++ F ++ "~n", [self(), ?MODULE | A]).

tsf(Reason) ->
    test_server:fail(Reason).

