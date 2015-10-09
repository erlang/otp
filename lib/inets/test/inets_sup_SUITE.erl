%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

-module(inets_sup_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [default_tree, ftpc_worker, tftpd_worker, 
     httpd_subtree, httpd_subtree_profile,
     httpc_subtree].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    inets:stop(),
    ok.

init_per_testcase(httpd_subtree, Config) ->
    Dog = test_server:timetrap(?t:minutes(1)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    PrivDir = ?config(priv_dir, Config), 			   
    Dir = filename:join(PrivDir, "root"),
    ok = file:make_dir(Dir),

    SimpleConfig  = [{port, 0},
		     {server_name,"www.test"},
		     {modules, [mod_get]},
		     {server_root, Dir},
		     {document_root, Dir},
		     {bind_address, any},
		     {ipfamily, inet}],
    try
	inets:stop(),
	inets:start(),
	inets:start(httpd, SimpleConfig),
	[{watchdog, Dog} | NewConfig]
    catch
	_:Reason ->
	    inets:stop(),
	    exit({failed_starting_inets, Reason})
    end;

init_per_testcase(httpd_subtree_profile, Config) ->
    Dog = test_server:timetrap(?t:minutes(1)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    PrivDir = ?config(priv_dir, Config), 			   
    Dir = filename:join(PrivDir, "root"),
    ok = file:make_dir(Dir),

    SimpleConfig  = [{port, 0},
		     {server_name,"www.test"},
		     {modules, [mod_get]},
		     {server_root, Dir},
		     {document_root, Dir},
		     {bind_address, any},
		     {profile, test_profile},
		     {ipfamily, inet}],
    try
	inets:stop(),
	inets:start(),
	{ok, _} = inets:start(httpd, SimpleConfig),
	[{watchdog, Dog} | NewConfig]
    catch
	_:Reason ->
	    inets:stop(),
	    exit({failed_starting_inets, Reason})
    end;
     

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?t:minutes(5)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    inets:stop(),
    ok = inets:start(),
    [{watchdog, Dog} | NewConfig].

end_per_testcase(Case, Config) when Case == httpd_subtree;
				    Case == httpd_subtree_profile ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    PrivDir = ?config(priv_dir, Config), 	
    Dir = filename:join(PrivDir, "root"),
    inets_test_lib:del_dirs(Dir),
    ok;

end_per_testcase(_, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    inets:stop(),
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
default_tree() ->
    [{doc, "Makes sure the correct processes are started and linked," 
      "in the default case."}].
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

ftpc_worker() ->
    [{doc, "Makes sure the ftp worker processes are added and removed "
      "appropriatly to/from the supervison tree."}].
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

tftpd_worker() ->
    [{doc, "Makes sure the tftp sub tree is correct."}].
tftpd_worker(Config) when is_list(Config) ->
    [] = supervisor:which_children(tftp_sup),   
    {ok, Pid0} = inets:start(tftpd, [{host, inets_test_lib:hostname()}, 
				     {port, 0}]),
    {ok, _Pid1} = inets:start(tftpd, [{host, inets_test_lib:hostname()}, 
				      {port, 0}], stand_alone),
    
    [{_,Pid0, worker, _}] = supervisor:which_children(tftp_sup),
    inets:stop(tftpd, Pid0),
    test_server:sleep(5000),
    [] = supervisor:which_children(tftp_sup),
    ok.

httpd_subtree() ->
    [{doc, "Makes sure the httpd sub tree is correct."}].
httpd_subtree(Config) when is_list(Config) ->
    do_httpd_subtree(Config, default).

httpd_subtree_profile(doc) ->
    ["Makes sure the httpd sub tree is correct when using a profile"]; 
httpd_subtree_profile(Config) when is_list(Config) ->
    do_httpd_subtree(Config, test_profile).

httpc_subtree() ->
    [{doc, "Makes sure the httpd sub tree is correct."}]. 
httpc_subtree(Config) when is_list(Config) ->
    {ok, Foo} = inets:start(httpc, [{profile, foo}]),

    {ok, Bar} = inets:start(httpc, [{profile, bar}], stand_alone),

    HttpcChildren = supervisor:which_children(httpc_profile_sup),

    {value, {httpc_manager, _, worker, [httpc_manager]}} =
	lists:keysearch(httpc_manager, 1, HttpcChildren),

    {value,{{httpc,foo}, _Pid, worker, [httpc_manager]}} = 
	lists:keysearch({httpc, foo}, 1, HttpcChildren),
    false = lists:keysearch({httpc, bar}, 1, HttpcChildren),
    
    inets:stop(httpc, Foo),
    exit(Bar, normal).

%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------

verify_child(Parent, Child, Type) ->
    Children = supervisor:which_children(Parent),
    verify_child(Children, Parent, Child, Type).

verify_child([], Parent, Child, _Type) ->
    {error, {child_not_found, Child, Parent}};
verify_child([{Id, _Pid, Type2, Mods}|Children], Parent, Child, Type) ->
    case lists:member(Child, Mods) of
	true when (Type2 =:= Type) ->
	    {ok, Id};
	true when (Type2 =/= Type) ->
	    {error, {wrong_type, Type2, Child, Parent}};
	false ->
	    verify_child(Children, Parent, Child, Type)
    end.

do_httpd_subtree(_Config, Profile) ->
   %% Check that we have the httpd top supervisor
    {ok, _} = verify_child(inets_sup, httpd_sup, supervisor),

    %% Check that we have the httpd instance supervisor
    {ok, Id} = verify_child(httpd_sup, httpd_instance_sup, supervisor),
    {httpd_instance_sup, Addr, Port, Profile} = Id,
    Instance = httpd_util:make_name("httpd_instance_sup", Addr, Port, Profile),
    
    %% Check that we have the expected httpd instance children
    {ok, _} = verify_child(Instance, httpd_connection_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_acceptor_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_misc_sup, supervisor),
    {ok, _} = verify_child(Instance, httpd_manager, worker),

    %% Check that the httpd instance acc supervisor has children
    InstanceAcc = httpd_util:make_name("httpd_acceptor_sup", Addr, Port, Profile),
    case supervisor:which_children(InstanceAcc) of
	[_ | _] -> 
	    ok;
	InstanceAccUnexpectedChildren ->
	    exit({unexpected_children, 
		  InstanceAcc, InstanceAccUnexpectedChildren})
    end,
    
    %% Check that the httpd instance misc supervisor has no children
    io:format("httpd_subtree -> verify misc~n", []),
    InstanceMisc = httpd_util:make_name("httpd_misc_sup", Addr, Port, Profile),
    case supervisor:which_children(InstanceMisc) of
	[] ->
	    ok;
	InstanceMiscUnexpectedChildren ->
	    exit({unexpected_children, 
		  InstanceMisc, InstanceMiscUnexpectedChildren})
    end,
    io:format("httpd_subtree -> done~n", []),
    ok.
