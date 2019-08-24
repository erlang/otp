%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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

-module(eldap_basic_SUITE).

-compile(export_all).

%%-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eldap/include/eldap.hrl").
-include_lib("eldap/ebin/ELDAPv3.hrl").


%% Control to delete a referral object:
-define(manageDsaIT, {control,"2.16.840.1.113730.3.4.2",false,asn1_NOVALUE}).

suite() ->
    [{timetrap,{seconds,360}}].

all() ->
    [app,
     appup,
     {group, encode_decode},
     {group, return_values},
     {group, v4_connections},
     {group, v6_connections},
     {group, plain_api},
     {group, ssl_api},
     {group, start_tls_api}
    ].

groups() ->
    [{encode_decode, [], [encode,
			  decode
			  ]},
     {plain_api,     [], [{group,api}]},
     {ssl_api,       [], [{group,api}, start_tls_on_ssl_should_fail]},
     {start_tls_api, [], [{group,api}, start_tls_twice_should_fail]},

     {api, [], [{group,api_not_bound},
		{group,api_bound}]},

     {api_not_bound, [], [elementary_search, search_non_existant,
			  add_when_not_bound,
			  bind]},
     {api_bound, [], [add_when_bound,
		      add_already_exists,
		      more_add,
		      add_referral,
		      search_filter_equalityMatch,
		      search_filter_substring_any,
		      search_filter_initial,
		      search_filter_final,
		      search_filter_and,
		      search_filter_or,
		      search_filter_and_not,
		      search_two_hits,
		      search_referral,
		      modify,
		      modify_referral,
		      delete,
		      delete_referral,
		      modify_dn_delete_old,
		      modify_dn_keep_old]},
     {v4_connections, [], connection_tests()},
     {v6_connections, [], connection_tests()},
     {return_values, [], [open_ret_val_success,
			  open_ret_val_error,
			  close_ret_val]}
    ].

connection_tests() ->
    [tcp_connection,
     tcp_connection_option,
     ssl_connection,
     client_side_start_tls_timeout,
     client_side_bind_timeout,
     client_side_add_timeout,
     client_side_search_timeout,
     close_after_tcp_error
    ].



init_per_suite(Config) ->
    SSL_available = init_ssl_certs_et_al(Config),
    LDAP_server =  find_first_server(false, [{config,eldap_server},
					     {config,ldap_server}, 
					     {"localhost",9876},
					     {"aramis.otp.ericsson.se",9876}]),
    LDAPS_server =
	case SSL_available of
	    true ->
		find_first_server(true,  [{config,ldaps_server},
					  {"localhost",9877},
					  {"aramis.otp.ericsson.se",9877}]);
	    false ->
		undefined
	end,
    [{ssl_available, SSL_available},
     {ldap_server,   LDAP_server},
     {ldaps_server,  LDAPS_server} | Config].

end_per_suite(_Config) ->
    try ssl:stop()
    catch
        _:_ -> ok
    end.


init_per_group(return_values, Config) ->
    case proplists:get_value(ldap_server,Config) of
	undefined ->
	    {skip, "LDAP server not availble"};
	{Host,Port} ->
	    ct:comment("ldap://~s:~p",[Host,Port]),
	    Config
    end;
init_per_group(plain_api, Config0) ->
    case proplists:get_value(ldap_server,Config0) of
	undefined ->
	    {skip, "LDAP server not availble"};
	Server = {Host,Port} ->
	    ct:comment("ldap://~s:~p",[Host,Port]),
	    initialize_db([{server,Server}, {ssl_flag,false}, {start_tls,false} | Config0])
    end;
init_per_group(ssl_api, Config0) ->
    case proplists:get_value(ldaps_server,Config0) of
	undefined ->
	    {skip, "LDAPS server not availble"};
	Server = {Host,Port} ->
	    ct:comment("ldaps://~s:~p",[Host,Port]),
	    initialize_db([{server,Server}, {ssl_flag,true}, {start_tls,false} | Config0])
    end;
init_per_group(start_tls_api, Config0) ->
    case {proplists:get_value(ldap_server,Config0), proplists:get_value(ssl_available,Config0)} of
	{undefined,true} ->
	    {skip, "LDAP server not availble"};
	{_,false} ->
	    {skip, "TLS not availble"};
	{Server={Host,Port}, true} ->
	    ct:comment("ldap://~s:~p + start_tls",[Host,Port]),
	    Config = [{server,Server}, {ssl_flag,false} | Config0],
	    case supported_extension("1.3.6.1.4.1.1466.20037", Config) of
		true -> initialize_db([{start_tls,true} | Config]);
		false -> {skip, "start_tls not supported according to the server"}
	    end
    end;
init_per_group(v4_connections, Config) ->
    [{tcp_listen_opts,  [{reuseaddr, true}]},
     {listen_host,  "localhost"},
     {tcp_connect_opts, []}
     |  Config];
init_per_group(v6_connections, Config) ->
    {ok, Hostname} = inet:gethostname(),
    case lists:member(list_to_atom(Hostname), ct:get_config(ipv6_hosts,[])) of
	true -> 
	    [{tcp_listen_opts,  [inet6,{reuseaddr, true}]},
	     {listen_host,  "::"},
	     {tcp_connect_opts, [{tcpopts,[inet6]}]}
	     |  Config];
	false ->
	    {skip, io_lib:format("~p is not an ipv6_host",[Hostname])}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(plain_api,     Config) -> clear_db(Config);
end_per_group(ssl_api,       Config) -> clear_db(Config);
end_per_group(start_tls_api, Config) -> clear_db(Config);
end_per_group(_Group, Config) -> Config.


init_per_testcase(ssl_connection, Config) ->
    case proplists:get_value(ssl_available,Config) of
	true ->
	    SSL_Port = 9999,
	    CertFile = filename:join(proplists:get_value(data_dir,Config), "certs/server/cert.pem"),
	    KeyFile = filename:join(proplists:get_value(data_dir,Config), "certs/server/key.pem"),

	    Parent = self(),
	    Listener = spawn_link(
			 fun() ->
				 case ssl:listen(SSL_Port, [{certfile, CertFile},
							    {keyfile, KeyFile}
							    | proplists:get_value(tcp_listen_opts,Config)
							   ]) of
				     {ok,SSL_LSock} ->
					 Parent ! {ok,self()},
					 (fun L() ->
						ct:log("ssl server waiting for connections...",[]),
						{ok, S} = ssl:transport_accept(SSL_LSock),
						ct:log("ssl:transport_accept/1 ok",[]),
						ok = ssl:ssl_accept(S),
						ct:log("ssl:ssl_accept/1 ok",[]),
						L()
				          end)();
				     Other ->
					 Parent ! {not_ok,Other,self()}
				 end
			 end),
	    receive
		{ok,Listener} ->
		    ct:log("SSL listening to port ~p (process ~p)",[SSL_Port, Listener]),
		    [{ssl_listener,Listener},
		     {ssl_listen_port,SSL_Port},
		     {ssl_connect_opts,[]}
		     | Config];
		{no_ok,SSL_Other,Listener} ->
		    ct:log("ssl:listen on port ~p failed: ~p",[SSL_Port,SSL_Other]),
		    {fail, "ssl:listen/2 failed"}
	    after 5000 ->
		    {fail, "Waiting for ssl:listen timeout"}
	    end;
	false ->
	    {skip, "ssl not available"}
    end;

init_per_testcase(TC, Config) ->
    case lists:member(TC,connection_tests()) of
	true ->
	    case gen_tcp:listen(0, proplists:get_value(tcp_listen_opts,Config)) of
		{ok,LSock} ->
		    {ok,{_,Port}} = inet:sockname(LSock),
		    [{listen_socket,LSock},
		     {listen_port,Port}
		     | Config];
		Other ->
		    {fail, Other}
	    end;

	false ->
	    case proplists:get_value(name,proplists:get_value(tc_group_properties, Config)) of
		api_not_bound ->
		    {ok,H} = open(Config),
		    [{handle,H} | Config];
		api_bound ->
		    {ok,H} = open(Config),
		    ok = eldap:simple_bind(H,
					   "cn=Manager,dc=ericsson,dc=se",
					   "hejsan"),
		    [{handle,H} | Config];
		_Name ->
		    Config
	    end
    end.

end_per_testcase(_, Config) ->
    catch gen_tcp:close( proplists:get_value(listen_socket, Config) ),
    catch eldap:close( proplists:get_value(handle,Config) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Test cases
%%%

%%%----------------------------------------------------------------
%%% Test that the eldap app file is ok
app(Config) when is_list(Config) ->
    ok = test_server:app_test(eldap).

%%%----------------------------------------------------------------
%%% Test that the eldap appup file is ok
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(eldap).

%%%----------------------------------------------------------------
open_ret_val_success(Config) ->
    {Host,Port} = proplists:get_value(ldap_server,Config),
    {ok,H} = eldap:open([Host], [{port,Port}]),
    catch eldap:close(H).

%%%----------------------------------------------------------------
open_ret_val_error(_Config) ->
    {error,_} = eldap:open(["nohost.example.com"], [{port,65535}]).

%%%----------------------------------------------------------------
close_ret_val(Config) ->
    {Host,Port} = proplists:get_value(ldap_server,Config),
    {ok,H} = eldap:open([Host], [{port,Port}]),
    ok = eldap:close(H).

%%%----------------------------------------------------------------
tcp_connection(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(tcp_connect_opts, Config),
    case eldap:open([Host], [{port,Port}|Opts]) of
	{ok,_H} ->
	    Sl = proplists:get_value(listen_socket, Config),
	    case gen_tcp:accept(Sl,1000) of
		{ok,_S} -> ok;
		{error,timeout} -> ct:fail("server side accept timeout",[]);
		Other -> ct:fail("gen_tdp:accept failed: ~p",[Other])
	    end;
	Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

%%%----------------------------------------------------------------
close_after_tcp_error(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(tcp_connect_opts, Config),
    T = 1000,
    case eldap:open([Host], [{timeout,T},{port,Port}|Opts]) of
    {ok,H} ->
        Sl = proplists:get_value(listen_socket, Config),
        gen_tcp:close(Sl),
        {error,{gen_tcp_error,closed}} = eldap:simple_bind(H, anon, anon),
        ok = eldap:close(H),
        wait_for_close(H);
    Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

wait_for_close(H) ->
    case erlang:is_process_alive(H) of
        true  -> timer:sleep(100),
                 wait_for_close(H);
        false -> ok
    end.

%%%----------------------------------------------------------------
ssl_connection(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(ssl_listen_port, Config),
    Opts = proplists:get_value(tcp_connect_opts, Config),
    SSLOpts = proplists:get_value(ssl_connect_opts, Config),
    case eldap:open([Host], [{port,Port},
			     {ssl,true},
			     {timeout,5000},
			     {sslopts,SSLOpts}|Opts]) of
	{ok,_H} -> ok;
	Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

%%%----------------------------------------------------------------
client_side_add_timeout(Config) ->
    client_timeout(
      fun(H) ->
	      eldap:add(H, "cn=Foo Bar,dc=host,dc=ericsson,dc=se",
			[{"objectclass", ["person"]},
			 {"cn", ["Foo Bar"]},
			 {"sn", ["Bar"]},
			 {"telephoneNumber", ["555-1232", "555-5432"]}])
      end, Config).

%%%----------------------------------------------------------------
client_side_bind_timeout(Config) ->
    client_timeout(
      fun(H) ->
	      eldap:simple_bind(H, anon, anon)
      end, Config).

%%%----------------------------------------------------------------
client_side_search_timeout(Config) ->
    client_timeout(
      fun(H) ->
	      eldap:search(H, [{base,"dc=host,dc=ericsson,dc=se"},
			       {filter, eldap:present("objectclass")},
			       {scope,  eldap:wholeSubtree()}])
      end, Config).

%%%----------------------------------------------------------------
client_side_start_tls_timeout(Config) ->
    client_timeout(
      fun(H) ->
	      eldap:start_tls(H, [])
      end, Config).

%%%----------------------------------------------------------------
tcp_connection_option(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(tcp_connect_opts, Config),
    Sl = proplists:get_value(listen_socket, Config),

    %% Make an option value to test.  The option must be implemented on all
    %% platforms that we test on.  Must check what the default value is
    %% so we don't happen to choose that particular value.
    {ok,[{linger,DefaultLinger}]} = inet:getopts(Sl, [linger]),
    TestLinger = case DefaultLinger of
		     {false,_} -> {true,5};
		     {true,_} -> {false,0}
		 end,

    case catch eldap:open([Host],
			  [{port,Port},{tcpopts,[{linger,TestLinger}]}|Opts]) of
	{ok,H} ->
	    case gen_tcp:accept(Sl,1000) of
		{ok,_} ->
		    case eldap:getopts(H, [{tcpopts,[linger]}]) of
			{ok,[{tcpopts,[{linger,ActualLinger}]}]} ->
			    case ActualLinger of
				TestLinger ->
				    ok;
				DefaultLinger ->
				    ct:fail("eldap:getopts: 'linger' didn't change,"
					    " got ~p (=default) expected ~p",
					    [ActualLinger,TestLinger]);
				_ ->
				    ct:fail("eldap:getopts: bad 'linger', got ~p expected ~p",
					    [ActualLinger,TestLinger])
			    end;
			Other ->
			    ct:fail("eldap:getopts: bad result ~p",[Other])
		    end;
		{error,timeout} ->
		    ct:fail("server side accept timeout",[])
	    end;

	Other ->
	    ct:fail("eldap:open failed: ~p",[Other])
    end.


%%%----------------------------------------------------------------
%%% Basic test that all api functions works as expected

%%%----------------------------------------------------------------
elementary_search(Config) ->
    {ok, #eldap_search_result{entries=[_]}} =
	eldap:search(proplists:get_value(handle,Config),
		     #eldap_search{base  = proplists:get_value(eldap_path, Config),
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}).

%%%----------------------------------------------------------------
search_non_existant(Config) ->
    {error, noSuchObject} =
	eldap:search(proplists:get_value(handle,Config),
		     #eldap_search{base  = "cn=Bar," ++ proplists:get_value(eldap_path, Config),
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}).

%%%----------------------------------------------------------------
add_when_not_bound(Config) ->
    {error, _} = eldap:add(proplists:get_value(handle,Config),
			   "cn=Jonas Jonsson," ++ proplists:get_value(eldap_path, Config),
			   [{"objectclass", ["person"]},
			    {"cn", ["Jonas Jonsson"]},
			    {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
bind(Config) ->
    ok = eldap:simple_bind(proplists:get_value(handle,Config),
			   "cn=Manager,dc=ericsson,dc=se",
			   "hejsan").

%%%----------------------------------------------------------------
add_when_bound(Config) ->
    ok = eldap:add(proplists:get_value(handle, Config),
		   "cn=Jonas Jonsson," ++  proplists:get_value(eldap_path, Config),
		   [{"objectclass", ["person"]},
		    {"cn", ["Jonas Jonsson"]},
		    {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
add_already_exists(Config) ->
    {error, entryAlreadyExists} =
	eldap:add(proplists:get_value(handle, Config),
		  "cn=Jonas Jonsson," ++ proplists:get_value(eldap_path, Config),
		  [{"objectclass", ["person"]},
		   {"cn", ["Jonas Jonsson"]},
		   {"sn", ["Jonsson"]}]).

%%%----------------------------------------------------------------
more_add(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    ok = eldap:add(H, "cn=Foo Bar," ++ BasePath,
		   [{"objectclass", ["person"]},
		    {"cn", ["Foo Bar"]},
		    {"sn", ["Bar"]},
		    {"telephoneNumber", ["555-1232", "555-5432"]}]),
    ok = eldap:add(H, "ou=Team," ++ BasePath,
		   [{"objectclass", ["organizationalUnit"]},
		    {"ou", ["Team"]}]).

%%%----------------------------------------------------------------
add_referral(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    {ok,{referral,["ldap://nowhere.example.com"++_]}} =
	eldap:add(H, "cn=Foo Bar,dc=notHere," ++ BasePath,
		  [{"objectclass", ["person"]},
		   {"cn", ["Foo Bar"]},
		   {"sn", ["Bar"]},
		   {"telephoneNumber", ["555-1232", "555-5432"]}]).

%%%----------------------------------------------------------------
search_filter_equalityMatch(Config) ->
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDN = "cn=Jonas Jonsson," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(proplists:get_value(handle, Config),
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("sn", "Jonsson"),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_substring_any(Config) ->
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDN = "cn=Jonas Jonsson," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(proplists:get_value(handle, Config),
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "ss"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_initial(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{initial, "B"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_final(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{final, "r"}]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_and(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDN = "cn=Foo Bar," ++ BasePath,
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=ExpectedDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'and'([eldap:substrings("sn", [{any, "a"}]),
							 eldap:equalityMatch("cn","Foo Bar")]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_filter_or(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    ExpectedDNs = lists:sort(["cn=Foo Bar," ++ BasePath,
			      "ou=Team," ++ BasePath]),
    {ok, #eldap_search_result{entries=Es}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'or'([eldap:substrings("sn", [{any, "a"}]),
							eldap:equalityMatch("ou","Team")]),
				   scope=eldap:singleLevel()}),
    ExpectedDNs = lists:sort([DN || #eldap_entry{object_name=DN} <- Es]).

%%%----------------------------------------------------------------
search_filter_and_not(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    {ok, #eldap_search_result{entries=[]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:'and'([eldap:substrings("sn", [{any, "a"}]),
							 eldap:'not'(
							   eldap:equalityMatch("cn","Foo Bar")
							  )]),
				   scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
search_two_hits(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    DN1 = "cn=Santa Claus," ++ BasePath,
    DN2 = "cn=Jultomten," ++ BasePath,
    %% Add two objects:
    ok = eldap:add(H, DN1,
		   [{"objectclass", ["person"]},
		    {"cn", ["Santa Claus"]},
		    {"sn", ["Santa"]},
		    {"description", ["USA"]}]),
    ok = eldap:add(H, DN2,
		   [{"objectclass", ["person"]},
		    {"cn", ["Jultomten"]},
		    {"sn", ["Tomten"]},
		    {"description", ["Sweden"]}]),

    %% Search for them:
    {ok, #eldap_search_result{entries=Es}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:present("description"),
				   scope=eldap:singleLevel()}),

    %% And check that they are the expected ones:
    ExpectedDNs = lists:sort([DN1, DN2]),
    ExpectedDNs = lists:sort([D || #eldap_entry{object_name=D} <- Es]),

    %% Restore the database:
    [ok=eldap:delete(H,DN) || DN <- ExpectedDNs].

%%%----------------------------------------------------------------
search_referral(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    DN = "cn=Santa Claus,dc=notHere," ++ BasePath,
    {ok,{referral,["ldap://nowhere.example.com"++_]}} =
	eldap:search(H, #eldap_search{base = DN,
				      filter = eldap:present("description"),
				      scope=eldap:singleLevel()}).

%%%----------------------------------------------------------------
modify(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    %% The object to modify
    DN = "cn=Foo Bar," ++ BasePath,

    %% Save a copy to restore later:
    {ok,OriginalAttrs} = attributes(H, DN),

    %% Do a change
    Mod = [eldap:mod_replace("telephoneNumber", ["555-12345"]),
	   eldap:mod_add("description", ["Nice guy"])],
    ok = eldap:modify(H, DN, Mod),

    %% Check that the object was changed
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("telephoneNumber", "555-12345"),
				   scope=eldap:singleLevel()}),

    %% Do another type of change
    ok = eldap:modify(H, DN, [eldap:mod_delete("telephoneNumber", [])]),
    %% and check that it worked by repeating the test above
    {ok, #eldap_search_result{entries=[]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:equalityMatch("telephoneNumber", "555-12345"),
				   scope=eldap:singleLevel()}),
    %% restore the orignal version:
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
modify_referral(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    %% The object to modify
    DN = "cn=Foo Bar,dc=notHere," ++ BasePath,

    %% Do a change
    Mod = [eldap:mod_replace("telephoneNumber", ["555-12345"]),
	   eldap:mod_add("description", ["Nice guy"])],
    {ok,{referral,["ldap://nowhere.example.com"++_]}} =
	eldap:modify(H, DN, Mod).

%%%----------------------------------------------------------------
delete(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    %% The element to play with:
    DN = "cn=Jonas Jonsson," ++ BasePath,

    %% Prove that the element is present before deletion
    {ok,OriginalAttrs} = attributes(H, DN),

    %% Do what the test has to do:
    ok = eldap:delete(H, DN),
    %% check that it really was deleted:
    {error, noSuchObject} = eldap:delete(H, DN),

    %% And restore the object for subsequent tests
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
delete_referral(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    %% The element to play with:
    DN = "cn=Jonas Jonsson,dc=notHere," ++ BasePath,
    {ok,{referral,["ldap://nowhere.example.com"++_]}} = eldap:delete(H, DN).

%%%----------------------------------------------------------------
modify_dn_delete_old(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    OrigCN = "Foo Bar",
    OriginalRDN = "cn="++OrigCN,
    DN = OriginalRDN ++ "," ++ BasePath,
    NewCN = "Niclas Andre",
    NewRDN = "cn="++NewCN,
    NewDN = NewRDN ++ "," ++BasePath,

    %% Check that the object to modify_dn of exists:
    {ok,OriginalAttrs} = attributes(H, DN),
    CN_orig = lists:sort(proplists:get_value("cn",OriginalAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),

    %% Modify and delete the old one:
    ok = eldap:modify_dn(H, DN, NewRDN, true, ""),

    %% Check that DN was modified and the old one was deleted:
    {ok,NewAttrs} = attributes(H, NewDN),
    CN_new = lists:sort(proplists:get_value("cn",NewAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=NewDN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),
    %% What we expect:
    CN_new = lists:sort([NewCN | CN_orig -- [OrigCN]]),

    %% Change back:
    ok = eldap:modify_dn(H, NewDN, OriginalRDN, true, ""),

    %% Check that DN was modified and the new one was deleted:
    {ok,SameAsOriginalAttrs} = attributes(H, DN),
    CN_orig = lists:sort(proplists:get_value("cn",SameAsOriginalAttrs)),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}).

%%%----------------------------------------------------------------
modify_dn_keep_old(Config) ->
    H = proplists:get_value(handle, Config),
    BasePath = proplists:get_value(eldap_path, Config),
    OriginalRDN = "cn=Foo Bar",
    DN = OriginalRDN ++ "," ++ BasePath,
    NewCN = "Niclas Andre",
    NewRDN = "cn="++NewCN,
    NewDN = NewRDN ++ "," ++BasePath,

    %% Check that the object to modify_dn of exists but the new one does not:
    {ok,OriginalAttrs} = attributes(H, DN),
    {ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN}]}} =
	eldap:search(H,
		     #eldap_search{base = BasePath,
				   filter = eldap:substrings("sn", [{any, "a"}]),
				   scope = eldap:singleLevel()}),

    %% Modify but keep the old "cn" attr:
    ok = eldap:modify_dn(H, DN, NewRDN, false, ""),

    %% Check that DN was modified and the old CN entry is not deleted:
    {ok,NewAttrs} = attributes(H, NewDN),
    CN_orig = proplists:get_value("cn",OriginalAttrs),
    CN_new = proplists:get_value("cn",NewAttrs),
    Expected = lists:sort([NewCN|CN_orig]),
    Expected = lists:sort(CN_new),

    %% Restore db:
    ok = eldap:delete(H, NewDN),
    restore_original_object(H, DN, OriginalAttrs).

%%%----------------------------------------------------------------
%%% Test that start_tls on an already upgraded connection makes no noise
start_tls_twice_should_fail(Config) ->
    {ok,H} = open_bind(Config),
    {error,tls_already_started} = eldap:start_tls(H, []),
    eldap:close(H).

%%%----------------------------------------------------------------
%%% Test that start_tls on an ldaps connection fails
start_tls_on_ssl_should_fail(Config) ->
    {ok,H} = open_bind(Config),
    {error,tls_already_started} = eldap:start_tls(H, []),
    eldap:close(H).

%%%----------------------------------------------------------------
encode(_Config) ->
    {ok,Bin} = 'ELDAPv3':encode('AddRequest', #'AddRequest'{entry="hejHopp"  ,attributes=[]} ),
    Expected = <<104,11,4,7,104,101,106,72,111,112,112,48,0>>,
    case Bin of
	Expected -> ok;
	_ -> ct:log("Encoded erroneously to:~n~p~nExpected:~n~p",[Bin,Expected]),
	     {fail, "Bad encode"}
    end.

%%%----------------------------------------------------------------
decode(_Config) ->
    {ok,Res} = 'ELDAPv3':decode('AddRequest', <<104,11,4,7,104,101,106,72,111,112,112,48,0>>),
    ct:log("Res = ~p", [Res]),
    Expected = #'AddRequest'{entry = "hejHopp",attributes = []},
    case Res of
	Expected -> ok;
	#'AddRequest'{entry= <<"hejHopp">>, attributes=[]} ->
	    {fail, "decoded to (correct) binary!!"};
	_ ->
	    {fail, "Bad decode"}
    end.



%%%****************************************************************
%%% Private

attributes(H, DN) ->
    case eldap:search(H,
		     #eldap_search{base  = DN,
				   filter= eldap:present("objectclass"),
				   scope = eldap:wholeSubtree()}) of
	{ok, #eldap_search_result{entries=[#eldap_entry{object_name=DN,
							attributes=OriginalAttrs}]}} ->
	    {ok, OriginalAttrs};
	Other ->
	    Other
    end.

restore_original_object(H, DN, Attrs) ->
    eldap:delete(H, DN),
    ok = eldap:add(H, DN, Attrs).


find_first_server(UseSSL, [{config,Key}|Ss]) ->
    case ct:get_config(Key) of
	{Host,Port} ->
	    ct:log("find_first_server config ~p -> ~p",[Key,{Host,Port}]),
	    find_first_server(UseSSL, [{Host,Port}|Ss]);
	undefined ->
	    ct:log("find_first_server config ~p is undefined",[Key]),
	    find_first_server(UseSSL, Ss)
    end;
find_first_server(UseSSL, [{Host,Port}|Ss]) ->
    case eldap:open([Host],[{port,Port},{ssl,UseSSL}]) of
	{ok,H} when UseSSL==false, Ss=/=[] ->
	    case eldap:start_tls(H,[]) of
		ok ->
		    ct:log("find_first_server ~p UseSSL=~p -> ok",[{Host,Port},UseSSL]),
		    eldap:close(H),
		    {Host,Port};
		Res ->
		    ct:log("find_first_server ~p UseSSL=~p failed with~n~p~nSave as spare host.",[{Host,Port},UseSSL,Res]),
		    eldap:close(H),
		    find_first_server(UseSSL, Ss++[{spare_host,Host,Port}])
	    end;
	{ok,H} ->
	    ct:log("find_first_server ~p UseSSL=~p -> ok",[{Host,Port},UseSSL]),
	    eldap:close(H),
	    {Host,Port};
	Res ->
	    ct:log("find_first_server ~p UseSSL=~p failed with~n~p",[{Host,Port},UseSSL,Res]),
	    find_first_server(UseSSL, Ss)
    end;
find_first_server(false, [{spare_host,Host,Port}|_]) ->
    ct:log("find_first_server can't find start_tls host, use the spare non-start_tls host for plain ldap: ~p",[{Host,Port}]),
    {Host,Port};
find_first_server(_, []) ->
    ct:log("find_first_server, nothing left to try",[]),
    undefined.

initialize_db(Config) ->
    case {open_bind(Config), inet:gethostname()} of
	{{ok,H}, {ok,MyHost}} ->
	    Path = "dc="++MyHost++",dc=ericsson,dc=se",
	    delete_old_contents(H, Path),
	    add_new_contents(H, Path, MyHost),
	    eldap:close(H),
	    [{eldap_path,Path}|Config];
	Other ->
	    ct:fail("initialize_db failed: ~p",[Other])
    end.

clear_db(Config) ->
    {ok,H} = open_bind(Config),
    Path = proplists:get_value(eldap_path, Config),
    delete_old_contents(H, Path),
    eldap:close(H),
    Config.

delete_old_contents(H, Path) ->
    case eldap:search(H, [{base,  Path},
			  {filter, eldap:present("objectclass")},
			  {scope,  eldap:wholeSubtree()}])
    of
	{ok, _R=#eldap_search_result{entries=Entries}} ->
	    case eldap:delete(H, "dc=notHere,"++Path, [?manageDsaIT]) of
		ok -> ok;
		{error,noSuchObject} -> ok;
		Other -> ct:fail("eldap:delete notHere ret ~p",[Other])
	    end,
	    [ok = eldap:delete(H,DN) || #eldap_entry{object_name=DN} <- Entries];
	_Res ->
	    ignore
    end.


-define(ok(X), ok(?MODULE,?LINE,X)).

add_new_contents(H, Path, MyHost) ->
    ?ok(eldap:add(H,"dc=ericsson,dc=se",
		 [{"objectclass", ["dcObject", "organization"]},
		  {"dc", ["ericsson"]},
		  {"o", ["Testing"]}])),
    ?ok(eldap:add(H,Path,
		 [{"objectclass", ["dcObject", "organization"]},
		  {"dc", [MyHost]},
		  {"o", ["Test machine"]}])),
    ?ok(eldap:add(H,  "dc=notHere,"++Path,
		  [{"objectclass", ["referral", 
				    "dcObject"
				   ]},
		   {"ref", ["ldap://nowhere.example.com/notHere,"++Path]},
		   {"dc", ["notHere"]}
		  ])).



ok(_, _, {error,entryAlreadyExists}) -> ok;
ok(_, _, ok) -> ok;
ok(MODULE, LINE, X) -> 
    ct:pal("~p:~p add_new_contents: ret from eldap:add = ~p",[MODULE,LINE,X]),
    X.



cond_start_tls(H, Config) ->
    case proplists:get_value(start_tls,Config) of
	true -> start_tls(H,Config);
	_ -> Config
    end.

start_tls(H, Config) ->
    KeyFile = filename:join([proplists:get_value(data_dir,Config),
			     "certs/client/key.pem"
			    ]),
    case eldap:start_tls(H, [{keyfile, KeyFile}]) of
	ok ->
	    [{start_tls_success,true} | Config];
	Error ->
	    ct:log("Start_tls on ~p failed: ~p",[proplists:get_value(url,Config) ,Error]),
	    ct:fail("start_tls failed")
    end.


%%%----------------------------------------------------------------
open_bind(Config) ->
    {ok,H} = open(Config),
    ok = eldap:simple_bind(H, "cn=Manager,dc=ericsson,dc=se", "hejsan"),
    {ok,H}.

open(Config) ->
    {Host,Port} = proplists:get_value(server,Config),
    SSLflag = proplists:get_value(ssl_flag,Config),
    {ok,H} = eldap:open([Host], [{port,Port},{ssl,SSLflag}]),
    cond_start_tls(H, Config),
    {ok,H}.

%%%----------------------------------------------------------------
supported_extension(OID, Config) ->
    {ok,H} = open_bind(Config),
    case eldap:search(H, [{scope,  eldap:baseObject()},
			  {filter, eldap:present("objectclass")},
			  {deref,  eldap:neverDerefAliases()},
			  {attributes, ["+"]}]) of
	{ok,R=#eldap_search_result{}} ->
	    eldap:close(H),
	    lists:member(OID,
			 [SE || EE <- R#eldap_search_result.entries,
				{"supportedExtension",SEs} <- EE#eldap_entry.attributes,
				SE<-SEs]);
	_ ->
	    eldap:close(H),
	    false
    end.

%%%----------------------------------------------------------------
client_timeout(Fun, Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(tcp_connect_opts, Config),
    T = 1000,
    case eldap:open([Host], [{timeout,T},{port,Port}|Opts]) of
	{ok,H} ->
	    T0 = erlang:monotonic_time(),
	    {error,{gen_tcp_error,timeout}} = Fun(H),
	    T_op = ms_passed(T0),
	    ct:log("Time = ~p, Timeout spec = ~p",[T_op,T]),
	    if
		T_op < T ->
		    {fail, "Timeout too early"};
		true ->
		    ok
	    end;

	Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

%% Help function, elapsed milliseconds since T0
ms_passed(T0) ->
    %% OTP 18
    erlang:convert_time_unit(erlang:monotonic_time() - T0,
			     native,
			     micro_seconds) / 1000.

%%%----------------------------------------------------------------
init_ssl_certs_et_al(Config) ->
    try ssl:start()
    of
	R when R==ok ; R=={error,{already_started,ssl}} ->
	    try make_certs:all("/dev/null",
			       filename:join(proplists:get_value(data_dir,Config), "certs"))
	    of
		{ok,_} -> true;
		Other ->
		    ct:comment("make_certs failed"),
		    ct:log("make_certs failed ~p", [Other]),
		    false
	    catch
		C:E ->
		    ct:comment("make_certs crashed"),
		    ct:log("make_certs failed ~p:~p", [C,E]),
		    false
	    end;
	_ ->
	    false
    catch
	Error:Reason ->
	    ct:comment("ssl failed to start"),
	    ct:log("init_per_suite failed to start ssl Error=~p Reason=~p", [Error, Reason]),
	    false
    end.
