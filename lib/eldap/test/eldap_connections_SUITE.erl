%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2014. All Rights Reserved.
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

-module(eldap_connections_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
%-include_lib("eldap/include/eldap.hrl").


all() ->
    [
     {group, v4},
     {group, v6}
    ].

     
init_per_group(v4, Config) ->
    [{listen_opts,  []},
     {listen_host,  "localhost"},
     {connect_opts, []}
     |  Config];
init_per_group(v6, Config) ->
    {ok, Hostname} = inet:gethostname(),
    case lists:member(list_to_atom(Hostname), ct:get_config(ipv6_hosts,[])) of
	true -> 
	    [{listen_opts,  [inet6]},
	     {listen_host,  "::"},
	     {connect_opts, [{tcpopts,[inet6]}]}
	     |  Config];
	false -> 
	    {skip, io_lib:format("~p is not an ipv6_host",[Hostname])}
    end.


end_per_group(_GroupName, Config) ->
    Config.


groups() ->
    [{v4, [], tests()},
     {v6, [], tests()}
    ].

tests() ->
    [tcp_connection, 
     tcp_connection_option, 
     ssl_connection,
     client_side_start_tls_timeout, 
     client_side_bind_timeout, 
     client_side_add_timeout,
     client_side_search_timeout
    ].

init_per_suite(Config) -> 
    HasSSL = init_ssl_certs_et_al(Config),
    [{has_ssl,HasSSL} | Config].

end_per_suite(_Config) -> ok.


init_per_testcase(ssl_connection, Config) ->
    case ?config(has_ssl,Config) of
	true ->
	    SSL_Port = 9999,
	    CertFile = filename:join(?config(data_dir,Config), "certs/server/cert.pem"),
	    KeyFile = filename:join(?config(data_dir,Config), "certs/server/key.pem"),

	    Parent = self(),
	    Listener = spawn_link(
			 fun() ->
				 case ssl:listen(SSL_Port, [{certfile, CertFile},
							    {keyfile, KeyFile},
							    {reuseaddr, true}]) of
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

init_per_testcase(_, Config) ->
    case gen_tcp:listen(0, proplists:get_value(listen_opts,Config)) of
	{ok,LSock} ->
	    {ok,{_,Port}} = inet:sockname(LSock),
	    [{listen_socket,LSock},
	     {listen_port,Port}
	     | Config];
	Other ->
	    {fail, Other}
    end.


end_per_testcase(_TestCase, Config) ->
    catch gen_tcp:close( proplists:get_value(listen_socket, Config) ).

%%%================================================================
%%%
%%% Test cases
%%% 
%%%----------------------------------------------------------------
tcp_connection(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(connect_opts, Config),
    case eldap:open([Host], [{port,Port}|Opts]) of
	{ok,_H} ->
	    Sl = proplists:get_value(listen_socket, Config),
	    case gen_tcp:accept(Sl,1000) of
		{ok,_S} -> ok;
		{error,timeout} -> ct:fail("server side accept timeout",[])
	    end;
	Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

%%%----------------------------------------------------------------
ssl_connection(Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(ssl_listen_port, Config),
    Opts = proplists:get_value(connect_opts, Config),
    SSLOpts = proplists:get_value(ssl_connect_opts, Config),
    case eldap:open([Host], [{port,Port},{ssl,true},
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
    Opts = proplists:get_value(connect_opts, Config),
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


%%%================================================================
%%%
%%% Private
%%% 

client_timeout(Fun, Config) ->
    Host = proplists:get_value(listen_host, Config),
    Port = proplists:get_value(listen_port, Config),
    Opts = proplists:get_value(connect_opts, Config),
    T = 1000,
    case eldap:open([Host], [{timeout,T},{port,Port}|Opts]) of
	{ok,H} -> 
	    T0 = now(),
	    {error,{gen_tcp_error,timeout}} = Fun(H),
	    T_op = diff(T0,now()),
	    ct:log("Time = ~p, Timeout spec = ~p",[T_op,T]),
	    if 
		T_op < T -> 
		    {fail, "Timeout too early"};
		true ->
		    ok
	    end;
		    
	Other -> ct:fail("eldap:open failed: ~p",[Other])
    end.

diff({M1,S1,U1},{M2,S2,U2}) ->
    ( ((M2-M1)*1000 + (S2-S1))*1000 + (U2-U1) ).
%%%----------------------------------------------------------------
init_ssl_certs_et_al(Config) ->
    try ssl:start()
    of
	R when R==ok ; R=={error,{already_started,ssl}} ->
	    try make_certs:all("/dev/null", 
			       filename:join(?config(data_dir,Config), "certs"))
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
