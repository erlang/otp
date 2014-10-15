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
    [{v4, [], [tcp_connection, tcp_connection_option]},
     {v6, [], [tcp_connection, tcp_connection_option]}
    ].


init_per_suite(Config) -> Config.


end_per_suite(_Config) -> ok.


init_per_testcase(_TestCase, Config) ->
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
