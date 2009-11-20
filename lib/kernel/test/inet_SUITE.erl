%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(inet_SUITE).

-include("test_server.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([all/1, t_gethostbyaddr/1, t_getaddr/1, t_gethostbyname/1,
	 t_gethostbyaddr_v6/1, t_getaddr_v6/1, t_gethostbyname_v6/1,
	 ipv4_to_ipv6/1, host_and_addr/1, parse/1, t_gethostnative/1, 
	 gethostnative_parallell/1, cname_loop/1, 
         gethostnative_soft_restart/1,gethostnative_debug_level/1,getif/1]).

-export([get_hosts/1, get_ipv6_hosts/1, parse_hosts/1, 
	 kill_gethost/0, parallell_gethost/0]).
-export([init_per_testcase/2, end_per_testcase/2]).


all(suite) ->
    [t_gethostbyaddr, t_gethostbyname, t_getaddr, 
     t_gethostbyaddr_v6, t_gethostbyname_v6, t_getaddr_v6, 
     ipv4_to_ipv6, host_and_addr, parse,t_gethostnative, 
     gethostnative_parallell, cname_loop,
     gethostnative_debug_level,gethostnative_soft_restart,
     getif].

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).


t_gethostbyaddr(doc) -> "Test the inet:gethostbyaddr/1 function.";
t_gethostbyaddr(Config) when is_list(Config) ->
    ?line {Name,FullName,IPStr,IP,Aliases,_,_} = ?config(test_host_ipv4_only, Config),
    ?line {ok,HEnt} = inet:gethostbyaddr(IPStr),
    ?line {ok,HEnt} = inet:gethostbyaddr(IP),
    ?line {error,Error} = inet:gethostbyaddr(Name),
    ?line ok = io:format("Failure reason: ~p: ~s",
			 [error,inet:format_error(Error)]),
    ?line HEnt_ = HEnt#hostent{h_addrtype = inet,
			       h_length = 4,
			       h_addr_list = [IP]},
    ?line HEnt_ = HEnt,
    case {os:type(),os:version()} of
	{{unix,freebsd},{5,0,0}} ->
	    %% The alias list seems to be buggy in FreeBSD 5.0.0.
	    ?line check_elems([{HEnt#hostent.h_name,[Name,FullName]}]),
	    io:format("Buggy alias list: ~p", [HEnt#hostent.h_aliases]),
	    ok;
	_ ->
	    ?line check_elems([{HEnt#hostent.h_name,[Name,FullName]},
			       {HEnt#hostent.h_aliases,[[],Aliases]}])
    end,

    ?line {_DName, _DFullName, DIPStr, DIP, _, _, _} =
	?config(test_dummy_host, Config),
    ?line {error,nxdomain} = inet:gethostbyaddr(DIPStr),
    ?line {error,nxdomain} = inet:gethostbyaddr(DIP),
    ok.

t_gethostbyaddr_v6(doc) -> "Test the inet:gethostbyaddr/1 inet6 function.";
t_gethostbyaddr_v6(Config) when is_list(Config) ->
    ?line {Name6, FullName6, IPStr6, IP6, Aliases6} =
	?config(test_host_ipv6_only, Config),

    ?line case inet:gethostbyaddr(IPStr6) of
        %% Even if IPv6 is not supported, the native resolver may succeed
	%% looking up the host. DNS lookup will probably fail.
	{error,nxdomain} ->
	    {skip, "IPv6 test fails! IPv6 not supported on this host!?"};
	{ok,HEnt6} ->
	    ?line {ok,HEnt6} = inet:gethostbyaddr(IP6),
	    ?line {error,Error6} = inet:gethostbyaddr(Name6),
	    ?line ok = io:format("Failure reason: ~p: ~s",
				 [Error6, inet:format_error(Error6)]),
	    ?line HEnt6_ = HEnt6#hostent{h_addrtype = inet6,
					 h_length = 16,
					 h_addr_list = [IP6]},
	    ?line HEnt6_ = HEnt6,
	    ?line check_elems([{HEnt6#hostent.h_name,[Name6,FullName6]},
			       {HEnt6#hostent.h_aliases,[[],Aliases6]}]),

	    ?line {_DName6, _DFullName6, DIPStr6, DIP6, _} =
		      ?config(test_dummy_ipv6_host, Config),
	    ?line {error,nxdomain} = inet:gethostbyaddr(DIPStr6),
	    ?line {error,nxdomain} = inet:gethostbyaddr(DIP6),    
	    ok
    end.

t_gethostbyname(doc) -> "Test the inet:gethostbyname/1 function.";
t_gethostbyname(suite) -> [];
t_gethostbyname(Config) when is_list(Config) ->
    ?line {Name,FullName,IPStr,IP,Aliases,IP_46_Str,_} =
	?config(test_host_ipv4_only, Config),
    ?line {ok,_} = inet:gethostbyname(IPStr),
    ?line {ok,HEnt} = inet:gethostbyname(Name),
    ?line {ok,HEnt} = inet:gethostbyname(list_to_atom(Name)),
    ?line HEnt_ = HEnt#hostent{h_addrtype = inet,
			       h_length = 4,
			       h_addr_list = [IP]},
    ?line HEnt_ = HEnt,
    ?line check_elems([{HEnt#hostent.h_name,[Name,FullName]},
		       {HEnt#hostent.h_aliases,[[],Aliases]}]),

    ?line {ok,HEntF} = inet:gethostbyname(FullName),
    ?line HEntF_ = HEntF#hostent{h_name = FullName,
				 h_addrtype = inet,
				 h_length = 4,
				 h_addr_list = [IP]},
    ?line HEntF_ = HEntF,
    ?line check_elems([{HEnt#hostent.h_aliases,[[],Aliases]}]),

    ?line {DName, _DFullName, _DIPStr, _DIP, _, _, _} =
	?config(test_dummy_host, Config),
    ?line {error,nxdomain} = inet:gethostbyname(DName),
    ?line {error,nxdomain} = inet:gethostbyname(IP_46_Str).

t_gethostbyname_v6(doc) -> "Test the inet:gethostbyname/1 inet6 function.";
t_gethostbyname_v6(suite) -> [];
t_gethostbyname_v6(Config) when is_list(Config) ->
    ?line {Name, _, _, _,Aliases,IP_46_Str,IP_46} =
	?config(test_host_ipv4_only, Config),

    case {inet:gethostbyname(IP_46_Str, inet6),
	  inet:gethostbyname(Name, inet6)} of
	{{ok,HEnt46},{ok,_}} -> 
	    ?line HEnt46_ = HEnt46#hostent{h_name = IP_46_Str,
					   h_addrtype = inet6,
					   h_length = 16,
					   h_addr_list = [IP_46]},
	    ?line HEnt46_ = HEnt46,
	    ?line check_elems([{HEnt46#hostent.h_aliases,[[],Aliases]}]),		  

	    ?line {Name6, FullName6, IPStr6, IP6, Aliases6} =
		      	?config(test_host_ipv6_only, Config),
	    ?line {ok,_} = inet:gethostbyname(IPStr6, inet6),
	    ?line {ok,HEnt6} = inet:gethostbyname(Name6, inet6),
	    ?line {ok,HEnt6} = inet:gethostbyname(list_to_atom(Name6), inet6),
	    ?line case HEnt6#hostent.h_addr_list of
		      [IP6] ->				% ipv6 ok
			  ?line HEnt6_ = HEnt6#hostent{h_addrtype = inet6,
						       h_length = 16,
						       h_addr_list = [IP6]},
			  ?line HEnt6_ = HEnt6,	    
			  ?line check_elems([{HEnt6#hostent.h_name,[Name6,FullName6]},
					     {HEnt6#hostent.h_aliases,[[],Aliases6]}]);
		      _ ->				% ipv4 compatible addr
			  ?line {ok,HEnt4} = inet:gethostbyname(Name6, inet),
			  ?line [IP4] = HEnt4#hostent.h_addr_list,
			  ?line {ok,IP46_2} = 
			      inet_parse:ipv6_address("::ffff:"++inet_parse:ntoa(IP4)),
			  ?line HEnt6_ = HEnt6#hostent{h_addrtype = inet6,
						       h_length = 16,
						       h_addr_list = [IP46_2]},
			  ?line HEnt6_ = HEnt6,
			  ?line check_elems([{HEnt6#hostent.h_name,[Name6,FullName6]}])
		  end,
	    
	    ?line {ok,HEntF6} = inet:gethostbyname(FullName6, inet6),
	    ?line case HEntF6#hostent.h_addr_list of
		      [IP6] ->				% ipv6 ok
			  ?line HEntF6_ = HEntF6#hostent{h_name = FullName6,
							 h_addrtype = inet6,
							 h_length = 16,
							 h_addr_list = [IP6]},
			  ?line HEntF6_ = HEntF6,
			  ?line check_elems([{HEntF6#hostent.h_aliases,[[],Aliases6]}]);
		      _ ->				% ipv4 compatible addr
			  ?line {ok,HEntF4} = inet:gethostbyname(FullName6, inet),
			  ?line [IPF4] = HEntF4#hostent.h_addr_list,
			  ?line {ok,IPF46_2} = 
			      inet_parse:ipv6_address("::ffff:"++inet_parse:ntoa(IPF4)),
			  ?line HEntF6_ = HEntF6#hostent{h_addrtype = inet6,
							 h_length = 16,
							 h_addr_list = [IPF46_2]},
			  ?line HEntF6_ = HEntF6,
			  ?line check_elems([{HEntF6#hostent.h_name,[Name6,FullName6]}])
		  end,
	    
	    ?line {DName6, _DFullName6, _DIPStr6, _DIP6, _} =
		      ?config(test_dummy_ipv6_host, Config),
	    ?line {error,nxdomain} = inet:gethostbyname(DName6, inet6),
	    ok;
	{_,_} ->
	    {skip, "IPv6 is not supported on this host"}
    end.

check_elems([{Val,Tests} | Elems]) ->
    check_elem(Val, Tests, Tests),
    check_elems(Elems);
check_elems([]) -> ok.

check_elem(Val, [Val|_], _) -> ok;
check_elem(Val, [_|Tests], Tests0) ->
    check_elem(Val, Tests, Tests0);
check_elem(Val, [], Tests0) ->
    ?t:fail({no_match,Val,Tests0}).


t_getaddr(doc) -> "Test the inet:getaddr/2 function.";
t_getaddr(suite) -> [];
t_getaddr(Config) when is_list(Config) ->
    ?line {Name,FullName,IPStr,IP,_,IP_46_Str,IP46} =
	?config(test_host_ipv4_only, Config),
    ?line {ok,IP} = inet:getaddr(list_to_atom(Name), inet),
    ?line {ok,IP} = inet:getaddr(Name, inet),
    ?line {ok,IP} = inet:getaddr(FullName, inet),
    ?line {ok,IP} = inet:getaddr(IP, inet),
    ?line {ok,IP} = inet:getaddr(IPStr, inet),
    ?line {error,nxdomain} = inet:getaddr(IP_46_Str, inet),
    ?line {error,eafnosupport} = inet:getaddr(IP46, inet),

    ?line {DName, DFullName, DIPStr, DIP, _, _, _} = ?config(test_dummy_host, Config),
    ?line {error,nxdomain} = inet:getaddr(DName, inet),
    ?line {error,nxdomain} = inet:getaddr(DFullName, inet),
    ?line {ok,DIP} = inet:getaddr(DIPStr, inet),
    ?line {ok,DIP} = inet:getaddr(DIP, inet).

t_getaddr_v6(doc) -> "Test the inet:getaddr/2 function.";
t_getaddr_v6(suite) -> [];
t_getaddr_v6(Config) when is_list(Config) ->
    ?line {Name,FullName,IPStr,_IP,_,IP_46_Str,IP46} =
	?config(test_host_ipv4_only, Config),
    case {inet:getaddr(IP_46_Str, inet6),inet:getaddr(Name, inet6)} of
	{{ok,IP46},{ok,_}} -> 
	    %% Since we suceeded in parsing an IPv6 address string and
	    %% look up the name, this computer fully supports IPv6.
	    ?line {ok,IP46} = inet:getaddr(IP46, inet6),
	    ?line {ok,IP46} = inet:getaddr(Name, inet6),
	    ?line {ok,IP46} = inet:getaddr(FullName, inet6),
	    ?line IP4toIP6 = inet:getaddr(IPStr, inet6),
	    ?line case IP4toIP6 of
		      {ok,IP46} ->		% only native can do this
			  ?line true = lists:member(native,
						    inet_db:res_option(lookup));
		      {error,nxdomain} ->
			  ok
		  end,
	    ?line {Name6, FullName6, IPStr6, IP6, _} =
				      	?config(test_host_ipv6_only, Config),
	    ?line {ok,_} = inet:getaddr(list_to_atom(Name6), inet6),
	    ?line {ok,_} = inet:getaddr(Name6, inet6),
	    ?line {ok,_} = inet:getaddr(FullName6, inet6),
	    ?line {ok,IP6} = inet:getaddr(IP6, inet6),
	    ?line {ok,IP6} = inet:getaddr(IPStr6, inet6),

	    ?line {DName6, DFullName6, DIPStr6, DIP6, _} =
		?config(test_dummy_ipv6_host, Config),
	    ?line {error,nxdomain} = inet:getaddr(DName6, inet6),
	    ?line {error,nxdomain} = inet:getaddr(DFullName6, inet6),
	    ?line {ok,DIP6} = inet:getaddr(DIPStr6, inet6),
	    ?line {ok,DIP6} = inet:getaddr(DIP6, inet6),
	    ok;
	{_,_} ->
 	    {skip, "IPv6 is not supported on this host"}
    end.

ipv4_to_ipv6(doc) -> "Test if IPv4 address is converted to IPv6 address.";
ipv4_to_ipv6(suite) -> [];
ipv4_to_ipv6(Config) when is_list(Config) ->
    %% Test what happens if an IPv4 address is looked up in an IPv6 context.
    %% If the native resolver succeeds to look it up, an IPv4 compatible
    %% address should be returned. If no IPv6 support on this host, an
    %% error should beturned.
    ?line {_Name,_FullName,IPStr,_IP,Aliases,IP_46_Str,IP_46} =
	?config(test_host_ipv4_only, Config),
    ?line IP4to6Res =
	case inet:getaddr(IPStr, inet6) of
	    {ok,IP_46} ->
		io:format("IPv4->IPv6: success~n"),
		true;
	    E = {error,nxdomain} ->
		io:format("IPv4->IPv6: nxdomain~n"),
		E;
	    E = {error,eafnosupport} ->
		io:format("IPv6->IPv4: eafnosupport~n"),
		E;
	    Other ->
		?line ?t:fail({ipv4_to_ipv6_lookup_failed,Other})
	end,
    ?line case {IP4to6Res,inet:gethostbyname(IPStr, inet6)} of
	      {true,{ok,HEnt}} ->
		  ?line true = lists:member(native, inet_db:res_option(lookup)),
		  ?line HEnt_ = HEnt#hostent{h_addrtype = inet6,
					     h_length = 16,
					     h_addr_list = [IP_46]},
		  ?line HEnt_ = HEnt,
		  ?line check_elems([{HEnt#hostent.h_name,[IP_46_Str,IPStr]},
				     {HEnt#hostent.h_aliases,[[],Aliases]}]);
	      {_,IP4to6Res} -> ok
	  end,
    ok.

host_and_addr(doc) -> ["Test looking up hosts and addresses. Use 'ypcat hosts' ",
		       "or the local eqivalent to find all hosts."];
host_and_addr(suite) -> [];
host_and_addr(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(5)),

    ?line lists:foreach(fun try_host/1, get_hosts(Config)),
    ?line test_server:timetrap_cancel(Dog),
    ok.

try_host({Ip0, Host}) ->
    ?line {ok,Ip} = inet:getaddr(Ip0, inet),
    ?line {ok,{hostent, _, _, inet, _, Ips1}} = inet:gethostbyaddr(Ip),
    ?line {ok,{hostent, _, _, inet, _, _Ips2}} = inet:gethostbyname(Host),
    ?line true = lists:member(Ip, Ips1),
    ok.

%% Get all hosts from the system using 'ypcat hosts' or the local
%% equvivalent.

get_hosts(Config) -> 
    case os:type() of
	{unix, _} ->
	    List = lists:map(fun(X) ->
				     atom_to_list(X)++" "
			     end, ?config(test_hosts, Config)),
	    Cmd = "ypmatch "++List++" hosts.byname", 
	    HostFile = os:cmd(Cmd),
	    get_hosts(HostFile, [], [], []);
	_ -> 
	    ?config(hardcoded_hosts, Config)
    end.

get_ipv6_hosts(Config) -> 
    case os:type() of
	{unix, _} ->
	    List = lists:map(fun(X) ->
				     atom_to_list(X)++" "
			     end, ?config(test_hosts, Config)),
	    Cmd = "ypmatch "++List++" ipnodes.byname", 
	    HostFile = os:cmd(Cmd),
	    get_hosts(HostFile, [], [], []);
	_ -> 
	    ?config(hardcoded_ipv6_hosts, Config)
    end.

get_hosts([$\t|Rest], Cur, Ip, Result) when Ip /= [] ->
    get_hosts(Rest, Cur, Ip, Result);
get_hosts([$\t|Rest], Cur, _Ip, Result) ->
    get_hosts(Rest, [], lists:reverse(Cur), Result);
get_hosts([$\r|Rest], Cur, Ip, Result) ->
    get_hosts(Rest, Cur, Ip, Result);
get_hosts([$\n|Rest], Cur, Ip, Result) ->
    [First|_] = string:tokens(lists:reverse(Cur), " "),
    Ips = string:tokens(Ip, ","),
    Hosts = [{I, First} || I <- Ips],
    get_hosts(Rest, [], [], Hosts++Result);
get_hosts([C|Rest], Cur, Ip, Result) ->
    get_hosts(Rest, [C|Cur], Ip, Result);
get_hosts([], _, _, Result) ->
    Result.
    
parse(suite) -> [parse_hosts];
parse(doc) -> ["Test that parsing of the hosts file or equivalent works,",
	       "and that erroneous lines are skipped"].
parse_hosts(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line HostFile = filename:join(DataDir, "hosts"),
    ?line inet_parse:hosts(HostFile),
    ?line HostFileErr1 = filename:join(DataDir, "hosts_err1"),
    ?line inet_parse:hosts(HostFileErr1),
    ?line Resolv = filename:join(DataDir,"resolv.conf"),
    ?line inet_parse:resolv(Resolv),
    ?line ResolvErr1 = filename:join(DataDir,"resolv.conf.err1"),
    ?line inet_parse:resolv(ResolvErr1).

t_gethostnative(suite) ->[];
t_gethostnative(doc) ->[];
t_gethostnative(Config) when is_list(Config) ->
%% this will result in 26 bytes sent which causes problem in Windows
%% if the port-program has not assured stdin to be read in BINARY mode
%% OTP-2555
    case os:type() of
	vxworks ->
	    {skipped, "VxWorks has no native gethostbyname()"};
	_ ->
	    ?line case inet_gethost_native:gethostbyname(
			 "a23456789012345678901234") of
		      {error,notfound} ->
			  ?line ok;
		      {error,no_data} ->
			  ?line ok
		  end
    end.

gethostnative_parallell(suite) ->    
    [];
gethostnative_parallell(doc) ->
    ["Check that the emulator survives crashes in gethost_native"];
gethostnative_parallell(Config) when is_list(Config) ->
    ?line {ok,Hostname} = inet:gethostname(),
    ?line {ok,_} = inet:gethostbyname(Hostname),
    case whereis(inet_gethost_native) of
	Pid when is_pid(Pid) ->
	    ?line do_gethostnative_parallell();
	_ ->
	    ?line {skipped, "Not running native gethostbyname"}
    end.

do_gethostnative_parallell() ->
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok,Node} = ?t:start_node(gethost_parallell, slave, 
				     [{args, "-pa " ++ PA}]),
    ?line ok = rpc:call(Node, ?MODULE, parallell_gethost, []),
    ?line receive after 10000 -> ok end,
    ?line pong = net_adm:ping(Node),
    ?line ?t:stop_node(Node),
    ok.

parallell_gethost() ->
    {ok,Hostname} = inet:gethostname(),
    process_flag(trap_exit,true),
    parallell_gethost_loop(10, Hostname).

parallell_gethost_loop(0, _) -> ok;
parallell_gethost_loop(N, Hostname) ->
    case whereis(inet_gethost_native) of
	Pid when is_pid(Pid) ->
	    true = exit(Pid,kill);
	_ ->
	    ok
    end,

    L = spawn_gethosters(Hostname, 10),
    release_gethosters(L),
    collect_gethosters(10),
    parallell_gethost_loop(N-1, Hostname).

spawn_gethosters(_, 0) ->
    [];
spawn_gethosters(Hostname, N) ->
    Collector = self(),
    [spawn(fun() ->
		   receive 
		       go ->
			   case (catch inet:gethostbyname(Hostname)) of
			       {ok,_} ->
				   Collector ! ok;
			       Else ->
				   Collector ! {error,Else}
			   end
		   end 
	   end) | 
     spawn_gethosters(Hostname, N-1)].

release_gethosters([]) ->
    ok;
release_gethosters([H|T]) ->
    H ! go,
    release_gethosters(T).

collect_gethosters(0) ->
    ok;
collect_gethosters(N) ->
    receive
	ok ->
	    collect_gethosters(N-1);
	Else ->
	    {failed, {unexpected, Else}}
    after 2000 ->
	    {failed, {missing, N}}
    end.

kill_gethost() ->
    kill_gethost(20).

kill_gethost(0) ->
    ok;
kill_gethost(N) ->
    put(kill_gethost_n,N),
    Pid = wait_for_gethost(10),
    true = exit(Pid,kill),
    wait_for_dead_gethost(10),
    kill_gethost(N-1).

wait_for_dead_gethost(0) ->
    exit({not_dead,inet_gethost_native});
wait_for_dead_gethost(N) ->
    case whereis(inet_gethost_native) of
	Pid when is_pid(Pid) ->
	    receive after 1000 ->
			    ok
		    end,
	    wait_for_dead_gethost(N-1);
	undefined ->
	    ok
    end.

wait_for_gethost(0) ->
    exit(gethost_not_found);
wait_for_gethost(N) ->
    {ok,Hostname} = inet:gethostname(),
    case (catch inet:gethostbyname(Hostname)) of
	{ok,_} ->
	    ok;
	Otherwise ->
	    %% This is what I call an exit tuple :)
	    exit({inet,gethostbyname, returned, Otherwise, 'when',
		  'N','=',N,'and','hostname','=',Hostname,'and',
		 kill_gethost_n,'=',get(kill_gethost_n)})
    end,
    case whereis(inet_gethost_native) of
	Pid when is_pid(Pid) ->
	    Pid;
	_ ->
	    receive
	    after 1000 ->
		    ok
	    end,
	    wait_for_gethost(N-1)
    end.
	    
cname_loop(suite) ->
    [];
cname_loop(doc) ->
    ["Check that the resolver handles a CNAME loop"];
cname_loop(Config) when is_list(Config) ->
    %% getbyname (hostent_by_domain)
    ?line ok = inet_db:add_rr("mydomain.com", in, ?S_CNAME, ttl, "mydomain.com"),
    ?line {error,nxdomain} = inet_db:getbyname("mydomain.com", ?S_A),
    ?line ok = inet_db:del_rr("mydomain.com", in, ?S_CNAME, "mydomain.com"),
    %% res_hostent_by_domain
    RR = #dns_rr{domain = "mydomain.com",
		 class = in,
		 type = ?S_CNAME,
		 data = "mydomain.com"},
    Rec = #dns_rec{anlist = [RR]},
    ?line {error,nxdomain} = inet_db:res_hostent_by_domain("mydomain.com", ?S_A, Rec),
    ok.



%% These must be run in the whole suite since they need
%% the host list and require inet_gethost_native to be started.
%%
-record(gethostnative_control, {control_seq,
				control_interval=100,
				lookup_delay=10,
				lookup_count=300,
				lookup_processes=20}).

gethostnative_soft_restart(suite) ->    
    [];
gethostnative_soft_restart(doc) ->
    ["Check that no name lookups fails during soft restart "
     "of inet_gethost_native"];
gethostnative_soft_restart(Config) when is_list(Config) ->
    ?line gethostnative_control(Config, 
				#gethostnative_control{
				  control_seq=[soft_restart]}).

gethostnative_debug_level(suite) ->    
    [];
gethostnative_debug_level(doc) ->
    ["Check that no name lookups fails during debug level change "
     "of inet_gethost_native"];
gethostnative_debug_level(Config) when is_list(Config) ->
    ?line gethostnative_control(Config, 
				#gethostnative_control{
				  control_seq=[{debug_level,1},
					       {debug_level,0}]}).

gethostnative_control(Config, Optrec) ->
    ?line case inet_db:res_option(lookup) of
	      [native] ->
		  case whereis(inet_gethost_native) of
		      Pid when is_pid(Pid) ->
			  ?line gethostnative_control_1(Config, Optrec);
		      _ ->
			  ?line {skipped, "Not running native gethostbyname"}
		  end;
	      _ ->
		  ?line {skipped, "Native not only lookup metod"}
	 end.

gethostnative_control_1(Config,
			#gethostnative_control{
			  control_seq=Seq,
			  control_interval=Interval,
			  lookup_delay=Delay,
			  lookup_count=Cnt,
			  lookup_processes=N}) ->
    ?line {ok, Hostname} = inet:gethostname(),
    ?line {ok, _} = inet:gethostbyname(Hostname),
    ?line Hosts = 
	[Hostname|[H || {_,H} <- get_hosts(Config)]
	 ++[H++D || H <- ["www.","www1.","www2.",""],
		    D <- ["erlang.org","erlang.se"]]
	 ++[H++"cslab.ericsson.net" || H <- ["morgoth.","hades.","styx."]]],
    %% Spawn some processes to do parallel lookups while
    %% I repeatedly do inet_gethost_native:control/1.
    ?line TrapExit = process_flag(trap_exit, true),
    ?line gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    ?line test_server:format(
	    "First intermission: now starting control sequence ~w\n",
	    [Seq]),
    ?line erlang:display(first_intermission),
    ?line gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts),
    ?line erlang:display(second_intermission),
    ?line test_server:format(
	    "Second intermission:  now stopping control sequence ~w\n",
	    [Seq]),
    ?line gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    ?line true = process_flag(trap_exit, TrapExit),
    ?line ok.

gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts) ->
    ?line Tag = make_ref(),
    ?line Parent = self(),
    ?line Lookupers = 
	[spawn_link(
	   fun () -> 
		   random:seed(),
		   lookup_loop(Hosts, Delay, Tag, Parent, Cnt, Hosts) 
	   end)
	 || _ <- lists:seq(1, N)],
    control_loop(Seq, Interval, Tag, Lookupers, Seq),
    gethostnative_control_3(Tag, ok).

gethostnative_control_3(Tag, Reason) ->
    receive
	{Tag,Error} ->
	    ?line gethostnative_control_3(Tag, Error)
    after 0 ->
	    Reason
    end.

control_loop([], _Interval, _Tag, [], _Seq) ->
    ok;
control_loop([], Interval, Tag, Lookupers, Seq) ->
    control_loop(Seq, Interval, Tag, Lookupers, Seq);
control_loop([Op|Ops], Interval, Tag, Lookupers, Seq) ->
    control_loop(Ops, Interval, Tag, 
		 control_loop_1(Op, Interval, Tag, Lookupers),
		 Seq).

control_loop_1(Op, Interval, Tag, Lookupers) ->
    ?line
	receive
	    {'EXIT',Pid,Reason} ->
		?line case Reason of
			  Tag -> % Done
			      ?line control_loop_1
				      (Op, Interval, Tag,
				       lists:delete(Pid, Lookupers));
			  _ ->
			      ?line io:format("Lookuper ~p died: ~p",
					      [Pid,Reason]),
			      ?line test_server:fail("Lookuper died")
		      end
	after Interval ->
		?line if Op =/= undefined -> 
			      ?line ok = inet_gethost_native:control(Op);
			 true ->
			      ?line ok
		      end,
		?line Lookupers
	end.

lookup_loop(_, _Delay, Tag, _Parent,  0, _Hosts) ->
    exit(Tag);
lookup_loop([], Delay, Tag, Parent, Cnt, Hosts) ->
    lookup_loop(Hosts, Delay, Tag, Parent, Cnt, Hosts);
lookup_loop([H|Hs], Delay, Tag, Parent, Cnt, Hosts) ->
    case inet:gethostbyname(H) of
	{ok,_Hent} -> ok;
	{error,nxdomain} -> ok;
	Error ->
	    ?line io:format("Name lookup error for ~p for ~p: ~p",
			    [self(),H,Error]),
	    Parent ! {Tag,Error}
    end,
    receive 
    after random:uniform(Delay) -> 
	    lookup_loop(Hs, Delay, Tag, Parent, Cnt-1, Hosts) 
    end.



getif(suite) ->
    [];
getif(doc) ->
    ["Tests basic functionality of getiflist, getif, and ifget"];
getif(Config) when is_list(Config) ->
    ?line {ok,Hostname} = inet:gethostname(),
    ?line {ok,Address} = inet:getaddr(Hostname, inet),
    ?line {ok,Loopback} = inet:getaddr("localhost", inet),
    ?line {ok,Interfaces} = inet:getiflist(),
    ?line Addresses = 
	lists:sort(
	  lists:foldl(
	    fun (I, Acc) ->
		    case inet:ifget(I, [addr]) of
			{ok,[{addr,A}]} -> [A|Acc];
			{ok,[]} -> Acc
		    end
	    end, [], Interfaces)),
    ?line {ok,Getif} = inet:getif(),
    ?line Addresses = lists:sort([A || {A,_,_} <- Getif]),
    ?line true = ip_member(Address, Addresses),
    ?line true = ip_member(Loopback, Addresses),
    ?line ok.

%% Works just like lists:member/2, except that any {127,_,_,_} tuple
%% matches any other {127,_,_,_}. We do this to handle Linux systems
%% that use (for instance) 127.0.1.1 as the IP address for the hostname.

ip_member({127,_,_,_}, [{127,_,_,_}|_]) -> true;
ip_member(K, [K|_]) -> true;
ip_member(K, [_|T]) -> ip_member(K, T);
ip_member(_, []) -> false.
