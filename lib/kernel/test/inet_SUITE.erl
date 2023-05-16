%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
-module(inet_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_res.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include("kernel_test_lib.hrl").

-export([
         all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

	 t_gethostbyaddr/0, t_gethostbyaddr/1,
	 t_getaddr/0, t_getaddr/1,
	 t_gethostbyname/0, t_gethostbyname/1, t_gethostbyname_empty/1,
	 t_gethostbyaddr_v6/0, t_gethostbyaddr_v6/1,
	 t_getaddr_v6/0, t_getaddr_v6/1,
	 t_gethostbyname_v6/0, t_gethostbyname_v6/1,
	 ipv4_to_ipv6/0, ipv4_to_ipv6/1,
	 host_and_addr/0, host_and_addr/1,
	 t_gethostnative/1, 
	 gethostnative_parallell/1, cname_loop/1,
         missing_hosts_reload/1, hosts_file_quirks/1,
         gethostnative_soft_restart/0, gethostnative_soft_restart/1,
	 gethostnative_debug_level/0, gethostnative_debug_level/1,
	 lookup_bad_search_option/1,
	 getif/1,
	 getif_ifr_name_overflow/1,getservbyname_overflow/1, getifaddrs/1,
	 is_ip_address/1,
	 parse_strict_address/1, ipv4_mapped_ipv6_address/1, ntoa/1,
         simple_netns/1, simple_netns_open/1,
         add_del_host/1, add_del_host_v6/1,
         simple_bind_to_device/1, simple_bind_to_device_open/1,
	 socknames_sctp/1, socknames_tcp/1, socknames_udp/1
        ]).

-export([
         get_hosts/1, get_ipv6_hosts/1, parse_hosts/1, parse_address/1,
	 kill_gethost/0, parallell_gethost/0, test_netns/0
        ]).



suite() ->
    [
     {ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}
    ].

all() -> 
    [
     t_gethostbyaddr, t_gethostbyname, t_gethostbyname_empty, t_getaddr,
     t_gethostbyaddr_v6, t_gethostbyname_v6, t_getaddr_v6,
     ipv4_to_ipv6, host_and_addr, is_ip_address, {group, parse},
     t_gethostnative, gethostnative_parallell, cname_loop,
     missing_hosts_reload, hosts_file_quirks,
     gethostnative_debug_level, gethostnative_soft_restart,
     lookup_bad_search_option,
     getif, getif_ifr_name_overflow, getservbyname_overflow,
     getifaddrs, parse_strict_address, ipv4_mapped_ipv6_address, ntoa,
     simple_netns, simple_netns_open,
     add_del_host, add_del_host_v6,
     simple_bind_to_device, simple_bind_to_device_open,
     {group, socknames}
    ].

groups() -> 
    [
     {parse,     [], parse_cases()},
     {socknames, [], socknames_cases()}
    ].

parse_cases() ->
    [
     parse_hosts,
     parse_address
    ].

socknames_cases() ->
    [
     socknames_sctp,
     socknames_tcp,
     socknames_udp
    ].

%% Required configuration
required(v4) ->
    [{require, test_host_ipv4_only},
     {require, test_dummy_host}];
required(v6) ->
    [{require, test_host_ipv6_only},
     {require, test_dummy_ipv6_host}];
required(hosts) ->
    case os:type() of
	{OS, _} when OS =:= win32 ->
	    [{require, hardcoded_hosts},
	     {require, hardcoded_ipv6_hosts}];
	_Else ->
	    [{require, test_hosts}]
    end.     


init_per_suite(Config0) ->

    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite([{allow_skip, false} | Config0]) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            
            ?P("init_per_suite -> end when "
               "~n      Config: ~p", [Config1]),
            
            Config1
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
            "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config0) ->
    ?P("init_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config0, erlang:nodes(), pi(links), pi(monitors)]),

    kernel_test_global_sys_monitor:reset_events(),

    Config1 = init_per_testcase2(Case, Config0),

    ?P("init_per_testcase -> done when"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [Config1, erlang:nodes(), pi(links), pi(monitors)]),
    Config1.

init_per_testcase2(gethostnative_debug_level, Config) ->
    ?TT(?MINS(2)),
    Config;
init_per_testcase2(gethostnative_soft_restart, Config) ->
    ?TT(?MINS(2)),
    Config;
init_per_testcase2(lookup_bad_search_option, Config) ->
    Db = inet_db,
    Key = res_lookup,
    %% The bad option cannot enter through inet_db:set_lookup/1,
    %% but through e.g .inetrc.
    Prev = ets:lookup(Db, Key),
    ets:delete(Db, Key),
    ets:insert(Db, {Key,[lookup_bad_search_option]}),
    ?P("init_per_testcase -> Misconfigured resolver lookup order"),
    [{Key,Prev}|Config];
init_per_testcase2(_Func, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    ?P("end_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), pi(links), pi(monitors)]),

    ?P("system events during test: "
       "~n   ~p", [kernel_test_global_sys_monitor:events()]),

    end_per_testcase2(Case, Config),

    ?P("end_per_testcase -> done with"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), pi(links), pi(monitors)]),
    ok.

end_per_testcase2(lookup_bad_search_option, Config) ->
    ?P("end_per_testcase2 -> restore resolver lookup order"),
    Db   = inet_db,
    Key  = res_lookup,
    Prev = proplists:get_value(Key, Config),
    ets:delete(Db, Key),
    ets:insert(Db, Prev),
    ?P("end_per_testcase2 -> resolver lookup order restored");
end_per_testcase2(_Func, _Config) ->
    ok.

t_gethostbyaddr() -> required(v4).
%% Test the inet:gethostbyaddr/1 function.
t_gethostbyaddr(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_gethostbyaddr(Config) end).

do_gethostbyaddr(Config) when is_list(Config) ->
    ?P("begin - try get config 'test_host_ipv4_only'"),
    {Name,FullName,IPStr,{A,B,C,D}=IP,Aliases,_,_} =
        ct:get_config(test_host_ipv4_only),
    ?P("config 'test_host_ipv4_only': "
       "~n   Name:      ~p"
       "~n   Full Name: ~p"
       "~n   IPStr:     ~p"
       "~n   (IP) A:    ~p"
       "~n   (IP) B:    ~p"
       "~n   (IP) C:    ~p"
       "~n   (IP) D:    ~p"
       "~n   Aliases:   ~p",
       [Name, FullName, IPStr, A, B, C, D,Aliases]),
    Rname = integer_to_list(D) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(A) ++ ".in-addr.arpa",
    {ok, HEnt} = inet:gethostbyaddr(IPStr),
    {ok, HEnt} = inet:gethostbyaddr(IP),
    ?P("gethostbyaddr for (both):"
       "~n   IPStr: ~p"
       "~n   IP:    ~p"
       "~n   => ~p", [IPStr, IP, HEnt]),
    {error, Reason} = inet:gethostbyaddr(Name),
    ok = ?P("Expected error with failure reason: "
            "~n   (~w) ~s", [Reason, inet:format_error(Reason)]),
    HEnt_ = HEnt#hostent{h_addrtype = inet,
                         h_length = 4,
                         h_addr_list = [IP]},
    HEnt_ = HEnt,
    case {os:type(),os:version()} of
        {{unix,freebsd},{5,0,0}} ->
            %% The alias list seems to be buggy in FreeBSD 5.0.0.
            check_elems([{HEnt#hostent.h_name,[Name,FullName]}]),
            ?P("Buggy alias list: ~p", [HEnt#hostent.h_aliases]),
            ok;
        _ ->
            ?P("alias list: "
               "~n   ~p", [HEnt#hostent.h_aliases]),
            ?P("check alias list: "
               "~n   ~p", [[Aliases,tl(Aliases),[Rname]]]),
            ?P("name:       ~p", [HEnt#hostent.h_name]),
            ?P("check name: ~p", [[Name,FullName]]),
            check_elems(
	      [{HEnt#hostent.h_name,[Name,FullName]},
	       {HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases),[Rname]]}])
    end,

    ?P("try get config 'test_dummy_host'"),
    {_DName, _DFullName, DIPStr, DIP, _, _, _} = ct:get_config(test_dummy_host),
    {error,nxdomain} = inet:gethostbyaddr(DIPStr),
    {error,nxdomain} = inet:gethostbyaddr(DIP),

    ?P("done"),
    ok.

t_gethostbyaddr_v6() -> required(v6).
%% Test the inet:gethostbyaddr/1 inet6 function.
t_gethostbyaddr_v6(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_gethostbyaddr_v6(Config) end).

do_gethostbyaddr_v6(Config) when is_list(Config) ->
    {Name6, FullName6, IPStr6, IP6, Aliases6} =
	ct:get_config(test_host_ipv6_only),

    case inet:gethostbyaddr(IPStr6) of
        %% Even if IPv6 is not supported, the native resolver may succeed
	%% looking up the host. DNS lookup will probably fail.
	{error,nxdomain} ->
	    {skip, "IPv6 test fails! IPv6 not supported on this host!?"};
	{ok, HEnt6} ->
	    {ok, HEnt6} = inet:gethostbyaddr(IP6),
	    {error, Reason6} = inet:gethostbyaddr(Name6),
	    ok = ?P("Expected error with failure reason: "
                    "~n   (~w) ~s", [Reason6, inet:format_error(Reason6)]),
	    HEnt6_ = HEnt6#hostent{h_addrtype = inet6,
				   h_length = 16,
				   h_addr_list = [IP6]},
	    HEnt6_ = HEnt6,
	    check_elems(
	      [{HEnt6#hostent.h_name,[Name6,FullName6]},
	       {HEnt6#hostent.h_aliases,[[],Aliases6,tl(Aliases6)]}]),

	    {_DName6, _DFullName6, DIPStr6, DIP6, _} =
		ct:get_config(test_dummy_ipv6_host),
	    {error,nxdomain} = inet:gethostbyaddr(DIPStr6),
	    {error,nxdomain} = inet:gethostbyaddr(DIP6),
	    ok
    end.

t_gethostbyname() -> required(v4).
%% Test the inet:gethostbyname/1 function.
t_gethostbyname(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_gethostbyname(Config) end).

do_gethostbyname(Config) when is_list(Config) ->
    {Name,FullName,IPStr,IP,Aliases,IP_46_Str,_} =
	ct:get_config(test_host_ipv4_only),
    {ok,_} = inet:gethostbyname(IPStr),
    {ok,HEnt} = inet:gethostbyname(Name),
    {ok,HEnt} = inet:gethostbyname(list_to_atom(Name)),
    HEnt_ = HEnt#hostent{h_addrtype = inet,
			 h_length = 4,
			 h_addr_list = [IP]},

    HEnt_ = HEnt,
    check_elems([{HEnt#hostent.h_name,[Name,FullName]},
		 {HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases)]}]),
    {ok,HEntF} = inet:gethostbyname(FullName),
    HEntF_ = HEntF#hostent{h_name = FullName,
			   h_addrtype = inet,
			   h_length = 4,
			   h_addr_list = [IP]},
    HEntF_ = HEntF,
    check_elems([{HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases)]}]),
    %%
    FullNameU = toupper(FullName),
    {ok,HEntU} = inet:gethostbyname(FullNameU),
    FullNameU = toupper(HEntU#hostent.h_name),
    #hostent{
       h_addrtype = inet,
       h_length = 4,
       h_addr_list = [IP]} = HEntU,
    check_elems(
      [{[toupper(H) || H <- HEntU#hostent.h_aliases],
	[[],[toupper(A) || A <- Aliases]]}]),

    {DName, _DFullName, _DIPStr, _DIP, _, _, _} =
	ct:get_config(test_dummy_host),
    {error,nxdomain} = inet:gethostbyname(DName),
    {error,nxdomain} = inet:gethostbyname(IP_46_Str),
    ok.


t_gethostbyname_empty(Config) when is_list(Config) ->
    element(1, os:type()) =:= unix andalso
        begin
            {error,nxdomain} = inet:gethostbyname(""),
            {error,nxdomain} = inet:gethostbyname('')
        end,
    {error,nxdomain} = inet:gethostbyname("."),
    {error,nxdomain} = inet:gethostbyname('.'),
    ok.


t_gethostbyname_v6() -> required(v6).
%% Test the inet:gethostbyname/1 inet6 function.
t_gethostbyname_v6(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_gethostbyname_v6(Config) end).

do_gethostbyname_v6(Config) when is_list(Config) ->
    {Name, FullName, IPStr, IP, Aliases} =
	ct:get_config(test_host_ipv6_only),

    case inet:gethostbyname(Name, inet6) of
	{ok,HEnt} ->
	    {ok,_} = inet:gethostbyname(IPStr, inet6),
	    {ok,HEnt} = inet:gethostbyname(list_to_atom(Name), inet6),
	    case HEnt#hostent.h_addr_list of
		[IP] -> % IPv6 address
		    #hostent{h_addrtype = inet6,
			     h_length = 16} = HEnt,
		    check_elems(
		      [{HEnt#hostent.h_name,[Name,FullName]},
		       {HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases)]}]);
		[IP46] -> % IPv4 compatible address
		    {ok,HEnt4} = inet:gethostbyname(Name, inet),
		    #hostent{h_addrtype = inet,
			     h_length = 4,
			     h_addr_list = [IP4]} = HEnt4,
		    {ok,IP46} =
			inet_parse:ipv6_address(
			  "::ffff:" ++ inet:ntoa(IP4)),
		    check_elems(
		      [{HEnt#hostent.h_name,[Name,FullName]}])
	    end,

	    {ok,HEntF} = inet:gethostbyname(FullName, inet6),
	    case HEntF#hostent.h_addr_list of
		[IP] -> % IPv6 address
		    #hostent{h_name = FullName,
			     h_addrtype = inet6,
			     h_length = 16} = HEntF,
		    check_elems(
		      [{HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases)]}]);
		[IP46F] -> % IPv4 compatible address
		    {ok,HEnt4F} = inet:gethostbyname(FullName, inet),
		    #hostent{h_addrtype = inet,
			     h_length = 4,
			     h_addr_list = [IP4F]} = HEnt4F,
		    {ok,IP46F} =
			inet_parse:ipv6_address(
			  "::ffff:" ++ inet:ntoa(IP4F)),
		    check_elems(
		      [{HEntF#hostent.h_name,[Name,FullName]}])
	    end;
	_ ->
	    {skip, "IPv6 is not supported on this host"}
    end.

check_elems([{Val,Tests} | Elems]) ->
    ?P("check_elems -> entry with"
       "~n   Val:   ~p"
       "~n   Tests: ~p", [Val, Tests]),
    check_elem(Val, Tests, Tests),
    check_elems(Elems);
check_elems([]) -> ok.

check_elem(Val, [Val|_], _) -> ok;
check_elem(Val, [_|Tests], Tests0) ->
    check_elem(Val, Tests, Tests0);
check_elem(Val, [], Tests0) ->
    ct:fail({no_match,Val,Tests0}).


t_getaddr() -> required(v4).
%% Test the inet:getaddr/2 function.
t_getaddr(Config) when is_list(Config) ->
    {Name,FullName,IPStr,IP,_,IP_46_Str,IP46} =
	ct:get_config(test_host_ipv4_only),
    {ok,IP} = inet:getaddr(list_to_atom(Name), inet),
    {ok,IP} = inet:getaddr(Name, inet),
    {ok,IP} = inet:getaddr(FullName, inet),
    {ok,IP} = inet:getaddr(IP, inet),
    {ok,IP} = inet:getaddr(IPStr, inet),
    {error,nxdomain} = inet:getaddr(IP_46_Str, inet),
    {error,eafnosupport} = inet:getaddr(IP46, inet),

    {DName, DFullName, DIPStr, DIP, _, _, _} = ct:get_config(test_dummy_host),
    {error,nxdomain} = inet:getaddr(DName, inet),
    {error,nxdomain} = inet:getaddr(DFullName, inet),
    {ok,DIP} = inet:getaddr(DIPStr, inet),
    {ok,DIP} = inet:getaddr(DIP, inet),
    ok.

t_getaddr_v6() -> required(v4) ++ required(v6).
%% Test the inet:getaddr/2 function.
t_getaddr_v6(Config) when is_list(Config) ->
    {Name,FullName,IPStr,IP,_} =
	ct:get_config(test_host_ipv6_only),

    case inet:getaddr(Name, inet6) of
	{ok,Addr} ->
	    IP = Addr,
	    {ok,IP} = inet:getaddr(toupper(Name), inet6),
	    {ok,IP} = inet:getaddr(list_to_atom(Name), inet6),
	    {ok,IP} = inet:getaddr(list_to_atom(toupper(Name)), inet6),
	    {ok,IP} = inet:getaddr(FullName, inet6),
	    {ok,IP} = inet:getaddr(toupper(FullName), inet6),
	    {ok,IP} = inet:getaddr(IP, inet6),
	    {ok,IP} = inet:getaddr(IPStr, inet6),
	    %%
	    {DName,DFullName,DIPStr,DIP,_} =
		ct:get_config(test_dummy_ipv6_host),
	    {error,nxdomain} = inet:getaddr(DName, inet6),
	    {error,nxdomain} = inet:getaddr(DFullName, inet6),
	    {ok,DIP} = inet:getaddr(DIPStr, inet6),
	    {ok,DIP} = inet:getaddr(DIP, inet6),
	    ok;
	_ ->
 	    {skip, "IPv6 is not supported on this host"}
    end.

ipv4_to_ipv6() -> required(v4).
%% Test if IPv4 address is converted to IPv6 address.
ipv4_to_ipv6(Config) when is_list(Config) ->
    %% Test what happens if an IPv4 address is looked up in an IPv6 context.
    %% If the native resolver succeeds to look it up, an IPv4 compatible
    %% address should be returned. If no IPv6 support on this host, an
    %% error should beturned.
    {_Name,_FullName,IPStr,_IP,Aliases,IP_46_Str,IP_46} =
	ct:get_config(test_host_ipv4_only),
    IP4to6Res =
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
		ct:fail({ipv4_to_ipv6_lookup_failed,Other})
	end,
    case {IP4to6Res,inet:gethostbyname(IPStr, inet6)} of
	{true,{ok,HEnt}} ->
	    HEnt_ = HEnt#hostent{h_addrtype = inet6,
				 h_length = 16,
				 h_addr_list = [IP_46]},
	    HEnt_ = HEnt,
	    check_elems([{HEnt#hostent.h_name,[IP_46_Str,IPStr]},
			 {HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases)]}]);
	{_,IP4to6Res} -> ok
    end,
    ok.


%% Test looking up hosts and addresses. Use 'ypcat hosts'
%% or the local equivalent to find all hosts.

host_and_addr() ->
    ?P("host_and_addr -> entry"),
    [{timetrap,{minutes,5}}|required(hosts)].

host_and_addr(Config) when is_list(Config) ->
    lists:foreach(fun try_host/1, get_hosts(Config)),
    ok.

try_host({Ip0, Host}) ->
    {ok, Ip}                             = inet:getaddr(Ip0, inet),
    {ok,{hostent, _, _, inet, _, Ips1}}  = inet:gethostbyaddr(Ip),
    {ok,{hostent, _, _, inet, _, _Ips2}} = inet:gethostbyname(Host),
    true = lists:member(Ip, Ips1),
    ok.

%% Get all hosts from the system using 'ypcat hosts' or the local
%% equvivalent.

get_hosts(_Config) -> 
    case os:type() of
	{unix, _} ->
	    List = lists:map(fun(X) ->
				     atom_to_list(X)++" "
			     end, ct:get_config(test_hosts)),
	    Cmd = "ypmatch "++List++" hosts.byname", 
	    HostFile = os:cmd(Cmd),
	    get_hosts(HostFile, [], [], []);
	_ -> 
	    ct:get_config(hardcoded_hosts)
    end.

get_ipv6_hosts(_Config) -> 
    case os:type() of
	{unix, _} ->
	    List = lists:map(fun(X) ->
				     atom_to_list(X)++" "
			     end, ct:get_config(ipv6_hosts)),
	    Cmd = "ypmatch "++List++" ipnodes.byname", 
	    HostFile = os:cmd(Cmd),
	    get_hosts(HostFile, [], [], []);
	_ -> 
	    ct:get_config(hardcoded_ipv6_hosts)
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


parse_hosts(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    HostFile = filename:join(DataDir, "hosts"),
    inet_parse:hosts(HostFile),
    HostFileErr1 = filename:join(DataDir, "hosts_err1"),
    inet_parse:hosts(HostFileErr1),
    Resolv = filename:join(DataDir,"resolv.conf"),
    inet_parse:resolv(Resolv),
    ResolvErr1 = filename:join(DataDir,"resolv.conf.err1"),
    inet_parse:resolv(ResolvErr1).

parse_address(Config) when is_list(Config) ->
    V4Reversable =
	[{{0,0,0,0},"0.0.0.0"},
         {{1,2,3,4},"1.2.3.4"},
	 {{253,252,251,250},"253.252.251.250"},
	 {{1,2,255,254},"1.2.255.254"}],
    V6Reversable =
	[{{0,0,0,0,0,0,0,0},"::"},
	 {{0,0,0,0,0,0,0,1},"::1"},
	 {{0,0,0,0,0,0,0,2},"::0.0.0.2"},
	 {{15,0,0,0,0,0,0,2},"f::2"},
	 {{15,16#f11,0,0,0,0,256,2},"f:f11::100:2"},
	 {{16#700,0,0,0,0,0,0,0},"700::"},
	 {{0,0,0,0,0,0,2,1},"::0.2.0.1"},
	 {{0,0,0,0,0,3,2,1},"::3:2:1"},
	 {{0,0,0,0,4,3,2,1},"::4:3:2:1"},
	 {{0,0,0,5,4,3,2,1},"::5:4:3:2:1"},
	 {{0,0,6,5,4,3,2,1},"::6:5:4:3:2:1"},
	 {{0,7,6,5,4,3,2,1},"0:7:6:5:4:3:2:1"},
	 {{7,0,0,0,0,0,0,0},"7::"},
	 {{7,6,0,0,0,0,0,0},"7:6::"},
	 {{7,6,5,0,0,0,0,0},"7:6:5::"},
	 {{7,6,5,4,0,0,0,0},"7:6:5:4::"},
	 {{7,6,5,4,3,0,0,0},"7:6:5:4:3::"},
	 {{7,6,5,4,3,2,0,0},"7:6:5:4:3:2::"},
	 {{7,6,5,4,3,2,1,0},"7:6:5:4:3:2:1:0"},
	 {{0,0,6,5,4,3,0,0},"::6:5:4:3:0:0"},
	 {{0,0,6,5,4,0,0,0},"0:0:6:5:4::"},
	 {{8,0,0,5,4,0,0,1},"8::5:4:0:0:1"},
	 {{8,0,0,5,0,0,0,1},"8:0:0:5::1"},
	 {{0,7,6,5,4,3,2,0},"0:7:6:5:4:3:2:0"},
	 {{0,0,6,5,4,3,0,0},"::6:5:4:3:0:0"},
	 {{0,0,0,5,4,0,0,0},"::5:4:0:0:0"},
	 {{0,0,0,0,4,0,0,0},"::4:0:0:0"},
	 {{0,0,0,5,0,0,0,0},"0:0:0:5::"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "c11:c22:5c33:c440:55c0:c66c:77:88"},
	 {{0,16#c22,16#5c33,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "0:c22:5c33:c440:55c0:c66c:77:88"},
	 {{16#c11,0,16#5c33,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "c11:0:5c33:c440:55c0:c66c:77:88"},
	 {{16#c11,16#c22,0,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "c11:c22:0:c440:55c0:c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,0,16#55c0,16#c66c,16#77,16#88},
	  "c11:c22:5c33:0:55c0:c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,0,16#c66c,16#77,16#88},
	  "c11:c22:5c33:c440:0:c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,0,16#77,16#88},
	  "c11:c22:5c33:c440:55c0:0:77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,16#c66c,0,16#88},
	  "c11:c22:5c33:c440:55c0:c66c:0:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,16#c66c,16#77,0},
	  "c11:c22:5c33:c440:55c0:c66c:77:0"},
	 {{0,0,16#5c33,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "::5c33:c440:55c0:c66c:77:88"},
	 {{16#c11,0,0,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "c11::c440:55c0:c66c:77:88"},
	 {{16#c11,16#c22,0,0,16#55c0,16#c66c,16#77,16#88},
	  "c11:c22::55c0:c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,0,0,16#c66c,16#77,16#88},
	  "c11:c22:5c33::c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,0,0,16#77,16#88},
	  "c11:c22:5c33:c440::77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,0,0,16#88},
	  "c11:c22:5c33:c440:55c0::88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,16#c66c,0,0},
	  "c11:c22:5c33:c440:55c0:c66c::"},
	 {{0,0,0,16#c440,16#55c0,16#c66c,16#77,16#88},
	  "::c440:55c0:c66c:77:88"},
	 {{16#c11,0,0,0,16#55c0,16#c66c,16#77,16#88},
	  "c11::55c0:c66c:77:88"},
	 {{16#c11,16#c22,0,0,0,16#c66c,16#77,16#88},
	  "c11:c22::c66c:77:88"},
	 {{16#c11,16#c22,16#5c33,0,0,0,16#77,16#88},
	  "c11:c22:5c33::77:88"},
	 {{16#c11,16#c22,16#5c33,16#c440,0,0,0,16#88},
	  "c11:c22:5c33:c440::88"},
	 {{16#c11,16#c22,16#5c33,16#c440,16#55c0,0,0,0},
	  "c11:c22:5c33:c440:55c0::"},
	 {{0,0,0,0,16#55c0,16#c66c,16#77,16#88},
	  "::55c0:c66c:77:88"},
	 {{16#c11,0,0,0,0,16#c66c,16#77,16#88},
	  "c11::c66c:77:88"},
	 {{16#c11,16#c22,0,0,0,0,16#77,16#88},
	  "c11:c22::77:88"},
	 {{16#c11,16#c22,16#5c33,0,0,0,0,16#88},
	  "c11:c22:5c33::88"},
	 {{16#c11,16#c22,16#5c33,16#c440,0,0,0,0},
	  "c11:c22:5c33:c440::"},
	 {{0,0,0,0,0,16#c66c,16#77,16#88},
	  "::c66c:77:88"},
	 {{16#c11,0,0,0,0,0,16#77,16#88},
	  "c11::77:88"},
	 {{16#c11,16#c22,0,0,0,0,0,16#88},
	  "c11:c22::88"},
	 {{16#c11,16#c22,16#5c33,0,0,0,0,0},
	  "c11:c22:5c33::"},
	 {{0,0,0,0,0,65535,258,65534},"::ffff:1.2.255.254"},
         {{16#fe80,12345,0,0,0,0,0,16#12},"fe80::12%012345"},
	 {{16#ffff,16#ffff,16#ffff,16#ffff,16#ffff,16#ffff,16#ffff,16#ffff},
	  "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"}
         |[{list_to_tuple(P++[(D1 bsl 8) bor D2,(D3 bsl 8) bor D4]),
            Q++S}
           || {{D1,D2,D3,D4},S} <-
                  tl(V4Reversable),
              {P,Q} <-
                  [{[0,0,0,0,0,16#ffff],"::ffff:"},
                   {[0,0,0,0,0,0],"::"}]]],
    V4Sloppy =
	[{{10,1,16#98,16#76},"10.0x019876"},
	 {{8#12,1,8#130,8#321},"012.01.054321"},
	 {{252,253,254,255},"252.253.254.0377"},
	 {{252,253,254,255},"0Xfc.000000000375.0x0000fe.255"},
	 {{252,253,254,255},"252.253.65279"},
	 {{252,253,254,255},"252.0xFD.0177377"},
	 {{252,253,254,255},"252.16645887"},
	 {{252,253,254,255},"00374.0XFDFEFF"},
	 {{252,253,254,255},"4244504319"},
	 {{252,253,254,255},"0xfcfdfeff"},
	 {{252,253,254,255},"00000000000037477377377"},
	 {{16#12,16#34,16#56,16#78},"0x12345678"},
	 {{16#12,16#34,16#56,16#78},"0x12.0x345678"},
	 {{16#12,16#34,16#56,16#78},"0x12.0X34.0x5678"},
	 {{16#12,16#34,16#56,16#78},"0x12.0X34.0x56.0X78"},
	 {{0,0,0,0},"0"},
	 {{0,0,0,0},"00"},
	 {{0,0,0,0},"0.0"},
	 {{0,0,0,0},"00.00.00"},
	 {{0,0,0,0},"0.00.0.0"},
	 {{0,0,0,0},"0.0.000000000000.0"}],
    V6Sloppy =
        [{{16#a,16#b,16#c,16#0,16#0,16#d,16#e,16#f},"A:B:C::d:e:f"},
         {{16#fe80,0,0,0,0,0,0,16#12},"fe80::12%XXXXXXX"}]
        ++
        [{{P,0,0,0,0,D2,(D1 bsl 8) bor D2,(D3 bsl 8) bor D4},
          Q++erlang:integer_to_list(D2, 16)++":"++S}
         || {{D1,D2,D3,D4},S} <- V4Reversable,
            {P,Q} <-
                [{16#2001,"2001::"},{16#177,"177::"},{16#ff0,"Ff0::"}]],
    V4Err =
	["0.256.0.1",
	 "1.2.3.4.5",
	 "256.255.65535",
	 "4294967296",
	 "0x100000000",
	 "040000000000",
	 "1.2.3.-4",
	 "1.2.-3.4",
	 "1.-2.3.4",
	 "-1.2.3.4",
	 "10.",
	 "172.16.",
	 "198.168.0.",
	 "127.0.0.1."],
    V6Err =
	[":::",
	 "f:::2",
	 "::-1",
	 "::g",
	 "f:f11::10100:2",
	 "f:f11::01100:2",
	 "::17000",
	 "::01700",
	 "10000::",
	 "01000::",
	 "::8:7:6:5:4:3:2:1",
	 "8:7:6:5:4:3:2:1::",
	 "8:7:6:5:4::3:2:1",
	 "::1.2.3.4.5",
	 "::1.2.3.04",
	 "::1.256.3.4",
	 "::-5.4.3.2",
	 "::5.-4.3.2",
	 "::5.4.-3.2",
	 "::5.4.3.-2",
	 "::FFFF:1.2.3.4.5",
	 "::10.",
	 "::FFFF:172.16.",
	 "fe80::198.168.0.",
	 "fec0::fFfF:127.0.0.1."],
    t_parse_address
      (parse_ipv6_address,
       false,
       V6Reversable++V6Sloppy++V6Err++V4Err),
    t_parse_address
      (parse_ipv6strict_address,
       true,
       V6Reversable++V6Err++V4Err),
    t_parse_address
      (parse_ipv4_address,
       false,
       V4Reversable++V4Sloppy++V4Err++V6Err++[S || {_,S} <- V6Reversable]),
    t_parse_address
      (parse_ipv4strict_address,
       true,
       V4Reversable++V4Err++V6Err++[S || {_,S} <- V4Sloppy++V6Reversable]).

t_parse_address(Func, _Reversable, []) ->
    io:format("~p done.~n", [Func]),
    ok;
t_parse_address(Func, Reversible, [{Addr,String}|L]) ->
    io:format("~p = ~p.~n", [Addr,String]),
    {ok,Addr} = inet:Func(String),
    case Reversible of
        true ->String = inet:ntoa(Addr);
        false -> ok
    end,
    t_parse_address(Func, Reversible, L);
t_parse_address(Func, Reversible, [String|L]) ->
    io:format("~p.~n", [String]),
    {error,einval} = inet:Func(String),
    t_parse_address(Func, Reversible, L).

parse_strict_address(Config) when is_list(Config) ->
    {ok, {127,0,0,1}} =
	inet:parse_strict_address("127.0.0.1"),
    {ok, {3089,3106,23603,50240,21952,50796,119,136}} =
	inet:parse_strict_address("c11:0c22:5c33:c440:55c0:c66c:77:0088"),
    {ok, {3089,3106,23603,50240,0,0,119,136}} =
	inet:parse_strict_address("c11:0c22:5c33:c440::077:0088").

is_ip_address(Config) when is_list(Config) ->
    IPv4Addresses = [
        {0, 0, 0, 0},
        {255, 255, 255, 255}
    ],
    IPv6Addresses = [
        {0, 0, 0, 0, 0, 0, 0, 0},
        {16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff, 16#ffff}
    ],
    NonIPAddresses = [
        foo,
        "0.0.0.0",
        {},
        {0},
        {0, 0},
        {0, 0, 0},
        {0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, foo},
        {0, 0, 0, 0, 0, 0, 0, foo},
        {0, 0, 0, 256},
        {0, 0, 256, 0},
        {0, 256, 0, 0},
        {256, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 16#10000},
        {0, 0, 0, 0, 0, 0, 16#10000, 0},
        {0, 0, 0, 0, 0, 16#10000, 0, 0},
        {0, 0, 0, 0, 16#10000, 0, 0, 0},
        {0, 0, 0, 16#10000, 0, 0, 0, 0},
        {0, 0, 16#10000, 0, 0, 0, 0, 0},
        {0, 16#10000, 0, 0, 0, 0, 0, 0},
        {16#10000, 0, 0, 0, 0, 0, 0, 0},
        {0, 0, 0, -1},
        {0, 0, -1, 0},
        {0, -1, 0, 0},
        {-1, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, -1},
        {0, 0, 0, 0, 0, 0, -1, 0},
        {0, 0, 0, 0, 0, -1, 0, 0},
        {0, 0, 0, 0, -1, 0, 0, 0},
        {0, 0, 0, -1, 0, 0, 0, 0},
        {0, 0, -1, 0, 0, 0, 0, 0},
        {0, -1, 0, 0, 0, 0, 0, 0},
        {-1, 0, 0, 0, 0, 0, 0, 0}
    ],

    true = lists:all(fun inet:is_ipv4_address/1, IPv4Addresses),
    false = lists:any(fun inet:is_ipv4_address/1, IPv6Addresses ++ NonIPAddresses),

    true = lists:all(fun inet:is_ipv6_address/1, IPv6Addresses),
    false = lists:any(fun inet:is_ipv6_address/1, IPv4Addresses ++ NonIPAddresses),

    true = lists:all(fun inet:is_ip_address/1, IPv6Addresses ++ IPv4Addresses),
    false = lists:any(fun inet:is_ip_address/1, NonIPAddresses).

ipv4_mapped_ipv6_address(Config) when is_list(Config) ->
    {D1,D2,D3,D4} = IPv4Address =
        {rand:uniform(256) - 1,
         rand:uniform(256) - 1,
         rand:uniform(256) - 1,
         rand:uniform(256) - 1},
    E7 = (D1 bsl 8) bor D2,
    E8 = (D3 bsl 8) bor D4,
    io:format("IPv4Address: ~p.~n", [IPv4Address]),
    {0,0,0,0,0,65535,E7,E8} = inet:ipv4_mapped_ipv6_address(IPv4Address),
    IPv6Address =
        {rand:uniform(65536) - 1,
         rand:uniform(65536) - 1,
         rand:uniform(65536) - 1,
         rand:uniform(65536) - 1,
         rand:uniform(65536) - 1,
         rand:uniform(65536) - 1, E7, E8},
    IPv4Address = inet:ipv4_mapped_ipv6_address(IPv6Address),
    ok.


ntoa(Config) when is_list(Config) ->
    M8 = 1 bsl 8,
    M16 = 1 bsl 16,
    V4Xs = rand_tuple(4, M8),
    V6Xs = rand_tuple(4, M16),
    ntoa(
      [{A, B, C, D} ||
          A <- [0, element(1, V4Xs), M8-1, -1, 256],
          B <- [0, element(2, V4Xs), M8-1, -1, 256],
          C <- [0, element(3, V4Xs), M8-1, -1, 256],
          D <- [0, element(4, V4Xs), M8-1, -1, 256]], M8-1),
    ntoa(
      [{E, F, G, H, G, G, E, F} ||
          E <- [0, element(1, V6Xs), M16-1, -1, M16],
          F <- [0, element(2, V6Xs), M16-1, -1, M16],
          G <- [0, element(3, V6Xs), M16-1, -1, M16],
          H <- [0, element(4, V6Xs), M16-1, -1, M16]], M16-1).

ntoa([A | As], Max) ->
    case
        lists:all(
          fun (X) when 0 =< X, X =< Max -> true;
              (_) -> false
          end, tuple_to_list(A))
    of
        true ->
            S = inet:ntoa(A),
            {ok, A} = inet:parse_address(S);
        false ->
            {error, einval} = inet:ntoa(A)
    end,
    ntoa(As, Max);
ntoa([], _Max) ->
    ok.

rand_tuple(N, M) ->
    rand_tuple(N, M, []).
%%
rand_tuple(0, _M, Acc) ->
    list_to_tuple(Acc);
rand_tuple(N, M, Acc) ->
    rand_tuple(N - 1, M, [rand:uniform(M) - 1 | Acc]).


t_gethostnative(Config) when is_list(Config) ->
    %% this will result in 26 bytes sent which causes problem in Windows
    %% if the port-program has not assured stdin to be read in BINARY mode
    %% OTP-2555
    case inet_gethost_native:gethostbyname(
	   "a23456789012345678901234") of
	{error,notfound} ->
	    ok;
	{error,no_data} ->
	    ok;
	{error,try_again} ->
	    ok
    end.

%% Check that the emulator survives crashes in gethost_native.
gethostnative_parallell(Config) when is_list(Config) ->
    {ok,Hostname} = inet:gethostname(),
    {ok,_} = inet:gethostbyname(Hostname),
    case whereis(inet_gethost_native) of
	Pid when is_pid(Pid) ->
	    do_gethostnative_parallell();
	_ ->
	    {skipped, "Not running native gethostbyname"}
    end.

do_gethostnative_parallell() ->
    {ok,Peer,Node} = ?CT_PEER(),
    ok = rpc:call(Node, ?MODULE, parallell_gethost, []),
    receive after 10000 -> ok end,
    pong = net_adm:ping(Node),
    peer:stop(Peer),
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

%% Check that the resolver handles a CNAME loop.
cname_loop(Config) when is_list(Config) ->
    %% getbyname (hostent_by_domain)
    ok = inet_db:add_rr("mydomain.com", in, ?S_CNAME, ttl, "mydomain.com"),
    {error,nxdomain} = inet_db:getbyname("mydomain.com", ?S_A),
    ok = inet_db:del_rr("mydomain.com", in, ?S_CNAME, "mydomain.com"),
    %% res_hostent_by_domain
    RR = #dns_rr{domain = "mydomain.com",
		 class = in,
		 type = ?S_CNAME,
		 data = "mydomain.com"},
    Rec = #dns_rec{anlist = [RR]},
    {error,nxdomain} = inet_db:res_hostent_by_domain("mydomain.com", ?S_A, Rec),
    ok.


%% Test that hosts file gets reloaded correctly in case when it
% was missing during initial startup
missing_hosts_reload(Config) when is_list(Config) ->
    RootDir = proplists:get_value(priv_dir,Config),
    HostsFile = filename:join(RootDir, atom_to_list(?MODULE) ++ ".hosts"),
    InetRc = filename:join(RootDir, "inetrc"),
    ok = file:write_file(InetRc, "{hosts_file, \"" ++ HostsFile ++ "\"}.\n"),
    {error, enoent} = file:read_file_info(HostsFile),
    % start a node
    {ok, Peer, TestNode} = ?CT_PEER(["-kernel", "inetrc", "\"" ++ InetRc ++ "\""]),
    % ensure it has our RC
    Rc = rpc:call(TestNode, inet_db, get_rc, []),
    {hosts_file, HostsFile} = lists:keyfind(hosts_file, 1, Rc),
    % ensure it does not resolve
    {error, nxdomain} = rpc:call(TestNode, inet_hosts, gethostbyname, ["somehost"]),
    % write hosts file
    ok = file:write_file(HostsFile, "1.2.3.4 somehost"),
    % wait for cached timestamp to expire
    timer:sleep(?RES_FILE_UPDATE_TM * 1000 + 100),
    % ensure it DOES resolve
    {ok,{hostent,"somehost",[],inet,4,[{1,2,3,4}]}} =
        rpc:call(TestNode, inet_hosts, gethostbyname, ["somehost"]),
    % cleanup
    peer:stop(Peer).


%% The /etc/hosts file format and limitations is quite undocumented.
%%
%% Our implementation of the hosts file resolver tries to
%% do the right thing.  Here is an attempt to define "the right thing",
%% and this test case tries to check most of these rules:
%%
%% * A hosts file consists of entries with one IP address,
%%   and a list of host names.  The IP address is IPv4 or IPv6.
%%   The first host name is the primary host name
%%   and the others are aliases.
%%
%% * A lookup for an IP address should return one #hostent{} record
%%   with the one IP address from the query, and the host names
%%   from all entries with the same IP address concatenated.
%%   The first host name from the first hosts file entry
%%   with the requested IP address will be the primary host name
%%   and all others are aliases.  All host names are returned
%%   as in the hosts file entries i.e character case is preserved.
%%
%% * A lookup for a host name is character case insensitive.
%%
%% * A lookup for a host name should return one #hostent{} record
%%   with the host name list from the first hosts file entry
%%   with an IP address of the requested address family
%%   that has a matching host name, as it is in the hosts file
%%   i.e character case is preserved.  The IP addresses in the
%%   returned #hostent{} record should be the first from the
%%   same matching hosts file entry, followed by all others
%%   for which there is a matching host name and address family.
%%   There should be no duplicates among the returned IP addresses.
%%
%% * These rules are of the opinion that if duplicate host names
%%   with the same character casing occurs for the same IP
%%   address, it is a configuration error, so it is not tested for
%%   and there is no preferred behaviour.
hosts_file_quirks(Config) when is_list(Config) ->
    Records = [R1, R2, R3, R4, R5] =
        [#hostent{
            h_name = h_ex(Name),
            h_aliases = [h_ex(Alias) || Alias <- Aliases],
            h_addrtype = Fam,
            h_length =
                case Fam of
                    inet -> 4;
                    inet6 -> 16
                end,
            h_addr_list =
                [case Fam of
                     inet -> inet_ex(N);
                     inet6 -> inet6_ex(N)
                 end]}
         || {{Fam,N}, Name, Aliases} <-
                [{{inet,1}, "a", ["B"]},
                 {{inet,2}, "D", []},
                 {{inet6,3}, "A", ["c"]},
                 {{inet,1}, "c", []},
                 {{inet,5}, "A", []}]],
    true = R1#hostent.h_addr_list =:= R4#hostent.h_addr_list,
    R14 =
        R1#hostent{
          h_aliases =
              R1#hostent.h_aliases ++
              [R4#hostent.h_name | R4#hostent.h_aliases]},
    R145 =
        R14#hostent{
          h_addr_list =
              R1#hostent.h_addr_list ++ R5#hostent.h_addr_list},
    %%
    RootDir = proplists:get_value(priv_dir,Config),
    HostsFile = filename:join(RootDir, atom_to_list(?MODULE) ++ "-quirks.hosts"),
    InetRc = filename:join(RootDir, "quirks.inetrc"),
    ok = file:write_file(HostsFile, hostents_to_list(Records)),
    ok = file:write_file(InetRc, "{hosts_file, \"" ++ HostsFile ++ "\"}.\n"),
    %%
    %% start a node
    {ok, Peer, TestNode} = ?CT_PEER(["-kernel", "inetrc", "\"" ++ InetRc ++ "\""]),
    %% ensure it has our RC
    Rc = rpc:call(TestNode, inet_db, get_rc, []),
    {hosts_file, HostsFile} = lists:keyfind(hosts_file, 1, Rc),
    false = lists:keyfind(host, 1, Rc),
    %%
    %% check entries
    io:format("Check hosts file contents~n", []),
    V1 =
      [{R14, inet_ex(1)},
       {R2, inet_ex(2)},
       {R3, inet6_ex(3)},
       {R5, inet_ex(5)},
       {R145, h_ex("a"), inet},
       {R14, h_ex("b"), inet},
       {R14, h_ex("c"), inet},
       {R2, h_ex("d"), inet},
       {R3, h_ex("a"), inet6},
       {R3, h_ex("c"), inet6}
      ],
    hosts_file_quirks_verify(TestNode, V1),
    %%
    %% test add and del
    A1 = inet_ex(1),
    Hs1 = [h_ex("a"), h_ex("B")],
    ok = rpc:call(TestNode, inet_db, add_host, [A1, Hs1]),
    io:format("Check after add host~n", []),
    hosts_file_quirks_verify(
      TestNode,
      [{R1, A1},
       {R2, inet_ex(2)},
       {R3, inet6_ex(3)},
       {R5, inet_ex(5)},
       {R1, h_ex("a"), inet},
       {R1, h_ex("b"), inet},
       {R14, h_ex("c"), inet},
       {R2, h_ex("d"), inet},
       {R3, h_ex("a"), inet6},
       {R3, h_ex("c"), inet6}
      ]),
    {host, A1, Hs1} =
        lists:keyfind(host, 1, rpc:call(TestNode, inet_db, get_rc, [])),
    ok = rpc:call(TestNode, inet_db, del_host, [inet_ex(1)]),
    io:format("Check after del host~n", []),
    hosts_file_quirks_verify(TestNode, V1),
    %%
    %% cleanup
    peer:stop(Peer).

hosts_file_quirks_verify(_TestNode, Vs) ->
    hosts_file_quirks_verify(_TestNode, Vs, true).
%%
hosts_file_quirks_verify(_TestNode, [], Ok) ->
    case Ok of
        true -> ok;
        false -> error(verify_failed)
    end;
hosts_file_quirks_verify(TestNode, [V | Vs], Ok) ->
    case
        case V of
            {R, Addr} ->
                {R, rpc:call(TestNode, inet_hosts, gethostbyaddr, [Addr])};
            {R, Host, Fam} ->
                {R, rpc:call(TestNode, inet_hosts, gethostbyname, [Host, Fam])}
        end
    of
        {nxdomain, {error, nxdomain}} ->
            hosts_file_quirks_verify(TestNode, Vs, Ok);
        {_R_1, {error, nxdomain}} ->
            io:format("Verify failed ~p: nxdomain~n", [V]),
            hosts_file_quirks_verify(TestNode, Vs, false);
        {R_1, {ok, R_1}} ->
            hosts_file_quirks_verify(TestNode, Vs, Ok);
        {_R_1, {ok, R_2}} ->
            io:format("Verify failed ~p: ~p~n", [V, R_2]),
            hosts_file_quirks_verify(TestNode, Vs, false)
    end.

%% Expand entry
h_ex(H) -> H ++ ".example.com".
inet_ex(N) -> {127,17,17,N}.
inet6_ex(N) -> {0,0,0,0,17,17,17,N}.

hostents_to_list([]) -> [];
hostents_to_list([R | Rs]) ->
    #hostent{
       h_name = Name,
       h_aliases = Aliases,
       h_addr_list = [IP]} = R,
    [inet:ntoa(IP), $\t,
     lists:join($\s, [Name | Aliases]), $\r, $\n
     | hostents_to_list(Rs)].


%% These must be run in the whole suite since they need
%% the host list and require inet_gethost_native to be started.
%%
-record(gethostnative_control, {control_seq,
				control_interval = 100,
				lookup_delay     = 10,
				lookup_count     = 300,
				lookup_processes = 20}).

gethostnative_soft_restart() -> required(hosts).

%% Check that no name lookups fails during soft restart
%% of inet_gethost_native.
gethostnative_soft_restart(Config) when is_list(Config) ->
    Opts  = gethostnative_adjusted_opts(Config,
                                        [soft_restart]),
    gethostnative_control(Config, Opts).

gethostnative_debug_level() -> required(hosts).

%% Check that no name lookups fails during debug level change
%% of inet_gethost_native.
gethostnative_debug_level(Config) when is_list(Config) ->
    Opts  = gethostnative_adjusted_opts(Config,
                                        [{debug_level, 1},
                                         {debug_level, 0}]),
    gethostnative_control(Config, Opts).

gethostnative_adjusted_opts(Config, CtrlSeq) ->
    Factor = ?config(kernel_factor, Config),
    gethostnative_adjusted_opts2(Factor, CtrlSeq).

gethostnative_adjusted_opts2(1, CtrlSeq) ->
    #gethostnative_control{control_seq = CtrlSeq};
gethostnative_adjusted_opts2(F, CtrlSeq) ->
    Opts = #gethostnative_control{control_seq = CtrlSeq},
    gethostnative_adjusted_opts3(F, Opts).

gethostnative_adjusted_opts3(
  F,
  #gethostnative_control{lookup_count     = Cnt,
                         lookup_processes = NProc} = Opts) ->
    Adjust = fun(X) ->
                     if
                         F > 10 ->
                             X div 4;
                         F > 5 ->
                             X div 3;
                         F > 2 -> 
                             (2 * X) div 3;
                         true ->
                             X
                     end
             end,
    Opts#gethostnative_control{lookup_count     = Adjust(Cnt),
                               lookup_processes = Adjust(NProc)}.


gethostnative_control(Config, Opts) ->
    case inet_db:res_option(lookup) of
	[native] ->
	    case whereis(inet_gethost_native) of
		Pid when is_pid(Pid) ->
		    gethostnative_control_1(Config, Opts);
		_ ->
		    {skipped, "Not running native gethostbyname"}
	    end;
	_ ->
	    {skipped, "Native not only lookup method"}
    end.

gethostnative_control_1(
  Config,
  #gethostnative_control{control_seq      = Seq,
                         control_interval = Interval,
                         lookup_delay     = Delay,
                         lookup_count     = Cnt,
                         lookup_processes = N}) ->
    ?P("gethostnative control -> begin with"
       "~n      Ctrl Seq:      ~p"
       "~n      Ctrl interval: ~p"
       "~n      Lookup delay:  ~p"
       "~n      Lookup count:  ~p"
       "~n      Lookup procs:  ~p", [Seq, Interval, Delay, Cnt, N]),
    {ok, Hostname} = inet:gethostname(),
    {ok, _}        = inet:gethostbyname(Hostname),
    Hosts          =
        [Hostname|[H || {_,H} <- get_hosts(Config)]
         ++[H++D || H <- ["www.","www1.","www2.",""],
        	    D <- ["erlang.org","erlang.se"]]
         ++[H++"cslab.ericsson.net" || H <- ["morgoth.","hades.","styx."]]],
    ?P("ctrl-1: Hosts: "
       "~n   ~p", [Hosts]),
    %% Spawn some processes to do parallel lookups while
    %% I repeatedly do inet_gethost_native:control/1.
    TrapExit = process_flag(trap_exit, true),
    gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    erlang:display(first_intermission),
    ?P("ctrl-1: First intermission: "
       "~n   Now starting control sequence ~p", [Seq]),
    gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts),
    erlang:display(second_intermission),
    ?P("ctrl-1: Second intermission: "
       "~n   Now stopping control sequence ~p", [Seq]),
    gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    true = process_flag(trap_exit, TrapExit),
    ?P("control-1: done"),
    ok.

gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts) ->
    Tag       = make_ref(),
    Parent    = self(),
    Lookup    = fun() ->
                        ?P("lookuper starting"),
                        lookup_loop(Hosts, Delay, Tag, Parent, Cnt, Hosts) 
                end,
    Lookupers = [spawn_link(Lookup) || _ <- lists:seq(1, N)],
    control_loop(Seq, Interval, Tag, Lookupers, Seq),
    gethostnative_control_3(Tag, ok).

gethostnative_control_3(Tag, Reason) ->
    receive
	{Tag, Lookuper, Error} ->
            ?P("control-3: received lookup error from ~p: ~p",
               [Lookuper, Error]),
	    gethostnative_control_3(Tag, Error)
    after 0 ->
            ?P("control-3: no (more) lookup errors"),
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
    ?P("ctrl-loop-1: await lookuper exit"),
    receive
        {'EXIT', _Pid, {timetrap_timeout, _, _}} ->
            ?P("ctrl-loop-1: "
               "timetrap timeout while ~w lookupers remaining: "
               "~n   Op:       ~p"
               "~n   Interval: ~p"
               "~n   Tag:      ~p",
               [length(Lookupers), Op, Interval, Tag]),
            if
                (Op =/= undefined) ->
                    exit({timetrap, {Op, Tag, length(Lookupers)}});
                true ->
                    exit({timetrap, {Tag, length(Lookupers)}})
            end;

	{'EXIT', Pid, Reason} ->
	    case Reason of
		Tag -> % Done
                    ?P("ctrl-loop-1: "
                       "received expected exit from lookuper ~p", [Pid]),
		    control_loop_1
		      (Op, Interval, Tag,
		       lists:delete(Pid, Lookupers));
		_ ->
                    ?P("ctrl-loop-1: "
                       "received unexpected exit from ~p: "
                       "~n   ~p", [Pid, Reason]),
		    ct:fail(?F("Unexpected exit from ~p", [Pid]))
	    end

    after Interval ->
            ?P("ctrl-loop-1: "
               "timeout => (maybe) attempt control ~p when"
               "~n   Lookupers: ~p", [Op, Lookupers]),
	    if Op =/= undefined ->
		    ok = inet_gethost_native:control(Op);
	       true ->
		    ok
	    end,
	    Lookupers
    end.

lookup_loop(_, _Delay, Tag, _Parent,  0, _Hosts) ->
    ?P("lookup-loop: done with ~p", [Tag]),
    exit(Tag);
lookup_loop([], Delay, Tag, Parent, Cnt, Hosts) ->
    ?P("lookup-loop: begin lookup (~p) again when count = ~p", [Tag, Cnt]),
    lookup_loop(Hosts, Delay, Tag, Parent, Cnt, Hosts);
lookup_loop([H|Hs], Delay, Tag, Parent, Cnt, Hosts) ->
    ?P("lookup-loop: try lookup (~p, ~p) ~p", [Tag, Cnt, H]),
    case inet:gethostbyname(H) of
	{ok, _Hent} ->
            ?P("lookup-loop: lookup => found"),
            ok;
	{error, nxdomain} ->
            ?P("lookup-loop: lookup => nxdomain"),
            ok;
	Error ->
	    ?P("loopkup-loop: lookup => error for ~p: ~p", [H, Error]),
	    Parent ! {Tag, self(), Error}
    end,
    receive
    after rand:uniform(Delay) ->
	    lookup_loop(Hs, Delay, Tag, Parent, Cnt-1, Hosts) 
    end.



%% Test lookup with erroneously configured lookup option (OTP-12133).
lookup_bad_search_option(Config) when is_list(Config) ->
    %% Manipulation of resolver config is done in init_per_testcase
    %% and end_per_testcase to ensure cleanup.
    {ok,Hostname} = inet:gethostname(),
    {ok,_Hent} = inet:gethostbyname(Hostname), % Will hang loop for this bug
    ok.



%% Tests basic functionality of getiflist, getif, and ifget.
getif(Config) when is_list(Config) ->
    case os:type() of
	{unix,Osname} ->
	    do_getif(Osname);
	{_,_} ->
	    {skip,"inet:getif/0 probably not supported"}
    end.

do_getif(Osname) ->
    {ok,Hostname} = inet:gethostname(),
    {ok,Address} = inet:getaddr(Hostname, inet),
    {ok,Loopback} = inet:getaddr("localhost", inet),
    {ok,Interfaces} = inet:getiflist(),
    HWAs =
	lists:sort(
	  lists:foldl(
	    fun (I, Acc) ->
		    case inet:ifget(I, [hwaddr]) of
			{ok,[{hwaddr,A}]} -> [A|Acc];
			{ok,[]} -> Acc
		    end
	    end, [], Interfaces)),
    io:format("HWAs = ~p~n", [HWAs]),
    (Osname =/= sunos)
	andalso ((length(HWAs) > 0) orelse (ct:fail(no_HWAs))),
    Addresses =
	lists:sort(
	  lists:foldl(
	    fun (I, Acc) ->
		    case inet:ifget(I, [addr]) of
			{ok,[{addr,A}]} -> [A|Acc];
			{ok,[]} -> Acc
		    end
	    end, [], Interfaces)),
    {ok,Getif} = inet:getif(),
    Addresses = lists:sort([A || {A,_,_} <- Getif]),
    true = ip_member(Address, Addresses),
    true = ip_member(Loopback, Addresses),
    ok.

%% Test long interface names do not overrun buffer.
getif_ifr_name_overflow(Config) when is_list(Config) ->
    case os:type() of
	{unix,Osname} ->
	    do_getif_ifr_name_overflow(Osname);
	{_,_} ->
	    {skip,"inet:ifget/2 probably not supported"}
    end.

do_getif_ifr_name_overflow(_) ->
    %% emulator should not crash
    {ok,[]} = inet:ifget(lists:duplicate(128, "x"), [addr]),
    ok.

%% Test long service names do not overrun buffer.
getservbyname_overflow(Config) when is_list(Config) ->
    %% emulator should not crash
    {error,einval} = inet:getservbyname(list_to_atom(lists:flatten(lists:duplicate(128, "x"))), tcp),
    ok.

%% Test inet:gifaddrs/0.
getifaddrs(Config) when is_list (Config) ->
    {ok,IfAddrs} = inet:getifaddrs(),
    io:format("IfAddrs = ~p.~n", [IfAddrs]),
    case [If || {If,Opts} <- IfAddrs, lists:keymember(hwaddr, 1, Opts)] of
        [] ->
            case os:type() of
                {unix,sunos} -> ok;
                OT ->
                    ct:fail({should_have_hwaddr,OT})
            end;
        [_|_] -> ok
    end,
    Addrs = ifaddrs(IfAddrs),
    io:format("Addrs = ~p.~n", [Addrs]),
    [check_addr(Addr) || Addr <- Addrs],
    ok.

check_addr(Addr)
  when tuple_size(Addr) =:= 8,
       element(1, Addr) band 16#FFC0 =:= 16#FE80 ->
    io:format("Addr: ~p link local; SKIPPED!~n", [Addr]),
    ok;
check_addr(Addr) ->
    io:format("Addr: ~p.~n", [Addr]),
    Ping = "ping",
    Pong = "pong",
    {ok,L} = gen_tcp:listen(0, [{ip,Addr},{active,false}]),
    {ok,P} = inet:port(L),
    {ok,S1} = gen_tcp:connect(Addr, P, [{active,false}]),
    {ok,S2} = gen_tcp:accept(L),
    ok = gen_tcp:send(S2, Ping),
    {ok,Ping} = gen_tcp:recv(S1, length(Ping)),
    ok = gen_tcp:send(S1, Pong),
    ok = gen_tcp:close(S1),
    {ok,Pong} = gen_tcp:recv(S2, length(Pong)),
    ok = gen_tcp:close(S2),
    ok = gen_tcp:close(L).

ifaddrs(IfOpts) ->
    IfMap = collect_ifopts(IfOpts),
    ChkFun =
        fun Self({{_,Flags} = Key, Opts}, ok) ->
                Broadcast = lists:member(broadcast, Flags),
                P2P = lists:member(pointtopoint, Flags),
                case Opts of
                    [{addr,_},{netmask,_},{broadaddr,_}|Os]
                      when Broadcast ->
                        Self({Key, Os}, ok);
                    [{addr,_},{netmask,_},{dstaddr,_}|Os]
                      when P2P ->
                        Self({Key, Os}, ok);
                    [{addr,_},{netmask,_}|Os] ->
                        Self({Key, Os}, ok);
                    [{hwaddr,_}|Os] ->
                        Self({Key, Os}, ok);
                    [] ->
                        ok
                end
        end,
    fold_ifopts(ChkFun, ok, IfMap),
    AddrsFun =
        fun ({{_,Flags}, Opts}, Acc) ->
                case
                    lists:member(running, Flags)
                    andalso (not lists:member(pointtopoint, Flags))
                of
                    true ->
                        lists:reverse(
                          [Addr || {addr,Addr} <- Opts],
                          Acc);
                    false ->
                        Acc
                end
        end,
    fold_ifopts(AddrsFun, [], IfMap).

collect_ifopts(IfOpts) ->
    collect_ifopts(IfOpts, #{}).
%%
collect_ifopts(IfOpts, IfMap) ->
    case IfOpts of
        [{If,[{flags,Flags}|Opts]}|IfOs] ->
            Key = {If,Flags},
            case maps:is_key(Key, IfMap) of
                true ->
                    ct:fail({unexpected_ifopts,IfOpts,IfMap});
                false ->
                    collect_ifopts(IfOs, IfMap, Opts, Key, [])
            end;
        [] ->
            IfMap;
        _ ->
            ct:fail({unexpected_ifopts,IfOpts,IfMap})
    end.
%%
collect_ifopts(IfOpts, IfMap, Opts, Key, R) ->
    case Opts of
        [{flags,_}|_] ->
            {If,_} = Key,
            collect_ifopts(
              [{If,Opts}|IfOpts], maps:put(Key, lists:reverse(R), IfMap));
        [OptVal|Os] ->
            collect_ifopts(IfOpts, IfMap, Os, Key, [OptVal|R]);
        [] ->
            collect_ifopts(IfOpts, maps:put(Key, lists:reverse(R), IfMap))
    end.

fold_ifopts(Fun, Acc, IfMap) ->
    fold_ifopts(Fun, Acc, IfMap, maps:keys(IfMap)).
%%
fold_ifopts(Fun, Acc, IfMap, Keys) ->
    case Keys of
        [Key|Ks] ->
            Opts = maps:get(Key, IfMap),
            fold_ifopts(Fun, Fun({Key,Opts}, Acc), IfMap, Ks);
        [] ->
            Acc
    end.

%% Works just like lists:member/2, except that any {127,_,_,_} tuple
%% matches any other {127,_,_,_}. We do this to handle Linux systems
%% that use (for instance) 127.0.1.1 as the IP address for the hostname.

ip_member({127,_,_,_}, [{127,_,_,_}|_]) -> true;
ip_member(K, [K|_]) -> true;
ip_member(K, [_|T]) -> ip_member(K, T);
ip_member(_, []) -> false.

%% Case fold to upper case according to RFC 4343
%%
toupper([C|Cs]) when is_integer(C) ->
    if  $a =< C, C =< $z ->
	    [(C - $a + $A)|toupper(Cs)];
	true ->
	    [C|toupper(Cs)]
    end;
toupper([]) ->
    [].


simple_netns(Config) when is_list(Config) ->
    {ok,U} = gen_udp:open(0),
    case inet:setopts(U, [{netns,""}]) of
	ok ->
	    jog_netns_opt(U),
	    ok = gen_udp:close(U),
	    %%
	    {ok,L} = gen_tcp:listen(0, []),
	    jog_netns_opt(L),
	    ok = gen_tcp:close(L),
	    %%
	    case gen_sctp:open() of
		{ok,S} ->
		    jog_netns_opt(S),
		    ok = gen_sctp:close(S);
		{error,eprotonosupport} ->
		    ok
	    end;
	{error,einval} ->
	    {skip,"setns() not supported"}
    end.

jog_netns_opt(S) ->
    %% This is just jogging the option mechanics
    ok = inet:setopts(S, [{netns,""}]),
    {ok,[{netns,""}]} = inet:getopts(S, [netns]),
    ok = inet:setopts(S, [{netns,"/proc/self/ns/net"}]),
    {ok,[{netns,"/proc/self/ns/net"}]} = inet:getopts(S, [netns]),
    ok.


%% Smoke test netns support.
simple_netns_open(Config) when is_list(Config) ->
    %% Note: {error,enoent} will be returned if the run-time executable
    %%       has support for netns, but /proc/self/ns/net is missing.
    case gen_udp:open(0, [binary,{netns,"/"},inet]) of
	{ok,U} ->
	    ok = gen_udp:close(U);
	{error,E1} when E1 =:= einval; E1 =:= eperm; E1 =:= enoent ->
	    ok
    end,
    case gen_tcp:listen(0, [binary,{netns,"/"},inet]) of
	{ok,T} ->
	    ok = gen_tcp:close(T);
	{error,E2} when E2 =:= einval; E2 =:= eperm; E2 =:= enoent ->
	    ok
    end,
    try gen_sctp:open(0, [binary,{netns,"/"},inet]) of
	{ok,S} ->
	    ok = gen_sctp:close(S);
	{error,E3}
	  when E3 =:= einval; E3 =:= eperm;
	       E3 =:= enoent; E3 =:= eprotonosupport ->
	    ok
    catch
	error:badarg ->
	    %% Some older platforms does not allow netns for sctp
	    ok
    end.


%% Manual test to be run outside test_server in an emulator
%% started by root, in a machine with setns() support...
test_netns() ->
    DefaultIF = v1,
    DefaultIP = {192,168,1,17},
    Namespace = "test",
    NamespaceIF = v2,
    NamespaceIP = {192,168,1,18},
    %%
    DefaultIPString = inet_parse:ntoa(DefaultIP),
    NamespaceIPString = inet_parse:ntoa(NamespaceIP),
    cmd("ip netns add ~s",
	[Namespace]),
    cmd("ip link add name ~w type veth peer name ~w netns ~s",
        [DefaultIF,NamespaceIF,Namespace]),
    cmd("ip netns exec ~s ip addr add ~s/30 dev ~w",
	[Namespace,NamespaceIPString,NamespaceIF]),
    cmd("ip netns exec ~s ip link set ~w up",
	[Namespace,NamespaceIF]),
    cmd("ip addr add ~s/30 dev ~w",
	[DefaultIPString,DefaultIF]),
    cmd("ip link set ~w up",
	[DefaultIF]),
    try test_netns(
	  {DefaultIF,DefaultIP},
	  filename:join("/var/run/netns/", Namespace),
	  {NamespaceIF,NamespaceIP}) of
	Result ->
	    io:put_chars(["#### Test done",io_lib:nl()]),
	    Result
    after
	cmd("ip link delete ~w type veth",
	    [DefaultIF]),
	cmd("ip netns delete ~s",
	    [Namespace])
    end.

test_netns({DefaultIF,DefaultIP}, Namespace, {NamespaceIF,NamespaceIP}) ->
    {ok,ListenSocket} = gen_tcp:listen(0, [{active,false}]),
    {ok,[{addr,DefaultIP}]} = inet:ifget(ListenSocket, DefaultIF, [addr]),
    {ok,ListenPort} = inet:port(ListenSocket),
    {ok,ConnectSocket} =
	gen_tcp:connect(
	  DefaultIP, ListenPort, [{active,false},{netns,Namespace}], 3000),
    {ok,[{addr,NamespaceIP}]} = inet:ifget(ConnectSocket, NamespaceIF, [addr]),
    {ok,ConnectPort} = inet:port(ConnectSocket),
    {ok,AcceptSocket} = gen_tcp:accept(ListenSocket, 0),
    {ok,AcceptPort} = inet:port(AcceptSocket),
    {ok,{NamespaceIP,ConnectPort}} = inet:peername(AcceptSocket),
    {ok,{DefaultIP,AcceptPort}} = inet:peername(ConnectSocket),
    ok = gen_tcp:send(ConnectSocket, "data"),
    ok = gen_tcp:close(ConnectSocket),
    {ok,"data"} = gen_tcp:recv(AcceptSocket, 4, 1000),
    {error,closed} = gen_tcp:recv(AcceptSocket, 1, 1000),
    ok = gen_tcp:close(AcceptSocket),
    ok = gen_tcp:close(ListenSocket).

cmd(Cmd, Args) ->
    cmd(io_lib:format(Cmd, Args)).
%%
cmd(CmdString) ->
    io:put_chars(["# ",CmdString,io_lib:nl()]),
    io:put_chars([os:cmd(CmdString++" ; echo '  =>' $?")]),
    ok.

-define(CAP_NET_RAW, 13).        %% from /usr/include/linux/capability.h

can_bind_to_device({unix, linux}, {Major, _, _})
  when Major > 2 ->
    Status = os:cmd("cat /proc/self/status | grep CapEff"),
    [_, CapEffStr] = string:tokens(Status, [$\n, $\t]),
    CapEff = list_to_integer(CapEffStr, 16),
    if CapEff band (1 bsl ?CAP_NET_RAW) =/= 0 ->
            ok;
       true ->
            {skip,"insufficient capabilities, CAP_NET_RAW not granted"}
    end;
can_bind_to_device(_OS, _Version) ->
    {skip,"socket option bind_to_device not supported on this OS or version"}.

simple_bind_to_device(Config) when is_list(Config) ->
    case can_bind_to_device(os:type(), os:version()) of
        ok ->
            {ok,U} = gen_udp:open(0),
            jog_bind_to_device_opt(U),
            ok = gen_udp:close(U),
            %%
            {ok,L} = gen_tcp:listen(0, []),
            jog_bind_to_device_opt(L),
            ok = gen_tcp:close(L),
            %%
            case gen_sctp:open() of
                {ok,S} ->
                    jog_bind_to_device_opt(S),
                    ok = gen_sctp:close(S);
                {error,eprotonosupport} ->
                    ok
            end;
        Other ->
            Other
    end.

%% Smoke test bind_to_device support.
simple_bind_to_device_open(Config) when is_list(Config) ->
    case can_bind_to_device(os:type(), os:version()) of
        ok ->
            {ok,U} = gen_udp:open(0, [binary,{bind_to_device,<<"lo">>},inet]),
            ok = gen_udp:close(U),
            {ok,T} = gen_tcp:listen(0, [binary,{bind_to_device,<<"lo">>},inet]),
            ok = gen_tcp:close(T),

            case gen_sctp:open(0, [binary,{bind_to_device,<<"lo">>},inet]) of
                {ok,S} ->
                    ok = gen_sctp:close(S);
                {error,eprotonosupport} ->
                    ok
            end;
        Other ->
            Other
    end.

jog_bind_to_device_opt(S) ->
    %% This is just jogging the option mechanics
    ok = inet:setopts(S, [{bind_to_device,<<>>}]),
    {ok,[{bind_to_device,<<>>}]} = inet:getopts(S, [bind_to_device]),
    ok = inet:setopts(S, [{bind_to_device,<<"lo">>}]),
    {ok,[{bind_to_device,<<"lo">>}]} = inet:getopts(S, [bind_to_device]),
    ok.

add_del_host(_Config) ->
    Name = "foo.com",
    Alias = "bar.org",
    Ip = {69,89,31,226},
    HostEnt = #hostent{
        h_name = Name,
        h_aliases = [Alias],
        h_addrtype = inet,
        h_length = 4,
        h_addr_list = [Ip]
    },
    {error, nxdomain} = inet_hosts:gethostbyname(Name, inet),
    ok = inet_db:add_host(Ip, [Name, Alias]),
    {ok, HostEnt} = inet_hosts:gethostbyname(Name, inet),
    {ok, HostEnt} = inet_hosts:gethostbyname(Alias, inet),
    ok = inet_db:del_host(Ip),
    {error, nxdomain} = inet_hosts:gethostbyname(Name, inet),
    {error, nxdomain} = inet_hosts:gethostbyname(Alias, inet),
    ok = inet_db:add_host(Ip, [Name, Alias]),
    {ok, HostEnt} = inet_hosts:gethostbyname(Name, inet).

add_del_host_v6(_Config) ->
    Name = "foo.com",
    Alias = "bar.org",
    Ip = {32,1,219,8,10,11,18,240},
    HostEnt = #hostent{
        h_name = Name,
        h_aliases = [Alias],
        h_addrtype = inet6,
        h_length = 16,
        h_addr_list = [Ip]
    },
    {error, nxdomain} = inet_hosts:gethostbyname(Name, inet6),
    ok = inet_db:add_host(Ip, [Name, Alias]),
    {ok, HostEnt} = inet_hosts:gethostbyname(Name, inet6),
    {ok, HostEnt} = inet_hosts:gethostbyname(Alias, inet6),
    ok = inet_db:del_host(Ip),
    {error, nxdomain} = inet_hosts:gethostbyname(Name, inet6),
    {error, nxdomain} = inet_hosts:gethostbyname(Alias, inet6),
    ok = inet_db:add_host(Ip, [Name, Alias]),
    {ok, HostEnt} = inet_hosts:gethostbyname(Name, inet6).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This is a supremely simple test case.
%%% It basically just tests that the function (inet:socknames/1,2)
%%% returns with the expected result (a list of addresses, which in
%%% the case of tcp and udp should be exactly one address long).

socknames_sctp(Config) when is_list(Config) ->
    %%% ?TC_TRY(?FUNCTION_NAME, fun() -> do_socknames_sctp(Config) end).
    {skip, not_implemented_yet}.


socknames_tcp(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_socknames_tcp0(Config) end).

do_socknames_tcp0(_Config) ->
    %% Begin with a the plain old boring (= port) socket(s)
    ?P("Test socknames for 'old' socket (=port)"),
    do_socknames_tcp1([]),

    %% And *maybe* also check the 'new' shiny socket sockets
    try socket:info() of
        #{} ->
            ?P("Test socknames for 'new' socket (=socket nif)"),
            do_socknames_tcp1([{inet_backend, socket}])
    catch
        error:notsup ->
            ?P("Skip test of socknames for 'new' socket (=socket nif)"),
            ok;
        error:undef:ST ->
            case ST of
                [{prim_socket,info,[],_}|_] ->
                    ?P("Skip test of socknames for 'new' socket (=socket nif)")
            end
    end.


do_socknames_tcp1(Conf) ->
    ?P("try create listen socket"),
    {ok, S1} = gen_tcp:listen(0, Conf),
    ?P("try get socknames for (listen) socket: "
       "~n      ~p", [S1]),
    PortNumber1 = case inet:socknames(S1) of
		      {ok, [{_Addr1, PortNo1}]} ->
			  PortNo1;
		      {ok, Addrs1} ->
                          ?P("failed socknames: unexpected number of addresses"
                             "~n      ~p", [Addrs1]),
			  exit({unexpected_addrs_length, length(Addrs1)});
		      {error, Reason1} ->
                          ?P("failed socknames: error"
                             "~n      ~p", [Reason1]),
			  exit({skip, {listen_socket, Reason1}})
		  end,
    ?P("try connect to listen socket on port ~p", [PortNumber1]),
    {ok, S2} = gen_tcp:connect("localhost", PortNumber1, Conf),
    ?P("try get socknames for (connected) socket: "
       "~n      ~p", [S2]),
    case inet:socknames(S2) of
	{ok, [_Addr2]} ->
	    ok;
	{ok, Addrs2} ->
            ?P("failed socknames: unexpected number of addresses"
               "~n      ~p", [Addrs2]),
	    exit({unexpected_addrs_length, length(Addrs2)});
	{error, Reason2} ->
            ?P("failed socknames: error"
               "~n      ~p", [Reason2]),
	    exit({skip, {connected_socket, Reason2}})
    end,
    ?P("try accept connection"),
    {ok, S3} = gen_tcp:accept(S1),
    ?P("try get socknames for (accepted) socket: "
       "~n      ~p", [S3]),
    case inet:socknames(S3) of
	{ok, [_Addr3]} ->
	    ok;
	{ok, Addrs3} ->
            ?P("failed socknames: unexpected number of addresses"
               "~n      ~p", [Addrs3]),
	    exit({unexpected_addrs_length, length(Addrs3)});
	{error, Reason3} ->
            ?P("failed socknames: error"
               "~n      ~p", [Reason3]),
	    exit({skip, {accepted_socket, Reason3}})
    end,
    ?P("close socket(s)"),
    (catch gen_tcp:close(S3)),
    (catch gen_tcp:close(S2)),
    (catch gen_tcp:close(S1)),
    ?P("done"),
    ok.


socknames_udp(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_socknames_udp0(Config) end).

do_socknames_udp0(_Config) ->
    %% Begin with a the plain old boring (= port) socket(s)
    ?P("Test socknames for 'old' socket (=port)"),
    do_socknames_udp1([]),

    %% And *maybe* also check the 'new' shiny socket sockets
    try socket:info() of
        #{} ->
            ?P("Test socknames for 'new' socket (=socket nif)"),
            do_socknames_udp1([{inet_backend, socket}])
    catch
        error : notsup ->
            ?P("Skip test of socknames for 'new' socket (=socket nif)"),
            ok;
        error:undef:ST ->
            case ST of
                [{prim_socket,info,[],_}|_] ->
                    ?P("Skip test of socknames for 'new' socket (=socket nif)")
            end
    end.


do_socknames_udp1(Conf) ->
    ?P("try create socket"),
    {ok, S1} = gen_udp:open(0, Conf),
    ?P("try get socknames for socket: "
       "~n      ~p", [S1]),
    case inet:socknames(S1) of
        {ok, [_Addr1]} ->
            ok;
        {ok, Addrs1} ->
            ?P("failed socknames: unexpected number of addresses"
               "~n      ~p", [Addrs1]),
            exit({unexpected_addrs_length, length(Addrs1)});
        {error, Reason1} ->
            ?P("failed socknames: error"
               "~n      ~p", [Reason1]),
            exit({skip, {listen_socket, Reason1}})
    end,
    ?P("close socket"),
    (catch gen_udp:close(S1)),
    ?P("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    

