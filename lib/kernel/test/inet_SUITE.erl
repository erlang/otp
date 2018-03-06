%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-include_lib("kernel/src/inet_dns.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 t_gethostbyaddr/0, t_gethostbyaddr/1,
	 t_getaddr/0, t_getaddr/1,
	 t_gethostbyname/0, t_gethostbyname/1,
	 t_gethostbyaddr_v6/0, t_gethostbyaddr_v6/1,
	 t_getaddr_v6/0, t_getaddr_v6/1,
	 t_gethostbyname_v6/0, t_gethostbyname_v6/1,
	 ipv4_to_ipv6/0, ipv4_to_ipv6/1,
	 host_and_addr/0, host_and_addr/1,
	 t_gethostnative/1, 
	 gethostnative_parallell/1, cname_loop/1, 
         gethostnative_soft_restart/0, gethostnative_soft_restart/1,
	 gethostnative_debug_level/0, gethostnative_debug_level/1,
	 lookup_bad_search_option/1,
	 getif/1,
	 getif_ifr_name_overflow/1,getservbyname_overflow/1, getifaddrs/1,
	 parse_strict_address/1, ipv4_mapped_ipv6_address/1,
         simple_netns/1, simple_netns_open/1,
         simple_bind_to_device/1, simple_bind_to_device_open/1]).

-export([get_hosts/1, get_ipv6_hosts/1, parse_hosts/1, parse_address/1,
	 kill_gethost/0, parallell_gethost/0, test_netns/0]).
-export([init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [t_gethostbyaddr, t_gethostbyname, t_getaddr,
     t_gethostbyaddr_v6, t_gethostbyname_v6, t_getaddr_v6,
     ipv4_to_ipv6, host_and_addr, {group, parse},
     t_gethostnative, gethostnative_parallell, cname_loop,
     gethostnative_debug_level, gethostnative_soft_restart,
     lookup_bad_search_option,
     getif, getif_ifr_name_overflow, getservbyname_overflow,
     getifaddrs, parse_strict_address, simple_netns, simple_netns_open,
     simple_bind_to_device, simple_bind_to_device_open].

groups() -> 
    [{parse, [], [parse_hosts, parse_address]}].

%% Required configuaration
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

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(lookup_bad_search_option, Config) ->
    Db = inet_db,
    Key = res_lookup,
    %% The bad option can not enter through inet_db:set_lookup/1,
    %% but through e.g .inetrc.
    Prev = ets:lookup(Db, Key),
    ets:delete(Db, Key),
    ets:insert(Db, {Key,[lookup_bad_search_option]}),
    io:format("Misconfigured resolver lookup order", []),
    [{Key,Prev}|Config];
init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(lookup_bad_search_option, Config) ->
    Db = inet_db,
    Key = res_lookup,
    Prev = proplists:get_value(Key, Config),
    ets:delete(Db, Key),
    ets:insert(Db, Prev),
    io:format("Restored resolver lookup order", []);
end_per_testcase(_Func, _Config) ->
    ok.

t_gethostbyaddr() ->
    required(v4).
%% Test the inet:gethostbyaddr/1 function.
t_gethostbyaddr(Config) when is_list(Config) ->
    {Name,FullName,IPStr,{A,B,C,D}=IP,Aliases,_,_} = ct:get_config(test_host_ipv4_only),
    Rname = integer_to_list(D) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(A) ++ ".in-addr.arpa",
    {ok,HEnt} = inet:gethostbyaddr(IPStr),
    {ok,HEnt} = inet:gethostbyaddr(IP),
    {error,Error} = inet:gethostbyaddr(Name),
    ok = io:format("Failure reason: ~p: ~s", [error,inet:format_error(Error)]),
    HEnt_ = HEnt#hostent{h_addrtype = inet,
                         h_length = 4,
                         h_addr_list = [IP]},
    HEnt_ = HEnt,
    case {os:type(),os:version()} of
        {{unix,freebsd},{5,0,0}} ->
            %% The alias list seems to be buggy in FreeBSD 5.0.0.
            check_elems([{HEnt#hostent.h_name,[Name,FullName]}]),
            io:format("Buggy alias list: ~p", [HEnt#hostent.h_aliases]),
            ok;
        _ ->
            io:format("alias list: ~p", [HEnt#hostent.h_aliases]),
            io:format(
	      "check alias list: ~p", [[Aliases,tl(Aliases),[Rname]]]),
            io:format("name: ~p", [HEnt#hostent.h_name]),
            io:format("check name: ~p", [[Name,FullName]]),
            check_elems(
	      [{HEnt#hostent.h_name,[Name,FullName]},
	       {HEnt#hostent.h_aliases,[[],Aliases,tl(Aliases),[Rname]]}])
    end,

    {_DName, _DFullName, DIPStr, DIP, _, _, _} = ct:get_config(test_dummy_host),
    {error,nxdomain} = inet:gethostbyaddr(DIPStr),
    {error,nxdomain} = inet:gethostbyaddr(DIP),
    ok.

t_gethostbyaddr_v6() -> required(v6).
%% Test the inet:gethostbyaddr/1 inet6 function.
t_gethostbyaddr_v6(Config) when is_list(Config) ->
    {Name6, FullName6, IPStr6, IP6, Aliases6} =
	ct:get_config(test_host_ipv6_only),

    case inet:gethostbyaddr(IPStr6) of
        %% Even if IPv6 is not supported, the native resolver may succeed
	%% looking up the host. DNS lookup will probably fail.
	{error,nxdomain} ->
	    {skip, "IPv6 test fails! IPv6 not supported on this host!?"};
	{ok,HEnt6} ->
	    {ok,HEnt6} = inet:gethostbyaddr(IP6),
	    {error,Error6} = inet:gethostbyaddr(Name6),
	    ok = io:format("Failure reason: ~p: ~s",
			   [Error6, inet:format_error(Error6)]),
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

t_gethostbyname_v6() -> required(v6).
%% Test the inet:gethostbyname/1 inet6 function.
t_gethostbyname_v6(Config) when is_list(Config) ->
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

host_and_addr() ->
    [{timetrap,{minutes,5}}|required(hosts)].

%% Test looking up hosts and addresses. Use 'ypcat hosts'
%% or the local eqivalent to find all hosts.
host_and_addr(Config) when is_list(Config) ->
    lists:foreach(fun try_host/1, get_hosts(Config)),
    ok.

try_host({Ip0, Host}) ->
    {ok,Ip} = inet:getaddr(Ip0, inet),
    {ok,{hostent, _, _, inet, _, Ips1}} = inet:gethostbyaddr(Ip),
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
t_parse_address(Func, Reversable, [{Addr,String}|L]) ->
    io:format("~p = ~p.~n", [Addr,String]),
    {ok,Addr} = inet:Func(String),
    case Reversable of
        true ->String = inet:ntoa(Addr);
        false -> ok
    end,
    t_parse_address(Func, Reversable, L);
t_parse_address(Func, Reversable, [String|L]) ->
    io:format("~p.~n", [String]),
    {error,einval} = inet:Func(String),
    t_parse_address(Func, Reversable, L).

parse_strict_address(Config) when is_list(Config) ->
    {ok, {127,0,0,1}} =
	inet:parse_strict_address("127.0.0.1"),
    {ok, {3089,3106,23603,50240,21952,50796,119,136}} =
	inet:parse_strict_address("c11:0c22:5c33:c440:55c0:c66c:77:0088"),
    {ok, {3089,3106,23603,50240,0,0,119,136}} =
	inet:parse_strict_address("c11:0c22:5c33:c440::077:0088").

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

t_gethostnative(Config) when is_list(Config) ->
    %% this will result in 26 bytes sent which causes problem in Windows
    %% if the port-program has not assured stdin to be read in BINARY mode
    %% OTP-2555
    case inet_gethost_native:gethostbyname(
	   "a23456789012345678901234") of
	{error,notfound} ->
	    ok;
	{error,no_data} ->
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
    PA = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(gethost_parallell, slave,
				       [{args, "-pa " ++ PA}]),
    ok = rpc:call(Node, ?MODULE, parallell_gethost, []),
    receive after 10000 -> ok end,
    pong = net_adm:ping(Node),
    test_server:stop_node(Node),
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



%% These must be run in the whole suite since they need
%% the host list and require inet_gethost_native to be started.
%%
-record(gethostnative_control, {control_seq,
				control_interval=100,
				lookup_delay=10,
				lookup_count=300,
				lookup_processes=20}).

gethostnative_soft_restart() -> required(hosts).

%% Check that no name lookups fails during soft restart
%% of inet_gethost_native.
gethostnative_soft_restart(Config) when is_list(Config) ->
    gethostnative_control(Config,
			  #gethostnative_control{
			     control_seq=[soft_restart]}).


gethostnative_debug_level() -> required(hosts).

%% Check that no name lookups fails during debug level change
%% of inet_gethost_native.
gethostnative_debug_level(Config) when is_list(Config) ->
    gethostnative_control(Config,
			  #gethostnative_control{
			     control_seq=[{debug_level,1},
					  {debug_level,0}]}).

gethostnative_control(Config, Optrec) ->
    case inet_db:res_option(lookup) of
	[native] ->
	    case whereis(inet_gethost_native) of
		Pid when is_pid(Pid) ->
		    gethostnative_control_1(Config, Optrec);
		_ ->
		    {skipped, "Not running native gethostbyname"}
	    end;
	_ ->
	    {skipped, "Native not only lookup metod"}
    end.

gethostnative_control_1(Config,
			#gethostnative_control{
			   control_seq=Seq,
			   control_interval=Interval,
			   lookup_delay=Delay,
			   lookup_count=Cnt,
			   lookup_processes=N}) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, _} = inet:gethostbyname(Hostname),
    Hosts =
	[Hostname|[H || {_,H} <- get_hosts(Config)]
	 ++[H++D || H <- ["www.","www1.","www2.",""],
		    D <- ["erlang.org","erlang.se"]]
	 ++[H++"cslab.ericsson.net" || H <- ["morgoth.","hades.","styx."]]],
    %% Spawn some processes to do parallel lookups while
    %% I repeatedly do inet_gethost_native:control/1.
    TrapExit = process_flag(trap_exit, true),
    gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    io:format(
      "First intermission: now starting control sequence ~w\n",
      [Seq]),
    erlang:display(first_intermission),
    gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts),
    erlang:display(second_intermission),
    io:format(
      "Second intermission:  now stopping control sequence ~w\n",
      [Seq]),
    gethostnative_control_2([undefined], Interval, Delay, Cnt, N, Hosts),
    true = process_flag(trap_exit, TrapExit),
    ok.

gethostnative_control_2(Seq, Interval, Delay, Cnt, N, Hosts) ->
    Tag = make_ref(),
    Parent = self(),
    Lookupers =
	[spawn_link(
	   fun () -> 
		   lookup_loop(Hosts, Delay, Tag, Parent, Cnt, Hosts) 
	   end)
	 || _ <- lists:seq(1, N)],
    control_loop(Seq, Interval, Tag, Lookupers, Seq),
    gethostnative_control_3(Tag, ok).

gethostnative_control_3(Tag, Reason) ->
    receive
	{Tag,Error} ->
	    gethostnative_control_3(Tag, Error)
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
    receive
	{'EXIT',Pid,Reason} ->
	    case Reason of
		Tag -> % Done
		    control_loop_1
		      (Op, Interval, Tag,
		       lists:delete(Pid, Lookupers));
		_ ->
		    io:format("Lookuper ~p died: ~p",
			      [Pid,Reason]),
		    ct:fail("Lookuper died")
	    end
    after Interval ->
	    if Op =/= undefined ->
		    ok = inet_gethost_native:control(Op);
	       true ->
		    ok
	    end,
	    Lookupers
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
	    io:format("Name lookup error for ~p for ~p: ~p",
		      [self(),H,Error]),
	    Parent ! {Tag,Error}
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
    case
	{os:type(),
	 [If ||
	     {If,Opts} <- IfAddrs,
	     lists:keymember(hwaddr, 1, Opts)]} of
	{{unix,sunos},[]} -> ok;
	{OT,[]} ->
	    ct:fail({should_have_hwaddr,OT});
	_ -> ok
    end,
    Addrs =
	[element(1, A) || A <- ifaddrs(IfAddrs)],
    io:format("Addrs = ~p.~n", [Addrs]),
    [check_addr(Addr) || Addr <- Addrs],
    ok.

check_addr({addr,Addr})
  when tuple_size(Addr) =:= 8,
       element(1, Addr) band 16#FFC0 =:= 16#FE80 ->
    io:format("Addr: ~p link local; SKIPPED!~n", [Addr]),
    ok;
check_addr({addr,Addr}) ->
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

-record(ifopts, {name,flags,addrs=[],hwaddr}).

ifaddrs([]) -> [];
ifaddrs([{If,Opts}|IOs]) ->
    #ifopts{flags=F} = Ifopts = check_ifopts(Opts, #ifopts{name=If}),
    case F of
	{flags,Flags} ->
	    case lists:member(running, Flags) of
		true -> Ifopts#ifopts.addrs;
		false -> []
	    end ++ ifaddrs(IOs);
	undefined ->
	    ifaddrs(IOs)
    end.

check_ifopts([], #ifopts{flags=F,addrs=Raddrs}=Ifopts) ->
    Addrs = lists:reverse(Raddrs),
    R = Ifopts#ifopts{addrs=Addrs},
    io:format("~p.~n", [R]),
    %% See how we did...
    {flags,Flags} = F,
    case lists:member(broadcast, Flags) of
	true ->
	    [case A of
		 {{addr,_},{netmask,_},{broadaddr,_}} ->
		     A;
		 {{addr,T},{netmask,_}} when tuple_size(T) =:= 8 ->
		     A
	     end || A <- Addrs];
	false ->
	    case lists:member(pointtopoint, Flags) of
		true ->
		    [case A of
			 {{addr,_},{netmask,_},{dstaddr,_}} ->
			     A
		     end || A <- Addrs];
		false ->
		    [case A of
			 {{addr,_},{netmask,_}} ->
			     A
		     end || A <- Addrs]
	    end
    end,
    R;
check_ifopts([{flags,_}=F|Opts], #ifopts{flags=undefined}=Ifopts) ->
    check_ifopts(Opts, Ifopts#ifopts{flags=F});
check_ifopts([{flags,_}=F|Opts], #ifopts{flags=Flags}=Ifopts) ->
    case F of
	Flags ->
	    check_ifopts(Opts, Ifopts);
	_ ->
	    ct:fail({multiple_flags,F,Ifopts})
    end;
check_ifopts(
  [{addr,_}=A,{netmask,_}=N,{dstaddr,_}=D|Opts],
  #ifopts{addrs=Addrs}=Ifopts) ->
    check_ifopts(Opts, Ifopts#ifopts{addrs=[{A,N,D}|Addrs]});
check_ifopts(
  [{addr,_}=A,{netmask,_}=N,{broadaddr,_}=B|Opts],
  #ifopts{addrs=Addrs}=Ifopts) ->
    check_ifopts(Opts, Ifopts#ifopts{addrs=[{A,N,B}|Addrs]});
check_ifopts(
  [{addr,_}=A,{netmask,_}=N|Opts],
  #ifopts{addrs=Addrs}=Ifopts) ->
    check_ifopts(Opts, Ifopts#ifopts{addrs=[{A,N}|Addrs]});
check_ifopts([{addr,_}=A|Opts], #ifopts{addrs=Addrs}=Ifopts) ->
    check_ifopts(Opts, Ifopts#ifopts{addrs=[{A}|Addrs]});
check_ifopts([{hwaddr,Hwaddr}=H|Opts], #ifopts{hwaddr=undefined}=Ifopts)
  when is_list(Hwaddr) ->
    check_ifopts(Opts, Ifopts#ifopts{hwaddr=H});
check_ifopts([{hwaddr,_}=H|_], #ifopts{}=Ifopts) ->
    ct:fail({multiple_hwaddrs,H,Ifopts}).

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
