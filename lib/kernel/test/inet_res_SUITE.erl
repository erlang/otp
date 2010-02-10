%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
-module(inet_res_SUITE).

-include("test_server.hrl").
-include("test_server_line.hrl").

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([all/1, init_per_testcase/2, end_per_testcase/2]).
-export([basic/1, resolve/1, edns0/1, txt_record/1, files_monitor/1]).
-export([gethostbyaddr/1, gethostbyaddr_v6/1,
	 gethostbyname/1, gethostbyname_v6/1,
	 getaddr/1, getaddr_v6/1, ipv4_to_ipv6/1, host_and_addr/1]).

-define(RUN_NAMED, "run-named").

all(suite) ->
    [basic, resolve, edns0, txt_record, files_monitor,
     gethostbyaddr, gethostbyaddr_v6, gethostbyname, gethostbyname_v6,
     getaddr, getaddr_v6, ipv4_to_ipv6, host_and_addr].

zone_dir(basic) ->
    otptest;
zone_dir(resolve) ->
    otptest;
zone_dir(edns0) ->
    otptest;
zone_dir(files_monitor) ->
    otptest;
zone_dir(_) ->
    undefined.

init_per_testcase(Func, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    try ns_init(zone_dir(Func), PrivDir, DataDir) of
	NsSpec ->
	    Lookup = inet_db:res_option(lookup),
	    inet_db:set_lookup([file,dns]),
	    case NsSpec of
		{_,{IP,Port},_} ->
		    inet_db:ins_alt_ns(IP, Port);
		_ -> ok
	    end,
	    Dog = test_server:timetrap(test_server:seconds(10)),
	    [{nameserver,NsSpec},{res_lookup,Lookup},{watchdog,Dog}|Config]
    catch
	SkipReason ->
	    {skip,SkipReason}
    end.

end_per_testcase(_Func, Config) ->
    test_server:timetrap_cancel(?config(watchdog, Config)),
    inet_db:set_lookup(?config(res_lookup, Config)),
    NsSpec = ?config(nameserver, Config),
    case NsSpec of
	{_,{IP,Port},_} ->
	    inet_db:del_alt_ns(IP, Port);
	_ -> ok
    end,
    ns_end(NsSpec, ?config(priv_dir, Config)).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nameserver control

ns(Config) ->
    {_ZoneDir,NS,_P} = ?config(nameserver, Config),
    NS.

ns_init(ZoneDir, PrivDir, DataDir) ->
    case os:type() of
	{unix,_} when ZoneDir =:= undefined -> undefined;
	{unix,_} ->
	    {ok,S} = gen_udp:open(0, [{reuseaddr,true}]),
	    {ok,PortNum} = inet:port(S),
	    gen_udp:close(S),
	    RunNamed = filename:join(DataDir, ?RUN_NAMED),
	    NS = {{127,0,0,1},PortNum},
	    P = erlang:open_port({spawn_executable,RunNamed},
				 [{cd,PrivDir},
				  {line,80},
				  {args,["127.0.0.1",
					 integer_to_list(PortNum),
					 atom_to_list(ZoneDir)]},
				  stderr_to_stdout,
				  eof]),
	    ns_start(ZoneDir, NS, P);
	_ ->
	    throw("Only run on Unix")
    end.

ns_start(ZoneDir, NS, P) ->
    case ns_collect(P) of
	eof ->
	    erlang:error(eof);
	"Running: "++_ ->
	    {ZoneDir,NS,P};
	"Error: "++Error ->
	    throw(Error);
	_ ->
	    ns_start(ZoneDir, NS, P)
    end.

ns_end(undefined, _PrivDir) -> undefined;
ns_end({ZoneDir,_NS,P}, PrivDir) ->
    port_command(P, ["quit",io_lib:nl()]),
    ns_stop(P),
    ns_printlog(filename:join([PrivDir,ZoneDir,"named.log"])),
    ok.

ns_stop(P) ->
    case ns_collect(P) of
	eof ->
	    erlang:port_close(P);
	_ ->
	    ns_stop(P)
    end.

ns_collect(P) ->
    ns_collect(P, []).
ns_collect(P, Buf) ->
    receive
	{P,{data,{eol,L}}} ->
	    Line = lists:flatten(lists:reverse(Buf, [L])),
	    io:format("~s", [Line]),
	    Line;
	{P,{data,{noeol,L}}} ->
	    ns_collect(P, [L|Buf]);
	{P,eof} ->
	    eof
    end.

ns_printlog(Fname) ->
    io:format("Name server log file contents:~n", []),
    case file:read_file(Fname) of
	{ok,Bin} ->
	    io:format("~s~n", [Bin]);
	_ ->
	    ok
    end.

%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic(doc) ->
    ["Lookup an A record with different API functions"];
basic(Config) when is_list(Config) ->
    NS = ns(Config),
    Name = "ns.otptest",
    IP = {127,0,0,254},
    %%
    %% nslookup
    {ok,Msg1} = inet_res:nslookup(Name, in, a, [NS]),
    io:format("~p~n", [Msg1]),
    [RR1] = inet_dns:msg(Msg1, anlist),
    IP = inet_dns:rr(RR1, data),
    Bin1 = inet_dns:encode(Msg1),
    %%io:format("Bin1 = ~w~n", [Bin1]),
    {ok,Msg1} = inet_dns:decode(Bin1),
    %%
    %% resolve
    {ok,Msg2} = inet_res:resolve(Name, in, a, [{nameservers,[NS]}]),
    io:format("~p~n", [Msg2]),
    [RR2] = inet_dns:msg(Msg2, anlist),
    IP = inet_dns:rr(RR2, data),
    Bin2 = inet_dns:encode(Msg2),
    %%io:format("Bin2 = ~w~n", [Bin2]),
    {ok,Msg2} = inet_dns:decode(Bin2),
    %%
    %% lookup
    [IP] = inet_res:lookup(Name, in, a, [{nameservers,[NS]}]),
    %%
    %% gethostbyname
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:gethostbyname(Name),
    %%
    %% getbyname
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:getbyname(Name, a),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve(doc) ->
    ["Lookup different records using resolve/2..4"];
resolve(Config) when is_list(Config) ->
    NS = ns(Config),
    Domain = "otptest",
    RDomain4 = "0.0.127.in-addr.arpa",
    RDomain6 = "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.ip6.arpa",
    Name = "resolve."++Domain,
    L = [{in,a,Name,[{127,0,0,28}],undefined},
	 {in,aaaa,Name,[{0,0,0,0,0,0,32512,28}],undefined},
	 {in,cname,"cname."++Name,[Name],undefined},
	 {in,a,"cname."++Name,[Name,{127,0,0,28}],undefined},
	 {in,ns,"ns."++Name,[],[Name]},
	 {in,soa,Domain,[],[{"ns.otptest","lsa.otptest",1,60,10,300,30}]},
	 %% WKS: protocol TCP (6), services (bits) TELNET (23) and SMTP (25)
	 {in,wks,"wks."++Name,[{{127,0,0,28},6,<<0,0,1,64>>}],undefined},
	 {in,ptr,"28."++RDomain4,[Name],undefined},
	 {in,ptr,"c.1.0.0.0.0.f.7."++RDomain6,[Name],undefined},
	 {in,hinfo,Name,[{"BEAM","Erlang/OTP"}],undefined},
	 {in,mx,RDomain4,[{10,"mx."++Domain}],undefined},
	 {in,srv,"_srv._tcp."++Name,[{10,3,4711,Name}],undefined},
	 {in,naptr,"naptr."++Name,
	  [{10,5,"s","http","","_srv._tcp."++Name}],undefined},
	 {in,txt,"txt."++Name,
	  [["Hej ","du ","glade "],["ta ","en ","spade!"]],undefined},
	 {in,mb,"mb."++Name,["mx."++Name],undefined},
	 {in,mg,"mg."++Name,["lsa."++Domain],undefined},
	 {in,mr,"mr."++Name,["lsa."++Domain],undefined},
	 {in,minfo,"minfo."++Name,
	  [{"minfo-owner."++Name,"minfo-bounce."++Name}],undefined},
	 {in,any,"cname."++Name,[Name],undefined},
	 {in,any,Name,[{127,0,0,28},
		       {0,0,0,0,0,0,32512,28},
		       {"BEAM","Erlang/OTP"}],undefined}
	],
    resolve([{edns,false},{nameservers,[NS]}], L),
    resolve([{edns,0},{nameservers,[NS]}], L).

resolve(_Opts, []) -> ok;
resolve(Opts, [{Class,Type,Name,Answers,Authority}=Q|Qs]) ->
    io:format("Query: ~p~nOptions: ~p~n", [Q,Opts]),
    {ok,Msg} = inet_res:resolve(Name, Class, Type, Opts),
    AnList =
	if
	    Answers =/= undefined ->
		lists:sort(Answers);
	    true ->
		undefined
	end,
    NsList =
	if
	    Authority =/= undefined ->
		lists:sort(Authority);
	    true ->
		undefined
	end,
    case {lists:sort
	  ([inet_dns:rr(RR, data) || RR <- inet_dns:msg(Msg, anlist)]),
	  lists:sort
	  ([inet_dns:rr(RR, data) || RR <- inet_dns:msg(Msg, nslist)])} of
	{AnList,NsList} ->
	    ok;
	{NsList,AnList} when Type =:= ns ->
	    %% This whole case statement is kind of inside out just
	    %% to accept this case when some legacy DNS resolvers
	    %% return the answer to a NS query in the answer section
	    %% instead of in the authority section.
	    ok;
	{AnList,_} when NsList =:= undefined ->
	    ok;
	{_,NsList} when AnList =:= undefined ->
	    ok
    end,
    Buf = inet_dns:encode(Msg),
    {ok,Msg} = inet_dns:decode(Buf),
    resolve(Opts, Qs).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

edns0(doc) ->
    ["Test EDNS and truncation"];
edns0(Config) when is_list(Config) ->
    NS = ns(Config),
    Domain = "otptest",
    Filler = "-5678901234567890123456789012345678.",
    MXs = lists:sort([{10,"mx."++Domain},
		      {20,"mx1"++Filler++Domain},
		      {20,"mx2"++Filler++Domain},
		      {20,"mx3"++Filler++Domain},
		      {20,"mx4"++Filler++Domain},
		      {20,"mx5"++Filler++Domain},
		      {20,"mx6"++Filler++Domain},
		      {20,"mx7"++Filler++Domain}]),
    false = inet_db:res_option(edns), % ASSERT
    true = inet_db:res_option(udp_payload_size) >= 1280, % ASSERT
    %% These will fall back to TCP
    MXs = lists:sort(inet_res:lookup(Domain, in, mx, [{nameservers,[NS]}])),
    %%
    {ok,#hostent{h_addr_list=As}} = inet_res:getbyname(Domain++".", mx),
    MXs = lists:sort(As),
    %%
    {ok,Msg1} = inet_res:resolve(Domain, in, mx),
    MXs = lists:sort(inet_res_filter(inet_dns:msg(Msg1, anlist), in, mx)),
    %% There should be no OPT record in the answer
    [] = [RR || RR <- inet_dns:msg(Msg1, arlist),
		inet_dns:rr(RR, type) =:= opt],
    Buf1 = inet_dns:encode(Msg1),
    {ok,Msg1} = inet_dns:decode(Buf1),
    %%
    %% Use EDNS - should not need to fall back to TCP
    %% there is no way to tell from the outside.
    %%
    {ok,Msg2} = inet_res:resolve(Domain, in, mx, [{edns,0}]),
    MXs = lists:sort(inet_res_filter(inet_dns:msg(Msg2, anlist), in, mx)),
    Buf2 = inet_dns:encode(Msg2),
    {ok,Msg2} = inet_dns:decode(Buf2),
    case [RR || RR <- inet_dns:msg(Msg2, arlist),
		inet_dns:rr(RR, type) =:= opt] of
	[OptRR] ->
	    io:format("~p~n", [inet_dns:rr(OptRR)]),
	    ok;
	[] ->
	    case os:type() of
		{unix,sunos} ->
		    case os:version() of
			{M,V,_} when M < 5;  M == 5, V =< 8 ->
			    %% In our test park only known platform
			    %% with an DNS resolver that can not do
			    %% EDNS0.
			    {comment,"No EDNS0"}
		    end
	    end
    end.

inet_res_filter(Anlist, Class, Type) ->
    [inet_dns:rr(RR, data) || RR <- Anlist,
			      inet_dns:rr(RR, type) =:= Type,
			      inet_dns:rr(RR, class) =:= Class].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

txt_record(suite) ->
    [];
txt_record(doc) ->
    ["Tests TXT records"];
txt_record(Config) when is_list(Config) ->
    D1 = "cslab.ericsson.net",
    D2 = "mail1.cslab.ericsson.net",
    {ok,#dns_rec{anlist=[RR1]}} = 
	inet_res:nslookup(D1, in, txt),
    io:format("~p~n", [RR1]),
    {ok,#dns_rec{anlist=[RR2]}} =
	inet_res:nslookup(D2, in, txt),
    io:format("~p~n", [RR2]),
    #dns_rr{domain=D1, class=in, type=txt, data=A1} = RR1,
    #dns_rr{domain=D2, class=in, type=txt, data=A2} = RR2,
    case [lists:flatten(A2)] of
	A1 = [[_|_]] -> ok
    end,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files_monitor(suite) ->
    [];
files_monitor(doc) ->
    ["Tests monitoring of /etc/hosts and /etc/resolv.conf, but not them"];
files_monitor(Config) when is_list(Config) ->
    Search = inet_db:res_option(search),
    HostsFile = inet_db:res_option(hosts_file),
    ResolvConf = inet_db:res_option(resolv_conf),
    Inet6 = inet_db:res_option(inet6),
    try do_files_monitor(Config)
    after
        inet_db:res_option(search, Search),
        inet_db:res_option(resolv_conf, ResolvConf),
	inet_db:res_option(hosts_file, HostsFile),
	inet_db:res_option(inet6, Inet6)
    end.

do_files_monitor(Config) ->
    Dir = ?config(priv_dir, Config),
    {ok,Hostname} = inet:gethostname(),
    FQDN =
	case inet_db:res_option(domain) of
	    "" ->
		Hostname;
	    _ ->
		Hostname++"."++inet_db:res_option(domain)
	end,
    HostsFile = filename:join(Dir, "files_monitor_hosts"),
    ResolvConf = filename:join(Dir, "files_monitor_resolv.conf"),
    ok = inet_db:res_option(resolv_conf, ResolvConf),
    ok = inet_db:res_option(hosts_file, HostsFile),
    [] = inet_db:res_option(search),
    {ok,#hostent{h_name = Hostname,
		 h_addrtype = inet,
		 h_length = 4,
		 h_addr_list = [{127,0,0,1}]}} = inet:gethostbyname(Hostname),
    {ok,#hostent{h_name = FQDN,
		 h_addrtype = inet,
		 h_length = 4,
		 h_addr_list = [{127,0,0,1}]}} = inet:gethostbyname(FQDN),
    {error,nxdomain} = inet_res:gethostbyname(Hostname),
    {error,nxdomain} = inet_res:gethostbyname(FQDN),
    {ok,{127,0,0,10}} = inet:getaddr("mx.otptest", inet),
    {ok,{0,0,0,0,0,0,32512,28}} = inet:getaddr("resolve.otptest", inet6),
    {ok,#hostent{h_name = Hostname,
		 h_addrtype = inet6,
		 h_length = 16,
		 h_addr_list = [{0,0,0,0,0,0,0,1}]}} =
	inet:gethostbyname(Hostname, inet6),
    {ok,#hostent{h_name = FQDN,
		 h_addrtype = inet6,
		 h_length = 16,
		 h_addr_list = [{0,0,0,0,0,0,0,1}]}} =
	inet:gethostbyname(FQDN, inet6),
    {error,nxdomain} = inet_res:gethostbyname("resolve"),
    %% XXX inet does not honour res_option inet6, might be a problem?
    %% therefore inet_res is called here
    ok = inet_db:res_option(inet6, true),
    {ok,#hostent{h_name = "resolve.otptest",
		 h_addrtype = inet6,
		 h_length = 16,
		 h_addr_list = [{0,0,0,0,0,0,32512,28}]}} =
	inet_res:gethostbyname("resolve.otptest"),
    {error,nxdomain} = inet_hosts:gethostbyname("files_monitor"),
    ok = file:write_file(ResolvConf, "search otptest\n"),
    ok = file:write_file(HostsFile, "::100 files_monitor\n"),
    receive after 7000 -> ok end, % RES_FILE_UPDATE_TM in inet_res.hrl is 5 s
    {ok,#hostent{h_name = "resolve.otptest",
		 h_addrtype = inet6,
		 h_length = 16,
		 h_addr_list = [{0,0,0,0,0,0,32512,28}]}} =
	inet_res:gethostbyname("resolve.otptest"),
    ["otptest"] = inet_db:res_option(search),
    {ok,#hostent{h_name = "files_monitor",
		 h_addrtype = inet6,
		 h_length = 16,
		 h_addr_list = [{0,0,0,0,0,0,0,256}]}} =
	inet_hosts:gethostbyname("files_monitor"),
    ok = inet_db:res_option(inet6, false),
    {ok,#hostent{h_name = "resolve.otptest",
		 h_addrtype = inet,
		 h_length = 4,
		 h_addr_list = [{127,0,0,28}]}} =
	inet:gethostbyname("resolve.otptest"),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compatibility tests. Call the inet_SUITE tests, but with
%% lookup = [file,dns] instead of [native]

gethostbyaddr(Config) -> inet_SUITE:t_gethostbyaddr(Config).
gethostbyaddr_v6(Config) -> inet_SUITE:t_gethostbyaddr_v6(Config).
gethostbyname(Config) -> inet_SUITE:t_gethostbyname(Config).
gethostbyname_v6(Config) -> inet_SUITE:t_gethostbyname_v6(Config).
getaddr(Config) -> inet_SUITE:t_getaddr(Config).
getaddr_v6(Config) -> inet_SUITE:t_getaddr_v6(Config).
ipv4_to_ipv6(Config) -> inet_SUITE:ipv4_to_ipv6(Config).
host_and_addr(Config) -> inet_SUITE:host_and_addr(Config).
