%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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
-module(inet_res_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export([basic/1, resolve/1, edns0/1, txt_record/1, files_monitor/1,
	 last_ms_answer/1]).
-export([
	 gethostbyaddr/0, gethostbyaddr/1,
	 gethostbyaddr_v6/0, gethostbyaddr_v6/1,
	 gethostbyname/0, gethostbyname/1,
	 gethostbyname_v6/0, gethostbyname_v6/1,
	 getaddr/0, getaddr/1,
	 getaddr_v6/0, getaddr_v6/1,
	 ipv4_to_ipv6/0, ipv4_to_ipv6/1,
	 host_and_addr/0, host_and_addr/1
	]).

-define(RUN_NAMED, "run-named").

%% This test suite use a script ?RUN_NAMED that tries to start
%% a temporary local nameserver BIND 8 or 9 that must be installed
%% on your machine.
%%
%% For example, on Ubuntu 16.04 / 18.04, as root:
%%     apt-get install bind9
%% Now, that is not enough since Apparmor will not allow
%% the nameserver daemon /usr/sbin/named to read from the test directory.
%% Assuming that you run tests in /ldisk/daily_build, and still on
%% Ubuntu 14.04, make /etc/apparmor.d/local/usr.sbin.named contain:
%%     /ldisk/daily_build/** r,
%% And yes; the trailing comma must be there...
%% And yes; create the file if it does not exist.
%% And yes; restart the apparmor daemon using "service apparmor restart"


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [basic, resolve, edns0, txt_record, files_monitor,
     last_ms_answer,
     gethostbyaddr, gethostbyaddr_v6, gethostbyname,
     gethostbyname_v6, getaddr, getaddr_v6, ipv4_to_ipv6,
     host_and_addr].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

zone_dir(TC) ->
    case TC of
	basic -> otptest;
	resolve -> otptest;
	edns0 -> otptest;
	files_monitor -> otptest;
	last_ms_answer -> otptest;
	_ -> undefined
    end.

init_per_testcase(Func, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    try ns_init(zone_dir(Func), PrivDir, DataDir) of
	NsSpec ->
	    Lookup = inet_db:res_option(lookup),
	    inet_db:set_lookup([file,dns]),
	    case NsSpec of
		{_,{IP,Port},_} ->
		    inet_db:ins_alt_ns(IP, Port);
		_ -> ok
	    end,
	    [{nameserver,NsSpec},{res_lookup,Lookup}|Config]
    catch
	SkipReason ->
	    {skip,SkipReason}
    end.

end_per_testcase(_Func, Config) ->
    inet_db:set_lookup(proplists:get_value(res_lookup, Config)),
    NsSpec = proplists:get_value(nameserver, Config),
    case NsSpec of
	{_,{IP,Port},_} ->
	    inet_db:del_alt_ns(IP, Port);
	_ -> ok
    end,
    ns_end(NsSpec, proplists:get_value(priv_dir, Config)).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nameserver control

ns(Config) ->
    {_ZoneDir,NS,_P} = proplists:get_value(nameserver, Config),
    NS.

ns_init(ZoneDir, PrivDir, DataDir) ->
    case os:type() of
	{unix,_} when ZoneDir =:= undefined -> undefined;
	{unix,_} ->
	    PortNum = case {os:type(),os:version()} of
			  {{unix,solaris},{M,V,_}} when M =< 5, V < 10 ->
			      11895 + rand:uniform(100);
			  _ ->
			      {ok,S} = gen_udp:open(0, [{reuseaddr,true}]),
			      {ok,PNum} = inet:port(S),
			      gen_udp:close(S),
			      PNum
		      end,
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
	    ns_start(ZoneDir, PrivDir, NS, P);
	_ ->
	    throw("Only run on Unix")
    end.

ns_start(ZoneDir, PrivDir, NS, P) ->
    case ns_collect(P) of
	eof ->
	    erlang:error(eof);
	"Running: "++_ ->
	    {ZoneDir,NS,P};
	"Error: "++Error ->
	    ns_printlog(filename:join([PrivDir,ZoneDir,"named.log"])),
	    throw(Error);
	_ ->
	    ns_start(ZoneDir, PrivDir, NS, P)
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour modifying nameserver proxy

proxy_start(TC, {NS,P}) ->
    Tag = make_ref(),
    Parent = self(),
    Pid =
	spawn_link(
	  fun () ->
		  try proxy_start(TC, NS, P, Parent, Tag)
		  catch C:X:Stacktrace ->
			  io:format(
			    "~w: ~w:~p ~p~n",
			    [self(),C,X,Stacktrace])
		  end
	  end),
    receive {started,Tag,Port} ->
	    ProxyNS = {{127,0,0,1},Port},
	    {proxy,Pid,Tag,ProxyNS}
    end.

proxy_start(TC, NS, P, Parent, Tag) ->
    {ok,Outbound} = gen_udp:open(0, [binary]),
    ok = gen_udp:connect(Outbound, NS, P),
    {ok,Inbound} = gen_udp:open(0, [binary]),
    {ok,Port} = inet:port(Inbound),
    Parent ! {started,Tag,Port},
    proxy(TC, Outbound, NS, P, Inbound).


%% To provoke the last_ms_answer bug (OTP-9221) the proxy
%% * Relays the query to the right nameserver
%% * Intercepts the reply but holds it until the timer that
%%   was started when receiving the query fires.
%% * Repeats the reply with incorrect query ID a number of
%%   times with a short interval.
%% * Sends the correct reply, to give a correct test result
%%   after bug correction.
%%
%% The repetition of an incorrect answer with tight interval will keep
%% inet_res in an inner loop in the code that decrements the remaining
%% time until it hits 0 which triggers a crash, if the outer timeout
%% parameter to inet_res:resolve is so short that it runs out during
%% these repetitions.
proxy(last_ms_answer, Outbound, NS, P, Inbound) ->
    receive
	{udp,Inbound,SrcIP,SrcPort,Data} ->
	    Time =
		inet_db:res_option(timeout) div inet_db:res_option(retry),
	    Tag = erlang:make_ref(),
	    erlang:send_after(Time - 10, self(), {time,Tag}),
	    ok = gen_udp:send(Outbound, NS, P, Data),
	    receive
		{udp,Outbound,NS,P,Reply} ->
		    {ok,Msg} = inet_dns:decode(Reply),
		    Hdr = inet_dns:msg(Msg, header),
		    Id = inet_dns:header(Hdr, id),
		    BadHdr =
			inet_dns:make_header(Hdr, id, (Id+1) band 16#ffff),
		    BadMsg = inet_dns:make_msg(Msg, header, BadHdr),
		    BadReply = inet_dns:encode(BadMsg),
		    receive
			{time,Tag} ->
			    proxy__last_ms_answer(
			      Inbound, SrcIP, SrcPort, BadReply, Reply, 30)
		    end
	    end
    end.

proxy__last_ms_answer(Socket, IP, Port, _, Reply, 0) ->
    ok = gen_udp:send(Socket, IP, Port, Reply);
proxy__last_ms_answer(Socket, IP, Port, BadReply, Reply, N) ->
    ok = gen_udp:send(Socket, IP, Port, BadReply),
    receive after 1 -> ok end,
    proxy__last_ms_answer(Socket, IP, Port, BadReply, Reply, N-1).

proxy_wait({proxy,Pid,_,_}) ->
    Mref = erlang:monitor(process, Pid),
    receive {'DOWN',Mref,_,_,_} -> ok end.

proxy_ns({proxy,_,_,ProxyNS}) -> ProxyNS.

%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Lookup an A record with different API functions.
basic(Config) when is_list(Config) ->
    NS = ns(Config),
    Name = "ns.otptest",
    NameC = caseflip(Name),
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
    %% Now with scrambled case
    {ok,Msg1b} = inet_res:nslookup(NameC, in, a, [NS]),
    io:format("~p~n", [Msg1b]),
    [RR1b] = inet_dns:msg(Msg1b, anlist),
    IP = inet_dns:rr(RR1b, data),
    Bin1b = inet_dns:encode(Msg1b),
    %%io:format("Bin1b = ~w~n", [Bin1b]),
    {ok,Msg1b} = inet_dns:decode(Bin1b),
    true =
	(tolower(inet_dns:rr(RR1, domain))
	 =:= tolower(inet_dns:rr(RR1b, domain))),
    %%
    %% resolve
    {ok,Msg2} = inet_res:resolve(Name, in, a, [{nameservers,[NS]},verbose]),
    io:format("~p~n", [Msg2]),
    [RR2] = inet_dns:msg(Msg2, anlist),
    IP = inet_dns:rr(RR2, data),
    Bin2 = inet_dns:encode(Msg2),
    %%io:format("Bin2 = ~w~n", [Bin2]),
    {ok,Msg2} = inet_dns:decode(Bin2),
    %% Now with scrambled case
    {ok,Msg2b} = inet_res:resolve(NameC, in, a, [{nameservers,[NS]},verbose]),
    io:format("~p~n", [Msg2b]),
    [RR2b] = inet_dns:msg(Msg2b, anlist),
    IP = inet_dns:rr(RR2b, data),
    Bin2b = inet_dns:encode(Msg2b),
    %%io:format("Bin2b = ~w~n", [Bin2b]),
    {ok,Msg2b} = inet_dns:decode(Bin2b),
    true =
	(tolower(inet_dns:rr(RR2, domain))
	  =:= tolower(inet_dns:rr(RR2b, domain))),
    %%
    %% lookup
    [IP] = inet_res:lookup(Name, in, a, [{nameservers,[NS]},verbose]),
    [IP] = inet_res:lookup(NameC, in, a, [{nameservers,[NS]},verbose]),
    %%
    %% gethostbyname
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:gethostbyname(Name),
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:gethostbyname(NameC),
    %%
    %% getbyname
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:getbyname(Name, a),
    {ok,#hostent{h_addr_list=[IP]}} = inet_res:getbyname(NameC, a),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Lookup different records using resolve/2..4.
resolve(Config) when is_list(Config) ->
    Class = in,
    NS = ns(Config),
    Domain = "otptest",
    RDomain4 = "0.0.127.in-addr.arpa",
    RDomain6 = "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.ip6.arpa",
    Name = "resolve."++Domain,
    L = [{a,Name,[{a,{127,0,0,28}}],undefined},
	 {aaaa,Name,[{aaaa,{0,0,0,0,0,0,32512,28}}],undefined},
	 {cname,"cname."++Name,[{cname,Name}],undefined},
	 {a,"cname."++Name,[{cname,Name},{a,{127,0,0,28}}],undefined},
	 {ns,"ns."++Name,[],[{ns,Name}]},
	 {soa,Domain,[],[{soa,{"ns.otptest","lsa.otptest",1,60,10,300,30}}]},
	 %% WKS: protocol TCP (6), services (bits) TELNET (23) and SMTP (25)
	 {wks,"wks."++Name,[{wks,{{127,0,0,28},6,<<0,0,1,64>>}}],undefined},
	 {ptr,"28."++RDomain4,[{ptr,Name}],undefined},
	 {ptr,"c.1.0.0.0.0.f.7."++RDomain6,[{ptr,Name}],undefined},
	 {hinfo,Name,[{hinfo,{"BEAM","Erlang/OTP"}}],undefined},
	 {mx,RDomain4,[{mx,{10,"mx."++Domain}}],undefined},
	 {srv,"_srv._tcp."++Name,[{srv,{10,3,4711,Name}}],undefined},
	 {naptr,"naptr."++Name,
	  [{naptr,{10,5,"s","http","","_srv._tcp."++Name}}],
	  undefined},
	 {txt,"txt."++Name,
	  [{txt,["Hej ","du ","glade "]},{txt,["ta ","en ","spade!"]}],
	  undefined},
	 {mb,"mb."++Name,[{mb,"mx."++Name}],undefined},
	 {mg,"mg."++Name,[{mg,"Lsa."++Domain}],undefined},
	 {mr,"mr."++Name,[{mr,"LSA."++Domain}],undefined},
	 {minfo,"minfo."++Name,
	  [{minfo,{"minfo-OWNER."++Name,"MinfoBounce."++Name}}],
	  undefined},
	 {any,"cname."++Name,[{cname,Name}],undefined},
	 {any,Name,
	  [{a,{127,0,0,28}},
	   {aaaa,{0,0,0,0,0,0,32512,28}},
	   {hinfo,{"BEAM","Erlang/OTP"}}],
	  undefined}
	],
    resolve(Class, [{edns,0},{nameservers,[NS]}], L),
    resolve(Class, [{edns,false},{nameservers,[NS]}], L),
    %% Again, to see ensure the cache does not mess things up
    resolve(Class, [{edns,0},{nameservers,[NS]}], L),
    resolve(Class, [{edns,false},{nameservers,[NS]}], L).

resolve(_Class, _Opts, []) ->
    ok;
resolve(Class, Opts, [{Type,Nm,Answers,Authority}=Q|Qs]) ->
    io:format("Query: ~p~nOptions: ~p~n", [Q,Opts]),
    {Name,NameC} =
	case erlang:phash2(Q) band 4 of
	    0 ->
		{Nm,caseflip(Nm)};
	    _ ->
		{caseflip(Nm),Nm}
	end,
    AnList =
	if
	    Answers =/= undefined ->
		normalize_answers(Answers);
	    true ->
		undefined
	end,
    NsList =
	if
	    Authority =/= undefined ->
		normalize_answers(Authority);
	    true ->
		undefined
	end,
    {ok,Msg} = inet_res:resolve(Name, Class, Type, Opts),
    check_msg(Class, Type, Msg, AnList, NsList),
    {ok,MsgC} = inet_res:resolve(NameC, Class, Type, Opts),
    check_msg(Class, Type, MsgC, AnList, NsList),
    resolve(Class, Opts, Qs).



normalize_answers(AnList) ->
    lists:sort([normalize_answer(Answer) || Answer <- AnList]).

normalize_answer({soa,{NS,HM,Ser,Ref,Ret,Exp,Min}}) ->
    {tolower(NS),tolower_email(HM),Ser,Ref,Ret,Exp,Min};
normalize_answer({mx,{Prio,DN}}) ->
    {Prio,tolower(DN)};
normalize_answer({srv,{Prio,Weight,Port,DN}}) ->
    {Prio,Weight,Port,tolower(DN)};
normalize_answer({naptr,{Order,Pref,Flags,Service,RE,Repl}}) ->
    {Order,Pref,Flags,Service,RE,tolower(Repl)};
normalize_answer({minfo,{RespM,ErrM}}) ->
    {tolower_email(RespM),tolower_email(ErrM)};
normalize_answer({T,MN}) when T =:= mg; T =:= mr ->
    tolower_email(MN);
normalize_answer({T,DN}) when T =:= cname; T =:= ns; T =:= ptr; T =:= mb ->
    tolower(DN);
normalize_answer(Answer) ->
    Answer.

check_msg(Class, Type, Msg, AnList, NsList) ->
    io:format("check_msg Type: ~p, Msg: ~p~n.", [Type,Msg]),
    case {normalize_answers(
	    [begin
		 Class = inet_dns:rr(RR, class),
		 {inet_dns:rr(RR, type),inet_dns:rr(RR, data)}
	     end || RR <- inet_dns:msg(Msg, anlist)]),
	  normalize_answers(
	    [begin
		 Class = inet_dns:rr(RR, class),
		 {inet_dns:rr(RR, type),inet_dns:rr(RR, data)}
	     end || RR <- inet_dns:msg(Msg, nslist)])} of
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
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test EDNS and truncation.
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
    MXs = lists:sort(inet_res:lookup(Domain, in, mx, [{nameservers,[NS]},verbose])),
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
			    %% with an DNS resolver that cannot do
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

%% Tests TXT records.
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

%% Tests monitoring of /etc/hosts and /etc/resolv.conf, but not them.
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
    Dir = proplists:get_value(priv_dir, Config),
    {ok,Hostname} = inet:gethostname(),
    io:format("Hostname = ~p.~n", [Hostname]),
    FQDN =
	case inet_db:res_option(domain) of
	    "" ->
		Hostname;
	    _ ->
		Hostname++"."++inet_db:res_option(domain)
	end,
    io:format("FQDN = ~p.~n", [FQDN]),
    HostsFile = filename:join(Dir, "files_monitor_hosts"),
    ResolvConf = filename:join(Dir, "files_monitor_resolv.conf"),
    ok = inet_db:res_option(resolv_conf, ResolvConf),
    ok = inet_db:res_option(hosts_file, HostsFile),
    [] = inet_db:res_option(search),
    %% The inet function will use its final fallback to find this host
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
    %% The inet function will use its final fallback to find this host
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

%% Answer just when timeout is triggered (OTP-9221).
last_ms_answer(Config) when is_list(Config) ->
    NS = ns(Config),
    Name = "ns.otptest",
    %%IP = {127,0,0,254},
    Time = inet_db:res_option(timeout) div inet_db:res_option(retry),
    PSpec = proxy_start(last_ms_answer, NS),
    ProxyNS = proxy_ns(PSpec),
    %%
    %% resolve; whith short timeout to trigger Timeout =:= 0 in inet_res
    {error,timeout} =
	inet_res:resolve(
	  Name, in, a, [{nameservers,[ProxyNS]},verbose], Time + 10),
    %%
    proxy_wait(PSpec),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Compatibility tests. Call the inet_SUITE tests, but with
%% lookup = [file,dns] instead of [native]

gethostbyaddr() -> inet_SUITE:t_gethostbyaddr().
gethostbyaddr(Config) -> inet_SUITE:t_gethostbyaddr(Config).
gethostbyaddr_v6() -> inet_SUITE:t_gethostbyaddr_v6().
gethostbyaddr_v6(Config) -> inet_SUITE:t_gethostbyaddr_v6(Config).
gethostbyname() -> inet_SUITE:t_gethostbyname().
gethostbyname(Config) -> inet_SUITE:t_gethostbyname(Config).
gethostbyname_v6() -> inet_SUITE:t_gethostbyname_v6().
gethostbyname_v6(Config) -> inet_SUITE:t_gethostbyname_v6(Config).
getaddr() -> inet_SUITE:t_getaddr().
getaddr(Config) -> inet_SUITE:t_getaddr(Config).
getaddr_v6() -> inet_SUITE:t_getaddr_v6().
getaddr_v6(Config) -> inet_SUITE:t_getaddr_v6(Config).
ipv4_to_ipv6() -> inet_SUITE:ipv4_to_ipv6().
ipv4_to_ipv6(Config) -> inet_SUITE:ipv4_to_ipv6(Config).
host_and_addr() -> inet_SUITE:host_and_addr().
host_and_addr(Config) -> inet_SUITE:host_and_addr(Config).



%% Case flip helper

caseflip([C|Cs]) when is_integer(C), $a =< C, C =< $z ->
    [(C - $a + $A)|caseflip_skip(Cs)];
caseflip([C|Cs]) when is_integer(C), $A =< C, C =< $Z ->
    [(C - $A + $a)|caseflip_skip(Cs)];
caseflip([C|Cs]) ->
    [C|caseflip(Cs)];
caseflip([]) ->
    [].

caseflip_skip([C|Cs]) when is_integer(C), $a =< C, C =< $z ->
    [C|caseflip(Cs)];
caseflip_skip([C|Cs]) when is_integer(C), $A =< C, C =< $Z ->
    [C|caseflip(Cs)];
caseflip_skip([C|Cs]) ->
    [C|caseflip_skip(Cs)];
caseflip_skip([]) ->
    [].

tolower_email([$.|Cs]) ->
    [$.|tolower(Cs)];
tolower_email([C|Cs]) ->
    [C|tolower_email(Cs)].

%% Case fold to lower case according to RFC 4343
%%
tolower([C|Cs]) when is_integer(C) ->
    if  $A =< C, C =< $Z ->
	    [(C - $A + $a)|tolower(Cs)];
	true ->
	    [C|tolower(Cs)]
    end;
tolower([]) ->
    [].
