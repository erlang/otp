%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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

-include("kernel_test_lib.hrl").


-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2
        ]).
-export([basic/1, name_addr_and_cached/1, resolve/1,
         edns0/1, edns0_multi_formerr/1, txt_record/1, files_monitor/1,
	 nxdomain_reply/1, last_ms_answer/1, intermediate_error/1,
         servfail_retry_timeout_default/1, servfail_retry_timeout_1000/1,
         label_compression_limit/1, update/1, tsig_client/1, tsig_server/1,
         mdns_encode_decode/1
        ]).
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

-define(RUN_NS, "run-ns").
-define(LOG_FILE, "ns.log").

%% This test suite use a script ?RUN_NS that tries to start
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
     {timetrap,{seconds,15}}].

all() -> 
    [basic, resolve, name_addr_and_cached,
     edns0, edns0_multi_formerr, txt_record, files_monitor,
     nxdomain_reply, last_ms_answer,
     intermediate_error,
     servfail_retry_timeout_default, servfail_retry_timeout_1000,
     label_compression_limit, update, tsig_client, tsig_server,
     mdns_encode_decode,
     gethostbyaddr, gethostbyaddr_v6, gethostbyname,
     gethostbyname_v6, getaddr, getaddr_v6, ipv4_to_ipv6,
     host_and_addr].

groups() -> 
    [].

init_per_suite(Config0) ->

    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
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

    Config1. %% We don't actually need to update or return config


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

zone_dir(TC) ->
    case TC of
	basic                -> otptest;
	name_addr_and_cached -> otptest;
	resolve              -> otptest;
	edns0                -> otptest;
	edns0_multi_formerr  -> otptest;
	files_monitor        -> otptest;
	nxdomain_reply       -> otptest;
	last_ms_answer       -> otptest;
	update               -> otptest;
	tsig_client          -> otptest;
        intermediate_error   ->
            {internal,
             #{rcode => ?REFUSED}};
        servfail_retry_timeout_default ->
            {internal,
             #{rcode => ?SERVFAIL, etd => 1500}};
        servfail_retry_timeout_1000 ->
            {internal,
             #{rcode => ?SERVFAIL, etd => 1000}};
	_ -> undefined
    end.

init_per_testcase(Func, Config) ->

    ?P("init_per_testcase -> entry with"
       "~n      Func:   ~p"
       "~n      Config: ~p", [Func, Config]),

    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    try ns_init(zone_dir(Func), PrivDir, DataDir) of
	NsSpec ->
            ?P("init_per_testcase -> get resolver lookup"),
	    Lookup = inet_db:res_option(lookup),
            ?P("init_per_testcase -> set file:dns"),
	    inet_db:set_lookup([file,dns]),
	    case NsSpec of
		{_,{IP,Port},_} ->
                    ?P("init_per_testcase -> insert alt nameserver ~p:~w",
                       [IP, Port]),
		    inet_db:ins_alt_ns(IP, Port);
		_ -> ok
	    end,
            %% dbg:tracer(),
            %% dbg:p(all, c),
            %% dbg:tpl(inet_res, query_nss_res, cx),
            ?P("init_per_testcase -> done:"
               "~n    NsSpec: ~p"
               "~n    Lookup: ~p", [NsSpec, Lookup]),
	    [{nameserver, NsSpec}, {res_lookup, Lookup} | Config]
    catch
	SkipReason ->
            ?P("init_per_testcase -> skip: ~p", [SkipReason]),
	    {skip, SkipReason};
        Class : Reason : Stacktrace ->
            ?P("init_per_testcase -> ~w: ~p"
               "~n    ~p~n", [Class, Reason, Stacktrace]),
            {fail, Reason}
    end.

end_per_testcase(_Func, Config) ->
    inet_db:set_lookup(proplists:get_value(res_lookup, Config)),
    NsSpec = proplists:get_value(nameserver, Config),
    case NsSpec of
	{_,{IP,Port},_} ->
	    inet_db:del_alt_ns(IP, Port);
	_ -> ok
    end,
    %% dbg:stop(),
    ns_end(NsSpec, proplists:get_value(priv_dir, Config)).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Nameserver control

ns(Config) ->
    {_ZoneDir,NS,_P} = proplists:get_value(nameserver, Config),
    NS.

ns_init(ZoneDir, PrivDir, DataDir) ->

    ?P("ns_init -> entry with"
       "~n      ZoneDir: ~p"
       "~n      PrivDir: ~p"
       "~n      DataDir: ~p", [ZoneDir, PrivDir, DataDir]),

    case {os:type(),ZoneDir} of
        {_,{internal,ServerSpec}} ->
            ns_start_internal(ServerSpec);
	{{unix,_},undefined} ->
            ?P("ns_init -> nothing"),
            undefined;
	{{unix,_},otptest} ->
            ?P("ns_init -> prepare start"),
	    PortNum = case {os:type(),os:version()} of
			  {{unix,solaris},{M,V,_}} when M =< 5, V < 10 ->
			      11895 + rand:uniform(100);
			  _ ->
			      S = ok(gen_udp:open(0, [{reuseaddr,true}])),
			      PNum = ok(inet:port(S)),
			      gen_udp:close(S),
			      PNum
		      end,
            ?P("ns_init -> use port number ~p", [PortNum]),
	    RunNamed = filename:join(DataDir, ?RUN_NS),
            ?P("ns_init -> use named ~p", [RunNamed]),
	    NS = {{127,0,0,1},PortNum},
            ?P("ns_init -> try open port (exec)"),
	    P = erlang:open_port({spawn_executable,RunNamed},
				 [{cd,PrivDir},
				  {line,80},
				  {args,["127.0.0.1",
					 integer_to_list(PortNum),
					 atom_to_list(ZoneDir)]},
                                  {env,[{"LOGNAME",os:getenv("LOGNAME",os:getenv("USER"))}]},
				  stderr_to_stdout,
				  eof]),
            ?P("ns_init -> port ~p", [P]),
	    ns_start(ZoneDir, PrivDir, NS, P);
	_ ->
	    throw("Only run on Unix")
    end.

ns_start(ZoneDir, PrivDir, NS, P) ->

    ?P("ns_start -> await message"),

    case ns_collect(P) of
	eof ->
            ?P("ns_start -> eof"),
	    erlang:error(eof);
	"Running: "++_ ->
            ?P("ns_start -> running"),
	    {ZoneDir,NS,P};
	"Skip: "++Reason ->
            ?P("ns_start -> skip: "
               "~n      ~p", [Reason]),
	    ns_printlog(filename:join([PrivDir,ZoneDir,?LOG_FILE])),
	    throw(Reason);
	"Error: "++Error ->
            ?P("ns_start -> error: "
               "~n      ~p", [Error]),
	    ns_printlog(filename:join([PrivDir,ZoneDir,?LOG_FILE])),
	    error(Error);
	_X ->
            ?P("ns_start -> retry"),
	    ns_start(ZoneDir, PrivDir, NS, P)
    end.


ns_start_internal(ServerSpec) ->

    ?P("ns_start_internal -> entry with"
       "~n      ServerSpec: ~p", [ServerSpec]),

    Parent = self(),
    Tag = make_ref(),
    {P,Mref} =
        spawn_monitor(
          fun () ->
                  _ = process_flag(trap_exit, true),
                  IP = {127,0,0,1},
                  SocketOpts = [{ip,IP},binary,{active,once}],
                  S = ok(gen_udp:open(0, SocketOpts)),
                  Port = ok(inet:port(S)),
                  ParentMref = monitor(process, Parent),
                  Parent ! {Tag,{IP,Port},self()},
                  ns_internal(ServerSpec, ParentMref, Tag, S)
          end),
    receive
        {Tag,_NS,P} = NsSpec ->
            ?P("ns_start_internal -> ~p started", [P]),
            demonitor(Mref, [flush]),
            NsSpec;
        {'DOWN',Mref,_,_,Reason} ->
            ?P("ns_start_internal -> failed start:"
               "~n      ~p", [Reason]),
            exit({ns_start_internal,Reason})
    end.

ns_end(undefined, _PrivDir) -> undefined;
ns_end({ZoneDir,_NS,P}, PrivDir) when is_port(P) ->
    port_command(P, ["quit",io_lib:nl()]),
    ns_stop(P),
    ns_printlog(filename:join([PrivDir,ZoneDir,"ns.log"])),
    ok;
ns_end({Tag,_NS,P}, _PrivDir) when is_pid(P) ->
    Mref = erlang:monitor(process, P),
    P ! Tag,
    receive
        {'DOWN',Mref,_,_,Reason} ->
            Reason = normal,
            ok
    end.

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
	    ?P("collected: ~s", [Line]),
	    Line;
	{P,{data,{noeol,L}}} ->
	    ns_collect(P, [L|Buf]);
	{P,eof} ->
	    eof
    end.

ns_printlog(Fname) ->
    ?P("Name server log file contents:"),
    case file:read_file(Fname) of
	{ok,Bin} ->
	    io:format("~s~n", [Bin]);
	_ ->
	    ok
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal name server

ns_internal(ServerSpec, Mref, Tag, S) ->
    ?P("ns-internal -> await message"),
    receive
        {'DOWN',Mref,_,_,Reason} ->
            ?P("ns-internal -> received DOWN: "
               "~n      ~p", [Reason]),
            exit(Reason);
        Tag ->
            ?P("ns-internal -> received tag: done"),
            ok;
        {udp,S,IP,Port,Data} ->
            ?P("ns-internal -> received UDP message"),
            Req = ok(inet_dns:decode(Data)),
            {Resp, ServerSpec2} = ns_internal(ServerSpec, Req),
            RespData = inet_dns:encode(Resp),
            _ = ok(gen_udp:send(S, IP, Port, RespData)),
            _ = ok(inet:setopts(S, [{active,once}])),
            ns_internal(ServerSpec2, Mref, Tag, S)
    end.

ns_internal(#{rcode := Rcode,
              ts    := TS0,
              etd   := ETD} = ServerSpec, Req) ->
    ?P("ns-internal -> request received (time validation)"),
    TS1    = timestamp(),
    Hdr    = inet_dns:msg(Req, header),
    Opcode = inet_dns:header(Hdr, opcode),
    Id     = inet_dns:header(Hdr, id),
    Rd     = inet_dns:header(Hdr, rd),
    %%
    Qdlist = inet_dns:msg(Req, qdlist),
    ?P("ns-internal -> time validation: "
       "~n      ETD:       ~w"
       "~n      TS1 - TS0: ~w", [ETD, TS1 - TS0]),
    RC = if ((TS1 - TS0) >= ETD) ->
                 ?P("ns-internal -> time validated"),
                 ?NOERROR;
            true ->
                 ?P("ns-internal -> time validation failed"),
                 Rcode
         end,
    Resp   = inet_dns:make_msg(
               [{header,
                 inet_dns:make_header(
                   [{id,     Id},
                    {qr,     true},
                    {opcode, Opcode},
                    {aa,     true},
                    {tc,     false},
                    {rd,     Rd},
                    {ra,     false},
                    {rcode,  RC}])},
                {qdlist, Qdlist}]),
    {Resp, ServerSpec#{ts => timestamp()}};
ns_internal(#{rcode := Rcode} = ServerSpec, Req) ->
    ?P("ns-internal -> request received"),
    Hdr    = inet_dns:msg(Req, header),
    Opcode = inet_dns:header(Hdr, opcode),
    Id     = inet_dns:header(Hdr, id),
    Rd     = inet_dns:header(Hdr, rd),
    %%
    Qdlist = inet_dns:msg(Req, qdlist),
    Resp   = inet_dns:make_msg(
               [{header,
                 inet_dns:make_header(
                   [{id,Id},
                    {qr,true},
                    {opcode,Opcode},
                    {aa,true},
                    {tc,false},
                    {rd,Rd},
                    {ra,false},
                    {rcode,Rcode}])},
                {qdlist,Qdlist}]),
    {Resp, ServerSpec#{ts => timestamp()}}.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Behaviour modifying nameserver proxy

proxy_start(TC, {NS,P}) ->
    Tag = make_ref(),
    Parent = self(),
    Pid =
	spawn_link(
	  fun () ->
		  try proxy_start(TC, NS, P, Parent, Tag)
		  catch
                      C:X:Stacktrace ->
			  ?P("~p Failed starting proxy: "
                             "~n      Class:      ~w"
                             "~n      Error:      ~p"
                             "~n      Stacktrace: ~p",
                             [self(), C, X, Stacktrace])
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
    ?P("begin"),
    NS = ns(Config),
    Name = "ns.otptest",
    NameC = caseflip(Name),
    NameD = NameC ++ ".",
    IP1 = {127,0,0,253},
    IP2 = {127,0,0,254},
    %%
    %% nslookup
    {ok,Msg1} = inet_res:nslookup(Name, in, a, [NS]),
    ?P("nslookup with ~p: ~n      ~p", [Name, Msg1]),
    [RR1, RR2] = lists:sort(inet_dns:msg(Msg1, anlist)),
    IP1 = inet_dns:rr(RR1, data),
    IP2 = inet_dns:rr(RR2, data),
    Bin1 = inet_dns:encode(Msg1),
    %%io:format("Bin1 = ~w~n", [Bin1]),
    {ok,Msg1} = inet_dns:decode(Bin1),
    %% Now with scrambled case
    {ok,Msg1b} = inet_res:nslookup(NameC, in, a, [NS]),
    ?P("nslookup with ~p: ~n      ~p", [NameC, Msg1b]),
    [RR1b, RR2b] = lists:sort(inet_dns:msg(Msg1b, anlist)),
    IP1 = inet_dns:rr(RR1b, data),
    IP2 = inet_dns:rr(RR2b, data),
    Bin1b = inet_dns:encode(Msg1b),
    %%io:format("Bin1b = ~w~n", [Bin1b]),
    {ok,Msg1b} = inet_dns:decode(Bin1b),
    true =
	(tolower(inet_dns:rr(RR1, domain))
	 =:= tolower(inet_dns:rr(RR1b, domain))),
    true =
	(tolower(inet_dns:rr(RR2, domain))
	 =:= tolower(inet_dns:rr(RR2b, domain))),
    %%
    %% resolve
    {ok,Msg2} = inet_res:resolve(Name, in, a, [{nameservers,[NS]},verbose]),
    ?P("resolve with ~p: ~n      ~p", [Name, Msg2]),
    [RR1c, RR2c] = lists:sort(inet_dns:msg(Msg2, anlist)),
    IP1 = inet_dns:rr(RR1c, data),
    IP2 = inet_dns:rr(RR2c, data),
    Bin2 = inet_dns:encode(Msg2),
    %%io:format("Bin2 = ~w~n", [Bin2]),
    {ok,Msg2} = inet_dns:decode(Bin2),
    %% Now with scrambled case
    {ok,Msg2b} = inet_res:resolve(NameC, in, a, [{nameservers,[NS]},verbose]),
    ?P("resolve with ~p: ~n      ~p", [NameC, Msg2b]),
    [RR1d, RR2d] = lists:sort(inet_dns:msg(Msg2b, anlist)),
    IP1 = inet_dns:rr(RR1d, data),
    IP2 = inet_dns:rr(RR2d, data),
    Bin2b = inet_dns:encode(Msg2b),
    %%io:format("Bin2b = ~w~n", [Bin2b]),
    {ok,Msg2b} = inet_dns:decode(Bin2b),
    true =
	(tolower(inet_dns:rr(RR1c, domain))
	  =:= tolower(inet_dns:rr(RR1d, domain))),
    true =
	(tolower(inet_dns:rr(RR2c, domain))
	  =:= tolower(inet_dns:rr(RR2d, domain))),
    ?P("resolve \"127.0.0.1\"~n", []),
    {ok, Msg3} =
        inet_res:resolve("127.0.0.1", in, a, [{nameservers,[NS]},verbose]),
    [] = inet_dns:msg(Msg3, anlist),
    {ok, Msg4} =
        inet_res:resolve("127.0.0.1", in, ptr, [{nameservers,[NS]},verbose]),
    [RR4] = inet_dns:msg(Msg4, anlist),
    "1.0.0.127.in-addr.arpa" = inet_dns:rr(RR4, domain),
    "test1-78901234567890123456789012345678.otptest" =
        inet_dns:rr(RR4, data),
    %%
    %% lookup
    ?P("lookup"),
    [IP1, IP2] =
        lists:sort(
          inet_res:lookup(Name, in, a, [{nameservers,[NS]},verbose])),
    [IP1, IP2] =
        lists:sort(
          inet_res:lookup(NameC, in, a, [{nameservers,[NS]},verbose])),
    [IP1, IP2] =
        lists:sort(
          inet_res:lookup(NameD, in, a, [{nameservers,[NS]},verbose])),
    %%
    %% gethostbyname
    ?P("gethostbyname"),
    {ok,#hostent{h_addr_list=IPs1}} = inet_res:gethostbyname(Name),
    [IP1, IP2] = lists:sort(IPs1),
    {ok,#hostent{h_addr_list=IPs2}} = inet_res:gethostbyname(NameC),
    [IP1, IP2] = lists:sort(IPs2),
    %%
    %% getbyname
    ?P("getbyname"),
    {ok,#hostent{h_addr_list=IPs3}} = inet_res:getbyname(Name, a),
    [IP1, IP2] = lists:sort(IPs3),
    {ok,#hostent{h_addr_list=IPs4}} = inet_res:getbyname(NameC, a),
    [IP1, IP2] = lists:sort(IPs4),
    ?P("end"),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check that lookup of names and addresses works the same
%% also when cached, by simply repeating the lookups

name_addr_and_cached(Config) when is_list(Config) ->
    ?P("begin"),

%%%    dbg:tracer(),
%%%    dbg:p(all, c),
%%%    dbg:tpl(inet_res, do_query, cx),

    NS = ns(Config),
    Domain = "otptest",
    Options =
        [{lookup, [dns]},
         {resolv_conf, []},
         {hosts_file, []},
         {domain, Domain},
         {nameservers, [NS]},
         {search, [Domain]},
         {alt_nameservers, []},
         {inet6, false},
         {usevc, false},
         {edns, 0}],
    SavedOptions =
        [{Option, inet_db:res_option(Option)}
         || {Option, _Value} <- Options],
    [inet_db:res_option(Option, Value)
     || {Option, Value} <- Options],
    try
        ?P("first pass"),
        %% Flip character case randomly
        name_addr(Domain, fun caseflip/1),
        %%
        ?P("second pass"),
        %% Use only character upper case,
        %% should get identical results from the cache
        name_addr(Domain, fun toupper/1)
    after
        [ok = inet_db:res_option(Option, Value)
         || {Option, Value} <- SavedOptions]
    end,
    ?P("done"),
    ok.

name_addr(Domain, CFlip) ->
%%    RDomain4 = "0.0.127.in-addr.arpa",
%%    RDomain6 = "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.ip6.arpa",
    H4 =
        #hostent{
           h_addrtype = inet,
           h_length = 4},
    H6 =
        #hostent{
           h_addrtype = inet6,
           h_length = 16},
    H_mx              = CFlip("mx"),
    H_mx_             = CFlip("mx."),
    IP4_mx            = {127,0,0,10},
    %%
    H_resolve         = CFlip("resolve"),
    H_resolve_        = CFlip("resolve."),
    H_cname_resolve   = CFlip("cname.resolve"),
    H_cname_resolve_  = CFlip("cname.resolve."),
    %%
    IP4_resolve       = {127,0,0,28},
    IP6_resolve       = {0,0,0,0,0,0,127 bsl 8,28},
    IP64_resolve      = {0,0,0,0,0,16#ffff,127 bsl 8,28},
    %%
    H_ns              = CFlip("ns"),
    H_ns_             = CFlip("ns."),
    IP4_ns_1          = {127,0,0,253},
    IP4_ns_2          = {127,0,0,254},
    IP6_ns_1          = {0,0,0,0,0,0,127 bsl 8,253},
    IP6_ns_2          = {0,0,0,0,0,0,127 bsl 8,254},
    IP64_ns_1         = {0,0,0,0,0,16#ffff,127 bsl 8,253},
    IP64_ns_2         = {0,0,0,0,0,16#ffff,127 bsl 8,254},
    Lookups =
        %% The search list is [Domain] so a lookup of a short
        %% name should return the fully qualified name with
        %% Domain appended.
        %%
        %% Lookup results should have the same character case
        %% as the query.  Both for short (search) lookups and
        %% for full name lookups.
        %%
        %% Lookup via a CNAME record should return the
        %% result as stored in DNS but the CNAME should be
        %% returned as an alias with the query character case
        %% preserved.
        %%
        %% Address lookups (reverse/PTR lookups) can only
        %% return the one address we looked up and the
        %% fully qualified name as in DNS, and no aliases.
        %%
        %% IPv6 address lookups in ::ffff.A.B.C.D
        %% (IPv4-compatible IPv6 addresses) should be
        %% internally done as IPv4 lookups by the client,
        %% without the caller noticing.
        %%
        [{{H_mx, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_mx++[$.|Domain],
            h_addr_list = [IP4_mx]}},
         {{H_mx_++Domain, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_mx_++Domain,
            h_addr_list = [IP4_mx]}},
         {{H_resolve, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_resolve++[$.|Domain],
            h_addr_list = [IP4_resolve]}},
         {{H_resolve_++Domain, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_resolve_++Domain,
            h_addr_list = [IP4_resolve]}},
         {{H_cname_resolve, inet}, fun lower_h_name/1, ?LINE,
          H4#hostent{
            h_name = "resolve."++Domain,
            h_aliases = [H_cname_resolve++[$.|Domain]],
            h_addr_list = [IP4_resolve]}},
         {{H_cname_resolve_++Domain, inet}, fun lower_h_name/1, ?LINE,
          H4#hostent{
            h_name = "resolve."++Domain,
            h_aliases = [H_cname_resolve_++Domain],
            h_addr_list = [IP4_resolve]}},
         %%
         {{H_ns, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_ns++[$.|Domain],
            h_addr_list = [IP4_ns_1,IP4_ns_2]}},
         {{H_ns_++Domain, inet}, fun norm/1, ?LINE,
          H4#hostent{
            h_name = H_ns_++Domain,
            h_addr_list = [IP4_ns_1,IP4_ns_2]}},
         %%
         {IP4_ns_2, fun norm/1, ?LINE,
          H4#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP4_ns_2]}},
         {IP4_ns_1, fun norm/1, ?LINE,
          H4#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP4_ns_1]}},
         {IP4_mx, fun norm/1, ?LINE,
          H4#hostent{
            h_name = "mx."++Domain,
            h_addr_list = [IP4_mx]}},
         {IP4_mx, fun norm/1, ?LINE,
          H4#hostent{
            h_name = "mx."++Domain,
            h_addr_list = [IP4_mx]}},
         %%
         %%
         %%
         {{H_resolve_++Domain, inet6}, fun norm/1, ?LINE,
          H6#hostent{
            h_name = H_resolve_++Domain,
            h_addr_list = [IP6_resolve]}},
         {{H_resolve, inet6}, fun norm/1, ?LINE,
          H6#hostent{
            h_name = H_resolve++[$.|Domain],
            h_addr_list = [IP6_resolve]}},
         {{H_cname_resolve, inet6}, fun lower_h_name/1, ?LINE,
          H6#hostent{
            h_name = "resolve."++Domain,
            h_aliases = [H_cname_resolve++[$.|Domain]],
            h_addr_list = [IP6_resolve]}},
         {{H_cname_resolve_++Domain, inet6}, fun lower_h_name/1, ?LINE,
          H6#hostent{
            h_name = "resolve."++Domain,
            h_aliases = [H_cname_resolve_++Domain],
            h_addr_list = [IP6_resolve]}},
         {IP6_resolve, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "resolve."++Domain,
            h_addr_list = [IP6_resolve]}},
         {IP64_resolve, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "resolve."++Domain,
            h_addr_list = [IP64_resolve]}},
         %%
         {{H_ns, inet6}, fun norm/1, ?LINE,
          H6#hostent{
            h_name = H_ns++[$.|Domain],
            h_addr_list =
                [IP6_ns_1,
                 IP6_ns_2]}},
         {{H_ns_++Domain, inet6}, fun norm/1, ?LINE,
          H6#hostent{
            h_name = H_ns_++Domain,
            h_addr_list =
                [IP6_ns_1,
                 IP6_ns_2]}},
         {IP6_ns_1, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP6_ns_1]}},
         {IP6_ns_2, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP6_ns_2]}},
         {IP64_ns_1, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP64_ns_1]}},
         {IP64_ns_2, fun norm/1, ?LINE,
          H6#hostent{
            h_name = "ns."++Domain,
            h_addr_list = [IP64_ns_2]}}
         %%
         %%
         %%
        ],
    Results =
        [case Target of
             {H, T} ->
                 inet_res:gethostbyname(H, T);
             Addr ->
                 inet_res:gethostbyaddr(Addr)
         end || {Target, _TFun, _Line, _HE} <- Lookups],
    ?P("inet_cache: ~p~n", [ets:tab2list(inet_cache)]),
    [] = merge_results(Lookups, Results),
    ok.

lower_h_name(#hostent{h_name = HName} = HE) ->
    norm(HE#hostent{h_name = tolower(HName)}).

%%%lower_h_aliases(#hostent{h_aliases = HAliases} = HE) ->
%%%    HE#hostent{
%%%      h_aliases = [tolower(HAlias) || HAlias <- HAliases]}.

norm(#hostent{h_addr_list = Addrs} = HE) ->
    HE#hostent{h_addr_list = lists:sort(Addrs)}.

merge_results([], []) ->
    [];
merge_results([{H_T, TFun, Line, HE} | Lookups], [Result | Results]) ->
    case
        case Result of
            {ok, ResultHE} ->
                {ok, TFun(ResultHE)};
            _ ->
                Result
        end
    of
        {ok, HE} ->
            merge_results(Lookups, Results);
        Other ->
            [{{Line, H_T, HE}, Other} | merge_results(Lookups, Results)]
    end.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Lookup different records using resolve/2..4.
resolve(Config) when is_list(Config) ->
    ?P("begin"),
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
	 {soa,Domain,
          undefined,
          [{soa,{"ns.otptest","lsa\\.soa.otptest",1,60,10,300,30}}]},
	 %% WKS: protocol TCP (6), services (bits) TELNET (23) and SMTP (25)
	 {wks,"wks."++Name,[{wks,{{127,0,0,28},6,<<0,0,1,64>>}}],undefined},
	 {ptr,"28."++RDomain4,[{ptr,Name}],undefined},
	 {ptr,"c.1.0.0.0.0.f.7."++RDomain6,[{ptr,Name}],undefined},
	 {hinfo,Name,[{hinfo,{"BEAM","Erlang/OTP"}}],undefined},
	 {mx,RDomain4,[{mx,{10,"mx."++Domain}}],undefined},
         {loc,"loc."++Name,
          [{loc,{{42.0625,13.125},17.0,100.0,{10000.0,10.0}}}],
          undefined},
	 {srv,"_srv._tcp."++Name,[{srv,{10,3,4711,Name}}],undefined},
	 {naptr,"naptr."++Name,
	  [{naptr,{10,5,"s","http","","_srv._tcp."++Name}}],
	  undefined},
	 {txt,"txt."++Name,
	  [{txt,["Hej ","du ","glade "]},{txt,["ta ","en ","spade!"]}],
	  undefined},
	 {mb,"mb."++Name,[{mb,"mx."++Name}],undefined},
	 {mg,"mg."++Name,[{mg,"lsa\\.mg."++Domain}],undefined},
	 {mr,"mr."++Name,[{mr,"lsa\\.mr."++Domain}],undefined},
	 {minfo,"minfo."++Name,
	  [{minfo,{"minfo-owner."++Name,"minfo-bounce."++Name}}],
	  undefined},
         {uri,"uri."++Name,[{uri,{10,3,"http://erlang.org"}}],undefined},
         {caa,"caa."++Name,
          [{caa,{1,"iodef","http://iodef.erlang.org"}}],
          undefined},
	 {any,"cname."++Name,[{cname,Name}],undefined},
	 {any,Name,
	  #{ {a,{127,0,0,28}} => [],
             {aaaa,{0,0,0,0,0,0,32512,28}} => [],
             {hinfo,{"BEAM","Erlang/OTP"}} => [] },
	  undefined}
	],
    ?P("resolve -> with edns 0"),
    resolve(Class, [{edns,0},{nameservers,[NS]}], L),
    ?P("resolve -> with edns false"),
    resolve(Class, [{edns,false},{nameservers,[NS]}], L),
    %% Again, to see ensure the cache does not mess things up
    ?P("resolve -> with edns 0 (again)"),
    resolve(Class, [{edns,0},{nameservers,[NS]}], L),
    ?P("resolve -> with edns false (again)"),
    Res = resolve(Class, [{edns,false},{nameservers,[NS]}], L),
    ?P("resolve -> done: ~p", [Res]),
    Res.

resolve(_Class, _Opts, []) ->
    ?P("resolve -> done"),
    ok;
resolve(Class, Opts, [{Type,Nm,Answers,Authority}=Q|Qs]) ->
    ?P("resolve ->"
       "~n      Query:   ~p"
       "~n      Options: ~p", [Q, Opts]),
    {Name,NameC} =
	case erlang:phash2(Q) band 4 of
	    0 ->
		{Nm,caseflip(Nm)};
	    _ ->
		{caseflip(Nm),Nm}
	end,
    NormAnswers = normalize_rrs(Answers),
    NormNSs = normalize_rrs(Authority),
    ?P("resolve -> resolve with ~p", [Name]),
    {ok,Msg} = inet_res:resolve(Name, Class, Type, Opts),
    check_msg(Class, Type, Msg, NormAnswers, NormNSs),
    ?P("resolve -> resolve with ~p", [NameC]),
    {ok,MsgC} = inet_res:resolve(NameC, Class, Type, Opts),
    check_msg(Class, Type, MsgC, NormAnswers, NormNSs),
    ?P("resolve -> next"),
    resolve(Class, Opts, Qs).



normalize_rrs(undefined = RRs) -> RRs;
normalize_rrs(RRList) when is_list(RRList) ->
    lists:sort([normalize_rr(RR) || RR <- RRList]);
normalize_rrs(RRs) when is_map(RRs) ->
    maps:fold(
      fun (RR, V, NormRRs) ->
              NormRRs#{(normalize_rr(RR)) => V}
      end, #{}, RRs).

normalize_rr({soa,{NS,HM,Ser,Ref,Ret,Exp,Min}}) ->
    {tolower(NS),tolower_email(HM),Ser,Ref,Ret,Exp,Min};
normalize_rr({mx,{Prio,DN}}) ->
    {Prio,tolower(DN)};
normalize_rr({srv,{Prio,Weight,Port,DN}}) ->
    {Prio,Weight,Port,tolower(DN)};
normalize_rr({naptr,{Order,Pref,Flags,Service,RE,Repl}}) ->
    {Order,Pref,Flags,Service,RE,tolower(Repl)};
normalize_rr({minfo,{RespM,ErrM}}) ->
    {tolower_email(RespM),tolower_email(ErrM)};
normalize_rr({T,MN}) when T =:= mg; T =:= mr ->
    tolower_email(MN);
normalize_rr({T,DN}) when T =:= cname; T =:= ns; T =:= ptr; T =:= mb ->
    tolower(DN);
normalize_rr(RR) ->
    RR.

check_msg(Class, Type, Msg, ExpectedAnswers, ExpectedNSs) ->
    ?P("check_msg ->"
       "~n      Type: ~p"
       "~n      Msg:  ~p", [Type,Msg]),
    NormAnList =
        normalize_rrs(
          [begin
               Class = inet_dns:rr(RR, class),
               {inet_dns:rr(RR, type),inet_dns:rr(RR, data)}
           end || RR <- inet_dns:msg(Msg, anlist)]),
    NormNsList =
           normalize_rrs(
             [begin
                  Class = inet_dns:rr(RR, class),
                  {inet_dns:rr(RR, type),inet_dns:rr(RR, data)}
              end || RR <- inet_dns:msg(Msg, nslist)]),
    case
        check_msg(ExpectedAnswers, NormAnList) andalso
        check_msg(ExpectedNSs, NormNsList)
    of
        true ->
            ok;
        false
          when Type =:= ns;
               Type =:= soa ->
            %% Some resolvers return the answer to a NS query
            %% in the answer section instead of in the authority section,
            %% and some do the same for a SOA query
            case
                check_msg(ExpectedAnswers, NormNsList) andalso
                check_msg(ExpectedNSs, NormAnList)
            of
                true ->
                    ok;
                false ->
                    error({Type,
                           {expected,ExpectedAnswers,ExpectedNSs},
                           {got,NormAnList,NormNsList}})
            end;
        false ->
            error({Type,
                   {expected,ExpectedAnswers,ExpectedNSs},
                   {got,NormAnList,NormNsList}})
    end,
    %% Test the encoder against the decoder; the least we can do
    Buf = inet_dns:encode(Msg),
    {ok,Msg} = inet_dns:decode(Buf),
    ok.

check_msg(undefined, RRs) when is_list(RRs)-> true;
check_msg(RRs1, RRs2) when is_list(RRs1), is_list(RRs2) ->
    RRs1 =:= RRs2;
check_msg(Expected, [RR|RRs]) when is_map(Expected) ->
    case Expected of
        #{RR := _} ->
            case RRs of
                []    -> true;
                [_|_] -> check_msg(Expected, RRs)
            end;
        #{} -> false
    end;
check_msg(#{}, []) -> false. % At least one has to be ok

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test EDNS and truncation.
edns0(Config) when is_list(Config) ->
    ?P("begin"),
    NS = ns(Config),
    Opts = [{nameservers,[NS]},verbose],
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
    MXs = lists:sort(inet_res:lookup(Domain, in, mx, Opts)),
    %%
    {ok,Msg1} = inet_res:resolve(Domain, in, mx, Opts),
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
    {ok,Msg2} = inet_res:resolve(Domain, in, mx, [{edns,0}|Opts]),
    MXs = lists:sort(inet_res_filter(inet_dns:msg(Msg2, anlist), in, mx)),
    Buf2 = inet_dns:encode(Msg2),
    {ok,Msg2} = inet_dns:decode(Buf2),
    Res = case [RR || RR <- inet_dns:msg(Msg2, arlist),
                      inet_dns:rr(RR, type) =:= opt] of
              [OptRR] ->
                  ?P("opt rr:"
                     "~n      ~p", [inet_dns:rr(OptRR)]),
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
                          end;
                      _ ->
                          ok
                  end
          end,
    ?P("done"),
    Res.

inet_res_filter(Anlist, Class, Type) ->
    [inet_dns:rr(RR, data) || RR <- Anlist,
			      inet_dns:rr(RR, type) =:= Type,
			      inet_dns:rr(RR, class) =:= Class].

%% Test EDNS we catch multiple dns_opt_rr as FORMERR (RFC 6891: 6.1.1)
edns0_multi_formerr(Config) when is_list(Config) ->
    Domain = "otptest",
    Opts = [{nameservers,[ns(Config)]},{edns,0}],
    {ok,DnsRec0} = inet_res:resolve(Domain, in, soa, Opts),
    [EDNS|_] = [ X || X <- DnsRec0#dns_rec.arlist, is_record(X, dns_rr_opt) ],
    DnsRec = DnsRec0#dns_rec{ arlist = [EDNS|DnsRec0#dns_rec.arlist] },
    {error,formerr} = inet_dns:decode(inet_dns:encode(DnsRec)),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests TXT records.
txt_record(Config) when is_list(Config) ->
    ?P("begin"),
    D1 = "cslab.ericsson.net",
    D2 = "mail1.cslab.ericsson.net",
    ?P("try nslookup of ~p", [D1]),
    {ok,#dns_rec{anlist=[RR1]}} = 
	inet_res:nslookup(D1, in, txt),
    ?P("RR1:"
       "~n      ~p", [RR1]),
    ?P("try nslookup of ~p", [D2]),
    {ok,#dns_rec{anlist=[RR2]}} =
	inet_res:nslookup(D2, in, txt),
    ?P("RR2:"
       "~n      ~p", [RR2]),
    #dns_rr{domain=D1, class=in, type=txt, data=A1} = RR1,
    #dns_rr{domain=D2, class=in, type=txt, data=A2} = RR2,
    case [lists:flatten(A2)] of
	A1 = [[_|_]] -> ok
    end,
    ?P("done"),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests monitoring of /etc/hosts and /etc/resolv.conf, but not them.
files_monitor(Config) when is_list(Config) ->
    ?P("begin"),
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
    end,
    ?P("done"),
    ok.

do_files_monitor(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    {ok,Hostname} = inet:gethostname(),
    ?P("Hostname: ~p", [Hostname]),
    FQDN =
	case inet_db:res_option(domain) of
	    "" ->
		Hostname;
	    _ ->
		Hostname++"."++inet_db:res_option(domain)
	end,
    ?P("FQDN: ~p", [FQDN]),
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
%% Get full DNS answer on nxdomain (when option set)
%% Check that we get the error code from the first server.

nxdomain_reply(Config) when is_list(Config) ->
    NS    = ns(Config),
    Name  = "nxdomain.otptest",
    Class = in,
    Type  = a,
    Opts  =
        [{nameservers,[NS]}, {servfail_retry_timeout, 1000}, verbose],
    ?P("try resolve"),
    {error, nxdomain} = inet_res:resolve(Name, Class, Type, Opts),
    {error, {nxdomain, Rec}} =
        inet_res:resolve(Name, Class, Type, [nxdomain_reply|Opts]),
    ?P("resolved: "
       "~n      ~p", [Rec]),
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
    %% resolve; with short timeout to trigger Timeout =:= 0 in inet_res
    {error,timeout} =
	inet_res:resolve(
	  Name, in, a, [{nameservers,[ProxyNS]},verbose], Time + 10),
    %%
    proxy_wait(PSpec),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First name server answers ?REFUSED, second does not answer.
%% Check that we get the error code from the first server.

intermediate_error(Config) when is_list(Config) ->
    NS      = ns(Config),
    Name    = "ns.otptest",
    Class   = in,
    Type    = a,
    IP      = {127,0,0,1},
    %% A "name server" that does not respond
    S       = ok(gen_udp:open(0, [{ip,IP},{active,false}])),
    Port    = ok(inet:port(S)),
    NSs     = [NS,{IP,Port}],
    Opts    = [{nameservers, NSs}, verbose],
    Timeout = 500,
    {error, {refused,_}} = inet_res:resolve(Name, Class, Type, Opts, Timeout),
    _ = gen_udp:close(S),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A name server that firstanswers ?SERVFAIL, the second try *if* the retry
%% is not received *too soon* (etd) answers noerror.

servfail_retry_timeout_default(Config) when is_list(Config) ->
    NS        = ns(Config),
    Name      = "ns.otptest",
    Class     = in,
    Type      = a,
    Opts      = [{nameservers,[NS]}, verbose],
    ?P("try resolve"),
    {ok, Rec} = inet_res:resolve(Name, Class, Type, Opts),
    ?P("resolved: "
       "~n      ~p", [Rec]),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A name server that firstanswers ?SERVFAIL, the second try *if* the retry
%% is not received *too soon* (etd) answers noerror.

servfail_retry_timeout_1000(Config) when is_list(Config) ->
    NS        = ns(Config),
    Name      = "ns.otptest",
    Class     = in,
    Type      = a,
    Opts      = [{nameservers,[NS]}, {servfail_retry_timeout, 1000}, verbose],
    ?P("try resolve"),
    {ok, Rec} = inet_res:resolve(Name, Class, Type, Opts),
    ?P("resolved: "
       "~n      ~p", [Rec]),
    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that label encoding compression limits at 14 bits pointer size

label_compression_limit(Config) when is_list(Config) ->
    FirstSz = 4,
    Count = 512,
    Sz = 16,
    %% We create a long DNS message with an answer list containing
    %% 1+512+1 RR:s.  The first label is 4 chars that with message
    %% and RR overhead places the second label on offset 32.
    %% All other labels are 16 chars that with RR overhead
    %% places them on offsets of N * 32.
    %%
    %% The labels are: "ZZZZ", then; "AAAAAAAAAAAAAAAA",
    %% "AAAAAAAAAAAAAAAB", incrementing, so no one is
    %% equal and can not be compressed, until the last one
    %% that refers to the second to last one, so it could be compressed.
    %%
    %% However, the second to last label lands on offset 512 * 32 = 16384
    %% which is out of reach for compression since compression uses
    %% a 14 bit reference from the start of the message.
    %%
    %% The last label can only be compressed when we instead
    %% generate a message with *one less* char in the first label,
    %% placing the second to last label on offset 16383.
    %%
    %% So, MsgShort can use compression for the last RR
    %% by referring to the second to last RR, but MsgLong can not.
    %%
    %% Disclaimer:
    %%    All offsets and overheads are deduced
    %%    through trial and observation
    %%
    [D | Domains] = gen_domains(Count, lists:duplicate(Sz, $A), []),
    LastD = "Y." ++ D,
    DomainsCommon = lists:reverse(Domains, [D, LastD]),
    DomainsShort =  [lists:duplicate(FirstSz-1, $Z) | DomainsCommon],
    DomainsLong =   [lists:duplicate(FirstSz, $Z) | DomainsCommon],
    MsgShort = gen_msg(DomainsShort),
    MsgLong = gen_msg(DomainsLong),
    DataShort = inet_dns:encode(MsgShort),
    DataShortSz = byte_size(DataShort),
    ?P("DataShort[~w]:~n    ~p~n", [DataShortSz, DataShort]),
    DataLong = inet_dns:encode(MsgLong),
    DataLongSz = byte_size(DataLong),
    ?P("DataLong[~w]:~n    ~p~n", [DataLongSz, DataLong]),
    %% When the first (long) RR size pushes the last compressed label out,
    %% that occupied a 2 bytes reference, it instead becomes a label
    %% with 1 byte size and a final empty label size 1
    0 = DataLongSz - (DataShortSz+1 - 2 + 1+Sz+1),
    ok.

gen_msg(Domains) ->
    inet_dns:make_msg(
      [{header, inet_dns:make_header()},
       {anlist, gen_rrs(Domains)}]).

gen_rrs(Domains) ->
    [inet_dns:make_rr([{class,in},{type,a},{domain,D},{data,{17,18,19,20}}]) ||
        D <- Domains].

gen_domains(0, _Domain, Acc) ->
    Acc;
gen_domains(N, Domain, Acc) ->
    gen_domains(
      N - 1, incr_domain(Domain), [lists:reverse(Domain) | Acc]).

incr_domain([$Z | Domain]) ->
    [$A | incr_domain(Domain)];
incr_domain([Char | Domain]) ->
    [Char+1 | Domain].


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that the data portion can only be zero bytes for UPDATEs
%% and that a real DNS server (Knot DNS) accepts our packet

update(Config) when is_list(Config) ->
    {NSIP,NSPort} = ns(Config),
    Domain = "otptest",

    % test that empty data for a query fails
    QueryRec = #dns_rec{
        header = #dns_header{ opcode = query },
        anlist = [
            #dns_rr{ domain = "test-update." ++ Domain, type = a }
        ]
    },
    true = try inet_dns:encode(QueryRec) of
        _ ->
            false
    catch
        error:{badmatch,[]}:_ ->
            true
    end,

    % test that empty data for an update
    UpdateRec = #dns_rec{
        header = #dns_header{ opcode = update },
        % Zone
        qdlist = [
            #dns_query{ domain = Domain, class = in, type = soa }
        ],
        % Update
        nslist = [
            #dns_rr{
                domain = "update-test." ++ Domain,
                ttl = 300,
                class = in,
                type = a,
                data = {192,0,2,1}
            }
        ]
    },
    UpdatePkt = inet_dns:encode(UpdateRec),
    true = is_binary(UpdatePkt),

    % check if an actual DNS server accepts it
    SockOpts = [binary,{active,false}],
    {ok,Sock} = gen_udp:open(0, SockOpts),
    ok = gen_udp:connect(Sock, NSIP, NSPort),
    ok = gen_udp:send(Sock, UpdatePkt),
    {ok,{NSIP,NSPort,ResponsePkt}} = gen_udp:recv(Sock, 0),
    ok = gen_udp:close(Sock),
    {ok,ResponseRec} = inet_dns:decode(ResponsePkt),
    #dns_rec{ header = #dns_header{ rcode = ?NOERROR } } = ResponseRec,

    ok.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests for TSIG

% note for implementors reading this, the usage of
% inet_dns_tsig.erl is identical except you do not need
% to inspect the reponse for the presence of a TSIG RR
tsig_client(Config) when is_list(Config) ->
    {NSIP,NSPort} = ns(Config),
    Domain = "otptest",
    Request = #dns_rec{
        header = #dns_header{},
        qdlist = [#dns_query{ domain = Domain, class = in, type = axfr }]
    },
    Pkt = inet_dns:encode(Request),

    Key = {"testkey","ded5ada3-07f2-42b9-84bf-82d30f6795ee"},
    TS0 = inet_dns_tsig:init([{key,Key}]),
    {ok,PktS,TS1} = inet_dns_tsig:sign(Pkt, TS0),

    SockOpts = [binary,{active,false},{nodelay,true},{packet,2}],
    {ok,Sock} = gen_tcp:connect(NSIP, NSPort, SockOpts),
    ok = gen_tcp:send(Sock, PktS),
    ok = gen_tcp:shutdown(Sock, write),
    Recv = fun(Recv, A) ->
        case gen_tcp:recv(Sock, 0) of
            {ok,P} ->
                Recv(Recv, [P|A]);
            {error,closed} ->
                lists:reverse(A)
        end
    end,
    PktR = Recv(Recv, []),
    ok = gen_tcp:close(Sock),

    true = PktR =/= [],

    {_TS,Response} = lists:foldl(fun(P, {T0,R0}) ->
        {ok,R} = inet_dns:decode(P),
        {ok,T} = inet_dns_tsig:verify(P, R, T0),
        {T,R0 ++ [R]}
    end, {TS1,[]}, PktR),

    % not necessary for the test, but helpful to those using this
    % to understand how to validate TCP responses, we need to check
    % that the last message has a TSIG RR (RRC8945, section 5.3.1)
    % N.B. unnecessary for UDP as handled by inet_dns_tsig:verify()
    #dns_rec{ arlist = ARList } = lists:last(Response),
    true = ARList =/= [],
    #dns_rr_tsig{} = lists:last(ARList),

    % actual implementations would here now consider if the additional
    % checks described in the WARNING at the top of inet_dns_tsig.erl
    % are applicable to their use case and if so implement them
    ok.

tsig_server(Config) when is_list(Config) ->
    case os:find_executable("dig") of
        false ->
            case os:find_executable("drill") of
                false ->
                    {skip, "Cannot find executable: dig|drill"};
                Drill ->
                    tsig_server(Config, Drill)
            end;
        Dig ->
            tsig_server(Config, Dig)
    end.
%%
tsig_server(_Config, DigDrill) ->
    Domain = "otptest",

    Key = {"testkey","b0b8006a-04ad-4a96-841a-a4eae78011a1"},
    Keys0 = [Key,{"grease0",""},{"grease1",""},{"grease2",""}],
    Rand = [ rand:uniform() || _ <- lists:seq(1, length(Keys0)) ],
    {_,Keys} = lists:unzip(lists:keysort(1, lists:zip(Rand, Keys0))),
    TS0 = inet_dns_tsig:init([{keys,Keys}]),

    SockOpts = [binary,{active,false},{nodelay,true},{packet,2}],
    {ok,LSock} = gen_tcp:listen(0, [{ip,{127,0,0,1}}|SockOpts]),
    {ok,LPort} = inet:port(LSock),

    _ = process_flag(trap_exit, true),
    {_, MRef} =
        spawn_opt(
          fun() ->
                  KeyName = element(1, Key),
                  KeySecret = base64:encode_to_string(element(2, Key)),
                  Command = DigDrill ++
                      " -p " ++ integer_to_list(LPort) ++
                      " -y hmac-sha256:" ++ KeyName ++ ":" ++ KeySecret ++
                      " " ++ Domain ++ ". @127.0.0.1 AXFR IN",
                  Opts = [in,eof,exit_status,stderr_to_stdout,hide],
                  Port = erlang:open_port({spawn,Command}, Opts),
                  Collect =
                      fun C(A) ->
                              receive
                                  {Port,{data,B}} ->
                                      C([B|A]);
                                  {Port,eof} ->
                                      C(A);
                                  {Port,{exit_status,Status}} ->
                                      {ok,{Status,lists:reverse(A)}};
                                  {Port,closed} ->
                                      {error,closed}
                              after 1000 ->
                                      {error,timeout}
                              end
                      end,
                  ClientResult = Collect([]),
                  true = erlang:port_close(Port),
                  exit({done,ClientResult})
          end, [link,monitor]),

    Result =
        case gen_tcp:accept(LSock, 5000) of
            {ok,Sock} ->
                try
                    ok = gen_tcp:close(LSock),
                    tsig_server(Domain, TS0, Sock)
                of
                    _ ->
                        gen_tcp:close(Sock)
                catch
                    Class : Reason : Stacktrace ->
                        _ = catch gen_tcp:close(Sock),
                        {Class,Reason,Stacktrace}
                end;
            Error ->
                _ = catch gen_tcp:close(LSock),
                Error
        end,

    receive
        {'DOWN',MRef,_,_,{done,{ok,{Code,Output}}}} ->
            ?P("Output of ~s (exit code ~B):~n~s~n",
               [DigDrill, Code, Output]),
            {0,ok} = {Code,Result};
        {'DOWN',MRef,_,_,CError} ->
            error({CError,Result})
    after 1000 ->
            error({timeout,Result})
    end.

tsig_server(Domain, TS0, Sock) ->
    SOAData = {"ns","lsa.soa",1,60,10,300,30},
    SOA = #dns_rr{ domain = Domain, class = in, type = soa, data = SOAData },
    PadData = ["0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"],
    Padding = #dns_rr{ class = in, type = txt, data = PadData },

    {ok,Pkt} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:shutdown(Sock, read),

    {ok,Request} = inet_dns:decode(Pkt),
    {ok,TS1} = inet_dns_tsig:verify(Pkt, Request, TS0),

    % actual implementations would here now consider if the additional
    % checks described in the WARNING at the top of inet_dns_tsig.erl
    % are applicable to their use case and if so implement them

    PktR0 = #dns_rec{
        header = #dns_header{
            id = Request#dns_rec.header#dns_header.id,
            qr = true
        },
        qdlist = Request#dns_rec.qdlist
    },

    Format = "~2.10.0B.padding." ++ Domain,
    AnList1 = [SOA] ++ [ Padding#dns_rr{
                    domain = io_lib:format(Format, [X]) } ||
                  X <- lists:seq(0, 9) ],
    PktR1 = inet_dns:encode(PktR0#dns_rec{ anlist = AnList1 }),
    {ok,PktR1S,TS2} = inet_dns_tsig:sign(PktR1, TS1),
    ok = gen_tcp:send(Sock, PktR1S),

    AnList2 = [ Padding#dns_rr{
                    domain = io_lib:format(Format, [X]) } ||
                  X <- lists:seq(10, 19) ],
    PktR2 = inet_dns:encode(PktR0#dns_rec{ anlist = AnList2 }),
    {ok,PktR2S,TS3} = inet_dns_tsig:sign(PktR2, TS2),
    ok = gen_tcp:send(Sock, PktR2S),

    AnList3 = [ Padding#dns_rr{
                    domain = io_lib:format(Format, [X]) } ||
                  X <- lists:seq(20, 29) ] ++ [SOA],
    PktR3 = inet_dns:encode(PktR0#dns_rec{ anlist = AnList3 }),
    {ok,PktR3S,_TS} = inet_dns_tsig:sign(PktR3, TS3),
    ok = gen_tcp:send(Sock, PktR3S).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% inet_dns encode/decode specials
%% Should maybe be a suite of its own.

mdns_encode_decode(Config) when is_list(Config) ->
    Id = 4711,
    Opcode = 'query',
    Class = in,
    Type = txt,
    Domain = "test.local",
    Text = ["abc", "123"],
    BinText = <<3,$a,$b,$c, 3,$1,$2,$3>>,  % Wire format for Text
    IN_h = 32769,                          % Class IN with high bit set
    %%
    %% Create a unicast-response to a mDNS query
    %% with cache-flush bit on the RR record (func field),
    %% which sets the class fields high bit when encoded
    Header =
        inet_dns:make_header(
          [{id, Id}, {qr, true}, {opcode, Opcode},
           {aa, false}, {tc, false}, {rd, false}, {ra, false}, {pr, false}]),
    Query =
        inet_dns:make_dns_query(
          [{class, Class}, {type, Type}, {domain, Domain},
           {unicast_response, true}]), % High bit 1
    TxtRR =
        inet_dns:make_rr(
          [{domain, Domain}, {class, Class}, {type, Type},
           {data, Text},
           {func, true}]), % High bit 1
    Msg =
        inet_dns:make_msg(
          [{header, Header}, {qdlist, [Query]}, {anlist, [TxtRR]}]),
    %%
    %% Encode and verify decode
    Buffer = inet_dns:encode(Msg),
    {{ok, Msg}, Msg} = {inet_dns:decode(Buffer), Msg},
    %%
    %% Decode as if not mDNS, which exposes the high class field bit,
    %% and doesn't decode the RR data
    Query2 =
        inet_dns:make_dns_query(
          Query,
          [{class,IN_h},
           {unicast_response,false}]),  % High bit 0
    TxtRR2 =
        inet_dns:make_rr(
          TxtRR,
          [{class,IN_h},
           {data,BinText},  % Raw, encoded
           {func,false}]),  % High bit 0
    Msg2 = inet_dns:make_msg(Msg, [{qdlist, [Query2]}, {anlist, [TxtRR2]}]),
    {{ok, Msg2}, Msg2} = {inet_dns:decode(Buffer, false), Msg2},
    %%
    %% Encode non-mDNS which ignores the high class bit flags
    %% in #dns_query.unicast_response and #dns_rr.func
    Buffer3 = inet_dns:encode(Msg, false),
    %%
    %% Decode non-mDNS and verify
    Query3 =
        inet_dns:make_dns_query(Query, unicast_response, false), % High bit 0
    TxtRR3 = inet_dns:make_rr(TxtRR, func, false),               % High bit 0
    Msg3 = inet_dns:make_msg(Msg, [{qdlist, [Query3]}, {anlist, [TxtRR3]}]),
    {{ok, Msg3}, Msg3} = {inet_dns:decode(Buffer3, false), Msg3},
    %%
    %% Decode mDNS should give the same answer since the high bits
    %% in the class fields are encoded as zero
    {{ok, Msg3}, Msg3} = {inet_dns:decode(Buffer3, true), Msg3},
    %%
    %% Encode non-mDNS with class >= 32768
    Buffer4 = inet_dns:encode(Msg2, false),
    {{ok, Msg2}, Msg2} = {inet_dns:decode(Buffer4, false), Msg2},
    %%
    %% Decoding for mDNS should set the high class field flags
    {{ok, Msg}, Msg} = {inet_dns:decode(Buffer4, true), Msg},
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


timestamp() ->
    erlang:monotonic_time(milli_seconds).


%% Case flip helper, randomly flips the case of about every second [a-zA-Z]

-compile({inline, [caseflip/3]}).

caseflip([C | Cs]) when is_integer(C), $a =< C, C =< $z ->
    caseflip(Cs, C, $a - $A);
caseflip([C | Cs]) when is_integer(C), $A =< C, C =< $Z ->
    caseflip(Cs, C, $A - $a);
caseflip([C | Cs]) ->
    [C | caseflip(Cs)];
caseflip([]) ->
    [].
%%
caseflip(Cs, C, Diff) ->
    [case 0.5 =< rand:uniform() of
         true ->
             C - Diff;
         false ->
             C
     end | caseflip(Cs)].


tolower_email([$. | Cs]) ->
    [$. | tolower(Cs)];
tolower_email([C | Cs]) ->
    [C | tolower_email(Cs)].

%% Case fold according to RFC 4343
%%
tolower([C | Cs]) when is_integer(C) ->
    if  $A =< C, C =< $Z ->
	    [(C - $A + $a) | tolower(Cs)];
	true ->
	    [C | tolower(Cs)]
    end;
tolower([]) ->
    [].

toupper([C | Cs]) when is_integer(C) ->
    if  $a =< C, C =< $z ->
	    [(C - $a + $A) | toupper(Cs)];
	true ->
	    [C | toupper(Cs)]
    end;
toupper([]) ->
    [].

-compile({inline,[ok/1]}).
ok(ok) -> ok;
ok({ok,X}) -> X;
ok({error,Reason}) -> error(Reason).
