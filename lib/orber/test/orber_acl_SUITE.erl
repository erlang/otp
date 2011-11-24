%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
%%
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for the ACL functions
%%
%%-----------------------------------------------------------------
-module(orber_acl_SUITE).

-include_lib("test_server/include/test_server.hrl").

-define(default_timeout, ?t:minutes(5)).

-define(match(ExpectedRes,Expr),
        fun() ->
               AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   ExpectedRes ->
                       io:format("------ CORRECT RESULT ------~n~p~n",
                                 [AcTuAlReS]),
                       AcTuAlReS;
                   _ ->
                       io:format("###### ERROR ERROR ######~nRESULT:  ~p~n",
                                 [AcTuAlReS]),
                       ?line exit(AcTuAlReS)
               end
       end()).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [ipv4_verify, ipv4_range, ipv4_interfaces, ipv4_bm,
     ipv6_verify, ipv6_range, ipv6_interfaces, ipv6_bm].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_suite(Config) ->
    if
        list(Config) ->
            Config;
        true ->
            exit("Config not a list")
    end.

end_per_suite(Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv4_verify(doc) -> ["Testing IPv4 Verify Operation."];
ipv4_verify(suite) -> [];
ipv4_verify(_) ->
    ?match(true, orber_acl:verify("192.168.64.148", "192.168.64.0/17", inet)),
    ?match({false,"192.168.128.0","192.168.255.255"},
	   orber_acl:verify("192.168.64.148", "192.168.255.0/17", inet)),
    ?match(true, orber_acl:verify("192.168.255.148", "192.168.128.0/17", inet)),
    ?match(true, orber_acl:verify("192.168.128.148", "192.168.128.0/17", inet)),
    ?match(true, orber_acl:verify("192.168.255.255", "192.168.128.0/16", inet)),
    ?match({false,"192.168.0.0","192.168.255.255"},
	   orber_acl:verify("192.169.255.255", "192.168.128.0/16", inet)),
    ?match(true, orber_acl:verify("192.168.128.255", "192.168.128.0/24", inet)),
    ?match({false,"192.168.128.0","192.168.128.255"},
	   orber_acl:verify("192.168.255.255", "192.168.128.0/24", inet)),
    ?match({false,"192.168.128.0","192.168.128.127"},
	   orber_acl:verify("192.168.128.255", "192.168.128.0/25", inet)),
    ?match(true, orber_acl:verify("192.168.128.255", "192.168.128.128/25", inet)),
    ?match(true, orber_acl:verify("192.168.128.128", "192.168.128.128/32", inet)),
    ?match({false,"192.168.128.128.","192.168.128.128."},
	   orber_acl:verify("192.168.128.255", "192.168.128.128/32", inet)),
    ?match(true, orber_acl:verify("192.168.128.128", "192.168.128.128", inet)),
    ?match({false,"192.168.128.128.","192.168.128.128."},
	   orber_acl:verify("192.168.128.255", "192.168.128.128", inet)),
    ?match(true, orber_acl:verify("192.168.128.255", "192.168.128.128/7", inet)),
    ok.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv4_range(doc) -> ["Testing IPv4 Range Operation."];
ipv4_range(suite) -> [];
ipv4_range(_) ->
    ?match({ok,"192.168.0.0", "192.168.127.255"},
	   orber_acl:range("192.168.64.0/17")),
    ?match({ok, "192.168.128.0", "192.168.255.255"},
	   orber_acl:range("192.168.255.0/17")),
    ?match({ok,"192.168.128.0","192.168.255.255"},
	   orber_acl:range("192.168.128.0/17")),
    ?match({ok,"192.168.0.0","192.168.255.255"},
	   orber_acl:range("192.168.128.0/16")),
    ?match({ok,"192.168.128.0","192.168.128.255"},
	   orber_acl:range("192.168.128.0/24")),
    ?match({ok,"192.168.128.0","192.168.128.127"},
	   orber_acl:range("192.168.128.0/25")),
    ?match({ok,"192.168.128.128","192.168.128.255"},
	orber_acl:range("192.168.128.128/25")),
    ?match({ok,"192.168.128.128.","192.168.128.128."},
	   orber_acl:range("192.168.128.128/32")),
    ?match({ok,"192.168.128.128.","192.168.128.128."},
	   orber_acl:range("192.168.128.128")),
    ?match({ok,"192.0.0.0","193.255.255.255"},
	   orber_acl:range("192.168.128.128/7")),
    ok.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv4_interfaces(doc) -> ["Testing IPv4 Interfaces Operation."];
ipv4_interfaces(suite) -> [];
ipv4_interfaces(_) ->
    ?match({ok, _},
	   orber_acl:init_acl([{tcp_in, "192.168.128.0/18", ["10.1.1.1"]},
			       {tcp_in, "192.167.64.0/18#4001/5001", ["10.1.1.2"]},
			       {tcp_in, "192.166.192.0/18"}], inet)),
    {ok, IPTuple1} = ?match({ok, _}, inet:getaddr("192.168.128.0", inet)),
    ?match({true, ["10.1.1.1"], 0}, orber_acl:match(IPTuple1, tcp_in, true)),
    ?match({false, [], 0}, orber_acl:match(IPTuple1, tcp_out, true)),
    {ok, IPTuple2} = ?match({ok, _}, inet:getaddr("192.167.64.0", inet)),
    ?match({true, ["10.1.1.2"], {4001,5001}}, orber_acl:match(IPTuple2, tcp_in, true)),
    ?match({false, [], 0}, orber_acl:match(IPTuple2, tcp_out, true)),
    {ok, IPTuple3} = ?match({ok, _}, inet:getaddr("192.166.192.0", inet)),
    ?match({true, [], 0}, orber_acl:match(IPTuple3, tcp_in, true)),
    ?match(false, orber_acl:match(IPTuple3, tcp_out)),
    ?match(ok, orber_acl:clear_acl()),
    ok.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv4_bm(doc) -> ["Benchmarking runtime critical IPv4 Operations."];
ipv4_bm(suite) -> [];
ipv4_bm(_) ->
    ?match({ok, _, _, _}, bm2([{tcp_in, "192.168.64.0/17"}], inet, "192.168.64.148")),
    ok.
%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv6_verify(doc) -> ["Testing IPv6 Verify Operation."];
ipv6_verify(suite) -> [];
ipv6_verify(_) ->
    case orber_test_lib:version_ok() of
	true ->
	    ?match(true, orber_acl:verify("2002:C0A8:0:0:0:0:0:0", "2002:C0A8::/48", inet6)),
	    ?match(true, orber_acl:verify("2002:C0A8:0:FFFF:FFFF:FFFF:FFFF:FFFF", "2002:C0A8::/48", inet6)),
	    ?match({false,"2002:C0A8:0:0:0:0:0:0", "2002:C0A8:0:FFFF:FFFF:FFFF:FFFF:FFFF"},
		   orber_acl:verify("2002:C0A8:1:FFFF:FFFF:FFFF:FFFF:FFFF", "2002:C0A8::/48", inet6)),
	    ?match(true, orber_acl:verify("2002:C0A8:1:FFFF:FFFF:FFFF:FFFF:FFFF", "2002:C0A8::/47", inet6)),
	    ?match({false,"2002:C0A8:0:0:0:0:0:0", "2002:C0A8:1:FFFF:FFFF:FFFF:FFFF:FFFF"},
		   orber_acl:verify("2002:C0A8:2:FFFF:FFFF:FFFF:FFFF:FFFF", "2002:C0A8::/47", inet6)),
	    ok;
	Reason ->
	    Reason
    end.
	    
%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv6_range(doc) -> ["Testing IPv6 Range Operation."];
ipv6_range(suite) -> [];
ipv6_range(_) ->
    case orber_test_lib:version_ok() of
	true ->
	    ?match({ok,"2002:C0A8:0:0:0:0:0:0", "2002:C0A8:0:FFFF:FFFF:FFFF:FFFF:FFFF"},
		   orber_acl:range("2002:C0A8::/48", inet6)),
	    ?match({ok,"2002:C0A8:0:0:0:0:0:0", "2002:C0A8:1:FFFF:FFFF:FFFF:FFFF:FFFF"},
		   orber_acl:range("2002:C0A8::/47", inet6)),
	    ok;
	Reason ->
	    Reason
    end.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv6_interfaces(doc) -> ["Testing IPv6 Interfaces Operation."];
ipv6_interfaces(suite) -> [];
ipv6_interfaces(_) ->
    case orber_test_lib:version_ok() of
	true ->
	    ?match({ok, _}, orber_acl:init_acl([{tcp_in, "2002:C0A8::/49", ["0:0:0:0:0:0:10.1.1.1"]}], inet6)),
	    {ok, IPTuple1} = ?match({ok, _}, inet:getaddr("2002:C0A8:0:7FFF:FFFF:FFFF:FFFF:FFFF", inet6)),
	    ?match({true, ["0:0:0:0:0:0:10.1.1.1"], 0}, orber_acl:match(IPTuple1, tcp_in, true)),
	    ?match(false, orber_acl:match(IPTuple1, tcp_out)),
	    ?match(ok, orber_acl:clear_acl()),
	    ok;
	Reason ->
	    Reason
    end.

%%-----------------------------------------------------------------
%% Test Case  : 
%% Description: 
%%-----------------------------------------------------------------
ipv6_bm(doc) -> ["Benchmarking runtime critical IPv6 Operations."];
ipv6_bm(suite) -> [];
ipv6_bm(_) ->
    case orber_test_lib:version_ok() of
	true ->
	    ?match({ok, _, _, _}, bm2([{tcp_in, "2002:C0A8::/48"}], inet6, "2002:C0A8:0:0:0:0:0:0")),
	    ok;
	Reason ->
	    Reason
    end.

%%-----------------------------------------------------------------
%% Local Functions
%%-----------------------------------------------------------------
-define(NO_OF_TIMES, 1000).

bm2(Filters, Family, Ip) ->
    {ok, IPTuple} = inet:getaddr(Ip, Family),
    orber_acl:init_acl(Filters, Family),
    TimeBefore1 = erlang:now(),
    bm_loop(IPTuple, ?NO_OF_TIMES),
    TimeAfter1 = erlang:now(),
    orber_acl:clear_acl(),
    Time1 = computeTime(TimeBefore1, TimeAfter1),
    orber_acl:init_acl(Filters, Family),
    TimeBefore2 = erlang:now(),
    bm_loop2(Ip, ?NO_OF_TIMES, Family),
    TimeAfter2 = erlang:now(),
    orber_acl:clear_acl(),
    Time2 = computeTime(TimeBefore2, TimeAfter2),
    orber_acl:init_acl(Filters, Family),
    TimeBefore3 = erlang:now(),
    bm_loop2(IPTuple, ?NO_OF_TIMES, Family),
    TimeAfter3 = erlang:now(),
    orber_acl:clear_acl(),
    Time3 = computeTime(TimeBefore3, TimeAfter3),
    {ok, round(?NO_OF_TIMES/Time1), round(?NO_OF_TIMES/Time2), round(?NO_OF_TIMES/Time3)}.


bm_loop(_Ip, 0) ->
    ok;
bm_loop(Ip, N) ->
    true = orber_acl:match(Ip, tcp_in),
    bm_loop(Ip, N-1).

bm_loop2(_Ip, 0, _Family) ->    
    ok;
bm_loop2(Ip, N, Family) ->
    {ok, IPTuple} = inet:getaddr(Ip, Family),
    true = orber_acl:match(IPTuple, tcp_in),
    bm_loop2(Ip, N-1, Family).

computeTime({_MegaSecb, Secb, MicroSecb}, {_MegaSeca, Seca, MicroSeca}) ->
    (Seca - Secb) + ((MicroSeca - MicroSecb) / 1000000).


%%-----------------------------------------------------------------
%% END OF MODULE
%%-----------------------------------------------------------------
