%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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

%% There are some environment variables that can be used to "manipulate"
%% the test suite: 
%%
%% Variable that controls which 'groups' are to run (with default values)
%%
%%         ESOCK_TEST_REG:         include
%%         ESOCK_TEST_MON:         include
%%         ESOCK_TEST_IOCTL:       include
%%         ESOCK_TEST_SOCK_CLOSE:  include
%%         ESOCK_TEST_TICKETS:     include
%%
%% Variable that controls "verbosity" of the test case(s):
%%
%%         ESOCK_TEST_QUIET: true (default) | false
%%

%% Run the entire test suite: 
%% ts:run(kernel, socket_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(kernel, socket_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(kernel, socket_SUITE, foo, [batch]).
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%%
%% S = fun() -> ts:run(kernel, socket_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% G = fun(GROUP) -> ts:run(kernel, socket_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% T = fun(TC) -> ts:run(kernel, socket_SUITE, TC, [batch]) end.
%%
%% S = fun() -> ct:run_test([{suite, socket_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, socket_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ct:run_test([{suite, socket_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%
%% Some official info about AF_UNIX
%% https://devblogs.microsoft.com/commandline/windowswsl-interop-with-af_unix/



-module(socket_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("socket_test_evaluator.hrl").
-include("kernel_test_lib.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% Socket Registry
         reg_s_single_open_and_close_and_count/1,
         reg_s_optional_open_and_close_and_count/1,


         %% Socket Monitor
         monitor_simple_open_and_close/1,
	 monitor_simple_open_and_exit/1,
	 monitor_simple_open_and_demon_and_close/1,
	 monitor_open_and_close_multi_socks/1,
	 monitor_open_and_exit_multi_socks/1,
	 monitor_open_and_demon_and_close_multi_socks/1,
	 monitor_open_and_close_multi_mon/1,
	 monitor_open_and_exit_multi_mon/1,
	 monitor_open_and_close_multi_socks_and_mon/1,
	 monitor_open_and_exit_multi_socks_and_mon/1,
	 monitor_closed_socket/1,

         %% *** Socket Closure ***
         sc_cpe_socket_cleanup_tcp4/1,
         sc_cpe_socket_cleanup_tcp6/1,
         sc_cpe_socket_cleanup_tcpL/1,
         sc_cpe_socket_cleanup_udp4/1,
         sc_cpe_socket_cleanup_udp6/1,
         sc_cpe_socket_cleanup_udpL/1,

         sc_lc_recv_response_tcp4/1,
         sc_lc_recv_response_tcp6/1,
         sc_lc_recv_response_tcpL/1,
         sc_lc_recvfrom_response_udp4/1,
         sc_lc_recvfrom_response_udp6/1,
         sc_lc_recvfrom_response_udpL/1,
         sc_lc_recvmsg_response_tcp4/1,
         sc_lc_recvmsg_response_tcp6/1,
         sc_lc_recvmsg_response_tcpL/1,
         sc_lc_recvmsg_response_udp4/1,
         sc_lc_recvmsg_response_udp6/1,
         sc_lc_recvmsg_response_udpL/1,
         sc_lc_acceptor_response_tcp4/1,
         sc_lc_acceptor_response_tcp6/1,
         sc_lc_acceptor_response_tcpL/1,

         sc_rc_recv_response_tcp4/1,
         sc_rc_recv_response_tcp6/1,
         sc_rc_recv_response_tcpL/1,
         sc_rc_recvmsg_response_tcp4/1,
         sc_rc_recvmsg_response_tcp6/1,
         sc_rc_recvmsg_response_tcpL/1,

         sc_rs_recv_send_shutdown_receive_tcp4/1,
         sc_rs_recv_send_shutdown_receive_tcp6/1,
         sc_rs_recv_send_shutdown_receive_tcpL/1,
         sc_rs_recvmsg_send_shutdown_receive_tcp4/1,
         sc_rs_recvmsg_send_shutdown_receive_tcp6/1,
         sc_rs_recvmsg_send_shutdown_receive_tcpL/1,

         %% Socket IOCTL simple
         ioctl_simple1/1,
         ioctl_simple2/1,
         ioctl_nread/1,
         %% Socket IOCTL get requests
         ioctl_get_gifname/1,
         ioctl_get_gifindex/1,
         ioctl_get_gifaddr/1,
         ioctl_get_gifdstaddr/1,
         ioctl_get_gifbrdaddr/1,
         ioctl_get_gifnetmask/1,
         ioctl_get_gifmtu/1,
         ioctl_get_gifhwaddr/1,
         ioctl_get_giftxqlen/1,
         ioctl_get_gifflags/1,
         ioctl_get_gifmap/1,
         ioctl_tcp_info/1,
         %% ioctl_set_requests/1,

         %% Tickets
         otp16359_maccept_tcp4/1,
         otp16359_maccept_tcp6/1,
         otp16359_maccept_tcpL/1,
         otp18240_accept_mon_leak_tcp4/1,
         otp18240_accept_mon_leak_tcp6/1,
         otp18635/1,
         otp19063/1,
         otp19251/1,
         otp19469_read_all/1, otp19469_read_part/1
        ]).


%% Internal exports
%% -export([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SLIB,       socket_test_lib).
-define(KLIB,       kernel_test_lib).
-define(LOGGER,     socket_test_logger).

-define(DATA,       <<"HOPPSAN">>). % Temporary
-define(FAIL(R),    exit(R)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    Groups = [{reg,          "ESOCK_TEST_REG",        include},
              {monitor,      "ESOCK_TEST_MON",        include},
              {ioctl,        "ESOCK_TEST_IOCTL",      include},
	      {socket_close, "ESOCK_TEST_SOCK_CLOSE", include},
	      {tickets,      "ESOCK_TEST_TICKETS",    include}],
    [use_group(Group, Env, Default) || {Group, Env, Default} <- Groups].

use_group(_Group, undefined, exclude) ->
    [];
use_group(Group, undefined, _Default) ->
    [{group, Group}];
use_group(Group, Env, Default) ->
	case os:getenv(Env) of
	    false when (Default =:= include) ->
		[{group, Group}];
	    false ->
		[];
	    Val ->
		case list_to_atom(string:to_lower(Val)) of
		    Use when (Use =:= include) orelse 
			     (Use =:= enable) orelse 
			     (Use =:= true) ->
			[{group, Group}];
		    _ ->
			[]
		end
	end.
    

groups() -> 
    [{reg,                         [], reg_simple_cases()},

     {monitor,                     [], monitor_cases()},

     {socket_close,                [], socket_close_cases()},
     {sc_ctrl_proc_exit,           [], sc_cp_exit_cases()},
     {sc_local_close,              [], sc_lc_cases()},
     {sc_remote_close,             [], sc_rc_cases()},
     {sc_remote_shutdown,          [], sc_rs_cases()},

     {ioctl,                       [], ioctl_cases()},
     {ioctl_simple,                [], ioctl_simple_cases()},
     {ioctl_get,                   [], ioctl_get_cases()},
     {ioctl_set,                   [], ioctl_set_cases()},

     %% Ticket groups
     {tickets,                     [], tickets_cases()},
     {otp16359,                    [], otp16359_cases()},
     {otp18240,                    [], otp18240_cases()},
     {otp19469,                    [], otp19469_cases()}
    ].
     
%% Socket Registry "simple" test cases
reg_simple_cases() ->
    [
     reg_s_single_open_and_close_and_count,
     reg_s_optional_open_and_close_and_count
    ].


%% Socket monitor test cases
monitor_cases() ->
    [
     monitor_simple_open_and_close,
     monitor_simple_open_and_exit,
     monitor_simple_open_and_demon_and_close,
     monitor_open_and_close_multi_socks,
     monitor_open_and_exit_multi_socks,
     monitor_open_and_demon_and_close_multi_socks,
     monitor_open_and_close_multi_mon,
     monitor_open_and_exit_multi_mon,
     monitor_open_and_close_multi_socks_and_mon,
     monitor_open_and_exit_multi_socks_and_mon,
     monitor_closed_socket
    ].


%% These cases tests what happens when the socket is closed/shutdown,
%% locally or remotely.
socket_close_cases() ->
    [
     {group, sc_ctrl_proc_exit},
     {group, sc_local_close},
     {group, sc_remote_close},
     {group, sc_remote_shutdown}
    ].

%% These cases are all about socket cleanup after the controlling process
%% exits *without* explicitly calling socket:close/1.
sc_cp_exit_cases() ->
    [
     sc_cpe_socket_cleanup_tcp4,
     sc_cpe_socket_cleanup_tcp6,
     sc_cpe_socket_cleanup_tcpL,
     sc_cpe_socket_cleanup_udp4,
     sc_cpe_socket_cleanup_udp6,
     sc_cpe_socket_cleanup_udpL
    ].

%% These cases tests what happens when the socket is closed locally.
sc_lc_cases() ->
    [
     sc_lc_recv_response_tcp4,
     sc_lc_recv_response_tcp6,
     sc_lc_recv_response_tcpL,

     sc_lc_recvfrom_response_udp4,
     sc_lc_recvfrom_response_udp6,
     sc_lc_recvfrom_response_udpL,

     sc_lc_recvmsg_response_tcp4,
     sc_lc_recvmsg_response_tcp6,
     sc_lc_recvmsg_response_tcpL,
     sc_lc_recvmsg_response_udp4,
     sc_lc_recvmsg_response_udp6,
     sc_lc_recvmsg_response_udpL,

     sc_lc_acceptor_response_tcp4,
     sc_lc_acceptor_response_tcp6,
     sc_lc_acceptor_response_tcpL
    ].

%% These cases tests what happens when the socket is closed remotely.
sc_rc_cases() ->
    [
     sc_rc_recv_response_tcp4,
     sc_rc_recv_response_tcp6,
     sc_rc_recv_response_tcpL,

     sc_rc_recvmsg_response_tcp4,
     sc_rc_recvmsg_response_tcp6,
     sc_rc_recvmsg_response_tcpL
    ].

%% These cases tests what happens when the socket is shutdown/closed remotely
%% after writing and reading is ongoing.
sc_rs_cases() ->
    [
     sc_rs_recv_send_shutdown_receive_tcp4,
     sc_rs_recv_send_shutdown_receive_tcp6,
     sc_rs_recv_send_shutdown_receive_tcpL,

     sc_rs_recvmsg_send_shutdown_receive_tcp4,
     sc_rs_recvmsg_send_shutdown_receive_tcp6,
     sc_rs_recvmsg_send_shutdown_receive_tcpL
    ].


ioctl_cases() ->
    [
     {group, ioctl_simple},
     {group, ioctl_get},
     {group, ioctl_set}
    ].


ioctl_simple_cases() ->
    [
     ioctl_simple1,
     ioctl_simple2,
     ioctl_nread
    ].


ioctl_get_cases() ->
    [
     ioctl_get_gifname,
     ioctl_get_gifindex,
     ioctl_get_gifaddr,
     ioctl_get_gifdstaddr,
     ioctl_get_gifbrdaddr,
     ioctl_get_gifnetmask,
     ioctl_get_gifmtu,
     ioctl_get_gifhwaddr,
     ioctl_get_giftxqlen,
     ioctl_get_gifflags,
     ioctl_get_gifmap,
     ioctl_tcp_info
    ].


ioctl_set_cases() ->
    [
    ].


tickets_cases() ->
    [
     {group, otp16359},
     {group, otp18240},
     otp18635,
     otp19063,
     otp19251,
     {group, otp19469}
    ].

otp16359_cases() ->
    [
     otp16359_maccept_tcp4,
     otp16359_maccept_tcp6,
     otp16359_maccept_tcpL
    ].


otp18240_cases() ->
    [
     otp18240_accept_mon_leak_tcp4,
     otp18240_accept_mon_leak_tcp6
    ].


otp19469_cases() ->
    [
     otp19469_read_all,
     otp19469_read_part
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config0) ->
    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),
    
    try socket:info() of
        #{} ->
            case ?KLIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

                    ?ENSURE_NOT_DOG_SLOW(Config1, 15),

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    socket:use_registry(false),
                    case quiet_mode(Config1) of
                        default ->
                            case ?LOGGER:start() of
                                ok ->
                                    Config1;
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end;
                        Quiet ->
                            case ?LOGGER:start(Quiet) of
                                ok ->
                                    [{esock_test_quiet, Quiet} | Config1];
                                {error, Reason} ->
                                    ?P("init_per_suite -> "
                                       "Failed starting logger"
                                       "~n   Reason: ~p"
                                       "~n", [Reason]),
                                    {skip, "Failed starting logger"}
                            end
                    end
            end
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    (catch ?LOGGER:stop()),

    Config1 = ?KLIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


init_per_group(GroupName, Config)
  when (GroupName =:= sc_remote_close) orelse
       (GroupName =:= sc_remote_shutdown) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [GroupName, Config]),
    %% Maybe we should skip the entire suite for this platform,
    %% but for now we just skip these groups, which seem to 
    %% have problems (node start).
    %% As stated elsewhere, its not really Fedora 16, but 
    %% the *really* slow VM that is the issue.
    try is_old_fedora16() of
        ok ->
            Config
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end;
init_per_group(GroupName, Config)
  when (GroupName =:= ioctl) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config:"
              "~n      ~p"
              "~nwhen"
              "~n   Supported IOCtl Requests: "
              "~n~s"
              "~n   Supported IOCtl Flags: "
              "~n~s"
              "~n", [GroupName, Config,
                     format_ioctls(socket:supports(ioctl_requests), 6),
                     format_ioctls(socket:supports(ioctl_flags), 6)]),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

format_ioctls(Reqs, PrefixSz) ->
    Max = format_ioctls_max(Reqs),
    format_ioctls(Reqs, string:pad("", PrefixSz), Max).

format_ioctls_max(Reqs) ->
    format_ioctls_max(Reqs, 0).

format_ioctls_max([], Max) ->
    Max;
format_ioctls_max([{Req, _}|Reqs], Max) when is_atom(Req) ->
    ReqStr = atom_to_list(Req),
    case length(ReqStr)+1 of
        NewMax when (NewMax > Max) ->
            format_ioctls_max(Reqs, NewMax);
        _ ->
            format_ioctls_max(Reqs, Max)
    end.

format_ioctls(IOCtls, Prefix, Max) ->
    format_ioctls(IOCtls, Prefix, Max, []).

format_ioctls([], _Prefix, _Max, Acc) ->
    lists:flatten(lists:reverse(Acc));
format_ioctls([{Key, Sup}|IOCtls], Prefix, Max, Acc) ->
    format_ioctls(IOCtls, Prefix, Max,
                  [format_ioctl(Key, Sup, Prefix, Max) | Acc]).

format_ioctl(Key, Sup, Prefix, Max) ->
    KeyStr  = io_lib:format("~w:", [Key]),
    KeyStr2 = lists:flatten(string:pad(KeyStr, Max, trailing, " ")),
    lists:flatten(io_lib:format("~s~s ~w~n", [Prefix, KeyStr2, Sup])).


init_per_testcase(_TC, Config) ->
    io:format("init_per_testcase(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_TC, Config]),
    Config.

end_per_testcase(_TC, Config) ->
    Config.


quiet_mode(Config) ->
    case lists:keysearch(esock_test_quiet, 1, Config) of
        {value, {esock_test_quiet, Quiet}} ->
            Quiet;
        false ->
            case os:getenv("ESOCK_TEST_QUIET") of
                "true"  -> true;
                "false" -> false;
                _       -> default
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                             REGISTRY                                %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We create a bunch of different sockets and ensure that the registry
%% has the correct info.

reg_s_single_open_and_close_and_count(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(reg_s_single_open_and_close_and_count,
           fun() ->
                   ok = reg_s_single_open_and_close_and_count()
           end).


reg_s_single_open_and_close_and_count() ->
    socket:use_registry(true),

    {OS, _} = os:type(),

    %% We may have some sockets already existing.
    %% Make sure we dont count them when we test.
    Existing = socket:which_sockets(),
    N = length(Existing),
    SupportsIPV6 =
        case (catch has_support_ipv6()) of
            ok ->
                true;
            _ ->
                false
        end,
    SupportsLOCAL =
        case (catch has_support_unix_domain_socket()) of
            ok ->
                true;
            _ ->
                false
        end,
    SupportsSCTP =
        case (catch has_support_sctp()) of
            ok ->
                true;
            _ ->
                false
        end,
    InitSockInfos =
        [
         {inet, stream, tcp},
         {inet, dgram,  udp}
        ] ++
        case SupportsIPV6 of
            true ->
                [
                 {inet6, stream, tcp},
                 {inet6, dgram,  udp}
                ];
            false ->
                []
        end ++
        case SupportsLOCAL of
            true when (OS =/= win32) ->
                [
                 {local, stream, default},
                 {local, dgram,  default}
                ];
            true ->
                [
                 {local, stream, default}
                ];
            false ->
                []
        end ++
        [
         {inet, stream, tcp},
         {inet, dgram,  udp}
        ] ++
        case SupportsSCTP of
            true ->
                %% On some platforms this is not enough,
                %% we need to actually check this "by doing it"...
                ?P("test open sctp socket"),
                case socket:open(inet,
                                 seqpacket,
                                 sctp,
                                 #{use_registry => false}) of
                    {ok, S} ->
                        ?P("test open sctp socket: success"),
                        (catch socket:close(S)),
                        [
                         {inet, seqpacket, sctp},
                         {inet, seqpacket, sctp}
                        ];
                    {error, _} ->
                        ?P("test open sctp socket: failed"),
                        []
                end;
            false ->
                []
        end ++
        [
         {inet, stream, tcp},
         {inet, dgram,  udp}
        ] ++
        case SupportsSCTP andalso SupportsIPV6 of
            true ->
                %% On some platforms this is not enough,
                %% we need to actually check this "by doing it"...
                ?P("test open sctp socket"),
                case socket:open(inet6,
                                 seqpacket,
                                 sctp,
                                 #{use_registry => false}) of
                    {ok, S6} ->
                        ?P("test open sctp socket: success"),
                        (catch socket:close(S6)),
                        [
                         {inet6, seqpacket, sctp},
                         {inet6, seqpacket, sctp}
                        ];
                    {error, _} ->
                        ?P("test open sctp socket: failed"),
                        []
                end;
            false ->
                []
        end,

    i("open sockets"),
    Socks =
        [fun({Domain, Type, Proto}) ->
                 i("open socket: ~w, ~w, ~w", [Domain, Type, Proto]),
                 {ok, Sock} = socket:open(Domain, Type, Proto),
                 Sock
         end(InitSockInfo) || InitSockInfo <- InitSockInfos],

    ?SLEEP(1000),


    %% **** Total Number Of Sockets ****

    NumSocks1 = N + length(Socks),
    NumberOf1 = socket:number_of(),

    i("verify (total) number of sockets(1): ~w, ~w",
      [NumSocks1, NumberOf1]),
    case (NumSocks1 =:= NumberOf1) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_sockets1, {NumSocks1, NumberOf1})
    end,


    %% **** Number Of IPv4 TCP Sockets ****

    %% inet, stream, tcp
    SiNumTCP = reg_si_num(InitSockInfos, inet, stream, tcp),
    SrNumTCP = reg_sr_num(Existing, inet, stream, tcp),

    i("verify number of IPv4 TCP sockets: ~w, ~w", [SiNumTCP, SrNumTCP]),
    case (SiNumTCP =:= SrNumTCP) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_ipv4_tcp_sockets, {SiNumTCP, SrNumTCP})
    end,


    %% **** Number Of IPv4 UDP Sockets ****

    %% inet, dgram, udp
    SiNumUDP = reg_si_num(InitSockInfos, inet, dgram, udp),
    SrNumUDP = reg_sr_num(Existing, inet, dgram, udp),

    i("verify number of IPv4 UDP sockets: ~w, ~w", [SiNumUDP, SrNumUDP]),
    case (SiNumUDP =:= SrNumUDP) of
        true ->
            ok;
        false ->
            exit({wrong_number_of_ipv4_udp_sockets, SiNumUDP, SrNumUDP})
    end,


    %% **** Number Of IPv4 SCTP Sockets ****

    %% inet, seqpacket, sctp
    SiNumSCTP = reg_si_num(InitSockInfos, inet, seqpacket, sctp),
    SrNumSCTP = reg_sr_num(Existing, inet, seqpacket, sctp),

    i("verify number of IPv4 SCTP sockets: ~w, ~w", [SiNumSCTP, SrNumSCTP]),
    case (SiNumSCTP =:= SrNumSCTP) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_sctp_sockets, {SiNumSCTP, SrNumSCTP})
    end,


    %% **** Number Of IPv4 Sockets ****

    %% inet
    SiNumINET = reg_si_num(InitSockInfos, inet),
    SrNumINET = reg_sr_num(Existing, inet),

    i("verify number of IPv4 sockets: ~w, ~w", [SiNumINET, SrNumINET]),
    case (SiNumINET =:= SrNumINET) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_ipv4_sockets, {SiNumINET, SrNumINET})
    end,


    %% **** Number Of IPv6 Sockets ****

    %% inet6
    SiNumINET6 = reg_si_num(InitSockInfos, inet6),
    SrNumINET6 = reg_sr_num(Existing, inet6),

    i("verify number of IPv6 sockets: ~w, ~w", [SiNumINET6, SrNumINET6]),
    case (SiNumINET6 =:= SrNumINET6) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_ipv6_sockets, {SiNumINET6, SrNumINET6})
    end,


    %% **** Number Of Unix Domain Sockets Sockets ****

    %% local
    SiNumLOCAL = reg_si_num(InitSockInfos, local),
    SrNumLOCAL = reg_sr_num(Existing, local),

    i("verify number of Unix Domain Sockets sockets: ~w, ~w",
      [SiNumLOCAL, SrNumLOCAL]),
    case (SiNumLOCAL =:= SrNumLOCAL) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_local_sockets, {SiNumLOCAL, SrNumLOCAL})
    end,


    %% **** Close *all* Sockets then verify Number Of Sockets ****

    i("close sockets"),
    lists:foreach(fun(S) ->
                          i("close socket"),                          
                          ok = socket:close(S)
                  end, Socks),

    ?SLEEP(1000),

    NumberOf2 = socket:number_of(),

    i("verify number of sockets(2): ~w, ~w", [N, NumberOf2]),
    case (N =:= NumberOf2) of
        true ->
            ok;
        false ->
            reg_si_fail(wrong_number_of_sockets2, {N, NumberOf2})
    end,


    i("verify owner sockets (none)", []),
    Expected1 = [],
    case socket:which_sockets(self()) of
        Expected1 ->
            ok;
        Unexpected1 ->
            reg_si_fail(wrong_sockets_own1, {Expected1, Unexpected1})
    end,

    i("create some sockets", []),
    OwnSockets = lists:sort(
                   [fun({D, T})->
                            i("create ~w:~w socket", [D, T]),
                            {ok, OSocks} = socket:open(D, T, default),
                            OSocks
                    end(SockInfo) || SockInfo <-
                                         [{inet, dgram},
                                          {inet, dgram},
                                          {inet, stream},
                                          {inet, stream}]]),

    i("verify owner sockets (~w)", [length(OwnSockets)]),
    case lists:sort(socket:which_sockets(self())) of
        OwnSockets ->
            ok;
        Unexpected2 ->
            reg_si_fail(wrong_sockets_own2, {OwnSockets, Unexpected2})
    end,

    i("close (own) sockets"),
    lists:foreach(fun(S) ->
                          i("close socket"),                          
                          ok = socket:close(S)
                  end, OwnSockets),
    ?SLEEP(1000),

    i("verify number of sockets(2) (again)"),
    NumberOf2 = socket:number_of(),

    i("verify pre-existing sockets(2)", []),
    case socket:which_sockets() of
        Existing ->
            ok;
        OtherSockets ->
            reg_si_fail(wrong_sockets2, {Existing, OtherSockets})
    end,

    socket:use_registry(false),
    ok.


reg_si_fail(Reason, Extra) ->
    socket:use_registry(false),
    exit({Reason, Extra}).

reg_si_num(SocksInfo, Domain)
  when ((Domain =:= inet) orelse (Domain =:= inet6) orelse (Domain =:= local)) ->
    reg_si_num(SocksInfo, Domain, undefined, undefined);
reg_si_num(SocksInfo, Type)
  when ((Type =:= stream) orelse (Type =:= dgram) orelse (Type =:= seqpacket)) ->
    reg_si_num(SocksInfo, undefined, Type, undefined);
reg_si_num(SocksInfo, Proto)
  when ((Proto =:= sctp) orelse (Proto =:= tcp) orelse (Proto =:= udp)) ->
    reg_si_num(SocksInfo, undefined, undefined, Proto).

reg_si_num(SocksInfo, Domain, undefined, undefined) ->
    F = fun({D, _T, _P}) when (D =:= Domain) -> true;
           (_) -> false
        end,
    reg_si_num2(F, SocksInfo);
reg_si_num(SocksInfo, undefined, Type, undefined) ->
    F = fun({_D, T, _P}) when (T =:= Type) -> true;
           (_) -> false
        end,
    reg_si_num2(F, SocksInfo);
reg_si_num(SocksInfo, undefined, undefined, Proto) ->
    F = fun({_D, _T, P}) when (P =:= Proto) -> true;
           (_) -> false
        end,
    reg_si_num2(F, SocksInfo);
reg_si_num(SocksInfo, Domain, Type, Proto) ->
    F = fun({D, T, P}) when (D =:= Domain) andalso
                            (T =:= Type) andalso
                            (P =:= Proto) ->
                true;
           (_) ->
                false
        end,
    reg_si_num2(F, SocksInfo).

reg_si_num2(F, SocksInfo) ->
    length(lists:filter(F, SocksInfo)).


reg_sr_num(Existing, Domain)
  when ((Domain =:= inet) orelse (Domain =:= inet6)) ->
    length(socket:which_sockets(Domain) -- Existing);
reg_sr_num(Existing, Domain)
  when (Domain =:= local) ->
    reg_sr_num(Existing, Domain, undefined, undefined);
reg_sr_num(Existing, Type)
  when ((Type =:= stream) orelse (Type =:= dgram) orelse (Type =:= seqpacket)) ->
    length(socket:which_sockets(Type) -- Existing);
reg_sr_num(Existing, Proto)
  when ((Proto =:= sctp) orelse (Proto =:= tcp) orelse (Proto =:= udp)) ->
    length(socket:which_sockets(Proto) -- Existing).

reg_sr_num(Existing, Domain, undefined, undefined) ->
    F = fun(#{domain := D}) when (D =:= Domain) ->
                true;
           (_X) ->
                false
        end,
    reg_sr_num2(Existing, F);
reg_sr_num(Existing, Domain, Type, Proto) ->
    F = fun(#{domain   := D,
              type     := T,
              protocol := P}) when (D =:= Domain) andalso
                                   (T =:= Type) andalso
                                   (P =:= Proto) ->
                true;
           (_X) ->
                %% i("reg_sr_num -> not counting: "
                %%   "~n   ~p", [_X]),
                false
        end,
    reg_sr_num2(Existing, F).

reg_sr_num2(Existing, F) ->
    length(socket:which_sockets(F) -- Existing).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We create a bunch of different sockets and ensure that the registry
%% has the correct info.

reg_s_optional_open_and_close_and_count(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(reg_s_optional_open_and_close_and_count,
           fun() ->
                   ok = reg_s_optional_open_and_close_and_count()
           end).


reg_s_optional_open_and_close_and_count() ->
    i("Make sure use of socket registry is enabled (regardless of default)"),
    socket:use_registry(true),
    #{use_registry := true} = socket:info(),

    i("get current socket base count"),
    Base = socket:number_of(),

    i("create a socket and ensure its counted"),
    {ok, S1} = socket:open(inet, dgram, udp),
    Base1 = Base + 1,
    case socket:number_of() of
        Base1 ->
            ok;
        Invalid1 -> 
            exit({wrong_number_of_sockets1, Invalid1, Base + 1})
    end,
    i("close the socket and ensure its counted (back to base)"),
    ok = socket:close(S1),
    case socket:number_of() of
        Base ->
            ok;
        Invalid2 -> 
            exit({wrong_number_of_sockets2, Invalid2, Base})
    end,

    i("create a socket with use_registry explicitly off "
      "and ensure its not counted"),
    {ok, S2} = socket:open(inet, dgram, udp, #{use_registry => false}),
    case socket:number_of() of
        Base ->
            ok;
        Invalid3 -> 
            exit({wrong_number_of_sockets3, Invalid3, Base})
    end,
    i("close the socket and ensure its not counted"),
    ok = socket:close(S2),
    case socket:number_of() of
        Base ->
            ok;
        Invalid4 -> 
            exit({wrong_number_of_sockets4, Invalid4, Base})
    end,

    i("Globally disable use of registry"),
    socket:use_registry(false),
    #{use_registry := false} = socket:info(),
    i("create a socket and ensure its not counted"),
    {ok, S3} = socket:open(inet, dgram, udp),
    case socket:number_of() of
        Base ->
            ok;
        Invalid5 -> 
            exit({wrong_number_of_sockets5, Invalid5, Base})
    end,
    i("close the socket and ensure its not counted"),
    ok = socket:close(S3),
    case socket:number_of() of
        Base ->
            ok;
        Invalid6 -> 
            exit({wrong_number_of_sockets6, Invalid6, Base})
    end,

    i("create a socket with use_registry explicitly on "
      "and ensure its counted"),
    {ok, S4} = socket:open(inet, dgram, udp, #{use_registry => true}),
    case socket:number_of() of
        Base1 ->
            ok;
        Invalid7 -> 
            exit({wrong_number_of_sockets7, Invalid7, Base + 1})
    end,
    i("close the socket and ensure counted (back to base)"),
    ok = socket:close(S4),
    case socket:number_of() of
        Base ->
            ok;
        Invalid8 -> 
            exit({wrong_number_of_sockets8, Invalid8, Base})
    end,

    socket:use_registry(false),
    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                       SOCKET MONITOR                                %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create one socket, monitor from a different process then close socket.
%% The process that did the monitor shall receive a socket DOWN.

monitor_simple_open_and_close(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_simple_open_and_close,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_simple_open_and_close(InitState)
           end).


mon_simple_open_and_close(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock, State)}
                   end},

         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket - create two",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef1 = socket:monitor(Sock),
                           MRef2 = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitor(s): "
				       "~n   1: ~p"
				       "~n   2: ~p", [MRef1, MRef2]),
			   {ok, State#{mon1 => MRef1, mon2 => MRef2}}
                   end},
         #{desc => "verify total number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "verify which monitors - (self) two",
           cmd  => fun(#{mon1 := MRef1,
			 mon2 := MRef2} = _State) ->
			   Mons = lists:sort([MRef1, MRef2]),
			   case lists:sort(socket:which_monitors(self())) of
			       Mons ->
				   ok;
			       SMons ->
				   ?SEV_EPRINT("Unexpected (self) monitors: "
					       "~n   Expected: ~p"
					       "~n   Actual:   ~p",
					       [Mons, SMons]),
				   {error, unexpected_monitors}
			   end
                   end},
         #{desc => "verify which monitors - (sock) two",
           cmd  => fun(#{sock := Sock,
			 mon1 := MRef1,
			 mon2 := MRef2} = _State) ->
			   Mons = lists:sort([MRef1, MRef2]),
			   case lists:sort(socket:which_monitors(Sock)) of
			       Mons ->
				   ok;
			       SMons ->
				   ?SEV_EPRINT("Unexpected (sock) monitors: "
					       "~n   Expected: ~p"
					       "~n   Actual:   ~p",
					       [Mons, SMons]),
				   {error, unexpected_monitors}
			   end
                   end},
         #{desc => "verify monitored by - only us",
           cmd  => fun(#{sock := Sock} = _State) ->
			   Self   = self(),
			   [Self] = socket:monitored_by(Sock),
			   ok
                   end},

         %% The actual test
         #{desc => "order owner to close",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await socket down 1",
           cmd  => fun(#{sock := Sock,
			 mon1 := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~s"
					       "~n      Info:   ~p",
					       [MRef,
						socket:to_list(Sock),
						Info]),
				   {ok, maps:remove(mon1, State)}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "await socket down 2",
           cmd  => fun(#{sock := Sock,
			 mon2  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~s"
					       "~n      Info:   ~p",
					       [MRef,
						socket:to_list(Sock),
						Info]),
				   {ok, maps:remove(mon2, State)}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "verify which monitors - only one",
           cmd  => fun(#{sock := Sock} = _State) ->
			   [] = socket:which_monitors(self()),
			   [] = socket:which_monitors(Sock),
			   ok
                   end},
         #{desc => "verify monitored by - none",
           cmd  => fun(#{sock := Sock} = _State) ->
			   [] = socket:monitored_by(Sock),
			   ok
                   end},

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},

         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create one socket, monitor from a different process then stop the
%% owner process.
%% The process that did the monitor shall receive a socket DOWN.

monitor_simple_open_and_exit(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_simple_open_and_exit,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_simple_open_and_exit(InitState)
           end).


mon_simple_open_and_exit(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitor: ~p", [MRef]),
			   {ok, State#{mon => MRef}}
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "verify which monitors - only one",
           cmd  => fun(#{sock := Sock,
			 mon  := MRef} = _State) ->
			   [MRef] = socket:which_monitors(self()),
			   [MRef] = socket:which_monitors(Sock),
			   ok
                   end},
         #{desc => "verify monitored by - only us",
           cmd  => fun(#{sock := Sock} = _State) ->
			   Self   = self(),
			   [Self] = socket:monitored_by(Sock),
			   ok
                   end},

         %% The actual test
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await socket down",
           cmd  => fun(#{sock := Sock,
			 mon  := MRef} = _State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   ok
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "verify which monitors - only one",
           cmd  => fun(#{sock := Sock} = _State) ->
			   [] = socket:which_monitors(self()),
			   [] = socket:which_monitors(Sock),
			   ok
                   end},
         #{desc => "verify monitored by - none",
           cmd  => fun(#{sock := Sock} = _State) ->
			   [] = socket:monitored_by(Sock),
			   ok
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create one socket, monitor from a different process, cancel_montor
%% (demonitor) and then close socket.
%% The process that did the monitor shall *not* receive a socket DOWN.

monitor_simple_open_and_demon_and_close(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_simple_open_and_demon_and_close,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_simple_open_and_demon_and_close(InitState)
           end).


mon_simple_open_and_demon_and_close(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock, State)}
                   end},

         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitor: ~p", [MRef]),
			   {ok, State#{mon => MRef}}
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% The actual test
         #{desc => "demonitor socket",
           cmd  => fun(#{mon := MRef} = State) ->
                           true = socket:cancel_monitor(MRef),
			   {ok, maps:remove(mon, State)}
                   end},

	 #{desc => "verify total number of monitors (=0)",
	   cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
		   end},
	 #{desc => "verify own number of monitors (=0)",
	   cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
		   end},

         #{desc => "order owner to close",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},

         #{desc => "await socket down",
           cmd  => fun(#{sock := Sock} = _State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_EPRINT("received UNEXPECTED down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   {error, unexpected_down}
			   after 5000 ->
				   ?SEV_IPRINT("expected socket down timeout"),
				   ok
			   end
                   end},

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},

         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets, monitor from a different process then close
%% socket. The process that did the monitor shall receive a socket DOWN.

monitor_open_and_close_multi_socks(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_open_and_close_multi_socks,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_close_multi_socks(InitState)
           end).


mon_open_and_close_multi_socks(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket 1",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 4",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock4 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 5",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock5 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock1  := Sock1,
			 sock2  := Sock2,
			 sock3  := Sock3,
			 sock4  := Sock4,
			 sock5  := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_READY(Tester, init, Socks),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close1)
                   end},
         #{desc => "close socket 1",
           cmd  => fun(#{sock1 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock1, State)}
                   end},

         #{desc => "await continue (close2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close2)
                   end},
         #{desc => "close socket 2",
           cmd  => fun(#{sock2 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock2, State)}
                   end},

         #{desc => "await continue (close3)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close3)
                   end},
         #{desc => "close socket 3",
           cmd  => fun(#{sock3 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock3, State)}
                   end},

         #{desc => "await continue (close4)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close4)
                   end},
         #{desc => "close socket 4",
           cmd  => fun(#{sock4 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock4, State)}
                   end},

         #{desc => "await continue (close5)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close5)
                   end},
         #{desc => "close socket 5",
           cmd  => fun(#{sock5 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock5, State)}
                   end},

         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock1 := Sock1,
			 sock2 := Sock2,
			 sock3 := Sock3,
			 sock4 := Sock4,
			 sock5 := Sock5} = State) ->
                           MRef1 = socket:monitor(Sock1),
                           MRef2 = socket:monitor(Sock2),
                           MRef3 = socket:monitor(Sock3),
                           MRef4 = socket:monitor(Sock4),
                           MRef5 = socket:monitor(Sock5),
			   ?SEV_IPRINT("Monitors:"
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [MRef1, MRef2, MRef3, MRef4, MRef5]),
			   {ok, State#{mon1 => MRef1,
				       mon2 => MRef2,
				       mon3 => MRef3,
				       mon4 => MRef4,
				       mon5 => MRef5}}
                   end},
         #{desc => "verify total number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% The actual test
         #{desc => "order owner to close socket 1",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close1),
                           ok
                   end},
         #{desc => "await socket 1 down",
           cmd  => fun(#{sock1 := Sock,
			 mon1  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~s"
					       "~n      Info:   ~p",
					       [MRef,
						socket:to_list(Sock),
						Info]),
				   State2 = maps:remove(sock1, State),
				   State3 = maps:remove(mon1,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=4)",
           cmd  => fun(_State) ->
			   4 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=4)",
           cmd  => fun(_State) ->
			   4 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 2",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close2),
                           ok
                   end},
         #{desc => "await socket 2 down",
           cmd  => fun(#{sock2 := Sock,
			 mon2  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~s"
					       "~n      Info:   ~p",
					       [MRef,
						socket:to_list(Sock),
						Info]),
				   State2 = maps:remove(sock2, State),
				   State3 = maps:remove(mon2,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 3",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close3),
                           ok
                   end},
         #{desc => "await socket 3 down",
           cmd  => fun(#{sock3 := Sock,
			 mon3  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~s"
					       "~n      Info:   ~p",
					       [MRef,
						socket:to_list(Sock),
						Info]),
				   State2 = maps:remove(sock3, State),
				   State3 = maps:remove(mon3,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 4",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close4),
                           ok
                   end},
         #{desc => "await socket 4 down",
           cmd  => fun(#{sock4 := Sock,
			 mon4  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock4, State),
				   State3 = maps:remove(mon4,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 5",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close5),
                           ok
                   end},
         #{desc => "await socket 5 down",
           cmd  => fun(#{sock5 := Sock,
			 mon5  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock5, State),
				   State3 = maps:remove(mon5,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},

         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets, monitor from a different process then exit
%% the owner process.
%% The process that did the monitor shall receive a socket DOWN.

monitor_open_and_exit_multi_socks(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_open_and_exit_multi_socks,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_exit_multi_socks(InitState)
           end).


mon_open_and_exit_multi_socks(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket 1",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 4",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock4 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 5",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock5 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock1  := Sock1,
			 sock2  := Sock2,
			 sock3  := Sock3,
			 sock4  := Sock4,
			 sock5  := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_READY(Tester, init, Socks),
                           ok
                   end},


         %% The actual test
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock1 := Sock1,
			 sock2 := Sock2,
			 sock3 := Sock3,
			 sock4 := Sock4,
			 sock5 := Sock5} = State) ->
                           MRef1 = socket:monitor(Sock1),
                           MRef2 = socket:monitor(Sock2),
                           MRef3 = socket:monitor(Sock3),
                           MRef4 = socket:monitor(Sock4),
                           MRef5 = socket:monitor(Sock5),
			   ?SEV_IPRINT("Monitors:"
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [MRef1, MRef2, MRef3, MRef4, MRef5]),
			   {ok, State#{mon1 => MRef1,
				       mon2 => MRef2,
				       mon3 => MRef3,
				       mon4 => MRef4,
				       mon5 => MRef5}}
                   end},
         #{desc => "verify total number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% The actual test
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},

         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await socket 1 down",
           cmd  => fun(#{sock1 := Sock,
			 mon1  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock1, State),
				   State3 = maps:remove(mon1,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "await socket 2 down",
           cmd  => fun(#{sock2 := Sock,
			 mon2  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock2, State),
				   State3 = maps:remove(mon2,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "await socket 3 down",
           cmd  => fun(#{sock3 := Sock,
			 mon3  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock3, State),
				   State3 = maps:remove(mon3,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "await socket 4 down",
           cmd  => fun(#{sock4 := Sock,
			 mon4  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock4, State),
				   State3 = maps:remove(mon4,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "await socket 5 down",
           cmd  => fun(#{sock5 := Sock,
			 mon5  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock5, State),
				   State3 = maps:remove(mon5,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets, monitor from a different process, demonitor
%% two of them then close socket.
%% The process that did the monitor shall receive a socket DOWN for
%% the sockets that are still monitored.

monitor_open_and_demon_and_close_multi_socks(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_open_and_demon_and_close_multi_socks,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_demon_and_close_multi_socks(InitState)
           end).


mon_open_and_demon_and_close_multi_socks(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket 1",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 4",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock4 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 5",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock5 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock1  := Sock1,
			 sock2  := Sock2,
			 sock3  := Sock3,
			 sock4  := Sock4,
			 sock5  := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_READY(Tester, init, Socks),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close1)
                   end},
         #{desc => "close socket 1",
           cmd  => fun(#{sock1 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock1, State)}
                   end},

         #{desc => "await continue (close3)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close3)
                   end},
         #{desc => "close socket 3",
           cmd  => fun(#{sock3 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock3, State)}
                   end},

         #{desc => "await continue (close5)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close5)
                   end},
         #{desc => "close socket 5",
           cmd  => fun(#{sock5 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock5, State)}
                   end},

         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock1 := Sock1,
			 sock2 := Sock2,
			 sock3 := Sock3,
			 sock4 := Sock4,
			 sock5 := Sock5} = State) ->
                           MRef1 = socket:monitor(Sock1),
                           MRef2 = socket:monitor(Sock2),
                           MRef3 = socket:monitor(Sock3),
                           MRef4 = socket:monitor(Sock4),
                           MRef5 = socket:monitor(Sock5),
			   ?SEV_IPRINT("Monitors:"
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [MRef1, MRef2, MRef3, MRef4, MRef5]),
			   {ok, State#{mon1 => MRef1,
				       mon2 => MRef2,
				       mon3 => MRef3,
				       mon4 => MRef4,
				       mon5 => MRef5}}
                   end},
         #{desc => "verify total number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% The actual test
         #{desc => "demonitor socket(s)",
           cmd  => fun(#{mon2 := MRef2,
			 mon4 := MRef4} = State) ->
                           true = socket:cancel_monitor(MRef2),
                           true = socket:cancel_monitor(MRef4),
			   ?SEV_IPRINT("cancel socket monitor 2 and 4"),
			   State2 = maps:remove(mon2,  State),
			   State3 = maps:remove(sock2, State2),
			   State4 = maps:remove(mon4,  State3),
			   State5 = maps:remove(sock4, State4),
			   {ok, State5}
                   end},
         #{desc => "verify total number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "order owner to close socket 1",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close1),
                           ok
                   end},
         #{desc => "await socket 1 down",
           cmd  => fun(#{sock1 := Sock,
			 mon1  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock1, State),
				   State3 = maps:remove(mon1,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 3",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close3),
                           ok
                   end},
         #{desc => "await socket 3 down",
           cmd  => fun(#{sock3 := Sock,
			 mon3  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock3, State),
				   State3 = maps:remove(mon3,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "order owner to close socket 5",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close5),
                           ok
                   end},
         #{desc => "await socket 5 down",
           cmd  => fun(#{sock5 := Sock,
			 mon5  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(sock5, State),
				   State3 = maps:remove(mon5,  State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},

         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create one socket (by 'owner' process), monitor from several different
%% processes, then close socket (from 'owner').
%% The processes that did the monitor shall receive a socket DOWN.

monitor_open_and_close_multi_mon(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_open_and_close_multi_mon,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_close_multi_mon(InitState)
           end).


mon_open_and_close_multi_mon(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock, State)}
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},
         #{desc => "await continue (socket)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, Sock} =
			       ?SEV_AWAIT_CONTINUE(Tester, tester, socket),
			   ?SEV_IPRINT("Socket: ~p", [Sock]),
			   {ok, State#{sock => Sock}}
                   end},


         %% The actual test
         #{desc => "await continue (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, monitor)
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitor: ~p", [MRef]),
			   {ok, State#{mon => MRef}}
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "announce ready (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, monitor),
                           ok
                   end},

         #{desc => "await continue (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, down)
                   end},

         #{desc => "await socket down",
           cmd  => fun(#{sock := Sock,
			 mon  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon, State),
				   State3 = maps:remove(sock, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'owner'",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},

         #{desc => "monitor 'client 1'",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 1) start",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "send socket to client 1",
           cmd  => fun(#{client1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 2'",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 2) start",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "send socket to client 2",
           cmd  => fun(#{client2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 3'",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 3) start",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},
         #{desc => "send socket to client 3",
           cmd  => fun(#{client3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 4'",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 4) start",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, init)
                   end},
         #{desc => "send socket to client 4",
           cmd  => fun(#{client4 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 5'",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 5) start",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, init)
                   end},
         #{desc => "send socket to client 5",
           cmd  => fun(#{client5 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},


         %% The actual test
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to monitor",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, monitor)
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 2 to monitor",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, monitor)
                   end},
         #{desc => "verify total number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 3 to monitor",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, monitor)
                   end},
         #{desc => "verify total number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 4 to monitor",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, monitor)
                   end},
         #{desc => "verify total number of monitors (=4)",
           cmd  => fun(_State) ->
			   4 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 5 to monitor",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, monitor)
                   end},
         #{desc => "verify total number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(),
			   ok
                   end},
         #{desc => "verify monitored by - none",
           cmd  => fun(#{sock    := Sock,
			 client1 := Pid1,
			 client2 := Pid2,
			 client3 := Pid3,
			 client4 := Pid4,
			 client5 := Pid5} = _State) ->
			   Clients = lists:sort([Pid1, Pid2, Pid3, Pid4, Pid5]),
			   case lists:sort(socket:monitored_by(Sock)) of
			       Clients ->
				   ok;
			       SClients ->
				   ?SEV_EPRINT("Unexpected clients: "
					       "~n   Expected: ~p"
					       "~n   Actual:   ~p",
					       [Clients, SClients]),
				   {error, unexpected_clients}
			   end
                   end},

         #{desc => "order client 1 to await down",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 2 to await down",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 3 to await down",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 4 to await down",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 5 to await down",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},

         #{desc => "order owner to close",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},

         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(owner, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 1) terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 1) termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 2) terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 2) termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 3) terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 3) termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 4) terminate",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 4) termination",
           cmd  => fun(#{client4 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client4, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 5) terminate",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 5) termination",
           cmd  => fun(#{client5 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client5, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start client 1 evaluator"),
    Client1 = ?SEV_START("client-1", ClientSeq, InitState),

    i("start client 2 evaluator"),
    Client2 = ?SEV_START("client-2", ClientSeq, InitState),

    i("start client 3 evaluator"),
    Client3 = ?SEV_START("client-3", ClientSeq, InitState),

    i("start client 4 evaluator"),
    Client4 = ?SEV_START("client-4", ClientSeq, InitState),

    i("start client 5 evaluator"),
    Client5 = ?SEV_START("client-5", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner   => Owner#ev.pid,
			client1 => Client1#ev.pid,
			client2 => Client2#ev.pid,
			client3 => Client3#ev.pid,
			client4 => Client4#ev.pid,
			client5 => Client5#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create one socket (by 'owner' process), monitor from several different
%% processes, then close socket (from 'owner').
%% The processes that did the monitor shall receive a socket DOWN.

monitor_open_and_exit_multi_mon(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(monitor_open_and_exit_multi_mon,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_exit_multi_mon(InitState)
           end).


mon_open_and_exit_multi_mon(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},
         #{desc => "await continue (socket)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, Sock} =
			       ?SEV_AWAIT_CONTINUE(Tester, tester, socket),
			   ?SEV_IPRINT("Socket: ~p", [Sock]),
			   {ok, State#{sock => Sock}}
                   end},


         %% The actual test
         #{desc => "await continue (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, monitor)
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitor: ~p", [MRef]),
			   {ok, State#{mon => MRef}}
                   end},
         #{desc => "verify own number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "announce ready (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, monitor),
                           ok
                   end},

         #{desc => "await continue (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, down)
                   end},

         #{desc => "await socket down",
           cmd  => fun(#{sock := Sock,
			 mon  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon, State),
				   State3 = maps:remove(sock, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'owner'",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},

         #{desc => "monitor 'client 1'",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 1) start",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "send socket to client 1",
           cmd  => fun(#{client1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 2'",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 2) start",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "send socket to client 2",
           cmd  => fun(#{client2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 3'",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 3) start",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},
         #{desc => "send socket to client 3",
           cmd  => fun(#{client3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 4'",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 4) start",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, init)
                   end},
         #{desc => "send socket to client 4",
           cmd  => fun(#{client4 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         #{desc => "monitor 'client 5'",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 5) start",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, init)
                   end},
         #{desc => "send socket to client 5",
           cmd  => fun(#{client5 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},


         %% The actual test
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to monitor",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, monitor)
                   end},
         #{desc => "verify total number of monitors (=1)",
           cmd  => fun(_State) ->
			   1 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 2 to monitor",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, monitor)
                   end},
         #{desc => "verify total number of monitors (=2)",
           cmd  => fun(_State) ->
			   2 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 3 to monitor",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, monitor)
                   end},
         #{desc => "verify total number of monitors (=3)",
           cmd  => fun(_State) ->
			   3 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 4 to monitor",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, monitor)
                   end},
         #{desc => "verify total number of monitors (=4)",
           cmd  => fun(_State) ->
			   4 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 5 to monitor",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, monitor)
                   end},
         #{desc => "verify total number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to await down",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 2 to await down",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 3 to await down",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 4 to await down",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 5 to await down",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},


         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(owner, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},

         %% Cleanup
         #{desc => "order (client 1) terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 1) termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 2) terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 2) termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 3) terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 3) termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 4) terminate",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 4) termination",
           cmd  => fun(#{client4 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client4, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 5) terminate",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 5) termination",
           cmd  => fun(#{client5 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client5, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start client 1 evaluator"),
    Client1 = ?SEV_START("client-1", ClientSeq, InitState),

    i("start client 2 evaluator"),
    Client2 = ?SEV_START("client-2", ClientSeq, InitState),

    i("start client 3 evaluator"),
    Client3 = ?SEV_START("client-3", ClientSeq, InitState),

    i("start client 4 evaluator"),
    Client4 = ?SEV_START("client-4", ClientSeq, InitState),

    i("start client 5 evaluator"),
    Client5 = ?SEV_START("client-5", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner   => Owner#ev.pid,
			client1 => Client1#ev.pid,
			client2 => Client2#ev.pid,
			client3 => Client3#ev.pid,
			client4 => Client4#ev.pid,
			client5 => Client5#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets (by 'owner' process), monitor each from several
%% different processes, then close each socket (from 'owner').
%% The processes that did the monitor shall receive one socket DOWN for
%% each socket.

monitor_open_and_close_multi_socks_and_mon(Config) when is_list(Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   FactorKey = kernel_factor,
                   case lists:keysearch(FactorKey, 1, Config) of
                       {value, {FactorKey, Factor}} when (Factor > 15) ->
                           skip("Very slow machine");
                       _ ->
                           ok
                   end
           end,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_close_multi_socks_and_mon(InitState)
           end).


mon_open_and_close_multi_socks_and_mon(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket 1",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 4",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock4 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 5",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock5 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock1  := Sock1,
			 sock2  := Sock2,
			 sock3  := Sock3,
			 sock4  := Sock4,
			 sock5  := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_READY(Tester, init, Socks),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (close1)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close1)
                   end},
         #{desc => "close socket 1",
           cmd  => fun(#{sock1 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock1, State)}
                   end},

         #{desc => "await continue (close2)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close2)
                   end},
         #{desc => "close socket 2",
           cmd  => fun(#{sock2 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock2, State)}
                   end},

         #{desc => "await continue (close3)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close3)
                   end},
         #{desc => "close socket 3",
           cmd  => fun(#{sock3 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock3, State)}
                   end},

         #{desc => "await continue (close4)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close4)
                   end},
         #{desc => "close socket 4",
           cmd  => fun(#{sock4 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock4, State)}
                   end},

         #{desc => "await continue (close5)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close5)
                   end},
         #{desc => "close socket 5",
           cmd  => fun(#{sock5 := Sock} = State) ->
                           ok = socket:close(Sock),
			   {ok, maps:remove(sock5, State)}
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},
         #{desc => "await continue (socket)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_CONTINUE(Tester, tester, socket),
			   ?SEV_IPRINT("Sockets: "
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [Sock1, Sock2, Sock3, Sock4, Sock5]),
			   {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},


         %% The actual test
         #{desc => "await continue (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, monitor)
                   end},

         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock1 := Sock1,
			 sock2 := Sock2,
			 sock3 := Sock3,
			 sock4 := Sock4,
			 sock5 := Sock5} = State) ->
                           MRef1 = socket:monitor(Sock1),
                           MRef2 = socket:monitor(Sock2),
                           MRef3 = socket:monitor(Sock3),
                           MRef4 = socket:monitor(Sock4),
                           MRef5 = socket:monitor(Sock5),
			   ?SEV_IPRINT("Monitors: "
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [MRef1, MRef2, MRef3, MRef4, MRef5]),
			   {ok, State#{mon1 => MRef1,
				       mon2 => MRef2,
				       mon3 => MRef3,
				       mon4 => MRef4,
				       mon5 => MRef5}}
                   end},
         #{desc => "verify own number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(self()),
			   ok
                   end},

         #{desc => "announce ready (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, monitor),
                           ok
                   end},

         #{desc => "await continue (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, down)
                   end},

         #{desc => "await socket 1 down",
           cmd  => fun(#{sock1 := Sock,
			 mon1  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon1, State),
				   State3 = maps:remove(sock1, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},

         #{desc => "await socket 2 down",
           cmd  => fun(#{sock2 := Sock,
			 mon2  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon2, State),
				   State3 = maps:remove(sock2, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},

         #{desc => "await socket 3 down",
           cmd  => fun(#{sock3 := Sock,
			 mon3  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon3, State),
				   State3 = maps:remove(sock3, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},

         #{desc => "await socket 4 down",
           cmd  => fun(#{sock4 := Sock,
			 mon4  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon4, State),
				   State3 = maps:remove(sock4, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},

         #{desc => "await socket 5 down",
           cmd  => fun(#{sock5 := Sock,
			 mon5  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon5, State),
				   State3 = maps:remove(sock5, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'owner'",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},

         #{desc => "monitor 'client 1'",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 1) start",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "send socket to client 1",
           cmd  => fun(#{client1 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 2'",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 2) start",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "send socket to client 2",
           cmd  => fun(#{client2 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 3'",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 3) start",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},
         #{desc => "send socket to client 3",
           cmd  => fun(#{client3 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 4'",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 4) start",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, init)
                   end},
         #{desc => "send socket to client 4",
           cmd  => fun(#{client4 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 5'",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 5) start",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, init)
                   end},
         #{desc => "send socket to client 5",
           cmd  => fun(#{client5 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},


         %% The actual test
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to monitor",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, monitor)
                   end},
         #{desc => "verify total number of monitors (=1*5)",
           cmd  => fun(_State) ->
			   1*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 2 to monitor",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, monitor)
                   end},
         #{desc => "verify total number of monitors (=2*5)",
           cmd  => fun(_State) ->
			   2*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 3 to monitor",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, monitor)
                   end},
         #{desc => "verify total number of monitors (=3*5)",
           cmd  => fun(_State) ->
			   3*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 4 to monitor",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, monitor)
                   end},
         #{desc => "verify total number of monitors (=4*5)",
           cmd  => fun(_State) ->
			   4*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 5 to monitor",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, monitor)
                   end},
         #{desc => "verify total number of monitors (=5*5)",
           cmd  => fun(_State) ->
			   5*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to await down",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 2 to await down",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 3 to await down",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 4 to await down",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 5 to await down",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order owner to close socket 1",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close1),
                           ok
                   end},
         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=5*4)",
           cmd  => fun(_State) ->
			   5*4 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order owner to close socket 2",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close2),
                           ok
                   end},
         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=5*3)",
           cmd  => fun(_State) ->
			   5*3 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order owner to close socket 3",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close3),
                           ok
                   end},
         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=5*2)",
           cmd  => fun(_State) ->
			   5*2 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order owner to close socket 4",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close4),
                           ok
                   end},
         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=5*1)",
           cmd  => fun(_State) ->
			   5*1 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order owner to close socket 5",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close5),
                           ok
                   end},
         #{desc => "await (client 1) down received",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) down received",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) down received",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) down received",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) down received",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},
         #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         %% Cleanup
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(owner, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 1) terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 1) termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 2) terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 2) termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 3) terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 3) termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 4) terminate",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 4) termination",
           cmd  => fun(#{client4 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client4, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 5) terminate",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 5) termination",
           cmd  => fun(#{client5 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client5, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start client 1 evaluator"),
    Client1 = ?SEV_START("client-1", ClientSeq, InitState),

    i("start client 2 evaluator"),
    Client2 = ?SEV_START("client-2", ClientSeq, InitState),

    i("start client 3 evaluator"),
    Client3 = ?SEV_START("client-3", ClientSeq, InitState),

    i("start client 4 evaluator"),
    Client4 = ?SEV_START("client-4", ClientSeq, InitState),

    i("start client 5 evaluator"),
    Client5 = ?SEV_START("client-5", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner   => Owner#ev.pid,
			client1 => Client1#ev.pid,
			client2 => Client2#ev.pid,
			client3 => Client3#ev.pid,
			client4 => Client4#ev.pid,
			client5 => Client5#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets (by 'owner' process), monitor each from several
%% different processes, then exit the owner.
%% The processes that did the monitor shall receive one socket DOWN for
%% each socket.

monitor_open_and_exit_multi_socks_and_mon(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   FactorKey = kernel_factor,
                   case lists:keysearch(FactorKey, 1, Config) of
                       {value, {FactorKey, Factor}} when (Factor > 15) ->
                           skip("Very slow machine");
                       _ ->
                           ok
                   end
           end,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_open_and_exit_multi_socks_and_mon(InitState)
           end).


mon_open_and_exit_multi_socks_and_mon(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket 1",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock1 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 2",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock2 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 3",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock3 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 4",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock4 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "create socket 5",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock5 => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock1  := Sock1,
			 sock2  := Sock2,
			 sock3  := Sock3,
			 sock4  := Sock4,
			 sock5  := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_READY(Tester, init, Socks),
                           ok
                   end},

         %% The actual test
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},
         #{desc => "await continue (socket)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_CONTINUE(Tester, tester, socket),
			   ?SEV_IPRINT("Sockets: "
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [Sock1, Sock2, Sock3, Sock4, Sock5]),
			   {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},


         %% The actual test
         #{desc => "await continue (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, monitor)
                   end},

         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "monitor socket",
           cmd  => fun(#{sock1 := Sock1,
			 sock2 := Sock2,
			 sock3 := Sock3,
			 sock4 := Sock4,
			 sock5 := Sock5} = State) ->
                           MRef1 = socket:monitor(Sock1),
                           MRef2 = socket:monitor(Sock2),
                           MRef3 = socket:monitor(Sock3),
                           MRef4 = socket:monitor(Sock4),
                           MRef5 = socket:monitor(Sock5),
			   ?SEV_IPRINT("Monitors: "
				       "~n   1: ~p"
				       "~n   2: ~p"
				       "~n   3: ~p"
				       "~n   4: ~p"
				       "~n   5: ~p",
				       [MRef1, MRef2, MRef3, MRef4, MRef5]),
			   {ok, State#{mon1 => MRef1,
				       mon2 => MRef2,
				       mon3 => MRef3,
				       mon4 => MRef4,
				       mon5 => MRef5}}
                   end},
         #{desc => "verify own number of monitors (=5)",
           cmd  => fun(_State) ->
			   5 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "announce ready (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, monitor),
                           ok
                   end},

         #{desc => "await continue (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, down)
                   end},

         #{desc => "await socket 1 down",
           cmd  => fun(#{sock1 := Sock,
			 mon1  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon1, State),
				   State3 = maps:remove(sock1, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},

         #{desc => "await socket 2 down",
           cmd  => fun(#{sock2 := Sock,
			 mon2  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon2, State),
				   State3 = maps:remove(sock2, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},

         #{desc => "await socket 3 down",
           cmd  => fun(#{sock3 := Sock,
			 mon3  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon3, State),
				   State3 = maps:remove(sock3, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},

         #{desc => "await socket 4 down",
           cmd  => fun(#{sock4 := Sock,
			 mon4  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon4, State),
				   State3 = maps:remove(sock4, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},

         #{desc => "await socket 5 down",
           cmd  => fun(#{sock5 := Sock,
			 mon5  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon5, State),
				   State3 = maps:remove(sock5, State2),
				   {ok, State3}
			   after 5000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},
         #{desc => "verify own number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(self()),
			   ok
                   end},
         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'owner'",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, [Sock1, Sock2, Sock3, Sock4, Sock5]} =
			       ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock1 => Sock1,
				       sock2 => Sock2,
				       sock3 => Sock3,
				       sock4 => Sock4,
				       sock5 => Sock5}}
                   end},

         #{desc => "monitor 'client 1'",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 1) start",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "send socket to client 1",
           cmd  => fun(#{client1 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 2'",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 2) start",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "send socket to client 2",
           cmd  => fun(#{client2 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 3'",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 3) start",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},
         #{desc => "send socket to client 3",
           cmd  => fun(#{client3 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 4'",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 4) start",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, init)
                   end},
         #{desc => "send socket to client 4",
           cmd  => fun(#{client4 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},

         #{desc => "monitor 'client 5'",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order (client 5) start",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, init)
                   end},
         #{desc => "send socket to client 5",
           cmd  => fun(#{client5 := Pid,
			 sock1   := Sock1,
			 sock2   := Sock2,
			 sock3   := Sock3,
			 sock4   := Sock4,
			 sock5   := Sock5} = _State) ->
			   Socks = [Sock1, Sock2, Sock3, Sock4, Sock5],
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Socks),
                           ok
                   end},


         %% The actual test
	 #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to monitor",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, monitor)
                   end},
	 #{desc => "verify total number of monitors (=1*5)",
           cmd  => fun(_State) ->
			   1*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 2 to monitor",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, monitor)
                   end},
	 #{desc => "verify total number of monitors (=2*5)",
           cmd  => fun(_State) ->
			   2*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 3 to monitor",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, monitor)
                   end},
	 #{desc => "verify total number of monitors (=3*5)",
           cmd  => fun(_State) ->
			   3*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 4 to monitor",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, monitor)
                   end},
	 #{desc => "verify total number of monitors (=4*5)",
           cmd  => fun(_State) ->
			   4*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 5 to monitor",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, monitor)
                   end},
	 #{desc => "verify total number of monitors (=5*5)",
           cmd  => fun(_State) ->
			   5*5 = socket:number_of_monitors(),
			   ok
                   end},

         #{desc => "order client 1 to await down",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 2 to await down",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 3 to await down",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 4 to await down",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},
         #{desc => "order client 5 to await down",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(owner, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await (client 1) ready",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, down)
                   end},
         #{desc => "await (client 2) ready",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, down)
                   end},
         #{desc => "await (client 3) ready",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, down)
                   end},
         #{desc => "await (client 4) ready",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client4, down)
                   end},
         #{desc => "await (client 5) ready",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client5, down)
                   end},

	 #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         %% Cleanup
         #{desc => "order (client 1) terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 1) termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 2) terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 2) termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 3) terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 3) termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 4) terminate",
           cmd  => fun(#{client4 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 4) termination",
           cmd  => fun(#{client4 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client4, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "order (client 5) terminate",
           cmd  => fun(#{client5 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (client 5) termination",
           cmd  => fun(#{client5 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client5, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start client 1 evaluator"),
    Client1 = ?SEV_START("client-1", ClientSeq, InitState),

    i("start client 2 evaluator"),
    Client2 = ?SEV_START("client-2", ClientSeq, InitState),

    i("start client 3 evaluator"),
    Client3 = ?SEV_START("client-3", ClientSeq, InitState),

    i("start client 4 evaluator"),
    Client4 = ?SEV_START("client-4", ClientSeq, InitState),

    i("start client 5 evaluator"),
    Client5 = ?SEV_START("client-5", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner   => Owner#ev.pid,
			client1 => Client1#ev.pid,
			client2 => Client2#ev.pid,
			client3 => Client3#ev.pid,
			client4 => Client4#ev.pid,
			client5 => Client5#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create several sockets (by 'owner' process), monitor each from several
%% different processes, then exit the owner.
%% The processes that did the monitor shall receive one socket DOWN for
%% each socket.

monitor_closed_socket(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(monitor_closed_socket,
           fun() ->
		   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = mon_closed_socket(InitState)
           end).


mon_closed_socket(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester,
			 sock   := Sock} = State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           (catch socket:close(Sock)),
                           {ok, maps:remove(sock, State)}
                   end},

         %% The actual test
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},
         #{desc => "await continue (socket)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, Sock} =
                               ?SEV_AWAIT_CONTINUE(Tester, tester, socket),
			   ?SEV_IPRINT("Socket: "
				       "~n      ~p", [Sock]),
			   {ok, State#{sock => Sock}}
                   end},


         %% The actual test
         #{desc => "await continue (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, monitor)
                   end},

         #{desc => "monitor socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           MRef = socket:monitor(Sock),
			   ?SEV_IPRINT("Monitors: "
				       "~n      ~p", [MRef]),
			   {ok, State#{mon => MRef}}
                   end},
         
         #{desc => "announce ready (monitor)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, monitor),
                           ok
                   end},

         #{desc => "await continue (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, down)
                   end},

         #{desc => "await socket down",
           cmd  => fun(#{sock := Sock,
			 mon  := MRef} = State) ->
			   receive
			       {'DOWN', MRef, socket, Sock, Info} ->
				   ?SEV_IPRINT("received expected down: "
					       "~n      MRef:   ~p"
					       "~n      Socket: ~p"
					       "~n      Info:   ~p",
					       [MRef, Sock, Info]),
				   State2 = maps:remove(mon,  State),
				   State3 = maps:remove(sock, State2),
				   {ok, State3}
			   after 1000 ->
				   ?SEV_EPRINT("socket down timeout"),
				   {error, timeout}
			   end
                   end},

         #{desc => "announce ready (down)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, down),
                           ok
                   end},


         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'owner'",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},

         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await client ready",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},
         #{desc => "send socket to client",
           cmd  => fun(#{client := Pid,
			 sock   := Sock} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, socket, Sock),
                           ok
                   end},

         %% The actual test
         %% There is no way we can actually know that the "system"
         %% does not create *any* sockets...
         #{desc => "order client to monitor",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, monitor),
                           ok
                   end},
         #{desc => "await client ready",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, monitor)
                   end},

         #{desc => "order client to await down",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, down),
                           ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(owner, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "await client ready",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, down)
                   end},

	 #{desc => "verify total number of monitors (=0)",
           cmd  => fun(_State) ->
			   0 = socket:number_of_monitors(),
			   ok
                   end},

	 ?SEV_SLEEP(?SECS(1)),

         %% Cleanup
         #{desc => "order client terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start client  evaluator"),
    Client = ?SEV_START("client", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner  => Owner#ev.pid,
			client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                         SOCKET CLOSURE                              %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_tcp4,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 TCP (stream) socket.

sc_cpe_socket_cleanup_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => stream,
                                 protocol => tcp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a Unix Domain (stream) socket (TCP).

sc_cpe_socket_cleanup_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_tcpL,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain   => local,
                                 type     => stream,
                                 protocol => default},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv4 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% (removed) when the controlling process terminates (without explicitly 
%% calling the close function). For a IPv6 UDP (dgram) socket.

sc_cpe_socket_cleanup_udp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => dgram,
                                 protocol => udp},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test that the sockets are cleaned up
%% ("removed") when the controlling process terminates (without explicitly 
%% calling the close function). For a Unix Domain (dgram) socket (UDP).

sc_cpe_socket_cleanup_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_cpe_socket_cleanup_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   InitState = #{domain   => local,
                                 type     => dgram,
                                 protocol => default},
                   ok = sc_cpe_socket_cleanup(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_cpe_socket_cleanup(InitState) ->
    OwnerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         type     := Type, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% *** The actual test ***
         %% We *intentionally* leave the socket "as is", no explicit close
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor owner",
           cmd  => fun(#{owner := Owner} = _State) ->
                           _MRef = erlang:monitor(process, Owner),
                           ok
                   end},
         #{desc => "order (owner) start",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (owner) ready",
           cmd  => fun(#{owner := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, owner, init),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "verify owner as controlling-process",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, Pid} ->
                                   ok;
                               {ok, Other} ->
                                   {error, {unexpected_owner, Other}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order (owner) terminate",
           cmd  => fun(#{owner := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await (owner) termination",
           cmd  => fun(#{owner := Pid} = _State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         ?SEV_SLEEP(?SECS(5)),

         %% The reason we get closed, is that as long as there is a ref to 
         %% the resource (socket), then it will not be garbage collected.
         %% Note that its still a race that the nif has processed that the
         %% "controlling process" has terminated. There really is no 
         %% proper timeout for this, but the 5 seconds "should" be enough...
         %% We should really have some way to subscribe to socket events...
         #{desc => "verify no socket (closed)",
           cmd  => fun(#{owner := Pid, sock := Sock} = _State) ->
                           case socket:getopt(Sock, otp, controlling_process) of
                               {ok, OtherPid} ->
                                   {error, {unexpected_success, Pid, OtherPid}};
                               {error, closed} ->
                                   ok;
                               {error, Reason} ->
                                   ?SEV_IPRINT("expected failure: ~p", [Reason]),
                                   {error, {unexpected_failure, Reason}}
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start (socket) owner evaluator"),
    Owner = ?SEV_START("owner", OwnerSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{owner => Owner#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Owner, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recv function.
%% Socket is IPv4.
%% 
%% <KOLLA>
%% 
%% We should really have a similar test cases for when the controlling
%% process exits and there are other processes in recv, accept, and 
%% all the other functions.
%% 
%% </KOLLA>

sc_lc_recv_response_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_recv_response_tcp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recv_response_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_recv_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is Unix Domain (stream) socket.

sc_lc_recv_response_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_recv_response_tcpL,
           fun() ->
		   has_support_unix_domain_socket()
	   end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_tcp(InitState) ->
    %% This (acceptor) is the server that accepts connections.
    %% But it is also suppose to close the connection socket, 
    %% and trigger the read failure (=closed) for the handler process.
    AcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := LSA} = _State) ->
                           ?SEV_IPRINT("bind to LSA: "
                                       "~n   ~p", [LSA]),
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock := LSock,
                         lsa   := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   ?SEV_IPRINT("bound to port: ~w", [Port]),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester,
                         lsa    := #{path := Path}}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Path),
                           ok;
                      (#{tester := Tester, lport := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},
                           
         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, {H1, H2, H3}} ->
                                   {ok, State#{handler1 => H1,
                                               handler2 => H2,
                                               handler3 => H3}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("connection accepted: "
                                               "~n   ~p", [socket:sockname(Sock)]),
                                   {ok, State#{csock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},
         #{desc => "transfer connection to handler 1",
           cmd  => fun(#{handler1 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 2",
           cmd  => fun(#{handler2 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "transfer connection to handler 3",
           cmd  => fun(#{handler3 := Handler, csock := Sock}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Handler, transfer, Sock),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close),
                           ok
                   end},
         #{desc => "close connection socket",
           cmd  => fun(#{csock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(csock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain := local,
                         lsock  := Sock,
                         lsa    := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->maps:remove(lsa, State) end,
                                           fun() -> State end),
                           State2 = maps:remove(lsock, State1),
                           State3 = maps:remove(lport, State2),
                           {ok, State3};
                      (#{lsock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(lsock, State),
                                   State2 = maps:remove(lport, State1),
                                   {ok, State2};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is to perform the recv for which
    %% we are testing the response.
    HandlerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           {Tester, Acceptor} = ?SEV_AWAIT_START(),
                           {ok, State#{tester  => Tester, 
                                      acceptor => Acceptor}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Acceptor} = _State) ->
                           _MRef = erlang:monitor(process, Acceptor),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (transfer)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Pid, acceptor, transfer) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (transfer)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, transfer),
                           ok
                   end},
         #{desc => "attempt recv (=> closed)",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           %% ok = socket:setopt(Sock, otp, debug, true),
                           case Recv(Sock) of
                               {ok, _Data} ->
                                   ?SEV_EPRINT("Unexpected data received"),
                                   {error, unexpected_success};
                               {error, closed} ->
                                   ?SEV_IPRINT("received expected 'closed' "
                                               "result"),
                                   State1 = maps:remove(sock, State),
                                   {ok, State1};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Unexpected read failure: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% The point of this is basically just to create the connection.
    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind socket to local address",
           cmd  => fun(#{sock := Sock, local_sa := LSA} = _State) ->
                           ?SEV_IPRINT("bind to LSA: "
                                       "~n   ~p", [LSA]),
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{domain := local = Domain,
                         tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, connect) of
                               {ok, ServerPath} ->
                                   ?SEV_IPRINT("Server Path: "
                                               "~n   ~s", [ServerPath]),
                                   ServerSA = #{family => Domain,
                                                path   => ServerPath},
                                   {ok, State#{server_sa => ServerSA}};
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{tester := Tester, local_sa := LSA} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, connect) of
                               {ok, Port} ->
                                   ServerSA = LSA#{port => Port},
                                   {ok, State#{server_sa => ServerSA}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := ServerSA}) ->
                           socket:connect(Sock, ServerSA)
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end                           
                   end},
         #{desc => "close socket",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}} = State) ->
                           sock_close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(sock, State1)};
                       (#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor acceptor",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 1",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 2",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor handler 3",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the acceptor
         #{desc => "order acceptor start",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await acceptor ready (init)",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, PortOrPath} ->
                                   {ok, State#{server_info => PortOrPath}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% Start the handler(s)
         #{desc => "order handler 1 start",
           cmd  => fun(#{acceptor := Acceptor, handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler1, init)
                   end},
         #{desc => "order handler 2 start",
           cmd  => fun(#{acceptor := Acceptor, handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler2, init)
                   end},
         #{desc => "order handler 3 start",
           cmd  => fun(#{acceptor := Acceptor, handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Acceptor),
                           ok
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, handler3, init)
                   end},

         %% Start the client
         #{desc => "order client start",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% The actual test
         #{desc => "order acceptor to continue (accept)",
           cmd  => fun(#{acceptor := Pid, 
                         handler1 := H1, 
                         handler2 := H2, 
                         handler3 := H3} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, {H1, H2, H3}),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (connect)",
           cmd  => fun(#{client := Pid, server_info := Info} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect, Info),
                           ok
                   end},
         #{desc => "await acceptor ready (accept)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, accept)
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, connect)
                   end},
         #{desc => "await handler 1 ready (transfer)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, transfer)
                   end},
         #{desc => "await handler 2 ready (transfer)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, transfer)
                   end},
         #{desc => "await handler 3 ready (transfer)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, transfer)
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order acceptor to continue (close connection socket)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await acceptor ready (close)",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, acceptor, close)
                   end},
         #{desc => "await handler 1 ready (recv closed)",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler1, recv_closed)
                   end},
         #{desc => "await handler 2 ready (recv closed)",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler2, recv_closed)
                   end},
         #{desc => "await handler 3 ready (recv closed)",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, handler3, recv_closed)
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(client, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(handler3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order acceptor to terminate",
           cmd  => fun(#{acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await acceptor termination",
           cmd  => fun(#{acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(acceptor, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start acceptor evaluator"),
    AccInitState = InitState,
    Acceptor = ?SEV_START("acceptor", AcceptorSeq, AccInitState),

    i("start handler 1 evaluator"),
    HandlerInitState = #{recv => maps:get(recv, InitState)},
    Handler1 = ?SEV_START("handler-1", HandlerSeq, HandlerInitState),

    i("start handler 2 evaluator"),
    Handler2 = ?SEV_START("handler-2", HandlerSeq, HandlerInitState),

    i("start handler 3 evaluator"),
    Handler3 = ?SEV_START("handler-3", HandlerSeq, HandlerInitState),

    i("start client evaluator"),
    ClientInitState = InitState,
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start tester evaluator"),
    TesterInitState = #{acceptor => Acceptor#ev.pid,
                        handler1 => Handler1#ev.pid,
                        handler2 => Handler2#ev.pid,
                        handler3 => Handler3#ev.pid,
                        client   => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Acceptor, 
                            Handler1, Handler2, Handler3, 
                            Client, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while a process is calling the recvfrom function.
%% Socket is IPv4.
%% 

sc_lc_recvfrom_response_udp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   Recv      = fun(Sock, To) ->
                                       socket:recvfrom(Sock, [], To)
                               end,
                   InitState = #{domain   => inet,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is IPv6.

sc_lc_recvfrom_response_udp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   Recv      = fun(Sock, To) ->
                                       socket:recvfrom(Sock, [], To)
                               end,
                   InitState = #{domain   => inet6,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recv function.
%% Socket is Unix Domainm (dgram) socket.

sc_lc_recvfrom_response_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   Recv      = fun(Sock, To) ->
                                       socket:recvfrom(Sock, [], To)
                               end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_receive_response_udp(InitState) ->
    PrimServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "open socket",
           cmd  => fun(#{domain := Domain, protocol := Proto} = State) ->
                           Sock = sock_open(Domain, dgram, Proto),
                           {ok, State#{sock => Sock}}
                   end},
         #{desc => "bind socket",
           cmd  => fun(#{sock := Sock, local_sa := LSA}) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ?SEV_IPRINT("src bound"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("src bind failed: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "socket name",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:sockname(Sock) of
                               {ok, SA} ->
                                   {ok, State#{sa => SA}};
                               {error, eafnosupport = Reason} ->
                                   ?SEV_IPRINT("Failed get socket name: "
                                               "~n   ~p", [Reason]),
                                   (catch socket:close(Sock)),
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed get socket name: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv, with timeout)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, recv) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "receive, with timeout",
           cmd  => fun(#{sock := Sock, recv := Recv, timeout := Timeout}) ->
                           case Recv(Sock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv, with timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},
         #{desc => "close socket",
           cmd  => fun(#{domain   := local,
                         sock     := Sock,
                         local_sa := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(sock, State1)};
                      (#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, terminate) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecServerSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                           
                   end},
         #{desc => "receive",
           cmd  => fun(#{sock := Sock, recv := Recv} = State) ->
                           case Recv(Sock, infinity) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor primary server",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 1",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 2",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary server 3",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary server' start",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' ready (init)",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_server, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary server 1
         #{desc => "order 'secondary server 1' start",
           cmd  => fun(#{sec_server1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 1' ready (init)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server1, init)
                   end},

         %% Start the secondary server 2
         #{desc => "order 'secondary server 2' start",
           cmd  => fun(#{sec_server2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 2' ready (init)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server2, init)
                   end},

         %% Start the secondary server 3
         #{desc => "order 'secondary server 3' start",
           cmd  => fun(#{sec_server3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary server 3' ready (init)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_server3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinite recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary server 1' to continue (recv)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 2' to continue (recv)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary server 3' to continue (recv)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary server' to continue (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary server' ready (recv, with timeout)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, recv)
                   end},
         #{desc => "order 'primary server' to continue (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary server' ready (close)",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_server, close)
                   end},
         #{desc => "await 'secondary server 1' ready (closed)",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server1, recv_closed)
                   end},
         #{desc => "await 'secondary server 2' ready (closed)",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server2, recv_closed)
                   end},
         #{desc => "await 'secondary server 3' ready (closed)",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_server3, recv_closed)
                   end},

         %% Terminations
         #{desc => "order 'secondary server 3' to terminate",
           cmd  => fun(#{sec_server3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 3' termination",
           cmd  => fun(#{sec_server3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 2' to terminate",
           cmd  => fun(#{sec_server2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 2' termination",
           cmd  => fun(#{sec_server2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary server 1' to terminate",
           cmd  => fun(#{sec_server1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary server 1' termination",
           cmd  => fun(#{sec_server1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_server1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary server' to terminate",
           cmd  => fun(#{prim_server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary server' termination",
           cmd  => fun(#{prim_server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_server, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    

    i("start 'primary server' evaluator"),
    PrimSrvInitState = InitState,
    PrimServer = ?SEV_START("prim-server", PrimServerSeq, PrimSrvInitState),

    i("start 'secondary server 1' evaluator"),
    SecSrvInitState = #{recv => maps:get(recv, InitState)},
    SecServer1 = ?SEV_START("sec-server-1", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 2' evaluator"),
    SecServer2 = ?SEV_START("sec-server-2", SecServerSeq, SecSrvInitState),

    i("start 'secondary server 3' evaluator"),
    SecServer3 = ?SEV_START("sec-server-3", SecServerSeq, SecSrvInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_server => PrimServer#ev.pid,
                        sec_server1 => SecServer1#ev.pid,
                        sec_server2 => SecServer2#ev.pid,
                        sec_server3 => SecServer3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimServer, 
                            SecServer1, SecServer2, SecServer3,
                            Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(), % recvmsg does not work on Windows
                   has_support_ipv4()
           end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(), % recvmsg does not work on Windows
                   has_support_ipv6()
           end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is Unix Domain (stream) socket.

sc_lc_recvmsg_response_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(), % recvmsg does not work on Windows
                   has_support_unix_domain_socket()
           end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_lc_recvmsg_response_udp4(_Config) when is_list(_Config) ->
    tc_try(sc_lc_recvmsg_response_udp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain   => inet,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_lc_recvmsg_response_udp6(_Config) when is_list(_Config) ->
    tc_try(sc_recvmsg_response_udp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain   => inet6,
                                 protocol => udp,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the recvmsg function.
%% Socket is Unix Domain (dgram) socket.

sc_lc_recvmsg_response_udpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_recvmsg_response_udpL,
           fun() ->
		   has_support_unix_domain_socket(),
		   is_not_windows()
	   end,
           fun() ->
                   Recv      = fun(Sock, To) -> socket:recvmsg(Sock, To) end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_lc_receive_response_udp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv4.

sc_lc_acceptor_response_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_acceptor_response_tcp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain   => inet,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is IPv6.

sc_lc_acceptor_response_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_acceptor_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 protocol => tcp},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% locally closed while the process is calling the accept function.
%% We test what happens with a non-controlling_process also, since we 
%% git the setup anyway.
%% Socket is Unix Domain (stream) socket.

sc_lc_acceptor_response_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(sc_lc_acceptor_response_tcpL,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain   => local,
                                 protocol => default},
                   ok = sc_lc_acceptor_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_lc_acceptor_response_tcp(InitState) ->
    PrimAcceptorSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start (from tester)",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain, 
                         protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Sock),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, accept) of
                               {ok, Timeout} ->
                                   {ok, State#{timeout => Timeout}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await connection",
           cmd  => fun(#{sock := LSock, timeout := Timeout} = _State) ->
                           case socket:accept(LSock, Timeout) of
                               {error, timeout} ->
                                   ok;
                               {ok, Sock} ->
                                   ?SEV_EPRINT("unexpected success"),
                                   (catch socket:close(Sock)),
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept timeout)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_timeout),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, close)
                   end},
         #{desc => "close socket",
           cmd  => fun(#{domain := local,
                         sock   := Sock,
                         lsa    := #{path := Path}} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 =
                                       unlink_path(Path,
                                                   fun() ->
                                                           maps:remove(lsa, State)
                                                   end,
                                                   fun() ->
                                                           State
                                                   end),
                                   {ok, maps:remove(sock, State1)};
                               {error, _} = ERROR ->
                                   unlink_path(Path),
                                   ERROR
                           end;
                      (#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   {ok, maps:remove(sock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

                                                % Termination
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, Sock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, sock => Sock}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init)
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ok = ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:accept(Sock) of
                               {error, closed} ->
                                   {ok, maps:remove(sock, State)};
                               {ok, _} ->
                                   {error, unexpected_success};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept closed)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept_closed)
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor 'primary acceptor'",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor 'secondary acceptor 1'",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 2",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor secondary acceptor 3",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the primary server
         #{desc => "order 'primary acceptor' start",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (init)",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           {ok, Sock} = ?SEV_AWAIT_READY(Pid, prim_acc, init),
                           {ok, State#{sock => Sock}}
                   end},

         %% Start the secondary acceptor 1
         #{desc => "order 'secondary acceptor 1' start",
           cmd  => fun(#{sec_acc1 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' ready (init)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, init)
                   end},

         %% Start the secondary acceptor 2
         #{desc => "order 'secondary acceptor 2' start",
           cmd  => fun(#{sec_acc2 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' ready (init)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, init)
                   end},

         %% Start the secondary acceptor 3
         #{desc => "order 'secondary acceptor 3' start",
           cmd  => fun(#{sec_acc3 := Pid, sock := Sock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Sock),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' ready (init)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, init)
                   end},


         %% The actual test
         %% Make all the seondary servers continue, with an infinite recvfrom
         %% and then the prim-server with a timed recvfrom.
         %% After the prim server notifies us (about the timeout) we order it
         %% to close the socket, which should cause the all the secondary 
         %% server to return with error-closed.

         #{desc => "order 'secondary acceptor 1' to continue (accept)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 2' to continue (accept)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'secondary acceptor 3' to continue (accept)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order 'primary acceptor' to continue",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept, ?SECS(5)),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (accept timeout)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, accept_timeout)
                   end},
         #{desc => "order 'primary acceptor' to continue (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await 'primary acceptor' ready (close)",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, prim_acc, close)
                   end},
         #{desc => "await 'secondary acceptor 1' ready (accept closed)",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc1, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 2' ready (accept closed)",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc2, accept_closed)
                   end},
         #{desc => "await 'secondary acceptor 3' ready (accept closed)",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, sec_acc3, accept_closed)
                   end},


         %% Terminations
         #{desc => "order 'secondary acceptor 3' to terminate",
           cmd  => fun(#{sec_acc3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 3' termination",
           cmd  => fun(#{sec_acc3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc3, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 2' to terminate",
           cmd  => fun(#{sec_acc2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 2' termination",
           cmd  => fun(#{sec_acc2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc2, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'secondary acceptor 1' to terminate",
           cmd  => fun(#{sec_acc1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'secondary acceptor 1' termination",
           cmd  => fun(#{sec_acc1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(sec_acc1, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order 'primary acceptor' to terminate",
           cmd  => fun(#{prim_acc := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await 'primary acceptor' termination",
           cmd  => fun(#{prim_acc := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   {ok, maps:remove(prim_acc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],


    i("start 'primary acceptor' evaluator"),
    PrimAccInitState = InitState,
    PrimAcc = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAccInitState),

    i("start 'secondary acceptor 1' evaluator"),
    SecAccInitState = #{},
    SecAcc1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 2' evaluator"),
    SecAcc2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAccInitState),

    i("start 'secondary acceptor 3' evaluator"),
    SecAcc3 = ?SEV_START("sec-acceptor-3", SecAcceptorSeq, SecAccInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{prim_acc => PrimAcc#ev.pid,
                        sec_acc1 => SecAcc1#ev.pid,
                        sec_acc2 => SecAcc2#ev.pid,
                        sec_acc3 => SecAcc3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([PrimAcc, SecAcc1, SecAcc2, SecAcc3, Tester]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv4.
%%
%% To minimize the chance of "weirdness", we should really have test cases
%% where the two sides of the connection is on different machines. But for
%% now, we will make do with different VMs on the same host.
%%

sc_rc_recv_response_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is IPv6.

sc_rc_recv_response_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => inet6,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recv function.
%% Socket is Unix Domain (stream) socket.

sc_rc_recv_response_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recv(Sock) end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_rc_receive_response_tcp(InitState) ->
    %% Each connection are handled by handler processes.
    %% These are created (on the fly) and handled internally 
    %% by the server!
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain, protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock    := LSock,
                         local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain   := local,
                         tester   := Tester,
                         local_sa := LSA}) ->
                           %% Actually we only need to send the path,
                           %% but to keep it simple, we send the "same"
                           %% as for non-local.
                           ?SEV_ANNOUNCE_READY(Tester, init, LSA),
                           ok;
                      (#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept all three connections)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept 1",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(1, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock1   => Sock,
                                               handler1 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 1 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1} = _State) ->
                           ?SEV_AWAIT_READY(Handler1, handler1, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "accept 2",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(2, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock2   => Sock,
                                               handler2 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 2 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2} = _State) ->
                           ?SEV_AWAIT_READY(Handler2, handler2, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1}])
                   end},
         #{desc => "accept 3",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler = sc_rc_tcp_handler_start(3, Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock3   => Sock,
                                               handler3 => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler 3 ready (init)",
           cmd  => fun(#{tester   := Tester, 
                         handler1 := Handler1, 
                         handler2 := Handler2, 
                         handler3 := Handler3} = _State) ->
                           ?SEV_AWAIT_READY(Handler3, handler3, init, 
                                            [{tester,   Tester},
                                             {handler1, Handler1},
                                             {handler2, Handler2}])
                   end},
         #{desc => "announce ready (accept all three connections)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},


         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler 1 to receive",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 2 to receive",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "order handler 3 to receive",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler 1 (recv)",
           cmd  => fun(#{tester := Tester,
                         handler1 := Pid1,
                         handler2 := Pid2,
                         handler3 := Pid3} = _State) ->
                           case ?SEV_AWAIT_READY(Pid1, handler1, recv, 
                                                 [{tester, Tester},
                                                  {handler2, Pid2},
                                                  {handler3, Pid3}]) of
                               {ok, Result} ->
                                   Result;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Unexpected failure: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 2 (recv)",
           cmd  => fun(#{tester := Tester,
                         handler1 := Pid1,
                         handler2 := Pid2,
                         handler3 := Pid3} = _State) ->
                           case ?SEV_AWAIT_READY(Pid2, handler2, recv, 
                                                 [{tester, Tester},
                                                  {handler1, Pid1},
                                                  {handler3, Pid3}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await ready from handler 3 (recv)",
           cmd  => fun(#{tester := Tester,
                         handler1 := Pid1,
                         handler2 := Pid2,
                         handler3 := Pid3} = _State) ->
                           case ?SEV_AWAIT_READY(Pid3, handler3, recv, 
                                                 [{tester, Tester},
                                                  {handler1, Pid1},
                                                  {handler2, Pid2}]) of
                               {ok, Result} ->
                                   Result;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv closed from all handlers)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv_closed),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler 1 to terminate",
           cmd  => fun(#{handler1 := Pid} = _State) ->
                           %% Pid ! {terminate, self(), ok},
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 1 termination",
           cmd  => fun(#{handler1 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock1,   State),
                           State2 = maps:remove(handler1, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 2 to terminate",
           cmd  => fun(#{handler2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 2 termination",
           cmd  => fun(#{handler2 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock2,   State),
                           State2 = maps:remove(handler2, State1),
                           {ok, State2}
                   end},
         #{desc => "order handler 3 to terminate",
           cmd  => fun(#{handler3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler 3 termination",
           cmd  => fun(#{handler3 := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock3,   State),
                           State2 = maps:remove(handler3, State1),
                           {ok, State2}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := #{path := Path}} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   State1 =
                                       unlink_path(Path,
                                                   fun() ->
                                                           maps:remove(lsa, State)
                                                   end,
                                                   fun() ->
                                                           State
                                                   end),
                                   {ok, maps:remove(lsock, State1)};
                               {error, _} = ERROR ->
                                   unlink_path(Path),
                                   ERROR
                           end;
                      (#{lsock := LSock} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   {ok, maps:remove(lsock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, {NodeID, ServerSA}} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       node_id   => NodeID, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(#{node_id := NodeID} = State) ->
                           {Peer, Node} =
                               start_node(?CT_PEER_NAME(?F("client_~w",
                                                           [NodeID]))),
                           {ok, State#{node => Node, peer => Peer}}
                   end},
         #{desc => "monitor client node 1",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node} = State) ->
                           Pid = sc_rc_tcp_client_start(Node),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient   := Client,
                         server_sa := ServerSA,
                         protocol  := Proto}) ->
                           ?SEV_ANNOUNCE_START(Client, {ServerSA, Proto}),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client process ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, connect, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (connected)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},
         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to close",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, close),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, close, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester, 
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rclient, Client}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error,
                                {unexpected_exit, rclient, noconnection}} ->
                                   %% Global might have disconnected => SKIP
                                   ?SEV_IPRINT("lost connection "
                                               "to remote client node => SKIP"),
                                   {skip, {rclient, noconnection}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "kill remote client",
           cmd  => fun(#{rclient := RClient}) ->
                           ?SEV_IPRINT("try kill remote client ~p", [RClient]),
                           ?SEV_ANNOUNCE_TERMINATE(RClient),
                           ok
                   end},
         #{desc => "await remote client termination",
           cmd  => fun(#{rclient := RClient} = State) ->
                           ?SEV_AWAIT_TERMINATION(RClient),
                           ?SEV_IPRINT("remote client ~p terminated",
                                       [RClient]),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{peer := Peer} = State) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end}
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State) ->
                           ?SEV_IPRINT("Success node stop - await nodedown: "
                                       "~n      ~p", [Node]),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(peer, State),
                                   State2 = maps:remove(node, State1),
                                   {ok, State2}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(peer, State),
                           State2 = maps:remove(node, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 1",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 2",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 3",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client(s)
         #{desc => "order client 1 start",
           cmd  => fun(#{client1   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {1, ServerSA}),
                           ok
                   end},
         #{desc => "await client 1 ready (init)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client1, init)
                   end},
         #{desc => "order client 2 start",
           cmd  => fun(#{client2   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {2, ServerSA}),
                           ok
                   end},
         #{desc => "await client 2 ready (init)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client2, init)
                   end},
         #{desc => "order client 3 start",
           cmd  => fun(#{client3   := Pid, 
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, {3, ServerSA}),
                           ok
                   end},
         #{desc => "await client 3 ready (init)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client3, init)
                   end},

         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (connect)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 1 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, connect, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (connect)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 2 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (connect)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client 3 ready (connect)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client3, connect, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (accept from all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order server continue (recv for all connections)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client 1 continue (close)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 1 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client1, client1, close, 
                                            [{server,  Server},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 2 continue (close)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 2 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client2, client2, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client3, Client3}]),
                           ok
                   end},
         #{desc => "order client 3 continue (close)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client 3 ready (close)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Client3, client1, close, 
                                            [{server,  Server},
                                             {client1, Client1},
                                             {client2, Client2}]),
                           ok
                   end},
         #{desc => "await server ready (close for all connections)",
           cmd  => fun(#{server  := Server,
                         client1 := Client1,
                         client2 := Client2,
                         client3 := Client3} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv_closed,
                                            [{client1, Client1},
                                             {client2, Client2},
                                             {client3, Client3}]),
                           ok
                   end},

         %% Terminations
         #{desc => "order client 1 to terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 1 termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 2 to terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 2 termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 3 to terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 3 termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client3, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = InitState,
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator(s)"),
    ClientInitState = InitState#{host => local_host()},
    Client1 = ?SEV_START("client-1", ClientSeq, ClientInitState),
    Client2 = ?SEV_START("client-2", ClientSeq, ClientInitState),
    Client3 = ?SEV_START("client-3", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server  => Server#ev.pid,
                        client1 => Client1#ev.pid,
                        client2 => Client2#ev.pid,
                        client3 => Client3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server,
                            Client1, Client2, Client3,
                            Tester]).


sc_rc_tcp_client_start(Node) ->
    Self = self(),
    Fun  = fun() -> sc_rc_tcp_client(Self) end,
    erlang:spawn(Node, Fun).


sc_rc_tcp_client(Parent) ->
    sc_rc_tcp_client_init(Parent),
    {ServerSA, Proto} = sc_rc_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = sc_rc_tcp_client_create(Domain, Proto),
    Path     = sc_rc_tcp_client_bind(Sock, Domain),
    sc_rc_tcp_client_announce_ready(Parent, init),
    sc_rc_tcp_client_await_continue(Parent, connect),
    sc_rc_tcp_client_connect(Sock, ServerSA),
    sc_rc_tcp_client_announce_ready(Parent, connect),
    sc_rc_tcp_client_await_continue(Parent, close),
    sc_rc_tcp_client_close(Sock, Path),
    sc_rc_tcp_client_announce_ready(Parent, close),
    Reason = sc_rc_tcp_client_await_terminate(Parent),
    ?SEV_IPRINT("terminate"),
    exit(Reason).

sc_rc_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

sc_rc_tcp_client_await_start(Parent) ->
    i("sc_rc_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

sc_rc_tcp_client_create(Domain, Proto) ->
    i("sc_rc_tcp_client_create -> entry"),
    case socket:open(Domain, stream, Proto) of
        {ok, Sock} ->
            case socket:getopt(Sock, otp, fd) of
                {ok, FD} ->
                    put(sname, ?F("rclient-~w", [FD])); % Update SName
                _ ->
                    ok
            end,
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

sc_rc_tcp_client_bind(Sock, Domain) ->
    i("sc_rc_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        ok ->
            case socket:sockname(Sock) of
                {ok, #{family := local, path := Path}} ->
                    Path;
                {ok, _} ->
                    undefined;
                {error, Reason1} ->
                    exit({sockname, Reason1})
            end;
        {error, Reason} ->
            exit({bind, Reason})
    end.

sc_rc_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_IPRINT("ready ~w", [Slogan]),
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

sc_rc_tcp_client_await_continue(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w continue", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

sc_rc_tcp_client_connect(Sock, ServerSA) ->
    i("sc_rc_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

sc_rc_tcp_client_close(Sock, Path) ->
    i("sc_rc_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            unlink_path(Path),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("failed closing: "
                        "~n   Reason: ~p", [Reason]),
            unlink_path(Path),
            {error, {close, Reason}}
    end.

sc_rc_tcp_client_await_terminate(Parent) ->
    i("sc_rc_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


%% The handlers run on the same node as the server (the local node).

sc_rc_tcp_handler_start(ID, Recv, Sock) ->
    Self     = self(),
    Fun      = fun() -> sc_rc_tcp_handler(ID, Self, Recv, Sock) end,
    {Pid, _} = erlang:spawn_monitor(Fun),
    Pid.

sc_rc_tcp_handler(ID, Parent, Recv, Sock) ->
    sc_rc_tcp_handler_init(ID, socket:getopt(Sock, otp, fd), Parent),
    sc_rc_tcp_handler_await(Parent, recv),
    RecvRes = sc_rc_tcp_handler_recv(Recv, Sock),
    sc_rc_tcp_handler_announce_ready(Parent, recv, RecvRes),
    Reason = sc_rc_tcp_handler_await(Parent, terminate),
    exit(Reason).

sc_rc_tcp_handler_init(ID, {ok, FD}, Parent) ->
    put(sname, ?F("handler-~w:~w", [ID, FD])),
    _MRef = erlang:monitor(process, Parent),
    ?SEV_IPRINT("started"),
    ?SEV_ANNOUNCE_READY(Parent, init),
    ok.

sc_rc_tcp_handler_await(Parent, terminate) ->
    ?SEV_IPRINT("await terminate"),
    ?SEV_AWAIT_TERMINATE(Parent, tester);
sc_rc_tcp_handler_await(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

sc_rc_tcp_handler_recv(Recv, Sock) ->
    ?SEV_IPRINT("recv"),
    try Recv(Sock) of
        {error, closed} ->
            ok;
        {ok, Data} ->
            ?SEV_IPRINT("unexpected success: "
                        "~n   (Unexp) Data: ~p"
                        "~n   Socket Info:  ~p", [Data, socket:info(Sock)]),
            {error, unexpected_success};
        {error, Reason} = ERROR ->
            ?SEV_IPRINT("receive error: "
                        "~n   ~p", [Reason]),
            ERROR
    catch
        error:notsup = Error:Stack ->
            ?SEV_IPRINT("receive ~w error: skip"
                        "~n   Stack: ~p", [Error, Stack]),
            exit({skip, Error});
        C:E:S ->
            ?SEV_IPRINT("receive failure: "
                        "~n   Class: ~p"
                        "~n   Error: ~p"
                        "~n   Stack: ~p", [C, E, S]),
            {error, {recv, C, E, S}}
    end.

sc_rc_tcp_handler_announce_ready(Parent, Slogan, Result) ->
    ?SEV_IPRINT("announce ready"),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv4.

sc_rc_recvmsg_response_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_rc_recvmsg_response_tcp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is IPv6.

sc_rc_recvmsg_response_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_rc_recvmsg_response_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => inet6,
                                 protocol => tcp,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is 
%% remotely closed while the process is calling the recvmsg function.
%% Socket is Unix Domain (stream) socket.

sc_rc_recvmsg_response_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(sc_rc_recvmsg_response_tcpL,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   Recv      = fun(Sock) -> socket:recvmsg(Sock) end,
                   InitState = #{domain   => local,
                                 protocol => default,
                                 recv     => Recv},
                   ok = sc_rc_receive_response_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recv function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv4.
%%
%% To minimize the chance of "weirdness", we should really have test cases
%% where the two sides of the connection is on different machines. But for
%% now, we will make do with different VMs on the same host.
%% This would of course not work for Unix Domain sockets.
%%

sc_rs_recv_send_shutdown_receive_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       socket:recv(Sock)
                               end,
                   Send      = fun(Sock, Data) ->
                                       socket:send(Sock, Data)
                               end,
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recv function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv6.

sc_rs_recv_send_shutdown_receive_tcp6(_Config) when is_list(_Config) ->
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       socket:recv(Sock)
                               end,
                   Send      = fun(Sock, Data) ->
                                       socket:send(Sock, Data)
                               end,
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recv function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is Unix Domain (stream) socket.

sc_rs_recv_send_shutdown_receive_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       socket:recv(Sock)
                               end,
                   Send      = fun(Sock, Data) ->
                                       socket:send(Sock, Data)
                               end,
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sc_rs_send_shutdown_receive_tcp(InitState) ->
    %% The connection is handled by a handler processes.
    %% This are created (on the fly) and handled internally
    %% by the server!
    ServerSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           i("get local address for ~p", [Domain]),
                           LSA = which_local_socket_addr(Domain),
                           {ok, State#{local_sa => LSA}}
                   end},
         #{desc => "create listen socket",
           cmd  => fun(#{domain := Domain, proto := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain   := local,
                         lsock    := LSock,
                         local_sa := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock := LSock, local_sa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   ?SEV_IPRINT("bound to port: ~w", [Port]),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester, local_sa := LSA}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, LSA),
                           ok;
                      (#{tester := Tester, local_sa := LSA, lport := Port}) ->
                           ServerSA = LSA#{port => Port},
                           ?SEV_ANNOUNCE_READY(Tester, init, ServerSA),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept",
           cmd  => fun(#{lsock := LSock, recv := Recv} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("accepted: try start handler"),
                                   Handler =
                                       sc_rs_tcp_handler_start(Recv, Sock),
                                   ?SEV_IPRINT("handler started"),
                                   {ok, State#{csock   => Sock,
                                               handler => Handler}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await handler ready (init)",
           cmd  => fun(#{tester  := Tester,
                         handler := Handler} = _State) ->
                           ?SEV_AWAIT_READY(Handler, handler, init,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         #{desc => "await continue (first recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler to receive (first)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler (first recv)",
           cmd  => fun(#{tester := Tester, handler := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler, recv,
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   ?SEV_IPRINT("first recv: ~p", [Result]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (first recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (second recv)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "order handler to receive (second)",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await ready from handler (second recv)",
           cmd  => fun(#{tester := Tester, handler := Pid} = _State) ->
                           case ?SEV_AWAIT_READY(Pid, handler, recv,
                                                 [{tester, Tester}]) of
                               {ok, Result} ->
                                   ?SEV_IPRINT("second recv: ~p", [Result]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (second recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order handler to terminate",
           cmd  => fun(#{handler := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await handler termination",
           cmd  => fun(#{handler := Pid} = State) ->
                           ?SEV_AWAIT_TERMINATION(Pid),
                           State1 = maps:remove(csock,   State),
                           State2 = maps:remove(handler, State1),
                           {ok, State2}
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{domain   := local,
                         lsock    := Sock,
                         local_sa := #{path := Path}} = State) ->
                           socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(local_sa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{lsock := LSock} = State) ->
                           case socket:close(LSock) of
                               ok ->
                                   {ok, maps:remove(lsock, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, ServerSA} = ?SEV_AWAIT_START(),
                           {ok, State#{tester    => Tester, 
                                       server_sa => ServerSA}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create node",
           cmd  => fun(State) ->
                           {Peer, Node} = start_node("client"),
                           {ok, State#{peer => Peer, node => Node}}
                   end},
         #{desc => "monitor client node",
           cmd  => fun(#{node := Node} = _State) ->
                           true = erlang:monitor_node(Node, true),
                           ok
                   end},
         #{desc => "start remote client on client node",
           cmd  => fun(#{node := Node,
                         send := Send} = State) ->
                           Pid = sc_rs_tcp_client_start(Node, Send),
                           ?SEV_IPRINT("client ~p started", [Pid]),
                           {ok, State#{rclient => Pid}}
                   end},
         #{desc => "monitor remote client",
           cmd  => fun(#{rclient := Pid}) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "order remote client to start",
           cmd  => fun(#{rclient   := Client,
                         proto     := Proto,
                         server_sa := ServerSA}) ->
                           ?SEV_ANNOUNCE_START(Client, {ServerSA, Proto}),
                           ok
                   end},
         #{desc => "await remote client ready",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, init, 
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect, 
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to continue (connect)",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client process ready (connect)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, connect,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         #{desc => "await continue (send)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_CONTINUE(Tester, tester, send,
                                                    [{rclient, Client}]) of
                               {ok, Data} ->
                                   {ok, State#{rclient_data => Data}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order remote client to send",
           cmd  => fun(#{rclient      := Client,
                         rclient_data := Data}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Data),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, send,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},


         #{desc => "await continue (shutdown)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, shutdown,
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to shutdown",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, shutdown),
                           ok
                   end},
         #{desc => "await remote client ready (shiutdown)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, shutdown,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (shutdown)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, shutdown),
                           ok
                   end},

         #{desc => "await continue (close)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, close,
                                               [{rclient, Client}]),
                           ok
                   end},
         #{desc => "order remote client to close",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, close),
                           ok
                   end},
         #{desc => "await remote client ready (closed)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, rclient, close,
                                            [{tester, Tester}])
                   end},
         #{desc => "announce ready (close)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, close),
                           ok
                   end},

         %% Termination
         #{desc => "await terminate (from tester)",
           cmd  => fun(#{tester  := Tester,
                         rclient := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester,
                                                     [{rclient, Client}]) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "kill remote client",
           cmd  => fun(#{rclient := Client}) ->
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await remote client termination",
           cmd  => fun(#{rclient := Client} = State) ->
                           ?SEV_AWAIT_TERMINATION(Client),
                           State1 = maps:remove(rclient, State),
                           {ok, State1}
                   end},
         #{desc => "stop client node",
           cmd  => fun(#{peer := Peer} = State) ->
                           {ok,
                            try peer:stop(Peer) of
                                ok ->
                                    State#{node_stop => ok};
                                {error, Reason} ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   ~p", [Reason]),
                                    State#{node_stop => error}
                            catch
                                C:E:S ->
                                    ?SEV_EPRINT("Unexpected node stop result: "
                                                "~n   Class: ~p"
                                                "~n   Error: ~p"
                                                "~n   Stack: ~p",[C, E, S]),
                                    State#{node_stop => error}
                            end}
                   end},
         #{desc => "await client node termination",
           cmd  => fun(#{node := Node, node_stop := ok} = State) ->
                           ?SEV_IPRINT("Success node stop - await nodedown"),
                           receive
                               {nodedown, Node} ->
                                   ?SEV_IPRINT("nodedown received - cleanup"),
                                   State1 = maps:remove(peer, State),
                                   State2 = maps:remove(node, State1),
                                   {ok, State2}
                           end;
                      (#{node_stop := error} = State) ->
                           ?SEV_IPRINT("Failed node stop - cleanup"),
                           State1 = maps:remove(peer, State),
                           State2 = maps:remove(node, State1),
                           {ok, State2}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% *** Init part ***
         #{desc => "monitor server",
           cmd  => fun(#{server := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client",
           cmd  => fun(#{client := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},

         %% Start the server
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await server ready (init)",
           cmd  => fun(#{server := Pid} = State) ->
                           {ok, ServerSA} = ?SEV_AWAIT_READY(Pid, server, init),
                           {ok, State#{server_sa => ServerSA}}
                   end},

         %% Start the client(s)
         #{desc => "order client start",
           cmd  => fun(#{client    := Pid,
                         server_sa := ServerSA} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, ServerSA),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},

         %% The actual test
         #{desc => "order server continue (accept)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client continue (connect)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect,
                                            [{server, Server}]),
                           ok
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept,
                                            [{client, Client}]),
                           ok
                   end},

         #{desc => "order client continue (send)",
           cmd  => fun(#{client := Pid,
                         data   := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, send, Data),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send,
                                            [{server, Server}]),
                           ok
                   end},

         #{desc => "order client continue (shutdown)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, shutdown),
                           ok
                   end},
         #{desc => "await client ready (shutdown)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, shutdown,
                                            [{server, Server}]),
                           ok
                   end},

         #{desc => "order server continue (first recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await server ready (first recv)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv,
                                            [{client, Client}]),
                           ok
                   end},

         #{desc => "order server continue (second recv)",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, recv),
                           ok
                   end},
         #{desc => "await server ready (second recv)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv,
                                            [{client, Client}]),
                           ok
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "order client continue (close)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, close),
                           ok
                   end},
         #{desc => "await client ready (close)",
           cmd  => fun(#{server := Server,
                         client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, close,
                                            [{server, Server}]),
                           ok
                   end},

         %% Terminations
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    i("start server evaluator"),
    ServerInitState = #{domain => maps:get(domain, InitState),
                        proto  => maps:get(proto,  InitState),
                        recv   => maps:get(recv,   InitState)},
    Server = ?SEV_START("server", ServerSeq, ServerInitState),

    i("start client evaluator"),
    ClientInitState = #{host   => local_host(),
                        domain => maps:get(domain, InitState),
                        proto  => maps:get(proto,  InitState),
                        send   => maps:get(send, InitState)},
    Client = ?SEV_START("client", ClientSeq, ClientInitState),

    i("start 'tester' evaluator"),
    TesterInitState = #{server => Server#ev.pid,
                        client => Client#ev.pid,
                        data   => maps:get(data, InitState)},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator"),
    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


sc_rs_tcp_client_start(Node, Send) ->
    Self = self(),
    Fun  = fun() -> sc_rs_tcp_client(Self, Send) end,
    erlang:spawn(Node, Fun).


sc_rs_tcp_client(Parent, Send) ->
    sc_rs_tcp_client_init(Parent),
    {ServerSA, Proto} = sc_rs_tcp_client_await_start(Parent),
    Domain   = maps:get(family, ServerSA),
    Sock     = sc_rs_tcp_client_create(Domain, Proto),
    Path     = sc_rs_tcp_client_bind(Sock, Domain),
    sc_rs_tcp_client_announce_ready(Parent, init),
    sc_rs_tcp_client_await_continue(Parent, connect),
    sc_rs_tcp_client_connect(Sock, ServerSA),
    sc_rs_tcp_client_announce_ready(Parent, connect),
    Data = sc_rs_tcp_client_await_continue(Parent, send),
    sc_rs_tcp_client_send(Sock, Send, Data),
    sc_rs_tcp_client_announce_ready(Parent, send),
    sc_rs_tcp_client_await_continue(Parent, shutdown),
    sc_rs_tcp_client_shutdown(Sock),
    sc_rs_tcp_client_announce_ready(Parent, shutdown),
    sc_rs_tcp_client_await_continue(Parent, close),
    sc_rs_tcp_client_close(Sock, Path),
    sc_rs_tcp_client_announce_ready(Parent, close),
    Reason = sc_rs_tcp_client_await_terminate(Parent),
    ?SEV_IPRINT("terminate"),
    exit(Reason).

sc_rs_tcp_client_init(Parent) ->
    put(sname, "rclient"),
    ?SEV_IPRINT("init"),
    _MRef = erlang:monitor(process, Parent),
    ok.

sc_rs_tcp_client_await_start(Parent) ->
    i("sc_rs_tcp_client_await_start -> entry"),
    ?SEV_AWAIT_START(Parent).

sc_rs_tcp_client_create(Domain, Proto) ->
    i("sc_rs_tcp_client_create -> entry"),
    case socket:open(Domain, stream, Proto) of
        {ok, Sock} ->
            Sock;
        {error, Reason} ->
            exit({open_failed, Reason})
    end.

sc_rs_tcp_client_bind(Sock, Domain) ->
    i("sc_rs_tcp_client_bind -> entry"),
    LSA = which_local_socket_addr(Domain),
    case socket:bind(Sock, LSA) of
        ok ->
            case socket:sockname(Sock) of
                {ok, #{family := local, path := Path}} ->
                    Path;
                {ok, _} ->
                    undefined;
                {error, Reason1} ->
                    exit({sockname, Reason1})
            end;
        {error, Reason} ->
            exit({bind, Reason})
    end.

sc_rs_tcp_client_announce_ready(Parent, Slogan) ->
    ?SEV_ANNOUNCE_READY(Parent, Slogan).

sc_rs_tcp_client_await_continue(Parent, Slogan) ->
    i("sc_rs_tcp_client_await_continue -> entry"),
    case ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan) of
        ok ->
            ok;
        {ok, Extra} ->
            Extra;
        {error, Reason} ->
            exit({await_continue, Slogan, Reason})
    end.


sc_rs_tcp_client_connect(Sock, ServerSA) ->
    i("sc_rs_tcp_client_connect -> entry"),
    case socket:connect(Sock, ServerSA) of
        ok ->
            ok;
        {error, Reason} ->
            exit({connect, Reason})
    end.

sc_rs_tcp_client_send(Sock, Send, Data) ->
    i("sc_rs_tcp_client_send -> entry"),
    try Send(Sock, Data) of
        ok ->
            ok;
        {error, Reason} ->
            exit({send, Reason})
    catch
        error : notsup = Reason : _ ->
            exit({skip, Reason})
    end.

sc_rs_tcp_client_shutdown(Sock) ->
    i("sc_rs_tcp_client_shutdown -> entry"),
    case socket:shutdown(Sock, write) of
        ok ->
            ok;
        {error, Reason} ->
            exit({shutdown, Reason})
    end.

sc_rs_tcp_client_close(Sock, Path) ->
    i("sc_rs_tcp_client_close -> entry"),
    case socket:close(Sock) of
        ok ->
            unlink_path(Path),
            ok;
        {error, Reason} ->
            ?SEV_EPRINT("failed closing: "
                        "~n   Reason: ~p", [Reason]),
            unlink_path(Path),
            {error, {close, Reason}}
    end.

sc_rs_tcp_client_await_terminate(Parent) ->
    i("sc_rs_tcp_client_await_terminate -> entry"),
    case ?SEV_AWAIT_TERMINATE(Parent, parent) of
        ok ->
            ok;
        {error, Reason} ->
            Reason
    end.


%% The handlers run on the same node as the server (the local node).

sc_rs_tcp_handler_start(Recv, Sock) ->
    Self     = self(),
    Fun      = fun() -> sc_rs_tcp_handler(Self, Recv, Sock) end,
    {Pid, _} = erlang:spawn_monitor(Fun),
    Pid.

sc_rs_tcp_handler(Parent, Recv, Sock) ->
    sc_rs_tcp_handler_init(Parent),
    sc_rs_tcp_handler_await(Parent, recv),
    ok = sc_rs_tcp_handler_recv(Recv, Sock, true),
    sc_rs_tcp_handler_announce_ready(Parent, recv, received),
    sc_rs_tcp_handler_await(Parent, recv),
    ok = sc_rs_tcp_handler_recv(Recv, Sock, false),
    sc_rs_tcp_handler_announce_ready(Parent, recv, closed),
    Reason = sc_rs_tcp_handler_await(Parent, terminate),
    exit(Reason).

sc_rs_tcp_handler_init(Parent) ->
    put(sname, "handler"),
    _MRef = erlang:monitor(process, Parent),
    ?SEV_IPRINT("started"),
    ?SEV_ANNOUNCE_READY(Parent, init),
    ok.

sc_rs_tcp_handler_await(Parent, terminate) ->
    ?SEV_IPRINT("await terminate"),
    ?SEV_AWAIT_TERMINATE(Parent, tester);
sc_rs_tcp_handler_await(Parent, Slogan) ->
    ?SEV_IPRINT("await ~w", [Slogan]),
    ?SEV_AWAIT_CONTINUE(Parent, parent, Slogan).

%% This should actually work - we leave it for now
sc_rs_tcp_handler_recv(Recv, Sock, First) ->
    ?SEV_IPRINT("recv"),
    try Recv(Sock) of
        {ok, _} when (First =:= true) ->
            ok;
        {error, closed} when (First =:= false) ->
            ok;
        {ok, _} ->
            ?SEV_IPRINT("unexpected success"),
            {error, unexpected_success};
        {error, Reason} = ERROR ->
            ?SEV_IPRINT("receive error: "
                        "~n   ~p", [Reason]),
            ERROR
    catch
        error : notsup = Reason : _ ->
            exit({skip, Reason});
        C:E:S ->
            ?SEV_IPRINT("receive failure: "
                        "~n   Class: ~p"
                        "~n   Error: ~p"
                        "~n   Stack: ~p", [C, E, S]),
            {error, {recv, C, E, S}}
    end.

sc_rs_tcp_handler_announce_ready(Parent, Slogan, Result) ->
    ?SEV_IPRINT("announce ready"),
    ?SEV_ANNOUNCE_READY(Parent, Slogan, Result),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recvmsg function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv4.

sc_rs_recvmsg_send_shutdown_receive_tcp4(_Config) when is_list(_Config) ->
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   ?TT(?SECS(30)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       case socket:recvmsg(Sock) of
                                           {ok, #{iov   := [Data]}} ->
                                               {ok, Data};
                                           {ok, #{addr  := _} = Msg} ->
                                               {error, {msg, Msg}};
                                           {error, _} = ERROR ->
                                               ERROR
                                       end
                               end,
                   Send      = fun(Sock, Data) when is_binary(Data) ->
                                  Msg = #{iov => [Data]},
                                  socket:sendmsg(Sock, Msg)
                               end,
                   InitState = #{domain => inet,
                                 proto  => tcp,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recvmsg function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is IPv6.

sc_rs_recvmsg_send_shutdown_receive_tcp6(_Config) when is_list(_Config) ->
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   ?TT(?SECS(10)),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       case socket:recvmsg(Sock) of
                                           {ok, #{iov   := [Data]}} ->
                                               {ok, Data};
                                           {ok, #{addr  := _} = Msg} ->
                                               {error, {msg, Msg}};
                                           {error, _} = ERROR ->
                                               ERROR
                                       end
                               end,
                   Send      = fun(Sock, Data) when is_binary(Data) ->
                                       Msg = #{iov => [Data]},
                                       socket:sendmsg(Sock, Msg)
                               end,
                   InitState = #{domain => inet6,
                                 proto  => tcp,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to test what happens when a socket is
%% remotely closed while the process is calling the recvmsg function.
%% The remote client sends data, then shutdown(write) and then the
%% reader attempts a recv.
%% Socket is UNix Domain (stream) socket.

sc_rs_recvmsg_send_shutdown_receive_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   {ok, CWD} = file:get_cwd(),
                   ?SEV_IPRINT("CWD: ~s", [CWD]),
                   MsgData   = ?DATA,
                   Recv      = fun(Sock) ->
                                       case socket:recvmsg(Sock) of
                                           %% On some platforms, the address
                                           %% *is* provided (e.g. linux)
                                           {ok, #{addr  := #{family := local},
                                                  iov   := [Data]}} ->
                                               {ok, Data};
                                           {ok, #{addr := _} = Msg} ->
                                               {error, {msg, Msg}};
                                           %% On some platforms, the address
                                           %% is *not* provided (e.g. FreeBSD)
                                           {ok, #{iov   := [Data]}} ->
                                               {ok, Data};
                                           {ok, Msg} ->
                                               {error, {msg, Msg}};
                                           {error, _} = ERROR ->
                                               ERROR
                                       end
                               end,
                   Send      = fun(Sock, Data) when is_binary(Data) ->
                                       Msg = #{iov => [Data]},
                                       socket:sendmsg(Sock, Msg)
                               end,
                   InitState = #{domain => local,
                                 proto  => default,
                                 recv   => Recv,
                                 send   => Send,
                                 data   => MsgData},
                   ok = sc_rs_send_shutdown_receive_tcp(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (*really* simply) test
%% "some" ioctl features.
%%

ioctl_simple1(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ioctl_requests()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_simple1(InitState)
           end).


do_ioctl_simple1(_State) ->
    Requests = socket:supports(ioctl_requests),
    ?P("Requests: ~p", [Requests]),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test "some" ioctl features.
%%

ioctl_simple2(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ioctl_requests(),
                   has_support_ioctl_gifconf()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_simple2(InitState)
           end).


do_ioctl_simple2(_State) ->
    i("create dummy dgram:UDP socket"),
    {ok, Sock1} = socket:open(inet, dgram, udp),
    i("perform simple ioctl (expect success)"),
    case socket:ioctl(Sock1, gifconf) of
        {ok, IfConf1} when is_list(IfConf1) ->
            i("=> success"),
            ok;
        {error, Reason1} ->
            i("error: unexpected failure: "
              "~n      ~p", [Reason1]),
            exit({unexpected_ioctl_failure, dgram, Reason1});
        Else11 ->
            i("error: unexpected result: "
              "~n      ~p", [Else11]),
            exit({unexpected_ioctl_result, dgram, Else11})
    end,
    i("close dummy dgram:UDP socket"),
    ok = socket:close(Sock1),
    i("perform simple ioctl (expect failure)"),
    case socket:ioctl(Sock1, gifconf) of
        {error, closed} ->
            i("=> success"),
            ok;
        Else12 ->
            i("error: unexpected result: "
              "~n      ~p", [Else12]),
            exit({unexpected_ioctl_result, dgram, Else12})
    end,

    i("create dummy stream:TCP socket"),
    {ok, Sock2} = socket:open(inet, stream, tcp),
    i("perform simple ioctl (expect success)"),
    case socket:ioctl(Sock2, gifconf) of
        {ok, IfConf2} when is_list(IfConf2) ->
            i("=> success"),
            ok;
        {error, Reason2} ->
            i("error: unexpected result: "
              "~n      ~p", [Reason2]),
            exit({unexpected_ioctl_failure, stream, Reason2});
        Else21 ->
            i("error: unexpected result: "
              "~n      ~p", [Else21]),
            exit({unexpected_ioctl_result, stream, Else21})
    end,
    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock2),
    i("perform simple ioctl (expect failure)"),
    case socket:ioctl(Sock2, gifconf) of
        {error, closed} ->
            i("=> success"),
            ok;
        Else22 ->
            i("error: unexpected result: "
              "~n      ~p", [Else22]),
            exit({unexpected_ioctl_result, stream, Else22})
    end,

    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This test case is intended to (simply) test the ioctl nread feature.
%%

ioctl_nread(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ioctl_requests(),
                   has_support_ioctl_nread()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_nread(InitState)
           end).

do_ioctl_nread(_) ->
    i("Get local socket address"),
    LSA = which_local_socket_addr(inet),
    i("Use LSA: ~p", [LSA]),

    i("Create dgram:UDP socket 1"),
    {ok, S1} = socket:open(inet, dgram, udp),

    i("Bind socket 1"),
    ok = socket:bind(S1, LSA),

    i("Create dgram:UDP socket 2"),
    {ok, S2} = socket:open(inet, dgram, udp),

    i("Bind socket 2"),
    ok = socket:bind(S2, LSA),

    i("Check data to read - expect 0 bytes"),
    {ok, 0} = socket:ioctl(S1, nread),
    
    i("Get socket 1 port number"),
    {ok, #{port := Port1}} = socket:sockname(S1),
    
    i("Send data to socket 1 (from socket 2)"),
    Data   = <<0,1,2,3,4,5,6,7,8,9>>,
    DataSz = byte_size(Data),
    ok = socket:sendto(S2, Data, LSA#{port => Port1}),
    
    i("Give it some time to arrive"),
    ?SLEEP(?SECS(1)),
    
    i("Verify that the correct amount of data (atleast ~p) is available", [DataSz]),
    case socket:ioctl(S1, nread) of
        {ok, DataSize} when (DataSize >= DataSz) ->
            i("Success: "
	      "~n   Min Size:    ~p"
	      "~n   Actual Size: ~p", [DataSz, DataSize]),
	    ok;
	{ok, DataSize} ->
            i("Unexpected data size: "
	      "~n   Expected (min) Size: ~p"
	      "~n   Actual Size:                ~p", [DataSz, DataSize]),
	    ct:fail({invalid_data_size, DataSz, DataSize})
    end,

    i("Read the data"),
    {ok, {_, Data}} = socket:recvfrom(S1),
    
    i("Verify that the data has been read (no more data is available)"),
    {ok, 0} = socket:ioctl(S1, nread),

    i("Cleanup"),
    socket:close(S1),
    socket:close(S2),

    i("Done"),
    ok.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These test case(s) are intended to (simply) test "some" ioctl get
%% request(s).
%%

ioctl_get_gifname(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifname()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifname(InitState)
           end).


do_ioctl_get_gifname(_State) ->
    i("create dummy dgram:UDP socket"),
    {ok, Sock} = socket:open(inet, dgram, udp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if names: "
      "~n      ~p", [IfNames]),
    _ = [case socket:ioctl(Sock, gifname, IfIdx) of
             {ok, IfName} ->
                 i("got expected interface name ~p for index ~w", [IfName, IfIdx]),
                 ok;
             {ok, OtherName} ->
                 %% Oups?!
                 i("<ERROR> got unexpected interface name for index ~w"
                   "~n      Expected: ~p"
                   "~n      Received: ~p", [IfIdx, IfName, OtherName]),
                 socket:close(Sock),
                 ?FAIL({unexpected_interface_name, IfIdx, OtherName, IfName});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface index ~w"
                   "~n      Reason: ~p", [IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy dgram:UDP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.


%% --- gifindex ---

ioctl_get_gifindex(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifindex()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifindex(InitState)
           end).


do_ioctl_get_gifindex(_State) ->
    i("create dummy dgram:UDP socket"),
    {ok, Sock} = socket:open(inet, dgram, udp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    _ = [case socket:ioctl(Sock, gifindex, IfName) of
             {ok, IfIdx} ->
                 i("got expected interface index ~w for interface ~p",
                   [IfIdx, IfName]),
                 ok;
             {ok, OtherIdx} ->
                 %% Oups?!
                 i("<ERROR> got unexpected interface index for interface ~p"
                   "~n      Expected: ~p"
                   "~n      Received: ~p", [IfName, IfIdx, OtherIdx]),
                 socket:close(Sock),
                 ?FAIL({unexpected_interface_index, IfName, OtherIdx, IfIdx});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p"
                   "~n      Reason: ~p", [IfName, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy dgram:UDP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifaddr ---

ioctl_get_gifaddr(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifaddr()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifaddr(InitState)
           end).


do_ioctl_get_gifaddr(_State) ->
    i("create dummy dgram:UDP socket"),
    {ok, Sock} = socket:open(inet, dgram, udp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifaddr, IfName) of
             {ok, #{family := Fam,
                    addr   := Addr}} ->
                 i("got (expected) socket address for interface ~p (~w): "
                   "~n      (~w) ~p", [IfName, IfIdx, Fam, Addr]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_addr, IfName, IfIdx, Crap});
             {error, eaddrnotavail = Reason} ->
                 i("got unexpected error for interface ~p (~w) => "
		   "SKIP interface"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 ignore;
             {error, eperm = Reason} ->
                 i("got unexpected error for interface ~p (~w) => "
		   "SKIP interface"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 ignore;
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy dgram:UDP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifdstaddr ---

ioctl_get_gifdstaddr(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifdstaddr()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifdstaddr(InitState)
           end).


do_ioctl_get_gifdstaddr(_State) ->
    Domain = inet_or_inet6(),
    LSA    = which_local_socket_addr(Domain),

    i("create and init listen stream:TCP socket"),
    {ok, LSock} = socket:open(Domain, stream, tcp),
    ok = socket:bind(LSock, LSA#{port => 0}),
    ok = socket:listen(LSock),
    {ok, #{port := LPort}} = socket:sockname(LSock),
    
    i("create and init connection stream:TCP socket"),
    {ok, CSock} = socket:open(Domain, stream, tcp),

    i("attempt connect (nowait)"),
    {ok, ASock} =
        case socket:connect(CSock, LSA#{port => LPort}, nowait) of
            ok ->
                i("connected - accept connection"),
                socket:accept(LSock);
            {select, {select_info, _Tag, SelectHandle} = _SelectInfo} ->
                i("selected - attempt accept"),
                {ok, AS} = socket:accept(LSock),

                i("await connection ready"),
                receive
                    {'$socket', CSock, select, SelectHandle} ->
                        i("select info - attempt complete connection"),
                        ok = socket:connect(CSock),
                        {ok, AS}
                end
        end,

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    verify_gifdstaddr(ASock, "accept",  IfNames),
    verify_gifdstaddr(CSock, "connect", IfNames),
    verify_gifdstaddr(LSock, "listen",  IfNames),

    i("close socket(s)"),
    _ = socket:close(CSock),
    _ = socket:close(ASock),
    _ = socket:close(LSock),

    i("done"),
    ok.



verify_gifdstaddr(_Sock, _Prefix, []) ->
    ok;
verify_gifdstaddr(Sock, Prefix, [{IfIdx, IfName} | IfNames]) ->
    i("[~s] attempt verify gifdstaddr for interface ~s (~w)",
      [Prefix, IfName, IfIdx]),
    verify_gifdstaddr(Sock, Prefix, IfIdx, IfName),
    verify_gifdstaddr(Sock, Prefix, IfNames).

verify_gifdstaddr(Sock, Prefix, IfIdx, IfName) ->
    {OsFam, OsName} = os:type(),
    case socket:ioctl(Sock, gifdstaddr, IfName) of
        {ok, #{family := Fam,
               addr   := Addr}} ->
            i("[~s] got (expected) (dest) socket address "
              "for interface ~p (~w): "
              "~n      (~w) ~p", [Prefix, IfName, IfIdx, Fam, Addr]),
            ok;
        {ok, Crap} ->
        %% Oups?!
            i("<ERROR> [~s] got unexpected result for interface ~p (~w)"
              "~n      ~p", [Prefix, IfName, IfIdx, Crap]),
            socket:close(Sock),
            ?FAIL({unexpected_addr, Prefix, IfName, IfIdx, Crap});
        {error, IgnoredReason} when IgnoredReason =:= eaddrnotavail;
                                    IgnoredReason =:= eperm;
                                    IgnoredReason =:= enotty ->
            i("[~s] got unexpected error for interface ~p (~w) => "
              "SKIP interface"
              "~n      Reason: ~p", [Prefix, IfName, IfIdx, IgnoredReason]),
            ignore;
	{error, einval = Reason} when (OsFam =:= unix) andalso
                                      ((OsName =:= darwin) orelse 
                                       (OsName =:= freebsd) orelse 
                                       (OsName =:= netbsd) orelse 
                                       (OsName =:= openbsd)) ->
	    i("[~s] got unexpected error for interface ~p (~w) => "
	      "SKIP interface"
	      "~n      Reason: ~p", [Prefix, IfName, IfIdx, Reason]),
	    ignore;
        {error, Reason} ->
            i("<ERROR> got unexpected error for interface ~p (~w)"
              "~n      Reason: ~p", [IfName, IfIdx, Reason]),
            socket:close(Sock),
            ?FAIL({unexpected_failure, Prefix, IfName, IfIdx, Reason})
    end.
    



%% --- gifbrdaddr ---

ioctl_get_gifbrdaddr(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifbrdaddr()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifbrdaddr(InitState)
           end).


do_ioctl_get_gifbrdaddr(_State) ->
    Domain = inet_or_inet6(),
    LSA    = which_local_socket_addr(Domain),

    i("create and init listen stream:TCP socket"),
    {ok, LSock} = socket:open(Domain, stream, tcp),
    ok = socket:bind(LSock, LSA#{port => 0}),
    ok = socket:listen(LSock),
    {ok, #{port := LPort}} = socket:sockname(LSock),
    
    i("create and init connection stream:TCP socket"),
    {ok, CSock} = socket:open(Domain, stream, tcp),

    i("attempt connect (nowait)"),
    {ok, ASock} =
        case socket:connect(CSock, LSA#{port => LPort}, nowait) of
            ok ->
                i("connected - accept connection"),
                socket:accept(LSock);
            {select, {select_info, _Tag, SelectHandle} = _SelectInfo} ->
                i("selected - attempt accept"),
                {ok, AS} = socket:accept(LSock),

                i("await connection ready"),
                receive
                    {'$socket', CSock, select, SelectHandle} ->
                        i("select info - attempt complete connection"),
                        ok = socket:connect(CSock),
                        {ok, AS}
                end
        end,

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    verify_gifbrdaddr(ASock, "accept",  IfNames),
    verify_gifbrdaddr(CSock, "connect", IfNames),
    verify_gifbrdaddr(LSock, "listen",  IfNames),

    i("close socket(s)"),
    _ = socket:close(CSock),
    _ = socket:close(ASock),
    _ = socket:close(LSock),

    i("done"),
    ok.


verify_gifbrdaddr(_Sock, _Prefix, []) ->
    ok;
verify_gifbrdaddr(Sock, Prefix, [{IfIdx, IfName} | IfNames]) ->
    i("[~s] attempt verify gifbrdaddr for interface ~s (~w)",
      [Prefix, IfName, IfIdx]),
    verify_gifbrdaddr(Sock, Prefix, IfIdx, IfName),
    verify_gifbrdaddr(Sock, Prefix, IfNames).

verify_gifbrdaddr(Sock, Prefix, IfIdx, IfName) ->
    {OsFam, OsName} = os:type(),
    case socket:ioctl(Sock, gifbrdaddr, IfName) of
        {ok, #{family := Fam,
               addr   := Addr}} ->
            i("[~s] got (expected) (broadcast) socket address for "
              "interface ~p (~w): "
              "~n      (~w) ~p", [Prefix, IfName, IfIdx, Fam, Addr]),
            ok;
        {ok, Crap} ->
            %% Oups?!
            i("<ERROR> [~s] got unexpected result for interface ~p (~w)"
              "~n      ~p", [Prefix, IfName, IfIdx, Crap]),
            socket:close(Sock),
            ?FAIL({unexpected_addr, IfName, IfIdx, Crap});
        {error, IgnoredReason} when IgnoredReason =:= eaddrnotavail;
                                    IgnoredReason =:= eperm;
                                    IgnoredReason =:= enotty ->
            i("[~s] got unexpected error for interface ~p (~w) => "
             "SKIP interface"
              "~n      Reason: ~p", [Prefix, IfName, IfIdx, IgnoredReason]),
            ignore;
	{error, einval = Reason} when (OsFam =:= unix) andalso
                                      ((OsName =:= darwin) orelse
                                       (OsName =:= freebsd) orelse
                                       (OsName =:= netbsd) orelse
                                       (OsName =:= openbsd)) ->
	    i("[~s] got unexpected error for interface ~p (~w) => "
	      "SKIP interface"
	      "~n      Reason: ~p", [Prefix, IfName, IfIdx, Reason]),
	    ignore;
        {error, Reason} ->
            i("<ERROR> [~s] got unexpected error for interface ~p (~w)"
              "~n      Reason: ~p", [Prefix, IfName, IfIdx, Reason]),
            socket:close(Sock),
            ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
    end.



%% --- gifnetmask ---

ioctl_get_gifnetmask(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifnetmask()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifnetmask(InitState)
           end).


do_ioctl_get_gifnetmask(_State) ->
    i("create dummy stream:TCP socket"),
    {ok, Sock} = socket:open(inet, stream, tcp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifnetmask, IfName) of
             {ok, #{family := Fam,
                    addr   := Addr}} ->
                 i("got (expected) (netmask) socket address for interface ~p (~w): "
                   "~n      (~w) ~p", [IfName, IfIdx, Fam, Addr]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_addr, IfName, IfIdx, Crap});
             {error, eaddrnotavail = Reason} ->
                 i("got unexpected error for interface ~p (~w) => SKIP interface"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 ignore;
	     {error, eperm = Reason} ->
		 i("got unexpected error for interface ~p (~w) => "
		   "SKIP interface"
		   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
		 ignore;
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifmtu ---

ioctl_get_gifmtu(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifmtu()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifmtu(InitState)
           end).


do_ioctl_get_gifmtu(_State) ->
    i("create dummy stream:TCP socket"),
    {ok, Sock} = socket:open(inet, stream, tcp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifmtu, IfName) of
             {ok, MTU} when is_integer(MTU) ->
                 i("got (expected) MTU for interface ~p (~w): "
                   "~n      ~p", [IfName, IfIdx, MTU]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_mtu, IfName, IfIdx, Crap});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifhwaddr ---

ioctl_get_gifhwaddr(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifhwaddr()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifhwaddr(InitState)
           end).


do_ioctl_get_gifhwaddr(_State) ->
    i("create dummy dgram:UDP socket"),
    {ok, Sock} = socket:open(inet, dgram, udp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifhwaddr, IfName) of
             {ok, #{family := Fam,
                    addr   := Addr}} when is_atom(Fam) orelse is_integer(Fam) ->
                 i("got (expected) socket address for interface ~p (~w): "
                   "~n      (~w) ~p", [IfName, IfIdx, Fam, Addr]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                         i("<ERROR> got unexpected result for interface ~p (~w)"
                           "~n      ~p", [IfName, IfIdx, Crap]),
                         socket:close(Sock),
                         ?FAIL({unexpected_addr, IfName, IfIdx, Crap});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy dgram:UDP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- giftxqlen ---

ioctl_get_giftxqlen(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_giftxqlen()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_giftxqlen(InitState)
           end).


do_ioctl_get_giftxqlen(_State) ->
    i("create dummy stream:TCP socket"),
    {ok, Sock} = socket:open(inet, stream, tcp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, giftxqlen, IfName) of
             {ok, QLen} when is_integer(QLen) ->
                 i("got (expected) TX QLen for interface ~p (~w): "
                   "~n      ~p", [IfName, IfIdx, QLen]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_mtu, IfName, IfIdx, Crap});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifflags ---

ioctl_get_gifflags(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifflags()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifflags(InitState)
           end).


do_ioctl_get_gifflags(_State) ->
    i("create dummy stream:TCP socket"),
    {ok, Sock} = socket:open(inet, stream, tcp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    AllFlags = [Flag || {Flag, Supported} <-
                            socket:supports(ioctl_flags), Supported =:= true],

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifflags, IfName) of
             {ok, Flags} when is_list(Flags) ->
                 i("got  flags for interface ~p (~w): "
                   "~n      ~p", [IfName, IfIdx, Flags]),
                 case Flags -- AllFlags of
                     [] ->
                         i("flags accounted for"),
                         ok;
                     ExtraFlags ->
                         i("<ERROR> got superfluous flags for interface ~p (~w)"
                           "~n      Received Flags:      ~p"
                           "~n      Superfluous Flags:   ~p"
                           "~n      All Supported Flags: ~p"
                           "~n", [IfName, IfIdx, Flags, ExtraFlags, AllFlags]),
                         socket:close(Sock),
                         ?FAIL({unexpected_superfluous_flags,
                                IfName, IfIdx, Flags, ExtraFlags, AllFlags})
                 end;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_mtu, IfName, IfIdx, Crap});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.



%% --- gifmap ---

ioctl_get_gifmap(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_net_if_names(),
                   has_support_ioctl_gifmap()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_ioctl_get_gifmap(InitState)
           end).


do_ioctl_get_gifmap(_State) ->
    i("create dummy stream:TCP socket"),
    {ok, Sock} = socket:open(inet, stream, tcp),

    i("get if names"),
    {ok, IfNames} = net:if_names(),

    i("try ioctl all if indexes: "
      "~n      ~p", [IfNames]),
    %% This a *very* simple test...
    %% ...just to check that we actually get an socket address
    _ = [case socket:ioctl(Sock, gifmap, IfName) of
             {ok, Map} when is_map(Map) ->
                 i("got (expected) map for interface ~p (~w): "
                   "~n      ~p", [IfName, IfIdx, Map]),
                 ok;
             {ok, Crap} ->
                 %% Oups?!
                 i("<ERROR> got unexpected result for interface ~p (~w)"
                   "~n      ~p", [IfName, IfIdx, Crap]),
                 socket:close(Sock),
                 ?FAIL({unexpected_mtu, IfName, IfIdx, Crap});
             {error, Reason} ->
                 i("<ERROR> got unexpected error for interface ~p (~w)"
                   "~n      Reason: ~p", [IfName, IfIdx, Reason]),
                 socket:close(Sock),
                 ?FAIL({unexpected_failure, IfName, IfIdx, Reason})
         end || {IfIdx, IfName} <- IfNames],

    i("close dummy stream:TCP socket"),
    ok = socket:close(Sock),

    i("done"),
    ok.




%% --- tcp_info ---

ioctl_tcp_info(_Config) when is_list(_Config) ->
    ?TT(?SECS(15)),
    Cond = fun() ->
		   has_support_ioctl_tcp_info()
	   end,
    TC   = fun() ->
		   Domain = inet,
		   case which_local_addr(Domain) of
		       {ok, Addr} ->
			   State = #{domain => Domain,
				     laddr  => Addr},
			   do_ioctl_tcp_info(State);
		       {error, Reason} ->
			   skip({no_local_addr, Reason})
		   end
	   end,
    tc_try(?FUNCTION_NAME, Cond, TC).


do_ioctl_tcp_info(#{domain := Domain,
		    laddr  := LAddr} = _State) ->
    LSA = #{family => Domain, addr => LAddr},

    i("[server] create stream:TCP server listen socket"),
    {ok, L} = socket:open(Domain, stream, tcp),

    i("[server] bind to ~p", [LSA]),
    ok = socket:bind(L, LSA),

    i("[server] make listen socket"),
    ok = socket:listen(L),

    i("[server] get sockname"),
    {ok, SSA} = socket:sockname(L),


    i("[client] create stream:TCP socket"),
    {ok, C} = socket:open(Domain, stream, tcp),

    i("[client] bind to ~p", [LSA]),
    ok = socket:bind(C, LSA),

    i("[client] connect to server: "
      "~n   ~p", [SSA]),
    ok = socket:connect(C, SSA),

    
    i("[server] accept connection"),
    {ok, A} = socket:accept(L),

    
    i("[client] try get tcp info"),
    case socket:ioctl(C, tcp_info) of
	{ok, CTcpInfo0} ->
	    i("[client] tcp info: "
	      "~n   ~p"
	      "~n", [CTcpInfo0]),
	    ok;
	{error, CReason0} ->
	    i("[client] failed get TCP info: "
	      "~n   ~p"
	      "~n", [CReason0]),
	    skip({client_tcp_info, 0, CReason0})
    end,

    i("[server] try get tcp info"),
    case socket:ioctl(A, tcp_info) of
	{ok, ATcpInfo0} ->
	    i("[server] tcp info: "
	      "~n   ~p"
	      "~n", [ATcpInfo0]),
	    ok;
	{error, AReason0} ->
	    i("[server] failed get TCP info: "
	      "~n   ~p"
	      "~n", [AReason0]),
	    skip({server_tcp_info, 0, AReason0})
    end,


    Data = <<0,1,2,3,4,5,6,7,8,9,
	     0,1,2,3,4,5,6,7,8,9>>,
    DSz  = byte_size(Data),
    i("[client] send some data"),
    ok = socket:send(C, Data),

    i("[client] try get tcp info (verify bytes-out)"),
    case socket:ioctl(C, tcp_info) of
	{ok, #{bytes_out := BytesOut} = CTcpInfo1} when (BytesOut =:= DSz) ->
	    i("[client] tcp info: "
	      "~n   ~p"
	      "~n", [CTcpInfo1]),
	    ok;
	{error, CReason1} ->
	    i("[client] failed get TCP info: "
	      "~n   ~p"
	      "~n", [CReason1]),
	    skip({client_tcp_info, 1, CReason1})
    end,

    i("[server] try get tcp info (verify bytes-in)"),
    case socket:ioctl(A, tcp_info) of
	{ok, #{bytes_in := BytesIn} = ATcpInfo1} when (BytesIn =:= DSz) ->
	    i("[server] tcp info: "
	      "~n   ~p"
	      "~n", [ATcpInfo1]),
	    ok;
	{error, AReason1} ->
	    i("[server] failed get TCP info: "
	      "~n   ~p"
	      "~n", [AReason1]),
	    skip({server_tcp_info, 1, AReason1})
    end,


    i("[server] recv some data"),
    {ok, _} = socket:recv(A),

    i("[client] try get tcp info"),
    {ok, CConnTime2} =
	case socket:ioctl(C, tcp_info) of
	    {ok, #{connection_time := CCT2} = CTcpInfo2} ->
		i("[client] tcp info: "
		  "~n   ~p"
		  "~n", [CTcpInfo2]),
		{ok, CCT2};
	    {error, CReason2} ->
		i("[client] failed get TCP info: "
		  "~n   ~p"
		  "~n", [CReason2]),
		skip({client_tcp_info, 2, CReason2})
	end,

    i("[server] try get tcp info"),
    {ok, AConnTime2} =
	case socket:ioctl(A, tcp_info) of
	    {ok, #{connection_time := ACT2} = ATcpInfo2} ->
		i("[server] tcp info: "
		  "~n   ~p"
		  "~n", [ATcpInfo2]),
		{ok, ACT2};
	    {error, AReason2} ->
		i("[server] failed get TCP info: "
		  "~n   ~p"
		  "~n", [AReason2]),
		skip({server_tcp_info, 2, AReason2})
	end,

    SLEEP = ?SECS(5),
    ?SLEEP(SLEEP),

    i("[client] try get tcp info (verify connection time)"),
    case socket:ioctl(C, tcp_info) of
	{ok, #{connection_time := CCT3} = CTcpInfo3}
	  when (CCT3 >= (SLEEP+CConnTime2)) ->
	    i("[client] tcp info: "
	      "~n   ~p"
	      "~n", [CTcpInfo3]),
	    ok;
	{error, CReason3} ->
	    i("[client] failed get TCP info: "
	      "~n   ~p"
	      "~n", [CReason3]),
	    skip({client_tcp_info, 3, CReason3})
    end,

    i("[server] try get tcp info (verify connection time)"),
    case socket:ioctl(A, tcp_info) of
	{ok, #{connection_time := ACT3} = ATcpInfo3} 
	  when (ACT3 >= (SLEEP+AConnTime2)) ->
	    i("[server] tcp info: "
	      "~n   ~p"
	      "~n", [ATcpInfo3]),
	    ok;
	{error, AReason3} ->
	    i("[server] failed get TCP info: "
	      "~n   ~p"
	      "~n", [AReason3]),
	    skip({server_tcp_info, 3, AReason3})
    end,


    i("cleanup"),
    ok = socket:close(L),
    ok = socket:close(A),
    ok = socket:close(C),

    i("done"),
    ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                            TICKETS                                  %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create several acceptor processes (processes that calls socket:accept/1)
%% and then a couple of clients connects to them without any problems.
%% TCP, IPv4.
otp16359_maccept_tcp4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(otp16359_maccept_tcp4,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain   => inet,
                                 protocol => tcp},
                   ok = otp16359_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create several acceptor processes (processes that calls socket:accept/1)
%% and then a couple of clients connects to them without any problems.
%% TCP, IPv6.
otp16359_maccept_tcp6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(otp16359_maccept_tcp6,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 protocol => tcp},
                   ok = otp16359_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create several acceptor processes (processes that calls socket:accept/1)
%% and then a couple of clients connects to them without any problems.
%% TCP, UNix Domain Sockets.
otp16359_maccept_tcpL(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(otp16359_maccept_tcpL,
           fun() -> has_support_unix_domain_socket() end,
           fun() ->
                   InitState = #{domain   => local,
                                 protocol => default},
                   ok = otp16359_maccept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp16359_maccept_tcp(InitState) ->
    PrimAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           Tester = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "which local address",
           cmd  => fun(#{domain := Domain} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           ?SEV_IPRINT("LSA: ~p", [LSA]),
                           {ok, State#{lsa => LSA}}
                   end},
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain,
                         protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{lsock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{domain := local,
                         lsock  := LSock,
                         lsa    := LSA} = _State) ->
                           case socket:bind(LSock, LSA) of
                               ok ->
                                   ok; % We do not care about the port for local
                               {error, _} = ERROR ->
                                   ERROR
                           end;
                      (#{lsock := LSock, lsa := LSA} = State) ->
                           case sock_bind(LSock, LSA) of
                               ok ->
                                   Port = sock_port(LSock),
                                   {ok, State#{lport => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{lsock := LSock}) ->
                           socket:listen(LSock)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{domain := local,
                         tester := Tester,
                         lsock  := LSock,
                         lsa    := #{path := Path}}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, {LSock, Path}),
                           ok;
                      (#{lsock := LSock, lport := LPort, tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, {LSock, LPort}),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept (with success)",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("Expected (accept) success: "
                                               "~n   ~p", [Sock]),
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_TERMINATE(Tester, tester),
                           ok
                   end},
         %% *** Close (connect) socket ***
         #{desc => "close (connect) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},
         %% *** Close (listen) socket ***
         #{desc => "close (listen) socket",
           cmd  => fun(#{domain := local,
                         lsock  := Sock,
                         lsa    := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(lsa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(lsock, State1)};
                      (#{lsock := LSock} = State) ->
                           sock_close(LSock),
                           {ok, maps:remove(lsock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SecAcceptorSeq =
        [
         %% *** Init part ***
         #{desc => "await start",
           cmd  => fun(State) ->
                           {Tester, LSock} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester,
                                       lsock  => LSock}}
                           
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test part ***
         #{desc => "await continue (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "attempt to accept",
           cmd  => fun(#{lsock := LSock} = State) ->
                           case socket:accept(LSock) of
                               {ok, Sock} ->
                                   ?SEV_IPRINT("Expected (accept) success: "
                                               "~n   ~p", [Sock]),
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         %% *** Close (connect) socket ***
         #{desc => "close (connect) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           sock_close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    ClientSeq =
        [
         %% *** Wait for start order ***
         #{desc => "await start (from tester)",
           cmd  => fun(#{domain := local} = State) ->
                           {Tester, Path} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_path => Path}};
                      (State) ->
                           {Tester, Port} = ?SEV_AWAIT_START(),
                           {ok, State#{tester => Tester, server_port => Port}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester}) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** The init part ***
         #{desc => "which server (local) address",
           cmd  => fun(#{domain      := local = Domain,
                         server_path := Path} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = #{family => Domain, path => Path},
                           {ok, State#{lsa => LSA, server_sa => SSA}};
                      (#{domain := Domain, server_port := Port} = State) ->
                           LSA = which_local_socket_addr(Domain),
                           SSA = LSA#{port => Port},
                           {ok, State#{lsa => LSA, server_sa => SSA}}
                   end},
         #{desc => "create socket",
           cmd  => fun(#{domain   := Domain,
                         protocol := Proto} = State) ->
                           case socket:open(Domain, stream, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock := Sock, lsa := LSA} = _State) ->
                           case sock_bind(Sock, LSA) of
                               ok ->
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},

         %% *** The actual test ***
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester} = _State) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "connect to server",
           cmd  => fun(#{sock := Sock, server_sa := SSA}) ->
                           case socket:connect(Sock, SSA) of
                               ok ->
                                   ?SEV_IPRINT("connected", []),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("failed connect: "
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         %% *** Termination ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{domain := local,
                         sock   := Sock,
                         lsa    := #{path := Path}} = State) ->
                           ok = socket:close(Sock),
                           State1 =
                               unlink_path(Path,
                                           fun() ->
                                                   maps:remove(lsa, State)
                                           end,
                                           fun() -> State end),
                           {ok, maps:remove(sock, State1)};
                      (#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
                           {ok, maps:remove(sock, State)}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    TesterSeq =
        [
         %% Init part
         #{desc => "monitor prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 1",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 2",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},
         #{desc => "monitor client 3",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           _MRef = erlang:monitor(process, Pid),
                           ok
                   end},


         %% Start the prim-acceptor
         #{desc => "start prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor ready (init)",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           {ok, {Sock, Port}} =
                               ?SEV_AWAIT_READY(Pid, prim_acceptor, init),
                           {ok, State#{lsock => Sock, lport => Port}}
                   end},

         %% Start sec-acceptor-1
         #{desc => "start sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 ready (init)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, init)
                   end},

         %% Start sec-acceptor-2
         #{desc => "start sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid, lsock := LSock} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LSock),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 ready (init)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, init)
                   end},

         %% Start client-1
         #{desc => "start client 1",
           cmd  => fun(#{client1 := Pid, lport := LPort} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LPort),
                           ok
                   end},
         #{desc => "await client 1 ready (init)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client1, init)
                   end},

         %% Start client-2
         #{desc => "start client 2",
           cmd  => fun(#{client2 := Pid, lport := LPort} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LPort),
                           ok
                   end},
         #{desc => "await client 2 ready (init)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client2, init)
                   end},

         %% Start client-3
         #{desc => "start client 3",
           cmd  => fun(#{client3 := Pid, lport := LPort} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, LPort),
                           ok
                   end},
         #{desc => "await client 3 ready (init)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client3, init)
                   end},

         %% Activate the acceptor(s)
         #{desc => "active prim-acceptor",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 1",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},
         #{desc => "active sec-acceptor 2",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, accept),
                           ok
                   end},

         ?SEV_SLEEP(?SECS(1)),

         %% Activate the client(s)
         #{desc => "active client 1",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},

         ?SEV_SLEEP(100),

         #{desc => "active client 2",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},

         ?SEV_SLEEP(100),

         #{desc => "active client 3",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Pid, connect),
                           ok
                   end},

         %% Await acceptor(s) completions
         #{desc => "await prim-acceptor ready (accept)",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, prim_acceptor, accept)
                   end},
         #{desc => "await sec-acceptor 1 ready (accept)",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor1, accept)
                   end},
         #{desc => "await sec-acceptor 2 ready (accept)",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, sec_acceptor2, accept)
                   end},
         #{desc => "await client 1 ready (connect)",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client1, connect)
                   end},
         #{desc => "await client 2 ready (connect)",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client2, connect)
                   end},
         #{desc => "await client 3 ready (connect)",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_AWAIT_READY(Pid, client3, connect)
                   end},

         %% Terminate
         #{desc => "order client 1 to terminate",
           cmd  => fun(#{client1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 1 termination",
           cmd  => fun(#{client1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 2 to terminate",
           cmd  => fun(#{client2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 2 termination",
           cmd  => fun(#{client2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order client 3 to terminate",
           cmd  => fun(#{client3 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await client 3 termination",
           cmd  => fun(#{client3 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(client3, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order prim-acceptor to terminate",
           cmd  => fun(#{prim_acceptor := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await prim-acceptor termination",
           cmd  => fun(#{prim_acceptor := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(prim_acceptor, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 1 to terminate",
           cmd  => fun(#{sec_acceptor1 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 1 termination",
           cmd  => fun(#{sec_acceptor1 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor1, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order sec-acceptor 2 to terminate",
           cmd  => fun(#{sec_acceptor2 := Pid} = _State) ->
                           ?SEV_ANNOUNCE_TERMINATE(Pid),
                           ok
                   end},
         #{desc => "await sec-acceptor 2 termination",
           cmd  => fun(#{sec_acceptor2 := Pid} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Pid) of
                               ok ->
                                   State1 = maps:remove(sec_acceptor2, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         
         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],



    i("create prim-acceptor evaluator"),
    PrimAInitState = InitState,
    PrimAcceptor = ?SEV_START("prim-acceptor", PrimAcceptorSeq, PrimAInitState),

    i("create sec-acceptor 1 evaluator"),
    SecAInitState1 = maps:remove(domain, InitState),
    SecAcceptor1 = ?SEV_START("sec-acceptor-1", SecAcceptorSeq, SecAInitState1),

    i("create sec-acceptor 2 evaluator"),
    SecAInitState2 = SecAInitState1,
    SecAcceptor2 = ?SEV_START("sec-acceptor-2", SecAcceptorSeq, SecAInitState2),

    i("create client 1 evaluator"),
    ClientInitState1 = InitState,
    Client1 = ?SEV_START("client-1", ClientSeq, ClientInitState1),

    i("create client 2 evaluator"),
    ClientInitState2 = InitState,
    Client2 = ?SEV_START("client-2", ClientSeq, ClientInitState2),

    i("create client 3 evaluator"),
    ClientInitState3 = InitState,
    Client3 = ?SEV_START("client-3", ClientSeq, ClientInitState3),

    i("create tester evaluator"),
    TesterInitState = #{prim_acceptor => PrimAcceptor#ev.pid,
                        sec_acceptor1 => SecAcceptor1#ev.pid,
                        sec_acceptor2 => SecAcceptor2#ev.pid,
                        client1       => Client1#ev.pid,
                        client2       => Client2#ev.pid,
                        client3       => Client3#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    i("await evaluator(s)"),
    ok = ?SEV_AWAIT_FINISH([PrimAcceptor, SecAcceptor1, SecAcceptor2,
                            Client1, Client2, Client3,
                            Tester]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is to verify that we do not leak monitors.
otp18240_accept_mon_leak_tcp4(Config) when is_list(Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain    => inet,
                                 protocol  => tcp,
                                 num_socks => 10},
                   ok = otp18240_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is to verify that we do not leak monitors.
otp18240_accept_mon_leak_tcp6(Config) when is_list(Config) ->
    ?TT(?SECS(30)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain    => inet6,
                                 protocol  => tcp,
                                 num_socks => 10},
                   ok = otp18240_accept_tcp(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp18240_accept_tcp(#{domain    := Domain,
                      protocol  := Proto,
                      num_socks := NumSocks}) ->
    Self = self(),
    {Pid, Mon} = spawn_monitor(fun() ->
                                       otp18240_acceptor(Self,
                                                         Domain, Proto,
                                                         NumSocks)
                               end),
    otp18240_await_acceptor(Pid, Mon).

otp18240_await_acceptor(Pid, Mon) ->
    receive
	{'DOWN', Mon, process, Pid, ok} ->
	    i("acceptor successfully terminated"),
            ok;
	{'DOWN', Mon, process, Pid, {skip, _} = SKIP} ->
	    i("acceptor successfully terminated"),
            exit(SKIP);
        {'DOWN', Mon, process, Pid, Info} ->
            i("acceptor unexpected termination: "
              "~n   ~p", [Info]),
            exit({unexpected_result, Info})
    after 5000 ->
	    i("acceptor process (~p) info"
	      "~n   Refs: ~p"
	      "~n   Info: ~p",
	      [Pid, monitored_by(Pid), erlang:process_info(Pid)]),
	    otp18240_await_acceptor(Pid, Mon)
    end.

otp18240_acceptor(Parent, Domain, Proto, NumSocks) ->
    i("[acceptor] begin with: "
      "~n   Parent:   ~p"
      "~n   Domain:   ~p"
      "~n   Protocol: ~p", [Parent, Domain, Proto]),
    MonitoredBy0 = monitored_by(),
    ?SLEEP(?SECS(5)),
    Addr = case ?SLIB:which_local_host_info(Domain) of
               {ok, #{addr := A}} ->
                   A;
               {error, Reason} ->
                   exit({skip, Reason})
           end,
    {ok, LSock} = socket:open(Domain, stream, Proto,
                              #{use_registry => false}),
    ok = socket:bind(LSock, #{family => Domain, addr => Addr, port => 0}),
    ok = socket:listen(LSock, NumSocks),
    ?SLEEP(?SECS(5)),
    MonitoredBy1 = monitored_by(),
    i("[acceptor]: listen socket created"
      "~n   'Montored By' before listen socket: ~p"
      "~n   'Montored By' after listen socket:  ~p",
      [MonitoredBy0, MonitoredBy1]),

    [LSockMon] = MonitoredBy1 -- MonitoredBy0,

    i("[acceptor]: "
      "~n   Listen Socket Monitor: ~p"
      "~n   Listen Socket info:    ~p",
      [LSockMon, socket:info(LSock)]),

    {ok, #{port := Port}} = socket:sockname(LSock),

    i("[acceptor] create ~w clients (connectors)", [NumSocks]),
    _Clients = [spawn_link(fun() ->
                                   otp18240_client(CID,
                                                   Domain, Proto,
                                                   Addr, Port)
                           end) || CID <- lists:seq(1, NumSocks)],

    i("[acceptor] accept ~w connections", [NumSocks]),
    ServSocks = [otp18240_do_accept(AID, LSock) ||
                    AID <- lists:seq(1, NumSocks)],

    i("[acceptor] close accepted connections when: "
      "~n   Listen Socket info: ~p", [socket:info(LSock)]),
    _ = [otp18240_do_close(S) || S <- ServSocks],

    %% at this point in time there should be no monitors from NIFs,
    %% because we're not accepting anything
    i("[acceptor] check monitor status"),
    MonitoredBy2 = monitored_by(),
    MonitoredBy3 = MonitoredBy2 -- [Parent, LSockMon],
    i("[acceptor] monitor status: "
      "~n   UnRefs:       ~p"
      "~n   MonitoredBy2: ~p"
      "~n   MonitoredBy3: ~p",
      [[Parent, LSockMon], MonitoredBy2, MonitoredBy3]),
    if
        ([] =:= MonitoredBy3) ->
            i("[acceptor] done"),
            socket:close(LSock),
            exit(ok);
        true ->
            socket:close(LSock),
            i("[acceptor] Unexpected monitors: "
              "~n   ~p", [MonitoredBy2]),
            exit({unexpected_monitors, MonitoredBy2})
    end.


otp18240_client(ID, Domain, Proto, Addr, PortNo) ->
    i("[connector ~w] try create connector socket", [ID]),
    {ok, Sock} = socket:open(Domain, stream, Proto, #{use_registry => false}),
    ok = socket:bind(Sock, #{family => Domain, addr => Addr, port => 0}),
    %% ok = socket:setopt(Sock, otp, debug, true),
    i("[connector ~w] try connect", [ID]),
    case socket:connect(Sock,
                        #{family => Domain, addr => Addr, port => PortNo}) of
        ok ->
            i("[connector ~w] connected - now try recv", [ID]),
            case socket:recv(Sock) of
                {ok, Data} ->
                    i("[connector ~w] received unexpected data: "
                      "~n   ~p", [ID, Data]),
                    (catch socket:close(Sock)),
                    exit('unexpected data');
                {error, closed} ->
                    i("[connector ~w] expected socket close", [ID]);
                {error, Reason} ->
                    i("[connector ~w] unexpected error when reading: "
                      "~n   ~p", [ID, Reason]),
                    (catch socket:close(Sock))
            end;
        {error, {completion_status, #{info := invalid_netname = R} = Reason}} ->
            i("[connector ~w] failed connecting: "
              "~n   ~p", [ID, Reason]),
            (catch socket:close(Sock)),
            exit({skip, R});
        {error, {completion_status, invalid_netname = Reason}} ->
            i("[connector ~w] failed connecting: "
              "~n   ~p", [ID, Reason]),
            (catch socket:close(Sock)),
            exit({skip, Reason});
        {error, enetunreach = Reason} ->
            i("[connector ~w] failed connecting: "
              "~n   ~p", [ID, Reason]),
            (catch socket:close(Sock)),
            exit({skip, Reason});

        {error, Reason} ->
            i("[connector ~w] failed connecting: "
              "~n   ~p", [ID, Reason]),
            (catch socket:close(Sock))
    end,
    i("[connector ~w] done", [ID]),
    ok.


otp18240_do_accept(ID, LSock) ->
    i("[acceptor ~w] try accept", [ID]),
    {ok, Sock} = socket:accept(LSock),
    i("[acceptor ~w] accepted: ~p", [ID, Sock]),
    {ID, Sock}.


otp18240_do_close({ID, Sock}) ->
    i("[acceptor ~w] try close ~p", [ID, Sock]),
    case socket:close(Sock) of
	ok ->
	    i("[acceptor ~w] socket closed", [ID]),
	    ok;
	{error, Reason} ->
	    i("[acceptor ~w] failed close socket: "
	      "~n   ~p", [ID, Reason]),
	    error
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is to verify that we do not leak monitors.
otp18635(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_not_windows(),
                   has_support_ipv4()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_otp18635(InitState)
           end).


do_otp18635(_) ->
    Parent = self(),

    ?P("try create (listen) socket when"
       "~n   (gen socket) info: ~p"
       "~n   Sockets:           ~p",
       [socket:info(), socket:which_sockets()]),

    ?P("Get \"proper\" local socket address"),
    LSA = which_local_socket_addr(inet),

    {ok, LSock} = socket:open(inet, stream, #{use_registry => true}),

    ?P("bind (listen) socket to: "
       "~n   ~p", [LSA]),
    ok = socket:bind(LSock, LSA),

    ?P("make listen socket"),
    ok = socket:listen(LSock),

    ?P("get sockname for listen socket"),
    {ok, SA} = socket:sockname(LSock),

    %% ok = socket:setopt(LSock, otp, debug, true),

                                                % show handle returned from nowait accept
    ?P("try accept with timeout = nowait - expect select when"
       "~n   (gen socket) info: ~p"
       "~n   Sockets:           ~p",
       [socket:info(), socket:which_sockets()]),
    {select, {select_info, _, Handle}} = socket:accept(LSock, nowait),
    ?P("expected select result: "
       "~n   Select Handle:     ~p"
       "~n   (gen socket) info: ~p"
       "~n   Sockets:           ~p",
       [Handle, socket:info(), socket:which_sockets()]),

    ?SLEEP(?SECS(1)),

    %% perform a blocking accept that will fail (timeout)
    ?P("attempt accept with timeout = 500 - expect failure (timeout)"),
    {error, timeout} = socket:accept(LSock, 500),

    ?P("await abort message for the first accept call: "
       "~n   Select Handle:     ~p"
       "~n   (gen socket) info: ~p"
       "~n   Sockets:           ~p",
       [Handle, socket:info(), socket:which_sockets()]),
    receive
        {'$socket', LSock, abort, {Handle, cancelled}} ->
            ?P("received expected abort message"),
            ok
    end,

    %% spawn a client to connect
    ?P("spawn connector when"
       "~n   (gen socket) info:  ~p"
       "~n   Listen Socket info: ~p"
       "~n   Sockets:            ~p",
       [socket:info(), socket:info(LSock), socket:which_sockets()]),
    {Connector, MRef} =
        spawn_monitor(
          fun() ->
                  ?P("[connector] try create socket"),
                  {ok, CSock} = socket:open(inet, stream),
                  ?P("[connector] try connect: "
                     "~n   (server) ~p", [SA]),
                  ok = socket:connect(CSock, SA),
                  ?P("[connector] connected - inform parent"),
                  Parent ! {self(), connected},
                  ?P("[connector] await termination command"),
                  receive
                      {Parent, terminate} ->
                          ?P("[connector] terminate - close socket"),
                          (catch socket:close(CSock)),
                          exit(normal)
                  end
          end),

    ?P("await (connection-) confirmation from connector (~p)", [Connector]),
    receive
        {Connector, connected} ->
            ?P("connector connected"),
            ok
    end,

    %% We should *not* get *any* select messages;
    %% since the second call (that *replaced* the current active request)
    %% timeout out
    ?P("wait for a select message that should never come"),
    Result =
        receive
            {'$socket', LSock, select, AnyHandle} ->
                ?P("received unexpected select message when"
                   "~n   Unexpected Handle:  ~p"
                   "~n   (gen socket) info:  ~p"
                   "~n   Listen Socket info: ~p"
                   "~n   Sockets:            ~p",
                   [AnyHandle,
                    socket:info(), socket:info(LSock), socket:which_sockets()]),
                error
        after 5000 ->
                ?P("expected timeout"),
                ok
        end,

    ?P("try accept the waiting connection when"
       "~n   (gen socket) info:  ~p"
       "~n   Listen Socket info: ~p"
       "~n   Sockets:            ~p",
       [socket:info(), socket:info(LSock), socket:which_sockets()]),

    {ok, ASock} = socket:accept(LSock),

    ?P("connection accepted"
       "~n   (gen socket) info:    ~p"
       "~n   Accepted socket:      ~p"
       "~n   Accepted Socket info: ~p"
       "~n   Listen Socket info:   ~p"
       "~n   Sockets:              ~p",
       [socket:info(),
        ASock,
        socket:info(ASock),
        socket:info(LSock),
        socket:which_sockets()]),

    ?P("cleanup"),
    socket:close(LSock),
    socket:close(ASock),
    Connector ! {self(), terminate},
    receive
        {'DOWN', MRef, process, Connector, _} ->
            ?P("connector terminated"),
            ok
    end,

    ?SLEEP(?SECS(1)),

    ?P("done when"
       "~n   (gen socket) info: ~p"
       "~n   Sockets:           ~p", [socket:info(), socket:which_sockets()]),

    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case is to verify recv on UDP with timeout zero (0) on Windows.
otp19063(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   %% is_windows(),
                   has_support_ipv4()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_otp19063(InitState)
           end).


do_otp19063(_) ->
    Parent = self(),

    ?P("Get \"proper\" local socket address"),
    LSA0 = which_local_socket_addr(inet),
    LSA  = LSA0#{port => 0},
    


    %% --- recv ---

    ?P("[recv] - create (listen) socket"),
    {ok, LSock1} = socket:open(inet, stream),

    ?P("[recv] bind (listen) socket to: "
       "~n   ~p", [LSA]),
    ok = socket:bind(LSock1, LSA),

    ?P("[recv] make listen socket"),
    ok = socket:listen(LSock1),

    ?P("[recv] get sockname for listen socket"),
    {ok, SA1} = socket:sockname(LSock1),

    ?P("[recv] attempt a nowait-accept"),
    {Tag, Handle} =
        case socket:accept(LSock1, nowait) of
            {select, {select_info, _, SH}} ->
                {select, SH};
            {completion, {completion_info, _, CH}} ->
                {completion, CH}
        end,

    ?P("[recv] spawn the connector process"),
    {Connector, MRef} =
        spawn_monitor(
          fun() ->
                  ?P("[connector] try create socket"),
                  {ok, CSock1} = socket:open(inet, stream),
                  ?P("[connector] bind socket to: "
                     "~n   ~p", [LSA]),
                  ok = socket:bind(CSock1, LSA),
                  ?P("[connector] try connect: "
                     "~n   (server) ~p", [SA1]),
                  ok = socket:connect(CSock1, SA1),
                  ?P("[connector] connected - inform parent"),
                  Parent ! {self(), connected},
                  ?P("[connector] await termination command"),
                  receive
                      {Parent, terminate} ->
                          ?P("[connector] terminate - close socket"),
                          (catch socket:close(CSock1)),
                          exit(normal)
                  end
          end),

    ?P("[recv] await (connection-) confirmation from connector (~p)",
       [Connector]),
    receive
        {Connector, connected} ->
            ?P("[recv] connector connected"),
            ok
    end,

    ?P("[recv] receive the accepted socket"),
    ASock1 =
        receive
            {'$socket', LSock1, completion, {Handle, {ok, AS}}}
              when (Tag =:= completion) ->
                AS;
            {'$socket', LSock1, completion, {Handle, {error, Reason1C}}}
              when (Tag =:= completion) ->
                exit({accept_failed, Reason1C});
           {'$socket', LSock1, select, Handle}  ->
                case socket:accept(LSock1, nowait) of
                    {ok, AS} ->
                        AS;
                    {error, Reason1S} ->
                        exit({accept_failed, Reason1S})
                end
        end,

    ?SLEEP(?SECS(1)),

    ?P("[recv] try read"),
    case socket:recv(ASock1, 0, 0) of
        {error, timeout} ->
            ok;
        Any1             ->
            ?P("Unexpected result: ~p", [Any1]),
            exit({unexpected_recv_result, Any1})
    end,


    %% --- recvfrom ---

    ?P("[recvfrom} create socket"),
    {ok, Sock2} = socket:open(inet, dgram),

    ?P("[recvfrom} bind socket to: "
       "~n   ~p", [LSA]),
    ok = socket:bind(Sock2, LSA),

    ?SLEEP(?SECS(1)),

    ?P("[recvfrom] try read"),
    {error, timeout} = socket:recvfrom(Sock2, 1024, 0),


    %% --- recvmsg ---

    ?P("[recvmsg] create socket"),
    {ok, Sock3} = socket:open(inet, dgram),

    ?P("[recvmsg] bind socket to: "
       "~n   ~p", [LSA]),
    ok = socket:bind(Sock3, LSA),

    ?SLEEP(?SECS(1)),

    ?P("[recvmsg] try read"),
    {error, timeout} = socket:recvmsg(Sock3, 0),


    ?P("cleanup"),

    Connector ! {self(), terminate},
    receive
        {'DOWN', MRef, process, Connector, _} ->
            ?P("connector terminated"),
            ok
    end,
    _ = socket:close(ASock1),
    _ = socket:close(LSock1),
    _ = socket:close(Sock2),
    _ = socket:close(Sock3),

    ?P("done"),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp19251(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   is_windows(),
                   has_support_unix_domain_socket()
           end,
           fun() ->
                   InitState = #{},
                   ok = do_otp19251(InitState)
           end).


do_otp19251(_) ->

    %% Just be on the safe side
    ?P("pre cleanup"),
    file:delete("sock1"),
    file:delete("sock2"),

    %% Create first socket
    ?P("create (and bind) first socket"),
    {ok, Sock1} = socket:open(local, stream),
    ok          = socket:bind(Sock1,
                              #{family => local,
                                path   => <<"sock1">>}),

    %% Attempt first invalid connect
    ?P("try first invalid connect - expect failure"),
    case socket:connect(Sock1, #{family => local, path => <<"none">>}) of
        {error, econnrefused} ->
            ?P("expected failure"),
            ok;
        %% {error, enoent} ->
        %%     ?P("expected failure"),
        %%     ok;
        {error, #{info := econnrefused}} ->
            ?P("expected failure"),
            ok;
        {error, #{info := Reason1} = EEI1} ->
            ?P("unexpected failure reason: "
               "~n   ~p", [EEI1]),
            ?FAIL({unexpected_failure_reason, Reason1});
        ok ->
            ?FAIL(unexpected_success)
    end,


    %% Create second socket
    ?P("create (and bind) second socket"),
    {ok, Sock2} = socket:open(local, stream),
    ok          = socket:bind(Sock2,
                              #{family => local,
                                path   => <<"sock2">>}),

    %% Attempt second invalid connect
    ?P("try first invalid connect - expect failure"),
    case socket:connect(Sock2, #{family => local, path => <<"none">>}) of
        {error, econnrefused} ->
            ?P("expected failure"),
            ok;
        %% {error, enoent} ->
        %%     ?P("expected failure"),
        %%     ok;
        {error, #{info := econnrefused}} ->
            ?P("expected failure"),
            ok;
        {error, #{info := Reason2} = EEI2} ->
            ?P("unexpected failure reason: "
               "~n   ~p", [EEI2]),
            ?FAIL({unexpected_failure_reason, Reason2});
        ok ->
            ?FAIL(unexpected_success)
    end,

    %% And verify that we can still create more sockets:
    ?P("create more sockets - expect success"),
    {ok, Sock3} = socket:open(local, stream),
    {ok, Sock4} = socket:open(local, stream),

    %% Cleanup
    ?P("cleanup"),
    _ = socket:close(Sock4),
    _ = socket:close(Sock3),
    _ = socket:close(Sock2),
    _ = socket:close(Sock1),

    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests that results are correct when only parts of the requested
%% amount of data is available when using socket:recv.
%% Behaviour should be as expected for both STREAM and DGRAM sockets.
%% That is;
%%   STREAM: read until buffer is full or timeout
%%   DGRAM:  - do never wait for *more* data - return with *any* available data
%%           - only wait if there is currently *no* data.
%% Client connects to server and sends chunks of data with 1 sec between
%% sends. Server reads all in one read.

otp19469_read_all(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    Cond = fun() -> 
                   has_support_ipv4()
           end,
    Pre  = fun() ->
                   Fam = inet,
                   case ?KLIB:which_local_addr(Fam) of
                       {ok, LA} ->
                           LSA = #{family => Fam,
                                   addr   => LA},
                           #{lsa => LSA};
                       _ ->
                           skip(no_local_addr)
                   end
           end,
    TC   = fun(InitState) ->
                   ok = do_otp19469_read_all(InitState)
           end,
    Post = fun(_) ->
                   ok
           end,
    ?KLIB:tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

otp19469_ra_client(SSA, Data) when is_list(Data) ->
    ?P("[client] create socket"),
    Socket = case socket:open(inet, stream) of
		 {ok, Sock} ->
		     Sock;
		 {error, Reason} ->
		     exit({failed_create_socket, Reason})
	     end,
    %% We are on the same host as the server so we can reuse the
    %% SockAddr of the server, except for the port.
    ?P("[client] bind socket"),
    ok = socket:bind(Socket, SSA#{port => 0}),
    
    ?P("[client] connect to server"),
    ok = socket:connect(Socket, SSA),
    
    ok = otp19469_ra_client(Socket, Data, 1),

    _ = socket:close(Socket),

    ok.

otp19469_ra_client(_Socket, [] = _Data, _N) ->
    ?P("[client] all chunks sent"),
    ok;
otp19469_ra_client(Socket, [Chunk|Data], N) when is_binary(Chunk) ->
    ?P("[client,~w] sleep some before send", [N]),
    ?SLEEP(500),
    ?P("[client,~w] try send chunk (~w bytes)", [N, byte_size(Chunk)]),
    ok = socket:send(Socket, Chunk),
    ?P("[client,~w] chunk sent", [N]),
    otp19469_ra_client(Socket, Data, N + 1).


do_otp19469_read_all(#{lsa := LSA}) ->

    ?P("[ctrl] create listen socket"),
    {ok, S1} = socket:open(inet, stream),
    ?P("[ctrl] bind socket"),
    ok = socket:bind(S1, LSA#{port => 0}),
    {ok, SA1} = socket:sockname(S1),
    ?P("[ctrl] make listen socket"),
    ok = socket:listen(S1),
 
    ?P("[ctrl] create data"),
    Chunk   = <<"0123456789">>,
    Data    = lists:duplicate(10, Chunk),
    DataSz  = iolist_size(Data),
    DataBin = iolist_to_binary(Data),
    
    ?P("[ctrl] create data"),
    _ = spawn_link(fun() -> otp19469_ra_client(SA1, Data) end),
    		        
    ?P("[ctrl] accept connection"),
    {ok, S2} = socket:accept(S1),

    ?P("[ctrl] try read ~w bytes", [DataSz]),
    {ok, DataBin} = socket:recv(S2, DataSz),

    ?P("[ctrl] cleanup"),
    _ = socket:close(S2),
    _ = socket:close(S1),
    
    ?P("[ctrl] done"),			
    ok.


%% ----------------------------------------------------------------------

%% Tests that results are correct when only parts of the requested
%% amount of data is available when using socket:recv.
%% Behaviour should be as expected for both STREAM and DGRAM sockets.
%% That is;
%%   STREAM: read until buffer is full or timeout
%%   DGRAM:  - do never wait for *more* data - return with *any* available data
%%           - only wait if there is currently *no* data.

otp19469_read_part(Config) when is_list(Config) ->
    ?TT(?SECS(10)),
    Cond = fun() -> 
                   has_support_ipv4()
           end,
    Pre  = fun() ->
                   Fam = inet,
                   case ?KLIB:which_local_addr(Fam) of
                       {ok, LA} ->
                           LSA = #{family => Fam,
                                   addr   => LA},
                           #{lsa => LSA};
                       _ ->
                           skip(no_local_addr)
                   end
           end,
    TC   = fun(InitState) ->
                   ok = do_otp19469_read_part(InitState)
           end,
    Post = fun(_) ->
                   ok
           end,
    ?KLIB:tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


do_otp19469_read_part(#{lsa := LSA}) ->

    ?P("try stream"),
    ok = do_otp19469_stream(LSA),
    ?P("try dgram"),
    ok = do_otp19469_dgram(LSA),
    
    ?P("done"),
    ok.


do_otp19469_stream(#{family := Fam} = LSA) ->

    ?P("[stream] create listen socket"),
    {ok, S1} = socket:open(Fam, stream),
    ok = socket:bind(S1, LSA#{port => 0}),
    {ok, SA1} = socket:sockname(S1),
    ok = socket:listen(S1),

    ?P("[stream] create connector socket"),
    {ok, S2} = socket:open(Fam, stream),
    ok = socket:bind(S2, LSA#{port => 0}),
    ok = socket:connect(S2, SA1),

    ?P("[stream] accept connection (acceptor socket)"),
    {ok, S3} = socket:accept(S1),

    %% send and receive the same amount =>
    %% expect plain success
    Data = <<"0123456789">>,
    DataSz = byte_size(Data),
    ?P("[stream] try send and recv ~w bytes", [DataSz]),
    ok = socket:send(S2, Data),
    {ok, Data} = socket:recv(S3, DataSz, ?SECS(5)),

    %% send 10 bytes and try receive 20 bytes =>
    %% expect timeout with data
    Data = <<"0123456789">>,
    ?P("[stream] try send ~w bytes and recv ~w bytes", [DataSz, 2*DataSz]),
    ok = socket:send(S2, Data),
    put(debug, true),
    _ = socket:setopt(S3, otp, debug, true),
    case os:type() of
        {unix, _} ->
            %% As of 28 we are back to this behaviour
            {error, {timeout, Data}} = socket:recv(S3, 2*DataSz, ?SECS(5));
        {win32, _} ->
            {error, {timeout, Data}} = socket:recv(S3, 2*DataSz, ?SECS(5))
    end,
    _ = socket:setopt(S3, otp, debug, false),
    put(debug, false),

    ?P("[stream] cleanup"),
    _ = socket:close(S3),
    _ = socket:close(S2),
    _ = socket:close(S1),

    ?P("[stream] done"),
    ok.


do_otp19469_dgram(#{family := Fam} = LSA) ->

    ?P("[dgram] create socket 1"),
    {ok, S1} = socket:open(Fam, dgram),
    ok = socket:bind(S1, LSA#{port => 0}),
    {ok, SA1} = socket:sockname(S1),

    ?P("[dgram] create socket 2"),
    {ok, S2} = socket:open(Fam, dgram),
    ok = socket:bind(S2, LSA#{port => 0}),

    %% send and receive the same amount =>
    %% expect plain success
    Data = <<"0123456789">>,
    DataSz = byte_size(Data),
    ?P("[dgram] try send and recv ~w bytes", [DataSz]),
    ok = socket:sendto(S2, Data, SA1),
    {ok, Data} = socket:recv(S1, DataSz, ?SECS(5)),
    ?P("[dgram] success"),

    %% send 10 bytes and try receive 20 bytes =>
    %% expect plain success
    Data = <<"0123456789">>,
    ?P("[dgram] try send ~w bytes and recv ~w bytes", [DataSz, 2*DataSz]),
    ok = socket:sendto(S2, Data, SA1),
    %% On Windows the behaviour seems to depend on the (OS) version...
    case socket:recv(S1, 2*DataSz, ?SECS(5)) of
        {ok, Data} ->
            ?P("[dgram] success"),
            ok;
        {error, {timeout, Data}} ->
            ?P("[dgram] timeout success"),
            ok;
        {error, Reason} ->
            ?P("unexpected error result:"
               "~n   Reason: ~p", [Reason]),
            ?FAIL({unexpected_failure, Reason})
    end,

    ?P("[dgram] cleanup"),
    _ = socket:close(S2),
    _ = socket:close(S1),

    ?P("[dgram] done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_open(Domain, Type, Proto) ->
    try socket:open(Domain, Type, Proto) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            ?FAIL({open, Reason})
    catch
        C:E:S ->
            ?FAIL({open, C, E, S})
    end.


sock_bind(Sock, LSA) ->
    try socket:bind(Sock, LSA) of
        ok = OK ->
            OK;
        {error, eaddrnotavail = Reason} ->
            ?SEV_IPRINT("Address not available"),
            throw({skip, Reason});
        {error, _} = ERROR ->
            ERROR
    catch
        C:E:S ->
            ?FAIL({bind, C, E, S})
    end.

%% sock_connect(Sock, SockAddr) ->
%%     try socket:connect(Sock, SockAddr) of
%%         ok ->
%%             ok;
%%         {error, Reason} ->
%%             ?FAIL({connect, Reason})
%%     catch
%%         C:E:S ->
%%             ?FAIL({connect, C, E, S})
%%     end.
    
sock_port(S) ->
    case socket:sockname(S) of
        {ok, #{port := Port}} -> Port;
        {ok, #{}}             -> undefined
    end.

%% sock_sockname(Sock) ->
%%     try socket:sockname(Sock) of
%%         {ok, SockAddr} ->
%%             SockAddr;
%%         {error, Reason} ->
%%             ?FAIL({sockname, Reason})
%%     catch
%%         C:E:S ->
%%             ?FAIL({sockname, C, E, S})
%%     end.
    
sock_close(Sock) ->
    try socket:close(Sock) of
        ok ->
            ok;
        {error, Reason} ->
            i("sock_close -> error: ~p", [Reason]),
            ?FAIL({close, Reason})
    catch
        C:E:S ->
            i("sock_close -> failed: ~p, ~p, ~p", [C, E, S]),
            ?FAIL({close, C, E, S})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local_host() ->
    try net_adm:localhost() of
        Host when is_list(Host) ->
	    %% Convert to shortname if long
	    case string:tokens(Host, [$.]) of
		[H|_] ->
		    list_to_atom(H)
	    end
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    end.


%% The point of this is to "ensure" that paths from different test runs
%% don't clash.

mk_unique_path() ->
    ?SLIB:mk_unique_path().


which_local_socket_addr(local = Domain) ->
    #{family => Domain,
      path   => mk_unique_path()};

%% This gets the local socket address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_socket_addr(Domain) ->
    case ?KLIB:which_local_host_info(Domain) of
        {ok, [#{addr := Addr}|_]} ->
            #{family => Domain,
              addr   => Addr};
        {error, Reason} ->
            ?FAIL(Reason)
    end.


which_local_addr(local = _Domain) ->
    mk_unique_path();

%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    ?KLIB:which_local_addr(Domain).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

monitored_by() ->
    monitored_by(self()).
monitored_by(Pid) ->	
    {monitored_by, Refs} = erlang:process_info(Pid, monitored_by),
    Refs.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

etc_issue() ->
    string:trim(os:cmd("cat /etc/issue")).

is_old_fedora16() ->
    is_old_fedora16( etc_issue() ).

%% We actually only have one host running this, a slow VM.
is_old_fedora16("Fedora release 16 " ++ _) ->
    skip("Very slow VM");
is_old_fedora16(_) ->
    ok.

is_not_windows() ->
    case os:type() of
        {win32, nt} ->
            skip("This does not work on Windows");
        _ ->
            ok
    end.

is_windows() ->
    case os:type() of
        {win32, nt} ->
            ok;
        _ ->
            skip("Only test on Windows")
    end.


has_support_unix_domain_socket() ->
    case socket:is_supported(local) of
	true ->
	    ok;
	false ->
	    skip("Not supported")
    end.

has_support_sctp() ->
    case os:type() of
        {win32, _} ->
            skip("Not supported");
        {unix, netbsd} ->
            %% XXX We will have to investigate this later...
            skip("Not supported");
        _ ->
            case socket:is_supported(sctp) of
                true ->
                    ok;
                false ->
                    skip("Not supported")
            end
    end.


%% The idea is that this function shall test if the host has 
%% support for IPv4 or IPv6.
%% If not, there is no point in running corresponding tests.
%% Currently we just skip.
has_support_ipv4() ->
    ?KLIB:has_support_ipv4().

has_support_ipv6() ->
    ?KLIB:has_support_ipv6().

inet_or_inet6() ->
    try
        has_support_ipv4(),
        inet
    catch
        throw:{skip, _Reason} ->
            has_support_ipv6(),
            inet6
    end.

has_support_net_if_names() ->
    try net:if_names() of
        {ok, N} when is_list(N) ->
            ok;
        _ ->
            skip("Not supported: net:if_names()")
    catch
        error : notsup ->
            skip("Not supported: net")
    end.

has_support_ioctl_requests() ->
    try socket:supports(ioctl_requests) of
        Reqs when is_list(Reqs) ->
            ok;
        _ ->
            skip("Not supported: ioctl_requests")
    catch
        error : notsup ->
            skip("Not supported: socket")
    end.

has_support_ioctl_gifconf() ->
    has_support_ioctl_request(gifconf).

has_support_ioctl_nread() ->
    has_support_ioctl_request(nread).

has_support_ioctl_gifname() ->
    has_support_ioctl_request(gifname).

has_support_ioctl_gifindex() ->
    has_support_ioctl_request(gifindex).

has_support_ioctl_gifaddr() ->
    has_support_ioctl_request(gifaddr).

has_support_ioctl_gifdstaddr() ->
    has_support_ioctl_request(gifdstaddr).

has_support_ioctl_gifbrdaddr() ->
    has_support_ioctl_request(gifbrdaddr).

has_support_ioctl_gifnetmask() ->
    has_support_ioctl_request(gifnetmask).

has_support_ioctl_gifmtu() ->
    has_support_ioctl_request(gifmtu).

has_support_ioctl_gifhwaddr() ->
    has_support_ioctl_request(gifhwaddr).

has_support_ioctl_giftxqlen() ->
    has_support_ioctl_request(giftxqlen).

has_support_ioctl_gifflags() ->
    has_support_ioctl_request(gifflags).

has_support_ioctl_gifmap() ->
    has_support_ioctl_request(gifmap).

has_support_ioctl_tcp_info() ->
    has_support_ioctl_request(tcp_info).

has_support_ioctl_request(Req) when is_atom(Req) ->
    try socket:is_supported(ioctl_requests, Req) of
        true ->
            ok;
        false ->
            skip(?F("Not supported: ioctl_request: ~w", [Req]))
    catch
        error : notsup ->
            skip("Not supported: socket")
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unlink_path(Path) ->
    unlink_path(Path, fun() -> ok end, fun() -> ok end).

unlink_path(Path, Success, Failure)
  when is_function(Success, 0), is_function(Failure, 0) ->
    case Path of
        undefined ->
            ?SEV_IPRINT("not a path to unlink"),
                    Success();
        _ ->
            ?SEV_IPRINT("try unlink path: "
                        "~n   ~s", [Path]),
            case file:delete(Path) of
                ok ->
                    ?SEV_IPRINT("path unlinked: "
                                "~n   Path: ~s", [Path]),
                    Success();
                Error ->
                    ?SEV_EPRINT("unlink failed: "
                                "~n   Path: ~s"
                                "~n   Res:  ~p", [Path, Error]),
                    Failure()
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% not_supported(What) ->
%%     skip({not_supported, What}).

%% not_yet_implemented() ->
%%     skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% *** tc_try/2,3 ***
%% Case:      Basically the test case name
%% TCCondFun: A fun that is evaluated before the actual test case
%%            The point of this is that it can performs checks to
%%            see if we shall run the test case at all.
%%            For instance, the test case may only work in specific
%%            conditions.
%% FCFun:     The test case fun
tc_try(Case, TCFun) ->
    ?TC_TRY(Case, TCFun).

tc_try(Case, TCCondFun, TCFun) ->
    ?TC_TRY(Case, TCCondFun, TCFun).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_node(Name) ->
    start_node(Name, 5000).

start_node(Name, Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
    Pa   = filename:dirname(code:which(?MODULE)),
    Args = ["-pa", Pa,
            "-s", atom_to_list(?PROXY), "start", atom_to_list(node()),
            "-s", "global", "sync"],
    try ?CT_PEER(#{name      => Name,
                   wait_boot => Timeout,
                   args      => Args}) of
        {ok, Peer, Node} ->
            ?SEV_IPRINT("Started node ~p - now (global) sync", [Name]),
            global:sync(), % Again, just in case...
            ?SEV_IPRINT("ping proxy"),
            case ?PPING(Node) of
                {error, Reason} ->
                    skip({ping_failed, Reason});
                pong ->
                    {Peer, Node}
            end;
        {error, Reason} ->
            ?SEV_EPRINT("failed starting node ~p (=> SKIP):"
                        "~n   ~p", [Name, Reason]),
            skip(Reason)
    catch
        Class:Reason:Stack ->
            ?SEV_EPRINT("Failed starting node: "
                        "~n   Class:  ~p"
                        "~n   Reason: ~p"
                        "~n   Stack:  ~p",
                        [Class, Reason, Stack]),
            skip({node_start, Class, Reason})
    end.

            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    FStr = ?F("[~s] " ++ F, [?FTS()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

