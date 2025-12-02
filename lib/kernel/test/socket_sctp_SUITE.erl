%%
%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%%         ESOCK_TEST_SCTP_MISC     include
%%         ESOCK_TEST_SCTP_BASIC    include
%%
%% Variable that controls "verbosity" of the test case(s):
%%
%%         ESOCK_TEST_QUIET: true (default) | false
%%

%% Run the entire test suite: 
%% ts:run(kernel, socket_sctp_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(kernel, socket_sctp_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(kernel, socket_sctp_SUITE, foo, [batch]).
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%% S = fun() -> ts:run(kernel, socket_sctp_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% S = fun() -> ct:run_test([{suite, socket_sctp_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ts:run(kernel, socket_sctp_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, socket_sctp_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ts:run(kernel, socket_SUITE, TC, [batch]) end.
%% T = fun(TC) -> ct:run_test([{suite, socket_sctp_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.



-module(socket_sctp_SUITE).

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
         %% *** Basic ***
         b_simple_open_and_close_ipv4/1,
         b_simple_open_and_close_ipv6/1,
         b_open_and_info_ipv4/1,
         b_open_and_info_ipv6/1,
         b_open_and_close_ipv4/1,
         b_open_and_close_ipv6/1,
         b_open_listen_and_close_ipv4/1,
         b_open_listen_and_close_ipv6/1,
         b_open_bind_and_close_ipv4/1,
         b_open_bind_and_close_ipv6/1,
         b_open_connect_and_close_ipv4/1,
         b_open_connect_and_close_ipv6/1,
	 b_stream/1,

         %% *** Misc ***
         m_min_data_exchange_ipv4/1,
         m_min_data_exchange_ipv6/1,
         m_stream_min_data_exchange_ipv4/1,
         m_stream_min_data_exchange_ipv6/1,
         m_multihoming_ipv4/1,
         m_multihoming_ipv6/1,
         m_unihoming_ipv6/1,
         m_multihoming_ipv4_and_ipv6/1,
         m_peeloff_ipv4/1,
         m_peeloff_ipv6/1,
         m_recv_close/1,
         m_buffers/1,

         %% *** Traffic ***
         t_exchange_st_ipv4/1,
         t_exchange_st_ipv6/1,
         t_exchange_mt_ipv4/1,
         t_exchange_mt_ipv6/1,

         %% *** Options ***
         o_default_sri_ipv4/1
        ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SLIB,       socket_test_lib).
-define(KLIB,       kernel_test_lib).
-define(LOGGER,     socket_test_logger).

-define(BASIC_REQ,  <<"hejsan">>).
-define(BASIC_REP,  <<"hoppsan">>).

-define(DATA,       <<"The quick brown fox jumps over a lazy dog 0123456789">>).
%% -define(FAIL(R),    exit(R)).

-define(SCTP_EVENTS(DataIO),
        #{data_io          => (DataIO),
          association      => true,
          address          => true,
          send_failure     => true,
          peer_error       => true,
          shutdown         => true,
          partial_delivery => true,
          adaptation_layer => false,
          authentication   => false,
          sender_dry       => false}).

-define(MK_SOCKOPT(Lvl, Opt), {(Lvl), (Opt)}).
-define(MK_OTP_SOCKOPT(Opt),  ?MK_SOCKOPT(otp,    (Opt))).
-define(MK_SOCK_SOCKOPT(Opt), ?MK_SOCKOPT(socket, (Opt))).
-define(MK_IP_SOCKOPT(Opt),   ?MK_SOCKOPT(ip,     (Opt))).
-define(MK_IPV6_SOCKOPT(Opt), ?MK_SOCKOPT(ipv6,   (Opt))).
-define(MK_SCTP_SOCKOPT(Opt), ?MK_SOCKOPT(sctp,   (Opt))).

-define(WHICH_SCTP_STATUS(Sock, AID),
        case socket:getopt(Sock,
                           ?MK_SCTP_SOCKOPT(status),
                           #{assoc_id => AID}) of
            {ok,    Status} -> Status;
            {error, closed} -> closed;
            {error, _}      -> undefined
        end).
-define(WHICH_SOCKNAME(Sock),
        fun() ->
                case socket:sockname(Sock) of
                    {ok, SockAddr}  -> SockAddr;
                    {error, closed} -> closed;
                    {error, _}      -> undefined
                end
        end()).
-define(ENABLE_SOCK_DEBUG(S),  socket:setopt(S, ?MK_OTP_SOCKOPT(debug), true)).
-define(DISABLE_SOCK_DEBUG(S), socket:setopt(S, ?MK_OTP_SOCKOPT(debug), false)).

-define(TRAFFIC_RUN_TIME, ?MINS(1)).
-define(TRAFFIC_DATA_M1, <<"The quick brown fox jumps over a lazy dog">>).
-define(TRAFFIC_DATA_M2, <<"The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog.">>).
-define(TRAFFIC_DATA_M3, <<"The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog."
                           "The quick brown fox jumps over a lazy dog.">>).
-define(TRAFFIC_DATA, [?TRAFFIC_DATA_M1,
                       ?TRAFFIC_DATA_M2,
                       ?TRAFFIC_DATA_M3]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    Groups = [
              {basic,                  "ESOCK_TEST_SCTP_BASIC",    include},
              {misc,                   "ESOCK_TEST_SCTP_MISC",     include},
              {traffic,                "ESOCK_TEST_SCTP_TRAFFIC",  include},
              {options,                "ESOCK_TEST_SCTP_OPTS",     include}
             ],
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
    [
     {basic,       [], basic_cases()},
     {misc,        [], misc_cases()},
     {homing,      [], homing_cases()},
     {traffic,     [], traffic_cases()},
     {options,     [], options_cases()}
    ].
     
basic_cases() ->
    [
     b_simple_open_and_close_ipv4,
     b_simple_open_and_close_ipv6,
     b_open_and_info_ipv4,
     b_open_and_info_ipv6,
     b_open_and_close_ipv4,
     b_open_and_close_ipv6,
     b_open_listen_and_close_ipv4,
     b_open_listen_and_close_ipv6,
     b_open_bind_and_close_ipv4,
     b_open_bind_and_close_ipv6,
     b_open_connect_and_close_ipv4,
     b_open_connect_and_close_ipv6,
     b_stream
    ].

misc_cases() ->
    [
     m_min_data_exchange_ipv4,
     m_min_data_exchange_ipv6,
     m_stream_min_data_exchange_ipv4,
     m_stream_min_data_exchange_ipv6,

     {group, homing},
     
     m_peeloff_ipv4,
     m_peeloff_ipv6,

     m_recv_close,

     m_buffers
    ].

homing_cases() ->
    [
     m_multihoming_ipv4,
     m_multihoming_ipv6,
     m_unihoming_ipv6,
     m_multihoming_ipv4_and_ipv6     
    ].

traffic_cases() ->
    [
     t_exchange_st_ipv4,
     t_exchange_st_ipv6,
     t_exchange_mt_ipv4,
     t_exchange_mt_ipv6
    ].

options_cases() ->
    [
     o_default_sri_ipv4
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config0) ->
    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    try socket:info() of
        #{} ->
            has_support_sctp(),
            case ?KLIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

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
            {skip, "ESock Not Supported"};
        error : undef ->
            {skip, "ESock Not Configured"}
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


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


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
%%                           API BASIC                                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and then close.
b_simple_open_and_close_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ipv4()
           end,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_simple_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and then close.
b_simple_open_and_close_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ipv6()
           end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_simple_open_and_close(InitState)
           end).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_simple_open_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = State) -> 
                           case socket:open(Domain, Type, Protocol) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         ?SEV_SLEEP(?SECS(1)),

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = _State) ->
                           socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and info of an IPv4 SCTP socket.
%% With some extra checks...
b_open_and_info_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ipv4()
           end,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_and_info(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and info of an IPv6 SCTP socket.
%% With some extra checks...
b_open_and_info_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() ->
                   has_support_ipv6()
           end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_and_info(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_open_and_info(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = State) -> 
                           case socket:open(Domain, Type, Protocol) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         #{desc => "get socket info",
           cmd  => fun(#{sock := Sock} = State) ->
                           Info = socket:info(Sock),
                           ?SEV_IPRINT("Got (some) Info: "
                                       "~n   ~p", [Info]),
                           {ok, State#{info => Info}}
                   end},

         #{desc => "validate socket info",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol,
                         info     := #{domain        := Domain,
                                       type          := Type,
                                       protocol      := Protocol,
                                       counters      := _,
                                       num_readers   := 0,
                                       num_writers   := 0,
                                       num_acceptors := 0}}) ->
                           ok;
                      (#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol,
                         info     := Info}) ->
                           ?SEV_EPRINT("Unexpected Info: "
                                       "~n   (expected) Domain:   ~p"
                                       "~n   (expected) Type:     ~p"
                                       "~n   (expected) Protocol: ~p"
                                       "~n   ~p",
                                       [Domain, Type, Protocol, Info]),
                           {error, unexpected_infio}
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = _State) ->
                           socket:close(Sock)
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv4 SCTP socket.
%% With some extra checks...
b_open_and_close_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) and close an IPv6 SCTP socket.
%% With some extra checks...
b_open_and_close_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_open_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           Res = socket:open(Domain, Type, Protocol), 
                           {ok, {S, Res}} 
                   end},
         #{desc => "validate open",
           cmd  => fun({S, {ok, Sock}}) -> 
                           NewS = S#{socket => Sock},
                           {ok, NewS};
                      ({_, {error, epfnosupport = Reason}}) ->
                           {skip, Reason};
                      ({_, {error, eprotonosupport = Reason}}) ->
                           {skip, Reason};
                      ({_, {error, esocktnosupport = Reason}}) ->
                           {skip, Reason};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         #{desc => "get domain (maybe)",
           cmd  => fun(#{socket := Sock} = S) ->
                           Res = socket:getopt(Sock, socket, domain),
                           {ok, {S, Res}}
                   end},
         #{desc => "validate domain (maybe)",
           cmd  => fun({#{domain := Domain} = S, {ok, Domain}}) -> 
                           ?SEV_IPRINT("expected domain: ~p", [Domain]),
                           {ok, S};
                      ({#{domain := ExpDomain}, {ok, Domain}}) ->
                           {error, {unexpected_domain, ExpDomain, Domain}};
                      %% Some platforms do not support this option
                      ({S, {error, {invalid, _}} = ERROR}) ->
                           case
                               socket:is_supported(options, socket, domain)
                           of
                               true ->
                                   ERROR;
                               false ->
                                   ?SEV_IPRINT("socket option 'domain' "
                                               "not supported"),
                                   {ok, S}
                           end;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         #{desc => "get type",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, socket, type), 
                           {ok, {State, Res}}
                   end},
         #{desc => "validate type",
           cmd  => fun({#{type := Type} = State, {ok, Type}}) ->
                           ?SEV_IPRINT("expected type: ~p", [Type]),
                           {ok, State};
                      ({#{type := ExpType}, {ok, Type}}) ->
                           {error, {unexpected_type, ExpType, Type}};
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         #{desc => "get protocol (maybe)",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, socket, protocol),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate protocol",
           cmd  => fun({#{protocol := Protocol} = State, {ok, Protocol}}) ->
                           ?SEV_IPRINT("expected protocol: ~p", [Protocol]),
                           {ok, State};
                      ({#{domain   := Domain,
			  protocol := ExpProtocol}, {ok, Protocol}}) ->
			   %% On OpenBSD (at least 6.6) something screwy happens
			   %% when domain = local.
			   %% It will report a completely different protocol
			   %% (icmp) but everything still works.
                           %% So we skip if this happens on OpenBSD...
			   case os:type() of
			       {unix, openbsd} when (Domain =:= local) ->
				   {skip, ?F("Unexpected protocol: ~p instead of ~p",
					     [Protocol, ExpProtocol])};
			       _ ->
				   {error, {unexpected_protocol,
					    ExpProtocol, Protocol}}
			   end;
                      %% Some platforms do not support this option
                      ({State, {error, {invalid, _}} = ERROR}) ->
			   case socket:is_supported(options, socket, protocol) of
			       true ->
                                   ERROR;
			       false ->
                                   ?SEV_IPRINT("socket option 'protocol' "
                                               "not supported"),
                                   {ok, State}
			   end;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         #{desc => "get controlling-process",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:getopt(Sock, otp, controlling_process),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate controlling-process",
           cmd  => fun({State, {ok, Pid}}) ->
                           case self() of
                               Pid ->
                                   {ok, State};
                               _ ->
                                   {error, {unexpected_owner, Pid}}
                           end;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         #{desc => "close socket",
           cmd  => fun(#{socket := Sock} = State) ->
                           Res = socket:close(Sock),
                           {ok, {State, Res}}
                   end},
         #{desc => "validate socket close",
           cmd  => fun({_, ok}) ->
                           ok;
                      ({_, {error, _} = ERROR}) ->
                           ERROR
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create), make listen and close an IPv4 SCTP socket.
%% With some extra checks...
b_open_listen_and_close_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv4() end,
           fun() ->
                   InitState = #{domain   => inet,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_listen_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create), make listen and close an IPv4 SCTP socket.
%% With some extra checks...
b_open_listen_and_close_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(?FUNCTION_NAME,
           fun() -> has_support_ipv6() end,
           fun() ->
                   InitState = #{domain   => inet6,
                                 type     => seqpacket,
                                 protocol => sctp},
                   ok = b_open_listen_and_close(InitState)
           end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_open_listen_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           case socket:open(Domain, Type, Protocol) of
                               {ok, Sock} ->
                                   {ok, S#{sock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "info socket (pre listen)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           ?SEV_IPRINT("socket info:"
                                       "~n   ~p", [socket:info(Sock)]),
                           ok
                   end},

         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case socket:listen(Sock) of
                               ok ->
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed make listen socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "info socket (post listen)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case socket:info(Sock) of
                               #{rstates := [listening]} = OkInfo ->
                                   ?SEV_IPRINT("OK socket info:"
                                               "~n   ~p", [OkInfo]),
                                   ok;
                               #{rstates := RStates} = ErrInfo ->
                                   ?SEV_EPRINT("Erronous (rstates) socket info:"
                                               "~n   ~p", [ErrInfo]),
                                   {error, RStates}
                           end
                   end},

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
                           {ok, State#{sock => undefined}}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create), bind and close an IPv4 SCTP socket.
%% With some extra checks...
b_open_bind_and_close_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = b_open_bind_and_close(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create), bind and close an IPv6 SCTP socket.
%% With some extra checks...
b_open_bind_and_close_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   Domain = inet6,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = b_open_bind_and_close(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_open_bind_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           case socket:open(Domain, Type, Protocol) of
                               {ok, Sock} ->
                                   {ok, S#{sock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "socket info (pre bind)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           ?SEV_IPRINT("socket info:"
                                       "~n   ~p", [socket:info(Sock)]),
                           ok
                   end},

         #{desc => "bind socket",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = _State) ->
                           SockAddr = #{family => Domain,
                                        addr   => Addr,
                                        port   => 0},
                           case socket:bind(Sock, SockAddr) of
                               ok ->
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed bind socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "socket info (post bind)",
           cmd  => fun(#{sock := Sock} = _State) ->
                           case socket:info(Sock) of
                               #{rstates := [bound]} = OkInfo ->
                                   {ok, SN} = socket:sockname(Sock),
                                   ?SEV_IPRINT("OK socket info:"
                                               "~n   Info:     ~p"
                                               "~n   SockName: ~p",
                                               [OkInfo, SN]),
                                   ok;
                               #{rstates := RStates} = ErrInfo ->
                                   ?SEV_EPRINT("Erronous (rstates) socket info:"
                                               "~n   ~p", [ErrInfo]),
                                   {error, RStates}
                           end
                   end},

         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           ok = socket:close(Sock),
                           {ok, State#{sock => undefined}}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq, InitState),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a listen socket and then create another
%% socket that connects to it: IPv4 SCTP socket(s).
b_open_connect_and_close_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = b_open_connect_and_close(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a listen socket and then create another
%% socket that connects to it: IPv6 SCTP socket(s).
b_open_connect_and_close_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   Domain = inet6,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = b_open_connect_and_close(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

b_open_connect_and_close(InitState) ->
    Seq = 
        [
         #{desc => "open listen socket",
           cmd  => fun(#{domain   := Domain,
                         protocol := Protocol} = S) -> 
                           case socket:open(Domain, seqpacket, Protocol) of
                               {ok, Sock} ->
                                   {ok, S#{lsock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "(listen) socket info (pre bind)",
           cmd  => fun(#{lsock := Sock} = _State) ->
                           ?SEV_IPRINT("socket info:"
                                       "~n   ~p", [socket:info(Sock)]),
                           ok
                   end},

         #{desc => "bind (listen) socket",
           cmd  => fun(#{lsock  := Sock,
                         domain := Domain,
                         addr   := Addr} = _State) ->
                           SockAddr = #{family => Domain,
                                        addr   => Addr,
                                        port   => 0},
                           case socket:bind(Sock, SockAddr) of
                               ok ->
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed bind socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "socket info (post bind)",
           cmd  => fun(#{lsock := Sock} = State) ->
                           case socket:info(Sock) of
                               #{rstates := [bound]} = OkInfo ->
                                   {ok, #{port := Port}} = SN =
                                       socket:sockname(Sock),
                                   ?SEV_IPRINT("OK socket info:"
                                               "~n   Info:     ~p"
                                               "~n   SockName: ~p",
                                               [OkInfo, SN]),
                                   {ok, State#{lport => Port}};
                               #{rstates := RStates} = ErrInfo ->
                                   ?SEV_EPRINT("Erronous (rstates) socket info:"
                                               "~n   ~p", [ErrInfo]),
                                   {error, RStates}
                           end
                   end},

         #{desc => "subscribe to (sctp) events",
           cmd  => fun(#{lsock  := Sock,
                         events := Evs} = _State) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},

         #{desc => "make listen socket",
           cmd  => fun(#{lsock := Sock} = _State) ->
                           socket:listen(Sock, true)
                   end},

         #{desc => "socket info (post listen)",
           cmd  => fun(#{lsock := Sock} = _State) ->
                           case socket:info(Sock) of
                               #{rstates := RStates} = OkInfo ->
                                   ?SEV_IPRINT("OK socket info:"
                                               "~n   Info: ~p", [OkInfo]),
                                   case lists:member(listening, RStates) of
                                       true ->
                                           ok;
                                       false ->
                                           {error, RStates}
                                   end
                           end
                   end},

         %% *** CONNECTING ***

         #{desc => "open (connecting) socket",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Protocol} = S) -> 
                           case socket:open(Domain, Type, Protocol) of
                               {ok, Sock} ->
                                   {ok, S#{csock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "(connecting) socket info (pre bind)",
           cmd  => fun(#{csock := Sock} = _State) ->
                           ?SEV_IPRINT("socket info:"
                                       "~n   ~p", [socket:info(Sock)]),
                           ok
                   end},

         #{desc => "bind (connecting) socket",
           cmd  => fun(#{csock  := Sock,
                         domain := Domain,
                         addr   := Addr} = _State) ->
                           SockAddr = #{family => Domain,
                                        addr   => Addr,
                                        port   => 0},
                           case socket:bind(Sock, SockAddr) of
                               ok ->
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed bind socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "(connecting) socket info (post bind)",
           cmd  => fun(#{csock := Sock} = _State) ->
                           case socket:info(Sock) of
                               #{rstates := [bound]} = OkInfo ->
                                   ?SEV_IPRINT("OK socket info:"
                                               "~n   Info: ~p", [OkInfo]),
                                   ok;
                               #{rstates := RStates} = ErrInfo ->
                                   ?SEV_EPRINT("Erronous (rstates) socket info:"
                                               "~n   ~p", [ErrInfo]),
                                   {error, RStates}
                           end
                   end},

         #{desc => "subscribe to (sctp) events",
           cmd  => fun(#{csock  := Sock,
                         events := Evs} = _State) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},


         #{desc => "connect to server",
           cmd  => fun(#{csock  := Sock,
                         domain := Domain,
                         addr   := Addr,
                         lport  := Port} = _State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
                           case socket:connect(Sock, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("connected"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed connect:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "await the connect confirmation: assoc-change:comm-up",
           cmd  => fun(#{csock  := Sock} = State) ->
                           ?SEV_IPRINT("try read assoc-change:comm-up notification message"),
                           case socket:recvmsg(Sock) of
                               {ok, #{flags := Flags} = Msg} ->
                                   ?SEV_IPRINT("try verify notification"),
                                   case lists:member(notification, Flags) of
                                       true ->
                                           ?SEV_IPRINT("notification"),
                                           case Msg of
                                               #{notification :=
                                                     #{type     := assoc_change,
                                                       state    := comm_up,
                                                       assoc_id := CAID,
                                                       '$esock_name' := sctp_notification}} ->
                                                   ?SEV_IPRINT("expected connect confirmation:"
                                                               "~n   AssocID: ~p", [CAID]),
                                                   {ok, State#{caid => CAID}};
                                               #{notification := Notif} ->
                                                   ?SEV_IPRINT("unexpected (connect) notification:"
                                                               "~n   Notif: ~p", [Notif]),
                                                   error;
                                               _ ->
                                                   ?SEV_EPRINT("unexpected connect confirmation :"
                                                               "~n   ~p", [Msg]),
                                                   error
                                           end;
                                       false ->
                                           ?SEV_EPRINT("no notification flag detected:"
                                                       "~n   ~p", [Msg]),
                                           error
                                   end;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed read connect :"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},

         #{desc => "(connected) socket info (post connect)",
           cmd  => fun(#{csock := Sock} = _State) ->
                           case socket:info(Sock) of
                               #{rstates := RStates,
                                  wstates := WStates} = Info ->
				    case lists:member(bound, RStates) andalso
				    	 lists:member(connected, WStates) of
					 true ->
                                   	      ?SEV_IPRINT("OK socket info:"
                                              		       "~n   Info: ~p", [Info]),
                                   		ok;
					 false ->
                                              ?SEV_EPRINT("Erronous states:"
					      			    "~n    Read States: ~p"
                                                       		    "~n    Write States: ~p",
								    [RStates, WStates]),
                                   		{error, {RStates, WStates}}
				end
                           end
                   end},

         #{desc => "(connecting) socket misc (ioctl) info",
           cmd  => fun(#{csock := Sock} = _State) ->
                           ?SEV_IPRINT("misc (ioctl) socket info: "
                                       "~n   nread:  ~p"
                                       "~n   atmark: ~p",
                                       [case socket:ioctl(Sock, nread) of
                                            {ok, NRead} -> NRead;
                                            {error, _} -> undefined
                                        end,
                                        case socket:ioctl(Sock, atmark) of
                                            {ok, Bool} -> Bool;
                                            {error, _} -> undefined
                                        end]),
                           ok
                   end},

         #{desc => "accept connection (recvmsg get assoc-change:comm-up)",
           cmd  => fun(#{lsock := Sock} = State) ->
                           ?SEV_IPRINT("try recv assoc-change:comm-up message"),
                           case socket:recvmsg(Sock) of
                               {ok, #{flags := Flags} = Msg} ->
                                   ?SEV_IPRINT("try verify notification"),
                                   case lists:member(notification, Flags) of
                                       true ->
                                           ?SEV_IPRINT("notification"),
                                           case Msg of
                                               #{notification :=
                                                     #{type     := assoc_change,
                                                       state    := comm_up,
                                                       assoc_id := AAID,
                                                       '$esock_name' := sctp_notification}} ->
                                                   ?SEV_IPRINT("expected accept notification:"
                                                               "~n   AssocID: ~p", [AAID]),
                                                   {ok, State#{aaid => AAID}};
                                               #{notification := Notif} ->
                                                   ?SEV_IPRINT("unexpected (accept) notification:"
                                                               "~n   Notif: ~p", [Notif]),
                                                   error;
                                               _ ->
                                                   ?SEV_EPRINT("unexpected accept msg :"
                                                               "~n   ~p", [Msg]),
                                                   error
                                           end;
                                       false ->
                                           ?SEV_EPRINT("no notification flag detected:"
                                                       "~n   ~p", [Msg]),
                                           error
                                   end
                           end
                   end},


         #{desc => "client assoc status",
           cmd  => fun(#{csock := Sock,
                         caid  := AID} = _State) ->
                           SockOpt      = ?MK_SCTP_SOCKOPT(status),
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("assoc ~w status:"
                                               "~n   ~p", [AID, Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "client socket send buffer size",
           cmd  => fun(#{csock := Sock,
                         caid  := _AID} = _State) ->
                           SockOpt = ?MK_SOCK_SOCKOPT(sndbuf),
                           case socket:getopt(Sock, SockOpt) of
                               {ok, Sz} ->
                                   ?SEV_IPRINT("send buffer size:"
                                               "~n   ~p", [Sz]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "client socket receive buffer size",
           cmd  => fun(#{csock := Sock,
                         caid  := _AID} = _State) ->
                           SockOpt = ?MK_SOCK_SOCKOPT(rcvbuf),
                           case socket:getopt(Sock, SockOpt) of
                               {ok, Sz} ->
                                   ?SEV_IPRINT("receive buffer size:"
                                               "~n   ~p", [Sz]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},


         #{desc => "initiate gracefully close client assoc socket (eof)",
           cmd  => fun(#{csock := Sock,
                         caid  := AID} = State) ->
                           ?SEV_IPRINT("try gracefully close assoc (eof)"),
                           EofSRI = #{assoc_id     => AID,
                                      stream       => 0,
                                      ssn          => 0,
                                      flags        => [eof],
                                      ppid         => 0,
                                      context      => 0,
                                      time_to_live => 0,
                                      tsn          => 0,
                                      cum_tsn      => 0},
                           EofMsg = #{iov  => [],
                                      ctrl => [#{level => sctp,
                                                 type  => sndrcv,
                                                 value => EofSRI}]},
			   %% ?ENABLE_SOCK_DEBUG(Sock),
                           case socket:sendmsg(Sock, EofMsg) of
                               ok ->
			           %% ?DISABLE_SOCK_DEBUG(Sock),
                                   ?SEV_IPRINT("eof message sent"),
                                   {ok, State#{caid => undefined}};
                               Error ->
			           %% ?DISABLE_SOCK_DEBUG(Sock),
                                   Error
                           end
                   end},

         #{desc => "await server side assoc-change:shutdown or peer-addr-change",
           cmd  => fun(#{lsock := Sock,
                         aaid  := AID} = State) ->
                           ?SEV_IPRINT("await assoc shutdown"),
                           case await_shutdown_or_maybe_peer_addr_change(Sock, AID) of
                               ok ->
                                   ?SEV_IPRINT("shutdown received"),
                                   {ok, maps:remove(aaid, State)};
                               {error, _} ->
                                   ?SEV_EPRINT("no shutdown received"),
                                   error
                           end
                   end},

         #{desc => "maybe await server side assoc-change:shutdown ",
           cmd  => fun(#{lsock := Sock,
                         aaid  := AID} = State) ->
                           ?SEV_IPRINT("try recv assoc shutdown message"),
                           case socket:recvmsg(Sock) of
                               {ok, #{flags := Flags} = Msg} ->
                                   ?SEV_IPRINT("try verify notification"),
                                   case lists:member(notification, Flags) of
                                       true ->
                                           ?SEV_IPRINT("notification"),
                                           case Msg of
                                               #{notification :=
                                                     #{type     := shutdown_event,
                                                       assoc_id := AID,
                                                       '$esock_name' := sctp_notification}} ->
                                                   ?SEV_IPRINT("expected assoc shutdown notification"),
                                                   {ok, maps:remove(aaid, State)};
                                               _ ->
                                                   ?SEV_EPRINT("unexpected assoc shutdown msg:"
                                                               "~n   ~p", [Msg]),
                                                   error
                                           end;
                                       false ->
                                           ?SEV_EPRINT("no notification flag detected:"
                                                       "~n   ~p", [Msg]),
                                           error
                                   end
                           end;
		   (_) ->
		       ?SEV_IPRINT("NOP"),
		       ok
		    end},


         #{desc => "close (connecting) socket",
           cmd  => fun(#{csock := Sock} = State) ->
                           ok = socket:close(Sock),
                           {ok, State#{csock => undefined}}
                   end},

         #{desc => "close (listen) socket",
           cmd  => fun(#{lsock := Sock} = State) ->
                           ok = socket:close(Sock),
                           {ok, State#{lsock => undefined}}
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],
    Evaluator = ?SEV_START("tester", Seq,
                           InitState#{events => ?SCTP_EVENTS(true)}),
    ok = ?SEV_AWAIT_FINISH([Evaluator]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Hello world stream socket.
b_stream(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => stream,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = do_b_stream(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

do_b_stream(#{domain   := Domain,
	      type     := Type,
	      protocol := sctp = Proto,
	      addr     := Addr}) ->
    {ok, S} = socket:open(Domain, Type, Proto),
    ok      = socket:bind(S, #{family => Domain,
			       addr   => Addr,
			       port   => 0}),
    ok      = socket:listen(S, true),
    ok      = do_from_other_process(fun() -> socket:listen(S, 10) end),
    ok      = socket:close(S),
    ok.


			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a listen socket and then create another
%% socket that connects to it and then exchange one message.
%% IPv4 SCTP socket(s).
m_min_data_exchange_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_min_data_exchange(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a listen socket and then create another
%% socket that connects to it and then exchange one message.
%% IPv6 SCTP socket(s).
m_min_data_exchange_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   Domain = inet6,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_min_data_exchange(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_min_data_exchange(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq =
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
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock, true)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, port := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (accept connection)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept connection: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case accept_connection(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce assoc accepted: "
                                       "~n   Assoc: ~p", [Assoc]),
                           ?SEV_ANNOUNCE_READY(Tester, accept),
                           ok
                   end},

         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "await first data (recvmsg)",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = State) ->
                           case recv_first_data(Sock) of
                               {ok, {_SA, ?DATA, AID, Stream}} ->
                                   ?SEV_IPRINT("expected data received on"
                                               "~n   Stream: ~p", [Stream]),
                                   {ok, State#{stream => Stream}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce recv"),
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         #{desc => "await continue (send)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send data back",
           cmd  => fun(#{sock   := Sock,
                         assoc  := #{assoc_id := AID},
                         stream := Stream} = _State) ->
                           ?SEV_IPRINT("build sparse SRI "
                                         "(only assoc-id and stream)"),
                           SRI = #{assoc_id => AID, stream => Stream},
                           Msg = #{iov => [?DATA],
                                   ctrl => [#{level => sctp,
                                              type  => sndrcv,
                                              value => SRI}]},
                           case socket:sendmsg(Sock, Msg) of
                               ok ->
                                   ?SEV_IPRINT("msg sent"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed send msg:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce message sent"),
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, await_shutdown)
                   end},
         #{desc => "await shutdown",
           cmd  => fun(#{sock   := Sock,
                         assoc  := #{assoc_id := AID}} = State) ->
                           case await_shutdown(Sock, AID) of
                               ok ->
                                   {ok, maps:remove(assoc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce shutdown received"),
                           ?SEV_ANNOUNCE_READY(Tester, await_shutdown),
                           ok
                   end},
         
         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
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
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock, State),
                                   State2 = maps:remove(port, State1),
                                   {ok, State2};
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
                           {Tester, ServerPort} = ?SEV_AWAIT_START(),
                           ?SEV_IPRINT("starting with"
                                       "~n   ServerPort: ~w", [ServerPort]),
                           {ok, State#{tester      => Tester,
                                       server_port => ServerPort}}
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
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "accept connection: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock        := Sock,
                         domain      := Domain,
                         addr        := Addr,
                         server_port := Port} = _State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
                           case socket:connect(Sock, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("connected"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed connect:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "await connect confirmation: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case connect_confirmation(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce connected: "
                                       "~n   Assoc: ~p", [Assoc]),
                           ?SEV_ANNOUNCE_READY(Tester, connect),
                           ok
                   end},

         #{desc => "await continue (send message)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send (initial) message",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = _State) ->
                           ?SEV_IPRINT("build sparse SRI "
                                         "(only assoc-id and stream)"),
                           SRI = #{assoc_id => AID, stream => 0},
                           Msg = #{iov => [?DATA],
                                   ctrl => [#{level => sctp,
                                              type  => sndrcv,
                                              value => SRI}]},
                           case socket:sendmsg(Sock, Msg) of
                               ok ->
                                   ?SEV_IPRINT("msg sent"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed send msg:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce message sent"),
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "await data (recvmsg)",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           case recv_data(Sock) of
                               {ok, {_SA, ?DATA, AID, 0}} ->
                                   ?SEV_IPRINT("expected data received"),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce recv"),
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         #{desc => "await continue (eof)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, eof)
                   end},
         #{desc => "gracefully close assoc (eof)",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           SRI = #{assoc_id => AID,
                                   stream   => 0,
                                   flags    => [eof]},
                           Msg = #{iov  => [<<>>],
                                   ctrl => [#{level => sctp,
                                              type  => sndrcv,
                                              value => SRI}]},
                           case socket:sendmsg(Sock, Msg) of
                               ok ->
                                   ?SEV_IPRINT("eof sent"),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (eof)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce eof done"),
                           ?SEV_ANNOUNCE_READY(Tester, eof),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},



         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   ?SEV_IPRINT("received termination "
                                               "from tester"),
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close listen socket",
           cmd  => fun(#{sock := Sock} = State0) ->
                           ?SEV_IPRINT("try close socket"),
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock,        State0),
                                   State2 = maps:remove(port,        State1),
                                   State3 = maps:remove(server_port, State2),
                                   ?SEV_IPRINT("socket closed and "
                                               "cleanup done"),
                                   {ok, State3};
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
         #{desc => "monitor 'server'",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (server) ready",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, Port} ->
                                   {ok, State#{server_port => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "monitor 'client'",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},


         %% *** The actual test ***
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, connect)
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, accept)
                   end},

         #{desc => "order server to continue (with recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with send)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send)
                   end},
         #{desc => "await server ready (recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv)
                   end},

         #{desc => "order client to continue (with recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server to continue (with send)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send),
                           ok
                   end},
         #{desc => "await server ready (send)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send)
                   end},
         #{desc => "await client ready (recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv)
                   end},

         #{desc => "order server to continue (with await-shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, await_shutdown),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with eof)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, eof),
                           ok
                   end},
         #{desc => "await client ready (eof)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, eof)
                   end},
         #{desc => "await server ready (await_shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, await_shutdown)
                   end},

         #{desc => "order server to continue (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, stats),
                           ok
                   end},
         #{desc => "order client to continue (stats)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, stats),
                           ok
                   end},


         %% *** Termination ***
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_IPRINT("announce 'terminate' to client (~p)",
                                       [Client]),
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Client) of
                               ok ->
                                   ?SEV_IPRINT("client (~p) terminated",
                                               [Client]),
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_IPRINT("announce 'terminate' to server (~p)",
                                       [Server]),
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Server) of
                               ok ->
                                   ?SEV_IPRINT("server (~p) terminated",
                                               [Server]),
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    SctpEvs = ?SCTP_EVENTS(true),

    i("start server evaluator"),
    Server = ?SEV_START("server", ServerSeq, InitState#{events => SctpEvs}),

    i("start client evaluator"),
    Client = ?SEV_START("client", ClientSeq, InitState#{events => SctpEvs}),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
			client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


accept_connection(Sock) ->
    ?SEV_IPRINT("await connection message (recvmsg assoc-change:comm-up)"),
    assoc_confirmation(Sock).


connect_confirmation(Sock) ->
    ?SEV_IPRINT("await connect confirmation (recvmsg assoc-change:comm-up)"),
    assoc_confirmation(Sock).


assoc_confirmation(Sock) ->
    case socket:recvmsg(Sock) of
        {ok, #{flags := Flags} = Msg} ->
            ?SEV_IPRINT("message received - try verify is notification"),
            case lists:member(notification, Flags) of
                true ->
                    ?SEV_IPRINT("is notification"),
                    case Msg of
                        #{addr         := Addr,
                          ctrl         := [],
                          notification :=
                              #{type             := assoc_change,
                                state            := comm_up,
                                assoc_id         := AID,
                                inbound_streams  := IS,
                                outbound_streams := OS,
                                '$esock_name'    := sctp_notification}} ->
                            ?SEV_IPRINT("expected (assoc-change:comm-up) "
                                        "notification:"
                                        "~n   Addr:    ~p"
                                        "~n   AssocID: ~p", [Addr, AID]),
                            {ok, {Addr, #{assoc_id         => AID,
                                          inbound_streams  => IS,
                                          outbound_streams => OS}}};
                        #{addr         := Addr,
                          notification := Notif} ->
                            ?SEV_IPRINT("unexpected notification:"
                                        "~n   Addr:  ~p"
                                        "~n   Notif: ~p", [Addr, Notif]),
                            {error, {unexpected_notification, Addr, Notif}};
                        _ ->
                            ?SEV_EPRINT("unexpected message :"
                                        "~n   ~p", [Msg]),
                            {error, {unexpected_msg, Msg}}
                    end;
                false ->
                    ?SEV_EPRINT("not notification:"
                                "~n   ~p", [Msg]),
                    {error, not_notification}
            end;
        {error, Reason} = ERROR ->
            ?SEV_EPRINT("recvmsg failed:"
                        "~n   ~p", [Reason]),
            throw(ERROR)
    end.


%% The reason for this (function) is that on some platforms
%% its possible that another messages
%%     peer-addr-change:addr-confirmation|addr-available
%% arrives before the data message.
recv_first_data(Sock) ->
    recv_first_data(Sock, infinity, false).
recv_first_data(Sock, Timeout) ->
    recv_first_data(Sock, Timeout, false).
recv_first_data(Sock, Timeout, false) ->
    case socket:recvmsg(Sock, Timeout) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AID,
                                      stream   := Stream,
                                      tsn      := TSN,
                                      '$esock_name' := sctp_sndrcvinfo}}]}} ->
            ?SEV_IPRINT("received expected message: "
                        "~n   SA:     ~p"
                        "~n   AID:    ~p"
                        "~n   Stream: ~p"
                        "~n   TSN:    ~p", [SA, AID, Stream, TSN]),
            {ok, {SA, Data, AID, Stream}};

        {ok, #{flags := Flags} = Msg} ->
            case lists:member(notification, Flags) of
                true ->
                    case Msg of
                        #{addr := SA,
                          ctrl  := [],
                          notification :=
                              #{type     := peer_addr_change,
                                state    := NState,
                                assoc_id := AID,
                                '$esock_name' := sctp_notification}}
                          when (NState =:= addr_confirmed) orelse
                               (NState =:= addr_available) ->
                            ?SEV_IPRINT("received "
                                        "peer-addr-change:"
                                        "addr-confirmed|addr-available "
                                        "message: "
                                        "~n   SA:     ~p"
                                        "~n   NState: ~p"
                                        "~n   AID:    ~p",
                                        [SA, NState, AID]),
                            recv_data(Sock, Timeout, AID);
                        #{addr         := Addr,
                          notification := Notif} ->
                            ?SEV_EPRINT("unexpected notification:"
                                        "~n   Addr:  ~p"
                                        "~n   Notif: ~p", [Addr, Notif]),
                            Reason = {unexpected_notification, Addr, Notif},
                            Error  = {error, Reason},
                            throw(Error)
                    end;
                false ->
                    ?SEV_EPRINT("received unexpected message: "
                                "~n   Msg: ~p", [Msg]),
                    {error, unexpected_msg}
            end;
        {error, Reason} = ERROR ->
            ?SEV_EPRINT("unexpected recv failure: "
                        "~n   Reason: ~p", [Reason]),
            ERROR
    end.


recv_data(Sock) ->
    recv_data(Sock, infinity, any).

recv_data(Sock, Timeout, AID) ->
    ?SEV_IPRINT("try recvmsg for AID: ~p", [AID]),
    case socket:recvmsg(Sock, Timeout) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := RecvAID,
                                      stream   := Stream,
                                      tsn      := TSN,
                                      '$esock_name' := sctp_sndrcvinfo}}]}}
				 when (RecvAID =:= AID) orelse (AID =:= any) ->
            ?SEV_IPRINT("received expected message: "
                        "~n   SA:     ~p"
                        "~n   AID:    ~p"
                        "~n   Stream: ~p"
                        "~n   TSN:    ~p", [SA, RecvAID, Stream, TSN]),
            {ok, {SA, Data, RecvAID, Stream}};
        {ok, Msg} ->
            ?SEV_EPRINT("received unexpected message: "
                        "~n   Msg: ~p", [Msg]),
            {error, unexpected_msg};
        {error, Reason} = ERROR ->
            ?SEV_EPRINT("unexpected recv failure: "
                        "~n   Reason: ~p", [Reason]),
            ERROR
    end.


await_shutdown(Sock, AID) ->
    try
        begin
            await_shutdown_or_maybe_peer_addr_change(Sock, AID),
            await_shutdown_complete(Sock, AID)
        end
    catch
        throw:Error ->
            Error
    end.

await_shutdown_or_maybe_peer_addr_change(Sock, AID) ->
     await_shutdown_or_maybe_peer_addr_change(Sock, AID, false).
await_shutdown_or_maybe_peer_addr_change(Sock, AID, PAC) ->
    ?SEV_IPRINT("try recv shutdown event"),
    case socket:recvmsg(Sock, infinity) of
       {ok, Msg} ->
             case shutdown_or_maybe_peer_addr_change(Msg, AID, PAC) of
	     	  {ok, peer_addr_change} ->
		       await_shutdown_or_maybe_peer_addr_change(Sock, AID, true);
		  {ok, _} ->
		       ok
	     end;
	{error, _} = ERROR ->
	     ERROR
    end.

shutdown_or_maybe_peer_addr_change(Msg, AID, PAC) ->
    case Msg of
        #{flags := Flags} ->
            case lists:member(notification, Flags) of
                true ->
                    case Msg of
                        #{ctrl         := [],
                          notification :=
                              #{type          := shutdown_event,
                                assoc_id      := AID,
                                '$esock_name' := sctp_notification} = Notif} ->
                            ?SEV_IPRINT("received expected shutdown event:"
                                        "~n   ~p", [Notif]),
                            {ok, shutdown_event};
                        #{ctrl         := [],
                          notification := #{type     := peer_addr_change,
                                            state    := NState,
                                            assoc_id := AID}}
                          when (PAC =:= false) andalso
                               ((NState =:= addr_confirmed) orelse
                                (NState =:= addr_available)) ->
                            ?SEV_IPRINT("received expected peer-addr-change event"),
                            {ok, peer_addr_change};
                        #{addr         := Addr,
                          notification := Notif} ->
                            ?SEV_EPRINT("unexpected notification:"
                                        "~n   Addr:  ~p"
                                        "~n   Notif: ~p", [Addr, Notif]),
                            Reason = {unexpected_notification, Addr, Notif},
                            Error  = {error, Reason},
                            throw(Error);
                        _ ->
                            ?SEV_EPRINT("unexpected message :"
                                        "~n   ~p", [Msg]),
                            throw({error, {unexpected_msg, Msg}})
                    end;
                false ->
                    ?SEV_EPRINT("not notification:"
                                "~n   ~p", [Msg]),
                    throw({error, not_notification})
            end;
        {error, Reason} = ERROR ->
            ?SEV_EPRINT("receive error:"
                        "~n   ~p", [Reason]),
            throw(ERROR)
    end.

await_shutdown_complete(Sock, AID) ->
    ?SEV_IPRINT("try recv shutdown-complete"),
    case socket:recvmsg(Sock, infinity) of
        {ok, #{flags := Flags} = Msg} ->
            case lists:member(notification, Flags) of
                true ->
                    case Msg of
                        #{ctrl         := [],
                          notification :=
                              #{type          := assoc_change,
                                state         := shutdown_comp,
                                assoc_id      := AID,
                                '$esock_name' := sctp_notification}} ->
                            ?SEV_IPRINT("received expected shutdown-complete"),
                            ok;
                        #{addr         := Addr,
                          notification := Notif} ->
                            ?SEV_EPRINT("unexpected notification:"
                                        "~n   Addr:  ~p"
                                        "~n   Notif: ~p", [Addr, Notif]),
                            Reason = {unexpected_notification, Addr, Notif},
                            Error  = {error, Reason},
                            throw(Error);
                        _ ->
                            ?SEV_EPRINT("unexpected message :"
                                        "~n   ~p", [Msg]),
                            throw({error, {unexpected_msg, Msg}})
                    end;
                false ->
                    ?SEV_EPRINT("not notification:"
                                "~n   ~p", [Msg]),
                    throw({error, not_notification})

            end;

        {error, closed} = ERROR ->
            %% This seems to happen on old linux systems,
            %% so check if this is such...if not fail
            case os:type() of
                {unix, linux} ->
                    Version = os:version(),
                    if
                        (Version > {4,4,74}) ->
                            ?SEV_EPRINT("socket already closed"),
                            throw(ERROR);
                        true -> 
                            ?SEV_IPRINT("socket already closed"),
                           ok
                    end;
                _ ->
                    ?SEV_EPRINT("socket already closed"),
                    throw(ERROR)
            end;

        {error, Reason} = ERROR ->
            ?SEV_EPRINT("receive error:"
                        "~n   ~p", [Reason]),
            throw(ERROR)
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a 'seqpacket' listen socket and then create
%% another 'stream' socket that connects to it and then exchange one
%% message. IPv4 SCTP socket(s).
m_stream_min_data_exchange_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{domain      => Domain,
                             addr        => Addr,
                             server_type => seqpacket,
                             client_type => stream,
                             protocol    => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_stream_min_data_exchange(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basically open (create) a 'seqpacket' listen socket and then create
%% another 'stream' socket that connects to it and then exchange one
%% message. IPv6 SCTP socket(s).
m_stream_min_data_exchange_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   Domain = inet6,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{domain      => Domain,
                             addr        => Addr,
                             server_type => seqpacket,
                             client_type => stream,
                             protocol    => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_stream_min_data_exchange(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_stream_min_data_exchange(InitState) ->
    process_flag(trap_exit, true),

    ServerSeq =
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
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain   := Domain,
                         type     := Type,
                         protocol := Proto} = State) ->
                           case socket:open(Domain, Type, Proto) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock, true)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, port := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (accept connection)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept connection: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case accept_connection(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce assoc accepted: "
                                       "~n   Assoc: ~p", [Assoc]),
			   #{inbound_streams  := IS,
			     outbound_streams := OS} = Assoc,
                           ?SEV_ANNOUNCE_READY(Tester, accept, {IS, OS}),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           ?SEV_IPRINT("try get status"),
                           SockOpt      = ?MK_SCTP_SOCKOPT(status),
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},


         #{desc => "await continue (send message)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send (initial) message",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = _State) ->
                           ?SEV_IPRINT("build sparse SRI "
                                       "(only assoc-id and stream)"),
                           SRI = #{assoc_id => AID, stream => 0},
                           Msg = #{iov => [?DATA],
                                   ctrl => [#{level => sctp,
                                              type  => sndrcv,
                                              value => SRI}]},
                           case socket:sendmsg(Sock, Msg) of
                               ok ->
                                   ?SEV_IPRINT("msg sent"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed send msg:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce message sent"),
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},

         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "await data (recvmsg)",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           case recv_first_data(Sock) of
                               {ok, {_SA, ?DATA, AID, 0}} ->
                                   ?SEV_IPRINT("expected data received"),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce recv"),
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},
         

         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           ?SEV_IPRINT("try get status"),
                           SockOpt      = ?MK_SCTP_SOCKOPT(status),
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
                           ok
                   end},


         #{desc => "await continue (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, await_shutdown)
                   end},
         #{desc => "await shutdown",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = State) ->
                           case await_shutdown(Sock, AID) of
                               ok ->
                                   {ok, maps:remove(assoc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce shutdown received"),
                           ?SEV_ANNOUNCE_READY(Tester, await_shutdown),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce stats"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
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
         #{desc => "close (server) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock, State),
                                   State2 = maps:remove(port, State1),
                                   {ok, State2};
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
                           {Tester, ServerPort} = ?SEV_AWAIT_START(),
                           ?SEV_IPRINT("starting with"
                                       "~n   ServerPort: ~w", [ServerPort]),
                           {ok, State#{tester      => Tester,
                                       server_port => ServerPort}}
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
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "accept connection: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock        := Sock,
                         domain      := Domain,
                         addr        := Addr,
                         server_port := Port} = _State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
                           case socket:connect(Sock, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("connected"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed connect:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "await connect confirmation: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case connect_confirmation(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "try verify assoc (peer-addr-info)",
           cmd  => fun(#{sock        := Sock,
                         domain      := Domain,
			 protocol    := Proto,
                         addr        := Addr,
                         server_port := Port,
			 assoc       := #{assoc_id := AID}} = State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
			   ?SEV_IPRINT("try get peer-addr-info"),
                           %% Create sparse peer-addr-info
			   OptValue = #{assoc_id => AID,
                                        addr     => ServerSA},
                           SockOpt  = {Proto, get_peer_addr_info},
			   case socket:getopt(Sock, SockOpt, OptValue) of
			       {ok, #{assoc_id := PAID,
				      state    := active}} ->
				   ?SEV_IPRINT("try verify assoc-id"),
				   match_unless_solaris(AID, PAID);
			       {ok, #{assoc_id := PAID,
				      state    := State}} ->
				   ?SEV_EPRINT("invalid assoc state:"
					       "~n   AID:   ~p"
					       "~n   State: ~p",
					       [PAID, State]),
				   Reason = {invalid_assoc_state, State},
				   {error, Reason};
			       {error, Reason0} ->
				   ?SEV_EPRINT("failed get "
					       "peer-addr-info:"
					       "~n   ~p", [Reason0]),
				   Reason =
				       {failed_get_peer_addr_info, Reason0},
				   {error, Reason}
			   end
		   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce connected: "
                                       "~n   Assoc: ~p", [Assoc]),
			   #{inbound_streams  := IS,
			     outbound_streams := OS} = Assoc,
                           ?SEV_ANNOUNCE_READY(Tester, connect, {IS, OS}),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := _AID} = _Assoc} = _State) ->
                           case socket:getopt(Sock, sctp, status) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},


         #{desc => "await continue (recv initial message)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, recv)
                   end},
         #{desc => "await first data (recvmsg)",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = State) ->
                           case recv_first_data(Sock) of
                               {ok, {_SA, ?DATA, AID, Stream}} ->
                                   ?SEV_IPRINT("expected data received on"
                                               "~n   Stream: ~p", [Stream]),
                                   {ok, State#{stream => Stream}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce recv"),
                           ?SEV_ANNOUNCE_READY(Tester, recv),
                           ok
                   end},

         #{desc => "await continue (send message)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, send)
                   end},
         #{desc => "send data back from other process",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = _State) ->
			   Sender =
			       fun() ->
				       ?SEV_IPRINT("build sparse SRI "
						   "(only assoc-id and "
						   "stream)"),
				       SRI = #{assoc_id => AID,
					       stream => 0},
				       Msg = #{iov => [?DATA],
					       ctrl => [#{level => sctp,
							  type  => sndrcv,
							  value => SRI}]},
				       case socket:sendmsg(Sock, Msg) of
					   ok ->
					       ?SEV_IPRINT("msg sent"),
					       ok;
					   {error, Reason} = ERROR ->
					       ?SEV_EPRINT("Failed send msg:"
							   "~n   ~p", [Reason]),
					       ERROR
				       end
			       end,
			   do_from_other_process(Sender)
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce message sent"),
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := _AID} = _Assoc} = _State) ->
                           case socket:getopt(Sock, sctp, status) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce stats"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   ?SEV_IPRINT("received termination "
                                               "from tester"),
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State0) ->
                           ?SEV_IPRINT("try close socket"),
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock,        State0),
                                   State2 = maps:remove(port,        State1),
                                   State3 = maps:remove(server_port, State2),
                                   ?SEV_IPRINT("socket closed and "
                                               "cleanup done"),
                                   {ok, State3};
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
         #{desc => "monitor 'server'",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (server) ready",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, Port} ->
                                   {ok, State#{server_port => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "monitor 'client'",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},


         %% *** The actual test ***
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = State) ->
                           {ok, {IS, OS}} =
			       ?SEV_AWAIT_READY(Client, client, connect),
			   ?SEV_IPRINT("client streams:"
				       "~n   Inbound:  ~w"
				       "~n   Outbound: ~w", [IS, OS]),
			   {ok, State#{client_is => IS,
				       client_os => OS}}
			   
			       
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, {IS, OS}} =
			       ?SEV_AWAIT_READY(Server, server, accept),
			   ?SEV_IPRINT("server streams:"
				       "~n   Inbound:  ~w"
				       "~n   Outbound: ~w", [IS, OS]),
			   {ok, State#{server_is => IS,
				       server_os => OS}}
                   end},

         #{desc => "verify streams",
           cmd  => fun(#{server_is := SIS,
			 server_os := SOS,
			 client_is := CIS,
			 client_os := COS} = _State) ->
			   if
			       (SOS =:= CIS) andalso (SIS =:= COS) ->
				   ?SEV_IPRINT("Streams verified"),
				   ok;
			       (SOS =:= CIS) ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "Server inbound not equal to Client outbound"
					       "~n   ~w =/= ~w", [SIS, COS]),
				   {error, stream_verification_failed};
			       (SIS =:= COS) ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "Server outbound not equal to Client inbound"
					       "~n   ~w =/= ~w", [SOS, CIS]),
				   {error, stream_verification_failed};
			       true ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "~n   Server Inbound:  ~w"
					       "~n   Client Outbound: ~w"
					       "~n   Server Outbound: ~w"
					       "~n   Client Inbound:  ~w",
					       [SIS, COS, SOS, CIS]),
				   {error, stream_verification_failed}
			   end
                   end},


         #{desc => "order client to continue (with status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, status),
                           ok
                   end},
         #{desc => "await client ready (status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, status)
                   end},

         #{desc => "order server to continue (with status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, status),
                           ok
                   end},
         #{desc => "await server ready (status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, status)
                   end},


         #{desc => "order client to continue (with recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order server to continue (with send)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, send),
                           ok
                   end},
         #{desc => "await server ready (send)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, send)
                   end},
         #{desc => "await client ready (recv)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, recv)
                   end},

         #{desc => "order server to continue (with recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, recv),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with send)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send)
                   end},
         #{desc => "await sever ready (recv)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, recv)
                   end},


         #{desc => "order client to continue (with status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, status),
                           ok
                   end},
         #{desc => "await client ready (status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, status)
                   end},

         #{desc => "order server to continue (with status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, status),
                           ok
                   end},
         #{desc => "await server ready (status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, status)
                   end},


         #{desc => "order server to continue (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, stats),
                           ok
                   end},
         #{desc => "await server ready (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, stats)
                   end},

         #{desc => "order client to continue (stats)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, stats),
                           ok
                   end},
         #{desc => "await client ready (stats)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, stats)
                   end},


         #{desc => "order server to continue (with await-shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, await_shutdown),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_IPRINT("order client (~p) to 'terminate'",
                                       [Client]),
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Client) of
                               ok ->
                                   ?SEV_IPRINT("client (~p) terminated",
                                               [Client]),
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (await_shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, await_shutdown)
                   end},

         #{desc => "order server to continue (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, stats),
                           ok
                   end},
         #{desc => "await server ready (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, stats)
                   end},


         %% *** Termination ***
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_IPRINT("order server (~p) to 'terminate'",
                                       [Server]),
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Server) of
                               ok ->
                                   ?SEV_IPRINT("server (~p) terminated",
                                               [Server]),
                                   State1 = maps:remove(server, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},

         %% *** We are done ***
         ?SEV_FINISH_NORMAL
        ],

    %% Only for seqpacket?
    SctpEvs = ?SCTP_EVENTS(true),

    i("start server evaluator"),
    ServerState = #{events   => SctpEvs,
		    domain   => maps:get(domain,      InitState),
		    type     => maps:get(server_type, InitState),
		    protocol => maps:get(protocol,    InitState),
		    addr     => maps:get(addr,        InitState)},
    Server = ?SEV_START("server", ServerSeq, ServerState),

    i("start client evaluator"),
    ClientState = #{events   => SctpEvs,
		    domain   => maps:get(domain,      InitState),
		    type     => maps:get(client_type, InitState),
		    protocol => maps:get(protocol,    InitState),
		    addr     => maps:get(addr,        InitState)},
    Client = ?SEV_START("client", ClientSeq, ClientState),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
			client => Client#ev.pid},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


match_unless_solaris(A, B) ->
    case os:type() of
	{ok, sunos} ->
	    ?SEV_IPRINT("skip assoc-id verification: "
			"~n   ~w, ~w", [A, B]),
	    ok;
	_ ->
	    if
		(A =:= B) ->
		    ?SEV_IPRINT("assoc-id (~w) verified", [A]),
		    ok;
		true ->
		    ?SEV_IPRINT("assoc-id verification failed:"
				"~n   ~w =/= ~w", [A, B]),
		    {error, {assoc_id_verification_failed, A, B}}
	    end
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basic multihoming

m_multihoming_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
		   Domain = inet,
                   case get_addrs_by_family(Domain, 2) of
                       {ok, Addrs} ->
                           #{domain   => Domain,
			     addrs    => lists:sort(Addrs),
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_xhoming(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


m_xhoming(#{domain   := inet_and_inet6 = Domain,
	    addrs    := [[Addr41, Addr42], [Addr61, Addr62]],
	    type     := seqpacket = _Type,
	    protocol := sctp      = _Proto}) ->
    ?P("~s -> entry with"
       "~n   Domain: ~p"
       "~n   Addr41: ~p"
       "~n   Addr42: ~p"
       "~n   Addr61: ~p"
       "~n   Addr62: ~p", [?FUNCTION_NAME,
			   Domain, Addr41, Addr42, Addr61, Addr62]),
    %% Connect to the first address to test bind
    ?P("~s -> connect to first addr to test bind", [?FUNCTION_NAME]),
    basic_open_and_connect([Addr41, Addr61, Addr42], Addr41),
    basic_open_and_connect([Addr61, Addr41], Addr61),

    %% Connect an address, not the first, to test bindx
    ?P("~s -> connect to addr to test bindx", [?FUNCTION_NAME]),
    basic_open_and_connect([Addr61, Addr62, Addr41], Addr62),
    basic_open_and_connect([Addr61, Addr62, Addr41], Addr41),
    ?P("~s -> done", [?FUNCTION_NAME]),
    ok;
m_xhoming(#{domain   := Domain,
	    addrs    := [Addr|_] = Addrs,
	    type     := seqpacket = _Type,
	    protocol := sctp      = _Proto})
  when (Domain =:= inet) orelse (Domain =:= inet6) ->
    basic_open_and_connect(Addrs, Addr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_multihoming_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
		   Domain = inet6,
                   case get_addrs_by_family(Domain, 2) of
                       {ok, Addrs} ->
                           #{domain   => Domain,
			     addrs    => Addrs,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_xhoming(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_unihoming_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
		   Domain = inet6,
                   case get_addrs_by_family(Domain, 1) of
                       {ok, Addrs} ->
                           #{domain   => Domain,
			     addrs    => Addrs,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_xhoming(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_multihoming_ipv4_and_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() ->
		   has_support_ipv4(),
		   has_support_ipv6()
	   end,
    Pre  = fun() ->
		   Domain = inet_and_inet6,
                   case get_addrs_by_family(Domain, 2) of
                       {ok, Addrs} ->
                           #{domain   => Domain,
			     addrs    => Addrs,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_xhoming(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Testing the peeloff function.
%% IPv4 SCTP socket(s).
m_peeloff_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    Cond = fun() -> 
                   %% has_support_socket_priority(),
                   has_support_sctp_nodelay(),
                   has_support_socket_linger(),
                   has_support_ipv4()
           end,
    Pre  = fun() ->
                   Domain = inet,
                   Evs = ?SCTP_EVENTS(true),
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{domain => Domain,
                             addr   => Addr,
                             events => Evs};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_peeloff(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Testing the peeloff function.
%% IPv6 SCTP socket(s).
m_peeloff_ipv6(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    Cond = fun() ->
                   %% has_support_socket_priority(),
                   has_support_sctp_nodelay(),
                   has_support_socket_linger(),
                   has_support_ipv6()
           end,
    Pre  = fun() ->
                   Domain = inet6,
                   Evs = ?SCTP_EVENTS(true),
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{domain => Domain,
                             addr   => Addr,
                             events => Evs};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = m_peeloff(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MPO_REQUEST(Pid, Ref, REQ),
        {?MODULE, Pid, {request, Ref, REQ}}).
-define(MPO_REPLY(Pid, Ref, REPLY),
        {?MODULE, Pid, {reply, Ref, REPLY}}).
-define(MPO_MSG(Pid, MSG),
        {?MODULE, Pid, {msg, MSG}}).

mpo_call({Pid, _}, Req) when is_pid(Pid) ->
    mpo_call(Pid, Req);
mpo_call(Pid, Req) when is_pid(Pid) ->
    Ref = erlang:make_ref(),
    Pid ! ?MPO_REQUEST(self(), Ref, Req),
    receive
        ?MPO_REPLY(Pid, Ref, Reply) ->
            Reply
    end.

mpo_cast(Pid, Info) ->
    Pid ! ?MPO_MSG(self(), Info),
    ok.

m_peeloff(#{domain := Domain,
            addr   := Addr} = State) ->
    Stream          = 3,
    Timeout         = 501,
    %% On some platforms, 'socket:priority' is not supported,
    %% so only include it on platforms that support it.
    InheritOpts =
        case is_supported_socket_priority() of
            true ->
                ?P("~s -> socket:priority *supported* - "
                   "include 'priority' option", [?FUNCTION_NAME]),
                [{{socket, priority}, 3}];
            false ->
                ?P("~s -> socket:priority *not* supported - "
                   "exclude 'priority' option", [?FUNCTION_NAME]),
                []
        end ++ 
        [{{sctp, nodelay}, true}] ++
        %% On Solaris attempting to set linger on a seqpacket socket
        %% results in 'eopnotsupp', so skip this option there.
        case is_solaris() of
            true ->
                ?P("~s -> *solaris* => exclude 'linger' option",
                   [?FUNCTION_NAME]),
                [];
            false ->
                ?P("~s -> *not* solaris => include 'linger' option",
                   [?FUNCTION_NAME]),
                [{{socket, linger}, #{onoff  => true, linger => 7}}]
        end,
    InheritSockOpts = [SockOpt || {SockOpt, _} <- InheritOpts],

    ?P("~s -> try start server", [?FUNCTION_NAME]),
    {{Server, SMRef} = ServerInfo, SPort} =
        m_peeloff_server_start(State),
    ?P("~s -> try start client", [?FUNCTION_NAME]),
    {{Client, CMRef} = ClientInfo, CPort, CAID} =
        m_peeloff_client_start(State, SPort),
    ?P("~s -> server and client started: "
       "~n   Server Info: ~p"
       "~n   Server Port: ~p"
       "~n   Client Info: ~p"
       "~n   Client Port: ~p"
       "~n   Client AID:  ~p",
       [?FUNCTION_NAME,
        ServerInfo, SPort,
        ClientInfo, CPort, CAID]),

    %% Client started implies connected to the server,
    %% but we also need (accept-) confirmation from the server.
    ?P("~s -> await server accept notification", [?FUNCTION_NAME]),
    SAID =
        receive
            {'DOWN', SMRef, process, Server, Reason1} ->
                ?P("~s -> received unexpected DOWN from server: "
                   "~n   ~p", [?FUNCTION_NAME]),
                mpo_cast(Client, terminate),
                exit({server_failed, Reason1});

            {'DOWN', CMRef, process, Client, Reason2} ->
                ?P("~s -> received unexpected DOWN from client: "
                   "~n   ~p", [?FUNCTION_NAME]),
                mpo_cast(Server, terminate),
                exit({client_failed, Reason2});

            ?MPO_MSG(Server, {accepted, SAID0}) ->
                ?P("~s -> expected (accept) response from server: "
                   "~n   AssocID: ~p", [?FUNCTION_NAME, SAID0]),
                SAID0;

            ?MPO_MSG(Pid1, Msg1) ->
                ?P("~s -> unexpected message: "
                   "~n   From: ~p"
                   "~n   Msg:  ~p", [?FUNCTION_NAME, Pid1, Msg1]),
                exit({unexpected_msg, Pid1, Msg1})

        after Timeout ->
                ?P("~s -> unexpected timeout", [?FUNCTION_NAME]),
                mpo_cast(Client, terminate),
                mpo_cast(Server, terminate),
                exit({timeout, server_accepted})
        end,


    %% ==================
    %% Message Transfer: client -> server -> ack to us from server
    %% Instruct the client to send a message and wait for the server
    %% pass it to us (when it gets it).
    ?P("~s -> issue 'send' to client", [?FUNCTION_NAME]),
    case mpo_call(Client, {send, Stream, ?DATA}) of
        ok ->
            ok;
        {error, Reason3a} ->
            ?P("~s -> (handler) send request failed: "
               "~n   ~p", [?FUNCTION_NAME, Reason3a]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({client_send_request_failed, Reason3a})
    end,
    ?P("~s -> await (received-) message from server", [?FUNCTION_NAME]),
    receive
        {'DOWN', SMRef, process, Server, Reason4a} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason4a]),
            mpo_cast(Client, terminate),
            exit({server_failed, Reason4a});

        {'DOWN', CMRef, process, Client, Reason5a} ->
            ?P("~s -> received unexpected DOWN from client: "
               "~n   ~p", [?FUNCTION_NAME, Reason5a]),
            mpo_cast(Server, terminate),
            exit({client_failed, Reason5a});

        ?MPO_MSG(Server, {received, {data, {SAID, Stream, ?DATA}}}) ->
            ?P("~s -> expected (received-) response from server",
               [?FUNCTION_NAME]),
            ok;

        ?MPO_MSG(Pid2a, Msg2a) ->
            ?P("~s -> unexpected message: "
               "~n   From: ~p"
               "~n   Msg:  ~p", [?FUNCTION_NAME, Pid2a, Msg2a]),
            exit({unexpected_msg, Pid2a, Msg2a})

    after Timeout ->
            ?P("~s -> unexpected timeout", [?FUNCTION_NAME]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({timeout, client_send_completion})
    end,



    %% ==================
    %% Message Transfer: server -> client -> ack to us from client
    %% Instruct the server to send a message and wait for the client
    %% pass it to us (when it gets it).
    ?P("~s -> issue 'send' to server", [?FUNCTION_NAME]),
    case mpo_call(Server, {send, Stream, ?DATA}) of
        ok ->
            ok;
        {error, Reason3b} ->
            ?P("~s -> (handler) send request failed: "
               "~n   ~p", [?FUNCTION_NAME, Reason3b]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({client_send_request_failed, Reason3b})
    end,
    ?P("~s -> await (received-) message from server", [?FUNCTION_NAME]),
    receive
        {'DOWN', SMRef, process, Server, Reason4b} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason4b]),
            mpo_cast(Client, terminate),
            exit({server_failed, Reason4b});

        {'DOWN', CMRef, process, Client, Reason5b} ->
            ?P("~s -> received unexpected DOWN from client: "
               "~n   ~p", [?FUNCTION_NAME, Reason5b]),
            mpo_cast(Server, terminate),
            exit({client_failed, Reason5b});

        ?MPO_MSG(Client, {received, {data, {CAID, Stream, ?DATA}}}) ->
            ?P("~s -> expected (received-) response from client",
               [?FUNCTION_NAME]),
            ok;

        ?MPO_MSG(Pid2b, Msg2b) ->
            ?P("~s -> unexpected message: "
               "~n   From: ~p"
               "~n   Msg:  ~p"
               "~nwhen"
               "~n   Client: ~p"
               "~n   CAID:   ~p"
               "~n   Stream: ~p",
               [?FUNCTION_NAME, Pid2b, Msg2b, Client, CAID, Stream]),
            exit({unexpected_msg, Pid2b, Msg2b})

    after Timeout ->
            ?P("~s -> unexpected timeout", [?FUNCTION_NAME]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({timeout, client_send_completion})
    end,


    ?P("~s -> try verify server peer-addr-info", [?FUNCTION_NAME]),
    CSA = #{family => Domain,
            addr   => Addr,
            port   => CPort},
    case mpo_call(Server,
                  {getopt, {sctp, get_peer_addr_info}, #{assoc_id => CAID,
                                                         addr     => CSA}}) of
        {ok, #{assoc_id := _, state := active} = SPAI} ->
            ?P("~s -> received expected PAI: "
               "~n   ~p", [?FUNCTION_NAME, SPAI]),
            ok;
        {error, Reason6} ->
            ?P("~s -> failed get PAI from handler: "
               "~n   ~p", [?FUNCTION_NAME, Reason6]),
            exit({failed_get_handler_pai, Reason6})
    end,


    ?P("~s -> try verify client peer-addr-info", [?FUNCTION_NAME]),
    SSA = #{family => Domain,
            addr   => Addr,
            port   => SPort},
    case mpo_call(Client,
                  {getopt, {sctp, get_peer_addr_info}, #{assoc_id => SAID,
                                                         addr     => SSA}}) of
        {ok, #{assoc_id := _, state := active} = CPAI} ->
            ?P("~s -> received expected PAI: "
               "~n   ~p", [?FUNCTION_NAME, CPAI]),
            ok;
        {error, Reason6b} ->
            ?P("~s -> failed get PAI from handler: "
               "~n   ~p", [?FUNCTION_NAME, Reason6b]),
            exit({failed_get_handler_pai, Reason6b})
    end,



    ?P("~s -> issue set \"inherit\" opts to server", [?FUNCTION_NAME]),
    lists:foreach(
      fun({SockOpt, Value}) ->
              case mpo_call(Server, {setopt, SockOpt, Value}) of
                  ok ->
                      ok;
                  {error, Reason7} ->
                      ?P("~s -> setopt request failed:"
                         "~n   SockOpt: ~p"
                         "~n   Value:   ~p"
                         "~n   Reason:  ~p",
                         [?FUNCTION_NAME, SockOpt, Value, Reason7]),
                      exit({server_setopt_failed, SockOpt, Reason7})
              end
      end,
      InheritOpts),
                     
    ?P("~s -> verify \"inherit\" opts from server", [?FUNCTION_NAME]),
    case [case mpo_call(Server, {getopt, SockOpt}) of
              {ok, Value} ->
                  ?P("~s -> got sockopt from server:"
                     "~n   SockOpt: ~p"
                     "~n   Value:   ~p", [?FUNCTION_NAME, SockOpt, Value]),
                  {SockOpt, Value};
              {error, Reason8} ->
                  ?P("~s -> failed get sockopt from server:"
                     "~n   SockOpt: ~p"
                     "~n   Reason:  ~p", [?FUNCTION_NAME, SockOpt, Reason8]),
                  exit({server_getopt_failed, SockOpt, Reason8})
          end || SockOpt <- InheritSockOpts] of
        InheritOpts ->
            ?P("~s -> \"inherit\" opts from server verified", [?FUNCTION_NAME]),
            ok;
        UnexpectedInheritOpts1 ->
            ?P("~s -> verification of \"inherit\" opts from server failed:"
               "~n   Expected: ~p"
               "~n   Actual:   ~p",
               [?FUNCTION_NAME, InheritOpts, UnexpectedInheritOpts1]),
            exit(inherit_opts_set_verification_failed)
    end,

    ?P("~s -> try server peeloff", [?FUNCTION_NAME]),
    {Handler, HPort} =
        case mpo_call(Server, {peeloff, InheritSockOpts}) of
            {ok, {H, 0}} ->
                ?P("~s -> peeloff success with port = 0 - use (server) ~p",
                   [?FUNCTION_NAME, SPort]),
                {H, SPort};
            {ok, {H, HP}} ->
                ?P("~s -> peeloff success with port = ~p",
                   [?FUNCTION_NAME, HP]),
                {H, HP};
            {error, Reason9} ->
                ?P("~s -> peeloff failed:"
                   "~n   ~p", [?FUNCTION_NAME, Reason9]),
                exit({peeloff, Reason9})
        end,
    ?P("~s -> monitor handler ~p (~w)", [?FUNCTION_NAME, Handler, HPort]),
    HMRef = erlang:monitor(process, Handler),


    ?P("~s -> verify \"inherit\" opts from handler", [?FUNCTION_NAME]),
    case [case mpo_call(Handler, {getopt, SockOpt}) of
              {ok, Value} ->
                  ?P("~s -> got sockopt from handler:"
                     "~n   SockOpt: ~p"
                     "~n   Value:   ~p", [?FUNCTION_NAME, SockOpt, Value]),
                  {SockOpt, Value};
              {error, Reason10} ->
                  ?P("~s -> failed get sockopt from handler:"
                     "~n   ~p", [?FUNCTION_NAME, Reason10]),
                  exit({handler_getopt_failed, SockOpt, Reason10})
          end || SockOpt <- InheritSockOpts] of
        InheritOpts ->
            ?P("~s -> \"inherit\" opts from handler verified",
               [?FUNCTION_NAME]),
            ok;
        UnexpectedInheritOpts2 ->
            ?P("~s -> verification of \"inherit\" opts from handler failed:"
               "~n   Expected: ~p"
               "~n   Actual:   ~p",
               [?FUNCTION_NAME, InheritOpts, UnexpectedInheritOpts2]),
            exit(inherit_opts_verification_failed)
    end,


    ?P("~s -> try verify handler peer-addr-info", [?FUNCTION_NAME]),
    case mpo_call(Handler,
                  {getopt, {sctp, get_peer_addr_info}, #{assoc_id => CAID,
                                                         addr     => CSA}}) of
        {ok, #{assoc_id := _, state := active} = HPAI} ->
            ?P("~s -> received expected PAI: "
               "~n   ~p", [?FUNCTION_NAME, HPAI]),
            ok;
        {error, Reason11} ->
            ?P("~s -> failed get PAI from handler: "
               "~n   ~p", [?FUNCTION_NAME, Reason11]),
            exit({failed_get_handler_pai, Reason11})
    end,

    ?P("~s -> issue 'send' to handler", [?FUNCTION_NAME]),
    case mpo_call(Handler, {send, Stream, ?DATA}) of
        ok ->
            ok;
        {error, Reason12} ->
            ?P("~s -> (handler) send request failed: "
               "~n   ~p", [?FUNCTION_NAME, Reason12]),
            mpo_cast(Client,  terminate),
            mpo_cast(Handler, terminate),
            mpo_cast(Server,  terminate),
            exit({handler_send_request_failed, Reason12})
    end,
       
    ?P("~s -> await (received-) message from client", [?FUNCTION_NAME]),
    receive
        {'DOWN', SMRef, process, Server, Reason13} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason13]),
            mpo_cast(Handler, terminate),
            mpo_cast(Client, terminate),
            exit({server_failed, Reason13});

        {'DOWN', HMRef, process, Handler, Reason14} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason14]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({handler_failed, Reason14});

        {'DOWN', CMRef, process, Client, Reason15} ->
            ?P("~s -> received unexpected DOWN from client: "
               "~n   ~p", [?FUNCTION_NAME, Reason15]),
            mpo_cast(Handler, terminate),
            mpo_cast(Server,  terminate),
            exit({client_failed, Reason15});

        ?MPO_MSG(Client, {received, {data, {CAID, Stream, ?DATA}}}) ->
            ?P("~s -> expected (received-) response from client",
               [?FUNCTION_NAME]),
            ok

    after Timeout ->
            ?P("~s -> unexpected timeout:"
               "~n   MQ: ~p", [?FUNCTION_NAME, ?MQ()]),
            mpo_cast(Client,  terminate),
            mpo_cast(Handler, terminate),
            mpo_cast(Server,  terminate),
            exit({timeout, client_data_response})
    end,

    ?P("~s -> instruct the handler to await shutdown", [?FUNCTION_NAME]),
    mpo_cast(Handler, expect_shutdown),

    ?P("~s -> close server and client sockets", [?FUNCTION_NAME]),
    mpo_cast(Server, close),
    mpo_cast(Client, close),

    ?P("~s -> await handler socket shutdown start confirmation",
       [?FUNCTION_NAME]),
    receive
        {'DOWN', HMRef, process, Handler, Reason16a} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason16a]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({handler_failed, Reason16a});

        ?MPO_MSG(Handler,
                 {received, {shutdown_event, SAID}}) ->
            ?P("~s -> expected shutdown-start from handler",
               [?FUNCTION_NAME]),
            ok

    after Timeout ->
            ?P("~s -> unexpected timeout while waiting for "
                 "(handler) shutdown start confirmation:"
               "~n   MQ: ~p"
               "~nwhen"
               "~n   Handler: ~p"
               "~n   AssocID: ~p", [?FUNCTION_NAME, ?MQ(), Handler, SAID]),
            mpo_cast(Client,  terminate),
            mpo_cast(Handler, terminate),
            mpo_cast(Server,  terminate),
            exit({timeout, handler_shutdown_start})
    end,

    ?P("~s -> await handler socket shutdown complete confirmation",
       [?FUNCTION_NAME]),
    receive
        {'DOWN', HMRef, process, Handler, Reason16b} ->
            ?P("~s -> received unexpected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason16b]),
            mpo_cast(Client, terminate),
            mpo_cast(Server, terminate),
            exit({handler_failed, Reason16b});

        ?MPO_MSG(Handler, {received, {assoc_change, shutdown_comp, SAID}}) ->
            ?P("~s -> expected shutdown-complete from handler",
               [?FUNCTION_NAME]),
            ok

    after Timeout ->
            ?P("~s -> unexpected timeout while waiting for (handler) shutdown confirmation:"
               "~n   MQ: ~p", [?FUNCTION_NAME, ?MQ()]),
            mpo_cast(Client,  terminate),
            mpo_cast(Handler, terminate),
            mpo_cast(Server,  terminate),
            exit({timeout, handler_shutdown})
    end,

    ?P("~s -> terminate client", [?FUNCTION_NAME]),
    mpo_cast(Client,  terminate),
    receive
        {'DOWN', CMRef, process, Client, Reason17} ->
            ?P("~s -> received expected DOWN from client: "
               "~n   ~p", [?FUNCTION_NAME, Reason17]),
            ok

    after Timeout ->
            ?P("~s -> unexpected (client termination) timeout",
               [?FUNCTION_NAME]),
            %% No more mister nice guy
            exit(Client,  kill),
            exit(Handler, kill),
            exit(Server,  kill),
            exit(failed_terminate_client)
    end,

    ?P("~s -> terminate handler", [?FUNCTION_NAME]),
    mpo_cast(Handler,  terminate),
    receive
        {'DOWN', HMRef, process, Handler, Reason18} ->
            ?P("~s -> received expected DOWN from handler: "
               "~n   ~p", [?FUNCTION_NAME, Reason18]),
            ok

    after Timeout ->
            ?P("~s -> unexpected (handler termination) timeout",
               [?FUNCTION_NAME]),
            %% No more mister nice guy
            exit(Handler, kill),
            exit(Server,  kill),
            exit(failed_terminate_handler)
    end,

    ?P("~s -> terminate server", [?FUNCTION_NAME]),
    mpo_cast(Server, terminate),
    receive
        {'DOWN', SMRef, process, Server, Reason19} ->
            ?P("~s -> received expected DOWN from server: "
               "~n   ~p", [?FUNCTION_NAME, Reason19]),
            ok

    after Timeout ->
            ?P("~s -> unexpected (server termination) timeout",
               [?FUNCTION_NAME]),
            %% No more mister nice guy
            exit(Server, kill),
            exit(failed_terminate_server)
    end,

    ?P("~s -> done", [?FUNCTION_NAME]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_peeloff_server_start(State) ->
    Self = self(),
    ServerInfo = {Server, MRef} =
        spawn_monitor(fun() ->
                              m_peeloff_server_init(State#{ctrl => Self})
                      end),
    receive
        {'DOWN', MRef, process, Server, Reason} ->
            ?P("~s -> received unexpected DOWN from starting server:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({server_start_failed, Reason});

        {?MODULE, Server, {started, Port}} ->
            ?P("~s -> server started:"
               "~n   PortNo: ~p", [?FUNCTION_NAME, Port]),
            {ServerInfo, Port}
    end.
                           
m_peeloff_server_init(#{ctrl         := CTRL,
                        domain       := Domain,
                        addr         := Addr,
                        events       := Evs} = State) ->
    ?P("~s -> try open", [?FUNCTION_NAME]),
    Sock = case socket:open(Domain, seqpacket, sctp) of
               {ok, S} ->
                   S;
               {error, Reason1} ->
                   ?P("~s -> open failed:"
                      "~n   ~p", [?FUNCTION_NAME, Reason1]),
                   exit({open_failed, Reason1})
           end,

    ?P("~s -> try bind", [?FUNCTION_NAME]),
    SockAddr = #{family => Domain,
                 addr   => Addr,
                 port   => 0},
    case socket:bind(Sock, SockAddr) of
        ok ->
            ok;
        {error, Reason2} ->
            ?P("~s -> bind failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason2]),
            exit({bind_failed, Reason2})
    end,

    ?P("~s -> try make listen socket", [?FUNCTION_NAME]),
    case socket:listen(Sock, true) of
        ok ->
            ok;
        {error, Reason3} ->
            ?P("~s -> listen failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason3]),
            exit({listen, Reason3})
    end,

    ?P("~s -> try set 'events' (sctp) socket option", [?FUNCTION_NAME]),
    case socket:setopt(Sock, ?MK_SCTP_SOCKOPT(events), Evs) of
        ok ->
            ok;
        {error, Reason4} ->
            ?P("~s -> setopt events failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason4]),
            exit({setopt_events, Reason4})
    end,

    ?P("~s -> try sockname (get port number)", [?FUNCTION_NAME]),
    Port = case socket:sockname(Sock) of
                {ok, #{port := P}} ->
                    P;
                {error, Reason5} ->
                   ?P("~s -> sockname failed:"
                      "~n   ~p", [?FUNCTION_NAME, Reason5]),
                    exit({sockname, Reason5})
            end,

    ?P("~s -> monitor ctrl", [?FUNCTION_NAME]),
    MRef = erlang:monitor(process, CTRL),

    ?P("~s -> inform ctrl (port number: ~p)", [?FUNCTION_NAME, Port]),
    CTRL ! {?MODULE, self(), {started, Port}},

    ?P("~s -> init done", [?FUNCTION_NAME]),
    m_peeloff_server_loop(State#{ctrl_mref    => MRef,
                                 sock         => Sock,
                                 port         => Port,
                                 connection   => undefined,
                                 select       => undefined,
                                 inherit_opts => []}).


%% m_peeloff_server_options(_Sock, []) ->
%%     ?P("~s -> all socket options successfully set", [?FUNCTION_NAME]),
%%     ok;
%% m_peeloff_server_options(Sock, [{SockOpt, Value}|Opts]) ->
%%     case socket:setopt(Sock, SockOpt, Value) of
%%         ok ->
%%             m_peeloff_server_options(Sock, Opts);
%%         {error, Reason} ->
%%             ?P("~s -> setopt events failed:"
%%                "~n   SockOpt: ~p"
%%                "~n   Value:   ~p"
%%                "~n   Reason:  ~p", [?FUNCTION_NAME, SockOpt, Value, Reason]),
%%             exit({setopt, SockOpt, Value, Reason})
%%     end.


m_peeloff_server_loop(#{ctrl       := CTRL,
                        connection := undefined,
                        select     := undefined,
                        sock       := Sock} = State)
  when (Sock =/= undefined) ->
    ?P("~s -> try accept connection", [?FUNCTION_NAME]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := Flags} = Msg} ->
            ?P("~s -> received message - verify connection attempt",
               [?FUNCTION_NAME]),
            case lists:member(notification, Flags) of
                true ->
                    ?SEV_IPRINT("is notification"),
                    case Msg of
                        #{notification :=
                              #{type          := assoc_change,
                                state         := comm_up,
                                assoc_id      := AID,
                                '$esock_name' := sctp_notification}} ->
                            ?P("~s -> "
                               "expected notification - "
                               "assoc-change:comm-up:"
                               "~n   AssocID: ~p", [?FUNCTION_NAME, AID]),
                            NewState = State#{connection => #{assoc_id => AID}},
                            mpo_cast(CTRL, {accepted, AID}),
                            m_peeloff_server_loop(NewState);
                        #{notification := Notif} ->
                            ?P("~s -> unexpected notification:"
                               "~n   ~p", [?FUNCTION_NAME, Notif]),
                            exit({unexpected_notification, Notif});
                        _ ->
                            ?P("~s -> unexpected notication msg :"
                               "~n   ~p", [?FUNCTION_NAME, Msg]),
                            exit({unexpected_notif_message, Msg})
                    end;
                false ->
                    ?P("~s -> unexpected msg :"
                       "~n   ~p", [?FUNCTION_NAME, Msg]),
                    exit({unexpected_message, Msg})
            end;

        {select, SelectInfo} ->
            ?P("~s -> select", [?FUNCTION_NAME]),
            m_peeloff_server_loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?P("~s -> recvmsg failed (accept):"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({recvmsg, accept, Reason})
    end;
m_peeloff_server_loop(#{ctrl       := CTRL,
                        ctrl_mref  := MRef,
                        connection := undefined,
                        select     := {select_info, _, SelectHandle},
                        sock       := Sock} = State) ->
    ?P("~s -> await select message (for accept)", [?FUNCTION_NAME]),
    receive
        {'DOWN', MRef, process, CTRL, Info} ->
            ?P("~s -> "
               "received unexpected DOWN from parent when awaiting connection:"
               "~n   ~p", [?FUNCTION_NAME, Info]),
            (catch socket:close(Sock)),
            exit({parent, Info});

        {'$socket', Sock, select, SelectHandle} ->
            ?P("~s -> received select message", [?FUNCTION_NAME]),
            m_peeloff_server_loop(State#{select => undefined});

        ?MPO_MSG(CTRL, close) ->
            ?P("~s -> received close request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            m_peeloff_server_loop(State#{select => undefined,
                                         sock   => undefined});

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            case maps:get(handler, State, undefined) of
                undefined ->
                    ok;
                {Handler, _} ->
                    mpo_cast(Handler, terminate)
            end,
            exit(normal)

    end;
m_peeloff_server_loop(#{ctrl       := CTRL,
                        connection := #{assoc_id := AID},
                        select     := undefined,
                        sock       := Sock} = State)
  when (Sock =/= undefined) ->
    ?P("~s -> try recv data when"
       "~n   Socket Info:   ~p"
       "~n   Socket Status: ~p",
       [?FUNCTION_NAME, socket:info(Sock), ?WHICH_SCTP_STATUS(Sock, AID)]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AID,
                                      stream   := Stream,
                                      '$esock_name' := sctp_sndrcvinfo}}]}} ->
            ?P("~s -> got data", [?FUNCTION_NAME]),
            Received = {data, {AID, Stream, Data}},
            mpo_cast(CTRL, {received, Received}),
            m_peeloff_server_loop(State);

        %% Solaris and ?
        {ok, #{notification := #{type     := peer_addr_change,
                                 state    := addr_available,
                                 assoc_id := AID}}} ->
            ?P("~s -> "
               "received expected peer-addr-change:addr-available for assoc ~w",
               [?FUNCTION_NAME, AID]),
            m_peeloff_server_loop(State);

        %% FreeBSD and ?
        {ok, #{notification := #{type     := peer_addr_change,
                                 state    := addr_confirmed,
                                 assoc_id := AID}}} ->
            ?P("~s -> "
               "received expected peer-addr-change:addr-confirmed for assoc ~w",
               [?FUNCTION_NAME, AID]),
            m_peeloff_server_loop(State);

        {ok, #{notification := #{type     := shutdown_event,
                                 assoc_id := AID}}} ->
            ?P("~s -> received unexpected shutdown event for ~w",
               [?FUNCTION_NAME, AID]),
            exit({unexpected_shutdown, AID});

        {select, SelectInfo} ->
            ?P("~s -> select", [?FUNCTION_NAME]),
            m_peeloff_server_loop(State#{select => SelectInfo});
        
        {error, Reason} ->
            ?P("~s -> recvmsg failed (data):"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({recvmsg, data, Reason})
    end;
m_peeloff_server_loop(#{ctrl       := CTRL,
                        ctrl_mref  := MRef,
                        connection := #{assoc_id := AID},
                        select     := {select_info, _, SelectHandle},
                        sock       := Sock} = State) ->
    ?P("~s -> await select message (for data) when"
       "~n   Socket Info:   ~p"
       "~n   Socket Status: ~p",
       [?FUNCTION_NAME, socket:info(Sock), ?WHICH_SCTP_STATUS(Sock, AID)]),
    receive
        {'DOWN', MRef, process, CTRL, Info} ->
            ?P("~s -> "
               "received unexpected DOWN from parent when awaiting data:"
               "~n   ~p", [?FUNCTION_NAME, Info]),
            (catch socket:close(Sock)),
            exit({parent, Info});

        {'$socket', Sock, select, SelectHandle} ->
            ?P("~s -> received select message", [?FUNCTION_NAME]),
            m_peeloff_server_loop(State#{select => undefined});

        ?MPO_REQUEST(From, Ref, Req) ->
            NewState = m_peeloff_server_request(State, From, Ref, Req),
            m_peeloff_server_loop(NewState);

        ?MPO_MSG(CTRL, close) ->
            ?P("~s -> received close request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            m_peeloff_server_loop(State#{connection => undefined,
                                         select     => undefined,
                                         sock       => undefined});

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            case maps:get(handler, State, undefined) of
                undefined ->
                    ok;
                {Handler, _} ->
                    mpo_cast(Handler, terminate)
            end,
            exit(normal)

    end;
m_peeloff_server_loop(#{ctrl       := CTRL,
                        ctrl_mref  := MRef} = State) ->
    ?P("~s -> await message (no connection, no socket)", [?FUNCTION_NAME]),
    receive
        {'DOWN', MRef, process, CTRL, Info} ->
            ?P("~s -> "
               "received unexpected DOWN from parent when awaiting data:"
               "~n   ~p", [?FUNCTION_NAME, Info]),
            exit({parent, Info});

        ?MPO_REQUEST(From, Ref, Req) ->
            NewState = m_peeloff_server_request(State, From, Ref, Req),
            m_peeloff_server_loop(NewState);

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            case maps:get(handler, State, undefined) of
                undefined ->
                    ok;
                {Handler, _} ->
                    mpo_cast(Handler, terminate)
            end,
            exit(normal)

    end.


m_peeloff_server_request(#{sock := Sock} = State, From,
                         Ref, {setopt, SockOpt, Value}) ->
    ?P("~s -> setopt when"
       "~n   SockOpt: ~p", [?FUNCTION_NAME, SockOpt]),
    Result = socket:setopt(Sock, SockOpt, Value),
    case Result of
        ok ->
            ?P("~s -> setopt success", [?FUNCTION_NAME]),
            ok;
        {error, Reason} ->
            ?P("~s -> setopt failure: "
               "~n   Reason: ~p"
               "~nwhen"
               "~n   New Value:     ~p"
               "~n   Current Value: ~p"
               "~n   (Socket) Type: ~p",
               [?FUNCTION_NAME, Reason,
                Value,
                case socket:getopt(Sock, SockOpt) of
                    {ok, CurrentValue} ->
                        CurrentValue;
                    {error, _} ->
                        undefined
                end,
                get_socket_type(Sock)]),
            ok
    end,
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_request(#{sock := Sock} = State, From,
                         Ref, {getopt, SockOpt}) ->
    ?P("~s -> getopt(2) when"
       "~n   SockOpt: ~p", [?FUNCTION_NAME, SockOpt]),
    Result = socket:getopt(Sock, SockOpt),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_request(#{sock := Sock} = State, From,
                         Ref, {getopt, SockOpt, Value}) ->
    ?P("~s -> getopt(3) when"
       "~n   SockOpt: ~p", [?FUNCTION_NAME, SockOpt]),
    Result = socket:getopt(Sock, SockOpt, Value),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_request(#{sock       := Sock,
                           connection := #{assoc_id := AID}} = State, From,
                         Ref, {send, Stream, Data}) ->
    ?P("~s -> send (~w bytes on ~w:~w)",
       [?FUNCTION_NAME, byte_size(Data), AID, Stream]),
    SRI = #{assoc_id => AID, stream => Stream},
    Msg = #{iov  => [Data],
            ctrl => [#{level => sctp,
                       type  => sndrcv,
                       value => SRI}]},
    Result = socket:sendmsg(Sock, Msg),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_request(
  #{connection := #{assoc_id := AID},
    select     := {select_info, _, _} = SelectInfo,
    sock       := Sock} = State, From,
  Ref, {peeloff, InheritOpts}) ->
    ?P("~s -> peeloff (cancel current select)", [?FUNCTION_NAME]),
    case socket:cancel(Sock, SelectInfo) of
        ok ->
            %% We start a handler that performs the actual peeloff!
            ?P("~s -> selected recv request cancelled - spawn handler",
               [?FUNCTION_NAME]),
            {{Handler, _} = HandlerInfo, Port} =
                m_peeloff_server_handler_start(State, Sock, AID, InheritOpts),
            ?P("~s -> handler started",
               [?FUNCTION_NAME]),
            From ! ?MPO_REPLY(self(), Ref, {ok, {Handler, Port}}),
            State#{connection => undefined,
                   select     => undefined,
                   handler    => HandlerInfo};
        {error, Reason} ->
            ?P("~s -> cancel failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            From ! ?MPO_REPLY(self(), Ref, {error, {cancel, Reason}}),
            State
    end;
m_peeloff_server_request(State, From, Ref, Req) ->
    ?P("~s -> unknown request: "
       "~n   State: ~p"
       "~n   From:  ~p"
       "~n   Ref:   ~p"
       "~n   Req:   ~p", [?FUNCTION_NAME, State, From, Ref, Req]),
    From ! ?MPO_REPLY(self(), Ref, {error, {unknown_request, Req}}),
    State.
            

get_socket_type(Sock) ->
    case socket:getopt(Sock, {socket, type}) of
        {ok, Type} ->
            Type;
        {error, _} -> % This is not allways supported...
            case socket:getopt(Sock, {otp, type}) of
                {ok, Type} ->
                    Type;
                _ ->
                    undefined
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_peeloff_server_handler_start(#{ctrl      := CTRL,
                                 ctrl_mref := MRef} = State0,
                               Sock, AID, InheritOpts) ->
    ?P("~s -> start handler", [?FUNCTION_NAME]),
    State1 = maps:remove(connection, State0),
    State2 = State1#{select => undefined},
    Self   = self(),
    HandlerInfo = {Handler, HMRef} =
        erlang:spawn_monitor(
          fun() ->
                  m_peeloff_server_handler_init(State2, Self,
                                                Sock, AID, InheritOpts)
          end),
    receive
        {'DOWN', MRef, process, CTRL, Reason} ->
            ?P("~s -> received unexpected down from CTRL:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({ctrl, Reason});

        {'DOWN', HMRef, process, Handler, Reason} ->
            ?P("~s -> received unexpected down from handler:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({handler, Reason});

        {?MODULE, Handler, {started, Port}} ->
            ?P("~s -> received started: ~p", [?FUNCTION_NAME, Port]),
            {HandlerInfo, Port};

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            exit(Handler, kill),
            exit(normal)

    end.
    
m_peeloff_server_handler_init(#{ctrl := CTRL} = State,
                              Parent,
                              Sock, AssocID, InheritOpts) ->
    ?P("~s -> try peeloff", [?FUNCTION_NAME]),
    NewSock =
        case socket:peeloff(Sock, AssocID, InheritOpts) of
            {ok, S} ->
                ?P("~s -> peel off success:"
                   "~n   ~p", [?FUNCTION_NAME, S]),
                S;
            {error, Reason1} ->
                ?P("~s -> peel off failure:"
                   "~n   ~p", [?FUNCTION_NAME, Reason1]),
            exit({peeloff, Reason1})
        end,
    
    ?P("~s -> try sockname", [?FUNCTION_NAME]),
    Port =
        case socket:sockname(NewSock) of
            {ok, #{port := P}} ->
                ?P("~s -> sockname success: ~p", [?FUNCTION_NAME, P]),
                P;
            {error, Reason2} ->
                ?P("~s -> sockname failure:"
                   "~n   ~p", [?FUNCTION_NAME, Reason2]),
            exit({sockname, Reason2})
    end,

    ?P("~s -> inform parent started", [?FUNCTION_NAME]),
    Parent ! {?MODULE, self(), {started, Port}},
    
    ?P("~s -> monitor processes", [?FUNCTION_NAME]),
    CMRef = erlang:monitor(process, CTRL, []),
    PMRef = erlang:monitor(process, Parent, []),

    ?P("~s -> started", [?FUNCTION_NAME]),
    m_peeloff_server_handler_loop(State#{sock        => NewSock,
                                         assoc_id    => AssocID,
                                         ctrl_mref   => CMRef,
                                         server      => Parent,
                                         server_mref => PMRef}).

m_peeloff_server_handler_loop(#{ctrl     := CTRL,
                                sock     := Sock,
                                assoc_id := AssocID,
                                select   := undefined} = State)
  when (Sock =/= undefined) ->
    ?P("~s -> try recvmsg when"
       "~n   Socket Info:   ~p"
       "~n   Socket Status: ~p",
       [?FUNCTION_NAME, socket:info(Sock), ?WHICH_SCTP_STATUS(Sock, AssocID)]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AssocID,
                                      stream   := Stream,
                                      '$esock_name' := sctp_sndrcvinfo}}]}} ->
            ?P("~s -> received data:"
               "~n   From:    ~p"
               "~n   Stream:  ~p"
               "~n   AssocID: ~p", [?FUNCTION_NAME, SA, Stream, AssocID]),
            mpo_cast(CTRL, {received, {data, {AssocID, Stream, Data}}}),
            m_peeloff_server_handler_loop(State);

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := assoc_change,
                                 state    := shutdown_comp,
                                 assoc_id := AssocID,
                                 '$esock_name' := sctp_notification}}} ->
            %% Shutdown is now complete
            ?P("~s -> shutdown-complete (~w) event", [?FUNCTION_NAME, AssocID]),
            mpo_cast(CTRL, {received, {assoc_change, shutdown_comp, AssocID}}),
            m_peeloff_server_handler_loop(State#{sock     => undefined,
                                                 assoc_id => undefined,
                                                 shutdown => complete});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := shutdown_event,
                                 assoc_id := AssocID,
                                 '$esock_name' := sctp_notification}}} ->
            ?P("~s -> received shutdown-event (~w)",
               [?FUNCTION_NAME, AssocID]),
            mpo_cast(CTRL, {received, {shutdown_event, AssocID}}),
            m_peeloff_server_handler_loop(State#{shutdown => 'begin'});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := peer_addr_change,
                                 state    := NState,
                                 assoc_id := AssocID,
                                 '$esock_name' := sctp_notification}}}
          when (NState =:= addr_confirmed) orelse
               (NState =:= addr_available) ->
            ?P("~s -> received "
               "peer-addr-change:~w"
               "for assoc ~w",
               [?FUNCTION_NAME, NState, AssocID]),
            mpo_cast(CTRL, {received,
                            {peer_addr_change, NState, AssocID}}),
            m_peeloff_server_handler_loop(State);

        {select, SelectInfo} ->
            ?P("~s -> select", [?FUNCTION_NAME]),
            m_peeloff_server_handler_loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?P("~s -> recvmsg failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({recvmsg, Reason})
    end;
m_peeloff_server_handler_loop(
  #{ctrl        := CTRL,
    ctrl_mref   := CMRef,
    server      := Server,
    server_mref := SMRef,
    sock        := Sock,
    assoc_id    := AID,
    select      := {select_info, _, SelectHandle}} = State)
  when (Sock =/= undefined) ->
    ?P("~s -> await select when"
       "~n   Socket Info:   ~p"
       "~n   Socket Status: ~p",
       [?FUNCTION_NAME, socket:info(Sock), ?WHICH_SCTP_STATUS(Sock, AID)]),
    receive
        {'DOWN', CMRef, process, CTRL, Reason} ->
            ?P("~s -> received unexpected down from ctrl:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            (catch socket:close(Sock)),
            exit({parent, Reason});

        {'DOWN', SMRef, process, Server, Reason} ->
            ?P("~s -> received unexpected down from server:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            (catch socket:close(Sock)),
            exit({server, Reason});

        {'$socket', Sock, select, SelectHandle} ->
            ?P("~s -> received select message", [?FUNCTION_NAME]),
            m_peeloff_server_handler_loop(State#{select => undefined});

        ?MPO_REQUEST(CTRL, Ref, Req) ->
            ?P("~s -> received request:"
               "~n    ~p", [?FUNCTION_NAME, Req]),
            NewState = m_peeloff_server_handler_request(State,
                                                        CTRL, Ref, Req),
            m_peeloff_server_handler_loop(NewState);

        ?MPO_MSG(CTRL, expect_shutdown) ->
            ?P("~s -> received expect-shutdown request", [?FUNCTION_NAME]),
            m_peeloff_server_handler_loop(State#{shutdown => expect});

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            exit(normal)
    end;
m_peeloff_server_handler_loop(#{ctrl        := CTRL,
                                ctrl_mref   := CMRef,
                                server      := Server,
                                server_mref := SMRef} = State) ->
    ?P("~s -> await message (no socket)", [?FUNCTION_NAME]),
    receive
        {'DOWN', CMRef, process, CTRL, Reason} ->
            ?P("~s -> received unexpected down from ctrl:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({parent, Reason});

        {'DOWN', SMRef, process, Server, Reason} ->
            ?P("~s -> received unexpected down from server:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({server, Reason});

        ?MPO_REQUEST(CTRL, Ref, Req) ->
            ?P("~s -> received request:"
               "~n    ~p", [?FUNCTION_NAME, Req]),
            NewState = m_peeloff_server_handler_request(State,
                                                        CTRL, Ref, Req),
            m_peeloff_server_handler_loop(NewState);

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            exit(normal)
    end.



m_peeloff_server_handler_request(#{sock := Sock} = State,
                                 From,
                                 Ref,
                                 {getopt, SockOpt})
  when (Sock =/= undefined) ->
    ?P("~s -> getopt(2) when"
       "~n   SockOpt:  ~p", [?FUNCTION_NAME, SockOpt]),
    Result = socket:getopt(Sock, SockOpt),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_handler_request(#{sock := Sock} = State,
                                 From,
                                 Ref,
                                 {getopt, SockOpt, OptValue})
  when (Sock =/= undefined) ->
    ?P("~s -> getopt(3) when"
       "~n   SockOpt:  ~p"
       "~n   OptValue: ~p", [?FUNCTION_NAME, SockOpt, OptValue]),
    Result = socket:getopt(Sock, SockOpt, OptValue),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_handler_request(#{sock     := Sock,
                                   assoc_id := AID} = State,
                                 From,
                                 Ref,
                                 {send, Stream, Data})
  when (Sock =/= undefined) ->
    ?P("~s -> send (~w bytes on ~w:~w)",
       [?FUNCTION_NAME, byte_size(Data), AID, Stream]),
    SRI     = #{assoc_id => AID, stream => Stream},
    CtrlSRI = #{level => sctp,
                type  => sndrcv,
                value => SRI},
    Msg = #{iov  => [Data],
            ctrl => [CtrlSRI]},
    Result = socket:sendmsg(Sock, Msg),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_server_handler_request(State, From, Ref, Req) ->
    ?P("~s -> unknown request: "
       "~n   State: ~p"
       "~n   From:  ~p"
       "~n   Ref:   ~p"
       "~n   Req:   ~p", [?FUNCTION_NAME, State, From, Ref, Req]),
    From ! ?MPO_REPLY(self(), Ref, {error, {unknown_request, Req}}),
    State.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_peeloff_client_start(State, ServerPort) ->
    Self = self(),
    ClientInfo = {Client, MRef} =
        spawn_monitor(
          fun() ->
                  m_peeloff_client_init(State#{ctrl        => Self,
                                               server_port => ServerPort})
          end),
    receive
        {'DOWN', MRef, process, Client, Reason} ->
            exit({client_start_failed, Reason});

        {?MODULE, Client, {started, Port, AID}} ->
            {ClientInfo, Port, AID}
    end.
                           
m_peeloff_client_init(#{ctrl        := CTRL,
                        domain      := Domain,
                        addr        := Addr,
                        server_port := ServerPort,
                        events      := Evs} = State) ->
    ?P("~s -> try open", [?FUNCTION_NAME]),
    Sock = case socket:open(Domain, seqpacket, sctp) of
               {ok, S} ->
                   S;
               {error, Reason1} ->
                   ?P("~s -> open failed: "
                      "~n   ~p", [?FUNCTION_NAME, Reason1]),
                   exit({open_failed, Reason1})
           end,

    ?P("~s -> try bind", [?FUNCTION_NAME]),
    SockAddr = #{family => Domain,
                 addr   => Addr,
                 port   => 0},
    case socket:bind(Sock, SockAddr) of
        ok ->
            ok;
        {error, Reason2} ->
            ?P("~s -> bind failed: "
               "~n   ~p", [?FUNCTION_NAME, Reason2]),
            exit({bind_failed, Reason2})
    end,

    ?P("~s -> try setopt events", [?FUNCTION_NAME]),
    case socket:setopt(Sock, {sctp, events}, Evs) of
        ok ->
            ok;
        {error, Reason4} ->
            ?P("~s -> setopt events failed: "
               "~n   ~p", [?FUNCTION_NAME, Reason4]),
            exit({setopt_events, Reason4})
    end,

    ?P("~s -> try sockname (get port number)", [?FUNCTION_NAME]),
    Port = case socket:sockname(Sock) of
                {ok, #{port := P}} ->
                    P;
                {error, Reason5} ->
                   ?P("~s -> sockname failed: "
                      "~n   ~p", [?FUNCTION_NAME, Reason5]),
                    exit({sockname, Reason5})
            end,

    ?P("~s -> try connect to server", [?FUNCTION_NAME]),
    SSA = #{family => Domain,
            addr   => Addr,
            port   => ServerPort},
    case socket:connect(Sock, SSA) of
	ok ->
            ?P("~s -> connect ok", [?FUNCTION_NAME]),
	    ok;
        {error, Reason6} ->
	    ?P("~s -> failed connect: "
	       "~n   ~p", [?FUNCTION_NAME, Reason6]),
	    (catch socket:close(Sock)),
            exit({connect_failed, Reason6})
    end,
    
    ?P("~s -> await connect confirmation", [?FUNCTION_NAME]),
    AssocID =
	case socket:recvmsg(Sock) of
	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := #{type     := assoc_change,
				     state    := comm_up,
				     assoc_id := AID}}} ->
		?P("~s -> received expected connect confirmation:"
                   "~n   Flags:   ~p"
                   "~n   SA:      ~p"
                   "~n   AssocID: ~p",
		   [?FUNCTION_NAME, Flags, SA, AID]),
		AID;
	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := #{type  := assoc_change,
				     state := cant_str_assoc = State}}} ->
		?P("~s -> received unexpected connect failure: ~p"
                   "~n   Flags: ~p"
                   "~n   SA:    ~p",
		   [?FUNCTION_NAME, State, Flags, SA]),
                exit({unexpected, State});
            {ok, Msg} ->
 		?P("~s -> received unexpected msg: "
                   "~n   ~p",
		   [?FUNCTION_NAME, Msg]),
		exit({unexpected_msg, Msg});
	    {error, Reason7} ->
		?P("~s -> unexpected recvmsg failure: "
                   "~n   ~p",
		   [?FUNCTION_NAME, Reason7]),
		exit({unexpected_recvmsg_failure, Reason7})
	end,

    ?P("~s -> monitor ctrl", [?FUNCTION_NAME]),
    MRef = erlang:monitor(process, CTRL),

    ?P("~s -> inform ctrl (port number ~p, assoc id ~w)",
       [?FUNCTION_NAME, Port, AssocID]),
    CTRL ! {?MODULE, self(), {started, Port, AssocID}},

    ?P("~s -> init done", [?FUNCTION_NAME]),
    m_peeloff_client_loop(State#{ctrl_mref => MRef,
                                 sock      => Sock,
                                 port      => Port,
                                 assoc_id  => AssocID,
                                 select    => undefined}).

m_peeloff_client_loop(#{ctrl   := CTRL,
                        sock   := Sock,
                        select := undefined} = State)
  when (Sock =/= undefined) ->
    ?P("~s -> try recvmsg", [?FUNCTION_NAME]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AID,
                                      stream   := Stream,
                                      '$esock_name' := sctp_sndrcvinfo}}]}} ->
            ?P("~s -> received data:"
               "~n   From:    ~p"
               "~n   Stream:  ~p"
               "~n   AssocID: ~p", [?FUNCTION_NAME, SA, Stream, AID]),
            mpo_cast(CTRL, {received, {data, {AID, Stream, Data}}}),
            m_peeloff_client_loop(State);

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := assoc_change,
                                 state    := comm_up,
                                 assoc_id := AID,
                                 '$esock_name' := sctp_notification}}} ->
            ?P("~s -> connect confirmation:"
               "~n   AssocID: ~p", [?FUNCTION_NAME, AID]),
            mpo_cast(CTRL, {received, {assoc_change, comm_up, AID}}),
            m_peeloff_client_loop(State#{assoc_id => AID});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := assoc_change,
                                 state    := shutdown_comp,
                                 assoc_id := AID,
                                 '$esock_name' := sctp_notification}}} ->
            ?P("~s -> connect confirmation:"
               "~n   AssocID: ~p", [?FUNCTION_NAME, AID]),
            mpo_cast(CTRL, {received, {assoc_change, shutdown_comp, AID}}),
            m_peeloff_client_loop(State#{assoc_id => AID});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := shutdown_event,
                                 assoc_id := AID,
                                 '$esock_name' := sctp_notification}}} ->
            ?P("~s -> received assoc (~w) shutdown notification",
               [?FUNCTION_NAME, AID]),
            mpo_cast(CTRL, {received, {shutdown_event, AID}}),
            m_peeloff_client_loop(State#{select => undefined});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := peer_addr_change,
                                 state    := addr_confirmed,
                                 assoc_id := AID,
                                 '$esock_name' := sctp_notification}}} ->
            ?P("~s -> received peer-addr-change:addr-confirmed for assoc ~w",
               [?FUNCTION_NAME, AID]),
            mpo_cast(CTRL, {received, {peer_addr_change, addr_confirmed, AID}}),
            m_peeloff_client_loop(State#{select => undefined});

        {select, SelectInfo} ->
            ?P("~s -> select", [?FUNCTION_NAME]),
            m_peeloff_client_loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?P("~s -> recvmsg failed:"
               "~n   ~p", [?FUNCTION_NAME, Reason]),
            exit({recvmsg, Reason})
    end;
m_peeloff_client_loop(
  #{ctrl      := CTRL,
    ctrl_mref := MRef,
    sock      := Sock,
    select    := {select_info, _, SelectHandle} = SelectInfo} = State) ->
    ?P("~s -> await select message", [?FUNCTION_NAME]),
    receive
        {'DOWN', MRef, process, CTRL, Info} ->
            ?P("~s -> "
               "received unexpected DOWN from ctrl:"
               "~n   ~p", [?FUNCTION_NAME, Info]),
            (catch socket:close(Sock)),
            exit({ctrl_died, Info});

        {'$socket', Sock, select, SelectHandle} ->
            ?P("~s -> received select message", [?FUNCTION_NAME]),
            m_peeloff_client_loop(State#{select => undefined});

        ?MPO_REQUEST(From, Ref, Req) ->
            ?P("~s -> received request ~p from ~p",
               [?FUNCTION_NAME, Ref, From]),
            NewState = m_peeloff_client_request(State, From, Ref, Req),
            m_peeloff_client_loop(NewState);

        ?MPO_MSG(CTRL, close) ->
            ?P("~s -> received close request", [?FUNCTION_NAME]),
            (catch socket:cancel(SelectInfo)),
            (catch socket:close(Sock)),
            m_peeloff_client_loop(State#{select   => undefined,
                                         sock     => undefined,
                                         port     => undefined,
                                         assoc_id => undefined});

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            (catch socket:close(Sock)),
            exit(normal)


    end;
m_peeloff_client_loop(#{ctrl      := CTRL,
                        ctrl_mref := MRef} = State) ->
    ?P("~s -> await message (no socket)", [?FUNCTION_NAME]),
    receive
        {'DOWN', MRef, process, CTRL, Info} ->
            ?P("~s -> "
               "received unexpected DOWN from ctrl:"
               "~n   ~p", [?FUNCTION_NAME, Info]),
            exit({ctrl_died, Info});

        ?MPO_REQUEST(From, Ref, Req) ->
            ?P("~s -> received request ~p from ~p",
               [?FUNCTION_NAME, Ref, From]),
            NewState = m_peeloff_client_request(State, From, Ref, Req),
            m_peeloff_client_loop(NewState);

        ?MPO_MSG(CTRL, terminate) ->
            ?P("~s -> received terminate request", [?FUNCTION_NAME]),
            exit(normal)


    end.


m_peeloff_client_request(#{sock     := Sock,
                           assoc_id := AID} = State,
                         From,
                         Ref,
                         {send, Stream, Data}) when (Sock =/= undefined) ->
    ?P("~s -> send request", [?FUNCTION_NAME]),
    SRI     = #{assoc_id => AID, stream => Stream},
    CtrlSRI = #{level => sctp,
                type  => sndrcv,
                value => SRI},
    Msg = #{iov  => [Data],
            ctrl => [CtrlSRI]},
    Result = socket:sendmsg(Sock, Msg),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_client_request(#{sock := Sock} = State, From,
                         Ref, {setopt, SockOpt, Value})
  when (Sock =/= undefined) ->
    ?P("~s -> setopt", [?FUNCTION_NAME]),
    Result = socket:setopt(Sock, SockOpt, Value),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_client_request(#{sock := Sock} = State, From,
                         Ref, {getopt, SockOpt})
  when (Sock =/= undefined) ->
    ?P("~s -> getopt(2)", [?FUNCTION_NAME]),
    Result = socket:getopt(Sock, SockOpt),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_client_request(#{sock := Sock} = State, From,
                         Ref, {getopt, SockOpt, Value})
  when (Sock =/= undefined) ->
    ?P("~s -> getopt(3)", [?FUNCTION_NAME]),
    Result = socket:getopt(Sock, SockOpt, Value),
    From ! ?MPO_REPLY(self(), Ref, Result),
    State;
m_peeloff_client_request(State, From, Ref, Req) ->
    ?P("~s -> unknown request:"
       "~n   State: ~p"
       "~n   From:  ~p"
       "~n   Ref:   ~p"
       "~n   Req:   ~p", [?FUNCTION_NAME, State, From, Ref, Req ]),
    From ! ?MPO_REPLY(self(), Ref, {error, {unknown_request, Req}}),
    State.
   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Testing the peeloff function.
%% IPv6 SCTP socket(s).
m_recv_close(_Config) when is_list(_Config) ->
    ?TT(?SECS(60)),
    Cond = fun() ->
                   has_support_ipv4()
           end,
    Pre  = fun() ->
                   Domain = inet,
                   Evs    = ?SCTP_EVENTS(true),
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr   => Addr,
                             domain => Domain,
                             events => Evs};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = do_recv_close(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


do_recv_close(#{domain := Domain,
                addr   := Addr,
                events := Evs}) ->
    ?P("~s -> create server socket (and listen)", [?FUNCTION_NAME]),
    {ok, SSock} = socket:open(Domain, seqpacket, sctp),
    ok = socket:setopt(SSock, ?MK_SCTP_SOCKOPT(events), Evs),
    ok = socket:bind(SSock, #{family => Domain, addr => Addr, port => 0}),
    {ok, #{port := SPort}} = socket:sockname(SSock),
    ok = socket:listen(SSock, true),

    ?P("~s -> create client socket (and connect to ~w)",
       [?FUNCTION_NAME, SPort]),
    {ok, CSock} = socket:open(Domain, seqpacket, sctp),
    ok = socket:connect(CSock,
                        #{family => Domain, addr => Addr, port => SPort}),

    TC   = self(),
    RECV = fun() ->
                   ?P("~s:reader -> await connection", [?FUNCTION_NAME]),
                   rc_await_connections(SSock),
                   ?P("~s:reader -> announce ready", [?FUNCTION_NAME]),
                   TC ! {?MODULE, self(), ready},
                   ?P("~s:reader -> try read", [?FUNCTION_NAME]),
                   Result = socket:recvmsg(SSock),
                   ?P("~s:reader -> read result:"
                      "~n   ~p", [?FUNCTION_NAME, Result]),
                   exit(Result)
           end,

    ?P("~s -> spawn reader", [?FUNCTION_NAME]),
    {ReaderPid, ReaderMRef} = erlang:spawn_monitor(RECV),
    receive
        {'DOWN', ReaderMRef, process, ReaderPid, ReaderPreReason} ->
            ?P("~s -> unexpected reader termination:"
               "~n   ~p", [?FUNCTION_NAME, ReaderPreReason]),
            (catch socket:close(CSock)),
            (catch socket:close(SSock)),
            ct:fail("Unexpected pre close from reader (~p): ~p",
                    [ReaderPid, ReaderPreReason]);

        {?MODULE, ReaderPid, ready} ->
            ?P("~s -> reader ready", [?FUNCTION_NAME]),
            ok
    after 30000 ->
            %% This is **extreme**, but there is no way to know
            %% how long it will take to iterate through all the
            %% addresses of a host...
            ?P("~s -> reader ready timeout", [?FUNCTION_NAME]),
            (catch socket:close(SSock)),
            (catch socket:close(CSock)),
            ct:fail("Unexpected pre close timeout (~p)", [ReaderPid])
    end,
    
    ?P("~s -> \"ensure\" reader reading...", [?FUNCTION_NAME]),
    receive
        Any ->
            ?P("~s -> Received unexpected message: "
               "~n   ~p", [Any]),
            (catch socket:close(SSock)),
            (catch socket:close(CSock)),
            ct:fail("Unexpected message: ~p", [Any])

    after 5000 ->
            ok
    end,
            
    ?P("~s -> close server socket", [?FUNCTION_NAME]),
    ok = socket:close(SSock),
    ?P("~s -> await reader termination", [?FUNCTION_NAME]),
    receive
        {'DOWN', ReaderMRef, process, ReaderPid, {error, closed}} ->
            ?P("~s -> expected reader termination result", [?FUNCTION_NAME]),
            ok;

        {'DOWN', ReaderMRef, process, ReaderPid, ReaderPostReason} ->
            ?P("~s -> unexpected reader termination: "
               "~n   ~p", [?FUNCTION_NAME, ReaderPostReason]),
            (catch socket:close(CSock)),
            ct:fail("Unexpected post close from reader (~p): ~p",
                    [ReaderPid, ReaderPostReason])

    after 5000 ->
            ?P("~s -> unexpected reader termination timeout", [?FUNCTION_NAME]),
            demonitor(ReaderMRef, [flush]),
            (catch socket:close(CSock)),
            exit(ReaderPid, kill),
            ct:fail("Reader (~p) termination timeout", [ReaderPid])
    end,

    ?P("~s -> close client socket", [?FUNCTION_NAME]),
    (catch socket:close(CSock)),

    ?P("~s -> done", [?FUNCTION_NAME]),
    ok.


rc_await_connections(S) ->
    rc_await_connections(S, 1).

rc_await_connections(S, N) ->
    ?P("~s(~w) -> await connection", [?FUNCTION_NAME, N]),
    case socket:recvmsg(S, 5000) of
        {ok, #{flags        := _,
               addr         := Sender,
               ctrl         := _,
               notification := Notif}} ->
            ?P("~s(~w) -> connection notification received:"
               "~n   From:         ~p"
               "~n   Notification: ~p", [?FUNCTION_NAME, N, Sender, Notif]),
            rc_await_connections(S, N+1);
        {error, timeout} ->
            ok
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check sndbuf and recbuf behaviour.
%% This is the 'socket' variant of the gen_sctp_SUITE:buffers/1
%% test case. It does not do exactly the same things, because
%% 'socket' behaves slightly differently than inet.
%%

m_buffers(_Config) when is_list(_Config) ->
    ?TT(?SECS(60)),
    Cond = fun() ->
                   has_support_ipv4()
           end,
    Pre  = fun() ->
                   Domain  = inet,
                   Evs     = ?SCTP_EVENTS(true),
                   Stream  = 1,
                   Timeout = 3333,
                   Limit   = 4096,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr    => Addr,
                             domain  => Domain,
                             events  => Evs,
                             stream  => Stream,
                             timeout => Timeout,
                             limit   => Limit};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = do_buffers(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

do_buffers(InitState) ->
    process_flag(trap_exit, true),
    
    ServerSeq =
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
         #{desc => "create (listen) socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, seqpacket, sctp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, epfnosupport = Reason} ->
                                   {skip, Reason};
                               {error, eprotonosupport = Reason} ->
                                   {skip, Reason};
                               {error, esocktnosupport = Reason} ->
                                   {skip, Reason};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed open socket: "
                                               "~n    ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "make listen socket",
           cmd  => fun(#{sock := Sock}) ->
                           socket:listen(Sock, true)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester, port := Port}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init, Port),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (accept connection)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, accept)
                   end},
         #{desc => "accept connection: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case accept_connection(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (accept)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce assoc accepted: "
                                       "~n   Assoc: ~p", [Assoc]),
			   #{inbound_streams  := IS,
			     outbound_streams := OS} = Assoc,
                           ?SEV_ANNOUNCE_READY(Tester, accept, {IS, OS}),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           ?SEV_IPRINT("try get status"),
                           SockOpt      = {sctp, status},
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},


         #{desc => "await continue (rcvbuf)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, rcvbuf)
                   end},
         #{desc => "setopt rcvbuf",
           cmd  => fun(#{sock  := Sock,
                         limit := Limit} = _State) ->
                           ?SEV_IPRINT("try set rcvbuf (to ~w)", [Limit]),
                           SockOpt      = {socket, rcvbuf},
                           case socket:setopt(Sock, SockOpt, Limit) of
                               ok ->
                                   ?SEV_IPRINT("setopt success"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed set rcvbuf: "
                                               "~n   Reason: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "getopt rcvbuf",
           cmd  => fun(#{sock  := Sock,
                         limit := Limit} = State) ->
                           ?SEV_IPRINT("try get rcvbuf for verification"),
                           SockOpt      = {socket, rcvbuf},
                           case socket:getopt(Sock, SockOpt) of
                               {ok, RcvBuf} when (RcvBuf >= Limit) ->
                                   ?SEV_IPRINT("rcvbuf verified: "
                                               "~n   Limit:  ~w"
                                               "~n   RcvBuf: ~w",
                                               [Limit, RcvBuf]),
                                   {ok, State#{rcvbuf => RcvBuf}};
                               {ok, Invalid} ->
                                   ?SEV_EPRINT("Invalid value for rcvbuf: "
                                               "~n   Limit:   ~w"
                                               "~n   Invalid: ~w",
                                               [Limit, Invalid]),
                                   {error, {invalid_rcvbuf, Invalid, Limit}};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed set rcvbuf: "
                                               "~n   Reason: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (rcvbuf)",
           cmd  => fun(#{tester := Tester,
                         rcvbuf := RcvBuf}) ->
			   ?SEV_IPRINT("announce rcvbuf"),
                           ?SEV_ANNOUNCE_READY(Tester, rcvbuf, RcvBuf),
                           ok
                   end},


         #{desc => "await continue (recv)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, ToRead} =
                               ?SEV_AWAIT_CONTINUE(Tester, tester, recv),
                           {ok, State#{to_read => ToRead}}
                   end},
         #{desc => "await data (recvmsg 1)",
           cmd  => fun(#{sock    := Sock,
                         to_read := _ToRead,
                         stream  := _,
                         timeout := Timeout} = State) ->
                           case recv_first_data(Sock, Timeout) of
                               {ok, {SenderSA, Data1, _AID, _}} ->
                                   ?SEV_IPRINT("Received"
                                               "~n   sz(Data1): ~w"
                                               "~n   From:      ~p",
                                               [byte_size(Data1), SenderSA]),
                                   {ok, State#{data => Data1}};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed recv: "
                                               "~n   Reason: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "await data (recvmsg 2)",
           cmd  => fun(#{sock    := Sock,
                         to_read := ToRead,
                         stream  := _Stream,
                         timeout := Timeout,
                         data    := Data1} = State) ->
                           case socket:recvmsg(Sock, Timeout) of
                               {ok, #{addr := SenderSA,
                                      ctrl := _,
                                      iov  := [Data2]}} ->
                                   Data = <<Data1/binary, Data2/binary>>,
                                   ?SEV_IPRINT("Received"
                                               "~n   sz(Data2): ~w"
                                               "~n   From:      ~p"
                                               "~nwhen"
                                               "~n   ToRead:    ~w"
                                               "~n   sz(Data):  ~w",
                                               [byte_size(Data2), SenderSA,
                                                ToRead, byte_size(Data)]),
                                   {ok, State#{data => Data}};
                               {ok, Msg} ->
                                   ?SEV_EPRINT("Received unexpected message: "
                                               "~n   Msg: ~p", [Msg]),
                                   {error, unexpected_msg};
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed recv: "
                                               "~n   Reason: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (recv)",
           cmd  => fun(#{tester := Tester,
                         data   := Data}) ->
			   ?SEV_IPRINT("announce recv"),
                           ?SEV_ANNOUNCE_READY(Tester, recv, Data),
                           ok
                   end},
         

         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           ?SEV_IPRINT("try get status"),
                           SockOpt      = {sctp, status},
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
                           ok
                   end},


         #{desc => "await continue (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, await_shutdown)
                   end},
         #{desc => "await shutdown",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = State) ->
                           case await_shutdown(Sock, AID) of
                               ok ->
                                   {ok, maps:remove(assoc, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (await_shutdown)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce shutdown received"),
                           ?SEV_ANNOUNCE_READY(Tester, await_shutdown),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce stats"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
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
         #{desc => "close (server) socket",
           cmd  => fun(#{sock := Sock} = State) ->
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock, State),
                                   State2 = maps:remove(port, State1),
                                   {ok, State2};
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
                           {Tester, ServerPort} = ?SEV_AWAIT_START(),
                           ?SEV_IPRINT("starting with"
                                       "~n   ServerPort: ~w", [ServerPort]),
                           {ok, State#{tester      => Tester,
                                       server_port => ServerPort}}
                   end},
         #{desc => "monitor tester",
           cmd  => fun(#{tester := Tester} = _State) ->
                           _MRef = erlang:monitor(process, Tester),
                           ok
                   end},

         %% *** Init part ***
         #{desc => "create socket",
           cmd  => fun(#{domain := Domain} = State) ->
                           case socket:open(Domain, seqpacket, sctp) of
                               {ok, Sock} ->
                                   {ok, State#{sock => Sock}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "bind to local address",
           cmd  => fun(#{sock   := Sock,
                         domain := Domain,
                         addr   := Addr} = State) ->
                           LSA = #{family => Domain,
                                   addr   => Addr,
                                   port   => 0},
                           case socket:bind(Sock, LSA) of
                               ok ->
                                   case socket:sockname(Sock) of
                                       {ok, #{port := Port}} ->
                                           ?SEV_IPRINT("bound to port: ~w",
                                                       [Port]),
                                           {ok, State#{port => Port}};
                                       {error, _} = ERROR ->
                                           ERROR
                                   end;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "subscribe to sctp events",
           cmd  => fun(#{sock   := Sock,
                         events := Evs}) ->
                           socket:setopt(Sock, sctp, events, Evs)
                   end},
         #{desc => "announce ready (init)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_ANNOUNCE_READY(Tester, init),
                           ok
                   end},


         %% The actual test
         #{desc => "await continue (connect)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, connect)
                   end},
         #{desc => "try connect",
           cmd  => fun(#{sock        := Sock,
                         domain      := Domain,
                         addr        := Addr,
                         server_port := Port} = _State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
                           case socket:connect(Sock, ServerSA) of
                               ok ->
                                   ?SEV_IPRINT("connected"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed connect:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "await connect confirmation: (recvmsg of assoc-change:comm-up)",
           cmd  => fun(#{sock := Sock} = State) ->
                           case connect_confirmation(Sock) of
                               {ok, {_, Assoc}} ->
                                   {ok, State#{assoc => Assoc}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "try verify assoc (peer-addr-info)",
           cmd  => fun(#{sock        := Sock,
                         domain      := Domain,
                         addr        := Addr,
                         server_port := Port,
			 assoc       := #{assoc_id := AID}} = State) ->
                           ServerSA = #{family => Domain,
                                        addr   => Addr,
                                        port   => Port},
			   ?SEV_IPRINT("try get peer-addr-info"),
                           %% Create sparse peer-addr-info
			   OptValue = #{assoc_id => AID,
                                        addr     => ServerSA},
                           SockOpt  = {sctp, get_peer_addr_info},
			   case socket:getopt(Sock, SockOpt, OptValue) of
			       {ok, #{assoc_id := PAID,
				      state    := active}} ->
				   ?SEV_IPRINT("try verify assoc-id"),
				   match_unless_solaris(AID, PAID);
			       {ok, #{assoc_id := PAID,
				      state    := State}} ->
				   ?SEV_EPRINT("invalid assoc state:"
					       "~n   AID:   ~p"
					       "~n   State: ~p",
					       [PAID, State]),
				   Reason = {invalid_assoc_state, State},
				   {error, Reason};
			       {error, Reason0} ->
				   ?SEV_EPRINT("failed get "
					       "peer-addr-info:"
					       "~n   ~p", [Reason0]),
				   Reason =
				       {failed_get_peer_addr_info, Reason0},
				   {error, Reason}
			   end
		   end},
         #{desc => "announce ready (connect)",
           cmd  => fun(#{tester := Tester, assoc := Assoc}) ->
			   ?SEV_IPRINT("announce connected: "
                                       "~n   Assoc: ~p", [Assoc]),
			   #{inbound_streams  := IS,
			     outbound_streams := OS} = Assoc,
                           ?SEV_ANNOUNCE_READY(Tester, connect, {IS, OS}),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID} = _Assoc} = _State) ->
                           SockOpt      = {sctp, status},
                           SparseStatus = #{assoc_id => AID},
                           case socket:getopt(Sock, SockOpt, SparseStatus) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},


         #{desc => "await continue (sndbuf)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, SndBuf} =
                               ?SEV_AWAIT_CONTINUE(Tester, tester, sndbuf),
                           ?SEV_IPRINT("continue with SndBuf: ~w", [SndBuf]),
                           {ok, State#{sndbuf => SndBuf}}
                   end},
         #{desc => "update sndbuf",
           cmd  => fun(#{sock   := Sock,
                         sndbuf := SndBuf} = _State) ->
                           case socket:setopt(Sock, {socket, sndbuf}, SndBuf) of
                               ok ->
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed set socket:sndbuf: "
                                               "~n   Reason: ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (sndbuf)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce sndbuf"),
                           ?SEV_ANNOUNCE_READY(Tester, sndbuf),
                           ok
                   end},

         #{desc => "await continue (send message)",
           cmd  => fun(#{tester := Tester} = State) ->
                           {ok, Data} =
                               ?SEV_AWAIT_CONTINUE(Tester, tester, send),
                           {ok, State#{data => Data}}
                   end},
         #{desc => "send data back from other process",
           cmd  => fun(#{sock   := Sock,
                         assoc  := #{assoc_id := AID},
                         data   := Data,
                         stream := Stream} = _State) ->
                           ?SEV_IPRINT("build sparse SRI "
                                       "(only assoc-id and "
                                       "stream)"),
                           SRI = #{assoc_id => AID,
                                   stream   => Stream},
                           Msg = #{iov => [Data],
                                   ctrl => [#{level => sctp,
                                              type  => sndrcv,
                                              value => SRI}]},
                           case socket:sendmsg(Sock, Msg) of
                               ok ->
                                   ?SEV_IPRINT("msg sent"),
                                   ok;
                               {error, Reason} = ERROR ->
                                   ?SEV_EPRINT("Failed send msg:"
                                               "~n   ~p", [Reason]),
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (send)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce message sent"),
                           ?SEV_ANNOUNCE_READY(Tester, send),
                           ok
                   end},


         %% GET SCTP-STATUS
         #{desc => "await continue (status)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, status)
                   end},
         #{desc => "status",
           cmd  => fun(#{sock  := Sock,
                         assoc := #{assoc_id := AID}} = _State) ->
                           case socket:getopt(Sock, {sctp, status},
                                              #{assoc_id => AID}) of
                               {ok, Status} ->
                                   ?SEV_IPRINT("status:"
                                               "~n   ~p", [Status]),
                                   ok;
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "announce ready (status)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce status"),
                           ?SEV_ANNOUNCE_READY(Tester, status),
                           ok
                   end},

         #{desc => "await continue (stats)",
           cmd  => fun(#{tester := Tester}) ->
                           ?SEV_AWAIT_CONTINUE(Tester, tester, stats)
                   end},
         #{desc => "statistics",
           cmd  => fun(#{sock := Sock} = _State) ->
                           #{counters := Counters} = socket:info(Sock),
			   ?SEV_IPRINT("Counters: "
                                       "~n   ~p", [Counters]),
                           ok
                   end},
         #{desc => "announce ready (stats)",
           cmd  => fun(#{tester := Tester}) ->
			   ?SEV_IPRINT("announce stats"),
                           ?SEV_ANNOUNCE_READY(Tester, stats),
                           ok
                   end},

         %% *** Terminate ***
         #{desc => "await terminate",
           cmd  => fun(#{tester := Tester} = State) ->
                           case ?SEV_AWAIT_TERMINATE(Tester, tester) of
                               ok ->
                                   ?SEV_IPRINT("received termination "
                                               "from tester"),
                                   {ok, maps:remove(tester, State)};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "close socket",
           cmd  => fun(#{sock := Sock} = State0) ->
                           ?SEV_IPRINT("try close socket"),
                           case socket:close(Sock) of
                               ok ->
                                   State1 = maps:remove(sock,        State0),
                                   State2 = maps:remove(port,        State1),
                                   State3 = maps:remove(server_port, State2),
                                   ?SEV_IPRINT("socket closed and "
                                               "cleanup done"),
                                   {ok, State3};
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
         #{desc => "monitor 'server'",
           cmd  => fun(#{server := Server} = _State) ->
                           _MRef = erlang:monitor(process, Server),
                           ok
                   end},
         #{desc => "order server start",
           cmd  => fun(#{server := Pid} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid),
                           ok
                   end},
         #{desc => "await (server) ready",
           cmd  => fun(#{server := Pid} = State) ->
                           case ?SEV_AWAIT_READY(Pid, acceptor, init) of
                               {ok, Port} ->
                                   {ok, State#{server_port => Port}};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "monitor 'client'",
           cmd  => fun(#{client := Client} = _State) ->
                           _MRef = erlang:monitor(process, Client),
                           ok
                   end},
         #{desc => "order client start",
           cmd  => fun(#{client := Pid, server_port := Port} = _State) ->
                           ?SEV_ANNOUNCE_START(Pid, Port),
                           ok
                   end},
         #{desc => "await client ready (init)",
           cmd  => fun(#{client := Pid} = _State) ->
                           ok = ?SEV_AWAIT_READY(Pid, client, init)
                   end},


         %% *** The actual test ***
         #{desc => "order server to continue (with accept)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, accept),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with connect)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, connect),
                           ok
                   end},
         #{desc => "await client ready (connect)",
           cmd  => fun(#{client := Client} = State) ->
                           {ok, {IS, OS}} =
			       ?SEV_AWAIT_READY(Client, client, connect),
			   ?SEV_IPRINT("client streams:"
				       "~n   Inbound:  ~w"
				       "~n   Outbound: ~w", [IS, OS]),
			   {ok, State#{client_is => IS,
				       client_os => OS}}
			   
			       
                   end},
         #{desc => "await server ready (accept)",
           cmd  => fun(#{server := Server} = State) ->
                           {ok, {IS, OS}} =
			       ?SEV_AWAIT_READY(Server, server, accept),
			   ?SEV_IPRINT("server streams:"
				       "~n   Inbound:  ~w"
				       "~n   Outbound: ~w", [IS, OS]),
			   {ok, State#{server_is => IS,
				       server_os => OS}}
                   end},

         #{desc => "verify streams",
           cmd  => fun(#{server_is := SIS,
			 server_os := SOS,
			 client_is := CIS,
			 client_os := COS} = _State) ->
			   if
			       (SOS =:= CIS) andalso (SIS =:= COS) ->
				   ?SEV_IPRINT("Streams verified"),
				   ok;
			       (SOS =:= CIS) ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "Server inbound not equal to Client outbound"
					       "~n   ~w =/= ~w", [SIS, COS]),
				   {error, stream_verification_failed};
			       (SIS =:= COS) ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "Server outbound not equal to Client inbound"
					       "~n   ~w =/= ~w", [SOS, CIS]),
				   {error, stream_verification_failed};
			       true ->
				   ?SEV_EPRINT("Stream verification failed: "
					       "~n   Server Inbound:  ~w"
					       "~n   Client Outbound: ~w"
					       "~n   Server Outbound: ~w"
					       "~n   Client Inbound:  ~w",
					       [SIS, COS, SOS, CIS]),
				   {error, stream_verification_failed}
			   end
                   end},


         #{desc => "order client to continue (with status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, status),
                           ok
                   end},
         #{desc => "await client ready (status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, status)
                   end},

         #{desc => "order server to continue (with status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, status),
                           ok
                   end},
         #{desc => "await server ready (status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, status)
                   end},



         #{desc => "order server to continue (with rcvbuf)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, rcvbuf),
                           ok
                   end},
         #{desc => "await server ready (status)",
           cmd  => fun(#{server := Server,
                         limit  := Limit} = State) ->
                           {ok, RcvBuf} =
                               ?SEV_AWAIT_READY(Server, server, rcvbuf),
                           ?SEV_IPRINT("server (rcvbuf) ready with: ~w",
                                       [RcvBuf]),
                           BufSz = RcvBuf + Limit,
                           Data = buf_mk_data(BufSz),
                           {ok, State#{bufsz => BufSz,
                                       data  => Data}}
                   end},
         #{desc => "order server to continue (with recv)",
           cmd  => fun(#{server := Server,
                         data   := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server,
                                                  recv, byte_size(Data)),
                           ok
                   end},

         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to continue (with sndbuf)",
           cmd  => fun(#{client := Client,
                         bufsz  := BufSz} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, sndbuf, BufSz),
                           ok
                   end},
         #{desc => "await client ready (sndbuf)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, sndbuf)
                   end},
         #{desc => "order client to continue (with send)",
           cmd  => fun(#{client := Client,
                         data   := Data} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, send, Data),
                           ok
                   end},
         #{desc => "await client ready (send)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, send)
                   end},

         #{desc => "await server ready (recv)",
           cmd  => fun(#{server := Server,
                         data   := Data} = _State) ->
                           case ?SEV_AWAIT_READY(Server, server, recv) of
                               {ok, Data} ->
                                   ?SEV_IPRINT("Expected data received"),
                                   ok;
                               {ok, WrongData} ->
                                   ?SEV_EPRINT("Unexpected data received:"
                                               "~n   sz(Data):      ~w"
                                               "~n   sz(WrongData): ~w",
                                               [byte_size(Data),
                                                byte_size(WrongData)]),
                                   {error, unexpected_data};
                               Any ->
                                   ?SEV_EPRINT("Unexpected response: "
                                               "~n   ~p", [Any]),
                                   {error, unexpected_response}
                           end
                   end},


         #{desc => "order client to continue (with status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, status),
                           ok
                   end},
         #{desc => "await client ready (status)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, status)
                   end},

         #{desc => "order server to continue (with status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, status),
                           ok
                   end},
         #{desc => "await server ready (status)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, status)
                   end},


         #{desc => "order server to continue (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, stats),
                           ok
                   end},
         #{desc => "await server ready (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, stats)
                   end},

         #{desc => "order client to continue (stats)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Client, stats),
                           ok
                   end},
         #{desc => "await client ready (stats)",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_AWAIT_READY(Client, client, stats)
                   end},


         #{desc => "order server to continue (with await-shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, await_shutdown),
                           ok
                   end},
         ?SEV_SLEEP(?SECS(1)),
         #{desc => "order client to terminate",
           cmd  => fun(#{client := Client} = _State) ->
                           ?SEV_IPRINT("order client (~p) to 'terminate'",
                                       [Client]),
                           ?SEV_ANNOUNCE_TERMINATE(Client),
                           ok
                   end},
         #{desc => "await client termination",
           cmd  => fun(#{client := Client} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Client) of
                               ok ->
                                   ?SEV_IPRINT("client (~p) terminated",
                                               [Client]),
                                   State1 = maps:remove(client, State),
                                   {ok, State1};
                               {error, _} = ERROR ->
                                   ERROR
                           end
                   end},
         #{desc => "await server ready (await_shutdown)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, await_shutdown)
                   end},

         #{desc => "order server to continue (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_ANNOUNCE_CONTINUE(Server, stats),
                           ok
                   end},
         #{desc => "await server ready (stats)",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_AWAIT_READY(Server, server, stats)
                   end},


         %% *** Termination ***
         #{desc => "order server to terminate",
           cmd  => fun(#{server := Server} = _State) ->
                           ?SEV_IPRINT("order server (~p) to 'terminate'",
                                       [Server]),
                           ?SEV_ANNOUNCE_TERMINATE(Server),
                           ok
                   end},
         #{desc => "await server termination",
           cmd  => fun(#{server := Server} = State) ->
                           case ?SEV_AWAIT_TERMINATION(Server) of
                               ok ->
                                   ?SEV_IPRINT("server (~p) terminated",
                                               [Server]),
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
    Server = ?SEV_START("server", ServerSeq, InitState),

    i("start client evaluator"),
    Client = ?SEV_START("client", ClientSeq, InitState),

    i("start tester evaluator"),
    TesterInitState = #{server => Server#ev.pid,
			client => Client#ev.pid,
                        limit  => maps:get(limit, InitState)},
    Tester = ?SEV_START("tester", TesterSeq, TesterInitState),

    ok = ?SEV_AWAIT_FINISH([Server, Client, Tester]).


buf_mk_data(Bytes) ->
    buf_mk_data(0, Bytes, <<>>).

buf_mk_data(N, Bytes, Bin) when N < Bytes ->
    buf_mk_data(N+4, Bytes, <<Bin/binary,N:32>>);
buf_mk_data(_, _, Bin) ->
    Bin.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Make sure that we can set (default) SRI

o_default_sri_ipv4(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   Domain = inet,
                   case ?WHICH_LOCAL_ADDR(Domain) of
                       {ok, Addr} ->
                           #{addr     => Addr,
                             domain   => Domain,
                             type     => seqpacket,
                             protocol => sctp};
                       {error, Reason} ->
                           throw({skip, Reason})
                   end
           end,
    TC   = fun(State) ->
                   ok = o_default_sri(State)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).


o_default_sri(#{domain   := Domain,
                type     := seqpacket = Type,
                protocol := sctp      = Proto}) ->
    {ok, S1}   = socket:open(Domain, Type, Proto),
    {ok, SRI0} = socket:getopt(S1, Proto, default_send_param),

    %% Just puck two likely candidates (stream and context)
    Stream0  = maps:get(stream, SRI0),
    Stream1  = Stream0 + 1,
    Context0 = maps:get(context, SRI0),
    Context1 = Context0 + 2,
    SRI1     = SRI0#{stream => Stream1, context => Context1},
    ok       = socket:setopt(S1, Proto, default_send_param, SRI1),
    case socket:getopt(S1, Proto, default_send_param) of
        {ok, #{stream := Stream1, context := Context1}} ->
            ok;
        {ok, UnexpectedSRI} ->
            ?P("Unexpected default SRI value: "
               "~n   ~p", [UnexpectedSRI]),
            exit({unexpected_sri, UnexpectedSRI});
        {error, Reason} ->
            ?P("Unexpected getopt failure: "
               "~n   ~p", [Reason]),
            exit({unexpected_getopt, Reason})
    end,
    socket:close(S1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_open_and_connect(ServerAddresses, ConnectToAddress) ->
    Verify = fun(_, _, _, _, _, _) -> ok end,
    basic_open_and_connect(ServerAddresses, ConnectToAddress, Verify).

basic_open_and_connect(ServerAddresses, ConnectToAddress, Verify)
  when is_list(ServerAddresses) andalso is_function(Verify, 6) ->
    Evs = ?SCTP_EVENTS(true),
    ServerFamily = fam_from_addrs(ServerAddresses),
    ?P("~s -> Serving ~p addresses: "
       "~n   ~p", [?FUNCTION_NAME, ServerFamily, ServerAddresses]),
    LSock = case socket:open(ServerFamily, seqpacket, sctp) of
                {ok, LS} ->
                    LS;
                {error, SOReason} ->
                    throw({sopen, SOReason})
            end,
    ?P("~s -> [server] subscribe to (sctp) events", [?FUNCTION_NAME]),
    ok = socket:setopt(LSock, sctp, events, Evs),
    ?P("~s -> [server] bind to:"
       "~n   ~p", [?FUNCTION_NAME, ServerAddresses]),
    {ok, LPort} = bind(LSock, ServerAddresses),
    ?P("~s -> [server] sockname:"
       "~n   ~p", [?FUNCTION_NAME, ?WHICH_SOCKNAME(LSock)]),
    ?P("~s -> [server] make listen socket", [?FUNCTION_NAME]),
    ok = socket:listen(LSock, true),

    ?P("~s -> [client] create (open) socket", [?FUNCTION_NAME]),
    ConnectToFam = fam_from_addr(ConnectToAddress),
    ClientFamily = ConnectToFam,
    CSock = case socket:open(ClientFamily, seqpacket, sctp) of
		{ok, CS} ->
		    CS;
		{error, COReason} ->
		    throw({copen, COReason})
	    end,
    ?P("~s -> [client] subscribe to (sctp) events", [?FUNCTION_NAME]),
    ok = socket:setopt(CSock, sctp, events, Evs),

    ?P("~s -> [client] try connect", [?FUNCTION_NAME]),
    case socket:connect(CSock,
			#{family => fam_from_addr(ConnectToAddress),
			  addr   => ConnectToAddress,
			  port   => LPort}) of
	ok ->
	    ok;
	{error, ConnectReason} ->
	    ?P("~s -> failed connect: "
	       "~n   ~p", [?FUNCTION_NAME, ConnectReason]),
	    ?P("~s -> try close client socket", [?FUNCTION_NAME]),
	    _ = socket:close(CSock),
	    ?P("~s -> try close server socket", [?FUNCTION_NAME]),
	    _ = socket:close(LSock),
	    ?P("~s -> sockets closed - done with failure", [?FUNCTION_NAME]),
	    exit({connect_failed, ConnectReason})
    end,
    ?P("~s -> [client] await connect response", [?FUNCTION_NAME]),
    ClientAID =
	case socket:recvmsg(CSock) of
	    {ok, #{notification := #{type     := assoc_change,
				     state    := comm_up,
				     assoc_id := CAID}}} ->
		?P("~s -> [client] expected connect response received: ~w",
		   [?FUNCTION_NAME, CAID]),
		CAID;
	    {ok, CUnexpMsg} ->
		?P("~s -> [client] unexpected connect response received: ~p",
		   [?FUNCTION_NAME, CUnexpMsg]),
		exit({unexpected_recvmsg_msg, CUnexpMsg});
	    {error, CReason} ->
		?P("~s -> [client] unexpected connect failure: ~p",
		   [?FUNCTION_NAME, CReason]),
		exit({unexpected_recvmsg_failure, CReason})
	end,

    ?P("~s -> [server] try accept connection", [?FUNCTION_NAME]),
    ServerAID =
	case socket:recvmsg(LSock) of
	    {ok, #{notification := #{type     := assoc_change,
				     state    := comm_up,
				     assoc_id := SAID}}} ->
		?P("~s -> [server] expected connection received: ~w",
		   [?FUNCTION_NAME, SAID]),
		SAID;
	    {ok, SUnexpMsg} ->
		?P("~s -> [server] unexpected accept response: ~p",
		   [?FUNCTION_NAME, SUnexpMsg]),
		exit({unexpected_s_recvmsg_msg, SUnexpMsg});
	    {error, SReason} ->
		?P("~s -> [server] unexpected accept failure: ~p",
		   [?FUNCTION_NAME, SReason]),
		exit({unexpected_s_recvmsg_failure, SReason})
	end,

    ?P("~s -> "
       "~n   [client] sockname:"
       "~n      ~p"
       "~n   [client] (assoc ~w) status:"
       "~n      ~p"
       "~n   [server] (assoc ~w) status:"
       "~n      ~p",
       [?FUNCTION_NAME,
        ?WHICH_SOCKNAME(CSock),
        ClientAID, ?WHICH_SCTP_STATUS(CSock, ClientAID),
        ServerAID, ?WHICH_SCTP_STATUS(LSock, ServerAID)]),


    ?P("~s -> try verify result", [?FUNCTION_NAME]),
    Result = Verify(LSock, ServerFamily, ServerAID,
		    CSock, ClientFamily, ClientAID),

    ?P("~s -> close sockets", [?FUNCTION_NAME]),
    ok = socket:close(CSock),
    ok = socket:close(LSock),

    ?P("~s -> done when result: ~p", [?FUNCTION_NAME, Result]),
    Result.
	
%% Any inet6 address => inet6 otherwise inet.
fam_from_addrs(Addrs) ->
    ?P("~s -> entry with"
       "~n   Addrs: ~p", [?FUNCTION_NAME, Addrs]),
    case lists:usort([fam_from_addr(Addr) || Addr <- Addrs]) of
	[inet, inet6] -> inet6;
	[inet]        -> inet;
	[inet6]       -> inet6
    end.

fam_from_addr(Addr) when tuple_size(Addr) =:= 4 -> inet;
fam_from_addr(Addr) when tuple_size(Addr) =:= 8 -> inet6.


bind(Sock, [Addr|Addrs]) ->
    SA = #{family => fam_from_addr(Addr),
           addr   => Addr,
           port   => 0},
    ?P("~s -> try bind socket to (primary) address", [?FUNCTION_NAME]),
    case socket:bind(Sock, SA) of
        ok when (length(Addrs) > 0) ->
            ?P("~s -> try get sockname", [?FUNCTION_NAME]),
            case socket:sockname(Sock) of
                {ok, #{port := Port}} ->
                    Addrs2 = [#{family => fam_from_addr(A),
                                addr   => A,
                                port   => Port} || A <- Addrs],
                    ?P("~s -> try bind rest addrs: "
		       "~n   ~p", [?FUNCTION_NAME, Addrs2]),
                    case socket:bind(Sock, Addrs2, add) of
                        ok ->
                            {ok, Port};
                        {error, Reason} ->
                            exit({bind3, Reason})
                    end;
                {error, Reason} ->
                    exit({sockname, Reason})
            end;
	ok ->
            case socket:sockname(Sock) of
                {ok, #{port := Port}} ->
		    {ok, Port};
                {error, Reason} ->
                    exit({sockname, Reason})
            end;		
        {error, Reason} ->
            exit({bind2, Reason})
    end;
bind(Sock, Addr) ->
    SA = #{family => fam_from_addr(Addr),
	   addr   => Addr,
	   port   => 0},
    socket:bind(Sock, SA).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_addrs_by_family(Fam, NumAddrs) ->
    case os:type() of
        {unix, OS} when (OS =:= linux) orelse (OS =:= freebsd) ->
            get_addrs_by_family2(Fam, NumAddrs);
        {unix, sunos} ->
            case get_addrs_by_family2(Fam, NumAddrs) of
                {ok, _} = OK when (Fam =:= inet) orelse (Fam =:= inet6) ->
                    OK;
                {ok, [Addrs4, Addrs6]} when (Fam =:= inet_and_inet6) ->
                    {ok, [ipv4_map_addrs(Addrs4), Addrs6]};
                {error, _} = ERROR ->
                    ERROR
            end;
        OS ->
            {error, {unsupported_os, OS}}
    end.

get_addrs_by_family2(Fam, NumAddrs)
  when (Fam =:= inet) orelse (Fam =:= inet6) ->
    {ok, get_addrs_by_family3(Fam, NumAddrs)};
get_addrs_by_family2(inet_and_inet6, NumAddrs) ->
    Addrs4 = get_addrs_by_family3(inet,  NumAddrs),
    Addrs6 = get_addrs_by_family3(inet6, NumAddrs),
    {ok, [Addrs4, Addrs6]}.

get_addrs_by_family3(Fam, NumAddrs) ->
    case net:getifaddrs(Fam) of
	{ok, IFs} ->
	    case get_addrs_by_family4(Fam, IFs) of
		Addrs when length(Addrs) >= NumAddrs ->
		    lists:sublist(Addrs, NumAddrs);
		Addrs ->
		    throw({too_few_addrs, length(Addrs), NumAddrs})
	    end;
	{error, Reason} ->
	    throw({no_addresses, Reason})
    end.

get_addrs_by_family4(Fam, IFs) ->
    lists:flatten([Addr || #{flags := Flags,
			     addr  := #{addr := Addr}} <- IFs,
			   lists:member(up, Flags) andalso
			       lists:member(running, Flags) andalso
			       is_good_addr(Fam, Addr)]).

is_good_addr(inet, Addr) when tuple_size(Addr) =:= 4 ->
    true;
is_good_addr(inet6, {0,0,0,0,0,16#ffff,_,_}) ->
    false; %% ipv4 mapped
is_good_addr(inet6, {16#fe80,_,_,_,_,_,_,_}) ->
    false; %% link-local
is_good_addr(inet6, Addr) when tuple_size(Addr) =:= 8 ->
    true;
is_good_addr(_Family, _Addr) ->
    false.

	    
ipv4_map_addrs(InetAddrs) ->
    [begin
	 <<AB:16>> = <<A,B>>,
	 <<CD:16>> = <<C,D>>,
	 {0, 0, 0, 0, 0, 16#ffff, AB, CD}
     end || {A,B,C,D} <- InetAddrs].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_from_other_process(Fun) when is_function(Fun, 0) ->
    Parent = self(),
    Ref    = make_ref(),
    {_, MRef} =
	spawn_monitor(fun() ->
			      try Fun() of
				  Result ->
				      Parent ! {Ref, Result}
			      catch
				  Class:Reason:Stacktrace ->
				      Parent ! {Ref, Class, Reason, Stacktrace}
			      end
		      end),
    receive
	{Ref, Result} ->
	    receive
		{'DOWN', MRef, _, _, _} ->
		    Result
	    end;
	{Ref, Class, Reason, Stacktrace} ->
	    receive
		{'DOWN', MRef, _, _, _} ->
		    erlang:raise(Class, Reason, Stacktrace)
	    end;
	{'DOWN',MRef, _, _, Reason} ->
	    erlang:exit(Reason)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_exchange_st_ipv4(_Config) when is_list(_Config) ->
    ?TT(?MINS(2)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   #{domain      => inet,
                     num_clients => 3}
           end,
    TC   = fun(Conf) ->
                   t_exchange_st(Conf)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

t_exchange_st_ipv6(_Config) when is_list(_Config) ->
    ?TT(?MINS(2)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   #{domain      => inet6,
                     num_clients => 3}
           end,
    TC   = fun(Conf) ->
                   t_exchange_st(Conf)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

t_exchange_mt_ipv4(_Config) when is_list(_Config) ->
    ?TT(?MINS(2)),
    Cond = fun() -> has_support_ipv4() end,
    Pre  = fun() ->
                   #{domain      => inet,
                     num_clients => 3}
           end,
    TC   = fun(Conf) ->
                   t_exchange_mt(Conf)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

t_exchange_mt_ipv6(_Config) when is_list(_Config) ->
    ?TT(?MINS(2)),
    Cond = fun() -> has_support_ipv6() end,
    Pre  = fun() ->
                   #{domain      => inet6,
                     num_clients => 3}
           end,
    TC   = fun(Conf) ->
                   t_exchange_mt(Conf)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME, Cond, Pre, TC, Post).

t_exchange_st(Conf) ->
    t_exchange(Conf#{threaded => false}).

t_exchange_mt(Conf) ->
    t_exchange(Conf#{threaded => true}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t_exchange(Conf) ->
    put(sname, "TC"),

    ?P("~s -> start server", [?FUNCTION_NAME]),
    {ok, {Server, ServerSA}} = t_exc_start_server(Conf),
    
    ?P("~s -> start clients", [?FUNCTION_NAME]),
    Clients = t_exc_start_clients(Conf, ServerSA),

    ?P("~s -> release clients", [?FUNCTION_NAME]),
    t_exc_release_clients(Clients, ?TRAFFIC_RUN_TIME),
    
    ?P("~s -> release clients", [?FUNCTION_NAME]),
    Result = t_exc_await_clients(Server, Clients, ?TRAFFIC_RUN_TIME),

    ?P("~s -> stop server", [?FUNCTION_NAME]),
    t_exc_stop_server(Server),

    ?P("~s -> done when"
       "~n   Result: ~p msgs/msec", [?FUNCTION_NAME, Result]),
    {comment, ?F("~w msgs/msec", [Result])}.


t_exc_start_clients(#{num_clients := NumClients}, ServerSA) ->
    t_exc_start_clients(NumClients, 1, ServerSA, []).

t_exc_start_clients(N, ID, ServerSA, Acc) when (N > 0) ->
    ?P("~s -> try start client ~w", [?FUNCTION_NAME, ID]),
    case socket_sctp_traffic_client:start(ID,
                                          ServerSA,
                                          ?TRAFFIC_DATA,
                                          #{debug => false}) of
        {ok, {{Pid, MRef}, {PortNo, AssocID}}} ->
            ?P("~s -> client ~w started", [?FUNCTION_NAME, ID]),
            t_exc_start_clients(N-1, ID+1, ServerSA,
                                [#{id       => ID,
                                   pid      => Pid,
                                   mref     => MRef,
                                   port     => PortNo,
                                   assoc_id => AssocID} | Acc]);
         {error, Reason} ->
            ?P("~s -> Failed starting agent ~w: "
               "~n   Reason: ~p", [?FUNCTION_NAME, ID, Reason]),
            t_exc_stop_clients(Acc),
            ct:fail("Failed starting client ~w", [ID])
    end;
t_exc_start_clients(0, _, _ServerSA, Acc) ->
    lists:reverse(Acc).

t_exc_stop_clients([]) ->
    ok;
t_exc_stop_clients([#{id  := ID,
                      pid := Pid} | Clients]) ->
    ?P("~s -> try stop client ~p (~p)", [?FUNCTION_NAME, ID, Pid]),
    socket_sctp_traffic_client:stop(Pid),
    t_exc_stop_clients(Clients).

t_exc_release_clients([], _) ->
    ?P("~s -> all client(s) released", [?FUNCTION_NAME]),
    ok;
t_exc_release_clients([#{id := ID,
                         pid := Pid} | Clients], RunTime) ->
    ?P("~s -> release client ~p (~p)", [?FUNCTION_NAME, ID, Pid]),
    socket_sctp_traffic_client:start_run(Pid, RunTime),
    t_exc_release_clients(Clients, RunTime).

t_exc_await_clients(Server, Clients, RunTime) ->
    t_exc_await_clients(Server, Clients, RunTime, RunTime+?SECS(10), []).

t_exc_await_clients(_Server, [], RunTime, _Timeout, Acc) ->
    {TotCnt, _OK, 0} =
        lists:foldl(fun({ok, _ID, Cnt}, {Sum, Ok, Err}) ->
                            {Cnt + Sum, Ok+1, Err};
                       ({error, _ID, _Error}, {Sum, Ok, Err}) ->
                            {Sum, Ok, Err+1}
                    end,
                    {0, 0, 0}, Acc),
    TotCnt div RunTime;
t_exc_await_clients({ServerPid, ServerMRef} = Server, Clients,
                    RunTime, Timeout, Acc) when (Timeout > 0) ->
    T1 = ?TS(),
    receive
        {'DOWN', ServerMRef, process, ServerPid, Info} ->
            ?P("~s -> Unexpected DOWN from server: "
               "~n   Reason: ~p", [?FUNCTION_NAME, Info]),
            t_exc_stop_clients(Clients),
            ct:fail("Unexpected server failure", []);

        {'DOWN', _MRef, process, Pid, {result, Cnt, Mode}} ->
            {Clients2, ID} = t_exc_process_client_down(Clients, Pid),
            ?P("~s -> Got result from client ~w:"
               "~n   Cnt:  ~w"
               "~n   Mode: ~w", [?FUNCTION_NAME, ID, Cnt, Mode]),
            T2 = ?TS(),
            Timeout2 = t_exc_await_clients_next_timeout(Timeout, T1, T2),
            t_exc_await_clients(Server, Clients2, RunTime,
                                Timeout2,
                                [{ok, ID, Cnt} | Acc]);

        {'DOWN', _MRef, process, Pid, Info} ->
            {Clients2, ID} = t_exc_process_client_down(Clients, Pid),
            ?P("~s -> Unexpected down from client ~w:"
               "~n   Info: ~w", [?FUNCTION_NAME, ID, Info]),
            T2 = ?TS(),
            Timeout2 = t_exc_await_clients_next_timeout(Timeout, T1, T2),
            t_exc_await_clients(Server, Clients2, RunTime, Timeout2,
                                [{error, ID, Info} | Acc])

    after Timeout ->
            t_exc_stop_clients(Clients),
            ct:fail("Client run timeout", [])
    end;
t_exc_await_clients(Server, Clients, _RunTime, _Timeout, Acc) ->
    ?P("~s -> Timeout while waiting for client results:"
       "~n   (remaining) Clients: ~p"
       "~n   Results:             ~p", [?FUNCTION_NAME, Clients, Acc]),
    t_exc_stop_server(Server),
    t_exc_stop_clients(Clients),
    ct:fail("Client timeout", []).


t_exc_await_clients_next_timeout(Timeout, T1, T2) ->
    Timeout2 = Timeout - (T2-T1),
    if
        (Timeout2 > 0) ->
            Timeout2;
        true ->
            0
    end.

t_exc_process_client_down(Clients, DownPid) ->
    {Acc, #{id := ID}} =
        lists:foldl(fun(#{pid := Pid} = Client, {Acc, undefined})
                          when (Pid =:= DownPid) ->
                            {Acc, Client};
                       (Client, {Acc, C}) ->
                            {[Client|Acc], C}
                    end,
                    {[], undefined},
                    Clients),
    {lists:reverse(Acc), ID}.


t_exc_start_server(Conf) ->
    socket_sctp_traffic_server:start(Conf#{debug => false}).

t_exc_stop_server({Pid, _}) ->
    socket_sctp_traffic_server:stop(Pid).
        
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

is_solaris() ->
    case os:type() of
        {unix, sunos} ->
            true;
        _ ->
            false
    end.

has_support_sctp() ->
    case os:type() of
        {win32, _} ->
            skip("SCTP Not Supported");
        {unix, netbsd} ->
            %% XXX We will have to investigate this later...
            skip("SCTP Not Supported");
        _ ->
            case socket:is_supported(sctp) of
                true ->
                    ok;
                false ->
                    skip("SCTP Not Supported")
            end
    end.


%% The idea is that this function shall test if the test host has 
%% support for IPv4 or IPv6. If not, there is no point in running
%% corresponding tests.
%% Currently we just skip.
has_support_ipv4() ->
    ?KLIB:has_support_ipv4().

has_support_ipv6() ->
    ?KLIB:has_support_ipv6().

%% has_support_socket_priority() ->
%%     has_support_socket_option_sock(priority).

is_supported_socket_priority() ->
    is_supported_socket_option_sock(priority).

has_support_socket_linger() ->
    has_support_socket_option_sock(linger).

has_support_sctp_nodelay() ->
    has_support_socket_option_sctp(nodelay).


has_support_socket_option_sock(Opt) ->
    has_support_socket_option(socket, Opt).

is_supported_socket_option_sock(Opt) ->
    is_supported_socket_option(socket, Opt).

has_support_socket_option_sctp(Opt) ->
    has_support_socket_option(sctp, Opt).


has_support_socket_option(Level, Option) ->
    case socket:is_supported(options, Level, Option) of
        true ->
            ok;
        false ->
            skip(?F("Not Supported: ~w option ~w", [Level, Option]))
    end.


is_supported_socket_option(Level, Option) ->
    socket:is_supported(options, Level, Option).


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
%% tc_try(Case, TCFun) ->
%%     ?TC_TRY(Case, TCFun).

tc_try(Case, TCCondFun, TCFun) ->
    ?TC_TRY(Case, TCCondFun, TCFun).

tc_try(Case, TCCondFun, TCPreFun, TCFun, TCPostFun) ->
    ?TC_TRY(Case, TCCondFun, TCPreFun, TCFun, TCPostFun).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start_node(Name) ->
%%     start_node(Name, 5000).

%% start_node(Name, Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
%%     Pa   = filename:dirname(code:which(?MODULE)),
%%     Args = ["-pa", Pa,
%%             "-s", atom_to_list(?PROXY), "start", atom_to_list(node()),
%%             "-s", "global", "sync"],
%%     try ?CT_PEER(#{name      => Name,
%%                    wait_boot => Timeout,
%%                    args      => Args}) of
%%         {ok, Peer, Node} ->
%%             ?SEV_IPRINT("Started node ~p - now (global) sync", [Name]),
%%             global:sync(), % Again, just in case...
%%             ?SEV_IPRINT("ping proxy"),
%%             pong = ?PPING(Node),
%%             {Peer, Node};
%%         {error, Reason} ->
%%             ?SEV_EPRINT("failed starting node ~p (=> SKIP):"
%%                         "~n   ~p", [Name, Reason]),
%%             skip(Reason)
%%     catch
%%         Class:Reason:Stack ->
%%             ?SEV_EPRINT("Failed starting node: "
%%                         "~n   Class:  ~p"
%%                         "~n   Reason: ~p"
%%                         "~n   Stack:  ~p",
%%                         [Class, Reason, Stack]),
%%             skip({node_start, Class, Reason})
%%     end.

            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% nowait(Config) ->
%%     case lists:member({select_handle, true}, Config) of
%%         true ->
%%             make_ref();
%%         false ->
%%             nowait
%%     end.

i(F) ->
    i(F, []).

i(F, A) ->
    FStr = ?F("[~s] " ++ F, [?FTS()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

