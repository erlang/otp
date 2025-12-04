%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

-ifndef(socket_test_lib_hrl).
-define(socket_test_lib_hrl, true).

-define(SLIB, socket_test_lib).

%% 'BENCH_SUITE' needs to be defined by the module using this macro
-define(BENCH_EVENT(__N__, __V__),
        #event{name = benchmark_data,
               data = [{suite, ?BENCH_SUITE},
                       {value, (__V__)},
                       {name,  (__N__)}]}).

-define(PI(I),      ?SLIB:pi((I))).
-define(PI(P,I),    ?SLIB:pi((P), (I))).
-define(PI(N,P,I),  ?SLIB:pi((N), (P), (I))).

-define(PCALL(F,T,D), ?SLIB:pcall((F), (T), (D))).


-define(MK_SOCKOPT(Lvl, Opt), {(Lvl), (Opt)}).
-define(MK_SOCKOPT_OTP(Opt),  ?MK_SOCKOPT(otp,    (Opt))).
-define(MK_SOCKOPT_SOCK(Opt), ?MK_SOCKOPT(socket, (Opt))).
-define(MK_SOCKOPT_IP(Opt),   ?MK_SOCKOPT(ip,     (Opt))).
-define(MK_SOCKOPT_IPV6(Opt), ?MK_SOCKOPT(ipv6,   (Opt))).
-define(MK_SOCKOPT_SCTP(Opt), ?MK_SOCKOPT(sctp,   (Opt))).

%% A server *may* not want to get any data event
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

-define(GET_SCTP_STATUS(Sock, AID),
        socket:getopt(Sock, ?MK_SOCKOPT_SCTP(status), #{assoc_id => AID})).
-define(WHICH_SCTP_STATUS(Sock, AID),
        case ?GET_SCTP_STATUS(Sock, AID) of
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
-define(ENABLE_SOCK_DEBUG(S),  socket:setopt(S, ?MK_SOCKOPT_OTP(debug), true)).
-define(DISABLE_SOCK_DEBUG(S), socket:setopt(S, ?MK_SOCKOPT_OTP(debug), false)).


-define(HAS_SUPPORT_SOCKET_OPTION(L, O),
        ?SLIB:has_support_socket_option((L), (O))).
-define(HAS_SUPPORT_SOCKET_OPTION_SOCK(O),
        ?SLIB:has_support_socket_option_sock((O))).
-define(HAS_SUPPORT_SOCKET_OPTION_IP(O),
        ?SLIB:has_support_socket_option_ip((O))).
-define(HAS_SUPPORT_SOCKET_OPTION_ipv6(O),
        ?SLIB:has_support_socket_option_ipv6((O))).
-define(HAS_SUPPORT_SOCKET_OPTION_TCP(O),
        ?SLIB:has_support_socket_option_tcp((O))).
-define(HAS_SUPPORT_SOCKET_OPTION_UDP(O),
        ?SLIB:has_support_socket_option_udp((O))).
-define(HAS_SUPPORT_SOCKET_OPTION_SCTP(O),
        ?SLIB:has_support_socket_option_sctp((O))).
-define(IS_ANY_OPTIONS_SUPPORTED(OPTS),
        ?SLIB:is_any_options_supported((OPTS))).

-define(IS_SUPPORTED_SOCKET_OPTION(LEVEL, OPTION),
        socket:is_supported(options, (LEVEL), (OPTION))).



-define(IS_GOOD_ENOUGH_PLATFORM(FAM, N, CVSN),
        ?SLIB:is_good_enough_platform((FAM), (N), (CVSN))).
-define(IS_NOT_PLATFORM(P, PS), ?SLIB:is_not_platform((P), (PS))).
-define(IS_NOT_FREEBSD(),       ?SLIB:is_not_freebsd()).
-define(IS_NOT_OPENBSD(),       ?SLIB:is_not_openbsd()).
-define(IS_NOT_NETBSD(),        ?SLIB:is_not_netbsd()).
-define(IS_NOT_DARWIN(),        ?SLIB:is_not_darwin()).

-define(MK_UNIQ_PATH(),         ?SLIB:mk_unique_path()).

-endif. % -ifdef(socket_test_lib_hrl).
