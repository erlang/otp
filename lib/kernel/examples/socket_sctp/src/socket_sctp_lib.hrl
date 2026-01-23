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

-ifndef(SOCKET_SCTP_LIB_HRL_).

-define(SOCKET_SCTP_LIB_HRL__, true).

-define(LIB, socket_sctp_lib).

-define(CATCH_AND_IGNORE(EXPR),
        try EXPR
        catch _:_:_ ->
                ok
        end).
-define(CATCH_AND_RETURN(EXPR),
        try EXPR
        catch __C:__E ->
                {error, {__C, __E}}
        end).

-define(WHICH_DOMAIN(IP), ?LIB:which_domain((IP))).
-define(WHICH_ADDR(D),    ?LIB:which_local_addr((D))).

-define(SCTP_EVENTS(DATA_IO),
        #{data_io          => (DATA_IO),
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

-define(WHICH_STATUS(Sock, AID),
        fun() ->
                case socket:getopt((Sock),
                                   ?MK_SCTP_SOCKOPT(status),
                                   #{assoc_id => (AID)}) of
                    {ok,    Status} -> Status;
                    {error, closed} -> closed;
                    {error, _}      -> undefined
                end
        end()).

-define(STOP(),           ?LIB:stop()).

-define(MQ(),             ?LIB:mq(self())).
-define(MQ(P),            ?LIB:mq((P))).

-define(F(FORMAT, ARGS),  ?LIB:f((FORMAT), (ARGS))).
-define(IS_STR(STR),      ?LIB:is_string((STR))).


-define(DEBUG(F),         ?DEBUG(F, [])).
-define(DEBUG(F, A),      ?LIB:dmsg((F), (A))).
-define(INFO(F),          ?INFO(F, [])).
-define(INFO(F, A),       ?LIB:imsg((F), (A))).
-define(WARNING(F),       ?WARNING(F, [])).
-define(WARNING(F, A),    ?LIB:wmsg((F), (A))).
-define(ERROR(F),         ?ERROR(F, [])).
-define(ERROR(F, A),      ?LIB:emsg((F), (A))).
-define(SLEEP(T),
        receive after trunc(T) -> ok end).

-endif.
