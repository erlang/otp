%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
%% Tests of capabilities exchange between Diameter nodes. In
%% particular, of error and event handling.
%%

-module(diameter_capx_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         start_services/1,
         add_listeners/1,
         s_no_common_application/1,
         c_no_common_application/1,
         s_no_common_security/1,
         c_no_common_security/1,
         s_unknown_peer/1,
         c_unknown_peer/1,
         s_unable/1,
         c_unable/1,
         s_client_reject/1,
         c_client_reject/1,
         remove_listeners/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/4,
         peer_down/4]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(CLIENT, client).
-define(SERVER, server).

-define(ADDR, {127,0,0,1}).

-define(REALM, "erlang.org").
-define(HOST(Name), Name ++ "." ++ ?REALM).

%% Config for diameter:start_service/2.
-define(SERVICE(Name),
        [{'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {'Acct-Application-Id', [?DIAMETER_APP_ID_ACCOUNTING]}
         | [{application, [{alias, A},
                           {dictionary, D},
                           {module, [?MODULE, A]}]}
            || {A,D} <- [{common, ?DIAMETER_DICT_COMMON},
                         {accounting, ?DIAMETER_DICT_ACCOUNTING}]]]).

-define(A, list_to_atom).
-define(L, atom_to_list).

-define(event,  #diameter_event).
-define(caps,   #diameter_caps).
-define(packet, #diameter_packet).

-define(cea,            #diameter_base_CEA).
-define(answer_message, #'diameter_base_answer-message').

-define(fail(T), erlang:error({T, process_info(self(), messages)})).

-define(TIMEOUT, 10000).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() -> [start,
          start_services,
          add_listeners,
          {group, all},
          {group, all, [parallel]},
          remove_listeners,
          stop_services,
          stop].

groups() ->
    [{all, [], lists:flatmap(fun tc/1, tc())}].

%% Generate a unique hostname for each testcase so that watchdogs
%% don't prevent a connection from being brought up immediately.
init_per_testcase(Name, Config) ->
    Uniq = ["." ++ integer_to_list(N) || N <- tuple_to_list(now())],
    [{host, lists:flatten([?L(Name) | Uniq])} | Config].

end_per_testcase(N, _)
  when N == start;
       N == start_services;
       N == add_listeners;
       N == remove_listeners;
       N == stop_services;
       N == stop ->
    ok;
end_per_testcase(Name, Config) ->
    CRef = ?util:read_priv(Config, Name),
    ok = diameter:remove_transport(?CLIENT, CRef).

%% Testcases all come in two flavours, client and server.
tc(Name) ->
    [?A([C,$_|?L(Name)]) || C <- "cs"].

tc() ->
    [no_common_application,
     no_common_security,
     unknown_peer,
     unable,
     client_reject].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER)),
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT)).

%% One server that responds only to base accounting, one that responds
%% to both this and the common application. Share a common service just
%% to simplify config, and because we can.
add_listeners(Config) ->
    Acct = listen(?SERVER,
                  [{capabilities, [{'Origin-Host', ?HOST("acct-srv")},
                                   {'Auth-Application-Id', []}]},
                   {applications, [accounting]},
                   {capabilities_cb, [fun server_capx/3, acct]}]),
    Base = listen(?SERVER,
                  [{capabilities, [{'Origin-Host', ?HOST("base-srv")}]},
                   {capabilities_cb, [fun server_capx/3, base]}]),
    ?util:write_priv(Config, ?MODULE, {Base, Acct}). %% lref/2 reads

remove_listeners(_Config) ->
    ok = diameter:remove_transport(?SERVER, true).

stop_services(_Config) ->
    ok = diameter:stop_service(?CLIENT),
    ok = diameter:stop_service(?SERVER).

stop(_Config) ->
    ok = diameter:stop().

%% ===========================================================================
%% All the testcases come in pairs, one for receiving an event on the
%% client side, one on the server side. Note that testcases will
%% receive events resulting from other testcases when running in
%% parallel since the events are per service. The unique client
%% Origin-Host for each testcase plus transport references are used to
%% ensure that only the relevant event is extracted from the mailbox.
%% Don't bother extracting events that aren't relevant.

%% ====================
%% Ask the accounting server to speak the common application and expect
%% DIAMETER_NO_COMMON_APPLICATION = 5010.

s_no_common_application(Config) ->
    server_closed(Config, fun no_common_application/1, 5010).

c_no_common_application(Config) ->
    client_closed(Config, "acct-srv", fun no_common_application/1, 5010).

no_common_application(Config) ->
    connect(Config, acct, [{capabilities, [{'Acct-Application-Id', []}]},
                           {applications, [common]}]).

%% ====================
%% Ask the base server to speak accounting with an unknown security
%% method and expect DIAMETER_NO_COMMON_SECURITY = 5017.

s_no_common_security(Config) ->
    server_closed(Config, fun no_common_security/1, 5017).

c_no_common_security(Config) ->
    client_closed(Config, "base-srv", fun no_common_security/1, 5017).

no_common_security(Config) ->
    connect(Config, base, [{capabilities, [{'Acct-Application-Id', []},
                                           {'Inband-Security-Id', [17, 18]}]},
                           {applications, [common]}]).

%% ====================
%% Have the base server reject a decent CER with the protocol error
%% DIAMETER_UNKNOWN_PEER = 3010.

s_unknown_peer(Config) ->
    server_reject(Config, fun base/1, 3010).

c_unknown_peer(Config) ->
    true = diameter:subscribe(?CLIENT),
    OH = ?HOST("base-srv"),

    {CRef, _} = base(Config),

    {'CEA', ?caps{},
            ?packet{msg = ?answer_message{'Origin-Host' = OH,
                                          'Result-Code' = 3010}}}
        = client_recv(CRef).

base(Config) ->
    connect(Config, base, []).

%% ====================
%% Have the base server reject a decent CER with the non-protocol
%% error DIAMETER_UNABLE_TO_COMPLY = 5012.

s_unable(Config) ->
    server_reject(Config, fun base/1, 5012).

c_unable(Config) ->
    client_closed(Config, "base-srv", fun base/1, 5012).

%% ====================
%% Have the client reject a decent CEA.

s_client_reject(Config) ->
    true = diameter:subscribe(?SERVER),
    OH = host(Config),

    {_, LRef} = client_reject(Config),

    receive
        ?event{service = ?SERVER,
               info = {up, LRef,
                           {_, ?caps{origin_host = {_, OH}}},
                           {listen, _},
                           ?packet{}}}
                    = Info ->
            Info
    after ?TIMEOUT ->
            ?fail({LRef, OH})
    end.

c_client_reject(Config) ->
    true = diameter:subscribe(?CLIENT),
    OH = ?HOST("acct-srv"),

    {CRef, _} = client_reject(Config),

    {'CEA', {capabilities_cb, _, discard},
            ?caps{origin_host = {_, OH}},
            ?packet{msg = ?cea{'Result-Code' = 2001}}}
        = client_recv(CRef).

client_reject(Config) ->
    connect(Config, acct, [{capabilities_cb, fun client_capx/2}]).

%% ===========================================================================

%% server_closed/3

server_closed(Config, F, RC) ->
    true = diameter:subscribe(?SERVER),
    OH = host(Config),

    {_, LRef} = F(Config),

    receive
        ?event{service = ?SERVER,
               info = {closed, LRef,
                               {'CER', RC,
                                       ?caps{origin_host = {_, OH}},
                                       ?packet{}}
                               = Reason,
                               {listen, _}}} ->
            Reason
    after ?TIMEOUT ->
            ?fail({LRef, OH})
    end.

%% server_reject/3

server_reject(Config, F, RC) ->
    true = diameter:subscribe(?SERVER),
    OH = host(Config),

    {_, LRef} = F(Config),

    receive
        ?event{service = ?SERVER,
               info = {closed, LRef,
                               {'CER', {capabilities_cb, _, RC},
                                       ?caps{origin_host = {_, OH}},
                                       ?packet{}}
                               = Reason,
                               {listen, _}}} ->
            Reason
    after ?TIMEOUT ->
            ?fail({LRef, OH})
    end.

%% cliient_closed/4

client_closed(Config, Host, F, RC) ->
    true = diameter:subscribe(?CLIENT),
    OH = ?HOST(Host),

    {CRef, _} = F(Config),

    {'CEA', RC, ?caps{origin_host = {_, OH}}, ?packet{}}
        = client_recv(CRef).

%% client_recv/1

client_recv(CRef) ->
    receive
        ?event{service = ?CLIENT,
               info = {closed, CRef, Reason, {connect, _}}} ->
            Reason
    after ?TIMEOUT ->
            ?fail(CRef)
    end.

%% server_capx/3

server_capx(_, ?caps{origin_host = {_, [_,$_|"unknown_peer." ++ _]}}, _) ->
    unknown;

server_capx(_, ?caps{origin_host = {_, [_,$_|"unable." ++ _]}}, _) ->
    5012;  %% DIAMETER_UNABLE_TO_COMPLY

server_capx(_, ?caps{origin_host = {OH,DH}}, _) ->
    io:format("connection: ~p -> ~p~n", [DH,OH]),
    ok.

%% client_capx/2

client_capx(_, ?caps{origin_host = {[_,$_|"client_reject." ++ _], _}}) ->
    discard.

%% ===========================================================================

host(Config) ->
    {_, H} = lists:keyfind(host, 1, Config),
    ?HOST(H).

listen(Name, Opts) ->
    ?util:listen(Name, tcp, Opts).

connect(Config, T, Opts) ->
    {_, H} = lists:keyfind(host, 1, Config),
    LRef = lref(Config, T),
    CRef = connect(LRef, [{capabilities, [{'Origin-Host', ?HOST(H)}]}
                          | Opts]),
    Name = lists:takewhile(fun(C) -> C /= $. end, H),
    ?util:write_priv(Config, Name, CRef),  %% end_per_testcase reads
    {CRef, LRef}.

connect(LRef, Opts) ->
    [PortNr] = ?util:lport(tcp, LRef, 20),
    {ok, CRef} = diameter:add_transport(?CLIENT,
                                        {connect, opts(PortNr, Opts)}),
    CRef.

opts(PortNr, Opts) ->
    [{transport_module, diameter_tcp},
     {transport_config, [{raddr, ?ADDR},
                         {rport, PortNr},
                         {ip, ?ADDR},
                         {port, 0}]}
     | Opts].

lref(Config, T) ->
    case ?util:read_priv(Config, ?MODULE) of
        {LRef, _} when T == base ->
            LRef;
        {_, LRef} when T == acct ->
            LRef
    end.

%% ===========================================================================
%% diameter callbacks

peer_up(?SERVER,
        {_, ?caps{origin_host = {"acct-srv." ++ _,
                                 [_,$_|"client_reject." ++ _]}}},
        State,
        _) ->
    State.

peer_down(?SERVER,
          {_, ?caps{origin_host = {"acct-srv." ++ _,
                                   [_,$_|"client_reject." ++ _]}}},
          State,
          _) ->
    State.
