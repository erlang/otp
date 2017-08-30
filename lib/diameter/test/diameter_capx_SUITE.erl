%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

%%
%% Tests of capabilities exchange between Diameter nodes. In
%% particular, of error and event handling.
%%

-module(diameter_capx_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         vendor_id/1,
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
%% Use only the Vendor-Specific-Application-Id record from the base
%% include, to test the independence of capabilities configuration
%% from the different definitions of Vendor-Id in RFC's 3588 and RFC
%% 6733.

%% ===========================================================================

-define(util, diameter_util).

-define(CLIENT, client).
-define(SERVER, server).

-define(ADDR, {127,0,0,1}).

-define(REALM, "erlang.org").
-define(HOST(Name), Name ++ "." ++ ?REALM).

%% Application id's that are never agreed upon at capabilities
%% exchange. Testcase no_common_application references them in order
%% to exercise Vendor-Specific-Application-Id handling.
-define(NOAPPS, [1111, 2222, 3333, 4444]).

%% Config for diameter:start_service/2.
-define(SERVICE,
        [{'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {'Acct-Application-Id', [?DIAMETER_APP_ID_ACCOUNTING]}
         | [{application, [{alias, A},
                           {dictionary, D},
                           {module, [?MODULE, A]}]}
            || {A,D} <- [{base3588, diameter_gen_base_rfc3588},
                         {acct3588, diameter_gen_base_accounting},
                         {base6733, diameter_gen_base_rfc6733},
                         {acct6733, diameter_gen_acct_rfc6733}]]]
        ++ [{application, [{dictionary, dict(N)},
                           {module, not_really}]}
            || N <- ?NOAPPS]).

-define(A, list_to_atom).
-define(L, atom_to_list).

-define(event,  #diameter_event).
-define(caps,   #diameter_caps).
-define(packet, #diameter_packet).

-define(fail(T), erlang:error({T, process_info(self(), messages)})).

-define(TIMEOUT, 10000).

-define(DICTS, [rfc3588, rfc6733]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() -> [start,
          vendor_id,
          start_services,
          add_listeners]
      ++ [{group, D, P} || D <- ?DICTS, P <- [[], [parallel]]]
      ++ [remove_listeners,
          stop_services,
          stop].

groups() ->
    Tc = lists:flatmap(fun tc/1, tc()),
    [{D, [], Tc} || D <- ?DICTS].

init_per_suite(Config) ->
    lists:foreach(fun load_dict/1, ?NOAPPS),
    Config.

end_per_suite(_Config) ->
    [] = [Mod || N <- ?NOAPPS,
                 Mod <- [dict(N)],
                 false <- [code:delete(Mod)]],
    ok.

%% Generate a unique hostname for each testcase so that watchdogs
%% don't prevent a connection from being brought up immediately.
init_per_testcase(Name, Config) ->
    [{host, ?L(Name) ++ "." ++ diameter_util:unique_string()}
     | Config].

init_per_group(Name, Config) ->
    [{rfc, Name} | Config].

end_per_group(_, _) ->
    ok.

end_per_testcase(N, _)
  when N == start;
       N == vendor_id;
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

%% Ensure that both integer and list-valued vendor id's can be
%% configured in a Vendor-Specific-Application-Id, the arity having
%% changed between RFC 3588 and RFC 6733.
vendor_id(_Config) ->
    [] = ?util:run([[fun vid/1, V] || V <- [1, [1], [1,2], x]]).

vid(V) ->
    RC = diameter:start_service(make_ref(),
                                [{'Vendor-Specific-Application-Id',
                                  [[{'Vendor-Id', V}]]}
                                 | ?SERVICE]),
    vid(V, RC).

vid(x, {error, _}) ->
    ok;
vid(_, ok) ->
    ok.

start_services(_Config) ->
    ok = diameter:start_service(?SERVER, ?SERVICE),
    ok = diameter:start_service(?CLIENT, ?SERVICE).

%% One server that responds only to base accounting, one that responds
%% to both this and the common application. Share a common service just
%% to simplify config, and because we can.
add_listeners(Config) ->
    Acct = [listen(?SERVER,
                   [{capabilities, [{'Origin-Host', ?HOST(H)},
                                    {'Auth-Application-Id', []}]},
                    {applications, [A | noapps()]},
                    {capabilities_cb, [fun server_capx/3, acct]}])
            || {A,H} <- [{acct3588, "acct3588-srv"},
                         {acct6733, "acct6733-srv"}]],
    Base = [listen(?SERVER,
                   [{capabilities, [{'Origin-Host', ?HOST(H)}]},
                    {applications, A ++ noapps()},
                    {capabilities_cb, [fun server_capx/3, base]}])
            || {A,H} <- [{[base3588, acct3588], "base3588-srv"},
                         {[base6733, acct6733], "base6733-srv"}]],
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
    Vs = [[{'Vendor-Id', 111},
           {'Auth-Application-Id', [1111]}],
          #'diameter_base_Vendor-Specific-Application-Id'
           {'Vendor-Id' = [222],
            'Acct-Application-Id' = [2222]}],
    server_closed(Config,
                  fun(C) -> no_common_application(C,Vs) end,
                  5010).

c_no_common_application(Config) ->
    Vs = [#'diameter_base_Vendor-Specific-Application-Id'
          {'Vendor-Id' = 333,
           'Auth-Application-Id' = [3333]},
          [{'Vendor-Id', [444]},
           {'Acct-Application-Id', [4444]}]],
    client_closed(Config,
                  "acct-srv",
                  fun(C) -> no_common_application(C,Vs) end,
                  5010).

no_common_application(Config, Vs) ->
    [Common, _Acct] = apps(Config),
    connect(Config,
            acct,
            [{capabilities, [{'Acct-Application-Id', []},
                             {'Vendor-Specific-Application-Id', Vs}]},
             {applications, [Common | noapps()]}]).

%% ====================
%% Ask the base server to speak accounting with an unknown security
%% method and expect DIAMETER_NO_COMMON_SECURITY = 5017.

s_no_common_security(Config) ->
    server_closed(Config, fun no_common_security/1, 5017).

c_no_common_security(Config) ->
    client_closed(Config, "base-srv", fun no_common_security/1, 5017).

no_common_security(Config) ->
    [Common, _Acct] = apps(Config),
    connect(Config, base, [{capabilities, [{'Acct-Application-Id', []},
                                           {'Inband-Security-Id', [17, 18]}]},
                           {applications, [Common]}]).

%% ====================
%% Have the base server reject a decent CER with the protocol error
%% DIAMETER_UNKNOWN_PEER = 3010.

s_unknown_peer(Config) ->
    server_reject(Config, fun base/1, 3010).

c_unknown_peer(Config) ->
    Dict0 = dict0(Config),
    true = diameter:subscribe(?CLIENT),
    OH = host(Config, "base-srv"),

    {CRef, _} = base(Config),

    {'CEA', ?caps{}, ?packet{msg = Msg}} = client_recv(CRef),

    ['diameter_base_answer-message' | _] = Dict0:'#get-'(Msg),
    [OH, 3010] = Dict0:'#get-'(['Origin-Host', 'Result-Code'], Msg).

base(Config) ->
    connect(Config, base, [{applications, apps(Config)}]).

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
    Dict0 = dict0(Config),
    true = diameter:subscribe(?CLIENT),
    OH = host(Config, "acct-srv"),

    {CRef, _} = client_reject(Config),

    {'CEA', {capabilities_cb, _, discard},
            ?caps{origin_host = {_, OH}},
            ?packet{msg = CEA}}
        = client_recv(CRef),

    [diameter_base_CEA | _] = Dict0:'#get-'(CEA),
    [2001] = Dict0:'#get-'(['Result-Code'], CEA).

client_reject(Config) ->
    connect(Config, acct, [{capabilities_cb, fun client_capx/2},
                           {applications, apps(Config)}]).

%% ===========================================================================

noapps() ->
    lists:map(fun dict/1, ?NOAPPS).

dict(N) ->
    ?A(?L(?MODULE) ++ "_" ++ integer_to_list(N)).

%% Compile and load minimal dictionary modules. These actually have to
%% exists since diameter will call their id/0 to extract application
%% id's, failing with app_not_configured if it can't.
load_dict(N) ->
    Mod = dict(N),
    A1 = erl_anno:new(1),
    A2 = erl_anno:new(2),
    A3 = erl_anno:new(3),
    A4 = erl_anno:new(4),
    Forms = [{attribute, A1, module, Mod},
             {attribute, A2, export, [{id,0}]},
             {function, A3, id, 0,
              [{clause, A4, [], [], [{integer, A4, N}]}]}],
    {ok, Mod, Bin, []} = compile:forms(Forms, [return]),
    {module, Mod} = code:load_binary(Mod, Mod, Bin),
    N = Mod:id().

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

%% client_closed/4

client_closed(Config, Host, F, RC) ->
    true = diameter:subscribe(?CLIENT),
    OH = host(Config, Host),

    {CRef, _} = F(Config),

    {'CEA', RC, ?caps{origin_host = {_, OH}}, ?packet{}}
        = client_recv(CRef).

srv(Config, Host) ->
    "rfc" ++ N = atom_to_list(proplists:get_value(rfc, Config)),
    [H, "srv" = S] = string:tokens(Host, "-"),
    H ++ N ++ "-" ++ S.

host(Config, Name) ->
    ?HOST(srv(Config, Name)).

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

dict0(Config) ->
    case proplists:get_value(rfc, Config) of
        rfc3588 -> diameter_gen_base_rfc3588;
        rfc6733 -> diameter_gen_base_rfc6733
    end.

apps(Config) ->
    case proplists:get_value(rfc, Config) of
        rfc3588 -> [base3588, acct3588];
        rfc6733 -> [base6733, acct6733]
    end.

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
    [PortNr] = ?util:lport(tcp, LRef),
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

lref(rfc3588, [LRef, _]) ->
    LRef;
lref(rfc6733, [_, LRef]) ->
    LRef;

lref(Config, T) ->
    lref(proplists:get_value(rfc, Config),
         case ?util:read_priv(Config, ?MODULE) of
             {R, _} when T == base ->
                 R;
             {_, R} when T == acct ->
                 R
         end).

%% ===========================================================================
%% diameter callbacks

peer_up(?SERVER,
        {_, ?caps{origin_host = {"acct" ++ _,
                                 [_,$_|"client_reject." ++ _]}}},
        State,
        _) ->
    State.

peer_down(?SERVER,
          {_, ?caps{origin_host = {"acct" ++ _,
                                   [_,$_|"client_reject." ++ _]}}},
          State,
          _) ->
    State.
