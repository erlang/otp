%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
%% Tests of events sent as a consequence of diameter:subscribe/1.
%% Watchdog events are dealt with more extensively in the watchdog
%% suite.
%%

-module(diameter_event_SUITE).

%% testcase, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([suite/0,
         all/0,
         traffic/1,
         flooding/1]).

%% internal
-export([up/1,
         down/1,
         cea_timeout/1]).

-include("diameter.hrl").


%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).
-define(REALM, "REALM").

-define(SERVER, "SERVER.SERVER-REALM").
-define(CLIENT, "CLIENT.CLIENT-REALM").

-define(DICT_COMMON, ?DIAMETER_DICT_COMMON).
-define(DICT_ACCT,   ?DIAMETER_DICT_ACCOUNTING).

-define(SERVER_CAPX_TMO, 6000).

%% Config for diameter:start_service/2.
-define(SERVICE(Host, Dicts),
        [{'Origin-Host', Host},
         {'Origin-Realm', realm(Host)},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Acct-Application-Id', [D:id() || D <- Dicts]},
         {decode_format, map}
         | [{application, [{dictionary, D},
                           {module, #diameter_callback{}}]}
            || D <- Dicts]]).

%% Diameter Result-Code's:
-define(NO_COMMON_APP, 5010).

-define(P(F), ?P(F, [])).
-define(P(F, A),
        io:format("~s "
                  "~p(~w) " ++ F ++ "~n",
                  [diameter_lib:formated_timestamp(),
                   ?FUNCTION_NAME, ?LINE | A])).

               
%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 180}}].

all() ->
    [traffic, flooding].

traffic(_Config) ->
    run().

%% Start one event collector process,
%% then start 
flooding(_Config) ->
    ok = diameter:start(),
    try
        ?util:run([{fun flooding/0, 120000}])
    after
        ok = diameter:stop()
    end.


flooding() ->
    Services  =
        [#{type         => server,
           name         => ?SERVER,
           service_opts => ?SERVICE(?SERVER, [?DICT_COMMON]),
           transport    => #{prot      => tcp,
                             operation => listen,
                             opts      => [{capabilities_cb, fun capx_cb/2},
                                           {capx_timeout, ?SERVER_CAPX_TMO}]}},
         #{type         => client,
           name         => ?CLIENT,
           service_opts => ?SERVICE(?CLIENT, [?DICT_COMMON, ?DICT_ACCT]),
           transport    => #{prot      => tcp,
                             operation => connect,
                             opts      => [{strict_mbit,    false},
                                           {connect_timer,  5000},
                                           {watchdog_timer, 15000},
                                           {transport_module, diameter_tcp},
                                           {transport_config, [{ip,    ?ADDR},
                                                               {port,  0},
                                                               {raddr, ?ADDR}]}]}}],
    Collector = flooding_collector_start([SvcName || #{name := SvcName} <-
                                                         Services]),
    flooding_start_and_stop_services(Services),
    flooding_collector_stop(Collector),
    ok.

%% -----

flooding_start_and_stop_services(Services) ->
    ?P("start start/stop test", []),
    flooding_start_and_stop_services(Services, 100).

flooding_start_and_stop_services(_Services, N) when (N =< 0) ->
    ok;
flooding_start_and_stop_services(Services, N) ->
    ?P("start start/stop test (~w remaining)", [N]),
    StartedServices = flooding_start_services(Services, []),
    flooding_stop_services(StartedServices),
    flooding_start_and_stop_services(Services, N-1).

flooding_start_services([], StartedServices) ->
    ?P("all services started", []),
    StartedServices;
flooding_start_services([Service|Services], StartedServices) ->
    #{} = Service2 = flooding_start_service(Service, StartedServices),
    flooding_start_services(Services, [Service2|StartedServices]).

flooding_start_service(#{type         := server,
                         name         := SvcName,
                         service_opts := SvcOpts,
                         transport    := #{prot      := tcp,
                                           operation := listen,
                                           opts      := TOpts} = T} = S, _) ->
    F = fun() ->
                ?P("(try) start (server) service ~p", [SvcName]),
                ok       = diameter:start_service(SvcName, SvcOpts),
                ?P("(try) start and add (listen) transport for service ~p",
                   [SvcName]),
                LRef     = ?util:listen(SvcName, tcp, TOpts),
                [PortNr] = ?util:lport(tcp, LRef),
                ?P("transport listen port: ~w", [PortNr]),
                T2 = T#{operation => {listen, PortNr}},
                exit(S#{transport => T2})
        end,
    {Pid, MRef} = erlang:spawn_monitor(F),
    receive
        {'DOWN', MRef, process, Pid, Info} when is_map(Info) ->
            ?P("server service started"),
            Info;
        {'DOWN', MRef, process, Pid, Info} ->
            ?P("server service start failed: "
               "~n   ~p", [Info]),
            exit(server_start_failed)
    end;
flooding_start_service(#{type         := client,
                         name         := SvcName,
                         service_opts := SvcOpts,
                         transport    := #{prot      := tcp,
                                           operation := connect,
                                           opts      := TOpts}} = S,
                       StartedServices) ->
    F = fun() ->
                ?P("(try) start (client) service ~p", [SvcName]),
                ok       = diameter:start_service(SvcName, SvcOpts),
                ?P("(try) start and add transport for service ~p", [SvcName]),
                {ok, PortNr} =
                    flooding_start_service_listen_portno(StartedServices),
                {value, {transport_config, TConf}} =
                    lists:keysearch(transport_config, 1, TOpts),
                TConf2 = [{rport, PortNr} | TConf],
                TOpts2 = lists:keyreplace(transport_config, 1, TOpts,
                                          {transport_config, TConf2}),
                ?P("(try) start and add transport for service ~p", [SvcName]),
                {ok, _Ref} = diameter:add_transport(SvcName, {connect, TOpts2}),
                exit(S)
        end,
    {Pid, MRef} = erlang:spawn_monitor(F),
    receive
        {'DOWN', MRef, process, Pid, Info} when is_map(Info) ->
            ?P("client service started"),
            Info;
        {'DOWN', MRef, process, Pid, Info} ->
            ?P("client service start failed: "
               "~n   ~p", [Info]),
            exit(client_start_failed)
    end.    


flooding_start_service_listen_portno([]) ->
    {error, not_found};
flooding_start_service_listen_portno(
  [#{type      := server,
     transport := #{operation := {listen, PortNr}}} | _]) ->
    {ok, PortNr};
flooding_start_service_listen_portno([_|StartedServices]) ->
    flooding_start_service_listen_portno(StartedServices).


flooding_stop_services([]) ->
    ?P("all services stopped", []),
    ok;
flooding_stop_services([#{name := SvcName}|Services]) ->
    ?P("(try) stop service ~p", [SvcName]),
    ok = diameter:stop_service(SvcName),
    flooding_stop_services(Services).


%% -----

flooding_collector_start(Services) ->
    Parent = self(),
    spawn_link(fun() -> flooding_collector_init(Parent, Services) end).

flooding_collector_stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {stop, self()},
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    end.

flooding_collector_init(Parent, Services) ->
    lists:foreach(fun(SvcName) -> diameter:subscribe(SvcName) end, Services),
    flooding_collector_loop(Parent, [{SvcName, []} || SvcName <- Services]).

flooding_collector_loop(Parent, Services) ->
    receive
        {stop, Parent} ->
            %% We do not need to unsubscribe
            %% Diameter handles this automatically
            %% Also, this means the test is done anyway...
            ?P("stop command from parent when service event counts are: "
               "~n   ~p", [Services]),
            exit(normal);

        #diameter_event{service = Name, info = Event} ->
            case lists:keysearch(Name, 1, Services) of
                {value, {Name, EvInfo}} ->
                    Ev = flooding_which_event(Event),
                    case lists:keysearch(Ev, 1, EvInfo) of
                        {value, {Ev, Cnt}} ->
                            NewCnt = Cnt+1,
                            ?P("received '~p' event ~w from known service ~p",
                               [Ev, NewCnt, Name]),
                            EvInfo2   = lists:keyreplace(Ev, 1, EvInfo,
                                                         {Ev, NewCnt}),
                            Services2 = lists:keyreplace(Name, 1, Services,
                                                         {Name, EvInfo2}),
                            flooding_collector_loop(Parent, Services2);
                        false ->
                            ?P("received first '~p' event from known service ~p",
                               [Ev, Name]),
                            EvInfo2   = [{Ev, 1}|EvInfo],
                            Services2 = lists:keyreplace(Name, 1, Services,
                                                         {Name, EvInfo2}),
                            flooding_collector_loop(Parent, Services2)
                    end;
                false ->
                    ?P("received ~p event from UNKNOWN service ~p",
                       [Event, Name]),
                    flooding_collector_loop(Parent, Services)
            end
    end.

flooding_which_event(Event) when is_atom(Event) ->
    Event;
flooding_which_event({closed = Event, _, _, _}) ->
    Event;
flooding_which_event({up = Event, _, _, _, _}) ->
    Event;
flooding_which_event({down = Event, _, _, _}) ->
    Event;
flooding_which_event({watchdog, _, _, {initial, okay}, _}) ->
    wd_init_okay;
flooding_which_event({watchdog, _, _, {initial, reopen}, _}) ->
    wd_init_reopen;
flooding_which_event({watchdog, _, _, {okay, down}, _}) ->
    wd_okay_down;
flooding_which_event({watchdog, _, _, {From, To}, _}) ->
    ?P("Unknown watchdog event: "
       "~n   From: ~p"
       "~n   To:   ~p", [From, To]),
    wd;
flooding_which_event({reconnect = Event, _, _}) ->
    Event;
flooding_which_event(Event) when is_tuple(Event) ->
    ?P("Unknown event: "
       "~n   ~p", [Event]),
    element(1, Event);
flooding_which_event(Unknown) ->
    ?P("Unknown event: "
       "~n   ~p", [Unknown]),
    exit({tuple_size, size(Unknown)}).



%% ===========================================================================

%% run/0

run() ->
    ok = diameter:start(),
    try
        ?util:run([{fun traffic/0, 60000}])
    after
        ok = diameter:stop()
    end.

%% traffic/0

traffic() ->
    PortNr = start_server(),
    Config = [{portnr, PortNr}],
    Funs = [up, down, cea_timeout],
    ?util:run([[{?MODULE, F, [[{name, F} | Config]]} || F <- Funs]]).

%% start_server/0

start_server() ->
    diameter:subscribe(?SERVER),
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER, [?DICT_COMMON])),
    LRef = ?util:listen(?SERVER, tcp, [{capabilities_cb, fun capx_cb/2},
                                       {capx_timeout, ?SERVER_CAPX_TMO}]),
    [PortNr] = ?util:lport(tcp, LRef),
    start = event(?SERVER),
    PortNr.

%% Connect with matching capabilities and expect the connection to
%% come up.
up(Config) ->
    {Svc, Ref, T} = connect(Config, [{strict_mbit, false},
                                     {connect_timer, 5000},
                                     {watchdog_timer, 15000}]),
    start = event(Svc),
    {{up, Ref, {TPid, Caps}, T, #diameter_packet{msg = M}}, _}
        = {event(Svc), T},
    ['CEA' | #{}] = M,  %% assert
    {watchdog, Ref, _, {initial, okay}, _} = event(Svc),
    %% Kill the transport process and see that the connection is
    %% reestablished after a watchdog timeout, not after connect_timer
    %% expiry.
    exit(TPid, kill),
    {{down, Ref, {TPid, Caps}, T}, _} = {event(Svc), T},
    {watchdog, Ref, _, {okay, down}, _} = event(Svc),
    {reconnect, Ref, _} = event(Svc, 10000, 20000).

%% Connect with non-matching capabilities and expect CEA from the peer
%% to indicate as much and then for the transport to be restarted
%% (after connect_timer).
down(Config) ->
    {Svc, Ref, T} = connect(Config, [{capabilities, [{'Acct-Application-Id',
                                                      [?DICT_ACCT:id()]}]},
                                     {applications, [?DICT_ACCT]},
                                     {connect_timer, 5000},
                                     {watchdog_timer, 20000}]),
    start = event(Svc),
    {{closed, Ref, {'CEA', ?NO_COMMON_APP, _, #diameter_packet{msg = M}}, T},
     _}
        = {event(Svc), T},
    ['CEA' | #{}] = M,  %% assert
    {reconnect, Ref, _} = event(Svc, 4000, 10000).

%% Connect with matching capabilities but have the server delay its
%% CEA and cause the client to timeout.
cea_timeout(Config) ->
    {Svc, Ref, T} = connect(Config, [{capx_timeout, ?SERVER_CAPX_TMO div 2},
                                     {connect_timer, 2*?SERVER_CAPX_TMO}]),
    start = event(Svc),
    {{closed, Ref, {'CEA', timeout}, T}, _} = {event(Svc), T}.

%% ----------------------------------------

%% Keep the server from sending CEA until the client has timed out.
capx_cb(_, #diameter_caps{origin_host = {_, "cea_timeout-" ++ _}}) ->
    receive after ?SERVER_CAPX_TMO -> ok end;

%% Or not.
capx_cb(_, _Caps) ->
    ok.

%% ----------------------------------------

%% Use the testcase name to construct Origin-Host of the client so
%% that the server can match on it in capx_cb/2.
connect(Config, Opts) ->
    Pre = atom_to_list(proplists:get_value(name, Config)),
    Name = Pre ++ uniq() ++ ?CLIENT,
    diameter:subscribe(Name),
    ok = start_service(Name, ?SERVICE(Name, [?DICT_COMMON, ?DICT_ACCT])),
    {connect, _} = T = opts(Config, Opts),
    {ok, Ref} = diameter:add_transport(Name, T),
    {Name, Ref, T}.

uniq() ->
    "-" ++ diameter_util:unique_string().

event(Name) ->
    receive #diameter_event{service = Name, info = T} -> T end.

event(Name, TL, TH) ->
    T0 = diameter_lib:now(),
    Event = event(Name),
    DT = diameter_lib:micro_diff(T0) div 1000,
    {true, true, DT, Event} = {TL < DT, DT < TH, DT, Event},
    Event.

start_service(Name, Opts) ->
    diameter:start_service(Name, [{monitor, self()} | Opts]).

opts(Config, Opts) ->
    PortNr = proplists:get_value(portnr, Config),

    {connect, [{transport_module, diameter_tcp},
               {transport_config, [{ip, ?ADDR}, {port, 0},
                                   {raddr, ?ADDR}, {rport, PortNr}]}
               | Opts]}.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).
