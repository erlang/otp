%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

-export([suite/0,
         all/0,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         start_server/1,
         up/1,
         down/1,
         cea_timeout/1,
         stop/1]).

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
         {'Acct-Application-Id', [D:id() || D <- Dicts]}
         | [{application, [{dictionary, D},
                           {module, #diameter_callback{}}]}
            || D <- Dicts]]).

%% Diameter Result-Code's:
-define(NO_COMMON_APP, 5010).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     start_server,
     up,
     down,
     cea_timeout,
     stop].

init_per_testcase(Name, Config) ->
    [{name, Name} | Config].

end_per_testcase(_, _) ->
    ok.

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_server(Config) ->
    diameter:subscribe(?SERVER),
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER, [?DICT_COMMON])),
    LRef = ?util:listen(?SERVER, tcp, [{capabilities_cb, fun capx_cb/2},
                                       {capx_timeout, ?SERVER_CAPX_TMO}]),
    [PortNr] = ?util:lport(tcp, LRef),
    ?util:write_priv(Config, portnr, PortNr),
    start = event(?SERVER).

%% Connect with matching capabilities and expect the connection to
%% come up.
up(Config) ->
    {Svc, Ref} = connect(Config, [{connect_timer, 5000},
                                  {watchdog_timer, 15000}]),
    start = event(Svc),
    {up, Ref, {TPid, Caps}, Cfg, #diameter_packet{}} = event(Svc),
    {watchdog, Ref, _, {initial, okay}, _} = event(Svc),
    %% Kill the transport process and see that the connection is
    %% reestablished after a watchdog timeout, not after connect_timer
    %% expiry.
    exit(TPid, kill),
    {down, Ref, {TPid, Caps}, Cfg} = event(Svc),
    {watchdog, Ref, _, {okay, down}, _} = event(Svc),
    {reconnect, Ref, _} = event(Svc, 10000, 20000).

%% Connect with non-matching capabilities and expect CEA from the peer
%% to indicate as much and then for the transport to be restarted
%% (after connect_timer).
down(Config) ->
    {Svc, Ref} = connect(Config, [{capabilities, [{'Acct-Application-Id',
                                                   [?DICT_ACCT:id()]}]},
                                  {applications, [?DICT_ACCT]},
                                  {connect_timer, 5000},
                                  {watchdog_timer, 20000}]),
    start = event(Svc),
    {closed, Ref, {'CEA', ?NO_COMMON_APP, _, #diameter_packet{}}, _}
        = event(Svc),
    {reconnect, Ref, _} = event(Svc, 4000, 10000).

%% Connect with matching capabilities but have the server delay its
%% CEA and cause the client to timeout.
cea_timeout(Config) ->
    {Svc, Ref} = connect(Config, [{capx_timeout, ?SERVER_CAPX_TMO div 2},
                                  {connect_timer, 2*?SERVER_CAPX_TMO}]),
    start = event(Svc),
    {closed, Ref, {'CEA', timeout}, _} = event(Svc).

stop(_Config) ->
    ok = diameter:stop().

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
    {ok, Ref} = diameter:add_transport(Name, opts(Config, Opts)),
    {Name, Ref}.

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
    PortNr = ?util:read_priv(Config, portnr),

    {connect, [{transport_module, diameter_tcp},
               {transport_config, [{ip, ?ADDR}, {port, 0},
                                   {raddr, ?ADDR}, {rport, PortNr}]}
               | Opts]}.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).
