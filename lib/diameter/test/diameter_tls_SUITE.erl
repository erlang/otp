%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% Tests of traffic between six Diameter nodes connected as follows.
%%
%%                  ---- SERVER.REALM1  (TLS after capabilities exchange)
%%                /
%%               /  ---- SERVER.REALM2  (ditto)
%%              | /
%%   CLIENT.REALM0 ----- SERVER.REALM3  (no security)
%%              | \
%%               \  ---- SERVER.REALM4  (TLS at connection establishment)
%%                \
%%                  ---- SERVER.REALM5  (ditto)
%%

-module(diameter_tls_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([start_ssl/1,
         start_diameter/1,
         make_certs/1, make_certs/0,
         start_services/1,
         add_transports/1,
         send1/1,
         send2/1,
         send3/1,
         send4/1,
         send5/1,
         remove_transports/1,
         stop_services/1,
         stop_diameter/1,
         stop_ssl/1]).

%% diameter callbacks
-export([prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT,  "CLIENT.REALM0").
-define(SERVER1, "SERVER.REALM1").
-define(SERVER2, "SERVER.REALM2").
-define(SERVER3, "SERVER.REALM3").
-define(SERVER4, "SERVER.REALM4").
-define(SERVER5, "SERVER.REALM5").

-define(SERVERS, [?SERVER1, ?SERVER2, ?SERVER3, ?SERVER4, ?SERVER5]).

-define(DICT_COMMON,  ?DIAMETER_DICT_COMMON).

-define(APP_ALIAS, the_app).
-define(APP_ID, ?DICT_COMMON:id()).

-define(NO_INBAND_SECURITY, 0).
-define(TLS, 1).

%% Config for diameter:start_service/2.
-define(SERVICE(Host, Dict),
        [{'Origin-Host', Host},
         {'Origin-Realm', realm(Host)},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Inband-Security-Id', [?NO_INBAND_SECURITY]},
         {'Auth-Application-Id', [Dict:id()]},
         {application, [{alias, ?APP_ALIAS},
                        {dictionary, Dict},
                        {module, #diameter_callback{peer_up = false,
                                                    peer_down = false,
                                                    pick_peer = false,
                                                    handle_error = false,
                                                    default = ?MODULE}},
                        {answer_errors, callback}]}]).

%% Config for diameter:add_transport/2. In the listening case, listen
%% on a free port that we then lookup using the implementation detail
%% that diameter_tcp registers the port with diameter_reg.
-define(CONNECT(PortNr, Caps, Opts),
        {connect, [{transport_module, diameter_tcp},
                   {transport_config, [{raddr, ?ADDR},
                                       {rport, PortNr},
                                       {ip, ?ADDR},
                                       {port, 0}
                                       | Opts]},
                   {capabilities, Caps}]}).
-define(LISTEN(Caps, Opts),
        {listen, [{transport_module, diameter_tcp},
                  {transport_config, [{ip, ?ADDR}, {port, 0} | Opts]},
                  {capabilities, Caps}]}).

-define(SUCCESS, 2001).
-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start_ssl,
     start_diameter,
     make_certs,
     start_services,
     add_transports,
     {group, all},
     {group, all, [parallel]},
     remove_transports,
     stop_services,
     stop_diameter,
     stop_ssl].

groups() ->
    [{all, [], tc()}].

%% Shouldn't really have to know about crypto here but 'ok' from
%% ssl:start() isn't enough to guarantee that TLS is available.
init_per_suite(Config) ->
    try
        false /= os:find_executable("openssl")
            orelse throw({?MODULE, no_openssl}),
        ok == (catch crypto:start())
            orelse throw({?MODULE, no_crypto}),
        Config
    catch
        {?MODULE, E} ->
            {skip, E}
    end.

end_per_suite(_Config) ->
    crypto:stop().

%% Testcases to run when services are started and connections
%% established.
tc() ->
    [send1,
     send2,
     send3,
     send4,
     send5].

%% ===========================================================================
%% testcases

start_ssl(_Config) ->
    ok = ssl:start().

start_diameter(_Config) ->
    ok = diameter:start().

make_certs() ->
    [{timetrap, {minutes, 2}}].

make_certs(Config) ->
    Dir = proplists:get_value(priv_dir, Config),

    [] = ?util:run([[fun make_cert/2, Dir, B] || B <- ["server1",
                                                       "server2",
                                                       "server4",
                                                       "server5",
                                                       "client"]]).

start_services(Config) ->
    Dir = proplists:get_value(priv_dir, Config),
    Servers = [server(S, sopts(S, Dir)) || S <- ?SERVERS],

    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),

    {save_config, [Dir | Servers]}.

add_transports(Config) ->
    {_, [Dir | Servers]} = proplists:get_value(saved_config, Config),

    true = diameter:subscribe(?CLIENT),

    Opts = ssl_options(Dir, "client"),
    Connections = [connect(?CLIENT, S, copts(N, Opts))
                   || {S,N} <- lists:zip(Servers, ?SERVERS)],

    ?util:write_priv(Config, "cfg", lists:zip(Servers, Connections)).


%% Remove the client transports and expect the corresponding server
%% transport to go down.
remove_transports(Config) ->
    Ts = ?util:read_priv(Config, "cfg"),
    [] = [T || S <- ?SERVERS, T <- [diameter:subscribe(S)], T /= true],
    lists:map(fun disconnect/1, Ts).

stop_services(_Config) ->
    [] = [{H,T} || H <- [?CLIENT | ?SERVERS],
                   T <- [diameter:stop_service(H)],
                   T /= ok].

stop_diameter(_Config) ->
    ok = diameter:stop().

stop_ssl(_Config) ->
    ok = ssl:stop().

%% Send an STR intended for a specific server and expect success.
send1(_Config) ->
    call(?SERVER1).
send2(_Config) ->
    call(?SERVER2).
send3(_Config) ->
    call(?SERVER3).
send4(_Config) ->
    call(?SERVER4).
send5(_Config) ->
    call(?SERVER5).

%% ===========================================================================
%% diameter callbacks

%% prepare_request/3

prepare_request(#diameter_packet{msg = Req},
                ?CLIENT,
                {_Ref, Caps}) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,

    {send, set(Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR}])}.

%% prepare_retransmit/3

prepare_retransmit(_Pkt, false, _Peer) ->
    discard.

%% handle_answer/4

handle_answer(Pkt, _Req, ?CLIENT, _Peer) ->
    #diameter_packet{msg = Rec, errors = []} = Pkt,
    Rec.

%% handle_request/3

handle_request(#diameter_packet{msg = #diameter_base_STR{'Session-Id' = SId}},
               OH,
               {_Ref, #diameter_caps{origin_host = {OH,_},
                                     origin_realm = {OR, _}}})
  when OH /= ?CLIENT ->
    {reply, #diameter_base_STA{'Result-Code' = ?SUCCESS,
                               'Session-Id' = SId,
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR}}.

%% ===========================================================================
%% support functions

call(Server) ->
    Realm = realm(Server),
    Req = ['STR', {'Destination-Realm', Realm},
                  {'Termination-Cause', ?LOGOUT},
                  {'Auth-Application-Id', ?APP_ID}],
    #diameter_base_STA{'Result-Code' = ?SUCCESS,
                       'Origin-Host' = Server,
                       'Origin-Realm' = Realm}
        = call(Req, [{filter, realm}]).

call(Req, Opts) ->
    diameter:call(?CLIENT, ?APP_ALIAS, Req, Opts).

set([H|T], Vs) ->
    [H | Vs ++ T].

disconnect({{LRef, _PortNr}, CRef}) ->
    ok = diameter:remove_transport(?CLIENT, CRef),
    receive #diameter_event{info = {down, LRef, _, _}} -> ok end.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

inband_security(Ids) ->
    [{'Inband-Security-Id', Ids}].

ssl_options(Dir, Base) ->
    Root = filename:join([Dir, Base]),
    [{ssl_options, [{certfile, Root ++ "_ca.pem"},
                    {keyfile,  Root ++ "_key.pem"}]}].

make_cert(Dir, Base) ->
    make_cert(Dir, Base ++ "_key.pem", Base ++ "_ca.pem").

make_cert(Dir, Keyfile, Certfile) ->
    [KP,CP] = [filename:join([Dir, F]) || F <- [Keyfile, Certfile]],

    KC = join(["openssl genrsa -out", KP, "2048"]),
    CC = join(["openssl req -new -x509 -key", KP, "-out", CP, "-days 7",
               "-subj /C=SE/ST=./L=Stockholm/CN=www.erlang.org"]),

    %% Hope for the best and only check that files are written.
    KR = os:cmd(KC),
    {_, {ok, _}} = {KR, file:read_file_info(KP)},
    CR = os:cmd(CC),
    {_, {ok, _}} = {CR, file:read_file_info(CP)},

    {KP,CP}.

join(Strs) ->
    string:join(Strs, " ").

%% server/2

server(Host, {Caps, Opts}) ->
    ok = diameter:start_service(Host, ?SERVICE(Host, ?DICT_COMMON)),
    {ok, LRef} = diameter:add_transport(Host, ?LISTEN(Caps, Opts)),
    {LRef, hd([_] = ?util:lport(tcp, LRef))}.

sopts(?SERVER1, Dir) ->
    {inband_security([?TLS]),
     ssl_options(Dir, "server1")};
sopts(?SERVER2, Dir) ->
    {inband_security([?NO_INBAND_SECURITY, ?TLS]),
     ssl_options(Dir, "server2")};
sopts(?SERVER3, _) ->
    {[], []};
sopts(?SERVER4, Dir) ->
    {[], ssl(ssl_options(Dir, "server4"))};
sopts(?SERVER5, Dir) ->
    {[], ssl(ssl_options(Dir, "server5"))}.

ssl([{ssl_options = T, Opts}]) ->
    [{T, true} | Opts].

%% connect/3

connect(Host, {_LRef, PortNr}, {Caps, Opts}) ->
    {ok, Ref} = diameter:add_transport(Host, ?CONNECT(PortNr, Caps, Opts)),
    receive
        #diameter_event{service = Host,
                        info = {up, Ref, _, _, #diameter_packet{}}} ->
            ok
    end,
    Ref.

copts(S, Opts)
  when S == ?SERVER1;
       S == ?SERVER2;
       S == ?SERVER3 ->
    {inband_security([?NO_INBAND_SECURITY, ?TLS]), Opts};
copts(S, Opts)
  when S == ?SERVER4;
       S == ?SERVER5 ->
    {[], ssl(Opts)}.
