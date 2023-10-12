%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

%% testcases, no common_test dependency
-export([run/0]).

%% common_test wrapping
-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         parallel/1]).

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
%% common_test wrapping

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [parallel].

%% Shouldn't really have to know about crypto here but 'ok' from
%% ssl:start() isn't enough to guarantee that TLS is available.
init_per_suite(Config) ->
    try
        [] == (catch make_certs(dir(Config)))
            orelse throw({?MODULE, no_certs}),
        ok == crypto:start() orelse throw({?MODULE, no_crypto}),
        ok == ssl:start() orelse throw({?MODULE, no_ssl}),
        Config
    catch
        {?MODULE, E} ->
            {skip, E}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    crypto:stop().

parallel(Config) ->
    run(dir(Config), false).

dir(Config) ->
    proplists:get_value(priv_dir, Config).

%% ===========================================================================

run() ->
    Tmp = ?util:mktemp("diameter_tls"),
    try
        run(Tmp, true)
    after
        file:del_dir_r(Tmp)
    end.

run(Dir, B) ->
    crypto:start(),
    ssl:start(),
    try
        ?util:run([{[fun traffic/2, Dir, B], 60000}])
    after
        diameter:stop(),
        ssl:stop(),
        crypto:stop()
    end.

traffic(Dir, true) ->
    [] = make_certs(Dir),
    traffic(Dir, false);

traffic(Dir, false) ->
    ok = diameter:start(),
    Servers = start_services(Dir),
    Connections = add_transports(Dir, Servers),
    [] = ?util:run([[fun call/1, S] || S <- ?util:scramble(?SERVERS)]),
    [] = remove_transports(Connections),
    [] = stop_services().

make_certs(Dir) ->
    ?util:run([[fun make_cert/2, Dir, B] || B <- ["server1",
                                                  "server2",
                                                  "server4",
                                                  "server5",
                                                  "client"]]).

start_services(Dir) ->
    Servers = [{S, {_,_} = server(S, sopts(S, Dir))} || S <- ?SERVERS],
    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),
    Servers.

add_transports(Dir, Servers) ->
    true = diameter:subscribe(?CLIENT),
    Opts = ssl_options(Dir, "client"),
    [{N, S, connect(?CLIENT, S, copts(N, Opts))} || {N,S} <- Servers].

%% Remove the client transports and expect the corresponding server
%% transport to go down.
remove_transports(Connections) ->
    [] = [T || S <- ?SERVERS, T <- [diameter:subscribe(S)], T /= true],
    [] = ?util:run([[fun disconnect/1, T] || T <- Connections]),
    [S || S <- ?SERVERS,
          I <- [receive #diameter_event{service = S, info = I} -> I end],
          down /= catch element(1, I)].

disconnect({_, {_LRef, _PortNr}, CRef}) ->
    ok = diameter:remove_transport(?CLIENT, CRef).

stop_services() ->
    [{H,T} || H <- [?CLIENT | ?SERVERS],
              T <- [diameter:stop_service(H)],
              T /= ok].

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

%% Send an STR intended for a specific server and expect success.
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

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

inband_security(Ids) ->
    [{'Inband-Security-Id', Ids}].

ssl_options(Dir, Base) ->
    Root = filename:join([Dir, Base]),
    [{ssl_options, [{verify, verify_none},{certfile, Root ++ "_ca.pem"},
                    {keyfile,  Root ++ "_key.pem"}]}].

make_cert(Dir, Base) ->
    make_cert(Dir, Base ++ "_key.pem", Base ++ "_ca.pem").

make_cert(Dir, Keyfile, Certfile) ->
    [KP,CP] = [filename:join([Dir, F]) || F <- [Keyfile, Certfile]],

    KC = join(["openssl genrsa -out", KP, "2048"]),
    CC = join(["openssl req -new -sha256 -x509 -key", KP, "-out", CP, "-days 7",
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
    {up, Ref, _, _, #diameter_packet{}}
         = receive
               #diameter_event{service = Host, info = Info}
                 when element(2, Info) == Ref ->
                   Info
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
