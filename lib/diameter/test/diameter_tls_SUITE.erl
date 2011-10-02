%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Tests of traffic between four Diameter nodes connected as follows.
%%
%%                  ---- SERVER.REALM1
%%                /
%%   CLIENT.REALM0 ----- SERVER.REALM2
%%                \
%%                  ---- SERVER.REALM3
%%
%% The first two connections are established over TLS, the third not.
%%

-module(diameter_tls_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([send1/1,
         send2/1,
         send3/1,
         remove_transports/1,
         stop_services/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-ifdef(DIAMETER_CT).
-include("diameter_gen_base_rfc3588.hrl").
-else.
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-endif.

-include_lib("diameter/include/diameter.hrl").
-include("diameter_ct.hrl").

%% ===========================================================================

-define(ADDR, {127,0,0,1}).

-define(CLIENT,  "CLIENT.REALM0").
-define(SERVER1, "SERVER.REALM1").
-define(SERVER2, "SERVER.REALM2").
-define(SERVER3, "SERVER.REALM3").

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
                        {module, ?MODULE},
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

-define(LOGOUT, ?'DIAMETER_BASE_TERMINATION-CAUSE_DIAMETER_LOGOUT').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [{group, N} || {N, _, _} <- groups()]
        ++ [remove_transports, stop_services].

groups() ->
    Ts = tc(),
    [{all, [], Ts},
     {p, [parallel], Ts}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_suite(Config) ->
    ok = ssl:start(),
    ok = diameter:start(),

    Dir = proplists:get_value(priv_dir, Config),
    Servers = [server(?SERVER1,
                      inband_security([?TLS]),
                      ssl_options(Dir, "server1")),
               server(?SERVER2,
                      inband_security([?NO_INBAND_SECURITY, ?TLS]),
                      ssl_options(Dir, "server2")),
               server(?SERVER3,
                      [],
                      [])],

    ok = diameter:start_service(?CLIENT, ?SERVICE(?CLIENT, ?DICT_COMMON)),

    true = diameter:subscribe(?CLIENT),

    Connections = connect(?CLIENT,
                          Servers,
                          inband_security([?NO_INBAND_SECURITY, ?TLS]),
                          ssl_options(Dir, "client")),

    [{transports, lists:zip(Servers, Connections)} | Config].

end_per_suite(_Config) ->
    ok = diameter:stop(),
    ok = ssl:stop().

%% Testcases to run when services are started and connections
%% established. These are trivial, the interesting stuff is setting up
%% the connections in init_per_suite/2.
tc() ->
    [send1,
     send2,
     send3].

%% ===========================================================================

inband_security(Ids) ->
    [{'Inband-Security-Id', Ids}].

ssl_options(Dir, Base) ->
    {Key, Cert} = make_cert(Dir, Base ++ "_key.pem", Base ++ "_ca.pem"),
    [{ssl_options, [{certfile, Cert}, {keyfile, Key}]}].

server(Host, Caps, Opts) ->
    ok = diameter:start_service(Host, ?SERVICE(Host, ?DICT_COMMON)),
    {ok, LRef} = diameter:add_transport(Host, ?LISTEN(Caps, Opts)),
    {LRef, portnr(LRef)}.

connect(Host, {_LRef, PortNr}, Caps, Opts) ->
    {ok, Ref} = diameter:add_transport(Host, ?CONNECT(PortNr, Caps, Opts)),
    ok = receive
             #diameter_event{service = Host,
                             info = {up, Ref, _, _, #diameter_packet{}}} ->
                 ok
         after 2000 ->
                 false
         end,
    Ref;
connect(Host, Ports, Caps, Opts) ->
    [connect(Host, P, Caps, Opts) || P <- Ports].

portnr(LRef) ->
    portnr(LRef, 20).

portnr(LRef, N)
  when 0 < N ->
    case diameter_reg:match({diameter_tcp, listener, {LRef, '_'}}) of
        [{T, _Pid}] ->
            {_, _, {LRef, {_Addr, LSock}}} = T,
            {ok, PortNr} = inet:port(LSock),
            PortNr;
        [] ->
            receive after 50 -> ok end,
            portnr(LRef, N-1)
    end.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

make_cert(Dir, Keyfile, Certfile) ->
    [K,C] = Paths = [filename:join([Dir, F]) || F <- [Keyfile, Certfile]],

    KCmd = join(["openssl genrsa -out", K, "2048"]),
    CCmd = join(["openssl req -new -x509 -key", K, "-out", C, "-days 7",
                 "-subj /C=SE/ST=./L=Stockholm/CN=www.erlang.org"]),

    %% Hope for the best and only check that files are written.
    os:cmd(KCmd),
    os:cmd(CCmd),

    [_,_] = [T || P <- Paths, {ok, T} <- [file:read_file_info(P)]],

    {K,C}.

join(Strs) ->
    string:join(Strs, " ").

%% ===========================================================================

%% Send an STR intended for a specific server and expect success.
send1(_Config) ->
    call(?SERVER1).
send2(_Config) ->
    call(?SERVER2).
send3(_Config) ->
    call(?SERVER3).

%% Remove the client transports and expect the corresponding server
%% transport to go down.
remove_transports(Config) ->
    Ts = proplists:get_value(transports, Config),

    true = diameter:subscribe(?SERVER1),
    true = diameter:subscribe(?SERVER2),
    true = diameter:subscribe(?SERVER3),

    lists:map(fun disconnect/1, Ts).

disconnect({{LRef, _PortNr}, CRef}) ->
    ok = diameter:remove_transport(?CLIENT, CRef),
    ok = receive #diameter_event{info = {down, LRef, _, _}} -> ok
         after 2000 -> false
         end.

stop_services(_Config) ->
    S = [?CLIENT, ?SERVER1, ?SERVER2, ?SERVER3],
    Ok = [ok || _ <- S],
    Ok = [diameter:stop_service(H) || H <- S].

%% ===========================================================================

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

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([Peer], _, ?CLIENT, _State) ->
    {ok, Peer}.

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

%% handle_error/4

handle_error(Reason, _Req, ?CLIENT, _Peer) ->
    {error, Reason}.

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
