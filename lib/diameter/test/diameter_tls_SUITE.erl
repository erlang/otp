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
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([start_ssl/1,
         start_diameter/1,
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
    [{timetrap, {seconds, 15}}].

all() ->
    [start_ssl,
     start_diameter,
     start_services,
     add_transports]
        ++ [{group, N} || {N, _, _} <- groups()]
        ++ [remove_transports, stop_services, stop_diameter, stop_ssl].

groups() ->
    Ts = tc(),
    [{all, [], Ts},
     {p, [parallel], Ts}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_suite(Config) ->
    case os:find_executable("openssl") of
        false ->
            {skip, no_openssl};
        _ ->
            Config
    end.

end_per_suite(_Config) ->
    ok.

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
    ok = receive #diameter_event{info = {down, LRef, _, _}} -> ok
         after 2000 -> false
         end.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).

inband_security(Ids) ->
    [{'Inband-Security-Id', Ids}].

ssl_options(Dir, Base) ->
    {Key, Cert} = make_cert(Dir, Base ++ "_key.pem", Base ++ "_ca.pem"),
    [{ssl_options, [{certfile, Cert}, {keyfile, Key}]}].

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

%% server/2

server(Host, {Caps, Opts}) ->
    ok = diameter:start_service(Host, ?SERVICE(Host, ?DICT_COMMON)),
    {ok, LRef} = diameter:add_transport(Host, ?LISTEN(Caps, Opts)),
    {LRef, portnr(LRef)}.

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

portnr(LRef) ->
    portnr(LRef, 20).

portnr(LRef, N)
  when 0 < N ->
    case diameter_reg:match({diameter_tcp, listener, {LRef, '_'}}) of
        [{T, _Pid}] ->
            {_, _, {LRef, {_Addr, LSock}}} = T,
            {ok, PortNr} = to_portnr(LSock) ,
            PortNr;
        [] ->
            receive after 500 -> ok end,
            portnr(LRef, N-1)
    end.

to_portnr(Sock)
  when is_port(Sock) ->
    inet:port(Sock);
to_portnr(Sock) ->
    case ssl:sockname(Sock) of
        {ok, {_,N}} ->
            {ok, N};
        No ->
            No
    end.

%% connect/3

connect(Host, {_LRef, PortNr}, {Caps, Opts}) ->
    {ok, Ref} = diameter:add_transport(Host, ?CONNECT(PortNr, Caps, Opts)),
    ok = receive
             #diameter_event{service = Host,
                             info = {up, Ref, _, _, #diameter_packet{}}} ->
                 ok
         after 2000 ->
                 false
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
