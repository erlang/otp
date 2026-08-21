%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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
%% Tests of the disconnect_cb configuration.
%%

-module(diameter_dpr_SUITE).

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         %% The test cases
         client/1,
         server/1,
         uncommon/1,
         transport/1,
         service/1,
         application/1
        ]).

%% internal
-export([connect/1,
         send_dpr/1,
         remove_transport/1,
         stop_service/1,
         check/1]).

%% disconnect_cb
-export([disconnect/5]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").

-include("diameter_util.hrl").


%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").

%% Config for diameter:start_service/2.
-define(SERVICE(Host),
        [{'Origin-Host', Host ++ ".erlang.org"},
         {'Origin-Realm', "erlang.org"},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', hd(Host)},  %% match this in disconnect/5
         {'Product-Name', "OTP/diameter"},
         {restrict_connections, false}]).

%% Disconnect reasons that diameter passes as the first argument of a
%% function configured as disconnect_cb.
-define(REASONS, [transport, service, application]).

%% Valid values for Disconnect-Cause.
-define(CAUSES, [0, rebooting, 1, busy, 2, goaway]).

%% Establish one client connection for each element of this list,
%% configured with disconnect/5, disconnect_cb returning the specified
%% value.
-define(RETURNS,
        [[close, {dpr, [{cause, invalid}]}],
         [ignore, close],
         []]
        ++ [[{dpr, [{timeout, 5000}, {cause, T}]}] || T <- ?CAUSES]).


-define(DL(F),    ?DL(F, [])).
-define(DL(F, A), ?LOG("DDPRS", F, A)).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [client, server, uncommon, transport, service, application].


init_per_suite(Config) ->
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?DUTIL:end_per_suite(Config).


-define(tc(Name), Name(_) -> ?DL("~w -> entry", [Name]), run([Name])).

?tc(client).
?tc(server).
?tc(uncommon).
?tc(transport).
?tc(service).
?tc(application).


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(List)
  when is_list(List) ->
    ?DL("run -> entry with"
        "~n   List: ~p", [List]),
    try
        ?RUN([[{[fun run/1, T], 15000} || T <- List]])
    after
        ?DL("run(after) -> stop diameter"),
        diameter:stop()
    end;

run(Grp) ->
    ?DL("run(~w) -> start (diameter) app", [Grp]),
    ok = diameter:start(),

    ?DL("run(~w) -> register services", [Grp]),
    ok = ?DEL_REG(?SERVER),
    ok = ?DEL_REG(?CLIENT),

    ?DL("run(~w) -> start (diameter) service 'server'", [Grp]),
    ok = diameter:start_service(?SERVER, service(?SERVER, Grp)),
    ?DL("run(~w) -> start (diameter) service 'client'", [Grp]),
    ok = diameter:start_service(?CLIENT, service(?CLIENT, Grp)),
    _ = lists:foldl(fun(F,A) ->
                            ?DL("run(~w) -> apply"
                                "~n   F: ~p"
                                "~n   A: ~p", [Grp, F, A]),
                            apply(?MODULE, F, [A])
                    end,
                    [{group, Grp}],
                    tc(Grp)),

    ?DL("run(~w) -> unregister services", [Grp]),
    ok = ?DEL_REG(?CLIENT),
    ok = ?DEL_REG(?SERVER),

    ?DL("run(~w) -> stop (diameter) app", [Grp]),
    ok = diameter:stop(),
    ?DL("run(~w) -> done", [Grp]),
    ok.

tc(T)
  when T == client;
       T == server;
       T == uncommon ->
    [send_dpr];

tc(_) ->
    [connect, remove_transport, stop_service, check].

service(?SERVER = Svc, _) ->
    ?SERVICE(Svc)
        ++ [{'Acct-Application-Id', [0,3]},
            {application, [{dictionary, diameter_gen_base_rfc6733},
                           {alias, common},
                           {module, #diameter_callback{_ = false}}]},
            {application, [{dictionary, diameter_gen_acct_rfc6733},
                           {alias, acct},
                           {module, #diameter_callback{_ = false}}]}];

%% Client that receives a server DPR despite no explicit support for
%% Diameter common messages.
service(?CLIENT = Svc, server) ->
    ?SERVICE(Svc)
        ++ [{'Acct-Application-Id', [3]},
            {application, [{dictionary, diameter_gen_acct_rfc6733},
                           {alias, acct},
                           {module, #diameter_callback{_ = false}}]}];

%% Client that sends DPR despite advertised only the accounting
%% application. The dictionary is required for encode.
service(?CLIENT = Svc, uncommon) ->
    ?SERVICE(Svc)
        ++ [{'Acct-Application-Id', [3]},
            {application, [{dictionary, diameter_gen_base_rfc6733},
                           {alias, common},
                           {module, #diameter_callback{_ = false}}]},
            {application, [{dictionary, diameter_gen_acct_rfc6733},
                           {alias, acct},
                           {module, #diameter_callback{_ = false}}]}];

service(?CLIENT = Svc, _) ->
    ?SERVICE(Svc)
        ++ [{'Auth-Application-Id', [0]},
            {application, [{dictionary, diameter_gen_base_rfc6733},
                           {alias, common},
                           {module, #diameter_callback{_ = false}}]}].

%% send_dpr/1

send_dpr(Config) ->
    ?DL("~w -> entry with"
        "~n   Config: ~p"
        "~n   => try listen", [?FUNCTION_NAME, Config]),
    LRef = ?LISTEN(?SERVER, tcp),
    ?DL("~w -> try connect", [?FUNCTION_NAME]),
    Ref  = ?CONNECT(?CLIENT, tcp, LRef, [{dpa_timeout, 10000}]),
    ?DL("~w -> get sender", [?FUNCTION_NAME]),
    Svc  = sender(group(Config)),
    ?DL("~w -> get connections for ~p", [?FUNCTION_NAME, Svc]),
    Info = case sdpr_await_connections(Svc) of
               no_connections ->
                   ?DL("~w -> no connections found: "
                       "~n   Svc:      ~p"
                       "~n   Svc info: ~p"
                       "~n   Services: ~p",
                       [?FUNCTION_NAME,
                        Svc,
                        diameter:service_info(Svc, all),
                        diameter:services()]),
                   ct:fail({no_connections, Svc});
               I ->
                   I
           end,
    {_, {TPid, _}} = lists:keyfind(peer, 1, Info),
    ?DL("~w -> make a call (expect result 2001)", [?FUNCTION_NAME]),
    #diameter_base_DPA{'Result-Code' = 2001}
        = diameter:call(Svc,
                        common,
                        ['DPR', {'Origin-Host', Svc ++ ".erlang.org"},
                         {'Origin-Realm', "erlang.org"},
                         {'Disconnect-Cause', 0}],
                        [{peer, TPid}]),
    ?DL("~w -> await down event", [?FUNCTION_NAME]),
    ok =  receive  %% ensure the transport dies on DPA
              #diameter_event{service = ?CLIENT, info = {down, Ref, _, _}} ->
                  ?DL("~w -> received down event", [?FUNCTION_NAME]),
                  ok
          after 5000 ->
                  MSGs = erlang:process_info(self(), messages),
                  ?DL("~w -> (down) event timeout: "
                      "~n   ~p", [?FUNCTION_NAME, MSGs]),
                  MSGs
          end,
    ?DL("~w -> done", [?FUNCTION_NAME]),
    ok.


-define(SDPR_AWAIT_CONN_N, 10).

sdpr_await_connections(Svc) ->
    sdpr_await_connections(Svc, ?SDPR_AWAIT_CONN_N).

sdpr_await_connections(_Svc, 0) ->
    no_connections;
sdpr_await_connections(Svc, N) ->
    case diameter:service_info(Svc, connections) of
        [I] when (N =:= ?SDPR_AWAIT_CONN_N) ->
            I;
        [I] when (N =/= ?SDPR_AWAIT_CONN_N) ->
            ?DL("sdpr_await_connections -> connections found at ~w", [N]),
            I;
        [] ->
            timer:sleep(500),
            sdpr_await_connections(Svc, N-1)
    end.

%% sender/1

sender(server) ->
    ?SERVER;

sender(_) ->
    ?CLIENT.

%% connect/1

connect(Config) ->
    Self = self(),
    Grp = group(Config),
    Pid = spawn(fun() -> init(Self) end), %% process for disconnect_cb to bang
    LRef = ?util:listen(?SERVER, tcp),
    Refs = [?util:connect(?CLIENT, tcp, LRef, opts(RCs, {Grp, Pid}))
            || RCs <- ?RETURNS],
    Pid ! (Grp == application orelse length(Refs)),
    [{config, [Pid | Refs]} | Config].

%% remove_transport/1

%% Remove all the client transports only in the transport group.
remove_transport(Config) ->
    transport == group(Config)
        andalso (ok = diameter:remove_transport(?CLIENT, true)),
    Config.

%% stop_service/1

%% Stop the service only in the service group.
stop_service(Config) ->
    service == group(Config)
        andalso (ok = diameter:stop_service(?CLIENT)),
    Config.

%% check/1

%% Check for callbacks before diameter:stop/0, not the other way around
%% for the timing reason explained below.
check(Config) ->
    Grp = group(Config),
    [Pid | Refs] = proplists:get_value(config, Config),
    Pid ! self(),                      %% ask for dictionary
    Dict = receive {Pid, D} -> D end,  %% get it
    check(Refs, ?RETURNS, Grp, Dict).  %% check for callbacks

%% ===========================================================================

%% Whether or not there are callbacks after diameter:stop() depends on
%% timing as long as the server runs on the same node: a server
%% transport could close the connection before the client has chance
%% to apply its callback. Therefore, just check that there haven't
%% been any callbacks yet.
check(_, _, application, Dict) ->
    [] = dict:to_list(Dict);

check([], [], _, _) ->
    ok;

check([Ref | Refs], CBs, Grp, Dict) ->
    check1(Ref, hd(CBs), Grp, Dict),
    check(Refs, tl(CBs), Grp, Dict).

check1(Ref, [ignore | RCs], Reason, Dict) ->
    check1(Ref, RCs, Reason, Dict);

check1(Ref, [_|_], Reason, Dict) ->
    {ok, Reason} = dict:find(Ref, Dict);  %% callback with expected reason

check1(Ref, [], _, Dict) ->
    error = dict:find(Ref, Dict).  %% no callback

%% ----------------------------------------

group(Config) ->
    proplists:get_value(group, Config).

%% Configure the callback with the group name (= disconnect reason) as
%% extra argument.
opts(RCs, T) ->
    [{disconnect_cb, {?MODULE, disconnect, [T, RC]}} || RC <- RCs].

%% Match the group name with the disconnect reason to ensure the
%% callback is being called as expected.
disconnect(Reason, Ref, Peer, {Reason, Pid}, RC) ->
    io:format("disconnect: ~p ~p~n", [Ref, Reason]),
    {_, #diameter_caps{vendor_id = {$C,$S}}} = Peer,
    Pid ! {Reason, Ref},
    RC.

init(Pid) ->
    monitor(process, Pid),
    exit(recv(receive T -> T end, dict:new())).

recv(true, Dict) ->
    recv(0, Dict);
recv(N, Dict) ->
    receive
        Pid when N == 0, is_pid(Pid) ->
            Pid ! {self(), Dict};
        {Reason, Ref} ->
            recv(N - 1, dict:store(Ref, Reason, Dict));
        {'DOWN', _, process, _, _} ->
            ok
    end.
