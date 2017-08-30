%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2]).

%% testcases
-export([start/1,
         connect/1,
         send_dpr/1,
         remove_transport/1,
         stop_service/1,
         check/1,
         stop/1]).

%% disconnect_cb
-export([disconnect/5]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc6733.hrl").

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

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, R} || R <- [client, server, uncommon | ?REASONS]].

%% The group determines how transports are terminated: by remove_transport,
%% stop_service or application stop.
groups() ->
    [{R, [], [start, send_dpr, stop]} || R <- [client, server, uncommon]]
        ++ [{R, [], Ts} || Ts <- [tc()], R <- ?REASONS].

init_per_suite(Config) ->  %% not need, but a useful place to enable trace
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Name, Config) ->
    [{group, Name} | Config].

end_per_group(_, _) ->
    ok.

tc() ->
    [start, connect, remove_transport, stop_service, check, stop].

%% ===========================================================================

%% start/1

start(Config)
  when is_list(Config) ->
    Grp = group(Config),
    ok = diameter:start(),
    ok = diameter:start_service(?SERVER, service(?SERVER, Grp)),
    ok = diameter:start_service(?CLIENT, service(?CLIENT, Grp)).

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
    LRef = ?util:listen(?SERVER, tcp),
    Ref = ?util:connect(?CLIENT, tcp, LRef, [{dpa_timeout, 10000}]),
    Svc = sender(group(Config)),
    [Info] = diameter:service_info(Svc, connections),
    {_, {TPid, _}} = lists:keyfind(peer, 1, Info),
    #diameter_base_DPA{'Result-Code' = 2001}
        = diameter:call(Svc,
                        common,
                        ['DPR', {'Origin-Host', Svc ++ ".erlang.org"},
                         {'Origin-Realm', "erlang.org"},
                         {'Disconnect-Cause', 0}],
                        [{peer, TPid}]),
    ok =  receive  %% ensure the transport dies on DPA
              #diameter_event{service = ?CLIENT, info = {down, Ref, _, _}} ->
                  ok
          after 5000 ->
                  erlang:process_info(self(), messages)
          end.

%% sender/1

sender(server) ->
    ?SERVER;

sender(_) ->
    ?CLIENT.

%% connect/1

connect(Config) ->
    Pid = spawn(fun init/0),  %% process for disconnect_cb to bang
    Grp = group(Config),
    LRef = ?util:listen(?SERVER, tcp),
    Refs = [?util:connect(?CLIENT, tcp, LRef, opts(RCs, {Grp, Pid}))
            || RCs <- ?RETURNS],
    ?util:write_priv(Config, config, [Pid | Refs]).

%% remove_transport/1

%% Remove all the client transports only in the transport group.
remove_transport(Config) ->
    transport == group(Config)
        andalso (ok = diameter:remove_transport(?CLIENT, true)).

%% stop_service/1

%% Stop the service only in the service group.
stop_service(Config) ->
    service == group(Config)
        andalso (ok = diameter:stop_service(?CLIENT)).

%% check/1

%% Check for callbacks before diameter:stop/0, not the other way around
%% for the timing reason explained below.
check(Config) ->
    Grp = group(Config),
    [Pid | Refs] = ?util:read_priv(Config, config),
    Pid ! self(),                      %% ask for dictionary
    Dict = receive {Pid, D} -> D end,  %% get it
    check(Refs, ?RETURNS, Grp, Dict).  %% check for callbacks

%% stop/1

stop(_Config) ->
    ok = diameter:stop().

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
    {group, Grp} = lists:keyfind(group, 1, Config),
    Grp.

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

init() ->
    exit(recv(dict:new())).

recv(Dict) ->
    receive
        Pid when is_pid(Pid) ->
            Pid ! {self(), Dict};
        {Reason, Ref} ->
            recv(dict:store(Ref, Reason, Dict))
    end.
