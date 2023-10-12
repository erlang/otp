%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2023. All Rights Reserved.
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
-module(ssl_bench_test_lib).

%% API
-export([setup/1, cleanup/1]).

%% Internal exports
-export([setup_server/1]).

-include("ssl_test_lib.hrl").
-define(remote_host, "NETMARKS_REMOTE_HOST").

setup(Name) ->
    NameStr = atom_to_list(Name),
    case os:getenv(?remote_host) of
        false ->
            {ok, Host} = inet:gethostname(),
            Remote = false,
            ok;
        Host ->
            Remote = true,
            ok
    end,
    Node = list_to_atom(NameStr ++ "@" ++ Host),
    case net_adm:ping(Node) of
        pong ->
            Node;
        pang ->
            PeerOptions =
                #{name => NameStr,
                  host => Host},
            ?CT_LOG("PeerOptions: ~p~n", [PeerOptions]),
            {ok, _Pid, Node} =
                peer:start(
                  case Remote of
                      true ->
                          Ssh = find_executable("ssh"),
                          Erl = find_executable("erl"),
                          PeerOptions#{exec => {Ssh, [Host, Erl]}};
                      false ->
                          PeerOptions
                  end),
            Path = code:get_path(),
            true = erpc:call(Node, code, set_path, [Path]),
            ok = erpc:call(Node, ?MODULE, setup_server, [node()]),
            ?CT_LOG("Client (~p) using ~ts~n",[node(), code:which(ssl)]),
            (Node =:= node()) andalso restrict_schedulers(client),
            Node
    end.

find_executable(Prog) ->
    case os:find_executable(Prog) of
        false -> Prog;
        P     -> P
    end.

setup_server(ClientNode) ->
    (ClientNode =:= node()) andalso restrict_schedulers(server),
    ?CT_PAL("Server (~p) using ~ts~n",[node(), code:which(ssl)]),
    ok.

restrict_schedulers(Type) ->
    %% We expect this to run on 8 core machine
    Extra0 = 1,
    Extra =  if (Type =:= server) -> -Extra0; true -> Extra0 end,
    Scheds = erlang:system_info(schedulers),
    erlang:system_flag(schedulers_online, (Scheds div 2) + Extra).

cleanup(Node) ->
    try erpc:call(Node, erlang, halt, [], 5000) of
        Result ->
            ct:fail({unexpected_return, Result})
    catch
        error : {erpc,noconnection} ->
            ok;
        Class : Reason : Stacktrace ->
            ct:fail({unexpected_exception, {Class,Reason,Stacktrace}})
    end.
