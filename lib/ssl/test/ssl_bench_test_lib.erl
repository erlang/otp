%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-export([setup/1]).

%% Internal exports
-export([setup_server/1]).

-define(remote_host, "NETMARKS_REMOTE_HOST").

setup(Name) ->
    Host = case os:getenv(?remote_host) of
	       false ->
		   {ok, This} = inet:gethostname(),
		   This;
	       RemHost ->
		   RemHost
	   end,
    Node = list_to_atom(atom_to_list(Name) ++ "@" ++ Host),
    SlaveArgs = case init:get_argument(pa) of
	       {ok, PaPaths} ->
		   lists:append([" -pa " ++ P || [P] <- PaPaths]);
	       _ -> []
	   end,
    %% io:format("Slave args: ~p~n",[SlaveArgs]),
    Prog =
	case os:find_executable("erl") of
	    false -> "erl";
	    P -> P
	end,
    io:format("Prog = ~p~n", [Prog]),

    case net_adm:ping(Node) of
	pong -> ok;
	pang ->
	    {ok, Node} =
                slave:start(Host, Name, SlaveArgs, no_link, Prog)
    end,
    Path = code:get_path(),
    true = rpc:call(Node, code, set_path, [Path]),
    ok = rpc:call(Node, ?MODULE, setup_server, [node()]),
    io:format("Client (~p) using ~ts~n",[node(), code:which(ssl)]),
    (Node =:= node()) andalso restrict_schedulers(client),
    Node.

setup_server(ClientNode) ->
    (ClientNode =:= node()) andalso restrict_schedulers(server),
    io:format("Server (~p) using ~ts~n",[node(), code:which(ssl)]),
    ok.

restrict_schedulers(Type) ->
    %% We expect this to run on 8 core machine
    Extra0 = 1,
    Extra =  if (Type =:= server) -> -Extra0; true -> Extra0 end,
    Scheds = erlang:system_info(schedulers),
    erlang:system_flag(schedulers_online, (Scheds div 2) + Extra).
