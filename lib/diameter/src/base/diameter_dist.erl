%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(diameter_dist).

-behaviour(gen_server).

%%
%% Implements callbacks that can be configured as a spawn_opt
%% transport configuration, to be able to distribute incoming Diameter
%% requests to handler processes (local or remote) in various ways.
%%

%% spawn_opt callbacks; initial argument constructed in diameter_traffic
-export([spawn_local/2,
         spawn_local/1,
         route_session/2,
         route_session/1]).

-include_lib("diameter/include/diameter.hrl").

%% server start
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         code_change/3,
         terminate/2]).

-define(SERVER, ?MODULE).  %% server monitoring node connections
-define(TABLE, ?MODULE).   %% node() binary -> node() atom

-define(B(A), atom_to_binary(A, utf8)).

%% spawn_local/2
%%
%% Callback that is equivalent to an options list. That is, the
%% following are equivalent when passed as options to
%% diameter:add_transport/2.
%%
%%   {spawn_opt, Opts}
%%   {spawn_opt, {diameter_dist, spawn_local, [Opts]}}

spawn_local(ReqT, Opts) ->
    spawn_opt(diameter_traffic, request, [ReqT], Opts).

%% spawn_local/1

spawn_local(ReqT) ->
    spawn_local(ReqT, []).

%% route_session/2
%%
%% Callback that routes requests containing Session-Id AVPs as
%% returned by diameter:session_id/0 back to the node on which the
%% function was called. This is only appropriate when sessions are
%% initiated by the own (typically client) node, and ids have been
%% returned from diameter:session_id/0.

route_session(ReqT, Opts) ->
    #diameter_packet{bin = Bin} = element(1, ReqT),
    Node = node_of_session_id(Bin),
    spawn_opt(Node, diameter_traffic, request, [ReqT], Opts).

%% route_session/1

route_session(ReqT) ->
    route_session(ReqT, []).

%% node_of_session_id/1
%%
%% Return the node name encoded as optional value in a Session-Id,
%% assuming the id has been created with diameter:session_id/0.
%%
%% node() is returned if a node name can't be extracted for any
%% reason.

node_of_session_id(<<_Head:20/binary, Avps/binary>>) ->
    sid_node(Avps);

node_of_session_id(_) ->
    node().

%% sid_node/1

%% Session-Id = Command Code 263, V-bit = 0.
sid_node(<<263:32, 0:1, _:7, Len:24, _/binary>> = Bin) ->
    case Bin of
        <<Avp:Len/binary>> ->
            <<_:8/binary, Sid/binary>> = Avp,
            sid_node(Sid, pattern(), 2);  %% look for the optional value
        _ ->
            node()
    end;

%% Jump to the next AVP. This is potentially costly for a message with
%% many AVPs and no Session-Id, which an attacker is prone to send.
%% 8.8 or RFC 6733 says that Session-Id SHOULD (but not MUST) appear
%% immediately following the Diameter Header, so there is no
%% guarantee.
sid_node(<<_:40, Len:24, _/binary>> = Bin) ->
    Pad = (4 - (Len rem 4)) rem 4,
    case Bin of
        <<_:Len/binary, _:Pad/binary, Rest/binary>> ->
            sid_node(Rest);
        _ ->
            node()
    end.

%% sid_node/2

%% Lookup the node name to ensure we don't convert arbitrary binaries
%% to atom.
sid_node(Bin, _, 0) ->
    case ets:lookup(?TABLE, Bin) of
        [{_, Node}] ->
            Node;
        [] ->
            node()
    end;

%% The optional value (if any) of a Session-Id follows the third
%% semicolon. Searching with binary:match/2 does better than matching,
%% especially when the pattern is compiled.
sid_node(Bin, CP, N) ->
    case binary:match(Bin, CP) of
        {Offset, 1} ->
            <<_:Offset/binary, _, Rest/binary>> = Bin,
            sid_node(Rest, CP, N-1);
        nomatch ->
            node()
    end.

%% pattern/0
%%
%% Since this is being called in a watchdog process, compile the
%% pattern once and maintain it in the process dictionary.

pattern() ->
    case get(?MODULE) of
        undefined ->
            CP = binary:compile_pattern(<<$;>>), %% tuple
            put(?MODULE, CP),
            CP;
        CP ->
            CP
    end.

%% ===========================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, _Args = [], _Opts  = []).

%% init/1
%%
%% Maintain [node() | nodes()] in a table that maps from binary-valued
%% names, so we can lookup the corresponding atoms rather than convert
%% binaries that aren't necessarily node names.

init([]) ->
    ets:new(?TABLE, [set, named_table]),
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, nodedown_reason]),
    ets:insert(?TABLE, [{B,N} || N <- [node() | nodes()],
                                 B <- [?B(N)]]),
    {ok, erlang:monotonic_time()}.

%% handle_call/3

handle_call(_, _From, S) ->
    {reply, nok, S}.

%% handle_cast/2

handle_cast(_, S) ->
    {noreply, S}.

%% handle_info/2

handle_info({nodeup, Node, _}, S) ->
    ets:insert(?TABLE, {?B(Node), Node}),
    {noreply, S};

handle_info({nodedown, Node, _}, S) ->
    ets:delete(?TABLE, ?B(Node)),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
