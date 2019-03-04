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

-type request() :: tuple().  %% callback argument from diameter_traffic

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

-spec spawn_local(ReqT :: request(), Opts :: list())
   -> pid().

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
%% only initiated by the own (typically client) node, and ids have
%% been returned from diameter:session_id/0.
%%
%% This can be used with #{search => 0} to route on something other
%% than Session-Id since default can be an MFA returning a node()
%% (applied to the incoming diameter_packet record) and dispatch can
%% be an MFA returning a pid() (applied to Node and the request MFA),
%% but this is no simpler than just implementing an own spawn_opt
%% callback. (Except with the default dispatch possibly.)

-spec route_session(ReqT :: request(), Opts)
   -> discard
    | pid()
 when Opts :: pos_integer()   %% aka #{search => N}
            | list()          %% aka #{dispatch => Opts}
            | #{search => non_neg_integer(), %% limit number of examined AVPs
                default => discard | mfa(),  %% return node() | false
                dispatch => list() | mfa()}. %% spawn options or return pid()

route_session(ReqT, Opts) ->
    #diameter_packet{bin = Bin} = Pkt = element(1, ReqT),
    Sid = session_id(avps(Bin), search(Opts)),
    Node = default(node_of_session_id(Sid), Sid, Opts, Pkt),
    dispatch(Node, ReqT, dispatch(Opts)).

%% avps/1

avps(<<_:20/binary, Bin/binary>>) ->
    Bin;

avps(_) ->
    false.

%% dispatch/3

dispatch(false, _, _) ->
    discard;

dispatch(Node, ReqT, {M,F,A}) ->
    apply(M, F, [Node, diameter_traffic, request, [ReqT] | A]);

dispatch(Node, ReqT, Opts) ->
    spawn_opt(Node, diameter_traffic, request, [ReqT], Opts).

%% route_session/1

route_session(ReqT) ->
    route_session(ReqT, []).

%% node_of_session_id/1
%%
%% Return the node name encoded as optional value in a Session-Id,
%% assuming the id has been created with diameter:session_id/0. Lookup
%% the node name to ensure we don't convert arbitrary binaries to
%% atom.

node_of_session_id([_, _, _, Bin]) ->
    case ets:lookup(?TABLE, Bin) of
        [{_, Node}] ->
            Node;
        [] ->
            false
    end;

node_of_session_id(_) ->
    false.

%% session_id/2

session_id(_, 0) ->  %% give up
    false;

%% Session-Id = Command Code 263, V-bit = 0.
session_id(<<263:32, 0:1, _:7, Len:24, _/binary>> = Bin, _) ->
    case Bin of
        <<Avp:Len/binary>> ->
            <<_:8/binary, Sid/binary>> = Avp,
            split(Sid);
        _ ->
            false
    end;

%% Jump to the next AVP. This is potentially costly for a message with
%% many AVPs and no Session-Id, which an attacker is prone to send.
%% 8.8 or RFC 6733 says that Session-Id SHOULD (but not MUST) appear
%% immediately following the Diameter Header, so there is no
%% guarantee.
session_id(<<_:40, Len:24, _/binary>> = Bin, N) ->
    Pad = (4 - (Len rem 4)) rem 4,
    case Bin of
        <<_:Len/binary, _:Pad/binary, Rest/binary>> ->
            session_id(Rest, if N == infinity -> N; true -> N-1 end);
        _ ->
            false
    end;

session_id(_, _) ->
    false.

%% split/1
%%
%% Split a Session-Id at no more than three semicolons: the optional
%% value (if any) follows the third. binary:split/2 does better than
%% matching character by character, especially when the pattern is
%% compiled.

split(Bin) ->
    split(3, Bin, pattern()).

%% split/3

split(0, Bin, _) ->
    [Bin];

split(N, Bin, Pattern) ->
    [H|T] = binary:split(Bin, Pattern),
    [H | case T of
             [] ->
                 T;
             [Rest] ->
                 split(N-1, Rest, Pattern)
         end].

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

%% dispatch/1

dispatch(#{} = Opts) ->
    maps:get(dispatch, Opts, []);

dispatch(Opts)
  when is_list(Opts) ->
    Opts;

dispatch(_) ->
    [].

%% search/1
%%
%% Bound number of AVPs examined when looking for Session-Id.

search(#{search := N})
  when is_integer(N), 0 =< N ->
    N;

search(N)
  when is_integer(N), 0 =< N ->
    N;

search(_) ->
    infinity.

%% default/3
%%
%% Choose a node when Session-Id lookup has failed.

default(false = No, _, #{default := discard}, _) ->
    No;

default(false, Sid, #{default := {M,F,A}}, Pkt) ->
    apply(M, F, [Sid, Pkt | A]);  %% false | node()

default(false, _, _, _) ->
    node();

default(Node, _, _, _) ->
    Node.

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
