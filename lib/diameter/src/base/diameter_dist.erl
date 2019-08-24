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

%% spawn_opt callbacks
-export([spawn_local/2,
         spawn_local/1,
         route_session/2,
         route_session/1]).

%% signal availability for handling incoming requests to route_sesssion/2
-export([attach/1,
         detach/1]).

%% consistent hashing
-export([hash/3,   %% for use as default MFA in route_session/2 options map
         hash/2]). %% arbitrary key/values

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

%% Maps a node name binary to the corresponding atom. Used by
%% route_session/2 to map the optional value of a Session-Id to
%% node().
-define(NODE_TABLE, diameter_dist_node).

%% Maps a diameter:service_name() to a node() that has called attach/1
%% to declare its willingness to handle incoming requests for the
%% service. Use by route_session/2 in case the optional value mapping
%% has failed.
-define(SERVICE_TABLE, diameter_dist_service).

-define(B(A), atom_to_binary(A, utf8)).
-define(ORCOND(List), list_to_tuple(['orelse', false | List])).
-define(HASH(T), erlang:phash2(T, 16#100000000)).

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
%% Callback that maps the Session-Id of an incoming request to a
%% handler node.
%%
%% With an options list, maps an id whose optional value is the name
%% of a connected node to the same node, to handle the case that the
%% session id has been returned from diameter:session_id/1; otherwise
%% to a node that has called diameter_dist:attach/1 using the
%% consistent hashing provided by hash/3, or to the local node() if a
%% session id could not be extracted or there are no attached nodes. A
%% handler process is spawned on the selected node using
%% erlang:spawn_opt/4.
%%
%% Different behaviour can be configured by supplying an options map
%% of the following form:
%%
%%   #{search => non_neg_integer(),
%%     id => [binary()],
%%     default => discard | local | mfa(),
%%     dispatch => list() | mfa()}
%%
%% The search member limits the number of AVPs that are examined in
%% the message (from the front), to avoid searching entire message in
%% case it's known that peers follow RFC 6733's recommendation that
%% Session-Id be placed at the head of a message. The default is to
%% search the entire message.
%%
%% The id member restricts the optional value mapping to session ids
%% whose DiamterIdentity is one of those specified. Set this to the
%% list of Diameter identities advertised by the service in question
%% (typically one) to ensure that only locally generated session ids
%% are mapped; or to the empty list to disable the mapping.
%%
%% The default member determines where to handle a message whose
%% Session-Id isn't found or whose optional value isn't mapped to the
%% name of a connected node. The atom local says the local node, an
%% MFA is invoked on Session-Id | false, the name of the diameter
%% service, and the message binary, and should return either a node()
%% or false to discard the message. Defaults to {diameter_dist, hash, []}.
%%
%% The dispatch member determines how the pid() of the request handler
%% process is retrieved. An MFA is applied to a previously selected
%% node(), and the module, function, and arguments list to apply in
%% the handler process to handle the request, the MFA being supplied
%% by diameter, and returns pid() | discard. A list is equivalent to
%% {erlang, spawn_opt, []}. Defaults to [].
%%
%% This can be used with search = 0 to route on something other than
%% Session-Id, but this is probably no simpler than just implementing
%% an own spawn_opt callback. (Except with the default dispatch possibly.)
%%
%% Note that if the peer is also implemented with OTP diameter and
%% generating session ids with diameter:session_id/1 then
%% route_session/2 can map an optional value to a local node that
%% happens to have the same name as one of the peer's nodes. This
%% could lead to an uneven distribution; for example, if the peer
%% nodes are a subset of the local nodes. In practice, it's typically
%% known if it's peers or the local node originating sessions; if the
%% former then setting id = [] disables the optional value mapping, if
%% the latter then setting default = local disables the hashing.
-spec route_session(ReqT :: request(), Opts)
   -> discard
    | pid()
 when Opts :: pos_integer()   %% aka #{search => N}
            | list()          %% aka #{dispatch => Opts}
            | #{search => non_neg_integer(), %% limit number of examined AVPs
                id => [binary()],   %% restrict optional value map on DiamIdent
                default => local    %% handle locally
                         | discard
                         | mfa(),   %% return node() | false
                dispatch => list()  %% spawn options
                          | mfa()}. %% (Node, M, F, A) -> pid() | discard

route_session(ReqT, Opts) ->
    {_, Bin} = Info = diameter_traffic:request_info(ReqT),
    Sid = session_id(avps(Bin), search(Opts)),
    Node = default(node_of_session_id(Sid, Opts), Sid, Opts, Info),
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

%% node_of_session_id/2
%%
%% Return the node name encoded as optional value in a Session-Id,
%% assuming the id has been created with diameter:session_id/0. Lookup
%% the node name to ensure we don't convert arbitrary binaries to
%% atom.

node_of_session_id([Id, _, _, Bin], #{id := Ids}) ->
    lists:member(Id, Ids) andalso nodemap(Bin);

node_of_session_id([_, _, _, Bin], _) ->
    nodemap(Bin);

node_of_session_id(_, _) ->
    false.

%% nodemap/1

nodemap(Bin) ->
    try
        ets:lookup_element(?NODE_TABLE, Bin, 2)
    catch
        error: badarg -> false
    end.

%% session_id/2

session_id(_, 0) ->  %% give up
    false;

%% Session-Id = Command Code 263, V-bit = 0.
session_id(<<263:32, 0:1, _:7, Len:24, _/binary>> = Bin, _) ->
    case Bin of
        <<Avp:Len/binary, _/binary>> ->
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

default(false, _, #{default := discard}, _) ->
    false;

default(false, _, #{default := local}, _) ->
    node();

default(false, Sid, #{default := {M,F,A}}, Info) ->
    {ServiceName, Bin} = Info,
    apply(M, F, [Sid, ServiceName, Bin | A]);  %% node() | false

default(false, Sid, _, Info) -> %% aka {?MODULE, hash, []}
    {ServiceName, Bin} = Info,
    hash(Sid, ServiceName, Bin);

default(Node, _, _, _) ->
    Node.

%% ===========================================================================

%% hash/3
%%
%% Consistent hashing of Session-Id to an attached node, or the local
%% node if Session-Id = false or no attached nodes.

hash(Sid, ServiceName, _) ->
    case false /= Sid andalso attached(ServiceName) of
        [_|_] = Nodes ->
            hash(Sid, Nodes);
        _ ->
            node()
    end.

%% hash/2
%%
%% Consistent hashing on arbitrary key/values. Returns false if the
%% list is empty.

%% No key or no values.
hash(_, []) ->
    false;

%% Not much choice.
hash(_, [Value]) ->
    Value;

%% Hash on a circle and choose the closest predecessor.
hash(Key, Values) ->
    Hash = ?HASH(Key),
    tl(lists:foldl(fun(V,A) ->
                           choose(Hash, [?HASH({Key, V}) | V], A)
                   end,
                   false,  %% < list()
                   Values)).

%% choose/3

choose(Hash, [Hash1 | _] = T, [Hash2 | _])
  when Hash1 =< Hash, Hash < Hash2 ->
    T;

choose(Hash, [Hash1 | _], [Hash2 | _] = T)
  when Hash2 =< Hash, Hash < Hash1 ->
    T;

choose(_, T1, T2) ->
    max(T1, T2).

%% ===========================================================================

%% attach/1
%%
%% Register the local node as a handler of incoming requests for the
%% specified services when using the route_session/2 spawn_opt
%% callback.

attach(ServiceNames) ->
    abcast({attach, node(), ServiceNames}).

%% detach/1
%%
%% Deregister the local node as a handler of incoming requests.

detach(ServiceNames) ->
    abcast({detach, node(), ServiceNames}).

%% abcast/1

abcast(T) ->
    gen_server:abcast([node() | nodes()], ?SERVER, T),
    ok.

%% attached/1

attached(ServiceName) ->
    try
        ets:lookup_element(?SERVICE_TABLE, ServiceName, 2)
    catch
        error: badarg -> []
    end.

%% cast/2

cast(Node, T) ->
    gen_server:cast({?SERVER, Node}, T).

%% attach/2

attach(Node, S) ->
    case sets:to_list(S) of
        [] ->
            ok;
        Services ->
            cast(Node, {attach, node(), Services})
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
    ets:new(?NODE_TABLE,    [set, named_table]),
    ets:new(?SERVICE_TABLE, [bag, named_table]),
    ok = net_kernel:monitor_nodes(true, [{node_type, visible},
                                         nodedown_reason]),
    ets:insert(?NODE_TABLE, [{?B(N), N} || N <- [node() | nodes()]]),
    abcast({attach, node()}),
    {ok, sets:new()}.

%% handle_call/3

handle_call(_, _From, S) ->
    {reply, nok, S}.

%% handle_cast/2

%% Remote node is asking which services the local node wants to handle.
handle_cast({attach, Node}, S)
  when Node /= node() ->
    attach(Node, S),
    {noreply, S};

%% Node wants to handle incoming requests ...
handle_cast({attach, Node, ServiceNames}, S) ->
    ets:insert(?SERVICE_TABLE, [{N, Node} || N <- ServiceNames]),
    {noreply, case node() of
                  Node ->
                      sets:union(S, sets:from_list(ServiceNames));
                  _ ->
                      S
              end};

%% ... or not.
handle_cast({detach, Node, ServiceNames}, S) ->
    ets:select_delete(?SERVICE_TABLE, [{{'$1', Node},
                                        [?ORCOND([{'==', '$1', {const, N}}
                                                  || N <- ServiceNames])],
                                        [true]}]),
    {noreply, case node() of
                  Node ->
                      sets:subtract(S, sets:from_list(ServiceNames));
                  _ ->
                      S
              end};

handle_cast(_, S) ->
    {noreply, S}.

%% handle_info/2

handle_info({nodeup, Node, _}, S) ->
    ets:insert(?NODE_TABLE, {?B(Node), Node}),
    cast(Node, {attach, node()}),  %% ask which services remote node handles
    attach(Node, S),               %% say which service local node handles
    {noreply, S};

handle_info({nodedown, Node, _}, S) ->
    ets:delete(?NODE_TABLE, ?B(Node)),
    ets:select_delete(?SERVICE_TABLE, [{{'_', Node}, [], [true]}]),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

%% Old code inadvertently monitored all nodes: start a new
%% subscription and remove the old one.
code_change(_OldVsn, State, "2.2") ->
    ok = net_kernel:monitor_nodes(true, [{node_type, visible},
                                         nodedown_reason]),
    ok = net_kernel:monitor_nodes(false, [{node_type, all},
                                          nodedown_reason]),
    {ok, State};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
