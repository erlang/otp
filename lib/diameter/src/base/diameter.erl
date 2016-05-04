%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter).

%% Configuration.
-export([start_service/2,
         stop_service/1,
         add_transport/2,
         remove_transport/2,
         subscribe/1,
         unsubscribe/1]).

%% Traffic.
-export([session_id/1,
         origin_state_id/0,
         call/3,
         call/4]).

%% Information.
-export([services/0,
         peer_info/1,
         peer_find/1,
         service_info/2]).

%% Start/stop the application. In a "real" application this should
%% typically be a consequence of a release file rather than by calling
%% start/stop explicitly.
-export([start/0,
         stop/0]).

-export_type([evaluable/0,
              restriction/0,
              message_length/0,
              remotes/0,
              sequence/0,
              app_alias/0,
              service_name/0,
              capability/0,
              peer_filter/0,
              peer_ref/0,
              service_opt/0,
              application_opt/0,
              app_module/0,
              transport_ref/0,
              transport_opt/0,
              transport_pred/0,
              call_opt/0]).

-export_type(['OctetString'/0,
              'Integer32'/0,
              'Integer64'/0,
              'Unsigned32'/0,
              'Unsigned64'/0,
              'Float32'/0,
              'Float64'/0,
              'Grouped'/0,
              'Address'/0,
              'Time'/0,
              'UTF8String'/0,
              'DiameterIdentity'/0,
              'DiameterURI'/0,
              'Enumerated'/0,
              'IPFilterRule'/0,
              'QoSFilterRule'/0]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% ---------------------------------------------------------------------------
%% start/0
%% ---------------------------------------------------------------------------

-spec start()
   -> ok
    | {error, term()}.

start() ->
    application:start(?APPLICATION).

%% ---------------------------------------------------------------------------
%% stop/0
%% ---------------------------------------------------------------------------

-spec stop()
   -> ok
    | {error, term()}.

stop() ->
    application:stop(?APPLICATION).

%% ---------------------------------------------------------------------------
%% start_service/2
%% ---------------------------------------------------------------------------

-spec start_service(service_name(), [service_opt()])
   -> ok
    | {error, term()}.

start_service(SvcName, Opts)
  when is_list(Opts) ->
    diameter_config:start_service(SvcName, Opts).

%% ---------------------------------------------------------------------------
%% stop_service/1
%% ---------------------------------------------------------------------------

-spec stop_service(service_name())
   -> ok
    | {error, term()}.

stop_service(SvcName) ->
    diameter_config:stop_service(SvcName).

%% ---------------------------------------------------------------------------
%% services/0
%% ---------------------------------------------------------------------------

-spec services()
   -> [service_name()].

services() ->
    [Name || {Name, _} <- diameter_service:services()].

%% ---------------------------------------------------------------------------
%% service_info/2
%% ---------------------------------------------------------------------------

-spec service_info(service_name(), atom() | [atom()])
   -> any().

service_info(SvcName, Option) ->
    diameter_service:info(SvcName, Option).

%% ---------------------------------------------------------------------------
%% peer_info/2
%% ---------------------------------------------------------------------------

-spec peer_info(peer_ref())
   -> [tuple()].

peer_info(PeerRef) ->
    diameter_service:peer_info(PeerRef).

%% ---------------------------------------------------------------------------
%% peer_find/1
%% ---------------------------------------------------------------------------

-spec peer_find(peer_ref() | pid())
   -> {peer_ref(), pid()}
    | false.

peer_find(Pid) ->
    diameter_peer_fsm:find(Pid).

%% ---------------------------------------------------------------------------
%% add_transport/3
%% ---------------------------------------------------------------------------

-spec add_transport(service_name(), {listen|connect, [transport_opt()]})
   -> {ok, transport_ref()}
    | {error, term()}.

add_transport(SvcName, {T, Opts} = Cfg)
  when is_list(Opts), (T == connect orelse T == listen) ->
    diameter_config:add_transport(SvcName, Cfg).

%% ---------------------------------------------------------------------------
%% remove_transport/2
%% ---------------------------------------------------------------------------

-spec remove_transport(service_name(), transport_pred())
   -> ok | {error, term()}.

remove_transport(SvcName, Pred) ->
    diameter_config:remove_transport(SvcName, Pred).

%% ---------------------------------------------------------------------------
%% subscribe/1
%% ---------------------------------------------------------------------------

-spec subscribe(service_name())
   -> true.

subscribe(SvcName) ->
    diameter_service:subscribe(SvcName).

%% ---------------------------------------------------------------------------
%% unsubscribe/1
%% ---------------------------------------------------------------------------

-spec unsubscribe(service_name())
   -> true.

unsubscribe(SvcName) ->
    diameter_service:unsubscribe(SvcName).

%% ---------------------------------------------------------------------------
%% session_id/1
%% ---------------------------------------------------------------------------

-spec session_id('DiameterIdentity'())
   -> 'OctetString'().

session_id(Ident) ->
    diameter_session:session_id(Ident).

%% ---------------------------------------------------------------------------
%% origin_state_id/0
%% ---------------------------------------------------------------------------

-spec origin_state_id()
   -> 'Unsigned32'().

origin_state_id() ->
    diameter_session:origin_state_id().

%% ---------------------------------------------------------------------------
%% call/3,4
%% ---------------------------------------------------------------------------

-spec call(service_name(), app_alias(), any(), [call_opt()])
   -> any().

call(SvcName, App, Message, Options) ->
    diameter_traffic:send_request(SvcName, {alias, App}, Message, Options).

call(SvcName, App, Message) ->
    call(SvcName, App, Message, []).

%% ===========================================================================

%% Diameter basic types

-type 'OctetString'() :: iolist().
-type 'Integer32'()   :: -2147483647..2147483647.
-type 'Integer64'()   :: -9223372036854775807..9223372036854775807.
-type 'Unsigned32'()  :: 0..4294967295.
-type 'Unsigned64'()  :: 0..18446744073709551615.
-type 'Float32'()     :: '-infinity' | float() | infinity.
-type 'Float64'()     :: '-infinity' | float() | infinity.
-type 'Grouped'()     :: list() | tuple().

%% Diameter derived types

-type 'Address'()
   :: inet:ip_address()
    | string().

-type 'Time'()             :: {{integer(), 1..12, 1..31},
                               {0..23, 0..59, 0..59}}.
-type 'UTF8String'()       :: iolist().
-type 'DiameterIdentity'() :: 'OctetString'().
-type 'DiameterURI'()      :: 'OctetString'().
-type 'Enumerated'()       :: 'Integer32'().
-type 'IPFilterRule'()     :: 'OctetString'().
-type 'QoSFilterRule'()    :: 'OctetString'().

%% The handle to a service.

-type service_name()
   :: any().

%% Capabilities options/avps on start_service/2 and/or add_transport/2

-type capability()
   :: {'Origin-Host',                    'DiameterIdentity'()}
    | {'Origin-Realm',                   'DiameterIdentity'()}
    | {'Host-IP-Address',                ['Address'()]}
    | {'Vendor-Id',                      'Unsigned32'()}
    | {'Product-Name',                   'UTF8String'()}
    | {'Supported-Vendor-Id',            ['Unsigned32'()]}
    | {'Auth-Application-Id',            ['Unsigned32'()]}
    | {'Vendor-Specific-Application-Id', ['Grouped'()]}
    | {'Firmware-Revision',              'Unsigned32'()}.

%% Filters for call/4

-type peer_filter()
   :: none
    | host
    | realm
    | {host,  any|'DiameterIdentity'()}
    | {realm, any|'DiameterIdentity'()}
    | {eval, evaluable()}
    | {neg, peer_filter()}
    | {all, [peer_filter()]}
    | {any, [peer_filter()]}.

-opaque peer_ref()
   :: pid().

-type evaluable()
   :: {module(), atom(), list()}
    | fun()
    | maybe_improper_list(evaluable(), list()).

-type sequence()
   :: {'Unsigned32'(), 0..32}.

-type restriction()
   :: false
    | node
    | nodes
    | [node()]
    | evaluable().

-type remotes()
   :: boolean()
    | [node()]
    | evaluable().

-type message_length()
   :: 0..16#FFFFFF.

%% Options passed to start_service/2

-type service_opt()
   :: capability()
    | {application, [application_opt()]}
    | {restrict_connections, restriction()}
    | {sequence, sequence() | evaluable()}
    | {share_peers, remotes()}
    | {string_decode, boolean()}
    | {strict_mbit, boolean()}
    | {incoming_maxlen, message_length()}
    | {use_shared_peers, remotes()}
    | {spawn_opt, list()}.

-type application_opt()
   :: {alias, app_alias()}
    | {dictionary, module()}
    | {module, app_module()}
    | {state, any()}
    | {call_mutates_state, boolean()}
    | {answer_errors, callback|report|discard}
    | {request_errors, answer_3xxx|answer|callback}.

-type app_alias()
   :: any().

-type app_module()
   :: module()
    | maybe_improper_list(module(), list())
    | #diameter_callback{}.

%% Identifier returned by add_transport/2

-type transport_ref()
   :: reference().

%% Options passed to add_transport/2

-type transport_opt()
   :: {transport_module, atom()}
    | {transport_config, any()}
    | {transport_config, any(), 'Unsigned32'() | infinity}
    | {pool_size, pos_integer()}
    | {applications, [app_alias()]}
    | {capabilities, [capability()]}
    | {capabilities_cb, evaluable()}
    | {capx_timeout, 'Unsigned32'()}
    | {disconnect_cb, evaluable()}
    | {dpr_timeout, 'Unsigned32'()}
    | {dpa_timeout, 'Unsigned32'()}
    | {length_errors, exit | handle | discard}
    | {connect_timer, 'Unsigned32'()}
    | {watchdog_timer, 'Unsigned32'() | {module(), atom(), list()}}
    | {watchdog_config, [{okay|suspect, non_neg_integer()}]}
    | {spawn_opt, list()}
    | {private, any()}.

%% Predicate passed to remove_transport/2

-type transport_pred()
   :: fun((transport_ref(), connect|listen, list()) -> boolean())
    | fun((transport_ref(), list()) -> boolean())
    | fun((list()) -> boolean())
    | transport_ref()
    | boolean()
    | list()
    | {connect|listen, transport_pred()}
    | {atom(), atom(), list()}.

%% Options passed to call/4

-type call_opt()
   :: {extra, list()}
    | {filter, peer_filter()}
    | {timeout, 'Unsigned32'()}
    | detach.
