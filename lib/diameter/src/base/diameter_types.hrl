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
%% Types for function specifications, primarily in diameter.erl. This
%% has nothing specifically to do with diameter_types.erl.
%%

-type evaluable()
   :: {module(), atom(), list()}
    | fun()
    | nonempty_improper_list(evaluable(), list()).   %% [evaluable() | Args]

-type app_alias()
   :: any().

-type service_name()
   :: any().

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

%% Options passed to start_service/2

-type service_opt()
   :: capability()
    | {application, [application_opt()]}.

-type application_opt()
   :: {alias, app_alias()}
    | {dictionary, module()}
    | {module, app_module()}
    | {state, any()}
    | {call_mutates_state, boolean()}
    | {answer_errors, callback|report|discard}.

-type app_module()
   :: module()
    | nonempty_improper_list(module(), list()).  %% list with module() head

%% Identifier returned by add_transport/2

-type transport_ref()
   :: reference().

%% Options passed to add_transport/2

-type transport_opt()
   :: {transport_module, atom()}
    | {transport_config, any()}
    | {applications, [app_alias()]}
    | {capabilities, [capability()]}
    | {watchdog_timer, 'Unsigned32'() | {module(), atom(), list()}}
    | {reconnect_timer, 'Unsigned32'()}
    | {private, any()}.

%% Predicate passed to remove_transport/2

-type transport_pred()
   :: fun((reference(), connect|listen, list()) -> boolean())
    | fun((reference(), list()) -> boolean())
    | fun((list()) -> boolean())
    | reference()
    | list()
    | {connect|listen, transport_pred()}
    | {atom(), atom(), list()}.

%% Options passed to call/4

-type call_opt()
   :: {extra, list()}
    | {filter, peer_filter()}
    | {timeout, 'Unsigned32'()}
    | detach.
