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
         service_info/2]).

%% Start/stop the application. In a "real" application this should
%% typically be a consequence of specifying diameter in a release file
%% rather than by calling start/stop explicitly.
-export([start/0,
         stop/0]).

-include("diameter_internal.hrl").
-include("diameter_types.hrl").

%%% --------------------------------------------------------------------------
%%% start/0
%%% --------------------------------------------------------------------------

-spec start()
   -> ok
    | {error, term()}.

start() ->
    application:start(?APPLICATION).

%%% --------------------------------------------------------------------------
%%% stop/0
%%% --------------------------------------------------------------------------

-spec stop()
   -> ok
    | {error, term()}.

stop() ->
    application:stop(?APPLICATION).

%%% --------------------------------------------------------------------------
%%% start_service/2
%%% --------------------------------------------------------------------------

-spec start_service(service_name(), [service_opt()])
   -> ok
    | {error, term()}.

start_service(SvcName, Opts)
  when is_list(Opts) ->
    diameter_config:start_service(SvcName, Opts).

%%% --------------------------------------------------------------------------
%%% stop_service/1
%%% --------------------------------------------------------------------------

-spec stop_service(service_name())
   -> ok
    | {error, term()}.

stop_service(SvcName) ->
    diameter_config:stop_service(SvcName).

%%% --------------------------------------------------------------------------
%%% services/0
%%% --------------------------------------------------------------------------

-spec services()
   -> [service_name()].

services() ->
    [Name || {Name, _} <- diameter_service:services()].

%%% --------------------------------------------------------------------------
%%% service_info/2
%%% --------------------------------------------------------------------------

-spec service_info(service_name(), atom() | [atom()])
   -> any().

service_info(SvcName, Option) ->
    diameter_service:info(SvcName, Option).

%%% --------------------------------------------------------------------------
%%% add_transport/3
%%% --------------------------------------------------------------------------

-spec add_transport(service_name(), {listen|connect, [transport_opt()]})
   -> {ok, transport_ref()}
    | {error, term()}.

add_transport(SvcName, {T, Opts} = Cfg)
  when is_list(Opts), (T == connect orelse T == listen) ->
    diameter_config:add_transport(SvcName, Cfg).

%%% --------------------------------------------------------------------------
%%% remove_transport/2
%%% --------------------------------------------------------------------------

-spec remove_transport(service_name(), transport_pred())
   -> ok | {error, term()}.

remove_transport(SvcName, Pred) ->
    diameter_config:remove_transport(SvcName, Pred).

%%% --------------------------------------------------------------------------
%%% # subscribe(SvcName)
%%%
%%% Description: Subscribe to #diameter_event{} messages for the specified
%%%              service.
%%% --------------------------------------------------------------------------

-spec subscribe(service_name())
   -> true.

subscribe(SvcName) ->
    diameter_service:subscribe(SvcName).

%%% --------------------------------------------------------------------------
%%% # unsubscribe(SvcName)
%%% --------------------------------------------------------------------------

-spec unsubscribe(service_name())
   -> true.

unsubscribe(SvcName) ->
    diameter_service:unsubscribe(SvcName).

%%% ----------------------------------------------------------
%%% # session_id/1
%%% ----------------------------------------------------------

-spec session_id('DiameterIdentity'())
   -> 'OctetString'().

session_id(Ident) ->
    diameter_session:session_id(Ident).

%%% ----------------------------------------------------------
%%% # origin_state_id/0
%%% ----------------------------------------------------------

-spec origin_state_id()
   -> 'Unsigned32'().

origin_state_id() ->
    diameter_session:origin_state_id().

%%% --------------------------------------------------------------------------
%%% # call/[34]
%%% --------------------------------------------------------------------------

-spec call(service_name(), app_alias(), any(), [call_opt()])
   -> any().

call(SvcName, App, Message, Options) ->
    diameter_service:call(SvcName, {alias, App}, Message, Options).

call(SvcName, App, Message) ->
    call(SvcName, App, Message, []).
