%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(snmpa_notification_delivery_info_receiver).

-export([verify/1]).

-type transportDomain() :: snmpa_conf:transportDomain().
-type transportAddressWithPort() :: snmpa_conf:transportAddressWithPort().


-callback delivery_info(Tag, Targets, DeliveryResult, Extra) -> snmp:void() when
      Tag :: term(),
      Targets :: [Target],
      Target ::
        {transportDomain(),
         transportAddressWithPort()},
      DeliveryResult ::
          no_response | got_response,
      Extra :: term().
-callback delivery_targets(Tag, Targets, Extra) -> snmp:void() when
      Tag :: term(),
      Targets :: [Target],
      Target ::
        {transportDomain(),
         transportAddressWithPort()},
      Extra :: term().

verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).

