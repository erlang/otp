%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc """
Behaviour module for the SNMP agent notification delivery information receiver.

This module defines the behaviour of the notification delivery information
receiver.

A `snmpa_notification_delivery_info_receiver` compliant module must export the
following functions:

- `c:delivery_targets/3`
- `c:delivery_info/4`

The semantics of them and their exact signatures are explained below.

Legacy notification delivery information receiver modules used a target argument
on the form `{IpAddr, PortNumber}` instead of `{Domain, Addr}`, and if the SNMP
Agent is run without changing the configuration to use transport domains the
notification delivery information receiver will still get the old arguments and
work as before.

## DATA TYPES

See the [data types in `snmpa_conf`](`m:snmpa_conf#types`).

[](){: #accept_recv } [](){: #delivery_targets }
""".

-export([verify/1]).

-type transportDomain() :: snmpa_conf:transportDomain().
-type transportAddressWithPort() :: snmpa_conf:transportAddressWithPort().


-doc """
Inform about delivery result.

This function is called for each target in the `Targets` argument of the
[`delivery_targets/3`](`c:delivery_targets/3`) function, see above.

The purpose is to inform the `receiver` of the result of the delivery (was the
notification acknowledged or not) for each target.
""".
-callback delivery_info(Tag, Targets, DeliveryResult, Extra) -> snmp:void() when
      Tag :: term(),
      Targets :: [Target],
      Target ::
        {transportDomain(),
         transportAddressWithPort()},
      DeliveryResult ::
          no_response | got_response,
      Extra :: term().
-doc """
Inform about target addresses.

This is the first function called when a notification delivery is in progress.
It informs the `receiver` which targets will get the notification. The result of
the delivery will be provided via successive calls to
[`delivery_info/4`](`c:delivery_info/4`) function, see below.

[](){: #delivery_info }
""".
-callback delivery_targets(Tag, Targets, Extra) -> snmp:void() when
      Tag :: term(),
      Targets :: [Target],
      Target ::
        {transportDomain(),
         transportAddressWithPort()},
      Extra :: term().

-doc false.
verify(Module) ->
    snmp_misc:verify_behaviour(?MODULE, Module).

