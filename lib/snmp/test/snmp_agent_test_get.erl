%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(snmp_agent_test_get).

-behaviour(snmpa_get_mechanism).


%%%-----------------------------------------------------------------
%%% snmpa_get_mechanism exports
%%%-----------------------------------------------------------------

-export([
         do_get/3, do_get/4,
         do_get_next/3,
         do_get_bulk/7
        ]).



do_get(UnsortedVarbinds, IsNotification, Extra) ->
    snmpa_get:do_get(UnsortedVarbinds, IsNotification, Extra).



do_get(MibView, UnsortedVarbinds, IsNotification, Extra) ->
    snmpa_get:do_get(MibView, UnsortedVarbinds, IsNotification, Extra).



do_get_next(MibView, UnsortedVBs, Extra) ->
    snmpa_get:do_get_next(MibView, UnsortedVBs, Extra).




do_get_bulk(MibView, NonRepeaters, MaxRepetitions,
            PduMS, Varbinds, GbMaxVBs, Extra) ->
    snmpa_get:do_get_bulk(MibView, NonRepeaters, MaxRepetitions,
                          PduMS, Varbinds, GbMaxVBs,
                          Extra).
