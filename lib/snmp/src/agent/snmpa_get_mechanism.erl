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

-module(snmpa_get_mechanism).

%%
%% This module defines the behaviour for the undocumented (hidden)
%% get-mechanism feature. This allows for implementing your own
%% handling of get, get-next and get-bulk requests.
%% Probably only useful for special cases (optimization).
%%



%% ----------- do_get/3 -----------------------------------------------------

-callback do_get(MibView        :: snmp_view_based_acm_mib:mibview(), 
                 VBs            :: [snmp:varbind()],
                 IsNotification :: boolean()) ->
    {noError, 0, ResVBs :: [snmp:varbind()]} |
    {ErrStatus :: snmp:error_status(), ErrIndex :: snmp:error_index(), []}.


%% ----------- do_get_next/2 ------------------------------------------------

-callback do_get_next(MibView :: snmp_view_based_acm_mib:mibview(),
                      VBs     :: [snmp:varbind()]) ->
    {noError, 0, ResVBs :: [snmp:varbind()]} |
    {ErrStatus :: snmp:error_status(), ErrIndex :: snmp:error_index(), []}.


%% ----------- do_get_bulk/6 ------------------------------------------------

-callback do_get_bulk(MibView        :: snmp_view_based_acm_mib:mibview(),
                      NonRepeaters   :: non_neg_integer(),
                      MaxRepetitions :: non_neg_integer(),
                      PduMS          :: pos_integer(),
                      VBs            :: [snmp:varbind()],
                      MaxVBs         :: pos_integer()) ->
    {noError, 0, ResVBs :: [snmp:varbind()]} |
    {ErrStatus :: snmp:error_status(), ErrIndex :: snmp:error_index(), []}.
