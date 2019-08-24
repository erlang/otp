%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-ifndef(snmpm_internal).
-define(snmpm_internal, true).

-define(DEFAULT_CONTEXT, "").
-define(SNMPM_EXTRA_INFO_TAG, snmpm_extra_info_tag).
-define(DEFAULT_EXTRA_INFO, {?SNMPM_EXTRA_INFO_TAG}).


-include_lib("snmp/src/app/snmp_internal.hrl").

-define(snmpm_info(F, A),    ?snmp_info("manager",    F, A)).
-define(snmpm_warning(F, A), ?snmp_warning("manager", F, A)).
-define(snmpm_error(F, A),   ?snmp_error("manager",   F, A)).

-endif. % -ifdef(snmpm_internal).



