%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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

-ifndef(snmpa_internal).
-define(snmpa_internal, true).

-include_lib("snmp/src/app/snmp_internal.hrl").

%% The DEFAULT_LOCAL_ENGINE_ID macro can only be used by the master_agent!!
-define(DEFAULT_LOCAL_ENGINE_ID,  snmp_framework_mib:get_engine_id()). 
-define(DEFAULT_NOTIF_EXTRA_INFO, {snmpa_default_notification_extra_info}).

%% -- Max number of VBs in a Get-BULK response --
%% (( The default value, 1000, is *way* more   ))
%% (( then there is room for in a normal pdu   ))
%% (( (unless the max pdu size has been        ))
%% (( cranked way up), so this value should    ))
%% (( suffice as "infinity" without actually   ))
%% (( causing memory issues for the VM ...     ))
-define(DEFAULT_GB_MAX_VBS, 1000).

-define(snmpa_info(F, A),    ?snmp_info("agent",    F, A)).
-define(snmpa_warning(F, A), ?snmp_warning("agent", F, A)).
-define(snmpa_error(F, A),   ?snmp_error("agent",   F, A)).

-endif. % -ifdef(snmpa_internal).



