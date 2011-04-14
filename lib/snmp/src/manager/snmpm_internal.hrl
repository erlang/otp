%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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



