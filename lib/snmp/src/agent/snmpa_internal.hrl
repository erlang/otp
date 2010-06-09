%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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

-define(DEFAULT_LOCAL_ENGINE_ID, snmp_framework_mib:get_engine_id()).

-define(snmpa_info(F, A),    ?snmp_info("agent",    F, A)).
-define(snmpa_warning(F, A), ?snmp_warning("agent", F, A)).
-define(snmpa_error(F, A),   ?snmp_error("agent",   F, A)).

-endif. % -ifdef(snmpa_internal).



