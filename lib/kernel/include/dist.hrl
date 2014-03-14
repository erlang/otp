%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2014. All Rights Reserved.
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
%% Distribution capabilities flags (corresponds with dist.h).
%%

-define(DFLAG_PUBLISHED,1).
-define(DFLAG_ATOM_CACHE,2).
-define(DFLAG_EXTENDED_REFERENCES,4).
-define(DFLAG_DIST_MONITOR,8).
-define(DFLAG_FUN_TAGS,16#10).
-define(DFLAG_DIST_MONITOR_NAME,16#20).
-define(DFLAG_HIDDEN_ATOM_CACHE,16#40).
-define(DFLAG_NEW_FUN_TAGS,16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS,16#100).
-define(DFLAG_EXPORT_PTR_TAG,16#200).
-define(DFLAG_BIT_BINARIES,16#400).
-define(DFLAG_NEW_FLOATS,16#800).
-define(DFLAG_UNICODE_IO,16#1000).
-define(DFLAG_DIST_HDR_ATOM_CACHE,16#2000).
-define(DFLAG_SMALL_ATOM_TAGS, 16#4000).
-define(DFLAG_UTF8_ATOMS, 16#10000).
-define(DFLAG_MAP_TAG, 16#20000).
