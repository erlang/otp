%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-define(DFLAG_BIG_CREATION, 16#40000).
-define(DFLAG_SEND_SENDER, 16#80000).

%% DFLAGs that require strict ordering or:ed together...
-define(DFLAGS_STRICT_ORDER_DELIVERY,
        ?DFLAG_DIST_HDR_ATOM_CACHE).


%% Also update dflag2str() in ../src/dist_util.erl
%% when adding flags...

-define(DFLAGS_ALL,
        (?DFLAG_PUBLISHED
             bor ?DFLAG_ATOM_CACHE
             bor ?DFLAG_EXTENDED_REFERENCES
             bor ?DFLAG_DIST_MONITOR
             bor ?DFLAG_FUN_TAGS
             bor ?DFLAG_DIST_MONITOR_NAME
             bor ?DFLAG_HIDDEN_ATOM_CACHE
             bor ?DFLAG_NEW_FUN_TAGS
             bor ?DFLAG_EXTENDED_PIDS_PORTS
             bor ?DFLAG_EXPORT_PTR_TAG
             bor ?DFLAG_BIT_BINARIES
             bor ?DFLAG_NEW_FLOATS
             bor ?DFLAG_UNICODE_IO
             bor ?DFLAG_DIST_HDR_ATOM_CACHE
             bor ?DFLAG_SMALL_ATOM_TAGS
             bor ?DFLAG_UTF8_ATOMS
             bor ?DFLAG_MAP_TAG
             bor ?DFLAG_BIG_CREATION
             bor ?DFLAG_SEND_SENDER)).
