%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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

-define(DFLAG_PUBLISHED,                 16#01).
-define(DFLAG_ATOM_CACHE,                16#02).
-define(DFLAG_EXTENDED_REFERENCES,       16#04).
-define(DFLAG_DIST_MONITOR,              16#08).
-define(DFLAG_FUN_TAGS,                  16#10).
-define(DFLAG_DIST_MONITOR_NAME,         16#20).
-define(DFLAG_HIDDEN_ATOM_CACHE,         16#40).
-define(DFLAG_NEW_FUN_TAGS,              16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS,      16#100).
-define(DFLAG_EXPORT_PTR_TAG,           16#200).
-define(DFLAG_BIT_BINARIES,             16#400).
-define(DFLAG_NEW_FLOATS,               16#800).
-define(DFLAG_UNICODE_IO,              16#1000).
-define(DFLAG_DIST_HDR_ATOM_CACHE,     16#2000).
-define(DFLAG_SMALL_ATOM_TAGS,         16#4000).
-define(DFLAG_UTF8_ATOMS,             16#10000).
-define(DFLAG_MAP_TAG,                16#20000).
-define(DFLAG_BIG_CREATION,           16#40000).
-define(DFLAG_SEND_SENDER,            16#80000).
-define(DFLAG_BIG_SEQTRACE_LABELS,   16#100000).
%% -define(DFLAG_NO_MAGIC,           16#200000). %% Used internally only
-define(DFLAG_EXIT_PAYLOAD,          16#400000).
-define(DFLAG_FRAGMENTS,           16#00800000).
-define(DFLAG_HANDSHAKE_23,        16#01000000).
-define(DFLAG_UNLINK_ID,           16#02000000).
-define(DFLAG_MANDATORY_25_DIGEST, 16#04000000).
-define(DFLAG_RESERVED,            16#f8000000).

%% Second 32-bit flag word.
-define(DFLAG_SPAWN,        (16#01 bsl 32)).
-define(DFLAG_NAME_ME,      (16#02 bsl 32)).
-define(DFLAG_V4_NC,        (16#04 bsl 32)).
-define(DFLAG_ALIAS,        (16#08 bsl 32)).
-define(DFLAG_LOCAL_EXT,    (16#10 bsl 32)). %% only used internally
-define(DFLAG_ALTACT_SIG,   (16#20 bsl 32)).

%% The following flags are mandatory in OTP 25. OTP 25 and higher
%% will accept ?DFLAG_MANDATORY_25_DIGEST as a shorthand for all those
%% flags.
-define(MANDATORY_DFLAGS_25,
        (?DFLAG_EXTENDED_REFERENCES bor
             ?DFLAG_FUN_TAGS bor
             ?DFLAG_EXTENDED_PIDS_PORTS bor
             ?DFLAG_NEW_FUN_TAGS bor
             ?DFLAG_EXPORT_PTR_TAG bor
             ?DFLAG_BIT_BINARIES bor
             ?DFLAG_NEW_FLOATS bor
             ?DFLAG_UTF8_ATOMS bor
             ?DFLAG_MAP_TAG bor
             ?DFLAG_BIG_CREATION bor
             ?DFLAG_HANDSHAKE_23)).

%% New mandatory flags in OTP 26
-define(MANDATORY_DFLAGS_26, (?DFLAG_V4_NC bor
                                  ?DFLAG_UNLINK_ID)).

%% All mandatory flags
-define(DFLAGS_MANDATORY, (?MANDATORY_DFLAGS_25 bor
                               ?MANDATORY_DFLAGS_26)).

%% Also update dflag2str() in ../src/dist_util.erl
%% when adding flags...

-define(ERL_DIST_VER_6, 6).  % OTP-23 at least

-define(ERL_DIST_VER_LOW, ?ERL_DIST_VER_6).
-define(ERL_DIST_VER_HIGH, ?ERL_DIST_VER_6).

%%%
%%% To avoid having to extend the number of distribution flags from 64
%%% to 128, a scheme for garbage collection of the flags was
%%% introduced in OTP 25.
%%%
%%% In OTP 25, ?DFLAG_MANDATORY_25_DIGEST was introduced as a synonym
%%% for the flags defined by ?MANDATORY_DFLAGS_25. OTP 25/26 will
%%% accept the old flags to support communication with 24 and earlier,
%%% as well as ?DFLAG_MANDATORY_25_DIGEST.
%%%
%%% OTP 27 will make ?DFLAG_MANDATORY_25_DIGEST mandatory, meaning that an
%%% OTP 27 node can only communicate with OTP 25 and higher.
%%%
%%% An OTP 27 node will also introduce the new flag
%%% ?DFLAG_MANDATORY_27_DIGEST:
%%%
%%% * If ?DFLAG_MANDATORY_27_DIGEST is set, it means that all bit
%%%   numbers defined by ?MANDATORY_DFLAGS_25, as well as the bit
%%%   number defined by ?DFLAG_MANDATORY_25_DIGEST and any other bits
%%%   made mandatory in OTP 26/27, lose their previous meanings. New
%%%   meanings can then be assigned to those bit numbers as
%%%   needed. (This is for communication between nodes from OTP 27 and
%%%   up.)
%%%
%%% * If ?DFLAG_MANDATORY_27_DIGEST is not set, then
%%%   ?DFLAG_MANDATORY_25_DIGEST must be set and all bit numbers
%%%   defined by ?MANDATORY_DFLAGS_25 are ignored. (This is for
%%%   communication between an OTP 27 node and an OTP 25/26 node.)
%%%
