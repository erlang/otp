%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
%%
%% %CopyrightEnd%
-module(t).

-export([t/0, u/0]).

-ignore_xref(lib0).
-ignore_xref(u/0).
-ignore_xref([{lib2,unknown_ignored,0}]).

t() ->
    %% lib0: nonexisting, ignored
    %% lib1: only unknown functions used
    %% lib2: one known used, one unknown function used, one local used
    %% lib3: one known function used
    lib0:unknown(),
    lib1:unknown(),
    lib2:f(), %% known, g/0 not used
    lib2:unknown_ignored(),
    lib2:unknown(),
    lib2:local(),
    lib3:f(),
    unknown:unknown().

u() ->
    true.
