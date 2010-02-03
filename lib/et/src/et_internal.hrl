%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Definition of internal data structures
%%----------------------------------------------------------------------

-define(detail_level_min, 0).
-define(detail_level_max, 100).

-record(filter,	{name, function}).

-define(DEFAULT_FILTER_NAME, all).
-define(DEFAULT_FILTER, #filter{name = ?DEFAULT_FILTER_NAME, function = fun(E) -> E end}).
