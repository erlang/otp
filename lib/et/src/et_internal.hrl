%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Definition of internal data structures
%%----------------------------------------------------------------------

-define(detail_level_min, 0).
-define(detail_level_max, 100).

-record(filter,	{name, function}).

-define(DEFAULT_FILTER_NAME, all).
-define(DEFAULT_FILTER, #filter{name = ?DEFAULT_FILTER_NAME, function = fun(E) -> E end}).
