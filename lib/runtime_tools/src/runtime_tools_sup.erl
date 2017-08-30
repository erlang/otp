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
%% ------------------------------------------------------------------------------
%% File    : runtime_tools_sup.erl
%% Author  : Lennart Öhman <lennart.ohman@st.se>

-module(runtime_tools_sup).
-behaviour(supervisor).

-export([init/1]).


%% =============================================================================
%% Callback functions for the runtime_tools_sup supervisor
%% =============================================================================

%% The runtime tools top most supervisor starts:
%% -The ttb_autostart component. This is used for tracing at startup
%%  using observer/ttb.
init(_AutoModArgs) ->
    Flags = {one_for_one, 0, 3600},
    Children = [{ttb_autostart, {ttb_autostart, start_link, []},
                 temporary, 3000, worker, [ttb_autostart]}],
    {ok, {Flags, Children}}.
%% -----------------------------------------------------------------------------
