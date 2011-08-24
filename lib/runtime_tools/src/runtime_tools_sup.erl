%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
%% -The inviso runtime component. This is the only way to get the runtime component
%%  started automatically (if for instance autostart is wanted).
%%  Note that it is not impossible that the runtime component terminates it self
%%  should it discover that no autostart is configured.
init(AutoModArgs) ->
    Flags = {one_for_one, 0, 3600},
    Children = [{inviso_rt, {inviso_rt, start_link_auto, [AutoModArgs]}, 
		 temporary, 3000, worker, [inviso_rt]},
                {ttb_autostart, {ttb_autostart, start_link, []},
                 temporary, 3000, worker, [ttb_autostart]}],
    {ok, {Flags, Children}}.
%% -----------------------------------------------------------------------------
