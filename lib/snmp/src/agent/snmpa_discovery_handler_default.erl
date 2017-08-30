%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(snmpa_discovery_handler_default).

-behaviour(snmpa_discovery_handler).


%%%-----------------------------------------------------------------
%%% Implements different error mechanisms.
%%%-----------------------------------------------------------------
-export([stage1_finish/3]).


%%-----------------------------------------------------------------
%% This function is called at the end of stage 1 of the discovery
%% process. If security-level is noAuthNoPriv, then 
%% 
%% 
%%-----------------------------------------------------------------
stage1_finish(_TargetName, _ManagerEngineID, _ExtraInfo) ->
    ignore.

