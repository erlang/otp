%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

