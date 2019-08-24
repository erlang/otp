%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-module(httpc_profile_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/1, restart_child/1, stop_child/1]).

%% Supervisor callback
-export([init/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpcServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpcServices]).

start_child(PropList) ->
    case proplists:get_value(profile, PropList) of
	undefined ->
	    {error, no_profile};
	Profile ->
	    Dir = 
		proplists:get_value(data_dir, PropList, only_session_cookies),
	    Spec = httpc_child_spec(Profile, Dir),
	    supervisor:start_child(?MODULE, Spec)
    end.
  
restart_child(Profile) ->
    Name = id(Profile),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:restart_child(?MODULE, Name);
        Error ->
            Error
    end.
    
stop_child(Profile) ->
   Name = id(Profile),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.

id(Profile) ->
    DefaultProfile = httpc:default_profile(),
    case Profile of
	DefaultProfile ->
	    httpc_manager;
	_ ->
	    {httpc, Profile}
    end.


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    init([[]]);
init([HttpcServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_spec(HttpcServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

child_spec([], Acc) ->
    Acc;
%% For backwards compatibility
child_spec([{httpc, {Profile, Dir}} | Rest], Acc) ->
    Spec = httpc_child_spec(Profile, Dir),
    child_spec(Rest, [Spec | Acc]);
child_spec([{httpc, PropList} | Rest], Acc) when is_list(PropList) ->
    Profile = proplists:get_value(profile, PropList),
    Dir = proplists:get_value(data_dir, PropList),
    Spec = httpc_child_spec(Profile, Dir),
    child_spec(Rest, [Spec | Acc]).

httpc_child_spec(Profile, Dir) ->
    Name = id(Profile),
    StartFunc = {httpc_manager, start_link, [Profile, Dir, inets]},
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [httpc_manager],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

