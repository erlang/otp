%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%% ----------------------------------------------------------------------
%% Purpose : Starts applications in the phases defined in the .app file's
%%           start_phases key. If the application includes other applications 
%%           these are also started according to their mod and 
%%           start_phases-keys in their .app file.
%% ----------------------------------------------------------------------

-module(application_starter).

-export([start/3]).

%%%=============================================================================
%%%=============================================================================
%%%=============================================================================
%%% start(Phases, Type, Applications) -> ok | {error, ErrorMessage}
%%%
%%% The applications are started by calling Module:start_phase(Phase, Type, Args) 
%%% where Module and is defined in the mod-key, Phase and Args are defined in
%%% the start_phases-key.
%%%=============================================================================
%%%=============================================================================
%%%=============================================================================
start([], _Type, _Apps) ->
    ok;
start([{Phase,_PhaseArgs}|Phases], Type, Apps) ->
    case start_apps(Phase, Type, Apps) of
	{error, _} = Error ->
	    Error;
	_ ->
	    start(Phases, Type, Apps)
    end.


%%%=============================================================================
%%% Start each application in the phase Phase. 
%%%=============================================================================
start_apps(_Phase, _Type, []) ->
    ok;
start_apps(Phase, Type, [App | Apps]) ->
    case catch run_start_phase(Phase, Type, App) of
	{error, _} = Error ->
	    Error;
	_ ->
	    start_apps(Phase, Type, Apps)
    end.


%%%=============================================================================
%%% If application_starter is used recursively, start also all the included
%%% applications in the phase Phase. 
%%%=============================================================================
run_start_phase(Phase, Type, App) ->
    {ok,{Mod,Arg}} = application:get_key(App, mod),
    case Mod of
	application_starter ->
	    [StartMod, _StartArgs] = Arg,
	    run_the_phase(Phase, Type, App, StartMod),
	    {ok, IncApps} = application:get_key(App, included_applications),
	    start_apps(Phase, Type, IncApps);
	_ ->
	    run_the_phase(Phase, Type, App, Mod)
    end.


%%%=============================================================================
%%% Start the application only if the start phase is defined in the 
%%% start_phases-key. 
%%%=============================================================================
run_the_phase(Phase, Type, App, Mod) ->
    Start_phases = case application_controller:get_key(App, start_phases) of
		       {ok, undefined} ->
			   throw({error, {start_phases_undefined, App}});
		       {ok, Sp} ->
			   Sp
		   end,
    case lists:keyfind(Phase, 1, Start_phases) of
	false ->
	    ok;
	{Phase, PhaseArgs} ->
	    case catch Mod:start_phase(Phase, Type, PhaseArgs) of
		ok ->
		    ok;
		{error, Reason} ->
		    throw({error, {Reason, 
				   {Mod, start_phase, 
				    [Phase, Type, PhaseArgs]}}});
		Other ->
		    throw({error, {bad_return_value, 
				   {{Mod, start_phase, 
				     [Phase, Type, PhaseArgs]}, 
				    Other}}})
	    end
    end.
