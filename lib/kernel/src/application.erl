%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(application).

-export([start/1, start/2, start_boot/1, start_boot/2, stop/1, 
	 load/1, load/2, unload/1, takeover/2,
	 which_applications/0, which_applications/1,
	 loaded_applications/0, permit/2]).
-export([set_env/3, set_env/4, unset_env/2, unset_env/3]).
-export([get_env/1, get_env/2, get_all_env/0, get_all_env/1]).
-export([get_key/1, get_key/2, get_all_key/0, get_all_key/1]).
-export([get_application/0, get_application/1, info/0]).
-export([start_type/0]).

-export([behaviour_info/1]).

%%%-----------------------------------------------------------------

-type restart_type() :: 'permanent' | 'transient' | 'temporary'.
-type application_opt() :: {'description', string()}
                         | {'vsn', string()}
                         | {'id', string()}
                         | {'modules', [atom() | {atom(), any()}]} 
                         | {'registered', [atom()]}
                         | {'applications', [atom()]}
                         | {'included_applications', [atom()]}
                         | {'env', [{atom(), any()}]}
                         | {'start_phases', [{atom(), any()}] | 'undefined'}
                         | {'maxT', timeout()}                % max timeout
                         | {'maxP', integer() | 'infinity'}   % max processes
                         | {'mod', {atom(), any()}}.
-type application_spec() :: {'application', atom(), [application_opt()]}.

%%------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [{start,2},{stop,1}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------
%%% This module is API towards application_controller and
%%% application_master.
%%%-----------------------------------------------------------------

-spec load(Application :: atom() | application_spec()) ->
	     'ok' | {'error', term()}.

load(Application) ->
    load(Application, []).

-spec load(Application :: atom() | application_spec(),
	   Distributed :: any()) -> 'ok' | {'error', term()}.

load(Application, DistNodes) ->
    case application_controller:load_application(Application) of
	ok when DistNodes =/= [] ->
	    AppName = get_appl_name(Application),
	    case dist_ac:load_application(AppName, DistNodes) of
		ok ->
		    ok;
		{error, R} ->
		    application_controller:unload_application(AppName),
		    {error, R}
	    end;
	Else ->
	    Else
    end.

-spec unload(Application :: atom()) -> 'ok' | {'error', term()}.

unload(Application) ->
    application_controller:unload_application(Application).

-spec start(Application :: atom()) -> 'ok' | {'error', term()}.

start(Application) ->
    start(Application, temporary).

-spec start(Application :: atom() | application_spec(),
	    RestartType :: restart_type()) -> any().

start(Application, RestartType) ->
    case load(Application) of
	ok ->
	    Name = get_appl_name(Application),
	    application_controller:start_application(Name, RestartType);
	{error, {already_loaded, Name}} ->
	    application_controller:start_application(Name, RestartType);
	Error ->
	    Error
    end.

-spec start_boot(Application :: atom()) -> 'ok' | {'error', term()}.

start_boot(Application) ->
    start_boot(Application, temporary).

-spec start_boot(Application :: atom(), RestartType :: restart_type()) ->
	     'ok' | {'error', term()}.

start_boot(Application, RestartType) ->
    application_controller:start_boot_application(Application, RestartType).

-spec takeover(Application :: atom(), RestartType :: restart_type()) -> any().

takeover(Application, RestartType) ->
    dist_ac:takeover_application(Application, RestartType).

-spec permit(Application :: atom(), Bool :: boolean()) -> 'ok' | {'error', term()}.

permit(Application, Bool) ->
    case Bool of
	true -> ok;
	false -> ok;
	Bad -> exit({badarg, {?MODULE, permit, [Application, Bad]}})
    end,
    case application_controller:permit_application(Application, Bool) of
	distributed_application ->
	    dist_ac:permit_application(Application, Bool);
	{distributed_application, only_loaded} ->
	    dist_ac:permit_only_loaded_application(Application, Bool);
	LocalResult ->
	    LocalResult
    end.

-spec stop(Application :: atom()) -> 'ok' | {'error', term()}.

stop(Application) ->
    application_controller:stop_application(Application).

-spec which_applications() -> [{atom(), string(), string()}].

which_applications() ->
    application_controller:which_applications().

-spec which_applications(timeout()) -> [{atom(), string(), string()}].

which_applications(infinity) ->
    application_controller:which_applications(infinity);
which_applications(Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:which_applications(Timeout).

-spec loaded_applications() -> [{atom(), string(), string()}].

loaded_applications() -> 
    application_controller:loaded_applications().

-spec info() -> any().

info() -> 
    application_controller:info().

-spec set_env(Application :: atom(), Key :: atom(), Value :: any()) -> 'ok'.

set_env(Application, Key, Val) -> 
    application_controller:set_env(Application, Key, Val).

-spec set_env(Application :: atom(), Key :: atom(),
	      Value :: any(), Timeout :: timeout()) -> 'ok'.

set_env(Application, Key, Val, infinity) ->
    application_controller:set_env(Application, Key, Val, infinity);
set_env(Application, Key, Val, Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:set_env(Application, Key, Val, Timeout).

-spec unset_env(atom(), atom()) -> 'ok'.

unset_env(Application, Key) -> 
    application_controller:unset_env(Application, Key).

-spec unset_env(atom(), atom(), timeout()) -> 'ok'.

unset_env(Application, Key, infinity) ->
    application_controller:unset_env(Application, Key, infinity);
unset_env(Application, Key, Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:unset_env(Application, Key, Timeout).

-spec get_env(atom()) -> 'undefined' | {'ok', term()}.

get_env(Key) -> 
    application_controller:get_pid_env(group_leader(), Key).

-spec get_env(atom(), atom()) -> 'undefined' | {'ok', term()}.

get_env(Application, Key) -> 
    application_controller:get_env(Application, Key).

-spec get_all_env() -> [] | [{atom(), any()}].

get_all_env() -> 
    application_controller:get_pid_all_env(group_leader()).

-spec get_all_env(atom()) -> [] | [{atom(), any()}].

get_all_env(Application) -> 
    application_controller:get_all_env(Application).

-spec get_key(atom()) -> 'undefined' | {'ok', term()}.

get_key(Key) -> 
    application_controller:get_pid_key(group_leader(), Key).

-spec get_key(atom(), atom()) -> 'undefined' | {'ok', term()}.

get_key(Application, Key) -> 
    application_controller:get_key(Application, Key).

-spec get_all_key() -> 'undefined' | [] | {'ok', [{atom(),any()},...]}.

get_all_key() ->
    application_controller:get_pid_all_key(group_leader()).

-spec get_all_key(atom()) -> 'undefined' | {'ok', [{atom(),any()},...]}.

get_all_key(Application) -> 
    application_controller:get_all_key(Application).

-spec get_application() -> 'undefined' | {'ok', atom()}.

get_application() -> 
    application_controller:get_application(group_leader()).

-spec get_application(Pid :: pid()) -> 'undefined' | {'ok', atom()}
		   ; (Module :: atom()) -> 'undefined' | {'ok', atom()}.

get_application(Pid) when is_pid(Pid) ->
    case process_info(Pid, group_leader) of
	{group_leader, Gl} ->
	    application_controller:get_application(Gl);
	undefined ->
	    undefined
    end;
get_application(Module) when is_atom(Module) ->
    application_controller:get_application_module(Module).

-spec start_type() -> 'undefined' | 'local' | 'normal'
		    | {'takeover', node()} | {'failover', node()}.

start_type() ->
    application_controller:start_type(group_leader()).

%% Internal
get_appl_name(Name) when is_atom(Name) -> Name;
get_appl_name({application, Name, _}) when is_atom(Name) -> Name.
