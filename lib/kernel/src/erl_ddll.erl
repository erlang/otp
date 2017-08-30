%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% Dynamic Driver Loader and Linker
%%
%% Interface for dynamic library/shared object driver loader/linker.
%% Provides methods for loading, unloading and listing drivers.

-module(erl_ddll).

-export([load_driver/2, load/2, 
	 unload_driver/1, unload/1, reload/2, reload_driver/2, 
	 format_error/1,info/1,info/0, start/0, stop/0]).

%%----------------------------------------------------------------------------

-type path()   :: string() | atom().
-type driver() :: iolist() | atom().

%%----------------------------------------------------------------------------
%%% BIFs

-export([demonitor/1, info/2, format_error_int/1, monitor/2,
         try_load/3, try_unload/2, loaded_drivers/0]).

-spec demonitor(MonitorRef) -> ok when
      MonitorRef :: reference().

demonitor(_) ->
    erlang:nif_error(undef).

-spec info(Name, Tag) -> Value when
      Name :: driver(),
      Tag :: processes | driver_options | port_count | linked_in_driver
           | permanent | awaiting_load | awaiting_unload,
      Value :: term().

info(_, _) ->
    erlang:nif_error(undef).

-spec format_error_int(ErrSpec) -> string() when
      ErrSpec :: term().

format_error_int(_) ->
    erlang:nif_error(undef).

-spec monitor(Tag, Item) -> MonitorRef when
      Tag :: driver,
      Item :: {Name, When},
      Name :: driver(),
      When :: loaded | unloaded | unloaded_only,
      MonitorRef :: reference().

monitor(_, _) ->
    erlang:nif_error(undef).

-spec try_load(Path, Name, OptionList) ->
                      {ok,Status} |
                      {ok, PendingStatus, Ref} |
                      {error, ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      OptionList :: [Option],
      Option :: {driver_options, DriverOptionList}
              | {monitor, MonitorOption}
              | {reload, ReloadOption},
      DriverOptionList :: [DriverOption],
      DriverOption :: kill_ports,
      MonitorOption :: pending_driver | pending,
      ReloadOption :: pending_driver | pending,
      Status :: loaded | already_loaded | PendingStatus,
      PendingStatus :: pending_driver | pending_process,
      Ref :: reference(),
      ErrorDesc :: ErrorAtom | OpaqueError,
      ErrorAtom :: linked_in_driver | inconsistent | permanent
                 | not_loaded_by_this_process | not_loaded
                 |  pending_reload | pending_process,
      OpaqueError :: term().

try_load(_, _, _) ->
    erlang:nif_error(undef).

-spec try_unload(Name, OptionList) ->
                        {ok, Status} |
                        {ok, PendingStatus, Ref} |
                        {error, ErrorAtom} when
      Name :: driver(),
      OptionList :: [Option],
      Option :: {monitor, MonitorOption} | kill_ports,
      MonitorOption :: pending_driver | pending,
      Status :: unloaded | PendingStatus,
      PendingStatus :: pending_driver | pending_process,
      Ref :: reference(),
      ErrorAtom :: linked_in_driver | not_loaded |
                   not_loaded_by_this_process | permanent.

try_unload(_, _) ->
    erlang:nif_error(undef).

-spec loaded_drivers() -> {ok, Drivers} when
      Drivers :: [Driver],
      Driver :: string().

loaded_drivers() ->
    erlang:nif_error(undef).

%%% End of BIFs


-spec start() -> {'error', {'already_started', 'undefined'}}.

start() ->
    {error, {already_started,undefined}}.

-spec stop() -> 'ok'.

stop() ->
    ok.

-spec load_driver(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: term().

load_driver(Path, Driver) ->
    do_load_driver(Path, Driver, [{driver_options,[kill_ports]}]).

-spec load(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc ::term().

load(Path, Driver) ->
    do_load_driver(Path, Driver, []).

do_load_driver(Path, Driver, DriverFlags) ->
    case erl_ddll:try_load(Path, Driver,[{monitor,pending_driver}]++DriverFlags) of
	{error, inconsistent} ->
	    {error,bad_driver_name}; % BC 
	{error, What} ->
	    {error,What};
	{ok, already_loaded} ->
	    ok;
	{ok,loaded} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'DOWN', Ref, driver, _, load_cancelled} ->
		    {error, load_cancelled};
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'DOWN', Ref, driver, _, {load_failure, Failure}} ->
		    {error, Failure};
		{'UP', Ref, driver, _, loaded} ->
		    ok
	    end
    end.

do_unload_driver(Driver,Flags) ->
    case erl_ddll:try_unload(Driver,Flags) of
	{error,What} ->
	    {error,What};
	{ok, pending_process} ->
	    ok;
	{ok, unloaded} ->
	    ok;
	{ok, pending_driver} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'UP', Ref, driver, _, unload_cancelled} ->
		    ok;
		{'DOWN', Ref, driver, _, unloaded} ->
		    ok
	    end
    end.

-spec unload_driver(Name) -> 'ok' | {'error', ErrorDesc} when
      Name :: driver(),
      ErrorDesc :: term().

unload_driver(Driver) ->
    do_unload_driver(Driver,[{monitor,pending_driver},kill_ports]).

-spec unload(Name) -> 'ok' | {'error', ErrorDesc} when
      Name :: driver(),
      ErrorDesc :: term().

unload(Driver) ->
    do_unload_driver(Driver,[]).

-spec reload(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: pending_process | OpaqueError,
      OpaqueError :: term().

reload(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver}]).

-spec reload_driver(Path, Name) -> 'ok' | {'error', ErrorDesc} when
      Path :: path(),
      Name :: driver(),
      ErrorDesc :: pending_process | OpaqueError,
      OpaqueError :: term().

reload_driver(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver},
				  {driver_options,[kill_ports]}]).			    

-spec format_error(ErrorDesc) -> string() when
      ErrorDesc :: term().

format_error(Code) ->
    case Code of
	%% This is the only error code returned only from erlang code...
	%% 'permanent' has a translation in the emulator, even though the erlang code uses it to...
	load_cancelled ->
	    "Loading was cancelled from other process";
	_ ->
	    erl_ddll:format_error_int(Code)
    end.

-spec info(Name) -> InfoList when
      Name :: driver(),
      InfoList :: [InfoItem, ...],
      InfoItem :: {Tag :: atom(), Value :: term()}.
 
info(Driver) ->
    [{processes, erl_ddll:info(Driver,processes)},
     {driver_options, erl_ddll:info(Driver,driver_options)},
     {port_count, erl_ddll:info(Driver,port_count)},
     {linked_in_driver, erl_ddll:info(Driver,linked_in_driver)},
     {permanent, erl_ddll:info(Driver,permanent)},
     {awaiting_load,  erl_ddll:info(Driver,awaiting_load)},
     {awaiting_unload, erl_ddll:info(Driver,awaiting_unload)}].

-spec info() -> AllInfoList when
      AllInfoList :: [DriverInfo],
      DriverInfo :: {DriverName, InfoList},
      DriverName :: string(),
      InfoList :: [InfoItem],
      InfoItem :: {Tag :: atom(), Value :: term()}.

info() ->
    {ok,DriverList} = erl_ddll:loaded_drivers(),
    [{X,Y} || X <- DriverList,
	       Y <- [catch info(X)],
	       is_list(Y), not lists:member({linked_in_driver,true},Y)]. 
