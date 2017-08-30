%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : cosTime.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(cosTime).
 
%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").

%%--------------- EXPORTS-------------------------------------
%% cosTime API external
-export([start/0, stop/0, 
	 install_time/0, uninstall_time/0,
	 install_timerevent/0, uninstall_timerevent/0, 
	 start_time_service/2, start_timerevent_service/1, stop_timerevent_service/1,
	 stop_time_service/1]).

%% cosTime API internal
-export([create_link/3, get_option/3, type_check/2, start_event_handler/1]).
 
%% Application callbacks
-export([start/2, init/1, stop/1]).

%%--------------- DEFINES ------------------------------------
-define(IDL_TIME_MODULES, ['oe_TimeBase', 
			   'oe_CosTime']).
-define(IDL_TIMEREVENT_MODULES, ['oe_CosTimerEvent']).
-define(SUPERVISOR_NAME, oe_cosTimeSup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).
-define(SUP_TIMESERVICE_SPEC(T,I), 
        ['CosTime_TimeService',[T, I], 
         [{sup_child, true}, {regname, {global, "oe_cosTimeService"}}]]).
-define(SUP_TIMEREVENTSERVICE_SPEC(Args), 
        ['CosTimerEvent_TimerEventService', Args, 
         [{sup_child, true}, {regname, {local, 'oe_cosTimerEventService'}}]]).
-define(SUP_TIMEREVENTHANDLER_SPEC(Name, Args), 
        ['CosTimerEvent_TimerEventHandler',Args, 
         [{sup_child, true}, {regname, {global, Name}}]]).
-define(SUP_CHILD, 
        {"oe_TimeChild",
         {cosTime,create_link, []},
	 transient,100000,worker,
         []}).

%%------------------------------------------------------------
%% function : install_*/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------

install_time() -> 
    case install_loop(?IDL_TIME_MODULES,[]) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

install_timerevent() -> 
    case install_loop(?IDL_TIMEREVENT_MODULES,[]) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

install_loop([], _) ->
    ok;
install_loop([H|T], Accum) ->
    case catch H:'oe_register'() of
	{'EXIT',{unregistered,App}} ->
	    ?write_ErrorMsg("Unable to register '~p'; application ~p not registered.\n"
			    "Trying to unregister ~p\n", [H,App,Accum]),
	    uninstall_loop(Accum, {exit, register});
	{'EXCEPTION',_} ->
	    ?write_ErrorMsg("Unable to register '~p'; propably already registered.\n"
			    "You are adviced to confirm this.\n"
			    "Trying to unregister ~p\n", [H,Accum]),
	    uninstall_loop(Accum, {exit, register});
	ok ->
	    install_loop(T, [H|Accum]);
	_ ->
 	    ?write_ErrorMsg("Unable to register '~p'; reason unknown.\n"
			    "Trying to unregister ~p\n", [H,Accum]),
	    uninstall_loop(Accum, {exit, register})
    end.

%%------------------------------------------------------------
%% function : uninstall_*/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosTime from the IFR DB
%%------------------------------------------------------------

uninstall_time() -> 
    case uninstall_loop(lists:reverse(?IDL_TIME_MODULES),ok) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

uninstall_timerevent() -> 
    case uninstall_loop(lists:reverse(?IDL_TIMEREVENT_MODULES),ok) of
	ok ->
	    ok;
	{error, Reason} ->
	    exit(Reason)
    end.

uninstall_loop([],ok) ->
    ok;
uninstall_loop([],{exit, register}) ->
    {error, {?MODULE, "oe_register failed"}};
uninstall_loop([],{exit, unregister}) ->
    {error, {?MODULE, "oe_unregister failed"}};
uninstall_loop([],{exit, both}) ->
    {error, {?MODULE, "oe_register and, for some of those already registered, oe_unregister failed"}};
uninstall_loop([H|T], Status) ->
    case catch H:'oe_unregister'() of
	ok ->
	    uninstall_loop(T, Status);
	_ when Status == ok ->
	    ?write_ErrorMsg("Unable to unregister '~p'; propably already unregistered.\n"
			    "You are adviced to confirm this.~n",[H]),
	    uninstall_loop(T, {exit, unregister});
	_ ->
 	    ?write_ErrorMsg("Unable to unregister '~p'; propably already unregistered.\n"
			    "You are adviced to confirm this.~n",[H]),
	    uninstall_loop(T, {exit, both})
    end.

%%------------------------------------------------------------
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosTime application.
%%------------------------------------------------------------
 
start() ->
    application:start(cosTime).
stop() ->
    application:stop(cosTime).
 
%%------------------------------------------------------------
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosTime, app_init).
 
 
%%------------------------------------------------------------
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
stop(_) ->
    ok.
 
%%------------------------------------------------------------
%% function : start_time_service
%% Arguments: Tdf - time difference to UTC
%%            Inaccuracy - ulonglong
%%            Upper - inaccuracy high
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
start_time_service(Tdf, Inaccuracy) when is_integer(Tdf) andalso is_integer(Inaccuracy) ->
    case supervisor:start_child(?SUPERVISOR_NAME, 
				?SUP_TIMESERVICE_SPEC(Tdf, Inaccuracy)) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	_Other->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
start_time_service(_Tdf, _Inaccuracy) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

    
%%------------------------------------------------------------
%% function : stop_time_service
%% Arguments: Obj - TimeService objref
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_time_service(Obj) ->
    corba:dispose(Obj).

%%------------------------------------------------------------
%% function : start_timerevent_service
%% Arguments: Timer - Timer Service Reference
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
start_timerevent_service(Timer) ->
    case supervisor:start_child(?SUPERVISOR_NAME, 
				?SUP_TIMEREVENTSERVICE_SPEC([Timer])) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	_Other->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------%
%% function : stop_timerevent_service
%% Arguments: Obj - TimerEventService objref
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_timerevent_service(Obj) ->
    corba:dispose(Obj).

%%-----------------------------------------------------------%
%% function : init
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
 
%% Starting using create_factory/X
init(own_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}};
%% When starting as an application.
init(app_init) ->
    {ok,{?SUP_FLAG, [?SUP_CHILD]}}.
 
%%-----------------------------------------------------------%
%% function : create_link
%% Arguments: Module - which Module to call
%%            Env/ArgList - ordinary oe_create arguments.
%% Returns  : 
%% Exception: 
%% Effect   : Necessary since we want the supervisor to be a 
%%            'simple_one_for_one'. Otherwise, using for example,
%%            'one_for_one', we have to call supervisor:delete_child
%%            to remove the childs startspecification from the 
%%            supervisors internal state.
%%------------------------------------------------------------
create_link(Module, Env, ArgList) ->
    Module:oe_create_link(Env, ArgList).

%%-----------------------------------------------------------%
%% function : start_event_handler
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

start_event_handler(Args) ->
    Name = create_name(eventhandler),
    case supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TIMEREVENTHANDLER_SPEC(Name,Args)) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	_Other->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.
   

%%-----------------------------------------------------------%
%% function : get_option
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

get_option(Key, OptionList, DefaultList) ->
    case lists:keysearch(Key, 1, OptionList) of
        {value,{Key,Value}} ->
            Value;
        _ ->
            case lists:keysearch(Key, 1, DefaultList) of
                {value,{Key,Value}} ->
                    Value;
                _->
                    {error, "Invalid option"}
            end
    end.

%%-----------------------------------------------------------%
%% function : type_check
%% Arguments: Obj  - objectrefernce to test.
%%            Mod  - Module which contains typeID/0.
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%------------------------------------------------------------

type_check(Obj, Mod) ->
    case catch corba_object:is_a(Obj,Mod:typeID()) of
        true ->
            ok;
        _ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.


%%-----------------------------------------------------------%
%% function : create_name/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
 
create_name(Type) ->
    Time = erlang:system_time(),
    Unique = erlang:unique_integer([positive]),
    lists:concat(['oe_',node(),'_',Type,'_',Time,'_',Unique]).

%%--------------- END OF MODULE ------------------------------


