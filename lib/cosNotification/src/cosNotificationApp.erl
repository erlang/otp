%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% File    : cosNotificationApp.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(cosNotificationApp).
 
%%--------------- INCLUDES -----------------------------------
%% Local
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").

-include("CosNotification_Definitions.hrl").
%%--------------- EXPORTS-------------------------------------
%% cosNotification API external
-export([start/0, stop/0, 
	 start_factory/1, start_factory/0, stop_factory/1,
	 start_global_factory/0, start_global_factory/1,
	 start_filter_factory/1, start_filter_factory/0, stop_filter_factory/1,
	 install/0, install/1, uninstall/0, uninstall/1,
	 install_event/0, install_event/1, uninstall_event/0, uninstall_event/1,
	 install_typed/0, install_typed/1, uninstall_typed/0, uninstall_typed/1,
	 create_structured_event/6, type_check/0, notify/0, max_events/0,
	 timeout_events/0, interval_events/0]).
 
%% Application callbacks
-export([start/2, init/1, stop/1]).

%%--------------- DEFINES ------------------------------------
-define(IDL_MODULES, ['oe_CosNotification', 
		      'oe_cosNotificationAppComm',
		      'oe_CosNotifyComm',
		      'oe_CosNotifyFilter', 
		      'oe_CosNotifyChannelAdmin']).
-define(EVENT_IDL_MODULES, ['oe_CosEventComm',
			    'oe_CosEventChannelAdmin']).
-define(TYPED_IDL_MODULES, ['oe_CosTypedEvent', 
			    'oe_CosTypedNotification']).

-define(FACTORY_NAME,    oe_cosNotificationFactory).
-define(SUPERVISOR_NAME, cosNotificationSup).


%%------------------------------------------------------------
%% function : install/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------

install() -> 
    install(0).

install(Time) when is_integer(Time) ->
    install_loop(?IDL_MODULES, timer:seconds(Time));
install(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : install_event/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------

install_event() -> 
    install_event(0).

install_event(Time) when is_integer(Time) ->
    install_loop(?EVENT_IDL_MODULES, timer:seconds(Time));
install_event(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : install_typed/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------

install_typed() -> 
    install_typed(0).

install_typed(Time) when is_integer(Time) ->
    install_loop(?TYPED_IDL_MODULES, timer:seconds(Time));
install_typed(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

install_loop([], _) ->
    ok;
install_loop([H|T], Time) ->
    H:'oe_register'(),
    timer:sleep(Time),
    install_loop(T, Time).

%%------------------------------------------------------------
%% function : uninstall/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosNotificationin from the IFR DB
%%------------------------------------------------------------

uninstall() -> 
    uninstall(0).

uninstall(Time) when is_integer(Time) ->
    uninstall_loop(lists:reverse(?IDL_MODULES), timer:seconds(Time));
uninstall(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : uninstall_event/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosNotificationin from the IFR DB
%%------------------------------------------------------------

uninstall_event() -> 
    uninstall_event(0).

uninstall_event(Time) when is_integer(Time) ->
    uninstall_loop(lists:reverse(?EVENT_IDL_MODULES), timer:seconds(Time));
uninstall_event(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : uninstall_typed/X
%% Arguments: - | Time (seconds)
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosNotificationin from the IFR DB
%%------------------------------------------------------------

uninstall_typed() -> 
    uninstall_typed(0).

uninstall_typed(Time) when is_integer(Time) ->
    uninstall_loop(lists:reverse(?TYPED_IDL_MODULES), timer:seconds(Time));
uninstall_typed(_Time) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

uninstall_loop([], _) ->
    ok;
uninstall_loop([H|T], Time) ->
    H:'oe_unregister'(),
    timer:sleep(Time),
    uninstall_loop(T, Time).
 
 
%%------------------------------------------------------------
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosTRansaction application.
%%------------------------------------------------------------
 
start() ->
    application:start(cosNotification).
stop() ->
    application:stop(cosNotification).
 
%%------------------------------------------------------------
%% function : start_factory 
%% Arguments: none or an argumentlist whith default values.
%% Returns  : ObjectRef | {'EXCEPTION', _} | {'EXIT', Reason}
%% Effect   : Starts a CosNotifyChannelAdmin_EventChannelFactory
%%------------------------------------------------------------
start_factory() ->
    start_factory(?not_DEFAULT_SETTINGS).
    
start_factory(Args) when is_list(Args) ->
    SO = 'CosNotification_Common':get_option(server_options, Args, ?not_DEFAULT_SETTINGS),
    SPEC = ['CosNotifyChannelAdmin_EventChannelFactory',Args,
	    [{sup_child, true}, 
	     {regname, {local, oe_cosNotificationFactory}}|SO]],
    case supervisor:start_child(?SUPERVISOR_NAME, SPEC) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	Other->
	    orber:dbg("[~p] cosNotificationApp:start_factory( ~p ).~n"
		      "Reason: ~p~n", [?LINE, Args, Other], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
start_factory(Args) ->
    orber:dbg("[~p] cosNotificationApp:start_factory( ~p ).~n"
	      "Bad parameters~n", [?LINE, Args], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
 
%%------------------------------------------------------------
%% function : start_global_factory 
%% Arguments: none or an argumentlist whith default values.
%% Returns  : ObjectRef | {'EXCEPTION', _} | {'EXIT', Reason}
%% Effect   : Starts a CosNotifyChannelAdmin_EventChannelFactory
%%------------------------------------------------------------
start_global_factory() ->
    start_global_factory(?not_DEFAULT_SETTINGS).
    
start_global_factory(Args) when is_list(Args) ->
    SO = 'CosNotification_Common':get_option(server_options, Args, ?not_DEFAULT_SETTINGS),
    Name = create_name(),
    SPEC = ['CosNotifyChannelAdmin_EventChannelFactory',Args,
	    [{sup_child, true}, 
	     {regname, {global, Name}}|SO]],
    case supervisor:start_child(?SUPERVISOR_NAME, SPEC) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	Other->
	    orber:dbg("[~p] cosNotificationApp:start_global_factory( ~p ).~n"
		      "Reason: ~p~n", [?LINE, Args, Other], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
start_global_factory(Args) ->
    orber:dbg("[~p] cosNotificationApp:start_global_factory( ~p ).~n"
	      "Bad parameters~n", [?LINE, Args], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
 
 
%%------------------------------------------------------------
%% function : stop_factory 
%% Arguments: Factory Object Reference
%% Returns  : ok | {'EXCEPTION', _}
%% Effect   : 
%%------------------------------------------------------------
 
stop_factory(Fac)->
    corba:dispose(Fac).
 
%%------------------------------------------------------------
%% function : start_filter_factory 
%% Arguments: none or an argumentlist which by default is defined
%%            in CosNotification_Definitions.hrl, i.e., '?not_FILTERFAC_DEF'
%% Returns  : ObjectRef | {'EXCEPTION', _} | {'EXIT', Reason}
%% Effect   : Starts a CosNotifyChannelAdmin_EventChannelFactory
%%------------------------------------------------------------
start_filter_factory() ->
    start_filter_factory([{typecheck, true},
			  {tty, false},
			  {logfile, false},
			  {server_options, []}]).
start_filter_factory(Args) when is_list(Args) ->
    SO = 'CosNotification_Common':get_option(server_options, Args, 
					     ?not_DEFAULT_SETTINGS),
    SPEC = ['CosNotifyFilter_FilterFactory',Args, [{sup_child, true}|SO]],
    case supervisor:start_child(?SUPERVISOR_NAME, SPEC) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	Other->
	    orber:dbg("[~p] cosNotificationApp:start_filter_factory( ~p ).~n"
		      "Reason: ~p~n", [?LINE, Args, Other], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
start_filter_factory(Args) ->
	    orber:dbg("[~p] cosNotificationApp:start_filter_factory( ~p ).~n"
		      "Bad parameters~n", [?LINE, Args], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
 
 
%%------------------------------------------------------------
%% function : stop_filter_factory 
%% Arguments: FilterFactory Object Reference
%% Returns  : ok | {'EXCEPTION', _}
%% Effect   : 
%%------------------------------------------------------------
 
stop_filter_factory(Fac)->
    corba:dispose(Fac).


%%------------------------------------------------------------
%% function : create_structured_event
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
create_structured_event(StrD,StrT,StrE,PSeqV,PSeqF,AnyR) 
  when is_list(StrD) andalso is_list(StrT) andalso is_list(StrE) 
       andalso is_list(PSeqV) andalso is_list(PSeqF) andalso
       is_record(AnyR, any) ->
#'CosNotification_StructuredEvent'{header = 
   #'CosNotification_EventHeader'{fixed_header = 
		  #'CosNotification_FixedEventHeader'{event_type =
				      #'CosNotification_EventType'{domain_name=StrD,
						   type_name=StrT},
				      event_name = StrE},
		  variable_header = PSeqV},
   filterable_data = PSeqF,
   remainder_of_body = AnyR};
create_structured_event(_StrD,_StrT,_StrE,_PSeqV,_PSeqF,_AnyR) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    

%%------------------------------------------------------------
%% function : type_check
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
type_check() ->
    case application:get_env(cosNotification, type_check) of
	{ok, Boolean} when is_atom(Boolean) ->
	    Boolean;
	_ ->
	    true
    end.
  
%%------------------------------------------------------------
%% function : notify
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
notify() ->
    case application:get_env(cosNotification, notify) of
	{ok, Module} when is_atom(Module) ->
	    Module;
	_ ->
	    false
    end.

%%------------------------------------------------------------
%% function : max_events
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
max_events() ->
    case application:get_env(cosNotification, max_events) of
	{ok, Max} when is_integer(Max) ->
	    Max;
	_ ->
	    50
    end.

%%------------------------------------------------------------
%% function : timeout_events
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
timeout_events() ->
    case application:get_env(cosNotification, timeout_events) of
	{ok, Max} when is_integer(Max) ->
	    Max;
	_ ->
	    3000000 %% 5 minutes
    end.


%%------------------------------------------------------------
%% function : interval_events
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
interval_events() ->
    case application:get_env(cosNotification, interval_events) of
	{ok, Max} when is_integer(Max) ->
	    Max;
	_ ->
	    10000 %% 10 seconds
    end.
  

%%------------------------------------------------------------
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosNotificationApp, app_init).
 
 
%%------------------------------------------------------------
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
stop(_) ->
    ok.
 
%%------------------------------------------------------------
%% function : init
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
 
%% Starting using create_factory/X
init(own_init) ->
    {ok,{{simple_one_for_one,50,10}, 
	 [{"oe_NotChild",
	   {'CosNotification_Common',create_link, []},
	   transient,100000,worker,
	   ['CosNotifyChannelAdmin_EventChannel',
	    'CosNotifyChannelAdmin_EventChannel_impl']}]}};
%% When starting as an application.
init(app_init) ->
    {ok,{{simple_one_for_one,50,10}, 
	 [{"oe_NotChild",
	   {'CosNotification_Common',create_link, []},
	   transient,100000,worker,
	   ['CosNotifyChannelAdmin_EventChannel',
	    'CosNotifyChannelAdmin_EventChannel_impl']}]}}.



%%------------------------------------------------------------
%% function : create_name
%% Arguments: 
%% Returns  : 
%% Effect   : Create a unique name to use when, for eaxmple, starting
%%            a new server.
%%------------------------------------------------------------
create_name() ->
    {MSec, Sec, USec} = erlang:now(),
    lists:concat(['oe_',node(),'_',MSec, '_', Sec, '_', USec]).

%%--------------- END OF MODULE ------------------------------
