%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% File        : cosEventApp.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(cosEventApp).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include("cosEventApp.hrl").


%%--------------- EXPORTS-------------------------------------
%% cosEvent API external
-export([start/0, stop/0, install/0, uninstall/0, start_channel/0, start_channel/1, 
	 start_channel_link/0, start_channel_link/1, stop_channel/1]).

%% cosEvent API internal
-export([create_link/3, get_option/2, type_check/3, disconnect/3, do_disconnect/3]).
 
%% Application callbacks
-export([start/2, init/1, stop/1]).

%%--------------- DEFINES ------------------------------------
-define(IDL_MODULES, ['oe_CosEventComm',
		      'oe_CosEventChannelAdmin', 
		      'oe_cosEventApp']).

-define(SUPERVISOR_NAME, oe_cosEventSup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).

-define(SUP_SPEC(Name, Args), 
        ['CosEventChannel_EventChannel',Args, 
         [{sup_child, true}, {regname, {global, Name}}]]).
-define(SUP_CHILD, 
        {"oe_EventChild",
         {cosEventApp,create_link, []},
	 transient,100000,worker,
         ['CosEventChannel_EventChannel']}).


%%-----------------------------------------------------------%
%% function : install
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------
install() -> 
    case install_loop(?IDL_MODULES, []) of
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
 			    "Trying to unregister ~p~n", [H,App,Accum]),
	    uninstall_loop(Accum, {exit, register});
	{'EXCEPTION',_} ->
 	    ?write_ErrorMsg("Unable to register '~p'; propably already registered.\n"
			    "You are adviced to confirm this.\n"
			    "Trying to unregister ~p~n", [H,Accum]),
	    uninstall_loop(Accum, {exit, register});
	ok ->
	    install_loop(T, [H|Accum]);
	_ ->
 	    ?write_ErrorMsg("Unable to register '~p'; reason unknown.\n"
			    "Trying to unregister ~p~n", [H,Accum]),
	    uninstall_loop(Accum, {exit, register})
    end.

%%-----------------------------------------------------------%
%% function : uninstall
%% Arguments: - 
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosEvent from the IFR DB
%%------------------------------------------------------------
uninstall() -> 
    case uninstall_loop(lists:reverse(?IDL_MODULES), ok) of
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
			    "You are adviced to confirm this.\n",[H]),
	    uninstall_loop(T, {exit, unregister});
	_ ->
	    ?write_ErrorMsg("Unable to unregister '~p'; propably already unregistered.\n"
			    "You are adviced to confirm this.\n",[H]),
	    uninstall_loop(T, {exit, both})
    end.
	
%%-----------------------------------------------------------%
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosTime application.
%%------------------------------------------------------------
 
start() ->
    application:start(cosEvent).
stop() ->
    application:stop(cosEvent).
 
%%-----------------------------------------------------------%
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosEventApp, app_init).
 
 
%%-----------------------------------------------------------%
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
stop(_) ->
    ok.
 
%%-----------------------------------------------------------%
%% function : start_channel
%% Arguments: -
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
start_channel() ->
    start_channel(?DEFAULT_OPTIONS).

start_channel(Options) when is_list(Options) ->
    ServerOpts = get_option(?SERVER, Options),
    'oe_CosEventComm_Channel':oe_create([Options, ServerOpts], ServerOpts);
start_channel(Options) ->
    orber:dbg("[~p] cosEventApp:start_channel(~p);~n"
	      "Options not correct.", [?LINE, Options], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
%%-----------------------------------------------------------%
%% function : start_channel
%% Arguments: -
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
start_channel_link() ->
    start_channel_link(?DEFAULT_OPTIONS).

start_channel_link(Options) when is_list(Options) ->
    ServerOpts = get_option(?SERVER, Options),
    'oe_CosEventComm_Channel':oe_create_link([Options, ServerOpts], ServerOpts);
start_channel_link(Options) ->
    orber:dbg("[~p] cosEventApp:start_channel_link(~p);~n"
	      "Options not correct.", [?LINE, Options], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
%%-----------------------------------------------------------%
%% function : stop_factory
%% Arguments: ChannelObj
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_channel(ChannelObj) ->
    corba:dispose(ChannelObj).

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
%% function : get_option
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
get_option(Key, OptionList) ->
    case lists:keysearch(Key, 1, OptionList) of
        {value,{Key,Value}} ->
            Value;
        _ ->
            case lists:keysearch(Key, 1, ?DEFAULT_OPTIONS) of
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
type_check(_Obj, _Mod, false) ->
    ok;
type_check(Obj, Mod, _) ->
    case catch corba_object:is_a(Obj, Mod:typeID()) of
        true ->
            ok;
        _ ->
	    orber:dbg("[~p] cosEventApp:type_check(~p) failed; Should be ~p", 
		      [?LINE, Obj, Mod], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------%
%% function : disconnect
%% Arguments: Module - one of the interfaces defined in CosEventComm.
%%            Function - the appropriate disconnect function.
%%            Object - the client object reference.
%% Returns  : ok
%% Exception: 
%% Effect   : If the process would try to diconnect itself it could
%%            result in a deadlock. Hence, we spawn a new process to do it.
%%------------------------------------------------------------
disconnect(Module, Function, Object) ->
    spawn(cosEventApp, do_disconnect, [Module, Function, Object]),
    ok.

do_disconnect(Module, Function, Object) ->
    catch Module:Function(Object),
    ?DBG("Disconnect ~p:~p(..).~n", [Module, Function]),
    ok.

%%--------------- END OF MODULE ------------------------------


