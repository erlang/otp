%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : cosEventDomainApp.erl
%% Purpose : 
%%--------------------------------------------------------------------

-module(cosEventDomainApp).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("cosNotification/include/CosNotification.hrl").
%% Application files
-include("cosEventDomainApp.hrl").
-include("CosEventDomainAdmin.hrl").

%%--------------- EXPORTS ------------------------------------
%% External MISC
-export([get_option/3, 
         create_name/2, 
         create_name/1,
	 create_id/0,
	 create_id/1,
         is_debug_compiled/0,
	 install/0,
	 uninstall/0,
	 start_factory/0,
	 start_factory/1,
	 start_factory_link/0,
	 start_factory_link/1,
	 stop_factory/1,
	 start/0,
	 stop/0,
	 create_link/3,
	 get_qos/1,
	 get_admin/1]).

%% Application callbacks
-export([start/2, init/1, stop/1]).

%%--------------- DEFINES ------------------------------------

-define(SUPERVISOR_NAME, oe_cosEventDomainSup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).

-define(SUP_SPEC(Name, Args), 
        ['CosEventDomainAdmin_EventDomain',Args, 
         [{sup_child, true}, {regname, {global, Name}}]]).
-define(SUP_CHILD, 
        {"oe_EventDomainChild",
         {cosEventDomainApp,create_link, []},
	 transient,100000,worker,
         ['CosEventDomainAdmin_EventDomain']}).


-define(DEFAULT_OPTIONS, []).

%%--------------- DEFINITIONS OF CONSTANTS -------------------
%%--------------- EXTERNAL MISC FUNCTIONS --------------------
%%-----------------------------------------------------------%
%% function : install
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
install() ->
    oe_CosEventDomainAdmin:oe_register().

%%-----------------------------------------------------------%
%% function : uninstall
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
uninstall() ->
    oe_CosEventDomainAdmin:oe_unregister().

%%-----------------------------------------------------------%
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosTime application.
%%------------------------------------------------------------
 
start() ->
    application:start(cosEventDomain).
stop() ->
    application:stop(cosEventDomain).
 
%%-----------------------------------------------------------%
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
 
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosEventDomainApp, app_init).
 
 
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
start_factory() ->
    start_factory(?DEFAULT_OPTIONS).

start_factory(Options) when is_list(Options) ->
    'CosEventDomainAdmin_EventDomainFactory':oe_create(Options);
start_factory(Options) ->
    orber:dbg("[~p] cosEventDomainApp:start_factory(~p);~n"
	      "Options not correct.", 
	      [?LINE, Options], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
%%-----------------------------------------------------------%
%% function : start_channel
%% Arguments: -
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
start_factory_link() ->
    start_factory_link(?DEFAULT_OPTIONS).

start_factory_link(Options) when is_list(Options) ->
    'CosEventDomainAdmin_EventDomainFactory':oe_create_link(Options);
start_factory_link(Options) ->
    orber:dbg("[~p] cosEventDomainApp:start_factory_link(~p);~n"
	      "Options not correct.", 
	      [?LINE, Options], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
%%-----------------------------------------------------------%
%% function : stop_factory
%% Arguments: ChannelObj
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_factory(ChannelObj) ->
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


%%------------------------------------------------------------
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
%% function : create_name/2
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------ 
create_name(Name,Type) ->
    {MSec, Sec, USec} = erlang:now(),
    lists:concat(['oe_',node(),'_',Type,'_',Name,'_',MSec, '_', Sec, '_', USec]).
 
%%-----------------------------------------------------------%
%% function : create_name/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
create_name(Type) ->
    {MSec, Sec, USec} = erlang:now(),
    lists:concat(['oe_',node(),'_',Type,'_',MSec, '_', Sec, '_', USec]).

%%------------------------------------------------------------
%% function : create_id/0
%% Arguments: - 
%% Returns  : CosEventDomainAdmin::DomainID (long)
%% Exception: 
%% Purpose  : 
%%------------------------------------------------------------
create_id(2147483647) ->
    -2147483648;
create_id(OldID) ->
    OldID+1.


create_id() ->
    {_A,_B,C}=now(),
    C.
%%------------------------------------------------------------
%% function : get_qos
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
get_qos([]) ->
    [];
get_qos(Properties) ->
    case get_qos(Properties, [], []) of
	{ok, Supported} ->
	    Supported;
	{error, Unsupported} ->
	    corba:raise(#'CosNotification_UnsupportedQoS'{qos_err = Unsupported})
    end.

get_qos([], Supported, []) ->
    {ok, Supported};
get_qos([], _, Unsupported) ->
    {error, Unsupported};
get_qos([#'CosNotification_Property'{name = ?CycleDetection,
				     value= #any{value = ?AuthorizeCycles}}|T], 
	Supported, Unsupported) ->
    get_qos(T, [{?CycleDetection, ?AuthorizeCycles}|Supported], Unsupported);
get_qos([#'CosNotification_Property'{name = ?CycleDetection,
				     value= #any{value = ?ForbidCycles}}|T], 
	Supported, Unsupported) ->
    get_qos(T, [{?CycleDetection, ?ForbidCycles}|Supported], Unsupported);
get_qos([#'CosNotification_Property'{name = ?CycleDetection}|T], 
	Supported, Unsupported) ->
    %% Illegal value supplied.
    get_qos(T, Supported, 
	    [#'CosNotification_PropertyError'
	     {code = 'UNSUPPORTED_VALUE', 
	      name = ?CycleDetection, 
	      available_range = #'CosNotification_PropertyRange'
	      {low_val=any:create(orber_tc:short(), ?AuthorizeCycles), 
	       high_val=any:create(orber_tc:short(), ?ForbidCycles)}}|Unsupported]);
get_qos([#'CosNotification_Property'{name = ?DiamondDetection,
				     value= #any{value = ?AuthorizeDiamonds}}|T], 
	      Supported, Unsupported) ->
    get_qos(T, [{?DiamondDetection, ?AuthorizeDiamonds}|Supported], Unsupported);
get_qos([#'CosNotification_Property'{name = ?DiamondDetection,
				     value= #any{value = ?ForbidDiamonds}}|T], 
	Supported, Unsupported) ->
    get_qos(T, [{?DiamondDetection, ?ForbidDiamonds}|Supported], Unsupported);
get_qos([#'CosNotification_Property'{name = ?DiamondDetection}|T], 
	Supported, Unsupported) ->
    %% Illegal value supplied.
    get_qos(T, Supported, 
	    [#'CosNotification_PropertyError'
	     {code = 'UNSUPPORTED_VALUE',
	      name = ?DiamondDetection,
	      available_range = #'CosNotification_PropertyRange'
	      {low_val=any:create(orber_tc:short(), ?AuthorizeDiamonds), 
	       high_val=any:create(orber_tc:short(), ?ForbidDiamonds)
	      }} | Unsupported]);
get_qos([#'CosNotification_Property'{name = Name}|T], Supported, Unsupported) ->
    %% Unknown QoS supplied.
    get_qos(T, Supported, 
	    [#'CosNotification_PropertyError'
	     {code = 'BAD_PROPERTY',
	      name = Name,
	      available_range = #'CosNotification_PropertyRange'
	      {low_val=any:create(orber_tc:null(), null), 
	       high_val=any:create(orber_tc:null(), null)}} | Unsupported]).

%%------------------------------------------------------------
%% function : get_admin
%% Arguments: 
%% Returns  : {"EXCEPTION', #'CosNotification_PropertyError'{}}
%% Exception: 
%% Effect   : No Admin supported.
%%------------------------------------------------------------
get_admin([]) ->
    [];
get_admin(Properties) ->
    get_admin(Properties, []).

get_admin([], Unsupported) ->
    corba:raise(#'CosNotification_UnsupportedAdmin'{admin_err = Unsupported});
get_admin([#'CosNotification_Property'{name = Name}|T], Unsupported) ->
    %% Unknown QoS supplied.
    get_admin(T, [#'CosNotification_PropertyError'
		  {code = 'BAD_PROPERTY',
		   name = Name,
		   available_range = #'CosNotification_PropertyRange'
		   {low_val=any:create(orber_tc:null(), null), 
		    high_val=any:create(orber_tc:null(), null)}} | Unsupported]).


%%------------------------------------------------------------
%% function : is_debug_compiled
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
-ifdef(debug).
    is_debug_compiled() -> true.
-else.
    is_debug_compiled() -> false.
-endif.
 
%%--------------- END OF MODULE ------------------------------
