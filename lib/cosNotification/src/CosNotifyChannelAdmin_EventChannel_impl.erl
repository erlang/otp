%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-------------------------------------------------------------------
%% File    : CosNotifyChannelAdmin_EventChannel_impl.erl
%% Purpose : 
%% Created : 28 Sep 1999
%%-------------------------------------------------------------------

-module('CosNotifyChannelAdmin_EventChannel_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").
-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%%----- CosNotifyChannelAdmin::EventChannel ------------------
-export([new_for_consumers/4,
	 new_for_suppliers/4,
	 get_consumeradmin/4,
	 get_supplieradmin/4,
	 get_all_consumeradmins/3,
	 get_all_supplieradmins/3]).

%% Attributes (external)
-export(['_get_MyFactory'/3,
	 '_get_default_consumer_admin'/3,
	 '_get_default_supplier_admin'/3,
	 '_get_default_filter_factory'/3]).

%%----- Inherit from CosNotification::QoSAdmin ---------------
-export([get_qos/3,
	 set_qos/4,
	 validate_qos/4]).

%%----- Inherit from CosNotification::AdminPropertiesAdmin ---
-export([get_admin/3,
	 set_admin/4]).

%%----- Inherit from CosEventChannelAdmin::EventChannel ------
-export([for_consumers/3,
	 for_suppliers/3,
	 destroy/3]).
%%--------------- Internal -----------------------------------
%%----- Inherit from cosNotificationComm ---------------------
-export([callAny/5,
	 callSeq/5]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%% Data structures
-record(state, {myFac,
		myConsumers = [],
	        defConsumerAdmin,
		defSupplierAdmin,
	        defConsumerAdminPid,
		defSupplierAdminPid,
		defFilterFac,
		defFilterFacPid,
		etsR,
		qosLocal,
		qosGlobal,
		admGlobal,
		options,
		idCounter = 0,
		'MaxQueueLength',
		'MaxConsumers',
		'MaxSuppliers'}).
%% Data structures constructors
-define(get_InitState(My, Fil, FilPid, QoS, LQoS, GA, MQ, MC, MS, O), 
	#state{myFac            = My,
	       defFilterFac     = Fil,
	       defFilterFacPid  = FilPid,
	       etsR = ets:new(oe_ets, [set, protected]),
	       qosLocal         = LQoS,
	       qosGlobal        = QoS,
	       admGlobal        = GA,
	       options          = O,
	       'MaxQueueLength' = MQ,
	       'MaxConsumers'   = MC,
	       'MaxSuppliers'   = MS}).

%% NOTE!!!
%% When forwarding events, the objects we will contact is ONLY ConsumerAdmins!!
%% Hence, we will store SupplierAdmins and ConsumerAdmins differently. SupplierAdmins
%% we store in our ets-table while the ConsumerAdmins will be stored on the local
%% State.
 
%% Data structures selectors
-define(get_supplierAdmins(S),      [S#state.defSupplierAdmin|
				     lists:flatten(ets:match(S#state.etsR, 
							     {'_','$1','_'}))]).
-define(get_consumerAdmins(S),      [{0, S#state.defConsumerAdmin,S#state.defConsumerAdminPid}
				     |S#state.myConsumers]).
-define(get_allAdmins(S),           [S#state.defSupplierAdmin, 
				     S#state.defConsumerAdmin|
				     lists:flatten(ets:match(S#state.etsR, 
							     {'_','$1','_'}))++
				     find_field(S#state.myConsumers, 2)]).
-define(get_consumerAdmIDs(S),      [0|find_field(S#state.myConsumers, 1)]).
-define(get_supplierAdmIDs(S),      [0|lists:flatten(ets:match(S#state.etsR, 
							       {'$1','_','_'}))]).

-define(get_supplierAdmin(S, I),    find_obj(ets:lookup(S#state.etsR, I))).
-define(get_consumerAdmin(S, I),    find_obj(lists:keysearch(I,1,S#state.myConsumers))).
-define(get_supplierAmount(S),      ets:info(S#state.etsR, size)).
-define(get_consumerAmount(S),      length(S#state.myConsumers)).

-define(get_MyFactory(S),           S#state.myFac).
-define(get_defConsumerAdm(S),      S#state.defConsumerAdmin).
-define(get_defSupplierAdm(S),      S#state.defSupplierAdmin).
-define(get_defConsumerAdmPid(S),   S#state.defConsumerAdminPid).
-define(get_defSupplierAdmPid(S),   S#state.defSupplierAdminPid).
-define(get_defFilterFac(S),        S#state.defFilterFac).
-define(get_defFilterFacPid(S),     S#state.defFilterFacPid).
-define(get_GlobalQoS(S),           S#state.qosGlobal).
-define(get_LocalQoS(S),            S#state.qosLocal).
-define(get_BothQoS(S),             {S#state.qosGlobal, S#state.qosLocal}).
-define(get_GlobalAdm(S),           S#state.admGlobal).
-define(get_Options(S),             S#state.options).
-define(get_MaxQueueLength(S),      S#state.'MaxQueueLength').
-define(get_MaxConsumers(S),        S#state.'MaxConsumers').
-define(get_MaxSuppliers(S),        S#state.'MaxSuppliers').
-define(get_IdCounter(S),           S#state.idCounter).

%% Data structures modifiers
-define(set_GlobalQoS(S,D),         S#state{qosGlobal=D}).
-define(set_BothQoS(S,GD,LD),       S#state{qosGlobal=GD, qosLocal=LD}).
-define(set_GlobalAdm(S,D),         S#state{admGlobal=D}).
-define(set_AllAdminP(S,GD, MQ, MC, MS),       
	                            S#state{admGlobal=GD,'MaxQueueLength'=MQ, 
					    'MaxConsumers'=MC, 'MaxSuppliers'=MS}).
-define(set_MaxQueueLength(S,D),    S#state{'MaxQueueLength'=D}).
-define(set_MaxConsumers(S,D),      S#state{'MaxConsumers'=D}).
-define(set_MaxSuppliers(S,D),      S#state{'MaxSuppliers'=D}).
-define(set_defConsumerAdm(S,D,P),  S#state{defConsumerAdmin=D, defConsumerAdminPid=P}).
-define(set_defSupplierAdm(S,D,P),  S#state{defSupplierAdmin=D, defSupplierAdminPid=P}).
-define(set_defFilterFac(S,D,P),    S#state{defFilterFac=D, defFilterFacPid=P}).
-define(add_supplierAdmin(S,I,O,P), ets:insert(S#state.etsR, {I,O,P})).
-define(add_consumerAdmin(S,I,O,P), S#state{myConsumers= [{I,O,P}|S#state.myConsumers]}).
-define(del_supplierAdmin(S,I),     ets:delete(S#state.etsR, I)).
-define(del_consumerAdmin(S,I),     S#state{myConsumers=
					    lists:keydelete(I, 1, 
							    S#state.myConsumers)}).
-define(del_consumerAdminRef(S,O),  S#state{myConsumers=
					    lists:keydelete(O, 2, 
							    S#state.myConsumers)}).
-define(del_AdminPid(S,P),          delete_obj(S, P)).
-define(set_IdCounter(S,V),         S#state{idCounter=V}).
-define(new_Id(S),             'CosNotification_Common':create_id(S#state.idCounter)).
%% MISC
-define(is_UndefDefConsAdm(S),      S#state.defConsumerAdmin == undefined).
-define(is_UndefDefSuppAdm(S),      S#state.defSupplierAdmin == undefined).
-define(is_UndefDefFilterFac(S),    S#state.defFilterFac == undefined).
-define(is_PersistentConnection(S),
	?not_GetConnectionReliability((S#state.qosLocal)) == ?not_Persistent).
-define(is_PersistentEvent(S),
	?not_GetEventReliability((S#state.qosLocal)) == ?not_Persistent).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?DBG("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, _Reason} when ?get_defConsumerAdmPid(State) == Pid ->
	    {noreply, ?set_defConsumerAdm(State, undefined, undefined)};
        {'EXIT', Pid, _Reason} when ?get_defSupplierAdmPid(State) == Pid ->
	    {noreply, ?set_defSupplierAdm(State, undefined, undefined)};
        {'EXIT', Pid, _Reason} when ?get_defFilterFacPid(State) == Pid ->
	    {noreply, ?set_defFilterFac(State, undefined, undefined)};
        {'EXIT', Pid, normal} ->
            {noreply, ?del_AdminPid(State, Pid)};
        _Other ->
            ?DBG("TERMINATED: ~p~n",[_Other]),
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([MyFac, InitQoS, InitAdmin, LocalQoS, [MaxQ, MaxC, MaxS|_], Options]) ->
    process_flag(trap_exit, true),
    SO = 'CosNotification_Common':get_option(server_options, Options, 
					     ?not_DEFAULT_SETTINGS),
    
    ?DBG("CHANNEL INIT STATE:~n~p~n~p~n~p  ~p  ~p  ~p~n~p~n",
		 [InitQoS, InitAdmin, LocalQoS,MaxQ, MaxC, MaxS, Options]),

    %% Both default Admin:s have the unique id 0 (OMG spec, 98-11-01, 
    %% Notification p 148), hence, no need to create a unique id.
    %% We don't have acces to OE_THIS in this stage so we cannot create the objects 
    %% now, even though the specification states that.
    %% DefConAdm = 'CosNotifyChannelAdmin_ConsumerAdmin':oe_create_link([0],[]),
    %% DefSupAdm = 'CosNotifyChannelAdmin_SupplierAdmin':oe_create_link([0],[]),

    case 'CosNotifyFilter_FilterFactory':oe_create_link([], [{sup_child, true}|SO]) of
	{ok, Pid, DefFiFac} ->
	    {ok, ?get_InitState(MyFac, DefFiFac, Pid, InitQoS, LocalQoS, 
				InitAdmin, MaxQ, MaxC, MaxS, Options)};
	Reason ->
	    {stop, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%----- CosNotifyChannelAdmin_EventChannel attributes -------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_MyFactory'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_MyFactory'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_MyFactory(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_default_consumer_admin'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_default_consumer_admin'(OE_THIS, _OE_FROM, State) 
  when ?is_UndefDefConsAdm(State) ->
    Op = 'CosNotification_Common':get_option(filterOp, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ConsumerAdmin':oe_create_link([0, OE_THIS,
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, DefConAdm} ->
	    {reply, DefConAdm, ?set_defConsumerAdm(State, DefConAdm, Pid)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:_get_default_consumer_admin();~n"
		      "Unable to create: CosNotifyChannelAdmin_ConsumerAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
'_get_default_consumer_admin'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_defConsumerAdm(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_default_supplier_admin'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_default_supplier_admin'(OE_THIS, _OE_FROM, State) 
  when ?is_UndefDefSuppAdm(State) ->
    Op = 'CosNotification_Common':get_option(filterOp, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_SupplierAdmin':oe_create_link([0, OE_THIS,
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, DefSupAdm} ->
	    {reply, DefSupAdm, ?set_defSupplierAdm(State, DefSupAdm, Pid)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:_get_default_supplier_admin();~n"
		      "Unable to create: CosNotifyChannelAdmin_SupplierAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
'_get_default_supplier_admin'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_defSupplierAdm(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_default_filter_factory'
%% Type     : readonly
%% Returns  : 
%%----------------------------------------------------------
'_get_default_filter_factory'(_OE_THIS, _OE_FROM, State) 
  when ?is_UndefDefFilterFac(State) ->
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyFilter_FilterFactory':oe_create_link([], [{sup_child, true}|SO]) of
	{ok, Pid, DefFiFac} ->
	    {reply, DefFiFac, ?set_defFilterFac(State, DefFiFac, Pid)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:_get_default_filter_factory();~n"
		      "Unable to create: CosNotifyChannelAdmin_FilterFactory.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
'_get_default_filter_factory'(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_defFilterFac(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : new_for_consumers
%% Arguments: Op - InterFilterGroupOperator: 'AND_OP' | 'OR_OP'
%%            Determines if the Admin:s proxy-children will
%%            use AND or OR when checking Filters.
%% Returns  : ConsAdm
%%            AdminId (out)
%%-----------------------------------------------------------
new_for_consumers(OE_THIS, _OE_FROM, State, Op) ->
    is_admin_limit_reached(?get_MaxConsumers(State), ?get_consumerAmount(State)),
    AdminId = ?new_Id(State),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ConsumerAdmin':oe_create_link([AdminId, OE_THIS, 
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, AdminCo} ->
	    %% Due to different storage, adding a new consumer is NOT done in the
	    %% same way as for suppliers.
	    NewState = ?add_consumerAdmin(State, AdminId, AdminCo, Pid),
	    {reply, {AdminCo, AdminId}, ?set_IdCounter(NewState, AdminId)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:nef_for_consumers();~n"
		      "Unable to create: CosNotifyChannelAdmin_ConsumerAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : new_for_suppliers
%% Arguments: Op - InterFilterGroupOperator: 'AND_OP' | 'OR_OP'
%%            Determines if the Admin:s proxy-children will
%%            use AND or OR when checking Filters.
%% Returns  : SuppAdm 
%%            AdminId (out)
%%-----------------------------------------------------------
new_for_suppliers(OE_THIS, _OE_FROM, State, Op) ->
    is_admin_limit_reached(?get_MaxSuppliers(State), ?get_supplierAmount(State)),
    AdminId = ?new_Id(State),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_SupplierAdmin':oe_create_link([AdminId, OE_THIS, 
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, AdminSu} ->
	    %% Due to different storage, adding a new supplier is NOT done in the
	    %% same way as for consumers.
	    ?add_supplierAdmin(State, AdminId, AdminSu, Pid),
	    {reply, {AdminSu, AdminId}, ?set_IdCounter(State, AdminId)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:new_for_suppliers();~n"
		      "Unable to create: CosNotifyChannelAdmin_SupplierAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : get_consumeradmin
%% Arguments: AdminId
%% Returns  : ConsAdmin
%%-----------------------------------------------------------
get_consumeradmin(OE_THIS, _OE_FROM, State, 0) when ?is_UndefDefConsAdm(State) ->
    Op = 'CosNotification_Common':get_option(filterOp, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ConsumerAdmin':oe_create_link([0, OE_THIS,
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, DefConAdm} ->
	    {reply, DefConAdm, ?set_defConsumerAdm(State, DefConAdm, Pid)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:get_consumer_admin();~n"
		      "Unable to create: CosNotifyChannelAdmin_ConsumerAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;	    
get_consumeradmin(_OE_THIS, _OE_FROM, State, 0) ->
    {reply, ?get_defConsumerAdm(State), State};
get_consumeradmin(_OE_THIS, _OE_FROM, State, AdminId) when is_integer(AdminId) ->
    {reply, ?get_consumerAdmin(State, AdminId), State};
get_consumeradmin(_, _, _, What) ->
    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:get_consumeradmin(~p);~n"
	      "Not an integer", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : get_supplieradmin
%% Arguments: AdminId
%% Returns  : 
%%-----------------------------------------------------------
get_supplieradmin(OE_THIS, _OE_FROM, State, 0) when ?is_UndefDefSuppAdm(State) ->
    Op = 'CosNotification_Common':get_option(filterOp, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_SupplierAdmin':oe_create_link([0, OE_THIS,
								     self(), Op, 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, DefSupAdm} ->
	    {reply, DefSupAdm, ?set_defSupplierAdm(State, DefSupAdm, Pid)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:get_supplieradmin();~n"
		      "Unable to create: CosNotifyChannelAdmin_SupplierAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
get_supplieradmin(_OE_THIS, _OE_FROM, State, 0) ->
    {reply, ?get_defSupplierAdm(State), State};
get_supplieradmin(_OE_THIS, _OE_FROM, State, AdminId) when is_integer(AdminId) ->
    {reply, ?get_supplierAdmin(State, AdminId), State};
get_supplieradmin(_, _, _, What) ->
    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:get_supplieradmin(~p);~n"
	      "Not an integer", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : get_all_consumeradmins
%% Arguments: -
%% Returns  : AdminIDSeq - a list of all unique ID:s.
%%-----------------------------------------------------------
get_all_consumeradmins(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_consumerAdmIDs(State), State}.

%%----------------------------------------------------------%
%% function : get_all_supplieradmins
%% Arguments: -
%% Returns  : AdminIDSeq - a list of all unique ID:s.
%%-----------------------------------------------------------
get_all_supplieradmins(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_supplierAdmIDs(State), State}.


%%----- Inherit from CosNotification::QoSAdmin --------------
%%----------------------------------------------------------%
%% function : get_qos
%% Arguments: -
%% Returns  : CosNotification::QoSProperties
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%%-----------------------------------------------------------
get_qos(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_GlobalQoS(State), State}.

%%----------------------------------------------------------%
%% function : set_qos
%% Arguments: QoS - CosNotification::QoSProperties, i.e.,
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%% Returns  : ok | {'EXCEPTION', CosNotification::UnsupportedQoS}
%%-----------------------------------------------------------
set_qos(_OE_THIS, _OE_FROM, State, QoS) ->
    {GQoS,LQoS} = 'CosNotification_Common':set_qos(QoS, ?get_BothQoS(State), 
						   channel, false, 
						   ?get_allAdmins(State)),
    {reply, ok, ?set_BothQoS(State, GQoS,LQoS)}.

%%----------------------------------------------------------%
%% function : validate_qos
%% Arguments: Required_qos - CosNotification::QoSProperties
%%            [#'Property'{name, value}, ...] where name eq. string()
%%            and value eq. any().
%% Returns  : {'EXCEPTION', CosNotification::UnsupportedQoS} |
%%            {ok, CosNotification::NamedPropertyRangeSeq}
%%-----------------------------------------------------------
validate_qos(_OE_THIS, _OE_FROM, State, Required_qos) ->
    QoS = 'CosNotification_Common':validate_qos(Required_qos, ?get_BothQoS(State), 
						channel, false, 
						?get_allAdmins(State)),
    {reply, {ok, QoS}, State}.

%%----- Inherit from CosNotification::AdminPropertiesAdmin --
%%-----------------------------------------------------------%
%% function : get_admin
%% Arguments: -
%% Returns  : AdminProperties
%%-----------------------------------------------------------
get_admin(_OE_THIS, _OE_FROM, State) ->
    {reply, ?get_GlobalAdm(State), State}.

%%----------------------------------------------------------%
%% function : set_admin
%% Arguments: Admin
%% Returns  : 
%%-----------------------------------------------------------
set_admin(_OE_THIS, _OE_FROM, State, Admin) ->
    {GAdm,[MaxQ, MaxC, MaxS|_]} = 
	'CosNotification_Common':set_adm(Admin, ?get_GlobalAdm(State)),
    {reply, ok, ?set_AllAdminP(State, GAdm, MaxQ, MaxC, MaxS)}.

%%----- Inherit from CosEventChannelAdmin::EventChannel -----
%%----------------------------------------------------------%
%% function : for_consumers
%% Arguments: -
%% Returns  : ConsAdm
%%-----------------------------------------------------------
for_consumers(OE_THIS, _OE_FROM, State) ->
    is_admin_limit_reached(?get_MaxConsumers(State), ?get_consumerAmount(State)),
    AdminId = ?new_Id(State),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_ConsumerAdmin':oe_create_link([AdminId, OE_THIS, 
								     self(), 'AND_OP', 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, AdminCo} ->
	    %% Due to different storage, adding a new consumer is NOT done in the
	    %% same way as for suppliers.
	    NewState = ?add_consumerAdmin(State, AdminId, AdminCo, Pid),
	    {reply, AdminCo, ?set_IdCounter(NewState, AdminId)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:for_consumers();~n"
		      "Unable to create: CosNotifyChannelAdmin_ConsumerAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : for_suppliers
%% Arguments: -
%% Returns  : SuppAdm
%%-----------------------------------------------------------
for_suppliers(OE_THIS, _OE_FROM, State) ->
    is_admin_limit_reached(?get_MaxSuppliers(State), ?get_supplierAmount(State)),
    AdminId = ?new_Id(State),
    SO = 'CosNotification_Common':get_option(server_options, ?get_Options(State), 
					     ?not_DEFAULT_SETTINGS),
    case catch 'CosNotifyChannelAdmin_SupplierAdmin':oe_create_link([AdminId, OE_THIS, 
								     self(), 'AND_OP', 
								     ?get_GlobalQoS(State),
								     ?get_LocalQoS(State),
								     ?get_Options(State)],
								    [{sup_child, true}|SO]) of
	{ok, Pid, AdminSu} ->
	    %% Due to different storage, adding a new supplier is NOT done in the
	    %% same way as for consumers.
	    ?add_supplierAdmin(State, AdminId, AdminSu, Pid),
	    {reply, AdminSu, ?set_IdCounter(State, AdminId)};
	What ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:for_suppliers();~n"
		      "Unable to create: CosNotifyChannelAdmin_SupplierAdmin.~n"
		      "Reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------%
%% function : destroy
%% Arguments: -
%% Returns  : ok
%%------------------------------------------------------------
destroy(_OE_THIS, _OE_FROM, State) ->
    {stop, normal, ok, State}.

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj([]) -> {'EXCEPTION', #'CosNotifyChannelAdmin_AdminNotFound'{}};
find_obj([{_, Obj,_}]) -> Obj;
find_obj({value, {_, Obj,_}}) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyChannelAdmin_AdminNotFound'{}}.


find_field(List, Field) ->              
    find_field(List, Field, []).

find_field([], _, Acc) ->        
    Acc;
find_field([{I,_,_}|T], 1, Acc) -> 
    find_field(T, 1, [I|Acc]);
find_field([{_,O,_}|T], 2, Acc) -> 
    find_field(T, 2, [O|Acc]);
% Left out for now to avoid dialyzer warning.
%find_field([{_,_,P}|T], 3, Acc) -> 
%    find_field(T, 3, [P|Acc]);
find_field(What, _, _) -> 
    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:find_field();~n"
	      "Data corrupt: ~p", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

delete_obj(State, Pid) ->
    case ets:match_object(State#state.etsR, {'_', '_', Pid}) of
	[] ->
	    State#state{myConsumers=lists:keydelete(Pid, 3, State#state.myConsumers)};
	[{Id,_Obj,Pid}] ->
	    ets:delete(State#state.etsR, Id),
	    State
    end.

%% Test if we have reached limit for Admin objects.
is_admin_limit_reached(0, _) ->
    %% When set to zero it means that there is no limit.
    ok;
is_admin_limit_reached(Max, Current) when Current >= Max ->
    %% The Current value do not include the default Admin objects, hence
    %% we use >= instead of >.
    corba:raise(#'IMP_LIMIT'{completion_status=?COMPLETED_NO});
is_admin_limit_reached(_, _) ->
    ok.

%%-----------------------------------------------------------
%% function : callSeq
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callSeq(_OE_THIS, OE_FROM, State, Event, Status) ->
    corba:reply(OE_FROM, ok),
    forward(seq, State, Event, Status).

%%-----------------------------------------------------------
%% function : callAny
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
callAny(_OE_THIS, OE_FROM, State, Event, Status) ->
    corba:reply(OE_FROM, ok),
    forward(any, State, Event, Status).

%% Forward events
forward(Type, State, Event, Status) ->
    forward(Type, ?get_consumerAdmins(State), State, Event, Status).

forward(_, [], State, _, _) ->
    {noreply, State};
forward(Type, [{_,undefined,_}|T], State, Event, Status) ->
    %% Match if no default objects associated.
    forward(Type, T, State, Event, Status);
forward(any, [{_,H,_}|T], State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callAny(H, Event, Status) of
	ok ->
	    ?DBG("CHANNEL FORWARD ANY: ~p~n",[Event]),
	    forward(any, T, State, Event, Status);
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin no longer exists; dropping it: ~p", 
		      [?LINE, H], ?DEBUG_LEVEL),
	    NewState = ?del_consumerAdminRef(State,H),
	    forward(any, T, NewState, Event, Status);
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin behaves badly: ~p/~p", 
		      [?LINE, R, H], ?DEBUG_LEVEL),
	    forward(any, T, State, Event, Status);
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin behaves badly: ~p~n"
		      "Dropping it: ~p", [?LINE, R, H], ?DEBUG_LEVEL),
	    NewState = ?del_consumerAdminRef(State, H),
	    forward(any, T, NewState, Event, Status)
    end;
forward(seq, [{_,H,_}|T], State, Event, Status) ->
    case catch oe_CosNotificationComm_Event:callSeq(H, Event, Status) of
	ok ->
	    ?DBG("CHANNEL FORWARD SEQUENCE: ~p~n",[Event]),
	    forward(seq, T, State, Event, Status);
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin no longer exists; dropping it: ~p", 
		      [?LINE, H], ?DEBUG_LEVEL),
	    NewState = ?del_consumerAdminRef(State,H),
	    forward(seq, T, NewState, Event, Status);
	R when ?is_PersistentConnection(State) ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin behaves badly: ~p/~p", 
		      [?LINE, R, H], ?DEBUG_LEVEL),
	    forward(seq, T, State, Event, Status);
	R ->
	    orber:dbg("[~p] CosNotifyChannelAdmin_EventChannel:forward();~n"
		      "Admin behaves badly: ~p~n"
		      "Dropping it: ~p", [?LINE, R, H], ?DEBUG_LEVEL),
	    NewState = ?del_consumerAdminRef(State,H),
	    forward(seq, T, NewState, Event, Status)
    end.


%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
