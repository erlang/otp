%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%% File    : cosProperty.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(cosProperty).
 
%%--------------- INCLUDES -----------------------------------
-include("cosProperty.hrl").
-include_lib("cosProperty/include/CosPropertyService.hrl").

%%--------------- EXPORTS-------------------------------------
%% cosProperty API external
-export([start/0, 
	 start_SetDefFactory/0,
	 start_SetFactory/0,
	 stop_SetDefFactory/1,
	 stop_SetFactory/1,
	 stop/0, 
	 install/0, 
	 install/1, 
	 install_db/0, 
	 install_db/1, 
	 install_db/2, 
	 uninstall/0,
	 uninstall/1,
	 uninstall_db/0]).

%% cosProperty API internal
-export([create_link/3, 
	 get_option/3, 
	 type_check/2, 
	 query_result/1,
	 start_PropertiesIterator/1, 
	 start_PropertyNamesIterator/1,
	 create_static_SetDef/2]).
 
%% Application callbacks
-export([start/2, init/1, stop/1]).

%%--------------- DEFINES ------------------------------------

-define(SUPERVISOR_NAME, oe_cosPropertySup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).
-define(SUP_PROP_SPEC(T,I), 
        ['CosPropertyService_PropertiesIterator',I, 
         [{sup_child, true}, {regname, {global, T}}]]).
-define(SUP_NAMES_SPEC(T,I), 
        ['CosPropertyService_PropertyNamesIterator',I, 
         [{sup_child, true}, {regname, {global, T}}]]).
-define(SUP_CHILD, 
        {"oe_PropertyChild",
         {cosProperty,create_link, []},
	 transient,100000,worker,
         []}).

%%------------------------------------------------------------
%% function : install
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------
install() ->
    install([]).
install(_Options) ->
    case catch oe_CosProperty:'oe_register'() of
        ok ->
	    ok;
        {'EXIT',{unregistered,App}} ->
            ?write_ErrorMsg("Unable to register cosProperty; application ~p not registered.~n", 
			    [App]),
            exit({unregistered,App});
        {'EXCEPTION',_} ->
            ?write_ErrorMsg("Unable to register cosProperty; propably already registered.
You are adviced to confirm this.~n", []),
            exit({error, "Register in the IFR failed."});
        Reason ->
            ?write_ErrorMsg("Unable to register cosProperty; reason ~p", [Reason]),
	    exit({error, "Register in the IFR failed."})
    end.

%%------------------------------------------------------------
%% function : install_db
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Install necessary data in the IFR DB
%%------------------------------------------------------------
install_db() -> 
    install_db(infinity, []).
install_db(Timeout) -> 
    install_db(Timeout, []).
install_db(Timeout, Options) ->
    case install_table(Timeout, Options) of
	ok ->
	    ok;
	{error, [DB_tables_created, Wait]} ->
	    ?write_ErrorMsg("Able to register cosProperty but failed adding table in mnesia (~p, ~p)", 
			    [DB_tables_created, Wait]),
	    exit({error, "Adding data in mnesia failed."});
	Why ->
	    ?write_ErrorMsg("Able to register cosProperty but failed adding table in mnesia with reason ~p", 
			    [Why]),
	    exit({error, "Adding data in mnesia failed."})
    end.   

%%------------------------------------------------------------
%% function : install_table
%% Arguments: -
%% Returns  : ok | {error, Data}
%% Effect   : Install necessary data in mnesia
%%------------------------------------------------------------
install_table(Timeout, Options) ->
    %% Fetch a list of the defined tables to see if 'oe_CosPropertyService' 
    %% is defined.
    AllTabs = mnesia:system_info(tables),
    DB_tables_created =
        case lists:member('oe_CosPropertyService', AllTabs) of
            true ->
                case lists:member({local_content, true},
                                  Options) of
                    true->
                        mnesia:add_table_copy('oe_CosPropertyService',
                                              node(),
                                              ram_copies);
                    _->
                        mnesia:create_table('oe_CosPropertyService',[{attributes,
                                                                record_info(fields,
                                                                            'oe_CosPropertyService')}
                                                               |Options])
                end;
            _ ->
                mnesia:create_table('oe_CosPropertyService',[{attributes,
                                                        record_info(fields,
                                                                    'oe_CosPropertyService')}
                                                       |Options])
        end,
    Wait = mnesia:wait_for_tables(['oe_CosPropertyService'], Timeout),
    %% Check if any error has occured yet. If there are errors, return them.
    if
        DB_tables_created == {atomic, ok},
        Wait == ok ->
            ok;
        true -> 
            {error, [DB_tables_created, Wait]}
    end.


%%------------------------------------------------------------
%% function : query_result
%% Arguments: -
%% Returns  : error | Data
%% Effect   : Check a read transaction
%%------------------------------------------------------------
query_result(Qres) -> 
    case Qres of
        {atomic, [Hres]} ->
            Hres#oe_CosPropertyService.properties;
        {atomic, [_Hres | _Tres]} ->
            error;
        {atomic, []} ->
            error;
        _Other ->
            error
    end.
 
%%------------------------------------------------------------
%% function : uninstall
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosProperty from the IFR DB
%%------------------------------------------------------------
uninstall() -> 
    uninstall([]). 
uninstall(_Options) -> 
    application:stop(cosProperty),
    oe_CosProperty:oe_unregister().

%%------------------------------------------------------------
%% function : uninstall
%% Arguments: -
%% Returns  : ok | EXIT | EXCEPTION
%% Effect   : Remove data related to cosProperty from the IFR DB
%%------------------------------------------------------------
uninstall_db() ->
    application:stop(cosProperty),
    case mnesia:delete_table('oe_CosPropertyService') of
	{atomic, ok} ->
	    ok;
	{aborted, _Reason} ->
	    exit({error, "Removing data from mnesia failed."})
    end.

%%------------------------------------------------------------
%% function : create_static_SetDef
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosProperty application.
%%------------------------------------------------------------
create_static_SetDef(PropTypes, PropDefs) ->
    InitProps = propertyDef2local(PropDefs, []),
    'CosPropertyService_PropertySetDef':oe_create({static, fixed_readonly, PropTypes,
						   PropDefs, InitProps, 
						   ?PropertySetDef}, 
						  [{pseudo, true}]).
propertyDef2local([#'CosPropertyService_PropertyDef'
		   {property_name  = Name, 
		    property_value = Value, 
		    property_mode  = fixed_readonly}|T], Acc) ->
    propertyDef2local(T, [{Name, Value, fixed_readonly}|Acc]);
propertyDef2local([], Acc) ->
    Acc;
propertyDef2local(_, _) ->
    exit({error, "Bad Mode type supplied. Must be fixed_readonly"}).

%%------------------------------------------------------------
%% function : start/stop
%% Arguments: 
%% Returns  : 
%% Effect   : Starts or stops the cosProperty application.
%%------------------------------------------------------------
start() ->
    application:start(cosProperty).
stop() ->
    application:stop(cosProperty).



%%-----------------------------------------------------------%
%% function : start_SetDefFactory
%% Arguments: -
%% Returns  : A PropertySetDefFactory reference.
%% Effect   : 
%%------------------------------------------------------------
start_SetDefFactory() ->
    'CosPropertyService_PropertySetDefFactory':oe_create([], [{pseudo, true}]).

%%-----------------------------------------------------------%
%% function : start_SetFactory
%% Arguments: -
%% Returns  : A PropertySetFactory reference.
%% Effect   : 
%%------------------------------------------------------------
start_SetFactory() ->
    'CosPropertyService_PropertySetFactory':oe_create([], [{pseudo, true}]).
    
%%-----------------------------------------------------------%
%% function : stop_SetDefFactory
%% Arguments: Factory - A PropertySetDefFactory reference.
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_SetDefFactory(Factory) ->
    corba:dispose(Factory).

%%-----------------------------------------------------------%
%% function : stop_SetFactory
%% Arguments: Factory - A PropertySetFactory reference.
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
stop_SetFactory(Factory) ->
    corba:dispose(Factory).

%%------------------------------------------------------------
%% function : start
%% Arguments: Type - see module application
%%            Arg  - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, cosProperty, app_init).
 
 
%%------------------------------------------------------------
%% function : stop
%% Arguments: Arg - see module application
%% Returns  : 
%% Effect   : Module callback for application
%%------------------------------------------------------------
stop(_) ->
    ok.
 
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
%% function : start_PropertiesIterator
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
start_PropertiesIterator(Args) ->
    Name = create_name(propertiesIterator),
    case supervisor:start_child(?SUPERVISOR_NAME, ?SUP_PROP_SPEC(Name, Args)) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	_Other->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.
   
%%-----------------------------------------------------------%
%% function : start_PropertyNamesIterator
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
start_PropertyNamesIterator(Args) ->
    Name = create_name(propertiesIterator),
    case supervisor:start_child(?SUPERVISOR_NAME, ?SUP_NAMES_SPEC(Name, Args)) of
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
    {MSec, Sec, USec} = erlang:now(),
    lists:concat(['oe_',node(),'_',Type,'_',MSec, '_', Sec, '_', USec]).

%%--------------- END OF MODULE ------------------------------


