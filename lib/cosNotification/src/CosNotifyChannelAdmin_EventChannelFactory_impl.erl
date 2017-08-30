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
%% File    : CosNotifyChannelAdmin_EventChannelFactory_impl.erl
%% Purpose : 
%%-------------------------------------------------------------------

-module('CosNotifyChannelAdmin_EventChannelFactory_impl').

%%--------------- INCLUDES -----------------------------------
%% Application files
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").
-include("CosNotification_Definitions.hrl").

%%--------------- IMPORTS ------------------------------------

%%--------------- EXPORTS ------------------------------------
%% External
-export([create_channel/5,
	 get_all_channels/3,
	 get_event_channel/4]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%% Data structures
-record(state, {adminProp,
		idCounter = 0,
		options,
		etsR,
		server_options}).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: See gen_server documentation.
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?debug_print("INFO: ~p~n", [Info]),
    case Info of
        {'EXIT', Pid, normal} ->
	    ets:match_delete(State#state.etsR, {'_','_',Pid}),
            {noreply, State};
        _Other ->
            ?debug_print("TERMINATED: ~p~n",[_Other]),
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init(Options) ->
    process_flag(trap_exit, true),
    SO = 'CosNotification_Common':get_option(server_options, Options, ?not_DEFAULT_SETTINGS),
    {ok, #state{options = Options,
		etsR    = ets:new(oe_ets, [set, protected]),
		server_options = SO}}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : create_channel
%% Arguments: InitQoS
%%            InitAdmin
%% Returns  : Ch - Channel obj ref
%%            Id - Channel Id (out-type)
%%-----------------------------------------------------------
create_channel(OE_THIS, _OE_FROM, State, InitQoS, InitAdmin) ->
    {QoS, LQoS} = 'CosNotification_Common':init_qos(InitQoS),
    {IAdm, LAdm} = 'CosNotification_Common':init_adm(InitAdmin),
    Id = 'CosNotification_Common':create_id(State#state.idCounter),
    case 'CosNotifyChannelAdmin_EventChannel':oe_create_link([OE_THIS, QoS, IAdm, 
							      LQoS, LAdm, 
							      State#state.options],
							     [{sup_child, true}|State#state.server_options]) of
	{ok, Pid, Ch} ->
	    ets:insert(State#state.etsR, {Id,Ch,Pid}),
	    {reply, {Ch, Id}, State#state{idCounter=Id}};
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : get_all_channels
%% Arguments: -
%% Returns  : ChannelIDSeq - List of alive channels created 
%%            by this factory.
%%-----------------------------------------------------------
get_all_channels(_OE_THIS, _OE_FROM, State) ->
    {reply, lists:flatten(ets:match(State#state.etsR, {'$1','_','_'})), State}.

%%----------------------------------------------------------%
%% function : get_event_channel
%% Arguments: ChannelId
%% Returns  : ChannelRef | 'CosNotifyChannelAdmin_ChannelNotFound'
%%-----------------------------------------------------------
get_event_channel(_OE_THIS, _OE_FROM, State, Id) ->
    {reply, find_obj(ets:lookup(State#state.etsR, Id)), State}.

%%--------------- LOCAL FUNCTIONS ----------------------------
find_obj([]) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}};
find_obj([{_, Obj,_}]) -> Obj;
find_obj(_) -> {'EXCEPTION', #'CosNotifyChannelAdmin_ChannelNotFound'{}}.

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
