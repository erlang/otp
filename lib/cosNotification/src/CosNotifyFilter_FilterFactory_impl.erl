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
%%----------------------------------------------------------------------
%% File    : CosNotifyFilter_FilterFactory_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosNotifyFilter_FilterFactory_impl').


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
-export([create_filter/3,
	 create_mapping_filter/4]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%% Data structures
-record(state, {adminProp,
		etsR,
		options}).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: See gen_server documentation.
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    ?debug_print("INFO: ~p  DATA: ~p~n", [State, _Info]),
    {noreply, State}.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init(Options) ->
    process_flag(trap_exit, true),
    {ok, #state{options = Options}}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : create_filter
%% Arguments: InitGrammar - string()
%% Returns  : CosNotifyFilter::Filter | 
%%            {'EXCEPTION', InvalidGrammar}
%%-----------------------------------------------------------
create_filter(OE_THIS, State, InitGrammar) ->
    case lists:member(InitGrammar, ?not_SupportedGrammars) of
	true ->
	    SO = 'CosNotification_Common':get_option(server_options, State#state.options, 
						     ?not_DEFAULT_SETTINGS),
	    Fi='CosNotifyFilter_Filter':oe_create_link([OE_THIS, self(), 
							InitGrammar],
						       SO),
	    {reply, Fi, State};
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidGrammar'{})
    end.

%%----------------------------------------------------------%
%% function : create_mapping_filter
%% Arguments: InitGrammar - string()
%% Returns  : CosNotifyFilter::Filter | 
%%            {'EXCEPTION', InvalidGrammar}
%%-----------------------------------------------------------
create_mapping_filter(OE_THIS, State, InitGrammar, DefVal) ->
    case lists:member(InitGrammar, ?not_SupportedGrammars) of
	true ->
	    SO = 'CosNotification_Common':get_option(server_options, State#state.options, 
						     ?not_DEFAULT_SETTINGS),
	    Fi='CosNotifyFilter_MappingFilter':oe_create_link([OE_THIS, self(), 
							       InitGrammar, DefVal],
							      SO),
	    {reply, Fi, State};
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidGrammar'{})
    end.

%%--------------- LOCAL FUNCTIONS ----------------------------
%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
