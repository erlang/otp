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
%% File        : CosEventDomainAdmin_EventDomainFactory_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosEventDomainAdmin_EventDomainFactory_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("cosNotification/include/CosNotification.hrl").

-include("CosEventDomainAdmin.hrl").
-include("cosEventDomainApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

-export([create_event_domain/4, 
	 get_all_domains/2, 
	 get_event_domain/3]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {current_id = -1, domains = []}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Function   : handle_info/2
%% Returns    : {noreply, State}   |
%%              {stop, Reason, State}  
%% Description: Handle, for example, exit signals.
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) ->
    {noreply, State#state{domains=delete_domain(State#state.domains, Pid, [])}};
handle_info(_Info, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------
%% Function   : create_event_domain
%% Arguments  : InitialQoS - CosNotification::QoSProperties
%%              InitialAdmin - CosNotification::AdminProperties
%% Returns    : CosEventDomainAdmin::EventDomain |
%%              {'EXCEPTION', #'CosNotification_UnsupportedQoS'{}} |
%%              {'EXCEPTION', #'CosNotification_UnsupportedAdmin'{}} |
%% Description: 
%%----------------------------------------------------------------------
create_event_domain(_OE_This, State, InitialQoS, InitialAdmin) ->
    Id = cosEventDomainApp:create_id(State#state.current_id),
    Admin = cosEventDomainApp:get_admin(InitialAdmin),
    QoS = cosEventDomainApp:get_qos(InitialQoS),
    case catch 'CosEventDomainAdmin_EventDomain':oe_create_link([self(), Id, 
								 QoS, Admin], 
								[{sup_child, true}]) of
	{ok, Pid, ED} ->
	    {reply, {ED, Id}, State#state{current_id = Id, 
					  domains = [{Id, ED, Pid}|State#state.domains]}};
	What ->
	    orber:dbg("[~p] CosEventDomainAdmin_EventDomainFactory:"
		      "create_event_domain();~n"
		      "Failed creatin a new EventDomain due to: ~p", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.
		    
%%----------------------------------------------------------------------
%% Function   : get_all_domains
%% Arguments  : -
%% Returns    : CosEventDomainAdmin::DomainIDSeq - [long()]
%% Description: 
%%----------------------------------------------------------------------
get_all_domains(_OE_This, State) ->
    {reply, get_all_domains_helper(State#state.domains, []), State}.

get_all_domains_helper([], Acc) ->
    Acc;
get_all_domains_helper([{Id, _, _}|T], Acc) ->
    get_all_domains_helper(T, [Id|Acc]).


%%----------------------------------------------------------------------
%% Function   : get_event_domain
%% Arguments  : CosEventDomainAdmin::DomainID - long()
%% Returns    : CosEventDomainAdmin::EventDomain |
%%              {'EXCEPTION', #'CosEventDomainAdmin_DomainNotFound'{}} |
%% Description: 
%%----------------------------------------------------------------------
get_event_domain(_OE_This, State, DomainID) ->
    {reply, get_event_domain_helper(State#state.domains, DomainID), State}.

get_event_domain_helper([], _) ->
    corba:raise(#'CosEventDomainAdmin_DomainNotFound'{});
get_event_domain_helper([{Id, ED, _}|_], Id) ->
    ED;
get_event_domain_helper([_|T], Id) ->
    get_event_domain_helper(T, Id).

%%======================================================================
%% Internal functions
%%======================================================================
delete_domain([], _, Acc) ->
    %% The domain didn't exist.
    Acc;
delete_domain([{_Id, _, Pid}], Pid, Acc) ->
    Acc;
delete_domain([{_Id, _, Pid}|T], Pid, Acc) ->
    T++Acc;
delete_domain([H|T], Pid, Acc) ->
    delete_domain(T, Pid, [H|Acc]).

%%======================================================================
%% END OF MODULE
%%======================================================================

