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
%%-----------------------------------------------------------------
%% File: orber_interceptors.erl
%% 
%% Description:
%%    This file contains the code for calling interceptors
%%
%%-----------------------------------------------------------------
-module(orber_interceptors).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([call_send_message_interceptors/2, call_receive_message_interceptors/1,
	 call_request_interceptors/2]).
-export([push_system_message_interceptor/2, pop_system_message_interceptor/1,
	create_interceptor_table/0]).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
call_receive_message_interceptors(Bytes) ->
    case getInMessageInterceptors() of
	[] ->
	    Bytes;
	Interceptors ->
	    apply_message_interceptors(Interceptors, receive_message, corba:create_nil_objref(),
				       lists:flatten(Bytes))
    end.
call_send_message_interceptors(ObjRef, Bytes) ->
    case getOutMessageInterceptors() of
	[] ->
	    Bytes;
	Interceptors ->
	    apply_message_interceptors(Interceptors, send_message, ObjRef, lists:flatten(Bytes))
    end.


call_request_interceptors(in, Bytes) ->
    case getInRequestInterceptors() of
	[] ->
	    Bytes;
	Interceptors ->
	    Bytes
    end;
call_request_interceptors(out, Bytes) ->
        case getOutRequestInterceptors() of
	[] ->
	    Bytes;
	Interceptors ->
	    Bytes
    end.

create_interceptor_table() ->
    %% Should be replicated mnesia
    ets:new(orber_interceptors, [protected, named_table, set]),
    ets:insert(orber_interceptors, {message_in_interceptors, []}),
    ets:insert(orber_interceptors, {message_out_interceptors, []}).

push_system_message_interceptor(in, Mod) ->
    case ets:lookup(orber_interceptors, message_in_interceptors) of
	[{_, Interceptors}] ->
	    ets:insert(orber_interceptors, {message_in_interceptors, [Mod | Interceptors]});
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
push_system_message_interceptor(out, Mod) ->
    case ets:lookup(orber_interceptors, message_out_interceptors) of
	[{_, Interceptors}] ->
	    ets:insert(orber_interceptors, {message_out_interceptors, Interceptors ++ [Mod]});
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.    

pop_system_message_interceptor(in) ->
    case ets:lookup(orber_interceptors, message_in_interceptors) of
	[{_, []}] ->
	    ok;
	[{_, [_ | Interceptors]}] ->
	    ets:insert(orber_interceptors, {message_in_interceptors, Interceptors});
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
pop_system_message_interceptor(out) ->
    case ets:lookup(orber_interceptors, message_out_interceptors) of
	[{_, []}] ->
	    ok;
	[{_, Interceptors}] ->
	    ets:insert(orber_interceptors, {message_out_interceptors,  remove_last_element(Interceptors)});
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.    

    

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
getInMessageInterceptors() ->
    case ets:lookup(orber_interceptors, message_in_interceptors) of
	[{_, Interceptors}] ->
	    Interceptors;
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.    

getOutMessageInterceptors() ->
    case ets:lookup(orber_interceptors, message_out_interceptors) of
	[{_, Interceptors}] ->
	    Interceptors;
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.    


getInRequestInterceptors() ->
    [].

getOutRequestInterceptors() ->
    [].


apply_message_interceptors([], F, ObjRef, Bytes) ->
    Bytes;
apply_message_interceptors([M | Rest], F, ObjRef, Bytes) ->
    apply_message_interceptors(Rest, F, ObjRef, apply(M, F, [ObjRef, Bytes])).


remove_last_element([]) ->
    [];
remove_last_element([M]) ->
    [];
remove_last_element([M |Tail]) ->
    remove_last_element([Tail]).

    
