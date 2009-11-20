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
%%--------------------------------------------------------------------
%% File    : CosNotification_Common.erl
%% Purpose : 
%%--------------------------------------------------------------------

-module('CosNotification_Common').


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
%% External MISC
-export([get_option/3, 
         create_name/2, 
         create_name/1,
	 create_id/0,
	 create_id/1,
         is_debug_compiled/0,
	 type_check/2,
         send_stubborn/5,
         create_link/3, 
	 disconnect/3, 
	 do_disconnect/3,
	 notify/1]).

%% Internal AdminProperties
-export([init_adm/1,
	 set_adm/2,
	 'MaxQueueLength'/6,
	 'MaxConsumers'/6,
	 'MaxSuppliers'/6]).
%% Internal QoS
-export([init_qos/1,
	 set_qos/5,
	 validate_qos/5,
	 validate_event_qos/2,
	 'EventReliability'/6,
	 'ConnectionReliability'/6,
	 'Priority'/6,
	 'StartTimeSupported'/6,
	 'StopTimeSupported'/6,
	 'Timeout'/6,
	 'OrderPolicy'/6,
	 'DiscardPolicy'/6,
	 'MaximumBatchSize'/6,
	 'PacingInterval'/6,
	 'MaxEventsPerConsumer'/6]).

%%--------------- DEFINITIONS OF CONSTANTS -------------------
%%--------------- EXTERNAL MISC FUNCTIONS --------------------
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
%% Returns  : id (long) =/= 0
%%            Both default Admin:s have the unique id 0 (OMG spec, 98-11-01, 
%%            Notification p 148), hence, we may not return 0.
%% Exception: 
%% Purpose  : Throughout the CosNotification service we use,
%%            according to the OMG specification, id:s (long),
%%            which must be "unique", to retrieve object references.
%%            For example: CosNotifyChannelAdmin::ChannelId/AdminID.
%%------------------------------------------------------------
create_id(-1) ->
    1;
create_id( 2147483647) ->
    -2147483648;
create_id(OldID) ->
    OldID+1.

create_id() ->
    {_A,_B,C}=now(),
    C.
 
%%-----------------------------------------------------------%
%% function : type_check
%% Arguments: Obj  - objectrefernce to test.
%%            Mod  - Module which contains typeID/0.
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%------------------------------------------------------------
type_check(Obj, Mod) ->
    case cosNotificationApp:type_check() of
	false ->
	    ok;
	_ ->
	    case catch corba_object:is_a(Obj,Mod:typeID()) of
		true ->
		    ok;
		false ->
		    orber:dbg("[~p] CosNotification_Common:type_check(~p);~n"
			      "The supplied Object is not or does not inherrit from: ~p", 
			      [?LINE, Obj, Mod], ?DEBUG_LEVEL),
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
		{'EXCEPTION', E} ->
		    orber:dbg("[~p] CosNotification_Common:type_check(~p, ~p);~n"
			      "Failed due to: ~p", 
			      [?LINE, Obj, Mod, E], ?DEBUG_LEVEL),
		    corba:raise(E);
		What ->
		    orber:dbg("[~p] CosNotification_Common:type_check(~p, ~p);~n"
			      "Failed due to: ~p", 
			      [?LINE, Obj, Mod, What], ?DEBUG_LEVEL),
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end
    end.
	

%%-----------------------------------------------------------%
%% function : notify
%% Arguments: Items - [Item]
%%            Item - {proxy, IOR} | {client, IOR} | {reason, term()}
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%------------------------------------------------------------
notify(Items) ->
    case cosNotificationApp:notify() of
	false ->
	    ok;
	Module ->
	    catch Module:terminated(Items),
	    ok
    end.
	

%%------------------------------------------------------------
%% function : send_stubborn
%% Arguments: M - module
%%            F - function
%%            A - arguments
%%            MaxR - Maximum no retries
%%            Wait - sleep Wait seconds before next try.
%% Returns  : see effect
%% Exception: 
%% Effect   : Retries repeatidly untill anything else besides
%%            'EXIT', 'COMM_FAILURE' or 'OBJECT_NOT_EXIST'
%%------------------------------------------------------------
 
send_stubborn(M, F, A, MaxR, Wait) when is_list(A) ->
    send_stubborn(M, F, A, MaxR, Wait, 0);
send_stubborn(M, F, A, MaxR, Wait) ->
    send_stubborn(M, F, [A], MaxR, Wait, 0).
send_stubborn(M, F, A, MaxR, _Wait, MaxR) ->
    orber:dbg("[~p] CosNotification_Common:send_stubborn( ~p ~p ~p ~p).~n"
	      "Failed to deliver the event.~n", [?LINE, M,F,A,MaxR], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
send_stubborn(M, F, A, MaxR, Wait, Times) ->
    ?debug_print("~p:~p(~p)  # of retries: ~p~n", [M,F,A, Times]),    
    case catch apply(M,F,A) of
        {'EXCEPTION', E} when is_record(E, 'COMM_FAILURE')->
            NewTimes = Times +1,
            timer:sleep(Wait),
            send_stubborn(M, F, A, MaxR, Wait, NewTimes);
        {'EXIT', _} ->
            NewTimes = Times +1,
            timer:sleep(Wait),
            send_stubborn(M, F, A, MaxR, Wait, NewTimes);
        Other ->
            Other
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
    spawn(?MODULE, do_disconnect, [Module, Function, Object]),
    ok.

do_disconnect(Module, Function, Object) ->
    catch Module:Function(Object),
    ?DBG("Disconnect ~p:~p(..).~n", [Module, Function]),
    ok.


 
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
 

%%------------------------------------------------------------
%%--------------- AdminPropertiesAdmin -----------------------
%%------------------------------------------------------------
%%------------------------------------------------------------
%% function : init_adm
%% Arguments: Wanted  - requested Admins to be set.
%% Returns  : #'CosNotification_UnsupportedAdmin'{} |
%%            {NewAdmProperties, [MaxQ, MaxC, MaxS]}
%% Effect   : may only be used when creating a channel!!!!!!!!
%%------------------------------------------------------------
init_adm(Wanted) ->
    {NewA,_} = set_properties(Wanted, ?not_DEFAULT_ADMINPROPERTIES, channelAdm, 
			      ?not_SUPPORTED_ADMINPROPERTIES, [], [], 
			      false, false, false),
    {NewA, [extract_value(NewA, ?not_MaxQueueLength),
	    extract_value(NewA, ?not_MaxConsumers),
	    extract_value(NewA, ?not_MaxSuppliers)]}.

set_adm(Wanted, Current) ->
    {NewA,_} = set_properties(Wanted, Current, channelAdm, 
			      ?not_SUPPORTED_ADMINPROPERTIES, 
			      [], [], false, false, false),
    {NewA, [extract_value(NewA, ?not_MaxQueueLength),
	    extract_value(NewA, ?not_MaxConsumers),
	    extract_value(NewA, ?not_MaxSuppliers)]}.

'MaxQueueLength'(Req,channelAdm,_, _, _, _) -> admin_ok(Req).
'MaxConsumers'(Req,channelAdm,_, _, _, _)->    admin_ok(Req).
'MaxSuppliers'(Req,channelAdm,_, _, _, _)->    admin_ok(Req).

admin_ok(Req) ->
    case any:get_value(Req#'CosNotification_Property'.value) of
	Val when is_integer(Val) andalso Val >= 0 ->
	    {ok, Req};
	_  ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.


%%------------------------------------------------------------
%%--------------- QOS FUNCTIONS ------------------------------
%%------------------------------------------------------------
%%------------------------------------------------------------
%% function : init_qos
%% Arguments: Wanted  - requested QoS to be set.
%% Returns  : see set_properties/9
%% Effect   : may only be used when creating a channel!!!!!!!!
%%------------------------------------------------------------
init_qos(Wanted) ->
    LQS = set_local_qos(?not_DEFAULT_QOS, ?not_CreateInitQoS()),
    set_properties(Wanted, ?not_DEFAULT_QOS, channel, ?not_SUPPORTED_QOS, 
		   [], [], false, [], LQS).

%%------------------------------------------------------------
%% function : set_qos/5
%% Arguments: Wanted  - requested QoS to be set.
%%            Current - current QoS OMG style
%%            LQS     - local representation of QoS.
%%            Type    - channel | admin | proxy
%%            Parent  - Factory if Channel, Channel if Admin etc
%%            Childs  - Admins if Channel etc
%% Returns  : see set_properties/9
%%------------------------------------------------------------
set_qos(Wanted, {Current, LQS}, proxy, Parent, _) ->
    set_properties(Wanted, Current, proxy, ?not_SUPPORTED_QOS, [], [], Parent, false,LQS);
set_qos(Wanted, {Current, LQS}, admin, Parent, Childs) ->
    set_properties(Wanted, Current, admin, ?not_SUPPORTED_QOS, [], [], Parent, Childs,LQS);
set_qos(Wanted, {Current, LQS}, channel, _, Childs) ->
    set_properties(Wanted, Current, channel, ?not_SUPPORTED_QOS, [], [], false, Childs,LQS).

%%------------------------------------------------------------
%% function : 
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : ok - if requested equal to current value.
%%            {ok, Req, LQS} - if new and allowed QoS
%%            {unsupported,#'CosNotification_PropertyError'{}} otherwise.
%% Effect   : 
%%------------------------------------------------------------
'EventReliability'(Req,channel, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetConnectionReliability(LQS), ?not_BestEffort, ?not_Persistent} of
	{Val, Val, _, _} ->
	    %% Is the value requested.
	    ok;
	{Val, _, Val, _} ->
	    {ok, Req, LQS};
	{Val, _, _, Val} ->
	    {ok, Req, LQS};
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end;
'EventReliability'(Req,_,_,_,_,_) ->
    %% only valid to set this QoS for channels (or per-event).
    {unsupported, 
     #'CosNotification_PropertyError'{
       code = 'UNAVAILABLE_PROPERTY', 
       name = Req#'CosNotification_Property'.name,
       available_range = #'CosNotification_PropertyRange'{
	 low_val=any:create(orber_tc:null(), null), 
	 high_val=any:create(orber_tc:null(), null)
	}
      }
    }.

%%------------------------------------------------------------
%% function : 'ConnectionReliability'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
%% The most complex QoS to set is ConnectionReliability, and the reason for this
%% is that we cannot set the Channel to offer best effort while its children
%% offer persistent. A child may only offer Persistent if its parent do, which
%% is why we must check the following:
%%           
%%                    #    Persistent        Change to       Best Effort
%%            _____
%%           |     | (1)                         ->       Check if children BE
%%           |Chann| (2)      ok                 <-
%%            -----
%%              |
%%            _____
%%           |     | (3)                         ->      Check if children BE
%%           |Admin| (4)  Check if parent Pers.  <-      
%%            -----
%%              |
%%            _____
%%           |     | (5)                         ->               ok
%%           |Proxy| (6) Check if parent Pers.   <-
%%            -----
%% NOTE: a parent always exists but we may change the QoS before creating any
%% childrens. The cases (2) and (5) is always ok, i.e., no need to confirm
%% with parent or children.
%%------------------------------------------------------------
'ConnectionReliability'(Req, channel, _Curr, _Parent, Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetConnectionReliability(LQS), ?not_BestEffort, ?not_Persistent} of
	{Val, Val, _, _} ->
	    %% Is the value requested.
	    ok;
	{Val, P, Val, P} ->
	    %% Requested is BestEffort, Current Persistent => (1)
	    check_with_relatives(Childs, Req, LQS);
	{Val, B, B, Val} ->
	    %% Requested is Persistent, Current BestEffort => (2)
	    {ok, Req, LQS};
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end;
'ConnectionReliability'(Req, admin, _Curr, Parent, Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetConnectionReliability(LQS), ?not_BestEffort, ?not_Persistent} of
	{Val, Val, _, _} ->
	    %% Is the value requested.
	    ok;
	{Val, P, Val, P} ->
	    %% Requested is BestEffort, Current Persistent => (3)
	    check_with_relatives(Childs, Req, LQS);
	{Val, B, B, Val} ->
	    %% Requested is Persistent, Current BestEffort => (4)
	    check_with_relatives([Parent], Req, LQS);
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end;
'ConnectionReliability'(Req, proxy, _Curr, Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetConnectionReliability(LQS), ?not_BestEffort, ?not_Persistent} of
	{Val, Val, _, _} ->
	    %% Is the value requested.
	    ok;
	{Val, P, Val, P} ->
	    %% Requested is BestEffort, Current Persistent => (5)
	    {ok, Req, LQS};
	{Val, B, B, Val} ->
	    %% Requested is Persistent, Current BestEffort => (6)
	    check_with_relatives([Parent], Req, LQS);
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'Priority'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'Priority'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetPriority(LQS), ?not_HighestPriority, ?not_LowestPriority} of
	{Val, Val, _, _} ->
	    ok;
	{Val, _, H, L} when Val =< H, Val >= L ->
	    {ok, Req, LQS};
	{_, _, H, L} ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:short(), L), 
		 high_val=any:create(orber_tc:short(), H)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'StartTimeSupported'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'StartTimeSupported'(Req, _Type, _Curr, _, _, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetStartTimeSupported(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when Val =/= true, Val =/= false ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:boolean(), false), 
		 high_val=any:create(orber_tc:boolean(), true)
		}
	      }
	    };
	_->
	    {ok, Req, LQS}
    end.

%%------------------------------------------------------------
%% function : 'StopTimeSupported'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'StopTimeSupported'(Req, _Type, _Curr, _, _, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetStopTimeSupported(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when Val =/= true, Val =/= false ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:boolean(), false), 
		 high_val=any:create(orber_tc:boolean(), true)
		}
	      }
	    };
	_->
	    {ok, Req, LQS}
    end.

%%------------------------------------------------------------
%% function : 'Timeout'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'Timeout'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetTimeout(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when Val >= ?not_MinTimeout, Val =< ?not_MaxTimeout ->
	    {ok, Req, LQS};
	{Val, _} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinTimeout), 
		 high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxTimeout)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'OrderPolicy'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'OrderPolicy'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetOrderPolicy(LQS), 'CosNotification':'AnyOrder'(),
	  'CosNotification':'PriorityOrder'()} of
	{Val, Val,_,_} ->
	    ok;
	{Val, _, L, H} when Val >= L, Val =< H ->
	    {ok, Req, LQS};
	{Val, _, L, H} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:short(), L), 
		 high_val=any:create(orber_tc:short(), H)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.


%%------------------------------------------------------------
%% function : 'DiscardPolicy'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'DiscardPolicy'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetDiscardPolicy(LQS), ?not_AnyOrder, ?not_PriorityOrder} of
	{Val, Val,_,_} ->
	    ok;
	{Val, _, L, H} when Val >= L, Val =< H ->
	    {ok, Req, LQS};
	{Val, _, L, H} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:short(), L), 
		 high_val=any:create(orber_tc:short(), H)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'DiscardPolicy'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'MaximumBatchSize'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetMaximumBatchSize(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when Val >= ?not_MinBatchSize, Val =< ?not_MaxBatchSize ->
	    {ok, Req, LQS};
	{Val, _} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinBatchSize), 
		 high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxBatchSize)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'UNSUPPORTED_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:long(), ?not_MinBatchSize), 
		 high_val=any:create(orber_tc:long(), ?not_MaxBatchSize)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'PacingInterval'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Comment  : PacingInterval is defined to be:
%%            * TimeBase::UtcT (p 57, 2.5.5, OMG TC Document telecom/98-11-01)
%%            * TimeBase::TimeT (p 189, appendix B, OMG TC Document telecom/98-11-01)
%%            This implementation use TimeBase::TimeT, especially since
%%            TimeBase::UtcT contains information which are of no importance.
%%            When writing this, the OMG homepage contained no information
%%            regarding this.
%%------------------------------------------------------------
'PacingInterval'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetPacingInterval(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when Val >= ?not_MinPacing, Val =< ?not_MaxPacing ->
	    {ok, Req, LQS};
	{Val, _} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinPacing), 
		 high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxPacing)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_TYPE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : 'MaxEventsPerConsumer'/6
%% Arguments: Req    - Requested QoS, #'CosNotification_Property'{}
%%            Type   - Requestee, channel | admin | proxy
%%            Curr   - Current QoS, #'CosNotification_Property'{}
%%            Parent - false | ObjRef
%%            Childs - false | [ObjRef1, .., ObjRefN]
%%            LQS    - #qos{} defined in CosNotification_Definitions.hrl
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
'MaxEventsPerConsumer'(Req, _Type, _Curr, _Parent, _Childs, LQS) ->
    case {any:get_value(Req#'CosNotification_Property'.value),
	  ?not_GetMaxEventsPerConsumer(LQS)} of
	{Val, Val} ->
	    ok;
	{Val, _} when is_integer(Val) andalso
		      Val >= ?not_MinConsumerEvents andalso
		      Val =< ?not_MaxConsumerEvents ->
	    {ok, Req, LQS};
	{Val, _} when is_integer(Val) ->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'BAD_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:unsigned_long_long(), ?not_MinConsumerEvents), 
		 high_val=any:create(orber_tc:unsigned_long_long(), ?not_MaxConsumerEvents)
		}
	      }
	    };
	_->
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'UNSUPPORTED_VALUE', 
	       name = Req#'CosNotification_Property'.name,
	       available_range = 
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:long(), ?not_MinConsumerEvents), 
		 high_val=any:create(orber_tc:long(), ?not_MaxConsumerEvents)
		}
	      }
	    }
    end.

%%------------------------------------------------------------
%% function : validate_qos/5
%% Arguments: Wanted  - requested QoS to be set.
%%            Curr    - current QoS OMG style and LQS, local 
%%                      representation of QoS, grouped as {OMGQ, LQS}
%%            Type    - channel | admin | proxy
%%            Parent  - Factory if Channel, Channel if Admin etc
%%            Childs  - Admins if Channel etc
%% Returns  : NamedPropertySeq | #'CosNotification_UnsupportedQoS'{}
%%            case 1 if all supported, case 2 if at least 1 QoS not
%%            supported.
%% See also p59, 2.5.6.4, OMG TC Document telecom/98-11-01. Quote:
%% "If the supplied QoS is supported, it returns additional QoS
%%  properties which could be optionally added as well."
%%------------------------------------------------------------
validate_qos(Wanted, Curr, Type, Parent, Childs) ->
    %% If not supported this function will raise an exception, which we should
    %% not catch, but all we need to is to raise the exception as it is.
    {_, LQS}=set_qos(Wanted, Curr, Type, Parent, Childs),
    NewNPR = check_limits(LQS, ?not_QOS_LIMITS),
    remove_qos(Wanted, LQS, NewNPR).

remove_qos([], _, NPR) ->
    NPR;
remove_qos([H|T], LQS, NPR) ->
    NewNPR=remove(NPR, H#'CosNotification_Property'.name),
    remove_qos(T, LQS, NewNPR).

check_limits(LQS, NPR) ->
    case {?not_GetEventReliability(LQS), ?not_GetConnectionReliability(LQS), 
	  ?not_Persistent, ?not_BestEffort} of
	{P,P,P,_B} ->
	    New = #'CosNotification_NamedPropertyRange'
	      {name=?not_EventReliability,
	       range=
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:short(), ?not_BestEffort), 
		 high_val=any:create(orber_tc:short(), ?not_BestEffort)
		}},
	    NewNPR=change(NPR, ?not_EventReliability, New),
	    remove(NewNPR, ?not_ConnectionReliability);
	{_,B,_P,B} ->
	    remove(NPR, ?not_EventReliability);
	{B,P,P,B} ->
	    New = #'CosNotification_NamedPropertyRange'
	      {name=?not_ConnectionReliability,
	       range=
	       #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:short(), ?not_BestEffort), 
		 high_val=any:create(orber_tc:short(), ?not_BestEffort)
		}},
	    change(NPR, ?not_ConnectionReliability, New)
    end.

%%------------------------------------------------------------
%% function : validate_event_qos/2
%% Arguments: Wanted  - requested QoS to be set.
%%            Curr    - LQS, local representation of QoS
%% Returns  : NamedPropertySeq | #'CosNotification_UnsupportedQoS'{}
%%            case 1 if all supported, case 2 if at least 1 QoS not
%%            supported.
%%------------------------------------------------------------
validate_event_qos(Wanted, Curr) ->
    v_e_q_helper(Wanted, Curr, []),
    [].
v_e_q_helper([], _Curr, []) ->
    %% Parsed all and foynd no conflicts.
    ok;
v_e_q_helper([], _Curr, Unsupp) ->
    %% Not possible to use these requested QoS.
    corba:raise(#'CosNotification_UnsupportedQoS'{qos_err = Unsupp});

%%--- EventReliability ---%%
v_e_q_helper([#'CosNotification_Property'{name=?not_EventReliability, 
					  value=#any{value=?not_BestEffort}}|T], Curr, Unsupp) ->
    %% Always ok.
    v_e_q_helper(T, Curr, Unsupp);
v_e_q_helper([#'CosNotification_Property'{name=?not_EventReliability, 
					  value=#any{value=?not_Persistent}}|T], Curr, Unsupp) 
  when ?not_GetConnectionReliability(Curr) =/= ?not_BestEffort,
       ?not_GetEventReliability(Curr) =/= ?not_BestEffort,
       ?not_GetStopTimeSupported(Curr) =/= true ->
    v_e_q_helper(T, Curr, Unsupp);
v_e_q_helper([#'CosNotification_Property'{name=?not_EventReliability}|T], 
	     Curr, Unsupp) ->
    %% Impossible to set to Persistent if the connection reliability is best effort.
    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
			   {code = 'UNAVAILABLE_VALUE', name = ?not_EventReliability,
			    available_range = 
			    #'CosNotification_PropertyRange'{
			      low_val=any:create(orber_tc:null(), null), 
			      high_val=any:create(orber_tc:null(), null)}}|Unsupp]);

%%--- Priority ---%%
v_e_q_helper([#'CosNotification_Property'{name=?not_Priority, value=#any{value=V}}|T], Curr, 
	     Unsupp) ->
    if 
	?not_GetOrderPolicy(Curr) =/= ?not_AnyOrder,
	?not_GetOrderPolicy(Curr) =/= ?not_Priority,
	?not_GetDiscardPolicy(Curr) =/= ?not_Priority ->
	    %% No use setting Priority since it's not currently used.
	    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
				   {code = 'UNAVAILABLE_VALUE', name = ?not_Priority,
				    available_range = #'CosNotification_PropertyRange'{
				      low_val=any:create(orber_tc:null(), null), 
				      high_val=any:create(orber_tc:null(), null)
				     }}|Unsupp]);
	V =< ?not_HighestPriority, V >= ?not_LowestPriority ->
	    v_e_q_helper(T, Curr, Unsupp);
	true ->
	    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
				   {code = 'BAD_VALUE', name = ?not_Priority,
				    available_range = 
				    #'CosNotification_PropertyRange'{
				      low_val=any:create(orber_tc:short(), 
							 ?not_LowestPriority), 
				      high_val=any:create(orber_tc:short(), 
							  ?not_HighestPriority)}}|Unsupp])
    end;

%%--- StartTime ---%%
v_e_q_helper([#'CosNotification_Property'{name=?not_StartTime}|T], Curr, Unsupp) 
  when ?not_GetStartTimeSupported(Curr) =/= false,
       ?not_GetEventReliability(Curr) =/= ?not_Persistent ->
    v_e_q_helper(T, Curr, Unsupp);
v_e_q_helper([#'CosNotification_Property'{name=?not_StartTime}|T], Curr, Unsupp) ->
    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
			   {code = 'UNAVAILABLE_VALUE', name = ?not_StartTime,
			    available_range = #'CosNotification_PropertyRange'{
			      low_val=any:create(orber_tc:null(), null), 
			      high_val=any:create(orber_tc:null(), null)
			     }}|Unsupp]);

%%--- StopTime ---%%
v_e_q_helper([#'CosNotification_Property'{name=?not_StopTime}|T], Curr, Unsupp)
  when ?not_GetStopTimeSupported(Curr) =/= false,
       ?not_GetEventReliability(Curr) =/= ?not_Persistent ->
    v_e_q_helper(T, Curr, Unsupp);
v_e_q_helper([#'CosNotification_Property'{name=?not_StopTime}|T], Curr, Unsupp) ->
    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
			   {code = 'UNAVAILABLE_VALUE', name = ?not_StopTime,
			    available_range = #'CosNotification_PropertyRange'{
			      low_val=any:create(orber_tc:null(), null), 
			      high_val=any:create(orber_tc:null(), null)
			     }}|Unsupp]);

%%--- Timeout ---%%
v_e_q_helper([#'CosNotification_Property'{name=?not_Timeout}|T], Curr, Unsupp)
  when  ?not_GetStopTimeSupported(Curr) =/= false,
	?not_GetEventReliability(Curr) =/= ?not_Persistent ->
    v_e_q_helper(T, Curr, Unsupp);
v_e_q_helper([#'CosNotification_Property'{name=?not_Timeout}|T], Curr, Unsupp) ->
    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
			   {code = 'UNAVAILABLE_VALUE', name = ?not_Timeout,
			    available_range = #'CosNotification_PropertyRange'{
			      low_val=any:create(orber_tc:null(), null), 
			      high_val=any:create(orber_tc:null(), null)
			     }}|Unsupp]);

%%--- Unknown Event QoS ---%%
v_e_q_helper([#'CosNotification_Property'{name=Name}|T], Curr, Unsupp) ->
    %% Unsupported property.
    v_e_q_helper(T, Curr, [#'CosNotification_PropertyError'
			   {code = 'BAD_PROPERTY', name = Name,
			    available_range = #'CosNotification_PropertyRange'{
			      low_val=any:create(orber_tc:null(), null), 
			      high_val=any:create(orber_tc:null(), null)
			     }}|Unsupp]);
v_e_q_helper(What, _, _) ->
    %% Not a Property struct.
    orber:dbg("[~p] CosNotification_Common:v_e_q_helper(~p);~n"
	      "Not a CosNotification_Property struct.", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-------------- QOS HELP FUNCTIONS --------------------------
%%------------------------------------------------------------
%% function : set_properties/9
%% Arguments: Wanted  - requested QoS to be set.
%%            Current - current QoS OMG style
%%            Type    - channel | admin | proxy
%%            Supported - List of supported QoS
%%            Unsupp  - acc
%%            NewQoS  - acc
%%            Parent  - Factory if Channel, Channel if Admin etc
%%            Childs  - Admins if Channel etc
%%            LQS     - local representation of QoS.
%% Returns  : {NewOMGStyleQoS, NewLocalQoS} | #'CosNotification_UnsupportedQoS'{}
%%------------------------------------------------------------
set_properties([], Curr, channelAdm, _, [], NewQoS,_,_,LAS) ->
    merge_properties(NewQoS, Curr, LAS);
set_properties([], Curr, _, _, [], NewQoS,_,_,LQS) ->
    %% set_local_qos and merge_properties are help functions found at the end of QoS
    %% functions.
    NewLQS = set_local_qos(NewQoS, LQS),
    merge_properties(NewQoS, Curr, NewLQS);
set_properties([], _, channelAdm, _, Unsupp, _,_,_,_) ->
    corba:raise(#'CosNotification_UnsupportedAdmin'{admin_err = Unsupp});
set_properties([], _, _, _, Unsupp, _,_,_,_) ->
    corba:raise(#'CosNotification_UnsupportedQoS'{qos_err = Unsupp});

set_properties([Req|Tail], Curr, Type, Supported, Unsupp, NewQoS, Parent, Childs,LQS) ->
    %% set_values and is_supported are help functions found at the end of QoS
    %% functions.
    case set_values(is_supported(Supported, Req), Req, Type, Curr, Parent, Childs,LQS) of
	{unsupported, U} ->
	    set_properties(Tail, Curr, Type, Supported, [U|Unsupp], NewQoS, Parent, Childs,LQS);
	{ok, S, NewLQS} ->
	    set_properties(Tail, Curr, Type, Supported, Unsupp, [S|NewQoS], Parent, Childs,NewLQS);
	{ok, S} ->
	    set_properties(Tail, Curr, Type, Supported, Unsupp, [S|NewQoS], Parent, Childs,LQS);
	ok ->
	    set_properties(Tail, Curr, Type, Supported, Unsupp, NewQoS, Parent, Childs,LQS)
    end.
    

set_values(unsupported,Req,_,_,_,_,_) ->
    {unsupported, 
     #'CosNotification_PropertyError'{
       code = 'BAD_PROPERTY', 
       name = Req#'CosNotification_Property'.name,
       available_range = #'CosNotification_PropertyRange'{
	 low_val=any:create(orber_tc:null(), null), 
	 high_val=any:create(orber_tc:null(), null)
	}
      }
    };
set_values({ok, Func}, Req, Type, Curr, Parent, Childs, LQS) ->
    ?MODULE:Func(Req, Type, Curr, Parent, Childs, LQS).

%% Update OMG style QoS list with new values.
merge_properties([], NewCurrQoS, LQS) ->
    {NewCurrQoS, LQS}; 
merge_properties([H|T], Curr, LQS) ->
    merge_properties(T, lists:keyreplace(H#'CosNotification_Property'.name, %% get key.
					 #'CosNotification_Property'.name, %% get index.
					 Curr, H), LQS).

%% Is the Property S among our supported QoS?    
is_supported([], _) -> 
    unsupported;
is_supported([{Name, Func}|_], #'CosNotification_Property'{name=Name}) -> 
    {ok, Func};
is_supported([_|T], S) -> 
    is_supported(T, S).

%% Find matching S-Property from a list of OMG style QoS
extract([], _) -> unsupported;
extract([H|_T], S) when H#'CosNotification_Property'.name==
		       S#'CosNotification_Property'.name -> 
    {ok, H};
extract([_|T], S) -> extract(T,S).

%% Find matching Property name from a list of OMG style QoS
extract_value([], _) -> unsupported;
extract_value([H|_T], Key) when H#'CosNotification_Property'.name== Key ->
    {ok, any:get_value(H#'CosNotification_Property'.value)};
extract_value([_|T], Key) -> extract(T,Key).

%% Remove matching S-QoS from a list of OMG style QoS
remove(List, Key) -> 
    lists:keydelete(Key,
		    #'CosNotification_NamedPropertyRange'.name, %% get index.
		    List).

change(List, Key, New) ->
    lists:keyreplace(Key,
		     #'CosNotification_NamedPropertyRange'.name, %% get index.
		     List, New).
%% Get QoS from supplied objects and check if it's the same as S.
check_with_relatives([], S, LQS) ->
    {ok, S, LQS};
check_with_relatives([undefined|T], S, LQS) ->
    check_with_relatives(T, S, LQS);
check_with_relatives([H|T], S, LQS) ->
    case catch extract('CosNotification_QoSAdmin':get_qos(H), S) of
	{ok, S} ->
	    check_with_relatives(T, S, LQS);
	_->
	    %% Varioues reasons for this case (Object not responding, not supported)
	    {unsupported, 
	     #'CosNotification_PropertyError'{
	       code = 'UNAVAILABLE_PROPERTY', 
	       name = S#'CosNotification_Property'.name,
	       available_range = #'CosNotification_PropertyRange'{
		 low_val=any:create(orber_tc:null(), null), 
		 high_val=any:create(orber_tc:null(), null)
		}
	      }
	    }
    end.

%% Set new values to locally defined representation of QoS. Using this approach is 
%% necessary since we must state the record-field at compile-time.
set_local_qos([], LQS) -> LQS;
set_local_qos([#'CosNotification_Property'{name=N,value=V}|T], LQS) ->
    NewLQS = 
	case N of
	    "EventReliability" ->
		?not_SetEventReliability(LQS, any:get_value(V));  
	    "ConnectionReliability" ->
		?not_SetConnectionReliability(LQS, any:get_value(V));
	    "Priority" ->
		?not_SetPriority(LQS, any:get_value(V));
	    "Timeout" ->
		?not_SetTimeout(LQS, any:get_value(V));
	    "OrderPolicy" ->
		?not_SetOrderPolicy(LQS, any:get_value(V));
	    "DiscardPolicy" ->
		?not_SetDiscardPolicy(LQS, any:get_value(V));
	    "MaximumBatchSize" ->
		?not_SetMaximumBatchSize(LQS, any:get_value(V));
	    "PacingInterval" ->
		?not_SetPacingInterval(LQS, any:get_value(V));
	    "StartTimeSupported" ->
		?not_SetStartTimeSupported(LQS, any:get_value(V));
	    "StopTimeSupported" ->
		?not_SetStopTimeSupported(LQS, any:get_value(V));
	    "MaxEventsPerConsumer" ->
		?not_SetMaxEventsPerConsumer(LQS, any:get_value(V))
	end,
    set_local_qos(T, NewLQS).

%%%%%%%%%%%%%%%%% END QOS FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------- END OF MODULE ------------------------------
