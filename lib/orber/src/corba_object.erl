%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: corba_object.erl
%% 
%% Description:
%%    This file contains the CORBA::Object interface
%%
%%-----------------------------------------------------------------
-module(corba_object).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").

%%-----------------------------------------------------------------
%% Standard interface CORBA::Object
%%-----------------------------------------------------------------
-export([get_interface/1,
	 is_nil/1, 
	 is_a/2,
	 is_a/3,
	 is_remote/1,
	 non_existent/1,
	 non_existent/2,
	 not_existent/1,
	 not_existent/2,
	 is_equivalent/2,
	 hash/2,
	 create_request/6]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

%%------------------------------------------------------------
%% Implementation of standard interface
%%------------------------------------------------------------
get_interface(Obj) ->
    case orber_env:light_ifr() of
	false ->
	    TypeId = iop_ior:get_typeID(Obj),
	    case mnesia:dirty_index_read(ir_InterfaceDef, TypeId, #ir_InterfaceDef.id) of
		%% If all we get is an empty list there are no such
		%% object registered in the IFR.
		[] ->
		    orber:dbg("[~p] corba_object:get_interface(~p); TypeID ~p not found in IFR.", 
			      [?LINE, Obj, TypeId], ?DEBUG_LEVEL),
		    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
		[#ir_InterfaceDef{ir_Internal_ID=Ref}] ->
		    orber_ifr_interfacedef:describe_interface({ir_InterfaceDef, Ref})
	    end;
	true ->
	    case catch iop_ior:get_key(Obj) of
		{'external', _Key} ->
		    orber:dbg("[~p] corba_object:get_interface(~p); Invalid object reference.", 
			      [?LINE, Obj], ?DEBUG_LEVEL),
		    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
		{_Local, _Key, _, _, Module} ->
		    case catch Module:oe_get_interface() of
			{'EXIT', What} ->
			    orber:dbg("[~p] corba_object:get_interface(~p);~n"
				      "The call-back module does not exist or incorrect IC-version used.~n"
				      "Reason: ~p", [?LINE, Module, What], ?DEBUG_LEVEL),
			    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
			InterfaceDesc ->
			    InterfaceDesc
		    end
	    end
    end.
	    

is_nil(Object) when is_record(Object, 'IOP_IOR') ->
    iop_ior:check_nil(Object);
is_nil({I,T,K,P,O,F}) ->
    iop_ior:check_nil({I,T,K,P,O,F});
is_nil(Obj) ->
    orber:dbg("[~p] corba_object:is_nil(~p); Invalid object reference.", 
			    [?LINE, Obj], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

is_a(Obj, Logical_type_id) ->
    is_a(Obj, Logical_type_id, []).

is_a(?ORBER_NIL_OBJREF, _, _Ctx) ->
    false;
is_a(#'IOP_IOR'{type_id = Logical_type_id}, Logical_type_id, _Ctx) ->
    true;
is_a(Obj, Logical_type_id, Ctx) when is_list(Ctx) ->
    case catch iop_ior:get_key(Obj) of
	{'external', Key} ->
	    orber_iiop:request(Key, '_is_a', [Logical_type_id], 
			       {orber_tc:boolean(),[orber_tc:string(0)],[]},
			       true, infinity, Obj, corba:get_implicit_context(Ctx));
	{_Local, _Key, _, _, Module} ->
	    case catch Module:oe_is_a(Logical_type_id) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba_object:is_a(~p);~n"
			      "The call-back module does not exist or incorrect IC-version used.~n"
			      "Reason: ~p", [?LINE, Module, What], ?DEBUG_LEVEL),
		    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
		Boolean ->
		    Boolean
	    end;
	_ ->
	    orber:dbg("[~p] corba_object:is_a(~p, ~p); Invalid object reference.", 
		      [?LINE, Obj, Logical_type_id], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
is_a(Obj, Logical_type_id, Ctx) ->
    orber:dbg("[~p] corba_object:is_a(~p, ~p, ~p);~n"
	      "Failed to supply a context list.", 
	      [?LINE, Obj, Logical_type_id, Ctx], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


non_existent(Obj) ->
    non_existent(Obj, []).

non_existent(?ORBER_NIL_OBJREF, _Ctx) ->
    true;
non_existent(Obj, Ctx) ->
    existent_helper(Obj, '_non_existent', Ctx).

not_existent(Obj) ->
    not_existent(Obj, []).
    
not_existent(?ORBER_NIL_OBJREF, _Ctx) ->
    true;
not_existent(Obj, Ctx) ->
    existent_helper(Obj, '_not_existent', Ctx).


existent_helper(Obj, Op, Ctx) when is_list(Ctx) ->
    case catch iop_ior:get_key(Obj) of
	{'internal', Key, _, _, _} ->
	    case catch orber_objectkeys:get_pid(Key) of
		{'EXCEPTION', E} when is_record(E,'OBJECT_NOT_EXIST') ->
		    true;
		{'EXCEPTION', X} ->
		    corba:raise(X);
		{'EXIT', R} ->
		    orber:dbg("[~p] corba_object:non_existent(~p); exit(~p).", 
			      [?LINE, Obj, R], ?DEBUG_LEVEL),
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
		_ ->
		    false
	    end;
	{'internal_registered', Key, _, _, _} ->
	    case Key of
		{pseudo, _} ->
		    false;
		_->
		    case whereis(Key) of
			undefined ->
			    true;
			_P ->
			    false
		    end
	    end;
	{'external', Key} ->
	    orber_iiop:request(Key, Op, [], 
			       {orber_tc:boolean(), [],[]}, 'true', 
			       infinity, Obj, corba:get_implicit_context(Ctx));
	true -> 	
	    false
    end;
existent_helper(Obj, Op, Ctx) ->
    orber:dbg("[~p] corba_object:existent_helper(~p, ~p, ~p);~n"
	      "Failed to supply a context list.", 
	      [?LINE, Obj, Op, Ctx], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    

is_remote(Obj) ->
    case catch iop_ior:get_key(Obj) of
	{'external', _} ->
	    true;
	_ ->
	    false
    end.


is_equivalent(Obj, Obj) ->
    true;
is_equivalent({I,T,K,P,_,_}, {I,T,K,P,_,_}) ->
    true;
is_equivalent(_, _) ->
    false.

hash(Obj, Maximum) ->
    erlang:phash(iop_ior:get_key(Obj), Maximum).


create_request(_Obj, _Ctx, _Op, _ArgList, NamedValueResult, _ReqFlags) ->
    {ok, NamedValueResult, []}.
