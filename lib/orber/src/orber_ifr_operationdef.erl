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
%%----------------------------------------------------------------------
%% File    : orber_ifr_operationdef.erl
%% Purpose : Code for Operationdef
%%----------------------------------------------------------------------

-module(orber_ifr_operationdef).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 '_get_id'/1,
	 '_set_id'/2,
	 '_get_name'/1,
	 '_set_name'/2,
	 '_get_version'/1,
	 '_set_version'/2,
	 '_get_defined_in'/1,
	 '_get_absolute_name'/1,
	 '_get_containing_repository'/1,
	 describe/1,
	 move/4,
	 '_get_result'/1,
	 '_get_result_def'/1,
	 '_set_result_def'/2,
	 '_get_params'/1,
	 '_set_params'/2,
	 '_get_mode'/1,
	 '_set_mode'/2,
	 '_get_contexts'/1,
	 '_set_contexts'/2,
	 '_get_exceptions'/1,
	 '_set_exceptions'/2
	]).

-import(orber_ifr_utils,[get_field/2,
		   set_field/3,
		   get_object/1,
		   set_object/1
		  ]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%%======================================================================
%%% OperationDef (Contained(IRObject))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    lists:map(fun(X) -> Idl = X#parameterdescription.type_def,
			orber_ifr_idltype:cleanup_for_destroy(Idl)
	      end,
	      '_get_params'({ObjType,ObjID})) ++
	orber_ifr_idltype:cleanup_for_destroy('_get_result_def'({ObjType,
								 ObjID})) ++
	orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version)
		   ?tcheck(ir_OperationDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,New_version).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_result'({ObjType, ObjID})
			  ?tcheck(ir_OperationDef, ObjType) ->
    get_field({ObjType,ObjID},result).

'_get_result_def'({ObjType, ObjID})
			      ?tcheck(ir_OperationDef, ObjType) ->
    get_field({ObjType,ObjID},result_def).

'_set_result_def'({ObjType, ObjID}, EO_Value)
			      ?tcheck(ir_OperationDef, ObjType) ->
    OperationDef = get_object({ObjType, ObjID}),
    New_OperationDef =
	OperationDef#ir_OperationDef{result = EO_Value#ir_IDLType.type,
				     result_def = EO_Value},
    set_object(New_OperationDef).

'_get_params'({ObjType,ObjID}) ?tcheck(ir_OperationDef,ObjType) ->
    get_field({ObjType,ObjID},params).

'_set_params'({ObjType, ObjID}, EO_Value)
			  ?tcheck(ir_OperationDef, ObjType) ->
    set_field({ObjType,ObjID}, params, EO_Value).

'_get_mode'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    get_field({ObjType,ObjID},mode).

'_set_mode'({ObjType, ObjID}, EO_Value) ?tcheck(ir_OperationDef, ObjType) ->
    OperationDef = get_object({ObjType, ObjID}),
    Set_OK = case EO_Value of
		 'OP_ONEWAY' ->
		     (OperationDef#ir_OperationDef.result == tk_void)
	         and
	         lists:foldl(fun(#parameterdescription{mode=Mode},AccIn) ->
				     (Mode == 'PARAM_IN') and AccIn
			     end,
			     true,OperationDef#ir_OperationDef.params);
		 _ ->
		     true
	     end,
    set_mode(Set_OK,{ObjType,ObjID},EO_Value).

set_mode(true,Objref,EO_Value) ->
    set_field(Objref,mode,EO_Value);
set_mode(false, Objref, EO_Value) ->
    orber:dbg("[~p] ~p:destroy(~p, ~p);~n"
	      "Illegal '_set_mode'.~n", 
	      [?LINE, ?MODULE, Objref, EO_Value], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

'_get_contexts'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    get_field({ObjType,ObjID},contexts).

'_set_contexts'({ObjType, ObjID}, EO_Value)
			    ?tcheck(ir_OperationDef, ObjType) ->
    set_field({ObjType,ObjID}, contexts, EO_Value).

'_get_exceptions'({ObjType, ObjID}) ?tcheck(ir_OperationDef, ObjType) ->
    get_field({ObjType,ObjID},exceptions).

'_set_exceptions'({ObjType, ObjID}, EO_Value)
			      ?tcheck(ir_OperationDef, ObjType) ->
    set_field({ObjType,ObjID}, exceptions, EO_Value).
