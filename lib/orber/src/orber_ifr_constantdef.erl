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
%% File    : orber_ifr_constantdef.erl
%% Purpose : Code for Constantdef
%%----------------------------------------------------------------------

-module(orber_ifr_constantdef).

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
	 '_get_type'/1,
	 '_get_type_def'/1,
	 '_set_type_def'/2,
	 '_get_value'/1,
	 '_set_value'/2
	]).

-import(orber_ifr_utils,[get_field/2,
		   set_field/3,
		   get_object/1,
		   set_object/1
		  ]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%%======================================================================
%%% ConstantDef (Contained(IRObject))

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType,ObjID}) ?tcheck(ir_ConstantDef,ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType, ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType,ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_idltype:cleanup_for_destroy(
      '_get_type_def'({ObjType,ObjID})) ++
	orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%% Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_ConstantDef,ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID})
			    ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).
						%
move({ObjType, ObjID}, New_container, New_name, New_version)
    ?tcheck(ir_ConstantDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_type'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    get_field({ObjType,ObjID},type).

'_get_type_def'({ObjType,ObjID}) ?tcheck(ir_ConstantDef,ObjType) ->
    get_field({ObjType,ObjID},type_def).

'_set_type_def'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ConstantDef, ObjType) ->
    ConstantDef = get_object({ObjType, ObjID}),
    New_ConstantDef = ConstantDef#ir_ConstantDef{type=EO_Value#ir_IDLType.type,
						 type_def = EO_Value},
    set_object(New_ConstantDef).

'_get_value'({ObjType, ObjID}) ?tcheck(ir_ConstantDef, ObjType) ->
    get_field({ObjType,ObjID},value).

'_set_value'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ConstantDef, ObjType) ->
    Typecode = get_field({ObjType,ObjID},type),
    {Value_typecode, _} = EO_Value,
    case Value_typecode == Typecode of
	true ->
	    set_field({ObjType, ObjID}, value, EO_Value);
	false ->
	    orber:dbg("[~p] ~p:destroy(~p, ~p, ~p);~n"
		      "Wrong typecode in set_value.~n", 
		      [?LINE, ?MODULE, ObjType, ObjID, EO_Value], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
    end.
