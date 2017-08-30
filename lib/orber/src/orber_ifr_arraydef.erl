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
%% File    : orber_ifr_arraydef.erl
%% Purpose : Code for Arraydef
%%----------------------------------------------------------------------

-module(orber_ifr_arraydef).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 '_get_type'/1,
	 '_get_length'/1,
	 '_set_length'/2,
	 '_get_element_type'/1,
	 '_get_element_type_def'/1,
	 '_set_element_type_def'/2
	]).

-import(orber_ifr_utils,[get_field/2,
		   get_object/1,
		   set_object/1
		  ]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%%======================================================================
%%% ArrayDef (IDLType(IRObject))

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    orber_ifr_idltype:cleanup_for_destroy(
      '_get_element_type_def'({ObjType,ObjID})) ++
	orber_ifr_idltype:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_length'({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    get_field({ObjType,ObjID},length).

'_set_length'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ArrayDef, ObjType) ->
    ArrayDef = get_object({ObjType, ObjID}),
    New_ArrayDef =
	ArrayDef#ir_ArrayDef{type = {tk_array,
				     ArrayDef#ir_ArrayDef.type,
				     ArrayDef#ir_ArrayDef.length},
			     length = EO_Value},
    set_object(New_ArrayDef).

'_get_element_type'({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    get_field({ObjType,ObjID},element_type).

'_get_element_type_def'({ObjType, ObjID}) ?tcheck(ir_ArrayDef, ObjType) ->
    get_field({ObjType,ObjID},element_type_def).

'_set_element_type_def'({ObjType, ObjID}, EO_Value)
		       ?tcheck(ir_ArrayDef, ObjType) ->
    ArrayDef = get_object({ObjType, ObjID}),
    New_type = {tk_array,
		EO_Value#ir_IDLType.type,
		ArrayDef#ir_ArrayDef.length},
    New_ArrayDef = ArrayDef#ir_ArrayDef{type = New_type,
					element_type = New_type,
					element_type_def = EO_Value},
    set_object(New_ArrayDef).
