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
%% File    : orber_ifr_aliasdef.erl
%% Purpose : Code for Aliasdef
%%----------------------------------------------------------------------

-module(orber_ifr_aliasdef).

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
	 '_get_original_type_def'/1,
	 '_set_original_type_def'/2
	]).

-import(orber_ifr_utils,[get_field/2,
		   get_object/1,
		   set_object/1
		  ]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%%======================================================================
%%% AliasDef (TypedefDef(Contained(IRObject), IDLType(IRObject)))

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_idltype:cleanup_for_destroy(
      '_get_original_type_def'({ObjType,ObjID})) ++
	orber_ifr_typedef:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%% Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_AliasDef,ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version)
    ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ?tcheck(ir_AliasDef, ObjType) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_original_type_def'({ObjType, ObjID})
				 ?tcheck(ir_AliasDef, ObjType) ->
    get_field({ObjType,ObjID},original_type_def).

'_set_original_type_def'({ObjType, ObjID}, EO_Value)
				 ?tcheck(ir_AliasDef, ObjType) ->
    AliasDef = get_object({ObjType, ObjID}),
    New_AliasDef = AliasDef#ir_AliasDef{type = {tk_alias,
						AliasDef#ir_AliasDef.id,
						AliasDef#ir_AliasDef.name,
						EO_Value#ir_IDLType.type},
					original_type_def = EO_Value},
    set_object(New_AliasDef).
