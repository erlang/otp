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
%% File    : orber_ifr_contained.erl
%% Purpose : Code for Contained
%%----------------------------------------------------------------------

-module(orber_ifr_contained).

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
	 describe/2,				%not in CORBA 2.0
	 move/4
	]).

-import(orber_ifr_utils,[get_object/1,
		   get_field/2,
		   set_field/3,
		   construct/3,
		   select/2,
		   write_result/1,
		   ifr_transaction_read_write/1
		  ]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%%======================================================================
%%% Contained (IRObject)

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType,ObjID}) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

%%% Note, that the destroy function is meant to be called within a
%%% transaction called in the destroy function of an object which
%%% inherits from Contained. A Contained should only be destroyed by
%%% destroying the object that inherits from a Contained. An attempt
%%% to call this function in user code will result in unpredictable
%%% results.

%%% Don't type check the object reference. We need to be able to
%%% handle several types of objects that inherit from Contained.

destroy(Contained_objref) ->
    ObjList = cleanup_for_destroy(Contained_objref),
    orber_ifr_irobject:destroy([Contained_objref | ObjList]).

cleanup_for_destroy(Contained_objref) ->
    Defined_in = '_get_defined_in'(Contained_objref),
    [Container_obj] = mnesia:read(Defined_in),
    New_container_obj =
	construct(Container_obj,contents,
		  lists:filter(fun(X) -> X /= Contained_objref end,
			       select(Container_obj,contents))),
    [fun() -> mnesia:write(New_container_obj) end].
    
%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_id'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},id).

'_set_id'({ObjType,ObjID}, EO_Value) ->
    set_field({ObjType, ObjID}, id, EO_Value).

'_get_name'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},name).

'_set_name'({ObjType,ObjID}, EO_Value) ->
    set_field({ObjType, ObjID}, name, EO_Value).

'_get_version'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},version).

'_set_version'({ObjType,ObjID}, EO_Value) ->
    set_field({ObjType, ObjID}, version, EO_Value).

'_get_defined_in'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},defined_in).

'_get_absolute_name'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},absolute_name).

'_get_containing_repository'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},containing_repository).

describe(ObjRef) ->
    Def_kind = '_get_def_kind'(ObjRef),
    Object = get_object(ObjRef),
    describe(Object,Def_kind).

describe(Object,Def_kind) ->
    Value =
	case Def_kind of
	    dk_Module ->
		#moduledescription{name = Object#ir_ModuleDef.name,
				   id = Object#ir_ModuleDef.id,
				   defined_in = Object#ir_ModuleDef.defined_in,
				   version = Object#ir_ModuleDef.version};
	    dk_Constant ->
		#constantdescription{name = Object#ir_ConstantDef.name,
				     id = Object#ir_ConstantDef.id,
				     defined_in =
				     Object#ir_ConstantDef.defined_in,
				     version = Object#ir_ConstantDef.version,
				     type = Object#ir_ConstantDef.type,
				     value = Object#ir_ConstantDef.value};
	    dk_Typedef ->
		#typedescription{name = Object#ir_TypedefDef.name,
				 id = Object#ir_TypedefDef.id,
				 defined_in = Object#ir_TypedefDef.defined_in,
				 version = Object#ir_TypedefDef.version,
				 type = Object#ir_TypedefDef.type};
	    dk_Struct ->
		?make_typedescription(Object,ir_StructDef);
	    dk_Union ->
		?make_typedescription(Object,ir_UnionDef);
	    dk_Enum ->
		?make_typedescription(Object,ir_EnumDef);
	    dk_Alias ->
		?make_typedescription(Object,ir_AliasDef);
	    dk_Exception ->
		#exceptiondescription{name = Object#ir_ExceptionDef.name,
				      id = Object#ir_ExceptionDef.id,
				      defined_in =
				      Object#ir_ExceptionDef.defined_in,
				      version = Object#ir_ExceptionDef.version,
				      type = Object#ir_ExceptionDef.type};
	    dk_Attribute ->
		#attributedescription{name = Object#ir_AttributeDef.name,
				      id = Object#ir_AttributeDef.id,
				      defined_in =
				      Object#ir_AttributeDef.defined_in,
				      version = Object#ir_AttributeDef.version,
				      type = Object#ir_AttributeDef.type,
				      mode = Object#ir_AttributeDef.mode};
	    dk_Operation ->
		#operationdescription{name = Object#ir_OperationDef.name,
				      id = Object#ir_OperationDef.id,
				      defined_in =
				      Object#ir_OperationDef.defined_in,
				      version = Object#ir_OperationDef.version,
				      result = Object#ir_OperationDef.result,
				      mode = Object#ir_OperationDef.mode,
				      contexts =
				      Object#ir_OperationDef.contexts,
				      parameters =
				      Object#ir_OperationDef.params,
				      exceptions =
				      Object#ir_OperationDef.exceptions};
	    dk_Interface ->
		#interfacedescription{name = Object#ir_InterfaceDef.name,
				      id = Object#ir_InterfaceDef.id,
				      defined_in =
				      Object#ir_InterfaceDef.defined_in,
				      version = Object#ir_InterfaceDef.version,
				      base_interfaces =
				      Object#ir_InterfaceDef.base_interfaces};
	    _ ->
		undefined
	end,
    #contained_description{kind=Def_kind, value=Value}.

move({ObjType,ObjID},{NewContainerType,NewContainerID},New_name,New_version) ->
    Move_OK =
	('_get_containing_repository'({NewContainerType,NewContainerID}) ==
	 '_get_containing_repository'({ObjType,ObjID}))
        and
	case NewContainerType of
	    ir_Repository ->
		lists:member(ObjType,[ir_ConstantDef,ir_TypedefDef,
				      ir_ExceptionDef,ir_InterfaceDef,
				      ir_ModuleDef]);
	    ir_ModuleDef ->
		lists:member(ObjType,[ir_ConstantDef,ir_TypedefDef,
				      ir_ExceptionDef,ir_ModuleDef,
				      ir_InterfaceDef]);
	    ir_InterfaceDef ->
		lists:member(ObjType,[ir_ConstantDef,ir_TypedefDef,
				      ir_ExceptionDef,ir_AttributeDef,
				      ir_OperationDef]);
	    _ ->
		false
	end
	and
	(orber_ifr_container:lookup_name({NewContainerType,NewContainerID},
					 New_name, -1, % *** -1?
					 dk_All, false) == []),
    move(Move_OK,{ObjType,ObjID},{NewContainerType,NewContainerID},New_name,
	 New_version).

move(true, Contained_objref, New_container, New_name, New_version) ->
    F = fun() -> Defined_in = '_get_defined_in'(Contained_objref),
		 [Old_container_obj] = mnesia:read(Defined_in),
		 New_old_container_obj =
		     construct(Old_container_obj,contents,
			       lists:filter(fun(X) -> X /= Contained_objref
					    end, select(Old_container_obj,
							contents))),
		 New_container_obj = mnesia:read(New_container),
		 Contents = orber_ifr_container:contents(New_container, dk_All,
							 true),
		 New_new_container_obj =
		     construct(construct(construct(New_container_obj, contents,
					   [Contained_objref | Contents]),
					 name,New_name),version,New_version),
		 mnesia:write(New_old_container_obj),
		 mnesia:write(New_new_container_obj)
	end,
    write_result(ifr_transaction_read_write(F));

move(false, _Contained_objref, _New_container, _New_name, _New_version) ->
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).
