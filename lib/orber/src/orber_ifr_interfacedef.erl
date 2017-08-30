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
%% File    : orber_ifr_interfacedef.erl
%% Purpose : Code for Interfacedef
%%----------------------------------------------------------------------

-module(orber_ifr_interfacedef).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 lookup/2,
	 contents/3,
	 lookup_name/5,
	 describe_contents/4,
	 create_module/4,
	 create_constant/6,
	 create_struct/5,
	 create_union/6,
	 create_enum/5,
	 create_alias/5,
	 create_interface/5,
	 create_exception/5,
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
	 '_get_base_interfaces'/1,
	 '_set_base_interfaces'/2,
	 is_a/2,
	 describe_interface/1,
	 create_attribute/6,
	 create_operation/9
	]).

-import(orber_ifr_utils,[get_object/1,
		   get_field/2,
		   set_field/3,
		   select/2,
		   makeref/1,
		   unique/0
		  ]).
-import(orber_ifr_container,[make_absolute_name/2,
			    make_containing_repository/1
			    ]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").


%%%======================================================================
%%% InterfaceDef (Container(IRObject), Contained(IRObject), IDLType(IRObject))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType,ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}) ++
	orber_ifr_container:cleanup_for_destroy({ObjType,ObjID}) ++
	orber_ifr_idltype:cleanup_for_destroy({ObjType,ObjID}).


%% BUG ! You can't remove inherited !!!!!
%%cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
%%    lists:map(fun(X) -> cleanup_for_destroy(X) end,
%%	      '_get_base_interfaces'({ObjType,ObjID})) ++  <<<<<<<<<< Here 
%%	orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}) ++
%%	orber_ifr_container:cleanup_for_destroy({ObjType,ObjID}) ++
%%	orber_ifr_idltype:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Container

lookup({ObjType, ObjID}, Search_name) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:lookup({ObjType,ObjID}, Search_name).

contents({ObjType, ObjID}, Limit_type, Exclude_inherited)
	?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:contents({ObjType,ObjID},Limit_type,Exclude_inherited).

lookup_name({ObjType, ObjID}, Search_name, Levels_to_search, Limit_type,
	    Exclude_inherited) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:lookup_name({ObjType,ObjID}, Search_name,
				    Levels_to_search, Limit_type,
				    Exclude_inherited).

describe_contents({ObjType, ObjID}, Limit_type, Exclude_inherited,
		  Max_returned_objs) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:describe_contents({ObjType,ObjID}, Limit_type,
					  Exclude_inherited,
					  Max_returned_objs).

create_module({ObjType, ObjID}, Id, Name, Version)
	     ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_module({ObjType, ObjID}, Id, Name, Version).

create_constant({ObjType, ObjID}, Id, Name, Version, Type, Value)
	       ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_constant({ObjType, ObjID}, Id, Name, Version,
					Type, Value).

create_struct({ObjType, ObjID}, Id, Name, Version, Members)
	     ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_struct({ObjType,ObjID},Id,Name,Version,Members).

create_union({ObjType, ObjID}, Id, Name, Version, Discriminator_type, Members)
	    ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_union({ObjType, ObjID}, Id, Name, Version,
				     Discriminator_type, Members).

create_enum({ObjType, ObjID}, Id, Name, Version, Members)
	   ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_enum({ObjType, ObjID},Id,Name,Version,Members).

create_alias({ObjType, ObjID}, Id, Name, Version, Original_type)
	    ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_alias({ObjType, ObjID}, Id, Name, Version,
				     Original_type).

create_interface({ObjType, ObjID}, Id, Name, Version, Base_interfaces)
		?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_interface({ObjType, ObjID}, Id, Name, Version,
					 Base_interfaces).

create_exception({ObjType, ObjID}, Id, Name, Version, Members)
		?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_container:create_exception({ObjType, ObjID}, Id, Name, Version,
					 Members).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID})
			    ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version)
		   ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces 

'_get_base_interfaces'({ObjType,ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->
    get_field({ObjType,ObjID},base_interfaces).

'_set_base_interfaces'({ObjType, ObjID}, EO_Value)
		      ?tcheck(ir_InterfaceDef, ObjType) ->
    set_field({ObjType,ObjID}, base_interfaces, EO_Value).



is_a({ObjType, ObjID}, Interface_id) ?tcheck(ir_InterfaceDef, ObjType) ->
    Base_interfaces = '_get_base_interfaces'({ObjType, ObjID}),
    lists:any(fun(X) ->
		      case catch orber_ifr_contained:'_get_id'(X) of
			  Interface_id ->
			      'true';
			  _ ->
			      'false'
		      end
	      end,
	      Base_interfaces).

describe_interface({ObjType, ObjID}) ?tcheck(ir_InterfaceDef, ObjType) ->

%%% *** Should we exclude the inherited operations here? Probably not,
%%% but I'm not sure at all.

%%% OpContents = orber_ifr_container:contents({ObjType,ObjID}, dk_Operation,
%%%					      true),

    %% If it is OK to set Exclude_inherited to true (as in the above
    %% code which is commented out), the following code is faster than
    %% calling the contents/3 above. Otherwise we have to rethink
    %% this.

    Object = get_object({ObjType, ObjID}),

%%%    Contents = select(Object, contents),
    %% This is faster:
    Contents = Object#ir_InterfaceDef.contents,

    ContentsObjects = lists:map(fun(ObjRef) ->
					get_object(ObjRef)
				end,
				Contents),
    OpContents = lists:filter(fun(Obj) ->
				      select(Obj,def_kind) == dk_Operation
			      end,
			      ContentsObjects),
    
    Ops = lists:map(fun(Obj) ->
			    orber_ifr_contained:describe(Obj,dk_Operation)
		    end, OpContents),

%%% *** See the comment above on the Exclude_inherited parameter, and
%%% the circumstances when not to use contents/3.

%%% AttrContents = orber_ifr_container:contents({ObjType,ObjID}, dk_Attribute,
%%%						true),

    AttrContents = lists:filter(fun(Obj) ->
				      select(Obj,def_kind) == dk_Attribute
			      end,
			      ContentsObjects),
    Attrs = lists:map(fun(Obj) ->
			      orber_ifr_contained:describe(Obj,dk_Attribute)
		      end, AttrContents),

    #fullinterfacedescription{name = Object#ir_InterfaceDef.name,
			      id = Object#ir_InterfaceDef.id,
			      defined_in = Object#ir_InterfaceDef.defined_in,
			      version = Object#ir_InterfaceDef.version,
			      operations = Ops,
			      attributes = Attrs,
			      base_interfaces =
			      Object#ir_InterfaceDef.base_interfaces,
			      type = Object#ir_InterfaceDef.type
			     }.

create_attribute(#orber_light_ifr_ref{} = LRef, _Id, _Name, _Version, _Type, _Mode) ->
    LRef;
create_attribute({ObjType, ObjID}, Id, Name, Version, Type, Mode)
		?tcheck(ir_InterfaceDef, ObjType) ->
    New_attribute = #ir_AttributeDef{ir_Internal_ID = unique(),
				     def_kind = dk_Attribute,
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = {ObjType, ObjID},
				     absolute_name =
				     make_absolute_name({ObjType,ObjID}, Name),
				     containing_repository =
				     make_containing_repository({ObjType,ObjID}),
				     type = get_field(Type,type),
				     type_def = Type,
				     mode = Mode},
    orber_ifr_container:add_to_container({ObjType,ObjID}, New_attribute, 
					 Id, ir_AttributeDef, 
					 #ir_AttributeDef.id),
    makeref(New_attribute).

create_operation(#orber_light_ifr_ref{} = LRef, _Id, _Name, _Version, _Result, 
		 _Mode, _Params, _Exceptions, _Contexts) ->
    LRef;
create_operation({ObjType, ObjID}, Id, Name, Version, Result, Mode, Params,
		 Exceptions, Contexts) ?tcheck(ir_InterfaceDef, ObjType) ->
    New_operation = #ir_OperationDef{ir_Internal_ID = unique(),
				     def_kind = dk_Operation,
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = {ObjType, ObjID},
				     absolute_name =
				     make_absolute_name({ObjType,ObjID}, Name),
				     containing_repository =
				     make_containing_repository({ObjType,ObjID}),
				     result = get_field(Result,type),
				     result_def = Result,
				     mode = Mode,
				     params = Params,
				     exceptions = Exceptions,
				     contexts = Contexts},
    orber_ifr_container:add_to_container({ObjType,ObjID}, New_operation,
					 Id, ir_OperationDef, 
					 #ir_OperationDef.id),
    makeref(New_operation).
