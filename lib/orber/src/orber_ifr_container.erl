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
%% File    : orber_ifr_container.erl
%% Purpose : Code for Container
%%----------------------------------------------------------------------

-module(orber_ifr_container).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 lookup/2,
	 contents/3,
	 lookup_name/5,
	 describe_contents/4,
	 make_absolute_name/2,			%not in CORBA 2.0
	 make_containing_repository/1,		%not in CORBA 2.0
	 add_to_container/5,			%not in CORBA 2.0
	 create_module/4,
	 create_constant/6,
	 create_struct/5,
	 create_union/6,
	 create_enum/5,
	 create_alias/5,
	 create_interface/5,
	 create_exception/5
	]).

-import(orber_ifr_utils,[get_field/2,select/2,construct/3,makeref/1,unique/0]).
-import(lists,[map/2,filter/2,flatten/1,sublist/2]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%%======================================================================
%%% Container (IRObject)

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'(ObjRef) ->
    orber_ifr_irobject:'_get_def_kind'(ObjRef).

%%% Note, that the destroy function is meant to be called within a
%%% transaction called in the destroy function of an object which
%%% inherits from Container. A Container should only be destroyed by
%%% destroying the object that inherits from a Container. An attempt
%%% to call this function in user code will result in unpredictable
%%% results.

%%% Don't type check the object reference. We need to be able to handle several
%%% types of objects that inherit from Container.

destroy(Container_objref) ->
    ObjList = cleanup_for_destroy(Container_objref),
    orber_ifr_irobject:destroy([Container_objref | ObjList]).

cleanup_for_destroy(Container_objref) ->
    Contents = get_field(Container_objref, contents),
    map(fun destroy_thing/1, Contents) ++ Contents.

%%% Destroy objects which inherit from Contained, i.e. objects that populate
%%% the contents list of a Container.

destroy_thing({ObjType,ObjID}) when ObjType == ir_ModuleDef ->
    orber_ifr_moduledef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_ConstantDef ->
    orber_ifr_constantdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_TypedefDef ->
    orber_ifr_typedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_StructDef ->
    orber_ifr_structdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_UnionDef ->
    orber_ifr_uniondef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_EnumDef ->
    orber_ifr_enumdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_AliasDef ->
    orber_ifr_aliasdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_ExceptionDef ->
    orber_ifr_exceptiondef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_AttributeDef ->
    orber_ifr_attributedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_OperationDef ->
    orber_ifr_operationdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_InterfaceDef ->
    orber_ifr_interfacedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({_ObjType,_ObjID}) ->
    %% Unknown object in Container contents.
    true.

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces
lookup(ObjRef, Search_name) ->
    Contents = contents(ObjRef, dk_All, false),

    %% We now have the contents (a list of object references).
    %% Let's find all objects with the correct name.

    case filter(fun({Type,ID}) ->
		   orber_ifr_contained:'_get_absolute_name'({Type,ID}) ==
		       Search_name
	   end,
	   Contents) of
	[Obj] ->
	    Obj;
	X ->
	    X
    end.


contents(ObjRef, Limit_type, Exclude_inherited) ->
    Contents = 
	flatten(get_field(ObjRef, contents) ++
		inherited_contents(ObjRef,Exclude_inherited)),
    AllContents = 
	Contents ++
	flatten(subcontents(Limit_type,Contents)),
    limit_contents(Limit_type,AllContents).       


subcontents(_,[]) -> [];
subcontents(Limit_type,Contents) ->
    map(fun(ObjRef) -> contents(ObjRef,Limit_type) end, Contents).
    
contents({ir_Repository,ObjID},Limit_type) ->
    orber_ifr_repository:contents({ir_Repository,ObjID},Limit_type,false);
contents({ir_ModuleDef,ObjID},Limit_type) ->
    orber_ifr_moduledef:contents({ir_ModuleDef,ObjID},Limit_type,false);
contents({ir_InterfaceDef,ObjID},Limit_type) ->
    orber_ifr_interfacedef:contents({ir_InterfaceDef,ObjID},Limit_type,false);
contents(_,_) -> [].

limit_contents(dk_All,Contents) -> Contents;
limit_contents(Limit_type,Contents) ->
    filter(fun(Obj_Ref) -> '_get_def_kind'(Obj_Ref) == Limit_type end,
	   Contents).


lookup_name(ObjRef, Search_name, Levels_to_search,
			Limit_type, Exclude_inherited) ->
    Contents = get_field(ObjRef, contents),
    AllContents = Contents ++ inherited_contents(ObjRef, Exclude_inherited),
    lookup_name(AllContents, Search_name, Levels_to_search, Limit_type).
    
inherited_contents({ir_InterfaceDef,ObjID}, false) ->
    map(fun(ObjRef) -> get_field(ObjRef,contents) end,
       orber_ifr_interfacedef:'_get_base_interfaces'({ir_InterfaceDef,ObjID}));
inherited_contents(_, false) -> [];
inherited_contents(_, true) -> [].

lookup_name(Contents, Search_name, Level, Limit_type) ->
    filter(fun(X) ->
		   (orber_ifr_contained:'_get_id'(X) == Search_name)
		   and
		   ('_get_def_kind'(X) == Limit_type)
	   end, Contents) ++
	sublookup_name(Contents, Search_name, Level - 1, Limit_type).

sublookup_name([],_,_,_) -> [];
sublookup_name(_,_,0,_) -> [];
sublookup_name(Contents, Search_name, Level, Limit_type) ->
    map(fun(X) ->
		Conts = subcontents(X),
		lookup_name(Conts, Search_name, Level - 1, Limit_type)
	end, Contents).

subcontents({ir_Repository,ObjID}) ->
    get_field({ir_Repository,ObjID}, contents);
subcontents({ir_ModuleDefObjType,ObjID}) ->
    get_field({ir_ModuleDef,ObjID}, contents);
subcontents({ir_InterfaceDef,ObjID}) ->
    get_field({ir_InterfaceDef,ObjID}, contents);
subcontents(_) -> [].

describe_contents(ObjRef, Limit_type, Exclude_inherited,
		  Max_returned_objs) ->
    Limited_contents = contents(ObjRef,Limit_type,Exclude_inherited),
    describe_contents(Limited_contents, Max_returned_objs, []).

describe_contents(_, 0, Acc) ->
    Acc;
describe_contents([], _Max_returned_objs, Acc) ->
    Acc;
describe_contents([H|T], Max_returned_objs, Acc) ->
    Desc = orber_ifr_contained:describe(H),
    describe_contents(T, Max_returned_objs-1, [Desc|Acc]).


%% This is a kludge. Se p. 6-11 in CORBA 2.0.
make_absolute_name({ObjType,ObjID}, Name) ->
    case ObjType of
	ir_Repository ->
	    "::" ++ Name;
	_ ->
	    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}) ++
		"::" ++ Name
    end.

%% This is a kludge. Se p. 6-15 in CORBA 2.0.
make_containing_repository({ObjType,ObjID}) ->
    case ObjType of
	ir_Repository ->
	    {ir_Repository,ObjID};
	_ ->
	    orber_ifr_contained:'_get_containing_repository'({ObjType, ObjID})
    end.

add_to_container(ContainerRef,Object, Id, Table, Index) ->
    F = fun() ->
		[Container_obj] = mnesia:wread(ContainerRef),
		case mnesia:index_read(Table, Id, Index) of
		    [] ->
			ObjectRef = makeref(Object),
			New_container_obj =
			    construct(Container_obj,contents,
				      [ObjectRef | select(Container_obj,contents)]),
			mnesia:write(New_container_obj),
			mnesia:write(Object);
		    _ ->
			mnesia:abort("duplicate")
		end
	end,
    case mnesia:transaction(F) of 
        {aborted, "duplicate"} ->
	    %% Must keep the misspelled word (must match IC generated code).
            exit({allready_registered, Id});
	{aborted, Reason} ->
	    orber:dbg("[~p] orber_ifr_container:add_to_container(~p). aborted:~n~p~n", 
		      [?LINE, Id, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	{atomic, _} ->
	    ok
    end.

add_to_light(#orber_light_ifr_ref{data = Data} = LRef, Id, Type, Name) ->
    BaseId = get_base_id(Data#lightdata.id, Id),
    NewScope = scoped_name(Data#lightdata.scope, Name, Type),
    F = fun() ->
		D = #orber_light_ifr{id = Id, 
				     module = list_to_atom(NewScope),
				     type = Type, base_id = BaseId},
		mnesia:write(D)
	end,
    case mnesia:transaction(F) of 
	{aborted, Reason} ->
	    orber:dbg("[~p] orber_ifr_container:add_to_light(~p). aborted:~n~p~n", 
		      [?LINE, Id, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	{atomic, _} ->
	    LRef#orber_light_ifr_ref{data = Data#lightdata{scope = NewScope,
							   id = BaseId}}
    end.

get_base_id("", Id) ->
    Id;
get_base_id(Id, _) ->
    Id.

scoped_name("", Name, _) ->
    Name;
scoped_name(Scope, _, ?IFR_ConstantDef) ->
    Scope;
scoped_name(Scope, Name, _) ->
    Scope ++ "_" ++ Name.

create_module(#orber_light_ifr_ref{} = LRef, Id, Name, _Version) ->
    add_to_light(LRef, Id, ?IFR_ModuleDef, Name);
create_module(ObjRef, Id, Name, Version) ->
    New_module = #ir_ModuleDef{ir_Internal_ID = unique(),
			       def_kind = dk_Module,
			       contents = [],
			       id = Id,
			       name = Name,
			       version = Version,
			       defined_in = ObjRef,
			       absolute_name =
			       make_absolute_name(ObjRef, Name),
			       containing_repository =
			       make_containing_repository(ObjRef)},
    add_to_container(ObjRef,New_module, Id, ir_ModuleDef, #ir_ModuleDef.id),
    makeref(New_module).

create_constant(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Type, _Value) ->
    add_to_light(LRef, Id, ?IFR_ConstantDef, Name);
create_constant(ObjRef, Id, Name, Version, Type, Value) ->
    IDL_typecode = get_field(Type,type),
    {Typecode, _} = Value,
    case IDL_typecode == Typecode of
	false ->
	    orber:dbg("[~p] ~p:create_constant(~p, ~p, ~p, ~p, ~p, ~p);~n"
		      "Wrong type.~n", 
		      [?LINE, ?MODULE, ObjRef, Id, Name, Version, Type, Value], 
		      ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	true ->
	    New_constant = #ir_ConstantDef{ir_Internal_ID = unique(),
					   def_kind = dk_Constant,
					   id = Id,
					   name = Name,
					   version = Version,
					   defined_in = ObjRef,
					   absolute_name =
					   make_absolute_name(ObjRef, Name),
					   containing_repository =
					   make_containing_repository(ObjRef),
					   type = get_field(Type,type),
					   type_def = Type,
					   value = Value},
	    add_to_container(ObjRef,New_constant, 
			     Id, ir_ConstantDef, #ir_ConstantDef.id),
	    makeref(New_constant)
    end.

create_struct(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Members) ->
    add_to_light(LRef, Id, ?IFR_StructDef, Name);
create_struct(ObjRef, Id, Name, Version, Members) ->
    New_struct = #ir_StructDef{ir_Internal_ID = unique(),
			       def_kind = dk_Struct,
			       id = Id,
			       name = Name,
			       version = Version,
			       defined_in = ObjRef,
			       absolute_name =
			       make_absolute_name(ObjRef, Name),
			       containing_repository =
			       make_containing_repository(ObjRef),
			       type = {tk_struct, Id, Name,
				       map(fun(#structmember{name=MemName,
							     type=Type}) ->
						   {MemName,Type} end,
						 Members)},
			       members = Members},
    add_to_container(ObjRef, New_struct, Id, ir_StructDef, #ir_StructDef.id),
    makeref(New_struct).

create_union(#orber_light_ifr_ref{} = LRef, Id, Name, _Version,
	     _Discriminator_type, _Members) ->
    add_to_light(LRef, Id, ?IFR_UnionDef, Name);
create_union(ObjRef, Id, Name, Version,
	     Discriminator_type, Members) ->
    Discriminator_type_code = get_field(Discriminator_type, type),
    New_union = #ir_UnionDef{ir_Internal_ID = unique(),
			     def_kind = dk_Union,
			     id = Id,
			     name = Name,
			     version = Version,
			     defined_in = ObjRef,
			     absolute_name =
			     make_absolute_name(ObjRef, Name),
			     containing_repository =
			     make_containing_repository(ObjRef),
			     type = {tk_union, Id, Name,
				     Discriminator_type_code, -1,
				     map(fun(#unionmember{name=MemName,
							  label=Label,
							  type=Type}) ->
						 {Label,MemName,Type} end,
					 Members)},
			     discriminator_type = Discriminator_type_code,
			     discriminator_type_def = Discriminator_type,
			     members = Members},
    add_to_container(ObjRef, New_union, Id, ir_UnionDef, #ir_UnionDef.id),
    makeref(New_union).

create_enum(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Members) ->
    add_to_light(LRef, Id, ?IFR_EnumDef, Name);
create_enum(ObjRef, Id, Name, Version, Members) ->
    New_enum = #ir_EnumDef{ir_Internal_ID = unique(),
			   def_kind = dk_Enum,
			   id = Id,
			   name = Name,
			   version = Version,
			   defined_in = ObjRef,
			   absolute_name =
			   make_absolute_name(ObjRef, Name),
			   containing_repository =
			   make_containing_repository(ObjRef),
			   type = {tk_enum, Id, Name, Members},
			   members = Members},
    add_to_container(ObjRef, New_enum, Id, ir_EnumDef, #ir_EnumDef.id),
    makeref(New_enum).

create_alias(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Original_type) ->
    add_to_light(LRef, Id, ?IFR_AliasDef, Name);
create_alias(ObjRef, Id, Name, Version, Original_type) ->
    New_alias = #ir_AliasDef{ir_Internal_ID = unique(),
			     def_kind = dk_Alias,
			     id = Id,
			     name = Name,
			     version = Version,
			     defined_in = ObjRef,
			     absolute_name =
			     make_absolute_name(ObjRef, Name),
			     containing_repository =
			     make_containing_repository(ObjRef),
			     type = {tk_alias, Id, Name,
				     get_field(Original_type,type)},
			     original_type_def = Original_type},
    add_to_container(ObjRef, New_alias, Id, ir_AliasDef, #ir_AliasDef.id),
    makeref(New_alias).

create_interface(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Base_interfaces) ->
    add_to_light(LRef, Id, ?IFR_InterfaceDef, Name);
create_interface(ObjRef, Id, Name, Version, Base_interfaces) ->
    New_interface = #ir_InterfaceDef{ir_Internal_ID = unique(),
				     def_kind = dk_Interface,
				     contents = [],
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = ObjRef,
				     absolute_name =
				     make_absolute_name(ObjRef,Name),
				     containing_repository =
				     make_containing_repository(ObjRef),
				     type = {tk_objref, Id, Name},
				     base_interfaces = Base_interfaces},
    add_to_container(ObjRef, New_interface, Id, ir_InterfaceDef, #ir_InterfaceDef.id),
    makeref(New_interface).

create_exception(#orber_light_ifr_ref{} = LRef, Id, Name, _Version, _Members) ->
    add_to_light(LRef, Id, ?IFR_ExceptionDef, Name);
create_exception(ObjRef, Id, Name, Version, Members) ->
    New_exception = #ir_ExceptionDef{ir_Internal_ID = unique(),
				     def_kind = dk_Exception,
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = ObjRef,
				     absolute_name =
				     make_absolute_name(ObjRef,Name),
				     containing_repository =
				     make_containing_repository(ObjRef),
				     type = {tk_except, Id, Name,
					    map(fun(#structmember{name=MemName,
								  type=Type})
						   ->
							{MemName,Type} end,
						Members)},
				     members = Members},
    add_to_container(ObjRef, New_exception, Id, ir_ExceptionDef, #ir_ExceptionDef.id),
    makeref(New_exception).
