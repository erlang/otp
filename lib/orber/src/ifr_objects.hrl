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
%%%----------------------------------------------------------------------
%%% File    : ir_objects.hrl
%%% Purpose : Record definitions for the IR DB 
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% *********************************************************************
%%% *									*
%%% *                        PLEASE NOTE				*
%%% *									*
%%% * If a record is removed or added in this file, the corresponding   *
%%% * database initialization code _MUST_ be updated accordingly.	*
%%% *									*
%%% * The initialization code is defined in a macro in this file.	*
%%% *									*
%%% * Also remember to update select/2 in orber_ifr.erl when adding	*
%%% * or deleting a record in this file.				*
%%% *									*
%%% *********************************************************************
%%%----------------------------------------------------------------------

%% Interface objects

%% There are eight interface objects in an interface repository:
%% Repository, ModuleDef, InterfaceDef, AttributeDef, OperationDef,
%% TypedefDef, ConstantDef and ExceptionDef (CORBA V2.0, page 6-5/6).

% The other objects defined here are used to build the above objects
% (CORBA V2.0, page 6-7).

% Object references are stored as mnesia object IDs, i.e. a tuple with
% the table name and the ir_Internal_ID.

% Inheritance strategy. We incorporate the inherited object into the
% inheriting object. The record element 'inherited_objects' is a list
% of objects that "this" object inherits from (i.e. full object
% records and not object references).

% The record element 'ir_Internal_ID' is a tag that uniquely
% identifies a record. See the function orber_ifr:unique().

						% IRObject, page 6-9
-record(ir_IRObject,	 {ir_Internal_ID,def_kind}).

						% Contained, page 6-9
-record(ir_Contained,    {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  id,
			  name,
			  version,
			  defined_in,
			  absolute_name,
			  containing_repository}).

						% Container, page 6-10
-record(ir_Container,    {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  contents}).

						% IDLType, page 6-15
-record(ir_IDLType,      {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  type}).

						% Repository, page 6-16
-record(ir_Repository,   {ir_Internal_ID,	%[Container]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  primitivedefs}).

						% ModuleDef, page 6-17
-record(ir_ModuleDef,    {ir_Internal_ID,	%[Container,Contained]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository %from Contained
			 }).

						% ConstantDef, page 6-17
-record(ir_ConstantDef,  {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  type_def,
			  value}).

						% TypedefDef, page 6-18
-record(ir_TypedefDef,   {ir_Internal_ID,	%[Contained,IDLType]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type			%from IDLType
			 }).

						% StructDef, page 6-19
-record(ir_StructDef,    {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  members}).

						% UnionDef, page 6-19
-record(ir_UnionDef,     {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  discriminator_type,
			  discriminator_type_def,
			  members}).

						% EnumDef, page 6-20
-record(ir_EnumDef,      {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  members}).

						% AliasDef, page 6-21
-record(ir_AliasDef,     {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  original_type_def}).

						% PrimitiveDef, page 6-21
-record(ir_PrimitiveDef, {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  kind}).

						% StringDef, page 6-22
-record(ir_StringDef,    {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  bound}).

-record(ir_WstringDef,   {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  bound}).

						% SequenceDef, page 6-22
-record(ir_SequenceDef,  {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  bound,
			  element_type,
			  element_type_def}).

						% ArrayDef, page 6-23
-record(ir_ArrayDef,     {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  length,
			  element_type,
			  element_type_def}).

						% ExceptionDef, page 6-23
-record(ir_ExceptionDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  members}).

						% AttributeDef, page 6-24
-record(ir_AttributeDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  type_def,
			  mode}).

						% OperationDef, page 6-25
-record(ir_OperationDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  result,
			  result_def,
			  params,
			  mode,
			  contexts,
			  exceptions}).

						% InterfaceDef, page 6-27
-record(ir_InterfaceDef, {ir_Internal_ID,	%[Container,Contained,IDLType]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  base_interfaces}).

						% TypeCode, page 6-33

-record(ir_FixedDef,     {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  digits,
			  scale}).


% TypeCodes cannot be defined as records, since each type code has a
% quite unique structure depending on the type. The old TypeCode
% record definition is left here as a comment in case we want to
% change back to the old style.

%% ir_TypeCode does not have a field ir_Internal_ID. TypeCodes are
%% never explicitly written to the database as separate DB-records.
%% TypeCodes are stored as full records whenever they are used in an
%% IFR-object.
%%-record(ir_TypeCode,     {kind,
%%			  parameter_list}).

						% ORB, page 6-39
-record(ir_ORB,          {ir_Internal_ID,   % *** Do we need any attributes
			  dummy}).	    % for this table? ORB is a pseudo-
					    % object so perhaps the table is
					    % unnecessary?

-record(orber_light_ifr, {id, %% IFR-id
			  module,
			  type,
			  base_id}).

-define(IFR_ModuleDef,    0).
-define(IFR_ConstantDef,  1).
-define(IFR_StructDef,    2).
-define(IFR_UnionDef,     3).
-define(IFR_EnumDef,      4).
-define(IFR_AliasDef,     5).
-define(IFR_InterfaceDef, 6).
-define(IFR_ExceptionDef, 7).


%%%----------------------------------------------------------------------
%%% 'ifr_object_list' is used by other modules. Do NOT remove or rename
%%% this list!
%%% An addition or deletion of a record above must be duplicated here in
%%% this list and in the macro 'ifr_record_tuple_list' below.
-define(ifr_object_list, [ir_ModuleDef,
			  ir_Contained,
			  ir_AttributeDef,
			  ir_Repository,
			  ir_OperationDef,
			  ir_InterfaceDef,
			  ir_TypedefDef,
			  ir_Container,
			  ir_EnumDef,
			  ir_UnionDef,
			  ir_StringDef,
			  ir_WstringDef,
			  ir_ORB,
			  ir_IDLType,
			  ir_ExceptionDef,
			  ir_IRObject,
			  ir_PrimitiveDef,
			  ir_ArrayDef,
			  ir_AliasDef,
			  ir_ConstantDef,
			  ir_StructDef,
			  ir_SequenceDef,
			  ir_FixedDef]).

-define(ifr_light_object_list, [orber_light_ifr]).

-define(cr_fun_tuple(Table, Options),
	{Table,
	 fun() ->
		 case mnesia:create_table(Table,[{attributes,
						  record_info(fields,
							      Table)}]++Options)of
		     {atomic,ok} ->
			 ok;
		     R ->
			 R
		 end
	 end}
       ).

-define(cr_fun_tuple_local(Table, IFR_storage_type),
	{Table,
	 fun() ->
		 case mnesia:add_table_copy(Table,node(), IFR_storage_type)of
		     {atomic,ok} ->
			 ok;
		     R ->
			 R
		 end
	 end}
       ).

-define(ifr_record_tuple_list(Options),
        [?cr_fun_tuple(ir_IRObject, Options),
         ?cr_fun_tuple(ir_Contained, [{index, [#ir_Contained.id]}|Options]),
         ?cr_fun_tuple(ir_Container, Options),
         ?cr_fun_tuple(ir_IDLType, Options),
         ?cr_fun_tuple(ir_Repository, Options),
         ?cr_fun_tuple(ir_ModuleDef, [{index, [#ir_ModuleDef.id]}|Options]),
         ?cr_fun_tuple(ir_ConstantDef, [{index, [#ir_ConstantDef.id]}|Options]),
         ?cr_fun_tuple(ir_TypedefDef, [{index, [#ir_TypedefDef.id]}|Options]),
         ?cr_fun_tuple(ir_StructDef, [{index, [#ir_StructDef.id]}|Options]),
         ?cr_fun_tuple(ir_UnionDef, [{index, [#ir_UnionDef.id]}|Options]),
         ?cr_fun_tuple(ir_EnumDef, [{index, [#ir_EnumDef.id]}|Options]),
         ?cr_fun_tuple(ir_AliasDef, [{index, [#ir_AliasDef.id]}|Options]),
         ?cr_fun_tuple(ir_PrimitiveDef, Options),
         ?cr_fun_tuple(ir_StringDef, Options),
         ?cr_fun_tuple(ir_WstringDef, Options),
         ?cr_fun_tuple(ir_SequenceDef, Options),
         ?cr_fun_tuple(ir_ArrayDef, Options),
         ?cr_fun_tuple(ir_ExceptionDef, [{index, [#ir_ExceptionDef.id]}|Options]),
         ?cr_fun_tuple(ir_AttributeDef, [{index, [#ir_AttributeDef.id]}|Options]),
         ?cr_fun_tuple(ir_OperationDef, [{index, [#ir_OperationDef.id]}|Options]),
         ?cr_fun_tuple(ir_InterfaceDef, [{index, [#ir_InterfaceDef.id]}| Options]),
%        ?cr_fun_tuple(ir_TypeCode, Options),
         ?cr_fun_tuple(ir_ORB, Options),
	 ?cr_fun_tuple(ir_FixedDef, Options)]).

-define(ifr_light_record_tuple_list(Options),
        [?cr_fun_tuple(orber_light_ifr, Options)]).


-define(ifr_record_tuple_list_local(IFR_storage_type),
        [?cr_fun_tuple_local(ir_IRObject, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Contained, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Container, IFR_storage_type),
         ?cr_fun_tuple_local(ir_IDLType, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Repository, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ModuleDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ConstantDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_TypedefDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_StructDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_UnionDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_EnumDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_AliasDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_PrimitiveDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_StringDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_WstringDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_SequenceDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ArrayDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ExceptionDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_AttributeDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_OperationDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_InterfaceDef, IFR_storage_type),
%        ?cr_fun_tuple_local(ir_TypeCode, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ORB, IFR_storage_type),
	 ?cr_fun_tuple_local(ir_FixedDef, IFR_storage_type)]).

-define(ifr_light_record_tuple_list_local(IFR_storage_type),
        [?cr_fun_tuple_local(orber_light_ifr, IFR_storage_type)]).
