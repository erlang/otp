%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : corba_ir_impl.erl
%% Purpose : Interface Repository for CORBA
%%----------------------------------------------------------------------

%%% NOTES:
%%%
%%%  For details about known deficiencies in this CORBA IFR
%%%  implementation, see the file ../doc/src/notes.txt.
%%%

-module(orber_ifr).

-export([
%%% Public interfaces:
	 init/2,
	 find_repository/0,
	 'IRObject__get_def_kind'/1,
	 %%'IRObject_destroy'/1,
	 'Contained__get_def_kind'/1,
	 %%'Contained_destroy'/1,
	 'Contained__get_id'/1,
	 'Contained__set_id'/2,
	 'Contained__get_name'/1,
	 'Contained__set_name'/2,
	 'Contained__get_version'/1,
	 'Contained__set_version'/2,
	 'Contained__get_defined_in'/1,
	 'Contained__get_absolute_name'/1,
	 'Contained__get_containing_repository'/1,
	 'Contained_describe'/1,
	 'Contained_move'/4,
	 'Container__get_def_kind'/1,
	 'Container_destroy'/1,
	 'Container_lookup'/2,
	 'Container_contents'/3,
	 'Container_lookup_name'/5,
	 'Container_describe_contents'/4,
	 'Container_create_module'/4,
	 'Container_create_constant'/6,
	 'Container_create_struct'/5,
	 'Container_create_union'/6,
	 'Container_create_enum'/5,
	 'Container_create_alias'/5,
	 'Container_create_interface'/5,
	 'Container_create_exception'/5,
	 'IDLType__get_def_kind'/1,
	 'IDLType_destroy'/1,
	 'IDLType__get_type'/1,
	 'Repository__get_def_kind'/1,
	 'Repository_destroy'/1,
	 'Repository_lookup'/2,
	 'Repository_contents'/3,
	 'Repository_lookup_name'/5,
	 'Repository_describe_contents'/4,
	 'Repository_create_module'/4,
	 'Repository_create_constant'/6,
	 'Repository_create_struct'/5,
	 'Repository_create_union'/6,
	 'Repository_create_enum'/5,
	 'Repository_create_alias'/5,
	 'Repository_create_interface'/5,
	 'Repository_create_exception'/5,
	 'Repository_lookup_id'/2,
	 'Repository_get_primitive'/2,
	 'Repository_create_string'/2,
	 'Repository_create_wstring'/2,
	 'Repository_create_sequence'/3,
	 'Repository_create_array'/3,
	 'Repository_create_idltype'/2,		%not in CORBA 2.0
	 'ModuleDef__get_def_kind'/1,
	 'ModuleDef_destroy'/1,
	 'ModuleDef_lookup'/2,
	 'ModuleDef_contents'/3,
	 'ModuleDef_lookup_name'/5,
	 'ModuleDef_describe_contents'/4,
	 'ModuleDef_create_module'/4,
	 'ModuleDef_create_constant'/6,
	 'ModuleDef_create_struct'/5,
	 'ModuleDef_create_union'/6,
	 'ModuleDef_create_enum'/5,
	 'ModuleDef_create_alias'/5,
	 'ModuleDef_create_interface'/5,
	 'ModuleDef_create_exception'/5,
	 'ModuleDef__get_id'/1,
	 'ModuleDef__set_id'/2,
	 'ModuleDef__get_name'/1,
	 'ModuleDef__set_name'/2,
	 'ModuleDef__get_version'/1,
	 'ModuleDef__set_version'/2,
	 'ModuleDef__get_defined_in'/1,
	 'ModuleDef__get_absolute_name'/1,
	 'ModuleDef__get_containing_repository'/1,
	 'ModuleDef_describe'/1,
	 'ModuleDef_move'/4,
	 'ConstantDef__get_def_kind'/1,
	 'ConstantDef_destroy'/1,
	 'ConstantDef__get_id'/1,
	 'ConstantDef__set_id'/2,
	 'ConstantDef__get_name'/1,
	 'ConstantDef__set_name'/2,
	 'ConstantDef__get_version'/1,
	 'ConstantDef__set_version'/2,
	 'ConstantDef__get_defined_in'/1,
	 'ConstantDef__get_absolute_name'/1,
	 'ConstantDef__get_containing_repository'/1,
	 'ConstantDef_describe'/1,
	 'ConstantDef_move'/4,
	 'ConstantDef__get_type'/1,
	 'ConstantDef__get_type_def'/1,
	 'ConstantDef__set_type_def'/2,
	 'ConstantDef__get_value'/1,
	 'ConstantDef__set_value'/2,
	 'TypedefDef__get_def_kind'/1,
	 'TypedefDef_destroy'/1,
	 'TypedefDef__get_id'/1,
	 'TypedefDef__set_id'/2,
	 'TypedefDef__get_name'/1,
	 'TypedefDef__set_name'/2,
	 'TypedefDef__get_version'/1,
	 'TypedefDef__set_version'/2,
	 'TypedefDef__get_defined_in'/1,
	 'TypedefDef__get_absolute_name'/1,
	 'TypedefDef__get_containing_repository'/1,
	 'TypedefDef_describe'/1,
	 'TypedefDef_move'/4,
	 'TypedefDef__get_type'/1,
	 'StructDef__get_def_kind'/1,
	 'StructDef_destroy'/1,
	 'StructDef__get_id'/1,
	 'StructDef__set_id'/2,
	 'StructDef__get_name'/1,
	 'StructDef__set_name'/2,
	 'StructDef__get_version'/1,
	 'StructDef__set_version'/2,
	 'StructDef__get_defined_in'/1,
	 'StructDef__get_absolute_name'/1,
	 'StructDef__get_containing_repository'/1,
	 'StructDef_describe'/1,
	 'StructDef_move'/4,
	 'StructDef__get_type'/1,
	 'StructDef__get_members'/1,
	 'StructDef__set_members'/2,
	 'UnionDef__get_def_kind'/1,
	 'UnionDef_destroy'/1,
	 'UnionDef__get_id'/1,
	 'UnionDef__set_id'/2,
	 'UnionDef__get_name'/1,
	 'UnionDef__set_name'/2,
	 'UnionDef__get_version'/1,
	 'UnionDef__set_version'/2,
	 'UnionDef__get_defined_in'/1,
	 'UnionDef__get_absolute_name'/1,
	 'UnionDef__get_containing_repository'/1,
	 'UnionDef_describe'/1,
	 'UnionDef_move'/4,
	 'UnionDef__get_type'/1,
	 'UnionDef__get_discriminator_type'/1,
	 'UnionDef__get_discriminator_type_def'/1,
	 'UnionDef__set_discriminator_type_def'/2,
	 'UnionDef__get_members'/1,
	 'UnionDef__set_members'/2,
	 'EnumDef__get_def_kind'/1,
	 'EnumDef_destroy'/1,
	 'EnumDef__get_id'/1,
	 'EnumDef__set_id'/2,
	 'EnumDef__get_name'/1,
	 'EnumDef__set_name'/2,
	 'EnumDef__get_version'/1,
	 'EnumDef__set_version'/2,
	 'EnumDef__get_defined_in'/1,
	 'EnumDef__get_absolute_name'/1,
	 'EnumDef__get_containing_repository'/1,
	 'EnumDef_describe'/1,
	 'EnumDef_move'/4,
	 'EnumDef__get_type'/1,
	 'EnumDef__get_members'/1,
	 'EnumDef__set_members'/2,
	 'AliasDef__get_def_kind'/1,
	 'AliasDef_destroy'/1,
	 'AliasDef__get_id'/1,
	 'AliasDef__set_id'/2,
	 'AliasDef__get_name'/1,
	 'AliasDef__set_name'/2,
	 'AliasDef__get_version'/1,
	 'AliasDef__set_version'/2,
	 'AliasDef__get_defined_in'/1,
	 'AliasDef__get_absolute_name'/1,
	 'AliasDef__get_containing_repository'/1,
	 'AliasDef_describe'/1,
	 'AliasDef_move'/4,
	 'AliasDef__get_type'/1,
	 'AliasDef__get_original_type_def'/1,
	 'AliasDef__set_original_type_def'/2,
	 'PrimitiveDef__get_def_kind'/1,
	 'PrimitiveDef_destroy'/1,
	 'PrimitiveDef__get_type'/1,
	 'PrimitiveDef__get_kind'/1,
	 'StringDef__get_def_kind'/1,
	 'StringDef_destroy'/1,
	 'StringDef__get_type'/1,
	 'StringDef__get_bound'/1,
	 'StringDef__set_bound'/2,
	 'WstringDef__get_def_kind'/1,
	 'WstringDef_destroy'/1,
	 'WstringDef__get_type'/1,
	 'WstringDef__get_bound'/1,
	 'WstringDef__set_bound'/2,
	 'FixedDef__get_def_kind'/1,
	 'FixedDef_destroy'/1,
	 'FixedDef__get_type'/1,
	 'FixedDef__get_digits'/1,
	 'FixedDef__set_digits'/2,
	 'FixedDef__get_scale'/1,
	 'FixedDef__set_scale'/2,
	 'SequenceDef__get_def_kind'/1,
	 'SequenceDef_destroy'/1,
	 'SequenceDef__get_type'/1,
	 'SequenceDef__get_bound'/1,
	 'SequenceDef__set_bound'/2,
	 'SequenceDef__get_element_type'/1,
	 'SequenceDef__get_element_type_def'/1,
	 'SequenceDef__set_element_type_def'/2,
	 'ArrayDef__get_def_kind'/1,
	 'ArrayDef_destroy'/1,
	 'ArrayDef__get_type'/1,
	 'ArrayDef__get_length'/1,
	 'ArrayDef__set_length'/2,
	 'ArrayDef__get_element_type'/1,
	 'ArrayDef__get_element_type_def'/1,
	 'ArrayDef__set_element_type_def'/2,
	 'ExceptionDef__get_def_kind'/1,
	 'ExceptionDef_destroy'/1,
	 'ExceptionDef__get_id'/1,
	 'ExceptionDef__set_id'/2,
	 'ExceptionDef__get_name'/1,
	 'ExceptionDef__set_name'/2,
	 'ExceptionDef__get_version'/1,
	 'ExceptionDef__set_version'/2,
	 'ExceptionDef__get_defined_in'/1,
	 'ExceptionDef__get_absolute_name'/1,
	 'ExceptionDef__get_containing_repository'/1,
	 'ExceptionDef_describe'/1,
	 'ExceptionDef_move'/4,
	 'ExceptionDef__get_type'/1,
	 'ExceptionDef__get_members'/1,
	 'ExceptionDef__set_members'/2,
	 'AttributeDef__get_def_kind'/1,
	 'AttributeDef_destroy'/1,
	 'AttributeDef__get_id'/1,
	 'AttributeDef__set_id'/2,
	 'AttributeDef__get_name'/1,
	 'AttributeDef__set_name'/2,
	 'AttributeDef__get_version'/1,
	 'AttributeDef__set_version'/2,
	 'AttributeDef__get_defined_in'/1,
	 'AttributeDef__get_absolute_name'/1,
	 'AttributeDef__get_containing_repository'/1,
	 'AttributeDef_describe'/1,
	 'AttributeDef_move'/4,
	 'AttributeDef__get_type'/1,
	 'AttributeDef__get_type_def'/1,
	 'AttributeDef__set_type_def'/2,
	 'AttributeDef__get_mode'/1,
	 'AttributeDef__set_mode'/2,
	 'OperationDef__get_def_kind'/1,
	 'OperationDef_destroy'/1,
	 'OperationDef__get_id'/1,
	 'OperationDef__set_id'/2,
	 'OperationDef__get_name'/1,
	 'OperationDef__set_name'/2,
	 'OperationDef__get_version'/1,
	 'OperationDef__set_version'/2,
	 'OperationDef__get_defined_in'/1,
	 'OperationDef__get_absolute_name'/1,
	 'OperationDef__get_containing_repository'/1,
	 'OperationDef_describe'/1,
	 'OperationDef_move'/4,
	 'OperationDef__get_result'/1,
	 'OperationDef__get_result_def'/1,
	 'OperationDef__set_result_def'/2,
	 'OperationDef__get_params'/1,
	 'OperationDef__set_params'/2,
	 'OperationDef__get_mode'/1,
	 'OperationDef__set_mode'/2,
	 'OperationDef__get_contexts'/1,
	 'OperationDef__set_contexts'/2,
	 'OperationDef__get_exceptions'/1,
	 'OperationDef__set_exceptions'/2,
	 'InterfaceDef__get_def_kind'/1,
	 'InterfaceDef_destroy'/1,
	 'InterfaceDef_lookup'/2,
	 'InterfaceDef_contents'/3,
	 'InterfaceDef_lookup_name'/5,
	 'InterfaceDef_describe_contents'/4,
	 'InterfaceDef_create_module'/4,
	 'InterfaceDef_create_constant'/6,
	 'InterfaceDef_create_struct'/5,
	 'InterfaceDef_create_union'/6,
	 'InterfaceDef_create_enum'/5,
	 'InterfaceDef_create_alias'/5,
	 'InterfaceDef_create_interface'/5,
	 'InterfaceDef_create_exception'/5,
	 'InterfaceDef__get_id'/1,
	 'InterfaceDef__set_id'/2,
	 'InterfaceDef__get_name'/1,
	 'InterfaceDef__set_name'/2,
	 'InterfaceDef__get_version'/1,
	 'InterfaceDef__set_version'/2,
	 'InterfaceDef__get_defined_in'/1,
	 'InterfaceDef__get_absolute_name'/1,
	 'InterfaceDef__get_containing_repository'/1,
	 'InterfaceDef_describe'/1,
	 'InterfaceDef_move'/4,
	 'InterfaceDef__get_type'/1,
	 'InterfaceDef__get_base_interfaces'/1,
	 'InterfaceDef__set_base_interfaces'/2,
	 'InterfaceDef_is_a'/2,
	 'InterfaceDef_describe_interface'/1,
	 'InterfaceDef_create_attribute'/6,
	 'InterfaceDef_create_operation'/9,
	 %%'TypeCode_equal'/2,
	 %%'TypeCode_kind'/1,
	 %%'TypeCode_id'/1,
	 %%'TypeCode_name'/1,
	 %%'TypeCode_member_count'/1,
	 %%'TypeCode_member_name'/2,
	 %%'TypeCode_member_type'/2,
	 %%'TypeCode_member_label'/2,
	 %%'TypeCode_discriminator_type'/1,
	 %%'TypeCode_default_index'/1,
	 %%'TypeCode_length'/1,
	 %%'TypeCode_content_type'/1,
	 %%'TypeCode_param_count'/1,
	 %%'TypeCode_parameter'/2,
	 'ORB_create_struct_tc'/3,
	 'ORB_create_union_tc'/4,
	 'ORB_create_enum_tc'/3,
	 'ORB_create_alias_tc'/3,
	 'ORB_create_exception_tc'/3,
	 'ORB_create_interface_tc'/2,
	 'ORB_create_string_tc'/1,
	 'ORB_create_wstring_tc'/1,
	 'ORB_create_sequence_tc'/2,
	 'ORB_create_recursive_sequence_tc'/2,
	 'ORB_create_array_tc'/2,
%%% "Methods" of the IFR "objects"
	 get_def_kind/1,
	 destroy/1,
	 get_id/1,
	 set_id/2,
	 get_name/1,
	 set_name/2,
	 get_version/1,
	 set_version/2,
	 get_defined_in/1,
	 get_absolute_name/1,
	 get_containing_repository/1,
	 describe/1,
	 move/4,
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
	 get_type/1,
	 lookup_id/2,
	 get_primitive/2,
	 create_string/2,
	 create_wstring/2,
	 create_sequence/3,
	 create_array/3,
	 create_idltype/2,		%not in CORBA 2.0
	 create_fixed/3,
	 get_type_def/1,
	 set_type_def/2,
	 get_value/1,
	 set_value/2,
	 get_members/1,
	 set_members/2,
	 get_discriminator_type/1,
	 get_discriminator_type_def/1,
	 set_discriminator_type_def/2,
	 get_original_type_def/1,
	 set_original_type_def/2,
	 get_kind/1,
	 get_bound/1,
	 set_bound/2,
	 get_element_type/1,
	 get_element_type_def/1,
	 set_element_type_def/2,
	 get_length/1,
	 set_length/2,
	 get_mode/1,
	 set_mode/2,
	 get_result/1,
	 get_result_def/1,
	 set_result_def/2,
	 get_params/1,
	 set_params/2,
	 get_contexts/1,
	 set_contexts/2,
	 get_exceptions/1,
	 set_exceptions/2,
	 get_base_interfaces/1,
	 set_base_interfaces/2,
	 is_a/2,
	 describe_interface/1,
	 create_attribute/6,
	 create_operation/9
 	]).

%% Light IFR operations
-export([initialize/3, 
	 get_module/2,
	 get_tc/2,
	 add_module/3, add_module/4,
	 add_constant/3, add_constant/4,
	 add_struct/3, add_struct/4,
	 add_union/3, add_union/4,
	 add_enum/3, add_enum/4,
	 add_alias/3, add_alias/4,
	 add_interface/3, add_interface/4,
	 add_exception/3, add_exception/4,
	 remove/2,
	 add_items/3]).


-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%======================================================================
%% Public interfaces to the IFR
%%======================================================================
%%=================== Light IFR operations =============================
%%----------------------------------------------------------------------
%% Function   : get_module
%% Arguments  : Id - string()
%%              Type - ?IFR_ModuleDef | ?IFR_ConstantDef | ?IFR_StructDef |
%%                     ?IFR_UnionDef | ?IFR_EnumDef | ?IFR_AliasDef |
%%                     ?IFR_InterfaceDef | ?IFR_ExceptionDef
%% Returns    : Module - atom() | {'EXCEPTION', E}
%% Raises     : #'MARSHAL'{}
%% Description: 
%%----------------------------------------------------------------------
get_module(Id, Type) ->
    case mnesia:dirty_read(orber_light_ifr, Id) of
	[#orber_light_ifr{module = Module, type = Type}] ->
	    Module;
	What ->
	    orber:dbg("[~p] ~p:get_module(~p, ~p).~n"
		      "Id doesn't exist, mismatch Id vs Type or DB error: ~p", 
		      [?LINE, ?MODULE, Id, Type, What], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE})
    end.


%%----------------------------------------------------------------------
%% Function   : get_tc
%% Arguments  : Id - string()
%%              Type - ?IFR_ModuleDef | ?IFR_ConstantDef | ?IFR_StructDef |
%%                     ?IFR_UnionDef | ?IFR_EnumDef | ?IFR_AliasDef |
%%                     ?IFR_InterfaceDef | ?IFR_ExceptionDef
%% Returns    : Module - atom() | {'EXCEPTION', E}
%% Raises     : #'MARSHAL'{}
%% Description: This function may *only* return correct TypeCode or raise
%%              a system exception!!
%%----------------------------------------------------------------------
get_tc(Id, Type) ->
    case catch mnesia:dirty_read(orber_light_ifr, Id) of
	[#orber_light_ifr{module = Module, type = Type}] ->
	    case catch Module:tc() of
		{'EXIT', Reason} ->
		    case Reason of
			{undef,[{Module, tc,[],_}|_]} ->
			    orber:dbg("[~p] ~p:get_tc(~p);~nMissing ~p:tc()~n",
				      [?LINE, ?MODULE, Id, Module], ?DEBUG_LEVEL),
			    corba:raise(#'UNKNOWN'{minor=(?ORBER_VMCID bor 1),
						   completion_status=?COMPLETED_MAYBE});
			_ ->
			    orber:dbg("[~p] ~p:get_tc(~p, ~p);~nEXIT reason: ~p~n",
				      [?LINE, ?MODULE, Id, Module, Reason], 
				      ?DEBUG_LEVEL),
			    corba:raise(#'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 1),
						   completion_status=?COMPLETED_MAYBE})
		    end;
		TC ->
		    TC
	    end;
	What when Type == ?IFR_ExceptionDef ->
	    orber:dbg("[~p] ~p:get_tc(~p, ExceptionDef);~nUnknown: ~p~n",
		      [?LINE, ?MODULE, Id, What], ?DEBUG_LEVEL),
	    corba:raise(#'UNKNOWN'{completion_status=?COMPLETED_MAYBE});
	What ->
	    orber:dbg("[~p] ~p:get_tc(~p, ~p);~nUnknown: ~p~n",
		      [?LINE, ?MODULE, Id, Type, What], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE})
    end.

%%----------------------------------------------------------------------
%% Function   : initialize
%% Arguments  : Timeout - integer() | infinity
%%              Options - [{Key, Value}]
%%              LightIFR - true | false
%% Returns    : ok  | {'EXCEPTION', E}
%% Raises     : #'INTF_REPOS'{}
%% Description: 
%%----------------------------------------------------------------------
initialize(Timeout, Options, LightIFR) ->
    orber_ifr_utils:init_DB(Timeout, Options, LightIFR).

%%----------------------------------------------------------------------
%% Function   : add_X
%% Arguments  : Id - string()
%%              Module - atom()
%%              BaseId - string()
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
add_module(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_ModuleDef, false).
add_module(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_ModuleDef, Transaction).

add_constant(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_ConstantDef, false).
add_constant(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_ConstantDef, Transaction).

add_struct(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_StructDef, false).
add_struct(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_StructDef, Transaction).

add_union(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_UnionDef, false).
add_union(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_UnionDef, Transaction).
    
add_enum(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_EnumDef, false).
add_enum(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_EnumDef, Transaction).
    
add_alias(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_AliasDef, false).
add_alias(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_AliasDef, Transaction).
    
add_interface(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_InterfaceDef, false).
add_interface(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_InterfaceDef, Transaction).

add_exception(Id, Module, BaseId) ->
    add_it(Id, Module, BaseId, ?IFR_ExceptionDef, false).
add_exception(Id, Module, BaseId, Transaction) ->
    add_it(Id, Module, BaseId, ?IFR_ExceptionDef, Transaction).
    

%%----------------------------------------------------------------------
%% Function   : add_it
%% Arguments  : Id - string()
%%              Module - atom()
%%              BaseId - string()
%%              Type - ?IFR_ModuleDef | ?IFR_ConstantDef | ?IFR_StructDef |
%%                     ?IFR_UnionDef | ?IFR_EnumDef | ?IFR_AliasDef |
%%                     ?IFR_InterfaceDef | ?IFR_ExceptionDef
%%              Transaction - true | false
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
add_it(Id, Module, BaseId, Type, true) ->
    F = fun() ->
		D = #orber_light_ifr{id = Id, module = Module,
				     type = Type, base_id = BaseId},
		mnesia:write(D)
	end,
    case mnesia:transaction(F) of 
        {aborted, Reason} ->
	    orber:dbg("[~p] orber_ifr:add_it(~p). aborted:~n~p~n", 
		      [?LINE, Id, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	{atomic, _} ->
	    ok
    end;
add_it(Id, Module, BaseId, Type, false) ->
    D = #orber_light_ifr{id = Id, module = Module,
			 type = Type, base_id = BaseId},
    mnesia:write(D).

%%----------------------------------------------------------------------
%% Function   : remove
%% Arguments  : BaseId - atom()
%%              Options - [KeyValue]
%%              KeyValue - {storage, mnesia | ets}
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
remove(ContainerId, _Options) ->
    F = fun() -> 
		MatchHead = #orber_light_ifr{id = '$1', base_id = ContainerId, _='_'},
		Result = '$1',
		IdList = mnesia:select(orber_light_ifr, 
				       [{MatchHead, [], [Result]}], 
				       write),
		lists:foreach(fun(RefId) ->
				      mnesia:delete({orber_light_ifr, RefId})
			      end, IdList)
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    orber:dbg("[~p] orber_ifr:remove(~p). aborted:~n~p~n", 
		      [?LINE, ContainerId, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	{atomic, _} ->
	    ok
    end.

%%----------------------------------------------------------------------
%% Function   : add_items
%% Arguments  : ContainerId - atom()
%%              Options - [KeyValue]
%%              KeyValue - {storage, mnesia | ets}
%%              Items - [{Id, Module, Type}]
%%              Id - string()
%%              Module - atom()
%%              Type - struct | except | union | interface
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
add_items(ContainerId, _Options, Items) ->
    F = fun() -> 
		mnesia:write_lock_table(orber_light_ifr),
		add_items_helper(Items, ContainerId)
	end,
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    orber:dbg("[~p] orber_ifr:add_items(~p). aborted:~n~p~n", 
		      [?LINE, ContainerId, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	{atomic, _} ->
	    ok
    end.

add_items_helper([{Id, Module, struct}|T], ContainerId) ->
    add_it(Id, Module, ContainerId, ?IFR_StructDef, false),
    add_items_helper(T, ContainerId);
add_items_helper([{Id, Module, interface}|T], ContainerId) ->
    add_it(Id, Module, ContainerId, ?IFR_InterfaceDef, false),
    add_items_helper(T, ContainerId);
add_items_helper([{Id, Module, except}|T], ContainerId) ->
    add_it(Id, Module, ContainerId, ?IFR_ExceptionDef, false),
    add_items_helper(T, ContainerId);
add_items_helper([{Id, Module, union}|T], ContainerId) ->
    add_it(Id, Module, ContainerId, ?IFR_UnionDef, false),
    add_items_helper(T, ContainerId);
add_items_helper([ok], _) ->
    ok.


%%=================== End Light IFR operations =========================

%% Initialize the database
init(Nodes, Timeout) when is_atom(Timeout) orelse is_integer(Timeout) ->
    orber_ifr_utils:init_DB(Timeout, [{disc_copies, Nodes}]);
init(Timeout, Nodes) ->
    orber_ifr_utils:init_DB(Timeout, [{disc_copies, Nodes}]).


%%% Find the repository
find_repository() ->
    orber_ifr_utils:create_repository().

'IRObject__get_def_kind'(Objref) ->
    orber_ifr_irobject:'_get_def_kind'(Objref).
%%'IRObject_destroy'(Objref) ->
%%    orber_ifr_irobject:destroy(Objref).

'Contained__get_def_kind'(Objref) ->
    orber_ifr_contained:'_get_def_kind'(Objref).
%%'Contained_destroy'(Objref) ->
%%    orber_ifr_contained:destroy(Objref).
'Contained__get_id'(Objref) ->
    orber_ifr_contained:'_get_id'(Objref).
'Contained__set_id'(Objref,Id) ->
    orber_ifr_contained:'_set_id'(Objref,Id).
'Contained__get_name'(Objref) ->
    orber_ifr_contained:'_get_name'(Objref).
'Contained__set_name'(Objref,Name) ->
    orber_ifr_contained:'_set_name'(Objref,Name).
'Contained__get_version'(Objref) ->
    orber_ifr_contained:'_get_version'(Objref).
'Contained__set_version'(Objref,Version) ->
    orber_ifr_contained:'_set_version'(Objref,Version).
'Contained__get_defined_in'(Objref) ->
    orber_ifr_contained:'_get_defined_in'(Objref).
'Contained__get_absolute_name'(Objref) ->
    orber_ifr_contained:'_get_absolute_name'(Objref).
'Contained__get_containing_repository'(Objref) ->
    orber_ifr_contained:'_get_containing_repository'(Objref).
'Contained_describe'(Objref) ->
    orber_ifr_contained:describe(Objref).
'Contained_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_contained:move(Objref,New_container,New_name,New_version).

'Container__get_def_kind'(Objref) ->
    orber_ifr_container:'_get_def_kind'(Objref).
'Container_destroy'(Objref) ->
    orber_ifr_container:destroy(Objref).
'Container_lookup'(Objref,Search_name) ->
    orber_ifr_container:lookup(Objref,Search_name).
'Container_contents'(Objref,Limit_type,Exclude_inherited) ->
    orber_ifr_container:contents(Objref,Limit_type,Exclude_inherited).
'Container_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			Exclude_inherited) ->
    orber_ifr_container:lookup_name(Objref,Search_name,Levels_to_search,Limit_type,
				    Exclude_inherited).
'Container_describe_contents'(Objref,Limit_type,Exclude_inherited,
					 Max_returned_objs) ->
    orber_ifr_container:describe_contents(Objref,Limit_type,Exclude_inherited,
					  Max_returned_objs).
'Container_create_module'(Objref,Id,Name,Version) ->
    orber_ifr_container:create_module(Objref,Id,Name,Version).
'Container_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    orber_ifr_container:create_constant(Objref,Id,Name,Version,Type,Value).
'Container_create_struct'(Objref,Id,Name,Version,Members) ->
    orber_ifr_container:create_struct(Objref,Id,Name,Version,Members).
'Container_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    orber_ifr_container:create_union(Objref,Id,Name,Version,Discriminator_type,
				     Members).
'Container_create_enum'(Objref,Id,Name,Version,Members) ->
    orber_ifr_container:create_enum(Objref,Id,Name,Version,Members).
'Container_create_alias'(Objref,Id,Name,Version,Original_type) ->
    orber_ifr_container:create_alias(Objref,Id,Name,Version,Original_type).
'Container_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    orber_ifr_container:create_interface(Objref,Id,Name,Version,Base_interfaces).
'Container_create_exception'(Objref,Id,Name,Version,Members) ->
    orber_ifr_container:create_exception(Objref,Id,Name,Version,Members).

'IDLType__get_def_kind'(Objref) ->
    orber_ifr_idltype:'_get_def_kind'(Objref).
'IDLType_destroy'(Objref) ->
    orber_ifr_idltype:destroy(Objref).
'IDLType__get_type'(Objref) ->
    orber_ifr_idltype:'_get_type'(Objref).

'Repository__get_def_kind'(Objref) ->
    orber_ifr_repository:'_get_def_kind'(Objref).
'Repository_destroy'(Objref) ->
    orber_ifr_repository:destroy(Objref).
'Repository_lookup'(Objref,Search_name) ->
    orber_ifr_repository:lookup(Objref,Search_name).
'Repository_contents'(Objref,Limit_type,Exclude_inherited) ->
    orber_ifr_repository:contents(Objref,Limit_type,Exclude_inherited).
'Repository_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			 Exclude_inherited) ->
    orber_ifr_repository:lookup_name(Objref,Search_name,Levels_to_search,Limit_type,
				     Exclude_inherited).
'Repository_describe_contents'(Objref,Limit_type,Exclude_inherited,
			       Max_returned_objs) ->
    orber_ifr_repository:describe_contents(Objref,Limit_type,Exclude_inherited,
					   Max_returned_objs).
'Repository_create_module'(Objref,Id,Name,Version) ->
    orber_ifr_repository:create_module(Objref,Id,Name,Version).
'Repository_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    orber_ifr_repository:create_constant(Objref,Id,Name,Version,Type,Value).
'Repository_create_struct'(Objref,Id,Name,Version,Members) ->
    orber_ifr_repository:create_struct(Objref,Id,Name,Version,Members).
'Repository_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    orber_ifr_repository:create_union(Objref,Id,Name,Version,Discriminator_type,
				      Members).
'Repository_create_enum'(Objref,Id,Name,Version,Members) ->
    orber_ifr_repository:create_enum(Objref,Id,Name,Version,Members).
'Repository_create_alias'(Objref,Id,Name,Version,Original_type) ->
    orber_ifr_repository:create_alias(Objref,Id,Name,Version,Original_type).
'Repository_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    orber_ifr_repository:create_interface(Objref,Id,Name,Version,Base_interfaces).
'Repository_create_exception'(Objref,Id,Name,Version,Members) ->
    orber_ifr_repository:create_exception(Objref,Id,Name,Version,Members).
'Repository_lookup_id'(Objref,Search_id) ->
    lookup_id(Objref,Search_id).
'Repository_get_primitive'(Objref,Kind) ->
    orber_ifr_repository:get_primitive(Objref,Kind).
'Repository_create_string'(Objref,Bound) ->
    orber_ifr_repository:create_string(Objref,Bound).
'Repository_create_wstring'(Objref,Bound) ->
    orber_ifr_repository:create_wstring(Objref,Bound).
'Repository_create_sequence'(Objref,Bound,Element_type) ->
    orber_ifr_repository:create_sequence(Objref,Bound,Element_type).
'Repository_create_array'(Objref,Length,Element_type) ->
    orber_ifr_repository:create_array(Objref,Length,Element_type).
'Repository_create_idltype'(Objref,Typecode) ->
    orber_ifr_repository:create_idltype(Objref,Typecode).

'ModuleDef__get_def_kind'(Objref) ->
    orber_ifr_moduledef:'_get_def_kind'(Objref).
'ModuleDef_destroy'(Objref) ->
    orber_ifr_moduledef:destroy(Objref).
'ModuleDef_lookup'(Objref,Search_name) ->
    orber_ifr_moduledef:lookup(Objref,Search_name).
'ModuleDef_contents'(Objref,Limit_type,Exclude_inherited) ->
    orber_ifr_moduledef:contents(Objref,Limit_type,Exclude_inherited).
'ModuleDef_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			Exclude_inherited) ->
    orber_ifr_moduledef:lookup_name(Objref,Search_name,Levels_to_search,Limit_type,
				    Exclude_inherited).
'ModuleDef_describe_contents'(Objref,Limit_type,Exclude_inherited,
			      Max_returned_objs) ->
    orber_ifr_moduledef:describe_contents(Objref,Limit_type,Exclude_inherited,
					  Max_returned_objs).
'ModuleDef_create_module'(Objref,Id,Name,Version) ->
    orber_ifr_moduledef:create_module(Objref,Id,Name,Version).
'ModuleDef_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    orber_ifr_moduledef:create_constant(Objref,Id,Name,Version,Type,Value).
'ModuleDef_create_struct'(Objref,Id,Name,Version,Members) ->
    orber_ifr_moduledef:create_struct(Objref,Id,Name,Version,Members).
'ModuleDef_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    orber_ifr_moduledef:create_union(Objref,Id,Name,Version,Discriminator_type,
				     Members).
'ModuleDef_create_enum'(Objref,Id,Name,Version,Members) ->
    orber_ifr_moduledef:create_enum(Objref,Id,Name,Version,Members).
'ModuleDef_create_alias'(Objref,Id,Name,Version,Original_type) ->
    orber_ifr_moduledef:create_alias(Objref,Id,Name,Version,Original_type).
'ModuleDef_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    orber_ifr_moduledef:create_interface(Objref,Id,Name,Version,Base_interfaces).
'ModuleDef_create_exception'(Objref,Id,Name,Version,Members) ->
    orber_ifr_moduledef:create_exception(Objref,Id,Name,Version,Members).
'ModuleDef__get_id'(Objref) ->
    orber_ifr_moduledef:'_get_id'(Objref).
'ModuleDef__set_id'(Objref,Id) ->
    orber_ifr_moduledef:'_set_id'(Objref,Id).
'ModuleDef__get_name'(Objref) ->
    orber_ifr_moduledef:'_get_name'(Objref).
'ModuleDef__set_name'(Objref,Name) ->
    orber_ifr_moduledef:'_set_name'(Objref,Name).
'ModuleDef__get_version'(Objref) ->
    orber_ifr_moduledef:'_get_version'(Objref).
'ModuleDef__set_version'(Objref,Version) ->
    orber_ifr_moduledef:'_set_version'(Objref,Version).
'ModuleDef__get_defined_in'(Objref) ->
    orber_ifr_moduledef:'_get_defined_in'(Objref).
'ModuleDef__get_absolute_name'(Objref) ->
    orber_ifr_moduledef:'_get_absolute_name'(Objref).
'ModuleDef__get_containing_repository'(Objref) ->
    orber_ifr_moduledef:'_get_containing_repository'(Objref).
'ModuleDef_describe'(Objref) ->
    orber_ifr_moduledef:describe(Objref).
'ModuleDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_moduledef:move(Objref,New_container,New_name,New_version).

'ConstantDef__get_def_kind'(Objref) ->
    orber_ifr_constantdef:'_get_def_kind'(Objref).
'ConstantDef_destroy'(Objref) ->
    orber_ifr_constantdef:destroy(Objref).
'ConstantDef__get_id'(Objref) ->
    orber_ifr_constantdef:'_get_id'(Objref).
'ConstantDef__set_id'(Objref,Id) ->
    orber_ifr_constantdef:'_set_id'(Objref,Id).
'ConstantDef__get_name'(Objref) ->
    orber_ifr_constantdef:'_get_name'(Objref).
'ConstantDef__set_name'(Objref,Name) ->
    orber_ifr_constantdef:'_set_name'(Objref,Name).
'ConstantDef__get_version'(Objref) ->
    orber_ifr_constantdef:'_get_version'(Objref).
'ConstantDef__set_version'(Objref,Version) ->
    orber_ifr_constantdef:'_set_version'(Objref,Version).
'ConstantDef__get_defined_in'(Objref) ->
    orber_ifr_constantdef:'_get_defined_in'(Objref).
'ConstantDef__get_absolute_name'(Objref) ->
    orber_ifr_constantdef:'_get_absolute_name'(Objref).
'ConstantDef__get_containing_repository'(Objref) ->
    orber_ifr_constantdef:'_get_containing_repository'(Objref).
'ConstantDef_describe'(Objref) ->
    orber_ifr_constantdef:describe(Objref).
'ConstantDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_constantdef:move(Objref,New_container,New_name,New_version).
'ConstantDef__get_type'(Objref) ->
    orber_ifr_constantdef:'_get_type'(Objref).
'ConstantDef__get_type_def'(Objref) ->
    orber_ifr_constantdef:'_get_type_def'(Objref).
'ConstantDef__set_type_def'(Objref,TypeDef) ->
    orber_ifr_constantdef:'_set_type_def'(Objref,TypeDef).
'ConstantDef__get_value'(Objref) ->
    orber_ifr_constantdef:'_get_value'(Objref).
'ConstantDef__set_value'(Objref,Value) ->
    orber_ifr_constantdef:'_set_value'(Objref,Value).

'TypedefDef__get_def_kind'(Objref) ->
    orber_ifr_typedef:'_get_def_kind'(Objref).
'TypedefDef_destroy'(Objref) ->
    orber_ifr_typedef:destroy(Objref).
'TypedefDef__get_id'(Objref) ->
    orber_ifr_typedef:'_get_id'(Objref).
'TypedefDef__set_id'(Objref,Id) ->
    orber_ifr_typedef:'_set_id'(Objref,Id).
'TypedefDef__get_name'(Objref) ->
    orber_ifr_typedef:'_get_name'(Objref).
'TypedefDef__set_name'(Objref,Name) ->
    orber_ifr_typedef:'_set_name'(Objref,Name).
'TypedefDef__get_version'(Objref) ->
    orber_ifr_typedef:'_get_version'(Objref).
'TypedefDef__set_version'(Objref,Version) ->
    orber_ifr_typedef:'_set_version'(Objref,Version).
'TypedefDef__get_defined_in'(Objref) ->
    orber_ifr_typedef:'_get_defined_in'(Objref).
'TypedefDef__get_absolute_name'(Objref) ->
    orber_ifr_typedef:'_get_absolute_name'(Objref).
'TypedefDef__get_containing_repository'(Objref) ->
    orber_ifr_typedef:'_get_containing_repository'(Objref).
'TypedefDef_describe'(Objref) ->
    orber_ifr_typedef:describe(Objref).
'TypedefDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_typedef:move(Objref,New_container,New_name,New_version).
'TypedefDef__get_type'(Objref) ->
    orber_ifr_typedef:'_get_type'(Objref).

'StructDef__get_def_kind'(Objref) ->
    orber_ifr_structdef:'_get_def_kind'(Objref).
'StructDef_destroy'(Objref) ->
    orber_ifr_structdef:destroy(Objref).
'StructDef__get_id'(Objref) ->
    orber_ifr_structdef:'_get_id'(Objref).
'StructDef__set_id'(Objref,Id) ->
    orber_ifr_structdef:'_set_id'(Objref,Id).
'StructDef__get_name'(Objref) ->
    orber_ifr_structdef:'_get_name'(Objref).
'StructDef__set_name'(Objref,Name) ->
    orber_ifr_structdef:'_set_name'(Objref,Name).
'StructDef__get_version'(Objref) ->
    orber_ifr_structdef:'_get_version'(Objref).
'StructDef__set_version'(Objref,Version) ->
    orber_ifr_structdef:'_set_version'(Objref,Version).
'StructDef__get_defined_in'(Objref) ->
    orber_ifr_structdef:'_get_defined_in'(Objref).
'StructDef__get_absolute_name'(Objref) ->
    orber_ifr_structdef:'_get_absolute_name'(Objref).
'StructDef__get_containing_repository'(Objref) ->
    orber_ifr_structdef:'_get_containing_repository'(Objref).
'StructDef_describe'(Objref) ->
    orber_ifr_structdef:describe(Objref).
'StructDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_structdef:move(Objref,New_container,New_name,New_version).
'StructDef__get_type'(Objref) ->
    orber_ifr_structdef:'_get_type'(Objref).
'StructDef__get_members'(Objref) ->
    orber_ifr_structdef:'_get_members'(Objref).
'StructDef__set_members'(Objref,Members) ->
    orber_ifr_structdef:'_set_members'(Objref,Members).

'UnionDef__get_def_kind'(Objref) ->
    orber_ifr_uniondef:'_get_def_kind'(Objref).
'UnionDef_destroy'(Objref) ->
    orber_ifr_uniondef:destroy(Objref).
'UnionDef__get_id'(Objref) ->
    orber_ifr_uniondef:'_get_id'(Objref).
'UnionDef__set_id'(Objref,Id) ->
    orber_ifr_uniondef:'_set_id'(Objref,Id).
'UnionDef__get_name'(Objref) ->
    orber_ifr_uniondef:'_get_name'(Objref).
'UnionDef__set_name'(Objref,Name) ->
    orber_ifr_uniondef:'_set_name'(Objref,Name).
'UnionDef__get_version'(Objref) ->
    orber_ifr_uniondef:'_get_version'(Objref).
'UnionDef__set_version'(Objref,Version) ->
    orber_ifr_uniondef:'_set_version'(Objref,Version).
'UnionDef__get_defined_in'(Objref) ->
    orber_ifr_uniondef:'_get_defined_in'(Objref).
'UnionDef__get_absolute_name'(Objref) ->
    orber_ifr_uniondef:'_get_absolute_name'(Objref).
'UnionDef__get_containing_repository'(Objref) ->
    orber_ifr_uniondef:'_get_containing_repository'(Objref).
'UnionDef_describe'(Objref) ->
    orber_ifr_uniondef:describe(Objref).
'UnionDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_uniondef:move(Objref,New_container,New_name,New_version).
'UnionDef__get_type'(Objref) ->
    orber_ifr_uniondef:'_get_type'(Objref).
'UnionDef__get_discriminator_type'(Objref) ->
    orber_ifr_uniondef:'_get_discriminator_type'(Objref).
'UnionDef__get_discriminator_type_def'(Objref) ->
    orber_ifr_uniondef:'_get_discriminator_type_def'(Objref).
'UnionDef__set_discriminator_type_def'(Objref,TypeDef) ->
    orber_ifr_uniondef:'_set_discriminator_type_def'(Objref,TypeDef).
'UnionDef__get_members'(Objref) ->
    orber_ifr_uniondef:'_get_members'(Objref).
'UnionDef__set_members'(Objref,Members) ->
    orber_ifr_uniondef:'_set_members'(Objref,Members).

'EnumDef__get_def_kind'(Objref) ->
    orber_ifr_enumdef:'_get_def_kind'(Objref).
'EnumDef_destroy'(Objref) ->
    orber_ifr_enumdef:destroy(Objref).
'EnumDef__get_id'(Objref) ->
    orber_ifr_enumdef:'_get_id'(Objref).
'EnumDef__set_id'(Objref,Id) ->
    orber_ifr_enumdef:'_set_id'(Objref,Id).
'EnumDef__get_name'(Objref) ->
    orber_ifr_enumdef:'_get_name'(Objref).
'EnumDef__set_name'(Objref,Name) ->
    orber_ifr_enumdef:'_set_name'(Objref,Name).
'EnumDef__get_version'(Objref) ->
    orber_ifr_enumdef:'_get_version'(Objref).
'EnumDef__set_version'(Objref,Version) ->
    orber_ifr_enumdef:'_set_version'(Objref,Version).
'EnumDef__get_defined_in'(Objref) ->
    orber_ifr_enumdef:'_get_defined_in'(Objref).
'EnumDef__get_absolute_name'(Objref) ->
    orber_ifr_enumdef:'_get_absolute_name'(Objref).
'EnumDef__get_containing_repository'(Objref) ->
    orber_ifr_enumdef:'_get_containing_repository'(Objref).
'EnumDef_describe'(Objref) ->
    orber_ifr_enumdef:describe(Objref).
'EnumDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_enumdef:move(Objref,New_container,New_name,New_version).
'EnumDef__get_type'(Objref) ->
    orber_ifr_enumdef:'_get_type'(Objref).
'EnumDef__get_members'(Objref) ->
    orber_ifr_enumdef:'_get_members'(Objref).
'EnumDef__set_members'(Objref,Members) ->
    orber_ifr_enumdef:'_set_members'(Objref,Members).

'AliasDef__get_def_kind'(Objref) ->
    orber_ifr_aliasdef:'_get_def_kind'(Objref).
'AliasDef_destroy'(Objref) ->
    orber_ifr_aliasdef:destroy(Objref).
'AliasDef__get_id'(Objref) ->
    orber_ifr_aliasdef:'_get_id'(Objref).
'AliasDef__set_id'(Objref,Id) ->
    orber_ifr_aliasdef:'_set_id'(Objref,Id).
'AliasDef__get_name'(Objref) ->
    orber_ifr_aliasdef:'_get_name'(Objref).
'AliasDef__set_name'(Objref,Name) ->
    orber_ifr_aliasdef:'_set_name'(Objref,Name).
'AliasDef__get_version'(Objref) ->
    orber_ifr_aliasdef:'_get_version'(Objref).
'AliasDef__set_version'(Objref,Version) ->
    orber_ifr_aliasdef:'_set_version'(Objref,Version).
'AliasDef__get_defined_in'(Objref) ->
    orber_ifr_aliasdef:'_get_defined_in'(Objref).
'AliasDef__get_absolute_name'(Objref) ->
    orber_ifr_aliasdef:'_get_absolute_name'(Objref).
'AliasDef__get_containing_repository'(Objref) ->
    orber_ifr_aliasdef:'_get_containing_repository'(Objref).
'AliasDef_describe'(Objref) ->
    orber_ifr_aliasdef:describe(Objref).
'AliasDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_aliasdef:move(Objref,New_container,New_name,New_version).
'AliasDef__get_type'(Objref) ->
    orber_ifr_aliasdef:'_get_type'(Objref).
'AliasDef__get_original_type_def'(Objref) ->
    orber_ifr_aliasdef:'_get_original_type_def'(Objref).
'AliasDef__set_original_type_def'(Objref,TypeDef) ->
    orber_ifr_aliasdef:'_set_original_type_def'(Objref,TypeDef).

'PrimitiveDef__get_def_kind'(Objref) ->
    orber_ifr_primitivedef:'_get_def_kind'(Objref).
'PrimitiveDef_destroy'(Objref) ->
    orber_ifr_primitivedef:destroy(Objref).
'PrimitiveDef__get_type'(Objref) ->
    orber_ifr_primitivedef:'_get_type'(Objref).
'PrimitiveDef__get_kind'(Objref) ->
    orber_ifr_primitivedef:'_get_kind'(Objref).

'StringDef__get_def_kind'(Objref) ->
    orber_ifr_stringdef:'_get_def_kind'(Objref).
'StringDef_destroy'(Objref) ->
    orber_ifr_stringdef:destroy(Objref).
'StringDef__get_type'(Objref) ->
    orber_ifr_stringdef:'_get_type'(Objref).
'StringDef__get_bound'(Objref) ->
    orber_ifr_stringdef:'_get_bound'(Objref).
'StringDef__set_bound'(Objref,Bound) ->
    orber_ifr_stringdef:'_set_bound'(Objref,Bound).

'WstringDef__get_def_kind'(Objref) ->
    orber_ifr_wstringdef:'_get_def_kind'(Objref).
'WstringDef_destroy'(Objref) ->
    orber_ifr_wstringdef:destroy(Objref).
'WstringDef__get_type'(Objref) ->
    orber_ifr_wstringdef:'_get_type'(Objref).
'WstringDef__get_bound'(Objref) ->
    orber_ifr_wstringdef:'_get_bound'(Objref).
'WstringDef__set_bound'(Objref,Bound) ->
    orber_ifr_wstringdef:'_set_bound'(Objref,Bound).

'FixedDef__get_def_kind'(Objref) ->
    orber_ifr_fixeddef:'_get_def_kind'(Objref).
'FixedDef_destroy'(Objref) ->
    orber_ifr_fixeddef:destroy(Objref).
'FixedDef__get_type'(Objref) ->
    orber_ifr_fixeddef:'_get_type'(Objref).
'FixedDef__get_digits'(Objref) ->
    orber_ifr_fixeddef:'_get_digits'(Objref).
'FixedDef__set_digits'(Objref,Digits) ->
    orber_ifr_fixeddef:'_set_digits'(Objref,Digits).
'FixedDef__get_scale'(Objref) ->
    orber_ifr_fixeddef:'_get_scale'(Objref).
'FixedDef__set_scale'(Objref,Scale) ->
    orber_ifr_fixeddef:'_set_scale'(Objref,Scale).

'SequenceDef__get_def_kind'(Objref) ->
    orber_ifr_sequencedef:'_get_def_kind'(Objref).
'SequenceDef_destroy'(Objref) ->
    orber_ifr_sequencedef:destroy(Objref).
'SequenceDef__get_type'(Objref) ->
    orber_ifr_sequencedef:'_get_type'(Objref).
'SequenceDef__get_bound'(Objref) ->
    orber_ifr_sequencedef:'_get_bound'(Objref).
'SequenceDef__set_bound'(Objref,Bound) ->
    orber_ifr_sequencedef:'_set_bound'(Objref,Bound).
'SequenceDef__get_element_type'(Objref) ->
    orber_ifr_sequencedef:'_get_element_type'(Objref).
'SequenceDef__get_element_type_def'(Objref) ->
    orber_ifr_sequencedef:'_get_element_type_def'(Objref).
'SequenceDef__set_element_type_def'(Objref,TypeDef) ->
    orber_ifr_sequencedef:'_set_element_type_def'(Objref,TypeDef).

'ArrayDef__get_def_kind'(Objref) ->
    orber_ifr_arraydef:'_get_def_kind'(Objref).
'ArrayDef_destroy'(Objref) ->
    orber_ifr_arraydef:destroy(Objref).
'ArrayDef__get_type'(Objref) ->
    orber_ifr_arraydef:'_get_type'(Objref).
'ArrayDef__get_length'(Objref) ->
    orber_ifr_arraydef:'_get_length'(Objref).
'ArrayDef__set_length'(Objref,Length) ->
    orber_ifr_arraydef:'_set_length'(Objref,Length).
'ArrayDef__get_element_type'(Objref) ->
    orber_ifr_arraydef:'_get_element_type'(Objref).
'ArrayDef__get_element_type_def'(Objref) ->
    orber_ifr_arraydef:'_get_element_type_def'(Objref).
'ArrayDef__set_element_type_def'(Objref,TypeDef) ->
    orber_ifr_arraydef:'_set_element_type_def'(Objref,TypeDef).

'ExceptionDef__get_def_kind'(Objref) ->
    orber_ifr_exceptiondef:'_get_def_kind'(Objref).
'ExceptionDef_destroy'(Objref) ->
    orber_ifr_exceptiondef:destroy(Objref).
'ExceptionDef__get_id'(Objref) ->
    orber_ifr_exceptiondef:'_get_id'(Objref).
'ExceptionDef__set_id'(Objref,Id) ->
    orber_ifr_exceptiondef:'_set_id'(Objref,Id).
'ExceptionDef__get_name'(Objref) ->
    orber_ifr_exceptiondef:'_get_name'(Objref).
'ExceptionDef__set_name'(Objref,Name) ->
    orber_ifr_exceptiondef:'_set_name'(Objref,Name).
'ExceptionDef__get_version'(Objref) ->
    orber_ifr_exceptiondef:'_get_version'(Objref).
'ExceptionDef__set_version'(Objref,Version) ->
    orber_ifr_exceptiondef:'_set_version'(Objref,Version).
'ExceptionDef__get_defined_in'(Objref) ->
    orber_ifr_exceptiondef:'_get_defined_in'(Objref).
'ExceptionDef__get_absolute_name'(Objref) ->
    orber_ifr_exceptiondef:'_get_absolute_name'(Objref).
'ExceptionDef__get_containing_repository'(Objref) ->
    orber_ifr_exceptiondef:'_get_containing_repository'(Objref).
'ExceptionDef_describe'(Objref) ->
    orber_ifr_exceptiondef:describe(Objref).
'ExceptionDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_exceptiondef:move(Objref,New_container,New_name,New_version).
'ExceptionDef__get_type'(Objref) ->
    orber_ifr_exceptiondef:'_get_type'(Objref).
'ExceptionDef__get_members'(Objref) ->
    orber_ifr_exceptiondef:'_get_members'(Objref).
'ExceptionDef__set_members'(Objref,Members) ->
    orber_ifr_exceptiondef:'_set_members'(Objref,Members).

'AttributeDef__get_def_kind'(Objref) ->
    orber_ifr_attributedef:'_get_def_kind'(Objref).
'AttributeDef_destroy'(Objref) ->
    orber_ifr_attributedef:destroy(Objref).
'AttributeDef__get_id'(Objref) ->
    orber_ifr_attributedef:'_get_id'(Objref).
'AttributeDef__set_id'(Objref,Id) ->
    orber_ifr_attributedef:'_set_id'(Objref,Id).
'AttributeDef__get_name'(Objref) ->
    orber_ifr_attributedef:'_get_name'(Objref).
'AttributeDef__set_name'(Objref,Name) ->
    orber_ifr_attributedef:'_set_name'(Objref,Name).
'AttributeDef__get_version'(Objref) ->
    orber_ifr_attributedef:'_get_version'(Objref).
'AttributeDef__set_version'(Objref,Version) ->
    orber_ifr_attributedef:'_set_version'(Objref,Version).
'AttributeDef__get_defined_in'(Objref) ->
    orber_ifr_attributedef:'_get_defined_in'(Objref).
'AttributeDef__get_absolute_name'(Objref) ->
    orber_ifr_attributedef:'_get_absolute_name'(Objref).
'AttributeDef__get_containing_repository'(Objref) ->
    orber_ifr_attributedef:'_get_containing_repository'(Objref).
'AttributeDef_describe'(Objref) ->
    orber_ifr_attributedef:describe(Objref).
'AttributeDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_attributedef:move(Objref,New_container,New_name,New_version).
'AttributeDef__get_type'(Objref) ->
    orber_ifr_attributedef:'_get_type'(Objref).
'AttributeDef__get_type_def'(Objref) ->
    orber_ifr_attributedef:'_get_type_def'(Objref).
'AttributeDef__set_type_def'(Objref,TypeDef) ->
    orber_ifr_attributedef:'_set_type_def'(Objref,TypeDef).
'AttributeDef__get_mode'(Objref) ->
    orber_ifr_attributedef:'_get_mode'(Objref).
'AttributeDef__set_mode'(Objref,Mode) ->
    orber_ifr_attributedef:'_set_mode'(Objref,Mode).

'OperationDef__get_def_kind'(Objref) ->
    orber_ifr_operationdef:'_get_def_kind'(Objref).
'OperationDef_destroy'(Objref) ->
    orber_ifr_operationdef:destroy(Objref).
'OperationDef__get_id'(Objref) ->
    orber_ifr_operationdef:'_get_id'(Objref).
'OperationDef__set_id'(Objref,Id) ->
    orber_ifr_operationdef:'_set_id'(Objref,Id).
'OperationDef__get_name'(Objref) ->
    orber_ifr_operationdef:'_get_name'(Objref).
'OperationDef__set_name'(Objref,Name) ->
    orber_ifr_operationdef:'_set_name'(Objref,Name).
'OperationDef__get_version'(Objref) ->
    orber_ifr_operationdef:'_get_version'(Objref).
'OperationDef__set_version'(Objref,Version) ->
    orber_ifr_operationdef:'_set_version'(Objref,Version).
'OperationDef__get_defined_in'(Objref) ->
    orber_ifr_operationdef:'_get_defined_in'(Objref).
'OperationDef__get_absolute_name'(Objref) ->
    orber_ifr_operationdef:'_get_absolute_name'(Objref).
'OperationDef__get_containing_repository'(Objref) ->
    orber_ifr_operationdef:'_get_containing_repository'(Objref).
'OperationDef_describe'(Objref) ->
    orber_ifr_operationdef:describe(Objref).
'OperationDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_operationdef:move(Objref,New_container,New_name,New_version).
'OperationDef__get_result'(Objref) ->
    orber_ifr_operationdef:'_get_result'(Objref).
'OperationDef__get_result_def'(Objref) ->
    orber_ifr_operationdef:'_get_result_def'(Objref).
'OperationDef__set_result_def'(Objref,ResultDef) ->
    orber_ifr_operationdef:'_set_result_def'(Objref,ResultDef).
'OperationDef__get_params'(Objref) ->
    orber_ifr_operationdef:'_get_params'(Objref).
'OperationDef__set_params'(Objref,Params) ->
    orber_ifr_operationdef:'_set_params'(Objref,Params).
'OperationDef__get_mode'(Objref) ->
    orber_ifr_operationdef:'_get_mode'(Objref).
'OperationDef__set_mode'(Objref,Mode) ->
    orber_ifr_operationdef:'_set_mode'(Objref,Mode).
'OperationDef__get_contexts'(Objref) ->
    orber_ifr_operationdef:'_get_contexts'(Objref).
'OperationDef__set_contexts'(Objref,Contexts) ->
    orber_ifr_operationdef:'_set_contexts'(Objref,Contexts).
'OperationDef__get_exceptions'(Objref) ->
    orber_ifr_operationdef:'_get_exceptions'(Objref).
'OperationDef__set_exceptions'(Objref,Exceptions) ->
    orber_ifr_operationdef:'_set_exceptions'(Objref,Exceptions).

'InterfaceDef__get_def_kind'(Objref) ->
    orber_ifr_interfacedef:'_get_def_kind'(Objref).
'InterfaceDef_destroy'(Objref) ->
    orber_ifr_interfacedef:destroy(Objref).
'InterfaceDef_lookup'(Objref,Search_name) ->
    orber_ifr_interfacedef:lookup(Objref,Search_name).
'InterfaceDef_contents'(Objref,Limit_type,Exclude_inherited) ->
    orber_ifr_interfacedef:contents(Objref,Limit_type,Exclude_inherited).
'InterfaceDef_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			   Exclude_inherited) ->
    orber_ifr_interfacedef:lookup_name(Objref,Search_name,Levels_to_search,Limit_type,
				       Exclude_inherited).
'InterfaceDef_describe_contents'(Objref,Limit_type,Exclude_inherited,
				 Max_returned_objs) ->
    orber_ifr_interfacedef:describe_contents(Objref,Limit_type,Exclude_inherited,
					     Max_returned_objs).
'InterfaceDef_create_module'(Objref,Id,Name,Version) ->
    orber_ifr_interfacedef:create_module(Objref,Id,Name,Version).
'InterfaceDef_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    orber_ifr_interfacedef:create_constant(Objref,Id,Name,Version,Type,Value).
'InterfaceDef_create_struct'(Objref,Id,Name,Version,Members) ->
    orber_ifr_interfacedef:create_struct(Objref,Id,Name,Version,Members).
'InterfaceDef_create_union'(Objref,Id,Name,Version,Discriminator_type,
			    Members) ->
    orber_ifr_interfacedef:create_union(Objref,Id,Name,Version,Discriminator_type,
					Members).
'InterfaceDef_create_enum'(Objref,Id,Name,Version,Members) ->
    orber_ifr_interfacedef:create_enum(Objref,Id,Name,Version,Members).
'InterfaceDef_create_alias'(Objref,Id,Name,Version,Original_type) ->
    orber_ifr_interfacedef:create_alias(Objref,Id,Name,Version,Original_type).
'InterfaceDef_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    orber_ifr_interfacedef:create_interface(Objref,Id,Name,Version,Base_interfaces).
'InterfaceDef_create_exception'(Objref,Id,Name,Version,Members) ->
    orber_ifr_interfacedef:create_exception(Objref,Id,Name,Version,Members).
'InterfaceDef__get_id'(Objref) ->
    orber_ifr_interfacedef:'_get_id'(Objref).
'InterfaceDef__set_id'(Objref,Id) ->
    orber_ifr_interfacedef:'_set_id'(Objref,Id).
'InterfaceDef__get_name'(Objref) ->
    orber_ifr_interfacedef:'_get_name'(Objref).
'InterfaceDef__set_name'(Objref,Name) ->
    orber_ifr_interfacedef:'_set_name'(Objref,Name).
'InterfaceDef__get_version'(Objref) ->
    orber_ifr_interfacedef:'_get_version'(Objref).
'InterfaceDef__set_version'(Objref,Version) ->
    orber_ifr_interfacedef:'_set_version'(Objref,Version).
'InterfaceDef__get_defined_in'(Objref) ->
    orber_ifr_interfacedef:'_get_defined_in'(Objref).
'InterfaceDef__get_absolute_name'(Objref) ->
    orber_ifr_interfacedef:'_get_absolute_name'(Objref).
'InterfaceDef__get_containing_repository'(Objref) ->
    orber_ifr_interfacedef:'_get_containing_repository'(Objref).
'InterfaceDef_describe'(Objref) ->
    orber_ifr_interfacedef:describe(Objref).
'InterfaceDef_move'(Objref,New_container,New_name,New_version) ->
    orber_ifr_interfacedef:move(Objref,New_container,New_name,New_version).
'InterfaceDef__get_type'(Objref) ->
    orber_ifr_interfacedef:'_get_type'(Objref).
'InterfaceDef__get_base_interfaces'(Objref) ->
    orber_ifr_interfacedef:'_get_base_interfaces'(Objref).
'InterfaceDef__set_base_interfaces'(Objref,BaseInterfaces) ->
    orber_ifr_interfacedef:'_set_base_interfaces'(Objref,BaseInterfaces).
'InterfaceDef_is_a'(Objref,Interface_id) ->
    orber_ifr_interfacedef:is_a(Objref,Interface_id).
'InterfaceDef_describe_interface'(Objref) ->
    orber_ifr_interfacedef:describe_interface(Objref).
'InterfaceDef_create_attribute'(Objref,Id,Name,Version,Type,Mode) ->
    orber_ifr_interfacedef:create_attribute(Objref,Id,Name,Version,Type,Mode).
'InterfaceDef_create_operation'(Objref,Id,Name,Version,Result,Mode,Params,
				Exceptions,Contexts) ->
    orber_ifr_interfacedef:create_operation(Objref,Id,Name,Version,Result,Mode,
					    Params,Exceptions,Contexts).

%%'TypeCode_equal'(Objref,Tc) ->
%%    orber_ifr_typecode:equal(Objref,Tc).
%%'TypeCode_kind'(Objref) ->
%%    orber_ifr_typecode:kind(Objref).
%%'TypeCode_id'(Objref) ->
%%    orber_ifr_typecode:id(Objref).
%%'TypeCode_name'(Objref) ->
%%    orber_ifr_typecode:name(Objref).
%%'TypeCode_member_count'(Objref) ->
%%    orber_ifr_typecode:member_count(Objref).
%%'TypeCode_member_name'(Objref,Index) ->
%%    orber_ifr_typecode:member_name(Objref,Index).
%%'TypeCode_member_type'(Objref,Index) ->
%%    orber_ifr_typecode:member_type(Objref,Index).
%%'TypeCode_member_label'(Objref,Index) ->
%%    orber_ifr_typecode:member_label(Objref,Index).
%%'TypeCode_discriminator_type'(Objref) ->
%%    orber_ifr_typecode:discriminator_type(Objref).
%%'TypeCode_default_index'(Objref) ->
%%    orber_ifr_typecode:default_index(Objref).
%%'TypeCode_length'(Objref) ->
%%    orber_ifr_typecode:length(Objref).
%%'TypeCode_content_type'(Objref) ->
%%    orber_ifr_typecode:content_type(Objref).
%%'TypeCode_param_count'(Objref) ->
%%    orber_ifr_typecode:param_count(Objref).
%%'TypeCode_parameter'(Objref,Index) ->
%%    orber_ifr_typecode:parameter(Objref,Index).

'ORB_create_struct_tc'(Id,Name,Members) ->
    orber_ifr_orb:create_struct_tc(Id,Name,Members).
'ORB_create_union_tc'(Id,Name,Discriminator_type,Members) ->
    orber_ifr_orb:create_union_tc(Id,Name,Discriminator_type,Members).
'ORB_create_enum_tc'(Id,Name,Members) ->
    orber_ifr_orb:create_enum_tc(Id,Name,Members).
'ORB_create_alias_tc'(Id,Name,Original_type) ->
    orber_ifr_orb:create_alias_tc(Id,Name,Original_type).
'ORB_create_exception_tc'(Id,Name,Members) ->
    orber_ifr_orb:create_exception_tc(Id,Name,Members).
'ORB_create_interface_tc'(Id,Name) ->
    orber_ifr_orb:create_interface_tc(Id,Name).
'ORB_create_string_tc'(Bound) ->
    orber_ifr_orb:create_string_tc(Bound).
'ORB_create_wstring_tc'(Bound) ->
    orber_ifr_orb:create_wstring_tc(Bound).
'ORB_create_sequence_tc'(Bound,Element_type) ->
    orber_ifr_orb:create_sequence_tc(Bound,Element_type).
'ORB_create_recursive_sequence_tc'(Bound,Offset) ->
    orber_ifr_orb:create_recursive_sequence_tc(Bound,Offset).
'ORB_create_array_tc'(Length,Element_type) ->
    orber_ifr_orb:create_array_tc(Length,Element_type).

%%%---------------------------------------------------------------
%%% "Methods" of the IFR "objects"

get_def_kind(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_def_kind'(Objref).
	     
%% Light IFR Operations
destroy(#orber_light_ifr_ref{data = #lightdata{id = Id}}) ->
    F = fun() -> 
		MatchHead = #orber_light_ifr{id = '$1', base_id = Id, _='_'},
		Result = '$1',
		IdList = mnesia:select(orber_light_ifr, 
				       [{MatchHead, [], [Result]}], 
				       write),
		lists:foreach(fun(RefId) ->
				      mnesia:delete({orber_light_ifr, RefId})
			      end, IdList)
	end,
    case mnesia:transaction(F) of
	{aborted, _} ->
            exit({"FAILED TO DELETE:", Id});
	{atomic, _} ->
	    ok
    end;
destroy(Objref) ->
    %% Destroying an ir_IRObject, ir_Contained or ir_Container directly
    %% is not allowed
    Mod = obj2mod(Objref),
    Mod:destroy(Objref).
	     
%%%---------------------------------------------------------------
%%%

get_id(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_id'(Objref).
	     
set_id(Objref,Id) ->
    Mod = obj2mod(Objref),
    Mod:'_set_id'(Objref,Id).
	     
get_name(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_name'(Objref).
	     
set_name(Objref,Name) ->
    Mod = obj2mod(Objref),
    Mod:'_set_name'(Objref,Name).
	     
get_version(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_version'(Objref).
	     
set_version(Objref,Version) ->
    Mod = obj2mod(Objref),
    Mod:'_set_version'(Objref,Version).

get_defined_in(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_defined_in'(Objref).
	     
get_absolute_name(Objref) ->
    Mod = obj2mod(Objref),
    Mod: '_get_absolute_name'(Objref).
	    
get_containing_repository(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_containing_repository'(Objref).
	     
describe(Objref) ->
    Mod = obj2mod(Objref),
    Mod:describe(Objref).

move(Objref,New_container,New_name,New_version) ->
    Mod = obj2mod(Objref),
    Mod:move(Objref,New_container,New_name,New_version).

%%%---------------------------------------------------------------
%%% 

lookup(Objref,Search_name) ->
    Mod = obj2mod(Objref),
    Mod:lookup(Objref,Search_name).

%% Light IFR Operation
contents(#orber_light_ifr_ref{data = #lightdata{id = _Id}}, 
	 _Limit_type, _Exclude_inherited) ->
    [];
contents(Objref,Limit_type,Exclude_inherited) ->
    Mod = obj2mod(Objref),
    Mod:contents(Objref,Limit_type,Exclude_inherited).

lookup_name(Objref,Search_name,Levels_to_search,Limit_type,Exclude_inherited) ->
    Mod = obj2mod(Objref),
    Mod:lookup_name(Objref,Search_name,Levels_to_search,Limit_type,Exclude_inherited).


describe_contents(Objref,Limit_type,Exclude_inherited,Max_returned_objs) ->
    Mod = obj2mod(Objref),
    Mod:describe_contents(Objref,Limit_type,Exclude_inherited,Max_returned_objs).

create_module(Objref,Id,Name,Version) ->
    Mod = obj2mod(Objref),
    Mod:create_module(Objref,Id,Name,Version).

create_constant(Objref,Id,Name,Version,Type,Value) ->
    Mod = obj2mod(Objref),
    Mod:create_constant(Objref,Id,Name,Version,Type,Value).

create_struct(Objref,Id,Name,Version,Members) ->
    Mod = obj2mod(Objref),
    Mod:create_struct(Objref,Id,Name,Version,Members).

create_union(Objref,Id,Name,Version,Discriminator_type,Members) ->
    Mod = obj2mod(Objref),
    Mod:create_union(Objref,Id,Name,Version,Discriminator_type,Members).

create_enum(Objref,Id,Name,Version,Members) ->
    Mod = obj2mod(Objref),
    Mod:create_enum(Objref,Id,Name,Version,Members).

create_alias(Objref,Id,Name,Version,Original_type) ->
    Mod = obj2mod(Objref),
    Mod:create_alias(Objref,Id,Name,Version,Original_type).

create_interface(Objref,Id,Name,Version,Base_interfaces) ->
    Mod = obj2mod(Objref),
    Mod:create_interface(Objref,Id,Name,Version,Base_interfaces).

create_exception(Objref,Id,Name,Version,Members) ->
    Mod = obj2mod(Objref),
    Mod:create_exception(Objref,Id,Name,Version,Members).

%%%---------------------------------------------------------------
%%% 

get_type(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_type'(Objref).

%%%---------------------------------------------------------------
%%% 

%% This list should contain the data in most-likely-to-be-accessed-order. 
-define(INDEXED_TABLE_LIST, [{ir_ExceptionDef, #ir_ExceptionDef.id},
			     {ir_InterfaceDef, #ir_InterfaceDef.id},
			     {ir_ModuleDef, #ir_ModuleDef.id},
			     {ir_StructDef, #ir_StructDef.id},
			     {ir_UnionDef, #ir_UnionDef.id},
			     {ir_AliasDef, #ir_AliasDef.id},
			     {ir_TypedefDef, #ir_TypedefDef.id},
			     {ir_ConstantDef, #ir_ConstantDef.id},
			     {ir_EnumDef, #ir_EnumDef.id},
			     {ir_AttributeDef, #ir_AttributeDef.id},
			     {ir_Contained, #ir_Contained.id},
			     {ir_OperationDef, #ir_OperationDef.id}]).


lookup_id(#orber_light_ifr_ref{}, Id) ->
    case mnesia:dirty_read(orber_light_ifr, Id) of
	[] ->
	    [];
	[#orber_light_ifr{module = Mod}] ->
	    #orber_light_ifr_ref{data = #lightdata{scope = atom_to_list(Mod), 
						   id = Id}}
    end;
lookup_id(_Objref,Id) ->
    %% We used the operation below before but it's very expensive.
    %% orber_ifr_repository:lookup_id(Objref,Id)
    lookup_id_helper(?INDEXED_TABLE_LIST, Id).

lookup_id_helper([], _) ->
    [];
lookup_id_helper([{Tab, IdNum}|T], Id) ->
    case mnesia:dirty_index_read(Tab, Id, IdNum) of
	[] ->
	    lookup_id_helper(T, Id);
	[FoundIt] ->
	    {Tab, element(2, FoundIt)}
    end.

get_primitive(Objref,Kind) ->
    orber_ifr_repository:get_primitive(Objref,Kind).

create_string(Objref,Bound) ->
    orber_ifr_repository:create_string(Objref,Bound).

create_wstring(Objref,Bound) ->
    orber_ifr_repository:create_wstring(Objref,Bound).

create_sequence(Objref,Bound,Element_type) ->
    orber_ifr_repository:create_sequence(Objref,Bound,Element_type).

create_array(Objref,Length,Element_type) ->
    orber_ifr_repository:create_array(Objref,Length,Element_type).

create_idltype(Objref,Typecode) ->		%not in CORBA 2.0
    orber_ifr_repository:create_idltype(Objref,Typecode).

create_fixed(Objref, Digits, Scale) ->
    orber_ifr_repository:create_fixed(Objref, Digits, Scale).

%%%---------------------------------------------------------------
%%% 

get_type_def(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_type_def'(Objref).
	     
set_type_def(Objref,TypeDef) ->
    Mod = obj2mod(Objref),
    Mod:'_set_type_def'(Objref,TypeDef).
	     
get_value(Objref) ->
    orber_ifr_constantdef:'_get_value'(Objref).

set_value(Objref,Value) ->
    orber_ifr_constantdef: '_set_value'(Objref,Value).

%%%---------------------------------------------------------------
%%% 

get_members(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_members'(Objref).
	     
set_members(Objref,Members) ->
    Mod = obj2mod(Objref),
    Mod:'_set_members'(Objref,Members).
	     
%%%---------------------------------------------------------------
%%% 

get_discriminator_type(Objref) ->
    orber_ifr_uniondef:'_get_discriminator_type'(Objref).

get_discriminator_type_def(Objref) ->
    orber_ifr_uniondef:'_get_discriminator_type_def'(Objref).

set_discriminator_type_def(Objref,TypeDef) ->
    orber_ifr_uniondef:'_set_discriminator_type_def'(Objref,TypeDef).

%%%---------------------------------------------------------------
%%% 

get_original_type_def(Objref) ->
    orber_ifr_aliasdef:'_get_original_type_def'(Objref).

set_original_type_def(Objref,TypeDef) ->
    orber_ifr_aliasdef:'_set_original_type_def'(Objref,TypeDef).

%%%---------------------------------------------------------------
%%% 

get_kind(Objref) ->
    orber_ifr_primitivedef:'_get_kind'(Objref).

%%%---------------------------------------------------------------
%%% 

get_bound(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_bound'(Objref).

set_bound(Objref,Bound) ->
    Mod = obj2mod(Objref),
    Mod:'_set_bound'(Objref,Bound).

%%%---------------------------------------------------------------
%%% 

get_element_type(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_element_type'(Objref).

get_element_type_def(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_element_type_def'(Objref).

set_element_type_def(Objref,TypeDef) ->
    Mod = obj2mod(Objref),
    Mod:'_set_element_type_def'(Objref,TypeDef).

%%%---------------------------------------------------------------
%%% 

get_length(Objref) ->
    orber_ifr_arraydef:'_get_length'(Objref).

set_length(Objref,Length) ->
    orber_ifr_arraydef:'_set_length'(Objref,Length).

%%%---------------------------------------------------------------
%%% 

get_mode(Objref) ->
    Mod = obj2mod(Objref),
    Mod:'_get_mode'(Objref).

set_mode(Objref,Mode) ->
    Mod = obj2mod(Objref),
    Mod:'_set_mode'(Objref,Mode).	     

%%%---------------------------------------------------------------
%%% 

get_result(Objref) ->
    orber_ifr_operationdef:'_get_result'(Objref).

get_result_def(Objref) ->
    orber_ifr_operationdef:'_get_result_def'(Objref).

set_result_def(Objref,ResultDef) ->
    orber_ifr_operationdef:'_set_result_def'(Objref,ResultDef).

get_params(Objref) ->
    orber_ifr_operationdef:'_get_params'(Objref).

set_params(Objref,Params) ->
    orber_ifr_operationdef:'_set_params'(Objref,Params).

get_contexts(Objref) ->
    orber_ifr_operationdef:'_get_contexts'(Objref).

set_contexts(Objref,Contexts) ->
    orber_ifr_operationdef:'_set_contexts'(Objref,Contexts).

get_exceptions(Objref) ->
    orber_ifr_operationdef:'_get_exceptions'(Objref).

set_exceptions(Objref,Exceptions) ->
    orber_ifr_operationdef:'_set_exceptions'(Objref,Exceptions).

%%%---------------------------------------------------------------
%%% 

get_base_interfaces(Objref) ->
    orber_ifr_interfacedef:'_get_base_interfaces'(Objref).

set_base_interfaces(Objref,BaseInterfaces) ->
    orber_ifr_interfacedef:'_set_base_interfaces'(Objref,BaseInterfaces).

is_a(Objref,Interface_id) ->
    orber_ifr_interfacedef:is_a(Objref,Interface_id).

describe_interface(Objref) ->
    orber_ifr_interfacedef:describe_interface(Objref).

create_attribute(Objref,Id,Name,Version,Type,Mode) ->
    orber_ifr_interfacedef:create_attribute(Objref,Id,Name,Version,Type,Mode).

create_operation(Objref,Id,Name,Version,Result,Mode,Params,Exceptions,Contexts) ->
    orber_ifr_interfacedef:create_operation(Objref,Id,Name,Version,Result,Mode,
					    Params,Exceptions,Contexts).

obj2mod({ir_IRObject, _}) ->
    orber_ifr_irobject;
obj2mod({ir_Contained, _}) ->
    orber_ifr_contained;
obj2mod({ir_Container, _}) ->
    orber_ifr_container;
obj2mod({ir_IDLType, _}) ->
    orber_ifr_idltype;
obj2mod({ir_Repository, _}) ->
    orber_ifr_repository;
obj2mod({ir_ModuleDef, _}) ->
    orber_ifr_moduledef;
obj2mod({ir_ConstantDef, _}) ->
    orber_ifr_constantdef;
obj2mod({ir_TypedefDef, _}) ->
    orber_ifr_typedef;
obj2mod({ir_StructDef, _}) ->
    orber_ifr_structdef;
obj2mod({ir_UnionDef, _}) ->
    orber_ifr_uniondef;
obj2mod({ir_EnumDef, _}) ->
    orber_ifr_enumdef;
obj2mod({ir_AliasDef, _}) ->
    orber_ifr_aliasdef;
obj2mod({ir_PrimitiveDef, _}) ->
    orber_ifr_primitivedef;
obj2mod({ir_StringDef, _}) ->
    orber_ifr_stringdef;
obj2mod({ir_WstringDef, _}) ->
    orber_ifr_wstringdef;
obj2mod({ir_SequenceDef, _}) ->
    orber_ifr_sequencedef;
obj2mod({ir_ArrayDef, _}) ->
    orber_ifr_arraydef;
obj2mod({ir_ExceptionDef, _}) ->
    orber_ifr_exceptiondef;
obj2mod({ir_AttributeDef, _}) ->
    orber_ifr_attributedef;
obj2mod({ir_OperationDef, _}) ->
    orber_ifr_operationdef;
obj2mod({ir_InterfaceDef, _}) ->
    orber_ifr_interfacedef;
obj2mod({ir_FixedDef, _}) ->
    orber_ifr_fidxeddef;
obj2mod(Obj) ->
    orber:dbg("[~p] orber_ifr:obj2mod(~p); unknown.", 
	      [?LINE, Obj], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).


