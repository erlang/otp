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
%% File    : orber_ifr_moduledef.erl
%% Purpose : Code for Moduledef
%%----------------------------------------------------------------------

-module(orber_ifr_moduledef).

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
	 move/4
	]).

-include("orber_ifr.hrl").

%%%======================================================================
%%% ModuleDef (Container(IRObject), Contained(IRObject))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType, ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->    
    F = fun() -> '_clean'({ObjType, ObjID}) end,
    orber_ifr_utils:ifr_transaction_write(F).

'_clean'(ObjRef) ->
    ObjList = cleanup_for_destroy(ObjRef),
    orber_ifr_irobject:destroy([ObjRef | ObjList]).
    

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:cleanup_for_destroy({ObjType,ObjID}) ++
	orber_ifr_contained:cleanup_for_destroy({ObjType,ObjID}).


%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Container

lookup({ObjType, ObjID}, Search_name) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:lookup({ObjType, ObjID}, Search_name).

contents({ObjType, ObjID}, Limit_type, Exclude_inherited)
	?tcheck(ir_ModuleDef, ObjType)->
    orber_ifr_container:contents({ObjType, ObjID},Limit_type,
				 Exclude_inherited).

lookup_name({ObjType, ObjID}, Search_name, Levels_to_search, Limit_type,
	    Exclude_inherited)
	   ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:lookup_name({ObjType,ObjID}, Search_name,
				    Levels_to_search, Limit_type,
				    Exclude_inherited).

describe_contents({ObjType, ObjID}, Limit_type, Exclude_inherited,
		  Max_returned_objs)
		 ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:describe_contents({ObjType, ObjID}, Limit_type,
					  Exclude_inherited,Max_returned_objs).

create_module({ObjType, ObjID}, Id, Name, Version)
	     ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_module({ObjType, ObjID}, Id, Name, Version).

create_constant({ObjType, ObjID}, Id, Name, Version, Type, Value)
	       ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_constant({ObjType, ObjID}, Id, Name, Version,
					Type, Value).

create_struct({ObjType, ObjID}, Id, Name, Version, Members)
	     ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_struct({ObjType,ObjID},Id,Name,Version,Members).

create_union({ObjType, ObjID}, Id, Name, Version, Discriminator_type, Members)
	    ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_union({ObjType, ObjID}, Id, Name, Version,
				     Discriminator_type, Members).

create_enum({ObjType, ObjID}, Id, Name, Version, Members)
	   ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_enum({ObjType, ObjID},Id,Name,Version,Members).

create_alias({ObjType, ObjID}, Id, Name, Version, Original_type)
	    ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_alias({ObjType, ObjID}, Id, Name, Version,
				     Original_type).

create_interface({ObjType, ObjID}, Id, Name, Version, Base_interfaces)
		?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_interface({ObjType, ObjID}, Id, Name, Version,
					 Base_interfaces).

create_exception({ObjType, ObjID}, Id, Name, Version, Members)
		?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_container:create_exception({ObjType, ObjID}, Id, Name, Version,
					 Members).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType,ObjID},EO_Value) ?tcheck(ir_ModuleDef,ObjType) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType, ObjID}) ?tcheck(ir_ModuleDef,ObjType) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version)
    ?tcheck(ir_ModuleDef, ObjType) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

%%% none %%
