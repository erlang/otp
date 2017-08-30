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
%% File    : orber_ifr_typedef.erl
%% Purpose : Code for Typedef
%%----------------------------------------------------------------------

-module(orber_ifr_typedef).

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
	 '_get_type'/1
	]).


-include("orber_ifr.hrl").

%%%======================================================================
%%% TypedefDef (Contained(IRObject), IDLType(IRObject))

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType, ObjID}).

%%% Note, that the destroy function is meant to be called within a
%%% transaction called in the destroy function of an object which
%%% inherits from TypedefDef. A TypedefDef should only be destroyed by
%%% destroying the object that inherits from a TypedefDef. An attempt
%%% to call this function in user code will result in unpredictable
%%% results.

%%% Don't type check the object reference. We need to be able to
%%% handle several types of objects that inherit from TypedefDef.

destroy(TypedefDef_objref) ->
    F = fun() -> ObjList = cleanup_for_destroy(TypedefDef_objref),
		 orber_ifr_irobject:destroy([TypedefDef_objref | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy(TypedefDef_objref) ->
    orber_ifr_contained:cleanup_for_destroy(TypedefDef_objref) ++
	orber_ifr_idltype:cleanup_for_destroy(TypedefDef_objref).
    

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from Contained

'_get_id'({ObjType, ObjID}) ->
    orber_ifr_contained:'_get_id'({ObjType,ObjID}).

'_set_id'({ObjType, ObjID}, EO_Value) ->
    orber_ifr_contained:'_set_id'({ObjType,ObjID},EO_Value).

'_get_name'({ObjType, ObjID}) ->
    orber_ifr_contained:'_get_name'({ObjType,ObjID}).

'_set_name'({ObjType, ObjID}, EO_Value) ->
    orber_ifr_contained:'_set_name'({ObjType,ObjID}, EO_Value).

'_get_version'({ObjType, ObjID}) ->
    orber_ifr_contained:'_get_version'({ObjType,ObjID}).

'_set_version'({ObjType, ObjID}, EO_Value) ->
    orber_ifr_contained:'_set_version'({ObjType,ObjID},EO_Value).

'_get_defined_in'({ObjType,ObjID}) ->
    orber_ifr_contained:'_get_defined_in'({ObjType,ObjID}).

'_get_absolute_name'({ObjType, ObjID}) ->
    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}).

'_get_containing_repository'({ObjType,ObjID}) ->
    orber_ifr_contained:'_get_containing_repository'({ObjType,ObjID}).

describe({ObjType, ObjID}) ->
    orber_ifr_contained:describe({ObjType,ObjID}).

move({ObjType, ObjID}, New_container, New_name, New_version) ->
    orber_ifr_contained:move({ObjType,ObjID},New_container,New_name,
			     New_version).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

%%% none %%
