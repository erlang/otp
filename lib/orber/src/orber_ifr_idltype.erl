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
%% File    : orber_ifr_idltype.erl
%% Purpose : Code for Idltype
%%----------------------------------------------------------------------

-module(orber_ifr_idltype).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 '_get_type'/1,
	 '_get_type_def'/1
	]).

-import(orber_ifr_utils,[get_field/2]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%%======================================================================
%%% IDLType (IRObject)

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType, ObjID}) ?tcheck(ir_IDLType, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType, ObjID}).

%%% Don't type check the object reference. We need to be able to
%%% handle several types of objects that inherit from IDLType.

destroy(IDLType_objref) ->
    F = fun() -> ObjList = cleanup_for_destroy(IDLType_objref),
		 orber_ifr_irobject:destroy(ObjList)
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy(IDLType_objref) ->
    [IDLType_objref].

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

%% What is this ? You cannot check this for ir_IDLType here !
%% ( an object type cannot be both .... ) 
%%'_get_type'({ObjType,ObjID}) ?tcheck(ir_IDLType, ObjType) ->
%%    get_field({ObjType,ObjID},type).


'_get_type'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},type).

'_get_type_def'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},type_def).
