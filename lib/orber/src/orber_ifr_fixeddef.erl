%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%% File    : orber_ifr_fixeddef.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(orber_ifr_fixeddef).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 '_get_type'/1,
	 '_get_digits'/1,
	 '_set_digits'/2,
	 '_get_scale'/1,
	 '_set_scale'/2]).

-import(orber_ifr_utils, [get_field/2,
			  set_field/3]).

-include("orber_ifr.hrl").

%%%======================================================================
%%% FixedDef (IDLType(IRObject))

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'({ObjType,ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    orber_ifr_irobject:'_get_def_kind'({ObjType,ObjID}).

destroy({ObjType, ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    F = fun() -> ObjList = cleanup_for_destroy({ObjType, ObjID}),
		 orber_ifr_irobject:destroy([{ObjType,ObjID} | ObjList])
	end,
    orber_ifr_utils:ifr_transaction_write(F).

cleanup_for_destroy({ObjType,ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    orber_ifr_idltype:cleanup_for_destroy({ObjType,ObjID}).

%%%----------------------------------------------------------------------
%%%  Interfaces inherited from IDLType

'_get_type'({ObjType, ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    orber_ifr_idltype:'_get_type'({ObjType, ObjID}).

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

'_get_digits'({ObjType, ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    get_field({ObjType,ObjID},digits).
'_get_scale'({ObjType, ObjID}) ?tcheck(ir_FixedDef, ObjType) ->
    get_field({ObjType,ObjID},scale).

'_set_digits'({ObjType, ObjID}, EO_Value)
		      ?tcheck(ir_FixedDef, ObjType) ->
    set_field({ObjType, ObjID}, digits, EO_Value).
'_set_scale'({ObjType, ObjID}, EO_Value)
		      ?tcheck(ir_FixedDef, ObjType) ->
    set_field({ObjType, ObjID}, scale, EO_Value).
