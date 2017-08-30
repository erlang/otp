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
%% File    : orber_ifr_irobject.erl
%% Purpose : Code for IRObject
%%----------------------------------------------------------------------

-module(orber_ifr_irobject).

-export(['_get_def_kind'/1,
	 destroy/1
	 ]).

-import(orber_ifr_utils,[get_field/2]).

-include("orber_ifr.hrl").
-include_lib("orber/include/corba.hrl").

%%%======================================================================
%%% IRObject

'_get_def_kind'({ObjType,ObjID}) ->
    get_field({ObjType,ObjID},def_kind).

%%% Note, that the destroy function is meant to be called within a
%%% transaction called in the destroy function of an object which
%%% inherits from IRObject. An IRObject should only be destroyed by
%%% destroying the object that inherits from an IRObject. An attempt
%%% to call this function in user code will result in unpredictable
%%% results.

%%% Don't type check the object reference. We need to be able to
%%% handle several types of objects that inherit from IRObject.

destroy(L) when is_list(L) ->
    destroy2(lists:reverse(L)).

destroy2([Things_HD | Things_TL]) ->
    destroy2(Things_HD),
    destroy2(Things_TL);

destroy2([]) -> 
    ok;
destroy2(F) when is_function(F) ->
    F();
destroy2(Thing) when is_tuple(Thing) ->
    mnesia:delete(Thing),
    ok;
destroy2(Thing) ->
    orber:dbg("[~p] ~p:destroy2(~p);~n"
	      "Strange argument for destroy.~n", 
	      [?LINE, ?MODULE, Thing], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

