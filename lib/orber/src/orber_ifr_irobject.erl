%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

