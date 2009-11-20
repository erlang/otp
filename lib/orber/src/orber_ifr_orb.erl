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
%% File    : orber_ifr_orb.erl
%% Purpose : Code for Orb
%%----------------------------------------------------------------------

-module(orber_ifr_orb).

-export([create_struct_tc/3,
	 create_union_tc/4,
	 create_enum_tc/3,
	 create_alias_tc/3,
	 create_exception_tc/3,
	 create_interface_tc/2,
	 create_string_tc/1,
	 create_wstring_tc/1,
	 create_sequence_tc/2,
	 create_recursive_sequence_tc/2,
	 create_array_tc/2
	]).


-include("orber_ifr.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%%======================================================================
%%% ORB

%%%----------------------------------------------------------------------
%%% Inherited interfaces

%% none %%

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

create_struct_tc(Id, Name, Members) ->
    {tk_struct,Id,Name,lists:map(fun(#structmember{name=MemName,type=Type}) ->
					 {MemName,Type} end,
				 Members)}.

create_union_tc(Id, Name, Discriminator_type, Members) ->
    {tk_union, Id, Name, Discriminator_type, -1, % *** is -1 correct???
     lists:map(fun(#unionmember{name=MemName, label=Label, type=Type}) ->
		       {Label,MemName,Type} end,
	       Members)}.

create_enum_tc(Id, Name, Members) ->
    {tk_enum, Id, Name, Members}.

create_alias_tc(Id, Name, Original_type) ->
    {tk_alias, Id, Name, orber_ifr_utils:get_field(Original_type,type)}.

create_exception_tc(Id, Name, Members) ->
    {tk_except,Id,Name,lists:map(fun(#structmember{name=MemName,type=Type}) ->
					 {MemName,Type} end,
				 Members)}.

create_interface_tc(Id, Name) ->
    {tk_objref, Id, Name}.

create_string_tc(Bound) ->
    {tk_string, Bound}.

create_wstring_tc(Bound) ->
    {tk_wstring, Bound}.

create_sequence_tc(Bound, Element_type) ->
    {tk_sequence,Element_type,Bound}.

create_recursive_sequence_tc(Bound, Offset) ->
    orber:dbg("[~p] ~p:create_recursive_sequence_tc(~p, ~p);~n"
	      "Create_recursive_sequence is not implemented.~n", 
	      [?LINE, ?MODULE, Bound, Offset], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

create_array_tc(Length, Element_type) ->
    {tk_array, Element_type, Length}.
