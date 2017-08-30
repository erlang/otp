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
%% File    : orber_ifr_typecode.erl
%% Purpose : Code for Typecode
%%----------------------------------------------------------------------

%%% NOTE: 
%%% Only make_typcode is for real here. All of the TypeCode interfaces
%%% specified in the IDL specification needs to be implemented.
%%%

-module(orber_ifr_typecode).

-export([
	 equal/2,
	 kind/1,
	 id/1,
	 name/1,
	 member_count/1,
	 member_name/2,
	 member_type/2,
	 member_label/2,
	 discriminator_type/1,
	 default_index/1,
	 '_length'/1,
	 content_type/1,
	 param_count/1,
	 parameter/2
	]).

-import(orber_ifr_utils,[get_field/2]).

-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/corba.hrl").



%%%----------------------------------------------------------------------
%%% Inherited interfaces

%% none %%

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

equal({ObjType, ObjID}, {Tc_ObjType, Tc_ObjID})
?tcheck(ir_TypeCode, ObjType) ->
    get_field({ObjType,ObjID},kind) == get_field({Tc_ObjType,Tc_ObjID},kind).

kind({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

id({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

name({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

member_count({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

member_name({ObjType, ObjID}, _Index) ->
    {ok, {ObjType, ObjID}}.

member_type({ObjType, ObjID}, _Index) ->
    {ok, {ObjType, ObjID}}.

member_label({ObjType, ObjID}, _Index) ->
    {ok, {ObjType, ObjID}}.

discriminator_type({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

default_index({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

'_length'({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

content_type({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

param_count({ObjType, ObjID}) ->
    {ok, {ObjType, ObjID}}.

parameter({ObjType, ObjID}, _Index) ->
    {ok, {ObjType, ObjID}}.
