%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: corba_nvlist.erl
%% Description:
%%    This file contains the CORBA::NVList handling 
%%
%%-----------------------------------------------------------------
-module(corba_nvlist).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% Standard interface CORBA::NVList
%%-----------------------------------------------------------------
-export([add_item/6,
	 free/1,
	 free_memory/1, 
	 get_count/1]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create_list/1,
	create_operation_list/1]).

%%------------------------------------------------------------
%% Implementation of standard interface CORBA::NVList
%%------------------------------------------------------------
add_item(List, Id, TC, Value, Len, ArgFlags) ->
    {ok, List}.

free(List) ->
    ok.

free_memory(List) ->
    ok.

get_count(List) ->
    {ok, 0}.

%%------------------------------------------------------------
%% Implementation of extra functions which creates NVList:s
%% theese ae used by the standard functions with the same name 
%% in the CORBA::ORB interface
%%------------------------------------------------------------

create_list(Count) ->
    {ok, create_list_2(Count, [])}.

create_list_2(0, Acc) ->
    Acc;
create_list_2(N, Acc) ->
   create_list_2(N-1, [[] | Acc]).

create_operation_list(OpDef) ->
    OpArgList = OpDef,
    {ok, create_operation_list_2(OpArgList, [])}.

create_operation_list_2([], Acc) ->
     Acc;
create_operation_list_2([OpArg | OpArgList], Acc) ->
    Rec = parse_oparg_def(OpArg),
    create_operation_list_2(OpArgList, [Rec | Acc]).

parse_oparg_def(OpArg) ->
    OpArg.











