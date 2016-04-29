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

-module(iiop_module_test_impl).
-include_lib("orber/include/corba.hrl").
-include("idl_output/iiop_module.hrl").


-export([send_void/1, send_short/3, send_ushort/3]).
-export([send_long/3, send_ulong/3, send_float/3]).
-export([send_double/3, send_boolean/3, send_char/3]).
-export([send_octet/3, send_any/3, send_object/3]).
-export([send_struct1/3, send_union1/3, send_enum1/3]).
-export([send_string/3, send_sequence1/3, send_array1/3]).
-export([ret_systemexception/1, ret_userexception/1]).



-export([start/0, stop/0]).
-export([init/1, terminate/2]).


init(_) ->
    {ok, []}.

terminate(Reason, _State) ->
    io:format("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.


start() ->
    SFok = corba:create('iiop_module_test', "IDL:iiop_module/test:1.0"),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "iiop_test"),
    NC2 = lname_component:set_id(lname_component:create(), "erl_test"),
    N = lname:insert_component(lname:create(), 1, NC1),
    N1 = lname:insert_component(N, 2, NC2),
    'CosNaming_NamingContext':bind(NS, N1, SFok),
    SFok.

stop() ->
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "iiop_test"),
    NC2 = lname_component:set_id(lname_component:create(), "erl_test"),
    N = lname:insert_component(lname:create(), 1, NC1),
    N1 = lname:insert_component(N, 2, NC2),
    'CosNaming_NamingContext':unbind(NS, N1).



send_void(S) ->
    {ok, S}.

send_short(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_ushort(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_long(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_ulong(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_float(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_double(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_boolean(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_char(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_octet(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_any(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_object(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_struct1(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_union1(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_enum1(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_string(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_sequence1(S, P1, P2) ->
    {{P1, P1, P2}, S}.

send_array1(S, P1, P2) ->
    {{P1, P1, P2}, S}.

ret_systemexception(S) ->
    throw(#'BAD_PARAM'{}),
    {ok, S}.

ret_userexception(S) ->
    throw(#iiop_module_Except1{why="not readable",rest_of_name=["foo", "bar"]}),
    {ok, S}.
