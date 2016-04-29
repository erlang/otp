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

-module(iiop_module_do_test_impl).


-export([run_all/3, run_userexception/2, run_systemexception/2]).
-export([createTestContext/0]).

-export([start/0, stop/0]).
-export([init/1, terminate/2]).


init(_) ->
    {ok, []}.

terminate(Reason, _State) ->
    io:format("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.

createTestContext() ->
    NS = corba:resolve_initial_references("NameService"),
    NC = lname_component:set_id(lname_component:create(), "iiop_test"),
    N = lname:insert_component(lname:create(), 1, NC),
    'CosNaming_NamingContext':bind_new_context(NS, N).

start() ->
    SFok = corba:create('iiop_module_do_test', "IDL:iiop_module/do_test:1.0"),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "iiop_test"),
    NC2 = lname_component:set_id(lname_component:create(), "erl_dotest"),
    N = lname:insert_component(lname:create(), 1, NC1),
    N1 = lname:insert_component(N, 2, NC2),
    'CosNaming_NamingContext':bind(NS, N1, SFok),
    SFok.

stop() ->
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "iiop_test"),
    NC2 = lname_component:set_id(lname_component:create(), "erl_dotest"),
    N = lname:insert_component(lname:create(), 1, NC1),
    N1 = lname:insert_component(N, 2, NC2),
    'CosNaming_NamingContext':unbind(NS, N1).

run_all(S, X, TL) ->
    ok = iiop_module_test:send_void(X),
    {tk_short, P1} = lists:nth(1, TL),
    {R1, IO1, O1} = iiop_module_test:send_short(X, P1, P1),
    RL1= [{tk_short, R1}],
    IOL1= [{tk_short, IO1}],
    OL1= [{tk_short, O1}],
    {tk_ushort, P2} = lists:nth(2, TL),
    {R2, IO2, O2} = iiop_module_test:send_ushort(X, P2, P2),
    RL2= [{tk_ushort, R2}|RL1],
    IOL2= [{tk_ushort, IO2}|IOL1],
    OL2= [{tk_ushort, O2}|OL1],
    {tk_long, P3} = lists:nth(3, TL),
    {R3, IO3, O3} = iiop_module_test:send_long(X, P3, P3),
    RL3= [{tk_long, R3}|RL2],
    IOL3= [{tk_long, IO3}|IOL2],
    OL3= [{tk_long, O3}|OL2],
    {tk_ulong, P4} = lists:nth(4, TL),
    {R4, IO4, O4} = iiop_module_test:send_ulong(X, P4, P4),
    RL4= [{tk_ulong, R4}|RL3],
    IOL4= [{tk_ulong, IO4}|IOL3],
    OL4= [{tk_ulong, O4}|OL3],
    {tk_float, P5} = lists:nth(5, TL),
    {R5, IO5, O5} = iiop_module_test:send_float(X, P5, P5),
    RL5= [{tk_float, R5}|RL4],
    IOL5= [{tk_float, IO5}|IOL4],
    OL5= [{tk_float, O5}|OL4],
    {tk_double, P6} = lists:nth(6, TL),
    {R6, IO6, O6} = iiop_module_test:send_double(X, P6, P6),
    RL6= [{tk_double, R6}|RL5],
    IOL6= [{tk_double, IO6}|IOL5],
    OL6= [{tk_double, O6}|OL5],
    {tk_boolean, P7} = lists:nth(7, TL),
    {R7, IO7, O7} = iiop_module_test:send_boolean(X, P7, P7),
    RL7= [{tk_boolean, R7}|RL6],
    IOL7= [{tk_boolean, IO7}|IOL6],
    OL7= [{tk_boolean, O7}|OL6],
    {tk_char, P8} = lists:nth(8, TL),
    {R8, IO8, O8} = iiop_module_test:send_char(X, P8, P8),
    RL= [{tk_char, R8} |RL7],
    IOL= [{tk_char, IO8} |IOL7],
    OL= [{tk_char, O8} |OL7],
    {{lists:reverse(RL),lists:reverse(IOL),lists:reverse(OL)}, S}.

run_systemexception(S, X) ->
    iiop_module_test:ret_systemexception(X),
    {ok, S}.

run_userexception(S, X) ->
    iiop_module_test:ret_userexception(X),
    {ok, S}.
