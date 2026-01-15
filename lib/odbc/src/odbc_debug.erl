%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : odbc_debug.erl
%%% Author  : Ingela Anderton Andin <ingela@erix.ericsson.se>
%%% Description : Issuse standard tracing on an odbc connection process
%%%
%%% Created : 12 Dec 2003 by Ingela Anderton Andin <ingela@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(odbc_debug).

-export([trace_odbc/2]).

%%%========================================================================
%%% Debug functions
%%%========================================================================

%%--------------------------------------------------------------------------
%% trace_odbc(Process, OnOff, <Level>) -> ok
%%	Process  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off
%%      Level   - exported | all
%% Description: Turns on tracing of messages sent and received by
%%              the server <Process> and tracing on all, or all exported 
%%              functions, according to level <Level>, in this module.
%%              Result will be printed on stdout.
%%--------------------------------------------------------------------------
trace_odbc(Process, OnOff) ->
    trace_odbc(Process, OnOff, exported).

trace_odbc(Process, on, exported) ->
    dbg:tracer(),
    dbg:tp(odbc, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok; 

trace_odbc(Process, on, all) ->
    dbg:tracer(),
    dbg:tpl(odbc, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok;

trace_odbc(_Process, off, _Level) ->
    dbg:stop(),
    ok.
