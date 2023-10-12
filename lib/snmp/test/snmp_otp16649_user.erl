%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Utility functions for the (snmp manager) user test(s).
%%----------------------------------------------------------------------

-module(snmp_otp16649_user).

-behaviour(snmpm_user).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
        ]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

-export([
	 handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3
	]).


%%----------------------------------------------------------------------
%% User callback functions:
%%----------------------------------------------------------------------

handle_error(ReqId, Reason, UserPid) ->
    UserPid ! {handle_error, self(), ReqId, Reason},
    ignore.
 
 
handle_agent(Addr, Port, SnmpInfo, UserPid, UserData) ->
    UserPid ! {handle_agent, self(), Addr, Port, SnmpInfo, UserData},
    ignore.
 
 
handle_pdu(TargetName, ReqId, SnmpResponse, UserPid) ->
    UserPid ! {handle_pdu, self(), TargetName, ReqId, SnmpResponse},
    ignore.
 
handle_trap(TargetName, SnmpTrap, UserPid) ->
    UserPid ! {handle_trap, self(), TargetName, SnmpTrap},
    ignore.
 
handle_inform(TargetName, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), TargetName, SnmpInform},
    receive
	{handle_inform_no_response, TargetName} ->
	    no_reply;
	{handle_inform_response, TargetName} ->
	    ignore
    end.

handle_report(TargetName, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), TargetName, SnmpReport},
    ignore.


