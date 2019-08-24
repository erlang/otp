%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(snmpm_user_default).

-behaviour(snmpm_user).

-export([handle_error/3,
	 handle_agent/5,
	 handle_pdu/4,
	 handle_trap/3,
	 handle_inform/3,
	 handle_report/3]).

handle_error(ReqId, Reason, UserData) ->
    info("received handle_error:"
	 "~n   ReqId:    ~p"
	 "~n   Reason:   ~p"
	 "~n   UserData: ~p", [ReqId, Reason, UserData]),
    ignore.


handle_agent(Domain, Address, Type, SnmpInfo, UserData) ->
    info("received handle_agent:~n"
	 "   Domain:   ~p~n"
	 "   Address:  ~p~n"
	 "   Type:     ~p~n"
	 "   SnmpInfo: ~p~n"
	 "   UserData: ~p", [Domain, Address, Type, SnmpInfo, UserData]),
    ignore.


handle_pdu(TargetName, ReqId, SnmpResponse, UserData) ->
    info("received handle_pdu:"
	 "~n   TargetName:   ~p"
	 "~n   ReqId:        ~p"
	 "~n   SnmpResponse: ~p"
	 "~n   UserData:     ~p", 
	 [TargetName, ReqId, SnmpResponse, UserData]),
    ignore.


handle_trap(TargetName, SnmpTrap, UserData) ->
    info("received handle_trap:"
	 "~n   TargetName:   ~p"
	 "~n   SnmpTrap: ~p"
	 "~n   UserData: ~p", 
	 [TargetName, SnmpTrap, UserData]),
    ignore.


handle_inform(TargetName, SnmpInform, UserData) ->
    info("received handle_inform:"
	 "~n   TargetName:   ~p"
	 "~n   SnmpInform: ~p"
	 "~n   UserData:   ~p", 
	 [TargetName, SnmpInform, UserData]),
    no_reply.


handle_report(TargetName, SnmpReport, UserData) ->
    info("received handle_inform:"
	 "~n   TargetName:   ~p"
	 "~n   SnmpReport: ~p"
	 "~n   UserData:   ~p", 
	 [TargetName, SnmpReport, UserData]),
    ignore.


info(F, A) ->
    error_logger:info_msg("SNMPM default user callback " ++ F ++ "~n", A).
