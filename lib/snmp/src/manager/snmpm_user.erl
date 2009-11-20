%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

-module(snmpm_user).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_error,    3}, 
     {handle_agent,    5}, 
     {handle_pdu,      4},
     {handle_trap,     3},
     {handle_inform,   3},
     {handle_report,   3}];
behaviour_info(_) ->
    undefined.


%% handle_error(ReqId, Reason, UserData) -> Reply
%% ReqId       -> integer()
%% Reason      -> term()
%% UserData    -> term()     (supplied when the user register)
%% Reply       -> ignore 

%% handle_agent(Addr, Port, Type, SnmpInfo, UserData) -> Reply
%% Addr        -> term()
%% Port        -> integer()
%% Type        -> pdu | trap | inform | report
%% SnmpInfo    -> {ErrorStatus, ErrorIndex, Varbinds}
%% UserId      -> term()
%% ErrorStatus -> atom()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user register)
%% Reply       -> ignore | {register, UserId, agent_info()}
%% agent_info() -> [{agent_info_item(), agent_info_value()}]
%%                 This is the same info as in update_agent_info/4

%% handle_pdu(TargetName, ReqId, SnmpResponse, UserData) -> Reply
%% TargetName   -> target_name()
%% ReqId        -> term() (returned when calling ag(...), ...)
%% SnmpResponse -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus  -> atom()
%% ErrorIndex   -> integer()
%% Varbinds     -> [varbind()]
%% UserData     -> term()     (supplied when the user register)
%% Reply        -> ignore 

%% handle_trap(TargetName, SnmpTrapInfo, UserData) -> Reply
%% TargetName   -> target_name()
%% SnmpTrapInfo -> {Enteprise, Generic, Spec, Timestamp, Varbinds} |
%%                 {ErrorStatus, ErrorIndex, Varbinds}
%% Enteprise    -> oid()
%% Generic      -> integer() 
%% Spec         -> integer() 
%% Timestamp    -> integer() 
%% ErrorStatus  -> atom()
%% ErrorIndex   -> integer()
%% Varbinds     -> [varbind()]
%% UserData     -> term()     (supplied when the user register)
%% Reply        -> ignore | unregister | {register, UserId, agent_info()}

%% handle_inform(TargetName, SnmpInform, UserData) -> Reply
%% TargetName  -> target_name()
%% SnmpInform  -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus -> atom()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user register)
%% Reply       -> ignore | unregister | {register, UserId, agent_info()}
%%          

%% handle_report(TargetName, SnmpReport, UserData) -> Reply
%% TargetName  -> target_name()
%% SnmpReport  -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus -> integer()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user register)
%% Reply       -> ignore | unregister | {register, UserId, agent_info()}

