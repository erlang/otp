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
%%------------------------------------------------------------
%% File: corba.hrl
%% 
%% Description:
%%    Standard header file for the Orber Erlang CORBA environment
%% 
%%-----------------------------------------------------------------
-ifndef(corba_hrl).
-define(corba_hrl, true).

%%
%% Implementation repository record (not used and can therefor be changed)
%%
-record(orb_ImplDef, {node, module, typename, start=start, args=[[]], pid}).

%%
%% Any record
%%
-record(any, {typecode, value}).

%%
%% Any record
%%
-record(fixed, {digits, scale, value}).

%%
%% Service context record
%%
-record('IOP_ServiceContext', {context_id, context_data}).

%%
%% Exception recod for the resolve initial reference functions
%%
-record('InvalidName', {'OE_ID'="IDL:omg.org/CORBA/ORB/InvalidName:1.0"}).

%% Orber OMG assigned TAG's
%% Service Context IDs 0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
%% Component IDs   0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
%% ORB type IDs    0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
-define(ORBER_ORB_TYPE_1, 16#45524904).
%-define(ORBER_ORB_TYPE_2, 16#45524905).
%-define(ORBER_ORB_TYPE_3, 16#45524906).
%-define(ORBER_ORB_TYPE_4, 16#45524907).

%-define(ORBER_COMPONENT_1, 16#45524904).
%-define(ORBER_COMPONENT_2, 16#45524905).
%-define(ORBER_COMPONENT_3, 16#45524906).
%-define(ORBER_COMPONENT_4, 16#45524907).

-define(ORBER_GENERIC_CTX_ID, 16#45524904).
%-define(ORBER_SERVICE_CTX_2, 16#45524905).
%-define(ORBER_SERVICE_CTX_3, 16#45524906).
%-define(ORBER_SERVICE_CTX_4, 16#45524907).

%%
%% System exceptions
%%

%% VMCID
%% VMCID base assigned to OMG
-define(CORBA_OMGVMCID, 16#4f4d0000).

%% Orber's VMCID base - "ER\x00\x00" - "ER\x0f\xff".
%% Range 16#45520000 -> 16#45520fff
-define(ORBER_VMCID,    16#45520000).

%% Some other Vendors VMCID bases.
-define(IONA_VMCID_1,   16#4f4f0000).
-define(IONA_VMCID_2,   16#49540000).
-define(SUN_VMCID,      16#53550000).
-define(BORLAND_VMCID,  16#56420000).
-define(TAO_VMCID,      16#54410000).
-define(PRISMTECH_VMCID,16#50540000).

-define(ex_body, {'OE_ID'="", minor=?ORBER_VMCID, completion_status}).

-record('UNKNOWN', ?ex_body).
-record('BAD_PARAM', ?ex_body).
-record('NO_MEMORY', ?ex_body).
-record('IMP_LIMIT', ?ex_body).
-record('COMM_FAILURE', ?ex_body).
-record('INV_OBJREF', ?ex_body).
-record('NO_PERMISSION', ?ex_body).
-record('INTERNAL', ?ex_body).
-record('MARSHAL', ?ex_body).
-record('INITIALIZE', ?ex_body).
-record('NO_IMPLEMENT', ?ex_body).
-record('BAD_TYPECODE', ?ex_body).
-record('BAD_OPERATION', ?ex_body).
-record('NO_RESOURCES', ?ex_body).
-record('NO_RESPONSE', ?ex_body).
-record('PERSIST_STORE', ?ex_body).
-record('BAD_INV_ORDER', ?ex_body).
-record('TRANSIENT', ?ex_body).
-record('FREE_MEM', ?ex_body).
-record('INV_IDENT', ?ex_body).
-record('INV_FLAG', ?ex_body).
-record('INTF_REPOS', ?ex_body).
-record('BAD_CONTEXT', ?ex_body).
-record('OBJ_ADAPTER', ?ex_body).
-record('DATA_CONVERSION', ?ex_body).
-record('OBJECT_NOT_EXIST', ?ex_body).
-record('TRANSACTION_REQUIRED', ?ex_body).
-record('TRANSACTION_ROLLEDBACK', ?ex_body).
-record('INVALID_TRANSACTION', ?ex_body).
-record('INV_POLICY', ?ex_body).
-record('CODESET_INCOMPATIBLE', ?ex_body).
-record('REBIND', ?ex_body).
-record('TIMEOUT', ?ex_body).
-record('TRANSACTION_UNAVAILABLE', ?ex_body).
-record('TRANSACTION_MODE', ?ex_body).
-record('BAD_QOS', ?ex_body).

%% Defines for the enum exception_type (is also used for reply_status)
-define(NO_EXCEPTION, 'no_exception').
-define(USER_EXCEPTION, 'user_exception').
-define(SYSTEM_EXCEPTION, 'system_exception').

%% Defines for the enum completion_status.
-define(COMPLETED_YES, 'COMPLETED_YES').
-define(COMPLETED_NO, 'COMPLETED_NO').
-define(COMPLETED_MAYBE, 'COMPLETED_MAYBE').


-undef(ex_body).


-endif.
