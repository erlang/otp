%%% #0.    BASIC INFORMATION
%%% ----------------------------------------------------------
%%% %CCaseFile:	gcp.hrl %
%%% Author:      EAB/UPD/AV
%%% Description: Internal include file.
%%% ----------------------------------------------------------
-hrl_id('9/190 55-CNA 113 033 Ux').
-hrl_vsn('/main/R1A/21').
-hrl_date('2005-05-31').
-hrl_author('uabasve').
%%% %CCaseTemplateFile:	module.hrl %
%%% %CCaseTemplateId: 17/002 01-FEA 202 714 Ux, Rev: /main/4 %
%%%
%%% Copyright (C) 2000-2005 by Ericsson Telecom AB
%%% SE-126 25  STOCKHOLM
%%% SWEDEN, tel int + 46 8 719 0000
%%%
%%% The program may be used and/or copied only with the written
%%% permission from Ericsson Telecom AB, or in accordance with
%%% the terms and conditions stipulated in the agreement/contract
%%% under which the program has been supplied.
%%%
%%% All rights reserved
%%%
%%% ----------------------------------------------------------
%%% #1.    REVISION LOG
%%% ----------------------------------------------------------
%%% Rev        Date       Name        What
%%% -----      -------    --------    ------------------------
%%% R1A/1      05-02-07   uabasve    Copied from EAS R7A/9
%%% R1A/2      05-02-08   ejojmjn    Removed SAAL
%%% R1A/3-     05-03-18   uabasve    Clean.
%%% ----------------------------------------------------------
%%%
%%% #2.    CODE
%%% #---------------------------------------------------------
%%% #2.1   DEFINITION OF CONSTANTS
%%% #---------------------------------------------------------

%% Keys into gcpVariables for various options/values.
-define(TRAFFIC_DESCRIPTOR_KEY, traffic_descriptor).

%% H.248 version at link creation.
-define(INITIAL_H248_VERSION, 1).

%% Exceptions for use within a module. ?MODULE is just extra protection
%% against catching something unexpected.
-define(THROW(Reason), throw({error, ?MODULE, ?LINE, Reason})).
-define(CATCH(Expr),   try Expr
                       catch throw: ?FAILURE(Reason) -> {error, Reason}
                       end).
-define(FAILURE(T), {error, ?MODULE, _, T}).

%% The SendHandle used by a GCP transport process must be a tuple
%% of length >= 2 whose first two elements are the pid of the
%% transport process and index (aka #gcpLinkTable.key) of the link
%% upon which incoming data has arrived.
-define(SH_PID(SendHandle),  element(1, SendHandle)).
-define(SH_LINK(SendHandle), element(2, SendHandle)).
-define(SH_SET_PID(SendHandle, Pid), setelement(1, SendHandle, Pid)).

%% Megaco process that CH and OM servers monitor. This needs to be
%% replaced by a documented method.
-define(MEGACO_APP, megaco_config).

%% The message that gcpI:send_reply sends to the process that's waiting
%% for an action reply.
-define(ACTION_REPLY_MESSAGE(ActionReplies, Result),
        {reply, ActionReplies, Result}).

%%% #---------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%% #---------------------------------------------------------

-record(mg, {pref}).
-record(mgc, {mgid}).

%% User configuration that gets mapped into megaco user info by
%% gcpLib:make_user_info/1. GCP exposes only a subset of what's
%% possible to set in megaco.
-record(user_config,
        {reply_timer          = 30000, %% ms to wait for reply ack
         %% Incoming transactions:
         pending_timer        = 10000, %% ms until outgoing transaction pending
         sent_pending_limit   = 5,     %% nr of outgoing pendings before 506
         %% Outgoing transactions:
         recv_pending_limit   = infinity,%% nr of incoming pendings before fail
         request_timer        = 3000,  %% ms to wait for response before resend
         request_retries      = 5,     %% nr unanswered sends before fail
         long_request_timer   = 15000, %% ms to wait for reply after pending
         long_request_retries = 5}).   %% nr of pendings/timeouts before fail

%% Record passed into transport implementations at transport start.
%% Expected to be passed back to gcpTransportI.
-record(receive_handle,
        {megaco_receive_handle,  %% passed to megaco:receive_message
         receive_message}).      %% gcpLinkTable.receive_message

%%% ---------------------------------------------------------------------------
%%% # gcpRegistrationTable
%%%
%%% Record containing defined MGC's/MG's (aka megaco users).
%%% ---------------------------------------------------------------------------

-record(gcpRegistrationTable,
        {key,                    %% user reference (aka MG/MGC id)
         role,                   %% mg | mgc
         mid,                    %% H.248 mid of the MGC/MG
         version,                %% of H.248
         callback,               %% {Module, ExtraArgs}
         config = #user_config{}}).

%%% ----------------------------------------------------------
%%% # gcpLinkTable
%%% ----------------------------------------------------------

-record(gcpLinkTable,
        {key,                    %% link reference
         endpoint,               %% #mgc{} | #mg{}
         user,                   %% registration table key
         chid,                   %% call handler of transport
         admin_state,            %% up | down
         op_state,               %% up | down | pending | disabled
         restart = auto,         %% auto | user
         encoding_mod,           %% module implementing megaco_encoder
         encoding_config,        %% as passed to encoding_mod
         transport_start,        %% {M,F,ExtraArgs} for transport start
         transport_data,         %% arbitrary, passed to transport_mod
         send_message,           %% {default|sysrpc|transport|module, Module}
         receive_message,        %% local | {M,F,ExtraArgs} for decode node
         tried = false,          %% Only for links owned by a MG.
                                 %% Used to indicate that a setup attempt
                                 %% has been performed on this link.
         t95_period = 350000}).

%%% ----------------------------------------------------------
%%% # gcpActiveLinkTable
%%% ----------------------------------------------------------

-record(gcpActiveLinkTable,
        {key,                    %% {mg|mgc, MgId}
         link,                   %% link reference
         chid,                   %% CH the link is tied to
         node,                   %% node the link is on
         conn_handle,            %% record megaco_conn_handle
         send_handle,            %% {TransportPid, LinkIdx, ...}
         version = ?INITIAL_H248_VERSION}).

%%% ----------------------------------------------------------
%%% # gcpVariables
%%% ----------------------------------------------------------

-record(gcpVariables,
        {key,
         value}).

%%% ----------------------------------------------------------
%%% # gcpReplyData
%%% ----------------------------------------------------------

-record(gcpReplyData,
        {callback,               %% {Module, Args}
         mgid,
         user_data,              %% As passed by the user on send
         prio,
         timestamp}).
