%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%% SCTP protocol contribution by Leonid Timochouk and Serge Aleynikov.
%% See also: $ERL_TOP/lib/kernel/AUTHORS
%%

%%
%% SCTP-related records.
%%

%% sctp_initmsg:    For creating a new association (send*) and
%%		      SCTP_OPT_INITMSG setsockopt:
-record(sctp_initmsg,
	{
	  num_ostreams,	 % 0	Use endpoint default
	  max_instreams, % 0	Use endpoint default
	  max_attempts,	 % 0	Use endpoint default
	  max_init_timeo % 0	Use endpoint default
	}).

%% sctp_sndrcvinfo: Possible "flags": Atoms, as below. Used
%% in "send*" and SCTP_OPT_DEFAULT_SEND_PARAM setsockopt:
-record(sctp_sndrcvinfo,
	{
	  stream,     % 0	Streams numbered from 0 (XXX?)
	  ssn,        % 0,	Ignored for send
	  flags,      % [unordered,
	  %%             addr_over,
	  %%             abort,
	  %%             eof]
	  ppid,       % 0,	Passed to the remote end
	  context,    % 0,	Passed to the user on error
	  timetolive, % 0,	In msec; 0 -> no expiration
	  tsn,        % 0,	Recv only: TSN of one of the chunks
	  cumtsn,     % 0,	Only for unordered recv
	  assoc_id    % 0		IMPORTANT!
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SCTP Notification Events:
%%

%% sctp_assoc_change: Possible valid "state" values include:
%%			comm_up, comm_lost, restart,
%%			shutdown_comp, cant_assoc
-record(sctp_assoc_change,
	{
	  state            = cant_assoc,
	  error            = 0,
	  outbound_streams = 0,
	  inbound_streams  = 0,
	  assoc_id	   = 0
	}).

%% sctp_paddr_change: Peer address is a list. Possible "state" values:
%%			addr_available, addr_unreachable,
%%			addr_removed,   addr_added,
%%			addr_made_prim
-record(sctp_paddr_change,
	{
	  addr	    = [0,0,0,0],
	  state	    = addr_available,
	  error     = 0,
	  assoc_id  = 0
	}).

%% sctp_remote_error: Possible "data" elements are Error Causes (Atoms
%%			(extending the info provided by "error" field).
-record(sctp_remote_error,
	{
	  error     = 0,
	  assoc_id  = 0,
	  data	    = []
	}).

%% sctp_send_failed: The "flags" is a Boolean specifying whether the
%%		       data have actually been transmitted over the wire.
%%		       "error" is similar to in #sctp_remote_error{} above.
%%                     "info" is the orig "*sndrcvinfo", and "data" is
%%		       the whole orig data chunk we attempted to send:
-record(sctp_send_failed,
	{
	  flags	    = false,
	  error     = 0,
	  info	    = #sctp_sndrcvinfo{},
	  assoc_id  = 0,
	  data	    = <<>>
	}).

%% sctp_shutdown_event: In this case, shut-down occurs on a particular
%%			  association, not on the whole socket.
-record(sctp_shutdown_event,
	{
	  assoc_id	= 0
	}).

%% sctp_adaptation_event: "adaptation_ind" is opaque user-specified data:
-record(sctp_adaptation_event,
	{
	  adaptation_ind = 0,
	  assoc_id       = 0
	}).

%% sctp_partial_delivery_event: XXX: Not clear whether it is delivered to
%%				the Sender or to the Recepient (probably the
%%				former). Currently, there is only 1 possible
%%				value for "indication":
-record(sctp_pdapi_event,
	{
	  indication = partial_delivery_aborted,
	  assoc_id   = 0
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SCTP Socket Options:
%%

-record(sctp_rtoinfo,	  	% For SCTP_OPT_RTOINFO
	{
	  assoc_id,
	  initial,  % 0
	  max,      % 0
	  min       % 0
	}).

-record(sctp_assocparams,	% For SCTP_OPT_ASSOCINFO
	{
	  assoc_id,
	  asocmaxrxt,               % 0
	  number_peer_destinations, % 0
	  peer_rwnd,                % 0
          local_rwnd,               % 0
	  cookie_life               % 0
	}).

% #sctp_initmsg{} and #sctp_sndrcvinfo{}, declared above, can also be options.

-record(sctp_prim,		% For SCTP_OPT_SET_PRIMARY_ADDR and
	{
	  assoc_id,
	  addr      % When set: {IP, Port}
	}).

-record(sctp_setpeerprim,	% For SCTP_OPT_SET_PEER_PRIMARY_ADDR
	{
	  assoc_id,
	  addr      % When set: { IP, Port}
	}).

-record(sctp_setadaptation,	% For SCTP_OPT_ADAPTATION_LAYER
	{
	  adaptation_ind % 0
	}).

-record(sctp_paddrparams,	% For SCTP_OPT_PEER_ADDR_PARAMS
	{
	  assoc_id,
	  address,    % When set: {IP, Port}
	  hbinterval, % 0
	  pathmaxrxt, % 0
	  pathmtu,    % 0
	  sackdelay,  % 0
	  flags	  % [hb_enable,     
	  %%         hb_disable
	  %%         hb_demand,       
	  %%         pmtud_enable,    
	  %%         pmtud_disable,   
	  %%         sackdelay_enable,
	  %%         sackdelay_disable]
	 }).


% SCTP events which will be subscribed by default upon opening the socket.
% NB: "data_io_event" controls delivery of #sctp_sndrcvinfo{} ancilary
% data, not events (which are normal data) in fact; it may be needed in
% order to get the AssocID of data just received:
%
-record(sctp_event_subscribe,
	{
	  data_io_event,          % true,	% Used by gen_sctp
	  association_event,      % true, 	% Used by gen_sctp
	  address_event,          % true,	% Unlikely to happen...
	  send_failure_event,     % true,	% Delivered as an ERROR
	  peer_error_event,       % true,	% Delivered as an ERROR
	  shutdown_event,         % true,	% Used by gen_sctp
	  partial_delivery_event, % true,	% Unlikely to happen...
	  adaptation_layer_event, % false	% Probably not needed...
	  authentication_event    % false       % Not implemented yet...
	}).

-record(sctp_assoc_value,	% For SCTP_OPT_DELAYED_ACK_TIME
	{
	  assoc_id,
	  assoc_value % 0
	}).



% sctp_paddrinfo and sctp_status are records for read-only options:
-record(sctp_paddrinfo,
	{
	  assoc_id,
	  address,       % When set: {IP, Port}
	  state,    % 'inactive', Or 'active'
	  cwnd,     % 0
	  srtt,     % 0,
	  rto,      % 0
	  mtu       % 0
	}).

-record(sctp_status,
	{
	  assoc_id,
	  state,               % empty,
	  % Other possible states:
	  % closed,	         cookie_wait,
	  % cookie_echoed,       established,
	  % shutdown_pending,    shutdow_sent,
	  % shutdown_received,   shutdown_ack_sent;
	  % NOT YET IMPLEMENTED:
	  % bound,	         listen
	  rwnd,                % 0
	  unackdata,           % 0,
	  penddata,            % 0,
	  instrms,             % 0,
	  outstrms,            % 0,
	  fragmentation_point, % 0,
	  primary              % When set: an #sctp_paddrinfo{} record
	}).
