%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

%%
%%-----------------------------------------------------------------
%% Purpose: Define the protocol options for Megaco/H.248 IP connections
%%-----------------------------------------------------------------

%%----------------------------------------------------------------------
%% IP options
%%----------------------------------------------------------------------
-record(megaco_udp,
	{port,
	 options   = [],
	 socket,
	 receive_handle,
	 module    = megaco,
	 serialize = false  % false: Spawn a new process for each message
	}).


-define(GC_MSG_LIMIT,1000).
-define(HEAP_SIZE(S),5000 + 2*(S)).


%%----------------------------------------------------------------------
%% Event Trace
%%----------------------------------------------------------------------

-define(udp_report(Level, UdpRec, From, To, Label, Contents),
	megaco:report_event(Level, From, To, Label,
			    [{line, ?MODULE, ?LINE}, UdpRec | Contents])).

-define(udp_debug(UdpRec, Label, Contents),
	?udp_report_debug(UdpRec,
			  megaco_udp,
			  megaco_udp,
			  Label,
			  Contents)).

-define(udp_report_important(C, From, To, Label, Contents), 
	?udp_report(20, C, From, To, Label, Contents)).
-define(udp_report_verbose(C,   From, To, Label, Contents), 
	?udp_report(40, C, From, To, Label, Contents)).
-define(udp_report_debug(C,     From, To, Label, Contents), 
	?udp_report(60, C, From, To, Label, Contents)).
-define(udp_report_trace(C,     From, To, Label, Contents), 
	?udp_report(80, C, From, To, Label, Contents)).
