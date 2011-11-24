%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%% uncomment this if tracing of handshake etc is wanted
%%-define(dist_trace, true). 
%%-define(dist_debug, true).


-ifdef(dist_debug).
-define(debug(Term), erlang:display(Term)).
-else.
-define(debug(Term), ok).
-endif.

-ifdef(dist_trace).
-define(trace(Fmt,Args), io:format("~p ~p:~s",[erlang:now(),node(),lists:flatten(io_lib:format(Fmt, Args))])).
% Use the one below for config-file (early boot) connection tracing
%-define(trace(Fmt,Args), erlang:display([erlang:now(),node(),lists:flatten(io_lib:format(Fmt, Args))])).
-define(trace_factor,8).
-else.
-define(trace(Fmt,Args), ok).
-define(trace_factor,1).
-endif.

-define(shutdown(Data), dist_util:shutdown(?MODULE, ?LINE, Data)).
-define(shutdown2(Data, Reason), dist_util:shutdown(?MODULE, ?LINE, Data, Reason)).

%% Handshake state structure
-record(hs_data, {
	  kernel_pid,        %% Pid of net_kernel
	  other_node,        %% Name of peer
	  this_node,         %% my nodename
	  socket,            %% The connection "socket"
	  timer,             %% The setup timer 
	                     %% (stream_dist_handshake:start_timer)
	  this_flags,        %% Flags my node should use
	  allowed,           %% Allowed nodes list
	  other_version,     %% The other nodes distribution version
	  other_flags,       %% The other nodes flags.
	  other_started,     %% True if the other node initiated.
	  f_send,            %% Fun that behaves like gen_tcp:send
	  f_recv,            %% Fun that behaves like gen_tcp:recv
	  f_setopts_pre_nodeup,  %% Sets "socket" options before
	                         %% nodeup is delivered to net_kernel
	  f_setopts_post_nodeup, %% Sets "socket" options after
	                         %% nodeup is delivered
	  f_getll,               %% Get low level port or pid.
	  f_address,         %% The address of the "socket", 
	                     %% generated from Socket,Node
	  %% These two are used in the tick loop,
	  %% so they are not fun's to avoid holding old code.
	  mf_tick,           %% Takes the socket as parameters and
	                     %% sends a tick, this is no fun, it
	                     %% is a tuple {M,F}.
	                     %% Is should place {tcp_closed, Socket}
	                     %% in the message queue on failure.
	  mf_getstat,        %% Returns 
			     %% {ok, RecvCnt, SendCnt, SendPend} for
	                     %% a given socket. This is a {M,F}, 
	                     %% returning {error, Reason on failure}
	  request_type = normal
}).
	  

%% The following should be filled in upon enter of... 
%% - handshake_we_started:
%% kernel_pid, other_node, this_node, socket, timer, 
%% this_flags, other_version, All fun's/mf's.
%% - handshake_other_started:
%% kernel_pid, this_node, socket, timer, 
%% this_flags, allowed, All fun's/mf's.

