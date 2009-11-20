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
%%----------------------------------------------------------------------
%% Purpose: Definition of public data structures
%%----------------------------------------------------------------------

-record(event,
	{detail_level, % Noise has a high level as opposed to essentials
	 trace_ts,     % Time when the trace was generated.
		       % Same as event_ts if omitted in trace data.
	 event_ts,     % Time when the event record was created.
	 from,	       % From actor, such as sender of a message.
	 to, 	       % To actor, such as receiver of message.
	 label,	       % Label intended to provide a brief event summary.
	 contents}).   % All nitty gritty details of the event.
