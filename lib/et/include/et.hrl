%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
