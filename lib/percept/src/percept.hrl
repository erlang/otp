%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

-define(seconds(EndTs,StartTs), timer:now_diff(EndTs, StartTs)/1000000).

%%% -------------------	%%%
%%% Type definitions	%%%
%%% -------------------	%%%

-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type true_mfa() :: {atom(), atom(), byte() | list()}.
-type state() :: 'active' | 'inactive'.
-type scheduler_id() :: {'scheduler_id', non_neg_integer()}.

%%% -------------------	%%%
%%% 	Records		%%%
%%% -------------------	%%%

-record(activity, {
	timestamp 		,%:: timestamp() , 
	id 			,%:: pid() | port() | scheduler_id(), 
	state = undefined	,%:: state() | 'undefined', 
	where = undefined	,%:: true_mfa() | 'undefined', 
	runnable_count = 0	%:: non_neg_integer()
	}).

-record(information, {
	id			,%:: pid() | port(), 
	name = undefined	,%:: atom() | string() | 'undefined', 
	entry = undefined	,%:: true_mfa() | 'undefined', 
	start = undefined 	,%:: timestamp() | 'undefined',
	stop = undefined	,%:: timestamp() | 'undefined', 
	parent = undefined 	,%:: pid() | 'undefined',
	children = []		%:: [pid()]
	}).

