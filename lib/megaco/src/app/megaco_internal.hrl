%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Define internal data structures and error codes
%%----------------------------------------------------------------------

-define(APPLICATION, megaco).

%% -define(debug, true).

%% N.B. Update megaco_config when a new field is added
-record(conn_data,
	{
	  conn_handle, 
	  serial,
	  max_serial,
	  request_timer,
	  long_request_timer,

	  %% Auto send of ack: false | true 
	  %% (if true, and if trans_ack is false or trans_timer 
	  %% is zero (0), then acks will be sent immediatly)
	  auto_ack,

	  %% ------
	  %% Accumulate trans acks/requests and send them "all" later
	  %% in one bigger message.
	  %% For this to take effekt, trans_timer has to be > 0
	  %% trans_ack and/or trans_req has to be true.
	  %% Accumulate transactions, and send them later, either 
	  %% when the timer expires, when maxcount number of
	  %% transactions has been accumulated or in the case
	  %% requests, when the maxsize number of bytes has been
	  %% accumulated (whichever happens first). 
	  %% (Note that, for acks, this is only valid if auto_ack 
	  %% is true)

	  trans_ack,            % false
	  trans_ack_maxcount,   % 10

	  trans_req,            % false   
	  trans_req_maxcount,   % 10
	  trans_req_maxsize,    % 2048

	  trans_timer,          % 0 (don't accumulate transactions)
	  trans_sender,         % The trans sender process ref, or undefined

	  pending_timer, 
	  
	  %% ------
	  %% These counter's are used for the MGCOriginatedPendingLimit
	  %% and MGOriginatedPendingLimit counters (of the root package).
	  %% If the user is an MGC, then 
	  %%   sent_pending_limit - represent MGCOriginatedPendingLimit
	  %%   recv_pending_limit - represent MGOriginatedPendingLimit
	  %% If the user is an MG, then 
	  %%   sent_pending_limit - represent MGOriginatedPendingLimit
	  %%   recv_pending_limit - represent MGCOriginatedPendingLimit
	  sent_pending_limit,  % infinity | integer() > 0
	  recv_pending_limit,  % infinity | integer() > 0

	  reply_timer, 
	  control_pid,
	  monitor_ref,
	  send_mod,
	  send_handle,
	  encoding_mod,
	  encoding_config,
	  protocol_version,
	  auth_data,
	  user_mod,
	  user_args,
	  reply_action,          % call | cast
	  reply_data,            % term()
	  threaded,              % boolean(), false
	  strict_version,        % boolean(), true
	  long_request_resend,   % boolean(), false
	  call_proxy_gc_timeout, % integer() > 0

	  %% This flag is used when a connection is being cancelled.
	  %% The purpuse is to avoid raise conditions with replies
	  %% during the cancellation. 
	  cancel,              % boolean(), false
	  resend_indication,   % boolean(), false

	  %% -------
	  %% Defined in the Segmentation Package (extends the root package)
	  %% 
	  segment_reply_ind,  % boolean(), false
	  segment_recv_acc,   % bool()
	  segment_recv_timer, % megaco_timer() | integer() > 0 | infinity
	  segment_send,       % none | infinity | integer() > 0
	  segment_send_timer, % megaco_timer() | integer() > 0 | infinity
	  max_pdu_size,       % infinity | integer() > 0

          request_keep_alive_timeout % plain | integer() >= 0
	 }).


%% N.B. Update megaco_config when a new field is added
-record(remote_conn_data,
	{conn_handle,
	 user_node,
	 monitor_ref}).


%%%----------------------------------------------------------------------
%%% Error/warning/info message macro(s)
%%%----------------------------------------------------------------------

-define(megaco_info(F, A),
	(catch error_logger:info_msg("[ ~w : ~w : ~p ] ~n" ++ F ++ "~n", 
				     [?APPLICATION, ?MODULE, self()|A]))).

-define(megaco_warning(F, A),
	(catch error_logger:warning_msg("[ ~w : ~w : ~p ] ~n" ++ F ++ "~n", 
					[?APPLICATION, ?MODULE, self()|A]))).

-define(megaco_error(F, A),
	(catch error_logger:error_msg("[ ~w : ~w : ~p ] ~n" ++ F ++ "~n", 
				      [?APPLICATION, ?MODULE, self()|A]))).


-define(megaco_ereport(Label, Report),
	?megaco_report(error_report, Label, Report)).

-define(megaco_wreport(Label, Report),
	?megaco_report(warning_report, Label, Report)).

-define(megaco_ireport(Label, Report), 
	?megaco_report(info_report, Label, Report)).

-define(megaco_report(Func, Label, Report), 
	(catch error_logger:Func([{label,       Label}, 
				  {application, ?APPLICATION}, 
				  {module,      ?MODULE}, 
				  {process,     self()} | Report]))).


%%%----------------------------------------------------------------------
%%% Default (ignore) value of the Extra argument to the 
%%% megaco:receive_message/5 and process_received_message functions/5.
%%%----------------------------------------------------------------------

-define(default_user_callback_extra, ignore_extra).


%%%----------------------------------------------------------------------
%%% Event Trace
%%%----------------------------------------------------------------------

-ifdef(megaco_trace_io).
-define(report(Level, C, Label, Contents), 
	io:format("*** [~s] ~p ~p *** "
		  "~n   ~p[~p] " ++ Label ++ 
		  "~n   ~p"
		  "~n   ~p"
		  "~n", 
		  [megaco:format_timestamp(now()), 
		   self(), Level, ?MODULE, ?LINE, C, Contents])).
-else.
-define(report(Level, C, Label, Contents),
	megaco:report_event(Level, ?APPLICATION, Label,
			    [{line, ?MODULE, ?LINE}, C | Contents])).
-endif.

-define(report_important(C, Label, Contents), ?report(20, C, Label, Contents)).
-define(report_verbose(  C, Label, Contents), ?report(40, C, Label, Contents)).
-define(report_debug(    C, Label, Contents), ?report(60, C, Label, Contents)).
-define(report_trace(    C, Label, Contents), ?report(80, C, Label, Contents)).


%%%----------------------------------------------------------------------
%%% Debug
%%%----------------------------------------------------------------------

-ifdef(megaco_debug).
-define(d(F,A), io:format("~w: " ++ F ++ "~n",[?MODULE|A])).
-else.
-define(d(F,A), ok).
-endif.
