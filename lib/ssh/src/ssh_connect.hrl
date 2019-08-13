%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

%%% Description : SSH connection protocol 

-define(DEFAULT_PACKET_SIZE, 65536).
-define(DEFAULT_WINDOW_SIZE, 10*?DEFAULT_PACKET_SIZE).

-define(DEFAULT_TIMEOUT, 5000).
-define(MAX_PROTO_VERSION, 255).      % Max length of the hello string

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CONNECT messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------
%%% #   SSH_MSG_xxx
%%% Description: Packet types used by the connection protocol.
%%%----------------------------------------------------------------------
-define(SSH_MSG_GLOBAL_REQUEST,  80).
-define(SSH_MSG_REQUEST_SUCCESS,  81).
-define(SSH_MSG_REQUEST_FAILURE,  82).
-define(SSH_MSG_CHANNEL_OPEN,  90).
-define(SSH_MSG_CHANNEL_OPEN_CONFIRMATION,  91).
-define(SSH_MSG_CHANNEL_OPEN_FAILURE,  92).
-define(SSH_MSG_CHANNEL_WINDOW_ADJUST,  93).
-define(SSH_MSG_CHANNEL_DATA,  94).
-define(SSH_MSG_CHANNEL_EXTENDED_DATA,  95).
-define(SSH_MSG_CHANNEL_EOF,  96).
-define(SSH_MSG_CHANNEL_CLOSE,  97).
-define(SSH_MSG_CHANNEL_REQUEST,  98).
-define(SSH_MSG_CHANNEL_SUCCESS,  99).
-define(SSH_MSG_CHANNEL_FAILURE,  100).

-record(ssh_msg_global_request,
	{
	  name,
	  want_reply,
	  data %% ...
	 }).

-record(ssh_msg_request_success,
	{
	  data  %% ...
	 }).

-record(ssh_msg_request_failure,
	{
	 }).


-record(ssh_msg_channel_open,
	{
	  channel_type,
	  sender_channel,
	  initial_window_size,
	  maximum_packet_size,
	  data %% ...
	 }).

-record(ssh_msg_channel_open_confirmation,
	{
	  recipient_channel,
	  sender_channel,
	  initial_window_size,
	  maximum_packet_size,
	  data  %% ...
	 }).


%%%----------------------------------------------------------------------
%%% #   SSH_OPEN_xxx
%%% Description: Reason codes for SSH_MSG_OPEN_FAILURE packages.
%%%----------------------------------------------------------------------

-define(SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,	1).
-define(SSH_OPEN_CONNECT_FAILED,		2).
-define(SSH_OPEN_UNKNOWN_CHANNEL_TYPE,		3).
-define(SSH_OPEN_RESOURCE_SHORTAGE,		4).

-record(ssh_msg_channel_open_failure,
	{
	  recipient_channel,
	  reason,
	  description,
	  lang
	 }).

	 
-record(ssh_msg_channel_window_adjust,
	{
	  recipient_channel,
	  bytes_to_add
	 }).

-record(ssh_msg_channel_data,
	{
	  recipient_channel,
	  data
	 }).

%%%----------------------------------------------------------------------
%%% #   SSH_EXTENDED_DATA_xxx
%%% Description: Type codes for SSH_MSG_CHANNEL_EXTENDED_DATA packages
%%%----------------------------------------------------------------------
-define(SSH_EXTENDED_DATA_DEFAULT, 0).
-define(SSH_EXTENDED_DATA_STDERR,  1).

-record(ssh_msg_channel_extended_data,
	{
	  recipient_channel,
	  data_type_code,
	  data
	 }).

-record(ssh_msg_channel_eof,
	{
	  recipient_channel
	 }).

-record(ssh_msg_channel_close,
	{
	  recipient_channel
	 }).


-record(ssh_msg_channel_request,
	{
	  recipient_channel,
	  request_type,
	  want_reply,
	  data         %% ...
	 }).


-record(ssh_msg_channel_success,
	{
	  recipient_channel
	 }).


-record(ssh_msg_channel_failure,
	{
	  recipient_channel
	 }).

-define(TERMINAL_WIDTH, 80).
-define(TERMINAL_HEIGHT, 24).
-define(DEFAULT_TERMINAL, "vt100").

-define(TTY_OP_END,0).  %% Indicates end of options.
-define(VINTR,1).       %% Interrupt character; 255 if none. Similarly for the
			%% other characters. Not all of these characters are
                        %% supported on all systems.
-define(VQUIT,2).       %% The quit character (sends SIGQUIT signal on POSIX
                        %%                    systems).
-define(VERASE,3).      %% Erase the character to left of the cursor.
-define(VKILL,4).       %% Kill the current input line.
-define(VEOF,5).        %% End-of-file character (sends EOF from the terminal).
-define(VEOL,6).        %% End-of-line character in addition to carriage return
                        %% or,and). linefeed.
-define(VEOL2,7).       %% Additional end-of-line character.
-define(VSTART,8).      %% Continues paused output (normally control-Q).
-define(VSTOP,9).       %% Pauses output (normally control-S).
-define(VSUSP,10).      %% Suspends the current program.
-define(VDSUSP,11).     %% Another suspend character.
-define(VREPRINT,12).   %% Reprints the current input line.
-define(VWERASE,13).    %% Erases a word left of cursor.
-define(VLNEXT,14).     %% Enter the next character typed literally, even if it
                        %% is a special character
-define(VFLUSH,15).     %% Character to flush output.
-define(VSWTCH,16).     %% Switch to a different shell layer.
-define(VSTATUS,17).    %% Prints system status line (load, command, pid etc).
-define(VDISCARD,18).   %% Toggles the flushing of terminal output.
-define(IGNPAR,30).     %% The ignore parity flag.  The parameter SHOULD be 0 if
                        %% this flag is FALSE set, and 1 if it is TRUE.
-define(PARMRK,31).     %% Mark parity and framing errors.
-define(INPCK,32).      %% Enable checking of parity errors.
-define(ISTRIP,33).     %% Strip 8th bit off characters.
-define(INLCR,34).      %% Map NL into CR on input.
-define(IGNCR,35).      %% Ignore CR on input.
-define(ICRNL,36).      %% Map CR to NL on input.
-define(IUCLC,37).      %% Translate uppercase characters to lowercase.
-define(IXON,38).       %% Enable output flow control.
-define(IXANY,39).      %% Any char will restart after stop.
-define(IXOFF,40).      %% Enable input flow control.
-define(IMAXBEL,41).    %% Ring bell on input queue full.
-define(ISIG,50).       %% Enable signals INTR, QUIT, [D]SUSP.
-define(ICANON,51).     %% Canonicalize input lines.
-define(XCASE,52).      %% Enable input and output of uppercase characters by
                        %% preceding their lowercase equivalents with `\'.
-define(ECHO,53).       %% Enable echoing.
-define(ECHOE,54).      %% Visually erase chars.
-define(ECHOK,55).      %% Kill character discards current line.
-define(ECHONL,56).     %% Echo NL even if ECHO is off.
-define(NOFLSH,57).     %% Don't flush after interrupt.
-define(TOSTOP,58).     %% Stop background jobs from output.
-define(IEXTEN,59).     %% Enable extensions.
-define(ECHOCTL,60).    %% Echo control characters as ^(Char).
-define(ECHOKE,61).     %% Visual erase for line kill.
-define(PENDIN,62).     %% Retype pending input.
-define(OPOST,70).      %% Enable output processing.
-define(OLCUC,71).      %% Convert lowercase to uppercase.
-define(ONLCR,72).      %% Map NL to CR-NL.
-define(OCRNL,73).      %% Translate carriage return to newline (output).
-define(ONOCR,74).      %% Translate newline to carriage return-newline
                        %% (output).
-define(ONLRET,75).     %% Newline performs a carriage return (output).
-define(CS7,90).        %% 7 bit mode.
-define(CS8,91).        %% 8 bit mode.
-define(PARENB,92).     %% Parity enable.
-define(PARODD,93).     %% Odd parity, else even.

%%  Specifies the input baud rate in bits per second.
-define(TTY_OP_ISPEED,128).
%%  Specifies the output baud rate in bits per second.
-define(TTY_OP_OSPEED,129).  

-record(channel,
	{
	  type,          %% "session"
	  sys,           %% "none", "shell", "exec" "subsystem"
	  user,          %% "user" process id (default to cm user)
	  flow_control, 

	  local_id,           %% local channel id

	  recv_window_size,
	  recv_window_pending = 0, %% Sum of window size updates that has not
	                           %% yet been sent. This limits the number
	                           %% of sent update msgs.
	  recv_packet_size,
	  recv_close = false,

	  remote_id,          %% remote channel id
	  send_window_size,
	  send_packet_size,
	  sent_close = false,
	  send_buf = []
	 }).

-record(connection, {
	  requests = [], %% [{ChannelId, Pid}...] awaiting reply on request,
	  channel_cache,
	  channel_id_seed,
	  cli_spec,
	  options,
	  exec,
	  system_supervisor,
	  sub_system_supervisor,
	  connection_supervisor
	 }).
