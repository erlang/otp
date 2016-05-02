%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : tftp.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : Trivial FTP
%%% Created : 18 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%% 
%%% This is a complete implementation of the following IETF standards:
%%%
%%%    RFC 1350, The TFTP Protocol (revision 2).
%%%    RFC 2347, TFTP Option Extension.
%%%    RFC 2348, TFTP Blocksize Option.
%%%    RFC 2349, TFTP Timeout Interval and Transfer Size Options.
%%%
%%% The only feature that not is implemented in this release is
%%% the "netascii" transfer mode.
%%%
%%% The start/1 function starts a daemon process which, listens for
%%% UDP packets on a port. When it receives a request for read or
%%% write it spawns a temporary server process which handles the
%%% actual transfer of the file. On the client side the read_file/3
%%% and write_file/3 functions spawns a temporary client process which
%%% establishes contact with a TFTP daemon and performs the actual
%%% transfer of the file.
%%%
%%% Most of the options are common for both the client and the server
%%% side, but some of them differs a little. Here are the available
%%% options:
%%%     
%%%   {debug, Level}
%%%
%%%     Level = none | error | warning brief | normal | verbose | all
%%%     
%%%     Controls the level of debug printouts. The default is none.
%%%     
%%%   {host, Host}
%%%
%%%     The name or IP address of the host where the TFTP daemon
%%%     resides. This option is only used by the client. See
%%%     'inet' about valid host names.
%%%     
%%%   {port, Port}
%%%
%%%     Port = integer()
%%%     
%%%     The TFTP port where the daemon listens. It defaults to the
%%%     standardized number 69. On the server side it may sometimes
%%%     make sense to set it to 0, which means that the daemon just
%%%     will pick a free port (which is returned by the start/1
%%%     function).
%%%     
%%%     If a socket has somehow already has been connected, the
%%%     {udp, [{fd, integer()}]} option can be used to pass the
%%%     open file descriptor to gen_udp. This can be automated
%%%     a bit by using a command line argument stating the
%%%     prebound file descriptor number. For example, if the
%%%     Port is 69 and the file descriptor 22 has been opened by
%%%     setuid_socket_wrap. Then the command line argument
%%%     "-tftpd_69 22" will trigger the prebound file
%%%     descriptor 22 to be used instead of opening port 69.
%%%     The UDP option {udp, [{fd, 22}]} autmatically be added.
%%%     See init:get_argument/ about command line arguments and
%%%     gen_udp:open/2 about UDP options.
%%%
%%%   {port_policy, Policy}
%%%
%%%     Policy = random | Port | {range, MinPort, MaxPort}
%%%     Port = MinPort = MaxPort = integer()
%%%     
%%%     Policy for the selection of the temporary port which is used
%%%     by the server/client during the file transfer. It defaults to
%%%     'random' which is the standardized policy. With this policy a
%%%     randomized free port used. A single port or a range of ports
%%%     can be useful if the protocol should pass thru a firewall.
%%%   
%%%   {prebound_fd, InitArgFlag}
%%%
%%%     InitArgFlag = atom()
%%%
%%%     If a socket has somehow already has been connected, the
%%%     {udp, [{fd, integer()}]} option can be used to pass the
%%%     open file descriptor to gen_udp.
%%%
%%%     The prebound_fd option makes it possible to pass give the
%%%     file descriptor as a command line argument. The typical
%%%     usage is when used in conjunction with setuid_socket_wrap
%%%     to be able to open privileged sockets. For example if the
%%%     file descriptor 22 has been opened by setuid_socket_wrap
%%%     and you have choosen my_tftp_fd as init argument, the
%%%     command line should like this "erl -my_tftp_fd 22" and 
%%%     FileDesc should be set to my_tftpd_fd. This would 
%%%     automatically imply {fd, 22} to be set as UDP option.
%%%   
%%%   {udp, UdpOptions}
%%%
%%%      Options to gen_udp:open/2.
%%%
%%%   {use_tsize, Bool}
%%%
%%%     Bool = boolean()
%%%     
%%%     Flag for automated usage of the "tsize" option. With this set
%%%     to true, the write_file/3 client will determine the filesize
%%%     and send it to the server as the standardized "tsize" option.
%%%     A read_file/3 client will just acquire filesize from the
%%%     server by sending a zero "tsize".
%%%     
%%%   {max_tsize, MaxTsize}
%%%
%%%     MaxTsize = integer() | infinity
%%%     
%%%     Threshold for the maximal filesize in bytes. The transfer will
%%%     be aborted if the limit is exceeded. It defaults to
%%%     'infinity'.
%%%
%%%   {max_conn, MaxConn}
%%%   
%%%     MaxConn = integer() | infinity
%%%     
%%%     Threshold for the maximal number of active connections. The
%%%     daemon will reject the setup of new connections if the limit
%%%     is exceeded. It defaults to 'infinity'.
%%%     
%%%   {TftpKey, TftpVal}
%%%
%%%      TftpKey = string()
%%%      TftpVal = string()
%%%
%%%      The name and value of a TFTP option.
%%%      
%%%   {reject, Feature}
%%%   
%%%      Feature = Mode | TftpKey
%%%      Mode    = read | write
%%%      TftpKey = string()
%%%      
%%%      Control which features that should be rejected.
%%%      This is mostly useful for the server as it may restrict
%%%      usage of certain TFTP options or read/write access.
%%%
%%%   {callback, {RegExp, Module, State}}
%%%
%%%    	 RegExp = string()
%%%    	 Module = atom()
%%%    	 State  = term()
%%%    	 
%%%      Registration of a callback module. When a file is to be
%%%      transferred, its local filename will be matched to the
%%%      regular expressions of the registered callbacks. The first
%%%      matching callback will be used the during the transfer.The
%%%      callback module must implement the 'tftp' behaviour.
%%%
%%%      On the server side the callback interaction starts with a
%%%      call to open/5 with the registered initial callback
%%%      state. open/5 is expected to open the (virtual) file. Then
%%%      either the read/1 or write/2 functions are invoked
%%%      repeatedly, once per transfererred block. At each function
%%%      call the state returned from the previous call is
%%%      obtained. When the last block has been encountered the read/1
%%%      or write/2 functions is expected to close the (virtual)
%%%      file.and return its last state. The abort/3 function is only
%%%      used in error situations. prepare/5 is not used on the server
%%%      side.
%%%      
%%%      On the client side the callback interaction is the same, but
%%%      it starts and ends a bit differently. It starts with a call
%%%      to prepare/5 with the same arguments as open/5
%%%      takes. prepare/5 is expected to validate the TFTP options,
%%%      suggested by the user and return the subset of them that it
%%%      accepts. Then the options is sent to the server which will
%%%      perform the same TFTP option negotiation procedure. The
%%%      options that are accepted by the server is forwarded to the
%%%      open/5 function on the client side. On the client side the
%%%      open/5 function must accept all option as is or reject the
%%%      transfer. Then the callback interaction follows the same
%%%      pattern as described above for the server side. When the last
%%%      block is encountered in read/1 or write/2 the returned stated
%%%      is forwarded to the user and returned from read_file/3 or
%%%      write_file/3.
%%%-------------------------------------------------------------------

-module(tftp).

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% Public functions
-export([
	 read_file/3,
	 write_file/3,
	 start/1,
	 info/1,
	 change_config/2,
	 start/0
	]).

%% Application local functions
-export([
	 start_standalone/1,
	 start_service/1,
	 stop_service/1, 
	 services/0,
	 service_info/1
	]).


-type peer() :: {PeerType :: inet | inet6,
		 PeerHost :: inet:ip_address(),
		 PeerPort :: port()}.

-type access() :: read | write.

-type options() :: [{Key :: string(), Value :: string()}].

-type error_code() :: undef | enoent | eacces | enospc |
		      badop | eexist | baduser | badopt |
		      integer().

-callback prepare(Peer :: peer(),
		  Access :: access(),
		  Filename :: file:name(),
		  Mode :: string(),
		  SuggestedOptions :: options(),
		  InitialState :: [] | [{root_dir, string()}]) ->
    {ok, AcceptedOptions :: options(), NewState :: term()} |
    {error, {Code :: error_code(), string()}}.

-callback open(Peer :: peer(),
	       Access :: access(),
	       Filename :: file:name(),
	       Mode :: string(),
	       SuggestedOptions :: options(),
	       State :: [] | [{root_dir, string()}] | term()) ->
    {ok, AcceptedOptions :: options(), NewState :: term()} |
    {error, {Code :: error_code(), string()}}.

-callback read(State :: term()) -> {more, binary(), NewState :: term()} |
				   {last, binary(), integer()} |
				   {error, {Code :: error_code(), string()}}.

-callback write(binary(), State :: term()) ->
    {more, NewState :: term()} |
    {last, FileSize :: integer()} |
    {error, {Code :: error_code(), string()}}.

-callback abort(Code :: error_code(), string(), State :: term()) -> 'ok'.

-include("tftp.hrl").


%%-------------------------------------------------------------------
%% read_file(RemoteFilename, LocalFilename, Options) ->
%%   {ok, LastCallbackState} | {error, Reason}
%%
%% RemoteFilename     = string()
%% LocalFilename      = binary | string()
%% Options            = [option()]
%% LastCallbackState  = term()
%% Reason             = term()
%%
%% Reads a (virtual) file from a TFTP server
%%
%% If LocalFilename is the atom 'binary', tftp_binary will be used as
%% callback module. It will concatenate all transferred blocks and
%% return them as one single binary in the CallbackState.
%%
%% When LocalFilename is a string, it will be matched to the
%% registered callback modules and hopefully one of them will be
%% selected. By default, tftp_file will be used as callback module. It
%% will write each transferred block to the file named
%% LocalFilename. The number of transferred bytes will be returned as
%% LastCallbackState.
%%-------------------------------------------------------------------

read_file(RemoteFilename, LocalFilename, Options) ->
    tftp_engine:client_start(read, RemoteFilename, LocalFilename, Options).
    
%%-------------------------------------------------------------------
%% write(RemoteFilename, LocalFilename, Options) ->
%%   {ok, LastCallbackState} | {error, Reason}
%%
%% RemoteFilename    = string()
%% LocalFilename     = binary() | string()
%% Options           = [option()]
%% LastCallbackState = term()
%% Reason            = term()
%%
%% Writes a (virtual) file to a TFTP server
%% 
%% If LocalFilename is a binary, tftp_binary will be used as callback
%% module. The binary will be transferred block by block and the number
%% of transferred bytes will be returned as LastCallbackState.
%%
%% When LocalFilename is a string, it will be matched to the
%% registered callback modules and hopefully one of them will be
%% selected. By default, tftp_file will be used as callback module. It
%% will read the file named LocalFilename block by block. The number
%% of transferred bytes will be returned as LastCallbackState.
%%-------------------------------------------------------------------

write_file(RemoteFilename, LocalFilename, Options) ->
    tftp_engine:client_start(write, RemoteFilename, LocalFilename, Options).

%%-------------------------------------------------------------------
%% start(Options) -> {ok, Pid} | {error, Reason}
%% 
%% Options = [option()]
%% Pid     = pid()
%% Reason  = term()
%%
%% Starts a daemon process which listens for udp packets on a
%% port. When it receives a request for read or write it spawns
%% a temporary server process which handles the actual transfer
%% of the (virtual) file.
%%-------------------------------------------------------------------

start(Options) ->
    tftp_engine:daemon_start(Options).

%%-------------------------------------------------------------------
%% info(Pid) -> {ok, Options} | {error, Reason}
%% 
%% Options = [option()]
%% Reason  = term()
%%
%% Returns info about a tftp daemon, server or client process
%%-------------------------------------------------------------------

info(Pid) ->
    tftp_engine:info(Pid).

%%-------------------------------------------------------------------
%% change_config(Pid, Options) -> ok | {error, Reason}
%% 
%% Options = [option()]
%% Reason  = term()
%%
%% Changes config for a tftp daemon, server or client process
%% Must be used with care.
%%-------------------------------------------------------------------

change_config(Pid, Options) ->
    tftp_engine:change_config(Pid, Options).

%%-------------------------------------------------------------------
%% start() -> ok | {error, Reason}
%% 
%% Reason = term()
%%
%% Start the application
%%-------------------------------------------------------------------

start() ->
    application:start(inets).

%%-------------------------------------------------------------------
%% Inets service behavior
%%-------------------------------------------------------------------

start_standalone(Options) ->
    start(Options).

start_service(Options) ->
    tftp_sup:start_child(Options).

stop_service(Pid) ->
    tftp_sup:stop_child(Pid).

services() ->
    tftp_sup:which_children().

service_info(Pid) ->
    info(Pid).
	     
	     
       
