%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
%% @doc Interface module for OSE messaging and process monitoring from Erlang
%%
%% For each mailbox created through {@link open/1} a OSE phantom process with
%% that name is started. Since phantom processes are used the memory footprint
%% of each mailbox is quite small.
%%
%% To receive messages you first have to subscribe to the specific message
%% numbers that you are interested in with {@link listen/2}. The messages
%% will be sent to the Erlang process that created the mailbox.
%%
%% @end
%%
-module(ose).

%%==============================================================================
%% Exported API
%%==============================================================================
-export([open/1,
         close/1,
         get_id/1,
         get_name/2,
         hunt/2,
         dehunt/2,
         attach/2,
         detach/2,
         send/4,
         send/5,
	 listen/2
	]).

%%==============================================================================
%% Types
%%==============================================================================
-opaque mailbox() :: port().
%% Mailbox handle.  Implemented as an erlang port.

-opaque mailbox_id() :: integer().
%% Mailbox ID, this is the same as the process id of an OSE process.
%% An integer.

-type message_number() :: 0..4294967295.
%% OSE Signal number

-opaque hunt_ref() :: {mailbox(),integer()}.
%% Reference from a hunt request.  This term will be included
%% in a successful hunt response.

-opaque attach_ref() :: {mailbox(),integer()}.
%% Reference from an attach request.  This term will be included
%% in the term returned when the attached mailbox disappears.

-export_type([mailbox_id/0,
	      message_number/0,
	      mailbox/0,
	      hunt_ref/0,
	      attach_ref/0]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(DRIVER_NAME, "ose_signal_drv").
-define(GET_SPID, 1).
-define(GET_NAME, 2).
-define(HUNT, 100).
-define(DEHUNT, 101).
-define(ATTACH, 102).
-define(DETACH, 103).
-define(SEND, 104).
-define(SEND_W_S, 105).
-define(LISTEN, 106).
-define(OPEN, 200).

-define(INT_32BIT(Int),(is_integer(Int) andalso (Int >= 0) andalso (Int < (1 bsl 32)))).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a mailbox with the given name and return a port that handles
%% the mailbox.
%%
%% An OSE phantom process with the given name will be created that will send any
%% messages sent through this mailbox. Any messages sent to the new OSE process
%% will automatically be converted to an Erlang message and sent to the Erlang
%% process that calls this function. See {@link listen/2} for details about the
%% format of the message sent.
%%
%% The caller gets linked to the created mailbox.
%%
%% raises: `badarg' | `system_limit'
%%
%% @see listen/2
%% @end
%%------------------------------------------------------------------------------
-spec open(Name) -> Port when
      Name :: iodata(),
      Port :: mailbox().
open(Name) ->
    try open_port({spawn_driver,?DRIVER_NAME}, [binary]) of
	Port ->
	    try port_command(Port,[?OPEN,Name]) of
		true ->
		    receive
			{ose_drv_reply,Port,{error,Error}} ->
			    close(Port),
			    erlang:error(Error,[Name]);
			{ose_drv_reply,Port,ok} ->
			    Port
		    end
	    catch
		error:badarg  -> close(Port),erlang:error(badarg,[Name])
	    end
    catch
	error:badarg  -> erlang:error(badarg,[Name])
    end.

%%------------------------------------------------------------------------------
%% @doc Close a mailbox
%%
%% This kills the OSE phantom process associated with this mailbox.
%%
%% Will also consume any ``{'EXIT',Port,_}'' message from the port that comes
%% due to the port closing when the calling process traps exits.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec close(Port) -> ok when
      Port :: mailbox().
close(Port) when is_port(Port) ->
    %% Copied from prim_inet
    case erlang:process_info(self(), trap_exit) of
	{trap_exit,true} ->
	    link(Port),
	    catch erlang:port_close(Port),
	    receive {'EXIT',Port,_} -> ok end;
	{trap_exit,false} ->
	    catch erlang:port_close(Port),
	    ok
    end;
close(NotPort) ->
    erlang:error(badarg,[NotPort]).

%%------------------------------------------------------------------------------
%% @doc Get the mailbox id for the given port.
%%
%% The mailbox id is the same as the OSE process id of the OSE phantom process
%% that this mailbox represents.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec get_id(Port) -> Pid when
      Port :: mailbox(),
      Pid :: mailbox_id().
get_id(Port) ->
    try port_control(Port, ?GET_SPID, <<>>) of
	<<Spid:32>> -> Spid
    catch error:_Error ->
	    erlang:error(badarg,[Port])
    end.

%%------------------------------------------------------------------------------
%% @doc Get the mailbox name for the given mailbox id.
%%
%% The mailbox name is the name of the OSE process with process id Pid.
%%
%% This call will fail with badarg if the underlying system does not support
%% getting the name from a process id.
%%
%% raises: `badarg'
%% @end
%%------------------------------------------------------------------------------
-spec get_name(Port, Pid) -> Name | undefined when
      Port :: mailbox(),
      Pid :: mailbox_id(),
      Name :: binary().
get_name(Port, Pid) when ?INT_32BIT(Pid) ->
    try port_control(Port, ?GET_NAME, <<Pid:32>>) of
	[] -> undefined;
	Res -> Res
    catch error:_Error ->
	    erlang:error(badarg,[Port,Pid])
    end;
get_name(Port, Pid) ->
    erlang:error(badarg,[Port,Pid]).


%%------------------------------------------------------------------------------
%% @doc Hunt for OSE process by name.
%%
%% Will send `{mailbox_up, Port, Ref, MboxId}'
%% to the calling process when the OSE process becomes available.
%%
%% Returns a reference term that can be used to cancel the hunt
%% using {@link dehunt/2}.
%%
%% raises: `badarg'
%%
%% @end
%%------------------------------------------------------------------------------
-spec hunt(Port, HuntPath) -> Ref when
      Port :: mailbox(),
      HuntPath :: iodata(),
      Ref :: hunt_ref().
hunt(Port, HuntPath) ->
    try port_command(Port, [?HUNT,HuntPath]) of
	true ->
	    receive
		{ose_drv_reply,Port,{error,Error}} ->
		    erlang:error(Error,[Port,HuntPath]);
		{ose_drv_reply,Port,Ref} ->
		    Ref
	    end
    catch error:_Error ->
	    erlang:error(badarg,[Port,HuntPath])
    end.

%%------------------------------------------------------------------------------
%% @doc Stop hunting for OSE process.
%%
%% If a message for this hunt has been sent but not received
%% by the calling process, it is removed from the message queue.
%% Note that this only works if the same process that did
%% the hunt does the dehunt.
%%
%% raises: `badarg'
%%
%% @see hunt/2
%% @end
%%------------------------------------------------------------------------------
-spec dehunt(Port, Ref) -> ok when
      Port :: mailbox(),
      Ref :: hunt_ref().
dehunt(Port, {Port,Ref}) when ?INT_32BIT(Ref) ->
    try port_command(Port, <<?DEHUNT:8, Ref:32>>) of
	true ->
	    receive
		{ose_drv_reply,Port,{error,enoent}} ->
		    %% enoent could mean that it is in the message queue
		    receive
			{mailbox_up, Port, {Port,Ref}, _} ->
			    ok
		    after 0 ->
			    ok
		    end;
		{ose_drv_reply,Port,ok} ->
		    ok
	    end
    catch error:_Error ->
	    erlang:error(badarg,[Port,{Port,Ref}])
    end;
dehunt(Port,Ref) ->
    erlang:error(badarg,[Port,Ref]).

%%------------------------------------------------------------------------------
%% @doc Attach to an OSE process.
%%
%% Will send `{mailbox_down, Port, Ref, MboxId}'
%% to the calling process if the OSE process exits.
%%
%% Returns a reference that can be used to cancel the attachment
%% using {@link detach/2}.
%%
%% raises: `badarg' | `enomem'
%%
%% @end
%%------------------------------------------------------------------------------
-spec attach(Port,Pid) -> Ref when
      Port :: mailbox(),
      Pid :: mailbox_id(),
      Ref :: attach_ref().
attach(Port, Spid) when ?INT_32BIT(Spid) ->
    try port_command(Port, <<?ATTACH:8, Spid:32>>) of
	true ->
	    receive
		{ose_drv_reply,Port,{error,Error}} ->
		    erlang:error(Error,[Port,Spid]);
		{ose_drv_reply,Port,Ref} ->
		    Ref
	    end
    catch error:_Error ->
	    erlang:error(badarg,[Port,Spid])
    end;
attach(Port,Spid) ->
    erlang:error(badarg,[Port,Spid]).


%%------------------------------------------------------------------------------
%% @doc Remove attachment to an OSE process.
%%
%% If a message for this monitor has been sent but not received
%% by the calling process, it is removed from the message queue.
%% Note that this only works of the same process
%% that did the attach does the detach.
%%
%% raises: `badarg'
%%
%% @see attach/2
%% @end
%%------------------------------------------------------------------------------
-spec detach(Port,Ref) -> ok when
      Port :: mailbox(),
      Ref :: attach_ref().
detach(Port, {Port,Ref} ) when ?INT_32BIT(Ref) ->
    try port_command(Port, <<?DETACH:8, Ref:32>>) of
	true ->
	    receive
		{ose_drv_reply,Port,{error,enoent}} ->
		    %% enoent could mean that it is in the message queue
		    receive
			{mailbox_down,Port,{Port,Ref},_} ->
			    ok
		    after 0 ->
			    ok
		    end;
		{ose_drv_reply,Port,ok} ->
		    ok
	    end
    catch error:_Error ->
	    erlang:error(badarg,[Port,{Port,Ref}])
    end;
detach(Port,Ref) ->
    erlang:error(badarg,[Port,Ref]).

%%------------------------------------------------------------------------------
%% @doc Send an OSE message.
%%
%% The message is sent from the OSE process' own ID that is: `get_id(Port)'.
%%
%% raises: `badarg'
%%
%% @see send/5
%% @end
%%------------------------------------------------------------------------------
-spec send(Port,Pid,SigNo,SigData) -> ok when
      Port :: mailbox(),
      Pid :: mailbox_id(),
      SigNo :: message_number(),
      SigData :: iodata().
send(Port, Spid, SigNo, SigData) when ?INT_32BIT(Spid), ?INT_32BIT(SigNo) ->
    try erlang:port_command(Port, [<<?SEND:8, Spid:32, SigNo:32>>, SigData]) of
	true -> ok
    catch error:_Error ->
	    erlang:error(badarg,[Port,Spid,SigNo,SigData])
    end;
send(Port,Spid,SigNo,SigData) ->
    erlang:error(badarg,[Port,Spid,SigNo,SigData]).


%%------------------------------------------------------------------------------
%% @doc Send an OSE message with different sender.
%%
%% As {@link send/4} but the sender will be `SenderPid'.
%%
%% raises: `badarg'
%%
%% @see send/4
%% @end
%%------------------------------------------------------------------------------
-spec send(Port,Pid,SenderPid,SigNo,SigData) -> ok when
      Port :: mailbox(),
      Pid :: mailbox_id(),
      SenderPid :: mailbox_id(),
      SigNo :: message_number(),
      SigData :: iodata().
send(Port, Spid, SenderPid, SigNo, SigData)
  when ?INT_32BIT(Spid), ?INT_32BIT(SenderPid), ?INT_32BIT(SigNo) ->
    try erlang:port_command(Port, [<<?SEND_W_S:8, Spid:32, SenderPid:32,
				     SigNo:32>>, SigData]) of
	true -> ok
    catch error:_Error ->
	    erlang:error(badarg,[Port,Spid,SenderPid,SigNo,SigData])
    end;
send(Port,Spid,SenderPid,SigNo,SigData) ->
    erlang:error(badarg,[Port,Spid,SenderPid,SigNo,SigData]).

%%------------------------------------------------------------------------------
%% @doc Start listening for specified OSE signal numbers.
%%
%% The mailbox will send `{message,Port,{FromMboxId,ToMboxId,MsgNo,MsgData}}'
%% to the process that created the mailbox when an OSE message with any
%% of the specified `SigNos' arrives.
%%
%% Repeated calls to listen will replace the current set of signal numbers to
%% listen to. i.e
%%
%% ```1>ose:listen(MsgB,[1234,12345]).
%%  ok
%%  2> ose:listen(MsgB,[1234,123456]).
%%  ok.'''
%%
%% The above will first listen for signals with numbers 1234 and 12345, and then
%% replace that with only listening to 1234 and 123456.
%%
%% With the current implementation it is not possible to listen to all signal
%% numbers.
%%
%% raises: `badarg' | `enomem'
%%
%% @end
%%------------------------------------------------------------------------------
-spec listen(Port, SigNos) -> ok when
      Port :: mailbox(),
      SigNos :: list(message_number()).
listen(Port, SigNos) when is_list(SigNos) ->
    USSigNos = lists:usort(SigNos),
    BinSigNos = try
		    << <<SigNo:32>> ||
			SigNo <- USSigNos,
			?INT_32BIT(SigNo) orelse erlang:error(badarg)
		    >>
		catch _:_ ->
			erlang:error(badarg,[Port,SigNos])
		end,
    try port_command(Port, [?LISTEN, BinSigNos]) of
	true ->
	    receive
		{ose_drv_reply,Port,{error,Error}} ->
		    erlang:error(Error,[Port,SigNos]);
		{ose_drv_reply,Port,Else} ->
		    Else
	    end
    catch error:_Error ->
	    erlang:error(badarg,[Port,SigNos])
    end;
listen(Port, SigNos) ->
    erlang:error(badarg,[Port,SigNos]).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================
