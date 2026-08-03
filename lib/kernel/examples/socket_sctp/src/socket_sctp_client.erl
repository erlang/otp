%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2026. All Rights Reserved.
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
%% Note that this (client) module is *not* an example!
%% This module is just intended to test the actual example:
%% the echo server.
%%


-module(socket_sctp_client).

-export([start_monitor/3, start_monitor/4, start_monitor/5,
         start/3, start/4, start/5,
         stop/1]).

-export([start_it/5]).

-include("socket_sctp_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MAX_RESEND,    5).
-define(ECHO_TIMEOUT,  timer:seconds(1)).
-define(DEFAULT_DEBUG, false).
-define(DEFAULT_OPTS,  #{debug        => ?DEFAULT_DEBUG,
                         max_resend   => ?MAX_RESEND,
                         echo_timeout => timer:seconds(1)}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type opts() :: #{debug        := boolean(),
                  max_resend   := pos_integer(),
                  echo_timeout := pos_integer()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_monitor(ID,
              ServerSA, MessageOrMessages) ->
    start_monitor(node(), ID, ServerSA, MessageOrMessages, #{}).

start_monitor(Node,
              ID, ServerSA, MessageOrMessages) ->
    start_monitor(Node, ID, ServerSA, MessageOrMessages, #{}).

-spec start_monitor(Node,
                    ID,
                    ServerSA,
                    MessageOrMessages,
                    Opts) -> {ok, Client} | {error, Reason} when
      Node              :: node(),
      ID                :: pos_integer(),
      ServerSA          :: socket:sockaddr(),
      MessageOrMessages :: Message | [Message | {Message, PostSendTimeout}],
      Message           :: string(),
      PostSendTimeout   :: pos_integer(),
      Opts              :: opts(),
      Client            :: {Pid, MRef, SA},
      Pid               :: pid(),
      MRef              :: reference(),
      SA                :: socket:sockaddr(),
      Reason            :: term().

start_monitor(Node,
              ID,
              ServerSA,
              MessageOrMessages, Opts)
  when is_atom(Node) andalso is_list(MessageOrMessages) andalso is_map(Opts) ->
    do_start_monitor(Node, ID, ServerSA, MessageOrMessages, ensure_opts(Opts)).


do_start_monitor(Node, ID, ServerSA, MessageOrMessages, Opts) ->
    case do_start(Node, ID, ServerSA, MessageOrMessages, Opts) of
        {ok, {Pid, SA}} when is_pid(Pid) ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef, SA}};
        {error, _} = ERROR ->
            ERROR
    end.



start(ID, ServerSA, MessageOrMessages) ->
    start(node(), ID, ServerSA, MessageOrMessages, #{}).

start(Node, ID, ServerSA, MessageOrMessages) ->
    start(Node, ID, ServerSA, MessageOrMessages, #{}).

-spec start(Node,
            ID,
            ServerSA,
            MessageOrMessages,
            Opts) -> {ok, Client} | {error, Reason} when
      Node              :: node(),
      ID                :: pos_integer(),
      ServerSA          :: socket:sockaddr(),
      MessageOrMessages :: Message | [Message | {Message, PostSendTimeout}],
      Message           :: string(),
      PostSendTimeout   :: pos_integer(),
      Opts              :: opts(),
      Client            :: {Pid, SA},
      Pid               :: pid(),
      SA                :: socket:sockaddr(),
      Reason            :: term().

start(Node,
      ID,
      ServerSA,
      MessageOrMessages, Opts)
  when is_atom(Node) andalso is_list(MessageOrMessages) andalso is_map(Opts) ->
    do_start(Node, ID, ServerSA, MessageOrMessages, ensure_opts(Opts)).


do_start(Node,
         ID,
         #{family := Fam,
           addr   := _,
           port   := _} = ServerSA,
         MessageOrMessages,
         #{debug        := Debug,
           max_resend   := MaxResend,
           echo_timeout := EchoTimeout} = Opts)
  when is_atom(Node) andalso (Node =/= node()) andalso
       is_integer(ID) andalso (ID > 0) andalso
       ((Fam =:= inet) orelse (Fam =:= inet6)) andalso
       is_list(MessageOrMessages) andalso
       is_boolean(Debug) andalso
       is_integer(MaxResend) andalso (MaxResend >= 0) andalso
       is_integer(EchoTimeout) andalso (EchoTimeout >= 0) ->
    Messages = process_messages(MessageOrMessages),
    Args     = [ID, self(), ServerSA, Messages, Opts],
    case rpc:call(Node, ?MODULE, start_it, Args) of
        {badrpc, Reason} ->
            {error, Reason};
        {ok, {Pid, _}} = OK when is_pid(Pid) ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end;
do_start(_,
         ID,
         #{family := Fam,
           addr   := _,
           port   := _} = ServerSA,
         MessageOrMessages,
         #{debug        := Debug,
           max_resend   := MaxResend,
           echo_timeout := EchoTimeout} = Opts)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) andalso
       is_integer(ID) andalso (ID > 0) andalso
       is_list(MessageOrMessages) andalso
       is_boolean(Debug) andalso
       is_integer(MaxResend) andalso (MaxResend >= 0) andalso
       is_integer(EchoTimeout) andalso (EchoTimeout >= 0) ->
    Messages = process_messages(MessageOrMessages),
    start_it(ID, self(), ServerSA, Messages, Opts).


stop(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {?MODULE, self(), stop},
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_it(ID,
         Parent,
         ServerSA,
         Messages,
         #{debug        := Debug,
           max_resend   := MaxResend,
           echo_timeout := EchoTimeout} = _Opts) ->

    put(sname, ?F("client-starter[~w]", [ID])),
    put(dbg, Debug),

    Self = self(),
    ?DEBUG("~s -> try start client ~w", [?FUNCTION_NAME, ID]),
    {Client, MRef} =
        spawn_monitor(
          fun() ->
                  init(#{id           => ID,
                         starter      => Self,
                         parent       => Parent,
		         server_sa    => ServerSA,
			 msgs         => Messages,
			 dbg          => Debug,
                         max_resend   => MaxResend,
                         echo_timeout => EchoTimeout})
          end),
    ?DEBUG("~s -> await client ~w start response", [?FUNCTION_NAME, ID]),
    receive
        {'DOWN', MRef, process, Client, Reason} ->
            ?ERROR("Client start failure: "
                   "~n   ~p", [Reason]),
            {error, Reason};

        {?MODULE, Client, {started, SA}} ->
            ?INFO("Client (~p) started:"
                  "~n   SA: ~p", [Client, SA]),
            {ok, {Client, SA}}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_messages(MessageOrMessages) ->
    case ?IS_STR(MessageOrMessages) of
        true ->
	    %% Ok, this was actually only one message
	    [{MessageOrMessages, 0}];
	false -> % Maybe messages
	    process_messages2(MessageOrMessages)
    end.

process_messages2([]) ->
    [];
process_messages2([{Message, TO}|Messages])
  when is_list(Message) andalso is_integer(TO) andalso (TO > 0) ->
    case ?IS_STR(Message) of
        true ->
	    [{Message, TO} | process_messages2(Messages)];
	false ->
	    ?WARNING("Skip invalid message: "
	             "~n   '~p'", [Message]),
	    process_messages2(Messages)
    end;
process_messages2([{Message, TO}|Messages])
  when is_list(Message) ->
    case ?IS_STR(Message) of
        true ->
	    ?WARNING("Skip invalid post echo timeout for: "
	    	     "~n   Message:           ~p"
		     "~n   (invalid) Timeout: ~p", [Message, TO]),
            [{Message, 0} | process_messages2(Messages)];
	false ->
	    ?WARNING("Skip invalid message: "
	             "~n   '~p'", [Message]),
	    process_messages2(Messages)
    end;
process_messages2([{Message, _TO}|Messages]) ->
    ?WARNING("Skip invalid message: "
	     "~n   '~p'", [Message]),
    process_messages2(Messages);
process_messages2([Message|Messages])
  when is_list(Message) ->
    case ?IS_STR(Message) of
        true ->
            [{Message, 0} | process_messages2(Messages)];
	false ->
	    ?ERROR("Skip invalid message: "
	           "~n   '~p'", [Message]),
	    process_messages2(Messages)
    end;
process_messages2([Message|Messages]) ->
    ?WARNING("Skip invalid message: "
	     "~n   '~p'", [Message]),
    process_messages2(Messages).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(#{id           := ID,
       starter      := Starter,
       parent       := Parent,
       server_sa    := #{family := Domain} = ServerSA,
       msgs         := Messages,
       dbg          := Debug,
       max_resend   := MaxResend,
       echo_timeout := EchoTimeout}) ->

    put(sname, ?F("client[~w]", [ID])),
    put(dbg,   Debug),

    ?DEBUG("~s -> try open (~w) socket", [?FUNCTION_NAME, Domain]),
    Sock = case socket:open(Domain, seqpacket, sctp) of
	       {ok, S} ->
		   S;
	       {error, OReason} ->
		   ?ERROR("Failed open socket: "
                          "~n   ~p", [OReason]),
		   ?STOP()
	   end,

    ?DEBUG("~s -> try get (local) address", [?FUNCTION_NAME]),
    Addr = case ?WHICH_ADDR(Domain) of
               {ok, A} ->
                   A;
               {error, AReason} ->
                   ?ERROR("failed get (local) address: "
                          "~n   ~p", [AReason]),
                   ?STOP()
           end,

    ?DEBUG("~s -> try bind socket (to ~p)", [?FUNCTION_NAME, Addr]),
    RawSA = #{family => Domain,
	      addr   => Addr,
	      port   => 0},
    case socket:bind(Sock, RawSA) of
	ok ->
	    ok;
	{error, BReason} ->
	    ?ERROR("Failed bind socket: "
                   "~n   ~p", [BReason]),
	    ?STOP()
    end,

    OwnSA = case socket:sockname(Sock) of
                {ok, OSA} ->
                    OSA;
                {error, OSReason} ->
                    ?ERROR("Failed get sockname: "
                           "~n   ~p", [OSReason]),
                    ?STOP()
            end,                    

    ?DEBUG("~s -> try subscribe to events", [?FUNCTION_NAME]),
    ok = socket:setopt(Sock, ?MK_SCTP_SOCKOPT(events), ?SCTP_EVENTS(true)),

    ?DEBUG("~s -> try connect to: "
           "~n   ~p", [?FUNCTION_NAME, ServerSA]),
    ok = socket:connect(Sock, ServerSA),
    

    ?DEBUG("~s -> await connect confirmation", [?FUNCTION_NAME]),
    {AssocID, OutStreams, InStreams} =
	case socket:recvmsg(Sock) of
	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := #{type             := assoc_change,
				     state            := comm_up,
				     assoc_id         := AID,
                                     outbound_streams := OS,
                                     inbound_streams  := IS}}} ->
		?INFO("Received expected connect confirmation:"
                      "~n   Flags:   ~p"
                      "~n   SA:      ~p"
                      "~n   AssocID: ~p"
                      "~n   OS:      ~p"
                      "~n   IS:      ~p",
                      [Flags, SA, AID, OS, IS]),
		{AID, OS, IS};

	    {ok, #{flags        := Flags,
                   addr         := SA,
                   notification := #{type  := assoc_change,
				     state := State}}} ->
		?ERROR("Received unexpected connect failure: ~p"
                       "~n   Flags: ~p"
                       "~n   SA:    ~p",
                       [?FUNCTION_NAME, State, Flags, SA]),
                exit({unexpected, State});

            {ok, Msg} ->
 		?ERROR("Received unexpected msg: "
                       "~n   ~p",
                       [?FUNCTION_NAME, Msg]),
		exit({unexpected_msg, Msg});

	    {error, ReasonC} ->
		?ERROR("Unexpected recvmsg failure: "
                       "~n   ~p",
                       [?FUNCTION_NAME, ReasonC]),
		exit({unexpected_recvmsg_failure, ReasonC})
	end,

    ?DEBUG("~s -> monitor parent", [?FUNCTION_NAME]),
    MRef = erlang:monitor(process, Parent),

    Starter ! {?MODULE, self(), {started, OwnSA}},

    loop(#{mode         => send,
           parent       => Parent,
           parent_mref  => MRef,
           sock         => Sock,
           assoc_id     => AssocID,
           stream       => 0,
           ostream      => OutStreams,
           istreams     => InStreams,
           select       => undefined,
	   resend       => {0, undefined},
           max_resend   => MaxResend,
           echo_timeout => EchoTimeout},
         Messages).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(#{mode     := shutdown,
       sock     := Sock,
       assoc_id := AID}, Messages) ->
    ?INFO("shutting down with ~w messages", [length(Messages)]),
    EofSRI = #{assoc_id     => AID,
               stream       => 0,
               ssn          => 0,
               flags        => [eof],
               ppid         => 0,
               context      => 0,
               time_to_live => 0,
               tsn          => 0,
               cum_tsn      => 0},
    EofMsg = #{iov  => [],
               ctrl => [#{level => sctp,
                          type  => sndrcv,
                          value => EofSRI}]},
    _ = socket:sendmsg(Sock, EofMsg),
    ?CATCH_AND_IGNORE(socket:close(Sock));
    
loop(#{sock     := Sock,
       assoc_id := AID,
       stream   := Stream} = State, [] = Messages)
  when (Sock   =/= undefined) andalso
       (AID    =/= undefined) andalso
       (Stream =/= undefined) ->
    ?INFO("all messages sent and ackwnoledged - send eof"),
    EofSRI = #{assoc_id     => AID,
               stream       => 0,
               ssn          => 0,
               flags        => [eof],
               ppid         => 0,
               context      => 0,
               time_to_live => 0,
               tsn          => 0,
               cum_tsn      => 0},
    EofMsg = #{iov  => [],
               ctrl => [#{level => sctp,
                          type  => sndrcv,
                          value => EofSRI}]},
    case socket:sendmsg(Sock, EofMsg) of
        ok ->
            ?INFO("~s -> eof message sent", [?FUNCTION_NAME]),
            loop(State#{assoc_id => undefined,
                        stream   => undefined},
                 Messages);
        {error, Reason} ->
            ?ERROR("Failed issue (send) eof (graceful shutdown):"
                   "~n   ~p", [Reason]),
            exit({eof, Reason})
    end;
loop(#{sock := Sock}, [] = _Messages) ->
    ?INFO("done"),
    ?CATCH_AND_IGNORE(socket:close(Sock)),
    exit(normal);
    
%% Send the message
loop(#{mode     := send,
       sock     := Sock,
       assoc_id := AID,
       stream   := Stream,
       ostream  := OutStreams,
       select   := undefined} = State,
     [{Message, _}|_] = Messages) ->
    ?DEBUG("~s(~w,~w) -> try send message", [?FUNCTION_NAME, AID, Stream]),
    SRI     = #{assoc_id => AID, stream => Stream},
    CtrlSRI = #{level => sctp,
                type  => sndrcv,
                value => SRI},
    Msg = #{iov  => [list_to_binary(Message)],
            ctrl => [CtrlSRI]},
    case socket:sendmsg(Sock, Msg) of
        ok ->
            ?INFO("message sent: "
                  "~n   ~p", [Message]),
            %% 'Message' will be removed from list when it has been acknowledged
	    NewState = start_echo_timer(State),
            loop(NewState#{stream => next_stream_id(Stream, OutStreams),
                           mode   => recv}, Messages);

        {select, SelectInfo} ->
            ?DEBUG("~s -> select", [?FUNCTION_NAME]),
            loop(State#{select => SelectInfo}, Messages);

        {error, Reason} ->
            ?ERROR("failed sending message:"
                   "~n   ~p", [Reason]),
            exit({sendmsg, Reason})
    end;

%% Await the acknowledgement
loop(#{mode     := recv,
       sock     := Sock,
       assoc_id := AID,
       select   := undefined} = State,
     [{Message, TO}|RestMessages] = Messages)
  when (Sock =/= undefined) andalso (AID =/= undefined) ->
    ?DEBUG("~s -> try recvmsg", [?FUNCTION_NAME]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AID,
                                      stream   := Stream}}]}} ->
            ?DEBUG("~s -> received data:"
                   "~n   From:    ~p"
                   "~n   AssocID: ~p"
                   "~n   Stream:  ~p", [?FUNCTION_NAME, SA, AID, Stream]),
            case binary_to_list(Data) of
                Message ->
                    ?INFO("received echo:"
                          "~n   ~p", [Message]),
		    NewState = cancel_echo_timer(State),
                    maybe_sleep(TO),
                    loop(NewState#{mode => send}, RestMessages);
                UnExpMessage ->
                    ?ERROR("Received unexpected massage: "
                           "~n   Expected: ~p"
                           "~n   Received: ~p", [Message, UnExpMessage]),
                    exit({unexpected_message, {Message, UnExpMessage}})
            end;

        {ok, #{notification := Notification}} ->
            ?WARNING("Received unexpected notification:"
                     "~n   ~p", [Notification]),
            loop(State, Messages);

        {select, SelectInfo} ->
            ?DEBUG("~s -> select", [?FUNCTION_NAME]),
            loop(State#{select => SelectInfo}, Messages);

        {error, Reason} ->
            ?ERROR("Receive failed:"
                   "~n   ~p", [Reason]),
            exit({recvmsg, Reason})
    end;

loop(#{mode        := Mode,
       parent      := Parent,
       parent_mref := MRef,
       sock        := Sock,
       select      := {select_info, _, SelectHandle}} = State,
     Messages) ->
    ?DEBUG("~s -> await ~w select message", [?FUNCTION_NAME, Mode]),
    receive
        {'DOWN', MRef, process, Parent, Info} ->
            ?ERROR("Received unexpected DOWN from parent:"
               "~n   ~p", [Info]),
            ?CATCH_AND_IGNORE(socket:close(Sock)),
            exit({parent_died, Info});

        {'$socket', Sock, select, SelectHandle} ->
            ?DEBUG("~s -> received select message", [?FUNCTION_NAME]),
            loop(State#{select => undefined}, Messages);

        {'$socket', Sock, abort, Info} ->
            ?ERROR("~s(~w) -> received unexpected abort message:"
                   "~n   ~p", [?FUNCTION_NAME, Mode, Info]),
            exit({abort, Info});

        {timeout, TRef, echo_timeout} ->
            ?DEBUG("~s -> received (echo-) timeout message", [?FUNCTION_NAME]),
	    NewState = handle_echo_timeout(State, TRef),
	    loop(NewState, Messages)
    end.
		   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_stream_id(Stream, NumStreams) when Stream < NumStreams ->
    Stream + 1;
next_stream_id(_, _) ->
    0.

%% This timer is used to handle that we do not get an echo in time
start_echo_timer(#{resend       := {NumResend, undefined},
                   echo_timeout := EchoTimeout} = State) ->
    TRef = erlang:start_timer(EchoTimeout, self(), echo_timeout),
    State#{resend => {NumResend+1, TRef}}.

cancel_echo_timer(#{resend := {_, TRef}} = State) ->
    _ = erlang:cancel_timer(TRef),
    State#{resend => {0, undefined}}.

%% We are waiting for an echo, but got a echo timeout: resend
handle_echo_timeout(#{mode       := recv,
                      resend     := {NumResend, TRef},
                      max_resend := MaxResend,
                      sock       := Sock,
		      select     := SelectInfo} = State, TRef)
    when (NumResend < MaxResend) andalso (SelectInfo =/= undefined) ->
    ?WARNING("echo timeout - issue resend"),
    ?CATCH_AND_IGNORE(socket:cancel(Sock, SelectInfo)),
    State#{mode   => send,
           resend => {NumResend, undefined},
	   select => undefined};
handle_echo_timeout(State, _TRef) ->
    ?ERROR("echo timeout - shut down"),
    State#{mode => shutdown, resend => timeout}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_sleep(TO) when (TO > 0) ->
    ?SLEEP(TO);
maybe_sleep(_) ->
    ok.

ensure_opts(Opts) ->
    maps:merge(?DEFAULT_OPTS, Opts).

