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

%%
%%----------------------------------------------------------------------
%% Purpose : Megaco/H.248 customization of the Event Tracer tool
%%----------------------------------------------------------------------
%%

-module(megaco_filter).

-export([start/0, start/1, filter/1, raw_filter/1,
	 pretty_error/1, string_to_term/1]).


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").
-include_lib("et/include/et.hrl").


%%----------------------------------------------------------------------
%% BUGBUG: There are some opportunities for improvements:
%%
%% * This version of the module does only handle version 1 of the messages.
%% 
%% * The record definition of megaco_transaction_reply is copied from 
%%   megaco_message_internal.hrl as that header file contains some
%%   records that already are defined in megaco_message_{v1,v2,v3}.hrl.
%% * The records megaco_udp and megaco_tcp are copied from the files
%%   megaco_udp.hrl and megaco_tcp.hrl respectively, as we cannot include
%%   both header files. 
%%   They both defines the macros HEAP_SIZE and GC_MSG_LIMIT.

%%-include("megaco_message_internal.hrl").
-record('megaco_transaction_reply',
	{
	  transactionId, 
	  immAckRequired       = asn1_NOVALUE, 
	  transactionResult,
	  segmentNumber        = asn1_NOVALUE,
	  segmentationComplete = asn1_NOVALUE
	 }). 

%% -include_lib("megaco/src/udp/megaco_udp.hrl").
-record(megaco_udp,
	{port,
	 options   = [],
	 socket,
	 receive_handle,
	 module    = megaco,
	 serialize = false  % false: Spawn a new process for each message
	}).

%% -include_lib("megaco/src/tcp/megaco_tcp.hrl").
-record(megaco_tcp,
	{host,
	 port,
	 options   = [],
	 socket,
	 proxy_pid,
	 receive_handle,
	 module    = megaco,
	 serialize = false  % false: Spawn a new process for each message
	}).


%%----------------------------------------------------------------------

start() ->
    start([]).

start(ExtraOptions) ->
    Options =
	[{event_order, event_ts},
	 {scale, 3},
	 {max_actors, infinity},
	 {trace_pattern, {megaco, max}},
	 {trace_global, true},
	 {dict_insert, {filter, ?MODULE}, fun filter/1},
	 {active_filter, ?MODULE},
	 {title, "Megaco tracer - Erlang/OTP"} | ExtraOptions],
    et_viewer:start(Options).

filter(E) ->
    case catch raw_filter(E) of
	{'EXIT', Reason} = Error->
	    io:format("~p: ~p\n", [?MODULE, Error]),
	    exit(Reason);
	E2 ->
	    E2
    end.

raw_filter(E) when is_record(E, event) ->
    From = filter_actor(E#event.from),
    To 	 = filter_actor(E#event.to),
    E2 	 = E#event{from = From, to = To},
    E3 	 = filter_contents(E#event.contents, E2),
    {true, E3}.

filter_actors(From, To, E) 
  when (E#event.from =:= ?APPLICATION) andalso (E#event.to =:= ?APPLICATION) ->
    Label = E#event.label,
    case lists:prefix("callback:", Label) of
	true ->
	    E#event{from = filter_actor(From),
		    to   = filter_user_actor(From)};
	false ->
	    case lists:prefix("return:", Label) of
		true ->
		    E#event{from = filter_user_actor(From),
			    to   = filter_actor(From)};
		false ->
		    case lists:prefix("receive bytes", Label) of
			true ->
			    E#event{from = filter_actor(To),
				    to   = filter_actor(From)};
			false ->
			    E#event{from = filter_actor(From),
				    to   = filter_actor(To)}
		    end
	    end
    end;
filter_actors(_From, _To, E) ->
    E.

filter_actor(Actor) ->
    String = do_filter_actor(Actor),
    if
	length(String) > 21 ->
	    string:substr(String, 1, 21) ++ [$*];
	true ->
	    String
    end.
	
filter_user_actor(Actor) ->
    String = do_filter_actor(Actor) ++ "@user",
    if
	length(String) > 21 ->
	    string:substr(String, 1, 21) ++ [$*];
	true ->
	    String
    end.
	
do_filter_actor(CH) when is_record(CH, megaco_conn_handle) ->
    Mid = CH#megaco_conn_handle.local_mid,
    do_filter_actor(Mid);
do_filter_actor(RH) when is_record(RH, megaco_receive_handle) ->
    Mid = RH#megaco_receive_handle.local_mid,
    do_filter_actor(Mid);
do_filter_actor(Actor) ->
    case Actor of
	{ip4Address, {'IP4Address', [A1,A2,A3,A4], asn1_NOVALUE}} ->
	    integer_to_list(A1) ++ [$.] ++
	    integer_to_list(A2) ++ [$.] ++
	    integer_to_list(A3) ++ [$.] ++
	    integer_to_list(A4);
	{ip4Address, {'IP4Address', [A1,A2,A3,A4], Port}} ->	
	    integer_to_list(A1) ++ [$.] ++
	    integer_to_list(A2) ++ [$.] ++
	    integer_to_list(A3) ++ [$.] ++
	    integer_to_list(A4) ++ [$:] ++
	    integer_to_list(Port);
	{domainName, {'DomainName', Name, asn1_NOVALUE}} ->
	    Name;
	{domainName, {'DomainName', Name, Port}} ->
	    Name ++ [$:] ++ integer_to_list(Port);
	{deviceName, Name} ->
	    Name;
	unknown_remote_mid ->
	    "preliminary_mid";
	preliminary_mid ->
	    "preliminary_mid";
	megaco ->
	    megaco;
	_Other ->
	    "UNKNOWN"
    end.


filter_contents(Contents, E) ->
    do_filter_contents(Contents, E, missing_conn_data, []).

do_filter_contents([H | T], E, ConnData, Contents) ->
    case H of
	Udp when is_record(Udp, megaco_udp) ->
	    RH = Udp#megaco_udp.receive_handle,
	    Actor = filter_actor(RH),
	    E2 = E#event{from = Actor, to = Actor},
	    Pretty =
		["Port:    ", integer_to_list(Udp#megaco_udp.port), "\n",
		 "Encoder: ", atom_to_list(RH#megaco_receive_handle.encoding_mod)],
	    do_filter_contents(T, E2, ConnData, [[Pretty, "\n"], Contents]);
	Tcp when is_record(Tcp, megaco_tcp) ->
	    RH = Tcp#megaco_tcp.receive_handle,
	    Actor = filter_actor(RH),
	    E2 = E#event{from = Actor, to = Actor},
	    Pretty =
		["Port:    ", integer_to_list(Tcp#megaco_tcp.port), "\n",
		 "Encoder: ", atom_to_list(RH#megaco_receive_handle.encoding_mod)],
	    do_filter_contents(T, E2, ConnData, [[Pretty, "\n"], Contents]);
	CD when is_record(CD, conn_data) ->
	    CH = CD#conn_data.conn_handle,
	    From = CH#megaco_conn_handle.local_mid,
	    To = CH#megaco_conn_handle.remote_mid,
	    E2 = filter_actors(From, To, E),
	    Serial = CD#conn_data.serial,
	    E3 = append_serial(Serial, E2),
	    do_filter_contents(T, E3, CD, Contents);
	CH when is_record(CH, megaco_conn_handle) ->
	    From = CH#megaco_conn_handle.local_mid,
	    To = CH#megaco_conn_handle.remote_mid,
	    E2 = filter_actors(From, To, E),
	    do_filter_contents(T, E2, ConnData, Contents);
	RH when is_record(RH, megaco_receive_handle) ->
	    Actor = RH#megaco_receive_handle.local_mid,
	    E2 = filter_actors(Actor, Actor, E),
	    do_filter_contents(T, E2, ConnData, Contents);
	{error, Reason} ->
	    Pretty = pretty_error({error, Reason}),
	    E2 = prepend_error(E),
	    do_filter_contents(T, E2, ConnData, [[Pretty, "\n"], Contents]);
	{'EXIT', Reason} ->
	    Pretty = pretty_error({'EXIT', Reason}),
	    E2 = prepend_error(E),
	    do_filter_contents(T, E2, ConnData, [[Pretty, "\n"], Contents]);
	ED when is_record(ED, 'ErrorDescriptor') ->
	    Pretty = pretty_error(ED),
	    E2 = prepend_error(E),
	    do_filter_contents(T, E2, ConnData, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionRequest') ->
	    Pretty = pretty(ConnData, {trans, {transactionRequest, Trans}}),
	    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
	{transactionRequest, Trans} when is_record(Trans, 'TransactionRequest') ->
	    Pretty = pretty(ConnData, {trans, {transactionRequest, Trans}}),
	    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionReply') ->
	    Pretty = pretty(ConnData, {trans, {transactionReply, Trans}}),
	    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, megaco_transaction_reply) ->
	    %% BUGBUG: Version 1 special
	    TransV1 = 
		#'TransactionReply'{transactionId     = Trans#megaco_transaction_reply.transactionId,
				    immAckRequired    = Trans#megaco_transaction_reply.immAckRequired,
				    transactionResult = Trans#megaco_transaction_reply.transactionResult},
	    Pretty = pretty(ConnData, {trans, {transactionReply, TransV1}}),
	    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionPending') ->	    
	    Pretty = pretty(ConnData, {trans, {transactionPending, Trans}}),
	    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionAck') ->	    
	    Pretty = pretty(ConnData, {trans, {transactionResponseAck, [Trans]}}),
	    case Trans#'TransactionAck'.lastAck of
		asn1_NOVALUE ->
		    do_filter_contents([], E, ConnData, [[Pretty, "\n"], Contents]);
		Last ->
		    Label = term_to_string(E#event.label),
		    E2 = E#event{label = Label ++ ".." ++ integer_to_list(Last)},
		    do_filter_contents([], E2, ConnData, [[Pretty, "\n"], Contents])
	    end;
	{context_id, _ContextId} ->
	    Pretty = pretty(ConnData, H),
	    do_filter_contents(T, E, ConnData, [[Pretty, "\n"], Contents]);
	{command_request, CmdReq} ->
	    Pretty = pretty(ConnData, CmdReq),
	    do_filter_contents(T, E, ConnData, [[Pretty, "\n"], Contents]);
	{user_reply, {ok, ARS}} ->
	    Pretty = [[pretty(ConnData, AR), "\n"] || AR <- ARS],
	    do_filter_contents(T, E, ConnData, [["USER REPLY OK: \n", Pretty, "\n"], Contents]);
	{user_reply, Error} ->
	    Pretty = pretty_error(Error),
	    do_filter_contents(T, E, ConnData, [["USER REPLY ERROR: \n", Pretty, "\n"], Contents]);
	{actionReplies, ARS} ->
	    Pretty = [[pretty(ConnData, AR), "\n"] || AR <- ARS],
	    do_filter_contents(T, E, ConnData, [["ACTION REPLIES: \n", Pretty, "\n"], Contents]);
	MegaMsg when is_record(MegaMsg, 'MegacoMessage') ->
	    Pretty = pretty(ConnData, MegaMsg),
	    do_filter_contents(T, E, ConnData, [Pretty, "\n", Contents]);
	{message, MegaMsg} when is_record(MegaMsg, 'MegacoMessage') ->
	    Pretty = pretty(ConnData, MegaMsg),
	    do_filter_contents(T, E, ConnData, [Pretty, "\n", Contents]);
	{bytes, Bin} when is_binary(Bin) ->
            E2 = 
		case E#event.label of
		    [$s, $e, $n, $d, $ , $b, $y, $t, $e, $s | Tail] ->
			L = lists:concat(["send ", size(Bin), " bytes", Tail]),
			E#event{label = L};
		    [$r, $e, $c, $e, $i, $v, $e, $ , $b, $y, $t, $e, $s | Tail] ->
			L = lists:concat(["receive ", size(Bin), " bytes", Tail]),
			E#event{label = L};
		    _ ->
			E
		end,
	    CharList = erlang:binary_to_list(Bin),
	    do_filter_contents(T, E2, ConnData, [[CharList , "\n"], Contents]);
	List when is_list(List) ->
	    %% BUGBUG: Workaround as megaco_messenger puts nested lists in its traces
	    do_filter_contents(List ++ T, E, ConnData, Contents);
	Int when is_integer(Int) ->
	    %% BUGBUG: Workaround as megaco_messenger puts nested lists in its traces
	    do_filter_contents(T, E, ConnData, Contents);
	{line, _Mod, _Line} ->
	    do_filter_contents(T, E, ConnData, Contents);
	{orig_conn_handle, _CH} ->
	    do_filter_contents(T, E, ConnData, Contents);
	{pid, Pid} when is_pid(Pid) ->
	    do_filter_contents(T, E, ConnData, Contents);
	pending ->
	    do_filter_contents(T, E, ConnData, Contents);
	reply ->
	    do_filter_contents(T, E, ConnData, Contents);
	{test_lib, _Mod, _Fun} ->
	    do_filter_contents(T, E, ConnData, Contents);
	{trans_id, _TransId} ->
	    do_filter_contents(T, E, ConnData, Contents);
	{send_func, _FunName} ->
	    do_filter_contents(T, E, ConnData, Contents);
	Pid when is_pid(Pid) ->
	    do_filter_contents(T, E, ConnData, Contents);
	Other ->
	    Pretty = pretty(ConnData, Other),
	    do_filter_contents(T, E, ConnData, [[Pretty, "\n"], Contents])
    end;
do_filter_contents([], E, _ConnData, Contents) ->
    E#event{contents = lists:flatten(lists:reverse(Contents))}.

append_serial(Serial, E) when is_integer(Serial) ->
    Label = term_to_string(E#event.label),
    E#event{label = Label ++ " #" ++ integer_to_list(Serial)};
append_serial(_Serial, E) ->
    E.

prepend_error(E) ->
    Label = term_to_string(E#event.label),
    E#event{label = "<ERROR> " ++ Label}.

pretty(_ConnData, {context_id, ContextId}) ->
     if
	 ContextId =:= ?megaco_null_context_id ->
	     ["CONTEXT ID: -\n"];
	 ContextId =:= ?megaco_choose_context_id ->
	     ["CONTEXT ID: $\n"];
	 ContextId =:= ?megaco_all_context_id ->
	     ["CONTEXT ID: *\n"];
	 is_integer(ContextId) ->
	     ["CONTEXT ID: ",integer_to_list(ContextId), "\n"]
     end;
pretty(_ConnData, MegaMsg) when is_record(MegaMsg, 'MegacoMessage') ->
    {ok, Bin} = megaco_pretty_text_encoder:encode_message([], MegaMsg),
    term_to_string(Bin);
pretty(_ConnData, CmdReq) when is_record(CmdReq, 'CommandRequest') ->
    {ok, Bin} = megaco_pretty_text_encoder:encode_command_request(CmdReq),
    term_to_string(Bin);
pretty(_ConnData, {complete_success, ContextId, RepList}) ->
    ActRep = #'ActionReply'{contextId    = ContextId, 
			    commandReply = RepList},
    {ok, Bin} = megaco_pretty_text_encoder:encode_action_reply(ActRep),
    term_to_string(Bin);
pretty(_ConnData, AR) when is_record(AR, 'ActionReply') ->
    {ok, Bin} = megaco_pretty_text_encoder:encode_action_reply(AR),
    term_to_string(Bin);
pretty(_ConnData, {partial_failure, ContextId, RepList}) ->
    ActRep = #'ActionReply'{contextId    = ContextId, 
			    commandReply = RepList},
    {ok, Bin} = megaco_pretty_text_encoder:encode_action_reply(ActRep),
    term_to_string(Bin);
pretty(_ConnData, {trans, Trans}) ->
    {ok, Bin} = megaco_pretty_text_encoder:encode_transaction(Trans),
    term_to_string(Bin);
pretty(__ConnData, Other) ->
    term_to_string(Other).

pretty_error({error, Reason}) ->
    ["ERROR: ", pretty_error(Reason)];
pretty_error({'EXIT', Reason}) ->
    ["EXIT: ", pretty_error(Reason)];
pretty_error({'ErrorDescriptor', Code, Reason}) ->
    ["CODE: ", integer_to_list(Code), " TEXT: ", pretty_error(Reason)];
pretty_error(Ugly) ->
    case string_to_term(Ugly) of
	{ok, Pretty} -> ["\n", Pretty];
	_            -> ["\n", term_to_string(Ugly)]
    end.

string_to_term(Chars) ->
    do_string_to_term([], Chars, 1).

do_string_to_term(Cont, Chars, Line) ->
    case catch erl_scan:tokens(Cont, Chars, Line) of
	{done, {ok, Tokens, _EndLine}, _Rest} ->
	    case erl_parse:parse_term(Tokens) of
		{ok, Term} ->
		    {ok, Term};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{more, Cont2} ->
	    do_string_to_term(Cont2, ". ", Line);
	Other ->
	    {error, Other}
    end.

term_to_string(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
term_to_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~p", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.
