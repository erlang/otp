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

%%
%%----------------------------------------------------------------------
%% Purpose : megaco/H.248 customization of generic event tracer
%%----------------------------------------------------------------------

-module(megaco_filter).

-export([start/0, start/1, filter/1,
	 pretty_error/1, string_to_term/1]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").
-include_lib("et/include/et.hrl").

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

filter(E) when is_record(E, event) ->
    From = filter_actor(E#event.from),
    To 	 = filter_actor(E#event.to),
    E2 	 = E#event{from = From, to = To},
    E3 	 = filter_contents(E#event.contents, E2, []),
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

filter_contents([], E, Contents) ->
    E#event{contents = lists:flatten(lists:reverse(Contents))};
filter_contents([H | T], E, Contents) ->
    case H of
	{line, _Mod, _Line} ->
	    filter_contents(T, E, Contents);
	CD when is_record(CD, conn_data) ->
	    CH = CD#conn_data.conn_handle,
	    From = CH#megaco_conn_handle.local_mid,
	    To   = CH#megaco_conn_handle.remote_mid,
	    E2 = filter_actors(From, To, E),
	    Serial = CD#conn_data.serial,
	    E3 = append_serial(Serial, E2),
	    filter_contents(T, E3, Contents);
	CH when is_record(CH, megaco_conn_handle) ->
	    From = CH#megaco_conn_handle.local_mid,
	    To   = CH#megaco_conn_handle.remote_mid,
	    E2 = filter_actors(From, To, E),
	    filter_contents(T, E2, Contents);
	{orig_conn_handle, _CH} ->
	    filter_contents(T, E, Contents);
	RH when is_record(RH, megaco_receive_handle) ->
	    Actor = RH#megaco_receive_handle.local_mid,
	    E2 = filter_actors(Actor, Actor, E),
	    filter_contents(T, E2, Contents);
	{pid, Pid} when is_pid(Pid) ->
	    filter_contents(T, E, Contents);
	pending ->
	    filter_contents(T, E, Contents);
	reply ->
	    filter_contents(T, E, Contents);
	{error, Reason} ->
	    Pretty = pretty_error({error, Reason}),
	    E2 = prepend_error(E),
	    filter_contents(T, E2, [[Pretty, "\n"], Contents]);
	{'EXIT', Reason} ->
	    Pretty = pretty_error({'EXIT', Reason}),
	    E2 = prepend_error(E),
	    filter_contents(T, E2, [[Pretty, "\n"], Contents]);
	ED when is_record(ED, 'ErrorDescriptor') ->
	    Pretty = pretty_error(ED),
	    E2 = prepend_error(E),
	    filter_contents(T, E2, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionRequest') ->
	    Pretty = pretty({trans, {transactionRequest, Trans}}),
	    filter_contents([], E, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionReply') ->
	    Pretty = pretty({trans, {transactionReply, Trans}}),
	    filter_contents([], E, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionPending') ->	    
	    Pretty = pretty({trans, {transactionPending, Trans}}),
	    filter_contents([], E, [[Pretty, "\n"], Contents]);
	Trans when is_record(Trans, 'TransactionAck') ->	    
	    Pretty = pretty({trans, {transactionResponseAck, [Trans]}}),
	    case Trans#'TransactionAck'.lastAck of
		asn1_NOVALUE ->
		    filter_contents([], E, [[Pretty, "\n"], Contents]);
		Last ->
		    Label = term_to_string(E#event.label),
		    E2 = E#event{label = Label ++ ".." ++ integer_to_list(Last)},
		    filter_contents([], E2, [[Pretty, "\n"], Contents])
	    end;
	{context_id, _ContextId} ->
	    Pretty = pretty(H),
	    filter_contents(T, E, [[Pretty, "\n"], Contents]);
	{command_request, CmdReq} ->
	    Pretty = pretty(CmdReq),
	    filter_contents(T, E, [[Pretty, "\n"], Contents]);
	{user_reply, {ok, ARS}} ->
	    Pretty = [[pretty(AR), "\n"] || AR <- ARS],
	    filter_contents(T, E, [["REPLY: \n", Pretty, "\n"], Contents]);
	{user_reply, Error} ->
	    Pretty = pretty_error(Error),
	    filter_contents(T, E, [["REPLY: \n", Pretty, "\n"], Contents]);
	{actionReplies, ARS} ->
	    Pretty = [[pretty(AR), "\n"] || AR <- ARS],
	    filter_contents(T, E, [["REPLY: \n", Pretty, "\n"], Contents]);
	MegaMsg when is_record(MegaMsg, 'MegacoMessage') ->
	    Pretty = pretty(MegaMsg),
	    filter_contents(T, E, [["MESSAGE: \n", Pretty, "\n"], Contents]);
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
	    filter_contents(T, E2, [[CharList , "\n"], Contents]);
	[] ->
	    filter_contents(T, E, Contents);
	{test_lib, _Mod, _Fun} ->
	    filter_contents(T, E, Contents);
	Other ->
	    Pretty = pretty(Other),
	    filter_contents(T, E, [[Pretty, "\n"], Contents])
    end.

append_serial(Serial, E) when is_integer(Serial) ->
    Label = term_to_string(E#event.label),
    E#event{label = Label ++ " #" ++ integer_to_list(Serial)};
append_serial(_Serial, E) ->
    E.

prepend_error(E) ->
    Label = term_to_string(E#event.label),
    E#event{label = "<ERROR> " ++ Label}.

pretty({context_id, ContextId}) ->
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
pretty(MegaMsg) when is_record(MegaMsg, 'MegacoMessage') ->
    case catch megaco_pretty_text_encoder:encode_message([], MegaMsg) of
	{ok, Bin} ->
	    term_to_string(Bin);
	_Bad ->
	    term_to_string(MegaMsg)
    end;
pretty(CmdReq) when is_record(CmdReq, 'CommandRequest') ->
    case catch megaco_pretty_text_encoder:encode_command_request(CmdReq) of
	{ok, IoList} ->
	    IoList2 = lists:flatten(IoList),
	    term_to_string(IoList2);
	_Bad ->
	    term_to_string(CmdReq)
    end;
pretty({complete_success, ContextId, RepList} = Res) ->
    ActRep = #'ActionReply'{contextId    = ContextId, 
			    commandReply = RepList},
    case catch megaco_pretty_text_encoder:encode_action_reply(ActRep) of
	{ok, IoList} ->
	    IoList2 = lists:flatten(IoList),
	    term_to_string(IoList2);
	_Bad ->
	    term_to_string(Res)
   end;
pretty(AR) when is_record(AR, 'ActionReply') ->
    case catch megaco_pretty_text_encoder:encode_action_reply(AR) of
	{ok, IoList} ->
	    IoList2 = lists:flatten(IoList),
	    term_to_string(IoList2);
	_Bad ->
	    term_to_string(AR)
   end;
pretty({partial_failure, ContextId, RepList} = Res) ->
    ActRep = #'ActionReply'{contextId    = ContextId, 
			    commandReply = RepList},
    case catch megaco_pretty_text_encoder:encode_action_reply(ActRep) of
	{ok, IoList} ->
	    IoList2 = lists:flatten(IoList),
	    term_to_string(IoList2);
	_Bad ->
	    term_to_string(Res)
   end;
pretty({trans, Trans}) ->
    case catch megaco_pretty_text_encoder:encode_transaction(Trans) of
	{ok, Bin} when is_binary(Bin) ->
	    IoList2 = lists:flatten(binary_to_list(Bin)),
	    term_to_string(IoList2);
	{ok, IoList} ->
	    IoList2 = lists:flatten(IoList),
	    term_to_string(IoList2);
	_Bad ->
	    term_to_string(Trans)
    end;
pretty(Other) ->
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
