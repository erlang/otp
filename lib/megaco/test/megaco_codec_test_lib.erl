%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
%% Purpose: Test library module for Megaco/H.248 encode/decode
%%----------------------------------------------------------------------

-module(megaco_codec_test_lib).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([
	 skip/1, 

	 display_text_messages/2, display_text_messages/3,
	 generate_text_messages/4,
	 test_msgs/6,

	 plain_decode_encode/5,
	 plain_encode_decode/5,
	 trans_first_encode_decode/5,
	 actions_first_encode_decode/5,
	 action_first_encode_decode/5,
	
	 encode_message/4,
	 decode_message/5, decode_message/6,

	 expect_instruction/3,
	 expect_encode/3, 
	 expect_encode_only/3, 
	 expect_encode_decode/4,
	 expect_encode_decode_only/4,
	 expect_decode/3, 
	 expect_decode_only/3, 
	 expect_decode_encode/4,
	 expect_decode_encode_only/4,
	 expect_exec/2
	]).


-record(expect_instruction, 
	{
	  %% Short description of what this instruction does 
	  description, % string()
	  
	  %% The actual instruction
	  command,     % function(Data) -> term()

	  %% Verification function of the instruction
	  verify       % function(Res, Data) -> {ok, NewData} | {error, Reason}
	  }
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_text_messages(V, Msgs) ->
    display_text_messages(V, [], Msgs).

display_text_messages(_, _, []) ->
    ok;
display_text_messages(V, EC, [{Name, Msg, _ED, _Conf}|Msgs]) ->
    (catch display_text_message(Name, EC, Msg, V)),
    display_text_messages(V, EC, Msgs).


display_text_message(Name, EC, Msg, V) when is_tuple(Msg) ->
    io:format("~n(Erlang) message ~p:~n~p~n", [Name, Msg]),
    case (catch megaco_pretty_text_encoder:encode_message(EC,V,Msg)) of
	{'EXIT', _R} ->
	    io:format("~nPretty encoded: failed (exit)~n", []);
	{error, {{deprecated, PWhat}, _}} ->
 	    io:format("~nPretty encoded: deprecated~n~p~n", [PWhat]),
	    throw(continue);
	{error, PReason} ->
 	    io:format("~nPretty encoded: failed (error)~n~p~n", [PReason]),
	    throw(continue);
	{ok, Pretty} ->
	    io:format("~nPretty encoded:~n~s~n", [binary_to_list(Pretty)])
    end,
    case (catch megaco_compact_text_encoder:encode_message(EC,V,Msg)) of
 	{'EXIT', _} ->
 	    io:format("~nCompact encoded: failed~n", []);
 	{error, {{deprecated, CWhat}, _}} ->
  	    io:format("~nPretty encoded: deprecated~n~p~n", [CWhat]);
 	{ok, Compact} ->
 	    io:format("~nCompact encoded:~n~s~n", [binary_to_list(Compact)])
    end;
display_text_message(_, _, _, _) ->
    skipping.

generate_text_messages(DirName, V, EC, Msgs) when is_atom(DirName) ->
    generate_text_messages(atom_to_list(DirName), V, EC, Msgs);
generate_text_messages(DirName, V, EC, Msgs) when is_list(DirName) ->
    DirPath = filename:join(["/tmp", DirName]),
    case file:make_dir(DirPath) of
	ok ->
	    generate_text_messages2(DirPath, V, EC, Msgs);
	{error, eexist} ->
	    generate_text_messages2(DirPath, V, EC, Msgs);
	{error, Reason} ->
	    io:format("Failed creating directory ~s: ~p~n", [DirPath, Reason]),
	    ok
    end.

generate_text_messages2(_, _, _, []) ->
    ok;
generate_text_messages2(Dir, V, EC, [{Name, Msg, _ED, _Conf}|Msgs]) ->
    (catch generate_text_message(Dir, Name, EC, Msg, V)),
    generate_text_messages2(Dir, V, EC, Msgs).

generate_text_message(Dir, Name, EC, Msg, V) ->
    io:format("~p: ", [Name]),
    case (catch megaco_pretty_text_encoder:encode_message(EC,V,Msg)) of
	{'EXIT', EReason} ->
	    io:format("failed encoding [exit]: ~n~p~n", [EReason]),
	    throw(continue);
	{error, {{deprecated, PWhat}, _}} ->
 	    io:format("failed encoding [deprecated]: ~n~p~n", [PWhat]),
	    throw(continue);
	{error, PReason} ->
 	    io:format("failed encoding [error]: ~n~p~n", [PReason]),
	    throw(continue);
	{ok, Pretty} ->
	    io:format("encoded", []),
	    FName = filename:flatten([Name, ".txt"]),
	    Filename = filename:join([Dir, FName]),
	    case (catch file:open(Filename, [write])) of
		{ok, Fd} ->
		    io:format(Fd, "~s", [binary_to_list(Pretty)]),
		    io:format(" - written to disk~n", []),
		    (catch file:close(Fd)),
		    ok;
		{error, OReason} ->
		    io:format(" - failed writing to disk: "
			      "~n~p~n~s~n", 
			      [OReason, binary_to_list(Pretty)]),
		    throw(continue)
	    end
    end.

test_msgs(Codec, DynamicDecode, Ver, EC, Check, Msgs) 
  when is_function(Check) andalso is_list(Msgs) ->
    io:format("~n", []),
    test_msgs(Codec, DynamicDecode, Ver, EC, Check, Msgs, []).

test_msgs(_Codec, _DD, _Ver, _EC, _Check, [], []) ->
    ok;
test_msgs(_Codec, _DD, _Ver, _EC, _Check, [], Errs) ->
    ?ERROR(lists:reverse(Errs));
test_msgs(Codec, DD, Ver, EC, Check, 
	  [{Name, {error, Error}, _ED, _Conf}|Msgs], Acc) ->
    io:format("error~n", []),
    test_msgs(Codec, DD, Ver, EC, Check, Msgs, [{Name, Error}|Acc]);
test_msgs(Codec, DD, Ver, EC, Check, 
	  [{Name, Msg, ED, Conf}|Msgs], Acc) ->
    Dbg = test_msgs_debug(Conf),
    put(dbg, Dbg),
    io:format("~-16w ", [Name]),
    case (catch encode_decode(ED, Check, Codec, DD, Ver, EC, Msg)) of
	ok ->
	    io:format("ok~n", []),
	    erase(dbg),
	    test_msgs(Codec, DD, Ver, EC, Check, Msgs, Acc);
	Error ->
	    io:format("error~n", []),
	    erase(dbg),
	    test_msgs(Codec, DD, Ver, EC, Check, Msgs, [{Name, Error}|Acc])
    end.

test_msgs_debug(Conf) ->
    case lists:keysearch(dbg, 1, Conf) of
	{value, {dbg, true}} ->
	    true;
	_ ->
	    false
    end.
    
encode_decode(Func, Check, Codec, DynamicDecode, Ver, EC, Msg1) 
  when is_function(Func) ->
    d("encode_decode -> entry with"
      "~n   Func:          ~p"
      "~n   Check:         ~p"
      "~n   Codec:         ~p"
      "~n   DynamicDecode: ~p"
      "~n   Ver:           ~p"
      "~n   EC:            ~p", 
      [Func, Check, Codec, DynamicDecode, Ver, EC]),
    case (catch Func(Codec, DynamicDecode, Ver, EC, Msg1)) of
	{ok, Msg1} ->
	    d("encode_decode -> expected result"),
	    ok;
	{ok, Msg2} ->
	    d("encode_decode -> unexpected result - check"),
	    case (catch Check(Msg1, Msg2)) of
		ok ->
		    d("encode_decode -> check - ok"),
		    ok;
		{error, Reason} ->
		    d("encode_decode -> check - error: "
		      "~n   Reason: ~p", [Reason]),
		    {error, {Reason, Msg1, Msg2}};
		Else ->
		    d("encode_decode -> check - failed: "
		      "~n   Else: ~p", [Else]),
		    {error, {invalid_check_result, Else}}
	    end;
	Else ->
	    d("encode_decode -> failed: "
	      "~n   Else: ~p", [Else]),
	    Else
    end.


%% *** plain_encode_decode ***

plain_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("plain_encode_decode -> entry with"
      "~n   Codec:         ~p"
      "~n   DynamicDecode: ~p"
      "~n   Ver:           ~p"
      "~n   EC:            ~p", [Codec, DynamicDecode, Ver, EC]),
    case (catch encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    d("plain_encode_decode -> encode - ok"),
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin, true);
	Error ->
	    d("plain_encode_decode -> encode - failed: "
	      "~n   Error: ~p", [Error]),
	    Error 
    end.


%% *** plain_decode_encode ***

plain_decode_encode(Codec, DynamicDecode, Ver, EC, M) when is_list(M) ->
    Bin = list_to_binary(M),
    plain_decode_encode(Codec, DynamicDecode, Ver, EC, Bin);
plain_decode_encode(Codec, DynamicDecode, Ver, EC, B) when is_binary(B) ->
    case (catch decode_message(Codec, DynamicDecode, Ver, EC, B, true)) of
	{ok, M} ->
	    encode_message(Codec, Ver, EC, M);
	Error ->
	    Error 
    end.


%% *** trans_first_encode_decode ***

trans_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("trans_first_encode_decode -> entry"),
    case (catch trans_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin, true);
	Error ->
	    Error 
    end.

trans_first_encode_message(Codec, Ver, EC, M1) ->
    d("trans_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_transactions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_transactions(Codec, Ver, EC, Trans) when is_list(Trans) ->
    d("encode_transactions -> entry"),
    [encode_transaction(Codec, Ver, EC, T) || T <- Trans].

encode_transaction(Codec, Ver, EC, T) ->
    d("encode_transaction -> entry"),
    case (catch Codec:encode_transaction(EC, Ver, T)) of
	{ok, EncodecTransactions} ->
	    EncodecTransactions;
	Error ->
	    throw({error, {transaction_encode_failed, Error, T}})
    end.


%% *** actions_first_encode_decode ***

actions_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("actions_first_encode_decode -> entry"),
    case (catch actions_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin, true);
	Error ->
	    Error 
    end.

actions_first_encode_message(Codec, Ver, EC, M1) ->
    d("actions_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_actions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_actions(Codec, Ver, EC, Trans) when is_list(Trans) ->
    d("encode_actions -> entry"),
    [encode_actions1(Codec, Ver, EC, T) || T <- Trans].

encode_actions1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    d("encode_actions1 -> entry"),
    #'TransactionRequest'{actions = ARs} = TR1,
    case (catch encode_action_requests(Codec, Ver, EC, ARs)) of
	{ok, EncodedARs} ->
	    TR2 = TR1#'TransactionRequest'{actions = EncodedARs},
	    {transactionRequest, TR2};
	Error ->
	    throw({error, {actions_encode_failed, Error, TR1}})
    end.

encode_action_requests(Codec, Ver, EC, ARs) ->
    d("encode_action_requests -> entry"),
    Codec:encode_action_requests(EC, Ver, ARs).


%% *** action_first_encode_decode ***

action_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("action_first_encode_decode -> entry"),
    case (catch action_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin, true);
	Error ->
	    Error 
    end.

action_first_encode_message(Codec, Ver, EC, M1) ->
    d("action_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_action(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_action(Codec, Ver, EC, Trans) when is_list(Trans) ->
    d("encode_action -> entry"),
    [encode_action1(Codec, Ver, EC, T) || T <- Trans].

encode_action1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    d("encode_action1 -> entry"),
    #'TransactionRequest'{actions = ARs1} = TR1,
    ARs2 = [encode_action_request(Codec, Ver, EC, AR) || AR <- ARs1],
    TR2  = TR1#'TransactionRequest'{actions = ARs2},
    {transactionRequest, TR2}.

encode_action_request(Codec, Ver, EC, AR) ->
    d("encode_action_request -> entry"),
    case (catch Codec:encode_action_request(EC, Ver, AR)) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    throw({error, {encode_action_request_failed, Error, AR}})
    end.


encode_message(Codec, Ver, EC, M) ->
    d("encode_message -> entry with"
      "~n   Codec: ~p"
      "~n   Ver:   ~p"
      "~n   EC:    ~p"
      "~n   M:     ~p", [Codec, Ver, EC, M]),    
%%     case (catch Codec:encode_message(EC, Ver, M)) of
%% 	{ok, Bin} ->
%% 	    d("encode_message -> encode - ok: "
%% 	      "~n~s", [binary_to_list(Bin)]),
%% 	    {ok, Bin};
%% 	Error ->
%% 	    d("encode_message -> encode - failed"),
%% 	    throw({error, {message_encode_failed, Error, M}})
%%     end.
    case (catch timer:tc(Codec, encode_message, [EC, Ver, M])) of
	{Time, {ok, Bin}} ->
	    d("encode_message -> encode - ok after ~p: "
	      "~n~s", [Time, binary_to_list(Bin)]),
	    {ok, Bin};
	{_Time, Error} ->
	    d("encode_message -> encode - failed"),
	    throw({error, {message_encode_failed, Error, M}})
    end.

decode_message(Codec, Dynamic, Ver, EC, M) ->
    decode_message(Codec, Dynamic, Ver, EC, M, false).

decode_message(Codec, true, _Ver, EC, M, _Timed) ->
    d("decode_message -> entry - when using dynamic"),
    Codec:decode_message(EC, dynamic, M);
decode_message(Codec, _, Ver, EC, M, false) ->
    d("decode_message -> entry with"
      "~n   Codec: ~p"
      "~n   Ver:   ~p"
      "~n   EC:    ~p", [Codec, Ver, EC]),
    Codec:decode_message(EC, Ver, M);
decode_message(Codec, _, Ver, EC, M, true) ->
    d("decode_message -> entry with"
      "~n   Codec: ~p"
      "~n   Ver:   ~p"
      "~n   EC:    ~p", [Codec, Ver, EC]),
    {Time, Result} = timer:tc(Codec, decode_message, [EC, Ver, M]),
    io:format("~-8w", [Time]),
    Result.


%% =======================================================================

%% ------------------------------------------------------------------
%% Create an instruction record
%% ------------------------------------------------------------------

expect_instruction(Desc, Cmd, Verify) 
  when is_list(Desc) andalso is_function(Cmd) andalso is_function(Verify) ->
    #expect_instruction{description = Desc,
			command     = Cmd,
			verify      = Verify}.
    

%% ------------------------------------------------------------------
%% Function:    expect_encode
%% Parameters:  Msg -> MegacoMessage
%%              Encode -> function/1
%%              Check -> function/1
%% Description: This function simply encodes, with the Encode fun, 
%%              and expects this to fail. The failure reason is 
%%              checked with the Check fun.
%% ------------------------------------------------------------------

expect_encode(InitialData, Encode, Check) 
  when is_function(Encode) andalso is_function(Check) ->
    Instructions = 
	[
	 %% Initial encode
	 expect_instruction(
	   "Encode (initial) message",
	   fun(Msg) when is_record(Msg, 'MegacoMessage') ->
		   (catch Encode(Msg));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({error, Reason}, _) ->
		   io:format("check error reason ", []),
		   case (catch Check(Reason)) of
		       ok ->
			   {ok, done};
		       Error ->
			   Error
		   end;
	      ({ok, Bin}, Msg) when is_binary(Bin) ->
		   M = binary_to_list(Bin), 
		   {error, {unexpected_encode_success, {M, Msg}}};
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_encode_only
%% Parameters:  InitialData -> list() | binary()
%%              Encode -> function/1
%%              Check -> function/1
%% Description: This function simply encodes, with the Encode fun, 
%%              and expects it to succeed, which is checked by 
%%              calling the Check fun with the resulting message.
%% ------------------------------------------------------------------

expect_encode_only(InitialData, Encode, Check) 
  when is_function(Encode) andalso is_function(Check) ->
    Instructions = 
	[
	 %% Initial encode
	 expect_instruction(
	   "Encode (initial) message",
	   fun(Msg) when is_record(Msg, 'MegacoMessage') ->
		   (catch Encode(Msg));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Bin}, _Msg) when is_binary(Bin) ->
		   case (catch Check(Bin)) of
		       ok ->
			   {ok, done};
		       Error ->
			   Error
		   end;
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_encode_decode
%% Parameters:  InitialData -> MegacoMessage
%%              Encode -> function/1
%%              Decode -> function/1
%%              Check -> function/2
%% Description: This function simply encodes, with the Encode fun, and 
%%              then decodes, with the Decode fun, the megaco message. 
%%              The resulting message should be identical, but if it 
%%              is not, the messages are checked, with the Check fun.
%% ------------------------------------------------------------------

expect_encode_decode(InitialData, Encode, Decode, Check) 
  when is_function(Encode) andalso 
       is_function(Decode) andalso 
       is_function(Check) ->
    Instructions = 
	[
	 %% Initial encode
	 expect_instruction(
	   "Encode (initial) message",
	   fun(M) when is_record(M, 'MegacoMessage') ->
		   (catch Encode(M));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Bin}, M) when is_binary(Bin) ->
		   {ok, {Bin, M}};
	      ({error, Reason}, _) ->
		   {error, {unexpected_encode_failure, Reason}};
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end),
	 
	 %% Decode the (encoded) message
	 expect_instruction(
	   "Decode message", 
	   fun({Bin, _}) when is_binary(Bin) ->
		   (catch Decode(Bin));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg1}, {_Bin, Msg1}) 
	      when is_record(Msg1, 'MegacoMessage') ->
		   io:format("messages identical - done ", []),
		   {ok, done};
	      ({ok, Msg2}, {_Bin, Msg1}) ->
		   io:format("messages not identical - check - ", []),
		   case (catch Check(Msg1, Msg2)) of
		       ok ->
			   io:format("equal ", []),
			   {ok, done};
		       Error ->
			   io:format("not equal ", []),
			   io:format("~nError: ~p~n", [Error]),
			   Error
		   end;
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_encode_decode_only
%% Parameters:  InitialData -> MegacoMessage
%%              Encode -> function/1
%%              Decode -> function/1
%%              Check -> function/2
%% Description: This function simply encodes, with the Encode fun, 
%%              and then decodes, with the Decode fun, the megaco 
%%              message and expects it to succeed. The resulting 
%%              message is checked by calling the Check fun with the 
%%              resulting message.
%% ------------------------------------------------------------------

expect_encode_decode_only(InitialData, Encode, Decode, Check) 
  when is_function(Encode) andalso 
       is_function(Decode) andalso 
       is_function(Check) ->
    Instructions = 
	[
	 %% Initial encode
	 expect_instruction(
	   "Encode (initial) message",
	   fun(M) when is_record(M, 'MegacoMessage') ->
		   (catch Encode(M));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Bin}, M) when is_binary(Bin) ->
		   {ok, {Bin, M}};
	      ({error, Reason}, _) ->
		   {error, {unexpected_encode_failure, Reason}};
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end),
	 
	 %% Decode the (encoded) message
	 expect_instruction(
	   "Decode message", 
	   fun({Bin, _}) when is_binary(Bin) ->
		   (catch Decode(Bin));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, _B) when is_record(Msg, 'MegacoMessage') ->
		   io:format("decoded - now check ", []),
		   case (catch Check(Msg)) of
		       ok ->
			   {ok, done};
		       Error ->
			   Error
		   end;
	      ({error, R}, _) ->
		   {Line, Mod, Reason} = 
		       case lists:keysearch(reason, 1, R) of
			   {value, {reason, {L, M, Raw}}} 
			   when is_list(Raw) ->
			       {L, M, lists:flatten(Raw)};
			   {value, {reason, {L, M, Raw}}} ->
			       {L, M, Raw};
			   _ ->
			       {-1, undefined, R}
		       end,
		   Tokens = 
		       case lists:keysearch(token, 1, R) of
			   {value, {token, T}} ->
			       T;
			   _ ->
			       undefined
		       end,
		   {error, {unexpected_decode_failure, 
			    {Mod, Line, Reason, Tokens}}};
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_decode
%% Parameters:  InitialData -> list() | binary()
%%              Decode -> function/1
%%              Check -> function/1
%% Description: This function simply decodes, with the Decode fun, 
%%              and expects this to fail. The failure reason is 
%%              checked with the Check fun.
%% ------------------------------------------------------------------

expect_decode(InitialData, Decode, Check) 
  when is_list(InitialData) ->
    expect_decode(list_to_binary(InitialData), Decode, Check);
expect_decode(InitialData, Decode, Check) 
  when is_function(Decode) andalso is_function(Check) ->
    Instructions = 
	[
	 %% Initial decode
	 expect_instruction(
	   "Decode (initial) message",
	   fun(Bin) when is_binary(Bin) ->
		   (catch Decode(Bin));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({error, Reason}, _) ->
		   io:format("check error reason - ", []),
		   case (catch Check(Reason)) of
		       ok ->
			   {ok, done};
		       Error ->
			   Error
		   end;
	      ({ok, Msg}, Bin) ->
		   io:format("unexpected decode success - ", []),
		   M = binary_to_list(Bin),
		   {error, {unexpected_decode_success, {Msg, M}}};
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_decode_only
%% Parameters:  InitialData -> list() | binary()
%%              Decode -> function/1
%%              Check -> function/2
%% Description: This function simply decodes, with the Decode fun, 
%%              and expects it to succeed, which is checked by 
%%              calling the Check fun with the resulting message.
%% ------------------------------------------------------------------

expect_decode_only(InitialData, Decode, Check) 
  when is_list(InitialData) ->
    expect_decode_only(list_to_binary(InitialData), Decode, Check);
expect_decode_only(InitialData, Decode, Check) 
  when is_function(Decode) andalso is_function(Check) ->
    Instructions = 
	[
	 %% Initial decode
	 expect_instruction(
	   "Decode (initial) message",
	   fun(B) when is_binary(B) ->
		   (catch Decode(B));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, _B) when is_record(Msg, 'MegacoMessage') ->
		   case (catch Check(Msg)) of
		       ok ->
			   {ok, done};
		       Error ->
			   Error
		   end;
	      ({error, R}, _) ->
		   {Line, Mod, Reason} = 
		       case lists:keysearch(reason, 1, R) of
			   {value, {reason, {L, M, Raw}}} 
			   when is_list(Raw) ->
			       {L, M, lists:flatten(Raw)};
			   {value, {reason, {L, M, Raw}}} ->
			       {L, M, Raw};
			   _ ->
			       {-1, undefined, R}
		       end,
		   Tokens = 
		       case lists:keysearch(token, 1, R) of
			   {value, {token, T}} ->
			       T;
			   _ ->
			       undefined
		       end,
		   {error, {unexpected_decode_failure, 
			    {Mod, Line, Reason, Tokens}}};
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_decode_encode
%% Parameters:  InitialData -> list() | binary()
%%              Decode -> function/1
%%              Encode -> function/1
%%              Check -> function/2
%% Description: This function simply decodes, with the Decode fun, 
%%              and then encodes, with the Encode fun, the megaco 
%%              message. The resulting binary message should be 
%%              identical, but if it is not, the messages are 
%%              decoded again and then if necessary checked, with 
%%              the Check fun.
%% ------------------------------------------------------------------

expect_decode_encode(InitialData, Decode, Encode, Check) 
  when is_list(InitialData) ->
    expect_decode_encode(list_to_binary(InitialData), Decode, Encode, Check);
expect_decode_encode(InitialData, Decode, Encode, Check) 
  when is_function(Decode) andalso 
       is_function(Encode) andalso 
       is_function(Check) ->
    Instructions = 
	[
	 %% Initial decode
	 expect_instruction(
	   "Decode (initial) message",
	   fun(B) when is_binary(B) ->
		   (catch Decode(B));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, B) when is_record(Msg, 'MegacoMessage') ->
		   {ok, {Msg, B}};
	      ({error, R}, _) ->
		   {Line, Mod, Reason} = 
		       case lists:keysearch(reason, 1, R) of
			   {value, {reason, {L, M, Raw}}} 
			   when is_list(Raw) ->
			       {L, M, lists:flatten(Raw)};
			   {value, {reason, {L, M, Raw}}} ->
			       {L, M, Raw};
			   _ ->
			       {-1, undefined, R}
		       end,
		   Tokens = 
		       case lists:keysearch(token, 1, R) of
			   {value, {token, T}} ->
			       T;
			   _ ->
			       undefined
		       end,
		   {error, {unexpected_decode_failure, 
			    {Mod, Line, Reason, Tokens}}};
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end),
	 
	 
	 %% Encode the (decoded) message
	 expect_instruction(
	   "Encode message", 
	   fun({Msg, _Bin}) when is_record(Msg, 'MegacoMessage') -> 
		   (catch Encode(Msg));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, B}, {_, B}) ->
		   io:format("binaries equal - done ", []),
		   {ok, done};
	      ({ok, B}, {Msg, _}) ->
		   {ok, {Msg, B}};
	      ({error, Reason}, _) ->
		   {error, {unexpected_encode_failure, Reason}};
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end),
	 
	 
	 %% Fallback instruction in case encode produced
	 %% a binary not equal to the initial
	 expect_instruction(
	   "Decode message (if binaries not equal)", 
	   fun(done) ->
		   done;
	      ({_Msg, B}) when is_binary(B) ->
		   (catch Decode(B));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, {Msg, _Bin}) when is_record(Msg, 'MegacoMessage') ->
		   io:format("messages identical - done ", []),
		   {ok, done};
	       (done, _) ->
		   io:format("done ", []),
		   {ok, done};
	       ({ok, Msg2}, {Msg1, _}) ->
		   io:format("messages not identical - check - ", []),
		   case (catch Check(Msg1, Msg2)) of
		       ok ->
			   io:format("equal ", []),
			   {ok, done};
		       Error ->
			   io:format("not equal ", []),
			   Error
		   end;
	       ({error, Reason}, _) ->
		      {error, {unexpected_decode_failure, Reason}};
	       (Crap, _) ->
		      {error, {unexpected_decode_result, Crap}}
	      end)
	],
    expect_exec(Instructions, InitialData).


%% ------------------------------------------------------------------
%% Function:    expect_decode_encode_only
%% Parameters:  InitialData -> list() | binary()
%%              Decode -> function/1
%%              Encode -> function/1
%%              Check -> function/2
%% Description: This function simply decodes, with the Decode fun, 
%%              and then encodes, with the Encode fun, the megaco 
%%              message. The resulting binary message is then checked
%%              with the Check fun.
%% ------------------------------------------------------------------

expect_decode_encode_only(InitialData, Decode, Encode, Check) 
  when is_list(InitialData) ->
    expect_decode_encode_only(list_to_binary(InitialData), 
			      Decode, Encode, Check);
expect_decode_encode_only(InitialData, Decode, Encode, Check) 
  when is_function(Decode) andalso 
       is_function(Encode) andalso 
       is_function(Check) ->
    Instructions = 
	[
	 %% Initial decode
	 expect_instruction(
	   "Decode (initial) message",
	   fun(B) when is_binary(B) ->
		   (catch Decode(B));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, Msg}, B) when is_record(Msg, 'MegacoMessage') ->
		   {ok, {Msg, B}};
	      ({error, R}, _) ->
		   {Line, Mod, Reason} = 
		       case lists:keysearch(reason, 1, R) of
			   {value, {reason, {L, M, Raw}}} 
			   when is_list(Raw) ->
			       {L, M, lists:flatten(Raw)};
			   {value, {reason, {L, M, Raw}}} ->
			       {L, M, Raw};
			   _ ->
			       {-1, undefined, R}
		       end,
		   Tokens = 
		       case lists:keysearch(token, 1, R) of
			   {value, {token, T}} ->
			       T;
			   _ ->
			       undefined
		       end,
		   {error, {unexpected_decode_failure, 
			    {Mod, Line, Reason, Tokens}}};
	      (Crap, _) ->
		   {error, {unexpected_decode_result, Crap}}
	   end),
	 
	 
	 %% Encode the (decoded) message
	 expect_instruction(
	   "Encode message", 
	   fun({Msg, _Bin}) when is_record(Msg, 'MegacoMessage') -> 
		   (catch Encode(Msg));
	      (Bad) ->
		   {error, {invalid_data, Bad}}
	   end,
	   fun({ok, B2}, {_, B1}) ->
		   io:format("encode ok - check bins - ", []),
		   case (catch Check(B1, B2)) of
		       ok ->
			   {ok, done};
		       Crap ->
			   {error, {unexpected_encode_check_result, Crap}}
		   end;
	      ({error, Reason}, _) ->
		   {error, {unexpected_encode_failure, Reason}};
	      (Crap, _) ->
		   {error, {unexpected_encode_result, Crap}}
	   end)
	],
    expect_exec(Instructions, InitialData).



%% ------------------------------------------------------------------
%% Function:    expect_exec
%% Parameters:  Instructions -> [instruction()]
%%              InitialData -> term()
%% Description: This function is the engine in the codec test 
%%              cases. It executes each instruction in turn.
%% ------------------------------------------------------------------

expect_exec(Instructions, InitialData) ->
    expect_exec(Instructions, InitialData, 1).

expect_exec([], _, _) ->
    io:format("~n", []),
    ok;
expect_exec([#expect_instruction{description = Desc, 
				 command     = Cmd, 
				 verify      = Verify}|T], Data, Num) ->
    io:format("~n   Exec command ~w: ~s => ", [Num, Desc]),
    case Verify((catch Cmd(Data)), Data) of
	{ok, NewData} ->
	    io:format("ok", []),
	    expect_exec(T, NewData, Num+1);
	{error, Reason} ->
	    io:format("error", []),
	    {error, {Num, Desc, Reason}}
    end.

%% =======================================================================

skip({What, Why}) when is_atom(What) andalso is_list(Why) ->
    Reason = lists:flatten(io_lib:format("~p: ~s", [What, Why])),
    ?SKIP(Reason);
skip({What, Why}) ->
    Reason = lists:flatten(io_lib:format("~p: ~p", [What, Why])),
    ?SKIP(Reason);
skip(Reason) when is_list(Reason) ->
    ?SKIP(Reason);
skip(Reason1) ->
    Reason2 = lists:flatten(io_lib:format("~p", [Reason1])),
    ?SKIP(Reason2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------


d(F) ->
    d(F, []).

d(F, A) ->
    d(get(dbg), F, A).

d(true, F, A) ->
    io:format("DBG:~w:" ++ F ++ "~n", [?MODULE|A]);
d(_, _, _) ->
    ok.

