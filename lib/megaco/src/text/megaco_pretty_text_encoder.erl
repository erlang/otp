%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2000-2025. All Rights Reserved.
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
%%% Purpose: Encode PRETTY Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_pretty_text_encoder).
-moduledoc false.

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,
	 decode_mini_message/3, 

	 version_of/2, 

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 encode_command_request/3,
	 encode_action_reply/3]).

%% Backward compatible funcs:
-export([encode_message/2, decode_message/2,

	 encode_transaction/1,
	 encode_command_request/1,
	 encode_action_reply/1]).

%% Do we need these here?
-export([trim_quoted_string/1,
	 term_to_compact_string/1,
	 term_to_pretty_string/1]).

-export([token_tag2string/1, token_tag2string/2]).


-include("megaco_text_tokens.hrl").
-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(V1_PARSE_MOD,     megaco_text_parser_v1).
-define(V2_PARSE_MOD,     megaco_text_parser_v2).
-define(V3_PARSE_MOD,     megaco_text_parser_v3).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EncodingConfig, V, MegaMsg).


%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,_}|EC], 1, MegaMsg) ->
    megaco_pretty_text_encoder_v1:encode_message(EC, MegaMsg);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 1, MegaMsg) ->
    megaco_pretty_text_encoder_v1:encode_message(EC, MegaMsg);

%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,_}|EC], 2, MegaMsg) ->
    megaco_pretty_text_encoder_v2:encode_message(EC, MegaMsg);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 2, MegaMsg) ->
    megaco_pretty_text_encoder_v2:encode_message(EC, MegaMsg);

%% <BACKWARD-COMPAT-CLAUSE>
encode_message([{version3,v3}|EC], 3, MegaMsg) ->
    megaco_pretty_text_encoder_v3:encode_message(EC, MegaMsg);
%% </BACKWARD-COMPAT-CLAUSE>

encode_message(EC, 3, MegaMsg) ->
    megaco_pretty_text_encoder_v3:encode_message(EC, MegaMsg);
encode_message(_EC, V, _MegaMsg) ->
    {error, {bad_version, V}}.
	    

%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

version_of(_EC, Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, _Tokens, V, _LastLine} ->
	    {ok, V};
	{error, Reason, Line} ->
	    {error, {decode_failed, Reason, Line}}
    end.


decode_message(EC, Bin) ->
    decode_message(EC, dynamic, Bin).

decode_message([], _, Bin) when is_binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{ok, _Tokens, V, _LastLine} ->
	    {error, {unsupported_version, V}};

	%% {error, Reason, Tokens, Line} ->
	%%     scan_error(Reason, Line, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    scan_error(Reason, Line, Bin) %% OTP-4007
    end;

%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,v3}], _, Bin) when is_binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{ok, _Tokens, V, _LastLine} ->
	    {error, {unsupported_version, V}};

	{error, Reason, Tokens, Line} ->
	    scan_error(Reason, Line, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    scan_error(Reason, Line, Bin) %% OTP-4007
    end;
%% </BACKWARD-COMPAT-CLAUSE>

decode_message([{flex, Port}], _, Bin) when is_binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{ok, _Tokens, V, _LastLine} ->
	    {error, {unsupported_version, V}};

	%% {error, Reason, Tokens, Line} ->
	%%     scan_error(Reason, Line, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    scan_error(Reason, Line, Bin) %% OTP-4007
    end;

%% <BACKWARD-COMPAT-CLAUSE>
decode_message([{version3,v3},{flex, Port}], _, Bin) when is_binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{ok, _Tokens, V, _LastLine} ->
	    {error, {unsupported_version, V}};

	%% {error, Reason, Tokens, Line} ->
	%%     scan_error(Reason, Line, Tokens, Bin);

	{error, Reason, Line} ->          %% OTP-4007
	    scan_error(Reason, Line, Bin) %% OTP-4007
    end;
%% </BACKWARD-COMPAT-CLAUSE>

decode_message(EC, _, Bin) when is_binary(Bin) ->
    {error, {bad_encoding_config, EC}};
decode_message(_EC, _, _BadBin) ->
    {error, bad_binary}.


do_decode_message(ParseMod, Tokens, Bin) ->
    case (catch ParseMod:parse(Tokens)) of
	{ok, MegacoMessage} ->
	    {ok, MegacoMessage};
	{error, Reason} ->
	    parse_error(Reason, Tokens, Bin);

	%% OTP-4007
	{'EXIT', Reason} ->
	    parse_error(Reason, Tokens, Bin)
    end.


decode_mini_message(EC, _, Bin) when is_binary(Bin) ->
    megaco_text_mini_decoder:decode_message(EC, Bin).


scan_error(Reason, Line, Bin) ->
    scan_error(Reason, Line, [], Bin).

scan_error("bad_property_parm: " ++ Reason, _Line, _Tokens, _Bin) ->
    {error, {bad_property_parm, Reason}};
scan_error(Reason, Line, Tokens, Bin) ->
    %%     io:format("scanner error: "
    %% 	      "~n   Reason: ~p"
    %% 	      "~n   Line:   ~p"
    %% 	      "~n   Tokens: ~p"
    %% 	      "~n   Bin:    ~p"
    %% 	      "~n", [Reason, Line, Tokens, Bin]),
    {error, [{reason, Reason, Line}, {token, Tokens}, {chars, Bin}]}.

parse_error(Reason, Tokens, Chars) ->
%%     io:format("parse_error -> entry with"
%%               "~n   Reason: ~p"
%% 	      "~n   Tokens: ~p"
%% 	      "~n   Chars:  ~p"
%% 	      "~n", [Reason, Tokens, Chars]),
    case Reason of
	{Line, Mod, [Prefix, [$[, TokenStringRaw, $]]]} when 
	      is_integer(Line) andalso 
	      is_atom(Mod) andalso 
	      is_list(Prefix) andalso 
	      is_list(TokenStringRaw) ->
	    TokenString = [l2i(X) || X <- TokenStringRaw, is_list(X)],
	    ReasonStr = Prefix ++ TokenString,
	    {error, [{reason, ReasonStr, Line}, {tokens, Tokens}, {chars, Chars}, {module, Mod}]};
	_ ->
	    {error, [{reason, Reason}, {token, Tokens}, {chars, Chars}]}
    end.


l2i(L) when is_list(L) ->
    case (catch list_to_integer(L)) of
	I when is_integer(I) ->
	    I;
	_ ->
	    L
    end.


%%----------------------------------------------------------------------
%% Convert a transaction record into a binary
%% Return {ok, Bin} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction(Trans) ->
    encode_transaction([], 1, Trans).

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,_}|EC], 1, Trans) ->
    megaco_pretty_text_encoder_v1:encode_transaction(EC, Trans);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 1, Trans) ->
    megaco_pretty_text_encoder_v1:encode_transaction(EC, Trans);

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,_}|EC], 2, Trans) ->
    megaco_pretty_text_encoder_v2:encode_transaction(EC, Trans);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 2, Trans) ->
    megaco_pretty_text_encoder_v2:encode_transaction(EC, Trans);

%% <BACKWARD-COMPAT-CLAUSE>
encode_transaction([{version3,v3}|EC], 3, Trans) ->
    megaco_pretty_text_encoder_v3:encode_transaction(EC, Trans);
%% </BACKWARD-COMPAT-CLAUSE>

encode_transaction(EC, 3, Trans) ->
    megaco_pretty_text_encoder_v3:encode_transaction(EC, Trans);
encode_transaction(_EC, V, _Trans) ->
    {error, {bad_version, V}}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,_}|EC], 1, ActReqs) when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v1:encode_action_requests(EC, ActReqs);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 1, ActReqs) when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v1:encode_action_requests(EC, ActReqs);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,_}|EC], 2, ActReqs) when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v2:encode_action_requests(EC, ActReqs);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 2, ActReqs) when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v2:encode_action_requests(EC, ActReqs);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_requests([{version3,v3}|EC], 3, ActReqs) 
  when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v3:encode_action_requests(EC, ActReqs);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_requests(EC, 3, ActReqs) when is_list(ActReqs) ->
    megaco_pretty_text_encoder_v3:encode_action_requests(EC, ActReqs);
encode_action_requests(_EC, V, _ActReqs) ->
    {error, {bad_version, V}}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,_}|EC], 1, ActReq) ->
    megaco_pretty_text_encoder_v1:encode_action_request(EC, ActReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 1, ActReq) ->
    megaco_pretty_text_encoder_v1:encode_action_request(EC, ActReq);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,_}|EC], 2, ActReq) ->
    megaco_pretty_text_encoder_v2:encode_action_request(EC, ActReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 2, ActReq) ->
    megaco_pretty_text_encoder_v2:encode_action_request(EC, ActReq);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_request([{version3,v3}|EC], 3, ActReq) ->
    megaco_pretty_text_encoder_v3:encode_action_request(EC, ActReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_request(EC, 3, ActReq) ->
    megaco_pretty_text_encoder_v3:encode_action_request(EC, ActReq);
encode_action_request(_EC, V, _ActReq) ->
    {error, {bad_version, V}}.


%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request(CmdReq) ->
    encode_command_request([], 1, CmdReq).

%% <BACKWARD-COMPAT-CLAUSE>
encode_command_request([{version3,_}|EC], 1, CmdReq) ->
    megaco_pretty_text_encoder_v1:encode_command_request(EC, CmdReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_command_request(EC, 1, CmdReq) ->
    megaco_pretty_text_encoder_v1:encode_command_request(EC, CmdReq);

%% <BACKWARD-COMPAT-CLAUSE>
encode_command_request([{version3,_}|EC], 2, CmdReq) ->
    megaco_pretty_text_encoder_v2:encode_command_request(EC, CmdReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_command_request(EC, 2, CmdReq) ->
    megaco_pretty_text_encoder_v2:encode_command_request(EC, CmdReq);

%% <BACKWARD-COMPAT-CLAUSE>
encode_command_request([{version3,v3}|EC], 3, CmdReq) ->
    megaco_pretty_text_encoder_v3:encode_command_request(EC, CmdReq);
%% </BACKWARD-COMPAT-CLAUSE>

encode_command_request(EC, 3, CmdReq) ->
    megaco_pretty_text_encoder_v3:encode_command_request(EC, CmdReq);
encode_command_request(_EC, V, _CmdReq) ->
    {error, {bad_version, V}}.


%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply(ActRep) ->
%%     io:format("~p:encode_action_reply -> entry with"
%% 	      "~n   ActRep: ~p"
%% 	      "~n", [?MODULE, ActRep]),
    encode_action_reply([], 1, ActRep).
				       
%% <BACKWARD-COMPAT-CLAUSE>
encode_action_reply([{version3,_}|EC], 1, ActRep) ->
    megaco_pretty_text_encoder_v1:encode_action_reply(EC, ActRep);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_reply(EC, 1, ActRep) ->
    megaco_pretty_text_encoder_v1:encode_action_reply(EC, ActRep);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_reply([{version3,_}|EC], 2, ActRep) ->
    megaco_pretty_text_encoder_v2:encode_action_reply(EC, ActRep);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_reply(EC, 2, ActRep) ->
    megaco_pretty_text_encoder_v2:encode_action_reply(EC, ActRep);

%% <BACKWARD-COMPAT-CLAUSE>
encode_action_reply([{version3,v3}|EC], 3, ActRep) ->
    megaco_pretty_text_encoder_v3:encode_action_reply(EC, ActRep);
%% </BACKWARD-COMPAT-CLAUSE>

encode_action_reply(EC, 3, ActRep) ->
    megaco_pretty_text_encoder_v3:encode_action_reply(EC, ActRep);
encode_action_reply(_EC, V, _ActRep) ->
    {error, {bad_version, V}}.


%%----------------------------------------------------------------------
term_to_compact_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~w", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
term_to_pretty_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~p", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
trim_quoted_string([H | T]) ->
    case ?classify_char(H) of
	safe_char   -> [H  | trim_quoted_string(T)];
	rest_char   -> [H  | trim_quoted_string(T)];
	white_space -> [H  | trim_quoted_string(T)];
	_BadChar     -> [$? | trim_quoted_string(T)]
    end;
trim_quoted_string([]) ->
    [].


%%----------------------------------------------------------------------
%% A utility function to pretty print the tags found in a megaco message
%%----------------------------------------------------------------------

-define(TT2S_BEST_VERSION, v3).

token_tag2string(Tag) ->
    token_tag2string(Tag, ?TT2S_BEST_VERSION).

token_tag2string(Tag, 1) ->
    token_tag2string(Tag, v1);
token_tag2string(Tag, v1) ->
    megaco_pretty_text_encoder_v1:token_tag2string(Tag);
token_tag2string(Tag, 2) ->
    token_tag2string(Tag, v2);
token_tag2string(Tag, v2) ->
    megaco_pretty_text_encoder_v2:token_tag2string(Tag);
token_tag2string(Tag, 3) ->
    token_tag2string(Tag, v3);
token_tag2string(Tag, v3) ->
    megaco_pretty_text_encoder_v3:token_tag2string(Tag);

token_tag2string(Tag, _Vsn) ->
    token_tag2string(Tag, ?TT2S_BEST_VERSION).

