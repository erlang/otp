%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
%%% Purpose: Encode PRETTY Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_text_mini_decoder).

-export([decode_message/2]).

-include("megaco_text_tokens.hrl").
-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% Convert a binary into a mini 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(_, Bin) when is_binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, _Vsn, _LastLine} ->
	    decode_message(Tokens);

	{error, Reason, Line} ->
	    parse_error(Reason, Line, []) 
    end;
decode_message(_EC, _BadBin) ->
    {error, bad_binary}.


decode_message(Tokens0) ->
    Tokens = strip(Tokens0, []),
    case (catch megaco_text_mini_parser:parse(Tokens)) of
	{ok, MegacoMessage} ->
	    {ok, MegacoMessage};
	{error, Reason} ->
	    parse_error(Reason, Tokens);

	%% OTP-4007
	{'EXIT', Reason} ->
	    parse_error(Reason, Tokens)
    end.

strip([], Tokens) ->
    lists:reverse(Tokens);
strip([{'TransToken', _Line, _Text}|_], Acc) ->
    strip_finish(Acc);
strip([{'ReplyToken', _Line, _Text}|_], Acc) ->
    strip_finish(Acc);
strip([{'PendingToken', _Line, _Text}|_], Acc) ->
    strip_finish(Acc);
strip([{'ResponseAckToken', _Line, _Text}|_], Acc) ->
    strip_finish(Acc);
strip([{'ErrorToken', _Line, _Text}|_], Acc) ->
    strip_finish(Acc);
strip([H|T], Acc) ->
    strip(T, [H|Acc]).

strip_finish(RevTokens) ->
    lists:reverse([{endOfMessage, 1, endOfMessage}|RevTokens]).


parse_error(Reason, Tokens) ->
    {error, [{reason, Reason}, {token, Tokens}]}.

parse_error(Reason, Line, Tokens) ->
    {error, [{reason, Reason, Line}, {token, Tokens}]}.


