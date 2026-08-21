%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(yeccscan).
-moduledoc false.
-export([scan/1, scan/3]).

scan(Inport) ->
    scan(Inport, '', {1, 1}).

scan(Inport, Prompt, Location1) ->
    case catch io:scan_erl_form(Inport, Prompt, Location1, [text, return]) of
	{eof, Location2} ->
	    {eof, Location2};
	{ok, Tokens, Location2} ->
            LexedTokens = lex(Tokens),
            ParsableTokens = [Token || Token <- LexedTokens,
                                       element(1, Token) =/= white_space,
                                       element(1, Token) =/= comment],
	    case ParsableTokens of
		[] ->
		    scan(Inport, Prompt, Location2);
		_ ->
		    {ok, LexedTokens, ParsableTokens, Location2}
	    end;
        {error, Reason} ->
            {error, Reason};
	{error, Descriptor, Location2} ->
	    {error, Descriptor, Location2};
	{'EXIT', Why} ->
	    io:format('yeccscan: Error scanning input line ~s~n',
                      [pos(Location1)]),
	    exit(Why)
    end.

pos({Line,Col}) ->
    io_lib:format("~w:~w", [Line, Col]);
pos(Line) ->
    io_lib:format("~w", [Line]).

lex([]) ->
    [];
lex([Token | Tokens]) ->
    case Token of
	{'dot', Location} ->
	    [{'dot', Location} | lex(Tokens)];
	{':', Location} ->
            [{':', Location} | lex(Tokens)];
        {'->', Location} ->
            [{'->', Location} | lex(Tokens)];
	{Category, Location, Symbol} ->
	    [{Category, Location, Symbol} | lex(Tokens)];
	{Other, Location} ->
            Cat = case erl_scan:reserved_word(Other) of
                      true -> reserved_word;
                      false -> reserved_symbol
                  end,
            [{Cat, Location, Other} | lex(Tokens)]
    end.
