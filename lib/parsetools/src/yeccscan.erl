%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-export([scan/1, scan/3]).

scan(Inport) ->
    scan(Inport, '', 1).

scan(Inport, Prompt, Line1) ->
    case catch io:scan_erl_form(Inport, Prompt, Line1) of
	{eof, Line2} ->
	    {eof, Line2};
	{ok, Tokens, Line2} ->
	    case Tokens of
		[] ->
		    scan(Inport, Prompt, Line2);
		_ ->
		    {ok, lex(Tokens), Line2}
	    end;
        {error, Reason} ->
            {error, Reason};
	{error, Descriptor, Line2} ->
	    {error, Descriptor, Line2};
	{'EXIT', Why} ->
	    io:format('yeccscan: Error scanning input line ~w~n', [Line1]),
	    exit(Why)
    end.

lex([]) ->
    [];
lex([Token | Tokens]) ->
    case Token of
	{'dot', Line} ->
	    [{'dot', Line} | lex(Tokens)];
	{':', Line} ->
            [{':', Line} | lex(Tokens)];
        {'->', Line} ->
            [{'->', Line} | lex(Tokens)];
	{Category, Line, Symbol} ->
	    [{Category, Line, Symbol} | lex(Tokens)];
	{Other, Line} ->
            Cat = case erl_scan:reserved_word(Other) of
                      true -> reserved_word;
                      false -> reserved_symbol
                  end,
            [{Cat, Line, Other} | lex(Tokens)]
    end.
