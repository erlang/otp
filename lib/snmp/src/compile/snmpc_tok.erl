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

-module(snmpc_tok).

%% c(snmpc_tok).

%%----------------------------------------------------------------------
%% Generic (?) Tokenizer.
%%----------------------------------------------------------------------
%% Token: {Category, Line, Value}|{Category, Line}
%% (Category == <KeyWord> ==> 2-tuple (otherwise 3-tuple)
%% Category: integer | quote | string 
%%         | variable | atom | <keyword> | <single_char>
%%----------------------------------------------------------------------

%% API
-export([start_link/2, stop/1, get_token/1, get_all_tokens/1, tokenize/2]).

%% Internal exports
-export([null_get_line/0, format_error/1, terminate/2, handle_call/3, init/1,
	 test/0]).

-include("snmpc_lib.hrl").


%%----------------------------------------------------------------------
%% Reserved_words: list of KeyWords. Example: ['IF', 'BEGIN', ..., 'GOTO']
%% Options: list of Option
%% Option: {file, <filename>},
%% or:     {get_line_function, {Mod, Func, Arg} (default io, get_line, [Fid])},
%%         get_line_function shall behave as io:get_line.
%%         {print_lineno, L}
%% Returns: {ok, Pid} | {error, Reason}
%%----------------------------------------------------------------------
start_link(Reserved_words, Options) ->
    case lists:keysearch(file, 1, Options) of
	{value, {file, Filename}} ->
	    case file:open(Filename, [read]) of
		{ok, Fid} ->
		    gen_server:start_link(?MODULE,
					  {Reserved_words, Options,
					   {io, get_line, [Fid, prompt]}}, []);
		Error ->
		    Str = format_error({"Cannot open file '~s' (~800p).~n",
					[Filename, Error]}),
		    {error,Str}
	    end;
	false ->
	    MFA = case lists:keysearch(get_line_function, 1, Options) of
		      {value, {get_line_function, {M, F, A}}} ->
			  {M,F,A};
		      false -> {?MODULE, null_get_line, []}
		  end,
	    gen_server:start_link(?MODULE,{Reserved_words,Options,MFA}, [])
    end.
    
%%--------------------------------------------------
%% Returns:
%% {ok, [Token], LineNo} | {eof, LineNo} | {error, Error_description, Endline}
%% For more information, see manual page for yecc (and its requirements on a
%% tokenizer).
%%--------------------------------------------------
get_token(TokPid) ->
    V = gen_server:call(TokPid, get_token, infinity),
    %%  io:format("tok:~w~n", [V]),
    V.

get_all_tokens(TokPid) ->
    V = gen_server:call(TokPid, get_all_tokens, infinity),
    %%    io:format("tok:~w~n", [V]),
    V.



%%--------------------------------------------------
%% Returns: {ok, Tokens, EndLine} | {error, Error_description, Endline}
%% Comment: Tokeniser must be started since all options reside in
%%          the process dictionary of the tokeniser process.
%%--------------------------------------------------
tokenize(TokPid, String) ->
    gen_server:call(TokPid, {tokenize, String}, infinity).

stop(TokPid) ->
    gen_server:call(TokPid,stop, infinity).

%%----------------------------------------------------------------------
%% Implementation
%%----------------------------------------------------------------------
insert_keywords_into_ets(_DB, []) -> done;
insert_keywords_into_ets(DB, [Word | T]) ->
    ets:insert(DB, {Word, reserved_word}),
    insert_keywords_into_ets(DB, T).

reserved_word(X) ->
    case ets:lookup(get(db), X) of
	[{X, reserved_word}] -> 
	    true;
	_ -> 
	    false
    end.

%% If you only need to tokenize strings
null_get_line() -> eof.

format_error({Format, Data}) ->
    io_lib:format(lists:append("Tokeniser error: ", Format), Data).

test() ->
    start_link(['AUGMENTS','BEGIN','CONTACT-INFO','DEFINITIONS','DEFVAL',
		'DESCRIPTION','DISPLAY-HINT','END','IDENTIFIER','IMPLIED',
		'INDEX','INTEGER','LAST-UPDATED','MAX-ACCESS','MODULE-IDENTITY',
		'NOTIFICATION-TYPE','OBJECT','OBJECT-IDENTITY','OBJECT-TYPE',
		'OBJECTS','ORGANIZATION','REFERENCE','REVISION',
		'SIZE','STATUS','SYNTAX','TEXTUAL-CONVENTION','UNITS',
		'current','deprecated','not-accessible','obsolete', 
		'read-create','read-only','read-write', 'IMPORTS', 'FROM',
		'MODULE-COMPLIANCE',
		'AGENT-CAPABILITIES',
		'PRODUCT-RELEASE',
		'SUPPORTS',
		'INCLUDES',
		'DisplayString', 
		'PhysAddress', 
		'MacAddress', 
		'TruthValue', 
		'TestAndIncr', 
		'AutonomousType', 
		'InstancePointer', 
		'VariablePointer', 
		'RowPointer', 
		'RowStatus', 
		'TimeStamp', 
		'TimeInterval', 
		'DateAndTime', 
		'StorageType', 
		'TDomain', 
		'TAddress'],
	       [{file, "modemmib.mib"}]).

init({Reserved_words, Options, GetLineMFA}) ->
    put(get_line_function,GetLineMFA),
    put(print_lineno,
	case lists:keysearch(print_lineno, 1, Options) of
	    {value, {print_lineno, L}} -> L;
	    false -> undefined
	end),
    DB = ets:new(reserved_words, [set, private]),
    insert_keywords_into_ets(DB, Reserved_words),
    put(db, DB),
    put(line, 0),
    {ok, ""}.

getLine() ->
    OldLine = put(line, 1 + get(line)),
    {M,F,A} = get(get_line_function),
    case get(print_lineno) of
	undefined -> true;
	X -> case OldLine rem X of
		 0 -> io:format('~w..',[OldLine]);
		 _ -> true
	     end
    end,
    apply(M,F,A).

%% You can only do this when no file is open.
handle_call({tokenizeString, String}, _From, "") ->
    {reply, safe_tokenize_whole_string(String), ""};

handle_call(get_token, _From, String) ->
    {ReplyToken, RestChars} = safe_tokenise(String),
    {reply, ReplyToken, RestChars};

handle_call(get_all_tokens, _From, String) ->
    Toks = get_all_tokens(String,[]),
    {reply, Toks, []};


handle_call(stop, _From, String) ->
    {stop, normal, ok, String}.

terminate(_Reason, _State) ->
    ok.

%% ErrorInfo = {ErrorLine,  Module,  ErrorDescriptor}
%% will be used as 
%% apply(Module, format_error, [ErrorDescriptor]). shall return a string.

%% Returns a reply
tokenize_whole_string(eof) -> [];
tokenize_whole_string(String) ->
    {Token, RestChars} =  tokenise(String),
    [Token | tokenize_whole_string(RestChars)].

safe_tokenize_whole_string(String) ->
    case catch tokenize_whole_string(String) of
	{error, ErrorInfo} -> {error, ErrorInfo, get(line)};
	Tokens -> {ok, Tokens, get(line)}
    end.

%%    throw({error, {get(line), ?MODULE, "Unexpected eof~n"}}).

%% Returns: {ReplyToken, NewState}
safe_tokenise(eof) -> {{eof, get(line)}, eof};
safe_tokenise(Chars) when is_list(Chars) ->
    case catch tokenise(Chars) of
	{error, ErrorInfo} -> {{error, ErrorInfo, get(line)}, {[], eof}};
	{Token, RestChars} when is_tuple(Token) ->
	    {{ok, [Token], get(line)}, RestChars}
    end.

get_all_tokens(eof,Toks) -> 
    lists:reverse(Toks);
get_all_tokens(Str,Toks) ->
    case catch tokenise(Str) of
	{error, ErrorInfo} -> {error, ErrorInfo};
	{Token, RestChars} when is_tuple(Token) -> 
	    %% ?vtrace("get_all_tokens -> Token: ~p", [Token]),
	    get_all_tokens(RestChars, [Token|Toks])
    end.

    

%%--------------------------------------------------
%% Returns: {Token, Rest}
%%--------------------------------------------------
tokenise([H|T]) when ($a =< H) andalso (H =< $z) ->
    get_name(atom, [H], T);

tokenise([H|T]) when ($A =< H) andalso (H =< $Z) ->
    get_name(variable, [H], T);

tokenise([$:,$:,$=|T]) ->
    {{'::=', get(line)}, T};

tokenise([$-,$-|T]) ->
    tokenise(skip_comment(T));

tokenise([$-,H|T]) when ($0 =< H ) andalso (H =< $9) ->
    {Val, Rest} = get_integer(T, [H]),
    {{integer, get(line), -1 * Val}, Rest};

tokenise([H|T]) when ($0 =< H) andalso (H =< $9) ->
    {Val, Rest} = get_integer(T, [H]),
    {{integer, get(line), Val}, Rest};

tokenise([$"|T]) ->
    collect_string($", T, []);

tokenise([$'|T]) ->
    collect_string($', T, []);

%% Read away white spaces
tokenise([9| T]) -> tokenise(T);
tokenise([10| T]) -> tokenise(T);
tokenise([13| T]) -> tokenise(T);
tokenise([32| T]) -> tokenise(T);

%% Handle singe characters like { } [ ] + = ...
tokenise([Ch | T]) ->
    Atm = list_to_atom([Ch]),
    {{Atm, get(line)}, T};

tokenise([]) ->
    tokenise(getLine());

tokenise(eof) ->
    {{'$end', get(line)}, eof}.

collect_string($", [$"|T],BackwardsStr) ->
    {{string, get(line), BackwardsStr}, T};

collect_string($', [$'|T],BackwardsStr) ->
    {{quote, get(line), BackwardsStr}, T};

collect_string(StopChar, [Ch|T], Str) ->
    collect_string(StopChar,T,[Ch|Str]);

collect_string(StopChar, [],Str) ->
    collect_string(StopChar, getLine(), Str);

collect_string(StopChar, eof, Str) ->
    throw({error, {get(line), ?MODULE,
		   {"Missing ~s in string:~n \"~s\"~n",
		    [[StopChar], lists:reverse(Str)]}}}).

get_name(Category, Name, [Char|T]) ->
    case isInName(Char) of
	true ->
	    get_name(Category, [Char|Name], T);
	false ->
	    makeNameRespons(Category, Name, [Char | T])
    end;
get_name(Category, Name, []) ->
    makeNameRespons(Category, Name, []).

makeNameRespons(Category, Name, RestChars) ->
    Atm = list_to_atom(lists:reverse(Name)),
    case reserved_word(Atm) of
	true -> {{Atm, get(line)}, RestChars};
	false -> {{Category, get(line), Atm}, RestChars}
    end.

isInName($-) -> true;
isInName(Ch) -> isalnum(Ch).
	    
isalnum(H) when ($A =< H) andalso (H =< $Z) ->
    true;
isalnum(H) when ($a =< H) andalso (H =< $z) ->
    true;
isalnum(H) when ($0 =< H) andalso (H =< $9) ->
    true;
isalnum(_) ->
    false.

isdigit(H) when ($0 =< H) andalso (H =< $9) ->
    true;
isdigit(_) ->
    false.

get_integer([H|T], "0") ->
    case isdigit(H) of
	true ->
	    throw({error, {get(line), ?MODULE,
			   {"Unexpected ~w~n",
			    [list_to_atom([H])]}}});
	false ->
	    {0, [H|T]}
    end;
get_integer([H|T], L) ->
    case isdigit(H) of
	true ->
	    get_integer(T, [H|L]);
	false ->
	    {list_to_integer(lists:reverse(L)), [H|T]}
    end;
get_integer([], L) ->
    {list_to_integer(lists:reverse(L)), []}.

%%--------------------------------------------------
%% ASN.1 type of comments. "--" is comment to eoln or next "--"
%%--------------------------------------------------
skip_comment([]) ->
    [];
skip_comment([$-,$-|T]) ->
    T;
skip_comment([_|T]) ->
    skip_comment(T).
