%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ic_pp).

-export([run/2]).

-define(is_number(X), X >= $0, X =< $9).
-define(is_upper(X), X >= $A, X =< $Z).
-define(is_lower(X), X >= $a, X =< $z).
-define(is_underline(X), X == $_).
-define(is_tab(X), X == 9).
-define(is_space(X), X == 32).
-define(tab, 9).
-define(space, 32).


%%======================================================================================
%%======================================================================================
%%======================================================================================
%% Preprocessor
%%
%% This preprocessor is equivalent to the gcc-preprocessor. It takes a file name and 
%% a list of preprocessor flags as an input and returns a processed text file.
%%
%% The processing is done in two phases. 
%% In the first phase the input file is tokenised into a list where all comments are 
%% replaced by a space and all "backslash-newline" sequences are removed.
%% 
%% In the second phase all macros are expanded.

%% %% %% NOTE: #if, #else, and #elif are not yet implemented.  
%% Only '#if 0' is implemented to be possible to keep old code as a comment for
%% future refence by putting '#if 0' before it and '#endif' after it.  
%%
%%======================================================================================
%%======================================================================================
%%======================================================================================


%%======================================================================================
%% Variables which are used throughout the program:
%% ------------------------------------------------
%% 
%% Command         A preprocessor command
%% Current         Temporary variable used when tokenising the file
%% Defs            The currently valid macro definitions
%% Err             The current list of errors = [{file, line number, error text}, ...]
%% File            The tokenised file (or what remains of it when expanding the macros)
%% Flags           The preprocessor flags
%% FN or FileName  Tbe name of the current file
%% IfCou           Used for ifdef/ifndef/endif  values: check_all | {endif, Endif, IfLine}
%%                   Endif = number of matching endif's yet to be found 
%%                   Ifline = the line number for the the first found ifdef/ifndef
%% IncDir          Directories to be searched for included files
%% IncFile         Stack of included files
%% IncLine         The line numer of an include
%% L               The current line number
%% Name            Name of a macro
%% Nl              Number of encountered newlines
%% No_of_para      Numer of parameters of the currently expanded macro
%% Out             The result of the second step
%% Parameters      The parameters of the currently expanded macro
%% PrevFile        The name of the "parent" file which includes the currently expanded file
%% Rem             Remaining of the file currently being expanded
%% Removed         The tokens removed, used when removing tokens to the end of a line
%% Result          The current result of something
%% SelfRef         List of variables which shoud not be expanded at the rescan to avoid
%%                   endless loops due to self referencing
%% Str             Temporary string
%% Text            A variable used for string handling, e.g at error handling
%% Tokens          Temoprary list when tokenising
%% War             The current list of warnings = [{file, line number, warning text}, ...]
%% X               Temporary variable used when the value is not important 
%% Y               Temporary variable used when the value is not important 
%% 
%%======================================================================================

%% Multiple Include Optimization
%%
%% Algorithm described at:
%% http://gcc.gnu.org/onlinedocs/cppinternals/Guard-Macros.html
-record(mio, {valid = true,   %% multiple include valid
              cmacro,         %% controlling macro of the current conditional directive
              depth = 0,      %% conditional directive depth
              included = []}).



%%======================================================================================
%%======================================================================================
%%======================================================================================
%% The main entry for the preprocessor
%%
%%
%% Output   {ok, Out, War} | {error, Err}
%%======================================================================================
%%======================================================================================
%%======================================================================================
run(FileName, Flags) when is_atom(FileName) ->
    run(atom_to_list(FileName), Flags);

run(FileName, Flags) ->
    IncDir = include_dir(Flags),
    
    case catch file:read_file(FileName) of
	{ok, Bin} ->
	    FileList = binary_to_list(Bin),
	    run(FileList, FileName, IncDir, Flags);
	{error, _} ->
	    Text = "No such file or directory",
	    {error, [FileName ++ ": " ++ Text]}
    end.
	     

run(FileList, FileName, IncDir, Flags) ->
    %%----------------------------------------------------------
    %% Run the first phase, i.e tokenise the file
    %%----------------------------------------------------------
    File = tokenise(FileList, FileName),

    %%----------------------------------------------------------
    %% Run the second phase, i.e expand macros
    %%----------------------------------------------------------
    {Out, Err, War, _Defs, _Mio, IfCou} = expand(File, FileName, IncDir, Flags),

    %%----------------------------------------------------------
    %% Check if all #if #ifdef #ifndef have a matching #endif
    %%----------------------------------------------------------
    IfError = case IfCou of
		     {endif, Endif, IfLine} when Endif > 0 ->
			 [{FileName, IfLine, "unterminated `#if' conditional"}];
		     _ ->
			 []
		 end,
    
    Err2 = Err++IfError,

    case Err2 of 
	[] ->
	    {ok, lists:flatten(lists:reverse(Out)), lists:reverse(War)};
	_ ->
	    {error, lists:reverse(Err2)}
    end.

%%======================================================================================
%% The entry for all included files
%%
%%
%% Output   {Out, Err, War, Defs, MultipleIncludeValid}
%%======================================================================================
run_include(FileName, FileList, _Out, Defs, Err, War, IncLine, IncFile, IncDir, Mio) ->

    %%----------------------------------------------------------
    %% Run the first phase, i.e tokenise the file
    %%----------------------------------------------------------
    [PrevFile | _T] = IncFile,
    {File,  FileInfoStart, FileInfoEnd} = 
	tokenise(FileList, FileName, IncLine, PrevFile),

    %%----------------------------------------------------------
    %% Run the second phase, i.e expand macros
    %%----------------------------------------------------------
    {Out2, Err2, War2, Defs2, Mio2, IfCou2} =
        expand([FileInfoStart|File]++FileInfoEnd, Defs, Err, War,
               [FileName|IncFile], IncDir,
               #mio{included=Mio#mio.included}),

    MergeIncluded = sets:to_list(sets:from_list(Mio#mio.included ++ Mio2#mio.included)),

    Mio3 =
        case {Mio2#mio.valid, Mio2#mio.cmacro} of
            {V, Macro} when V == false;
                            Macro == undefined ->
                update_mio(Mio#mio{included=MergeIncluded});
            {true, _} ->
                update_mio({include, FileName}, Mio#mio{included=MergeIncluded})
        end,
    
    %%----------------------------------------------------------
    %% Check if all #if #ifdef #ifndef have a matching #endif
    %%----------------------------------------------------------
    IfError = case IfCou2 of
		       {endif, Endif, IfLine} when Endif > 0 ->
			   [{FileName, IfLine, "unterminated `#if' conditional"}];
		       _ ->
			   []
		   end,

    {Out2, Defs2, Err2++IfError, War2, Mio3}.




%%===================================================================================
%%===================================================================================
%%===================================================================================
%% Tokenise the file
%%
%%
%% Output: File
%%
%% Description:
%% The input file is tokenised into a list where all comments are replaced
%% by a space and all "backslash-newline" sequences are removed.
%%
%% A file information is added at start and end of an included file to set the
%% current file name and line number.
%%
%%
%% A token consists of:
%% --------------------
%%
%%  {char, Char}           special characters like ()[]{},!%&  etc
%%  {command,Command}      a macro command
%%  {expanded,Str}         an expanded variable, used to prevent infinite loops 
%%                           at self reference
%%  {file_info,FI}         start and end information of a file
%%                           FI is a string of the following format: 
%%                           "# Line FileName Int" were Int is 
%%                           1 if start of an included file, 
%%                           2 when returning to "parent" file
%%  {nl, L}                newline
%%  {number,Num)           variable, a string starting with a number
%%  {self_ref,Var}         to allow reference to a variable again, used when expanding 
%%                           self refering macros
%%  space                  a space
%%  space_exp              a space, special notation to prevent not wanted concatination 
%%  {string, Str}          a (tail of a) string constant
%%  {string_part, Str}     a head of a string constant defined on several consecutive lines
%%  {sys_head, Str}        (tail of) the file name of included system file
%%  {sys_head_part , Str}  the file name of included system file
%%  {var,Var}              variable, a string starting with minuscular or capital letter or
%%                           an underline
%% 
%% Note, comments are not removed within a character or string constant
%% or inside an include-definition where the file name is delimited with < >
%%===================================================================================
%%===================================================================================
%%===================================================================================

tokenise(File, FileName) ->
    {Result, _L} = token(File, 2, [], not_set, 0),
    FI_start = lists:reverse(lists:flatten(io_lib:format("# 1 \"~ts\"~n",[FileName]))),
    FileInfoStart = {file_info, FI_start},
    [FileInfoStart | Result].

tokenise(File, FileName, IncLine, PrevFile) ->
    {Result, _L} = token(File, 2, [], not_set, 0),
    FI_start = lists:reverse(lists:flatten(io_lib:format("# 1 \"~ts\" 1~n",[FileName]))),
    FileInfoStart = {file_info, FI_start},
    FI_end = lists:reverse(lists:flatten(io_lib:format("# ~p \"~ts\" 2~n~n",[IncLine-1,PrevFile]))),
    FileInfoEnd = [{file_info, FI_end}],
    {Result,  FileInfoStart, FileInfoEnd}.
%    [FileInfoStart | Result] ++ FileInfoEnd.


%%===================================================================================
%% token(InputFile, L, Result, Gen)
%%    Gen     information of the first token on the line, default = not_set
%%
%% Output: File
%%===================================================================================

%%==================================================================
%% Normal line
%%==================================================================
%%---------------------------------------
%% All file tokenised
%%---------------------------------------
token([], L, [{nl,NL}|Result], _Gen, _BsNl) when L == NL+1->
    {lists:reverse([{nl,NL}|Result]), L};
token([], L, Result, _Gen, _BsNl) ->
    {lists:reverse([{nl,L-1}|Result]), L};

%%---------------------------------------
%% String
%%---------------------------------------
token(File, L, Result, string, BsNl) ->
    case token_string(File, []) of
	{Rem, Str, nl} ->
	    Result1 = [{nl, L}, {string,Str} | Result],
	    token(Rem, L+1, Result1, string, BsNl);
	{Rem, Str} ->
	    token(Rem, L, [{string,Str}|Result], not_set, BsNl)
    end;

token([$"|File], L, Result, Gen, BsNl) ->
    case token_string(File, []) of
	{Rem, Str, nl} ->
	    Result1 = [{nl, L}, {string_part,Str} | Result],
	    token(Rem, L+1, Result1, string, BsNl);
	{Rem, Str} ->
	    token(Rem, L, [{string,Str}|Result], Gen, BsNl)
    end;

%%---------------------------------------
%% Include with < >
%%---------------------------------------
token(File, L, Result, include, BsNl) ->
    case token_include(File, []) of
	{Rem, Str, nl} ->
	    Result1 = [{nl, L}, {sys_head,Str} | Result],
	    token(Rem, L+1, Result1, include, BsNl);
	{Rem, Str} ->
	    token(Rem, L, [{sys_head,Str}|Result], not_set, BsNl)
    end;

token([$<|File], L, [space,{command,"include"}|Result], Gen, BsNl) ->
    case token_include(File, []) of
	{Rem, Str, nl} ->
	    Result1 = [{nl, L}, {sys_head_part,Str}, space, {command,"include"} |Result],
	    token(Rem, L+1,Result1, include, BsNl);
	{Rem, Str} ->
	    Result1 = [{sys_head,Str}, space, {command,"include"} |Result],
	    token(Rem, L, Result1, Gen, BsNl)
    end;
token([$<|File], L, [{command,"include"}|Result], Gen, BsNl) ->
    case token_include(File, []) of
	{Rem, Str, nl} ->
	    Result1 = [{nl, L}, {sys_head_part,Str}, space, {command,"include"} |Result],
	    token(Rem, L+1,Result1, include, BsNl);
	{Rem, Str} ->
	    Result1 = [{sys_head,Str}, space, {command,"include"} |Result],
	    token(Rem, L, Result1, Gen, BsNl)
    end;




%%---------------------------------------
%% CR (just remove these)
%%---------------------------------------
token([$\r|File], L, Result, Gen, BsNl) ->
%    Bs = lists:duplicate(BsNl+1,{nl,L}),
    token(File, L, Result, Gen, BsNl); %% Bs or BsNl?

%%---------------------------------------
%% Newline
%%---------------------------------------
token([$\n|File], L, Result, _Gen, BsNl) ->
    Bs = lists:duplicate(BsNl+1,{nl,L}),
    token(File, L+1, Bs++Result, not_set, 0);

token([$\\,$\n|File], L, Result, Gen, BsNl) ->
    token(File, L, Result, Gen, BsNl+1);

%%---------------------------------------
%% Comments
%%---------------------------------------
token([$/,$/|File], L, Result, not_set, BsNl) ->
    Rem = skip_to_nl(File),
    token(Rem, L+1,[{nl, L} | Result], not_set, BsNl);
token([$/,$/|File], L, Result, _Gen, BsNl) ->
    Rem = skip_to_nl(File),
    token(Rem, L+1,[{nl, L} | Result], not_set, BsNl);

token([$/,$*|File], L, Result, not_set, BsNl) ->
    case token_comment(File) of
	{Rem, nl} ->
	    token(Rem, L+1, [{nl, L} | Result], not_set, BsNl);
	Rem ->
	    token(Rem, L, Result, not_set, BsNl)
    end;
token([$/,$*|File], L, Result, Gen, BsNl) ->
    case token_comment(File) of
	{Rem, nl} ->
	    token(Rem, L+1, [{nl, L}, space | Result], not_set, BsNl);
	Rem ->
	    token(Rem, L, [space|Result], Gen, BsNl)
    end;

%%---------------------------------------
%% Variable
%%---------------------------------------
token([X|File], L, Result, Gen, BsNl) when ?is_upper(X) ->
    GenNew = case Gen of not_set -> var; _ -> Gen end,
    {Rem, Var} = tok_var(File, [X]),
    token(Rem, L, [{var,Var}|Result], GenNew, BsNl);
token([X|File], L, Result, Gen, BsNl) when ?is_lower(X) ->
    GenNew = case Gen of not_set -> var; _ -> Gen end,
    {Rem, Var} = tok_var(File, [X]),
    token(Rem, L, [{var,Var}|Result], GenNew, BsNl);
token([X|File], L, Result, Gen, BsNl) when ?is_underline(X) ->
    GenNew = case Gen of not_set -> var; _ -> Gen end,
    {Rem, Var} = tok_var(File, [X]),
    token(Rem, L, [{var,Var}|Result], GenNew, BsNl);

%%---------------------------------------
%% Number
%%---------------------------------------
token([X|File], L, Result, Gen, BsNl) when ?is_number(X) ->
    GenNew = case Gen of not_set -> number; _ -> Gen end,
    {Rem, Tokens} = tok_number(File, [X]),
    token(Rem, L, [{number,Tokens}|Result], GenNew, BsNl);

%%---------------------------------------
%% Space
%%---------------------------------------
token([X|File], L, [Y|Result], Gen, BsNl) when ?is_space(X) ->
    case Y of
	space ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, [Y|Result], Gen, BsNl);
	{nl,_,_} ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, Result, Gen, BsNl);
	_ ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, [space, Y |Result], Gen, BsNl)
    end;

token([X|File], L, [Y|Result], Gen, BsNl) when ?is_tab(X) ->
    case Y of
	space ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, [Y|Result], Gen, BsNl);
	{nl,_,_} ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, Result, Gen, BsNl);
	_ ->
	    Rem = remove_leading_spaces(File),
	    token(Rem, L, [space, Y |Result], Gen, BsNl)
    end;

%%---------------------------------------
%% Command
%%---------------------------------------
token([$#|File], L, Result, not_set, BsNl) ->
    {Rem, Command} = token_pp_com(File),
    case catch list_to_integer(Command) of
	{'EXIT', _} ->
	    token(Rem, L, [{command,Command}|Result], not_set, BsNl);
	_Int ->
	    Result1 = [{number,Command}, {command,"line"}| Result],
	    token(Rem, L, Result1, not_set, BsNl)
    end;

%%---------------------------------------
%% Char
%%---------------------------------------
token([X|File], L, Result, Gen, BsNl) ->
    GenNew = case Gen of not_set -> char; _ -> Gen end,
    token(File, L, [{char,X}|Result], GenNew, BsNl).


%%==================================================================
%% Scan to the end of a token
%%==================================================================
%%---------------------------------------
%% Number
%%---------------------------------------
tok_number([], Str) ->
    {[], lists:reverse(Str)};
tok_number([X|File], Str) when ?is_upper(X) ->
    tok_number(File, [X|Str]);
tok_number([X|File], Str) when ?is_lower(X) ->
    tok_number(File, [X|Str]);
tok_number([X|File], Str) when ?is_underline(X) ->
    tok_number(File, [X|Str]);
tok_number([X|File], Str) when ?is_number(X) ->
    tok_number(File, [X|Str]);
tok_number(File, Str) ->
    {File, lists:reverse(Str)}.


%%---------------------------------------
%% Variable
%%---------------------------------------
tok_var([], Str) ->
    {[], lists:reverse(Str)};
tok_var([X|File], Str) when ?is_upper(X) ->
    tok_var(File, [X|Str]);
tok_var([X|File], Str) when ?is_lower(X) ->
    tok_var(File, [X|Str]);
tok_var([X|File], Str) when ?is_underline(X) ->
    tok_var(File, [X|Str]);
tok_var([X|File], Str) when ?is_number(X) ->
    tok_var(File, [X|Str]);
tok_var(File, Str) ->
    {File, lists:reverse(Str)}.


%%---------------------------------------
%% Preprocessor command
%%---------------------------------------
token_pp_com([X|File]) when ?is_upper(X) ->
    tok_var(File, [X]);
token_pp_com([X|File]) when ?is_lower(X) ->
    tok_var(File, [X]);
token_pp_com([X|File]) when ?is_underline(X) ->
    tok_var(File, [X]);
token_pp_com([X|File]) when ?is_number(X) ->
    tok_var(File, [X]);
token_pp_com(File) ->
    Rem = remove_leading_spaces(File),
    {Rem, "null"}.



%%---------------------------------------
%% Comment
%%---------------------------------------
token_comment([]) ->
    [];
token_comment([$*,$/|File]) ->
    File;
token_comment([$\n|File]) ->
    {[$/,$*|File], nl};
token_comment([$\r,$\n|File]) ->
    {[$/,$*|File], nl};
token_comment([$\\,$\n|File]) ->
    {[$/,$*|File], nl};
%token_comment([$\\,$\n|File]) ->
%    token_comment(File);
token_comment([_|File]) ->
    token_comment(File).


%%---------------------------------------
%% String
%%---------------------------------------
token_string([], Str) ->
    {[], lists:reverse(Str)};
token_string([$"|File], Str) ->
    {File, lists:reverse(Str)};
token_string([$\n|File], Str) ->
    {File, lists:reverse(Str), nl};
token_string([$\r,$\n|File], Str) ->
    {File, lists:reverse(Str), nl};
token_string([$\\,$\n|File], Str) ->
    token_string(File, Str);
token_string([X|File], Str) ->
    token_string(File, [X|Str]).


%%---------------------------------------
%% Include
%%---------------------------------------
token_include([], Str) ->
    {[], lists:reverse(Str)};
token_include([$>|File], Str) ->
    {File, lists:reverse(Str)};
token_include([$\n|File], Str) ->
    {File, lists:reverse(Str), nl};
token_include([$\r,$\n|File], Str) ->
    {File, lists:reverse(Str), nl};
token_include([$\\,$\n|File], Str) ->
    token_include(File, Str);
token_include([X|File], Str) ->
    token_include(File, [X|Str]).




%%===================================================================================
%% detokenise a list of tokens, until next newline
%%
%% Output: a string
%%===================================================================================
detokenise(Tokens) ->
    detokenise(Tokens, []).

detokenise([], Result) ->
    lists:flatten(Result);
detokenise([space], Result) ->
    lists:flatten(Result);
detokenise([space_exp], Result) ->
    lists:flatten(Result);
detokenise([space|Rem], Result) ->
    detokenise(Rem, Result++[?space]);
detokenise([space_exp|Rem], Result) ->
    detokenise(Rem, Result++[?space]);
detokenise([nl|Rem], Result) ->
    detokenise(Rem, Result++[$\n]);
detokenise([{_, String}|Rem], Result) ->
    detokenise(Rem, Result++[String]).


detokenise_pragma(Tokens) ->
    detokenise_pragma(Tokens, []).

detokenise_pragma([], Result) ->
    lists:flatten(Result);
detokenise_pragma([space], Result) ->
    lists:flatten(Result);
detokenise_pragma([space|Rem], Result) ->
    detokenise_pragma(Rem, Result++[?space]);
detokenise_pragma([nl|Rem], Result) ->
    detokenise_pragma(Rem, Result++[$\n]);
detokenise_pragma([{string, String}|Rem], Result) ->
    detokenise_pragma(Rem, Result++[$"|String]++[$"]);
detokenise_pragma([{_, String}|Rem], Result) ->
    detokenise_pragma(Rem, Result++[String]).







%%======================================================================================
%%======================================================================================
%%======================================================================================
%% Expand macros.
%%
%%
%% Output:  A text file
%%
%% Description: Expands all macros. All macro definitions are logged in a list 'Defs'
%%              and all found errors and warnings are logged in a list 'Err' and 'War',
%%              respectively.
%%
%%              When a macro name is found in a source line it is expanded according
%%              to the current 'Defs'-list. The macro must agree both to the name
%%              and number of parameters, otherwise an error is reported.
%%======================================================================================
%%======================================================================================
%%======================================================================================
 

expand(List, FileName, IncDir, Flags) ->
    %% Get all definitions from preprocessor commnads
    %% and merge them on top of the file collected.
    CLDefs = get_cmd_line_defs(Flags),
    expand(List, [], [], CLDefs, [FileName], IncDir, #mio{}, check_all, [], [], 1, FileName).

expand(List, Defs, Err, War, [FileName|IncFile], IncDir, Mio) ->
    expand(List, [], [], Defs, [FileName|IncFile], IncDir, Mio, check_all, Err, War, 1, FileName).

%%=======================================================
%% Main loop for the expansion
%%=======================================================
expand([], Out, _SelfRef, Defs, _IncFile, _IncDir, Mio, IfCou, Err, War, _L, _FN) ->
%    io:format("~n   ===============~n"),
%    io:format("   definitions    ~p~n",[lists:reverse(Defs)]),
%    io:format("   found warnings ~p~n",[lists:reverse(War)]),
%    io:format("   found errors   ~p~n",[lists:reverse(Err)]),
%    io:format("   ===============~n~n~n"),
    {Out, Err, War, Defs, Mio, IfCou};

expand([{file_info, Str} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, Str++Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN);

%%---------------------------------------
%% Searching for endif, 
%% i.e skip all source lines until matching
%% end if is encountered
%%---------------------------------------
expand([{command,Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN)
  when Command == "ifdef" ->
    {_Removed, Rem2, _Nl} = read_to_nl(Rem),
    IfCou2 = {endif, Endif+1, IfLine},
    expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err, War, L, FN);


expand([{command,Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN)
  when Command == "ifndef" ->
    {_Removed, Rem2, _Nl} = read_to_nl(Rem),
    IfCou2 = {endif, Endif+1, IfLine},
    expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err, War, L, FN);


expand([{command,Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN)
  when Command == "if" ->
    case pp_command(Command, Rem, Defs, IncDir, Mio, Err, War, L, FN) of
        {{'if', true}, Rem2, Err2, War2, Nl} ->
            IfCou2 = {endif, Endif+1, IfLine},
            expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err2, War2, L+Nl, FN);
%%        {{'if', false}, Rem2, Err2, War2, Nl} -> Not implemented yet
        {{'if', error}, Rem2, Err2, War2, Nl} ->
            IfCou2 = {endif, Endif, IfLine},
            expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err2, War2, L+Nl, FN)
    end;

expand([{command,Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN)
  when Command == "endif" ->
    {_Removed, Rem2, Nl} = read_to_nl(Rem),
    case Endif of
	1 ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
            expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio, check_all, Err, War, L+Nl, FN);
	_ ->
            IfCou2 = {endif, Endif-1, IfLine},
            expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err, War, L+Nl, FN)
    end;


expand([{command,_Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN) ->
    {_Removed, Rem2, _Nl} = read_to_nl(Rem),
    IfCou2 = {endif, Endif, IfLine},
    expand(Rem2, Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err, War, L, FN);

%% Solves a bug when spaces in front of hashmark !
expand([space | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN) ->
    expand(Rem, Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN);

expand([{nl,_Nl} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN) ->
    expand(Rem, Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN);


expand([_X | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN) ->
    {_Removed, Rem2, Nl} = read_to_nl(Rem),
    Out2 = lists:duplicate(Nl,$\n) ++ Out,
    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio, {endif, Endif, IfLine}, Err, War, L, FN);





%%---------------------------------------
%% Check all tokens
%%---------------------------------------
expand([{nl, _N} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [$\n | Out], SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L+1, FN);

expand([space | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [?space | Out], SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN);

expand([space_exp | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [?space | Out], SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN);

expand([{command,Command} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, check_all, Err, War, L, FN) ->
    case pp_command(Command, Rem, Defs, IncDir, Mio, Err, War, L, FN) of
	{define, Rem2, Defs2, Err2, War2, Nl} -> 
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs2, IncFile, IncDir, update_mio(Mio), check_all, Err2, War2, L+Nl, FN);

	{undef, Rem2, Defs2, Err2, War2, Nl} -> 
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs2, IncFile, IncDir, update_mio(Mio), check_all, Err2, War2, L+Nl, FN);

	{{include, ok}, FileName, FileCont, Rem2, Nl, Err2, War2} ->
	    {Out3, Defs3, Err3, War3, Mio2} =
		run_include(FileName, FileCont, Out, Defs, Err2, War2, L+Nl, IncFile, IncDir, Mio),
	    Nls = [],
	    Out4 = Out3++Nls++Out,
	    expand(Rem2, Out4, SelfRef, Defs3, IncFile, IncDir, Mio2, check_all, Err3, War3, L+Nl, FN);
	
	{{include, error}, Rem2, Nl, Err2, War2} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, Err2, War2, L+Nl, FN);

	{{include, skip}, Rem2} ->
	    Out2 = [$\n|Out],
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, Err, War, L+1, FN);

	{{ifdef, true}, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    IfCou2 = {endif, 1, L},
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err2, War2, L+Nl, FN);
	{{ifdef, false}, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Mio2 = update_mio(ifdef, Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err2, War2, L+Nl, FN);

	{{ifndef, true}, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    IfCou2 = {endif, 1, L},
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err2, War2, L+Nl, FN);
	{{ifndef, false}, Macro, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Mio2 = update_mio({ifndef, Macro}, Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err2, War2, L+Nl, FN);

	{endif, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Mio2 = update_mio(endif, Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err2, War2, L+Nl, FN);

	{{'if', true}, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    IfCou2 = {endif, 1, L},
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio, IfCou2, Err2, War2, L+Nl, FN);
%%	{{'if', false}, Removed, Rem2, Nl} ->   Not implemented at present
	{{'if', error}, Rem2, Err2, War2, Nl} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Mio2 = update_mio('if', Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err2, War2, L+Nl, FN);

	{'else', {_Removed, Rem2, Nl}} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Err2 = {FN, L, "`else' command is not implemented at present"},
	    Mio2 = update_mio('else', Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, [Err2|Err], War, L+Nl, FN);

	{'elif', {_Removed, Rem2, Nl}} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    Err2 = {FN, L, "`elif' command is not implemented at present"},
	    Mio2 = update_mio('elif', Mio),
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, [Err2|Err], War, L+Nl, FN);

	{warning, {WarningText, Rem2, Nl}} ->
	    [FileName|_More] = IncFile,
	    War2 = {FileName, L, "warning: #warning "++detokenise(WarningText)},
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, Err, [War2|War], L+Nl, FN);
	    
	{error, {ErrorText, Rem2, Nl}} ->
	    [FileName|_More] = IncFile,
	    Err2 = {FileName, L, detokenise(ErrorText)},
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, [Err2|Err], War, L+Nl, FN);
	    
	{{line, ok}, {_Removed, Rem2, Nl}, L2, FN2, LineText} ->
	    Out2 = lists:duplicate(Nl,$\n)++LineText++Out,
	    [_X|IF] = IncFile,
	    IncFile2 = [FN2|IF],
	    expand(Rem2, Out2, SelfRef, Defs, IncFile2, IncDir, update_mio(Mio), check_all, Err, War, L2, FN2);
	{{line, error}, {_Removed, Rem2, Nl}, Err2} ->
	    Out2 = lists:duplicate(Nl,$\n) ++ Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, [Err2|Err], War, L+Nl, FN);

	hash_mark ->
	    expand(Rem, Out, SelfRef, Defs, IncFile, IncDir, Mio, check_all, Err, War, L, FN);

	{pragma, Rem2, Nl, Text} ->
	    Out2 = lists:duplicate(Nl,$\n)++Text++Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, Err, War, L+Nl, FN);

	{ident, Rem2, Nl, Text} ->
	    Out2 = lists:duplicate(Nl,$\n)++Text++Out,
	    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, update_mio(Mio), check_all, Err, War, L+Nl, FN);

	{not_recognised, {Removed, Rem2, Nl}} ->
	    Text = lists:reverse([$#|Command]),
	    RemovedS = lists:reverse([?space|detokenise(Removed)]),
	    Out2 = [$\n|RemovedS]++Text++Out,
	    Mio2 = update_mio(Mio),
	    case Command of
		[X|_T] when ?is_upper(X) ->
		    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err, War, L+Nl, FN);
		[X|_T] when ?is_lower(X) ->
		    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err, War, L+Nl, FN);
		[X|_T] when ?is_underline(X) ->
		    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, Err, War, L+Nl, FN);
		_ ->
		    Err2 = {FN, L, "invalid preprocessing directive name"},
		    expand(Rem2, Out2, SelfRef, Defs, IncFile, IncDir, Mio2, check_all, [Err2|Err], War, L+Nl, FN)
	    end;

	Else ->
%	    io:format(" %%%%Else%%%%%% ~p~n",[Else]),
	    exit(Else)
    end;


expand([{var, "__LINE__"}|Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    LL = io_lib:format("~p",[L]),
    expand(Rem, [LL | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, "__FILE__"}|Rem], Out, SelfRef, Defs, IncFile, Mio, IncDir, IfCou, Err, War, L, FN) ->
    expand(Rem, [$",FN,$" | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, "__DATE__"}|Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    {{Y,M,D},{_H,_Mi,_S}} = calendar:universal_time(),
    Date = io_lib:format("\"~s ~p ~p\"",[month(M),D,Y]),
    expand(Rem, [Date | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, "__TIME__"}|Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    {{_Y,_M,_D},{H,Mi,S}} = calendar:universal_time(),
    HS = if H < 10 -> "0"++integer_to_list(H);
	    true -> integer_to_list(H)
	 end,
    MiS = if Mi < 10 -> "0"++integer_to_list(Mi);
       true -> integer_to_list(Mi)
    end,
    SS = if S < 10 -> "0"++integer_to_list(S);
       true -> integer_to_list(S)
    end,
    Time = io_lib:format("\"~s:~s:~s\"",[HS,MiS,SS]),
    expand(Rem, [Time | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, "__INCLUDE_LEVEL__"}|Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    IL = io_lib:format("~p",[length(IncFile)-1]),
    expand(Rem, [IL | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, "__BASE_FILE__"}|Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    [BF|_T] = lists:reverse(IncFile),
    expand(Rem, [$",BF,$" | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{var, Var} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    {Out2, Err2, War2, Rem2, SelfRef2} = 
	source_line(Var, Rem, SelfRef, Defs, Err, War, L, FN),
    expand(Rem2, [Out2 | Out], SelfRef2, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err2, War2, L, FN);

expand([{char, Char} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [Char | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{number, Number} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [Number | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{expanded, Str} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [Str | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{self_ref, Str} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    SelfRef2 = lists:delete(Str,SelfRef),
    expand(Rem, Out, SelfRef2, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{string, Str} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    expand(Rem, [$", Str, $" | Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L, FN);

expand([{string_part, Str} | Rem], Out, SelfRef, Defs, IncFile, IncDir, Mio, IfCou, Err, War, L, FN) ->
    {Str2, Rem2, Nl} = expand_string_part([$"|Str], Rem),
    expand(Rem2, [Str2| Out], SelfRef, Defs, IncFile, IncDir, update_mio(Mio), IfCou, Err, War, L+Nl, FN).








%%========================================================================
%% Expand a line starting as a partial string
%%========================================================================
expand_string_part(Str, File) ->
    expand_string_part(File, Str, 0).

expand_string_part([{string, Str_part} | Rem], Str, Nl) ->
    {Str++Str_part++[$"], Rem, Nl};
expand_string_part([space | Rem], Str, Nl) ->
    expand_string_part(Rem, Str, Nl);
expand_string_part([nl| Rem], Str, Nl) ->
    expand_string_part(Rem, Str++[$\n], Nl);
expand_string_part([{string_part, Str_part} | Rem], Str, Nl) ->
    expand_string_part(Rem, Str++Str_part, Nl).





%%========================================================================
%% Parse and integrate command line macro directives
%% At this momment, only -D and -U are supported (gcc like)
%%========================================================================    


%% Collect all command line macro definitions
get_cmd_line_defs(Flags) ->
    Adjusted = parse_cmd_line(Flags,[]),

    {_Out, _Err, _War, Defs, _IfCou, _Mio} =
	expand(tokenise(Adjusted,""),
	       [],
	       [],
	       [],
	       [],
	       [],
	       #mio{},
	       check_all,
	       [],
	       [],
	       1,
	       ""),
    Defs.

%% Parse command line macros
parse_cmd_line([],Found) ->
    lists:flatten(lists:reverse(Found));

parse_cmd_line([45,68|Rest],Found) ->
    {Collected,RestCmds} = collect_define(Rest,[]),
    parse_cmd_line(RestCmds,[Collected|Found]);

parse_cmd_line([45,85|Rest],Found) ->
    {Collected,RestCmds} = collect_undefine(Rest,[]),
    parse_cmd_line(RestCmds,[Collected|Found]);

parse_cmd_line([_|Rest],Found) ->
    parse_cmd_line(Rest,Found).


%% Collect defines and translate them
%% into a text format 
collect_define([],Found) ->
    { "#define "++lists:reverse(Found)++"\n", [] };
collect_define([32|Rest],Found) ->
    { "#define "++lists:reverse(Found)++"\n", Rest };
collect_define([61|Rest],[]) ->
    { "", Rest };
collect_define([61|Rest],Found) ->
    collect_define(Rest,[32|Found]);
collect_define([C|Rest],Found) ->
    collect_define(Rest,[C|Found]).


%% Collect undefines and translate them
%% into a text format 
collect_undefine([],Found) ->
    { "#undef "++lists:reverse(Found)++"\n", [] };
collect_undefine([32|Rest],Found) ->
    { "#undef "++lists:reverse(Found)++"\n", Rest };
collect_undefine([C|Rest],Found) ->
    collect_undefine(Rest,[C|Found]).












%%======================================================================================
%%======================================================================================
%%======================================================================================
%% Read a preprocessor command
%%
%%
%% Output:  Depending of the command, typically = {Command, Rem, Err, War, Nl}
%%
%%======================================================================================
%%======================================================================================
%%======================================================================================

pp_command(Command, [space|File], Defs, IncDir, Mio, Err, War, L, FN) ->
    pp_command(Command, File, Defs, IncDir, Mio, Err, War, L, FN);

pp_command(Command, File, Defs, IncDir, Mio, Err, War, L, FN) ->

    case Command of
	%%----------------------------------------
	%% #define
	%%----------------------------------------
	"define" ->
	    case define(File, Err, War, L, FN) of
		{error, Rem, Err2, War2, Nl} ->
		    {define, Rem, Defs, Err2, War2, Nl};
		{warning, Rem, Name, No_of_para, Parameters, Macro, Err2, War2, Nl} ->
		    case is_define_ok(Name, No_of_para, Parameters, Macro, Defs) of
			{yes, Defs2} ->
			    {define, Rem, Defs2, Err2, War2, Nl};
			{no, Defs2} ->
			    Text = lists:flatten(io_lib:format("`~s' redefined",[Name])),
			    {define, Rem, Defs2, Err2, [{FN, L, Text}|War2], Nl};
			{error, Text, Defs2} ->
			    {define, Rem, Defs2, [{FN, L, Text}|Err2], War2, Nl}
		    end;
		{ok, Rem, Name, No_of_para, Parameters, Macro, Err2, War2, Nl} ->
		    case is_define_ok(Name, No_of_para, Parameters, Macro, Defs) of
			{yes, Defs2} ->
			    {define, Rem, Defs2, Err2, War2, Nl};
			{no, Defs2} ->
			    Text = lists:flatten(io_lib:format("`~s' redefined",[Name])),
			    {define, Rem, Defs2, Err2, [{FN, L, Text}|War2], Nl};
			{error, Text, Defs2} ->
			    {define, Rem, Defs2, [{FN, L, Text}|Err2], War2, Nl}
		    end
	    end;
	
	%%----------------------------------------
	%% #undef
	%%----------------------------------------
	"undef" ->
	    case undef(File, Err, War, L, FN) of
		{error, Rem, Err2, War2, Nl} ->
		    {undef, Rem, Defs, Err2, War2, Nl};
		{ok, Rem, Name, Err2, War2, Nl} ->
		    Defs2 = lists:keydelete(Name, 1, Defs),
		    {undef, Rem, Defs2, Err2, War2, Nl}
	    end;
	
	%%----------------------------------------
	%% #include
	%%----------------------------------------
	"include" ->
             case include(File, IncDir, Mio) of
                 {error, Rem, Nl, Err2} ->
                     {{include, error}, Rem, Nl, [{FN, L, Err2}|Err], War};
                 {error, Rem, Nl, Err2, NameNl} ->
                     {{include, error}, Rem, Nl, [{FN, L+ NameNl, Err2}|Err], War};
                 {ok, FileNamePath, FileCont, Rem, Nl} ->
                     {{include, ok}, FileNamePath, FileCont, Rem, Nl, Err, War};
                 {skip, Rem} ->
                     {{include, skip}, Rem}
             end;
	
	%%----------------------------------------
	%% #ifdef
	%%----------------------------------------
	"ifdef" ->
	    case define(File, Err, War, L, FN) of
		{error, Rem, Err2, War2, Nl} ->
		    {{ifdef, false}, Rem, Defs, Err2, War2, Nl};
		{warning, Rem, Name, No_of_para, _Parameters, _Macro, Err2, War2, Nl} ->
		    case is_defined_before(Name, No_of_para, Defs) of
			yes ->
			    {{ifdef, false}, Rem, Err2, War2, Nl};
			no ->
			    {{ifdef, true}, Rem, Err2, War2, Nl}
		    end;
		{ok, Rem, Name, No_of_para, _Parameters, _Macro, Err2, War2, Nl} ->
		    case is_defined_before(Name, No_of_para, Defs) of
			yes ->
			    {{ifdef, false}, Rem, Err2, War2, Nl};
			no ->
			    {{ifdef, true}, Rem, Err2, War2, Nl}
		    end
	    end;



	%%----------------------------------------
	%% #ifndef
	%%----------------------------------------
	"ifndef" ->
	    case define(File, Err, War, L, FN) of
		{error, Rem, Err2, War2, Nl} ->
		    {{ifndef, false}, Rem, Defs, Err2, War2, Nl};
		{warning, Rem, Name, No_of_para, _Parameters, _Macro, Err2, War2, Nl} ->
		    case is_defined_before(Name, No_of_para, Defs) of
			yes ->
			    {{ifndef, true}, Rem, Err2, War2, Nl};
			no ->
			    {{ifndef, false}, Name, Rem, Err2, War2, Nl}
		    end;
		{ok, Rem, Name, No_of_para, _Parameters, _Macro, Err2, War2, Nl} ->
		    case is_defined_before(Name, No_of_para, Defs) of
			yes ->
			    {{ifndef, true}, Rem, Err2, War2, Nl};
			no ->
			    {{ifndef, false}, Name, Rem, Err2, War2, Nl}
		    end
	    end;
	

	%%----------------------------------------
	%% #endif
	%%----------------------------------------
	"endif" ->
	    {Removed, Rem, Nl} = read_to_nl(File),
	    case Removed of
		[] -> 
		    {endif, Rem, Err, War, 1};
		_ ->
		    Text = "ignoring the tail of the line",
		    {ok, Rem, Err, [{FN, L, Text}|War], Nl}
	    end;
		    
	
	%%----------------------------------------
	%% #if
	%%----------------------------------------
	"if" ->
	    case if_zero(File, Err, War, L, FN) of
		{error, Rem2, _Removed, Nl} ->
		    Err2 = {FN, L, "only '#if 0' is implemented at present"},
		    {{'if', error}, Rem2, [Err2 | Err], War, Nl};
		{ok, Rem2, 0, _Removed, Nl} -> 
		    {{'if', true}, Rem2, Err, War, Nl};
		{ok, Rem2, _Num, _Removed, Nl} -> 
		    Err2 = {FN, L, "only '#if 0' is implemented at present"},
		    {{'if', error}, Rem2, [Err2 | Err], War, Nl}
	    end;
	
	%%----------------------------------------
	%% #else
	%%----------------------------------------
	"else" ->
	    {'else', read_to_nl(File)};
	
	%%----------------------------------------
	%% #elif
	%%----------------------------------------
	"elif" ->
	    {'elif', read_to_nl(File)};
	
	%%----------------------------------------
	%% #pragma
	%%----------------------------------------
	"pragma" ->
	    {Removed, Rem, Nl} = read_to_nl(File),
	    {pragma, Rem, Nl, lists:reverse("#pragma " ++ detokenise_pragma(Removed))};
	
	%%----------------------------------------
	%% #ident
	%%----------------------------------------
	"ident" ->
	    {Removed, Rem, Nl} = read_to_nl(File),
	    {ident, Rem, Nl, lists:reverse("#ident " ++ detokenise_pragma(Removed))};
	
	%%----------------------------------------
	%% #warning
	%%----------------------------------------
	"warning" ->
	    {warning, read_to_nl(File)};
	
	%%----------------------------------------
	%% #error
	%%----------------------------------------
	"error" ->
	    {error, read_to_nl(File)};
	
	%%----------------------------------------
	%% #line
	%%----------------------------------------
	"line" ->
	    line(File, L, FN);
	
	%%----------------------------------------
	%% #
	%%----------------------------------------
	"null" ->
	    hash_mark;
	
	%%----------------------------------------
	%% not recognised preprocessor commands
	%%----------------------------------------
	_Else ->
	    {not_recognised, read_to_nl(File)}
    end.




%%===============================================================
%%===============================================================
%%===============================================================
%% if
%%
%% Only #if 0 is implemented at the time to be able to use if
%% to comment some code parts.
%%===============================================================
%%===============================================================
%%===============================================================

if_zero(File, _Err, _War, _L, _FN) ->
    case if_zero(File) of
	{ok, Remain, Num, Removed, Nl} ->
	    case catch list_to_integer(Num) of
		{'EXIT', _} ->
		    {Removed2, Rem2, Nl2} = read_to_nl(File),
		    {error, Rem2, Removed2, Nl2};
		Int ->
		    {ok, Remain, Int, Removed, Nl}
	    end;
	E ->
	    E
    end.

if_zero([{number,Num}]) ->
    {ok, [], Num, [], 0};
if_zero([{number,Num}, space]) ->
    {ok, [], Num, [], 0};
if_zero([{number,Num} | Rem]) ->
    {Removed, Rem2, Nl} = read_to_nl(Rem),
    {ok, Rem2, Num, Removed, Nl};
%if_zero([{number,Num}, {nl,_X} | Rem]) ->
%    {ok, Rem, Num, [], 1};
if_zero(Rem) ->
    {Removed, Rem2, Nl} = read_to_nl(Rem),
    {error, Rem2, Removed, Nl}.



%%===============================================================
%%===============================================================
%%===============================================================
%% Define macro
%%
%% Check the syntax of the macro, extract the parameters if any.
%% If valid macro it is added to the Defs-list.
%% If a macro is redefined, a warning will be given, the latest
%% definition is always the valid one.
%%===============================================================
%%===============================================================
%%===============================================================

define(File, Err, War, L, FN) ->
    case define_name(File) of
	{ok, Rem, Name, No_of_para, Parameters, Macro, Nl} ->
	    {ok, Rem, Name, No_of_para, Parameters, Macro, Err, War, Nl};
	{{warning,no_space}, Rem, Name, No_of_para, Parameters, Macro, Nl} ->
	    Text = lists:flatten(io_lib:format("missing white space after `#define ~s'",[Name])),
	    {warning, Rem, Name, No_of_para, Parameters, Macro, Err, [{FN, L, Text}|War], Nl};
	{error, invalid_name, Nl} ->
	    Text = "invalid macro name",
	    {_Removed, Rem, Nl2} = read_to_nl(File),
	    {error, Rem, [{FN, L, Text}|Err], War, Nl+Nl2};
	{error, invalid_name, Name, Nl} ->
	    Text = lists:flatten(io_lib:format("invalid macro name `~s'",[Name])),
	    {_Removed, Rem, Nl2} = read_to_nl(File),
	    {error, Rem, [{FN, L, Text}|Err], War, Nl+Nl2};
	{error, illegal_arg} ->
	    {Removed, Rem, Nl} = read_to_nl(File),
	    RemovedS = detokenise(Removed),
	    Text = lists:flatten(io_lib:format("Invalid argument list ~s",[RemovedS])),
	    {error, Rem, [{FN, L, Text}|Err], War, Nl}
    end.



%%===========================================================  
%% Check if valid macro 
%%===========================================================  
define_name([]) ->
    {warning, no_macro};
define_name([space]) ->
    {warning, no_macro};
%% Macro with parameters
define_name([{var,Name},{char,$(}|Rem]) ->
    case read_para([{char,$(}|Rem]) of
	{ok, Rem2, Para, NoOfPara} ->
	    {Removed, Rem3, _Nl} = read_to_nl(Rem2),
	    {ok, Rem3, Name, NoOfPara, Para, Removed, 1};
	Error ->
	    Error
    end;
%% Macro without parameters
define_name([{var,Name}]) ->
    {ok, [], Name, 0, [], [], 0};
define_name([{var,Name}, space | Rem]) ->
    {Removed, Rem2, Nl} = read_to_nl(Rem),
    {ok, Rem2, Name, 0, [], Removed, Nl};
define_name([{var,Name}, {nl,_X} | Rem]) ->
    {ok, Rem, Name, 0, [], [], 1};
define_name([{var,Name} | Rem]) ->
    {Removed, Rem2, Nl} = read_to_nl(Rem),
    {{warning,no_space}, Rem2, Name, 0, [], Removed, Nl};
%% Invalid macro name
define_name([{number, Name} | _Rem]) ->
    {error, invalid_name, Name, 0};
define_name(_Rem) ->
    {error, invalid_name, 0}.







%%===============================================================
%%===============================================================
%%===============================================================
%% Undefine macro
%%
%% If it is a valid undef command the macro name will be deleted
%% from the Defs-list
%%===============================================================
%%===============================================================
%%===============================================================

undef(File, Err, War, L, FN) ->
    case undef(File) of
	{ok, Rem, Name, Nl} ->
	    {ok, Rem, Name, Err, War, Nl};
	{warning, Rem, Name, Nl} ->
	    Text = "ignoring the tail of the line",
	    {ok, Rem, Name, Err, [{FN, L, Text}|War], Nl};
	{error, invalid_name} ->
	    Text = "invalid macro name",
	    {_Removed, Rem, Nl} = read_to_nl(File),
	    {error, Rem, [{FN, L, Text}|Err], War, Nl};
	{error, invalid_name, Name} ->
	    Text = lists:flatten(io_lib:format("invalid macro name `~s'",[Name])),
	    {_Removed, Rem, Nl} = read_to_nl(File),
	    {error, Rem, [{FN, L, Text}|Err], War, Nl}
    end.

%%-------------------------------------------------  
%% Check if valid macro name
%%-------------------------------------------------  
undef([]) ->
    {error, invalid_name, []};
%% Valid name
undef([{var,Name}]) ->
    {ok, [], Name, 0};
undef([{var,Name}, {nl,_X} | Rem]) ->
    {ok, Rem, Name, 1};
undef([{var,Name}, space, {nl,_X} | Rem]) ->
    {ok, Rem, Name, 1};
undef([{var,Name} | Rem]) ->
    {_Removed, Rem2, Nl} = read_to_nl(Rem),
    {warning, Rem2, Name, Nl};
%% Invalid macro name
undef([{number, Name} | _Rem]) ->
    {error, invalid_name, Name};
undef(_Rem) ->
    {error, invalid_name}.
    





%%===============================================================
%%===============================================================
%%===============================================================
%% Include macro
%%
%% Read the included file 
%%===============================================================
%%===============================================================
%%===============================================================

include(File, IncDir, Mio) ->
    case include2(File) of
        {ok, FileName, Rem, Nl, FileType} ->
            Result = read_inc_file(FileName, IncDir, Mio),
            case {Result, Nl, FileType} of
                {{ok, FileNamePath, FileCont}, _, _} ->
                    {ok, FileNamePath, FileCont, Rem, Nl};
                {skip, _, _} ->
                    {skip, Rem};
                {{error, Text}, _, own_file} ->
                    NameNl = count_nl(FileName,0),
                    Error = lists:flatten(io_lib:format("~s: ~s",[FileName,Text])),
                    {error, Rem, Nl, Error,  NameNl};
                {{error, Text}, 1, sys_file} ->
                    NameNl = count_nl(FileName,0),
                    Error = lists:flatten(io_lib:format("~s: ~s",[FileName,Text])),
                    {error, Rem, Nl, Error,  NameNl};
                {{error, _Text}, _, sys_file} ->
                    {error, Rem, Nl, "`#include' expects \"FILENAME\" or <FILENAME>"}
            end;
        {error, {_Removed, Rem, Nl}} ->
            {error, Rem, Nl, "`#include' expects \#FILENAME\" or <FILENAME>"}
    end.



count_nl([],Nl) ->
    Nl;
count_nl([$\n|T],Nl) ->
    count_nl(T,Nl+1);
count_nl([_H|T],Nl) ->
    count_nl(T,Nl).

%%=================================================
%% Extract the file name from the token list
%%=================================================
include2([space|Rem]) ->
    include2(Rem);

include2([{string, FileName}]) ->
    {ok, FileName, [], 1, own_file};
include2([{string, FileName}, space]) ->
    {ok, FileName, [], 1, own_file};
include2([{string, FileName}, {nl, _X} | Rem]) ->
    {ok, FileName, Rem, 1, own_file};
include2([{string, FileName}, space, {nl, _X} | Rem]) ->
    {ok, FileName, Rem, 1, own_file};
include2([{string, _FileName}, _No_nl | Rem]) ->
    {error, read_to_nl(Rem)};
include2([{string_part, File_part}, {nl, _X} | Rem]) ->
     case include_read_string_file_name(File_part++[$\n], Rem, 1) of
	 {ok, FileName, Rem2, Nl} ->
	     {ok, FileName, Rem2, Nl, own_file};
	 error ->
	     {error, read_to_nl([{string_part,File_part} | Rem])}
     end;
include2([{sys_head, FileName}]) ->
    {ok, FileName, [], 1, sys_file};
include2([{sys_head, FileName}, space]) ->
    {ok, FileName, [], 1, sys_file};
include2([{sys_head, FileName}, {nl, _X}  | Rem]) ->
    {ok, FileName, Rem, 1, sys_file};
include2([{sys_head, FileName}, space, {nl, _X}  | Rem]) ->
    {ok, FileName, Rem, 1, sys_file};
include2([{sys_head, _FileName}, _No_nl | Rem]) ->
    {error, read_to_nl(Rem)};
include2([{sys_head_part ,File_part}, {nl, _X} | Rem]) ->
     case include_read_sys_file_name(File_part++[$\n], Rem, 1) of
	 {ok, FileName, Rem2, Nl} ->
	     {ok, FileName, Rem2, Nl, sys_file};
	 error ->
	     {error, read_to_nl([{sys_head_part, File_part} | Rem])}
     end;
include2(Rem) ->
    {error, read_to_nl(Rem)}.



%%-------------------------------------------------  
%% File name framed by " "
%%-------------------------------------------------  
include_read_string_file_name(File, [{string, File_part}, {nl,_X} | Rem], Nl) ->
    {ok, File++File_part, Rem, Nl+1};
include_read_string_file_name(File, [{string_part, File_part}, {nl,_X} | Rem], Nl) ->
    include_read_string_file_name(File++File_part++[$\n], Rem, Nl+1);
include_read_string_file_name(_File, _X, _Nl) ->
    error.

%%-------------------------------------------------  
%% File name framed by < >
%%-------------------------------------------------  
include_read_sys_file_name(File, [{sys_head, File_part}, {nl,_X} | Rem], Nl) ->
    {ok, File++File_part, Rem, Nl+1};
include_read_sys_file_name(File, [{sys_head_part, File_part}, {nl,_X} | Rem], Nl) ->
    include_read_sys_file_name(File++File_part++[$\n], Rem, Nl+1);
include_read_sys_file_name(_File, _X, _Nl) ->
    error.







%%===============================================================
%%===============================================================
%%===============================================================
%% Line macro
%%
%% The line macro may redefine both the current line number and 
%% the current file name: #line ' new_line_nr'  'new_file_name'
%%===============================================================
%%===============================================================
%%===============================================================

line(File, L, FN) ->
    line(File, L, FN, not_defined, not_defined).



line([], L, FN, _Line, _File) ->
    {{line, error}, {[],[],0}, {FN,L,"invalid format `#line' directive"}};

line([space|Rem], L, FN, Line, File) ->
    line(Rem, L, FN, Line, File);

%%------------------------------
%% Line number expected
%%------------------------------
line([{number,Number}|Rem], L, FN, not_defined, File) ->
    case catch list_to_integer(Number) of
	{'EXIT', _} ->
	    {{line, error}, read_to_nl(Rem), {FN,L,"invalid format `#line' directive"}};
	Int ->
	    line(Rem, L, FN, Int, File)
    end;
line(Rem, L, FN, not_defined, _File) ->
    {{line, error}, read_to_nl(Rem), {FN,L,"invalid format `#line' directive"}};

%%------------------------------
%% File name or newline expected
%%------------------------------
line([{nl, _NL}|Rem], _L, FN, Line, not_defined) ->
    {{line, ok}, {[],Rem,1}, Line, FN, io_lib:format("~n~p ~p #",[FN, Line-1])};
line([{string,NewFN}|Rem], _L, _FN, Line, not_defined) ->
    {{line, ok}, read_to_nl(Rem), Line, NewFN, io_lib:format("~n~p ~p #",[NewFN, Line-1])};
line(Rem, L, FN, _Line, _File) ->
    {{line, error}, read_to_nl(Rem), {FN,L,"invalid format `#line' directive"}}.
    



%%======================================================================================
%%======================================================================================
%%======================================================================================
%% Source line
%%
%%
%% Output:  {Str, Err, War, Rem, SelfRef}
%%
%% Description: The input source line is searched for macros. If a macro is found it
%%              is expanded. The result of an expansion is rescanned for more macros.
%%              To prevent infinite loops if the macro is self referring
%%              an extra token is put into the Rem list. The variable SelfRef
%%              contains all the macros which are inhibited to be expanded.
%%              A special specae token is also inserted to prevent not wanted 
%%              concatinations if one of the variables to be concatinated is expanded.
%%======================================================================================
%%======================================================================================
%%======================================================================================

source_line(Str, Rem, SelfRef, Defs, Err, War, L, FN) ->
    {Rem2, Para, No_of_para} = case read_para(Rem) of
				   {ok, RemT, ParaT, No_of_paraT} ->
				       {RemT, ParaT, No_of_paraT};
				   {error, illegal_arg} ->
				       {[], [], 0}
			       end,


    %%-------------------------------------------------  
    %% Check if a valid macro
    %%-------------------------------------------------  
    case lists:keysearch(Str, 1, Defs) of
	%% a macro without parameters
	{value, {Str, 0, _MacroPara, Macro}} ->
	    case lists:member(Str, SelfRef) of
		true ->
		    {[Str], Err, War, Rem, SelfRef};
		false ->
		    ExpandedRes2 = sl_mark_expanded(Macro, Str),
		    {[], Err, War, ExpandedRes2 ++ [{self_ref,Str}|Rem], [Str|SelfRef]}
	    end;

	%% a macro with parameters
	{value, {Str, N, _MacroPara, Macro}} when N == No_of_para ->
	    case lists:member(Str, SelfRef) of
		true ->
		    {[Str], Err, War, Rem, SelfRef};
		false ->
		    ExpandedRes = sl_macro_expand(Macro, Para, Defs),
		    ExpandedRes2 = sl_mark_expanded(ExpandedRes, Str),
		    {[], Err, War, ExpandedRes2 ++ [{self_ref,Str}|Rem2], [Str|SelfRef]}
	    end;

	%% a variable, because it doesn't have any parameters
	{value, {Str, _N, _MacroPara, _Macro}} when No_of_para == 0 ->
	    {Str, Err, War, Rem, SelfRef};

	%% illegal no of parameters
	{value, {Str, N, _MacroPara, _Macro}} when No_of_para < N ->
	    Text = io_lib:format(" macro `~s' used with just ~p arg",[Str,No_of_para]),
	    Err2 = {FN, L, lists:flatten(Text)},
	    {Str, [Err2|Err], War, Rem, SelfRef};
	{value, {Str, _N, _MacroPara, _Macro}} ->
	    Text = io_lib:format(" macro `~s' used with too many (~p) args",[Str,No_of_para]),
	    Err2 = {FN, L, lists:flatten(Text)},
	    {Str, [Err2|Err], War, Rem, SelfRef};

	%% no macro
	false ->
	    {Str, Err, War, Rem, SelfRef}
    end.





%%=================================================
%% Expand a macro
%%=================================================
sl_macro_expand(Macro, Para, Defs) ->
    sl_macro_expand(Macro, Para, Defs, []).


%%...................
%% End
%%...................
sl_macro_expand([], _Para, _Defs, Res) ->
    lists:reverse(Res);

%%...................
%% Concatination
%%...................
%% para ## para 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_para_para({para, N},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% para## para 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_para_para({para, N},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% para ##para 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_para_para({para, N},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% para##para 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_para_para({para, N},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);

%% para ## var 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, space, {var, Var}|T], Para, Defs, Res) ->
    Exp = sl_para_var({para, N}, {var, Var}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% para## var 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, space, {var, Var} | T], Para, Defs, Res) ->
    [{var, VarN}] = lists:nth(N,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,Var},{expanded,VarN}, space |Res]);
%% para ##var 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, {var, Var} | T], Para, Defs, Res) ->
    [{var, VarN}] = lists:nth(N,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,Var},{expanded,VarN}, space |Res]);
%% para##var 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, {var, Var} | T], Para, Defs, Res) ->
    [{var, VarN}] = lists:nth(N,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,Var},{expanded,VarN}, space |Res]);

%% var ## para 
sl_macro_expand([{var, Var}, space, {char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_var_para({var, Var},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% var## para 
sl_macro_expand([{var, Var}, {char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_var_para({var, Var},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% var ##para 
sl_macro_expand([{var, Var}, space, {char,$#}, {char,$#}, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_var_para({var, Var},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);
%% var##para 
sl_macro_expand([{var, Var}, {char,$#}, {char,$#}, {para,M} | T], Para, Defs, Res) ->
    Exp = sl_var_para({var, Var},{para, M}, Para),
    sl_macro_expand(Exp++T, Para, Defs, [space |Res]);

%% expanded ## para 
sl_macro_expand([space, {char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, [{expanded, Var}|Res]) ->
    [{var, VarM}] = lists:nth(M,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,VarM},{expanded,Var} |Res]);
%% expanded## para 
sl_macro_expand([{char,$#}, {char,$#}, space, {para,M} | T], Para, Defs, [{expanded, Var}|Res]) ->
    [{var, VarM}] = lists:nth(M,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,VarM},{expanded,Var} |Res]);
%% expanded ##para 
sl_macro_expand([space, {char,$#}, {char,$#}, {para,M} | T], Para, Defs, [{expanded, Var}|Res]) ->
    [{var, VarM}] = lists:nth(M,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,VarM},{expanded,Var} |Res]);
%% expanded##para 
sl_macro_expand([{char,$#}, {char,$#}, {para,M} | T], Para, Defs, [{expanded, Var}|Res]) ->
    [{var, VarM}] = lists:nth(M,Para),
    sl_macro_expand(T, Para, Defs, [{expanded,VarM},{expanded,Var} |Res]);

%% para ## ? 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, space, X | T], Para, Defs, Res) ->
    Reexp = sl_macro_reexpand(lists:nth(N,Para), Defs, []),
    sl_macro_expand([X, space|T], Para, Defs, lists:flatten([Reexp, space|Res]));
%% para## ? 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, space, X | T], Para, Defs, Res) ->
    Reexp = sl_macro_reexpand(lists:nth(N,Para), Defs, []),
    sl_macro_expand([X, space|T], Para, Defs, lists:flatten([Reexp, space|Res]));
%% para ##? 
sl_macro_expand([{para, N}, space, {char,$#}, {char,$#}, X | T], Para, Defs, Res) ->
    Reexp = sl_macro_reexpand(lists:nth(N,Para), Defs, []),
    sl_macro_expand([X, space|T], Para, Defs, lists:flatten([Reexp, space|Res]));
%% para##? 
sl_macro_expand([{para, N}, {char,$#}, {char,$#}, X | T], Para, Defs, Res) ->
    Reexp = sl_macro_reexpand(lists:nth(N,Para), Defs, []),
    sl_macro_expand([X, space|T], Para, Defs, lists:flatten([Reexp, space|Res]));

sl_macro_expand([{char,$#}, {char,$#}, space |T], Para, Defs, [space|Res]) ->
    sl_macro_expand(T, Para, Defs, Res);
sl_macro_expand([{char,$#}, {char,$#} |T], Para, Defs, [space|Res]) ->
    sl_macro_expand(T, Para, Defs, Res);
sl_macro_expand([{char,$#}, {char,$#}, space |T], Para, Defs, Res) ->
    sl_macro_expand(T, Para, Defs, Res);
sl_macro_expand([{char,$#}, {char,$#} |T], Para, Defs, Res) ->
    sl_macro_expand(T, Para, Defs, Res);
    
%%...................
%% Stringification
%%...................
sl_macro_expand([{char,$#}, {para, N}|T], Para, Defs, Res) ->
    Nth = lists:nth(N,Para),
    Tokens = detokenise(Nth),
    sl_macro_expand(T, Para, Defs, [{string,Tokens}|Res]);
sl_macro_expand([{char,$#}, space, {para, N}|T], Para, Defs, Res) ->
    Nth = lists:nth(N,Para),
    Tokens = detokenise(Nth),
    sl_macro_expand(T, Para, Defs, [{string,Tokens}|Res]);

%%...................
%% A parameter
%%...................
sl_macro_expand([{para, N}|T], Para, Defs, Res) ->
    Reexp = sl_macro_reexpand(lists:nth(N,Para), Defs, []),
    sl_macro_expand(T, Para, Defs, lists:flatten([Reexp|Res]));

%%...................
%% No parameter
%%...................
sl_macro_expand([H|T], Para, Defs, Res) ->
    sl_macro_expand(T, Para, Defs, [H|Res]).



%%-------------------------------------------------  
%% Expand parameters
%%-------------------------------------------------  
sl_para_para({para, N}, {para, M}, Para) ->
    case sl_para_1st(lists:nth(N,Para)) of
	{ok, Para1st} ->
	    Para1st ++ sl_para_2nd(lists:nth(M,Para));
	{exp, Para1st} ->
	    Para1st ++ sl_para_2nd(lists:nth(M,Para)) ++ [space_exp];
	{space, Para1st} ->
	    Para1st ++ [space_exp | sl_para_2nd(lists:nth(M,Para))]
    end.


sl_var_para(Var, {para, M}, Para) ->
    [Var|sl_para_2nd(lists:nth(M,Para))].


sl_para_var({para, N}, Var, Para) ->
    case sl_para_1st(lists:nth(N,Para)) of
	{ok, Para1st} ->
	    Para1st ++ [Var];
	{exp, Para1st} ->
	    Para1st ++ [Var | space_exp];
	{space, Para1st} ->
	    Para1st ++ [space_exp | Var]
    end.


sl_para_1st([{var, Var}]) ->
    {ok,[{expanded,Var}]};
sl_para_1st([{var, Var}, space]) ->
    {ok,[{expanded,Var}]};
sl_para_1st([{var, Var}, space_exp]) ->
    {exp, [{expanded,Var}]};
sl_para_1st(L) ->
    {space, L}.

sl_para_2nd([{var, Var}]) ->
    [{expanded,Var}];
sl_para_2nd([{var, Var}, space_exp]) ->
    [{expanded,Var}];
sl_para_2nd([space, {var, Var}]) ->
    [{expanded,Var}];
sl_para_2nd([space_exp, {var, Var}]) ->
    [{expanded,Var}];
sl_para_2nd(L) ->
    L++[space].



%%-------------------------------------------------  
%% Check if the expansion is a valid macro,
%% do not reexpand if concatination
%%-------------------------------------------------  
sl_macro_reexpand([], _Defs, Result) ->
    Result;
sl_macro_reexpand([{var,Var}|Rem], Defs, Result) ->
    case lists:keysearch(Var, 1, Defs) of
	{value, {Var, 0, _MacroPara, Macro}} ->
	    Rem2 = case Rem of
		     [space | RemT] ->
			 [space_exp | RemT];
		     _ ->
			 [space_exp | Rem]
		 end,
	    sl_macro_reexpand(Macro++Rem2, Defs, Result);
	_ ->
	    sl_macro_reexpand(Rem, Defs, [{var,Var}|Result])
    end;
sl_macro_reexpand([H|Rem], Defs, Result) ->
    sl_macro_reexpand(Rem, Defs, [H|Result]).



%%-------------------------------------------------  
%% Self referring macros are marked not be reexpanded
%%-------------------------------------------------  
sl_mark_expanded(QQ, Str) ->
    sl_mark_expanded(QQ, Str, []).

sl_mark_expanded([], _Str, Res) ->
    lists:reverse(Res);
sl_mark_expanded([H|T], Str, Res) ->
    case H of
	{_,Str} ->
	    sl_mark_expanded(T, Str, [{expanded, Str}|Res]);
	_ ->
	    sl_mark_expanded(T, Str, [H|Res])
    end.









%%======================================================================================
%%======================================================================================
%%======================================================================================
%% Misceleaneous functions
%%======================================================================================
%%======================================================================================
%%======================================================================================


%%===============================================================
%%  Check the Flags for include directories
%%===============================================================
include_dir(Flags) when is_list(Flags)->
    include_dir(Flags,[]);
include_dir(_Flags) ->
    [].
    
include_dir(Flags,IncDirs) ->
    case string:str(Flags,"-I") of
	0 ->
	    lists:reverse(IncDirs);
	X ->
	    {NewDir, RemainingFlags} =
		gobble_inc_dir(string:sub_string(Flags, X+2),nq,[]),
	    include_dir(RemainingFlags, [NewDir|IncDirs])
    end.

% nq = not-quoted, q = quoted.
% Possible strange scenarios:
%  /usr/test\ ing/
%  "/usr/test ing/"
%  /usr/test\"ing/
%  "/usr/test\"ing/"
gobble_inc_dir([],nq,Acc) ->
    % Only accept nq here, if we end up here in q mode the user has missed a "
    {lists:reverse(Acc),[]};
gobble_inc_dir([$\\,$"|R],Q,Acc) ->
    gobble_inc_dir(R,Q,[$"|Acc]);
gobble_inc_dir([$"|R],nq,Acc) ->
    gobble_inc_dir(R,q,Acc);
gobble_inc_dir([$"|R],q,Acc) ->
    gobble_inc_dir(R,nq,Acc);
gobble_inc_dir([$\\,$ |R],nq,Acc) ->
    gobble_inc_dir(R,nq,[$ |Acc]);
gobble_inc_dir([$ |R],nq,Acc) ->
    {lists:reverse(Acc),R};
gobble_inc_dir([C|R],Q,Acc) ->
    gobble_inc_dir(R,Q,[C|Acc]).


%%===============================================================
%%  Read a included file. Try current dir first then the IncDir list
%%===============================================================

read_inc_file(FileName, IncDir, Mio) ->
    case find_inc_file(FileName, IncDir) of
        {ok, AbsFile} ->
            %% is included before?
            case lists:member(FileName, Mio#mio.included) of
                false ->
                    case catch file:read_file(AbsFile) of
                        {ok, Bin} ->
                            FileList = binary_to_list(Bin),
                            {ok, AbsFile, FileList};
                        {error, Text} ->
			    {error, Text}
                    end;
                true ->
                    skip
            end;
        {error, Text} ->
            {error, Text}
    end.

find_inc_file(FileName, IncDir) ->
    case catch file:read_file_info(FileName) of
	{ok, _} ->
	    {ok, FileName};
	{error, _} ->
	    find_inc_file2(FileName, IncDir)
    end.

find_inc_file2(_FileName, []) ->
    {error, "No such file or directory"};
find_inc_file2(FileName, [D|Rem]) ->
    Dir = case lists:last(D) of
	      $/ ->
		  D;
	      _ ->
		  D++"/"
	  end,
    case catch file:read_file_info(Dir++FileName) of
	{ok, _} ->
	    {ok, Dir++FileName};
	{error, _} ->
	    find_inc_file2(FileName, Rem)
    end.


%%===============================================================
%%  Read parameters of a macro or a variable in a source line
%%===============================================================
read_para([{char,$(} | Rem]) ->
    read_para(Rem, 1, [], [], 1);
read_para([space,{char,$(} | Rem]) ->
    read_para(Rem, 1, [], [], 1);
read_para(_Rem) ->
    {ok, [], [], 0}.


%% Abrupt end of the list
read_para([], _NoOfParen, _Current, _Para, _NoOfPara) ->
    {error, illegal_arg};
%% All parameters checked
read_para([{char,$)}|Rem], 1, [], Para, NoOfPara) ->
    {ok, Rem, lists:reverse(Para), NoOfPara};
read_para([{char,$)}|Rem], 1, Current, Para, NoOfPara) ->
    {ok, Rem, lists:reverse([Current|Para]), NoOfPara};

%% Continue reading
read_para([{char,$)}|Rem], NoOfParen, Current, Para, NoOfPara) ->
    read_para(Rem, NoOfParen-1, Current++[{char,$)}], Para, NoOfPara);
read_para([{char,$(}|Rem], NoOfParen, Current, Para, NoOfPara) ->
    read_para(Rem, NoOfParen+1, Current++[{char,$(}], Para, NoOfPara);
read_para([{char,$,}|Rem], NoOfParen, Current, Para, NoOfPara) when NoOfParen == 1 ->
    read_para(Rem, NoOfParen, [], [Current|Para], NoOfPara+1);
read_para([space|Rem], NoOfParen, [], Para, NoOfPara) ->
    read_para(Rem, NoOfParen, [], Para, NoOfPara);
read_para([X|Rem], NoOfParen, Current, Para, NoOfPara) ->
    read_para(Rem, NoOfParen, Current++[X], Para, NoOfPara).






%%===================================================================================
%% check if a macro is already defined
%%===================================================================================
is_define_ok(Name, No_of_para, Parameters, Macro, Defs) ->
    
    case lists:keysearch(Name, 1, Defs) of
	{value, {Name, No_of_para, _MacroPara, Macro}} ->
	    {yes, Defs};
	{value, _} ->
	    Defs2 = lists:keydelete(Name, 1, Defs),
	    NewMacro = is_define_ok_check_para(Parameters, Macro, []),
	    case is_stringify_ok(NewMacro) of
		yes ->
		    {no, [{Name, No_of_para, Parameters, NewMacro}|Defs2]};
		no ->
		    ErrorText = "`#' operator is not followed by a macro argument name",
		    {error, ErrorText, [{Name, No_of_para, Parameters, NewMacro}|Defs2]}
	    end;
	false ->
	    NewMacro = is_define_ok_check_para(Parameters, Macro, []),
	    case is_stringify_ok(NewMacro) of
		yes ->
		    {yes, [{Name, No_of_para, Parameters, NewMacro}|Defs]};
		no ->
		    ErrorText = "`#' operator is not followed by a macro argument name",
		    {error, ErrorText, [{Name, No_of_para, Parameters, NewMacro}|Defs]}
	    end
    end.

is_define_ok_check_para(_Para, [], Result) ->
    lists:reverse(Result);

is_define_ok_check_para(Para, [H|T], Result) ->
    case define_arg_para_number(1, Para, H) of
	no_para -> 
	    is_define_ok_check_para(Para, T, [H|Result]);
	N ->
	    is_define_ok_check_para(Para, T, [{para,N}|Result])
    end.

define_arg_para_number(_N, [], _Current) ->
    no_para;
define_arg_para_number(N, [H|_Para], Current) when H == [Current] ->
    N;
define_arg_para_number(N, [_H|Para], Current) ->
    define_arg_para_number(N+1, Para, Current).
    

is_stringify_ok([]) ->
    yes;
is_stringify_ok([{char,$#},{char,$#}|T]) ->
    is_stringify_ok(T);
is_stringify_ok([{char,$#},space,{para,_X}|T]) ->
    is_stringify_ok(T);
is_stringify_ok([{char,$#},{para,_X}|T]) ->
    is_stringify_ok(T);
is_stringify_ok([{char,$#},space,{var,_X}|T]) ->
    is_stringify_ok(T);
is_stringify_ok([{char,$#},{var,_X}|T]) ->
    is_stringify_ok(T);
is_stringify_ok([{char,$#},space,{nl,_X}|_T]) ->
    no;
is_stringify_ok([{char,$#},{nl,_X}|_T]) ->
    no;
is_stringify_ok([{char,$#}|_T]) ->
    no;
is_stringify_ok([_H|T]) ->
    is_stringify_ok(T).

%%===================================================================================
%% check if a macro is already defined
%%===================================================================================
is_defined_before(Name, No_of_para, Defs) ->
    case lists:keysearch(Name, 1, Defs) of
	{value, {Name, No_of_para, _MacroPara, _Macro}} ->
	    yes;
	{value, _} ->
	    no;
	false ->
	    no
    end.
    



%%===================================================================================
%% read_to_nl(File)
%%===================================================================================
read_to_nl([space|Rem]) ->
    read_to_nl(Rem, [], 1);
read_to_nl(Rem) ->
    read_to_nl(Rem, [], 1).

read_to_nl([], Result, Nl) ->
    {lists:reverse(Result), [], Nl};
read_to_nl([{nl, _N}|Rem], [{string_part,String} | Result], Nl) ->
    read_to_nl(Rem, [nl, {string_part,String}|Result], Nl+1);
read_to_nl([{nl, _N}|Rem], [{sys_head_part,String} | Result], Nl) ->
    read_to_nl(Rem, [nl, {sys_head_part,String}|Result], Nl+1);
read_to_nl([{nl, _N}|Rem], Result, Nl) ->
    {lists:reverse(Result), Rem, Nl};
read_to_nl([space|Rem], Result, Nl) ->
    read_to_nl(Rem, [space|Result], Nl);
read_to_nl([{X,String}|Rem], Result, Nl) ->
    read_to_nl(Rem, [{X,String}|Result], Nl).




%%===========================================================  
%% Read characters until next newline
%%===========================================================  
%read_to_nl2(Str) -> read_to_nl2([],Str).

%read_to_nl2(Line, [])        -> {Line,[]};
%read_to_nl2(Line, [$\n|Str]) -> {Line, Str};
%read_to_nl2(Line, [X|Str])   -> read_to_nl2([X|Line], Str).




%%===========================================================  
%% Remove leading spaces from a list
%%===========================================================  
remove_leading_spaces([?space|List]) ->
    remove_leading_spaces(List);
remove_leading_spaces([?tab|List]) ->
    remove_leading_spaces(List);
remove_leading_spaces(List) ->
    List.




%%===========================================================  
%% Skip characters until next newline
%%===========================================================  
skip_to_nl([]) -> [];
skip_to_nl([$\n | Str]) -> Str;
skip_to_nl([$\\,$\n | Str]) -> [$/,$/|Str];
skip_to_nl([_|Str]) -> skip_to_nl(Str).




month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".


%% Multiple Include Optimization
%%
%% Algorithm described at:
%% http://gcc.gnu.org/onlinedocs/cppinternals/Guard-Macros.html
update_mio({include, FileName}, #mio{included=Inc}=Mio) ->
    Mio#mio{valid=false, included=[FileName|Inc]};

%% valid=false & cmacro=undefined indicates it is already decided this file is
%% not subject to MIO
update_mio(_, #mio{valid=false, depth=0, cmacro=undefined}=Mio) ->
    Mio;

%% if valid=true, there is no non-whitespace tokens before this ifndef
update_mio({'ifndef', Macro}, #mio{valid=true, depth=0, cmacro=undefined}=Mio) ->
    Mio#mio{valid=false, cmacro=Macro, depth=1};

%% detect any tokens before top level #ifndef
update_mio(_, #mio{valid=true, depth=0, cmacro=undefined}=Mio) ->
    Mio#mio{valid=false};

%% If cmacro is alreay set, this is after the top level #endif
update_mio({'ifndef', _}, #mio{valid=true, depth=0}=Mio) ->
    Mio#mio{valid=false, cmacro=undefined};

%% non-top level conditional, just update depth
update_mio({'ifndef', _}, #mio{depth=D}=Mio) when D > 0 ->
    Mio#mio{depth=D+1};
update_mio('ifdef', #mio{depth=D}=Mio) ->
    Mio#mio{depth=D+1};
update_mio('if', #mio{depth=D}=Mio) ->
    Mio#mio{depth=D+1};

%% top level #else #elif invalidates multiple include optimization
update_mio('else', #mio{depth=1}=Mio) ->
    Mio#mio{valid=false, cmacro=undefined};
update_mio('else', Mio) ->
    Mio;
update_mio('elif', #mio{depth=1}=Mio) ->
    Mio#mio{valid=false, cmacro=undefined};
update_mio('elif', Mio) ->
    Mio;

%% AT exit to top level, if the controlling macro is not set, this could be the
%% end of a non-ifndef conditional block, or there were tokens before entering
%% the #ifndef block. In either way, this invalidates the MIO
%%
%% It doesn't matter if `valid` is true at the time of exiting, it is set to
%% true.  This will be used to detect if more tokens are following the top
%% level #endif.
update_mio('endif', #mio{depth=1, cmacro=undefined}=Mio) ->
    Mio#mio{valid=false, depth=0};
update_mio('endif', #mio{depth=1}=Mio) ->
    Mio#mio{valid=true, depth=0};
update_mio('endif', #mio{depth=D}=Mio) when D > 1 ->
    Mio#mio{valid=true, depth=D-1};

%%if more tokens are following the top level #endif.
update_mio('endif', #mio{depth=1, cmacro=undefined}=Mio) ->
    Mio#mio{valid=false, depth=0};
update_mio('endif', #mio{depth=D}=Mio) when D > 0 ->
    Mio#mio{valid=true, depth=D-1};
update_mio(_, Mio) ->
    Mio#mio{valid=false}.

%% clear `valid`, this doesn't matter since #endif will restore it if
%% appropriate
update_mio(Mio) ->
    Mio#mio{valid=false}.


