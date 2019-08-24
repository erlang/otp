%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

%% Erlang token scanning functions of io library.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037    NUL - US        control
%% 040 - 057    SPC - /         punctuation
%% 060 - 071    0 - 9           digit
%% 072 - 100    : - @           punctuation
%% 101 - 132    A - Z           uppercase
%% 133 - 140    [ - `           punctuation
%% 141 - 172    a - z           lowercase
%% 173 - 176    { - ~           punctuation
%% 177          DEL             control
%% 200 - 237                    control
%% 240 - 277    NBSP - ¿        punctuation
%% 300 - 326    À - Ö           uppercase
%% 327          ×               punctuation
%% 330 - 336    Ø - Þ           uppercase
%% 337 - 366    ß - ö           lowercase
%% 367          ÷               punctuation
%% 370 - 377    ø - ÿ           lowercase
%%
%% Many punctuation characters have special meaning:
%%  $\s, $_, $", $$, $%, $', $.
%% DEL is a punctuation.
%%
%% Must watch using × \327, very close to x \170.

-module(erl_scan).

%%% External exports

-export([string/1,string/2,string/3,tokens/3,tokens/4,
         format_error/1,reserved_word/1]).

-export([column/1,end_location/1,line/1,location/1,text/1,
         category/1,symbol/1]).

%%% Private
-export([continuation_location/1]).

-export_type([error_info/0,
              options/0,
              return_cont/0,
              token/0,
              tokens_result/0]).

%%%
%%% Defines and type definitions
%%%

-define(COLUMN(C), (is_integer(C) andalso C >= 1)).
%% Line numbers less than zero have always been allowed:
-define(ALINE(L), is_integer(L)).
-define(STRING(S), is_list(S)).
-define(RESWORDFUN(F), is_function(F, 1)).

-type category() :: atom().
-type resword_fun() :: fun((atom()) -> boolean()).
-type option() :: 'return' | 'return_white_spaces' | 'return_comments'
                | 'text' | {'reserved_word_fun', resword_fun()}.
-type options() :: option() | [option()].
-type symbol() :: atom() | float() | integer() | string().
-type token() :: {category(), Anno :: erl_anno:anno(), symbol()}
               | {category(), Anno :: erl_anno:anno()}.
-type tokens() :: [token()].
-type error_description() :: term().
-type error_info() :: {erl_anno:location(), module(), error_description()}.

%%% Local record.
-record(erl_scan,
        {resword_fun = fun reserved_word/1 :: resword_fun(),
         ws          = false               :: boolean(),
         comment     = false               :: boolean(),
         text        = false               :: boolean()}).

%%----------------------------------------------------------------------------

-spec format_error(ErrorDescriptor) -> string() when
      ErrorDescriptor :: error_description().
format_error({string,Quote,Head}) ->
    lists:flatten(["unterminated " ++ string_thing(Quote) ++
                   " starting with " ++
                   io_lib:write_string(Head, Quote)]);
format_error({illegal,Type}) ->
    lists:flatten(io_lib:fwrite("illegal ~w", [Type]));
format_error(char) -> "unterminated character";
format_error({base,Base}) ->
    lists:flatten(io_lib:fwrite("illegal base '~w'", [Base]));
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

-spec string(String) -> Return when
      String :: string(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String) ->
    string(String, 1, []).

-spec string(String, StartLocation) -> Return when
      String :: string(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      StartLocation :: erl_anno:location(),
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String, StartLocation) ->
    string(String, StartLocation, []).

-spec string(String, StartLocation, Options) -> Return when
      String :: string(),
      Options :: options(),
      Return :: {'ok', Tokens :: tokens(), EndLocation}
              | {'error', ErrorInfo :: error_info(), ErrorLocation},
      StartLocation :: erl_anno:location(),
      EndLocation :: erl_anno:location(),
      ErrorLocation :: erl_anno:location().
string(String, Line, Options) when ?STRING(String), ?ALINE(Line) ->
    string1(String, options(Options), Line, no_col, []);
string(String, {Line,Column}, Options) when ?STRING(String),
                                            ?ALINE(Line),
                                            ?COLUMN(Column) ->
    string1(String, options(Options), Line, Column, []).

-type char_spec() :: string() | 'eof'.
-type cont_fun() :: fun((char_spec(), #erl_scan{},
                         erl_anno:line(), erl_anno:column(),
                         tokens(), any()) -> any()).
-opaque return_cont() :: {erl_scan_continuation,
                          string(), erl_anno:column(), tokens(),
                          erl_anno:line(),
                          #erl_scan{}, any(), cont_fun()}.
-type tokens_result() :: {'ok', Tokens :: tokens(),
                          EndLocation :: erl_anno:location()}
                       | {'eof', EndLocation :: erl_anno:location()}
                       | {'error', ErrorInfo :: error_info(),
                          EndLocation :: erl_anno:location()}.

-spec tokens(Continuation, CharSpec, StartLocation) -> Return when
      Continuation :: return_cont() | [],
      CharSpec :: char_spec(),
      StartLocation :: erl_anno:location(),
      Return :: {'done',Result :: tokens_result(),LeftOverChars :: char_spec()}
              | {'more', Continuation1 :: return_cont()}.
tokens(Cont, CharSpec, StartLocation) ->
    tokens(Cont, CharSpec, StartLocation, []).

-spec tokens(Continuation, CharSpec, StartLocation, Options) -> Return when
      Continuation :: return_cont() | [],
      CharSpec :: char_spec(),
      StartLocation :: erl_anno:location(),
      Options :: options(),
      Return :: {'done',Result :: tokens_result(),LeftOverChars :: char_spec()}
              | {'more', Continuation1 :: return_cont()}.
tokens([], CharSpec, Line, Options) when ?ALINE(Line) ->
    tokens1(CharSpec, options(Options), Line, no_col, [], fun scan/6, []);
tokens([], CharSpec, {Line,Column}, Options) when ?ALINE(Line),
                                                  ?COLUMN(Column) ->
    tokens1(CharSpec, options(Options), Line, Column, [], fun scan/6, []);
tokens({erl_scan_continuation,Cs,Col,Toks,Line,St,Any,Fun},
       CharSpec, _Loc, _Opts) ->
    tokens1(Cs++CharSpec, St, Line, Col, Toks, Fun, Any).

continuation_location({erl_scan_continuation,_,no_col,_,Line,_,_,_}) ->
    Line;
continuation_location({erl_scan_continuation,_,Col,_,Line,_,_,_}) ->
    {Line,Col}.

-spec column(Token) -> erl_anno:column() | 'undefined' when
      Token :: token().

column(Token) ->
    erl_anno:column(element(2, Token)).

-spec end_location(Token) -> erl_anno:location() | 'undefined' when
      Token :: token().

end_location(Token) ->
    erl_anno:end_location(element(2, Token)).

-spec line(Token) -> erl_anno:line() when
      Token :: token().

line(Token) ->
    erl_anno:line(element(2, Token)).

-spec location(Token) -> erl_anno:location() when
      Token :: token().

location(Token) ->
    erl_anno:location(element(2, Token)).

-spec text(Token) -> erl_anno:text() | 'undefined' when
      Token :: token().

text(Token) ->
    erl_anno:text(element(2, Token)).

-spec category(Token) -> category() when
      Token :: token().

category({Category,_Anno}) ->
    Category;
category({Category,_Anno,_Symbol}) ->
    Category;
category(T) ->
    erlang:error(badarg, [T]).

-spec symbol(Token) -> symbol() when
      Token :: token().

symbol({Category,_Anno}) ->
    Category;
symbol({_Category,_Anno,Symbol}) ->
    Symbol;
symbol(T) ->
    erlang:error(badarg, [T]).

%%%
%%% Local functions
%%%

string_thing($') -> "atom";   %' Stupid Emacs
string_thing(_) -> "string".

-define(WHITE_SPACE(C),
        is_integer(C) andalso
         (C >= $\000 andalso C =< $\s orelse C >= $\200 andalso C =< $\240)).
-define(DIGIT(C), C >= $0, C =< $9).
-define(CHAR(C), is_integer(C), C >= 0).
-define(UNICODE(C),
        is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF)).

-define(UNI255(C), C >= 0, C =< 16#ff).

options(Opts0) when is_list(Opts0) ->
    Opts = lists:foldr(fun expand_opt/2, [], Opts0),
    [RW_fun] =
        case opts(Opts, [reserved_word_fun], []) of
            badarg ->
                erlang:error(badarg, [Opts0]);
            R ->
                R
        end,
    Comment = proplists:get_bool(return_comments, Opts),
    WS = proplists:get_bool(return_white_spaces, Opts),
    Txt = proplists:get_bool(text, Opts),
    #erl_scan{resword_fun = RW_fun,
              comment     = Comment,
              ws          = WS,
              text        = Txt};
options(Opt) ->
    options([Opt]).

opts(Options, [Key|Keys], L) ->
    V = case lists:keyfind(Key, 1, Options) of
            {reserved_word_fun,F} when ?RESWORDFUN(F) ->
                {ok,F};
            {Key,_} ->
                badarg;
            false ->
                {ok,default_option(Key)}
        end,
    case V of
        badarg ->
            badarg;
        {ok,Value} ->
            opts(Options, Keys, [Value|L])
    end;
opts(_Options, [], L) ->
    lists:reverse(L).

default_option(reserved_word_fun) ->
    fun reserved_word/1.

expand_opt(return, Os) ->
    [return_comments,return_white_spaces|Os];
expand_opt(O, Os) ->
    [O|Os].

tokens1(Cs, St, Line, Col, Toks, Fun, Any) when ?STRING(Cs); Cs =:= eof ->
    case Fun(Cs, St, Line, Col, Toks, Any) of
        {more,{Cs0,Ncol,Ntoks,Nline,Nany,Nfun}} ->
            {more,{erl_scan_continuation,Cs0,Ncol,Ntoks,Nline,St,Nany,Nfun}};
        {ok,Toks0,eof,Nline,Ncol} ->
            Res = case Toks0 of
                      [] ->
                          {eof,location(Nline, Ncol)};
                      _ ->
                          {ok,lists:reverse(Toks0),location(Nline,Ncol)}
                  end,
            {done,Res,eof};
        {ok,Toks0,Rest,Nline,Ncol} ->
            {done,{ok,lists:reverse(Toks0),location(Nline, Ncol)},Rest};
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

string1(Cs, St, Line, Col, Toks) ->
    case scan1(Cs, St, Line, Col, Toks) of
        {more,{Cs0,Ncol,Ntoks,Nline,Any,Fun}} ->
            case Fun(Cs0++eof, St, Nline, Ncol, Ntoks, Any) of
                {ok,Toks1,_Rest,Line2,Col2} ->
                    {ok,lists:reverse(Toks1),location(Line2, Col2)};
                {{error,_,_}=Error,_Rest} ->
                    Error
            end;
        {ok,Ntoks,[_|_]=Rest,Nline,Ncol} ->
            string1(Rest, St, Nline, Ncol, Ntoks);
        {ok,Ntoks,_,Nline,Ncol} ->
            {ok,lists:reverse(Ntoks),location(Nline, Ncol)};
        {{error,_,_}=Error,_Rest} ->
            Error
    end.

scan(Cs, St, Line, Col, Toks, _) ->
    scan1(Cs, St, Line, Col, Toks).

scan1([$\s|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_spcs(Cs, St, Line, Col, Toks, 1);
scan1([$\s|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([$\n|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_newline(Cs, St, Line, Col, Toks);
scan1([$\n|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
scan1([C|Cs], St, Line, Col, Toks) when C >= $A, C =< $Z ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $a, C =< $z ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
%% Optimization: some very common punctuation characters:
scan1([$,|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ",", ',', 1);
scan1([$(|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "(", '(', 1);
scan1([$)|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ")", ')', 1);
scan1([${|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "{", '{', 1);
scan1([$}|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "}", '}', 1);
scan1([$[|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "[", '[', 1);
scan1([$]|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "]", ']', 1);
scan1([$;|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ";", ';', 1);
scan1([$_=C|Cs], St, Line, Col, Toks) ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
%% More punctuation characters below.
scan1([$\%|Cs], St, Line, Col, Toks) when not St#erl_scan.comment ->
    skip_comment(Cs, St, Line, Col, Toks, 1);
scan1([$\%=C|Cs], St, Line, Col, Toks) ->
    scan_comment(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C]);
scan1("..."++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "...", '...', 3);
scan1(".."=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
scan1(".."++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "..", '..', 2);
scan1("."=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
scan1([$.=C|Cs], St, Line, Col, Toks) ->
    scan_dot(Cs, St, Line, Col, Toks, [C]);
scan1([$"|Cs], St, Line, Col, Toks) -> %" Emacs
    State0 = {[],[],Line,Col},
    scan_string(Cs, St, Line, incr_column(Col, 1), Toks, State0);
scan1([$'|Cs], St, Line, Col, Toks) -> %' Emacs
    State0 = {[],[],Line,Col},
    scan_qatom(Cs, St, Line, incr_column(Col, 1), Toks, State0);
scan1([$$|Cs], St, Line, Col, Toks) ->
    scan_char(Cs, St, Line, Col, Toks);
scan1([$\r|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    white_space_end(Cs, St, Line, Col, Toks, 1, "\r");
scan1([C|Cs], St, Line, Col, Toks) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_atom(Cs, St, Line, Col, Toks, [C]);
scan1([C|Cs], St, Line, Col, Toks) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(Cs, St, Line, Col, Toks, [C]);
scan1([$\t|Cs], St, Line, Col, Toks) when St#erl_scan.ws ->
    scan_tabs(Cs, St, Line, Col, Toks, 1);
scan1([$\t|Cs], St, Line, Col, Toks) ->
    skip_white_space(Cs, St, Line, Col, Toks, 1);
scan1([C|Cs], St, Line, Col, Toks) when ?WHITE_SPACE(C) ->
    case St#erl_scan.ws of
        true ->
            scan_white_space(Cs, St, Line, Col, Toks, [C]);
        false ->
            skip_white_space(Cs, St, Line, Col, Toks, 1)
    end;
%% Punctuation characters and operators, first recognise multiples.
%% << <- <=
scan1("<<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<<", '<<', 2);
scan1("<-"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<-", '<-', 2);
scan1("<="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<=", '<=', 2);
scan1("<"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% >> >=
scan1(">>"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">>", '>>', 2);
scan1(">="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">=", '>=', 2);
scan1(">"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% -> --
scan1("->"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "->", '->', 2);
scan1("--"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "--", '--', 2);
scan1("-"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% ++
scan1("++"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "++", '++', 2);
scan1("+"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% =:= =/= =< == =>
scan1("=:="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=:=", '=:=', 3);
scan1("=:"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
scan1("=/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=/=", '=/=', 3);
scan1("=/"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
scan1("=<"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=<", '=<', 2);
scan1("=>"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=>", '=>', 2);
scan1("=="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "==", '==', 2);
scan1("="=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% /=
scan1("/="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "/=", '/=', 2);
scan1("/"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% ||
scan1("||"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "||", '||', 2);
scan1("|"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% :=
scan1(":="++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ":=", ':=', 2);
%% :: for typed records
scan1("::"++Cs, St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "::", '::', 2);
scan1(":"=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
%% Optimization: punctuation characters less than 127:
scan1([$=|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "=", '=', 1);
scan1([$:|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ":", ':', 1);
scan1([$||Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "|", '|', 1);
scan1([$#|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "#", '#', 1);
scan1([$/|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "/", '/', 1);
scan1([$?|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "?", '?', 1);
scan1([$-|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "-", '-', 1);
scan1([$+|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "+", '+', 1);
scan1([$*|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "*", '*', 1);
scan1([$<|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "<", '<', 1);
scan1([$>|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, ">", '>', 1);
scan1([$!|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "!", '!', 1);
scan1([$@|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "@", '@', 1);
scan1([$\\|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "\\", '\\', 1);
scan1([$^|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "^", '^', 1);
scan1([$`|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "`", '`', 1);
scan1([$~|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "~", '~', 1);
scan1([$&|Cs], St, Line, Col, Toks) ->
    tok2(Cs, St, Line, Col, Toks, "&", '&', 1);
%% End of optimization.
scan1([C|Cs], St, Line, Col, Toks) when ?UNI255(C) ->
    Str = [C],
    tok2(Cs, St, Line, Col, Toks, Str, list_to_atom(Str), 1);
scan1([C|Cs], _St, Line, Col, _Toks) when ?CHAR(C) ->
    Ncol = incr_column(Col, 1),
    scan_error({illegal,character}, Line, Col, Line, Ncol, Cs);
scan1([]=Cs, _St, Line, Col, Toks) ->
    {more,{Cs,Col,Toks,Line,[],fun scan/6}};
scan1(eof=Cs, _St, Line, Col, Toks) ->
    {ok,Toks,Cs,Line,Col}.

scan_atom(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more,Ncs} ->
            {more,{[],Col,Toks,Line,Ncs,fun scan_atom/6}};
        {Wcs,Cs} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    case (St#erl_scan.resword_fun)(Name) of
                        true ->
                            tok2(Cs, St, Line, Col, Toks, Wcs, Name);
                        false ->
                            tok3(Cs, St, Line, Col, Toks, atom, Wcs, Name)
                    end;
                _Error ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,atom}, Line, Col, Line, Ncol, Cs)
            end
    end.

scan_variable(Cs0, St, Line, Col, Toks, Ncs0) ->
    case scan_name(Cs0, Ncs0) of
        {more,Ncs} ->
            {more,{[],Col,Toks,Line,Ncs,fun scan_variable/6}};
        {Wcs,Cs} ->
            case catch list_to_atom(Wcs) of
                Name when is_atom(Name) ->
                    tok3(Cs, St, Line, Col, Toks, var, Wcs, Name);
                _Error ->
                    Ncol = incr_column(Col, length(Wcs)),
                    scan_error({illegal,var}, Line, Col, Line, Ncol, Cs)
            end
    end.

scan_name([C|Cs], Ncs) when C >= $a, C =< $z ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $A, C =< $Z ->
    scan_name(Cs, [C|Ncs]);
scan_name([$_=C|Cs], Ncs) ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when ?DIGIT(C) ->
    scan_name(Cs, [C|Ncs]);
scan_name([$@=C|Cs], Ncs) ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $ß, C =< $ÿ, C =/= $÷ ->
    scan_name(Cs, [C|Ncs]);
scan_name([C|Cs], Ncs) when C >= $À, C =< $Þ, C =/= $× ->
    scan_name(Cs, [C|Ncs]);
scan_name([], Ncs) ->
    {more,Ncs};
scan_name(Cs, Ncs) ->
    {lists:reverse(Ncs),Cs}.

-define(STR(St, S), if St#erl_scan.text -> S; true -> [] end).

scan_dot([$%|_]=Cs, St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, Ncs),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot([$\n=C|Cs], St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, ?STR(St, Ncs++[C])),
    {ok,[{dot,Anno}|Toks],Cs,Line+1,new_column(Col, 1)};
scan_dot([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    Anno = anno(Line, Col, St, ?STR(St, Ncs++[C])),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 2)};
scan_dot(eof=Cs, St, Line, Col, Toks, Ncs) ->
    Anno = anno(Line, Col, St, Ncs),
    {ok,[{dot,Anno}|Toks],Cs,Line,incr_column(Col, 1)};
scan_dot(Cs, St, Line, Col, Toks, Ncs) ->
    tok2(Cs, St, Line, Col, Toks, Ncs, '.', 1).

%%% White space characters are very common, so it is worthwhile to
%%% scan them fast and store them compactly. (The words "whitespace"
%%% and "white space" usually mean the same thing. The Erlang
%%% specification denotes the characters with ASCII code in the
%%% interval 0 to 32 as "white space".)
%%%
%%% Convention: if there is a white newline ($\n) it will always be
%%% the first character in the text string. As a consequence, there
%%% cannot be more than one newline in a white_space token string.
%%%
%%% Some common combinations are recognized, some are not. Examples
%%% of the latter are tab(s) followed by space(s), like "\t  ".
%%% (They will be represented by two (or more) tokens.)
%%%
%%% Note: the character sequence "\r\n" is *not* recognized since it
%%% would violate the property that $\n will always be the first
%%% character. (But since "\r\n\r\n" is common, it pays off to
%%% recognize "\n\r".)

scan_newline([$\s|Cs], St, Line, Col, Toks) ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\t|Cs], St, Line, Col, Toks) ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, 2);
scan_newline([$\r|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\r");
scan_newline([$\f|Cs], St, Line, Col, Toks) ->
    newline_end(Cs, St, Line, Col, Toks, 2, "\n\f");
scan_newline([], _St, Line, Col, Toks) ->
    {more,{[$\n],Col,Toks,Line,[],fun scan/6}};
scan_newline(Cs, St, Line, Col, Toks) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, "\n").

scan_nl_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 17 ->
    scan_nl_spcs(Cs, St, Line, Col, Toks, N+1);
scan_nl_spcs([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun scan_nl_spcs/6}};
scan_nl_spcs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_spcs(N)).

scan_nl_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 11 ->
    scan_nl_tabs(Cs, St, Line, Col, Toks, N+1);
scan_nl_tabs([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun scan_nl_tabs/6}};
scan_nl_tabs(Cs, St, Line, Col, Toks, N) ->
    newline_end(Cs, St, Line, Col, Toks, N, nl_tabs(N)).

%% Note: returning {more,Cont} is meaningless here; one could just as
%% well return several tokens. But since tokens() scans up to a full
%% stop anyway, nothing is gained by not collecting all white spaces.
scan_nl_white_space([$\n|Cs], #erl_scan{text = false}=St, Line, no_col=Col,
                    Toks0, Ncs) ->
    Toks = [{white_space,anno(Line),lists:reverse(Ncs)}|Toks0],
    scan_newline(Cs, St, Line+1, Col, Toks);
scan_nl_white_space([$\n|Cs], St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    Anno = anno(Line, Col, St, Ncs),
    Token = {white_space,Anno,Ncs},
    scan_newline(Cs, St, Line+1, new_column(Col, length(Ncs)), [Token|Toks]);
scan_nl_white_space([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    scan_nl_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_nl_white_space([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_nl_white_space/6}};
scan_nl_white_space(Cs, #erl_scan{text = false}=St, Line, no_col=Col,
                    Toks, Ncs) ->
    Anno = anno(Line),
    scan1(Cs, St, Line+1, Col, [{white_space,Anno,lists:reverse(Ncs)}|Toks]);
scan_nl_white_space(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    Anno = anno(Line, Col, St, Ncs),
    Token = {white_space,Anno,Ncs},
    scan1(Cs, St, Line+1, new_column(Col, length(Ncs)), [Token|Toks]).

newline_end(Cs, #erl_scan{text = false}=St, Line, no_col=Col,
            Toks, _N, Ncs) ->
    scan1(Cs, St, Line+1, Col, [{white_space,anno(Line),Ncs}|Toks]);
newline_end(Cs, St, Line, Col, Toks, N, Ncs) ->
    Anno = anno(Line, Col, St, Ncs),
    scan1(Cs, St, Line+1, new_column(Col, N), [{white_space,Anno,Ncs}|Toks]).

scan_spcs([$\s|Cs], St, Line, Col, Toks, N) when N < 16 ->
    scan_spcs(Cs, St, Line, Col, Toks, N+1);
scan_spcs([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun scan_spcs/6}};
scan_spcs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, spcs(N)).

scan_tabs([$\t|Cs], St, Line, Col, Toks, N) when N < 10 ->
    scan_tabs(Cs, St, Line, Col, Toks, N+1);
scan_tabs([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun scan_tabs/6}};
scan_tabs(Cs, St, Line, Col, Toks, N) ->
    white_space_end(Cs, St, Line, Col, Toks, N, tabs(N)).

skip_white_space([$\n|Cs], St, Line, Col, Toks, _N) ->
    skip_white_space(Cs, St, Line+1, new_column(Col, 1), Toks, 0);
skip_white_space([C|Cs], St, Line, Col, Toks, N) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, St, Line, Col, Toks, N+1);
skip_white_space([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun skip_white_space/6}};
skip_white_space(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

%% Maybe \t and \s should break the loop.
scan_white_space([$\n|_]=Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs));
scan_white_space([C|Cs], St, Line, Col, Toks, Ncs) when ?WHITE_SPACE(C) ->
    scan_white_space(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_white_space([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_white_space/6}};
scan_white_space(Cs, St, Line, Col, Toks, Ncs) ->
    white_space_end(Cs, St, Line, Col, Toks, length(Ncs), lists:reverse(Ncs)).

-compile({inline,[white_space_end/7]}).

white_space_end(Cs, St, Line, Col, Toks, N, Ncs) ->
    tok3(Cs, St, Line, Col, Toks, white_space, Ncs, Ncs, N).

scan_char([$\\|Cs]=Cs0, St, Line, Col, Toks) ->
    case scan_escape(Cs, incr_column(Col, 2)) of
        more ->
            {more,{[$$|Cs0],Col,Toks,Line,[],fun scan/6}};
        {error,Ncs,Error,Ncol} ->
            scan_error(Error, Line, Col, Line, Ncol, Ncs);
        {eof,Ncol} ->
            scan_error(char, Line, Col, Line, Ncol, eof);
        {nl,Val,Str,Ncs,Ncol} ->
            Anno = anno(Line, Col, St, ?STR(St, "$\\"++Str)), %"
            Ntoks = [{char,Anno,Val}|Toks],
            scan1(Ncs, St, Line+1, Ncol, Ntoks);
        {Val,Str,Ncs,Ncol} ->
            Anno = anno(Line, Col, St, ?STR(St, "$\\"++Str)), %"
            Ntoks = [{char,Anno,Val}|Toks],
            scan1(Ncs, St, Line, Ncol, Ntoks)
    end;
scan_char([$\n=C|Cs], St, Line, Col, Toks) ->
    Anno = anno(Line, Col, St, ?STR(St, [$$,C])),
    scan1(Cs, St, Line+1, new_column(Col, 1), [{char,Anno,C}|Toks]);
scan_char([C|Cs], St, Line, Col, Toks) when ?UNICODE(C) ->
    Anno = anno(Line, Col, St, ?STR(St, [$$,C])),
    scan1(Cs, St, Line, incr_column(Col, 2), [{char,Anno,C}|Toks]);
scan_char([C|_Cs], _St, Line, Col, _Toks) when ?CHAR(C) ->
    scan_error({illegal,character}, Line, Col, Line, incr_column(Col, 1), eof);
scan_char([], _St, Line, Col, Toks) ->
    {more,{[$$],Col,Toks,Line,[],fun scan/6}};
scan_char(eof, _St, Line, Col, _Toks) ->
    scan_error(char, Line, Col, Line, incr_column(Col, 1), eof).

scan_string(Cs, St, Line, Col, Toks, {Wcs,Str,Line0,Col0}) ->
    case scan_string0(Cs, St, Line, Col, $\", Str, Wcs) of %"
        {more,Ncs,Nline,Ncol,Nstr,Nwcs} ->
            State = {Nwcs,Nstr,Line0,Col0},
            {more,{Ncs,Ncol,Toks,Nline,State,fun scan_string/6}};
        {char_error,Ncs,Error,Nline,Ncol,EndCol} ->
            scan_error(Error, Nline, Ncol, Nline, EndCol, Ncs);
        {error,Nline,Ncol,Nwcs,Ncs} ->
            Estr = string:slice(Nwcs, 0, 16), % Expanded escape chars.
            scan_error({string,$\",Estr}, Line0, Col0, Nline, Ncol, Ncs); %"
        {Ncs,Nline,Ncol,Nstr,Nwcs} ->
            Anno = anno(Line0, Col0, St, Nstr),
            scan1(Ncs, St, Nline, Ncol, [{string,Anno,Nwcs}|Toks])
    end.

scan_qatom(Cs, St, Line, Col, Toks, {Wcs,Str,Line0,Col0}) ->
    case scan_string0(Cs, St, Line, Col, $\', Str, Wcs) of %'
        {more,Ncs,Nline,Ncol,Nstr,Nwcs} ->
            State = {Nwcs,Nstr,Line0,Col0},
            {more,{Ncs,Ncol,Toks,Nline,State,fun scan_qatom/6}};
        {char_error,Ncs,Error,Nline,Ncol,EndCol} ->
            scan_error(Error, Nline, Ncol, Nline, EndCol, Ncs);
        {error,Nline,Ncol,Nwcs,Ncs} ->
            Estr = string:slice(Nwcs, 0, 16), % Expanded escape chars.
            scan_error({string,$\',Estr}, Line0, Col0, Nline, Ncol, Ncs); %'
        {Ncs,Nline,Ncol,Nstr,Nwcs} ->
            case catch list_to_atom(Nwcs) of
                A when is_atom(A) ->
                    Anno = anno(Line0, Col0, St, Nstr),
                    scan1(Ncs, St, Nline, Ncol, [{atom,Anno,A}|Toks]);
                _ ->
                    scan_error({illegal,atom}, Line0, Col0, Nline, Ncol, Ncs)
            end
    end.

scan_string0(Cs, #erl_scan{text=false}, Line, no_col=Col, Q, [], Wcs) ->
    scan_string_no_col(Cs, Line, Col, Q, Wcs);
scan_string0(Cs, #erl_scan{text=true}, Line, no_col=Col, Q, Str, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Str, Wcs);
scan_string0(Cs, St, Line, Col, Q, [], Wcs) ->
    scan_string_col(Cs, St, Line, Col, Q, Wcs);
scan_string0(Cs, _St, Line, Col, Q, Str, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Str, Wcs).

%% Optimization. Col =:= no_col.
scan_string_no_col([Q|Cs], Line, Col, Q, Wcs) ->
    {Cs,Line,Col,_DontCare=[],lists:reverse(Wcs)};
scan_string_no_col([$\n=C|Cs], Line, Col, Q, Wcs) ->
    scan_string_no_col(Cs, Line+1, Col, Q, [C|Wcs]);
scan_string_no_col([C|Cs], Line, Col, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_no_col(Cs, Line, Col, Q, [C|Wcs]);
scan_string_no_col(Cs, Line, Col, Q, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Wcs, Wcs).

%% Optimization. Col =/= no_col.
scan_string_col([Q|Cs], St, Line, Col, Q, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    Str = ?STR(St, [Q|Wcs++[Q]]),
    {Cs,Line,Col+1,Str,Wcs};
scan_string_col([$\n=C|Cs], St, Line, _xCol, Q, Wcs) ->
    scan_string_col(Cs, St, Line+1, 1, Q, [C|Wcs]);
scan_string_col([C|Cs], St, Line, Col, Q, Wcs) when C =/= $\\, ?UNICODE(C) ->
    scan_string_col(Cs, St, Line, Col+1, Q, [C|Wcs]);
scan_string_col(Cs, _St, Line, Col, Q, Wcs) ->
    scan_string1(Cs, Line, Col, Q, Wcs, Wcs).

%% Note: in those cases when a 'char_error' tuple is returned below it
%% is tempting to skip over characters up to the first Q character,
%% but then the end location of the error tuple would not correspond
%% to the start location of the returned Rest string. (Maybe the end
%% location could be modified, but that too is ugly.)
scan_string1([Q|Cs], Line, Col, Q, Str0, Wcs0) ->
    Wcs = lists:reverse(Wcs0),
    Str = [Q|lists:reverse(Str0, [Q])],
    {Cs,Line,incr_column(Col, 1),Str,Wcs};
scan_string1([$\n=C|Cs], Line, Col, Q, Str, Wcs) ->
    Ncol = new_column(Col, 1),
    scan_string1(Cs, Line+1, Ncol, Q, [C|Str], [C|Wcs]);
scan_string1([$\\|Cs]=Cs0, Line, Col, Q, Str, Wcs) ->
    case scan_escape(Cs, Col) of
        more ->
            {more,Cs0,Line,Col,Str,Wcs};
        {error,Ncs,Error,Ncol} ->
            {char_error,Ncs,Error,Line,Col,incr_column(Ncol, 1)};
        {eof,Ncol} ->
            {error,Line,incr_column(Ncol, 1),lists:reverse(Wcs),eof};
        {nl,Val,ValStr,Ncs,Ncol} ->
            Nstr = lists:reverse(ValStr, [$\\|Str]),
            Nwcs = [Val|Wcs],
            scan_string1(Ncs, Line+1, Ncol, Q, Nstr, Nwcs);
        {Val,ValStr,Ncs,Ncol} ->
            Nstr = lists:reverse(ValStr, [$\\|Str]),
            Nwcs = [Val|Wcs],
            scan_string1(Ncs, Line, incr_column(Ncol, 1), Q, Nstr, Nwcs)
    end;
scan_string1([C|Cs], Line, no_col=Col, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, Line, Col, Q, [C|Str], [C|Wcs]);
scan_string1([C|Cs], Line, Col, Q, Str, Wcs) when ?UNICODE(C) ->
    scan_string1(Cs, Line, Col+1, Q, [C|Str], [C|Wcs]);
scan_string1([C|Cs], Line, Col, _Q, _Str, _Wcs) when ?CHAR(C) ->
    {char_error,Cs,{illegal,character},Line,Col,incr_column(Col, 1)};
scan_string1([]=Cs, Line, Col, _Q, Str, Wcs) ->
    {more,Cs,Line,Col,Str,Wcs};
scan_string1(eof, Line, Col, _Q, _Str, Wcs) ->
    {error,Line,Col,lists:reverse(Wcs),eof}.

-define(OCT(C), C >= $0, C =< $7).
-define(HEX(C), C >= $0 andalso C =< $9 orelse
                C >= $A andalso C =< $F orelse
                C >= $a andalso C =< $f).

%% \<1-3> octal digits
scan_escape([O1,O2,O3|Cs], Col) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,[O1,O2,O3],Cs,incr_column(Col, 3)};
scan_escape([O1,O2], _Col) when ?OCT(O1), ?OCT(O2) ->
    more;
scan_escape([O1,O2|Cs], Col) when ?OCT(O1), ?OCT(O2) ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,[O1,O2],Cs,incr_column(Col, 2)};
scan_escape([O1], _Col) when ?OCT(O1) ->
    more;
scan_escape([O1|Cs], Col) when ?OCT(O1) ->
    {O1 - $0,[O1],Cs,incr_column(Col, 1)};
%% \x{<hex digits>}
scan_escape([$x,${|Cs], Col) ->
    scan_hex(Cs, incr_column(Col, 2), []);
scan_escape([$x], _Col) ->
    more;
scan_escape([$x|eof], Col) ->
    {eof,incr_column(Col, 1)};
%% \x<2> hexadecimal digits
scan_escape([$x,H1,H2|Cs], Col) when ?HEX(H1), ?HEX(H2) ->
    Val = erlang:list_to_integer([H1,H2], 16),
    {Val,[$x,H1,H2],Cs,incr_column(Col, 3)};
scan_escape([$x,H1], _Col) when ?HEX(H1) ->
    more;
scan_escape([$x|Cs], Col) ->
    {error,Cs,{illegal,character},incr_column(Col, 1)};
%% \^X -> CTL-X
scan_escape([$^=C0,$\n=C|Cs], Col) ->
    {nl,C,[C0,C],Cs,new_column(Col, 1)};
scan_escape([$^=C0,C|Cs], Col) when ?CHAR(C) ->
    Val = C band 31,
    {Val,[C0,C],Cs,incr_column(Col, 2)};
scan_escape([$^], _Col) ->
    more;
scan_escape([$^|eof], Col) ->
    {eof,incr_column(Col, 1)};
scan_escape([$\n=C|Cs], Col) ->
    {nl,C,[C],Cs,new_column(Col, 1)};
scan_escape([C0|Cs], Col) when ?UNICODE(C0) ->
    C = escape_char(C0),
    {C,[C0],Cs,incr_column(Col, 1)};
scan_escape([C|Cs], Col) when ?CHAR(C) ->
    {error,Cs,{illegal,character},incr_column(Col, 1)};
scan_escape([], _Col) ->
    more;
scan_escape(eof, Col) ->
    {eof,Col}.

scan_hex([C|Cs], no_col=Col, Wcs) when ?HEX(C) ->
    scan_hex(Cs, Col, [C|Wcs]);
scan_hex([C|Cs], Col, Wcs) when ?HEX(C) ->
    scan_hex(Cs, Col+1, [C|Wcs]);
scan_hex(Cs, Col, Wcs) ->
    scan_esc_end(Cs, Col, Wcs, 16, "x{").

scan_esc_end([$}|Cs], Col, Wcs0, B, Str0) ->
    Wcs = lists:reverse(Wcs0),
    case catch erlang:list_to_integer(Wcs, B) of
        Val when ?UNICODE(Val) ->
            {Val,Str0++Wcs++[$}],Cs,incr_column(Col, 1)};
        _ ->
            {error,Cs,{illegal,character},incr_column(Col, 1)}
    end;
scan_esc_end([], _Col, _Wcs, _B, _Str0) ->
    more;
scan_esc_end(eof, Col, _Wcs, _B, _Str0) ->
    {eof,Col};
scan_esc_end(Cs, Col, _Wcs, _B, _Str0) ->
    {error,Cs,{illegal,character},Col}.

escape_char($n) -> $\n;                         % \n = LF
escape_char($r) -> $\r;                         % \r = CR
escape_char($t) -> $\t;                         % \t = TAB
escape_char($v) -> $\v;                         % \v = VT
escape_char($b) -> $\b;                         % \b = BS
escape_char($f) -> $\f;                         % \f = FF
escape_char($e) -> $\e;                         % \e = ESC
escape_char($s) -> $\s;                         % \s = SPC
escape_char($d) -> $\d;                         % \d = DEL
escape_char(C) -> C.

scan_number([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_number(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_number([$.,C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C,$.|Ncs]);
scan_number([$.]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_number/6}};
scan_number([$#|Cs]=Cs0, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(Ncs) of
        B when B >= 2, B =< 1+$Z-$A+10 ->
            Bcs = Ncs++[$#],
            scan_based_int(Cs, St, Line, Col, Toks, {B,[],Bcs});
        B ->
            Len = length(Ncs),
            scan_error({base,B}, Line, Col, Line, incr_column(Col, Len), Cs0)
    end;
scan_number([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_number/6}};
scan_number(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_integer(Ncs) of
        N when is_integer(N) ->
            tok3(Cs, St, Line, Col, Toks, integer, Ncs, N);
        _ ->
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,integer}, Line, Col, Line, Ncol, Cs)
    end.

scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when ?DIGIT(C), C < $0+B ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when C >= $A, B > 10, C < $A+B-10 ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([C|Cs], St, Line, Col, Toks, {B,Ncs,Bcs})
    when C >= $a, B > 10, C < $a+B-10 ->
    scan_based_int(Cs, St, Line, Col, Toks, {B,[C|Ncs],Bcs});
scan_based_int([]=Cs, _St, Line, Col, Toks, State) ->
    {more,{Cs,Col,Toks,Line,State,fun scan_based_int/6}};
scan_based_int(Cs, St, Line, Col, Toks, {B,Ncs0,Bcs}) ->
    Ncs = lists:reverse(Ncs0),
    case catch erlang:list_to_integer(Ncs, B) of
        N when is_integer(N) ->
            tok3(Cs, St, Line, Col, Toks, integer, Bcs++Ncs, N);
        _ ->
            Len = length(Bcs)+length(Ncs),
            Ncol = incr_column(Col, Len),
            scan_error({illegal,integer}, Line, Col, Line, Ncol, Cs)
    end.

scan_fraction([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_fraction(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_fraction([E|Cs], St, Line, Col, Toks, Ncs) when E =:= $e; E =:= $E ->
    scan_exponent_sign(Cs, St, Line, Col, Toks, [E|Ncs]);
scan_fraction([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_fraction/6}};
scan_fraction(Cs, St, Line, Col, Toks, Ncs) ->
    float_end(Cs, St, Line, Col, Toks, Ncs).

scan_exponent_sign([C|Cs], St, Line, Col, Toks, Ncs) when C =:= $+; C =:= $- ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_exponent_sign([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_exponent_sign/6}};
scan_exponent_sign(Cs, St, Line, Col, Toks, Ncs) ->
    scan_exponent(Cs, St, Line, Col, Toks, Ncs).

scan_exponent([C|Cs], St, Line, Col, Toks, Ncs) when ?DIGIT(C) ->
    scan_exponent(Cs, St, Line, Col, Toks, [C|Ncs]);
scan_exponent([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_exponent/6}};
scan_exponent(Cs, St, Line, Col, Toks, Ncs) ->
    float_end(Cs, St, Line, Col, Toks, Ncs).

float_end(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    case catch list_to_float(Ncs) of
        F when is_float(F) ->
            tok3(Cs, St, Line, Col, Toks, float, Ncs, F);
        _ ->
            Ncol = incr_column(Col, length(Ncs)),
            scan_error({illegal,float}, Line, Col, Line, Ncol, Cs)
    end.

skip_comment([C|Cs], St, Line, Col, Toks, N) when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            skip_comment(Cs, St, Line, Col, Toks, N+1);
        false ->
            Ncol = incr_column(Col, N+1),
            scan_error({illegal,character}, Line, Col, Line, Ncol, Cs)
    end;
skip_comment([]=Cs, _St, Line, Col, Toks, N) ->
    {more,{Cs,Col,Toks,Line,N,fun skip_comment/6}};
skip_comment(Cs, St, Line, Col, Toks, N) ->
    scan1(Cs, St, Line, incr_column(Col, N), Toks).

scan_comment([C|Cs], St, Line, Col, Toks, Ncs) when C =/= $\n, ?CHAR(C) ->
    case ?UNICODE(C) of
        true ->
            scan_comment(Cs, St, Line, Col, Toks, [C|Ncs]);
        false ->
            Ncol = incr_column(Col, length(Ncs)+1),
            scan_error({illegal,character}, Line, Col, Line, Ncol, Cs)
    end;
scan_comment([]=Cs, _St, Line, Col, Toks, Ncs) ->
    {more,{Cs,Col,Toks,Line,Ncs,fun scan_comment/6}};
scan_comment(Cs, St, Line, Col, Toks, Ncs0) ->
    Ncs = lists:reverse(Ncs0),
    tok3(Cs, St, Line, Col, Toks, comment, Ncs, Ncs).

tok2(Cs, #erl_scan{text = false}=St, Line, no_col=Col, Toks, _Wcs, P) ->
    scan1(Cs, St, Line, Col, [{P,anno(Line)}|Toks]);
tok2(Cs, St, Line, Col, Toks, Wcs, P) ->
    Anno = anno(Line, Col, St, Wcs),
    scan1(Cs, St, Line, incr_column(Col, length(Wcs)), [{P,Anno}|Toks]).

tok2(Cs, #erl_scan{text = false}=St, Line, no_col=Col, Toks, _Wcs, P, _N) ->
    scan1(Cs, St, Line, Col, [{P,anno(Line)}|Toks]);
tok2(Cs, St, Line, Col, Toks, Wcs, P, N) ->
    Anno = anno(Line, Col, St, Wcs),
    scan1(Cs, St, Line, incr_column(Col, N), [{P,Anno}|Toks]).

tok3(Cs, #erl_scan{text = false}=St, Line, no_col=Col, Toks, Item, _S, Sym) ->
    scan1(Cs, St, Line, Col, [{Item,anno(Line),Sym}|Toks]);
tok3(Cs, St, Line, Col, Toks, Item, String, Sym) ->
    Token = {Item,anno(Line, Col, St, String),Sym},
    scan1(Cs, St, Line, incr_column(Col, length(String)), [Token|Toks]).

tok3(Cs, #erl_scan{text = false}=St, Line, no_col=Col, Toks, Item,
     _String, Sym, _Length) ->
    scan1(Cs, St, Line, Col, [{Item,anno(Line),Sym}|Toks]);
tok3(Cs, St, Line, Col, Toks, Item, String, Sym, Length) ->
    Token = {Item,anno(Line, Col, St, String),Sym},
    scan1(Cs, St, Line, incr_column(Col, Length), [Token|Toks]).

scan_error(Error, Line, Col, EndLine, EndCol, Rest) ->
    Loc = location(Line, Col),
    EndLoc = location(EndLine, EndCol),
    scan_error(Error, Loc, EndLoc, Rest).

scan_error(Error, ErrorLoc, EndLoc, Rest) ->
    {{error,{ErrorLoc,?MODULE,Error},EndLoc},Rest}.

-compile({inline,[anno/4]}).

anno(Line, no_col, #erl_scan{text = false}, _String) ->
    anno(Line);
anno(Line, no_col, #erl_scan{text = true}, String) ->
    Anno = anno(Line),
    erl_anno:set_text(String, Anno);
anno(Line, Col, #erl_scan{text = false}, _String) ->
    anno({Line, Col});
anno(Line, Col, #erl_scan{text = true}, String) ->
    Anno = anno({Line, Col}),
    erl_anno:set_text(String, Anno).

location(Line, no_col) ->
    Line;
location(Line, Col) when is_integer(Col) ->
    {Line,Col}.

-compile({inline,[anno/1,incr_column/2,new_column/2]}).

anno(Location) ->
    erl_anno:new(Location).

incr_column(no_col=Col, _N) ->
    Col;
incr_column(Col, N) when is_integer(Col) ->
    Col + N.

new_column(no_col=Col, _Ncol) ->
    Col;
new_column(Col, Ncol) when is_integer(Col) ->
    Ncol.

nl_spcs(2)  -> "\n ";
nl_spcs(3)  -> "\n  ";
nl_spcs(4)  -> "\n   ";
nl_spcs(5)  -> "\n    ";
nl_spcs(6)  -> "\n     ";
nl_spcs(7)  -> "\n      ";
nl_spcs(8)  -> "\n       ";
nl_spcs(9)  -> "\n        ";
nl_spcs(10) -> "\n         ";
nl_spcs(11) -> "\n          ";
nl_spcs(12) -> "\n           ";
nl_spcs(13) -> "\n            ";
nl_spcs(14) -> "\n             ";
nl_spcs(15) -> "\n              ";
nl_spcs(16) -> "\n               ";
nl_spcs(17) -> "\n                ".

spcs(1)  -> " ";
spcs(2)  -> "  ";
spcs(3)  -> "   ";
spcs(4)  -> "    ";
spcs(5)  -> "     ";
spcs(6)  -> "      ";
spcs(7)  -> "       ";
spcs(8)  -> "        ";
spcs(9)  -> "         ";
spcs(10) -> "          ";
spcs(11) -> "           ";
spcs(12) -> "            ";
spcs(13) -> "             ";
spcs(14) -> "              ";
spcs(15) -> "               ";
spcs(16) -> "                ".

nl_tabs(2)  -> "\n\t";
nl_tabs(3)  -> "\n\t\t";
nl_tabs(4)  -> "\n\t\t\t";
nl_tabs(5)  -> "\n\t\t\t\t";
nl_tabs(6)  -> "\n\t\t\t\t\t";
nl_tabs(7)  -> "\n\t\t\t\t\t\t";
nl_tabs(8)  -> "\n\t\t\t\t\t\t\t";
nl_tabs(9)  -> "\n\t\t\t\t\t\t\t\t";
nl_tabs(10) -> "\n\t\t\t\t\t\t\t\t\t";
nl_tabs(11) -> "\n\t\t\t\t\t\t\t\t\t\t".

tabs(1)  ->  "\t";
tabs(2)  ->  "\t\t";
tabs(3)  ->  "\t\t\t";
tabs(4)  ->  "\t\t\t\t";
tabs(5)  ->  "\t\t\t\t\t";
tabs(6)  ->  "\t\t\t\t\t\t";
tabs(7)  ->  "\t\t\t\t\t\t\t";
tabs(8)  ->  "\t\t\t\t\t\t\t\t";
tabs(9)  ->  "\t\t\t\t\t\t\t\t\t";
tabs(10) ->  "\t\t\t\t\t\t\t\t\t\t".

-spec reserved_word(Atom :: atom()) -> boolean().
reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('try') -> true;
reserved_word('cond') -> true;
reserved_word('catch') -> true;
reserved_word('andalso') -> true;
reserved_word('orelse') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word(_) -> false.
