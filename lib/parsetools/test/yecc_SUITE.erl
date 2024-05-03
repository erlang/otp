%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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
-module(yecc_SUITE).

%-define(debug, true).

-include_lib("stdlib/include/erl_compile.hrl").
-include_lib("stdlib/include/assert.hrl").

-ifdef(debug).
-define(config(X,Y), foo).
-define(datadir, "yecc_SUITE_data").
-define(privdir, "yecc_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([app_test/1,
	 
	 file/1, syntax/1, compile/1, rules/1, expect/1,
	 conflicts/1, deterministic/1,
	 
	 empty/1, prec/1, yeccpre/1, lalr/1, old_yecc/1, 
	 other_examples/1,
	 
	 otp_5369/1, otp_6362/1, otp_7945/1, otp_8483/1, otp_8486/1,
	 
	 otp_7292/1, otp_7969/1, otp_8919/1, otp_10302/1, otp_11269/1,
         otp_11286/1, otp_14285/1, otp_17023/1, otp_17535/1]).

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, test_server:minutes(1)).

init_per_testcase(_Case, Config) ->
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test, {group, checks}, {group, examples},
     {group, bugs}, {group, improvements}].

groups() -> 
    [{checks, [],
      [file, syntax, compile, rules, expect, conflicts, deterministic]},
     {examples, [],
      [empty, prec, yeccpre, lalr, old_yecc, other_examples]},
     {bugs, [],
      [otp_5369, otp_6362, otp_7945, otp_8483, otp_8486, otp_17535]},
     {improvements, [], [otp_7292, otp_7969, otp_8919, otp_10302,
                         otp_11269, otp_11286, otp_14285, otp_17023]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


app_test(doc) ->
    ["Tests the applications consistency."];
app_test(suite) ->
    [];
app_test(Config) when is_list(Config) ->
    ok=test_server:app_test(parsetools),
    ok.


file(doc) ->
    "Bad files and options.";
file(suite) -> [];
file(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, false}],
    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file("not_a_file", Ret),
    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file("not_a_file", [{return,true}]),
    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file("not_a_file", [{report,false},return_errors]),
    error = yecc:file("not_a_file"),
    error = yecc:file("not_a_file", [{return,false},report]),
    error = yecc:file("not_a_file", [return_warnings,{report,false}]),
    Filename = filename:join(Dir, "file.yrl"),
    file:delete(Filename),

    {'EXIT', {badarg, _}} = (catch yecc:file({foo})),
    {'EXIT', {badarg, _}} = 
        (catch yecc:file(Filename, {parserfile,{foo}})),
    {'EXIT', {badarg, _}} = 
        (catch yecc:file(Filename, {includefile,{foo}})),

    {'EXIT', {badarg, _}} = (catch yecc:file(Filename, no_option)),
    {'EXIT', {badarg, _}} = 
        (catch yecc:file(Filename, [return | report])),
    {'EXIT', {badarg, _}} = 
        (catch yecc:file(Filename, {return,foo})),
    {'EXIT', {badarg, _}} = 
        (catch yecc:file(Filename, includefile)),

    Mini = <<"Nonterminals nt. 
              Terminals t.
              Rootsymbol nt.
              nt -> t.">>,
    ok = file:write_file(Filename, Mini),
    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file(Filename, [{parserfile,"//"} | Ret]),

    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file(Filename, [{includefile,"//"} | Ret]),
    {error,[{_,[{none,yecc,{file_error,_}}]}],[]} = 
        yecc:file(Filename, [{includefile,"/ /"} | Ret]),

    YeccPre = filename:join(Dir, "yeccpre.hrl"),
    ok = file:write_file(YeccPre, <<"syntax error. ">>),
    PreErrors1 = run_test(Config, Mini, YeccPre),
    {errors,[_],[]} = extract(YeccPre, PreErrors1),
    ok = file:write_file(YeccPre, my_yeccpre()),
    {'EXIT', {undef,_}} = (catch run_test(Config, Mini, YeccPre)),

    MiniCode = <<"
              Nonterminals nt. 
              Terminals t.
              Rootsymbol nt.
              nt -> t.
              Erlang code.
             ">>,
    {'EXIT', {undef,_}} = (catch run_test(Config, MiniCode, YeccPre)),

    file:delete(YeccPre),
    file:delete(Filename),

    ok.

syntax(doc) ->
    "Syntax checks.";
syntax(suite) -> [];
syntax(Config) when is_list(Config) ->
    Dir = ?privdir,
    %% Report errors. Very simple test of format_error/1.
    Ret = [return, {report, true}],
    Filename = filename:join(Dir, "file.yrl"),
    Parserfile = filename:join(Dir, "file.erl"),
    Parserfile1 = filename:join(Dir, "a file"),

    ok = file:write_file(Filename, <<"">>),
    {error,[{_,[{none,yecc,no_grammar_rules},
                      {none,yecc,nonterminals_missing},
                      {none,yecc,rootsymbol_missing},
                      {none,yecc,terminals_missing}]}],[]} = 
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"Nonterminals">>),
    {error,[{_,[{_,yecc,{error,yeccparser,_}}]}],[]} = 
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"Nonterminals nt.">>),
    {error,[{_,[{none,yecc,no_grammar_rules},
                      {none,yecc,rootsymbol_missing},
                      {none,yecc,terminals_missing}]}],[]} = 
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"Nonterminals nt. Terminals t.">>),
    {error,[{_,[{none,yecc,no_grammar_rules},
                      {none,yecc,rootsymbol_missing}]}],[]} = 
        yecc:file(Filename, Ret),

    %% Nonterminals and terminals not disjoint.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt 't t'. Terminals t 't t'. Rootsymbol nt.">>),
    {error,[{_,[{none,yecc,no_grammar_rules},
                {{1,17},yecc,{symbol_terminal_and_nonterminal,'t t'}}]}],
           []} = yecc:file(Filename, Ret),

    %% Rootsymbol is not a nonterminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. 
            Rootsymbol t. nt -> t.">>),
    {error,[{_,[{{2,24},yecc,{bad_rootsymbol,t}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Rootsymbol is not a nonterminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. 
            Rootsymbol t. nt -> t.">>),
    {error,[{_,[{{2,24},yecc,{bad_rootsymbol,t}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Endsymbol is a nonterminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. 
            Endsymbol nt.
            nt -> t.">>),
    {error,[{_,[{{2,23},yecc,{endsymbol_is_nonterminal,nt}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Endsymbol is a terminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. 
            Endsymbol t.
            nt -> t.">>),
    {error,[{_,[{{2,23},yecc,{endsymbol_is_terminal,t}}]}],[]} =
        yecc:file(Filename, Ret),

    %% No grammar rules.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. Endsymbol e.">>),
    {error,[{_,[{none,yecc,no_grammar_rules}]}],[]} = 
        yecc:file(Filename, Ret),

    %% Bad declaration.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. Endsymbol e.
            nt -> t. e e.">>),
    {ok,_,[{_,[{{2,22},yecc,bad_declaration}]}]} =
        yecc:file(Filename, Ret),

    %% Bad declaration with warnings_as_errors.
    ok = file:delete(Parserfile),
    error = yecc:file(Filename, [warnings_as_errors]),
    false = filelib:is_regular(Parserfile),
    error = yecc:file(Filename, [return_warnings,warnings_as_errors]),
    false = filelib:is_regular(Parserfile),
    {error,_,[{_,[{{2,22},yecc,bad_declaration}]}]} =
        yecc:file(Filename, [return_errors,warnings_as_errors]),
    false = filelib:is_regular(Parserfile),
    {ok,_,[{_,[{{2,22},yecc,bad_declaration}]}]} =
        yecc:file(Filename, [return_warnings]),
    true = filelib:is_regular(Parserfile),

    %% Bad declaration.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. 
            Rootsymbol nt nt. Rootsymbol nt. Endsymbol e.
            nt -> t.">>),
    {ok,_,[{_,[{{2,13},yecc,bad_declaration}]}]} =
        yecc:file(Filename, Ret),
    {ok,_,[{_,[{2,yecc,bad_declaration}]}]} =
        yecc:file(Filename, [{error_location, line} | Ret]),

    %% Syntax error found by yeccparser.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. Endsymbol e.
            a - a.">>),
    {error,[{_,[{{2,15},yecc,{error,_,_}}]}],[]} =
        yecc:file(Filename, Ret),
    {error,[{_,[{2,yecc,{error,_,_}}]}],[]} =
        yecc:file(Filename, [{error_location, line} | Ret]),

    %% Syntax error: unknown nonterminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol nt. Endsymbol e.
            'unknown ' -> t.">>),
    {error,[{_,[{{2,13},yecc,{undefined_nonterminal,'unknown '}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Undefined rhs symbols. Note quotes in output.
    ok = file:write_file(Filename, 
         <<"Nonterminals Nonterminals nt. 
            Terminals t Terminals. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> Nonterminals.
            Nonterminals -> Terminals receive foo 45 
                            '17' 'a b'.">>),
    {error,[{_,[{{6,39},yecc,{undefined_symbol,'receive'}},
                {{6,47},yecc,{undefined_symbol,foo}},
                {{6,51},yecc,{undefined_symbol,45}},
                {{7,29},yecc,{undefined_symbol,'17'}},
                {{7,34},yecc,{undefined_symbol,'a b'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% '$empty' used early, before Terminals. OK.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. 
            nt -> '$empty'.
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.">>),
    {ok,_,[{_,[{{3,23},yecc,{unused_terminal,t}}]}]} =
        yecc:file(Filename, Ret),

    %% Illegal use of '$empty'.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt nt2. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t.
            nt2 -> t '$empty'.">>),
    {error,[{_,[{{6,22},yecc,illegal_empty}]}],[]} =
        yecc:file(Filename, Ret),

    ParserFile3 = [{parserfile, Parserfile1}],

    %% Bad Erlang expression in action. Changed in OTP-7224.
    ok = file:write_file(Filename,
         <<"Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t : a bad code.">>),
    {ok, _, []} = yecc:file(Filename, ParserFile3 ++ Ret),

    SzYeccPre = yeccpre_size(),
    %% Note: checking the line numbers. Changes when yeccpre.hrl changes.
    fun() ->
            {error,[{_,[{{5,25},_,["syntax error before: ","bad"]}]},
                    {_,[{{L1,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{L2,_},_,{bad_inline,{yeccpars2_2_,1}}},
                        {{_,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{_,_},_,{bad_nowarn_unused_function,{yeccpars2_2_,1}}}]}],
             []} = compile:file(Parserfile1, [basic_validation,return]),
            ?assertEqual(L1, 38 + SzYeccPre),
            ?assertEqual(L2, 47 + SzYeccPre)
    end(),

    %% Bad macro in action. OTP-7224.
    ok = file:write_file(Filename,
         <<"Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t : ?F(3).">>),
    {ok, _, []} = yecc:file(Filename, ParserFile3 ++ Ret),
    %% Note: checking the line numbers. Changes when yeccpre.hrl changes.
    fun() ->
            {error,[{_,[{{5,24},_,{undefined,'F',1}}]},
                    {_,[{{L1,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{L2,_},_,{bad_inline,{yeccpars2_2_,1}}},
                        {{_,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{_,_},_,{bad_nowarn_unused_function,{yeccpars2_2_,1}}}]}],
             []} = compile:file(Parserfile1, [basic_validation,return]),
            ?assertEqual(L1, 38 + SzYeccPre),
            ?assertEqual(L2, 47 + SzYeccPre)
    end(),

    %% Check line numbers. OTP-7224.
    ok = file:write_file(Filename,
         <<"Terminals t. 
            Nonterminals nt. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t : ?F(3).
            Erlang code.
            -define(F(X), X).
            t() ->
               bad().">>),
    {ok, _, []} = yecc:file(Filename, ParserFile3 ++ Ret),
    {error,[{_,[{{9,16},_,{undefined_function,{bad,0}}}]}],
           [{_,[{{8,13},_,{unused_function,{t,0}}}]}]}
        = compile:file(Parserfile1, [basic_validation, return]),

    %% Terminals defined before nonterminals. (One of many checks...)
    %% Used to give an error message, but now allowed.
    ok = file:write_file(Filename,
         <<"Terminals t. 
            Nonterminals nt. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t.
            Erlang code.">>),
    {ok, _, []} = yecc:file(Filename, Ret),

    %% Precedence with swapped arguments. Bad declaration.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t.
            Right t. 
            Left nt 100.">>),
    {ok,_,[{_,[{{6,13},yecc,bad_declaration},
               {{7,13},yecc,bad_declaration}]}]} =
        yecc:file(Filename, Ret),

    %% Precedence with unknown operator.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t.
            Unary 100 '-'.">>),
    {error,[{_,[{{6,23},yecc,{precedence_op_is_unknown,'-'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Precedence with endsymbol operator.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> t.
            Unary 100 e.">>),
    {error,[{_,[{{6,23},yecc,{precedence_op_is_endsymbol,e}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Duplicated precedence.
    ok = file:write_file(Filename, <<"
            Nonterminals nt. 
            Terminals t '+'. 
            Rootsymbol nt. 
            nt -> t '+' nt.
            Left 100 '+'.
            Right 200 t.
            Left 200 '+'.
            Left 200 '+'.
            Right 200 t.">>),
    {error,[{_,[{{8,22},yecc,{duplicate_precedence,'+'}},
                {{9,22},yecc,{duplicate_precedence,'+'}},
                {{10,23},yecc,{duplicate_precedence,t}}]}],
           []} = yecc:file(Filename, Ret),

    %% Duplicated nonterminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals 'n t' 'n t'. Terminals t. 
            Rootsymbol 'n t'. 'n t' -> t.">>),
    {ok, _, [{_,[{{1,14},yecc,{duplicate_nonterminal,'n t'}}]}]} =
        yecc:file(Filename, Ret),
    {ok, _, [{_,[{{1,14},yecc,{duplicate_nonterminal,'n t'}}]}]} =
        yecc:file(Filename, [return_warnings, {report, false}]),
    {ok, _} = yecc:file(Filename),
    {ok, _} = 
        yecc:file(Filename, [{report,false}, {return_warnings,false}]),

    %% Duplicated terminal.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals 't t' 't t'. 
            Rootsymbol nt. nt -> 't t'.">>),
    {ok, _, [{_,[{{1,28},yecc,{duplicate_terminal,'t t'}}]}]} =
       yecc:file(Filename, Ret),

    %% Two Nonterminals declarations.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. 
            Nonterminals nt2.
            Rootsymbol nt2. nt -> t. nt2 -> nt.">>),
    {error,[{_,[{{2,13},yecc,{duplicate_declaration,'Nonterminals'}}]}],
           []} = yecc:file(Filename, Ret),

    %% Three Terminals declarations.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. 
            Terminals t1.
            Terminals t1.
            Rootsymbol nt. nt -> t t1.">>),
    {error,[{_,[{{2,13},yecc,{duplicate_declaration,'Terminals'}},
                {{3,13},yecc,{duplicate_declaration,'Terminals'}}]}],
           []} = yecc:file(Filename, Ret),

    %% Two root symbols.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol t. 
            Rootsymbol nt. nt -> t.">>),
    {error,[{_,[{{2,13},yecc,{duplicate_declaration,'Rootsymbol'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Two end symbols.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol t. 
            Endsymbol e. 
            Endsymbol e. nt -> t.">>),
    {error,[{_,[{{3,13},yecc,{duplicate_declaration,'Endsymbol'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Two end symbols.
    ok = file:write_file(Filename, 
         <<"Nonterminals nt. Terminals t. Rootsymbol t. 
            Expect 1.
            Expect 0.
            Endsymbol e. nt -> t.">>),
    {error,[{_,[{{3,13},yecc,{duplicate_declaration,'Expect'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Some words should not be used.
    ok = file:write_file(Filename, <<"
            Terminals '$empty' '$end'.
            Nonterminals '$undefined'.
            Rootsymbol '$undefined'.
            Endsymbol '$end'.
            '$undefined' -> '$empty'.
          ">>),
    {error,[{_,[{{2,23},yecc,{reserved,'$empty'}},
                {{2,32},yecc,{reserved,'$end'}},
                {{3,26},yecc,{reserved,'$undefined'}},
                {{5,23},yecc,{endsymbol_is_terminal,'$end'}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Undefined pseudo variable.
    ok = file:write_file(Filename,<<"
              Nonterminals nt. 
              Terminals t.
              Rootsymbol nt.
              nt -> t : '$2'.
          ">>),
    {error,[{_,[{{5,25},yecc,{undefined_pseudo_variable,'$2'}}]}],[]} =
        yecc:file(Filename, Ret),
    
    %% Space in module name.
    ok = file:write_file(Filename, <<"
        Nonterminals list.
        Terminals element.
        Rootsymbol list.

        list -> element : {single, '$1'}.
        list -> list element : {pair, '$1', '$2'}.

        Erlang code.

        -export([t/0]).

        t() ->
            L = [{element, 1}, {element, 2}, {element, 3}],
            {ok,_} = parse(L),
            ok.
       ">>),

    Parserfile2 = filename:join(Dir, "a \"file\""),
    %% The parser (yeccgramm.yrl) allows many symbol names...
    ok = file:write_file(Filename, <<"
        Nonterminals Nonterminals Rootsymbol ':' '->'. 
        Terminals Terminals. 
        Rootsymbol Rootsymbol. 
        Endsymbol e.
        Rootsymbol -> Nonterminals ':' '->'.
        Nonterminals -> Terminals.
        ':' -> '->'.
        '->' -> Terminals.
       ">>),
    {ok, _} = yecc:file(Filename, [{parserfile, Parserfile1}]),
    {ok, _} = yecc:file(Filename, [{parserfile, Parserfile1},time]),
    {ok,[]} = compile:file(Parserfile1, [basic_validation]),

    Quotes = <<"
            Nonterminals 
            tail try 17 42 Unused ' unused ' '42' 'fnutt\\'' receive.

            Terminals 
            ']' '|' ',' '\"hi\"' 'there\\'' 'you\\'' ACCEPT.

            Rootsymbol 17.

            Endsymbol '$end'.

            17 -> try : '$1':more([\"hi \\\" there\", '.', 3.14, % $3 , % '$1' is a module
                                    -104, ' ', ' a', '->', $a, $\t]), '$1'.

            try -> tail 'you\\'' 'fnutt\\''.

            'fnutt\\'' -> 'you\\'' ACCEPT.

            '42' -> tail.

            tail -> ']' : 
                           (fun(Strange) -> foo end)(apa).
            tail -> '|' try ']' : 
                           (fun(Strange) -> foo;
                               (_) -> 'when' % warning
                            end)('ap a', foo),
                           %% This line intentionally left blank.
                           R = #rec{},
                           A = R#rec.a,
                           <<\"hi\">> = <<\"hi\">>,
                           there.
            tail -> ',' try tail : {cons,line('$2'),'$2','$3'}.


            Erlang code.

            -export([t/0]).

            -record(rec, {a}).

            just(Some, Code) ->
                true.

            more(Code) ->
                io:format(\"~p~n\", [Code]), true.

            line(_) ->
                17.

            t() ->
                ok. % compiled successfully...

             ">>,

    ok = file:write_file(Filename, Quotes),
    {ok,_,[{_,
            [{{3,25},yecc,{unused_nonterminal,42}},
             {{3,28},yecc,{unused_nonterminal,'Unused'}},
             {{3,35},yecc,{unused_nonterminal,' unused '}},
             {{3,46},yecc,{unused_nonterminal,'42'}},
             {{3,61},yecc,{unused_nonterminal,'receive'}},
             {{6,25},yecc,{unused_terminal,'"hi"'}},
             {{6,32},yecc,{unused_terminal,'there\''}}]}]} =
        yecc:file(Filename, Ret),

    Ts = [{quotes, Quotes, default, ok}],
    run(Config, Ts),

    %% Non-terminal has no rules, but is unused.
    ok = file:write_file(Filename,<<"
              Nonterminals nt bad. 
              Terminals t. 
              Rootsymbol nt. 

              nt -> t : something.
          ">>),
    {ok,_,[{_,[{{2,31},yecc,{unused_nonterminal,bad}}]}]} =
        yecc:file(Filename, Ret),

    %% Non-terminal has no rules and is used.
    ok = file:write_file(Filename,<<"
              Nonterminals nt bad. 
              Terminals t. 
              Rootsymbol nt. 

              nt -> t bad : something.
          ">>),
    {error,[{_,[{{2,31},yecc,{missing_syntax_rule,bad}}]}],[]} =
        yecc:file(Filename, Ret),

    %% Warning in Erlang code. With and without file attributes.
    ok = file:write_file(Filename,<<"
              Nonterminals nt. 
              Terminals t. 
              Rootsymbol nt. 

              nt -> t : t(17).

              Erlang code.

              t(A) ->
                  b.
          ">>),
    {ok, _, []} = 
        yecc:file(Filename, [file_attributes | Ret]),
    Opts = [basic_validation, return],
    Erlfile = filename:join(Dir, "file.erl"),
    {ok,[],[{_,[{{10,17},_,_}]}]} = compile:file(Erlfile, Opts),
    {ok, _, []} = 
        yecc:file(Filename, [{file_attributes,false} | Ret]),
    {ok,[],[{_,[{{4,17},_,_}]}]} = compile:file(Erlfile, Opts),

    file:delete(Parserfile1 ++ ".erl"),
    file:delete(Parserfile2 ++ ".erl"),
    file:delete(Filename),

    ok.

compile(doc) ->
    "Check of compile/3.";
compile(suite) -> [];
compile(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.yrl"),
    Parserfile = filename:join(Dir, "file.erl"),
    ok = file:write_file(Filename, 
                               <<"Nonterminals nt. 
                                  Terminals t.
                                  Rootsymbol nt.
                                  nt -> t.">>),
    error = yecc:compile(Filename, "//", #options{}),
    ok = yecc:compile(Filename, Parserfile, #options{}),
    file:delete(Parserfile),
    file:delete(Filename),
    ok.

rules(doc) ->
    "Check of rules.";
rules(suite) -> [];
rules(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, false}],
    Filename = filename:join(Dir, "file.yrl"),

    ok = file:write_file(Filename,
      <<"Nonterminals nt. Terminals t. Rootsymbol nt. 
         nt -> t. nt -> t.">>),
    {error,[{_,[{none,yecc,{conflict,_}}]}],
           [{_,[{none,yecc,{conflicts,0,1}}]}]} = 
        yecc:file(Filename, [return, report]),

    ok = file:write_file(Filename, <<"
                Nonterminals A B E.
                Terminals c d f x y.
                Rootsymbol A.

                A -> B c d.
                A -> E c f.
                B -> x y.
                E -> x y.
                ">>),
    {error,[{_,[{none,yecc,{conflict,_}}]}],
           [{_,[{none,yecc,{conflicts,0,1}}]}]} = 
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename,<<"
              Terminals t.
              Nonterminals nt.
              Rootsymbol nt.
              nt -> '$empty' : '$1'.
              nt -> t.
          ">>),
    {error,[{_,[{{5,32},yecc,{undefined_pseudo_variable,'$1'}}]}],[]} =
        yecc:file(Filename, Ret),
    
    ok = file:write_file(Filename,<<"
              Terminals t.
              Nonterminals nt.
              Rootsymbol nt.
              nt -> '$empty' : '$0'.
              nt -> t.
          ">>),
    {error,[{_,[{{5,32},yecc,{undefined_pseudo_variable,'$0'}}]}],[]} =
        yecc:file(Filename, Ret),

    file:delete(Filename),
    
    %% No precedences.
    Ts = [{rules_1,<<"
              Nonterminals exp.
              Terminals number '*' '+' '(' ')'.
              Rootsymbol exp.

              exp -> exp '+' exp : {plus,'$1','$3'}.
              exp -> exp '*' exp : {times,'$1','$3'}.
              exp -> number : element(2, '$1').
              exp -> '(' exp ')' : '$2'.

              Erlang code.

              -export([t/0]).

              t() ->
                  {ok, {times, 1, {plus, 3, 5}}} = 
                    parse([{number,1}, {'*',2}, {number,3}, 
                                       {'+',4}, {number,5}]),
                  ok.
          ">>,
         default,
         ok}],
    run(Config, Ts),
    ok.


expect(doc) ->
    "Check of expect.";
expect(suite) -> [];
expect(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, true}],
    Filename = filename:join(Dir, "file.yrl"),

    ok = file:write_file(Filename, <<"
                Nonterminals e. 
                Terminals i t else. 
                Rootsymbol e. 
                Expect a.

                e -> i e t e.
                e -> i e t e else e.
                ">>),
    {error,[{_,[{{5,24},yecc,{bad_expect,a}}]}],[]} =
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"
                Nonterminals e. 
                Terminals i t else. 
                Rootsymbol e. 
                Expect 1.

                e -> i e t e.
                e -> i e t e else e.
                ">>),
    {ok, _, []} = yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"
                Nonterminals e. 
                Terminals i t else. 
                Rootsymbol e. 
                Expect 2.

                e -> i e t e.
                e -> i e t e else e.
                ">>),
    {ok, _, [{_,[{none,yecc,{conflicts,1,0}}]}]} = 
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename, <<"
                Nonterminals e. 
                Terminals i t else. 
                Rootsymbol e. 
                Expect -2.

                e -> i e t e.
                e -> i e t e else e.
                ">>),
    {error,[{_,[{{5,24},yecc,{error,_yeccparser,_}}]}],[]} =
        yecc:file(Filename, Ret),

    %% States N. An undocumented declaration used for testing.
    ok = file:write_file(Filename, <<"
                Nonterminals nt. 
                Terminals t.
                Rootsymbol nt.
                States 100.
                nt -> t.">>),
    {ok,_,[{_, [{none,yecc,{n_states,100,3}}]}]} = 
        yecc:file(Filename, Ret),
    
    ok = file:write_file(Filename, <<"
                Nonterminals nt. 
                Terminals t.
                Rootsymbol nt.
                States bad.
                nt -> t.">>),
    {error,[{_,[{{5,24},yecc,{bad_states,bad}}]}],[]} =
        yecc:file(Filename, Ret),
    
    file:delete(Filename),
    ok.

conflicts(doc) ->
    "Shift/reduce and reduce/reduce conflicts.";
conflicts(suite) -> [];
conflicts(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, true}],
    Filename = filename:join(Dir, "file.yrl"),

    ok = file:write_file(Filename, <<"
            Nonterminals S List Tuple Elements Element.
            Terminals '{' '}' '[' ']' ',' nil e.
            Rootsymbol S. 

            S -> '$empty' : empty.
            S -> List : '$1'.

            List -> '$empty' : [].
            List -> nil : [].
            List -> '[' Tuple ']' : {list, '$2'}.

            Tuple -> '$empty' : {}.
            Tuple -> '{' '}' : {}.
            Tuple -> '{' Elements '}' : {tuple, '$2'}.

            Elements -> '$empty' : none.
            Elements -> Elements ',' Element : {elements, '$3', '$1'}.
            Elements -> Element : '$1'.

            Element -> List : '$1'.
            Element -> Tuple : '$1'.
            Element -> e : '$1'.
           ">>),
    {error,[{_,_}],[{_,[{none,yecc,{conflicts,1,5}}]}]} = 
        yecc:file(Filename, Ret),

    %% Can easily be resolved (but don't do it!).
    ok = file:write_file(Filename, <<"
            Nonterminals S List Tuple Elements Element.
            Terminals '{' '}' '[' ']' ',' nil e.
            Rootsymbol S. 

            Right 100 List.

            S -> '$empty' : empty.
            S -> List : '$1'.

            List -> '$empty' : [].
            List -> nil : [].
            List -> '[' Tuple ']' : {list, '$2'}.

            Tuple -> '$empty' : {}.
            Tuple -> '{' '}' : {}.
            Tuple -> '{' Elements '}' : {tuple, '$2'}.

            Elements -> '$empty' : none.
            Elements -> Elements ',' Element : {elements, '$3', '$1'}.
            Elements -> Element : '$1'.

            Element -> List : '$1'.
            Element -> Tuple : '$1'.
            Element -> e : '$1'.
           ">>),
    {ok, _, []} = 
        yecc:file(Filename, Ret),

    file:delete(Filename),
    ok.

deterministic(doc) ->
    "Check yecc respects the +deterministic flag.";
deterministic(suite) -> [];
deterministic(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.yrl"),
    Parserfile = filename:join(Dir, "file.erl"),
    ok = file:write_file(Filename,
                               <<"Nonterminals nt.
                                  Terminals t.
                                  Rootsymbol nt.
                                  nt -> t.">>),

    %% Generated yecc parsers need to include the yeccpre.hrl
    %% header file, so we'll get a -file attribute corresponding
    %% to that include. In deterministic mode, that include should
    %% only use the basename, "yeccpre.hrl", but otherwise, it should
    %% contain the full path.

    %% Matches when OTP is not installed (e.g. /lib/parsetools/include/yeccpre.hrl)
    %% and when it is (e.g. /lib/parsetools-2.3.2/include/yeccpre.hrl)
    AbsolutePathSuffix = "/lib/parsetools.*/include/yeccpre\.hrl",

    ok = yecc:compile(Filename, Parserfile, #options{specific=[deterministic]}),
    {ok, FormsDet} = epp:parse_file(Parserfile,[]),
    ?assertMatch(false, search_for_file_attr(AbsolutePathSuffix, FormsDet)),
    ?assertMatch({value, _}, search_for_file_attr("yeccpre\.hrl", FormsDet)),
    file:delete(Parserfile),

    ok = yecc:compile(Filename, Parserfile, #options{}),
    {ok, Forms} = epp:parse_file(Parserfile,[]),
    ?assertMatch({value, _}, search_for_file_attr(AbsolutePathSuffix, Forms)),
    file:delete(Parserfile),

    file:delete(Filename),
    ok.

empty(doc) ->
    "'$empty'.";
empty(suite) -> [];
empty(Config) when is_list(Config) ->
    Ts = [{empty_1, <<"
            Nonterminals nt. 
            Terminals t. 
            Rootsymbol nt. 
            Endsymbol e.
            nt -> '$empty': nothing.
            nt -> t : something.

            Erlang code.

            -export([t/0]).

            t() ->
                {ok, nothing} = parse([{e,2}]),
                {ok, something} = parse([{t,1},{e,2}]),
                ok.">>,
           default,
           ok},
          {empty_2, <<"
            %% There used to be a bug when it came to the left
            %% corners. In this example rule 2 is a prefix of rule 1
            %% such that the rule 2 generates $empty but rule 1 does
            %% not. The old buggy code seemingly worked well too.
            Nonterminals top A B C D.
            Terminals t.
            Rootsymbol top.

            top -> A B C D : {'$1','$2','$3','$4'}.
            top -> A B C : {'$1','$2','$3'}.

            A -> '$empty' : e.
            B -> '$empty' : e.
            C -> '$empty' : e.

            D -> t : t.

            Erlang code.

            -export([t/0]).

            t() ->
                {ok,{e,e,e,t}} = parse([{t, 1}]),
                {ok,{e,e,e}} = parse([]),
                ok.
           ">>,
           default,
           ok}],
    run(Config, Ts),
    ok.

prec(doc) ->
    "Precedence.";
prec(suite) -> [];
prec(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, false}],
    Filename = filename:join(Dir, "file.yrl"),

    %% Don't know what 'Unary' actually means, but this is how it has
    %% always worked...
    ok = file:write_file(Filename, <<"
                Nonterminals E.
                Terminals '*' '+' '=' integer.
                Rootsymbol E.

                E -> E '=' E : {op, '=', '$1', '$3'}.
                E -> E '*' E : {op, '*', '$1', '$3'}.
                E -> E '+' E  : {op, '+', '$1', '$3'}.
                E -> integer : '$1'.

                Unary 100 '='.
                Left 200 '+'.
                Left 400 '*'.
              ">>),
    {error,[{_,[{none,yecc,{conflict,_}}]}],
           [{_,[{none,yecc,{conflicts,1,0}}]}]} = 
        yecc:file(Filename, Ret),

    Ts = [{prec_1, <<"
        Nonterminals E Expr Uminus.
        Terminals '=' '+' '*' '-' '(' ')' integer var dot.
        Rootsymbol Expr.

        Expr -> E dot : '$1'.
        E -> var '=' E : {match, line_of('$1'), '$1', '$3'}.
        E -> E '+' E : {op, line_of('$1'), '+', '$1', '$3'}.
        E -> E '-' E : {op, line_of('$1'), '-', '$1', '$3'}.
        E -> E '*' E : {op, line_of('$1'), '*', '$1', '$3'}.
        E -> Uminus : '$1'.
        E -> '(' E ')' : '$2'.
        E -> integer : '$1'.

        Uminus -> '-' E :   {op, line_of('$1'), '-', '$2'}.

        Right 200 '='.
        Left 300 '+'.
        Left 300 '-'.
        Left 400 '*'.
        Unary 500 Uminus.

        Erlang code.

        -export([t/0, t/1]).

        line_of(Token) ->
            element(2, Token).

        t() ->
            {ok, -56} = t(\"A = (4 + 3) * - 8. \"),
            {ok, 28} = t(\"A = 4 + B=3 * 8. \"),
            {ok, 28} = t(\"4 - 3 * - 8. \"),
            {ok, -20} = t(\"4 - 3 * 8. \"),
            {ok, 2} = t(\"1 - - 1.\"),
            ok.

        t(S) ->
            case erl_scan:string(S) of
                {ok, Tokens, _Line} ->
                    case parse(Tokens) of
                        {ok, Expr} ->
                            case catch erl_eval:expr(Expr, []) of
                                {value, Value, _Bs} ->
                                    {ok, Value};
                                {'EXIT', Reason} ->
                                    {error, Reason}
                            end;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end.
        ">>,
        default,
        ok},

          {prec_2, <<"
        Nonterminals E uminus.
        Terminals '*' '-' integer.
        Rootsymbol E.

        E -> E '-' E : {op, '-', '$1', '$3'}.
        E -> E '*' E : {op, '*', '$1', '$3'}.
        E -> uminus: '$1'.
        E -> integer : '$1'.

        uminus -> '-' E :  {op, '-', '$2'}.

        Left 200 '-'.
        Left 400 '*'.
        Unary 500 uminus.

        Erlang code.

        -export([t/0]).

        t() ->
            %% This used to be parsed -(4 * 3), but that has been corrected:
            {ok,{op,'*',{op,'-',{integer,1,4}},{integer,3,12}}} = 
                parse([{'-',0},{integer,1,4},{'*',2},{integer,3,12}]),

            {ok,{op,'-',{op,'-',{integer,1,4}},{integer,3,12}}} = 
                parse([{'-',0},{integer,1,4},{'-',2},{integer,3,12}]),
            {ok,{op,'*',{op,'-',{op,'-',{integer,2,4}}},{integer,4,12}}} = 
                parse([{'-',0},{'-',1},{integer,2,4},{'*',3},{integer,4,12}]),
            {ok,{op,'-',{integer,1,4},{op,'-',{integer,4,12}}}} = 
                parse([{integer,1,4},{'-',2},{'-',3},{integer,4,12}]),
            ok.
        ">>,
        default,
        ok},
          {prec_3, <<"
            Nonterminals nt. 
            Terminals '(' ')' t op. 
            Rootsymbol nt. 

            Nonassoc 100 op.

            nt -> nt op nt : {'$2', '$1', '$3'}.
            nt -> '(' nt ')' : '$2'.
            nt -> t : '$1'.

            Erlang code.

            -export([t/0]).

            t() ->
                {error,{4,yecc_test,[\"syntax error before: \",\"op\"]}} = 
                    parse([{t,1},{op,2},{t,3},{op,4},{t,5}]),
                {ok,{{op,2},{t,1},{{op,5},{t,4},{t,6}}}} = 
                    parse([{t,1},{op,2},{'(',3},{t,4},{op,5},{t,6},{')',7}]),
                ok.
        ">>,
        default,
        ok},

          {prec_4, <<"
            Nonterminals E.
            Terminals '-' '+' '=' id.
            Rootsymbol E.

            E -> E '=' E : {op, '=', '$1', '$3'}.
            E -> E '+' E  : {op, '+', '$1', '$3'}.
            E -> '-' E : {op, '-', '$2'}.
            E -> id : '$1'.

            Nonassoc 100 '='.
            Right 200 '+' '-'.

            Erlang code.

            -export([t/0]).

            t() ->
                {ok,{op,'=',{id,1},{op,'-',{op,'+',{id,4},{id,6}}}}} = 
                    parse([{id,1},{'=',2},{'-',3},{id,4},{'+',5},{id,6}]),
                ok.

        ">>,
        default,
        ok}],

    run(Config, Ts),
    ok.

yeccpre(doc) ->
    "Errors etc. in actions, handled by yeccpre. parse_and_scan.";
yeccpre(suite) -> [];
yeccpre(Config) when is_list(Config) ->
    Ts = [{error_1, <<"
            Nonterminals list.
            Terminals element e.
            Rootsymbol list.

            list -> element : {single, '$1'}.
            list -> list element : throw(error).
            list -> e : return_error(element(2, '$1'), bad_element).

            Erlang code.

            -export([t/0]).

            t() ->
               error = (catch parse([{element,1},{element,2}])),
               {error, {2, _, Mess}} = parse([{element,1},{e,2}]),
               ok.
           ">>,
           default,
           ok},

          {error_2, <<"
            Nonterminals list.
            Terminals element.
            Rootsymbol list.

            list -> element.

            Erlang code.

            -export([t/0, scan/1]).

            scan(How) ->
                case How of
                    thrown -> throw(thrown);
                    error -> erlang:error(error);
                    exit -> exit(exit)
                end.

            t() ->
                try parse_and_scan({yecc_test, scan, [thrown]})
                catch throw: thrown ->
                        ok
                end,
                try parse_and_scan({yecc_test, scan, [error]})
                catch error: error ->
                        ok
                end,
                try parse_and_scan({fun yecc_test:scan/1, [exit]})
                catch exit: exit ->
                        ok
                end,
                try parse_and_scan({fun scan/1, [thrown]})
                catch throw: thrown ->
                        ok
                end,
                ok.
           ">>,
           default,
           ok},

          {error_3, <<"
            Nonterminals statement.
            Terminals keyword string.
            Rootsymbol statement.

            statement -> keyword string.

            Erlang code.

            -export([t/0]).

            t() ->
                %% Never crash in yecctoken_to_string/1 or its helpers,
                %% even if when tokens are not in the format that erl_scan
                %% produces.
                {error, _} = parse([{string, 1, <<\"foo\">>}]),
                ok.
           ">>,
           default,
           ok}],

    run(Config, Ts),
    ok.

lalr(doc) ->
    "Examples of grammars that are LALR(1) but not SLR(1).";
lalr(suite) -> [];
lalr(Config) when is_list(Config) ->
    Ts = [{lalr_1, <<"
            %% http://inst.cs.berkeley.edu/~cs164/lectures/slide14a.pdf
            %% http://pages.cpsc.ucalgary.ca/~robin/class/411/LR.1.html
            %% b d . c. Shift or reduce?
            Nonterminals S A. 
            Terminals a b c d.
            Rootsymbol S.
            S -> A a : {r1,'$1','$2'}.
            S -> b A c : {r2,'$1','$2','$3'}.
            S -> d c : {r3,'$1','$2'}.
            S -> b d a : {r4,'$1','$2','$3'}.

            A -> d : {r5,'$1'}.

            Erlang code.

            -export([t/0]).

            t() ->
               %% Reduce!
               {ok,{r2,{b,1},{r5,{d,2}},{c,3}}} = parse([{b,1},{d,2},{c,3}]),
               ok.
           ">>,
           default,
           ok},
         {lalr_2, <<"
            %% http://www.cs.pitt.edu/~mock/cs2210/lectures/lecture5.pdf
            Nonterminals S L R. 
            Terminals '*' id '='.
            Rootsymbol S.
            S -> L '=' R : {r1,'$1','$3'}.
            S -> R : {r2,'$1'}.

            L -> '*' R : {r3,'$2'}.
            L -> id : {r4,'$1'}.

            R -> L : {r5,'$1'}.

            Erlang code.

            -export([t/0]).

            t() ->
               {ok,{r1,{r3,{r5,{r4,{id,1}}}},{r5,{r4,{id,3}}}}} = 
                   parse([{'*',0},{id,1},{'=',2},{id,3}]),
               ok.
           ">>,
           default,
           ok}],
    run(Config, Ts),
    ok.

old_yecc(doc) ->
    "The old interface yecc:yecc/2,3,4.";
old_yecc(suite) -> [];
old_yecc(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.yrl"),
    Parserfile = filename:join(Dir, "file.erl"),
    Mini = <<"Nonterminals nt. 
              Terminals t.
              Rootsymbol nt.
              nt -> t.">>,
    ok = file:write_file(Filename, Mini),
    {_, _} = yecc:yecc(Filename, Parserfile),
    {_, _} = yecc:yecc(Filename, Parserfile, true),
    {_, _} = yecc:yecc(Filename, Parserfile, true),
    TE = process_flag(trap_exit, true),
    {'EXIT', error} = 
        (catch yecc:yecc(Filename, Parserfile, false, Parserfile)),
    _ = process_flag(trap_exit, TE),
    ok.

other_examples(doc) ->
    "Misc examples.";
other_examples(suite) -> [];
other_examples(Config) when is_list(Config) ->
    Ts = [{empty, <<"
            %% G1 from the 1974 article.

            Nonterminals LIST ELEMENT.
            Terminals ',' a b.
            Rootsymbol LIST.

            LIST -> LIST ',' ELEMENT : {'$1', '$3'}.
            LIST -> ELEMENT : '$1'.

            ELEMENT -> a : '$1'.
            ELEMENT -> b : '$1'.

            Erlang code.

            -export([t/0]).

            t() ->
                L = [{a, 1}, {',', 2}, {b, 3}],
                {ok,{{a,1},{b,3}}} = parse(L),
                ok.
            ">>,
           default,
           ok}],
    run(Config, Ts),
    ok.


otp_5369(doc) ->
    "OTP-5369. A bug in parse_and_scan reported on erlang questions.";
otp_5369(suite) -> [];
otp_5369(Config) when is_list(Config) ->
    Ts = [{otp_5369,<<"
        Nonterminals list.
        Terminals element.
        Rootsymbol list.

        list -> element : {single, '$1'}.
        list -> list element : {pair, '$1', '$2'}.

        Erlang code.

        -export([t/0, scan/1]).

        scan(Lexer) ->
            Lexer ! {scan, self()},
            receive
                {ok, Lexer, [Token], Position} ->
                    {ok, [Token], Position};
                {eof, Lexer, Position} ->
                    {eof, Position}
            end.

        make_scanner(L) ->
            spawn_link(fun() ->
                               loop(L)
                       end).

        loop([]) ->
            receive
                {scan, Pid} ->
                    Pid ! {eof, self(), 1000}
            end;
        loop([Token = {_, P}|T]) ->
            receive
                {scan, Pid} ->
                    Pid ! {ok, self(), [Token], P},
                    loop(T)
            end.

        t(L) ->
            case parse_and_scan({yecc_test, scan, [make_scanner(L)]}) of
                {ok, _Result} ->
                    ok;
                {error, {_LineNumber, Module, Message}} ->
                    _M = apply(Module, format_error, [Message]),
                    not_ok
            end.

        t() ->
            not_ok = t([]), %% This test has been added afterwards.
                            %% OTP-5369 did not fix this bug!
            L = [{element, 1},
                 {element, 2},
                 {element, 3}],
            t(L).
      ">>,
      default,
      ok}],
    run(Config, Ts),
    ok.

otp_6362(doc) ->
    "OTP-6362. A precedence declaration bug reported on erlang questions.";
otp_6362(suite) -> [];
otp_6362(Config) when is_list(Config) ->
    Ts = [{otp_6362_1,<<"
        Nonterminals cmp compare expr.
        Terminals 'string' '>' '='.
        Rootsymbol compare.
        Nonassoc 250 cmp.
        compare -> expr cmp expr     : {cmp, '$2', '$1', '$3'}.
        compare -> expr              : {cmp, '==', '$1', {string, \"TRUE\"}}.
        cmp -> '>' '='               : '>='.
        cmp -> '>'                   : '>'.
        expr -> 'string'             : '$1'.

        Erlang code.

        -export([t/0]).

        t() ->
            {ok,{cmp,'>',{string,1},{string,3}}} = 
                parse([{string,1}, {'>', 2}, {string,3}]),
            ok.
      ">>,
      default,
      ok}],
    run(Config, Ts),

    Dir = ?privdir,
    %% Report errors. Very simple test of format_error/1.
    Ret = [return, {report, true}],
    Filename = filename:join(Dir, "file.yrl"),

    %% An error introduced due to this ticket. Terminals can be
    %% assigned conflicting precedences, which cannot be resolved.
    ok = file:write_file(Filename,<<"
            Nonterminals cmp compare expr fopp.
            Terminals string '>' '='.
            Rootsymbol compare.
            Nonassoc 250 cmp.
            Left 300 '>'.

            compare -> expr cmp expr	: {cmp, '$2', '$1', '$3'}.
            compare -> expr fopp expr	: {cmp, '$2', '$1', '$3'}.
            compare -> expr '=' expr        : '$1'.
            compare -> expr 		: {cmp, '==', '$1', {string, foo}}.
            cmp -> '>' '='			: '>='.
            cmp -> '>'			: '>'.
            expr -> string  		: '$1'.

            fopp -> '>' '='			: '>='.
            fopp -> '>'			: '>'.">>),

    Ret = [return, {report, true}],
    {error,[{_,[{none,yecc,{conflict,_}}]}],[]} = 
        yecc:file(Filename, Ret),
    file:delete(Filename),
    ok.

my_yeccpre() ->
  %% Version 1.1.
  <<"parse(Tokens) ->
         yeccpars0(Tokens, false).

     parse_and_scan({F, A}) -> % Fun or {M, F}
         yeccpars0([], {F, A});
     parse_and_scan({M, F, A}) ->
         yeccpars0([], {{M, F}, A}).

     format_error(Message) ->
         case io_lib:deep_char_list(Message) of
             true ->
                 Message;
             _ ->
                 io_lib:write(Message)
         end.

     % To be used in grammar files to throw an error message to the parser
     % toplevel. Doesn't have to be exported!
     -compile({nowarn_unused_function,{return_error,2}}).
     return_error(Line, Message) ->
         throw({error, {Line, yecc_test, Message}}).

     yeccpars0(Tokens, MFA) ->
         try yeccpars1(Tokens, MFA, 0, [], [])
         catch 
             throw: {error, {_Line, yecc_test, _M}} = Error -> 
                        Error % probably from return_error/1
         end.

     % Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
    yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
        yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
                  Tokenizer);
    yeccpars1([], {F, A}, State, States, Vstack) ->
        case apply(F, A) of
            {ok, Tokens, _Endline} ->
                yeccpars1(Tokens, {F, A}, State, States, Vstack);
            {eof, _Endline} ->
                yeccpars1([], false, State, States, Vstack);
            {error, Descriptor, _Endline} ->
                {error, Descriptor}
        end;
    yeccpars1([], false, State, States, Vstack) ->
        yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

     % For internal use only.
     yeccerror(Token) ->
         {error,
          {element(2, Token), yecc_test,
           [\"syntax error before: \", yecctoken2string(Token)]}}.

     yecctoken2string({atom, _, A}) -> io_lib:write(A);
     yecctoken2string({integer,_,N}) -> io_lib:write(N);
     yecctoken2string({float,_,F}) -> io_lib:write(F);
     yecctoken2string({char,_,C}) -> io_lib:write_char(C);
     yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
     yecctoken2string({string,_,S}) -> io_lib:write_string(S);
     yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
     yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
     yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
     yecctoken2string({'$end', _}) ->
         [];
     yecctoken2string({Other, _}) when is_atom(Other) ->
         io_lib:format('~w', [Other]);
     yecctoken2string(Other) ->
         io_lib:write(Other).
">>.

otp_7945(doc) ->
    "OTP-7945. A bug introduced in R13A.";
otp_7945(suite) -> [];
otp_7945(Config) when is_list(Config) ->
    A2 = erl_anno:new(2),
    A3 = erl_anno:new(3),
    {error,_} = erl_parse:parse([{atom,A3,foo},{'.',A2,9,9}]),
    ok.

otp_8483(doc) ->
    "OTP-8483. reduce/accept conflict";
otp_8483(suite) -> [];
otp_8483(Config) when is_list(Config) ->
    Dir = ?privdir,
    Input = filename:join(Dir, "bug.yrl"),

    Bug1 = <<"Nonterminals elem seq.
              Terminals 'foo'.
              Rootsymbol elem.
              elem -> 'foo'.
              elem -> seq.
              seq -> elem.
              seq -> seq elem.">>,
    ok = file:write_file(Input, Bug1),
    Ret = [return, {report, true}],
    {error,[{_,[{none,yecc,{conflict,_}},
                {none,yecc,{conflict,{'$end',4,{[seq,elem],3,{6,15}},
                                      {reduce,[seq,seq,elem],4,{7,15}}}}},
                      {none,yecc,{conflict,_}}]}],
           [{_,[{none,yecc,{conflicts,1,3}}]}]} = 
        yecc:file(Input, Ret),
    {error,[{_,[{none,yecc,{conflict,_}},
                {none,yecc,{conflict,{'$end',4,{[seq,elem],3,6},
                                      {reduce,[seq,seq,elem],4,7}}}},
                      {none,yecc,{conflict,_}}]}],
           [{_,[{none,yecc,{conflicts,1,3}}]}]} =
        yecc:file(Input, [{error_location, line} | Ret]),
    file:delete(Input),
    ok.

otp_8486(doc) ->
    "OTP-8486.";
otp_8486(suite) -> [];
otp_8486(Config) when is_list(Config) ->
    Ts = [{otp_8486,<<"
           Nonterminals boolean command.
           Terminals '(' ')' if then else true and or skip while do.
           Rootsymbol command.
           Left 100 or.
           Left 200 and.
           boolean -> '(' boolean ')' : '$2'.
           boolean -> 'true' : b.
           boolean -> boolean 'and' boolean : {a,'$1','$3'}.
           boolean -> boolean 'or' boolean : {o,'$1','$3'}.
           command -> 'skip' : s.
           command -> 'if' boolean 'then' command 'else' command : 
                                  {i,'$2','$4','$6'}.
           command -> 'while' boolean 'do' command : {w,'$2','$4'}.

           Erlang code.
           -export([t/0]).
           t() ->
               {ok,{i,{o,b,b},s,s}} =
                   parse([{'if',1},{'true',1},{'or',1},{'true',1},
                          {'then',1},{'skip',1},{'else',1},{'skip',1}]),
               ok.
          ">>,default,ok}],
    run(Config, Ts),
    ok.

otp_7292(doc) ->
    "OTP-7292. Header declarations for edoc.";
otp_7292(suite) -> [];
otp_7292(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.yrl"),
    Parserfile1 = filename:join(Dir, "a file"),

    Contents =  <<"Nonterminals nt. 
                   Terminals t. 
                   Rootsymbol nt. 
                   Endsymbol e.
                   nt -> t : a bad code.

                   Header \"%% copyright bla bla bla\"
                   \"%% @private\" \"%% foo\"
                   \"%% @author X.Y.\".

                   Erlang code.

                   %% @private
                   %% etc.

                   foo() ->
                       bar. ">>,

    %% Check that correct line number is used in messages.
    ok = file:write_file(Filename, Contents),
    ParserFile3 = [{parserfile, Parserfile1}],
    Ret = [return, {report, true}],
    {ok, _, []} = yecc:file(Filename, ParserFile3 ++ Ret),

    %% Note: checking the line numbers. Changes when yeccpre.hrl changes.
    fun() ->
            SzYeccPre = yeccpre_size(),
            {error,
                   [{_,[{{5,32},_,["syntax error before: ","bad"]}]},
                    {_,[{{L1,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{L2,_},_,{bad_inline,{yeccpars2_2_,1}}},
                        {{_,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{_,_},_,{bad_nowarn_unused_function,{yeccpars2_2_,1}}}]}],
             [{_,[{{16,20},_,{unused_function,{foo,0}}}]}]} =
                compile:file(Parserfile1, [basic_validation, return]),
            ?assertEqual(L1, 48 + SzYeccPre),
            ?assertEqual(L2, 57 + SzYeccPre)
    end(),

    YeccPre = filename:join(Dir, "yeccpre.hrl"),
    ok = file:write_file(YeccPre, 
       [<<"-export([parse/1, parse_and_scan/1, format_error/1]).\n">>,
        yeccpre_v1_2()]),
    Inc = [{includefile,YeccPre}],
    {ok, _, []} = yecc:file(Filename, ParserFile3 ++ Inc ++ Ret),
    fun() ->
            SzYeccPre = yeccpre_size(YeccPre),
            {error,
                   [{_,[{{5,32},_,["syntax error before: ","bad"]}]},
                    {_,[{{L1,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{L2,_},_,{bad_inline,{yeccpars2_2_,1}}},
                        {{_,_},_,{undefined_function,{yeccpars2_2_,1}}},
                        {{_,_},_,{bad_nowarn_unused_function,{yeccpars2_2_,1}}}]}],
                   [{_,[{{16,20},_,{unused_function,{foo,0}}}]}]} =
                compile:file(Parserfile1, [basic_validation, return]),
            ?assertEqual(L1, 45 + SzYeccPre),
            ?assertEqual(L2, 54 + SzYeccPre)
    end(),

    file:delete(YeccPre),
    file:delete(Parserfile1 ++ ".erl"),
    file:delete(Filename),

    ok.

yeccpre_v1_2() ->
    <<"
parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) ->
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {fun M:F/Arity, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

-define(CODE_VERSION, \"1.2\").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error : Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    \"yeccpars2\" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    \"yeccgoto_\" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, 
              Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      [\"syntax error before: \", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).
">>.

run(Config, Tests) ->
    F = fun({N,P,Pre,E}) ->
                case catch run_test(Config, P, Pre) of
                    E -> 
                        ok;
                    Bad -> 
                        ct:fail("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad])
                end
        end,
    lists:foreach(F, Tests).

otp_7969(doc) ->
    "OTP-7969. Interface to the I/O protocol..";
otp_7969(suite) -> [];
otp_7969(Config) when is_list(Config) ->
    {ok,Ts1,_} =
        erl_scan:string("'foo\nbar'", {1,1}, [text]),
    {error,{{2,5},_,["syntax error before: ",[]]}} = erl_parse:parse(Ts1),

    {ok,Ts1_1,_} = erl_scan:string("'foo\nbar'", 1, [text]),
    {error,{2,_,["syntax error before: ",[]]}} = erl_parse:parse(Ts1_1),

    {ok,Ts2,_EndLocation} =
        erl_scan:string("'foo\nbar'", {1,1}, []),
    %% Can't do better than report possibly wrong location:
    {error,{{1,1},_,["syntax error before: ",[]]}} = erl_parse:parse(Ts2),

    {ok, Ts11, _}=R1 = erl_scan:string("f() -> a."),
    F1 = fun() -> {ok,Ts11 ++ [{'$end',2}],2} end,
    A1 = erl_anno:new(1),
    {ok,{function,A1,f,0,[{clause,A1,[],[],[{atom,A1,a}]}]}} =
        erl_parse:parse_and_scan({F1, []}),
    F2 = fun() -> erl_scan:string("f() -> ,") end,
    {error,{1,erl_parse,_}} = erl_parse:parse_and_scan({F2, []}),
    F3 = fun() -> case erase(foo) of
                            bar -> 
                                {ok,[{'$end',2}],3};
                            undefined ->
                                put(foo,bar), R1
                        end
               end,
    {ok,{function,A1,f,0,[{clause,A1,[],[],[{atom,A1,a}]}]}} =
        erl_parse:parse_and_scan({F3,[]}),
    F4 = fun() -> {error, {1, ?MODULE, bad}, 2} end,
    {error, {1,?MODULE,bad}} = erl_parse:parse_and_scan({F4, []}),
    F5 = fun() -> {eof, 3} end,
    {error,{3,erl_parse,_}} = erl_parse:parse_and_scan({F5, []}),
    {error,{999999,erl_parse,_}} = erl_parse:parse([]),
    {ok, Ts21, EL} = erl_scan:string("f() -> a; g() -> b. ", {1,1}),
    F6 = fun() -> {ok, Ts21, EL} end,
    {error,{{1,11},erl_parse,_}} = erl_parse:parse_and_scan({F6, []}),
    ok.

otp_8919(doc) ->
    "OTP-8919. Improve formatting of Yecc error messages.";
otp_8919(suite) -> [];
otp_8919(Config) when is_list(Config) ->
    A1 = erl_anno:new(1),
    {error,{1,Mod,Mess}} = erl_parse:parse([{cat,A1,"hello"}]),
    "syntax error before: \"hello\"" = lists:flatten(Mod:format_error(Mess)),
    ok.

otp_10302(doc) ->
    "OTP-10302. Unicode characters scanner/parser.";
otp_10302(suite) -> [];
otp_10302(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "OTP-10302.yrl"),
    Ret = [return, {report, true}],
    Mini1 = <<"%% coding: utf-8
               Nonterminals Häpp.
               nt -> t.">>,
    ok = file:write_file(Filename, Mini1),
    %% This could (and should) be refined:
    {error,[{Filename,[{{2,29},Mod1,Err1}]}],[]} =
        yecc:file(Filename, Ret),
    "cannot translate from UTF-8" = Mod1:format_error(Err1),

    Mini2 = <<"%% coding: Utf-8
               Nonterminals Hopp.
               Terminals t.
               Rootsymbol Hopp.

               Hopp -> t.

               Erlang code.

               t() ->
                   Häpp.">>,
    ok = file:write_file(Filename, Mini2),
    {error,[{Filename,[{11,Mod2,Err2}]}],[]} =
        yecc:file(Filename, Ret),
    "cannot parse; possibly encoding mismatch" = Mod2:format_error(Err2),

    Mini3 = <<"%% coding: latin-1
               Nonterminals Hopp.
               Terminals t.
               Rootsymbol Hopp.

               Hopp -> t.

               Erlang code.

               t() ->
                   Häpp.">>,
    ok = file:write_file(Filename, Mini3),
    YeccPre = filename:join(Dir, "yeccpre.hrl"),
    ok = file:write_file(YeccPre, [<<"%% coding: UTF-8\n ä.\n">>]),
    Inc = [{includefile,YeccPre}],
    {error,[{_,[{2,yecc,cannot_parse}]}],[]} =
        yecc:file(Filename, Inc ++ Ret),

    ok = file:write_file(Filename,
     <<"%% coding: UTF-8
        Nonterminals Hopp.
        Terminals t.
        Rootsymbol \"Ã¶rn_Ð\".
        Hopp -> t : '$1'.">>),
    {error,[{Filename,[{{4,20},yecc,{bad_symbol,"örn_"++[1024]}}]}],[]} =
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename,
     <<"%% coding: UTF-8
        Nonterminals Hopp.
        Terminals t.
        Rootsymbol Hopp.
        Endsymbol \"Ã¶rn_Ð\".
        Hopp -> t : '$1'.">>),
    {error,[{Filename,[{{5,19},yecc,{bad_symbol,"örn_"++[1024]}}]}],[]} =
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename,
     <<"%% coding: UTF-8
        Nonterminals Hopp.
        Terminals t.
        Rootsymbol Hopp.
        Expect \"Ã¶rn_Ð\".
        Hopp -> t : '$1'.">>),
    {error,[{Filename,[{{5,16},yecc,{bad_symbol,"örn_"++[1024]}}]}],[]} =
        yecc:file(Filename, Ret),

    ok = file:write_file(Filename,
     <<"%% coding: UTF-8
        Nonterminals Hopp.
        Terminals t.
        Rootsymbol Hopp.
        States \"Ã¶rn_Ð\".
        Hopp -> t : '$1'.">>),
    {error,[{Filename,[{{5,16},yecc,{bad_symbol,"örn_"++[1024]}}]}],[]} =
        yecc:file(Filename, Ret),

    Ts = [{otp_10302_1,<<"
           %% coding: UTF-8
           Header \"%% Ã¶rn_Ð\" \"%% \\x{400}B\".
           Nonterminals HÃ¤pp list.
           Terminals element.
           Rootsymbol HÃ¤pp.

           HÃ¤pp -> list : '$1'.

           list -> element : '$1'.
           list -> list element :
                       begin
                           HÃ¤pp = foo,
                           {HÃ¤pp, 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"}
                       end.

           Erlang code.

           -export([t/0]).

           t() ->
               L = [{element, 1}, {element,2}],
               {ok, R} = parse(L),
               HÃ¤pp = foo,
               {_,_,[1024,66],[246,114,110,95,1024]} = R,
               {HÃ¤pp,'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,
               ok.
          ">>,default,ok},
          {otp_10302_2,<<"
           %% coding: Latin-1
           Nonterminals Häpp list.
           Terminals element.
           Rootsymbol Häpp.

           Häpp -> list : '$1'.

           list -> element : '$1'.
           list -> list element :
                       begin
                           Häpp = foo,
                           {Häpp, 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"}
                       end.

           Erlang code.

           -export([t/0]).

           t() ->
               L = [{element, 1}, {element,2}],
               {ok, R} = parse(L),
               Häpp = foo,
               {_,_,[1024,66],[195,182,114,110,95,208,128]} = R,
               {Häpp,'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,
               ok.
          ">>,default,ok}],
    run(Config, Ts),
    ok.

otp_11269(doc) ->
    "OTP-11269. A bug.";
otp_11269(suite) -> [];
otp_11269(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "OTP-11269.yrl"),
    Ret = [return, {report, false}],
    Pai = <<"Nonterminals
             list list0 list1 newline_list.

             Terminals
             '\n' semi.

             Rootsymbol  list.

             Endsymbol '$end'.

             list ->   newline_list list0 : '$2'.

             list0 -> list1 '\n' newline_list : '$1'.

             list1 -> list1 semi newline_list list1 :
                           {command_connect, '$1', '$4', semi}.

             newline_list -> newline_list '\n' : nil.">>,
    ok = file:write_file(Filename, Pai),
    {ok,ErlFile,[{_YrlFile,[{none,yecc,{conflicts,1,0}}]}]} =
        yecc:file(Filename, Ret),
    Opts = [return, warn_unused_vars,{outdir,Dir}],
    {ok,'OTP-11269',_Warnings} = compile:file(ErlFile, Opts),
    ok.

otp_11286(doc) ->
    "OTP-11286. A Unicode filename bug; both Leex and Yecc.";
otp_11286(suite) -> [];
otp_11286(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(["+fnu"]),
    Dir = ?privdir,
    UName = [1024] ++ "u",
    UDir = filename:join(Dir, UName),
    _ = rpc:call(Node, file, make_dir, [UDir]),

    %% Note: Cannot use UName as filename since the filename is used
    %% as module name. To be fixed in R18.
    Filename = filename:join(UDir, 'OTP-11286.yrl'),
    Ret = [return, {report, false}, time],

    Mini1 = <<"%% coding: utf-8
               Terminals t.
               Nonterminals nt.
               Rootsymbol  nt.
               nt -> t.">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini1]),
    {ok,ErlFile,[]} = rpc:call(Node, yecc, file, [Filename, Ret]),
    Opts = [return, warn_unused_vars,{outdir,Dir}],
    {ok,_,_} = rpc:call(Node, compile, file, [ErlFile, Opts]),

    Mini2 = <<"Terminals t.
               Nonterminals nt.
               Rootsymbol  nt.
               nt -> t.">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini2]),
    {ok,ErlFile,[]} = rpc:call(Node, yecc, file, [Filename, Ret]),
    Opts = [return, warn_unused_vars,{outdir,Dir}],
    {ok,_,_} = rpc:call(Node, compile, file, [ErlFile, Opts]),

    Mini3 = <<"%% coding: latin-1
               Terminals t.
               Nonterminals nt.
               Rootsymbol  nt.
               nt -> t.">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini3]),
    {ok,ErlFile,[]} = rpc:call(Node, yecc, file, [Filename, Ret]),
    Opts = [return, warn_unused_vars,{outdir,Dir}],
    {ok,_,_} = rpc:call(Node, compile, file, [ErlFile, Opts]),

    peer:stop(Peer),
    ok.

otp_14285(Config) ->
    Dir = ?privdir,
    YeccPre = filename:join(Dir, "yeccpre.hrl"),
    ok = file:write_file(YeccPre,
                               [<<"-export([t/0]).\n">>,my_yeccpre()]),

    T0 = <<"
        Nonterminals '\\x{400}'. 
        Terminals t.
        Rootsymbol '\\x{400}'.
       '\\x{400}' -> t : '$1'.
       Erlang code.
       t() ->
           L = [{t, 1}],
           {ok, R} = parse(L),
           {t, 1} = R,
           ok.">>,
    Ts0 = [{otp_14285_1,
           [<<"%% coding: Latin-1\n">>,T0],YeccPre,ok},
         {otp_14285_2,
           [<<"%% coding: coding: UTF-8\n">>,T0],YeccPre,ok}],
    run(Config, Ts0),
    file:delete(YeccPre),

    T1 = <<"
        Nonterminals '1\\x{400}' list 'unused\\x{400}'.
        Terminals '2\\x{400}'.
        Rootsymbol '1\\x{400}'.

        '1\\x{400}' -> list : '$1'.

        list -> '2\\x{400}' : '$1'.
        list -> list '2\\x{400}' : {foo,'\\x{400}'}.

        Erlang code.

        -export([t/0]).

        t() ->
            L = [{'2\\x{400}', 1}, {'2\\x{400}',2}],
            {ok, R} = parse(L),
            {foo,A} = R,
            '\\x{400}' = A,
            [1024] = atom_to_list(A),
            ok.">>,

    Ts1 = [{otp_14285_3,
            [<<"%% coding: Latin-1\n">>,T1],default,ok},
           {otp_14285_4,
            [<<"%% coding: UTF-8\n">>,T1],default,ok}],
    run(Config, Ts1),

    T2 = <<"
        Nonterminals E.
        Terminals '-' '+' '=' id.
        Rootsymbol E.
        Endsymbol '\\x{400}'.

        E -> E '=' E : {op, '=', '$1', '$3'}.
        E -> E '+' E  : {op, '+', '$1', '$3'}.
        E -> '-' E : {op, '-', '$2'}.
        E -> id : '$1'.

        Nonassoc 100 '='.
        Right 200 '+' '-'.

        Erlang code.

        -export([t/0]).

        t() ->
            {ok,{op,'=',{id,1},{op,'-',{op,'+',{id,4},{id,6}}}}} = 
                parse([{id,1},{'=',2},{'-',3},{id,4},{'+',5},{id,6},
                      {'\\x{400}',1}]),
            ok.">>,

    Ts2 = [{otp_14285_5,
            [<<"%% coding: Latin-1\n">>,T2],default,ok},
           {otp_14285_6,
            [<<"%% coding: UTF-8\n">>,T2],default,ok}],
    run(Config, Ts2),

    ok.

otp_17023(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.yrl"),
    Ret = [return, {report, true}],

    {'EXIT', {badarg, _}} = (catch yecc:file(Filename, [{noopt,true}])),
    OldEnv = os:getenv("ERL_COMPILER_OPTIONS"),
    true = os:putenv("ERL_COMPILER_OPTIONS", "strong_validation"),
    ok = file:write_file(Filename,<<"
              Nonterminals nt. 
              Terminals t.
              Rootsymbol nt.
              nt -> t : '$2'.
          ">>),
    {error,[{_,[{{5,25},yecc,{undefined_pseudo_variable,'$2'}}]}],[]} =
        yecc:file(Filename, Ret),
    true = os:putenv("ERL_COMPILER_OPTIONS", "{return, false}"),
    error = yecc:file(Filename, Ret),
    error = yecc:file(Filename, [return | Ret]), % overridden
    case OldEnv of
        false ->
            os:unsetenv("ERL_COMPILER_OPTIONS");
        _ ->
            os:putenv("ERL_COMPILER_OPTIONS", OldEnv)
    end,
    ok.

otp_17535(doc) ->
    "GH-5067. Compiler finds unused functions.";
otp_17535(suite) -> [];
otp_17535(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "OTP-17535.yrl"),
    Ret = [return, {report, true}],
    J = <<"Nonterminals start statem.
          Terminals  'if'.
          Rootsymbol start.
          start -> statem : b.
          statem -> 'if' statem : a.
          Erlang code.">>,
    ok = file:write_file(Filename, J),
    {ok, ErlFile, []} = yecc:file(Filename, Ret),
    {ok, _, []} = compile:file(ErlFile, [return]),
    ok.

yeccpre_size() ->
    yeccpre_size(default_yeccpre()).

yeccpre_size(File) ->
    n_lines(File).
    
default_yeccpre() ->
    filename:join([code:lib_dir(parsetools),"include","yeccpre.hrl"]).

n_lines(File) ->
    {ok, Bin} = file:read_file(File),
    length([C || C=$\n <- binary_to_list(Bin)]).

run_test(Config, Def, Pre) ->
    %% io:format("testing ~s~n", [binary_to_list(Def)]),
    DefFile = 'yecc_test.yrl',
    Filename = 'yecc_test.erl',
    DataDir = ?privdir,
    YrlFile = filename:join(DataDir, DefFile),
    ErlFile = filename:join(DataDir, Filename),
    Opts = [return, warn_unused_vars,{outdir,DataDir}],
    ok = file:write_file(YrlFile, Def),
    YOpts = [return, {report, false} | 
             case Pre of
                 default ->
                     [];
                 _ ->
                     [{includefile,Pre}]
             end],
    P0 = pps(),
    YRet = yecc:file(YrlFile, [verbose | YOpts]),
    case {pps(), YRet} of
        {P0, {ok, _Outfile, _YWs}} ->
                 case compile:file(ErlFile, Opts) of
                     {ok, _M, _Ws} -> 
                         AbsFile = filename:rootname(ErlFile, ".erl"),
                         Mod = yecc_test,
                         code:purge(Mod),
                         code:load_abs(AbsFile, Mod),
                         Mod:t();
                         %% warnings(ErlFile, Ws);
                     {error, [{ErlFile,Es}], []} -> {error, Es, []};
                     {error, [{ErlFile,Es}], [{ErlFile,Ws}]} -> {error, Es, Ws};
                     Error  -> Error
                 end;
        {P0, {error, [{YrlFile,YEs}], []}} -> {error, YEs, []};
        {P0, {error, [{YrlFile,YEs}], [{YrlFile,YWs}]}} -> {error, YEs, YWs};
        {P0, YError} -> YError;
        {P, _} ->
            ct:fail("failure, got ~p~n, expected ~p\n", [P, P0])
    end.

extract(File, {error, Es, Ws}) ->
    {errors, extract(File, Es), extract(File, Ws)};    
extract(File, Ts) ->
    lists:append([T || {F, T} <- Ts,  F =:= File]).

pps() ->
    {port_list(), process_list()}.

port_list() ->
    [{P,safe_second_element(erlang:port_info(P, name))} || P <- erlang:ports()].

process_list() ->
    [{P,process_info(P, registered_name),
      safe_second_element(process_info(P, initial_call))} || 
        P <- processes(), is_process_alive(P)].

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.

search_for_file_attr(PartialFilePathRegex, Forms) ->
    lists:search(fun
                   ({attribute, _, file, {FileAttr, _}}) ->
                      case re:run(FileAttr, PartialFilePathRegex, [unicode]) of
                        nomatch -> false;
                        _ -> true
                      end;
                   (_) -> false end,
                 Forms).
