%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
-module(leex_SUITE).

%-define(debug, true).

-include_lib("stdlib/include/erl_compile.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(debug).
-define(config(X,Y), foo).
-define(datadir, "leex_SUITE_data").
-define(privdir, "leex_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 file/1, compile/1, syntax/1, deterministic/1,
	 
	 pt/1, man/1, ex/1, ex2/1, not_yet/1,
	 line_wrap/1,
	 otp_10302/1, otp_11286/1, unicode/1, otp_13916/1, otp_14285/1,
         otp_17023/1, compiler_warnings/1, column_support/1]).

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
    [{group, checks}, {group, examples}, {group, tickets}, {group, bugs}].

groups() -> 
    [{checks, [], [file, compile, syntax, deterministic]},
     {examples, [], [pt, man, ex, ex2, not_yet, unicode, column_support]},
     {tickets, [], [otp_10302, otp_11286, otp_13916, otp_14285, otp_17023,
                    compiler_warnings]},
     {bugs, [], [line_wrap]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



file(doc) ->
    "Bad files and options.";
file(suite) -> [];
file(Config) when is_list(Config) ->
    Dir = ?privdir,
    Ret = [return, {report, false}],
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", Ret),
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", [{return,true}]),
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", [{report,false},return_errors]),
    error = leex:file("not_a_file"),
    error = leex:file("not_a_file", [{return,false},report]),
    error = leex:file("not_a_file", [return_warnings,{report,false}]),

    Filename = filename:join(Dir, "file.xrl"),
    file:delete(Filename),

    {'EXIT', {badarg, _}} = (catch leex:file({foo})),
    {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {parserfile,{foo}})),
    {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {includefile,{foo}})),

    {'EXIT', {badarg, _}} = (catch leex:file(Filename, no_option)),
    {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, [return | report])),
    {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {return,foo})),
    {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, includefile)),

    {'EXIT', {badarg, _}} =
        (catch leex:file(Filename, {tab_size,0})),
    {'EXIT', {badarg, _}} =
        (catch leex:file(Filename, {tab_size,"4"})),
    {'EXIT', {badarg, _}} =
        (catch leex:file(Filename, {tab_size,3.5})),
    {'EXIT', {badarg, _}} =
        (catch leex:file(Filename, {error_location,{line,column}})),
    {'EXIT', {badarg, _}} =
        (catch leex:file(Filename, {error_location,col})),

    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    ok = file:write_file(Filename, Mini),
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [{includefile,"/ /"} | Ret]),

    LeexPre = filename:join(Dir, "leexinc.hrl"),
    ok = file:write_file(LeexPre, <<"syntax error.\n">>),
    PreErrors = run_test(Config, Mini, LeexPre),
    {errors,
           [{{1,8},_,["syntax error before: ","error"]},
            {{3,1},_,undefined_module}],
           []} =
        extract(LeexPre, PreErrors),
    file:delete(LeexPre),

    Ret2 = [return, report_errors, report_warnings, verbose],
    Scannerfile = filename:join(Dir, "file.erl"),
    ok = file:write_file(Scannerfile, <<"nothing">>),
    unwritable(Scannerfile),
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, Ret2),
    writable(Scannerfile),
    file:delete(Scannerfile),

    Dotfile = filename:join(Dir, "file.dot"),
    ok = file:write_file(Dotfile, <<"nothing">>),
    unwritable(Dotfile),
    {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [dfa_graph | Ret2]),
    writable(Dotfile),
    file:delete(Dotfile),

    ok = file:delete(Scannerfile),
    Warn = <<"Definitions.1998\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    ok = file:write_file(Filename, Warn),
    error = leex:file(Filename, [warnings_as_errors]),
    false = filelib:is_regular(Scannerfile),
    error = leex:file(Filename, [return_warnings,warnings_as_errors]),
    false = filelib:is_regular(Scannerfile),
    {error,_,[{Filename,[{1,leex,ignored_characters}]}]} =
        leex:file(Filename, [return_errors,warnings_as_errors]),
    false = filelib:is_regular(Scannerfile),
    {ok,Scannerfile,[{Filename,[{1,leex,ignored_characters}]}]} =
        leex:file(Filename, [return_warnings]),
    true = filelib:is_regular(Scannerfile),

    file:delete(Filename),
    ok.

compile(doc) ->
    "Check of compile/3.";
compile(suite) -> [];
compile(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Scannerfile = filename:join(Dir, "file.erl"),
    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    ok = file:write_file(Filename, Mini),
    ok = leex:compile(Filename, Scannerfile, #options{}),
    file:delete(Scannerfile),
    file:delete(Filename),
    ok.

syntax(doc) ->
    "Syntax checks.";
syntax(suite) -> [];
syntax(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Ret = [return, {report, true}],
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "%% comment\n"
                                 "Rules.\n"
                                 "{L}+  : {token,{word,TokenLine,TokenChars}}.\n
                                 ">>),
    {error,[{_,[{7,leex,missing_code}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+  : \n">>),
    {error,[{_,[{5,leex,missing_code}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] :">>),
    {error,[{_,[{4,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ : .\n"
                                 "[] : ">>),
    {error,[{_,[{5,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] : .\n">>),
    {error,[{_,[{4,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ ">>),
    {error,[{_,[{5,leex,bad_rule}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ ; ">>),
    {error,[{_,[{4,leex,bad_rule}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] : '99\n">>),
    {error,[{_,[{4,erl_scan,_}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n">>),
    {error,[{_,[{3,leex,empty_rules}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "Erlang code.\n">>),
    {error,[{_,[{4,leex,empty_rules}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n">>),
    {error,[{_,[{2,leex,missing_rules}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Erlang code.\n">>),
    {error,[{_,[{3,leex,missing_rules}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"">>),
    %% This is a weird line:
    {error,[{_,[{0,leex,missing_defs}]}],[]} = leex:file(Filename, Ret),
    ok = file:write_file(Filename, 
                               <<"Rules.\n">>),
    {error,[{_,[{1,leex,missing_defs}]}],[]} = leex:file(Filename, Ret),

    %% Check that correct line number is used in messages.
    ErlFile = filename:join(Dir, "file.erl"),
    Ret1 = [{scannerfile,ErlFile}|Ret],
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+  : {token,\n"
                                 "         {word,TokenLine,TokenChars,\n"
                                 "          DDDD}}.\n" % unbound
                                 "Erlang code.\n"
                                 "an error.\n">>),     % syntax error
    {ok, _, []} = leex:file(Filename, Ret1),
    {error, 
           [{_,[{{8,4},_,["syntax error before: ","error"]}]},
            {_,[{{6,6},_,{unbound_var,'DDDD'}}]}],
           []} =
        compile:file(ErlFile, [basic_validation, return]),

    %% Ignored characters
    ok = file:write_file(Filename,
                               <<"Definitions. D = [0-9]\n"
                                 "Rules. [a-z] : .\n"
                                 "1 : skip_token.\n"
                                 "Erlang code. f() -> a.\n">>),
    {ok,_,[{_,
                  [{1,leex,ignored_characters},
                   {2,leex,ignored_characters},
                   {4,leex,ignored_characters}]}]} = 
        leex:file(Filename, Ret),

    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\  : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{unterminated,"\\"}}}]}],[]} =
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\x  : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{illegal_char,"\\x"}}}]}],[]} =
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\x{  : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{unterminated,"\\x{"}}}]}],[]} =
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[^ab : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{unterminated,"["}}}]}],[]} =
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "(a : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{unterminated,"("}}}]}],[]} =
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[b-a] : token.\n">>),
    {error,[{_,[{4,leex,{regexp,{char_class,"b-a"}}}]}],[]} =
        leex:file(Filename, Ret),

    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "\\x{333333333333333333333333} : token.\n">>),
    {error,[{_,[{4,leex,{regexp,
                                {illegal_char,
                                 "\\x{333333333333333333333333}"}}}]}],[]} =
        leex:file(Filename, Ret),
    ok.

deterministic(doc) ->
    "Check leex respects the +deterministic flag.";
deterministic(suite) -> [];
deterministic(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Scannerfile = filename:join(Dir, "file.erl"),
    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    ok = file:write_file(Filename, Mini),

    %% Generated leex scanners include the leexinc.hrl header file by default,
    %% so we'll get a -file attribute corresponding to that include. In
    %% deterministic mode, that include should only use the basename,
    %% "leexinc.hrl", but otherwise, it should contain the full path.

    %% Matches when OTP is not installed (e.g. /lib/parsetools/include/leexinc.hrl)
    %% and when it is (e.g. /lib/parsetools-2.3.2/include/leexinc.hrl)
    AbsolutePathSuffix = ".*/lib/parsetools.*/include/leexinc\.hrl",

    ok = leex:compile(Filename, Scannerfile, #options{specific=[deterministic]}),
    {ok, FormsDet} = epp:parse_file(Scannerfile,[]),
    ?assertMatch(false, search_for_file_attr(AbsolutePathSuffix, FormsDet)),
    ?assertMatch({value, _}, search_for_file_attr("leexinc\.hrl", FormsDet)),
    file:delete(Scannerfile),

    ok = leex:compile(Filename, Scannerfile, #options{}),
    {ok, Forms} = epp:parse_file(Scannerfile,[]),
    ?assertMatch({value, _}, search_for_file_attr(AbsolutePathSuffix, Forms)),
    file:delete(Scannerfile),

    file:delete(Filename),
    ok.

pt(doc) ->
    "Pushing back characters.";
pt(suite) -> [];
pt(Config) when is_list(Config) ->
    %% Needs more testing...
    Ts = [{pt_1, 
         <<"Definitions.\n"
            "D  = [0-9]\n"
            "L  = [a-z]\n"

            "Rules.\n"
            "{L}+  : {token,{word,TokenLoc,TokenChars}}.\n"
            "abc{D}+  : {skip_token,\"sture\" ++ string:substr(TokenChars, 4)}.\n"
            "{D}+  : {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.\n"
            "\\s  : .\n"
            "\\r\\n  : {end_token,{crlf,TokenLine}}.\n"

            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->
                {ok,[{word,{1,7},\"sture\"},{integer,{1,12},123}],{1,15}} =
                    string(\"abc123\"), ok. ">>,
           default,
           ok}],

    run(Config, Ts),
    ok.

unicode(suite) ->
    [];
unicode(Config) when is_list(Config) ->
    Ts = [{unicode_1, 
	   <<"%% -*- coding: utf-8 -*-\n"
	     "Definitions.\n"
	     "RTLarrow    = (â)\n"
	     "Rules.\n"
	     "{RTLarrow}  : {token,{\"â\",TokenLoc}}.\n"
	     "Erlang code.\n"
	     "-export([t/0]).\n"
	     "t() -> {ok, [{\"â\", {1,1}}], {1,4}} = string(\"â\"), ok.">>,
           default,
           ok}],

    run(Config, Ts),
    ok.

man(doc) ->
    "Examples from the manpage.";
man(suite) -> [];
man(Config) when is_list(Config) ->
    Ts = [{man_1, 
     <<"Definitions.\n"
       "Rules.\n"
       "[a-z][0-9a-zA-Z_]* :\n"
       "    {token,{atom,TokenLoc,list_to_atom(TokenChars)}}.\n"
       "[A-Z_][0-9a-zA-Z_]* :\n"
       "    {token,{var,TokenLoc,list_to_atom(TokenChars)}}.\n"
       "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)? : \n"
       "   {token,{float,TokenLoc,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,{1,1},3.14},{atom,{1,5},atom},{var,{1,10},'V314'}],{1,14}} =\n"
       "        string(\"3.14atom V314\"),\n"
       "    ok.\n">>,
           default,
           ok},
        {man_2,
     <<"Definitions.\n"
       "D = [0-9]\n"
       "Rules.\n"
       "{D}+ :\n"
       "  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.\n"
       "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
       "  {token,{float,TokenLoc,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,{1,1},3.14},{integer,{1,6},314}],{1,9}} = \n"
       "        string(\"3.14 314\"),\n"
       "    ok.\n">>,
           default,
           ok}],
    
    run(Config, Ts),
    ok.

ex(doc) ->
    "Examples.";
ex(suite) -> [];
ex(Config) when is_list(Config) ->
    Ts = [{ex_1,
      <<"Definitions.\n"
        "D = [0-543-705-982]\n"
        "Rules.\n"
        "{D}+ :\n"
        "  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.\n"
        "[^235]+ :\n"
        "  {token,{list_to_atom(TokenChars),TokenLoc}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[{integer,{1,1},12},{' c\\na',{1,3}},{integer,{2,2},34},{b789a,{2,4}}],{2,9}} =\n"
        "        string(\"12 c\\na34b789a\"),\n"
        "    ok.\n">>,
           default,
           ok},

          {ex_2,
      <<"Definitions.\n"
        "L = [a-z]\n"
        "D = [0-9]\n"
        "Rules.\n"
        "{L}+ : {token,chars}.\n"
        "zyx{D}+ : {token,zyx}.\n"
        "\\s : skip_token.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[chars,zyx],{1,14}} = string(\"abcdef zyx123\"),\n"
        "    ok.\n">>,
           default,
           ok},

          {ex_3,
      <<"Definitions.\n"
        "NL = [\\n]\n"
        "Rules.\n"
        "{NL}* : {token,newlines}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "     {ok,[],{1,1}} = string(\"\"), ok.\n">>, % string("a") would loop...
           default,
           ok},

          {ex_4,
      <<"Definitions.\n"
        "SP1 = [\\n-\\s]\n"
        "SP0 = [\\000-\\n]\n"
        "Rules.\n"
        "{SP0}+ : {token,{small,TokenChars}}.\n"
        "{SP1}+ : {token,{big,TokenChars}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "     string(\"\\x00\\n\\s\\n\\n\"),\n"
        "     ok.\n">>,
          default,
          ok},

          {ex_5, 
      <<"Definitions.\n"
        "L = [a-z]\n"
        "W = [\\s\\b\\n\\r\\t\\e\\v\\d\\f]\n"
        "Rules.\n"
        "\\[{L}+(,{L}+)*\\] : {token,{list,TokenChars}}.\n"
        "\"{L}+\" : {token,{string,TokenChars}}.\n"
        "\\$. : {token,{char,TokenChars}}.\n"
        "{W}+ : {token,{white,TokenChars}}.\n"
        "ff\\f+ : {token,{form,TokenChars}}.\n"
        "\\$\\^+\\\\+ : {token,{other,TokenChars}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[{white,\"\\b\\f\"}],{1,3}} = string(\"\\b\\f\"),\n"
        "    {ok,[{form,\"ff\\f\"}],{1,4}} = string(\"ff\\f\"),\n"
        "    {ok,[{string,\"\\\"foo\\\"\"}],{1,6}} = string(\"\\\"foo\\\"\"),\n"
        "    {ok,[{char,\"$.\"}],{1,3}} = string(\"$\\.\"),\n"
        "    {ok,[{list,\"[a,b,c]\"}],{1,8}} = string(\"[a,b,c]\"),\n"
        "    {ok,[{other,\"$^\\\\\"}],{1,4}} = string(\"$^\\\\\"),\n"
        "    ok.\n">>,
           default,
           ok},

         {ex_6,
      <<"Definitions.\n"
        "L = [a-z]\n"
        "Rules.\n"
        "L}+ : {token,{TokenChars,#r.f}}.\n"
        "Erlang code.\n"
        "-record(r, {f}).\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    string(\"abc\"),\n"
        "    ok.\n">>,
          default,
          ok},

         {ex_7, %% Assumes regexp can handle \x
      <<"Definitions.\n"
        "H1 = \\x11\\x{ab}\n"
        "H2 = [\\x{30}\\x{ac}]\n"
        "Rules.\n"
        "{H1}{H2}+ : {token,{hex,TokenChars}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[{hex,[17,171,48,172]}],{1,7}} =\n"
        "        string(\"\\x{11}\\xab0\\xac\"),\n"
        "    ok.\n">>,
          default,
          ok}],
    
    run(Config, Ts),
    ok.

ex2(doc) ->
    "More examples.";
ex2(suite) -> [];
ex2(Config) when is_list(Config) ->
    Xrl = 
     <<"
%%% File : erlang_scan.xrl
%%% Author : Robert Virding
%%% Purpose : Token definitions for Erlang.
 
Definitions.
O  = [0-7]
D  = [0-9]
H  = [0-9a-fA-F]
U  = [A-Z]
L  = [a-z]
A  = ({U}|{L}|{D}|_|@)
WS  = ([\\000-\\s]|%.*)
 
Rules.
{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :
      {token,{float,TokenLoc,list_to_float(TokenChars)}}.
{D}+#{H}+  :  base(TokenLoc, TokenChars).
{D}+    :  {token,{integer,TokenLoc,list_to_integer(TokenChars)}}.
{L}{A}*    :  Atom = list_to_atom(TokenChars),
      {token,case reserved_word(Atom) of
         true -> {Atom,TokenLoc};
         false -> {atom,TokenLoc,Atom}
       end}.
'(\\\\\\^.|\\\\.|[^'])*' :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      case catch list_to_atom(string_gen(S)) of
       {'EXIT',_} -> {error,\"illegal atom \" ++ TokenChars};
       Atom -> {token,{atom,TokenLoc,Atom}}
      end.
({U}|_){A}*  :  {token,{var,TokenLoc,list_to_atom(TokenChars)}}.
\"(\\\\\\^.|\\\\.|[^\"])*\" :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      {token,{string,TokenLoc,string_gen(S)}}.
\\$(\\\\{O}{O}{O}|\\\\\\^.|\\\\.|.) :
      {token,{char,TokenLoc,cc_convert(TokenChars)}}.
->    :  {token,{'->',TokenLoc}}.
:-    :  {token,{':-',TokenLoc}}.
\\|\\|    :  {token,{'||',TokenLoc}}.
<-    :  {token,{'<-',TokenLoc}}.
\\+\\+    :  {token,{'++',TokenLoc}}.
--    :  {token,{'--',TokenLoc}}.
=/=    :  {token,{'=/=',TokenLoc}}.
==    :  {token,{'==',TokenLoc}}.
=:=    :  {token,{'=:=',TokenLoc}}.
/=    :  {token,{'/=',TokenLoc}}.
>=    :  {token,{'>=',TokenLoc}}.
=<    :  {token,{'=<',TokenLoc}}.
<=    :  {token,{'<=',TokenLoc}}.
<<    :  {token,{'<<',TokenLoc}}.
>>    :  {token,{'>>',TokenLoc}}.
::    :  {token,{'::',TokenLoc}}.
[]()[}{|!?/;:,.*+#<>=-] :
      {token,{list_to_atom(TokenChars),TokenLoc}}.
\\.{WS}    :  {end_token,{dot,TokenLoc}}.
{WS}+    :  skip_token.
 
Erlang code.
 
-export([reserved_word/1]).

%% reserved_word(Atom) -> Bool
%% return 'true' if Atom is an Erlang reserved word, else 'false'.

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
reserved_word('spec') -> true;
reserved_word(_) -> false.

base(L, Cs) ->
    H = string:chr(Cs, $#),
    case list_to_integer(string:substr(Cs, 1, H-1)) of
        B when B > 16 -> {error,\"illegal base\"};
        B ->
            case base(string:substr(Cs, H+1), B, 0) of
                error -> {error,\"illegal based number\"};
                N -> {token,{integer,L,N}}
            end
    end.

base([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base(Cs, Base, Next);
base([_|_], _, _) -> error;      %Unknown character
base([], _, N) -> N.

cc_convert([$$,$\\\\|Cs]) ->
    hd(string_escape(Cs));
cc_convert([$$,C]) -> C.

string_gen([$\\\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
        O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\\000, C =< $\\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\\n;        %\\n = LF
escape_char($r) -> $\\r;        %\\r = CR
escape_char($t) -> $\\t;        %\\t = TAB
escape_char($v) -> $\\v;        %\\v = VT
escape_char($b) -> $\\b;        %\\b = BS
escape_char($f) -> $\\f;        %\\f = FF
escape_char($e) -> $\\e;        %\\e = ESC
escape_char($s) -> $\\s;        %\\s = SPC
escape_char($d) -> $\\d;        %\\d = DEL
escape_char(C) -> C.
      ">>, % "
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "erlang_scan.xrl"),
    ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "erlang_scan.erl"),
    {ok, _} = leex:file(XrlFile, [{error_location, column}]),
    {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
    code:purge(erlang_scan),
    AbsFile = filename:rootname(ErlFile, ".erl"),
    code:load_abs(AbsFile, erlang_scan),

    F = fun(Cont, Chars, Location) ->
                erlang_scan:tokens(Cont, Chars, Location)
        end,
    F1 = fun(Cont, Chars, Location) ->
                erlang_scan:token(Cont, Chars, Location)
        end,
    fun() ->
            S = "ab cd. ",
            {ok, Ts, {1,8}} = scan_tokens_1(S, F, {1,1}),
            {ok, Ts, {1,8}} = scan_token_1(S, F1, {1,1}),
            {ok, Ts, {1,8}} = scan_tokens(S, F, {1,1}),
            {ok, Ts, {1,8}} = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "'ab\n cd'. ",
            {ok, Ts, {2,7}} = scan_tokens_1(S, F, {1,1}),
            {ok, Ts, {2,7}} = scan_token_1(S, F1, {1,1}),
            {ok, Ts, {2,7}} = scan_tokens(S, F, {1,1}),
            {ok, Ts, {2,7}} = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "99. ",
            {ok, Ts, {1,5}} = scan_tokens_1(S, F, {1,1}),
            {ok, Ts, {1,5}} = scan_token_1(S, F1, {1,1}),
            {ok, Ts, {1,5}} = scan_tokens(S, F, {1,1}),
            {ok, Ts, {1,5}} = erlang_scan:string(S, {1,1})
    end(),
    {ok,[{integer,{1,1},99},{dot,{1,3}}],{1,5}} = erlang_scan:string("99. "),
    fun() ->
            Atom = "'" ++ lists:duplicate(1000,$a) ++ "'",
            S = Atom ++ ". ",
            Reason = "illegal atom " ++ Atom,
            Err = {error,{{1,1003},erlang_scan,{user,Reason}},{1,1003}},
            {done,Err,[]} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[]} = scan_token_1(S, F1, {1,1}),
            {done,Err,[]} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "\x{aaa}. ",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[_]} = scan_token_1(S, F1, {1,1}), % Note: Rest non-empty
            {done,Err,[]} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "\x{aaa} + 1. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[_]} = scan_token_1(S, F1, {1,1}), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "\x{aaa} \x{bbb}. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[_]} = scan_token_1(S, F1, {1,1}), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "\x{aaa} 18#34. 34",
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,[]} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[_]} = scan_token_1(S, F1, {1,1}), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    fun() ->
            S = "\x{aaa}"++eof,
            Err = {error,{{1,1},erlang_scan,{illegal,[2730]}},{1,1}},
            {done,Err,eof} = scan_tokens_1(S, F, {1,1}),
            {done,Err,[_]} = scan_token_1(S, F1, {1,1}), % Note: Rest non-empty
            {done,Err,eof} = scan_tokens(S, F, {1,1}),
            Err = erlang_scan:string(S, {1,1})
    end(),
    ok.

scan_tokens(String, Fun, Location) ->
    scan_tokens(String, Fun, Location, []).

scan_tokens(String, Fun, Location, Rs) ->
    case Fun([], String, Location) of
        {done, {error,_,_}, _} = Error ->
            Error;
        {done, {ok,Ts,End}, ""} ->
            {ok, lists:append(lists:reverse([Ts|Rs])), End};
        {done, {ok,Ts,End}, Rest} ->
            scan_tokens(Rest, Fun, End, [Ts|Rs])
    end.

scan_tokens_1(String, Fun, Location) ->
    scan_tokens_1({more, []}, String, Fun, Location, []).

scan_tokens_1({done, {error, _, _}, _}=Error, _Cs, _Fun, _Location, _Rs) ->
    Error;
scan_tokens_1({done, {ok,Ts,End}, ""}, "", _Fun, _Location, Rs) ->
    {ok,lists:append(lists:reverse([Ts|Rs])),End};
scan_tokens_1({done, {ok,Ts,End}, Rest}, Cs, Fun, _Location, Rs) ->
    scan_tokens_1({more,[]}, Rest++Cs, Fun, End, [Ts|Rs]);
scan_tokens_1({more, Cont}, [C | Cs], Fun, Loc, Rs) ->
    R = Fun(Cont, [C], Loc),
    scan_tokens_1(R, Cs, Fun, Loc, Rs);
scan_tokens_1({more, Cont}, eof, Fun, Loc, Rs) ->
    R = Fun(Cont, eof, Loc),
    scan_tokens_1(R, eof, Fun, Loc, Rs).

scan_token_1(String, Fun, Location) ->
    scan_token_1({more, []}, String, Fun, Location, []).

scan_token_1({done, {error, _, _}, _}=Error, _Cs, _Fun, _Location, _Rs) ->
    Error;
scan_token_1({done, {ok,Ts,End}, ""}, "", _Fun, _Location, Rs) ->
    {ok,lists:reverse([Ts|Rs]),End};
scan_token_1({done, {ok,Ts,End}, Rest}, Cs, Fun, _Location, Rs) ->
    scan_token_1({more,[]}, Rest++Cs, Fun, End, [Ts|Rs]);
scan_token_1({more, Cont}, [C | Cs], Fun, Loc, Rs) ->
    R = Fun(Cont, [C], Loc),
    scan_token_1(R, Cs, Fun, Loc, Rs).

%% End of ex2

line_wrap(doc) ->    "Much more examples.";
line_wrap(suite) -> [];
line_wrap(Config) when is_list(Config) ->
    Xrl =
     <<"
Definitions.
Rules.
[a]+[\\n]*= : {token, {first, TokenLoc}}.
[a]+ : {token, {second, TokenLoc}}.
[\\s\\r\\n\\t]+ : skip_token.
Erlang code.
      ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "test_line_wrap.xrl"),
    ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "test_line_wrap.erl"),
    {ok, _} = leex:file(XrlFile, []),
    {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
    code:purge(test_line_wrap),
    AbsFile = filename:rootname(ErlFile, ".erl"),
    code:load_abs(AbsFile, test_line_wrap),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,{1,1}},{second,{2,1}}],2} = test_line_wrap:string(S)
    end(),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,{3,1}},{second,{4,1}}],4} = test_line_wrap:string(S, 3)
    end(),
    fun() ->
            {done,{ok,{second,{1,1}},1},"\na"} = test_line_wrap:token([], "a\na"),
            {more,Cont1} = test_line_wrap:token([], "\na"),
            {done,{ok,{second,{2,1}},2},eof} = test_line_wrap:token(Cont1, eof)
    end(),
    fun() ->
            {more,Cont1} = test_line_wrap:tokens([], "a\na"),
            {done,{ok,[{second,{1,1}},{second,{2,1}}],2},eof} = test_line_wrap:tokens(Cont1, eof)
    end(),
    ok.

%% End of line_wrap

not_yet(doc) ->
    "Not yet implemented.";
not_yet(suite) -> [];
not_yet(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Ret = [return, {report, true}],
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "Rules.\n"
                                 "$ : .\n"
                                 "Erlang code.\n">>),
    {error,[{_,[{3,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "Rules.\n"
                                 "^ : .\n"
                                 "Erlang code.\n">>),
    {error,[{_,[{3,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),

    ok.

otp_10302(doc) ->
    "OTP-10302. Unicode characters scanner/parser.";
otp_10302(suite) -> [];
otp_10302(Config) when is_list(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Ret = [return, {report, true}],

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "ä"
     >>),
    {error,[{_,[{2,leex,cannot_parse}]}],[]} =
        leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "ä"
     >>),
    {error,[{_,[{3,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),Häpp}}.\n"
     >>),
    {error,[{_,[{7,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    ok = file:write_file(Filename,<<
         "%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars)}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    Häpp\n"
      >>),
    {error,[{_,[{11,leex,cannot_parse}]}],[]} = leex:file(Filename, Ret),

    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    LeexPre = filename:join(Dir, "leexinc.hrl"),
    ok = file:write_file(LeexPre, <<"%% coding: UTF-8\n ä">>),
    PreErrors = run_test(Config, Mini, LeexPre),
    {error,[{IncludeFile,[{2,leex,cannot_parse}]}],[]} = PreErrors,
    "leexinc.hrl" = filename:basename(IncludeFile),

    Ts = [{uni_1,
       <<"%% coding: UTF-8\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),\n"
         "begin HÃ¤pp = foo, HÃ¤pp end,"
         " 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    %% HÃ¤pp, 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"\n"
         "    {ok, [R], {1,4}} = string(\"tip\"),\n"
         "    {tip,foo,'HÃ¤pp',[1024,66],[246,114,110,95,1024]} = R,\n"
         "    HÃ¤pp = foo,\n"
         "    {tip, HÃ¤pp, 'HÃ¤pp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,\n"
         "    ok.\n">>,
          default,
          ok},
      {uni_2,
       <<"%% coding: Latin-1\n"
         "Definitions.\n"
         "A = a\n"
         "L = [{A}-{Z}]\n"
         "Z = z\n"
         "Rules.\n"
         "{L}+ : {token,{list_to_atom(TokenChars),\n"
         "begin Häpp = foo, Häpp end,"
         " 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"}}.\n"
         "Erlang code.\n"
         "-export([t/0]).\n"
         "t() ->\n"
         "    %% Häpp, 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"\n"
         "    {ok, [R], {1,4}} = string(\"tip\"),\n"
         "    {tip,foo,'Häpp',[1024,66],[195,182,114,110,95,208,128]} = R,\n"
         "    Häpp = foo,\n"
         "    {tip, Häpp, 'Häpp',\"\\x{400}B\",\"Ã¶rn_Ð\"} = R,\n"
         "    ok.\n">>,
          default,
          ok}],
    run(Config, Ts),

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
    Filename = filename:join(UDir, 'OTP-11286.xrl'),
    Scannerfile = filename:join(UDir, 'OTP-11286.erl'),
    Options = [return, {scannerfile, Scannerfile}],

    Mini1 = <<"%% coding: utf-8\n"
              "Definitions.\n"
              "D  = [0-9]\n"
              "Rules.\n"
              "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
              "Erlang code.\n">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini1]),
    {ok, _, []} = rpc:call(Node, leex, file, [Filename, Options]),
    {ok,_,_} = rpc:call(Node, compile, file,
                  [Scannerfile,[basic_validation,return]]),

    Mini2 = <<"Definitions.\n"
              "D  = [0-9]\n"
              "Rules.\n"
              "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
              "Erlang code.\n">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini2]),
    {ok, _, []} = rpc:call(Node, leex, file, [Filename, Options]),
    {ok,_,_} = rpc:call(Node, compile, file,
                  [Scannerfile,[basic_validation,return]]),

    Mini3 = <<"%% coding: latin-1\n"
              "Definitions.\n"
              "D  = [0-9]\n"
              "Rules.\n"
              "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
              "Erlang code.\n">>,
    ok = rpc:call(Node, file, write_file, [Filename, Mini3]),
    {ok, _, []} = rpc:call(Node, leex, file, [Filename, Options]),
    {ok,_,_} = rpc:call(Node, compile, file,
                  [Scannerfile,[basic_validation,return]]),

    peer:stop(Peer),
    ok.

otp_13916(doc) ->
    "OTP-13916. Leex rules with newlines result in bad line numbers";
otp_13916(suite) -> [];
otp_13916(Config) when is_list(Config) ->
    Ts = [{otp_13916_1,
           <<"Definitions.\n"
             "W = [a-zA-Z0-9]\n"
             "S = [\\s\\t]\n"
             "B = [\\n\\r]\n"
             "Rules.\n"
             "%% mark line break(s) and empty lines by token 'break'\n"
             "%% in order to use as delimiters\n"
             "{B}({S}*{B})+ : {token, {break,   TokenLoc}}.\n"
             "{B}           : {token, {break,   TokenLoc}}.\n"
             "{S}+          : {token, {blank,   TokenLoc, TokenChars}}.\n"
             "{W}+          : {token, {word,    TokenLoc, TokenChars}}.\n"
             "Erlang code.\n"
             "-export([t/0]).\n"
             "t() ->\n"
             "    {ok,[{break,{1,1}},{blank,{4,1},\"  \"},{word,{4,3},\"breaks\"}],{4,9}} =\n"
             "        string(\"\\n\\n  \\n  breaks\"),\n"
             "{ok,[{break,{1,1}},{word,{4,1},\"works\"}],{4,6}} =\n"
             "        string(\"\\n\\n  \\nworks\"),\n"
             "    {ok,[{break,{1,1}},{word,{4,1},\"L4\"},{break,{4,3}},\n"
             "         {word,{5,1},\"L5\"},{break,{5,3}},{word,{7,1},\"L7\"}], {7,3}} =\n"
             "        string(\"\\n\\n  \\nL4\\nL5\\n\\nL7\"),\n"
             "{ok,[{break,{1,1}},{blank,{4,1},\" \"},{word,{4,2} ,\"L4\"},\n"
             "     {break,{4,4}},{blank,{5,1},\" \"},{word,{5,2},\"L5\"},\n"
             "     {break,{5,4}},{blank,{7,1},\" \"},{word,{7,2},\"L7\"}], {7,4}} =\n"
             "        string(\"\\n\\n  \\n L4\\n L5\\n\\n L7\"),\n"
             "    ok.\n">>,
           default,
           ok}],
    run(Config, Ts),
    ok.

otp_14285(Config) ->
    %% x{400} takes 2 bytes to represent
    Ts = [{otp_14285_1,
           <<"%% encoding: latin-1\n"
             "Definitions.\n"
             "A = a\n"
             "Z = z\n"
             "L = [{A}-{Z}]\n"
             "U = [\\x{400}]\n"
             "Rules.\n"
             "{L}+ : {token,l}.\n"
             "{U}+ : {token,{TokenLine,'\\x{400}'}}.\n"
             "Erlang code.\n"
             "-export([t/0]).\n"
             "t() ->\n"
             "    {ok,[{1,'\\x{400}'}],{1,3}} = string(\"\\x{400}\"), ok.\n">>,
           default,
           ok},
          {otp_14285_2,
           <<"%% encoding: UTF-8\n"
             "Definitions.\n"
             "A = a\n"
             "Z = z\n"
             "L = [{A}-{Z}]\n"
             "U = [\x{400}]\n"
             "Rules.\n"
             "{L}+ : {token,l}.\n"
             "{U}+ : {token,'\x{400}'}.\n"
             "Erlang code.\n"
             "-export([t/0]).\n"
             "t() ->\n"
             "    {ok,['\x{400}'],{1,3}} = string(\"\x{400}\"), ok.\n"/utf8>>,
           default,
           ok}],
    run(Config, Ts),
    ok.

otp_17023(Config) ->
    Dir = ?privdir,
    Filename = filename:join(Dir, "file.xrl"),
    Ret = [return, {report, true}],

    {'EXIT', {badarg, _}} = (catch leex:file(Filename, [{noopt,true}])),
    OldEnv = os:getenv("ERL_COMPILER_OPTIONS"),
    true = os:putenv("ERL_COMPILER_OPTIONS", "strong_validation"),
    ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "Rules.\n"
                                 "^ : .\n"
                                 "Erlang code.\n">>),
    {error,[{_,[{3,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    true = os:putenv("ERL_COMPILER_OPTIONS", "{return, false}"),
    error = leex:file(Filename, Ret),
    error = leex:file(Filename, [return | Ret]), % overridden
    case OldEnv of
        false ->
            os:unsetenv("ERL_COMPILER_OPTIONS");
        _ ->
            os:putenv("ERL_COMPILER_OPTIONS", OldEnv)
    end,
    ok.

%% Additional tests added with column support
column_support(Config) ->
    Ts = [{token_col_var,
        <<"Definitions.\n"
        "D = [0-9]\n"
        "W = [\\s\\n]\n"
        "Rules.\n"
        "{W}+ :\n"
        "skip_token.\n"
        "{D}+ :\n"
        "{token,{integer,{TokenLine,TokenCol},list_to_integer(TokenChars)}}.\n"
        "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
        "{token,{float,{TokenLine,TokenCol},list_to_float(TokenChars)}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "{ok,[{float, {2,1}, 4.44},{integer, {3,3}, 5},{integer, {7,3}, 7}],{8,2}}"
        "= string(\"\n4.44  \n  5 \n  \n\n\n  7 \n \"), ok.\n">>,
            default,
            ok},
        {tab,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, TokenLoc}}.\n"
            "[a]+ : {token, {second, TokenLoc}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,27}},{second,{2,19}}],{2,25}} = string(\"   \t \t\t  a\\n \t \t  aaa\t\"), ok.\n">>,
        default,
        ok},
        {tab_custom_size,
        <<"Definitions.\n"
            "Rules.\n"
            "[a]+[\\n]*= : {token, {first, TokenLoc}}.\n"
            "[a]+ : {token, {second, TokenLoc}}.\n"
            "[\\s\\r\\n\\t]+ : skip_token.\n"
            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->\n"
            "{ok,[{second,{1,15}},{second,{2,9}}],{2,16}} = string(\"   \t \t\t  a\\n \t \t  aaa\t\"), ok.\n">>,
            default,
            [{tab_size,3}],
        ok}],
    run(Config, Ts),
    ok.

%% OTP-17499. GH-4918.
compiler_warnings(Config) ->
    Xrl =
        <<"
Definitions.
Rules.
/(\\\\.|[^\\\\/]|\\[(/|\\\\.|[^\\\\/])+\\]|\\((\\\\.|/|[^\\\\/])+\\)+)*/ : a.
/\\*([^*][\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+/ : b.
Erlang code.
         ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "compiler_warnings.xrl"),
    ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "compiler_warnings.erl"),
erlang:display({xrlfile,XrlFile}),
    {ok, _} = leex:file(XrlFile, []),
erlang:display({erlfile,ErlFile}),
    {ok, compiler_warnings, []} = compile:file(ErlFile, [return]),
    ok.

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    ok = file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    ok = file:write_file_info(Fname, Info#file_info{mode = Mode}).

run(Config, Tests) ->
    F = fun F({N,P,Pre,E}) ->
                F({N,P,Pre,[],E});
            F({N,P,Pre,Opts,E}) ->
            case catch run_test(Config,P,Pre,Opts) of
                E ->
                    ok;
                Bad ->
                    ct:fail("~nTest ~p failed. Expected~n  ~p~n"
                              "but got~n  ~p~n", [N, E, Bad])
            end
        end,
    lists:foreach(F, Tests).

run_test(Config, Def, Pre) ->
    run_test(Config, Def, Pre, []).

run_test(Config, Def, Pre, LOpts0) ->
    %% io:format("testing ~s~n", [binary_to_list(Def)]),
    DefFile = 'leex_test.xrl',
    Filename = 'leex_test.erl',
    DataDir = ?privdir,
    XrlFile = filename:join(DataDir, DefFile),
    ErlFile = filename:join(DataDir, Filename),
    Opts = [return, warn_unused_vars,{outdir,DataDir}],
    ok = file:write_file(XrlFile, Def),
    LOpts = LOpts0 ++ [return, {report, false} |
             case Pre of
                 default ->
                     [];
                 _ ->
                     [{includefile,Pre}]
             end],
    XOpts = [verbose, dfa_graph, {error_location, column}], % just to get some code coverage...
    LRet = leex:file(XrlFile, XOpts ++ LOpts),
    case LRet of
        {ok, _Outfile, _LWs} ->
                 CRet = compile:file(ErlFile, Opts),
                 case CRet of
                     {ok, _M, _Ws} -> 
                         AbsFile = filename:rootname(ErlFile, ".erl"),
                         Mod = leex_test,
                         code:purge(Mod),
                         code:load_abs(AbsFile, Mod),
                         Mod:t();
                         %% warnings(ErlFile, Ws);
                     {error, [{ErlFile,Es}], []} -> {error, Es, []};
                     {error, [{ErlFile,Es}], [{ErlFile,Ws}]} -> {error, Es, Ws};
                     Error  -> Error
                 end;
        {error, [{XrlFile,LEs}], []} -> {error, LEs, []};
        {error, [{XrlFile,LEs}], [{XrlFile,LWs}]} -> {error, LEs, LWs};
        LError -> LError
    end.

extract(File, {error, Es, Ws}) ->
    {errors, extract(File, Es), extract(File, Ws)};    
extract(File, Ts) ->
    lists:append([T || {F, T} <- Ts,  F =:= File]).

search_for_file_attr(PartialFilePathRegex, Forms) ->
    lists:search(fun
                   ({attribute, _, file, {FileAttr, _}}) ->
                      case re:run(FileAttr, PartialFilePathRegex, [unicode]) of
                        nomatch -> false;
                        _ -> true
                      end;
                   (_) -> false end,
                 Forms).
