%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-include_lib("kernel/include/file.hrl").

-ifdef(debug).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(datadir, "leex_SUITE_data").
-define(privdir, "leex_SUITE_priv").
-define(t, test_server).
-else.
-include_lib("common_test/include/ct.hrl").
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 file/1, compile/1, syntax/1,
	 
	 pt/1, man/1, ex/1, ex2/1, not_yet/1,
	 line_wrap/1,
	 otp_10302/1, otp_11286/1, unicode/1]).

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, checks}, {group, examples}, {group, bugs}].

groups() -> 
    [{checks, [], [file, compile, syntax]},
     {examples, [], [pt, man, ex, ex2, not_yet, unicode]},
     {tickets, [], [otp_10302, otp_11286]},
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
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", Ret),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", [{return,true}]),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file("not_a_file", [{report,false},return_errors]),
    ?line error = leex:file("not_a_file"),
    ?line error = leex:file("not_a_file", [{return,false},report]),
    ?line error = leex:file("not_a_file", [return_warnings,{report,false}]),

    Filename = filename:join(Dir, "file.xrl"),
    file:delete(Filename),

    ?line {'EXIT', {badarg, _}} = (catch leex:file({foo})),
    ?line {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {parserfile,{foo}})),
    ?line {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {includefile,{foo}})),

    ?line {'EXIT', {badarg, _}} = (catch leex:file(Filename, no_option)),
    ?line {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, [return | report])),
    ?line {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, {return,foo})),
    ?line {'EXIT', {badarg, _}} = 
        (catch leex:file(Filename, includefile)),

    Mini = <<"Definitions.\n"
             "D  = [0-9]\n"
             "Rules.\n"
             "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
             "Erlang code.\n">>,
    ?line ok = file:write_file(Filename, Mini),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [{scannerfile,"//"} | Ret]),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [{includefile,"//"} | Ret]),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [{includefile,"/ /"} | Ret]),

    LeexPre = filename:join(Dir, "leexinc.hrl"),
    ?line ok = file:write_file(LeexPre, <<"syntax error.\n">>),
    PreErrors = run_test(Config, Mini, LeexPre),
    ?line {errors,
           [{1,_,["syntax error before: ","error"]},
            {3,_,undefined_module}],
           []} =
        extract(LeexPre, PreErrors),
    file:delete(LeexPre),

    Ret2 = [return, report_errors, report_warnings, verbose],
    Scannerfile = filename:join(Dir, "file.erl"),
    ?line ok = file:write_file(Scannerfile, <<"nothing">>),
    ?line unwritable(Scannerfile),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, Ret2),
    ?line writable(Scannerfile),
    file:delete(Scannerfile),

    Dotfile = filename:join(Dir, "file.dot"),
    ?line ok = file:write_file(Dotfile, <<"nothing">>),
    ?line unwritable(Dotfile),
    ?line {error,[{_,[{none,leex,{file_error,_}}]}],[]} = 
        leex:file(Filename, [dfa_graph | Ret2]),
    ?line writable(Dotfile),
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
    ?line ok = file:write_file(Filename, Mini),
    ?line error = leex:compile(Filename, "//", #options{}),
    ?line ok = leex:compile(Filename, Scannerfile, #options{}),
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
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "%% comment\n"
                                 "Rules.\n"
                                 "{L}+  : {token,{word,TokenLine,TokenChars}}.\n
                                 ">>),
    ?line {error,[{_,[{7,leex,missing_code}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+  : \n">>),
    ?line {error,[{_,[{5,leex,missing_code}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] :">>),
    ?line {error,[{_,[{4,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ : .\n"
                                 "[] : ">>),
    ?line {error,[{_,[{5,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] : .\n">>),
    ?line {error,[{_,[{4,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ ">>),
    ?line {error,[{_,[{5,leex,bad_rule}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+ ; ">>),
    ?line {error,[{_,[{4,leex,bad_rule}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[] : '99\n">>),
    ?line {error,[{_,[{4,erl_scan,_}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n">>),
    ?line {error,[{_,[{3,leex,empty_rules}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "Erlang code.\n">>),
    ?line {error,[{_,[{4,leex,empty_rules}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n">>),
    ?line {error,[{_,[{2,leex,missing_rules}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Erlang code.\n">>),
    ?line {error,[{_,[{3,leex,missing_rules}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"">>),
    %% This is a weird line:
    ?line {error,[{_,[{0,leex,missing_defs}]}],[]} = leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename, 
                               <<"Rules.\n">>),
    ?line {error,[{_,[{1,leex,missing_defs}]}],[]} = leex:file(Filename, Ret),

    %% Check that correct line number is used in messages.
    ErlFile = filename:join(Dir, "file.erl"),
    Ret1 = [{scannerfile,ErlFile}|Ret],
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+  : {token,\n"
                                 "         {word,TokenLine,TokenChars,\n"
                                 "          DDDD}}.\n" % unbound
                                 "Erlang code.\n"
                                 "an error.\n">>),     % syntax error
    ?line {ok, _, []} = leex:file(Filename, Ret1),
    ?line {error, 
           [{_,[{8,_,["syntax error before: ","error"]}]},
            {_,[{6,_,{unbound_var,'DDDD'}}]}],
           []} =
        compile:file(ErlFile, [basic_validation, return]),

    %% Ignored characters
    ?line ok = file:write_file(Filename,
                               <<"Definitions. D = [0-9]\n"
                                 "Rules. [a-z] : .\n"
                                 "1 : skip_token.\n"
                                 "Erlang code. f() -> a.\n">>),
    ?line {ok,_,[{_,
                  [{1,leex,ignored_characters},
                   {2,leex,ignored_characters},
                   {4,leex,ignored_characters}]}]} = 
        leex:file(Filename, Ret),

    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\  : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{unterminated,"\\"}}}]}],[]} =
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\x  : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{illegal_char,"\\x"}}}]}],[]} =
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "{L}+\\x{  : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{unterminated,"\\x{"}}}]}],[]} =
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[^ab : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{unterminated,"["}}}]}],[]} =
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "(a : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{unterminated,"("}}}]}],[]} =
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "[b-a] : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,{char_class,"b-a"}}}]}],[]} =
        leex:file(Filename, Ret),

    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "D  = [0-9]\n"
                                 "Rules.\n"
                                 "\\x{333333333333333333333333} : token.\n">>),
    ?line {error,[{_,[{4,leex,{regexp,
                                {illegal_char,
                                 "\\x{333333333333333333333333}"}}}]}],[]} =
        leex:file(Filename, Ret),
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
            "{L}+  : {token,{word,TokenLine,TokenChars}}.\n"
            "abc{D}+  : {skip_token,\"sture\" ++ string:substr(TokenChars, 4)}.\n"
            "{D}+  : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.\n"
            "\\s  : .\n"
            "\\r\\n  : {end_token,{crlf,TokenLine}}.\n"

            "Erlang code.\n"
            "-export([t/0]).\n"
            "t() ->
                 {ok,[{word,1,\"sture\"},{integer,1,123}],1} =
                     string(\"abc123\"), ok. ">>,
           default,
           ok}],

    ?line run(Config, Ts),
    ok.

unicode(suite) ->
    [];
unicode(Config) when is_list(Config) ->
    Ts = [{unicode_1, 
	   <<"%% -*- coding: utf-8 -*-\n"
	     "Definitions.\n"
	     "RTLarrow    = (←)\n"
	     "Rules.\n"
	     "{RTLarrow}  : {token,{'<-',TokenLine}}.\n"
	     "Erlang code.\n"
	     "-export([t/0]).\n"
	     "t() -> {ok, [{'<-', 1}], 1} = string(\"←\"), ok.">>,
           default,
           ok}],

    ?line run(Config, Ts),
    ok.

man(doc) ->
    "Examples from the manpage.";
man(suite) -> [];
man(Config) when is_list(Config) ->
    Ts = [{man_1, 
     <<"Definitions.\n"
       "Rules.\n"
       "[a-z][0-9a-zA-Z_]* :\n"
       "    {token,{atom,TokenLine,list_to_atom(TokenChars)}}.\n"
       "[A-Z_][0-9a-zA-Z_]* :\n"
       "    {token,{var,TokenLine,list_to_atom(TokenChars)}}.\n"
       "(\\+|-)?[0-9]+\\.[0-9]+((E|e)(\\+|-)?[0-9]+)? : \n"
       "   {token,{float,TokenLine,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,1,3.14},{atom,1,atom},{var,1,'V314'}],1} =\n"
       "        string(\"3.14atom V314\"),\n"
       "    ok.\n">>,
           default,
           ok},

          {man_2,
     <<"Definitions.\n"
       "D = [0-9]\n"
       "Rules.\n"
       "{D}+ :\n"
       "  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.\n"
       "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :\n"
       "  {token,{float,TokenLine,list_to_float(TokenChars)}}.\n"
       "\\s : skip_token.\n"
       "Erlang code.\n"
       "-export([t/0]).\n"
       "t() ->\n"
       "    {ok,[{float,1,3.14},{integer,1,314}],1} = \n"
       "        string(\"3.14 314\"),\n"
       "    ok.\n">>,
           default,
           ok}],
    
    ?line run(Config, Ts),
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
        "  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.\n"
        "[^235]+ :\n"
        "  {token,{list_to_atom(TokenChars),TokenLine}}.\n"
        "Erlang code.\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    {ok,[{integer,1,12},{' c\\na',1},{integer,2,34},{b789a,2}],2} =\n"
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
        "    {ok,[chars,zyx],1} = string(\"abcdef zyx123\"),\n"
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
        "     {ok,[],1} = string(\"\"), ok.\n">>, % string("a") would loop...
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
        "    {ok,[{white,\"\\b\\f\"}],1} = string(\"\\b\\f\"),\n"
        "    {ok,[{form,\"ff\\f\"}],1} = string(\"ff\\f\"),\n"
        "    {ok,[{string,\"\\\"foo\\\"\"}],1} = string(\"\\\"foo\\\"\"),\n"
        "    {ok,[{char,\"$.\"}],1} = string(\"$\\.\"),\n"
        "    {ok,[{list,\"[a,b,c]\"}],1} = string(\"[a,b,c]\"),\n"
        "    {ok,[{other,\"$^\\\\\"}],1} = string(\"$^\\\\\"),\n"
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
        "    {ok,[{hex,[17,171,48,172]}],1} =\n"
        "        string(\"\\x{11}\\xab0\\xac\"),\n"
        "    ok.\n">>,
          default,
          ok}],
    
    ?line run(Config, Ts),
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
      {token,{float,TokenLine,list_to_float(TokenChars)}}.
{D}+#{H}+  :  base(TokenLine, TokenChars).
{D}+    :  {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{L}{A}*    :  Atom = list_to_atom(TokenChars),
      {token,case reserved_word(Atom) of
         true -> {Atom,TokenLine};
         false -> {atom,TokenLine,Atom}
       end}.
'(\\\\\\^.|\\\\.|[^'])*' :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      case catch list_to_atom(string_gen(S)) of
       {'EXIT',_} -> {error,\"illegal atom \" ++ TokenChars};
       Atom -> {token,{atom,TokenLine,Atom}}
      end.
({U}|_){A}*  :  {token,{var,TokenLine,list_to_atom(TokenChars)}}.
\"(\\\\\\^.|\\\\.|[^\"])*\" :
      %% Strip quotes.
      S = lists:sublist(TokenChars, 2, TokenLen - 2),
      {token,{string,TokenLine,string_gen(S)}}.
\\$(\\\\{O}{O}{O}|\\\\\\^.|\\\\.|.) :
      {token,{char,TokenLine,cc_convert(TokenChars)}}.
->    :  {token,{'->',TokenLine}}.
:-    :  {token,{':-',TokenLine}}.
\\|\\|    :  {token,{'||',TokenLine}}.
<-    :  {token,{'<-',TokenLine}}.
\\+\\+    :  {token,{'++',TokenLine}}.
--    :  {token,{'--',TokenLine}}.
=/=    :  {token,{'=/=',TokenLine}}.
==    :  {token,{'==',TokenLine}}.
=:=    :  {token,{'=:=',TokenLine}}.
/=    :  {token,{'/=',TokenLine}}.
>=    :  {token,{'>=',TokenLine}}.
=<    :  {token,{'=<',TokenLine}}.
<=    :  {token,{'<=',TokenLine}}.
<<    :  {token,{'<<',TokenLine}}.
>>    :  {token,{'>>',TokenLine}}.
::    :  {token,{'::',TokenLine}}.
[]()[}{|!?/;:,.*+#<>=-] :
      {token,{list_to_atom(TokenChars),TokenLine}}.
\\.{WS}    :  {end_token,{dot,TokenLine}}.
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
      ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "erlang_scan.xrl"),
    ?line ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "erlang_scan.erl"),
    ?line {ok, _} = leex:file(XrlFile, []),
    ?line {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
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
            {ok, Ts, 1} = scan_tokens_1(S, F, 1),
            {ok, Ts, 1} = scan_token_1(S, F1, 1),
            {ok, Ts, 1} = scan_tokens(S, F, 1),
            {ok, Ts, 1} = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "'ab\n cd'. ",
            {ok, Ts, 2} = scan_tokens_1(S, F, 1),
            {ok, Ts, 2} = scan_token_1(S, F1, 1),
            {ok, Ts, 2} = scan_tokens(S, F, 1),
            {ok, Ts, 2} = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "99. ",
            {ok, Ts, 1} = scan_tokens_1(S, F, 1),
            {ok, Ts, 1} = scan_token_1(S, F1, 1),
            {ok, Ts, 1} = scan_tokens(S, F, 1),
            {ok, Ts, 1} = erlang_scan:string(S, 1)
    end(),
    {ok,[{integer,1,99},{dot,1}],1} = erlang_scan:string("99. "),
    fun() ->
            Atom = "'" ++ lists:duplicate(1000,$a) ++ "'",
            S = Atom ++ ". ",
            Reason = "illegal atom " ++ Atom,
            Err = {error,{1,erlang_scan,{user,Reason}},1},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[]} = scan_token_1(S, F1, 1),
            {done,Err,[]} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa}. ",
            Err = {error,{1,erlang_scan,{illegal,[2730]}},1},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,[]} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} + 1. 34",
            Err = {error,{1,erlang_scan,{illegal,[2730]}},1},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} \x{bbb}. 34",
            Err = {error,{1,erlang_scan,{illegal,[2730]}},1},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa} 18#34. 34",
            Err = {error,{1,erlang_scan,{illegal,[2730]}},1},
            {done,Err,[]} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,"34"} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
    end(),
    fun() ->
            S = "\x{aaa}"++eof,
            Err = {error,{1,erlang_scan,{illegal,[2730]}},1},
            {done,Err,eof} = scan_tokens_1(S, F, 1),
            {done,Err,[_]} = scan_token_1(S, F1, 1), % Note: Rest non-empty
            {done,Err,eof} = scan_tokens(S, F, 1),
            Err = erlang_scan:string(S, 1)
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
[a]+[\\n]*= : {token, {first, TokenLine}}.
[a]+ : {token, {second, TokenLine}}.
[\\s\\r\\n\\t]+ : skip_token.
Erlang code.
      ">>,
    Dir = ?privdir,
    XrlFile = filename:join(Dir, "test_line_wrap.xrl"),
    ?line ok = file:write_file(XrlFile, Xrl),
    ErlFile = filename:join(Dir, "test_line_wrap.erl"),
    {ok, _} = leex:file(XrlFile, []),
    {ok, _} = compile:file(ErlFile, [{outdir,Dir}]),
    code:purge(test_line_wrap),
    AbsFile = filename:rootname(ErlFile, ".erl"),
    code:load_abs(AbsFile, test_line_wrap),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,1},{second,2}],2} = test_line_wrap:string(S)
    end(),
    fun() ->
            S = "aaa\naaa",
            {ok,[{second,3},{second,4}],4} = test_line_wrap:string(S, 3)
    end(),
    fun() ->
            {done,{ok,{second,1},1},"\na"} = test_line_wrap:token([], "a\na"),
            {more,Cont1} = test_line_wrap:token([], "\na"),
            {done,{ok,{second,2},2},eof} = test_line_wrap:token(Cont1, eof)
    end(),
    fun() ->
            {more,Cont1} = test_line_wrap:tokens([], "a\na"),
            {done,{ok,[{second,1},{second,2}],2},eof} = test_line_wrap:tokens(Cont1, eof)
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
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "Rules.\n"
                                 "$ : .\n"
                                 "Erlang code.\n">>),
    ?line {error,[{_,[{3,leex,{regexp,_}}]}],[]} = 
        leex:file(Filename, Ret),
    ?line ok = file:write_file(Filename,
                               <<"Definitions.\n"
                                 "Rules.\n"
                                 "^ : .\n"
                                 "Erlang code.\n">>),
    ?line {error,[{_,[{3,leex,{regexp,_}}]}],[]} = 
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
    ?line ok = file:write_file(LeexPre, <<"%% coding: UTF-8\n ä">>),
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
         "    {ok, [R], 1} = string(\"tip\"),\n"
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
         "    {ok, [R], 1} = string(\"tip\"),\n"
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
    Node = start_node(otp_11286, "+fnu"),
    Dir = ?privdir,
    UName = [1024] ++ "u",
    UDir = filename:join(Dir, UName),
    ok = rpc:call(Node, file, make_dir, [UDir]),

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

    true = test_server:stop_node(Node),
    ok.

start_node(Name, Args) ->
    [_,Host] = string:tokens(atom_to_list(node()), "@"),
    ct:log("Trying to start ~w@~s~n", [Name,Host]),
    case test_server:start_node(Name, peer, [{args,Args}]) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    Node
    end.

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    ok = file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    ok = file:write_file_info(Fname, Info#file_info{mode = Mode}).

run(Config, Tests) ->
    F = fun({N,P,Pre,E}) ->
                case catch run_test(Config, P, Pre) of
                    E -> 
                        ok;
                    Bad -> 
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foreach(F, Tests).

run_test(Config, Def, Pre) ->
    %% io:format("testing ~s~n", [binary_to_list(Def)]),
    DefFile = 'leex_test.xrl',
    Filename = 'leex_test.erl',
    DataDir = ?privdir,
    XrlFile = filename:join(DataDir, DefFile),
    ErlFile = filename:join(DataDir, Filename),
    Opts = [return, warn_unused_vars,{outdir,DataDir}],
    ok = file:write_file(XrlFile, Def, [{encoding, unicode}]),
    LOpts = [return, {report, false} | 
             case Pre of
                 default ->
                     [];
                 _ ->
                     [{includefile,Pre}]
             end],
    XOpts = [verbose, dfa_graph], % just to get some code coverage...
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

fail() ->
    ?t:fail().
