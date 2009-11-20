%%
%% Demo file for the Syntax Tools package.
%%
%% The program is self-instructing. Compile `demo' from the shell and
%% execute `demo:run()'.

-module(demo).

-export([run/0, run_1/0, run_2/0, run_3/0, run_4/0, 
	 view/1, view/2, view/3]).

small_file() -> "test.erl".

medium_file() -> "test_comprehensions.erl".

big_file() -> "erl_comment_scan.erl".

run() ->
    make:all([load]),
    io:fwrite("\n\n** Enter `demo:run_1()' to parse and pretty-print\n"
	      "the file \"~s\" with the default field width.\n\n",
	      [small_file()]),
    ok.

run_1() ->
    view(small_file()),
    io:fwrite("\n\n\n** Enter `demo:run_2()' to parse and pretty-print\n"
	      "the file \"~s\" with a small field width.\n\n",
	      [small_file()]),
    ok.

run_2() ->
    view(small_file(), 15),
    io:fwrite("\n\n\n** Enter `demo:run_3()' to parse and pretty-print\n"
	      "the file \"~s\" with field width 35\n\n",
	      [medium_file()]),
    ok.

run_3() ->
   view(medium_file(), 35),
    io:fwrite("\n\n\n** Enter `demo:run_4()' to parse and pretty-print\n"
	      "the file \"~s\" with field width 55 and ribbon width 40.\n\n",
	      [big_file()]),
    ok.

run_4() ->
    view(big_file(), 55, 40),
    io:fwrite("\n\n\n** Done! Now you can play around with the function\n"
	      "`demo:view(FileName, PaperWidth, RibbonWidth)' on any\n"
	      "Erlang source files you have around you.\n"
	      "(Include the \".erl\" suffix in the file name.\n"
	      "RibbonWidth and PaperWidth are optional.)\n\n"),
    ok.

view(Name) ->
    SyntaxTree = read(Name),
    print(SyntaxTree).

view(Name, Paper) ->
    SyntaxTree = read(Name),
    print(SyntaxTree, Paper).

view(Name, Paper, Ribbon) ->
    SyntaxTree = read(Name),
    print(SyntaxTree, Paper, Ribbon).

print(SyntaxTree) ->
    io:put_chars(erl_prettypr:format(SyntaxTree)).

print(SyntaxTree, Paper) ->
    io:put_chars(erl_prettypr:format(SyntaxTree, [{paper, Paper}])).

print(SyntaxTree, Paper, Ribbon) ->
    io:put_chars(erl_prettypr:format(SyntaxTree, [{paper, Paper},
						  {ribbon, Ribbon}])).

read(Name) ->
    {ok, Forms} = epp:parse_file(Name, [], []),
    Comments = erl_comment_scan:file(Name),
    erl_recomment:recomment_forms(Forms, Comments).
