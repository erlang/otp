%% Comment with <html> code &amp; </html>
%% and also some "quotes" and 'single quotes'

-module(m1).

-compile(export_all).

-include("header1.hrl").
-include("header2.hrl").
-include("header3.hrl").

-define(MACRO1,value).

%% This macro is used to select parser in erl2html2.
%% If EPP_SWITCH is defined epp is used, else epp_dodger.
epp_switch() ->
    ?EPP_SWITCH.

%%% Comment
foo(x) ->
    %% Comment
    ok_x;
foo(y) ->
    %% Second clause
    ok_y.

'quoted_foo'() ->
    ok.

'quoted_foo_with_"_and_/'() ->
    ok.

'quoted_foo_with_(_and_)'() ->
    ok.

'quoted_foo_with_<_and_>'() ->
    ok.

bar() ->
    do_something(),
ok. % indentation error, OTP-9710

%% Function inside macro definition
?MACRO_DEFINING_A_FUNCTION.

%% Two function one one line
quuux() -> ok. quuuux() -> ok.

%% do_something/0 does something
do_something() ->
    ?MACRO1.
%% comments after last line
