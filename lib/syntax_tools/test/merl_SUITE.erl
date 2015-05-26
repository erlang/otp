%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
-module(merl_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% include the Merl header file
-include_lib("syntax_tools/include/merl.hrl").

%% for assert macros
-include_lib("eunit/include/eunit.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([merl_smoke_test/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [merl_smoke_test].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(tokens2str(X), ??X).

merl_smoke_test(Config) when is_list(Config) ->
    ?assertThrow({error, "1: syntax error before: '{'" ++ _},
                 f(merl:quote("{"))),
    ?assertEqual(tuple, erl_syntax:type(merl:term({}))),
    ?assertEqual("{foo, 42}", f(merl:term({foo, 42}))),
    ?assertEqual("f(X) -> {ok, X}.", f(?Q("f(X) -> {ok, X}."))),
    ?assertEqual("{foo, 42}", f(?Q("{foo, 42}"))),
    ?assertEqual("2 + 2", f(?Q("2 + 2"))),
    ?assertEqual("%% comment preserved\n{foo, 42}",
                 f(?Q(["%% comment preserved", "{foo, 42}"]))),
    ?assertEqual("'@foo'", f(merl:tree(merl:template(?Q("'@foo'"))))),
    ?assertEqual("42", f(merl:subst(?Q("_@foo"), [{foo, merl:term(42)}]))),
    ?assertEqual({ok, []}, merl:match(?Q("foo"), ?Q("foo"))),
    ?assertEqual(42, merl:switch(?Q("foo"), [fun () -> 42 end])),
    ?assertEqual("{foo}", f(begin Foo = ?Q("foo"), ?Q("{_@Foo}") end)),
    ?assertEqual("{foo}", f(begin Foo = foo, ?Q("{_@Foo@}") end)),
    ?assertEqual("{[bar], baz()}",
                 f(begin
                       Tree = ?Q("{foo, [bar], baz()}"),
                       ?Q("{foo, _@Bar, '@Baz'}") = Tree,
                       ?Q("{_@Bar, _@Baz}")
                   end)),
    ?assertEqual("{[bar], baz()}",
                 f(begin
                       Tree = ?Q("{foo, [bar], baz()}"),
                       case Tree of
                           ?Q("{foo, _@Bar, '@Baz'}") -> ?Q("{_@Bar, _@Baz}")
                       end
                   end)),
    ok.

%% utilities

f(Ts) when is_list(Ts) ->
    lists:flatmap(fun erl_prettypr:format/1, Ts);
f(T) ->
    erl_prettypr:format(T).
