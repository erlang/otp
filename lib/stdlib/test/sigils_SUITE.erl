%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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

-module(sigils_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2,end_per_group/2]).

-export([compiled_sigils/1, scan_sigils/1, parse_sigils/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [compiled_sigils, scan_sigils, parse_sigils].

init_per_testcase(_Case, Config) ->
    Config.
%%
end_per_testcase(_Case, _Config) ->
    ok.

groups() -> [].
%%
init_per_group(_GroupName, Config) ->
    Config.
%%
end_per_group(_GroupName, Config) ->
    Config.


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,30}}].
%%
init_per_suite(Config) ->
    Config.
%%
end_per_suite(_Config) ->
    ok.

%% #########################################################################

compiled_sigils(Config) when is_list(Config) ->
    QB = <<"ab\"c\"\d"/utf8>>, % Quoted Binary
    QB = ~"ab\"c\"\d",
    QB = ~b"ab\"c\"\d",
    QB = ~b"""
          ab"c"\d
          """,
    VB = <<"ab\"c\"\\d"/utf8>>, % Verbatim Binary
    VB = ~B"ab\"c\"\d",
    VB = ~"""
          ab"c"\d
          """,
    VB = ~B"""
          ab"c"\d
          """,
    %%
    QS = "ab\"c\"\d", % Quoted String
    QS = ~s"ab\"c\"\d",
    QS = ~s"""
          ab"c"\d
          """,
    VS = "ab\"c\"\\d", % Verbatim String
    VS = ~S"ab\"c\"\d",
    VS = ~S""""
          ab"c"\d
          """",
    %%
    case "abc" of
        ~s"abc" -> ok
    end,
    (fun (~B"abc") -> ok end)(<<"abc"/utf8>>),
    ok.

%% #########################################################################

scan_sigils(Config) when is_list(Config) ->
    [begin
        {S,_,_,T} = Spec,
         P1 = pos(element(2, Spec)),
         P2 = pos(element(3, Spec)),
         {_, {error,{P1,erl_scan,{string,T,_}},P2}} =
             {Spec, erl_scan:string(S, {1,1}, [])}
     end ||
        Spec <-
            [{"\"",1,2,$"},
             {"\"x",1,3,$"},
             {"\"\"\"",1,4,{$",3}},
             {"\"\"\"\n",1,{2,1},{$",3}},
             {"~\"",2,3,$"},
             {"~\"x",2,4,$"},
             {"~\"\"\"\"",2,6,{$",4}},
             {"~\"\"\"\"\n",2,{2,1},{$",4}},
             {"~s\"",3,4,$"},
             {"~s\"x",3,5,$"},
             {"~s\"\"\"",3,6,{$",3}},
             {"~s\"\"\"\n",3,{2,1},{$",3}},
             {"~S\"",3,4,$"},
             {"~S\"x",3,5,$"},
             {"~S\"\"\"",3,6,{$",3}},
             {"~S\"\"\"\nxx",3,{2,3},{$",3}}]],
    [begin
        {S,_,_} = Spec,
         P1 = pos(element(2, Spec)),
         P2 = pos(element(3, Spec)),
         {_, {error,{P1,erl_scan,{illegal,string}},P2}} =
             {Spec, erl_scan:string(S, {1,1}, [])}
     end ||
        Spec <-
            [{"~",2,2}, {"~ ",2,2}, {"~_",3,3}, {"~_ ",3,3}]],
    %%
    AtomSize = 255,
    IllegalAtomString = lists:duplicate(AtomSize+1, $a),
    EndPos1 = 2+AtomSize+1,
    {error,{{1,1},erl_scan,{illegal,sigil_prefix}},{1,EndPos1}} =
        erl_scan:string("~"++IllegalAtomString, {1,1}, []),
    EndPos2 = 4+AtomSize+1,
    {error,{{1,4},erl_scan,{illegal,sigil_suffix}},{1,EndPos2}} =
        erl_scan:string("~\"\""++IllegalAtomString, {1,1}, []),
    ok.


pos({R,C} = Pos) when is_integer(R), is_integer(C) -> Pos;
pos(C) when is_integer(C) -> {1,C}.

%% #########################################################################

parse_sigils(Config) when is_list(Config) ->
    {ok, <<"ab\"c\d"/utf8>>, {1,12}} =
        parse_term(
          """
          ~"ab\"c\d".
          """),
    %%
    IllegalPrefix = "illegal sigil prefix.",
    IllegalSuffix = "illegal sigil suffix.",
    AllSigils = [" ~","~s","~S","~b","~B"],
    [{_, {error,{1,1},erl_parse,IllegalPrefix,{2,5}}} =
         {String, parse_term("~_"++String)}
     || String <-
            [""""
             "
                "
             """",
             """"
             """
              """
             """"]],
    %%
    [{_, {error,{1,8},erl_parse,IllegalSuffix,{1,9}}} =
         {Prefix, parse_term(Prefix++"\"abc\"x")}
     || Prefix <- AllSigils],
    %%
    [{_, {error,{3,6},erl_parse,IllegalSuffix,{3,7}}} =
         {Prefix,
          parse_term(
            Prefix++""""
                """
                  abc
                  """x
                """")}
     || Prefix <- AllSigils],
    %%
    [{_, {error,{3,7},erl_parse,"syntax error before: \"\"",{3,9}}} =
         {Prefix,
          parse_term(
            Prefix++""""
                """
                  abc
                  """ ""
                """")}
     || Prefix <- AllSigils],
    %%
    [{_, {error,{{3,6},erl_scan,string_concat},{3,6}}} =
         {Prefix,
          erl_scan:string(
            Prefix++"""""
                """
                  abc
                  """"
                """"", {1,1}, [])}
     || Prefix <- AllSigils],
    [{_, {error,{{1,8},erl_scan,string_concat},{1,8}}} =
         {Prefix,
          erl_scan:string(
            Prefix++"""
                "abc""
                """,
            {1,1}, [])}
     || Prefix <- AllSigils],
    ok.

parse_term(String) ->
    {ok,Tokens,EndPos} = erl_scan:string(String, {1,1}, []),
    case erl_parse:parse_term(Tokens) of
        {ok, Parsed} ->
            {ok, Parsed, EndPos};
        {error, {Pos,Mod,Str}} ->
            {error, Pos, Mod, lists:flatten(Str), EndPos}
    end.
