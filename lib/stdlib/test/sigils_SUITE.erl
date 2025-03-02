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

-export([compiled_sigils/1, scan_sigils/1, parse_sigils/1, parse_f_sigil/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [compiled_sigils, scan_sigils, parse_sigils, parse_f_sigil].

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
    <<"<{[(/`#'|\"\\)]}>»"/utf8>> =
        ~b"""
        <{[(/`#'|\"\\)]}>»
        """ =
        ~"""
        <{[(/`#'|"\)]}>»
        """ =
        ~B"""
        <{[(/`#'|"\)]}>»
        """,
    <<"<{[(/`#'|\"\\)]}»"/utf8>> = % >
        ~b<<{[(/`#'|"\\)]}»> =
        ~B<<{[(/`#'|"\)]}»>,
    <<"<{[(/`#'|\"\\)]>»"/utf8>> = % }
        ~b{<{[(/`#'|"\\)]>»} =
        ~B{<{[(/`#'|"\)]>»},
    <<"<{[(/`#'|\"\\)}>»"/utf8>> = % ]
        ~b[<{[(/`#'|"\\)}>»] =
        ~B[<{[(/`#'|"\)}>»],
    <<"<{[(/`#'|\"\\]}>»"/utf8>> = % )
        ~b(<{[(/`#'|"\\]}>») =
        ~B(<{[(/`#'|"\]}>»),
    <<"<{[(`#'|\"\\)]}>»"/utf8>> = % /
        ~b/<{[(`#'|"\\)]}>»/ =
        ~B/<{[(`#'|"\)]}>»/,
    <<"<{[(/#'|\"\\)]}>"/utf8>> = % `
        ~b`<{[(/#'|"\\)]}>` =
        ~B`<{[(/#'|"\)]}>`,
    <<"<{[(/`|'\"\\)]}>»"/utf8>> = % #
        ~b#<{[(/`|'"\\)]}>»# =
        ~B#<{[(/`|'"\)]}>»#,
    <<"<{[(/#|\"\\)]}>»"/utf8>> = % '
        ~b'<{[(/#|"\\)]}>»' =
        ~B'<{[(/#|"\)]}>»',
    <<"<{[(/#'\"\\)]}>»"/utf8>> = % |
        ~b|<{[(/#'"\\)]}>»| =
        ~B|<{[(/#'"\)]}>»|,
    <<"<{[(/#'|\\)]}>»"/utf8>> = % "
        ~b"<{[(/#'|\\)]}>»" =
        ~B"<{[(/#'|\)]}>»",
    %%
    "<{[(/`#'|\"\\)]}>»" =
        ~s"""
        <{[(/`#'|\"\\)]}>»
        """ =
        ~S"""
        <{[(/`#'|"\)]}>»
        """,
    "<{[(/`#'|\"\\)]}»" = % >
        ~s<<{[(/`#'|"\\)]}»> =
        ~S<<{[(/`#'|"\)]}»>,
    "<{[(/`#'|\"\\)]>»" = % }
        ~s{<{[(/`#'|"\\)]>»} =
        ~S{<{[(/`#'|"\)]>»},
    "<{[(/`#'|\"\\)}>»" = % ]
        ~s[<{[(/`#'|"\\)}>»] =
        ~S[<{[(/`#'|"\)}>»],
    "<{[(/`#'|\"\\]}>»" = % )
        ~s(<{[(/`#'|"\\]}>») =
        ~S(<{[(/`#'|"\]}>»),
    "<{[(`#'|\"\\)]}>»" = % /
        ~s/<{[(`#'|"\\)]}>»/ =
        ~S/<{[(`#'|"\)]}>»/,
    "<{[(/#'|\"\\)]}>" = % `
        ~s`<{[(/#'|"\\)]}>` =
        ~S`<{[(/#'|"\)]}>`,
    "<{[(/`|'\"\\)]}>»" = % #
        ~s#<{[(/`|'"\\)]}>»# =
        ~S#<{[(/`|'"\)]}>»#,
    "<{[(/#|\"\\)]}>»" = % '
        ~s'<{[(/#|"\\)]}>»' =
        ~S'<{[(/#|"\)]}>»',
    "<{[(/#'\"\\)]}>»" = % |
        ~s|<{[(/#'"\\)]}>»| =
        ~S|<{[(/#'"\)]}>»|,
    "<{[(/#'|\\)]}>»" = % "
        ~s"<{[(/#'|\\)]}>»" =
        ~S"<{[(/#'|\)]}>»",
    %%
    Pound = 16#A3,
    Euro = 16#20AC,
    [Pound,Pound,Euro,Euro] = ~s[£\xA3€\x{20AC}],
    [Pound,$\\,$x,$A,$3,Euro,$\\|"x{20AC}"] = ~S[£\xA3€\x{20AC}],
    <<"££€€"/utf8>> = ~b[£\xA3€\x{20AC}],
    <<"£\\xA3€\\x{20AC}"/utf8>> = ~B[£\xA3€\x{20AC}],
    %%
    case "abc" of
        ~s"abc" -> ok
    end,
    (fun (~B"abc") -> ok end)(<<"abc"/utf8>>),
    ok.

%% #########################################################################

scan_sigils(Config) when is_list(Config) ->
    [begin
        {S,_,_,What,Head} = Spec,
         P1 = pos(element(2, Spec)),
         P2 = pos(element(3, Spec)),
         {_, {error,{P1,erl_scan,{unterminated,What,Head}},P2}} =
             {Spec, erl_scan:string(S, {1,1}, [])}
     end ||
        Spec <-
            [{"\"",2,2,string,""},
             {"\"x",2,3,string,"x"},
             {"\"\"\"",4,4,{string,3},""},
             {"\"\"\"\n",4,{2,1},{string,3},"\n"},
             {"~(",3,3,{sigil,'',$(,$)},""},
             {"~[",3,3,{sigil,'',$[,$]},""},
             {"~{",3,3,{sigil,'',${,$}},""},
             {"~<",3,3,{sigil,'',$<,$>},""},
             {"~`",3,3,{sigil,'',$`,$`},""},
             {"~\"\"\"\"",6,6,{sigil,'',4},""},
             {"~\"\"\"\"\n",6,{2,1},{sigil,'',4},"\n"},
             {"~s/",4,4,{sigil,'s',$/,$/},""},
             {"~s|x",4,5,{sigil,'s',$|,$|},"x"},
             {"~s\"\"\"",6,6,{sigil,'s',3},""},
             {"~s\"\"\"\n",6,{2,1},{sigil,'s',3},"\n"},
             {"~S'",4,4,{sigil,'S',$',$'},""},
             {"~S#x",4,5,{sigil,'S',$#,$#},"x"},
             {"~S\"\"\"",6,6,{sigil,'S',3},""},
             {"~S\"\"\"\nxx",6,{2,3},{sigil,'S',3},"\nxx"}]],
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
    IllegalPrefix = "illegal sigil prefix",
    IllegalSuffix = "illegal sigil suffix",
    AllSigils = [" ~","~s","~S","~b","~B","~f","~fs","~fS","~fb","~fB","~sf","~Sf","~bf","~Bf"],
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
    [{_, {error,_,erl_parse,IllegalSuffix,_}} =
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
    [{_, {error,{_,erl_scan,string_concat},_}} =
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

parse_f_sigil(Config) when is_list(Config) ->
    {ok,
     [{match,
       {1,1},
       {var,{1,1},'OO'},
       {bin,
        {1,6},
        [{bin_element,
          {1,7},
          {string,{1,7},"oo"},
          default,
          [utf8]}]}},
      {match,
       {2,1},
       {var,{2,1},'Baz'},
       {bin,
        {2,7},
        [{bin_element,
          {2,8},
          {string,{2,8},"baz"},
          default,
          [utf8]}]}},
      {bin,
       {3,1},
       [{bin_element,
         [{text,"f"},{location,{3,1}}],
         {string,[{text,"f"},{location,{3,1}}],"f"},
         default,
         [utf8]},
        {bin_element,
         {3,2},
         {block,{3,2},[{var,1,'OO'}]},
         default,
         [binary]},
        {bin_element,
         [{text,"b"},{location,{3,6}}],
         {string,[{text,"b"},{location,{3,6}}],"b"},
         default,
         [utf8]},
        {bin_element,
         {3,7},
         {block,
          {3,7},
          [{match,1,
            {var,1,'A'},
            {bin,1,
             [{bin_element,1,{string,1,"a"},default,[utf8]}]}},
           {bin,1,
            [{bin_element,
              {1,1},
              {block,{1,1},[{var,1,'A'}]},
              default,
              [binary]},
             {bin_element,
              [{text,"r"},{location,{1,4}}],
              {string,[{text,"r"},{location,{1,4}}],"r"},
              default,
              [utf8]},
             {bin_element,
              {1,5},
              {block,{1,5},[{var,1,'Baz'}]},
              default,
              [binary]}]}]},
         default,
         [binary]},
        {bin_element,
         [{text,"{qux}{{quux"},{location,{3,27}}],
         {string,
          [{text,"{qux}{{quux"},{location,{3,27}}],
          "{qux}{{quux"},
         default,
         [utf8]}]}],
     {3,48}} =
        parse_exprs(
          """
          OO = ~"oo",
          Baz = ~"baz",
          ~f[f{OO}b{A=~"a",~f"{A}r{Baz}"}\{qux}\{\{quux].
          """),
    {error,{1,6},erl_parse,
     "Unterminated interpolation expression in ~f string. Expected '}'.",{1,11}} =
        parse_exprs("""
        ~f"error{"
        """),
    InvalidValues = [~"1", ~"{}", ~"[]", ~"#{}", ~"$x", ~"fun() -> foo end", ~"self()",
                     ~[erlang:list_to_port("#Port<0.486>")], ~"erlang:make_ref()"],
    [{error,_,erl_parse,"syntax error before: ",_} =
        parse_exprs(binary_to_list(~f"""
        {~f"{X}"}
        """)) || X <- InvalidValues],
    %%
    ExpectedValues = [
        {"~f", ~"foobar"},
        {"~fs", [~"foo", ~"bar"]},
        {"~fS", [~"foo", ~"bar"]},
        {"~fb", ~"foobar"},
        {"~fB", ~"foobar"},
        {"~sf", [~"foo", ~"bar"]},
        {"~Sf", [~"foo", ~"bar"]},
        {"~bf", ~"foobar"},
        {"~Bf", ~"foobar"}],
    [Value = eval_exprs(Sigil ++ "[foo{~\"bar\"}].", []) || {Sigil, Value} <- ExpectedValues].

parse_exprs(String) ->
    {ok,Tokens,EndPos} = erl_scan:string(String, {1,1}, []),
    case erl_parse:parse_exprs(Tokens) of
        {ok, Parsed} ->
            {ok, Parsed, EndPos};
        {error, {Pos,Mod,Str}} ->
            {error, Pos, Mod, lists:flatten(Str), EndPos}
    end.

eval_exprs(String, Bindings) ->
    {ok, Parsed, _EndPos} = parse_exprs(String),
    {value, Value, _} = erl_eval:exprs(Parsed, Bindings),
    Value.
