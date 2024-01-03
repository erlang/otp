-module(md_to_html_SUITE).

%% callbacks
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

%% test format
-export([convert_erlang_html/1, convert_unknown_format/1]).

%% test non-existing moduledoc
-export([non_existing_moduledoc/1,hidden_moduledoc/1,existing_moduledoc/1]).

%% test non-existing docs
-export([non_existing_doc/1, hidden_doc/1, existing_doc/1]).

%% headings
-export([h1_test/1, h2_test/1, h3_test/1, h4_test/1, h5_test/1, h6_test/1]).

%% quotes
-export([single_line_quote_test/1, double_char_for_quote_test/1,
         ignore_three_spaces_before_quote/1, multiple_line_quote_test/1,
         paragraph_in_between_test/1]).

%% paragraph
-export([paragraph_after_heading_test/1, quote_before_and_after_paragraph_test/1]).

%% inline code
-export([single_line_code_test/1, multiple_line_code_test/1, paragraph_between_code_test/1]).

%% br
-export([start_with_br_test/1, multiple_br_followed_by_paragraph_test/1, ending_br_test/1]).

%% Comments
-export([begin_comment_test/1, after_paragraph_comment/1, forget_closing_comment/1 ]).

%% Headings
-export([format_heading_test/1, format_paragraph_test/1, format_multiple_inline/1,
         format_multiple_inline_format_short/1, format_multiple_inline_format_long/1,
         format_multiple_inline_format_mixed/1, unmatched_format_simple/1,
         unmatched_format_with_inline/1, unmatched_complex_format_with_inline/1]).

%% Bullet lists
-export([singleton_bullet_list/1, singleton_bullet_list_followed_new_paragraph/1, singleton_bullet_list_with_format/1,
         singleton_bullet_list_followed_inner_paragraph/1, singleton_bullet_list_followed_inner_paragraph2/1,
         multiline_bullet_indented_list/1, multiline_bullet_indented_list2/1,
         multiline_bullet_list/1, even_nested_bullet_list/1, odd_nested_bullet_list/1,
         complex_nested_bullet_list/1, complex_nested_bullet_list2/1]).

%% Numbered lists
-export([singleton_numbered_list/1, singleton_numbered_list_followed_new_paragraph/1,
         singleton_numbered_list_with_format/1, singleton_numbered_list_followed_inner_paragraph/1,
         singleton_numbered_list_followed_inner_paragraph2/1, multiline_numbered_indented_list/1,
      multiline_numbered_indented_list2/1, multiline_numbered_list/1, even_nested_numbered_list/1,
      odd_nested_numbered_list/1]).

-define(ERLANG_HTML, <<"application/erlang+html">>).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").


-define(EXPECTED_FUN(Expected), {{function, foo, 0}, [], [], Expected, #{}}).

all() ->
    [{group, different_format_generator},
     {group, module_generator},
     {group, doc_generator},
     {group, header_generator},
     {group, quote_generator},
     {group, paragraph_generator},
     {group, code_generator},
     {group, br_generator},
     {group, comment_generator},
     {group, format_generator},
     {group, bullet_list_generator},
     {group, numbered_list_generator}
    ].

groups() ->
    [{different_format_generator, [parallel], different_format_conversion_tests()},
     {module_generator, [sequence], moduledoc_tests()},
     {doc_generator, [parallel], doc_tests()},
     {header_generator, [parallel], header_tests()},
     {quote_generator, [parallel], quote_tests()},
     {paragraph_generator, [parallel], paragraph_tests()},
     {code_generator, [parallel], code_tests()},
     {br_generator, [parallel], br_tests()},
     {comment_generator, [parallel], comment_tests()},
     {format_generator, [parallel], format_tests()},
     {bullet_list_generator, [parallel], bullet_list_tests()},
     {numbered_list_generator, [parallel], numbered_list_tests()}
    ].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

different_format_conversion_tests() ->
    [ convert_erlang_html,
      convert_unknown_format
    ].

moduledoc_tests() ->
    [ non_existing_moduledoc,
      hidden_moduledoc,
      existing_moduledoc
    ].

doc_tests() ->
    [ non_existing_doc,
      hidden_doc,
      existing_doc
    ].

header_tests() ->
    [ h1_test,
      h2_test,
      h3_test,
      h4_test,
      h5_test,
      h6_test
    ].

quote_tests() ->
    [ single_line_quote_test,
      double_char_for_quote_test,
      ignore_three_spaces_before_quote,
      multiple_line_quote_test,
      paragraph_in_between_test
    ].

paragraph_tests() ->
    [ paragraph_after_heading_test,
      quote_before_and_after_paragraph_test
    ].

code_tests() ->
    [ single_line_code_test,
      multiple_line_code_test,
      paragraph_between_code_test
    ].

br_tests() ->
    [ start_with_br_test,
      multiple_br_followed_by_paragraph_test,
      ending_br_test
    ].

comment_tests() ->
    [ begin_comment_test,
      after_paragraph_comment,
      forget_closing_comment
    ].

format_tests() ->
    [ format_heading_test,
      format_paragraph_test,
      format_multiple_inline,
      format_multiple_inline_format_long,
      format_multiple_inline_format_short,
      format_multiple_inline_format_mixed,
      unmatched_format_simple,
      unmatched_format_with_inline,
      unmatched_complex_format_with_inline
    ].

bullet_list_tests() ->
    [ singleton_bullet_list,
      singleton_bullet_list_followed_new_paragraph,
      singleton_bullet_list_with_format,
      singleton_bullet_list_followed_inner_paragraph,
      singleton_bullet_list_followed_inner_paragraph2,
      multiline_bullet_indented_list,
      multiline_bullet_indented_list2,
      multiline_bullet_list,
      even_nested_bullet_list,
      odd_nested_bullet_list,
      complex_nested_bullet_list,
      complex_nested_bullet_list2
    ].

numbered_list_tests() ->
    [ singleton_numbered_list,
      singleton_numbered_list_followed_new_paragraph,
      singleton_numbered_list_with_format,
      singleton_numbered_list_followed_inner_paragraph,
      singleton_numbered_list_followed_inner_paragraph2,
      multiline_numbered_indented_list,
      multiline_numbered_indented_list2,
      multiline_numbered_list,
      even_nested_numbered_list,
      odd_nested_numbered_list
    ].

convert_erlang_html(_Conf) ->
    Doc = #{<<"en">> => [{p, [], [<<"Test">>]}]},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}}],

    Docs = create_eep48(erlang, ?ERLANG_HTML, none, #{}, Functions),
    Docs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(Docs),
    ok.

convert_unknown_format(_Conf) ->
    Doc = #{<<"en">> => <<"<text>Here</text>">>},
    Functions = create_fun(Doc),

    Docs = create_eep48(erlang, <<"xml">>, Doc, #{},Functions),
    Docs = beam_doc:markdown_to_shelldoc(Docs),
    ok.

non_existing_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, none, #{}, []),
    _ = compile(Docs),
    ok.

hidden_moduledoc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, hidden, #{}, []),
    _ = compile(Docs),
    ok.

existing_moduledoc(_Conf) ->
    Docs = create_eep48_doc(<<"# Here">>),
    HtmlDocs = compile(Docs),
    #{<<"en">> := HtmlModDoc} = extract_moduledoc(HtmlDocs),
    H1 = header(1, <<"Here">>),
    [H1] = HtmlModDoc,
    ok.

non_existing_doc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, none, #{}, create_fun(none)),
    HtmlDocs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(HtmlDocs),
    ok.

hidden_doc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, none, #{}, create_fun(hidden)),
    HtmlDocs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(HtmlDocs),
    ok.

existing_doc(_Conf) ->
    Docs = create_eep48(erlang, <<"text/markdown">>, none, #{},
                        create_fun(#{<<"en">> => <<"Test">>})),
    HtmlDocs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(HtmlDocs),
    ok.

h1_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [header(1, <<"Here">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

h2_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n## Header 2">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h1, [], [<<"Here">>]}, {h2, [], [<<"Header 2">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

h3_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n### Header 3">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h1, [], [<<"Here">>]}, {h3, [], [<<"Header 3">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

h4_test(_Conf) ->
    Docs = create_eep48_doc(<<"### Here\n#### Header 4">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> => [{h3, [], [<<"Here">>]}, {h4, [], [<<"Header 4">>]}]},
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

h5_test(_Conf) ->
    Doc = #{<<"en">> => <<"### Here\n#### Header 4\n##### H5">>},
    DocH5 = #{<<"en">> => <<"##### H5">>},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH5, #{}}],
    Docs = create_eep48(erlang, <<"text/markdown">>, Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{<<"en">> => [ {h3, [], [<<"Here">>]},
                                 {h4, [], [<<"Header 4">>]},
                                 {h5, [], [<<"H5">>]}]},
    ExpectedH5 = #{<<"en">> => [ {h5, [], [<<"H5">>]} ]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH5, #{}} = E2,
    ok.

h6_test(_Conf) ->
    Doc = #{<<"en">> => <<"### Here\n#### Header 4\n##### H5">>},
    DocH6 = #{<<"en">> => <<"###### H6\n## H2">>},
    Functions = [{{function, foo, 0}, [], [], Doc, #{}},
                 {{function, bar, 0}, [], [], DocH6, #{}}],
    Docs = create_eep48(erlang, <<"text/markdown">>, Doc, #{}, Functions),

    HtmlDocs = compile(Docs),
    ExpectedH3 = #{<<"en">> => [ {h3, [], [<<"Here">>]},
                                 {h4, [], [<<"Header 4">>]},
                                 {h5, [], [<<"H5">>]}]},
    ExpectedH6 = #{<<"en">> => [ {h6, [], [<<"H6">>]},
                                 {h2, [], [<<"H2">>]} ]},
    ExpectedH3 = extract_moduledoc(HtmlDocs),
    [ E1, E2 ] = extract_doc(HtmlDocs),
    {{function, foo, 0}, [], [], ExpectedH3, #{}} = E1,
    {{function, bar, 0}, [], [], ExpectedH6, #{}} = E2,
    ok.


single_line_quote_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n> This is a quote">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ header(1, <<"Here">>),
                          quote(<<"This is a quote">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

double_char_for_quote_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n>> This is a quote">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ header(1, <<"Here">>),
                          quote(<<"\n">>),
                          quote(<<"\n">>),
                          quote(<<"This is a quote">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

ignore_three_spaces_before_quote(_Conf) ->
    Docs = create_eep48_doc(<<"   > # Here">>),
    HtmlDocs = compile(Docs),

    Expected = expected(quote(<<"# Here">>)),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiple_line_quote_test(_Conf) ->
    Docs = create_eep48_doc(<<"> # Here\n> This is a quote">>),
    HtmlDocs = compile(Docs),

    Expected = expected(quote(<<"# Here\nThis is a quote">>)),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

paragraph_in_between_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Header 1\nThis is text\n> A quote\n## Header 2\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ header(1, <<"Header 1">>),
                          p(<<"This is text">>),
                          quote(<<"A quote">>),
                          header(2, <<"Header 2">>),
                          p(<<"Body content">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

paragraph_after_heading_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Header 1\nThis is text\n\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = #{<<"en">> =>
                     [ header(1, <<"Header 1">>),
                       p(<<"This is text">>),
                       br(),
                       p(<<"Body content">>)]},
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

quote_before_and_after_paragraph_test(_Conf) ->
    Docs = create_eep48_doc(<<"> Quote 1\nThis is text\n> Quote 2\nBody content">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ quote(<<"Quote 1">>),
                          p(<<"This is text">>),
                          quote(<<"Quote 2">>),
                          p(<<"Body content">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

single_line_code_test(_Conf) ->
    Docs = create_eep48_doc(<<"# Here\n    This is code">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ header(1, <<"Here">>),
                          code(<<"This is code">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiple_line_code_test(_Conf) ->
    Docs = create_eep48_doc(<<"    # Here\n    This is code\n        Nested Line">>),
    HtmlDocs = compile(Docs),

    Expected = expected(code(<<"# Here\nThis is code\n    Nested Line">>)),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

paragraph_between_code_test(_Conf) ->
    Docs = create_eep48_doc(<<"This is a paragraph\n",
                              "\n",
                              "    # Here\n",
                              "    This is code\n",
                              "        Nested Line\n",
                              "Another paragraph">>),
    HtmlDocs = compile(Docs),

    Expected = expected([ p(<<"This is a paragraph">>),
                          br(),
                          code(<<"# Here\nThis is code\n    Nested Line">>),
                          p(<<"Another paragraph">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

start_with_br_test(_Conf) ->
    Docs = create_eep48_doc(<<"\n\nAnother paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ br(),
                          br(),
                          p(<<"Another paragraph">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiple_br_followed_by_paragraph_test(_Conf) ->
    Docs = create_eep48_doc(<<"\nAnother paragraph\n\nAnother paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ br(),
                          p(<<"Another paragraph">>),
                          br(),
                          p(<<"Another paragraph">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

ending_br_test(_Conf) ->
    Docs = create_eep48_doc(<<"Test\n">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p(<<"Test">>), br() ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

begin_comment_test(_Conf) ->
    Docs = create_eep48_doc(<<"<!-- Ignore -->Test">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p(<<"Test">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

after_paragraph_comment(_Conf) ->
    Docs = create_eep48_doc(<<"Test\n<!-- Ignore -->Test">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p(<<"Test">>), p(<<"Test">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

forget_closing_comment(_Conf) ->
    Docs = create_eep48_doc(<<"Test\n<!-- Ignore Test">>),
    {'EXIT', {missing_close_comment, _}} = catch compile(Docs),
    ok.

format_heading_test(_Conf) ->
    Docs = create_eep48_doc(<<"# **H1**\n## _H2_\n### `H3`">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ header(1, em(<<"H1">>)),
                          header(2, it(<<"H2">>)),
                          header(3, inline_code(<<"H3">>))
                        ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

format_paragraph_test(_Conf) ->
    Docs = create_eep48_doc(<<"**H1** *HH* _H2_ __H3__ `code`">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ em(<<"H1">>),
                              <<" ">>,
                              it(<<"HH">>),
                              <<" ">>,
                              it(<<"H2">>),
                              <<" ">>,
                              em(<<"H3">>),
                              <<" ">>,
                              inline_code(<<"code">>)
                            ])
                        ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

format_multiple_inline(_Conf) ->
    Docs = create_eep48_doc(<<"**H1 *HH***">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ em([<<"H1 ">>, it(<<"HH">>)])]) ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

format_multiple_inline_format_long(_Conf) ->
    Docs = create_eep48_doc(<<"**H1 __HH__**">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ em([<<"H1 ">>, em(<<"HH">>)])]) ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

format_multiple_inline_format_short(_Conf) ->
    Docs = create_eep48_doc(<<"_H1 *HH*_">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ it([<<"H1 ">>, it(<<"HH">>)])]) ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

format_multiple_inline_format_mixed(_Conf) ->
    Docs = create_eep48_doc(<<"__H1 *HH* _test_ **test2**__ _there_">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ em([<<"H1 ">>, it(<<"HH">>), <<" ">>, it(<<"test">>), <<" ">>, em(<<"test2">>)]),
                              <<" ">>,
                              it(<<"there">>)])
                        ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

unmatched_format_simple(_Conf) ->
    Docs = create_eep48_doc(<<"**Bold*">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([<<"**Bold*">> ])]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs).

unmatched_format_with_inline(_Conf) ->
    Docs = create_eep48_doc(<<"**Bold *Italics*">>),
    HtmlDocs = compile(Docs),
    Expected = expected(p([<<"**Bold ">>, it(<<"Italics">>) ])),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs).

unmatched_complex_format_with_inline(_Conf) ->
    Docs = create_eep48_doc(<<"__H1 *HH* _test_ **test2**_ _there_">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ p([ <<"__H1 ">>, it(<<"HH">>), <<" ">>, it(<<"test">>), <<" ">>,
                              em(<<"test2">>),
                              it(<<" ">>),
                              <<"there_">>])
                        ]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_bullet_list(_Config) ->
    Docs = create_eep48_doc(<<"* One liner">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p(<<"One liner">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_bullet_list_followed_new_paragraph(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n\nThis is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p(<<"One liner">>))]), br(), br(), p(<<"This is a new paragraph">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_bullet_list_followed_inner_paragraph(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n  This is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li([p(<<"One liner">>), p(<<"This is a new paragraph">>)])]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_bullet_list_followed_inner_paragraph2(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\nThis is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li([p(<<"One liner">>), p(<<"This is a new paragraph">>)])]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.


singleton_bullet_list_with_format(_Config) ->
    Docs = create_eep48_doc(<<"* *One* __liner__">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p([it(<<"One">>), <<" ">>,  em(<<"liner">>)]))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_bullet_list(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n* Second line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p(<<"One liner">>)), li(p(<<"Second line">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_bullet_indented_list(_Config) ->
    Docs = create_eep48_doc(
             <<"  * One liner\n  * Second line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p(<<"One liner">>)), li(p(<<"Second line">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_bullet_indented_list2(_Config) ->
    Docs = create_eep48_doc(
             <<"  * _One liner_\n  * _Second_ `line`">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([li(p(it(<<"One liner">>))),
                             li(p([it(<<"Second">>), <<" ">>,  inline_code(<<"line">>)]))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

even_nested_bullet_list(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n  * First nested line\n  * Second nested line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([
                             li([ p(<<"One liner">>),
                                  ul([ li(p(<<"First nested line">>)),
                                       li(p(<<"Second nested line">>))
                                     ])
                                ])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

odd_nested_bullet_list(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n  * First nested line\n  * Second nested line\n  * Third nested line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([
                             li([ p(<<"One liner">>),
                                  ul([ li(p(<<"First nested line">>)),
                                       li(p(<<"Second nested line">>)),
                                       li(p(<<"Third nested line">>))
                                     ])
                                ])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

complex_nested_bullet_list(_Config) ->
    Docs = create_eep48_doc(<<"* One liner\n  * First nested line\n* Second line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ul([
                             li([ p(<<"One liner">>),
                                  ul([ li(p(<<"First nested line">>)) ])
                                ]),
                             li([p(<<"Second line">>)])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

complex_nested_bullet_list2(_Config) ->
    Docs = create_eep48_doc(<<"
* One liner
  * First nested line
    * Second level nested line
  * Second nested line
    * Another nested line
* Second one liner">>),
    HtmlDocs = compile(Docs),
    Expected = expected([br(),
                         ul([
                             li([ p(<<"One liner">>),
                                  ul([ li([
                                           p(<<"First nested line">>),
                                           ul([ li([p(<<"Second level nested line">>)])])
                                          ]),
                                       li([
                                           p(<<"Second nested line">>),
                                           ul([ li([p(<<"Another nested line">>)])])
                                          ])
                                     ])
                                ]),
                             li([ p(<<"Second one liner">>)])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.


singleton_numbered_list(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p(<<"One liner">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.


singleton_numbered_list_followed_new_paragraph(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\n\nThis is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p(<<"One liner">>))]), br(), br(), p(<<"This is a new paragraph">>)]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_numbered_list_followed_inner_paragraph(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\n  This is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li([p(<<"One liner">>), p(<<"This is a new paragraph">>)])]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

singleton_numbered_list_followed_inner_paragraph2(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\nThis is a new paragraph">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li([p(<<"One liner">>), p(<<"This is a new paragraph">>)])]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.


singleton_numbered_list_with_format(_Config) ->
    Docs = create_eep48_doc(<<"1. *One* __liner__">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p([it(<<"One">>), <<" ">>,  em(<<"liner">>)]))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_numbered_indented_list(_Config) ->
    Docs = create_eep48_doc(<<"  1. One liner\n  2. Second line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p(<<"One liner">>)), li(p(<<"Second line">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_numbered_indented_list2(_Config) ->
    Docs = create_eep48_doc(
             <<"  1. _One liner_\n  2. _Second_ `line`">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p(it(<<"One liner">>))),
                             li(p([it(<<"Second">>), <<" ">>,  inline_code(<<"line">>)]))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

multiline_numbered_list(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\n2. Second line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([li(p(<<"One liner">>)), li(p(<<"Second line">>))]), br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

even_nested_numbered_list(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\n  1. First nested line\n  2. Second nested line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([
                             li([ p(<<"One liner">>),
                                  ol([ li(p(<<"First nested line">>)),
                                       li(p(<<"Second nested line">>))
                                     ])
                                ])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.

odd_nested_numbered_list(_Config) ->
    Docs = create_eep48_doc(<<"1. One liner\n  1. First nested line\n  2. Second nested line\n  3. Third nested line">>),
    HtmlDocs = compile(Docs),
    Expected = expected([ol([
                             li([ p(<<"One liner">>),
                                  ol([ li(p(<<"First nested line">>)),
                                       li(p(<<"Second nested line">>)),
                                       li(p(<<"Third nested line">>))
                                     ])
                                ])
                            ]),
                         br()]),
    Expected = extract_moduledoc(HtmlDocs),
    [ ?EXPECTED_FUN(Expected) ] = extract_doc(HtmlDocs),
    ok.


header(Level, Text) when is_integer(Level) ->
    HeadingLevel = integer_to_list(Level),
    HeadingLevelAtom = list_to_existing_atom("h" ++ HeadingLevel),
    {HeadingLevelAtom, [], [Text]}.

quote(X) ->
    {pre,[],[{code,[],[X]}]}.

code(X) ->
    {pre,[],[inline_code(X)]}.

inline_code(X) ->
    {code,[],[X]}.

p(X) when is_list(X) ->
    {p, [], X};
p(X) when is_tuple(X); is_binary(X) ->
    p([X]).

em(X) when is_list(X) ->
    {em, [], X};
em(X) when is_binary(X); is_tuple(X) ->
    em([X]).

it(X) when is_list(X) ->
    {i, [], X};
it(X) when is_binary(X); is_tuple(X) ->
    it([X]).

br() ->
    {br, [], []}.

ul(Items) when is_list(Items) ->
    {ul, [], Items}.

ol(Items) when is_list(Items) ->
    {ol, [], Items}.

li(Item) when is_tuple(Item); is_binary(Item) ->
    li([Item]);
li(Items) when is_list(Items) ->
    {li, [], Items}.

-spec create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) -> #docs_v1{} when
      Language  :: atom(),
      Mime      :: binary(),
      ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
      Metadata  :: map(),
      Docs      :: [{{Kind, Name, Arity},
                     Anno :: erl_anno:anno(),
                     Signature :: [binary()],
                     Doc :: #{DocLanguage := DocValue} | none | hidden,
                     Metadata :: map()
                    }],
      Kind      :: function | type | callback,
      Name      :: atom(),
      Arity     :: non_neg_integer(),
      DocLanguage :: binary(),
      DocValue :: binary() | term().
create_eep48(Language, Mime, ModuleDoc, Metadata, Docs) ->
    #docs_v1{anno = [],
             beam_language = Language,
             format = Mime,
             module_doc = ModuleDoc,
             metadata = maps:merge(#{ otp_doc_vsn => ?CURR_DOC_VERSION }, Metadata),
             docs = Docs}.

extract_moduledoc(Docs) ->
    Docs#docs_v1.module_doc.

extract_doc(Docs) ->
    Docs#docs_v1.docs.

extract_format(Docs) ->
    Docs#docs_v1.format.

create_eep48_doc(Doc) when is_binary(Doc) ->
    create_eep48_doc(Doc, <<"text/markdown">>).

create_eep48_doc(Doc, Format) when is_binary(Doc) ->
    Docs = #{<<"en">> => Doc},
    create_eep48(erlang, Format, Docs, #{}, create_fun(Docs)).

create_fun(Docs) ->
    [{{function, foo, 0}, [], [], Docs, #{}}].


expected(X) when is_list(X)->
    #{<<"en">> => X};
expected(X) when is_tuple(X) ->
    expected([X]).

compile(Docs) ->
    HtmlDocs = beam_doc:markdown_to_shelldoc(Docs),
    ok = shell_docs:validate(HtmlDocs),
    ?NATIVE_FORMAT = extract_format(HtmlDocs),
    HtmlDocs.
