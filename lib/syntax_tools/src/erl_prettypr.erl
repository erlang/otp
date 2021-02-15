%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 1997-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc Pretty printing of abstract Erlang syntax trees.
%%
%% This module is a front end to the pretty-printing library module
%% `prettypr', for text formatting of abstract syntax trees defined by
%% the module `erl_syntax'.

-module(erl_prettypr).

-export([file/2, format/1, format/2, best/1, best/2, layout/1, layout/2,
         get_ctxt_precedence/1, set_ctxt_precedence/2,
         get_ctxt_paperwidth/1, set_ctxt_paperwidth/2,
         get_ctxt_linewidth/1, set_ctxt_linewidth/2, get_ctxt_hook/1,
         set_ctxt_hook/2, get_ctxt_user/1, set_ctxt_user/2]).

-import(prettypr,
        [above/2,
         beside/2,
         empty/0,
         follow/3,
         nest/2]).

-import(erl_parse,
        [preop_prec/1,
         inop_prec/1,
         func_prec/0,
         max_prec/0,
         type_inop_prec/1,
         type_preop_prec/1]).

-import(erl_syntax,
        [operator_literal/1,
         variable_literal/1,
         integer_literal/1,
         float_literal/1,
         atom_literal/2,
         char_literal/2,
         string_literal/2,
         tuple_elements/1,
         list_prefix/1,
         text_string/1,
         conjunction/1,
         disjunction/1,
         eof_marker/0]).

-define(PADDING, 2).
-define(PAPER, 80).
-define(RIBBON, 80).

-type hook() :: fun((erl_syntax:syntaxTree(), _, _) -> prettypr:document()).
-type clause_t() :: 'case_expr' | 'fun_expr'
                  | 'if_expr' | 'receive_expr' | 'try_expr'
                  | {'function', prettypr:document()}
                  | 'spec'.

-record(ctxt,
        {prec = 0 :: integer(),
         sub_indent = 2 :: non_neg_integer(),
         break_indent = 4 :: non_neg_integer(),
         clause :: 'undefined' | clause_t(),
         hook :: 'undefined' | hook(),
         paper = ?PAPER :: integer(),
         ribbon = ?RIBBON :: integer(),
         user :: term(),
         encoding = epp:default_encoding() :: epp:source_encoding(),
         formatter = fun prettypr:sep/1 :: fun((list()) -> prettypr:document()),
         separator = empty() :: prettypr:document() | list(non_neg_integer()),
         empty_lines = sets:new() :: sets:set(integer())
        }).

-type context() :: #ctxt{}.

%% =====================================================================
%% The following functions examine and modify contexts:

%% @spec (context()) -> integer()
%% @doc Returns the operator precedence field of the prettyprinter
%% context.
%%
%% @see set_ctxt_precedence/2

-spec get_ctxt_precedence(context()) -> integer().

get_ctxt_precedence(Ctxt) ->
    Ctxt#ctxt.prec.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the operator precedence field of the prettyprinter
%% context. See the {@link //stdlib/erl_parse} module for operator precedences.
%%
%% @see //stdlib/erl_parse
%% @see get_ctxt_precedence/1

-spec set_ctxt_precedence(context(), integer()) -> context().

set_ctxt_precedence(Ctxt, Prec) ->
    set_prec(Ctxt, Prec).

set_prec(Ctxt, Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally

reset_prec(Ctxt) ->
    set_prec(Ctxt, 0).    % used internally

set_clause(Ctxt, Clause) ->
    Ctxt#ctxt{clause = Clause}.

reset_clause(Ctxt) ->
    set_clause(Ctxt, undefined).

set_formatter(Ctxt, Type) when Type == form_list ->
    set_formatter(Ctxt, fun vertical_sep/1);
set_formatter(Ctxt, Type) when Type == tuple;
                               Type == binary;
                               Type == infix_expr;
                               Type == catch_expr;
                               Type == match_expr;
                               Type == generator;
                               Type == binary_generator;
                               Type == record_field;
                               Type == record_type_field;
                               Type == typed_record_field;
                               Type == map_field_assoc;
                               Type == map_field_exact;
                               Type == map_type_assoc;
                               Type == map_type_exact;
                               Type == annotated_type;
                               Type == bitstring_type;
                               Type == constraint;
                               Type == integer_range_type;
                               Type == type_union;
                               Type == tuple_type;
                               Type == type_application;
                               Type == function ->
    set_formatter(Ctxt, par);
set_formatter(Ctxt, Type) when Type == list;
                               Type == list_comp;
                               Type == binary_comp;
                               Type == clause;
                               Type == receive_expr;
                               Type == case_expr;
                               Type == try_expr;
                               Type == if_expr;
                               Type == fun_expr;
                               Type == named_fun_expr;
                               Type == implicit_fun;
                               Type == function_type;
                               Type == constrained_function_type ->
    set_formatter(Ctxt, sep);
set_formatter(Ctxt, Type) when Type == binary_field;
                               Type == prefix_expr;
                               Type == class_qualifier;
                               Type == size_qualifier;
                               Type == module_qualifier;
                               Type == arity_qualifier;
                               Type == record_expr;
                               Type == record_access;
                               Type == record_index_expr;
                               Type == record_type;
                               Type == map_expr;
                               Type == map_type;
                               Type == macro;
                               Type == attribute;
                               Type == application;
                               Type == user_type_application ->
    set_formatter(Ctxt, beside);
set_formatter(Ctxt, Type) when Type == block_expr ->
    set_formatter(Ctxt, above);
set_formatter(Ctxt, Formatter) when Formatter == beside;
                                    Formatter == above ->
    set_formatter(Ctxt, fun prettypr:Formatter/2);
set_formatter(Ctxt, Formatter) when Formatter == sep;
                                    Formatter == par ->
    set_formatter(Ctxt, fun prettypr:Formatter/1);
set_formatter(Ctxt, Formatter) when is_function(Formatter, 2) ->
    set_formatter(Ctxt, nosep(Formatter));
set_formatter(Ctxt, Formatter) when is_function(Formatter, 1) ->
    Ctxt#ctxt{formatter=Formatter};
set_formatter(Ctxt, _) ->
    Ctxt.

set_nested(Ctxt) ->
    set_nested(Ctxt, break_indent).

set_nested(Ctxt, sub_indent) ->
    set_nested(Ctxt, Ctxt#ctxt.sub_indent);
set_nested(Ctxt, break_indent) ->
    set_nested(Ctxt, Ctxt#ctxt.break_indent);
set_nested(Ctxt = #ctxt{formatter = Formatter}, Indent) ->
    set_formatter(Ctxt, fun(L) -> nest(Indent, Formatter(L)) end).

set_separator(Ctxt, Char) when is_integer(Char) ->
    set_separator(Ctxt, [Char]);
set_separator(Ctxt, String) when is_list(String) ->
    Ctxt#ctxt{separator = prettypr:text(String)}.

reset_separator(Ctxt) ->
    Ctxt#ctxt{separator = empty()}.

%% @spec (context()) -> integer()
%% @doc Returns the paper widh field of the prettyprinter context.
%% @see set_ctxt_paperwidth/2

-spec get_ctxt_paperwidth(context()) -> integer().

get_ctxt_paperwidth(Ctxt) ->
    Ctxt#ctxt.paper.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the paper widh field of the prettyprinter context.
%%
%% Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.
%%
%% @see get_ctxt_paperwidth/1

-spec set_ctxt_paperwidth(context(), integer()) -> context().

set_ctxt_paperwidth(Ctxt, W) ->
    Ctxt#ctxt{paper = W}.

%% @spec (context()) -> integer()
%% @doc Returns the line widh field of the prettyprinter context.
%% @see set_ctxt_linewidth/2

-spec get_ctxt_linewidth(context()) -> integer().

get_ctxt_linewidth(Ctxt) ->
    Ctxt#ctxt.ribbon.

%% @spec (context(), integer()) -> context()
%%
%% @doc Updates the line widh field of the prettyprinter context.
%%
%% Note: changing this value (and passing the resulting context to a
%% continuation function) does not affect the normal formatting, but may
%% affect user-defined behaviour in hook functions.
%%
%% @see get_ctxt_linewidth/1

-spec set_ctxt_linewidth(context(), integer()) -> context().

set_ctxt_linewidth(Ctxt, W) ->
    Ctxt#ctxt{ribbon = W}.

%% @spec (context()) -> hook()
%% @doc Returns the hook function field of the prettyprinter context.
%% @see set_ctxt_hook/2

-spec get_ctxt_hook(context()) -> hook().

get_ctxt_hook(Ctxt) ->
    Ctxt#ctxt.hook.

%% @spec (context(), hook()) -> context()
%% @doc Updates the hook function field of the prettyprinter context.
%% @see get_ctxt_hook/1

-spec set_ctxt_hook(context(), hook()) -> context().

set_ctxt_hook(Ctxt, Hook) ->
    Ctxt#ctxt{hook = Hook}.

%% @spec (context()) -> term()
%% @doc Returns the user data field of the prettyprinter context.
%% @see set_ctxt_user/2

-spec get_ctxt_user(context()) -> term().

get_ctxt_user(Ctxt) ->
    Ctxt#ctxt.user.

%% @spec (context(), term()) -> context()
%% @doc Updates the user data field of the prettyprinter context.
%% @see get_ctxt_user/1

-spec set_ctxt_user(context(), term()) -> context().

set_ctxt_user(Ctxt, X) ->
    Ctxt#ctxt{user = X}.


file(Name, Opts) ->
    Encoding = [{encoding, Enc} || Enc <- [epp:read_encoding(Name)],
                                   Enc =/= none],
    {Comments, Forms} = parse_forms(Name, Encoding),
    {ok, FD} = file:open(Name, [write | Encoding]),
    try
        FlatForms = erl_syntax:flatten_form_list(erl_syntax:form_list(Forms)),
        CommentedForms = erl_recomment:recomment_forms(FlatForms, Comments),
        io:put_chars(FD, format(CommentedForms, Opts ++ Encoding)),
        io:nl(FD)
    after
        file:close(FD)
    end.

parse_forms(Name, Encoding) ->
    {ok, FD} = file:open(Name, [read | Encoding]),
    try scan_forms(FD, 1, [], [])
    after file:close(FD)
    end.

scan_forms(FD, Line, Comments, Forms) ->
    case io:scan_erl_form(FD, "", Line, [return, text]) of
        {ok, Tokens, NewLine} ->
            {_Ws, NoWs} = lists:partition(fun is_wspace/1, Tokens),
            {LineComents, Ts} = lists:partition(fun is_comment/1, NoWs),
            Form =
            case erl_parse:parse_form(epp_dodger:scan_form(Ts, [])) of
                {error, {_ELine, _, Msg}} ->
                    {_, NoLeadingWs} = lists:splitwith(fun is_wspace/1, Tokens),
                    OrigTxt = string:join(token_texts(NoLeadingWs), ""),
                    error_logger:error_msg("~p~n~p", [OrigTxt, Msg]),
                    erl_syntax:text(OrigTxt);
                {ok, F} ->
                    epp_dodger:rewrite_form(F)
            end,
            scan_forms(FD,
                       NewLine,
                       convert_comments(LineComents, Comments),
                       [Form | Forms]);
        {eof, _} ->
            {lists:reverse(Comments),
             lists:reverse(Forms)};
        eof ->
            {lists:reverse(Comments),
             lists:reverse(Forms)}
    end.

convert_comments([], Acc) -> Acc;
convert_comments([{comment, Data, Orig} | Rest], Acc) ->
    Line =
    case erl_anno:line(Data) of
        undefined -> 0;
        L -> L
    end,
    convert_comments(
      Rest,
      scan_comment_line(
        Line,
        erl_comment_scan:scan_lines(Orig),
        Acc)).

scan_comment_line(_Line, [], Acc) -> Acc;
scan_comment_line(Line, [{Line0, Col, Indent, Text} | Rest], Acc) ->
    scan_comment_line(Line, Rest, [{Line+Line0-1, Col, Indent, Text} | Acc]).

token_texts(Tokens) ->
    [erl_scan:text(T) || T <- Tokens, not is_dot(T)].

is_wspace(T) -> element(1, T) == white_space.
is_comment(T) -> element(1, T) == comment.
is_dot(T) -> element(1, T) == dot.


%% =====================================================================
%% @spec format(Tree::syntaxTree()) -> string()
%% @equiv format(Tree, [])

-spec format(erl_syntax:syntaxTree()) -> string().

format(Node) ->
    format(Node, []).


%% =====================================================================
%% @spec format(Tree::syntaxTree(), Options::[term()]) -> string()
%%
%% @type syntaxTree() = erl_syntax:syntaxTree().
%%
%% An abstract syntax tree. See the {@link erl_syntax} module for
%% details.
%%
%% @type hook() = (syntaxTree(), context(), Continuation) ->
%%                            prettypr:document()
%%	    Continuation = (syntaxTree(), context()) ->
%%                            prettypr:document().
%%
%% A call-back function for user-controlled formatting. See {@link
%% format/2}.
%%
%% @type context(). A representation of the current context of the
%% pretty-printer. Can be accessed in hook functions.
%%
%% @doc Prettyprint-formats an abstract Erlang syntax tree as text. For
%% example, if you have a `.beam' file that has been compiled with
%% `debug_info', the following should print the source code for the
%% module (as it looks in the debug info representation):
%% ```{ok,{_,[{abstract_code,{_,AC}}]}} =
%%            beam_lib:chunks("myfile.beam",[abstract_code]),
%%    io:put_chars(erl_prettypr:format(erl_syntax:form_list(AC)))
%% '''
%%
%% Available options:
%% <dl>
%%   <dt>{hook, none | {@link hook()}}</dt>
%%       <dd>Unless the value is `none', the given function is called
%%       for each node whose list of annotations is not empty; see below
%%       for details. The default value is `none'.</dd>
%%
%%   <dt>{paper, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, including indentation. The default value is 80.</dd>
%%
%%   <dt>{ribbon, integer()}</dt>
%%       <dd>Specifies the preferred maximum number of characters on any
%%       line, not counting indentation. The default value is 65.</dd>
%%
%%   <dt>{user, term()}</dt>
%%       <dd>User-specific data for use in hook functions. The default
%%       value is `undefined'.</dd>
%%   <dt>{encoding, epp:source_encoding()}</dt>
%%       <dd>Specifies the encoding of the generated file.</dd>
%% </dl>
%%
%% A hook function (cf. the {@link hook()} type) is passed the current
%% syntax tree node, the context, and a continuation. The context can be
%% examined and manipulated by functions such as `get_ctxt_user/1' and
%% `set_ctxt_user/2'. The hook must return a "document" data structure
%% (see {@link layout/2} and {@link best/2}); this may be constructed in
%% part or in whole by applying the continuation function. For example,
%% the following is a trivial hook:
%% ```
%%     fun (Node, Ctxt, Cont) -> Cont(Node, Ctxt) end
%% '''
%% which yields the same result as if no hook was given.
%% The following, however:
%% ```
%%     fun (Node, Ctxt, Cont) ->
%%         Doc = Cont(Node, Ctxt),
%%         prettypr:beside(prettypr:text("<b>"),
%%                         prettypr:beside(Doc,
%%                                         prettypr:text("</b>")))
%%     end
%% '''
%% will place the text of any annotated node (regardless of the
%% annotation data) between HTML "boldface begin" and "boldface end"
%% tags.
%%
%% @see erl_syntax
%% @see format/1
%% @see layout/2
%% @see best/2
%% @see get_ctxt_user/1
%% @see set_ctxt_user/2

-spec format(erl_syntax:syntaxTree(), [term()]) -> string().

format(Node, Options) ->
    case best(Node, Options) of
        empty -> throw(no_layout);
        L -> remove_space_parentheses(transform(L))
    end.

remove_space_parentheses(String) ->
    remove_space_parentheses(String,
                             [{"[(]\\s(\\S)", "(\\1"},
                              {"(\\S)\\s[)]", "\\1)"}]).

remove_space_parentheses(String, []) ->
    String;
remove_space_parentheses(String, [{Regex, Replace} = This | Next]) ->
    Opts = [global, {return, list}],
    case re:run(String, Regex, [{capture, none}]) of
        match ->
            New = re:replace(String, Regex, Replace, Opts),
            remove_space_parentheses(New, [This | Next]);
        nomatch ->
            remove_space_parentheses(String, Next)
    end.

transform(L) ->
    lists:reverse(transform(0, L, [])).

transform(N, {above, {text,  _} = T, L}, Cs) ->
    transform(N, L, [$\n | transform(N, T, Cs)]);
transform(N, {nest, N1, L}, Cs) ->
    transform(N + N1, L, Cs);
transform(N, {text, S}, Cs) ->
    lists:reverse(lists:flatten(tl(S)), indent(N, Cs));
transform(_N, null, Cs) ->
    Cs.

indent(N, Cs) when N > 0 ->
    indent(N - 1, [$\s | Cs]);
indent(_N, Cs) ->
    Cs.

%% =====================================================================
%% @spec best(Tree::syntaxTree()) -> empty | prettypr:document()
%% @equiv best(Tree, [])

-spec best(erl_syntax:syntaxTree()) -> 'empty' | prettypr:document().

best(Node) ->
    best(Node, []).


%% =====================================================================
%% @spec best(Tree::syntaxTree(), Options::[term()]) ->
%%           empty | prettypr:document()
%%
%% @doc Creates a fixed "best" abstract layout for a syntax tree. This
%% is similar to the `layout/2' function, except that here, the final
%% layout has been selected with respect to the given options. The atom
%% `empty' is returned if no such layout could be produced. For
%% information on the options, see the `format/2' function.
%%
%% @see best/1
%% @see layout/2
%% @see format/2
%% @see prettypr:best/3

-spec best(erl_syntax:syntaxTree(), [term()]) -> 'empty' | prettypr:document().

best(Node, Options) ->
    Layout =
    case erl_syntax:type(Node) of
        T when T == function;
               T == attribute ->
            layout(erl_syntax:form_list([Node]), Options);
        _ -> layout(Node, Options)
    end,
    W = proplists:get_value(paper, Options, ?PAPER),
    L = proplists:get_value(ribbon, Options, ?RIBBON),
    prettypr:best(Layout, W, L).


%% =====================================================================
%% @spec layout(Tree::syntaxTree()) -> prettypr:document()
%% @equiv layout(Tree, [])

-spec layout(erl_syntax:syntaxTree()) -> prettypr:document().

layout(Node) ->
    layout(Node, []).


%% =====================================================================
%% @spec layout(Tree::syntaxTree(), Options::[term()]) -> prettypr:document()
%%
%% @doc Creates an abstract document layout for a syntax tree. The
%% result represents a set of possible layouts (cf. module `prettypr').
%% For information on the options, see {@link format/2}; note, however,
%% that the `paper' and `ribbon' options are ignored by this function.
%%
%% This function provides a low-level interface to the pretty printer,
%% returning a flexible representation of possible layouts, independent
%% of the paper width eventually to be used for formatting. This can be
%% included as part of another document and/or further processed
%% directly by the functions in the `prettypr' module, or used in a hook
%% function (see `format/2' for details).
%%
%% @see prettypr
%% @see format/2
%% @see layout/1

-spec layout(erl_syntax:syntaxTree(), [term()]) -> prettypr:document().

layout(Node, Options) ->
    layout_hook(Node, to_ctxt(Options)).

to_ctxt(Ctxt) when is_record(Ctxt, ctxt) -> Ctxt;
to_ctxt(Options) when is_list(Options) ->
    Fields = record_info(fields, ctxt),
    Indexes = lists:seq(2, record_info(size, ctxt)),
    to_ctxt(lists:zip(Indexes, Fields), Options, #ctxt{}).

to_ctxt([], _Options, Ctxt) -> Ctxt;
to_ctxt([{Index, Field} | Rest], Options, Ctxt) ->
    to_ctxt(Rest, Options,
            case proplists:get_value(Field, Options) of
                undefined -> Ctxt;
                Value -> erlang:setelement(Index, Ctxt, Value)
            end).

%% This handles annotations and hooks:
layout_hook(Node, Ctxt = #ctxt{hook = Hook}) ->
    case erl_syntax:get_ann(Node) of
        %% Hooks are not called if there are no annotations.
        Ann when Ann == [] orelse Hook == undefined ->
            lay_comments(Node, Ctxt);
        _ -> Hook(Node, Ctxt, fun lay_comments/2)
    end.

%% This handles attached comments:
lay_comments(Node, Ctxt) ->
    case erl_syntax:has_comments(Node) of
        true ->
            Pre = erl_syntax:get_precomments(Node),
            Post = erl_syntax:get_postcomments(Node),
            NoComment = erl_syntax:remove_comments(Node),
            Ctxt1 = set_formatter(reset_separator(Ctxt), above),
            lay_nodes([no_pad(Pre ++ Post), NoComment], Ctxt1);
        false ->
            lay_nodes(Node, Ctxt)
    end.

no_pad(Cs) -> [erl_syntax:comment(0, erl_syntax:comment_text(C)) || C <- Cs].

%% This part ignores annotations and comments:
lay_nodes(Node, _Ctxt) when Node == none; Node == any_size; Node == [] ->
    empty();
lay_nodes(Nodes, Ctxt = #ctxt{formatter = Formatter, separator = Separator})
  when is_list(Nodes) ->
    Formatter(
      lists:filter(
        fun(I) -> I =/= prettypr:text("") andalso I =/= empty() end,
        seq(Nodes,
            Separator,
            Ctxt,
            fun ({N, #ctxt{} = C}, _) -> layout_hook(N, C);
                ({N, P}, C) when is_integer(P) -> layout_hook(N, set_prec(C, P));
                (N, C) -> layout_hook(N, C)
            end)));
lay_nodes(nil, Ctxt) ->
    lay_nodes(erl_syntax:nil(), Ctxt);
lay_nodes(Node, Ctxt) ->
    Type =  erl_syntax:type(Node),
    lay_type(Node, set_formatter(Ctxt, Type), Type).


%% We list literals and other common cases first.
lay_type(Node, _Ctxt, operator) ->
    prettypr:text(operator_literal(Node));
lay_type(Node, _Ctxt, variable) ->
    prettypr:text(variable_literal(Node));
lay_type(Node, _Ctxt, integer) ->
    prettypr:text(integer_literal(Node));
lay_type(Node, _Ctxt, float) ->
    prettypr:text(tidy_float(float_literal(Node)));
lay_type(Node, Ctxt, atom) ->
    prettypr:text(atom_literal(Node, Ctxt#ctxt.encoding));
lay_type(Node, Ctxt, char) ->
    prettypr:text(char_literal(Node, Ctxt#ctxt.encoding));
lay_type(Node, _Ctxt, text) ->
    prettypr:text(text_string(Node));
lay_type(Node, Ctxt, string) ->
    lay_string(string_literal(Node, Ctxt#ctxt.encoding), Ctxt);
lay_type(_Node, _Ctxt, nil) ->
    prettypr:text("[]");
lay_type(_Node, _Ctxt, underscore) ->
    prettypr:text("_");
lay_type(_Node, _Ctxt, fun_type) ->
    prettypr:text("fun()");
lay_type(Node, Ctxt, conjunction) ->
    layout_hook(erl_syntax:conjunction_body(Node), set_separator(Ctxt, $,));
lay_type(Node, Ctxt, disjunction) ->
    layout_hook(erl_syntax:disjunction_body(Node), set_separator(Ctxt, $;));
lay_type(Node, Ctxt, tuple) ->
    curlies(conjunction(tuple_elements(Node)), reset_prec(Ctxt));
lay_type(Node, Ctxt, list) ->
    Node1 = erl_syntax:compact_list(Node),
    Suffix =  erl_syntax:list_suffix(Node1),
    lay_type(Node1, reset_prec(Ctxt), {list, Suffix});
lay_type(Node, Ctxt, {list, none}) ->
    squares(conjunction(list_prefix(Node)), Ctxt);
lay_type(Node, Ctxt, {list, Suffix}) ->
    Body = conjunction(list_prefix(Node)),
    Content = [Body, [erl_syntax:text("|"), Suffix]],
    squares(Content, reset_separator(Ctxt));
lay_type(Node, Ctxt, list_comp) ->
    Ctxt1 = reset_prec(Ctxt),
    Template = erl_syntax:list_comp_template(Node),
    Body = conjunction(erl_syntax:list_comp_body(Node)),
    Content = [Template, [erl_syntax:text("||"), Body]],
    squares(Content, reset_separator(Ctxt1));
lay_type(Node, Ctxt, binary_comp) ->
    Ctxt1 = reset_prec(Ctxt),
    Template = erl_syntax:binary_comp_template(Node),
    Body = conjunction(erl_syntax:binary_comp_body(Node)),
    Content = [Template, [erl_syntax:text("||"), Body]],
    tags(Content, reset_separator(Ctxt1));
lay_type(Node, Ctxt, binary) ->
    tags(conjunction(erl_syntax:binary_fields(Node)), Ctxt);
lay_type(Node, Ctxt, binary_field) ->
    SubType = {binary_field, erl_syntax:binary_field_types(Node)},
    lay_type(Node, Ctxt, SubType);
lay_type(Node, Ctxt, {binary_field, []}) ->
    layout_hook(erl_syntax:binary_field_body(Node), set_prec(Ctxt, max_prec()));
lay_type(Node, Ctxt, {binary_field, BitTypes}) ->
    BTC = set_separator(Ctxt, $-),
    lay_op(erl_syntax:operator('/'),
           erl_syntax:binary_field_body(Node),
           BitTypes,
           fun(Op) -> {_LP, P, RP} = inop_prec(Op), {max_prec(), 0, set_prec(BTC, RP), P} end,
           Ctxt);
lay_type(Node, Ctxt, form_list) ->
    Ctxt1 = reset_prec(set_separator(Ctxt, $.)),
    FlatForm = erl_syntax:flatten_form_list(Node),
    Forms = erl_syntax:form_list_elements(FlatForm),
    ListOfForms = split_form_list(Forms, Ctxt1),
    layout_hook(ListOfForms, Ctxt);
lay_type(Node, Ctxt, parentheses) ->
    parentheses(erl_syntax:parentheses_body(Node), Ctxt);
lay_type(Node, Ctxt, infix_expr) ->
    lay_op(erl_syntax:infix_expr_operator(Node),
           erl_syntax:infix_expr_left(Node),
           erl_syntax:infix_expr_right(Node),
           fun(Op) -> {LP, P, RP} = inop_prec(Op), {LP, 0, RP, P} end,
           Ctxt);
lay_type(Node, Ctxt, prefix_expr) ->
    Operator = erl_syntax:prefix_expr_operator(Node),
    {Prec, RightPrec} = preop_prec(erl_syntax:operator_name(Operator)),
    lay_op([{Operator, 0},
            {erl_syntax:prefix_expr_argument(Node), RightPrec}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, catch_expr) ->
    {Prec, RightPrec} = preop_prec('catch'),
    lay_op([{erl_syntax:operator('catch'), 0},
            {erl_syntax:catch_expr_body(Node), RightPrec}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, match_expr) ->
    lay_op(erl_syntax:operator('='),
           erl_syntax:match_expr_pattern(Node),
           erl_syntax:match_expr_body(Node),
           fun(Op) -> {LP, P, RP} = inop_prec(Op), {LP, 0, RP, P} end,
           Ctxt);
lay_type(Node, Ctxt, class_qualifier) ->
    lay_op(erl_syntax:operator(':'),
           erl_syntax:class_qualifier_argument(Node),
           erl_syntax:class_qualifier_body(Node),
           fun(_) -> M = max_prec(), {M, M, M, 0} end,
           Ctxt);
lay_type(Node, Ctxt, size_qualifier) ->
    lay_op(erl_syntax:operator(':'),
           erl_syntax:size_qualifier_body(Node),
           erl_syntax:size_qualifier_argument(Node),
           fun(_) -> M = max_prec(), {M, M, M, M} end,
           Ctxt); %TODO check this
lay_type(Node, Ctxt, module_qualifier) ->
    lay_op(erl_syntax:operator(':'),
           erl_syntax:module_qualifier_argument(Node),
           erl_syntax:module_qualifier_body(Node),
           fun(Op) -> {LP, P, RP} = inop_prec(Op), {LP, 0, RP, P} end,
          Ctxt);
lay_type(Node, Ctxt, arity_qualifier) ->
    lay_op(erl_syntax:operator('/'),
           erl_syntax:arity_qualifier_body(Node),
           erl_syntax:arity_qualifier_argument(Node),
           fun(Op) -> {LP, P, RP} = inop_prec(Op), {LP, P, RP, max_prec()} end,
           Ctxt);
lay_type(Node, Ctxt, generator) ->
    lay_op(erl_syntax:operator('<-'),
           erl_syntax:generator_pattern(Node),
           erl_syntax:generator_body(Node),
           fun(_Op) -> {0, 0, 0, 0}
           end,
           Ctxt);
lay_type(Node, Ctxt, binary_generator) ->
    lay_op(erl_syntax:operator('<='),
           erl_syntax:binary_generator_pattern(Node),
           erl_syntax:binary_generator_body(Node),
           fun(_Op) -> {0, 0, 0, 0} end,
           Ctxt);
lay_type(Node, Ctxt, application) ->
    lay_call(erl_syntax:application_operator(Node),
             erl_syntax:application_arguments(Node),
             Ctxt);
lay_type(Node, Ctxt, user_type_application) ->
    lay_call(erl_syntax:user_type_application_name(Node),
             erl_syntax:user_type_application_arguments(Node),
             Ctxt);
lay_type(Node, Ctxt, record_access) ->
    {LeftPrec, Prec, RightPrec} = inop_prec('#'),
    lay_op([{erl_syntax:record_access_argument(Node), LeftPrec},
            {erl_syntax:text("#"), max_prec()},
            {erl_syntax:record_access_type(Node), 0},
            {erl_syntax:text("."), max_prec()},
            {erl_syntax:record_access_field(Node), RightPrec}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, record_expr) ->
    {LeftPrec, Prec, _RightPrec} = inop_prec('#'),
    lay_op([{erl_syntax:record_expr_argument(Node), LeftPrec},
            {erl_syntax:text("#"), max_prec()},
            {erl_syntax:record_expr_type(Node), 0},
            {erl_syntax:text("{"), max_prec()},
            {conjunction(erl_syntax:record_expr_fields(Node)),
             set_formatter(reset_prec(Ctxt), sep)},
            {erl_syntax:text("}"), max_prec()}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, record_index_expr) ->
    {Prec, RightPrec} = preop_prec('#'),
    lay_op([{erl_syntax:text("#"), max_prec()},
            {erl_syntax:record_index_expr_type(Node), 0},
            {erl_syntax:text("."), max_prec()},
            {erl_syntax:record_index_expr_field(Node), RightPrec}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, record_field) ->
    lay_type(Node, Ctxt, {record_field, erl_syntax:record_field_value(Node)});
lay_type(Node, Ctxt, {record_field, none}) ->
    layout_hook(erl_syntax:record_field_name(Node), reset_prec(Ctxt));
lay_type(Node, Ctxt, {record_field, Value}) ->
    Name = erl_syntax:record_field_name(Node),
    lay_op(erl_syntax:operator('='),
           erl_syntax:record_field(Name, none),
           Value,
           fun(_) -> {0, 0, 0, 0} end,
           Ctxt);
lay_type(Node, Ctxt, map_expr) ->
    {LeftPrec, Prec, _RightPrec} = inop_prec('#'),
    lay_op([{erl_syntax:map_expr_argument(Node), LeftPrec},
            {erl_syntax:text("#{"), max_prec()},
            {conjunction(erl_syntax:map_expr_fields(Node)),
             set_formatter(reset_prec(Ctxt), sep)},
            {erl_syntax:text("}"), max_prec()}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, map_field_assoc) ->
    Par =  fun(A) -> prettypr:par(A, Ctxt#ctxt.sub_indent) end,
    lay_op(erl_syntax:operator('=>'),
           erl_syntax:map_field_assoc_name(Node),
           erl_syntax:map_field_assoc_value(Node),
           fun(_) -> {0, 0, 0, 0} end,
           set_formatter(Ctxt, Par));
lay_type(Node, Ctxt, map_field_exact) ->
    lay_op(erl_syntax:operator(':='),
           erl_syntax:map_field_exact_name(Node),
           erl_syntax:map_field_exact_value(Node),
           fun(_) -> {0, 0, 0, 0} end,
           Ctxt);
lay_type(Node, Ctxt = #ctxt{clause = {Name, GuardSep}}, clause)
  when Name == none orelse Name == undefined orelse not is_atom(Name) ->
    Ctxt1 = reset_clause(reset_prec(Ctxt)),
    Patterns = erl_syntax:clause_patterns(Node),
    Guard = erl_syntax:clause_guard(Node),
    Body = erl_syntax:clause_body(Node),
    lay_clause(Name, Patterns, GuardSep, Guard, Body, Ctxt1);
lay_type(Node, Ctxt = #ctxt{clause = Clause}, clause) when Clause == if_expr orelse
                                                           Clause == function_type ->
    lay_type(Node, Ctxt#ctxt{clause = {none, none}}, clause);
lay_type(Node, Ctxt = #ctxt{clause = Clause}, clause) when Clause == receive_timeout ->
    lay_type(Node, Ctxt#ctxt{clause = {none, "after"}}, clause);
lay_type(Node, Ctxt = #ctxt{clause = Clause}, clause) when Clause == case_expr orelse
                                                           Clause == receive_expr orelse
                                                           Clause == try_expr orelse
                                                           Clause == constrained_function_type ->
    lay_type(Node, Ctxt#ctxt{clause = none}, clause);
%% If a clause is formatted out of context, we
%% use a "fun-expression" clause style.
lay_type(Node, Ctxt = #ctxt{clause = Clause}, clause) when Clause == fun_expr;
                                                           Clause == implicit_fun ->
    lay_type(Node, Ctxt#ctxt{clause = undefined}, clause);
lay_type(Node, Ctxt = #ctxt{clause = Name}, clause) ->
    lay_type(Node, Ctxt#ctxt{clause = {Name, "when"}}, clause);
%% Comments on the name itself will be repeated for each
%% clause, but that seems to be the best way to handle it.
lay_type(Node, Ctxt, function) ->
    layout_hook(disjunction(erl_syntax:function_clauses(Node)),
           reset_prec(set_clause(Ctxt, erl_syntax:function_name(Node))));
lay_type(Node, Ctxt, receive_expr = Type) ->
    Ctxt1 = reset_prec(set_clause(Ctxt, Type)),
    Ctxt2 = set_nested(Ctxt1),
    Clauses = erl_syntax:receive_expr_clauses(Node),
    ClausesLayout = [{disjunction(Clauses), set_clause(Ctxt2, Type)}],
    Timeout =  erl_syntax:receive_expr_timeout(Node),
    Action = erl_syntax:receive_expr_action(Node),
    After = erl_syntax:clause(Timeout, Action),
    wrap("receive",
         if_non_empty(Clauses, ClausesLayout)
         ++ [{After, set_clause(Ctxt2, receive_timeout)}],
         "end",
         Ctxt1);
lay_type(Node, Ctxt, case_expr = Type) ->
    Ctxt1 = reset_prec(set_clause(Ctxt, Type)),
    Ctxt2 = set_nested(Ctxt1, sub_indent),
    Arg = disjunction(erl_syntax:case_expr_argument(Node)),
    Clauses = disjunction(erl_syntax:case_expr_clauses(Node)),
    layout_hook([[erl_syntax:text("case"), {[Arg], Ctxt2}, erl_syntax:text("of")],
            {Clauses, set_nested(set_formatter(Ctxt2, above))},
            erl_syntax:text("end")],
           reset_separator(Ctxt1));
lay_type(Node, Ctxt, try_expr = Type) ->
    Body = erl_syntax:try_expr_body(Node),
    Clauses = erl_syntax:try_expr_clauses(Node),
    Handlers = erl_syntax:try_expr_handlers(Node),
    Afters = erl_syntax:try_expr_after(Node),
    Ctxt1 = reset_prec(set_clause(Ctxt, Type)),
    Ctxt2 = set_formatter(Ctxt1, above),
    Ctxt3 = set_nested(Ctxt2),
    ClausesLayout = [erl_syntax:text("of"),
                     {disjunction(Clauses), Ctxt3}],
    HandlersLayout = [erl_syntax:text("catch"),
                      {disjunction(Handlers), Ctxt3}],
    AftersLayout = [erl_syntax:text("after"),
                    {conjunction(Afters), Ctxt3}],
    layout_hook([[erl_syntax:text("try"),
             {conjunction(Body), Ctxt3}
             | if_non_empty(Clauses, ClausesLayout)]]
           ++ if_non_empty(Handlers, HandlersLayout)
           ++ if_non_empty(Afters, AftersLayout)
           ++ [erl_syntax:text("end")],
           reset_separator(Ctxt));
lay_type(Node, Ctxt, block_expr) ->
    Body = conjunction(erl_syntax:block_expr_body(Node)),
    lay_expr("begin", Body, Ctxt);
lay_type(Node, Ctxt, if_expr = Type) ->
    Clauses = disjunction(erl_syntax:if_expr_clauses(Node)),
    lay_expr("if", Clauses, set_clause(Ctxt, Type));
lay_type(Node, Ctxt, fun_expr = Type) ->
    Clauses = disjunction(erl_syntax:fun_expr_clauses(Node)),
    lay_expr("fun", Clauses, set_clause(Ctxt, Type));
lay_type(Node, Ctxt, named_fun_expr) ->
    Name = erl_syntax:named_fun_expr_name(Node),
    Clauses = disjunction(erl_syntax:named_fun_expr_clauses(Node)),
    lay_expr("fun", Clauses, set_clause(Ctxt, Name));
lay_type(Node, Ctxt, implicit_fun = Type) ->
    Ctxt1 = reset_prec(set_clause(Ctxt, Type)),
    wrap("fun", {erl_syntax:implicit_fun_name(Node), Ctxt1}, none, Ctxt1);
lay_type(Node, Ctxt, function_type = Type) ->
    Args =
    case erl_syntax:function_type_arguments(Node) of
        any_arity -> [erl_syntax:text("...")];
        Arguments -> Arguments
    end,
    Return = erl_syntax:function_type_return(Node),
    Ctxt1 = set_clause(Ctxt, Type),
    case Ctxt#ctxt.clause of
        undefined ->
            Spec = erl_syntax:clause(erl_syntax:parentheses(Args), none, Return),
            wrap("fun", {erl_syntax:parentheses(Spec), Ctxt1}, none, Ctxt1);
        _ ->
            Spec = erl_syntax:clause(Args, none, Return),
            SpecClause = erl_syntax:clause([{Spec, Ctxt}], none, []),
            layout_hook(SpecClause, reset_separator(Ctxt1))
    end;
lay_type(Node, Ctxt, macro) ->
    %% This is formatted similar to a normal function call, but
    %% prefixed with a "?".
    M = max_prec(),
    Name = erl_syntax:macro_name(Node),
    Arguments = erl_syntax:macro_arguments(Node),
    lay_call([{[{erl_syntax:operator('?'), M}, Name], reset_separator(Ctxt)}],
             Arguments,
             Ctxt);
lay_type(Node, _Ctxt, comment) ->
    CText = erl_syntax:comment_text(Node),
    CPad = erl_syntax:comment_padding(Node),
    Vertical = nosep(fun prettypr:above/2),
    try
    nest(lists:min([?PADDING | [CPad||is_integer(CPad)]]),
         %% Stack lines of text above each other and prefix each string in
         %% the list with a single `%' character.
         Vertical([ prettypr:text([$% | CText]) ]))
    catch _:E ->
        io:format("~p~n", [CText]),
        exit(E)
    end;
lay_type(Node, Ctxt, error_marker) ->
    E = erl_syntax:error_marker_info(Node),
    beside(prettypr:text("** "),
           beside(lay_error_info(E, reset_prec(Ctxt)),
                  prettypr:text(" **")));
lay_type(Node, Ctxt, warning_marker) ->
    E = erl_syntax:warning_marker_info(Node),
    beside(prettypr:text("%% WARNING: "),
           lay_error_info(E, reset_prec(Ctxt)));
lay_type(_Node, _Ctxt, eof_marker) ->
    empty();
lay_type(Node, Ctxt, attribute) ->
    %% The attribute name and arguments are formatted similar to
    %% a function call, but prefixed with a "-" and followed by
    %% a period. If the arguments is `none', we only output the
    %% attribute name, without following parentheses.
    Name = erl_syntax:attribute_name(Node),
    Tag = (catch erl_syntax:concrete(Name)),
    Ctxt1 = reset_separator(reset_prec(Ctxt)),
    {Arguments, Ctxt2} = lay_type(Node, Ctxt1, {attribute, Tag}),
    Follow =  fun(A, B) -> follow(A, B, Ctxt#ctxt.break_indent) end,
    prefix("-", [Name | Arguments], set_formatter(Ctxt2, Follow));
lay_type(Node, Ctxt, {attribute, Tag}) when Tag =:= spec orelse
                                            Tag =:= callback ->
    [SpecTuple] = erl_syntax:attribute_arguments(Node),
    [FuncName, FuncTypes] = tuple_elements(SpecTuple),
    FuncName1 =
    case erl_syntax:type(FuncName) of
        tuple ->
            case tuple_elements(FuncName) of
                [F0, _] -> F0;
                [M0, F0, _] -> erl_syntax:module_qualifier(M0, F0);
                _ -> FuncName
            end;
        _ ->
            FuncName
    end,
    ClauseCtxt = set_clause(Ctxt, FuncName1),
    case erl_syntax:concrete(dodge_macros(FuncTypes)) of
        [Clause] ->
            {[Clause], ClauseCtxt};
        [Clause | Rest] ->
            RestCtxt = set_clause(Ctxt, fun_expr), % TODO
            Clauses = disjunction([{Clause, ClauseCtxt}
                                   | [{R, RestCtxt} || R <- Rest]]),
            {[{Clauses, set_formatter(Ctxt, sep)}], Ctxt}
    end;
lay_type(Node, Ctxt, {attribute, Tag}) when Tag =:= type orelse
                                            Tag =:= opaque ->
    [TypeTuple] = erl_syntax:attribute_arguments(Node),
    TypeTupleElements = tuple_elements(TypeTuple),
    [TypeName | TypeRest] = lists:map(fun dodge_macros/1, TypeTupleElements),
    [TypeBody, TypeElements] = lists:map(fun erl_syntax:concrete/1, TypeRest),

    {_, OpPrec, _} = type_inop_prec('::'),
    {_, Prec} = func_prec(),
    TypeHead = maybe_parentheses(
                 call_layout(TypeName, TypeElements, reset_prec(Ctxt)),
                 Prec, Ctxt),
    TypeLayout = maybe_parentheses(
                   [{TypeHead, set_formatter(Ctxt, beside)},
                    {erl_syntax:operator('::'), 0},
                    {TypeBody, OpPrec}],
                   OpPrec,
                   set_formatter(Ctxt, par)),

    Follow =  fun(A, B) -> follow(A, B, Ctxt#ctxt.break_indent) end,
    {TypeLayout, set_formatter(Ctxt, Follow)};
lay_type(Node, Ctxt, {attribute, Tag}) when Tag =:= export_type orelse
                                            Tag =:= optional_callbacks ->
    [FuncNames] = erl_syntax:attribute_arguments(Node),
    FuncArityQualifiers =
    erl_syntax:list(
      [erl_syntax:arity_qualifier(
         erl_syntax:atom(Atom),
         erl_syntax:integer(Arity))
       || {Atom, Arity} <- erl_syntax:concrete(dodge_macros(FuncNames))]),
    FakeNode = erl_syntax:attribute(erl_syntax:tree(Tag), FuncArityQualifiers),
    lay_type(FakeNode, Ctxt, {attribute, dodge_macros});
lay_type(Node, Ctxt, {attribute, _Tag}) ->
    case erl_syntax:attribute_arguments(Node) of
        none -> {[], Ctxt};
        Args -> {call_layout(undefined, Args, Ctxt), Ctxt}
    end;
lay_type(Node, Ctxt, record_type_field) ->
    lay_op(erl_syntax:operator('::'),
           erl_syntax:record_type_field_name(Node),
           erl_syntax:record_type_field_type(Node),
           fun(Op) -> {_, P, _} = type_inop_prec(Op), {0, 0, 0, P} end,
           Ctxt);
lay_type(Node, Ctxt, typed_record_field) ->
    lay_op(erl_syntax:operator('::'),
           erl_syntax:typed_record_field_body(Node),
           erl_syntax:typed_record_field_type(Node),
           fun(Op) -> {_, P, _} = type_inop_prec(Op), {0, 0, P, P} end,
           Ctxt);
lay_type(Node, Ctxt, annotated_type) ->
    lay_op(erl_syntax:operator('::'),
           erl_syntax:annotated_type_name(Node),
           erl_syntax:annotated_type_body(Node),
           fun(Op) -> {_, P, _} = type_inop_prec(Op), {0, 0, P, P} end,
           Ctxt);
lay_type(Node, Ctxt, type_application) ->
    %% Prefer shorthand notation.
    TypeName = erl_syntax_lib:analyze_type_application(Node),
    lay_type(Node, Ctxt, {type_application, TypeName});
lay_type(_Node, _Ctxt, {type_application, {nil, 0}}) ->
    prettypr:text("[]");
lay_type(Node, Ctxt, {type_application, {list, 1}}) ->
    [Argument] = erl_syntax:type_application_arguments(Node),
    squares([Argument], reset_prec(Ctxt));
lay_type(Node, Ctxt, {type_application, {nonempty_list, 1}}) ->
    [Argument] = erl_syntax:type_application_arguments(Node),
    squares(conjunction([Argument, erl_syntax:text("...")]), reset_prec(Ctxt));
lay_type(Node, Ctxt, {type_application, _}) ->
    Name = erl_syntax:type_application_name(Node),
    Arguments = erl_syntax:type_application_arguments(Node),
    lay_call(Name, Arguments, set_formatter(Ctxt, beside));
lay_type(Node, Ctxt, bitstring_type) ->
    Ctxt0 = reset_separator(set_prec(Ctxt, max_prec())),
    BesideCtxt = set_formatter(Ctxt0, beside),
    DM = [ {[erl_syntax:text("_:"), M], BesideCtxt}
           || M <- [erl_syntax:bitstring_type_m(Node)],
              (erl_syntax:type(M) =/= integer orelse
               erl_syntax:integer_value(M) =/= 0) ],
    DN = [ {[erl_syntax:text("_:_*"), N], BesideCtxt}
           || N <- [erl_syntax:bitstring_type_n(Node)],
              not (erl_syntax:type(N) == integer andalso
                   erl_syntax:integer_value(N) == 0) ],
    tags(conjunction(DM++DN), Ctxt0);
lay_type(Node, Ctxt, Type = constrained_function_type) ->
    Body = erl_syntax:constrained_function_type_body(Node),
    Argument = erl_syntax:constrained_function_type_argument(Node),
    BodyArgs =
    case erl_syntax:function_type_arguments(Body) of
        any_arity -> [erl_syntax:text("...")];
        Args -> Args
    end,
    Return = erl_syntax:function_type_return(Body),
    Spec = erl_syntax:clause(BodyArgs, none, Return),
    ArgSpec = erl_syntax:clause([{Spec, Ctxt}], Argument, []),
    Ctxt1 = set_clause(Ctxt, Type),
    case Ctxt#ctxt.clause of
        undefined ->
            wrap("fun", {erl_syntax:parentheses(ArgSpec), Ctxt1}, none, Ctxt1);
        _ ->
            layout_hook(ArgSpec, reset_separator(Ctxt1))
    end;
lay_type(Node, Ctxt, constraint) ->
    Name = erl_syntax:constraint_argument(Node),
    Args = erl_syntax:constraint_body(Node),
    lay_type(Node, Ctxt, {constraint, is_subtype(Name, Args), Name, Args});
lay_type(_Node, Ctxt, {constraint, true, _Name, [Var, Type]}) ->
    lay_op(erl_syntax:operator('::'),
           Var,
           Type,
           fun(Op) -> {L, P, R} = type_inop_prec(Op), {L, 0, R, P} end,
           Ctxt);
lay_type(_Node, Ctxt, {constraint, false, Name, Args}) ->
    lay_call(Name, Args, set_formatter(Ctxt, beside));
lay_type(Node, Ctxt, tuple_type) ->
    curlies(conjunction(erl_syntax:tuple_type_elements(Node)),
            reset_prec(Ctxt));
lay_type(Node, Ctxt, map_type) ->
    {Prec, _RightPrec} = type_preop_prec('#'),
    lay_op([{erl_syntax:text("#{"), max_prec()},
            {conjunction(erl_syntax:map_type_fields(Node)),
             set_formatter(reset_prec(Ctxt), sep)},
            {erl_syntax:text("}"), max_prec()}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, map_type_assoc) ->
    lay_op(erl_syntax:operator('=>'),
           erl_syntax:map_type_assoc_name(Node),
           erl_syntax:map_type_assoc_value(Node),
           fun(_) -> {0, 0, 0, 0} end,
           Ctxt);
lay_type(Node, Ctxt, map_type_exact) ->
    lay_op(erl_syntax:operator(':='),
           erl_syntax:map_type_exact_name(Node),
           erl_syntax:map_type_exact_value(Node),
           fun(_) -> {0, 0, 0, 0} end,
           Ctxt);
lay_type(Node, Ctxt, integer_range_type) ->
    lay_op(erl_syntax:operator('..'),
           erl_syntax:integer_range_type_low(Node),
           erl_syntax:integer_range_type_high(Node),
           fun(Op) -> {L, P, R} = type_inop_prec(Op), {L, 0, R, P} end,
           Ctxt);
lay_type(Node, Ctxt, record_type) ->
    {Prec, _RightPrec} = type_preop_prec('#'),
    lay_op([{erl_syntax:text("#"), max_prec()},
            {erl_syntax:record_type_name(Node), 0},
            {erl_syntax:text("{"), max_prec()},
            {conjunction(erl_syntax:record_type_fields(Node)),
             set_formatter(reset_prec(Ctxt), sep)},
            {erl_syntax:text("}"), max_prec()}],
           Prec,
           Ctxt);
lay_type(Node, Ctxt, type_union) ->
    {_LeftRpec, Prec, RightPrec} = type_inop_prec('|'),
    Ctxt1 = set_separator(Ctxt, $|),
    lay_op([{erl_syntax:type_union_types(Node), set_prec(Ctxt1, RightPrec)}],
           Prec,
           Ctxt).


lay_op(Operator, Left, Right, PrecFun, Ctxt) ->
    lay_op(erl_syntax:type(Operator), Operator, Left, Right, PrecFun, Ctxt).

lay_op(operator, Operator, Left, Right, PrecFun, Ctxt) ->
    Name = erl_syntax:operator_name(Operator),
    lay_op(Name, Operator, Left, Right, PrecFun, Ctxt);
lay_op(Name, Operator, Left, Right, PrecFun, Ctxt) ->
    {PrecL, PrecO, PrecR, PrecP} = PrecFun(Name),
    lay_op([{Left, set_prec(Ctxt, PrecL)},
            {Operator, set_prec(Ctxt, PrecO)},
            {Right, set_nested(set_prec(Ctxt, PrecR), sub_indent)}],
           PrecP, Ctxt).

lay_op(Ops, Prec, Ctxt) ->
    layout_hook(maybe_parentheses(Ops, Prec, Ctxt), reset_separator(Ctxt)).


lay_expr(Expr, Body, Ctxt) ->
    Ctxt1 = reset_prec(Ctxt),
    wrap(Expr, {Body, set_nested(Ctxt1)}, "end", Ctxt1).


is_subtype(Name, [Var, _]) ->
    (erl_syntax:is_atom(Name, is_subtype) andalso
     erl_syntax:type(Var) =:= variable);
is_subtype(_, _) -> false.

split_form_list([], _Ctxt) -> [];
split_form_list([{eof, _}], _Ctxt) -> [];
split_form_list([eof], _Ctxt) -> [];
split_form_list([First | Forms], Ctxt) ->
    T1 = erl_syntax:type(First),
    case T1 of
        eof_marker ->
            split_form_list(Forms, Ctxt);
        _ ->
            {Same, Rest} = lists:splitwith(
                              fun(F) -> T1 == erl_syntax:type(F) end,
                              Forms),
            Ctxt1 =
            case T1 of
                attribute -> set_formatter(Ctxt, above);
                text -> set_formatter(Ctxt, above);
                comment -> set_formatter(Ctxt, above);
                _ -> Ctxt
            end,
            NewForms = [First | Same] ++ [eof_marker()],
            [{NewForms, Ctxt1} | split_form_list(Rest, Ctxt)]
    end.


%% Macros are not handled well.
dodge_macros([Type]) ->
    [dodge_macros(Type)];
dodge_macros(Type) ->
    F = fun (T) ->
                case erl_syntax:type(T) of
                    macro ->
                        Var = erl_syntax:macro_name(T),
                        VarName0 = erl_syntax:variable_name(Var),
                        VarName = list_to_atom("?"++atom_to_list(VarName0)),
                        Atom = erl_syntax:atom(VarName),
                        Atom;
                    _ -> T
                end
        end,
    erl_syntax_lib:map(F, Type).

nosep([], _) -> empty();
nosep([SingleItem], _) -> SingleItem;
nosep([[SingleItem] | Tail], Guide) ->
    nosep([SingleItem | Tail], Guide);
nosep([Head | Tail], Guide) when Head == null orelse Head == [0] ->
    nosep(Tail, Guide);
nosep([Head | Tail], Guide) ->
    Guide(Head, nosep(Tail, Guide)).

nosep(Ds) when is_list(Ds) ->
    nosep(Ds, fun prettypr:beside/2);
nosep(Guide) when is_function(Guide) ->
    fun(Ds) -> nosep(Ds, Guide) end.

prefix(Prefix, D, Ctxt) ->
    wrap(Prefix, {D, Ctxt}, none, set_formatter(Ctxt, beside)).

curlies(D, Ctxt) -> wrap(D, "{}", Ctxt).

squares(D, Ctxt) -> wrap(D, "[]", Ctxt).

parentheses(D, Ctxt) -> wrap(D, "()", Ctxt).

tags(D, Ctxt) -> wrap(D, "<<>>", Ctxt).

wrap(D, Wrapper, Ctxt) ->
    WrapperLength = string:length(Wrapper),
    SliceSize = WrapperLength bsr 1,
    Begin = string:slice(Wrapper, 0, SliceSize),
    End = string:slice(Wrapper, SliceSize, WrapperLength),
    wrap(Begin, {D, Ctxt}, End, set_formatter(Ctxt, beside)).

wrap(Begin, D, End, Ctxt) ->
    Tail = if_non_empty(End, [erl_syntax:text(End) || is_list(End)]),
    layout_hook([erl_syntax:text(Begin), D | Tail], reset_separator(Ctxt)).


maybe_parentheses(D, Prec, #ctxt{prec = P}) when P > Prec ->
    erl_syntax:parentheses(D);
maybe_parentheses(D, _, _) ->
    D.

if_non_empty([], _) -> [];
if_non_empty(_, Result) -> Result.



lay_string(S, Ctxt) ->
    %% S includes leading/trailing double-quote characters. The segment
    %% width is 2/3 of the ribbon width - this seems to work well.
    W = (Ctxt#ctxt.ribbon * 2) div 3,
    lay_string_1(S, length(S), W).

lay_string_1(S, L, W) when L > W, W > 0 ->
    %% Note that L is the minimum, not the exact, printed length.
    case split_string(S, W - 1, L) of
	{_S1, ""} ->
	    prettypr:text(S);
	{S1, S2} ->
	    above(prettypr:text(S1 ++ "\""),
		  lay_string_1([$" | S2], L - W + 1, W))  %" stupid emacs
    end;
lay_string_1(S, _L, _W) ->
    prettypr:text(S).

split_string(Xs, N, L) ->
    split_string_1(Xs, N, L, []).

%% We only split strings at whitespace, if possible. We must make sure
%% we do not split an escape sequence.

split_string_1([$\s | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$\s | As]), Xs};
split_string_1([$\t | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$t, $\\ | As]), Xs};
split_string_1([$\n | Xs], N, L, As) when N =< 0, L >= 5 ->
    {lists:reverse([$n, $\\ | As]), Xs};
split_string_1([$\\ | Xs], N, L, As) ->
    split_string_2(Xs, N - 1, L - 1, [$\\ | As]);
split_string_1(Xs, N, L, As) when N =< -10, L >= 5 ->
    {lists:reverse(As), Xs};
split_string_1([X | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [X | As]);
split_string_1([], _N, _L, As) ->
    {lists:reverse(As), ""}.

split_string_2([$^, X | Xs], N, L, As) ->
    split_string_1(Xs, N - 2, L - 2, [X, $^ | As]);
split_string_2([$x, ${ | Xs], N, L, As) ->
    split_string_3(Xs, N - 2, L - 2, [${, $x | As]);
split_string_2([X1, X2, X3 | Xs], N, L, As) when
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7, X3 >= $0, X3 =< $7 ->
    split_string_1(Xs, N - 3, L - 3, [X3, X2, X1 | As]);
split_string_2([X1, X2 | Xs], N, L, As) when
  X1 >= $0, X1 =< $7, X2 >= $0, X2 =< $7 ->
    split_string_1(Xs, N - 2, L - 2, [X2, X1 | As]);
split_string_2([X | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [X | As]).

split_string_3([$} | Xs], N, L, As) ->
    split_string_1(Xs, N - 1, L - 1, [$} | As]);
split_string_3([X | Xs], N, L, As) when
  X >= $0, X =< $9; X >= $a, X =< $z; X >= $A, X =< $Z ->
    split_string_3(Xs, N - 1, L -1, [X | As]);
split_string_3([X | Xs], N, L, As) when
  X >= $0, X =< $9 ->
    split_string_1(Xs, N - 1, L -1, [X | As]).


%% Note that for the clause-making functions, the guard argument
%% can be `none', which has different interpretations in different
%% contexts.
lay_clause(none, [], _S, none,   [], _Ctxt) -> empty();
lay_clause(N   ,  P,  S,    G,   [], Ctxt) ->
    layout_hook([lay_pattern(N, P, S, guard(G), Ctxt)], Ctxt);
lay_clause(N   ,  P,  S,    G, Body, Ctxt) ->
    layout_hook([lay_pattern(N, P, S, guard(G), Ctxt),
            {conjunction(maybe_empty_lines(Body, Ctxt)),
             set_nested(set_formatter(Ctxt, above))}],
           set_separator(Ctxt, " ->")).

lay_pattern(none, [], none, Guard, Ctxt) ->
    {Guard, reset_separator(Ctxt)};
lay_pattern(none, [], Separator, Guard, Ctxt) ->
    {[none | Guard], set_separator(Ctxt, Separator)};
lay_pattern(Name, Patterns, Separator, Guard, Ctxt) when is_list(Separator) ->
    {_, Prec} = func_prec(),
    Head = maybe_parentheses(call_layout(Name, Patterns, Ctxt), Prec, Ctxt),
    HeadCtxt = reset_separator(set_formatter(Ctxt, beside)),
    GuardLayout = if_non_empty(Guard, [{Guard, set_nested(Ctxt, sub_indent)}]),
    {[{Head, HeadCtxt} | GuardLayout], set_separator(Ctxt, [$\s | Separator])};
lay_pattern(Name, Patterns, none, Guard, Ctxt) ->
    lay_pattern(Name, Patterns, [], Guard, Ctxt).

guard(none) -> [];
guard(Guard) when is_list(Guard) andalso is_list(hd(Guard)) ->
    guard(disjunction([conjunction(G) || G <- Guard]));
guard(Guard) when is_list(Guard) andalso length(Guard) > 0 ->
    guard(conjunction(Guard));
guard(Guard) ->
    [Guard].

lay_error_info({L, M, T}=T0, Ctxt) when is_integer(L), is_atom(M) ->
    case catch M:format_error(T) of
	S when is_list(S) ->
	    if L > 0 ->
		    beside(prettypr:text(io_lib:format("~w: ",[L])), prettypr:text(S));
	       true ->
		    prettypr:text(S)
	    end;
	_ ->
	    lay_concrete(T0, Ctxt)
    end;
lay_error_info(T, Ctxt) ->
    lay_concrete(T, Ctxt).

lay_concrete(T, Ctxt) ->
    layout_hook(erl_syntax:abstract(T), Ctxt).


lay_call(Name, Arguments, Ctxt) ->
    {_LeftPrec, Prec} = func_prec(),
    lay_op(call_layout(Name, Arguments, Ctxt), Prec, Ctxt).


call_layout(none, Arguments, Ctxt) ->
    ArgsCtxt = set_formatter(reset_prec(Ctxt), sep),
    [{conjunction(Arguments), ArgsCtxt}];
call_layout(undefined, Arguments, Ctxt) ->
    [{Arguments1, Ctxt1}] = call_layout(none, Arguments, Ctxt),
    [{erl_syntax:parentheses(Arguments1), Ctxt1}];
call_layout(Name, none, Ctxt) ->
    {LeftPrec, _Prec} = func_prec(),
    [{Name, set_prec(Ctxt, LeftPrec)}];
call_layout(Name, Arguments, Ctxt) ->
    Ctxt0 = reset_separator(set_formatter(Ctxt, beside)),
    [{Name1, NCtxt}] = call_layout(Name, none, Ctxt0),
    [{Arguments1, ACtxt}] = call_layout(none, Arguments, Ctxt0),
    [{[{[Name1, erl_syntax:text("(")], NCtxt},
       {[{Arguments1, set_nested(ACtxt, sub_indent)},
         erl_syntax:text(")")],
        Ctxt0}],
      ACtxt}].


seq(Items, Separator, Ctxt, Fun) ->
    seq(Items, Separator, Ctxt, Fun, []).

seq([], _, _, _, []) ->
    [empty()];
seq([H], _Separator, Ctxt, Fun, Acc) ->
    lists:reverse([Fun(H, Ctxt) | Acc]);
seq([H | T], Separator, Ctxt, Fun, Acc) ->
    seq(T, Separator, Ctxt, Fun, [beside(Fun(H, Ctxt), Separator) | Acc]).

vertical_sep(Ds) ->
    vertical_sep(prettypr:text(""), Ds).

vertical_sep(_Sep, [D]) ->
    D;
vertical_sep(Sep, [D | Ds]) ->
    above(above(D, Sep), vertical_sep(Sep, Ds));
vertical_sep(_Sep, []) ->
    [].

tidy_float([$., C | Cs]) ->
    [$., C | tidy_float_1(Cs)];  % preserve first decimal digit
tidy_float([$e | _] = Cs) ->
    tidy_float_2(Cs);
tidy_float([C | Cs]) ->
    [C | tidy_float(Cs)];
tidy_float([]) ->
    [].

tidy_float_1([$0, $0, $0 | Cs]) ->
    tidy_float_2(Cs);    % cut mantissa at three consecutive zeros.
tidy_float_1([$e | _] = Cs) ->
    tidy_float_2(Cs);
tidy_float_1([C | Cs]) ->
    [C | tidy_float_1(Cs)];
tidy_float_1([]) ->
    [].

tidy_float_2([$e, $+, $0]) -> [];
tidy_float_2([$e, $+, $0 | Cs]) -> tidy_float_2([$e, $+ | Cs]);
tidy_float_2([$e, $+ | _] = Cs) -> Cs;
tidy_float_2([$e, $-, $0]) -> [];
tidy_float_2([$e, $-, $0 | Cs]) -> tidy_float_2([$e, $- | Cs]);
tidy_float_2([$e, $- | _] = Cs) -> Cs;
tidy_float_2([$e | Cs]) -> tidy_float_2([$e, $+ | Cs]);
tidy_float_2([_C | Cs]) -> tidy_float_2(Cs);
tidy_float_2([]) -> [].

maybe_empty_lines([], _Ctxt) -> [];
maybe_empty_lines([H], _Ctxt) -> [H];
maybe_empty_lines([H | T], Ctxt) ->
    case is_last_and_before_empty_line(H, T, Ctxt) of
        true ->
            [{[H, none], Ctxt}
             | maybe_empty_lines(T, Ctxt)];
        false ->
            [H | maybe_empty_lines(T, Ctxt)]
    end;
maybe_empty_lines(Node, _Ctxt) -> Node.

is_last_and_before_empty_line(H, [], #ctxt{empty_lines = EmptyLines}) ->
    try sets:is_element(get_line(H) + 1, EmptyLines)
    catch error:badarith -> false
    end;
is_last_and_before_empty_line(H, [H2 | _], #ctxt{empty_lines = EmptyLines}) ->
    try ((get_line(H2) - get_line(H)) >= 2) and sets:is_element(get_line(H) + 1, EmptyLines)
    catch error:badarith -> false
    end.

get_line(Tree) ->
    Anno = erl_syntax:get_pos(Tree),
    erl_anno:line(Anno).

%% =====================================================================
