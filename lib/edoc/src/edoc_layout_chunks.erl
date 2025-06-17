%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2019-2021 Radek Szymczyszyn
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% @author Radek Szymczyszyn <lavrin@gmail.com>
%% @end
%% =====================================================================

%% @doc Convert EDoc module documentation to an
%% <a href="https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html">EEP-48</a>
%% `docs_v1' chunk.
%%
%% This layout is only expected to work with {@link edoc_doclet_chunks}.
%% Section <a href="chapter.html#Using_the_EDoc_API">Using the EDoc API</a>
%% in the EDoc User's Guide shows an example of using this module.
%%
%% This module breaks the convention stated in `edoc_doclet' to not rely on `edoc.hrl'
%% in doclets and layouts. It uses `#entry{}' records directly to recover information
%% that is not otherwise available to layouts.
%% @see //stdlib/shell_docs
%% @see edoc_doclet_chunks
%% @end
-module(edoc_layout_chunks).

-compile(nowarn_deprecated_catch).

% -behaviour(edoc_layout).
-export([module/2, overview/2]).

-include("edoc.hrl").

-export_type([docs_v1/0,
              docs_v1_entry/0,
              beam_language/0,
              mime_type/0,
              doc/0,
              doc_language/0,
              doc_string/0,
              metadata/0,
              signature/0]).

-include_lib("kernel/include/eep48.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-type docs_v1() :: #docs_v1{anno :: erl_anno:anno(),
                            beam_language :: beam_language(),
                            format :: mime_type(),
                            module_doc :: doc(),
                            metadata :: metadata(),
                            docs :: [docs_v1_entry()]}.
%% The Docs v1 chunk according to EEP 48.

-type docs_v1_entry() :: {_KindNameArity :: {atom(), atom(), arity()},
                          _Anno :: erl_anno:anno(),
                          _Signature :: signature(),
                          _Doc :: doc(),
                          _Metadata :: metadata()}.
%% A tuple equivalent to the `#docs_v1_entry{}' record,
%% but with the record name field skipped.

-type beam_language() :: atom().
-type mime_type() :: binary().
-type doc() :: #{doc_language() => doc_string()} | none | hidden.
-type doc_language() :: binary().
-type doc_string() :: binary().
-type metadata() :: map().
-type signature() :: [binary()].

-type xmerl_doc_node() :: #xmlComment{}
                        | #xmlElement{}
                        | #xmlPI{}
                        | #xmlText{}.
%% Subtype of {@link xmerl_xpath:nodeEntity()}.
%% It corresponds to `#xmlElement.content' as defined by `xmerl.hrl', sans the `#xmlDecl{}'.

-type xmerl_attribute() :: #xmlAttribute{}.

-type xpath() :: string().

-define(caught(Reason, M, F),
	{_, {Reason, [{M, F, _, _} | _]}}).

%%
%%' EDoc layout callbacks
%%

%% @doc Convert EDoc module documentation to an EEP-48 style doc chunk.
-spec module(edoc:edoc_module(), proplists:proplist()) -> binary().
module(Doc, Options) ->
    %% Require `entries' or fail.
    case lists:keyfind(entries, 1, Options) of
	{entries, _} -> ok;
	_ -> erlang:error(no_entries, [Doc, Options])
    end,
    Chunk = edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

-spec overview(Element :: term(), proplists:proplist()) -> term().
overview(E=#xmlElement{name = overview, content = Es}, Options) ->
    xpath_to_chunk("./title", E, Options)
        ++ xmerl_to_chunk(edoc_layout:copyright(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:version(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:since(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:authors(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:references(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:sees(Es), Options)
	    ++ xmerl_to_chunk(edoc_layout:todos(Es), Options)
        ++ xpath_to_chunk("./description/fullDescription", E, Options).
     
%%.
%%' Chunk construction
%%

-spec edoc_to_chunk(edoc:edoc_module(), proplists:proplist()) -> docs_v1().
edoc_to_chunk(Doc, Opts) ->
    [Doc] = xmerl_xpath:string("//module", Doc),
    {source, File} = lists:keyfind(source, 1, Opts),
    Entries = entries(Opts),
    ModuleEntry = edoc_data:get_entry(module, Entries),
    Line = ModuleEntry#entry.line,
    Anno = erl_anno:set_file(File, erl_anno:new(Line)),
    ModuleDoc = doc_contents("./description/fullDescription", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts)),
    Docs = doc_entries(Doc, Opts),
    docs_v1(Anno, ModuleDoc, Metadata, Docs).

-spec doc_contents(XPath, Doc, Opts) -> doc() when
      XPath :: xpath(),
      Doc :: edoc:edoc_module(),
      Opts :: proplists:proplist().
doc_contents(XPath, Doc, Opts) ->
    case doc_visibility(XPath, Doc, Opts) of
	hidden -> hidden;
	show -> doc_contents_(XPath, Doc, Opts)
    end.

-spec doc_visibility(_, _, _) -> none | hidden | show.
doc_visibility(_XPath, Doc, Opts) ->
    case {xpath_to_text("./@private", Doc, Opts),
	  proplists:get_bool(show_private, Opts),
	  xpath_to_text("./@hidden", Doc, Opts)}
    of
	%% Generating `@private' documentation was explicitly requested
	{<<"yes">>, true, _} ->
	    show;
	%% EDoc `@private' maps to EEP-48 `hidden'
	{<<"yes">>, _, _} ->
	    hidden;
	%% EDoc `@hidden' is EEP-48 `hidden'
	{_, _, <<"yes">>} ->
	    hidden;
	_ ->
	    show
    end.

doc_contents_(_XPath, Doc, Opts) ->
    Equiv = xpath_to_chunk("./equiv", Doc, Opts),
    Desc = xpath_to_chunk("./description/fullDescription", Doc, Opts),
    See = xpath_to_chunk("./see", Doc, Opts),
    doc_content(Equiv ++ Desc ++ See, Opts).

meta_deprecated(Doc, Opts) ->
    Deprecated = xpath_to_text("./deprecated/description/fullDescription", Doc, Opts),
    [{deprecated, Deprecated} || is_truthy(Deprecated)].

meta_since(Doc, Opts) ->
    Since = xpath_to_text("./since", Doc, Opts),
    [{since, Since} || is_truthy(Since)].

is_truthy(<<>>) -> false;
is_truthy(B) when is_binary(B) -> true.

doc_entries(Doc, Opts) ->
    types(Doc, Opts) ++ callbacks(Doc, Opts) ++ functions(Doc, Opts).

types(Doc, Opts) ->
    [type(TD, Opts) || TD <- xmerl_xpath:string("//typedecls/typedecl", Doc)].

type(Doc, Opts) ->
    Name = xpath_to_atom("./typedef/erlangName/@name", Doc, Opts),
    [#xmlElement{content=Content}] = xmerl_xpath:string("./typedef/argtypes", Doc),
    Arity = length(Content),
    Anno = anno(Doc, Opts),
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    EntryDoc = doc_contents("./description/fullDescription", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      meta_type_sig(Name, Arity, Anno, entries(Opts))),
    docs_v1_entry(type, Name, Arity, Anno, Signature, EntryDoc, Metadata).

-spec meta_type_sig(atom(), arity(), erl_anno:anno(), [edoc:entry()]) -> Metadata when
      Metadata :: [{signature, [erl_parse:abstract_form()]}].
meta_type_sig(Name, Arity, Anno, Entries) ->
    Line = erl_anno:line(Anno),
    Tags = edoc_data:get_all_tags(Entries),
    case lists:filtermap(fun (T) -> select_tag(T, Name, Arity, Line) end, Tags) of
	[] -> [];
	[TypeAttr] ->
	    [{signature, [TypeAttr]}]
    end.

select_tag(#tag{name = type, line = Line, origin = code} = T,
	   Name, Arity, Line) ->
    TypeTree = T#tag.form,
    TypeAttr = erl_syntax:revert(TypeTree),
    case TypeAttr of
	{attribute, _, Type, {Name, _, Args}}
	  when (type =:= Type orelse opaque =:= Type orelse nominal =:= Type),
	       length(Args) == Arity ->
	    {true, TypeAttr};
	_ ->
	    false
    end;
select_tag(_, _, _, _) -> false.

callbacks(_Doc, Opts) ->
    Entries = entries(Opts),
    Tags = edoc_data:get_all_tags(Entries),
    Callbacks = edoc_data:get_tags(callback, Tags),
    [callback(Cb, Opts) || Cb <- Callbacks].

callback(Cb = #tag{name = callback, origin = code}, Opts) ->
    #tag{line = Line,
	 data = {{Name, Arity}, MaybeDoc},
	 form = Form} = Cb,
    EntryDoc = case MaybeDoc of
		   none -> none;
		   _ -> doc_content([xmerl_to_binary(MaybeDoc, Opts)], Opts)
	       end,
    {source, File} = lists:keyfind(source, 1, Opts),
    Anno = erl_anno:set_file(File, erl_anno:new(Line)),
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    Metadata = maps:from_list([{signature, [Form]}]),
    docs_v1_entry(callback, Name, Arity, Anno, Signature, EntryDoc, Metadata).

functions(Doc, Opts) ->
    [function(F, Opts) || F <- xmerl_xpath:string("//module/functions/function", Doc)].

function(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    {Line, Signature, Spec} = function_line_sig_spec({Name, Arity}, Opts),
    {source, File} = lists:keyfind(source, 1, Opts),
    Anno = erl_anno:set_file(File, erl_anno:new(Line)),
    EntryDoc = doc_contents("./", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      Spec),
    docs_v1_entry(function, Name, Arity, Anno, Signature, EntryDoc, Metadata).

-spec function_line_sig_spec(edoc:function_name(), proplists:proplist()) -> R when
      R :: {non_neg_integer(), signature(), [{signature, erl_parse:abstract_form()}]}.
function_line_sig_spec(NA, Opts) ->
    Entries = entries(Opts),
    #entry{name = NA, line = Line} = E = lists:keyfind(NA, #entry.name, Entries),
    {ArgNames, Sig} = args_and_signature(E),
    case lists:keyfind(spec, #tag.name, E#entry.data) of
	false ->
	    {Line, Sig, []};
	#tag{name = spec, origin = comment} ->
	    {Line, Sig, []};
	#tag{name = spec, origin = code} = T ->
	    F = erl_syntax:revert(T#tag.form),
	    Annotated = annotate_spec(ArgNames, F, source_file(Opts), Line),
	    {Line, Sig, [{signature, [Annotated]}]}
    end.

args_and_signature(E = #entry{}) ->
    %% At this point `#entry.args' might be two things:
    %% - a list of arg names if no `-spec' is present,
    %% - if `-spec' is present, it's a list of clauses;
    %%   the number of clauses is the same as the number of spec clauses;
    %%   all clauses have the first function clause arg names.
    %%
    %% See `test/eep48_SUITE_data/eep48_sigs.erl'
    %% and `test/eep48_SUITE_data/eep48_specs.erl' for examples.
    {Name, _} = E#entry.name,
    case E#entry.args of
	[Args | _] = Clauses when is_list(Args) ->
	    {Clauses, format_signature(Name, Args)};
	Args when is_list(Args) ->
	    {Args, format_signature(Name, Args)}
    end.

format_signature(Name, []) ->
    [<<(atom_to_binary(Name, utf8))/bytes, "()">>];
format_signature(Name, [Arg]) ->
    [<<(atom_to_binary(Name, utf8))/bytes, "(", (atom_to_binary(Arg, utf8))/bytes, ")">>];
format_signature(Name, [Arg | Args]) ->
    [<<(atom_to_binary(Name, utf8))/bytes, "(", (atom_to_binary(Arg, utf8))/bytes, ",">>
     | format_signature(Args)].

format_signature([Arg]) ->
    [<<(atom_to_binary(Arg, utf8))/bytes, ")">>];
format_signature([Arg | Args]) ->
    [<<(atom_to_binary(Arg, utf8))/bytes, ",">> | format_signature(Args)].

annotate_spec(ArgClauses, Spec, SourceFile, Line) ->
    try
	annotate_spec_(ArgClauses, Spec)
    catch
	error:{bounded_fun_arity, Vars} ->
	    bounded_fun_arity_error(Vars, Spec, SourceFile, Line)
    end.

bounded_fun_arity_error(Vars, Spec, SourceFile, Line) ->
    edoc_report:warning(Line, SourceFile,
			"cannot handle spec with constraints - arity mismatch.\n"
			"This is a bug in EDoc spec formatter - please report it at "
			"https://bugs.erlang.org/\n"
			"Identified arguments: ~p\n"
			"Original spec: ~s\n",
			[[ VName || {var, _, VName} <- Vars ], erl_pp:attribute(Spec)]),
    Spec.

annotate_spec_(ArgClauses, {attribute, Pos, spec, Data} = Spec) ->
    {NA, SpecClauses} = Data,
    case catch lists:zip(ArgClauses, SpecClauses) of
	?caught(function_clause, lists, zip) ->
	    edoc_report:warning("cannot annotate spec: "
				"function and spec clause numbers do not match\n", []),
	    Spec;
	ArgSpecClauses ->
	    NewData = {NA, [ annotate_clause(AC, SC) || {AC, SC} <- ArgSpecClauses ]},
	    {attribute, Pos, spec, NewData}
    end.

annotate_clause(ArgNames, {type, Pos, 'fun', Data}) ->
    [{type, _, product, ArgTypes}, RetType] = Data,
    AnnArgTypes = [ ann_fun_type(Name, Pos, Type) || {Name, Type} <- lists:zip(ArgNames, ArgTypes) ],
    NewData = [{type, Pos, product, AnnArgTypes}, RetType],
    {type, Pos, 'fun', NewData};
annotate_clause(ArgNames, {type, Pos, 'bounded_fun', Data}) ->
    [{type, _, 'fun', _} = Clause, Constraints] = Data,
    {NewClause, NewConstraints} = annotate_bounded_fun_clause(ArgNames, Clause, Constraints),
    {type, Pos, 'bounded_fun', [NewClause, NewConstraints]}.

ann_fun_type(_Name, _Pos, {ann_type,_,_} = AnnType) ->
    AnnType;
ann_fun_type(Name, Pos, Type) ->
    TypeVar = erl_syntax:set_pos(erl_syntax:variable(Name), Pos),
    AnnType = erl_syntax:set_pos(erl_syntax:annotated_type(TypeVar, Type), Pos),
    erl_syntax:revert(AnnType).

annotate_bounded_fun_clause(ArgNames, {type, Pos, 'fun', Data}, Constraints) ->
    [{type, _, product, Args}, RetType] = Data,
    NewVarsAndConstraints = lists:foldl(fun ({Name, Arg}, Acc) ->
						bounded_fun_arg(Acc#{name := Name, arg := Arg})
					end,
					#{name => undefined,
					  arg => undefined,
					  pos => Pos,
					  new_vars => [],
					  new_constraints => [],
					  ret_type => RetType,
					  constraints => Constraints},
					lists:zip(ArgNames, Args)),
    #{new_vars := TypeVars, new_constraints := NewConstraints} = NewVarsAndConstraints,
    length(ArgNames) == length(TypeVars) orelse erlang:error({bounded_fun_arity, TypeVars}),
    NewConstraints2 = case RetType of
			  {var, _, _} -> [get_constraint(RetType, Constraints) | NewConstraints];
			  _ -> NewConstraints
		      end,
    NewData = [{type, Pos, product, lists:reverse(TypeVars)}, RetType],
    {{type, Pos, 'fun', NewData}, lists:reverse(NewConstraints2)}.

bounded_fun_arg(#{ arg := {Singleton, _, _} = Arg } = Acc) when atom =:= Singleton;
								integer =:= Singleton ->
    #{new_vars := NVs} = Acc,
    Acc#{new_vars := [Arg | NVs]};
bounded_fun_arg(#{ arg := {var, _, '_'} = V } = Acc) ->
    #{new_vars := NVs} = Acc,
    Acc#{new_vars := [V | NVs]};
bounded_fun_arg(#{ arg := {var, _, _} = V } = Acc) ->
    #{new_vars := NVs, new_constraints := NCs, constraints := Cs, ret_type := RetType} = Acc,
    %% Is this variable directly constrained?
    %% I.e. is there a `when V :: ...' clause present?
    case get_constraint(V, Cs) of
	{type, _, constraint, _} = C ->
	    Acc#{new_vars := [V | NVs],
		 new_constraints := [C | NCs]};
	no_constraint ->
	    %% If a variable is not constrained directly, but mentioned
	    %% in another variable's constraint, it's fine - e.g. `Key':
	    %%
	    %% -spec is_key(Key, Orddict) -> boolean() when
	    %%       Orddict :: orddict(Key, Value :: term()).
	    case get_mention(V, Cs) of
		{type, _, constraint, _} ->
		    Acc#{new_vars := [V | NVs]};
		no_mention ->
		    %% Is the argument type variable mentioned in the return value?
		    {var, _, Name} = V,
		    RetNames = erl_syntax_lib:variables(RetType),
		    case sets:is_element(Name, RetNames) of
			true ->
			    Acc#{new_vars := [V | NVs]};
			false ->
			    Acc
		    end
	    end
    end;
bounded_fun_arg(#{ arg := {ann_type, Var, Type} } = Acc) ->
    bounded_fun_arg_(Var, Type, Acc);
bounded_fun_arg(#{ arg := Type } = Acc) when remote_type =:= element(1, Type);
					     type =:= element(1, Type);
					     user_type =:= element(1, Type) ->
    #{name := Name, pos := Pos} = Acc,
    Var = erl_syntax:revert(erl_syntax:set_pos(erl_syntax:variable(Name), Pos)),
    bounded_fun_arg_(Var, Type, Acc).

bounded_fun_arg_(Var, Type, Acc) ->
    #{pos := Pos, new_vars := NVs, new_constraints := NCs} = Acc,
    C = {type, Pos, constraint, [{atom, Pos, is_subtype}, [Var, Type]]},
    Acc#{new_vars := [Var | NVs],
	 new_constraints := [C | NCs]}.

get_constraint({var, _, Name}, Constraints) ->
    F = fun
	    ({type, _, constraint, [_, [{var, _, CName}, _]]}) when Name =:= CName -> true;
	    (_) -> false
	end,
    case lists:filter(F, Constraints) of
	[C] -> C;
	[] -> no_constraint
    end.

get_mention({var, _, Name}, Constraints) ->
    F = fun
	    ({type, _, constraint, _} = C) ->
		Vars = erl_syntax_lib:variables(C),
		sets:is_element(Name, Vars);
	    (_) -> false
	end,
    case lists:filter(F, Constraints) of
	[C | _] -> C;
	[] -> no_mention
    end.

-spec entries(proplists:proplist()) -> [edoc:entry()].
entries(Opts) ->
    {entries, Entries} = lists:keyfind(entries, 1, Opts),
    Entries.

-spec source_file(proplists:proplist()) -> [edoc:entry()].
source_file(Opts) ->
    {source, Source} = lists:keyfind(source, 1, Opts),
    Source.

-spec doc_content(_, _) -> doc().
doc_content([], _Opts) -> #{};
doc_content(Content, Opts) ->
    DocLanguage = proplists:get_value(lang, Opts, <<"en">>),
    #{DocLanguage => Content}.

docs_v1(Anno, ModuleDoc, Metadata, Docs) ->
    #docs_v1{anno = Anno,
             module_doc = ModuleDoc,
             metadata = Metadata,
             docs = Docs}.

anno(Doc, Opts) ->
    {source, File} = lists:keyfind(source, 1, Opts),
    Line = xpath_to_integer("./@line", Doc, Opts),
    erl_anno:set_file(File, erl_anno:new(Line)).

-spec docs_v1_entry(_, _, _, _, _, _, _) -> docs_v1_entry().
docs_v1_entry(Kind, Name, Arity, Anno, Signature, EntryDoc, Metadata) ->
    {{Kind, Name, Arity}, Anno, Signature, EntryDoc, Metadata}.

-spec xpath_to_text(_, _, _) -> binary().
xpath_to_text(XPath, Doc, Opts) ->
    case xmerl_xpath:string(XPath, Doc) of
	[] -> <<>>;
	[#xmlAttribute{} = Attr] ->
	    {_ , Value} = format_attribute(Attr),
            case shell_docs:normalize([Value]) of
                [{p,[],[Normal]}] -> Normal
            end;
	[#xmlElement{}] = Elements ->
	    xmerl_to_binary(Elements, Opts);
	[_|_] ->
	    erlang:error(multiple_nodes, [XPath, Doc, Opts])
    end.

xmerl_to_binary(XML, Opts) ->
    iolist_to_binary(chunk_to_text(xmerl_to_chunk(XML, Opts))).

chunk_to_text([]) -> [];
chunk_to_text([Node | Nodes]) ->
    case Node of
	_ when is_binary(Node) -> [Node | chunk_to_text(Nodes)];
	{_Tag, _Attrs, SubNodes} -> [chunk_to_text(SubNodes) | chunk_to_text(Nodes)]
    end.

xpath_to_atom(XPath, Doc, Opts) ->
    binary_to_atom(xpath_to_text(XPath, Doc, Opts), utf8).

xpath_to_integer(XPath, Doc, Opts) ->
    binary_to_integer(xpath_to_text(XPath, Doc, Opts)).

xpath_to_chunk(XPath, Doc, Opts) ->
    XmerlDoc = xmerl_xpath:string(XPath, Doc),
    xmerl_to_chunk(XmerlDoc, Opts).

%%.
%%' Xmerl to chunk format
%%

-spec xmerl_to_chunk([xmerl_doc_node()], proplists:proplist()) -> shell_docs:chunk_elements().
xmerl_to_chunk(Contents, Opts) ->
    shell_docs:normalize(format_content(Contents, Opts)).

-spec format_content([xmerl_doc_node()], proplists:proplist()) -> shell_docs:chunk_elements().
format_content(Contents, Opts) ->
    {SeeTags, OtherTags} = lists:partition(fun (#xmlElement{name = see}) -> true;
					       (_) -> false end,
					   Contents),
    lists:flatten([ format_content_(T, Opts) || T <- OtherTags ] ++ rewrite_see_tags(SeeTags, Opts)).

-spec format_content_(xmerl_doc_node(), proplists:proplist()) -> shell_docs:chunk_elements().
format_content_(#xmlPI{}, _)      -> [];
format_content_(#xmlComment{}, _) -> [];

format_content_(#xmlText{} = T, _) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
	true -> [];
	false -> [unicode:characters_to_binary(Text)]
    end;

format_content_(#xmlElement{name = equiv} = E, Opts) ->
    format_element(rewrite_equiv_tag(E), Opts);
format_content_(#xmlElement{name = a} = E, Opts) ->
    format_element(rewrite_a_tag(E), Opts);
format_content_(#xmlElement{name = title} = E, Opts) ->
    format_element(rewrite_title_tag(E), Opts);
format_content_(#xmlElement{} = E, Opts) ->
    format_element(E, Opts);
format_content_({Tag, Content}, Opts) ->
    format_content_(xmerl_lib:normalize_element({Tag, [], Content}), Opts);
format_content_(List, Opts) when is_list(List) ->
    format_content_(#xmlText{ value = List }, Opts).

format_element(#xmlElement{} = E, Opts) ->
    #xmlElement{name = Name, content = Content, attributes = Attributes} = E,
    case {is_edoc_tag(Name), is_html_tag(Name)} of
	{true, _} ->
	    format_content(Content, Opts);
	{_, false} ->
	    edoc_report:warning(0, source_file(Opts), "'~s' is not allowed - skipping tag, extracting content", [Name]),
            [<<"<",(atom_to_binary(Name))/binary,">">>,
             format_content(Content, Opts),
             <<"</",(atom_to_binary(Name))/binary,">">>];
	_ ->
	    [{Name, format_attributes(Attributes), format_content(Content, Opts)}]
    end.

-spec format_attributes([xmerl_attribute()]) -> [shell_docs:chunk_element_attr()].
format_attributes(Attrs) ->
    [ format_attribute(Attr) || Attr <- Attrs ].

-spec format_attribute(xmerl_attribute()) -> shell_docs:chunk_element_attr().
format_attribute(#xmlAttribute{} = Attr) ->
    #xmlAttribute{name = Name, value = V} = Attr,
    %% From xmerl.hrl: #xmlAttribute.value :: IOlist() | atom() | integer()
    case V of
	_ when is_list(V)    -> {Name, unicode:characters_to_binary(V)};
	_ when is_atom(V)    -> {Name, atom_to_binary(V, utf8)};
	_ when is_integer(V) -> {Name, integer_to_binary(V)}
    end.

-spec is_edoc_tag(atom()) -> boolean().
is_edoc_tag(fullDescription) -> true;
is_edoc_tag(since) -> true;
is_edoc_tag(_) -> false.

-spec is_html_tag(atom()) -> boolean().
is_html_tag(Tag) ->
    Tags = shell_docs:supported_tags(),
    lists:member(Tag, Tags).

rewrite_a_tag(#xmlElement{name = a} = E) ->
    SimpleE = xmerl_lib:simplify_element(E),
    xmerl_lib:normalize_element(rewrite_docgen_link(SimpleE)).

rewrite_title_tag(#xmlElement{name = title} = E) ->
    E#xmlElement{ name = h1 }.

rewrite_see_tags([], _Opts) -> [];
rewrite_see_tags([#xmlElement{name = see} | _] = SeeTags, Opts) ->
    Grouped = [ rewrite_see_tag(T) || T <- SeeTags ],
    NewXML = {p, [], [{em,[],["See also: "]}] ++ lists:join(", ", Grouped) ++ ["."]},
    %% Convert strings to binaries in the entire new tree:
    [format_content_(xmerl_lib:normalize_element(NewXML), Opts)].

rewrite_see_tag(#xmlElement{name = see} = E) ->
    %% TODO: this is not formatted nicely by shell_docs...
    %% missing `p' around preceding description
    SeeTag = xmerl_lib:simplify_element(E),
    {see, Attrs, XML} = rewrite_docgen_link(SeeTag),
    {a, Attrs, XML}.

rewrite_docgen_link({Tag, AttrL, SubEls} = E) when Tag =:= a; Tag =:= see ->
    Attrs = maps:from_list(AttrL),
    case {maps:get('docgen-rel', Attrs, false), maps:get('docgen-href', Attrs, false)} of
	{false, false} -> E;
	{false, _} -> inconsistent_docgen_attrs(Attrs);
	{_, false} -> inconsistent_docgen_attrs(Attrs);
	{ShortRel, URI} ->
	    AttrsNoDocgen = maps:without(['docgen-rel', 'docgen-href'], Attrs),
	    NewAttrs = AttrsNoDocgen#{rel => expand_docgen_rel(ShortRel),
				      href => URI},
	    {Tag, maps:to_list(NewAttrs), SubEls}
    end.

inconsistent_docgen_attrs(Attrs) ->
    %% Only one of `docgen-rel` and `docgen-href` is found - should not happen!
    erlang:error({inconsistent_docgen_attrs, Attrs}).

%% @doc `Rel' is actually a stringified {@link edoc_refs:docgen_rel()}.
-spec expand_docgen_rel(Rel) -> string() when
      Rel :: string().
expand_docgen_rel(Rel)
  when Rel =:= "seemfa"; Rel =:= "seeerl"; Rel =:= "seetype"; Rel =:= "seeapp";
       Rel =:= "seecom"; Rel =:= "seecref"; Rel =:= "seefile" ; Rel =:= "seeguide" ->
    "https://erlang.org/doc/link/" ++ Rel.

rewrite_equiv_tag(#xmlElement{name = equiv} = E) ->
    NewE = case xmerl_lib:simplify_element(E) of
	       {equiv, [], [{expr, [], Expr}]} ->
		   {p, [], ["Equivalent to ", Expr, "."]};
	       {equiv, [], [{expr, [], Expr}, {see, _, _} = SeeTag]} ->
		   {see, Attrs, _} = rewrite_docgen_link(SeeTag),
		   {p, [], ["Equivalent to ", {a, Attrs, Expr}, "."]}
	   end,
    xmerl_lib:normalize_element(NewE).

%%. vim: foldmethod=marker foldmarker=%%',%%.
