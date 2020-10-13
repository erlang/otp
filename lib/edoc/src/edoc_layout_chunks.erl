%% @doc Convert EDoc module documentation to an EEP-48 `docs_v1' chunk.
%% @since 0.12
-module(edoc_layout_chunks).

-behaviour(edoc_layout).
-export([module/2]).

%% This breaks the convention stated in `edoc_doclet' to not rely on `edoc.hrl'
%% in doclets and layouts.
%% However, without direct `#entry{}' access it's quite unwieldy to pass spec/type
%% info for storage in chunks.
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

-type docs_v1_entry() :: #docs_v1_entry{kind_name_arity :: {atom(), atom(), arity()},
                                        anno :: erl_anno:anno(),
                                        signature :: signature(),
                                        doc :: doc(),
                                        metadata :: metadata()}.

-type beam_language() :: atom().
-type mime_type() :: binary().
-type doc() :: #{doc_language() => doc_string()} | none | hidden.
-type doc_language() :: binary().
-type doc_string() :: binary().
-type metadata() :: map().
-type signature() :: [binary()].

-type xmerl_document_node() :: #xmlElement{}
                             | #xmlText{}
                             | #xmlPI{}
                             | #xmlComment{}
                             | #xmlDecl{}.
%% `#xmlElement.content' as defined by `xmerl.hrl'.

-type xmerl_attribute() :: #xmlAttribute{}.

-type xpath() :: string().

%%
%%' EDoc layout callbacks
%%

%% @doc Convert EDoc module documentation to an EEP-48 style doc chunk.
-spec module(edoc:xmerl_module(), proplists:proplist()) -> binary().
module(Doc, Options) ->
    %% Require `entries' or fail.
    case lists:keyfind(entries, 1, Options) of
	{entries, _} -> ok;
	_ -> erlang:error(no_entries, [Doc, Options])
    end,
    Chunk = edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

%%.
%%' Chunk construction
%%

-spec edoc_to_chunk(edoc:xmerl_module(), proplists:proplist()) -> docs_v1().
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
      Doc :: edoc:xmerl_module(),
      Opts :: proplists:proplist().
doc_contents(XPath, Doc, Opts) ->
    case doc_visibility(XPath, Doc, Opts) of
	hidden -> hidden;
	none -> none;
	regular -> doc_contents_(XPath, Doc, Opts)
    end.

doc_visibility(XPath, Doc, Opts) ->
    case {xpath_to_text("./@private", Doc, Opts),
	  xpath_to_text("./@hidden", Doc, Opts)}
    of
	{<<"yes">>, _} ->
	    %% EDoc `@private' is EEP-48 `hidden'
	    hidden;
	{_, <<"yes">>} ->
	    %% EDoc `@hidden' is EEP-48 `none'
	    none;
	_ ->
	    regular
    end.

doc_contents_(XPath, Doc, Opts) ->
    Equiv = xpath_to_chunk("./equiv", Doc),
    Desc = xpath_to_chunk("./description/fullDescription", Doc),
    See = xpath_to_chunk("./see", Doc),
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
    EntryDoc = doc_contents("./description/fullDescription", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      meta_type_sig(Name, Arity, Anno, entries(Opts))),
    docs_v1_entry(type, Name, Arity, Anno, EntryDoc, Metadata).

-spec meta_type_sig(atom(), arity(), erl_anno:anno(), [edoc:entry()]) -> Metadata when
      Metadata :: #{signature => erl_parse:abstract_form()}.
meta_type_sig(Name, Arity, Anno, Entries) ->
    Line = erl_anno:line(Anno),
    Tags = edoc_data:get_all_tags(Entries),
    case lists:keyfind(Line, #tag.line, Tags) of
	#tag{name = type, line = Line, origin = code} = T ->
	    TypeTree = T#tag.form,
	    TypeAttr = erl_syntax:revert(TypeTree),
	    %% Assert that the lookup by line really gives us the right type attribute:
	    {attribute, _, type, {Name, _, Args}} = TypeAttr,
	    {Name, Arity} = {Name, length(Args)},
	    [{signature, [TypeAttr]}];
	_ ->
	    []
    end.

callbacks(Doc, Opts) ->
    [callback(C, Opts) || C <- xmerl_xpath:string("//module/callbacks/callback", Doc)].

callback(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    Entries = entries(Opts),
    Tags = edoc_data:get_all_tags(Entries),
    {Line, DocContent, Meta} = callback_line_doc_and_meta(Name, Arity, Tags),
    {source, File} = lists:keyfind(source, 1, Opts),
    Anno = erl_anno:set_file(File, erl_anno:new(Line)),
    EntryDoc = doc_content(DocContent, Opts),
    Metadata = maps:from_list(Meta),
    docs_v1_entry(callback, Name, Arity, Anno, EntryDoc, Metadata).

select_callback(Name, Arity) ->
    fun (#tag{name = callback, data = {{N, A}, _}})
	  when N =:= Name, A =:= Arity -> true;
	(_) -> false
    end.

callback_line_doc_and_meta(Name, Arity, Tags) ->
    case lists:filter(select_callback(Name, Arity), Tags) of
	[#tag{name = callback, origin = code} = T] ->
	    #tag{line = L, data = {_, D0}, form = F} = T,
	    D1 = case D0 of
		     none -> none;
		     _ -> xmerl_to_binary(D0)
		 end,
	    {L, [D1], [{signature, [F]}]};
	_ ->
	    %% TODO: callback placeholders...
	    {0, none, []}
    end.

functions(Doc, Opts) ->
    [function(F, Opts) || F <- xmerl_xpath:string("//module/functions/function", Doc)].

function(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    {Line, MetaSig} = function_line_and_signature({Name, Arity}, entries(Opts)),
    {source, File} = lists:keyfind(source, 1, Opts),
    Anno = erl_anno:set_file(File, erl_anno:new(Line)),
    EntryDoc = doc_contents("./", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      MetaSig),
    docs_v1_entry(function, Name, Arity, Anno, EntryDoc, Metadata).

-spec function_line_and_signature(edoc:function_name(), [edoc:entry()]) -> R when
      R :: {non_neg_integer(), [{signature, erl_parse:abstract_form()}]}.
function_line_and_signature(NA, Entries) ->
    #entry{name = NA, line = Line} = E = lists:keyfind(NA, #entry.name, Entries),
    case lists:keyfind(spec, #tag.name, E#entry.data) of
	false ->
	    {Line, []};
	#tag{name = spec} = T ->
	    {Line, [{signature, [erl_syntax:revert(T#tag.form)]}]}
    end.

-spec entries(proplists:proplist()) -> [edoc:entry()].
entries(Opts) ->
    {entries, Entries} = lists:keyfind(entries, 1, Opts),
    Entries.

-spec doc_content(_, _) -> doc().
doc_content([], _Opts) -> none;
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

-spec docs_v1_entry(_, _, _, _, _, _) -> docs_v1_entry().
docs_v1_entry(Kind, Name, Arity, Anno, EntryDoc, Metadata) ->
    %% `Signature' is a pretty-printed label. The real signature (spec) is stored in `Metadata'.
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    {{Kind, Name, Arity}, Anno, Signature, EntryDoc, Metadata}.

-spec xpath_to_text(_, _, _) -> binary().
xpath_to_text(XPath, Doc, Opts) ->
    case xmerl_xpath:string(XPath, Doc) of
	[] -> <<>>;
	[#xmlAttribute{} = Attr] ->
	    {_ , Value} = format_attribute(Attr),
	    hd(shell_docs:normalize([Value]));
	[#xmlElement{}] = Elements ->
	    xmerl_to_binary(Elements);
	[_|_] ->
	    erlang:error(multiple_nodes, [XPath, Doc, Opts])
    end.

xmerl_to_binary(XML) ->
    iolist_to_binary(chunk_to_text(xmerl_to_chunk(XML))).

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

xpath_to_chunk(XPath, Doc) ->
    XmerlDoc = xmerl_xpath:string(XPath, Doc),
    xmerl_to_chunk(XmerlDoc).

%%.
%%' Xmerl to chunk format
%%

%% TODO: shell_docs:chunk_elements() is not exported yet.
-spec xmerl_to_chunk(edoc:xmerl_module()) -> shell_docs:chunk_elements().
xmerl_to_chunk(Contents) ->
    shell_docs:normalize(format_content(Contents)).

-spec format_content(edoc:xmerl_module()) -> shell_docs:chunk_elements().
format_content(Contents) ->
    lists:flatten([ format_content_(C) || C <- Contents ]).

-spec format_content_(xmerl_document_node()) -> shell_docs:chunk_elements().
format_content_(#xmlPI{})      -> [];
format_content_(#xmlComment{}) -> [];
format_content_(#xmlDecl{})    -> [];

format_content_(#xmlText{} = T) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
	true -> [];
	false -> [unicode:characters_to_binary(Text)]
    end;

format_content_(#xmlElement{name = equiv} = E) ->
    format_element(rewrite_equiv_tag(E));
format_content_(#xmlElement{name = a} = E) ->
    format_element(rewrite_a_tag(E));
format_content_(#xmlElement{name = see} = E) ->
    format_element(rewrite_see_tag(E));
format_content_(#xmlElement{} = E) ->
    format_element(E).

format_element(#xmlElement{} = E) ->
    #xmlElement{name = Name, content = Content, attributes = Attributes} = E,
    case {is_edoc_tag(Name), is_html_tag(Name)} of
	{true, _} ->
	    format_content(Content);
	{_, false} ->
	    edoc_report:warning("'~s' is not accepted - skipping tag, extracting content", [Name]),
	    format_content(Content);
	_ ->
	    [{Name, format_attributes(Attributes), format_content(Content)}]
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
    %% This is only a subset of existing HTML tags.
    %% Compare with https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    Tags = [a,p,h1,h2,h3,i,br,em,pre,code,ul,ol,li,dl,dt,dd],
    lists:member(Tag, Tags).

rewrite_a_tag(#xmlElement{name = a} = E) ->
    SimpleE = xmerl_lib:simplify_element(E),
    xmerl_lib:normalize_element(rewrite_docgen_link(SimpleE)).

rewrite_see_tag(#xmlElement{name = see} = E) ->
    %% TODO: this is not formatted nicely by shell_docs...
    %% missing `p' around preceding description
    SeeTag = xmerl_lib:simplify_element(E),
    {see, Attrs, XML} = rewrite_docgen_link(SeeTag),
    NewXML = {p, [], ["See also ", {a, Attrs, XML}, "."]},
    xmerl_lib:normalize_element(NewXML).

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
	    {Tag, [{'orig-tag', Tag}] ++ maps:to_list(NewAttrs), SubEls}
    end.

inconsistent_docgen_attrs(Attrs) ->
    %% Only one of `docgen-rel` and `docgen-href` is found - should not happen!
    erlang:error({inconsistent_docgen_attrs, Attrs}).

-spec expand_docgen_rel(edoc_refs:docgen_rel()) -> string().
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
