-module(eep48_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/eep48.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([edoc_app_should_pass_shell_docs_validation/1,
	 module_anno/1,
	 function_anno/1,
	 type_anno/1,
	 cb_anno/1,
	 function_since_tag/1,
	 function_deprecated_tag/1,
	 type_since_tag/1,
	 type_deprecated_tag/1,
	 cb_since_tag/1,
	 cb_deprecated_tag/1,
	 links/1,
	 see_tags_grouped_together/1,
	 equiv/1,
	 f_sig_single_simple_clause/1,
	 f_sig_single_simple_clause_with_spec/1,
	 f_sig_multiple_simple_clauses/1,
	 f_sig_multiple_simple_clauses_with_spec/1,
	 f_sig_single_record_clause/1,
	 f_sig_single_record_clause_with_spec/1,
	 f_sig_multiple_record_clauses/1,
	 f_sig_multiple_record_clauses_with_spec/1,
	 f_spec_type_without_name/1,
	 f_spec_types_mixed/1,
	 f_spec_with_multiple_clauses/1,
	 f_spec_with_multiple_clauses_one_fun_clause/1,
	 f_spec_lhs_match_expr/1,
	 f_spec_rhs_match_expr/1,
	 f_spec_unnamed_pattern/1,
	 f_spec_bounded_single_clause_fun/1,
	 f_spec_bounded_multiple_clause_fun/1,
	 f_spec_bounded_singleton_atom/1,
	 f_spec_bounded_singleton_int/1,
	 f_spec_rettype_constraint/1,
	 f_spec_indirect_constraint/1,
	 f_spec_arg_type_in_retval/1,
	 f_redundant_spec/1,
	 f_only_attr/1,
	 f_only_tag/1,
	 f_prefixed_spec/1,
	 t_only_attr/1,
	 t_only_tag/1,
	 t_redundant/1]).

-define(a2b(A), atom_to_binary(A, utf8)).
-define(io2b(IO), iolist_to_binary(IO)).

%%
%% CT preamble
%%

suite() -> [].

all() -> [edoc_app_should_pass_shell_docs_validation,
	  module_anno,
	  function_anno,
	  type_anno,
	  cb_anno,
	  function_since_tag,
	  function_deprecated_tag,
	  type_since_tag,
	  type_deprecated_tag,
	  cb_since_tag,
	  cb_deprecated_tag,
	  links,
	  see_tags_grouped_together,
	  equiv,
	  f_sig_single_simple_clause,
	  f_sig_single_simple_clause_with_spec,
	  f_sig_multiple_simple_clauses,
	  f_sig_multiple_simple_clauses_with_spec,
	  f_sig_single_record_clause,
	  f_sig_single_record_clause_with_spec,
	  f_sig_multiple_record_clauses,
	  f_sig_multiple_record_clauses_with_spec,
	  f_spec_type_without_name,
	  f_spec_types_mixed,
	  f_spec_with_multiple_clauses,
	  f_spec_with_multiple_clauses_one_fun_clause,
	  f_spec_lhs_match_expr,
	  f_spec_rhs_match_expr,
	  f_spec_unnamed_pattern,
	  f_spec_bounded_single_clause_fun,
	  f_spec_bounded_multiple_clause_fun,
	  f_spec_bounded_singleton_atom,
	  f_spec_bounded_singleton_int,
	  f_spec_rettype_constraint,
	  f_spec_indirect_constraint,
	  f_spec_arg_type_in_retval,
	  f_redundant_spec,
	  f_only_attr,
	  f_only_tag,
	  f_prefixed_spec,
	  t_only_attr,
	  t_only_tag,
	  t_redundant].

%% TODO: remove these cases once EDoc supports extracting the relevant tags
not_supported() -> [type_since_tag,
		    type_deprecated_tag,
		    cb_since_tag,
		    cb_deprecated_tag].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(edoc_app_should_pass_shell_docs_validation = _CaseName, Config) ->
    case test_server:is_cover() of
        true ->
            {skip,"Test fails when cover compiled"};
        false ->
            {ok, #{ebin := EbinDir} = CopyInfo} =
                copy_application(edoc, ?config(priv_dir, Config)),
            true = code:add_patha(EbinDir),
            [{edoc_copy, CopyInfo} | Config]
    end;
init_per_testcase(CaseName, Config) ->
    case lists:member(CaseName, not_supported()) of
	true ->
	    {skip, "not supported"};
	false ->
	    Config
    end.

end_per_testcase(edoc_app_should_pass_shell_docs_validation = _CaseName, Config) ->
    #{ebin := EbinDir} = ?config(edoc_copy, Config),
    true = code:del_path(EbinDir),
    Config;
end_per_testcase(_CaseName, Config) -> Config.

%%
%% Tests
%%

edoc_app_should_pass_shell_docs_validation(_Config) ->
    ok = edoc:application(edoc, [{preprocess,true},
                                 {doclet, edoc_doclet_chunks},
				 {layout, edoc_layout_chunks},
				 private, hidden]),
    ok = application:load(edoc),
    {ok, Modules} = application:get_key(edoc, modules),
    [ shell_docs:validate(M) || M <- Modules ].

module_anno(Config) ->
    Docs = #docs_v1{} = get_chunk(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual([{file, "eep48_meta.erl"}, {location, 1}],
		 Docs#docs_v1.anno).

function_anno(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual([{file, "eep48_meta.erl"}, {location, 35}],
		 get_anno(function, fun_with_since_tag, 0, Docs)).

type_anno(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual([{file, "eep48_meta.erl"}, {location, 22}],
		 get_anno(type, type_with_since_tag, 0, Docs)).

cb_anno(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual([{file, "eep48_meta.erl"}, {location, 28}],
		 get_anno(callback, cb_with_since_tag, 0, Docs)).

function_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>, get_function_meta_field(since, fun_with_since_tag, 0, Docs) ).

function_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated function.">>,
		 get_function_meta_field(deprecated, fun_with_deprecated_tag, 0, Docs) ).

type_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>, get_type_meta_field(since, type_with_since_tag, 0, Docs) ).

type_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated type.">>,
		 get_type_meta_field(deprecated, type_with_deprecated_tag, 0, Docs) ).

cb_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>,
		 get_callback_meta_field(since, cb_with_since_tag, 0, Docs) ).

cb_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated callback.">>,
		 get_callback_meta_field(deprecated, cb_with_deprecated_tag, 0, Docs) ).

links(Config) ->
    Docs = get_docs(Config, eep48_links),
    %?debugVal(Docs, 1000),
    ?assertEqual({<<"seeerl">>, <<"eep48_links">>},
		 get_doc_link({function, module_link, 0}, Docs)),
    ?assertEqual({<<"seeapp">>, <<"edoc:index">>},
		 get_doc_link({function, app_link, 0}, Docs)),
    ?assertEqual({<<"seeerl">>, <<"edoc:edoc_doclet">>},
		 get_doc_link({function, app_module_link, 0}, Docs)),
    ?assertEqual({<<"seemfa">>, <<"edoc:edoc#files/2">>},
		 get_doc_link({function, app_mfa_link, 0}, Docs)),
    ?assertEqual({<<"seemfa">>, <<"eep48_SUITE#suite/0">>},
		 get_doc_link({function, external_function_link, 0}, Docs)),
    ?assertEqual({<<"seemfa">>, <<"#f/0">>},
		 get_doc_link({function, local_function_link, 0}, Docs)),
    ?assertEqual({<<"seetype">>, <<"#t/0">>},
		 get_doc_link({function, local_type_link, 0}, Docs)),
    ?assertEqual({<<"seetype">>, <<"#t/0">>},
		 get_doc_link({function, local_type_link_macro, 0}, Docs)),
    ?assertEqual({<<"seetype">>, <<"eep48_links#t/0">>},
		 get_doc_link({function, external_type_link, 0}, Docs)).

see_tags_grouped_together(Config) ->
    Docs = get_chunk(Config, eep48_links),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"See also: equiv_target/1, fun_with_equiv_doc_and_see/1.\n">>,
		  ?io2b(string:find(shell_docs:render(eep48_links, ?FUNCTION_NAME, Docs),
				    "See also")) ).

equiv(Config) ->
    Docs = get_docs(Config, eep48_links),
    %?debugVal(Docs, 1000),
    ?assertMatch(<<"Equivalent to equiv_target(ok).">>,
		 get_flat_doc({function, fun_with_equiv_tag, 0}, Docs)),
    ?assertMatch(<<"Equivalent to {<<\"arbitrary\">>, erlang, \"term\"}.">>,
		 get_flat_doc({function, fun_with_non_call_equiv_tag, 0}, Docs)).

f_sig_single_simple_clause(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_single_simple_clause(Arg)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_sig_single_simple_clause_with_spec(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_single_simple_clause_with_spec(Arg)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_sig_multiple_simple_clauses(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_multiple_simple_clauses(C1A1,">>, <<"C1A2)">>],
		 get_sig({function, ?FUNCTION_NAME, 2}, Docs)).

f_sig_multiple_simple_clauses_with_spec(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_multiple_simple_clauses_with_spec(C1A1,">>, <<"C1A2)">>],
		 get_sig({function, ?FUNCTION_NAME, 2}, Docs)).

f_sig_single_record_clause(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_single_record_clause(R)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_sig_single_record_clause_with_spec(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_single_record_clause_with_spec(R)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_sig_multiple_record_clauses(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_multiple_record_clauses(R)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_sig_multiple_record_clauses_with_spec(Config) ->
    Docs = get_docs(Config, eep48_sigs),
    %?debugVal(Docs, 1000),
    ?assertEqual([<<"f_sig_multiple_record_clauses_with_spec(R)">>],
		 get_sig({function, ?FUNCTION_NAME, 1}, Docs)).

f_spec_type_without_name(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_type_without_name(Arg :: atom()) -> ok.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 1}, Docs) ).

f_spec_types_mixed(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_types_mixed(Arg1 :: my_type(), Arg2 :: tuple()) -> ok.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_with_multiple_clauses(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ct:pal("EDoc repeats the first clause's param names for all clauses. "
	   "The actual param names are `(A1, A2); (S, I)'.", []),
    ?assertEqual( <<"-spec f_spec_with_multiple_clauses(A1 :: my_type(), A2 :: atom()) ->\n"
		    "                                      atoms;\n"
		    "                                  (A1 :: string(), A2 :: integer()) ->\n"
		    "                                      not_atoms.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_with_multiple_clauses_one_fun_clause(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_with_multiple_clauses_one_fun_clause(A1 :: my_type(),\n"
		    "                                                  A2 :: atom()) ->\n"
		    "                                                     atoms;\n"
		    "                                                 (A1 :: string(),\n"
		    "                                                  A2 :: integer()) ->\n"
		    "                                                     not_atoms.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_lhs_match_expr(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_lhs_match_expr(Pattern :: any()) -> ok.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 1}, Docs) ).

f_spec_rhs_match_expr(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_rhs_match_expr(Pattern :: any()) -> ok.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 1}, Docs) ).

f_spec_unnamed_pattern(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_unnamed_pattern(_ :: any()) -> ok.\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 1}, Docs) ).

f_spec_bounded_single_clause_fun(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_bounded_single_clause_fun(A, T, S, I) -> ok\n"
		    "                                          when\n"
		    "                                              A :: my_type(),\n"
		    "                                              T :: tuple(),\n"
		    "                                              S :: string(),\n"
		    "                                              I :: integer().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 4}, Docs) ).

f_spec_bounded_multiple_clause_fun(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ct:pal("TODO: This expectation is buggy: the actual param names are `(A1, A2, A3, A4)', "
	   "but EDoc infers them to be `(A1, A2, A3, I)'.", []),
    ?assertEqual( <<"-spec f_spec_bounded_multiple_clause_fun(A1, A2, A3, I) -> ok\n"
		    "                                            when\n"
		    "                                                A1 :: my_type(),\n"
		    "                                                A2 :: tuple(),\n"
		    "                                                A3 :: string(),\n"
		    "                                                I :: integer();\n"
		    "                                        (A1, A2, A3, A) -> ok\n"
		    "                                            when\n"
		    "                                                A1 :: string(),\n"
		    "                                                A2 :: integer(),\n"
		    "                                                A3 :: list(),\n"
		    "                                                A :: atom().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 4}, Docs) ).

f_spec_bounded_singleton_atom(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_bounded_singleton_atom(I, a) -> ok when I :: integer().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_bounded_singleton_int(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_bounded_singleton_int(I, 1) -> ok when I :: integer().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_rettype_constraint(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_rettype_constraint() -> R when R :: atom().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 0}, Docs) ).

f_spec_indirect_constraint(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_indirect_constraint(A, B) -> ok when B :: [A].\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_spec_arg_type_in_retval(Config) ->
    Docs = get_docs(Config, eep48_specs),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_spec_arg_type_in_retval(A, B) -> [A] when B :: atom().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 2}, Docs) ).

f_redundant_spec(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_redundant_spec() -> atom().\n">>,
                  get_pp_spec({function, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertMatch( <<"Function with a redundant spec.">>,
		  get_flat_doc({function, ?FUNCTION_NAME, 0}, Docs) ).

f_only_attr(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec f_only_attr() -> atom().\n">>,
                  get_pp_spec({function, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertMatch( <<"Function with only a spec attribute.">>,
		  get_flat_doc({function, ?FUNCTION_NAME, 0}, Docs) ).

f_only_tag(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertException(error, {badkey, signature},
		     get_pp_spec({function, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertMatch( <<"Function with only a spec tag.">>,
		  get_flat_doc({function, ?FUNCTION_NAME, 0}, Docs) ).

f_prefixed_spec(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-spec eep48_redundant:f_prefixed_spec() -> any().\n">>,
		  get_pp_spec({function, ?FUNCTION_NAME, 0}, Docs) ).

t_only_attr(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-type t_only_attr() :: any().\n">>,
		  get_pp_spec({type, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertEqual( <<"Type t_only_attr defined with an attribute.">>,
		  get_flat_doc({type, ?FUNCTION_NAME, 0}, Docs) ).

t_only_tag(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertException(error, {badkey, signature},
		     get_pp_spec({type, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertEqual( <<"Type t_only_tag defined with a tag.">>,
		  get_flat_doc({type, ?FUNCTION_NAME, 0}, Docs) ).

t_redundant(Config) ->
    Docs = get_docs(Config, eep48_redundant),
    %?debugVal(Docs, 1000),
    ?assertEqual( <<"-type t_redundant() :: any().\n">>,
		  get_pp_spec({type, ?FUNCTION_NAME, 0}, Docs) ),
    ?assertEqual( <<"Type t_redundant defined with an attribute, redundant with a tag.">>,
		  get_flat_doc({type, ?FUNCTION_NAME, 0}, Docs) ).

%%
%% Helpers
%%

get_chunk(Config, M) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, Chunk} = get_doc_chunk(DataDir, PrivDir, M),
    Chunk.

get_docs(Config, M) ->
    Chunk = get_chunk(Config, M),
    Chunk#docs_v1.docs.

get_function_meta_field(Field, F, A, Docs) ->
    get_meta_field(Field, function, F, A, Docs).

get_type_meta_field(Field, T, A, Docs) ->
    get_meta_field(Field, type, T, A, Docs).

get_callback_meta_field(Field, Cb, A, Docs) ->
    get_meta_field(Field, callback, Cb, A, Docs).

get_meta_field(Field, Kind, Name, Arity, Docs) ->
    Meta = get_metadata(lookup_entry(Kind, Name, Arity, Docs)),
    maps:get(Field, Meta).

get_doc_chunk(DataDir, PrivDir, Mod) ->
    TagsErl = filename:join([DataDir, atom_to_list(Mod) ++ ".erl"]),
    edoc:files([TagsErl], [{doclet, edoc_doclet_chunks},
			   {layout, edoc_layout_chunks},
			   {dir, PrivDir}]),
    TagsChunk = filename:join([PrivDir, "chunks", atom_to_list(Mod) ++ ".chunk"]),
    {ok, BChunk} = file:read_file(TagsChunk),
    Chunk = binary_to_term(BChunk),
    {ok, Chunk}.

lookup_entry(Kind, Function, Arity, Docs) ->
    [Entry] = lists:filter(fun({{K, F, A},_Anno,_Sig,_Doc,_Meta})
				 when K =:= Kind andalso F =:= Function, A =:= Arity ->
				   true;
			      (_) ->
				   false
			   end, Docs),
    Entry.

get_metadata({_, _, _, _, Metadata}) -> Metadata.

get_doc_link(KNA, Docs) ->
    D = get_doc(KNA, Docs),
    case lists:foldl(fun F({a, _, _} = E, Acc) ->
                        [E | Acc];
                    F({_E, _, Es}, Acc) when is_list(Es) ->
                        lists:foldl(F, Acc, Es);
                    F(_, Acc) ->
                        Acc
                end, [], D) of
        [{a, Attrs, _}] ->
            <<"https://erlang.org/doc/link/", ShortRel/bytes>> = fetch(rel, Attrs),
            {ShortRel, fetch(href, Attrs)};
        _Else ->
            ct:log("Could not find link in ~p",[D]),
            ct:fail("Did not find link in docs")
    end.

get_anno(Kind, Name, Arity, Docs) ->
    {_, Anno, _, _, _} = lookup_entry(Kind, Name, Arity, Docs),
    Anno.

fetch(K, List) ->
    case lists:keyfind(K, 1, List) of
	false -> erlang:error({not_found, K, List});
	{K, V} -> V
    end.

get_flat_doc(KNA, Docs) ->
    flatten_doc(get_doc(KNA, Docs)).

get_sig({K, N, A}, Docs) ->
    Entry = docs_v1_entry(lookup_entry(K, N, A, Docs)),
    Entry#docs_v1_entry.signature.

get_pp_spec({K, N, A}, Docs) ->
    [Spec] = get_meta_field(signature, K, N, A, Docs),
    iolist_to_binary(erl_pp:attribute(Spec)).

flatten_doc(XML) ->
    iolist_to_binary(lists:reverse(xmerl_lib:foldxml(fun flatten_xml/2, [], XML))).

flatten_xml(T, Acc) when is_binary(T) -> [T | Acc];
flatten_xml({_, _, XML}, Acc) -> xmerl_lib:foldxml(fun flatten_xml/2, Acc, XML).

get_doc({K, N, A}, Docs) ->
    Entry = docs_v1_entry(lookup_entry(K, N, A, Docs)),
    maps:get(<<"en">>, Entry#docs_v1_entry.doc).

copy_application(_App, undefined) ->
    ct:fail("~s: target dir undefined", [?FUNCTION_NAME]);
copy_application(App, TargetDir) ->
    DocDir	= filename:join([TargetDir, App, "doc"]),
    EbinDir	= filename:join([TargetDir, App, "ebin"]),
    IncludeDir	= filename:join([TargetDir, App, "include"]),
    SrcDir	= filename:join([TargetDir, App, "src"]),
    ok = file:make_dir(filename:join([TargetDir, App])),
    ok = file:make_dir(DocDir),
    ok = file:make_dir(EbinDir),
    ok = file:make_dir(IncludeDir),
    ok = file:make_dir(SrcDir),
    copy_app_dir(App, ebin, EbinDir),
    copy_app_dir(App, include, IncludeDir),
    copy_app_dir(App, src, SrcDir),
    {ok, #{ebin => EbinDir, doc => DocDir, src => SrcDir}}.

copy_app_dir(App, Dir, TargetDir) ->
    {ok, Files} = file:list_dir(code:lib_dir(App, Dir)),
    lists:foreach(fun (F) ->
			  file:copy(filename:join(code:lib_dir(App, Dir), F),
				    filename:join(TargetDir, F))
		  end, Files).

docs_v1_entry({KNA, Anno, Sig, Doc, Meta}) ->
    #docs_v1_entry{kind_name_arity = KNA,
		   anno = Anno,
		   signature = Sig,
		   doc = Doc,
		   metadata = Meta}.
