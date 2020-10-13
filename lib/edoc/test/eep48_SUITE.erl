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
	 equiv/1]).

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
	  equiv].

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
    {ok, #{ebin := EbinDir} = CopyInfo} = copy_application(edoc, ?config(priv_dir, Config)),
    true = code:add_patha(EbinDir),
    [{edoc_copy, CopyInfo} | Config];
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
    ok = edoc:application(edoc, [{doclet, edoc_doclet_chunks},
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
    ?assertEqual({<<"seetype">>, <<"eep48_links#t/0">>},
		 get_doc_link({function, external_type_link, 0}, Docs)).

equiv(Config) ->
    Docs = get_docs(Config, eep48_links),
    %?debugVal(Docs, 1000),
    ?assertMatch(<<"Equivalent to equiv_target(ok).">>,
		 get_flat_doc({function, fun_with_equiv_tag, 0}, Docs)),
    ?assertMatch(<<"Equivalent to {<<\"arbitrary\">>, erlang, \"term\"}.">>,
		 get_flat_doc({function, fun_with_non_call_equiv_tag, 0}, Docs)).

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

lookup_function(F, A, Docs) -> lookup_entry(function, F, A, Docs).

lookup_type(T, A, Docs) -> lookup_entry(type, T, A, Docs).

lookup_callback(Cb, A, Docs) -> lookup_entry(callback, Cb, A, Docs).

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
    [Link] = [ Node || {a, _, _} = Node <- get_doc(KNA, Docs) ],
    {a, Attrs, _} = Link,
    <<"https://erlang.org/doc/link/", ShortRel/bytes>> = fetch(rel, Attrs),
    {ShortRel, fetch(href, Attrs)}.

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
