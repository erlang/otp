
-module(beam_doc_SUITE).
-export([all/0, groups/0, init_per_group/2, end_per_group/2, singleton_moduledoc/1, singleton_doc/1,
         docmodule_with_doc_attributes/1, hide_moduledoc/1, docformat/1,
         singleton_docformat/1, singleton_meta/1, slogan/1,
         types_and_opaques/1, callback/1, hide_moduledoc2/1,
         private_types/1, export_all/1, equiv/1, spec/1, deprecated/1, warn_missing_doc/1,
         doc_with_file/1, doc_with_file_error/1, all_string_formats/1,
         docs_from_ast/1, spec_switch_order/1, user_defined_type/1, skip_doc/1,
         no_doc_attributes/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(get_name(), atom_to_list(?FUNCTION_NAME)).

all() ->
    [{group, documentation_generation_tests}, doc_with_file].

groups() ->
    [{documentation_generation_tests, [parallel], documentation_generation_tests()}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

documentation_generation_tests() ->
    [singleton_moduledoc,
     singleton_doc,
     docmodule_with_doc_attributes,
     hide_moduledoc,
     hide_moduledoc2,
     docformat,
     singleton_docformat,
     singleton_meta,
     slogan,
     types_and_opaques,
     callback,
     private_types,
     export_all,
     equiv,
     spec,
     deprecated,
     warn_missing_doc,
     doc_with_file_error,
     all_string_formats,
     spec_switch_order,
     docs_from_ast,
     user_defined_type,
     skip_doc,
     no_doc_attributes
    ].

singleton_moduledoc(Conf) ->
    ModuleName = "singletonmoduledoc",
    {ok, ModName} = default_compile_file(Conf, ModuleName),

    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    {ok, {docs_v1, _,_, Mime,ModuleDoc, _,_}} = code:get_doc(ModName),
    ok.

singleton_doc(Conf) ->
    ModuleName = "singletondoc",
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    Doc = #{<<"en">> => <<"Doc test module">>},
    FooDoc = #{<<"en">> => <<"Tests multi-clauses">>},
    {ok, {docs_v1, 1,_, Mime, none, _,
          [{{function, foo,1},_, [<<"foo(ok)">>], FooDoc, _},
           {{function, main,0},_, [<<"main()">>], Doc, _}]}} = code:get_doc(ModName),
    ok.

docmodule_with_doc_attributes(Conf) ->
    ModuleName = "docmodule_with_doc_attributes",
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    Mime = <<"text/markdown">>,
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Doc = #{<<"en">> => <<"Doc test module">>},
    FileDocs =  #{<<"en">> => <<"# README\n\nThis is a test">>},
    {ok, #docs_v1{ anno = ModuleAnno,
                   beam_language = erlang,
                   format = Mime,
                   module_doc = ModuleDoc,
                   metadata = #{},
                   docs = Docs
                 }} = code:get_doc(ModName),

    
    [{{function,no_docs_multi,1},NoDocsMultiAnno,[<<"no_docs_multi/1">>],none,#{}},
     {{function,with_file_docs,0},FileDocsAnno, [<<"with_file_docs()">>],FileDocs,#{}},
     {{function,no_docs,0},NoDocsAnno, [<<"no_docs()">>],none,#{}},
     {{function,ok,0}, OkAnno, [<<"ok()">>],none,#{authors := "Someone"}},
     {{function, main,_},MainAnno, _, Doc, _}] = Docs,
    
    ?assertEqual(5, erl_anno:line(ModuleAnno)),
    ?assertEqual(10, erl_anno:line(MainAnno)),
    ?assertEqual(18, erl_anno:line(OkAnno)),
    ?assertEqual(21, erl_anno:line(NoDocsAnno)),
    ?assertEqual(1, erl_anno:line(FileDocsAnno)),
    ?assertEqual("README", filename:basename(erl_anno:file(FileDocsAnno))),
    ?assertEqual(28, erl_anno:line(NoDocsMultiAnno)),

    ok.

hide_moduledoc(Conf) ->
    {ok, ModName} = default_compile_file(Conf, "hide_moduledoc"),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function, main, 0}, _, [<<"main()">>],
            #{ <<"en">> := <<"Doc test module">> }, #{}}]}} = code:get_doc(ModName),
    ok.

%% TODO: crashes
hide_moduledoc2(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _Mime, hidden, _,
          [{{function,handle_call,1},{16,2},[<<"handle_call/1">>],hidden,#{}},
           {{function, main, 0}, _, [<<"main()">>], hidden, #{}}]}} = code:get_doc(ModName),
    ok.

docformat(Conf) ->
    {ok, ModName} = default_compile_file(Conf, "docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => "text/asciidoc",
             deprecated => "Use something else",
             otp_doc_vsn => {1,0,0},
             since => "1.0"},
    Doc = #{<<"en">> => <<"Doc test module">>},
    {ok, {docs_v1, _,_, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,_},_, _, Doc, _}]}} = code:get_doc(ModName),
    ok.

singleton_docformat(Conf) ->
    {ok, ModName} = default_compile_file(Conf, "singleton_docformat"),
    ModuleDoc = #{<<"en">> => <<"Moduledoc test module">>},
    Meta = #{format => <<"text/asciidoc">>,
             deprecated => ~"Use something else",
             otp_doc_vsn => {1,0,0},
             since => ~"1.0"},
    Doc = #{<<"en">> => <<"Doc test module\n\nMore info here">>},
    FunMeta = #{ authors => [<<"Beep Bop">>], equiv => <<"main/3">>, since => ~"1.0" },
    {ok, {docs_v1, _,erlang, <<"text/asciidoc">>, ModuleDoc, Meta,
          [{{function, main,0},_, [<<"main()">>], Doc, FunMeta}]}} = code:get_doc(ModName),
    ok.

singleton_meta(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    Meta = #{ authors => [<<"Beep Bop">>], equiv => <<"main/3">>, since => ~"1.0" },
    DocMain1 = #{<<"en">> => <<"Returns always ok.">>},
    {ok, {docs_v1, _,erlang, <<"text/markdown">>, none, #{ since := ~"1.0" },
          [{{function, main1,0},_, [<<"main1()">>], DocMain1, #{equiv := <<"main(_)">>,
                                                                since := ~"1.1"}},
           {{function, main,0},_, [<<"main()">>], none, Meta}]}}
        = code:get_doc(ModName),
    ok.

slogan(Conf) ->
  ModuleName = ?get_name(),
  {ok, ModName} = default_compile_file(Conf, ModuleName),
  Doc = #{<<"en">> => <<"Returns ok.">>},
  BarDoc = #{ <<"en">> => <<"foo()\nNot a slogan since foo =/= bar">> },
  NoSloganDoc = #{ <<"en">> => <<"Not a slogan\n\nTests slogans in multi-clause">>},
  {ok, {docs_v1, _,_, _, none, _,
          [Connect, MulticlauseSloganIgnored, SpecNoDocSlogan, NoDocSlogan,
           Slogan2, Slogan1, NoSlogan, Bar, Main]}} = code:get_doc(ModName),

  {{function,connect,2},_,
   [<<"connect(TCPSocket, TLSOptions)">>],none,#{equiv := <<"connect/3">>,since := <<"OTP R14B">>}} = Connect,
  {{function,spec_multiclause_slogan_ignored,1},_,[<<"spec_multiclause_slogan_ignored(X)">>],none,#{}} = MulticlauseSloganIgnored,
  {{function, spec_no_doc_slogan, 1}, _, [<<"spec_no_doc_slogan(Y)">>], none, #{}} = SpecNoDocSlogan,
  {{function, no_doc_slogan, 1}, _, [<<"no_doc_slogan(X)">>], none, #{}}= NoDocSlogan,
  {{function, spec_slogan, 2}, _, [<<"spec_slogan(Y, Z)">>], _, #{}} = Slogan2,
  {{function, spec_slogan, 1}, _, [<<"spec_slogan(Y)">>], _, #{}} = Slogan1,
  {{function, no_slogan,1},_,[<<"no_slogan/1">>], NoSloganDoc, #{}} = NoSlogan,
  {{function, bar,0},_,[<<"bar()">>], BarDoc, #{}} = Bar,
  {{function, main,1},_,[<<"main(Foo)">>], Doc, #{}} = Main,
  ok.

types_and_opaques(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName, Warnings} = default_compile_file(Conf, ModuleName, [return_warnings]),
    TypeDoc = #{<<"en">> => <<"Represents the name of a person.">>},
    GenericsDoc = #{<<"en">> => <<"Tests generics">>},
    OpaqueDoc = #{<<"en">> =>
                      <<"Represents the name of a person that cannot be named.">>},
    MaybeOpaqueDoc = #{<<"en">> => <<"mmaybe(X) ::= nothing | X.\n\nRepresents a maybe type.">>},
    MaybeMeta = #{ authors => "Someone else", exported => true },
    NaturalNumberMeta = #{since => "1.0", equiv => <<"non_neg_integer/0">>, exported => true},

    {ok, {docs_v1, _,_, _, none, _,
          [%% Type Definitions
           Public, Intermediate, HiddenNoWarnType, HiddenType, OtherPrivateType, MyPrivateType,
           MyMap, StateEnter, CallbackMode,CallbackResult, EncodingFunc, Three,
           Two, One, Hidden, HiddenFalse, MMaybe, Unnamed, Param,NatNumber, Name,
           HiddenIncludedType,
           %% Functions
           UsesPublic, Ignore, MapFun, PrivateEncoding, Foo
          ]}} = code:get_doc(ModName),

    {{type,public,0},{125,2},[<<"public()">>],none,#{exported := true}} = Public,
    {{type,intermediate,0},{124,2},[<<"intermediate()">>],none,#{exported := false}} = Intermediate,
    {{type,hidden_nowarn_type,0},{120,2},[<<"hidden_nowarn_type()">>],hidden,#{exported := false}} = HiddenNoWarnType,
    {{type,hidden_type,0},{117,2},[<<"hidden_type()">>],hidden,#{exported := false}} = HiddenType,
    {{type,my_other_private_type,0},MyOtherPrivateTypeLine,
              [<<"my_other_private_type()">>],none,#{exported := false}} = OtherPrivateType,
    {{type,my_private_type,0},MyPrivateTypeLine,
     [<<"my_private_type()">>],none,#{exported := false}} = MyPrivateType,
    {{type,mymap,0},MyMapLine,[<<"mymap()">>],none,#{exported := false}} = MyMap,
    {{type,state_enter,0},StateEnterLine,[<<"state_enter()">>],none,#{exported := false}}=StateEnter,
    {{type,callback_mode,0},CallbackModeLine, [<<"callback_mode()">>],none,#{exported := false}} = CallbackMode,
    {{type,callback_mode_result,0},CallbackResultLine,
               [<<"callback_mode_result()">>],none,#{exported := true}} = CallbackResult,
    {{type,encoding_func,0},_,[<<"encoding_func()">>],none,#{exported := false}} = EncodingFunc,
    {{type,three,0},_,[<<"three()">>],none,#{exported := false}} = Three,
    {{type,two,0},_,[<<"two()">>],none,#{exported := false}} = Two,
    {{type,one,0},_,[<<"one()">>],none,#{exported := false}} = One,
    {{type,hidden,0},_,[<<"hidden()">>],hidden,#{exported := true}} = Hidden,
    {{type,hidden_false,0},_,[<<"hidden_false()">>],hidden,
     #{exported := true, authors := "Someone else"}} = HiddenFalse,
    {{type, mmaybe,1},_,[<<"mmaybe(X)">>], MaybeOpaqueDoc, MaybeMeta} = MMaybe,
    {{type, unnamed,0},{30,2},[<<"unnamed()">>], OpaqueDoc,
     #{equiv := <<"non_neg_integer()">>, exported := true}} = Unnamed,
    {{type, param,1},_,[<<"param(X)">>], GenericsDoc,
     #{equiv := <<"madeup()">>, exported := true}} = Param,
    {{type, natural_number,0},_,[<<"natural_number()">>], none, NaturalNumberMeta} = NatNumber,
    {{type, name,1},_,[<<"name(_)">>], TypeDoc, #{exported := true}} = Name,
    {{type, hidden_included_type, 0}, _, _, hidden, #{exported := false }} = HiddenIncludedType,

    {{function,uses_public,0},{128,1},[<<"uses_public()">>],none,#{}} = UsesPublic,
    {{function,ignore_type_from_hidden_fun,0},_,[<<"ignore_type_from_hidden_fun()">>],hidden,#{}} = Ignore,
    {{function,map_fun,0},_,[<<"map_fun()">>],none,#{}} = MapFun,
    {{function,private_encoding_func,2},_,[<<"private_encoding_func(Data, Options)">>],none,#{}} = PrivateEncoding,
    {{function,foo,0},_,[<<"foo()">>],none,#{}} = Foo,

    ?assertEqual(103, erl_anno:line(MyOtherPrivateTypeLine)),
    ?assertEqual(102, erl_anno:line(MyPrivateTypeLine)),
    ?assertEqual(99, erl_anno:line(MyMapLine)),
    ?assertEqual(96, erl_anno:line(StateEnterLine)),
    ?assertEqual(95, erl_anno:line(CallbackModeLine)),
    ?assertEqual(93, erl_anno:line(CallbackResultLine)),

    [{File, Ws}, {HrlFile, HrlWs}] = Warnings,
    ?assertEqual("types_and_opaques.erl", filename:basename(File)),
    ?assertEqual({{117,2}, beam_doc,
                  {hidden_type_used_in_exported_fun,{hidden_type,0}}}, lists:nth(4, Ws)),

    ?assertEqual("types_and_opaques.hrl", filename:basename(HrlFile)),
    ?assertEqual({{1,2}, beam_doc,
                  {hidden_type_used_in_exported_fun,{hidden_included_type,0}}}, lists:nth(1, HrlWs)),

    {ok, ModName, [_]} =
        default_compile_file(Conf, ModuleName, [return_warnings, nowarn_hidden_doc, nowarn_unused_type]),

    ok.

callback(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName, [{File,Warnings}]} =
        default_compile_file(Conf, ModuleName, [return_warnings, report]),
    Doc = #{<<"en">> => <<"Callback fn that always returns ok.">>},
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    ChangeOrder = #{<<"en">> => <<"Test changing order">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{callback,nowarn,1},{39,2},[<<"nowarn(Arg)">>],hidden,#{}},
           {{callback,warn,0},{36,2},[<<"warn()">>],hidden,#{}},
           {{callback,bounded,1},_,[<<"bounded(X)">>],none,#{}},
           {{callback,multi,1},_,[<<"multi(Argument)">>],
            #{ <<"en">> := <<"A multiclause callback with slogan docs">> },#{}},
           {{callback,multi_no_slogan,1},_,[<<"multi_no_slogan/1">>],none,#{}},
           {{callback,ann,1},_,[<<"ann(X)">>],none,#{}},
           {{callback,param,1},_,[<<"param(X)">>],none,#{}},
           {{callback, change_order,0},_,[<<"change_order()">>], ChangeOrder,
            #{equiv := <<"ok()">>}},
           {{callback, all_ok,0},_,[<<"all_ok()">>], Doc, #{}},
           {{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>},
            #{equiv := <<"main()">>}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback,
            #{equiv := <<"ok/0">>}}
          ]}} = code:get_doc(ModName),

    ?assertEqual("callback.erl", filename:basename(File)),
    io:format("Warnings: ~p~n", [Warnings]),
    ?assertEqual(1, length(Warnings)),
    ?assertMatch({{36,2},beam_doc,{hidden_callback,{warn,0}}}, lists:nth(1, Warnings)),

    {ok, ModName, []} =
        default_compile_file(Conf, ModuleName, [return_warnings, report, nowarn_hidden_doc]),

    ok.

private_types(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),

    {ok, {docs_v1, _,_, _, none, _,
          [
           %% Types
           RemoteTypeT, TupleT, RecordAT, RecordInlineT,
           MapValue2T, MapKey2T, MapValueT, MapKeyT,
           FunRet2T, FunRetT, FunT, Complex, BoundedRetT,
           ArgT, BoundedArgT, Private, HiddenExportT, PrivateCBT,
           OpaqueT, PublicT, PrivateT,
           %% Callbacks
           CBar,
           %% Functions
           Bounded, HiddenTypeExposed, Hidden, Bar]}} = code:get_doc(ModName),

    ?assertMatch({{type,remote_type_t,1}, _, _, none, #{exported := false}},RemoteTypeT),
    ?assertMatch({{type,tuple_t,0}, _, _, none, #{exported := false}},TupleT),
    ?assertMatch({{type,record_a_t,0}, _, _, none, #{exported := false}},RecordAT),
    ?assertMatch({{type,record_inline_t,0}, _, _, none, #{exported := false}},RecordInlineT),
    ?assertMatch({{type,map_value_2_t,0}, _, _, none, #{exported := false}},MapValue2T),
    ?assertMatch({{type,map_key_2_t,0}, _, _, none, #{exported := false}},MapKey2T),
    ?assertMatch({{type,map_value_t,0}, _, _, none, #{exported := false}},MapValueT),
    ?assertMatch({{type,map_key_t,0}, _, _, none, #{exported := false}},MapKeyT),
    ?assertMatch({{type,fun_ret_2_t,0}, _, _, none, #{exported := false}},FunRet2T),
    ?assertMatch({{type,fun_ret_t,0}, _, _, none, #{exported := false}},FunRetT),
    ?assertMatch({{type,fun_t,0}, _, _, none, #{exported := false}},FunT),
    ?assertMatch({{type,complex,1}, _, _, none, #{exported := true}},Complex),
    ?assertMatch({{type,bounded_ret_t,0}, _, _, none, #{exported := false}},BoundedRetT),
    ?assertMatch({{type,arg_t,0}, _, _, none, #{exported := false}},ArgT),
    ?assertMatch({{type,bounded_arg_t,0}, _, _, none, #{exported := false}},BoundedArgT),
    ?assertMatch({{type,private,0}, {30,2}, [<<"private()">>], hidden, #{exported := false}},Private),
    ?assertMatch({{type,hidden_export_t,0},_,[<<"hidden_export_t()">>],hidden,#{exported := true}},HiddenExportT),
    ?assertMatch({{type,private_cb_t,0},_,_,none,#{exported := false}},PrivateCBT),
    ?assertMatch({{type,opaque_t,0},_, [<<"opaque_t()">>], none,#{ exported := true}},OpaqueT),
    ?assertMatch({{type,public_t,0},_, [<<"public_t()">>], none,#{ exported := true}},PublicT),
    ?assertMatch({{type,private_t,0},_, [<<"private_t()">>], none,#{ exported := false}},PrivateT),
    ?assertMatch({{callback,bar,1},_,_,none,#{}},CBar),
    ?assertMatch({{function,bounded,2},_,_,none,#{}},Bounded),
    ?assertMatch({{function,hidden_type_exposed,0},{34,1},[<<"hidden_type_exposed()">>],none,#{}},HiddenTypeExposed),
    ?assertMatch({{function,hidden,0},_,[<<"hidden()">>],hidden,#{}},Hidden),
    ?assertMatch({{function,bar,0},_,[<<"bar()">>],none,#{}},Bar),

    ok.


export_all(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    ImpCallback = #{<<"en">> => <<"This is a test">>},
    FunctionDoc = #{<<"en">> => <<"all_ok()\n\nCalls all_ok/0">>},
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main2,0},_,[<<"main2()">>], #{<<"en">> := <<"Second main">>},
            #{equiv := <<"main()">>}},
           {{function, main,0},_,[<<"main()">>], FunctionDoc, #{}},
           {{function, all_ok,0},_, [<<"all_ok()">>],ImpCallback,
            #{equiv := <<"ok/0">>}}
          ]}} = code:get_doc(ModName),
    ok.

equiv(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, none, _,
          [{{function, main, 2},_,[<<"main(A, B)">>], none,
            #{ }},
            {{function, main, 1},_,[<<"main(A)">>], none,
             #{ equiv := <<"main(A, 1)">> }}
          ]}} = code:get_doc(ModName),
    ok.

spec(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, #{ ~"en" := ~"" }, _,
          [{{type,no,0},_,[<<"no()">>],none,#{exported := false}},
           {{type,yes,0},_,[<<"yes()">>],none,#{exported := false}},
           {{callback,me,1},_,[<<"me/1">>],none,#{}},
           {{function,baz,1},_,[<<"baz(X)">>],none,#{}},
           {{function,foo,1},_,[<<"foo(X)">>],none,#{}}]}} = code:get_doc(ModName),
    ok.

user_defined_type(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, #{ ~"en" := ~"" }, _, []}} = code:get_doc(ModName),
    ok.

deprecated(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the type deprecated:test(_) is deprecated; Deprecation reason">>}},
           {{type,test,0},_,[<<"test()">>],none,#{deprecated := <<"the type deprecated:test() is deprecated; see the documentation for details">>}},
           {{callback,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the callback deprecated:test(_) is deprecated; Deprecation reason">>}},
           {{callback,test,0},_,[<<"test()">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,2},_,[<<"test(N, M)">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"deprecated:test/1 is deprecated; Deprecation reason">>}},
           {{function,test,0},_,[<<"test()">>],none,#{deprecated := <<"deprecated:test/0 is deprecated; see the documentation for details">>}}]}} =
        code:get_doc(ModName),

    {ok, ModName} = default_compile_file(Conf, ModuleName, [{d,'TEST_WILDCARD'},
                                                            {d, 'REASON', next_major_release}]),
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the type deprecated:test(_) is deprecated; see the documentation for details">>}},
           {{type,test,0},_,[<<"test()">>],none,#{deprecated := <<"the type deprecated:test() is deprecated; see the documentation for details">>}},
           {{callback,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the callback deprecated:test(_) is deprecated; will be removed in the next major release. See the documentation for details">>}},
           {{callback,test,0},_,[<<"test()">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,2},_,[<<"test(N, M)">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"deprecated:test/1 is deprecated; will be removed in the next major release. See the documentation for details">>}},
           {{function,test,0},_,[<<"test()">>],none,#{deprecated := <<"deprecated:test/0 is deprecated; see the documentation for details">>}}]}} =
        code:get_doc(ModName),

    {ok, ModName} = default_compile_file(Conf, ModuleName, [{d,'ALL_WILDCARD'},
                                                            {d,'REASON',next_version},
                                                            {d,'TREASON',eventually}]),
    {ok, {docs_v1, _,_, _, none, _,
          [{{type,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the type deprecated:test(_) is deprecated; will be removed in a future release. See the documentation for details">>}},
           {{type,test,0},_,[<<"test()">>],none,#{deprecated := <<"the type deprecated:test() is deprecated; see the documentation for details">>}},
           {{callback,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"the callback deprecated:test(_) is deprecated; will be removed in the next version. See the documentation for details">>}},
           {{callback,test,0},_,[<<"test()">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,2},_,[<<"test(N, M)">>],none,#{deprecated := <<"Meta reason">>}},
           {{function,test,1},_,[<<"test(N)">>],none,#{deprecated := <<"deprecated:test/1 is deprecated; will be removed in the next version. See the documentation for details">>}},
           {{function,test,0},_,[<<"test()">>],none,#{deprecated := <<"deprecated:test/0 is deprecated; see the documentation for details">>}}]}} =
        code:get_doc(ModName),
    ok.

warn_missing_doc(Conf) ->

    warn_missing_doc(Conf, [function, type, callback], [warn_missing_doc]),
    warn_missing_doc(Conf, [function], [warn_missing_doc_functions]),
    warn_missing_doc(Conf, [function, type], [warn_missing_doc_functions, warn_missing_doc_types]),
    warn_missing_doc(Conf, [type, callback], [warn_missing_doc_types, warn_missing_doc_callbacks]),
    warn_missing_doc(Conf, [callback], [warn_missing_doc_callbacks]),

    warn_missing_doc(Conf, [type, callback], [warn_missing_doc, nowarn_missing_doc_functions]),
    warn_missing_doc(Conf, [function, callback], [warn_missing_doc, nowarn_missing_doc_types]),
    warn_missing_doc(Conf, [type], [warn_missing_doc, nowarn_missing_doc_callbacks, nowarn_missing_doc_functions]),
    warn_missing_doc(Conf, [], [warn_missing_doc_functions, nowarn_missing_doc]),

    ok.

warn_missing_doc(Conf, ExpectedWarnings, Options) ->
    ModuleName = ?get_name(),
    {ok, ModName, Ws} =
        default_compile_file(Conf, ModuleName, [return_warnings, report | Options]),

    {ok, {docs_v1, _,_, _, none, _,
          [{{type,test,1},_,[<<"test(N)">>],none,_},
           {{type,test,0},_,[<<"test()">>],none,_},
           {{callback,test,0},_,[<<"test()">>],none,_},
           {{function,test,1},_,[<<"test(N)">>],none,_},
           {{function,test,0},_,[<<"test()">>],none,_},
           {{function,test,2},_,[<<"test(N, M)">>],none,_}]}
    } = code:get_doc(ModName),

    case ExpectedWarnings of
        [] ->
            ?assertEqual([],Ws);
        _ ->
            [{File,Warnings} | Hrl] = Ws,
            ExpectedWarningCount = 1 + lists:sum(
                                         lists:flatten(
                                           [[2 || lists:member(type, ExpectedWarnings)],
                                            [1 || lists:member(callback, ExpectedWarnings)],
                                            [2 || lists:member(function, ExpectedWarnings)]])),

            ?assertEqual("warn_missing_doc.erl", filename:basename(File)),
            ?assertEqual(ExpectedWarningCount, length(Warnings)),
            ?assertMatch({1, beam_doc, missing_moduledoc}, lists:nth(1, Warnings)),
            TypePos =
                case lists:member(type, ExpectedWarnings) of
                    true ->
                        ?assertMatch({{6,2}, beam_doc, {missing_doc, {type,test,0}}}, lists:nth(2, Warnings)),
                        ?assertMatch({{7,2}, beam_doc, {missing_doc, {type,test,1}}}, lists:nth(3, Warnings)),
                        4;
                    false ->
                        2
                end,

            CBPos =
                case lists:member(callback, ExpectedWarnings) of
                    true ->
                        ?assertMatch({{9,2}, beam_doc, {missing_doc, {callback,test,0}}}, lists:nth(TypePos, Warnings)),
                        TypePos + 1;
                    false ->
                        TypePos
                end,

            case lists:member(function, ExpectedWarnings) of
                true ->
                    ?assertMatch({{13,1}, beam_doc, {missing_doc, {function,test,0}}}, lists:nth(CBPos, Warnings)),
                    ?assertMatch({{14,1}, beam_doc, {missing_doc, {function,test,1}}}, lists:nth(CBPos+1, Warnings)),
                    [{HrlFile, HrlWarnings}] = Hrl,
                    ?assertEqual("warn_missing_doc.hrl", filename:basename(HrlFile)),
                    ?assertEqual(1, length(HrlWarnings)),
                    ?assertMatch({{2,1}, beam_doc, {missing_doc, {function,test,2}}}, lists:nth(1, HrlWarnings));
                false ->
                    ok
            end
    end.

doc_with_file(Conf) ->
    ModuleName = ?get_name(),
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(proplists:get_value(data_dir, Conf)),
        {ok, ModName} = default_compile_file(Conf, ModuleName, [{i, "./folder"}]),
        {ok, {docs_v1, ModuleAnno,_, _, #{<<"en">> := <<"# README\n\nThis is a test">>}, _,
              [{{type,bar,1},_,[<<"bar(X)">>],none,#{exported := false}},
               {{type,foo,1},_,[<<"foo(X)">>],none,#{exported := true}},
               {{type,private_type_exported,0},_,[<<"private_type_exported()">>],
                #{<<"en">> := <<"# TYPES\n\nTest">>}, #{exported := false}},
               {{function,main2,1},Main2Anno,[<<"main2(I)">>],
                #{<<"en">> := <<"# File\n\ntesting fetching docs from other folders">>}, #{}},
               {{function,main,1},_,[<<"main(Var)">>],
                #{<<"en">> := <<"# Fun\n\nTest importing function">>},#{}}]}} = code:get_doc(ModName),

        ?assertEqual(1, erl_anno:line(ModuleAnno)),
        ?assertEqual(1, erl_anno:line(Main2Anno)),
        ?assertEqual("./folder/FILE", erl_anno:file(Main2Anno)),
        ok
    after
        ok = file:set_cwd(Cwd)
    end.

doc_with_file_error(Conf) ->
    ModuleName = ?get_name(),
    {error,
     [{_,
       [{{6,2},epp,{moduledoc,file,"doesnotexist"}},
        {{8,2},epp,{doc,file,"doesnotexist"}},
        {{11,2},epp,{doc,file,"doesnotexist"}}]}] = Errors, []} = default_compile_file(Conf, ModuleName),
    [[Mod:format_error(Error) || {_Loc, Mod, Error} <- Errs] || {_File, Errs} <- Errors],
    {error, _, []} = default_compile_file(Conf, ModuleName, [report]),
    ok.

all_string_formats(Conf) ->
    ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),

    {ok, {docs_v1, _ModuleAnno,_, _, #{<<"en">> := <<"Moduledoc test module">>}, _,
              [
               {{function,six,0},_,_, #{<<"en">> := <<"all_string_formats-all_string_formats">>}, #{}},
               {{function,five,0},_,_, #{<<"en">> := <<"all_string_formats-Doc module">>}, #{}},
               {{function,four,0},_,_, #{<<"en">> := <<"Doc test mödule"/utf8>>}, #{}},
               {{function,three,0},_,_, #{<<"en">> := <<"Doctestmodule">>}, #{}},
               {{function,two,0},_,_, #{<<"en">> := <<"Doc test module">>}, #{}},
               {{function,one,0},_,_, #{<<"en">> := <<"Doc test module">>}, #{}}
              ]}} = code:get_doc(ModName),
    ok.

spec_switch_order(Conf) ->
  ModuleName = ?get_name(),
    {ok, ModName} = default_compile_file(Conf, ModuleName),

    {ok, {docs_v1, _ModuleAnno,_, _, _, _,
          [NotFalse, Other, Bar, Foo]}} = code:get_doc(ModName),
  {{function,not_false,0}, {52,1}, [<<"not_false()">>], none,#{}} = NotFalse,
  {{function,other,0},{36,2},[<<"other()">>],hidden,#{}} = Other,
  {{function,bar,1},{30,2},[<<"bar(X)">>],hidden,#{}} = Bar,
  {{function,foo,1}, {22, 2}, [<<"foo(Var)">>], #{ <<"en">> := <<"Foo does X">> }, #{}} = Foo.

skip_doc(Conf) ->
  ModuleName =?get_name(),
  {ok, ModName} = default_compile_file(Conf, ModuleName, [no_docs]),

  {ok,{docs_v1,0,erlang,<<"application/erlang+html">>,none,
     #{generated := true,
       otp_doc_vsn := {1,0,0}},
       [{{function,main,0},{8,1},[<<"main/0">>],none,#{}},
        {{function,foo,1},{16,1},[<<"foo/1">>],none,#{}}]}} = code:get_doc(ModName),

  {error, missing} =
      code:get_doc(ModName, #{ sources => [eep48] }),

  {ok, _ModName} = compile_file(Conf, ModuleName, [report, return_errors, no_docs]),
  {error, missing} = code:get_doc(ModName),
  ok.

no_doc_attributes(Conf) ->
  ModuleName =?get_name(),
  {ok, ModName} = default_compile_file(Conf, ModuleName, []),

  {error, missing} =
      code:get_doc(ModName, #{ sources => [eep48] }),

  ok.

docs_from_ast(_Conf) ->
    Code = """
      -module(test).
      -moduledoc "moduledoc".
      -export([main/0]).
      -doc "main".
      main() -> ok.
      """,

    {ok, test, BeamCode} = compile:forms(scan_and_parse(Code),[debug_info]),
    {ok, {test, [{documentation, Docs }]}} = beam_lib:chunks(BeamCode, [documentation]),

    ?assertMatch(
       #docs_v1{ module_doc = #{ <<"en">> := <<"moduledoc">> },
                 anno = 2,
                 docs = [{{function,main,0}, 4, _, #{ <<"en">> := <<"main">> }, _}]},
       Docs),

    check_no_doc_attributes(BeamCode),

    {ok, test, BeamCodeWSource} = compile:forms(scan_and_parse(Code),[beam_docs, debug_info, {source, "test.erl"}]),
    {ok, {test, [{documentation, DocsWSource }]}} = beam_lib:chunks(BeamCodeWSource, [documentation]),

    ?assertMatch(
       #docs_v1{ module_doc = #{ <<"en">> := <<"moduledoc">> },
                 anno = 2,
                 docs = [{{function,main,0}, 4,
                          _, #{ <<"en">> := <<"main">> }, _}]},
       DocsWSource),
    check_no_doc_attributes(BeamCodeWSource),
    ok.

scan_and_parse(Code) ->
    {ok, Toks, _} = erl_scan:string(Code),
    parse(Toks).

parse([]) -> [];
parse(Toks) ->
    {Form, [Dot | Rest]} = lists:splitwith(fun(E) -> element(1,E) =/= dot end, Toks),
    {ok, F} = erl_parse:parse_form(Form ++ [Dot]),
    [F | parse(Rest)].

compile_file(Conf, ModuleName, ExtraOpts) ->
    ErlModName = ModuleName ++ ".erl",
    Filename = filename:join(proplists:get_value(data_dir, Conf), ErlModName),
    io:format("Compiling: ~ts~n~p~n",[Filename, ExtraOpts]),
    case compile:file(Filename, ExtraOpts) of
        Res when element(1, Res) =:= ok ->
            ModName = element(2, Res),
            case lists:search(fun (X) -> X =:= no_docs end, ExtraOpts) of
              false when length(ExtraOpts) > 0 ->
                check_no_doc_attributes(code:which(ModName)),
                Res;
              _ ->
                Res
            end;
        Else ->
            Else
    end.

default_compile_file(Conf, ModuleName) ->
  default_compile_file(Conf, ModuleName, []).
default_compile_file(Conf, ModuleName, ExtraOpts) ->
  compile_file(Conf, ModuleName, [report, return_errors, debug_info] ++ ExtraOpts).


%% Verify that all doc and moduledoc attributes are stripped from debug_info
check_no_doc_attributes(Mod) ->
    {ok, {_ModName,
          [{debug_info,
            {debug_info_v1,erl_abstract_code,
             {AST, Opts}}}]}} = beam_lib:chunks(Mod, [debug_info]),
    false = lists:search(
              fun(E) ->
                      element(1,E) == attribute
                          andalso
                            (element(3,E) == doc orelse element(3,E) == moduledoc)
              end, AST),
    false = lists:member(no_docs, Opts),
    ok.
