%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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
%%
-module(edlin_expand_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         init_per_group/2,end_per_group/2]).
-export([normal/1, type_completion/1, quoted_fun/1, quoted_module/1, quoted_both/1,
         invalid_module/1, erl_1152/1, get_coverage/1, check_trailing/1, unicode/1,
         filename_completion/1, binding_completion/1, record_completion/1, no_completion/1,
         map_completion/1, function_parameter_completion/1, fun_completion/1]).
-record(a_record,
        {a_field   :: atom1 | atom2 | btom | 'my atom' | {atom3, {atom4, non_neg_integer()}} | 'undefined',
         b_field   :: boolean() | 'undefined',
         c_field   :: list(term()) | 'undefined',
         d_field   :: non_neg_integer() | 'undefined'}).
-record('Quoted_record',
        {'A_field'   :: atom1 | atom2 | btom | 'my atom' | {atom3, {atom4, non_neg_integer()}} | 'undefined',
         b_field   :: boolean() | 'undefined',
         c_field   :: list(term()) | 'undefined',
         d_field   :: non_neg_integer() | 'undefined'}).
-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    cleanup(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [normal, filename_completion, binding_completion, get_coverage, type_completion, 
     record_completion, fun_completion, map_completion, function_parameter_completion,
     no_completion, quoted_fun, quoted_module, quoted_both, erl_1152, check_trailing,
     invalid_module, unicode].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    cleanup(),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

cleanup() ->
    [try
         code:purge(M),
         code:delete(M)
     catch _:_ -> ok end || M <- [expand_test, expand_test1, expand_function_parameter,
                                  'ExpandTestCaps', 'ExpandTestCaps1',
                                complete_function_parameter]].

normal(Config) when is_list(Config) ->
    {module,expand_test} = compile_and_load(Config,expand_test),
    %% These tests might fail if another module with the prefix
    %% "expand_" happens to also be loaded.
    {yes,"test:",[]} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],[#{
               title:="functions",
               elems:=[{"a_fun_name",[{ending,"("}]},
                        {"a_less_fun_name",_},
                        {"b_comes_after_a",_},
                        {"expand0arity_entirely",_},
                        {"module_info",_}]
              }]} = do_expand("expand_test:"),
    {yes,[],[#{title:="functions",
                      elems:=[{"a_fun_name",_},{"a_less_fun_name",_}]}]} = do_expand("expand_test:a_"),
    {yes,"arity_entirely()",[]} = do_expand("expand_test:expand0"),
    ok.

to_atom(Str) ->
    case erl_scan:string(Str) of
        {ok, [{atom,_,A}], _} ->
            {ok, A};
        _ ->
            error
    end.

type_completion(_Config) ->
    ct:timetrap({minutes, 20}),
    {Time,_} = timer:tc(fun() -> do_expand("erl_pp:expr(") end),
    case Time of
        Time when Time > 5000000 ->
            {skip, lists:flatten(io_lib:format("Expansion too slow (~p) on this machine",[Time]))};
        _ ->
            parallelforeach(
            fun(Mod) ->
                    Exports = edlin_expand:get_exports(Mod),
                    [try
                        Str = io_lib:write_atom(Mod) ++ ":" ++ io_lib:write_atom(Func) ++ "(",
                        do_expand(Str)
                    catch E:R:ST ->
                            erlang:raise(E, {R, Mod, Func}, ST)
                    end || {Func, _}<- Exports]
            end, [list_to_atom(M) || {M,_,_} <- code:all_available()]),
            ok
    end.

parallelforeach(Fun, List) ->
    case parallelforeach(Fun, List, #{}, erlang:system_info(schedulers_online)) of
        [] -> ok;
        Else -> ct:fail(Else)
    end.
parallelforeach(_Fun, [], Workers, _MaxWorkers) when map_size(Workers) =:= 0 ->
    [];
parallelforeach(Fun, List, Workers, MaxWorkers) when MaxWorkers =:= map_size(Workers);
                                                     List =:= [] ->
    receive
        {'DOWN', Ref, _, _, normal} ->
            parallelforeach(Fun, List, maps:remove(Ref, Workers), MaxWorkers);

        {'DOWN', Ref, _, _, Reason} ->
            {Arg, NewWorkers} = maps:take(Ref, Workers),
            [{Arg, Reason} | parallelforeach(Fun, List, NewWorkers, MaxWorkers)]
    end;
parallelforeach(Fun, [H|T], Workers, MaxWorkers) ->
    {_Pid, Ref} = spawn_monitor(fun() -> Fun(H) end),
    parallelforeach(Fun, T, Workers#{ Ref => H }, MaxWorkers).

filename_completion(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, CWD} = file:get_cwd(),
    file:set_cwd(DataDir),
    {yes,"e\"",[]} = do_expand("\"visible\\ fil"),
    {no,[],[]} = do_expand("\"visible fil"),
    {no,[],[]} = do_expand("\" "),
    {no,[],[]} = do_expand("\"\""),
    {yes, "e\"", []} = do_expand("\".hidden\\ fil"),
    {yes,"/", []} = do_expand("\".."),
    {yes,"ta/", _} = do_expand("\"../edlin_expand_SUITE_da"),
    {yes,"erl\"",[]} = do_expand("\"complete_function_parameter."),
    R = case {os:type(), file:native_name_encoding()} of
        {{win32,_}, _} -> {skip, "Unicode on filenames in windows are tricky"};
        {_,latin1} -> {skip, "Cannot interpret unicode filenames when native_name_encoding is latin1"};
        _ ->
            {yes,"isible",
            [{"visible file",_},{"visible_file",_},{"visible😀_file",_}]} = do_expand("\"v"),
            {yes,"e\"",[]} = do_expand("\"visible😀_fil"),
            {yes,[],
            [{"../",[]},
            {".hidden file",_},
            {".hidden_file",_},
            {".hidden😀_file",_}]} = do_expand("\"."),
            ok
    end,
    file:set_cwd(CWD),
    R.

no_completion(_Config) ->
    %% No autocompletion, and no crashes
    {no, _, _} = do_expand("[{"),
    {no, _, _} = do_expand("a [{"),
    {no, _, _} = do_expand("a [{{"),
    ok.
record_completion(_Config) ->
    %% test record completion for loaded records
    %% test record field name completion
    %% test record field completion
    {yes,"ord{", []} = do_expand("#a_rec"),
    {yes,"uoted_record'{", []} = do_expand("#'Q"),
    {no, [], [#{title:="fields", elems:=[{"a_field",_}, {"b_field",_}, {"c_field",_}, {"d_field",_}]}]} = do_expand("#a_record{"),
    {no, [], [#{title:="fields", elems:=[{"a_field",_}, {"b_field",_}, {"c_field",_}, {"d_field",_}]}]} = do_expand("#a_record."),
    {yes,"eld=", []} = do_expand("#a_record{a_fi"),
    {no,[],[#{title:="types",elems:=
             [{"atom1",[]},
              {"atom2",[]},
              {"btom",[]},
              {"'my atom'",[]},
              {"{atom3, ...}",[]}],
             options:=[{hide,title}]}]} = do_expand("#a_record{a_field="),
    %% test that an already specified field does not get suggested again
    {no,[],
    [#{title:="fields", elems:=
              [{"a_field",[{ending,"="}]},
               {"b_field",[{ending,"="}]},
               {"c_field",[{ending,"="}]},
               {"d_field",[{ending,"="}]}],
              options:=[highlight_all]}]} = do_expand("#a_record{a_field={atom3,b},"),
    %% test match argument
    {yes,_,[]} = do_expand("#a_record{a_field={atom3"),
    {no,[],[#{title:="types",elems:=[{"{atom4, ...}",[]}],options:=[{hide,title}]}]} = do_expand("#a_record{a_field={atom3,"),
    {no,[],[#{title:="types",elems:=[{"integer() >= 0",[]}],options:=[{hide,title}]}]} = do_expand("#a_record{a_field={atom3,{atom4, "),
    {no,_,_} = do_expand("#a_record{a_field={atom3,{atom4, 1"),
    {no,_,_} = do_expand("#a_record{a_field={atom3,{atom4, 1}"),
    ok.

fun_completion(_Config) ->
    {yes, "/1", []} = do_expand("fun lists:unzip3"),
    {no, [], []} = do_expand("fun lists:unzip3/1,"),
    {no, [], []} = do_expand("lists:unzip3/1"),
    {no, [], [{"2",_},{"3",_}]} = do_expand("lists:seq/"),
    %{yes, ", ", _} = do_expand("lists:all(fun erlang:is_atom/1"),
    {no, [], [#{}]} = do_expand("lists:all(fun erlang:is_atom/1, "),
    ok.

binding_completion(_Config) ->
    %% test that bindings in the shell can be completed
    {yes,"ding",[]} = do_expand("Bin"),
    {yes,"ding",[]} = do_expand("file:open(Bin"),
    {yes,"ding",[]} = do_expand("fun (X, Y) -> Bin"),
    %% test unicode
    {yes,"öndag", []} = do_expand("S"),
    {yes,"", []} = do_expand("Ö"),
    ok.

map_completion(_Config) ->
    %% test that key suggestion works for a known map in bindings
    {no,[],[{"a_key",[{ending, "=>"}]},{"b_key",_},{"c_key",_}]} = do_expand("MapBinding#{"),
    {yes, "_key=>", []} = do_expand("MapBinding#{b"),
    {yes, "_key=>", []} = do_expand("MapBinding # { b"),
    %% test that an already specified key does not get suggested again
    {no, [], [{"a_key",_},{"c_key", _}]} = do_expand("MapBinding#{b_key=>1,"),
    %% test that unicode works
    ok.

function_parameter_completion(Config) ->
    %% test first and second parameter
    %% test multiple arities with same type on first parameter
    %% test multiple arities with different type on first parameter
    %% test that recursive types does not trigger endless loop
    %% test that getting type of out of bound parameter does not trigger crash
    compile_and_load2(Config,complete_function_parameter),
    {no, [], [#{elems:=[#{title:="complete_function_parameter:an_untyped_fun/2", elems:=[]}]}]} = do_expand("complete_function_parameter:an_untyped_fun("),
    {yes,":",[]} = do_expand("complete_function_parameter:an_untyped_fun(complete_function_parameter"),
    {no, [], [#{elems:=[#{elems:=[#{title:="types",elems:=[{"integer()",[]}]}]}]}]} = do_expand("complete_function_parameter:a_fun_name("),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"integer()",[]}]}]}]}]} = do_expand("complete_function_parameter:a_fun_name(1,"),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"integer()",[]}]}]}]}]}  = do_expand("complete_function_parameter : a_fun_name ( 1 , "),
    {no, [], []} = do_expand("complete_function_parameter:a_fun_name(1,2"),
    {no, [], []} = do_expand("complete_function_parameter:a_fun_name(1,2,"),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"any()",[]},{"[any() | [Deeplist]]",[]}]}]}]}]} = do_expand("complete_function_parameter:a_deeplist_fun("),
    {no,[],[#{title:="typespecs",
                elems:=[#{title:=
                               "complete_function_parameter:multi_arity_fun(T1)",
                          elems:=[#{title:="types",
                                    elems:=[{"integer()",[]}],
                                    options:=[{hide,title}]}],
                          options:=[{highlight_param,1}]},
                           #{title:=
                               "complete_function_parameter:multi_arity_fun(T1, T2)",
                            elems:=[#{title:="types",
                                    elems:=[{"integer()",[]}],
                                    options:=[{hide,title}]}],
                            options:=[{highlight_param,1}]},
                            #{title:=
                                "complete_function_parameter:multi_arity_fun()",
                            options:=[],
                            elems:=[")"]}],
                options:=[highlight_all]}]} = do_expand("complete_function_parameter:multi_arity_fun("),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"true",[]},{"false",[]}]}]}]}]} = do_expand("complete_function_parameter:multi_arity_fun(1,"),
    {no,[],
        [#{elems :=
            [#{elems :=
                    [#{elems := [{"integer()",[]}],
                        options := [{hide,title}],
                        title := "types"}],
                options := [{highlight_param,1}],
                title :=
                    "complete_function_parameter:different_multi_arity_fun(T1)"},
                #{elems :=
                    [#{elems := [{"true",[]},{"false",[]}],
                        options := [{hide,title}],
                        title := "types"}],
                options := [{highlight_param,1}],
                title :=
                    "complete_function_parameter:different_multi_arity_fun(B1, T1)"}],
        options := [highlight_all],
        title := "typespecs"}]} = do_expand("complete_function_parameter:different_multi_arity_fun("),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"integer()",[]}]}]}]}]} = do_expand("complete_function_parameter:different_multi_arity_fun(false,"),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"{atom1, ...}",[]},
                                              {"atom1",[]},
                                              {"atom2",[]},
                                              {"[atom4 | atom5]",[]}]}]}]}]} = do_expand("complete_function_parameter:advanced_nested_parameter("),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"atom1",[]}]}]}]}]} = do_expand("complete_function_parameter:advanced_nested_parameter({"),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"{integer() >= 0, ...}",[]}]}]}]}]} = do_expand("complete_function_parameter:advanced_nested_parameter({atom1,"),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"atom4",[]},{"atom5",[]}]}]}]}]} = do_expand("complete_function_parameter:advanced_nested_parameter(["),
    {no, [], [#{elems:=[#{elems:=[#{elems:=[{"atom4",[]},{"atom5",[]}]}]}]}]} = do_expand("complete_function_parameter : advanced_nested_parameter ( [ , "),
    {no,[],
    [#{elems :=
           [#{elems :=
                  [#{elems := [{"integer()",[]}],
                     options := [{hide,title}],
                     title := "types"}],
              options := [{highlight_param,1}],
              title :=
                  "complete_function_parameter:different_multi_arity_fun(T1)"},
            #{elems :=
                  [#{elems := [{"true",[]},{"false",[]}],
                     options := [{hide,title}],
                     title := "types"}],
              options := [{highlight_param,1}],
              title :=
                  "complete_function_parameter:different_multi_arity_fun(B1, T1)"}],
       options := [highlight_all],
       title := "typespecs"}]} = do_expand("complete_function_parameter:different_multi_arity_fun("),
    %% Hide results where the type of the first parameters does not match the prototype header
    {no,[],
    [#{title:="typespecs",
        elems:=[#{title:="complete_function_parameter:different_multi_arity_fun(B1, T1)",
            elems:=[#{title:="types",
                 elems:=[{"integer()",[]}],
                 options:=[{hide,title}]}],
            options:=[{highlight_param,2}]}],
        options:=[highlight_all]}]} = do_expand("complete_function_parameter:different_multi_arity_fun(false,"),
    {no, _, []} = do_expand("complete_function_parameter:different_multi_arity_fun(atom,"),
    {yes, _, _} = do_expand("complete_function_parameter:'emoji"),

    ok.

get_coverage(Config) ->
    compile_and_load2(Config,complete_function_parameter),
    do_expand("\""),
    do_expand("\"."),
    do_expand("\"../"),
    do_expand("\"/"),
    do_expand("fun m:f"),
    do_expand("fun m:f/"),
    do_expand("#"),
    do_expand("MapBinding#{"),
    do_expand("B"),
    do_expand("#a_record{"),
    do_expand("#a_record{a_field=>"),

    %% match_arguments and is_type tests
    do_expand("complete_function_parameter:map_parameter_function(#{"),
    do_expand("complete_function_parameter:map_parameter_function(#{a"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1,"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1,d=>err"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1,d=>error"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1,d=>error}"),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>1,b=>2,c=>3,d=>error"),
    do_expand("complete_function_parameter:map_parameter_function(#{}, "),
    do_expand("complete_function_parameter:map_parameter_function(#{V=>1}, "),
    do_expand("complete_function_parameter:map_parameter_function(#{a=>V}, "),
    do_expand("complete_function_parameter:tuple_parameter_function({a,b}, "),
    do_expand("complete_function_parameter:tuple_parameter_function({a,V}, "),
    do_expand("complete_function_parameter:list_parameter_function([], "),
    do_expand("complete_function_parameter:list_parameter_function([atom], "),
    true = {no, [], []} =/= do_expand("complete_function_parameter:list_parameter_function([V], "),
    do_expand("complete_function_parameter:non_empty_list_parameter_function([atom], "),
    do_expand("complete_function_parameter:non_empty_list_parameter_function([], "),
    do_expand("complete_function_parameter:binary_parameter_function(<<0>>, "),
    do_expand("complete_function_parameter:binary_parameter_function(<<V>>, "),
    do_expand("complete_function_parameter:integer_parameter_function(0, "),
    do_expand("complete_function_parameter:non_neg_integer_parameter_function(1, "),
    do_expand("complete_function_parameter:neg_integer_parameter_function(-1, "),
    do_expand("complete_function_parameter:float_parameter_function(0.1, "),
    do_expand("complete_function_parameter:pid_parameter_function(<0.1.0>, "),
    do_expand("complete_function_parameter:port_parameter_function(#Port<0.1>, "),
    do_expand("complete_function_parameter:record_parameter_function(#a_record{a = 1}, "),
    do_expand("complete_function_parameter:function_parameter_function(#Fun<erl_eval.1.0>, "),
    do_expand("complete_function_parameter:function_parameter_function(fun(X) -> X end, "), %% Todo verify fun arity
    do_expand("complete_function_parameter:function_parameter_function(receive X -> X end, "),
    do_expand("complete_function_parameter:function_parameter_function(V, "),
    do_expand("complete_function_parameter:function_parameter_function((1+2), "),
    do_expand("complete_function_parameter:function_parameter_function(some_call(), "),
    do_expand("complete_function_parameter:function_parameter_function(#Nope<1.0>, "),
    do_expand("complete_function_parameter:reference_parameter_function(#Ref<1.0.1.0>, "),
    do_expand("complete_function_parameter:any_parameter_function(#Ref<1.0.1.0>, "),
    do_expand("complete_function_parameter:ann_type_parameter_function(atom, "),
    true = {no, [], []} =/= do_expand("complete_function_parameter:ann_type_parameter_function2(1, "),
    do_expand("complete_function_parameter:atom_parameter_function(atom, "),
    do_expand("complete_function_parameter:ann_type_parameter_function(atom"),
    do_expand("complete_function_parameter:atom_parameter_function(atom"),
    %% user_defined function
    {yes, "func(", _} =
        do_expand("my_"),
    {no,[],[#{elems :=
            [#{elems :=
                    [#{elems := [{"#my_record",[]}],
                        options := [{hide,title}],
                        title := "types"}],
                options := [{highlight_param,1}],
                title := "shell_default:my_func(A)"}],
        options := [highlight_all],
        title := "typespecs"}]} =
        do_expand("my_func("),
    {yes,"=",[]} =
        do_expand("my_func(#my_record{ field"),
    {no,[],
        [#{elems :=
                [#{elems := [{"a_value",[]},{"b_value",[]}],
                    options := [{separator," :: "},{highlight_all}],
                    title := "erlang:my_type()"}],
            options := [{hide,title}],
            title := "types"}]} =
        do_expand("my_func(#my_record{ field=>"),
    {yes,"ue, ",[]} =
        do_expand("my_func(#my_record{field=>a_val"),
    %% bifs()
    {yes, "st(", _} =
        do_expand("integer_to_li"),
    %% commands()
    {yes, "(", _} =
        do_expand("bt"),
    {yes, ":", _}=edlin_expand:expand(lists:reverse("complete_function_parameter")),
    {no, [], _}=edlin_expand:expand(lists:reverse("#")),
    {no, [], _}=edlin_expand:expand(lists:reverse("UnbindedMap#")),
    {no, [], _}=edlin_expand:expand(lists:reverse("UnbindedMap#{")),
    {no, [], _}=edlin_expand:expand(lists:reverse("#{")),
    {yes, "(", _}=edlin_expand:expand(lists:reverse("complete_function_parameter:a_fun_name")),

    do_expand("fun l"),
    %{yes, ">", _} = 
    do_expand("fun () -"),
    %{yes, "n ", _} =
    do_expand("fun () whe "),
    %{no, [], []} =
    do_expand("fun () when "),
    %{no, [], []} = 
    do_expand("fun () -> "),
    %% {keyword, ...}
    do_expand("M#"),
    do_expand("#non_existant_record"),
    do_expand("#a_record{ non_existand_field"),
    
    
    %% match_arguments coverage
    do_expand("complete_function_parameter:integer_parameter_function(atom,"), %% match_argument -> false
    do_expand("complete_function_parameter:a_zero_arity_fun()"), %% match_argument, parameters empty
    do_expand("erlang:system_info(thread"),
    do_expand("erlang:system_info(thread_nope"),
    do_expand("erlang:system_info(threads"),
    do_expand("erlang:process_flag(priori"),
    do_expand("erlang:process_flag(priority_nope"),
    do_expand("erlang:process_flag(priority, "),
    do_expand("erlang:process_flag(priority, atom"),
    do_expand("erlang:process_flag(priority, 1"),
    do_expand("erlang:system_info({allocator,"),
    do_expand("lists:seq(1"),
    do_expand("lists:seq(1, 10"),
    do_expand("ssh:connect({"),
    do_expand("ssh:connect({255,"),
    do_expand("ssh:connect({'$i"),
    do_expand("ssh:connect({'$x"),
    do_expand("ssh:connect({1000,"),
    do_expand("ssh:connect({255,255,255,255,255,255,255,255"),
    do_expand("ssh:connect({255,255,255,255}, ["),
    do_expand("ssh:connect(receive V -> V end, "),
    do_expand("ssh:connect((1+2), "),
    do_expand("ssh:connect(hej(), "),
    do_expand("ssh:connect(fun() -> ok end, "),
    do_expand("ssh:connect(fun a:b/1, "),
    do_expand("ssh:connect(V, [in"),
    do_expand("ssh:connect(V, [inet"),
    do_expand("ssh:connect(V, [inet, "),
    do_expand("atom_to_list("),
    do_expand("help("),
    do_expand("h(lists"),
    do_expand("ht(lists"),
    do_expand("h(file,"),
    do_expand("ht(file,"),
    do_expand("ht(file,mode"),
    do_expand("file:get_cwd()"),
    do_expand("fl"),
    do_expand("fl("),
    do_expand("fl()"),
    do_expand("MapBinding#"),
    do_expand("RecordBinding#"),
    do_expand("TupleBinding#"),
    do_expand("Binding#"),
    do_expand("MyVar"),
    {_, _, M0} = do_expand("ssh:connect("),
    do_format(M0),
    {_, _, M1} = do_expand("ssh:connect({"),
    do_format(M1),
    {_, _, M6} = do_expand("ssh:connect({},["),
    do_format(M6),
    lists:flatten(edlin_expand:format_matches(M6, 20)),
    {_, _, M2} = do_expand("e"),
    do_format(M2),
    {_, _, M3} = do_expand("erlang:i"),
    do_format(M3),
    {_, _, M4} = do_expand("complete_function_parameter:an_untyped_fun("),
    do_format(M4),
    lists:flatten(edlin_expand:format_matches(M4, 20)),
    {_,_,M5}=edlin_expand:expand("e"),
    do_format(M5),
    {_,_,M7}=edlin_expand:expand("erlang:"),
    do_format(M7),
    {_,_,M8}=edlin_expand:expand("e"),
    do_format(M8),
    lists:flatten(edlin_expand:format_matches(M8, 20)),
    {_,_,M9}=edlin_expand:expand("complete_function_parameter:an_untyped_fun("),
    lists:flatten(edlin_expand:format_matches(M9, 20)),
    do_format(M5),
    {_, _, M10} = edlin_expand:expand("ssh:connect({},["),
    do_format(M10),
    lists:flatten(edlin_expand:format_matches(M10, 20)),
    %% Test that we are not filtering duplicates bit with different case or different string lengths
    {yes,"e", M11} = do_expand("complete_function_parameter:cas"),
    "\e[;1;4mfunctions\e[0m\ncaseSensitiveFunction(        casesensitivefunction(        \ncaseSensitiveFunctionName(\n" = do_format(M11),
    ok.

%% Normal module name, some function names using quoted atoms.
quoted_fun(Config) when is_list(Config) ->
    {module,expand_test} = compile_and_load(Config,expand_test),
    {module,expand_test1} = compile_and_load(Config,expand_test1),
    %% should be no colon after test this time
    {yes, "test", [#{title:="modules", elems:=[{"expand_test",[{ending, ":"}]},{"expand_test1",_}]}]} = do_expand("expand_"),
    {no, [], []} = do_expand("expandXX_"),
    {no,[],[#{title:="functions",
                     elems:=[{"'#weird-fun-name'",_},
                            {"'Quoted_fun_name'",_},
                            {"'Quoted_fun_too'",_},
                            {"a_fun_name",_},
                            {"a_less_fun_name",_},
                            {"b_comes_after_a",_},
                            {"module_info",_}]}]} = do_expand("expand_test1:"),
    {yes,"_",[#{elems:=[{"a_fun_name",_},
                              {"a_less_fun_name",_}]}]} = do_expand("expand_test1:a"),
    {yes,[],[#{elems:=[{"a_fun_name",_},
                             {"a_less_fun_name",_}]}]} = do_expand("expand_test1:a_"),
    {yes,[],[#{elems:=[{"'#weird-fun-name'",_},
                             {"'Quoted_fun_name'",_},
                             {"'Quoted_fun_too'",_}]}]} = do_expand("expand_test1:'"),
    {yes,"uoted_fun_",[#{elems:=[{"'Quoted_fun_name'",_},
                                       {"'Quoted_fun_too'",_}]}]} = do_expand("expand_test1:'Q"),
    {yes,[],[#{elems:=[{"'Quoted_fun_name'",_},
                             {"'Quoted_fun_too'",_}]}]} = do_expand("expand_test1:'Quoted_fun_"),
    {yes,"weird-fun-name'(",[]} = do_expand("expand_test1:'#"),

    %% Since there is a module_info/1 as well as a module_info/0
    %% there should not be a closing parenthesis added.
    {yes,"(",[]} = do_expand("expand_test:module_info"),
    ok.

quoted_module(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = compile_and_load(Config,'ExpandTestCaps'),
    {yes, "Caps':",[]} = do_expand("'ExpandTest"),
    {no,[],[#{elems:=[{"a_fun_name",_},
                            {"a_less_fun_name",_},
                            {"b_comes_after_a",_},
                            {"module_info",_}]}]} = do_expand("'ExpandTestCaps':"),
    {yes,[],[#{title:="functions", elems:=[{"a_fun_name",_},
                                                {"a_less_fun_name",_}]}]} = do_expand("'ExpandTestCaps':a_"),
    ok.

%% Test that expansion does not break when module/function is invalid
invalid_module(Config) when is_list(Config) ->

    {no, "", []} = do_expand("0"),
    {no, "", []} = do_expand("0:"),
    {no, "", []} = do_expand("0:a"),
    {no, "", []} = do_expand("0:a("),
    {no, "", []} = do_expand("lists:0"),
    {no, "", []} = do_expand("lists:0("),
    {no, "", []} = do_expand("fun 0"),
    {no, "", []} = do_expand("fun 0:"),
    {no, "", []} = do_expand("fun 0:a"),
    {no, "", []} = do_expand("fun 0:a/"),
    {no, "", []} = do_expand("fun lists:0"),
    {no, "", []} = do_expand("fun lists:0/"),
    ok.

quoted_both(Config) when is_list(Config) ->
    {module,'ExpandTestCaps'} = compile_and_load(Config,'ExpandTestCaps'),
    {module,'ExpandTestCaps1'} = compile_and_load(Config,'ExpandTestCaps1'),
    %% should be no colon (or quote) after test this time
    {yes, "Caps", [#{elems:=[{"'ExpandTestCaps'",[{ending, ":"}]},{"'ExpandTestCaps1'",_}]}]} = do_expand("'ExpandTest"),
    {no,[],[#{elems:=[{"'#weird-fun-name'",_},
                            {"'Quoted_fun_name'",_},
                            {"'Quoted_fun_too'",_},
                            {"a_fun_name",_},
                            {"a_less_fun_name",_},
                            {"b_comes_after_a",_},
                            {"module_info",_}]}]} = do_expand("'ExpandTestCaps1':"),
    {yes,"_",[#{elems:=[{"a_fun_name",_},{"a_less_fun_name",_}]}]} = do_expand("'ExpandTestCaps1':a"),
    {yes,[],[#{elems:=[{"a_fun_name",_},
                             {"a_less_fun_name",_}]}]} = do_expand("'ExpandTestCaps1':a_"),
    {yes,[],[#{elems:=[{"'#weird-fun-name'",_},
                             {"'Quoted_fun_name'",_},
                             {"'Quoted_fun_too'",_}]}]} = do_expand("'ExpandTestCaps1':'"),
    {yes,"uoted_fun_",[#{elems:=[{"'Quoted_fun_name'",_},{"'Quoted_fun_too'",_}]}]} = do_expand("'ExpandTestCaps1':'Q"),
    {yes,[],[#{elems:=[{"'Quoted_fun_name'",_},
                             {"'Quoted_fun_too'",_}]}]} = do_expand("'ExpandTestCaps1':'Quoted_fun_"),
    {yes,"weird-fun-name'()",[]} = do_expand("'ExpandTestCaps1':'#"),
    ok.

%% Note: pull request #1152.
erl_1152(Config) when is_list(Config) ->
    "foo"++"    "++[1089]++_ = do_format(["foo",[1089]]),
    ok.

check_trailing(Config) when is_list(Config) ->
    Str = lists:duplicate(80, $1),
    StrF = do_format([Str]),
    {_, "...\n"} = lists:split(76, StrF),
    ok.

unicode(Config) when is_list(Config) ->
    {module,unicode_expand} = compile_and_load(Config,'unicode_expand'),
    {no,[], [#{elems:=[{"'кlирилли́ческий атом'",_},
                             {"'кlирилли́ческий атомB'",_},
                             {"module_info",_}]}]} = do_expand("unicode_expand:"),
    {yes,"рилли́ческий атом", [#{elems:=[{"'кlирилли́ческий атом'",_},
                                              {"'кlирилли́ческий атомB'",_}]}]} = do_expand("unicode_expand:'кlи"),
    {yes,"еский атом", [#{elems:=[{"'кlирилли́ческий атом'",_},
                                        {"'кlирилли́ческий атомB'",_}]}]} = do_expand("unicode_expand:'кlирилли́ч"),
    {yes,"(", []} = do_expand("unicode_expand:'кlирилли́ческий атомB'"),
    "'кlирилли́ческий атом'     'кlирилли́ческий атомB'    module_info\n" =
        do_format([{"'кlирилли́ческий атом'",[]},
                   {"'кlирилли́ческий атомB'",[]},
                   {"module_info",[]}]),
    ok.

do_expand(String) ->
    io:format("~ts", [String]),
    Bs = [
          {'Binding', 0},
          {'MapBinding', #{a_key=>0, b_key=>1, c_key=>2}},
          {'RecordBinding', {some_record, 1, 2}},
          {'TupleBinding', {0, 1, 2}},
          {'Söndag', 0},
          {'Ö', 0}],
    Rt = ets:new(records, []),

    Rt2 = [{my_record, {attribute,[{text,"record"},
                        {location,{1,2}}],
                    record,
                    {my_record, [{typed_record_field,{record_field,[{text,"field"},
                                                                            {location,{1,20}}],
                                                                            {atom,[{text,"field"},{location,{1,20}}],field},
                                                                            {atom,[{text,"a_value"},{location,{1,28}}],a_value}},
                                                            {user_type,[{text,"my_type"},{location,{1,33}}],
                                                                        my_type,[]}}]}}}],
    Ft = [{{function,{shell_default,my_func,1}},fun(_A)->0 end},
          {{function_type,{shell_default,my_func,1}},
                {attribute,[{text,"spec"},{location,{1,2}}],
                spec,
                {{my_func,1},
                [{type,[{text,"("},{location,{1,14}}],
                bounded_fun,
                [{type,[{text,"("},{location,{1,14}}],
                'fun',
                [{type,[{text,"("},{location,{1,14}}],
                        product,
                        [{var,[{text,"A"},{location,{1,15}}],'A'}]},
                {type,[{text,"integer"},{location,{1,21}}],integer,[]}]},
                [{type,[{text,"A"},{location,{1,36}}],
                constraint,
                [{atom,[{text,"A"},{location,{1,36}}],is_subtype},
                [{var,[{text,"A"},{location,{1,36}}],'A'},
           {type,[{text,"#"},{location,{1,41}}],
                record,
                [{atom,[{text,"my_record"},{location,{1,42}}],
                        my_record}]}]]}]]}]}}},
        {{type,my_type},
            {attribute,[{text,"type"},{location,{1,2}}],
            type,
            {my_type,{type,[{text,"a"},{location,{1,20}}],
            union,
            [{atom,[{text,"a_value"},{location,{1,20}}],a_value},
            {atom,[{text,"b_value"},{location,{1,24}}],b_value}]},
            []}}}],
    shell:read_and_add_records(edlin_expand_SUITE, '_', [], Bs, Rt),
    edlin_expand:expand(lists:reverse(String), [], {shell_state, Bs, ets:tab2list(Rt)++Rt2, Ft}).

do_format(StringList) ->
    lists:flatten(edlin_expand:format_matches(StringList, 79)).

compile_and_load2(Config, Module) ->
    Filename = filename:join(
                 proplists:get_value(data_dir,Config),
                 atom_to_list(Module)),
    PrivDir = proplists:get_value(priv_dir,Config),
    c:c(Filename, [debug_info, {outdir, PrivDir}]).

compile_and_load(Config,Module) ->
    Filename = filename:join(
                 proplists:get_value(data_dir,Config),
                 atom_to_list(Module)),
    {ok,Module,Bin} = compile:file(Filename, [binary]),
    code:load_binary(Module, Filename, Bin).
