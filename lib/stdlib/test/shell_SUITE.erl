%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(shell_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([forget/1, records/1, known_bugs/1, otp_5226/1, otp_5327/1,
	 otp_5435/1, otp_5195/1, otp_5915/1, otp_5916/1,
	 bs_match_misc_SUITE/1, bs_match_int_SUITE/1,
	 bs_match_tail_SUITE/1, bs_match_bin_SUITE/1,
	 bs_construct_SUITE/1,
	 refman_bit_syntax/1, 
	 progex_bit_syntax/1, progex_records/1, 
	 progex_lc/1, progex_funs/1,
	 otp_5990/1, otp_6166/1, otp_6554/1,
	 otp_7184/1, otp_7232/1, otp_8393/1, otp_10302/1, otp_13719/1]).

-export([ start_restricted_from_shell/1, 
	  start_restricted_on_command_line/1,restricted_local/1]).

%% Internal export.
-export([otp_5435_2/0, prompt1/1, prompt2/1, prompt3/1, prompt4/1,
	 prompt5/1]).

%%
%% Define to run outside of test server
%%
%% -define(STANDALONE,1).

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-define(t,test_server).
-export([config/2]).
-define(line, noop, ).
config(priv_dir,_) ->
    ".".
-else.
-include_lib("common_test/include/ct.hrl").
-export([init_per_testcase/2, end_per_testcase/2]).
init_per_testcase(_Case, Config) ->
    OrigPath = code:get_path(),
    code:add_patha(proplists:get_value(priv_dir,Config)),
    [{orig_path,OrigPath} | Config].

end_per_testcase(_Case, Config) ->
    OrigPath = proplists:get_value(orig_path,Config),
    code:set_path(OrigPath),
    application:unset_env(stdlib, restricted_shell),
    (catch code:purge(user_default)),
    (catch code:delete(user_default)),
    ok.
-endif.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() -> 
    [forget, records, known_bugs, otp_5226, otp_5327,
     otp_5435, otp_5195, otp_5915, otp_5916, {group, bits},
     {group, refman}, {group, progex}, {group, tickets},
     {group, restricted}].

groups() -> 
    [{restricted, [],
      [start_restricted_from_shell,
       start_restricted_on_command_line, restricted_local]},
     {bits, [],
      [bs_match_misc_SUITE, bs_match_tail_SUITE,
       bs_match_bin_SUITE, bs_construct_SUITE]},
     {refman, [], [refman_bit_syntax]},
     {progex, [],
      [progex_bit_syntax, progex_records, progex_lc,
       progex_funs]},
     {tickets, [],
      [otp_5990, otp_6166, otp_6554, otp_7184,
       otp_7232, otp_8393, otp_10302, otp_13719]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


-record(state, {bin, reply, leader, unic = latin1}).


%% Test that a restricted shell can be started from the normal shell.
start_restricted_from_shell(Config) when is_list(Config) ->
    [{error,nofile}] = scan(<<"begin shell:start_restricted("
			      "nonexisting_module) end.">>),
    Test = filename:join(proplists:get_value(priv_dir, Config),
			 "test_restricted.erl"),
    Contents = <<"-module(test_restricted).
                  -export([local_allowed/3, non_local_allowed/3]).
local_allowed(m,[],State) ->
    {true,State};
local_allowed(ugly,[],_State) ->
    non_conforming_reply;
local_allowed(_,_,State) ->
    {false,State}.

non_local_allowed({shell,stop_restricted},[],State) ->
    {true,State};
non_local_allowed({erlang,'+'},[_],State) ->
    {true,State};
non_local_allowed({erlang,'-'},[_,_],_State) ->
    non_conforming_reply;
non_local_allowed({h, d}, [Arg], S) ->
    {{redirect, {erlang,hd}, [Arg]}, S};
non_local_allowed(_,_,State) ->
    {false,State}.
">>,
    ok = compile_file(Config, Test, Contents, []),
"exception exit: restricted shell starts now" =
comm_err(<<"begin shell:start_restricted("
	   "test_restricted) end.">>),
{ok, test_restricted} =
application:get_env(stdlib, restricted_shell),
"Module" ++ _ = t({<<"begin m() end.">>, utf8}),
"exception exit: restricted shell does not allow c(foo)" =
comm_err(<<"begin c(foo) end.">>),
"exception exit: restricted shell does not allow init:stop()" =
comm_err(<<"begin init:stop() end.">>),
"exception exit: restricted shell does not allow init:stop()" =
comm_err(<<"begin F = fun() -> init:stop() end, F() end.">>),
"exception error: an error occurred when evaluating an arithmetic expression" =
comm_err(<<"begin +a end.">>),
"exception exit: restricted shell does not allow a + b" =
comm_err(<<"begin a+b end.">>),
"exception exit: restricted shell does not allow - b" =
comm_err(<<"begin -b end.">>),
"exception exit: restricted shell does not allow 1 + 2" =
comm_err(<<"begin if atom(1 + 2> 0) -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow 1 + 2" =
comm_err(<<"begin if is_atom(1 + 2> 0) -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow - 2" =
comm_err(<<"begin if - 2 -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow - 2" =
comm_err(<<"begin if (- 2 > 0)  andalso true -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow - 2" =
comm_err(<<"begin if (- 2 > 0)  orelse true -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow 1 + 2" =
comm_err(<<"begin if 1 + 2 > 0 -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow 1 + 2" =
comm_err(<<"begin if erlang:is_atom(1 + 2> 0) -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow is_integer(1)" =
comm_err(<<"begin if is_integer(1) -> 1; true -> 2 end end.">>),
"exception exit: restricted shell does not allow is_integer(1)" =
comm_err(<<"begin if integer(1) -> 1; true -> 2 end end.">>),
"exception exit: "
"restricted shell module returned bad value non_conforming_reply" =
comm_err(<<"ugly().">>),
[one] = scan(<<"h:d([one,two]).">>),
"exception exit: "
"restricted shell module returned bad value non_conforming_reply" =
comm_err(<<"1 - 2.">>),
"exception exit: restricted shell stopped"=
comm_err(<<"begin shell:stop_restricted() end.">>),
undefined =
application:get_env(stdlib, restricted_shell),
ok.

%% Check restricted shell when started from the command line.
start_restricted_on_command_line(Config) when is_list(Config) ->
    {ok,Node} = start_node(shell_suite_helper_1,
			   "-pa "++proplists:get_value(priv_dir,Config)++
			       " -stdlib restricted_shell foo"),
    "Warning! Restricted shell module foo not found: nofile"++_ =
	t({Node, <<"begin m() end.">>}),
    "exception exit: restricted shell does not allow m()" =
	comm_err({Node, <<"begin m() end.">>}),
    [ok] =
	(catch scan({Node, <<"begin q() end.">>})),
    test_server:stop_node(Node),
    Test = filename:join(proplists:get_value(priv_dir, Config),
			       "test_restricted2.erl"),
    Contents = <<"-module(test_restricted2).
                  -export([local_allowed/3, non_local_allowed/3]).
                  local_allowed(m,[],State) ->
                      {true,State};
                  local_allowed(_,_,State) ->
                      {false,State}.

                  non_local_allowed({shell,stop_restricted},[],State) ->
                      {true,State};
                  non_local_allowed({erlang,node},[],State) ->
                      {true,State};
                  non_local_allowed(_,_,State) ->
                      {false,State}.
                 ">>,
    ok = compile_file(Config, Test, Contents, []),
    {ok,Node2} = start_node(shell_suite_helper_2,
				 "-pa "++proplists:get_value(priv_dir,Config)++
				 " -stdlib restricted_shell test_restricted2"),
    "Module" ++ _ = t({Node2,<<"begin m() end.">>, utf8}),
    "exception exit: restricted shell does not allow c(foo)" =
	comm_err({Node2,<<"begin c(foo) end.">>}),
    "exception exit: restricted shell does not allow init:stop()" =
	comm_err({Node2,<<"begin init:stop() end.">>}),
    "exception exit: restricted shell does not allow init:stop()" =
	comm_err({Node2,<<"begin F = fun() -> init:stop() end, F() end.">>}),
    [Node2] =
	scan({Node2, <<"begin erlang:node() end.">>}),
    [Node2] =
	scan({Node2, <<"begin node() end.">>}),
    "exception exit: restricted shell stopped"=
	comm_err({Node2,<<"begin shell:stop_restricted() end.">>}),
    [ok] =
	scan({Node2, <<"begin q() end.">>}),
    test_server:stop_node(Node2),
    ok.

%% Tests calling local shell functions with spectacular arguments in
%% restricted shell.
restricted_local(Config) when is_list(Config) ->
    [{error,nofile}] = scan(<<"begin shell:start_restricted("
				    "nonexisting_module) end.">>),
    Test = filename:join(proplists:get_value(priv_dir, Config),
			       "test_restricted_local.erl"),
    Contents = <<"-module(test_restricted_local).
                  -export([local_allowed/3, non_local_allowed/3]).
                  local_allowed(m,[],State) ->
                      {true,State};
                  local_allowed(banan,_,State) ->
                      {true,State};
                  local_allowed(funkis,_,State) ->
                      {true,State};
                  local_allowed(c,_,State) ->
                      {true,State};
                  local_allowed(_,_,State) ->
                      {false,State}.

                  non_local_allowed({shell,stop_restricted},[],State) ->
                      {true,State};
                  non_local_allowed(_,_,State) ->
                      {false,State}.
                 ">>,
    ok = compile_file(Config, Test, Contents, []),
    Test2 = filename:join(proplists:get_value(priv_dir, Config),
			       "user_default.erl"),
    Contents2 = <<"-module(user_default).
                  -export([funkis/1,apple/1]).
                  funkis(F) when is_function(F) ->
                      funkis;
                  funkis(_) ->
                      nofunkis.
                  apple(_) ->
                      apple.
                 ">>,
    ok = compile_file(Config, Test2, Contents2, []),
    "exception exit: restricted shell starts now" =
	comm_err(<<"begin shell:start_restricted("
			 "test_restricted_local) end.">>),
    {ok, test_restricted_local} =
	application:get_env(stdlib, restricted_shell),
    "exception exit: restricted shell does not allow foo(" ++ _ =
	comm_err(<<"begin F=fun() -> hello end, foo(F) end.">>),
    "exception error: undefined shell command banan/1" =
	comm_err(<<"begin F=fun() -> hello end, banan(F) end.">>),
    "{error,"++_ = t(<<"begin F=fun() -> hello end, c(F) end.">>),
    "exception exit: restricted shell does not allow l(" ++ _ =
	comm_err(<<"begin F=fun() -> hello end, l(F) end.">>),
    "exception error: variable 'F' is unbound" =
	comm_err(<<"begin F=fun() -> hello end, f(F), F end.">>),
    [funkis] =
	scan(<<"begin F=fun() -> hello end, funkis(F) end.">>),
    "exception exit: restricted shell does not allow apple(" ++ _ =
	comm_err(<<"begin F=fun() -> hello end, apple(F) end.">>),
    "exception exit: restricted shell stopped"=
	comm_err(<<"begin shell:stop_restricted() end.">>),
    undefined =
	application:get_env(stdlib, restricted_shell),
    (catch code:purge(user_default)),
    true = (catch code:delete(user_default)),
    ok.
    

%% f/0 and f/1.
forget(Config) when is_list(Config) ->
    %% f/0
    [ok] = scan(<<"begin f() end.">>),
    "1: variable 'A' is unbound" =
        comm_err(<<"A = 3, f(), A.">>),
    [ok] = scan(<<"A = 3, A = f(), A.">>),

    %% f/1
    [ok] = scan(<<"begin f(A) end.">>),
    "1: variable 'A' is unbound" =
        comm_err(<<"A = 3, f(A), A.">>),
    [ok] = scan(<<"A = 3, A = f(A), A.">>),
    "exception error: no function clause matching call to f/1" =
        comm_err(<<"f(a).">>),
    ok.

%% Test of the record support. OTP-5063.
records(Config) when is_list(Config) ->
    %% rd/2
    [{attribute,_,record,{bar,_}},ok] =
        scan(<<"rd(foo,{bar}), 
                rd(bar,{foo = (#foo{})#foo.bar}),
                rl(bar).">>),
    "variable 'R' is unbound" = % used to work (before OTP-5878, R11B)
        exit_string(<<"rd(foo,{bar}), 
                       R = #foo{},
                       rd(bar,{foo = R#foo.bar}).">>),
    "exception error: no function clause matching call to rd/2" =
        comm_err(<<"rd({foo},{bar}).">>),
    "bad record declaration" = exit_string(<<"A = bar, rd(foo,A).">>),
    [foo] = scan(<<"begin rd(foo,{bar}) end.">>),
    "1: record foo undefined" =
         comm_err(<<"begin rd(foo,{bar}), #foo{} end.">>),
    ['f o o'] = scan(<<"rd('f o o', {bar}).">>),
    [foo] = scan(<<"rd(foo,{bar}), rd(foo,{foo = #foo{}}).">>),

    %% rf/0,1
    [_, {attribute,_,record,{foo,_}},ok] =
         scan(<<"rf('_'). rd(foo,{bar}),rl().">>),
    "1: record foo undefined" =
        comm_err(<<"rd(foo,{bar}), #foo{}, rf(foo), #foo{}.">>),
    [ok,{foo,undefined}] =
        scan(<<"rd(foo,{bar}), A = #foo{}, rf(foo). A.">>),
    [_] = scan(<<"begin rf() end.">>),
    [ok] = scan(<<"begin rf(foo) end.">>),

    %% rp/1
    "#foo{bar = undefined}.\nok.\n" =
        t(<<"rd(foo,{bar}), rp(#foo{}).">>),
    [{foo,3,4,3},ok] = scan(<<"rd(foo,{a = 3, b}), rp({foo,3,4,3}).">>),
    "#foo{a = 12}.\nok.\n" = t(<<"rd(foo,{a = 3}), rp({foo,12}).">>),
    [{[{foo}],12},ok] = scan(<<"rd(foo,{a = 3}), rp({[{foo}],12}).">>),

    %% rr/1,2,3
    MS = ?MODULE_STRING,
    RR1 = "rr(" ++ MS ++ "). #state{}.",
    "[state]\n"
          "#state{bin = undefined,reply = undefined,leader = undefined,\n"
          "       unic = latin1}.\n" =
        t(RR1),
    RR2 = "rr(" ++ MS ++ ",[state]). #state{}.",
    "[state]\n"
          "#state{bin = undefined,reply = undefined,leader = undefined,\n"
          "       unic = latin1}.\n" =
        t(RR2),
    RR3 = "rr(" ++ MS ++ ",'_'). #state{}.",
    "[state]\n"
          "#state{bin = undefined,reply = undefined,leader = undefined,\n"
          "       unic = latin1}.\n" =
        t(RR3),
    RR4 = "rr(" ++ MS ++ ", '_', {d,test1}).",
    [[state]] = scan(RR4),

    Test = filename:join(proplists:get_value(priv_dir, Config), "test.erl"),
    Contents = <<"-module(test).
                  -record(state, {bin :: binary(),
                                  reply = no,
                                  leader = some :: atom()}).

                  -ifdef(test1).
                  -record(test1, {f}).
                  -endif.

                  -ifdef(test2).
                  -record(test2, {g}).
                  -endif.">>,
    ok = file:write_file(Test, Contents),

    RR5 = "rr(\"" ++ Test ++ "\", '_', {d,test1}), rl([test1,test2]).",
    A1 = erl_anno:new(1),
    [{attribute,A1,record,{test1,_}},ok] = scan(RR5),
    RR6 = "rr(\"" ++ Test ++ "\", '_', {d,test2}), rl([test1,test2]).",
    [{attribute,A1,record,{test2,_}},ok] = scan(RR6),
    RR7 = "rr(\"" ++ Test ++ 
           "\", '_', [{d,test1},{d,test2,17}]), rl([test1,test2]).",
    [{attribute,A1,record,{test1,_}},{attribute,A1,record,{test2,_}},ok] =
        scan(RR7),
    PreReply = scan(<<"rr(prim_file).">>), % preloaded...
    true = is_list(PreReply),
    Dir = filename:join(proplists:get_value(priv_dir, Config), "*.erl"),
    RR8 = "rp(rr(\"" ++ Dir ++ "\")).",
    [_,ok] = scan(RR8),
    file:delete(Test),

    RR1000 = "begin rr(" ++ MS ++ ") end.",
    [_] = scan(RR1000),
    RR1001 = "begin rr(" ++ MS ++ ", state) end.",
    [_] = scan(RR1001),
    RR1002 = "begin rr(" ++ MS ++ ", state,{i,'.'}) end.",
    [_] = scan(RR1002),

    [{error,nofile}] = scan(<<"rr(not_a_module).">>),
    [{error,invalid_filename}] = scan(<<"rr({foo}).">>),
    [[]] = scan(<<"rr(\"not_a_file\").">>),

    %% using records
    [2] = scan(<<"rd(foo,{bar}), record_info(size, foo).">>),
    [true] = scan(<<"rd(foo,{bar}), is_record(#foo{}, foo).">>),
    [true] = scan(<<"rd(foo,{bar}), erlang:is_record(#foo{}, foo).">>),
    [true] = scan(<<"rd(foo,{bar}),
                     fun() when record(#foo{},foo) -> true end().">>),
    [2] = scan(<<"rd(foo,{bar}), #foo.bar.">>),
    "#foo{bar = 17}.\n" =
        t(<<"rd(foo,{bar}), A = #foo{}, A#foo{bar = 17}.">>),

    %% test of is_record/2 in lc
    "[#foo{bar = 3}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "is_record(X, foo)].">>),
    "[x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "not is_record(X, foo)].">>),
    "[#foo{bar = 3}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin is_record(X, foo) end].">>),
    "[x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin not is_record(X, foo) end].">>),

    "[#foo{bar = 3},x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "is_record(X, foo) or not is_binary(X)].">>),
    "[#foo{bar = 3},x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "not is_record(X, foo) or not is_binary(X)].">>),
    "[#foo{bar = 3}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "is_record(X, foo) or is_reference(X)].">>),
    "[x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "not is_record(X, foo) or is_reference(X)].">>),

    "[#foo{bar = 3},x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin is_record(X, foo) or not is_binary(X) end].">>),
    "[#foo{bar = 3},x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin not is_record(X, foo) or not is_binary(X) end].">>),
    "[#foo{bar = 3}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin is_record(X, foo) or is_reference(X) end].">>),
    "[x,[],{a,b}].\n" =
        t(<<"rd(foo,{bar}), [X || X <- [#foo{bar=3},x,[],{a,b}],"
            "begin not is_record(X, foo) or is_reference(X) end].">>),

    [ok] =
        scan(<<"rd(a,{}), is_record({a},a) andalso true, b().">>),
    
    %% nested record defs
    "#b{a = #a{}}.\n" = t(<<"rd(a,{}), rd(b, {a = #a{}}), #b{}.">>),

    [ok,ok,ok] = scan(<<"rf('_'), rp(rp(rl(rf(rf(rf(rl())))))).">>),

    ok.

%% Known bugs.
known_bugs(Config) when is_list(Config) ->
    %% erl_eval:merge_bindings/2 cannot handle _removal_ of bindings.
    [3] = scan(<<"A = 3, length(begin f(A), [3] end), A.">>),
    ok.

%% OTP-5226. Wildcards accepted when reading BEAM files using rr/1,2,3.
otp_5226(Config) when is_list(Config) ->
    Test1 = <<"-module(test1).
               -record('_test1', {a,b}).">>,
    Test2 = <<"-module(test2).
               -record('_test2', {c,d}).">>,
    File1 = filename("test1.erl", Config),
	      File2 = filename("test2.erl", Config),
	      Beam = filename("*.beam", Config),
	      ok = compile_file(Config, File1, Test1, [no_debug_info]),
	      ok = compile_file(Config, File2, Test2, [no_debug_info]),
	      RR = "rr(\"" ++ Beam ++ "\").",
	      [Recs] = scan(RR),
	      true = lists:member('_test1', Recs),
	      true = lists:member('_test2', Recs),
	      file:delete(filename("test1.beam", Config)),
	      file:delete(filename("test2.beam", Config)),
	      file:delete(File1),
	      file:delete(File2),
	      ok.

%% OTP-5226. Test of eval_bits, mostly.
otp_5327(Config) when is_list(Config) ->
    "exception error: bad argument" =
        comm_err(<<"<<\"hej\":default>>.">>),
    <<"abc">> =
        erl_parse:normalise({bin,1,[{bin_element,1,{string,1,"abc"},
				     default,default}]}),
    [<<"abc">>] = scan(<<"<<(<<\"abc\">>):3/binary>>.">>),
    [<<"abc">>] = scan(<<"<<(<<\"abc\">>)/binary>>.">>),
    "exception error: bad argument" =
        comm_err(<<"<<(<<\"abc\">>):4/binary>>.">>),
    true = byte_size(hd(scan("<<3.14:64/float>>."))) =:= 8,
    true = byte_size(hd(scan("<<3.14:32/float>>."))) =:= 4,
    "exception error: bad argument" =
        comm_err(<<"<<3.14:128/float>>.">>),
    "exception error: bad argument" =
        comm_err(<<"<<10:default>>.">>),
    [<<98,1:1>>] = scan(<<"<<3:3,5:6>>.">>),
    {'EXIT',{badarg,_}} =
        (catch erl_parse:normalise({bin,1,[{bin_element,1,{integer,1,17},
                                            {atom,1,all},
                                            default}]})),
    [<<-20/signed>>] = scan(<<"<<-20/signed>> = <<-20>>.">>),
    [<<-300:16/signed>>] =
	scan(<<"<<-300:16/signed>> = <<-300:16>>.">>),
    [<<-1000:24/signed>>] =
	scan(<<"<<-1000:24/signed>> = <<-1000:24>>.">>),
    [<<-(1 bsl 29):32/signed>>] =
        scan(<<"<<-(1 bsl 29):32/signed>> = <<-(1 bsl 29):32>>.">>),

    "exception error: no match of right hand side value <<0,0,0>>" =
        comm_err(<<"<<B:3/unit:7-binary,_/binary>> = <<0:24>>.">>),
    true = [<<103133:64/float>>] =:=
        scan(<<"<<103133:64/float>> = <<103133:64/float>>.">>),
    true = [<<103133.0:64/float>>] =:=
        scan(<<"<<103133.0:64/float>> = <<103133:64/float>>.">>),
    true = [<<103133:64/float>>] =:= scan(<<"<<103133:64/float>>.">>),
    Int = 17,
    true = [<<Int:64/float>>] =:= scan(<<"Int = 17, <<Int:64/float>>.">>),
    "exception error: no match of right hand side value" ++ _ =
        comm_err(<<"<<103133:64/binary>> = <<103133:64/float>>.">>),
    "exception error: interpreted function with arity 1 called with two arguments" =
        comm_err(<<"(fun(X) -> X end)(a,b).">>),
    {'EXIT', {{illegal_pattern,_}, _}} =
        (catch evaluate("<<A:a>> = <<17:32>>.", [])),
    C = <<"
         <<A:4,B:4,C:4,D:4,E:4,F:4>> = <<\"hej\">>,
         case <<7:4,A:4,B:4,C:4,D:4,E:4,F:4,3:4>> of
            <<_:4,\"hej\",3:4>> -> 1;
            _ -> 2
         end.
        ">>,
    1 = evaluate(C, []),
    %% unbound_var would be nicer...
    {'EXIT',{{illegal_pattern,_},_}} =
        (catch evaluate(<<"<<A:B>> = <<17:32>>.">>, [])),
    %% undefined_bittype is turned into badmatch:
    {'EXIT',{{badmatch,<<17:32>>},_}} =
        (catch evaluate(<<"<<A/apa>> = <<17:32>>.">>, [])),
    {'EXIT',_} =
        (catch evaluate(<<"<<17/binary-unit:8-unit:16>>.">>, [])),
    {'EXIT',_} =
        (catch evaluate(<<"<<17:32/unsigned-signed>> = <<17:32>>.">>, [])),
    {'EXIT',_} =
        (catch evaluate(<<"<<17:32/unsigned-signed>>.">>, [])),
    <<17:32>> = evaluate(<<"<<17:32/signed-signed>>.">>, []),
    {'EXIT',_} =
        (catch evaluate(<<"<<32/unit:8>>.">>, [])),
    ok.

%% OTP-5435. sys_pre_expand not in the path.
otp_5435(Config) when is_list(Config) ->
    true = <<103133:64/float>> =:=
        evaluate(<<"<<103133:64/float>> = <<103133:64/float>>.">>, []),
    true = <<103133.0:64/float>> =:=
        evaluate(<<"<<103133.0:64/float>> = <<103133:64/float>>.">>, []),
    true = is_alive(),
    {ok, Node} = start_node(shell_SUITE_otp_5435),
    ok = rpc:call(Node, ?MODULE, otp_5435_2, []),
    test_server:stop_node(Node),
    ok.

start_node(Name) ->
    PA = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, "-pa " ++ PA}]).

otp_5435_2() ->
    true = code:del_path(compiler),
    %% sys_pre_expand can no longer be found
    %% OTP-5876. But erl_expand_records can!
    [{attribute,_,record,{bar,_}},ok] =
        scan(<<"rd(foo,{bar}), 
                rd(bar,{foo = (#foo{})#foo.bar}),
	       rl(bar).">>),
    ok.

%% OTP-5195. QLC, mostly.
otp_5195(Config) when is_list(Config) ->
    %% QLC. It was easier to put these cases here than in qlc_SUITE.
    "[#a{b = undefined}].\n" =
        t(<<"rd(a,{b}), qlc:e(qlc:q([X || X <- [#a{}],is_record(X, a)])).">>),

    %% An experimental shell used to translate error tuples:
    %% "(qlc) \"1: generated variable 'X' must not be used in "
    %% "list expression\".\n" = 
    %%    t(<<"qlc:q([X || X <- [{a}], Y <- [X]]).">>),
    %% Same as last one (if the shell does not translate error tuples):
    [{error,qlc,{1,qlc,{used_generator_variable,'X'}}}] =
        scan(<<"qlc:q([X || X <- [{a}], Y <- [X]]).">>),
    {error,qlc,{1,qlc,{used_generator_variable,'X'}}} =
        evaluate(<<"qlc:q([X || X <- [{a}], Y <- [X]]).">>, []),
    Ugly = <<"qlc:e(qlc:q([X || X <- qlc:append([[1,2,3],ugly()])])).">>,
    "undefined shell command ugly/0" = error_string(Ugly),
    {'EXIT',{undef,_}} = (catch evaluate(Ugly, [])),

    V_1 = <<"qlc:e(qlc:q([X || X <- qlc:append([[1,2,3],v(-1)])])).">>,
    "- 1: command not found" = comm_err(V_1),
    {'EXIT', {undef,_}} = (catch evaluate(V_1, [])),

    "1\n2\n3\n3.\n" =
        t(<<"1. 2. 3. 3 = fun(A) when A =:= 2 -> v(3) end(v(2)).">>),

    List4 = t(<<"[a,list]. A = [1,2]. "
		"qlc:q([X || X <- qlc:append(A, v(1))]). "
		"[1,2,a,list] = qlc:e(v(-1)).">>),
    "[1,2,a,list].\n" = string:substr(List4, string:len(List4)-13),

    ok.

%% OTP-5915. Strict record tests in guards.
otp_5915(Config) when is_list(Config) ->
    C = <<"
        rd(r, {a = 4,b}),
	  rd(r1, {a,b}),
	  rd(r2, {a = #r1{},b,c=length([1,2,3])}),
	  rd(r3, {a = fun(_) -> #r1{} end(1), b}),

	  foo = fun(A) when A#r1.a > A#r1.b -> foo end(#r1{b = 2}),
	  0 = fun(A) when A#r2.a -> 0 end(#r2{a = true}),
	  1 = fun(A) when (#r1{a = A})#r1.a > 2 -> 1 end(3),
	  2 = fun(N) when ((#r2{a = #r{a = 4}, b = length([a,b,c])})#r2.a)#r.a > N ->
		      2 end(2),
	  3 = fun(A) when (A#r2.a)#r1.a =:= 3 -> 3 end(#r2{a = #r1{a = 3}}),
	  ok = fun() ->
		       F = fun(A) when record(A#r.a, r1) -> 4;
			      (A) when record(A#r1.a, r1) -> 5
			   end,
		       5 = F(#r1{a = #r1{}}),
		       4 = F(#r{a = #r1{}}),
		       ok
	       end(),
	  3 = fun(A) when record(A#r1.a, r),
			  (A#r1.a)#r.a > 3 -> 3
	      end(#r1{a = #r{a = 4}}),
	  7 = fun(A) when record(A#r3.a, r1) -> 7 end(#r3{}),
	  [#r1{a = 2,b = 1}] =
	  fun() ->
		  [A || A <- [#r1{a = 1, b = 3},
			      #r2{a = 2,b = 1},
			      #r1{a = 2, b = 1}],
			A#r1.a >
			    A#r1.b]
	  end(),
	  {[_],b} =
	  fun(L) ->
                    %% A is checked only once:
		  R1 = [{A,B} || A <- L, A#r1.a, B <- L, A#r1.b],
		  A = #r2{a = true},
                    %% A is checked again:
		  B = if A#r1.a -> a; true -> b end,
		  {R1,B}
	  end([#r1{a = true, b = true}]),

	  p = fun(A) when (A#r1.a =:= 2) or (A#r2.a =:= 1) -> o;
		 (_) -> p
	      end(#r1{a = 2}),

	  o = fun(A) when (A#r1.a =:= 2) orelse (A#r2.a =:= 1) -> o;
		 (_) -> p
	      end(#r1{a = 2}),

	  3 = fun(A) when A#r1.a > 3,
			  record(A, r1) -> 3
	      end(#r1{a = 5}),

	  ok = fun() ->
		       F = fun(A) when (A#r2.a =:= 1) orelse (A#r2.a) -> 2;
			      (A) when (A#r1.a =:= 1) orelse (A#r1.a) -> 1;
			      (A) when (A#r2.a =:= 2) andalso (A#r2.b) -> 3
			   end,
		       1 = F(#r1{a = 1}),
		       2 = F(#r2{a = true}),
		       3 = F(#r2{a = 2, b = true}),
		       ok
	       end(),

	  b = fun(A) when false or not (A#r.a =:= 1) -> a;
		 (_) -> b
	      end(#r1{a = 1}),
	  b = fun(A) when not (A#r.a =:= 1) or false -> a;
		 (_) -> b
	      end(#r1{a = 1}),

	  ok = fun() ->
		       F = fun(A) when not (A#r.a =:= 1) -> yes;
			      (_) -> no
			   end,
		       no = F(#r1{a = 2}),
		       yes = F(#r{a = 2}),
		       no = F(#r{a = 1}),
		       ok
	       end(),

	  a = fun(A) when record(A, r),
			  A#r.a =:= 1,
			  A#r.b =:= 2 ->a
	      end(#r{a = 1, b = 2}),
	  a = fun(A) when erlang:is_record(A, r),
			  A#r.a =:= 1,
			  A#r.b =:= 2 -> a
	      end(#r{a = 1, b = 2}),
	  a = fun(A) when is_record(A, r),
			  A#r.a =:= 1,
			  A#r.b =:= 2 -> a
	      end(#r{a = 1, b = 2}),

	  nop = fun(A) when (is_record(A, r1) and (A#r1.a > 3)) or (A#r2.a < 1) ->
			japp;
		   (_) ->
			nop
		end(#r2{a = 0}),
	  nop = fun(A) when (A#r1.a > 3) or (A#r2.a < 1) -> japp;
		   (_) ->
			nop
		end(#r2{a = 0}),

	  ok = fun() ->
		       F = fun(A) when (A#r1.a =:= 2) or (A#r2.a =:= 1) -> o;
			      (_) -> p
			   end,
		       p = F(#r2{a = 1}),
		       p = F(#r1{a = 2}),
		       ok
	       end(),

	  ok = fun() ->
		       F = fun(A) when fail, A#r1.a; A#r1.a -> ab;
			      (_) -> bu
			   end,
		       ab = F(#r1{a = true}),
		       bu = F(#r2{a = true}),
		       ok
	       end(),

	  both = fun(A) when A#r.a, A#r.b -> both
		 end(#r{a = true, b = true}),

	  ok = fun() ->
		       F = fun(A, B) when ((A#r1.a) orelse (B#r2.a))
					  or (B#r2.b) or (A#r1.b) -> true;
			      (_, _) -> false
			   end,
		       true = F(#r1{a = false, b = false}, #r2{a = false, b = true}),
		       false = F(#r1{a = true, b = true}, #r1{a = false, b = true}),
		       ok
	       end(),

	  ok.">>,
    [ok] = scan(C),
	  ok.

%% OTP-5916. erlang:is_record/3 allowed in guards.
otp_5916(Config) when is_list(Config) ->
    C = <<"
        rd(r1, {a,b}),
	  rd(r2, {a,b}),

	  true = if erlang:is_record(#r1{},r1,3) -> true; true ->  false end,
	  false = if erlang:is_record(#r2{},r1,3) -> true; true ->  false end,

	  true = if is_record(#r1{},r1,3) -> true; true ->  false end,
	  false = if is_record(#r2{},r1,3) -> true; true ->  false end,

	  ok.">>,
    [ok] = scan(C),
	  ok.


%% OTP-5327. Adopted from parts of emulator/test/bs_match_misc_SUITE.erl.
bs_match_misc_SUITE(Config) when is_list(Config) ->
    C = <<"
      F1 = fun() -> 3.1415 end,

	  FOne = fun() -> 1.0 end,

	  Fcmp = fun(F1, F2) when (F1 - F2) / F2 < 0.0000001 -> ok end,

	  MakeSubBin = fun(Bin0) ->
			       Sz = size(Bin0),
			       Bin1 = <<37,Bin0/binary,38,39>>,
			       <<_:8,Bin:Sz/binary,_:8,_:8>> = Bin1,
			       Bin
		       end,

	  MatchFloat =
	  fun(Bin0, Fsz, I) ->
		  Bin = MakeSubBin(Bin0),
		  Bsz = size(Bin) * 8,
		  Tsz = Bsz - Fsz - I,
		  <<_:I,F:Fsz/float,_:Tsz>> = Bin,
		  F
	  end,

	  TFloat = fun() ->
			   F = F1(),
			   G = FOne(),

			   G = MatchFloat(<<63,128,0,0>>, 32, 0),
			   G = MatchFloat(<<63,240,0,0,0,0,0,0>>, 64, 0),

			   Fcmp(F, MatchFloat(<<F:32/float>>, 32, 0)),
			   Fcmp(F, MatchFloat(<<F:64/float>>, 64, 0)),
			   Fcmp(F, MatchFloat(<<1:1,F:32/float,127:7>>, 32, 1)),
			   Fcmp(F, MatchFloat(<<1:1,F:64/float,127:7>>, 64, 1)),
			   Fcmp(F, MatchFloat(<<1:13,F:32/float,127:3>>, 32, 13)),
			   Fcmp(F, MatchFloat(<<1:13,F:64/float,127:3>>, 64, 13))
		   end,
	  TFloat(),

	  F2 = fun() -> 2.7133 end,

	  MatchFloatLittle = fun(Bin0, Fsz, I) ->
				     Bin = MakeSubBin(Bin0),
				     Bsz = size(Bin) * 8,
				     Tsz = Bsz - Fsz - I,
				     <<_:I,F:Fsz/float-little,_:Tsz>> = Bin,
				     F
			     end,

	  LittleFloat = fun() ->
				F = F2(),
				G = FOne(),

				G = MatchFloatLittle(<<0,0,0,0,0,0,240,63>>, 64, 0),
				G = MatchFloatLittle(<<0,0,128,63>>, 32, 0),

				Fcmp(F, MatchFloatLittle(<<F:32/float-little>>, 32, 0)),
				Fcmp(F, MatchFloatLittle(<<F:64/float-little>>, 64, 0)),
				Fcmp(F, MatchFloatLittle(<<1:1,F:32/float-little,127:7>>, 32, 1)),
				Fcmp(F, MatchFloatLittle(<<1:1,F:64/float-little,127:7>>, 64, 1)),
				Fcmp(F, MatchFloatLittle(<<1:13,F:32/float-little,127:3>>, 32, 13)),
				Fcmp(F, MatchFloatLittle(<<1:13,F:64/float-little,127:3>>, 64, 13))
			end,
	  LittleFloat(),

	  Sean1 = fun(<<B/binary>>) when size(B) < 4 -> small;
		     (<<1, _B/binary>>) -> large
		  end,

	  Sean = fun() ->
			 small = Sean1(<<>>),
			 small = Sean1(<<1>>),
			 small = Sean1(<<1,2>>),
			 small = Sean1(<<1,2,3>>),
			 large = Sean1(<<1,2,3,4>>),

			 small = Sean1(<<4>>),
			 small = Sean1(<<4,5>>),
			 small = Sean1(<<4,5,6>>),
			 {'EXIT',{function_clause,_}} = (catch Sean1(<<4,5,6,7>>))
		 end,
	  Sean(),

	  NativeBig = fun() ->
			      <<37.33:64/native-float>> = <<37.33:64/big-float>>,
			      <<3974:16/native-integer>> = <<3974:16/big-integer>>
		      end,

	  NativeLittle = fun() ->
				 <<37869.32343:64/native-float>> = <<37869.32343:64/little-float>>,
				 <<7974:16/native-integer>> = <<7974:16/little-integer>>
			 end,

	  Native = fun() ->
			   <<3.14:64/native-float>> = <<3.14:64/native-float>>,
			   <<333:16/native>> = <<333:16/native>>,
			   <<38658345:32/native>> = <<38658345:32/native>>,
			   case <<1:16/native>> of
			       <<0,1>> -> NativeBig();
			       <<1,0>> -> NativeLittle()
			   end
		   end,
	  Native(),

	  Split = fun(<<N:16,B:N/binary,T/binary>>) -> {B,T} end,

	  Split2 = fun(N, <<N:16,B:N/binary,T/binary>>) -> {B,T} end,

	  Split_2 = fun(<<N0:8,N:N0,B:N/binary,T/binary>>) -> {B,T} end,

	  Skip = fun(<<N:8,_:N/binary,T/binary>>) -> T end,

	  SizeVar = fun() ->
			    {<<45>>,<<>>} = Split(<<1:16,45>>),
			    {<<45>>,<<46,47>>} = Split(<<1:16,45,46,47>>),
			    {<<45,46>>,<<47>>} = Split(<<2:16,45,46,47>>),

			    {<<45,46,47>>,<<48>>} = Split_2(<<16:8,3:16,45,46,47,48>>),

			    {<<45,46>>,<<47>>} = Split2(2, <<2:16,45,46,47>>),
			    {'EXIT',{function_clause,_}} =
				(catch Split2(42, <<2:16,45,46,47>>)),

			    <<\"cdef\">> = Skip(<<2:8,\"abcdef\">>)
       end,
			      SizeVar(),

			      Wcheck = fun(<<A>>) when A==3-> ok1;
					  (<<_,_:2/binary>>) -> ok2;
					  (<<_>>) -> ok3;
					  (Other) -> {error,Other}
				       end,

			      Wiger = fun()  ->
					      ok1 = Wcheck(<<3>>),
					      ok2 = Wcheck(<<1,2,3>>),
					      ok3 = Wcheck(<<4>>),
					      {error,<<1,2,3,4>>} = Wcheck(<<1,2,3,4>>),
					      {error,<<>>} = Wcheck(<<>>)
				      end,
			      Wiger(),

			      ok.
">>,
    [ok] = scan(C),
ok = evaluate(C, []).

%% This one is not run during night builds since it takes several minutes.

%% OTP-5327. Adopted from emulator/test/bs_match_int_SUITE.erl.
bs_match_int_SUITE(Config) when is_list(Config) ->
    C = <<"
       FunClause = fun({'EXIT',{function_clause,_}}) -> ok end,

	  Mkbin = fun(L) when list(L) -> list_to_binary(L) end,

	  GetInt1 = fun(<<I:0>>) -> I;
		       (<<I:8>>) -> I;
		       (<<I:16>>) -> I;
		       (<<I:24>>) -> I;
		       (<<I:32>>) -> I
		    end,

	  GetInt2 = fun(Bin0, I, F) when size(Bin0) < 4 ->
			    Bin = <<0,Bin0/binary>>,
			    I = GetInt1(Bin),
			    F(Bin, I, F);
		       (_, I, _F) -> I
		    end,

	  GetInt = fun(Bin) ->
			   I = GetInt1(Bin),
			   GetInt2(Bin, I, GetInt2)
		   end,


	  Cmp128 = fun(<<I:128>>, I) -> equal;
		      (_, _) -> not_equal
		   end,

	  Uint2 = fun([H|T], Acc, F) -> F(T, Acc bsl 8 bor H, F);
		     ([], Acc, _F) -> Acc
		  end,

	  Uint = fun(L) -> Uint2(L, 0, Uint2) end,

	  Integer = fun() ->
			    0 = GetInt(Mkbin([])),
			    0 = GetInt(Mkbin([0])),
			    42 = GetInt(Mkbin([42])),
			    255 = GetInt(Mkbin([255])),
			    256 = GetInt(Mkbin([1,0])),
			    257 = GetInt(Mkbin([1,1])),
			    258 = GetInt(Mkbin([1,2])),
			    258 = GetInt(Mkbin([1,2])),
			    65534 = GetInt(Mkbin([255,254])),
			    16776455 = GetInt(Mkbin([255,253,7])),
			    4245492555 = GetInt(Mkbin([253,13,19,75])),
			    4294967294 = GetInt(Mkbin([255,255,255,254])),
			    4294967295 = GetInt(Mkbin([255,255,255,255])),
			    Eight = [200,1,19,128,222,42,97,111],
			    Cmp128(Eight, Uint(Eight)),
			    FunClause(catch GetInt(Mkbin(lists:seq(1,5))))
		    end,
	  Integer(),

	  Sint = fun(Bin) ->
			 case Bin of
			     <<I:8/signed>> -> I;
			     <<I:8/signed,_:3,_:5>> -> I;
			     Other -> {no_match,Other}
			 end
		 end,

	  SignedInteger = fun() ->
				  {no_match,_} = Sint(Mkbin([])),
				  {no_match,_} = Sint(Mkbin([1,2,3])),
				  127 = Sint(Mkbin([127])),
				  -1 = Sint(Mkbin([255])),
				  -128 = Sint(Mkbin([128])),
				  42 = Sint(Mkbin([42,255])),
				  127 = Sint(Mkbin([127,255]))
			  end,
	  SignedInteger(),

	  Dynamic5 = fun(Bin, S1, S2, A, B) ->
			     case Bin of
				 <<A:S1,B:S2>> ->
                              %% io:format(\"~p ~p ~p ~p~n\", [S1,S2,A,B]),
				     ok;
				 _Other -> erlang:error(badmatch, [Bin,S1,S2,A,B])
			     end
		     end,

	  Dynamic2 = fun(Bin, S1, F) when S1 >= 0 ->
			     S2 = size(Bin) * 8 - S1,
			     Dynamic5(Bin, S1, S2, (1 bsl S1) - 1, (1 bsl S2) - 1),
			     F(Bin, S1-1, F);
			(_, _, _) -> ok
		     end,

	  Dynamic = fun(Bin, S1) ->
			    Dynamic2(Bin, S1, Dynamic2)
		    end,

	  Dynamic(Mkbin([255]), 8),
	  Dynamic(Mkbin([255,255]), 16),
	  Dynamic(Mkbin([255,255,255]), 24),
	  Dynamic(Mkbin([255,255,255,255]), 32),

	  BigToLittle4 =
	  fun([B0,B1,B2,B3,B4,B5,B6,B7|T], N, Acc, F) when N >= 8 ->
		  F(T, N-8, [B0,B1,B2,B3,B4,B5,B6,B7|Acc], F);
	     (List, N, Acc, _F) -> lists:sublist(List, 1, N) ++ Acc
	  end,

	  BigToLittle =
	  fun(List, N) -> BigToLittle4(List, N, [], BigToLittle4) end,

	  ReversedSublist =
	  fun(_List, 0, Acc, _F) -> Acc;
	     ([H|T], N, Acc, F) -> F(T, N-1, [H|Acc], F)
	  end,

	  TwoComplementAndReverse =
	  fun([H|T], Carry, Acc, F) ->
		  Sum = 1-H+Carry,
		  F(T, Sum div 2, [Sum rem 2|Acc], F);
	     ([], Carry, Acc, _F) -> [Carry|Acc]
	  end,

	  MakeInt = fun(_List, 0, Acc, _F) -> Acc;
		       ([H|T], N, Acc, F) -> F(T, N-1, Acc bsl 1 bor H, F)
		    end,

	  MakeSignedInt =
	  fun(_List, 0) -> 0;
	     ([0|_]=List, N) -> MakeInt(List, N, 0, MakeInt);
	     ([1|_]=List0, N) ->
		  List1 = ReversedSublist(List0, N, [], ReversedSublist),
		  List2 = TwoComplementAndReverse(List1, 1, [],
						  TwoComplementAndReverse),
		  -MakeInt(List2, length(List2), 0, MakeInt)
	  end,

	  BitsToList =
	  fun([H|T], 0, F) -> F(T, 16#80, F);
	     ([H|_]=List, Mask, F) ->
		  [case H band Mask of
                       0 -> 0;
                       _ -> 1
		   end | F(List, Mask bsr 1, F)];
	     ([], _, _F) -> []
	  end,

	  MoreDynamic3 =
	  fun(Action, Bin, List, Bef, Aft, F) when Bef =< Aft ->
		  Action(Bin, List, Bef, Aft-Bef),
		  F(Action, Bin, List, Bef, Aft-1, F);
	     (_, _, _, _, _, _) -> ok
	  end,

	  MoreDynamic2 =
	  fun(Action, Bin, [_|T]=List, Bef, F) ->
		  MoreDynamic3(Action, Bin, List, Bef, size(Bin)*8,
			       MoreDynamic3),
		  F(Action, Bin, T, Bef+1, F);
	     (_, _, [], _, _F) -> ok
	  end,

	  MoreDynamic1 =
	  fun(Action, Bin) ->
		  BitList = BitsToList(binary_to_list(Bin),16#80,BitsToList),
		  MoreDynamic2(Action, Bin, BitList, 0, MoreDynamic2)
	  end,

	  MoreDynamic = fun() ->
           %% Unsigned big-endian numbers.
				Unsigned  = fun(Bin, List, SkipBef, N) ->
						    SkipAft = 8*size(Bin) - N - SkipBef,
						    <<_:SkipBef,Int:N,_:SkipAft>> = Bin,
						    Int = MakeInt(List, N, 0, MakeInt)
					    end,
				MoreDynamic1(Unsigned, erlang:md5(Mkbin([42]))),

           %% Signed big-endian numbers.
				Signed  = fun(Bin, List, SkipBef, N) ->
						  SkipAft = 8*size(Bin) - N - SkipBef,
						  <<_:SkipBef,Int:N/signed,_:SkipAft>> = Bin,
						  case MakeSignedInt(List, N) of
						      Int -> ok;
						      Other ->
							  io:format(\"Bin = ~p,\", [Bin]),
                                     io:format(\"SkipBef = ~p, N = ~p\", 
                                               [SkipBef,N]),
								    io:format(\"Expected ~p, got ~p\",
                                               [Int,Other])
								    end
								    end,
								    MoreDynamic1(Signed, erlang:md5(Mkbin([43]))),

           %% Unsigned little-endian numbers.
								    UnsLittle  = fun(Bin, List, SkipBef, N) ->
											 SkipAft = 8*size(Bin) - N - SkipBef,
											 <<_:SkipBef,Int:N/little,_:SkipAft>> = Bin,
											 Int = MakeInt(BigToLittle(List, N), N, 0,
												       MakeInt)
										 end,
								    MoreDynamic1(UnsLittle, erlang:md5(Mkbin([44]))),

           %% Signed little-endian numbers.
								    SignLittle  = fun(Bin, List, SkipBef, N) ->
											  SkipAft = 8*size(Bin) - N - SkipBef,
											  <<_:SkipBef,Int:N/signed-little,_:SkipAft>> = Bin,
											  Little = BigToLittle(List, N),
											  Int = MakeSignedInt(Little, N)
										  end,
								    MoreDynamic1(SignLittle, erlang:md5(Mkbin([45])))
								    end,
								    MoreDynamic(),

								    ok.
">>,
    [ok] = scan(C),
ok = evaluate(C, []).

%% OTP-5327. Adopted from emulator/test/bs_match_tail_SUITE.erl.
bs_match_tail_SUITE(Config) when is_list(Config) ->
    C = <<"
          GetTailUsed = fun(<<A:1,T/binary>>) -> {A,T} end,

          GetTailUnused = fun(<<A:15,_/binary>>) -> A end,

          GetDynTailUsed = fun(Bin, Sz) ->
				   <<A:Sz,T/binary>> = Bin,
				   {A,T}
                           end,

          GetDynTailUnused = fun(Bin, Sz) ->
				     <<A:Sz,_/binary>> = Bin,
				     A
                             end,

          Mkbin = fun(L) when list(L) -> list_to_binary(L) end,

          TestZeroTail = fun(<<A:8>>) -> A end,

          TestZeroTail2 = fun(<<_A:4,_B:4>>) -> ok end,

          ZeroTail = fun() ->
			     7 = (catch TestZeroTail(Mkbin([7]))),
			     {'EXIT',{function_clause,_}} =
				 (catch TestZeroTail(Mkbin([1,2]))),
			     {'EXIT',{function_clause,_}} =
				 (catch TestZeroTail2(Mkbin([1,2,3])))
		     end,
          ZeroTail(),

          AlGetTailUsed = fun(<<A:16,T/binary>>) -> {A,T} end,

          AlGetTailUnused = fun(<<A:16,_/binary>>) -> A end,

          Aligned = fun() ->
			    Tail1 = Mkbin([]),
			    {258,Tail1} = AlGetTailUsed(Mkbin([1,2])),
			    Tail2 = Mkbin(lists:seq(1, 127)),
			    {35091,Tail2} = AlGetTailUsed(Mkbin([137,19|Tail2])),

			    64896 = AlGetTailUnused(Mkbin([253,128])),
			    64895 = AlGetTailUnused(Mkbin([253,127|lists:seq(42, 255)])),

			    Tail3 = Mkbin(lists:seq(0, 19)),
			    {0,Tail1} = GetDynTailUsed(Tail1, 0),
			    {0,Tail3} = GetDynTailUsed(Mkbin([Tail3]), 0),
			    {73,Tail3} = GetDynTailUsed(Mkbin([73|Tail3]), 8),

			    0 = GetDynTailUnused(Mkbin([]), 0),
			    233 = GetDynTailUnused(Mkbin([233]), 8),
			    23 = GetDynTailUnused(Mkbin([23,22,2]), 8)
		    end,
          Aligned(),

          UnAligned = fun() ->
			      {'EXIT',{function_clause,_}} =
				  (catch GetTailUsed(Mkbin([42]))),
			      {'EXIT',{{badmatch,_},_}} =
				  (catch GetDynTailUsed(Mkbin([137]), 3)),
			      {'EXIT',{function_clause,_}} =
				  (catch GetTailUnused(Mkbin([42,33]))),
			      {'EXIT',{{badmatch,_},_}} =
				  (catch GetDynTailUnused(Mkbin([44]), 7))
		      end,
          UnAligned(),
          ok.
">>,
    [ok] = scan(C),
ok = evaluate(C, []).

%% OTP-5327. Adopted from emulator/test/bs_match_bin_SUITE.erl.
bs_match_bin_SUITE(Config) when is_list(Config) ->
    ByteSplitBinary = 
        <<"ByteSplit = 
             fun(L, B, Pos, Fun) when Pos >= 0 ->
                     Sz1 = Pos,
                     Sz2 = size(B) - Pos,
                     <<B1:Sz1/binary,B2:Sz2/binary>> = B,
                     B1 = list_to_binary(lists:sublist(L, 1, Pos)),
                     B2 = list_to_binary(lists:nthtail(Pos, L)),
		     Fun(L, B, Pos-1, Fun);
		(L, B, _, _Fun) -> ok
             end,
	  Mkbin = fun(L) when list(L) -> list_to_binary(L) end,
	  L = lists:seq(0, 57),
	  B = Mkbin(L),
	  ByteSplit(L, B, size(B), ByteSplit),
	  Id = fun(I) -> I end,
	  MakeUnalignedSubBinary =
	  fun(Bin0) ->
		  Bin1 = <<0:3,Bin0/binary,31:5>>,
		  Sz = size(Bin0),
		  <<0:3,Bin:Sz/binary,31:5>> = Id(Bin1),
		  Bin
	  end,
	  Unaligned = MakeUnalignedSubBinary(B),
	  ByteSplit(L, Unaligned, size(Unaligned), ByteSplit),
	  ok.
">>,
    [ok] = scan(ByteSplitBinary),
ok = evaluate(ByteSplitBinary, []),
BitSplitBinary =
<<"Mkbin = fun(L) when list(L) -> list_to_binary(L) end,

           MakeInt = 
  fun(List, 0, Acc, _F) -> Acc;
     ([H|T], N, Acc, F) -> F(T, N-1, Acc bsl 1 bor H, F)
  end,

  MakeBinFromList =
  fun(List, 0, _F) -> Mkbin([]);
     (List, N, F) ->
	  list_to_binary([MakeInt(List, 8, 0, MakeInt),
			  F(lists:nthtail(8, List), N-8, F)])
  end,

  BitSplitBinary3 =
  fun(Action, Bin, List, Bef, Aft, F) when Bef =< Aft ->
	  Action(Bin, List, Bef, (Aft-Bef) div 8 * 8),
	  F(Action, Bin, List, Bef, Aft-8, F);
     (_, _, _, _, _, _) -> ok
  end,

  BitSplitBinary2 =
  fun(Action, Bin, [_|T]=List, Bef, F) ->
	  BitSplitBinary3(Action, Bin, List, Bef, size(Bin)*8,
			  BitSplitBinary3),
	  F(Action, Bin, T, Bef+1, F);
     (Action, Bin, [], Bef, F) -> ok
  end,

  BitsToList =
  fun([H|T], 0, F) -> F(T, 16#80, F);
     ([H|_]=List, Mask, F) ->
	  [case H band Mask of
	       0 -> 0;
	       _ -> 1
	   end | F(List, Mask bsr 1, F)];
     ([], _, _F) -> []
  end,

  BitSplitBinary1 =
  fun(Action, Bin) ->
	  BitList = BitsToList(binary_to_list(Bin), 16#80,
			       BitsToList),
	  BitSplitBinary2(Action, Bin, BitList, 0, BitSplitBinary2)
  end,

  Fun = fun(Bin, List, SkipBef, N) ->
		SkipAft = 8*size(Bin) - N - SkipBef,
		<<I1:SkipBef,OutBin:N/binary-unit:1,I2:SkipAft>> = Bin,
		OutBin = MakeBinFromList(List, N, MakeBinFromList)
	end,

  BitSplitBinary1(Fun, erlang:md5(<<1,2,3>>)),
  Id = fun(I) -> I end,
  MakeUnalignedSubBinary =
  fun(Bin0) ->
	  Bin1 = <<0:3,Bin0/binary,31:5>>,
	  Sz = size(Bin0),
	  <<0:3,Bin:Sz/binary,31:5>> = Id(Bin1),
	  Bin
  end,
  BitSplitBinary1(Fun, MakeUnalignedSubBinary(erlang:md5(<<1,2,3>>))),
  ok.
">>,
    [ok] = scan(BitSplitBinary),
ok = evaluate(BitSplitBinary, []).

-define(FAIL(Expr), "{'EXIT',{badarg,_}} = (catch " ??Expr ")").

-define(COF(Int0),
        "(fun(Int) ->
                       true = <<Int:32/float>> =:= <<(float(Int)):32/float>>,
	true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
	    end)(Nonliteral(" ??Int0 ")),
true = <<" ??Int0 ":32/float>> =:= <<(float("??Int0")):32/float>>,
true = <<" ??Int0 ":64/float>> =:= <<(float("??Int0")):64/float>>").

-define(COF64(Int0),
        "(fun(Int) ->
                       true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
	    end)(Nonliteral(" ??Int0 ")),
true = <<" ??Int0 ":64/float>> =:= <<(float("??Int0")):64/float>>").

%% OTP-5327. Adopted from parts of emulator/test/bs_construct_SUITE.erl.
bs_construct_SUITE(Config) when is_list(Config) ->
    C1 = <<"

       Testf_1 = fun(W, B) -> "
            ?FAIL(<<42:W>>) ","
				  ?FAIL(<<3.14:W/float>>) ","
				  ?FAIL(<<B:W/binary>>) "
       end,

	   TestF = fun() -> "
            ?FAIL(<<3.14>>) ","
				?FAIL(<<<<1,2>>>>) ","

				?FAIL(<<2.71/binary>>) ","
				?FAIL(<<24334/binary>>) ","
				?FAIL(<<24334344294788947129487129487219847/binary>>) ","

				?FAIL(<<<<1,2,3>>/float>>) ",

            %% Negative field widths.
            Testf_1(-8, <<1,2,3,4,5>>),"

            ?FAIL(<<42:(-16)>>) ","
				?FAIL(<<3.14:(-8)/float>>) ","
				?FAIL(<<<<23,56,0,2>>:(-16)/binary>>) ","
				?FAIL(<<<<23,56,0,2>>:(2.5)/binary>>) ","
				?FAIL(<<<<23,56,0,2>>:(anka)>>) "
       end,                 
	   TestF(),

	   NotUsed1 = fun(I, BinString) -> <<I:32,BinString/binary>>, ok end,

	   NotUsed2 = fun(I, Sz) -> <<I:Sz>>, ok end,

	   NotUsed3 = fun(I) -><<I:(-8)>>, ok end,

	   NotUsed = fun() ->
			     ok = NotUsed1(3, <<\"dum\">>),
            {'EXIT',{badarg,_}} = (catch NotUsed1(3, \"dum\")), "
						  ?FAIL(NotUsed2(444, -2)) ","
						  ?FAIL(NotUsed2(444, anka)) ","
						  ?FAIL(NotUsed3(444)) "
       end,
						  NotUsed(),

						  InGuard3 = fun(Bin, A, B) when <<A:13,B:3>> == Bin -> 1;
								(Bin, A, B) when <<A:16,B/binary>> == Bin -> 2;
								(Bin, A, B) when <<A:14,B/float,3:2>> == Bin -> 3;
								(Bin, A, B) when {a,b,<<A:14,B/float,3:2>>} == Bin ->
								     cant_happen;
								(_, _, _) -> nope
							     end,

						  InGuard = fun() ->
								    1 = InGuard3(<<16#74ad:16>>, 16#e95, 5),
								    2 = InGuard3(<<16#3A,16#F7,\"hello\">>, 16#3AF7, <<\"hello\">>),
            3 = InGuard3(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
										   nope = InGuard3(<<1>>, 42, b),
										   nope = InGuard3(<<1>>, a, b),
										   nope = InGuard3(<<1,2>>, 1, 1),
										   nope = InGuard3(<<4,5>>, 1, 2.71),
										   nope = InGuard3(<<4,5>>, 1, <<12,13>>)
										   end,
										   InGuard(),

										   Nonliteral = fun(X) -> X end,

										   CoerceToFloat = fun() -> "
           ?COF(0) ","
														?COF(-1) ","
														?COF(1) ","
														?COF(42) ","
														?COF(255) ","
														?COF(-255) ","
														?COF64(298748888888888888888888888883478264866528467367364766666666666666663) ","
														?COF64(-367546729879999999999947826486652846736736476555566666663) "
       end,
										   CoerceToFloat(),
										   ok.
">>,
    [ok] = scan(C1),
ok = evaluate(C1, []),

%% There is another one, lib/compiler/test/bs_construct_SUITE.erl...
C2 = <<"
       I = fun(X) -> X end,

       Fail = fun() -> 

		      I_minus_777 = I(-777),
		      I_minus_2047 = I(-2047),

         %% One negative field size, but the sum of field sizes will be 1 byte.
         %% Make sure that we reject that properly.

		      {'EXIT',{badarg,_}} = (catch <<I_minus_777:2048/unit:8,
						     57:I_minus_2047/unit:8>>),

         %% Same thing, but use literals.
		      {'EXIT',{badarg,_}} = (catch <<I_minus_777:2048/unit:8,
						     57:(-2047)/unit:8>>),

         %% Bad alignment.
		      I_one = I(1),
		      <<1:1>> = <<2375:I_one>>,
		      <<3:2>> = <<45:1,2375:I_one>>,
		      <<14:4>> = <<45:1,2375:I_one,918:2>>,
		      <<118:7>> = <<45:1,2375:I_one,918:5>>,

         %% Not numbers.
		      {'EXIT',{badarg,_}} = (catch <<45:(I(not_a_number))>>),
		      {'EXIT',{badarg,_}} = (catch <<13:8,45:(I(not_a_number))>>),

         %% Unaligned sizes.
		      BadSz = I(7),
		      <<2:4>> = <<34:4>>,
		      <<34:7>> = <<34:BadSz>>,

		      [] = [X || {X} <- [], X == <<3:BadSz>>],
		      [] = [X || {X} <- [], X == <<3:4>>]
	      end,
       Fail(),

       FloatBin1 = fun(F) ->
			   {<<1,2,3>>,F+3.0}
		   end,

       FloatBin = fun() ->
           %% Some more coverage.
			  {<<1,2,3>>,7.0} = FloatBin1(4)
		  end,
       FloatBin(),

       ok.
">>,
    [ok] = scan(C2),
ok = evaluate(C2, []).

evaluate(B, Vars) when is_binary(B) ->
    evaluate(binary_to_list(B), Vars);
evaluate(Str, Vars) ->
    {ok,Tokens,_} =
	erl_scan:string(Str),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    case erl_eval:exprs(Exprs, Vars, none) of
	{value, Result, _} ->
	    Result
    end.


%% Bit syntax examples from the Reference Manual. OTP-5237.
refman_bit_syntax(Config) when is_list(Config) ->
    %% Reference Manual "Bit Syntax Expressions"
    Bin1 = <<1,17,42>>,
    true = [1,17,42] =:= binary_to_list(Bin1),
    Bin2 = <<"abc">>,
    true = "abc" =:= binary_to_list(Bin2),
    Bin3 = <<1,17,42:16>>,
    true = [1,17,0,42] =:= binary_to_list(Bin3),
    <<_A,_B,C:16>> = <<1,17,42:16>>,
    true = C =:= 42,
    <<D:16,_E,F>> = <<1,17,42:16>>,
    true = D =:= 273,
    true = F =:= 42,
    <<_G,H/binary>> = <<1,17,42:16>>,
    true = H =:= <<17,0,42>>,

    [ok] =
        scan(<<"Bin1 = <<1,17,42>>,
                         true = [1,17,42] =:= binary_to_list(Bin1),
	       Bin2 = <<\"abc\">>,
                         true = \"abc\" =:= binary_to_list(Bin2),
                         Bin3 = <<1,17,42:16>>,
			true =
			[1,17,0,42] =:= binary_to_list(Bin3),
			<<A,B,C:16>> = <<1,17,42:16>>,
			true = C =:= 42,
			<<D:16,E,F>> = <<1,17,42:16>>,
			true = D =:= 273,
			true = F =:= 42,
			<<G,H/binary>> = <<1,17,42:16>>,
			true = H =:= <<17,0,42>>,
			ok.">>),

    %% Binary comprehensions.
    <<2,4,6>> =  << << (X*2) >> || <<X>> <= << 1,2,3 >> >>,
			ok.


-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

%% Bit syntax examples from Programming Examples. OTP-5237.
progex_bit_syntax(Config) when is_list(Config) ->
    Bin11 = <<1, 17, 42>>,
    true = [1, 17, 42] =:= binary_to_list(Bin11),
    Bin12 = <<"abc">>,
    true = [97, 98, 99] =:= binary_to_list(Bin12),

    A = 1, B = 17, C = 42,
    Bin2 = <<A, B, C:16>>,
    true = [1, 17, 00, 42] =:= binary_to_list(Bin2),
    <<D:16, E, F/binary>> = Bin2,
    true = D =:= 273,
    true = E =:= 00,
    true = [42] =:= binary_to_list(F),

    Fun4 = fun(Dgram) ->
                   DgramSize = byte_size(Dgram),
                   case Dgram of 
                       <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16, 
			 ID:16, Flgs:3, FragOff:13,
			 TTL:8, Proto:8, HdrChkSum:16,
			 SrcIP:32, DestIP:32,
			 RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
                           OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
                           <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
                           {SrvcType, TotLen, Flgs, FragOff, ID, HdrChkSum,
                            Proto, TTL, SrcIP, DestIP, Data, Opts};
                       _ ->
                           not_ok
                   end
           end,
    true = Fun4(<<>>) =:= not_ok,
    true = is_tuple(Fun4(list_to_binary([<<?IP_VERSION:4,5:4>>,
                                         list_to_binary(lists:seq(1,255))]))),

    X = 23432324, Y = 24324234,
    <<10:7>> = <<X:1, Y:6>>,
    Z = 234324324,
    XYZ = <<X:1, Y:6, Z:1>>,
    true = [20] =:= binary_to_list(XYZ),
    Hello1 = <<"hello">>,
    Hello2 = <<$h,$e,$l,$l,$o>>,
    true = "hello" =:= binary_to_list(Hello1),
    true = "hello" =:= binary_to_list(Hello2),

    FunM1 = fun(<<X1:7/binary, Y1:1/binary>>) -> {X1,Y1} end,
    true = {<<"1234567">>,<<"8">>} =:= FunM1(<<"12345678">>),

    FunM2 = fun(<<_X1:7/binary-unit:7, _Y1:1/binary-unit:1>>) -> ok;
               (_) -> not_ok end,
    true = not_ok =:= FunM2(<<"1">>),

    BL = [{3,4,5},{6,7,8}],
    Lst = [0,0,0,3,0,0,0,4,0,0,0,5,0,0,0,6,0,0,0,7,0,0,0,8],
    B1 = triples_to_bin1(BL),
    true = Lst =:= binary_to_list(B1),
    B2 = triples_to_bin2(BL),
    true = Lst =:= binary_to_list(B2),

    [ok] = scan(
	     <<"Bin11 = <<1, 17, 42>>,
        true = [1, 17, 42] =:= binary_to_list(Bin11),
	       Bin12 = <<\"abc\">>,
        true = [97, 98, 99] =:= binary_to_list(Bin12),

			 A = 1, B = 17, C = 42,
			 Bin2 = <<A, B, C:16>>,
			 true = [1, 17, 00, 42] =:= binary_to_list(Bin2),
			 <<D:16, E, F/binary>> = Bin2,
			 true = D =:= 273,
			 true = E =:= 00,
			 true = [42] =:= binary_to_list(F),

			 Fun4 = fun(Dgram) ->
					DgramSize = byte_size(Dgram),
					case Dgram of
					    <<4:4, HLen:4, SrvcType:8, TotLen:16,
					      ID:16, Flgs:3, FragOff:13,
					      TTL:8, Proto:8, HdrChkSum:16,
					      SrcIP:32, DestIP:32,
					      RestDgram/binary>> when HLen>=5,
								      4*HLen=<DgramSize ->
						OptsLen = 4*(HLen - 5),
						<<Opts:OptsLen/binary,Data/binary>> = RestDgram,
						{SrvcType, TotLen, Flgs, FragOff, ID, HdrChkSum,
						 Proto, TTL, SrcIP, DestIP, Data, Opts};
					    _ ->
						not_ok
					end
				end,
			 true = Fun4(<<>>) =:= not_ok,
			 true = is_tuple(Fun4(list_to_binary
						([<<4:4,5:4>>,list_to_binary(lists:seq(1,255))]))),

			 X = 23432324, Y = 24324234,
			 <<10:7>> = <<X:1, Y:6>>,
			 Z = 234324324,
			 XYZ = <<X:1, Y:6, Z:1>>,
			 true = [20] =:= binary_to_list(XYZ),
			 Hello1 = <<\"hello\">>,
        Hello2 = <<$h,$e,$l,$l,$o>>,
				    true = \"hello\" =:= binary_to_list(Hello1),
        true = \"hello\" =:= binary_to_list(Hello2),

        FunM1 = fun(<<X1:7/binary, Y1:1/binary>>) -> {X1,Y1} end,
				    true = {<<\"1234567\">>,<<\"8\">>} =:= FunM1(<<\"12345678\">>),

        FunM2 = fun(<<_X1:7/binary-unit:7, _Y1:1/binary-unit:1>>) -> ok;
                   (_) -> not_ok end,
					      true = not_ok =:= FunM2(<<\"1\">>),
        ok.">>),

    ok.

triples_to_bin1(T) ->
    triples_to_bin1(T, <<>>).

triples_to_bin1([{X,Y,Z} | T], Acc) ->
    triples_to_bin1(T, <<Acc/binary, X:32, Y:32, Z:32>>);   % inefficient
triples_to_bin1([], Acc) -> 
    Acc.

triples_to_bin2(T) ->
    triples_to_bin2(T, []).

triples_to_bin2([{X,Y,Z} | T], Acc) ->
    triples_to_bin2(T, [<<X:32, Y:32, Z:32>> | Acc]);
triples_to_bin2([], Acc) -> 
    list_to_binary(lists:reverse(Acc)).

%% Record examples from Programming Examples. OTP-5237.
progex_records(Config) when is_list(Config) ->
    Test1 = 
	<<"-module(recs).
          -record(person, {name = \"\", phone = [], address}).
          -record(name, {first = \"Robert\", last = \"Ericsson\"}).
          -record(person2, {name = #name{}, phone}).
-export([t/0]).

t() ->
    _P1 = #person{phone=[0,8,2,3,4,3,1,2], name=\"Robert\"},
              \"Robert\" = _P1#person.name,
              [0,8,2,3,4,3,1,2] = _P1#person.phone,
		  undefined = _P1#person.address,

		  _P2 = #person{name = \"Jakob\", _ = '_'},
               \"Jakob\" = _P2#person.name,
              '_' = _P2#person.phone,
				'_' = _P2#person.address,

				P = #person{name = \"Joe\", phone = [0,8,2,3,4,3,1,2]},
              \"Joe\" = P#person.name,
              [0,8,2,3,4,3,1,2] = P#person.phone,
					    undefined = P#person.address,

					    P1 = #person{name=\"Joe\", phone=[1,2,3], address=\"A street\"},
              P2 = P1#person{name=\"Robert\"},
              \"Robert\" = P2#person.name,
              [1,2,3] = P2#person.phone,
			     \"A street\" = P2#person.address,
              a_person = foo(P1),

			     {found, [1,2,3]} =
				 find_phone([#person{name = a},
					     #person{name = b, phone = [3,2,1]},
					     #person{name = c, phone = [1,2,3]}],
					    c),

			     P3 = #person{name=\"Joe\", phone=[0,0,7], address=\"A street\"},
              #person{name = Name} = P3, 
					  \"Joe\" = Name,

              \"Robert\" = demo(),
              ok.

foo(P) when is_record(P, person) -> a_person;
foo(_) -> not_a_person.

find_phone([#person{name=Name, phone=Phone} | _], Name) ->
    {found,  Phone};
find_phone([_| T], Name) ->
    find_phone(T, Name);
find_phone([], _Name) ->
    not_found.

demo() ->
    P = #person2{name= #name{first=\"Robert\",last=\"Virding\"},
                               phone=123},
		 _First = (P#person2.name)#name.first.
">>,
    ok = run_file(Config, recs, Test1),

Test1_shell =
<<"rd(person, {name = \"\", phone = [], address}),
          rd(name, {first = \"Robert\", last = \"Ericsson\"}),
          rd(person2, {name = #name{}, phone}),

		    _P1 = #person{phone=[0,8,2,3,4,3,1,2], name=\"Robert\"},
          \"Robert\" = _P1#person.name,
          [0,8,2,3,4,3,1,2] = _P1#person.phone,
				  undefined = _P1#person.address,

				  _P2 = #person{name = \"Jakob\", _ = '_'},
           \"Jakob\" = _P2#person.name,
          '_' = _P2#person.phone,
						'_' = _P2#person.address,

						P = #person{name = \"Joe\", phone = [0,8,2,3,4,3,1,2]},
          \"Joe\" = P#person.name,
          [0,8,2,3,4,3,1,2] = P#person.phone,
							    undefined = P#person.address,

							    P1 = #person{name=\"Joe\", phone=[1,2,3], address=\"A street\"},
          P2 = P1#person{name=\"Robert\"},
          \"Robert\" = P2#person.name,
          [1,2,3] = P2#person.phone,
			 \"A street\" = P2#person.address,
          Foo = fun(P) when is_record(P, person) -> a_person;
                   (_) -> not_a_person
                end,
			 a_person = Foo(P1),

			 Find = fun([#person{name=Name, phone=Phone} | _], Name, Fn) ->
					{found,  Phone};
				   ([_| T], Name, Fn) ->
					Fn(T, Name, Fn);
				   ([], _Name, _Fn) ->
					not_found
				end,

			 {found, [1,2,3]} = Find([#person{name = a},
						  #person{name = b, phone = [3,2,1]},
						  #person{name = c, phone = [1,2,3]}],
						 c,
						 Find),

			 P3 = #person{name=\"Joe\", phone=[0,0,7], address=\"A street\"},
          #person{name = Name} = P3, 
				      \"Joe\" = Name,

          Demo = fun() ->
			 P17 = #person2{name= #name{first=\"Robert\",last=\"Virding\"},
                           phone=123},
					_First = (P17#person2.name)#name.first
					end,

					\"Robert\" = Demo(),
          ok.
">>,
    [ok] = scan(Test1_shell),

Test2 =
<<"-module(recs).
          -record(person, {name, age, phone = [], dict = []}).
-compile(export_all).

t() -> ok.

make_hacker_without_phone(Name, Age) ->
    #person{name = Name, age = Age,
	    dict = [{computer_knowledge, excellent},
		    {drinks, coke}]}.
print(#person{name = Name, age = Age,
	      phone = Phone, dict = Dict}) ->
    io:format(\"Name: ~s, Age: ~w, Phone: ~w ~n\"
                        \"Dictionary: ~w.~n\", [Name, Age, Phone, Dict]).

          birthday(P) when record(P, person) -> 
		     P#person{age = P#person.age + 1}.

register_two_hackers() ->
    Hacker1 = make_hacker_without_phone(\"Joe\", 29),
             OldHacker = birthday(Hacker1),
             %% The central_register_server should have
             %% an interface function for this.
					central_register_server ! {register_person, Hacker1},
					central_register_server ! {register_person,
								   OldHacker#person{name = \"Robert\",
                                        phone = [0,8,3,2,4,5,3,1]}}.
">>,
    ok = run_file(Config, recs, Test2),
ok.

%% List comprehension examples from Programming Examples. OTP-5237.
progex_lc(Config) when is_list(Config) ->
    Test1 = 
	<<"-module(lc).
          -export([t/0]).

t() ->
    [a,4,b,5,6] = [X || X <- [1,2,a,3,4,b,5,6], X > 3],
    [4,5,6] = [X || X <- [1,2,a,3,4,b,5,6], integer(X), X > 3],
    [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] =
	[{X, Y} || X <- [1,2,3], Y <- [a,b]],

    [1,2,3,4,5,6,7,8] = sort([4,5,1,8,3,6,7,2]),
    [[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]] =
	perms([b,u,g]),
    [] = pyth(11),
    [{3,4,5},{4,3,5}] = pyth(12),
    [{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{8,15,17},
     {9,12,15},{12,5,13},{12,9,15},{12,16,20},{15,8,17},
     {16,12,20}] = pyth(50),
    [] = pyth1(11),
    [{3,4,5},{4,3,5}] = pyth1(12),
    [{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{8,15,17},
     {9,12,15},{12,5,13},{12,9,15},{12,16,20},{15,8,17},
     {16,12,20}] = pyth1(50),
    [1,2,3,4,5] = append([[1,2,3],[4,5]]),
    [2,3,4] = map(fun(X) -> X + 1 end, [1,2,3]),
    [2,4] = filter(fun(X) -> X > 1 end, [0,2,4]),
    [1,2,3,7] = select(b,[{a,1},{b,2},{c,3},{b,7}]),
    [2,7] = select2(b,[{a,1},{b,2},{c,3},{b,7}]),
    ok.

sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot]) ++
	[Pivot] ++
	sort([ X || X <- T, X >= Pivot]);
sort([]) -> [].

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

pyth(N) ->
    [ {A,B,C} ||
	A <- lists:seq(1,N),
	B <- lists:seq(1,N),
	C <- lists:seq(1,N),
	A+B+C =< N,
	A*A+B*B == C*C
    ].

pyth1(N) ->
    [{A,B,C} ||
	A <- lists:seq(1,N),
	B <- lists:seq(1,N-A+1),
	C <- lists:seq(1,N-A-B+2),
	A+B+C =< N,
	A*A+B*B == C*C ].

append(L)   ->  [X || L1 <- L, X <- L1].
map(Fun, L) -> [Fun(X) || X <- L].
filter(Pred, L) -> [X || X <- L, Pred(X)].

select(X, L) ->  [Y || {X, Y} <- L].
select2(X, L) ->  [Y || {X1, Y} <- L, X == X1].
">>,
    ok = run_file(Config, lc, Test1),

Test1_shell =
<<"[a,4,b,5,6] = [X || X <- [1,2,a,3,4,b,5,6], X > 3],
          [4,5,6] = [X || X <- [1,2,a,3,4,b,5,6], integer(X), X > 3],
  [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}] =
  [{X, Y} || X <- [1,2,3], Y <- [a,b]],

  Sort = fun([Pivot|T], Fn) ->
		 Fn([ X || X <- T, X < Pivot], Fn) ++
		     [Pivot] ++
		     Fn([ X || X <- T, X >= Pivot], Fn);
	    ([], _Fn) -> []
	 end,

  [1,2,3,4,5,6,7,8] = Sort([4,5,1,8,3,6,7,2], Sort),
  Perms = fun([], _Fn) -> [[]];
	     (L, Fn)  -> [[H|T] || H <- L, T <- Fn(L--[H], Fn)]
	  end,
  [[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]] =
  Perms([b,u,g], Perms),

  Pyth = fun(N) ->
		 [ {A,B,C} ||
		     A <- lists:seq(1,N),
		     B <- lists:seq(1,N),
		     C <- lists:seq(1,N),
		     A+B+C =< N,
		     A*A+B*B == C*C
		 ]
	 end,

  [] = Pyth(11),
  [{3,4,5},{4,3,5}] = Pyth(12),
%%[{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{8,15,17},
%% {9,12,15},{12,5,13},{12,9,15},{12,16,20},{15,8,17},
%% {16,12,20}] = Pyth(50),

  Pyth1 = fun(N) ->
		  [{A,B,C} ||
		      A <- lists:seq(1,N),
		      B <- lists:seq(1,N-A+1),
		      C <- lists:seq(1,N-A-B+2),
		      A+B+C =< N,
		      A*A+B*B == C*C ]
	  end,

  [] = Pyth1(11),
  [{3,4,5},{4,3,5}] = Pyth1(12),
  [{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{8,15,17},
   {9,12,15},{12,5,13},{12,9,15},{12,16,20},{15,8,17},
   {16,12,20}] = Pyth1(50),

  Append = fun(L)   ->  [X || L1 <- L, X <- L1] end,
  [1,2,3,4,5] = Append([[1,2,3],[4,5]]),
  Map = fun(Fun, L) -> [Fun(X) || X <- L] end,
  [2,3,4] = Map(fun(X) -> X + 1 end, [1,2,3]),
  Filter = fun(Pred, L) -> [X || X <- L, Pred(X)] end,
  [2,4] = Filter(fun(X) -> X > 1 end, [0,2,4]),

  Select = fun(X, L) ->  [Y || {X, Y} <- L] end,
  [1,2,3,7] = Select(b,[{a,1},{b,2},{c,3},{b,7}]),
  Select2 = fun(X, L) ->  [Y || {X1, Y} <- L, X == X1] end,
  [2,7] = Select2(b,[{a,1},{b,2},{c,3},{b,7}]),
  ok.
">>,
    [ok] = scan(Test1_shell),
ok.

%% Funs examples from Programming Examples. OTP-5237.
progex_funs(Config) when is_list(Config) ->
    Test1 = 
	<<"-module(funs).
          -compile(export_all).

double([H|T]) -> [2*H|double(T)];
double([])    -> [].

add_one([H|T]) -> [H+1|add_one(T)];
add_one([])    -> [].

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].

double2(L)  -> map(fun(X) -> 2*X end, L).
add_one2(L) -> map(fun(X) -> 1 + X end, L).

print_list(Stream, [H|T]) ->
    io:format(Stream, \"~p~n\", [H]),
              print_list(Stream, T);
		  print_list(Stream, []) ->
		     true.

broadcast(Msg, [Pid|Pids]) ->
    Pid ! Msg,
    broadcast(Msg, Pids);
broadcast(_, []) ->
    true.

foreach(F, [H|T]) ->
    F(H),
    foreach(F, T);
foreach(F, []) ->
    ok.

print_list2(S, L) ->
    foreach(fun(H) -> io:format(S, \"~p~n\",[H]) end, L).

          broadcast2(M, L) -> foreach(fun(Pid) -> Pid ! M end, L).

t1() -> map(fun(X) -> 2 * X end, [1,2,3,4,5]).

t2() -> map(fun double/1, [1,2,3,4,5]).

t3() -> map({?MODULE, double3}, [1,2,3,4,5]).

double3(X) -> X * 2.

f(F, Args) when function(F) ->
    apply(F, Args);
f(N, _) when integer(N) ->
    N.

print_list3(File, List) ->
    {ok, Stream} = file:open(File, write),
    foreach(fun(X) -> io:format(Stream,\"~p~n\",[X]) end, List),
              file:close(Stream).

print_list4(File, List) ->
    {ok, Stream} = file:open(File, write),
    foreach(fun(File) ->
		    io:format(Stream,\"~p~n\",[File])
                      end, List),
		    file:close(Stream).

any(Pred, [H|T]) ->
    case Pred(H) of
	true  ->  true;
	false ->  any(Pred, T)
    end;
any(Pred, []) ->
    false.

all(Pred, [H|T]) ->
    case Pred(H) of
	true  ->  all(Pred, T);
	false ->  false
    end;
all(Pred, []) ->
    true.

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) -> Accu.

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs], Accu2};
mapfoldl(F, Accu, []) -> {[], Accu}.

filter(F, [H|T]) ->
    case F(H) of
	true  -> [H|filter(F, T)];
	false -> filter(F, T)
    end;
filter(F, []) -> [].

diff(L1, L2) ->
    filter(fun(X) -> not lists:member(X, L2) end, L1).

intersection(L1,L2) -> filter(fun(X) -> lists:member(X,L1) end, L2).

takewhile(Pred, [H|T]) ->
    case Pred(H) of
	true  -> [H|takewhile(Pred, T)];
	false -> []
    end;
takewhile(Pred, []) ->
    [].

dropwhile(Pred, [H|T]) ->
    case Pred(H) of
	true  -> dropwhile(Pred, T);
	false -> [H|T]
    end;
dropwhile(Pred, []) ->
    [].

splitlist(Pred, L) ->
    splitlist(Pred, L, []).

splitlist(Pred, [H|T], L) ->
    case Pred(H) of
	true  -> splitlist(Pred, T, [H|L]);
	false -> {lists:reverse(L), [H|T]}
    end;
splitlist(Pred, [], L) ->
    {lists:reverse(L), []}.

first(Pred, [H|T]) ->
    case Pred(H) of
	true ->
	    {true, H};
	false ->
	    first(Pred, T)
    end;
first(Pred, []) ->
    false.

ints_from(N) ->
    fun() ->
	    [N|ints_from(N+1)]
    end.

pconst(X) ->
    fun (T) ->
	    case T of
		[X|T1] -> {ok, {const, X}, T1};
		_      -> fail
	    end
    end.

pand(P1, P2) ->
    fun (T) ->
	    case P1(T) of
		{ok, R1, T1} ->
		    case P2(T1) of
			{ok, R2, T2} ->
			    {ok, {'and', R1, R2}};
			fail ->
			    fail
		    end;
		fail ->
		    fail
	    end
    end.

por(P1, P2) ->
    fun (T) ->
	    case P1(T) of
		{ok, R, T1} ->
		    {ok, {'or',1,R}, T1};
		fail ->
		    case P2(T) of
			{ok, R1, T1} ->
			    {ok, {'or',2,R1}, T1};
			fail ->
			    fail
		    end
	    end
    end.

grammar() ->
    pand(
      por(pconst(a), pconst(b)),
      por(pconst(c), pconst(d))).

parse(List) ->
    (grammar())(List).


t() ->
    [2,4,6,8] = double([1,2,3,4]),
    [2,3,4,5] = add_one([1,2,3,4]),
    [2,4,6,8] = double2([1,2,3,4]),
    [2,3,4,5] = add_one2([1,2,3,4]),
    XX = ints_from(1),
    [1 | _] = XX(),
    1 = hd(XX()),
    Y = tl(XX()),
    2 = hd(Y()),

    P1 = pconst(a),
    {ok,{const,a},[b,c]} = P1([a,b,c]),
    fail = P1([x,y,z]),

    {ok,{'and',{'or',1,{const,a}},{'or',1,{const,c}}}} =
	parse([a,c]),
    {ok,{'and',{'or',1,{const,a}},{'or',2,{const,d}}}} =
	parse([a,d]),
    {ok,{'and',{'or',2,{const,b}},{'or',1,{const,c}}}} =
	parse([b,c]),
    {ok,{'and',{'or',2,{const,b}},{'or',2,{const,d}}}} =
	parse([b,d]),
    fail = parse([a,b]),
    ok.
">>,
    ok = run_file(Config, funs, Test1),

Test2_shell =
<<"Double = fun(X) -> 2 * X end,
          [2,4,6,8,10] = lists:map(Double, [1,2,3,4,5]),

  Big =  fun(X) -> if X > 10 -> true; true -> false end end,
  false = lists:any(Big, [1,2,3,4]),
  true = lists:any(Big, [1,2,3,12,5]),
  false = lists:all(Big, [1,2,3,4,12,6]),
  true = lists:all(Big, [12,13,14,15]),
  L = [\"I\",\"like\",\"Erlang\"],
          11 = lists:foldl(fun(X, Sum) -> length(X) + Sum end, 0, L),
       Upcase =  fun(X) when $a =< X,  X =< $z -> X + $A - $a;
		    (X) -> X
		 end,
       Upcase_word = fun(X) -> lists:map(Upcase, X) end,
       \"ERLANG\" = Upcase_word(\"Erlang\"),
          [\"I\",\"LIKE\",\"ERLANG\"] = lists:map(Upcase_word, L),
          {[\"I\",\"LIKE\",\"ERLANG\"],11} =
               lists:mapfoldl(fun(Word, Sum) -> 
				      {Upcase_word(Word), Sum + length(Word)}
                              end, 0, L),
	    [500,12,45] = lists:filter(Big, [500,12,2,45,6,7]),
	    [200,500,45] = lists:takewhile(Big, [200,500,45,5,3,45,6]),
	    [5,3,45,6] = lists:dropwhile(Big, [200,500,45,5,3,45,6]),
	    {[200,500,45],[5,3,45,6]} =
		lists:splitwith(Big, [200,500,45,5,3,45,6]),
          %% {true,45} = lists:first(Big, [1,2,45,6,123]),
          %% false = lists:first(Big, [1,2,4,5]),

	    Adder = fun(X) -> fun(Y) -> X + Y end end,
	    Add6 = Adder(6),
	    16 = Add6(10),
	    ok.
">>,
    [ok] = scan(Test2_shell),
ok.


%% OTP-5990. {erlang,is_record}.
otp_5990(Config) when is_list(Config) ->
    [true] =
        scan(<<"rd('OrdSet', {orddata = {},ordtype = type}), "
               "S = #'OrdSet'{ordtype = {}}, "
               "if tuple(S#'OrdSet'.ordtype) -> true; true -> false end.">>),
    ok.

%% OTP-6166. Order of record definitions.
otp_6166(Config) when is_list(Config) ->
    Test1 = filename:join(proplists:get_value(priv_dir, Config), "test1.hrl"),
    Contents1 = <<"-module(test1).
                   -record(r5, {f}). -record(r3, {f = #r5{}}). "
                  "-record(r1, {f = #r3{}}). -record(r4, {f = #r1{}}). "
"-record(r2, {f = #r4{}}).">>,
    ok = file:write_file(Test1, Contents1),

    Test2 = filename:join(proplists:get_value(priv_dir, Config), "test2.hrl"),
    Contents2 = <<"-module(test2).
                   -record(r5, {f}). -record(r3, {f = #r5{}}). "
                  "-record(r1, {f = #r3{}}). -record(r4, {f = #r1{}}). "
                  "-record(r2, {f = #r4{}}).
                   -record(r6, {f = #r5{}}).            % r6 > r0
                   -record(r0, {f = #r5{}, g = #r5{}}). % r0 < r5">>,
    ok = file:write_file(Test2, Contents2),
    
    RR12 = "[r1,r2,r3,r4,r5] = rr(\"" ++ Test1 ++ "\"), 
            [r0,r1,r2,r3,r4,r5,r6] = rr(\"" ++ Test2 ++ "\"), 
            R0 = #r0{}, R6 = #r6{}, 
            true = is_record(R0, r0),
            true = is_record(R6, r6),
            ok. ",
    [ok] = scan(RR12),

    file:delete(Test1),
    file:delete(Test2),
    ok.

%% OTP-6554. Formatted exits and error messages.
otp_6554(Config) when is_list(Config) ->
    %% Should check the stacktrace as well...
    "exception error: bad argument" =
        comm_err(<<"math:sqrt(a).">>),
    "exception error: bad argument" =
        comm_err(<<"fun(X, Y) -> X ++ Y end(a, b).">>),
    "exception error: bad argument" =
        comm_err(<<"math:sqrt(lists:seq(1,40)).">>),
    "exception error: bad argument" =
        comm_err(<<"math:sqrt(lists:seq(1,10)).">>),
    "exception error: bad argument" =
        comm_err(<<"a ++ b.">>),
    "exception error: bad argument" =
        comm_err(<<"I = {file_info,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined},
                    aa ++ I.">>),
    "exception error: bad argument" =
        comm_err(<<"I = {file_info,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined},
                    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ++ I.">>),
    "exception error: bad argument" =
        comm_err(<<"I = {file_info,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined},
                    I ++ I.">>),
    "exception error: bad argument" =
        comm_err(<<"fun(X) -> not X end(a).">>),
    "exception error: bad argument: a" =
        comm_err(<<"fun(A, B) -> A orelse B end(a, b).">>),
    "exception error: an error occurred when evaluating an arithmetic expression" =
        comm_err(<<"math:sqrt(2)/round(math:sqrt(0)).">>),
    "exception error: interpreted function with arity 1 called with no arguments" =
        comm_err(<<"fun(V) -> V end().">>),
    "exception error: interpreted function with arity 1 called with two arguments" =
        comm_err(<<"fun(V) -> V end(1,2).">>),
    "exception error: interpreted function with arity 0 called with one argument" =
        comm_err(<<"fun() -> v end(1).">>),
    "exception error: interpreted function with arity 0 called with 4 arguments" =
        comm_err(<<"fun() -> v end(1,2,3,4).">>),
    "exception error: math:sqrt/1 called with two arguments" =
        comm_err(<<"fun math:sqrt/1(1,2).">>),
    "exception error: bad function 1." ++ _ =
        comm_err(<<"(math:sqrt(2))().">>),
    "exception error: bad function [1," ++ _ =
        comm_err(<<"(lists:seq(1, 100))().">>),
    "exception error: no match of right hand side value 1" ++ _ =
        comm_err(<<"a = math:sqrt(2).">>),
    "exception error: no match of right hand side value" ++ _ =
        comm_err(<<"I = {file_info,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined,undefined,
                         undefined,undefined,undefined,undefined},
                    a = I.">>),
    "exception error: no case clause matching 1" ++ _ =
        comm_err(<<"case math:sqrt(2) of a -> ok end.">>),
    "exception error: no case clause matching [1," ++ _ =
        comm_err(<<"V = lists:seq(1, 20), case V of a -> ok end.">>),
    "exception error: no function clause matching" =
        comm_err(<<"fun(P) when is_pid(P) -> true end(a).">>),
    case test_server:is_native(erl_eval) of
	true ->
	    %% Native code has different exit reason. Don't bother
	    %% testing them.
	    ok;
	false ->
	    "exception error: {function_clause," =
		comm_err(<<"erlang:error(function_clause, "
			  "[unproper | list]).">>),
	    %% Cheating:
	    "exception error: no function clause matching "
		"erl_eval:do_apply(4)" ++ _ =
		comm_err(<<"erlang:error(function_clause, [4]).">>),
		"exception error: no function clause matching "
		"lists:reverse(" ++ _ =
		comm_err(<<"F=fun() -> hello end, lists:reverse(F).">>),
		"exception error: no function clause matching "
		"lists:reverse(34) (lists.erl, line " ++ _ =
		comm_err(<<"lists:reverse(34).">>)
    end,
    "exception error: function_clause" =
        comm_err(<<"erlang:error(function_clause, 4).">>),
    "exception error: no function clause matching" ++ _ =
        comm_err(<<"fun(a, b, c, d) -> foo end"
                   "       (lists:seq(1,17),"
                   "        lists:seq(1, 18),"
                   "        lists:seq(1, 40),"
                   "        lists:seq(1, 5)).">>),

    "exception error: no function clause matching" =
        comm_err(<<"fun(P, q) when is_pid(P) -> true end(a, b).">>),
    "exception error: no true branch found when evaluating an if expression" =
        comm_err(<<"if length([a,b]) > 17 -> a end.">>),
    "exception error: no such process or port" =
        comm_err(<<"Pid = spawn(fun() -> a end),"
                   "timer:sleep(1),"
                   "link(Pid).">>),
    "exception error: a system limit has been reached" =
        comm_err(<<"list_to_atom(lists:duplicate(300,$a)).">>),
    "exception error: bad receive timeout value" =
        comm_err(<<"receive after a -> foo end.">>),
    "exception error: no try clause matching 1" ++ _ =
        comm_err(<<"try math:sqrt(2) of bar -> yes after 3 end.">>),
    "exception error: no try clause matching [1" ++ _ =
        comm_err(<<"V = lists:seq(1, 20),"
                   "try V of bar -> yes after 3 end.">>),
    "exception error: undefined function math:sqrt/2" =
        comm_err(<<"math:sqrt(2, 2).">>),
    "exception error: limit of number of arguments to interpreted function "
          "exceeded" = 
        comm_err(<<"fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) ->"
                   "   a end().">>),
    "exception error: bad filter a" =
        comm_err(<<"[b || begin a end].">>),
    "exception error: bad generator a" =
        comm_err(<<"[X || X <- a].">>),
    "exception throw: undef" = comm_err(<<"throw(undef).">>),
    "exception exit: undef" = comm_err(<<"exit(undef).">>),

    "exception exit: foo" =
          comm_err(<<"catch spawn_link(fun() ->"
                     "                     timer:sleep(300), exit(foo) "
                     "                 end),"
                     "timer:sleep(500).">>),
    [ok] = scan(
                   <<"begin process_flag(trap_exit, true),"
                     "  Pid = spawn_link(fun() ->"
                     "                       timer:sleep(300), exit(foo) "
                     "                   end),"
                     "  timer:sleep(500),"
                     "  receive {'EXIT', Pid, foo} -> ok end end.">>),
    "exception exit: badarith" =
          comm_err(<<"catch spawn_link(fun() ->"
                     "                     timer:sleep(300), 1/0 "
                     "                 end),"
                     "timer:sleep(500).">>),
    "exception exit: {nocatch,foo}" =
          comm_err(<<"catch spawn_link(fun() ->"
                     "                     timer:sleep(300), throw(foo) "
                     "                 end),"
                     "timer:sleep(500).">>),
    [ok] = scan(
                   <<"begin process_flag(trap_exit, true),"
                     "  Pid = spawn_link(fun() ->"
                     "                       timer:sleep(300), throw(foo) "
                     "                   end),"
                     "  timer:sleep(500),"
                     "  receive {'EXIT', Pid, {{nocatch,foo},_}} -> ok end "
                     "end.">>),

    "exception error: an error occurred when evaluating an arithmetic expression" =
        comm_err(<<"begin catch_exception(true), 1/0 end.">>),
    "exception error: an error occurred when evaluating an arithmetic expression" =
        comm_err(<<"begin catch_exception(false), 1/0 end.">>),
    "exception error: no function clause matching call to catch_exception/1" =
        comm_err(<<"catch_exception(1).">>),

    %% A bug was corrected (expansion of 'try'):
    "2: command not found" =
        comm_err(<<"try 1 of 1 -> v(2) after 3 end.">>),
    %% Cover a few lines:
    "3: command not found" =
        comm_err(<<"receive foo -> foo after 0 -> v(3) end.">>),
    "3: command not found" =
        comm_err(<<"receive foo -> foo after 0 -> e(3) end.">>),
    "1 / 0: command not found" = comm_err(<<"v(1/0).">>),
    "1\n1.\n" = t(<<"1. e(1).">>),
    [ok] = scan(<<"h().">>),
    "exception exit: normal" = comm_err(<<"exit(normal).">>),
    [foo] = scan(<<"begin history(0), foo end.">>),
    application:unset_env(stdlib, shell_history_length),
    [true] = scan(<<"begin <<10:(1024*1024*10)>>,"
                          "<<10:(1024*1024*10)>>, garbage_collect() end.">>),
    "1: syntax error before: '.'" = comm_err("1-."),
    %% comm_err(<<"exit().">>), % would hang
    "exception error: no function clause matching call to history/1" =
        comm_err(<<"history(foo).">>),
    "exception error: no function clause matching call to results/1" =
        comm_err(<<"results(foo).">>),

    Test = filename:join(proplists:get_value(priv_dir, Config),
			       "otp_6554.erl"),
    Contents = <<"-module(otp_6554).
                  -export([local_allowed/3, non_local_allowed/3]).
                  local_allowed(_,_,State) ->
                      {true,State}.

                  non_local_allowed(_,_,State) ->
                      {true,State}.
                 ">>,
    ok = compile_file(Config, Test, Contents, []),
    "exception exit: restricted shell starts now" =
	comm_err(<<"begin shell:start_restricted(otp_6554) end.">>),
    "-record(r,{}).\n1.\nok.\n" =
                    t(<<"f(), f(B), h(), b(), history(20), results(20),"
                        "rd(r, {}), rl(r), rf('_'), rl(), rf(),"
                        "rp(1), _ = rr({foo}), _ = rr({foo}, []),"
                        "rr({foo}, [], []), ok.">>),
    "false.\n" = t(<<"catch_exception(true).">>),
    "exception exit: restricted shell stopped"=
	comm_err(<<"begin shell:stop_restricted() end.">>),
    "true.\n" = t(<<"catch_exception(false).">>),

    "20\n1\n1\n1: results(2)\n2: 1\n-> 1\n3: v(2)\n-> 1.\nok.\n" =
             t(<<"results(2). 1. v(2). h().">>),
    application:unset_env(stdlib, shell_saved_results),
    "1\nfoo\n17\nB = foo\nC = 17\nF = fun() ->\n           foo"
          "\n    end.\nok.\n" = 
        t(<<"begin F = fun() -> foo end, 1 end. B = F(). C = 17. b().">>),

    "3: command not found" = comm_err(<<"#{v(3) => v}.">>),
    "3: command not found" = comm_err(<<"#{k => v(3)}.">>),
    "3: command not found" = comm_err(<<"#{v(3) := v}.">>),
    "3: command not found" = comm_err(<<"#{k := v(3)}.">>),
    "3: command not found" = comm_err(<<"(v(3))#{}.">>),
    %% Tests I'd like to do: (you should try them manually)
    %% "catch spawn_link(fun() -> timer:sleep(1000), exit(foo) end)."
    %%   "** exception error: foo" should be output after 1 second
    %% "catch spawn_link(fun() -> timer:sleep(1000), 1/0 end)."
    %%   "** exception error: bad argument..." should be output after 1 second
    %% "1/0", "exit(foo)", "throw(foo)".
    %%   "h()." should show {'EXIT', {badarith,..}}, {'EXIT',{foo,...}},
    %%   and {'EXIT',{{nocatch,foo},...}}.

    ok.

%% OTP-7184. Propagate exit signals from dying evaluator process.
otp_7184(Config) when is_list(Config) ->
    register(otp_7184, self()),
    catch
           t(<<"P = self(),
                spawn_link(fun() -> process_flag(trap_exit,true),
                                    P ! up,
                                    receive X -> 
                                        otp_7184 ! {otp_7184, X}
                                    end 
                           end),
                receive up -> ok end,
                erlang:raise(throw, thrown, []).">>),
    receive {otp_7184,{'EXIT',_,{{nocatch,thrown},[]}}} -> ok end,

    catch
           t(<<"P = self(),
                spawn_link(fun() -> process_flag(trap_exit,true),
                                    P ! up,
                                    receive X -> 
                                        otp_7184 ! {otp_7184, X}
                                    end 
                           end),
                receive up -> ok end,
                erlang:raise(exit, fini, []).">>),
    receive {otp_7184,{'EXIT',_,{fini,[]}}} -> ok end,

    catch
           t(<<"P = self(),
                spawn_link(fun() -> process_flag(trap_exit,true),
                                    P ! up,
                                    receive X -> 
                                        otp_7184 ! {otp_7184,X}
                                    end 
                           end),
                receive up -> ok end,
                erlang:raise(error, bad, []).">>),
    receive {otp_7184,{'EXIT',_,{bad,[]}}} -> ok end,

    unregister(otp_7184),

    %% v/1, a few missed cases
    "17\n<<0,0,0,64>>.\nok.\n" =
        t(<<"17. "
            "<<64:32>>. "
           "<<64>> = << << X >> || << X >>  <= v(2), X > v(1) >>, ok.">>),

    "17\n<<0,17>>.\n" =t(<<"17. <<(v(1)):16>>.">>),

    ok.

%% OTP-7232. qlc:info() bug.
otp_7232(Config) when is_list(Config) ->
    Info = <<"qlc:info(qlc:sort(qlc:q([X || X <- [55296,56296]]), "
             "{order, fun(A,B)-> A>B end})).">>,
    "qlc:sort([55296,56296],\n"
    "         [{order,\n"
    "           fun(A, B) ->\n"
    "                  A > B\n"
    "           end}])" = evaluate(Info, []),
    ok.

%% OTP-8393. Prompt string.
otp_8393(Config) when is_list(Config) ->
    _ = shell:prompt_func(default),
    "Bad prompt function: '> '" =
        prompt_err(<<"shell:prompt_func('> ').">>),

    _ = shell:prompt_func(default),
    "exception error: an error occurred when evaluating an arithmetic expression"++_ =
        prompt_err(<<"shell:prompt_func({shell_SUITE,prompt4}).">>),

    _ = shell:prompt_func(default),
    "default.\n" =
        t(<<"shell:prompt_func({shell_SUITE,prompt2}).">>),

    _ = shell:prompt_func(default),
    "default\nl.\n" =
        t(<<"shell:prompt_func({shell_SUITE,prompt3}). l.">>),

    %%
    %% Although this tests that you can set a unicode prompt function
    %% it does not really test that it does work with the io-servers.
    %% That is instead tested in the io_proto_SUITE, which has
    %% the right infrastructure in place for such tests. /PaN
    %%
    _ = shell:prompt_func(default),
    "default\nl.\n" =
        t(<<"shell:prompt_func({shell_SUITE,prompt5}). l.">>),

    %% Restricted shell.
    Contents = <<"-module(test_restricted_shell).
                  -export([local_allowed/3, non_local_allowed/3]).
                  local_allowed(_,_,State) ->
                      {false,State}.

                  non_local_allowed({shell,stop_restricted},[],State) ->
                      {true,State};
                  non_local_allowed({shell,prompt_func},[_L],State) ->
                      {true,State};
                  non_local_allowed({shell_SUITE,prompt1},[_L],State) ->
                      {true,State};
                  non_local_allowed(_,_,State) ->
                      {false,State}.
                 ">>,
    Test = filename:join(proplists:get_value(priv_dir, Config),
			       "test_restricted_shell.erl"),
    ok = compile_file(Config, Test, Contents, []),
    _ = shell:prompt_func(default),
    "exception exit: restricted shell starts now" =
	comm_err(<<"begin shell:start_restricted("
			 "test_restricted_shell) end.">>),
    "default.\n"++_ =
        t(<<"shell:prompt_func({shell_SUITE,prompt1}).">>),
    "exception exit: restricted shell does not allow apple(" ++ _ =
	comm_err(<<"apple(1).">>),
    "{shell_SUITE,prompt1}.\n" =
        t(<<"shell:prompt_func(default).">>),
    "exception exit: restricted shell stopped"=
	comm_err(<<"begin shell:stop_restricted() end.">>),
    undefined =
	application:get_env(stdlib, restricted_shell),

    NR = shell:results(20),
    "default\n20.\n" =
        t(<<"shell:prompt_func({shell_SUITE,prompt3}). results(0).">>),

    _ = shell:prompt_func(default),
    0 = shell:results(NR),
    ok.

prompt1(_L) ->
    "prompt> ".

prompt2(_L) ->
    {'EXIT', []}.

prompt3(L) ->
    N = proplists:get_value(history, L),
    integer_to_list(N).

prompt4(_L) ->
    erlang:apply(fun erlang:'/'/2, [1,0]).

prompt5(_L) ->
    [1050,1072,1082,1074,1086,32,1077,32,85,110,105,99,111,100,101,32,63].

-ifdef(not_used).
exit_term(B) ->
    "** exception exit:" ++ Reply = t(B),
    S0 = string:left(Reply, string:chr(Reply, $\n)-1),
    S = string:strip(S0, right, $*),
    {ok,Ts,_} = erl_scan:string(S),
    {ok,Term} = erl_parse:parse_term(Ts),
    Term.
-endif.

error_string(B) ->
    "** exception error:" ++ Reply = t(B),    
    caught_string(Reply).

exit_string(B) ->
    "** exception exit:" ++ Reply = t(B),
    caught_string(Reply).

caught_string(Reply) ->
    S0 = string:left(Reply, string:chr(Reply, $\n)-1),
    S1 = string:strip(S0, right, $.),
    S2 = string:strip(S1, left, $*),
    S =  string:strip(S2, both, $ ),
    string:strip(S, both, $").

comm_err(B) ->
    Reply = t(B),
    S0 = string:left(Reply, string:chr(Reply, $\n)-1),
    S1 = string:strip(S0, left, $*),
    S2 = string:strip(S1, both, $ ),
    S = string:strip(S2, both, $"),
    string:strip(S, right, $.).

prompt_err(B) ->
    Reply = t(B),
    S00 = string:sub_string(Reply, string:chr(Reply, $\n)+1),
    S0 = string:left(S00, string:chr(S00, $\n)-1),
    S1 = string:strip(S0, left, $*),
    S2 = string:strip(S1, both, $ ),
    S = string:strip(S2, both, $"),
    string:strip(S, right, $.).

%% OTP-10302. Unicode.
otp_10302(Config) when is_list(Config) ->
    {ok,Node} = start_node(shell_suite_helper_2,
			   "-pa "++proplists:get_value(priv_dir,Config)++
			   " +pc unicode"),
    Test1 =
        <<"begin
               io:setopts([{encoding,utf8}]),
               [1024] = \"\\x{400}\",
               rd(rec, {a = \"\\x{400}\"}),
               ok = rl(rec)
            end.">>,
    "-record(rec,{a = \"\x{400}\"}).\nok.\n" = t({Node,Test1}),

    Test3 =
        <<"io:setopts([{encoding,utf8}]).
           rd(rec, {a = \"\\x{400}\"}).
           ok = rp(#rec{}).">>,
    "ok.\nrec\n#rec{a = \"\x{400}\"}.\nok.\n" = t({Node,Test3}),

    Test4 =
        <<"io:setopts([{encoding,utf8}]).
           A = [1024] = \"\\x{400}\".
           b().
           h().">>,

    "ok.\n\"\x{400}\"\nA = \"\x{400}\".\nok.\n"
    "1: io:setopts([{encoding,utf8}])\n-> ok.\n"
    "2: A = [1024] = \"\x{400}\"\n-> \"\x{400}\"\n"
    "3: b()\n-> ok.\nok.\n" = t({Node,Test4}),

    Test5 =
        <<"begin
               io:setopts([{encoding,utf8}]),
               results(0),
               A = [1024] = \"\\x{400}\",
               b(),
               h()
           end.">>,
    "A = \"\x{400}\".\nok.\n" = t({Node,Test5}),

    %% One $" is "lost":
    true =
       "\x{400}\": command not found" =:=
       prompt_err({Node,
                   <<"io:setopts([{encoding,utf8}]). v(\"\x{400}\")."/utf8>>,
                   unicode}),

    "ok.\ndefault\n* Bad prompt function: \"\x{400}\".\n" =
        t({Node,<<"io:setopts([{encoding,utf8}]). "
             "shell:prompt_func(\"\x{400}\")."/utf8>>,
           unicode}),
    rpc:call(Node,shell, prompt_func, [default]),
    _ = shell:prompt_func(default),

    %% Test lib:format_exception() (cf. OTP-6554)
    Test6 =
        <<"begin
               A = <<\"\\xaa\">>,
               S = lists:flatten(io_lib:format(\"~p/~p.\", [A, A])),
               {ok, Ts, _} = erl_scan:string(S, 1, [unicode]),
               {ok, Es} = erl_parse:parse_exprs(Ts),
               B = erl_eval:new_bindings(),
               erl_eval:exprs(Es, B)
           end.">>,

    "** exception error: an error occurred when evaluating"
        " an arithmetic expression\n     in operator  '/'/2\n"
        "        called as <<\"\xaa\">> / <<\"\xaa\">>.\n" = t(Test6),
    Test7 =
        <<"io:setopts([{encoding,utf8}]).
           A = <<\"\\xaa\">>,
           S = lists:flatten(io_lib:format(\"~p/~p.\", [A, A])),
           {ok, Ts, _} = erl_scan:string(S, 1, [unicode]),
           {ok, Es} = erl_parse:parse_exprs(Ts),
           B = erl_eval:new_bindings(),
           erl_eval:exprs(Es, B).">>,
    
    "ok.\n** exception error: an error occurred when evaluating"
        " an arithmetic expression\n     in operator  '/'/2\n"
        "        called as <<\"ª\">> / <<\"ª\">>.\n" = t({Node,Test7}),
    Test8 =
        <<"begin
               A = [1089],
               S = lists:flatten(io_lib:format(\"~tp/~tp.\", [A, A])),
               {ok, Ts, _} = erl_scan:string(S, 1, [unicode]),
               {ok, Es} = erl_parse:parse_exprs(Ts),
               B = erl_eval:new_bindings(),
               erl_eval:exprs(Es, B)
           end.">>,
    "** exception error: an error occurred when evaluating"
        " an arithmetic expression\n     in operator  '/'/2\n"
        "        called as [1089] / [1089].\n" = t(Test8),
    Test9 =
        <<"io:setopts([{encoding,utf8}]).
           A = [1089],
           S = lists:flatten(io_lib:format(\"~tp/~tp.\", [A, A])),
           {ok, Ts, _} = erl_scan:string(S, 1, [unicode]),
           {ok, Es} = erl_parse:parse_exprs(Ts),
           B = erl_eval:new_bindings(),
           erl_eval:exprs(Es, B).">>,

    "ok.\n** exception error: an error occurred when evaluating"
        " an arithmetic expression\n     in operator  '/'/2\n"
        "        called as \"\x{441}\" / \"\x{441}\".\n" = t({Node,Test9}),
    Test10 =
        <<"A = {\"1\\xaa\",
           $\\xaa,
           << <<\"hi\">>/binary >>,
           <<\"1\xaa\">>},
           fun(a) -> true end(A).">>,
    "** exception error: no function clause matching \n"
    "                    erl_eval:'-inside-an-interpreted-fun-'"
    "({\"1\xc2\xaa\",170,<<\"hi\">>,\n                                    "
    "                        <<\"1\xc2\xaa\">>}) .\n" = t(Test10),
    Test11 =
        <<"io:setopts([{encoding,utf8}]).
           A = {\"1\\xaa\",
           $\\xaa,
           << <<\"hi\">>/binary >>,
           <<\"1\xaa\">>},
           fun(a) -> true end(A).">>,

    "ok.\n** exception error: no function clause matching \n"
    "                    erl_eval:'-inside-an-interpreted-fun-'"
    "({\"1\xaa\",170,<<\"hi\">>,\n                                           "
    "                 <<\"1\xaa\"/utf8>>}) .\n"  = t({Node,Test11}),
    Test12 = <<"fun(a, b) -> false end(65, [1089]).">>,
    "** exception error: no function clause matching \n"
    "                    erl_eval:'-inside-an-interpreted-fun-'(65,[1089])"
    " .\n" = t(Test12),
    Test13 =
        <<"io:setopts([{encoding,utf8}]).
           fun(a, b) -> false end(65, [1089]).">>,
    "ok.\n** exception error: no function clause matching \n"
    "                    erl_eval:'-inside-an-interpreted-fun-'(65,\"\x{441}\")"
    " .\n" = t({Node,Test13}),

    test_server:stop_node(Node),
    ok.

otp_13719(Config) when is_list(Config) ->
    Test = <<"-module(otp_13719).
              -record(bar, {}).
              -record(foo, {bar :: #bar{}}).">>,
    File = filename("otp_13719.erl", Config),
    Beam = filename("otp_13719.beam", Config),
    ok = compile_file(Config, File, Test, []),
    RR = "rr(\"" ++ Beam ++ "\"). #foo{}.",
    "[bar,foo]\n#foo{bar = undefined}.\n" = t(RR),
    file:delete(filename("test.beam", Config)),
    file:delete(File),
    ok.

scan(B) ->
    F = fun(Ts) -> 
                case erl_parse:parse_term(Ts) of
                    {ok,Term} ->
                        Term;
                    _Error ->
                        {ok,Form} = erl_parse:parse_form(Ts),
                        Form
                end
        end,
    scan(t(B), F).

scan(S0, F) ->
    case erl_scan:tokens([], S0, 1, [unicode]) of
        {done,{ok,Ts,_},S} ->
            [F(Ts) | scan(S, F)];
        _Else ->
            []
    end.

t({Node,Bin,Enc}) when is_atom(Node),is_binary(Bin), is_atom(Enc) ->
    t0({Bin,Enc}, fun() -> start_new_shell(Node) end);
t({Node,Bin}) when is_atom(Node),is_binary(Bin) ->
    t0({Bin,latin1}, fun() -> start_new_shell(Node) end);
t(Bin) when is_binary(Bin) ->
    t0({Bin,latin1}, fun() -> start_new_shell() end);
t({Bin,Enc}) when is_binary(Bin), is_atom(Enc) ->
    t0({Bin,Enc}, fun() -> start_new_shell() end);
t(L) ->
    t(list_to_binary(L)).

t0({Bin,Enc}, F) ->
    %% Spawn a process so that io_request messages do not interfer.
    P = self(),
    C = spawn(fun() -> t1(P, {Bin, Enc}, F) end),
    receive {C, R} -> R end.

t1(Parent, {Bin,Enc}, F) ->
    io:format("*** Testing ~s~n", [binary_to_list(Bin)]),
    S = #state{bin = Bin, unic = Enc, reply = [], leader = group_leader()},
    group_leader(self(), self()),
    _Shell = F(),
    try 
        server_loop(S)
    catch exit:R -> Parent ! {self(), R};
          throw:{?MODULE,LoopReply,latin1} ->
	    L0 = binary_to_list(list_to_binary(LoopReply)),
	    [$\n | L1] = lists:dropwhile(fun(X) -> X =/= $\n end, L0),
	    Parent ! {self(), dotify(L1)};
          throw:{?MODULE,LoopReply,_Uni} ->
	    Tmp = unicode:characters_to_binary(LoopReply),
	    L0 = unicode:characters_to_list(Tmp),
	    [$\n | L1] = lists:dropwhile(fun(X) -> X =/= $\n end, L0),
	    Parent ! {self(), dotify(L1)}
    after group_leader(S#state.leader, self())
    end.

dotify([$., $\n | L]) ->
    [$., $\n | dotify(L)];
dotify([$,, $\n | L]) ->
    [$,, $\n | dotify(L)];
dotify("ok\n" ++ L) ->
    "ok.\n" ++ dotify(L);
dotify("\nok\n" ++ L) ->
    ".\nok.\n" ++ dotify(L);
dotify([$\n]) ->
    [$., $\n];
dotify([C | L]) ->
    [C | dotify(L)];
dotify([]) ->
    [].

start_new_shell() ->
    Shell = shell:start(),
    link(Shell),
    Shell.

start_new_shell(Node) ->
    Shell = rpc:call(Node,shell,start,[]),
    link(Shell),
    Shell.

%% This is a very minimal implementation of the IO protocol...

server_loop(S) ->
    receive 
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
	    server_loop(do_io_request(Request, From, S, ReplyAs));
	NotExpected ->
            exit(NotExpected)
    end.
            
do_io_request(Req, From, S, ReplyAs) ->
    case io_requests([Req], [], S) of
        {_Status,{eof,_},S1} ->
	    io_reply(From, ReplyAs, {error,terminated}),
	    throw({?MODULE,S1#state.reply,S1#state.unic});
	{_Status,Reply,S1} ->
	    io_reply(From, ReplyAs, Reply),
	    S1
    end.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

io_requests([{requests, Rs1} | Rs], Cont, S) ->
    io_requests(Rs1, [Rs | Cont], S);
io_requests([R | Rs], Cont, S) ->
    case io_request(R, S) of
        {ok, ok, S1} ->
            io_requests(Rs, Cont, S1);
        Reply ->
            Reply
    end;
io_requests([], [Rs|Cont], S) ->
    io_requests(Rs, Cont, S);
io_requests([], [], S) -> 
    {ok,ok,S}.

io_request({setopts, Opts}, S) ->
    #state{unic = OldEnc, bin = Bin} = S,
    NewEnc = case proplists:get_value(encoding, Opts) of
                 undefined -> OldEnc;
                 utf8 -> unicode;
                 New -> New
             end,
    NewBin = case {OldEnc, NewEnc} of
                 {E, E} -> Bin;
                 {latin1, _} ->
                     unicode:characters_to_binary(Bin, latin1, unicode);
                 {_, latin1} ->
                     unicode:characters_to_binary(Bin, unicode, latin1);
                 {_, _} -> Bin
             end,
    {ok, ok, S#state{unic = NewEnc, bin = NewBin}};
io_request(getopts, S) ->
    {ok,[{encoding,S#state.unic}],S};
io_request({get_geometry,columns}, S) ->
    {ok,80,S};
io_request({get_geometry,rows}, S) ->
    {ok,24,S};
io_request({put_chars,Chars}, S) ->
    {ok,ok,S#state{reply = [S#state.reply | Chars]}};
io_request({put_chars,latin1,Chars}, S) ->
    {ok,ok,S#state{reply = [S#state.reply | Chars]}};
io_request({put_chars,unicode,Chars0}, S) ->
    Chars = unicode:characters_to_list(Chars0),
    {ok,ok,S#state{reply = [S#state.reply | Chars]}};
io_request({put_chars,Mod,Func,Args}, S) ->
    case catch apply(Mod, Func, Args) of
        Chars when is_list(Chars) -> 
            io_request({put_chars,Chars}, S)
    end;
io_request({put_chars,Enc,Mod,Func,Args}, S) ->
    case catch apply(Mod, Func, Args) of
        Chars when is_list(Chars) -> 
            io_request({put_chars,Enc,Chars}, S)
    end;
io_request({get_until,_Prompt,Mod,Func,ExtraArgs}, S) ->
    get_until(Mod, Func, ExtraArgs, S, latin1);
io_request({get_until,Enc,_Prompt,Mod,Func,ExtraArgs}, S) ->
    get_until(Mod, Func, ExtraArgs, S, Enc).

get_until(Mod, Func, ExtraArgs, S, Enc) ->
    get_until_loop(Mod, Func, ExtraArgs, S, {more,[]}, Enc).

get_until_loop(M, F, As, S, {more,Cont}, Enc) ->
    Bin = S#state.bin,
    case byte_size(Bin) of
        0 ->
            get_until_loop(M, F, As, S, 
                           catch apply(M, F, [Cont,eof|As]), Enc);
	_ when S#state.unic =:= latin1 ->
	    get_until_loop(M, F, As, S#state{bin = <<>>},
			   catch apply(M, F, [Cont,binary_to_list(Bin)|As]), Enc);
	_ ->
	    get_until_loop(M, F, As, S#state{bin = <<>>},
			   catch apply(M, F, [Cont,unicode:characters_to_list(Bin)|As]), Enc)
    end;
get_until_loop(_M, _F, _As, S, {done,Res,Buf}, Enc) ->
    {ok,Res,S#state{bin = buf2bin(Buf, Enc)}};
get_until_loop(_M, F, _As, S, _Other, _Enc) ->
    {error,{error,F},S}.

buf2bin(eof,_) ->
    <<>>;
buf2bin(Buf,latin1) ->
    list_to_binary(Buf);
buf2bin(Buf,utf8) ->
    unicode:characters_to_binary(Buf,unicode,unicode);
buf2bin(Buf,unicode) ->
    unicode:characters_to_binary(Buf,unicode,unicode).

run_file(Config, Module, Test) ->
    FileName = filename(lists:concat([Module, ".erl"]), Config),
    BeamFile = filename(lists:concat([Module, ".beam"]), Config),
    LoadBeamFile = filename(Module, Config),
    ok = file:write_file(FileName, Test),
    ok = compile_file(Config, FileName, Test, []),
    code:purge(Module),
    {module, Module} = code:load_abs(LoadBeamFile), 
    ok = Module:t(),
    file:delete(FileName),
    file:delete(BeamFile),
    ok.

compile_file(Config, File, Test, Opts0) ->
    Opts = [export_all,return,{outdir,proplists:get_value(priv_dir, Config)}|Opts0],
    ok = file:write_file(File, Test),
    case compile:file(File, Opts) of
              {ok, _M, _Ws} -> ok;
              _ -> error
          end.

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, Config) ->
    filename:join(proplists:get_value(priv_dir, Config), Name).

start_node(Name, Xargs) ->
    N = test_server:start_node(Name, slave, [{args, " " ++ Xargs}]),
    global:sync(),
    N.

