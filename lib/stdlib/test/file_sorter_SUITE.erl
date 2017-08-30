%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(file_sorter_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t,test_server).
-define(privdir(_), "./file_sorter_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), proplists:get_value(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, basic/1, badarg/1, 
	 term_sort/1, term_keysort/1,
	 binary_term_sort/1, binary_term_keysort/1,
	 binary_sort/1, 
	 term_merge/1, term_keymerge/1, 
	 binary_term_merge/1, binary_term_keymerge/1,
	 binary_merge/1,
	 term_check/1, term_keycheck/1,
	 binary_term_check/1, binary_term_keycheck/1,
	 binary_check/1,
	 inout/1, misc/1, many/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [basic, badarg, term_sort, term_keysort,
     binary_term_sort, binary_term_keysort, binary_sort,
     term_merge, term_keymerge, binary_term_merge,
     binary_term_keymerge, binary_merge, term_check,
     binary_term_keycheck, binary_term_check,
     binary_term_keycheck, binary_check, inout, misc, many].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Basic test case.
basic(Config) when is_list(Config) ->
    Fmt = binary,
    Arg = {format,Fmt},
    Foo = outfile("foo", Config),
    P0 = pps(),

    F1s = [F1] = to_files([[]], Fmt, Config),
    ok = file_sorter:sort(F1),
    [] = from_files(F1, Fmt),
    ok = file_sorter:keysort(17, F1),
    [] = from_files(F1, Fmt),
    ok = file_sorter:merge(F1s, Foo),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:keymerge(17, F1s, Foo),
    [] = from_files(Foo, Fmt),
    delete_files([Foo | F1s]),

    [F2] = to_files([[foo,bar]], Fmt, Config),
    ok = file_sorter:sort([F2], F2, Arg),
    [bar,foo] = from_files(F2, Fmt),
    delete_files(F2),

    Fs1 = to_files([[foo],[bar]], Fmt, Config),
    ok = file_sorter:sort(Fs1, Foo, Arg),
    [bar,foo] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:merge(Fs1, Foo, Arg),
    [bar,foo] = from_files(Foo, Fmt),
    delete_files([Foo | Fs1]),

    Fmt2 = binary_term,
    Arg2 = {format, Fmt2},
    [F3] = to_files([[{foo,1},{bar,2}]], Fmt2, Config),
    ok = file_sorter:keysort([2], [F3], F3, Arg2),
    [{foo,1},{bar,2}] = from_files(F3, Fmt2),
    delete_files(F3),

    Fs2 = to_files([[{foo,1}],[{bar,2}]], Fmt2, Config),
    ok = file_sorter:keysort(1, Fs2, Foo, Arg2),
    [{bar,2},{foo,1}] = from_files(Foo, Fmt2),
    delete_files(Foo),
    ok = file_sorter:keymerge(1, Fs2, Foo, Arg2),
    [{bar,2},{foo,1}] = from_files(Foo, Fmt2),
    delete_files([Foo | Fs2]),

    true = P0 =:= pps(),

    ok.

%% Call functions with bad arguments.
badarg(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    BadFile = filename:join(PrivDir, "not_a_file"),
    ABadFile = filename:absname(BadFile),
    file:delete(BadFile),
    {error,{file_error,ABadFile,enoent}} =
	file_sorter:sort(BadFile),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:sort({flipp})),
    {error,{file_error,ABadFile,enoent}} =
	file_sorter:keysort(1, BadFile),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:keysort(1, {flipp})),

    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:merge([{flipp}],foo)),
    {error,{file_error,ABadFile,enoent}} =
	file_sorter:keymerge(1,[BadFile],foo),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:keymerge(1,[{flipp}],foo)),
    {'EXIT', {{badarg, _}, _}} =
	(catch file_sorter:merge(fun(X) -> X end, foo)),
    {'EXIT', {{badarg, _}, _}} =
	(catch file_sorter:keymerge(1, fun(X) -> X end, foo)),

    {error,{file_error,ABadFile,enoent}} =
	file_sorter:check(BadFile),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:check({flipp})),
    {error,{file_error,ABadFile,enoent}} =
	file_sorter:keycheck(1, BadFile),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:keycheck(1, {flipp})),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:check([{flipp}],foo)),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:keycheck(1,[{flipp}],foo)),
    {'EXIT', {{badarg, _}, _}} =
	(catch file_sorter:check(fun(X) -> X end, foo)),
    {'EXIT', {{badarg, _}, _}} =
	(catch file_sorter:keycheck(1, fun(X) -> X end, foo)),

    Fs1 = to_files([[1,2,3]], binary_term, Config),
    {'EXIT', {{badarg, flipp}, _}} =
	(catch file_sorter:check(Fs1 ++ flipp, [])),
    [F1] = Fs1,
    {error,{file_error,_,_}} =
        file_sorter:sort(Fs1, foo, [{tmpdir,F1},{size,0}]),
    delete_files(Fs1),
    Fs2 = to_files([[1,2,3]], binary_term, Config),
    {error,{file_error,_,enoent}} = 
	file_sorter:sort(Fs2, foo, [{tmpdir,filename:absname(BadFile)},
                                    {size,0}]),
    delete_files(Fs2),

    {'EXIT', {{badarg, bad}, _}} =
	(catch file_sorter:check([], [{format,term} | bad])),
    {'EXIT', {{badarg, [{flipp}]}, _}} =
	(catch file_sorter:check([{flipp}])),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch file_sorter:keycheck(1, {flipp})),
    {'EXIT', {{badarg, [{flipp}]}, _}} =
	(catch file_sorter:keycheck(2, [{flipp}])),
    {error,{file_error,_,eisdir}} = file_sorter:keycheck(1, []),
    {'EXIT', {{badarg, kp}, _}} = (catch file_sorter:keycheck(kp, [])),
    {'EXIT', {{badarg, kp}, _}} =
	(catch file_sorter:keycheck([1, kp], [])),
    {'EXIT', {{badarg, kp}, _}} =
	(catch file_sorter:keycheck([1 | kp], [])),
    {'EXIT', {{badarg, []}, _}} = (catch file_sorter:keycheck([], [])),
    {'EXIT', {{badarg, {format, foo}}, _}} =
	(catch file_sorter:check([], {format,foo})),
    {'EXIT', {{badarg, not_an_option}, _}} =
	(catch file_sorter:keycheck(7, [], [not_an_option])),
    {'EXIT', {{badarg, format}, _}} =
	(catch file_sorter:keycheck(1, [], [{format, binary}])),
    {'EXIT', {{badarg, order}, _}} =
	(catch file_sorter:keycheck(1, [], [{order, fun compare/2}])),

    do_badarg(fun(I, O) -> file_sorter:sort(I, O) end,
	      fun(Kp, I, O) -> file_sorter:keysort(Kp, I, O) end,
	      BadFile),
    do_badarg_opt(fun(I, O, X) -> file_sorter:sort(I, O, X) end,
		  fun(Kp, I, O, X) -> file_sorter:keysort(Kp, I, O, X)
		  end),
    do_badarg(fun(I, O) -> file_sorter:merge(I, O) end,
	      fun(Kp, I, O) -> file_sorter:keymerge(Kp, I, O) end,
	      BadFile),
    do_badarg_opt(fun(I, O, X) -> file_sorter:merge(I, O, X) end,
		  fun(Kp, I, O, X) -> file_sorter:keymerge(Kp, I, O, X)
		  end).

do_badarg(F, KF, BadFile) ->
    [Char | _] = BadFile,
    AFlipp = filename:absname(flipp),
    {error,{file_error,AFlipp,enoent}} = F([flipp | flopp], foo),
    {'EXIT', {{badarg, {foo,bar}}, _}} = (catch F([], {foo,bar})),
    {'EXIT', {{badarg, Char}, _}} = (catch F(BadFile, [])),
    {'EXIT', {{badarg, {flipp}}, _}} = (catch F({flipp}, [])),

    {'EXIT', {{badarg, Char}, _}} = (catch KF(1, BadFile, [])),
    {'EXIT', {{badarg, {flipp}}, _}} = (catch KF(1, {flipp}, [])),
    {error,{file_error,AFlipp,enoent}} =
	KF(2, [flipp | flopp], foo),
    {'EXIT', {{badarg, {foo,bar}}, _}} = (catch KF(1, [], {foo,bar})),
    {'EXIT', {{badarg, kp}, _}} = (catch KF(kp, [], foo)),
    {'EXIT', {{badarg, kp}, _}} = (catch KF([1, kp], [], foo)),
    {'EXIT', {{badarg, kp}, _}} = (catch KF([1 | kp], [], foo)),
    {'EXIT', {{badarg, []}, _}} = (catch KF([], [], foo)),
    ok.

do_badarg_opt(F, KF) ->
    AFlipp = filename:absname(flipp),
    {error,{file_error,AFlipp,enoent}} =
	F([flipp | flopp], foo, []),
    {'EXIT', {{badarg, {flipp}}, _}} = (catch F([{flipp}], foo, [])),
    {'EXIT', {{badarg, {out,put}}, _}} = (catch F([], {out,put}, [])),
    {'EXIT', {{badarg, not_an_option}, _}} =
	(catch F([], foo, [not_an_option])),
    {'EXIT', {{badarg, {format, foo}}, _}} =
	(catch F([], foo, {format,foo})),
    {'EXIT', {{badarg, {size,foo}}, _}} = (catch F([], foo, {size,foo})),

    {'EXIT', {{badarg, {size, -1}}, _}} = (catch F([], foo, {size,-1})),
    {'EXIT', {{badarg, {no_files, foo}}, _}} =
	(catch F([], foo, {no_files,foo})),
    {'EXIT', {{badarg, {no_files, 1}}, _}} =
	(catch F([], foo, {no_files,1})),
    {'EXIT', {{badarg, 1}, _}} = (catch F([], foo, {tmpdir,1})),
    {'EXIT', {{badarg, {order,1}}, _}} = (catch F([], foo, {order,1})),
    {'EXIT', {{badarg, {compressed, flopp}}, _}} =
	(catch F([], foo, {compressed,flopp})),
    {'EXIT', {{badarg, {unique,flopp}}, _}} =
	(catch F([], foo, {unique,flopp})),
    {'EXIT', {{badarg, {header,foo}}, _}} =
	(catch F([], foo, {header,foo})),
    {'EXIT', {{badarg, {header, 0}}, _}} =
	(catch F([], foo, {header,0})),
    {'EXIT', {{badarg, {header, 1 bsl 35}}, _}} =
	(catch F([], foo, {header,1 bsl 35})),
    {'EXIT', {{badarg, header}, _}} =
	(catch F([], foo, [{header,1},{format,term}])),

    {'EXIT', {{badarg, not_an_option}, _}} =
	(catch KF(7, [], foo, [not_an_option])),
    {'EXIT', {{badarg,format}, _}} =
	(catch KF(1, [], foo, [{format, binary}])),
    {'EXIT', {{badarg, order}, _}} =
	(catch KF(1, [], foo, [{order, fun compare/2}])),
    {'EXIT', {{badarg, {flipp}}, _}} =
	(catch KF(2, [{flipp}], foo,[])),
    {error,{file_error,AFlipp,enoent}} =
	KF(2, [flipp | flopp], foo,[]),
    {'EXIT', {{badarg, {out, put}}, _}} =
	(catch KF(1, [], {out,put}, [])),
    {'EXIT', {{badarg, kp}, _}} = (catch KF(kp, [], foo, [])),
    {'EXIT', {{badarg, kp}, _}} = (catch KF([1, kp], [], foo, [])),
    {'EXIT', {{badarg, kp}, _}} = (catch KF([1 | kp], [], foo, [])),
    ok.

%% Sort terms on files.
term_sort(Config) when is_list(Config) ->
    sort(term, [{compressed,false}], Config),
    sort(term, [{order, fun compare/2}], Config),
    sort(term, [{order, ascending}, {compressed,true}], Config),
    sort(term, [{order, descending}], Config),
    ok.

%% Keysort terms on files.
term_keysort(Config) when is_list(Config) ->
    keysort(term, [{tmpdir, ""}], Config),
    keysort(term, [{order,descending}], Config),
    ok.

%% Sort binary terms on files.
binary_term_sort(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    sort({2, binary_term}, [], Config),
    sort(binary_term, [{tmpdir, list_to_atom(PrivDir)}], Config),
    sort(binary_term, [{tmpdir,PrivDir}], Config),
    sort({3,binary_term}, [{order, fun compare/2}], Config),
    sort(binary_term, [{order, fun compare/2}], Config),
    sort(binary_term, [{order,descending}], Config),
    ok.

%% Keysort binary terms on files.
binary_term_keysort(Config) when is_list(Config) ->
    keysort({3, binary_term}, [], Config),
    keysort(binary_term, [], Config),
    keysort(binary_term, [{order,descending}], Config),
    ok.

%% Sort binaries on files.
binary_sort(Config) when is_list(Config) ->
    PrivDir = ?privdir(Config),
    sort({2, binary}, [], Config),
    sort(binary, [{tmpdir, list_to_atom(PrivDir)}], Config),
    sort(binary, [{tmpdir,PrivDir}], Config),
    sort({3,binary}, [{order, fun compare/2}], Config),
    sort(binary, [{order, fun compare/2}], Config),
    sort(binary, [{order,descending}], Config),
    ok.

%% Merge terms on files.
term_merge(Config) when is_list(Config) ->
    merge(term, [{order, fun compare/2}], Config),
    merge(term, [{order, ascending}, {compressed,true}], Config),
    merge(term, [{order, descending}, {compressed,false}], Config),
    ok.

%% Keymerge terms on files.
term_keymerge(Config) when is_list(Config) ->
    keymerge(term, [], Config),
    keymerge(term, [{order, descending}], Config),
    funmerge(term, [], Config),
    ok.

%% Merge binary terms on files.
binary_term_merge(Config) when is_list(Config) ->
    merge(binary_term, [], Config),
    merge({7, binary_term}, [], Config),
    merge({3, binary_term}, [{order, fun compare/2}], Config),
    ok.

%% Keymerge binary terms on files.
binary_term_keymerge(Config) when is_list(Config) ->
    keymerge({3, binary_term}, [], Config),
    keymerge(binary_term, [], Config),
    funmerge({3, binary_term}, [], Config),
    funmerge(binary_term, [], Config),
    ok.

%% Merge binaries on files.
binary_merge(Config) when is_list(Config) ->
    merge(binary, [], Config),
    merge({7, binary}, [], Config),
    merge({3, binary}, [{order, fun compare/2}], Config),
    ok.

%% Check terms on files.
term_check(Config) when is_list(Config) ->
    check(term, Config),
    ok.

%% Check binary terms on files.
binary_term_check(Config) when is_list(Config) ->
    check(binary_term, Config),
    ok.

%% Keycheck terms on files.
term_keycheck(Config) when is_list(Config) ->
    keycheck(term, Config),
    ok.

%% Keycheck binary terms on files.
binary_term_keycheck(Config) when is_list(Config) ->
    keycheck(binary_term, Config),
    ok.

%% Check binary terms on files.
binary_check(Config) when is_list(Config) ->
    check(binary, Config),
    ok.

%% Funs as input or output.
inout(Config) when is_list(Config) ->
    BTF = {format, binary_term},
    Foo = outfile("foo", Config),

    %% Input is fun.
    End = fun(read) -> end_of_input end,

    IF1 = fun(read) -> {[1,7,5], End} end,
    ok = file_sorter:sort(IF1, Foo, [{format, term}]),
    %% 'close' is called, but the return value is caught and ignored.
    IF2 = fun(read) -> {[1,2,3], fun(close) -> throw(ignored) end} end,
    {error, bad_object} = file_sorter:sort(IF2, Foo, BTF),

    IF3 = fun(no_match) -> foo end,
    {'EXIT', {function_clause, _}} =
	(catch file_sorter:sort(IF3, Foo)),
    IF4 = fun(read) -> throw(my_message) end,
    my_message = (catch file_sorter:sort(IF4, Foo)),
    IF5 = fun(read) -> {error, my_error} end,
    {error, my_error} = file_sorter:sort(IF5, Foo),

    %% Output is fun.
    {error, bad_object} =
	file_sorter:sort(IF2, fun(close) -> ignored end, BTF),
    Args = [{format, term}],
    {error, bad_object} =
	file_sorter:keysort(1, IF2, fun(close) -> ignored end, Args),
    OF1 = fun(close) -> fine; (L) when is_list(L) -> fun(close) -> nice end end,
    nice = file_sorter:sort(IF1, OF1, Args),
    OF2 = fun(_) -> my_return end,
    my_return = file_sorter:sort(IF1, OF2, Args),
    OF3 = fun(_) -> throw(my_message) end,
    my_message = (catch file_sorter:sort(IF1, OF3, Args)),
    OF4 = fun(no_match) -> foo end,
    {'EXIT', {function_clause, _}} =
	(catch file_sorter:sort(IF1, OF4, Args)),

    P0 = pps(),
    Fs1 = to_files([[3,1,2,5,4], [8,3,10]], term, Config),
    error = file_sorter:sort(Fs1, fun(_) -> error end, Args),
    delete_files(Fs1),

    true = P0 =:= pps(),

    %% Passing a value from the input functions to the output functions.
    IFV1 = fun(read) -> {end_of_input, 17} end,
    OFV1 = fun({value, Value}) -> ofv(Value, []) end,
    {17, []} = file_sorter:sort(IFV1, OFV1, Args),

    %% Output is not a fun. The value returned by input funs is ignored.
    %% OTP-5009.
    ok = file_sorter:sort(IFV1, Foo, [{format,term}]),
    [] = from_files(Foo, term),
    delete_files(Foo),

    ok.

ofv(Value, A) ->
    fun(close) -> 
	    {Value, lists:append(lists:reverse(A))};
       (L) when is_list(L) ->
	    ofv(Value, [L | A])
    end.

%% Many temporary files.
many(Config) when is_list(Config) ->
    Foo = outfile("foo", Config),
    PrivDir = ?privdir(Config),
    P0 = pps(),

    Args = [{format, term}],
    L1 = lists:map(fun(I) -> {one, two, three, I} end, lists:seq(1,1000)),
    L2 = lists:map(fun(I) -> {four, five, six, I} end, lists:seq(1,1000)),
    Fs2 = to_files([L1, L2], term, Config),
    ok = file_sorter:sort(Fs2, Foo, [{size,1000} | Args]),
    R = lists:sort(L1++L2),
    R = from_files(Foo, term),
    2000 = length(R),
    ok = file_sorter:sort(Fs2, Foo, [{no_files,4},{size,1000} | Args]),
    R = from_files(Foo, term),
    ok =
	file_sorter:sort(Fs2, Foo, 
			 [{no_files,4},{size,1000},{order,descending} | Args]),
    true = lists:reverse(R) =:= from_files(Foo, term),
    ok =
	file_sorter:sort(Fs2, Foo, 
			 [{no_files,4},{size,1000},
			  {order,fun compare/2} | Args]),
    R = from_files(Foo, term),
    ok = file_sorter:keysort(4, Fs2, Foo,
			     [{no_files,4},{size,1000} | Args]),
    RK = lists:keysort(4, L1++L2),
    RK = from_files(Foo, term),
    delete_files(Foo),
    ok =
	file_sorter:keysort(4, Fs2, Foo, 
			    [{no_files,4},{size,1000},{order,descending} | Args]),
    true = lists:reverse(RK) =:= from_files(Foo, term),
    delete_files(Foo),
    ok = file_sorter:keysort(4, Fs2, Foo,
			     [{size,500},{order,descending} | Args]),
    true = lists:reverse(RK) =:= from_files(Foo, term),
    delete_files(Foo),
    error = file_sorter:sort(Fs2, fun(_) -> error end,
			     [{tmpdir, PrivDir}, {no_files,3},
			      {size,10000} | Args]),

    TmpDir = filename:join(PrivDir, "tmpdir"),
    file:del_dir(TmpDir),
    ok = file:make_dir(TmpDir),
    case os:type() of
	{unix, _} ->
	    ok = file:change_mode(TmpDir, 8#0000),
	    {error, {file_error, _,_}} =
		file_sorter:sort(Fs2, fun(_M) -> foo end,
				 [{no_files,3},{size,10000},
				  {tmpdir,TmpDir} | Args]);
	_ ->
	    true
    end,
    ok = file:del_dir(TmpDir),
    delete_files(Fs2),
    true = P0 =:= pps(),
    ok.

%% Some other tests.
misc(Config) when is_list(Config) ->
    BTF = {format, binary_term},
    Foo = outfile("foo", Config),
    FFoo = filename:absname(Foo),
    P0 = pps(),

    [File] = Fs1 = to_files([[1,3,2]], term, Config),
    ok = file:write_file(Foo,<<>>),
    case os:type() of
	{unix, _} ->
	    ok = file:change_mode(Foo, 8#0000),
	    {error,{file_error,FFoo,eacces}} =
		file_sorter:sort(Fs1, Foo, {format,term});
	_ ->
	    true
    end,
    file:delete(Foo),
    NoBytes = 16, % RAM memory will never get this big, or?
    ALot = (1 bsl (NoBytes*8)) - 1,
    ok = file:write_file(File, <<ALot:NoBytes/unit:8,"foobar">>),
    FFile = filename:absname(File),
    {error, {bad_object,FFile}} =
        file_sorter:sort(Fs1, Foo, [BTF, {header, 20}]),
    ok = file:write_file(File, <<30:32,"foobar">>),
    {error, {premature_eof, FFile}} = file_sorter:sort(Fs1, Foo, BTF),
    ok = file:write_file(File, <<6:32,"foobar">>),
    {error, {bad_object,FFile}} = file_sorter:sort(Fs1, Foo, BTF),
    case os:type() of
	{unix, _} ->
	    ok = file:change_mode(File, 8#0000),
	    {error, {file_error,FFile,eacces}} =
		file_sorter:sort(Fs1, Foo),
	    {error, {file_error,FFile,eacces}} =
		file_sorter:sort(Fs1, Foo, {format, binary_term});
	_ ->
	    true
    end,
    delete_files(Fs1),
    true = P0 =:= pps(),

    %% bigger than chunksize
    E1 = <<32000:32, 10:256000>>,
    E2 = <<32000:32, 5:256000>>,
    E3 = <<32000:32, 8:256000>>,
    ok = file:write_file(Foo, [E1, E2, E3]),
    ok = file_sorter:sort([Foo], Foo, [{format,binary},{size,10000}]),
    ok = file_sorter:sort([Foo], Foo, [{format,fun(X) -> X end},
				       {size,10000}]),
    Es = list_to_binary([E2,E3,E1]),
    {ok, Es} = file:read_file(Foo),
    delete_files(Foo),
    true = P0 =:= pps(),

    %% keysort more than one element
    L = [{c,1,a},{c,2,b},{c,3,c},{b,1,c},{b,2,b},{b,3,a},{a,1,a},{a,2,b},
         {a,3,c}],
    Fs2 = to_files([L], binary_term, Config),
    ok = file_sorter:keysort([2,3], Fs2, Foo, {format, binary_term}),
    KS2_1 = from_files(Foo, binary_term),
    KS2_2 = lists:keysort(2,lists:keysort(3, L)),
    KS2_1 = KS2_2,
    ok = file_sorter:keysort([2,3], Fs2, Foo,
			     [{format, binary_term},{size,5}]),
    KS2_3 = from_files(Foo, binary_term),
    KS2_3 = KS2_2,
    ok = file_sorter:keysort([2,3,1], Fs2, Foo, {format, binary_term}),
    KS3_1 = from_files(Foo, binary_term),
    KS3_2 = lists:keysort(2, lists:keysort(3,lists:keysort(1, L))),
    KS3_1 = KS3_2,
    ok = file_sorter:keysort([2,3,1], Fs2, Foo,
			     [{format, binary_term},{size,5}]),
    KS3_3 = from_files(Foo, binary_term),
    KS3_3 = KS3_2,
    delete_files([Foo | Fs2]),
    true = P0 =:= pps(),

    %% bigger than chunksize
    %% Assumes that CHUNKSIZE = 16384. Illustrates that the Last argument
    %% of merge_files/5 is necessary. 
    EP1 = erlang:make_tuple(2728,foo),
    EP2 = lists:duplicate(2729,qqq),
    LL = [EP1, EP2, EP1, EP2, EP1, EP2],
    Fs3 = to_files([LL], binary, Config),
    ok = file_sorter:sort(Fs3, Foo, [{format,binary}, {unique,true}]),
    [EP1,EP2] = from_files(Foo, binary),
    delete_files(Foo),
    ok = file_sorter:sort(Fs3, Foo,
			  [{format,binary_term}, {unique,true},
			   {size,30000}]),
    [EP1,EP2] = from_files(Foo, binary_term),
    delete_files([Foo | Fs3]),

    true = P0 =:= pps(),

    BE1 = <<20000:32, 17:160000>>,
    BE2 = <<20000:32, 1717:160000>>,
    ok = file:write_file(Foo, [BE1,BE2,BE1,BE2]),
    ok = file_sorter:sort([Foo], Foo, [{format,binary},
				       {size,10000},
				       {unique,true}]),
    BEs = list_to_binary([BE1, BE2]),
    {ok, BEs} = file:read_file(Foo),
    delete_files(Foo),
    true = P0 =:= pps(),

    Fs4 = to_files([[7,4,1]], binary_term, Config),
    {error, {bad_term, _}} = file_sorter:sort(Fs4, Foo, {format, term}),
    delete_files([Foo | Fs4]),
    true = P0 =:= pps(),

    ok.

%%% 
%%% Utilities.
%%% 

sort(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,5} | XArgs]),
    TmpArgs = [{tmpdir,?privdir(Config)} | Args],
    Foo = outfile("foo", Config),

    %% Input is a fun. Output is a fun.
    [] = file_sorter:sort(input([], 2, Fmt), output([], Fmt), Args),
    L1 = [3,1,2,5,4],
    S1 = file_sorter:sort(input(L1, 2, Fmt), output([], Fmt), TmpArgs),
    S1 = rev(lists:sort(L1), TmpArgs),

    %% Input is a file. Output is a fun.
    [] = file_sorter:sort([], output([], Fmt), Args),
    L2 = [3,1,2,5,4],
    Fs1 = to_files([L2], Fmt, Config),
    S2 = file_sorter:sort(Fs1, output([], Fmt), TmpArgs),
    S2 = rev(lists:sort(L2), TmpArgs),
    delete_files(Fs1),

    %% Input is a file. Output is a file
    ok = file_sorter:sort([], Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:sort([], Foo, [{unique,true} | Args]),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    L3 = [3,1,2,5,4,6],
    Fs2 = to_files([L3], Fmt, Config),
    ok = file_sorter:sort(Fs2, Foo, Args),
    true = rev(lists:sort(L3), Args) =:= from_files(Foo, Fmt),
    delete_files([Foo | Fs2]),
    L4 = [1,3,4,1,2,5,4,5,6],
    Fs3 = to_files([L4], Fmt, Config),
    ok = file_sorter:sort(Fs3, Foo, Args++[{unique,true},
					   {size,100000}]),
    true = rev(lists:usort(L4), Args) =:= from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:sort(Fs3, Foo, Args++[{unique,true}]),
    true = rev(lists:usort(L4), Args) =:= from_files(Foo, Fmt),
    delete_files([Foo | Fs3]),

    %% Input is a fun. Output is a file.
    ok = file_sorter:sort(input([], 2, Fmt), Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    L5 = [3,1,2,5,4,7],
    ok = file_sorter:sort(input(L5, 2, Fmt), Foo, Args),
    true = rev(lists:sort(L5), Args) =:= from_files(Foo, Fmt),
    delete_files(Foo),

    %% Removing duplicate keys.
    KFun = key_compare(2),
    L6 = [{5,e},{2,b},{3,c},{1,a},{4,d}] ++ [{2,c},{1,b},{4,a}],
    KUArgs = lists:keydelete(order, 1, Args) ++ 
	[{unique, true}, {order, KFun},{size,100000}],
    ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KUArgs),
    true = rev(lists:ukeysort(2, L6), KUArgs) =:= from_files(Foo, Fmt),
    KArgs = lists:keydelete(unique, 1, KUArgs),
    ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KArgs),
    true = rev(lists:keysort(2, L6), KArgs) =:= from_files(Foo, Fmt),

    %% Removing duplicate keys. Again.
    KUArgs2 = lists:keydelete(order, 1, Args) ++ 
	[{unique, true}, {order, KFun},{size,5}],
    ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KUArgs2),
    true = rev(lists:ukeysort(2, L6), KUArgs2) =:= from_files(Foo, Fmt),
    KArgs2 = lists:keydelete(unique, 1, KUArgs2),
    ok = file_sorter:sort(input(L6, 2, Fmt), Foo, KArgs2),
    true = rev(lists:keysort(2, L6), KArgs2) =:= from_files(Foo, Fmt),
    delete_files(Foo),

    ok.

keysort(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,50}, {no_files, 2} | XArgs]),
    TmpArgs = Args ++ [{tmpdir,?privdir(Config)}],
    Foo = outfile("foo", Config),

    %% Input is files. Output is a file.
    ok = file_sorter:keysort(2, [], Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:keysort(2, [], Foo, [{unique,true} | Args]),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    L0 = [{a,2},{a,1},{a,2},{a,2},{a,1},{a,2},{a,2},{a,3}],
    Fs0 = to_files([L0], Fmt, Config),
    S = rev(lists:ukeysort(1, L0), Args),
    ok =
	file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true},
						  {size,100000}]),
    S = from_files(Foo, Fmt),
    ok =
	file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true},
						  {size,5}]),
    S = from_files(Foo, Fmt),
    ok = file_sorter:keysort(1, Fs0, Foo, Args ++ [{unique,true}]),
    S = from_files(Foo, Fmt),
    delete_files([Foo | Fs0]),
    L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    All = [L11, L21, L31, L41],
    AllFlat = lists:append(All),
    Sorted = rev(lists:keysort(2, AllFlat), Args),
    Fs1 = to_files(All, Fmt, Config),
    ok = file_sorter:keysort(2, Fs1, Foo, Args),
    Sorted = from_files(Foo, Fmt),
    delete_files(Foo),

    %% Input is files. Output is a fun.
    [] = file_sorter:keysort(2, [], output([], Fmt), Args),
    KS1 = file_sorter:keysort(2, Fs1, output([], Fmt), TmpArgs),
    Sorted = KS1,
    delete_files(Fs1),

    %% Input is a fun. Output is a file.
    ok = file_sorter:keysort(2, input([], 2, Fmt), Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:keysort(2, input(AllFlat, 4, Fmt), Foo, Args),
    Sorted = from_files(Foo,  Fmt),
    delete_files(Foo),

    %% Input is a fun. Output is a fun.
    [] = file_sorter:keysort(2, input([], 2, Fmt), output([], Fmt),Args),
    KS2 =
	file_sorter:keysort(2, input(AllFlat, 4, Fmt), output([], Fmt), 
			    TmpArgs),
    Sorted = KS2,
    ok.

merge(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,5} | XArgs]),
    Foo = outfile("foo", Config),

    %% Input is a file. Output is a fun.
    [] = file_sorter:merge([], output([], Fmt), Args),
    L2 = [[1,3,5],[2,4,5]],
    Fs1 = to_files(L2, Fmt, Config),
    S2 = file_sorter:sort(Fs1, output([], Fmt), Args),
    S2 = rev(lists:sort(lists:append(L2)), Args),
    delete_files(Fs1),

    %% Input is a file. Output is a file
    ok = file_sorter:merge([], Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:merge([], Foo, [{unique,true} | Args]),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    L31 = [1,2,3],
    L32 = [2,3,4],
    L33 = [4,5,6],
    L3r = [L31, L32, L33],
    L3 = [rev(L31,Args), rev(L32,Args), rev(L33,Args)],
    Fs2 = to_files(L3, Fmt, Config),
    ok = file_sorter:merge(Fs2, Foo, Args),
    true = rev(lists:merge(L3r), Args) =:= from_files(Foo, Fmt),
    ok = file_sorter:merge(Fs2, Foo, Args++[{unique,true},
					    {size,100000}]),
    true = rev(lists:umerge(L3r), Args) =:= from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:merge(Fs2, Foo, Args++[{unique,true}]),
    true = rev(lists:umerge(L3r), Args) =:= from_files(Foo, Fmt),
    delete_files([Foo | Fs2]),

    ok.

keymerge(Fmt, XArgs, Config) ->
    Args = make_args(Fmt, [{size,50}, {no_files, 2} | XArgs]),
    Foo = outfile("foo", Config),

    %% Input is files. Output is a file.
    ok = file_sorter:keymerge(2, [], Foo, Args),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    ok = file_sorter:keymerge(2, [], Foo, [{unique,true} | Args]),
    [] = from_files(Foo, Fmt),
    delete_files(Foo),
    L0 = [rev([{a,1},{a,2}], Args), rev([{a,2},{a,1},{a,3}], Args)],
    Fs0 = to_files(L0, Fmt, Config),
    delete_files(Foo),
    ok = file_sorter:keymerge(1, Fs0, Foo, Args ++ [{unique,false}]),
    S2 = rev([{a,1},{a,2},{a,2},{a,1},{a,3}], Args),
    S2 = from_files(Foo, Fmt),
    delete_files([Foo | Fs0]),
    L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    All =
	[rev(L11, Args), rev(L21, Args), rev(L31, Args), rev(L41, Args)],
    Merged1 = lists:keymerge(2, L11, L21),
    Merged2 = lists:keymerge(2, L31, L41),
    Merged = rev(lists:keymerge(2, Merged1, Merged2), Args),
    Fs1 = to_files(All, Fmt, Config),
    ok = file_sorter:keymerge(2, Fs1, Foo, Args),
    Merged = from_files(Foo, Fmt),

    fun() -> 
	    UArgs = [{unique,true} | Args],
	    UMerged1 = lists:ukeymerge(2, L11, L21),
	    UMerged2 = lists:ukeymerge(2, L31, L41),
	    UMerged = rev(lists:ukeymerge(2, UMerged1, UMerged2), Args),
	    ok = file_sorter:keymerge(2, Fs1, Foo, UArgs),
	    UMerged = from_files(Foo, Fmt),
	    UArgs2 = make_args(Fmt, [{unique,true}, {size,50} | XArgs]),
	    ok = file_sorter:keymerge(2, Fs1, Foo, UArgs2),
	    UMerged = from_files(Foo, Fmt),
	    List = rev([{a,1,x4},{b,2,x4},{c,3,x4}], Args),
	    FsL = to_files([List], Fmt, Config),
	    ok = file_sorter:keymerge(2, FsL, Foo, UArgs),
	    List = from_files(Foo, Fmt),
	    List1 = [{a,1,x4},{b,2,x4},{c,3,x4}],
	    List2 = [{a,3,x4},{b,4,x4},{c,5,x4}],
	    FsLL = to_files([rev(List1, Args), rev(List2, Args)], Fmt, Config),
	    ok = file_sorter:keymerge(2, FsLL, Foo, UArgs),
	    List1_2 = rev(lists:ukeymerge(2, List1, List2), Args),
	    List1_2 = from_files(Foo, Fmt),
	    delete_files(Foo)
    end(),

    %% Input is files. Output is a fun.
    Fs3 = to_files(All, Fmt, Config),
    [] = file_sorter:keysort(2, [], output([], Fmt), Args),
    KS1 = file_sorter:keymerge(2, Fs3, output([], Fmt), Args),
    Merged = KS1,
    delete_files([Foo | Fs3]),

    L2 = [[{a,1}],[{a,2}],[{a,3}],[{a,4}],[{a,5}],[{a,6}],[{a,7}]],
    Fs2 = to_files(L2, Fmt, Config),
    M = file_sorter:keymerge(1, Fs2, output([], Fmt), Args),
    M = rev(lists:append(L2), Args),
    delete_files(Fs2),

    LL1 = [{d,4},{e,5},{f,6}],
    LL2 = [{a,1},{b,2},{c,3}],
    LL3 = [{j,10},{k,11},{l,12}],
    LL4 = [{g,7},{h,8},{i,9}],
    LL5 = [{p,16},{q,17},{r,18}],
    LL6 = [{m,13},{n,14},{o,15}],
    LLAll = [rev(LL1, Args),rev(LL2, Args),rev(LL3, Args),
	     rev(LL4, Args),rev(LL5, Args),rev(LL6, Args)],
    FsLL6 = to_files(LLAll, Fmt, Config),
    LL = rev(lists:sort(lists:append(LLAll)), Args),
    ok = file_sorter:keymerge(1, FsLL6, Foo, Args),
    LL = from_files(Foo, Fmt),
    ok = file_sorter:keymerge(1, FsLL6, Foo, [{unique,true} | Args]),
    LL = from_files(Foo, Fmt),
    delete_files([Foo | FsLL6]),

    ok.

funmerge(Fmt, XArgs, Config) ->
    KComp = key_compare(2),
    Args = make_args(Fmt, [{order,KComp},{size,5}, {no_files, 5} | XArgs]),
    UArgs = [{unique,true} | Args],
    Foo = outfile(foo, Config),

    EFs = to_files([[]], Fmt, Config),
    ok = file_sorter:merge(EFs, Foo, UArgs),
    [] = from_files(Foo, Fmt),
    delete_files([Foo | EFs]),

    L11 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    L21 = [{a,1,x3},{b,2,x3},{c,3,x3}],
    L31 = [{a,1,x2},{b,2,x2},{c,3,x2}],
    L41 = [{a,1,x1},{b,2,x1},{c,3,x1}],
    CAll = [L11, L21, L31, L41],
    CMerged1 = lists:merge(KComp, L11, L21),
    CMerged2 = lists:merge(KComp, L31, L41),
    CMerged = lists:merge(KComp, CMerged1, CMerged2),
    CFs1 = to_files(CAll, Fmt, Config),
    ok = file_sorter:merge(CFs1, Foo, Args),
    CMerged = from_files(Foo, Fmt),

    Args4 = make_args(Fmt, [{size,50} | XArgs]),
    ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | Args4]),
    CMerged = from_files(Foo, Fmt),

    UMerged1 = lists:umerge(KComp, L11, L21),
    UMerged2 = lists:umerge(KComp, L31, L41),
    UMerged = lists:umerge(KComp, UMerged1, UMerged2),
    ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | UArgs]),
    UMerged = from_files(Foo, Fmt),
    UArgs2 = 
        lists:keydelete(order, 1,
                        make_args(Fmt, [{unique,true}, {size,50} | XArgs])),
    ok = file_sorter:merge(CFs1, Foo, [{order,KComp} | UArgs2]),
    UMerged = from_files(Foo, Fmt),
    delete_files(Foo),

    List1 = [{a,1,x4},{b,2,x4},{c,3,x4}],
    List2 = [{a,3,x4},{b,4,x4},{c,5,x4}],
    List3 = [{a,5,x4},{b,6,x4},{c,7,x4}],
    FsLL = to_files([List1, List2, List3], Fmt, Config),
    ok = file_sorter:merge(FsLL, Foo, Args),
    List1_2 = lists:merge(KComp,lists:merge(KComp,List1,List2),List3),
    List1_2 = from_files(Foo, Fmt),
    ok = file_sorter:merge(FsLL, Foo, [{order,KComp} | UArgs]),
    UList1_2 =
        lists:umerge(KComp,lists:umerge(KComp, List1, List2),List3),
    UList1_2 = from_files(Foo, Fmt),
    delete_files([Foo | CFs1]),

    fun() ->
	    LL1 = [{d,4},{e,5},{f,6}],
	    LL2 = [{a,1},{b,2},{c,3}],
	    LL3 = [{j,10},{k,11},{l,12}],
	    LL4 = [{g,7},{h,8},{i,9}],
	    LL5 = [{p,16},{q,17},{r,18}],
	    LL6 = [{m,13},{n,14},{o,15}],
	    LLAll = [LL1,LL2,LL3,LL4,LL5,LL6],
	    FsLL6 = to_files(LLAll, Fmt, Config),
	    LL = lists:sort(lists:append(LLAll)),
	    ok = file_sorter:merge(FsLL6, Foo, Args),
	    LL = from_files(Foo, Fmt),
	    ok = file_sorter:merge(FsLL6, Foo, UArgs),
	    LL = from_files(Foo, Fmt),
	    delete_files([Foo | FsLL6])
    end(),

    fun() ->
	    RLL1 = [{b,2},{h,8},{n,14}],
	    RLL2 = [{a,1},{g,7},{m,13}],
	    RLL3 = [{d,4},{j,10},{p,16}],
	    RLL4 = [{c,3},{i,9},{o,15}],
	    RLL5 = [{f,6},{l,12},{r,18}],
	    RLL6 = [{e,5},{k,11},{q,17}],
	    RLLAll = [RLL1,RLL2,RLL3,RLL4,RLL5,RLL6],
	    RFsLL6 = to_files(RLLAll, Fmt, Config),
	    RLL = lists:sort(lists:append(RLLAll)),
	    ok = file_sorter:merge(RFsLL6, Foo, Args),
	    RLL = from_files(Foo, Fmt),
	    ok = file_sorter:merge(RFsLL6, Foo, UArgs),
	    RLL = from_files(Foo, Fmt),
	    delete_files([Foo | RFsLL6])
    end(),

    ok.

check(Fmt, Config) ->
    Args0 = make_args(Fmt, [{size,5}]),
    Args = Args0 ++ [{tmpdir,?privdir(Config)}],

    Fun = fun compare/2,

    L1 = [3,1,2,5,4],
    [F1_0] = Fs1 = to_files([L1], Fmt, Config),
    F1 = filename:absname(F1_0),
    {ok, [{F1,2,1}]} = file_sorter:check(Fs1, Args),
    {ok, [{F1,2,1}]} = file_sorter:check(Fs1, [{order,Fun} | Args]),
    {ok, [{F1,2,1}]} = file_sorter:check(Fs1, [{unique,true} | Args]),
    {ok, [{F1,2,1}]} =
	file_sorter:check(Fs1, [{order,Fun},{unique,true} | Args]),
    {ok, [{F1,3,2}]} =
	file_sorter:check(Fs1, [{order,descending} | Args]),
    {ok, [{F1,3,2}]} =
	file_sorter:check(Fs1, [{unique,true},{order,descending} | Args]),
    delete_files(Fs1),

    L2 = [[1,2,2,3,3,4,5,5],[5,5,4,3,3,2,2,1]],
    [F2_0,F3_0] = Fs2 = to_files(L2, Fmt, Config),
    F2 = filename:absname(F2_0),
    F3 = filename:absname(F3_0),
    {ok, [{F3,3,4}]} = file_sorter:check(Fs2, Args),
    {ok, [{F3,3,4}]} = file_sorter:check(Fs2, [{order,Fun} | Args]),
    {ok, [{F2,3,2},{F3,2,5}]} =
	file_sorter:check(Fs2, [{unique, true} | Args]),
    {ok, [{F2,3,2},{F3,2,5}]} =
	file_sorter:check(Fs2, [{order,Fun},{unique, true} | Args]),
    {ok, [{F2,2,2}]} =
	file_sorter:check(Fs2, [{order,descending} | Args]),
    {ok, [{F2,2,2},{F3,2,5}]} =
	file_sorter:check(Fs2, [{unique,true},{order,descending} | Args]),
    delete_files(Fs2),

    L3 = [1,2,3,4],
    Fs3 = to_files([L3], Fmt, Config),
    {ok, []} = file_sorter:check(Fs3, [{unique,true} | Args]),
    {ok, []} =
	file_sorter:check(Fs3, [{unique,true},{order,Fun} | Args]),
    delete_files(Fs3),

    %% big objects
    T1 = erlang:make_tuple(10000,foo),
    T2 = erlang:make_tuple(10000,bar),
    L4 = [T1,T2],
    [FF_0] = Fs4 = to_files([L4], Fmt, Config),
    FF = filename:absname(FF_0),
    {ok, [{FF,2,T2}]} = file_sorter:check(Fs4, [{unique,true} | Args]),
    delete_files(Fs4),

    CFun = key_compare(2),
    L10 = [[{1,a},{2,b},T10_1={1,b},{3,c}], [{1,b},T10_2={2,a}]],
    [F10_0,F11_0] = Fs10 = to_files(L10, Fmt, Config),
    F10_1 = filename:absname(F10_0),
    F11_1 = filename:absname(F11_0),
    {ok, [{F10_1,3,T10_1},{F11_1,2,T10_2}]} =
        file_sorter:check(Fs10, [{unique,true},{order,CFun} | Args]),
    delete_files(Fs10),

    ok.

keycheck(Fmt, Config) ->
    Args0 = make_args(Fmt, [{size,5}]),
    Args = Args0 ++ [{tmpdir,?privdir(Config)}],

    L1 = [[{a,1},{b,2}], [{c,2},{b,1},{a,3}]],
    [F1_0,F2_0] = Fs1 = to_files(L1, Fmt, Config),
    F1 = filename:absname(F1_0),
    F2 = filename:absname(F2_0),
    {ok, [{F2,2,{b,1}}]} = file_sorter:keycheck(1, Fs1, Args),
    {ok, [{F2,2,{b,1}}]} =
	file_sorter:keycheck(1, Fs1, [{unique,true} | Args]),
    {ok, [{F1,2,{b,2}}]} =
	file_sorter:keycheck(1, Fs1, [{order,descending},{unique,true} | Args]),
    delete_files(Fs1),

    L2 = [[{a,1},{a,2},{a,2},{b,2}], [{c,2},{b,1},{b,2},{b,2},{a,3}]],
    [F3_0,F4_0] = Fs2 = to_files(L2, Fmt, Config),
    F3 = filename:absname(F3_0),
    F4 = filename:absname(F4_0),
    {ok, [{F4,2,{b,1}}]} = file_sorter:keycheck(1, Fs2, Args),
    {ok, [{F3,2,{a,2}},{F4,2,{b,1}}]} =
	file_sorter:keycheck(1, Fs2, [{unique,true} | Args]),
    {ok, [{F3,4,{b,2}}]} =
	file_sorter:keycheck(1, Fs2, [{order,descending} | Args]),
    {ok, [{F3,2,{a,2}},{F4,3,{b,2}}]} =
	file_sorter:keycheck(1, Fs2, 
			     [{order,descending},{unique,true} | Args]),
    delete_files(Fs2),

    ok.

rev(L, Args) ->
    case lists:member({order, descending}, Args) of
	true ->
	    lists:reverse(L);
	false ->
	    L
    end.

make_args({HL, Fmt}, Args) ->
    make_args(Fmt, [{header, HL} | Args]);
make_args(Fmt, Args) ->
    [{format, Fmt} | Args].

compare(X, Y) ->
    X =< Y.

key_compare(I) ->
    fun(X, Y) -> 
            element(I, bin_to_term(X)) =< element(I, bin_to_term(Y))
    end.

bin_to_term(B) when is_binary(B) -> binary_to_term(B);
bin_to_term(T) -> T.

-define(CHUNKSIZE, 8096).

pps() ->
    erlang:ports().

input(L, N, term) ->
    input(L, N);
input(L, N, {_HL, Format}) when Format =:= binary_term; Format =:= binary ->
    binput(L, N);
input(L, N, Format) when Format =:= binary_term; Format =:= binary ->
    binput(L, N).

binput(L, N) ->
    Bs = lists:map(fun(T) -> term_to_binary(T) end, L),
    input(Bs, N).

input(L, N) ->
    fun(close) ->
	    ok;
       (read) ->
	    case L of
		[] -> end_of_input;
		_ ->
		    R = lists:sublist(L, N),
		    NL = lists:nthtail(length(R), L),
		    {R, input(NL, N)}
	    end
    end.

output(L, term) ->
    output(L);
output(L, {_HL, Format}) when Format =:= binary_term; Format =:= binary ->
    boutput(L);
output(L, Format) when Format =:= binary_term; Format =:= binary ->
    boutput(L).

output(A) ->
    fun(close) -> 
	    lists:append(lists:reverse(A));
       (L) when is_list(L) ->
	    output([L | A])
    end.

boutput(A) ->
    fun(close) -> 
	    Bs = lists:append(lists:reverse(A)),
	    lists:map(fun(B) -> binary_to_term(B) end, Bs);
       (L) when is_list(L) ->
	    boutput([L | A])
    end.

outfile(Name, Config) ->
    list_to_atom(filename:join(?privdir(Config), Name)).

%% [[term()]] -> [filename()]
to_files(Lists, term, Config) ->
    terms_to_files(Lists, Config);
to_files(Lists, Format, Config) when Format =:= binary_term; 
				     Format =:= binary ->
    bins_to_files(Lists, 4, Config);
to_files(Lists, {HL, Format}, Config) when Format =:= binary_term; 
					   Format =:= binary ->
    bins_to_files(Lists, HL, Config).

%% [[term()]] -> [filename()]
terms_to_files(Lists, Config) ->
    PrivDir = ?privdir(Config),
    terms_to_files(Lists, PrivDir, 1).

terms_to_files([L | Ls], PrivDir, N) ->
    F = lists:concat([?MODULE, '_', N]),
    File = filename:join(PrivDir, F),
    {ok, Fd} = file:open(File, [write]),
    write_terms(Fd, L),
    file:close(Fd),
    [list_to_atom(File) | terms_to_files(Ls, PrivDir, N+1)];
terms_to_files([], _PrivDir, _N) ->
    [].

write_terms(Fd, [T | Ts]) ->
    io:format(Fd, "~p.~n", [T]),
    write_terms(Fd, Ts);
write_terms(_Fd, []) ->
    ok.

%% [[term()]] -> [filename()]
bins_to_files(Lists, HL, Config) ->
    PrivDir = ?privdir(Config),
    bins_to_files(Lists, PrivDir, 1, HL).

bins_to_files([L | Fs], PrivDir, N, HL) ->
    F = lists:concat([?MODULE, '_', N]),
    File = filename:join(PrivDir, F),
    {ok, Fd} = file:open(File, [raw,binary,write]),
    write_bins(Fd, L, HL),
    file:close(Fd),
    [list_to_atom(File) | bins_to_files(Fs, PrivDir, N+1, HL)];
bins_to_files([], _PrivDir, _N, _HL) ->
    [].

write_bins(Fd, [T | Ts], HL) ->
    B = term_to_binary(T),
    Sz = byte_size(B),
    ok = file:write(Fd, [<<Sz:HL/unit:8>>, B]),
    write_bins(Fd, Ts, HL);
write_bins(_Fd, [], _HL) ->
    ok.

%% [filename()] -> [[term()]] or filename() -> [term()]
from_files(Files, term) ->
    terms_from_files(Files);
from_files(Files, Format) when Format =:= binary_term; Format =:= binary ->
    bins_from_files(Files, 4);
from_files(Files, {HL, Format}) when Format =:= binary_term; 
                                     Format =:= binary ->
    bins_from_files(Files, HL).

%% [filename()] -> [[term()]] or filename() -> [term()]
terms_from_files(File) when is_atom(File) ->
    [Terms] = terms_from_files([File]),
    Terms;
terms_from_files(Files) ->
    lists:map(fun(F) -> terms_from_file(F) end, Files).

terms_from_file(File) ->
    {ok, Fd} = file:open(File, [read,compressed]),
    terms_from_file(Fd, []).

terms_from_file(Fd, L) ->
    case io:read(Fd, '') of
	{ok, Term} ->
	    terms_from_file(Fd, [Term | L]);
	eof ->
	    file:close(Fd),
	    lists:reverse(L)
    end.

%% [filename()] -> [[term()]]
bins_from_files(File, HL) when is_atom(File) ->
    [Bins] = bins_from_files([File], HL),
    Bins;
bins_from_files(Files, HL) ->
    lists:map(fun(F) -> collect(F, HL) end, Files).

delete_files(File) when is_atom(File) ->
    file:delete(File);
delete_files(Files) ->
    lists:foreach(fun(F) -> file:delete(F) end, Files).

%%%
%%% Collects binaries converted to terms in a list. Not very efficient.
%%%
collect(F, HL) ->
    {ok, Fd} = file:open(F, [read, binary, raw, compressed]),
    R = (catch c(Fd, <<>>, 0, ?CHUNKSIZE, HL, [])),
    file:close(Fd),
    R.

c(Fd, Bin0, Size0, NoBytes, HL, L) ->
    case file:read(Fd, NoBytes) of
	{ok, Bin} ->
	    Size = Size0 + byte_size(Bin),
	    NBin = list_to_binary([Bin0, Bin]),
	    c1(Fd, NBin, Size, HL, L);
	eof when Size0 =:= 0 ->
	    lists:reverse(L);
        eof ->
	    ct:fail({error, premature_eof});
	Error ->
	    ct:fail(Error)
    end.

c1(Fd, B, BinSize, HL, L) -> 
    case B of 
	<<Size:HL/unit:8, Bin/binary>> ->
	    if 
		Size > BinSize - HL, Size > ?CHUNKSIZE ->
		    c(Fd, B, BinSize, Size + HL, HL, L);
		Size > BinSize - HL ->
		    c(Fd, B, BinSize, ?CHUNKSIZE, HL, L);
		true ->
		    <<BinTerm:Size/binary, R/binary>> = Bin,	    
		    E = case catch binary_to_term(BinTerm) of
                            {'EXIT', _} ->
				ct:fail({error, bad_object});
			    Term ->
				Term
			end,
		    NBinSize = BinSize - HL - Size,
		    c1(Fd, R, NBinSize, HL, [E | L])
	    end;
	_ ->
	    c(Fd, B, BinSize, ?CHUNKSIZE, HL, L)
    end.
