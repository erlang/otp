%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
-module(error_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 head_mismatch_line/1, head_mismatch_same_function_name/1, warnings_as_errors/1,
	 bif_clashes/1, transforms/1,maps_warnings/1,bad_utf8/1,bad_decls/1]).

%% Used by transforms/1 test case.
-export([parse_transform/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [head_mismatch_line,head_mismatch_same_function_name,
       warnings_as_errors,bif_clashes,
       transforms,maps_warnings,bad_utf8,bad_decls]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


bif_clashes(Config) when is_list(Config) ->
    Ts = [{bif_clashes1,
           <<"
              -export([t/0]).
              t() ->
                 length([a,b,c]).

              length(X) ->
               erlang:length(X).
             ">>,
           [return_warnings],
	   {error,
	    [{{4,18}, erl_lint,{call_to_redefined_old_bif,{length,1}}}], []} }],
    [] = run(Config, Ts),
    Ts1 = [{bif_clashes2,
           <<"
              -export([t/0]).
              -import(x,[length/1]).
              t() ->
                 length([a,b,c]).
             ">>,
           [return_warnings],
	    {error,
	     [{{3,16}, erl_lint,{redefine_old_bif_import,{length,1}}}], []} }],
    [] = run(Config, Ts1),
    Ts00 = [{bif_clashes3,
           <<"
              -export([t/0]).
              -compile({no_auto_import,[length/1]}).
              t() ->
                 length([a,b,c]).

              length(X) ->
               erlang:length(X).
             ">>,
           [return_warnings],
	   []}],
    [] = run(Config, Ts00),
    Ts11 = [{bif_clashes4,
           <<"
              -export([t/0]).
              -compile({no_auto_import,[length/1]}).
              -import(x,[length/1]).
              t() ->
                 length([a,b,c]).
             ">>,
           [return_warnings],
	    []}],
    [] = run(Config, Ts11),
    Ts000 = [{bif_clashes5,
           <<"
              -export([t/0]).
              t() ->
                 binary_part(<<1,2,3,4>>,1,2).

              binary_part(X,Y,Z) ->
               erlang:binary_part(X,Y,Z).
             ">>,
           [return_warnings],
	   {warning,
	    [{{4,18}, erl_lint,{call_to_redefined_bif,{binary_part,3}}}]} }],
    [] = run(Config, Ts000),
    Ts111 = [{bif_clashes6,
           <<"
              -export([t/0]).
              -import(x,[binary_part/3]).
              t() ->
                  binary_part(<<1,2,3,4>>,1,2).
             ">>,
           [return_warnings],
	    {warning,
	     [{{3,16}, erl_lint,{redefine_bif_import,{binary_part,3}}}]} }],
    [] = run(Config, Ts111),
    Ts2 = [{bif_clashes7,
           <<"
              -export([t/0]).
              -compile({no_auto_import,[length/1]}).
              -import(x,[length/1]).
              t() ->
                 length([a,b,c]).
              length(X) ->
                 erlang:length(X).
             ">>,
           [],
          {error,
           [{{7,15},erl_lint,{define_import,{length,1}}}],
           []} }],
    [] = run2(Config, Ts2),
    Ts3 = [{bif_clashes8,
           <<"
              -export([t/1]).
              -compile({no_auto_import,[length/1]}).
              t(X) when length(X) > 3 ->
                 length([a,b,c]).
              length(X) ->
                 erlang:length(X).
             ">>,
           [],
          {error,
           [{{4,25},erl_lint,{illegal_guard_local_call,{length,1}}}],
           []} }],
    [] = run2(Config, Ts3),
    Ts4 = [{bif_clashes9,
           <<"
              -export([t/1]).
              -compile({no_auto_import,[length/1]}).
              -import(x,[length/1]).
              t(X) when length(X) > 3 ->
                 length([a,b,c]).
             ">>,
           [],
          {error,
           [{{5,25},erl_lint,{illegal_guard_local_call,{length,1}}}],
           []} }],
    [] = run2(Config, Ts4),

    ok.




%% Tests that a head mismatch is reported on the correct line (OTP-2125).
head_mismatch_line(Config) when is_list(Config) ->
    [E|_] = get_compilation_errors(Config, "head_mismatch_line"),
    {{26,1}, Mod, Reason} = E,
    ("head mismatch: previous function foo/1 is distinct from bar/1. "
     "Is the semicolon in foo/1 unwanted?") = lists:flatten(Reason),
    Mod:format_error(Reason),
    ok.

%% Tests that a head mismatch with the same function name reports a different error from above.
%% https://github.com/erlang/otp/pull/7383#issuecomment-1586564294
head_mismatch_same_function_name(Config) when is_list(Config) ->
    [E|_] = get_compilation_errors(Config, "head_mismatch_same_function_name"),
    {{25,1}, Mod, Reason} = E,
    ("head mismatch: function foo with arities 1 and 2 is regarded as "
     "two distinct functions. Is the number of arguments incorrect "
     "or is the semicolon in foo/1 unwanted?") = lists:flatten(Reason),
    Mod:format_error(Reason),
    ok.

%% Compiles a test file and returns the list of errors.

get_compilation_errors(Config, Filename) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, Filename),
    {error, [{_Name, E}|_], []} = compile:file(File, [return_errors]),
    E.

warnings_as_errors(Config) when is_list(Config) ->
    TestFile = test_filename(Config),
    BeamFile = filename:rootname(TestFile, ".erl") ++ ".beam",
    OutDir = proplists:get_value(priv_dir, Config),

    Ts1 = [{warnings_as_errors,
           <<"
               t() ->
                 A = unused,
                 ok.
             ">>,
	    [warnings_as_errors, export_all, {outdir, OutDir}],
	    {error,
	     [],
	     [{{3,18},erl_lint,{unused_var,'A'}}]} }],
    [] = run(Ts1, TestFile, write_beam),
    false = filelib:is_regular(BeamFile),

    Ts2 = [{warning_unused_var,
           <<"
               t() ->
                 A = unused,
                 ok.
             ">>,
	    [return_warnings, export_all, {outdir, OutDir}],
	    {warning,
	       [{{3,18},erl_lint,{unused_var,'A'}}]} }],

    [] = run(Ts2, TestFile, write_beam),
    true = filelib:is_regular(BeamFile),
    ok = file:delete(BeamFile),

    ok.

transforms(Config) ->
    Ts1 = [{undef_parse_transform,
	    <<"
              -compile({parse_transform,non_existing}).
             ">>,
	    [return],
	    {error,[{none,compile,{undef_parse_transform,non_existing}}],[]}}],
    [] = run(Config, Ts1),

    Ts2 = <<"
              -compile({parse_transform,",?MODULE_STRING,"}).
             ">>,

    {error,[{none,compile,{parse_transform,?MODULE,{error,too_bad,_}}}],[]} =
	run_test(Ts2, test_filename(Config), [{pt_error,error}], dont_write_beam),

    {error,[{none,compile,{parse_transform,?MODULE,{error,undef,_}}}],[]} =
        run_test(Ts2, test_filename(Config), [{pt_error,call_undef}], dont_write_beam),

    {error,[{none,compile,{parse_transform,?MODULE,{exit,exited,_}}}],[]} =
        run_test(Ts2, test_filename(Config), [{pt_error,exit}], dont_write_beam),

    {error,[{none,compile,{parse_transform,?MODULE,{throw,thrown,[_|_]}}}],[]} =
        run_test(Ts2, test_filename(Config), [{pt_error,throw}], dont_write_beam),

    ok.

parse_transform(_, Opts) ->
    {_,Error} = lists:keyfind(pt_error, 1, Opts),
    case Error of
        call_undef ->
            camembert:délicieux();
        throw ->
            throw(thrown);
        exit ->
            exit(exited);
        error ->
            error(too_bad)
    end.

maps_warnings(Config) when is_list(Config) ->
    Ts1 = [{map_ok_use_of_pattern,
	   <<"
              -export([t/1]).
              t(K) ->
                 #{K := 1 = V} = id(#{<<\"hi all\">> => 1}),
		 V.
              id(I) -> I.
             ">>,
	    [return],
	    []},
	{map_illegal_use_of_pattern,
	   <<"
              -export([t/0,t/2]).
	      t(K,#{ K := V }) -> V.
              t() ->
                 V = 32,
                 #{<<\"hi\",V,\"all\">> := 1} = id(#{<<\"hi all\">> => 1}).
              id(I) -> I.
             ">>,
	    [return],
	    {error,[{{3,15},erl_lint,{unbound_var,'K'}}],[]}}
    ],
    [] = run2(Config, Ts1),
    ok.

bad_utf8(Config) ->
    Ts = [{bad_explicit_utf8,
	   %% If coding is specified explicitly as utf-8, there should be
	   %% a compilation error for a latin-1 comment.
	   <<"%% coding: utf-8
              %% Bj",246,"rn
              t() -> \"",246,"\".
             ">>,
	   [],
	   {error,[{{2,15},epp,cannot_parse},
		   {{2,15},file_io_server,invalid_unicode}],
	    []}
	  },

          {bad_implicit_utf8,
           %% If there is no coding comment given, encoding defaults to utf-8
	   %% and there should be a compilation error for a latin-1 comment.
	   <<"
              %% Bj",246,"rn
              t() -> \"",246,"\".
             ">>,
	   [],
	   {error,[{{2,15},epp,cannot_parse},
		   {{2,15},file_io_server,invalid_unicode}],
	    []}
          }
         ],
    [] = run2(Config, Ts),
    ok.

bad_decls(Config) ->
    Ts = [{bad_decls_1,
	   <<"\n-module({l}).
             ">>,
	   [],
           {error,[{{2,9},erl_parse,"bad " ++ ["module"] ++ " declaration"}],
            []}
	  },
          {bad_decls_2,
	   <<"\n-module(l, m).
             ">>,
	   [],
           {error,[{{2,12},erl_parse,"bad variable list"}],[]}
	  },
          {bad_decls_3,
	   <<"\n-export([a/1], Y).
             ">>,
	   [],
           {error,[{{2,16},erl_parse,"bad " ++ ["export"] ++ " declaration"}],
            []}
	  },
          {bad_decls_4,
	   <<"\n-import([a/1], Y).
             ">>,
	   [],
           {error,[{{2,16},erl_parse,"bad " ++ ["import"] ++ " declaration"}],
            []}
	  },
          {bad_decls_5,
	   <<"\n-ugly({A,B}).
             ">>,
	   [],
           {error,[{{2,7},erl_parse,"bad attribute"}],[]}
	  },
          {bad_decls_6,
	   <<"\n-ugly(a, b).
             ">>,
	   [],
           {error,[{{2,10},erl_parse,"bad attribute"}],[]}
	  },
          {bad_decls_7,
	   <<"\n-export([A/1]).
             ">>,
	   [],
           {error,[{{2,10},erl_parse,"bad function name"}],[]}
	  },
          {bad_decls_8,
	   <<"\n-export([a/a]).
             ">>,
	   [],
           {error,[{{2,12},erl_parse,"bad function arity"}],[]}
	  },
          {bad_decls_9,
	   <<"\n-export([a/1, {3,4}]).
             ">>,
	   [],
           {error,[{{2,15},erl_parse,"bad Name/Arity"}],[]}
	  },
          {bad_decls_10,
	   <<"\n-record(A, {{bad,a}}).
             ">>,
	   [],
           {error,[{{2,9},erl_parse,"bad " ++ ["record"] ++ " declaration"}],
            []}
           },
          {bad_decls_11,
	   <<"\n-record(a, [a,b,c,d]).
             ">>,
	   [],
           {error,[{{2,12},erl_parse,"bad record declaration"}],[]}
           },
          {bad_decls_12,
	   <<"\n-record(a).
             ">>,
	   [],
           {error,[{{2,9},erl_parse,"bad " ++ ["record"] ++ " declaration"}],
            []}
           }
          ],
    [] = run2(Config, Ts),

    {error,{{1,4},erl_parse,"bad term"}} = parse_string("1, 2 + 4."),
    {error,{{1,1},erl_parse,"bad term"}} = parse_string("34 + begin 34 end."),
    ok.

parse_string(S) ->
    {ok,Ts,_} = erl_scan:string(S, {1, 1}),
    erl_parse:parse_term(Ts).


run(Config, Tests) ->
    File = test_filename(Config),
    run(Tests, File, dont_write_beam).

run(Tests, File, WriteBeam) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch run_test(P, File, Ws, WriteBeam) of
                    E -> 
                        BadL;
                    Bad -> 
                        io:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).

run2(Config, Tests) ->
    File = test_filename(Config),
    run2(Tests, File, dont_write_beam).

run2(Tests, File, WriteBeam) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch filter(run_test(P, File, Ws, WriteBeam)) of
                    E ->
                        BadL;
                    Bad ->
                        io:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).

filter({error,Es,_Ws}) ->
    {error,Es,[]};
filter(X) ->
    X.


%% Compiles a test module and returns the list of errors and warnings.

test_filename(Conf) ->
    Filename = ["errors_test_",test_lib:uniq(),".erl"],
    DataDir = proplists:get_value(priv_dir, Conf),
    filename:join(DataDir, Filename).

run_test(Test0, File, Warnings, WriteBeam) ->
    ModName = filename:rootname(filename:basename(File), ".erl"),
    Mod = list_to_atom(ModName),
    Test = iolist_to_binary(["-module(",ModName,"). ",Test0]),
    Opts = case WriteBeam of
	       dont_write_beam ->
		   [binary,return_errors|Warnings];
	       write_beam ->
		   [return_errors|Warnings]
	   end,
    ok = file:write_file(File, Test),

    %% Compile once just to print all errors and warnings.
    compile:file(File, [binary,report|Warnings]),

    %% Test result of compilation.
    io:format("~p\n", [Opts]),
    Res = case compile:file(File, Opts) of
	      {ok,Mod,_,[{_File,Ws}]} ->
                  print_diagnostics(Ws, Test),
		  {warning,Ws};
	      {ok,Mod,_,[]} ->
		  [];
	      {ok,Mod,[{_File,Ws}]} ->
		  {warning,Ws};
	      {ok,Mod,[]} ->
		  [];
	      {error,[{XFile,Es}],Ws} = _ZZ when is_list(XFile) ->
                  print_diagnostics(Es, Test),
		  {error,Es,Ws};
	      {error,[{XFile,Es1},{XFile,Es2}],Ws} = _ZZ
		when is_list(XFile) ->
                  Es = Es1 ++ Es2,
                  print_diagnostics(Es, Test),
		  {error,Es,Ws};
	      {error,Es,[{_File,Ws}]} = _ZZ->
                  print_diagnostics(Es ++ Ws, Test),
		  {error,Es,Ws}
	  end,
    file:delete(File),
    Res.

print_diagnostics(Warnings, Source) ->
    case binary:match(Source, <<"-file(">>) of
        nomatch ->
            Lines = binary:split(Source, <<"\n">>, [global]),
            Cs = [print_diagnostic(W, Lines) || W <- Warnings],
            io:put_chars(Cs);
        _ ->
            %% There are probably fake line numbers greater than
            %% the number of actual lines.
            ok
    end.

print_diagnostic({{LineNum,Column},Mod,Data}, Lines) ->
    Line0 = lists:nth(LineNum, Lines),
    <<Line1:(Column-1)/binary,_/binary>> = Line0,
    Spaces = re:replace(Line1, <<"[^\t]">>, <<" ">>, [global]),
    CaretLine = [Spaces,"^"],
    [io_lib:format("~p:~p: ~ts\n", [LineNum,Column,Mod:format_error(Data)]),
     Line0, "\n",
     CaretLine, "\n\n"];
print_diagnostic(_, _) ->
    [].

fail() ->
    ct:fail(failed).
