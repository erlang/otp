%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
	 head_mismatch_line/1,warnings_as_errors/1, bif_clashes/1,
	 transforms/1,maps_warnings/1,bad_utf8/1]).

%% Used by transforms/1 test case.
-export([parse_transform/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [head_mismatch_line,warnings_as_errors,bif_clashes,
       transforms,maps_warnings,bad_utf8]}].

init_per_suite(Config) ->
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
	    [{4, erl_lint,{call_to_redefined_old_bif,{length,1}}}], []} }],
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
	     [{3, erl_lint,{redefine_old_bif_import,{length,1}}}], []} }],
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
	    [{4, erl_lint,{call_to_redefined_bif,{binary_part,3}}}]} }],
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
	     [{3, erl_lint,{redefine_bif_import,{binary_part,3}}}]} }],
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
           [{7,erl_lint,{define_import,{length,1}}}],
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
           [{4,erl_lint,{illegal_guard_local_call,{length,1}}}],
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
           [{5,erl_lint,{illegal_guard_local_call,{length,1}}}],
           []} }],
    [] = run2(Config, Ts4),

    ok.




%% Tests that a head mismatch is reported on the correct line (OTP-2125).
head_mismatch_line(Config) when is_list(Config) ->
    [E|_] = get_compilation_errors(Config, "head_mismatch_line"),
    {26, Mod, Reason} = E,
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
	     [{3,erl_lint,{unused_var,'A'}}]} }],
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
	       [{3,erl_lint,{unused_var,'A'}}]} }],

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
    {error,[{none,compile,{parse_transform,?MODULE,{too_bad,_}}}],[]} =
	run_test(Ts2, test_filename(Config), [], dont_write_beam),
    Ts3 = <<"
              -compile({parse_transform,",?MODULE_STRING,"}).
             ">>,
    {error,[{none,compile,{parse_transform,?MODULE,{undef,_}}}],[]} =
        run_test(Ts3, test_filename(Config), [call_undef], dont_write_beam),
    ok.

parse_transform(_, Opts) ->
    case lists:member(call_undef, Opts) of
        false -> error(too_bad);
        true -> camembert:délicieux()
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
	    {error,[{3,erl_lint,{unbound_var,'K'}},
		    {6,erl_lint,illegal_map_key}],[]}}
    ],
    [] = run2(Config, Ts1),
    ok.

bad_utf8(Config) ->
    Ts = [{bad_utf8,
	   %% If coding is specified explicitly as utf-8, there should be
	   %% a compilation error; we must not fallback to parsing the
	   %% file in latin-1 mode.
	   <<"%% coding: utf-8
              %% Bj",246,"rn
              t() -> \"",246,"\".
             ">>,
	   [],
	   {error,[{2,epp,cannot_parse},
		   {2,file_io_server,invalid_unicode}],
	    []}
	  }],
    [] = run2(Config, Ts),
    ok.


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
    Test = ["-module(",ModName,"). ",Test0],
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
		  {warning,Ws};
	      {ok,Mod,_,[]} ->
		  [];
	      {ok,Mod,[{_File,Ws}]} ->
		  {warning,Ws};
	      {ok,Mod,[]} ->
		  [];
	      {error,[{XFile,Es}],Ws} = _ZZ when is_list(XFile) ->
		  {error,Es,Ws};
	      {error,[{XFile,Es1},{XFile,Es2}],Ws} = _ZZ
		when is_list(XFile) ->
		  {error,Es1++Es2,Ws};
	      {error,Es,[{_File,Ws}]} = _ZZ->
		  {error,Es,Ws}
	  end,
    file:delete(File),
    Res.

fail() ->
    ct:fail(failed).
