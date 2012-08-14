%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(error_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 head_mismatch_line/1,warnings_as_errors/1, bif_clashes/1,
	 column_number/1
   ]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [head_mismatch_line, warnings_as_errors, bif_clashes, column_number].

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
    ?line [] = run(Config, Ts),
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
    ?line [] = run(Config, Ts1),
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
    ?line [] = run(Config, Ts00),
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
    ?line [] = run(Config, Ts11),
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
    ?line [] = run(Config, Ts000),
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
    ?line [] = run(Config, Ts111),
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
    ?line [] = run2(Config, Ts2),
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
    ?line [] = run2(Config, Ts3),
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
    ?line [] = run2(Config, Ts4),

    ok.



%% Tests that messages are correctly reported with column numbers
%% if the column option is set.
column_number(Config) when is_list(Config) ->
    Ts1 = [{column_number_warning,
	   <<"\nt(X) -> ok.">>,
	   [return_warnings, export_all, column],
	   {warning, [{{2, 3}, erl_lint, {unused_var, 'X'}}]}}],
    ?line [] = run(Config, Ts1),
    ok.

%% Tests that a head mismatch is reported on the correct line (OTP-2125).
head_mismatch_line(Config) when is_list(Config) ->
    ?line [E|_] = get_compilation_errors(Config, "head_mismatch_line"),
    ?line {26, Mod, Reason} = E,
    ?line Mod:format_error(Reason),
    ok.

%% Compiles a test file and returns the list of errors.

get_compilation_errors(Config, Filename) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line File = filename:join(DataDir, Filename),
    ?line {error, [{_Name, E}|_], []} = compile:file(File, [return_errors]),
    E.

warnings_as_errors(Config) when is_list(Config) ->
    ?line TestFile = test_filename(Config),
    ?line BeamFile = filename:rootname(TestFile, ".erl") ++ ".beam",
    ?line OutDir = ?config(priv_dir, Config),

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
    ?line [] = run(Ts1, TestFile, write_beam),
    ?line false = filelib:is_regular(BeamFile),

    Ts2 = [{warning_unused_var,
           <<"
               t() ->
                 A = unused,
                 ok.
             ">>,
	    [return_warnings, export_all, {outdir, OutDir}],
	    {warning,
	       [{3,erl_lint,{unused_var,'A'}}]} }],

    ?line [] = run(Ts2, TestFile, write_beam),
    ?line true = filelib:is_regular(BeamFile),
    ?line ok = file:delete(BeamFile),

    ok.


run(Config, Tests) ->
    ?line File = test_filename(Config),
    run(Tests, File, dont_write_beam).

run(Tests, File, WriteBeam) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch run_test(P, File, Ws, WriteBeam) of
                    E -> 
                        BadL;
                    Bad -> 
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).

run2(Config, Tests) ->
    ?line File = test_filename(Config),
    run2(Tests, File, dont_write_beam).

run2(Tests, File, WriteBeam) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch filter(run_test(P, File, Ws, WriteBeam)) of
                    E ->
                        BadL;
                    Bad ->
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
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
    Filename = "errors_test.erl",
    DataDir = ?config(priv_dir, Conf),
    filename:join(DataDir, Filename).

run_test(Test0, File, Warnings, WriteBeam) ->
    ?line Test = ["-module(errors_test). ", Test0],
    ?line Opts = case WriteBeam of
		     dont_write_beam ->
			 [binary,return_errors|Warnings];
		     write_beam ->
			 [return_errors|Warnings]
		 end,
    ?line ok = file:write_file(File, Test),

    %% Compile once just to print all errors and warnings.
    ?line compile:file(File, [binary,report|Warnings]),

    %% Test result of compilation.
    ?line Res = case compile:file(File, Opts) of
		    {ok,errors_test,_,[{_File,Ws}]} ->
			%io:format("compile:file(~s,~p) ->~n~p~n",
			%	  [File,Opts,Ws]),
			{warning,Ws};
		    {ok,errors_test,_,[]} ->
			%io:format("compile:file(~s,~p) ->~n~p~n",
			%	  [File,Opts,Ws]),
			[];
		    {ok,errors_test,[{_File,Ws}]} ->
			{warning,Ws};
		    {ok,errors_test,[]} ->
			[];
		    {error,[{XFile,Es}],Ws} = _ZZ when is_list(XFile) ->
			%io:format("compile:file(~s,~p) ->~n~p~n",
			%	  [File,Opts,_ZZ]),
			{error,Es,Ws};
		    {error,Es,[{_File,Ws}]} = _ZZ->
			%io:format("compile:file(~s,~p) ->~n~p~n",
			%	  [File,Opts,_ZZ]),
			{error,Es,Ws}
		end,
    file:delete(File),
    Res.

fail() ->
    io:format("failed~n"),
    ?t:fail().
