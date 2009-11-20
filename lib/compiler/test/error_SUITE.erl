%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

-include("test_server.hrl").

-export([all/1,
	 head_mismatch_line/1,r11b_binaries/1]).

all(suite) ->
    test_lib:recompile(?MODULE),
    [head_mismatch_line,r11b_binaries].

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

r11b_binaries(Config) when is_list(Config) ->
    Ts = [{r11b_binaries,
	   <<"
             t1(Bin) ->
               case Bin of
	         _ when size(Bin) > 20 -> erlang:error(too_long);
                 <<_,T/binary>> -> t1(T);
	         <<>> -> ok
             end.

             t2(<<_,T/bytes>>) ->
               split_binary(T, 4).

             t3(X) ->
               <<42,X/binary>>.

             t4(X) ->
               <<N:32>> = X,
               N.
           ">>,
           [r11],
	   {error,
	    [{5,v3_core,no_binaries},
	     {6,v3_core,no_binaries},
	     {9,v3_core,no_binaries},
	     {13,v3_core,no_binaries},
	     {16,v3_core,no_binaries}],
	    []} }],
    ?line [] = run(Config, Ts),
    ok.


run(Config, Tests) ->
    F = fun({N,P,Ws,E}, BadL) ->
                case catch run_test(Config, P, Ws) of
                    E -> 
                        BadL;
                    Bad -> 
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Bad]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).


%% Compiles a test module and returns the list of errors and warnings.

run_test(Conf, Test0, Warnings) ->
    Filename = 'errors_test.erl',
    ?line DataDir = ?config(priv_dir, Conf),
    ?line Test = ["-module(errors_test). ", Test0],
    ?line File = filename:join(DataDir, Filename),
    ?line Opts = [binary,export_all,return|Warnings],
    ?line ok = file:write_file(File, Test),

    %% Compile once just to print all errors and warnings.
    ?line compile:file(File, [binary,export_all,report|Warnings]),

    %% Test result of compilation.
    ?line Res = case compile:file(File, Opts) of
		    {error,[{_File,Es}],Ws} ->
			{error,Es,Ws}
		end,
    file:delete(File),
    Res.

fail() ->
    io:format("failed~n"),
    ?t:fail().
