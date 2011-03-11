%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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

%%%------------------------------------------------------------------
%%% Test Server self test. 
%%%------------------------------------------------------------------
-module(test_server_line_SUITE).
-include_lib("test_server/include/test_server.hrl").

-export([all/0,suite/0]).
-export([init_per_suite/1,end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).
-export([parse_transform/1, lines/1]).

suite() -> 
    [{ct_hooks,[ts_install_cth]},
     {doc,["Test of parse transform for collection line numbers"]}].

all() -> [parse_transform,lines].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    ?line test_server_line:clear(),
    Dog = ?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    ?line test_server_line:clear(),
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

parse_transform(suite) -> [];
parse_transform(doc) -> [];
parse_transform(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    code:add_pathz(DataDir),

    ?line ok = parse_transform_test:excluded(),
    ?line [] = test_server_line:get_lines(),

    ?line test_server_line:clear(),
    ?line ok = parse_transform_test:func(),

    ?line [{parse_transform_test,func4,58},
	   {parse_transform_test,func,49},
	   {parse_transform_test,func3,56},
	   {parse_transform_test,func,39},
	   {parse_transform_test,func2,54},
	   {parse_transform_test,func,36},
	   {parse_transform_test,func1,52},
	   {parse_transform_test,func,35}] = test_server_line:get_lines(),
    
    code:del_path(DataDir),
    ok.

lines(suite) -> [];
lines(doc) -> ["Test parse transform for collection line numbers"];
lines(Config) when is_list(Config) ->
    ?line L0 = [{mod,func,1},{mod,func,2},{mod,func,3},
		{m,f,4},{m,f,5},{m,f,6},
		{mo,fu,7},{mo,fu,8},{mo,fu,9}],
    ?line LL = string:copies(L0, 1000),
    ?line T1 = erlang:now(),
    ?line lists:foreach(fun ({M,F,L}) -> 
				test_server_line:'$test_server_line'(M, F, L) 
			end, LL),
    ?line T2 = erlang:now(),
    ?line Long = test_server_line:get_lines(),
    ?line test_server_line:clear(),
    
    ?line T3 = erlang:now(),
    ?line lists:foreach(fun ({M,F,L}) -> 
				test_server_line:'$test_server_lineQ'(M, F, L) 
			end, LL),
    ?line T4 = erlang:now(),
    ?line LongQ = test_server_line:get_lines(),

    ?line io:format("'$test_server_line': ~f~n'$test_server_lineQ': ~f~n",
		    [timer:now_diff(T2, T1)/1000, timer:now_diff(T4, T3)/1000]),
    ?line io:format("'$test_server_line' result long:~p~n", [Long]),
    ?line io:format("'$test_server_lineQ' result long:~p~n", [LongQ]),

    if Long =:= LongQ ->
	    ?line ok;
       true ->
	    ?line ?t:fail("The two methods did not produce same result for"
			  " long lists of lines")
    end,
    
    ?line test_server_line:clear(),
    ?line lists:foreach(fun ({M,F,L}) -> 
				test_server_line:'$test_server_line'(M, F, L) 
			end, L0),
    ?line Short = test_server_line:get_lines(),
    ?line test_server_line:clear(),
    ?line lists:foreach(fun ({M,F,L}) -> 
				test_server_line:'$test_server_lineQ'(M, F, L) 
			end, L0),
    ?line ShortQ = test_server_line:get_lines(),

    ?line io:format("'$test_server_line' result short:~p~n", [Short]),
    ?line io:format("'$test_server_lineQ' result short:~p~n", [ShortQ]),
    
    if Short =:= ShortQ ->
	    ?line ok;
       true ->
	    ?line ?t:fail("The two methods did not produce same result for"
			  " shot lists of lines\n")
    end.
