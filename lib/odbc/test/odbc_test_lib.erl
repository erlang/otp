%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

%%

-module(odbc_test_lib).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("odbc_test.hrl").
-include("test_server.hrl").

unique_table_name() ->                              
    lists:reverse(lists:foldl(fun($@, Acc) -> [$t, $A |Acc] ;
				 (X, Acc) -> [X |Acc] end, 
			      [], atom_to_list(node()))).

match_float(Float, Match, Delta) ->
    (Float < Match + Delta) and (Float > Match - Delta).

odbc_check() ->
    case erlang:system_info(wordsize) of
	4 ->
	    case test_server:os_type() of
		{unix, sunos} ->
		    ok;
		{unix, linux} ->
		    ok;
		{win32, _} ->
		    ok;
		Other ->
		    lists:flatten(
		      io_lib:format("Platform not supported: ~w",
				    [Other]))
	    end;
	Other ->
	    case test_server:os_type() of
		{unix, linux} ->
		    ok;
		Platform ->
		    lists:flatten(
		      io_lib:format("Word on platform ~w size"
				    " ~w not supported", [Other,
							  Platform]))
	    end
    end.

check_row_count(Count, Count) ->
    test_server:format("Correct row count Count: ~p~n", [Count]),   
    true;
check_row_count(_, undefined) ->
    test_server:format("Undefined row count ~n", []),  
    true;
check_row_count(Expected, Count) ->
    test_server:format("Incorrect row count Expected ~p Got ~p~n",
		       [Expected, Count]),   
    false.

to_upper(List) ->
    lists:map(fun(Str) -> string:to_upper(Str) end, List).
