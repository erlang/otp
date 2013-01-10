%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
				 ($-,Acc) -> Acc;
				 (X, Acc) -> [X |Acc] end, 
			      [], atom_to_list(node()))).

match_float(Float, Match, Delta) ->
    (Float < Match + Delta) and (Float > Match - Delta).

odbc_check() ->
    case os:type() of
	{unix,darwin} ->
	    lists:flatten(
	      io_lib:format("Currently we have no working drivers for MAC",
			    []));
	_ ->
	    case erlang:system_info({wordsize, external}) of
		4 ->
		    ok;
		Other ->
		    case os:type() of
			{unix, linux} ->
			    ok;
			Platform ->
			    lists:flatten(
			      io_lib:format("Word on platform ~w size"
					    " ~w not supported", [Other,
								  Platform]))
		    end
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

strict(Ref, mysql) ->
    odbc:sql_query(Ref, "SET sql_mode='STRICT_ALL_TABLES,STRICT_TRANS_TABLES';");
strict(_,_) ->
    ok.

platform_options() ->
    [].

skip() ->
    case os:type() of
	{unix, linux} ->
	    Issue = linux_issue(),
	    is_sles9(Issue);
	{unix, sunos} ->
	    not supported_solaris();
	_ ->
	    false
    end.

supported_solaris() ->
    case os:version() of
	{_,10,_} ->
	    true;
	_ ->
	    false
    end.

linux_issue() ->
    {ok, Binary} = file:read_file("/etc/issue"),
    string:tokens(binary_to_list(Binary), " ").

is_sles11(IssueTokens) ->
    lists:member("11", IssueTokens).

is_sles10(IssueTokens) ->
    lists:member("10", IssueTokens).

is_sles9(IssueTokens) ->
    lists:member("9", IssueTokens).

is_ubuntu(IssueTokens) ->
    lists:member("Ubuntu", IssueTokens).
