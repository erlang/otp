%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(mnesia_power_SUITE).
-compile([export_all]).
%%-define(line_trace,1).
-include("test_server.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) -> [run].

-define(iterations,3). %% nof power-off cycles to do before acceptance
-define(rows,8). %% nof database rows to use (not too big, please)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(sum_table_1,{row,a,b,c,s}).

run(suite) -> [];
run(Config) ->
    ?line mnesia:create_schema([node()]),
    ?line mnesia:start(),
    ?line mnesia:create_table([{name, sum_table_1}, {disc_copies,[node()]},
			       {attributes,record_info(fields,sum_table_1)}]),
    ?line run_test(Config,?iterations).

run(Config,N) ->
    ?line mnesia:start(),
    ?line check_consistency(sum_table_1),
    case N of
	0 -> ?line ok;
	N -> ?line run_test(Config,N)
    end.
    
run_test(Config,N) ->
    ?line Pid1a = start_manipulator(sum_table_1),
    ?line Pid1b = start_manipulator(sum_table_1),
    ?line Pid1c = start_manipulator(sum_table_1),
    ?line test_server:resume_point(?MODULE,run,[Config,N-1]),
    ?line test_server:format(1,"Manipulating data like crazy now, "
		       "power off any time..."),
    ?line test_server:sleep(infinity).

start_manipulator(Table) ->
    ?line spawn_link(?MODULE,manipulator_init,[Table]).

manipulator_init(Table) ->
    random:seed(4711,0,0),
    manipulator(0,Table).

manipulator(N,Table) ->
    ?line Fun = 
	fun() ->
		?line Row = random:uniform(?rows),
		?line A = random:uniform(100000),
		?line B = random:uniform(100000),
		?line C = random:uniform(100000),
		?line Sum = A+B+C,
		?line case mnesia:write(#sum_table_1
					{row=Row,a=A,b=B,c=C,s=Sum}) of
			  ok -> ok;
			  Other ->
			      ?line io:format("Trans failed: ~p\n",[Other])
		      end
	end,
    ?line mnesia:transaction(Fun),
    case mnesia:table_info(sum_table_1,size) of
	0 -> exit(still_empty);
	_ -> ok
    end,
    case N rem 2000 of
	0 -> io:format("~p did ~p operations",[self(),N]),
	     check_consistency(sum_table_1);
	_ -> ok
    end,
    ?line manipulator(N+1,Table).

check_consistency(Table) ->
    io:format("Checking consistency of table ~p\n",[Table]),
    All = mnesia:table_info(Table,wild_pattern),
    ?line Fun =
        fun() ->
		mnesia:match_object(All)
        end,
  ?line case mnesia:transaction(Fun) of
	    {atomic,Val} ->
		check_consistency_rows(Val,0);
	    Other ->
		io:format("Trans failed: ~p\n",[Other]),
		exit(failed),
		check_consistency(Table)
	  end.

check_consistency_rows([#sum_table_1{a=A,b=B,c=C,s=Sum}|Rows],N) ->
    ?line Sum=A+B+C,
    ?line check_consistency_rows(Rows,N+1);
check_consistency_rows([],N) ->
    io:format("All ~p rows were consistent\n",[N]),
    {ok,N};
check_consistency_rows(Thing,N) ->
    io:format("Mnesia transaction returned:\n~p\n",[Thing]),
    exit({bad_format,Thing}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				  
    
		
			      
