%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_digit_map_test).

-compile(export_all).

-include("megaco_test_lib.hrl").


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, tickets}].

groups() -> 
    [{tickets, [],
      [{group, otp_5750}, {group, otp_5799},
       {group, otp_5826}, {group, otp_7449}]},
     {otp_5750, [], [otp_5750_01, otp_5750_02]},
     {otp_5799, [], [otp_5799_01]},
     {otp_5826, [], [otp_5826_01, otp_5826_02, otp_5826_03]},
     {otp_7449, [], [otp_7449_1, otp_7449_2]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5750_01(suite) ->
    [];
otp_5750_01(doc) ->
    [];
otp_5750_01(Config) when is_list(Config) ->
    DM = "1 | 123",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, "1"))
	  end,
	  fun({ok, {full, "1"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  (catch tde(DM, "123"))
	  end,
	  fun({ok, {unambiguous, "123"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {3,
	  fun() ->
		  (catch tde(DM, "124"))
	  end,
	  fun({error, _}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5750_02(suite) ->
    [];
otp_5750_02(doc) ->
    [];
otp_5750_02(Config) when is_list(Config) ->
    DM = "xxx | xxL3 | xxS4",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch otp_5750_02_exec(500, DM, "113"))
	  end,
	  fun({ok, {unambiguous, "113"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  (catch otp_5750_02_exec(500, DM, "114"))
	  end,
	  fun({ok, {unambiguous, "114"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {3,
	  fun() ->
		  (catch otp_5750_02_exec(5000, DM, "11ssss3"))
	  end,
	  fun({ok, {unambiguous, "113"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {4,
	  fun() ->
		  (catch otp_5750_02_exec(5000, DM, "11ssss4"))
	  end,
	  fun({ok, {unambiguous, "114"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}

	 ],
	  
    dm_tests(Tests),

    ok.

otp_5750_02_exec(To, DM, Evs) ->
    Pid = self(),
    Tester = 
	spawn(fun() ->
		      Res = tde(DM, Evs),
		      Pid ! {result, self(), Res}
	      end),
    receive
	{result, Tester, Res} ->
	    Res
    after To ->
	    {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5799_01(suite) ->
    [];
otp_5799_01(doc) ->
    [];
otp_5799_01(Config) when is_list(Config) ->
    DM = "234 | 23456",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, "2349"))
	  end,
	  fun({ok, {full, "234", $9}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_01(suite) ->
    [];
otp_5826_01(doc) ->
    [];
otp_5826_01(Config) when is_list(Config) ->
    DM = "123Z56",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long, $5},$6]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {2,
	  fun() ->
		  (catch tde(DM, [$1,$2,{long, $3},{long,$5},$6]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {3,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long,$5},{long,$6}]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {4,
	  fun() ->
		  (catch tde(DM, [$1,$2,{long, $3},{long,$5},{long,$6}]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_02(suite) ->
    [];
otp_5826_02(doc) ->
    [];
otp_5826_02(Config) when is_list(Config) ->
    DM = "12356",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long, $5},$6]))
	  end,
	  fun({ok, {unambiguous, "12356"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_03(suite) ->
    [];
otp_5826_03(doc) ->
    [];
otp_5826_03(Config) when is_list(Config) ->
    DM = "12346 | 12Z346 | 12Z34Z7 | 1234Z8",

    %% First case
    Tests = 
	[
 	 {1,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,$6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {2,
 	  fun() ->
 		  (catch tde(DM, [$1, {long, $2}, {long, $3},$4, $6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {3,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},{long, $4},$6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {4,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,{long, $7}]))
 	  end,
 	  fun({ok, {unambiguous, "12Z34Z7"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {5,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,{long, $8}]))
 	  end,
 	  fun({error, 
	       {unexpected_event, {long, $8}, _Collected, _Expected}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {6,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,$3,$4,{long, $8}]))
 	  end,
 	  fun({ok, {unambiguous, "1234Z8"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7449_1(suite) ->
    [];
otp_7449_1(doc) ->
    [];
otp_7449_1(Config) when is_list(Config) ->
    DM = "([0-9ef])",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$0]))
	  end,
	  fun({ok, {unambiguous, "0"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {2,
	  fun() ->
		  (catch tde(DM, [$1]))
	  end,
	  fun({ok, {unambiguous, "1"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {3,
	  fun() ->
		  (catch tde(DM, [$2]))
	  end,
	  fun({ok, {unambiguous, "2"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {4,
	  fun() ->
		  (catch tde(DM, [$3]))
	  end,
	  fun({ok, {unambiguous, "3"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {5,
	  fun() ->
		  (catch tde(DM, [$4]))
	  end,
	  fun({ok, {unambiguous, "4"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {6,
	  fun() ->
		  (catch tde(DM, [$5]))
	  end,
	  fun({ok, {unambiguous, "5"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {7,
	  fun() ->
		  (catch tde(DM, [$6]))
	  end,
	  fun({ok, {unambiguous, "6"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {8,
	  fun() ->
		  (catch tde(DM, [$7]))
	  end,
	  fun({ok, {unambiguous, "7"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {9,
	  fun() ->
		  (catch tde(DM, [$8]))
	  end,
	  fun({ok, {unambiguous, "8"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {10,
	  fun() ->
		  (catch tde(DM, [$9]))
	  end,
	  fun({ok, {unambiguous, "9"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {11,
	  fun() ->
		  (catch tde(DM, [$a]))
	  end,
	  fun({error, {unexpected_event, $a, _Collected, _Expected}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {12,
	  fun() ->
		  (catch tde(DM, [$e]))
	  end,
	  fun({ok, {unambiguous, "e"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {13,
	  fun() ->
		  (catch tde(DM, [$f]))
	  end,
	  fun({ok, {unambiguous, "f"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {14,
	  fun() ->
		  (catch tde(DM, [$a]))
	  end,
	  fun({error, {unexpected_event, $a, 
		       [] = _Collected, 
		       [{letter, [{range, $0, $9},  
				  {single, $e}, 
				  {single, $f}]}] = _Expected}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_7449_2(suite) ->
    [];
otp_7449_2(doc) ->
    [];
otp_7449_2(Config) when is_list(Config) ->
    DM = "([0-9]ef)",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$0,$e,$f]))
	  end,
	  fun({ok, {unambiguous, "0ef"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {2,
	  fun() ->
		  (catch tde(DM, [$1,$e,$f]))
	  end,
	  fun({ok, {unambiguous, "1ef"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {3,
	  fun() ->
		  (catch tde(DM, [$2]))
	  end,
	  fun({error, {unexpected_event, 
		       inter_event_timeout, [$2] = _Collected, 
		       [{single, $e}] = _Expecting}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {4,
	  fun() ->
		  (catch tde(DM, [$3,$f,$f]))
	  end,
	  fun({error, {unexpected_event, $f, [$3] = _Collected, 
		       [{single, $e}] = _Expected}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}, 
	 {5,
	  fun() ->
		  (catch tde(DM, [$a,$e,$f]))
	  end,
	  fun({error, {unexpected_event, $a, 
		       [] = _Collected, 
		       [{letter, [{range, $0, $9}]}] = _Expected}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dm_tests([]) ->
    ok;
dm_tests([{No, Exec, Ver}|Tests]) 
  when is_integer(No) andalso is_function(Exec) andalso is_function(Ver) ->
    case dm_test(Exec, Ver) of
	ok ->
	    dm_tests(Tests);
	{error, Reason} ->
	    ?ERROR({No, Reason});
	Error ->
	    ?ERROR({No, Error})
    end.

dm_test(Exec, Verify) ->
    (catch Verify(Exec())).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tde(DM, Evs) -> megaco:test_digit_event(DM, Evs).

