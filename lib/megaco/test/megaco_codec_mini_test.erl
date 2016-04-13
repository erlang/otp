%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% Purpose: Test mini encoding/decoding (codec) module of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_codec_mini_test).

%% ----

-include_lib("megaco/include/megaco.hrl").
%% -include_lib("megaco/include/megaco_message_v3.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([t/0, t/1]).

-export([all/0,groups/0,init_per_group/2,end_per_group/2, 

	 tickets/0, 
	 
	 otp7672_msg01/1,
 	 otp7672_msg02/1,
 
	 init_per_testcase/2, end_per_testcase/2]).  


%% ----

-define(SET_DBG(S,D), begin put(severity, S), put(dbg, D) end).
-define(RESET_DBG(),  begin erase(severity),  erase(dbg)  end).

expand(RootCase) ->
    expand([RootCase], []).

expand([], Acc) ->
    lists:flatten(lists:reverse(Acc));
expand([Case|Cases], Acc) ->
    case (catch apply(?MODULE,Case,[suite])) of
        [] ->
            expand(Cases, [Case|Acc]);
        C when is_list(C) ->
            expand(Cases, [expand(C, [])|Acc]);
        _ ->
            expand(Cases, [Case|Acc])
    end.


%% ----

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

init_per_testcase(Case, Config) ->
    C = 
	case lists:suffix("time_test", atom_to_list(Case)) of
	    true ->
		[{tc_timeout, timer:minutes(10)}|Config];
	    false ->
		put(verbosity,trc),
		Config
	end,
    megaco_test_lib:init_per_testcase(Case, C).

end_per_testcase(Case, Config) ->
    erase(verbosity),
    megaco_test_lib:end_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all() -> 
    [{group, tickets}].

groups() -> 
    [{tickets, [], [otp7672_msg01, otp7672_msg02]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% ----

tickets() ->
    Flag  = process_flag(trap_exit, true),    
    Cases = expand(tickets),
    Fun   = fun(Case) ->
		    C = init_per_testcase(Case, [{tc_timeout, 
						  timer:minutes(10)}]),
		    io:format("Eval ~w~n", [Case]),
		    Result = 
			case (catch apply(?MODULE, Case, [C])) of
			    {'EXIT', Reason} ->
 				io:format("~n~p exited:~n   ~p~n", 
 					  [Case, Reason]),
				{error, {Case, Reason}};
			    Res ->
				Res
			end,
		    end_per_testcase(Case, C),
		    Result
	    end,
    process_flag(trap_exit, Flag),
    lists:map(Fun, Cases).

		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
%% Ticket test cases:


%% --------------------------------------------------------------
%% 

otp7672_msg01(suite) ->
    [];
otp7672_msg01(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg01 -> entry", []),
    ok = otp7672( otp7672_msg01() ),
    %% ?RESET_DBG(), 
    ok.

otp7672_msg02(suite) ->
    [];
otp7672_msg02(Config) when is_list(Config) ->
    %% ?SET_DBG(trc, true),
    d("otp7672_msg02 -> entry", []),
    ok = otp7672( otp7672_msg02() ),
    %% ?RESET_DBG(), 
    ok.


otp7672_msg01() ->
    <<"!/1 <TEST> ">>.

otp7672_msg02() ->
    <<"!/1 <TE> ">>.

otp7672(Msg) ->
    case megaco_text_mini_decoder:decode_message([], Msg) of
	{ok, M} ->
	    t("mini decode successfull: ~n~p", [M]),
	    ok;
	Error ->
	    e("mini decode failed: ~n~p", [Error]),
	    {error, Error}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

%% l(F,A) ->
%%     p(printable(get(severity),log),log,F,A).

e(F,A) ->
    p(printable(get(severity),err),err,F,A).


printable(trc,_) ->
    true;
printable(dbg,trc) ->
    false;
printable(dbg,_) ->
    true;
printable(log,log) ->
    true;
printable(log,err) ->
    true;
printable(err,err) ->
    true;
printable(_,_) ->
    false.


p(true,L,F,A) ->
    io:format("~s:" ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "TRC";
image_of(dbg) ->
    "DBG";
image_of(log) ->
    "LOG";
image_of(err) ->
    "ERR";
image_of(L) ->
    io_lib:format("~p",[L]).

