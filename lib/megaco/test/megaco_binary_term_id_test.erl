%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose:
%%----------------------------------------------------------------------

-module(megaco_binary_term_id_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/include/megaco.hrl"). 
-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([t/0]).

%% Test suite exports
-export([all/0,groups/0,init_per_group/2,end_per_group/2,
	init_per_testcase/2, end_per_testcase/2]).  


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([te01/1,te02/1,te03/1,te04/1,te05/1,
	 te06/1,te07/1,te08/1,te09/1,te10/1,
	 te11/1,te12/1,te13/1,te14/1,te15/1,
	 te16/1,te17/1,te18/1,te19/1]).
-export([td01/1,td02/1,td03/1,td04/1,td05/1,td06/1]).


%% ---------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all() -> 
    [{group, encode_first}, {group, decode_first}].

groups() -> 
    [{encode_first, [], encode_first_cases()},
     {decode_first, [], decode_first_cases()}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).


%%======================================================================
%% External functions
%%======================================================================

t() ->
    display([do(Case) || Case <- cases()]),
    ok.


cases() -> encode_first_cases() ++ decode_first_cases().

encode_first_cases() -> 
[te01, te02, te03, te04, te05, te06, te07, te08, te09,
 te10, te11, te12, te13, te14, te15, te16, te17, te18,
 te19].
decode_first_cases() -> 
[td01, td02, td03, td04, td05, td06].

do(Case) ->
    case doc(Case) of
	{'EXIT',_} ->
	    {Case,error};
	Description ->
	    io:format("test case ~p~n",[Case]),
	    case suite(Case) of
		{'EXIT',Reason} ->
		    Res = check_result(Case,Description,ok,{error,Reason}),
		    {Case,Res};
		{Expected,Result} ->
		    Res = check_result(Case,Description,Expected,Result),
		    {Case,Res}
	    end
    end.

doc(Case)   -> (catch apply(?MODULE,Case,[doc])).
suite(Case) -> (catch apply(?MODULE,Case,[])).

display(R) -> 
    [display(Case,Result) || {Case,Result} <- R].

display(C,error) ->
    io:format("Test case ~p failed~n",[C]);
display(C,warning) ->
    io:format("Test case ~p conspicuous~n",[C]);
display(C,ok) ->
    io:format("Test case ~p succeeded~n",[C]).

check(D,ok,{ok,T1,T2,T3}) ->
    Result = case check_ok_result(T1,T3) of
		 ok -> 
		     ok;
		 {error,Reason} ->
		     io:format("  => inconsistent result"
			       "~n  Start and end record differ"
			       "~n  ~s"
			       "~n  ~s"
			       "~n  ~w"
			       "~n",
			       [D,Reason,T2]),
		     warning
	     end,
    Result;
check(D,error,{ok,T1,T2,T3}) ->
    io:format("  => failed"
	      "~n  ~s"
	      "~n  ~p"
	      "~n  ~p"
	      "~n  ~p"
	      "~n",
	      [D,T1,T2,T3]),
    error;
check(_D,error,{error,_Reason}) ->
    ok;
check(D,ok,{error,Reason}) ->
    io:format("  => failed"
	      "~n  ~s"
	      "~n  Failed for reason"
	      "~n  ~p"
	      "~n",
	      [D,Reason]),
    error.

check_result(_C,D,ok,{ok,T1,T2,T3}) ->
    Result = case check_ok_result(T1,T3) of
		 ok -> 
		     io:format("  => succeeded"
			       "~n  ~s"
			       "~n  ~p"
			       "~n  ~w"
			       "~n  ~p",
			       [D,T1,T2,T3]),
		     ok;
		 {error,Reason} ->
		     io:format("  => inconsistent result"
			       "~n  Start and end record differ"
			       "~n  ~s"
			       "~n  ~s"
			       "~n  ~w",
			       [D,Reason,T2]),
		     warning
	     end,
    io:format("~n~n--------------------~n",[]),
    Result;
check_result(_C,D,error,{ok,T1,T2,T3}) ->
    io:format("  => failed"
	      "~n  ~s"
	      "~n  ~p"
	      "~n  ~p"
	      "~n  ~p"
	      "~n~n--------------------~n",
	      [D,T1,T2,T3]),
    error;
check_result(_C,D,error,{error,Reason}) ->
    io:format("  => succeeded"
	      "~n  ~s"
	      "~n  Operation failed (expectedly) for reason"
	      "~n  ~p"
	      "~n~n--------------------~n",
	      [D,Reason]),
    ok;
check_result(_C,D,ok,{error,Reason}) ->
    io:format("  => failed"
	      "~n  ~s"
	      "~n  Failed for reason"
	      "~n  ~p"
	      "~n~n--------------------~n",
	      [D,Reason]),
    error.

check_ok_result(R,R) when is_record(R,megaco_term_id) ->
    ok; % Same record type and same record content
check_ok_result(S,E) when is_record(S,megaco_term_id) andalso 
                          is_record(E,megaco_term_id) ->
    Reason = check_megaco_term_id_record(S,E),
    {error,Reason}; % Same record type but different record content
check_ok_result(R,R) when is_record(R,'TerminationID') ->
    ok;
check_ok_result(S,E) when is_record(S,'TerminationID') andalso 
                          is_record(E,'TerminationID') ->
    Reason = check_TerminationID_record(S,E),
    {error,Reason}; % Same record type but different record content
check_ok_result(_S,_E) ->
    {error,"NOT THE SAME RECORD TYPES"}. % OOPS, Not even the same record type
    
check_megaco_term_id_record(#megaco_term_id{contains_wildcards = Cw1, 
					    id = Id1},
			    #megaco_term_id{contains_wildcards = Cw2, 
					    id = Id2}) ->
    Result = case check_megaco_term_id_cw(Cw1,Cw2) of
		 ok ->
		     check_megaco_term_id_id(Id1,Id2);
		 {error,R1} ->
		     R2 = check_megaco_term_id_id(Id1,Id2),
		     io_lib:format("~s~s",[R1,R2])
    end,
    lists:flatten(Result).
    
check_megaco_term_id_cw(Cw,Cw) ->
    ok;
check_megaco_term_id_cw(Cw1,Cw2) ->
    R = io_lib:format("~n   The 'contains_wildcard' property of the start"
		      "~n   megaco_term_id record "
		      "~n   has value ~w "
		      "~n   but the end record "
		      "~n   has value ~w",
		      [Cw1,Cw2]),
    {error,R}.

check_megaco_term_id_id(Id,Id) ->
    ok;
check_megaco_term_id_id(Id1,Id2) ->
    R = io_lib:format("~n   The 'id' property of the start"
		      "~n   megaco_term_id record "
		      "~n   has value ~w "
		      "~n   but the end record "
		      "~n   has value ~w",
		      [Id1,Id2]),
    {error,R}.


check_TerminationID_record(#'TerminationID'{wildcard = W1, id = Id1},
			   #'TerminationID'{wildcard = W2, id = Id2}) ->
    Result = case check_TerminationID_w(W1,W2) of
		 ok ->
		     check_TerminationID_id(Id1,Id2);
		 {error,R1} ->
		     R2 = check_TerminationID_id(Id1,Id2),
		     io_lib:format("~s~s",[R1,R2])
    end,
    lists:flatten(Result).
    
check_TerminationID_w(W,W) ->
    ok;
check_TerminationID_w(W1,W2) ->
    R = io_lib:format("~n   The 'wildcard' property of the start"
		      "~n   'TerminationID' record "
		      "~n   has value ~w "
		      "~n   but the end record "
		      "~n   has value ~w",
		      [W1,W2]),
    {error,R}.

check_TerminationID_id(Id,Id) ->
    ok;
check_TerminationID_id(Id1,Id2) ->
    R = io_lib:format("~n   The 'id' property of the start"
		      "~n   'TerminationID' record "
		      "~n   has value ~w "
		      "~n   but the end record "
		      "~n   has value ~w",
		      [Id1,Id2]),
    {error,R}.


%% --------------------------------------------------------
%% Start test cases
%% --------------------------------------------------------

%% basic_enc_dec01
te01(doc) ->
    "Basic encoding & then decoding test [1,1]\n  (asn -> binary -> asn)";

te01(suite) ->
    [];

te01(Config) when is_list(Config) ->
    {Exp,Res} = te01(),
    ok = check(te01(doc),Exp,Res).


te01() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid01()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec02
te02(doc) ->
    "Basic encoding & then decoding test [1,2]";

te02(suite) ->
    [];

te02(Config) when is_list(Config) ->
    {Exp,Res} = te02(),
    ok = check(te02(doc),Exp,Res).

te02() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid02()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec03
te03(doc) ->
    "Basic encoding & then decoding test [1,3]";

te03(suite) ->
    [];

te03(Config) when is_list(Config) ->
    {Exp,Res} = te03(),
    ok = check(te03(doc),Exp,Res).

te03() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid03()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec04
te04(doc) ->
    "Basic encoding & then decoding test [1,4]\n  (asn -> binary -> asn)";

te04(suite) ->
    [];

te04(Config) when is_list(Config) ->
    {Exp,Res} = te04(),
    ok = check(te04(doc),Exp,Res).

te04() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid04()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec05
te05(doc) ->
    "Basic encoding & then decoding test [1,5]\n  (asn -> binary -> asn)";

te05(suite) ->
    [];

te05(Config) when is_list(Config) ->
    {Exp,Res} = te05(),
    ok = check(te05(doc),Exp,Res).

te05() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid05()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% no_wildcard_spec_but_fond_some_enc_dec
te06(doc) ->
    "Specified NO wildcards, but found some just the same [1,6]\n  (asn -> binary -> asn)";

te06(suite) ->
    [];

te06(Config) when is_list(Config) ->
    {Exp,Res} = te06(),
    ok = check(te06(doc),Exp,Res).

te06() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid06()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% invalid_char_enc_dec
te07(doc) ->
    "Invalid character found (2) [1,7]\n  (asn -> binary -> asn)";

te07(suite) ->
    [];

te07(Config) when is_list(Config) ->
    {Exp,Res} = te07(),
    ok = check(te07(doc),Exp,Res).

te07() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid07()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% erroneous_first_level_length_enc_dec01
te08(doc) ->
    "Erroneous length of first level (a character after wildcard) [1,8]\n  (asn -> binary -> asn)";

te08(suite) ->
    [];

te08(Config) when is_list(Config) ->
    {Exp,Res} = te08(),
    ok = check(te08(doc),Exp,Res).

te08() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid08()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% erroneous_first_level_length_enc_dec02
te09(doc) ->
    "Erroneous length of first level (a character after last valid) [1,9]\n  (asn -> binary -> asn)";

te09(suite) ->
    [];

te09(Config) when is_list(Config) ->
    {Exp,Res} = te09(),
    ok = check(te09(doc),Exp,Res).

te09() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid09()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard01
te10(doc) ->
    "Basic encode & decode with one recursive wildcard [1,10]\n  (asn -> binary -> asn)\n   (NOTE THAT THIS SHOULD LATER BE A LEVEL WILDCARD)";

te10(suite) ->
    [];

te10(Config) when is_list(Config) ->
    {Exp,Res} = te10(),
    ok = check(te10(doc),Exp,Res).

te10() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid10()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard02
te11(doc) ->
    "Basic encode & decode with one recursive wildcard [1,11]\n  (asn -> binary -> asn)";

te11(suite) ->
    [];

te11(Config) when is_list(Config) ->
    {Exp,Res} = te11(),
    ok = check(te11(doc),Exp,Res).

te11() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid11()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard03
te12(doc) ->
    "Basic encode & decode with one recursive wildcard [1,12]\n  (asn -> binary -> asn)";

te12(suite) ->
    [];

te12(Config) when is_list(Config) ->
    {Exp,Res} = te12(),
    ok = check(te12(doc),Exp,Res).

te12() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid12()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard04
te13(doc) ->
    "Basic encode & decode with one recursive wildcard [1,13]\n  (asn -> binary -> asn)";

te13(suite) ->
    [];

te13(Config) when is_list(Config) ->
    {Exp,Res} = te13(),
    ok = check(te13(doc),Exp,Res).

te13() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid13()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard05
te14(doc) ->
    "Basic encode & decode with one recursive wildcard [2,13]\n  (asn -> binary -> asn)";

te14(suite) ->
    [];

te14(Config) when is_list(Config) ->
    {Exp,Res} = te14(),
    ok = check(te14(doc),Exp,Res).

te14() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config2(),mtid13()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_wildcard_1level
te15(doc) ->
    "Basic encode & decode with one wildcard in the first level\nand then one empty level [1,15]\n  (asn -> binary -> asn)";

te15(suite) ->
    [];

te15(Config) when is_list(Config) ->
    {Exp,Res} = te15(),
    ok = check(te15(doc),Exp,Res).

te15() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config1(),mtid15()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard06
te16(doc) ->
    "Basic encode & decode with one recursive wildcard [2,15]\n  (asn -> binary -> asn)";

te16(suite) ->
    [];

te16(Config) when is_list(Config) ->
    {Exp,Res} = te16(),
    ok = check(te16(doc),Exp,Res).

te16() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config2(),mtid15()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard07
te17(doc) ->
    "Basic encode & decode with one recursive wildcard [2,11]\n  (asn -> binary -> asn)";

te17(suite) ->
    [];

te17(Config) when is_list(Config) ->
    {Exp,Res} = te17(),
    ok = check(te17(doc),Exp,Res).

te17() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config2(),mtid11()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard08
te18(doc) ->
    "Basic encode & decode with one recursive wildcard [4,16]\n  (asn -> binary -> asn)";

te18(suite) ->
    [];

te18(Config) when is_list(Config) ->
    {Exp,Res} = te18(),
    ok = check(te18(doc),Exp,Res).

te18() ->
    put(encode_debug,dbg),
    put(decode_debug,dbg),
    Res = encode_decode(config4(),mtid16()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_enc_dec_with_one_rec_wildcard09
te19(doc) ->
    "Basic encode & decode with one recursive wildcard [4,17]\n  (asn -> binary -> asn)";

te19(suite) ->
    [];

te19(Config) when is_list(Config) ->
    {Exp,Res} = te19(),
    ok = check(te19(doc),Exp,Res).

te19() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = encode_decode(config4(),mtid17()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% root_term_dec_enc
td01(doc) ->
    "Root termination decoding & then encoding test [1,1]\n  (binary -> asn -> binary)";

td01(suite) ->
    [];

td01(Config) when is_list(Config) ->
    {Exp,Res} = td01(),
    ok = check(td01(doc),Exp,Res).

td01() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config1(),atid1()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% One byte to much (should be two bytes but is three)
%% basic_dec_enc01
td02(doc) ->
    "Basic decoding & then encoding test [1,2]\n  (binary -> asn -> binary)";

td02(suite) ->
    [];

td02(Config) when is_list(Config) ->
    {Exp,Res} = td02(),
    ok = check(td02(doc),Exp,Res).

td02() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config1(),atid2()),
    erase(encode_debug),
    erase(decode_debug),
    {error,Res}.


%% --------------------------------------------------------

%% basic_dec_enc02
td03(doc) ->
    "Basic decoding & then encoding test [2,2]\n  (binary -> asn -> binary)";

td03(suite) ->
    [];

td03(Config) when is_list(Config) ->
    {Exp,Res} = td03(),
    ok = check(td03(doc),Exp,Res).

td03() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config2(),atid2()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_dec_enc03
td04(doc) ->
    "Basic decoding & then encoding test [2,3]\n  (binary -> asn -> binary)";

td04(suite) ->
    [];

td04(Config) when is_list(Config) ->
    {Exp,Res} = td04(),
    ok = check(td04(doc),Exp,Res).

td04() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config2(),atid3()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_dec_enc04
td05(doc) ->
    "Basic decoding & then encoding test [2,4]\n  (binary -> asn -> binary)";

td05(suite) ->
    [];

td05(Config) when is_list(Config) ->
    {Exp,Res} = td05(),
    ok = check(td05(doc),Exp,Res).

td05() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config2(),atid4()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------

%% basic_dec_enc05
td06(doc) ->
    "Basic decoding & then encoding test [3,5]\n  (binary -> asn -> binary)";

td06(suite) ->
    [];

td06(Config) when is_list(Config) ->
    {Exp,Res} = td06(),
    ok = check(td06(doc),Exp,Res).

td06() ->
    %% put(encode_debug,dbg),
    %% put(decode_debug,dbg),
    Res = decode_encode(config3(),atid5()),
    erase(encode_debug),
    erase(decode_debug),
    {ok,Res}.


%% --------------------------------------------------------
%% --------------------------------------------------------

encode_decode(C,T) ->
    case encode(C,T) of
	{ok,T1} ->
	    case decode(C,T1) of
		{ok,T2} ->
		    {ok,T,T1,T2};
		{error,R2} ->
		    {error,{decode_error,T,T1,R2}};
		{exit,E2} ->
		    {error,{decode_exit,T,T1,E2}}
	    end;
	{error,R1} ->
	    {error,{encode_error,T,R1}};
	{exit,E1} ->
	    {error,{encode_exit,T,E1}}
    end.

decode_encode(C,T) ->
    case decode(C,T) of
	{ok,T1} ->
	    case encode(C,T1) of
		{ok,T2} ->
		    {ok,T,T1,T2};
		{error,R2} ->
		    {error,{encode_error,T,T1,R2}};
		{exit,E2} ->
		    {error,{encode_exit,T,T1,E2}}
	    end;
	{error,R1} ->
	    {error,{decode_error,T,R1}};
	{exit,E1} ->
	    {error,{decode_exit,T,E1}}
    end.

%% ------------------

config1() -> [3,8,5].
config2() -> [3,5,4,8,4].
config3() -> [3,5,4,16,4].
config4() -> [8,8,8].  % Default config

mtid01() -> 
    #megaco_term_id{contains_wildcards = false,
		    id = [[$1,$0,$1],
			  [$1,$1,$0,$1,$0,$0,$0,$1],
			  [$1,$1,$0,$1,$1]]}.
mtid02() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$$],[$1,$1,$0,$1,$$],[$1,$1,$*]]}.
mtid03() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$1],[$1,$0,$1,$$],[$1,$1,$*]]}.
mtid04() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$$],[$1,$1,$0,$1,$$],[$1,$1,$1,$0,$0]]}.
mtid05() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$1],[$1,$1,$0,$1,$$],[$1,$1,$1,$0,$0]]}.
mtid06() -> 
    #megaco_term_id{contains_wildcards = false,
		    id = [[$1,$0,$$],[$1,$1,$0,$1,$$],[$1,$1,$*]]}.
mtid07() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$*],[$1,2,$0,$1,$$],[$1,$1,$1,$0,$0]]}.
mtid08() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$*,$1],[$1,$1,$0,$1,$$],[$1,$1,$1,$0,$0]]}.
mtid09() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$0,$1],[$1,$1,$0,$1,$$],[$1,$1,$1,$0,$0]]}.
mtid10() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$1],[$1,$0,$1,$$],[$$]]}.
mtid11() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$1],[$1,$0,$1,$$]]}.
mtid12() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$1],[$1,$0,$1,$0,$1,$1,$$]]}.
mtid13() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$*]]}.
%% Empty last level
%% mtid14() -> 
%%    #megaco_term_id{contains_wildcards = true,
%%		    id = [[$1,$0,$*],[$1,$0,$0,$$],[]]}.

%% Empty second level and missing last level
mtid15() -> 
    #megaco_term_id{contains_wildcards = true,
		    id = [[$1,$0,$*],[]]}.
%% Megaco all wildcard termination id
mtid16() ->
	#megaco_term_id{contains_wildcards = true,
			id                 = [[?megaco_all]]}.
%% Megaco choose wildcard termination id
mtid17() ->
	#megaco_term_id{contains_wildcards = true,
			id                 = [[?megaco_choose]]}.

aroot() ->
    #'TerminationID'{wildcard = [],
		     id       = [16#FF, 16#FF, 16#FF, 16#FF,
				 16#FF, 16#FF, 16#FF, 16#FF]}.
atid1() -> aroot().
atid2() ->
    #'TerminationID'{wildcard = [],
		     id       = [2#00000001, 02#00011110, 2#0]}.
atid3() ->
    #'TerminationID'{wildcard = [[2#00010111], [2#00000111]],
		     id       = [2#0, 2#00011110, 2#0]}.
atid4() -> 
    #'TerminationID'{wildcard = [[2#01001111]],
		     id       = [2#0000001, 2#0, 2#0]}.
%% 
atid5() -> 
    #'TerminationID'{wildcard = [[2#01001111]],
		     id       = [2#00000001, 2#00110011, 
				 2#00000000, 2#00000000]}.

%%--

    
%% ------------------

encode(C,T) ->
    encode(get(encode_debug),C,T).

encode(L,C,T) -> 
    put(dsev,L),
    Res = encode1(C,T),
    erase(dsev),
    Res.

encode1(C,T) ->
    case (catch megaco_binary_term_id:encode(C,T)) of
	{'EXIT',Reason} ->
	    {exit,Reason};
	Else ->
	    Else
    end.

decode(C,T) ->
    decode(get(decode_debug),C,T).

decode(L,C,T) ->
    put(dsev,L),
    Res = decode1(C,T),
    erase(dsev),
    Res.

decode1(C,T) -> 
    case (catch megaco_binary_term_id:decode(C,T)) of
	{'EXIT',Reason} ->
	    {exit,Reason};
	Else ->
	    Else
    end.




