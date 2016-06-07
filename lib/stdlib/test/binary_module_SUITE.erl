%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(binary_module_SUITE).

-export([all/0, suite/0,
	 interesting/1,scope_return/1,random_ref_comp/1,random_ref_sr_comp/1,
	 random_ref_fla_comp/1,parts/1, bin_to_list/1, list_to_bin/1,
	 copy/1, referenced/1,guard/1,encode_decode/1,badargs/1,longest_common_trap/1]).

-export([random_number/1, make_unaligned/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,10}}].

all() -> 
    [scope_return,interesting, random_ref_fla_comp, random_ref_sr_comp,
     random_ref_comp, parts, bin_to_list, list_to_bin, copy,
     referenced, guard, encode_decode, badargs,
     longest_common_trap].


-define(MASK_ERROR(EXPR),mask_error((catch (EXPR)))).


%% Test various badarg exceptions in the module.
badargs(Config) when is_list(Config) ->
    badarg = ?MASK_ERROR(binary:compile_pattern([<<1,2,3:3>>])),
    badarg = ?MASK_ERROR(binary:compile_pattern([<<1,2,3>>|<<1,2>>])),
    badarg = ?MASK_ERROR(binary:compile_pattern(<<1,2,3:3>>)),
    badarg = ?MASK_ERROR(binary:compile_pattern(<<>>)),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3:3>>,<<1>>)),
    badarg = ?MASK_ERROR(binary:matches(<<1,2,3:3>>,<<1>>)),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,
				      [{scope,{0,1},1}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,
				      [{scape,{0,1}}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,
				      [{scope,{0,1,1}}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,[{scope,0,1}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,[{scope,[0,1]}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,
				      [{scope,{0.1,1}}])),
    badarg = ?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,
				      [{scope,{1,1.1}}])),
    badarg =
	?MASK_ERROR(
	   binary:match(<<1,2,3>>,<<1>>,
			[{scope,{16#FF,
				 16#FFFFFFFFFFFFFFFF}}])),
    badarg =
	?MASK_ERROR(
	   binary:match(<<1,2,3>>,<<1>>,
			[{scope,{16#FFFFFFFFFFFFFFFF,
				 -16#7FFFFFFFFFFFFFFF-1}}])),
    badarg =
	?MASK_ERROR(
	   binary:match(<<1,2,3>>,<<1>>,
			[{scope,{16#FFFFFFFFFFFFFFFF,
				 16#7FFFFFFFFFFFFFFF}}])),
    badarg =
	?MASK_ERROR(
	   binary:part(<<1,2,3>>,{16#FF,
				  16#FFFFFFFFFFFFFFFF})),
    badarg =
	?MASK_ERROR(
	   binary:part(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
				  -16#7FFFFFFFFFFFFFFF-1})),
    badarg =
	?MASK_ERROR(
	   binary:part(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
				  16#7FFFFFFFFFFFFFFF})),
    badarg =
	?MASK_ERROR(
	   binary:part(make_unaligned(<<1,2,3>>),{1,1,1})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{1,1,1})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{16#FFFFFFFFFFFFFFFF,
						  -16#7FFFFFFFFFFFFFFF-1})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{16#FF,
						  16#FFFFFFFFFFFFFFFF})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{16#FFFFFFFFFFFFFFFF,
						  16#7FFFFFFFFFFFFFFF})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{16#FFFFFFFFFFFFFFFFFF,
						  -16#7FFF})),
    badarg =
	?MASK_ERROR(
	   binary_part(make_unaligned(<<1,2,3>>),{16#FF,
						  -16#7FFF})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{16#FF,
					 16#FFFFFFFFFFFFFFFF})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
					 -16#7FFFFFFFFFFFFFFF-1})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{16#FFFFFFFFFFFFFFFF,
					 16#7FFFFFFFFFFFFFFF})),
    [1,2,3] =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>)),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,[])),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{1,2,3})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{1.0,1})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3>>,{1,1.0})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3:3>>,{1,1})),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list(<<1,2,3:3>>)),
    badarg =
	?MASK_ERROR(
	   binary:bin_to_list([1,2,3])),

    nomatch =
	?MASK_ERROR(binary:match(<<1,2,3>>,<<1>>,[{scope,{0,0}}])),
    badarg =
	?MASK_ERROR(binary:match(<<1,2,3>>,{bm,<<>>},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:match(<<1,2,3>>,[],[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:match(<<1,2,3>>,{ac,<<>>},[{scope,{0,1}}])),
    {bm,BMMagic} = binary:compile_pattern([<<1,2,3>>]),
    {ac,ACMagic} = binary:compile_pattern([<<1,2,3>>,<<4,5>>]),
    badarg =
	?MASK_ERROR(binary:match(<<1,2,3>>,{bm,ACMagic},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:match(<<1,2,3>>,{ac,BMMagic},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(
	   binary:match(<<1,2,3>>,
			{bm,ets:match_spec_compile([{'_',[],['$_']}])},
			[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(
	   binary:match(<<1,2,3>>,
			{ac,ets:match_spec_compile([{'_',[],['$_']}])},
			[{scope,{0,1}}])),
    [] =
	?MASK_ERROR(binary:matches(<<1,2,3>>,<<1>>,[{scope,{0,0}}])),
    badarg =
	?MASK_ERROR(binary:matches(<<1,2,3>>,{bm,<<>>},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:matches(<<1,2,3>>,[],[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:matches(<<1,2,3>>,{ac,<<>>},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:matches(<<1,2,3>>,{bm,ACMagic},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(binary:matches(<<1,2,3>>,{ac,BMMagic},[{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(
	   binary:matches(<<1,2,3>>,
			  {bm,ets:match_spec_compile([{'_',[],['$_']}])},
			  [{scope,{0,1}}])),
    badarg =
	?MASK_ERROR(
	   binary:matches(<<1,2,3>>,
			  {ac,ets:match_spec_compile([{'_',[],['$_']}])},
			  [{scope,{0,1}}])),
    %% OTP-11350
    badarg = ?MASK_ERROR(
		binary:matches(<<"foo">>, 
			       [<<>>, <<"f">>])),
    badarg =
	?MASK_ERROR(binary:longest_common_prefix(
		      [<<0:10000,1,2,4,1:3>>,
		       <<0:10000,1,2,3>>])),
    badarg =
	?MASK_ERROR(binary:longest_common_suffix(
		      [<<0:10000,1,2,4,1:3>>,
		       <<0:10000,1,2,3>>])),
    badarg =
	?MASK_ERROR(binary:encode_unsigned(-1)),
    badarg =
	?MASK_ERROR(
	   binary:encode_unsigned(-16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)),
    badarg =
	?MASK_ERROR(
	   binary:first(<<1,2,4,1:3>>)),
    badarg =
	?MASK_ERROR(
	   binary:first([1,2,4])),
    badarg =
	?MASK_ERROR(
	   binary:last(<<1,2,4,1:3>>)),
    badarg =
	?MASK_ERROR(
	   binary:last([1,2,4])),
    badarg =
	?MASK_ERROR(
	   binary:at(<<1,2,4,1:3>>,2)),
    badarg =
	?MASK_ERROR(
	   binary:at(<<>>,2)),
    badarg =
	?MASK_ERROR(
	   binary:at([1,2,4],2)),
    ok.

%% Whitebox test to force special trap conditions in
%% longest_common_{prefix,suffix}.
longest_common_trap(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("oldlimit: ~p~n",
	      [erts_debug:set_internal_state(binary_loop_limit,10)]),
    erlang:bump_reductions(10000000),
    _ = binary:longest_common_prefix(
	  [<<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0:10000,1,3,3>>,
	   <<0:10000,1,2,4>>]),
    _ = binary:longest_common_prefix(
	  [<<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,3>>,
	   <<0:10000,1,3,3>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,4>>,
	   <<0:10000,1,2,3>>,
	   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	   <<0:10000,1,2,4>>]),
    erlang:bump_reductions(10000000),
    _ = binary:longest_common_suffix(
	  [<<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,3,3,0:10000,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	   <<1,2,4,0:10000>>]),
    _ = binary:longest_common_suffix(
	  [<<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<1,2,4,0:10000>>,
	   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	   <<1,2,4,0:10000>>]),
    Subj = subj(),
    Len = byte_size(Subj),
    Len = binary:longest_common_suffix(
	    [Subj,Subj,Subj]),
    io:format("limit was: ~p~n",
	      [erts_debug:set_internal_state(binary_loop_limit,
					     default)]),
    erts_debug:set_internal_state(available_internal_state,false),
    ok.

subj() ->
    Me = self(),
    spawn(fun() ->
		  X0 = iolist_to_binary([
					 "1234567890",
					 lists:duplicate(100, $x)
					]),
		  Me ! X0,
		  receive X -> X end
	  end),
    X0 = receive A -> A end,
    <<X1:32/binary,_/binary>> = X0,
    Subject= <<X1/binary>>,
    Subject.


%% Test correct return values for scopes (OTP-9701).
scope_return(Config) when is_list(Config) ->
    N=10000,
    Bin=binary:copy(<<"a">>,N),
    scope_loop(Bin,0,N).

scope_loop(_,N,N) ->
    ok;
scope_loop(Bin,N,M) ->
    {N,1} = binary:match(Bin,<<"a">>,[{scope,{N,1}}]),
    {N,1} = binary:match(Bin,[<<"a">>,<<"b">>],[{scope,{N,1}}]),
    scope_loop(Bin,N+1,M).

%% Try some interesting patterns.
interesting(Config) when is_list(Config) ->
    X = do_interesting(binary),
    X = do_interesting(binref).

do_interesting(Module) ->
    {0,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"12">>,<<"1234">>,
						 <<"23">>,<<"3">>,
						 <<"34">>,<<"456">>,
						 <<"45">>,<<"6">>])),
    [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   Module:compile_pattern([<<"12">>,<<"1234">>,
							   <<"23">>,<<"3">>,
							   <<"34">>,<<"456">>,
							   <<"45">>,<<"6">>])),
    [{0,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"12">>,<<"1234">>,
						     <<"23">>,<<"3">>,
						     <<"34">>,<<"456">>,
						     <<"45">>])),
    [{0,2},{2,2}] = Module:matches(<<"123456">>,
				   Module:compile_pattern([<<"12">>,
							   <<"23">>,<<"3">>,
							   <<"34">>,<<"456">>,
							   <<"45">>])),
    {1,4} = Module:match(<<"123456">>,
			 Module:compile_pattern([<<"34">>,<<"34">>,
						 <<"12347">>,<<"2345">>])),
    [{1,4}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2345">>])),
    [{2,2}] = Module:matches(<<"123456">>,
			     Module:compile_pattern([<<"34">>,<<"34">>,
						     <<"12347">>,<<"2346">>])),

    {0,4} = Module:match(<<"123456">>,
			 [<<"12">>,<<"1234">>,
			  <<"23">>,<<"3">>,
			  <<"34">>,<<"456">>,
			  <<"45">>,<<"6">>]),
    [{0,4},{5,1}] = Module:matches(<<"123456">>,
				   [<<"12">>,<<"1234">>,
				    <<"23">>,<<"3">>,
				    <<"34">>,<<"456">>,
				    <<"45">>,<<"6">>]),
    [{0,4}] = Module:matches(<<"123456">>,
			     [<<"12">>,<<"1234">>,
			      <<"23">>,<<"3">>,
			      <<"34">>,<<"456">>,
			      <<"45">>]),
    [{0,2},{2,2}] = Module:matches(<<"123456">>,
				   [<<"12">>,
				    <<"23">>,<<"3">>,
				    <<"34">>,<<"456">>,
				    <<"45">>]),
    {1,4} = Module:match(<<"123456">>,
			 [<<"34">>,<<"34">>,
			  <<"12347">>,<<"2345">>]),
    [{1,4}] = Module:matches(<<"123456">>,
			     [<<"34">>,<<"34">>,
			      <<"12347">>,<<"2345">>]),
    [{2,2}] = Module:matches(<<"123456">>,
			     [<<"34">>,<<"34">>,
			      <<"12347">>,<<"2346">>]),
    nomatch = Module:match(<<1,2,3,4>>,<<2>>,[{scope,{0,1}}]),
    {1,1} = Module:match(<<1,2,3,4>>,<<2>>,[{scope,{0,2}}]),
    nomatch = Module:match(<<1,2,3,4>>,<<2,3>>,[{scope,{0,2}}]),
    {1,2} = Module:match(<<1,2,3,4>>,<<2,3>>,[{scope,{0,3}}]),
    {1,2} = Module:match(<<1,2,3,4>>,<<2,3>>,[{scope,{0,4}}]),
    badarg = ?MASK_ERROR(Module:match(<<1,2,3,4>>,<<2,3>>,
				      [{scope,{0,5}}])),
    {1,2} = Module:match(<<1,2,3,4>>,<<2,3>>,[{scope,{4,-4}}]),
    {0,3} = Module:match(<<1,2,3,4>>,<<1,2,3>>,[{scope,{4,-4}}]),
    {0,4} = Module:match(<<1,2,3,4>>,<<1,2,3,4>>,[{scope,{4,-4}}]),
    badarg = ?MASK_ERROR(Module:match(<<1,2,3,4>>,<<1,2,3,4>>,
				      [{scope,{3,-4}}])),
    [] = Module:matches(<<1,2,3,4>>,<<2>>,[{scope,{0,1}}]),
    [{1,1}] = Module:matches(<<1,2,3,4>>,[<<2>>,<<3>>],[{scope,{0,2}}]),
    [] = Module:matches(<<1,2,3,4>>,<<2,3>>,[{scope,{0,2}}]),
    [{1,2}] = Module:matches(<<1,2,3,4>>,<<2,3>>,[{scope,{0,3}}]),
    [{1,2}] = Module:matches(<<1,2,3,4>>,<<2,3>>,[{scope,{0,4}}]),
    [{1,2}] = Module:matches(<<1,2,3,4>>,[<<2,3>>,<<4>>],
			     [{scope,{0,3}}]),
    [{1,2},{3,1}] = Module:matches(<<1,2,3,4>>,[<<2,3>>,<<4>>],
				   [{scope,{0,4}}]),
    badarg = ?MASK_ERROR(Module:matches(<<1,2,3,4>>,<<2,3>>,
					[{scope,{0,5}}])),
    [{1,2}] = Module:matches(<<1,2,3,4>>,<<2,3>>,[{scope,{4,-4}}]),
    [{1,2},{3,1}] = Module:matches(<<1,2,3,4>>,[<<2,3>>,<<4>>],
				   [{scope,{4,-4}}]),
    [{0,3}] = Module:matches(<<1,2,3,4>>,<<1,2,3>>,[{scope,{4,-4}}]),
    [{0,4}] = Module:matches(<<1,2,3,4>>,<<1,2,3,4>>,[{scope,{4,-4}}]),
    badarg = ?MASK_ERROR(Module:matches(<<1,2,3,4>>,<<1,2,3,4>>,
					[{scope,{3,-4}}])),
    badarg = ?MASK_ERROR(Module:matches(<<1,2,3,4>>,[<<1,2,3,4>>],
					[{scope,{3,-4}}])),
    [<<1,2,3>>,<<6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,<<4,5>>),
    [<<1,2,3>>,<<6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					 [<<4,5>>,<<7>>]),
    [<<1,2,3>>,<<6>>,<<8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					   [<<4,5>>,<<7>>],[global]),
    [<<1,2,3>>,<<6>>,<<>>,<<>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					       [<<4,5>>,<<7>>,<<8>>],
					       [global]),
    [<<1,2,3>>,<<6>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
				     [<<4,5>>,<<7>>,<<8>>],
				     [global,trim]),
    [<<1,2,3>>,<<6>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
				     [<<4,5>>,<<7>>,<<8>>],
				     [global,trim_all]),
    [<<1,2,3,4,5,6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					 [<<4,5>>,<<7>>,<<8>>],
					 [global,trim,{scope,{0,4}}]),
    [<<1,2,3>>,<<6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					 [<<4,5>>,<<7>>,<<8>>],
					 [global,trim,{scope,{0,5}}]),

    [<<>>,<<>>,<<3>>,<<6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					       [<<1>>,<<2>>,<<4,5>>],
					       [global,trim]),
    [<<3>>,<<6,7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
				     [<<1>>,<<2>>,<<4,5>>],
				     [global,trim_all]),

    [<<1,2,3>>,<<>>,<<7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
					    [<<4,5>>,<<6>>],
					    [global,trim]),
    [<<1,2,3>>,<<7,8>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
				       [<<4,5>>,<<6>>],
				       [global,trim_all]),
    [<<>>,<<>>,<<3>>,<<>>,<<6>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
						[<<1>>,<<2>>,<<4>>,<<5>>,<<7>>,<<8>>],
						[global,trim]),
    [<<3>>,<<6>>] = Module:split(<<1,2,3,4,5,6,7,8>>,
				 [<<1>>,<<2>>,<<4>>,<<5>>,<<7>>,<<8>>],
				 [global,trim_all]),
    [<<>>] = binary:split(<<>>, <<",">>, []),
    [] = binary:split(<<>>, <<",">>, [trim]),
    [] = binary:split(<<>>, <<",">>, [trim_all]),
    [] = binary:split(<<>>, <<",">>, [global,trim]),
    [] = binary:split(<<>>, <<",">>, [global,trim_all]),

    badarg = ?MASK_ERROR(
		Module:replace(<<1,2,3,4,5,6,7,8>>,
			       [<<4,5>>,<<7>>,<<8>>],<<99>>,
			       [global,trim,{scope,{0,5}}])),
    <<1,2,3,99,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<99>>,[]),
    <<1,2,3,99,6,99,99>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					  [<<4,5>>,<<7>>,<<8>>],<<99>>,
					  [global]),
    <<1,2,3,99,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<99>>,
					[global,{scope,{0,5}}]),
    <<1,2,3,99,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<99>>,
					[global,{scope,{0,5}}]),
    <<1,2,3,99,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<99>>,
					[global,{scope,{0,5}}]),
    badarg = ?MASK_ERROR(Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<99>>,
					[global,{scope,{0,5}},
					 {insert,1}])),
    <<1,2,3,99,4,5,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					    [<<4,5>>,<<7>>,<<8>>],<<99>>,
					    [global,{scope,{0,5}},
					     {insert_replaced,1}]),
    <<1,2,3,9,4,5,9,6,7,8>> = Module:replace(<<1,2,3,4,5,6,7,8>>,
					     [<<4,5>>,<<7>>,<<8>>],
					     <<9,9>>,
					     [global,{scope,{0,5}},
					      {insert_replaced,1}]),
    badarg = ?MASK_ERROR(Module:replace(<<1,2,3,4,5,6,7,8>>,
					[<<4,5>>,<<7>>,<<8>>],<<>>,
					[global,{scope,{0,5}},
					 {insert_replaced,1}])),
    2 = Module:longest_common_prefix([<<1,2,4>>,<<1,2,3>>]),
    2 = Module:longest_common_prefix([<<1,2,4>>,<<1,2>>]),
    1 = Module:longest_common_prefix([<<1,2,4>>,<<1>>]),
    0 = Module:longest_common_prefix([<<1,2,4>>,<<>>]),
    1 = Module:longest_common_prefix([<<1,2,4>>,<<1,2,3>>,<<1,3,3>>]),
    1 = Module:longest_common_prefix([<<1,2,4>>,<<1,2,3>>,<<1,3,3>>,<<1,2,4>>]),
    1251 = Module:longest_common_prefix([<<0:10000,1,2,4>>,
					 <<0:10000,1,2,3>>,
					 <<0:10000,1,3,3>>,
					 <<0:10000,1,2,4>>]),
    12501 = Module:longest_common_prefix([<<0:100000,1,2,4>>,
					  <<0:100000,1,2,3>>,
					  <<0:100000,1,3,3>>,
					  <<0:100000,1,2,4>>]),
    1251 = Module:longest_common_prefix(
	     [make_unaligned(<<0:10000,1,2,4>>),
	      <<0:10000,1,2,3>>,
	      make_unaligned(<<0:10000,1,3,3>>),
	      <<0:10000,1,2,4>>]),
    12501 = Module:longest_common_prefix(
	      [<<0:100000,1,2,4>>,
	       make_unaligned(<<0:100000,1,2,3>>),
	       <<0:100000,1,3,3>>,
	       make_unaligned(<<0:100000,1,2,4>>)]),
    1250001 = Module:longest_common_prefix([<<0:10000000,1,2,4>>,
					    <<0:10000000,1,2,3>>,
					    <<0:10000000,1,3,3>>,
					    <<0:10000000,1,2,4>>]),
    if % Too cruel for the reference implementation
	Module =:= binary ->
	    erts_debug:set_internal_state(available_internal_state,true),
	    io:format("oldlimit: ~p~n",
		      [erts_debug:set_internal_state(
			 binary_loop_limit,100)]),
	    1250001 = Module:longest_common_prefix(
			[<<0:10000000,1,2,4>>,
			 <<0:10000000,1,2,3>>,
			 <<0:10000000,1,3,3>>,
			 <<0:10000000,1,2,4>>]),
	    io:format("limit was: ~p~n",
		      [erts_debug:set_internal_state(binary_loop_limit,
						     default)]),
	    erts_debug:set_internal_state(available_internal_state,
					  false);
	true ->
	    ok
    end,
    1 = Module:longest_common_suffix([<<0:100000000,1,2,4,5>>,
				      <<0:100000000,1,2,3,5>>,
				      <<0:100000000,1,3,3,5>>,
				      <<0:100000000,1,2,4,5>>]),
    1 = Module:longest_common_suffix([<<1,2,4,5>>,
				      <<0:100000000,1,2,3,5>>,
				      <<0:100000000,1,3,3,5>>,
				      <<0:100000000,1,2,4,5>>]),
    1 = Module:longest_common_suffix([<<1,2,4,5,5>>,<<5,5>>,
				      <<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5>>]),
    0 = Module:longest_common_suffix([<<1,2,4,5,5>>,<<5,5>>,
				      <<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4>>]),
    2 = Module:longest_common_suffix([<<1,2,4,5,5>>,<<5,5>>,
				      <<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    1 = Module:longest_common_suffix([<<1,2,4,5,5>>,<<5>>,
				      <<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    0 = Module:longest_common_suffix([<<1,2,4,5,5>>,<<>>,
				      <<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    0 = Module:longest_common_suffix([<<>>,<<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    0 = Module:longest_common_suffix([<<>>,<<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    2 = Module:longest_common_suffix([<<5,5>>,<<0:100000000,1,3,3,5,5>>,
				      <<0:100000000,1,2,4,5,5>>]),
    2 = Module:longest_common_suffix([<<5,5>>,<<5,5>>,<<4,5,5>>]),
    2 = Module:longest_common_suffix([<<5,5>>,<<5,5>>,<<5,5>>]),
    3 = Module:longest_common_suffix([<<4,5,5>>,<<4,5,5>>,<<4,5,5>>]),
    0 = Module:longest_common_suffix([<<>>]),
    badarg = ?MASK_ERROR(Module:longest_common_suffix([])),
    badarg = ?MASK_ERROR(Module:longest_common_suffix([apa])),
    badarg = ?MASK_ERROR(Module:longest_common_suffix([[<<>>]])),
    badarg = ?MASK_ERROR(Module:longest_common_suffix([[<<0>>,
							<<1:9>>]])),
    0 = Module:longest_common_prefix([<<>>]),
    badarg = ?MASK_ERROR(Module:longest_common_prefix([])),
    badarg = ?MASK_ERROR(Module:longest_common_prefix([apa])),
    badarg = ?MASK_ERROR(Module:longest_common_prefix([[<<>>]])),
    badarg = ?MASK_ERROR(Module:longest_common_prefix([[<<0>>,
							<<1:9>>]])),

    <<1:6,Bin:3/binary,_:2>> = <<1:6,1,2,3,1:2>>,
    <<1,2,3>> = Bin,
    1 = Module:first(Bin),
    1 = Module:first(<<1>>),
    1 = Module:first(<<1,2,3>>),
    badarg = ?MASK_ERROR(Module:first(<<>>)),
    badarg = ?MASK_ERROR(Module:first(apa)),
    3 = Module:last(Bin),
    1 = Module:last(<<1>>),
    3 = Module:last(<<1,2,3>>),
    badarg = ?MASK_ERROR(Module:last(<<>>)),
    badarg = ?MASK_ERROR(Module:last(apa)),
    1 = Module:at(Bin,0),
    1 = Module:at(<<1>>,0),
    1 = Module:at(<<1,2,3>>,0),
    2 = Module:at(<<1,2,3>>,1),
    3 = Module:at(<<1,2,3>>,2),
    badarg = ?MASK_ERROR(Module:at(<<1,2,3>>,3)),
    badarg = ?MASK_ERROR(Module:at(<<1,2,3>>,-1)),
    badarg = ?MASK_ERROR(Module:at(<<1,2,3>>,apa)),
    "hejsan" = [ Module:at(<<"hejsan">>,I) || I <- lists:seq(0,5) ],

    badarg = ?MASK_ERROR(Module:bin_to_list(<<1,2,3>>,3,-4)),
    [1,2,3] = ?MASK_ERROR(Module:bin_to_list(<<1,2,3>>,3,-3)),

    badarg = ?MASK_ERROR(Module:decode_unsigned(<<1,2,1:2>>,big)),
    badarg = ?MASK_ERROR(Module:decode_unsigned(<<1,2,1:2>>,little)),
    badarg = ?MASK_ERROR(Module:decode_unsigned(apa)),
    badarg = ?MASK_ERROR(Module:decode_unsigned(125,little)),
    0 = ?MASK_ERROR(Module:decode_unsigned(<<>>,little)),
    0 = ?MASK_ERROR(Module:decode_unsigned(<<>>,big)),
    0 = ?MASK_ERROR(Module:decode_unsigned(<<0>>,little)),
    0 = ?MASK_ERROR(Module:decode_unsigned(<<0>>,big)),
    0 = ?MASK_ERROR(Module:decode_unsigned(make_unaligned(<<0>>),
					   little)),
    0 = ?MASK_ERROR(Module:decode_unsigned(make_unaligned(<<0>>),big)),
    badarg = ?MASK_ERROR(Module:encode_unsigned(apa)),
    badarg = ?MASK_ERROR(Module:encode_unsigned(125.3,little)),
    badarg = ?MASK_ERROR(Module:encode_unsigned({1},little)),
    badarg = ?MASK_ERROR(Module:encode_unsigned([1],little)),
    <<0>> = ?MASK_ERROR(Module:encode_unsigned(0,little)),
    <<0>> = ?MASK_ERROR(Module:encode_unsigned(0,big)),
    ok.

%% Test binary:encode_unsigned/1,2 and binary:decode_unsigned/1,2.
encode_decode(Config) when is_list(Config) ->
    rand:seed(exsplus, {1271,769940,559934}),
    ok = encode_decode_loop({1,200},1000), % Need to be long enough
						% to create offheap binaries
    ok.

encode_decode_loop(_Range,0) ->
    ok;
encode_decode_loop(Range, X) ->
    N = random_number(Range),
    A = binary:encode_unsigned(N),
    B = binary:encode_unsigned(N,big),
    C = binref:encode_unsigned(N),
    D = binref:encode_unsigned(N,big),
    E = binary:encode_unsigned(N,little),
    F = binref:encode_unsigned(N,little),
    G = binary:decode_unsigned(A),
    H = binary:decode_unsigned(A,big),
    I = binref:decode_unsigned(A),
    J = binary:decode_unsigned(E,little),
    K = binref:decode_unsigned(E,little),
    L = binary:decode_unsigned(make_unaligned(A)),
    M = binary:decode_unsigned(make_unaligned(E),little),
    PaddedBig = <<0:48,A/binary>>,
    PaddedLittle = <<E/binary,0:48>>,
    O = binary:decode_unsigned(PaddedBig),
    P = binary:decode_unsigned(make_unaligned(PaddedBig)),
    Q = binary:decode_unsigned(PaddedLittle,little),
    R = binary:decode_unsigned(make_unaligned(PaddedLittle),little),
    S = binref:decode_unsigned(PaddedLittle,little),
    T = binref:decode_unsigned(PaddedBig),
    case (((A =:= B) and (B =:= C) and (C =:= D)) and
						    ((E =:= F)) and
								  ((N =:= G) and (G =:= H) and (H =:= I) and
													   (I =:= J) and (J =:= K) and (K =:= L) and (L =:= M)) and
																				  ((M =:= O) and (O =:= P) and (P =:= Q) and (Q =:= R) and
																											 (R =:= S) and (S =:= T)))of
	true ->
	    encode_decode_loop(Range,X-1);
	_ ->
	    io:format("Failed to encode/decode ~w~n(Results ~p)~n",
		      [N,[A,B,C,D,E,F,G,H,I,J,K,L,M,x,O,P,Q,R,S,T]]),
	    exit(mismatch)
    end.

%% Smoke test of the guard BIFs binary_part/2,3.
guard(Config) when is_list(Config) ->
    {comment, "Guard tests are run in emulator test suite"}.

%% Test referenced_byte_size/1 bif.
referenced(Config) when is_list(Config) ->
    badarg = ?MASK_ERROR(binary:referenced_byte_size([])),
    badarg = ?MASK_ERROR(binary:referenced_byte_size(apa)),
    badarg = ?MASK_ERROR(binary:referenced_byte_size({})),
    badarg = ?MASK_ERROR(binary:referenced_byte_size(1)),
    A = <<1,2,3>>,
    B = binary:copy(A,1000),
    3 = binary:referenced_byte_size(A),
    3000 = binary:referenced_byte_size(B),
    <<_:8,C:2/binary>> = A,
    3 = binary:referenced_byte_size(C),
    2 = binary:referenced_byte_size(binary:copy(C)),
    <<_:7,D:2/binary,_:1>> = A,
    2 = binary:referenced_byte_size(binary:copy(D)),
    3 = binary:referenced_byte_size(D),
    <<_:8,E:2/binary,_/binary>> = B,
    3000 = binary:referenced_byte_size(E),
    2 = binary:referenced_byte_size(binary:copy(E)),
    <<_:7,F:2/binary,_:1,_/binary>> = B,
    2 = binary:referenced_byte_size(binary:copy(F)),
    3000 = binary:referenced_byte_size(F),
    ok.



%% Test list_to_bin/1 BIF.
list_to_bin(Config) when is_list(Config) ->
    %% Just some smoke_tests first, then go nuts with random cases
    badarg = ?MASK_ERROR(binary:list_to_bin({})),
    badarg = ?MASK_ERROR(binary:list_to_bin(apa)),
    badarg = ?MASK_ERROR(binary:list_to_bin(<<"apa">>)),
    F1 = fun(L) ->
		 ?MASK_ERROR(binref:list_to_bin(L))
	 end,
    F2 = fun(L) ->
		 ?MASK_ERROR(binary:list_to_bin(L))
	 end,
    random_iolist:run(1000,F1,F2),
    ok.

%% Test copy/1,2 BIFs.
copy(Config) when is_list(Config) ->
    <<1,2,3>> = binary:copy(<<1,2,3>>),
    RS = random_string({1,10000}),
    RS = RS2 = binary:copy(RS),
    false = erts_debug:same(RS,RS2),
    <<>> = ?MASK_ERROR(binary:copy(<<1,2,3>>,0)),
    badarg = ?MASK_ERROR(binary:copy(<<1,2,3:3>>,2)),
    badarg = ?MASK_ERROR(binary:copy([],0)),
    <<>> = ?MASK_ERROR(binary:copy(<<>>,0)),
    badarg = ?MASK_ERROR(binary:copy(<<1,2,3>>,1.0)),
    badarg = ?MASK_ERROR(binary:copy(<<1,2,3>>,
				     16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)),
    <<>> = binary:copy(<<>>,10000),
    rand:seed(exsplus, {1271,769940,559934}),
    ok = random_copy(3000),
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("oldlimit: ~p~n",
	      [erts_debug:set_internal_state(binary_loop_limit,10)]),
    Subj = subj(),
    XX = binary:copy(Subj,1000),
    XX = binref:copy(Subj,1000),
    ok = random_copy(1000),
    kill_copy_loop(1000),
    io:format("limit was: ~p~n",
	      [erts_debug:set_internal_state(binary_loop_limit,
					     default)]),
    erts_debug:set_internal_state(available_internal_state,false),
    ok.

kill_copy_loop(0) ->
    ok;
kill_copy_loop(N) ->
    {Pid,Ref} = spawn_monitor(fun() ->
				      ok = random_copy(1000)
			      end),
    receive
    after 10 ->
	    ok
    end,
    exit(Pid,kill),
    receive
	{'DOWN',Ref,process,Pid,_} ->
	    kill_copy_loop(N-1)
    after 1000 ->
	    exit(did_not_die)
    end.

random_copy(0) ->
    ok;
random_copy(N) ->
    Str = random_string({0,N}),
    Num = rand:uniform(N div 10+1),
    A = ?MASK_ERROR(binary:copy(Str,Num)),
    B = ?MASK_ERROR(binref:copy(Str,Num)),
    C = ?MASK_ERROR(binary:copy(make_unaligned(Str),Num)),
    case {(A =:= B), (B =:= C)} of
	{true,true} ->
	    random_copy(N-1);
	_ ->
	    io:format("Failed to pick copy ~s ~p times~n",
		      [Str,Num]),
	    io:format("A:~p,~nB:~p,~n,C:~p.~n",
		      [A,B,C]),
	    exit(mismatch)
    end.

%% Test bin_to_list/1,2,3 BIFs.
bin_to_list(Config) when is_list(Config) ->
    %% Just some smoke_tests first, then go nuts with random cases
    X = <<1,2,3,4,0:1000000,5>>,
    Y = make_unaligned(X),
    LX = binary:bin_to_list(X),
    LX = binary:bin_to_list(X,0,byte_size(X)),
    LX = binary:bin_to_list(X,byte_size(X),-byte_size(X)),
    LX = binary:bin_to_list(X,{0,byte_size(X)}),
    LX = binary:bin_to_list(X,{byte_size(X),-byte_size(X)}),
    LY = binary:bin_to_list(Y),
    LY = binary:bin_to_list(Y,0,byte_size(Y)),
    LY = binary:bin_to_list(Y,byte_size(Y),-byte_size(Y)),
    LY = binary:bin_to_list(Y,{0,byte_size(Y)}),
    LY = binary:bin_to_list(Y,{byte_size(Y),-byte_size(Y)}),
    1 = hd(LX),
    5 = lists:last(LX),
    1 = hd(LY),
    5 = lists:last(LY),
    X = list_to_binary(LY),
    Y = list_to_binary(LY),
    X = list_to_binary(LY),
    [5] = lists:nthtail(byte_size(X)-1,LX),
    [0,5] = lists:nthtail(byte_size(X)-2,LX),
    [0,5] = lists:nthtail(byte_size(Y)-2,LY),
    rand:seed(exsplus, {1271,769940,559934}),
    ok = random_bin_to_list(5000),
    ok.

random_bin_to_list(0) ->
    ok;
random_bin_to_list(N) ->
    Str = random_string({1,N}),
    Parts0 = random_parts(10,N),
    Parts1 = Parts0 ++ [ {X+Y,-Y} || {X,Y} <- Parts0 ],
    [ begin
	  try
	      true = ?MASK_ERROR(binary:bin_to_list(Str,Z)) =:=
		  ?MASK_ERROR(binref:bin_to_list(Str,Z)),
	      true = ?MASK_ERROR(binary:bin_to_list(Str,Z)) =:=
		  ?MASK_ERROR(binary:bin_to_list(make_unaligned(Str),Z))
	  catch
	      _:_ ->
		  io:format("Error, Str = <<\"~s\">>.~nZ = ~p.~n",
			    [Str,Z]),
		  exit(badresult)
	  end
      end || Z <- Parts1 ],
    [ begin
	  try
	      true = ?MASK_ERROR(binary:bin_to_list(Str,A,B)) =:=
		  ?MASK_ERROR(binref:bin_to_list(Str,A,B)),
	      true = ?MASK_ERROR(binary:bin_to_list(Str,A,B)) =:=
		  ?MASK_ERROR(binary:bin_to_list(make_unaligned(Str),A,B))
	  catch
	      _:_ ->
		  io:format("Error, Str = <<\"~s\">>.~nA = ~p.~nB = ~p.~n",
			    [Str,A,B]),
		  exit(badresult)
	  end
      end || {A,B} <- Parts1 ],
    random_bin_to_list(N-1).

%% Test the part/2,3 BIFs.
parts(Config) when is_list(Config) ->
    %% Some simple smoke tests to begin with
    Simple = <<1,2,3,4,5,6,7,8>>,
    <<1,2>> = binary:part(Simple,0,2),
    <<1,2>> = binary:part(Simple,{0,2}),
    Simple = binary:part(Simple,0,8),
    Simple = binary:part(Simple,{0,8}),
    badarg = ?MASK_ERROR(binary:part(Simple,0,9)),
    badarg = ?MASK_ERROR(binary:part(Simple,{0,9})),
    badarg = ?MASK_ERROR(binary:part(Simple,1,8)),
    badarg = ?MASK_ERROR(binary:part(Simple,{1,8})),
    badarg = ?MASK_ERROR(binary:part(Simple,{3,-4})),
    badarg = ?MASK_ERROR(binary:part(Simple,{3.0,1})),
    badarg = ?MASK_ERROR(
		binary:part(Simple,{16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFF
				    ,1})),
    <<2,3,4,5,6,7,8>> = binary:part(Simple,{1,7}),
    <<2,3,4,5,6,7,8>> = binary:part(Simple,{8,-7}),
    Simple = binary:part(Simple,{8,-8}),
    badarg = ?MASK_ERROR(binary:part(Simple,{1,-8})),
    badarg = ?MASK_ERROR(binary:part(Simple,{8,-9})),
    badarg = ?MASK_ERROR(binary:part(Simple,{0,-1})),
    <<>> = binary:part(Simple,{8,0}),
    badarg = ?MASK_ERROR(binary:part(Simple,{9,0})),
    badarg = ?MASK_ERROR(binary:part(Simple,{-1,0})),
    badarg = ?MASK_ERROR(binary:part(Simple,{7,2})),
    <<8>> = binary:part(Simple,{7,1}),
    rand:seed(exsplus, {1271,769940,559934}),
    random_parts(5000),
    ok.


random_parts(0) ->
    ok;
random_parts(N) ->
    Str = random_string({1,N}),
    Parts0 = random_parts(10,N),
    Parts1 = Parts0 ++ [ {X+Y,-Y} || {X,Y} <- Parts0 ],
    [ begin
	  true = ?MASK_ERROR(binary:part(Str,Z)) =:=
	      ?MASK_ERROR(binref:part(Str,Z)),
	  true = ?MASK_ERROR(binary:part(Str,Z)) =:=
	      ?MASK_ERROR(erlang:binary_part(Str,Z)),
	  true = ?MASK_ERROR(binary:part(Str,Z)) =:=
	      ?MASK_ERROR(binary:part(make_unaligned(Str),Z))
      end || Z <- Parts1 ],
    random_parts(N-1).

random_parts(0,_) ->
    [];
random_parts(X,N) ->
    Pos = rand:uniform(N),
    Len = rand:uniform((Pos * 12) div 10),
    [{Pos,Len} | random_parts(X-1,N)].

%% Test pseudorandomly generated cases against reference implementation.
random_ref_comp(Config) when is_list(Config) ->
    ct:timetrap({minutes,30}), %% valgrind needs a lot of time
    put(success_counter,0),
    rand:seed(exsplus, {1271,769940,559934}),
    Nr = {1,40},
    Hr = {30,1000},
    I1 = 1500,
    I2 = 5,
    do_random_match_comp(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_match_comp2(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_match_comp3(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_match_comp4(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_matches_comp(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_matches_comp2(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_matches_comp3(I2,Nr,Hr),
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("oldlimit: ~p~n",[ erts_debug:set_internal_state(binary_loop_limit,100)]),
    do_random_match_comp(I1,Nr,Hr),
    do_random_matches_comp3(I2,Nr,Hr),
    io:format("limit was: ~p~n",[ erts_debug:set_internal_state(binary_loop_limit,default)]),
    erts_debug:set_internal_state(available_internal_state,false),
    ok.

%% Test pseudorandomly generated cases against reference implementation
%% of split and replace.
random_ref_sr_comp(Config) when is_list(Config) ->
    ct:timetrap({minutes,30}), %% valgrind needs a lot
    put(success_counter,0),
    rand:seed(exsplus, {1271,769940,559934}),
    Nr = {1,40},
    Hr = {30,1000},
    I1 = 1500,
    do_random_split_comp(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_replace_comp(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_split_comp2(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    do_random_replace_comp2(I1,Nr,Hr),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ok.

%% Test pseudorandomly generated cases against reference implementation
%% of split and replace.
random_ref_fla_comp(Config) when is_list(Config) ->
    put(success_counter,0),
    rand:seed(exsplus, {1271,769940,559934}),
    do_random_first_comp(5000,{1,1000}),
    do_random_last_comp(5000,{1,1000}),
    do_random_at_comp(5000,{1,1000}),
    io:format("Number of successes: ~p~n",[get(success_counter)]),
    ok.

do_random_first_comp(0,_) ->
    ok;
do_random_first_comp(N,Range) ->
    S = random_string(Range),
    A = ?MASK_ERROR(binref:first(S)),
    B = ?MASK_ERROR(binary:first(S)),
    C = ?MASK_ERROR(binary:first(make_unaligned(S))),
    case {(A =:= B), (B =:= C)} of
	{true,true} ->
	    do_random_first_comp(N-1,Range);
	_ ->
	    io:format("Failed to pick first of ~s~n",
		      [S]),
	    io:format("A:~p,~nB:~p,~n,C:~p.~n",
		      [A,B,C]),
	    exit(mismatch)
    end.

do_random_last_comp(0,_) ->
    ok;
do_random_last_comp(N,Range) ->
    S = random_string(Range),
    A = ?MASK_ERROR(binref:last(S)),
    B = ?MASK_ERROR(binary:last(S)),
    C = ?MASK_ERROR(binary:last(make_unaligned(S))),
    case {(A =:= B), (B =:= C)} of
	{true,true} ->
	    do_random_last_comp(N-1,Range);
	_ ->
	    io:format("Failed to pick last of ~s~n",
		      [S]),
	    io:format("A:~p,~nB:~p,~n,C:~p.~n",
		      [A,B,C]),
	    exit(mismatch)
    end.
do_random_at_comp(0,_) ->
    ok;
do_random_at_comp(N,{Min,Max}=Range) ->
    S = random_string(Range),
    XMax = Min + ((Max - Min) * 3) div 4,
    Pos = random_length({Min,XMax}), %% some out of range
    A = ?MASK_ERROR(binref:at(S,Pos)),
    B = ?MASK_ERROR(binary:at(S,Pos)),
    C = ?MASK_ERROR(binary:at(make_unaligned(S),Pos)),
    if
	A =/= badarg ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C)} of
	{true,true} ->
	    do_random_at_comp(N-1,Range);
	_ ->
	    io:format("Failed to pick last of ~s~n",
		      [S]),
	    io:format("A:~p,~nB:~p,~n,C:~p.~n",
		      [A,B,C]),
	    exit(mismatch)
    end.

do_random_matches_comp(0,_,_) ->
    ok;
do_random_matches_comp(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Needles = [random_string(NeedleRange) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    Haystack = random_string(HaystackRange),
    true = do_matches_comp(Needles,Haystack),
    do_random_matches_comp(N-1,NeedleRange,HaystackRange).

do_random_matches_comp2(0,_,_) ->
    ok;
do_random_matches_comp2(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_matches_comp(Needles,Haystack),
    do_random_matches_comp2(N-1,NeedleRange,HaystackRange).

do_random_matches_comp3(0,_,_) ->
    ok;
do_random_matches_comp3(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    RefRes = binref:matches(Haystack,Needles),
    RefRes = binary:matches(Haystack,Needles),
    Compiled = binary:compile_pattern(Needles),
    true = do_matches_comp_loop(10000,Compiled,Haystack, RefRes),
    do_random_matches_comp3(N-1,NeedleRange,HaystackRange).

do_matches_comp_loop(0,_,_,_) ->
    true;
do_matches_comp_loop(N, Needles, Haystack0,RR) ->
    DummySize=N*8,
    Haystack1 = <<0:DummySize,Haystack0/binary>>,
    RR1=[{X+N,Y} || {X,Y} <- RR],
    true = do_matches_comp2(Needles,Haystack1,RR1),
    Haystack2 = <<Haystack0/binary,Haystack1/binary>>,
    RR2 = RR ++ [{X2+N+byte_size(Haystack0),Y2} || {X2,Y2} <- RR],
    true = do_matches_comp2(Needles,Haystack2,RR2),
    do_matches_comp_loop(N-1, Needles, Haystack0,RR).


do_matches_comp2(N,H,A) ->
    C = ?MASK_ERROR(binary:matches(H,N)),
    case (A =:= C) of
	true ->
	    true;
	_ ->
	    io:format("Failed to match ~p (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~n,C:~p.~n",
		      [A,C]),
	    exit(mismatch)
    end.
do_matches_comp(N,H) ->
    A = ?MASK_ERROR(binref:matches(H,N)),
    B = ?MASK_ERROR(binary:matches(H,N)),
    C = ?MASK_ERROR(binary:matches(make_unaligned(H),
				   binary:compile_pattern([make_unaligned2(X) || X <- N]))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C)} of
	{true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~p (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p,~n",
		      [A,B,C]),
	    exit(mismatch)
    end.

do_random_match_comp(0,_,_) ->
    ok;
do_random_match_comp(N,NeedleRange,HaystackRange) ->
    Needle = random_string(NeedleRange),
    Haystack = random_string(HaystackRange),
    true = do_match_comp(Needle,Haystack),
    do_random_match_comp(N-1,NeedleRange,HaystackRange).

do_random_match_comp2(0,_,_) ->
    ok;
do_random_match_comp2(N,NeedleRange,HaystackRange) ->
    Haystack = random_string(HaystackRange),
    Needle = random_substring(NeedleRange,Haystack),
    true = do_match_comp(Needle,Haystack),
    do_random_match_comp2(N-1,NeedleRange,HaystackRange).

do_random_match_comp3(0,_,_) ->
    ok;
do_random_match_comp3(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_match_comp3(Needles,Haystack),
    do_random_match_comp3(N-1,NeedleRange,HaystackRange).

do_random_match_comp4(0,_,_) ->
    ok;
do_random_match_comp4(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_string(NeedleRange) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_match_comp3(Needles,Haystack),
    do_random_match_comp4(N-1,NeedleRange,HaystackRange).

do_match_comp(N,H) ->
    A = ?MASK_ERROR(binref:match(H,N)),
    B = ?MASK_ERROR(binary:match(make_unaligned(H),N)),
    C = ?MASK_ERROR(binary:match(H,binary:compile_pattern([N]))),
    D = ?MASK_ERROR(binary:match(H,binary:compile_pattern(make_unaligned(N)))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B), (B =:= C),(C =:= D)} of
	{true,true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~s (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p,~n,D:~p.~n",
		      [A,B,C,D]),
	    exit(mismatch)
    end.

do_match_comp3(N,H) ->
    A = ?MASK_ERROR(binref:match(H,N)),
    B = ?MASK_ERROR(binary:match(H,N)),
    C = ?MASK_ERROR(binary:match(H,binary:compile_pattern(N))),
    if
	A =/= nomatch ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case {(A =:= B),(B =:= C)} of
	{true,true} ->
	    true;
	_ ->
	    io:format("Failed to match ~s (needle) against ~s (haystack)~n",
		      [N,H]),
	    io:format("A:~p,~nB:~p,~n,C:~p.~n",
		      [A,B,C]),
	    exit(mismatch)
    end.

do_random_split_comp(0,_,_) ->
    ok;
do_random_split_comp(N,NeedleRange,HaystackRange) ->
    Haystack = random_string(HaystackRange),
    Needle = random_substring(NeedleRange,Haystack),
    true = do_split_comp(Needle,Haystack,[]),
    true = do_split_comp(Needle,Haystack,[global]),
    true = do_split_comp(Needle,Haystack,[global,trim]),
    true = do_split_comp(Needle,Haystack,[global,trim_all]),
    true = do_split_comp(Needle,Haystack,[global,trim,trim_all]),
    do_random_split_comp(N-1,NeedleRange,HaystackRange).
do_random_split_comp2(0,_,_) ->
    ok;
do_random_split_comp2(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    true = do_split_comp(Needles,Haystack,[]),
    true = do_split_comp(Needles,Haystack,[global]),
    true = do_split_comp(Needles,Haystack,[global,trim]),
    true = do_split_comp(Needles,Haystack,[global,trim_all]),
    true = do_split_comp(Needles,Haystack,[global,trim,trim_all]),
    do_random_split_comp2(N-1,NeedleRange,HaystackRange).

do_split_comp(N,H,Opts) ->
    A = ?MASK_ERROR(binref:split(H,N,Opts)),
    D = ?MASK_ERROR(binary:split(H,binary:compile_pattern(N),Opts)),
    if
	(A =/= [N]) and is_list(A) ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case (A =:= D) of
	true ->
	    true;
	_ ->
	    io:format("Failed to split ~n~p ~n(haystack) with ~n~p ~n(needle) "
		      "~nand options ~p~n",
		      [H,N,Opts]),
	    io:format("A:~p,D:~p.~n",
		      [A,D]),
	    exit(mismatch)
    end.

do_random_replace_comp(0,_,_) ->
    ok;
do_random_replace_comp(N,NeedleRange,HaystackRange) ->
    Haystack = random_string(HaystackRange),
    Needle = random_substring(NeedleRange,Haystack),
    Repl = random_string(NeedleRange),
    Insertat = random_length(NeedleRange), %Sometimes larger than Repl
    true = do_replace_comp(Needle,Haystack,Repl,[]),
    true = do_replace_comp(Needle,Haystack,Repl,[global]),
    true = do_replace_comp(Needle,Haystack,Repl,
			   [global,{insert_replaced,Insertat}]),
    do_random_replace_comp(N-1,NeedleRange,HaystackRange).
do_random_replace_comp2(0,_,_) ->
    ok;
do_random_replace_comp2(N,NeedleRange,HaystackRange) ->
    NumNeedles = element(2,HaystackRange) div element(2,NeedleRange),
    Haystack = random_string(HaystackRange),
    Needles = [random_substring(NeedleRange,Haystack) ||
		  _ <- lists:duplicate(NumNeedles,a)],
    Repl = random_string(NeedleRange),
    Insertat = random_length(NeedleRange), %Sometimes larger than Repl
    true = do_replace_comp(Needles,Haystack,Repl,[]),
    true = do_replace_comp(Needles,Haystack,Repl,[global]),
    true = do_replace_comp(Needles,Haystack,Repl,
			   [global,{insert_replaced,Insertat}]),
    do_random_replace_comp2(N-1,NeedleRange,HaystackRange).

do_replace_comp(N,H,R,Opts) ->
    A = ?MASK_ERROR(binref:replace(H,N,R,Opts)),
    D = ?MASK_ERROR(binary:replace(H,binary:compile_pattern(N),R,Opts)),
    if
	(A =/= N) and is_binary(A) ->
	    put(success_counter,get(success_counter)+1);
	true ->
	    ok
    end,
    case (A =:= D) of
	true ->
	    true;
	_ ->
	    io:format("Failed to replace ~s (haystack) by ~s (needle) "
		      "inserting ~s (replacement) and options ~p~n",
		      [H,N,R,Opts]),
	    io:format("A:~p,D:~p.~n",
		      [A,D]),
	    exit(mismatch)
    end.

one_random_number(N) ->
    M = ((N - 1) rem 10) + 1,
    element(M,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9}).

one_random(N) ->
    M = ((N - 1) rem 68) + 1,
    element(M,{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,
	       $u,$v,$w,$x,$y,$z,$å,$ä,$ö,$A,$B,$C,$D,$E,$F,$G,$H,
	       $I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,$W,$X,$Y,$Z,$Å,
	       $Ä,$Ö,$0,$1,$2,$3,$4,$5,$6,$7,$8,$9}).

random_number({Min,Max}) -> % Min and Max are *length* of number in
						% decimal positions
    X = rand:uniform(Max - Min + 1) + Min - 1,
    list_to_integer([one_random_number(rand:uniform(10)) || _ <- lists:seq(1,X)]).


random_length({Min,Max}) ->
    rand:uniform(Max - Min + 1) + Min - 1.
random_string({Min,Max}) ->
    X = rand:uniform(Max - Min + 1) + Min - 1,
    list_to_binary([one_random(rand:uniform(68)) || _ <- lists:seq(1,X)]).
random_substring({Min,Max},Hay) ->
    X = rand:uniform(Max - Min + 1) + Min - 1,
    Y = byte_size(Hay),
    Z = if
	    X > Y -> Y;
	    true -> X
	end,
    PMax = Y - Z,
    Pos = rand:uniform(PMax + 1) - 1,
    <<_:Pos/binary,Res:Z/binary,_/binary>> = Hay,
    Res.

mask_error({'EXIT',{Err,_}}) ->
    Err;
mask_error(Else) ->
    Else.

make_unaligned(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = byte_size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.
make_unaligned2(Bin0) when is_binary(Bin0) ->
    Bin1 = <<31:5,Bin0/binary,0:3>>,
    Sz = byte_size(Bin0),
    <<31:5,Bin:Sz/binary,0:3>> = id(Bin1),
    Bin.

id(I) -> I.
