%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2013. All Rights Reserved.
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
%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bit_binaries_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 misc/1,horrid_match/1,test_bitstr/1,test_bit_size/1,asymmetric_tests/1,
	 big_asymmetric_tests/1,binary_to_and_from_list/1,
	 big_binary_to_and_from_list/1,send_and_receive/1,
	 send_and_receive_alot/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [misc,horrid_match,test_bitstr,test_bit_size,
       asymmetric_tests,big_asymmetric_tests,
       binary_to_and_from_list,big_binary_to_and_from_list,
       send_and_receive,send_and_receive_alot]}].
      

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


misc(Config) when is_list(Config) ->
    ?line <<1:100>> = <<1:100>>,
    ?line {ok,ok} = {match(7),match(9)},
    ?line {ok,ok} = {match1(15),match1(31)},
    ok.


match(N) ->
  <<0:N>> = <<0:N>>,
  ok.

match1(N) ->
  <<42:N/little>> = <<42:N/little>>,
  ok.

test_bit_size(Config) when is_list(Config) ->
    ?line 101 = erlang:bit_size(<<1:101>>),
    ?line 1001 = erlang:bit_size(<<1:1001>>),
    ?line 1001 = erlang:bit_size(<<-10:1001>>),
    ?line 80 = erlang:bit_size(<<1:80>>),
    ?line 800 = erlang:bit_size(<<1:800>>),
    ?line Bin = <<0:16#1000000>>,
    ?line BigBin = list_to_bitstring([Bin||_ <- lists:seq(1,16#10)]++[<<1:1>>]),
    ?line 16#10000001 = erlang:bit_size(BigBin),
    %% Only run these on computers with lots of memory
    %% HugeBin = list_to_bitstring([BigBin||_ <- lists:seq(1,16#10)]++[<<1:1>>]),
    %% 16#100000011 = erlang:bit_size(HugeBin), 
    ?line 0 = erlang:bit_size(<<>>),
    ok.

horrid_match(Config) when is_list(Config) ->
    ?line <<1:4,B:24/bitstring>> = <<1:4,42:24/little>>,
    ?line <<42:24/little>> = B, 
    ok.
			 
test_bitstr(Config) when is_list(Config) ->
    ?line <<1:7,B/bitstring>> = <<1:7,<<1:1,6>>/bitstring>>,
    ?line <<1:1,6>> = B,
    ?line B = <<1:1,6>>,
  ok.
		      
asymmetric_tests(Config) when is_list(Config) ->
    ?line <<1:12>> = <<0,1:4>>,
    ?line <<0,1:4>> = <<1:12>>,
    ?line <<1:1,X/bitstring>> = <<128,255,0,0:2>>,
    ?line <<1,254,0,0:1>> = X,
    ?line X = <<1,254,0,0:1>>,
    ?line <<1:1,X1:25/bitstring>> = <<128,255,0,0:2>>,
    ?line <<1,254,0,0:1>> = X1,
    ?line X1 = <<1,254,0,0:1>>,
    ok.

big_asymmetric_tests(Config) when is_list(Config) ->
    ?line <<1:875,1:12>> = <<1:875,0,1:4>>,
    ?line <<1:875,0,1:4>> = <<1:875,1:12>>,
    ?line <<1:1,X/bitstring>> = <<128,255,0,0:2,1:875>>,
    ?line <<1,254,0,0:1,1:875>> = X,
    ?line X = <<1,254,0,0:1,1:875>>,
    ?line <<1:1,X1:900/bitstring>> = <<128,255,0,0:2,1:875>>,
    ?line <<1,254,0,0:1,1:875>> = X1,
    ?line X1 = <<1,254,0,0:1,1:875>>,
  ok.

binary_to_and_from_list(Config) when is_list(Config) ->
    ?line <<1,2,3,4,1:1>> = list_to_bitstring(bitstring_to_list(<<1,2,3,4,1:1>>)),
    ?line [1,2,3,4,<<1:1>>] = bitstring_to_list(<<1,2,3,4,1:1>>),
    ?line <<1:1,1,2,3,4>> = list_to_bitstring([<<1:1>>,1,2,3,4]),
    ?line [128,129,1,130,<<0:1>>] = bitstring_to_list(<<1:1,1,2,3,4>>),
    ok.
 
big_binary_to_and_from_list(Config) when is_list(Config) ->
    ?line <<1:800,2,3,4,1:1>> =
	list_to_bitstring(bitstring_to_list(<<1:800,2,3,4,1:1>>)),
    ?line [1,2,3,4|_Rest1] = bitstring_to_list(<<1,2,3,4,1:800,1:1>>),
    ?line <<1:801,1,2,3,4>> = list_to_bitstring([<<1:801>>,1,2,3,4]),
    ok.  

send_and_receive(Config) when is_list(Config) -> 
    ?line Bin = <<1,2:7>>,
    Pid = spawn_link(fun() -> receiver(Bin) end),
    ?line Pid ! {self(),<<1:7,8:5,Bin/bitstring>>},
    ?line receive
	      ok ->
		  ok
	  end.

receiver(Bin) ->	 
    receive
	{Pid,<<1:7,8:5,Bin/bitstring>>} ->
	    Pid ! ok
  end.
	    
send_and_receive_alot(Config) when is_list(Config) -> 
    Bin = <<1:1000001>>,
    Pid = spawn_link(fun() -> receiver_alot(Bin) end),
    spamalot(100,Bin,Pid).

spamalot(N,Bin,Pid) when N > 0 ->
    Pid ! {self(),<<1:7,8:5,Bin/bitstring>>},
    receive
	ok ->
	    ok
    end,
    spamalot(N-1,Bin,Pid);
spamalot(0,_Bin,Pid) ->
    Pid ! no_more,
    ok.

receiver_alot(Bin) ->	 
    receive
	{Pid,<<1:7,8:5,Bin/bitstring>>} ->
	    Pid ! ok;
	no_more -> ok
    end,
    receiver_alot(Bin).



