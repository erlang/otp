%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%% Originally based on Per Gustafsson's test suite.
%%

-module(bs_bincomp_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 byte_aligned/1,bit_aligned/1,extended_byte_aligned/1,
	 extended_bit_aligned/1,mixed/1,tracing/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [byte_aligned, bit_aligned, extended_byte_aligned,
     extended_bit_aligned, mixed, tracing].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



byte_aligned(Config) when is_list(Config) ->
  <<"abcdefg">> = << <<(X+32)>> || <<X>> <= <<"ABCDEFG">> >>,
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || <<X:32>> <= <<1:32,2:32,3:32,4:32>> >>,
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || <<X:16>> <= <<1:16,2:16,3:16,4:16>> >>,
  ok.

bit_aligned(Config) when is_list(Config) ->
  <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> = 
    << <<(X+32):7>> || <<X>> <= <<"ABCDEFG">> >>,
  <<"ABCDEFG">> = 
    << <<(X-32)>> || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> >>,
  <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || <<X:31>> <= <<1:31,2:31,3:31,4:31>> >>,
  <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || <<X:15>> <= <<1:15,2:15,3:15,4:15>> >>,
  ok.

extended_byte_aligned(Config) when is_list(Config) ->
  <<"abcdefg">> = << <<(X+32)>> || X <- "ABCDEFG" >>,
  "abcdefg" = [(X+32) || <<X>> <= <<"ABCDEFG">>],
  <<1:32/little,2:32/little,3:32/little,4:32/little>> =
    << <<X:32/little>> || X <- [1,2,3,4] >>,
  [256,512,768,1024] =
    [X || <<X:16/little>> <= <<1:16,2:16,3:16,4:16>>],
  ok.

extended_bit_aligned(Config) when is_list(Config) ->
  <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>> = 
    << <<(X+32):7>> || X <- "ABCDEFG" >>,
  "ABCDEFG" = [(X-32) || <<X:7>> <= <<$a:7,$b:7,$c:7,$d:7,$e:7,$f:7,$g:7>>],
  <<1:31/little,2:31/little,3:31/little,4:31/little>> =
    << <<X:31/little>> || X <- [1,2,3,4] >>,
  [256,512,768,1024] =
    [X || <<X:15/little>> <= <<1:15,2:15,3:15,4:15>>],
  ok.

mixed(Config) when is_list(Config) ->
  <<2,3,3,4,4,5,5,6>> =  
    << <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>,
  <<2,3,3,4,4,5,5,6>> =  
    << <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, Y <- [1,2] >>,
  <<2,3,3,4,4,5,5,6>> =  
    << <<(X+Y)>> || X <- [1,2,3,4], Y <- [1,2] >>,
  [2,3,3,4,4,5,5,6] =  
    [(X+Y) || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>>],
  [2,3,3,4,4,5,5,6] =  
    [(X+Y) || <<X>> <= <<1,2,3,4>>, Y <- [1,2]],
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
    << <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>> >>,
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
    << <<(X+Y):3>> || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2] >>,
  <<2:3,3:3,3:3,4:3,4:3,5:3,5:3,6:3>> =  
    << <<(X+Y):3>> || X <- [1,2,3,4], Y <- [1,2] >>,
  [2,3,3,4,4,5,5,6] =  
    [(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, <<Y:3>> <= <<1:3,2:3>>],
  [2,3,3,4,4,5,5,6] =  
    [(X+Y) || <<X:3>> <= <<1:3,2:3,3:3,4:3>>, Y <- [1,2]],
  ok.

%% OTP-8179: Call tracing on binary comprehensions would cause a crash.
tracing(Config) when is_list(Config) ->
    Self = self(),
    Tracer = spawn_opt(fun() -> tracer(Self, 0) end,
		       [link,{priority,max}]),
    Pattern = [{'_',[],[{return_trace}]}],
    erlang:trace_pattern({?MODULE,'_','_'}, Pattern, [local]),
    erlang:trace(self(), true, [call,{tracer,Tracer}]),
    random_binaries(1000),
    Tracer ! done,
    receive
	{Tracer,N} ->
	    {comment,integer_to_list(N) ++ " trace messages"}
    end.

random_binary() ->
    Seq = [1,2,3,4,5,6,7,8,9,10],
    << <<($a + rand:uniform($z - $a)):8>> || _ <- Seq >>.

random_binaries(N) when N > 0 ->
    random_binary(),
    random_binaries(N - 1);
random_binaries(_) -> ok.

tracer(Parent, N) ->
    receive
	Msg ->
	    case Msg of
		done ->
		    Parent ! {self(),N};
		_ ->
		    tracer(Parent, N+1)
	    end
    end.
