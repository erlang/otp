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
-module(otp_7202).
-export([?MODULE/0]).

?MODULE() ->
    test().

test() ->
    List = [a],
    Error = case func() of
                no_value -> true;
                {ok, V} -> V
             end,
    %% Liveness calculation for the make_fun2 instruction was wrong -
    %% it looked like Error would not be needed by the make_fun2 instruction.
    lists:foreach(fun(_E) ->
                          case Error of
                              true ->
                                  ok;
                              false ->
                                  ok
                          end
                  end, List).

func() ->
    no_value. 



    
    
