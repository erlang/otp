%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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



    
    
