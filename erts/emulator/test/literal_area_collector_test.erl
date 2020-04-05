%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019. All Rights Reserved.
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
-module(literal_area_collector_test).

-export([check_idle/1]).

check_idle(Timeout) when is_integer(Timeout) > 0 ->
    Start = erlang:monotonic_time(millisecond),
    LAC = find_lac(),
    wait_until(fun () ->
                       case process_info(LAC, [status,
                                               current_function,
                                               current_stacktrace,
                                               message_queue_len]) of
                           [{status,waiting},
                            {current_function,
                             {erts_literal_area_collector,msg_loop,4}},
                            {current_stacktrace,
                             [{erts_literal_area_collector,msg_loop,4,_}]},
                            {message_queue_len,0}] ->
                               true;
                           CurrState ->
                               Now = erlang:monotonic_time(millisecond),
                               case Now - Start > Timeout of
                                   true ->
                                       exit({non_idle_literal_area_collecor,
                                             CurrState});
                                   false ->
                                       false
                               end
                       end
               end),
    ok.
    

find_lac() ->
    try
        lists:foreach(fun (P) ->
                              case process_info(P, initial_call) of
                                  {initial_call,
                                   {erts_literal_area_collector,start,0}} ->
                                      throw({lac, P});
                                  _ ->
                                      ok
                              end
                      end, processes()),
        exit(no_literal_area_collector)
    catch
        throw:{lac, LAC} ->
            LAC
    end.
                                  

wait_until(Fun) ->
    Res = try
              Fun()
          catch
              T:R -> {T,R}
          end,
    case Res of
        true -> ok;
        _ -> wait_until(Fun)
    end.
