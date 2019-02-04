%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket_test_lib).

-export([
         pi/1, pi/2, pi/3,

         %% Time stuff
         timestamp/0,
         tdiff/2,
         formated_timestamp/0,
         format_timestamp/1,

         %% String and format
         f/2,

         %% Skipping
         not_yet_implemented/0,
         skip/1
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi(Item) when is_atom(Item) ->
    pi(self(), Item).

pi(Pid, Item) when is_pid(Pid) andalso is_atom(Item) ->
    {Item, Info} = process_info(Pid, Item),
    Info;
pi(Node, Pid) when is_pid(Pid) ->
    rpc:call(Node, erlang, process_info, [Pid]).

pi(Node, Pid, Item) when is_pid(Pid) andalso is_atom(Item) ->
    rpc:call(Node, erlang, process_info, [Pid, Item]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    os:timestamp().


tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, _N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    %% {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    %% FormatTS = 
    %%     io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
    %%                   [YYYY, MM, DD, Hour, Min, Sec, N3]),  
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
