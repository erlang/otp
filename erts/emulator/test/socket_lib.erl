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

-module(socket_lib).

-export([
         sleep/1,
         req/0, rep/0,
         enc_req_msg/2, enc_rep_msg/2,
         enc_msg/3, dec_msg/1,
         request/3, reply/4,
         f/2,
         i/1, i/2,
         e/2
        ]).


-define(REQ, 0).
-define(REP, 1).


%% ---

sleep(T) ->
    receive after T -> ok end.


%% ---

req() -> ?REQ.
rep() -> ?REP.

enc_req_msg(N, Data) ->
    enc_msg(?REQ, N, Data).

enc_rep_msg(N, Data) ->
    enc_msg(?REP, N, Data).

enc_msg(Type, N, Data) when is_list(Data) ->
    enc_msg(Type, N, list_to_binary(Data));
enc_msg(Type, N, Data) 
  when is_integer(Type) andalso is_integer(N) andalso is_binary(Data) ->
    <<Type:32/integer, N:32/integer, Data/binary>>.
    
dec_msg(<<?REQ:32/integer, N:32/integer, Data/binary>>) ->
    {request, N, Data};
dec_msg(<<?REP:32/integer, N:32/integer, Data/binary>>) ->
    {reply, N, Data}.


%% ---

request(Tag, Pid, Request) ->
    Ref = make_ref(),
    Pid ! {Tag, self(), Ref, Request},
    receive
        {Tag, Pid, Ref, Reply} ->
            Reply
    end.
    
reply(Tag, Pid, Ref, Reply) ->
    Pid ! {Tag, self(), Ref, Reply}.


%% ---

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%% ---

e(F, A) ->
    p("<ERROR> " ++ F, A).

i(F) ->
    i(F, []).
i(F, A) ->
    p("*** " ++ F, A).

p(F, A) ->
    p(get(sname), F, A).

p(SName, F, A) ->
    io:format("[~s,~p][~s] " ++ F ++ "~n", 
              [SName,self(),formated_timestamp()|A]).


%% ---

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(Now, N2T, true).

format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
    FormatExtra = ".~.2.0w",
    ArgsExtra   = [N3 div 10000],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra);
format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
    FormatExtra = "",
    ArgsExtra   = [],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra).

format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
    {Date, Time}   = N2T(N),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
                      [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
    lists:flatten(FormatDate).


