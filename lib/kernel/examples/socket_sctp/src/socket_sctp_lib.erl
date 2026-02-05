%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

-module(socket_sctp_lib).

-export([stop/0,
         mq/1,
         f/2, is_string/1,
         which_domain/1, which_local_addr/1,
         dmsg/2, imsg/2, wmsg/2, emsg/2]).


%% ===========================================================================

which_domain(IP) when is_tuple(IP) andalso (tuple_size(IP) =:= 8) ->
    inet6;
which_domain(_) ->
    inet.


%% This gets the local "proper" address
%% (not {127, ...} or {169,254, ...} or {0, ...} or {16#fe80, ...})
which_local_addr(Domain) ->
     Filter = fun(#{flags := Flags,
                    addr  := #{family := Fam}}) when (Fam =:= Domain) ->
                      (not lists:member(loopback, Flags)) andalso
                          lists:member(up, Flags);
		(_) ->
		     false
	     end,
    case net:getifaddrs(Filter) of
        {ok, [#{addr := #{addr := Addr}}|_]} ->
            {ok, Addr};
        {ok, _} ->
            {ok, any};
        {error, _} = ERROR ->
            ERROR
    end.


%% ===========================================================================

mq(Pid) when is_pid(Pid) ->
    {messages, MQ} = process_info(Pid, messages),
    MQ.


%% ===========================================================================

is_string(MaybeStr) ->
    io_lib:char_list(MaybeStr).


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

             
%% ===========================================================================

stop() ->
    erlang:halt().


%% ===========================================================================

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    {Hour, Min, Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                             [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatTS).

dmsg(F, A) ->
    print(get(dbg), "<DEBUG>", F, A).

imsg(F, A) ->						  
    print(true, "<INFO>", F, A).

wmsg(F, A) ->
    print(true, "<WARNING>", F, A).

emsg(F, A) ->
    print(true, "<ERROR>", F, A).

print(true, Pre, F, A) ->
    io:format("~s [ ~s ][ ~s ] "++ F ++ "~n",
              [Pre, formated_timestamp(), get(sname) | A]);
print(_, _, _, _) ->
    ok.

