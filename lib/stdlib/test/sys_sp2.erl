%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(sys_sp2).
-export([start_link/1]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3]).

%% Implements the ch4 example from the Design Principles doc.  Same as
%% sys_sp1 except this module does not export system_get_state/1 or
%% system_replace_state/2

start_link(NumCh) ->
    proc_lib:start_link(?MODULE, init, [[self(),NumCh]]).

alloc() ->
    ?MODULE ! {self(), alloc},
    receive
        {?MODULE, Res} ->
            Res
    end.

free(Ch) ->
    ?MODULE ! {free, Ch},
    ok.

init([Parent,NumCh]) ->
    register(?MODULE, self()),
    Chs = channels(NumCh),
    Deb = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Chs, Parent, Deb).

loop(Chs, Parent, Deb) ->
    receive
        {From, alloc} ->
            Deb2 = sys:handle_debug(Deb, fun write_debug/3,
                                    ?MODULE, {in, alloc, From}),
            {Ch, Chs2} = alloc(Chs),
            From ! {?MODULE, Ch},
            Deb3 = sys:handle_debug(Deb2, fun write_debug/3,
                                    ?MODULE, {out, {?MODULE, Ch}, From}),
            loop(Chs2, Parent, Deb3);
        {free, Ch} ->
            Deb2 = sys:handle_debug(Deb, fun write_debug/3,
                                    ?MODULE, {in, {free, Ch}}),
            Chs2 = free(Ch, Chs),
            loop(Chs2, Parent, Deb2);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent,
                                  ?MODULE, Deb, Chs)
    end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _Chs) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

channels(NumCh) ->
    {_Allocated=[], _Free=lists:seq(1,NumCh)}.

alloc({_, []}) ->
    {error, "no channels available"};
alloc({Allocated, [H|T]}) ->
    {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free}=Channels) ->
    case lists:member(Ch, Alloc) of
        true ->
            {lists:delete(Ch, Alloc), [Ch|Free]};
        false ->
            Channels
    end.
