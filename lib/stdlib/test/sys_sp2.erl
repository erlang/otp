%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(sys_sp2).
-export([start_link/1, stop/0]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
         write_debug/3]).

%% Implements the ch4 example from the Design Principles doc.  Same as
%% sys_sp1 except this module does not export system_get_state/1 or
%% system_replace_state/2

start_link(NumCh) ->
    proc_lib:start_link(?MODULE, init, [[self(),NumCh]]).

stop() ->
    ?MODULE ! stop,
    ok.

alloc() ->
    ?MODULE ! {self(), alloc},
    receive
        {?MODULE, Res} ->
            Res
    end.

free(Ch) ->
    ?MODULE ! {free, Ch},
    ok.

%% can't use 2-tuple for state here as we do in sys_sp1, since the 2-tuple
%% is not compatible with the backward compatibility handling for
%% sys:get_state in sys.erl
-record(state, {alloc,free}).

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
                                  ?MODULE, Deb, Chs);
        stop ->
            sys:handle_debug(Deb, fun write_debug/3,
                             ?MODULE, {in, stop}),
            ok
    end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _Chs) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

channels(NumCh) ->
    #state{alloc=[], free=lists:seq(1,NumCh)}.

alloc(#state{free=[]}=Channels) ->
    {{error, "no channels available"}, Channels};
alloc(#state{alloc=Allocated, free=[H|T]}) ->
    {H, #state{alloc=[H|Allocated], free=T}}.

free(Ch, #state{alloc=Alloc, free=Free}=Channels) ->
    case lists:member(Ch, Alloc) of
        true ->
            #state{alloc=lists:delete(Ch, Alloc), free=[Ch|Free]};
        false ->
            Channels
    end.
