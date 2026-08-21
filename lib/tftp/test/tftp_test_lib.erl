%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2026. All Rights Reserved.
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

-module(tftp_test_lib).

-compile([export_all, nowarn_export_all]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Infrastructure for test suite
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_catch(Fun, Module, Line, FunctionName) ->
    try Fun() of
        Result ->
            log("~p~n", [Result],
                Module, Line, FunctionName),
            {Result}
    catch Class : Reason : Stacktrace ->
            CLASS = 'CLASS'(Class),
            log("~w : ~w~n    ~p~n", [CLASS, Reason, Stacktrace],
                Module, Line, FunctionName),
            {CLASS, Reason}
    end.

'CLASS'(Class) ->
    case Class of
        exit  -> 'EXIT';
        error -> 'ERROR';
        throw -> 'THROW'
    end.

log(Format, Args, Mod, Line, FunctionName) ->
    io:format(user, "~w:~w:~w: " ++ Format, [Mod, FunctionName, Line] ++ Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(generic_state, {state, prepare, open, read, write, abort}).

prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    State   = lookup_option(state,   mandatory, Initial),
    Prepare = lookup_option(prepare, mandatory, Initial),
    Open    = lookup_option(open,    mandatory, Initial),
    Read    = lookup_option(read,    mandatory, Initial),
    Write   = lookup_option(write,   mandatory, Initial),
    Abort   = lookup_option(abort,   mandatory, Initial),
    case Prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, State) of
	{ok, AcceptedOptions, NewState} ->
	    {ok,
	     AcceptedOptions,
	     #generic_state{state   = NewState,
			    prepare = Prepare,
			    open    = Open,
			    read    = Read,
			    write   = Write,
			    abort   = Abort}};
	Other ->
	    Other
    end.

open(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    case prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, Initial) of
 	{ok, SuggestedOptions2, GenericState} ->
	    open(Peer, Access, LocalFilename, Mode, SuggestedOptions2, GenericState);
	Other ->
	    Other
    end;
open(Peer, Access, LocalFilename, Mode, SuggestedOptions, #generic_state{state = State, open = Open} = GenericState) ->
    case Open(Peer, Access, LocalFilename, Mode, SuggestedOptions, State) of
	{ok, SuggestedOptions2, NewState} ->
	    {ok, SuggestedOptions2, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

read(#generic_state{state = State, read = Read} = GenericState) ->
    case Read(State) of
	{more, DataBlock, NewState} ->
	    {more, DataBlock, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

write(DataBlock, #generic_state{state = State, write = Write} = GenericState) ->
    case Write(DataBlock, State) of
	{more, NewState} ->
	    {more, GenericState#generic_state{state = NewState}};
	Other ->
	    Other
    end.

abort(Code, Text, #generic_state{state = State, abort = Abort}) ->
    Abort(Code, Text, State).

lookup_option(Key, Default, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

