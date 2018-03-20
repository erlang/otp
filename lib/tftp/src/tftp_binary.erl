%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%%

%%%-------------------------------------------------------------------
%%% File    : tft_binary.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 24 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_binary).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

-behaviour(tftp).

-export([prepare/6, open/6, read/1, write/2, abort/3]).

-record(read_state,  {options, blksize, bin,  is_native_ascii, is_network_ascii, count}).
-record(write_state, {options, blksize, list, is_native_ascii, is_network_ascii}).

%%-------------------------------------------------------------------
%% Prepare
%%-------------------------------------------------------------------

prepare(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Client side
    IsNativeAscii = is_native_ascii(Initial),
    case catch handle_options(Access, Filename, Mode, SuggestedOptions, IsNativeAscii) of
	{ok, IsNetworkAscii, AcceptedOptions} when Access =:= read, is_binary(Filename) ->
	    State = #read_state{options  	 = AcceptedOptions,
				blksize  	 = lookup_blksize(AcceptedOptions),
				bin      	 = Filename,
				is_network_ascii = IsNetworkAscii,
			        count            = size(Filename),
				is_native_ascii  = IsNativeAscii},
	    {ok, AcceptedOptions, State};
	{ok, IsNetworkAscii, AcceptedOptions} when Access =:= write, Filename =:= binary ->
	    State = #write_state{options  	  = AcceptedOptions,
				 blksize  	  = lookup_blksize(AcceptedOptions),
				 list     	  = [],
				 is_network_ascii = IsNetworkAscii,
				 is_native_ascii  = IsNativeAscii},
	    {ok, AcceptedOptions, State};
	{ok, _, _} ->
	    {error, {undef, "Illegal callback usage. Mode and filename is incompatible."}};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
prepare(_Peer, _Access, _Bin, _Mode, _SuggestedOptions, _Initial) ->
    {error, {undef, "Illegal callback options."}}.

%%-------------------------------------------------------------------
%% Open
%%-------------------------------------------------------------------

open(Peer, Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Server side
    case prepare(Peer, Access, Filename, Mode, SuggestedOptions, Initial) of
	{ok, AcceptedOptions, State} ->
	    open(Peer, Access, Filename, Mode, AcceptedOptions, State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(_Peer, Access, Filename, Mode, NegotiatedOptions, State) when is_record(State, read_state) ->
    %% Both sides
    case catch handle_options(Access, Filename, Mode, NegotiatedOptions, State#read_state.is_native_ascii) of
	{ok, IsNetworkAscii, Options}
	when Options =:= NegotiatedOptions,
	     IsNetworkAscii =:= State#read_state.is_network_ascii ->
	    {ok, NegotiatedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(_Peer, Access, Filename, Mode, NegotiatedOptions, State) when is_record(State, write_state) ->
    %% Both sides
    case catch handle_options(Access, Filename, Mode, NegotiatedOptions, State#write_state.is_native_ascii) of
	{ok, IsNetworkAscii, Options}
	when Options =:= NegotiatedOptions,
	     IsNetworkAscii =:= State#write_state.is_network_ascii ->
	    {ok, NegotiatedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(Peer, Access, Filename, Mode, NegotiatedOptions, State) -> 
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    open(Peer, Access, Filename, Mode, NegotiatedOptions, State2).

%%-------------------------------------------------------------------
%% Read
%%-------------------------------------------------------------------

read(#read_state{bin = Bin} = State) when is_binary(Bin) ->
    BlkSize = State#read_state.blksize,
    if
	size(Bin) >= BlkSize ->
	    <<Block:BlkSize/binary, Bin2/binary>> = Bin,
	    State2 = State#read_state{bin = Bin2},
	    {more, Block, State2};
	size(Bin) < BlkSize ->
	    {last, Bin, State#read_state.count}
    end;
read(State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    read(State2).

%%-------------------------------------------------------------------
%% Write
%%-------------------------------------------------------------------

write(Bin, #write_state{list = List} = State) when is_binary(Bin), is_list(List) ->
    Size = size(Bin),
    BlkSize = State#write_state.blksize,
    if
	Size =:= BlkSize ->
	    {more, State#write_state{list = [Bin | List]}};
	Size < BlkSize ->
	    Bin2 = list_to_binary(lists:reverse([Bin | List])),
	    {last, Bin2}
    end;
write(Bin, State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    write(Bin, State2).

%%-------------------------------------------------------------------
%% Abort
%%-------------------------------------------------------------------

abort(_Code, _Text, #read_state{bin = Bin} = State) 
  when is_record(State, read_state), is_binary(Bin) ->
    ok;
abort(_Code, _Text, #write_state{list = List} = State)
  when is_record(State, write_state), is_list(List) ->
    ok;
abort(Code, Text, State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    abort(Code, Text, State2).

%%-------------------------------------------------------------------
%% Process options
%%-------------------------------------------------------------------

handle_options(Access, Bin, Mode, Options, IsNativeAscii) ->
    IsNetworkAscii = handle_mode(Mode, IsNativeAscii),
    Options2 = do_handle_options(Access, Bin, Options),
    {ok, IsNetworkAscii, Options2}.

handle_mode(Mode, IsNativeAscii) ->
    case Mode of
	"netascii" when IsNativeAscii =:= true -> true;
	"octet" -> false;
	_ -> throw({error, {badop, "Illegal mode " ++ Mode}})
    end.

do_handle_options(Access, Bin, [{Key, Val} | T]) ->
    case Key of
	"tsize" ->
	    case Access of
		read when Val =:= "0", is_binary(Bin) ->
		    Tsize = integer_to_list(size(Bin)),
		    [{Key, Tsize} | do_handle_options(Access, Bin, T)];
		_ ->
		    handle_integer(Access, Bin, Key, Val, T, 0, infinity)
	    end;
	"blksize" ->
	    handle_integer(Access, Bin, Key, Val, T, 8, 65464);
	"timeout" ->
	    handle_integer(Access, Bin, Key, Val, T, 1, 255);
	_ ->
	    do_handle_options(Access, Bin, T)
    end;
do_handle_options(_Access, _Bin, []) ->
    [].


handle_integer(Access, Bin, Key, Val, Options, Min, Max) ->
    case catch list_to_integer(Val) of
	{'EXIT', _} ->
	    do_handle_options(Access, Bin, Options);
	Int when Int >= Min, Int =< Max ->
	    [{Key, Val} | do_handle_options(Access, Bin, Options)];
	Int when Int >= Min, Max =:= infinity ->
	    [{Key, Val} | do_handle_options(Access, Bin, Options)];
	_Int ->
	    throw({error, {badopt, "Illegal " ++ Key ++ " value " ++ Val}})
    end.

lookup_blksize(Options) ->
    case lists:keysearch("blksize", 1, Options) of
	{value, {_, Val}} ->
	    list_to_integer(Val);
	false ->
	    512
    end.

is_native_ascii([]) ->
    is_native_ascii();
is_native_ascii([{native_ascii, Bool}]) ->
    case Bool of
	true  -> true;
	false -> false
    end.

is_native_ascii() ->
    case os:type() of
	{win32, _} -> true;
	_          -> false
    end.
    
%% Handle upgrade from old releases. Please, remove this function in next release.
upgrade_state({read_state,  Options, Blksize, Bin, IsNetworkAscii, Count}) ->
    {read_state,  Options, Blksize, Bin, false, IsNetworkAscii, Count};
upgrade_state({write_state, Options, Blksize, List, IsNetworkAscii}) ->
    {write_state, Options, Blksize, List, false, IsNetworkAscii}.
