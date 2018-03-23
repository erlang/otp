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
%%% File    : tft_file.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 24 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_file).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

-behaviour(tftp).

-export([prepare/6, open/6, read/1, write/2, abort/3]).

%%%-------------------------------------------------------------------
%%% Defines
%%%-------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

-record(initial,
	{filename,
	 is_native_ascii}).

-record(state,
	{access,
	 filename,
	 is_native_ascii,
	 is_network_ascii,
	 root_dir,
	 options,
	 blksize,
	 fd,
	 count,
	 buffer}).

%%-------------------------------------------------------------------
%% prepare(Peer, Access, Filename, Mode, SuggestedOptions, InitialState) -> 
%%    {ok, AcceptedOptions, NewState} | {error, Code, Text}
%%
%% Peer             = {PeerType, PeerHost, PeerPort}
%% PeerType         = inet | inet6
%% PeerHost         = ip_address()
%% PeerPort         = integer()
%% Acess            = read | write
%% Filename         = string()
%% Mode             = string()
%% SuggestedOptions = [{Key, Value}]
%% AcceptedOptions  = [{Key, Value}]
%% Key              = string()
%% Value            = string()
%% InitialState     = [] | [{root_dir, string()}]
%% NewState         = term()
%% Code             = undef | enoent | eacces | enospc |
%%                    badop | eexist | baduser | badopt |
%%                    integer()
%% Text             = string()
%%
%% Prepares open of a file on the client side.
%% 
%% Will be followed by a call to open/4 before any read/write access
%% is performed. The AcceptedOptions will be sent to the server which
%% will reply with those options that it accepts. The options that are
%% accepted by the server will be forwarded to open/4 as SuggestedOptions.
%%
%% No new options may be added, but the ones that are present as
%% SuggestedOptions may be omitted or replaced with new values
%% in the AcceptedOptions.
%%-------------------------------------------------------------------

prepare(_Peer, Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Client side
    case catch handle_options(Access, Filename, Mode, SuggestedOptions, Initial) of
	{ok, Filename2, IsNativeAscii, IsNetworkAscii, AcceptedOptions} ->
	    State = #state{access           = Access,
			   filename         = Filename2,
			   is_native_ascii  = IsNativeAscii,
			   is_network_ascii = IsNetworkAscii,
			   options  	    = AcceptedOptions,
			   blksize  	    = lookup_blksize(AcceptedOptions),
			   count    	    = 0,
			   buffer   	   =  []},
	    {ok, AcceptedOptions, State};
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end.

%% ---------------------------------------------------------
%% open(Peer, Access, Filename, Mode, SuggestedOptions, State) -> 
%%    {ok, AcceptedOptions, NewState} | {error, Code, Text}
%%
%% Peer             = {PeerType, PeerHost, PeerPort}
%% PeerType         = inet | inet6
%% PeerHost         = ip_address()
%% PeerPort         = integer()
%% Acess            = read | write
%% Filename         = string()
%% Mode             = string()
%% SuggestedOptions = [{Key, Value}]
%% AcceptedOptions  = [{Key, Value}]
%% Key              = string()
%% Value            = string()
%% State            = InitialState | #state{}
%% InitialState     = [] | [{root_dir, string()}]
%% NewState         = term()
%% Code             = undef | enoent | eacces  | enospc |
%%                    badop | eexist | baduser | badopt |
%%                    integer()
%% Text             = string()
%%
%% Opens a file for read or write access.
%% 
%% On the client side where the open/4 call has been preceeded by a
%% call to prepare/4, all options must be accepted or rejected.
%% On the server side, where there are no preceeding prepare/4 call,
%% noo new options may be added, but the ones that are present as
%% SuggestedOptions may be omitted or replaced with new values
%% in the AcceptedOptions.
%%-------------------------------------------------------------------

open(Peer, Access, Filename, Mode, SuggestedOptions, Initial) when is_list(Initial) ->
    %% Server side
    case prepare(Peer, Access, Filename, Mode, SuggestedOptions, Initial) of
	{ok, AcceptedOptions, State} ->
	    open(Peer, Access, Filename, Mode, AcceptedOptions, State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(_Peer, Access, Filename, Mode, NegotiatedOptions, State) when is_record(State, state) ->
    %% Both sides
    case catch handle_options(Access, Filename, Mode, NegotiatedOptions, State) of
	{ok, _Filename2, _IsNativeAscii, _IsNetworkAscii, Options} 
	   when Options =:= NegotiatedOptions ->
	    do_open(State);
	{error, {Code, Text}} ->
	    {error, {Code, Text}}
    end;
open(Peer, Access, Filename, Mode, NegotiatedOptions, State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    open(Peer, Access, Filename, Mode, NegotiatedOptions, State2).

do_open(State) when is_record(State, state) ->
    case file:open(State#state.filename, file_options(State)) of
	{ok, Fd} ->
	    {ok, State#state.options, State#state{fd = Fd}};
	{error, Reason} when is_atom(Reason) ->
	    {error, file_error(Reason)}
    end.
	
file_options(State) ->
    case State#state.access of
	read  -> [read, read_ahead, raw, binary];
	write -> [write, delayed_write, raw, binary]
    end.

file_error(Reason) when is_atom(Reason) ->
    Details = file:format_error(Reason),
    case Reason of
	eexist -> {Reason, Details};
	enoent -> {Reason, Details};
	eacces -> {Reason, Details};
	eperm  -> {eacces, Details};
	enospc -> {Reason, Details};
	_      -> {undef,  Details ++ " (" ++ atom_to_list(Reason) ++ ")"}
    end.

%%-------------------------------------------------------------------
%% read(State) ->
%%   {more, Bin, NewState} | {last, Bin, FileSize} | {error, {Code, Text}}
%%
%% State    = term()
%% NewState = term()
%% Bin      = binary()
%% FileSize = integer()
%% Code     = undef | enoent | eacces  | enospc |
%%            badop | eexist | baduser | badopt |
%%            integer()
%% Text     = string()
%%
%% Reads a chunk from the file
%% 
%% The file is automatically closed when the last chunk is read.
%%-------------------------------------------------------------------

read(#state{access = read} = State) ->
    BlkSize = State#state.blksize,
    case file:read(State#state.fd, BlkSize) of
	{ok, Bin} when is_binary(Bin), size(Bin) =:= BlkSize ->
	    Count = State#state.count + size(Bin),
	    {more, Bin, State#state{count = Count}};
	{ok, Bin} when is_binary(Bin), size(Bin) < BlkSize ->
	    _ = file:close(State#state.fd),
	    Count = State#state.count + size(Bin),
	    {last, Bin, Count};
	eof ->
	    {last, <<>>, State#state.count};
	{error, Reason} ->
	    _ = file:close(State#state.fd),
	    {error, file_error(Reason)}
    end;
read(State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    read(State2).

%%-------------------------------------------------------------------
%% write(Bin, State) ->
%%   {more, NewState} | {last, FileSize} | {error, {Code, Text}}
%%
%% State    = term()
%% NewState = term()
%% Bin      = binary()
%% FileSize = integer()
%% Code     = undef | enoent | eacces  | enospc |
%%            badop | eexist | baduser | badopt |
%%            integer()
%% Text     = string()
%%
%% Writes a chunk to the file
%%
%% The file is automatically closed when the last chunk is written
%%-------------------------------------------------------------------

write(Bin, #state{access = write} = State) when is_binary(Bin) ->
    Size = size(Bin),
    BlkSize = State#state.blksize,
    case file:write(State#state.fd, Bin) of
	ok when Size =:= BlkSize->
	    Count = State#state.count + Size,
	    {more, State#state{count = Count}};
	ok when Size < BlkSize->
	    _ = file:close(State#state.fd),
	    Count = State#state.count + Size,
	    {last, Count};
	{error, Reason}  ->
	    _ = file:close(State#state.fd),
	    _ = file:delete(State#state.filename),
	    {error, file_error(Reason)}
    end;
write(Bin, State) ->
    %% Handle upgrade from old releases. Please, remove this clause in next release.
    State2 = upgrade_state(State),
    write(Bin, State2).

%%-------------------------------------------------------------------
%% abort(Code, Text, State) -> ok
%% 
%% State    = term()
%% Code     = undef  | enoent | eacces  | enospc |
%%            badop  | eexist | baduser | badopt |
%%            badblk | integer()
%% Text     = string()
%%
%% Aborts the file transfer
%%-------------------------------------------------------------------

abort(_Code, _Text, #state{fd = Fd, access = Access} = State) ->
    _ = file:close(Fd),
    case Access of
	write ->
	    ok = file:delete(State#state.filename);
	read ->
	    ok
    end.

%%-------------------------------------------------------------------
%% Process options
%%-------------------------------------------------------------------

handle_options(Access, Filename, Mode, Options, Initial) ->
    I = #initial{filename = Filename, is_native_ascii = is_native_ascii()},
    {Filename2, IsNativeAscii} = handle_initial(Initial, I),
    IsNetworkAscii = handle_mode(Mode, IsNativeAscii),
    Options2 = do_handle_options(Access, Filename2, Options),
    {ok, Filename2, IsNativeAscii, IsNetworkAscii, Options2}.

handle_mode(Mode, IsNativeAscii) ->
    case Mode of
	"netascii" when IsNativeAscii =:= true -> true;
	"octet" -> false;
	_ -> throw({error, {badop, "Illegal mode " ++ Mode}})
    end.

handle_initial([{root_dir, Dir} | Initial], I) ->
    case catch filename_join(Dir, I#initial.filename) of
	{'EXIT', _} ->
	    throw({error, {badop, "Internal error. root_dir is not a string"}});
	Filename2 ->
	    handle_initial(Initial, I#initial{filename = Filename2})
    end;
handle_initial([{native_ascii, Bool} | Initial], I) ->
    case Bool of
	true  -> handle_initial(Initial, I#initial{is_native_ascii = true});
	false -> handle_initial(Initial, I#initial{is_native_ascii = false})
    end;
handle_initial([], I) when is_record(I, initial) ->
    {I#initial.filename, I#initial.is_native_ascii};
handle_initial(State, _) when is_record(State, state) ->
    {State#state.filename, State#state.is_native_ascii}.

filename_join(Dir, Filename) ->
    case filename:pathtype(Filename) of
	absolute ->
	    [_ | RelFilename] = filename:split(Filename),
	    filename:join([Dir, RelFilename]);
	_ ->
	    filename:join([Dir, Filename])
    end.

do_handle_options(Access, Filename, [{Key, Val} | T]) ->
    case Key of
	"tsize" ->
	    case Access of
		read when Val =:= "0" ->
		    case file:read_file_info(Filename) of
			{ok, FI} ->
			    Tsize = integer_to_list(FI#file_info.size),
			    [{Key, Tsize} | do_handle_options(Access, Filename, T)];
			{error, _} ->
			    do_handle_options(Access, Filename, T)
		    end;
		_ ->
		    handle_integer(Access, Filename, Key, Val, T, 0, infinity)
	    end;
	"blksize" ->
	    handle_integer(Access, Filename, Key, Val, T, 8, 65464);
	"timeout" ->
	    handle_integer(Access, Filename, Key, Val, T, 1, 255);
	_ ->
	    do_handle_options(Access, Filename, T)
    end;
do_handle_options(_Access, _Filename, []) ->
    [].


handle_integer(Access, Filename, Key, Val, Options, Min, Max) ->
    case catch list_to_integer(Val) of
	{'EXIT', _} ->
	    do_handle_options(Access, Filename, Options);
	Int when Int >= Min, Int =< Max ->
	    [{Key, Val} | do_handle_options(Access, Filename, Options)];
	Int when Int >= Min, Max =:= infinity ->
	    [{Key, Val} | do_handle_options(Access, Filename, Options)];
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

is_native_ascii() ->
    case os:type() of
	{win32, _} -> true;
	_          -> false
    end.

%% Handle upgrade from old releases. Please, remove this function in next release.
upgrade_state({state, Access, Filename, RootDir, Options, BlkSize, Fd, Count, Buffer}) ->
    {state, Access, Filename, false, false, RootDir, Options, BlkSize, Fd, Count, Buffer}.
