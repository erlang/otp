%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(log_mf_h).
-moduledoc """
An event handler that logs events to disk.

This module is a `gen_event` handler module that can be installed in any
`gen_event` process. It logs onto disk all events that are sent to an event
manager. Each event is written as a binary, which makes the logging very fast.
However, a tool such as the Report Browser (`m:rb`) must be used to read the
files. The events are written to multiple files. When all files have been used,
the first one is reused and overwritten. The directory location, the number of
files, and the size of each file are configurable. The directory will include
one file called `index`, and report files `1, 2, ...`.

## See Also

`m:gen_event`, `m:rb`
""".

-behaviour(gen_event).

-export([init/3, init/4]).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-export([handle_call/2, code_change/3]). 

-export_type([args/0]).

%%-----------------------------------------------------------------

-type b()    :: non_neg_integer().
-type f()    :: 1..255.
-type pred() :: fun((term()) -> boolean()).

%%-----------------------------------------------------------------

-record(state, {dir    :: file:filename(),
		maxB   :: b(),
		maxF   :: f(),
		curB   :: b(),
		curF   :: f(),
		cur_fd :: file:fd(),
		index = [],  %% Seems unused - take out??
		pred   :: pred()}).

%%%-----------------------------------------------------------------
%%% This module implements an event handler that writes events
%%% to multiple files (configurable).
%%%-----------------------------------------------------------------
%% Func: init/3, init/4
%% Args: Dir  = string()
%%       MaxB = integer() 
%%       MaxF = byte()
%%       Pred = fun(Event) -> boolean()
%% Purpose: An event handler.  Writes binary events
%%          to files in the directory Dir.  Each file is called
%%          1, 2, 3, ..., MaxF.  Writes MaxB bytes on each file.
%%          Creates a file called 'index' in the Dir.
%%          This file contains the last written FileName.
%%          On startup, this file is read, and the next available
%%          filename is used as first logfile.
%%          Each event is filtered with the predicate function Pred.
%%          Reports can be browsed with Report Browser Tool (rb).
%% Returns: Args = term()
%%          The Args term should be used in a call to
%%            gen_event:add_handler(EventMgr, log_mf_h, Args)
%%              EventMgr = pid() | atom().
%%-----------------------------------------------------------------

-doc "Term to be sent to `gen_event:add_handler/3`.".
-opaque args() :: {file:filename(), b(), f(), pred()}.


-doc(#{equiv => init/4}).
-spec init(Dir, MaxBytes, MaxFiles) -> Args when
      Dir :: file:filename(),
      MaxBytes :: non_neg_integer(), % b()
      MaxFiles :: 1..255, % f()
      Args :: args().

init(Dir, MaxB, MaxF) -> init(Dir, MaxB, MaxF, fun(_) -> true end).

-doc """
Initiates the event handler. Returns `Args`, which is to be used in a call to
`gen_event:add_handler(EventMgr, log_mf_h, Args)`.

`Dir` specifies which directory to use for the log files. `MaxBytes` specifies
the size of each individual file. `MaxFiles` specifies how many files are used.
`Pred` is a predicate function used to filter the events. If no predicate
function is specified, all events are logged.
""".
-spec init(Dir, MaxBytes, MaxFiles, Pred) -> Args when
      Dir :: file:filename(),
      MaxBytes :: non_neg_integer(), % b()
      MaxFiles :: 1..255, % f()
      Pred :: fun((Event :: term()) -> boolean()), % pred()
      Args :: args().

init(Dir, MaxB, MaxF, Pred) -> {Dir, MaxB, MaxF, Pred}.

%%-----------------------------------------------------------------
%% Call-back functions from gen_event
%%-----------------------------------------------------------------

-doc false.
-spec init({file:filename(), non_neg_integer(), f(), pred()}) -> {'ok', #state{}} | {'error', term()}.

init({Dir, MaxB, MaxF, Pred}) when is_integer(MaxF), MaxF > 0, MaxF < 256 -> 
    First = 
	case read_index_file(Dir) of
	    {ok, LastWritten} -> inc(LastWritten, MaxF);
	    _ -> 1
	end,
    case catch file_open(Dir, First) of
	{ok, Fd} ->
	    {ok, #state{dir = Dir, maxB = MaxB, maxF = MaxF, pred = Pred,
			curF = First, cur_fd = Fd, curB = 0}};
	Error -> Error
    end.

%%-----------------------------------------------------------------
%% The handle_event/2 function may crash!  In this case, this
%% handler is removed by gen_event from the event handlers.
%% Fails: 'file_open' if file:open failed for a log file.
%%        'write_index_file' if file:write_file failed for the
%%            index file.
%%        {file_exit, Reason} if the current Fd crashes. 
%%-----------------------------------------------------------------

-doc false.
-spec handle_event(term(), #state{}) -> {'ok', #state{}}.

handle_event(Event, State) ->
    #state{curB = CurB, maxB = MaxB, curF = CurF, maxF = MaxF,
	   dir = Dir, cur_fd = CurFd, pred = Pred} = State,
    case catch Pred(Event) of
	true -> 
	    Bin = term_to_binary(tag_event(Event)),
	    Size = byte_size(Bin),
	    NewState =
		if
		    CurB + Size < MaxB -> State;
		    true ->
			ok = file:close(CurFd),
			NewF = inc(CurF, MaxF),
			{ok, NewFd} = file_open(Dir, NewF),
			State#state{cur_fd = NewFd, curF = NewF, curB = 0}
		end,
	    [Hi,Lo] = put_int16(Size),
            case file:write(NewState#state.cur_fd, [Hi, Lo, Bin]) of
                ok ->
                    ok;
                {error, Reason} ->
                    exit({file_exit, Reason})
            end,
	    {ok, NewState#state{curB = NewState#state.curB + Size + 2}};
	_ ->
	    {ok, State}
    end.

-doc false.
-spec handle_info(term(), #state{}) -> {'ok', #state{}}.

handle_info({emulator, GL, Chars}, State) ->
    handle_event({emulator, GL, Chars}, State);
handle_info(_, State) ->
    {ok, State}.

-doc false.
-spec terminate(term(), #state{}) -> #state{}.

terminate(_, State) ->
    ok = file:close(State#state.cur_fd),
    State.

-doc false.
-spec handle_call('null', #state{}) -> {'ok', 'null', #state{}}.

handle_call(null, State) ->
    {ok, null, State}.

-doc false.
-spec code_change(term(), #state{}, term()) -> {'ok', #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Misc local functions
%%-----------------------------------------------------------------

file_open(Dir, FileNo) ->
    case file:open(Dir ++ [$/ | integer_to_list(FileNo)], [raw, write]) of
	{ok, Fd} ->
	    write_index_file(Dir, FileNo),
	    {ok, Fd};
	_ -> 
	    exit(file_open)
    end.

put_int16(I) ->
    [((I band 16#ff00) bsr 8),I band 16#ff].

tag_event(Event) ->
    {erlang:localtime(), Event}.

read_index_file(Dir) ->
    case file:open(Dir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    Res = case catch file:read(Fd, 1) of
		      {ok, [Index]} -> {ok, Index};
		      _ -> error
		  end,
	    ok = file:close(Fd),
	    Res;
	_ -> error
    end.

%%-----------------------------------------------------------------
%% Write the index file.  This file contains one binary with
%% the last used filename (an integer).
%% Write a temporary file and rename it in order to make the update
%% atomic.
%%-----------------------------------------------------------------

write_index_file(Dir, Index) ->
    File = Dir ++ "/index",
    TmpFile = File ++ ".tmp",
    case file:open(TmpFile, [raw, write]) of
	{ok, Fd} ->
	    ok = file:write(Fd, [Index]),
	    ok = file:close(Fd),
	    ok = file:rename(TmpFile,File),
	    ok;
	_ -> exit(write_index_file)
    end.

inc(N, Max) ->
    if
	N < Max -> N + 1;
	true -> 1
    end.
