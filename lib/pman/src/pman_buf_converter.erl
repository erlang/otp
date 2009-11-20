%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose : The purpouse of the converter process is to take
%%           care of the raw data that is received by the tracing
%%           process (a pman_shell process) and pass it on to
%%           the buffer process in chunks that can be handled.
%%
%%           This module is a part of the buffering system, and
%%           should not be used except through the API defined
%%           in the pman_buf module.
%%
%%----------------------------------------------------------------------

-module(pman_buf_converter).

%%-compile(export_all).
-export([init/2]).

-include("pman_buf.hrl").


%% ---------------------------------------------------------------
%% Starts the process which received the raw data from the debugger, 
%% cuts and forwards it to the buffer in smaller chunks. High priority
%% to avoid large message queues waiting to be processed.

init(Buffer_Pid, FileName) ->
    process_flag(priority, max),
    converter_loop(Buffer_Pid,[],0,true,[], FileName).
    
converter_loop(Buffer_Pid,Raw,Size,State,Last, FileName) ->
    receive
	{file,Shell} ->
	    case init_file(lists:append(Raw,Last),
			   FileName,
			   Shell,
			   Buffer_Pid) of
		true  -> converter_loop(Buffer_Pid,
					[to_buffer],
					1,
					State,
					[],
					FileName);
		false -> converter_loop(Buffer_Pid,
					Raw,
					Size,
					State,
					Last,
					FileName)
	    end;
	{raw,Trace} ->
	    {New_Raw,New_Size,New_State,New_Last} =
		converter_data(Trace, Buffer_Pid, Raw, Size, State, Last),
	    converter_loop(Buffer_Pid,
			   New_Raw,
			   New_Size,
			   New_State,
			   New_Last,
			   FileName);
        {buffer,accept} when Raw /= [] ->
	    {Length,Rest,Print} = pman_buf_utils:split(Raw,?PRINT_LEN,0,[]),
	    Buffer_Pid!{raw,Print,Length},
	    converter_loop(Buffer_Pid,Rest,Size-Length,false,Last,FileName);
	{buffer,accept} when Last /= [] ->
	    {New_Raw,New_Size,New_State,New_Last} =
		converter_data(Last,Buffer_Pid,Raw,Size,true,[]),
	    converter_loop(Buffer_Pid,
			   New_Raw,
			   New_Size,
			   New_State,
			   New_Last,
			   FileName);
	{buffer,accept} ->
	    converter_loop(Buffer_Pid,Raw,Size,true,Last, FileName);  
	{clear,Str} ->
	    Buffer_Pid!{clear,Str},
	    converter_loop(Buffer_Pid,[],0,State,Last,FileName)
    end.

converter_data(Trace,Buffer_Pid,Raw,Size,State,Last) ->
    if
	?BUFF_SIZE - Size > 0 ->
	    {Len,Rest,New_Trace} = pman_buf_utils:split(Trace,
							?BUFF_SIZE-Size,
							0,[]),
	    {New_Raw,New_Last} =
		case Rest of
		    [] ->
			{lists:append(Raw,New_Trace),Last};
		    [_|_] ->
			case Last of
			    [] ->
				{lists:append(Raw,New_Trace),Rest};
			    _ ->{lists:concat([Raw,New_Trace,[cut_buffer]]),
				 Rest}
			end
		end,
	    case State of true ->
		    {Length,Cut_Raw,Print} = pman_buf_utils:split(New_Raw,
								  ?PRINT_LEN,
								  0,[]),
		    Buffer_Pid!{raw,Print,Length},
		    {Cut_Raw,Size-Length,false,New_Last};
		_ ->
		    {New_Raw,Size+Len,false,New_Last}
	    end;
	true ->
	    {Raw,Size,State,Trace}
    end.


%% ---------------------------------------------------------------
%% Initializes the environment for saving the trace to file. The 
%% actual saving is taken care of by the buffer process.

init_file(Raw,FileName, Name,Buffer_Pid) ->
     case open_file(FileName, Name) of
	 {false,T} ->
	     pman_win:msg_win(T),
	     false;
	 {File,T} ->
	     Buffer_Pid!{converter,file},
	     pman_win:dialog_window(gs:start(),T),
	     save_loop_init(File,Raw)
     end.

open_file(FileName, _Shell) ->
%%    L = "pman_trace." ++ Shell,
    case file:open(FileName, [read,write]) of
	{error, _} -> 
	    Str = "ERROR: Could not create_file\n" ++ FileName,
	    {false,Str};
	{ok,File} ->
	    file:position(File, {eof, 0}),
	    Str1 = " Appending trace log to file\n" ++ FileName,
	    {File,Str1}
    end.


save_loop_init(Fd,Raw) ->
    {Date, Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    io:format(Fd,"%%% ~n",[]),
    io:format(Fd,"%%% Trace output~n",[]),
    io:format(Fd,"%%% Started at ~4p-~2p-~2p ~2p:~2p:~2p~n",
	      [Year, Month, Day,
	       Hour, Minute, Second
	      ]),
    io:format(Fd,"%%% ~n~n",[]),

    Print = lists:map(fun(X) -> pman_buf_utils:textformat(X) end, Raw),
    receive
	{buffer,Text} when is_list(Text) ->
	    io:format(Fd,Text,[]),
	    io:format(Fd,Print,[]),
	    save_loop(Fd)
    end.

save_loop(Fd) ->
    receive
	{raw,Raw} ->
	    Print = lists:map(fun(X) -> pman_buf_utils:textformat(X) end, Raw),
	    io:format(Fd,Print,[]),
	    save_loop(Fd);
	buffer -> true
    end.





