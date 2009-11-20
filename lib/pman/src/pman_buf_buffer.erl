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
%%%----------------------------------------------------------------------
%%% Purpose : The purpouse of the buffer process is to take
%%%           care of the data that is received by the converter
%%%           process and pass it on to the printer process in chunks
%%%	      that can be handled.
%%%
%%%           This module is a part of the buffering system, and
%%%           should not be used except through the API defined
%%%           in the pman_buf module.
%%%
%%%----------------------------------------------------------------------

-module(pman_buf_buffer).

%%-compile(export_all).
-export([init/1]).

-include("pman_buf.hrl").



%%
%% Initialization function for the buffer process.
%% To be started with spawn from the calling process.
%%

init(Editor) ->
    Printer_pid = spawn_link(pman_buf_printer,init,[Editor,self()]),
    receive
	{converter_pid,Pid} ->
	    Pid!{buffer,accept},
	    buffer_loop([],0,0,Printer_pid,Pid)
    end.



%%
%% Receive loop for the buffer process.
%%

buffer_loop(Buffer,Size,Acc,Printer,Converter) ->
    receive
	{save_buffer,Name} ->
	    Printer!{save_buffer,Name},
	    buffer_loop(Buffer,Size,Acc,Printer,Converter);
	{raw,Raw,Length} ->   %%output to editor
	    New_Size = Size + Length,
	    if New_Size < ?BUFF_SIZE ->
		    Converter!{buffer,accept};
	       true -> ok
	    end,
	    Print = lists:map(fun(X) -> pman_buf_utils:textformat(X) end, Raw),
	    New_Buff = lists:append(Buffer,Print),
	    buffer_loop(New_Buff,New_Size,Acc,Printer,Converter);
	{clear,Text,N_Converter} ->
	    Converter!{buffer,accept},
	    Printer!clear,
	    buffer_loop([Text],1,1,Printer,N_Converter);
	{printer,send} when Buffer /= [] ->
	     if
		 Acc  > ?EDITOR_MAX ->
		     Printer!clear,
		     Printer !{buffer,"Cleared Buffer due to Size\n\n"},
		     buffer_loop(Buffer,Size,1,Printer,Converter);
		 true ->
		     {Length,Rest,Print} = pman_buf_utils:split(Buffer,
								?PRINT_LEN,
								0,
								[]),
		     Printer ! {buffer,Print},
		     New_Size = Size - Length,
		     if New_Size < ?BUFF_SIZE ->
			     Converter!{buffer,accept};
			true -> ok
		     end,
		     buffer_loop(Rest,New_Size,Acc+Length,Printer,Converter)
	     end;
	{converter,file} ->
	    Converter!{buffer,Buffer},
	    self()!{raw,[to_file],1},
	    buffer_loop([],0,Acc,Printer,Converter)
    end.


