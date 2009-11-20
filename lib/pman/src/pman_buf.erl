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
%%% Purpose : This module is the exported interface to the buffering mechanism
%%%           used in PMAN to prevent the trace output to congest
%%%           the system.
%%%
%%%           This module depends on the modules (direct or indirectly):
%%%               pman_buf.hrl
%%%               pman_buf_utils.erl
%%%               pman_buf_buffer.erl
%%%               pman_buf_converter.erl
%%%               pman_buf_printer.erl
%%%
%%%----------------------------------------------------------------------

-module(pman_buf).

%%-compile(export_all).
-export([start/2,clear/3]).


-include("pman_buf.hrl").


%% The buffering mechanism consists of three processes that 
%% work in a chain to prevent the process monitor from congesting
%% output mechanism.
%% 
%% Messages are buffered in the CONVERTER process before they are sent to
%% to the BUFFER process where they are formatted before they are finally
%% sent to either a file or the PRINTER process. The printer process
%% outputs the messages in the graphical user interface.
%%
%%
%%
%%     	   -->   CONVERTER  -->   BUFFER    --> PRINTER --> gui
%%                                 |
%%                                 |
%%                                 |
%%                                 V
%%
%%                                file
%%





%% ----------------------------------------------------------------
%% The amount of data produced by a trace message may be large, and
%% cause the run time system to run out of memory. To avoid this,
%% the task of storing, cutting buffers, formating data and printing
%% it is performed by three processes: The buffer, the converter and 
%% the printer.
%%
%% The converter accepts the raw data, a list
%% of {trace,Msg} tuples. Having max priority, it assures that the
%% amount of raw data stored never excedes ?BUFF_SIZE messages.
%% (With the exception of the last batch received, which assures that
%% the last trace message printed is never a buffer cut message.)
%% Whenever there is space available in the buffer process, (The
%% Buffer process stores max. ?BUFF_SIZE converted messages),
%% the buffer asks for more unconverted messages, and ?PRINT_LEN messages 
%% are sent. They are converted by the buffer, and added to the list
%% of messages to be sent. 

%% The printer process requests formatted messages from the buffer,
%% and in chuncs of ?MAX_OUTPUT sends them to the buffer. If traces
%% are to be dumped on file, due to the max priority, such is handled
%% in the converter, and buffers are not cut.
%%


%% ---------------------------------------------------------------
%% Initializes the buffering mechanism, which consist of three
%% processes, each involved with a phase of the formattation and
%% output of data to the process windows.

start(Editor, FileName) ->
    Buffer_Pid = spawn_link(pman_buf_buffer,init,[Editor]),
    Converter_Pid =
	spawn_link(pman_buf_converter,init,[Buffer_Pid, FileName]),
    Buffer_Pid!{converter_pid, Converter_Pid},
    #buffer{converter=Converter_Pid,buffer=Buffer_Pid}.



%% ---------------------------------------------------------------
%% Kills the converter and the clears the buffer with formated data
%% starting a new converter.

clear(Buff,String, FileName) ->
    exit(Buff#buffer.converter,win_killed),
    Converter_Pid=spawn_link(pman_buf_converter,init,[Buff#buffer.buffer,
						      FileName]),
    Buff#buffer.buffer!{clear,String,Converter_Pid },
    Buff#buffer{converter = Converter_Pid}.



