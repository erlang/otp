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

-module(pman_buf_printer).

%%-compile(export_all).
-export([init/2]).


-include("pman_buf.hrl").

%% ---------------------------------------------------------------
%% Starts the part of the buffer which regulates the flow of data to
%% be printed in the pid editors


init(Editor,Buffer_pid) ->
    Buffer_pid!{printer,send},
    printer_loop(Editor,Buffer_pid).
    
printer_loop(Editor,Buffer_pid)->
    receive
	{save_buffer,Name} ->
	    gs:config(Editor,{save,Name}),
	    TT = "Buffer saved in file\n" ++ Name,
	    pman_win:dialog_window(gs:start(),TT),
	    printer_loop(Editor,Buffer_pid);
	{buffer,Trace} ->
	    case lists:flatlength(Trace) of
		Len when Len > ?MAX_OUTPUT ->
		    printer_long(lists:flatten(Trace),Editor),
		    Buffer_pid!{printer,send},
		    printer_loop(Editor,Buffer_pid);
		_ ->
		    		    Buffer_pid!{printer,send},
		    print_trace(Editor,Trace),
		    printer_loop(Editor,Buffer_pid)
	    end;
	clear ->
	    pman_win:configeditor(Editor, [{enable, true}]),
	    pman_win:configeditor(Editor,clear),
	    pman_win:configeditor(Editor, [{enable, false}]),
	    printer_loop(Editor,Buffer_pid);
	_Other ->
	    printer_loop(Editor,Buffer_pid)
    end.

printer_long([],_) -> ok;
printer_long(Trace,Editor) ->
    receive
	clear ->
	    pman_win:configeditor(Editor, [{enable, true}]),
	    pman_win:configeditor(Editor,clear),
	    pman_win:configeditor(Editor, [{enable, false}])
    after 0 ->
	    {_Length,Rest,Print} = pman_buf_utils:split(Trace,
							?MAX_OUTPUT,
							0,
							[]),
	    print_trace(Editor,Print),
	    printer_long(Rest,Editor)
    end.



%% ---------------------------------------------------------------
%% Function which print trace messages on the window 
%% ---------------------------------------------------------------

print_trace(Editor,Elements) ->
    pman_win:configeditor(Editor, [{enable, true}]),
    pman_win:configeditor(Editor, [{insert, {'end',Elements}}]),
    pman_win:configeditor(Editor, [{enable, false}]).	
