%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : etrap_logmgr.erl
%% Purpose : Make it easier to use disk_log.
%%----------------------------------------------------------------------

-module(etrap_logmgr).

%%--------------- INCLUDES -----------------------------------
%% Local
-include_lib("ETraP_Common.hrl").
%%--------------- IMPORTS-------------------------------------
%%--------------- EXPORTS-------------------------------------
-export([start/1, stop/1, log_safe/2, log_lazy/2, get_next/2]).


%%------------------------------------------------------------
%% function : start
%% Arguments: LogName   - name of the disk_log.
%% Returns  : 
%% Effect   : creating linked log
%%------------------------------------------------------------

start(LogName) ->  
    case catch disk_log:open([{name, LogName}, 
			      {file, LogName}, 
			      {type, halt},
			      {size, infinity}]) of
        {ok, LogName} ->
            ok;
        {error, Reason} ->
	    ?tr_error_msg("Initiating internal log failed: ~p", [Reason]),
	    exit({error, Reason});
	{repaired, LogName, {recovered, _Rec}, {badbytes, _Bad}} ->
            ok;
	Other ->
	    ?tr_error_msg("Initiating internal log failed: ~p", [Other]),
	    exit({error, Other})
    end.

%%------------------------------------------------------------
%% function : stop
%% Arguments: LogName - name of the disk_log.
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

stop(LogName) ->
    case catch disk_log:close(LogName) of
        ok ->
            ok;
        {error, Reason} ->
	    ?tr_error_msg("Stopping internal log failed: ~p", [Reason]),
	    {error, Reason};
	Other ->
	    ?tr_error_msg("Stopping internal log failed: ~p", [Other]),
	    {error, Other}
    end.
    

%%------------------------------------------------------------
%% function : log_safe
%% Arguments: LogName -  name of the disk_log. If 'dummy' is
%%            used nothing should be logged. Reason, reuse code.
%%            LogRecord - record to store in the log.
%% Returns  : 
%% Effect   : Writes a logrecord and synchronizes to make sure
%%            that the record is stored.
%%------------------------------------------------------------

log_safe(dummy, _) ->
    ok;
log_safe(LogName, LogRecord) ->
    case write_safe(LogName, LogRecord) of
	ok ->
	    ok;
	_ ->
	    %% We have to catch the exit because in some cases
	    %% it's not possible to abort action in the 2PC-protocol.
	    case catch start(LogName) of
		ok ->
		    write_safe(LogName, LogRecord);
		{'EXIT', Reason} ->
		    {error, Reason}
	    end
    end.


write_safe(LogName, LogRecord) ->
    case catch disk_log:log(LogName, LogRecord) of
        ok -> % wrote to kernel successfully
            case catch disk_log:sync(LogName) of
		ok -> % Written to disk successfully
		    ok;
		{error, Reason} ->
		    ?tr_error_msg("Internal log write failed: ~p   ~p",
				  [Reason, LogName]),
		    {error, Reason};
		Other -> 
		    ?tr_error_msg("Internal log write failed: ~p   ~p",
				  [Other, LogName]),
		    {error, Other}
	    end;
	{error, Reason} ->
	    ?tr_error_msg("Internal log write failed: ~p   ~p", [Reason, LogName]),
	    {error, Reason};
	Other ->
	    ?tr_error_msg("Internal log write failed: ~p   ~p", [Other, LogName]),
	    {error, Other}
    end.


%%------------------------------------------------------------
%% function : log_lazy
%% Arguments: LogName -  name of the disk_log. If 'dummy' is
%%            used nothing should be logged. Reason, reuse code.
%%            LogRecord - record to store in the log. 
%% Returns  : 
%% Effect   : Writes a logrecord. The record may be lost.
%%------------------------------------------------------------

log_lazy(dummy, _LogRecord) ->
    ok;
log_lazy(LogName, LogRecord) ->
    case write_lazy(LogName, LogRecord) of
	ok ->
	    ok;
	_ ->
	    %% We have to catch the exit because in some cases
	    %% it's not possible to abort action in the 2PC-protocol.
	    case catch start(LogName) of
		ok ->
		    write_lazy(LogName, LogRecord);
		{'EXIT', Reason} ->
		    {error, Reason}
	    end
    end.

write_lazy(LogName, LogRecord) ->
    case catch disk_log:log(LogName, LogRecord) of
        ok ->  
	    %% wrote to kernel successfully
            ok;
        {error, Reason} -> 
	    %% Write to kernel failed with Reason
	    ?tr_error_msg("Internal log write failed: ~p", [Reason]),
	    {error, Reason};
	Other -> 
	    %% unknown message received.
	    ?tr_error_msg("Internal log write failed: ~p", [Other]),
	    {error, Other}
    end.


%%------------------------------------------------------------
%% function : get_next
%% Arguments: LogName -  name of the disk_log.
%%            Cursor - place to read from.
%% Returns  : {Cursor, LogRecs} - A cursor and up to N logrecords.
%%            eof - the atom 'eof', indicating logfile empty. 
%%            {error, Reason} - error.
%% Effect   : 
%% Purpose  : Used when performing a REDO scan
%%------------------------------------------------------------

get_next(LogName, Cursor) ->
    case catch disk_log:chunk(LogName, Cursor, 1) of
	{NewCursor, [Data]} ->
	    {Data, NewCursor};
	eof ->
	    eof;
	{error, Reason} ->
	    ?tr_error_msg("Internal log '~p' read failed: ~p",
		      [LogName, Reason]),
	    exit({error, Reason});
	_Other ->
	    ?tr_error_msg("Internal log '~p' read failed: 'log_corrupt'", [LogName]),
	    exit({error, "log_corrupt"})
    end.

%%--------------- END OF MODULE ------------------------------
