%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Author: Lennart Öhman, lennart.ohman@st.se

%%
%% INVISO LogFileMerger TracePort File READER.
%%
%% This module implements a reader process capable of reading traceport files
%% and feeding them according to the logfile merger process message protocoll
%% to the logfile merger process.
%% This module can also serve as example for writing file readers for other
%% file formats.
%%
%% A reader process must:
%%   Support the reader-receiver protocoll.
%%     receive next_entry message: {get_next_entry,ReceiverPid}
%%     recieve stop message should the receiver wish to quit: {stop,ReceiverPid}.
%%     send next_entry message, either with entry or fault-code.
%%       next_entry message contains:{next_entry,self(),PidMappings,Timestamp,Term}
%%                                   {next_entry,self(),Error}
%%     recognize receiver termination (EXIT-signal).
%%  Understand logfile structure, both filename structure and content.
%%  Understand content (log-entry) details to extract the entry and entry
%%    components as timestamp and originating pid (to make pid-mappings).
%%  Understand any trace information files (ti).
%%
%%  The logfile structure written by inviso_rt_meta is:
%%    {Pid,Alias,Op,TimeStamp} where:
%%      Pid=pid(), if Alias==unalias: pid()|other_than_pid()
%%      Op=alias|unalias,
%%      TimeStamp=now()
%% -----------------------------------------------------------------------------
-module(inviso_lfm_tpfreader).

-export([init/2]).
%% -----------------------------------------------------------------------------

-export([handle_logfile_sort_wrapset/1]).   % Exported as a service to other readers.
%% -----------------------------------------------------------------------------

%% init(RecPid,FileStruct)=N/A
%%   RecPid=pid(), the process id of the log file merger.
%%   FileStruct=LogFiles | [LogFiles,...]
%%     LogFiles=[{trace_log,[File,...]} [,{ti_log,[File]}] ]
%%       File=string()
%% Spawn on this function to start a reader process for trace-port generated
%% logfiles, possibly with inviso-generated ti-files.
init(RecPid,LogFiles=[Tuple|_]) when tuple(Tuple) -> % Only one LogFiles.
    init(RecPid,[LogFiles]);
init(RecPid,FileStruct) when list(FileStruct) ->
    logfiles_loop(RecPid,FileStruct).
%% -----------------------------------------------------------------------------

logfiles_loop(RecPid,[LogFiles|Rest]) ->
    {TIalias,TIunalias}=handle_ti_file(LogFiles),% If there is a ti-file, read it.
    Files=handle_logfiles(LogFiles),        % Returns a sorted list of logfiles.
    case open_next_file(Files) of
	{ok,FileName,FD,NewFiles} ->
	    case loop(RecPid,FileName,NewFiles,TIalias,TIunalias,FD) of
		next ->
		    logfiles_loop(RecPid,Rest);
		stop ->
		    true                    % Terminate normally.
	    end;
	done ->                             % Hmm, already out of files.
	    true;                           % Then lets terminate normally.
	{error,Reason} ->                   % Couldn't even open the first file.
	    exit(Reason)
    end;
logfiles_loop(_RecPid,[]) ->                % No more files in LogFiles.
    true.                                   % Terminate normally.

%% This workloop reads an entry from the input file upon request from the merger
%% process and sends it back to the merger process (Parent). If the file ends
%% there are more files to open and read in Files, the next file will be opened. 
loop(RecPid,FileName,Files,TIalias,TIunalias,FD) ->
    receive
	{get_next_entry,RecPid} ->           % The receiver request the next entry.
	    case fetch_next(FileName,FD,Files) of
		{ok,Term,NewCurrFile,NewFiles,NewFD} ->
		    TS=find_timestamp_in_term(Term),
		    PidMappings=make_pid_mappings(Term,TIalias,TIunalias,TS),
		    RecPid ! {next_entry,self(),PidMappings,TS,Term},
		    loop(RecPid,NewCurrFile,NewFiles,TIalias,TIunalias,NewFD);
		{error,Reason} ->            % Not a properly formatted entry.
		    RecPid ! {next_entry,self(),{error,Reason}},
		    loop(RecPid,FileName,Files,TIalias,TIunalias,FD);
		done ->                     % No more files to read in this LogFiles.
		    next                    % Are there more Files in FileStruct?
	    end;
	{stop,RecPid} ->                    % The receiver process is done.
	    file:close(FD),                 % Close file and terminate normally.
	    stop
    end.
%% -----------------------------------------------------------------------------

%% Function which reads the next trace-entry from the file handled by FD, or if
%% that file reaches EOF opens the next file in Files. Files must be sorted in
%% the correct order.
%% Returns {ok,Term,NewFileName,NewFiles,NewFD}, {error,Reason} or 'done'.
fetch_next(FileName,FD,Files) ->
    case read_traceport_file(FileName,FD) of
	{ok,Term} ->                        % There were more terms in the file.
	    {ok,Term,FileName,Files,FD};    % No changes necessary then.
	eof ->                              % This file is empty, try next file!
	    file:close(FD),
	    case open_next_file(Files) of
		{ok,NewFileName,NewFD,NewFiles} -> % A new file has been opened.
		    fetch_next(NewFileName,NewFD,NewFiles); % Try again.
		done ->                     % No more files.
		    done;
		{error,Reason} ->           % Problems opening files.
		    {error,Reason}
	    end;
	{error,Reason} ->                   % Problems reading the file.
	    {error,Reason}
    end.

read_traceport_file(FileName,FD) ->
    case file:read(FD,5) of                 % Trace-port file entries start with 5 bytes.
	{ok,<<0,Size:32>>} ->               % Each entry in a traceport file begins.
	    case file:read(FD,Size) of
		{ok,Bin} when binary(Bin),size(Bin)=:=Size ->
		    try binary_to_term(Bin) of
			Term ->             % Bin was a properly formatted term!
			    {ok,Term}
		    catch
			error:_Reason ->    % Not a properly formatted term!
			    {error,{binary_to_term,[FileName,Bin]}}
		    end;
		{ok,Bin} ->                 % Incorrect length.
		    {error,{faulty_length,[FileName,Size,Bin]}};
		eof ->                      % This is premature end of file!
		    {error,{premature_eof,FileName}}
	    end;
	{ok,<<1,DroppedMsgs:32>>} ->
	    {ok,{drop,DroppedMsgs}};
	{ok,JunkBin} ->                     % Don't understand, report it as error.
	    {error,{junk,[FileName,JunkBin]}};
	eof ->                              % A correct end of file!
	    eof
    end.

%% Help function which opens a file in raw binary mode and returns
%% {ok,FileName,FD,Rest} or {error,Reason}.
open_next_file([]) ->                       % There are no more files to open.
    done;
open_next_file([FileName|Rest]) ->
    case file:open(FileName,[read,raw,binary]) of
	{ok,FD} ->
	    {ok,FileName,FD,Rest};
	{error,Reason} ->
	    {error,{open,[FileName,Reason]}}
    end.
%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Help functions.
%% ==============================================================================


%% Help function which extract the originating process id from the log entry
%% term and returns a list of all associations to the PID found in TIalias.
make_pid_mappings(_,void,_,_) ->            % Trace Information is not used.
    [];                                     % Simply no pid mappings then!
make_pid_mappings(Term,TIalias,TIunalias,TS)
  when element(1,Term)==trace;element(1,Term)==trace_ts ->
    Pid=element(2,Term),                    % The pid.
    TempAliases=find_aliases(ets:lookup(TIalias,Pid),TS),
    remove_expired_aliases(TempAliases,TIalias,TIunalias,TS),
    lists:map(fun({_,_,Alias})->Alias end,
	      find_aliases(ets:lookup(TIalias,Pid),TS));
make_pid_mappings(_Term,_TIalias,_TIunalias,_TS) -> % Don't understand Term.
    [].                                     % Simply no translations then!

%% Help function traversing a list of ets-alias-table entries and returning a
%% list of those old enough to have happend before TS.
%% Note that it is possible to have an Offset in microseconds. This because an
%% association may end up in the ti-file a short time after logentries starts
%% to appear in the log file for the process in question. We therefore like to
%% allow some slack, 
find_aliases(List,TS) ->
    lists:filter(fun({_,Now,_}) when Now<TS -> true;
		    (_) -> false
		 end,
		 List).
%% ------------------------------------------------------------------------------

%% Help function which removes aliases that are no longer valid from the
%% ETS table. It uses unalias entries which are older than TS but younger than
%% the alias association.
%% Returns nothing significant.
remove_expired_aliases([{Pid,Now1,Alias}|Rest],TIalias,TIunalias,TS) ->
    Candidates=ets:lookup(TIunalias,Alias),
    lists:foreach(fun({_,Now2,P})
		     when (Now2>Now1) and
		          (Now2<TS) and
		          ((P==Pid) or (not(is_pid(P)))) ->
			  ets:delete_object(TIalias,{Pid,Now1,Alias}),
			  true;             % This alias is infact no longer.
		     (_) ->
			  false
		  end,
		  Candidates),
    remove_expired_aliases(Rest,TIalias,TIunalias,TS);
remove_expired_aliases([],_,_,_) ->
    true.
%% ------------------------------------------------------------------------------

find_timestamp_in_term({trace_ts,_,_,_,TS}) ->
    TS;
find_timestamp_in_term({trace_ts,_,_,_,_,TS}) ->
    TS;
find_timestamp_in_term(_) ->                % Don't know if there is a timestamp.
    false.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Help function handling a trace-information file and building the TIstruct storage.
%% -----------------------------------------------------------------------------

%% Help function which opens a standard ti-file, reads its content and
%% builds two ETS-table where PID is primary index in the one for aliases, and
%% the alias is primary index in the one for unalias.
%% Returns a handle to the two ETS tables.
%%
%% This function currently handles:
%%   (1) plain straight raw binary files.
handle_ti_file(FileStruct) ->
    case lists:keysearch(ti_log,1,FileStruct) of
	{value,{_,[FileName]}} when list(FileName) -> % There is one ti-file in this set.
	    case file:open(FileName,[read,raw,binary]) of
		{ok,FD} ->
		    TIdAlias=ets:new(list_to_atom("inviso_ti_atab_"++pid_to_list(self())),
				     [bag]),
		    TIdUnalias=ets:new(list_to_atom("inviso_ti_utab_"++pid_to_list(self())),
				       [bag]),
		    handle_ti_file_2(FD,TIdAlias,TIdUnalias), % Fill the table.
		    file:close(FD),
		    {TIdAlias,TIdUnalias};
		{error,_Reason} ->          % Hmm, unable to open the file.
		    {void,void}             % Treat it as no ti-file.
	    end;
	{value,_} ->                        % Some other file-set.
	    {void,void};                    % Pretend we don't understand.
	false ->                            % No ti-file in this set.
	    {void,void}
    end.

handle_ti_file_2(FD,TIdAlias,TIdUnalias) ->
    case file:read(FD,5) of                 % First read the header.
	{ok,<<_,Size:32>>} ->
	    case file:read(FD,Size) of      % Read the actual term.
		{ok,Bin} when size(Bin)=:=Size ->
		    try binary_to_term(Bin) of
			{Pid,Alias,alias,NowStamp} -> % Save this association.
			    ets:insert(TIdAlias,{Pid,NowStamp,Alias}),
			    handle_ti_file_2(FD,TIdAlias,TIdUnalias);
			{Pid,Alias,unalias,NowStamp} ->
			    ets:insert(TIdUnalias,{Alias,NowStamp,Pid}),
			    handle_ti_file_2(FD,TIdAlias,TIdUnalias);
			_Term ->            % Don't understand!
			    handle_ti_file_2(FD,TIdAlias,TIdUnalias)
		    catch
			error:_Reason ->    % Badly formatted term
			    handle_ti_file_2(FD,TIdAlias,TIdUnalias)
		    end;
		{ok,_JunkBin} ->            % To short probably.
		    handle_ti_file_2(FD,TIdAlias,TIdUnalias); % Just drop it.
		eof ->                      % Should not come here, but
		    {TIdAlias,TIdUnalias}   % not much we can do, drop it and stop.
	    end;
	{ok,_} ->                           % Also an error.
	    handle_ti_file_2(FD,TIdAlias,TIdUnalias);
	eof ->                              % This is the normal eof point.
	    {TIdAlias,TIdUnalias}
    end.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Help functions sorting out what kind of logfiles we have to deal with.
%% -----------------------------------------------------------------------------

%% Help function which takes the filestruct argument and retrieves the names
%% of all log-files mentioned there. If there are several logfiles, this function
%% sorts them beginning with the oldest. That means that this function must
%% have knowledge of how wrap-sets and so on works.
%% Today known set-types:
%%   (1) file: One plain file.
%%   (2) wrap_set: List of files belonging to a wrap-set. Must be sorted.
handle_logfiles(FileStruct) ->
    handle_logfiles_2(lists:keysearch(trace_log,1,FileStruct)).

handle_logfiles_2({value,{_,[FileName]}}) when list(FileName)-> % One single plain file.
    [FileName];
handle_logfiles_2({value,{_,Files}}) when list(Files) -> % A wrap-set.
    handle_logfile_sort_wrapset(Files);
handle_logfiles_2(_) ->
    [].                                      % Pretend there were no files otherwise.

%% Help function which sorts the files in WrapSet beginning with the oldest.
%% It assumes that a logfile is Name++SeqNo++Suffix.
%% First the Name and Suffix must be established. We look at all files to find
%% that out.
%% Returns a list of sorted filenames.
%% This function is exported since it might turn useful in own implemented
%% readers.
handle_logfile_sort_wrapset(Set=[_FileName]) -> % Only one file! Done then :-)
    Set;
handle_logfile_sort_wrapset([]) ->           % Also pretty simple :-)
    [];
handle_logfile_sort_wrapset(FileSet) ->
    Prefix=find_common_prefix(FileSet),
    Suffix=find_common_prefix(lists:map(fun(Str)->lists:reverse(Str) end,FileSet)),
    find_hole_in_wrapset(FileSet,length(Prefix),length(Suffix)).

%% Help function which finds the longest common prefix of all strings in the
%% argument-list. Returns that string.
find_common_prefix(Files=[[FirstChar|_]|_]) ->
    find_common_prefix_2(Files,FirstChar,[],[]);
find_common_prefix([_|_]) ->                 % Means that prefix is "".
    "".

find_common_prefix_2([[CurrChar|RestString]|Rest],CurrChar,Files,RevPrefix) ->
    find_common_prefix_2(Rest,CurrChar,[RestString|Files],RevPrefix);
find_common_prefix_2([_String|_],_CurrChar,_Files,RevPrefix) ->
    lists:reverse(RevPrefix);                % Found a difference.
find_common_prefix_2([],CurrChar,Files=[[FirstChar|_]|_],RevPrefix) ->
    find_common_prefix_2(Files,FirstChar,[],[CurrChar|RevPrefix]);
find_common_prefix_2([],CurrChar,_,RevPrefix) ->
    lists:reverse([CurrChar|RevPrefix]).     % Actually, prefix was entire string!

%% Help function which returns a sorted list of FileSet with the oldest first.
find_hole_in_wrapset(FileSet,PreLen,SufLen) ->
    NumberedFiles=find_hole_in_wrapset_2(FileSet,PreLen,SufLen),
    find_hole_in_wrapset_3(lists:sort(NumberedFiles),0,[]). % Wrap-sets start at 0.

find_hole_in_wrapset_2([FileName|Rest],PreLen,SufLen) ->
    [{list_to_integer(lists:sublist(FileName,PreLen+1,length(FileName)-PreLen-SufLen)),
      FileName}|
     find_hole_in_wrapset_2(Rest,PreLen,SufLen)];
find_hole_in_wrapset_2([],_,_) ->
    [].

find_hole_in_wrapset_3([{N,FileName}|Rest],N,Acc) ->
    find_hole_in_wrapset_3(Rest,N+1,[FileName|Acc]);
find_hole_in_wrapset_3([{_,FileName}|Rest],_N,Acc) -> % FileName is the oldest one.
    [FileName|lists:map(fun({_,FN})->FN end,Rest)]++lists:reverse(Acc);
find_hole_in_wrapset_3([],_,Acc) ->          % Means all were in order.
    lists:reverse(Acc).    
%% -----------------------------------------------------------------------------


			      
    
















