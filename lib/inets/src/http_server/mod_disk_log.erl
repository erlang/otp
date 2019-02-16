%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(mod_disk_log).

%% Application internal API
-export([error_log/2, report_error/2, security_log/2]).

%% Callback API
-export([do/1, load/2, store/2, remove/1]).

-define(VMODULE,"DISK_LOG").

-include("httpd.hrl").
-include("httpd_internal.hrl").

%%%=========================================================================
%%%  API 
%%%=========================================================================

%% security_log
security_log(#mod{config_db = ConfigDb} = Info, Event) ->
    Format = get_log_format(ConfigDb),
    Date = httpd_util:custom_date(),
    case httpd_log:security_entry(security_disk_log, no_security_log, 
				  Info, Date, Event) of
	no_security_log ->
	    ok;
	{Log, Entry} ->
	    write(Log, Entry, Format)
    end.

report_error(ConfigDB, Error) ->
    Format = get_log_format(ConfigDB),
    Date = httpd_util:custom_date(),
    case httpd_log:error_report_entry(error_disk_log, no_error_log, ConfigDB,
				      Date, Error) of
	no_error_log ->
	    ok;
	{Log, Entry} ->
	    write(Log, Entry, Format)
    end.

error_log(Info, Reason) ->
    Date = httpd_util:custom_date(),
    error_log(Info, Date, Reason).

error_log(#mod{config_db = ConfigDB} = Info, Date, Reason) ->
    Format = get_log_format(ConfigDB),
     case httpd_log:error_entry(error_disk_log, no_error_log, 
			       Info, Date, Reason) of
	no_error_log ->
	    ok;
	{Log, Entry} ->
	     write(Log, Entry, Format)
    end.

%%%=========================================================================
%%%  CALLBACK API 
%%%=========================================================================
%%--------------------------------------------------------------------------
%% do(ModData) -> {proceed, OldData} | {proceed, NewData} | {break, NewData} 
%%                | done
%%     ModData = #mod{}
%%
%% Description:  See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
do(Info) ->
    AuthUser  = auth_user(Info#mod.data),
    Date      = httpd_util:custom_date(),
    log_internal_info(Info,Date,Info#mod.data),
    LogFormat = get_log_format(Info#mod.config_db),
    case proplists:get_value(status, Info#mod.data) of
	%% A status code has been generated!
	{StatusCode, _PhraseArgs, Reason} ->
	    transfer_log(Info, "-", AuthUser, Date, StatusCode, 0,
			 LogFormat),
	    if
		StatusCode >= 400 ->
		    error_log(Info, Date, Reason);
		true ->
		    not_an_error
	    end,
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, Info#mod.data) of
		{already_sent,StatusCode,Size} ->
		    transfer_log(Info, "-", AuthUser, Date, StatusCode,
				 Size, LogFormat),
		    {proceed,Info#mod.data};

		{response, Head, _Body} ->
		    Size = proplists:get_value(content_length, Head, 0),
		    Code = proplists:get_value(code, Head, 200),
		    transfer_log(Info, "-", AuthUser, Date, Code, 
				 Size, LogFormat),
		    {proceed,Info#mod.data};	
		
		{_StatusCode, Response} ->
		    transfer_log(Info, "-", AuthUser, Date, 200,
				 httpd_util:flatlength(Response), LogFormat),
		    {proceed,Info#mod.data};
		undefined ->
		    transfer_log(Info, "-", AuthUser, Date, 200,
				 0, LogFormat),
		    {proceed,Info#mod.data}
	    end
    end.

%%--------------------------------------------------------------------------
%% load(Line, Context) ->  eof | ok | {ok, NewContext} | 
%%                     {ok, NewContext, Directive} | 
%%                     {ok, NewContext, DirectiveList} | {error, Reason}
%% Line = string()
%% Context = NewContext = DirectiveList = [Directive]
%% Directive = {DirectiveKey , DirectiveValue}
%% DirectiveKey = DirectiveValue = term()
%% Reason = term() 
%%
%% Description: See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
load("TransferDiskLogSize " ++ TransferDiskLogSize, []) ->
    try re:split(TransferDiskLogSize, " ",  [{return, list}]) of
	[MaxBytes, MaxFiles] ->
	    case make_integer(MaxBytes) of
		{ok,MaxBytesInteger} ->
		    case make_integer(MaxFiles) of
			{ok,MaxFilesInteger} ->
			    {ok,[],{transfer_disk_log_size,
				    {MaxBytesInteger,MaxFilesInteger}}};
			{error,_} ->
			    {error,
			     ?NICE(string:strip(TransferDiskLogSize)++
				   " is an invalid TransferDiskLogSize")}
		    end;
		_ ->
		    {error,?NICE(string:strip(TransferDiskLogSize)++
				     " is an invalid TransferDiskLogSize")}
	    end
    catch _:_ ->
	    {error,?NICE(string:strip(TransferDiskLogSize) ++
			     " is an invalid TransferDiskLogSize")}   
    end;
load("TransferDiskLog " ++ TransferDiskLog,[]) ->
    {ok,[],{transfer_disk_log,string:strip(TransferDiskLog)}};
 
load("ErrorDiskLogSize " ++  ErrorDiskLogSize, []) ->
    try re:split(ErrorDiskLogSize," ", [{return, list}]) of
	[MaxBytes,MaxFiles] ->
	    case make_integer(MaxBytes) of
		{ok,MaxBytesInteger} ->
		    case make_integer(MaxFiles) of
			{ok,MaxFilesInteger} ->
			    {ok,[],{error_disk_log_size,
				    {MaxBytesInteger,MaxFilesInteger}}};
			{error,_} ->
			    {error,?NICE(string:strip(ErrorDiskLogSize)++
					 " is an invalid ErrorDiskLogSize")}
		    end;
		{error,_} ->
		    {error,?NICE(string:strip(ErrorDiskLogSize)++
				 " is an invalid ErrorDiskLogSize")}
	    end
    catch _:_ ->
	    {error,?NICE(string:strip(ErrorDiskLogSize) ++
			     " is an invalid TransferDiskLogSize")}   
    end;
load("ErrorDiskLog " ++ ErrorDiskLog, []) ->
    {ok, [], {error_disk_log, string:strip(ErrorDiskLog)}};

load("SecurityDiskLogSize " ++ SecurityDiskLogSize, []) ->
    try re:split(SecurityDiskLogSize, " ", [{return, list}]) of
	[MaxBytes, MaxFiles] ->
	    case make_integer(MaxBytes) of
		{ok, MaxBytesInteger} ->
		    case make_integer(MaxFiles) of
			{ok, MaxFilesInteger} ->
			    {ok, [], {security_disk_log_size,
				      {MaxBytesInteger, MaxFilesInteger}}};
			{error,_} ->
			    {error, 
			     ?NICE(string:strip(SecurityDiskLogSize) ++
				   " is an invalid SecurityDiskLogSize")}
		    end;
		{error, _} ->
		    {error, ?NICE(string:strip(SecurityDiskLogSize) ++
				  " is an invalid SecurityDiskLogSize")}
	    end
    catch _:_ ->
	    {error,?NICE(string:strip(SecurityDiskLogSize) ++
			     " is an invalid SecurityDiskLogSize")}   	
    end;
load("SecurityDiskLog " ++ SecurityDiskLog, []) ->
    {ok, [], {security_disk_log, string:strip(SecurityDiskLog)}};

load("DiskLogFormat " ++ Format, []) ->
    case string:strip(Format) of
	"internal" ->
	    {ok, [], {disk_log_format,internal}};
	"external" ->
	    {ok, [], {disk_log_format,external}};
	_Default ->
	    {ok, [], {disk_log_format,external}}
    end.

%%--------------------------------------------------------------------------
%% store(Directive, DirectiveList) -> {ok, NewDirective} | 
%%                                    {ok, [NewDirective]} |
%%                                    {error, Reason} 
%% Directive = {DirectiveKey , DirectiveValue}
%% DirectiveKey = DirectiveValue = term()
%% Reason = term() 
%%
%% Description: See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
store({transfer_disk_log,TransferDiskLog}, ConfigList) 
  when is_list(TransferDiskLog) ->
    case create_disk_log(TransferDiskLog, 
			 transfer_disk_log_size, ConfigList) of
	{ok,TransferDB} ->
	    {ok,{transfer_disk_log,TransferDB}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({transfer_disk_log,TransferLog}, _) ->
    {error, {wrong_type, {transfer_disk_log, TransferLog}}};
store({security_disk_log,SecurityDiskLog},ConfigList) 
  when is_list(SecurityDiskLog) ->
    case create_disk_log(SecurityDiskLog, 
			 security_disk_log_size, ConfigList) of
	{ok,SecurityDB} ->
	    {ok,{security_disk_log,SecurityDB}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({security_disk_log, SecurityLog}, _) ->
    {error, {wrong_type, {security_disk_log, SecurityLog}}};

store({error_disk_log,ErrorDiskLog},ConfigList) when is_list(ErrorDiskLog) ->
    case create_disk_log(ErrorDiskLog, error_disk_log_size, ConfigList) of
	{ok,ErrorDB} ->
	    {ok,{error_disk_log,ErrorDB}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({error_disk_log,ErrorLog}, _) ->
    {error, {wrong_type, {error_disk_log, ErrorLog}}};
store({transfer_disk_log_size, {ByteInt, FileInt}} = Conf, _) 
  when is_integer(ByteInt), is_integer(FileInt)->
    {ok, Conf};
store({transfer_disk_log_size, Value}, _) ->
    {error, {wrong_type, {transfer_disk_log_size, Value}}};
store({error_disk_log_size, {ByteInt, FileInt}} = Conf, _) 
  when is_integer(ByteInt), is_integer(FileInt)->
    {ok, Conf};
store({error_disk_log_size, Value}, _) ->
    {error, {wrong_type, {error_disk_log_size, Value}}};
store({security_disk_log_size, {ByteInt, FileInt}} = Conf, _) 
  when is_integer(ByteInt), is_integer(FileInt)->
    {ok, Conf};
store({security_disk_log_size, Value}, _) ->
    {error, {wrong_type, {security_disk_log_size, Value}}};
store({disk_log_format, Value} = Conf, _) when Value == internal; 
					Value == external ->
    {ok, Conf};
store({disk_log_format, Value}, _) ->
    {error, {wrong_type, {disk_log_format, Value}}}.

%%--------------------------------------------------------------------------
%% remove(ConfigDb) -> _
%%
%% Description: See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
remove(ConfigDB) ->
    lists:foreach(fun([DiskLog]) -> close(DiskLog) end,
		  ets:match(ConfigDB,{transfer_disk_log,'$1'})),
    lists:foreach(fun([DiskLog]) -> close(DiskLog) end,
		  ets:match(ConfigDB,{error_disk_log,'$1'})),
    lists:foreach(fun([DiskLog]) -> close(DiskLog) end,
		  ets:match(ConfigDB,{security_disk_log,'$1'})),
    ok.

%%%========================================================================
%%% Internal functions
%%%======================================================================== 

%% transfer_log
transfer_log(Info, RFC931, AuthUser, Date, StatusCode, Bytes, Format) ->
    case httpd_log:access_entry(transfer_disk_log, no_transfer_log,
				Info, RFC931, AuthUser, Date, StatusCode, 
				Bytes) of
	no_transfer_log ->
	    ok;
	{Log, Entry} ->
    	    write(Log, Entry, Format)
    end.


get_log_format(ConfigDB)->
    httpd_util:lookup(ConfigDB,disk_log_format,external).

%%----------------------------------------------------------------------
%% Open or creates the disklogs 
%%----------------------------------------------------------------------
log_size(ConfigList, Tag) ->
    proplists:get_value(Tag, ConfigList, {500*1024,8}).

create_disk_log(LogFile, SizeTag, ConfigList) ->
    Filename = string:strip(LogFile),
    {MaxBytes, MaxFiles} = log_size(ConfigList, SizeTag),
    case filename:pathtype(Filename) of
	absolute ->
	    create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList);
	volumerelative ->
	    create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList);
	relative ->
	    case proplists:get_value(server_root,ConfigList) of
		undefined ->
		    {error,
		     ?NICE(Filename++
			   " is an invalid ErrorLog beacuse ServerRoot "
			   "is not defined")};
		ServerRoot ->
		    AbsoluteFilename = filename:join(ServerRoot,Filename),
		    create_disk_log(AbsoluteFilename, MaxBytes, MaxFiles,
				     ConfigList)
	    end
    end.

create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList) ->
    Format = proplists:get_value(disk_log_format, ConfigList, external),
    open(Filename, MaxBytes, MaxFiles, Format).
    

%%----------------------------------------------------------------------
%% Function:    open/4
%% Description: Open a disk log file.
%% Control which format the disk log will be in. The external file 
%% format is used as default since that format was used by older 
%% implementations of inets.
%%
%% When the internal disk log format is used, we will do some extra 
%% controls. If the files are valid, try to repair them and if 
%% thats not possible, truncate.
%%----------------------------------------------------------------------

open(Filename, MaxBytes, MaxFiles, internal) ->
    Opt0 = {format, internal},
    Opts1 = [Opt0, {repair, true}],
    Opts2 = [Opt0, {repair, truncate}],
    open1(Filename, MaxBytes, MaxFiles, Opts1, Opts2);
open(Filename, MaxBytes, MaxFiles, _) ->
    Opts = [{format, external}],
    open1(Filename, MaxBytes, MaxFiles, Opts, Opts).

open1(Filename, MaxBytes, MaxFiles, Opts1, Opts2) ->
    Opts0 = [{name, Filename}, {file, Filename}, {type, wrap}],
    case open2(Opts0 ++ Opts1, Opts0 ++ Opts2, {MaxBytes, MaxFiles}) of
        {ok, LogDB} ->
            {ok, LogDB};
        {repaired, LogDB, {recovered, _}, {badbytes, _}} ->
            {ok, LogDB};
        {error, Reason} ->
            {error, 
             ?NICE("Can't create " ++ Filename ++ 
                   lists:flatten(io_lib:format(", ~p",[Reason])))};
        _ ->
            {error, ?NICE("Can't create "++Filename)}
    end.

open2(Opts1, Opts2, Size) ->
    case disk_log:open(Opts1) of
        {error, {badarg, size}} ->
            %% File did not exist, add the size option and try again
            disk_log:open([{size, Size} | Opts1]);
        {error, {Reason, _}} when
                Reason == not_a_log_file;
                Reason == invalid_index_file ->
            %% File was corrupt, add the truncate option and try again
            disk_log:open([{size, Size} | Opts2]);
        Else ->
            Else
    end.


%%----------------------------------------------------------------------
%% Actually writes the entry to the disk_log. If the log is an 
%% internal disk_log write it with log otherwise with blog.
%%----------------------------------------------------------------------  
write(Log, Entry, internal) ->
    disk_log:log(Log, list_to_binary(Entry));

write(Log, Entry, _) ->
    disk_log:blog(Log, list_to_binary(Entry)).

%% Close the log file
close(Log) ->
    disk_log:close(Log).

auth_user(Data) ->
    case proplists:get_value(remote_user,Data) of
	undefined ->
	    "-";
	RemoteUser ->
	    RemoteUser
    end.
%% log_internal_info

log_internal_info(_, _,[]) ->
    ok;
log_internal_info(Info,Date,[{internal_info,Reason}|Rest]) ->
    error_log(Info,Date,Reason),
    log_internal_info(Info,Date,Rest);
log_internal_info(Info,Date,[_|Rest]) ->
    log_internal_info(Info,Date,Rest).

make_integer(List) ->
    try list_to_integer(List) of
	N ->
	    {ok, N}
    catch 
	_:_ ->
	    {error, {badarg, list_to_integer, List}}
    end.
