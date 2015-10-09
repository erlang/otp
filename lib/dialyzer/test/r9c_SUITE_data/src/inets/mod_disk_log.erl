%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mod_disk_log.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%
-module(mod_disk_log).
-export([do/1,error_log/5,security_log/2,load/2,store/2,remove/1]).

-export([report_error/2]).

-define(VMODULE,"DISK_LOG").
-include("httpd_verbosity.hrl").

-include("httpd.hrl").

%% do

do(Info) ->
    AuthUser  = auth_user(Info#mod.data),
    Date      = custom_date(),
    log_internal_info(Info,Date,Info#mod.data),
    LogFormat = get_log_format(Info#mod.config_db),
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    transfer_log(Info, "-", AuthUser, Date, StatusCode, 0, LogFormat),
	    if
		StatusCode >= 400 ->
		    error_log(Info, Date, Reason, LogFormat);
		true ->
		    not_an_error
	    end,
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		{already_sent,StatusCode,Size} ->
		    transfer_log(Info, "-", AuthUser, Date, StatusCode,
				 Size, LogFormat),
		    {proceed,Info#mod.data};

		{response, Head, Body} ->
		    Size = httpd_util:key1search(Head, content_length, 0),
		    Code = httpd_util:key1search(Head, code, 200),
		    transfer_log(Info, "-", AuthUser, Date, Code,
				 Size, LogFormat),
		    {proceed,Info#mod.data};

		{StatusCode,Response} ->
		    transfer_log(Info, "-", AuthUser, Date, 200,
				 httpd_util:flatlength(Response), LogFormat),
		    {proceed,Info#mod.data};
		undefined ->
		    transfer_log(Info, "-", AuthUser, Date, 200,
				 0, LogFormat),
		    {proceed,Info#mod.data}
	    end
    end.

custom_date() ->
    LocalTime     = calendar:local_time(),
    UniversalTime = calendar:universal_time(),
    Minutes       = round(diff_in_minutes(LocalTime,UniversalTime)),
    {{YYYY,MM,DD},{Hour,Min,Sec}} = LocalTime,
    Date =
	io_lib:format("~.2.0w/~.3s/~.4w:~.2.0w:~.2.0w:~.2.0w ~c~.2.0w~.2.0w",
		      [DD,httpd_util:month(MM),YYYY,Hour,Min,Sec,sign(Minutes),
		       abs(Minutes) div 60,abs(Minutes) rem 60]),
    lists:flatten(Date).

diff_in_minutes(L,U) ->
    (calendar:datetime_to_gregorian_seconds(L) -
     calendar:datetime_to_gregorian_seconds(U))/60.

sign(Minutes) when Minutes > 0 ->
    $+;
sign(Minutes) ->
    $-.

auth_user(Data) ->
    case httpd_util:key1search(Data,remote_user) of
	undefined ->
	    "-";
	RemoteUser ->
	    RemoteUser
    end.

%% log_internal_info

log_internal_info(Info,Date,[]) ->
    ok;
log_internal_info(Info,Date,[{internal_info,Reason}|Rest]) ->
    Format = get_log_format(Info#mod.config_db),
    error_log(Info,Date,Reason,Format),
    log_internal_info(Info,Date,Rest);
log_internal_info(Info,Date,[_|Rest]) ->
    log_internal_info(Info,Date,Rest).


%% transfer_log

transfer_log(Info,RFC931,AuthUser,Date,StatusCode,Bytes,Format) ->
    case httpd_util:lookup(Info#mod.config_db,transfer_disk_log) of
	undefined ->
	    no_transfer_log;
	TransferDiskLog ->
	    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
	    Entry = io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w~n",
				  [RemoteHost,RFC931,AuthUser,Date,
				   Info#mod.request_line,StatusCode,Bytes]),
	    write(TransferDiskLog, Entry, Format)
    end.


%% error_log

error_log(Info, Date, Reason, Format) ->
    Format=get_log_format(Info#mod.config_db),
    case httpd_util:lookup(Info#mod.config_db,error_disk_log) of
	undefined ->
	    no_error_log;
	ErrorDiskLog ->
	    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
	    Entry =
		io_lib:format("[~s] access to ~s failed for ~s, reason: ~p~n",
			      [Date, Info#mod.request_uri,
			       RemoteHost, Reason]),
	    write(ErrorDiskLog, Entry, Format)
    end.

error_log(SocketType, Socket, ConfigDB, {PortNumber, RemoteHost}, Reason) ->
    Format = get_log_format(ConfigDB),
    case httpd_util:lookup(ConfigDB,error_disk_log) of
	undefined ->
	    no_error_log;
	ErrorDiskLog ->
	    Date  = custom_date(),
	    Entry =
		io_lib:format("[~s] server crash for ~s, reason: ~p~n",
			      [Date,RemoteHost,Reason]),
	    write(ErrorDiskLog, Entry, Format),
	    ok
    end.


%% security_log

security_log(ConfigDB, Event) ->
    Format = get_log_format(ConfigDB),
    case httpd_util:lookup(ConfigDB,security_disk_log) of
	undefined ->
	    no_error_log;
	DiskLog ->
	    Date  = custom_date(),
	    Entry = io_lib:format("[~s] ~s ~n", [Date, Event]),
	    write(DiskLog, Entry, Format),
	    ok
    end.

report_error(ConfigDB, Error) ->
    Format = get_log_format(ConfigDB),
    case httpd_util:lookup(ConfigDB, error_disk_log) of
	undefined ->
	    no_error_log;
	ErrorDiskLog ->
	    Date  = custom_date(),
	    Entry = io_lib:format("[~s] reporting error: ~s",[Date,Error]),
	    write(ErrorDiskLog, Entry, Format),
	    ok
    end.

%%----------------------------------------------------------------------
%% Get the current format of the disklog
%%----------------------------------------------------------------------
get_log_format(ConfigDB)->
    httpd_util:lookup(ConfigDB,disk_log_format,external).


%%
%% Configuration
%%

%% load

load([$T,$r,$a,$n,$s,$f,$e,$r,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ |
      TransferDiskLogSize],[]) ->
    case regexp:split(TransferDiskLogSize," ") of
	{ok,[MaxBytes,MaxFiles]} ->
	    case httpd_conf:make_integer(MaxBytes) of
		{ok,MaxBytesInteger} ->
		    case httpd_conf:make_integer(MaxFiles) of
			{ok,MaxFilesInteger} ->
			    {ok,[],{transfer_disk_log_size,
				    {MaxBytesInteger,MaxFilesInteger}}};
			{error,_} ->
			    {error,
			     ?NICE(httpd_conf:clean(TransferDiskLogSize)++
				   " is an invalid TransferDiskLogSize")}
		    end;
		{error,_} ->
		    {error,?NICE(httpd_conf:clean(TransferDiskLogSize)++
				 " is an invalid TransferDiskLogSize")}
	    end
    end;
load([$T,$r,$a,$n,$s,$f,$e,$r,$D,$i,$s,$k,$L,$o,$g,$ |TransferDiskLog],[]) ->
    {ok,[],{transfer_disk_log,httpd_conf:clean(TransferDiskLog)}};

load([$E,$r,$r,$o,$r,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ | ErrorDiskLogSize],[]) ->
    case regexp:split(ErrorDiskLogSize," ") of
	{ok,[MaxBytes,MaxFiles]} ->
	    case httpd_conf:make_integer(MaxBytes) of
		{ok,MaxBytesInteger} ->
		    case httpd_conf:make_integer(MaxFiles) of
			{ok,MaxFilesInteger} ->
			    {ok,[],{error_disk_log_size,
				    {MaxBytesInteger,MaxFilesInteger}}};
			{error,_} ->
			    {error,?NICE(httpd_conf:clean(ErrorDiskLogSize)++
					 " is an invalid ErrorDiskLogSize")}
		    end;
		{error,_} ->
		    {error,?NICE(httpd_conf:clean(ErrorDiskLogSize)++
				 " is an invalid ErrorDiskLogSize")}
	    end
    end;
load([$E,$r,$r,$o,$r,$D,$i,$s,$k,$L,$o,$g,$ |ErrorDiskLog],[]) ->
    {ok, [], {error_disk_log, httpd_conf:clean(ErrorDiskLog)}};

load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ |SecurityDiskLogSize],[]) ->
    case regexp:split(SecurityDiskLogSize, " ") of
	{ok, [MaxBytes, MaxFiles]} ->
	    case httpd_conf:make_integer(MaxBytes) of
		{ok, MaxBytesInteger} ->
		    case httpd_conf:make_integer(MaxFiles) of
			{ok, MaxFilesInteger} ->
			    {ok, [], {security_disk_log_size,
				      {MaxBytesInteger, MaxFilesInteger}}};
			{error,_} ->
			    {error, ?NICE(httpd_conf:clean(SecurityDiskLogSize)++
					  " is an invalid SecurityDiskLogSize")}
		    end;
		{error, _} ->
		    {error, ?NICE(httpd_conf:clean(SecurityDiskLogSize)++
				  " is an invalid SecurityDiskLogSize")}
	    end
    end;
load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$i,$s,$k,$L,$o,$g,$ |SecurityDiskLog],[]) ->
    {ok, [], {security_disk_log, httpd_conf:clean(SecurityDiskLog)}};

load([$D,$i,$s,$k,$L,$o,$g,$F,$o,$r,$m,$a,$t,$ |Format],[]) ->
    case httpd_conf:clean(Format) of
	"internal" ->
	    {ok, [], {disk_log_format,internal}};
	"external" ->
	    {ok, [], {disk_log_format,external}};
	_Default ->
	    {ok, [], {disk_log_format,external}}
    end.

%% store

store({transfer_disk_log,TransferDiskLog},ConfigList) ->
    case create_disk_log(TransferDiskLog, transfer_disk_log_size, ConfigList) of
	{ok,TransferDB} ->
	    {ok,{transfer_disk_log,TransferDB}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({security_disk_log,SecurityDiskLog},ConfigList) ->
    case create_disk_log(SecurityDiskLog, security_disk_log_size, ConfigList) of
	{ok,SecurityDB} ->
	    {ok,{security_disk_log,SecurityDB}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({error_disk_log,ErrorDiskLog},ConfigList) ->
    case create_disk_log(ErrorDiskLog, error_disk_log_size, ConfigList) of
	{ok,ErrorDB} ->
	    {ok,{error_disk_log,ErrorDB}};
	{error,Reason} ->
	    {error,Reason}
    end.


%%----------------------------------------------------------------------
%% Open or creates the disklogs
%%----------------------------------------------------------------------
log_size(ConfigList, Tag) ->
    httpd_util:key1search(ConfigList, Tag, {500*1024,8}).

create_disk_log(LogFile, SizeTag, ConfigList) ->
    Filename = httpd_conf:clean(LogFile),
    {MaxBytes, MaxFiles} = log_size(ConfigList, SizeTag),
    case filename:pathtype(Filename) of
	absolute ->
	    create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList);
	volumerelative ->
	    create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList);
	relative ->
	    case httpd_util:key1search(ConfigList,server_root) of
		undefined ->
		    {error,
		     ?NICE(Filename++
			   " is an invalid ErrorLog beacuse ServerRoot is not defined")};
		ServerRoot ->
		    AbsoluteFilename = filename:join(ServerRoot,Filename),
		    create_disk_log(AbsoluteFilename, MaxBytes, MaxFiles,
				     ConfigList)
	    end
    end.

create_disk_log(Filename, MaxBytes, MaxFiles, ConfigList) ->
    Format = httpd_util:key1search(ConfigList, disk_log_format, external),
    open(Filename, MaxBytes, MaxFiles, Format).



%% remove
remove(ConfigDB) ->
    lists:foreach(fun([DiskLog]) -> close(DiskLog) end,
		  ets:match(ConfigDB,{transfer_disk_log,'$1'})),
    lists:foreach(fun([DiskLog]) -> close(DiskLog) end,
		  ets:match(ConfigDB,{error_disk_log,'$1'})),
    ok.


%%
%% Some disk_log wrapper functions:
%%

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
    Opts = [{format, internal}, {repair, truncate}],
    open1(Filename, MaxBytes, MaxFiles, Opts);
open(Filename, MaxBytes, MaxFiles, _) ->
    Opts = [{format, external}],
    open1(Filename, MaxBytes, MaxFiles, Opts).

open1(Filename, MaxBytes, MaxFiles, Opts0) ->
    Opts1 = [{name, Filename}, {file, Filename}, {type, wrap}] ++ Opts0,
    case open2(Opts1, {MaxBytes, MaxFiles}) of
        {ok, LogDB} ->
            {ok, LogDB};
        {error, Reason} ->
            ?vlog("failed opening disk log with args:"
                  "~n   Filename: ~p"
                  "~n   MaxBytes: ~p"
                  "~n   MaxFiles: ~p"
                  "~n   Opts0:    ~p"
                  "~nfor reason:"
                  "~n   ~p", [Filename, MaxBytes, MaxFiles, Opts0, Reason]),
            {error,
             ?NICE("Can't create " ++ Filename ++
                   lists:flatten(io_lib:format(", ~p",[Reason])))};
        _ ->
            {error, ?NICE("Can't create "++Filename)}
    end.

open2(Opts, Size) ->
    case disk_log:open(Opts) of
        {error, {badarg, size}} ->
            %% File did not exist, add the size option and try again
            disk_log:open([{size, Size} | Opts]);
        Else ->
            Else
    end.


%%----------------------------------------------------------------------
%% Actually writes the entry to the disk_log. If the log is an
%% internal disk_log write it with log otherwise with blog.
%%----------------------------------------------------------------------
write(Log, Entry, internal) ->
    disk_log:log(Log, Entry);

write(Log, Entry, _) ->
    disk_log:blog(Log, Entry).

%% Close the log file
close(Log) ->
    disk_log:close(Log).
