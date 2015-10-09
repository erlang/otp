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
%%     $Id: mod_log.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%
-module(mod_log).
-export([do/1,error_log/5,security_log/2,load/2,store/2,remove/1]).

-export([report_error/2]).

-include("httpd.hrl").

-define(VMODULE,"LOG").
-include("httpd_verbosity.hrl").

%% do

do(Info) ->
    AuthUser = auth_user(Info#mod.data),
    Date     = custom_date(),
    log_internal_info(Info,Date,Info#mod.data),
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    transfer_log(Info,"-",AuthUser,Date,StatusCode,0),
	    if
		StatusCode >= 400 ->
		    error_log(Info,Date,Reason);
		true ->
		    not_an_error
	    end,
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		{already_sent,StatusCode,Size} ->
		    transfer_log(Info,"-",AuthUser,Date,StatusCode,Size),
		    {proceed,Info#mod.data};
		{response,Head,Body} ->
		    Size=httpd_util:key1search(Head,content_length,unknown),
		    Code=httpd_util:key1search(Head,code,unknown),
		    transfer_log(Info,"-",AuthUser,Date,Code,Size),
		    {proceed,Info#mod.data};
		{StatusCode,Response} ->
		    transfer_log(Info,"-",AuthUser,Date,200,
				 httpd_util:flatlength(Response)),
		    {proceed,Info#mod.data};
		undefined ->
		    transfer_log(Info,"-",AuthUser,Date,200,0),
		    {proceed,Info#mod.data}
	    end
    end.

custom_date() ->
    LocalTime=calendar:local_time(),
    UniversalTime=calendar:universal_time(),
    Minutes=round(diff_in_minutes(LocalTime,UniversalTime)),
    {{YYYY,MM,DD},{Hour,Min,Sec}}=LocalTime,
    Date =
	io_lib:format("~.2.0w/~.3s/~.4w:~.2.0w:~.2.0w:~.2.0w ~c~.2.0w~.2.0w",
		      [DD, httpd_util:month(MM), YYYY, Hour, Min, Sec,
		       sign(Minutes),
		       abs(Minutes) div 60, abs(Minutes) rem 60]),
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
    error_log(Info,Date,Reason),
    log_internal_info(Info,Date,Rest);
log_internal_info(Info,Date,[_|Rest]) ->
    log_internal_info(Info,Date,Rest).

%% transfer_log

transfer_log(Info,RFC931,AuthUser,Date,StatusCode,Bytes) ->
    case httpd_util:lookup(Info#mod.config_db,transfer_log) of
	undefined ->
	    no_transfer_log;
	TransferLog ->
	    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
	    case (catch io:format(TransferLog, "~s ~s ~s [~s] \"~s\" ~w ~w~n",
				  [RemoteHost, RFC931, AuthUser,
				   Date, Info#mod.request_line,
				   StatusCode, Bytes])) of
		ok ->
		    ok;
		Error ->
		    error_logger:error_report(Error)
	    end
    end.

%% security log

security_log(Info, Reason) ->
    case httpd_util:lookup(Info#mod.config_db, security_log) of
	undefined ->
	    no_security_log;
	SecurityLog ->
	    io:format(SecurityLog,"[~s] ~s~n", [custom_date(), Reason])
    end.

%% error_log

error_log(Info,Date,Reason) ->
    case httpd_util:lookup(Info#mod.config_db, error_log) of
	undefined ->
	    no_error_log;
	ErrorLog ->
	    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
	    io:format(ErrorLog,"[~s] access to ~s failed for ~s, reason: ~p~n",
		      [Date,Info#mod.request_uri,RemoteHost,Reason])
    end.

error_log(SocketType,Socket,ConfigDB,{PortNumber,RemoteHost},Reason) ->
    case httpd_util:lookup(ConfigDB,error_log) of
	undefined ->
	    no_error_log;
	ErrorLog ->
	    Date=custom_date(),
	    io:format(ErrorLog,"[~s] server crash for ~s, reason: ~p~n",
		      [Date,RemoteHost,Reason]),
	    ok
    end.

report_error(ConfigDB,Error) ->
    case httpd_util:lookup(ConfigDB,error_log) of
	undefined ->
	    no_error_log;
	ErrorLog ->
	    Date=custom_date(),
	    io:format(ErrorLog,"[~s] reporting error: ~s~n",[Date,Error]),
	    ok
    end.

%%
%% Configuration
%%

%% load

load([$T,$r,$a,$n,$s,$f,$e,$r,$L,$o,$g,$ |TransferLog],[]) ->
    {ok,[],{transfer_log,httpd_conf:clean(TransferLog)}};
load([$E,$r,$r,$o,$r,$L,$o,$g,$ |ErrorLog],[]) ->
    {ok,[],{error_log,httpd_conf:clean(ErrorLog)}};
load([$S,$e,$c,$u,$r,$i,$t,$y,$L,$o,$g,$ |SecurityLog], []) ->
    {ok, [], {security_log, httpd_conf:clean(SecurityLog)}}.

%% store

store({transfer_log,TransferLog},ConfigList) ->
    case create_log(TransferLog,ConfigList) of
	{ok,TransferLogStream} ->
	    {ok,{transfer_log,TransferLogStream}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({error_log,ErrorLog},ConfigList) ->
    case create_log(ErrorLog,ConfigList) of
	{ok,ErrorLogStream} ->
	    {ok,{error_log,ErrorLogStream}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({security_log, SecurityLog},ConfigList) ->
    case create_log(SecurityLog, ConfigList) of
	{ok, SecurityLogStream} ->
	    {ok, {security_log, SecurityLogStream}};
	{error, Reason} ->
	    {error, Reason}
    end.

create_log(LogFile,ConfigList) ->
    Filename = httpd_conf:clean(LogFile),
    case filename:pathtype(Filename) of
	absolute ->
	    case file:open(Filename, [read,write]) of
		{ok,LogStream} ->
		    file:position(LogStream,{eof,0}),
		    {ok,LogStream};
		{error,_} ->
		    {error,?NICE("Can't create "++Filename)}
	    end;
	volumerelative ->
	    case file:open(Filename, [read,write]) of
		{ok,LogStream} ->
		    file:position(LogStream,{eof,0}),
		    {ok,LogStream};
		{error,_} ->
		    {error,?NICE("Can't create "++Filename)}
	    end;
	relative ->
	    case httpd_util:key1search(ConfigList,server_root) of
		undefined ->
		    {error,
		     ?NICE(Filename++
			   " is an invalid logfile name beacuse ServerRoot is not defined")};
		ServerRoot ->
		    AbsoluteFilename=filename:join(ServerRoot,Filename),
		    case file:open(AbsoluteFilename, [read,write]) of
			{ok,LogStream} ->
			    file:position(LogStream,{eof,0}),
			    {ok,LogStream};
			{error,Reason} ->
			    {error,?NICE("Can't create "++AbsoluteFilename)}
		    end
	    end
    end.

%% remove

remove(ConfigDB) ->
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{transfer_log,'$1'})),
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{error_log,'$1'})),
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{security_log,'$1'})),
    ok.
