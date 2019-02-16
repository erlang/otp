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
-module(mod_log).

%% Application internal API
-export([error_log/2, security_log/2, report_error/2]).

%% Callback API
-export([do/1, load/2, store/2, remove/1]).

-include("httpd.hrl").
-include("httpd_internal.hrl").
-define(VMODULE,"LOG").

%%%=========================================================================
%%%  API 
%%%=========================================================================

%% security log
security_log(Info, ReasonStr) ->
    Date = httpd_util:custom_date(),
    case httpd_log:security_entry(security_log, no_security_log, Info,
				 Date, ReasonStr) of
	no_security_log ->
	    ok;
	{Log, Entry} ->
	    io:format(Log, "~s", [Entry])
    end.

%% error_log
error_log(Info, Reason) ->
    Date = httpd_util:custom_date(),
    error_log(Info, Date, Reason).

error_log(Info, Date, Reason) ->
    case httpd_log:error_entry(error_log, no_error_log, 
			       Info, Date, Reason) of
	no_error_log ->
	    ok;
	{Log, Entry} ->
	    io:format(Log, "~s", [Entry])
    end.

report_error(ConfigDB, Error) ->
    Date = httpd_util:custom_date(),
    case httpd_log:error_report_entry(error_log, no_error_log, ConfigDB,
				      Date, Error) of
	no_error_log ->
	    ok;
	{Log, Entry} ->
	    io:format(Log, "~s", [Entry])
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
    AuthUser = auth_user(Info#mod.data),
    Date     = httpd_util:custom_date(),
    log_internal_info(Info,Date,Info#mod.data),
    case proplists:get_value(status, Info#mod.data) of
	%% A status code has been generated!
	{StatusCode, _PhraseArgs, Reason} ->
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
	    case proplists:get_value(response, Info#mod.data) of
		{already_sent,StatusCode,Size} ->
		    transfer_log(Info,"-",AuthUser,Date,StatusCode,Size),
		    {proceed,Info#mod.data};
		{response, Head, _Body} ->
                    Size = content_length(Head),
		    Code = proplists:get_value(code,Head,unknown),
		    transfer_log(Info, "-", AuthUser, Date, Code, Size),
		    {proceed, Info#mod.data};
		{StatusCode, Response} ->
		    transfer_log(Info, "-", AuthUser, Date, StatusCode,
				 httpd_util:flatlength(Response)),
		    {proceed,Info#mod.data};
		undefined ->
		    transfer_log(Info,"-",AuthUser,Date,200,0),
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
load("TransferLog " ++ TransferLog, []) ->
    {ok,[],{transfer_log,string:strip(TransferLog)}};
load("ErrorLog " ++ ErrorLog, []) ->
    {ok,[],{error_log,string:strip(ErrorLog)}};
load("SecurityLog " ++ SecurityLog, []) ->
    {ok, [], {security_log, string:strip(SecurityLog)}}.

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
store({transfer_log,TransferLog}, ConfigList) when is_list(TransferLog)->
    case create_log(TransferLog,ConfigList) of
	{ok,TransferLogStream} ->
	    {ok,{transfer_log,TransferLogStream}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({transfer_log,TransferLog}, _) ->
    {error, {wrong_type, {transfer_log, TransferLog}}};
store({error_log,ErrorLog}, ConfigList) when is_list(ErrorLog) ->
    case create_log(ErrorLog,ConfigList) of
	{ok,ErrorLogStream} ->
	    {ok,{error_log,ErrorLogStream}};
	{error,Reason} ->
	    {error,Reason}
    end;
store({error_log,ErrorLog}, _) ->
    {error, {wrong_type, {error_log, ErrorLog}}};
store({security_log, SecurityLog}, ConfigList) when is_list(SecurityLog) ->
    case create_log(SecurityLog, ConfigList) of
	{ok, SecurityLogStream} ->
	    {ok, {security_log, SecurityLogStream}};
	{error, Reason} ->
	    {error, Reason}
    end;
store({security_log, SecurityLog}, _) ->
    {error, {wrong_type, {security_log, SecurityLog}}}.

%%--------------------------------------------------------------------------
%% remove(ConfigDb) -> _
%%
%% Description: See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
remove(ConfigDB) ->
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{transfer_log,'$1'})),
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{error_log,'$1'})),
    lists:foreach(fun([Stream]) -> file:close(Stream) end,
		  ets:match(ConfigDB,{security_log,'$1'})),
    ok.

%%%========================================================================
%%% Internal functions
%%%======================================================================== 
%% transfer_log
transfer_log(Info,RFC931,AuthUser,Date,StatusCode,Bytes) ->
    case httpd_log:access_entry(transfer_log, no_transfer_log,
				Info, RFC931, AuthUser, Date, 
				StatusCode, Bytes) of
	no_transfer_log ->
	    ok;
	{Log, Entry} ->
	    io:format(Log, "~s", [Entry])
    end.
	    
create_log(LogFile, ConfigList) ->
    Filename = string:strip(LogFile),
    case filename:pathtype(Filename) of
	absolute ->
	    case file:open(Filename, [read, write]) of
		{ok,LogStream} ->
		    file:position(LogStream,{eof,0}),
		    {ok,LogStream};
		{error,_} ->
		    {error,?NICE("Can't create "++Filename)}
	    end;
	volumerelative ->
	    case file:open(Filename, [read, write]) of
		{ok,LogStream} ->
		    file:position(LogStream,{eof,0}),
		    {ok,LogStream};
		{error,_} ->
		    {error,?NICE("Can't create "++Filename)}
	    end;
	relative ->
	    case proplists:get_value(server_root,ConfigList) of
		undefined ->
		    {error,
		     ?NICE(Filename++
			   " is an invalid logfile name beacuse "
			   "ServerRoot is not defined")};
		ServerRoot ->
		    AbsoluteFilename=filename:join(ServerRoot,Filename),
		    case file:open(AbsoluteFilename, [read, write]) of
			{ok,LogStream} ->
			    file:position(LogStream,{eof,0}),
			    {ok,LogStream};
			{error, _Reason} ->
			    {error,?NICE("Can't create "++AbsoluteFilename)}
		    end
	    end
    end.

%% log_internal_info
log_internal_info(_Info, _Date, []) ->
    ok;
log_internal_info(Info,Date,[{internal_info,Reason}|Rest]) ->
    error_log(Info, Date, Reason),
    log_internal_info(Info,Date,Rest);
log_internal_info(Info,Date,[_|Rest]) ->
    log_internal_info(Info,Date,Rest).

auth_user(Data) ->
    case proplists:get_value(remote_user, Data) of
	undefined ->
	    "-";
	RemoteUser ->
	    RemoteUser
    end.

content_length(Head) ->
    case proplists:get_value(content_length, Head) of
        undefined ->
            unknown;
        Size ->
            list_to_integer(Size)
    end.
