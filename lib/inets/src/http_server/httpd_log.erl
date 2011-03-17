%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%%
-module(httpd_log).

-include("httpd.hrl").

-export([access_entry/8, error_entry/5, error_report_entry/5, 
	 security_entry/5]).

%%%=========================================================================
%%%  Internal Application API 
%%%=========================================================================
access_entry(Log, NoLog, Info, RFC931, AuthUser, Date, StatusCode, Bytes) ->
    ConfigDB = Info#mod.config_db,
    case httpd_util:lookup(ConfigDB, Log) of
	undefined ->
	    NoLog;
	LogRef ->
	    {_, RemoteHost}
		= (Info#mod.init_data)#init_data.peername,
	    RequestLine = Info#mod.request_line,
	    Headers =  Info#mod.parsed_header,
	    Entry = do_access_entry(ConfigDB, Headers, RequestLine, 
				    RemoteHost, RFC931, AuthUser,
				    Date, StatusCode, Bytes),
	    {LogRef, Entry}
    end.

error_entry(Log, NoLog, Info, Date, Reason) ->
    ConfigDB = Info#mod.config_db,
    case httpd_util:lookup(ConfigDB, Log) of
	undefined ->
	    NoLog;
	LogRef ->
	    {_, RemoteHost} =
		(Info#mod.init_data)#init_data.peername,
	    URI = Info#mod.request_uri,
	    Entry = do_error_entry(ConfigDB, RemoteHost, URI, Date, Reason), 
	    {LogRef, Entry}
    end.

error_report_entry(Log, NoLog, ConfigDb, Date, ErrorStr) ->
    case httpd_util:lookup(ConfigDb, Log) of
	undefined ->
	    NoLog;
	LogRef ->
	     Entry = io_lib:format("[~s], ~s~n", [Date, ErrorStr]),
	     {LogRef, Entry}
     end.

security_entry(Log, NoLog, #mod{config_db = ConfigDb}, Date, Reason) ->
    case httpd_util:lookup(ConfigDb, Log) of
	undefined ->
	    NoLog;
	LogRef ->
	    Entry = io_lib:format("[~s] ~s~n", [Date, Reason]),
	    {LogRef, Entry}
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
do_access_entry(ConfigDB, Headers, RequestLine,
	     RemoteHost, RFC931, AuthUser, Date, StatusCode,
	     Bytes) ->
    case httpd_util:lookup(ConfigDB, log_format, common) of
	common ->
	    lists:flatten(io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w~n",
			  [RemoteHost, RFC931, AuthUser, Date,
			   RequestLine, 
			   StatusCode, Bytes]));
	combined ->
	    Referer = 
		proplists:get_value("referer", Headers, "-"),
	    UserAgent = 
		proplists:get_value("user-agent", 
				    Headers, "-"),
	    io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w ~s ~s~n",
			  [RemoteHost, RFC931, AuthUser, Date,
			   RequestLine, StatusCode, Bytes, 
			   Referer, UserAgent])
    end.


do_error_entry(ConfigDB, RemoteHost, undefined, Date, Reason) ->
     case httpd_util:lookup(ConfigDB, error_log_format, pretty) of
	pretty ->
	   io_lib:format("[~s] server crash for ~s, reason: ~n~p~n~n", 
			 [Date, RemoteHost, Reason]);
	compact ->
	   io_lib:format("[~s] server crash for ~s, reason: ~w~n", 
			 [Date, RemoteHost, Reason])
	     
    end;
    
do_error_entry(ConfigDB, RemoteHost, URI, Date, Reason) ->
    case httpd_util:lookup(ConfigDB, error_log_format, pretty) of
	pretty ->
	   io_lib:format("[~s] access to ~s failed for ~s, reason: ~n~p~n",
			 [Date, URI, RemoteHost, Reason]);
	compact ->
	   io_lib:format( "[~s] access to ~s failed for ~s, reason: ~w~n", 
			 [Date, URI, RemoteHost, Reason])	     
    end.
