%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(httpd_log).

-include("httpd.hrl").

-export([access_entry/8, error_entry/5, error_report_entry/5, 
	 security_entry/5]).


%%%=========================================================================
%%%  Internal Application API 
%%%=========================================================================

-spec access_entry(Log        :: term(), % Id of the log
		   NoLog      :: term(), % What to return when no log is found
		   Info       :: #mod{},
		   RFC931     :: string(),
		   AuthUser   :: string(), 
		   Date       :: string(), 
		   StatusCode :: pos_integer(),
		   Size       :: 0 | pos_integer() | string()) ->
			  {Log :: atom() | pid(), Entry :: string()} | term() .

%% Somethime the size in the form of the content_length is put here, which
%% is actually in the form of a string
%% So it can either be the size as an integer, the size as a string
%% or, worst case scenario, bytes.
access_entry(Log, NoLog, Info, RFC931, AuthUser, Date, StatusCode, 
	     SizeStrOrBytes) 
  when is_list(SizeStrOrBytes) ->
    Size = 
	case (catch list_to_integer(SizeStrOrBytes)) of
	    I when is_integer(I) ->
		%% This is from using the content_length (which is a string)
		I;
	    _ ->
		%% This is better than nothing
		httpd_util:flatlength(SizeStrOrBytes) 
	end,
    access_entry(Log, NoLog, Info, RFC931, AuthUser, Date, StatusCode, Size);
access_entry(Log, NoLog, 
	     #mod{config_db     = ConfigDB, 
		  init_data     = #init_data{peername = {_, RemoteHost}}, 
		  request_line  = RequestLine,
		  parsed_header = Headers}, 
	     RFC931, AuthUser, Date, StatusCode, Size) ->
    MakeEntry = 
	fun() ->
		do_access_entry(ConfigDB, Headers, RequestLine, 
				RemoteHost, RFC931, AuthUser,
				Date, StatusCode, Size)
	end,
    log_entry(Log, NoLog, ConfigDB, MakeEntry).


-spec error_entry(Log    :: term(), % Id of the log
		  NoLog  :: term(), % What to return when no log is found
		  Info   :: #mod{},
		  Date   :: string(), 
		  Reason :: term()) ->
			 {Log :: atom() | pid(), Entry :: string()} | term().

error_entry(Log, NoLog, 
	    #mod{config_db   = ConfigDB,
		 init_data   = #init_data{peername = {_, RemoteHost}}, 
		 request_uri = URI}, Date, Reason) ->
    MakeEntry = 
	fun() ->
		do_error_entry(ConfigDB, RemoteHost, URI, Date, Reason)	
	end,
    log_entry(Log, NoLog, ConfigDB, MakeEntry).


-spec error_report_entry(Log      :: term(), 
			 NoLog    :: term(), 
			 ConfigDB :: term(),
			 Date     :: string(), 
			 ErrroStr :: string()) ->
    {Log :: atom() | pid(), Entry :: string()} | term().

error_report_entry(Log, NoLog, ConfigDb, Date, ErrorStr) ->
    MakeEntry = fun() -> io_lib:format("[~s], ~s~n", [Date, ErrorStr]) end,
    log_entry(Log, NoLog, ConfigDb, MakeEntry).


-spec security_entry(Log      :: term(), 
		     NoLog    :: term(), 
		     ConfigDB :: term(),
		     Date     :: string(), 
		     Reason   :: term()) ->
    {Log :: atom() | pid(), Entry :: string()} | term().

security_entry(Log, NoLog, #mod{config_db = ConfigDB}, Date, Reason) ->
    MakeEntry = fun() -> io_lib:format("[~s] ~s~n", [Date, Reason]) end,
    log_entry(Log, NoLog, ConfigDB, MakeEntry).


log_entry(Log, NoLog, ConfigDb, MakeEntry) when is_function(MakeEntry) ->
    case httpd_util:lookup(ConfigDb, Log) of
	undefined ->
	    NoLog;
	LogRef ->
	    {LogRef, MakeEntry()}
    end.
   
    
%%%========================================================================
%%% Internal functions
%%%========================================================================

do_access_entry(ConfigDB, Headers, RequestLine,
		RemoteHost, RFC931, AuthUser, Date, StatusCode,
		Size) ->
    case httpd_util:lookup(ConfigDB, log_format, common) of
	common ->
	    lists:flatten(io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w~n",
			  [RemoteHost, RFC931, AuthUser, Date,
			   RequestLine, 
			   StatusCode, Size]));
	combined ->
	    Referer = 
		proplists:get_value("referer", Headers, "-"),
	    UserAgent = 
		proplists:get_value("user-agent", 
				    Headers, "-"),
	    io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w ~s ~s~n",
			  [RemoteHost, RFC931, AuthUser, Date,
			   RequestLine, StatusCode, Size, 
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
