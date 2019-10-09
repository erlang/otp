%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(mod_responsecontrol).
-export([do/1]).

-include("httpd.hrl").
-include("httpd_internal.hrl").

do(Info) ->
    case proplists:get_value(status, Info#mod.data) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, Info#mod.data) of
		%% No response has been generated!
		undefined ->
		    case do_responsecontrol(Info) of
			continue ->
			    {proceed, Info#mod.data};
			Response ->
			    {proceed,[Response | Info#mod.data]}
		    end;
		%% A response has been generated or sent!
		_Response ->
 		    {proceed, Info#mod.data}
	    end
    end.

%%----------------------------------------------------------------------
%%Control that the request header did not contians any limitations 
%%wheather a response shall be createed or not
%%----------------------------------------------------------------------
do_responsecontrol(Info) ->
    Path = mod_alias:path(Info#mod.data, Info#mod.config_db, 
			  Info#mod.request_uri),
    case file:read_file_info(Path) of
	{ok, FileInfo} ->
	    control(Path, Info, FileInfo);
	_ ->
	    %% The requested asset is not a plain file and then it must 
	    %% be generated everytime its requested
	    continue
    end.

%%----------------------------------------------------------------------
%%Control the If-Match, If-None-Match,  and If-Modified-Since    
%%----------------------------------------------------------------------


%% If a client sends more then one of the if-XXXX fields in a request
%% The standard says it does not specify the behaviour so I specified it :-)
%% The priority between the fields is 
%% 1.If-modified
%% 2.If-Unmodified
%% 3.If-Match
%% 4.If-Nomatch

%% This means if more than one of the fields are in the request the 
%% field with highest priority will be used

%%If the request is a range request the If-Range field will be the winner.

control(Path, Info, FileInfo) ->
    case control_range(Path, Info, FileInfo) of
	undefined ->
	    case control_Etag(Path, Info, FileInfo) of
		undefined ->
		    case control_modification(Path, Info, FileInfo) of
			continue ->
			    continue;
			ReturnValue ->
			    send_return_value(ReturnValue, FileInfo)
		    end;
		continue ->
		    continue;
		ReturnValue ->
		    send_return_value(ReturnValue, FileInfo)
	    end;
	Response->
	    Response
    end.

%%----------------------------------------------------------------------
%%If there are both a range and an if-range field control if
%%----------------------------------------------------------------------
control_range(Path,Info,FileInfo) ->
    case proplists:get_value("range", Info#mod.parsed_header) of
	undefined->
	    undefined;
	_Range ->
	    case proplists:get_value("if-range", Info#mod.parsed_header) of
		undefined ->
		    undefined;
		EtagOrDate ->
		    control_if_range(Path,Info,FileInfo,EtagOrDate)
	    end
    end.

control_if_range(_Path, Info, FileInfo, EtagOrDate) ->
    case httpd_util:convert_request_date(strip_date(EtagOrDate)) of
	bad_date ->
	    FileEtag=httpd_util:create_etag(FileInfo),
	    case FileEtag of
		EtagOrDate ->
		    continue;
		_ ->
		    {if_range,send_file}
	    end;
	_ErlDate ->    
	    %%We got the date in the request if it is 
	    case control_modification_data(Info, FileInfo#file_info.mtime,
					   "if-range") of
		modified ->
		    {if_range,send_file};
		_UnmodifiedOrUndefined->
		    continue
	    end
    end.
		 
%%----------------------------------------------------------------------
%%Controls the values of the If-Match and I-None-Mtch
%%----------------------------------------------------------------------
control_Etag(Path, Info, FileInfo)->
    FileEtag = httpd_util:create_etag(FileInfo),
    %%Control if the E-Tag for the resource  matches one of the Etags in
    %%the -if-match header field
    case control_match(Info, FileInfo, "if-match", FileEtag) of
	nomatch ->
	    %%None of the Etags in the if-match field matched the current 
	    %%Etag for the resource return a 304 
	    {412, Info, Path};
	match ->
	    continue;
	undefined ->
	    case control_match(Info, FileInfo, "if-none-match",  FileEtag) of
		nomatch -> 
		    continue;
		match ->
		    case  Info#mod.method of
			"GET" ->
			    {304, Info, Path};
			"HEAD" ->
			    {304, Info, Path};
			_OtherrequestMethod ->
			    {412, Info, Path}
		    end;
		undefined ->
		    undefined
	    end
    end.

%%----------------------------------------------------------------------
%%Control if there are any Etags for HeaderField in the request if so 
%%Control if they match the Etag for the requested file
%%----------------------------------------------------------------------
control_match(Info, _FileInfo, HeaderField, FileEtag)-> 
    case split_etags(proplists:get_value(HeaderField,
					 Info#mod.parsed_header)) of
 	undefined->
	    undefined;
        Etags->
	    %%Control that the match any star not is availible 
	    case lists:member("*",Etags) of
		true-> 
		    match;
		false->
		    compare_etags(FileEtag, Etags)
	    end
    end.

%%----------------------------------------------------------------------
%%Split the etags from the request
%%----------------------------------------------------------------------
split_etags(undefined)->
    undefined;
split_etags(Tags) ->
    string:tokens(Tags,", ").

%%----------------------------------------------------------------------
%%Control if the etag for the file is in the list
%%----------------------------------------------------------------------
compare_etags(Tag,Etags) ->
    case lists:member(Tag,Etags) of
	true ->
	    match;
	_ ->
	    nomatch
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Control if the file is modified                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	    

%%----------------------------------------------------------------------
%% Control the If-Modified-Since and If-Not-Modified-Since header fields
%%----------------------------------------------------------------------
control_modification(Path,Info,FileInfo)->
    case control_modification_data(Info,
				   FileInfo#file_info.mtime,
				   "if-modified-since") of
	modified->
	    continue;	
	unmodified->
	    {304, Info, Path};
	{bad_date, _} = BadDate->
	    {400, Info, BadDate};
	undefined ->
	    case control_modification_data(Info,
					   FileInfo#file_info.mtime,
					   "if-unmodified-since") of
		modified  ->
		    {412, Info, Path};
		_ContinueUndefined ->
		    continue	
	    end
    end.

%%----------------------------------------------------------------------
%%Controls the date from the http-request if-modified-since and 
%%if-not-modified-since against the modification data of the
%%File
%%----------------------------------------------------------------------     
%%Info is the record about the request
%%ModificationTime is the time the file was edited last
%%Header Field is the name of the field  to control

control_modification_data(Info, ModificationTime, HeaderField)-> 
    case strip_date(proplists:get_value(HeaderField, 
					Info#mod.parsed_header)) of
 	undefined->
	    undefined;
	LastModified0 ->
	    case httpd_util:convert_request_date(LastModified0) of
	    	bad_date ->
	    	    {bad_date, LastModified0};
	    	ConvertedReqDate ->
                    LastModified =  calendar:universal_time_to_local_time(ConvertedReqDate),		   
                     FileTime =
			calendar:datetime_to_gregorian_seconds(ModificationTime),
		    FieldTime = 
			calendar:datetime_to_gregorian_seconds(LastModified),
		    if 
			FileTime =< FieldTime ->
			    unmodified;
			FileTime >= FieldTime ->
			    modified	    
		    end
                    
	    end
    end.

%% IE4 & NS4 sends an extra '; length=xxxx' string at the end of the If-Modified-Since
%% header, we detect this and ignore it (the RFCs does not mention this).
strip_date(undefined) ->
    undefined;
strip_date([]) ->
    [];
strip_date([$;,$ | _]) ->
    [];
strip_date([C | Rest]) ->
    [C | strip_date(Rest)].

send_return_value({412,_,_}, _FileInfo)->
    {status,{412,none,"Precondition Failed"}};

send_return_value({400,_, {bad_date, BadDate}}, _FileInfo)->
    {status, {400, none, "Bad date: " ++ BadDate}};

send_return_value({304,Info,Path}, FileInfo)->
    Suffix = httpd_util:suffix(Path),
    MimeType = httpd_util:lookup_mime_default(Info#mod.config_db,Suffix,
					      "text/plain"),
    LastModified =
	case (catch httpd_util:rfc1123_date(FileInfo#file_info.mtime)) of
	    Date when is_list(Date) ->
		[{last_modified, Date}];
	    _ -> %% This will rarly happen, but could happen
		 %% if a computer is wrongly configured. 
		[]
	end,
    
    Header = [{code,304},
	      {etag, httpd_util:create_etag(FileInfo)},
	      {content_length,"0"}, {mime_type, MimeType} | LastModified],
    {response, {response, Header, nobody}}.
