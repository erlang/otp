%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(toolbar_lib).
-include_lib("kernel/include/file.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Toolbar
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library functions
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([gs_contribs_dir/0,help_file/0,otp_file/0]).
-export([error_string/1]).
-export([legal_file/1]).
-export([insert_newlines/1]).
-export([tool_info_syntax/2]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Exported functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% gs_contribs_dir() => string()
% Return the directory of the GS contributions
%----------------------------------------
gs_contribs_dir() ->
    filename:join(code:lib_dir(gs),"contribs/ebin/").

%----------------------------------------
% help_file() => string()
% Returns the address to the toolbar help file
%----------------------------------------
help_file() ->
    filename:join([code:lib_dir(toolbar),"doc", "html", "index.html"]).

%----------------------------------------
% otp_file() => string()
% Returns the address to the OTP documentation
%----------------------------------------
otp_file() ->
    filename:join([code:root_dir(),"doc", "index.html"]).

%----------------------------------------
% error_string(Reason) => string()
%   Reason - nofile | format | noname | nostart | illegal | raccess | waccess
% Given Reason, returns a short "explanation string"
%----------------------------------------
error_string(nofile) -> "File does not exist";
error_string(format) -> "File on wrong format";

error_string(noname) -> "No tool name is specified";
error_string(nostart) -> "No start function is specified";

error_string(illegal) -> "Illegal file name";

error_string(raccess) ->  "File is not readable";
error_string(waccess) ->  "File is not writeable".

%----------------------------------------
% legal_file(File) => ok | directory | {error,nofile} | {error,raccess}
%   File - string() File name
% Checks if File is an existing and readable file.
%----------------------------------------
legal_file(File) ->
    case file:read_file_info(File) of

	%% File exists...
	{ok,#file_info{type=regular,access=Access}} ->
	    if
		
		%% ...but is read protected
		Access/=read,
		Access/=read_write ->
		    {error,raccess};
		
		%% ...and is possible to read
		true ->
		    ok
	    end;

	{ok,#file_info{type=directory}} ->
	    directory;
	
	%% File does not exist
	_Error ->
	    {error,nofile}
    end.

%----------------------------------------
% insert_newlines(Strings) => string()
%   Strings - string() | [string()]
% If Strings is a list of strings, return a string where all these strings
% are concatenated with newlines in between, otherwise return Strings.
%----------------------------------------
insert_newlines([String|Rest]) when is_list(String), Rest/=[]->
    String ++ "\n" ++ insert_newlines(Rest);
insert_newlines([Last]) ->
    [Last];
insert_newlines(Other) ->
    Other.

%----------------------------------------
% tool_info_syntax(Version,ToolInfo) => {ok,CorrToolInfo} | {error,Reason}
%   Version - string()
%   ToolInfo - tuple()
%   CorrToolInfo - list()
%   Reason - version | format | noname | nostart
% Return a corrected (blanks removed etc) version of ToolInfo
% if the syntax of ToolInfo is correct (ie contains all
% mandatory elements and all values are of the specified type).
%
% Currently accepted versions:
%   "0.1" (which should be "1.0")
%   "1.1" (same as "0.1")
%----------------------------------------
tool_info_syntax("1.1",ToolInfo) ->
    tool_info_syntax("0.1",ToolInfo);
tool_info_syntax("0.1",ToolInfo) when is_tuple(ToolInfo) ->
    syntax01(tuple_to_list(ToolInfo),false,false,[]);
tool_info_syntax("0.1",_) ->
    {error,format};
tool_info_syntax("1.2",ToolInfo) when is_list(ToolInfo)->
    syntax01(ToolInfo,false,false,[]);
tool_info_syntax(_Vsn,_) ->
    {error,version}.


%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% syntax01(List,NameF,StartF,Res) => {ok,Res} | {error,Reason}
%   List - [tuple()]
%   NameF, StartF - boolean()
%   Res - [tuple()]
%   Reason - format | noname | nostart
% Version 0.1 syntax check of .tool file.
%----------------------------------------
syntax01([{tool,Str}|Rest],false,StartF,Res) when is_list(Str) ->
    case string:strip(Str) of
	[] ->
	    {error,format};
	Tool ->
	    syntax01(Rest,true,StartF,[{tool,Tool}|Res])
    end;
syntax01([{start,{M,F,A}}|Rest],NameF,false,Res) when is_atom(M),
						      is_atom(F),
						      is_list(A) ->
    syntax01(Rest,NameF,true,[{start,{M,F,A}}|Res]);
syntax01([{icon,Str}|Rest],NameF,StartF,Res) when is_list(Str) ->
    case string:strip(Str) of
	[] ->
	    syntax01(Rest,NameF,StartF,Res);
	Icon ->
	    syntax01(Rest,NameF,StartF,[{icon,Icon}|Res])
    end;
syntax01([{message,Str}|Rest],NameF,StartF,Res) when is_list(Str) ->
    case string:strip(Str) of
	[] ->
	    syntax01(Rest,NameF,StartF,Res);
	Message ->
	    syntax01(Rest,NameF,StartF,
		     [{message,lists:sublist(Message,1,30)}|Res])
    end;
syntax01([{html,Str}|Rest],NameF,StartF,Res) when is_list(Str) ->
    case string:strip(Str) of
	[] ->
	    syntax01(Rest,NameF,StartF,Res);
	Html ->
	    syntax01(Rest,NameF,StartF,[{html,Html}|Res])
    end;
%%The fields used by webtool must be removed
syntax01([_|Rest],NameF,StartF,Res) ->
    syntax01(Rest,NameF,StartF,Res);

syntax01([],true,true,Res) ->
    {ok,Res};
syntax01([],false,_,_) ->
    {error,noname};
syntax01([],_,false,_) ->
    {error,nostart};
syntax01(_,_,_,_) ->
    {error,format}.





















