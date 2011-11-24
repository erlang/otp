%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
-module(httpd_file).

-export([handle_error/4]).

-include("httpd.hrl").
-include("httpd_internal.hrl").


handle_error(eacces, Op, ModData, Path) ->
    handle_error(403, Op, ModData, Path, ": Forbidden");
handle_error(enoent, Op, ModData, Path) ->
    handle_error(404, Op, ModData, Path, ": File not found");
handle_error(enotdir, Op, ModData, Path) ->
    handle_error(404, Op, ModData, Path,
	         ": A component of the file name is not a directory");
handle_error(emfile, Op, _ModData, Path) ->
    handle_error(500, Op, none, Path, ": Too many open files");
handle_error({enfile,_}, Op, _ModData, Path) ->
    handle_error(500, Op, none, Path, ": File table overflow");
handle_error(_Reason, Op, _ModData, Path) ->
    handle_error(500, Op, none, Path, "").
	    
handle_error(StatusCode, Op, none, Path, Reason) ->
    {StatusCode, none, ?NICE("Can't " ++ Op ++ " " ++ Path ++ Reason)};
handle_error(StatusCode, Op, ModData, Path, Reason) ->
    {StatusCode, ModData#mod.request_uri,
     ?NICE("Can't " ++ Op ++ " " ++ Path ++ Reason)}.
