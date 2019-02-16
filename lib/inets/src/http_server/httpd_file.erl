%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
handle_error(eisdir, Op, ModData, Path) ->
    handle_error(403, Op, ModData, Path,
	         ":Ilegal operation expected a file not a directory");
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
