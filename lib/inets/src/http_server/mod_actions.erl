%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mod_actions).
-export([do/1,load/2, store/2]).

-include("httpd.hrl").
-include("httpd_internal.hrl").

%% do

do(Info) ->
  case proplists:get_value(status, Info#mod.data) of
    %% A status code has been generated!
    {_StatusCode, _PhraseArgs, _Reason} ->
      {proceed,Info#mod.data};
    %% No status code has been generated!
    undefined ->
      case proplists:get_value(response, Info#mod.data) of
	%% No response has been generated!
	undefined ->
	  Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			      Info#mod.request_uri),
	  Suffix = httpd_util:suffix(Path),
	  MimeType = httpd_util:lookup_mime(Info#mod.config_db,Suffix,
					  "text/plain"),
	  Actions = httpd_util:multi_lookup(Info#mod.config_db,action),
	  case action(Info#mod.request_uri,MimeType,Actions) of
	    {yes, RequestURI} ->
	      {proceed, [{new_request_uri, RequestURI} | Info#mod.data]};
	    no ->
	      Scripts = httpd_util:multi_lookup(Info#mod.config_db, script),
	      case script(Info#mod.request_uri, Info#mod.method, Scripts) of
		{yes, RequestURI} ->
		  {proceed,[{new_request_uri, RequestURI} | Info#mod.data]};
		no ->
		  {proceed, Info#mod.data} 
	      end
	  end;
	%% A response has been generated or sent!
	_Response ->
	  {proceed, Info#mod.data}
      end
  end.

action(_RequestURI, _MimeType, []) ->
  no;
action(RequestURI, MimeType, [{MimeType, CGIScript} | _Rest]) ->
  {yes, CGIScript ++ RequestURI};
action(RequestURI, MimeType, [_ | Rest]) ->
  action(RequestURI, MimeType, Rest).

script(_RequestURI, _Method, []) ->
  no;
script(RequestURI, Method, [{Method, CGIScript} | _Rest]) ->
  {yes, CGIScript ++ RequestURI};
script(RequestURI, Method, [_ | Rest]) ->
  script(RequestURI, Method, Rest).

%%
%% Configuration
%%

%% load

load("Action "++  Action, []) ->
  case re:split(Action, " ", [{return, list}]) of
      [MimeType, CGIScript] ->
	  {ok,[],{action, {MimeType, CGIScript}}};
      _ ->
	  {error,?NICE(string:strip(Action)++" is an invalid Action")}
  end;
load("Script " ++ Script,[]) ->
  case re:split(Script, " ", [{return, list}]) of
      [Method, CGIScript] ->
	  {ok,[],{script, {Method, CGIScript}}};
      _ ->
	  {error,?NICE(string:strip(Script)++" is an invalid Script")}
  end.

store({action, {MimeType, CGIScript}} = Conf, _) when is_list(MimeType),
						      is_list(CGIScript)  ->
    {ok, Conf};
store({action, Value}, _) ->
    {error, {wrong_type, {action, Value}}};

store({script, {Method, CGIScript}} = Conf, _) when is_list(Method),
						    is_list(CGIScript) ->
    case string:to_lower(Method) of
	"get" ->
	    {ok, Conf};
	"post" ->
	    {ok, Conf};
	_ ->
	    {error, {wrong_type, Conf}}
    end;

store({script, Value}, _) ->
    {error, {wrong_type, {script, Value}}}.



