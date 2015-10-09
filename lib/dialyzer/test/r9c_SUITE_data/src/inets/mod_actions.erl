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
%%     $Id: mod_actions.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(mod_actions).
-export([do/1,load/2]).

-include("httpd.hrl").

%% do

do(Info) ->
  case httpd_util:key1search(Info#mod.data,status) of
    %% A status code has been generated!
    {StatusCode,PhraseArgs,Reason} ->
      {proceed,Info#mod.data};
    %% No status code has been generated!
    undefined ->
      case httpd_util:key1search(Info#mod.data,response) of
	%% No response has been generated!
	undefined ->
	  Path=mod_alias:path(Info#mod.data,Info#mod.config_db,
			      Info#mod.request_uri),
	  Suffix=httpd_util:suffix(Path),
	  MimeType=httpd_util:lookup_mime(Info#mod.config_db,Suffix,
					  "text/plain"),
	  Actions=httpd_util:multi_lookup(Info#mod.config_db,action),
	  case action(Info#mod.request_uri,MimeType,Actions) of
	    {yes,RequestURI} ->
	      {proceed,[{new_request_uri,RequestURI}|Info#mod.data]};
	    no ->
	      Scripts=httpd_util:multi_lookup(Info#mod.config_db,script),
	      case script(Info#mod.request_uri,Info#mod.method,Scripts) of
		{yes,RequestURI} ->
		  {proceed,[{new_request_uri,RequestURI}|Info#mod.data]};
		no ->
		  {proceed,Info#mod.data}
	      end
	  end;
	%% A response has been generated or sent!
	Response ->
	  {proceed,Info#mod.data}
      end
  end.

action(RequestURI,MimeType,[]) ->
  no;
action(RequestURI,MimeType,[{MimeType,CGIScript}|Rest]) ->
  {yes,CGIScript++RequestURI};
action(RequestURI,MimeType,[_|Rest]) ->
  action(RequestURI,MimeType,Rest).

script(RequestURI,Method,[]) ->
  no;
script(RequestURI,Method,[{Method,CGIScript}|Rest]) ->
  {yes,CGIScript++RequestURI};
script(RequestURI,Method,[_|Rest]) ->
  script(RequestURI,Method,Rest).

%%
%% Configuration
%%

%% load

load([$A,$c,$t,$i,$o,$n,$ |Action],[]) ->
  case regexp:split(Action," ") of
    {ok,[MimeType,CGIScript]} ->
      {ok,[],{action,{MimeType,CGIScript}}};
    {ok,_} ->
      {error,?NICE(httpd_conf:clean(Action)++" is an invalid Action")}
  end;
load([$S,$c,$r,$i,$p,$t,$ |Script],[]) ->
  case regexp:split(Script," ") of
    {ok,[Method,CGIScript]} ->
      {ok,[],{script,{Method,CGIScript}}};
    {ok,_} ->
      {error,?NICE(httpd_conf:clean(Script)++" is an invalid Script")}
  end.
