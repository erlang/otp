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
%% Implements  The WWW Common Gateway Interface Version 1.1

-module(mod_cgi).

-export([env/3]).

%%% Callback API
-export([do/1, load/2, store/2]).

-include("http_internal.hrl").
-include("httpd_internal.hrl").
-include("httpd.hrl").

-define(VMODULE,"CGI").

-define(DEFAULT_CGI_TIMEOUT, 15000).

%%%=========================================================================
%%%  API
%%%=========================================================================
%%--------------------------------------------------------------------------
%% do(ModData, _, AfterScript) ->  [{EnvVariable, Value}]
%%                
%%     AfterScript = string()
%%     ModData = #mod{}
%%     EnvVariable = string() 
%%     Value = term()
%% Description: Keep for now as it is documented in the man page
%%-------------------------------------------------------------------------
env(ModData, _Script, AfterScript) ->
    ScriptElements = script_elements(ModData, AfterScript),
    httpd_script_env:create_env(cgi, ModData, ScriptElements).

%%%=========================================================================
%%%  Callback API
%%%=========================================================================

%%--------------------------------------------------------------------------
%% do(ModData) -> {proceed, OldData} | {proceed, NewData} | {break, NewData} 
%%                | done
%%     ModData = #mod{}
%%
%% Description:  See httpd(3) ESWAPI CALLBACK FUNCTIONS
%%-------------------------------------------------------------------------
do(ModData) ->
    case proplists:get_value(status, ModData#mod.data) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, ModData#mod.data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, ModData#mod.data) of
		undefined ->
		    generate_response(ModData);
		_Response ->
		    {proceed, ModData#mod.data}
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

%% ScriptNoCache true|false, defines whether the server shall add     
%%                           header fields to stop proxies and          
%%                           clients from saving the page in history  
%%                           or cache                                 
%%                                                                             
load("ScriptNoCache " ++ CacheArg, [])->
    case catch list_to_atom(string:strip(CacheArg)) of
        true ->
	    {ok, [], {script_nocache, true}};
	false ->
	   {ok, [], {script_nocache, false}};
	_ ->
	   {error, ?NICE(string:strip(CacheArg)++
			 " is an invalid ScriptNoCache directive")}
    end;
%% ScriptTimeout Seconds, The number of seconds that the server       
%%                        maximum will wait for the script to         
%%                        generate a part of the document   
load("ScriptTimeout " ++ Timeout, [])->
    case catch list_to_integer(string:strip(Timeout)) of
	TimeoutSec when is_integer(TimeoutSec)  ->
	   {ok, [], {script_timeout,TimeoutSec*1000}};
	_ ->
	   {error, ?NICE(string:strip(Timeout)++
			 " is an invalid ScriptTimeout")}
    end.

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
store({script_nocache, Value} = Conf, _) 
  when Value == true; Value == false ->
    {ok, Conf};
store({script_nocache, Value}, _) ->
    {error, {wrong_type, {script_nocache, Value}}};
store({script_timeout, Value}, _) 
  when is_integer(Value), Value >= 0 ->
    {ok, {script_timeout, Value * 1000}};
store({script_timeout, Value}, _) ->
    {error, {wrong_type, {script_timeout, Value}}}.
	
%%%========================================================================
%%% Internal functions
%%%========================================================================
generate_response(ModData) ->
    RequestURI =
	case proplists:get_value(new_request_uri, ModData#mod.data) of
	    undefined ->
		ModData#mod.request_uri;
	    Value ->
		Value
	end,
    ScriptAliases =
	httpd_util:multi_lookup(ModData#mod.config_db, script_alias),
    case mod_alias:real_script_name(ModData#mod.config_db, RequestURI,
				    ScriptAliases) of
	{Script, AfterScript} ->
	    exec_script(ModData, Script, AfterScript, 
			RequestURI);
	not_a_script ->
	    {proceed, ModData#mod.data}
    end.

is_executable(File) ->
    Dir      = filename:dirname(File),
    FileName = filename:basename(File),
    case os:type() of
	{win32,_} ->
	    %% temporary (hopefully) fix for win32 OTP-3627
	    is_win32_executable(Dir,FileName);
	_ ->
	    is_executable(Dir, FileName) 
    end.

is_executable(Dir, FilName) ->
    case os:find_executable(FilName, Dir) of
	false ->
	    false;
	_ ->
	    true
    end.

%% Start temporary (hopefully) fix for win32 OTP-3627
%% ---------------------------------
is_win32_executable(Dir, FileName) ->
    NewFileName = strip_extention(FileName, [".bat",".exe",".com", ".cmd"]),
    is_executable(Dir, NewFileName).

strip_extention(FileName, []) ->
    FileName;
strip_extention(FileName, [Extention | Extentions]) ->
    case filename:basename(FileName, Extention) of
	FileName ->
	    strip_extention(FileName, Extentions); 
	NewFileName ->
	    NewFileName
    end.

%% End fix
%% ---------------------------------

exec_script(ModData, Script, AfterScript, RequestURI) ->
    exec_script(is_executable(Script), ModData, Script, 
		AfterScript, RequestURI).

exec_script(true, ModData, Script, AfterScript, _RequestURI) ->
    process_flag(trap_exit,true),
    Dir  = filename:dirname(Script),
    ScriptElements = script_elements(ModData, AfterScript),
    Env = (catch httpd_script_env:create_env(cgi, ModData, ScriptElements)),

    %% Run script
    Port = (catch open_port({spawn, Script},[binary, stream,
					     {cd, Dir}, {env, Env}])),
    case Port of
	Port when is_port(Port) ->
	    send_request_body_to_script(ModData, Port),
	    deliver_webpage(ModData, Port); % Take care of script output
	Error ->
	    exit({open_port_failed, Error,
		  [{mod,?MODULE},
		   {uri,ModData#mod.request_uri}, {script,Script},
		   {env,Env},{dir,Dir}]})
    end;

exec_script(false, ModData, _Script, _AfterScript, _RequestURI) ->
    {proceed,
     [{status,
       {404,ModData#mod.request_uri,
	?NICE("You don't have permission to execute " ++
	      ModData#mod.request_uri ++ " on this server")}}|
      ModData#mod.data]}.
    
send_request_body_to_script(ModData, Port) ->
    case ModData#mod.entity_body of
	[] ->
	    ok;
	EntityBody ->
	    port_command(Port, EntityBody)
    end.	
	   
deliver_webpage(#mod{config_db = Db} = ModData, Port) ->
    Timeout = script_timeout(Db),    
    case receive_headers(Port, httpd_cgi, parse_headers, 
			 [<<>>, [], []], Timeout) of
	{Headers, Body} ->
	    case httpd_cgi:handle_headers(Headers) of
		{proceed, AbsPath} ->
		    {proceed, [{real_name, 
				httpd_util:split_path(AbsPath)} | 
			       ModData#mod.data]};
		{ok, HTTPHeaders, Status} ->
		    IsDisableChunkedSend = 
			httpd_response:is_disable_chunked_send(Db),
		    case (ModData#mod.http_version =/= "HTTP/1.1") or 
			(IsDisableChunkedSend) of
			true ->
			    send_headers(ModData, Status, 
					 [{"connection", "close"}
					   | HTTPHeaders]);
			false ->
			    send_headers(ModData, Status,
					 [{"transfer-encoding",
					   "chunked"} | HTTPHeaders])
		    end,
		    handle_body(Port, ModData, Body, Timeout, size(Body),
				IsDisableChunkedSend)
	    end;
	{'EXIT', Port, Reason} ->
	    process_flag(trap_exit, false),
	    {proceed, [{status, {400, none, reason(Reason)}} |
		       ModData#mod.data]};
	timeout ->
	    (catch port_close(Port)), % KILL the port !!!!
	    send_headers(ModData, {504, "Timeout"}, []),
	    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket),
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, 0}} | ModData#mod.data]}
    end.
	    
receive_headers(Port, Module, Function, Args, Timeout) ->
      receive
	  {Port, {data, Response}} when is_port(Port) ->
	      case Module:Function([Response | Args]) of
		  {NewModule, NewFunction, NewArgs} ->
		      receive_headers(Port, NewModule, 
				      NewFunction, NewArgs, Timeout);
		  {ok, {Headers, Body}} ->
		      {Headers, Body}
	      end;
	  {'EXIT', Port, Reason} when is_port(Port) ->
	      {'EXIT', Port, Reason};
	  {'EXIT', Pid, Reason} when is_pid(Pid) ->
	      exit({linked_process_died, Pid, Reason})
      after Timeout ->
	      timeout
      end.

send_headers(ModData, {StatusCode, _}, HTTPHeaders) ->
    ExtraHeaders = httpd_response:cache_headers(ModData, script_nocache),
    httpd_response:send_header(ModData, StatusCode, 
			       ExtraHeaders ++ HTTPHeaders).

handle_body(Port, #mod{method = "HEAD"} = ModData, _, _, Size, _) ->
    (catch port_close(Port)), % KILL the port !!!!
    process_flag(trap_exit,false),
    {proceed, [{response, {already_sent, 200, Size}} | ModData#mod.data]};

handle_body(Port, ModData, Body, Timeout, Size, IsDisableChunkedSend) ->
    httpd_response:send_chunk(ModData, Body, IsDisableChunkedSend),
    receive 
	{Port, {data, Data}} when is_port(Port) ->
	    handle_body(Port, ModData, Data, Timeout, Size + size(Data),
			IsDisableChunkedSend);
	{'EXIT', Port, normal} when is_port(Port) ->
	    httpd_response:send_final_chunk(ModData, IsDisableChunkedSend),
	    process_flag(trap_exit,false),
	    {proceed, [{response, {already_sent, 200, Size}} |
		       ModData#mod.data]};
	{'EXIT', Port, Reason} when is_port(Port) ->
	    process_flag(trap_exit, false),
	    {proceed, [{status, {400, none, reason(Reason)}} | 
		       ModData#mod.data]};
	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    exit({mod_cgi_linked_process_died, Pid, Reason})
    after Timeout ->
	    (catch port_close(Port)), % KILL the port !!!!
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, Size}} |
		      ModData#mod.data]}
    end.

script_elements(#mod{method = "GET"}, {[], QueryString}) ->
    [{query_string, QueryString}];
script_elements(#mod{method = "GET"}, {PathInfo, []}) ->
    [{path_info, PathInfo}];
script_elements(#mod{method = "GET"}, {PathInfo, QueryString}) ->
    [{query_string, QueryString}, {path_info, PathInfo}];
script_elements(#mod{method = "POST", entity_body = Body}, _) ->
    [{entity_body, Body}];
script_elements(#mod{method = "PATCH", entity_body = Body}, _) ->
    [{entity_body, Body}];
script_elements(#mod{method = "PUT", entity_body = Body}, _) ->
    [{entity_body, Body}];
script_elements(_, _) ->
    [].

script_timeout(Db) ->
    httpd_util:lookup(Db, script_timeout, ?DEFAULT_CGI_TIMEOUT).

%% Convert error to printable string
%%
reason({error,emfile})     -> ": To many open files";
reason({error,{enfile,_}}) -> ": File/port table overflow";
reason({error,enomem})     -> ": Not enough memory";
reason({error,eagain})     -> ": No more available OS processes";
reason(Reason) -> lists:flatten(io_lib:format("Reason: ~p~n", [Reason])).
