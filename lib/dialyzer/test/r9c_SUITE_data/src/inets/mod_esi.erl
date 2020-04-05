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
%%     $Id: mod_esi.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%
-module(mod_esi).
-export([do/1,load/2]).

%%Functions provided to help erl scheme alias programmer to
%%Create dynamic webpages that are sent back to the user during
%%Generation
-export([deliver/2]).


-include("httpd.hrl").

-define(VMODULE,"ESI").
-include("httpd_verbosity.hrl").

-define(GATEWAY_INTERFACE,"CGI/1.1").
-define(DEFAULT_ERL_TIMEOUT,15000).
%% do

do(Info) ->
    ?vtrace("do",[]),
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		%% No response has been generated!
		undefined ->
		    case erl_or_eval(Info#mod.request_uri,
				     Info#mod.config_db) of
			{eval,CGIBody,Modules} ->
			    eval(Info,Info#mod.method,CGIBody,Modules);
			{erl,CGIBody,Modules} ->
			    erl(Info,Info#mod.method,CGIBody,Modules);
			proceed ->
			    {proceed,Info#mod.data}
		    end;
		%% A response has been generated or sent!
		Response ->
		    {proceed,Info#mod.data}
	    end
    end.



%% erl_or_eval

erl_or_eval(RequestURI, ConfigDB) ->
    case erlp(RequestURI, ConfigDB) of
	false ->
	    case evalp(RequestURI, ConfigDB) of
		false ->
		    ?vtrace("neither erl nor eval",[]),
		    proceed;
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

erlp(RequestURI, ConfigDB) ->
    case httpd_util:multi_lookup(ConfigDB, erl_script_alias) of
	[] ->
	    false;
	AliasMods ->
	    erlp_find_alias(RequestURI,AliasMods)
    end.

erlp_find_alias(_RequestURI,[]) ->
    ?vtrace("erlp_find_alias -> no match",[]),
    false;
erlp_find_alias(RequestURI,[{Alias,Modules}|Rest]) ->
    case regexp:first_match(RequestURI,"^"++Alias++"/") of
	{match,1,Length} ->
	    ?vtrace("erlp -> match with Length: ~p",[Length]),
	    {erl,string:substr(RequestURI,Length+1),Modules};
	nomatch ->
	    erlp_find_alias(RequestURI,Rest)
    end.

evalp(RequestURI, ConfigDB) ->
    case httpd_util:multi_lookup(ConfigDB, eval_script_alias) of
	[] ->
	    false;
	AliasMods ->
	    evalp_find_alias(RequestURI,AliasMods)
    end.

evalp_find_alias(_RequestURI,[]) ->
    ?vtrace("evalp_find_alias -> no match",[]),
    false;
evalp_find_alias(RequestURI,[{Alias,Modules}|Rest]) ->
    case regexp:first_match(RequestURI,"^"++Alias++"\\?") of
	{match, 1, Length} ->
	    ?vtrace("evalp_find_alias -> match with Length: ~p",[Length]),
	    {eval, string:substr(RequestURI,Length+1),Modules};
	nomatch ->
	    evalp_find_alias(RequestURI,Rest)
    end.


%%
%% Erl mechanism
%%

%%This is exactly the same as the GET method the difference is that
%%The response must not contain any data expect the response header


erl(Info,"HEAD",CGIBody,Modules) ->
    erl(Info,"GET",CGIBody,Modules);

erl(Info,"GET",CGIBody,Modules) ->
    ?vtrace("erl GET request",[]),
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok, [Mod,FuncAndInput]} ->
	    ?vtrace("~n   Mod:          ~p"
		    "~n   FuncAndInput: ~p",[Mod,FuncAndInput]),
	    case httpd_util:split(FuncAndInput,"[\?/]",2) of
		{ok, [Func,Input]} ->
		    ?vtrace("~n   Func:  ~p"
			    "~n   Input: ~p",[Func,Input]),
		    exec(Info,"GET",CGIBody,Modules,Mod,Func,
			 {input_type(FuncAndInput),Input});
		{ok, [Func]} ->
		    exec(Info,"GET",CGIBody,Modules,Mod,Func,{no_input,""});
		{ok, BadRequest} ->
		    {proceed,[{status,{400,none,BadRequest}}|Info#mod.data]}
	    end;
	{ok, BadRequest} ->
	    ?vlog("erl BAD (GET-) request",[]),
	    {proceed, [{status,{400,none,BadRequest}}|Info#mod.data]}
    end;

erl(Info, "POST", CGIBody, Modules) ->
    ?vtrace("erl POST request",[]),
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok,[Mod,Func]} ->
	    ?vtrace("~n   Mod:  ~p"
		    "~n   Func: ~p",[Mod,Func]),
	    exec(Info,"POST",CGIBody,Modules,Mod,Func,
		 {entity_body,Info#mod.entity_body});
	{ok,BadRequest} ->
	    ?vlog("erl BAD (POST-) request",[]),
	    {proceed,[{status,{400,none,BadRequest}}|Info#mod.data]}
    end.

input_type([]) ->
    no_input;
input_type([$/|Rest]) ->
    path_info;
input_type([$?|Rest]) ->
    query_string;
input_type([First|Rest]) ->
    input_type(Rest).


%% exec

exec(Info,Method,CGIBody,["all"],Mod,Func,{Type,Input}) ->
    ?vtrace("exec ~s 'all'",[Method]),
    exec(Info,Method,CGIBody,[Mod],Mod,Func,{Type,Input});
exec(Info,Method,CGIBody,Modules,Mod,Func,{Type,Input}) ->
    ?vtrace("exec ~s request with:"
	    "~n   Modules: ~p"
	    "~n   Mod:     ~p"
	    "~n   Func:    ~p"
	    "~n   Type:    ~p"
	    "~n   Input:   ~p",
	    [Method,Modules,Mod,Func,Type,Input]),
    case lists:member(Mod,Modules) of
	true ->
	    {_,RemoteAddr}=(Info#mod.init_data)#init_data.peername,
	    ServerName=(Info#mod.init_data)#init_data.resolve,
	    Env=get_environment(Info,ServerName,Method,RemoteAddr,Type,Input),
	    ?vtrace("and now call the module",[]),
	    case try_new_erl_scheme_method(Info,Env,Input,list_to_atom(Mod),list_to_atom(Func)) of
		{error,not_new_method}->
		    case catch apply(list_to_atom(Mod),list_to_atom(Func),[Env,Input]) of
			{'EXIT',Reason} ->
			    ?vlog("exit with Reason: ~p",[Reason]),
			    {proceed,[{status,{500,none,Reason}}|Info#mod.data]};
			Response ->
			    control_response_header(Info,Mod,Func,Response)
		    end;
		ResponseResult->
		    ResponseResult
	    end;
	false ->
	    ?vlog("unknown module",[]),
	    {proceed,[{status,{403,Info#mod.request_uri,
			       ?NICE("Client not authorized to evaluate: "++CGIBody)}}|Info#mod.data]}
    end.

control_response_header(Info,Mod,Func,Response)->
    case control_response(Response,Info,Mod,Func) of
	{proceed,[{response,{StatusCode,Response}}|Rest]} ->
	    case httpd_util:lookup(Info#mod.config_db,erl_script_nocache,false) of
		true ->
		    case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
			{ok,[Head,Body]}->
			    Date=httpd_util:rfc1123_date(),
			    Cache="Cache-Control:no-cache\r\nPragma:no-cache\r\nExpires:"++ Date ++ "\r\n",
			    {proceed,[{response,{StatusCode,[Head,"\r\n",Cache,"\r\n",Body]}}|Rest]};
			_->
			   {proceed,[{response,{StatusCode,Response}}|Rest]}
		    end;
		WhatEver->
		    {proceed,[{response,{StatusCode,Response}}|Rest]}
	    end;
	WhatEver->
	    WhatEver
    end.

control_response(Response,Info,Mod,Func)->
    ?vdebug("Response: ~n~p",[Response]),
    case mod_cgi:status_code(lists:flatten(Response)) of
	{ok,StatusCode} ->
	    {proceed,[{response,{StatusCode,Response}}|Info#mod.data]};
	{error,Reason} ->
	    {proceed,
	     [{status,{400,none,
		       ?NICE("Error in "++Mod++":"++Func++"/2: "++
			     lists:flatten(io_lib:format("~p",[Reason])))}}|
	      Info#mod.data]}
    end.

parsed_header([]) ->
    [];
parsed_header([{Name,[Value|R1]}|R2]) when list(Value) ->
    NewName=lists:map(fun(X) -> if X == $- -> $_; true -> X end end,Name),
    [{list_to_atom("http_"++httpd_util:to_lower(NewName)),
      multi_value([Value|R1])}|parsed_header(R2)];
parsed_header([{Name,Value}|Rest]) when list(Value)->
    {ok,NewName,_}=regexp:gsub(Name,"-","_"),
    [{list_to_atom("http_"++httpd_util:to_lower(NewName)),Value}|
     parsed_header(Rest)].

multi_value([]) ->
    [];
multi_value([Value]) ->
    Value;
multi_value([Value|Rest]) ->
    Value++", "++multi_value(Rest).

%%
%% Eval mechanism
%%


eval(Info,"POST",CGIBody,Modules) ->
    ?vtrace("eval(POST) -> method not supported",[]),
    {proceed,[{status,{501,{"POST",Info#mod.request_uri,Info#mod.http_version},
		       ?NICE("Eval mechanism doesn't support method POST")}}|
	      Info#mod.data]};

eval(Info,"HEAD",CGIBody,Modules) ->
    %%The function that sends the data in httpd_response handles HEAD reqest by not
    %% Sending the body
    eval(Info,"GET",CGIBody,Modules);


eval(Info,"GET",CGIBody,Modules) ->
    ?vtrace("eval(GET) -> entry when"
	    "~n   Modules: ~p",[Modules]),
    case auth(CGIBody,Modules) of
	true ->
	    case erl_eval:eval_str(string:concat(CGIBody,". ")) of
		{error,Reason} ->
		    ?vlog("eval -> error:"
			  "~n   Reason: ~p",[Reason]),
		    {proceed,[{status,{500,none,Reason}}|Info#mod.data]};
		{ok,Response} ->
		    ?vtrace("eval -> ok:"
			    "~n   Response: ~p",[Response]),
		    case mod_cgi:status_code(lists:flatten(Response)) of
			{ok,StatusCode} ->
			    {proceed,[{response,{StatusCode,Response}}|Info#mod.data]};
			{error,Reason} ->
			    {proceed,[{status,{400,none,Reason}}|Info#mod.data]}
		    end
	    end;
	false ->
	    ?vlog("eval -> auth failed",[]),
	    {proceed,[{status,
		       {403,Info#mod.request_uri,
			?NICE("Client not authorized to evaluate: "++CGIBody)}}|
		      Info#mod.data]}
    end.

auth(CGIBody,["all"]) ->
    true;
auth(CGIBody,Modules) ->
    case regexp:match(CGIBody,"^[^\:(%3A)]*") of
	{match,Start,Length} ->
	    lists:member(string:substr(CGIBody,Start,Length),Modules);
	nomatch ->
	    false
    end.

%%----------------------------------------------------------------------
%%Creates the environment list that will be the first arg to the
%%Functions that is called through the ErlScript Schema
%%----------------------------------------------------------------------

get_environment(Info,ServerName,Method,RemoteAddr,Type,Input)->
    Env=[{server_software,?SERVER_SOFTWARE},
		 {server_name,ServerName},
		 {gateway_interface,?GATEWAY_INTERFACE},
		 {server_protocol,?SERVER_PROTOCOL},
		 {server_port,httpd_util:lookup(Info#mod.config_db,port,80)},
		 {request_method,Method},
		 {remote_addr,RemoteAddr},
		 {script_name,Info#mod.request_uri}|
		 parsed_header(Info#mod.parsed_header)],
    get_environment(Type,Input,Env,Info).


get_environment(Type,Input,Env,Info)->
    Env1=case Type of
	query_string ->
	    [{query_string,Input}|Env];
	path_info ->
	    Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
	    {_,PathTranslated,_}=mod_alias:real_name(Info#mod.config_db,[$/|Input],Aliases),
	    [{path_info,"/"++httpd_util:decode_hex(Input)},
	     {path_translated,PathTranslated}|Env];
	entity_body ->
	    [{content_length,httpd_util:flatlength(Input)}|Env];
	no_input ->
	    Env
    end,
    get_environment(Info,Env1).

get_environment(Info,Env)->
    case httpd_util:key1search(Info#mod.data,remote_user) of
	undefined ->
	    Env;
	RemoteUser ->
	    [{remote_user,RemoteUser}|Env]
    end.
%%
%% Configuration
%%

%% load

load([$E,$r,$l,$S,$c,$r,$i,$p,$t,$A,$l,$i,$a,$s,$ |ErlScriptAlias],[]) ->
    case regexp:split(ErlScriptAlias," ") of
	{ok, [ErlName|Modules]} ->
	    {ok, [], {erl_script_alias, {ErlName,Modules}}};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(ErlScriptAlias)++
			 " is an invalid ErlScriptAlias")}
    end;
load([$E,$v,$a,$l,$S,$c,$r,$i,$p,$t,$A,$l,$i,$a,$s,$ |EvalScriptAlias],[]) ->
    case regexp:split(EvalScriptAlias, " ") of
	{ok, [EvalName|Modules]} ->
	    {ok, [], {eval_script_alias, {EvalName,Modules}}};
	{ok, _} ->
	    {error, ?NICE(httpd_conf:clean(EvalScriptAlias)++
			  " is an invalid EvalScriptAlias")}
    end;
load([$E,$r,$l,$S,$c,$r,$i,$p,$t,$T,$i,$m,$e,$o,$u,$t,$ |Timeout],[])->
    case catch list_to_integer(httpd_conf:clean(Timeout)) of
	TimeoutSec when integer(TimeoutSec)  ->
	   {ok, [], {erl_script_timeout,TimeoutSec*1000}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(Timeout)++
			 " is an invalid ErlScriptTimeout")}
    end;
load([$E,$r,$l,$S,$c,$r,$i,$p,$t,$N,$o,$C,$a,$c,$h,$e |CacheArg],[])->
    case catch list_to_atom(httpd_conf:clean(CacheArg)) of
        true ->
	    {ok, [], {erl_script_nocache,true}};
	false ->
	   {ok, [], {erl_script_nocache,false}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(CacheArg)++
			 " is an invalid ErlScriptNoCache directive")}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Functions below handles the data from the dynamic webpages         %%
%% That sends data back to the user part by part                      %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%%Deliver is the callback function users can call to deliver back data to the
%%client
%%----------------------------------------------------------------------

deliver(SessionID,Data)when pid(SessionID) ->
    SessionID ! {ok,Data},
    ok;
deliver(SessionID,Data) ->
    {error,bad_sessionID}.


%%----------------------------------------------------------------------
%% The method that tries to execute the new format
%%----------------------------------------------------------------------

%%It would be nicer to use erlang:function_exported/3 but if the
%%Module isn't loaded the function says that it is not loaded


try_new_erl_scheme_method(Info,Env,Input,Mod,Func)->
    process_flag(trap_exit,true),
    Pid=spawn_link(Mod,Func,[self(),Env,Input]),
    Timeout=httpd_util:lookup(Info#mod.config_db,erl_script_timeout,?DEFAULT_ERL_TIMEOUT),
    RetVal=receive_response_data(Info,Pid,0,undefined,[],Timeout),
    process_flag(trap_exit,false),
    RetVal.


%%----------------------------------------------------------------------
%%The function receives the data from the process that generates the page
%%and send the data to the client through the mod_cgi:send function
%%----------------------------------------------------------------------

receive_response_data(Info,Pid,Size,StatusCode,AccResponse,Timeout) ->
    ?DEBUG("receive_response_data()-> Script Size: ~p,StatusCode ~p ,Timeout: ~p ~n",[Size,StatusCode,Timeout]),
    receive
	{ok, Response} ->
	    NewStatusCode=mod_cgi:update_status_code(StatusCode,Response),

	    ?DEBUG("receive_response_data/2 NewStatusCode: ~p~n",[NewStatusCode]),
	    case mod_cgi:send(Info, NewStatusCode,Response, Size,AccResponse) of
		socket_closed ->
		    (catch exit(Pid,final)),
		    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
		head_sent->
		    (catch exit(Pid,final)),
		    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
		_ ->
		    %%The data is sent and the socket is not closed contine
		    NewSize = mod_cgi:get_new_size(Size,Response),
		    receive_response_data(Info,Pid,NewSize,NewStatusCode,"notempty",Timeout)
	    end;
	{'EXIT', Pid, Reason} when AccResponse==[] ->
	    {error,not_new_method};
	{'EXIT', Pid, Reason} when pid(Pid) ->
	    NewStatusCode=mod_cgi:update_status_code(StatusCode,AccResponse),
	    mod_cgi:final_send(Info,NewStatusCode,Size,AccResponse),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]};
	%% This should not happen!
	WhatEver ->
	    NewStatusCode=mod_cgi:update_status_code(StatusCode,AccResponse),
	    mod_cgi:final_send(Info,StatusCode,Size,AccResponse),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]}
    after
	Timeout ->
	    (catch exit(Pid,timeout)), % KILL the port !!!!
	    httpd_socket:close(Info#mod.socket_type,Info#mod.socket),
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]}
    end.
