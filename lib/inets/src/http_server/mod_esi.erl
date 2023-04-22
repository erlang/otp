%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
-module(mod_esi).

%% API
%% Functions provided to help erl scheme alias programmer to 
%% Create dynamic webpages that are sent back to the user during 
%% Generation
-export([deliver/2]).

%% Callback API
-export([do/1, store/2]).

-include("httpd.hrl").
-include("httpd_internal.hrl").
-include_lib("kernel/include/logger.hrl").

-define(VMODULE,"ESI").
-define(DEFAULT_ERL_TIMEOUT,15).


%%%=========================================================================
%%%  API 
%%%=========================================================================

%%--------------------------------------------------------------------------
%% deliver(SessionID, Data) -> ok | {error, bad_sessionID}
%%	SessionID = pid()
%%	Data = string() | io_list() (first call must send a string that 
%%	contains all header information including "\r\n\r\n", unless there
%%	is no header information at all.)
%%
%% Description: Send <Data> (Html page generated sofar) to the server
%% request handling process so it can forward it to the client.
%%-------------------------------------------------------------------------
deliver(SessionID, Data) when is_pid(SessionID) ->
    SessionID ! {esi_data, Data},
    ok;
deliver(_SessionID, _Data) ->
    {error, bad_sessionID}.


%%%=========================================================================
%%%  CALLBACK API 
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
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, ModData#mod.data};
	undefined ->
	    case proplists:get_value(response, ModData#mod.data) of
		undefined ->
		    generate_response(ModData);
		_Response ->
		    {proceed, ModData#mod.data}
	    end
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
store({erl_script_alias, {Name, [all]}} = Conf, _) 
  when is_list(Name) ->
 	    {ok, Conf};

store({erl_script_alias, {Name, Modules}} = Conf, _) 
  when is_list(Name) ->
    try httpd_util:modules_validate(Modules) of
   	ok ->
   	    {ok, Conf}
    catch
   	throw:Error ->
   	    {error, {wrong_type, {erl_script_alias, Error}}}
    end;

store({erl_script_alias, Value}, _) ->
    {error, {wrong_type, {erl_script_alias, Value}}};
store({erl_script_timeout, TimeoutSec}, _) 
  when is_integer(TimeoutSec) andalso (TimeoutSec >= 0) ->
    {ok, {erl_script_timeout, TimeoutSec}};
store({erl_script_timeout, Value}, _) ->
    {error, {wrong_type, {erl_script_timeout, Value}}};
store({erl_script_nocache, Value} = Conf, _) 
  when (Value =:= true) orelse (Value =:= false) ->
    {ok, Conf};
store({erl_script_nocache, Value}, _) ->
    {error, {wrong_type, {erl_script_nocache, Value}}}.


%%%========================================================================
%%% Internal functions
%%%========================================================================   
generate_response(ModData) ->
    case scheme(ModData#mod.request_uri, ModData#mod.config_db) of
	{erl, ESIBody, Modules} ->
	    erl(ModData, ESIBody, Modules);
	no_scheme ->
	    {proceed, ModData#mod.data}
    end.

scheme(RequestURI, ConfigDB) ->
    case match_script(RequestURI, ConfigDB, erl_script_alias) of
	no_match ->
            no_scheme;
	{EsiBody, ScriptModules} ->
	    {erl, EsiBody, ScriptModules}
    end.

match_script(RequestURI, ConfigDB, AliasType) ->
    case httpd_util:multi_lookup(ConfigDB, AliasType) of
	[] ->
	    no_match;
	AliasAndMods ->
	    match_esi_script(RequestURI, AliasAndMods, AliasType)
    end.

match_esi_script(_, [], _) ->
    no_match;
match_esi_script(RequestURI, [{Alias,Modules} | Rest], AliasType) ->
    AliasMatchStr = alias_match_str(Alias, AliasType),
    case re:run(RequestURI, AliasMatchStr, [{capture, first}]) of
	{match, [{0, Length}]} ->
	    {string:substr(RequestURI, Length + 1), Modules};
	nomatch ->
	    match_esi_script(RequestURI, Rest, AliasType)
    end.

alias_match_str(Alias, erl_script_alias) ->
    "^" ++ Alias ++ "/".

%%------------------------ Erl mechanism --------------------------------

erl(#mod{method = Method} = ModData, ESIBody, Modules) 
  when (Method =:= "GET") orelse (Method =:= "HEAD") orelse (Method =:= "DELETE") ->
    case httpd_util:split(ESIBody,":|%3A|/",2) of
	{ok, [ModuleName, FuncAndInput]} ->
	    case httpd_util:split(FuncAndInput,"[\?/]",2) of
		{ok, [FunctionName, Input]} ->
		    generate_webpage(ModData, ESIBody, Modules, 
				     ModuleName,
				     FunctionName, Input,
				     script_elements(FuncAndInput, Input));
		{ok, [FunctionName]} ->
		    generate_webpage(ModData, ESIBody, Modules, 
				     ModuleName,
				     FunctionName, "", 
				     script_elements(FuncAndInput, ""));
		{ok, BadRequest} ->
		    {proceed,[{status,{400,none, BadRequest}} | 
			      ModData#mod.data]}
	    end;
	{ok, BadRequest} ->
	    {proceed, [{status,{400, none, BadRequest}} | ModData#mod.data]}
    end;

erl(#mod{method = Method, entity_body = Body} = ModData,
    ESIBody, Modules) when Method =:= "PUT" orelse
                           Method =:= "PATCH" ->
    case httpd_util:split(ESIBody,":|%3A|/",2) of
	{ok, [ModuleName, FuncAndInput]} ->                
	    case httpd_util:split(FuncAndInput,"[\?/]",2) of
		{ok, [FunctionName, Input]} ->
		    generate_webpage(ModData, ESIBody, Modules,
				     ModuleName,
				     FunctionName, {Input,Body},
				     script_elements(FuncAndInput, Input));
		{ok, [FunctionName]} ->
		    generate_webpage(ModData, ESIBody, Modules,
				     ModuleName,
				     FunctionName, {undefined,Body},
				     script_elements(FuncAndInput, ""));
		{ok, BadRequest} ->
		    {proceed,[{status,{400,none, BadRequest}} |
			      ModData#mod.data]}
	    end;
	{ok, BadRequest} ->
	    {proceed, [{status,{400, none, BadRequest}} | ModData#mod.data]}
    end;   

erl(#mod{method = "POST", entity_body = Body} = ModData, ESIBody, Modules) ->
    case httpd_util:split(ESIBody,":|%3A|/",2) of
	{ok,[ModuleName, Function]} ->
	    generate_webpage(ModData, ESIBody, Modules, 
			     ModuleName,
			     Function, Body, []);
	{ok, BadRequest} ->
	    {proceed,[{status, {400, none, BadRequest}} | ModData#mod.data]}
    end.

generate_webpage(ModData, ESIBody, [all], Module, FunctionName,
		 Input, ScriptElements) ->
    try
        ModuleAtom = list_to_existing_atom(Module),
        generate_webpage(ModData, ESIBody, [ModuleAtom], Module,
                         FunctionName, Input, ScriptElements)
    catch
        _:_ ->
            {proceed, [{status, {404, ModData#mod.request_uri, "Not found"}}
                      | ModData#mod.data]}
    end;
generate_webpage(ModData, ESIBody, Modules, Module, Function,
		 Input, ScriptElements) when is_atom(Module), is_atom(Function) ->
    case lists:member(Module, Modules) of
	true ->
	    Env = httpd_script_env:create_env(esi, ModData, ScriptElements),
	    case erl_scheme_webpage_chunk(Module, Function, 
					  Env, Input, ModData) of
		{error, erl_scheme_webpage_chunk_undefined} ->
                    {proceed, [{status, {404, ModData#mod.request_uri, "Not found"}}
                               | ModData#mod.data]};
		ResponseResult ->
		    ResponseResult
	    end;
	false ->
	    {proceed, [{status, {403, ModData#mod.request_uri,
				 ?NICE("Client not authorized to evaluate: "
				       ++  ESIBody)}} | ModData#mod.data]}
    end;
generate_webpage(ModData, ESIBody, Modules, ModuleName, FunctionName,
		 Input, ScriptElements) ->
    try
        Module = list_to_existing_atom(ModuleName),
        _ = code:ensure_loaded(Module),
        Function = list_to_existing_atom(FunctionName),
        generate_webpage(ModData, ESIBody, Modules, Module, Function,
                         Input, ScriptElements)
    catch
        _:_ ->
            {proceed, [{status, {404, ModData#mod.request_uri, "Not found"}}
                      | ModData#mod.data]}
    end.

%% API that allows the dynamic wepage to be sent back to the client 
%% in small chunks at the time during generation.
erl_scheme_webpage_chunk(Mod, Func, Env, Input, ModData) -> 
    process_flag(trap_exit, true),
    Self = self(),
    %% Spawn worker that generates the webpage.
    %% It would be nicer to use erlang:function_exported/3 but if the 
    %% Module isn't loaded the function says that it is not loaded
    Pid = spawn_link(
	    fun() ->
		    case catch Mod:Func(Self, Env, Input) of
			{'EXIT', {undef,_}} ->
			    exit(erl_scheme_webpage_chunk_undefined);
			{continue, _} = Continue ->
                            exit(Continue);
                        _ ->
			    ok  
		    end
	    end),
 
    Response = deliver_webpage_chunk(ModData, Pid), 
    process_flag(trap_exit,false),
    Response.

deliver_webpage_chunk(#mod{config_db = Db} = ModData, Pid) ->
    Timeout = erl_script_timeout(Db),
    deliver_webpage_chunk(ModData, Pid, Timeout).

deliver_webpage_chunk(#mod{config_db = Db} = ModData, Pid, Timeout) ->
    case receive_headers(Timeout) of
	{error, Reason} ->
	    %% Happens when webpage generator callback/3 is undefined
	    {error, Reason}; 
        {continue, _} = Continue ->
            Continue;
	{Headers, Body} ->
            {ok, NewHeaders, StatusCode} = httpd_esi:handle_headers(Headers),
            %% All 1xx (informational), 204 (no content), and 304 (not modified)
            %% responses MUST NOT include a message-body, and thus are always
            %% terminated by the first empty line after the header fields.
            %% This implies that chunked encoding MUST NOT be used for these
            %% status codes.
            IsDisableChunkedSend =
                httpd_response:is_disable_chunked_send(Db) orelse
                StatusCode =:= 204 orelse                      %% No Content
                StatusCode =:= 304 orelse                      %% Not Modified
                (100 =< StatusCode andalso StatusCode =< 199), %% Informational
            case (ModData#mod.http_version =/= "HTTP/1.1") or
                (IsDisableChunkedSend) of
                true ->
                    send_headers(ModData, StatusCode, 
                                 [{"connection", "close"} | 
                                  NewHeaders]);
                false ->
                    send_headers(ModData, StatusCode, 
                                 [{"transfer-encoding", 
                                   "chunked"} | NewHeaders])
            end,
            handle_body(Pid, ModData, Body, Timeout, length(Body), StatusCode,
                        IsDisableChunkedSend);
        timeout ->
            send_headers(ModData, 504, [{"connection", "close"}]),
	    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket),
	    {proceed,[{response, {already_sent, 504, 0}} | ModData#mod.data]}
    end.

receive_headers(Timeout) ->
    receive
	{esi_data, Chunk} ->
	    httpd_esi:parse_headers(lists:flatten(Chunk));		
	{ok, Chunk} ->
	    httpd_esi:parse_headers(lists:flatten(Chunk));		
	{'EXIT', Pid, erl_scheme_webpage_chunk_undefined} when is_pid(Pid) ->
	    {error, erl_scheme_webpage_chunk_undefined};
	{'EXIT', Pid, {continue, _} = Continue} when is_pid(Pid) ->
            Continue;
        {'EXIT', Pid, Reason} when is_pid(Pid) ->
	    exit({mod_esi_linked_process_died, Pid, Reason})
    after Timeout ->
	    timeout
    end.

send_headers(ModData, StatusCode, HTTPHeaders) ->
    ExtraHeaders = httpd_response:cache_headers(ModData, erl_script_nocache),
    httpd_response:send_header(ModData, StatusCode, 
			       ExtraHeaders ++ HTTPHeaders).

handle_body(_, #mod{method = "HEAD"} = ModData, _, _, Size, StatusCode, _) ->
    {proceed, [{response, {already_sent, StatusCode, Size}} | ModData#mod.data]};

handle_body(Pid, ModData, Body, Timeout, Size, StatusCode, IsDisableChunkedSend) ->
    httpd_response:send_chunk(ModData, Body, IsDisableChunkedSend),
    receive 
	{esi_data, Data} when is_binary(Data) ->
	    handle_body(Pid, ModData, Data, Timeout, Size + byte_size(Data), StatusCode,
			IsDisableChunkedSend);
	{esi_data, Data} ->
	    handle_body(Pid, ModData, Data, Timeout, Size + length(Data), StatusCode,
			IsDisableChunkedSend);
	{ok, Data} ->
	    handle_body(Pid, ModData, Data, Timeout, Size + length(Data), StatusCode,
			IsDisableChunkedSend);
	{'EXIT', Pid, normal} when is_pid(Pid) ->
	    httpd_response:send_final_chunk(ModData, IsDisableChunkedSend),
	    {proceed, [{response, {already_sent, StatusCode, Size}} | 
		       ModData#mod.data]};
	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    httpd_util:error_log(ModData#mod.config_db,  
                                 httpd_logger:error_report('HTTP', 
                                                           [{mod_esi, Reason}], ModData, ?LOCATION)),
	    httpd_response:send_final_chunk(ModData, 
					    [{"Warning", "199 inets server - body maybe incomplete, "
					      "internal server error"}],
					    IsDisableChunkedSend),
	    done
    after Timeout ->
	    kill_esi_delivery_process(Pid),
	    httpd_response:send_final_chunk(ModData, [{"Warning", "199 inets server - "
						       "body maybe incomplete, timed out"}],
					    IsDisableChunkedSend),
	    done
    end.

kill_esi_delivery_process(Pid) -> 
    exit(Pid, kill),
    receive 
	{'EXIT', Pid, killed} ->	
	    %% Clean message queue
	    receive
		{esi_data, _} ->
		    ok
	    after 0 ->
		    ok
	    end,
	    receive
		{ok, _} ->
		    ok
	    after 0 ->
		    ok
	    end
    end.    
	

erl_script_timeout(Db) ->
    httpd_util:lookup(Db, erl_script_timeout, ?DEFAULT_ERL_TIMEOUT) * 1000.

script_elements(FuncAndInput, Input) ->
    case input_type(FuncAndInput) of
        path_info ->
	    [{path_info, Input}];
	query_string ->
	    [{query_string, Input}];
	_ ->
	    []
    end.

input_type([]) ->
    no_input;
input_type([$/|_Rest]) ->
    path_info;
input_type([$?|_Rest]) ->
    query_string;
input_type([_First|Rest]) ->
    input_type(Rest).

