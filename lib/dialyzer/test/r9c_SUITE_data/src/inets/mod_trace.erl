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
%%     $Id: mod_trace.erl,v 1.1 2008/12/17 09:53:36 mikpe Exp $
%%
-module(mod_trace).

-export([do/1]).

-include("httpd.hrl").


do(Info) ->
    %%?vtrace("do",[]),
    case Info#mod.method of
	"TRACE" ->
	    case httpd_util:response_generated(Info) of
		false->
		    generate_trace_response(Info);
	        true->
		    {proceed,Info#mod.data}
	    end;
	_ ->
	    {proceed,Info#mod.data}
    end.


%%---------------------------------------------------------------------
%%Generate the trace response the trace response consists of a
%%http-header and the body will be the request.
%5----------------------------------------------------------------------

generate_trace_response(Info)->
    RequestHead=Info#mod.parsed_header,
    Body=generate_trace_response_body(RequestHead),
    Len=length(Body),
    Response=["HTTP/1.1 200 OK\r\n",
	      "Content-Type:message/http\r\n",
	      "Content-Length:",integer_to_list(Len),"\r\n\r\n",
	      Info#mod.request_line,Body],
    httpd_socket:deliver(Info#mod.socket_type,Info#mod.socket,Response),
    {proceed,[{response,{already_sent,200,Len}}|Info#mod.data]}.

generate_trace_response_body(Parsed_header)->
    generate_trace_response_body(Parsed_header,[]).

generate_trace_response_body([],Head)->
    lists:flatten(Head);
generate_trace_response_body([{[],[]}|Rest],Head) ->
    generate_trace_response_body(Rest,Head);
generate_trace_response_body([{Field,Value}|Rest],Head) ->
    generate_trace_response_body(Rest,[Field ++ ":" ++ Value ++ "\r\n"|Head]).
