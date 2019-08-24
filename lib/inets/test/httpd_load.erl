%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(httpd_load).

-include_lib("common_test/include/ct.hrl").

%% General testcases bodies called from httpd_SUITE
-export([load_test/5]).

%% Help functions 
-export([load_test_client/8]).

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
load_test(Type, Port, Host, Node,  NofTesters) ->
    URIs = 
	[
	 "/index.html", 
	 "/echo.shtml", 
	 "/",
	 "/flastmod.shtml", 
	 "/misc/"
	],
    Fun = fun(Mod, Host1, Port1, Node1, Req, Exp) -> 
		  ok = httpd_test_lib:verify_request(Mod, Host1, Port1,
						     Node1, Req, Exp)
	  end,
    load_test(Fun, URIs ++ URIs, Type, Host, Port, Node, NofTesters, []).
%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

load_test(_, _, _, _, _, _, 0, []) ->
    ok;
load_test(Fun, URIs, Type, Host, Port, Node,  0, List) ->
    receive 
	{Pid, done} ->
	    load_test(Fun, URIs, Type, Host, Port, Node,  0, 
		      lists:delete(Pid, List));
	{'EXIT', Pid, normal} ->
	    load_test(Fun, URIs, Type, Host, Port, Node,  0, 
		      lists:delete(Pid, List));
	{'EXIT', Pid, Reason} ->
	    Str = lists:flatten(io_lib:format("client ~p exited: ~p", 
					      [Pid,Reason])),
	    ct:fail(Str);
	_ ->
	    load_test(Fun, URIs, Type, Host, Port, Node,  0, List)
    end;

load_test(Fun, URIs, Type, Host, Port, Node,  X, List) ->
    Pid = spawn_link(?MODULE, load_test_client,
		     [Fun, URIs, Type,  Host,  Port,  Node,  self(), 100]),
    load_test(Fun, lists:reverse(URIs), Type, Host, Port, Node,  X-1,
	      [Pid | List]).

load_test_client(_Fun, [], _Type, _Host, _Port, _Node,  Boss, _Timeout) ->
    load_test_client_done(Boss);
load_test_client(Fun, [URI|URIs], Type, Host, Port, Node,  Boss, Timeout) ->  
    Req = "GET "++URI++" HTTP/1.0\r\nConnection: Close\r\n"
	"From: m@erix\r\nReferer: http://www.ericsson.se/\r\n\r\n",
    Timeout1 = 
	case (catch Fun(Type,  Host,  Port,  Node,  Req, 
			[{statuscode, 200}, {statuscode, 500}, 
			 {statuscode, 503}, {version, "HTTP/1.0"}])) of
	    {'EXIT', {suite_failed, connection_closed, _, _}} ->
		%% Some platforms seems to handle heavy load badly.
		%% So, back off and see if this helps
		2 * Timeout;
	    _ ->
		Timeout
	end,
    ct:sleep(Timeout1),
    load_test_client(Fun, URIs, Type, Host, Port, Node, Boss, Timeout1).

load_test_client_done(Boss) ->
    Boss ! {self(), done}.

