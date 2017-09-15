%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

%%% @doc Callback module for ct_telnet, for connecting to a telnet
%%% server on a unix host.
%%% 
%%% <p>It requires the following entry in the config file:</p>
%%% <pre>
%%% {unix,[{telnet,HostNameOrIpAddress},
%%%        {port,PortNum},                 % optional
%%%        {username,UserName},
%%%        {password,Password},
%%%        {keep_alive,Bool},              % optional
%%%        {tcp_nodely,Bool}]}             % optional</pre>
%%%
%%% <p>To communicate via telnet to the host specified by
%%% <code>HostNameOrIpAddress</code>, use the interface functions in
%%% <code>ct_telnet</code>, e.g. <code>open(Name), cmd(Name,Cmd), ...</code>.</p>
%%%
%%% <p><code>Name</code> is the name you allocated to the unix host in
%%% your <code>require</code> statement. E.g.</p>
%%% <pre>   suite() -> [{require,Name,{unix,[telnet]}}].</pre>
%%% <p>or</p>
%%% <pre>   ct:require(Name,{unix,[telnet]}).</pre>
%%%
%%% <p>The "keep alive" activity (i.e. that Common Test sends NOP to the server
%%% every 10 seconds if the connection is idle) may be enabled or disabled for one 
%%% particular connection as described here. It may be disabled for all connections
%%% using <c>telnet_settings</c> (see <c>ct_telnet</c>).</p>
%%%
%%% <p>Note that the <code>{port,PortNum}</code> tuple is optional and if
%%% omitted, default telnet port 23 will be used. Also the <c>keep_alive</c> tuple
%%% is optional, and the value defauls to true (enabled).</p>
%%%
%%% @see ct
%%% @see ct_telnet
-module(unix_telnet).

%% Callbacks for ct_telnet.erl
-export([connect/7,get_prompt_regexp/0]).
-import(ct_telnet,[start_gen_log/1,log/4,end_gen_log/0]).

-define(username,"login: ").
-define(password,"Password: ").
-define(prx,"login: |Password: |\\\$ |> ").

%%%-----------------------------------------------------------------
%%% @spec get_prompt_regexp() -> PromptRegexp
%%%      PromptRegexp = ct_telnet:prompt_regexp()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Return a suitable regexp string that will match common
%%% prompts for users on unix hosts.</p>
get_prompt_regexp() ->
    ?prx.


%%%-----------------------------------------------------------------
%%% @spec connect(ConnName,Ip,Port,Timeout,KeepAlive,Extra) -> 
%%%   {ok,Handle} | {error,Reason}
%%%      ConnName = ct:target_name()
%%%      Ip = string() | {integer(),integer(),integer(),integer()}
%%%      Port = integer()
%%%      Timeout = integer()
%%%      KeepAlive = bool()
%%%      TCPNoDelay = bool()
%%%      Extra = ct:target_name() | {Username,Password}
%%%      Username = string()
%%%      Password = string()
%%%      Handle = ct_telnet:handle()
%%%      Reason = term()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Setup telnet connection to a unix host.</p>
connect(ConnName,Ip,Port,Timeout,KeepAlive,TCPNoDelay,Extra) ->
    case Extra of
	{Username,Password} -> 
	    connect1(ConnName,Ip,Port,Timeout,KeepAlive,TCPNoDelay,
		     Username,Password);
	KeyOrName ->
	    case get_username_and_password(KeyOrName) of
		{ok,{Username,Password}} ->
		    connect1(ConnName,Ip,Port,Timeout,KeepAlive,TCPNoDelay,
			     Username,Password);
		Error ->
		    Error
	    end
    end.

connect1(Name,Ip,Port,Timeout,KeepAlive,TCPNoDelay,Username,Password) ->
    start_gen_log("unix_telnet connect"),
    Result = 
	case ct_telnet_client:open(Ip,Port,Timeout,KeepAlive,TCPNoDelay,Name) of
	    {ok,Pid} ->
		case ct_telnet:silent_teln_expect(Name,Pid,[],
						  [prompt],?prx,[]) of
		    {ok,{prompt,?username},_} ->
			log(Name,send,"Logging in to ~p:~p", [Ip,Port]),
			ok = ct_telnet_client:send_data(Pid,Username),
			log(Name,send,"Username: ~ts",[Username]),
			case ct_telnet:silent_teln_expect(Name,Pid,[],
							  prompt,?prx,[]) of
			    {ok,{prompt,?password},_} ->
				ok = ct_telnet_client:send_data(Pid,Password),
				Stars =
                                    lists:duplicate(string:length(Password),$*),
				log(Name,send,"Password: ~s",[Stars]),
%				ok = ct_telnet_client:send_data(Pid,""),
				case ct_telnet:silent_teln_expect(Name,Pid,[],
								  prompt,
								  ?prx,[]) of
				    {ok,{prompt,Prompt},_} 
				    when Prompt=/=?username,
					 Prompt=/=?password ->
					{ok,Pid};
				    Error ->
					log(Name,recv,"Password failed\n~tp\n",
					    [Error]),
					{error,Error}
				end;
			    Error ->
				log(Name,recv,"Login to ~p:~p failed\n~tp\n",[Ip,Port,Error]),
				{error,Error}
			end;
		    {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} ->
			{ok,Pid};
		    Error ->
			log(Name,conn_error,
			    "Did not get expected prompt from ~p:~p\n~tp\n",
			    [Ip,Port,Error]),
			{error,Error}
		end;
	    Error ->
		log(Name,conn_error,
		    "Could not open telnet connection to ~p:~p\n~tp\n",
		    [Ip,Port,Error]),
		Error
	end,
    end_gen_log(),
    Result.

get_username_and_password(Name) ->
    case ct:get_config({Name,username}) of
	undefined ->
	    {error,{no_username,Name}};
	Username ->
	    case ct:get_config({Name,password}) of
		undefined ->
		    {error,{no_password,Name}};
		Password ->
		    {ok,{Username,Password}}
	    end
    end.
	    
