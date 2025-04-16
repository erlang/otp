%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

-module(unix_telnet).
-moduledoc """
Callback module for `m:ct_telnet`, for connecting to a Telnet server on a UNIX
host.

It requires the following entry in the configuration file:

```erlang
{unix,[{telnet,HostNameOrIpAddress},
       {port,PortNum},                 % optional
       {username,UserName},
       {password,Password},
       {keep_alive,Bool}]}.            % optional
```

To communicate through Telnet to the host specified by `HostNameOrIpAddress`,
use the interface functions in `m:ct_telnet`, for example, `open(Name)` and
`cmd(Name,Cmd)`.

`Name` is the name you allocated to the Unix host in your `require` statement,
for example:

```erlang
suite() -> [{require,Name,{unix,[telnet]}}].
```

or

```erlang
ct:require(Name,{unix,[telnet]}).
```

The "keep alive" activity (that is, that `Common Test` sends NOP to the server
every 10 seconds if the connection is idle) can be enabled or disabled for one
particular connection as described here. It can be disabled for all connections
using `telnet_settings` (see `m:ct_telnet`).

The `{port,PortNum}` tuple is optional and if omitted, default Telnet port 23 is
used. Also the `keep_alive` tuple is optional, and the value default to `true`
(enabled).

## See Also

`m:ct`, `m:ct_telnet`
""".

%% Callbacks for ct_telnet.erl
-export([connect/7,get_prompt_regexp/0]).
-import(ct_telnet,[start_gen_log/1,log/4,end_gen_log/0]).

-define(username,"login: ").
-define(password,"Password: ").
-define(prx,"login: |Password: |\\\$ |> ").

-doc """
Callback for `ct_telnet.erl`.

Returns a suitable `regexp` string matching common prompts for users on Unix
hosts.
""".
-spec get_prompt_regexp() -> Pattern
              when Pattern :: ct_telnet:prompt_regexp().
get_prompt_regexp() ->
    ?prx.

-doc """
Callback for `ct_telnet.erl`.

[](){: #connect-6 }

Setup Telnet connection to a Unix host.
""".
-doc(#{since => <<"OTP 18.3.3">>}).
-spec connect(ConnName, Ip, Port, Timeout, KeepAlive, TCPNoDelay, Extra) ->
          {'ok', Handle} | {'error', Reason}
              when ConnName :: ct:target_name(),
                   Ip :: inet:socket_address() | inet:hostname(),
                   Port :: inet:port_number(),
                   Timeout :: timeout(),
                   KeepAlive :: boolean(),
                   TCPNoDelay :: boolean(),
                   Extra :: {Username, Password} | KeyOrName,
                   Username :: iodata(),
                   Password :: iodata(),
                   KeyOrName :: ct:key_or_name(),
                   Handle :: ct_telnet:handle(),
                   Reason :: term().
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
	    
