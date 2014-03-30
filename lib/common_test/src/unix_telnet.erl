%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% @doc Callback module for ct_telnet for talking telnet
%%% to a unix host.
%%% 
%%% <p>It requires the following entry in the config file:</p>
%%% <pre>
%%% {unix,[{telnet,HostNameOrIpAddress},
%%%        {port,PortNum},                 % optional
%%%        {username,UserName},
%%%        {password,Password},
%%%        {keep_alive,Bool}]}.            % optional</pre>
%%%
%%% <p>To talk telnet to the host specified by
%%% <code>HostNameOrIpAddress</code>, use the interface functions in
%%% <code>ct</code>, e.g. <code>open(Name), cmd(Name,Cmd), ...</code>.</p>
%%%
%%% <p><code>Name</code> is the name you allocated to the unix host in
%%% your <code>require</code> statement. E.g.</p>
%%% <pre>   suite() -> [{require,Name,{unix,[telnet,username,password]}}].</pre>
%%% <p>or</p>
%%% <pre>   ct:require(Name,{unix,[telnet,username,password]}).</pre>
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

-compile(export_all).

%% Callbacks for ct_telnet.erl
-export([connect/6,get_prompt_regexp/0]).
-import(ct_telnet,[start_gen_log/1,log/4,end_gen_log/0]).

-define(username,"login: ").
-define(password,"Password: ").
-define(prx,"login: |Password: |\\\$ |> ").

%%%-----------------------------------------------------------------
%%% @hidden
%%% @spec get_prompt_regexp() -> PromptRegexp
%%%      PromptRegexp = ct_telnet:prompt_regexp()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Return the prompt regexp for telnet connections to the
%%% interwatch instrument.</p>
get_prompt_regexp() ->
    ?prx.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @spec connect(ConnName,Ip,Port,Timeout,KeepAlive,Extra) -> 
%%%   {ok,Handle} | {error,Reason}
%%%      ConnName = ct:target_name()
%%%      Ip = string() | {integer(),integer(),integer(),integer()}
%%%      Port = integer()
%%%      Timeout = integer()
%%%      KeepAlive = bool()
%%%      Extra = {Username,Password}
%%%      Username = string()
%%%      Password = string()
%%%      Handle = ct_telnet:handle()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Setup telnet connection to a UNIX host.</p>
connect(ConnName,Ip,Port,Timeout,KeepAlive,Extra) ->
    case Extra of
	{Username,Password} -> 
	    connect1(ConnName,Ip,Port,Timeout,KeepAlive,
		     Username,Password);
	KeyOrName ->
	    case get_username_and_password(KeyOrName) of
		{ok,{Username,Password}} ->
		    connect1(ConnName,Ip,Port,Timeout,KeepAlive,
			     Username,Password);
		Error ->
		    Error
	    end
    end.

connect1(Name,Ip,Port,Timeout,KeepAlive,Username,Password) ->
    start_gen_log("unix_telnet connect"),
    Result = 
	case ct_telnet_client:open(Ip,Port,Timeout,KeepAlive,Name) of
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
				Stars = lists:duplicate(length(Password),$*),
				log(Name,send,"Password: ~s",[Stars]),
				ok = ct_telnet_client:send_data(Pid,""),
				case ct_telnet:silent_teln_expect(Name,Pid,[],
								  prompt,
								  ?prx,[]) of
				    {ok,{prompt,Prompt},_} 
				    when Prompt=/=?username,
					 Prompt=/=?password ->
					{ok,Pid};
				    Error ->
					log(Name,recv,"Password failed\n~p\n",
					    [Error]),
					{error,Error}
				end;
			    Error ->
				log(Name,recv,"Login to ~p:~p failed\n~p\n",[Ip,Port,Error]),
				{error,Error}
			end;
		    {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} ->
			{ok,Pid};
		    Error ->
			log(Name,conn_error,
			    "Did not get expected prompt from ~p:~p\n~p\n",
			    [Ip,Port,Error]),
			{error,Error}
		end;
	    Error ->
		log(Name,conn_error,
		    "Could not open telnet connection to ~p:~p\n~p\n",
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
	    
