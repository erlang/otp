%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
%%%        {port,PortNum},
%%%        {username,UserName},
%%%        {password,Password}]}.
%%% </pre>
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
%%% <p>Note that the <code>{port,PortNum}</code> tuple is optional and if
%%% omitted, default telnet port 23 will be used.</p>
%%%
%%% @see ct
%%% @see ct_telnet
-module(unix_telnet).

-compile(export_all).

%% Callbacks for ct_telnet.erl
-export([connect/4,get_prompt_regexp/0]).
-import(ct_telnet,[start_log/1,cont_log/2,end_log/0]).

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
%%% @spec connect(Ip,Port,Timeout,Extra) -> {ok,Handle} | {error,Reason}
%%%      Ip = string() | {integer(),integer(),integer(),integer()}
%%%      Port = integer()
%%%      Timeout = integer()
%%%      Extra = {Username,Password}
%%%      Username = string()
%%%      Password = string()
%%%      Handle = ct_telnet:handle()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Setup telnet connection to a UNIX host.</p>
connect(Ip,Port,Timeout,Extra) ->
    case Extra of
	{Username,Password} -> 
	    connect(Ip,Port,Timeout,Username,Password);
	Name ->
	    case get_username_and_password(Name) of
		{ok,{Username,Password}} -> 
		    connect(Ip,Port,Timeout,Username,Password);
		Error -> 
		    Error
	    end
    end.

connect(Ip,Port,Timeout,Username,Password) ->
    start_log("unix_telnet:connect"),
    Result = 
	case ct_telnet_client:open(Ip,Port,Timeout) of
	    {ok,Pid} ->
		case ct_telnet:silent_teln_expect(Pid,[],[prompt],?prx,[]) of
		    {ok,{prompt,?username},_} ->
			ok = ct_telnet_client:send_data(Pid,Username),
			cont_log("Username: ~s",[Username]),
			case ct_telnet:silent_teln_expect(Pid,[],prompt,?prx,[]) of
			    {ok,{prompt,?password},_} ->
				ok = ct_telnet_client:send_data(Pid,Password),
				Stars = lists:duplicate(length(Password),$*),
				cont_log("Password: ~s",[Stars]),
				ok = ct_telnet_client:send_data(Pid,""),
				case ct_telnet:silent_teln_expect(Pid,[],prompt,
								  ?prx,[]) of
				    {ok,{prompt,Prompt},_} 
				    when Prompt=/=?username, Prompt=/=?password ->
					{ok,Pid};
				    Error ->
					cont_log("Password failed\n~p\n",
						 [Error]),
					{error,Error}
				end;
			    Error ->
				cont_log("Login failed\n~p\n",[Error]),
				{error,Error}
			end;
		    {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} ->
			{ok,Pid};
		    Error ->
			cont_log("Did not get expected prompt\n~p\n",[Error]),
			{error,Error}
		end;
	    Error ->
		cont_log("Could not open telnet connection\n~p\n",[Error]),
		Error
	end,
    end_log(),
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
	    
