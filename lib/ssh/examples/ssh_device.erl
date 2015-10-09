%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2015. All Rights Reserved.
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

-module(ssh_device).

%% api
-export([ssh_device/5]).

%%% I wrote this because of i think a fully ssh client sample will be easy to start the ssh module better than
%%% go though each function file.
ssh_device(Host, Port, User, Pass, Cmd) ->
    ssh:start(),
    case ssh:connect(Host, Port,
		     [{user, User}, {password, Pass},
		      {silently_accept_hosts, true}, {quiet_mode, true}])
	of
      {ok, Conn} ->
	  {ok, ChannelId} = ssh_connection:session_channel(Conn,
							   infinity),
	  ssh_connection:exec(Conn, ChannelId, Cmd, infinity),
	  Init_rep = <<>>,
	  wait_for_response(Conn, Host, Init_rep),
	  ssh:close(Conn);
      {error, nxdomain} ->
          {error,nxdomain}
    end.

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------
wait_for_response(Conn, Host, Acc) ->
    receive
      {ssh_cm, Conn, Msg} ->
	  case Msg of
	    {closed, _ChannelId} ->
		{ok,Acc};
	    {data, _, _, A} ->
		Acc2 = <<Acc/binary, A/binary>>,
		wait_for_response(Conn, Host, Acc2);
	    _ ->
		wait_for_response(Conn, Host, Acc)
	  end
    after 
    	5000 ->
    	        {error,timeout}
    end.
