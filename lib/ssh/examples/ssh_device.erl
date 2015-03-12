%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2015. All Rights Reserved.
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
