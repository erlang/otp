%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

%%

-module(ssh_device).

%% api
-export(compile_all).

ssh_device(H, P, U, Pass, Cmd) ->
    ssh:start(),
    case ssh:connect(H, P,[{user, U}, {password, Pass},{silently_accept_hosts, true},{quiet_mode,true}]) of
    	{ok, Conn} ->
    		{ok, ChannelId} = ssh_connection:session_channel(Conn,infinity),
    		ssh_connection:exec(Conn, ChannelId, Cmd,infinity),
    		Init_rep = <<>>,
    		wait_for_response(Conn,H,Init_rep),
    		ssh:close(Conn);
    	{error,nxdomain}->
    	{ok,Try1}=ssh:connect(vnet:q_ip(H), P,[{user, U}, {password, Pass},{silently_accept_hosts, true},{quiet_mode,true}]),
    		{ok, ChannelId1} = ssh_connection:session_channel(Try1,infinity),
    		ssh_connection:exec(Try1, ChannelId1, Cmd,infinity),
    		Init_rep2 = <<>>,
    		wait_for_response(Try1,H,Init_rep2),
    		ssh:close(Try1)
    end.

wait_for_response(Conn,H,Acc) ->
    receive
      {ssh_cm, Conn, Msg} ->
          case Msg of
            {closed, _ChannelId} -> 
            	%file:write_file(string:concat("log\\",H), Acc, [append]),
            	io:format([">>>>>>>>>>>>>>>>>>> ",H," <<<<<<<<<<<<<<<<<\r\n",Acc,"\r\n"]);
            {data, _, _, A} ->
            	Acc2= <<Acc/binary,A/binary>>,
                wait_for_response(Conn,H,Acc2);
            _ -> 
            	%io:format("~p", [Msg]),
            	wait_for_response(Conn,H,Acc)
          end
    end.
