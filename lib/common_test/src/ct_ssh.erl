%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(ct_ssh).

%% SSH Functions
-export([connect/1, connect/2, connect/3,
 	 disconnect/1,
 	 session_open/1, session_open/2,
 	 session_close/2,
 	 send/3, send/4, send/5,
 	 receive_response/2, receive_response/3, receive_response/4,
 	 send_and_receive/3, send_and_receive/4, send_and_receive/5,
	 send_and_receive/6,
 	 exec/2, exec/3, exec/4,
         subsystem/3, subsystem/4,
         shell/2, shell/3]).

%% STFP Functions
-export([sftp_connect/1, 

	 read_file/2, write_file/3, list_dir/2, open/3, opendir/2, 
	 close/2, read/3, pread/4, aread/3, apread/4, write/3, 
	 pwrite/4, awrite/3, apwrite/4, position/3, read_file_info/2, 
	 get_file_info/2, read_link_info/2, write_file_info/3, 
	 read_link/2, make_symlink/3, rename/3, delete/2, make_dir/2, 
	 del_dir/2,

	 read_file/3, write_file/4, list_dir/3, open/4, opendir/3, 
	 close/3, read/4, pread/5, aread/4, apread/5, write/4, 
	 pwrite/5, awrite/4, apwrite/5, position/4, read_file_info/3, 
	 get_file_info/3, read_link_info/3, write_file_info/4, 
	 read_link/3, make_symlink/4, rename/4, delete/3, make_dir/3, 
	 del_dir/3]).

%% Callbacks
-export([init/3, handle_msg/2, reconnect/2, terminate/2, close/1]).

-define(DEFAULT_TIMEOUT, 10000).

-record(state, {ssh_ref, conn_type, target}).

-type handle() :: pid().

%%%-----------------------------------------------------------------
%%%------------------------ SSH COMMANDS ---------------------------

connect(KeyOrName) ->
    connect(KeyOrName, host).

connect(KeyOrName, ConnType) when is_atom(ConnType) ->
    connect(KeyOrName, ConnType, []);

connect(KeyOrName, ExtraOpts) when is_list(ExtraOpts) ->
    connect(KeyOrName, host, ExtraOpts).

connect(KeyOrName, ConnType, ExtraOpts) ->
    case ct:get_config(KeyOrName) of
	undefined ->
	    log(heading(connect,KeyOrName), "Failed: ~tp\n",
		[{not_available,KeyOrName}]),
	    {error,{not_available,KeyOrName}};
	SSHData ->
	    AllOpts = ExtraOpts++SSHData,
	    {ConnType1,Addr,AllOpts1} =
		case ConnType of
		    host ->
			case proplists:get_value(ssh, AllOpts) of
			    undefined ->
				case proplists:get_value(sftp, AllOpts) of
				    undefined ->
					log(heading(connect,KeyOrName), 
					    "No host information specified!\n",[]);
				    SFTPAddr ->
					{sftp,SFTPAddr,AllOpts}
				end;
			    SSHAddr ->
				{ssh,SSHAddr,AllOpts}
			end;
		    _ ->
			case proplists:get_value(ConnType, AllOpts) of
			    undefined when ConnType == ssh ->
				case proplists:get_value(sftp, AllOpts) of
				    undefined ->
					{ssh,undefined,AllOpts};
				    SFTPAddr ->
					try_log(heading(connect,KeyOrName), 
						"Note: Opening ssh connection "
						"to sftp host.\n",
					    []),
					{ssh,SFTPAddr,
					 [{ssh,SFTPAddr} |
					  proplists:delete(sftp, AllOpts)]}
				end;
			    undefined when ConnType == sftp ->
				case proplists:get_value(ssh, AllOpts) of
				    undefined ->
					{sftp,undefined,AllOpts};
				    SSHAddr ->
					try_log(heading(connect,KeyOrName), 
						"Note: Opening sftp connection "
						"to ssh host.\n",
					    []),
					{sftp,SSHAddr,
					 [{sftp,SSHAddr}|proplists:delete(ssh, AllOpts)]}
				end;
			    SSHorSFTPAddr ->
				{ConnType,SSHorSFTPAddr,AllOpts}
			end
		end,
	    case {Addr,proplists:get_value(port, AllOpts1)} of
		{undefined,_} ->
		    log(heading(connect,KeyOrName), "Failed: ~tp\n",
			[{not_available,{KeyOrName,ConnType1}}]),
		    {error,{not_available,{KeyOrName,ConnType1}}};
		{_,undefined} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~tp:22\n",
			    [ConnType1,Addr]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,22}, 
				      AllOpts1, ?MODULE);		    
		{_,Port} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~tp:~w\n",
			    [ConnType1,Addr,Port]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,Port}, 
				      AllOpts1, ?MODULE)
	    end
    end.

disconnect(SSH) ->
    case get_handle(SSH) of
	{ok,Pid} ->
	    try_log(heading(disconnect,SSH), "Handle: ~p", [Pid], 5000),
	    case ct_gen_conn:stop(Pid) of
		{error,{process_down,Pid,noproc}} ->
		    {error,already_closed};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.

session_open(SSH) ->
    call(SSH, {session_open,?DEFAULT_TIMEOUT}).

session_open(SSH, Timeout) ->
    call(SSH, {session_open,Timeout}).

session_close(SSH, ChannelId) ->
    call(SSH, {session_close,ChannelId}).

exec(SSH, Command) ->
    exec(SSH, undefined, Command, ?DEFAULT_TIMEOUT).

exec(SSH, Command, Timeout) when is_list(Command) ->
    exec(SSH, undefined, Command, Timeout);

exec(SSH, ChannelId, Command) when is_integer(ChannelId) ->
    exec(SSH, ChannelId, Command, ?DEFAULT_TIMEOUT).

exec(SSH, ChannelId, Command, Timeout) ->
    call(SSH, {exec,ChannelId,Command,Timeout}).

receive_response(SSH, ChannelId) ->
    receive_response(SSH, ChannelId, close, ?DEFAULT_TIMEOUT).

receive_response(SSH, ChannelId, End) when is_function(End) ->
    receive_response(SSH, ChannelId, End, ?DEFAULT_TIMEOUT);

receive_response(SSH, ChannelId, Timeout) when is_integer(Timeout) ->
    receive_response(SSH, ChannelId, close, Timeout).

receive_response(SSH, ChannelId, End, Timeout) ->
    call(SSH, {receive_response,ChannelId,End,Timeout}).

send(SSH, ChannelId, Data) ->
    send(SSH, ChannelId, 0, Data, ?DEFAULT_TIMEOUT).

send(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send(SSH, ChannelId, 0, Data, Timeout);

send(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send(SSH, ChannelId, Type, Data, ?DEFAULT_TIMEOUT).

send(SSH, ChannelId, Type, Data, Timeout) ->
    call(SSH, {send,ChannelId,Type,Data,Timeout}).

send_and_receive(SSH, ChannelId, Data) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, ?DEFAULT_TIMEOUT).

send_and_receive(SSH, ChannelId, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, ?DEFAULT_TIMEOUT);

send_and_receive(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, ?DEFAULT_TIMEOUT).

send_and_receive(SSH, ChannelId, Data, End, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, Timeout) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, Type, Data, End, ?DEFAULT_TIMEOUT).

send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) ->
    call(SSH, {send_and_receive,ChannelId,Type,Data,End,Timeout}).

subsystem(SSH, ChannelId, Subsystem) ->
    subsystem(SSH, ChannelId, Subsystem, ?DEFAULT_TIMEOUT).

subsystem(SSH, ChannelId, Subsystem, Timeout) ->
    call(SSH, {subsystem,ChannelId,Subsystem,Timeout}).


-spec shell(SSH, ChannelId) -> Result when
      SSH :: handle() | ct:target_name(),
      ChannelId :: ssh:ssh_channel_id(),
      Result :: ok | {error,term()}.
shell(SSH, ChannelId) ->
    shell(SSH, ChannelId, ?DEFAULT_TIMEOUT).

-spec shell(SSH, ChannelId, Timeout) -> Result when
      SSH :: handle() | ct:target_name(),
      ChannelId :: ssh:ssh_channel_id(),
      Timeout :: timeout(),
      Result :: ok | {error,term()}.
shell(SSH, ChannelId, Timeout) ->
    call(SSH, {shell,ChannelId,Timeout}).


%%%-----------------------------------------------------------------
%%%------------------------ SFTP COMMANDS --------------------------

sftp_connect(SSH) ->
    call(SSH, sftp_connect).

read_file(SSH, File) ->
    call(SSH, {read_file,sftp,File}).

read_file(SSH, Server, File) ->
    call(SSH, {read_file,Server,File}).

write_file(SSH, File, Iolist) ->
    call(SSH, {write_file,sftp,File,Iolist}).

write_file(SSH, Server, File, Iolist) ->
    call(SSH, {write_file,Server,File,Iolist}).

list_dir(SSH, Path) ->
    call(SSH, {list_dir,sftp,Path}).

list_dir(SSH, Server, Path) ->
    call(SSH, {list_dir,Server,Path}).

open(SSH, File, Mode) ->
    call(SSH, {open,sftp,File,Mode}).

open(SSH, Server, File, Mode) ->
    call(SSH, {open,Server,File,Mode}).

opendir(SSH, Path) ->
    call(SSH, {opendir,sftp,Path}).

opendir(SSH, Server, Path) ->
    call(SSH, {opendir,Server,Path}).

close(SSH, Handle) ->
    call(SSH, {close,sftp,Handle}).

close(SSH, Server, Handle) ->
    call(SSH, {close,Server,Handle}).

read(SSH, Handle, Len) ->
    call(SSH, {read,sftp,Handle,Len}).

read(SSH, Server, Handle, Len) ->
    call(SSH, {read,Server,Handle,Len}).

pread(SSH, Handle, Position, Length) ->
    call(SSH, {pread,sftp,Handle,Position,Length}).

pread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {pread,Server,Handle,Position,Length}).

aread(SSH, Handle, Len) ->
    call(SSH, {aread,sftp,Handle,Len}).

aread(SSH, Server, Handle, Len) ->
    call(SSH, {aread,Server,Handle,Len}).

apread(SSH, Handle, Position, Length) ->
    call(SSH, {apread,sftp,Handle,Position,Length}).

apread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {apread,Server,Handle,Position,Length}).

write(SSH, Handle, Data) ->
    call(SSH, {write,sftp,Handle,Data}).

write(SSH, Server, Handle, Data) ->
    call(SSH, {write,Server,Handle,Data}).

pwrite(SSH, Handle, Position, Data) ->
    call(SSH, {pwrite,sftp,Handle,Position,Data}).

pwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {pwrite,Server,Handle,Position,Data}).

awrite(SSH, Handle, Data) ->
    call(SSH, {awrite,sftp,Handle, Data}).

awrite(SSH, Server, Handle, Data) ->
    call(SSH, {awrite,Server,Handle, Data}).

apwrite(SSH, Handle, Position, Data) ->
    call(SSH, {apwrite,sftp,Handle,Position,Data}).

apwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {apwrite,Server,Handle,Position,Data}).

position(SSH, Handle, Location) ->
    call(SSH, {position,sftp,Handle,Location}).

position(SSH, Server, Handle, Location) ->
    call(SSH, {position,Server,Handle,Location}).

read_file_info(SSH, Name) ->
    call(SSH, {read_file_info,sftp,Name}).

read_file_info(SSH, Server, Name) ->
    call(SSH, {read_file_info,Server,Name}).

get_file_info(SSH, Handle) ->
    call(SSH, {get_file_info,sftp,Handle}).

get_file_info(SSH, Server, Handle) ->
    call(SSH, {get_file_info,Server,Handle}).

read_link_info(SSH, Name) ->
    call(SSH, {read_link_info,sftp,Name}).

read_link_info(SSH, Server, Name) ->
    call(SSH, {read_link_info,Server,Name}).

write_file_info(SSH, Name, Info) ->
    call(SSH, {write_file_info,sftp,Name,Info}).

write_file_info(SSH, Server, Name, Info) ->
    call(SSH, {write_file_info,Server,Name,Info}).

read_link(SSH, Name) ->
    call(SSH, {read_link,sftp,Name}).

read_link(SSH, Server, Name) ->
    call(SSH, {read_link,Server,Name}).

make_symlink(SSH, Name, Target) ->
    call(SSH, {make_symlink,sftp,Name,Target}).

make_symlink(SSH, Server, Name, Target) ->
    call(SSH, {make_symlink,Server,Name,Target}).

rename(SSH, OldName, NewName) ->
    call(SSH, {rename,sftp,OldName,NewName}).

rename(SSH, Server, OldName, NewName) ->
    call(SSH, {rename,Server,OldName,NewName}).


delete(SSH, Name) ->
    call(SSH, {delete,sftp,Name}).

delete(SSH, Server, Name) ->
    call(SSH, {delete,Server,Name}).

make_dir(SSH, Name) ->
    call(SSH, {make_dir,sftp,Name}).

make_dir(SSH, Server, Name) ->
    call(SSH, {make_dir,Server,Name}).

del_dir(SSH, Name) ->
    call(SSH, {del_dir,sftp,Name}).

del_dir(SSH, Server, Name) ->
    call(SSH, {del_dir,Server,Name}).


%%%=================================================================
%%% Callback functions
%%%=================================================================

init(KeyOrName, {ConnType,Addr,Port}, AllOpts) ->
    User = proplists:get_value(user, AllOpts),
    Password = case proplists:get_value(password, AllOpts) of
		   undefined -> "";
		   Pwd -> Pwd
	       end,
    AllOpts1 = case proplists:get_value(connect_timeout, AllOpts) of
		   undefined ->
		       [{connect_timeout,?DEFAULT_TIMEOUT}|AllOpts];
		   _ ->
		       AllOpts		      
	       end,
    Options =
	lists:foldl(fun({ssh,_},Opts) -> Opts;
		       ({sftp,_},Opts) -> Opts;
		       ({port,_},Opts) -> Opts;
		       ({silently_accept_hosts,_},Opts) -> Opts;
		       ({user_interaction,_},Opts) -> Opts;
		       (Opt={Key,_},Opts) -> 
			    case lists:keymember(Key, 1, Opts) of
				true -> Opts;
				false -> [Opt|Opts]
			    end;
		       (_,Opts) -> Opts
		    end, [], AllOpts1),
    FinalOptions = [{silently_accept_hosts,true},
		    {user_interaction,false} | Options],
    _ = crypto:start(),
    _ = ssh:start(),
    Result = case ConnType of
		 ssh ->
		     ssh:connect(Addr, Port, FinalOptions);
		 sftp ->
		     ssh_sftp:start_channel(Addr, Port, FinalOptions)
	     end,
    case Result of
	Error = {error,_} ->
	    Error;
	Ok ->
	    SSHRef = element(2, Ok),
	    try_log(heading(init,KeyOrName), 
		    "Opened ~w connection:\n"
		    "Host: ~tp (~p)\nUser: ~tp\nPassword: ~p\n",
		[ConnType,Addr,Port,User,
                 lists:duplicate(string:length(Password),$*)]),
	    {ok,SSHRef,#state{ssh_ref=SSHRef, conn_type=ConnType,
			      target=KeyOrName}}
    end.

handle_msg(sftp_connect, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(sftp_connect,Target), "SSH Ref: ~p", [SSHRef]),
    {ssh_sftp:start_channel(SSHRef),State};

handle_msg({session_open,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(session_open,Target), "SSH Ref: ~p, Timeout: ~p",
	    [SSHRef,TO]),
    {ssh_connection:session_channel(SSHRef, TO),State};

handle_msg({session_close,Chn}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(session_close,Target), "SSH Ref: ~p, Chn: ~p", [SSHRef,Chn]),
    {ssh_connection:close(SSHRef, Chn),State};

handle_msg({exec,Chn,Command,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    Chn1 = 
	if Chn == undefined ->
		try_log(heading(exec,Target), 
			"Opening channel for exec, SSH Ref: ~p", [SSHRef]),
		case ssh_connection:session_channel(SSHRef, TO) of	
		    {ok,C} -> C;
		    CErr -> CErr
		end;
	   true ->
		Chn
	end,
    case Chn1 of
	{error,_} = ChnError ->
	    log(heading(exec,Target), "Opening channel failed: ~tp", [ChnError]),
	    {ChnError,State};
	_ ->
	    try_log(heading(exec,Target), 
		    "SSH Ref: ~p, Chn: ~p, Command: ~tp, Timeout: ~p",
		    [SSHRef,Chn1,Command,TO]),
	    case ssh_connection:exec(SSHRef, Chn1, Command, TO) of
		success ->
		    Result = do_recv_response(SSHRef, Chn1, [], close, TO),
		    ssh_connection:close(SSHRef, Chn1),
		    {Result,State};
		Other ->
		    {{error,Other},State}
	    end
    end;

handle_msg({receive_response,Chn,End,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(receive_response,Target), 
	    "SSH Ref: ~p, Chn: ~p, Timeout: ~p", [SSHRef,Chn,TO]),
    Result = do_recv_response(SSHRef, Chn, [], End, TO),
    {Result,State};

handle_msg({send,Chn,Type,Data,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(send,Target), 
	    "SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~n"
	    "Data: ~tp", [SSHRef,Chn,Type,TO,Data]),
    Result = ssh_connection:send(SSHRef, Chn, Type, Data, TO),
    {Result,State};

handle_msg({send_and_receive,Chn,Type,Data,End,TO}, State) -> 
    #state{ssh_ref=SSHRef, target=Target} = State,   
    try_log(heading(send_and_receive,Target), 
	    "SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~n"
	    "Data: ~tp", [SSHRef,Chn,Type,TO,Data]),
    case ssh_connection:send(SSHRef, Chn, Type, Data, TO) of
	ok ->
	    Result = do_recv_response(SSHRef, Chn, [], End, TO),
	    {Result,State};
	Error ->
	    {Error,State}
    end;

handle_msg({subsystem,Chn,Subsystem,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(subsystem,Target), 
	    "SSH Ref: ~p, Chn: ~p, Subsys: ~tp, Timeout: ~p",
	    [SSHRef,Chn,Subsystem,TO]),
    Result = ssh_connection:subsystem(SSHRef, Chn, Subsystem, TO),
    {Result,State};

handle_msg({shell,Chn,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(shell,Target),
	    "SSH Ref: ~p, Chn: ~p, Timeout: ~p",
            [SSHRef,Chn,TO]),
    Result = ssh_connection:shell(SSHRef, Chn),
    {Result,State};

%% --- SFTP Commands ---

handle_msg({read_file,Srv,File}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file(ref(Srv,SSHRef), File),S};

handle_msg({write_file,Srv,File,Iolist}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file(ref(Srv,SSHRef), File, Iolist),S};

handle_msg({list_dir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:list_dir(ref(Srv,SSHRef), Path),S};

handle_msg({open,Srv,File,Mode}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:open(ref(Srv,SSHRef), File, Mode),S};

handle_msg({opendir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:opendir(ref(Srv,SSHRef), Path),S};

handle_msg({close,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:close(ref(Srv,SSHRef), Handle),S};

handle_msg({read,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({pread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pread(ref(Srv,SSHRef),Handle,Position,Length),S};

handle_msg({aread,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:aread(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({apread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apread(ref(Srv,SSHRef), Handle, Position, Length),S};

handle_msg({write,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({pwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({awrite,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:awrite(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({apwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({position,Srv,Handle,Location}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:position(ref(Srv,SSHRef), Handle, Location),S};

handle_msg({read_file_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file_info(ref(Srv,SSHRef), Name),S};

handle_msg({get_file_info,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:get_file_info(ref(Srv,SSHRef), Handle),S};

handle_msg({read_link_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link_info(ref(Srv,SSHRef), Name),S};

handle_msg({write_file_info,Srv,Name,Info}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file_info(ref(Srv,SSHRef), Name, Info),S};

handle_msg({read_link,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link(ref(Srv,SSHRef), Name),S};

handle_msg({make_symlink,Srv,Name,Target}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_symlink(ref(Srv,SSHRef), Name, Target),S};

handle_msg({rename,Srv,OldName,NewName}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:rename(ref(Srv,SSHRef), OldName, NewName),S};

handle_msg({delete,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:delete(ref(Srv,SSHRef), Name),S};

handle_msg({make_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_dir(ref(Srv,SSHRef), Name),S};

handle_msg({del_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:del_dir(ref(Srv,SSHRef), Name),S}.

reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ssh}.

close(SSHRef) ->
    disconnect(SSHRef).

terminate(SSHRef, State) ->
    case State#state.conn_type of
	ssh ->
	    try_log(heading(disconnect_ssh,State#state.target),
		    "SSH Ref: ~p",[SSHRef], 5000),
	    ssh:close(SSHRef);
	sftp ->
	    try_log(heading(disconnect_sftp,State#state.target),
		    "SFTP Ref: ~p",[SSHRef], 5000),
	    ssh_sftp:stop_channel(SSHRef)
    end.


%%%=================================================================
%%% Internal functions

do_recv_response(SSH, Chn, Data, End, Timeout) ->
    receive
	{ssh_cm, SSH, {open,Chn,RemoteChn,{session}}} ->
	    debug("RECVD open"),
	    {ok,{open,Chn,RemoteChn,{session}}};

	{ssh_cm, SSH, {closed,Chn}} ->
	    ssh_connection:close(SSH, Chn),
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {data,Chn,_,NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    debug("RECVD~n~tp", [binary_to_list(NewData)]),
	    DataAcc = Data ++ binary_to_list(NewData),
	    if is_function(End) ->
		    case End(DataAcc) of
			true -> 
			    {ok,DataAcc};
			false ->
			    do_recv_response(SSH, Chn, DataAcc, End, Timeout)
		    end;
	       true ->
		    do_recv_response(SSH, Chn, DataAcc, End, Timeout)
	    end;

	{ssh_cm, SSH, {eof,Chn}} ->
	    debug("RECVD EOF~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {exit_signal,Chn,Signal,Err,_Lang}} ->
	    debug("RECVD exit_signal~n~p ~p~n~p ~p", [SSH,Chn,Signal,Err]),
	    do_recv_response(SSH, Chn, Data, End, Timeout);
	%%	    {ok,{exit_signal,Chn,Signal,Err,_Lang}};

	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
	    debug("RECVD exit_status~n~p ~p~n~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Data, End, Timeout);
	%%	    {ok,{exit_status,Chn,_Status}};


	%%      --- INTERACTIVE MESSAGES - NOT HANDLED ---
	%%
	%% 	{ssh_cm, SSH, {subsystem,Chn,WantReply,Name}} ->
	%% 	    debug("RECVD SUBS WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH, Chn, Data, End, Timeout);

	%% 	{ssh_cm, SSH, {shell,WantReply}} ->
	%% 	    debug("RECVD SHELL WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH,Chn,Data,End,Timeout);

	%% 	{ssh_cm, SSH, {pty,Chn,WantReply,Pty}} ->
	%% 	    debug("RECVD PTY WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply,Pty]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH, Chn, Data, End, Timeout);

	%%	{ssh_cm, SSH, WCh={window_change,_Chn,_Width,_Height,_PixWidth,_PixHeight}} ->
	%%	    debug("RECVD WINCH"),
	%%	    {ok,WCh};

	Other ->
	    debug("UNEXPECTED MESSAGE~n~p ~p~n~tp", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data, End, Timeout)

    after Timeout ->
	    case End of
		timeout -> 
		    {ok,Data};
		_ -> 
		    {timeout,Data}
	    end
    end.

get_handle(SSH) when is_pid(SSH) ->
    {ok,SSH};
get_handle(SSH) ->
    case ct_util:get_connection(SSH, ?MODULE) of
	{ok,{Pid,_}} ->
	    {ok,Pid};
	{error,no_registered_connection} ->
	    connect(SSH);
	Error ->
	    Error
    end.

call(SSH, Msg) ->
    call(SSH, Msg, infinity).
	
call(SSH, Msg, Timeout) ->
    case get_handle(SSH) of
	{ok,Pid} ->
	    ct_gen_conn:call(Pid, Msg, Timeout);
	Error ->
	    Error
    end.

ref(sftp, SSHRef) -> SSHRef;
ref(Server, _) -> Server.

mod(Cmd) ->
    [Op,_Server|Args] = tuple_to_list(Cmd),
    list_to_tuple([Op|Args]).

heading(Function, Ref) ->
    io_lib:format("ct_ssh:~tw ~tp",[Function,Ref]).

log(Heading, Str, Args) ->
    ct_gen_conn:log(Heading, Str, Args).  

try_log(Heading, Str, Args) ->
    try_log(Heading, Str, Args, infinity).

try_log(Heading, Str, Args, Timeout) ->
    case ct_util:is_silenced(ssh, Timeout) of
	true ->
	    ok;
	false ->
	    ct_gen_conn:log(Heading, Str, Args);
	_Error ->
	    ok
    end.

debug(Str) ->
    debug(Str, []).

debug(_Str, _Args) ->
    %%    io:format("~n--- ct_ssh debug ---~n" ++ _Str ++ "~n", _Args),
    ok.
