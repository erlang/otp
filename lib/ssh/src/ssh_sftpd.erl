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

%%% Description: SFTP server daemon

-module(ssh_sftpd).

%%-behaviour(gen_server).
-behaviour(ssh_channel).

-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").
-include("ssh_xfer.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([subsystem_spec/1,
	 listen/1, listen/2, listen/3, stop/1]).

-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2, code_change/3]).

-record(state, {
	  xf,   			% [{channel,ssh_xfer states}...]
	  cwd,				% current dir (on first connect)
	  root,				% root dir
	  remote_channel,		% remote channel
	  pending,                      % binary() 
	  file_handler,			% atom() - callback module 
	  file_state,                   % state for the file callback module
	  max_files,                    % integer >= 0 max no files sent during READDIR
	  handles			% list of open handles
	  %% handle is either {<int>, directory, {Path, unread|eof}} or
	  %% {<int>, file, {Path, IoDevice}}
	 }).

%%====================================================================
%% API
%%====================================================================
subsystem_spec(Options) ->
    {"sftp", {?MODULE, Options}}.

%%% DEPRECATED START %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: listen() -> Pid | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
listen(Port) ->
    listen(any, Port, []).
listen(Port, Options) ->
    listen(any, Port, Options).
listen(Addr, Port, Options) ->
    SubSystems = [subsystem_spec(Options)],
    ssh:daemon(Addr, Port, [{subsystems, SubSystems} |Options]).

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh_cli:stop(Pid).


%%% DEPRECATED END %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% subsystem callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Options) ->
    {FileMod, FS0} = case proplists:get_value(file_handler, Options, 
					      {ssh_sftpd_file,[]}) of
			 {F, S} ->
			     {F, S};
			 F ->
			     {F, []}
		     end,
    
    {{ok, Default}, FS1} = FileMod:get_cwd(FS0),
    CWD = proplists:get_value(cwd, Options, Default),
    
    Root0 = proplists:get_value(root, Options, ""),
    
    %% Get the root of the file system (symlinks must be followed,
    %% otherwise the realpath call won't work). But since symbolic links
    %% isn't supported on all plattforms we have to use the root property
    %% supplied by the user.
    {Root, State} = 
	case resolve_symlinks(Root0, 
			      #state{root = Root0,
				     file_handler = FileMod, 
				     file_state = FS1}) of
	    {{ok, Root1}, State0} ->
		{Root1, State0};
	    {{error, _}, State0} ->
		{Root0, State0}
	end,
    MaxLength = proplists:get_value(max_files, Options, 0),

    Vsn = proplists:get_value(vsn, Options, 5),

    {ok,  State#state{cwd = CWD, root = Root, max_files = MaxLength,
		      handles = [], pending = <<>>,
		      xf = #ssh_xfer{vsn = Vsn, ext = []}}}.


%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, Type, Data}}, State) ->
    State1 = handle_data(Type, Data, State),
    {ok, State1};

handle_ssh_msg({ssh_cm, _, {eof, ChannelId}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    Report = io_lib:format("Connection closed by peer ~n Error ~p~n",
			   [Error]),
    error_logger:error_report(Report),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    
    Report = io_lib:format("Connection closed by peer ~n Status ~p~n",
			   [Status]),
    error_logger:error_report(Report),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles other messages
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId,  ConnectionManager}, 
	   #state{xf =Xf} = State) ->
    {ok,  State#state{xf = Xf#ssh_xfer{cm = ConnectionManager,
				       channel = ChannelId}}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_, #state{handles=Handles, file_handler=FileMod, file_state=FS}) ->
    CloseFun = fun({_, file, {_, Fd}}, FS0) ->
		       {_Res, FS1} = FileMod:close(Fd, FS0),
		       FS1;
		  (_, FS0) ->
		       FS0
	       end,
    lists:foldl(CloseFun, FS, Handles),
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_data(0, <<?UINT32(Len), Msg:Len/binary, Rest/binary>>, 
	    State = #state{pending = <<>>}) ->
    <<Op, ?UINT32(ReqId), Data/binary>> = Msg,
    NewState = handle_op(Op, ReqId, Data, State),
    case Rest of
	<<>> ->
	    NewState;   
	_ ->
	    handle_data(0, Rest, NewState)
    end;
	     
handle_data(0, Data, State = #state{pending = <<>>}) ->
    State#state{pending = Data};

handle_data(Type, Data, State = #state{pending = Pending}) -> 
     handle_data(Type, <<Pending/binary, Data/binary>>, 
                 State#state{pending = <<>>}).
 
handle_op(?SSH_FXP_INIT, Version, B, State) when is_binary(B) ->
    XF = State#state.xf,
    Vsn = lists:min([XF#ssh_xfer.vsn, Version]),
    XF1 = XF#ssh_xfer{vsn = Vsn},
    ssh_xfer:xf_send_reply(XF1, ?SSH_FXP_VERSION, <<?UINT32(Vsn)>>),
    State#state{xf = XF1};
handle_op(?SSH_FXP_REALPATH, ReqId,
	  <<?UINT32(Rlen), RPath:Rlen/binary>>,
	  State0) ->
    RelPath0 = binary_to_list(RPath),
    RelPath = relate_file_name(RelPath0, State0, _Canonicalize=false),
    {Res, State} = resolve_symlinks(RelPath, State0),
    case Res of
	{ok, AbsPath} ->
	    NewAbsPath = chroot_filename(AbsPath, State),
	    ?dbg(true, "handle_op ?SSH_FXP_REALPATH: RelPath=~p AbsPath=~p\n",
		 [RelPath, NewAbsPath]),
	    XF = State#state.xf,
	    Attr = #ssh_xfer_attr{type=directory},
	    ssh_xfer:xf_send_name(XF, ReqId, NewAbsPath, Attr),
	    State;
	{error, _} = Error ->
	    send_status(Error, ReqId, State)
    end;
handle_op(?SSH_FXP_OPENDIR, ReqId,
	 <<?UINT32(RLen), RPath:RLen/binary>>,
	  State0 = #state{xf = #ssh_xfer{vsn = Vsn}, 
			  file_handler = FileMod, file_state = FS0}) ->
    RelPath = binary_to_list(RPath),
    AbsPath = relate_file_name(RelPath, State0),
    
    XF = State0#state.xf,
    {IsDir, FS1} = FileMod:is_dir(AbsPath, FS0),
    State1 = State0#state{file_state = FS1},
    case IsDir of
	false when Vsn > 5 ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_NOT_A_DIRECTORY,
				    "Not a directory"),
	    State1;
	false ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_FAILURE,
				    "Not a directory"),
	    State1;
	true ->
	    add_handle(State1, XF, ReqId, directory, {RelPath,unread})
    end;
handle_op(?SSH_FXP_READDIR, ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary>>,
	  State) ->
    XF = State#state.xf,
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, directory, {_RelPath, eof}} ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_EOF),
	    State;
	{Handle, directory, {RelPath, Status}} ->
	    read_dir(State, XF, ReqId, Handle, RelPath, Status);
	_ ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_CLOSE,  ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary>>,
	  State = #state{handles = Handles, xf = XF,
			 file_handler = FileMod, file_state = FS0}) ->
    case get_handle(Handles, BinHandle) of
	{Handle, Type, T} ->
	    FS1 = case Type of
			     file ->
				 close_our_file(T, FileMod, FS0);
			     _ ->
				 FS0
			 end,
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_OK),
	    State#state{handles = lists:keydelete(Handle, 1, Handles),
			file_state = FS1};
	_ ->
	    ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_LSTAT, ReqId, Data, State) ->
    stat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State, read_link_info);
handle_op(?SSH_FXP_STAT, ReqId, Data, State) ->
    stat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State, read_file_info);
handle_op(?SSH_FXP_FSTAT, ReqId, Data, State) ->
    fstat((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State);
handle_op(?SSH_FXP_OPEN, ReqId, Data, State) ->
    open((State#state.xf)#ssh_xfer.vsn, ReqId, Data, State);
handle_op(?SSH_FXP_READ, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary,
				 ?UINT64(Offset), ?UINT32(Len)>>,
	  State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, file, {_Path, IoDevice}} ->
	    read_file(ReqId, IoDevice, Offset, Len, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, 
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_WRITE, ReqId,
	  <<?UINT32(HLen), BinHandle:HLen/binary, ?UINT64(Offset),
	   ?UINT32(Len), Data:Len/binary>>, State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, file, {_Path, IoDevice}} ->
	    write_file(ReqId, IoDevice, Offset, Data, State);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end;
handle_op(?SSH_FXP_READLINK, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State = #state{file_handler = FileMod, file_state = FS0}) ->
    RelPath = binary_to_list(BPath),
    AbsPath = relate_file_name(RelPath, State),
    {Res, FS1} = FileMod:read_link(AbsPath, FS0),
    case Res of
	{ok, NewPath} ->
	    ssh_xfer:xf_send_name(State#state.xf, ReqId, NewPath,
				  #ssh_xfer_attr{type=regular});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error))
    end,
    State#state{file_state = FS1};
handle_op(?SSH_FXP_SETSTAT, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				    Attr/binary>>, State0) ->
    Path = relate_file_name(BPath, State0),
    {Status, State1} = set_stat(Attr, Path, State0),
    send_status(Status, ReqId, State1);
handle_op(?SSH_FXP_MKDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary,
				  Attr/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    {Res, FS1} = FileMod:make_dir(Path, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	ok ->
	    {_, State2} = set_stat(Attr, Path, State1),
	    send_status(ok, ReqId, State2);
	{error, Error} ->
	    send_status({error, Error}, ReqId, State1)
    end;
handle_op(?SSH_FXP_FSETSTAT, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary, 
				     Attr/binary>>, 
	  State0 = #state{handles = Handles}) ->

    case get_handle(Handles, BinHandle) of
	{_Handle, _Type, {Path,_}} ->
	    {Status, State1} = set_stat(Attr, Path, State0),
	    send_status(Status, ReqId, State1);
	_ ->
	    ssh_xfer:xf_send_status(State0#state.xf, ReqId,
				    ?SSH_FX_INVALID_HANDLE),
	    State0
    end;
handle_op(?SSH_FXP_REMOVE, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    %%  case FileMod:is_dir(Path) of %% This version 6 we still have ver 5
    %% 	true ->
    %% 	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
    %% 				    ?SSH_FX_FILE_IS_A_DIRECTORY); 
    %% 	false ->
    {Status, FS1} = FileMod:delete(Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1);
    %%end;
handle_op(?SSH_FXP_RMDIR, ReqId, <<?UINT32(PLen), BPath:PLen/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    {Status, FS1} = FileMod:del_dir(Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1);

handle_op(?SSH_FXP_RENAME, ReqId,
  	  Bin = <<?UINT32(PLen), _:PLen/binary, ?UINT32(PLen2),
  		 _:PLen2/binary>>,
  	  State = #state{xf = #ssh_xfer{vsn = Vsn}}) when Vsn==3; Vsn==4  ->
    handle_op(?SSH_FXP_RENAME, ReqId, <<Bin/binary, 0:32>>, State);

handle_op(?SSH_FXP_RENAME, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2), 
	   BPath2:PLen2/binary, ?UINT32(Flags)>>,
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    Path2 = relate_file_name(BPath2, State0),
    case Flags band ?SSH_FXP_RENAME_ATOMIC of
	0 ->
	    case Flags band ?SSH_FXP_RENAME_OVERWRITE of
		0 ->
		    {Res, FS1} = FileMod:read_link_info(Path2, FS0),
		    State1 = State0#state{file_state = FS1},
		    case Res of
			{ok, _Info} ->
			    ssh_xfer:xf_send_status(
			      State1#state.xf, 
			      ReqId,
			      ?SSH_FX_FILE_ALREADY_EXISTS),
			    State1;
			_ ->
			    rename(Path, Path2, ReqId, State1)
		    end;
		_ ->
		    rename(Path, Path2, ReqId, State0)
	    end;
	_ ->
	    ssh_xfer:xf_send_status(State0#state.xf, ReqId,
				    ?SSH_FX_OP_UNSUPPORTED),
	    State0
    end;
handle_op(?SSH_FXP_SYMLINK, ReqId,
	  <<?UINT32(PLen), BPath:PLen/binary, ?UINT32(PLen2),
	   BPath2:PLen2/binary>>, 
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    Path = relate_file_name(BPath, State0),
    Path2 = relate_file_name(BPath2, State0),
    {Status, FS1} = FileMod:make_symlink(Path2, Path, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1).

new_handle([], H) ->
    H;
new_handle([{N, _} | Rest], H) when N > H ->
    new_handle(Rest, N+1);
new_handle([_ | Rest], H) ->
    new_handle(Rest, H).

add_handle(State, XF, ReqId, Type, DirFileTuple) ->
    Handles = State#state.handles,
    Handle = new_handle(Handles, 0),
    ssh_xfer:xf_send_handle(XF, ReqId, integer_to_list(Handle)),
    State#state{handles = [{Handle, Type, DirFileTuple} | Handles]}.
    
get_handle(Handles, BinHandle) ->
    case (catch list_to_integer(binary_to_list(BinHandle))) of
	I when is_integer(I) ->
	    case lists:keysearch(I, 1, Handles) of
		{value, T} -> T;
		false -> error
	    end;
	_ ->
	    error
    end.

%%% read_dir/5: read directory, send names, and return new state
read_dir(State0 = #state{file_handler = FileMod, max_files = MaxLength, file_state = FS0},
	 XF, ReqId, Handle, RelPath, {cache, Files}) ->
    AbsPath = relate_file_name(RelPath, State0),
    ?dbg(true, "read_dir: AbsPath=~p\n", [AbsPath]),
    if
	length(Files) > MaxLength ->
	    {ToSend, NewCache} = lists:split(MaxLength, Files),
	    {NamesAndAttrs, FS1} = get_attrs(AbsPath, ToSend, FileMod, FS0),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State0#state.handles,
				       {Handle, directory, {RelPath,{cache, NewCache}}}),
	    State0#state{handles = Handles, file_state = FS1};
	true ->
	    {NamesAndAttrs, FS1} = get_attrs(AbsPath, Files, FileMod, FS0),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State0#state.handles,
				       {Handle, directory, {RelPath,eof}}),
	    State0#state{handles = Handles, file_state = FS1}
    end;
read_dir(State0 = #state{file_handler = FileMod, max_files = MaxLength, file_state = FS0},
	 XF, ReqId, Handle, RelPath, _Status) ->
    AbsPath = relate_file_name(RelPath, State0),
    ?dbg(true, "read_dir: AbsPath=~p\n", [AbsPath]),
    {Res, FS1} = FileMod:list_dir(AbsPath, FS0),
    case Res of
	{ok, Files} when MaxLength == 0 orelse MaxLength > length(Files) ->
	    {NamesAndAttrs, FS2} = get_attrs(AbsPath, Files, FileMod, FS1),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State0#state.handles,
				       {Handle, directory, {RelPath,eof}}),
	    State0#state{handles = Handles, file_state = FS2};
	{ok, Files} ->
	    {ToSend, Cache} = lists:split(MaxLength, Files),
	    {NamesAndAttrs, FS2} = get_attrs(AbsPath, ToSend, FileMod, FS1),
	    ssh_xfer:xf_send_names(XF, ReqId, NamesAndAttrs),
	    Handles = lists:keyreplace(Handle, 1,
				       State0#state.handles,
				       {Handle, directory, {RelPath,{cache, Cache}}}),
	    State0#state{handles = Handles, file_state = FS2};
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
    end.


%%% get_attrs: get stat of each file and return
get_attrs(RelPath, Files, FileMod, FS) ->
    get_attrs(RelPath, Files, FileMod, FS, []).

get_attrs(_RelPath, [], _FileMod, FS, Acc) ->
    {lists:reverse(Acc), FS};
get_attrs(RelPath, [F | Rest], FileMod, FS0, Acc) ->
    Path = filename:absname(F, RelPath),
    ?dbg(true, "get_attrs fun: F=~p\n", [F]),
    case FileMod:read_link_info(Path, FS0) of
	{{ok, Info}, FS1} ->
	    Attrs = ssh_sftp:info_to_attr(Info),
	    get_attrs(RelPath, Rest, FileMod, FS1, [{F, Attrs} | Acc]);
	{{error, enoent}, FS1} ->
	    get_attrs(RelPath, Rest, FileMod, FS1, Acc);
	{Error, FS1} ->
	    {Error, FS1}
    end.

close_our_file({_,Fd}, FileMod, FS0) ->
    {_Res, FS1} = FileMod:close(Fd, FS0),
    FS1.

%%% stat: do the stat
stat(Vsn, ReqId, Data, State, F) when Vsn =< 3->
    <<?UINT32(BLen), BPath:BLen/binary>> = Data,
    stat(ReqId, binary_to_list(BPath), State, F);
stat(Vsn, ReqId, Data, State, F) when Vsn >= 4->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(_Flags)>> = Data,
    stat(ReqId, binary_to_list(BPath), State, F).

fstat(Vsn, ReqId, Data, State) when Vsn =< 3->
    <<?UINT32(HLen), Handle:HLen/binary>> = Data,
    fstat(ReqId, Handle, State);
fstat(Vsn, ReqId, Data, State) when Vsn >= 4->
    <<?UINT32(HLen), Handle:HLen/binary, ?UINT32(_Flags)>> = Data,
    fstat(ReqId, Handle, State).

fstat(ReqId, BinHandle, State) ->
    case get_handle(State#state.handles, BinHandle) of
	{_Handle, _Type, {Path, _}} ->
	    stat(ReqId, Path, State, read_file_info);
	_ ->
	    ssh_xfer:xf_send_status(State#state.xf, ReqId, 
				    ?SSH_FX_INVALID_HANDLE),
	    State
    end.

stat(ReqId, RelPath, State0=#state{file_handler=FileMod, 
				   file_state=FS0}, F) ->
    AbsPath = relate_file_name(RelPath, State0),
    XF = State0#state.xf,
    ?dbg(false, "stat: AbsPath=~p\n", [AbsPath]),
    {Res, FS1} = FileMod:F(AbsPath, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	{ok, FileInfo} ->
	    ssh_xfer:xf_send_attr(XF, ReqId, 
				  ssh_sftp:info_to_attr(FileInfo)),
	    State1;
	{error, E} ->
	    send_status({error, E}, ReqId, State1)
    end.

decode_4_open_flag(create_new) ->
    [write];
decode_4_open_flag(create_truncate) ->
    [write];
decode_4_open_flag(truncate_existing) ->
    [write];
decode_4_open_flag(open_existing) ->
    [read].

decode_4_flags([OpenFlag | Flags]) ->
    decode_4_flags(Flags, decode_4_open_flag(OpenFlag)).

decode_4_flags([], Flags) ->
    Flags;
decode_4_flags([append_data|R], _Flags) ->
    decode_4_flags(R, [append]);
decode_4_flags([append_data_atomic|R], _Flags) ->
    decode_4_flags(R, [append]);
decode_4_flags([_|R], Flags) ->
    decode_4_flags(R, Flags).

decode_4_access_flag(read_data) ->
    [read];
decode_4_access_flag(list_directory) ->
    [read];
decode_4_access_flag(write_data) ->
    [write];
decode_4_access_flag(add_file) ->
    [write];
decode_4_access_flag(add_subdirectory) ->
    [read];
decode_4_access_flag(append_data) ->
    [append];
decode_4_access_flag(_) ->
    [read].

decode_4_acess([_ | _] = Flags) ->
    lists:map(fun(Flag) -> 
		      [decode_4_access_flag(Flag)]
	      end, Flags);
decode_4_acess([]) ->
    [].

open(Vsn, ReqId, Data, State) when Vsn =< 3 ->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(PFlags),
     _Attrs/binary>> = Data,
    Path = binary_to_list(BPath),
    Flags = ssh_xfer:decode_open_flags(Vsn, PFlags) -- [creat, excl, trunc],
    ?dbg(true, "open: Flags=~p\n", [Flags]),
    do_open(ReqId, State, Path, Flags);
open(Vsn, ReqId, Data, State) when Vsn >= 4 ->
    <<?UINT32(BLen), BPath:BLen/binary, ?UINT32(Access),
     ?UINT32(PFlags), _Attrs/binary>> = Data,
    Path = binary_to_list(BPath),
    FlagBits = ssh_xfer:decode_open_flags(Vsn, PFlags),
    AcessBits = ssh_xfer:decode_ace_mask(Access),
    ?dbg(true, "open: Fl=~p\n", [FlagBits]),
    %% TODO: This is to make sure the Access flags are not ignored
    %% but this should be thought through better. This solution should
    %% be considered a hack in order to buy some time. At least
    %% it works better than when the Access flags where totally ignored.
    %% A better solution may need some code refactoring that we do
    %% not have time for right now.
    AcessFlags = decode_4_acess(AcessBits),
    Flags = lists:append(lists:umerge(
			   [[decode_4_flags(FlagBits)] | AcessFlags])),

    ?dbg(true, "open: Flags=~p\n", [Flags]),
    
    do_open(ReqId, State, Path, Flags).

do_open(ReqId, State0, Path, Flags) ->
    #state{file_handler = FileMod, file_state = FS0, root = Root} = State0,
    XF = State0#state.xf,
    F = [raw, binary | Flags],
    %%   case FileMod:is_dir(Path) of %% This is version 6 we still have 5
    %% 	true ->
    %% 	    ssh_xfer:xf_send_status(State#state.xf, ReqId,
    %% 				    ?SSH_FX_FILE_IS_A_DIRECTORY);
    %% 	false ->
    
    AbsPath = case Root of
		  "" ->
		      Path;
		  _ ->
		      relate_file_name(Path, State0)  
	      end,

    {Res, FS1} = FileMod:open(AbsPath, F, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	{ok, IoDevice} ->
	    add_handle(State1, XF, ReqId, file, {Path,IoDevice});
	{error, Error} ->
	    ssh_xfer:xf_send_status(State1#state.xf, ReqId,
				    ssh_xfer:encode_erlang_status(Error)),
	    State1
    end.

%% resolve all symlinks in a path
resolve_symlinks(Path, State) ->
    resolve_symlinks(Path, _LinkCnt=32, State).

resolve_symlinks(Path, LinkCnt, State0) ->
    resolve_symlinks_2(filename:split(Path), State0, LinkCnt, []).

resolve_symlinks_2(_Path, State, LinkCnt, _AccPath) when LinkCnt =:= 0 ->
    %% Too many links (there might be a symlink loop)
    {{error, emlink}, State};
resolve_symlinks_2(["." | RestPath], State0, LinkCnt, AccPath) ->
    resolve_symlinks_2(RestPath, State0, LinkCnt, AccPath);
resolve_symlinks_2([".." | RestPath], State0, LinkCnt, AccPath) ->
    %% Remove the last path component
    AccPathComps0 = filename:split(AccPath),
    Path =  case lists:reverse(tl(lists:reverse(AccPathComps0))) of
		[] ->
		    "";
		AccPathComps ->
		    filename:join(AccPathComps)
	    end,
    resolve_symlinks_2(RestPath, State0, LinkCnt, Path);
resolve_symlinks_2([PathComp | RestPath], State0, LinkCnt, AccPath0) ->
    #state{file_handler = FileMod, file_state = FS0} = State0,
    AccPath1 = filename:join(AccPath0, PathComp),
    {Res, FS1} = FileMod:read_link(AccPath1, FS0),
    State1 = State0#state{file_state = FS1},
    case Res of
	{ok, Target0} ->     % path is a symlink
	    %% The target may be a relative or an absolute path and
	    %% may contain symlinks
	    Target1 = filename:absname(Target0, AccPath0),
	    {FollowRes, State2} = resolve_symlinks(Target1, LinkCnt-1, State1),
 	    case FollowRes of
 		{ok, Target} ->
 		    resolve_symlinks_2(RestPath, State2, LinkCnt-1, Target);
 		{error, _} = Error ->
 		    {Error, State2}
 	    end;
	{error, einval} ->   % path exists, but is not a symlink
	    resolve_symlinks_2(RestPath, State1, LinkCnt, AccPath1);
	{error, _} = Error ->
	    {Error, State1}
    end;
resolve_symlinks_2([], State, _LinkCnt, AccPath) ->
    {{ok, AccPath}, State}.


relate_file_name(File, State) ->
    relate_file_name(File, State, _Canonicalize=true).

relate_file_name(File, State, Canonicalize) when is_binary(File) ->
    relate_file_name(binary_to_list(File), State, Canonicalize);
relate_file_name(File, #state{cwd = CWD, root = ""}, Canonicalize) ->
    relate_filename_to_path(File, CWD, Canonicalize);
relate_file_name(File, #state{root = Root}, Canonicalize) ->
    case is_within_root(Root, File) of
	true ->
	    File;
	false ->
	    RelFile = make_relative_filename(File),
	    NewFile = relate_filename_to_path(RelFile, Root, Canonicalize),
	    case is_within_root(Root, NewFile) of
		true ->
		    NewFile;
		false ->
		    Root
	    end
    end.

is_within_root(Root, File) ->
    lists:prefix(Root, File).

%% Remove leading slash (/), if any, in order to make the filename
%% relative (to the root)
make_relative_filename("/")       -> "./"; % Make it relative and preserve /
make_relative_filename("/"++File) -> File;
make_relative_filename(File)      -> File.

relate_filename_to_path(File0, Path, Canonicalize) ->
    File1 = filename:absname(File0, Path),
    File2 = if Canonicalize -> canonicalize_filename(File1);
	       true         -> File1
	    end,
    ensure_trailing_slash_is_preserved(File0, File2).

%% It seems as if the openssh client (observed with the
%% openssh-4.2p1-18.30 package on SLED 10), and possibly other clients
%% as well (Maverick?), rely on the fact that a trailing slash (/) is
%% preserved.  If trailing slashes aren't preserved, symlinks which
%% point at directories won't be properly identified as directories.
%%
%% A failing example: 
%%
%%    1) assume the following directory structure:
%%       $ mkdir /tmp/symlink-target
%%       $ touch /tmp/symlink-target/foo
%%       $ ln -s /tmp/symlink-target /tmp/symlink
%%
%%    2) login using the sftp client in openssh
%%       sftp> cd /tmp/
%%       sftp> ls symlink-target
%%       symlink-target/foo   
%%       sftp> ls symlink
%%       symlink/                 <===== foo should have been visible here
%%       sftp> cd symlink-target
%%       sftp> ls
%%       foo  
%%       sftp> cd ..
%%       sftp> cd symlink
%%       sftp> ls
%%                                <===== foo should have been visible here
%%
%% The symlinks are resolved by file:read_link_info/1 only if the path
%% has a trailing slash, which seems to something that some of the
%% sftp clients utilize:
%%
%%    1> file:read_link_info(".../symlink").
%%    {ok,{file_info,4,symlink,read_write,
%%                   {{2008,10,20},{10,25,26}},
%%                   {{2008,10,17},{16,22,33}},
%%                   {{2008,10,17},{16,22,33}},
%%                   41471,1,2053,0,570447,20996,9935}}
%%    
%%    2> file:read_link_info(".../symlink/").
%%    {ok,{file_info,8192,directory,read_write,
%%                   {{2008,10,20},{10,36,2}},
%%                   {{2008,10,20},{10,44,35}},
%%                   {{2008,10,20},{10,44,35}},
%%                   17407,29,2053,0,521224,0,0}}
ensure_trailing_slash_is_preserved(File0, File1) ->
    case {lists:suffix("/", File0), lists:suffix("/", File1)} of
	{true, false} -> File1 ++ "/";
	_Other        -> File1
    end.
	    
    

%%% fix file just a little: a/b/.. -> a and a/. -> a
canonicalize_filename(File0) ->
    File = filename:join(canonicalize_filename_2(filename:split(File0), [])),
    ensure_trailing_slash_is_preserved(File0, File).

canonicalize_filename_2([".." | Rest], ["/"] = Acc) ->
    canonicalize_filename_2(Rest, Acc);
canonicalize_filename_2([".." | Rest], [_Dir | Paths]) ->
    canonicalize_filename_2(Rest, Paths);
canonicalize_filename_2(["." | Rest], Acc) ->
    canonicalize_filename_2(Rest, Acc);
canonicalize_filename_2([A | Rest], Acc) ->
    canonicalize_filename_2(Rest, [A | Acc]);
canonicalize_filename_2([], Acc) ->
    lists:reverse(Acc).

%% return a filename which is relative to the root directory
%% (any filename that's outside the root directory is forced to the root)
chroot_filename(Filename, #state{root = Root}) ->
    FilenameComps0 = filename:split(Filename),
    RootComps = filename:split(Root),
    filename:join(chroot_filename_2(FilenameComps0, RootComps)).

chroot_filename_2([PathComp | FilenameRest], [PathComp | RootRest]) ->
    chroot_filename_2(FilenameRest, RootRest);
chroot_filename_2(FilenameComps, []) when length(FilenameComps) > 0 ->
    %% Ensure there's a leading / (filename:join above will take care
    %% of any duplicates)
    ["/" | FilenameComps];
chroot_filename_2(_FilenameComps, _RootComps) ->
    %% The filename is either outside the root or at the root.  In
    %% both cases we want to force the filename to the root.
    ["/"].


read_file(ReqId, IoDevice, Offset, Len,	
	  State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    {Res1, FS1} = FileMod:position(IoDevice, {bof, Offset}, FS0),
    case Res1 of
	{ok, _NewPos} ->
	    {Res2, FS2} = FileMod:read(IoDevice, Len, FS1),
	    State1 = State0#state{file_state = FS2},
	    case Res2 of
		{ok, Data} ->
		    ssh_xfer:xf_send_data(State1#state.xf, ReqId, Data),
		    State1;
		{error, Error} ->
		    send_status({error, Error}, ReqId, State1);
		eof ->
		    send_status(eof, ReqId, State1)
	    end;
	{error, Error} ->
    	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
    end.

write_file(ReqId, IoDevice, Offset, Data, 
	   State0 = #state{file_handler = FileMod, file_state = FS0}) ->
    {Res, FS1} = FileMod:position(IoDevice, {bof, Offset}, FS0),
    case Res of
	{ok, _NewPos} ->
	    {Status, FS2} = FileMod:write(IoDevice, Data, FS1),
	    State1 = State0#state{file_state = FS2},
	    send_status(Status, ReqId, State1);
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    send_status({error, Error}, ReqId, State1)
    end.

get_status(ok) ->
    ?SSH_FX_OK;
get_status(eof) ->
    ?SSH_FX_EOF;
get_status({error,Error}) ->
    ssh_xfer:encode_erlang_status(Error).

send_status(Status, ReqId, State) ->
    ssh_xfer:xf_send_status(State#state.xf, ReqId, get_status(Status)),
    State.

set_stat(<<>>, _Path, State) ->
    {ok, State};
set_stat(Attr, Path, 
	 State0 = #state{file_handler=FileMod, file_state=FS0}) ->
    {DecodedAttr, _Rest} = 
	ssh_xfer:decode_ATTR((State0#state.xf)#ssh_xfer.vsn, Attr),
    ?dbg(true, "set_stat DecodedAttr=~p\n", [DecodedAttr]),
    Info = ssh_sftp:attr_to_info(DecodedAttr),
    {Res1, FS1} = FileMod:read_link_info(Path, FS0),
    case Res1 of
	{ok, OldInfo} ->
	    NewInfo = set_file_info(Info, OldInfo),
	    ?dbg(true, "set_stat Path=~p\nInfo=~p\nOldInfo=~p\nNewInfo=~p\n",
		 [Path, Info, OldInfo, NewInfo]),
	    {Res2, FS2} = FileMod:write_file_info(Path, NewInfo, FS1),
	    State1 = State0#state{file_state = FS2},
	    {Res2, State1};
	{error, Error} ->
	    State1 = State0#state{file_state = FS1},
	    {{error, Error}, State1}
    end.


set_file_info_sel(undefined, F) ->
    F;
set_file_info_sel(F, _) ->
    F.

set_file_info(#file_info{atime = Dst_atime, mtime = Dst_mtime, 
			 ctime = Dst_ctime,
			 mode = Dst_mode, uid = Dst_uid, gid = Dst_gid},
	      #file_info{atime = Src_atime, mtime = Src_mtime, 
			 ctime = Src_ctime,
			 mode = Src_mode, uid = Src_uid, gid = Src_gid}) ->  
    #file_info{atime = set_file_info_sel(Dst_atime, Src_atime),
	       mtime = set_file_info_sel(Dst_mtime, Src_mtime),
	       ctime = set_file_info_sel(Dst_ctime, Src_ctime),
	       mode = set_file_info_sel(Dst_mode, Src_mode),
	       uid = set_file_info_sel(Dst_uid, Src_uid),
	       gid = set_file_info_sel(Dst_gid, Src_gid)}.

rename(Path, Path2, ReqId, State0) ->
    #state{file_handler = FileMod, file_state = FS0} = State0,
    {Status, FS1} = FileMod:rename(Path, Path2, FS0),
    State1 = State0#state{file_state = FS1},
    send_status(Status, ReqId, State1).
