%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File        : CosFileTransfer_FileTransferSession_impl.erl
%% Description : 
%%
%% Created     : 12 Sept 2000
%%----------------------------------------------------------------------
-module('CosFileTransfer_FileTransferSession_impl').


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-include("cosFileTransferApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%% Interface functions
-export(['_get_protocols_supported'/2,
	 set_directory/3,
	 create_file/3,
	 create_directory/3,
	 get_file/3,
	 delete/3,
	 transfer/4,
	 append/4,
	 insert/5,
	 logout/2]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([oe_orber_create_directory_current/2, oe_orber_get_content/4,
	 oe_orber_count_children/3]).
-export([invoke_call/3]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {protocols, server, type, current, module, connection, mytype, 
		connection_timeout}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(create_InitState(P, S, T, C, M, Co, Ty, CT),
	#state{protocols=P, server=S, type=T, current=C, module=M, connection=Co,
	       mytype=Ty, connection_timeout=CT}).

-define(get_Protocols(S),         S#state.protocols).
-define(get_Server(S),            S#state.server).
-define(get_CurrentDir(S),        S#state.current).
-define(get_Module(S),            S#state.module).
-define(get_Connection(S),        S#state.connection).
-define(get_MyType(S),            S#state.mytype).
-define(get_ConnectionTimeout(S), S#state.connection_timeout).
-define(set_CurrentDir(S, C),     S#state{current=C}).

-define(is_FTP(S),                S#state.type=='FTP').
-define(is_FTAM(S),               S#state.type=='FTAM').
-define(is_NATIVE(S),             S#state.type=='NATIVE').
-define(is_ORBER_NATIVE(S),       S#state.module==cosFileTransferNATIVE_file).


%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init(['FTP', Host, Port, User, Password, _Account, Protocol, Timeout]) ->
    {ok, Pid} = inets:start(ftpc, [{host, Host}, {port, Port}], stand_alone),
    ok = ftp:user(Pid, User, Password),
    {ok, PWD} = ftp:pwd(Pid),
    {Connection, ProtocolSupport} = setup_local(Protocol),
    {ok, ?create_InitState(ProtocolSupport, Pid, 'FTP', 
			   PWD, ftp, Connection, Protocol, Timeout)};
init([{'NATIVE', Mod}, Host, Port, User, Password, _Account, Protocol, Timeout]) ->
    {ok, Pid} = Mod:open(Host, Port),
    ok = Mod:user(Pid, User, Password),
    {ok, PWD} = Mod:pwd(Pid),
    {Connection, ProtocolSupport} = setup_local(Protocol),
    {ok, ?create_InitState(ProtocolSupport, Pid, 'NATIVE', 
			   PWD, Mod, Connection, Protocol, Timeout)}.
    

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, #state{type = Type, server = Server, module = Mod} = State) ->
    case ?get_MyType(State) of
	ssl ->
	    catch ssl:close(?get_Connection(State));
	_ ->
	    catch gen_tcp:close(?get_Connection(State))
    end,
    case Type of
	'FTP' ->
	    inets:stop(ftpc, Server);
	'NATIVE' ->
	    Mod:close(Server);
	_ ->
	    ok
    end,
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% function : handle_info/2
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
        {'EXIT', _Pid, Reason} ->
            {stop, Reason, State};
        _Other ->
            {noreply, State}
    end.
 
%%======================================================================
%% CosFileTransfer::FileTransferSession
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : _get_protocols_supported
%% Arguments  : 
%% Returns    : A list of CosFileTransfer::ProtocolSupport, i.e.,
%%              struct ProtocolSupport { 
%%                  Istring protocol_name; 
%%                  ProtocolAddressList addresses; %% eq a list of strings.
%%              }; 
%% Description: 
%%----------------------------------------------------------------------
'_get_protocols_supported'(_OE_This, State) ->
    {reply, ?get_Protocols(State), State}.

%%----------------------------------------------------------------------
%% Function   : set_directory
%% Arguments  : Directory - CosFileTransfer::Directory
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
set_directory(_OE_This, State, Directory)  when ?is_FTP(State); ?is_NATIVE(State) ->
    Mod  = ?get_Module(State),
    Path = filename:join('CosFileTransfer_Directory':
			 '_get_complete_file_name'(Directory)),
    case catch Mod:cd(?get_Server(State), Path) of
	ok ->
	    {reply, ok, ?set_CurrentDir(State, Path)};
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="Directory not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not loggen in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	_ ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason = "Unexpected error."})
    end.

%%----------------------------------------------------------------------
%% Function   : create_file
%% Arguments  : FileNameList
%% Returns    : File
%% Description: This operation creates a File Object representing a 
%%              file which may or may not exist. Typically used as
%%              argument when invoking transfer/3. See also get_file/2.
%%----------------------------------------------------------------------
create_file(OE_This, State, FileNameList) ->
    {reply, cosFileTransferApp:create_file(OE_This, FileNameList), State}.

%%----------------------------------------------------------------------
%% Function   : create_directory
%% Arguments  : FileNameList - full path name.
%% Returns    : Directory
%% Description: 
%%----------------------------------------------------------------------
create_directory(OE_This, State, FileNameList) when ?is_FTP(State); 
						    ?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    case Mod:mkdir(?get_Server(State), filename:join(FileNameList)) of
	ok ->
	    {reply, cosFileTransferApp:create_dir(OE_This, FileNameList), State};
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="Directory not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not loggen in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	 _ ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Unknown error."})
    end.


%%----------------------------------------------------------------------
%% Function   : get_file
%% Arguments  : FileNameList
%% Returns    : FileWrapper
%% Description: This operation should be independent of the working Directory,
%%              i.e., a full path name must be supplied. The file or 
%%              directory the returned object is supposed to represent
%%              MUST(!!!!) exist.
%%----------------------------------------------------------------------
get_file(OE_This, State, FileNameList) when ?is_FTP(State); 
					    ?is_NATIVE(State) ->
    case check_type(OE_This, State, filename:join(FileNameList)) of
	{ndirectory, _Listing} ->
	    {reply, 
	     #'CosFileTransfer_FileWrapper'{the_file = 
					    cosFileTransferApp:
					    create_dir(OE_This, 
						       FileNameList),
					    file_type = ndirectory}, 
	     State};
	nfile ->
	    {reply, 
	     #'CosFileTransfer_FileWrapper'{the_file = 
					    cosFileTransferApp:
					    create_file(OE_This,
							FileNameList), 
					    file_type = nfile}, 
	     State};
	Other ->
	    %% If we want to return {stop, ....}
	    Other
    end.

%%----------------------------------------------------------------------
%% Function   : delete
%% Arguments  : File
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
delete(_OE_This, State, File) when ?is_FTP(State); ?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    Result =
	case 'CosPropertyService_PropertySet':
	    get_property_value(File, "is_directory") of
	    #any{value=false} ->
		Mod:delete(?get_Server(State), 
			   filename:join('CosFileTransfer_File':
					 '_get_complete_file_name'(File)));
	    #any{value=true} ->
		Mod:rmdir(?get_Server(State),
			  filename:join('CosFileTransfer_File':
					'_get_complete_file_name'(File)));
	    Other ->
		Other
	end,
    case Result of
	ok ->
	    {reply, ok, State};
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="File or Directory not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not loggen in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	_ ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Unknown error."})
    end.

%%----------------------------------------------------------------------
%% Function   : transfer
%% Arguments  : SrcFile eq DestFile eq CosFileTransfer::File
%% Returns    : -
%% Description: DestFile must be a newly created File object, using create_file()
%%              on the Target FileTransferSession, prior to calling transfer().
%%----------------------------------------------------------------------
transfer(OE_This, State, SrcFile, DestFile) when ?is_ORBER_NATIVE(State) ->
    case which_FTS_type(OE_This, SrcFile, DestFile) of
	{source, TargetFTS} ->
	    %% The source FTS is supposed to be the active one, set up a connection.
	    Protocols = 'CosFileTransfer_FileTransferSession':
		        '_get_protocols_supported'(TargetFTS),
	    SrcName  = 'CosFileTransfer_File':'_get_complete_file_name'(SrcFile),
	    Pid = spawn(?MODULE, invoke_call, [self(), transfer, 
					       [TargetFTS, SrcFile, DestFile]]), 
	    send_file(Protocols, ?get_MyType(State), ?get_ConnectionTimeout(State),
		      filename:join(SrcName)),
	    check_reply(Pid),
	    {reply, ok, State};
	{target, _SourceFTS} ->
	    DestName = 'CosFileTransfer_File':'_get_complete_file_name'(DestFile),
	    receive_file(?get_MyType(State), ?get_Connection(State), 
			 ?get_ConnectionTimeout(State),
			 filename:join(DestName), write),
	    {reply, ok, State}
    end;
transfer(OE_This, State, SrcFile, DestFile) when ?is_FTP(State); ?is_NATIVE(State) ->
    case which_FTS_type(OE_This, SrcFile, DestFile) of
	{source, TargetFTS} ->
	    source_FTS_operation(State, SrcFile, DestFile, transfer, 0, TargetFTS);
	{target, _SourceFTS} ->
	    target_FTS_operation(State, SrcFile, DestFile, send, 0)
    end.


%%----------------------------------------------------------------------
%% Function   : append
%% Arguments  : SrcFile eq DestFile eq CosFileTransfer::File
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
append(OE_This, State, SrcFile, DestFile) when ?is_ORBER_NATIVE(State) ->
    case which_FTS_type(OE_This, SrcFile, DestFile) of
	{source, TargetFTS} ->
	    SrcName  = filename:join('CosFileTransfer_File':
				     '_get_complete_file_name'(SrcFile)),
	    check_type(OE_This, State, SrcName),
	    %% The source FTS is supposed to be the active one, set up a connection.
	    Protocols = 'CosFileTransfer_FileTransferSession':
		        '_get_protocols_supported'(TargetFTS),
	    Pid = spawn(?MODULE, invoke_call, [self(), append, 
					       [TargetFTS, SrcFile, DestFile]]), 
	    send_file(Protocols, ?get_MyType(State), ?get_ConnectionTimeout(State),
		      SrcName),
	    check_reply(Pid),
	    {reply, ok, State};
	{target, _SourceFTS} ->
	    DestName = filename:join('CosFileTransfer_File':
				     '_get_complete_file_name'(DestFile)),
	    check_type(OE_This, State, DestName),
	    receive_file(?get_MyType(State), ?get_Connection(State),
			 ?get_ConnectionTimeout(State), DestName, append),
	    {reply, ok, State}
    end;
append(OE_This, State, SrcFile, DestFile) when ?is_NATIVE(State) ->
    case which_FTS_type(OE_This, SrcFile, DestFile) of
	{source, TargetFTS} ->
	    source_FTS_operation(State, SrcFile, DestFile, append, 0, TargetFTS);
	{target, _SourceFTS} ->
	    target_FTS_operation(State, SrcFile, DestFile, append, 0)
    end;
append(_OE_This, _State, _SrcFile, _DestFile) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).


%%----------------------------------------------------------------------
%% Function   : insert
%% Arguments  : SrcFile eq DestFile eq CosFileTransfer::File
%%              Offset - long
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
insert(OE_This, State, SrcFile, DestFile, Offset) when ?is_NATIVE(State) ->
    case which_FTS_type(OE_This, SrcFile, DestFile) of
	{source, TargetFTS} when ?is_ORBER_NATIVE(State) ->
	    SrcName  = 'CosFileTransfer_File':'_get_complete_file_name'(SrcFile),
	    check_type(OE_This, State, filename:join(SrcName)),
	    %% The source FTS is supposed to be the active one, set up a connection.
	    Protocols = 'CosFileTransfer_FileTransferSession':
		'_get_protocols_supported'(TargetFTS),
	    Pid = spawn(?MODULE, invoke_call, [self(), insert, 
					       [TargetFTS, SrcFile, 
						DestFile, Offset]]),
	    send_file(Protocols, ?get_MyType(State), ?get_ConnectionTimeout(State),
		      filename:join(SrcName)),
	    check_reply(Pid),
	    {reply, ok, State};
	{source, TargetFTS} ->
	    source_FTS_operation(State, SrcFile, DestFile, insert, Offset, TargetFTS);
	{target, _SourceFTS} ->
	    target_FTS_operation(State, SrcFile, DestFile, insert, Offset)
    end;
insert(_OE_This, _State, _SrcFile, _DestFile, _Offset) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).


%%----------------------------------------------------------------------
%% Function   : logout
%% Arguments  : -
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
logout(_OE_This, State) when ?is_FTP(State); ?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    catch Mod:close(?get_Server(State)),
    {stop, normal, ok, State}.

%%======================================================================
%% Internal functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : oe_orber_create_directory_current
%% Arguments  : -
%% Returns    : Directory
%% Description: Creates a Directory describing the working directory
%%              of the remote server, e.g., an FTP-server.
%%----------------------------------------------------------------------
oe_orber_create_directory_current(OE_This, State)  when ?is_FTP(State); 
							?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    FileNameList = filename:split(?get_CurrentDir(State)),
    case Mod:nlist(?get_Server(State), ?get_CurrentDir(State)) of
	{ok, _Listing} ->
	    {reply, cosFileTransferApp:create_dir(OE_This, FileNameList), 
	     State};
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="Directory not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not loggen in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	 _ ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Unknown error."})
    end.
%%----------------------------------------------------------------------
%% Function   : oe_orber_get_content
%% Arguments  : -
%% Returns    : string
%% Description: 
%%----------------------------------------------------------------------
oe_orber_get_content(OE_This, State, FileNameList, Parent)  when ?is_FTP(State); 
								 ?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    case Mod:nlist(?get_Server(State), filename:join(FileNameList)) of
	{ok, Listing} ->
	    create_content(Listing, OE_This, State, Parent, FileNameList);
	{error, epath} ->
	    {reply, [], State};
	_ ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="Directory not found."})
    end.

%%----------------------------------------------------------------------
%% Function   : oe_orber_count_children
%% Arguments  : -
%% Returns    : string
%% Description: 
%%----------------------------------------------------------------------
oe_orber_count_children(OE_This, State, FileNameList)  when ?is_FTP(State); 
							 ?is_NATIVE(State) ->
    case catch check_type(OE_This, State, filename:join(FileNameList)) of
	{ndirectory, Members} ->
	    {reply, length(Members), State};
	{stop, normal, _, _} ->
	    {stop, normal, 
	     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}, 
	     State};
	_->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : delete_tmp_file
%% Arguments  : -
%% Returns    : ok | {'EXCEPTION', E}
%% Description: 
%%----------------------------------------------------------------------
delete_tmp_file(TmpFileName, ErrorMsg) ->
    case file:delete(TmpFileName) of
	ok ->
	    ok;
	_ ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'{reason=ErrorMsg})
    end.


%%----------------------------------------------------------------------
%% Function   : invoke_call
%% Arguments  : -
%% Returns    : ok | {'EXCEPTION', E}
%% Description: 
%%----------------------------------------------------------------------
invoke_call(Pid, Op, Args) ->
    Result = (catch apply('CosFileTransfer_FileTransferSession', Op, Args)),
    Pid ! {transfer_result, self(), Result},
    ok.

%%----------------------------------------------------------------------
%% Function   : check_reply
%% Arguments  : Pid - the pid of the spawned process.
%% Returns    : ok | {'EXCEPTION', E}
%% Description: 
%%----------------------------------------------------------------------
check_reply(Pid) ->
    receive 
	{transfer_result, Pid, ok} ->
	    ok;
	{transfer_result, Pid, {'EXCEPTION', E}} ->
	    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:check_reply();
Raised exception: ", [?LINE, E], ?DEBUG_LEVEL),
	    corba:raise(E);
	{transfer_result, Pid, {'EXIT', Reason}} ->
	    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:check_reply();
Got EXIT-signal with reason: ", [?LINE, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{minor=199, 
				    completion_status=?COMPLETED_NO})
    after infinity ->
	    %% Should we add an exception here or do we reuse the iiop_timeout?
	    %% For now keep as is.
	    corba:raise(#'INTERNAL'{minor=199, 
				    completion_status=?COMPLETED_NO})
    end.


%%----------------------------------------------------------------------
%% Function   : which_FTS_type
%% Arguments  : -
%% Returns    : {source, FTS} | {target, FTS} | {'EXCEPTION', #'BAD_PARAM'{}}
%% Description: Used to determine if the target FTS is supposed to act
%%              as sender or receiver and also return the counter part FTS. 
%%              An exception is raised if the user supplied incorrect parameters.
%%----------------------------------------------------------------------
which_FTS_type(OE_This, SrcFile, DestFile) ->
    TargetFTS = 'CosFileTransfer_File':'_get_associated_session'(DestFile),
    SourceFTS = 'CosFileTransfer_File':'_get_associated_session'(SrcFile),
    case corba_object:is_equivalent(OE_This, TargetFTS) of
	true ->
	    {target, SourceFTS};
	false ->	    
	    case corba_object:is_equivalent(OE_This, SourceFTS) of
		true ->
		    {source, TargetFTS};
		false ->
		  corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end
    end.


%%----------------------------------------------------------------------
%% Function   : setup_connection
%% Arguments  : A list of #'CosFileTransfer_ProtocolSupport'{}
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
setup_connection([], Protocol, _) ->
    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:setup_connection(~p);
The Protocols listed are not supported.", [?LINE, Protocol], ?DEBUG_LEVEL),
    corba:raise(#'CosFileTransfer_TransferException'{reason="Unsupported protocol"});
setup_connection([#'CosFileTransfer_ProtocolSupport'{protocol_name="TCP/IP", 
						     addresses=Addr}|_], 
		 tcp, Timeout) ->
    setup_connection_helper(Addr, gen_tcp, [], Timeout);
setup_connection([#'CosFileTransfer_ProtocolSupport'{protocol_name="SSL", 
						     addresses=Addr}|_], 
		 ssl, Timeout) ->
    Options = [{certfile, cosFileTransferApp:ssl_client_certfile()},
	       {verify, cosFileTransferApp:ssl_client_verify()},
	       {depth, cosFileTransferApp:ssl_client_depth()}] ++ 
	ssl_client_cacertfile_option(),
    setup_connection_helper(Addr, ssl, Options, Timeout);
setup_connection([_|T], Type, Timeout) ->
    setup_connection(T, Type, Timeout).

setup_connection_helper([], _, _, _) ->
    corba:raise(#'CosFileTransfer_RequestFailureException'
		{reason="Unable to contact remote server."});
setup_connection_helper([H|T], Driver, Options, Timeout) ->
    case string:tokens(H, ":") of
	[Host, Port] when Driver == gen_tcp ->
	    case gen_tcp:connect(Host, list_to_integer(Port), 
				 [binary, 
				  {packet, raw}, 
				  {reuseaddr, true}, 
				  {nodelay, true}|Options], Timeout) of
		{ok, Sock} ->
		    {gen_tcp, Sock};
		_->
		    %% No response.
		    setup_connection_helper(T, Driver, Options, Timeout)
	    end;
	[Host, Port] when Driver == ssl ->
	    case ssl:connect(Host, list_to_integer(Port), 
			     [binary,
			      {packet, 0},
			      {active, false}|Options], Timeout) of
		{ok, Sock} ->
		    {ssl, Sock};
		_->
		    %% No response.
		    setup_connection_helper(T, Driver, Options, Timeout)
	    end;
	_ ->
	    %% Badly configured address.
	    setup_connection_helper(T, Driver, Options, Timeout)
    end.

ssl_client_cacertfile_option() ->
    case cosFileTransferApp:ssl_client_cacertfile() of
	[] ->
	    [];
	X when is_list(X) ->
	    {cacertfile, X};
	_ ->
	    []
    end.

%%----------------------------------------------------------------------
%% Function   : create_content
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
create_content(Listing, OE_This, State, Parent, PathList) ->
    create_content(string:tokens(Listing, ?SEPARATOR), OE_This, 
		   State, Parent, PathList, []).

create_content([], _OE_This, State, _Parent, _PathList, Acc) ->
    {reply, Acc, State};
create_content([H|T], OE_This, State, Parent, PathList, Acc) ->
    FullPathList = PathList ++[filename:basename(H)],
    case check_type(OE_This, State, filename:join(FullPathList)) of
	nfile ->
	    create_content(T, OE_This, State, Parent, PathList, 
			   [#'CosFileTransfer_FileWrapper'
			    {the_file = cosFileTransferApp:create_file(OE_This, 
								       FullPathList, 
								       Parent),
			     file_type = nfile}|Acc]);
	{ndirectory, _Members} ->
	    create_content(T, OE_This, State, Parent, PathList, 
			   [#'CosFileTransfer_FileWrapper'
			    {the_file = cosFileTransferApp:create_dir(OE_This, 
								      FullPathList, 
								      Parent),
			     file_type = ndirectory}|Acc]);
	Other ->
	    Other
    end.
    

%%----------------------------------------------------------------------
%% Function   : MISC functions
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
setup_local(tcp) ->
    {ok,Socket}=gen_tcp:listen(0, [binary, 
				   {packet, 0},
				   {backlog,1},
				   {active, false}]),
    {ok, Port} = inet:port(Socket),
    {Socket, [#'CosFileTransfer_ProtocolSupport'{protocol_name="TCP/IP",
						 addresses = [local_address(Port)]}]};
setup_local(ssl) ->
    Options = [{certfile, cosFileTransferApp:ssl_server_certfile()},
	       {verify, cosFileTransferApp:ssl_server_verify()},
	       {depth, cosFileTransferApp:ssl_server_depth()}] ++ 
	ssl_server_cacertfile_option(),
    {ok,Socket}=ssl:listen(0, [binary, 
			       {packet, 0},
			       {backlog,1},
			       {active, false}|Options]),
    {ok, {_Address, Port}} = ssl:sockname(Socket),
    {Socket, [#'CosFileTransfer_ProtocolSupport'{protocol_name="SSL",
						 addresses = [local_address(Port)]}]}.

local_address(Port) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4)++":"++integer_to_list(Port).

ssl_server_cacertfile_option() ->
    case cosFileTransferApp:ssl_server_cacertfile() of
	[] ->
	    [];
	X when is_list(X) ->
	    [{cacertfile, X}];
	_ ->
	    []
    end.

%%----------------------------------------------------------------------
%% Function   : source_file_operation
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
source_FTS_operation(State, SrcFile, DestFile, Op, Offset, FTS) ->
    Mod = ?get_Module(State),
    %% The source FTS is supposed to be the active one, set up a connection.
    Protocols = 'CosFileTransfer_FileTransferSession':'_get_protocols_supported'(FTS),
    SrcName  = 'CosFileTransfer_File':'_get_complete_file_name'(SrcFile),
    TempName = cosFileTransferApp:create_name("TemporarySrcFile"),
    case Mod:recv(?get_Server(State),  filename:join(SrcName), TempName) of
	ok when Op == insert ->
            %% Downloaded the File, we are now ready to transmit.
	    Pid = spawn(?MODULE, invoke_call, [self(), insert, 
					       [FTS, SrcFile, DestFile, Offset]]),
	    send_file(Protocols, ?get_MyType(State), ?get_ConnectionTimeout(State),
		      TempName),
            %% Delete the temporary local copy.
	    delete_tmp_file(TempName, 
			    "Transfer completed but failed to remove temporary local copy."),
	    check_reply(Pid),
	    {reply, ok, State};
	ok ->
            %% Downloaded the File, we are now ready to transmit.
	    Pid = spawn(?MODULE, invoke_call, [self(), Op, [FTS, SrcFile, DestFile]]),
	    send_file(Protocols, ?get_MyType(State), ?get_ConnectionTimeout(State),
		      TempName),
	    %% Delete the temporary local copy.
	    delete_tmp_file(TempName, 
			    "Transfer completed but failed to remove temporary local copy."),
	    check_reply(Pid),
	    {reply, ok, State};
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="File not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not loggen in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."})
    end.
  
%%----------------------------------------------------------------------
%% Function   : target_file_operation
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
target_FTS_operation(State, _SrcFile, DestFile, Op, Offset) ->
    Mod = ?get_Module(State),
    DestName = 'CosFileTransfer_File':'_get_complete_file_name'(DestFile),
    TempName = cosFileTransferApp:create_name("TemporaryDestFile"),
    receive_file(?get_MyType(State), ?get_Connection(State), 
		 ?get_ConnectionTimeout(State), TempName, write),
    Result =
    if
	Op == insert ->
	    Mod:insert(?get_Server(State), TempName, filename:join(DestName), Offset);
	true ->
	    Mod:Op(?get_Server(State), TempName, filename:join(DestName))
    end,
    case Result of
	ok ->
            %% Delete the temporary local copy.
	    delete_tmp_file(TempName, 
			    "Transfer completed but failed to remove temporary local copy."),
            %% Completed the transfer successfully.
	    {reply, ok, State};
	{error, epath} ->
	    delete_tmp_file(TempName,
			    "IllegalOperationException and not able to remove temporary local copy."),
	    corba:raise(#'CosFileTransfer_IllegalOperationException'
			{reason="Not allowed by destination."});
	{error, elogin} ->
	    delete_tmp_file(TempName,
			    "SessionException and not able to remove temporary local copy."),
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not logged in."});
	{error, econn} ->
	    delete_tmp_file(TempName,
			    "TransferException and not able to remove temporary local copy."),
	    corba:raise(#'CosFileTransfer_TransferException'
			{reason="Premature connection ending."});
		{error, etnospc} ->
	    delete_tmp_file(TempName,
			    "RequestFailureException and not able to remove temporary local copy."),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	{error, efnamena} ->
	    delete_tmp_file(TempName,
			    "IllegalOperationException and not able to remove temporary local copy."),
	    corba:raise(#'CosFileTransfer_IllegalOperationException'
			{reason="Not allowed by destination."})
    end.

%%----------------------------------------------------------------------
%% Function   : receive_file
%% Arguments  : Driver   - currently only gen_tcp supported.
%%              LSocket  - which socket to use.
%%              FileName - an absolute file name representing the
%%                         file we want to create or append to.
%%              Type     - 'read', 'write', 'append'.
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
receive_file(tcp, LSock, Timeout, FileName, Type) ->
    %% The Type can be the ones allowed by the file-module, i.e., 
    %% 'read', 'write' or 'append'
    FD = file_open(FileName, Type),
    case gen_tcp:accept(LSock, Timeout) of
	{ok, Sock} ->
	    receive_file_helper(gen_tcp, Sock, FD);
	{error, timeout} ->
	    orber:dbg("[~p] CosFileTransfer_FileTransferSession:receive_file();~n"
		      "gen_tcp:accept(~p) timed out", [?LINE, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="TCP accept timed out.."});
	{error, Why} ->
 	    orber:dbg("[~p] CosFileTransfer_FileTransferSession:receive_file();~n"
		      "gen_tcp:accept(~p) failed: ~p", [?LINE, Timeout, Why], ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="TCP accept failed."})
    end;
receive_file(ssl, LSock, Timeout, FileName, Type) ->
    %% The Type can be the ones allowed by the file-module, i.e., 
    %% 'read', 'write' or 'append'
    FD = file_open(FileName, Type),
    case ssl:transport_accept(LSock, Timeout) of
	{ok, Sock} ->
	    case ssl:ssl_accept(Sock, Timeout) of
		ok ->
		    receive_file_helper(ssl, Sock, FD);
		{error, Error} ->
		    orber:dbg("[~p] CosFileTransfer_FileTransferSession:receive_file();~n"
			      "ssl:ssl_accept(~p) failed: ~p", 
			      [?LINE, Timeout, Error], ?DEBUG_LEVEL),
		    corba:raise(#'CosFileTransfer_RequestFailureException'
				{reason="TCP accept failed."})
	    end;
	{error, timeout} ->
	    orber:dbg("[~p] CosFileTransfer_FileTransferSession:receive_file();~n"
		      "ssl:transport_accept(~p) timed out", 
		      [?LINE, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="TCP accept timed out.."});
	{error, Why} ->
	    orber:dbg("[~p] CosFileTransfer_FileTransferSession:receive_file();~n"
		      "ssl:transport_accept(~p) failed: ~p", 
		      [?LINE, Timeout, Why], ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="TCP accept failed."})
    end.

receive_file_helper(Driver, Sock, FD) ->
    case Driver:recv(Sock, 0) of
	{ok, Bin} ->
	    file:write(FD, Bin),
	    receive_file_helper(Driver, Sock, FD);
	{error, closed} ->
	    file:close(FD);
	What ->
	    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:receive_file(~p);
Error occured when receiving data: ~p", [?LINE, Driver, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : send_file
%% Arguments  : Driver   - currently only gen_tcp supported.
%%              Sock     - which socket to use.
%%              FileName - an absolute file name representing the
%%                         file we want to send.
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
send_file(Protocols, Type, Timeout, FileName) ->
    {Driver, Sock} = setup_connection(Protocols, Type, Timeout),
    FD = file_open(FileName, read),
    BuffSize = cosFileTransferApp:get_buffert_size(),
    send_file_helper(Driver, Sock, FD, BuffSize).

send_file_helper(Driver, Sock, FD, BuffSize) ->
    case file:read(FD, BuffSize) of
	eof ->
	    file:close(FD),
	    Driver:close(Sock);
	{ok, Bin} ->
	    case Driver:send(Sock, Bin) of
		ok ->
		    send_file_helper(Driver, Sock, FD, BuffSize);
		What ->
		    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:send_file_helper(~p);
Error occured when sending data: ~p", [?LINE, Driver, What], ?DEBUG_LEVEL),
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end
    end.


file_open(File, Type) ->
    case file:open(File, [raw, binary, Type]) of
	{ok, FD} ->
	    FD;
	{error, What} ->
	    orber:debug_level_print("[~p] CosFileTransfer_FileTransferSession:file_open(~p);
Failed to open the file due to: ~p", [?LINE, File, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Unable to open given file."})
    end.

%%----------------------------------------------------------------------
%% Function   : check_type
%% Arguments  : FullName - an absolute file name representing the
%%                         file or directory we want to evaluate.
%% Returns    : 
%% Description: 
%% When communcating with FTP-servers on different platforms a variety of
%% answers can be returned. A few examples:
%%
%% ### ftp:nlist on an empty directory ###
%% {ok, ""}, {error, epath}
%% 
%% ### ftp:nlist on a non-existing directory or file ###
%% {ok, "XXX: No such file or directory}, {error, epath}
%%
%% ### ftp:nlist on an existing directory with one contained item ###
%% {ok, "Item"}
%%
%% Comparing the above we see that it's virtually impossible to tell apart
%% {ok, "XXX: No such file or directory} and {ok, "Item"}.
%% Hence, it's easier to test if it's possible to do ftp:cd instead.
%% Ugly, but rather effective. If we look at the bright side, it's only
%% necessary when we try to lookup:
%% * non-existing item
%% * A directory with one member only.
%% * An empty directory.
%% 
%% Furthermore, no need for traversing Listings etc.
%%----------------------------------------------------------------------
check_type(_OE_This, State, FullName) when ?is_FTP(State); ?is_NATIVE(State) ->
    Mod = ?get_Module(State),
    Result =
	case Mod:nlist(?get_Server(State), FullName) of
	    {ok, Listing} when length(Listing) > 0->
		case string:tokens(Listing, ?SEPARATOR) of
		    [FullName] ->
			nfile;
		    Members when length(Members) > 1 ->
			%% Must test if more than one member since sometimes
			%% this operation returns for example:
			%% {ok, "XXX No such file or drectory"}
			{ndirectory, Members};
		    Member ->
			case Mod:cd(?get_Server(State), FullName) of
			    ok ->
				case Mod:cd(?get_Server(State), 
					    ?get_CurrentDir(State)) of
				    ok ->
					{ndirectory, Member};
				    _ ->
                            		%% Failed, we cannot continue since the
					%% FTS now pointso an incorrect Directory.
					%% Hence, we must terminate.
					{stop, normal, 
					 {'EXCEPTION', 
					  #'CosFileTransfer_RequestFailureException'
					  {reason="Unknown error."}}, State}
				end;
			    {error, E} ->
				{error, E};	
			    _ ->
				nfile
			end
		end;
	    {error, epath} ->
		%% Might be a file.
		DirName = filename:dirname(FullName),
		case Mod:nlist(?get_Server(State), DirName) of
		    {ok,  Listing} when length(Listing) > 0->
			Members = string:tokens(Listing, ?SEPARATOR),
			case lists:member(FullName, Members) of
			    true ->
				nfile;
			    _ ->
				BName = filename:basename(FullName),
				case lists:member(BName, Members) of
				    true ->
					nfile;
				    _ ->
					{error, epath}
				end
			end;
		    _ ->
			{error, epath}
		end;
	    _ ->
		case Mod:cd(?get_Server(State), FullName) of
		    ok ->
			case Mod:cd(?get_Server(State), ?get_CurrentDir(State)) of
			    ok ->
				{ndirectory, []};
			    _ ->
                            	%% Failed, we cannot continue since the
				%% FTS now pointso an incorrect Directory.
				%% Hence, we must terminate.
				{stop, normal, 
				 {'EXCEPTION', 
				  #'CosFileTransfer_RequestFailureException'
				  {reason="Unknown error."}}, State}
			end;
		    _ ->
			{error, epath}
		end
	end,
    case Result of
	{error, epath} ->
	    corba:raise(#'CosFileTransfer_FileNotFoundException'
			{reason="File or Directory not found."});
	{error, elogin} ->
	    corba:raise(#'CosFileTransfer_SessionException'
			{reason="User not logged in."});
	{error, econn} ->
	    corba:raise(#'CosFileTransfer_RequestFailureException'
			{reason="Premature connection ending."});
	Other ->
	    Other
    end.



%%======================================================================
%% END OF MODULE
%%======================================================================
