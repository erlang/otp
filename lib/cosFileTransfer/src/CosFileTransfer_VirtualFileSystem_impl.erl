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
%% File        : CosFileTransfer_VirtualFileSystem_impl.erl
%% Description : 
%%
%% Created     : 12 Sept 2000
%%----------------------------------------------------------------------
-module('CosFileTransfer_VirtualFileSystem_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-include_lib("cosFileTransfer/include/CosFileTransfer.hrl").
-include("cosFileTransferApp.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%% Interface functions
-export(['_get_file_system_type'/2,
	 '_get_supported_content_types'/2,
	 login/5]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {type, content, host, port, protocol, timeout, module}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(create_InitState(T, C, H, P, Pr, TO),
	#state{type=T,
	       content=C,
	       host=H,
	       port=P,
	       protocol=Pr,
	       timeout=TO}).
-define(create_NativeInitState(T, C, H, P, Pr, TO, M),
	#state{type=T,
	       content=C,
	       host=H,
	       port=P,
	       protocol=Pr,
	       timeout=TO,
	       module=M}).

-define(get_Type(S),        S#state.type).
-define(get_Content(S),     S#state.content).
-define(get_Host(S),        S#state.host).
-define(get_Port(S),        S#state.port).
-define(get_StartDir(S),    S#state.startdir).
-define(get_Module(S),      S#state.module).
-define(get_Protocol(S),    S#state.protocol).
-define(get_Timeout(S),     S#state.timeout).

-define(is_FTP(S),          S#state.type == 'FTP').
-define(is_FTAM(S),         S#state.type == 'FTAM').
-define(is_NATIVE(S),       S#state.type == 'NATIVE').


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
init([{Type, Mod}, Content, Host, Port, Options]) ->
    Prot = cosFileTransferApp:get_option(protocol, Options, ?DEFAULT_CONFIG),
    Time = timer:seconds(cosFileTransferApp:get_option(connect_timeout, Options, 
						       ?DEFAULT_CONFIG)),
    {ok, ?create_NativeInitState(Type, Content, Host, Port, Prot, Time, Mod)};
init([Type, Content, Host, Port, Options]) ->
    Prot = cosFileTransferApp:get_option(protocol, Options, ?DEFAULT_CONFIG),
    Time = timer:seconds(cosFileTransferApp:get_option(connect_timeout, Options, 
						       ?DEFAULT_CONFIG)),
    {ok, ?create_InitState(Type, Content, Host, Port, Prot, Time)}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
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
handle_info(_Info, State) ->
    {noreply, State}.

%%======================================================================
%% CosFileTransfer::VirtualFileSystem
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : '_get_file_system_type'
%% Arguments  : -
%% Returns    : CosFileTransfer::NativeFileSystemType, i.e., 'FTP', 'FTAM',
%%              or 'NATIVE'. Currently only 'FTP' is allowed.
%% Description: 
%%----------------------------------------------------------------------
'_get_file_system_type'(_OE_This, State) ->
    {reply, ?get_Type(State), State}.

%%---------------------------------------------------------------------%
%% Function   : '_get_supported_content_types'
%% Arguments  : 
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
'_get_supported_content_types'(_OE_This, State) ->
    {reply, ?get_Content(State), State}.

%%----------------------------------------------------------------------
%% Function   : login
%% Arguments  : User     - string()
%%              Password - string()
%%              Account  - string()
%% Returns    : FileTransferSession object and Directory object (out-type).
%% Description: 
%%----------------------------------------------------------------------
login(_OE_This, State, User, Password, Account) when ?is_FTP(State) ->
    case catch 'CosFileTransfer_FileTransferSession':
	oe_create(['FTP', ?get_Host(State), ?get_Port(State), User, Password, Account,
		  ?get_Protocol(State), ?get_Timeout(State)],
		  [{sup_child, true}]) of
	{ok, _Pid, FTS} ->
	    Dir = 'CosFileTransfer_FileTransferSession':
		oe_orber_create_directory_current(FTS),
	    {reply, {FTS, Dir}, State};
	What ->
	    orber:debug_level_print("[~p] CosFileTransfer_VirtualFileSystem:login(~p ~p ~p ~p);
Unable to create a FileTransferSession: ~p", 
				    [?LINE, ?get_Host(State), ?get_Port(State), User,
				     ?get_Protocol(State), What],
				    ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_SessionException'{reason="Failed creating a FTS"})
    end;
login(_OE_This, State, User, Password, Account) when ?is_NATIVE(State) ->
    case catch 'CosFileTransfer_FileTransferSession':
	oe_create([{'NATIVE', ?get_Module(State)}, ?get_Host(State), 
		   ?get_Port(State), User, Password, Account,
		   ?get_Protocol(State), ?get_Timeout(State)],
		  [{sup_child, true}]) of
	{ok, _Pid, FTS} ->
	    Dir = 'CosFileTransfer_FileTransferSession':
		oe_orber_create_directory_current(FTS),
	    {reply, {FTS, Dir}, State};
	What ->
	    orber:debug_level_print("[~p] CosFileTransfer_VirtualFileSystem:login(~p ~p ~p ~p);
Unable to create a FileTransferSession: ~p", 
				    [?LINE, ?get_Host(State), ?get_Port(State), User,
				     ?get_Protocol(State), What],
				    ?DEBUG_LEVEL),
	    corba:raise(#'CosFileTransfer_SessionException'{reason="Failed creating a FTS"})
    end.

%%======================================================================
%% Internal functions
%%======================================================================

    

%%======================================================================
%% END OF MODULE
%%======================================================================
