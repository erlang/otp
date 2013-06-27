%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2013. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : file_server.erl
%%% Author  : Raimo Niskanen <raimo@erix.ericsson.se>
%%% Purpose : A simple file server
%%% Created : 13 Oct 2000 by Raimo Niskanen <raimo@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(file_server).

-behaviour(gen_server).

%% External exports
-export([format_error/1]).
-export([start/0, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-define(FILE_IO_SERVER_TABLE, file_io_servers).

-define(FILE_SERVER, file_server_2).      % Registered name
-define(FILE_IO_SERVER, file_io_server).  % Module
-define(PRIM_FILE, prim_file).            % Module

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
format_error({_Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({_Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

start() -> do_start(start).
start_link() -> do_start(start_link).

stop() -> 
    gen_server:call(?FILE_SERVER, stop, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

-type state() :: port().	% Internal type

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

-spec init([]) -> {'ok', state()} | {'stop', term()}.

init([]) ->
    process_flag(trap_exit, true),
    case ?PRIM_FILE:start() of
	{ok, Handle} ->
	    ?FILE_IO_SERVER_TABLE =
                ets:new(?FILE_IO_SERVER_TABLE, [named_table]),
	    {ok, Handle};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_call(term(), term(), state()) ->
        {'noreply', state()} |
	{'reply', 'eof' | 'ok' | {'error', term()} | {'ok', term()}, state()} |
	{'stop', 'normal', 'stopped', state()}.

handle_call({open, Name, ModeList}, {Pid, _Tag} = _From, Handle)
  when is_list(ModeList) ->
    Child = ?FILE_IO_SERVER:start_link(Pid, Name, ModeList),
    case Child of
	{ok, P} when is_pid(P) ->
	    ets:insert(?FILE_IO_SERVER_TABLE, {P, Name});
	_ ->
	    ok
    end,
    {reply, Child, Handle};

handle_call({open, _Name, _Mode}, _From, Handle) ->
    {reply, {error, einval}, Handle};

handle_call({read_file, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_file(Name), Handle};

handle_call({write_file, Name, Bin}, _From, Handle) ->
    {reply, ?PRIM_FILE:write_file(Name, Bin), Handle};

handle_call({set_cwd, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:set_cwd(Handle, Name), Handle};

handle_call({delete, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:delete(Handle, Name), Handle};

handle_call({rename, Fr, To}, _From, Handle) ->
    {reply, ?PRIM_FILE:rename(Handle, Fr, To), Handle};

handle_call({make_dir, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:make_dir(Handle, Name), Handle};

handle_call({del_dir, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:del_dir(Handle, Name), Handle};

handle_call({list_dir, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:list_dir(Handle, Name), Handle};
handle_call({list_dir_all, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:list_dir_all(Handle, Name), Handle};

handle_call(get_cwd, _From, Handle) ->
    {reply, ?PRIM_FILE:get_cwd(Handle), Handle};
handle_call({get_cwd}, _From, Handle) ->
    {reply, ?PRIM_FILE:get_cwd(Handle), Handle};
handle_call({get_cwd, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:get_cwd(Handle, Name), Handle};

handle_call({read_file_info, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_file_info(Handle, Name), Handle};

handle_call({read_file_info, Name, Opts}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_file_info(Handle, Name, Opts), Handle};

handle_call({altname, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:altname(Handle, Name), Handle};

handle_call({write_file_info, Name, Info}, _From, Handle) ->
    {reply, ?PRIM_FILE:write_file_info(Handle, Name, Info), Handle};

handle_call({write_file_info, Name, Info, Opts}, _From, Handle) ->
    {reply, ?PRIM_FILE:write_file_info(Handle, Name, Info, Opts), Handle};

handle_call({read_link_info, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_link_info(Handle, Name), Handle};

handle_call({read_link_info, Name, Opts}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_link_info(Handle, Name, Opts), Handle};

handle_call({read_link, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_link(Handle, Name), Handle};
handle_call({read_link_all, Name}, _From, Handle) ->
    {reply, ?PRIM_FILE:read_link_all(Handle, Name), Handle};

handle_call({make_link, Old, New}, _From, Handle) ->
    {reply, ?PRIM_FILE:make_link(Handle, Old, New), Handle};

handle_call({make_symlink, Old, New}, _From, Handle) ->
    {reply, ?PRIM_FILE:make_symlink(Handle, Old, New), Handle};

handle_call({copy, SourceName, SourceOpts, DestName, DestOpts, Length},
	    _From, Handle) ->
    Reply = 
	case ?PRIM_FILE:open(SourceName, [read, binary | SourceOpts]) of
	    {ok, Source} ->
		SourceReply = 
		    case ?PRIM_FILE:open(DestName, 
					 [write, binary | DestOpts]) of
			{ok, Dest} ->
			    DestReply = 
				?PRIM_FILE:copy(Source, Dest, Length),
			    ?PRIM_FILE:close(Dest),
			    DestReply;
			{error, _} = Error ->
			    Error
		    end,
		?PRIM_FILE:close(Source),
		SourceReply;
	    {error, _} = Error ->
		Error
	end,
    {reply, Reply, Handle};

handle_call(stop, _From, Handle) ->
    {stop, normal, stopped, Handle};

handle_call(Request, From, Handle) ->
    error_logger:error_msg("handle_call(~p, ~p, _)", [Request, From]),
    {noreply, Handle}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast(Msg, State) ->
    error_logger:error_msg("handle_cast(~p, _)", [Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_info(term(), state()) ->
        {'noreply', state()} | {'stop', 'normal', state()}.

handle_info({'EXIT', Pid, _Reason}, Handle) when is_pid(Pid) ->
    ets:delete(?FILE_IO_SERVER_TABLE, Pid),
    {noreply, Handle};

handle_info({'EXIT', Handle, _Reason}, Handle) ->
    error_logger:error_msg("Port controlling ~w terminated in ~w",
			   [?FILE_SERVER, ?MODULE]),
    {stop, normal, Handle};

handle_info(Info, State) ->
    error_logger:error_msg("handle_Info(~p, _)", [Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, Handle) ->
    ?PRIM_FILE:stop(Handle).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%% The basic file server and start-up.
%%%
%%% The file server just handles the open command/message and acts as a
%%% router for messages between the port and the file processes. If a
%%% file process terminates we close the associated file.

%% Start = start | start_link
do_start(Start) ->
    case init:get_argument(master) of
	error ->
	    gen_server:Start({local,?FILE_SERVER}, ?MODULE, [], []);
	{ok, [[Node]]} ->
	    do_start(Start, list_to_atom(Node), ?FILE_SERVER);
	X ->
	    {error, {get_argument, master, X}}
    end.

%% Should mimic gen_server:Start
do_start(Start, Node, Name) ->
    case rpc:call(Node, erlang, whereis, [Name]) of
	Filer when is_pid(Filer); Filer =:= undefined ->
	    case catch do_start_slave(Start, Filer, Name) of
		{'EXIT', Reason} ->
		    {error, Reason};
		Result ->
		    Result
	    end;
	Other ->
	    {error, {no_master, Other}}
    end.

%% May exit upon failure, return {ok, SlavePid} if all well.
do_start_slave(start_link, Filer, Name) ->
    Self = self(),
    Token = make_ref(),
    Slave = spawn_link(fun() -> relay_start(Self, Token, Filer, Name) end),
    receive
	{started, Token} ->
	    {ok, Slave}
    end;
do_start_slave(start, Filer, Name) ->
    Self = self(),
    Token = make_ref(),
    Slave = spawn(fun() -> relay_start(Self, Token, Filer, Name) end),
    SlaveMonitor = erlang:monitor(process, Slave),
    receive
	{started, Token} ->
	    erlang:demonitor(SlaveMonitor, [flush]),
	    {ok, Slave};
	{'DOWN', SlaveMonitor, _, _, Reason} ->
	    exit(Reason)
    end.
			  
%% We have the relay process file internal.
%% We do not need to load slave as a mandatory module
%% during system startup.

relay_start(Parent, Token, Filer, Name) when is_pid(Filer) ->
    case catch register(Name, self()) of
	true ->
	    ok;
	_ ->
	    exit({already_started, whereis(Name)})
    end,
    %% This will fail towards an R5 node or older, Filer is a pid()
    FilerMonitor = erlang:monitor(process, Filer),
    process_flag(trap_exit, true),
    Parent ! {started, Token},
    relay_loop(Parent, Filer, FilerMonitor);
relay_start(Parent, Token, undefined, _Name) ->
    %% Dummy process to keep kernel supervisor happy
    process_flag(trap_exit, true),
    Parent ! {started, Token},
    receive
	{'EXIT', Parent, Reason} ->
	    exit(Reason)
    end.

relay_loop(Parent, Filer, FilerMonitor) ->
    receive
	{'DOWN', FilerMonitor, _, _, Reason} ->
	    exit(Reason);
	{'EXIT', Parent, Reason} ->
	    exit(Reason);
        Msg ->
            Filer ! Msg
    end,
    relay_loop(Parent, Filer, FilerMonitor).
