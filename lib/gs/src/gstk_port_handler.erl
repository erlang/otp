%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% ------------------------------------------------------------
%%
%% This is a driver for the 'gstk' application modified to 
%% handle events for gs. 'gstk' is a modified standalone wish.
%% 
%% FIXME
%% mkdir tcl ; cd tcl
%% ( cd /usr/local/pgm/tcl-8.3.3 ; tar -cf - * ) | tar -xf -
%% ( cd /usr/local/pgm/tk-8.3.3 ; tar -cf - * ) | tar -xf -
%% rm -fr include man bin/tclsh
%% cd ..
%% tar -cf tcl.tar *
%%
%% ------------------------------------------------------------

-module(gstk_port_handler).
-compile([{nowarn_deprecated_function,{gs,error,2}}]).

-include("gstk.hrl").

% The executable can have many names. There is not always
% a plain "wish" program. 
% FIXME There has to be a better solution....
% FIXME Add option in app file or environmen  variable.

-define(WISHNAMES, ["wish85","wish8.5",
		    "wish84","wish8.4",
		    "wish83","wish8.3",
		    "wish82","wish8.2",
		    "wish"]).

%% ------------------------------------------------------------
%%                  DEBUG FUNCTIONS 
%% ------------------------------------------------------------
-export([exec/1,call/2,
	 start_link/1,init/2,ping/1,stop/1]).
-export([wait_for_connection/2]).

-define(START_TIMEOUT , 1000 * 30).
-define(ACCEPT_TIMEOUT, 1000 * 20).

-define(DEBUGLEVEL, 4).

-ifdef(DEBUG).

-define(DBG(DbgLvl,Format, Data),dbg(DbgLvl, Format, Data)).
-define(DBG_STR(DbgLvl, What, Str),dbg_str(DbgLvl, What, Str)).

dbg(DbgLvl, Format, Data) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format("DBG: " ++ Format, Data);
dbg(_DbgLvl, _Format, _Data) -> ok.

dbg_str(DbgLvl, What, Str) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format("DBG: ~s~s\n", [What,dbg_s(Str)]);
dbg_str(_DbgLvl, _What, _Data) -> ok.

dbg_s([]) ->
    [];
dbg_s([C | Str]) when list(C) ->
    [dbg_s(C) | dbg_s(Str)];
dbg_s([C | Str]) when C >= 20, C < 255 ->
    [C | dbg_s(Str)];
dbg_s([$\n | Str]) ->
    ["\\n" | dbg_s(Str)];
dbg_s([$\r | Str]) ->
    ["\\r" | dbg_s(Str)];
dbg_s([$\t | Str]) ->
    ["\\t" | dbg_s(Str)];
dbg_s([C | Str]) when integer(C) ->
    [io_lib:format("\\~.3.0w",[C]) | dbg_s(Str)].

-else.

-define(DBG(DbgLvl,Format, Data), true).
-define(DBG_STR(DbgLvl, What, Str), true).

-endif.

%% ------------------------------------------------------------
%%                  INTERFACE FUNCTIONS 
%% ------------------------------------------------------------

% Note: gs is not a true application so this doesn't work :-(
% Communication protocol between Erlang backend and wish program
% that can be set in the application environment, e.i. tested
% with "erl -gs backend_comm socket"
%
%   backend_comm = socket | port
%
% We fake reading the application variables from the command line.
% Note that multiple -gs arguments can't be used.

get_env(App, KeyAtom) ->
    KeyStr = atom_to_list(KeyAtom),
    ?DBG(1,"Result from init:get_argument(~w): ~p\n",
	[KeyAtom,init:get_argument(App)]),
    case init:get_argument(App) of
	{ok,[[KeyStr,ValStr]]} ->
	    {ok,list_to_atom(ValStr)};
	_ ->
	    undefined
    end.

start_link(Gstk) ->
    ?DBG(1, "start_link(~w)~n", [Gstk]),
%    io:format("STARTS ~p\n",[erlang:localtime()]),
    Mode = 
	% FIXME: Want to use application:get_env() if we where an true app
	case {os:type(),get_env(gs,backend_comm)} of
	    {{win32,_Flavor},undefined} ->
		use_socket;
	    {_OS,undefined} ->
		use_port;
	    {_OS,{ok,socket}} ->
		use_socket;
	    {_OS,{ok,port}} ->
		use_port
	end,
    ?DBG(1,"We use mode: ~w (~w)\n",[Mode,get_env(gs,backend_comm)]),
    Pid = spawn_link(gstk_port_handler, init, [Gstk,Mode]),
    receive
	{Pid, ok} ->
	    {ok, Pid};
	{Pid, error, Reason} ->
	    {error, Reason}
    after ?START_TIMEOUT ->
	    {error, timeout}
    end.

call(PortHandler, Cmd) ->
    PortHandler ! {call, ["erlcall {",Cmd,$}]},
    receive
        {result, Result}         -> 
            ?DBG(1, "call reply: ~p~n", [Result]),
            {result,     Result};
        {bad_result, Bad_Result} ->
            ?DBG(1, "bad call reply: ~p~n", [Bad_Result]),
            {bad_result, Bad_Result}
    end.

ping(PortHandler) ->
    ?DBG(1, "ping~n", []),
    PortHandler ! {ping, self()},
    receive
	{pong,_From,PortOrSock} -> {ok,PortOrSock}
    end.

stop(PortHandler) ->
    ?DBG(1, "stop~n", []),
    PortHandler ! {stop,self()},
    receive
	{stopped,PortHandler} -> ok
    end.

%% Purpose: asyncron call to tk 
%% too expensive 
% FIXME   
exec(Cmd) ->
    get(port_handler) ! {exec, ["erlexec {",Cmd,$}]},
    ok.

% in gstk context, but I don't want "ifndef nt40" in other 
% modules than this one.
%exec(Cmd) ->
%    ?DBG_STR(1, "", ["erlexec {",Cmd,"}"]),
%    case get(port) of
%	{socket,Sock} ->
%	    gen_tcp:send(Sock, ["erlexec {",Cmd,$}]);
%	{port,Port} ->
%	    Port ! {get(port_handler),{command,["erlexec {",Cmd,$}]}}
%    end,
%    ok.

%% ===========================================================================
%% The server
%% ===========================================================================

%% ---------------------------------------------------------------------
%% We initiate by starting the wish port program and use the pipe
%% or a socket to communicate with it.
%%
%% gstk: is the pid of the gstk process that started me. 
%%      all my input (from the port) is forwarded to it.
%%----------------------------------------------------------------------
-record(state,{out,gstk}).

init(Gstk, Mode) ->
    process_flag(trap_exit,true),

    % ------------------------------------------------------------
    % Set up paths
    % ------------------------------------------------------------

    PrivDir = code:priv_dir(gs),
    TclDir = filename:join(PrivDir,"tcl"),
    TclBinDir = filename:join(TclDir,"bin"),
    TclLibDir = filename:join(TclDir,"lib"),

    InitScript = filename:nativename(filename:join(PrivDir,"gstk.tcl")),

    ?DBG(1, "TclBinDir  : ~s\n", [TclBinDir]),
    ?DBG(1, "TclLibDir  : ~s\n", [TclLibDir]),
    ?DBG(1, "InitScript : ~s\n", [InitScript]),

    % ------------------------------------------------------------
    % Search for wish in priv and in system search path
    % ------------------------------------------------------------

    {Wish,Options} = 
	case filelib:wildcard(filename:join(TclBinDir,"wish*")) of
	    % If more than one wish in priv we assume they are the same
	    [PrivWish | _] ->
		% ------------------------------------------------
		% We have to set TCL_LIBRARY and TK_LIBRARY because else
		% 'wish' will search in the original installation directory
		% for 'tclIndex' and this may be an incompatible version on
		% the host we run on.
		% ------------------------------------------------

		[TclLibrary] =
		    filelib:wildcard(filename:join(PrivDir,
						   "tcl/lib/tcl[1-9]*")),
		[TkLibrary]  =
		    filelib:wildcard(filename:join(PrivDir,
						   "tcl/lib/tk[1-9]*")),

		Opts = [{env,[{"TCL_LIBRARY", TclLibrary},
			      {"TK_LIBRARY", TkLibrary},
			      {"LD_LIBRARY_PATH",TclLibDir}]},
		       {packet,4}],
		{PrivWish,Opts};
	    _ ->
		% We use the system wish program
		{search_wish(?WISHNAMES, Gstk),[{packet,4}]}
	end,


    ?DBG(1, "Wish       : ~s\n", [Wish]),

    Cmd =
	case Mode of
	    use_socket ->
		% ------------------------------------------------------------
		% Set up a listening socket and call accept in another process
		% ------------------------------------------------------------
		SocketOpts =
		    [
		     {nodelay, true},
		     {packet,4},
		     {reuseaddr,true}
		    ],
		% Let OS pick a number
		{ok,ListenSocket} = gen_tcp:listen(0, SocketOpts),
		{ok,ListenPort} = inet:port(ListenSocket),

		% Wait in another process
		spawn_link(?MODULE,wait_for_connection,[self(),ListenSocket]),
		lists:concat([Wish," ",InitScript," -- ",PrivDir," ",
			      ListenPort]);
	    use_port ->
		lists:concat([Wish," ",InitScript," -- ",PrivDir])
	end,

    ?DBG(1, "Port opts  :\n~p\n", [Options]),

    % FIXME remove timing if not debugging
    Port =
	case timer:tc(erlang,open_port,[{spawn, Cmd}, Options]) of
	    {_T,Port1} when is_port(Port1) ->
		?DBG(1,"open_port takes ~p milliseconds\n",[_T/1000]),
		link(Port1),
		Port1;
	    {_T,{error,_Reason1}} ->		% FIXME: Why throw away reason?!
		?DBG(1,"ERROR: ~p\n",[_Reason1]),
		Gstk ! {self(), error, backend_died},
		exit(normal)
	end,

    State =
	case Mode of
	    use_socket ->
		% ------------------------------------------------------------
		% Wait for a connection
		% ------------------------------------------------------------
		Sock =
		    receive
			{connected,Socket} ->
			    Socket;
			% FIXME: Why throw away reason?!
			{'EXIT', _Pid, _Reason2} ->
			    Gstk ! {self(), error, backend_died},
			    exit(normal)
		    end,

		?DBG(1,"Got socket ~p~n",[Sock]),
		#state{out={socket,Sock}, gstk=Gstk};
	    use_port ->
		#state{out={port,Port}, gstk=Gstk}
	end,

    Gstk ! {self(), ok},			% Tell caller we are prepared
    idle(State).

search_wish([], Gstk) ->
    Gstk ! {self(), error, backend_died},
    exit(normal);
search_wish([WishName | WishNames], Gstk) ->
    case os:find_executable(WishName) of
	false ->
	    search_wish(WishNames, Gstk);
	Wish ->
	    Wish
    end.
	
%%----------------------------------------------------------------------
%% If we use sockets we wait for connection from port prog
%%----------------------------------------------------------------------

wait_for_connection(CallerPid, ListenSocket) ->
    {ok,Sock} = gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT),
    ?DBG(1,"Got accept ~p~p~n",[self(),Sock]),
    ok = gen_tcp:controlling_process(Sock,CallerPid),
    CallerPid ! {connected,Sock}.

%% ===========================================================================
%% The main loop
%% ===========================================================================

idle(State) ->
    ?DBG(1, "idle~n", []),
%    io:format("IDLE ~p\n",[erlang:localtime()]),
    receive

	{call, Cmd} ->
	    output(State, Cmd),
	    idle(State);

	{exec, Cmd} ->
	    collect_exec_calls(Cmd, [], 0, State),
	    idle(State);

	{_Port, {data, Input}} ->
	    ?DBG_STR(2, "INPUT[port]: ", [Input]),
	    handle_input(State, Input),
	    idle(State);

	{tcp, _Sock, Input} ->
	    ?DBG_STR(2, "INPUT[sock]: ", [Input]),
	    handle_input(State, Input),
	    idle(State);

	{ping,From} ->
	    From ! {pong,self(),State#state.out},
	    idle(State);

	{stop,From} ->
	    From ! {stopped,self()};

	% FIXME: We are we not to terminate if watforsocket
	% terminated but what about the port???????
	{'EXIT',_Pid,normal} ->
	    ?DBG(1, "EXIT[~w]: normal~n", [_Pid]),
	    idle(State);

	{'EXIT',Pid,Reason} ->
	    %%io:format("Port died when in idle loop!~n"),
	    ?DBG(1,"EXIT[~w]~n~p~n",[Pid,Reason]),
	    exit({port_handler,Pid,Reason});

	Other ->
	    ?DBG(1,"OTHER: ~p~n",[Other]),
	    gs:error("gstk_port_handler: got other: ~w~n",[Other]),
	    idle(State)
    end.

%% ----------------------------------------------------------------------

-define(MAXQUEUE, 4).				% FIXME find value...

collect_exec_calls(Cmd, Queue, QueueLen, State) when QueueLen < ?MAXQUEUE ->
    receive
	{exec, NewCmd} ->
%	    io:format("collect~p~n", [NewCmd]),
	    collect_exec_calls(NewCmd, [Cmd | Queue], QueueLen+1, State)
    after 0 ->
	    if
		QueueLen == 0 ->
		    output(State, Cmd);
		true ->
		    output(State, join_cmd_reverse(Cmd, Queue, []))
	    end
    end;
collect_exec_calls(Cmd, Queue, _QueueLen, State) -> % Queue is full, output
    String = join_cmd_reverse(Cmd, Queue, []),
%    io:format("queue full: ~p~n", [String]),
    output(State, String).


join_cmd_reverse(Cmd, [], DeepStr) ->
    [DeepStr | Cmd];
join_cmd_reverse(Cmd, [Cmd1 | Cmds], DeepStr) ->
    join_cmd_reverse(Cmd, Cmds, [Cmd1,$; | DeepStr]).

%% ----------------------------------------------------------------------
%%
%% Handle incoming data
%% 1 - Event
%% 2 - Reply from call
%% 3 - Bad reply from call
%% 4 - Error
%% 5 - End of message
%% 

handle_input(State,[Type | Data]) ->
    GstkPid = State#state.gstk,
    case Type of
	1 ->
	    handle_event(GstkPid,Data);

	2 ->
	    GstkPid ! {result, Data};

	3 ->
	    GstkPid ! {bad_result, Data};

	4 ->
	    gs:error("gstk_port_handler: error in input : ~s~n",[Data])
    end.

%% ----------------------------------------------------------------------
%% output a command to the port
%% buffer several incoming execs
%%
output(#state{out = {socket,Sock}}, Cmd) ->
    ?DBG_STR(1, "OUTPUT[sock]: ", [Cmd]),
    ok = gen_tcp:send(Sock, Cmd);

output(#state{out = {port,Port}}, Cmd) ->
    ?DBG_STR(1, "OUTPUT[port]: ", [Cmd]),
    Port ! {self(), {command, Cmd}}.

% FIXME why test list?
handle_event(GstkPid, Bytes) when is_list(Bytes) ->
    Event = tcl2erl:parse_event(Bytes),
    ?DBG(1,"Event = ~p\n",[Event]),
    gstk:event(GstkPid, Event). %% Event is {ID, Etag, Args}
