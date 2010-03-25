%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wxe_server.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 17 Jan 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

%% @hidden
-module(wxe_master).
-behaviour(gen_server).

%% API
-export([start/0, init_port/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {cb_port,  %% Callback port and to erlang messages goes via it.
		users,    %% List of wx servers, needed ??
		driver}). %% Driver name so wx_server can create it's own port

-include("gen/wxe_debug.hrl").
-include("gen/gl_debug.hrl").

-define(DRIVER, "wxe_driver").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: init_port() -> {UserPort,CallBackPort} | error(Error)
%% Description: Creates the port
%%--------------------------------------------------------------------
init_port() ->
    case whereis(?MODULE) of
	undefined ->
	    case start() of
		{ok,Pid} -> Pid;
		{error,{already_started,Pid}} -> Pid;
		{error, {Reason,Stack}} ->
		    erlang:raise(error, Reason, Stack)
	    end;
	Pid ->
	    Pid
    end,
    {Driver, CBport} = gen_server:call(?MODULE, init_port, infinity),
    Port = open_port({spawn,Driver},[binary]),
    receive wx_port_initiated -> ok end,
    {Port, CBport}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    DriverName = ?DRIVER,
    PrivDir = priv_dir(),
    erlang:group_leader(whereis(init), self()),
    case catch erlang:system_info(smp_support) of
	true -> ok;
	_ -> 
	    error_logger:format("WX ERROR: SMP emulator required", []),
	    erlang:error(not_smp)
    end,
    case catch (erlang:system_info(version) >= "5.6.2") of
	true -> ok;
	_ -> %% Needs to be able to disable FPU exceptions. 
	    error_logger:format("WX ERROR: OTP R12B-2 or Emulator >= 5.6.2 required", []),
	    erlang:error(wrong_version)
    end,

    %% io:format("Loading ~p @ ~p ~n", [DriverName,PrivDir]),
    case os:type() of
        {win32,_} ->  %% Needed for mingwm10.dll
            Path = os:getenv("PATH"),
            os:putenv("PATH", PrivDir ++ ";" ++ Path);
        _ -> ok
    end,

    case erl_ddll:load_driver(PrivDir,DriverName) of
	ok -> ok;
	{error, What} -> 
	    error_logger:format("WX Failed loading ~p@~p ~n", [DriverName,PrivDir]),
	    Str = erl_ddll:format_error(What),
	    erlang:error({load_driver,Str})
    end,
    process_flag(trap_exit, true),
    DriverWithArgs = DriverName ++ " " ++ code:priv_dir(wx) ++ [0],
    
    case catch open_port({spawn, DriverWithArgs},[binary]) of
	{'EXIT', Err} -> 
	    erlang:error({open_port,Err});
	Port ->
	    wx_debug_info = ets:new(wx_debug_info, [named_table]),
	    wx_non_consts = ets:new(wx_non_consts, [named_table]),
	    true = ets:insert(wx_debug_info, wxdebug_table()),
	    true = ets:insert(wx_debug_info, gldebug_table()),
	    spawn_link(fun() -> debug_ping(Port) end),
	    receive 
		{wx_consts, List} ->
		    true = ets:insert(wx_non_consts, List)
	    end,
	    {ok, #state{cb_port=Port, driver=DriverName, users=gb_sets:empty()}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(init_port, From, State=#state{driver=Driver,cb_port=CBPort, users=Users}) ->
    {reply, {Driver,CBPort}, State#state{users=gb_sets:add(From,Users)}};
handle_call(_Request, _From, State) ->
    %%io:format("Unknown request ~p sent to ~p from ~p ~n",[_Request, ?MODULE, _From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    %%io:format("Unknown message ~p sent to ~p~n",[_Msg, ?MODULE]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({wxe_driver, error, Msg}, State) ->
    error_logger:format("WX ERROR: ~s~n", [Msg]),
    {noreply, State};
handle_info({wxe_driver, internal_error, Msg}, State) ->
    error_logger:format("WX INTERNAL ERROR: ~s~n", [Msg]),
    {noreply, State};
handle_info({wxe_driver, debug, Msg}, State) ->
    io:format("WX DBG: ~s~n", [Msg]),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("Unknown message ~p sent to ~p~n",[_Info, ?MODULE]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    erlang:display({?MODULE, killed, process_info(self(),trap_exit),_Reason}),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%%%%%%%%%% INTERNAL %%%%%%%%%%%%%%%%%%%%%%%%

%% If you want anything done, do it yourself. 

priv_dir() ->
    Type = erlang:system_info(system_architecture),
    {file, Path} = code:is_loaded(?MODULE),
    Priv = case filelib:is_regular(Path) of
	       true ->
		   Beam = filename:join(["ebin/",atom_to_list(?MODULE) ++ ".beam"]),
		   filename:join(strip(Path, Beam), "priv");
	       false ->
		   code:priv_dir(wx)
	   end,
    try 
	{ok, Dirs0} = file:list_dir(Priv),
	Dirs1 = split_dirs(Dirs0),
	Dirs  = lists:reverse(lists:sort(Dirs1)),    
	
	Best = best_dir(hd(split_dirs([Type])),Dirs, Priv),
	filename:join(Priv, Best)
    catch _:_ ->
	    error_logger:format("WX ERROR: Could not find suitable \'~s\' for ~s in: ~s~n", 
				[?DRIVER, Type, Priv]),
	    erlang:error({load_driver, "No driver found"})
    end.
    
best_dir(Dir, Dirs0, Priv) ->
    Dirs = [{D,D} || D <- Dirs0],
    best_dir(Dir, Dirs, [], Priv).

best_dir(Pre, [{[],_}|R], Acc, Priv) -> %% Empty skip'em
    best_dir(Pre, R, Acc, Priv);
best_dir(Pre, [{Pre,Dir}|R], Acc, Priv) -> 
    Real = dir_app(lists:reverse(Dir)),
    case file:list_dir(filename:join(Priv,Real)) of
	{ok, Fs} ->
	    case lists:any(fun(File) -> filename:rootname(File) =:= ?DRIVER end, Fs) of
		true ->  Real; %% Found dir and it contains a driver
		false -> best_dir(Pre, R, Acc, Priv)
	    end;
	_ ->
	    best_dir(Pre, R, Acc, Priv)
    end;
best_dir(Pre, [{[_|F],Dir}|R], Acc, Priv) ->
    best_dir(Pre, R, [{F,Dir}|Acc], Priv);
best_dir(_Pre, [], [],_) -> throw(no_dir);  %% Nothing found
best_dir([_|Pre], [], Acc, Priv) ->
    best_dir(Pre, lists:reverse(Acc), [], Priv);
best_dir([], _, _,_) -> throw(no_dir).  %% Nothing found

split_dirs(Dirs0) ->
    ToInt = fun(Str) ->
		    try 
			list_to_integer(Str)
		    catch _:_ -> Str
		    end
	    end,
    Split = fun(Dir) ->
		    Toks = tokens(Dir,".-"),
		    lists:reverse([ToInt(Str) || Str <- Toks])
	    end,
    lists:map(Split,Dirs0).

dir_app([]) -> [];
dir_app([Dir]) -> Dir;
dir_app(Dir) ->
    dir_app2(Dir).
dir_app2([Int]) when is_integer(Int) ->
    integer_to_list(Int);
dir_app2([Str]) when is_list(Str) ->
    Str;
dir_app2([Head|Rest]) when is_integer(Head) ->
    integer_to_list(Head) ++ dir_app2(Rest);
dir_app2([Head|Rest]) when is_list(Head) ->
    Head ++ dir_app2(Rest).
    
strip(Src, Src) ->
    [];
strip([H|R], Src) ->
    [H| strip(R, Src)].


debug_ping(Port) ->
    timer:sleep(1*333),    
    _R = (catch erlang:port_call(Port, 0, [])),
%%    io:format("Erlang ping ~p ~n", [_R]),
    debug_ping(Port).

tokens(S,Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [[C]|Toks]);
        false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    lists:reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [[C], lists:reverse(Cs) |Toks]);
        false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).
