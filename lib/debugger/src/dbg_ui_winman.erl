%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(dbg_ui_winman).
-behaviour(gen_server).

%% External exports
-export([start/0]).
-export([insert/2, is_started/1,
	 clear_process/1,
	 raise/1,
	 windows_menu/1, update_windows_menu/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(win, {owner,     % pid()
	      title,     % string()
	      win        % gsobj()
	     }).

-record(state, {wins=[]  % [#win{}]
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start()
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local,?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% insert(Title, Win)
%%   Title = string()
%%   Win = gsobj()
%%--------------------------------------------------------------------
insert(Title, Win) ->
    gen_server:cast(?MODULE, {insert, self(), Title, Win}).

%%--------------------------------------------------------------------
%% is_started(Title) -> true | false
%%   Title = string()
%%--------------------------------------------------------------------
is_started(Title) ->
    case gen_server:call(?MODULE, {is_started, Title}, infinity) of
	{true, Win} ->
	    raise(Win),
	    true;
	false ->
	    false
    end.

%%--------------------------------------------------------------------
%% clear_process(Title)
%%   Title = string()
%%--------------------------------------------------------------------
clear_process(Title) ->
    gen_server:cast(?MODULE, {clear_process, Title}).

%%--------------------------------------------------------------------
%% raise(Win)
%%   Win = gsobj()
%%--------------------------------------------------------------------
raise(Win) ->
    gs:config(Win, [raise, {iconify, false}, {setfocus, true}]).

%%--------------------------------------------------------------------
%% windows_menu(MenuBar)
%%   MenuBar = gsobj()
%%--------------------------------------------------------------------
windows_menu(MenuBar) ->
    gs:menubutton('WindowsMenuBtn', MenuBar,
		  [{label,{text," Windows "}},
		   {font, dbg_ui_win:font(normal)}]),
    gs:menu('WindowsMenu', 'WindowsMenuBtn', []).

%%--------------------------------------------------------------------
%% update_windows_menu(Data)
%%   Data = {New, Old}
%%     New = Old = list()
%%--------------------------------------------------------------------
update_windows_menu([MonInfo|Infos]) ->
    gs:destroy('WindowsMenu'),
    gs:menu('WindowsMenu', 'WindowsMenuBtn', []),
    menuitem(MonInfo),
    gs:menuitem(separator, 'WindowsMenu', [{itemtype, separator}]),
    lists:foreach(fun(Info) -> menuitem(Info) end, Infos).

menuitem({Title, Win}) ->
    gs:menuitem(Title, 'WindowsMenu', [{label, {text,Title}},
				       {font, dbg_ui_win:font(normal)},
				       {data, {dbg_ui_winman,Win}}]).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Arg) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({is_started, Title}, _From, State) ->
    Reply = case lists:keysearch(Title, #win.title, State#state.wins) of
		{value, Win} -> {true, Win#win.win};
		false -> false
	    end,
    {reply, Reply, State}.

handle_cast({insert, Pid, Title, Win}, State) ->
    link(Pid),
    Wins = State#state.wins ++ [#win{owner=Pid, title=Title, win=Win}],
    inform_all(Wins),
    {noreply, State#state{wins=Wins}};

handle_cast({clear_process, Title}, State) ->
    OldWins = State#state.wins,
    Wins = case lists:keysearch(Title, #win.title, OldWins) of
		 {value, #win{owner=Pid}} ->     
		   Msg = {dbg_ui_winman, destroy},
		   Pid ! Msg,
		   lists:keydelete(Title, #win.title, OldWins);
	       false -> 
		   OldWins
    end,
    {noreply, State#state{wins=Wins}}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    [Mon | _Wins] = State#state.wins,
    if
	Pid==Mon#win.owner -> {stop, normal, State};
	true ->
	    Wins2 = lists:keydelete(Pid, #win.owner, State#state.wins),
	    inform_all(Wins2),
	    {noreply, State#state{wins=Wins2}}
    end.

terminate(_Reason, State) ->
    delete_all(State#state.wins),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

inform_all(Wins) ->
    Infos = lists:map(fun(#win{title=Title, win=Win}) -> {Title, Win} end,
			  Wins),
    Msg = {dbg_ui_winman, update_windows_menu, Infos},
    lists:foreach(fun(#win{owner=Pid}) -> Pid ! Msg end, Wins).

delete_all(Wins) ->
    Msg = {dbg_ui_winman, destroy},
    lists:foreach(fun(#win{owner=Pid}) -> Pid ! Msg end, Wins).
