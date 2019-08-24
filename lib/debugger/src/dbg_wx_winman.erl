%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(dbg_wx_winman).
-behaviour(gen_server).

%% External exports
-export([start/0]).
-export([insert/2, is_started/1,
	 clear_process/1,
	 raise/1,
	 update_windows_menu/2]).

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
    case wxTopLevelWindow:isIconized(Win) of
       true -> wxTopLevelWindow:iconize(Win, [{iconize, false}]);
       false -> ignore
    end,
    wxWindow:raise(Win).

%%--------------------------------------------------------------------
%% update_windows_menu(Data)
%%   Data = {New, Old}
%%     New = Old = list()
%%--------------------------------------------------------------------
update_windows_menu(Win, [MonInfo|Infos]) ->
    Menu = get('Windows'),
    OldItems = wxMenu:getMenuItems(Menu),
    [wxMenu:delete(Menu, Item) || Item <- OldItems],
    menuitem(Win, Menu,MonInfo, 700),
    _ = wxMenu:appendSeparator(Menu),
    wx:foldl(fun(Info,Acc) -> menuitem(Win,Menu,Info,Acc) end, 701, Infos).

menuitem(Window, Menu, {Title, Win}, Id) ->
    _ = wxMenu:append(Menu, Id, Title),
    wxWindow:connect(Window, command_menu_selected, 
		     [{id,Id},{userData,{dbg_ui_winman,Win}}]),
    Id+1.


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({is_started, Title}, _From, State) ->
    Reply = case lists:keyfind(Title, #win.title, State#state.wins) of
		false -> false;
		Win -> {true, Win#win.win}
	    end,
    {reply, Reply, State}.

handle_cast({insert, Pid, Title, Win}, State) ->
    link(Pid),
    Wins = State#state.wins ++ [#win{owner=Pid, title=Title, win=Win}],
    inform_all(Wins),
    {noreply, State#state{wins=Wins}};

handle_cast({clear_process, Title}, State) ->
    OldWins = State#state.wins,
    Wins = case lists:keyfind(Title, #win.title, OldWins) of
	       #win{owner=Pid} ->
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
	Pid =:= Mon#win.owner -> {stop, normal, State};
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
