%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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
-module(observer_sys_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(ID_REFRESH, 101).
-define(ID_REFRESH_INTERVAL, 102).

%% Records
-record(sys_wx_state,
	{parent,
	 node,
	 parent_notebook,
	 panel, sizer,
	 menubar,
	 fields,
	 timer}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, Parent]) ->
    SysInfo = observer_backend:sys_info(),
    {Info, Stat} = info_fields(),
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    TopSizer = wxBoxSizer:new(?wxHORIZONTAL),
    {FPanel0, _FSizer0, Fields0} =
	observer_lib:display_info(Panel, observer_lib:fill_info(Info, SysInfo)),
    {FPanel1, _FSizer1, Fields1} =
	observer_lib:display_info(Panel, observer_lib:fill_info(Stat, SysInfo)),
    wxSizer:add(TopSizer, FPanel0, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(TopSizer, FPanel1, [{flag, ?wxEXPAND}, {proportion, 1}]),
    BorderFlags = ?wxLEFT bor ?wxRIGHT,
    wxSizer:add(Sizer, TopSizer, [{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
				  {proportion, 0}, {border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    Timer = observer_lib:start_timer(10),
    {Panel, #sys_wx_state{parent=Parent,
			  parent_notebook=Notebook,
			  panel=Panel, sizer=Sizer,
			  timer=Timer, fields=Fields0 ++ Fields1}}.

create_sys_menu(Parent) ->
    View = {"View", [#create_menu{id = ?ID_REFRESH, text = "Refresh\tCtrl-R"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh interval"}]},
    observer_wx:create_menus(Parent, [View]).

update_syspage(#sys_wx_state{node = Node, fields=Fields, sizer=Sizer}) ->
    SysInfo = observer_wx:try_rpc(Node, observer_backend, sys_info, []),
    {Info, Stat} = info_fields(),
    observer_lib:update_info(Fields, observer_lib:fill_info(Info, SysInfo) ++
				 observer_lib:fill_info(Stat, SysInfo)),
    wxSizer:layout(Sizer).

info_fields() ->
    Info = [{"System and Architecture",
	     [{"System Version", otp_release},
	      {"Erts Version", version},
	      {"Compiled for", system_architecture},
	      {"Emulator Wordsize", wordsize_external},
	      {"Process Wordsize", wordsize_internal},
	      {"Smp Support",  smp_support},
	      {"Thread Support",  threads},
	      {"Async thread pool size",  thread_pool_size}
	     ]},
	    {"CPU's and Threads",
	     [{"Logical CPU's", logical_processors},
	      {"Online Logical CPU's", logical_processors_online},
	      {"Available Logical CPU's", logical_processors_available},
	      {"Schedulers", schedulers},
	      {"Online schedulers", schedulers_online},
	      {"Available schedulers", schedulers_available}
	     ]}
	   ],
    Stat = [{"Memory Usage", right,
	     [{"Total", {bytes, total}},
	      {"Processes", {bytes, processes}},
	      {"Atoms", {bytes, atom}},
	      {"Binaries", {bytes, binary}},
	      {"Code", {bytes, code}},
	      {"Ets", {bytes, ets}}
	     ]},
	    {"Statistics", right,
	     [{"Up time", {time_ms, uptime}},
	      {"Max Processes", process_limit},
	      {"Processes", process_count},
	      {"Run Queue", run_queue},
	      {"IO Input",  {bytes, io_input}},
	      {"IO Output", {bytes, io_output}}
	     ]}
	   ],
    {Info, Stat}.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(refresh_interval, #sys_wx_state{panel = Panel,
					    node = Node} = State) ->
    try
	update_syspage(State)
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node)
    end,
    {noreply, State};

handle_info({active, Node}, #sys_wx_state{parent = Parent, panel = Panel,
					  timer = Timer} = State) ->
    UpdState = State#sys_wx_state{node = Node},
    create_sys_menu(Parent),
    try
	update_syspage(UpdState),
	{noreply, UpdState#sys_wx_state{timer=observer_lib:start_timer(Timer)}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node),
	    {noreply, State}
    end;

handle_info(not_active, #sys_wx_state{timer = Timer} = State) ->
    {noreply, State#sys_wx_state{timer = observer_lib:stop_timer(Timer)}};

handle_info(Info, State) ->
    io:format("~p:~p: Unhandled info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Unhandled Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p~p: Unhandled cast ~p~n",[?MODULE, ?LINE, Msg]),
    {noreply, State}.

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}},
	     #sys_wx_state{node = Node, panel = Panel} = State) ->
    try
	update_syspage(State)
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node)
    end,
    {noreply, State};

handle_event(#wx{id = ?ID_REFRESH_INTERVAL,
		 event = #wxCommand{type = command_menu_selected}},
	     #sys_wx_state{timer = Timer0, parent_notebook = Notebook} = State) ->
    Timer = observer_lib:interval_dialog(Notebook, Timer0, 1, 5*60),
    {noreply, State#sys_wx_state{timer=Timer}};

handle_event(Event, State) ->
    io:format("~p:~p: Unhandled event ~p\n", [?MODULE,?LINE,Event]),
    {noreply, State}.
