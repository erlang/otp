%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
-module(observer_sys_wx).

-behaviour(wx_object).

-export([start_link/2]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-export([sys_info/0]).

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
    SysInfo = sys_info(),
    {Info, Stat} = info_fields(),
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    {FPanel0, _FSizer0, Fields0} =
	observer_lib:display_info(Panel, observer_lib:fill_info(Info, SysInfo)),
    {FPanel1, _FSizer1, Fields1} =
	observer_lib:display_info(Panel, observer_lib:fill_info(Stat, SysInfo)),
    wxSizer:add(Sizer, FPanel0, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM bor ?wxLEFT},
				 {proportion, 1}, {border, 5}]),
    wxSizer:add(Sizer, FPanel1, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM bor ?wxRIGHT},
				 {proportion, 1}, {border, 5}]),
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
    SysInfo = observer_wx:try_rpc(Node, ?MODULE, sys_info, []),
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
	     [{"System Logical CPU's", logical_processors},
	      {"Erlang Logical CPU's", logical_processors_online},
	      {"Used Logical CPU's", logical_processors_available}
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
	     [{"Up time", uptime},
	      {"Max Processes", process_limit},
	      {"Processes", process_count},
	      {"Run Queue", run_queue},
	      {"IO Input",  io_input},
	      {"IO Output",  io_output}
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
    {stop, not_yet_implemented, State}.

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


sys_info() ->
    {{_,Input},{_,Output}} = erlang:statistics(io),
    [{process_count, erlang:system_info(process_count)},
     {process_limit, erlang:system_info(process_limit)},
     {uptime, {time_ms, element(1, erlang:statistics(wall_clock))}},
     {run_queue, erlang:statistics(run_queue)},
     {io_input, {bytes, Input}},
     {io_output, {bytes, Output}},
     {logical_processors, erlang:system_info(logical_processors)},
     {logical_processors_available, erlang:system_info(logical_processors_available)},
     {logical_processors_online, erlang:system_info(logical_processors_online)},

     {otp_release, erlang:system_info(otp_release)},
     {version, erlang:system_info(version)},
     {system_architecture, erlang:system_info(system_architecture)},
     {kernel_poll, erlang:system_info(kernel_poll)},
     {smp_support, erlang:system_info(smp_support)},
     {threads, erlang:system_info(threads)},
     {thread_pool_size, erlang:system_info(thread_pool_size)},
     {wordsize_internal, erlang:system_info({wordsize, internal})},
     {wordsize_external, erlang:system_info({wordsize, external})} |
     erlang:memory()
    ].
