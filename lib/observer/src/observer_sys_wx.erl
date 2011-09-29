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

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(ID_REFRESH, 101).
-define(ID_REFRESH_INTERVAL, 102).

%% Records
-record(sys_wx_state,
	{parent,
	 panel,
	 menubar,
	 parent_notebook,
	 no_procs,
	 no_cpu,
	 no_cpu_available,
	 no_cpu_online,
	 tot_alloc,
	 proc_used,
	 proc_alloc,
	 atom_used,
	 atom_alloc,
	 binary_alloc,
	 code_alloc,
	 ets_alloc,
	 node_label,
	 node,
	 refr_timer = false,
	 refr_intv = 30}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, Parent]) ->
    SysPanel = wxPanel:new(Notebook, []),

    %% Setup sizers
    SysSizer = wxBoxSizer:new(?wxVERTICAL),

    SysNodeSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Node:"}]),

    SysLoadSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Load:"}]),
    SysLeftLoadSizer = wxBoxSizer:new(?wxVERTICAL),
    SysMidLoadSizer = wxBoxSizer:new(?wxHORIZONTAL),
    SysRightLoadSizer = wxBoxSizer:new(?wxVERTICAL),

    SysMemSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Memory:"}]),
    SysLeftMemSizer = wxBoxSizer:new(?wxVERTICAL),
    SysMidMemSizer = wxBoxSizer:new(?wxHORIZONTAL),
    SysRightMemSizer = wxBoxSizer:new(?wxVERTICAL),

    wxSizer:add(SysSizer, SysNodeSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysSizer, SysLoadSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysSizer, SysMemSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysLoadSizer, SysLeftLoadSizer),
    wxSizer:add(SysLoadSizer, SysMidLoadSizer),
    wxSizer:add(SysLoadSizer, SysRightLoadSizer),

    wxSizer:add(SysMemSizer, SysLeftMemSizer),
    wxSizer:add(SysMemSizer, SysMidMemSizer),
    wxSizer:add(SysMemSizer, SysRightMemSizer),

    wxSizer:addSpacer(SysMidLoadSizer, 90),
    wxSizer:addSpacer(SysMidMemSizer, 70),

    %% Create labels
    NodeInfo = get_syspage_info(node()),
    NodeLabel = create_info_label(SysPanel, SysNodeSizer, observer_sys:node_name_str(NodeInfo)),

    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's available:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's online:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "existing processes:"),
    NoCpuTxt = create_info_label(SysPanel, SysRightLoadSizer, observer_sys:no_cpu_str(NodeInfo)),
    NoCpuAvTxt = create_info_label(SysPanel, SysRightLoadSizer, observer_sys:no_cpu_available_str(NodeInfo)),
    NoCpuOnTxt = create_info_label(SysPanel, SysRightLoadSizer, observer_sys:no_cpu_online_str(NodeInfo)),
    NoProcsTxt = create_info_label(SysPanel, SysRightLoadSizer, observer_sys:no_procs_str(NodeInfo)),

    create_info_label(SysPanel, SysLeftMemSizer, "total allocated:"),
    create_info_label(SysPanel, SysLeftMemSizer, "used by processes:"),
    create_info_label(SysPanel, SysLeftMemSizer, "allocated for processes:"),
    create_info_label(SysPanel, SysLeftMemSizer, "used by atoms:"),
    create_info_label(SysPanel, SysLeftMemSizer, "allocated for atoms:"),
    create_info_label(SysPanel, SysLeftMemSizer, "allocated for binaries:"),
    create_info_label(SysPanel, SysLeftMemSizer, "allocated for code"),
    create_info_label(SysPanel, SysLeftMemSizer, "allocated for ETS:"),
    TotAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:tot_alloc_str(NodeInfo)),
    ProcUsedTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:proc_used_str(NodeInfo)),
    ProcAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:proc_alloc_str(NodeInfo)),
    AtomUsedTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:atom_used_str(NodeInfo)),
    AtomAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:atom_alloc_str(NodeInfo)),
    BinaryAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:binary_alloc_str(NodeInfo)),
    CodeAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:code_alloc_str(NodeInfo)),
    EtsAllocTxt = create_info_label(SysPanel, SysRightMemSizer, observer_sys:ets_alloc_str(NodeInfo)),

    %% Create StateRecord
    SysPanelState = #sys_wx_state{
      parent = Parent,
      panel = SysPanel,
      parent_notebook = Notebook,
      node_label = NodeLabel,
      no_procs = NoProcsTxt,
      no_cpu = NoCpuTxt,
      no_cpu_available = NoCpuAvTxt,
      no_cpu_online= NoCpuOnTxt,
      tot_alloc = TotAllocTxt,
      proc_used = ProcUsedTxt,
      proc_alloc = ProcAllocTxt,
      atom_used = AtomUsedTxt,
      atom_alloc = AtomAllocTxt,
      binary_alloc = BinaryAllocTxt,
      code_alloc = CodeAllocTxt,
      ets_alloc = EtsAllocTxt,
      node = node()},

    wxPanel:setSizer(SysPanel, SysSizer),
    {SysPanel, SysPanelState}.

get_syspage_info(Node) ->
    observer_wx:try_rpc(Node, observer_sys, node_info, []).

create_info_label(Panel, Sizer, Msg) ->
    WxText = wxStaticText:new(Panel, ?wxID_ANY, Msg),
    wxSizer:add(Sizer, WxText),
    WxText.

create_sys_menu(Parent) ->
    View = {"View", [#create_menu{id = ?ID_REFRESH, text = "Refresh"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh interval"}]},
    observer_wx:create_menus(Parent, [View]).

update_syspage(#sys_wx_state{node = Node} = State) ->
    Info = get_syspage_info(Node),
    update_info_label(node_label, Info, State#sys_wx_state.node_label),
    update_info_label(no_procs, Info, State#sys_wx_state.no_procs),
    update_info_label(no_cpu, Info, State#sys_wx_state.no_cpu),
    update_info_label(no_cpu_available, Info, State#sys_wx_state.no_cpu_available),
    update_info_label(no_cpu_online, Info, State#sys_wx_state.no_cpu_online),
    update_info_label(tot_alloc, Info, State#sys_wx_state.tot_alloc),
    update_info_label(proc_used, Info, State#sys_wx_state.proc_used),
    update_info_label(proc_alloc, Info, State#sys_wx_state.proc_alloc),
    update_info_label(atom_used, Info, State#sys_wx_state.atom_used),
    update_info_label(atom_alloc, Info, State#sys_wx_state.atom_alloc),
    update_info_label(binary_alloc, Info, State#sys_wx_state.binary_alloc),
    update_info_label(code_alloc, Info, State#sys_wx_state.code_alloc),
    update_info_label(ets_alloc, Info, State#sys_wx_state.ets_alloc).

update_info_label(node_label, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:node_name_str(Info));
update_info_label(no_procs, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:no_procs_str(Info));
update_info_label(no_cpu, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:no_cpu_str(Info));
update_info_label(no_cpu_available, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:no_cpu_available_str(Info));
update_info_label(no_cpu_online, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:no_cpu_online_str(Info));
update_info_label(tot_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:tot_alloc_str(Info));
update_info_label(proc_used, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:proc_used_str(Info));
update_info_label(proc_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:proc_alloc_str(Info));
update_info_label(atom_used, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:atom_used_str(Info));
update_info_label(atom_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:atom_alloc_str(Info));
update_info_label(binary_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:binary_alloc_str(Info));
update_info_label(code_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:code_alloc_str(Info));
update_info_label(ets_alloc, Info, WxTxt) ->
    wxStaticText:setLabel(WxTxt, observer_sys:ets_alloc_str(Info)).


%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(refresh_interval, #sys_wx_state{panel = Panel,
					    node = Node} = State) ->
    try
	update_syspage(State)
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node)
    end,
    {noreply, State};

handle_info({node, Node}, #sys_wx_state{panel = Panel} = State) ->
    UpdState = State#sys_wx_state{node = Node},
    try
	update_syspage(UpdState),
	{noreply, UpdState}

    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node),
	    {noreply, State}
    end;

handle_info({active, Node},
	    #sys_wx_state{parent = Parent,
			  panel = Panel,
			  refr_timer = Timer0,
			  refr_intv = Intv} = State) ->
    UpdState = State#sys_wx_state{node = Node},
    create_sys_menu(Parent),
    try
	update_syspage(UpdState),
	Timer = case Timer0 of
		    true ->
			{ok, Ref} = timer:send_interval(Intv*1000, refresh_interval),
			Ref;
		    false ->
			false
		end,
	{noreply, UpdState#sys_wx_state{refr_timer = Timer}}

    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Panel, Node),
	    {noreply, State}
    end;


handle_info(not_active, #sys_wx_state{refr_timer = Timer0} = State) ->
    Timer = case Timer0 of
		false -> false;
		true -> true;
		Timer0 ->
		    timer:cancel(Timer0),
		    true
	    end,
    {noreply, State#sys_wx_state{refr_timer = Timer}};

handle_info(Info, State) ->
    io:format("~p, ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
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
	     #sys_wx_state{refr_timer = Timer0,
			   refr_intv = Intv0,
			   parent_notebook = Notebook} = State) ->
    Parent = observer_tv_wx:get_wx_parent(Notebook),
    case observer_tv_wx:interval_dialog(Parent, Timer0 /= false, Intv0, 1, 5*60) of
	cancel ->
	    {noreply, State};
	{true, Intv} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {ok, Timer} = timer:send_interval(Intv * 1000, refresh_interval),
	    {noreply, State#sys_wx_state{refr_timer=Timer, refr_intv=Intv}};
	{false, _} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {noreply, State#sys_wx_state{refr_timer=false}}
    end;

handle_event(Event, State) ->
    io:format("handle event ~p\n", [Event]),
    {noreply, State}.
