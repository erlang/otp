%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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
	 node,
	 parent_notebook,
	 panel, sizer,
	 menubar,
	 alloc,
	 fields,
	 timer}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, Parent]) ->
    SysInfo = observer_backend:sys_info(),
    AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
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
    {MemPanel, MemoryInfo} = create_mem_info(Panel, AllocInfo),
    wxSizer:add(Sizer, TopSizer, [{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
				  {proportion, 0}, {border, 5}]),
    wxSizer:add(Sizer, MemPanel, [{flag, ?wxEXPAND bor BorderFlags bor ?wxBOTTOM},
				    {proportion, 1}, {border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    Timer = observer_lib:start_timer(10),
    {Panel, #sys_wx_state{parent=Parent,
			  parent_notebook=Notebook,
			  panel=Panel, sizer=Sizer, alloc=MemoryInfo,
			  timer=Timer, fields=Fields0 ++ Fields1}}.

create_sys_menu(Parent) ->
    View = {"View", [#create_menu{id = ?ID_REFRESH, text = "Refresh\tCtrl-R"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh interval"}]},
    observer_wx:create_menus(Parent, [View]).

update_syspage(#sys_wx_state{node = Node, fields=Fields, sizer=Sizer, alloc=AllocCtrl}) ->
    SysInfo = observer_wx:try_rpc(Node, observer_backend, sys_info, []),
    AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
    {Info, Stat} = info_fields(),
    observer_lib:update_info(Fields, observer_lib:fill_info(Info, SysInfo) ++
				 observer_lib:fill_info(Stat, SysInfo)),
    update_alloc(AllocCtrl, AllocInfo),
    wxSizer:layout(Sizer).

create_mem_info(Parent, Fields) ->
    Panel = wxPanel:new(Parent),
    wxWindow:setBackgroundColour(Panel, {255,255,255}),
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES bor ?wxLC_VRULES,
    Grid = wxListCtrl:new(Panel, [{style, Style}]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Allocator Type",  ?wxLIST_FORMAT_LEFT,  200},
		 {"Block size (kB)",  ?wxLIST_FORMAT_RIGHT, 150},
		 {"Carrier size (kB)",?wxLIST_FORMAT_RIGHT, 150}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),
    update_alloc(Grid, Fields),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT},
			      {border, 5}, {proportion, 1}]),
    wxWindow:setSizerAndFit(Panel, Sizer),
    {Panel, Grid}.

update_alloc(Grid, AllocInfo) ->
    Fields = alloc_info(AllocInfo, [], 0, 0, true),
    wxListCtrl:deleteAllItems(Grid),
    Update = fun({Name, BS, CS}, Row) ->
		     wxListCtrl:insertItem(Grid, Row, ""),
		     wxListCtrl:setItem(Grid, Row, 0, observer_lib:to_str(Name)),
		     wxListCtrl:setItem(Grid, Row, 1, observer_lib:to_str(BS div 1024)),
		     wxListCtrl:setItem(Grid, Row, 2, observer_lib:to_str(CS div 1024)),
		     Row + 1
	     end,
    lists:foldl(Update, 0, Fields),
    Fields.

alloc_info([{Type,Instances}|Allocators],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    {BS,CS,NewTotalBS,NewTotalCS,NewIncludeTotal} =
	sum_alloc_instances(Instances,0,0,TotalBS,TotalCS),
    alloc_info(Allocators,[{Type,BS,CS}|TypeAcc],NewTotalBS,NewTotalCS,
	       IncludeTotal andalso NewIncludeTotal);
alloc_info([],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    Types = [X || X={_,BS,CS} <- TypeAcc, (BS>0 orelse CS>0)],
    case IncludeTotal of
	true ->
	    [{total,TotalBS,TotalCS} | lists:reverse(Types)];
	false ->
	    lists:reverse(Types)
    end.

sum_alloc_instances(false,BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,false};
sum_alloc_instances([{_,_,Data}|Instances],BS,CS,TotalBS,TotalCS) ->
    {NewBS,NewCS,NewTotalBS,NewTotalCS} =
	sum_alloc_one_instance(Data,BS,CS,TotalBS,TotalCS),
    sum_alloc_instances(Instances,NewBS,NewCS,NewTotalBS,NewTotalCS);
sum_alloc_instances([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,true}.

sum_alloc_one_instance([{sbmbcs,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS,TotalCS);
sum_alloc_one_instance([{_,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([{_,[{blocks_size,BS},{carriers_size,CS}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([_|Rest],BS,CS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,BS,CS,TotalBS,TotalCS);
sum_alloc_one_instance([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS}.

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
