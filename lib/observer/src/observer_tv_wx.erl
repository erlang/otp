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
-module(observer_tv_wx).

-export([start_link/2, display_table_info/4]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-export([get_table_list/1]). %% RPC called move to runtime tools?

-export([get_wx_parent/1, interval_dialog/5]).

-import(observer_pro_wx, [to_str/1]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").
-include("observer_tv.hrl").

-define(GRID, 500).
-define(ID_REFRESH, 401).
-define(ID_REFRESH_INTERVAL, 402).
-define(ID_ETS, 403).
-define(ID_MNESIA, 404).
-define(ID_UNREADABLE, 405).
-define(ID_SYSTEM_TABLES, 406).
-define(ID_TABLE_INFO, 407).

-record(opt, {type=ets,
	      sys_hidden=true,
	      unread_hidden=true,
	      sort_key=2,
	      sort_incr=true
	     }).

-record(state,
	{
	  parent,
	  grid,
	  node=node(),
	  opt=#opt{},
	  selected,
	  tabs,
	  refr_timer=false,
	  refr_intv=30
	}).

start_link(Notebook,  Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

init([Notebook, Parent]) ->
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
    Grid = wxListCtrl:new(Panel, [{winid, ?GRID}, {style, Style}]),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1}, {border, 5}]),
    wxWindow:setSizer(Panel, Sizer),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Table Name", ?wxLIST_FORMAT_LEFT,  200},
		 {"Table Id",   ?wxLIST_FORMAT_RIGHT, 100},
		 {"Objects",    ?wxLIST_FORMAT_RIGHT, 100},
		 {"Size (kB)",  ?wxLIST_FORMAT_RIGHT, 100},
		 {"Owner Pid",  ?wxLIST_FORMAT_CENTER, 150},
		 {"Owner Name", ?wxLIST_FORMAT_LEFT,  200}
		],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_activated),
    wxListCtrl:connect(Grid, command_list_item_selected),
    wxListCtrl:connect(Grid, command_list_col_click),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    {Panel, #state{grid=Grid, parent=Parent}}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{node=Node, grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    {noreply, State#state{tabs=Tabs}};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{node=Node, grid=Grid,
			    opt=Opt0=#opt{sort_key=Key, sort_incr=Bool}}) ->
    Opt = case Col+2 of
	      Key -> Opt0#opt{sort_incr=not Bool};
	      NewKey -> Opt0#opt{sort_key=NewKey}
	  end,
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, tabs=Tabs}};

handle_event(#wx{id=Id}, State = #state{node=Node, grid=Grid, opt=Opt0})
  when Id >= ?ID_ETS, Id =< ?ID_SYSTEM_TABLES ->
    Opt = case Id of
	      ?ID_ETS -> Opt0#opt{type=ets};
	      ?ID_MNESIA -> Opt0#opt{type=mnesia};
	      ?ID_UNREADABLE -> Opt0#opt{unread_hidden= not Opt0#opt.unread_hidden};
	      ?ID_SYSTEM_TABLES -> Opt0#opt{sys_hidden= not Opt0#opt.sys_hidden}
	  end,
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, tabs=Tabs}};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-2, lists:seq(0, Cols - 2)),
		     Size = max(200, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

handle_event(#wx{obj=Grid, event=#wxList{type=command_list_item_activated,
					 itemIndex=Index}},
	     State=#state{grid=Grid, node=Node, opt=#opt{type=Type}, tabs=Tabs}) ->
    Table = lists:nth(Index+1, Tabs),
    case Table#tab.protection of
	private ->
	    self() ! {error, "Table has 'private' protection and can not be read"};
	_ ->
	    observer_tv_table:start_link(Grid, [{node,Node}, {type,Type}, {table,Table}])
    end,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Index}},
	     State) ->
    {noreply, State#state{selected=Index}};

handle_event(#wx{id=?ID_TABLE_INFO},
	     State = #state{grid=Grid, node=Node, opt=#opt{type=Type}, tabs=Tabs, selected=Sel}) ->
    case Sel of
	undefined ->
	    {noreply, State};
	R when is_integer(R) ->
	    Table = lists:nth(Sel+1, Tabs),
	    Parent = get_wx_parent(Grid),
	    display_table_info(Parent, Node, Type, Table),
	    {noreply, State}
    end;

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, refr_timer=Timer0, refr_intv=Intv0}) ->
    Parent = get_wx_parent(Grid),
    case interval_dialog(Parent, Timer0 /= false, Intv0, 10, 5*60) of
	cancel ->
	    {noreply, State};
	{true, Intv} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {ok, Timer} = timer:send_interval(Intv * 1000, refresh_interval),
	    {noreply, State#state{refr_timer=Timer, refr_intv=Intv}};
	{false, _} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {noreply, State#state{refr_timer=false}}
    end;

handle_event(Event, State) ->
    io:format("~p:~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_sync_event(Event, _Obj, _State) ->
    io:format("~p:~p, handle sync_event ~p\n", [?MODULE, ?LINE, Event]),
    ok.

handle_call(Event, From, State) ->
    io:format("~p:~p, handle call (~p) ~p\n", [?MODULE, ?LINE, From, Event]),
    {noreply, State}.

handle_cast(Event, State) ->
    io:format("~p:~p, handle cast ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info(refresh_interval, State = #state{node=Node, grid=Grid, opt=Opt}) ->
    io:format("refresh interval ~p~n", [time()]),
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    {noreply, State#state{tabs=Tabs}};

handle_info({active, Node},
	    State = #state{parent=Parent, grid=Grid, opt=Opt,
			   refr_timer = Refr, refr_intv=Intv}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    create_menus(Parent, Opt),
    Timer = case Refr of
		true ->
		    {ok, Ref} = timer:send_interval(Intv*1000, refresh_interval),
		    Ref;
		false ->
		    false
	    end,
    {noreply, State#state{node=Node, tabs=Tabs, refr_timer=Timer}};

handle_info(not_active, State = #state{refr_timer = Timer0}) ->
    Timer = case Timer0 of
		false -> false;
		true -> true;
		Timer0 ->
		    timer:cancel(Timer0),
		    true
	    end,
    {noreply, State#state{refr_timer=Timer}};

handle_info({node, Node}, State = #state{grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    Tabs = update_grid(Grid, Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{node=Node, tabs=Tabs}};

handle_info({error, Error}, State) ->
    handle_error(Error),
    {noreply, State};

handle_info(Event, State) ->
    io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

terminate(Event, _State) ->
    io:format("~p:~p, terminate ~p\n", [?MODULE, ?LINE, Event]),
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent, #opt{sys_hidden=Sys, unread_hidden=UnR, type=Type}) ->
    MenuEntries = [{"View",
		    [#create_menu{id = ?ID_TABLE_INFO, text = "Table information\tCtrl-I"},
		     separator,
		     #create_menu{id = ?ID_ETS, text = "&Ets Tables",
				  type=radio, check=Type==ets},
		     #create_menu{id = ?ID_MNESIA, text = "&Mnesia Tables",
				  type=radio, check=Type==mnesia},
		     separator,
		     #create_menu{id = ?ID_UNREADABLE, text = "View &Unreadable Tables",
				  type=check, check=not UnR},
		     #create_menu{id = ?ID_SYSTEM_TABLES, text = "View &System Tables",
				  type=check, check=not Sys},
		     separator,
		     #create_menu{id = ?ID_REFRESH, text = "Refresh\tCtrl-R"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh Interval..."}
		    ]}],
    observer_wx:create_menus(Parent, MenuEntries).

get_tables(Node, Opt) ->
    case rpc:call(Node, ?MODULE, get_table_list, [Opt]) of
	{badrpc, Error} ->
	    self() ! {error, Error},
	    [];
	{error, Error} ->
	    self() ! {error, Error},
	    [];
	Result ->
	    Result
    end.

get_table_list(#opt{type=ets, unread_hidden=HideUnread, sys_hidden=HideSys}) ->
    Info = fun(Id, Acc) ->
		   try
		       TabId = case ets:info(Id, named_table) of
				   true -> ignore;
				   false -> Id
			       end,
		       Name = ets:info(Id, name),
		       Protection = ets:info(Id, protection),
		       ignore(HideUnread andalso Protection == private, unreadable),
		       Owner = ets:info(Id, owner),
		       RegName = case catch process_info(Owner, registered_name) of
				     [] -> ignore;
				     {registered_name, ProcName} -> ProcName
				 end,
		       ignore(HideSys andalso ordsets:is_element(RegName, sys_processes()), system_tab),
		       ignore(HideSys andalso ordsets:is_element(Name, sys_tables()), system_tab),
		       ignore((RegName == mnesia_monitor)
			      andalso Name /= schema
			      andalso is_atom((catch mnesia:table_info(Name, where_to_read))), mnesia_tab),
		       Memory = ets:info(Id, memory) * erlang:system_info(wordsize),
		       Tab = #tab{name = Name,
				  id = TabId,
				  protection = Protection,
				  owner = Owner,
				  size = ets:info(Id, size),
				  reg_name = RegName,
				  type = ets:info(Id, type),
				  keypos = ets:info(Id, keypos),
				  heir = ets:info(Id, heir),
				  memory = Memory,
				  compressed = ets:info(Id, compressed),
				  fixed = ets:info(Id, fixed)
				 },
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~n",[Id, _What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], ets:all());
get_table_list(#opt{type=mnesia, sys_hidden=HideSys}) ->
    Owner = ets:info(schema, owner),
    Owner /= undefined orelse
	throw({error, "Mnesia is not running on: " ++ atom_to_list(node())}),
    {registered_name, RegName} = process_info(Owner, registered_name),
    Info = fun(Id, Acc) ->
		   try
		       Name = Id,
		       ignore(HideSys andalso ordsets:is_element(Name, mnesia_tables()), system_tab),
		       ignore(Name =:= schema, mnesia_tab),
		       Storage = mnesia:table_info(Id, storage_type),
		       Tab0 = #tab{name = Name,
				   owner = Owner,
				   size = mnesia:table_info(Id, size),
				   reg_name = RegName,
				   type = mnesia:table_info(Id, type),
				   keypos = 2,
				   memory = mnesia:table_info(Id, memory) * erlang:system_info(wordsize),
				   storage = Storage,
				   index = mnesia:table_info(Id, index)
				  },
		       Tab = if Storage == disc_only_copies ->
				     Tab0#tab{fixed = element(2, dets:info(Id, safe_fixed)) /= []};
				(Storage == ram_copies) orelse
				(Storage == disc_copies) ->
				     Tab0#tab{fixed = ets:info(Id, fixed),
					      compressed = ets:info(Id, compressed)};
				true -> Tab0
			     end,
		       [Tab|Acc]
		   catch _:_What ->
			   %% io:format("Skipped ~p: ~p ~n",[Id, _What]),
			   Acc
		   end
	   end,
    lists:foldl(Info, [], mnesia:system_info(tables)).

display_table_info(Parent, Node, Source, Table) ->
    Title = "Table Info: " ++ atom_to_list(Table#tab.name),
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title,
			    [{style, ?wxCAPTION bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER}]),

    IdInfo = {"Identification and Owner",
	      [{"Name", Table#tab.name},
	       {"Id", case Table#tab.id of
			  ignore -> Table#tab.name;
			  Id -> Id
		      end},
	       {"Named table", Table#tab.id == ignore},
	       {"Owner", Table#tab.owner},
	       {"Owner Name", case Table#tab.reg_name of
				  ignore -> "-";
				  Id -> Id
			      end},
	       {"Heir", Table#tab.heir},
	       {"Node", Node}]},
    MnesiaSettings = case Source of
			 ets -> [];
			 mnesia ->
			     [{"Local storage type", case Table#tab.storage of
							 unknown -> "Not available";
							 ST -> ST
						     end},
			      {"Index positions", list_to_strings(Table#tab.index)}]
		     end,
    Settings = {"Settings",
		[{"Source",       Source},
		 {"Key Position", Table#tab.keypos},
		 {"Table Type",   Table#tab.type},
		 {"Protection Mode", Table#tab.protection},
		 {"Fixed",        Table#tab.fixed}
		 | MnesiaSettings ]},
    Memory = {"Memory Usage",
	      [{"Number of objects", Table#tab.size},
	       {"Memory allocated",  integer_to_list(Table#tab.memory div 1024) ++ "kB"},
	       {"Compressed",        Table#tab.compressed}]},

    Sizer = display_info_wx(Frame, [IdInfo, Settings, Memory]),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:center(Frame),
    wxFrame:show(Frame).

list_to_strings([]) -> "None";
list_to_strings([A]) -> integer_to_list(A);
list_to_strings([A,B]) ->
    integer_to_list(A) ++ " ," ++ list_to_strings(B).

get_wx_parent(Window) ->
    Parent = wxWindow:getParent(Window),
    case wx:is_null(Parent) of
	true -> Window;
	false -> get_wx_parent(Parent)
    end.

display_info_wx(Frame, Info) ->
    Panel = wxPanel:new(Frame),
    wxWindow:setBackgroundColour(Panel, {255,255,255}),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(Sizer, 5),
    Add = fun(BoxInfo) ->
		  Box = create_box(Panel, BoxInfo),
		  wxSizer:add(Sizer, Box, [{flag, ?wxEXPAND bor ?wxALL},
					   {border, 5}])
	  end,
    [Add(I) || I <- Info],
    wxSizer:addSpacer(Sizer, 5),
    wxWindow:setSizerAndFit(Panel, Sizer),
    Sizer.

create_box(Panel, {Title, Info}) ->
    Box = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, Title}]),
    Left  = wxBoxSizer:new(?wxVERTICAL),
    Right = wxBoxSizer:new(?wxVERTICAL),
    Expand = [{flag, ?wxEXPAND}],
    AddRow = fun({Desc, Value}) ->
		     wxSizer:add(Left, wxStaticText:new(Panel, ?wxID_ANY, Desc ++ ":"), Expand),
		     wxSizer:add(Right, wxStaticText:new(Panel, ?wxID_ANY, to_str(Value)), Expand)
	     end,
    [AddRow(Entry) || Entry <- Info],
    wxSizer:add(Box, Left),
    wxSizer:addSpacer(Box, 10),
    wxSizer:add(Box, Right),
    wxSizer:addSpacer(Box, 30),
    Box.

sys_tables() ->
    [ac_tab,  asn1,
     cdv_dump_index_table,  cdv_menu_table,  cdv_decode_heap_table,
     cell_id,  cell_pos,  clist,
     cover_internal_data_table,   cover_collected_remote_data_table, cover_binary_code_table,
     code, code_names,  cookies,
     corba_policy,  corba_policy_associations,
     dets, dets_owners, dets_registry,
     disk_log_names, disk_log_pids,
     eprof,  erl_atom_cache, erl_epmd_nodes,
     etop_accum_tab,  etop_tr,
     ets_coverage_data,
     file_io_servers,
     gs_mapping, gs_names,  gstk_db,
     gstk_grid_cellid, gstk_grid_cellpos, gstk_grid_id,
     httpd,
     id,
     ign_req_index, ign_requests,
     index,
     inet_cache, inet_db, inet_hosts,
     'InitialReferences',
     int_db,
     interpreter_includedirs_macros,
     ir_WstringDef,
     lmcounter,  locks,
%     mnesia_decision,
     mnesia_gvar, mnesia_stats,
%     mnesia_transient_decision,
     pg2_table,
     queue,
     schema,
     shell_records,
     snmp_agent_table, snmp_local_db2, snmp_mib_data, snmp_note_store, snmp_symbolic_ets,
     tkFun, tkLink, tkPriv,
     ttb, ttb_history_table,
     udp_fds, udp_pids
    ].

sys_processes() ->
    [auth, code_server, global_name_server, inet_db,
     mnesia_recover, net_kernel, timer_server, wxe_master].

mnesia_tables() ->
    [ir_AliasDef, ir_ArrayDef, ir_AttributeDef, ir_ConstantDef,
     ir_Contained, ir_Container, ir_EnumDef, ir_ExceptionDef,
     ir_IDLType, ir_IRObject, ir_InterfaceDef, ir_ModuleDef,
     ir_ORB, ir_OperationDef, ir_PrimitiveDef, ir_Repository,
     ir_SequenceDef, ir_StringDef, ir_StructDef, ir_TypedefDef,
     ir_UnionDef, logTable, logTransferTable, mesh_meas,
     mesh_type, mnesia_clist, orber_CosNaming,
     orber_objkeys, user
    ].

handle_error(Foo) ->
    try
	Str = io_lib:format("ERROR: ~s~n",[Foo]),
	display_info(Str)
    catch _:_ ->
	    display_info(io_lib:format("ERROR: ~p~n",[Foo]))
    end,
    ok.

display_info(Str) ->
    Dlg = wxMessageDialog:new(wx:null(), Str),
    wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    ok.

interval_dialog(Parent, Enabled, Value, Min, Max) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Update Interval",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor
				?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    Check = wxCheckBox:new(Panel, ?wxID_ANY, "Periodical refresh"),
    wxCheckBox:setValue(Check, Enabled),
    Style = ?wxSL_HORIZONTAL bor ?wxSL_AUTOTICKS bor ?wxSL_LABELS,
    Slider = wxSlider:new(Panel, ?wxID_ANY, Value, Min, Max,
			  [{style, Style}, {size, {200, -1}}]),
    wxWindow:enable(Slider, [{enable, Enabled}]),
    InnerSizer = wxBoxSizer:new(?wxVERTICAL),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    Flags = [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}],
    wxSizer:add(InnerSizer, Check,  Flags),
    wxSizer:add(InnerSizer, Slider, Flags),
    wxPanel:setSizer(Panel, InnerSizer),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSizer, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSizer, Buttons, [{flag, ?wxEXPAND}]),
    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),
    wxCheckBox:connect(Check, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{commandInt=Enable0}},_) ->
					   Enable = Enable0 > 0,
					   wxWindow:enable(Slider, [{enable, Enable}])
				   end}]),
    Res = case wxDialog:showModal(Dialog) of
	      ?wxID_OK ->
		  {wxCheckBox:isChecked(Check), wxSlider:getValue(Slider)};
	      ?wxID_CANCEL ->
		  cancel
	  end,
    wxDialog:destroy(Dialog),
    Res.


update_grid(Grid, Opt, Tables) ->
    wx:batch(fun() -> update_grid2(Grid, Opt, Tables) end).
update_grid2(Grid, #opt{sort_key=Sort,sort_incr=Dir}, Tables) ->
    wxListCtrl:deleteAllItems(Grid),
    Update =
	fun(#tab{name = Name, id = Id, owner = Owner, size = Size, memory = Memory,
		 protection = Protection, reg_name = RegName}, Row) ->
		_Item = wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, {240,240,255});
		   true -> ignore
		end,
		if Protection == private ->
			wxListCtrl:setItemTextColour(Grid, Row, {200,130,50});
		   true -> ignore
		end,

		lists:foreach(fun({_, ignore}) -> ignore;
				 ({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col, to_str(Val))
			      end,
			      [{0,Name}, {1,Id}, {2,Size}, {3, Memory div 1024},
			       {4,Owner}, {5,RegName}]),
		%% wxListCtrl:setItemData(Grid, Item, Item),
		Row + 1
	end,
    ProcInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Tables));
		   true -> lists:keysort(Sort, Tables)
	       end,
    lists:foldl(Update, 0, ProcInfo),
    ProcInfo.

ignore(true, Reason) -> throw(Reason);
ignore(_,_ ) -> ok.
