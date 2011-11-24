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
-module(observer_pro_wx).

-behaviour(wx_object).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("../include/etop.hrl").
-include("observer_defs.hrl").
-include("etop_defs.hrl").

%% Defines
-define(COL_PID,  0).
-define(COL_NAME, 1).
-define(COL_TIME, 2).
-define(COL_REDS, 3).
-define(COL_MEM,  4).
-define(COL_MSG,  5).
-define(COL_FUN,  6).

-define(ID_KILL, 201).
-define(ID_PROC, 202).
-define(ID_REFRESH, 203).
-define(ID_REFRESH_INTERVAL, 204).
-define(ID_DUMP_TO_FILE, 205).
-define(ID_TRACEMENU, 206).
-define(ID_TRACE_ALL_MENU, 207).
-define(ID_TRACE_NEW_MENU, 208).
-define(ID_ACCUMULATE, 209).

%% Records

-record(sort,
	{
	  sort_key=?COL_REDS,
	  sort_incr=false
	}).

-record(holder, {parent,
		 info,
		 sort=#sort{},
		 accum=[],
		 attrs,
		 node,
		 backend_pid
		}).

-record(state, {parent,
		grid,
		panel,
		popup_menu,
		parent_notebook,
		timer,
		procinfo_menu_pids=[],
		sel={[], []},
		holder}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent]) ->
    Attrs = observer_lib:create_attrs(),
    Self = self(),
    Holder = spawn_link(fun() -> init_table_holder(Self, Attrs) end),
    {ProPanel, State} = setup(Notebook, Parent, Holder),
    {ProPanel, State#state{holder=Holder}}.

setup(Notebook, Parent, Holder) ->
    ProPanel = wxPanel:new(Notebook, []),

    Grid  = create_list_box(ProPanel, Holder),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),

    wxWindow:setSizer(ProPanel, Sizer),

    Popup = create_popup_menu(ProPanel),

    State = #state{parent=Parent,
		   grid=Grid,
		   panel=ProPanel,
		   popup_menu=Popup,
		   parent_notebook=Notebook,
		   holder=Holder,
		   timer={false, 10}
		   },
    {ProPanel, State}.


%% UI-creation

create_pro_menu(Parent, Holder) ->
    MenuEntries = [{"File",
		    [#create_menu{id=?ID_DUMP_TO_FILE, text="Dump to file"}]},
		   {"View",
		    [#create_menu{id=?ID_ACCUMULATE, text="Accumulate",
				  type=check,
				  check=call(Holder, {get_accum, self()})},
		     separator,
		     #create_menu{id=?ID_REFRESH, text="Refresh\tCtrl-R"},
		     #create_menu{id=?ID_REFRESH_INTERVAL, text="Refresh Interval"}]},
		   {"Trace",
		    [#create_menu{id=?ID_TRACEMENU, text="Trace selected processes"},
		     #create_menu{id=?ID_TRACE_NEW_MENU, text="Trace new processes"}
		     %% , #create_menu{id=?ID_TRACE_ALL_MENU, text="Trace all processes"}
		    ]}
		  ],
    observer_wx:create_menus(Parent, MenuEntries).

create_popup_menu(ParentFrame) ->
    MiniFrame = wxMiniFrame:new(ParentFrame, ?wxID_ANY, "Options", [{style, ?wxFRAME_FLOAT_ON_PARENT}]),
    Panel = wxPanel:new(MiniFrame),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    TraceBtn = wxButton:new(Panel, ?ID_TRACEMENU, [{label, "Trace selected"},
						   {style, ?wxNO_BORDER}]),
    ProcBtn = wxButton:new(Panel, ?ID_PROC, [{label, "Process info"},
					     {style, ?wxNO_BORDER}]),
    KillBtn = wxButton:new(Panel, ?ID_KILL, [{label, "Kill process"},
					     {style, ?wxNO_BORDER}]),

    wxButton:connect(TraceBtn, command_button_clicked),
    wxButton:connect(ProcBtn, command_button_clicked),
    wxButton:connect(KillBtn, command_button_clicked),
    wxSizer:add(Sizer, TraceBtn, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, ProcBtn, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, KillBtn, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:setSizeHints(Sizer, MiniFrame),
    MiniFrame.

create_list_box(Panel, Holder) ->
    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_HRULES,
    ListCtrl = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Row, Col) ->
					       call(Holder, {get_row, self(), Row, Col})
				       end},
				      {onGetItemAttr,
				       fun(_, Item) ->
					       call(Holder, {get_attr, self(), Item})
				       end}
				     ]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(ListCtrl, Col, Li),
			   wxListCtrl:setColumnWidth(ListCtrl, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Pid", ?wxLIST_FORMAT_CENTRE,  120},
		 {"Name or Initial Func", ?wxLIST_FORMAT_LEFT, 200},
		 {"Time", ?wxLIST_FORMAT_CENTRE, 50},
		 {"Reds", ?wxLIST_FORMAT_RIGHT, 100},
		 {"Memory", ?wxLIST_FORMAT_RIGHT, 100},
		 {"MsgQ",  ?wxLIST_FORMAT_RIGHT, 50},
		 {"Current Function", ?wxLIST_FORMAT_LEFT,  200}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:setItemCount(ListCtrl, 1),
    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    %% Use focused instead of selected, selected doesn't generate events
    %% for all multiple selections on Linux
    wxListCtrl:connect(ListCtrl, command_list_item_focused),
    ListCtrl.

dump_to_file(Parent, FileName, Holder) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    %% Holder closes the file when it's done
	    Holder ! {dump, Fd};
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    MD = wxMessageDialog:new(Parent, FailMsg),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD)
    end.

start_procinfo(undefined, _Frame, Opened) ->
    Opened;
start_procinfo(Pid, Frame, Opened) ->
    case lists:member(Pid, Opened) of
	true ->
	    Opened;
	false ->
	    observer_procinfo:start(Pid, Frame, self()),
	    [Pid | Opened]
    end.

call(Holder, What) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! What,
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    after 2000 ->
	    io:format("Hanging call ~p~n",[What]),
	    ""
    end.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, State0=#state{grid=Grid}) ->
    State = update_selection(State0),

    wxListCtrl:setItemCount(Grid, Count),
    wxListCtrl:refreshItems(Grid, 0, Count-1),

    {noreply, State};

handle_info(refresh_interval, #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_info({procinfo_menu_closed, Pid},
	    #state{procinfo_menu_pids=Opened}=State) ->
    NewPids = lists:delete(Pid, Opened),
    {noreply, State#state{procinfo_menu_pids=NewPids}};

handle_info({active, Node},
	    #state{holder=Holder, timer=Timer, parent=Parent}=State) ->
    create_pro_menu(Parent, Holder),
    Holder ! {change_node, Node},
    {noreply, State#state{timer=observer_lib:start_timer(Timer)}};

handle_info(not_active, #state{timer=Timer0}=State) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info({node, Node}, #state{holder=Holder}=State) ->
    Holder ! {change_node, Node},
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p:~p, Unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, #state{holder=Holder}) ->
    Holder ! stop,
    etop:stop(),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.


handle_call(Msg, _From, State) ->
    io:format("~p:~p: Unhandled call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    io:format("~p:~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=?ID_DUMP_TO_FILE}, #state{panel=Panel, holder=Holder}=State) ->
    FD  =  wxFileDialog:new(Panel,
			    [{style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    wxDialog:destroy(FD),
	    dump_to_file(Panel, Path, Holder);
	_ ->
	    wxDialog:destroy(FD)
    end,
    {noreply, State};

handle_event(#wx{id=?ID_ACCUMULATE,
		 event=#wxCommand{type=command_menu_selected, commandInt=CmdInt}},
	     #state{holder=Holder}=State) ->
    Holder ! {accum, CmdInt =:= 1},
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH, event=#wxCommand{type=command_menu_selected}},
	     #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     #state{panel=Panel, timer=Timer0}=State) ->
    Timer = observer_lib:interval_dialog(Panel, Timer0, 1, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(#wx{id=?ID_KILL},
	     #state{popup_menu=Pop,sel={[_|Ids], [ToKill|Pids]}}=State) ->
    wxWindow:show(Pop, [{show, false}]),
    exit(ToKill, kill),
    {noreply, State#state{sel={Ids,Pids}}};


handle_event(#wx{id = ?ID_PROC},
	     #state{panel=Panel,
		    popup_menu=Pop,
		    sel={_, [Pid|_]},
		    procinfo_menu_pids=Opened}=State) ->
    wxWindow:show(Pop, [{show, false}]),
    Opened2 = start_procinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(#wx{id = ?ID_TRACEMENU},
	     #state{popup_menu=Pop, sel={_, Pids}, panel=Panel}=State)  ->
    wxWindow:show(Pop, [{show, false}]),
    case Pids of
	[] ->
	    observer_wx:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Pids ->
	    observer_trace_wx:add_processes(observer_wx:get_tracer(), Pids),
	    {noreply,  State}
    end;

handle_event(#wx{id=?ID_TRACE_NEW_MENU, event=#wxCommand{type=command_menu_selected}}, State) ->
    observer_trace_wx:add_processes(observer_wx:get_tracer(), [new]),
    {noreply,  State};

handle_event(#wx{event=#wxSize{size={W,_}}},
	     #state{grid=Grid}=State) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-Cols*3-?LCTRL_WDECR, lists:seq(0, Cols - 2)),
		     Size = max(200, Last),
		     %% io:format("Width ~p ~p => ~p~n",[W, Last, Size]),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_right_click,
			       itemIndex=Row}},
	     #state{popup_menu=Popup,
		    holder=Holder}=State) ->

    case call(Holder, {get_row, self(), Row, pid}) of
	{error, undefined} ->
	    wxWindow:show(Popup, [{show, false}]),
	    undefined;
	{ok, _} ->
	    wxWindow:move(Popup, wx_misc:getMousePosition()),
	    wxWindow:show(Popup)
    end,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_focused,
			       itemIndex=Row}},
	     #state{grid=Grid,popup_menu=Pop,holder=Holder} = State) ->
    case Row >= 0 of
	true ->
	    wxWindow:show(Pop, [{show, false}]),
	    SelIds = [Row|lists:delete(Row, get_selected_items(Grid))],
	    Pids = call(Holder, {get_pids, self(), SelIds}),
	    %% io:format("Focused ~p -> ~p~n",[State#state.sel, {SelIds, Pids}]),
	    {noreply, State#state{sel={SelIds, Pids}}};
	false ->
	    %% io:format("Focused -1~n",[]),
	    {noreply, State}
    end;

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     #state{holder=Holder}=State) ->
    Holder !  {change_sort, Col},
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated}},
	     #state{panel=Panel, procinfo_menu_pids=Opened,
		    sel={_, [Pid|_]}}=State)
  when Pid =/= undefined ->
    Opened2 = start_procinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(Event, State) ->
    io:format("~p:~p: handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_selection(State=#state{holder=Holder, grid=Grid,
			      sel={SelIds0, SelPids0}}) ->
    Sel = {SelIds,_SelPids} = call(Holder, {get_rows_from_pids, self(), SelPids0}),
    set_focus(SelIds0, SelIds, Grid),
    case SelIds =:= SelIds0 of
	true -> ok;
	false ->
	    wx:batch(fun() ->
			     [wxListCtrl:setItemState(Grid, I, 0, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds0],
			     [wxListCtrl:setItemState(Grid, I, 16#FFFF, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds]
		     end)
    end,
    %%io:format("Update ~p -> ~p~n",[{SelIds0, SelPids0}, Sel]),
    State#state{sel=Sel}.

get_selected_items(Grid) ->
    get_selected_items(Grid, -1, []).

get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index, [{geometry, ?wxLIST_NEXT_ALL},
						{state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item | ItemAcc])
    end.

set_focus([], [], _Grid) -> ok;
set_focus([Same|_], [Same|_], _Grid) -> ok;
set_focus([], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED),
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED).


%%%%%%%%%%%%%%%%%%%%%%%%%%%TABLE HOLDER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table_holder(Parent, Attrs) ->
    Backend = spawn_link(node(), observer_backend,etop_collect,[self()]),
    table_holder(#holder{parent=Parent,
			 info=#etop_info{procinfo=[]},
			 node=node(),
			 backend_pid=Backend,
			 attrs=Attrs
			}).

table_holder(#holder{info=#etop_info{procinfo=Info}, attrs=Attrs,
		     node=Node, backend_pid=Backend}=S0) ->
    receive
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Info),
	    table_holder(S0);
	{get_attr, From, Row} ->
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
	{Backend, EtopInfo=#etop_info{}} ->
	    State = handle_update(EtopInfo, S0),
	    table_holder(State#holder{backend_pid=undefined});
	refresh when is_pid(Backend)->
	    table_holder(S0); %% Already updating
	refresh ->
	    Pid = spawn_link(Node,observer_backend,etop_collect,[self()]),
	    table_holder(S0#holder{backend_pid=Pid});
	{change_sort, Col} ->
	    State = change_sort(Col, S0),
	    table_holder(State);
	{get_pids, From, Indices} ->
	    get_pids(From, Indices, Info),
	    table_holder(S0);
	{get_rows_from_pids, From, Pids} ->
	    get_rows_from_pids(From, Pids, Info),
	    table_holder(S0);

	{get_node, From} ->
	    From ! {self(), Node},
	    table_holder(S0);
	{change_node, NewNode} ->
	    case Node == NewNode of
		true ->
		    table_holder(S0);
		false ->
		    self() ! refresh,
		    table_holder(S0#holder{node=NewNode})
	    end;
	{accum, Bool} ->
	    table_holder(change_accum(Bool,S0));
	{get_accum, From} ->
	    From ! {self(), S0#holder.accum == true},
	    table_holder(S0);
	{dump, Fd} ->
	    etop_txt:do_update(Fd, S0#holder.info, #opts{node=Node}),
	    file:close(Fd),
	    table_holder(S0);
	stop ->
	    ok;
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.

change_sort(Col, S0=#holder{parent=Parent, info=EI=#etop_info{procinfo=Data}, sort=Sort0}) ->
    {Sort, ProcInfo}=sort(Col, Sort0, Data),
    Parent ! {holder_updated, length(Data)},
    S0#holder{info=EI#etop_info{procinfo=ProcInfo}, sort=Sort}.

change_accum(true, S0) ->
    S0#holder{accum=true};
change_accum(false, S0=#holder{info=#etop_info{procinfo=Info}}) ->
    self() ! refresh,
    S0#holder{accum=lists:sort(Info)}.

handle_update(EI=#etop_info{procinfo=ProcInfo0},
	      S0=#holder{parent=Parent, sort=Sort=#sort{sort_key=KeyField}}) ->
    {ProcInfo1, S1} = accum(ProcInfo0, S0),
    {_SO, ProcInfo} = sort(KeyField, Sort#sort{sort_key=undefined}, ProcInfo1),
    Parent ! {holder_updated, length(ProcInfo)},
    S1#holder{info=EI#etop_info{procinfo=ProcInfo}}.

accum(ProcInfo, State=#holder{accum=true}) ->
    {ProcInfo, State};
accum(ProcInfo0, State=#holder{accum=Previous}) ->
    ProcInfo = lists:sort(ProcInfo0),
    {accum2(ProcInfo,Previous,[]), State#holder{accum=ProcInfo}}.

accum2([PI=#etop_proc_info{pid=Pid, reds=Reds, runtime=RT}|PIs],
       [#etop_proc_info{pid=Pid, reds=OldReds, runtime=OldRT}|Old], Acc) ->
    accum2(PIs, Old, [PI#etop_proc_info{reds=Reds-OldReds, runtime=RT-OldRT}|Acc]);
accum2(PIs=[#etop_proc_info{pid=Pid}|_], [#etop_proc_info{pid=OldPid}|Old], Acc)
  when Pid > OldPid ->
    accum2(PIs, Old, Acc);
accum2([PI|PIs], Old, Acc) ->
    accum2(PIs, Old, [PI|Acc]);
accum2([], _, Acc) -> Acc.

sort(Col, Opt=#sort{sort_key=Col, sort_incr=Bool}, Table) ->
    {Opt#sort{sort_incr=not Bool}, lists:reverse(Table)};
sort(Col, S=#sort{sort_incr=true}, Table) ->
    {S#sort{sort_key=Col}, lists:keysort(col_to_element(Col), Table)};
sort(Col, S=#sort{sort_incr=false}, Table) ->
    {S#sort{sort_key=Col}, lists:reverse(lists:keysort(col_to_element(Col), Table))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_procinfo_data(Col, Info) ->
    element(col_to_element(Col), Info).
col_to_element(?COL_PID)  -> #etop_proc_info.pid;
col_to_element(?COL_NAME) -> #etop_proc_info.name;
col_to_element(?COL_MEM)  -> #etop_proc_info.mem;
col_to_element(?COL_TIME) -> #etop_proc_info.runtime;
col_to_element(?COL_REDS) -> #etop_proc_info.reds;
col_to_element(?COL_FUN)  -> #etop_proc_info.cf;
col_to_element(?COL_MSG)  -> #etop_proc_info.mq.

get_pids(From, Indices, ProcInfo) ->
    Processes = [(lists:nth(I+1, ProcInfo))#etop_proc_info.pid || I <- Indices],
    From ! {self(), Processes}.

get_row(From, Row, pid, Info) ->
    Pid = case Row =:= -1 of
	      true ->  {error, undefined};
	      false -> {ok, get_procinfo_data(?COL_PID, lists:nth(Row+1, Info))}
	  end,
    From ! {self(), Pid};
get_row(From, Row, Col, Info) ->
    Data = case Row+1 > length(Info) of
	       true ->
		   "";
	       false ->
		   ProcInfo = lists:nth(Row+1, Info),
		   get_procinfo_data(Col, ProcInfo)
	   end,
    From ! {self(), observer_lib:to_str(Data)}.

get_rows_from_pids(From, Pids0, Info) ->
    Res = lists:foldl(fun(Pid, Data = {Ids, Pids}) ->
			      case index(Pid, Info, 0) of
				  false -> Data;
				  Index -> {[Index|Ids], [Pid|Pids]}
			      end
		      end, {[],[]}, Pids0),
    From ! {self(), Res}.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->  Attrs#attrs.even;
		    false -> Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.

index(Pid, [#etop_proc_info{pid=Pid}|_], Index) -> Index;
index(Pid, [_|PI], Index) -> index(Pid, PI, Index+1);
index(_, _, _) -> false.
