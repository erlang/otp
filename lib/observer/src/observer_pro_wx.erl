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
	 handle_event/2, handle_cast/2, to_str/1]).
-export([get_row/4, get_attr/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").
-include("observer_defs.hrl").

%% Defines
-define(COL_PID,  0).
-define(COL_NAME, 1).
-define(COL_TIME, 2).
-define(COL_REDS, 3).
-define(COL_MEM, 4).
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
-define(ID_NO_OF_LINES, 209).
-define(ID_ACCUMULATE, 210).

-define(START_LINES, 50). %% hardcoded startvalue representing the number of visible lines

%% Records
-record(attrs, {even, odd, deleted, changed, searched}).

-record(holder, {parent,
		 info,
		 attrs}).


-record(pro_wx_state, {parent,
		       etop_monitor,
		       holder_monitor,
		       grid,
		       panel,
		       popup_menu,
		       parent_notebook,
		       trace_options = #trace_options{},
		       match_specs = [],
		       refr_timer = false,
		       tracemenu_opened,
		       procinfo_menu_pids = [],
		       selected_pids = [],
		       last_selected,
		       sort_dir = decr, % decr::atom | incr::incr
		       holder}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent]) ->
    {EtopMonitor, Config} = etop:start(observer),
    SortDir = decr,
    Attrs = create_attrs(),
    Self = self(),
    change_lines(?START_LINES),
    Info = etop:update(Config, SortDir),
    {Holder, HolderMon} = spawn_monitor(fun() ->
						init_table_holder(Self,
								  Info,
								  Attrs)
					end),
    Count = length(Info#etop_info.procinfo),
    {ProPanel, State} = setup(Notebook, Parent, Holder, Count),
    refresh_grid(Holder, SortDir),
    MatchSpecs = generate_matchspecs(),
    {ProPanel, State#pro_wx_state{etop_monitor = EtopMonitor,
				  holder_monitor = HolderMon,
				  sort_dir = SortDir,
				  match_specs = MatchSpecs}}.

setup(Notebook, Parent, Holder, Count) ->
    ProPanel = wxPanel:new(Notebook, []),

    Grid = create_list_box(ProPanel, Holder, Count),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),

    wxWindow:setSizer(ProPanel, Sizer),

    Popup = create_popup_menu(ProPanel),

    State =  #pro_wx_state{parent = Parent,
			   grid        = Grid,
			   panel       = ProPanel,
			   popup_menu = Popup,
			   parent_notebook = Notebook,
			   tracemenu_opened = false,
			   holder = Holder},
    {ProPanel, State}.

generate_matchspecs() ->
    try
	StrMs1 = "[{'_', [], [{return_trace}]}].",
	StrMs2 = "[{'_', [], [{exception_trace}]}].",
	StrMs3 = "[{'_', [], [{message, {caller}}]}].",
	StrMs4 = "[{'_', [], [{message, {process_dump}}]}].",

	{ok, Tokens1, _} = erl_scan:string(StrMs1),
	{ok, Tokens2, _} = erl_scan:string(StrMs2),
	{ok, Tokens3, _} = erl_scan:string(StrMs3),
	{ok, Tokens4, _} = erl_scan:string(StrMs4),
	{ok, Term1} = erl_parse:parse_term(Tokens1),
	{ok, Term2} = erl_parse:parse_term(Tokens2),
	{ok, Term3} = erl_parse:parse_term(Tokens3),
	{ok, Term4} = erl_parse:parse_term(Tokens4),

	[#match_spec{term_ms = Term1, str_ms = StrMs1},
	 #match_spec{term_ms = Term2, str_ms = StrMs2},
	 #match_spec{term_ms = Term3, str_ms = StrMs3},
	 #match_spec{term_ms = Term4, str_ms = StrMs4}]
    catch
	_:_ ->
	    []
    end.

%% UI-creation

create_pro_menu(Parent) ->
    MenuEntries = [{"View",
		    [#create_menu{id = ?ID_REFRESH, text = "Refresh"},
		     #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh Interval"}]},
		   {"Options",
		    [#create_menu{id = ?ID_DUMP_TO_FILE, text = "Dump to file"},
		     #create_menu{id = ?ID_ACCUMULATE, text = "Accumulate", type = check},
		     #create_menu{id = ?ID_NO_OF_LINES, text = "Number of lines"}]},
		   {"Trace",
		    [#create_menu{id = ?ID_TRACEMENU, text = "Trace selected processes"},
		     #create_menu{id = ?ID_TRACE_NEW_MENU, text = "Trace new processes"},
		     #create_menu{id = ?ID_TRACE_ALL_MENU, text = "Trace all processes"}]}
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

create_list_box(Panel, Holder, Count) ->
    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL,
    ListCtrl = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Item, Col) -> get_row(Holder, Item, Col) end},
				      {onGetItemAttr,
				       fun(_, Item) -> get_attr(Holder, Item) end}
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
		 {"Reds", ?wxLIST_FORMAT_CENTRE, 50},
		 {"Memory", ?wxLIST_FORMAT_CENTRE, 50},
		 {"MsgQ",  ?wxLIST_FORMAT_LEFT, 50},
		 {"Current Function", ?wxLIST_FORMAT_LEFT,  200}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    wxListCtrl:connect(ListCtrl, command_list_item_selected),
    wxListCtrl:setItemCount(ListCtrl, Count),
    ListCtrl.

change_node(Node) ->
    etop_server ! {config, {node, Node}}.

get_node() ->
    etop_server ! {get_opt, node, self()},
    receive
	{node, Node} ->
	    Node
    end.

change_accum(Bool) ->
    etop_server ! {config, {accumulate, Bool}}.

change_lines(Int) when is_integer(Int) ->
    etop_server ! {config, {lines, Int}}.

get_lines() ->
    etop_server ! {get_opt, lines, self()},
    receive
	{lines, Lines} ->
	    Lines
    end.

change_intv(NewIntv) ->
    etop_server ! {config, {interval, NewIntv}}.

get_intv() ->
    etop_server ! {get_opt, intv, self()},
    receive
	{intv, Intv} ->
	    Intv
    end.

get_sort() ->
    etop_server ! {get_opt, sort, self()},
    receive {sort, Sort} ->
	    Sort
    end.

refresh_grid(Holder, Dir) ->
    etop_server ! {update, Holder, Dir}.

change_sort(Col, Dir) ->
    case get_sort() =:= map_sort_order(Col) of
	true when Dir =:= incr->
	    decr;
	true when Dir =:= decr ->
	    incr;
	false ->
	    change_sort(Col),
	    Dir
    end.
change_sort(?COL_PID) ->
    etop_server ! {config, {sort, pid}};
change_sort(?COL_NAME) ->
    etop_server ! {config, {sort, name}};
change_sort(?COL_TIME) ->
    etop_server ! {config, {sort, runtime}};
change_sort(?COL_REDS) ->
    etop_server ! {config, {sort, reductions}};
change_sort(?COL_MEM) ->
    etop_server ! {config, {sort, memory}};
change_sort(?COL_MSG) ->
    etop_server ! {config, {sort, msg_q}};
change_sort(?COL_FUN) ->
    etop_server ! {config, {sort, cf}}.


map_sort_order(Col) ->
    case Col of
	?COL_PID  -> pid;
	?COL_NAME -> name;
	?COL_TIME -> runtime;
	?COL_REDS -> reductions;
	?COL_MEM  -> memory;
	?COL_MSG  -> msg_q;
	?COL_FUN  -> cf
    end.

to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({A, B}) ->
    lists:concat([A, ":", B]);
to_str({M,F,A}) ->
    lists:concat([M, ":", F, "/", A]);
to_str(Value) when is_list(Value) ->
    case lists:all(fun(X) -> is_integer(X) end, Value) of
	true -> Value;
	false ->
	    lists:foldl(fun(X, Acc) ->
				to_str(X) ++ " " ++ Acc end,
			"", Value)
    end;
to_str(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_str(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_str(No) when is_integer(No) ->
    integer_to_list(No);
to_str(ShouldNotGetHere) ->
    erlang:error({?MODULE, to_str, ShouldNotGetHere}).

clear_all(Grid) ->
    lists:foreach(fun(I) ->
			  wxListCtrl:setItemState(Grid, I, 0, ?wxLIST_STATE_SELECTED),
			  wxListCtrl:setItemState(Grid, I, 0, ?wxLIST_STATE_FOCUSED)
		  end,
		  lists:seq(0, wxListCtrl:getItemCount(Grid))).

set_selected_items(Grid, Holder, Pids) ->
    Count = wxListCtrl:getItemCount(Grid),
    set_selected_items(Grid, Holder, 0, Pids, Count, []).

set_selected_items(_, _, Index, Pids, Max, Acc) when Pids =:= []; Index =:= Max ->
    Acc;
set_selected_items(Grid, Holder, Index, Pids, Max, Acc) ->
    {ok, Pid} = get_row(Holder, Index, pid),
    case lists:member(Pid, Pids) of
	true ->
	    wxListCtrl:setItemState(Grid, Index,
				    ?wxLIST_STATE_SELECTED,
				    ?wxLIST_STATE_SELECTED),
	    set_selected_items(Grid, Holder, Index+1, lists:delete(Pid, Pids), Max, [Pid | Acc]);
	false ->
	    set_selected_items(Grid, Holder, Index+1, Pids, Max, Acc)
    end.

get_selected_items(Grid) ->
    get_selected_items(Grid, -1, []).

get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index,
				  [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item+1 | ItemAcc])
    end.

interval_dialog(ParentFrame, ParentPid, Enabled, Value, Min, Max) ->
    Dialog = wxDialog:new(ParentFrame, ?wxID_ANY, "Update Interval",
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

    OKBtn = wxButton:new(Dialog, ?wxID_OK),
    CancelBtn = wxButton:new(Dialog, ?wxID_CANCEL),
    Buttons = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(Buttons, OKBtn),
    wxStdDialogButtonSizer:addButton(Buttons, CancelBtn),

    Flags = [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}],
    wxSizer:add(InnerSizer, Check,  Flags),
    wxSizer:add(InnerSizer, Slider, Flags),
    wxPanel:setSizer(Panel, InnerSizer),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSizer, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSizer, Buttons, [{flag, ?wxEXPAND}]),
    wxStdDialogButtonSizer:realize(Buttons),
    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),
    wxCheckBox:connect(Check, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{type = command_checkbox_clicked,
							    commandInt=Enable0}},_) ->
					   Enable = Enable0 > 0,
					   wxWindow:enable(Slider, [{enable, Enable}])
				   end}]),

    wxButton:connect(OKBtn, command_button_clicked,
		     [{callback,
		       fun(#wx{id = ?wxID_OK,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       ParentPid ! {wxCheckBox:isChecked(Check), wxSlider:getValue(Slider)},
			       wxDialog:destroy(Dialog)
		       end}]),
    wxButton:connect(CancelBtn, command_button_clicked,
		     [{callback,
		       fun(#wx{id = ?wxID_CANCEL,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       ParentPid ! cancel,
			       wxDialog:destroy(Dialog)
		       end}]),

    wxDialog:show(Dialog).



line_dialog(ParentFrame, OldLines, Holder, Dir) ->
    Dialog = wxDialog:new(ParentFrame, ?wxID_ANY, "Enter number of lines",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, OldLines}]),
    InnerSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(InnerSz, TxtCtrl, [{flag, ?wxEXPAND bor ?wxALL}]),
    wxPanel:setSizer(Panel, InnerSz),

    OKBtn = wxButton:new(Dialog, ?wxID_OK),
    CancelBtn = wxButton:new(Dialog, ?wxID_CANCEL),

    Buttons = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(Buttons, OKBtn),
    wxStdDialogButtonSizer:addButton(Buttons, CancelBtn),

    TopSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSz, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSz, Buttons, [{flag, ?wxEXPAND}]),
    wxStdDialogButtonSizer:realize(Buttons),
    wxWindow:setSizerAndFit(Dialog, TopSz),
    wxSizer:setSizeHints(TopSz, Dialog),

    wxButton:connect(OKBtn, command_button_clicked,
		     [{callback,
		       fun(#wx{id = ?wxID_OK,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       try
				   NewLines = list_to_integer(wxTextCtrl:getValue(TxtCtrl)),
				   case NewLines >= 0 of
				       true ->
					   change_lines(NewLines),
					   refresh_grid(Holder, Dir);
				       false ->
					   observer_wx:create_txt_dialog(Panel,
									 "Invalid input",
									 "Error",
									 ?wxICON_ERROR)
				   end
			       catch error:badarg ->
				       observer_wx:create_txt_dialog(Panel,
								     "Invalid input",
								     "Error",
								     ?wxICON_ERROR)
			       end,
			       wxDialog:destroy(Dialog)
		       end}]),
    wxButton:connect(CancelBtn, command_button_clicked,
		     [{callback,
		       fun(#wx{id = ?wxID_CANCEL,
			       event = #wxCommand{type = command_button_clicked}},_) ->
			       wxDialog:destroy(Dialog)
		       end}]),
    wxDialog:show(Dialog).


create_attrs() ->
    Font = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Text = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOXTEXT),
    #attrs{even = wx:typeCast(wx:null(), wxListItemAttr),
	   odd  = wxListItemAttr:new(Text, {240,240,255}, Font),
	   searched = wxListItemAttr:new(Text, {235,215,90}, Font)
	  }.

dump_to_file(Parent, FileName, Holder) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    Holder ! {dump, Fd};
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    MD = wxMessageDialog:new(Parent, FailMsg),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD)
    end.

start_procinfo(Node, Pid, Frame, Opened) ->
    case lists:member(Pid, Opened) of
	true ->
	    Opened;
	false ->
	    observer_procinfo:start(Node, Pid, Frame, self()),
	    [Pid | Opened]
    end.



get_selected_pids(Holder, Indices) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_pids, self(), Indices},
    receive
	{'DOWN', Ref, _, _, _} -> [];
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

get_row(Holder, Row, Column) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_row, self(), Row, Column},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.


get_attr(Holder, Item) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! {get_attr, self(), Item},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.


%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({'DOWN', Ref, _, _, _},
	    #pro_wx_state{etop_monitor = EtopMon} = State) when Ref =:= EtopMon ->
    io:format("Etop died~n"),
    {stop, shutdown, State};

handle_info({'DOWN', Ref, _, _, _},
	    #pro_wx_state{holder_monitor = HMonitor} = State) when Ref =:= HMonitor ->
    io:format("Holder died~n"),
    {stop, shutdown, State};

handle_info({holder_updated, Count}, #pro_wx_state{grid = Grid,
						   holder = Holder,
						   selected_pids = Pids} = State) ->
    Pids2 = wx:batch(fun() ->
			     wxListCtrl:setItemCount(Grid, Count),
			     clear_all(Grid),
			     Pids2 = set_selected_items(Grid, Holder, Pids),
			     wxListCtrl:refreshItems(Grid, 0, Count),
			     Pids2
		     end),
    {noreply, State#pro_wx_state{selected_pids = Pids2}};

handle_info(refresh_interval, #pro_wx_state{sort_dir = Dir,
					    holder = Holder} = State) ->
    refresh_grid(Holder, Dir),
    {noreply, State};

handle_info({tracemenu_closed, TraceOpts, MatchSpecs}, State) ->
    {noreply, State#pro_wx_state{tracemenu_opened = false,
				 trace_options = TraceOpts,
				 match_specs = MatchSpecs}};

handle_info({procinfo_menu_closed, Pid},
	    #pro_wx_state{procinfo_menu_pids = Opened} = State) ->
    NewPids = lists:delete(Pid, Opened),
    {noreply, State#pro_wx_state{procinfo_menu_pids = NewPids}};

handle_info({active, Node}, #pro_wx_state{holder = Holder,
					  sort_dir = Dir,
					  refr_timer = Timer0,
					  parent = Parent} = State) ->
    create_pro_menu(Parent),
    change_node(Node),
    refresh_grid(Holder, Dir),
    Timer = case Timer0 of
		true ->
		    Intv = get_intv(),
		    {ok, Ref} = timer:send_interval(Intv, refresh_interval),
		    Ref;
		false ->
		    false
	    end,
    {noreply, State#pro_wx_state{refr_timer = Timer}};

handle_info(not_active, #pro_wx_state{refr_timer = Timer0} = State) ->
    Timer = case Timer0 of
		false -> false;
		true -> true;
		Timer0 ->
		    timer:cancel(Timer0),
		    true
	    end,
    {noreply, State#pro_wx_state{refr_timer=Timer,
				 selected_pids = [],
				 last_selected = undefined}};

handle_info({node, Node}, #pro_wx_state{holder = Holder, sort_dir = Dir} = State) ->
    change_node(Node),
    refresh_grid(Holder, Dir),
    {noreply, State#pro_wx_state{selected_pids = [],
				 last_selected = undefined}};

handle_info(Info, State) ->
    io:format("~p, ~p, Handled unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, #pro_wx_state{holder = Holder}) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    Holder ! stop,
    etop:stop(),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.


handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id = ?ID_DUMP_TO_FILE},
	     #pro_wx_state{panel = Panel,
			   holder = Holder} = State) ->
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

handle_event(#wx{id = ?ID_ACCUMULATE, event = #wxCommand{type = command_menu_selected,
							 commandInt = CmdInt}},
	     #pro_wx_state{holder = Holder,
			   sort_dir = Dir} = State) when CmdInt =:= 1->
    change_accum(true),
    refresh_grid(Holder, Dir),
    {noreply, State};

handle_event(#wx{id = ?ID_ACCUMULATE, event = #wxCommand{type = command_menu_selected,
							 commandInt = CmdInt}},
	     #pro_wx_state{holder = Holder,
			   sort_dir = Dir} = State) when CmdInt =:= 0 ->
    change_accum(false),
    refresh_grid(Holder, Dir),
    {noreply, State};

handle_event(#wx{id = ?ID_NO_OF_LINES, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{panel = Panel,
			   sort_dir = Dir,
			   holder = Holder} = State) ->
    OldLines = integer_to_list(get_lines()),
    line_dialog(Panel, OldLines, Holder, Dir),
    {noreply, State};

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{sort_dir = Dir, holder = Holder} = State) ->
    refresh_grid(Holder, Dir),
    {noreply, State};

handle_event(#wx{id = ?ID_REFRESH_INTERVAL},
	     #pro_wx_state{panel = Panel, refr_timer=Timer0} = State) ->
    Intv0 = get_intv() div 1000,
    interval_dialog(Panel, self(), Timer0 /= false, Intv0, 1, 5*60),
    receive
	cancel ->
	    {noreply, State};
	{true, Intv} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    change_intv(Intv),
	    {ok, Timer} = timer:send_interval(Intv * 1000, refresh_interval),
	    {noreply, State#pro_wx_state{refr_timer=Timer}};
	{false, _} ->
	    case Timer0 of
		false -> ok;
		_ -> timer:cancel(Timer0)
	    end,
	    {noreply, State#pro_wx_state{refr_timer=false}}
    end;

handle_event(#wx{id = ?ID_KILL}, #pro_wx_state{popup_menu = Pop,
					       selected_pids = Pids,
					       last_selected = ToKill} = State) ->

    wxWindow:show(Pop, [{show, false}]),
    exit(ToKill, kill),
    Pids2 = lists:delete(ToKill, Pids),
    {noreply, State#pro_wx_state{selected_pids = Pids2, last_selected = undefined}};


handle_event(#wx{id = ?ID_PROC},
	     #pro_wx_state{panel = Panel,
			   popup_menu = Pop,
			   last_selected = Pid,
			   procinfo_menu_pids = Opened} = State) ->
    wxWindow:show(Pop, [{show, false}]),
    Node = get_node(),
    Opened2 = start_procinfo(Node, Pid, Panel, Opened),
    {noreply, State#pro_wx_state{procinfo_menu_pids = Opened2}};

handle_event(#wx{id = ?ID_TRACEMENU},
	     #pro_wx_state{popup_menu = Pop,
			   trace_options = Options,
			   match_specs = MatchSpecs,
			   selected_pids = Pids,
			   tracemenu_opened = false,
			   panel = Panel} = State)  ->
    wxWindow:show(Pop, [{show, false}]),
    case Pids of
	[] ->
	    observer_wx:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Pids ->
	    Node = get_node(),
	    observer_trace_wx:start(Node,
				    Pids,
				    Options,
				    MatchSpecs,
				    Panel,
				    self()),
	    {noreply,  State#pro_wx_state{tracemenu_opened = true}}
    end;

handle_event(#wx{id = ?ID_TRACE_ALL_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{trace_options = Options,
			   match_specs = MatchSpecs,
			   tracemenu_opened = false,
			   panel = Panel} = State) ->
    Node = get_node(),
    observer_trace_wx:start(Node,
			    all,
			    Options,
			    MatchSpecs,
			    Panel,
			    self()),
    {noreply, State#pro_wx_state{tracemenu_opened = true}};


handle_event(#wx{id = ?ID_TRACE_NEW_MENU, event = #wxCommand{type = command_menu_selected}},
	     #pro_wx_state{trace_options = Options,
			   match_specs = MatchSpecs,
			   tracemenu_opened = false,
			   panel = Panel} = State) ->
    Node = get_node(),
    observer_trace_wx:start(Node,
			    new,
			    Options,
			    MatchSpecs,
			    Panel,
			    self()),
    {noreply,  State#pro_wx_state{tracemenu_opened = true}};

handle_event(#wx{event=#wxSize{size={W,_}}},
	     #pro_wx_state{grid=Grid} = State) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-2, lists:seq(0, Cols - 2)),
		     Size = max(200, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
    {noreply, State};

handle_event(#wx{event = #wxList{type = command_list_item_right_click,
				 itemIndex = Row}},
	     #pro_wx_state{popup_menu = Popup,
			   holder = Holder} = State) ->

    case get_row(Holder, Row, pid) of
	{error, undefined} ->
	    wxWindow:show(Popup, [{show, false}]),
	    undefined;
	{ok, _} ->
	    wxWindow:move(Popup, wx_misc:getMousePosition()),
	    wxWindow:show(Popup)
    end,
    {noreply, State};

handle_event(#wx{event = #wxList{type = command_list_item_selected,
				 itemIndex = Row}},
	     #pro_wx_state{grid = Grid,
			   popup_menu = Pop,
			   holder = Holder} = State) ->

    NewPid = case get_row(Holder, Row, pid) of
		 {error, undefined} ->
		     undefined;
		 {ok, P} when is_pid(P) ->
		     P
	     end,
    wxWindow:show(Pop, [{show, false}]),
    Pids = get_selected_pids(Holder, get_selected_items(Grid)),
    {noreply, State#pro_wx_state{selected_pids = Pids,
				 last_selected = NewPid}};

handle_event(#wx{event = #wxList{type = command_list_col_click, col = Col}},
	     #pro_wx_state{sort_dir = OldDir,
			   holder = Holder} = State) ->
    NewDir = change_sort(Col, OldDir),
    refresh_grid(Holder, NewDir),
    {noreply, State#pro_wx_state{sort_dir = NewDir}};

handle_event(#wx{event = #wxList{type = command_list_item_activated}},
	     #pro_wx_state{panel = Panel,
			   procinfo_menu_pids= Opened,
			   last_selected = Pid} = State) when Pid =/= undefined ->
    Node = get_node(),
    Opened2 = start_procinfo(Node, Pid, Panel, Opened),
    {noreply, State#pro_wx_state{procinfo_menu_pids = Opened2}};

handle_event(Event, State) ->
    io:format("~p~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

























%%%%%%%%%%%%%%%%%%%%%%%%%%%TABLE HOLDER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_table_holder(Parent, Info, Attrs) ->
    table_holder(#holder{parent = Parent,
			 info = Info,
			 attrs = Attrs}).


table_holder(#holder{parent = Parent,
		     info = Info,
		     attrs = Attrs} = S0) ->
    receive
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Info#etop_info.procinfo),
	    table_holder(S0);
	{get_attr, From, Row} ->
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
	{get_pids, From, Indices} ->
	    get_pids(From, Indices, Info#etop_info.procinfo),
	    table_holder(S0);
	{update, #etop_info{procinfo = ProcInfo} = NewInfo} ->
	    Parent ! {holder_updated, length(ProcInfo)},
	    table_holder(S0#holder{info = NewInfo});
	{dump, Fd} ->
	    etop_server ! {observer_dump, Fd, Info},
	    table_holder(S0);
	stop ->
	    ok;
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.


get_procinfo_data(?COL_PID, #etop_proc_info{pid = Pid}) ->
    Pid;
get_procinfo_data(?COL_NAME, #etop_proc_info{name = Name})  ->
    Name;
get_procinfo_data(?COL_MEM, #etop_proc_info{mem = Mem}) ->
    Mem;
get_procinfo_data(?COL_TIME, #etop_proc_info{runtime = RT}) ->
    RT;
get_procinfo_data(?COL_REDS, #etop_proc_info{reds = Reds}) ->
    Reds;
get_procinfo_data(?COL_FUN, #etop_proc_info{cf = CF}) ->
    CF;
get_procinfo_data(?COL_MSG, #etop_proc_info{mq = MQ}) ->
    MQ.

get_pids(From, Indices, ProcInfo) ->
    From ! {self(),
	    [X#etop_proc_info.pid || X <-
					 [lists:nth(I, ProcInfo) || I <- Indices]]}.

get_row(From, Row, pid, Info) ->
    Pid = case Row =:= -1 of
	      true ->
		  {error, undefined};
	      false ->
		  {ok, get_procinfo_data(?COL_PID, lists:nth(Row+1, Info))}
	  end,
    From ! {self(), Pid};
get_row(From, Row, Col, Info) ->
    Data = case Row+1 > length(Info) of
	       true ->
		   null;
	       false ->
		   ProcInfo = lists:nth(Row+1, Info),
		   get_procinfo_data(Col, ProcInfo)
	   end,
    From ! {self(), io_lib:format("~p", [Data])}.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->
			Attrs#attrs.even;
		    false ->
			Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.
