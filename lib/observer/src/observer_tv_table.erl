%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(observer_tv_table).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-export([format/1]).

-include("observer_defs.hrl").
-import(observer_lib, [to_str/1]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_tv.hrl").

-define(ID_TABLE_INFO, 400).
-define(ID_REFRESH, 401).
-define(ID_REFRESH_INTERVAL, 402).
-define(ID_EDIT, 403).
-define(ID_DELETE, 404).
-define(ID_SEARCH, 405).

-define(SEARCH_ENTRY, 420).
-define(GOTO_ENTRY,   421).

-define(DEFAULT_COL_WIDTH, 150).

-record(state,
	{
	  parent,
	  frame,
	  grid,
	  status,
	  sizer,
	  search,
	  selected,
	  node=node(),
	  columns,
	  pid,
	  source,
	  tab,
	  attrs,
	  timer={false, 30}
	}).

-record(opt,
	{
	  sort_key=2,
	  sort_incr=true
	}).

-record(search,
	{enable=true,          %  Subwindow is enabled
	 win,                  %  Sash Sub window obj
	 name,                 %  name

	 search,               %  Search input ctrl
	 goto,                 %  Goto  input ctrl
	 radio,                %  Radio buttons

	 find                  %  Search string
	}).

-record(find, {start,              % start pos
	       strlen,             % Found
	       found               % false
	      }).

start_link(Parent, Opts) ->
    wx_object:start_link(?MODULE, [Parent, Opts], []).

init([Parent, Opts]) ->
    Source = proplists:get_value(type, Opts),
    Table  = proplists:get_value(table, Opts),
    Node   = proplists:get_value(node, Opts),
    Title0 = atom_to_list(Table#tab.name) ++ " @ " ++ atom_to_list(Node),
    Title = case Source of
		ets -> "TV Ets: " ++ Title0;
		mnesia -> "TV Mnesia: " ++ Title0
	    end,
    Frame = wxFrame:new(Parent, ?wxID_ANY, Title, [{size, {800, 600}}]),
    IconFile = filename:join(code:priv_dir(observer), "erlang_observer.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon),
    MenuBar = wxMenuBar:new(),
    create_menus(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),
    %% wxFrame:setAcceleratorTable(Frame, AccelTable),
    wxMenu:connect(Frame, command_menu_selected),

    StatusBar = wxFrame:createStatusBar(Frame, []),
    try
	TabId = table_id(Table),
	ColumnNames = column_names(Node, Source, TabId),
	KeyPos = key_pos(Node, Source, TabId),

	Attrs = observer_lib:create_attrs(),

	Self = self(),
	Holder = spawn_link(fun() ->
				    init_table_holder(Self, Table, Source,
						      length(ColumnNames), Node, Attrs)
			    end),

	Panel = wxPanel:new(Frame),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
	Grid = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Item,Col) -> get_row(Holder, Item, Col+1) end},
				      {onGetItemAttr,
				       fun(_, Item) -> get_attr(Holder, Item) end}
				     ]),
	wxListCtrl:connect(Grid, command_list_item_activated),
	wxListCtrl:connect(Grid, command_list_item_selected),
	wxListCtrl:connect(Grid, command_list_col_click),
	wxListCtrl:connect(Grid, size, [{skip, true}]),
	wxWindow:setFocus(Grid),

	Search = search_area(Panel),
	wxSizer:add(Sizer, Grid,
		    [{flag, ?wxEXPAND bor ?wxALL}, {proportion, 1}, {border, 5}]),
	wxSizer:add(Sizer, Search#search.win,
		    [{flag,?wxEXPAND bor ?wxLEFT bor ?wxRIGHT bor
			  ?wxRESERVE_SPACE_EVEN_IF_HIDDEN},
		     {border, 5}]),
	wxWindow:setSizer(Panel, Sizer),
	wxSizer:hide(Sizer, Search#search.win),

	Cols = add_columns(Grid, 0, ColumnNames),
	wxFrame:show(Frame),
	{Panel, #state{frame=Frame, grid=Grid, status=StatusBar, search=Search,
		       sizer = Sizer,
		       parent=Parent, columns=Cols,
		       pid=Holder, source=Source, tab=Table#tab{keypos=KeyPos},
		       attrs=Attrs}}
    catch node_or_table_down ->
	    wxFrame:destroy(Frame),
	    stop
    end.

add_columns(Grid, Start, ColumnNames) ->
    Li = wxListItem:new(),
    AddListEntry = fun(Name, Col) ->
			   wxListItem:setText(Li, to_str(Name)),
			   wxListItem:setAlign(Li, ?wxLIST_FORMAT_LEFT),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, ?DEFAULT_COL_WIDTH),
			   Col + 1
		   end,
    Cols = lists:foldl(AddListEntry, Start, ColumnNames),
    wxListItem:destroy(Li),
    Cols.

create_menus(MB) ->
    File = wxMenu:new(),
    wxMenu:append(File, ?ID_TABLE_INFO, "Table Information\tCtrl-I"),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenuBar:append(MB, File, "File"),
    Edit = wxMenu:new(),
    wxMenu:append(Edit, ?ID_EDIT, "Edit Object"),
    wxMenu:append(Edit, ?ID_DELETE, "Delete Object\tCtrl-D"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?ID_SEARCH, "Search\tCtrl-S"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?ID_REFRESH, "Refresh\tCtrl-R"),
    wxMenu:append(Edit, ?ID_REFRESH_INTERVAL, "Refresh interval..."),
    wxMenuBar:append(MB, Edit, "Edit"),
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_HELP, "Help"),
    wxMenuBar:append(MB, Help, "Help"),
    ok.

search_area(Parent) ->
    HSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Find:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC1 = wxTextCtrl:new(Parent, ?SEARCH_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(HSz, TC1,  [{proportion,3}, {flag, ?wxEXPAND}]),
    Nbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Next"),
    wxRadioButton:setValue(Nbtn, true),
    wxSizer:add(HSz,Nbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Pbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Previous"),
    wxSizer:add(HSz,Pbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Cbtn = wxCheckBox:new(Parent, ?wxID_ANY, "Match Case"),
    wxSizer:add(HSz,Cbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(HSz, 15,15, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Goto Entry:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC2 = wxTextCtrl:new(Parent, ?GOTO_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(HSz, TC2,  [{proportion,0}, {flag, ?wxEXPAND}]),
    wxTextCtrl:connect(TC1, command_text_updated),
    wxTextCtrl:connect(TC1, command_text_enter),
    wxTextCtrl:connect(TC1, kill_focus),
    wxTextCtrl:connect(TC2, command_text_enter),
    wxWindow:connect(Parent, command_button_clicked),

    #search{name='Search Area', win=HSz,
	    search=TC1,goto=TC2,radio={Nbtn,Pbtn,Cbtn}}.

edit(Index, #state{pid=Pid, frame=Frame}) ->
    Str = get_row(Pid, Index, all_multiline),
    case observer_lib:user_term_multiline(Frame, "Edit object:", Str) of
	cancel -> ok;
	{ok, Term} -> Pid ! {edit, Index, Term};
	Err = {error, _} -> self() ! Err
    end.

handle_event(#wx{id=?ID_REFRESH},State = #state{pid=Pid}) ->
    Pid ! refresh,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{pid=Pid, grid=Grid, selected=OldSel}) ->
    SelObj = case OldSel of
                 undefined -> undefined;
                 _ -> get_row(Pid, OldSel, term)
             end,
    Pid ! {sort, Col+1},
    case SelObj =/= undefined andalso search(Pid, SelObj, -1, true, term) of
        false when is_integer(OldSel) ->
            wxListCtrl:setItemState(Grid, OldSel, 0, ?wxLIST_STATE_SELECTED),
            {noreply, State#state{selected=undefined}};
        false ->
            {noreply, State#state{selected=undefined}};
        Row ->
            wxListCtrl:setItemState(Grid, Row, 16#FFFF, ?wxLIST_STATE_SELECTED),
            {noreply, State#state{selected=Row}}
    end;

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Index}},
	     State = #state{pid=Pid, grid=Grid, status=StatusBar}) ->
    N = wxListCtrl:getItemCount(Grid),
    Str = get_row(Pid, Index, all),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Objects: ~w: ~s",[N, Str])),
    {noreply, State#state{selected=Index}};

handle_event(#wx{event=#wxList{type=command_list_item_activated, itemIndex=Index}},
	     State) ->
    edit(Index, State),
    {noreply, State};

handle_event(#wx{id=?ID_EDIT}, State = #state{selected=undefined}) ->
    {noreply, State};
handle_event(#wx{id=?ID_EDIT}, State = #state{selected=Index}) ->
    edit(Index, State),
    {noreply, State};

handle_event(#wx{id=?ID_DELETE}, State = #state{selected=undefined}) ->
    {noreply, State};
handle_event(#wx{id=?ID_DELETE},
	     State = #state{grid=Grid, pid=Pid, status=StatusBar, selected=Index}) ->
    Str = get_row(Pid, Index, all),
    Pid ! {delete, Index},
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Deleted object: ~s",[Str])),
    wxListCtrl:setItemState(Grid, Index, 0, ?wxLIST_STATE_FOCUSED),
    {noreply, State#state{selected=undefined}};

handle_event(#wx{id=?wxID_CLOSE}, State = #state{frame=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, State};

handle_event(Help = #wx{id=?wxID_HELP}, State) ->
    observer ! Help,
    {noreply, State};

handle_event(#wx{id=?GOTO_ENTRY, event=#wxCommand{cmdString=Str}},
	     State = #state{grid=Grid}) ->
    try
	Row0 = list_to_integer(Str),
	Row1 = max(0, Row0),
	Row  = min(wxListCtrl:getItemCount(Grid)-1,Row1),
	wxListCtrl:ensureVisible(Grid, Row),
	ok
    catch _:_ -> ok
    end,
    {noreply, State};

%% Search functionality
handle_event(#wx{id=?ID_SEARCH},
	     State = #state{grid=Grid, sizer=Sz, search=Search, selected=Index}) ->
    is_integer(Index) andalso
	wxListCtrl:setItemState(Grid, Index, 0, ?wxLIST_STATE_FOCUSED),
    wxSizer:show(Sz, Search#search.win),
    wxWindow:setFocus(Search#search.search),
    wxSizer:layout(Sz),
    {noreply, State};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxFocus{}},
	     State = #state{search=Search, pid=Pid}) ->
    Pid ! {mark_search_hit, false},
    {noreply, State#state{search=Search#search{find=undefined}}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=""}},
	     State = #state{search=Search, pid=Pid}) ->
    Pid ! {mark_search_hit, false},
    {noreply, State#state{search=Search#search{find=undefined}}};
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{type=command_text_enter,cmdString=Str}},
	     State = #state{grid=Grid, pid=Pid, status=SB,
			    search=Search=#search{radio={Next0, _, Case0},
						  find=Find}})
  when Find =/= undefined ->
    Dir  = wxRadioButton:getValue(Next0) xor wx_misc:getKeyState(?WXK_SHIFT),
    Case = wxCheckBox:getValue(Case0),
    Pos = if Find#find.found, Dir ->  %% Forward Continuation
		  Find#find.start+1;
	     Find#find.found ->  %% Backward Continuation
		  Find#find.start-1;
	     Dir ->   %% Forward wrap
		  0;
	     true ->  %% Backward wrap
		  wxListCtrl:getItemCount(Grid)-1
	  end,
    Pid ! {mark_search_hit, false},
    case search(Pid, Str, Pos, Dir, Case) of
	false ->
	    wxStatusBar:setStatusText(SB, io_lib:format("Not found (regexp): ~s",[Str])),
	    Pid ! {mark_search_hit, Find#find.start},
	    wxListCtrl:refreshItem(Grid, Find#find.start),
	    {noreply, State#state{search=Search#search{find=Find#find{found=false}}}};
	Row ->
	    wxListCtrl:ensureVisible(Grid, Row),
	    wxListCtrl:refreshItem(Grid, Row),
	    Status = "Found: (Hit Enter for next, Shift-Enter for previous)",
	    wxStatusBar:setStatusText(SB, Status),
	    {noreply, State#state{search=Search#search{find=#find{start=Row, found=true}}}}
    end;
handle_event(#wx{id=?SEARCH_ENTRY, event=#wxCommand{cmdString=Str}},
	     State = #state{grid=Grid, pid=Pid, status=SB,
			    search=Search=#search{radio={Next0, _, Case0},
						  find=Find}}) ->
    try
	Dir  = wxRadioButton:getValue(Next0),
	Case = wxCheckBox:getValue(Case0),
	Start = case Dir of
		    true -> 0;
		    false -> wxListCtrl:getItemCount(Grid)-1
		end,
	Cont = case Find of
		   undefined ->
		       #find{start=Start, strlen=length(Str)};
		   #find{strlen=Old} when Old < length(Str) ->
		       Find#find{start=Start, strlen=length(Str)};
		   _ ->
		       Find#find{strlen=length(Str)}
	       end,

	Pid ! {mark_search_hit, false},
	case search(Pid, Str, Cont#find.start, Dir, Case) of
	    false ->
		wxStatusBar:setStatusText(SB, io_lib:format("Not found (regexp): ~s",[Str])),
		{noreply, State};
	    Row ->
		wxListCtrl:ensureVisible(Grid, Row),
		wxListCtrl:refreshItem(Grid, Row),
		Status = "Found: (Hit Enter for next, Shift-Enter for previous)",
		wxStatusBar:setStatusText(SB, Status),
		{noreply, State#state{search=Search#search{find=#find{start=Row, found=true}}}}
	end
    catch _:_ -> {noreply, State}
    end;

handle_event(#wx{id=?ID_TABLE_INFO},
	     State = #state{frame=Frame, node=Node, source=Source, tab=Table}) ->
    observer_tv_wx:display_table_info(Frame, Node, Source, Table),
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, timer=Timer0}) ->
    Timer = observer_lib:interval_dialog(Grid, Timer0, 10, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(_Event, State) ->
    %io:format("~p:~p, handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_sync_event(_Event, _Obj, _State) ->
    %io:format("~p:~p, handle sync_event ~p\n", [?MODULE, ?LINE, Event]),
    ok.

handle_call(_Event, _From, State) ->
    %io:format("~p:~p, handle call (~p) ~p\n", [?MODULE, ?LINE, From, Event]),
    {noreply, State}.

handle_cast(_Event, State) ->
    %io:format("~p:~p, handle cast ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info({no_rows, N}, State = #state{grid=Grid, status=StatusBar}) ->
    wxListCtrl:setItemCount(Grid, N),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Objects: ~w",[N])),
    {noreply, State};

handle_info({new_cols, New}, State = #state{grid=Grid, columns=Cols0}) ->
    Cols = add_columns(Grid, Cols0, New),
    {noreply, State#state{columns=Cols}};

handle_info({refresh, Min, Min}, State = #state{grid=Grid}) ->
    wxListCtrl:refreshItem(Grid, Min), %% Avoid assert in wx below if Max is 0
    {noreply, State};
handle_info({refresh, Min, Max}, State = #state{grid=Grid}) ->
    Max > 0 andalso wxListCtrl:refreshItems(Grid, Min, Max),
    {noreply, State};

handle_info(refresh_interval, State = #state{pid=Pid}) ->
    Pid ! refresh,
    {noreply, State};

handle_info({error, Error}, State = #state{frame=Frame}) ->
    ErrorStr =
	try io_lib:format("~ts", [Error]), Error
	catch _:_ -> io_lib:format("~p", [Error])
	end,
    Dlg = wxMessageDialog:new(Frame, ErrorStr),
    wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    {noreply, State};

handle_info(_Event, State) ->
    %% io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, _Event]),
    {noreply, State}.

terminate(_Event, #state{pid=Pid, attrs=Attrs}) ->
    %% ListItemAttr are not auto deleted
    #attrs{odd=Odd, even=Even, deleted=D, searched=S,
	   changed_odd=Ch1, changed_even=Ch2,
	   new_odd=New1, new_even=New2
	  } = Attrs,
    wxListItemAttr:destroy(Odd), wxListItemAttr:destroy(Even),
    wxListItemAttr:destroy(D),
    wxListItemAttr:destroy(Ch1),wxListItemAttr:destroy(Ch2),
    wxListItemAttr:destroy(New1),wxListItemAttr:destroy(New2),
    wxListItemAttr:destroy(S),
    unlink(Pid),
    exit(Pid, window_closed),
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Table holder needs to be in a separate process otherwise
%%  the callback get_row/3 may deadlock if the process do
%%  wx calls when callback is invoked.
get_row(Table, Item, Column) ->
    Ref = erlang:monitor(process, Table),
    Table ! {get_row, self(), Item, Column},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

get_attr(Table, Item) ->
    Ref = erlang:monitor(process, Table),
    Table ! {get_attr, self(), Item},
    receive
	{'DOWN', Ref, _, _, _} -> wx:null();
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

search(Table, Str, Row, Dir, Case) ->
    Ref = erlang:monitor(process, Table),
    Table ! {search, [Str, Row, Dir, Case]},
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Table, Res} ->
	    erlang:demonitor(Ref),
	    Res
    end.

-record(holder, {node, parent, pid,
		 table=array:new(), n=0, columns,
		 temp=[],
		 search,
		 source, tabid,
		 sort,
		 key,
		 type,
		 attrs
		}).

init_table_holder(Parent, Table, MnesiaOrEts, Cols, Node, Attrs) ->
    TabId = case Table#tab.id of
		ignore -> Table#tab.name;
		Id -> Id
	    end,
    self() ! refresh,
    table_holder(#holder{node=Node, parent=Parent,
			 source=MnesiaOrEts, tabid=TabId, columns=Cols,
			 sort=#opt{sort_key=Table#tab.keypos, sort_incr=true},
			 type=Table#tab.type, key=Table#tab.keypos,
			 attrs=Attrs}).

table_holder(S0 = #holder{parent=Parent, pid=Pid, table=Table}) ->
    receive
	{get_attr, From, Row} ->
	    get_attr(From, Row, S0),
	    table_holder(S0);
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Table),
	    table_holder(S0);
	{Pid, Data} ->
	    S1 = handle_new_data_chunk(Data, S0),
	    table_holder(S1);
	{sort, Col} ->
	    Parent ! {refresh, 0, S0#holder.n-1},
	    table_holder(sort(Col, S0));
	{search, Data} ->
	    table_holder(search(Data, S0));
	{mark_search_hit, Row} ->
	    Old = S0#holder.search,
	    is_integer(Old) andalso (Parent ! {refresh, Old, Old}),
	    table_holder(S0#holder{search=Row});
	refresh when is_pid(Pid) ->
	    %% Already getting the table...
	    %% io:format("ignoring refresh", []),
	    table_holder(S0);
	refresh ->
	    GetTab = rpc:call(S0#holder.node, observer_backend, get_table,
			      [self(), S0#holder.tabid, S0#holder.source]),
	    table_holder(S0#holder{pid=GetTab});
	{delete, Row} ->
	    delete_row(Row, S0),
	    table_holder(S0);
	{edit, Row, Term} ->
	    edit_row(Row, Term, S0),
	    table_holder(S0);
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    Parent ! {refresh, 0, S0#holder.n-1},
	    table_holder(S0)
    end.

handle_new_data_chunk(Data, S0 = #holder{columns=Cols, parent=Parent}) ->
    S1 = #holder{n=N,columns=NewCols} = handle_new_data_chunk2(Data, S0),
    Parent ! {no_rows, N},
    Parent ! {refresh, 0, N-1},
    case NewCols =:= Cols of
	true -> S1;
	false ->
	    Parent ! {new_cols, lists:seq(Cols+1, NewCols)},
	    S1
    end.

handle_new_data_chunk2('$end_of_table',
		       S0 = #holder{sort=Opt0, key=Key,
				    table=Old, temp=New}) ->
    Merged = merge(array:to_list(Old), New, Key),
    {Opt,Sorted} = sort(Opt0#opt.sort_key, Opt0#opt{sort_key = undefined}, Merged),
    SortedA = array:from_list(Sorted),
    S0#holder{sort=Opt, table=SortedA, n=array:size(SortedA), temp=[], pid=undefined};
handle_new_data_chunk2(Data, S0 = #holder{columns=Cols0, source=ets, temp=Tab0}) ->
    {Tab, Cols} = parse_ets_data(Data, Cols0, Tab0),
    S0#holder{columns=Cols, temp=Tab};
handle_new_data_chunk2(Data, S0 = #holder{source=mnesia, temp=Tab}) ->
    S0#holder{temp=(Data ++ Tab)}.

parse_ets_data([[Rec]|Rs], C, Tab) ->
    parse_ets_data(Rs, max(tuple_size(Rec), C), [Rec|Tab]);
parse_ets_data([Recs|Rs], C0, Tab0) ->
    {Tab, Cols} = parse_ets_data(Recs, C0, Tab0),
    parse_ets_data(Rs, Cols, Tab);
parse_ets_data([], Cols, Tab) ->
    {Tab, Cols}.

sort(Col, S=#holder{sort=Opt0, table=Table0}) ->
    {Opt, Table} = sort(Col, Opt0, array:to_list(Table0)),
    S#holder{sort=Opt, table=array:from_list(Table)}.

sort(Col, Opt = #opt{sort_key=Col, sort_incr=Bool}, Table) ->
    {Opt#opt{sort_incr=not Bool}, lists:reverse(Table)};
sort(Col, S=#opt{sort_incr=true}, Table) ->
    {S#opt{sort_key=Col}, keysort(Col, Table)};
sort(Col, S=#opt{sort_incr=false}, Table) ->
    {S#opt{sort_key=Col}, lists:reverse(keysort(Col, Table))}.

keysort(Col, Table) ->
    Sort = fun([A0|_], [B0|_]) ->
		   A = try element(Col, A0) catch _:_ -> [] end,
		   B = try element(Col, B0) catch _:_ -> [] end,
		   case A == B of
		       true -> A0 =< B0;
		       false -> A < B
		   end;
	      (A0, B0) when is_tuple(A0), is_tuple(B0) ->
		   A = try element(Col, A0) catch _:_ -> [] end,
		   B = try element(Col, B0) catch _:_ -> [] end,
		   case A == B of
		       true -> A0 =< B0;
		       false -> A < B
		   end
	   end,
    lists:sort(Sort, Table).

search([Term, -1, true, term], S=#holder{parent=Parent, table=Table}) ->
    Search = fun(Idx, [Tuple|_]) ->
                     Tuple =:= Term andalso throw(Idx),
                     Tuple
             end,
    try array:map(Search, Table) of
        _ -> Parent ! {self(), false}
    catch Index ->
            Parent ! {self(), Index}
    end,
    S;
search([Str, Row, Dir0, CaseSens],
       S=#holder{parent=Parent, n=N, table=Table}) ->
    Opt = case CaseSens of
	      true -> [];
	      false -> [caseless]
	  end,
    Dir = case Dir0 of
	      true -> 1;
	      false -> -1
	  end,
    Res = case re:compile(Str, Opt) of
	      {ok, Re} -> re_search(Row, Dir, N, Re, Table);
	      {error, _} -> false
	  end,
    Parent ! {self(), Res},
    S#holder{search=Res}.

re_search(Row, Dir, N, Re, Table) when Row >= 0, Row < N ->
    [Term|_] = array:get(Row, Table),
    Str = format(Term),
    Res = re:run(Str, Re),
    case Res of
	nomatch -> re_search(Row+Dir, Dir, N, Re, Table);
	{match,_} ->
	    Row
    end;
re_search(_, _, _, _, _) ->
    false.

get_row(From, Row, Col, Table) ->
    case array:get(Row, Table) of
	[Object|_] when Col =:= all ->
	    From ! {self(), format(Object)};
	[Object|_] when Col =:= all_multiline ->
	    From ! {self(), io_lib:format("~p", [Object])};
        [Object|_] when Col =:= term ->
	    From ! {self(), Object};
	[Object|_] when tuple_size(Object) >= Col ->
	    From ! {self(), format(element(Col, Object))};
	_ ->
	    From ! {self(), ""}
    end.

get_attr(From, Row, #holder{attrs=Attrs, search=Row}) ->
    What = Attrs#attrs.searched,
    From ! {self(), What};
get_attr(From, Row, #holder{table=Table, attrs=Attrs}) ->
    Odd = (Row rem 2) > 0,
    What = case array:get(Row, Table) of
	       [_|deleted]  -> Attrs#attrs.deleted;
	       [_|changed] when Odd -> Attrs#attrs.changed_odd;
	       [_|changed] -> Attrs#attrs.changed_even;
	       [_|new] when Odd -> Attrs#attrs.new_odd;
	       [_|new] -> Attrs#attrs.new_even;
	       _ when Odd -> Attrs#attrs.odd;
	       _ ->  Attrs#attrs.even
	   end,
    From ! {self(), What}.

merge([], New, _Key) ->
    [[N] || N <- New]; %% First time
merge(Old, New, Key) ->
    merge2(keysort(Key, Old), keysort(Key, New), Key).

-dialyzer({no_improper_lists, merge2/3}).
merge2([[Obj|_]|Old], [Obj|New], Key) ->
    [[Obj]|merge2(Old, New, Key)];
merge2([[A|Op]|Old], [B|New], Key)
  when element(Key, A) == element(Key, B) ->
    case Op of
	deleted ->
	    [[B|new]|merge2(Old, New, Key)];
	_ ->
	    [[B|changed]|merge2(Old, New, Key)]
    end;
merge2([[A|Op]|Old], New = [B|_], Key)
  when element(Key, A) < element(Key, B) ->
    case Op of
	deleted -> merge2(Old, New, Key);
	_ -> [[A|deleted]|merge2(Old, New, Key)]
    end;
merge2(Old = [[A|_]|_], [B|New], Key)
  when element(Key, A) > element(Key, B) ->
    [[B|new]|merge2(Old, New, Key)];
merge2([], New, _Key) ->
    [[N|new] || N <- New];
merge2(Old, [], _Key) ->
    lists:foldl(fun([_O|deleted], Acc) -> Acc;
		   ([O|_], Acc) -> [[O|deleted]|Acc]
		end, [], Old).


delete_row(Row, S0 = #holder{parent=Parent}) ->
    case delete(Row, S0) of
	ok ->
	    self() ! refresh;
	{error, Err} ->
	    Parent ! {error, "Could not delete object: " ++ Err}
    end.


delete(Row, #holder{tabid=Id, table=Table,
		    source=Source, node=Node}) ->
    [Object|_] = array:get(Row, Table),
    try
	case Source of
	    ets ->
		true = rpc:call(Node, ets, delete_object, [Id, Object]);
	    mnesia ->
		ok = rpc:call(Node, mnesia, dirty_delete_object, [Id, Object])
	end,
	ok
    catch _:_Error ->
	    {error, "node or table is not available"}
    end.

edit_row(Row, Term, S0 = #holder{parent=Parent}) ->
    case delete(Row, S0) of
	ok ->
	    case insert(Term, S0) of
		ok -> self() ! refresh;
		Err -> Parent ! {error, Err}
	    end;
	{error, Err} ->
	    Parent ! {error, "Could not edit object: " ++ Err}
    end.

insert(Object, #holder{tabid=Id, source=Source, node=Node}) ->
    try
	case Source of
	    ets ->
		true = rpc:call(Node, ets, insert, [Id, Object]);
	    mnesia ->
		ok = rpc:call(Node, mnesia, dirty_write, [Id, Object])
	end,
	ok
    catch _:_Error ->
	    {error, "node or table is not available"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

column_names(Node, Type, Table) ->
    case Type of
	ets -> [1, 2];
	mnesia ->
	    Attrs = rpc:call(Node, mnesia, table_info, [Table, attributes]),
	    is_list(Attrs) orelse throw(node_or_table_down),
	    ["Record Name"|Attrs]
    end.

table_id(#tab{id=ignore, name=Name}) -> Name;
table_id(#tab{id=Id}) -> Id.

key_pos(_, mnesia, _) -> 2;
key_pos(Node, ets, TabId) ->
    KeyPos = rpc:call(Node, ets, info, [TabId, keypos]),
    is_integer(KeyPos) orelse throw(node_or_table_down),
    KeyPos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format(Tuple) when is_tuple(Tuple) ->
    [${ |format_tuple(Tuple, 1, tuple_size(Tuple))];
format(List) when is_list(List) ->
    format_list(List);
format(Bin) when is_binary(Bin), byte_size(Bin) > 100 ->
    io_lib:format("<<#Bin:~w>>", [byte_size(Bin)]);
format(Bin) when is_binary(Bin) ->
    try
	true = printable_list(unicode:characters_to_list(Bin)),
	io_lib:format("<<\"~ts\">>", [Bin])
    catch _:_ ->
	    io_lib:format("~w", [Bin])
    end;
format(Float) when is_float(Float) ->
    io_lib:format("~.3g", [Float]);
format(Term) ->
    io_lib:format("~w", [Term]).

format_tuple(Tuple, I, Max) when I < Max ->
    [format(element(I, Tuple)), $,|format_tuple(Tuple, I+1, Max)];
format_tuple(Tuple, Max, Max) ->
    [format(element(Max, Tuple)), $}];
format_tuple(_Tuple, 1, 0) ->
    [$}].

format_list([]) -> "[]";
format_list(List) ->
    case printable_list(List) of
	true ->  io_lib:format("\"~ts\"", [map_printable_list(List)]);
	false -> [$[ | make_list(List)]
    end.

make_list([Last]) ->
    [format(Last), $]];
make_list([Head|Tail]) when is_list(Tail) ->
    [format(Head), $,|make_list(Tail)];
make_list([Head|Tail]) ->
    [format(Head), $|, format(Tail), $]].

map_printable_list([$\n|Cs]) ->
    [$\\, $n|map_printable_list(Cs)];
map_printable_list([$\r|Cs]) ->
    [$\\, $r|map_printable_list(Cs)];
map_printable_list([$\t|Cs]) ->
    [$\\, $t|map_printable_list(Cs)];
map_printable_list([$\v|Cs]) ->
    [$\\, $v|map_printable_list(Cs)];
map_printable_list([$\b|Cs]) ->
    [$\\, $b|map_printable_list(Cs)];
map_printable_list([$\f|Cs]) ->
    [$\\, $f|map_printable_list(Cs)];
map_printable_list([$\e|Cs]) ->
    [$\\, $e|map_printable_list(Cs)];
map_printable_list([]) -> [];
map_printable_list([C|Cs]) ->
    [C|map_printable_list(Cs)].

%% printable_list([Char]) -> bool()
%%  Return true if CharList is a list of printable characters, else
%%  false.

printable_list([C|Cs]) when is_integer(C), C >= $ , C =< 255 ->
    printable_list(Cs);
printable_list([$\n|Cs]) ->
    printable_list(Cs);
printable_list([$\r|Cs]) ->
    printable_list(Cs);
printable_list([$\t|Cs]) ->
    printable_list(Cs);
printable_list([$\v|Cs]) ->
    printable_list(Cs);
printable_list([$\b|Cs]) ->
    printable_list(Cs);
printable_list([$\f|Cs]) ->
    printable_list(Cs);
printable_list([$\e|Cs]) ->
    printable_list(Cs);
printable_list([]) -> true;
printable_list(_Other) -> false.	     %Everything else is false
