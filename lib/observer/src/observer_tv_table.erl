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

-module(observer_tv_table).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-export([get_table/3]).

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
	  timer
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
    Frame = wxFrame:new(Parent, ?wxID_ANY, Title, [{size, {800, 300}}]),
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
    Str = get_row(Pid, Index, all),
    Dialog = wxTextEntryDialog:new(Frame, "Edit object:", [{value, Str}]),
    case wxTextEntryDialog:showModal(Dialog) of
	?wxID_OK ->
	    New = wxTextEntryDialog:getValue(Dialog),
	    wxTextEntryDialog:destroy(Dialog),
	    case Str =:= New of
		true -> ok;
		false ->
		    complete_edit(Index, New, Pid)
	    end;
	?wxID_CANCEL ->
	    wxTextEntryDialog:destroy(Dialog)
    end.

complete_edit(Row, New0, Pid) ->
    New = case lists:reverse(New0) of
	      [$.|_] -> New0;
	      _ -> New0 ++ "."
	  end,
    try
	{ok, Tokens, _} = erl_scan:string(New),
	{ok, Term} = erl_parse:parse_term(Tokens),
	Pid ! {edit, Row, Term}
    catch _:{badmatch, {error, {_, _, Err}}} ->
	    self() ! {error, ["Parse error: ", Err]};
	  _Err ->
	    self() ! {error, ["Syntax error in: ", New]}
    end.

handle_event(#wx{id=?ID_REFRESH},State = #state{pid=Pid}) ->
    Pid ! refresh,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{pid=Pid}) ->
    Pid ! {sort, Col+1},
    {noreply, State};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    wx:batch(fun() ->
		     Cols = wxListCtrl:getColumnCount(Grid),
		     Last = lists:foldl(fun(I, Last) ->
						Last - wxListCtrl:getColumnWidth(Grid, I)
					end, W-?LCTRL_WDECR, lists:seq(0, Cols - 2)),
		     Size = max(?DEFAULT_COL_WIDTH, Last),
		     wxListCtrl:setColumnWidth(Grid, Cols-1, Size)
	     end),
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
	     State = #state{pid=Pid, status=StatusBar, selected=Index}) ->
    Str = get_row(Pid, Index, all),
    Pid ! {delete, Index},
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Deleted object: ~s",[Str])),
    {noreply, State};

handle_event(#wx{id=?wxID_CLOSE}, State) ->
    {stop, normal, State};

handle_event(Help = #wx{id=?wxID_HELP}, State = #state{parent=Parent}) ->
    Parent ! Help,
    {noreply, State};

handle_event(#wx{id=?GOTO_ENTRY, event=#wxCommand{cmdString=Str}},
	     State = #state{grid=Grid}) ->
    try
	Row0 = list_to_integer(Str),
	Row1 = min(0, Row0),
	Row  = max(wxListCtrl:getItemCount(Grid)-1,Row1),
	wxListCtrl:ensureVisible(Grid, Row),
	ok
    catch _:_ -> ok
    end,
    {noreply, State};

%% Search functionality
handle_event(#wx{id=?ID_SEARCH},
	     State = #state{sizer=Sz, search=Search}) ->
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
	    wxStatusBar:setStatusText(SB, "Not found"),
	    Pid ! {mark_search_hit, Find#find.start},
	    wxListCtrl:refreshItem(Grid, Find#find.start),
	    {noreply, State#state{search=Search#search{find=#find{found=false}}}};
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
		wxStatusBar:setStatusText(SB, "Not found"),
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

handle_info({no_rows, N}, State = #state{grid=Grid, status=StatusBar}) ->
    wxListCtrl:setItemCount(Grid, N),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Objects: ~w",[N])),
    {noreply, State};
handle_info({new_cols, New}, State = #state{grid=Grid, columns=Cols0}) ->
    Cols = add_columns(Grid, Cols0, New),
    {noreply, State#state{columns=Cols}};
handle_info({refresh, Min, Max}, State = #state{grid=Grid}) ->
    wxListCtrl:refreshItems(Grid, Min, Max),
    {noreply, State};
handle_info({error, Error}, State = #state{frame=Frame}) ->
    Dlg = wxMessageDialog:new(Frame, Error),
    wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    {noreply, State};

handle_info(Event, State) ->
    io:format("~p:~p, handle info ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

terminate(_Event, #state{pid=Pid, attrs=Attrs}) ->
    %% ListItemAttr are not auto deleted
    #attrs{odd=Odd, deleted=D, changed=Ch, searched=S} = Attrs,
    wxListItemAttr:destroy(Odd),
    wxListItemAttr:destroy(D),
    wxListItemAttr:destroy(Ch),
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
	{'DOWN', Ref, _, _, _} -> "";
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
		 table=[], n=0, columns,
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
	    GetTab = rpc:call(S0#holder.node, ?MODULE, get_table,
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
	    table_holder(S0)
    end.

handle_new_data_chunk(Data, S0 = #holder{columns=Cols, parent=Parent}) ->
    S1 = #holder{columns=NewCols} = handle_new_data_chunk2(Data, S0),
    case NewCols =:= Cols of
	true -> S1;
	false ->
	    Parent ! {new_cols, lists:seq(Cols+1, NewCols)},
	    S1
    end.

handle_new_data_chunk2('$end_of_table',
		       S0 = #holder{parent=Parent, sort=Opt,
				    key=Key,
				    table=Old, temp=New}) ->
    Table = merge(Old, New, Key),
    N = length(Table),
    Parent ! {no_rows, N},
    sort(Opt#opt.sort_key, S0#holder{n=N, pid=undefine,
				     sort=Opt#opt{sort_key = undefined},
				     table=Table, temp=[]});
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

sort(Col, S=#holder{n=N, parent=Parent, sort=Opt0, table=Table0}) ->
    {Opt, Table} = sort(Col, Opt0, Table0),
    Parent ! {refresh, 0, N-1},
    S#holder{sort=Opt, table=Table}.

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

search([Str, Row, Dir0, CaseSens],
       S=#holder{parent=Parent, table=Table}) ->
    Opt = case CaseSens of
	      true -> [];
	      false -> [caseless]
	  end,
    {ok, Re} = re:compile(Str, Opt),
    Dir = case Dir0 of
	      true -> 1;
	      false -> -1
	  end,
    Res = search(Row, Dir, Re, Table),
    Parent ! {self(), Res},
    S#holder{search=Res}.

search(Row, Dir, Re, Table) ->
    Res = try lists:nth(Row+1, Table) of
	      Term ->
		  Str = io_lib:format("~w", [Term]),
		  re:run(Str, Re)
	  catch _:_ -> no_more
	  end,
    case Res of
	nomatch -> search(Row+Dir, Dir, Re, Table);
	no_more -> false;
	{match,_} -> Row
    end.

get_row(From, Row, Col, Table) ->
    case lists:nth(Row+1, Table) of
	[Object|_] when Col =:= all ->
	    From ! {self(), io_lib:format("~w", [Object])};
	[Object|_] when tuple_size(Object) >= Col ->
	    From ! {self(), io_lib:format("~w", [element(Col, Object)])};
	_ ->
	    From ! {self(), ""}
    end.

get_attr(From, Row, #holder{attrs=Attrs, search=Row}) ->
    What = Attrs#attrs.searched,
    From ! {self(), What};
get_attr(From, Row, #holder{table=Table, attrs=Attrs}) ->
    What = case lists:nth(Row+1, Table) of
	       [_|deleted]  -> Attrs#attrs.deleted;
	       [_|changed]  -> Attrs#attrs.changed;
	       [_|new]      -> Attrs#attrs.changed;
	       _ when (Row rem 2) > 0 ->
		   Attrs#attrs.odd;
	       _ ->
		   Attrs#attrs.even
	   end,
    From ! {self(), What}.

merge([], New, _Key) ->
    [[N] || N <- New]; %% First time
merge(Old, New, Key) ->
    merge2(keysort(Key, Old), keysort(Key, New), Key).

merge2([[Obj|_]|Old], [Obj|New], Key) ->
    [[Obj]|merge2(Old, New, Key)];
merge2([[A|_]|Old], [B|New], Key)
  when element(Key, A) == element(Key, B) ->
    [[B|changed]|merge2(Old, New, Key)];
merge2([[A|_]|Old], New = [B|_], Key)
  when element(Key, A) < element(Key, B) ->
    [[A|deleted]|merge2(Old, New, Key)];
merge2(Old = [[A|_]|_], [B|New], Key)
  when element(Key, A) > element(Key, B) ->
    [[B|new]|merge2(Old, New, Key)];
merge2([], New, _Key) ->
    [[N|new] || N <- New];
merge2(Old, [], _Key) ->
    [[O|deleted] || [O|_] <- Old].


delete_row(Row, S0 = #holder{parent=Parent}) ->
    case delete(Row, S0) of
	ok ->
	    self() ! refresh;
	{error, Err} ->
	    Parent ! {error, "Could not delete object: " ++ Err}
    end.


delete(Row, #holder{tabid=Id, table=Table,
		    source=Source, node=Node}) ->
    [Object|_] = lists:nth(Row+1, Table),
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
get_table(Parent, Table, Module) ->
    spawn(fun() ->
		  link(Parent),
		  get_table2(Parent, Table, Module)
	  end).

get_table2(Parent, Table, Type) ->
    Size = case Type of
	       ets -> ets:info(Table, size);
	       mnesia -> mnesia:table_info(Table, size)
	   end,
    case Size > 0 of
	false ->
	    Parent ! {self(), '$end_of_table'},
	    normal;
	true when Type =:= ets ->
	    Mem = ets:info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    get_ets_loop(Parent, ets:match(Table, '$1', NoElements));
	true ->
	    Mem = mnesia:table_info(Table, memory),
	    Average = Mem div Size,
	    NoElements = max(10, 20000 div Average),
	    Ms = [{'$1', [], ['$1']}],
	    Get = fun() ->
			  get_mnesia_loop(Parent, mnesia:select(Table, Ms, NoElements, read))
		  end,
	    %% Not a transaction, we don't want to grab locks when inspecting the table
	    mnesia:async_dirty(Get)
    end.

get_ets_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_ets_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_ets_loop(Parent, ets:match(Cont)).

get_mnesia_loop(Parent, '$end_of_table') ->
    Parent ! {self(), '$end_of_table'};
get_mnesia_loop(Parent, {Match, Cont}) ->
    Parent ! {self(), Match},
    get_ets_loop(Parent, mnesia:select(Cont)).

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
