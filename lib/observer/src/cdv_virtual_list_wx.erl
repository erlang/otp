%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(cdv_virtual_list_wx).

-behaviour(wx_object).

-export([start_link/2, start_link/3,
	 start_detail_win/1, start_detail_win/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

%% Defines
-define(COL_ID,  0).
-define(ID_DETAILS, 202).

%% Records

-record(sort,
	{
	  sort_key,
	  sort_incr=true
	}).

-record(holder, {parent,
		 info,
		 last_row,
		 sort,
		 attrs,
		 callback
		}).

-record(state, {grid,
		panel,
		detail_wins=[],
		holder,
		callback,
		trunc_warn=[],
		menu_cols=[],     % columns to show in right click menu
		menu_items=[]}).  % right click menu items for the selected row

start_link(ParentWin, Callback) ->
    wx_object:start_link({local,Callback},?MODULE,
			 [ParentWin, Callback, all], []).

start_link(ParentWin, Callback, Owner) ->
    wx_object:start_link(?MODULE, [ParentWin, Callback, Owner], []).

start_detail_win(Id) ->
    case Id of
	"<"++_ ->
	    start_detail_win(Id, process);
	"#Port"++_ ->
	    start_detail_win(Id, port);
	_ ->
	    io:format("cdv: unknown identifier: ~p~n",[Id]),
	    ignore
    end.

start_detail_win(Id, process) ->
    start_detail_win_2(cdv_proc_cb, Id);
start_detail_win(Id, port) ->
    start_detail_win_2(cdv_port_cb, Id);
start_detail_win(Id, node) ->
    start_detail_win_2(cdv_dist_cb, Id);
start_detail_win(Id, module) ->
    start_detail_win_2(cdv_mod_cb, Id);
start_detail_win(Id, ets) ->
    start_detail_win_2(cdv_ets_cb, Id);
start_detail_win(Id, sched) ->
    start_detail_win_2(cdv_sched_cb, Id).


start_detail_win_2(Callback,Id) ->
    wx_object:cast(Callback,{start_detail_win,Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([ParentWin, Callback, Owner]) ->
    {Holder,TW} = spawn_table_holder(Callback, Owner),
    Panel = wxPanel:new(ParentWin),
    {Grid,MenuCols}  = create_list_box(Panel, Holder, Callback, Owner),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),

    wxWindow:setSizer(Panel, Sizer),

    State = #state{grid=Grid,
		   panel=Panel,
		   holder=Holder,
		   callback=Callback,
		   trunc_warn=TW,
		   menu_cols=MenuCols
		   },
    {Panel, State}.

%% UI-creation

create_list_box(Panel, Holder, Callback, Owner) ->
    Style =
	?wxLC_SINGLE_SEL bor ?wxLC_REPORT bor ?wxLC_VIRTUAL bor
	?wxLC_HRULES bor ?wxHSCROLL bor ?wxVSCROLL,
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
    ListItems = Callback:col_spec(),
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:setItemCount(ListCtrl, 0),
    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_col_click),


    %% If detail pages can be opened from this list - catch double
    %% click and right click
    DetailCols =
	case catch Callback:get_detail_cols(Owner) of
	    {DC,DoubleClick} when is_list(DC), DC=/=[] ->
		wxListCtrl:connect(ListCtrl, command_list_item_right_click),
		if DoubleClick ->
			wxListCtrl:connect(ListCtrl, command_list_item_activated);
		   true ->
			ok
		end,
		DC;
	    _ ->
		[]
	end,

    {ListCtrl,DetailCols}.

do_start_detail_win(undefined, State) ->
    State;
do_start_detail_win(Id, #state{panel=Panel,detail_wins=Opened,
			       holder=Holder,callback=Callback}=State) ->
    NewOpened =
	case lists:keyfind(Id, 1, Opened) of
	    false ->
		Data = call(Holder, {get_data, self(), Id}),
		case cdv_detail_wx:start_link(Id, Data, Panel, Callback) of
		    {error, _} -> Opened;
		    IW -> [{Id, IW} | Opened]
		end;
	    {_, IW} ->
		wxFrame:raise(IW),
		Opened
	end,
    State#state{detail_wins=NewOpened}.

call(Holder, What) when is_atom(Holder) ->
    call(whereis(Holder), What);
call(Holder, What) when is_pid(Holder) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! What,
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    after 5000 ->
	    io:format("Hanging call ~p~n",[What]),
	    ""
    end;
call(_,_) ->
    "".


%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, State=#state{grid=Grid}) ->
    wxListCtrl:setItemCount(Grid, Count),
    Count > 0 andalso wxListCtrl:refreshItems(Grid, 0, Count-1),
    {noreply, State};

handle_info(active, State) ->
    cdv_wx:set_status(State#state.trunc_warn),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p:~p, Unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, #state{holder=Holder}) ->
    Holder ! stop,
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call(new_dump, _From,
	    #state{grid=Grid,detail_wins=Opened,
		   holder=Holder,callback=Callback}=State) ->
    lists:foreach(fun({_Id, IW}) -> wxFrame:destroy(IW) end, Opened),
    wxListCtrl:deleteAllItems(Grid),
    Ref = erlang:monitor(process,Holder),
    Holder ! stop,
    receive {'DOWN',Ref,_,_,_} -> ok end,
    {NewHolder,TW} = spawn_table_holder(Callback, all),
    {reply, ok, State#state{detail_wins=[],holder=NewHolder,trunc_warn=TW}};

handle_call(Msg, _From, State) ->
    io:format("~p:~p: Unhandled call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast({start_detail_win,Id}, State) ->
    State2 = do_start_detail_win(Id, State),
    {noreply, State2};

handle_cast({detail_win_closed, Id},#state{detail_wins=Opened}=State) ->
    Opened2 = lists:keydelete(Id, 1, Opened),
    {noreply, State#state{detail_wins=Opened2}};

handle_cast(Msg, State) ->
    io:format("~p:~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=MenuId,
		 event=#wxCommand{type = command_menu_selected}},
	     #state{menu_items=MenuItems} = State) ->
    case lists:keyfind(MenuId,1,MenuItems) of
	{MenuId,Type,Id} ->
	    start_detail_win(Id, Type);
	false ->
	    ok
    end,
    {noreply, State};

handle_event(#wx{event=#wxSize{size={W,_}}},
	     #state{grid=Grid}=State) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_right_click,
			       itemIndex=Row}},
	     #state{panel=Panel, holder=Holder, menu_cols=MenuCols} = State) ->
    Menu = wxMenu:new(),
    MenuItems =
	lists:flatmap(
	  fun({Type, Col}) ->
		  MenuId = ?ID_DETAILS + Col,
		  ColText = call(Holder, {get_row, self(), Row, Col}),
		  case ColText of
		      Empty when Empty=="[]"; Empty=="" -> [];
		      _ ->
			  What =
			      case catch list_to_integer(ColText) of
				  NodeId when is_integer(NodeId),
					      Type =:= node ->
				      "node " ++ ColText;
				  _ ->
				      ColText
			      end,
			  Text = "Properties for " ++ What,
			  wxMenu:append(Menu, MenuId, Text),
			  [{MenuId,Type,ColText}]
		  end
	  end,
	  MenuCols),
    case MenuItems of
	[] ->
	    wxMenu:destroy(Menu);
	_ ->
	    wxWindow:popupMenu(Panel, Menu),
	    wxMenu:destroy(Menu)
    end,
    {noreply,State#state{menu_items=MenuItems}};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     #state{holder=Holder}=State) ->
    Holder !  {change_sort, Col},
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated,
			       itemIndex=Row}},
	     #state{holder=Holder, menu_cols=MenuCols} = State) ->
    case MenuCols of
	[{Type, _}|_] ->
	    Id = call(Holder, {get_row, self(), Row, id}),
	    start_detail_win(Id, Type);
	_ ->
	    ignore
    end,
    {noreply, State};

handle_event(Event, State) ->
    io:format("~p:~p: handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%TABLE HOLDER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spawn_table_holder(Callback, Owner) ->
    {Info,TW} = Callback:get_info(Owner),
    Attrs = observer_lib:create_attrs(),
    Parent = self(),
    Holder =
	case Owner of
	    all ->
		Name = list_to_atom(atom_to_list(Callback) ++ "__holder"),
		spawn_link(
		  fun() ->
			  register(Name,self()),
			  init_table_holder(Parent, Attrs, Callback, Info)
		  end),
		Name;
	    _ ->
		spawn_link(
		  fun() ->
			  init_table_holder(Parent, Attrs, Callback, Info)
		  end)
	end,
    {Holder,TW}.

init_table_holder(Parent, Attrs, Callback, InfoList0) ->
    Sort = #sort{sort_key=Callback:col_to_elem(id)},
    {_Sort, InfoList} = do_sort(Sort,InfoList0),
    Info = array:from_list(InfoList),
    NRows = array:size(Info),
    Parent ! {holder_updated, NRows},
    table_holder(#holder{parent=Parent,
			 info=Info,
			 sort=Sort,
			 attrs=Attrs,
			 callback=Callback}).

table_holder(#holder{callback=Callback, attrs=Attrs, info=Info}=S0) ->
    receive
	_M={get_row, From, Row, Col} ->
	    %% erlang:display(_M),
	    State = get_row(From, Row, Col, S0),
	    table_holder(State);
	_M={get_attr, From, Row} ->
	    %% erlang:display(_M),
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
	_M={change_sort, Col} ->
	    %% erlang:display(_M),
	    State = change_sort(Callback:col_to_elem(Col), S0),
	    table_holder(State);
	_M={get_data, From, Id} ->
	    search_id(From, Id, Callback, Info),
	    table_holder(S0);
	stop ->
	    ok;
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.

search_id(From, Id, Callback, Info) ->
    Find = fun(_, RowInfo, _) ->
		   search_id(Callback, RowInfo, Id)
	   end,
    Res = try array:foldl(Find, not_found, Info)
	  catch Data -> Data end,
    From ! {self(), Res},
    ok.

search_id(Callback, RowInfo, Id) ->
    case observer_lib:to_str(get_cell_data(Callback, id, RowInfo)) of
	Id   -> throw(RowInfo);
	_Str -> not_found
    end.

change_sort(Col, S0=#holder{parent=Parent, info=Info0, sort=Sort0}) ->
    NRows = array:size(Info0),
    InfoList0 = array:to_list(Info0),
    {Sort, InfoList}=sort(Col, Sort0, InfoList0),
    Info = array:from_list(InfoList),
    Parent ! {holder_updated, NRows},
    S0#holder{info=Info, last_row=undefined, sort=Sort}.

sort(Col, Opt=#sort{sort_key=Col, sort_incr=Bool}, Table) ->
    do_sort(Opt#sort{sort_incr=not Bool}, Table);
sort(Col, Sort,Table) ->
    do_sort(Sort#sort{sort_key=Col, sort_incr=true}, Table).

do_sort(Sort=#sort{sort_key=Col, sort_incr=true}, Table) ->
    {Sort, lists:keysort(Col, Table)};
do_sort(Sort=#sort{sort_key=Col, sort_incr=false}, Table) ->
    {Sort, lists:reverse(lists:keysort(Col, Table))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cell_data(Callback, ColNo, RowInfo) ->
    case element(Callback:col_to_elem(ColNo), RowInfo) of
	undefined -> "";
	Cell -> try Callback:format(Cell) catch error:undef -> Cell end
    end.

get_row(From, Row, Col,
	#holder{callback=Callback, last_row={Row,RowInfo}}=State) ->
    Data = get_cell_data(Callback, Col, RowInfo),
    From ! {self(), observer_lib:to_str(Data)},
    State;
get_row(From, Row, Col, #holder{callback=Callback, info=Info}=S0) ->
    {Data,State} =
	case Row >= array:size(Info) of
	    true ->
		{"",S0};
	    false ->
		RowInfo = array:get(Row, Info),
		CellData = get_cell_data(Callback, Col, RowInfo),
		{CellData,S0#holder{last_row={Row,RowInfo}}}
	end,
    From ! {self(), observer_lib:to_str(Data)},
    State.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->  Attrs#attrs.even;
		    false -> Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.
