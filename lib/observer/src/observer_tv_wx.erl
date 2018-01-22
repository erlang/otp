%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
-module(observer_tv_wx).

-export([start_link/3, display_table_info/4]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

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
-define(ID_SHOW_TABLE, 408).

-record(opts, {type=ets,
               sys_hidden=true,
               unread_hidden=true}).

-record(sort, {sort_incr=true,
               sort_key=2}).

-record(state,
	{
	  parent,
	  grid,
	  panel,
	  node=node(),
	  opts=#opts{},
          holder,
	  selected,
	  timer
	}).

start_link(Notebook,  Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

init([Notebook, Parent, Config]) ->
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    Opts=#opts{type=maps:get(type, Config, ets),
               sys_hidden=maps:get(sys_hidden, Config, true),
               unread_hidden=maps:get(unread_hidden, Config, true)},

    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
    Self = self(),
    Attrs = observer_lib:create_attrs(),
    Holder = spawn_link(fun() -> init_table_holder(Self, Attrs) end),
    CBs = [{onGetItemText, fun(_, Item,Col) -> get_row(Holder, Item, Col) end},
           {onGetItemAttr, fun(_, Item) -> get_attr(Holder, Item) end}],
    Grid = wxListCtrl:new(Panel, [{winid, ?GRID}, {style, Style} | CBs]),
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
		 {"Objects",    ?wxLIST_FORMAT_RIGHT, 100},
		 {"Size (kB)",  ?wxLIST_FORMAT_RIGHT, 100},
		 {"Owner Pid",  ?wxLIST_FORMAT_CENTER, 150},
		 {"Owner Name", ?wxLIST_FORMAT_LEFT,  200},
		 {"Table Id",   ?wxLIST_FORMAT_LEFT, 250}
		],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_activated),
    wxListCtrl:connect(Grid, command_list_item_right_click),
    wxListCtrl:connect(Grid, command_list_item_selected),
    wxListCtrl:connect(Grid, command_list_col_click),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    {Panel, #state{grid=Grid, parent=Parent, panel=Panel,
                   opts=Opts, timer=Config, holder=Holder}}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{holder=Holder, node=Node, opts=Opts}) ->
    Tables = get_tables(Node, Opts),
    Holder ! {refresh, Tables},
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{holder=Holder}) ->
    Holder ! {sort, Col},
    {noreply, State};

handle_event(#wx{id=Id}, State = #state{node=Node, holder=Holder, grid=Grid, opts=Opt0})
  when Id >= ?ID_ETS, Id =< ?ID_SYSTEM_TABLES ->
    Opt = case Id of
	      ?ID_ETS -> Opt0#opts{type=ets};
	      ?ID_MNESIA -> Opt0#opts{type=mnesia};
	      ?ID_UNREADABLE -> Opt0#opts{unread_hidden= not Opt0#opts.unread_hidden};
	      ?ID_SYSTEM_TABLES -> Opt0#opts{sys_hidden= not Opt0#opts.sys_hidden}
	  end,
    case get_tables2(Node, Opt) of
	Error = {error, _} ->
            Id =:= ?ID_MNESIA andalso
                wxMenuBar:check(observer_wx:get_menubar(), ?ID_ETS, true),
	    self() ! Error,
	    {noreply, State};
	Tables ->
	    Holder ! {refresh, Tables},
	    wxWindow:setFocus(Grid),
	    {noreply, State#state{opts=Opt}}
    end;

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated,
			       itemIndex=Index}},
	     State=#state{holder=Holder, node=Node, opts=#opts{type=Type}, grid=Grid}) ->
    case get_table(Holder, Index) of
        #tab{protection=private} ->
            self() ! {error, "Table has 'private' protection and can not be read"};
        #tab{}=Table ->
	    observer_tv_table:start_link(Grid, [{node,Node}, {type,Type}, {table,Table}]);
        _ -> ignore
    end,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_right_click}},
	     State=#state{panel=Panel}) ->
    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?ID_TABLE_INFO, "Table info"),
    wxMenu:append(Menu, ?ID_SHOW_TABLE, "Show Table Content"),
    wxWindow:popupMenu(Panel, Menu),
    wxMenu:destroy(Menu),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Index}},
	     State=#state{holder=Holder}) ->
    Holder ! {selected, Index},
    {noreply, State#state{selected=Index}};

handle_event(#wx{id=?ID_TABLE_INFO},
	     State = #state{holder=Holder, grid=Grid, node=Node, opts=#opts{type=Type}, selected=Sel}) ->
    case Sel of
	undefined ->
	    {noreply, State};
	R when is_integer(R) ->
	    Table = get_table(Holder, Sel),
	    display_table_info(Grid, Node, Type, Table),
	    {noreply, State}
    end;

handle_event(#wx{id=?ID_SHOW_TABLE},
	     State=#state{holder=Holder, grid=Grid, node=Node, opts=#opts{type=Type}, selected=Sel}) ->
    case Sel of
	undefined ->
	    {noreply, State};
	R when is_integer(R) ->
	    case get_table(Holder, R) of
		#tab{protection=private} ->
		    self() ! {error, "Table has 'private' protection and can not be read"};
                #tab{}=Table ->
		    observer_tv_table:start_link(Grid, [{node,Node}, {type,Type}, {table,Table}]);
                _ -> ignore
	    end,
	    {noreply, State}
    end;

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, timer=Timer0}) ->
    Timer = observer_lib:interval_dialog(Grid, Timer0, 10, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(_Event, State) ->
    {noreply, State}.

handle_sync_event(_Event, _Obj, _State) ->
    ok.

handle_call(get_config, _, #state{timer=Timer, opts=Opt}=State) ->
    #opts{type=Type, sys_hidden=Sys, unread_hidden=Unread} = Opt,
    Conf0 = observer_lib:timer_config(Timer),
    Conf = Conf0#{type=>Type, sys_hidden=>Sys, unread_hidden=>Unread},
    {reply, Conf, State};

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).

handle_info(refresh_interval, State = #state{holder=Holder, node=Node, opts=Opt}) ->
    Tables = get_tables(Node, Opt),
    Holder ! {refresh, Tables},
    {noreply, State};

handle_info({active, Node}, State = #state{parent=Parent, holder=Holder, grid=Grid,
                                           opts=Opt0, timer=Timer0}) ->
    {Tables, Opt} = case Opt0#opts.type =:= mnesia andalso get_tables2(Node, Opt0) of
                        Ts when is_list(Ts) ->
                            {Ts, Opt0};
                        _ -> % false or error getting mnesia tables
                            Opt1 = Opt0#opts{type=ets},
                            {get_tables(Node, Opt1), Opt1}
                    end,
    Holder ! {refresh, Tables},
    wxWindow:setFocus(Grid),
    create_menus(Parent, Opt),
    Timer = observer_lib:start_timer(Timer0, 10),
    {noreply, State#state{node=Node, timer=Timer, opts=Opt}};

handle_info(not_active, State = #state{timer = Timer0}) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info({error, Error}, #state{panel=Panel,opts=Opt}=State) ->
    Str = io_lib:format("ERROR: ~ts~n",[Error]),
    observer_lib:display_info_dialog(Panel,Str),
    case Opt#opts.type of
        mnesia -> wxMenuBar:check(observer_wx:get_menubar(), ?ID_ETS, true);
        _ -> ok
    end,
    {noreply, State#state{opts=Opt#opts{type=ets}}};

handle_info({refresh, Min, Min}, State = #state{grid=Grid}) ->
    wxListCtrl:setItemCount(Grid, Min+1),
    wxListCtrl:refreshItem(Grid, Min), %% Avoid assert in wx below if Max is 0
    observer_wx:set_status(io_lib:format("Tables: ~w", [Min+1])),
    {noreply, State};
handle_info({refresh, Min, Max}, State = #state{grid=Grid}) ->
    wxListCtrl:setItemCount(Grid, Max+1),
    Max > 0 andalso wxListCtrl:refreshItems(Grid, Min, Max),
    observer_wx:set_status(io_lib:format("Tables: ~w", [Max+1])),
    {noreply, State};

handle_info({selected, New, Size}, #state{grid=Grid, selected=Old} = State) ->
    if
        is_integer(Old), Old < Size ->
            wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_SELECTED);
        true -> ignore
    end,
    if is_integer(New) ->
            wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_SELECTED),
            wxListCtrl:ensureVisible(Grid, New);
       true -> ignore
    end,
    {noreply, State#state{selected=New}};

handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Event, #state{holder=Holder}) ->
    Holder ! stop,
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent, #opts{sys_hidden=Sys, unread_hidden=UnR, type=Type}) ->
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

get_tables(Node, Opts) ->
    case get_tables2(Node, Opts) of
	Error = {error, _} ->
	    self() ! Error,
	    [];
	Res ->
	    Res
    end.
get_tables2(Node, #opts{type=Type, sys_hidden=Sys, unread_hidden=Unread}) ->
    Args = [Type, [{sys_hidden,Sys}, {unread_hidden,Unread}]],
    case rpc:call(Node, observer_backend, get_table_list, Args) of
	{badrpc, Error} ->
	    {error, Error};
	Error = {error, _} ->
	    Error;
	Result ->
	    [list_to_tabrec(Tab) || Tab <- Result]
    end.

col2key(0) -> #tab.name;
col2key(1) -> #tab.size;
col2key(2) -> #tab.memory;
col2key(3) -> #tab.owner;
col2key(4) -> #tab.reg_name;
col2key(5) -> #tab.id.

list_to_tabrec(PL) ->
    #tab{name = proplists:get_value(name, PL),
	 id = proplists:get_value(id, PL, ignore),
	 size = proplists:get_value(size, PL, 0),
	 memory= proplists:get_value(memory, PL, 0),   %% In bytes
	 owner=proplists:get_value(owner, PL),
	 reg_name=proplists:get_value(reg_name, PL),
	 protection = proplists:get_value(protection, PL, public),
	 type=proplists:get_value(type, PL, set),
	 keypos=proplists:get_value(keypos, PL, 1),
	 heir=proplists:get_value(heir, PL, none),
	 compressed=proplists:get_value(compressed, PL, false),
	 fixed=proplists:get_value(fixed, PL, false),
	 %% Mnesia Info
	 storage =proplists:get_value(storage, PL),
	 index = proplists:get_value(index, PL)}.

display_table_info(Parent0, Node, Source, Table) ->
    Parent = observer_lib:get_wx_parent(Parent0),
    Title = "Table Info: " ++ atom_to_list(Table#tab.name),
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title,
			    [{style, ?wxSYSTEM_MENU bor ?wxCAPTION
				  bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER}]),

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
	       {"Memory allocated",  {bytes, Table#tab.memory}},
	       {"Compressed",        Table#tab.compressed}]},

    {_, Sizer, _} = observer_lib:display_info(Frame, [IdInfo,Settings,Memory]),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:setMinSize(Frame, {300, -1}),
    wxFrame:center(Frame),
    wxFrame:show(Frame).

list_to_strings([]) -> "None";
list_to_strings([A]) -> integer_to_list(A);
list_to_strings([A|B]) ->
    integer_to_list(A) ++ " ," ++ list_to_strings(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Table holder needs to be in a separate process otherwise
%%  the callback get_row/3 may deadlock if the process do
%%  wx calls when callback is invoked.

get_table(Table, Item) ->
    get_row(Table, Item, all).

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

-record(holder, {node, parent, pid,
		 tabs=array:new(),
                 sort=#sort{},
		 attrs,
                 sel
		}).

init_table_holder(Parent, Attrs) ->
    Parent ! refresh,
    table_holder(#holder{node=node(), parent=Parent, attrs=Attrs}).

table_holder(S0 = #holder{parent=Parent, tabs=Tabs0, sel=Sel0}) ->
    receive
	{get_attr, From, Row} ->
	    get_attr(From, Row, S0),
	    table_holder(S0);
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Tabs0),
	    table_holder(S0);
	{sort, Col} ->
            STab = get_sel(Sel0, Tabs0),
	    Parent ! {refresh, 0, array:size(Tabs0)-1},
            S1 = sort(col2key(Col), S0),
            Sel = sel_idx(STab, S1#holder.tabs),
            Parent ! {selected, Sel, array:size(Tabs0)},
	    table_holder(S1#holder{sel=Sel});
	{refresh, Tabs1} ->
            STab = get_sel(Sel0, Tabs0),
            Tabs = case S0#holder.sort of
                       #sort{sort_incr=false, sort_key=Col} ->
                           array:from_list(lists:reverse(lists:keysort(Col, Tabs1)));
                       #sort{sort_key=Col} ->
                           array:from_list(lists:keysort(Col, Tabs1))
                   end,
	    Parent ! {refresh, 0, array:size(Tabs)-1},
            Sel = sel_idx(STab, Tabs),
            Parent ! {selected, Sel,array:size(Tabs)},
	    table_holder(S0#holder{tabs=Tabs, sel=Sel});
        {selected, Sel} ->
            table_holder(S0#holder{sel=Sel});
	stop ->
	    ok;
	What ->
	    io:format("Table holder got ~tp~n",[What]),
	    Parent ! {refresh, 0, array:size(Tabs0)-1},
	    table_holder(S0)
    end.

get_sel(undefined, _Tabs) ->
    undefined;
get_sel(Idx, Tabs) ->
    array:get(Idx, Tabs).

sel_idx(undefined, _Tabs) ->
    undefined;
sel_idx(Tab, Tabs) ->
    Find = fun(Idx, C, Acc) -> C =:= Tab andalso throw({found, Idx}), Acc end,
    try array:foldl(Find, undefined, Tabs)
    catch {found, Idx} -> Idx
    end.

sort(Col, #holder{sort=#sort{sort_key=Col, sort_incr=Incr}=S, tabs=Table0}=H) ->
    Table = lists:reverse(array:to_list(Table0)),
    H#holder{sort=S#sort{sort_incr=(not Incr)},
             tabs=array:from_list(Table)};
sort(Col, #holder{sort=#sort{sort_incr=Incr}=S, tabs=Table0}=H) ->
    Table = case Incr of
                false -> lists:reverse(lists:keysort(Col, array:to_list(Table0)));
                true -> lists:keysort(Col, array:to_list(Table0))
            end,
    H#holder{sort=S#sort{sort_key=Col},
             tabs=array:from_list(Table)}.

get_row(From, Row, Col, Table) ->
    Object = array:get(Row, Table),
    From ! {self(), get_col(Col, Object)}.

get_col(all, Rec) ->
    Rec;
get_col(2, #tab{}=Rec) -> %% Memory in kB
    observer_lib:to_str(element(#tab.memory, Rec) div 1024);
get_col(Col, #tab{}=Rec) ->
    case element(col2key(Col), Rec) of
        ignore -> "";
        Val -> observer_lib:to_str(Val)
    end;
get_col(_, _) ->
    "".

get_attr(From, Row, #holder{tabs=Tabs, attrs=Attrs}) ->
    EvenOdd = case (Row rem 2) > 0 of
                  true -> Attrs#attrs.odd;
                  false -> Attrs#attrs.even
              end,
    What = try array:get(Row, Tabs) of
               #tab{protection=private} ->
                   Attrs#attrs.deleted;
               _ ->
                   EvenOdd
           catch _ ->
                   EvenOdd
           end,
    From ! {self(), What}.
