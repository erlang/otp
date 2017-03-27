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
	  panel,
	  node=node(),
	  opt=#opt{},
	  selected,
	  tabs,
	  timer
	}).

start_link(Notebook,  Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

init([Notebook, Parent, Config]) ->
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
                   timer=Config,
                   opt=#opt{type=maps:get(type, Config, ets),
                            sys_hidden=maps:get(sys_hidden, Config, true),
                            unread_hidden=maps:get(unread_hidden, Config, true)}
                  }}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{node=Node, grid=Grid, opt=Opt}) ->
    Tables = get_tables(Node, Opt),
    {Tabs,Sel} = update_grid(Grid, sel(State), Opt, Tables),
    Sel =/= undefined andalso wxListCtrl:ensureVisible(Grid, Sel),
    {noreply, State#state{tabs=Tabs, selected=Sel}};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{node=Node, grid=Grid,
			    opt=Opt0=#opt{sort_key=Key, sort_incr=Bool}}) ->
    Opt = case col2key(Col) of
	      Key -> Opt0#opt{sort_incr=not Bool};
	      NewKey -> Opt0#opt{sort_key=NewKey}
	  end,
    Tables = get_tables(Node, Opt),
    {Tabs,Sel} = update_grid(Grid, sel(State), Opt, Tables),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, tabs=Tabs, selected=Sel}};

handle_event(#wx{id=Id}, State = #state{node=Node, grid=Grid, opt=Opt0})
  when Id >= ?ID_ETS, Id =< ?ID_SYSTEM_TABLES ->
    Opt = case Id of
	      ?ID_ETS -> Opt0#opt{type=ets};
	      ?ID_MNESIA -> Opt0#opt{type=mnesia};
	      ?ID_UNREADABLE -> Opt0#opt{unread_hidden= not Opt0#opt.unread_hidden};
	      ?ID_SYSTEM_TABLES -> Opt0#opt{sys_hidden= not Opt0#opt.sys_hidden}
	  end,
    case get_tables2(Node, Opt) of
	Error = {error, _} ->
            Id =:= ?ID_MNESIA andalso
                wxMenuBar:check(observer_wx:get_menubar(), ?ID_ETS, true),
	    self() ! Error,
	    {noreply, State};
	Tables ->
	    {Tabs, Sel} = update_grid(Grid, sel(State), Opt, Tables),
	    wxWindow:setFocus(Grid),
	    {noreply, State#state{opt=Opt, tabs=Tabs, selected=Sel}}
    end;

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated,
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

handle_event(#wx{event=#wxList{type=command_list_item_right_click}},
	     State=#state{panel=Panel}) ->

    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?ID_TABLE_INFO, "Table info"),
    wxMenu:append(Menu, ?ID_SHOW_TABLE, "Show Table Content"),
    wxWindow:popupMenu(Panel, Menu),
    wxMenu:destroy(Menu),
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
	    display_table_info(Grid, Node, Type, Table),
	    {noreply, State}
    end;

handle_event(#wx{id=?ID_SHOW_TABLE},
	     State=#state{grid=Grid, node=Node, opt=#opt{type=Type}, tabs=Tabs, selected=Sel}) ->
    case Sel of
	undefined ->
	    {noreply, State};
	R when is_integer(R) ->
	    Table = lists:nth(Sel+1, Tabs),
	    case Table#tab.protection of
		private ->
		    self() ! {error, "Table has 'private' protection and can not be read"};
		_ ->
		    observer_tv_table:start_link(Grid, [{node,Node}, {type,Type}, {table,Table}])
	    end,
	    {noreply, State}
    end;

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, timer=Timer0}) ->
    Timer = observer_lib:interval_dialog(Grid, Timer0, 10, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

handle_sync_event(_Event, _Obj, _State) ->
    ok.

handle_call(get_config, _, #state{timer=Timer, opt=Opt}=State) ->
    #opt{type=Type, sys_hidden=Sys, unread_hidden=Unread} = Opt,
    Conf0 = observer_lib:timer_config(Timer),
    Conf = Conf0#{type=>Type, sys_hidden=>Sys, unread_hidden=>Unread},
    {reply, Conf, State};

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).

handle_info(refresh_interval, State = #state{node=Node, grid=Grid, opt=Opt,
                                             tabs=OldTabs}) ->
    case get_tables(Node, Opt) of
        OldTabs ->
            %% no change
            {noreply, State};
        Tables ->
            {Tabs, Sel} = update_grid(Grid, sel(State), Opt, Tables),
            Sel =/= undefined andalso wxListCtrl:ensureVisible(Grid, Sel),
            {noreply, State#state{tabs=Tabs, selected=Sel}}
    end;

handle_info({active, Node}, State = #state{parent=Parent, grid=Grid, opt=Opt0,
					   timer=Timer0}) ->
    {Tables, Opt} = case Opt0#opt.type =:= mnesia andalso get_tables2(Node, Opt0) of
                        Ts when is_list(Ts) ->
                            {Ts, Opt0};
                        _ -> % false or error getting mnesia tables
                            Opt1 = Opt0#opt{type=ets},
                            {get_tables(Node, Opt1), Opt1}
                    end,
    {Tabs,Sel} = update_grid(Grid, sel(State), Opt, Tables),
    wxWindow:setFocus(Grid),
    create_menus(Parent, Opt),
    Timer = observer_lib:start_timer(Timer0, 10),
    {noreply, State#state{node=Node, tabs=Tabs, timer=Timer, opt=Opt, selected=Sel}};

handle_info(not_active, State = #state{timer = Timer0}) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info({error, Error}, #state{panel=Panel,opt=Opt}=State) ->
    Str = io_lib:format("ERROR: ~s~n",[Error]),
    observer_lib:display_info_dialog(Panel,Str),
    case Opt#opt.type of
        mnesia -> wxMenuBar:check(observer_wx:get_menubar(), ?ID_ETS, true);
        _ -> ok
    end,
    {noreply, State#state{opt=Opt#opt{type=ets}}};

handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Event, _State) ->
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

get_tables(Node, Opts) ->
    case get_tables2(Node, Opts) of
	Error = {error, _} ->
	    self() ! Error,
	    [];
	Res ->
	    Res
    end.
get_tables2(Node, #opt{type=Type, sys_hidden=Sys, unread_hidden=Unread}) ->
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

update_grid(Grid, Selected, Opt, Tables) ->
    wx:batch(fun() -> update_grid2(Grid, Selected, Opt, Tables) end).

update_grid2(Grid, {SelName,SelId}, #opt{sort_key=Sort,sort_incr=Dir}, Tables) ->
    wxListCtrl:deleteAllItems(Grid),
    Update =
	fun(#tab{name = Name, id = Id, owner = Owner, size = Size, memory = Memory,
		 protection = Protection, reg_name = RegName},
            {Row, Sel}) ->
		_Item = wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, ?BG_EVEN);
		   true -> ignore
		end,
		if Protection == private ->
			wxListCtrl:setItemTextColour(Grid, Row, {200,130,50});
		   true -> ignore
		end,

		lists:foreach(fun({_, ignore}) -> ignore;
				 ({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col, observer_lib:to_str(Val))
			      end,
			      [{0,Name}, {1,Size}, {2, Memory div 1024},
			       {3,Owner}, {4,RegName}, {5,Id}]),
                if SelName =:= Name, SelId =:= Id ->
                        wxListCtrl:setItemState(Grid, Row, 16#FFFF, ?wxLIST_STATE_SELECTED),
                        {Row+1, Row};
                   true ->
                        wxListCtrl:setItemState(Grid, Row, 0, ?wxLIST_STATE_SELECTED),
                        {Row+1, Sel}
                end
	end,
    ProcInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Tables));
		   true -> lists:keysort(Sort, Tables)
	       end,
    {_, Sel} = lists:foldl(Update, {0, undefined}, ProcInfo),
    {ProcInfo, Sel}.

sel(#state{selected=Sel, tabs=Tabs}) ->
    try lists:nth(Sel+1, Tabs) of
        #tab{name=Name, id=Id} -> {Name, Id}
    catch _:_ ->
            {undefined, undefined}
    end.
