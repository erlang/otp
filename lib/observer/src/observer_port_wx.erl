%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2014. All Rights Reserved.
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
-module(observer_port_wx).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(GRID, 300).
-define(ID_REFRESH, 301).
-define(ID_REFRESH_INTERVAL, 302).
-define(ID_PORT_INFO, 303).
-define(ID_PORT_INFO_SELECTED, 304).
-define(ID_TRACE_PORTS, 305).
-define(ID_TRACE_NAMES, 306).
-define(ID_TRACE_NEW, 307).
-define(ID_TRACE_ALL, 308).
-define(ID_CLOSE_PORT, 309).

-define(TRACE_PORTS_STR, "Trace selected ports").
-define(TRACE_NAMES_STR, "Trace selected ports, "
	"if a process have a registered name "
	"processes with same name will be traced on all nodes").

-record(port,
	{id,
	 connected,
	 name,
	 controls,
	 slot,
	 id_str,
	 links,
	 monitors}).

-record(opt, {sort_key=2,
	      sort_incr=true
	     }).

-record(state,
	{
	  parent,
	  grid,
	  panel,
	  node=node(),
	  opt=#opt{},
	  right_clicked_port,
	  ports,
	  timer,
	  open_wins=[]
	}).

start_link(Notebook,  Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

init([Notebook, Parent]) ->
    Panel = wxPanel:new(Notebook),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Style = ?wxLC_REPORT bor ?wxLC_HRULES,
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
    ListItems = [{"Id", ?wxLIST_FORMAT_LEFT,  150},
		 {"Connected", ?wxLIST_FORMAT_LEFT, 150},
		 {"Name", ?wxLIST_FORMAT_LEFT, 150},
		 {"Controls", ?wxLIST_FORMAT_LEFT, 200},
		 {"Slot", ?wxLIST_FORMAT_RIGHT, 50}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_right_click),
    wxListCtrl:connect(Grid, command_list_item_activated),
    wxListCtrl:connect(Grid, command_list_col_click),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    {Panel, #state{grid=Grid, parent=Parent, panel=Panel, timer={false, 10}}}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{node=Node, grid=Grid, opt=Opt}) ->
    Ports0 = get_ports(Node),
    Ports = update_grid(Grid, Opt, Ports0),
    {noreply, State#state{ports=Ports}};

handle_event(#wx{obj=Obj, event=#wxClose{}}, #state{open_wins=Opened} = State) ->
    NewOpened =
	case lists:keytake(Obj,2,Opened) of
	    false -> Opened;
	    {value,_,Rest} -> Rest
	end,
    {noreply, State#state{open_wins=NewOpened}};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     State = #state{node=Node, grid=Grid,
			    opt=Opt0=#opt{sort_key=Key, sort_incr=Bool}}) ->
    Opt = case Col+2 of
	      Key -> Opt0#opt{sort_incr=not Bool};
	      NewKey -> Opt0#opt{sort_key=NewKey}
	  end,
    Ports0 = get_ports(Node),
    Ports = update_grid(Grid, Opt, Ports0),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt=Opt, ports=Ports}};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated,
			       itemIndex=Index}},
	     State=#state{grid=Grid, ports=Ports, open_wins=Opened}) ->
    Port = lists:nth(Index+1, Ports),
    NewOpened = display_port_info(Grid, Port, Opened),
    {noreply, State#state{open_wins=NewOpened}};

handle_event(#wx{event=#wxList{type=command_list_item_right_click,
			       itemIndex=Index}},
	     State=#state{panel=Panel, ports=Ports}) ->
    case Index of
	-1 ->
	    {noreply, State};
	_ ->
	    Port = lists:nth(Index+1, Ports),
	    Menu = wxMenu:new(),
	    wxMenu:append(Menu, ?ID_PORT_INFO,
			  "Port info for " ++ erlang:port_to_list(Port#port.id)),
	    wxMenu:append(Menu, ?ID_TRACE_PORTS,
			  "Trace selected ports",
			  [{help, ?TRACE_PORTS_STR}]),
	    wxMenu:append(Menu, ?ID_TRACE_NAMES,
			  "Trace selected ports by name (all nodes)",
			  [{help, ?TRACE_NAMES_STR}]),
	    wxMenu:append(Menu, ?ID_CLOSE_PORT,
			  "Close " ++ erlang:port_to_list(Port#port.id)),
	    wxWindow:popupMenu(Panel, Menu),
	    wxMenu:destroy(Menu),
	    {noreply, State#state{right_clicked_port=Port}}
    end;

handle_event(#wx{id=?ID_PORT_INFO},
	     State = #state{grid=Grid, right_clicked_port=Port,
			    open_wins=Opened}) ->
    case Port of
	undefined ->
	    {noreply, State};
	_ ->
	    NewOpened = display_port_info(Grid, Port, Opened),
	    {noreply, State#state{right_clicked_port=undefined,
				  open_wins=NewOpened}}
    end;

handle_event(#wx{id=?ID_PORT_INFO_SELECTED},
	     State = #state{grid=Grid, ports=Ports, open_wins=Opened}) ->
    case get_selected_items(Grid,Ports) of
	[] ->
	    observer_wx:create_txt_dialog(State#state.panel, "No selected ports",
					  "Port Info", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Selected ->
	    NewOpened = lists:foldl(fun(P,O) -> display_port_info(Grid, P, O) end,
				    Opened, Selected),
	    {noreply, State#state{open_wins = NewOpened}}
    end;

handle_event(#wx{id=?ID_CLOSE_PORT}, State = #state{right_clicked_port=Port}) ->
    case Port of
	undefined ->
	    {noreply, State};
	_ ->
	    erlang:port_close(Port#port.id),
	    {noreply, State#state{right_clicked_port=undefined}}
	end;

handle_event(#wx{id=?ID_TRACE_PORTS}, #state{grid=Grid, ports=Ports}=State)  ->
    case get_selected_items(Grid, Ports) of
	[] ->
	    observer_wx:create_txt_dialog(State#state.panel, "No selected ports",
					  "Tracer", ?wxICON_EXCLAMATION);
	Selected ->
	    SelectedIds = [Port#port.id || Port <- Selected],
	    observer_trace_wx:add_ports(SelectedIds)
    end,
    {noreply,  State};

handle_event(#wx{id=?ID_TRACE_NAMES}, #state{grid=Grid, ports=Ports}=State)  ->
    case get_selected_items(Grid, Ports) of
	[] ->
	    observer_wx:create_txt_dialog(State#state.panel, "No selected ports",
					  "Tracer", ?wxICON_EXCLAMATION);
	Selected ->
	    IdsOrRegs =
		[case Port#port.name of
		     [] -> Port#port.id;
		     Name -> Name
		 end || Port <- Selected],
	    observer_trace_wx:add_ports(IdsOrRegs)
    end,
    {noreply,  State};

handle_event(#wx{id=?ID_TRACE_NEW, event=#wxCommand{type=command_menu_selected}}, State) ->
    observer_trace_wx:add_ports([new_ports]),
    {noreply,  State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, timer=Timer0}) ->
    Timer = observer_lib:interval_dialog(Grid, Timer0, 10, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(#wx{event=#wxMouse{type=left_down}, userData=TargetPid}, State) ->
    observer ! {open_link, TargetPid},
    {noreply, State};

handle_event(#wx{obj=Obj, event=#wxMouse{type=enter_window}}, State) ->
    wxTextCtrl:setForegroundColour(Obj,{0,0,100,255}),
    {noreply, State};

handle_event(#wx{obj=Obj, event=#wxMouse{type=leave_window}}, State) ->
    wxTextCtrl:setForegroundColour(Obj,?wxBLUE),
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

handle_sync_event(_Event, _Obj, _State) ->
    ok.

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).

handle_info({portinfo_open, PortIdStr},
	    State = #state{grid=Grid, ports=Ports, open_wins=Opened}) ->
    Port = lists:keyfind(PortIdStr,#port.id_str,Ports),
    NewOpened = display_port_info(Grid, Port, Opened),
    {noreply, State#state{open_wins = NewOpened}};

handle_info(refresh_interval, State = #state{node=Node, grid=Grid, opt=Opt,
                                             ports=OldPorts}) ->
    case get_ports(Node) of
        OldPorts ->
            %% no change
            {noreply, State};
        Ports0 ->
            Ports = update_grid(Grid, Opt, Ports0),
            {noreply, State#state{ports=Ports}}
    end;

handle_info({active, Node}, State = #state{parent=Parent, grid=Grid, opt=Opt,
					   timer=Timer0}) ->
    Ports0 = get_ports(Node),
    Ports = update_grid(Grid, Opt, Ports0),
    wxWindow:setFocus(Grid),
    create_menus(Parent),
    Timer = observer_lib:start_timer(Timer0),
    {noreply, State#state{node=Node, ports=Ports, timer=Timer}};

handle_info(not_active, State = #state{timer = Timer0}) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info({error, Error}, State) ->
    handle_error(Error),
    {noreply, State};

handle_info(_Event, State) ->
    {noreply, State}.

terminate(_Event, _State) ->
    ok.

code_change(_, _, State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent) ->
    MenuEntries =
	[{"View",
	  [#create_menu{id = ?ID_PORT_INFO_SELECTED,
			text = "Port info for selected ports\tCtrl-I"},
	   separator,
	   #create_menu{id = ?ID_REFRESH, text = "Refresh\tCtrl-R"},
	   #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh Interval..."}
	  ]},
	 {"Trace",
	  [#create_menu{id=?ID_TRACE_PORTS, text="Trace selected ports"},
	   #create_menu{id=?ID_TRACE_NAMES, text="Trace selected ports by name (all nodes)"},
	   #create_menu{id=?ID_TRACE_NEW, text="Trace new ports"}
	  ]}
	],
    observer_wx:create_menus(Parent, MenuEntries).

get_ports(Node) ->
    case get_ports2(Node) of
	Error = {error, _} ->
	    self() ! Error,
	    [];
	Res ->
	    Res
    end.
get_ports2(Node) ->
    case rpc:call(Node, observer_backend, get_port_list, []) of
	{badrpc, Error} ->
	    {error, Error};
	Error = {error, _} ->
	    Error;
	Result ->
	    [list_to_portrec(Port) || Port <- Result]
    end.

list_to_portrec(PL) ->
    %% PortInfo:
    %% {registered_name, RegisteredName :: atom()} |
    %% {id, Index :: integer() >= 0} |
    %% {connected, Pid :: pid()} |
    %% {links, Pids :: [pid()]} |
    %% {name, String :: string()} |
    %% {input, Bytes :: integer() >= 0} |
    %% {output, Bytes :: integer() >= 0} |
    %% {os_pid, OsPid :: integer() >= 0 | undefined},
    PortId = proplists:get_value(port_id, PL),
    #port{id = PortId,
	  id_str = erlang:port_to_list(PortId),
	  slot = proplists:get_value(id, PL),
	  connected = proplists:get_value(connected, PL),
	  links = proplists:get_value(links, PL, []),
	  name = proplists:get_value(registered_name, PL, []),
	  monitors = proplists:get_value(monitors, PL, []),
	  controls = proplists:get_value(name, PL)}.

portrec_to_list(#port{id = Id,
		      slot = Slot,
		      connected = Connected,
		      links = Links,
		      name = Name,
		      monitors = Monitors,
		      controls = Controls}) ->
    [{id,Id},
     {slot,Slot},
     {connected,Connected},
     {links,Links},
     {name,Name},
     {monitors,Monitors},
     {controls,Controls}].

display_port_info(Parent, PortRec, Opened) ->
    PortIdStr = PortRec#port.id_str,
    case lists:keyfind(PortIdStr,1,Opened) of
	false ->
	    Frame = do_display_port_info(Parent, PortRec),
	    [{PortIdStr,Frame}|Opened];
	{_,Win} ->
	    wxFrame:raise(Win),
	    Opened
    end.

do_display_port_info(Parent0, PortRec) ->
    Parent = observer_lib:get_wx_parent(Parent0),
    Title = "Port Info: " ++ PortRec#port.id_str,
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title,
			    [{style, ?wxSYSTEM_MENU bor ?wxCAPTION
				  bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER}]),

    Port = portrec_to_list(PortRec),
    Fields0 = port_info_fields(Port),
    {_FPanel, _Sizer, _UpFields} = observer_lib:display_info(Frame, Fields0),
    wxFrame:center(Frame),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:show(Frame),
    Frame.


port_info_fields(Port) ->
    Struct =
	[{"Overview",
	  [{"Name",             name},
	   {"Connected",        {click,connected}},
	   {"Slot",             slot},
	   {"Controls",         controls}]},
	 {scroll_boxes,
	  [{"Links",1,{click,links}},
	   {"Monitors",1,{click,filter_monitor_info()}}]}],
    observer_lib:fill_info(Struct, Port).

filter_monitor_info() ->
    fun(Data) ->
	    Ms = proplists:get_value(monitors, Data),
	    [Pid || {process, Pid} <- Ms]
    end.


handle_error(Foo) ->
    Str = io_lib:format("ERROR: ~s~n",[Foo]),
    observer_lib:display_info_dialog(Str).

update_grid(Grid, Opt, Ports) ->
    wx:batch(fun() -> update_grid2(Grid, Opt, Ports) end).
update_grid2(Grid, #opt{sort_key=Sort,sort_incr=Dir}, Ports) ->
    wxListCtrl:deleteAllItems(Grid),
    Update =
	fun(#port{id = Id,
		  slot = Slot,
		  connected = Connected,
		  name = Name,
		  controls = Ctrl},
	    Row) ->
		_Item = wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 0 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, ?BG_EVEN);
		   true -> ignore
		end,

		lists:foreach(fun({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col,
							 observer_lib:to_str(Val))
			      end,
			      [{0,Id},{1,Connected},{2,Name},{3,Ctrl},{4,Slot}]),
		Row + 1
	end,
    PortInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Ports));
		   true -> lists:keysort(Sort, Ports)
	       end,
    lists:foldl(Update, 0, PortInfo),
    PortInfo.


get_selected_items(Grid, Data) ->
    get_indecies(get_selected_items(Grid, -1, []), Data).
get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index, [{geometry, ?wxLIST_NEXT_ALL},
						{state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item | ItemAcc])
    end.

get_indecies(Items, Data) ->
    get_indecies(Items, 0, Data).
get_indecies([I|Rest], I, [H|T]) ->
    [H|get_indecies(Rest, I+1, T)];
get_indecies(Rest = [_|_], I, [_|T]) ->
    get_indecies(Rest, I+1, T);
get_indecies(_, _, _) ->
    [].
