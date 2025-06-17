%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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
-module(observer_sock_wx).
-moduledoc false.

%% {ok, S1} = socket:open(inet,  stream,    tcp).
%% {ok, S2} = socket:open(inet6, stream,    tcp).
%% {ok, S3} = socket:open(inet,  dgram,     udp).
%% {ok, S4} = socket:open(inet6, dgram,     udp).
%% {ok, S5} = socket:open(inet,  seqpacket, sctp).
%% {ok, S6} = socket:open(inet6, seqpacket, sctp).
%% {ok, S7} = socket:open(local, stream,    default).
%% {ok, S8} = socket:open(local, dgram,     default).
%% {ok, S9} = gen_tcp:listen(0, [{inet_backend, socket}]).
%% ok = socket:bind(S7, #{family => local, path => "/tmp/foobarA"}).
%% ok = socket:bind(S8, #{family => local, path => "/tmp/foobarB"}).

-export([start_link/3]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(GRID, 300).
-define(ID_REFRESH, 301).
-define(ID_REFRESH_INTERVAL, 302).
-define(ID_SOCKET_INFO, 303).
-define(ID_SOCKET_INFO_SELECTED, 304).
%% -define(ID_DEBUG_SOCKETS, 305).
%% -define(ID_DEBUG_NAMES, 306).
%% -define(ID_DEBUG_NEW, 307).
%% -define(ID_DEBUG_ALL, 308).
-define(ID_CLOSE_SOCKET, 309).

-define(DEBUG_SOCKETS_STR, "Debug selected sockets").

-record(socket,
	{id,
	 id_str,
	 kind,
	 fd,
	 owner,
	 domain,
	 type,
	 protocol,
	 raddress,
	 laddress,
	 rstate,
         wstate,
	 monitored_by,
	 statistics,
	 options}).

-record(opt, {sort_key  = 2,
	      sort_incr = true,
              odd_bg
	     }).

-record(state,
	{
	  parent,
	  grid,
	  panel,
	  sizer,
	  fields,
	  node = {node(), true},
	  opt  = #opt{},
	  right_clicked_socket,
	  sockets,
	  timer,
	  open_wins=[]
	}).

start_link(Notebook,  Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

info_fields() ->
    Gen = [{"General socket info",
	    [{"IOV Max",                                 iov_max},
	     {"Counter Size (in bits)",                  num_cnt_bits},
	     {"Number of sockets",                       num_sockets},
	     {"Number of (socket) monitors",             num_monitors},
	     {"Number of sockets in the 'inet' domain",  num_dinet},
	     {"Number of sockets in the 'inet6' domain", num_dinet6},
	     {"Number of sockets in the 'local' domain", num_dlocal},
	     {"Number of type 'stream' sockets",         num_tstreams},
	     {"Number of type 'dgram' sockets",          num_tdgrams},
	     {"Number of type 'seqpacket' sockets",      num_tseqpkgs},
	     {"Number of protocol 'ip' sockets",         num_pip},
	     {"Number of protocol 'sctp' sockets",       num_psctp},
	     {"Number of protocol 'tcp' sockets",        num_ptcp},
	     {"Number of protocol 'udp' sockets",        num_pudp}
	    ]}],
    Gen.

update_gen_socket_info(#state{node   = {Node, true},
			      fields = Fields,
			      sizer  = Sizer}) ->
    case rpc:call(Node, observer_backend, socket_info, []) of
	Info when is_list(Info) ->
	    Gen = info_fields(),
	    observer_lib:update_info(Fields,
	    			     observer_lib:fill_info(Gen, Info,
	    						    "Not Supported")),
	    wxSizer:layout(Sizer);
	_ ->
	    ignore
    end;
update_gen_socket_info(#state{node = _}) ->
    ignore.


%% Two parts of this panel:
%% 1) First part is general socket info (basically: socket:info/0)
%% 2) Second part is a list (grid) och each socket and info about it
init([Notebook, Parent, Config]) ->
    %% put(debug, true),
    try
	begin
	    do_init(Notebook, Parent, Config, observer_backend:socket_info())
	end
    catch
	_C:_E:_S ->
	    %% Current node does not support socket (windows?)
	    do_init(Notebook, Parent, Config, [])
    end.

do_init(Notebook, Parent, Config, Info) ->
    Gen    = info_fields(),
    Panel  = wxPanel:new(Notebook),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    GenSizer = wxBoxSizer:new(?wxHORIZONTAL),
    {GenPanel, _GenSizer, GenFields} =
	observer_lib:display_info(Panel, 
				  observer_lib:fill_info(Gen, Info,
							 "Not Supported")),
    wxSizer:add(GenSizer, GenPanel,
		[{flag, ?wxEXPAND}, {proportion, 1}]),
    BorderFlags = ?wxLEFT bor ?wxRIGHT,
    wxSizer:add(Sizer, GenSizer,
		[{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
		 {proportion, 0}, {border, 5}]),
    Style  = ?wxLC_REPORT bor ?wxLC_HRULES,
    Grid   = wxListCtrl:new(Panel, [{winid, ?GRID}, {style, Style}]),
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
    Scale = observer_wx:get_scale(),
    ListItems = [{"Id",          ?wxLIST_FORMAT_LEFT, Scale*350},
		 {"Owner",       ?wxLIST_FORMAT_LEFT, Scale*100},
		 {"Fd",          ?wxLIST_FORMAT_LEFT, Scale*50},
		 {"Domain",      ?wxLIST_FORMAT_LEFT, Scale*60},
		 {"Type",        ?wxLIST_FORMAT_LEFT, Scale*100},
		 {"Protocol",    ?wxLIST_FORMAT_LEFT, Scale*100},
		 {"Read State",  ?wxLIST_FORMAT_LEFT, Scale*150},
		 {"Write State", ?wxLIST_FORMAT_LEFT, Scale*150}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:connect(Grid, command_list_item_right_click),
    wxListCtrl:connect(Grid, command_list_item_activated),
    wxListCtrl:connect(Grid, command_list_col_click),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    Even = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOX),
    Odd  = observer_lib:mix(Even,
			    wxSystemSettings:getColour(?wxSYS_COLOUR_HIGHLIGHT),
			    0.8),
    Opt  = #opt{odd_bg = Odd},
    {Panel, #state{parent = Parent,
		   panel  = Panel,
		   sizer  = Sizer,
		   fields = GenFields,
		   grid   = Grid,
		   timer  = Config,
		   opt    = Opt}}.

handle_event(#wx{id=?ID_REFRESH},
	     State = #state{node = Node,
			    grid = Grid,
			    opt  = Opt} = State) ->
    _ = update_gen_socket_info(State),
    Sockets0 = get_sockets(Node),
    Sockets  = update_grid(Grid, sel(State), Opt, Sockets0),
    {noreply, State#state{sockets = Sockets}};

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
    Sockets0 = get_sockets(Node),
    Sockets  = update_grid(Grid, sel(State), Opt, Sockets0),
    wxWindow:setFocus(Grid),
    {noreply, State#state{opt = Opt, sockets = Sockets}};

handle_event(#wx{event=#wxSize{size={W,_}}},  State=#state{grid=Grid}) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event = #wxList{type      = command_list_item_activated,
				 itemIndex = Index}},
	     State = #state{grid      = Grid,
			    sockets   = Sockets,
			    open_wins = Opened}) ->
    if
        length(Sockets) >= (Index+1) ->
            Socket    = lists:nth(Index+1, Sockets),
            NewOpened = display_socket_info(Grid, Socket, Opened),
            {noreply, State#state{open_wins = NewOpened}};
        true -> % Race - should we do somthing here?
            {noreply, State}
    end;

handle_event(#wx{event = #wxList{type      = command_list_item_right_click,
				 itemIndex = Index}},
	     State = #state{panel = Panel, sockets=Sockets}) ->
    case Index of
	-1 ->
	    {noreply, State};
	_ ->
	    Socket = lists:nth(Index + 1, Sockets),
	    Menu   = wxMenu:new(),
	    wxMenu:append(Menu, ?ID_SOCKET_INFO,
			  f("Socket info for ~s", [Socket#socket.id_str])),
	    %% wxMenu:append(Menu, ?ID_DEBUG_SOCKETS,
	    %% 		  "Debug selected sockets",
	    %% 		  [{help, ?DEBUG_SOCKETS_STR}]),
	    wxMenu:append(Menu, ?ID_CLOSE_SOCKET,
			  f("Close ~p", [Socket#socket.id_str])),
	    wxWindow:popupMenu(Panel, Menu),
	    wxMenu:destroy(Menu),
	    {noreply, State#state{right_clicked_socket = Socket}}
    end;

handle_event(#wx{id = ?ID_SOCKET_INFO},
	     State = #state{grid                 = Grid,
			    right_clicked_socket = Socket,
			    open_wins            = Opened}) ->
    case Socket of
	undefined ->
	    {noreply, State};
	_ ->
	    NewOpened = display_socket_info(Grid, Socket, Opened),
	    {noreply, State#state{right_clicked_socket = undefined,
				  open_wins            = NewOpened}}
    end;

handle_event(#wx{id = ?ID_SOCKET_INFO_SELECTED},
	     State = #state{grid      = Grid,
			    sockets   = Sockets,
			    open_wins = Opened}) ->
    case get_selected_items(Grid, Sockets) of
	[] ->
	    observer_wx:create_txt_dialog(State#state.panel,
					  "No selected sockets",
					  "Socket Info", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Selected ->
	    NewOpened = lists:foldl(fun(S, O) ->
					    display_socket_info(Grid, S, O)
				    end,
				    Opened, Selected),
	    {noreply, State#state{open_wins = NewOpened}}
    end;

handle_event(#wx{id = ?ID_CLOSE_SOCKET},
	     State = #state{right_clicked_socket = Socket}) ->
    case Socket of
	undefined ->
	    {noreply, State};
	_ ->
	    socket:close(Socket#socket.id),
	    {noreply, State#state{right_clicked_socket = undefined}}
	end;

%% handle_event(#wx{id=?ID_DEBUG_NEW, event=#wxCommand{type=command_menu_selected}}, State) ->
%%     observer_trace_wx:add_aockets([new_sockets]),
%%     {noreply,  State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     State = #state{grid=Grid, timer=Timer0}) ->
    Timer = observer_lib:interval_dialog(Grid, Timer0, 10, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(#wx{obj=MoreEntry,event=#wxMouse{type=left_down},userData={more,More}}, State) ->
    observer_lib:add_scroll_entries(MoreEntry,More),
    {noreply, State};

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

handle_call(get_config, _, #state{timer=Timer}=State) ->
    {reply, observer_lib:timer_config(Timer), State};

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).

handle_info(refresh_interval, State = #state{node    = Node,
					     grid    = Grid,
					     opt     = Opt,
                                             sockets = OldSockets} = State) ->
    case get_sockets(Node) of
        OldSockets ->
            %% no change
            {noreply, State};
        Sockets0 ->
	    _ = update_gen_socket_info(State),
            Sockets = update_grid(Grid, sel(State), Opt, Sockets0),
            {noreply, State#state{sockets = Sockets}}
    end;

handle_info({active, NodeName},
	    #state{parent = Parent,
		   grid   = Grid,
		   opt    = Opt,
		   timer  = Timer0} = State0) ->
    Available = socketinfo_available(NodeName),
    Available orelse popup_unavailable_info(NodeName),
    State1   = State0#state{node = {NodeName, Available}},
    _ = update_gen_socket_info(State1),
    Sockets0 = get_sockets(NodeName, Available),
    Sockets  = update_grid(Grid, sel(State1), Opt, Sockets0),
    wxWindow:setFocus(Grid),
    create_menus(Parent),
    Timer = observer_lib:start_timer(Timer0, 10),
    {noreply, State1#state{sockets = Sockets,
			   timer   = Timer}};

handle_info(not_active, State = #state{timer = Timer0}) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info({info, {socket_info_not_available, NodeName}},
            State = #state{panel=Panel}) ->
    Str = io_lib:format("Can not fetch socket info from ~p.~n"
                        "Too old OTP version.", [NodeName]),
    observer_lib:display_info_dialog(Panel, Str),
    {noreply, State};

handle_info({error, Error}, #state{panel=Panel} = State) ->
    ErrorStr = if is_list(Error) -> Error;
		  true -> f("~p", [Error])
	       end,
    Str = io_lib:format("ERROR: ~ts~n", [ErrorStr]),
    observer_lib:display_info_dialog(Panel, Str),
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
	  [#create_menu{id = ?ID_SOCKET_INFO_SELECTED,
			text = "Socket info for selected sockets\tCtrl+I"},
	   separator,
	   #create_menu{id = ?ID_REFRESH, text = "Refresh\tCtrl+R"},
	   #create_menu{id = ?ID_REFRESH_INTERVAL, text = "Refresh Interval..."}
	  ]}%% ,
	 %% {"Debug",
	 %%  [#create_menu{id=?ID_DEBUG_SOCKETS, text="Debug selected socket"},
	 %%   #create_menu{id=?ID_DEBUG_NEW,     text="Debug new sockets"}
	 %%  ]}
	],
    observer_wx:create_menus(Parent, MenuEntries).


get_sockets({NodeName, Available}) ->
    get_sockets(NodeName, Available);
get_sockets(NodeName) when is_atom(NodeName) ->
    case rpc:call(NodeName, observer_backend, get_socket_list, []) of
	SocketInfoMaps when is_list(SocketInfoMaps) ->
	    [infomap_to_rec(SockInfo) || SockInfo <- SocketInfoMaps];
	{badrpc,
	 {'EXIT', {undef, [{observer_backend, get_socket_list, [], []}]}}} ->
	    {error, "No socket backend support"};
	{badrpc, Error} ->
	    {error, Error};
	{error, _} = ERROR ->
	    ERROR
    end.

get_sockets(_NodeName, false) ->
    [];
get_sockets(NodeName, true) ->
    case get_sockets(NodeName) of
	{error, _} = ERROR ->
	    self() ! ERROR,
	    [];
	Res ->
	    Res
    end.


infomap_to_rec(#{id           := Id,
		 id_str       := IdStr,
		 kind         := Kind,
		 fd           := FD,
		 owner        := Owner,
		 domain       := Domain,
		 type         := Type,
		 protocol     := Protocol,
		 rstates      := RState,
		 wstates      := WState,
		 monitored_by := MonitoredBy,
		 statistics   := Statistics,
		 options      := Options} = Info) ->
      #socket{id           = Id,
	      id_str       = IdStr,
	      kind         = Kind,
	      fd           = FD,
	      owner        = Owner,
	      domain       = Domain,
	      type         = Type,
	      protocol     = Protocol,
	      raddress     = maps:get(raddress, Info, undefined),
	      laddress     = maps:get(laddress, Info, undefined),
	      rstate       = RState,
	      wstate       = WState,
	      monitored_by = MonitoredBy,
	      statistics   = Statistics,
	      options      = Options}.

socketrec_to_list(#socket{id           = Id,
			  id_str       = IdStr,
			  kind         = Kind,
			  fd           = FD,
			  owner        = Owner,
			  domain       = Domain,
			  type         = Type,
			  protocol     = Protocol,
			  raddress     = RAddr,
			  laddress     = LAddr,
			  rstate       = RState,
			  wstate       = WState,
			  monitored_by = MonitoredBy,
			  statistics   = Statistics,
			  options      = Options}) ->
    [{id,           Id},
     {id_str,       IdStr},
     {kind,         Kind},
     {fd,           FD},
     {owner,        Owner},
     {domain,       Domain},
     {type,         Type},
     {protocol,     Protocol},
     {raddress,     RAddr},
     {laddress,     LAddr},
     {rstate,       RState},
     {wstate,       WState},
     {monitored_by, MonitoredBy},
     {statistics,   Statistics},
     {options,      Options}].

display_socket_info(Parent, #socket{id_str = IdStr} = Sock, Opened) ->
    case lists:keyfind(IdStr, 1, Opened) of
	false ->
	    Frame = do_display_socket_info(Parent, Sock),
	    [{IdStr, Frame}|Opened];
	{_,Win} ->
	    wxFrame:raise(Win),
	    Opened
    end.

do_display_socket_info(Parent0, #socket{id_str = IdStr} = SocketRec) ->
    Parent = observer_lib:get_wx_parent(Parent0),
    Title = "Socket Info: " ++ IdStr,
    Scale = observer_wx:get_scale(),
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title,
			    [{style, ?wxSYSTEM_MENU bor ?wxCAPTION
				  bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER},
                             {size,{Scale * 600, Scale * 400}}]),
    ScrolledWin = wxScrolledWindow:new(Frame,[{style,?wxHSCROLL bor ?wxVSCROLL}]),
    wxScrolledWindow:enableScrolling(ScrolledWin,true,true),
    wxScrolledWindow:setScrollbars(ScrolledWin,20,20,0,0),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxWindow:setSizer(ScrolledWin,Sizer),
    Socket = socketrec_to_list(SocketRec),
    Fields0 = socket_info_fields(Socket),
    _UpFields = observer_lib:display_info(ScrolledWin, Sizer, Fields0),
    wxFrame:center(Frame),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxFrame:show(Frame),
    Frame.



socket_info_fields(Socket0) ->
    {Struct0, Socket} = extra_fields(Socket0),
    Struct =
	[{"Overview",
	  [{"Owner",            {click,owner}},
	   {"Fd",               fd},
	   {"Domain",           domain},
           {"Type",             type},
           {"Protocol",         protocol},
           {"Read State",       rstate},
           {"Write State",      wstate}]},
	 {scroll_boxes,
	  [{"Monitored by",1,{click,monitored_by}}]} | Struct0],
    %% d("socket_info_fields -> "
    %%   "~n   Struct: ~p"
    %%   "~n   Socket: ~p", [Struct, Socket]),
    observer_lib:fill_info(Struct, Socket).

extra_fields(Socket) ->
    Statistics = proplists:get_value(statistics, Socket, []),
    Options    = proplists:get_value(options, Socket, []),
    Struct     = [{"Net",
		   [{"Local Address",  laddress},
		    {"Remote Address", raddress}]},
		  {"Statistics",
		   [stat_name_and_unit(Key) || {Key,_} <- Statistics]},
		  {"Options",
		   [{socket, sockopt_to_list(Key), Key} ||
		       {Key, _} <- Options]}],
    Socket1    = lists:keydelete(statistics, 1, Socket),
    Socket2    = lists:keydelete(options, 1, Socket1),
    {Struct, Socket2 ++ Statistics ++ Options}.

stat_name_and_unit(acc_fails = Key) ->
    {"Number of accept fails", Key};
stat_name_and_unit(acc_success = Key) ->
    {"Number of accept success", Key};
stat_name_and_unit(acc_tries = Key) ->
    {"Number of accept tries", Key};
stat_name_and_unit(acc_waits = Key) ->
    {"Number of accept waits", Key};

stat_name_and_unit(read_byte = Key) ->
    {"Total read", {bytes, Key}};
stat_name_and_unit(read_fails = Key) ->
    {"Number of read fails", Key};
stat_name_and_unit(read_tries = Key) ->
    {"Number of read tries", Key};
stat_name_and_unit(read_waits = Key) ->
    {"Number of read waits", Key};
stat_name_and_unit(read_pkg = Key) ->
    {"Number of packats read", Key};
stat_name_and_unit(read_pkg_max = Key) ->
    {"Largest package read", {bytes, Key}};

stat_name_and_unit(write_byte = Key) ->
    {"Total written", {bytes, Key}};
stat_name_and_unit(write_fails = Key) ->
    {"Number of write fails", Key};
stat_name_and_unit(write_tries = Key) ->
    {"Number of write tries", Key};
stat_name_and_unit(write_waits = Key) ->
    {"Number of write waits", Key};
stat_name_and_unit(write_pkg = Key) ->
    {"Number of packats written", Key};
stat_name_and_unit(write_pkg_max = Key) ->
    {"Largest package written", {bytes, Key}};
stat_name_and_unit(Key) ->
    {atom_to_list(Key), Key}.

sockopt_to_list({LevelOrProto, Opt}) ->
    f("~w:~w", [LevelOrProto, Opt]).


update_grid(Grid, Sel, Opt, Ports) ->
    wx:batch(fun() -> update_grid2(Grid, Sel, Opt, Ports) end).
update_grid2(Grid, Sel, #opt{sort_key  = Sort,
			     sort_incr = Dir,
			     odd_bg    = BG}, Ports) ->
    wxListCtrl:deleteAllItems(Grid),
    Update =
	fun(#socket{id       = Id,
		    id_str   = IdStr,
		    owner    = Owner,
		    fd       = Fd,
		    domain   = Domain,
		    type     = Type,
		    protocol = Proto,
		    rstate   = RState,
		    wstate   = WState},
	    Row) ->
		_Item = wxListCtrl:insertItem(Grid, Row, ""),
		if (Row rem 2) =:= 1 ->
			wxListCtrl:setItemBackgroundColour(Grid, Row, BG);
		   true -> ignore
		end,

		lists:foreach(fun({Col, Val}) ->
				      wxListCtrl:setItem(Grid, Row, Col,
							 observer_lib:to_str(Val))
			      end,
			      [{0,IdStr},
			       {1,Owner},
			       {2,Fd},
			       {3,Domain},
			       {4,Type},
			       {5,Proto},
			       {6,if (RState =:= []) -> "-"; true -> RState end},
			       {7,if (WState =:= []) -> "-"; true -> WState end}]),
                case lists:member(Id, Sel) of
                    true ->
                        wxListCtrl:setItemState(Grid,
						Row,
						16#FFFF,
						?wxLIST_STATE_SELECTED);
                    false ->
                        wxListCtrl:setItemState(Grid,
						Row,
						0,
						?wxLIST_STATE_SELECTED)
                end,
		Row + 1
	end,
    PortInfo = case Dir of
		   false -> lists:reverse(lists:keysort(Sort, Ports));
		   true -> lists:keysort(Sort, Ports)
	       end,
    lists:foldl(Update, 0, PortInfo),
    PortInfo.

sel(#state{grid = Grid, sockets = Sockets}) ->
    [Id || #socket{id = Id} <- get_selected_items(Grid, Sockets)].

get_selected_items(Grid, Data) ->
    get_indecies(get_selected_items(Grid, -1, []), Data).
get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index,
				  [{geometry, ?wxLIST_NEXT_ALL},
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

socketinfo_available(NodeName) ->
    _ = rpc:call(NodeName, code, ensure_loaded, [observer_backend]),
    case rpc:call(NodeName, erlang, function_exported,
                  [observer_backend, get_socket_list, 0]) of
        true  -> true;
        false -> false
    end.

popup_unavailable_info(NodeName) ->
    self() ! {info, {socket_info_not_available, NodeName}},
    ok.

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% d(F) ->
%%     d(F, []).

%% d(Debug, F) when is_boolean(Debug) andalso is_list(F) ->
%%     d(Debug, F, []);
%% d(F, A) when is_list(F) andalso is_list(A) ->
%%     d(get(debug), F, A).

%% d(true, F, A) ->
%%     io:format("[oswx] " ++ F ++ "~n", A);
%% d(_, _, _) ->
%%     ok.


