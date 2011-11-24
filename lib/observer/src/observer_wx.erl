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
-module(observer_wx).

-behaviour(wx_object).

-export([start/0]).
-export([create_menus/2, get_attrib/1, get_tracer/0,
	 create_txt_dialog/4, try_rpc/4, return_to_localnode/2]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2, check_page_title/1]).

%% Includes
-include_lib("wx/include/wx.hrl").

-include("observer_defs.hrl").

%% Defines

-define(ID_PING, 1).
-define(ID_CONNECT, 2).
-define(ID_NOTEBOOK, 3).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

-define(TRACE_STR, "Trace Overview").

%% Records
-record(state,
	{frame,
	 menubar,
	 menus = [],
	 status_bar,
	 notebook,
	 main_panel,
	 pro_panel,
	 tv_panel,
	 sys_panel,
	 trace_panel,
	 active_tab,
	 node,
	 nodes
	}).

start() ->
    wx_object:start(?MODULE, [], []).

create_menus(Object, Menus) when is_list(Menus) ->
    wx_object:call(Object, {create_menus, Menus}).

get_attrib(What) ->
    wx_object:call(observer, {get_attrib, What}).

get_tracer() ->
    wx_object:call(observer, get_tracer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    register(observer, self()),
    wx:new(),
    catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Observer",
			[{size, {1000, 500}}, {style, ?wxDEFAULT_FRAME_STYLE}]),
    IconFile = filename:join(code:priv_dir(observer), "erlang_observer.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon),

    State = #state{frame = Frame},
    UpdState = setup(State),
    net_kernel:monitor_nodes(true),
    process_flag(trap_exit, true),
    {Frame, UpdState}.

setup(#state{frame = Frame} = State) ->
    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),

    {Nodes, NodeMenus} = get_nodes(),
    DefMenus = default_menus(NodeMenus),
    observer_lib:create_menus(DefMenus, MenuBar, default),

    wxFrame:setMenuBar(Frame, MenuBar),
    StatusBar = wxFrame:createStatusBar(Frame, []),
    wxFrame:setTitle(Frame, atom_to_list(node())),
    wxStatusBar:setStatusText(StatusBar, atom_to_list(node())),

    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

    %% System Panel
    SysPanel = observer_sys_wx:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, SysPanel, "System", []),

    %% Setup sizer create early to get it when window shows
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),

    wxNotebook:connect(Notebook, command_notebook_page_changing),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxMenu:connect(Frame, command_menu_selected),
    wxFrame:show(Frame),

    %% I postpone the creation of the other tabs so they can query/use
    %% the window size

    %% Process Panel
    ProPanel = observer_pro_wx:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),

    %% Table Viewer Panel
    TVPanel = observer_tv_wx:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, TVPanel, "Table Viewer", []),

    %% Trace Viewer Panel
    TracePanel = observer_trace_wx:start_link(Notebook, self()),
    wxNotebook:addPage(Notebook, TracePanel, ?TRACE_STR, []),

    %% Force redraw (window needs it)
    wxWindow:refresh(Panel),

    SysPid = wx_object:get_pid(SysPanel),
    SysPid ! {active, node()},
    UpdState = State#state{main_panel = Panel,
			   notebook = Notebook,
			   menubar = MenuBar,
			   status_bar = StatusBar,
			   sys_panel = SysPanel,
			   pro_panel = ProPanel,
			   tv_panel  = TVPanel,
			   trace_panel = TracePanel,
			   active_tab = SysPid,
			   node  = node(),
			   nodes = Nodes
			  },
    %% Create resources which we don't want to duplicate
    SysFont = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    SysFontSize = wxFont:getPointSize(SysFont),
    Modern = wxFont:new(SysFontSize, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    put({font, modern}, Modern),
    put({font, fixed}, Modern),
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{event=#wxNotebook{type=command_notebook_page_changing}},
	     #state{active_tab=Previous, node=Node} = State) ->
    Pid = get_active_pid(State),
    Previous ! not_active,
    Pid ! {active, Node},
    {noreply, State#state{active_tab=Pid}};

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?wxID_HELP, event = #wxCommand{type = command_menu_selected}}, State) ->
    External = "http://www.erlang.org/doc/apps/observer/index.html",
    Internal = filename:join([code:lib_dir(observer),"doc", "html", "index.html"]),
    Help = case filelib:is_file(Internal) of
	       true -> Internal;
	       false -> External
	   end,
    wx_misc:launchDefaultBrowser(Help) orelse
	create_txt_dialog(State#state.frame, "Could not launch browser: ~n " ++ Help,
			  "Error", ?wxICON_ERROR),
    {noreply, State};

handle_event(#wx{id = ?wxID_ABOUT, event = #wxCommand{type = command_menu_selected}},
	     State = #state{frame=Frame}) ->
    AboutString = "Observe an erlang system\n"
	"Authors: Olle Mattson & Magnus Eriksson & Dan Gudmundsson",
    Style = [{style, ?wxOK bor ?wxSTAY_ON_TOP},
	     {caption, "About"}],
    wxMessageDialog:showModal(wxMessageDialog:new(Frame, AboutString, Style)),
    {noreply, State};


handle_event(#wx{id = ?ID_CONNECT, event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame} = State) ->
    UpdState = case create_connect_dialog(connect, State) of
		   cancel ->
		       State;
		   {value, [], _, _} ->
		       create_txt_dialog(Frame, "Node must have a name",
					 "Error", ?wxICON_ERROR),
		       State;
		   {value, NodeName, LongOrShort, Cookie} -> %Shortname,
		       try
			   case connect(list_to_atom(NodeName), LongOrShort, list_to_atom(Cookie)) of
			       {ok, set_cookie} ->
				   change_node_view(node(), State);
			       {error, set_cookie} ->
				   create_txt_dialog(Frame, "Could not set cookie",
						     "Error", ?wxICON_ERROR),
				   State;
			       {error, net_kernel, _Reason} ->
				   create_txt_dialog(Frame, "Could not enable node",
						     "Error", ?wxICON_ERROR),
				   State
			   end
		       catch _:_ ->
			       create_txt_dialog(Frame, "Could not enable node",
						 "Error", ?wxICON_ERROR),
			       State
		       end
	       end,
    {noreply, UpdState};

handle_event(#wx{id = ?ID_PING, event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame} = State) ->
    UpdState = case create_connect_dialog(ping, State) of
		   cancel ->  State;
		   {value, Value} when is_list(Value) ->
		       try
			   Node = list_to_atom(Value),
			   case net_adm:ping(Node) of
			       pang ->
				   create_txt_dialog(Frame, "Connect failed", "Pang", ?wxICON_EXCLAMATION),
				   State;
			       pong ->
				   change_node_view(Node, State)
			   end
		       catch _:_ ->
			       create_txt_dialog(Frame, "Connect failed", "Pang", ?wxICON_EXCLAMATION),
			       State
		       end
	       end,
    {noreply, UpdState};

handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}}, State)
  when Id > ?FIRST_NODES_MENU_ID, Id < ?LAST_NODES_MENU_ID ->

    Node = lists:nth(Id - ?FIRST_NODES_MENU_ID, State#state.nodes),
    UpdState = change_node_view(Node, State),
    {noreply, UpdState};

handle_event(Event, State) ->
    Pid = get_active_pid(State),
    Pid ! Event,
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_call({create_menus, TabMenus}, _From,
	    State = #state{menubar=MenuBar, menus=PrevTabMenus}) ->
    wx:batch(fun() ->
		     clean_menus(PrevTabMenus, MenuBar),
		     observer_lib:create_menus(TabMenus, MenuBar, plugin)
	     end),
    {reply, ok, State#state{menus=TabMenus}};

handle_call({get_attrib, Attrib}, _From, State) ->
    {reply, get(Attrib), State};

handle_call(get_tracer, _From, State=#state{trace_panel=TraceP}) ->
    {reply, TraceP, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info({nodeup, _Node}, State) ->
    State2 = update_node_list(State),
    {noreply, State2};

handle_info({nodedown, Node},
	    #state{frame = Frame} = State) ->
    State2 = case Node =:= State#state.node of
		 true ->
		     change_node_view(node(), State);
		 false ->
		     State
	     end,
    State3 = update_node_list(State2),
    Msg = ["Node down: " | atom_to_list(Node)],
    create_txt_dialog(Frame, Msg, "Node down", ?wxICON_EXCLAMATION),
    {noreply, State3};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    io:format("Child crashed exiting:  ~p ~p~n", [_Pid,_Reason]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{frame = Frame}) ->
    wxFrame:destroy(Frame),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_rpc(Node, Mod, Func, Args) ->
    case
	rpc:call(Node, Mod, Func, Args) of
	{badrpc, Reason} ->
	    error_logger:error_report([{node, Node},
				       {call, {Mod, Func, Args}},
				       {reason, {badrpc, Reason}}]),
	    error({badrpc, Reason});
	Res ->
	    Res
    end.

return_to_localnode(Frame, Node) ->
    case node() =/= Node of
	true ->
	    create_txt_dialog(Frame, "Error occured on remote node",
			      "Error", ?wxICON_ERROR),
	    disconnect_node(Node);
	false ->
	    ok
    end.

create_txt_dialog(Frame, Msg, Title, Style) ->
    MD = wxMessageDialog:new(Frame, Msg, [{style, Style}]),
    wxMessageDialog:setTitle(MD, Title),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD).

connect(NodeName, 0, Cookie) ->
    connect2(NodeName, shortnames, Cookie);
connect(NodeName, 1, Cookie) ->
    connect2(NodeName, longnames, Cookie).

connect2(NodeName, Opts, Cookie) ->
    case net_adm:names() of
	{ok, _} -> %% Epmd is running
	    ok;
	{error, address} ->
	    Epmd = os:find_executable("epmd"),
	    os:cmd(Epmd)
    end,
    case net_kernel:start([NodeName, Opts]) of
	{ok, _} ->
	    case is_alive() of
		true ->
		    erlang:set_cookie(node(), Cookie),
		    {ok, set_cookie};
		false ->
		    {error, set_cookie}
	    end;
	{error, Reason} ->
	    {error, net_kernel, Reason}
    end.

change_node_view(Node, State = #state{pro_panel=Pro, sys_panel=Sys, tv_panel=Tv}) ->
    lists:foreach(fun(Pid) -> wx_object:get_pid(Pid) ! {node, Node} end,
		  [Pro, Sys, Tv]),
    StatusText = ["Observer - " | atom_to_list(Node)],
    wxFrame:setTitle(State#state.frame, StatusText),
    wxStatusBar:setStatusText(State#state.status_bar, StatusText),
    State#state{node = Node}.

check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

get_active_pid(#state{notebook=Notebook, pro_panel=Pro, sys_panel=Sys, tv_panel=Tv, trace_panel=Trace}) ->
    Panel = case check_page_title(Notebook) of
		"Processes" -> Pro;
		"System" -> Sys;
		"Table Viewer" -> Tv;
		?TRACE_STR -> Trace
	    end,
    wx_object:get_pid(Panel).

create_connect_dialog(ping, #state{frame = Frame}) ->
    Dialog = wxTextEntryDialog:new(Frame, "Connect to node"),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    Value = wxTextEntryDialog:getValue(Dialog),
	    wxDialog:destroy(Dialog),
	    {value, Value};
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    cancel
    end;
create_connect_dialog(connect, #state{frame = Frame}) ->
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Distribute node "),

    VSizer = wxBoxSizer:new(?wxVERTICAL),
    RadioBoxSizer = wxBoxSizer:new(?wxHORIZONTAL),

    Choices = ["Short name", "Long name"],
    RadioBox = wxRadioBox:new(Dialog, 1, "",
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      Choices,
			      [{majorDim, 2},
			       {style, ?wxHORIZONTAL}]),

    NameText = wxStaticText:new(Dialog, ?wxID_ANY, "Node name: "),
    NameCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{size, {200, 25}}]),
    wxTextCtrl:setValue(NameCtrl, "observer"),
    CookieText = wxStaticText:new(Dialog, ?wxID_ANY, "Secret cookie: "),
    CookieCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY,
				[{size, {200, 25}}, {style, ?wxTE_PASSWORD}]),

    BtnSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxID_DEFAULT),
    Flags = [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}],
    wxSizer:add(RadioBoxSizer, RadioBox, Flags),

    wxSizer:add(VSizer, RadioBoxSizer, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, NameText),
    wxSizer:add(VSizer, NameCtrl, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, CookieText),
    wxSizer:add(VSizer, CookieCtrl, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, BtnSizer, [{flag, ?wxALIGN_LEFT}]),

    wxWindow:setSizer(Dialog, VSizer),
    CookiePath = filename:join(os:getenv("HOME"), ".erlang.cookie"),
    DefaultCookie = case filelib:is_file(CookiePath) of
			true ->
			    {ok, Bin} = file:read_file(CookiePath),
			    binary_to_list(Bin);
			false ->
			    ""
		    end,
    wxTextCtrl:setValue(CookieCtrl, DefaultCookie),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    NameValue = wxTextCtrl:getValue(NameCtrl),
	    NameLngthValue = wxRadioBox:getSelection(RadioBox),
	    CookieValue = wxTextCtrl:getValue(CookieCtrl),
	    wxDialog:destroy(Dialog),
	    {value, NameValue, NameLngthValue, CookieValue};
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    cancel
    end.

default_menus(NodesMenuItems) ->
    Quit  = #create_menu{id = ?wxID_EXIT, text = "Quit"},
    About = #create_menu{id = ?wxID_ABOUT, text = "About"},
    Help  = #create_menu{id = ?wxID_HELP},
    NodeMenu = case erlang:is_alive() of
		   true ->  {"Nodes", NodesMenuItems ++
				 [#create_menu{id = ?ID_PING, text = "Connect Node"}]};
		   false -> {"Nodes", NodesMenuItems ++
				 [#create_menu{id = ?ID_CONNECT, text = "Enable distribution"}]}
	       end,
    case os:type() =:= {unix, darwin} of
	false -> 
	    FileMenu = {"File", [Quit]},
	    HelpMenu = {"Help", [About,Help]},
	    [FileMenu, NodeMenu, HelpMenu];
	true ->
	    %% On Mac quit and about will be moved to the "default' place
	    %% automagicly, so just add them to a menu that always exist.
	    %% But not to the help menu for some reason
	    {Tag, Menus} = NodeMenu,
	    [{Tag, Menus ++ [Quit,About]}, {"&Help", [Help]}]
    end.

clean_menus(Menus, MenuBar) ->
    remove_menu_items(Menus, MenuBar).

remove_menu_items([{MenuStr = "File", Menus}|Rest], MenuBar) ->
    MenuId = wxMenuBar:findMenu(MenuBar, MenuStr),
    Menu = wxMenuBar:getMenu(MenuBar, MenuId),
    Items = [wxMenu:findItem(Menu, Tag) || #create_menu{text=Tag} <- Menus],
    [wxMenu:delete(Menu, MItem) || MItem <- Items],
    case os:type() =:= {unix, darwin} of
	true ->
	    wxMenuBar:remove(MenuBar, MenuId),
	    wxMenu:destroy(Menu);
	false ->
	    ignore
    end,
    remove_menu_items(Rest, MenuBar);
remove_menu_items([{"Nodes", _}|_], _MB) ->
    ok;
remove_menu_items([{Tag, _Menus}|Rest], MenuBar) ->
    MenuId = wxMenuBar:findMenu(MenuBar, Tag),
    Menu = wxMenuBar:getMenu(MenuBar, MenuId),
    wxMenuBar:remove(MenuBar, MenuId),
    Items = wxMenu:getMenuItems(Menu),
    [wxMenu:'Destroy'(Menu, Item) || Item <- Items],
    wxMenu:destroy(Menu),
    remove_menu_items(Rest, MenuBar);
remove_menu_items([], _MB) ->
    ok.

get_nodes() ->
    Nodes = [node()| nodes()],
    {_, Menues} =
	lists:foldl(fun(Node, {Id, Acc}) when Id < ?LAST_NODES_MENU_ID ->
			    {Id + 1, [#create_menu{id=Id + ?FIRST_NODES_MENU_ID,
						   text=atom_to_list(Node)} | Acc]}
		    end, {1, []}, Nodes),
    {Nodes, lists:reverse(Menues)}.

update_node_list(State = #state{menubar=MenuBar}) ->
    {Nodes, NodesMenuItems} = get_nodes(),
    NodeMenuId = wxMenuBar:findMenu(MenuBar, "Nodes"),
    NodeMenu = wxMenuBar:getMenu(MenuBar, NodeMenuId),
    wx:foreach(fun(Item) -> wxMenu:'Destroy'(NodeMenu, Item) end,
	       wxMenu:getMenuItems(NodeMenu)),

    Index = wx:foldl(fun(Record, Index) ->
			     observer_lib:create_menu_item(Record, NodeMenu, Index)
		     end, 0, NodesMenuItems),

    Dist = case erlang:is_alive() of
	       true  -> #create_menu{id = ?ID_PING, text = "Connect node"};
	       false -> #create_menu{id = ?ID_CONNECT, text = "Enable distribution"}
	   end,
    observer_lib:create_menu_item(Dist, NodeMenu, Index),
    State#state{nodes = Nodes}.
