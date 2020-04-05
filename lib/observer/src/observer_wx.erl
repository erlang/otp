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
-module(observer_wx).

-behaviour(wx_object).

-export([start/0, stop/0]).
-export([create_menus/2, get_attrib/1, get_tracer/0, get_active_node/0, get_menubar/0,
     get_scale/0, set_status/1, create_txt_dialog/4, try_rpc/4, return_to_localnode/2]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2, check_page_title/1]).

%% Includes
-include_lib("wx/include/wx.hrl").

-include("observer_defs.hrl").

%% Defines

-define(ID_PING, 1).
-define(ID_CONNECT, 2).
-define(ID_NOTEBOOK, 3).
-define(ID_CDV,      4).
-define(ID_LOGVIEW, 5).

-define(FIRST_NODES_MENU_ID, 1000).
-define(LAST_NODES_MENU_ID,  2000).

-define(TRACE_STR, "Trace Overview").
-define(ALLOC_STR, "Memory Allocators").

%% Records
-record(state,
	{frame,
	 menubar,
	 menus = [],
	 status_bar,
	 notebook,
	 main_panel,
         panels,
	 active_tab,
	 node,
	 nodes,
	 prev_node="",
	 log = false,
	 reply_to=false,
         config
	}).

start() ->
    case wx_object:start(?MODULE, [], []) of
	Err = {error, _} -> Err;
	_Obj -> ok
    end.

stop() ->
    wx_object:call(observer, stop).

create_menus(Object, Menus) when is_list(Menus) ->
    wx_object:call(Object, {create_menus, Menus}).

get_attrib(What) ->
    wx_object:call(observer, {get_attrib, What}).

set_status(What) ->
    wx_object:cast(observer, {status_bar, What}).

get_tracer() ->
    wx_object:call(observer, get_tracer).

get_active_node() ->
    wx_object:call(observer, get_active_node).

get_menubar() ->
    wx_object:call(observer, get_menubar).

get_scale() ->
    ScaleStr = os:getenv("OBSERVER_SCALE", "1"),
    try list_to_integer(ScaleStr) of
        Scale when Scale < 1 -> 1;
        Scale -> Scale
    catch _:_ ->
        1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    register(observer, self()),
    wx:new(),
    catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1),
    Scale = get_scale(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Observer",
			[{size, {Scale * 850, Scale * 600}}, {style, ?wxDEFAULT_FRAME_STYLE}]),
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
    Config = load_config(),
    Cnf = fun(Who) ->
                  proplists:get_value(Who, Config, #{})
          end,
    MenuBar = wxMenuBar:new(),

    {Nodes, NodeMenus} = get_nodes(),
    DefMenus = default_menus(NodeMenus),
    observer_lib:create_menus(DefMenus, MenuBar, default),

    wxFrame:setMenuBar(Frame, MenuBar),

    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

    %% System Panel
    SysPanel = observer_sys_wx:start_link(Notebook, self(), Cnf(sys_panel)),
    wxNotebook:addPage(Notebook, SysPanel, "System", []),

    %% Setup sizer create early to get it when window shows
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),

    StatusBar = wxStatusBar:new(Frame),
    wxFrame:setStatusBar(Frame, StatusBar),
    wxFrame:setTitle(Frame, atom_to_list(node())),
    wxStatusBar:setStatusText(StatusBar, atom_to_list(node())),

    wxNotebook:connect(Notebook, command_notebook_page_changed,
                       [{skip, true}, {id, ?ID_NOTEBOOK}]),
    wxFrame:connect(Frame, close_window, []),
    wxMenu:connect(Frame, command_menu_selected),
    wxFrame:show(Frame),

    %% Freeze and thaw is buggy currently
    DoFreeze = [?wxMAJOR_VERSION,?wxMINOR_VERSION] < [2,9]
        orelse element(1, os:type()) =:= win32,
    DoFreeze andalso wxWindow:freeze(Panel),
    %% I postpone the creation of the other tabs so they can query/use
    %% the window size

    %% Perf Viewer Panel
    PerfPanel = observer_perf_wx:start_link(Notebook, self(), Cnf(perf_panel)),
    wxNotebook:addPage(Notebook, PerfPanel, "Load Charts", []),

    %% Memory Allocator Viewer Panel
    AllcPanel = observer_alloc_wx:start_link(Notebook, self(), Cnf(allc_panel)),
    wxNotebook:addPage(Notebook, AllcPanel, ?ALLOC_STR, []),

    %% App Viewer Panel
    AppPanel = observer_app_wx:start_link(Notebook, self(), Cnf(app_panel)),
    wxNotebook:addPage(Notebook, AppPanel, "Applications", []),

    %% Process Panel
    ProPanel = observer_pro_wx:start_link(Notebook, self(), Cnf(pro_panel)),
    wxNotebook:addPage(Notebook, ProPanel, "Processes", []),

    %% Port Panel
    PortPanel = observer_port_wx:start_link(Notebook, self(), Cnf(port_panel)),
    wxNotebook:addPage(Notebook, PortPanel, "Ports", []),

    %% Table Viewer Panel
    TVPanel = observer_tv_wx:start_link(Notebook, self(), Cnf(tv_panel)),
    wxNotebook:addPage(Notebook, TVPanel, "Table Viewer", []),

    %% Trace Viewer Panel
    TracePanel = observer_trace_wx:start_link(Notebook, self(), Cnf(trace_panel)),
    wxNotebook:addPage(Notebook, TracePanel, ?TRACE_STR, []),

    %% Force redraw (windows needs it)
    wxWindow:refresh(Panel),
    DoFreeze andalso wxWindow:thaw(Panel),

    wxFrame:raise(Frame),
    wxFrame:setFocus(Frame),

    SysPid = wx_object:get_pid(SysPanel),
    SysPid ! {active, node()},
    Panels = [{sys_panel, SysPanel, "System"},   %% In order
              {perf_panel, PerfPanel, "Load Charts"},
              {allc_panel, AllcPanel, ?ALLOC_STR},
              {app_panel,  AppPanel, "Applications"},
              {pro_panel, ProPanel, "Processes"},
              {port_panel, PortPanel, "Ports"},
              {tv_panel, TVPanel, "Table Viewer"},
              {trace_panel, TracePanel, ?TRACE_STR}],

    UpdState = State#state{main_panel = Panel,
			   notebook = Notebook,
			   menubar = MenuBar,
			   status_bar = StatusBar,
			   active_tab = SysPid,
                           panels = Panels,
			   node  = node(),
			   nodes = Nodes
			  },
    %% Create resources which we don't want to duplicate
    SysFont = wxSystemSettings:getFont(?wxSYS_SYSTEM_FIXED_FONT),
    %% OemFont = wxSystemSettings:getFont(?wxSYS_OEM_FIXED_FONT),
    %% io:format("Sz sys ~p(~p) oem ~p(~p)~n",
    %% 	      [wxFont:getPointSize(SysFont), wxFont:isFixedWidth(SysFont),
    %% 	       wxFont:getPointSize(OemFont), wxFont:isFixedWidth(OemFont)]),
    Fixed = case wxFont:isFixedWidth(SysFont) of
		true -> SysFont;
		false -> %% Sigh
		    SysFontSize = wxFont:getPointSize(SysFont),
		    wxFont:new(SysFontSize, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)
	    end,
    put({font, fixed}, Fixed),
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{event=#wxNotebook{type=command_notebook_page_changed, nSel=Next}},
	     #state{active_tab=Previous, node=Node, panels=Panels, status_bar=SB} = State) ->
    {_, Obj, _} = lists:nth(Next+1, Panels),
    case wx_object:get_pid(Obj) of
	Previous ->
            {noreply, State};
	Pid ->
            wxStatusBar:setStatusText(SB, ""),
	    Previous ! not_active,
	    Pid ! {active, Node},
	    {noreply, State#state{active_tab=Pid}}
    end;

handle_event(#wx{id = ?ID_CDV, event = #wxCommand{type = command_menu_selected}}, State) ->
    spawn(crashdump_viewer, start, []),
    {noreply, State};

handle_event(#wx{event = #wxClose{}}, State) ->
    stop_servers(State),
    {noreply, State};

handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_menu_selected}}, State) ->
    stop_servers(State),
    {noreply, State};

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
		   cancel -> State;
		   {value, Value} when is_list(Value) ->
		       try
			   Node = list_to_atom(Value),
			   case net_adm:ping(Node) of
			       pang ->
				   create_txt_dialog(Frame, "Connect failed", "Pang", ?wxICON_EXCLAMATION),
				   State#state{prev_node=Value};
			       pong ->
				   State1 = change_node_view(Node, State),
				   State1#state{prev_node=Value}
			   end
		       catch _:_ ->
			       create_txt_dialog(Frame, "Connect failed", "Pang", ?wxICON_EXCLAMATION),
			       State#state{prev_node=Value}
		       end
	       end,
    {noreply, UpdState};

handle_event(#wx{id = ?ID_LOGVIEW, event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame, log = PrevLog, node = Node} = State) ->
    try
	ok = ensure_sasl_started(Node),
	ok = ensure_mf_h_handler_used(Node),
	ok = ensure_rb_mode(Node, PrevLog),
	case PrevLog of
	    false ->
		rpc:block_call(Node, rb, start, []),
		set_status("Observer - " ++ atom_to_list(Node) ++ " (rb_server started)"),
		{noreply, State#state{log=true}};
	    true ->
		rpc:block_call(Node, rb, stop, []),
		set_status("Observer - " ++ atom_to_list(Node) ++ " (rb_server stopped)"),
		{noreply, State#state{log=false}}
	end
    catch
	throw:Reason ->
	    create_txt_dialog(Frame, Reason, "Log view status", ?wxICON_ERROR),
	    {noreply, State}
    end;

handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}},
	     #state{nodes= Ns , node = PrevNode, log = PrevLog} = State)
  when Id > ?FIRST_NODES_MENU_ID, Id < ?LAST_NODES_MENU_ID ->
    Node = lists:nth(Id - ?FIRST_NODES_MENU_ID, Ns),
    %% Close rb_server only if another node than current one selected
    LState = case PrevLog of
		 true  -> case Node == PrevNode of
			      false -> rpc:block_call(PrevNode, rb, stop, []),
				       State#state{log=false} ;
			      true  -> State
			  end;
		 false -> State
             end,
    {noreply, change_node_view(Node, LState)};

handle_event(Event, #state{active_tab=Pid} = State) ->
    Pid ! Event,
    {noreply, State}.

handle_cast({status_bar, Msg}, State=#state{status_bar=SB}) ->
    wxStatusBar:setStatusText(SB, Msg),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_call({create_menus, TabMenus}, _From,
	    State = #state{menubar=MenuBar, menus=PrevTabMenus}) ->
    if TabMenus == PrevTabMenus -> ignore;
       true ->
	    wx:batch(fun() ->
			     clean_menus(PrevTabMenus, MenuBar),
			     observer_lib:create_menus(TabMenus, MenuBar, plugin)
		     end)
    end,
    {reply, ok, State#state{menus=TabMenus}};

handle_call({get_attrib, Attrib}, _From, State) ->
    {reply, get(Attrib), State};

handle_call(get_tracer, _From, State=#state{panels=Panels}) ->
    {_, TraceP, _} = lists:keyfind(trace_panel, 1, Panels),
    {reply, TraceP, State};

handle_call(get_active_node, _From, State=#state{node=Node}) ->
    {reply, Node, State};

handle_call(get_menubar, _From, State=#state{menubar=MenuBar}) ->
    {reply, MenuBar, State};

handle_call(stop, From, State) ->
    stop_servers(State),
    {noreply, State#state{reply_to=From}};

handle_call(log_status, _From, State) ->
    {reply, State#state.log, State};

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

handle_info({open_link, Id0}, State = #state{panels=Panels,frame=Frame}) ->
    Id = case Id0 of
	      [_|_] -> try list_to_pid(Id0) catch _:_ -> Id0 end;
	      _ -> Id0
	  end,
    %% Forward to process tab
    case Id of
	Pid when is_pid(Pid) ->
            {pro_panel, ProcViewer, _} = lists:keyfind(pro_panel, 1, Panels),
	    wx_object:get_pid(ProcViewer) ! {procinfo_open, Pid};
	"#Port" ++ _ = Port ->
            {port_panel, PortViewer, _} = lists:keyfind(port_panel, 1, Panels),
	    wx_object:get_pid(PortViewer) ! {portinfo_open, Port};
	_ ->
	    Msg = io_lib:format("Information about ~p is not available or implemented",[Id]),
	    Info = wxMessageDialog:new(Frame, Msg),
	    wxMessageDialog:showModal(Info),
	    wxMessageDialog:destroy(Info)
    end,
    {noreply, State};

handle_info({get_debug_info, From}, State = #state{notebook=Notebook, active_tab=Pid}) ->
    From ! {observer_debug, wx:get_env(), Notebook, Pid},
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal ->
	    {noreply, State};
	_ ->
	    io:format("Observer: Child (~s) crashed exiting:  ~p ~tp~n",
		      [pid2panel(Pid, State), Pid, Reason]),
	    {stop, normal, State}
    end;

handle_info({stop, Me}, State) when Me =:= self() ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

stop_servers(#state{node=Node, log=LogOn, panels=Panels} = _State) ->
    LogOn andalso rpc:block_call(Node, rb, stop, []),
    Me = self(),
    save_config(Panels),
    Stop = fun() ->
		   try
		       _ = [wx_object:stop(Panel) || {_, Panel, _} <- Panels],
		       ok
		   catch _:_ -> ok
		   end,
		   Me ! {stop, Me}
	   end,
    spawn(Stop).

terminate(_Reason, #state{frame = Frame, reply_to=From}) ->
    wxFrame:destroy(Frame),
    wx:destroy(),
    case From of
	false -> ignore;
	_ -> gen_server:reply(From, ok)
    end,
    ok.

load_config() ->
    case file:consult(config_file()) of
        {ok, Config} -> Config;
        _ -> []
    end.

save_config(Panels) ->
    Configs = [{Name, wx_object:call(Panel, get_config)} || {Name, Panel, _} <- Panels],
    File = config_file(),
    case filelib:ensure_dir(File) of
        ok ->
            Format = [io_lib:format("~tp.~n",[Conf]) || Conf <- Configs],
            _ = file:write_file(File, Format);
        _ ->
            ignore
    end.

config_file() ->
    Dir = filename:basedir(user_config, "erl_observer"),
    filename:join(Dir, "config.txt").

code_change(_, _, State) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_rpc(Node, Mod, Func, Args) ->
    case
	rpc:call(Node, Mod, Func, Args) of
	{badrpc, Reason} ->
	    error_logger:error_report([{node, Node},
				       {call, {Mod, Func, Args}},
				       {reason, {badrpc, Reason}}]),
	    observer ! {nodedown, Node},
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
    MD = wxMessageDialog:new(Frame, Msg, [{style, Style}, {caption,Title}]),
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

change_node_view(Node, #state{active_tab=Tab} = State) ->
    Tab ! not_active,
    Tab ! {active, Node},
    StatusText = ["Observer - " | atom_to_list(Node)],
    wxFrame:setTitle(State#state.frame, StatusText),
    wxStatusBar:setStatusText(State#state.status_bar, StatusText),
    State#state{node = Node}.

check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

pid2panel(Pid, #state{panels=Panels}) ->
    PanelPids = [{Name, wx_object:get_pid(Obj)} || {Name, Obj, _} <- Panels],
    case lists:keyfind(Pid, 2, PanelPids) of
        false -> "unknown";
        {Name,_} -> Name
    end.

create_connect_dialog(ping, #state{frame = Frame, prev_node=Prev}) ->
    Dialog = wxTextEntryDialog:new(Frame, "Connect to node", [{value, Prev}]),
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
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Distribute node",
			  [{style, ?wxDEFAULT_FRAME_STYLE bor ?wxRESIZE_BORDER}]),

    VSizer = wxBoxSizer:new(?wxVERTICAL),

    Choices = ["Short name", "Long name"],
    RadioBox = wxRadioBox:new(Dialog, 1, "", ?wxDefaultPosition, ?wxDefaultSize,
			      Choices, [{majorDim, 2}, {style, ?wxHORIZONTAL}]),

    NameText = wxStaticText:new(Dialog, ?wxID_ANY, "Node name: "),
    NameCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY, [{size, {300,-1}}]),
    wxTextCtrl:setValue(NameCtrl, "observer"),
    CookieText = wxStaticText:new(Dialog, ?wxID_ANY, "Secret cookie: "),
    CookieCtrl = wxTextCtrl:new(Dialog, ?wxID_ANY,[{style, ?wxTE_PASSWORD}]),

    BtnSizer = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    Dir = ?wxLEFT bor ?wxRIGHT bor ?wxDOWN,
    Flags = [{flag, ?wxEXPAND bor Dir bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}],
    wxSizer:add(VSizer, RadioBox, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, NameText, [{flag, ?wxLEFT}, {border, 5}]),
    wxSizer:add(VSizer, NameCtrl, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, CookieText, [{flag, ?wxLEFT}, {border, 5}]),
    wxSizer:add(VSizer, CookieCtrl, Flags),
    wxSizer:addSpacer(VSizer, 10),
    wxSizer:add(VSizer, BtnSizer, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL},{border, 5}]),

    wxWindow:setSizerAndFit(Dialog, VSizer),
    wxSizer:setSizeHints(VSizer, Dialog),
    {ok,[[HomeDir]]} = init:get_argument(home),
    CookiePath = filename:join(HomeDir, ".erlang.cookie"),
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
    CDV   = #create_menu{id = ?ID_CDV, text = "Examine Crashdump"},
    Quit  = #create_menu{id = ?wxID_EXIT, text = "Quit"},
    About = #create_menu{id = ?wxID_ABOUT, text = "About"},
    Help  = #create_menu{id = ?wxID_HELP},
    FileMenu = {"File", [CDV, Quit]},
    NodeMenu = case erlang:is_alive() of
		   true ->  {"Nodes", NodesMenuItems ++
				 [#create_menu{id = ?ID_PING, text = "Connect Node"}]};
		   false -> {"Nodes", NodesMenuItems ++
				 [#create_menu{id = ?ID_CONNECT, text = "Enable distribution"}]}
	       end,
    LogMenu =  {"Log", [#create_menu{id = ?ID_LOGVIEW, text = "Toggle log view"}]},
    case os:type() =:= {unix, darwin} of
	false ->
	    FileMenu = {"File", [CDV, Quit]},
	    HelpMenu = {"Help", [About,Help]},
	    [FileMenu, NodeMenu, LogMenu, HelpMenu];
	true ->
	    %% On Mac quit and about will be moved to the "default' place
	    %% automagicly, so just add them to a menu that always exist.
	    %% But not to the help menu for some reason

	    {Tag, Menus} = FileMenu,
	    [{Tag, Menus ++ [Quit,About]}, NodeMenu, LogMenu, {"&Help", [Help]}]
    end.

clean_menus(Menus, MenuBar) ->
    remove_menu_items(Menus, MenuBar).

remove_menu_items([{MenuStr = "File", Menus}|Rest], MenuBar) ->
    case wxMenuBar:findMenu(MenuBar, MenuStr) of
	?wxNOT_FOUND ->
	    remove_menu_items(Rest, MenuBar);
	MenuId ->
	    Menu = wxMenuBar:getMenu(MenuBar, MenuId),
	    Items = [wxMenu:findItem(Menu, Tag) || #create_menu{text=Tag} <- Menus],
	    [wxMenu:delete(Menu, MItem) || MItem <- Items],
	    remove_menu_items(Rest, MenuBar)
    end;
remove_menu_items([{"Nodes", _}|_], _MB) ->
    ok;
remove_menu_items([{Tag, _Menus}|Rest], MenuBar) ->
    case wxMenuBar:findMenu(MenuBar, Tag) of
	?wxNOT_FOUND ->
	    remove_menu_items(Rest, MenuBar);
	MenuId ->    
	    Menu = wxMenuBar:getMenu(MenuBar, MenuId),
	    wxMenuBar:remove(MenuBar, MenuId),
	    Items = wxMenu:getMenuItems(Menu),
	    [wxMenu:'Destroy'(Menu, Item) || Item <- Items],
	    wxMenu:destroy(Menu),
	    remove_menu_items(Rest, MenuBar)
    end;
remove_menu_items([], _MB) ->
    ok.

get_nodes() ->
    Nodes0 = case erlang:is_alive() of
		false -> [];
		true  ->
		    case net_adm:names() of
			{error, _} -> nodes();
			{ok, Names} ->
			    epmd_nodes(Names) ++ nodes()
		    end
	     end,
    Nodes = lists:usort(Nodes0),
    {_, Menues} =
	lists:foldl(fun(Node, {Id, Acc}) when Id < ?LAST_NODES_MENU_ID ->
			    {Id + 1, [#create_menu{id=Id + ?FIRST_NODES_MENU_ID,
						   text=atom_to_list(Node)} | Acc]}
		    end, {1, []}, Nodes),
    {Nodes, lists:reverse(Menues)}.

epmd_nodes(Names) ->
    [_, Host] = string:lexemes(atom_to_list(node()),"@"),
    [list_to_atom(Name ++ [$@|Host]) || {Name, _} <- Names].

update_node_list(State = #state{menubar=MenuBar}) ->
    {Nodes, NodesMenuItems} = get_nodes(),
    NodeMenu = case wxMenuBar:findMenu(MenuBar, "Nodes") of
		   ?wxNOT_FOUND -> 
		       Menu = wxMenu:new(),
		       wxMenuBar:append(MenuBar, Menu, "Nodes"),
		       Menu;
		   NodeMenuId ->
		       Menu = wxMenuBar:getMenu(MenuBar, NodeMenuId),
		       wx:foreach(fun(Item) -> wxMenu:'Destroy'(Menu, Item) end,
				  wxMenu:getMenuItems(Menu)),
		       Menu
	       end,
	
    Index = wx:foldl(fun(Record, Index) ->
			     observer_lib:create_menu_item(Record, NodeMenu, Index)
		     end, 0, NodesMenuItems),
    
    Dist = case erlang:is_alive() of
	       true  -> #create_menu{id = ?ID_PING, text = "Connect node"};
	       false -> #create_menu{id = ?ID_CONNECT, text = "Enable distribution"}
	   end,
    observer_lib:create_menu_item(Dist, NodeMenu, Index),
    State#state{nodes = Nodes}.

ensure_sasl_started(Node) ->
   %% is sasl started ?
   Apps = rpc:block_call(Node, application, which_applications, []),
   case lists:keyfind(sasl, 1, Apps) of
       false        ->  throw("Error: sasl application not started."),
                        error;
       {sasl, _, _} ->  ok
   end.

ensure_mf_h_handler_used(Node) ->
   %% is log_mf_h used ?
   Handlers =
        case rpc:block_call(Node, gen_event, which_handlers, [error_logger]) of
            {badrpc,{'EXIT',noproc}} -> []; % OTP-21+ and no event handler exists
            Hs -> Hs
        end,
   case lists:any(fun(L)-> L == log_mf_h end, Handlers) of
       false -> throw("Error: log_mf_h handler not used in sasl."),
                error;
       true  -> ok
   end.

ensure_rb_mode(Node, PrevLog) ->
    ok = ensure_rb_module_loaded(Node),
    ok = is_rb_compatible(Node),
    ok = is_rb_server_running(Node, PrevLog),
    ok.


ensure_rb_module_loaded(Node) ->
   %% Need to ensure that module is loaded in order to detect exported
   %% functions on interactive nodes
   case rpc:block_call(Node, code, ensure_loaded, [rb]) of
       {badrpc, Reason} ->
	   throw("Error: badrpc - " ++ io_lib:format("~tp",[Reason]));
       {error, Reason} ->
	   throw("Error: rb module load error - " ++ io_lib:format("~tp",[Reason]));
       {module,rb} ->
	   ok
   end.

is_rb_compatible(Node) ->
   %% Simply test that rb:log_list/0 is exported
   case rpc:block_call(Node, erlang, function_exported, [rb, log_list, 0]) of
       false -> throw("Error: Node's Erlang release must be at least R16B02.");
       true  -> ok
   end.

is_rb_server_running(Node, LogState) ->
   %% If already started, somebody else may use it.
   %% We cannot use it too, as far log file would be overriden. Not fair.
   case rpc:block_call(Node, erlang, whereis, [rb_server]) of
       Pid when is_pid(Pid), (LogState == false) ->
	   throw("Error: rb_server is already started and maybe used by someone.");
       Pid when is_pid(Pid) ->
	   ok;
       undefined ->
	   ok
   end.
