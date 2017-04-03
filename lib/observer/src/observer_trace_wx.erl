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

-module(observer_trace_wx).

-export([start_link/3, add_processes/1, add_ports/1]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(SAVE_TRACEOPTS, 305).
-define(LOAD_TRACEOPTS, 306).
-define(TOGGLE_TRACE, 307).
-define(ADD_NEW_PROCS, 308).
-define(ADD_NEW_PORTS, 309).
-define(ADD_TP, 310).
-define(TRACE_OUTPUT, 311).
-define(DEF_MS_FUNCS,  312).
-define(DEF_MS_SEND,  313).
-define(DEF_MS_RECV,  314).
-define(DEF_PROC_OPTS,  315).
-define(DEF_PORT_OPTS,  316).

-define(NODES_WIN, 330).
-define(ADD_NODES, 331).
-define(REMOVE_NODES, 332).

-define(PROC_WIN, 340).
-define(EDIT_PROCS, 341).
-define(REMOVE_PROCS, 342).

-define(PORT_WIN, 350).
-define(EDIT_PORTS, 351).
-define(REMOVE_PORTS, 352).

-define(MODULES_WIN, 360).
-define(REMOVE_MOD_MS, 361).

-define(FUNCS_WIN, 370).
-define(EDIT_FUNCS_MS, 371).
-define(REMOVE_FUNCS_MS, 372).

-define(LOG_WIN, 380).
-define(LOG_SAVE, 381).
-define(LOG_CLEAR, 382).

-define(NO_NODES_HELP,"Right click to add nodes").
-define(NODES_HELP,"Select nodes to see traced processes and ports").
-define(NO_P_HELP,"Add items from Processes/Ports tab").
-define(P_HELP,"Select nodes to see traced processes and ports").
-define(NO_TP_HELP,"Add trace pattern with button below").
-define(TP_HELP,"Select module to see trace patterns").

-record(state,
	{parent,
	 panel,
	 n_view, proc_view, port_view, m_view, f_view,  %% The listCtrl's
	 logwin, %% The latest log window
	 nodes = [],
	 toggle_button,
	 tpids = [],  % #titem
	 tports = [], % #titem
	 def_proc_flags = [],
	 def_port_flags = [],
	 output = [],
	 tpatterns = dict:new(), % Key =:= Module::atom, Value =:= {M, F, A, MatchSpec}
	 match_specs = []}). % [ #match_spec{} ]

-record(titem, {id, opts}).

start_link(Notebook, ParentPid, Config) ->
    wx_object:start_link(?MODULE, [Notebook, ParentPid, Config], []).

add_processes(Pids) when is_list(Pids) ->
    wx_object:cast(observer_wx:get_tracer(), {add_processes, Pids}).

add_ports(Ports) when is_list(Ports) ->
    wx_object:cast(observer_wx:get_tracer(), {add_ports, Ports}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, ParentPid, Config]) ->
    wx:batch(fun() -> create_window(Notebook, ParentPid, Config) end).

create_window(Notebook, ParentPid, Config) ->
    %% Create the window
    Panel = wxPanel:new(Notebook, [{size, wxWindow:getClientSize(Notebook)}]),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Splitter = wxSplitterWindow:new(Panel, [{size, wxWindow:getClientSize(Panel)},
					    {style, ?SASH_STYLE}]),
    {NodeProcView, NodeView, ProcessView, PortView} =
	create_proc_port_view(Splitter),
    {MatchSpecView,ModView,FuncView} = create_matchspec_view(Splitter),
    wxSplitterWindow:setSashGravity(Splitter, 0.5),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSplitterWindow:splitHorizontally(Splitter, NodeProcView, MatchSpecView,
				       [{sashPosition,368}]),
    wxSizer:add(Sizer, Splitter, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}, {proportion, 1}]),
    %% Buttons
    Buttons = wxBoxSizer:new(?wxHORIZONTAL),
    ToggleButton = wxToggleButton:new(Panel, ?TOGGLE_TRACE, "Start Trace", []),
    wxSizer:add(Buttons, ToggleButton, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:addSpacer(Buttons, 15),
    wxSizer:add(Buttons, wxButton:new(Panel, ?ADD_NODES, [{label, "Add Nodes"}])),
    wxSizer:add(Buttons, wxButton:new(Panel, ?ADD_NEW_PROCS, [{label, "Add 'new' Processes"}])),
    wxSizer:add(Buttons, wxButton:new(Panel, ?ADD_NEW_PORTS, [{label, "Add 'new' Ports"}])),
    wxSizer:add(Buttons, wxButton:new(Panel, ?ADD_TP, [{label, "Add Trace Pattern"}])),
    wxMenu:connect(Panel, command_togglebutton_clicked, [{skip, true}]),
    wxMenu:connect(Panel, command_button_clicked, [{skip, true}]),
    wxSizer:add(Sizer, Buttons, [{flag, ?wxLEFT bor ?wxRIGHT bor ?wxDOWN},
				 {border, 5}, {proportion,0}]),
    wxWindow:setSizer(Panel, Sizer),
    MS = parse_ms(maps:get(match_specs, Config, []), default_matchspecs()),
    {Panel, #state{parent=ParentPid, panel=Panel,
		   n_view=NodeView, proc_view=ProcessView, port_view=PortView,
		   m_view=ModView, f_view=FuncView,
		   toggle_button = ToggleButton,
                   output=maps:get(output, Config, []),
                   def_proc_flags=maps:get(procflags, Config, []),
                   def_port_flags=maps:get(portflags, Config, []),
                   match_specs=MS
                  }}.

default_matchspecs() ->
    [{Key,default_matchspecs(Key)} || Key <- [funcs,send,'receive']].
default_matchspecs(Key) ->
    Ms = get_default_matchspecs(Key),
    [make_ms(Name,Term,FunStr) || {Name,Term,FunStr} <- Ms].

get_default_matchspecs(funcs) ->
    [{"Return Trace", [{'_', [], [{return_trace}]}],
      "fun(_) -> return_trace() end"},
     {"Exception Trace", [{'_', [], [{exception_trace}]}],
      "fun(_) -> exception_trace() end"},
     {"Message Caller", [{'_', [], [{message,{caller}}]}],
      "fun(_) -> message(caller()) end"},
     {"Message Dump", [{'_', [], [{message,{process_dump}}]}],
      "fun(_) -> message(process_dump()) end"}];
get_default_matchspecs(send) ->
    [{"To local node", [{['$1','_'], [{'==',{node,'$1'},{node}}], []}],
      "fun([Pid,_]) when node(Pid)==node() ->\n    true\nend"},
     {"To remote node", [{['$1','_'], [{'=/=',{node,'$1'},{node}}], []}],
      "fun([Pid,_]) when node(Pid)=/=node() ->\n    true\nend"}];
get_default_matchspecs('receive') ->
    [{"From local node", [{['$1','_','_'], [{'==','$1',{node}}], []}],
      "fun([Node,_,_]) when Node==node() ->\n    true\nend"},
     {"From remote node", [{['$1','_','_'], [{'=/=','$1',{node}}], []}],
      "fun([Node,_,_]) when Node=/=node() ->\n    true\nend"}].


create_proc_port_view(Parent) ->
    Panel  = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Style = ?wxLC_REPORT bor ?wxLC_HRULES,
    Splitter = wxSplitterWindow:new(Panel, [{style, ?SASH_STYLE}]),
    Nodes = wxListCtrl:new(Splitter, [{winid, ?NODES_WIN}, {style, Style}]),
    ProcsPortsSplitter = wxSplitterWindow:new(Splitter, [{style, ?SASH_STYLE}]),
    Procs = wxListCtrl:new(ProcsPortsSplitter, [{winid,?PROC_WIN},{style,Style}]),
    Ports = wxListCtrl:new(ProcsPortsSplitter, [{winid,?PORT_WIN},{style,Style}]),
    Li = wxListItem:new(),
    wxListItem:setText(Li, "Nodes"),
    wxListCtrl:insertColumn(Nodes, 0, Li),

    AddProc = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Procs, Col, Li),
			   wxListCtrl:setColumnWidth(Procs, Col, DefSize),
			   Col + 1
		   end,
    ProcListItems = [{"Process Id",    ?wxLIST_FORMAT_CENTER,  120},
		     {"Trace Options", ?wxLIST_FORMAT_LEFT, 300}],
    lists:foldl(AddProc, 0, ProcListItems),

    AddPort = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Ports, Col, Li),
			   wxListCtrl:setColumnWidth(Ports, Col, DefSize),
			   Col + 1
		   end,
    PortListItems = [{"Port Id",    ?wxLIST_FORMAT_CENTER,  120},
		     {"Trace Options", ?wxLIST_FORMAT_LEFT, 300}],
    lists:foldl(AddPort, 0, PortListItems),

    wxListItem:destroy(Li),

    wxSplitterWindow:setSashGravity(Splitter, 0.0),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSplitterWindow:splitVertically(Splitter, Nodes, ProcsPortsSplitter,
				     [{sashPosition, 155}]),
    wxSizer:add(MainSz, Splitter, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSplitterWindow:setSashGravity(ProcsPortsSplitter, 0.5),
    wxSplitterWindow:setMinimumPaneSize(ProcsPortsSplitter,50),
    wxSplitterWindow:splitHorizontally(ProcsPortsSplitter, Procs, Ports,
				       [{sashPosition, 182}]),

    wxListCtrl:connect(Procs, command_list_item_right_click),
    wxListCtrl:connect(Ports, command_list_item_right_click),
    wxListCtrl:connect(Nodes, command_list_item_right_click),
    wxListCtrl:connect(Nodes, command_list_item_selected),
    wxListCtrl:connect(Procs, size, [{skip, true}]),
    wxListCtrl:connect(Ports, size, [{skip, true}]),
    wxListCtrl:connect(Nodes, size, [{skip, true}]),

    wxListCtrl:setToolTip(Nodes, ?NO_NODES_HELP),
    wxListCtrl:setToolTip(Procs, ?NO_P_HELP),
    wxListCtrl:setToolTip(Ports, ?NO_P_HELP),

    wxPanel:setSizer(Panel, MainSz),
    wxWindow:setFocus(Procs),
    {Panel, Nodes, Procs, Ports}.

create_matchspec_view(Parent) ->
    Panel  = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Style = ?wxLC_REPORT bor ?wxLC_HRULES,
    Splitter = wxSplitterWindow:new(Panel, [{style, ?SASH_STYLE}]),
    Modules = wxListCtrl:new(Splitter, [{winid, ?MODULES_WIN},
					{style, Style  bor ?wxLC_SINGLE_SEL}]),
    Funcs   = wxListCtrl:new(Splitter, [{winid, ?FUNCS_WIN}, {style, Style}]),
    Li = wxListItem:new(),

    wxListItem:setText(Li, "Modules"),
    wxListCtrl:insertColumn(Modules, 0, Li),
    wxListItem:setText(Li, "Functions"),
    wxListCtrl:insertColumn(Funcs, 0, Li),
    wxListCtrl:setColumnWidth(Funcs, 0, 150),
    wxListItem:setText(Li, "Match Spec"),
    wxListCtrl:insertColumn(Funcs, 1, Li),
    wxListCtrl:setColumnWidth(Funcs, 1, 300),
    wxListItem:destroy(Li),

    wxSplitterWindow:setSashGravity(Splitter, 0.0),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSplitterWindow:splitVertically(Splitter, Modules, Funcs, [{sashPosition, 155}]),
    wxSizer:add(MainSz, Splitter,   [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxListCtrl:connect(Modules, size, [{skip, true}]),
    wxListCtrl:connect(Funcs,   size, [{skip, true}]),
    wxListCtrl:connect(Modules, command_list_item_selected),
    wxListCtrl:connect(Modules, command_list_item_right_click),
    wxListCtrl:connect(Funcs, command_list_item_right_click),
    wxListCtrl:setToolTip(Panel, ?NO_TP_HELP),
    wxPanel:setSizer(Panel, MainSz),
    {Panel, Modules, Funcs}.

create_menues(Parent) ->
    Menus = [{"File",
	      [#create_menu{id = ?LOAD_TRACEOPTS, text = "Load settings"},
	       #create_menu{id = ?SAVE_TRACEOPTS, text = "Save settings"}]},
	     {"Options",
	      [#create_menu{id = ?TRACE_OUTPUT, text = "Output"},
	       #create_menu{id = ?DEF_MS_FUNCS, text = "Default Match Specifications for Functions"},
	       #create_menu{id = ?DEF_MS_SEND, text = "Default Match Specifications for 'send'"},
	       #create_menu{id = ?DEF_MS_RECV, text = "Default Match Specifications for 'receive'"},
	       #create_menu{id = ?DEF_PROC_OPTS, text = "Default Process Options"},
	       #create_menu{id = ?DEF_PORT_OPTS, text = "Default Port Options"}]}
	    ],
    observer_wx:create_menus(Parent, Menus).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Main window
handle_event(#wx{obj=Obj, event=#wxSize{size={W,_}}}, State) ->
    case wx:getObjectType(Obj) =:= wxListCtrl of
	true ->  observer_lib:set_listctrl_col_size(Obj, W);
	false -> ok
    end,
    {noreply, State};

handle_event(#wx{id=?ADD_NEW_PROCS}, State = #state{panel=Parent, def_proc_flags=TraceOpts}) ->
    try
	Opts = observer_traceoptions_wx:process_trace(Parent, TraceOpts),
	Process = #titem{id=new_processes, opts=Opts},
	{noreply, do_add_processes([Process], State#state{def_proc_flags=Opts})}
    catch cancel -> {noreply, State}
    end;

handle_event(#wx{id=?ADD_NEW_PORTS}, State = #state{panel=Parent, def_port_flags=TraceOpts}) ->
    try
	Opts = observer_traceoptions_wx:port_trace(Parent, TraceOpts),
	Port = #titem{id=new_ports, opts=Opts},
	{noreply, do_add_ports([Port], State#state{def_port_flags=Opts})}
    catch cancel -> {noreply, State}
    end;

handle_event(#wx{id=?ADD_TP},
	     State = #state{panel=Parent, nodes=Nodes, match_specs=Ms}) ->
    Node = case Nodes of
	       [N|_] -> N;
	       [] -> node()
	   end,
    case observer_traceoptions_wx:trace_pattern(self(), Parent, Node, Ms) of
	cancel ->
	    {noreply, State};
	Patterns ->
	    {noreply, do_add_patterns(Patterns, State)}
    end;

handle_event(#wx{id=?MODULES_WIN, event=#wxList{type=command_list_item_selected, itemIndex=Row}},
	     State = #state{tpatterns=TPs, m_view=Mview, f_view=Fview}) ->
    Module = list_to_atom(wxListCtrl:getItemText(Mview, Row)),
    update_functions_view(dict:fetch(Module, TPs), Fview),
    {noreply, State};

handle_event(#wx{id=?NODES_WIN,
		 event=#wxList{type=command_list_item_selected}},
	     State = #state{tpids=Tpids, tports=Tports, n_view=Nview,
			    proc_view=ProcView, port_view=PortView, nodes=Ns}) ->
    Nodes = get_selected_items(Nview, Ns),
    update_p_view(Tpids, ProcView, Nodes),
    update_p_view(Tports, PortView, Nodes),
    {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}},
	     #state{panel = Panel,
		    nodes = Nodes,
		    tpids = TProcs,
		    tports = TPorts,
		    tpatterns = TPs0,
		    toggle_button = ToggleBtn,
		    output = Opts
		   } = State) ->
    try
	TPs = dict:to_list(TPs0),
	(TProcs == []) andalso (TPorts == []) andalso throw({error, "No processes or ports traced"}),
	(Nodes == []) andalso throw({error, "No nodes traced"}),
	HaveCallTrace = fun(#titem{opts=Os}) -> lists:member(functions,Os) end,
	WStr = "Call trace actived but no trace patterns used",
	(TPs == []) andalso lists:any(HaveCallTrace, TProcs) andalso
	    observer_wx:create_txt_dialog(Panel, WStr, "Warning", ?wxICON_WARNING),

	{TTB, LogWin}  = ttb_output_args(Panel, Opts),
	{ok, _} = ttb:tracer(Nodes, TTB),
	setup_ttb(TPs, TProcs, TPorts),
	wxToggleButton:setLabel(ToggleBtn, "Stop Trace"),
	{noreply, State#state{logwin=LogWin}}
    catch {error, Msg} ->
	    observer_wx:create_txt_dialog(Panel, Msg, "Error", ?wxICON_ERROR),
	    wxToggleButton:setValue(ToggleBtn, false),
	    {noreply, State}
    end;

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}},
	     #state{toggle_button = ToggleBtn} = State) ->
    %%Stop tracing
    ttb:stop(nofetch),
    wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
    wxToggleButton:setValue(ToggleBtn, false),
    {noreply, State#state{logwin=false}};

handle_event(#wx{id=Id, obj=LogWin, event=Ev},
	     #state{toggle_button = ToggleBtn, logwin=Latest} = State)
  when Id =:= ?LOG_WIN; is_record(Ev, wxClose) ->
    case LogWin of
	Latest ->
	    %%Stop tracing
	    ttb:stop(nofetch),
	    wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
	    wxToggleButton:setValue(ToggleBtn, false),
	    {noreply, State#state{logwin=false}};
	_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?LOG_CLEAR, userData=TCtrl}, State) ->
    wxTextCtrl:clear(TCtrl),
    {noreply, State};

handle_event(#wx{id=?LOG_SAVE, userData=TCtrl}, #state{panel=Panel} = State) ->
    Dialog = wxFileDialog:new(Panel, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    wxDialog:destroy(Dialog),
	    wxTextCtrl:saveFile(TCtrl, [{file, Path}]);
	_ ->
	    wxDialog:destroy(Dialog),
	    ok
    end,
    {noreply, State};

handle_event(#wx{id = ?SAVE_TRACEOPTS},
	     #state{panel = Panel} = State) ->
    Dialog = wxFileDialog:new(Panel, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    write_file(Panel, Path, get_config(State));
	_ ->
	    ok
    end,
    wxDialog:destroy(Dialog),
    {noreply, State};


handle_event(#wx{id = ?LOAD_TRACEOPTS}, #state{panel = Panel} = State) ->
    Dialog = wxFileDialog:new(Panel, [{style, ?wxFD_FILE_MUST_EXIST}]),
    State2 = case wxFileDialog:showModal(Dialog) of
		 ?wxID_OK ->
		     Path = wxFileDialog:getPath(Dialog),
		     read_settings(Path, State);
		 _ ->
		     State
	     end,
    wxDialog:destroy(Dialog),
    {noreply, State2};

handle_event(#wx{id=?PROC_WIN, event=#wxList{type=command_list_item_right_click}},
	     State = #state{panel=Panel, proc_view=LCtrl, tpids=Tpids,
			    n_view=Nview, nodes=Nodes}) ->
    case get_visible_ps(Tpids, Nodes, Nview) of
	[] ->
	    ok;
	Visible ->
	    case get_selected_items(LCtrl, Visible) of
		[] ->
		    ok;
		_ ->
		    create_right_click_menu(
		      Panel,
		      [{?EDIT_PROCS, "Edit process options"},
		       {?REMOVE_PROCS, "Remove processes"}])
	    end
    end,
    {noreply, State};

handle_event(#wx{id=?PORT_WIN, event=#wxList{type=command_list_item_right_click}},
	     State = #state{panel=Panel, port_view=LCtrl, tports=Tports,
			    n_view=Nview, nodes=Nodes}) ->
    case get_visible_ps(Tports, Nodes, Nview) of
	[] ->
	    ok;
	Visible ->
	    case get_selected_items(LCtrl, Visible) of
		[] ->
		    ok;
		_ ->
		    create_right_click_menu(
		      Panel,
		      [{?EDIT_PORTS, "Edit port options"},
		       {?REMOVE_PORTS, "Remove ports"}])
	    end
    end,
    {noreply, State};

handle_event(#wx{id=?MODULES_WIN,event=#wxList{type=command_list_item_right_click}},
	     State = #state{panel=Panel, m_view=Mview, tpatterns=TPs}) ->
    case get_selected_items(Mview, lists:sort(dict:fetch_keys(TPs))) of
	[] ->
	    ok;
	_ ->
	    create_right_click_menu(
	      Panel,
	      [{?REMOVE_MOD_MS, "Remove trace patterns"}])
    end,
    {noreply,State};

handle_event(#wx{id=?FUNCS_WIN,event=#wxList{type=command_list_item_right_click}},
	     State = #state{panel=Panel, m_view=Mview, f_view=Fview,
			    tpatterns=TPs}) ->
    case get_selected_items(Mview, lists:sort(dict:fetch_keys(TPs))) of
	[] ->
	    ok;
	[Module] ->
	    case get_selected_items(Fview, dict:fetch(Module, TPs)) of
		[] ->
		    ok;
		_ ->
		    create_right_click_menu(
		      Panel,
		      [{?EDIT_FUNCS_MS, "Edit matchspecs"},
		       {?REMOVE_FUNCS_MS, "Remove trace patterns"}])
	    end
    end,
    {noreply,State};

handle_event(#wx{id=?NODES_WIN,event=#wxList{type=command_list_item_right_click}},
	     State = #state{panel=Panel, n_view=Nview, nodes=Nodes}) ->
    Menu =
	case get_selected_items(Nview, Nodes) of
	    [] ->
		[{?ADD_NODES, "Add nodes"}];
	    _ ->
		[{?ADD_NODES, "Add nodes"},
		 {?REMOVE_NODES, "Remove nodes"}]
	end,
    create_right_click_menu(Panel,Menu),
    {noreply, State};

handle_event(#wx{id=?EDIT_PROCS}, #state{panel=Panel, tpids=Tpids, proc_view=Procs} = State) ->
    try
	[#titem{opts=DefOpts}|_] = Selected = get_selected_items(Procs, Tpids),
	Opts = observer_traceoptions_wx:process_trace(Panel, DefOpts),
	Changed = [Tpid#titem{opts=Opts} || Tpid <- Selected],
	{noreply, do_add_processes(Changed, State#state{def_proc_flags=Opts})}
    catch _:_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?REMOVE_PROCS},
	     #state{tpids=Tpids, proc_view=LCtrl,
		    n_view=Nview, nodes=Nodes} = State) ->
    Selected = get_selected_items(LCtrl, Tpids),
    Pids = Tpids -- Selected,
    update_p_view(Pids, LCtrl, Nodes, Nview),
    {noreply, State#state{tpids=Pids}};

handle_event(#wx{id=?EDIT_PORTS}, #state{panel=Panel, tports=Tports, port_view=Ports} = State) ->
    try
	[#titem{opts=DefOpts}|_] = Selected = get_selected_items(Ports, Tports),
	Opts = observer_traceoptions_wx:port_trace(Panel, DefOpts),
	Changed = [Tport#titem{opts=Opts} || Tport <- Selected],
	{noreply, do_add_ports(Changed, State#state{def_port_flags=Opts})}
    catch _:_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?REMOVE_PORTS},
	     #state{tports=Tports, port_view=LCtrl,
		    n_view=Nview, nodes=Nodes} = State) ->
    Selected = get_selected_items(LCtrl, Tports),
    Ports = Tports -- Selected,
    update_p_view(Ports, LCtrl, Nodes, Nview),
    {noreply, State#state{tports=Ports}};

handle_event(#wx{id=?DEF_PROC_OPTS}, #state{panel=Panel, def_proc_flags=PO} = State) ->
    try
	Opts = observer_traceoptions_wx:process_trace(Panel, PO),
	{noreply, State#state{def_proc_flags=Opts}}
    catch _:_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?DEF_PORT_OPTS}, #state{panel=Panel, def_port_flags=PO} = State) ->
    try
	Opts = observer_traceoptions_wx:port_trace(Panel, PO),
	{noreply, State#state{def_port_flags=Opts}}
    catch _:_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?DEF_MS_FUNCS}, #state{panel=Panel, match_specs=Ms} = State) ->
    try %% Return selected MS and sends new MS's to us
	observer_traceoptions_wx:select_matchspec(self(), Panel, Ms, funcs)
    catch _:_ ->
	    cancel
    end,
    {noreply, State};

handle_event(#wx{id=?DEF_MS_SEND}, #state{panel=Panel, match_specs=Ms} = State) ->
    try %% Return selected MS and sends new MS's to us
	observer_traceoptions_wx:select_matchspec(self(), Panel, Ms, send)
    catch _:_ ->
	    cancel
    end,
    {noreply, State};

handle_event(#wx{id=?DEF_MS_RECV}, #state{panel=Panel, match_specs=Ms} = State) ->
    try %% Return selected MS and sends new MS's to us
	observer_traceoptions_wx:select_matchspec(self(), Panel, Ms, 'receive')
    catch _:_ ->
	    cancel
    end,
    {noreply, State};

handle_event(#wx{id=?EDIT_FUNCS_MS}, #state{panel=Panel, tpatterns=TPs,
					    f_view=LCtrl, m_view=Mview,
					    match_specs=Mss
					   } = State) ->
    try
	case get_selected_items(Mview, lists:sort(dict:fetch_keys(TPs))) of
	    [] ->
		throw({error,"No module selected"});
	    [Module] ->
		Selected = get_selected_items(LCtrl, dict:fetch(Module, TPs)),
		Key = case Module of
			  'Events' ->
			      SelectedEvents =
				  [Event || #tpattern{fa=Event} <- Selected],
			      E1 = hd(SelectedEvents),
			      case lists:all(fun(E) when E==E1 -> true;
						(_) -> false
					     end,
					     SelectedEvents) of
				  true -> E1;
				  false -> throw({error,"Can not set match specs for multiple event types"})
			      end;
			  _ -> funcs
		      end,
		Ms = observer_traceoptions_wx:select_matchspec(self(), Panel,
							       Mss, Key),
		Changed = [TP#tpattern{ms=Ms} || TP <- Selected],
		{noreply, do_add_patterns({Module, Changed}, State)}
	end
    catch {error, Msg} ->
	    observer_wx:create_txt_dialog(Panel, Msg, "Error", ?wxICON_ERROR),
	    {noreply, State};
	  cancel ->
	    {noreply, State}
    end;

handle_event(#wx{id=?REMOVE_FUNCS_MS}, #state{tpatterns=TPs0, f_view=LCtrl, m_view=Mview} = State) ->
    case get_selected_items(Mview, lists:sort(dict:fetch_keys(TPs0))) of
	[] -> {noreply, State};
	[Module] ->
	    FMs0 = dict:fetch(Module, TPs0),
	    Selected = get_selected_items(LCtrl, FMs0),
	    FMs = FMs0 -- Selected,
	    update_functions_view(FMs, LCtrl),
	    TPs = case FMs of
		      [] ->
			  New = dict:erase(Module, TPs0),
			  update_modules_view(lists:sort(dict:fetch_keys(New)), Module, Mview),
			  New;
		      _ ->
			  dict:store(Module, FMs, TPs0)
		  end,
	    {noreply, State#state{tpatterns=TPs}}
    end;

handle_event(#wx{id=?REMOVE_MOD_MS}, #state{tpatterns=TPs0, f_view=LCtrl, m_view=Mview} = State) ->
    case get_selected_items(Mview, lists:sort(dict:fetch_keys(TPs0))) of
	[] -> {noreply, State};
	[Module] ->
	    update_functions_view([], LCtrl),
	    TPs = dict:erase(Module, TPs0),
	    update_modules_view(lists:sort(dict:fetch_keys(TPs)), Module, Mview),
	    {noreply, State#state{tpatterns=TPs}}
    end;

handle_event(#wx{id=?TRACE_OUTPUT}, #state{panel=Panel, output=Out0} = State) ->
    try
	Out = observer_traceoptions_wx:output(Panel, Out0),
	{noreply, State#state{output=Out}}
    catch _:_ ->
	    {noreply, State}
    end;

handle_event(#wx{id=?ADD_NODES}, #state{panel=Panel, n_view=Nview, nodes=Ns0} = State) ->
    try
	Possible = [node()|nodes()] -- Ns0,
	case Possible of
	    [] ->
		Msg = "Already selected all connected nodes\n"
		    "Use the Nodes menu to connect to new nodes first.",
		observer_wx:create_txt_dialog(Panel, Msg, "No available nodes", ?wxICON_INFORMATION),
		throw(cancel);
	    _ ->
		Ns = lists:usort(Ns0 ++ observer_traceoptions_wx:select_nodes(Panel, Possible)),
		update_nodes_view(Ns, Nview),
		{noreply, State#state{nodes=Ns}}
	end
    catch cancel ->
	    {noreply, State}
    end;

handle_event(#wx{id=?REMOVE_NODES}, #state{n_view=Nview, nodes=Ns0} = State) ->
    Sel = get_selected_items(Nview, Ns0),
    Ns = Ns0 -- Sel,
    update_nodes_view(Ns, Nview),
    {noreply, State#state{nodes = Ns}};

handle_event(#wx{id=ID, event = What}, State) ->
    io:format("~p:~p: Unhandled event: ~p, ~p ~n", [?MODULE, ?LINE, ID, What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(get_config, _, State) ->
    Config0 = get_config(State),
    Config = lists:keydelete(trace_p, 1, Config0),
    {reply, maps:from_list(Config), State};
handle_call(Msg, From, _State) ->
    error({unhandled_call, Msg, From}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({add_processes, Pids}, State = #state{panel=Parent, def_proc_flags=TraceOpts}) ->
    try
	Opts = observer_traceoptions_wx:process_trace(Parent, TraceOpts),
	POpts = [#titem{id=Pid, opts=Opts} || Pid <- Pids],
	S = do_add_processes(POpts, State#state{def_proc_flags=Opts}),
	{noreply, S}
    catch cancel ->
	    {noreply, State}
    end;
handle_cast({add_ports, Ports}, State = #state{panel=Parent, def_port_flags=TraceOpts}) ->
    try
	Opts = observer_traceoptions_wx:port_trace(Parent, TraceOpts),
	POpts = [#titem{id=Id, opts=Opts} || Id <- Ports],
	S = do_add_ports(POpts, State#state{def_port_flags=Opts}),
	{noreply, S}
    catch cancel ->
	    {noreply, State}
    end;
handle_cast(Msg, _State) ->
    error({unhandled_cast, Msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({active, _Node}, State=#state{parent=Parent}) ->
    create_menues(Parent),
    {noreply, State};

handle_info(not_active, State) ->
    {noreply, State};

handle_info({update_ms, NewMs}, State) ->
    {noreply, State#state{match_specs=NewMs}};

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.

terminate(_Reason, #state{nodes=_Nodes}) ->
    ttb:stop(nofetch),
    ok.

code_change(_, _, State) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_add_patterns({Module, NewPs}, State=#state{tpatterns=TPs0, m_view=Mview, f_view=Fview}) ->
    Old = case dict:find(Module, TPs0) of
	      {ok, Prev}  -> Prev;
	      error -> []
	  end,
    case merge_patterns(NewPs, Old) of
	{Old, [], []} ->
	    State;
	{MPatterns, _New, _Changed} ->
	    %% if dynamicly updates update New and Changed
	    TPs = dict:store(Module, MPatterns, TPs0),
	    update_modules_view(lists:sort(dict:fetch_keys(TPs)), Module, Mview),
	    update_functions_view(dict:fetch(Module, TPs), Fview),
	    State#state{tpatterns=TPs}
    end.

do_add_processes(POpts, S0=#state{n_view=Nview, proc_view=LCtrl, tpids=OldPids, nodes=OldNodes}) ->
    CheckFun = fun(Pid) -> is_pid(Pid) end,
    {Pids, Nodes} = do_add_pid_or_port(POpts, Nview, LCtrl,
					OldPids, OldNodes, CheckFun),
    S0#state{tpids=Pids, nodes=Nodes}.

do_add_ports(POpts, S0=#state{n_view=Nview, port_view=LCtrl, tports=OldPorts, nodes=OldNodes}) ->
    CheckFun = fun(Port) -> is_port(Port) end,
    {Ports, Nodes} = do_add_pid_or_port(POpts, Nview, LCtrl,
					OldPorts, OldNodes, CheckFun),
    S0#state{tports=Ports, nodes=Nodes}.

do_add_pid_or_port(POpts, Nview, LCtrl, OldPs, Ns0, Check) ->
    case merge_trace_items(POpts, OldPs) of
	{OldPs, [], []} ->
	    {OldPs,Ns0};
	{Ps, New, _Changed} ->
	    Ns1 = lists:usort([node(Id) || #titem{id=Id} <- New, Check(Id)]),
	    Nodes = case ordsets:subtract(Ns1, Ns0) of
			[] when Ns0==[] -> [observer_wx:get_active_node()];
			[] -> Ns0; %% No new Nodes
			NewNs -> ordsets:union(NewNs, Ns0)
		    end,
	    update_nodes_view(Nodes, Nview),
	    update_p_view(Ps, LCtrl, Nodes, Nview),
	    {Ps, Nodes}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_visible_ps(PidsOrPorts, [Node], _Nview) ->
    %% If only one node, treat this as selected
    get_visible_ps(PidsOrPorts, [Node]);
get_visible_ps(PidsOrPorts, Nodes, Nview) ->
    get_visible_ps(PidsOrPorts, get_selected_items(Nview, Nodes)).

get_visible_ps(PidsOrPorts, Nodes) ->
    %% Show pids/ports belonging to the selected nodes only (+ named pids/ports)
    [P || P <- PidsOrPorts,
	  is_atom(P#titem.id) orelse
	      lists:member(node(P#titem.id),Nodes)].

update_p_view(PidsOrPorts, LCtrl, Nodes, Nview) ->
    update_p_view(get_visible_ps(PidsOrPorts, Nodes, Nview), LCtrl).
update_p_view(PidsOrPorts, LCtrl, Nodes) ->
    update_p_view(get_visible_ps(PidsOrPorts, Nodes), LCtrl).

update_p_view(PidsOrPorts, LCtrl) ->
    %% pid- or port-view
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(#titem{id=Id, opts=Opts}, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso
			 wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str(Id)),
		     wxListCtrl:setItem(LCtrl, Row, 1, observer_lib:to_str(Opts)),
		     Row+1
	     end, 0, PidsOrPorts),
    case PidsOrPorts of
	[] ->
	    wxListCtrl:setToolTip(LCtrl,?NO_P_HELP);
	_ ->
	    wxListCtrl:setToolTip(LCtrl,?P_HELP)
    end.

update_nodes_view(Nodes, LCtrl) ->
    Selected =
	case Nodes of
	    [_] -> Nodes;
	    _ -> get_selected_items(LCtrl, Nodes)
	end,
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(Node, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso
			 wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str(Node)),
		     lists:member(Node,Selected) andalso % keep selection
			 wxListCtrl:setItemState(LCtrl, Row, 16#FFFF,
						 ?wxLIST_STATE_SELECTED),
		     Row+1
	     end, 0, Nodes),
    case Nodes of
	[] ->
	    wxListCtrl:setToolTip(LCtrl,?NO_NODES_HELP);
	_ ->
	    wxListCtrl:setToolTip(LCtrl,?NODES_HELP)
    end.

update_modules_view(Mods, Module, LCtrl) ->
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(Mod, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso
			 wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str(Mod)),
		     (Mod =:= Module) andalso
			 wxListCtrl:setItemState(LCtrl, Row, 16#FFFF, ?wxLIST_STATE_SELECTED),
		     Row+1
	     end, 0, Mods),
    Parent = wxListCtrl:getParent(LCtrl),
    case Mods of
	[] ->
	    wxListCtrl:setToolTip(Parent,?NO_TP_HELP);
	_ ->
	    wxListCtrl:setToolTip(Parent,?TP_HELP)
    end.

update_functions_view(Funcs, LCtrl) ->
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(#tpattern{m=M, fa=FA, ms=#match_spec{str=Ms}}, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     FuncStr =
			 case M of
			     'Events' ->
				 observer_lib:to_str(FA);
			     _ ->
				 observer_lib:to_str({func,FA})
			 end,
		     wxListCtrl:setItem(LCtrl, Row, 0, FuncStr),
		     wxListCtrl:setItem(LCtrl, Row, 1, Ms),
		     Row+1
	     end, 0, Funcs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Trace items are processes and ports
merge_trace_items([N1=#titem{id=NewP}|Ns], [N2=#titem{id=NewP}|Old])
  when NewP==new_processes; NewP==new_ports ->
    {Ids, New, Changed} = merge_trace_items_1(Ns,Old),
    {[N1|Ids], New, [{N2,N2}|Changed]};
merge_trace_items([N1=#titem{id=NewP}|Ns], Old)
  when NewP==new_processes; NewP==new_ports ->
    {Ids, New, Changed} = merge_trace_items_1(Ns,Old),
    {[N1|Ids], [N1|New], Changed};
merge_trace_items(Ns, [N2=#titem{id=NewP}|Old])
  when NewP==new_processes; NewP==new_ports ->
    {Ids, New, Changed} = merge_trace_items_1(Ns,Old),
    {[N2|Ids], New, Changed};
merge_trace_items(New, Old) ->
    merge_trace_items_1(New, Old).

merge_trace_items_1(New, Old) ->
    merge(lists:sort(New), Old, #titem.id, [], [], []).

merge_patterns(New, Old) ->
    merge(lists:sort(New), Old, #tpattern.fa, [], [], []).

merge([N|Ns], [N|Os], El, New, Ch, All) ->
    merge(Ns, Os, El, New, Ch, [N|All]);
merge([N|Ns], [O|Os], El, New, Ch, All)
  when element(El, N) == element(El, O) ->
    merge(Ns, Os, El, New, [{O,N}|Ch], [N|All]);
merge([N|Ns], Os=[O|_], El, New, Ch, All)
  when element(El, N) < element(El, O) ->
    merge(Ns, Os, El, [N|New], Ch, [N|All]);
merge(Ns=[N|_], [O|Os], El, New, Ch, All)
  when element(El, N) > element(El, O) ->
    merge(Ns, Os, El, New, Ch, [O|All]);
merge([], Os, _El, New, Ch, All) ->
    {lists:reverse(All, Os), New, Ch};
merge(Ns, [], _El, New, Ch, All) ->
    {lists:reverse(All, Ns), Ns++New, Ch}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ttb_output_args(Parent, Opts) ->
    ToWindow = proplists:get_value(window, Opts, true),
    ToShell = proplists:get_value(shell, Opts, false),
    ToFile = proplists:get_value(file, Opts, false),
    ToWindow orelse ToShell orelse ToFile orelse
	throw({error, "No output of trace"}),
    {LogWin,Text} = create_logwindow(Parent, ToWindow),
    Write = output_fun(Text, ToShell),
    Shell = output_shell(ToFile, Write),
    FileOpts = output_file(ToFile, proplists:get_value(wrap, Opts, false), Opts),
    {[{file, {local,FileOpts}}|Shell], LogWin}.

output_shell(true, false) ->
    []; %% File only
output_shell(true, Write) when is_function(Write) ->
    [{shell, Write}];
output_shell(false, Write) when is_function(Write) ->
    [{shell, {only, Write}}].

output_fun(false, false) -> false;
output_fun(false, true) -> fun(Trace) -> io:put_chars(textformat(Trace)) end;
output_fun(Text, false) ->
    Env = wx:get_env(),
    fun(Trace) ->
	    wx:set_env(Env),
	    wxTextCtrl:appendText(Text, textformat(Trace))
    end;
output_fun(Text, true) ->
    Env = wx:get_env(),
    fun(Trace) ->
	    wx:set_env(Env),
	    IoList = textformat(Trace),
	    wxTextCtrl:appendText(Text, IoList),
	    io:put_chars(textformat(Trace))
    end.

output_file(false, _, _Opts) ->
    "ttb";  %% Will be ignored
output_file(true, false, Opts) ->
    proplists:get_value(filename, Opts, "ttb");
output_file(true, true, Opts) ->
    Name = proplists:get_value(filename, Opts, "ttb"),
    Size = proplists:get_value(wrap_sz, Opts, 128),
    Count = proplists:get_value(wrap_c, Opts, 8),
    {wrap, Name, Size*1024, Count}.


create_logwindow(_Parent, false) -> {false, false};
create_logwindow(Parent, true) ->
    LogWin = wxFrame:new(Parent, ?LOG_WIN, "Trace Log", [{size, {750, 800}}]),
    MB = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File, ?LOG_CLEAR, "Clear Log\tCtrl-C"),
    wxMenu:append(File, ?LOG_SAVE, "Save Log\tCtrl-S"),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenuBar:append(MB, File, "File"),
    wxFrame:setMenuBar(LogWin, MB),
    Text = wxTextCtrl:new(LogWin, ?wxID_ANY,
			  [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2 bor
				?wxTE_DONTWRAP bor ?wxTE_READONLY}]),
    Font = observer_wx:get_attrib({font, fixed}),
    Attr = wxTextAttr:new(?wxBLACK, [{font, Font}]),
    true = wxTextCtrl:setDefaultStyle(Text, Attr),
    wxFrame:connect(LogWin, close_window, [{skip, true}]),
    wxFrame:connect(LogWin, command_menu_selected, [{userData, Text}]),
    wxFrame:show(LogWin),
    {LogWin, Text}.

setup_ttb(TPs, TPids, TPorts) ->
    _R1 = [setup_tps(FTP, []) || {_, FTP} <- TPs],
    _R2 = [ttb:p(Pid, dbg_flags(proc,Flags)) ||
	      #titem{id=Pid, opts=Flags} <- TPids],
    _R3 = [ttb:p(Port, dbg_flags(port,Flags)) ||
	      #titem{id=Port, opts=Flags} <- TPorts],
    ok.

%% Sigh order is important
setup_tps([First=#tpattern{fa={_,'_'}}|Rest], Prev) ->
    setup_tp(First),
    [setup_tp(TP) || TP <- lists:reverse(Prev)],
    setup_tps(Rest, []);
setup_tps([First=#tpattern{fa={F,_}}|Rest], Prev = [#tpattern{fa={F,_}}|_]) ->
    setup_tps(Rest, [First|Prev]);
setup_tps([First|Rest], Prev) ->
    [setup_tp(TP) || TP <- lists:reverse(Prev)],
    setup_tps(Rest, [First]);
setup_tps([], Prev) ->
    [setup_tp(TP) || TP <- lists:reverse(Prev)].

setup_tp(#tpattern{m='Events',fa=Event, ms=#match_spec{term=Ms}}) ->
    ttb:tpe(Event,Ms);
setup_tp(#tpattern{m=M,fa={F,A}, ms=#match_spec{term=Ms}}) ->
    ttb:tpl(M,F,A,Ms).

dbg_flags(Type,Flags) ->
    [dbg_flag(Type,Flag) || Flag <- Flags].

dbg_flag(_,send) -> s;
dbg_flag(_,'receive') -> r;
dbg_flag(proc,functions) -> c;
dbg_flag(proc,on_spawn) -> sos;
dbg_flag(proc,on_link) -> sol;
dbg_flag(proc,on_first_spawn) -> sofs;
dbg_flag(proc,on_first_link) -> sofl;
dbg_flag(proc,events) -> p;
dbg_flag(port,events) -> ports;
dbg_flag(_,Flag) -> Flag.

textformat(Trace) when element(1, Trace) == trace_ts, tuple_size(Trace) >= 4 ->
    format_trace(Trace, tuple_size(Trace)-1, element(tuple_size(Trace),Trace));
textformat(Trace) when element(1, Trace) == drop, tuple_size(Trace) =:= 2 ->
    io_lib:format("*** Dropped ~p messages.~n", [element(2,Trace)]);
textformat(Trace) when element(1, Trace) == seq_trace, tuple_size(Trace) >= 3 ->
    io_lib:format("*** Seq trace not implmented.~n", []);
textformat(_) ->
    "".

format_trace(Trace, Size, TS0={_,_,MS}) ->
    {_,{H,M,S}} = calendar:now_to_local_time(TS0),
    TS = io_lib:format("~.2.0w:~.2.0w:~.2.0w:~.6.0w", [H,M,S,MS]),
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> "";
		Message ->
		    io_lib:format("~s (~100p) << ~100p~n", [TS,From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    To = element(5, Trace),
	    io_lib:format("~s (~100p) ~100p ! ~100p~n", [TS,From,To,Message]);
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io_lib:format("~s (~100p) call ~s (~100p) ~n", [TS,From,ffunc(MFA),Message]);
		MFA ->
		    io_lib:format("~s (~100p) call ~s~n", [TS,From,ffunc(MFA)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io_lib:format("~s (~100p) returned from ~s -> ~100p~n", [TS,From,ffunc(MFA),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io_lib:format("~s (~100p) returning to ~s~n", [TS,From,ffunc(MFA)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io_lib:format("~s (~100p) spawn ~100p as ~s~n", [TS,From,Pid,ffunc(MFA)]);
	Op ->
	    io_lib:format("~s (~100p) ~100p ~s~n", [TS,From,Op,ftup(Trace,4,Size)])
    end.

%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~100p:~100p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~100p:~100p/~100p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~100p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when is_integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~100p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~100p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~100p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index) ->
    io_lib:format("~100p", [element(Index, Trace)]);
ftup(Trace, Index, Size) ->
    [io_lib:format("~100p ", [element(Index, Trace)])
     | ftup(Trace, Index+1, Size)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_config(#state{def_proc_flags = ProcFlags,
                  def_port_flags = PortFlags,
                  match_specs = MatchSpecs0,
                  tpatterns = TracePatterns,
                  output = Output}) ->
    MSToList = fun(#match_spec{name=Id, term=T, func=F}) ->
		       [{name,Id},{term,T},{func,F}]
	       end,
    MatchSpecs = [{ms,Key,[MSToList(MS) || MS <- MSs]} ||
		     {Key,MSs} <- MatchSpecs0],
    TPToTuple = fun(#tpattern{fa={F,A}, ms=Ms}) ->
                        {F,A,MSToList(Ms)}
		end,
    ModuleTermList = [{tp, Module, [TPToTuple(FTP) || FTP <- FTPs]} ||
			 {Module,FTPs} <- dict:to_list(TracePatterns)],
    [{procflags,ProcFlags},
     {portflags,PortFlags},
     {match_specs,MatchSpecs},
     {output,Output},
     {trace_p,ModuleTermList}].

write_file(Frame, Filename, Config) ->
    Str =
	["%%%\n%%% This file is generated by Observer\n",
	 "%%%\n%%% DO NOT EDIT!\n%%%\n",
	 [io_lib:format("~p.~n",[MSTerm]) ||
             MSTerm <- proplists:get_value(match_specs, Config)],
	 io_lib:format("~p.~n",[lists:keyfind(procflags, 1, Config)]),
	 io_lib:format("~p.~n",[lists:keyfind(portflags, 1, Config)]),
	 io_lib:format("~p.~n",[lists:keyfind(output, 1, Config)]),
	 [io_lib:format("~p.~n",[ModuleTerm]) ||
             ModuleTerm <- proplists:get_value(trace_p, Config)]
	],

    case file:write_file(Filename, list_to_binary(Str)) of
	ok ->
	    success;
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    observer_wx:create_txt_dialog(Frame, FailMsg, "Error", ?wxICON_ERROR)
    end.

read_settings(Filename, #state{match_specs=Ms0, def_proc_flags=ProcFs0, def_port_flags=PortFs0} = State) ->
    case file:consult(Filename) of
	{ok, Terms} ->
	    Ms = parse_ms(Terms, Ms0),
	    ProcFs1 = proplists:get_value(procflags, Terms, []) ++
		proplists:get_value(traceopts, Terms, []), % for backwards comp.
	    ProcFs = lists:usort(ProcFs0 ++ ProcFs1),
	    PortFs = lists:usort(PortFs0 ++
				     proplists:get_value(portflags, Terms, [])),
	    Out = proplists:get_value(output, Terms, []),
	    lists:foldl(fun parse_tp/2,
			State#state{match_specs=Ms, def_proc_flags=ProcFs,
				    def_port_flags=PortFs, output=Out},
			Terms);
	{error, _} ->
	    observer_wx:create_txt_dialog(State#state.panel,
					  "Could not load settings",
					  "Error", ?wxICON_ERROR),
	    State
    end.

parse_ms(Terms, OldMSs) ->
    MSs =
	case [{Key,[make_ms(MS) || MS <- MSs]} || {ms,Key,MSs} <- Terms] of
	    [] ->
		case [make_ms(MS) || {ms,MS} <- Terms] of
		    [] ->
			[];
		    FuncMSs -> % for backwards compatibility
			[{funcs,FuncMSs}]
		end;
	    KeyMSs ->
		KeyMSs
	end,
    parse_ms_1(MSs, dict:from_list(OldMSs)).

parse_ms_1([{Key,MSs} | T], Dict) ->
    parse_ms_1(T, dict:append_list(Key,MSs,Dict));
parse_ms_1([],Dict) ->
    [{Key,rm_dups(MSs,[])} || {Key,MSs} <- dict:to_list(Dict)].

rm_dups([H|T],Acc) ->
    case lists:member(H,Acc) of
	true ->
	    rm_dups(T,Acc);
	false ->
	    rm_dups(T,[H|Acc])
    end;
rm_dups([],Acc) ->
    lists:reverse(Acc).

make_ms(MS) ->
    [{func,FunStr},{name,Name},{term,Term}] = lists:keysort(1,MS),
    make_ms(Name,Term,FunStr).

make_ms(Name, Term, FunStr) ->
    #match_spec{name=Name, term=Term, str=io_lib:format("~w", Term), func = FunStr}.

parse_tp({tp, Mod, FAs}, State) ->
    Patterns = [#tpattern{m=Mod,fa={F,A}, ms=make_ms(List)} ||
		   {F,A,List} <- FAs],
    do_add_patterns({Mod, Patterns}, State);
parse_tp(_, State) ->
    State.

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

create_right_click_menu(Panel,Menus) ->
    Menu = wxMenu:new(),
    [wxMenu:append(Menu,Id,Str) || {Id,Str} <- Menus],
    wxWindow:popupMenu(Panel, Menu),
    wxMenu:destroy(Menu).
