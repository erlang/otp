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
-module(cdv_wx).
-compile(export_all).
-behaviour(wx_object).

-export([start/1]).
-export([get_attrib/1, set_status/1, create_txt_dialog/4]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2, check_page_title/1]).

%% Includes
-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/file.hrl").

-include("observer_defs.hrl").

%% Defines

-define(SERVER, cdv_wx).

-define(ID_UG, 1).
-define(ID_HOWTO, 2).
-define(ID_NOTEBOOK, 3).

-define(GEN_STR,   "General").
-define(PRO_STR,   "Processes").
-define(PORT_STR,  "Ports").
-define(ETS_STR,   "ETS Tables").
-define(TIMER_STR, "Timers").
-define(SCHEDULER_STR, "Schedulers").
-define(FUN_STR,   "Funs").
-define(ATOM_STR,  "Atoms").
-define(DIST_STR,  "Nodes").
-define(MOD_STR,   "Modules").
-define(MEM_STR,   "Memory").
-define(INT_STR,   "Internal Tables").

%% Records
-record(state,
	{server,
	 file,
	 frame,
	 menubar,
	 menus = [],
	 status_bar,
	 notebook,
	 main_panel,
	 gen_panel,
	 pro_panel,
	 port_panel,
	 ets_panel,
	 timer_panel,
	 sched_panel,
	 fun_panel,
	 atom_panel,
	 dist_panel,
	 mod_panel,
	 mem_panel,
	 int_panel,
	 active_tab
	}).

start(File) ->
    case wx_object:start(?MODULE, File, []) of
	Err = {error, _} -> Err;
	_Obj -> ok
    end.

get_attrib(What) ->
    wx_object:call(?SERVER, {get_attrib, What}).

set_status(What) ->
    wx_object:cast(?SERVER, {status_bar, What}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(File0) ->
    register(?SERVER, self()),
    wx:new(),

    {ok,CdvServer} = crashdump_viewer:start_link(),

    catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Crashdump Viewer",
			[{size, {850, 600}}, {style, ?wxDEFAULT_FRAME_STYLE}]),
    IconFile = filename:join(code:priv_dir(observer), "erlang_observer.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon),

    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

    %% Setup "statusbar" to show warnings
    StatusBar = observer_lib:create_status_bar(Panel),

    %% Setup sizer create early to get it when window shows
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, StatusBar, [{flag, ?wxEXPAND bor ?wxALL},
				       {proportion, 0},
				       {border,4}]),
    wxPanel:setSizer(Panel, MainSizer),

    wxNotebook:connect(Notebook, command_notebook_page_changing),
    wxFrame:connect(Frame, close_window, [{skip, true}]),
    wxMenu:connect(Frame, command_menu_selected),

    case load_dump(Frame,File0) of
	{ok,File} ->
	    %% Set window title
	    T1 = "Crashdump Viewer: ",
	    Title =
		if length(File) > 70 ->
			T1 ++ filename:basename(File);
		   true ->
			T1 ++ File
		end,
	    wxFrame:setTitle(Frame, Title),

	    setup(#state{server=CdvServer,
			 file=File,
			 frame=Frame,
			 status_bar=StatusBar,
			 notebook=Notebook,
			 main_panel=Panel});
	error ->
	    wxFrame:destroy(Frame),
	    wx:destroy(),
	    crashdump_viewer:stop(),
	    ignore
    end.

setup(#state{frame=Frame, notebook=Notebook}=State) ->

    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),
    DefMenus = default_menus(),
    observer_lib:create_menus(DefMenus, MenuBar, default),
    wxFrame:setMenuBar(Frame, MenuBar),

    %% General information Panel
    GenPanel = add_page(Notebook, ?GEN_STR, cdv_info_wx, cdv_gen_cb),

    %% Process Panel
    ProPanel = add_page(Notebook, ?PRO_STR, cdv_virtual_list_wx, cdv_proc_cb),

    %% Port Panel
    PortPanel = add_page(Notebook, ?PORT_STR, cdv_virtual_list_wx, cdv_port_cb),

    %% Table Panel
    EtsPanel = add_page(Notebook, ?ETS_STR, cdv_virtual_list_wx, cdv_ets_cb),

    %% Timer Panel
    TimerPanel = add_page(Notebook, ?TIMER_STR, cdv_virtual_list_wx,cdv_timer_cb),

    %% Scheduler Panel
    SchedPanel = add_page(Notebook, ?SCHEDULER_STR, cdv_virtual_list_wx, cdv_sched_cb),

    %% Fun Panel
    FunPanel = add_page(Notebook, ?FUN_STR, cdv_virtual_list_wx, cdv_fun_cb),

    %% Atom Panel
    AtomPanel = add_page(Notebook, ?ATOM_STR, cdv_virtual_list_wx, cdv_atom_cb),

    %% Distribution Panel
    DistPanel = add_page(Notebook, ?DIST_STR, cdv_virtual_list_wx, cdv_dist_cb),

    %% Loaded Modules Panel
    ModPanel = add_page(Notebook, ?MOD_STR, cdv_virtual_list_wx, cdv_mod_cb),

    %% Memory Panel
    MemPanel = add_page(Notebook, ?MEM_STR, cdv_multi_wx, cdv_mem_cb),

    %% Memory Panel
    IntPanel = add_page(Notebook, ?INT_STR, cdv_multi_wx, cdv_int_tab_cb),

    %% Show the window
    wxFrame:show(Frame),

    GenPid = wx_object:get_pid(GenPanel),
    GenPid ! active,
    observer_lib:destroy_progress_dialog(),
    process_flag(trap_exit, true),
    {Frame, State#state{menubar = MenuBar,
			gen_panel = GenPanel,
			pro_panel = ProPanel,
			port_panel = PortPanel,
			ets_panel = EtsPanel,
			timer_panel = TimerPanel,
			sched_panel = SchedPanel,
			fun_panel = FunPanel,
			atom_panel = AtomPanel,
			dist_panel = DistPanel,
			mod_panel = ModPanel,
			mem_panel = MemPanel,
			int_panel = IntPanel,
			active_tab = GenPid
		       }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{event=#wxNotebook{type=command_notebook_page_changing}},
	     #state{active_tab=Previous} = State) ->
    case get_active_pid(State) of
	Previous -> {noreply, State};
	Pid ->
	    Pid ! active,
	    {noreply, State#state{active_tab=Pid}}
    end;

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?wxID_OPEN,
		 event = #wxCommand{type = command_menu_selected}},
	     State) ->
    NewState =
	case load_dump(State#state.frame,undefined) of
	    {ok,File} ->
		Panels = [State#state.gen_panel,
			  State#state.pro_panel,
			  State#state.port_panel,
			  State#state.ets_panel,
			  State#state.timer_panel,
			  State#state.fun_panel,
			  State#state.atom_panel,
			  State#state.dist_panel,
			  State#state.mod_panel,
			  State#state.mem_panel,
			  State#state.int_panel],
		_ = [wx_object:call(Panel,new_dump) || Panel<-Panels],
		wxNotebook:setSelection(State#state.notebook,0),
		observer_lib:destroy_progress_dialog(),
		State#state{file=File};
	    error ->
		State
    end,
    {noreply,NewState};

handle_event(#wx{id = ?wxID_EXIT,
		 event = #wxCommand{type = command_menu_selected}},
	     State) ->
    {stop, normal, State};

handle_event(#wx{id = HelpId,
		 event = #wxCommand{type = command_menu_selected}},
	     State) when HelpId==?wxID_HELP; HelpId==?ID_UG; HelpId==?ID_HOWTO ->
    Help = get_help_doc(HelpId),
    wx_misc:launchDefaultBrowser(Help) orelse
	create_txt_dialog(State#state.frame,
			  "Could not launch browser: ~n " ++ Help,
			  "Error", ?wxICON_ERROR),
    {noreply, State};

handle_event(#wx{id = ?wxID_ABOUT,
		 event = #wxCommand{type = command_menu_selected}},
	     State = #state{frame=Frame}) ->
    AboutString = "Display information from an erlang crash dump",
    Style = [{style, ?wxOK bor ?wxSTAY_ON_TOP},
	     {caption, "About"}],
    wxMessageDialog:showModal(wxMessageDialog:new(Frame, AboutString, Style)),
    {noreply, State};

handle_event(Event, State) ->
    Pid = get_active_pid(State),
    Pid ! Event,
    {noreply, State}.

handle_cast({status_bar, Msg}, State=#state{status_bar=SB}) ->
    wxTextCtrl:clear(SB),
    wxTextCtrl:writeText(SB, Msg),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_call({get_attrib, Attrib}, _From, State) ->
    {reply, get(Attrib), State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info({'EXIT', Pid, normal}, #state{server=Pid}=State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    io:format("Child (~s) crashed exiting:  ~p ~p~n",
	      [pid2panel(Pid, State), Pid,_Reason]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{frame = Frame}) ->
    wxFrame:destroy(Frame),
    wx:destroy(),
    crashdump_viewer:stop(),
    ok.

code_change(_, _, State) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_page(Notebook,Title,Callback,Extra) ->
    Panel = Callback:start_link(Notebook, Extra),
    wxNotebook:addPage(Notebook, Panel, Title, []),
    Panel.

create_txt_dialog(Frame, Msg, Title, Style) ->
    MD = wxMessageDialog:new(Frame, Msg, [{style, Style}]),
    wxMessageDialog:setTitle(MD, Title),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD).

check_page_title(Notebook) ->
    Selection = wxNotebook:getSelection(Notebook),
    wxNotebook:getPageText(Notebook, Selection).

get_active_pid(#state{notebook=Notebook, gen_panel=Gen, pro_panel=Pro,
		      port_panel=Ports, ets_panel=Ets, timer_panel=Timers,
		      fun_panel=Funs, atom_panel=Atoms, dist_panel=Dist,
		      mod_panel=Mods, mem_panel=Mem, int_panel=Int,
		      sched_panel=Sched
		     }) ->
    Panel = case check_page_title(Notebook) of
		?GEN_STR -> Gen;
		?PRO_STR -> Pro;
		?PORT_STR -> Ports;
		?ETS_STR -> Ets;
		?TIMER_STR -> Timers;
		?SCHEDULER_STR -> Sched;
		?FUN_STR -> Funs;
		?ATOM_STR -> Atoms;
		?DIST_STR -> Dist;
		?MOD_STR -> Mods;
		?MEM_STR -> Mem;
		?INT_STR -> Int
	    end,
    wx_object:get_pid(Panel).

pid2panel(Pid, #state{gen_panel=Gen, pro_panel=Pro, port_panel=Ports,
		      ets_panel=Ets, timer_panel=Timers, fun_panel=Funs,
		      atom_panel=Atoms, dist_panel=Dist, mod_panel=Mods,
		      mem_panel=Mem, int_panel=Int}) ->
    case Pid of
	Gen -> ?GEN_STR;
	Pro -> ?PRO_STR;
	Ports -> ?PORT_STR;
	Ets -> ?ETS_STR;
	Timers -> ?TIMER_STR;
	Funs -> ?FUN_STR;
	Atoms -> ?ATOM_STR;
	Dist -> ?DIST_STR;
	Mods -> ?MOD_STR;
	Mem -> ?MEM_STR;
	Int -> ?INT_STR;
	_ -> "unknown"
    end.

default_menus() ->
    Open  = #create_menu{id = ?wxID_OPEN, text = "Open new crash dump"},
    Quit  = #create_menu{id = ?wxID_EXIT, text = "Quit"},
    About = #create_menu{id = ?wxID_ABOUT, text = "About"},
    Help  = #create_menu{id = ?wxID_HELP},
    UG    = #create_menu{id = ?ID_UG, text = "Crashdump viewer user's guide"},
    Howto = #create_menu{id = ?ID_HOWTO, text = "How to interpret crash dump"},
    case os:type() =:= {unix, darwin} of
	false ->
	    FileMenu = {"File", [Open,Quit]},
	    HelpMenu = {"Help", [About,Help,UG,Howto]},
	    [FileMenu, HelpMenu];
	true ->
	    %% On Mac quit and about will be moved to the "default' place
	    %% automagicly, so just add them to a menu that always exist.
	    [{"File", [Open, About,Quit]}, {"&Help", [Help,UG,Howto]}] 
    end.


load_dump(Frame,undefined) ->
    FD  =  wxFileDialog:new(wx:null(),
			    [{style,?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    wxDialog:destroy(FD),
	    load_dump(Frame,Path);
	_ ->
	    wxDialog:destroy(FD),
	    error
    end;
load_dump(Frame,FileName) ->
    ok = observer_lib:display_progress_dialog("Crashdump Viewer",
					      "Loading crashdump"),
    crashdump_viewer:read_file(FileName),
    case observer_lib:wait_for_progress() of
	ok    ->
	    %% Set window title
	    T1 = "Crashdump Viewer: ",
	    Title =
		if length(FileName) > 70 ->
			T1 ++ filename:basename(FileName);
		   true ->
			T1 ++ FileName
		end,
	    wxFrame:setTitle(Frame, Title),
	    {ok,FileName};
	error ->
	    error
    end.

%%%-----------------------------------------------------------------
%%% Find help document (HTML files)
get_help_doc(HelpId) ->
    Internal = get_internal_help_doc(HelpId),
    case filelib:is_file(Internal) of
	true -> Internal;
	false -> get_external_help_doc(HelpId)
    end.

get_internal_help_doc(?ID_HOWTO) ->
    filename:join(erts_doc_dir(),help_file(?ID_HOWTO));
get_internal_help_doc(HelpId) ->
    filename:join(observer_doc_dir(),help_file(HelpId)).

get_external_help_doc(?ID_HOWTO) ->
    filename:join("http://www.erlang.org/doc/apps/erts",help_file(?ID_HOWTO));
get_external_help_doc(HelpId) ->
    filename:join("http://www.erlang.org/doc/apps/observer",help_file(HelpId)).

observer_doc_dir() ->
    filename:join([code:lib_dir(observer),"doc","html"]).

erts_doc_dir() ->
    ErtsVsn = erlang:system_info(version),
    RootDir = code:root_dir(),
    VsnErtsDir = filename:join(RootDir,"erts-"++ErtsVsn),
    DocDir = filename:join(["doc","html"]),
    case filelib:is_dir(VsnErtsDir) of
	true ->
	    filename:join(VsnErtsDir,DocDir);
	false ->
	    %% So this can be run in source tree
	    filename:join([RootDir,"erts",DocDir])
    end.

help_file(?wxID_HELP)  -> "crashdump_help.html";
help_file(?ID_UG)    -> "crashdump_ug.html";
help_file(?ID_HOWTO) -> "crash_dump.html".
