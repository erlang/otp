%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(crashdump_viewer_wx).

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

init(File) ->
    {ok,CdvServer} = crashdump_viewer:start_link(),

    register(?SERVER, self()),
    wx:new(),
    catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Crashdump viewer",
			[{size, {850, 600}}, {style, ?wxDEFAULT_FRAME_STYLE}]),
    IconFile = filename:join(code:priv_dir(observer), "erlang_observer.png"),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_PNG}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon),

    State = #state{server=CdvServer, file = File, frame = Frame},
    UpdState = setup(State),
    process_flag(trap_exit, true),
    {Frame, UpdState}.

setup(#state{file = File0, frame = Frame} = State) ->
    %% Setup Menubar & Menus
    MenuBar = wxMenuBar:new(),
    DefMenus = default_menus(),
    observer_lib:create_menus(DefMenus, MenuBar, default),
    wxFrame:setMenuBar(Frame, MenuBar),

    %% Setup panels
    Panel = wxPanel:new(Frame, []),
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

    %% Setup "statusbar" to show warnings
    StatusBar = observer_lib:create_status_bar(Panel),

    %% Load a crashdump
    File = load_dump(Panel,File0),

    %% Set window title
    T1 = "Crashdump Viewer: ",
    Title =
	if length(File) > 70 ->
		T1 ++ filename:basename(File);
	   true ->
		T1 ++ File
	end,
    wxFrame:setTitle(Frame, Title),

    %% General information Panel
    GenPanel = add_page(Notebook, ?GEN_STR, cdv_info_page, cdv_gen_wx),

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
    wxFrame:show(Frame),

    %% I postpone the creation of the other tabs so they can query/use
    %% the window size

    %% Process Panel
    ProPanel = add_page(Notebook, ?PRO_STR, cdv_virtual_list, cdv_proc_wx),

    %% Port Panel
    PortPanel = add_page(Notebook, ?PORT_STR, cdv_virtual_list, cdv_port_wx),

    %% Table Panel
    EtsPanel = add_page(Notebook, ?ETS_STR, cdv_virtual_list, cdv_ets_wx),

    %% Timer Panel
    TimerPanel = add_page(Notebook, ?TIMER_STR, cdv_virtual_list, cdv_timer_wx),

    %% Fun Panel
    FunPanel = add_page(Notebook, ?FUN_STR, cdv_virtual_list, cdv_fun_wx),

    %% Atom Panel
    AtomPanel = add_page(Notebook, ?ATOM_STR, cdv_virtual_list, cdv_atom_wx),

    %% Distribution Panel
    DistPanel = add_page(Notebook, ?DIST_STR, cdv_virtual_list, cdv_dist_wx),

    %% Loaded Modules Panel
    ModPanel = add_page(Notebook, ?MOD_STR, cdv_virtual_list, cdv_mod_wx),

    %% Memory Panel
    MemPanel = add_page(Notebook, ?MEM_STR, cdv_multi_panel, cdv_mem_wx),

    %% Memory Panel
    IntPanel = add_page(Notebook, ?INT_STR, cdv_multi_panel, cdv_int_tab_wx),

    %% Force redraw (window needs it)
    wxWindow:refresh(Panel),

    GenPid = wx_object:get_pid(GenPanel),
    GenPid ! active,
    UpdState = State#state{file = File,
			   main_panel = Panel,
			   notebook = Notebook,
			   menubar = MenuBar,
			   status_bar = StatusBar,
			   gen_panel = GenPanel,
			   pro_panel = ProPanel,
			   port_panel = PortPanel,
			   ets_panel = EtsPanel,
			   timer_panel = TimerPanel,
			   fun_panel = FunPanel,
			   atom_panel = AtomPanel,
			   dist_panel = DistPanel,
			   mod_panel = ModPanel,
			   mem_panel = MemPanel,
			   int_panel = IntPanel,
			   active_tab = GenPid
			  },
    %% Create resources which we don't want to duplicate
    SysFont = wxSystemSettings:getFont(?wxSYS_SYSTEM_FIXED_FONT),
    Fixed = case wxFont:isFixedWidth(SysFont) of
		true -> SysFont;
		false -> %% Sigh
		    SysFontSize = wxFont:getPointSize(SysFont),
		    wxFont:new(SysFontSize,
			       ?wxFONTFAMILY_MODERN,
			       ?wxFONTSTYLE_NORMAL,
			       ?wxFONTWEIGHT_NORMAL)
	    end,
    put({font, fixed}, Fixed),
    UpdState.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Callbacks
handle_event(#wx{event=#wxNotebook{type=command_notebook_page_changing}},
	     #state{active_tab=Previous} = State) ->
    case get_active_pid(State) of
	Previous -> {noreply, State};
	Pid ->
	    Previous ! not_active,
	    Pid ! active,
	    {noreply, State#state{active_tab=Pid}}
    end;

handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};

handle_event(#wx{id = ?wxID_OPEN,
		 event = #wxCommand{type = command_menu_selected}},
	     State) ->
    File = load_dump(State#state.main_panel,undefined),
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
    _ = [wx_object:get_pid(Panel) ! new_dump || Panel<-Panels],
    State#state.active_tab ! active,
    {noreply, State#state{file=File}};

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
		      mod_panel=Mods, mem_panel=Mem, int_panel=Int
		     }) ->
    Panel = case check_page_title(Notebook) of
		?GEN_STR -> Gen;
		?PRO_STR -> Pro;
		?PORT_STR -> Ports;
		?ETS_STR -> Ets;
		?TIMER_STR -> Timers;
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
	    %% But not to the help menu for some reason
%!	    {Tag, Menus} = NodeMenu,
%!	    [{Tag, Menus ++ [Quit,About]}, {"&Help", [Help]}]
	    [{"File", [Quit,About]}, {"&Help", [Help,UG,Howto]}] %?siri: does this work?
    end.


load_dump(Panel,undefined) ->
    FD  =  wxFileDialog:new(Panel,
			    [{style,?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    wxDialog:destroy(FD),
	    load_dump(Panel, Path);
	_ ->
	    wxDialog:destroy(FD)
    end;
load_dump(Panel, FileName) ->
    crashdump_viewer:read_file(FileName),
    update_progress(Panel,false),
    FileName.

update_progress(Panel,PD) ->
    case crashdump_viewer:get_progress() of
	{ok, done} ->
	    wxProgressDialog:destroy(PD);
	{ok, Percent} when is_integer(Percent) ->
	    wxProgressDialog:update(PD,Percent),
	    update_progress(Panel,PD);
	{ok,Msg} ->
	    case PD of
		false -> ok;
		_ -> wxProgressDialog:destroy(PD)
	    end,
	    update_progress(Panel,new_progress(Msg));
	{error, Reason} ->
	    wxProgressDialog:destroy(PD),
	    FailMsg = file:format_error(Reason),
	    MD = wxMessageDialog:new(Panel, FailMsg),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD)
    end.

new_progress(Msg) ->
  wxProgressDialog:new("Crashdump viewer",Msg,
		       [{maximum,100},
			{style,
			 ?wxPD_APP_MODAL bor
			     ?wxPD_SMOOTH bor
			     ?wxPD_AUTO_HIDE}]).
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
