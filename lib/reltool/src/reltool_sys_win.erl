%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(reltool_sys_win).

%% Public
-export([start_link/1, get_server/1, set_app/2, open_app/2]).

%% Internal
-export([init/1, loop/1]).

%% sys callback functions
-export([
         system_continue/3,
         system_terminate/4,
         system_code_change/4
        ]).

-include_lib("wx/include/wx.hrl").
-include("reltool.hrl").

-record(state,
        {parent_pid,
         server_pid,
         app_wins,
         sys,
         common,
         config_file,
         target_dir,
         boot_dir,
         frame,
         panel,
	 book,
	 rel_book,
         lib_tree,
         status_bar,
         source,
         whitelist,
         blacklist,
         derived,
         fgraph_wins,
	 app_box,
	 mod_box,
	 warning_list,
	 warning_wins
        }).

-define(WIN_WIDTH, 800).
-define(WIN_HEIGHT, 600).

-define(CLOSE_ITEM, ?wxID_EXIT).    %% Use OS specific version if available
-define(ABOUT_ITEM, ?wxID_ABOUT).   %% Use OS specific
-define(CONTENTS_ITEM, 300).
-define(APP_GRAPH_ITEM, 301).
-define(MOD_GRAPH_ITEM, 302).
-define(LOAD_CONFIG_ITEM, 303).
-define(SAVE_CONFIG_NODEF_NODER_ITEM, 304).
-define(SAVE_CONFIG_NODEF_DER_ITEM, 305).
-define(SAVE_CONFIG_DEF_NODER_ITEM, 306).
-define(SAVE_CONFIG_DEF_DER_ITEM, 307).
-define(UNDO_CONFIG_ITEM, 308).
-define(RESET_CONFIG_ITEM, 309).
-define(GEN_REL_FILES_ITEM, 310).
-define(GEN_TARGET_ITEM, 311).

-define(APP_PAGE, "Applications").
-define(LIB_PAGE, "Libraries").
-define(SYS_PAGE, "System settings").
-define(REL_PAGE, "Releases").

-define(APPS_APP_COL, 0).
-define(source, "Available").
-define(whitelist, "Included").
-define(blacklist, "Excluded").
-define(derived, "Derived").

-define(WARNING_COL, 0).
-define(DEFAULT_WARNING_TIP, "Warnings are listed in this window").
-define(WARNING_POPUP_SIZE, {400,150}).

-define(safe_config,{sys,[{incl_cond,exclude},
			  {app,kernel,[{incl_cond,include}]},
			  {app,stdlib,[{incl_cond,include}]},
			  {app,sasl,[{incl_cond,include}]}]}).

-record(root_data, {dir}).
-record(lib_data, {dir, tree, item}).
-record(escript_data, {file, tree, item}).
-record(app_data, {name, dir}).
-record(app_win, {name, pid}).
-record(fgraph_win, {frame, pid}).
-record(root_popup, {dir, choices, tree, item}).
-record(lib_popup, {dir, choices, tree, item}).
-record(escript_popup, {file, choices, tree, item}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_link(Opts) ->
    proc_lib:start_link(?MODULE,
			init,
			[[{safe_config, false}, {parent, self()} | Opts]],
			infinity,
			[]).

get_server(Pid) ->
    reltool_utils:call(Pid, get_server).

set_app(Pid, App) ->
    reltool_utils:call(Pid, {set_app, App}).

open_app(Pid, AppName) ->
    reltool_utils:call(Pid, {open_app, AppName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init(Options) ->
    try
	do_init(Options)
    catch
	error:Reason ->
	    io:format("~p: ~p~n",[Reason, erlang:get_stacktrace()]),
	    exit({Reason, erlang:get_stacktrace()})
    end.

do_init([{safe_config, Safe}, {parent, Parent} | Options]) ->
    case reltool_server:start_link(Options) of
        {ok, ServerPid, C, Sys} ->
	    process_flag(trap_exit, C#common.trap_exit),
            wx:new(),
            wx:debug(C#common.wx_debug),

            %% wx_misc:beginBusyCursor(),
	    {ok, Warnings} = reltool_server:get_status(ServerPid),
	    exit_dialog(Warnings),
	    S = #state{parent_pid = Parent,
		       server_pid = ServerPid,
		       common = C,
		       config_file = filename:absname("config.reltool"),
		       target_dir = filename:absname("reltool_target_dir"),
		       app_wins = [],
		       sys = Sys,
		       fgraph_wins = [],
		       warning_wins = []},
	    S2 = create_window(S),
	    S5 = wx:batch(fun() ->
				  Title = atom_to_list(?APPLICATION),
				  wxFrame:setTitle(S2#state.frame,
						   Title),
				  %% wxFrame:setMinSize(Frame,
				  %% {?WIN_WIDTH, ?WIN_HEIGHT}),
				  wxStatusBar:setStatusText(
				    S2#state.status_bar,
				    "Done."),
				  S3 = redraw_apps(S2),
				  S4 = redraw_libs(S3),
				  redraw_config_page(S4)
			  end),
	    %% wx_misc:endBusyCursor(),
	    %% wxFrame:destroy(Frame),
	    proc_lib:init_ack(S#state.parent_pid, {ok, self()}),
	    loop(S5);
	{error, Reason} ->
	    restart_server_safe_config(Safe,Parent,Reason)
    end.

restart_server_safe_config(true,Parent,Reason) ->
    io:format("~w(~w): <ERROR> ~p\n", [?MODULE, ?LINE, Reason]),
    proc_lib:init_ack(Parent, {error,Reason});
restart_server_safe_config(false,Parent,Reason) ->
    wx:new(),
    Strings =
	[{?wxBLACK,"Could not start reltool server:\n\n"},
	 {?wxRED,Reason++"\n\n"},
	 {?wxBLACK,
	  io_lib:format(
	    "Resetting the configuration to:~n~n  ~p~n~n"
	    "Do you want to continue with this configuration?",
	    [?safe_config])}],

    case question_dialog_2("Reltool server start error", Strings) of
	?wxID_OK ->
	    do_init([{safe_config,true},{parent,Parent},?safe_config]);
	?wxID_CANCEL ->
	    io:format("~w(~w): <ERROR> ~p\n", [?MODULE, ?LINE, Reason]),
	    proc_lib:init_ack(Parent,{error,Reason})
    end.

exit_dialog([]) ->
    ok;
exit_dialog(Warnings) ->
    Question = "Do you want to continue despite these warnings?",
    Details = lists:flatten([[W, $\n] || W <- Warnings]),
    case question_dialog(Question, Details) of
        ?wxID_OK ->
	    ok;
        ?wxID_CANCEL  ->
	    io:format("~w(~w): <ERROR> ~ts\n", [?MODULE, ?LINE, Details]),
	    exit(Details)
    end.

loop(S) ->
    receive
        {system, From, Msg} ->
            Common = S#state.common,
            sys:handle_system_msg(Msg,
				  From,
				  S#state.parent_pid,
				  ?MODULE,
				  Common#common.sys_debug,
				  S);
        #wx{obj = ObjRef,
            event = #wxClose{type = close_window}} = Msg ->
            if
                ObjRef =:= S#state.frame ->
                    wxFrame:destroy(ObjRef),
                    exit(shutdown);
                true ->
                    FWs = S#state.fgraph_wins,
                    case lists:keysearch(ObjRef, #fgraph_win.frame, FWs) of
                        {value, FW} ->
                            reltool_fgraph_win:stop(FW#fgraph_win.pid,
						    shutdown),
                            wxFrame:destroy(ObjRef),
                            FWs2 =
				lists:keydelete(ObjRef, #fgraph_win.frame, FWs),
                            ?MODULE:loop(S#state{fgraph_wins = FWs2});
                        false ->
			    WWs = S#state.warning_wins,
			    case lists:member(ObjRef, WWs) of
				true ->
				    wxFrame:destroy(ObjRef),
				    WWs2 = lists:delete(ObjRef, WWs),
				    ?MODULE:loop(S#state{warning_wins = WWs2});
				false ->
				    error_logger:format("~w~w got unexpected "
							"message:\n\t~p\n",
							[?MODULE, self(), Msg]),
				    ?MODULE:loop(S)
			    end
                    end
            end;
        #wx{id = ?CLOSE_ITEM,
	    event = #wxCommand{type = command_menu_selected},
	    userData = main_window} ->
            wxFrame:destroy(S#state.frame),
            exit(shutdown);
	#wx{event = #wxSize{}} = Wx ->
	    Wx2 = reltool_utils:get_latest_resize(Wx),
	    S2 = handle_event(S, Wx2),
            ?MODULE:loop(S2);
        #wx{} = Wx ->
            S2 = handle_event(S, Wx),
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, get_server} ->
            reltool_utils:reply(ReplyTo, Ref, {ok, S#state.server_pid}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_app,  NewApp}} ->
            {ok, AnalysedApp, S2} = do_set_app(S, NewApp),
            reltool_utils:reply(ReplyTo, Ref, {ok, AnalysedApp}),
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, {open_app, AppName}} ->
            S2 = do_open_app(S, AppName),
	    {value, #app_win{pid = AppPid}} =
		lists:keysearch(AppName, #app_win.name, S2#state.app_wins),
            reltool_utils:reply(ReplyTo, Ref, {ok, AppPid}),
            ?MODULE:loop(S2);
        {'EXIT', Pid, Reason} when Pid =:= S#state.parent_pid ->
            [reltool_fgraph_win:stop(FW#fgraph_win.pid, Reason) ||
		FW <- S#state.fgraph_wins],
            exit(Reason);
        {'EXIT', _Pid, _Reason} = Exit ->
            {FWs, AWs} = handle_child_exit(Exit,
					   S#state.fgraph_wins,
					   S#state.app_wins),
            ?MODULE:loop(S#state{fgraph_wins = FWs, app_wins = AWs});
        Msg ->
            error_logger:format("~w~w got unexpected message:\n\t~p\n",
                                [?MODULE, self(), Msg]),
            ?MODULE:loop(S)
    end.

handle_child_exit({'EXIT', Pid, _Reason} = Exit, FWs, AWs) ->
    case lists:keymember(Pid, #fgraph_win.pid, FWs) of
        true ->
            msg_warning(Exit, forcegraph_window),
            {lists:keydelete(Pid, #fgraph_win.pid, FWs), AWs};
        false ->
            case lists:keymember(Pid, #app_win.pid, AWs) of
                true ->
                    msg_warning(Exit, application_window),
                    {FWs, lists:keydelete(Pid, #app_win.pid, AWs)};
                false ->
		    msg_warning(Exit, unknown),
		    {FWs, AWs}
            end
    end.

msg_warning({'EXIT', _Pid, shutdown}, Type) when Type =/= unknown ->
    ok;
msg_warning(Exit, Type) ->
    error_logger:format("~w~w got unexpected message (~w):\n\t~p\n",
                        [?MODULE, self(), Type, Exit]).

create_window(S) ->
    Title = lists:concat([?APPLICATION, " - starting up"]),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title,
			[{size, {?WIN_WIDTH, ?WIN_HEIGHT}}]),
    %%wxFrame:setSize(Frame, {?WIN_WIDTH, ?WIN_HEIGHT}),
    %% wxFrame:setMinSize(Frame, {?WIN_WIDTH, ?WIN_HEIGHT}),
    Bar = wxFrame:createStatusBar(Frame,[]),
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    %% Label = wxStaticText:new(Panel, ?wxID_ANY, Text, [{style, ?wxTE_READONLY}]),
    %% Sizer = wxBoxSizer:new(?wxVERTICAL),
    %% wxSizer:add(Sizer, Label, [{flag, ?wxEXPAND}, {proportion, 1}]),
    %% wxPanel:setSizer(Panel, Sizer),
    %% wxSizer:fit(Sizer, Frame),
    %% wxSizer:setSizeHints(Sizer, Frame),

    %% Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, []),
    %%  Frame = S#state.frame,
    wxToolTip:setDelay(3000),
    Panel = wxPanel:new(Frame, []),
    %% Bar = wxFrame:createStatusBar(Frame,[]),
    create_menubar(Frame),

    Book = wxNotebook:new(Panel, ?wxID_ANY, []),
    S2 = S#state{frame = Frame, panel = Panel, book = Book, status_bar = Bar},
    S3 = lists:foldl(fun(Fun, Acc) -> Fun(Acc) end,
		     S2,
		     [
		      fun create_app_page/1,
		      fun create_lib_page/1,
		      fun create_main_release_page/1,
		      fun create_config_page/1
		     ]),

    S4 = create_warning_list(S3),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Book, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, S4#state.warning_list, [{flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:connect(Frame, close_window),

    wxFrame:show(Frame),
    S4.

create_menubar(Frame) ->
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File, ?APP_GRAPH_ITEM,
		  "Display application dependency graph" ),
    wxMenu:append(File, ?MOD_GRAPH_ITEM,
		  "Display module dependency graph" ),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?RESET_CONFIG_ITEM, "Reset configuration to default" ),
    wxMenu:append(File, ?UNDO_CONFIG_ITEM, "Undo configuration (toggle)" ),
    wxMenu:append(File, ?LOAD_CONFIG_ITEM, "Load configuration" ),
    Save = wxMenu:new(),
    wxMenu:append(Save, ?SAVE_CONFIG_NODEF_NODER_ITEM,
		  "Save explicit configuration  "
		  "(neither defaults nor derivates)"),
    wxMenu:append(Save, ?SAVE_CONFIG_DEF_NODER_ITEM,
		  "Save configuration defaults  (defaults only)"),
    wxMenu:append(Save, ?SAVE_CONFIG_NODEF_DER_ITEM,
		  "Save configuration derivates (derivates only))"),
    wxMenu:append(Save, ?SAVE_CONFIG_DEF_DER_ITEM,
		  "Save extended configuration  (both defaults and derivates)"),

    wxMenu:append(File, ?wxID_ANY, "Save configuration", Save),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?GEN_REL_FILES_ITEM,
		  "Generate rel, script and boot files" ),
    wxMenu:append(File, ?GEN_TARGET_ITEM, "Generate target system" ),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxMenuBar:append(MenuBar, Help, "Help" ),
    wxMenu:append(Help, ?CONTENTS_ITEM, "Contents" ),
    wxMenu:append(Help, ?ABOUT_ITEM, "About" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
                         command_menu_selected,
                         [{userData, main_window}]),
    MenuBar.

create_app_page(#state{book = Book} = S) ->
    Panel = wxPanel:new(Book, []),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    SourceCtrl  = create_app_list_ctrl(Panel, Sizer, ?source,
				       whitelist_add, blacklist_add),
    wxSizer:add(Sizer,
                wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    WhiteCtrl   = create_app_list_ctrl(Panel, Sizer, ?whitelist,
				       whitelist_del, blacklist_add),
    wxSizer:add(Sizer,
                wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    BlackCtrl   = create_app_list_ctrl(Panel, Sizer, ?blacklist,
				       whitelist_add, blacklist_del),
    wxSizer:add(Sizer,
                wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    DerivedCtrl = create_app_list_ctrl(Panel, Sizer, ?derived,
				       whitelist_add, blacklist_add),
    %% S3 = redraw_apps(S2),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(Book, Panel, ?APP_PAGE, []),
    S#state{source = SourceCtrl,
	    whitelist = WhiteCtrl,
	    blacklist = BlackCtrl,
	    derived = DerivedCtrl}.

create_app_list_ctrl(Panel, OuterSz, Title, Tick, Cross) ->
    %% Create list control
    Width = lists:max([100, ?WIN_WIDTH - 40]) div 4,
    Height = lists:max([100, ?WIN_HEIGHT - 100]),
    ListCtrl = wxListCtrl:new(Panel,
                              [{style,
                                ?wxLC_REPORT bor
                                %% ?wxLC_SORT_ASCENDING bor
                                %% ?wxLC_SINGLE_SEL bor
                                ?wxVSCROLL},
			       {size, {Width, Height}}]),
    ToolTip = "Select application(s) or open separate "
	"application window with a double click.",
    wxListCtrl:setToolTip(ListCtrl, ToolTip),

    %% Prep images
    reltool_utils:assign_image_list(ListCtrl),

    %% Prep column label
    ListItem  = wxListItem:new(),
    wxListItem:setAlign(ListItem, ?wxLIST_FORMAT_LEFT),
    wxListItem:setText(ListItem, Title),
    wxListItem:setWidth(ListItem, reltool_utils:get_column_width(ListCtrl)),
    wxListCtrl:insertColumn(ListCtrl, ?APPS_APP_COL, ListItem),
    wxListItem:destroy(ListItem),

    %% Create button
    ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
    create_button(Panel, ButtonSz, ListCtrl, Title, "wxART_TICK_MARK", Tick),
    create_button(Panel, ButtonSz, ListCtrl, Title, "wxART_CROSS_MARK", Cross),


    InnerSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(InnerSz,
		ListCtrl,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxSizer:add(InnerSz,
		ButtonSz,
                [{flag, ?wxEXPAND}]),
    wxSizer:add(OuterSz,
		InnerSz,
                [{flag, ?wxEXPAND}, {proportion, 1}]),

    %% Subscribe on events
    wxEvtHandler:connect(ListCtrl, size,
			 [{skip, true}, {userData, app_list_ctrl}]),
    wxEvtHandler:connect(ListCtrl, command_list_item_activated),
    wxWindow:connect(ListCtrl, enter_window),

    ListCtrl.

%% create_button(_Panel, Sizer, _ListCtrl, _BitMapName, _Tag, undefined) ->
%%     wxSizer:addStretchSpacer(Sizer);
create_button(Panel, Sizer, ListCtrl, Title, BitMapName, Action) ->
    BitMap = wxArtProvider:getBitmap(BitMapName),
    Button = wxBitmapButton:new(Panel, ?wxID_ANY, BitMap, []),
    ToolTip = action_to_tool_tip(Title, Action),
    wxBitmapButton:setToolTip(Button, ToolTip),
    Options = [{userData, {app_button, Action, ListCtrl}}],
    wxEvtHandler:connect(Button, command_button_clicked, Options),
    wxSizer:add(Sizer,
		Button,
                [{border, 2},
                 {flag, ?wxALL},
                 {proportion, 1}]).

action_to_tool_tip(Label, Action) ->
    case Action of
        whitelist_add when Label =:= ?whitelist ->
            "Remove selected application(s) from whitelist.";
        whitelist_add ->
            "Add selected application(s) to whitelist.";
        whitelist_del ->
            "Remove selected application(s)from whitelist.";
        blacklist_add when Label =:= ?blacklist ->
            "Remove selected application(s) from blacklist.";
        blacklist_add ->
            "Add selected application(s) to blacklist.";
        blacklist_del ->
            "Remove selected application(s) from blacklist."
    end.

create_lib_page(#state{book = Book} = S) ->
    Panel = wxPanel:new(Book, []),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    Tree = wxTreeCtrl:new(Panel,
			  [{style , ?wxTR_HAS_BUTTONS bor ?wxTR_HIDE_ROOT}]),
    ToolTip = "Edit application sources.",
    wxBitmapButton:setToolTip(Tree, ToolTip),

    wxFrame:connect(Tree, command_tree_item_activated),
    wxFrame:connect(Tree, command_tree_item_right_click),

    wxSizer:add(Sizer,
		Tree,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(Book, Panel, ?LIB_PAGE, []),
    S#state{lib_tree = Tree}.

redraw_libs(#state{lib_tree = Tree, sys = Sys} = S) ->
    wxTreeCtrl:deleteAllItems(Tree),

    Top = wxTreeCtrl:addRoot(Tree, "Sources", []),
    {ok, Erts} = reltool_server:get_app(S#state.server_pid, erts),
    append_root(Tree, Top, Sys#sys.root_dir, Erts),

    LibItem = wxTreeCtrl:appendItem(Tree, Top, "Library directories", []),
    LibData = #lib_data{dir = undefined, tree = Tree, item = LibItem},
    wxTreeCtrl:setItemData(Tree, LibItem, LibData),
    [append_lib(Tree, LibItem, Dir) || Dir <- Sys#sys.lib_dirs],

    EscriptItem = append_item(Tree, Top, "Escript files", undefined),
    EscriptData = #escript_data{file = undefined,
				tree = Tree,
				item = EscriptItem},
    wxTreeCtrl:setItemData(Tree,EscriptItem, EscriptData),
    [append_escript(Tree, EscriptItem, File) || File <- Sys#sys.escripts],
    wxTreeCtrl:expand(Tree, LibItem),
    wxTreeCtrl:expand(Tree, EscriptItem),
    S.

append_root(Tree, Parent, Dir, Erts) ->
    Top = append_item(Tree, Parent, "Root directory", undefined),
    Data = #root_data{dir = Dir},
    RootItem = append_item(Tree, Top, Dir, Data),
    ErtsItem = append_item(Tree, RootItem, "erts", undefined),
    [append_app(Tree, ErtsItem, filename:basename(filename:dirname(D)), D)
     || D <- Erts#app.sorted_dirs],
    LibItem = append_item(Tree, RootItem, "lib", undefined),
    LibDir = filename:join([Dir, "lib"]),
    LibDirs = reltool_utils:lib_dirs(LibDir),
    AppDirs = lists:sort(fun reltool_utils:app_dir_test/2, LibDirs),
    [append_app(Tree, LibItem, D, LibDir) || D <- AppDirs],
    wxTreeCtrl:expand(Tree, Top),
    RootItem.

append_lib(Tree, Parent, Dir) ->
    Item = wxTreeCtrl:appendItem(Tree, Parent, Dir, []),
    Data = #lib_data{dir = Dir, tree = Tree, item = Item},
    wxTreeCtrl:setItemData(Tree, Item, Data),
    append_apps(Tree, Item, Dir).

append_apps(Tree, Item, Dir) ->
    AppDirs = lists:sort(fun reltool_utils:app_dir_test/2,
                         reltool_utils:lib_dirs(Dir)),
    [append_app(Tree, Item, D, Dir) || D <- AppDirs],
    Item.

append_app(Tree, Parent, Base, Dir) ->
    Data = #app_data{name = Base, dir = Dir},
    append_item(Tree, Parent, Base, Data).

append_escript(Tree, Parent, File) ->
    Data = #escript_data{file = File},
    append_item(Tree, Parent, File, Data).

append_item(Tree, Parent, Label, Data) ->
    Item = wxTreeCtrl:appendItem(Tree, Parent, Label, []),
    wxTreeCtrl:setItemData(Tree, Item, Data),
    Item.

create_config_page(#state{sys = Sys, book = Book} = S) ->
    Panel = wxPanel:new(Book, []),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    AppConds = reltool_utils:incl_conds(),
    AppBox = wxRadioBox:new(Panel,
                            ?wxID_ANY,
                            "Application inclusion policy",
                            ?wxDefaultPosition,
                            ?wxDefaultSize,
                            AppConds,
                            []),
    AppToolTip = "Choose default policy for inclusion of applications. ",
    wxBitmapButton:setToolTip(AppBox, AppToolTip),
    AppChoice = reltool_utils:incl_cond_to_index(Sys#sys.incl_cond),
    wxRadioBox:setSelection(AppBox, AppChoice),
    wxEvtHandler:connect(AppBox, command_radiobox_selected,
			 [{userData, config_incl_cond}]),
    ModConds = reltool_utils:mod_conds(),
    ModBox = wxRadioBox:new(Panel,
                            ?wxID_ANY,
                            "Module inclusion policy",
                            ?wxDefaultPosition,
                            ?wxDefaultSize,
                            ModConds,
                            []),
    ModToolTip = "Choose default policy for module inclusion.",
    wxBitmapButton:setToolTip(ModBox, ModToolTip),

    ModChoice = reltool_utils:mod_cond_to_index(Sys#sys.mod_cond),
    wxRadioBox:setSelection(ModBox, ModChoice),
    wxEvtHandler:connect(ModBox, command_radiobox_selected,
			 [{userData, config_mod_cond}]),

    wxSizer:add(Sizer,
		AppBox,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxSizer:add(Sizer,
		ModBox,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(Book, Panel, ?SYS_PAGE, []),
    S#state{app_box = AppBox, mod_box = ModBox}.

redraw_config_page(#state{sys = Sys, app_box = AppBox, mod_box = ModBox} = S) ->
    AppChoice = reltool_utils:incl_cond_to_index(Sys#sys.incl_cond),
    wxRadioBox:setSelection(AppBox, AppChoice),
    ModChoice = reltool_utils:mod_cond_to_index(Sys#sys.mod_cond),
    wxRadioBox:setSelection(ModBox, ModChoice),
    S.

create_warning_list(#state{panel = Panel} = S) ->
    ListCtrl = wxListCtrl:new(Panel,
                              [{style,
				?wxLC_REPORT bor
				    ?wxLC_HRULES bor
				    ?wxVSCROLL},
			       {size, {?WIN_WIDTH,80}}]),
    reltool_utils:assign_image_list(ListCtrl),
    wxListCtrl:insertColumn(ListCtrl, ?WARNING_COL, "Warnings",
			    [{format,?wxLIST_FORMAT_LEFT},
			     {width,reltool_utils:get_column_width(ListCtrl)}]),
    wxListCtrl:setToolTip(ListCtrl, ?DEFAULT_WARNING_TIP),
    wxEvtHandler:connect(ListCtrl, size,
			 [{skip, true}, {userData, warnings}]),
    wxEvtHandler:connect(ListCtrl, command_list_item_activated,
			 [{userData, warnings}]),
    wxEvtHandler:connect(ListCtrl, motion, [{userData, warnings}]),
    wxEvtHandler:connect(ListCtrl, enter_window),
    S#state{warning_list=ListCtrl}.

redraw_warnings(S) ->
    {ok,Warnings} = reltool_server:get_status(S#state.server_pid),
    redraw_warnings(S#state.warning_list,Warnings),
    length(Warnings).

redraw_warnings(ListCtrl, []) ->
    wxListCtrl:deleteAllItems(ListCtrl),
    ok;
redraw_warnings(ListCtrl, Warnings) ->
    wxListCtrl:deleteAllItems(ListCtrl),
    Show = fun(Warning, Row) ->
		   wxListCtrl:insertItem(ListCtrl, Row, ""),
		   wxListCtrl:setItem(ListCtrl,
				      Row,
				      ?WARNING_COL,
				      Warning,
				      [{imageId, ?WARN_IMAGE}]),
		   Row + 1
	   end,
    wx:foldl(Show, 0, Warnings),
    ok.


create_main_release_page(#state{book = Book} = S) ->
    Panel = wxPanel:new(Book, []),
    RelBook = wxNotebook:new(Panel, ?wxID_ANY, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

    Create  = wxButton:new(Panel, ?wxID_ANY, [{label, "Create"}]),
    wxButton:setToolTip(Create, "Create a new release."),
    wxButton:connect(Create, command_button_clicked, [{userData, create_rel}]),
    wxSizer:add(ButtonSizer, Create),

    Delete  = wxButton:new(Panel, ?wxID_ANY, [{label, "Delete"}]),
    wxButton:setToolTip(Delete, "Delete a release."),
    wxButton:connect(Delete, command_button_clicked, [{userData, delete_rel}]),
    wxSizer:add(ButtonSizer, Delete),

    View  = wxButton:new(Panel, ?wxID_ANY, [{label, "View script"}]),
    wxButton:setToolTip(View, "View generated script file."),
    wxButton:connect(View, command_button_clicked, [{userData, view_script}]),
    wxSizer:add(ButtonSizer, View),

    [add_release_page(RelBook, Rel) || Rel <- (S#state.sys)#sys.rels],

    wxSizer:add(Sizer, RelBook, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, ButtonSizer, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(Book, Panel, ?REL_PAGE, []),
    S#state{rel_book = RelBook}.

add_release_page(Book, #rel{name = RelName, rel_apps = RelApps}) ->
    Panel = wxPanel:new(Book, []),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    AppNames = [kernel, stdlib |
		 [RA#rel_app.name || RA <- RelApps] -- [kernel, stdlib]],
    RelBox = wxListBox:new(
	       Panel,?wxID_ANY,
	       [{pos,?wxDefaultPosition},
		{size,?wxDefaultSize},
		{choices,[[atom_to_list(AppName)] || AppName <- AppNames]},
		{style,?wxLB_EXTENDED}]),
    wxEvtHandler:connect(RelBox, command_listbox_selected,
			 [{userData, {config_rel_cond, RelName}}]),
    RelToolTip = "Choose which applications that shall "
	"be included in the release resource file.",
    wxBitmapButton:setToolTip(RelBox, RelToolTip),

    wxSizer:add(Sizer,
		RelBox,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(Book, Panel, RelName, []).

do_open_app(S, AppBase) when is_list(AppBase) ->
    {AppName, _AppVsn} = reltool_utils:split_app_name(AppBase),
    do_open_app(S, AppName);
do_open_app(S, '') ->
    S;
do_open_app(#state{server_pid = ServerPid, common = C, app_wins = AppWins} = S,
	    AppName)
  when is_atom(AppName) ->
    case lists:keysearch(AppName, #app_win.name, AppWins) of
        false ->
            WxEnv = wx:get_env(),
            {ok, Pid} =
		reltool_app_win:start_link(WxEnv, ServerPid, C, AppName),
            AW = #app_win{name = AppName, pid = Pid},
            S#state{app_wins = [AW | AppWins]};
        {value, AW} ->
            reltool_app_win:raise(AW#app_win.pid),
            S
    end.

root_popup(S, Root, Tree, Item) ->
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, 0, "Root dir"),
    wxMenu:appendSeparator(PopupMenu),
    wxMenu:append(PopupMenu, 1, "Edit"),
    Choices = [edit],
    Popup = #root_popup{dir = Root, choices = Choices,
			tree = Tree, item = Item},
    wxEvtHandler:connect(PopupMenu, command_menu_selected, [{userData, {popup, Popup}}]),
    wxWindow:popupMenu(S#state.frame, PopupMenu),

    S.

lib_popup(S, Lib, Tree, Item) ->
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, 0, "Library dir"),
    wxMenu:appendSeparator(PopupMenu),
    wxMenu:append(PopupMenu, 1, "Add"),
    Choices =
        case wxTreeCtrl:getItemData(Tree, Item) of
            #lib_data{dir = undefined} ->
                [add];
            #lib_data{} ->
                wxMenu:append(PopupMenu, 2, "Edit"),
                wxMenu:append(PopupMenu, 3, "Delete"),
                [add, edit, delete]
        end,
    Popup = #lib_popup{dir = Lib, choices = Choices, tree = Tree, item = Item},
    wxEvtHandler:connect(PopupMenu, command_menu_selected, [{userData, {popup, Popup}}]),
    wxWindow:popupMenu(S#state.frame, PopupMenu),
    S.

escript_popup(S, File, Tree, Item) ->
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, 0, "Escript file"),
    wxMenu:appendSeparator(PopupMenu),
    wxMenu:append(PopupMenu, 1, "Add"),
    Choices =
        case wxTreeCtrl:getItemData(Tree, Item) of
            #escript_data{file = undefined} ->
                [add];
            #escript_data{} ->
                wxMenu:append(PopupMenu, 2, "Edit"),
                wxMenu:append(PopupMenu, 3, "Delete"),
                [add, edit, delete]
        end,
    Popup = #escript_popup{file = File, choices = Choices,
			   tree = Tree, item = Item},
    wxEvtHandler:connect(PopupMenu, command_menu_selected, [{userData, {popup, Popup}}]),
    wxWindow:popupMenu(S#state.frame, PopupMenu),
    S.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(S, #wx{id = Id, obj= ObjRef, userData = UserData, event = Event} = _Wx) ->
    %% io:format("wx: ~p\n", [Wx]),
    case Event of
	_ when UserData =:= warnings;
	       is_tuple(UserData), element(1,UserData)=:=warning ->
	    handle_warning_event(S, ObjRef, UserData, Event);
	#wxSize{type = size, size = {W, _H}} when UserData =:= app_list_ctrl ->
	    wxListCtrl:setColumnWidth(ObjRef, ?APPS_APP_COL, W),
	    S;
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?APP_GRAPH_ITEM ->
            update_app_graph(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?MOD_GRAPH_ITEM ->
            update_mod_graph(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?RESET_CONFIG_ITEM ->
            reset_config(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?UNDO_CONFIG_ITEM ->
            undo_config(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?LOAD_CONFIG_ITEM ->
            load_config(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?SAVE_CONFIG_NODEF_NODER_ITEM ->
            save_config(S, false, false);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?SAVE_CONFIG_NODEF_DER_ITEM ->
            save_config(S, false, true);
        #wxCommand{type = command_menu_selected} when Id =:= ?SAVE_CONFIG_DEF_NODER_ITEM ->
            save_config(S, true, false);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?SAVE_CONFIG_DEF_DER_ITEM ->
            save_config(S, true, true);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?GEN_REL_FILES_ITEM ->
            gen_rel_files(S);
        #wxCommand{type = command_menu_selected}
	  when Id =:= ?GEN_TARGET_ITEM ->
            gen_target(S);
        #wxCommand{type = command_menu_selected}
	  when UserData =:= main_window, Id =:= ?CONTENTS_ITEM ->
	    {file, BeamFile} = code:is_loaded(?MODULE),
	    EbinDir = filename:dirname(BeamFile),
	    AppDir = filename:dirname(EbinDir),
	    HelpFile = filename:join([AppDir, "doc", "html", "index.html"]),
	    Url = "file://" ++ filename:absname(HelpFile),
	    wx_misc:launchDefaultBrowser(Url),
            S;
        #wxCommand{type = command_menu_selected}
	  when UserData =:= main_window, Id =:= ?ABOUT_ITEM ->
	    AboutStr = "Reltool is a release management tool. It analyses a "
		" given Erlang/OTP installation and determines various "
		" dependencies between applications. The graphical frontend "
		" depicts the dependencies and enables interactive "
		" customization of a target system. The backend provides a "
		" batch interface for generation of customized target systems.",
	    MD = wxMessageDialog:new(S#state.frame,
				     AboutStr,
				     [{style, ?wxOK bor ?wxICON_INFORMATION},
				      {caption, "About Reltool"}]),
	    wxMessageDialog:showModal(MD),
	    wxMessageDialog:destroy(MD),
            S;
        #wxCommand{type = command_menu_selected = Type, cmdString = Str} ->
	    case UserData of
		{popup, Popup} ->
		    handle_popup_event(S, Type, Id, ObjRef, Popup, Str);
		true ->
		    S
	    end;
	#wxMouse{type = enter_window} ->
	    %% The following is commented out because it raises the
	    %% main system window on top of popup windows.
	    %% wxWindow:setFocus(ObjRef),
	    S;
        _ ->
	    case wxNotebook:getPageText(S#state.book,
					wxNotebook:getSelection(S#state.book)) of
		?APP_PAGE -> handle_app_event(S, Event, ObjRef, UserData);
		?LIB_PAGE -> handle_source_event(S, Event, ObjRef, UserData);
		?SYS_PAGE -> handle_system_event(S, Event, ObjRef, UserData);
		?REL_PAGE -> handle_release_event(S, Event, ObjRef, UserData)
            end
    end.

handle_warning_event(S, ObjRef, _, #wxSize{type = size}) ->
    ColumnWidth = reltool_utils:get_column_width(ObjRef),
    wxListCtrl:setColumnWidth(ObjRef, ?WARNING_COL, ColumnWidth),
    S;
handle_warning_event(S, ObjRef, _, #wxMouse{type = motion, x=X, y=Y}) ->
    Pos = reltool_utils:wait_for_stop_motion(ObjRef, {X,Y}),
    warning_list_set_tool_tip(os:type(),ObjRef,Pos),
    S;
handle_warning_event(S, ObjRef, _, #wxList{type = command_list_item_activated,
					itemIndex = Pos}) ->
    Text = wxListCtrl:getItemText(ObjRef, Pos),
    S#state{warning_wins = [display_warning(S,Text) | S#state.warning_wins]};
handle_warning_event(S, _ObjRef, {warning,Frame},
		     #wxCommand{type = command_button_clicked}) ->
    wxFrame:destroy(Frame),
    S#state{warning_wins = lists:delete(Frame,S#state.warning_wins)}.

warning_list_set_tool_tip({win32,_},ListCtrl,{_X,Y}) ->
    case win_find_item(ListCtrl,Y,0) of
	-1 ->
	    wxListCtrl:setToolTip(ListCtrl,?DEFAULT_WARNING_TIP);
	_Index ->
	    %% The following is commented out because there seems to
	    %% be an utomatic tooltip under Windows that shows the
	    %% expanded list item in case it is truncated because it
	    %% is too long for column width.
	    %% Tip =
	    %% 	case wxListCtrl:getItemText(ListCtrl,Index) of
	    %% 	    "" ->
	    %% 		?DEFAULT_WARNING_TIP;
	    %% 	    Text ->
	    %% 		"WARNING:\n" ++ Text
	    %% 	end,
	    %% wxListCtrl:setToolTip(ListCtrl,Tip),
	    ok
    end;
warning_list_set_tool_tip(_,ListCtrl,Pos) ->
    case wxListCtrl:findItem(ListCtrl,-1,Pos,0) of
	Index when Index >= 0 ->
	    Tip =
		case wxListCtrl:getItemText(ListCtrl,Index) of
		    "" ->
			?DEFAULT_WARNING_TIP;
		    Text ->
			"WARNING:\n" ++ Text
		end,
	    wxListCtrl:setToolTip(ListCtrl, Tip);
	_ ->
	    ok
    end.

win_find_item(ListCtrl,YPos,Index) ->
    case wxListCtrl:getItemRect(ListCtrl,Index) of
	{true,{_,Y,_,H}} when YPos>=Y, YPos=<Y+H ->
	    Index;
	{true,_} ->
	    win_find_item(ListCtrl,YPos,Index+1);
	{false,_} ->
	    -1
    end.

display_warning(S,Warning) ->
    Pos = warning_popup_position(S,?WARNING_POPUP_SIZE),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Warning",[{pos,Pos}]),
    Panel = wxPanel:new(Frame, []),
    TextStyle = ?wxTE_READONLY bor ?wxTE_WORDWRAP bor ?wxTE_MULTILINE,
    Text = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, Warning},
					     {style, TextStyle},
					     {size, ?WARNING_POPUP_SIZE}]),
    Attr = wxTextAttr:new(),
    wxTextAttr:setLeftIndent(Attr,10),
    wxTextAttr:setRightIndent(Attr,10),
    true = wxTextCtrl:setDefaultStyle(Text, Attr),
    wxTextAttr:destroy(Attr),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Text, [{border, 2},
			      {flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1}]),

    Close  = wxButton:new(Panel, ?wxID_ANY, [{label, "Close"}]),
    wxButton:setToolTip(Close, "Close window."),
    wxButton:connect(Close, command_button_clicked, [{userData,{warning,Frame}}]),
    wxSizer:add(Sizer, Close, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:connect(Frame, close_window),

    wxFrame:show(Frame),
    Frame.

warning_popup_position(#state{frame=MF,warning_list=WL},{WFW,WFH}) ->
    {MFX,MFY} = wxWindow:getPosition(MF),
    {MFW,MFH} = wxWindow:getSize(MF),
    {_WLW,WLH} = wxWindow:getSize(WL),

    %% Position the popup in the middle of the main frame, above the
    %% warning list, and with a small offset from the exact middle...
    Offset = 50,
    X = MFX + (MFW-WFW) div 2 - Offset,
    Y = MFY + (MFH-WLH-WFH) div 2 - Offset,
    {X,Y}.

handle_popup_event(S, _Type, 0, _ObjRef, _UserData, _Str) ->
    S;
handle_popup_event(#state{sys = Sys} = S, _Type, Pos, _ObjRef,
		   #root_popup{dir = OldDir, choices = Choices}, _Str) ->
    case lists:nth(Pos, Choices) of
        edit ->
            Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
            case select_dir(S#state.frame,
			    "Change root directory",
			    OldDir,
			    Style) of
		{ok, NewDir} when NewDir =:= OldDir ->
		    %% Same dir.Ignore.
		    S;
		{ok, NewDir} ->
                    Sys2 = Sys#sys{root_dir = NewDir},
                    do_set_sys(S#state{sys = Sys2});
                cancel ->
                    S
            end
    end;
handle_popup_event(#state{sys = Sys} = S, _Type, Pos, _ObjRef,
		   #lib_popup{dir = OldDir, choices = Choices}, _Str) ->
    case lists:nth(Pos, Choices) of
        add ->
            {ok, Cwd} = file:get_cwd(),
            Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
            case select_dir(S#state.frame, "Select a library directory to add", Cwd, Style) of
                {ok, NewDir} ->
                    case lists:member(NewDir, Sys#sys.lib_dirs) of
                        true ->
                            %% Ignore duplicate. Keep old.
                            S;
                        false ->
                            LibDirs = Sys#sys.lib_dirs ++ [NewDir],
                            Sys2 = Sys#sys{lib_dirs = LibDirs},
                            do_set_sys(S#state{sys = Sys2})
                    end;
                cancel ->
                    S
                end;
        edit ->
            Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
            case select_dir(S#state.frame,
			    "Change library directory",
			    OldDir,
			    Style) of
                {ok, NewDir} ->
                    case lists:member(NewDir, Sys#sys.lib_dirs) of
                        true ->
                            %% Ignore duplicate. Keep old.
                            S;
                        false ->
                            Pred = fun(E) -> E =/= OldDir end,
                            {Before, [_| After]} =
                                lists:splitwith(Pred, Sys#sys.lib_dirs),
                            LibDirs2 = Before ++ [NewDir | After],
                            Sys2 = Sys#sys{lib_dirs = LibDirs2},
                            do_set_sys(S#state{sys = Sys2})
                    end;
                cancel ->
                    S
            end;
        delete ->
            LibDirs = Sys#sys.lib_dirs -- [OldDir],
            Sys2 = Sys#sys{lib_dirs = LibDirs},
            do_set_sys(S#state{sys = Sys2})
    end;
handle_popup_event(#state{sys = Sys} = S, _Type, Pos, _ObjRef,
		   #escript_popup{file = OldFile, choices = Choices}, _Str) ->
    case lists:nth(Pos, Choices) of
        add ->
            OldFile2 =
                case OldFile of
                    undefined ->
                        {ok, Cwd} = file:get_cwd(),
                        filename:join([Cwd, "myEscript"]);
                    _ ->
                        OldFile
                end,
            Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
            case select_file(S#state.frame,
			     "Select an escript file to add",
			     OldFile2,
			     Style) of
                {ok, NewFile} ->
                    case lists:member(NewFile, Sys#sys.escripts) of
                        true ->
                            %% Ignore duplicate. Keep old.
                            S;
                        false ->
                            Escripts = Sys#sys.escripts ++ [NewFile],
                            Sys2 = Sys#sys{escripts = Escripts},
                            do_set_sys(S#state{sys = Sys2})
                    end;
                cancel ->
                    S
            end;
        edit ->
            Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
            case select_file(S#state.frame,
			     "Change escript file name",
			     OldFile,
			     Style) of
                {ok, NewFile} ->
                    case lists:member(NewFile, Sys#sys.escripts) of
                        true ->
                            %% Ignore duplicate. Keep old.
                            S;
                        false ->
                            Pred = fun(E) -> E =/= OldFile end,
                            {Before, [_| After]} =
				lists:splitwith(Pred, Sys#sys.escripts),
                            Escripts2 = Before ++ [NewFile | After],
                            Sys2 = Sys#sys{escripts = Escripts2},
                            do_set_sys(S#state{sys = Sys2})
                    end;
                cancel ->
                    S
            end;
        delete ->
            Escripts = Sys#sys.escripts -- [OldFile],
            Sys2 = Sys#sys{escripts = Escripts},
            do_set_sys(S#state{sys = Sys2})
    end.

handle_system_event(#state{sys = Sys} = S,
                    #wxCommand{type = command_radiobox_selected,
			       cmdString = Choice},
                    _ObjRef,
                    config_mod_cond) ->
    ModCond = reltool_utils:list_to_mod_cond(Choice),
    Sys2 = Sys#sys{mod_cond = ModCond},
    do_set_sys(S#state{sys = Sys2});
handle_system_event(#state{sys = Sys} = S,
                    #wxCommand{type = command_radiobox_selected,
			       cmdString = Choice},
                    _ObjRef,
                    config_incl_cond) ->
    AppCond = reltool_utils:list_to_incl_cond(Choice),
    Sys2 = Sys#sys{incl_cond = AppCond},
    do_set_sys(S#state{sys = Sys2});
handle_system_event(S, Event, ObjRef, UserData) ->
    error_logger:format("~w~w got unexpected wx sys event to ~p "
			"with user data: ~p\n\t ~p\n",
                        [?MODULE, self(), ObjRef, UserData, Event]),
    S.

handle_release_event(S, _Event, _ObjRef, UserData) ->
    io:format("Release data: ~p\n", [UserData]),
    S.

handle_source_event(S,
		    #wxTree{type = command_tree_item_activated,
			       item = Item},
		    ObjRef,
		    _UserData) ->
    case wxTreeCtrl:getItemData(ObjRef, Item) of
        #root_data{dir = _Dir} ->
            %% io:format("Root dialog: ~tp\n", [Dir]),
            S;
        #lib_data{dir = _Dir} ->
            %% io:format("Lib dialog: ~tp\n", [Dir]),
            S;
        #escript_data{file = _File} ->
            %% io:format("Escript dialog: ~tp\n", [File]),
            S;
        #app_data{name = Name} ->
            do_open_app(S, Name);
        undefined ->
            S
    end;
handle_source_event(S,
		    #wxTree{type = command_tree_item_right_click,
			       item = Item},
		    Tree,
		    _UserData) ->
    case wxTreeCtrl:getItemData(Tree, Item) of
        #root_data{dir = Dir} ->
            wx:batch(fun() -> root_popup(S, Dir, Tree, Item) end);
        #lib_data{dir = Dir} ->
            wx:batch(fun() -> lib_popup(S, Dir, Tree, Item) end);
        #escript_data{file = File} ->
            wx:batch(fun() -> escript_popup(S, File, Tree, Item) end);
        #app_data{name = Name} ->
            io:format("App menu: ~tp\n", [Name]),
            S;
        undefined ->
            S
    end.

handle_app_event(S,
		 #wxList{type = command_list_item_activated,
			    itemIndex = Pos},
		 ListCtrl,
		 _UserData) ->
    AppName = wxListCtrl:getItemText(ListCtrl, Pos),
    do_open_app(S, AppName);
handle_app_event(S,
		 #wxCommand{type = command_button_clicked},
		 _ObjRef,
		 {app_button, Action, ListCtrl}) ->
    Items = reltool_utils:get_items(ListCtrl),
    handle_app_button(S, Items, Action);
handle_app_event(S, Event, ObjRef, UserData) ->
    error_logger:format("~w~w got unexpected wx app event to "
			"~p with user data: ~p\n\t ~p\n",
                        [?MODULE, self(), ObjRef, UserData, Event]),
    S.

handle_app_button(#state{server_pid = ServerPid,
			 status_bar = Bar,
			 app_wins = AppWins} = S,
		  Items,
		  Action) ->
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    NewApps = [move_app(S, Item, Action) || Item <- Items],
    case reltool_server:set_apps(ServerPid, NewApps) of
	{ok, _Warnings} ->
	    ok;
	{error, Reason} ->
	    display_message(Reason, ?wxICON_ERROR)
    end,
    [ok = reltool_app_win:refresh(AW#app_win.pid) || AW <- AppWins],
    redraw_apps(S).

do_set_sys(#state{sys = Sys, server_pid = ServerPid, status_bar = Bar} = S) ->
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    Status = reltool_server:set_sys(ServerPid, Sys),
    check_and_refresh(S, Status).

move_app(S, {_ItemNo, AppBase}, Action) ->
    {AppName, _Vsn} = reltool_utils:split_app_name(AppBase),
    {ok, OldApp} = reltool_server:get_app(S#state.server_pid, AppName),
    AppCond =
        case Action of
            whitelist_add ->
                case OldApp#app.incl_cond of
                    include   -> undefined;
                    exclude   -> include;
                    undefined -> include
                end;
            whitelist_del ->
                undefined;
            blacklist_add ->
                exclude;
            blacklist_del ->
                undefined;
            _ ->
                error_logger:format("~w~w got unexpected app "
				    "button event: ~p ~p\n",
                                    [?MODULE, self(), Action, AppBase]),
                OldApp#app.incl_cond
        end,
    OldApp#app{incl_cond = AppCond}.

do_set_app(#state{server_pid = ServerPid,
		  status_bar = Bar,
		  app_wins = AppWins} = S, NewApp) ->
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    Result = reltool_server:set_app(ServerPid, NewApp),
    ReturnApp =
	case Result of
	    {ok, AnalysedApp, _Warnings} ->
		AnalysedApp;
	    {error, Reason} ->
		display_message(Reason, ?wxICON_ERROR),
		{ok,OldApp} = reltool_server:get_app(ServerPid, NewApp#app.name),
		OldApp
	end,
    [ok = reltool_app_win:refresh(AW#app_win.pid) || AW <- AppWins],
    S2 = redraw_apps(S),
    {ok, ReturnApp, S2}.

redraw_apps(#state{server_pid = ServerPid,
                   source = SourceCtrl,
                   whitelist = WhiteCtrl,
                   blacklist = BlackCtrl,
                   derived = DerivedCtrl} = S) ->
    {ok, SourceApps} = reltool_server:get_apps(ServerPid, source),
    {ok, WhiteApps} = reltool_server:get_apps(ServerPid, whitelist),
    {ok, BlackApps} = reltool_server:get_apps(ServerPid, blacklist),
    {ok, DerivedApps} = reltool_server:get_apps(ServerPid, derived),

    BadApps = fun(#app{used_by_apps = UsedBy} = A) when UsedBy =/= [] ->
                      A#app{status = missing};
                 (A) ->
                      A
              end,
    BlackApps2 = lists:map(BadApps, BlackApps),
    redraw_apps(SourceApps, SourceCtrl, ?CROSS_IMAGE, ?WARN_IMAGE),
    WhiteN = redraw_apps(WhiteApps, WhiteCtrl, ?TICK_IMAGE, ?ERR_IMAGE),
    redraw_apps(BlackApps2, BlackCtrl, ?CROSS_IMAGE, ?WARN_IMAGE),
    DerivedN = redraw_apps(DerivedApps, DerivedCtrl, ?TICK_IMAGE, ?ERR_IMAGE),

    WarningsN = redraw_warnings(S),
    WarningText = if WarningsN==1 -> "warning";
		     true -> "warnings"
		  end,
    Status = lists:concat([WhiteN, " whitelisted modules and ",
			   DerivedN, " derived modules, ",
			   WarningsN, " ", WarningText, "."]),
    wxStatusBar:setStatusText(S#state.status_bar, Status),
    S.

redraw_apps(Apps, ListCtrl, OkImage, ErrImage) ->
    do_redraw_apps(ListCtrl, Apps, OkImage, ErrImage).

do_redraw_apps(ListCtrl, [], _OkImage, _ErrImage) ->
    wxListCtrl:deleteAllItems(ListCtrl),
    0;
do_redraw_apps(ListCtrl, Apps, OkImage, ErrImage) ->
    OldItems = reltool_utils:get_items(ListCtrl),
    wxListCtrl:deleteAllItems(ListCtrl),
    AddImage =
        fun(App) ->
                case App#app.status of
                    ok -> {OkImage, App#app.label, App};
                    missing -> {ErrImage, App#app.label, App}
                end
        end,
    ImageApps = lists:map(AddImage, Apps),
    Show =
        fun({ImageId, Text, App}, {Row, ModCount, Items}) ->
                wxListCtrl:insertItem(ListCtrl, Row, ""),
                if (Row rem 2) =:= 0 ->
                        wxListCtrl:setItemBackgroundColour(ListCtrl,
							   Row,
							   {240,240,255});
                   true ->
                        ignore
                end,
                wxListCtrl:setItem(ListCtrl,
				   Row,
				   ?APPS_APP_COL,
				   Text,
				   [{imageId, ImageId}]),
                N = length([M || M <- App#app.mods,
				 M#mod.is_included =:= true]),
                {Row + 1, ModCount + N, [{Row, Text} | Items]}
        end,
    {_, N, NewItems} = wx:foldl(Show, {0, 0, []}, lists:sort(ImageApps)),
    reltool_utils:select_items(ListCtrl, OldItems, lists:reverse(NewItems)),
    N.

update_app_graph(S) ->
    {ok, WhiteApps} = reltool_server:get_apps(S#state.server_pid, whitelist),
    {ok, DerivedApps} = reltool_server:get_apps(S#state.server_pid, derived),

    WhiteNames = [A#app.name || A <- WhiteApps],
    DerivedNames = [A#app.name || A <- DerivedApps],
    Nodes = WhiteNames ++ DerivedNames,
    %% WhiteUses = [N || A <- WhiteApps,
    %% N <- A#app.uses_apps, lists:member(N, Nodes)],
    %% DerivedUses = [N || A <- DerivedApps,
    %% N <- A#app.uses_apps, lists:member(N, Nodes)],

    WhiteLinks = [[A#app.name, U] || A <- WhiteApps,
                                     U <- A#app.uses_apps,
                                     U =/= A#app.name,
                                     lists:member(U, Nodes)],
    DerivedLinks = [[A#app.name, U] || A <- DerivedApps,
                                       U <- A#app.uses_apps,
                                       U =/= A#app.name,
                                       lists:member(U, Nodes)],
    Links = lists:usort(WhiteLinks ++ DerivedLinks),
    %% io:format("Links: ~p\n", [Links]),
    Title = lists:concat([?APPLICATION, " - application graph"]),
    create_fgraph_window(S, Title, Nodes, Links).

update_mod_graph(S) ->
    {ok, WhiteApps} = reltool_server:get_apps(S#state.server_pid, whitelist),
    {ok, DerivedApps} = reltool_server:get_apps(S#state.server_pid, derived),
    WhiteMods = lists:usort([M || A <- WhiteApps,
				  M <- A#app.mods,
				  M#mod.is_included =:= true]),
    DerivedMods = lists:usort([M || A <- DerivedApps,
				    M <- A#app.mods,
				    M#mod.is_included =:= true]),

    WhiteNames = [M#mod.name || M <- WhiteMods],
    DerivedNames = [M#mod.name || M <- DerivedMods],
    Nodes = WhiteNames ++ DerivedNames,

    WhiteLinks = [[M#mod.name, U] || M <- WhiteMods,
                                     U <- M#mod.uses_mods,
                                     U =/= M#mod.name,
                                     lists:member(U, Nodes)],
    DerivedLinks = [[M#mod.name, U] || M <- DerivedMods,
                                       U <- M#mod.uses_mods,
                                       U =/= M#mod.name,
                                       lists:member(U, Nodes)],
    Links = lists:usort(WhiteLinks ++ DerivedLinks),
    %% io:format("Links: ~p\n", [Links]),
    Title = lists:concat([?APPLICATION, " - module graph"]),
    create_fgraph_window(S, Title, Nodes, Links).

create_fgraph_window(S, Title, Nodes, Links) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, []),
    wxFrame:setSize(Frame, {?WIN_WIDTH, ?WIN_HEIGHT}),
    Panel = wxPanel:new(Frame, []),
    Options = [{size, {lists:max([100, ?WIN_WIDTH - 100]), ?WIN_HEIGHT}}],
    {Server, Fgraph} = reltool_fgraph_win:new(Panel, Options),
    Choose = fun(?MISSING_APP_NAME) -> alternate;
                (_) -> default
             end,
    [reltool_fgraph_win:add_node(Server, N, Choose(N)) || N <- Nodes],
    [reltool_fgraph_win:add_link(Server, {From, To}) || [From, To] <- Links],

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Fgraph, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, Sizer),
    %% wxSizer:fit(Sizer, Frame),
    %% wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    FW = #fgraph_win{frame = Frame, pid = Server},
    S#state{fgraph_wins = [FW | S#state.fgraph_wins]}.

reset_config(#state{status_bar = Bar} = S) ->
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    Status = reltool_server:reset_config(S#state.server_pid),
    check_and_refresh(S, Status).

undo_config(#state{status_bar = Bar} = S) ->
    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
    ok = reltool_server:undo_config(S#state.server_pid),
    refresh(S).

load_config(#state{status_bar = Bar, config_file = OldFile} = S) ->
    Style = ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST,
    case select_file(S#state.frame,
		     "Select a file to load the configuration from",
		     OldFile,
		     Style) of
        {ok, NewFile} ->
            wxStatusBar:setStatusText(Bar, "Processing libraries..."),
            Status = reltool_server:load_config(S#state.server_pid, NewFile),
            check_and_refresh(S#state{config_file = NewFile}, Status);
        cancel ->
            S
    end.

save_config(#state{config_file = OldFile} = S, InclDefaults, InclDerivates) ->
    Style = ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT,
    case select_file(S#state.frame,
		     "Select a file to save the configuration to",
		     OldFile,
		     Style) of
        {ok, NewFile} ->
            Status = reltool_server:save_config(S#state.server_pid,
						NewFile,
						InclDefaults,
						InclDerivates),
            check_and_refresh(S#state{config_file = NewFile}, Status);
        cancel ->
            S
    end.

gen_rel_files(#state{status_bar = Bar, target_dir = OldDir} = S) ->
    Style = ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT,
    case select_dir(S#state.frame,
		    "Select a directory to generate rel, script and boot files to",
		    OldDir,
		    Style) of
        {ok, NewDir} ->
	    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
            Status = reltool_server:gen_rel_files(S#state.server_pid, NewDir),
            check_and_refresh(S, Status);
        cancel ->
            S
    end.

gen_target(#state{status_bar = Bar, target_dir = OldDir} = S) ->
    Style = ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT,
    case select_dir(S#state.frame,
		    "Select a directory to generate a target system to",
		    OldDir,
		    Style) of
        {ok, NewDir} ->
	    wxStatusBar:setStatusText(Bar, "Processing libraries..."),
            Status = reltool_server:gen_target(S#state.server_pid, NewDir),
            check_and_refresh(S#state{target_dir = NewDir}, Status);
        cancel ->
            S
    end.

select_file(Frame, Message, DefaultFile, Style) ->
    Dialog = wxFileDialog:new(Frame,
                              [{message, Message},
                               {defaultDir, filename:dirname(DefaultFile)},
                               {defaultFile, filename:basename(DefaultFile)},
                               {style, Style}]),
    Choice =
        case wxMessageDialog:showModal(Dialog) of
            ?wxID_CANCEL ->  cancel;
            ?wxID_OK -> {ok, wxFileDialog:getPath(Dialog)}
        end,
    wxFileDialog:destroy(Dialog),
    Choice.

select_dir(Frame, Message, DefaultDir, Style) ->
    Dialog = wxDirDialog:new(Frame,
                             [{title, Message},
                              {defaultPath, DefaultDir},
                              {style, Style}]),
    Choice =
        case wxMessageDialog:showModal(Dialog) of
            ?wxID_CANCEL ->  cancel;
            ?wxID_OK -> {ok, wxDirDialog:getPath(Dialog)}
        end,
    wxDirDialog:destroy(Dialog),
    Choice.

check_and_refresh(S, Status) ->
    case Status of
        ok ->
            true;
        {ok, _Warnings} ->
	    true;
        {error, Reason} when is_list(Reason) ->
            display_message(Reason, ?wxICON_ERROR),
	    false;
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("Error:\n\n~p\n", [Reason])),
            display_message(Msg, ?wxICON_ERROR),
	    false
    end,
    refresh(S).

refresh(S) ->
    {ok, Sys} = reltool_server:get_sys(S#state.server_pid),
    [ok = reltool_app_win:refresh(AW#app_win.pid) || AW <- S#state.app_wins],
    S2 = S#state{sys = Sys},
    S3 = redraw_libs(S2),
    S4 = redraw_apps(S3),
    redraw_config_page(S4).

question_dialog(Question, Details) ->
    %% Parent = S#state.frame,
    Parent = wx:typeCast(wx:null(), wxWindow),
    %% [{style, ?wxYES_NO bor ?wxICON_ERROR bor ?wx}]),
    DialogStyle = ?wxRESIZE_BORDER bor ?wxCAPTION bor ?wxSYSTEM_MENU bor
	?wxMINIMIZE_BOX bor ?wxMAXIMIZE_BOX bor ?wxCLOSE_BOX,
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Undo dialog",
			  [{style, DialogStyle}]),
    Color = wxWindow:getBackgroundColour(Dialog),
    TextStyle = ?wxTE_READONLY bor ?wxTE_MULTILINE bor ?wxHSCROLL,
    Text1 = wxTextCtrl:new(Dialog, ?wxID_ANY,
			   [{style, ?wxTE_READONLY bor ?wxBORDER_NONE}]),
    wxWindow:setBackgroundColour(Text1, Color),
    wxTextCtrl:appendText(Text1, Question),
    Text2 = wxTextCtrl:new(Dialog, ?wxID_ANY,
			   [{size, {600, 400}}, {style, TextStyle}]),
    wxWindow:setBackgroundColour(Text2, Color),
    wxTextCtrl:appendText(Text2, Details),
    %% wxDialog:setAffirmativeId(Dialog, ?wxID_YES),
    %% wxDialog:setEscapeId(Dialog, ?wxID_NO),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Text1, [{border, 2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Text2, [{border, 2},
			       {flag, ?wxEXPAND},
			       {proportion, 1}]),
    ButtSizer = wxDialog:createStdDialogButtonSizer(Dialog,
						    ?wxOK bor ?wxCANCEL),
    wxSizer:add(Sizer, ButtSizer, [{border, 2}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Dialog, Sizer),
    wxSizer:fit(Sizer, Dialog),
    wxSizer:setSizeHints(Sizer, Dialog),
    Answer = wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    Answer.

display_message(Message, Icon) ->
    Dialog = wxMessageDialog:new(wx:null(),
                                 Message,
                                 [{style, ?wxOK bor Icon}]),
    wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog).

%% Strings = [{Color,String}]
question_dialog_2(DialogLabel, Strings) ->
    %% Parent = S#state.frame,
    Parent = wx:typeCast(wx:null(), wxWindow),
    %% [{style, ?wxYES_NO bor ?wxICON_ERROR bor ?wx}]),
    DialogStyle = ?wxRESIZE_BORDER bor ?wxCAPTION bor ?wxSYSTEM_MENU bor
	?wxMINIMIZE_BOX bor ?wxMAXIMIZE_BOX bor ?wxCLOSE_BOX,
    Dialog = wxDialog:new(Parent, ?wxID_ANY, DialogLabel,
			  [{style, DialogStyle}]),
    Color = wxWindow:getBackgroundColour(Dialog),
    TextStyle = ?wxTE_READONLY bor ?wxTE_MULTILINE bor ?wxHSCROLL,
    Text = wxTextCtrl:new(Dialog, ?wxID_ANY,
			  [{size, {600, 400}}, {style, TextStyle}]),
    wxWindow:setBackgroundColour(Text, Color),
    TextAttr = wxTextAttr:new(),
    add_text(Text,TextAttr,Strings),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Text, [{border, 2}, {flag, ?wxEXPAND}, {proportion, 1}]),
    ButtSizer = wxDialog:createStdDialogButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(Sizer, ButtSizer, [{border, 2}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Dialog, Sizer),
    wxSizer:fit(Sizer, Dialog),
    wxSizer:setSizeHints(Sizer, Dialog),
    Answer = wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    Answer.

add_text(Text,Attr,[{Color,String}|Strings]) ->
    wxTextAttr:setTextColour(Attr, Color),
    wxTextCtrl:setDefaultStyle(Text, Attr),
    wxTextCtrl:appendText(Text, String),
    add_text(Text,Attr,Strings);
add_text(_,_,[]) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(_Parent, _Debug, S) ->
    ?MODULE:loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S,_Module,_OldVsn,_Extra) ->
    {ok, S}.
