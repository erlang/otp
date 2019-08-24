%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(reltool_app_win).

%% Public
-export([start_link/4, raise/1, refresh/1, open_mod/2]).

%% Internal
-export([init/5, loop/1]).

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
         xref_pid,
	 mod_wins,
         sys,
         common,
         app,
         frame,
         panel,
         book,
         status_bar,
         %% page, % apps | source | config
         config_app_global, config_app_local, config_app_local_box,
         config_mod_global, config_mod_local, config_mod_local_box,
	 config_latest, config_selected, config_source_box,

         app_used_by_ctrl, app_required_ctrl, app_incl_ctrl, app_uses_ctrl,
         mods_source_ctrl, mods_white_ctrl, mods_black_ctrl, mods_derived_ctrl,
         deps_used_by_ctrl, deps_uses_ctrl,
         popup_menu}).
-record(mod_win, {name, pid}).

-define(WIN_WIDTH, 800).
-define(WIN_HEIGHT, 600).
%% -define(MODS_MOD_COL_WIDTH, 250).
%% -define(MODS_APP_COL_WIDTH, 250).
%% -define(APPS_APP_COL_WIDTH, 250).

-define(CLOSE_ITEM, ?wxID_EXIT).    %% Use OS specific version if available
-define(ABOUT_ITEM, ?wxID_ABOUT).   %% Use OS specific
-define(CONTENTS_ITEM, 300).

-define(MODS_MOD_COL, 0).
-define(MODS_APP_COL, 1).
-define(APPS_APP_COL, 0).

-define(source, "Available").
-define(whitelist, "Included").
-define(blacklist, "Excluded").
-define(derived, "Derived").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_link(WxEnv, Xref, Common, AppName) ->
    proc_lib:start_link(?MODULE,
			init,
			[self(), WxEnv, Xref, Common, AppName],
			infinity,
			[]).

raise(Pid) ->
    reltool_utils:cast(Pid, raise).

refresh(Pid) ->
    reltool_utils:cast(Pid, refresh).

open_mod(Pid, ModName) ->
    reltool_utils:call(Pid, {open_mod, ModName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init(Parent, WxEnv, Xref, C, AppName) ->
    try
	do_init(Parent, WxEnv, Xref, C, AppName)
    catch
	error:Reason:Stacktrace ->
	    exit({Reason, Stacktrace})
    end.

do_init(Parent, WxEnv, Xref, C, AppName) ->
    process_flag(trap_exit, C#common.trap_exit),
    {ok, App} = reltool_server:get_app(Xref, AppName),
    {ok, Sys} = reltool_server:get_sys(Xref),
    S = #state{parent_pid = Parent,
	       xref_pid = Xref,
	       mod_wins = [],
	       sys = Sys,
	       common = C,
	       app = App},
    proc_lib:init_ack(Parent, {ok, self()}),
    wx:set_env(WxEnv),
    wx:debug(C#common.wx_debug),
    S2 = wx:batch(fun() -> create_window(S) end),
    loop(S2).

loop(#state{xref_pid = Xref, common = C, app = App} = S) ->
    receive
        {system, From, Msg} ->
            Dbg = C#common.sys_debug,
            sys:handle_system_msg(Msg,
				  From,
				  S#state.parent_pid,
				  ?MODULE,
				  Dbg,
				  S);
        {cast, _From, raise} ->
            wxFrame:raise(S#state.frame),
            wxFrame:setFocus(S#state.frame),
            ?MODULE:loop(S);
        {cast, _From, refresh} ->
	    case reltool_server:get_app(Xref, App#app.name) of
		{ok, App2} ->
		    {ok, Sys} = reltool_server:get_sys(Xref),
		    S2 = redraw_window(S#state{sys = Sys, app = App2}),
		    [ok = reltool_mod_win:refresh(MW#mod_win.pid) ||
			MW <- S2#state.mod_wins],
		    ?MODULE:loop(S2);
		{error, _Reason} ->
		    wxFrame:destroy(S#state.frame),
		    exit(shutdown)
	    end;
        {call, ReplyTo, Ref, {open_mod, ModName}} ->
	    S2 = create_mod_window(S, ModName),
	    {value, #mod_win{pid = ModPid}} =
		lists:keysearch(ModName, #mod_win.name, S2#state.mod_wins),
	    reltool_utils:reply(ReplyTo, Ref, {ok, ModPid}),
	    ?MODULE:loop(S2);
	#wx{event = #wxSize{}} = Wx ->
	    Wx2 = reltool_utils:get_latest_resize(Wx),
	    S2 = handle_event(S, Wx2),
	    ?MODULE:loop(S2);
        #wx{obj = ObjRef,
            event = #wxClose{type = close_window}} ->
            wxFrame:destroy(ObjRef),
            exit(shutdown);
        #wx{} = Wx ->
            S2 = handle_event(S, Wx),
            ?MODULE:loop(S2);
        {'EXIT', Pid, Reason} when Pid =:= S#state.parent_pid ->
            exit(Reason);
        {'EXIT', Pid, _Reason} = Exit ->
	    exit_warning(Exit),
            S2 = S#state{mod_wins = lists:keydelete(Pid,
						    #mod_win.pid,
						    S#state.mod_wins)},
            ?MODULE:loop(S2);
        Msg ->
            error_logger:format("~w~w got unexpected message:\n\t~tp\n",
                                [?MODULE, self(), Msg]),
            ?MODULE:loop(S)
    end.

exit_warning({'EXIT', _Pid, shutdown}) ->
    ok;
exit_warning({'EXIT', _Pid, _Reason} = Msg) ->
    error_logger:format("~w~w got unexpected message:\n\t~tp\n",
			[?MODULE, self(), Msg]).

create_window(#state{app = App} = S) ->
    Title = app_title(App),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, []),
    %% wxFrame:setSize(Frame, {?WIN_WIDTH, ?WIN_HEIGHT}),
    Panel = wxPanel:new(Frame, []),
    StatusBar = wxFrame:createStatusBar(Frame,[]),

    Book = wxNotebook:new(Panel, ?wxID_ANY, []),

    S2 = S#state{frame = Frame,
                 panel = Panel,
                 book = Book,
                 status_bar = StatusBar},
    Derived = app_to_mods(S2),
    S3 = create_mods_page(S2, Derived),
    S4 = create_apps_page(S3, Derived),
    S5 = create_deps_page(S4, Derived),
    S6 = create_config_page(S5),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Book, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:show(Frame),

    wxFrame:connect(Frame, close_window),
    S6.

app_title(App) ->
    lists:concat([?APPLICATION, " - ", App#app.label]).

create_apps_page(S, Derived) ->
    Panel = wxPanel:new(S#state.book, []),
    Main = wxBoxSizer:new(?wxVERTICAL),
    Upper = wxBoxSizer:new(?wxHORIZONTAL),
    Lower = wxBoxSizer:new(?wxHORIZONTAL),

    UsedByCtrl = create_apps_list_ctrl(Panel, Upper, "Used by"),
    wxSizer:add(Upper,
		wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),

    RequiredCtrl = create_apps_list_ctrl(Panel, Upper, "Required"),
    wxSizer:add(Upper, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    InclCtrl = create_apps_list_ctrl(Panel, Upper, "Included"),
    wxSizer:add(Upper, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    UsesCtrl = create_apps_list_ctrl(Panel, Upper, "Uses"),
    S2 = S#state{app_required_ctrl = RequiredCtrl,
		 app_used_by_ctrl = UsedByCtrl,
		 app_incl_ctrl = InclCtrl,
		 app_uses_ctrl = UsesCtrl},
    redraw_apps(S2, Derived),
    wxSizer:add(Main, Upper,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),

    wxSizer:add(Main, Lower,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Main),
    wxNotebook:addPage(S2#state.book, Panel, "Application dependencies", []),
    S2.

create_apps_list_ctrl(Panel, Sizer, Text) ->
    Width = lists:max([100, ?WIN_WIDTH - 40]) div 4,
    Height = lists:max([100, ?WIN_HEIGHT - 100]),
    ListCtrl = wxListCtrl:new(Panel,
			      [{style,
				?wxLC_REPORT bor
				%% ?wxLC_SORT_ASCENDING bor
				?wxLC_SINGLE_SEL bor
				?wxHSCROLL bor
				?wxVSCROLL},
			       {size, {Width, Height}}
			      ]),

    %% Prep images
    reltool_utils:assign_image_list(ListCtrl),

    %% Prep column label
    ListItem  = wxListItem:new(),
    wxListItem:setAlign(ListItem, ?wxLIST_FORMAT_LEFT),
    wxListItem:setText(ListItem, Text),
    wxListItem:setWidth(ListItem, reltool_utils:get_column_width(ListCtrl)),
    wxListCtrl:insertColumn(ListCtrl, ?APPS_APP_COL, ListItem),
    wxListItem:destroy(ListItem),

    wxSizer:add(Sizer, ListCtrl,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxEvtHandler:connect(ListCtrl, size,
			 [{skip, true}, {userData, apps_list_ctrl}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated,
		       [{userData, open_app}]),
    wxWindow:connect(ListCtrl, enter_window),
    ListCtrl.

create_deps_page(S, Derived) ->
    Panel = wxPanel:new(S#state.book, []),
    Main = wxBoxSizer:new(?wxHORIZONTAL),

    UsedByCtrl = create_mods_list_ctrl(Panel,
				       Main,
				       "Modules using this",
				       " and their applications",
				       undefined,
				       undefined),
    wxSizer:add(Main, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    UsesCtrl   = create_mods_list_ctrl(Panel,
				       Main,
				       "Used modules",
				       " and their applications",
				       undefined,
				       undefined),
    S2 = S#state{deps_used_by_ctrl = UsedByCtrl,
                 deps_uses_ctrl = UsesCtrl},
    redraw_mods(S2, Derived),
    wxPanel:setSizer(Panel, Main),
    wxNotebook:addPage(S2#state.book, Panel, "Module dependencies", []),
    S2.

create_mods_page(S, Derived) ->
    Panel = wxPanel:new(S#state.book, []),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),

    SourceCtrl = create_mods_list_ctrl(Panel,
				       MainSz,
				       ?source,
				       "",
				       whitelist_add,
				       blacklist_add),
    wxSizer:add(MainSz, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    WhiteCtrl = create_mods_list_ctrl(Panel,
				      MainSz,
				      ?whitelist,
				      "",
				      whitelist_del,
				      blacklist_add),
    wxSizer:add(MainSz, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    BlackCtrl = create_mods_list_ctrl(Panel,
				      MainSz,
				      ?blacklist,
				      "",
				      whitelist_add,
				      blacklist_del),
    wxSizer:add(MainSz, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    DerivedCtrl = create_mods_list_ctrl(Panel,
					MainSz,
					?derived,
					"",
					whitelist_add,
					blacklist_add),
    S2 = S#state{mods_source_ctrl  = SourceCtrl,
                 mods_white_ctrl   = WhiteCtrl,
                 mods_black_ctrl   = BlackCtrl,
                 mods_derived_ctrl = DerivedCtrl},
    redraw_mods(S2, Derived),
    wxPanel:setSizer(Panel, MainSz),
    wxNotebook:addPage(S2#state.book, Panel, "Modules", []),
    S2.

create_mods_list_ctrl(Panel, OuterSz, Title, AppText, Tick, Cross) ->
    ListCtrl = wxListCtrl:new(Panel,
                              [{style,
                                ?wxLC_REPORT bor
                                %% ?wxLC_SORT_ASCENDING bor
                                %% ?wxLC_SINGLE_SEL bor
                                ?wxHSCROLL bor
				?wxVSCROLL}]),
    ToolTip = "Select module(s) or open separate module "
	"window with a double click.",
    wxListCtrl:setToolTip(ListCtrl, ToolTip),

    %% Prep images
    reltool_utils:assign_image_list(ListCtrl),

    %% Prep column label
    ListItem  = wxListItem:new(),
    wxListItem:setAlign(ListItem, ?wxLIST_FORMAT_LEFT),
    wxListItem:setText(ListItem, Title),
    wxListCtrl:insertColumn(ListCtrl, ?MODS_MOD_COL, ListItem),
    %% wxListCtrl:setColumnWidth(ListCtrl, ?MODS_MOD_COL, ?MODS_MOD_COL_WIDTH),
    Prop =
        case AppText =/= "" of
            true  ->
                wxListItem:setText(ListItem, AppText),
                wxListCtrl:insertColumn(ListCtrl, ?MODS_APP_COL, ListItem),
                %% wxListCtrl:setColumnWidth(ListCtrl, ?MODS_APP_COL,
		%% ?MODS_APP_COL_WIDTH),
                2;
            false ->
                1
        end,
    wxListItem:destroy(ListItem),

    ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
    create_button(Panel, ButtonSz, ListCtrl, Title, "wxART_TICK_MARK", Tick),
    create_button(Panel, ButtonSz, ListCtrl, Title, "wxART_CROSS_MARK", Cross),
    wxEvtHandler:connect(ListCtrl, size,
			 [{skip, true}, {userData, mods_list_ctrl}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated,
		       [{userData, open_mod}]),
    wxWindow:connect(ListCtrl, enter_window),
    InnerSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(InnerSz, ListCtrl,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    wxSizer:add(InnerSz, ButtonSz,
                [{flag, ?wxALL bor ?wxEXPAND}]),
    wxSizer:add(OuterSz, InnerSz,
                [{flag, ?wxALL bor ?wxEXPAND},
                 {proportion, Prop}]),
    ListCtrl.

create_button(_Panel, Sizer, _ListCtrl, _Title, _BitMapName, undefined) ->
    wxSizer:addStretchSpacer(Sizer);
create_button(Panel, Sizer, ListCtrl, Title, BitMapName, Action) ->
    %% InnerSz = wxBoxSizer:new(?wxVERTICAL),
    BitMap = wxArtProvider:getBitmap(BitMapName),
    Button = wxBitmapButton:new(Panel, ?wxID_ANY, BitMap, []),
    ToolTip = action_to_tool_tip(Title, Action),
    wxBitmapButton:setToolTip(Button, ToolTip),
    %% wxSizer:add(InnerSz, Button, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
    Opts = [{userData, {mod_button, Action, ListCtrl}}],
    wxEvtHandler:connect(Button, command_button_clicked, Opts),
    wxSizer:add(Sizer, Button,
                [{border, 2},
                 {flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL},
                 {proportion, 1}]).

action_to_tool_tip(Label, Action) ->
    case Action of
	whitelist_add when Label =:= ?whitelist ->
	    "Remove selected module(s) from whitelist.";
	whitelist_add ->
	    "Add selected module(s) to whitelist.";
	whitelist_del ->
	    "Remove selected module(s)from whitelist.";
	blacklist_add when Label =:= ?blacklist ->
	    "Remove selected module(s) from blacklist.";
	blacklist_add ->
	    "Add selected module(s) to blacklist.";
	blacklist_del ->
	    "Remove selected module(s) from blacklist."
    end.

create_config_page(#state{app = App} = S) ->
    Panel = wxPanel:new(S#state.book, []),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),

    %% Source dirs
    {LatestRadio, SelectedRadio, SourceBox} =
	create_double_box(Panel,
			  TopSizer,
			  "Source selection policy",
			  "Use latest version",
			  use_latest_vsn,
			  "Use selected version",
			  use_selected_vsn,
			  "Directories",
			  App#app.sorted_dirs,
			  version),

    InclSizer = wxBoxSizer:new(?wxHORIZONTAL),

    %% Application inclusion
    {AppGlobalRadio, AppLocalRadio, AppLocalBox} =
	create_double_box(Panel,
			  InclSizer,
			  "Application inclusion policy",
			  "Use global config",
			  global_incl_cond,
			  "Use application specific config",
			  local_incl_cond,
			  "Application specific",
			  reltool_utils:incl_conds(),
			  incl_cond),

    %% Module inclusion
    {ModGlobalRadio, ModLocalRadio, ModLocalBox} =
	create_double_box(Panel,
			  InclSizer,
			  "Module inclusion policy",
			  "Use global config",
			  global_mod_cond,
			  "Use application specific config",
			  local_mod_cond,
			  "Application specific",
			  reltool_utils:mod_conds(),
			  mod_cond),
    wxSizer:add(TopSizer, InclSizer,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}, {proportion, 1}]),

    S2 = S#state{config_app_global = AppGlobalRadio,
                 config_app_local = AppLocalRadio,
                 config_app_local_box = AppLocalBox,
		 config_mod_global = ModGlobalRadio,
                 config_mod_local = ModLocalRadio,
                 config_mod_local_box = ModLocalBox,
		 config_latest = LatestRadio,
		 config_selected = SelectedRadio,
		 config_source_box = SourceBox},
    redraw_config(S2),
    wxPanel:setSizer(Panel, TopSizer),
    wxNotebook:addPage(S2#state.book, Panel, "Application settings", []),
    S2.

create_double_box(Panel, Sizer, TopLabel,
		  OuterText, OuterData,
		  InnerText, InnerData,
		  InternalLabel, InternalChoices, InternalChoiceData) ->
    TopSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
				    [{label, TopLabel}]),
    OuterSizer = wxBoxSizer:new(?wxVERTICAL),
    OuterRadio = wxRadioButton:new(Panel, ?wxID_ANY, OuterText,
				    [{style, ?wxRB_GROUP}]),
    wxEvtHandler:connect(OuterRadio, command_radiobutton_selected,
			 [{userData, OuterData}]),
    InnerRadio = wxRadioButton:new(Panel, ?wxID_ANY, InnerText),
    wxEvtHandler:connect(InnerRadio, command_radiobutton_selected,
			 [{userData, InnerData}]),
    InnerBox = wxRadioBox:new(Panel,
			      ?wxID_ANY,
			      InternalLabel,
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      InternalChoices,
			      []),
    wxEvtHandler:connect(InnerBox, command_radiobox_selected,
			 [{userData, InternalChoiceData}]),
    wxSizer:add(OuterSizer, OuterRadio,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    wxSizer:add(OuterSizer, InnerRadio,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    wxSizer:add(TopSizer, OuterSizer,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    wxSizer:add(TopSizer, InnerBox,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, TopSizer,
                [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}, {proportion, 1}]),
    {OuterRadio, InnerRadio, InnerBox}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#state{sys = Sys, app = App} = S, Wx) ->
    %% io:format("wx: ~p\n", [Wx]),
    case Wx of
	#wx{obj = ObjRef, event = #wxMouse{type = enter_window}} ->
	    wxWindow:setFocus(ObjRef),
	    S;
	#wx{obj= ListCtrl,
	    userData = mods_list_ctrl,
	    event = #wxSize{type = size, size = {W, _H}}} ->
	    HasApps = (wxListCtrl:getColumnCount(ListCtrl) > 1),
	    case HasApps of
		false ->
		    wxListCtrl:setColumnWidth(ListCtrl, ?MODS_MOD_COL, W);
		true ->
		    wxListCtrl:setColumnWidth(ListCtrl,
					      ?MODS_MOD_COL,
					      (2 * W) div 3),
		    wxListCtrl:setColumnWidth(ListCtrl, ?MODS_APP_COL, W div 3)
	    end,
	    S;
	#wx{obj = ListCtrl,
	    userData = apps_list_ctrl,
	    event = #wxSize{type = size, size = {W, _H}}} ->
	    wxListCtrl:setColumnWidth(ListCtrl, ?APPS_APP_COL, W),
	    S;
        #wx{userData = open_app,
            obj = ListCtrl,
            event = #wxList{type = command_list_item_activated,
			    itemIndex = Pos}} ->
            AppBase = wxListCtrl:getItemText(ListCtrl, Pos),
	    {AppName, _AppVsn} = reltool_utils:split_app_name(AppBase),
            {ok, _AppPid} = reltool_sys_win:open_app(S#state.parent_pid,
						     AppName),
            S;
        #wx{userData = open_mod,
            obj = ListCtrl,
            event = #wxList{type = command_list_item_activated,
			    itemIndex = Pos}} ->
            ModName = list_to_atom(wxListCtrl:getItemText(ListCtrl, Pos)),
	    create_mod_window(S, ModName);
        #wx{userData = global_incl_cond} ->
            %% Use global setting
            change_incl_cond(S, App, undefined);
        #wx{userData = local_incl_cond} ->
            %% Use app spec setting
            change_incl_cond(S, App, Sys#sys.incl_cond);
        #wx{userData = incl_cond,
            %% Change app spec setting
            event = #wxCommand{type = command_radiobox_selected,
                               cmdString = Sel}} ->
            AppCond = reltool_utils:list_to_incl_cond(Sel),
            change_incl_cond(S, App, AppCond);

        #wx{userData = global_mod_cond} ->
            %% Use global setting
            change_mod_cond(S, App, undefined);
        #wx{userData = local_mod_cond} ->
            %% Use app spec setting
            change_mod_cond(S, App, Sys#sys.mod_cond);
        #wx{userData = mod_cond,
            %% Change app spec setting
            event = #wxCommand{type = command_radiobox_selected,
                               cmdString = Sel}} ->
            ModCond = reltool_utils:list_to_mod_cond(Sel),
            change_mod_cond(S, App, ModCond);

        #wx{userData = use_latest_vsn} ->
            %% Use latest version
	    App2 = App#app{use_selected_vsn = undefined},
	    S2 = change_version(S, App2, App#app.active_dir),
            redraw_window(S2);
        #wx{userData = use_selected_vsn} ->
            %% Use selected version
	    App2 = App#app{use_selected_vsn = dir},
	    {ok, App3} = reltool_sys_win:set_app(S#state.parent_pid, App2),
	    S2 = S#state{app = App3},
	    redraw_window(S2);
        #wx{userData = version,
            event = #wxCommand{type = command_radiobox_selected,
                               cmdString = ActiveDir}} ->
            %% Change app source
	    App2 = App#app{use_selected_vsn = dir},
	    S2 = change_version(S, App2, ActiveDir),
            redraw_window(S2);
        #wx{userData = {mod_button, Action, ListCtrl},
            event = #wxCommand{type = command_button_clicked}} ->
            Items = reltool_utils:get_items(ListCtrl),
	    handle_mod_button(S, Items, Action);
        _ ->
            error_logger:format("~w~w got unexpected app event from "
				"wx:\n\t~tp\n",
                                [?MODULE, self(), Wx]),
            S
    end.

create_mod_window(#state{parent_pid = RelPid, xref_pid = Xref, common = C} = S,
		  ModName) ->
    case lists:keysearch(ModName, #mod_win.name, S#state.mod_wins) of
        false ->
            WxEnv = wx:get_env(),
            {ok, Pid} =
		reltool_mod_win:start_link(WxEnv, Xref, RelPid, C, ModName),
            MW = #mod_win{name = ModName, pid = Pid},
            S#state{mod_wins = [MW | S#state.mod_wins]};
        {value, MW} ->
            reltool_app_win:raise(MW#mod_win.pid),
            S
    end.

handle_mod_button(#state{app = App} = S, Items, Action) ->
    App2 = lists:foldl(fun(Item, A) -> move_mod(A, Item, Action) end,
		       App,
		       Items),
    {ok, App3} = reltool_sys_win:set_app(S#state.parent_pid, App2),
    S2 = S#state{app = App3},
    redraw_window(S2).

move_mod(App, {_ItemNo, ModStr}, Action) ->
    ModName = list_to_atom(ModStr),
    Mods = App#app.mods,
    {value, M} = lists:keysearch(ModName, #mod.name, Mods),
    AppCond =
	case Action of
	    whitelist_add ->
		case M#mod.incl_cond of
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
		error_logger:format("~w~w got unexpected mod "
				    "button event: ~w\n\t ~tp\n",
				    [?MODULE, self(), ModName, Action]),
		M#mod.incl_cond
	end,
    M2 = M#mod{incl_cond = AppCond},
    Mods2 = lists:keystore(ModName, #mod.name, Mods, M2),
    App#app{mods = Mods2}.

change_incl_cond(S, App, NewAppCond) ->
    App2 = App#app{incl_cond = NewAppCond},
    {ok, App3} = reltool_sys_win:set_app(S#state.parent_pid, App2),
    S2 = S#state{app = App3},
    redraw_window(S2).

change_mod_cond(S, App, NewModCond) ->
    App2 = App#app{mod_cond = NewModCond},
    {ok, App3} = reltool_sys_win:set_app(S#state.parent_pid, App2),
    S2 = S#state{app = App3},
    redraw_window(S2).

change_version(S, App, NewDir) ->
    App2 = App#app{active_dir = NewDir,
		   label = undefined,
		   vsn = undefined,
		   info = undefined},
    {ok, App3} = reltool_sys_win:set_app(S#state.parent_pid, App2),
    Title = app_title(App3),
    wxFrame:setTitle(S#state.frame, Title),
    S#state{app = App3}.

redraw_apps(#state{app = #app{info = AppInfo},
                   app_used_by_ctrl = UsedByCtrl,
                   app_required_ctrl = RequiredCtrl,
                   app_incl_ctrl = InclCtrl,
                   app_uses_ctrl = UsesCtrl,
		   xref_pid = Xref},
	    {_SourceMods,
	     _WhiteMods,
	     _BlackMods,
	     _DerivedMods,
	     UsedByMods,
	     UsesMods}) ->
    UsedByApps =
	lists:usort([{M#mod.app_name, Image} || {Image, _, M} <- UsedByMods]),
    Select =
	fun(AppName) ->
		{ok, App} = reltool_server:get_app(Xref, AppName),
		case App#app.status of
		    missing -> {AppName, ?ERR_IMAGE};
		    ok      -> {AppName, ?TICK_IMAGE}
		end
	end,
    RequiredApps = lists:sort(lists:map(Select, AppInfo#app_info.applications)),
    InclApps = lists:map(Select, AppInfo#app_info.incl_apps),
    UsesApps =
	lists:usort([{M#mod.app_name, Image} || {Image, _, M} <- UsesMods]),
    do_redraw_apps(UsedByCtrl, UsedByApps),
    do_redraw_apps(RequiredCtrl, RequiredApps),
    do_redraw_apps(InclCtrl, InclApps),
    do_redraw_apps(UsesCtrl, UsesApps),
    ok.

do_redraw_apps(ListCtrl, []) ->
    wxListCtrl:deleteAllItems(ListCtrl);
    %% wxListCtrl:setColumnWidth(ListCtrl, ?APPS_APP_COL,
%% ?wxLIST_AUTOSIZE_USEHEADER);
do_redraw_apps(ListCtrl, AppImages) ->
    wxListCtrl:deleteAllItems(ListCtrl),
    Add =
        fun({AppName, ImageId}, {Row, Prev}) when AppName =/= Prev ->
                wxListCtrl:insertItem(ListCtrl, Row, ""),
                if (Row rem 2) =:= 0 ->
                        wxListCtrl:setItemBackgroundColour(ListCtrl,
							   Row,
							   {240,240,255});
                   true ->
                        ignore
                end,
                Str = atom_to_list(AppName),
                wxListCtrl:setItem(ListCtrl,
				   Row,
				   ?APPS_APP_COL,
				   Str,
				   [{imageId, ImageId}]),
                {Row + 1, AppName};
	   ({_, _}, Acc) ->
		Acc
        end,
    wx:foldl(Add, {0, undefined}, AppImages).

%% print(X, X, Format, Args) ->
%%     io:format(Format, Args);
%% print(_, _, _, _) ->
%%     ok.

redraw_mods(#state{mods_source_ctrl = SourceCtrl,
                   mods_white_ctrl = WhiteCtrl,
                   mods_black_ctrl = BlackCtrl,
                   mods_derived_ctrl = DerivedCtrl,
                   deps_used_by_ctrl = UsedByCtrl,
                   deps_uses_ctrl = UsesCtrl,
		   app = #app{is_pre_included = IsPre, is_included = IsIncl},
		   status_bar = Bar},
	    {SourceMods,
	     WhiteMods,
	     BlackMods,
	     DerivedMods,
	     UsedByMods,
	     UsesMods}) ->
    InclStatus =
	case IsIncl of
	    true when IsPre =:= true -> "Whitelist - ";
	    true -> "Derived - ";
	    false -> "Blacklist - ";
	    undefined -> "Source - "
	end,
    Status = lists:concat([InclStatus,
			   length(WhiteMods), " whitelisted modules and ",
			   length(DerivedMods), " derived modules."]),
    wxStatusBar:setStatusText(Bar, Status),
    opt_redraw_mods(SourceCtrl, SourceMods),
    opt_redraw_mods(WhiteCtrl, WhiteMods),
    opt_redraw_mods(BlackCtrl, BlackMods),
    opt_redraw_mods(DerivedCtrl, DerivedMods),
    opt_redraw_mods(UsedByCtrl, UsedByMods),
    opt_redraw_mods(UsesCtrl, UsesMods).

app_to_mods(#state{xref_pid = Xref, app = App}) ->
    SourceMods  = [M || M <- App#app.mods,
			M#mod.is_included =/= true,
			M#mod.is_pre_included =/= false],
    WhiteMods = [M || M <- App#app.mods,
                      M#mod.is_pre_included =:= true],
    BlackMods = [M || M <- App#app.mods,
                      M#mod.is_pre_included =:= false],
    DerivedMods  = [M || M <- App#app.mods,
                         M#mod.is_included =:= true,
                         M#mod.is_pre_included =/= true],
    GetMod =
        fun(ModName) when is_atom(ModName) ->
                {ok, M} = reltool_server:get_mod(Xref, ModName),
		if
		    M#mod.app_name =:= App#app.name,
		    M#mod.is_included =:= true ->
			false;
		    true ->
			{true, M}
		end
        end,
    UsedByMods = lists:zf(GetMod, App#app.used_by_mods),
    UsesMods = lists:zf(GetMod, App#app.uses_mods),
    {
     [select_image(source,    M) || M <- SourceMods],
     [select_image(whitelist, M) || M <- WhiteMods],
     [select_image(blacklist, M) || M <- BlackMods],
     [select_image(derived,   M) || M <- DerivedMods],
     [select_image(used_by,   M) || M <- UsedByMods],
     [select_image(uses,      M) || M <- UsesMods]
    }.

select_image(Kind, M) ->
    Image =
	case Kind of
	    blacklist when M#mod.status =:= missing ->
		?WARN_IMAGE;
	    source when M#mod.status =:= missing ->
		?WARN_IMAGE;
	    _ when M#mod.status =:= missing ->
		?ERR_IMAGE;
	    blacklist when M#mod.incl_cond =:= exclude ->
		?CROSS_IMAGE;
	    blacklist ->
		?SOURCE_IMAGE;
	    source ->
		?CROSS_IMAGE;
	    whitelist when M#mod.incl_cond =:= include ->
		?TICK_IMAGE;
	    whitelist ->
		?SOURCE_IMAGE;
	    derived ->
		?TICK_IMAGE;
	    used_by when M#mod.is_included =:= true ->
		?TICK_IMAGE;
	    used_by when M#mod.is_included =:= false ->
		?WARN_IMAGE;
	    used_by ->
		?ERR_IMAGE;
	    uses when M#mod.is_included =:= true ->
		?TICK_IMAGE;
	    uses when M#mod.is_included =:= false ->
		?WARN_IMAGE;
	    uses ->
		?ERR_IMAGE
        end,
    {Image, M#mod.app_name, M}.

opt_redraw_mods(undefined, _ImageMods) ->
    ok;
opt_redraw_mods(ListCtrl, ImageMods) ->
    HasApps = (wxListCtrl:getColumnCount(ListCtrl) > 1),
    do_redraw_mods(ListCtrl, ImageMods, HasApps).

do_redraw_mods(ListCtrl, [], _HasApps) ->
    wxListCtrl:deleteAllItems(ListCtrl);
do_redraw_mods(ListCtrl, ImageMods, HasApps) ->
    wxListCtrl:deleteAllItems(ListCtrl),
    Add =
        fun({ImageId, AppName, #mod{name = ModName}}, Row) ->
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
				   ?MODS_MOD_COL,
				   atom_to_list(ModName),
				   [{imageId, ImageId}]),
                case HasApps of
                    false ->
                        ok;
                    true ->
                        wxListCtrl:setItem(ListCtrl,
					   Row,
					   ?MODS_APP_COL,
					   atom_to_list(AppName),
					   [{imageId, ImageId}])
                end,
                Row + 1
        end,
    wx:foldl(Add, 0, lists:sort(ImageMods)).

redraw_config(#state{sys = #sys{incl_cond = GlobalIncl,
				mod_cond = GlobalSource},
		     app = #app{incl_cond = LocalIncl,
				mod_cond = LocalSource,
				use_selected_vsn = UseSelected,
				active_dir = ActiveDir,
				sorted_dirs = SortedDirs},
		     config_app_global = AppGlobalRadio,
		     config_app_local = AppLocalRadio,
		     config_app_local_box = AppLocalBox,
		     config_mod_global = ModGlobalRadio,
		     config_mod_local = ModLocalRadio,
		     config_mod_local_box = ModLocalBox,
		     config_latest = LatestRadio,
		     config_selected = SelectedRadio,
		     config_source_box = SourceBox}) ->
    redraw_double_box(GlobalIncl,
		      LocalIncl,
		      AppGlobalRadio,
		      AppLocalRadio,
		      AppLocalBox,
		      fun reltool_utils:incl_cond_to_index/1),
    redraw_double_box(GlobalSource,
		      LocalSource,
		      ModGlobalRadio,
		      ModLocalRadio,
		      ModLocalBox,
		      fun reltool_utils:mod_cond_to_index/1),
    redraw_double_box(false,
		      UseSelected,
		      LatestRadio,
		      SelectedRadio,
		      SourceBox,
		      fun(false) ->
			      0;
			 (_) ->
			      reltool_utils:elem_to_index(ActiveDir,
							  SortedDirs) - 1
		      end).

redraw_double_box(Global, Local, GlobalRadio, LocalRadio, LocalBox, GetChoice) ->
    AppCond =
        case Local of
            undefined ->
                wxRadioButton:setValue(GlobalRadio, true),
                wxRadioButton:setValue(LocalRadio, false),
                wxRadioBox:disable(LocalBox),
                Global;
            _ ->
                wxRadioButton:setValue(GlobalRadio, false),
                wxRadioButton:setValue(LocalRadio, true),
                wxRadioBox:enable(LocalBox),
                Local
        end,
    Choice = GetChoice(AppCond),
    wxRadioBox:setSelection(LocalBox, Choice).

redraw_window(S) ->
    %% wx_misc:beginBusyCursor(),
    Derived = app_to_mods(S),
    redraw_config(S),
    redraw_mods(S, Derived),
    redraw_apps(S, Derived),
    %% wx_misc:endBusyCursor(),
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(_Parent, _Debug, S) ->
    ?MODULE:loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S,_Module,_OldVsn,_Extra) ->
    {ok, S}.
