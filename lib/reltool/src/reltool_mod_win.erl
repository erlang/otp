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

-module(reltool_mod_win).

%% Public
-export([start_link/5, raise/1, refresh/1]).

%% Internal
-export([init/6, loop/1]).

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
	 rel_pid,
	 mod_wins,
         sys,
         common,
         mod,
	 name,
         frame,
         panel,
         book,
         status_bar,
         deps_used_by_ctrl,
	 deps_uses_ctrl,
         popup_menu,
	 active_page,
	 code_pages}).

-record(code_page,
	{name,
	 editor,
	 find_objs,
	 find_data}).

-record(find_objs,
	{search,               %  Search input ctrl
	 goto,                 %  Goto  input ctrl
	 radio}).              %  Radio buttons

-record(find_data,
	{start,              % start pos
	 found,              % status
	 history}).          % list of recent positions
	
-define(WIN_WIDTH, 800).
-define(WIN_HEIGHT, 600).

-define(CLOSE_ITEM, ?wxID_EXIT).    %% Use OS specific version if available
-define(ABOUT_ITEM, ?wxID_ABOUT).   %% Use OS specific
-define(CONTENTS_ITEM, 300).
-define(SEARCH_ENTRY,   413).
-define(GOTO_ENTRY,     414).

-define(MODS_MOD_COL, 0).
-define(MODS_APP_COL, 1).

-define(INITIAL_CODE_PAGE_NAME, "Code").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_link(WxEnv, Xref, RelPid, Common, ModName) ->
    proc_lib:start_link(?MODULE,
			init,
			[self(), WxEnv, Xref, RelPid, Common, ModName],
			infinity,
			[]).

raise(Pid) ->
    reltool_utils:cast(Pid, raise).

refresh(Pid) ->
    reltool_utils:cast(Pid, refresh).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init(Parent, WxEnv, Xref, RelPid, C, ModName) ->
    try
	do_init(Parent, WxEnv, Xref, RelPid, C, ModName)
    catch
	error:Reason ->
	    exit({Reason, erlang:get_stacktrace()})
    end.

do_init(Parent, WxEnv, Xref, RelPid, C, ModName) ->
    process_flag(trap_exit, C#common.trap_exit),
    {ok, Mod} = reltool_server:get_mod(Xref, ModName),
    {ok, Sys} = reltool_server:get_sys(Xref),
    S = #state{parent_pid = Parent,
	       xref_pid = Xref,
	       rel_pid = RelPid,
	       sys = Sys,
	       mod = Mod,
	       name = atom_to_list(Mod#mod.name),
	       common = C},
    proc_lib:init_ack(Parent, {ok, self()}),
    wx:set_env(WxEnv),
    wx:debug(C#common.wx_debug),
    S2 = wx:batch(fun() -> create_window(S) end),
    loop(S2).

loop(#state{xref_pid = Xref, common = C, mod = Mod} = S) ->
    receive
	Msg ->
	    %% io:format("~ts~w -> ~p\n", [S#state.name, self(), Msg]),
	    case Msg of
		{system, From, SysMsg} ->
		    Dbg = C#common.sys_debug,
		    sys:handle_system_msg(SysMsg,
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
		    %% wx_misc:beginBusyCursor(),
		    case reltool_server:get_mod(Xref, Mod#mod.name) of
			{ok, Mod2} ->
			    {ok, Sys} = reltool_server:get_sys(Xref),
			    S2 = redraw_window(S#state{sys = Sys, mod = Mod2}),
			    %% wx_misc:endBusyCursor(),
			    ?MODULE:loop(S2);
			{error, _} ->
			    wxFrame:destroy(S#state.frame),
			    exit(shutdown)
		    end;
		{'EXIT', Pid, Reason} when Pid =:= S#state.parent_pid ->
		    exit(Reason);
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
		_ ->
		    error_logger:format("~w~w got unexpected message:\n\t~p\n",
					[?MODULE, self(), Msg]),
		    ?MODULE:loop(S)
	    end
    end.

create_window(#state{mod = Mod, name = ModStr} = S) ->
    Title = atom_to_list(?APPLICATION) ++ " - " ++
	atom_to_list(Mod#mod.app_name) ++ " - " ++
	ModStr ++ ".erl",
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, []),
    %% wxFrame:setSize(Frame, {?WIN_WIDTH, ?WIN_HEIGHT}),
    Panel = wxPanel:new(Frame, []),
    StatusBar = wxFrame:createStatusBar(Frame,[]),

    Book = wxNotebook:new(Panel, ?wxID_ANY, []),

    S2 = S#state{frame = Frame,
                 panel = Panel,
                 book = Book,
                 status_bar = StatusBar,
		 code_pages = []},
    S3 = create_deps_page(S2),
    S4 = create_code_page(S3, ?INITIAL_CODE_PAGE_NAME),
    S5 = create_config_page(S4),
    wxNotebook:setSelection(Book, 0),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Book, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),

    wxEvtHandler:connect(Book, command_notebook_page_changed, [{skip, true}]),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),

    S5.

create_deps_page(S) ->
    Panel = wxPanel:new(S#state.book, []),
    Main = wxBoxSizer:new(?wxHORIZONTAL),

    UsedByCtrl = create_mods_list_ctrl(Panel,
				       Main,
				       "Modules using this",
				       " and their applications"),
    wxSizer:add(Main,
		wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),
    UsesCtrl = create_mods_list_ctrl(Panel,
				     Main,
				     "Used modules",
				     " and their applications"),
    S2 = S#state{deps_used_by_ctrl = UsedByCtrl,
                 deps_uses_ctrl = UsesCtrl},
    redraw_mods(S2),
    wxPanel:setSizer(Panel, Main),
    wxNotebook:addPage(S2#state.book, Panel, "Dependencies", []),
    S2.

create_mods_list_ctrl(Panel, Sizer, ModText, AppText) ->
    Width = lists:max([100, ?WIN_WIDTH - 40]) div 2,
    Height = lists:max([100, ?WIN_HEIGHT - 100]),
    ListCtrl = wxListCtrl:new(Panel,
                              [{style,
                                ?wxLC_REPORT bor
                                %% ?wxLC_SORT_ASCENDING bor
                                ?wxLC_SINGLE_SEL bor
                                ?wxHSCROLL bor
				?wxVSCROLL},
			       {size, {Width, Height}}]),
    %% Prep images
    reltool_utils:assign_image_list(ListCtrl),

    %% Prep column label
    ListItem  = wxListItem:new(),
    wxListItem:setAlign(ListItem, ?wxLIST_FORMAT_LEFT),
    wxListItem:setText(ListItem, ModText),
    wxListCtrl:insertColumn(ListCtrl, ?MODS_MOD_COL, ListItem),
    %% wxListCtrl:setColumnWidth(ListCtrl, ?MODS_MOD_COL, ?MODS_MOD_COL_WIDTH),

    wxListItem:setText(ListItem, AppText),
    wxListCtrl:insertColumn(ListCtrl, ?MODS_APP_COL, ListItem),
    %% wxListCtrl:setColumnWidth(ListCtrl, ?MODS_APP_COL, ?MODS_APP_COL_WIDTH),
    wxListItem:destroy(ListItem),

    wxEvtHandler:connect(ListCtrl, size,
			 [{skip, true}, {userData, mods_list_ctrl}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated,
		       [{userData, open_app}]),
    wxWindow:connect(ListCtrl, enter_window),

    wxSizer:add(Sizer, ListCtrl,
                [{border, 2},
                 {flag, ?wxALL bor ?wxEXPAND},
                 {proportion, 1}]),
    ListCtrl.

create_code_page(#state{book = Book, code_pages = Pages, name = ModStr} = S,
		 PageName) ->
    case find_page(S, PageName) of
	not_found ->
	    Page = do_create_code_page(S, PageName),
	    Pages2 = Pages ++ [Page],
	    Pos = length(Pages2),
	    wxNotebook:setSelection(Book, Pos),
	    case find_page(S, ?INITIAL_CODE_PAGE_NAME) of
		not_found ->
		    ignore;
		{found, _, CodePos} ->
		    %% Rename initial code page
		    wxNotebook:setPageText(Book, CodePos, ModStr)
	    end,
	    S#state{active_page = Page, code_pages = Pages2};
	{found, Page, Pos} ->
	    wxNotebook:setSelection(Book, Pos),
	    S#state{active_page = Page}
    end.

find_page(S, PageName) ->
    find_page(S#state.code_pages, PageName, 1).

find_page([Page | Pages], PageName, Pos) ->
    case Page#code_page.name =:= PageName of
	true ->
	    {found, Page, Pos};
	false ->
	    find_page(Pages, PageName, Pos + 1)
    end;
find_page([], _PageName, _Pos) ->
    not_found.

do_create_code_page(#state{xref_pid = Xref, mod = M} = S, PageName) ->
    Panel = wxPanel:new(S#state.book, []),
    Editor = create_editor(Panel),
    ToolTip = "Double click on a function call to "
	"search the function definition.",
    wxBitmapButton:setToolTip(Editor, ToolTip),
    {Objs, Data, SearchSz} = create_search_area(Panel),

    {ok, App} = reltool_server:get_app(Xref, M#mod.app_name),
    ErlBin =
	case App#app.is_escript of
	    false -> find_regular_bin(App, M);
	    _ -> find_escript_bin(App, M)
	end,

    load_code(Editor, ErlBin),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(Sizer, SearchSz, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Sizer),
    wxNotebook:addPage(S#state.book, Panel, PageName, []),
    #code_page{name  = PageName,
	       editor = Editor,
	       find_objs = Objs,
	       find_data = Data}.

find_regular_bin(App, Mod) ->
    ActiveDir = App#app.active_dir,
    SrcDir = filename:join([ActiveDir, "src"]),
    ModStr = atom_to_list(Mod#mod.name),
    Base = "^" ++ ModStr ++ "\\.erl$",
    Find = fun(F, _Acc) -> throw({file:read_file(F),epp:read_encoding(F)}) end,
    case catch filelib:fold_files(SrcDir, Base, true, Find, {error, enoent}) of
	{{ok, Bin},Encoding0} ->
	    Encoding =
		case Encoding0 of
		    none -> epp:default_encoding();
		    _ -> Encoding0
		end,
	    unicode:characters_to_binary(Bin,Encoding,utf8);
	{error, enoent} ->
	    %% Reconstructing the source code from debug info if possible
	    BeamFile = filename:join([ActiveDir, "ebin", ModStr ++ ".beam"]),
	    case source_from_beam(BeamFile) of
		{ok,Source} ->
		    Source;
		error ->
		    unicode:characters_to_binary(
		      ["%% Bad luck, cannot find any "
		       "debug info in the file \"", BeamFile])
	    end
    end.

source_from_beam(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
	{ok,{_,[{abstract_code,{_,AC}}]}} ->
	    IoList = [erl_pp:form(F,[{encoding,utf8}]) || F <- AC],
	    {ok,unicode:characters_to_binary(IoList)};
	_ ->
	    error
    end.

find_escript_bin(#app{active_dir = ActiveDir}, Mod) ->
    NotFound = false,
    ModName = Mod#mod.name,
    {Fun, Escript} =
	case filelib:is_regular(ActiveDir) of
	    true ->
		%% File is on top level in the escript
		{fun(FullName, _GetInfo, GetBin, Acc) ->
			 case filename:split(FullName) of
			     [_] ->
				 Bin = GetBin(),
				 case beam_lib:version(Bin) of
				     {ok,{M, _}} when M =:= ModName;
						      FullName =:= "." ->
					 case source_from_beam(Bin) of
					     {ok,Source} ->
						 {obj,Source};
					     error ->
						 Acc
					 end;
				     _ ->
					 Acc
				 end;
			     _ ->
				 Acc
			 end
		 end,
		 ActiveDir};
	    false ->
		%% File is in an archive
		Ext = code:objfile_extension(),
		SrcFile = lists:concat([ModName, ".erl"]),
		ObjFile = lists:concat([ModName, Ext]),
		{fun(FullName, _GetInfo, GetBin, Acc) ->
			 io:format("", []),
			 case filename:split(FullName) of
			     [_AppName, "ebin", F]
			       when F =:= ObjFile, Acc =:= NotFound ->
				 case source_from_beam(GetBin()) of
				     {ok,Source} ->
					 {obj,Source};
				     _ ->
					 Acc
				 end;
			     [_AppName, "src", F] when F =:= SrcFile ->
				 {text, GetBin()};
			     _ ->
				 Acc
			 end
		 end,
		 filename:dirname(ActiveDir)}
	end,
    try
	case reltool_utils:escript_foldl(Fun, NotFound, Escript) of
	    {ok, {text, Bin}} ->
		Bin;
	    {ok, {obj, Bin}} ->
		Bin;
	    _ ->
		unicode:characters_to_binary(
		  ["%% Bad luck, cannot find the "
		   "code in the escript ", Escript, "."])
	end
    catch
	throw:Reason when is_list(Reason) ->
	    unicode:characters_to_binary(
	      ["%% Bad luck, cannot find the code "
	       "in the escript ", Escript, ": ", Reason])
    end.

create_config_page(S) ->
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#state{xref_pid = Xref} = S, Wx) ->
    %% io:format("wx: ~p\n", [Wx]),
    case Wx of
	#wx{obj= ListCtrl,
	    userData = mods_list_ctrl,
	    event = #wxSize{type = size, size = {W, _H}}} ->
	    wxListCtrl:setColumnWidth(ListCtrl, ?MODS_MOD_COL, (2 * W) div 3),
	    wxListCtrl:setColumnWidth(ListCtrl, ?MODS_APP_COL, W div 3),
	    S;
        #wx{userData = open_app,
            obj = ListCtrl,
            event = #wxList{type = command_list_item_activated,
			    itemIndex = Pos}} ->
	    ModStr = wxListCtrl:getItemText(ListCtrl, Pos),
            ModName = list_to_atom(ModStr),
            {ok, Mod} = reltool_server:get_mod(Xref, ModName),
	    S2 = create_code_page(S#state{mod = Mod}, ModStr),
	    find_regexp_forward(S2, S2#state.name ++ ":");
            %% ok = reltool_sys_win:open_app(S#state.rel_pid, Mod#mod.app_name),
            %% S;
	#wx{obj = Editor,
            event = #wxStyledText{type = stc_doubleclick}} ->
	    goto_function(S, Editor);
	#wx{id = ?SEARCH_ENTRY,
            event = #wxCommand{type = command_text_enter, cmdString = Str}} ->
	    find_string(S, Str);
	#wx{id = ?GOTO_ENTRY,
            event = #wxCommand{type = command_text_enter, cmdString = Str}} ->
	    goto_line(S, Str);
        #wx{event = #wxNotebook{type = command_notebook_page_changed}} ->
	    case wxNotebook:getSelection(S#state.book) of
		0 -> % Deps page
		    S;
		N -> % Code page
		    Page = lists:nth(N, S#state.code_pages),
		    S#state{active_page = Page}
	    end;
	#wx{event = #wxCommand{type = command_button_clicked},
	    userData = history_back} ->
	    goto_back(S);
	#wx{obj = ObjRef, event = #wxMouse{type = enter_window}} ->
	    wxWindow:setFocus(ObjRef),
	    S;
	_ ->
            error_logger:format("~w~w got unexpected mod event from "
				"wx:\n\t~p\n",
                                [?MODULE, self(), Wx]),
            S
    end.

redraw_mods(#state{xref_pid = Xref,
		   deps_used_by_ctrl = UsedByCtrl,
                   deps_uses_ctrl = UsesCtrl,
		   mod = #mod{is_pre_included = IsPre,
			      is_included = IsIncl,
			      uses_mods = UsesModNames,
			      used_by_mods = UsedByModNames},
		   status_bar = Bar}) ->
    InclStatus =
	case IsIncl of
	    true when IsPre =:= true -> "Whitelist - ";
	    true -> "Derived - ";
	    false -> "Blacklist - ";
	    undefined -> "Source - "
	end,
    Status = lists:concat([InclStatus,
			   " uses ", length(UsesModNames),
			   " modules and ",
			   " is used by ", length(UsedByModNames),
			   " modules."]),
    wxStatusBar:setStatusText(Bar, Status),
    UsesMods = [select_image(Xref, M) || M <- UsesModNames],
    UsedByMods = [select_image(Xref, M) || M <- UsedByModNames],
    redraw_mods(UsedByCtrl, UsedByMods),
    redraw_mods(UsesCtrl, UsesMods).

select_image(Xref, ModName) ->
    {ok, M} = reltool_server:get_mod(Xref, ModName),
    Image =
	case M#mod.is_included of
	    _ when M#mod.app_name =:= ?MISSING_APP_NAME -> ?ERR_IMAGE;
	    true -> ?TICK_IMAGE;
	    false -> ?WARN_IMAGE;
	    undefined -> ?ERR_IMAGE
	end,	
    {Image, M#mod.app_name, M}.

redraw_mods(ListCtrl, []) ->
    wxListCtrl:deleteAllItems(ListCtrl);
redraw_mods(ListCtrl, ImageMods) ->
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
                wxListCtrl:setItem(ListCtrl, Row, ?MODS_MOD_COL,
				   atom_to_list(ModName), [{imageId, ImageId}]),
		wxListCtrl:setItem(ListCtrl, Row, ?MODS_APP_COL,
				   atom_to_list(AppName), [{imageId, ImageId}]),
                Row + 1
        end,
    wx:foldl(Add, 0, lists:sort(ImageMods)).

redraw_config(S) ->
    S.

redraw_window(S) ->
    redraw_config(S),
    redraw_mods(S),
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goto_line(#state{active_page = P} = S, LineNo) when is_integer(LineNo) ->
    Editor = P#code_page.editor,
    wxStyledTextCtrl:gotoLine(Editor, LineNo),
    Left = wxStyledTextCtrl:getCurrentPos(Editor),
    Right = wxStyledTextCtrl:getLineEndPosition(Editor, LineNo),
    wxStyledTextCtrl:setSelection(Editor, Left, Right),
    S;
goto_line(#state{active_page = P} =S, Str) when is_list(Str) ->
    try
	LineNo = list_to_integer(Str),
	CurrentPos = wxStyledTextCtrl:getCurrentPos(P#code_page.editor),
	S2 = add_pos_to_history(S, CurrentPos),
	goto_line(S2, LineNo - 1)
    catch
	_:_ ->
	    wxStatusBar:setStatusText(S#state.status_bar, "Not a line number"),
	    S
    end.

find_string(S, Str) ->
    find_string(S, Str, 0).

find_regexp_forward(S, Str) ->
    S2 = find_string(S, Str, ?wxSTC_FIND_REGEXP),
    TextCtrl = ((S2#state.active_page)#code_page.find_objs)#find_objs.search,
    wxTextCtrl:setValue(TextCtrl, Str),
    S2.

find_string(#state{active_page =
		   #code_page{editor = Editor,
			      find_objs = #find_objs{radio={NextO,_,CaseO}},
			      find_data = #find_data{found = Found} = Data} = P} = S,
	    Str,
	    Flag) ->
    wxStyledTextCtrl:hideSelection(Editor, true),
    Dir  = wxRadioButton:getValue(NextO) xor wx_misc:getKeyState(?WXK_SHIFT),
    Case = wxCheckBox:getValue(CaseO),
    Pos =
	if
	    Found, Dir ->  %% Forward Continuation
		wxStyledTextCtrl:getAnchor(Editor);
	    Found ->  %% Backward Continuation
		wxStyledTextCtrl:getCurrentPos(Editor);
	    Dir ->   %% Forward wrap
		0;
	    true ->  %% Backward wrap
		wxStyledTextCtrl:getLength(Editor)
	end,
    wxStyledTextCtrl:gotoPos(Editor,Pos),
    wxStyledTextCtrl:searchAnchor(Editor),
    Flag2 =
	if  Case -> Flag bor ?wxSTC_FIND_MATCHCASE;
	    true -> Flag
	end,
    Res =
	if
	    Dir -> wxStyledTextCtrl:searchNext(Editor, Flag2, Str);
	    true -> wxStyledTextCtrl:searchPrev(Editor, Flag2, Str)
	end,
    Found2 =
	case Res >= 0 of	
	    true ->
		wxStyledTextCtrl:hideSelection(Editor, false),
		%% io:format("Found ~p ~n",[Res]),
		LineNo = wxStyledTextCtrl:lineFromPosition(Editor,Res),
		wxStyledTextCtrl:scrollToLine(Editor, LineNo - 3),
		wxStatusBar:setStatusText(S#state.status_bar, ""),
		true;
	    false ->
		wxStatusBar:setStatusText(S#state.status_bar,
					  "Not found (Hit Enter to "
					  "wrap search)"),
		false
	end,
    P2 = P#code_page{find_data = Data#find_data{found = Found2}},
    Pages = lists:keystore(P#code_page.name,
			   #code_page.name,
			   S#state.code_pages,
			   P2),
    S#state{active_page = P2, code_pages = Pages}.

goto_function(S, Editor) ->
    wxStyledTextCtrl:hideSelection(Editor, false),
    CurrentPos = wxStyledTextCtrl:getCurrentPos(Editor),
    Left = wxStyledTextCtrl:wordStartPosition(Editor, CurrentPos, true),
    Right = wxStyledTextCtrl:wordEndPosition(Editor, CurrentPos, true),
    ColonPos = Left - 1,
    Left2 =
	case wxStyledTextCtrl:getCharAt(Editor, ColonPos) of
	    $: ->
		wxStyledTextCtrl:wordStartPosition(Editor, ColonPos, true);
	    _ ->
		Left
	end,
    Right2 =
	case wxStyledTextCtrl:getCharAt(Editor, Right) of
	    $: ->
		wxStyledTextCtrl:wordEndPosition(Editor, Right + 1, true);
	    _ ->
		Right
	end,
    case [wxStyledTextCtrl:getCharAt(Editor, Right2)] of
	"(" ->
	    wxStyledTextCtrl:setSelection(Editor, Left2, Right2),
	    Text = wxStyledTextCtrl:getSelectedText(Editor),
	    S2 = add_pos_to_history(S, CurrentPos),
	    do_goto_function(S2, string:tokens(Text, ":"));
	_ ->
	    %% No function call
	    wxStyledTextCtrl:hideSelection(Editor, false),
	    wxStyledTextCtrl:setSelection(Editor, Left2, Right2),
	    S
    end.

do_goto_function(S, []) ->
    S;
do_goto_function(#state{active_page = P} = S, [FunName]) ->
    wxStyledTextCtrl:gotoPos(P#code_page.editor, 1),
    find_regexp_forward(S, "^" ++ FunName ++ "(");
do_goto_function(S, [ModStr, FunStr]) ->
    case reltool_server:get_mod(S#state.xref_pid, list_to_atom(ModStr)) of
	{ok, Mod} when Mod#mod.app_name =/= ?MISSING_APP_NAME ->
	    S2 = create_code_page(S#state{mod = Mod}, ModStr),
	    find_regexp_forward(S2, "^" ++ FunStr ++ "(");
	{ok, _} ->
	    wxStatusBar:setStatusText(S#state.status_bar,
				      "No such module: " ++ ModStr),
	    S
    end.

goto_back(#state{active_page =
		 #code_page{editor = Editor, find_data = Data} = Page,
		 code_pages = Pages} = S) ->
    case Data#find_data.history of
	[PrevPos | History] ->
	    LineNo = wxStyledTextCtrl:lineFromPosition(Editor, PrevPos),
	    Data2 = Data#find_data{history = History},
	    Page2 = Page#code_page{find_data = Data2},
	    Pages2 = lists:keystore(Page2#code_page.name,
				    #code_page.name,
				    Pages,
				    Page2),
	    goto_line(S#state{active_page = Page2, code_pages = Pages2},
		      LineNo);
	[] ->
	    wxStatusBar:setStatusText(S#state.status_bar, "No history"),
	    S
    end.

add_pos_to_history(#state{active_page = Page, code_pages = Pages} = S,
		   CurrentPos) ->
    Data = Page#code_page.find_data,
    Data2 = Data#find_data{history = [CurrentPos | Data#find_data.history]},
    Page2 = Page#code_page{find_data = Data2},
    Pages2 =
	lists:keystore(Page2#code_page.name, #code_page.name, Pages, Page2),
    S#state{active_page = Page2, code_pages = Pages2}.

create_editor(Parent) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    %%Ed = wxStyledTextCtrl:new(Parent, [{size, {700, 500}}]),
    Ed = wxStyledTextCtrl:new(Parent),

    wxStyledTextCtrl:styleClearAll(Ed),
    wxStyledTextCtrl:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
    LW = wxStyledTextCtrl:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
    wxStyledTextCtrl:setMarginWidth(Ed, 0, LW),

    wxStyledTextCtrl:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    %%wxStyledTextCtrl:hideSelection(Ed, true),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}],
    SetStyle = fun({Style, Color}) ->
		       wxStyledTextCtrl:styleSetFont(Ed, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Ed, Style, Color)
	       end,
    lists:foreach(fun (Style) -> SetStyle(Style) end, Styles),
    wxStyledTextCtrl:setKeyWords(Ed, 0, keyWords()),

    %% Margins Markers
    %% Breakpoint Should be a pixmap?
    wxStyledTextCtrl:markerDefine(Ed, 0, ?wxSTC_MARK_CIRCLE,
				  [{foreground, {170,20,20}}]),
    wxStyledTextCtrl:markerDefine(Ed, 0, ?wxSTC_MARK_CIRCLE,
				  [{background, {200,120,120}}]),
    %% Disabled Breakpoint
    wxStyledTextCtrl:markerDefine(Ed, 1, ?wxSTC_MARK_CIRCLE,
				  [{foreground, {20,20,170}}]),
    wxStyledTextCtrl:markerDefine(Ed, 1, ?wxSTC_MARK_CIRCLE,
				  [{background, {120,120,200}}]),

    %% Current Line
    wxStyledTextCtrl:markerDefine(Ed, 2, ?wxSTC_MARK_ARROW,
				  [{foreground, {20,170,20}}]),
    wxStyledTextCtrl:markerDefine(Ed, 2, ?wxSTC_MARK_ARROW,
				  [{background, {200,255,200}}]),
    wxStyledTextCtrl:markerDefine(Ed, 3, ?wxSTC_MARK_BACKGROUND,
				  [{background, {200,255,200}}]),

    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
    wxStyledTextCtrl:setYCaretPolicy(Ed, Policy, 3),
    wxStyledTextCtrl:setVisiblePolicy(Ed, Policy, 3),

    wxStyledTextCtrl:connect(Ed, stc_doubleclick),
    wxWindow:connect(Ed, enter_window),

    wxStyledTextCtrl:setReadOnly(Ed, true),
    Ed.

create_search_area(Parent) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Sizer, wxStaticText:new(Parent, ?wxID_ANY, "Find:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC1 = wxTextCtrl:new(Parent, ?SEARCH_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(Sizer, TC1,  [{proportion,3}, {flag, ?wxEXPAND}]),
    Nbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Next"),
    wxRadioButton:setValue(Nbtn, true),
    wxSizer:add(Sizer,Nbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Pbtn = wxRadioButton:new(Parent, ?wxID_ANY, "Previous"),
    wxSizer:add(Sizer,Pbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    Cbtn = wxCheckBox:new(Parent, ?wxID_ANY, "Match Case"),
    wxSizer:add(Sizer,Cbtn,[{flag,?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(Sizer, 15,15, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, wxStaticText:new(Parent, ?wxID_ANY, "Goto Line:"),
		[{flag,?wxALIGN_CENTER_VERTICAL}]),
    TC2 = wxTextCtrl:new(Parent, ?GOTO_ENTRY, [{style, ?wxTE_PROCESS_ENTER}]),
    wxSizer:add(Sizer, TC2,  [{proportion,0}, {flag, ?wxEXPAND}]),
    Button = wxButton:new(Parent, ?wxID_ANY, [{label, "Back"}]),
    wxSizer:add(Sizer, Button, []),

    wxEvtHandler:connect(Button, command_button_clicked,
			 [{userData, history_back}]),
    %% wxTextCtrl:connect(TC1, command_text_updated),
    wxTextCtrl:connect(TC1, command_text_enter),
    %% wxTextCtrl:connect(TC1, kill_focus),
    wxTextCtrl:connect(TC2, command_text_enter),
    wxWindow:connect(Parent, command_button_clicked),
    {#find_objs{search = TC1,goto = TC2,radio = {Nbtn,Pbtn,Cbtn}},
     #find_data{start = 0, found = false, history = []},
     Sizer}.

load_code(Ed, Code) when is_binary(Code) ->
    wxStyledTextCtrl:setReadOnly(Ed, false),
    wxStyledTextCtrl:setTextRaw(Ed, <<Code/binary, 0:8>>),
    Lines = wxStyledTextCtrl:getLineCount(Ed),
    Sz = trunc(math:log10(Lines))+1,
    LW = wxStyledTextCtrl:textWidth(Ed,
				    ?wxSTC_STYLE_LINENUMBER,
				    lists:duplicate(Sz, $9)),
    %%io:format("~p ~p ~p~n", [Lines, Sz, LW]),
    wxStyledTextCtrl:setMarginWidth(Ed, 0, LW+5),
    wxStyledTextCtrl:setReadOnly(Ed, true),
    Ed.

keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(_Parent, _Debug, S) ->
    ?MODULE:loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S,_Module,_OldVsn,_Extra) ->
    {ok, S}.
