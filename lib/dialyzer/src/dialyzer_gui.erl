%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
%%

%%%-----------------------------------------------------------------------
%%% File    : dialyzer_gui.erl
%%% Authors : Tobias Lindahl <tobiasl@it.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : The graphical user interface for the Dialyzer tool.
%%%
%%% Created : 27 Apr 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-----------------------------------------------------------------------

-module(dialyzer_gui).

-export([start/1]).

-include("dialyzer.hrl").

%%------------------------------------------------------------------------

-define(DIALYZER_ERROR_TITLE,   "Dialyzer Error").
-define(DIALYZER_MESSAGE_TITLE, "Dialyzer Message").

%%------------------------------------------------------------------------

-type gs_object() :: any().  %% XXX: should be imported from gs

-record(mode, {start_byte_code   :: gs_object(),
	       start_src_code    :: gs_object()}).

-record(menu, {file_save_log     :: gs_object(),
	       file_save_warn    :: gs_object(),
	       file_quit         :: gs_object(),
	       help_about        :: gs_object(),
	       help_manual       :: gs_object(),
	       help_warnings     :: gs_object(),
	       opts_macros       :: gs_object(),
	       opts_includes     :: gs_object(),
	       plt_empty         :: gs_object(),
	       plt_search_doc    :: gs_object(),
	       plt_show_doc      :: gs_object(),
	       warnings          :: gs_object()}).

-record(gui_state, {add_all      :: gs_object(),
		    add_file     :: gs_object(),
		    add_rec      :: gs_object(),
		    chosen_box   :: gs_object(),
		    analysis_pid :: pid(),
		    del_file     :: gs_object(),
		    doc_plt      :: dialyzer_plt:plt(),
		    clear_chosen :: gs_object(),
		    clear_log    :: gs_object(),
		    clear_warn   :: gs_object(),
		    init_plt     :: dialyzer_plt:plt(),
		    dir_entry    :: gs_object(),
		    file_box     :: gs_object(),
		    file_wd      :: gs_object(),
		    gs           :: gs_object(),
		    log          :: gs_object(),
		    menu         :: #menu{},
		    mode         :: #mode{},
		    options      :: #options{},
		    packer       :: gs_object(),
		    run          :: gs_object(),
		    stop         :: gs_object(),
		    top          :: gs_object(),
		    warnings_box :: gs_object(),
		    backend_pid  :: pid()}).
	       
%%------------------------------------------------------------------------

-spec start(#options{}) -> ?RET_NOTHING_SUSPICIOUS.

start(DialyzerOptions = #options{from = From, init_plt = InitPltFile,
				 legal_warnings = LegalWarnings}) ->
  process_flag(trap_exit, true),

  GS = gs:start(),
  code:add_pathsa(["."]),
  WH = [{width, 1000}, {height, 550}],
  EmptySpace = {stretch, 1},

  {ok, Host} = inet:gethostname(),
  %% --------- Top Window --------------
  TopWin = gs:window(GS, [{title, "Dialyzer " ++ ?VSN ++ " @ " ++ Host},
			  {configure, true},
			  {default, listbox, {bg, white}},
			  {default, editor, {bg, white}},
			  {default, entry, {bg, white}},
			  {default, button, {font, {helvetica, bold, 12}}},
			  {default, label, {font, {helvetica, bold, 12}}}
			  |WH]),
  Packer = gs:frame(TopWin, [{packer_x, [{stretch, 3},{fixed, 200},
					 {stretch, 7}]},
			     {packer_y, [{fixed, 25}, {fixed, 20},
					 {stretch, 1, 50},
					 {fixed, 25}, {fixed, 20},
					 {stretch, 1, 50},
					 {fixed, 25}]}]),

  %% --------- Chosen box --------------
  gs:label(Packer, [{label, {text, "Directories or modules to analyze"}}, 
		    {height, 20}, {pack_xy, {1, 2}}]),
  ChosenBox = gs:listbox(Packer, [{pack_xy, {1, 3}}, {vscroll, right},
				  {selectmode, multiple}]),

  %% --------- File box --------------
  gs:label(Packer, [{label, {text, "File"}}, {height, 20}, {pack_xy, {1,5}}]),
  FilePacker = gs:frame(Packer, [{packer_x, [{fixed, 30}, {stretch, 1, 100}]},
				 {packer_y, [{fixed, 25}, {stretch, 1, 25}]},
				 {pack_xy, {1, 6}}]),
  gs:label(FilePacker, [{label, {text, "Dir:"}}, {pack_xy, {1, 1}}]),
  DirEntry = gs:entry(FilePacker, [{height, 30}, {pack_xy, {2, 1}},
				   {keypress, true}]),
  File = gs:listbox(FilePacker, [{pack_x, {1,2}}, {pack_y, 2},
				 {selectmode, multiple}, {doubleclick, true}, 
				 {vscroll, right}]),

  %% --------- Options --------------
  gs:label(Packer, [{label, {text, "Analysis Options"}}, 
		    {height, 20}, {pack_xy, {2, 2}}]),
  ModePacker = gs:frame(Packer, [{packer_x, [{fixed, 75}, {fixed, 120}]},
				 {packer_y, [{fixed, 20}, {fixed, 20},
					     {fixed, 20},
					     %% EmptySpace,
					     {fixed, 20}, {fixed, 20}, 
					     {fixed, 20}, EmptySpace]},
				 {bw, 10}, {relief, flat},
				 {default, {radiobutton, {align, w}}},
				 {default, {label, {align, w}}},
				 {pack_xy, {2, 3}}]),

  %% Bytecode vs. Source code
  gs:label(ModePacker, [{label, {text, "File Type:"}},
			{height, 20}, {pack_xy, {1,1}}]),
  {ByteSel, SrcSel} = case From of
			byte_code -> {[{select, true}], []}; 
			src_code -> {[], [{select, true}]}
		      end,
  ModeByteCode = gs:radiobutton(ModePacker,
				ByteSel ++ [{group, start_from},
					    {label, {text,"BeamFiles"}},
					    {pack_xy, {2,1}}]),
  ModeSrcCode = gs:radiobutton(ModePacker,
			       SrcSel ++ [{group, start_from},
					  {label, {text,"SourceFiles"}},
					  {pack_xy, {2,2}}]),
  Mode = #mode{start_byte_code = ModeByteCode,
	       start_src_code = ModeSrcCode},

  %% --------- Log box --------------
  gs:label(Packer, [{label, {text, "Log"}}, {height, 20}, {pack_xy, {3,2}}]),
  Log = gs:editor(Packer, [{pack_x, 3}, {pack_y, 3}, {enable, false},
			   {font, {courier, 12}}, {vscroll, right},
			   {wrap, word}]),

  %% --------- Warnings box --------------
  gs:label(Packer, [{label, {text, "Warnings"}},{height, 20},{pack_xy, {3,5}}]),
  WarningsBox = gs:editor(Packer, [{pack_x, {2,3}}, {pack_y, 6},
				   {enable, false},
				   {font, {courier, 12}}, {vscroll, right},
				   {wrap, word}]),

  %% --------- Buttons --------------
  ButtonPackerHighLeft = 
    gs:frame(Packer, [{packer_x, [{fixed, 50}, {fixed, 65}, EmptySpace]},
		      {pack_xy, {1,4}}]),
  ButtonPackerHighRight = 
    gs:frame(Packer, [{packer_x, [{fixed, 70}, {fixed, 70}, EmptySpace]},
		      {pack_xy, {3,4}}]),
  ButtonPackerLowLeft = 
    gs:frame(Packer, [{packer_x, [{fixed, 50},
				  {fixed, 60},
				  {fixed, 110},
				  EmptySpace]},
		      {pack_xy, {1,7}}]),
  ButtonPackerLowRight = 
    gs:frame(Packer, [{packer_x, [{fixed, 100},
				  {fixed, 70},
				  EmptySpace,
				  {fixed, 70},
				  {fixed, 70}]}, 
		      {pack_x, {2,3}}, {pack_y, 7}]),

  WHButton = [{width, 60}, {height, 20}],
  AddFile = gs:button(ButtonPackerLowLeft, [{pack_xy, {1, 1}}, 
					    {label, {text,"Add"}}|WHButton]),
  AddAll = gs:button(ButtonPackerLowLeft, [{pack_xy, {2, 1}}, 
					   {label, {text,"Add All"}}|WHButton]),
  AddRec = gs:button(ButtonPackerLowLeft, [{pack_xy, {3, 1}}, 
                                           {label, {text,"Add Recursively"}}
                                           |WHButton]),
  DelFile = gs:button(ButtonPackerHighLeft, [{pack_xy, {1, 1}}, 
					    {label, {text,"Delete"}}|WHButton]),
  ClearChosen = gs:button(ButtonPackerHighLeft, [{pack_xy, {2, 1}},
					         {label, {text,"Delete All"}}
					         |WHButton]),
  ClearLog = gs:button(ButtonPackerHighRight, [{pack_xy, {1, 1}}, 
					       {label, {text,"Clear Log"}}
					       |WHButton]),
  ClearWarn = gs:button(ButtonPackerLowRight, [{pack_xy, {1, 1}}, 
					       {label, {text,"Clear Warnings"}}
					       |WHButton]),

  Run = gs:button(ButtonPackerLowRight, [{pack_xy, {4, 1}},
					 {label, {text,"Run"}}|WHButton]),
  Stop = gs:button(ButtonPackerLowRight, [{pack_xy, {5, 1}}, {enable, false}, 
					  {label, {text,"Stop"}}|WHButton]),

  %% --------- Menu --------------  
  MenuBar = gs:menubar(TopWin, []),

  %% File Menu
  MenuBarFile = gs:menubutton(MenuBar, [{label, {text, "File"}}]),
  MenuFile = gs:menu(MenuBarFile, []),
  MenuFileSaveWarn = gs:menuitem(MenuFile, [{label, {text, "Save Warnings"}}]),
  MenuFileSaveLog = gs:menuitem(MenuFile, [{label, {text, "Save Log"}}]),
  MenuFileQuit = gs:menuitem(MenuFile, [{label, {text, "Quit"}}]),

  %% Warnings Menu
  MenuBarWarn = gs:menubutton(MenuBar, [{label, {text, "Warnings"}}]),
  MenuWarn = gs:menu(MenuBarWarn, []),
  MenuWarnMatch = gs:menuitem(MenuWarn, [{label, {text, "Match failures"}}, 
					 {itemtype, check}, {select, true}]),
  MenuWarnFailingCall = gs:menuitem(MenuWarn, 
				    [{label, {text, "Failing function calls"}},
				     {itemtype, check}, {select, true}]),
  MenuWarnFunApp = gs:menuitem(MenuWarn, [{label, 
					   {text, "Bad fun applications"}},
					  {itemtype, check}, {select, true}]),
  MenuWarnOpaque = gs:menuitem(MenuWarn, [{label, 
					   {text, "Opaqueness violations"}},
					  {itemtype, check}, {select, true}]),
  MenuWarnLists = gs:menuitem(MenuWarn,
			      [{label, {text, "Improper list constructions"}}, 
			       {itemtype, check}, {select, true}]),
  MenuWarnNotCalled = gs:menuitem(MenuWarn, 
				  [{label, {text, "Unused functions"}}, 
				   {itemtype, check}, {select, true}]),
  MenuWarnReturnOnlyExit = gs:menuitem(MenuWarn,
				       [{label, 
					 {text, "Error handling functions"}},
					{itemtype, check}, {select, false}]),
  MenuWarnReturnNoReturn = gs:menuitem(MenuWarn,
				       [{label,
					 {text, "Functions of no return"}},
					{itemtype, check}, {select, true}]),  
  MenuWarnCallNonExported = gs:menuitem(MenuWarn,
					[{label, 
					  {text, "Call to unexported function"}},
					 {itemtype, check}, {select, true}]), 
  MenuWarnRaceCondition = gs:menuitem(MenuWarn,
				      [{label,
                                        {text,"Possible race conditions"}},
                                       {itemtype, check}, {select, false}]),
  MenuWarnContractTypes = gs:menuitem(MenuWarn,
				      [{label, {text, "Wrong contracts"}},
				       {itemtype, check}, {select, true}]),
  MenuWarnContractSyntax = gs:menuitem(MenuWarn,
				       [{label,
					 {text, "Wrong contract syntax"}},
					{itemtype, check}, {select, true}]),
  
  %% PLT Menu
  MenuBarPLT = gs:menubutton(MenuBar, [{label, {text,"PLT"}}]),
  MenuPLT = gs:menu(MenuBarPLT, []),
  MenuPLTEmpty = gs:menuitem(MenuPLT, [{label, {text, "Init with empty PLT"}},
				       {itemtype, check}, {select, false}]),
  MenuPLTShow = gs:menuitem(MenuPLT, [{label, {text, "Show contents"}}]),
  MenuPLTSearch = gs:menuitem(MenuPLT, [{label, {text, "Search contents"}}]),

  %% Options Menu
  MenuBarOpts = gs:menubutton(MenuBar, [{label,{text,"Options"}}]),
  MenuOpts = gs:menu(MenuBarOpts, []),
  MenuOptsMacros = gs:menuitem(MenuOpts,
			       [{label, {text, "Manage Macro Definitions"}}]),
  MenuOptsIncludes = gs:menuitem(MenuOpts,
				 [{label, {text, "Manage Include Directories"}}]),
  
  %% Help
  MenuBarHelp = gs:menubutton(MenuBar, [{label, {text, "Help"}}, {side, right}]),
  MenuHelp = gs:menu(MenuBarHelp, []),
  MenuHelpManual = gs:menuitem(MenuHelp, [{label, {text, "Manual"}}]),
  MenuHelpWarnings = gs:menuitem(MenuHelp, [{label, {text, "Warning Options"}}]),
  MenuHelpAbout = gs:menuitem(MenuHelp, [{label, {text, "About"}}]),
  
  Warnings = [{?WARN_RETURN_NO_RETURN, MenuWarnReturnNoReturn},
	      {?WARN_RETURN_ONLY_EXIT, MenuWarnReturnOnlyExit},
	      {?WARN_NOT_CALLED, MenuWarnNotCalled},
	      {?WARN_NON_PROPER_LIST, MenuWarnLists},
	      {?WARN_FUN_APP, MenuWarnFunApp},
	      {?WARN_MATCHING, MenuWarnMatch},
	      {?WARN_OPAQUE, MenuWarnOpaque},
	      {?WARN_FAILING_CALL, MenuWarnFailingCall},
	      {?WARN_CALLGRAPH, MenuWarnCallNonExported},
              {?WARN_RACE_CONDITION, MenuWarnRaceCondition},
	      %% For contracts. 
	      {?WARN_CONTRACT_TYPES, MenuWarnContractTypes},
	      {?WARN_CONTRACT_SYNTAX, MenuWarnContractSyntax}
	     ],

  init_warnings(Warnings, LegalWarnings),

  Menu = #menu{file_quit = MenuFileQuit,
	       plt_empty = MenuPLTEmpty,
	       help_manual = MenuHelpManual,
	       help_about = MenuHelpAbout,
	       help_warnings = MenuHelpWarnings,
	       opts_macros = MenuOptsMacros,
	       opts_includes = MenuOptsIncludes,
	       plt_search_doc = MenuPLTSearch,
	       plt_show_doc = MenuPLTShow,
	       file_save_log = MenuFileSaveLog,
	       file_save_warn = MenuFileSaveWarn,
	       warnings = Warnings},

  %% --------- Init --------------
  gs:config(TopWin, [{map, true}]),
  gs:config(Packer, WH),
  {ok, CWD} = file:get_cwd(),
  
  InitPlt = try dialyzer_plt:from_file(InitPltFile)
	    catch throw:{dialyzer_error, _} -> dialyzer_plt:new()
	    end,

  State = #gui_state{add_all = AddAll,
		     add_file = AddFile,
		     add_rec = AddRec,
		     chosen_box = ChosenBox, 
		     clear_chosen = ClearChosen, 
		     clear_log = ClearLog, 
		     clear_warn = ClearWarn, 
		     del_file = DelFile, 
		     doc_plt = dialyzer_plt:new(),
		     dir_entry = DirEntry,
		     file_box = File, 
		     file_wd = CWD,
		     gs = GS,
		     init_plt = InitPlt,
		     log = Log,
		     menu = Menu,
		     mode = Mode,
		     options = DialyzerOptions,
		     packer = Packer, 
		     run = Run,
		     stop = Stop,
		     top = TopWin, 
		     warnings_box = WarningsBox},
  NewState = change_dir_or_add_file(State, "."),
  gui_loop(NewState).

%% ----------------------------------------------------------------
%%
%%  Main GUI Loop
%%

-spec gui_loop(#gui_state{}) -> ?RET_NOTHING_SUSPICIOUS.

gui_loop(#gui_state{add_all = AddAll, add_file = AddFile, add_rec = AddRec,
		    backend_pid = BackendPid, chosen_box = ChosenBox,
		    clear_chosen = ClearChosen, clear_log = ClearLog,
		    clear_warn = ClearWarn, del_file = DelFile,
		    dir_entry = DirEntry, file_box = File, log = Log,
		    menu = Menu, packer = Packer, run = Run, stop = Stop,
		    top = TopWin, warnings_box = Warn} = State) ->
  %% --- Menu ---
  Quit = Menu#menu.file_quit,
  Manual = Menu#menu.help_manual,
  Warnings = Menu#menu.help_warnings,
  About = Menu#menu.help_about,
  SaveLog = Menu#menu.file_save_log,
  SaveWarn = Menu#menu.file_save_warn,
  SearchPlt = Menu#menu.plt_search_doc,
  ShowPlt = Menu#menu.plt_show_doc,
  Macros = Menu#menu.opts_macros,
  Includes = Menu#menu.opts_includes,
  
  receive
    {gs, TopWin, configure, _Data, [W, H|_]} ->
      gs:config(Packer, [{width, W}, {height, H}]),
      gui_loop(State);
    {gs, TopWin, destroy, _Data, _Args} ->
      ?RET_NOTHING_SUSPICIOUS;
    {gs, File, doubleclick, _, [_Id, Text|_]} ->
      NewState = change_dir_or_add_file(State, Text),
      gui_loop(NewState);
    {gs, DirEntry, keypress, _, ['Return'|_]} ->
      gs:config(TopWin, [{setfocus, true}]),
      NewState = change_dir_absolute(State, gs:read(DirEntry, text)),
      gui_loop(NewState);
    {gs, DirEntry, keypress, _, _} ->
      gui_loop(State);
    %% ----- Buttons -----
    {gs, AddFile, click, _, _} ->
      handle_add_files(State),
      gui_loop(State);
    {gs, AddAll, click, _, _} ->
      handle_add_all_click(State),
      gui_loop(State);
    {gs, AddRec, click, _, _} ->
      handle_add_rec_click(State),
      gui_loop(State);
    {gs, DelFile, click, _, _} ->
      handle_file_delete(State),
      gui_loop(State);
    {gs, ClearChosen, click, _, _} ->
      gs:config(ChosenBox, [clear]),
      gui_loop(State);
    {gs, ClearLog, click, _, _} ->
      Log = State#gui_state.log,
      gs:config(Log, [{enable, true}]),
      gs:config(Log, [clear]),
      gs:config(Log, [{enable, false}]),
      gui_loop(State);
    {gs, ClearWarn, click, _, _} ->
      Warn = State#gui_state.warnings_box,
      gs:config(Warn, [{enable, true}]),
      gs:config(Warn, [clear]),
      gs:config(Warn, [{enable, false}]),
      gui_loop(State);
    {gs, Run, click, _, _} ->
      NewState = start_analysis(State),
      gui_loop(NewState);
    {gs, Stop, click, _, _} ->
      config_gui_stop(State),
      BackendPid ! {self(), stop},
      update_editor(Log, "\n***** Analysis stopped ****\n"),
      gui_loop(State);
    %% ----- Menu -----
    {gs, Quit, click, _, _} ->
      case maybe_quit(State) of
	true -> ?RET_NOTHING_SUSPICIOUS;
	false -> gui_loop(State)
      end;
    {gs, Manual, click, _, _} ->
      spawn_link(fun() -> manual(State) end),
      gui_loop(State);
    {gs, Warnings, click, _, _} ->
      spawn_link(fun() -> warnings(State) end),
      gui_loop(State);
    {gs, About, click, _, _} ->
      spawn_link(fun() -> about(State) end),
      gui_loop(State);
    {gs, SaveLog, click, _, _} ->
      save_log(State),
      gui_loop(State);
    {gs, SaveWarn, click, _, _} ->
      save_warn(State),
      gui_loop(State);
    {gs, SearchPlt, click, _, _} ->
      spawn_link(fun() -> search_doc_plt(State) end),
      gui_loop(State);
    {gs, ShowPlt, click, _, _} ->
      spawn_link(fun() -> show_doc_plt(State) end),
      gui_loop(State);
    {gs, Macros, click, _, _} ->
      Self = self(),
      spawn_link(fun() -> macro_dialog(State, Self) end),
      gui_loop(State);
    {gs, Includes, click, _, _} ->
      Self = self(),
      spawn_link(fun() -> include_dialog(State, Self) end),
      gui_loop(State);
    {new_options, NewOptions} ->
      NewState = State#gui_state{options = NewOptions},
      gui_loop(NewState);
    %% ----- Analysis -----
    {BackendPid, ext_calls, ExtCalls} ->
      Msg = io_lib:format("The following functions are called "
			  "but type information about them is not available.\n"
			  "The analysis might get more precise by including "
			  "the modules containing these functions:\n\n\t~p\n", 
			  [ExtCalls]),
      free_editor(State, "Analysis done", Msg),
      gui_loop(State);
    {BackendPid, log, LogMsg} ->
      update_editor(Log, LogMsg),
      gui_loop(State);
    {BackendPid, warnings, Warns} ->
      SortedWarns = lists:keysort(2, Warns),  %% Sort on file/line
      WarnList = [dialyzer:format_warning(W) || W <- SortedWarns],
      update_editor(Warn, lists:flatten(WarnList)),
      gui_loop(State);
    {BackendPid, done, _NewPlt, NewDocPlt} ->
      message(State, "Analysis done"),
      config_gui_stop(State),
      gui_loop(State#gui_state{doc_plt = NewDocPlt});
    {'EXIT', BackendPid, {error, Reason}} ->
      free_editor(State, ?DIALYZER_ERROR_TITLE, Reason),
      config_gui_stop(State),
      gui_loop(State);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      free_editor(State, ?DIALYZER_ERROR_TITLE, io_lib:format("~p", [Reason])),
      config_gui_stop(State),
      gui_loop(State);
    _Other ->
      %% io:format("Received ~p\n", [Other]),
      gui_loop(State)
  end.

%% ----------------------------------------------------------------
%%
%%  Main window actions
%%

%% ---- Adding and deleting files ----

handle_add_all_click(#gui_state{chosen_box = ChosenBox, file_box = File, 
				file_wd = FWD, mode = Mode}) ->
  case gs:read(File, items) of
    [] ->
      ok;
    Add0 ->
      gs:config(File, [{selection, clear}]),
      Add1 = ordsets:subtract(Add0, [".."]),
      Add = ordsets:from_list([filename:join(FWD, X) || X <- Add1]),
      case gs:read(Mode#mode.start_byte_code, select) of
	true ->
	  add_files(filter_mods(Add, ".beam"), ChosenBox, byte_code);
	false ->
	  add_files(filter_mods(Add, ".erl"), ChosenBox, src_code)
      end  
  end.

all_subdirs(Dirs) ->
  all_subdirs(Dirs, []).

all_subdirs([Dir|T], Acc) ->
  {ok, Files} = file:list_dir(Dir),
  SubDirs = lists:zf(fun(F) ->
                       SubDir = filename:join(Dir, F),
                       case filelib:is_dir(SubDir) of
                         true -> {true, SubDir};
                         false -> false
                       end
                   end, Files),
  NewAcc = ordsets:union(ordsets:from_list(SubDirs), Acc),
  all_subdirs(T ++ SubDirs, NewAcc);
all_subdirs([], Acc) ->
  Acc.

handle_add_rec_click(#gui_state{chosen_box = ChosenBox, file_box = File, 
				file_wd = FWD, mode = Mode}) ->
  case gs:read(File, selection) of
    [] ->
      ok;
    List ->
      gs:config(File, [{selection, clear}]),
      Dirs1 = [gs:read(File, {get, X}) || X <- List],
      Dirs2 = ordsets:from_list([filename:join(FWD, X) || X <- Dirs1]),
      Dirs3 = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Dirs2),
      TargetDirs = ordsets:union(Dirs3, all_subdirs(Dirs3)),
      {Code, Ext} = case gs:read(Mode#mode.start_byte_code, select) of
		      true  -> {byte_code, ".beam"};
		      false -> {src_code, ".erl"}
		    end,
      add_files(filter_mods(TargetDirs, Ext), ChosenBox, Code)
  end.

handle_add_files(#gui_state{chosen_box = ChosenBox, file_box = File, 
			    file_wd = FWD, mode = Mode}) ->
  case gs:read(File, selection) of
    [] ->
      ok;
    List ->
      gs:config(File, [{selection, clear}]),
      Add0 = [gs:read(File, {get, X}) || X <- List],
      Add = ordsets:from_list([filename:join(FWD, X) || X <- Add0]),
      case gs:read(Mode#mode.start_byte_code, select) of
	true -> 
	  add_files(filter_mods(Add, ".beam"), ChosenBox, byte_code);
	false ->
	  add_files(filter_mods(Add, ".erl"), ChosenBox, src_code)
      end
  end.

filter_mods(Mods, Extension) ->
  Fun = fun(X) -> 
	    filename:extension(X) =:= Extension
	      orelse 
		(filelib:is_dir(X) andalso
		 contains_files(X, Extension))
	end,
  ordsets:filter(Fun, Mods).

contains_files(Dir, Extension) ->
  {ok, Files} = file:list_dir(Dir),
  lists:any(fun(X) -> filename:extension(X) =:= Extension end, Files).

add_files(Add, ChosenBox, Type) ->
  Set = gs:read(ChosenBox, items),
  Set1 = 
    case Type of
      byte_code -> filter_mods(Set, ".beam");
      src_code -> filter_mods(Set, ".erl")
    end,
  Files = ordsets:union(Add, Set1),
  gs:config(ChosenBox, [{items, Files}]),
  ok.

handle_file_delete(#gui_state{chosen_box = ChosenBox}) ->
  List = gs:read(ChosenBox, selection),
  lists:foreach(fun(X) -> gs:config(ChosenBox, [{del, X}]) end,
		lists:reverse(lists:sort(List))).

%% ---- Other ----

change_dir_or_add_file(#gui_state{file_wd = FWD, mode = Mode, dir_entry = Dir,
				  chosen_box = CBox, file_box = File} = State,
		       Text) ->
  NewWDorFile =
    case Text of
      ".." -> filename:join(butlast(filename:split(FWD)));
      "." -> FWD;
      _ -> filename:join(FWD, Text)
    end,
  case filelib:is_dir(NewWDorFile) of
    true ->
      gs:config(Dir, [{text, NewWDorFile}]),
      {ok, List} = file:list_dir(NewWDorFile),
      gs:config(File, [{items, [".."|lists:sort(List)]}]),
      State#gui_state{file_wd = NewWDorFile};
    false ->
      case gs:read(Mode#mode.start_byte_code, select) of
	true -> 
	  case filter_mods([NewWDorFile], ".beam") of
	    [] -> ok;
	    RealFiles -> add_files(RealFiles, CBox, byte_code)
	  end;
	false -> 
	  case filter_mods([NewWDorFile], ".erl") of
	    [] -> ok;
	    RealFiles -> add_files(RealFiles, CBox, src_code)
	  end
      end,
      State
  end.

butlast([H1, H2 | T]) ->
  [H1 | butlast([H2|T])];
butlast([_]) ->
  [];
butlast([]) ->
  ["/"].

change_dir_absolute(#gui_state{file_wd = FWD, dir_entry = Dir,
			       file_box = File} = State, 
		    Text) ->
  case filelib:is_dir(Text) of
    true ->
      WD = filename:join(FWD, Text),
      gs:config(Dir, [{text, WD}]),
      {ok, List} = file:list_dir(WD),
      gs:config(File, [{items, [".."|lists:sort(List)]}]),
      State#gui_state{file_wd = WD};
    false ->
      State
  end.

init_warnings([{Tag, GSItem}|Left], LegalWarnings) ->
  Select = ordsets:is_element(Tag, LegalWarnings),
  gs:config(GSItem, [{select, Select}]),
  init_warnings(Left, LegalWarnings);
init_warnings([], _LegalWarnings) ->
  ok.

config_gui_start(State) ->
  Enabled = [{enable, true}],
  Disabled = [{enable, false}],
  gs:config(State#gui_state.stop, Enabled),
  gs:config(State#gui_state.run, Disabled),
  gs:config(State#gui_state.del_file, Disabled),
  gs:config(State#gui_state.clear_chosen, Disabled),
  gs:config(State#gui_state.add_file, Disabled),
  gs:config(State#gui_state.add_all, Disabled),
  gs:config(State#gui_state.add_rec, Disabled),
  gs:config(State#gui_state.clear_warn, Disabled),
  gs:config(State#gui_state.clear_log, Disabled),
  Menu = State#gui_state.menu,
  gs:config(Menu#menu.file_save_warn, Disabled),
  gs:config(Menu#menu.file_save_log, Disabled),
  gs:config(Menu#menu.opts_macros, Disabled),
  gs:config(Menu#menu.opts_includes, Disabled),
  gs:config(Menu#menu.plt_empty, Disabled),
  gs:config(Menu#menu.plt_search_doc, Disabled),
  gs:config(Menu#menu.plt_show_doc, Disabled),
  Mode = State#gui_state.mode,
  gs:config(Mode#mode.start_byte_code, Disabled),
  gs:config(Mode#mode.start_src_code, Disabled).

config_gui_stop(State) ->
  Enabled = [{enable, true}],
  Disabled = [{enable, false}],
  gs:config(State#gui_state.stop, Disabled),
  gs:config(State#gui_state.run, Enabled),
  gs:config(State#gui_state.del_file, Enabled),
  gs:config(State#gui_state.clear_chosen, Enabled),
  gs:config(State#gui_state.add_file, Enabled),
  gs:config(State#gui_state.add_all, Enabled),
  gs:config(State#gui_state.add_rec, Enabled),
  gs:config(State#gui_state.clear_warn, Enabled),
  gs:config(State#gui_state.clear_log, Enabled),
  Menu = State#gui_state.menu,
  gs:config(Menu#menu.file_save_warn, Enabled),
  gs:config(Menu#menu.file_save_log, Enabled),
  gs:config(Menu#menu.opts_macros, Enabled),
  gs:config(Menu#menu.opts_includes, Enabled),
  gs:config(Menu#menu.plt_empty, Enabled),
  gs:config(Menu#menu.plt_search_doc, Enabled),
  gs:config(Menu#menu.plt_show_doc, Enabled),
  Mode = State#gui_state.mode,
  gs:config(Mode#mode.start_byte_code, Enabled),
  gs:config(Mode#mode.start_src_code, Enabled).

%% ----------------------------------------------------------------
%%
%%  Messages
%%

message(State, Message) ->
  output_sms(State, ?DIALYZER_MESSAGE_TITLE, Message).

error_sms(State, Message) ->
  output_sms(State, ?DIALYZER_ERROR_TITLE, Message).

%%
%% This function is to be used *only* for small messages because lines
%% are not wrapped and the created window has a limited area for text.
%% For bigger messages, the function free_editor/3 is to be used.
%%
output_sms(#gui_state{gs = GS, top = TopWin}, Title, Message) ->
  %% Lines = string:words(Message, $\n),
  %% io:format("The message has ~w lines\n", [Lines]),
  WH = [{width, 400}, {height, 100}],
  MessageWin = gs:window(GS, [{title, Title},
			      {default, button, {font, {helvetica, bold, 12}}}
			      |WH]),
  MessagePacker = gs:frame(MessageWin, [{packer_y, [{fixed, 75}, {fixed, 25}]},
					{packer_x, [{fixed, 175},{fixed, 50},
						    {fixed, 175}]}]),
  gs:label(MessagePacker, [{pack_x, {1, 3}}, {pack_y, 1}, 
			   {label, {text, Message}}]),
  OK = gs:button(MessagePacker, [{label, {text, "OK"}}, {pack_xy, {2, 2}}]),
  gs:config(MessageWin, [{map, true}]),
  gs:config(MessagePacker, WH),
  message_loop(OK, MessageWin, TopWin).

message_loop(Ok, Win, TopWin) ->
  receive
    {gs, Ok, click, _, _} ->
      gs:destroy(Win);
    {gs, Win, destroy, _, _} ->
      ok;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, _, _, _, _} ->
      message_loop(Ok, Win, TopWin)
  end.

dialog(#gui_state{gs = GS, top = TopWin}, Message, OkLabel, CancelLabel) ->
  WH = [{width, 400}, {height, 100}],
  WHButton = [{width, 70}, {height, 20}],
  DialogWin = gs:window(GS, [{title, "Dialyzer Message"},
			     {default, button, {font, {helvetica, bold, 12}}}
			     |WH]),
  DialogPacker = gs:frame(DialogWin, [{packer_y, [{fixed, 75}, {fixed, 25}]},
				      {packer_x, [{fixed, 150}, {fixed, 50},
						  {fixed, 50}, {fixed, 150}]}]),
  gs:label(DialogPacker, [{pack_x, {1,4}}, {pack_y, 1}, 
			  {label, {text, Message}}]),
  Ok = gs:button(DialogPacker, [{label, {text, OkLabel}}, 
				{pack_xy, {2,2}}|WHButton]),
  Cancel = gs:button(DialogPacker, [{label, {text, CancelLabel}}, 
				    {pack_xy, {3,2}}|WHButton]),
  gs:config(DialogWin, [{map, true}]),
  gs:config(DialogPacker, WH),
  dialog_loop(Ok, Cancel, DialogWin, TopWin).

dialog_loop(Ok, Cancel, Win, TopWin) ->
  receive
    {gs, Ok, click, _, _} ->
      gs:destroy(Win),
      true;
    {gs, Cancel, click, _, _} ->
      gs:destroy(Win),
      false;
    {gs, Win, destroy, _, _} ->
      false;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, _, _, _, _} ->
      dialog_loop(Ok, Cancel, Win, TopWin)
  end.

maybe_quit(#gui_state{top = TopWin} = State) ->
  case dialog(State, "Do you really want to quit?", "Yes", "No") of
    true ->
      flush(),
      gs:destroy(TopWin),
      gs:stop(),
      true;
    false ->
      false
  end.


%% ----------------------------------------------------------------
%%
%%  Menu actions
%%

%% ---- Help Menu ----

manual(State) ->
  help_menu_common(State, "Dialyzer Manual", 500, "manual.txt", white).

warnings(State) ->
  help_menu_common(State, "Dialyzer Warnings", 500, "warnings.txt", white).

about(State) ->
  help_menu_common(State, "About Dialyzer", 160, "about.txt", yellow).

help_menu_common(#gui_state{gs = GS, top = TopWin} = State,
		 Title, Height, TxtFileName, BackGroundColor) ->
  WH = [{width, 600}, {height, Height}],
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, editor, {bg, BackGroundColor}} | WH]),
  EmptySpace = {stretch, 1},
  Frame = gs:frame(Win, [{packer_x, [EmptySpace, {fixed, 60}, EmptySpace]}, 
			 {packer_y, [EmptySpace, {fixed, 30}]} | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1, 3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2, 2}}]),
  gs:config(Win, [{map, true}]),
  gs:config(Frame, WH),
  AboutFile = filename:join([code:lib_dir(dialyzer), "doc", TxtFileName]),
  case gs:config(Editor, {load, AboutFile}) of
    {error, Reason} ->
      gs:destroy(Win),
      error_sms(State, 
		io_lib:format("Could not find doc/~s file!\n\n ~p", 
			      [TxtFileName, Reason]));
    ok ->
      gs:config(Editor, [{enable, false}]),
      show_info_loop(TopWin, Win, Frame, Button)
  end.

%% ---- File Menu ----

save_log(#gui_state{file_wd = CWD, log = Log} = State) ->
  {Win, Entry, OkButton, CancelButton} = file_box(State, "Save Log", CWD),
  save_loop(State, OkButton, CancelButton, Entry, Win, Log).

save_warn(#gui_state{file_wd = CWD, warnings_box = WBox} = State) ->
  {Win, Entry, OkButton, CancelButton} = file_box(State, "Save Warnings", CWD),
  save_loop(State, OkButton, CancelButton, Entry, Win, WBox).

file_box(#gui_state{gs = GS}, Title, Default) ->
  WH = [{width, 400}, {height, 75}],
  Win = gs:window(GS, [{title, Title}|WH]),
  Fix25 = {fixed, 27}, Fix75 = {fixed, 75},
  WinPacker = gs:frame(Win, [{packer_y, [Fix25, Fix25, Fix25]},
			     {packer_x, [Fix75, Fix75, Fix75, {fixed, 175}]}]),
  gs:label(WinPacker, [{pack_xy, {1,2}}, {label, {text, "Enter file:"}}]),
  Entry = gs:entry(WinPacker, [{pack_x, {2,4}}, {pack_y, 2}, {keypress, true}]),
  OkButton = gs:button(WinPacker, [{label, {text, "Ok"}}, {pack_xy, {2,3}}]),
  CancelButton = gs:button(WinPacker, [{label, {text, "Cancel"}}, 
				       {pack_xy, {3,3}}]),
  gs:config(Entry, [{text, Default}]),
  gs:config(Win, [{map, true}]),
  gs:config(WinPacker, WH),
  {Win, Entry, OkButton, CancelButton}.

save_loop(#gui_state{top = TopWin} = State,
	  OkButton, CancelButton, Entry, Save, Editor) ->
  receive
    {gs, OkButton, click, _, _} ->
      File = gs:read(Entry, text),
      case gs:config(Editor, [{save, File}]) of
	{error, _} ->
	  error_sms(State, "Could not write to file:\n" ++ File),
	  save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
	_ ->
	  gs:destroy(Save)
      end;
    {gs, Entry, keypress, _, ['Return'|_]} ->
      File = gs:read(Entry, text),
      case gs:config(Editor, [{save, File}]) of
	{error, _} ->
	  error_sms(State, "Could not write to file:\n" ++ File),
	  save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
	_ ->
	  gs:destroy(Save)
      end;
    {gs, Entry, keypress, _, _} ->
      save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Save);
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, Save, destroy, _, _} ->
      ok;
    {gs, _, _, _, _} ->
      save_loop(State, OkButton, CancelButton, Entry, Save, Editor)
  end.

%% ---- Plt Menu ----

search_doc_plt(#gui_state{gs = GS, top = TopWin} = State) ->
  WH = [{width, 400}, {height, 100}],
  WHB = [{width, 120}, {height, 30}],
  Title = io_lib:format("Search the PLT", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  EmptySpace = {stretch, 1},
  Frame = gs:frame(Win, [{packer_x, [EmptySpace, EmptySpace, EmptySpace]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, 
				     EmptySpace, {fixed, 30}]} | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Module"}}]),
  ModEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  gs:label(Frame, [{pack_xy, {2,1}}, {label, {text, "Function"}}]),
  FunEntry = gs:entry(Frame, [{pack_xy, {2,2}}]),
  gs:label(Frame, [{pack_xy, {3,1}}, {label, {text, "Arity"}}]),
  ArityEntry = gs:entry(Frame, [{pack_xy, {3,2}}]),
  ButtonPacker = gs:frame(Frame, [{pack_xy, {2,4}}, 
				  {packer_x, [{fixed, 60}, {fixed, 60}]},
				  {packer_y, {fixed, 30}}]),
  SearchButton = gs:button(ButtonPacker, [{label, {text, "Search"}}, 
					  {pack_xy, {1,1}}]),
  CancelButton = gs:button(ButtonPacker, [{label, {text, "Cancel"}}, 
					  {pack_xy, {2,1}}]),
  gs:config(Win, [{map, true}]),
  gs:config(Frame, WH),
  gs:config(ButtonPacker, WHB),
  search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
		      FunEntry, ArityEntry, Win, TopWin).

search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
		    FunEntry, ArityEntry, Win, TopWin) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, SearchButton, click, _, _} ->
      M = format_search(gs:read(ModEntry, text)),
      F = format_search(gs:read(FunEntry, text)),
      A = format_search(gs:read(ArityEntry, text)),
      case dialyzer_plt:get_specs(State#gui_state.doc_plt, M, F, A) of
	"" ->
	  error_sms(State, "No such function"),
	  search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
			      FunEntry, ArityEntry, Win, TopWin);
	NonEmptyString ->
	  gs:destroy(Win),
	  free_editor(State, "Content of PLT", NonEmptyString)
      end
  end.

format_search([]) ->
  '_';
format_search(String) ->
  try list_to_integer(String)
  catch error:_ -> list_to_atom(String)
  end.

show_doc_plt(#gui_state{doc_plt = DocPLT} = State) ->
  case dialyzer_plt:get_specs(DocPLT) of
    "" -> error_sms(State, "No analysis has been made yet!\n");
    NonEmptyString -> free_editor(State, "Content of PLT", NonEmptyString)
  end.

free_editor(#gui_state{gs = GS, top = TopWin}, Title, Contents0) ->
  Contents = lists:flatten(Contents0),
  Tokens = string:tokens(Contents, "\n"),
  NofLines = length(Tokens),
  LongestLine = lists:max([length(X) || X <- Tokens]),
  Height0 = NofLines * 25 + 80,
  Height = if Height0 > 500 -> 500; true -> Height0 end,
  Width0 = LongestLine * 7 + 60,
  Width = if Width0 > 800 -> 800; true -> Width0 end,
  WH = [{width, Width}, {height, Height}],
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  EmptySpace = {stretch, 1},
  Frame = gs:frame(Win, [{packer_x, [EmptySpace, {fixed, 60}, EmptySpace]},
			 {packer_y, [EmptySpace, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}, {enable, true}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),
  gs:config(Editor, [{insert, {insert, Contents}}]),
  gs:config(Editor, [{enable, false}]),
  gs:config(Win, [{map, true}]),
  gs:config(Frame, WH),
  show_info_loop(TopWin, Win, Frame, Button).

%% ---- Common ----

show_info_loop(TopWin, Win, Frame, Button) ->
  receive
    {gs, Button, click, _, _} ->
      gs:destroy(Win);
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, Win, destroy, _, _} ->
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      show_info_loop(TopWin, Win, Frame, Button)
  end.

include_dialog(#gui_state{gs = GS, options = Options}, Parent) ->
  WH = [{width, 300}, {height, 400}],
  Title = io_lib:format("Include Directories", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, entry, {bg, white}}| WH]),
  EmptySpace = {stretch, 1},
  Frame = gs:frame(Win, [{packer_x, [EmptySpace]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, {fixed, 30},
				     EmptySpace, {fixed, 30}, {fixed, 30}]}
			 | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Directory"}}]),
  DirEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  ButtonPacker1 = gs:frame(Frame, [{pack_xy, {1,3}},
				   {packer_x, [{fixed, 70}, {fixed, 70},
					       EmptySpace]},
				   {packer_y, {fixed, 30}}]),
  AddButton = gs:button(ButtonPacker1, [{label, {text, "Add"}}, 
					{pack_xy, {1,1}}]),
  Dirs = [io_lib:format("~s", [X]) || X <- Options#options.include_dirs],
  DirBox = gs:listbox(Frame, [{pack_xy, {1,4}}, {vscroll, right},
			      {bg, white}, {configure, true},
			      {selectmode, multiple}, {items, Dirs}]),
  ButtonPacker2 = gs:frame(Frame, [{pack_xy, {1,5}},
				   {packer_x, [{fixed, 60}, {fixed, 70},
					       EmptySpace]},
				   {packer_y, {fixed, 30}}]),
  DeleteButton = gs:button(ButtonPacker2, [{label, {text, "Delete"}}, 
					   {pack_xy, {1,1}}]),
  DeleteAllButton = gs:button(ButtonPacker2, [{label, {text, "Delete All"}}, 
					      {pack_xy, {2,1}}]),
  ButtonPacker3 = gs:frame(Frame, [{pack_xy, {1,6}},
				   {packer_x, [EmptySpace,
					       {fixed, 60}, {fixed, 60}]},
				   {packer_y, {fixed, 30}}]),
  OkButton = gs:button(ButtonPacker3, [{label, {text, "Ok"}},
				       {pack_xy, {2,1}}]),
  CancelButton = gs:button(ButtonPacker3, [{label, {text, "Cancel"}},
					   {pack_xy, {3,1}}]),
  gs:config(Win, [{map, true}]),
  gs:config(Frame, WH),
  include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton,
	       DirBox, DirEntry, OkButton, CancelButton, Win).

include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	     DirBox, DirEntry, OkButton, CancelButton, Win) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, OkButton, click, _, _} ->
      gs:destroy(Win),
      Parent ! {new_options, Options},
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, AddButton, click, _, _} ->
      Dirs = Options#options.include_dirs,
      NewDirs =
	case gs:read(DirEntry, text) of
	  [] -> Dirs;
	  Add -> [Add|Dirs]
	end,
      NewOptions = Options#options{include_dirs = NewDirs},
      gs:config(DirBox, [{items, NewDirs}]),
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, DeleteAllButton, click, _, _} ->
      gs:config(DirBox, [clear]),
      NewOptions = Options#options{include_dirs = []},      
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, DeleteButton, click, _, _} ->
      NewOptions =
	case gs:read(DirBox, selection) of
	  [] ->
	    Options;
	  List ->
	    lists:foreach(fun(X) -> gs:config(DirBox, [{del, X}]) end,
			  lists:sort(List)),
	    NewDirs = gs:read(DirBox, items),
	    Options#options{include_dirs = NewDirs}
	end,
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, Win, destroy, _, _} ->
      ok
  end.

macro_dialog(#gui_state{gs = GS, options = Options}, Parent) ->
  WH = [{width, 300}, {height, 400}],
  Title = io_lib:format("Macro Definitions", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, entry, {bg, white}}| WH]),
  EmptySpace = {stretch, 1},
  Frame = gs:frame(Win, [{packer_x, [EmptySpace, EmptySpace]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, {fixed, 30},
				     EmptySpace, {fixed, 30}, {fixed, 30}]}
			 | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Macro"}}]),
  MacroEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  gs:label(Frame, [{pack_xy, {2,1}}, {label, {text, "Term"}}]),
  TermEntry = gs:entry(Frame, [{pack_xy, {2,2}}]),
  ButtonPacker1 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 3}, 
				   {packer_x, [{fixed, 70},{fixed, 70},
					       EmptySpace]},
				   {packer_y, {fixed, 30}}]),
  AddButton = gs:button(ButtonPacker1, [{label, {text, "Add"}}, 
					{pack_xy, {1,1}}]),
  Macros = [io_lib:format("~p = ~p",[X,Y]) || {X,Y} <- Options#options.defines],
  MacroBox = gs:listbox(Frame, [{pack_x, {1,2}}, {pack_y, 4}, {vscroll, right},
				{bg, white}, {configure, true},
				{selectmode, multiple},
				{items, Macros}]),
  ButtonPacker2 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 5}, 
				   {packer_x, [{fixed, 60}, {fixed, 70},
					       EmptySpace]},
				   {packer_y, {fixed, 30}}]),
  DeleteButton = gs:button(ButtonPacker2, [{label, {text, "Delete"}}, 
					   {pack_xy, {1,1}}]),
  DeleteAllButton = gs:button(ButtonPacker2, [{label, {text, "Delete All"}}, 
					      {pack_xy, {2,1}}]),
  ButtonPacker3 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 6}, 
				   {packer_x, [EmptySpace,
					       {fixed, 60}, {fixed, 60}]},
				   {packer_y, {fixed, 30}}]),
  OkButton = gs:button(ButtonPacker3, [{label, {text, "Ok"}},
				       {pack_xy, {2,1}}]),
  CancelButton = gs:button(ButtonPacker3, [{label, {text, "Cancel"}},
					   {pack_xy, {3,1}}]),
  gs:config(Win, [{map, true}]),
  gs:config(Frame, WH),
  macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	     MacroBox, MacroEntry, TermEntry, OkButton, CancelButton, Win).

macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	   MacroBox, MacroEntry, TermEntry, OkButton, CancelButton, Win) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, OkButton, click, _, _} ->
      gs:destroy(Win),
      Parent ! {new_options, Options},
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, AddButton, click, _, _} ->
      Defines = Options#options.defines,
      NewDefines = 
	case gs:read(MacroEntry, text) of
	  "" -> Defines;
	  Macro ->
	    Empty = [{text, ""}],
	    case gs:read(TermEntry, text) of
	      "" -> 
		gs:config(MacroEntry, Empty),
		orddict:store(list_to_atom(Macro), true, Defines);
	      String ->
		case parse(String) of
		  {ok, Term} ->
		    gs:config(MacroEntry, Empty),
		    gs:config(TermEntry, Empty),
		    orddict:store(list_to_atom(Macro), Term, Defines);
		  {error, _Reason} ->
		    Defines
		end
	    end
	end,
      NewOptions = Options#options{defines = NewDefines},		 
      NewEntries = [io_lib:format("~p = ~p", [X, Y]) || {X, Y} <- NewDefines],
      gs:config(MacroBox, [{items, NewEntries}]),
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, DeleteAllButton, click, _, _} ->
      gs:config(MacroBox, [clear]),
      NewOptions = Options#options{defines = []},
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, DeleteButton, click, _, _} ->
      NewOptions =
	case gs:read(MacroBox, selection) of
	  [] ->
	    Options;
	  List ->
	    gs:config(MacroBox, [{selection, clear}]),
	    Fun = 
	      fun(X) ->
		  Val = gs:read(MacroBox, {get, X}),
		  [MacroName|_] = re:split(Val, " ", [{return, list}]),
		  list_to_atom(MacroName)
	      end,
	    Delete = [Fun(X) || X <- List],
	    lists:foreach(fun(X) -> gs:config(MacroBox, [{del, X}]) end,
			  lists:reverse(lists:sort(List))),
	    Defines = Options#options.defines,
	    NewDefines = lists:foldl(fun(X, Acc) -> 
					 orddict:erase(X, Acc)
				     end,
				     Defines, Delete),
	    Options#options{defines = NewDefines}
	end,
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, Win, destroy, _, _} ->
      ok
  end.

parse(String) ->
  case erl_scan:string(String ++ ".", 1) of
    {ok, Ts, _} ->
      case erl_parse:parse_exprs(Ts) of
	{ok, [Expr]} ->	  
	  try erl_parse:normalise(Expr)
	  catch error:Reason -> {error, Reason}
	  end;
	{error, E} ->
	  parse_error(E)
      end;
    {error, E, _} ->
      parse_error(E)
  end.

parse_error(E) ->
  S = io_lib:fwrite("Error parsing expression: ~P.", [E,15]),
  {error, S}.

%% ----------------------------------------------------------------
%%
%%  Run the analysis
%%

start_analysis(State) ->
  Analysis = build_analysis_record(State),
  case get_anal_files(State, Analysis#analysis.start_from) of
    error ->
      Msg = "You must choose one or more files or dirs\n"
	"before starting the analysis!",
      error_sms(State, Msg),
      config_gui_stop(State),      
      State;
    {ok, Files} ->
      Msg = "\n========== Starting Analysis ==========\n\n",
      update_editor(State#gui_state.log, Msg),
      NewAnalysis = Analysis#analysis{files = Files},
      run_analysis(State, NewAnalysis)
  end.

build_analysis_record(#gui_state{mode = Mode, menu = Menu, options = Options,
				 init_plt = InitPlt0}) ->
  StartFrom =
    case gs:read(Mode#mode.start_byte_code, select) of
      true -> byte_code;
      false -> src_code
    end,
  InitPlt =
    case gs:read(Menu#menu.plt_empty, select) of
      true -> dialyzer_plt:new();
      false -> InitPlt0
    end,
  #analysis{defines = Options#options.defines,
	    include_dirs = Options#options.include_dirs,
	    plt = InitPlt,
	    start_from = StartFrom}.

get_anal_files(#gui_state{chosen_box = ChosenBox}, StartFrom) ->
  Files = gs:read(ChosenBox, items),
  FilteredMods =
    case StartFrom of
      src_code -> filter_mods(Files, ".erl");
      byte_code -> filter_mods(Files, ".beam")
    end,
  FilteredDirs = [X || X <- Files, filelib:is_dir(X)],
  case ordsets:union(FilteredMods, FilteredDirs) of
    [] -> error;
    Set -> {ok, Set}
  end.

run_analysis(State, Analysis) ->
  config_gui_start(State),
  Self = self(),
  NewAnalysis = Analysis#analysis{doc_plt = dialyzer_plt:new()},
  LegalWarnings = find_legal_warnings(State),
  Fun = 
    fun() -> 
	dialyzer_analysis_callgraph:start(Self, LegalWarnings, NewAnalysis)
    end,
  BackendPid = spawn_link(Fun),
  State#gui_state{backend_pid = BackendPid}.

find_legal_warnings(#gui_state{menu = #menu{warnings = Warnings}}) ->
  ordsets:from_list([Tag || {Tag, GSItem} <- Warnings, 
			    gs:read(GSItem, select) =:= true]).

flush() ->
  receive
    _ -> flush()
  after 
    0 -> ok
  end.

update_editor(Editor, Msg) ->
  gs:config(Editor, [{enable, true}]),
  NofRows = gs:read(Editor, size),
  gs:config(Editor, [{insertpos, 'end'}]),
  gs:config(Editor, [{insert, {insert, Msg}}]),
  NewNofRows = gs:read(Editor, size),
  ScrollPos = gs:read(Editor, vscrollpos),
  gs:config(Editor, [{vscrollpos, ScrollPos + NewNofRows - NofRows}]),
  gs:config(Editor, [{enable, false}]).
