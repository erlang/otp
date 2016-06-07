%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2015. All Rights Reserved.
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
%%

%%%-----------------------------------------------------------------------
%%% File    : dialyzer_gui_wx.erl
%%% Authors : Elli Fragkaki <ellifrag@gmail.com>
%%% Description : The wx-based graphical user interface of dialyzer.
%%%
%%% Created : 07 Oct 2009 by Elli Fragkaki <ellifrag@gmail.com>
%%%-----------------------------------------------------------------------

-module(dialyzer_gui_wx).

-export([start/1]).

-include("dialyzer.hrl").
-include("dialyzer_gui_wx.hrl").

%%------------------------------------------------------------------------

-define(DIALYZER_ERROR_TITLE,   "Dialyzer Error").
-define(DIALYZER_MESSAGE_TITLE, "Dialyzer Message").

%%------------------------------------------------------------------------

-record(menu, {file                   :: wx:wx_object(),
	       warnings               :: wx:wx_object(),
	       plt                    :: wx:wx_object(),
	       options                :: wx:wx_object(),
	       help                   :: wx:wx_object()}).
-type menu() :: #menu{}.

-record(gui_state, {add               :: wx:wx_object(),
		    add_dir           :: wx:wx_object(),
		    add_rec           :: wx:wx_object(),
		    chosen_box        :: wx:wx_object(),
		    del_file          :: wx:wx_object(),
		    doc_plt           :: dialyzer_plt:plt(),
		    clear_chosen      :: wx:wx_object(),
		    clear_log         :: wx:wx_object(),
		    explain_warn      :: wx:wx_object(),
		    clear_warn        :: wx:wx_object(),
		    init_plt          :: dialyzer_plt:plt(),
		    dir_entry         :: wx:wx_object(),
		    file_box          :: wx:wx_object(),
		    files_to_analyze  :: ordsets:ordset(string()),
		    gui               :: wx:wx_object(),
		    log               :: wx:wx_object(),
		    menu              :: menu(),
		    mode              :: wx:wx_object(),
		    options           :: #options{},
		    run               :: wx:wx_object(),
		    stop              :: wx:wx_object(),
		    frame             :: wx:wx_object(),
		    warnings_box      :: wx:wx_object(),
		    explanation_box   :: wx:wx_object() | 'undefined',
		    wantedWarnings    :: list(),
		    rawWarnings       :: list(),
		    backend_pid       :: pid() | 'undefined',
		    expl_pid          :: pid() | 'undefined'}).
	       
%%------------------------------------------------------------------------

-spec start(#options{}) -> ?RET_NOTHING_SUSPICIOUS.

start(DialyzerOptions) ->
  process_flag(trap_exit, true),
  Wx = wx:new(),
  State = wx:batch(fun() -> create_window(Wx, DialyzerOptions) end),
  gui_loop(State).

create_window(Wx, #options{init_plts = InitPltFiles} = DialyzerOptions) ->
  {ok, Host} = inet:gethostname(),

  %%---------- initializing frame ---------
  Frame = wxFrame:new(Wx, -1,  "Dialyzer " ++ ?VSN ++ " @ " ++ Host),  
  wxFrame:connect(Frame, close_window),
  FileMenu = createFileMenu(),
  WarningsMenu = createWarningsMenu(),
  PltMenu = createPltMenu(),
  OptionsMenu = createOptionsMenu(),
  HelpMenu = createHelpMenu(),

  MenuBar = wxMenuBar:new(),
  wxMenuBar:append(MenuBar, FileMenu,     "File"),
  wxMenuBar:append(MenuBar, WarningsMenu, "Warnings"),
  wxMenuBar:append(MenuBar, PltMenu,      "Plt"),
  wxMenuBar:append(MenuBar, OptionsMenu,  "Options"),
  wxMenuBar:append(MenuBar, HelpMenu,     "Help"),
  wxFrame:setMenuBar(Frame, MenuBar),
  ok = wxFrame:connect(Frame, command_menu_selected),

  %%----------- Set Labels -------------
  Lab1 = wxStaticText:new(Frame, ?LABEL1, "Directories or modules to analyze"),
  OptionsLabel = wxStaticText:new(Frame, ?LABEL2, "Analysis Options"),
  LogLabel = wxStaticText:new(Frame, ?LABEL3, "Log"),
  FileLabel = wxStaticText:new(Frame, ?LABEL4, "File: "),
  DirLabel = wxStaticText:new(Frame, ?LABEL5, "Dir: "),
  WarningsLabel = wxStaticText:new(Frame, ?LABEL6, "Warnings"),

  %%---------- Set TextBoxes -----------
  ChosenBox = wxListBox:new(Frame, ?ChosenBox,
			[{size, {250,200}},
			 {style, ?wxLB_EXTENDED bor ?wxLB_HSCROLL
			  bor ?wxLB_NEEDED_SB}]),
  LogBox = wxTextCtrl:new(Frame, ?LogBox,
			[{size, {530,200}},
			 {style, ?wxTE_MULTILINE
			  bor ?wxTE_READONLY bor ?wxHSCROLL}]),
  DefaultPath = code:root_dir(),
 
  FilePicker = wxFilePickerCtrl:new(Frame, ?FilePicker,
				   [{path, DefaultPath},
				    {message, "Choose File to Analyse"},
				    {style,?wxFLP_FILE_MUST_EXIST bor ?wxFLP_USE_TEXTCTRL}]),
  wxPickerBase:setTextCtrlProportion(FilePicker,3),
  wxPickerBase:setPickerCtrlProportion(FilePicker,2),
  DirPicker = wxDirPickerCtrl:new(Frame, ?DirPicker,
				   [{path, DefaultPath},
				    {message, "Choose Directory to Analyze"},
				    {style,?wxDIRP_DIR_MUST_EXIST bor ?wxDIRP_USE_TEXTCTRL}]),
  WarningsBox = wxListBox:new(Frame, ?WarningsBox,
			[{size, {700,200}},
			 {style,  ?wxLB_HSCROLL
			  bor ?wxLB_NEEDED_SB}]),

  %%--------- Set Buttons --------------
  DeleteButton = wxButton:new(Frame, ?Del_Button, [{label, "Delete"}]),
  DeleteAllButton = wxButton:new(Frame, ?DelAll_Button, [{label, "Delete All"}]),
  FileType = wxRadioBox:new(Frame, ?RADIOBOX, " File Type: " , {1,1}, {150,90},
			    [["BeamFiles"],["SourceFiles"]]),
  ClearLogButton = wxButton:new(Frame, ?ClearLog_Button, [{label, "Clear Log"}]),
  AddButton = wxButton:new(Frame, ?Add_Button, [{label, "Add"}]),
  AddDirButton = wxButton:new(Frame, ?AddDir_Button, [{label, "Add Dir"}]),
  AddRecButton = wxButton:new(Frame, ?AddRec_Button, [{label, "Add Recursively"}]),
  ExplainWarnButton = wxButton:new(Frame, ?ExplWarn_Button, [{label, "Explain Warning"}]),
  ClearWarningsButton = wxButton:new(Frame, ?ClearWarn_Button, [{label, "Clear Warnings"}]),
  RunButton = wxButton:new(Frame, ?Run_Button, [{label, "Run"}]),
  StopButton = wxButton:new(Frame, ?Stop_Button, [{label, "Stop"}]),
  wxWindow:disable(StopButton),
  %%--------- Connect Buttons -----------
  wxButton:connect(DeleteButton, command_button_clicked),
  wxButton:connect(DeleteAllButton, command_button_clicked),
  wxButton:connect(ClearLogButton, command_button_clicked),
  wxButton:connect(AddButton, command_button_clicked),
  wxButton:connect(AddDirButton, command_button_clicked),
  wxButton:connect(AddRecButton, command_button_clicked),
  wxButton:connect(ExplainWarnButton, command_button_clicked),
  wxButton:connect(ClearWarningsButton, command_button_clicked),
  wxButton:connect(RunButton, command_button_clicked),
  wxButton:connect(StopButton, command_button_clicked),

  %%------------Set Layout ------------
  All = wxBoxSizer:new(?wxVERTICAL),
  Top = wxBoxSizer:new(?wxHORIZONTAL),
  Left = wxBoxSizer:new(?wxVERTICAL),
  Right = wxBoxSizer:new(?wxVERTICAL),
  RightUp = wxBoxSizer:new(?wxHORIZONTAL),

  Opts = [{flag,?wxEXPAND bor ?wxALL}, {proportion,1}, {border, 1}],
  Opts3 = [{flag,?wxEXPAND bor ?wxALL}, {proportion,3}, {border, 1}],
  Center = [{flag, ?wxALIGN_CENTER_HORIZONTAL}],

  ChooseItem = wxBoxSizer:new(?wxVERTICAL),
  FileTypeItem = wxBoxSizer:new(?wxVERTICAL),
  LogItem = wxBoxSizer:new(?wxVERTICAL),
  FileDirItem = wxBoxSizer:new(?wxVERTICAL),
  FileItem = wxBoxSizer:new(?wxHORIZONTAL),
  DirItem = wxBoxSizer:new(?wxHORIZONTAL),
  AddDirButtons = wxBoxSizer:new(?wxHORIZONTAL),
  WarningsItem = wxBoxSizer:new(?wxVERTICAL),
  ChooseButtons = wxBoxSizer:new(?wxHORIZONTAL),
  WarnButtons = wxBoxSizer:new(?wxHORIZONTAL),
  RunButtons = wxBoxSizer:new(?wxHORIZONTAL),
  Buttons = wxFlexGridSizer:new(3),
  
  _ = wxSizer:add(ChooseButtons, DeleteButton, ?BorderOpt),
  _ = wxSizer:add(ChooseButtons, DeleteAllButton, ?BorderOpt),
  _ = wxSizer:add(ChooseItem, Lab1, Center),
  _ = wxSizer:add(ChooseItem, ChosenBox, Opts),
  _ = wxSizer:add(ChooseItem, ChooseButtons, ?BorderOpt),
  _ = wxSizer:add(FileTypeItem, OptionsLabel),
  _ = wxSizer:add(FileTypeItem, FileType, [{border, 5}, {flag, ?wxALL}]),
  _ = wxSizer:add(LogItem, LogLabel, Center),
  _ = wxSizer:add(LogItem, LogBox, Opts3),
  _ = wxSizer:add(LogItem, ClearLogButton, ?BorderOpt),
  _ = wxSizer:add(FileItem, FileLabel),
  _ = wxSizer:add(FileItem, FilePicker),
  _ = wxSizer:add(DirItem, DirLabel),
  _ = wxSizer:add(DirItem, DirPicker),
  _ = wxSizer:add(AddDirButtons, AddDirButton, ?BorderOpt),
  _ = wxSizer:add(AddDirButtons, AddRecButton, ?BorderOpt),
  _ = wxSizer:add(FileDirItem, FileItem),
  _ = wxSizer:add(FileDirItem, AddButton, ?BorderOpt),
  _ = wxSizer:add(FileDirItem, DirItem, ?BorderOpt),
  _ = wxSizer:add(FileDirItem, AddDirButtons, ?BorderOpt),
  _ = wxSizer:add(WarnButtons, ExplainWarnButton, ?BorderOpt),
  _ = wxSizer:add(WarnButtons, ClearWarningsButton, ?BorderOpt),
  _ = wxSizer:add(RunButtons, RunButton, ?BorderOpt),
  _ = wxSizer:add(RunButtons, StopButton, ?BorderOpt),
  _ = wxSizer:add(Buttons, WarnButtons),
  _ = wxSizer:add(Buttons, wxStaticText:new(Frame, ?LABEL7, ""),
		  [{flag, ?wxEXPAND}]),
  _ = wxSizer:add(Buttons, RunButtons),
  _ = wxFlexGridSizer:addGrowableCol(Buttons, 1),
  _ = wxSizer:add(WarningsItem, WarningsLabel, Center),
  _ = wxSizer:add(WarningsItem, WarningsBox, Opts3),
  _ = wxSizer:add(WarningsItem, Buttons,
		  [{flag, ?wxEXPAND bor ?wxALL}, ?Border]),
  _ = wxSizer:add(Left, ChooseItem, Opts),
  _ = wxSizer:add(Left, FileDirItem,
		  [{proportion, 1}, {border, 60}, {flag, ?wxTOP}]),
  _ = wxSizer:add(RightUp, FileTypeItem, ?BorderOpt),
  _ = wxSizer:add(RightUp, LogItem, Opts3),
  _ = wxSizer:add(Right, RightUp, Opts3),
  _ = wxSizer:add(Right, WarningsItem, Opts3),
  _ = wxSizer:add(Top, Left, Opts),
  _ = wxSizer:add(Top, Right, Opts3),

  _ = wxSizer:add(All, Top, Opts),
  wxWindow:setSizer(Frame, All),
  wxWindow:setSizeHints(Frame, {1150,600}),
  wxWindow:show(Frame),
 
  Warnings = [{?WARN_RETURN_NO_RETURN, ?menuID_WARN_NO_RETURN_FUN},
	      {?WARN_RETURN_ONLY_EXIT, ?menuID_WARN_ERROR_HANDLING_FUN},
	      {?WARN_NOT_CALLED, ?menuID_WARN_UNUSED_FUN},
	      {?WARN_NON_PROPER_LIST, ?menuID_WARN_LIST_CONSTR},
	      {?WARN_FUN_APP, ?menuID_WARN_BAD_FUN},
	      {?WARN_MATCHING, ?menuID_WARN_MATCH_FAILURES},
	      {?WARN_OPAQUE, ?menuID_WARN_OPAQUE},
	      {?WARN_FAILING_CALL, ?menuID_WARN_FAIL_FUN_CALLS},
	      {?WARN_CALLGRAPH, ?menuID_WARN_UNEXPORTED_FUN},
              {?WARN_RACE_CONDITION, ?menuID_WARN_RACE_CONDITIONS},
	      %% For contracts.
	      {?WARN_CONTRACT_TYPES,?menuID_WARN_WRONG_CONTRACTS},
	      {?WARN_CONTRACT_SYNTAX, ?menuID_WARN_CONTRACT_SYNTAX}
	     ],
  Menu = #menu{file = FileMenu,
	       warnings = WarningsMenu,
	       plt = PltMenu,
	       options =OptionsMenu,
	       help = HelpMenu},

  InitPlt =
    case InitPltFiles of
      [] -> dialyzer_plt:new();
      _ ->
        Plts = [dialyzer_plt:from_file(F) || F <- InitPltFiles],
        dialyzer_plt:merge_plts_or_report_conflicts(InitPltFiles, Plts)
    end,
  
  #gui_state{add = AddButton,
	     add_dir = AddDirButton,
	     add_rec = AddRecButton,
	     chosen_box = ChosenBox, 
	     clear_chosen = DeleteAllButton, 
	     clear_log = ClearLogButton, 
	     explain_warn = ExplainWarnButton,
	     clear_warn = ClearWarningsButton, 
	     del_file = DeleteButton, 
	     doc_plt = dialyzer_plt:new(),
	     dir_entry = DirPicker,
	     file_box = FilePicker, 
	     files_to_analyze = ordsets:new(),
	     gui = Wx,
	     init_plt = InitPlt,
	     log = LogBox,
	     menu = Menu,
	     mode = FileType,
	     options = DialyzerOptions,
	     run = RunButton,
	     stop = StopButton,
	     frame = Frame, 
	     warnings_box = WarningsBox,
	     wantedWarnings = Warnings,
	     rawWarnings = []}.

createFileMenu() ->
  FileMenu = wxMenu:new(),
  _ = wxMenu:append(FileMenu, wxMenuItem:new([{id, ?menuID_FILE_SAVE_WARNINGS},
					      {text, "Save &Warnings"}])),
  _ = wxMenu:append(FileMenu, wxMenuItem:new([{id,   ?menuID_FILE_SAVE_LOG},
					      {text, "Save &Log"}])),
  _ = wxMenu:append(FileMenu, wxMenuItem:new([{id,   ?menuID_FILE_QUIT},
					      {text, "E&xit\tAlt-X"}])),
  FileMenu.

createWarningsMenu() ->
  WarningsMenu = wxMenu:new(),
  addCheckedItem(WarningsMenu, ?menuID_WARN_MATCH_FAILURES, "Match failures"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_FAIL_FUN_CALLS,
		 "Failing function calls"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_BAD_FUN, "Bad fun applications"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_OPAQUE, "Opaqueness violations"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_LIST_CONSTR,
		 "Improper list constructions"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_UNUSED_FUN, "Unused functions"),
  _ = wxMenu:appendCheckItem(WarningsMenu, ?menuID_WARN_ERROR_HANDLING_FUN,
			     "Error handling functions"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_NO_RETURN_FUN,
		 "Functions of no return"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_UNEXPORTED_FUN,
		 "Call to unexported function"),
  _ = wxMenu:appendCheckItem(WarningsMenu, ?menuID_WARN_RACE_CONDITIONS,
			     "Possible race conditions"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_WRONG_CONTRACTS, "Wrong contracts"),
  addCheckedItem(WarningsMenu, ?menuID_WARN_CONTRACT_SYNTAX,
		 "Wrong contract syntax"),
  WarningsMenu.

addCheckedItem(Menu, ItemId, Str) ->
  _ = wxMenu:appendCheckItem(Menu, ItemId, Str), 
  wxMenu:check(Menu, ItemId, true).

createPltMenu() ->
  PltMenu = wxMenu:new(),
  _ = wxMenu:appendCheckItem(PltMenu, ?menuID_PLT_INIT_EMPTY,
			     "Init with empty PLT"),
  _ = wxMenu:append(PltMenu, wxMenuItem:new([{id, ?menuID_PLT_SHOW_CONTENTS},
					     {text, "Show contents"}])),
  _ = wxMenu:append(PltMenu, wxMenuItem:new([{id, ?menuID_PLT_SEARCH_CONTENTS},
					     {text, "Search contents"}])),
  PltMenu.

createOptionsMenu() ->
  OptsMenu  = wxMenu:new(),
  _ = wxMenu:append(OptsMenu, wxMenuItem:new([{id, ?menuID_OPTIONS_MACRO},
					      {text, "Manage Macro Definitions"}])),
  _ = wxMenu:append(OptsMenu, wxMenuItem:new([{id, ?menuID_OPTIONS_INCLUDE_DIR},
					      {text, "Manage Include Directories"}])),
  OptsMenu.

createHelpMenu() ->
  HelpMenu = wxMenu:new(),
  _ = wxMenu:append(HelpMenu, wxMenuItem:new([{id, ?menuID_HELP_MANUAL},
					      {text, "Manual"}])),
  _ = wxMenu:append(HelpMenu, wxMenuItem:new([{id, ?menuID_HELP_WARNING_OPTIONS},
					      {text, "Warning Options"}])),
  _ = wxMenu:append(HelpMenu, wxMenuItem:new([{id, ?menuID_HELP_ABOUT},
					      {text, "About"}])),
  HelpMenu.

%% ----------------------------------------------------------------
%%
%%  Main GUI Loop
%%

-spec gui_loop(#gui_state{}) -> ?RET_NOTHING_SUSPICIOUS.

gui_loop(#gui_state{backend_pid = BackendPid, doc_plt = DocPlt,
		    log = Log, frame = Frame,
		    warnings_box = WarningsBox} = State) ->
  receive 
    #wx{event = #wxClose{}} ->
      %% io:format("~p Closing window ~n", [self()]),
      ok = wxFrame:setStatusText(Frame, "Closing...",[]),
      wxWindow:destroy(Frame),
      ?RET_NOTHING_SUSPICIOUS;
    %% ----- Menu -----
    #wx{id = ?menuID_FILE_SAVE_LOG, obj = Frame, 
	event = #wxCommand{type = command_menu_selected}} ->
      save_file(State, log),
      gui_loop(State);
    #wx{id=?menuID_FILE_SAVE_WARNINGS, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      save_file(State, warnings),
      gui_loop(State);
    #wx{id=?menuID_FILE_QUIT, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      case maybe_quit(State) of
	true -> ?RET_NOTHING_SUSPICIOUS;
	false -> gui_loop(State)
      end;
    #wx{id=?menuID_PLT_SHOW_CONTENTS, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      show_doc_plt(State),
      gui_loop(State);
    #wx{id=?menuID_PLT_SEARCH_CONTENTS, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      case dialyzer_plt:get_specs(DocPlt) of
	"" -> error_sms(State, "No analysis has been made yet!\n");
	_ -> search_doc_plt(State)
      end,
      gui_loop(State);
    #wx{id=?menuID_OPTIONS_INCLUDE_DIR, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      NewOptions = include_dialog(State),
      NewState = State#gui_state{options = NewOptions},
      gui_loop(NewState);
    #wx{id=?menuID_OPTIONS_MACRO, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      NewOptions = macro_dialog(State),
      NewState = State#gui_state{options = NewOptions},
      gui_loop(NewState);
    #wx{id=?menuID_HELP_MANUAL, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      handle_help(State, "Dialyzer Manual", "manual.txt"),
      gui_loop(State);
    #wx{id=?menuID_HELP_WARNING_OPTIONS, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      handle_help(State, "Dialyzer Warnings", "warnings.txt"),
      gui_loop(State);
    #wx{id=?menuID_HELP_ABOUT, obj=Frame, 
	event=#wxCommand{type=command_menu_selected}} ->
      Message = "	       This is DIALYZER version "  ++ ?VSN ++  " \n"++
	"DIALYZER is a DIscrepancy AnaLYZer for ERlang programs.\n\n"++
	"     Copyright (C) Tobias Lindahl <tobiasl@it.uu.se>\n"++
	"                   Kostis Sagonas <kostis@it.uu.se>\n\n",
      output_sms(State, "About Dialyzer", Message, info),
      gui_loop(State);
    %% ------ Buttons ---------
    #wx{id=?Add_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      State1 = handle_add_files(State),
      gui_loop(State1);
    #wx{id=?AddDir_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      State1 = handle_add_dir(State),
      gui_loop(State1);
    #wx{id=?AddRec_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      State1 = handle_add_rec(State),
      gui_loop(State1);
    #wx{id=?Del_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      State1 = handle_file_delete(State),
      gui_loop(State1);
    #wx{id=?DelAll_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      State1 = handle_file_delete_all(State),
      gui_loop(State1);
    #wx{id=?ClearLog_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      wxTextCtrl:clear(State#gui_state.log),
      gui_loop(State);
    #wx{id=?ExplWarn_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      handle_explanation(State),
      gui_loop(State);
    #wx{id=?ClearWarn_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      wxListBox:clear(WarningsBox),
      NewState = State#gui_state{rawWarnings = []},
      gui_loop(NewState);
    #wx{id=?Run_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      NewState = start_analysis(State),
      gui_loop(NewState);
    #wx{id=?Stop_Button,
	event=#wxCommand{type=command_button_clicked}} ->
      BackendPid ! {self(), stop},
      config_gui_stop(State),
      update_editor(Log, "\n***** Analysis stopped ****\n"),
      gui_loop(State);
    %% ----- Analysis -----
    {BackendPid, ext_calls, ExtCalls} ->
      Msg = io_lib:format("The following functions are called "
			  "but type information about them is not available.\n"
			  "The analysis might get more precise by including "
			  "the modules containing these functions:\n\n\t~p\n", 
			  [ExtCalls]),
      free_editor(State,"Analysis Done",  Msg),
      gui_loop(State);
    {BackendPid, ext_types, ExtTypes} ->
      Map = fun({M,F,A}) -> io_lib:format("~p:~p/~p",[M,F,A]) end,
      ExtTypeString = string:join(lists:map(Map, ExtTypes), "\n"),
      Msg = io_lib:format("The following remote types are being used "
			  "but information about them is not available.\n"
			  "The analysis might get more precise by including "
			  "the modules containing these types and making sure "
			  "that they are exported:\n~s\n", [ExtTypeString]),
      free_editor(State, "Analysis done", Msg),
      gui_loop(State);
    {BackendPid, log, LogMsg} ->
      update_editor(Log, LogMsg),
      gui_loop(State);
    {BackendPid, warnings, Warns} ->
      SortedWarns = lists:keysort(2, Warns),  %% Sort on file/line
      NewState = add_warnings(State, SortedWarns),
      gui_loop(NewState);
    {BackendPid, cserver, CServer, Plt} ->
      Self = self(),
      Fun = 
	fun() -> 
	    dialyzer_explanation:expl_loop(Self, CServer, Plt)
	end,
      ExplanationPid = spawn_link(Fun),
      gui_loop(State#gui_state{expl_pid = ExplanationPid});
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
      gui_loop(State)
  end.

maybe_quit(#gui_state{frame = Frame} = State) ->
  case dialog(State, "Do you really want to quit?", ?DIALYZER_MESSAGE_TITLE) of
    true ->
      wxWindow:destroy(Frame),
      true;
    false ->
      false
  end.

%% ------------ Yes/No Question ------------
dialog(#gui_state{frame = Frame}, Message, Title) ->
  MessageWin = wxMessageDialog:new(Frame, Message, [{caption, Title},{style, ?wxYES_NO bor ?wxICON_QUESTION bor ?wxNO_DEFAULT}]),
  case wxDialog:showModal(MessageWin) of 
    ?wxID_YES ->
      true;
    ?wxID_NO ->
      false;
    ?wxID_CANCEL ->
      false
  end.
 
search_doc_plt(#gui_state{gui = Wx} = State) ->
  Dialog = wxFrame:new(Wx, ?SearchPltDialog, "Search the PLT",[{size,{400,100}},{style, ?wxSTAY_ON_TOP}]),
  Size = {size,{120,30}},
  ModLabel = wxStaticText:new(Dialog, ?ModLabel, "Module"),
  ModText = wxTextCtrl:new(Dialog, ?ModText,[Size]),
  FunLabel = wxStaticText:new(Dialog, ?FunLabel, "Function"),
  FunText = wxTextCtrl:new(Dialog, ?FunText,[Size]),
  ArLabel = wxStaticText:new(Dialog, ?ArLabel, "Arity"),
  ArText = wxTextCtrl:new(Dialog, ?ArText,[Size]),
  SearchButton = wxButton:new(Dialog, ?SearchButton, [{label, "Search"}]),
  wxButton:connect(SearchButton, command_button_clicked),
  Cancel = wxButton:new(Dialog, ?Search_Cancel, [{label, "Cancel"}]),
  wxButton:connect(Cancel, command_button_clicked),

  Layout = wxBoxSizer:new(?wxVERTICAL),
  Top = wxBoxSizer:new(?wxHORIZONTAL),
  ModLayout = wxBoxSizer:new(?wxVERTICAL),
  FunLayout = wxBoxSizer:new(?wxVERTICAL),
  ArLayout = wxBoxSizer:new(?wxVERTICAL),
  Buttons = wxBoxSizer:new(?wxHORIZONTAL),

  _ = wxSizer:add(ModLayout, ModLabel, ?BorderOpt),
  _ = wxSizer:add(ModLayout, ModText, ?BorderOpt),
  _ = wxSizer:add(FunLayout, FunLabel, ?BorderOpt),
  _ = wxSizer:add(FunLayout,FunText, ?BorderOpt),
  _ = wxSizer:add(ArLayout, ArLabel, ?BorderOpt),
  _ = wxSizer:add(ArLayout,ArText, ?BorderOpt),
  _ = wxSizer:add(Buttons, SearchButton, ?BorderOpt),
  _ = wxSizer:add(Buttons,Cancel, ?BorderOpt),

  _ = wxSizer:add(Top, ModLayout),
  _ = wxSizer:add(Top, FunLayout),
  _ = wxSizer:add(Top, ArLayout),
  _ = wxSizer:add(Layout, Top,[{flag, ?wxALIGN_CENTER}]),
  _ = wxSizer:add(Layout, Buttons,[{flag, ?wxALIGN_CENTER bor ?wxBOTTOM}]),
  wxFrame:connect(Dialog, close_window),
  wxWindow:setSizer(Dialog, Layout),
  wxFrame:show(Dialog),
  search_plt_loop(State, Dialog, ModText, FunText, ArText, SearchButton, Cancel).

search_plt_loop(State= #gui_state{doc_plt = DocPlt, frame = Frame}, Win, ModText, FunText, ArText, Search, Cancel) ->
  receive
    #wx{id = ?Search_Cancel,
	event = #wxCommand{type = command_button_clicked}} ->
      wxWindow:destroy(Win);
    #wx{id = ?SearchPltDialog, event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win);
    #wx{event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win),
      wxWindow:destroy(Frame);
    #wx{id = ?SearchButton,
	event = #wxCommand{type = command_button_clicked}} ->
      M = format_search(wxTextCtrl:getValue(ModText)),
      F = format_search(wxTextCtrl:getValue(FunText)),
      A = format_search(wxTextCtrl:getValue(ArText)),
      
      if 
	(M =:= '_') orelse (F =:= '_') orelse (A =:= '_') ->
	  error_sms(State, "Please give:\n Module (atom)\n Function (atom)\n Arity (integer)\n"),
	  search_plt_loop(State, Win, ModText, FunText, ArText, Search, Cancel);
	 true ->
	  case dialyzer_plt:get_specs(DocPlt, M, F, A) of
	    none ->
	      error_sms(State, "No such function"),
	      search_plt_loop(State, Win, ModText, FunText, ArText, Search, Cancel);
	    NonEmptyString ->
	      wxWindow:destroy(Win),
	      free_editor(State, "Content of PLT", NonEmptyString)
	  end
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

message(State, Message) ->
  output_sms(State, ?DIALYZER_MESSAGE_TITLE, Message, info).

error_sms(State, Message) ->
  output_sms(State, ?DIALYZER_ERROR_TITLE, Message, error).

output_sms(#gui_state{frame = Frame}, Title, Message, Type) ->
  Style = case Type of
	    error -> ?wxOK bor ?wxICON_ERROR;
	    info  -> ?wxOK bor ?wxICON_INFORMATION
	  end,
  Options = [{caption, Title}, {style, Style}],
  MessageWin = wxMessageDialog:new(Frame, Message, Options),
  wxWindow:setSizeHints(MessageWin, {350,100}),
  wxDialog:showModal(MessageWin),
  ok.

free_editor(#gui_state{gui = Wx, frame = Frame}, Title, Contents0) ->
  Contents = lists:flatten(Contents0),
  Tokens = string:tokens(Contents, "\n"),
  NofLines = length(Tokens),
  LongestLine = lists:max([length(X) || X <- Tokens]),
  Height0 = NofLines * 25 + 80,
  Height = if Height0 > 500 -> 500; true -> Height0 end,
  Width0 = LongestLine * 7 + 60,
  Width = if Width0 > 800 -> 800; true -> Width0 end,
  Size = {size,{Width, Height}},
  Win = wxFrame:new(Wx, ?Message, Title, [{size,{Width+4, Height+50}}]),  
  
  Editor = wxTextCtrl:new(Win, ?Message_Info,
			  [Size,
			   {style, ?wxTE_MULTILINE
			    bor ?wxTE_READONLY bor ?wxVSCROLL bor ?wxEXPAND}]),
  wxTextCtrl:appendText(Editor, Contents),
  wxFrame:connect(Win, close_window),
  Ok = wxButton:new(Win, ?Message_Ok, [{label, "OK"}]),
  wxButton:connect(Ok, command_button_clicked),
  Layout = wxBoxSizer:new(?wxVERTICAL),
  
  _ = wxSizer:add(Layout, Editor, ?BorderOpt),
  Flag = ?wxALIGN_CENTER bor ?wxBOTTOM bor ?wxALL,
  _ = wxSizer:add(Layout, Ok, [{flag, Flag}, ?Border]),
  wxWindow:setSizer(Win, Layout),
  wxWindow:show(Win),
  show_info_loop(Frame, Win).

show_info_loop(Frame, Win) ->
  receive
    #wx{id = ?Message_Ok, event = #wxCommand{type = command_button_clicked}} ->
      wxWindow:destroy(Win);
    #wx{id = ?Message, event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win);
    #wx{event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Frame)
  end.

handle_add_files(#gui_state{chosen_box = ChosenBox, file_box = FileBox,
			    files_to_analyze = FileList,
			    mode = Mode} = State) ->
  case wxFilePickerCtrl:getPath(FileBox) of
    "" ->
      State;
    File ->
      NewFile = ordsets:new(),
      NewFile1 = ordsets:add_element(File,NewFile),
      Ext = 
	case wxRadioBox:getSelection(Mode) of
	  0 -> ".beam";
	  1-> ".erl"
	end,
      State#gui_state{files_to_analyze = add_files(filter_mods(NewFile1, Ext), FileList, ChosenBox, Ext)}
  end.

handle_add_dir(#gui_state{chosen_box = ChosenBox, dir_entry = DirBox,
			  files_to_analyze = FileList, mode = Mode} = State) ->
  case wxDirPickerCtrl:getPath(DirBox) of
    "" ->
      State;
    Dir -> 
      NewDir = ordsets:new(),
      NewDir1 = ordsets:add_element(Dir,NewDir),
      Ext = case wxRadioBox:getSelection(Mode) of
	      0 -> ".beam";
	      1-> ".erl"
	    end,
      State#gui_state{files_to_analyze = add_files(filter_mods(NewDir1,Ext), FileList, ChosenBox, Ext)}
  end.
	    
handle_add_rec(#gui_state{chosen_box = ChosenBox, dir_entry = DirBox,
			  files_to_analyze = FileList, mode = Mode} = State) ->
  case wxDirPickerCtrl:getPath(DirBox) of
    "" ->
      State;
    Dir -> 
      NewDir = ordsets:new(),
      NewDir1 = ordsets:add_element(Dir,NewDir),
      TargetDirs = ordsets:union(NewDir1, all_subdirs(NewDir1)),
      Ext = case wxRadioBox:getSelection(Mode) of
	      0 -> ".beam";
	      1 -> ".erl"
	    end,
      State#gui_state{files_to_analyze = add_files(filter_mods(TargetDirs, Ext), FileList, ChosenBox, Ext)}
  end.

handle_file_delete(#gui_state{chosen_box = ChosenBox,
			      files_to_analyze = FileList} = State) ->
  {_, List} = wxListBox:getSelections(ChosenBox),
  Set = ordsets:from_list([wxControlWithItems:getString(ChosenBox, X) || X <- List]),
  FileList1 = ordsets:subtract(FileList,Set),
  lists:foreach(fun (X) -> wxListBox:delete(ChosenBox, X) end, List),
  State#gui_state{files_to_analyze = FileList1}.

handle_file_delete_all(#gui_state{chosen_box = ChosenBox} = State) ->
  wxListBox:clear(ChosenBox),
  State#gui_state{files_to_analyze = ordsets:new()}.
	    
add_files(File, FileList, ChosenBox, Ext) ->
  Set = filter_mods(FileList, Ext),
  Files = ordsets:union(File, Set),
  Files1 = ordsets:to_list(Files),
  wxListBox:set(ChosenBox, Files1),
  Files.

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
    case wxRadioBox:getSelection(Mode) of
      0 -> byte_code;
      1 -> src_code
    end,
  InitPlt =
    case wxMenu:isChecked(Menu#menu.plt, ?menuID_PLT_INIT_EMPTY) of
      true -> dialyzer_plt:new();
      false -> InitPlt0
    end,
  #analysis{defines = Options#options.defines,
	    include_dirs = Options#options.include_dirs,
	    plt = InitPlt,
	    start_from = StartFrom,
	    solvers = Options#options.solvers}.

get_anal_files(#gui_state{files_to_analyze = Files}, StartFrom) ->
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

find_legal_warnings(#gui_state{menu = #menu{warnings = MenuWarnings},
			       wantedWarnings = Warnings }) ->
  ordsets:from_list([Tag || {Tag, MenuItem} <- Warnings, 
			    wxMenu:isChecked(MenuWarnings, MenuItem)]).

update_editor(Editor, Msg) ->
  wxTextCtrl:appendText(Editor,Msg).

config_gui_stop(State) ->  
  wxWindow:disable(State#gui_state.stop),
  wxWindow:enable(State#gui_state.run),
  wxWindow:enable(State#gui_state.del_file),
  wxWindow:enable(State#gui_state.clear_chosen),
  wxWindow:enable(State#gui_state.add),
  wxWindow:enable(State#gui_state.add_dir),
  wxWindow:enable(State#gui_state.add_rec),
  wxWindow:enable(State#gui_state.clear_warn),
  wxWindow:enable(State#gui_state.clear_log),
  Menu = State#gui_state.menu,
  wxMenu:enable(Menu#menu.file,?menuID_FILE_SAVE_WARNINGS,true),
  wxMenu:enable(Menu#menu.file,?menuID_FILE_SAVE_LOG,true),
  wxMenu:enable(Menu#menu.options,?menuID_OPTIONS_MACRO,true),
  wxMenu:enable(Menu#menu.options,?menuID_OPTIONS_INCLUDE_DIR,true),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_INIT_EMPTY,true),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_SHOW_CONTENTS,true),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_SEARCH_CONTENTS,true),
  wxRadioBox:enable(State#gui_state.mode).

config_gui_start(State) ->
  wxWindow:enable(State#gui_state.stop),
  wxWindow:disable(State#gui_state.run),
  wxWindow:disable(State#gui_state.del_file),
  wxWindow:disable(State#gui_state.clear_chosen),
  wxWindow:disable(State#gui_state.add),
  wxWindow:disable(State#gui_state.add_dir),
  wxWindow:disable(State#gui_state.add_rec),
  wxWindow:disable(State#gui_state.clear_warn),
  wxWindow:disable(State#gui_state.clear_log),
  Menu = State#gui_state.menu,
  wxMenu:enable(Menu#menu.file,?menuID_FILE_SAVE_WARNINGS, false),
  wxMenu:enable(Menu#menu.file,?menuID_FILE_SAVE_LOG, false),
  wxMenu:enable(Menu#menu.options,?menuID_OPTIONS_MACRO, false),
  wxMenu:enable(Menu#menu.options,?menuID_OPTIONS_INCLUDE_DIR, false),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_INIT_EMPTY, false),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_SHOW_CONTENTS, false),
  wxMenu:enable(Menu#menu.plt,?menuID_PLT_SEARCH_CONTENTS, false),
  wxRadioBox:disable(State#gui_state.mode).

save_file(#gui_state{frame = Frame, warnings_box = WBox, log = Log} = State, Type) ->
  {Message, Box} = case Type of
		     warnings -> {"Save Warnings", WBox};
		     log -> {"Save Log", Log}
		   end,
  case wxTextCtrl:getValue(Box) of
    "" -> error_sms(State,"There is nothing to save...\n");
    _ ->
      DefaultPath = code:root_dir(),
      FileDialog = wxFileDialog:new(Frame,
				    [{defaultDir, DefaultPath},
				     {message, Message},
				     {style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
      case wxFileDialog:showModal(FileDialog) of
	?wxID_OK ->
	  Path = wxFileDialog:getPath(FileDialog),
	  case wxTextCtrl:saveFile(Box,[{file,Path}]) of
	    true -> ok;
	    false -> error_sms(State, "Could not write to file:\n" ++ Path)
	  end;
	?wxID_CANCEL -> wxWindow:destroy(FileDialog);
	_ -> error_sms(State, "Could not write to file:\n")
      end
  end.
					
include_dialog(#gui_state{gui = Wx, frame = Frame, options = Options}) ->
  Size = {size,{300,480}},
  Dialog = wxFrame:new(Wx, ?IncludeDir, "Include Directories",[Size]),
  DirLabel = wxStaticText:new(Dialog, ?InclLabel, "Directory: "),
  DefaultPath = code:root_dir(),
  DirPicker = wxDirPickerCtrl:new(Dialog, ?InclPicker,
				   [{path, DefaultPath},
				    {message, "Choose Directory to Include"},
				    {style,?wxDIRP_DIR_MUST_EXIST bor ?wxDIRP_USE_TEXTCTRL}]), 
  Box = wxListBox:new(Dialog, ?InclBox,
			[{size, {200,300}},
			 {style, ?wxLB_EXTENDED bor ?wxLB_HSCROLL
			  bor ?wxLB_NEEDED_SB}]), 
  AddButton = wxButton:new(Dialog, ?InclAdd, [{label, "Add"}]),
  DeleteButton = wxButton:new(Dialog, ?InclDel, [{label, "Delete"}]),
  DeleteAllButton = wxButton:new(Dialog, ?InclDelAll, [{label, "Delete All"}]),
  Ok = wxButton:new(Dialog, ?InclOk, [{label, "OK"}]),
  Cancel = wxButton:new(Dialog, ?InclCancel, [{label, "Cancel"}]),
  wxButton:connect(AddButton, command_button_clicked),
  wxButton:connect(DeleteButton, command_button_clicked),
  wxButton:connect(DeleteAllButton, command_button_clicked),
  wxButton:connect(Ok, command_button_clicked),
  wxButton:connect(Cancel, command_button_clicked),
  Dirs = [io_lib:format("~s", [X]) || X <- Options#options.include_dirs],
  wxListBox:set(Box, Dirs),
  Layout = wxBoxSizer:new(?wxVERTICAL),
  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  Buttons1 = wxBoxSizer:new(?wxHORIZONTAL),

  _ = wxSizer:add(Layout, DirLabel, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  _ = wxSizer:add(Layout, DirPicker, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  _ = wxSizer:add(Layout,AddButton, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, ?Border]),
  _ = wxSizer:add(Layout,Box, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, ?Border]),
  _ = wxSizer:add(Buttons, DeleteButton, ?BorderOpt),
  _ = wxSizer:add(Buttons, DeleteAllButton, ?BorderOpt),
  _ = wxSizer:add(Layout,Buttons, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  _ = wxSizer:add(Buttons1, Ok, ?BorderOpt),
  _ = wxSizer:add(Buttons1,Cancel, ?BorderOpt),
  _ = wxSizer:add(Layout,Buttons1,[{flag, ?wxALIGN_RIGHT bor ?wxBOTTOM}]),

  wxFrame:connect(Dialog, close_window),
  wxWindow:setSizer(Dialog, Layout),
  wxFrame:show(Dialog),
  include_loop(Options, Dialog, Box, DirPicker, Frame).

include_loop(Options, Win, Box, DirPicker, Frame) ->
  receive
    #wx{id = ?InclCancel,
	event = #wxCommand{type = command_button_clicked}} ->
      wxWindow:destroy(Win), 
      Options;
    #wx{id = ?IncludeDir, event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win),
      Options;
    #wx{event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win),
      wxWindow:destroy(Frame);
    #wx{id = ?InclOk,
	event = #wxCommand{type = command_button_clicked}} ->
     wxWindow:destroy(Win),
     Options;
    #wx{id = ?InclAdd,
	event = #wxCommand{type = command_button_clicked}} ->
      Dirs = Options#options.include_dirs,
       NewDirs =
	case wxDirPickerCtrl:getPath(DirPicker) of
	  "" -> Dirs;
	  Add -> [Add|Dirs]
	end,
      NewOptions = Options#options{include_dirs = NewDirs},
      wxListBox:set(Box, NewDirs),
      include_loop(NewOptions, Win, Box, DirPicker, Frame);
     #wx{id = ?InclDel,
	event = #wxCommand{type = command_button_clicked}} ->
      NewOptions =
	case wxListBox:getSelections(Box) of
	  {0,_} -> Options;
	  {_,List} ->
	    DelList = [wxControlWithItems:getString(Box,X) || X <- List],
	    NewDirs = Options#options.include_dirs -- DelList,
	    lists:foreach(fun (X) -> wxListBox:delete(Box, X) end, List),
	    Options#options{include_dirs = NewDirs}
	end,
      include_loop(NewOptions, Win, Box, DirPicker, Frame);
    #wx{id = ?InclDelAll,
	event = #wxCommand{type = command_button_clicked}} ->
      wxListBox:clear(Box),
      NewOptions = Options#options{include_dirs = []},
      include_loop(NewOptions, Win, Box, DirPicker, Frame)
  end. 
  
macro_dialog(#gui_state{gui = Wx, frame = Frame, options = Options}) ->
  Size = {size,{300,480}},
  Size1 = {size,{120,30}},
  Dialog = wxFrame:new(Wx, ?MacroDir, "Macro Definitions",[Size]),
  MacroLabel = wxStaticText:new(Dialog, ?MacroLabel, "Macro"),
  TermLabel = wxStaticText:new(Dialog, ?TermLabel, "Term"),
  MacroText = wxTextCtrl:new(Dialog, ?MacroText, [Size1]),
  TermText = wxTextCtrl:new(Dialog, ?TermText, [Size1]),
  Box = wxListBox:new(Dialog, ?MacroBox,
			[{size, {250,300}},
			 {style, ?wxLB_EXTENDED bor ?wxLB_HSCROLL
			  bor ?wxLB_NEEDED_SB}]),

  AddButton = wxButton:new(Dialog, ?MacroAdd, [{label, "Add"}]),
  DeleteButton = wxButton:new(Dialog, ?MacroDel, [{label, "Delete"}]),
  DeleteAllButton = wxButton:new(Dialog, ?MacroDelAll, [{label, "Delete All"}]),
  Ok = wxButton:new(Dialog, ?MacroOk, [{label, "OK"}]),
  Cancel = wxButton:new(Dialog, ?MacroCancel, [{label, "Cancel"}]),
  wxButton:connect(AddButton, command_button_clicked),
  wxButton:connect(DeleteButton, command_button_clicked),
  wxButton:connect(DeleteAllButton, command_button_clicked),
  wxButton:connect(Ok, command_button_clicked),
  wxButton:connect(Cancel, command_button_clicked),

  Macros = [io_lib:format("~p = ~p", [X, Y]) 
	    || {X,Y} <- Options#options.defines],
  
  wxListBox:set(Box, Macros),
  Layout = wxBoxSizer:new(?wxVERTICAL),
  Item = wxBoxSizer:new(?wxHORIZONTAL),
  MacroItem = wxBoxSizer:new(?wxVERTICAL),
  TermItem = wxBoxSizer:new(?wxVERTICAL),
  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  Buttons1 = wxBoxSizer:new(?wxHORIZONTAL),

  _ = wxSizer:add(MacroItem, MacroLabel, ?BorderOpt),
  _ = wxSizer:add(MacroItem, MacroText, ?BorderOpt),
  _ = wxSizer:add(TermItem, TermLabel, ?BorderOpt),
  _ = wxSizer:add(TermItem, TermText, ?BorderOpt),
  _ = wxSizer:add(Item, MacroItem),
  _ = wxSizer:add(Item, TermItem),
  _ = wxSizer:add(Layout, Item, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  _ = wxSizer:add(Layout, AddButton, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, ?Border]),
  _ = wxSizer:add(Layout, Box, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, ?Border]),
  _ = wxSizer:add(Buttons, DeleteButton, ?BorderOpt),
  _ = wxSizer:add(Buttons, DeleteAllButton, ?BorderOpt),
  _ = wxSizer:add(Layout, Buttons, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  _ = wxSizer:add(Buttons1, Ok, ?BorderOpt),
  _ = wxSizer:add(Buttons1, Cancel, ?BorderOpt),
  _ = wxSizer:add(Layout, Buttons1, [{flag, ?wxALIGN_RIGHT bor ?wxBOTTOM}]),

  wxFrame:connect(Dialog, close_window),
  wxWindow:setSizer(Dialog, Layout),
  wxFrame:show(Dialog),
  macro_loop(Options, Dialog, Box, MacroText, TermText, Frame).

macro_loop(Options, Win, Box, MacroText, TermText, Frame) ->
  receive
    #wx{id = ?MacroCancel,
	event = #wxCommand{type = command_button_clicked}} ->
      wxWindow:destroy(Win), 
      Options;
    #wx{id = ?MacroDir, event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win),
      Options;
    #wx{event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Win),
      wxWindow:destroy(Frame);
    #wx{id = ?MacroOk,
	event = #wxCommand{type = command_button_clicked}} ->
     wxWindow:destroy(Win),
     Options;
    #wx{id = ?MacroAdd,
	event = #wxCommand{type = command_button_clicked}} ->
      Defines = Options#options.defines,
       NewDefines = 
	case wxTextCtrl:getValue(MacroText) of
	  "" -> Defines;
	  Macro ->
	    case wxTextCtrl:getValue(TermText) of
	      "" -> 
		orddict:store(list_to_atom(Macro), true, Defines);
	      String ->
		orddict:store(list_to_atom(Macro), String, Defines)
	    end
	end,
      NewOptions = Options#options{defines = NewDefines},
      NewEntries = [io_lib:format("~p = ~p", [X, Y]) || {X, Y} <- NewDefines],
      wxListBox:set(Box, NewEntries),
      macro_loop(NewOptions, Win, Box, MacroText, TermText, Frame);
     #wx{id = ?MacroDel,
	event = #wxCommand{type = command_button_clicked}} ->
      NewOptions =
	case wxListBox:getSelections(Box) of
	  {0, _} -> Options;
	  {_, List} ->
	    Fun = 
	      fun(X) ->
		  Val = wxControlWithItems:getString(Box,X),
		  [MacroName|_] = re:split(Val, " ", [{return, list}]),
		  list_to_atom(MacroName)
	      end,
	    Delete = [Fun(X) || X <- List],
	    lists:foreach(fun (X) -> wxListBox:delete(Box, X) end, List),
	    Defines = Options#options.defines,
	    NewDefines = lists:foldl(fun(X, Acc) ->
					 orddict:erase(X, Acc)
				     end,
				     Defines, Delete),
	    Options#options{defines = NewDefines}
	end,
      macro_loop(NewOptions, Win, Box, MacroText, TermText, Frame);
    #wx{id = ?MacroDelAll,
	event = #wxCommand{type = command_button_clicked}} ->
      wxListBox:clear(Box),
      NewOptions = Options#options{defines = []},
      macro_loop(NewOptions, Win, Box,  MacroText, TermText, Frame)
  end.   

handle_help(State, Title, Txt) ->
  FileName = filename:join([code:lib_dir(dialyzer), "doc", Txt]),
  case file:open(FileName, [read]) of
    {error, Reason} ->
      error_sms(State, 
		io_lib:format("Could not find doc/~s file!\n\n ~p", 
			      [Txt, Reason]));
    {ok, _Handle} ->
      case file:read_file(FileName) of
	{error, Reason} ->
	  error_sms(State, 
		    io_lib:format("Could not read doc/~s file!\n\n ~p", 
				  [Txt, Reason]));
	{ok, Binary} ->
	  Contents = binary_to_list(Binary),
	  free_editor(State, Title, Contents)
      end
  end.

add_warnings(#gui_state{warnings_box = WarnBox,
			rawWarnings = RawWarns} = State, Warnings) ->
  NewRawWarns = RawWarns ++ Warnings,
  WarnList = [dialyzer:format_warning(W) -- "\n" || W <- NewRawWarns],
  wxListBox:set(WarnBox, WarnList),
  State#gui_state{rawWarnings = NewRawWarns}.
  
handle_explanation(#gui_state{rawWarnings = RawWarns,
			      warnings_box = WarnBox,
			      expl_pid = ExplPid} = State) ->
  case wxListBox:isEmpty(WarnBox) of
    true ->
      error_sms(State, "\nThere are no warnings.\nRun the dialyzer first.");
    false ->
      case wxListBox:getSelections(WarnBox)of
	{0, []} ->
	  error_sms(State,"\nYou must choose a warning to be explained\n");
	{_, [WarnNumber]} ->
	  Warn = lists:nth(WarnNumber+1,RawWarns),
	  Self = self(),
	  ExplPid ! {Self, warning, Warn},
	  explanation_loop(State)
      end
  end.

explanation_loop(#gui_state{expl_pid = ExplPid} = State) ->
  receive
    {ExplPid, explanation, Explanation} ->
      show_explanation(State, Explanation);
    _ -> io:format("Unknown message\n"),
	 explanation_loop(State)
  end.

show_explanation(#gui_state{gui = Wx} = State, Explanation) ->
  case Explanation of
    none ->
      output_sms(State, ?DIALYZER_MESSAGE_TITLE, 
		 "There is not any explanation for this error!\n", info);
    Expl ->
      ExplString = format_explanation(Expl),
      Size = {size,{700, 300}},
      Win = wxFrame:new(Wx, ?ExplWin, "Dialyzer Explanation", [{size,{740, 350}}]),  
      
      Editor = wxTextCtrl:new(Win, ?ExplText,
			      [Size,
			       {style, ?wxTE_MULTILINE
				bor ?wxTE_READONLY bor ?wxVSCROLL bor ?wxEXPAND}]),
      wxTextCtrl:appendText(Editor, ExplString),
      wxFrame:connect(Win, close_window),
      ExplButton = wxButton:new(Win, ?ExplButton, [{label, "Further Explain"}]),
      wxButton:connect(ExplButton, command_button_clicked),
      Ok = wxButton:new(Win, ?ExplOk, [{label, "OK"}]),
      wxButton:connect(Ok, command_button_clicked),
      Layout = wxBoxSizer:new(?wxVERTICAL),
      Buttons = wxBoxSizer:new(?wxHORIZONTAL),
      _ = wxSizer:add(Buttons, ExplButton, ?BorderOpt),
      _ = wxSizer:add(Buttons, Ok, ?BorderOpt),
      _ = wxSizer:add(Layout, Editor, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, ?Border]),
      _ = wxSizer:add(Layout, Buttons,[{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
      wxWindow:setSizer(Win, Layout),
      wxWindow:show(Win),
      NewState = State#gui_state{explanation_box = Editor},
      show_explanation_loop(NewState, Win, Explanation)
  end.
  
show_explanation_loop(#gui_state{frame = Frame, expl_pid = ExplPid} = State, Win, Explanation) ->
  receive
    {ExplPid, none, _} -> 
      output_sms(State, ?DIALYZER_MESSAGE_TITLE, 
		       "There is not any other explanation for this error!\n", info),
      show_explanation_loop(State, Win,  Explanation);
    {ExplPid, further, NewExplanation} ->
      update_explanation(State, NewExplanation),
      show_explanation_loop(State, Win,  NewExplanation);
    #wx{id = ?ExplButton, event = #wxCommand{type = command_button_clicked}} ->
      ExplPid ! {self(), further, Explanation},
      show_explanation_loop(State, Win, Explanation);
    #wx{id = ?ExplOk, event = #wxCommand{type = command_button_clicked}} ->
      wxWindow:destroy(Win);
    #wx{id = ?ExplWin, event = #wxClose{type = close_window}} ->
       wxWindow:destroy(Win);
    #wx{event = #wxClose{type = close_window}} ->
      wxWindow:destroy(Frame)
  end.

update_explanation(#gui_state{explanation_box = Box}, Explanation) ->
  ExplString = format_explanation(Explanation),
  wxTextCtrl:appendText(Box, "\n --------------------------- \n"),
  wxTextCtrl:appendText(Box, ExplString).

format_explanation({function_return, {M, F, A}, NewList}) ->
  io_lib:format("The function ~p: ~p/~p returns ~p\n",
		[M, F, A, erl_types:t_to_string(NewList)]);
format_explanation(Explanation) ->
  io_lib:format("~p\n", [Explanation]).
