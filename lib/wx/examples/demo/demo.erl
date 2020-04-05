%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2017. All Rights Reserved.
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

%% This is example of the widgets and usage of wxErlang
%% Hopefully it will contain all implemented widgets, it's event handling
%% and some tutorials of how to use sizers and other stuff.

-module(demo).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/0, start/1, start_link/0, start_link/1, format/3, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).


-record(state, {win, demo, example, selector, log, code}).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

start() ->
    start([]).

start(Debug) ->
    wx_object:start(?MODULE, Debug, []).

start_link() ->
    start_link([]).

start_link(Debug) ->
    wx_object:start_link(?MODULE, Debug, []).

format(#state{log=Log}, Str, Args) ->
    wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
    ok;
format(Config,Str,Args) ->
    Log = proplists:get_value(log, Config),
    wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
    ok.


-define(DEBUG_NONE, 101).
-define(DEBUG_VERBOSE, 102).
-define(DEBUG_TRACE, 103).
-define(DEBUG_DRIVER, 104).
    
init(Options) ->
    wx:new(Options),
    process_flag(trap_exit, true),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang widgets", [{size,{1000,500}}]),
    MB = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenu:append(File, ?wxID_PRINT, "&Print code"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    Debug    = wxMenu:new([]),
    wxMenu:appendRadioItem(Debug, ?DEBUG_NONE, "None"), 
    wxMenu:appendRadioItem(Debug, ?DEBUG_VERBOSE, "Verbose"), 
    wxMenu:appendRadioItem(Debug, ?DEBUG_TRACE, "Trace"), 
    wxMenu:appendRadioItem(Debug, ?DEBUG_DRIVER, "Driver"), 
    Help    = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"), 
    wxMenu:append(Help, ?wxID_ABOUT, "About"), 
    wxMenuBar:append(MB, File, "&File"),
    wxMenuBar:append(MB, Debug, "&Debug"),
    wxMenuBar:append(MB, Help, "&Help"),
    wxFrame:setMenuBar(Frame,MB),

    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window),

    _SB = wxFrame:createStatusBar(Frame,[]),

    %% Setup on toplevel because stc seems to steal this on linux
    wxFrame:dragAcceptFiles(Frame, true),
    wxFrame:connect(Frame, drop_files),

    %%   T        Uppersplitter
    %%   O        Left   |    Right
    %%   P  Widgets|Code |    Demo
    %%   S  -------------------------------
    %%   P          Log Window
    TopSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),    
    UpperSplitter = wxSplitterWindow:new(TopSplitter, [{style, ?wxSP_NOBORDER}]),    
    LeftSplitter  = wxSplitterWindow:new(UpperSplitter, [{style, ?wxSP_NOBORDER}]),
    %% Setup so that sizers and initial sizes, resizes the windows correct
    wxSplitterWindow:setSashGravity(TopSplitter,   0.5),
    wxSplitterWindow:setSashGravity(UpperSplitter, 0.60),
    wxSplitterWindow:setSashGravity(LeftSplitter,  0.20),

    %% LeftSplitter:
    Example = fun(Beam) ->
		      "ex_" ++ F = filename:rootname(Beam),
		      F
	      end,
    Mods = [Example(F) || F <- filelib:wildcard("ex_*.beam")],

    CreateLB = fun(Parent) ->
		       wxListBox:new(Parent, ?wxID_ANY, 
				     [{style, ?wxLB_SINGLE},
				      {choices, Mods}])
	       end,
    {LBPanel, [LB],_} = create_subwindow(LeftSplitter, "Example", [CreateLB]),
    wxListBox:setSelection(LB, 0),
    wxListBox:connect(LB, command_listbox_selected),

    CreateCode = fun(Parent) -> 
			 code_area(Parent)
		 end,
    {CodePanel, [Code],_} = create_subwindow(LeftSplitter, "Code", [CreateCode]),

    wxSplitterWindow:splitVertically(LeftSplitter, LBPanel, CodePanel,
				     [{sashPosition,150}]),

    %% Demo: 
    {DemoPanel, [], DemoSz} = create_subwindow(UpperSplitter, "Demo", []),
    
    %% UpperSplitter:
    wxSplitterWindow:splitVertically(UpperSplitter, LeftSplitter, DemoPanel,
				     [{sashPosition,600}]),
    
    %% TopSplitter: 
    AddEvent = fun(Parent) ->
		       EventText = wxTextCtrl:new(Parent, 
						  ?wxID_ANY, 
						  [{style, ?wxTE_DONTWRAP bor 
						    ?wxTE_MULTILINE bor ?wxTE_READONLY}
						  ]),
		       wxTextCtrl:appendText(EventText, "Welcome\n"),
		       EventText
	       end,

    {EvPanel, [EvCtrl],_} = create_subwindow(TopSplitter, "Events", [AddEvent]),
    
    wxSplitterWindow:splitHorizontally(TopSplitter, UpperSplitter, EvPanel, 
				       [{sashPosition,-100}]),
    
    wxFrame:show(Frame),
    
    State = #state{win=Frame, demo={DemoPanel,DemoSz}, selector=LB, log=EvCtrl, code=Code},
    %% Load the first example:
    Ex = wxListBox:getStringSelection(LB),        
    process_flag(trap_exit, true),
    ExampleObj = load_example(Ex, State), 
    wxSizer:add(DemoSz, ExampleObj, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:layout(DemoSz),
    
    %% The windows should be set up now, Reset Gravity so we get what we want
    wxSplitterWindow:setSashGravity(TopSplitter,   1.0),
    wxSplitterWindow:setSashGravity(UpperSplitter, 0.0),
    wxSplitterWindow:setSashGravity(LeftSplitter,  0.0),
    wxSplitterWindow:setMinimumPaneSize(TopSplitter, 1),
    wxSplitterWindow:setMinimumPaneSize(UpperSplitter, 1),
    wxSplitterWindow:setMinimumPaneSize(LeftSplitter, 1),

    wxToolTip:enable(true),
    wxToolTip:setDelay(500),

    {Frame, State#state{example=ExampleObj}}.

create_subwindow(Parent, BoxLabel, Funs) ->
    Panel = wxPanel:new(Parent),
    Sz    = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
    wxPanel:setSizer(Panel, Sz),
    Ctrls = [Fun(Panel) || Fun <- Funs],
    [wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]) 
     || Ctrl <- Ctrls],
    {Panel, Ctrls, Sz}.

%%%%%%%%%%%%
%% Callbacks

%% Handled as in normal gen_server callbacks
handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State};
handle_info(Msg, State) ->
    format(State, "Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    format(State, "Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    format(State, "Got cast ~p~n",[Msg]),
    {noreply,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event=#wxCommand{type=command_listbox_selected, cmdString=Ex}}, 
	     State = #state{demo={_,DemoSz}, example=Example, code=Code}) ->
    case Ex of
	[] ->
	    {noreply, State};
	_  ->
	    wxSizer:detach(DemoSz, Example),
	    wx_object:call(Example, shutdown),
	    unload_code(Code),
	    NewExample = load_example(Ex, State),
	    wxSizer:add(DemoSz, NewExample, [{proportion,1}, {flag, ?wxEXPAND}]),
	    wxSizer:layout(DemoSz),
	    {noreply, State#state{example=NewExample}}
    end;
handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}},
	     State = #state{}) ->
    case Id of
	?wxID_PRINT ->
	    %% If you are going to printout mainly text it is easier if
	    %% you generate HTML code and use a wxHtmlEasyPrint
	    %% instead of using DCs

            %% Printpreview doesn't work in >2.9 without this
            wxIdleEvent:setMode(?wxIDLE_PROCESS_ALL),
	    Module = "ex_" ++ wxListBox:getStringSelection(State#state.selector) ++ ".erl",
	    HEP = wxHtmlEasyPrinting:new([{name, "Print"},
					  {parentWindow, State#state.win}]),
	    Html = demo_html_tagger:erl2htmltext(Module),
	    wxHtmlEasyPrinting:previewText(HEP, Html),
	    {noreply, State};
	?DEBUG_TRACE ->
	    wx:debug(trace),
	    {noreply, State};
	?DEBUG_DRIVER ->
	    wx:debug(driver),
	    {noreply, State};
	?DEBUG_VERBOSE ->
	    wx:debug(verbose),
	    {noreply, State};
	?DEBUG_NONE ->
	    wx:debug(none),
	    {noreply, State};
	?wxID_HELP ->
	    wx_misc:launchDefaultBrowser("http://www.erlang.org/doc/apps/wx/part_frame.html"),
	    {noreply, State};
	?wxID_ABOUT ->
	    WxWVer = io_lib:format("~p.~p.~p.~p",
				   [?wxMAJOR_VERSION, ?wxMINOR_VERSION,
				    ?wxRELEASE_NUMBER, ?wxSUBRELEASE_NUMBER]),
	    application:load(wx),
	    {ok, WxVsn} = application:get_key(wx,  vsn),
	    AboutString =
		"Demo of various widgets\n"
		"Authors: Olle & Dan\n\n" ++
		"Frontend: wx-" ++ WxVsn ++
		"\nBackend: wxWidgets-" ++ lists:flatten(WxWVer),

	    wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
							  [{style,
							    ?wxOK bor
							    ?wxICON_INFORMATION bor
							    ?wxSTAY_ON_TOP},
							   {caption, "About"}])),
	    {noreply, State};
	?wxID_EXIT ->
	    wx_object:call(State#state.example, shutdown),
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end;
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};
handle_event(Ev,State) ->
    format(State, "~p Got event ~p ~n",[?MODULE, Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, State = #state{win=Frame}) ->
    catch wx_object:call(State#state.example, shutdown),
    wxFrame:destroy(Frame),
    wx:destroy().

%%%%%%%%%%%%%%%%% Internals %%%%%%%%%%

load_example(Ex, #state{demo={DemoPanel,DemoSz}, log=EvCtrl, code=Code}) ->
    ModStr = "ex_" ++ Ex,
    Mod = list_to_atom(ModStr),
    ModFile = ModStr ++ ".erl",
    load_code(Code, file:read_file(ModFile)),
    find(Code),
    Mod:start([{parent, DemoPanel}, {demo_sz, DemoSz}, {log, EvCtrl}]).

-define(stc, wxStyledTextCtrl).

code_area(Parent) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    Ed = wxStyledTextCtrl:new(Parent),

    ?stc:styleClearAll(Ed),
    ?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    ?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    ?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
    ?stc:setMarginWidth(Ed, 0, LW),
    ?stc:setMarginWidth(Ed, 1, 0),

    ?stc:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    %%?stc:hideSelection(Ed, true),

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
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
	       %% Optional 2.9 stuff
	       {?wxSTC_ERLANG_COMMENT_FUNCTION, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_MODULE, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160,53,35}},
	       {?wxSTC_ERLANG_ATOM_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_MACRO_QUOTED, {40,144,170}},
	       {?wxSTC_ERLANG_RECORD_QUOTED, {40,100,20}},
	       {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_BIFS, {130,40,172}},
	       {?wxSTC_ERLANG_MODULES, {64,102,244}},
	       {?wxSTC_ERLANG_MODULES_ATT, {64,102,244}}
	      ],
    SetStyle = fun({Style, Color}) ->
		       ?stc:styleSetFont(Ed, Style, FixedFont),
		       ?stc:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    ?stc:setKeyWords(Ed, 0, keyWords()),
    
    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
    ?stc:setYCaretPolicy(Ed, Policy, 3),
    ?stc:setVisiblePolicy(Ed, Policy, 3),

    %% ?stc:connect(Ed, stc_doubleclick),
    %% ?stc:connect(Ed, std_do_drop, fun(Ev, Obj) -> io:format("Ev ~p ~p~n",[Ev,Obj]) end),
    ?stc:setReadOnly(Ed, true),
    Ed.

load_code(Ed, {ok, Code}) ->
    ?stc:setReadOnly(Ed, false),
    ?stc:setTextRaw(Ed, <<Code/binary, 0:8>>),
    Lines = ?stc:getLineCount(Ed),
    Sz = trunc(math:log10(Lines))+1,
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Sz, $9)),
    %%io:format("~p ~p ~p~n", [Lines, Sz, LW]),
    ?stc:setMarginWidth(Ed, 0, LW+5),
    ?stc:setReadOnly(Ed, true),
    Ed.

unload_code(Ed) ->
    ?stc:setReadOnly(Ed, false),
    ?stc:setTextRaw(Ed, <<0:8>>),
    ?stc:setReadOnly(Ed, true),
    Ed.

find(Ed) ->
    ?stc:searchAnchor(Ed),
    Res = ?stc:searchNext(Ed, ?wxSTC_FIND_REGEXP, "^init"),
    case Res >= 0 of
	true -> 
	    %% io:format("Found ~p ~n",[Res]),
	    ?stc:scrollToLine(Ed,?stc:lineFromPosition(Ed,Res) - 1),
	    true;
	false ->
	    io:format("Not Found ~s ~n",["^init"]),
	    false
    end.

keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).

