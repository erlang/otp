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
%%%-------------------------------------------------------------------
%%% File    : sudoku_gui.erl
%%% Author  :  <dgud@erix.ericsson.se>
%%% Description : The Gui and it's event loop
%%%
%%% Created :  9 Jan 2008 by  <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(sudoku_gui).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
	 terminate/2, code_change/3]).

-export([new/1]).

-behaviour(wx_object).

-include("sudoku.hrl").

-import(sudoku_game, [indx/1]).

%%%%%%%%%%  Graphic engine %%%%%%%%%%%%%%

-record(gs,{board,show_err=true,level=hard,game,frame,orig=[], print_d, print_psdd}).

new(Game) ->
    wx:new(),
    wx_object:start_link(?MODULE, [Game], []).

%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%

init([Game]) ->
    {Frame, Board} = wx:batch(fun() -> create_window() end),
    Game ! {gfx, self()},
    {Frame, init_printer(#gs{board=Board,game=Game,frame=Frame})}.

create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "Sudoku", []),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Opt     = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "&New Game"),
    wxMenu:append(File, ?OPEN, "&Open Game"),
    wxMenu:append(File, ?SAVE, "&Save Game"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?PRINT, "Print"),
    wxMenu:append(File, ?PRINT_PAGE_SETUP, "Page Setup"),
    wxMenu:append(File, ?PRINT_PRE, "Print Preview"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?QUIT, "&Quit Game"),

    wxMenu:append(Help, ?RULES, "Rules"),
    wxMenu:append(Help, ?ABOUT, "About"), 

    wxMenu:appendRadioItem(Opt, ?TRIVIAL, "Level: Trivial"),
    wxMenu:appendRadioItem(Opt, ?EASY, "Level: Easy"),
    LItem = wxMenu:appendRadioItem(Opt, ?NORMAL, "Level: Normal"),
    wxMenu:appendRadioItem(Opt, ?HARD, "Level: Hard"),
    wxMenu:appendRadioItem(Opt, ?HARDEST, "Level: Hardest"),
    wxMenu:appendSeparator(Opt),
    EItem = wxMenu:appendCheckItem(Opt, ?SHOW_ERROR, "Show errors"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Opt, "O&ptions"),
    wxMenuBar:append(MenuBar, Help, "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Top    = wxBoxSizer:new(?wxHORIZONTAL),

    Panel = wxPanel:new(Frame), 
    NewGame = wxButton:new(Panel, ?NEW, [{label,"New Game"}]),
    wxButton:connect(NewGame, command_button_clicked),
    Empty = wxButton:new(Panel, ?EMPTY, [{label,"Empty Board "}]),
    wxButton:connect(Empty, command_button_clicked),
    Clean = wxButton:new(Panel, ?CLEAR, [{label,"Clear"}]),
    wxButton:connect(Clean, command_button_clicked),
    Hint  = wxButton:new(Panel, ?HINT, [{label, "Hint"}]),
    wxButton:connect(Hint, command_button_clicked),

    wxSizer:addSpacer(Top,2),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:add(Top, NewGame, wxSizerFlags:left(SF)), 
    wxSizer:addSpacer(Top,3),
    wxSizer:add(Top, Empty,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Clean,   wxSizerFlags:center(SF)),
    wxSizer:addSpacer(Top,3),   
    wxSizer:add(Top, Hint,    wxSizerFlags:right(SF)),

    wxSizer:addSpacer(MainSz,5),
    wxSizer:add(MainSz, Top, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:addSpacer(MainSz,10),

    Board = sudoku_board:new(Panel),

    wxSizer:add(MainSz, Board, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxWindow:setSizer(Panel,MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),
    %% Check after append so it's initialized on all platforms
    wxMenuItem:check(LItem),
    wxMenuItem:check(EItem),
    {Frame, Board}.

status(Win, F, A) ->
    Str = lists:flatten(io_lib:format(F, A)),
    wxFrame:setStatusText(Win, Str).

%%%%%%%%%%%%%%%% Info i.e. messages %%%%%%%%%%%%%%%%%%%%%

handle_info(quit, S=#gs{game=G,frame=F}) ->
    wxWindow:close(F),
    wx_core:quit(), 
    G ! quit,
    {stop, shutdown, S};

handle_info({init, Init}, S = #gs{board=Board,frame=F}) ->
    sudoku_board:setup_board(Board, Init),
    status(F, "Given ~p  Left ~p", [length(Init), sudoku_board:left(Board)]),
    {noreply, S#gs{orig=[indx(Id)||{Id,_}<-Init]}};
handle_info({correct, ButtI}, S = #gs{board=Board, orig=Orig,frame=F}) ->
    sudoku_board:butt_correct(Board, ButtI, true),
    case sudoku_board:left(Board) of
	0 -> 
	    Str = "Congrats, now try a harder one",
	    MD = wxMessageDialog:new(F,Str,
				     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
				      {caption, "Complete"}]),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD),
	    status(F, "Given ~p  Left ~p", [length(Orig), 0]);
	L ->
	    status(F, "Given ~p  Left ~p", [length(Orig), L])
    end,
    {noreply, S};
handle_info({wrong, ButtI}, S = #gs{board=Board}) ->
    case S#gs.show_err of
	true ->
	    sudoku_board:butt_correct(Board, ButtI, false);
	false ->
	    ignore
    end,
    {noreply, S};
handle_info({set_val, ButtI, Val}, S = #gs{game=G,board=Board,orig=Orig}) ->
    case lists:member(indx(ButtI), Orig) of
	false -> set_val(ButtI, Val, Board, G);
	true ->  ignore
    end,
    {noreply, S};
handle_info({working, Done}, S = #gs{frame=F}) ->
    status(F, "Thinking: ~p%", [Done]),
    {noreply, S};
handle_info({busy, Mode},S) -> 
    case Mode of
	start -> wx_misc:beginBusyCursor();
	stop  -> wx_misc:endBusyCursor()
    end,
    {noreply, S}.

%%%%%%%%%%%%%%%%% GUI-Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=?HINT, event=#wxCommand{type=command_button_clicked}},
	     S = #gs{game=G}) ->
    G ! {solve,false},
    {noreply,S};

handle_event(#wx{event=#wxClose{}},
	     S = #gs{game=G,frame=F}) ->
    catch wxWindow:'Destroy'(F),
    G ! quit,
    {stop, shutdown, S};

handle_event(#wx{id=?QUIT, event=#wxCommand{type=command_menu_selected}},
	     S = #gs{game=G,frame=F}) ->
    wxWindow:close(F,[]),
    G ! quit,
    {stop, shutdown, S};

%% type=command_button_clicked,
handle_event(#wx{id=?NEW, event=#wxCommand{}},
	     S = #gs{game=G, board=Board}) ->
    G ! {op,?NEW,S#gs.level},
    sudoku_board:setup_board(Board,[]),
    {noreply, S#gs{orig=[]}};
handle_event(#wx{id=?EMPTY, event=#wxCommand{}},
	     S = #gs{game=G, board=Board}) ->
    G ! {op,?EMPTY},
    sudoku_board:setup_board(Board,[]),
    {noreply, S#gs{orig=[]}};
handle_event(#wx{id=?CLEAR, event=#wxCommand{}},
	     S = #gs{game=G,board=Board}) ->    
    Vals = sudoku_board:clear_board(Board),
    G ! {loaded, Vals},
    {noreply, S};
handle_event(#wx{id=ID, event=#wxCommand{}}, S) when ID > 125 ->
    New = dialog(ID, S),
    {noreply, New};
handle_event(Msg,S) ->
    io:format("~p: Unhandled event ~p~n",[?MODULE, Msg]),
    %%sudoku_board:event(Msg, Ids),
    {noreply, S}.

handle_call(What, _From, State) ->
    {stop, {call, What}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    normal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dialog(?SHOW_ERROR, S=#gs{show_err=Show}) ->
    S#gs{show_err = not Show};
dialog(ID, S) when ID >= 210, ID =< 240 ->
    Level = sudoku_game:level(ID-200),
    S#gs{level = Level};
dialog(?SAVE, S=#gs{frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor 
				   ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    {ok,Fd} = file:open(Path, [write]),
	    List = sudoku_board:get_board_data(Board),
	    io:format(Fd, "~w.~n", [List]),
	    file:close(Fd);
	_ ->
	    ignore
    end,
    wxDialog:destroy(FD),
    S;
dialog(?OPEN, S=#gs{game=Server, frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    case file:consult(Path) of
		{ok, [Game]} when is_list(Game) ->
		    Vals = sudoku_board:set_board_data(Board, Game),
		    Server ! {loaded, Vals};		    
		_ ->
		    ignore
	    end;
	_ ->
	    ignore
    end,
    wxFileDialog:destroy(FD),
    S;
dialog(?ABOUT,  S=#gs{frame=Frame}) ->
    Str = "I'm just testing WxWidgets.\n"
	"Testing various features and usages.\n/Dgud",
    MD = wxMessageDialog:new(Frame,Str,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About box"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    S;
dialog(?RULES, S) ->
    wx_misc:launchDefaultBrowser("http://www.sudoku.com"),
    S;

dialog(?PRINT_PAGE_SETUP, S = #gs{frame=Frame, print_psdd=PsDD0, print_d=PD0}) ->
    wxPageSetupDialogData:setPrintData(PsDD0, PD0),
    PSD = wxPageSetupDialog:new(Frame, [{data,PsDD0}]),
    wxPageSetupDialog:showModal(PSD),
	
    PSDD1 = wxPageSetupDialog:getPageSetupData(PSD),
    PD1 = wxPageSetupDialogData:getPrintData(PSDD1),
    %% Create new objects using copy constr.
    PD = wxPrintData:new(PD1),
    PsDD = wxPageSetupDialogData:new(PSDD1),
    wxPageSetupDialog:destroy(PSD),
    wxPageSetupDialogData:destroy(PsDD0),
    wxPrintData:destroy(PD0),
    S#gs{print_psdd=PsDD, print_d=PD};
dialog(?PRINT_PRE, S = #gs{frame=Frame, print_d=PD, board=Board}) ->
    {ok, BoardS} = sudoku_board:get_state(Board),
    PDD = wxPrintDialogData:new(PD),
    Printout1 = wxPrintout:new("Print 1", fun(This,Page) -> printout(This,Page,BoardS, S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Printout2 = wxPrintout:new("Print 2", fun(This,Page) -> printout(This,Page,BoardS, S) end,
			       [{getPageInfo, fun getPageInfo/1}]),
    Preview = wxPrintPreview:new(Printout1, [{printoutForPrinting,Printout2},{data,PDD}]),
    case wxPrintPreview:isOk(Preview) of
	true ->
	    PF = wxPreviewFrame:new(Preview, Frame, [{title, "Print Preview"}]),
	    wxPreviewFrame:centre(PF, [{dir, ?wxBOTH}]),
	    wxPreviewFrame:initialize(PF),
	    wxPreviewFrame:show(PF);
	false ->
	    io:format("Could not create preview window.\n"
		      "Perhaps your current printer is not set correctly?~n", []),
	    wxPrintPreview:destroy(Preview)
    end,
    S;
dialog(?PRINT, S = #gs{frame=Frame, print_d=PD, board=Board}) ->
    {ok, BoardS} = sudoku_board:get_state(Board),
    PDD = wxPrintDialogData:new(PD),
    Printer = wxPrinter:new([{data,PDD}]),
    Printout = wxPrintout:new("Print", fun(This,Page) -> printout(This,Page,BoardS,S) end,
			      [{getPageInfo, fun getPageInfo/1}]),

    case wxPrinter:print(Printer, Frame, Printout, [{prompt,true}]) of
	false -> 
	    case wxPrinter:getLastError() of
		?wxPRINTER_ERROR ->
		    io:format("There was a problem printing.\n"
			      "Perhaps your current printer is not set correctly?~n", []);
		_ ->
		    io:format("You canceled printing~n", [])
	    end,
	    wxPrinter:destroy(Printer),
	    S;
	true ->
	    PDD2 = wxPrinter:getPrintDialogData(Printer),
	    PD2  = wxPrintDialogData:getPrintData(PDD2),
	    %% Copy data PD2 will be deleted when Printer is destroyed
	    PD3 = wxPrintData:new(PD2),  
	    wxPrintData:destroy(PD),
	    wxPrinter:destroy(Printer),
	    S#gs{print_d = PD3}
    end;

dialog(Other, S) ->
    io:format("other ~p~n",[Other]),
    S.

init_printer(S) ->
    PD   = wxPrintData:new(),

    %% You could set an initial paper size here
    %%    g_printData->SetPaperId(wxPAPER_LETTER); // for Americans
    %%    g_printData->SetPaperId(wxPAPER_A4);    // for everyone else    

    PSDD = wxPageSetupDialogData:new(PD),
    wxPageSetupDialogData:setMarginTopLeft(PSDD,{15,15}),
    wxPageSetupDialogData:setMarginBottomRight(PSDD,{15,15}),

    S#gs{print_d=PD, print_psdd=PSDD}.

getPageInfo(_This) -> 
    {1,1,1,1}.

printout(This, _Page, Board, #gs{print_psdd=PsDD}) ->
    MX = MY = 500,  
    wxPrintout:fitThisSizeToPageMargins(This, {MX,MY}, PsDD),

    _DBG = {_X,_Y,W,H} = wxPrintout:getLogicalPageMarginsRect(This, PsDD),
    wxPrintout:offsetLogicalOrigin(This,(W-MX) div 2, (H-MY) div 2),
    DC = wxPrintout:getDC(This),
    sudoku_board:redraw(DC, {500,500}, Board),
    true.

set_val(Id, Val, Board, G) ->
    sudoku_board:set_butt(Board, Id,Val),
    G ! {validate, Id, Val},
    ok.



