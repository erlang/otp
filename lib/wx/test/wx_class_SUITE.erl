%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wx_class_SUITE.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description :
%%%
%%% Created : 13 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_class_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("wx_test_lib.hrl").

%% Initialization functions.
init_per_suite(Config) ->
    wx_test_lib:init_per_suite(Config).

end_per_suite(Config) ->
    wx_test_lib:end_per_suite(Config).

init_per_testcase(Func,Config) ->
    wx_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) ->
    wx_test_lib:end_per_testcase(Func,Config).

%% SUITE specification
suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [calendarCtrl, treeCtrl, notebook, staticBoxSizer,
     clipboard, helpFrame, htmlWindow, listCtrlSort, listCtrlVirtual,
     radioBox, systemSettings, taskBarIcon, toolbar, popup].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% The test cases

%% create and test a calendar, especially the DateAttr no-deletition.
calendarCtrl(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
calendarCtrl(Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(Wx, 1, "Calendar", [])),
    Panel = wxPanel:new(Frame),
    Sz = wxBoxSizer:new(?wxVERTICAL),

    {YMD={_,_,Day},_} = DateTime = calendar:now_to_datetime(erlang:now()),
    Cal = ?mt(wxCalendarCtrl, wxCalendarCtrl:new(Panel, ?wxID_ANY,
						 [{date,DateTime}
						 ])),
    wxSizer:add(Sz,Cal),

    DateAttr0 = ?mt(wxCalendarDateAttr, wxCalendarCtrl:getAttr(Cal,Day)),
    case wx:is_null(DateAttr0) of
	true ->
	    ?log("DateAttr is null~n",[]);
	false ->
	    ?log("DateAttr is useable~n",[]),
	    DateAttr = ?mt(wxCalendarDateAttr, wxCalendarDateAttr:new()),
	    wxCalendarDateAttr:setBackgroundColour(DateAttr, {0,243,0}),
	    wxCalendarCtrl:setAttr(Cal, Day, DateAttr),
	    DateAttr1 = ?mt(wxCalendarDateAttr, wxCalendarCtrl:getAttr(Cal,Day)),
	    io:format("DateAttr ~p~n",[DateAttr1]),
	    ?m({0,243,0,255}, wxCalendarDateAttr:getBackgroundColour(DateAttr1))
    end,

    ?m({YMD, _},wxCalendarCtrl:getDate(Cal)),

    wxCalendarCtrl:connect(Cal, calendar_weekday_clicked),
    wxCalendarCtrl:connect(Cal, calendar_day_changed),
    wxCalendarCtrl:connect(Cal, calendar_month_changed),
    wxCalendarCtrl:connect(Cal, calendar_year_changed),
    wxCalendarCtrl:connect(Cal, calendar_doubleclicked),
    wxCalendarCtrl:connect(Cal, calendar_sel_changed),

    wxWindow:setSizer(Panel,Sz),
    wxSizer:setSizeHints(Sz,Frame),
    wxWindow:show(Frame),

    wx_test_lib:wx_destroy(Frame,Config).


treeCtrl(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
treeCtrl(Config) ->
    Wx = wx:new(),

    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    Panel = wxPanel:new(Frame, []),
    Tree = ?mt(wxTreeCtrl,wxTreeCtrl:new(Panel, [{style , ?wxTR_HAS_BUTTONS}])),
    Root = wxTreeCtrl:addRoot(Tree, "Root", []),
    ?m(true, is_integer(Root)),
    Item1 = wxTreeCtrl:appendItem(Tree, Root, "Item1", []),
    ?m(true, is_integer(Item1)),
    ?m(ok,  wxTreeCtrl:setItemData(Tree, Item1, {data, item1})),
    Item2 = wxTreeCtrl:appendItem(Tree, Root, "Item2", []),
    ?m(ok,  wxTreeCtrl:setItemData(Tree, Item2, {data, item2})),
    Item3 = wxTreeCtrl:appendItem(Tree, Root, "Item3", []),
    ?m(ok, wxTreeCtrl:setItemData(Tree, Item3, {data, item3})),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Tree, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxWindow:setSizerAndFit(Panel, Sizer),
    wxFrame:show(Frame),

    ok = wxTreeCtrl:expand(Tree, Root),
    ?m([], wxTreeCtrl:getItemData(Tree, Root)),
    ?m({data,item1}, wxTreeCtrl:getItemData(Tree, Item1)),
    ?m({data,item2}, wxTreeCtrl:getItemData(Tree, Item2)),
    ?m({data,item3}, wxTreeCtrl:getItemData(Tree, Item3)),

    {true, {X0,Y0,W0,H0}} = ?m({_,_},wxTreeCtrl:getBoundingRect(Tree, Item1, [{textOnly, true}])),
    ?m({true, {_,Y1,_,_}} when Y1 > Y0, wxTreeCtrl:getBoundingRect(Tree, Item2)),
    ?m({Item1, _}, wxTreeCtrl:hitTest(Tree, {X0+W0 div 2, Y0+H0 div 2})),
    ?m(true, wxTreeCtrl:isTreeItemIdOk(Item1)),
    ?m({0, _}, wxTreeCtrl:hitTest(Tree, {X0+W0+W0, Y0+H0+4*H0})),
    ?m(false, wxTreeCtrl:isTreeItemIdOk(0)),

    wxFrame:connect(Tree, command_tree_item_expanded),
    wxFrame:connect(Tree, command_tree_item_collapsed),
    wxFrame:connect(Frame, close_window),

    wxTreeCtrl:editLabel(Tree, Root),


    wx_test_lib:wx_destroy(Frame,Config).

notebook(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
notebook(Config) ->
    Wx = wx:new(),

    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    Panel = wxPanel:new(Frame, []),
    Book = wxNotebook:new(Panel, ?wxID_ANY, []),

    Panel1 = wxPanel:new(Book, []),
    List1 = wxListBox:new(Panel1,
                          ?wxID_ANY,
                          [{choices, ["aaa1", "bb1", "c1"]},
                           {style,
                            ?wxLB_SORT bor
                            ?wxLB_NEEDED_SB bor
                            ?wxLB_EXTENDED}]),
    wxNotebook:addPage(Book, Panel1, "List1", []),
    Sizer1 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer1, List1, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(Panel1, Sizer1),

    Panel2 = wxPanel:new(Book, []),
    List2 = wxListBox:new(Panel2,
                          ?wxID_ANY,
                          [{choices, ["aaa2", "bb2", "c2"]},
                           {style,
                            ?wxLB_SORT bor
                            ?wxLB_NEEDED_SB bor
                            ?wxLB_EXTENDED}]),
    wxNotebook:addPage(Book, Panel2, "List2", []),
    Sizer2 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer2, List2, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(Panel2, Sizer2),

    Panel3 = wxPanel:new(Book, []),
    List3 = wxListBox:new(Panel3,
                          ?wxID_ANY,
                          [{choices, ["aaa3", "bb3", "c3"]},
                           {style,
                            ?wxLB_SORT bor
                            ?wxLB_NEEDED_SB bor
                            ?wxLB_EXTENDED}]),
    wxNotebook:addPage(Book, Panel3, "List3", []),
    Sizer3 = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer3, List3, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(Panel3, Sizer3),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Book, [{flag, ?wxEXPAND}, {proportion, 1}]),


    wxWindow:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:show(Frame),

    wxFrame:connect(Frame, close_window),

    wx_test_lib:wx_destroy(Frame,Config).

staticBoxSizer(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
staticBoxSizer(Config) ->
    Wx = wx:new(),
    wx:debug(2),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    Panel = wxPanel:new(Frame, []),
    InclSizer = ?mt(wxStaticBoxSizer,
		    wxStaticBoxSizer:new(?wxVERTICAL, Panel,
					 [{label, "Module inclusion policy"}])),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, InclSizer,
		[{border, 2}, {flag, ?wxALL bor ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizerAndFit(Panel, Sizer),

    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).


clipboard(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
clipboard(_Config) ->
    wx:new(),
    CB = ?mt(wxClipboard, wxClipboard:get()),
    wxClipboard:usePrimarySelection(CB),
    ?m(false, wx:is_null(CB)),
    case wxClipboard:open(CB) of
	true ->
	    case wxClipboard:isSupported(CB, ?wxDF_TEXT) of
		false ->
		    ?log("No text on the clipboard~n",[]);
		true ->
		    Text = ?mt(wxTextDataObject, wxTextDataObject:new()),
		    case wxClipboard:getData(CB,Text) of
			true ->
			    ?log("PASTE: ~s ~n", [wxTextDataObject:getText(Text)]);
			false ->
			    ?log("Couldn't access clipboard~n",[])
		    end,
		    wxTextDataObject:destroy(Text)
	    end,
	    wxClipboard:close(CB);
	false ->
	    ?log("Clipboard open failed~n",[])
    end,
    case wxClipboard:open(CB) of
	true ->
	    Paste = ?mt(wxTextDataObject, wxTextDataObject:new([{text,"From Erlang"}])),
	    case wxClipboard:addData(CB,Paste) of
		true ->
		    ?log("Put text on clipboard~n", []);
		false ->
		    ?log("Couldn't copy data to clipboard~n",[])
	    end,
	    wxClipboard:close(CB);
	false ->
	    ?log("Clipboard open failed~n",[])
    end,
    ?log("Flushing ~n",[]),
    wxClipboard:flush(CB),
    ?log("Stopping ~n",[]),
    ok.

helpFrame(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
helpFrame(Config) ->
    Wx = wx:new(),
    MFrame = wx:batch(fun() ->
			      MFrame = wxFrame:new(Wx, ?wxID_ANY, "Main Frame"),
			      wxPanel:new(MFrame, [{size, {600,400}}]),
			      wxWindow:show(MFrame),
			      MFrame
		      end),
    timer:sleep(9),

    {X0, Y0} = wxWindow:getScreenPosition(MFrame),
    {X, Y, W,H} = wxWindow:getScreenRect(MFrame),
    io:format("Pos0: ~p ~p ~p Pos: ~p:~p Size: ~p:~p ~n",
	      [X0,Y0, wxWindow:clientToScreen(MFrame, {0,0}), X,Y,W,H]),

    Pos = {X+5, Y+(H div 2)},
    Size = {W-10, (H div 2) - 5},

    Comp = wxFrame:new(MFrame, ?wxID_ANY, "Completion Window",
		       [{pos, Pos}, {size, Size},
			{style, ?wxFRAME_FLOAT_ON_PARENT}]),
    LB = wxListBox:new(Comp, 42, [{style, ?wxLB_SINGLE},
				  {size, Size}]),

    Items = ["Item " ++ integer_to_list(N) || N <- lists:seq(1, 10)],
    wxListBox:insertItems(LB,Items,0),

    wxWindow:show(Comp),
    wx_test_lib:wx_destroy(MFrame,Config).

htmlWindow(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
htmlWindow(Config) ->
    Wx = wx:new(),
    {MFrame,HPanel} =
	wx:batch(fun() ->
			 MFrame = wxFrame:new(Wx, ?wxID_ANY, "Main Frame"),
			 HPanel = wxHtmlWindow:new(MFrame, [{size, {600,400}}]),
			 wxWindow:show(MFrame),
			 {MFrame, HPanel}
		 end),
    timer:sleep(9),

    WxMod = code:which(wx),
    WxDir = filename:split(filename:dirname(WxMod)) -- ["ebin"],
    Html = filename:join(filename:join(WxDir),filename:join("doc", "html")),

    Index = filename:join(Html, "wx.html"),

    ?m(ok, wxHtmlWindow:connect(HPanel, command_html_link_clicked,
				[{callback,
				  fun(Ev,_) ->
					  io:format("Link clicked: ~p~n",[Ev])
				  end}])),

    case filelib:is_file(Index) of
	true ->
	    ?m(true, wxHtmlWindow:loadFile(HPanel, Index)),
	    ok;
	false ->
	    ok
    end,

    wx_test_lib:wx_destroy(MFrame,Config).


listCtrlSort(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
listCtrlSort(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),

    LC = wxListCtrl:new(Frame, [{style, ?wxLC_REPORT}]),

    %% must be done crashes in wxwidgets otherwise.
    wxListCtrl:insertColumn(LC, 0, "Column"),

    Add = fun(Int) ->
		  wxListCtrl:insertItem(LC, Int, "ABC " ++ integer_to_list(Int)),
		  %% ItemData Can only be integers currently
		  wxListCtrl:setItemData(LC, Int, abs(50-Int))
	  end,

    wx:foreach(Add, lists:seq(0,50)),
    wxWindow:show(Frame),

    timer:sleep(2000),

    Sort = fun() ->
		   wxListCtrl:sortItems(LC, fun(A, B) ->
						    %% io:format("S ~p ~p ~n",[A,B]),
						    if A =:= B ->  0;
						       A < B   -> -1;
						       true    ->  1
						    end
					    end)
	   end,

    Time = timer:tc(erlang, apply, [Sort,[]]),
    io:format("Sorted ~p ~n",[Time]),

    Item = wxListItem:new(),
    wxListItem:setMask(Item, ?wxLIST_MASK_TEXT),
    _List = wx:map(fun(Int) ->
			   wxListItem:setId(Item, Int),
			   ?m(true, wxListCtrl:getItem(LC, Item)),
			   io:format("~p: ~s~n",[Int, wxListItem:getText(Item)])
		   end, lists:seq(0,10)),
    wxListItem:destroy(Item),

    wx_test_lib:wx_destroy(Frame,Config).

listCtrlVirtual(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
listCtrlVirtual(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    IA = wxListItemAttr:new(),
    wxListItemAttr:setTextColour(IA, {190, 25, 25}),
    LC = wxListCtrl:new(Frame,
			[{style, ?wxLC_REPORT bor ?wxLC_VIRTUAL},
			 {onGetItemText, fun(_This, Item, 0) ->
						 "Row " ++ integer_to_list(Item);
					    (_, Item, 1) when Item rem 5 == 0 ->
						 "Column 2";
					    (_, _, _) -> ""
					 end},
			 {onGetItemAttr, fun(_This, Item) when Item rem 3 == 0 ->
						 IA;
					    (_This, _Item)  ->
						 wx:typeCast(wx:null(), wxListItemAttr)
					 end},
			 {onGetItemColumnImage, fun(_This, Item, 1) ->
							Item rem 4;
						   (_, _, _) ->
							-1
						end}
			]),

    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_COPY", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_MISSING_IMAGE", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_TICK_MARK", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_CROSS_MARK", [{size, {16,16}}])),
    wxListCtrl:assignImageList(LC, IL, ?wxIMAGE_LIST_SMALL),

    wxListCtrl:insertColumn(LC, 0, "Column 1"),
    wxListCtrl:insertColumn(LC, 1, "Column 2"),
    wxListCtrl:setColumnWidth(LC, 0, 200),
    wxListCtrl:setColumnWidth(LC, 1, 200),
    wxListCtrl:setItemCount(LC, 1000000),

    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).


radioBox(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
radioBox(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),

    TrSortRadioBox = wxRadioBox:new(Frame, ?wxID_ANY, "Sort by:",
				    {100, 100},{100, 100}, ["Timestamp"]),

    io:format("TrSortRadioBox ~p ~n", [TrSortRadioBox]),
    %% If I uncomment any of these lines, it will crash

    io:format("~p~n", [catch wxControlWithItems:setClientData(TrSortRadioBox, 0, timestamp)]),
    %?m(_, wxListBox:append(TrSortRadioBox, "Session Id", session_id)),
    %?m(_, wxListBox:insert(TrSortRadioBox, "Session Id", 0, session_id)),

    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).


systemSettings(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
systemSettings(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),

    ?m({_,_,_,_}, wxSystemSettings:getColour(?wxSYS_COLOUR_DESKTOP)),
    ?mt(wxFont, wxSystemSettings:getFont(?wxSYS_SYSTEM_FONT)),
    ?m(true, is_integer(wxSystemSettings:getMetric(?wxSYS_MOUSE_BUTTONS))),
    ?m(true, is_integer(wxSystemSettings:getScreenType())),

    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).


textCtrl(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
textCtrl(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),

    TC = ?mt(wxTextCtrl, wxTextCtrl:new(Frame, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH2}])),
    wxTextCtrl:appendText(TC, "This line is in default color\n"),
    Attr = ?mt(wxTextAttr, wxTextAttr:new(?wxRED)),
    wxTextCtrl:setDefaultStyle(TC, Attr),
    wxTextCtrl:appendText(TC, "This line is in ?wxRED color\n"),
    wxTextAttr:setTextColour(Attr, ?wxBLACK),
    wxTextCtrl:setDefaultStyle(TC, Attr),
    wxTextCtrl:appendText(TC, "This line is in ?wxBLACK color\n"),
    Default = wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOWTEXT),
    wxTextAttr:setTextColour(Attr, Default),
    wxTextCtrl:setDefaultStyle(TC, Attr),
    wxTextCtrl:appendText(TC, "This line is in default color\n"),
    wxTextAttr:destroy(Attr),
    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).

taskBarIcon(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
taskBarIcon(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    TBI = wxTaskBarIcon:new(),
    Image = wxImage:new(filename:join(code:priv_dir(debugger), "erlang_bug.png")),
    io:format("Image ~p~n",[wxImage:ok(Image)]),
    Icon = wxIcon:new(filename:join(code:priv_dir(debugger), "erlang_bug.png"), [{type, ?wxBITMAP_TYPE_PNG}]),
    wxTaskBarIcon:setIcon(TBI, Icon, [{tooltip, "Testing wxTaskBarIcon"}]),
    wxWindow:show(Frame),
    wxTaskBarIcon:connect(TBI, taskbar_left_down, [{callback, fun(Ev,_) -> io:format("Left clicked: ~p~n",[Ev]) end}]),
    wxTaskBarIcon:connect(TBI, taskbar_right_down, [{callback,fun(Ev,_) -> io:format("Right clicked: ~p~n",[Ev]) end}]),
    wx_test_lib:wx_destroy(Frame,Config).

toolbar(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
toolbar(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    TB = wxFrame:createToolBar(Frame),
    wxToolBar:addTool(TB, 747, "PressMe", wxArtProvider:getBitmap("wxART_COPY", [{size, {16,16}}]),
		      [{shortHelp, "Press Me"}]),

    Add = fun(#wx{}, _) ->
		  wxToolBar:addTool(TB, -1, "Added", wxArtProvider:getBitmap("wxART_TICK_MARK", [{size, {16,16}}]),
				    [{shortHelp, "Test 2 popup text"}])
	  end,

    wxFrame:connect(Frame, command_menu_selected, [{callback, Add}, {id, 747}]),
    wxFrame:show(Frame),
    wx_test_lib:wx_destroy(Frame,Config).


popup(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
popup(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    TB = wxFrame:createToolBar(Frame),
    wxToolBar:addTool(TB, 747, "PressMe", wxArtProvider:getBitmap("wxART_COPY", [{size, {16,16}}]),
		      [{shortHelp, "Press Me"}]),

    Log = fun(#wx{id=Id, event=Ev}, Obj) ->
		  io:format("Got ~p from ~p~n", [Id, Ev]),
		  wxEvent:skip(Obj)
	  end,
    CreatePopup = fun() ->
			  Pop = wxPopupTransientWindow:new(Frame),
			  Panel = wxPanel:new(Pop),
			  Sz = wxBoxSizer:new(?wxVERTICAL),
			  wxSizer:add(Sz, wxButton:new(Panel, 42, [{label, "A button"}])),
			  wxSizer:add(Sz, Txt = wxStaticText:new(Panel, 43, "Some static text")),
			  wxSizer:add(Sz, wxButton:new(Panel, 44, [{label, "B button"}])),
			  wxPanel:setSizerAndFit(Panel, Sz),
			  wxSizer:setSizeHints(Sz, Pop),
			  wxWindow:connect(Pop, command_button_clicked, [{callback, Log}]),
			  wxWindow:connect(Txt, left_up, [{callback, Log}]),
			  wxWindow:connect(Txt, middle_up, [{callback, Log}]),
			  wxWindow:connect(Txt, right_up, [{callback, Log}]),
			  wxWindow:connect(Pop, show, [{callback, Log}]),
			  Pos = wx_misc:getMousePosition(),
			  wxPopupTransientWindow:position(Pop, Pos, {-1, -1}),
			  wxPopupTransientWindow:popup(Pop),
			  Pop
		  end,
    wxFrame:connect(Frame, command_menu_selected, [{id, 747}]),
    wxFrame:show(Frame),

    Pop = CreatePopup(),
    Scale = case wx_test_lib:user_available(Config) of
		true -> 25;
		false -> 1
	    end,
    receive
	#wx{} -> CreatePopup()
    after 200*Scale ->
	    wxPopupTransientWindow:dismiss(Pop)
    end,
    wx_test_lib:wx_destroy(Frame,Config).
