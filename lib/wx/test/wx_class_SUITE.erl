%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
suite() -> [{ct_hooks,[ts_install_cth]}, {timetrap,{minutes,2}}].

all() ->
    [calendarCtrl, treeCtrl, notebook, staticBoxSizer,
     clipboard, helpFrame, htmlWindow, listCtrlSort, listCtrlVirtual,
     radioBox, systemSettings, taskBarIcon, toolbar, popup, modal].

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

    {YMD={_,_,Day},_} = DateTime = calendar:now_to_datetime(os:timestamp()),
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
clipboard(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Main Frame"),
    Ctrl = wxTextCtrl:new(Frame, ?wxID_ANY, [{size, {600,400}}, {style, ?wxTE_MULTILINE}]),
    wxTextCtrl:connect(Ctrl, command_text_copy, [{skip, true}]),
    wxTextCtrl:connect(Ctrl, command_text_cut, [{skip, true}]),
    wxTextCtrl:connect(Ctrl, command_text_paste, [{skip, true}]),
    wxWindow:show(Frame),

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
    wx_test_lib:wx_destroy(Frame,Config).


helpFrame(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
helpFrame(Config) ->
    Wx = wx:new(),
    MFrame = wx:batch(fun() ->
			      MFrame = wxFrame:new(Wx, ?wxID_ANY, "Main Frame"),
			      wxPanel:new(MFrame, [{size, {600,400}}]),
			      wxFrame:connect(MFrame, show),
			      wxWindow:show(MFrame),
			      MFrame
		      end),
    receive #wx{event=#wxShow{}} -> ok
    after 1000 -> exit(show_timeout)
    end,

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

    %% Test that wx-asserts are sent to error logger
    %% Force an assert on 3.0 (when debug compiled which it is by default)
    wxListItem:setId(Item, 200),
    case os:type() of
	{win32, _} ->
	    wxListItem:setColumn(Item, 3),
	    io:format("Got ~p ~n", [wxListCtrl:insertItem(LC, Item)]),
	    wxListItem:setColumn(Item, 0);
	_ -> %% Uses generic listctrl
	    %% we can't use the code above on linux with wx-2.8.8 because it segfaults.
	    io:format("Got ~p ~n", [wxListCtrl:getItem(LC, Item)])
    end,

    wxListItem:setMask(Item, ?wxLIST_MASK_TEXT),
    _List = wx:map(fun(Int) ->
			   wxListItem:setId(Item, Int),
			   ?m(true, wxListCtrl:getItem(LC, Item)),
			   io:format("~p: ~s~n",[Int, wxListItem:getText(Item)])
		   end, lists:seq(0,10)),

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
				    {100, 100},{100, 100},
				    ["Timestamp", "Session", "FooBar"]),

    io:format("TrSortRadioBox ~p ~n", [TrSortRadioBox]),
    wxRadioBox:setSelection(TrSortRadioBox, 2),
    wxRadioBox:setItemToolTip(TrSortRadioBox, 2, "Test"),
    TT0 = ?mt(wxToolTip,wxRadioBox:getItemToolTip(TrSortRadioBox, 0)),
    TT1 = ?mt(wxToolTip,wxRadioBox:getItemToolTip(TrSortRadioBox, 2)),
    ?m(true, wx:is_null(TT0)),
    ?m("Test", wxToolTip:getTip(TT1)),
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
    BM1 = wxArtProvider:getBitmap("wxART_COPY", [{size, {16,16}}, {client, "wxART_TOOLBAR"}]),
    BM2 = wxArtProvider:getBitmap("wxART_TICK_MARK", [{size, {16,16}}, {client, "wxART_TOOLBAR"}]),
    wxToolBar:addTool(TB, 747, "PressMe", BM1,
		      [{shortHelp, "Press Me"}]),
    catch wxToolBar:addStretchableSpace(TB),  %% wxWidgets 3.0 only
    Add = fun(#wx{}, _) ->
		  wxToolBar:addTool(TB, -1, "Added", BM2,
				    [{shortHelp, "Test 2 popup text"}]),
		  catch wxToolBar:addStretchableSpace(TB), %% wxWidgets 3.0 only
		  wxToolBar:realize(TB)
	  end,

    wxToolBar:realize(TB),
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
		  io:format("Got ~p from ~p~n", [Ev, Id]),
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
    wxFrame:connect(Frame, show),
    wxFrame:show(Frame),
    receive #wx{event=#wxShow{}} -> ok
    after 1000 -> exit(show_timeout)
    end,

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

locale(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
locale(_Config) ->
    wx:new(),
    io:format("SystemEncoding: ~p~n",[wxLocale:getSystemEncoding()]),
    io:format("SystemEncodingName: ~ts~n",[wxLocale:getSystemEncodingName()]),
    io:format("SystemLanguage: ~p~n",[wxLocale:getSystemLanguage()]),
    io:format("SystemLanguageName: ~p~n",[wxLocale:getLanguageName(wxLocale:getSystemLanguage())]),
    lang_env(),
    LC = wxLocale:new(),
    %% wxLocale:addCatalog(LC, "wxstd"),
    io:format("Swedish: ~p~n",[wxLocale:getLanguageName(?wxLANGUAGE_SWEDISH)]),
    R0 = wxLocale:init(LC, [{language, ?wxLANGUAGE_SWEDISH}, {flags, 0}]),
    io:format("initiated ~p~n",[R0]),
    lang_env(),
    ok.
%% wx_test_lib:wx_destroy(Frame,Config).

lang_env() ->
    Env0 = os:getenv(),
    Env = [[R,"\n"]||R <- Env0],
    %%io:format("~p~n",[lists:sort(Env)]),
    Opts = [global, multiline, {capture, all, list}],
    format_env(re:run(Env, "LC_ALL.*", Opts)),
    format_env(re:run(Env, "^LANG.*=.*$", Opts)),
    ok.
format_env({match, List}) ->
    [io:format("  ~ts~n",[L]) || L <- List];
format_env(nomatch) -> ok.

%%  Add a testcase that tests that we can recurse in showModal
%%  because it hangs in observer if object are not destroyed correctly
%%  when popping the stack

modal(Config) ->
    Wx = wx:new(),
    case {?wxMAJOR_VERSION, ?wxMINOR_VERSION, ?wxRELEASE_NUMBER} of
	{2, Min, Rel} when Min < 8 orelse (Min =:= 8 andalso Rel < 11) ->
	    {skip, "old wxWidgets version"};
	_ ->
	    Frame = wxFrame:new(Wx, -1, "Test Modal windows"),
	    wxFrame:show(Frame),
	    Env = wx:get_env(),
	    Tester = self(),
	    ets:new(test_state, [named_table, public]),
	    Upd = wxUpdateUIEvent:getUpdateInterval(),
	    wxUpdateUIEvent:setUpdateInterval(500),
	    _Pid = spawn(fun() ->
				 wx:set_env(Env),
				 modal_dialog(Frame, 1, Tester)
			 end),
	    %% need to sleep so we know that the window is stuck in
	    %% the ShowModal event loop and not in an earlier event loop
	    %% wx2.8 invokes the event loop from more calls than wx-3
	    M1 = receive {dialog, W1, 1} -> timer:sleep(1200), ets:insert(test_state, {W1, ready}), W1 end,
	    M2 = receive {dialog, W2, 2} -> timer:sleep(1200), ets:insert(test_state, {W2, ready}), W2 end,

	    receive done -> ok end,
	    receive {dialog_done, M2, 2} -> M2 end,
	    receive {dialog_done, M1, 1} -> M1 end,

	    wxUpdateUIEvent:setUpdateInterval(Upd),
	    wx_test_lib:wx_destroy(Frame,Config)
    end.

modal_dialog(Parent, Level, Tester) when Level < 3 ->
    M1 = wxTextEntryDialog:new(Parent, "Dialog " ++ integer_to_list(Level)),
    io:format("Creating dialog ~p ~p~n",[Level, M1]),
    wxDialog:connect(M1, show, [{callback, fun(#wx{event=Ev},_) ->
						   case Ev of
						       #wxShow{show=true} ->
							   Tester ! {dialog, M1, Level};
						       _ -> ignore
						   end
					   end}]),
    DoOnce = fun(_,_) ->
		     case ets:take(test_state, M1) of
			 [] -> ignore;
			 [_] -> modal_dialog(M1, Level+1, Tester)
		     end
	     end,
    wxDialog:connect(M1, update_ui, [{callback, DoOnce}]),
    ?wxID_OK = wxDialog:showModal(M1),
    wxDialog:destroy(M1),
    case Level > 1 of
	true ->
	    io:format("~p: End dialog ~p ~p~n",[?LINE, Level-1, Parent]),
	    wxDialog:endModal(Parent, ?wxID_OK);
	false -> ok
    end,
    Tester ! {dialog_done, M1, Level},
    ok;
modal_dialog(Parent, Level, Tester) ->
    io:format("~p: End dialog ~p ~p~n",[?LINE, Level-1, Parent]),
    wxDialog:endModal(Parent, ?wxID_OK),
    Tester ! done.
