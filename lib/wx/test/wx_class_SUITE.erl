%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-export([all/0, init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, fin_per_testcase/2, end_per_testcase/2]).

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
fin_per_testcase(Func,Config) -> %% For test_server
    wx_test_lib:end_per_testcase(Func,Config).

%% SUITE specification
all() ->
    all(suite).
all(suite) ->
    [
     calendarCtrl, 
     treeCtrl,
     notebook,
     staticBoxSizer,
     clipboard,
     helpFrame,
     htmlWindow,
     listCtrlSort,
     radioBox
    ].

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
	    ?log("DateAttr is useable~n",[])
    end,
    DateAttr = ?mt(wxCalendarDateAttr, wxCalendarDateAttr:new()),
    wxCalendarDateAttr:setBackgroundColour(DateAttr, {0,243,0}),
    wxCalendarCtrl:setAttr(Cal, Day, DateAttr),
    DateAttr1 = ?mt(wxCalendarDateAttr, wxCalendarCtrl:getAttr(Cal,Day)),
    ?m({0,243,0,255}, wxCalendarDateAttr:getBackgroundColour(DateAttr1)),

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
    
    ?m([], wxTreeCtrl:getItemData(Tree, Root)),
    ?m({data,item1}, wxTreeCtrl:getItemData(Tree, Item1)),
    ?m({data,item2}, wxTreeCtrl:getItemData(Tree, Item2)),
    ?m({data,item3}, wxTreeCtrl:getItemData(Tree, Item3)),
    
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
    
    LC = wxListCtrl:new(Frame, [{style, ?wxLC_REPORT bor ?wxLC_SORT_ASCENDING}]),

    %% must be done crashes in wxwidgets otherwise.
    wxListCtrl:insertColumn(LC, 0, "Column"),
    
    Add = fun(Int) ->    
		  wxListCtrl:insertItem(LC, Int, integer_to_list(Int)),
		  %% ItemData Can only be integers currently
		  wxListCtrl:setItemData(LC, Int, abs(2500-Int))
	  end,
    
    wx:foreach(Add, lists:seq(0,5000)),
    wxWindow:show(Frame),

    timer:sleep(200),

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
    _List = wx:map(fun(Int) ->
			   wxListItem:setId(Item, Int),
			   ?m(true, wxListCtrl:getItem(LC, Item)),
			   io:format("~s~n",[wxListItem:getText(Item)])
		   end, lists:seq(0,100)),
    wxListItem:destroy(Item),

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
