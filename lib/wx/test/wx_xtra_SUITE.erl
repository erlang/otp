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
%%%-------------------------------------------------------------------
%%% File    : wx_basic_SUITE.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Basic SUITE, some simple tests to show that the basics 
%%%               are working.
%%% Created :  3 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_xtra_SUITE).
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
    [destroy_app, multiple_add_in_sizer, app_dies,
     menu_item_debug].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

%%  Verify that everything is handled on the queue first
%%  before wx:destroy is called.
destroy_app(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
destroy_app(_Config) ->
    %% This is timing releated but we test a couple of times
    wx_test_lib:flush(),
    ?m(ok, destroy_app_test(15)).

destroy_app_test(N) when N > 0 ->
    Wx = ?mr(wx_ref, wx:new()),    
    Frame = wxFrame:new(Wx, 1, "Destroy"),
    ?m(ok, wxFrame:destroy(Frame)),
    receive 
	Msg -> Msg
    after 150 -> 
	    wx:destroy(),
	    destroy_app_test(N-1)
    end;
destroy_app_test(_) -> 
    receive 
	Msg -> Msg
    after 1000 ->  ok
    end.


%% This should work, and does but not when run automaticly on windows 
%% for some strange reason (it just hangs), run it last.
app_dies(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
app_dies(_Config) ->
    Tester = fun(Die0) ->
		     Die = (Die0*2) + ?LINE,
		     Wx = wx:new(),
		     oops(Die,?LINE),
		     Frame = wxFrame:new(Wx, 1, ?MODULE_STRING ++ integer_to_list(?LINE)),
		     oops(Die,?LINE),
		     wxFrame:createStatusBar(Frame, []),
		     oops(Die,?LINE),
		     Win=wxWindow:new(Frame, ?wxID_ANY),
		     oops(Die,?LINE),
		     _Pen  = wxPen:new({0,0,0}, [{width, 3}]),
		     oops(Die,?LINE),
		     _Font = wxFont:new(10, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[]),
		     oops(Die,?LINE), 
		     wxWindow:connect(Win, key_up),  
		     oops(Die,?LINE),
		     wxWindow:connect(Win, key_up, [{callback, fun(_,_) -> callback end}]),
		     oops(Die,?LINE),
		     wxFrame:show(Frame),
		     oops(Die,?LINE),
		     timer:sleep(100), %% Let the window be shown before DC can be created
		     DC0  = wxClientDC:new(Win),
		     oops(Die,?LINE),
		     DC   = wxBufferedDC:new(DC0),
		     oops(Die,?LINE),
		     _Size = wxWindow:getSize(Win),
		     oops(Die,?LINE),		    %% redraw(DC, Size, G),
		     wxBufferedDC:destroy(DC),
		     oops(Die,?LINE),
		     wxClientDC:destroy(DC0),
		     oops(last,?LINE)
	     end,
    process_flag(trap_exit,true),
    app_dies2(Tester, 1),
    ok.

app_dies2(Test, N) ->
    spawn_link(fun() -> Test(N) end),
    receive
	{'EXIT', _, {oops, Server, What}} ->
	    Ref = erlang:monitor(process, Server),
	    receive {'DOWN', Ref, _, _, _} -> ok end,
	    timer:sleep(100),
	    What =/= last andalso app_dies2(Test, N+1)
    end.

oops(Die, Line) when (Die =:= last) orelse (Die =< Line) ->
    timer:sleep(200),
    ?log(" Exits at line ~p~n",[Line]),
    Server = element(3, wx:get_env()),
    exit({oops, Server, Die});
oops(_,_) -> ok.


%% This have happend often enough that I have special code to handle 
%% this user error (i.e. using the a window twice in an sizer).
multiple_add_in_sizer(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
multiple_add_in_sizer(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Button Fix"),
    wxFrame:connect(Frame, close_window),

    FramePanel = wxPanel:new(Frame),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(FramePanel, Sizer),
    wxSizer:setSizeHints(Sizer, Frame),

    Panel = wxPanel:new(FramePanel),
    Button = wxButton:new(Panel, -1, [{label, "Centre Me!"}]),

    PanelSizer = wxBoxSizer:new(?wxVERTICAL),

%%%%%%%%%%% THIS CALL CRASHES BEAM AT DESTROY TIME %%%%%%%%%%%%%
    wxPanel:setSizer(Panel, PanelSizer),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    ButtonSizer = wxBoxSizer:new(?wxVERTICAL),

    SizerFlags = wxSizerFlags:new(),
    wxSizerFlags:align(SizerFlags, ?wxALIGN_CENTRE),

    wxSizer:add(ButtonSizer, Button, SizerFlags), %% no tricks

    wxSizerFlags:expand(SizerFlags), %
    wxSizer:add(PanelSizer, ButtonSizer, SizerFlags),

    %% PanelSizer is added to a size twice
    wxSizer:add(Sizer, PanelSizer, SizerFlags),

    wxFrame:setSize(Frame, 400, 300),
    io:format("Panel ~p PSizer ~p ~n",[Panel, PanelSizer]),
    %% io:format("F
    wxWindow:show(Frame),
    wx_test_lib:wx_destroy(Frame, Config).

menu_item_debug(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
menu_item_debug(Config) ->
    %% Debugging a menu entry problem
    %% Run it with: lists:map(fun(_) -> [{0,{ok,_,_}}] = wxt:t() end, lists:seq(1,50)), ok.
    Wx = wx:new(),
    wx:debug(trace),
    Frame = wxFrame:new(Wx, -1, "Button Fix"),
    wxFrame:connect(Frame, close_window),

    wxPanel:new(Frame),
    MenuBar = create_menus(Frame),
    wxWindow:show(Frame),
    N = wxMenuBar:getMenuCount(MenuBar),
    io:format("No of menus ~p~n",[N]),
    [io:format("Menu ~p ~p~n",[Id, wxMenuBar:getLabelTop(MenuBar, Id)]) 
     || Id <- lists:seq(0, N-1)],
    wx_test_lib:wx_destroy(Frame,Config).


create_menus(Frame) ->
    MenuBar = ?mt(wxMenuBar, wxMenuBar:new()),
    File    = ?mt(wxMenu, wxMenu:new([])),
    Help    = ?mt(wxMenu, wxMenu:new([])),

    ?mt(wxMenuItem, wxMenu:append(Help, ?wxID_ABOUT, "&About", [])),
    ?mt(wxMenuItem, wxMenu:append(Help, ?wxID_HELP, "&Help", [])),
    ?mt(wxMenuItem, wxMenu:append(File, ?wxID_EXIT, "Exit", [])),
    T1    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T1, Id, integer_to_list(Id), []))
     || Id <- lists:seq(100, 120)],
    T2    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T2, Id, integer_to_list(Id), []))
     || Id <- lists:seq(200, 220)],
    T3    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T3, Id, integer_to_list(Id), []))
     || Id <- lists:seq(300, 320)],
    T4    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T4, Id, integer_to_list(Id), []))
     || Id <- lists:seq(400, 420)],
    T5    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T5, Id, integer_to_list(Id), []))
     || Id <- lists:seq(500, 520)],
    T6    = ?mt(wxMenu, wxMenu:new([])),
    [wxMenuItem:getId(wxMenu:append(T6, Id, integer_to_list(Id), []))
     || Id <- lists:seq(600, 620)],

    ?m(ok,wxFrame:connect(Frame, command_menu_selected)),

    ?m(true, wxMenuBar:insert(MenuBar, 0,File, "&File")),
    ?m(true, wxMenuBar:insert(MenuBar, 1,Help, "&Help")),
    ?m(true, wxMenuBar:insert(MenuBar, 2,T1, "T1")),
    ?m(true, wxMenuBar:insert(MenuBar, 3,T2, "T2")),
    ?m(true, wxMenuBar:insert(MenuBar, 4,T3, "T3")),
    ?m(true, wxMenuBar:insert(MenuBar, 5,T4, "T4")),
    ?m(true, wxMenuBar:insert(MenuBar, 6,T5, "T5")),
    ?m(true, wxMenuBar:insert(MenuBar, 7,T6, "T6")),

    ?m(ok, wxFrame:setMenuBar(Frame,MenuBar)),
    MenuBar.
