%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
%%% File    : wx_basic_SUITE.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Basic SUITE, some simple tests to show that the basics 
%%%               are working.
%%% Created :  3 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_xtra_SUITE).
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
     destroy_app,
     multiple_add_in_sizer,
     app_dies
    ].

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
    wx:destroy(),
    receive 
	Msg -> Msg
    after 150 -> destroy_app_test(N-1)
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
	{'EXIT', _, {oops, last}} -> ok;
	{'EXIT', _, {oops, _}} -> app_dies2(Test, N+1)
    end.

oops(Die, Line) when (Die =:= last) orelse (Die =< Line) ->
    timer:sleep(500),
    ?log(" Exits at line ~p~n",[Line]),
    exit({oops, Die});
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

