%% -*- coding: utf-8 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-module(wx_basic_SUITE).
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
    [silent_start, create_window, several_apps, wx_api, wx_misc,
     data_types, wx_object].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

  
%% The test cases

%% test silent start of wx
silent_start(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
silent_start(_Config) ->
    ?mr(wx_ref, wx:new([])),
    wx:destroy(),

    ?mr(wx_ref, wx:new([{silent_start, true}])),
    wx:destroy(),

    ?mr(wx_ref, wx:new([{silent_start, true}, {debug, verbose}])),
    wx:destroy(),

    ?mr(wx_ref, wx:new([{silent_start, false}])),
    wx:destroy(),

    ?mr('EXIT', catch wx:new([{silent_start, foo}])),
    
    ok.

%% create and test creating a window
create_window(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
create_window(Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(Wx, 1, "Hello World")),
    timer:sleep(1000),
    ?m(true,wxWindow:show(Frame, [])),
    wx_test_lib:wx_destroy(Frame, Config).

%% create several windows from independent processes 
%% to simulate several applications and test creating a window
several_apps(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
several_apps(Config) -> 
    Parent = self(),
    Pids = [spawn_link(fun() -> several_apps(Parent, N, Config) end) 
	    || N <- lists:seq(1,4)],
    process_flag(trap_exit,true),
    ?m_multi_receive([{complete,Pid} || Pid <- Pids]),
    [Pid ! quit || Pid <- Pids],
    case wx_test_lib:user_available(Config) of
	true ->
	    receive {'EXIT',_,foo} -> ok end;
	false ->
	    ok
    end.

several_apps(Parent, N, Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(Wx, 1, "Hello World No:" ++ 
				     integer_to_list(N))),
    create_menus(Frame),
    wxFrame:connect(Frame,size),
    ?m(true,wxWindow:show(Frame, [])),
    receive 
	#wx{obj=Frame, event=#wxSize{}} ->
	    Parent ! {complete, self()}
    end,
    receive quit -> ok end,
    wx_test_lib:wx_destroy(Frame, Config),
    exit(foo).


%% Test the wx.erl api functionality.
wx_api(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
wx_api(Config) ->
    Wx = ?mr(wx_ref, wx:new()), 
    ?m(true, wx:is_null(Wx)),
    Null = ?mr(wx_ref, wx:null()),
    ?m(true, wx:is_null(Null)),
    Frame = ?mt(wxFrame, wxFrame:new(Wx, 1, "WX API: " ++ unicode:characters_to_list("åäöÅÄÖ"))),
    ?m(false, wx:is_null(Frame)),
    ?m(wxFrame, wx:getObjectType(Frame)),
    Env = ?mr(wx_env, wx:get_env()),
    %% Test some error cases 
    erase(wx_env),
    ?m({'EXIT', {{wxe,unknown_port},_}},wxWindow:show(Frame, [])),
    ?m({'EXIT', {{wxe,unknown_port},_}},wx:debug(2)),
    
    ?m(ok,wx:set_env(Env)),
    ?m(ok,wx:debug(1)),
    ?m(ok,wx:debug(2)),
    ?m(ok,wx:debug(0)),
    ?m(ok,wx:debug(none)),
    ?m(ok,wx:debug(verbose)),
    ?m(ok,wx:debug(trace)),
    
    Mem = ?mr(wx_mem, wx:create_memory(10)),
    ?m(true, is_binary(wx:get_memory_bin(Mem))),
    ?mt(foo, wx:typeCast(Frame, foo)),

    RecBatch = fun() -> 
		       wx:batch(fun() -> create_menus(Frame) end)
	       end,
    ?m(batch_ret, wx:batch(fun() -> RecBatch(), batch_ret end)),
    ?m(ok, wx:foreach(fun(A) -> true = lists:member(A,[1,2,3,4,5]) end, 
		      lists:seq(1,5))),
    ?m([2,3,4,5,6], wx:map(fun(A) -> A+1 end, lists:seq(1,5))),
    ?m({5,15}, wx:foldl(fun(A,{_,Acc}) -> {A,A+Acc} end, {0,0},
			lists:seq(1,5))),
    ?m({1,15}, wx:foldr(fun(A,{_,Acc}) -> {A,A+Acc} end, {0,0},
			lists:seq(1,5))),
    ?m(ok,wx:debug(none)),
    
    ?m(ball, wx:batch(fun() -> throw(ball), batch_ret end)),
    ?m({'EXIT', door}, wx:batch(fun() -> exit(door), batch_ret end)),
    ?m({'EXIT',{message,_ST}}, 
       wx:batch(fun() -> erlang:error(message), batch_ret end)),
    

    ?m({'EXIT',_},wxWindow:show(wx:null(), [])),
    ?m(true,wxWindow:show(Frame, [])),
    Temp = ?mt(wxButton, wxButton:new(Frame, -1)),
    ?m(ok,wxButton:setLabel(Temp, "Testing")),
    ?m(ok,wxButton:destroy(Temp)),
    ?m({'EXIT',_},wxButton:getLabel(Temp)),
    
    case wx_test_lib:user_available(Config) of
	true -> 	    
	    %% Hmm popup doesn't return until mouse is pressed.
	    Menu = wxMenu:new(),
	    wxMenu:append(Menu, 0, "Press", []),
	    wxMenu:append(Menu, 1, "Me", []),
	    ?m(true, wxWindow:popupMenu(Frame, Menu)),
	    %% This didn't work for a while
	    ?m(true, wx:batch(fun() -> 
				      wxMenu:append(Menu, 2, "AGAIN", []),
				      wxWindow:popupMenu(Frame, Menu) 
			      end)),
	    ok;
	_ ->
	    ignore
    end,
    
%%     dbg:tracer(),
%%     {_, _Port, Server, _Dbg} = wx:get_env(),
%%     dbg:p(Server, [m, call]),
%%     dbg:p(new, [m, call]),
%%     dbg:tpl(wxe_server,'_', [{'_', [], [{return_trace}]}]),
    wx_test_lib:wx_destroy(Frame,Config).
		  
create_menus(Frame) ->
    MenuBar = ?mt(wxMenuBar, wxMenuBar:new()),
    File    = ?mt(wxMenu, wxMenu:new([])),
    Help    = ?mt(wxMenu, wxMenu:new([])),
    ?mt(wxMenuItem, wxMenu:append(Help, ?wxID_ABOUT, "&About", [])),
    ?mt(wxMenuItem, wxMenu:append(Help, ?wxID_HELP, "&Help", [])),
    ?mt(wxMenuItem, wxMenu:append(File, ?wxID_EXIT, "Exit", [])), 
    ?m(ok,wxFrame:connect(Frame, command_menu_selected)), 
    ?m(true, wxMenuBar:append(MenuBar, File, "&File")),
    ?m(true, wxMenuBar:append(MenuBar, Help, "&Help")),
    ?m(ok, wxFrame:setMenuBar(Frame,MenuBar)).


%% Test the wx_misc.erl api functionality.
wx_misc(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
wx_misc(Config) ->    
    wx:new([{debug, trace}]),
    put(wx_test_verbose, true),
    ?m(ok, wx_misc:bell()),
    ?m(true, length(wx_misc:getUserId()) > 0),
    ?m(true, is_list(wx_misc:getEmailAddress())),
    Home = ?m([_|_], wx_misc:getHomeDir()),
    ?m(true, filelib:is_dir(Home)),
    ?m(true, length(wx_misc:getOsDescription()) > 0),
    IsLitte = case <<1:32/native>> of 
		  <<1:8, 0:24>> -> true;
		  <<0:24,1:16>> -> false
	      end,
    ?m(IsLitte, wx_misc:isPlatformLittleEndian()),
    ?m(true, is_boolean(wx_misc:isPlatform64Bit())),
    
    ?mr(wxMouseState, wx_misc:getMouseState()),
    ?m({_,_}, wx_misc:getMousePosition()),
    
    %% Don't hold home down when testing :-)
    ?m(false, wx_misc:getKeyState(?WXK_HOME)), 

    
    %% wx:shutdown()  %% How do you test this?

    case os:type() of 
	{win32, _} -> %% These hangs when running automatic tests
	    skip;     %% through ssh on windows. Works otherwise
	_ -> 
	    wx_misc:shell([{command,"echo TESTING close the popup shell"}])
    end,

    case wx_test_lib:user_available(Config) of
	true ->
	    wx_misc:shell();
	false ->
	    %% Don't want to spawn a shell if no user	   
	    skip %% is available
    end,

    ?m(false, wx_misc:isBusy()),
    ?m(ok, wx_misc:beginBusyCursor([])),
    ?m(true, wx_misc:isBusy()),
    ?m(ok, wx_misc:endBusyCursor()),
    
    %%?m(true, is_boolean(wx_misc:setDetectableAutoRepeat(true)),
    Curr  = wx_misc:getCurrentId(),
    ?m(true, is_integer(Curr)),
    NewId = wx_misc:newId(),
    ?m(ok, wx_misc:registerId(NewId+1)),
    ?m(true, (NewId+1) /= wx_misc:newId()),
    
    wx:destroy().


%% Check that all the data_types works in communication 
%% between erlang and c++ thread.
data_types(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
data_types(_Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    
    Frame = wxFrame:new(Wx, 1, "Data Types"),
    CDC = wxClientDC:new(Frame),

    %% From wx.erl
    %% The following classes are implemented directly as erlang types: <br />
    %% wxPoint={x,y},wxSize={w,h},wxRect={x,y,w,h},wxColour={r,g,b [,a]},wxString=[integer],
    %% wxGBPosition={r,c},wxGBSpan={rs,cs},wxGridCellCoords={r,c}.

    %% Strings
    ?m("Data Types", wxFrame:getTitle(Frame)),

    %% Doubles
    ?m(ok, wxDC:setUserScale(CDC, 123.45, 234.67)),
    ?m({123.45,234.67}, wxDC:getUserScale(CDC)),

    %% Colors input is 3 or 4 tuple, returns are 4 tuples
    ?m(ok, wxDC:setTextForeground(CDC, {100,10,1})),
    ?m({100,10,1,255}, wxDC:getTextForeground(CDC)),
    ?m(ok, wxDC:setTextForeground(CDC, {100,10,1,43})),
    ?m({100,10,1,43}, wxDC:getTextForeground(CDC)),

    %% Bool 
    ?m(ok, wxDC:setAxisOrientation(CDC, true, false)),
    ?m(true, is_boolean(wxDC:isOk(CDC))),
    
    %% wxCoord 
    ?m(true, is_integer(wxDC:maxX(CDC))),
    
    %% wxSize
    ?m({_,_}, wxWindow:getSize(Frame)),

    %% DateTime 
    DateTime = {Date, _Time} = calendar:now_to_datetime(erlang:now()),
    io:format("DateTime ~p ~n",[DateTime]),
    Cal = ?mt(wxCalendarCtrl, wxCalendarCtrl:new(Frame, ?wxID_ANY, [{date,DateTime}])),
    ?m({Date,_}, wxCalendarCtrl:getDate(Cal)),
    ?m(true, is_boolean(wxCalendarCtrl:setDate(Cal,DateTime))),
    ?m({Date,_}, wxCalendarCtrl:getDate(Cal)),

    wxClientDC:destroy(CDC),
    %%wx_test_lib:wx_destroy(Frame,Config).
    wx:destroy().

wx_object(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
wx_object(Config) ->
    wx:new(),
    Me = self(),
    Init = fun() ->
		   Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Test wx_object", [{size, {500, 400}}]),
		   Sz = wxBoxSizer:new(?wxHORIZONTAL),
		   Panel = wxPanel:new(Frame),
		   wxSizer:add(Sz, Panel, [{flag, ?wxEXPAND}, {proportion, 1}]),
		   wxPanel:connect(Panel, size, [{skip, true}]),
		   wxPanel:connect(Panel, paint, [callback, {userData, Me}]),
		   wxWindow:show(Frame),
		   {Frame, {Frame, Panel}}
	   end,
    Frame = ?mt(wxFrame, wx_obj_test:start([{init, Init}])),
    timer:sleep(500),
    ?m(ok, check_events(flush())),

    Me = self(),
    ?m({call, foobar, {Me, _}}, wx_object:call(Frame, foobar)),
    ?m(ok, wx_object:cast(Frame, foobar2)),
    ?m([{cast, foobar2}], flush()),
    FramePid = wx_object:get_pid(Frame),
    io:format("wx_object pid ~p~n",[FramePid]),
    FramePid ! foo3,
    ?m([{info, foo3}], flush()),

    ?m(ok, wx_object:cast(Frame, fun(_) -> hehe end)),
    ?m([{cast, hehe}], flush()),
    wxWindow:refresh(Frame),
    ?m([{sync_event, #wx{event=#wxPaint{}}, _}], flush()),
    ?m(ok, wx_object:cast(Frame, fun(_) -> timer:sleep(200), slept end)),
    %% The sleep above should not hinder the Paint event below
    %% Which it did in my buggy handling of the sync_callback
    wxWindow:refresh(Frame),
    ?m([{sync_event, #wx{event=#wxPaint{}}, _}], flush()),
    ?m([{cast, slept}], flush()),

    Monitor = erlang:monitor(process, FramePid),
    case proplists:get_value(user, Config, false) of
	false ->
	    timer:sleep(100),
	    wxFrame:destroy(Frame);
	true ->
	    timer:sleep(500),
	    ?m(ok, wxFrame:destroy(Frame));
	_ ->
	    ?m(ok, wxEvtHandler:connect(Frame, close_window, [{skip,true}])),
	    wx_test_lib:wait_for_close()
    end,
    ?m(ok, receive
	       {'DOWN', Monitor, _, _, _} ->
		   ?m([{terminate, wx_deleted}], flush()),
		   ok
	   after 1000 ->
		   Msgs = flush(),
		   io:format("Error ~p Alive ~p~n",[Msgs, is_process_alive(FramePid)])
	   end),
    catch wx:destroy(),
    ok.

check_events(Msgs) ->
    check_events(Msgs, 0,0).

check_events([{event, #wx{event=#wxSize{}}}|Rest], Async, Sync) ->
    check_events(Rest, Async+1, Sync);
check_events([{sync_event, #wx{event=#wxPaint{}}, Obj}|Rest], Async, Sync) ->
    ?mt(wxPaintEvent, Obj),
    check_events(Rest, Async, Sync+1);
check_events([], Async, Sync) ->
    case Async > 0 of  %% Test sync explictly
	true -> ok;
	false -> {Async, Sync}
    end.

flush() ->
    flush([], 500).

flush(Acc, Wait) ->
    receive
	Msg -> flush([Msg|Acc], Wait div 10)
    after Wait ->
	    lists:reverse(Acc)
    end.
