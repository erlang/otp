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
%%% File    : wx_event_SUITE.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Test event handling as much as possible
%%% Created :  3 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_event_SUITE).
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([connect/1, disconnect/1, disconnect_cb/1, connect_msg_20/1, connect_cb_20/1,
         mouse_on_grid/1, spin_event/1, connect_in_callback/1, recursive/1,
         dialog/1, char_events/1, callback_clean/1, handler_clean/1
        ]).

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
    [connect, disconnect, disconnect_cb, connect_msg_20, connect_cb_20,
     mouse_on_grid, spin_event, connect_in_callback, recursive,
     dialog, char_events, callback_clean, handler_clean
    ].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

  
%% The test cases

%% Test that the various options to connect work as expected.
connect(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
connect(Config) ->
    ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(wx:null(), 1, "Event Testing")),
    Panel = ?mt(wxPanel, wxPanel:new(Frame)),
    Window = wxWindow:new(Panel, -1),

    Tester = self(),
    CB = fun(#wx{event=#wxSize{},userData=UserD}, SizeEvent) ->
		 ?mt(wxSizeEvent, SizeEvent),
		 Tester ! {got_size, UserD}
	 end,
    
    ?m(ok, wxEvtHandler:connect(Panel, size,[{skip, true},{userData, panel}])),
    ?m(ok, wxEvtHandler:connect(Panel, size,[{callback,CB},{userData, panel}])),

    ?m({'EXIT', {{badarg,_},_}}, 
       wxEvtHandler:connect(Panel, there_is_no_such_event)),

    ?m({'EXIT', {{badarg,_},_}}, 
       wxEvtHandler:connect(Panel, there_is_no_such_event, [{callback,CB}])),

    ?m(ok, wxWindow:connect(Window, size,[{callback,CB},{userData, window}])),
    ?m(ok, wxWindow:connect(Window, size,[{skip,true},{userData, window}])),

    %% For trivial side effect free callbacks, can deadlock easily otherwise
    CB1 = fun(_,_) -> Tester ! {got_size, nospawn_cb} end,
    ?m(ok, wxWindow:connect(Frame,  size, [{callback,{nospawn, CB1}}])),
    ?m(ok, wxFrame:connect(Frame,  size, [{skip, true}])),
    ?m(true, wxFrame:show(Frame)),

    wxWindow:setSize(Panel, {200,100}),
    wxWindow:setSize(Window, {200,100}),

    get_size_messages(Frame, [frame, panel_cb, window_cb, window, nospawn_cb]),
    
    wx_test_lib:wx_destroy(Frame, Config).

get_size_messages(_, []) -> ok;    
get_size_messages(Frame, Msgs) ->
    receive 
	#wx{obj=Frame,event=#wxSize{}} ->  %% ok
	    get_size_messages(Frame, lists:delete(frame, Msgs));
	#wx{userData=window, event=#wxSize{}} ->
	    ?m(true, lists:member(window_cb, Msgs)),	   
	    get_size_messages(Frame, lists:delete(window, Msgs));
	#wx{userData=panel, event=#wxSize{}} ->
	    ?m(true, lists:member(panel, Msgs)),	   
	    get_size_messages(Frame, lists:delete(panel, Msgs));
	{got_size,window} ->
	    ?m(false, lists:member(window, Msgs)),
	    get_size_messages(Frame, lists:delete(window_cb, Msgs));
	{got_size,panel} -> 
	    get_size_messages(Frame, lists:delete(panel_cb, Msgs));
	{got_size,nospawn_cb} ->
	    get_size_messages(Frame, lists:delete(nospawn_cb, Msgs));
	Other ->
	    ?error("Got unexpected msg ~p ~p~n", [Other,Msgs])
    after 1000 ->
	    ?error("Timeout ~p~n", [Msgs])
    end.

disconnect(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
disconnect(Config) ->
    ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(wx:null(), 1, "Event Testing")),
    Panel = ?mt(wxPanel, wxPanel:new(Frame)),

    Tester = self(),
    CB = fun(#wx{event=#wxSize{},userData=UserD}, SizeEvent) ->
		 ?mt(wxSizeEvent, SizeEvent),
		 Tester ! {got_size, UserD}
	 end,
    ?m(ok, wxFrame:connect(Frame,  close_window)),
    ?m(ok, wxFrame:connect(Frame,  size)),
    ?m(ok, wxEvtHandler:connect(Panel, size,[{skip, true},{userData, panel}])),
    ?m(ok, wxEvtHandler:connect(Panel, size,[{callback,CB},{userData, panel}])),

    ?m(true, wxFrame:show(Frame)),

    wxWindow:setSize(Panel, {200,100}),    
    get_size_messages(Frame, [frame, panel_cb]),
    wx_test_lib:flush(),

    ?m(true, wxEvtHandler:disconnect(Panel, size, [{callback,CB}])),
    ?m(ok, wxWindow:setSize(Panel, {200,101})),
    get_size_messages(Frame, [panel]),
    timer:sleep(1000),
    wx_test_lib:flush(),

    ?m(false, wxEvtHandler:disconnect(Panel, non_existing_event_type)),
    ?m(true, wxEvtHandler:disconnect(Panel, size)),
    ?m(ok, wxWindow:setSize(Panel, {200,102})),
    timer:sleep(1000),
    ?m([], wx_test_lib:flush()),

    wx_test_lib:wx_destroy(Frame, Config).



disconnect_cb(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
disconnect_cb(Config) ->
    ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(wx:null(), 1, "Event Testing")),
    Panel = ?mt(wxPanel, wxPanel:new(Frame)),

    Tester = self(),
    CB = fun(#wx{event=#wxSize{},userData=UserD}, SizeEvent) ->
		 ?mt(wxSizeEvent, SizeEvent),
		 wxEvtHandler:disconnect(Frame, close_window),
		 Tester ! {got_size, UserD}
	 end,
    ?m(ok, wxFrame:connect(Frame,  close_window)),
    ?m(ok, wxFrame:connect(Frame,  size)),
    ?m(ok, wxEvtHandler:connect(Panel, size, [{callback,CB},{userData, panel}])),

    ?m(true, wxFrame:show(Frame)),

    wxWindow:setSize(Panel, {200,100}),
    get_size_messages(Frame, [frame, panel_cb]),
    wx_test_lib:flush(),

    wx_test_lib:wx_destroy(Frame, Config).

%% Test that the msg events are forwarded as supposed to 
connect_msg_20(TestInfo) 
  when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
connect_msg_20(Config) ->
    ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(wx:null(), 1, "Event 20 Testing")),
    Tester = self(),
    Env = wx:get_env(),
    
    EvtHandler = fun() ->
			 wx:set_env(Env),
			 wxFrame:connect(Frame,size,[{skip,true}]),
			 Tester ! initiated,
			 receive #wx{obj=Frame,event=#wxSize{}} ->
				 Tester ! got_it
			 end
		 end,
    Msgs = [begin spawn_link(EvtHandler), got_it end|| _ <- lists:seq(1,20)],

    ?m_multi_receive(lists:duplicate(20, initiated)),    
    ?m(true, wxFrame:show(Frame)),

    ?m_multi_receive(Msgs),
    wx_test_lib:wx_destroy(Frame, Config).

%% Test that the callbacks works as msgs
connect_cb_20(TestInfo) 
  when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
connect_cb_20(Config) ->
    ?mr(wx_ref, wx:new()),
    Frame = ?mt(wxFrame, wxFrame:new(wx:null(), 1, "Event 20 Testing")),
    Tester = self(),
    Env = wx:get_env(),
    
    wxFrame:connect(Frame,size,[{callback, 
				 fun(#wx{event=#wxSize{}},_SizeEv) -> 
					 Tester ! main_got_it
				 end}]),

    EvtHandler = fun() ->
			 wx:set_env(Env),
			 Self = self(),
			 CB = fun(#wx{event=#wxSize{}}, 
				  WxSizeEventObj) ->
				      wxEvent:skip(WxSizeEventObj),
				      Tester ! got_it,
				      Self ! quit
			      end,
			 wxFrame:connect(Frame,size,[{callback, CB}]),
			 Tester ! initiated,
			 receive quit -> ok
			 end
		 end,
    Msgs = [begin spawn_link(EvtHandler), got_it end|| _ <- lists:seq(1,20)],
    
    ?m_multi_receive(lists:duplicate(20, initiated)),
    ?m(true, wxFrame:show(Frame)),
    
    ?m_multi_receive(Msgs),
    ?m_receive(main_got_it),

    wx_test_lib:wx_destroy(Frame, Config).
   

mouse_on_grid(TestInfo) 
  when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
mouse_on_grid(Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame"),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    
    Grid = wxGrid:new(Panel, ?wxID_ANY),
    wxGrid:createGrid(Grid, 10, 10, []),
    wxSizer:add(Sizer, Grid, [{proportion, 1}]),
        
    wxWindow:connect(Panel, motion),
    wxWindow:connect(Panel, middle_down), 

    %% Undocumented function
    GridWindow = ?mt(wxWindow, wxGrid:getGridWindow(Grid)),
    wxWindow:connect(GridWindow, motion),
    wxWindow:connect(GridWindow, middle_down),

    wxWindow:setSizerAndFit(Panel, Sizer),
    wxFrame:show(Frame),
    
    wx_test_lib:wx_destroy(Frame, Config).


spin_event(TestInfo) 
  when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
spin_event(Config) ->
    Wx = ?mr(wx_ref, wx:new()),

    %% Spin events and scrollEvent share some events id's
    %% test that they work

    Frame = wxFrame:new(Wx, ?wxID_ANY, "Spin Events"),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    HSz = wxBoxSizer:new(?wxHORIZONTAL),

    SB = wxSpinButton:new(Panel, [{id, 100}]),
    wxSizer:add(HSz, SB, []),
    wxSpinButton:connect(SB, spin),
    wxSpinButton:connect(SB, spin_up),
    wxSpinButton:connect(SB, spin_down),

    SC = wxSpinCtrl:new(Panel, [{id, 101}, {min, -12}, {max, 12}, 
				{value, "-3"}, {initial, 3}, 
				{style, ?wxSP_ARROW_KEYS bor ?wxSP_WRAP}]),
    wxSpinCtrl:connect(SC, command_spinctrl_updated),
    wxSizer:add(HSz, SC, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, HSz, [{proportion, 0},{flag, ?wxEXPAND}]),
    
    SL = wxSlider:new(Panel, 102, 57, 22, 99),
    wxSlider:connect(SL, scroll_thumbtrack),
    wxSlider:connect(SL, scroll_lineup),
    wxSlider:connect(SL, scroll_linedown),
    wxSizer:add(Sizer, SL, [{proportion, 0},{flag, ?wxEXPAND}]),
       
    wxWindow:setSizerAndFit(Panel, Sizer),
    wxFrame:show(Frame),
    wx_test_lib:flush(),

%% Set value does not generate a spin event...
%%     wxSpinButton:setValue(SB, 7),
%%     ?m_receive(#wx{id=100, event=#wxSpin{type=spin}}),
%%     wxSpinCtrl:setValue(SC, 8),
%%     ?m_receive(#wx{id=101, event=#wxSpin{type=command_spinctrl_updated}}),
%%     wxSlider:setValue(SL, 29),
%%     ?m_receive(#wx{id=102, event=#wxScroll{}}),

    wx_test_lib:wx_destroy(Frame, Config).


%% Test that we can connect to events from inside a callback fun
%% This is needed for example inside a callback that does a wxWindow:popupMenu/2
connect_in_callback(TestInfo) 
  when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
connect_in_callback(Config) ->
    Wx = ?mr(wx_ref, wx:new()),
    %% wx:debug([driver,trace]),
    %% io:format("gdb -p ~s~n",[os:getpid()]),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Connect in callback"),
    Panel = wxPanel:new(Frame, []),

    Tester = self(),

    %% Connect in callbacks works different in 2.9
    %% such that new events are not fired until the previous
    %% callback have returned.

    %% That means that a callback can not wait for other events
    %% in receive since they will not come.
    %% It also means that you can not attach a new callback directly from
    %% the callback since that callback will be removed when the temporary
    %% process that executes the outer callback (may) die(s) before the callback
    %% is invoked

    %% Thus connect in callbacks needs to done in a another process, and
    %% not in the fun directly
    Env = wx:get_env(),
    TestWindow =
	fun() ->
		wx:set_env(Env),
		Me = self(),
		F1 = wxFrame:new(Frame, ?wxID_ANY, "Frame size event"),
		wxFrame:connect(F1,size,[{callback,
					  fun(_,_) ->
						  io:format("CB2 got size~n",[]),
						  Me ! {continue, F1}
					  end}]),
		wxWindow:show(F1),
		receive
		    {continue, F1} ->
			true = wxFrame:disconnect(F1, size),
			Tester ! {continue, F1}
		end
	end,
    wxFrame:connect(Frame,show,
		    [{callback,
		      fun(#wx{event=#wxShow{}},_SizeEv) ->
			      io:format("Frame got size~n",[]),
			      spawn(TestWindow)
		      end}]),
    wxPanel:connect(Panel,size,
		    [{callback,
		      fun(#wx{event=#wxSize{}},_SizeEv) ->
			      io:format("Panel got size~n",[]),
			      spawn(fun() ->
					    wx:set_env(Env),
					    F1 = wxFrame:new(Frame, ?wxID_ANY,
							     "Panel size event"),
					    wxFrame:connect(F1,size),
					    wxWindow:show(F1),
					    receive
						#wx{event=#wxSize{}} ->
						    io:format("All Fine ~n",[]),
						    wxFrame:destroy(F1)
					    end
				    end)
		      end}]),
    wxFrame:show(Frame),

    ok = receive {continue, F1} -> wxFrame:destroy(F1)
	 after 5000 -> timeout end,
    wx_test_lib:flush(),
    wx_test_lib:wx_destroy(Frame, Config).

%% Test that event callback which triggers another callback works
%% i.e. the callback invoker in driver will recurse
recursive(TestInfo)   when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
recursive(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Connect in callback"),
    Panel = wxPanel:new(Frame, []),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    Ctrl1 = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {300, -1}}]),
    Ctrl2 = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {300, -1}}]),
    wxSizer:add(Sz, Ctrl1, [{proportion, 1},{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, Ctrl2, [{proportion, 1},{flag, ?wxEXPAND}]),
    wxWindow:setSizerAndFit(Panel, Sz),

    CB1 =  fun(#wx{event=#wxCommand{cmdString=String}}, _) ->
		   io:format(" CB1: ~s~n",[String]),
		   wxTextCtrl:setValue(Ctrl2, io_lib:format("from CB1 ~s", [String]))
	   end,
    CB2 =  fun(#wx{event=#wxCommand{cmdString=String}}, _) ->
		   io:format("    CB2: ~s~n",[String]),
		   ok
	   end,
    wxTextCtrl:connect(Ctrl1, command_text_updated, [{callback,CB1}]),
    wxTextCtrl:connect(Ctrl2, command_text_updated, [{callback,CB2}]),
    wxFrame:connect(Frame, size,
		    [{callback,
		      fun(#wx{event=#wxSize{size=Size}}, _) ->
			      io:format("Size init: ~s ~n",[wxTextCtrl:getValue(Ctrl2)]),
			      wxTextCtrl:setValue(Ctrl1, io_lib:format("Size ~p", [Size])),
			      io:format("Size done: ~s ~n",[wxTextCtrl:getValue(Ctrl2)])
		      end}]),
    wxFrame:show(Frame),
    wx_test_lib:flush(),

    wx_test_lib:wx_destroy(Frame, Config).


dialog(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
dialog(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Testing"),
    wxFrame:show(Frame),
    Env = wx:get_env(),
    Tester = self(),
    PD = wxProgressDialog:new("Dialog","Testing",
			      [%%{parent, Frame},
			       {maximum,101},
			       {style, ?wxPD_SMOOTH bor ?wxPD_AUTO_HIDE}]),
    Forward = fun(#wx{event=#wxInitDialog{}}, Ev) ->
		      ?mt(wxInitDialogEvent, Ev),
		      io:format("Heyhoo~n", []),
		      wxEvent:skip(Ev),
		      Tester ! {progress_dialog,PD}
	      end,
    wxDialog:connect(PD, init_dialog, [{callback, Forward}]),
    Recurse = fun(Recurse, N) ->
		      true = wxProgressDialog:update(PD, min(N,100)),
		      timer:sleep(5),
		      Recurse(Recurse,N+1)
	      end,
    Run = fun() ->
		  wx:set_env(Env),
		  Recurse(Recurse, 0)
	  end,
    Worker = spawn_link(Run),
    timer:sleep(500),
    io:format("Got ~p~n", [wx_test_lib:flush()]),
    unlink(Worker),
    wxProgressDialog:destroy(PD),
    wx_test_lib:wx_destroy(Frame, Config).



char_events(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
char_events(Config) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Press any key"),
    Panel = wxPanel:new(Frame, []),
    wxFrame:connect(Frame, enter_window, [{callback, fun(_,_) ->
							     io:format("Set focus~n"),
							     wxWindow:setFocus(Panel)
						     end}]),
    KeyEvent = fun(Ev,Obj) -> io:format("Got ~p~n",[Ev]), wxEvent:skip(Obj) end,
    [wxWindow:connect(Panel, Types, [{callback,KeyEvent}])
     || Types <- [key_down, key_up, char]],
    wxWindow:connect(Frame, char_hook, [{callback,KeyEvent}]),

    wxFrame:show(Frame),
    wx_test_lib:flush(),

    wx_test_lib:wx_destroy(Frame, Config).

callback_clean(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
callback_clean(Config) ->
    %% Be sure that event handling are cleanup up correctly and don't keep references to old
    %% fun's and event listeners
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Frame Window"),
    wxFrame:show(Frame),

    %% wx:debug([verbose,driver]),
    Dlg = wxDialog:new(Frame, ?wxID_ANY, "Testing"),
    Panel = wxPanel:new(Dlg, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Button = wxButton:new(Panel, 600, [{label, "Foobar"}]),
    wxSizer:add(Sizer, Button, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, wxDialog:createStdDialogButtonSizer(Dlg,?wxOK bor ?wxCANCEL)),
    wxDialog:setSizerAndFit(Dlg, Sizer),

    Env = wx:get_env(),
    SetupEventHandlers =
	fun() ->
		wx:set_env(Env),
		Me = self(),
		Print = fun(#wx{id=ID, event=#wxCommand{}},Ev) ->
				io:format("~p Clicked ~p~n", [self(), ID]),
				Me ! #wx{event=#wxClose{}},
				wxEvent:skip(Ev, [{skip, true}]);
			   (#wx{id=ID, event=#wxClose{}},Ev) ->
				io:format("~p Closed ~p~n", [self(), ID]),
				wxEvent:skip(Ev, [{skip, true}])
			end,

		wxDialog:connect(Dlg, command_button_clicked,[{callback,Print}]),
		wxDialog:connect(Dlg, close_window, [{skip, true}])
	end,
    ?m({[],[],[]}, white_box_check_event_handlers()),
    Pid = spawn_link(fun() ->
    			     SetupEventHandlers(),
    			     receive #wx{event=#wxClose{}} -> ok;
    				     remove -> ok
    			     end
    		     end),
    timer:sleep(500), %% Give it time to remove it
    ?m({[{Pid,_}],[_],[_]}, white_box_check_event_handlers()),

    Pid ! remove,
    timer:sleep(500), %% Give it time to remove it
    ?m({[],[],[]}, white_box_check_event_handlers()),

    SetupEventHandlers(),
    ?m({[{_,_}],[_],[_]}, white_box_check_event_handlers()),

    wxDialog:show(Dlg),
    wx_test_lib:wx_close(Dlg, Config),
    wxDialog:destroy(Dlg),
    timer:sleep(500), %% Give it time to remove it
    ?m({[],[],[]}, white_box_check_event_handlers()),

    wx_test_lib:flush(),
    io:format("**Deleting Frame**~n",[]),
    wx_test_lib:wx_destroy(Frame, Config).
    %% timer:sleep(infinity),
    %% ok.


white_box_check_event_handlers() ->
    {_,_,Server,_} = wx:get_env(),
    {status, _, _, [Env, _, _, _, Data]} = sys:get_status(Server),
    [_H, _data, {data, [{_, Record}]}] = Data,
    {state, _Port1, _Port2, Users, [], CBs, _Next} = Record,
    {[{Pid, Evs} ||
	 {Pid, {user, Evs}} <- gb_trees:to_list(Users),
	 Evs =/= []], %% Ignore empty
     gb_trees:to_list(CBs),
     [Funs || Funs = {Id, {Fun,_}} <- Env, is_integer(Id), is_function(Fun)]
    }.

handler_clean(TestInfo) when is_atom(TestInfo) ->
    wx_test_lib:tc_info(TestInfo);
handler_clean(_Config) ->
    wx:new(),
    Init = fun() -> create_window() end,
    Frame1 = wx_obj_test:start([{init, Init}]),
    ?mt(wxFrame, Frame1),
    wxWindow:show(Frame1),
    timer:sleep(500),
    ?m([_|_], lists:sort(wx_test_lib:flush())),
    ?m(ok, wx_obj_test:stop(Frame1)),
    ?m([{terminate,normal}], lists:sort(wx_test_lib:flush())),

    Terminate = fun({Frame,_}) -> wxWindow:destroy(Frame) end,
    Frame2 = wx_obj_test:start([{init, Init}, {terminate, Terminate}]),
    wxWindow:show(Frame2),
    timer:sleep(500),
    ?m([_|_], lists:sort(wx_test_lib:flush())),
    ?m(ok, wx_obj_test:stop(Frame2)),
    ?m([{terminate,normal}], lists:sort(wx_test_lib:flush())),
    timer:sleep(104),
    ?m({[],[],[]}, white_box_check_event_handlers()),
    ?m(ok, wx:destroy()),
    ok.

create_window() ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Test wx_object", [{size, {500, 400}}]),
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Panel = wxPanel:new(Frame),
    wxSizer:add(Sz, Panel, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:connect(Frame, show),
    %% wxPanel:connect(Panel, paint, [callback, {userData, foobar}]),
    wxWindow:connect(Panel, size, [callback]),
    {Frame, {Frame, Panel}}.
