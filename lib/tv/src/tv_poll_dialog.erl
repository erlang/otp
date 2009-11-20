%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%%*********************************************************************
%%% 
%%%   Description:      Code for the "set poll interval" dialog with the user.
%%%
%%%*********************************************************************

-module(tv_poll_dialog).



-export([start/1, init/2]).



-include("tv_int_msg.hrl").



-define(WINDOW_WIDTH, 305).
-define(WINDOW_HEIGHT, 185).

-define(DEFAULT_BG_COLOR, {217, 217, 217}).






%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************



%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


start(Pos) ->
    Pid = self(),
    ProcPid = spawn_link(?MODULE, init, [Pid, Pos]),
    receive_answer(ProcPid).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


init(Pid, Pos) ->
    process_flag(trap_exit, true),
    {ScalePos, ScaleRange, Poll, Color} = case Pos of
				       infinity ->
					   {0, {20, 20}, false, {255, 255, 255}};
				       _Other ->
					   {Pos, {20, 300}, true, {0, 0, 0}}
				   end,
    S = gs:start(),
    Win = gs:window(S, [{width, ?WINDOW_WIDTH},
			{height, ?WINDOW_HEIGHT},
			{bg, ?DEFAULT_BG_COLOR},
			{title, "[TV]   Set Poll Interval"},
			{configure, true},
			{destroy, true}
		       ]),

    NoPollBtn = gs:radiobutton(Win, [{height, 30}, 
				     {width, 143},
				     {x, 10},
				     {y, 10},
				     {bg, ?DEFAULT_BG_COLOR},
				     {fg, {0, 0, 0}},
				     {value, no_poll},
				     {label, {text, "Manual Polling"}},
				     {select, not(Poll)}
				    ]),

    PollBtn = gs:radiobutton(Win, [{height, 30}, 
				   {width, 163},
				   {x, 10},
				   {y, 60},
				   {bg, ?DEFAULT_BG_COLOR},
				   {fg, {0, 0, 0}},
				   {value, poll},
				   {label, {text, "Automatic Polling"}},
				   {select, Poll}
				  ]),

    Lbl = gs:label(Win, [{label, {text, "Poll Interval (seconds):"}},
			 {align, center},
			 {bg, ?DEFAULT_BG_COLOR},
			 {fg, Color},
			 {width, 183},
			 {height, 30},
			 {x, 10},
			 {y, 100}
			]),
    
    Scale = gs:scale(Win, [{bg, ?DEFAULT_BG_COLOR},
			   {fg, Color},
			   {orient, horizontal},
			   {range, ScaleRange},
			   {pos, ScalePos},
			   {width, 285},
			   {height, 50},
			   {x, 10},
			   {y, 130}
			  ]),
    
    OkBtn = gs:button(Win, [{label, {text, "OK"}},
			    {bg, ?DEFAULT_BG_COLOR},
			    {fg, {0, 0, 0}},
			    {align, center},
			    {width, 60},
			    {height, 30},
			    {x, 230},
			    {y, 10}
			   ]),

    CancelBtn = gs:button(Win, [{label, {text, "Cancel"}},
				{bg, ?DEFAULT_BG_COLOR},
				{fg, {0, 0, 0}},
				{align, center},
				{width, 60},
				{height, 30},
				{x, 230},
				{y, 60}
			       ]),
    
    gs:config(Win, {map, true}),
    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, CancelBtn, Poll, Pos).
    






%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


receive_answer(ProcPid) ->
    receive_answer(ProcPid, undefined, undefined, undefined, undefined).









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


receive_answer(ProcPid, DataReqMsg, WinConfMsg, MarkedRowMsg, SubsetMsg) ->
    receive Msg ->
	    case Msg of 

		{browser, ProcPid, cancel} ->
		    PcPid = self(),
		    PcPid ! DataReqMsg,
		    PcPid ! WinConfMsg,
		    PcPid ! MarkedRowMsg,
		    PcPid ! SubsetMsg,   
		    cancel;

		{browser, ProcPid, {true, PollInterval}} ->
		    PcPid = self(),
		    PcPid ! DataReqMsg,
		    PcPid ! WinConfMsg,
		    PcPid ! MarkedRowMsg,
		    PcPid ! SubsetMsg,   
		    PollInterval;

		{browser, ProcPid, {false, _Pollinterval}} ->
		    PcPid = self(),
		    PcPid ! DataReqMsg,
		    PcPid ! WinConfMsg,
		    PcPid ! MarkedRowMsg,
		    PcPid ! SubsetMsg,   
		    infinity;

		#pc_data_req{} ->
		    receive_answer(ProcPid, Msg, WinConfMsg, MarkedRowMsg, SubsetMsg);

		#pc_win_conf{} ->
		    receive_answer(ProcPid, DataReqMsg, Msg, MarkedRowMsg, SubsetMsg);

		#pc_marked_row{} ->
		    receive_answer(ProcPid, DataReqMsg, WinConfMsg, Msg, SubsetMsg);

		#dbs_subset{} ->
		    receive_answer(ProcPid, DataReqMsg, WinConfMsg, MarkedRowMsg, Msg);

		#pc_menu_msg{data = exit_button} ->
		    self() ! Msg,
		    cancel;

		#pc_set_sorting_mode{sender = Sender} ->
		    Sender ! #pd_ignore{sender = self()},
		    ProcPid ! raise_and_beep,
		    receive_answer(ProcPid, DataReqMsg, WinConfMsg, MarkedRowMsg, SubsetMsg);

		{'EXIT', _Sender, _Reason} ->
		    self() ! Msg,
		    cancel;

		_Other ->
		    ProcPid ! raise_and_beep,
		    receive_answer(ProcPid, DataReqMsg, WinConfMsg, MarkedRowMsg, SubsetMsg)
	    end
    end.
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, CancelBtn, Poll, Pos) ->
    receive
	{gs, Scale, click, _, [NewPos | _]} ->
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, Poll, NewPos);

	{gs, NoPollBtn, click, _, _} ->
	    gs:config(Lbl, [{fg, {255, 255, 255}}]),
	    gs:config(Scale, [{fg, {255, 255, 255}}, {pos, 0}, {range, {20, 20}}]),
	    receive
		{gs, Scale, click, _, _} ->
		    done
	    after 500 ->
		    done
	    end,
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, false, Pos);

	{gs, PollBtn, click, _, _} ->
	    gs:config(Lbl, [{fg, {0, 0, 0}}]),
	    gs:config(Scale, [{fg, {0, 0, 0}}, {pos, Pos}, {range, {20, 300}}]),
	    receive
		{gs, Scale, click, _, _} ->
		    done
	    after 500 ->
		    done
	    end,
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, true, Pos);

	{gs, OkBtn, click, _, _} ->
	    Pid ! {browser, self(), {Poll, Pos}};

	{gs, CancelBtn, click, _, _} ->
	    Pid ! {browser, self(), cancel};

	{gs, _, destroy, _, _} ->
	    Pid ! {browser, self(), cancel};


	{gs, Win, configure, _, _} ->
	    gs:config(Win, [{width, ?WINDOW_WIDTH},
			    {height, ?WINDOW_HEIGHT}
			   ]),
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, Poll, Pos);


	raise_and_beep ->
	    gs:config(Win, [raise,
			   beep]),
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, Poll, Pos);


	{'EXIT', _Sender, _Reason} ->
	    Pid ! {browser, self(), cancel};


	_Other ->
	    io:format("Poll dialog received message ~w ~n", [_Other]),
	    browser_loop(Pid, Win, NoPollBtn, PollBtn, Lbl, Scale, OkBtn, 
			 CancelBtn, Poll, Pos)

   end.

