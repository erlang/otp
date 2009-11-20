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
%%%   Description:      Code for pw, the window controlling part of the table tool.
%%%
%%%*********************************************************************


-module(tv_pw).



-export([pw/1]).




-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pw_int_def.hrl").







%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      pw.
%%
%% Return Value:  None.
%%
%% Description:   Process controlling the graphical window, as well as the 
%%                menubuttons.
%%
%% Parameters:    None.
%%======================================================================



pw(Master) ->
    process_flag(trap_exit, true),
    ProcVars = #process_variables{master_pid = Master},
    blocked(ProcVars).









%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





%%======================================================================
%% Function:      blocked.
%%
%% Return Value:  None.
%%
%% Description:   When started or explicitly blocked, pw enters this state,
%%                where nothing is performed until the module explicitly is 
%%                deblocked.
%%
%% Parameters:    
%%======================================================================


blocked(ProcVars) ->
    receive
	Msg ->
	    case Msg of
		#pw_deblock{} ->
		    deblock(Msg, ProcVars);
		_Other ->
		    blocked(ProcVars)
	    end
    end.
    








%%======================================================================
%% Function:      deblocked.
%%
%% Return Value:  None.
%%
%% Description:   When deblocked, a window shall be created according to 
%%                specification received in pw_deblock message.
%%
%% Parameters:    Rec:  received pw_deblock message.
%%======================================================================



deblock(Msg, ProcVars) ->
    #process_variables{window_params = WinP,
		       menu_params   = MenuP}  = ProcVars,

    NewWinP  = ?WIN_FUNC_FILE:create_window(Msg, WinP),
    NewMenuP = ?WIN_FUNC_FILE:create_menubar(NewWinP, MenuP),

    Sender = Msg#pw_deblock.sender,
    Sender ! #pw_deblock_cfm{sender = self(), 
			     win_id = NewWinP#window_params.window_id
			    },

    NewProcVars = ProcVars#process_variables{window_params = NewWinP,
					     menu_params   = NewMenuP
					    },
    deblocked_loop(NewProcVars).
    
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


deblocked_loop(ProcVars) ->
    receive
	Msg ->
	    case Msg of

		{gs, Id, Event, Data, Args} ->
		    NewProcVars = gs_messages({Id, Event, Data, Args}, ProcVars),
		    deblocked_loop(NewProcVars);
		
		   % Messages from pc!
		#pw_select_menu{menu=Menu} ->
		    gs:config(Menu, [{select,true}]),
		    deblocked_loop(ProcVars);

		#pw_create_menu{} ->
		    NewProcVars = ?WIN_FUNC_FILE:create_menu(Msg, ProcVars),
		       % Send confirmation...
		    Sender = Msg#pw_create_menu.sender,
		    Sender ! #pw_create_menu_cfm{sender = self()},
		    deblocked_loop(NewProcVars);

		#pw_set_window_title{win_title = WinTitle} ->
		    WinP = ProcVars#process_variables.window_params,
		    gs:config(WinP#window_params.window_id, [{title, "[TV]   " ++ WinTitle}]),
		    NewWinP = WinP#window_params{window_title = WinTitle},
		    NewProcVars = ProcVars#process_variables{window_params = NewWinP},
		    deblocked_loop(NewProcVars);

		#pw_deblock{} ->
		    deblock(Msg, ProcVars);

	           % Exit signals!
		{'EXIT', Pid, Reason} ->
		    MasterPid = ProcVars#process_variables.master_pid,
		    exit_signals({Pid, Reason}, MasterPid),
		    deblocked_loop(ProcVars);

		_Other ->
		    deblocked_loop(ProcVars)

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


exit_signals(Exit_info, MasterPid) ->
    case Exit_info of
	{MasterPid, _Reason} ->        % When from master, just quit!
	    exit(normal);
	_Other ->
	    done
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


gs_messages(Msg, ProcVars) ->
    MasterPid = ProcVars#process_variables.master_pid,
    case Msg of

	{_Id, click, Data, _Args} ->
	    MasterPid ! #pc_menu_msg{sender = self(),
				     data   = Data},
	    ProcVars;

        {_Win, keypress, _Data, [Key, _ , _, 1 | _T]} ->
	    MenuP        = ProcVars#process_variables.menu_params,
	    ShortcutList = MenuP#menu_params.shortcuts,
	    send_shortcut_data(Key, ShortcutList, MasterPid),
	    ProcVars;

	Msg0 = {Win, configure, _, _} ->
	    {Win, configure, _, [W, H | _T]} = flush_msgs(Msg0),
	    WinP = ProcVars#process_variables.window_params,
	    #window_params{window_id         = WindowId,
			   min_window_width  = MinAllowedWidth,
			   min_window_height = MinAllowedHeight} = WinP,
	    FinalWidth  = ?COMM_FUNC_FILE:max(W, MinAllowedWidth),
	    FinalHeight = ?COMM_FUNC_FILE:max(H, MinAllowedHeight),
	    ?WIN_FUNC_FILE:resize_window(WindowId, FinalWidth, FinalHeight),
	    MasterPid ! #pc_win_conf{sender = self(),
				     width  = FinalWidth, 
				     height = FinalHeight},
	    NewWinP = WinP#window_params{window_width  = FinalWidth,
					 window_height = FinalHeight
					},
	    ProcVars#process_variables{window_params = NewWinP};

	{_Win, destroy, _Data, _Args} ->
	    exit(normal);

	_Other ->
	    ProcVars
    end.

flush_msgs(Msg0 = {Win, Op, _, _}) ->
    receive {gs, Win,Op,D,P} ->
	    flush_msgs({Win,Op,D,P})
    after 200 ->
	    Msg0
    end.

send_shortcut_data(_Key, [], _MasterPid) ->
    done;
send_shortcut_data(Key, ShortcutList, MasterPid) ->
    case lists:keysearch(Key, 1, ShortcutList) of
	{value, {Key, Data}} ->
	    MasterPid ! #pc_menu_msg{sender = self(),
				     data   = Data};
	false ->
	    done
    end.






    
    
    




    

