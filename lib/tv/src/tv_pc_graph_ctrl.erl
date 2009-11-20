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
-module(tv_pc_graph_ctrl).



-export([create_menu/4, win_conf/2]).


-include("tv_int_msg.hrl").
-include("tv_pc_int_def.hrl").








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


create_menu(PwPid, MenuTitle, TitleAccPos, MenuList) ->
    PwPid ! #pw_create_menu{sender        = self(),
			    menutitle     = MenuTitle,
			    title_acc_pos = TitleAccPos,
			    menulist      = MenuList
			   },
    receive 
	#pw_create_menu_cfm{} ->
	    done
    after 10000 ->
	    exit(error)
    end.
    








%%======================================================================
%% Function:      win_conf.
%%
%% Return Value:  None.
%%
%% Description:   Configures all objects in the window according to new coordinates.
%%
%% Parameters:    
%%======================================================================


win_conf(Msg, ProcVars) ->
    #pc_win_conf{width  = NewWidth, 
		 height = NewHeight}  = Msg,

    #process_variables{pd_pid        = PdPid,
		       window_params = WinP}  = ProcVars,
    
    #window_params{window_width  = OldWindowWidth,
		   window_height = OldWindowHeight}  = WinP,
		   

    case {NewWidth, NewHeight} of
	{OldWindowWidth, OldWindowHeight} ->
	    ProcVars;
	_Other ->
	    PdPid ! #pd_win_conf{sender = self(),
				 width  = NewWidth,
				 height = NewHeight
				},
	    NewWinP = WinP#window_params{window_width  = NewWidth,
					 window_height = NewHeight},
	    
	    ProcVars#process_variables{window_params = NewWinP}
    end.












