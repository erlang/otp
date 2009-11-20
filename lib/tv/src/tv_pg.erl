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
-module(tv_pg).



-export([pg/1]).


-include("tv_int_def.hrl").
-include("tv_pg_int_def.hrl").
-include("tv_pd_int_msg.hrl").








%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      pg.
%%
%% Return Value:  None.
%%
%% Description:   Process controlling the grid part of the display.
%%
%% Parameters:    None.
%%======================================================================


pg(ParentPid) ->
    process_flag(trap_exit, true),
    ProcVars = #process_variables{parent_pid = ParentPid},
    loop(ProcVars).







%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





%%======================================================================
%% Function:      loop.
%%
%% Return Value:  None.
%%
%% Description:   Eternal (well, almost) loop, receiving messages and 
%%                handling them.
%%
%% Parameters:    
%%======================================================================



loop(ProcVars) ->
    receive
	Msg ->
	    case Msg of
		
		
		#pg_data{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = update_grid_data(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_list_info{lists_as_strings=ListAsStr} ->
		    NewProcVars = tv_pg_gridfcns:handle_list_info(ListAsStr, ProcVars),
		    loop(NewProcVars);

		#pg_horizontal_scroll{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = scroll_grid_horizontally(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_remove_marks{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = tv_pg_gridfcns:remove_marks(ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_col_marked{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = mark_grid_col(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_row_marked{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = mark_grid_row(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_resize_grid_col{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = resize_grid_column(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_resize_grid{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = resize_grid(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);

		#pg_init_grid{} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = init_grid(Msg, ProcVars),
		    mark_nonbusy(GridId),
		    PdPid = ProcVars#process_variables.parent_pid,
		    PdPid ! #pg_ready{sender = self()},
		    loop(NewProcVars);

		{gs, Id, Event, Data, Args} ->
		    GridId = mark_busy(ProcVars),
		    NewProcVars = gs_messages({Id, Event, Data, Args}, ProcVars),
		    mark_nonbusy(GridId),
		    loop(NewProcVars);


		{'EXIT', Pid, Reason} ->
		    ParentPid = ProcVars#process_variables.parent_pid,
		    exit_signals({Pid, Reason}, ParentPid, ProcVars),
		    loop(ProcVars);

		_Other ->
		    loop(ProcVars)
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


exit_signals(ExitInfo, ParentPid, _ProcVars) ->
    case ExitInfo of
	{ParentPid, _Reason} ->
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

    case Msg of

	{Id, buttonpress, {gridcell, RealCol, RealRow, _FrameId}, [1 | _]} ->
	    NewProcVars = tv_pg_gridfcns:mark_cell_and_notify(Id, RealCol, 
							       RealRow, ProcVars),
	    NewProcVars;


	_OtherMessage ->
	    ProcVars

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


init_grid(Msg, ProcVars) ->
    #pg_init_grid{parent_id  = ParentId,
		  width      = Width,
		  height     = Height,
		  xpos       = Xpos,
		  ypos       = Ypos,
		  nof_rows   = NofRows,
		  row_height = RowHeight} = Msg,
    tv_pg_gridfcns:init_grid(ParentId, Width, Height, Xpos, Ypos, NofRows, 
			      RowHeight, ProcVars).








    
%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_grid(Msg, ProcVars) ->
    #pg_resize_grid{width  = Width,
		    height = Height}  = Msg,
    tv_pg_gridfcns:resize_grid(Width, Height, ProcVars).









    
%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_grid_column(Msg, ProcVars) ->
    #pg_resize_grid_col{real_col_no    = RealCol,
			virtual_col_no = VirtualCol,
			xdiff          = Xdiff}  = Msg,
    tv_pg_gridfcns:resize_grid_column(RealCol, VirtualCol, Xdiff, ProcVars).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


scroll_grid_horizontally(Msg, ProcVars) ->
    FirstColShown = ?COMM_FUNC_FILE:max(1, Msg#pg_horizontal_scroll.leftmost_virtual_col),
    tv_pg_gridfcns:scroll_grid_horizontally(FirstColShown, ProcVars).





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_grid_data(Msg, ProcVars) ->
    #pg_data{data            = Data,
	     first_row_shown = FirstRowShown} = Msg,
    tv_pg_gridfcns:update_grid_data(Data, FirstRowShown, ProcVars).










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_grid_col(Msg, ProcVars) ->
    #pg_col_marked{virtual_col = VirtualCol} = Msg,
    tv_pg_gridfcns:mark_col(VirtualCol, ProcVars).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_grid_row(Msg, ProcVars) ->
    #pg_row_marked{virtual_row = VirtualRow} = Msg,
    tv_pg_gridfcns:mark_row(VirtualRow, ProcVars).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_busy(ProcVars) ->
    GridP = ProcVars#process_variables.grid_params,
    GridId = GridP#grid_params.fg_frame,
    gs:config(GridId, [{cursor, busy}]),
    GridId.
    

    




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_nonbusy(GridId) ->
    gs:config(GridId, [{cursor, arrow}]).

