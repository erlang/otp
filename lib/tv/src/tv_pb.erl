%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(tv_pb).



-export([pb/1]).


-include("tv_int_def.hrl").
-include("tv_pd_int_msg.hrl").
-include("tv_pb_int_def.hrl").








%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      pb.
%%
%% Return Value:  None.
%%
%% Description:   Process controlling the grid buttons on the display.
%%
%% Parameters:    None.
%%======================================================================


pb(ParentPid) ->
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
		
		#pb_update_vbtns{} ->
		    NewProcVars = update_vbtns(Msg, ProcVars),
		    loop(NewProcVars);

		#pb_key_info{} ->
		    NewProcVars = update_keys(Msg, ProcVars),
		    loop(NewProcVars);

		#pb_update_hbtns{} ->
		    NewProcVars = update_hbtns(Msg, ProcVars),
		    loop(NewProcVars);

		#pb_set_sort_col{} ->
		    NewProcVars = set_sort_col(Msg, ProcVars),
		    loop(NewProcVars);

		#pb_remove_marks{} ->
		    NewProcVars = remove_marks(ProcVars),
		    loop(NewProcVars);

		#pb_init_btns{} ->
		    NewProcVars = init_btns(Msg, ProcVars),
		    loop(NewProcVars);

		{gs, Id, Event, Data, Args} ->
		    NewProcVars = gs_messages({Id, Event, Data, Args}, ProcVars),
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

	{Id, click, {hbtn, RealCol, VirtualCol}, _Args} ->
	    handle_col_marking(Id, RealCol, VirtualCol, ProcVars);

	{Id, buttonpress, {resbtn, RealCol, VirtualCol, Xpos}, [1 | _Tail]} ->
	    handle_col_resizing(Id, RealCol, VirtualCol, Xpos, ProcVars),
	    ProcVars;

	{_Id, click, {vbtn, RealRow, VirtualRow}, _Args} ->
	    handle_row_marking(RealRow, VirtualRow, ProcVars);

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


remove_marks(ProcVars) ->
    #process_variables{col_mark_params = ColMarkP,
		       row_mark_params = RowMarkP}  = ProcVars,

    #col_mark_params{col_btn_id          = BtnId,
		     virtual_col_marked  = VirtualCol,
		     virtual_sort_col    = SortCol}  = ColMarkP,
    
    case BtnId of 
	undefined ->
	    done;
	_AnyId ->
	    case VirtualCol of
		SortCol ->
		    gs:config(BtnId, [{bg, ?SORT_MARK_COLOR},
				      {fg, {0, 0, 0}}
				     ]);
		_Other ->
		    gs:config(BtnId, [{bg, ?DEFAULT_BG_COLOR},
				      {fg, {0, 0, 0}}
				     ])
	    end
    end,
    
    NewRowMarkP = RowMarkP#row_mark_params{virtual_row_marked = undefined,
					   real_row_marked    = undefined
					  },
    NewColMarkP = ColMarkP#col_mark_params{col_btn_id         = undefined,
					   virtual_col_marked = undefined
					  },
    ProcVars#process_variables{col_mark_params = NewColMarkP,
			       row_mark_params = NewRowMarkP
			      }.
    
				   
		    
		    
		    
    
    
    




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


handle_col_marking(BtnId, RealCol, VirtualCol, ProcVars) ->
    #process_variables{parent_pid      = PdPid,
		       col_mark_params = ColMarkP,
		       row_mark_params = RowMarkP}  = ProcVars,

    #col_mark_params{col_btn_id          = OldBtnId,
		     virtual_col_marked  = OldVirtualCol,
		     virtual_sort_col    = SortCol}  = ColMarkP,

    {ColMarked, NewColMarkP} = mark_col_btn(BtnId, OldBtnId, VirtualCol, 
					    OldVirtualCol, RealCol, SortCol, 
					    ColMarkP),

    PdPid ! #pb_col_marked{sender      = self(),
			   col_marked  = ColMarked,
			   real_col    = RealCol,
			   virtual_col = VirtualCol
			  },

    NewRowMarkP = RowMarkP#row_mark_params{virtual_row_marked = undefined,
					   real_row_marked    = undefined
					  },
    ProcVars#process_variables{col_mark_params = NewColMarkP,
			       row_mark_params = NewRowMarkP
			      }.

    




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


handle_row_marking(RealRow, VirtualRow, ProcVars) ->
    #process_variables{parent_pid      = PdPid,
		       col_mark_params = ColMarkP,
		       row_mark_params = RowMarkP}  = ProcVars,

    #col_mark_params{col_btn_id          = OldBtnId,
		     virtual_col_marked  = OldVirtualCol,
		     virtual_sort_col    = SortCol}  = ColMarkP,

    {_ColMarked, NewColMarkP} = mark_col_btn(OldBtnId, OldBtnId, OldVirtualCol, 
					     OldVirtualCol, undefined, SortCol, 
					     ColMarkP),

    #row_mark_params{virtual_row_marked = OldVirtualRow} = RowMarkP,

       % Check if row shall be marked or unmarked!
    {RowMarked, NewRowMarkP} = check_marked_row(VirtualRow, OldVirtualRow, RealRow,
						RowMarkP),

    PdPid ! #pb_row_marked{sender      = self(),
			   row_marked  = RowMarked,
			   real_row    = RealRow,
			   virtual_row = VirtualRow
			  },
    
    ProcVars#process_variables{row_mark_params = NewRowMarkP,
			       col_mark_params = NewColMarkP}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


%% Three cases: no button previously clicked, or same button clicked,
%% or some other button clicked.

check_marked_row(NewVirtRow, undefined, RealRow, RowMarkP) ->
       % No btn already pressed!
    {true, RowMarkP#row_mark_params{virtual_row_marked = NewVirtRow,
				    real_row_marked    = RealRow}};
check_marked_row(NewVirtRow, OldVirtRow, _RealRow, RowMarkP) when NewVirtRow =:= OldVirtRow ->
       % The button previously pressed has been pressed again!
    {false, RowMarkP#row_mark_params{virtual_row_marked = undefined,
				     real_row_marked    = undefined}};
check_marked_row(NewVirtRow, _OldVirtRow, RealRow, RowMarkP) ->
       % A new btn has been pressed!
    {true, RowMarkP#row_mark_params{virtual_row_marked = NewVirtRow,
				    real_row_marked    = RealRow}}.












%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_sort_col(Msg, ProcVars) ->
    #pb_set_sort_col{virtual_col = SortCol} = Msg,
    tv_pb_funcs:set_new_sort_col(SortCol, ProcVars).
    






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


%% Three cases: no button previously clicked, or same button clicked,
%% or some other button clicked.

mark_col_btn(NewId, undefined, NewVirtCol, _OldVirtCol, _RealCol, _SortCol, ColMarkP) ->
       % No btn already pressed!
    gs:config(NewId, [{bg, ?COL_MARK_COLOR},
		      {fg, {255, 255, 255}}
		     ]),
    {true, ColMarkP#col_mark_params{col_btn_id         = NewId,
				    virtual_col_marked = NewVirtCol}};
mark_col_btn(NewId, _OldId, NewVirtCol, OldVirtCol, _RealCol, SortCol, ColMarkP) when NewVirtCol =:= OldVirtCol, NewVirtCol =:= SortCol ->
       % The button previously pressed has been pressed again!
    gs:config(NewId, [{bg, ?SORT_MARK_COLOR},
		      {fg, {0, 0, 0}}
		     ]),
    {false, ColMarkP#col_mark_params{col_btn_id         = undefined,
				     virtual_col_marked = undefined}};
mark_col_btn(NewId, _OldId, NewVirtCol, OldVirtCol, _RealCol, _SortCol, ColMarkP) when NewVirtCol =:= OldVirtCol ->
       % The button previously pressed has been pressed again!
    gs:config(NewId, [{bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}}
		     ]),
    {false, ColMarkP#col_mark_params{col_btn_id         = undefined,
				     virtual_col_marked = undefined}};
mark_col_btn(NewId, OldId, NewVirtCol, _OldVirtCol, _RealCol, _SortCol, ColMarkP) ->
       % A new btn has been pressed!
    gs:config(OldId, [{bg, ?DEFAULT_BG_COLOR},
			 {fg, {0, 0, 0}}
			]),
    gs:config(NewId, [{bg, ?COL_MARK_COLOR},
		      {fg, {255, 255, 255}}
		     ]),
    {true, ColMarkP#col_mark_params{col_btn_id         = NewId,
				    virtual_col_marked = NewVirtCol}}.
    		    
		    
	    
	    
    
    





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


handle_col_resizing(RbtnId, RealCol, VirtualCol, Xpos, ProcVars) ->
    gs:config(RbtnId, [{motion, true}]),
    #process_variables{parent_pid        = ParentPid,
		       grid_frame_id     = GrFrId,
		       grid_frame_height = Height,
		       hbtn_height       = HbtnH,
		       resbtn_width      = RbtnW,
		       cols_shown        = ColsShown} = ProcVars,
    
    LineId = gs:frame(GrFrId, [{width, 1},
			       {height, Height - HbtnH},
			       {x, Xpos},
			       {y, HbtnH - 1},
			       {bg, ?DEFAULT_BG_COLOR}
			      ]),
    MinColWidth = RbtnW,
    
    OldColWidth = lists:nth(RealCol, ColsShown),
    Xdiff = get_xdiff(RbtnId, 1, 0, LineId, Xpos, MinColWidth - OldColWidth),

    ParentPid ! #pb_new_colwidth{sender      = self(),
				 real_col    = RealCol,
				 virtual_col = VirtualCol,
				 xdiff       = Xdiff},

    gs:config(RbtnId, [{motion, false}]),
    gs:destroy(LineId).


    









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_xdiff(Id, Btn, LastXdiff, LineId, LineXpos, MinAllowedXdiff) ->
    receive
	{gs, Id, motion, {resbtn, _RealCol, _VirtCol, _OldXpos}, [NewXdiff | _T]} ->
	    UsedXdiff = erlang:max(MinAllowedXdiff, NewXdiff),
	    gs:config(LineId, [{x, LineXpos + UsedXdiff}]),
	    get_xdiff(Id, Btn, UsedXdiff, LineId, LineXpos, MinAllowedXdiff);
	{gs, Id, buttonrelease, _Data, [Btn | _T]} ->
	    LastXdiff;
	{gs, Id, buttonrelease, _Data, _Args} ->
	    get_xdiff(Id, Btn, LastXdiff, LineId, LineXpos, MinAllowedXdiff);
	{gs, Id, buttonpress, _Data, _Args} ->
	    get_xdiff(Id, Btn, LastXdiff, LineId, LineXpos, MinAllowedXdiff)
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


init_btns(Msg, ProcVars) ->
    #pb_init_btns{parent_id       = ParentId,
		  parent_width    = Width,
		  parent_height   = Height,
		  ypos            = Ypos,
		  hbtn_height     = HbtnH,
		  resbtn_width    = RbtnW,
		  vbtn_width      = VbtnW,
		  nof_rows        = NofRows,
		  row_height      = RowHeight,
		  first_col_shown = FirstColShown,
		  cols_shown      = ColsShown} = Msg,

    NewProcVars = tv_pb_funcs:init_btns(ParentId, Ypos, HbtnH, VbtnW, RbtnW, 
					 FirstColShown, ColsShown, NofRows, 
					 RowHeight, ProcVars),

    gs:frame(ParentId, [{bg, {0, 0, 0}},
			{bw, 0},
			{width, 1300},
			{height, 1},
			{x, 0},
			{y, Ypos - 1}
		       ]),
    NewProcVars#process_variables{grid_frame_width  = Width,
				  grid_frame_height = Height
				 }.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_hbtns(Msg, ProcVars) ->
    #pb_update_hbtns{parent_width    = Width,
		     parent_height   = Height,
		     first_col_shown = FirstColShown,
		     cols_shown      = ColsShown} = Msg,

    NewProcVars = tv_pb_funcs:update_hbtns(FirstColShown, ColsShown, ProcVars),

    NewProcVars#process_variables{grid_frame_width  = Width,
				  grid_frame_height = Height
				 }.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_vbtns(Msg, ProcVars) ->
    #pb_update_vbtns{color_list       = Colors,
		     first_row_shown  = FirstRowShown,
		     nof_rows_shown   = NofRowsShown,
		     blinking_enabled = BlinkEnabled} = Msg,
    
    tv_pb_funcs:update_vbtns(NofRowsShown, FirstRowShown, Colors, BlinkEnabled, 
			      ProcVars).
    





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_keys(Msg, ProcVars) ->
    #pb_key_info{list_of_keys = KeyList} = Msg,
    tv_pb_funcs:update_keys(KeyList, ProcVars).
