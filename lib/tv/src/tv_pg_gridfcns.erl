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
-module(tv_pg_gridfcns).




-export([init_grid/8, 
	 resize_grid/3, 
	 resize_grid_column/4, 
	 update_grid_data/3,
	 scroll_grid_horizontally/2, 
	 mark_cell_and_notify/4, 
	 remove_marks/1,
	 mark_col/2, 
	 mark_row/2,
	 handle_list_info/2
	]).





-include("tv_pd_int_msg.hrl").
-include("tv_pg_int_def.hrl").







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


init_grid(GridParentId, GridWidth, 
	  GridHeight, GridXpos, GridYpos, NofRows, RowHeight, ProcVars) ->

       % Get the size and ID of the grid-parent frame, i.e., the
       % grid-frame! Do not confuse the base-frames below with 
       % the grid-frame!

    #process_variables{parent_pid  = ParentPid,
		       grid_params = GridP}   = ProcVars,
    
    #grid_params{fg_color        = GridFgColor,
		 nof_cols        = NofCols,
		 col_width       = DefaultColWidth,
		 first_col_shown = FirstColShown,
		 col_widths      = ColWidths} = GridP,
    
       % Create the two frames the column frames are placed on!
       % These two frames defines the size of the grid.
    BgFrame = create_base_frame(GridParentId, GridWidth, GridHeight, 
				 GridXpos, GridYpos, GridFgColor),
    FgFrame = create_base_frame(BgFrame, GridWidth - 1, GridHeight - 1, 
			    0, 0, GridFgColor),

       % Compute the the colwidths necessary to cover the grid.
    ColsShown = compute_cols_shown(FirstColShown, ColWidths, GridWidth, NofCols, 
				   DefaultColWidth),
    NofRowsShown = compute_rows_shown(GridHeight, RowHeight),

       % Tell parent about the width of columns shown!
    ParentPid ! #pg_col_info{sender              = self(),
			     first_col_shown     = FirstColShown,
			     width_of_cols_shown = ColsShown,
			     nof_rows_shown      = NofRowsShown
			    },

    NewNofCols = erlang:max(length(ColsShown), NofCols),

       % The GridColWidths list shall contain the current width of each frame.
    NewColWidths = update_col_widths(ColsShown, ColWidths, FirstColShown, 
				     DefaultColWidth),
    
       % Create column frames, one for each column, and rows (labels) on each frame.
    {FrameIdList, ColLabelList} = create_col_frames(NewNofCols, NofRows, RowHeight,
						    FgFrame, GridP, [], []),

       % Get lists of label-ID's for each row. (When we created the column frames,
       % we got the id's of labels placed on each column, i.e., vertically. 
       % However, most often we want the id's for one row, i.e., label id's
       % horisontally.)
    RowIdList    = get_row_ids(NofRows, ColLabelList, []),
    
       % Update the grid_params record with the new values!
    NewGridP = GridP#grid_params{bg_frame       = BgFrame,
				 fg_frame       = FgFrame,
				 grid_width     = GridWidth,
				 grid_height    = GridHeight,
				 grid_xpos      = GridXpos,
				 grid_ypos      = GridYpos,
				 nof_cols       = NewNofCols,
				 col_widths     = NewColWidths,
				 cols_shown     = ColsShown,
				 nof_rows       = NofRows,
				 row_height     = RowHeight,
				 nof_rows_shown = NofRowsShown,
				 col_frame_ids  = FrameIdList,
				 col_ids        = ColLabelList,
				 row_ids        = RowIdList,
				 row_data_list  = lists:duplicate(NofRows, notext)
				},
    
    ProcVars#process_variables{grid_parent_id = GridParentId, 
			       grid_params    = NewGridP}.










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_grid(NewWidth, NewHeight, ProcVars) ->
    #process_variables{parent_pid  = ParentPid,
		       grid_params = GridP,
		       mark_params  = MarkP}   = ProcVars,
    
    #grid_params{bg_frame         = BgFrame,
		 fg_frame         = FgFrame,
		 nof_cols         = NofCols,
		 nof_rows         = NofRows,
		 col_width        = DefaultColWidth,
		 first_col_shown  = FirstColShown,
		 col_widths       = ColWidths,
		 row_height       = RowHeight,
		 col_frame_ids    = ColFrameIds,
		 col_ids          = ColIds,
		 row_ids          = RowIds,
		 bg_color         = BgColor,
		 fg_color         = FgColor,
		 row_data_list    = RowDataList,
		 lists_as_strings = ListAsStr}  = GridP,
    
    gs:config(BgFrame, [{width, NewWidth},
			{height, NewHeight}
		       ]),
    gs:config(FgFrame, [{width, NewWidth - 1},
			{height, NewHeight - 1}
		       ]),

    ColsShown    = compute_cols_shown(FirstColShown, ColWidths, NewWidth, NofCols, 
				      DefaultColWidth),

    NofRowsShown = compute_rows_shown(NewHeight, RowHeight),


       % Tell parent about the width of columns shown!
    ParentPid ! #pg_col_info{sender              = self(),
			     first_col_shown     = FirstColShown,
			     width_of_cols_shown = ColsShown,
			     nof_rows_shown      = NofRowsShown
			    },

    NewColWidths = update_col_widths(ColsShown, ColWidths, FirstColShown, 
				     DefaultColWidth),
    
    NofColsShown = length(ColsShown),
    {NewNofCols, NewColFrameIds, NewColIds, NewRowIds} = 
	check_nof_cols(ColsShown, (NofColsShown - NofCols), ColFrameIds, ColIds, 
		       RowIds, NofRows, RowHeight, FgColor, BgColor ),

    clear_fields(lists:nthtail(NofColsShown, NewColIds), 
		 lists:nthtail(NofRowsShown, NewRowIds)),

    RowsToUpdate = lists:sublist(NewRowIds, NofRowsShown),

    refresh_visible_rows(RowsToUpdate, FirstColShown, NofColsShown, RowDataList, ListAsStr),
    
    NewGridP = GridP#grid_params{grid_width     = NewWidth,
				 grid_height    = NewHeight,
				 nof_cols       = NewNofCols,
				 nof_rows_shown = NofRowsShown,
				 cols_shown     = ColsShown,
				 col_widths     = NewColWidths,
				 col_frame_ids  = NewColFrameIds,
				 col_ids        = NewColIds,
				 row_ids        = NewRowIds
				},

    refresh_marks(NewGridP, MarkP),

    ProcVars#process_variables{grid_params = NewGridP}.
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_grid_column(RealCol, VirtualCol, Xdiff, ProcVars) ->
    #process_variables{parent_pid  = ParentPid,
		       grid_params = GridP,
		       mark_params = MarkP}   = ProcVars,
    
    #grid_params{grid_width       = GridWidth,
		 first_col_shown  = FirstColShown,
		 nof_cols         = NofCols,
		 col_widths       = ColWidths,
		 col_frame_ids    = ColFrameIds,
		 col_ids          = ColIds,
		 col_width        = DefaultColWidth,
		 row_ids          = RowIds,
		 max_col_width    = MaxColWidth,
		 min_col_width    = MinColWidth,
		 nof_rows         = NofRows,
		 nof_rows_shown   = NofRowsShown,
		 row_height       = RowHeight,
		 bg_color         = BgColor,
		 fg_color         = FgColor,
		 row_data_list    = RowDataList,
		 lists_as_strings = ListAsStr}  = GridP,
    
       % Get new width!
    Width = erlang:min(MaxColWidth, erlang:max((lists:nth(VirtualCol, ColWidths) + Xdiff),
				 MinColWidth)),

       % Resize the column.
    NewWidthOfCol = resize_one_column(RealCol, Width, ColFrameIds, MaxColWidth, 
				      MinColWidth),

       % Update the ColWidths list.
    TempColWidths = lists:sublist(ColWidths, VirtualCol - 1) ++ 
	[NewWidthOfCol | lists:nthtail(VirtualCol, ColWidths)],
    
       % Check the other columns, whether a new column has to be created.
    ColsShown    = compute_cols_shown(FirstColShown, TempColWidths, GridWidth, 
				      NofCols, DefaultColWidth),

       % Get the final ColWidths list, after all updates!
    NewColWidths = update_col_widths(ColsShown, TempColWidths, FirstColShown, 
				     DefaultColWidth),

       % Tell parent about the width of columns shown!
    ParentPid ! #pg_col_info{sender              = self(),
			     first_col_shown     = FirstColShown,
			     width_of_cols_shown = ColsShown,
			     nof_rows_shown      = NofRowsShown
			    },

       % Get the new number of columns (may have changed).
    NofColsShown = length(ColsShown),
    {NewNofCols, NewColFrameIds, NewColIds, NewRowIds} = 
	check_nof_cols(ColsShown, (NofColsShown - NofCols), ColFrameIds, ColIds, 
		       RowIds, NofRows, RowHeight, FgColor, BgColor ),

    RowsToUpdate = lists:sublist(NewRowIds, NofRowsShown),
    refresh_visible_rows(RowsToUpdate, FirstColShown, NofColsShown, RowDataList, ListAsStr),
    
    NewGridP = GridP#grid_params{nof_cols       = NewNofCols,
				 cols_shown     = ColsShown,
				 col_widths     = NewColWidths,
				 col_frame_ids  = NewColFrameIds,
				 col_ids        = NewColIds,
				 row_ids        = NewRowIds
				},

    refresh_marks(NewGridP, MarkP),

    ProcVars#process_variables{grid_params = NewGridP}.
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


handle_list_info(ListAsStr, ProcVars) ->
    #process_variables{grid_params = GridP}   = ProcVars,
    
    #grid_params{first_col_shown  = FirstColShown,
		 cols_shown       = ColsShown,
		 nof_rows_shown   = NofRowsShown,
		 row_data_list    = RowDataList,
		 row_ids          = RowIds,
		 lists_as_strings = OldListAsStr}  = GridP,

    case ListAsStr of
	OldListAsStr ->
	    ProcVars;
	_NewValue ->
	    NofColsShown = length(ColsShown),
	    RowsToUpdate = lists:sublist(RowIds, NofRowsShown),
	    refresh_visible_rows(RowsToUpdate, FirstColShown, NofColsShown, 
				 RowDataList, ListAsStr),
	    NewGridP = GridP#grid_params{lists_as_strings = ListAsStr},
	    ProcVars#process_variables{grid_params = NewGridP}
    end.
    



update_grid_data(Data, FirstRowShown, ProcVars) ->
    #process_variables{grid_params = GridP,
		       mark_params = MarkP}   = ProcVars,
    
    #grid_params{first_col_shown  = FirstColShown,
		 cols_shown       = ColsShown,
		 nof_rows         = NofRows,
		 nof_rows_shown   = NofRowsShown,
		 row_ids          = RowIds,
		 lists_as_strings = ListAsStr}  = GridP,
    
    NofColsShown = length(ColsShown),
    RowsToUpdate = lists:sublist(RowIds, NofRowsShown),

    NewMarkP = move_marks(FirstColShown, FirstRowShown, GridP, MarkP),

    update_visible_rows(RowsToUpdate, FirstColShown, NofColsShown, Data, ListAsStr),
    NewRowDataList = make_row_data_list(1, NofRows, Data),
    
    NewGridP = GridP#grid_params{first_row_shown = FirstRowShown,
				 row_data_list   = NewRowDataList},

    ProcVars#process_variables{grid_params = NewGridP,
			       mark_params = NewMarkP}.
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


scroll_grid_horizontally(NewFirstColShown, ProcVars) ->
    #process_variables{parent_pid  = ParentPid,
		       grid_params = GridP,
		       mark_params = MarkP}   = ProcVars,
    
    #grid_params{grid_width       = Width,
		 nof_cols         = NofCols,
		 nof_rows         = NofRows,
		 nof_rows_shown   = NofRowsShown,
		 first_row_shown  = FirstRowShown,
		 col_width        = DefaultColWidth,
		 max_col_width    = MaxColWidth,
		 min_col_width    = MinColWidth,
		 col_widths       = ColWidths,
		 row_height       = RowHeight,
		 col_frame_ids    = ColFrameIds,
		 col_ids          = ColIds,
		 row_ids          = RowIds,
		 bg_color         = BgColor,
		 fg_color         = FgColor,
		 row_data_list    = RowDataList,
		 lists_as_strings = ListAsStr}  = GridP,
    
       % Probably it is unnecessary to check whether any new columns shall be 
       % created or not, but what the heck, we don't want to crash...
    ColsShown    = compute_cols_shown(NewFirstColShown, ColWidths, Width, NofCols,
				      DefaultColWidth),
    NofColsShown = length(ColsShown),

    ParentPid ! #pg_col_info{sender              = self(),
			     first_col_shown     = NewFirstColShown,
			     width_of_cols_shown = ColsShown,
			     nof_rows_shown      = NofRowsShown
			    },

    NewMarkP = move_marks(NewFirstColShown, FirstRowShown, GridP, MarkP),

    NewColWidths = update_col_widths(ColsShown, ColWidths, NewFirstColShown, 
				     DefaultColWidth),
    
    {NewNofCols, NewColFrameIds, NewColIds, NewRowIds} = 
	check_nof_cols(ColsShown, (NofColsShown - NofCols), ColFrameIds, ColIds, 
		       RowIds, NofRows, RowHeight, FgColor, BgColor ),
    

    RowsToUpdate = lists:sublist(NewRowIds, NofRowsShown),
    resize_all_grid_columns(1, ColsShown, NewColFrameIds, MaxColWidth, MinColWidth),

    refresh_visible_rows(RowsToUpdate, NewFirstColShown, NofColsShown, RowDataList, ListAsStr),

       % Clear fields currently not visible.
    clear_fields(lists:nthtail(NofColsShown, NewColIds), 
		 lists:nthtail(NofRowsShown, NewRowIds)),

    
    NewGridP = GridP#grid_params{nof_cols        = NewNofCols,
				 cols_shown      = ColsShown,
				 col_widths      = NewColWidths,
				 col_frame_ids   = NewColFrameIds,
				 col_ids         = NewColIds,
				 row_ids         = NewRowIds,
				 first_col_shown = NewFirstColShown
				},

    ProcVars#process_variables{grid_params = NewGridP,
			       mark_params = NewMarkP}.
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_row(VirtualRow, ProcVars) ->
    #process_variables{grid_params = GridP,
		       mark_params = MarkP} = ProcVars,
    
    #grid_params{first_row_shown = FirstRowShown,
		 nof_rows_shown  = NofRowsShown,
		 row_ids         = RowIds}  = GridP,
    
    mark_row(VirtualRow, FirstRowShown, FirstRowShown + NofRowsShown - 1, RowIds,
	     ?GRID_MARK_COLOR),

    NewMarkP = MarkP#mark_params{cell_id     = undefined,
				 virtual_col = undefined,
				 virtual_row = VirtualRow
				},

    ProcVars#process_variables{mark_params = NewMarkP}.










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_col(VirtualCol, ProcVars) ->
    #process_variables{grid_params = GridP,
		       mark_params = MarkP} = ProcVars,
    
    #grid_params{first_col_shown = FirstColShown,
		 cols_shown      = ColsShown,
		 col_ids         = ColIds}  = GridP,
    
    NofColsShown = length(ColsShown),
    mark_col(VirtualCol, FirstColShown, FirstColShown + NofColsShown - 1, ColIds,
	     ?GRID_MARK_COLOR),

    NewMarkP = MarkP#mark_params{cell_id     = undefined,
				 virtual_col = VirtualCol,
				 virtual_row = undefined
				},

    ProcVars#process_variables{mark_params = NewMarkP}.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_cell_and_notify(CellId, RealCol, RealRow, ProcVars) ->
    #process_variables{parent_pid  = ParentPid,
		       grid_params = GridP,
		       mark_params = MarkP} = ProcVars,
    
    #grid_params{first_col_shown = FirstColShown,
		 first_row_shown = FirstRowShown} = GridP,

    OldCellId = MarkP#mark_params.cell_id,

    VirtualCol = FirstColShown + RealCol - 1,
    VirtualRow = FirstRowShown + RealRow - 1,

    %% Right now, when the table tool only is passive, i.e., we cannot edit
    %% the table content, we don't want to be able to mark empty cells.
    
    {text, CellText} = gs:read(CellId, label),

    CellMarked = case CellText of
		     "" -> false;
		     _AnyText when CellId=:=OldCellId -> false;
		     _AnyText -> true
		 end,

    remove_marks(ProcVars),
    update_marked_cells(CellId, OldCellId, CellMarked),

    notify_about_cell_marked(ParentPid, CellMarked, RealCol, RealRow, 
			     VirtualCol, VirtualRow, CellText),
    
    NewMarkP = case CellMarked of
		   true ->
		       MarkP#mark_params{cell_id     = CellId,
					 virtual_col = VirtualCol,
					 virtual_row = VirtualRow
					};
		   false ->
		       MarkP#mark_params{cell_id     = undefined,
					 virtual_col = 0,
					 virtual_row = undefined
					}
	       end,
    
    ProcVars#process_variables{mark_params = NewMarkP}.








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
    #process_variables{mark_params = MarkP,
		       grid_params = GridP} = ProcVars,

    #grid_params{first_col_shown = FirstColShown,
		 cols_shown      = ColsShown,
		 col_ids         = ColIds,
		 first_row_shown = FirstRowShown,
		 nof_rows_shown  = NofRowsShown,
		 row_ids         = RowIds}  = GridP,


    #mark_params{cell_id     = CellId,
		 virtual_col = VirtualCol,
		 virtual_row = VirtualRow} = MarkP,

    case {VirtualCol, VirtualRow} of
	{undefined, undefined} ->
	    update_marked_cells(CellId, CellId, false);
	{_AnyCol, undefined} ->
	    NofColsShown = length(ColsShown),
	    unmark_col(VirtualCol, FirstColShown, FirstColShown + NofColsShown - 1, 
		       ColIds);
	{undefined, _AnyRow} ->
	    unmark_row(VirtualRow, FirstRowShown, FirstRowShown + NofRowsShown - 1, 
		       RowIds);
	_Other ->
	    update_marked_cells(CellId, CellId, false)
    end,
    
    NewMarkP = MarkP#mark_params{cell_id     = undefined,
				 virtual_col = 0,
				 virtual_row = undefined
				},
    ProcVars#process_variables{mark_params = NewMarkP}.
    
		 







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


move_marks(FirstCol, FirstRow, GridP, MarkP) ->
    #grid_params{first_col_shown = OldFirstCol,
		 cols_shown      = ColsShown,
		 first_row_shown = OldFirstRow,
		 nof_rows_shown  = NofRowsShown,
		 col_ids         = ColIds,
		 row_ids         = RowIds}  = GridP,

    #mark_params{virtual_col = VirtualCol,
		 virtual_row = VirtualRow} = MarkP,


    case {VirtualCol, VirtualRow} of
	{undefined, undefined} ->
	    NofColsShown = length(ColsShown),
	    move_marked_cell(FirstCol, FirstRow, NofColsShown, 
			     NofRowsShown, RowIds, MarkP);
	{_AnyCol, undefined} ->
	    NofColsShown = length(ColsShown),
	    OldLastCol = OldFirstCol + NofColsShown - 1,
	    LastCol    = FirstCol + NofColsShown - 1,
	    move_marked_col(VirtualCol, OldFirstCol, OldLastCol, 
			    FirstCol, LastCol, ColIds, MarkP);
	{undefined, _AnyRow} ->
	    OldLastRow = OldFirstRow + NofRowsShown - 1,
	    LastRow    = FirstRow + NofRowsShown - 1,
	    move_marked_row(VirtualRow, OldFirstRow, OldLastRow, 
			    FirstRow, LastRow, RowIds, MarkP);
	{_CellCol, _CellRow} ->
	    NofColsShown = length(ColsShown),
	    move_marked_cell(FirstCol, FirstRow, NofColsShown, 
			     NofRowsShown, RowIds, MarkP)
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


refresh_marks(GridP, MarkP) ->
    #grid_params{first_col_shown = FirstCol,
		 cols_shown      = ColsShown,
		 first_row_shown = FirstRow,
		 nof_rows_shown  = NofRowsShown,
		 col_ids         = ColIds,
		 row_ids         = RowIds}  = GridP,

    #mark_params{virtual_col = VirtualCol,
		 virtual_row = VirtualRow} = MarkP,


    case {VirtualCol, VirtualRow} of
	{undefined, undefined} ->
	    NofColsShown = length(ColsShown),
	    move_marked_cell(FirstCol, FirstRow, NofColsShown, NofRowsShown, 
			     RowIds, MarkP);
	{_AnyCol, undefined} ->
	    NofColsShown = length(ColsShown),
	    LastCol    = FirstCol + NofColsShown - 1,
	    mark_col(VirtualCol, FirstCol, LastCol, ColIds, ?GRID_MARK_COLOR);
	{undefined, _AnyRow} ->
	    LastRow    = FirstRow + NofRowsShown - 1,
	    mark_row(VirtualRow, FirstRow, LastRow, RowIds, ?GRID_MARK_COLOR);
	{_CellCol, _CellRow} ->
	    NofColsShown = length(ColsShown),
	    move_marked_cell(FirstCol, FirstRow, NofColsShown, NofRowsShown, 
			     RowIds, MarkP)
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


move_marked_col(VirtualCol, 
		OldFirstCol, OldLastCol, FirstCol, LastCol, ColIds, MarkP) ->
    unmark_col(VirtualCol, OldFirstCol, OldLastCol, ColIds),
    mark_col(VirtualCol, FirstCol, LastCol, ColIds, ?GRID_MARK_COLOR),
    MarkP#mark_params{cell_id = undefined}.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_col(VirtualCol, FirstCol, _LastCol, _ColIds, _Color) when VirtualCol < FirstCol ->
    done;
mark_col(VirtualCol, _FirstCol, LastCol, _ColIds, _Color) when VirtualCol > LastCol ->
    done;
mark_col(VirtualCol, FirstCol, _LastCol, ColIds, Color) ->
    RealCol = VirtualCol - FirstCol + 1,
    MarkedColIds = lists:nth(RealCol, ColIds),
    mark_all_cells(MarkedColIds, Color).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


unmark_col(VirtualCol, FirstCol, LastCol, ColIds) ->
    mark_col(VirtualCol, FirstCol, LastCol, ColIds, ?DEFAULT_GRID_BGCOLOR).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_all_cells([], _Color) ->
    done;
mark_all_cells([CellId | T], Color) ->
    gs:config(CellId, [{bg, Color}]),
    mark_all_cells(T, Color).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_row(VirtualRow, FirstRow, _LastRow, _RowIds, _Color) when VirtualRow < FirstRow ->
    done;
mark_row(VirtualRow, _FirstRow, LastRow, _RowIds, _Color) when VirtualRow > LastRow ->
    done;
mark_row(VirtualRow, FirstRow, _LastRow, RowIds, Color) ->
    RealRow      = VirtualRow - FirstRow + 1,
    MarkedRowIds = lists:nth(RealRow, RowIds),
    mark_all_cells(MarkedRowIds, Color).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


unmark_row(VirtualRow, FirstRow, LastRow, RowIds) ->
    mark_row(VirtualRow, FirstRow, LastRow, RowIds, ?DEFAULT_GRID_BGCOLOR).










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


move_marked_row(VirtualRow, 
		OldFirstRow, OldLastRow, FirstRow, LastRow, RowIds, MarkP) ->
    unmark_row(VirtualRow, OldFirstRow, OldLastRow, RowIds),
    mark_row(VirtualRow, FirstRow, LastRow, RowIds, ?GRID_MARK_COLOR),
    MarkP#mark_params{cell_id = undefined}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


move_marked_cell(FirstColShown, 
		 FirstRowShown, NofColsShown, NofRowsShown, RowIds, MarkP) ->
    #mark_params{cell_id     = OldCellId,
		 virtual_col = VirtualCol,
		 virtual_row = VirtualRow} = MarkP,
    
    case OldCellId of
	undefined ->
	    MarkP;
	_OtherId ->
	    NewRealCol = VirtualCol - FirstColShown + 1,
	    NewRealRow = VirtualRow - FirstRowShown + 1,
	    update_marked_cells(undefined, OldCellId, false),
	    case check_if_new_mark_visible(NewRealCol, NewRealRow, 
					   NofColsShown, NofRowsShown) of
		false ->
		    MarkP;
		true ->
		    NewCellId = lists:nth(NewRealCol, 
					  lists:nth(NewRealRow, RowIds)),
		    update_marked_cells(NewCellId, undefined, true),
		    MarkP#mark_params{cell_id  = NewCellId}
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


check_if_new_mark_visible(Col, _Row, NofCols, _NofRows) when Col > NofCols ->
    false;
check_if_new_mark_visible(Col, _Row, _NofCols, _NofRows) when Col =< 0 ->
    false;
check_if_new_mark_visible(_Col, Row, _NofCols, NofRows) when Row > NofRows ->
    false;
check_if_new_mark_visible(_Col, Row, _NofCols, _NofRows) when Row =< 0 ->
    false;
check_if_new_mark_visible(_Col, _Row, _NofCols, _NofRows) ->
    true.
    










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_marked_cells(CellId, OldCellId, _MarkedCell) when CellId =:= OldCellId ->
    gs:config(CellId, [{bg, ?DEFAULT_GRID_BGCOLOR}]);
update_marked_cells(_CellId, undefined, false) ->
    done;
update_marked_cells(CellId, undefined, true) ->
    gs:config(CellId, [{bg, ?GRID_MARK_COLOR}]);
update_marked_cells(CellId, OldCellId, true) ->
    gs:config(OldCellId, [{bg, ?DEFAULT_GRID_BGCOLOR}]),
    gs:config(CellId, [{bg, ?GRID_MARK_COLOR}]);
update_marked_cells(_CellId, OldCellId, false) ->
    gs:config(OldCellId, [{bg, ?DEFAULT_GRID_BGCOLOR}]).

    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


notify_about_cell_marked(Pid, Marked, RealCol, RealRow, VirtCol, VirtRow, Text) ->
    Pid ! #pg_cell_marked{sender      = self(),
			  cell_marked = Marked,
			  real_col    = RealCol,
			  real_row    = RealRow,
			  virtual_col = VirtCol,
			  virtual_row = VirtRow,
			  cell_text   = Text
			 }.
    







%%%---------------------------------------------------------------------
%%% START of functions used to print data in the grid fields.
%%%---------------------------------------------------------------------




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


refresh_visible_rows([], _FirstColShown, _NofColsShown, _DataList, _ListAsStr) ->
    done;
refresh_visible_rows(RowIds, _FirstColShown, _NofColsShown, [], _ListAsStr) ->
    clear_cols_or_rows(RowIds);
refresh_visible_rows([OneRowIds | RemRowIds], FirstColShown, NofColsShown,
		    [DataItemList | RemDataItemLists], ListAsStr) ->
    NewDataItemList = get_data_sublist(DataItemList, FirstColShown, NofColsShown),
    update_one_row(lists:sublist(OneRowIds, NofColsShown), NewDataItemList, ListAsStr),
    refresh_visible_rows(RemRowIds, FirstColShown, NofColsShown, RemDataItemLists, ListAsStr).









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_visible_rows([], _FirstColShown, _NofColsShown, _DataList, _ListAsStr) ->
    done;
update_visible_rows(RowIds, _FirstColShown, _NofColsShown, [], _ListAsStr) ->
    clear_cols_or_rows(RowIds);
update_visible_rows([OneRowIds | RemRowIds], FirstColShown, NofColsShown,
		    [DataItem | RemData], ListAsStr) ->
       % We convert the received item to a list! This way we know that 
       % '[notext]' shall be printed as 'notext', while 'notext' shall
       % be printed as ''.
    TempDataItemList = item_to_list(DataItem),
    DataItemList = get_data_sublist(TempDataItemList, FirstColShown, 
				    NofColsShown),
    update_one_row(lists:sublist(OneRowIds, NofColsShown), DataItemList, ListAsStr),
    update_visible_rows(RemRowIds, FirstColShown, NofColsShown, RemData, ListAsStr).










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_one_row(OneRowIds, [], _ListAsStr) ->
    clear_one_col_or_row(OneRowIds);
update_one_row([], _DataItemList, _ListAsStr) ->
    done;
update_one_row([LabelId | RemLabelIds], [notext | T], ListAsStr) ->
    gs:config(LabelId, [{label, {text, ""}}
		       ]),
    update_one_row(RemLabelIds, T, ListAsStr);
update_one_row([LabelId | RemLabelIds], [DataElem | T], ListAsStr) ->
    Str = case ListAsStr of
	      true ->
		  tv_io_lib:format(" ~p", [DataElem]);
	      false ->
		  " " ++ lists:flatten(tv_io_lib:write(DataElem))
	  end,
    gs:config(LabelId, [{label, {text, Str}}
		       ]),
    update_one_row(RemLabelIds, T, ListAsStr).
    
		      









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


make_row_data_list(N, NofRows, []) when N > NofRows ->
    [];
make_row_data_list(N, NofRows, []) ->
       % If NofRows == N, we get the empty list here!
    lists:duplicate(NofRows- N, notext);
make_row_data_list(N, NofRows, [_DataItem | _RemData]) when N > NofRows ->
    [];
make_row_data_list(N, NofRows, [DataItem | RemData]) ->
       % We convert the received item to a list! This way we know that 
       % '[notext]' shall be printed as 'notext', while 'notext' shall
       % be printed as ''.
    [item_to_list(DataItem) | make_row_data_list(N + 1, NofRows, RemData)].
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


item_to_list(Item) when is_tuple(Item) ->
    tuple_to_list(Item);
item_to_list(Item) ->
    [Item].

    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_data_sublist(DataList, StartPos, Length) ->
    case catch lists:sublist(DataList, StartPos, Length) of
	{'EXIT', _Reason} ->
	    [];
	Sublist ->
	    Sublist
    end.







%%%---------------------------------------------------------------------
%%% END of functions used to print data in the grid fields.
%%%---------------------------------------------------------------------





%%%---------------------------------------------------------------------
%%% START of functions used to resize the grid columns.
%%%---------------------------------------------------------------------




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_all_grid_columns(_RealCol, [], _ColFrameIds, _MaxColWidth, _MinColWidth) ->
    done;
resize_all_grid_columns(RealCol, [ColWidth | Tail], ColFrameIds, MaxColWidth, MinColWidth) ->

    resize_one_column(RealCol, ColWidth, ColFrameIds, MaxColWidth, MinColWidth),
    resize_all_grid_columns(RealCol + 1, Tail, ColFrameIds, MaxColWidth, 
			    MinColWidth). 








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_one_column(RealCol, Width, ColFrameIds, MaxW, MinW) ->
    NewWidthOfCol = erlang:min(MaxW, erlang:max(Width, MinW)),
    case length(ColFrameIds) of
	RealCol ->
	    done;
	_Other ->
	    FrameId = lists:nth(RealCol + 1, ColFrameIds), 
	    gs:config(FrameId, [{x, NewWidthOfCol + 1}])
    end,
    NewWidthOfCol.




%%%---------------------------------------------------------------------
%%% END of functions used to resize the grid columns.
%%%---------------------------------------------------------------------






%%%---------------------------------------------------------------------
%%% START of functions used to update the grid. 
%%%---------------------------------------------------------------------




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


clear_fields(ColIds, RowIds) ->
    clear_cols_or_rows(ColIds),
    clear_cols_or_rows(RowIds).









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


clear_cols_or_rows([]) ->
    done;
clear_cols_or_rows([IdList | RemIdLists]) ->
    clear_one_col_or_row(IdList),
    clear_cols_or_rows(RemIdLists).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


clear_one_col_or_row([]) ->
    done;
clear_one_col_or_row([LabelId | RemLabelIds]) ->
    gs:config(LabelId, [{label, {text, ""}}
		       ]),
    clear_one_col_or_row(RemLabelIds).
    




%%%---------------------------------------------------------------------
%%% END of functions used to update the grid. 
%%%---------------------------------------------------------------------






%%%---------------------------------------------------------------------
%%% START of functions used to compute the part of the grid that has to 
%%% be updated, as well as deciding whether a new column has to be added.
%%% Old columns (i.e., columns not visible) are not removed, but they 
%%% shall not be updated until they once again becomes visible.
%%%---------------------------------------------------------------------





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


check_nof_cols(_ColsShown, NofNewCols, ColFrameIds, ColIds, RowIds, 
	       _NofRows, _RowHeight, _FgColor, _BgColor) when NofNewCols =< 0 ->
    {length(ColFrameIds), ColFrameIds, ColIds, RowIds};
check_nof_cols(ColsShown, NofNewCols, ColFrameIds, ColIds, 
	       RowIds, NofRows, RowHeight, FgColor, BgColor) ->
    NewColNo = length(ColFrameIds) + 1,
       % We don't care about the pathological case where no columns have been
       % created. If the gridwidth, or the columnwidth, was set to =< 0 during
       % initialisation, then no columns will have been created. The program 
       % will probably also have crashed. If any smart jackass has set invalid
       % values on these important parameters, then he can only blame himself.
    ParentId = lists:nth((NewColNo - 1), ColFrameIds),
    ParentColWidth = lists:nth((NewColNo - 1), ColsShown),
    Xpos = ParentColWidth + 1,

    {ColFrameId, LabelIds} = add_one_col_frame(ParentId, NewColNo, Xpos, FgColor, 
					       BgColor, NofRows, RowHeight),

    NewColFrameIds = ColFrameIds ++ [ColFrameId],
    NewColIds      = ColIds ++ [LabelIds],
    NewRowIds      = update_row_ids(RowIds, LabelIds),
    
    check_nof_cols(ColsShown, NofNewCols - 1, NewColFrameIds, NewColIds, NewRowIds,
		   NofRows, RowHeight, FgColor, BgColor).


    
    
    






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_row_ids([], _LabelIds) ->
    [];
update_row_ids([OneRowIds | RemainingRows], [NewElemId | RemainingElemIds]) ->
    [OneRowIds ++ [NewElemId] | update_row_ids(RemainingRows, RemainingElemIds)].













%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_col_widths(ColsShown, ColWidths, FirstColShown, DefaultColWidth) ->
       % What we do here is that we first (if necessary) add default 
       % column widths to the ColWidth list until it reaches to where 
       % ColsShown starts (vitually seen). 
       % In the second step we take the appropriate elements from the
       % ColsShown list and add them to the ColWidths list, until it is
       % of sufficient length. 
       % Of course this may seem unnecessary - it would suffice to just
       % add default widths to the ColWidths list until it is long enough,
       % since the compute_cols_shown function  right now just adds default 
       % width columns to the ColsShown list, when the ColWidths list is empty.
       % However, this could change (maybe we some other time want the last
       % column to carry all remaining width, instead of adding new columns).
       % Besides, we don't like hidden dependencies between functions!!!

    NofColsShown   = length(ColsShown),
    NewColWidths   = set_necessary_col_widths_length(FirstColShown, ColWidths, 
						 DefaultColWidth),
       % Now NofVirtualCols will always be equal to, or greater 
       % than, FirstColShown - 1.
    
    NofVirtualCols          = length(NewColWidths),
    NecessaryNofVirtualCols = FirstColShown + (NofColsShown - 1),
    if 
	NecessaryNofVirtualCols > NofVirtualCols ->
	    TailNo = NofVirtualCols - FirstColShown + 1,   % Always >= 0 !!!
	    NewColWidths ++ lists:nthtail(TailNo, ColsShown);
	true ->
	    NewColWidths
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


set_necessary_col_widths_length(FirstColShown, ColWidths, DefaultColWidth) ->
       % First check that (length(ColWidths) - FirstColShown) >= -1.
       % If not, add elements so the relation holds true!
    MissingDefaultWidthElems = FirstColShown - length(ColWidths),
    if 
	MissingDefaultWidthElems > 1 ->
	    ColWidths ++ lists:duplicate(MissingDefaultWidthElems - 1, 
					 DefaultColWidth);
	true ->
	    ColWidths
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


compute_rows_shown(GridHeight, RowHeight) ->
    (GridHeight div RowHeight) + 1.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


compute_cols_shown(FirstColShown, ColWidths, GridWidth, _NofCols, DefaultColWidth) ->
    ColWidthsLength = length(ColWidths),
       % Normally ColWidths shall be long enough, but just to make sure...
       % (We could have chosen to update ColWidths here to, but right now
       % we do it instead explicitly when resizeing the grid, changing the 
       % column size(s), and scrolling horizontally.)
    UsedColWidths = if
			ColWidthsLength < FirstColShown ->
			    [];
			true ->
			    lists:nthtail(FirstColShown - 1, ColWidths)
		    end,
    compute_cols_shown(UsedColWidths, GridWidth, DefaultColWidth).






compute_cols_shown(_ColWidths, RemainingWidth, _DefColW) when RemainingWidth =< 0 ->
    [];
compute_cols_shown([], RemainingWidth, DefaultColWidth) ->
    [DefaultColWidth | compute_cols_shown([], RemainingWidth - DefaultColWidth, 
					  DefaultColWidth)];
compute_cols_shown([VirtualColWidth | T], RemainingWidth, DefaultColWidth) ->
    [VirtualColWidth | compute_cols_shown(T, RemainingWidth - VirtualColWidth,
					 DefaultColWidth)].


    
    

%%%---------------------------------------------------------------------
%%% END of functions used to compute the part of the grid that has to 
%%% be updated, as well as deciding whether a new column has to be added.
%%%---------------------------------------------------------------------







%%%---------------------------------------------------------------------
%%% START of functions used to create the grid (baseframes, columns 
%%% and rows), as well as sorting the ID's appropriately.
%%%---------------------------------------------------------------------



%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_base_frame(ParentId, Width, Height, Xpos, Ypos, BgColor) ->
    gs:frame(ParentId, [{width, Width},
			{height, Height},
			{x, Xpos},
			{y, Ypos},
			{bg, BgColor}
		       ]).





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_col_frames(0, _NofRows, _RowHeight, _ParentId, _GridP, ColFrameAcc, LabelAcc) ->
    {lists:reverse(ColFrameAcc), lists:reverse(LabelAcc)};
create_col_frames(N, NofRows, RowHeight, ParentId, GridP, ColFrameAcc, LabelAcc) ->
       % Yes, it *IS* inefficient to copy GridP for each loop.
       % However, it is only done once, and for a limited number of times,
       % and we avoid having a lot of parameters!
    #grid_params{bg_color   = BgColor,
		 fg_color   = FgColor,
		 nof_cols   = NofCols,
		 col_width  = ColWidth} = GridP,
    Xpos = if
	       N =:= NofCols ->
		   0;
	       true ->
		   ColWidth + 1
	   end,
    
    ColNo = NofCols - N + 1,
    {ColFrameId, LabelIds} = add_one_col_frame(ParentId, ColNo, Xpos, FgColor, 
					       BgColor, NofRows, RowHeight),
    create_col_frames(N - 1, NofRows, RowHeight, ColFrameId, GridP, 
		      [ColFrameId | ColFrameAcc], [LabelIds | LabelAcc]).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


add_one_col_frame(ParentId, ColNo, Xpos, FgColor, BgColor, NofRows, RowHeight) ->
    ColFrameId   = create_one_col_frame(ParentId, Xpos, FgColor),
    FirstRowYpos = 1, 
    FirstRowNo   = 1,
    LabelIds     = create_rows_on_frame(ColFrameId, FirstRowNo, NofRows, RowHeight,
					FirstRowYpos, FgColor, BgColor, ColNo, []), 
    {ColFrameId, LabelIds}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_one_col_frame(ParentId, Xpos, BgColor) ->
    ColFrameWidth     = 1200,
    ColFrameHeight    = 900,
    Ypos = 0,
    gs:frame(ParentId, [{width, ColFrameWidth},
			{height, ColFrameHeight},
			{x, Xpos},
			{y, Ypos},
			{bg, BgColor}
		       ]).
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_rows_on_frame(_FrameId, RowNo, NofRows, _H, _Y, _Fg, _Bg, _ColNo, Acc) when RowNo > NofRows -> 
    lists:reverse(Acc);
create_rows_on_frame(FrameId, RowNo, NofRows, H, Y, Fg, Bg, ColNo, RAcc) -> 
    Width = 1200, 
    R = gs:label(FrameId, [{width, Width},
			   {height, H},
			   {x, 1},
			   {y, Y},
			   {bg, Bg},
			   {fg, Fg},
			   {align, w},
			   {buttonpress, true},
			   {data, {gridcell, ColNo, RowNo, FrameId}}
			  ]),
    NextRowNo = RowNo + 1,
    NextY = Y + H +1,
    create_rows_on_frame(FrameId, NextRowNo, NofRows, H, NextY, Fg, Bg, ColNo, 
			     [R | RAcc]).









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_row_ids(0, _Cols, RowAcc) ->
    RowAcc;
get_row_ids(RowNo, Cols, RowAcc) ->
    Row = extract_ids_for_one_row(RowNo, Cols),
    get_row_ids(RowNo - 1, Cols, [Row | RowAcc]).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


extract_ids_for_one_row(_N, []) ->
    [];
extract_ids_for_one_row(N, [ColIds | Tail]) ->
    [lists:nth(N, ColIds) | extract_ids_for_one_row(N, Tail)].



%%%---------------------------------------------------------------------
%%% END of functions used to create the grid.
%%%---------------------------------------------------------------------
