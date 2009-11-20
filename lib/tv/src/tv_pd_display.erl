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
%%%   Description:      Part of pd controlling the graphics.
%%%
%%%*********************************************************************

-module(tv_pd_display).




-export([init_display/4, 
	 display_data/8,
	 resize_display/3, 
	 resize_column/4, 
	 scroll_horizontally/2, 
	 scroll_vertically/2, 
	 perform_horizontal_scroll/2,
	 perform_vertical_scroll/2, 
	 marked_cell/5, 
	 update_toolbar_label/5,
	 update_toolbar_editor/2,
	 get_data_element/4, 
	 hide_toolbar_editor/1, 
	 show_toolbar_editor/1]).





-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pd_int_def.hrl").
-include("tv_pd_int_msg.hrl").







%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************



%%======================================================================
%% Function:      init_display.
%%
%% Return Value:  Id of the display (here:canvas) created.
%%
%% Description:   Creates the canvas and the scale.
%%
%% Parameters:    Id of the window the display shall be created in.
%%======================================================================


init_display(WindowId, WindowWidth, WindowHeight, ProcVars) ->
       % Get all necessary window parameters!
    #process_variables{pg_pid         = PgPid,
		       pb_pid         = PbPid,
		       frame_params   = FrameP,
		       scale_params   = ScaleP,
		       toolbar_params = ToolP}  = ProcVars,
    
    NewFrameP = tv_pd_frames:create_display_frames(WindowId, WindowWidth,
						    WindowHeight, FrameP),
    
    #frame_params{grid_frame_id     = GridParentId,
		  grid_frame_width  = GridParentWidth,
		  grid_frame_height = GridParentHeight} = NewFrameP,

    PgPid ! #pg_init_grid{sender     = self(),
			  parent_id  = GridParentId,
			  width      = GridParentWidth,
			  height     = GridParentHeight,
			  xpos       = ?VBTN_WIDTH - 1,
			  ypos       = ?KEY_MARK_AREA_HEIGHT + ?HBTN_HEIGHT - 1,
			  nof_rows   = ?NOF_GRIDROWS,
			  row_height = ?ROW_HEIGHT
			 },


    receive
	#pg_col_info{first_col_shown     = FirstColShown, 
		     width_of_cols_shown = ColsShown,
		     nof_rows_shown      = NofRowsShown} ->
	    
	    PbPid ! #pb_init_btns{sender          = self(),
				  parent_id       = GridParentId,
				  parent_width    = GridParentWidth,
				  parent_height   = GridParentHeight,
				  ypos            = ?KEY_MARK_AREA_HEIGHT,
				  hbtn_height     = ?HBTN_HEIGHT,
				  resbtn_width    = ?RESBTN_WIDTH,
				  vbtn_width      = ?VBTN_WIDTH,
				  nof_rows        = ?NOF_GRIDROWS,
				  row_height      = ?ROW_HEIGHT,
				  first_col_shown = FirstColShown,
				  cols_shown      = ColsShown
				 },

	    NewScaleP = tv_pd_scale:init_scale(NewFrameP, ScaleP),	    

	    NewToolP  = init_toolbar(NewFrameP, ToolP),

	    ProcVars#process_variables{window_id       = WindowId,
				       window_width    = WindowWidth,
				       window_height   = WindowHeight,
				       first_col_shown = FirstColShown,
				       nof_rows_shown  = NofRowsShown,
				       cols_shown      = ColsShown,
				       frame_params    = NewFrameP,
				       scale_params    = NewScaleP,
				       toolbar_params  = NewToolP
				      }
    end.
    




resize_display(NewWinW, NewWinH, ProcVars) ->
    #process_variables{pg_pid          = PgPid,
		       pb_pid          = PbPid,
		       color_list      = ColorList,
		       first_row_shown = FirstRowShown,
		       frame_params    = FrameP,
		       scale_params    = ScaleP,
		       toolbar_params  = ToolP}  = ProcVars,
    
    NewFrameP = tv_pd_frames:resize_display_frames(NewWinW, NewWinH, FrameP),
    
    #frame_params{grid_frame_width  = GridParentWidth,
		  grid_frame_height = GridParentHeight} = NewFrameP,
    
    PgPid ! #pg_resize_grid{sender = self(),
			    width  = GridParentWidth,
			    height = GridParentHeight
			   },

    receive
	#pg_col_info{first_col_shown     = FirstColShown, 
		     width_of_cols_shown = ColsShown,
		     nof_rows_shown      = NofRowsShown} ->
	    
	    PbPid ! #pb_update_hbtns{sender          = self(),
				     parent_width    = GridParentWidth,
				     parent_height   = GridParentHeight,
				     first_col_shown = FirstColShown,
				     cols_shown      = ColsShown
				    },

	    PbPid ! #pb_update_vbtns{sender           = self(),
				     color_list       = ColorList,
				     first_row_shown  = FirstRowShown,
				     nof_rows_shown   = NofRowsShown,
				     blinking_enabled = false
				    },

	    NewScaleP = tv_pd_scale:resize_scale(NewFrameP, ScaleP),

	    NewToolP = resize_toolbar(NewFrameP, ToolP),
	    
	    ProcVars#process_variables{window_width    = NewWinW,
				       window_height   = NewWinH,
				       first_col_shown = FirstColShown,
				       nof_rows_shown  = NofRowsShown,
				       cols_shown      = ColsShown,
				       frame_params    = NewFrameP,
				       scale_params    = NewScaleP,
				       toolbar_params  = NewToolP
				      }
    end.
    






resize_column(RealCol, VirtualCol, Xdiff, ProcVars) ->
    #process_variables{pg_pid = PgPid, 
		       pb_pid = PbPid,
		       frame_params = FrameP} = ProcVars,
    
    PgPid ! #pg_resize_grid_col{sender         = self(),
				real_col_no    = RealCol,
				virtual_col_no = VirtualCol,
				xdiff          = Xdiff
			       },
    
    #frame_params{grid_frame_width  = GridFrameWidth,
		  grid_frame_height = GridFrameHeight} = FrameP,
    receive
	#pg_col_info{first_col_shown     = FirstColShown,
		     width_of_cols_shown = ColsShown,
		     nof_rows_shown      = NofRowsShown} ->
	    
	    PbPid ! #pb_update_hbtns{parent_width    = GridFrameWidth,
				     parent_height   = GridFrameHeight,
				     first_col_shown = FirstColShown,
				     cols_shown      = ColsShown
				    },

	    ProcVars#process_variables{first_col_shown = FirstColShown,
				       nof_rows_shown  = NofRowsShown,
				       cols_shown      = ColsShown
				      }
    end.
    







display_data(Pos, Range, _MaxValue, List, KeyList, MaxElemSize, MarkedRowData,ProcVars) ->
    #process_variables{master_pid     = PcPid,
		       rec_pid        = RecPid,
		       pg_pid         = PgPid,
		       pb_pid         = PbPid,
		       writable       = Writable,
		       sorting_on     = SortingOn,
		       nof_rows_shown = NofRowsShown,
		       scale_params   = ScaleP,
		       toolbar_params = ToolP, 
		       mark_params    = MarkP}  = ProcVars,
    
    {DataList, ColorList} = split_dblist(List, [], []),

    NewMarkP = update_marks(SortingOn, DataList, ColorList, MarkedRowData, Pos, NofRowsShown, 
			    Writable, Range, PcPid, PgPid, RecPid, ToolP, MarkP),

    PgPid ! #pg_data{sender = self(),
		     data   = DataList,
		     first_row_shown = Pos
		    },

    PbPid ! #pb_update_vbtns{sender           = self(),
			     color_list       = ColorList,
			     first_row_shown  = Pos,
			     nof_rows_shown   = NofRowsShown,
			     blinking_enabled = false
			    },

    PbPid ! #pb_key_info{sender       = self(),
			 list_of_keys = KeyList
			},

       % May be new number of elements in the total list!
    ?SCALE_FUNC_FILE:set_scale_range(vscale, Range, ScaleP),
       % May be new vertical scale position required!
    NewScaleP = ?SCALE_FUNC_FILE:set_scale_pos(vscale, Pos, ScaleP),
       % May be new maximum size of elements!
    ?SCALE_FUNC_FILE:set_scale_range(hscale, {1, MaxElemSize}, NewScaleP),
    
    ProcVars#process_variables{data_list       = DataList,
			       color_list      = ColorList,
			       first_row_shown = Pos,
			       initialising    = false,
			       scale_params    = NewScaleP,
			       mark_params     = NewMarkP
			      }.







scroll_vertically(MouseBtn, ProcVars) ->
    #process_variables{scale_params = ScaleP} = ProcVars,
    
    OldScalePos = ScaleP#scale_params.vscale_pos,
    NewScalePos  = get_new_scalepos(MouseBtn, OldScalePos),
    
    case NewScalePos of
	OldScalePos ->
	    ProcVars;
	NewValue ->
	    perform_vertical_scroll(NewValue, ProcVars)	    
    end.







scroll_horizontally(MouseBtn, ProcVars) ->
    #process_variables{scale_params = ScaleP} = ProcVars,
    
    OldScalePos = ScaleP#scale_params.hscale_pos,
    NewScalePos  = get_new_scalepos(MouseBtn, OldScalePos),
    
    case NewScalePos of
	OldScalePos ->
	    ProcVars;
	NewValue ->
	    perform_horizontal_scroll(NewValue, ProcVars)
    end.








perform_vertical_scroll(NewScalePos, ProcVars) ->
    #process_variables{master_pid   = MasterPid,
		       initialising = Init,
		       scale_params = ScaleP} = ProcVars,
    
       %% To avoid erroneous scrollbar signals during creation of the display.
    case Init of
	true ->
	    done;
	false ->
	    MasterPid ! #pc_data_req{sender = self(),
				     element = NewScalePos,
				     nof_elements = ?NOF_GRIDROWS}
    end,
    
       % Since the order of click/buttonrelease messages isn't
       % precise, set the scale to the returned pos (may otherwise 
       % differ one unit).
    NewScaleP = ?SCALE_FUNC_FILE:set_scale_pos(vscale, 
					       NewScalePos, 
					       ScaleP),

    ProcVars#process_variables{scale_params = NewScaleP}.







perform_horizontal_scroll(NewScalePos, ProcVars) ->
    #process_variables{pg_pid       = PgPid,
		       pb_pid       = PbPid,
		       frame_params = FrameP,
		       scale_params = ScaleP} = ProcVars,
    
       % Since the order of click/buttonrelease messages isn't
       % precise, set the scale to the returned pos (may otherwise 
       % differ one unit).
    NewScaleP = ?SCALE_FUNC_FILE:set_scale_pos(hscale, 
					       NewScalePos, 
					       ScaleP),

    PgPid ! #pg_horizontal_scroll{sender = self(),
				  leftmost_virtual_col = NewScalePos
				 },

    #frame_params{grid_frame_width  = GridFrameWidth,
		  grid_frame_height = GridFrameHeight} = FrameP,
    receive
	#pg_col_info{first_col_shown     = FirstColShown,
		     width_of_cols_shown = ColsShown,
		     nof_rows_shown      = NofRowsShown} ->
	    
	    PbPid ! #pb_update_hbtns{parent_width    = GridFrameWidth,
				     parent_height   = GridFrameHeight,
				     first_col_shown = FirstColShown,
				     cols_shown      = ColsShown
				    },
	    
	    ProcVars#process_variables{first_col_shown = FirstColShown,
				       cols_shown      = ColsShown,
				       nof_rows_shown  = NofRowsShown,
				       scale_params    = NewScaleP
				      }
    end.








marked_cell(true, VirtualCol, RealRow, VirtualRow, ProcVars) ->
    #process_variables{master_pid     = MasterPid,
		       rec_pid        = RecPid,
		       data_list      = DataList,
		       color_list     = ColorList,
		       writable       = Writable,
		       mark_params    = MarkP,
		       toolbar_params = ToolP} = ProcVars,
    
    {DataElement, MarkedRowObject} = get_data_element(cell, DataList, RealRow, VirtualCol),
    update_toolbar_label(DataElement, ToolP, VirtualRow, VirtualCol, Writable),
    send_to_rec_edit(RecPid, {update_mode,MarkedRowObject}),

    MarkedRowColor  = lists:nth(RealRow, ColorList),

    MasterPid ! #pc_marked_row{sender = self(),
			       row_no = VirtualRow,
			       object = MarkedRowObject,
			       color  = MarkedRowColor
			      },
    NewMarkP = MarkP#mark_params{cell_col_no    = VirtualCol,
				 row_no         = RealRow,
				 virtual_row_no = VirtualRow,
				 marked_object  = MarkedRowObject,
				 marked_color   = MarkedRowColor
				},
    ProcVars#process_variables{mark_params = NewMarkP
			      };
marked_cell(false, VirtualCol, _RealRow, VirtualRow, ProcVars) ->
    #process_variables{master_pid      = MasterPid, 
		       rec_pid         = RecPid,
		       pb_pid          = PbPid,
		       writable        = Writable,
		       mark_params     = MarkP} = ProcVars,

    PbPid ! #pb_remove_marks{sender = self()},

    case VirtualRow of
	undefined ->
	    done;
	_AnyRow ->
	    update_toolbar_label(notext, ProcVars#process_variables.toolbar_params, 
				 VirtualRow, VirtualCol, Writable),
	    send_to_rec_edit(RecPid, insert_mode)
    end,
    MasterPid ! #pc_marked_row{sender = self(),
			       %% row_no = VirtualRow 
			       row_no = undefined,
			       object = undefined,
			       color  = undefined
			      },
    NewMarkP = MarkP#mark_params{cell_col_no    = undefined,
				 row_no         = undefined,
				 virtual_row_no = undefined,
				 marked_object  = undefined,
				 marked_color   = undefined
				},
    ProcVars#process_variables{mark_params = NewMarkP
			      }.
				  







update_toolbar_label(notext, ToolP, _VirtualRowNo, _VirtualColNo, Writable) ->
    #toolbar_params{row_col_label_id = RowColLblId,
		    fg_label_id      = FgLblId,
		    editor_id        = EdId}  = ToolP,
    gs:config(RowColLblId, [{label, {text,""}}]),
    gs:config(FgLblId, [{enable,true}]),
    gs:config(FgLblId, [{delete, {0,1000000000}}]),
    gs:config(FgLblId, [{insert, {0, ""}}]),
    case Writable of
	true ->
	    gs:config(FgLblId, [{cursor, text},
				{setfocus, true}]);
	false ->
	    gs:config(FgLblId, [{enable, false}, 
				{cursor, arrow},
				{setfocus, false}])
    end,
    update_toolbar_editor(EdId, notext);
update_toolbar_label({DataToShow}, ToolP, VirtualRowNo, VirtualColNo, Writable) ->
    #toolbar_params{row_col_label_id = RowColLblId,
		    fg_label_id      = FgLblId,
		    editor_id        = EdId}  = ToolP,

    case VirtualRowNo of
	undefined ->
	       %%  No row - nothing can possibly be marked!
	    case Writable of
		true ->
		    gs:config(FgLblId, [{setfocus,true},
					{cursor, text}]);
		false ->
		    gs:config(FgLblId, [{enable,false}, 
					{setfocus, false},
					{cursor, arrow}])
	    end;
	_AnyRow ->
	    RowStr  = "R" ++ integer_to_list(VirtualRowNo),
	    ColStr  = case VirtualColNo of
			  undefined ->
			      "";
			  _AnyCol ->
			      " x C" ++ integer_to_list(VirtualColNo)
		      end,
	    DataStr  = lists:flatten(tv_io_lib:format("~p", [DataToShow])),
	    gs:config(RowColLblId, [{label, {text,RowStr++ColStr}}]),
	    gs:config(FgLblId, [{enable,true}]),
	    gs:config(FgLblId, [{delete, {0,10000000}}]),
	    gs:config(FgLblId, [{insert, {0,DataStr}}]),
	    case Writable of
		true ->
		    gs:config(FgLblId, [{setfocus,true},
					{cursor, text}]);
		false ->
		    gs:config(FgLblId, [{enable,false}, 
					{setfocus, false},
					{cursor, arrow}])
	    end,
	    update_toolbar_editor(EdId, {DataToShow})
    end.








get_data_element(row, DataList, RowNo, _VirtualCol) ->
    if
	length(DataList) < RowNo ->
	    {notext, undefined};
	true ->
	    RowObj = lists:nth(RowNo, DataList),
	    {{RowObj}, RowObj}
    end;
get_data_element(cell, DataList, RowNo, ColNo) ->
       %% It's the responsibility of pg to ensure that there is a data item
       %% for the cell marked, meaning we don't *have* to check the length of
       %% the data items. However, since we in the future may want to edit
       %% even empty cells, we check it!
    if
	length(DataList) < RowNo ->
	    {notext, undefined};
	true ->
	    DataItem = lists:nth(RowNo, DataList),
	    if 
		is_tuple(DataItem) ->
		    if size(DataItem) < ColNo ->
			    {notext, DataItem};
		       true ->
			    {{element(ColNo, DataItem)}, DataItem}
		    end;
		true ->
		    {{DataItem}, DataItem}
	    end
    end.








show_toolbar_editor(ProcVars) ->
    #process_variables{frame_params   = FrameP,
		       toolbar_params = ToolP} = ProcVars,
    
    #frame_params{toolbar_frame_height = THeight} = FrameP,
    
    #toolbar_params{editor_frame_id = EdFrameId} = ToolP,
    
    Xpos = 0,
    Ypos = THeight - 8 - ?ROW_COL_LBL_HEIGHT + 1,
    gs:config(EdFrameId, [{x, Xpos}, 
			  {y, Ypos}
			 ]),
    ProcVars.








hide_toolbar_editor(ProcVars) ->
    #process_variables{toolbar_params = ToolP} = ProcVars,
    
    #toolbar_params{editor_frame_id = EdFrameId} = ToolP,
    
    Xpos = 0,
    Ypos = (-1) * gs:read(EdFrameId, height) - 50,
    gs:config(EdFrameId, [{x, Xpos}, 
			  {y, Ypos}
			 ]),
    ProcVars.






%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





update_toolbar_editor(EdId, notext) ->
    gs:config(EdId, [{enable, true}]),
    gs:config(EdId, [clear]),
    gs:config(EdId, [{enable, false}]);
update_toolbar_editor(EdId, {DataToShow}) ->
    Str = io_lib:format("~n~p~n", [DataToShow]),
    gs:config(EdId, [{enable, true}]),
    gs:config(EdId, [clear]),
    gs:config(EdId, [{overwrite, {insert, Str}}]),
    gs:config(EdId, [{enable, false}]).
    





update_marks(true, _DataList, _ColorList, _MarkedRowData, 
	     _Pos, _NofRowsShown, _Writable, _Range, PcPid, PgPid, RecPid, ToolP, MarkP) ->
    PgPid ! #pg_remove_marks{sender = self()},
       %% Too much trouble trying to find the marked object again!
       %% On the other hand, is the mark based on the row number
       %% or the row content? Probably different strategies now, depending
       %% on where in the code we are...  :-(
    %% update_toolbar_label(notext, ToolP, undefined, undefined, Writable),
    update_toolbar_editor(ToolP#toolbar_params.editor_id, notext),
    send_to_rec_edit(RecPid, insert_mode),
    PcPid ! #pc_marked_row{sender = self(),
			   row_no = undefined,
			   object = undefined,
			   color  = undefined
			  },
    MarkP#mark_params{cell_col_no    = undefined,
		      row_no         = undefined,
		      virtual_row_no = undefined,
		      marked_object  = undefined,
		      marked_color   = undefined
		     };
update_marks(false, DataList, ColorList, MarkedRowData, 
	     Pos, NofRowsShown, Writable, Range, PcPid, PgPid, RecPid, ToolP, MarkP) ->
    #mark_params{cell_col_no    = CellColNo,
		 virtual_row_no = VirtualRowNo}  = MarkP,
    
       % Marked row data contains the color also!
    {RowData, RowColors} = split_dblist(MarkedRowData, [], []),

    case VirtualRowNo of
	undefined ->
	    MarkP;
	_AnyRow ->
	    if
		VirtualRowNo > element(2, Range) ->
		       %% Mark outside the existing list! Uh-uh, remove the mark immediately! 8-0
		    update_marks(true, DataList, ColorList, MarkedRowData, Pos, NofRowsShown, 
				 Writable, Range, PcPid, PgPid, RecPid, ToolP, MarkP);
		true ->
		    {DataElement, RowObj} = choose_data_to_show(VirtualRowNo, CellColNo, RowData, 
								DataList, Pos),
		    {_, RowObjColor} = choose_data_to_show(VirtualRowNo, CellColNo, RowColors, 
							   ColorList, Pos),
		    case DataElement of
			notext ->
			    %%	send_to_rec_edit(RecPid, insert_mode);
			    done;
			_OtherElement ->
			    %%  send_to_rec_edit(RecPid, {update_mode, RowObj})
			    send_to_rec_edit(RecPid, {reset_info, RowObj})
		    end,

		    %%    case RowObj of
		    %%	OldMarkedObj ->
		    %%	    done;
		    %%	_NewObj ->
		    %%	    update_toolbar_label(DataElement, ToolP, VirtualRowNo, 
		    %%				 CellColNo, Writable)
		    %%    end,

		    %% update_toolbar_label(DataElement,ToolP,VirtualRowNo,CellColNo,Writable),

		    update_toolbar_editor(ToolP#toolbar_params.editor_id, DataElement),
		    MarkP#mark_params{marked_object = RowObj,
				      marked_color  = RowObjColor}
	    end
    end.
			  


				  

choose_data_to_show(VirtualRowNo, undefined, _RowData, DataList, Pos) when VirtualRowNo >= Pos, VirtualRowNo =< (Pos + length(DataList) - 1) ->
    get_data_element(row, DataList, VirtualRowNo - Pos + 1, undefined);
choose_data_to_show(_VirtualRowNo, undefined, RowData, _DataList, _Pos) ->
    get_data_element(row, RowData, 1, undefined);
choose_data_to_show(VirtualRowNo, CellColNo, _RowData, DataList, Pos)
  when VirtualRowNo >= Pos, VirtualRowNo =< (Pos + length(DataList) - 1) ->
    get_data_element(cell, DataList, VirtualRowNo - Pos + 1, CellColNo);
choose_data_to_show(_VirtualRowNo, CellColNo, RowData, _DataList, _Pos) ->
    get_data_element(cell, RowData, 1, CellColNo).
    





get_new_scalepos(Btn, LastScalePos) ->
    receive
	{gs, _Id, click, _Data, [NewScalePos | _T]} ->
	    get_new_scalepos(Btn, NewScalePos);
	
	{gs, _Id, buttonrelease, _Data, [Btn | _T]} ->
	    LastScalePos;
	
	{gs, _Id, buttonrelease, _Data, _Args} ->
	    get_new_scalepos(Btn, LastScalePos);
	
	{gs, _Id, buttonpress, _Data, _Args} ->
	    get_new_scalepos(Btn, LastScalePos)
    
    end.







split_dblist([], DataAcc, ColorAcc) -> 
    {lists:reverse(DataAcc), lists:reverse(ColorAcc)};
split_dblist([{Data, Color} | Tail], DataAcc, ColorAcc) ->
    split_dblist(Tail, [Data | DataAcc], [Color | ColorAcc]).


    





init_toolbar(FrameP, ToolP) ->
    #frame_params{display_id           = DispId,
		  toolbar_frame_id     = TId,
		  toolbar_frame_width  = TWidth,
		  toolbar_frame_height = THeight,
		  grid_frame_width     = GWidth} = FrameP,

    NewToolP = init_toolbar_btns(TId, ToolP),
    {RowColLblId, BgLabelId, FgLabelId, BtnId} = 
	init_toolbar_label(TId, TWidth, THeight, GWidth),
    
    PopUpFrame = gs:frame(TId, [{width, 80},
				{height, 20},
				{x, 0},
				{y, -30},
				{bg, {0, 0, 0}}
			       ]),
    
    PopUpLabel = gs:label(PopUpFrame, [{width, 78}, 
				       {height, 18}, 
				       {bg, {255,255,190}}, 
				       {x,1}, 
				       {y,1}, 
				       {align, center},
				       {label, {text,""}}, 
				       {font,{screen,12}}]),
    
    {EditorFrameId, EditorId} = init_toolbar_editor(DispId, TWidth, THeight),

    NewToolP#toolbar_params{parent_id        = TId,
			    row_col_label_id = RowColLblId,
			    bg_label_id      = BgLabelId,
			    fg_label_id      = FgLabelId,
			    label_btn_id     = BtnId,
			    pop_up_frame_id  = PopUpFrame,
			    pop_up_label_id  = PopUpLabel,
			    editor_frame_id  = EditorFrameId,
			    editor_id        = EditorId
			   }.






init_toolbar_btns(TId, ToolP) ->
    PicDir = code:priv_dir(tv),
%    PicDir = "../priv",
       % Toolbar btns are 25x25, the bitmaps are 20x20.
    create_one_toolbar_btn(TId, 1, PicDir ++ "/edit1.xbm",
			   {toolbar, insert_object, "Edit Object"}),
    create_one_toolbar_btn(TId, 3, PicDir ++ "/search.xbm",
			   {toolbar, search_object, "Search Object"}),
    create_one_toolbar_btn(TId, 5, PicDir ++ "/sort.xbm",
			   {toolbar, sort_rising_order, "Sort Ascending"}),
    create_one_toolbar_btn(TId, 6, PicDir ++ "/no_sort.xbm",
			   {toolbar, no_sorting,"No Sorting"}),
    create_one_toolbar_btn(TId, 7, PicDir ++ "/sort_reverse.xbm",
			   {toolbar, sort_falling_order,"Sort Descending"}),
    create_one_toolbar_btn(TId, 9, PicDir ++ "/poll.xbm",
			   {toolbar, poll_table,"Poll Table"}),
    create_one_toolbar_btn(TId, 11, PicDir ++ "/info.xbm",
			   {toolbar, table_info,"Table Info"}),
    create_one_toolbar_btn(TId, 13, PicDir ++ "/help.xbm",
			   {toolbar, help_button, "Help"}),
    ToolP.
    







create_one_toolbar_btn(ParentId, N, Image, Data) ->
    BtnWidth  = 25,
    BtnHeight = 25,
    StartXpos = 0,
    BtnXpos   = StartXpos + ((N - 1) * BtnWidth),
    BtnYpos   = 2,
    BgColor   = ?DEFAULT_BG_COLOR,
    FgColor   = {178,34,34},       % Firebrick

    gs:button(ParentId, [{width, BtnWidth},
			 {height, BtnHeight}, 
			 {x, BtnXpos}, 
			 {y, BtnYpos},
			 {enter, true},
			 {leave, true},
			 {label, {image, Image}}, 
			 {data, Data},
			 {fg, FgColor},
			 {bg, BgColor}
			]).
    




resize_toolbar(FrameP, ToolP) ->
    #frame_params{toolbar_frame_width  = TWidth,
		  toolbar_frame_height = THeight,
		  grid_frame_width     = GWidth} = FrameP,
    
    #toolbar_params{bg_label_id      = BgId,
		    fg_label_id      = FgId,
		    row_col_label_id = RowColId,
		    label_btn_id     = BtnId,
		    editor_frame_id  = FrId,
		    editor_id        = EdId} = ToolP,

    resize_toolbar_label(BgId, FgId, RowColId, BtnId, TWidth, THeight, GWidth),
    resize_toolbar_editor(FrId, EdId, TWidth, THeight),
    ToolP.








init_toolbar_label(ParentId, ParentWidth, ParentHeight, GWidth) ->
    {BgWidth, BgHeight, BgXpos, BgYpos, FgWidth, FgHeight, FgXpos, FgYpos, BtnWidth,
     BtnHeight, BtnXpos, BtnYpos} = 
	get_toolbar_label_coords(ParentWidth, ParentHeight),

    BgId = gs:label(ParentId, [{width, BgWidth},
			       {height, BgHeight},
			       {x, BgXpos},
			       {y, BgYpos},
			       {bg, {0, 0, 0}},
			       {fg, {0, 0, 0}}
			      ]),

    
    RowColLblHeight = ?ROW_COL_LBL_HEIGHT,
    RowColLblWidth  = GWidth - ?VBTN_WIDTH,
    RowColLblYpos   = BgYpos + RowColLblHeight + 18,
    
    RowColLblId = gs:label(ParentId, [{width, RowColLblWidth},
				      {height, RowColLblHeight},
				      {x, ?VBTN_WIDTH},
				      {y, RowColLblYpos},
				      {bg, ?DEFAULT_BG_COLOR},
				      {fg, {178,34,34}},
				      {align,center},
				      {font,{screen,12}},				   
				      {label, {text,""}}
				     ]),
    
    FgId = gs:entry(editentry, ParentId, [{width, FgWidth},
					  {height, FgHeight},
					  {x, FgXpos},
					  {y, FgYpos},
					  {bg, {255,255,255}},
					  {fg, {0,0,0}},
					  {bw, 1},
					  {font,{screen,12}},
					  {justify, left},
					  {cursor, arrow},
					  {setfocus, false},
					  {enable, false},
					  {keypress,true}
					 ]),

    PicDir = code:priv_dir(tv),
    BtnId = gs:button(ParentId, [{width, BtnWidth},
				 {height, BtnHeight},
				 {x, BtnXpos},
				 {y, BtnYpos},
				 {bg, ?DEFAULT_BG_COLOR},
				 {fg, {0, 0, 0}},
				 {label, {image, PicDir ++ "/more.xbm"}},
				 {data, {labelbtn, pop_up}}
				]),

    {RowColLblId, BgId, FgId, BtnId}.
    






init_toolbar_editor(DispId, TWidth, THeight) ->
    {BgWidth, BgHeight, BgXpos, BgYpos, Width, Height, Xpos, Ypos} = 
	get_toolbar_editor_coords(TWidth, THeight),
    
    EditorFrame = gs:frame(DispId, [{width, BgWidth},
				    {height, BgHeight},
				    {x, BgXpos},
				    {y, BgYpos},
				    {bg, {0, 0, 0}}
				   ]),

    Editor = gs:editor(EditorFrame, [{width, Width},
				     {height, Height},
				     {x, Xpos},
				     {y, Ypos},
				     {vscroll, right},
				     {wrap, word},
				     {bg, {255, 255, 255}},
				     {fg, {0, 0, 0}},
				     {enable, false}
				    ]),
    
    {EditorFrame, Editor}.
    






get_toolbar_editor_coords(TWidth, _THeight) ->
    BgWidth  = TWidth,
    BgHeight = 200,
    BgXpos   = 0,
    BgYpos   = (-1) * BgHeight - 50,
    FgWidth  = BgWidth - 2,
    FgHeight = BgHeight - 2,
    FgXpos   = 1,
    FgYpos   = 1,

    {BgWidth, BgHeight, BgXpos, BgYpos, FgWidth, FgHeight, FgXpos, FgYpos}.






resize_toolbar_editor(FrId, EdId, TWidth, THeight) ->
    {BgWidth, BgHeight, _BgXpos, _BgYpos, FgWidth, FgHeight, _FgXpos, _FgYpos} = 
	get_toolbar_editor_coords(TWidth, THeight),
    gs:config(FrId, [{width, BgWidth},
		     {height, BgHeight}
		    ]),

    gs:config(EdId, [{width, FgWidth},
		     {height, FgHeight}
		    ]).






resize_toolbar_label(BgId, FgId, RowColId, BtnId, ParentWidth, ParentHeight, GWidth) ->
    {BgWidth, BgHeight, _BgXpos, _BgYpos, FgWidth, FgHeight, _FgXpos, _FgYpos, _BtnWidth,
     _BtnHeight, BtnXpos, BtnYpos} = 
	get_toolbar_label_coords(ParentWidth, ParentHeight),

    gs:config(RowColId, [{width, GWidth - ?VBTN_WIDTH}]),

    gs:config(BgId, [{width, BgWidth},
		     {height, BgHeight}
		    ]),

    gs:config(BtnId, [{x, BtnXpos},
		      {y, BtnYpos}
		     ]),

    gs:config(FgId, [{width, FgWidth},
		     {height, FgHeight}
		    ]).    





get_toolbar_label_coords(ParentWidth, ParentHeight) ->
    BtnWidth  = 19,
    BgWidth   = ParentWidth,
    BgHeight  = 26,
    BgXpos    = 0,
    BgYpos    = ParentHeight - BgHeight - 8 - ?ROW_COL_LBL_HEIGHT + 2,
    FgHeight  = BgHeight - 2,
    FgWidth   = BgWidth - BtnWidth - 3,
    FgXpos    = BgXpos + 1,
    FgYpos    = BgYpos + 1,
    BtnHeight = BgHeight - 2,
    BtnXpos   = FgWidth + 2,
    BtnYpos   = BgYpos + 1,

    {BgWidth, BgHeight, BgXpos, BgYpos, FgWidth, FgHeight, FgXpos, FgYpos, BtnWidth,
    BtnHeight, BtnXpos, BtnYpos}.
    





send_to_rec_edit(undefined, _Msg) ->
    done;
send_to_rec_edit(RecPid, Msg) ->
    RecPid ! Msg.




