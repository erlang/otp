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
%%%   Description:      Internal definitions for the pd part of the table tool.
%%%
%%%*********************************************************************

-define(SCALE_FUNC_FILE, tv_pd_scale).
-define(DISP_FUNC_FILE, tv_pd_display).


-define(SCALE_WIDTH, 75).
-define(VSCALE_WIDTH, 75).
-define(HSCALE_HEIGHT, 75).
-define(MENUBAR_HEIGHT, 30).
-define(TOOLBAR_HEIGHT, 84).  %% 97
-define(DISPLAY_HEIGHT, 849).
-define(MISC_AREA_HEIGHT, 0).
-define(GRID_HEIGHT, 849).
-define(NOF_GRIDROWS, 35). %% 29
-define(NOF_GRIDCOLS, 10).
-define(DEFAULT_COLWIDTH, 100).
-define(ROW_HEIGHT, 20). %% 24
-define(VBTN_WIDTH, 55). %% 18
-define(HBTN_HEIGHT, 20).
-define(RESBTN_WIDTH, 5).
-define(DEFAULT_GRID_BGCOLOR, {255,255,255}).
-define(DEFAULT_GRID_FGCOLOR, {0,0,0}).
-define(GRID_MARK_COLOR, {0,255,255}).
-define(GRID_FONT, {courier,12}).

-define(ROW_COL_LBL_WIDTH, 140).
-define(ROW_COL_LBL_HEIGHT, 14).
    


-define(KEY_MARK_AREA_HEIGHT, 21).


-define(DEFAULT_BG_COLOR, {217,217,217}).
-define(DEFAULT_ROW_COLOR, {178,34,34}).   % Firebrick!
-define(DEFAULT_GRID_COLOR, {0,0,0}). 
-define(LIGHT_GRAY, {226,226,226}).
-define(DARK_VIOLET, {148,0,211}).
-define(FIREBRICK, {178,34,34}).
-define(ANTIQUE_WHITE, {255,255,235}).


-record(frame_params, {display_id,
		       toolbar_frame_id,
		       toolbar_frame_width,
		       toolbar_frame_height,
		       sheet_frame_id,
		       sheet_frame_width,
		       sheet_frame_height,
		       sheet_bgframe_id,
		       grid_frame_id,
		       grid_frame_width,
		       grid_frame_height,
		       grid_bgframe_id
		      }).




-record(scale_params, {vscale_id,
		       vscale_pos = 0,
		       hscale_id,
		       hscale_pos = 0
		      }).




-record(mark_params, {cell_id, 
		      cell_col_no,         % Virtual number!
		      row_no,              % Real number!
		      virtual_row_no,
		      col_no,              % Virtual number!
		      sort_col_no,
		      marked_object,
		      marked_color
		     }).



-record(toolbar_params, {parent_id,
			 row_col_label_id,
			 bg_label_id,
			 fg_label_id,
			 label_btn_id,
			 pop_up_frame_id,
			 pop_up_label_id,
			 editor_frame_id,
			 editor_id
			}).


-record(process_variables, {master_pid,
			    pg_pid,
			    pb_pid,
			    rec_pid,
			    window_id,
			    window_width,
			    window_height,
			    initialising     = true,
			    table_type,
			    table_name,
			    record_name,
			    writable         = false,
			    lists_as_strings = true,
			    sorting_on       = false,
			    first_col_shown  = 1,
			    first_row_shown  = 1,
			    nof_rows_shown,
			    cols_shown       = [],
			    data_list        = [],
			    color_list       = [],
			    frame_params     = #frame_params{},
			    scale_params     = #scale_params{},
			    mark_params      = #mark_params{},
			    toolbar_params   = #toolbar_params{}
			   }).
