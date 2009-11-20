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

-define(WHITE, {255,255,255}).

-define(DEFAULT_BG_COLOR, {217,217,217}).

-define(COL_MARK_COLOR, {0, 0, 0}).
-define(SORT_MARK_COLOR, {255,215,0}).

-define(BLINK_COLOR1, {255,0,0}).
-define(BLINK_COLOR2, {0,255,0}).
-define(BLINK_COLOR3, {0,0,0}).
-define(BTN_FONT, {courier,12}).




-record(col_mark_params, {col_btn_id,
			  virtual_col_marked,
			  sort_btn_id,
			  virtual_sort_col
			 }).



-record(row_mark_params, {virtual_row_marked,
			  real_row_marked
			 }).




-record(process_variables, {parent_pid,
			    grid_frame_id,
			    grid_frame_width,
			    grid_frame_height,
			    ypos,
			    hbtn_height,
			    vbtn_width,
			    resbtn_width,
			    first_col_shown,
			    hbtns_shown      = [],
			    vbtns_shown      = [],
			    resbtns_shown    = [],
			    cols_shown       = [],
			    key_numbers      = [],
			    key_ids          = [],
			    blink_color_list = [?BLINK_COLOR1, 
						?BLINK_COLOR2, 
						?BLINK_COLOR3],
			    col_mark_params  = #col_mark_params{},
			    row_mark_params  = #row_mark_params{}
			   }).



-record(hbtn, {virtual_col,
	       real_col,
	       id,
	       width,
	       xpos
	      }).



-record(resbtn, {virtual_col,
		 real_col,
		 id,
		 width,
		 xpos
		}).



-record(vbtn, {virtual_row,
	       real_row,
	       id,
	       height,
	       ypos
	      }).



