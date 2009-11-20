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
%%% MESSAGES OWNED BY PG
%%%*********************************************************************



%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_init_grid, {sender,
		       parent_id,
		       width,
		       height,
		       xpos,
		       ypos,
		       nof_rows,
		       row_height
		      }).



-record(pg_list_info, {sender,
		       lists_as_strings}).


%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_col_info, {sender,
		      first_col_shown,
		      width_of_cols_shown,
		      nof_rows_shown
		     }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_col_marked, {sender,
			virtual_col
		       }).






%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_row_marked, {sender,
			virtual_row
		       }).







%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pg_data, {sender,
		  data,
		  first_row_shown
		 }).






%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pg_cell_marked, {sender,
			 cell_marked,    % true or false
			 real_col,
			 real_row,
			 virtual_col,
			 virtual_row,
			 cell_text
			}).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pg_resize_grid, {sender,
			 width,
			 height
			}).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pg_resize_grid_col, {sender,
			     real_col_no,
			     virtual_col_no,
			     xdiff
			    }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pg_horizontal_scroll, {sender,
			       leftmost_virtual_col
			      }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_ready, {sender}).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_remove_marks, {sender}).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_mark_col, {sender,
		      virtual_col,
		      real_col
		     }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pg_mark_row, {sender,
		      virtual_row,
		      real_row
		     }).






%%%*********************************************************************
%%% MESSAGES OWNED BY PB
%%%*********************************************************************



%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================

-record(pb_init_btns, {sender,
		       parent_id,
		       parent_width,
		       parent_height,
		       ypos,
		       hbtn_height,
		       resbtn_width,
		       vbtn_width,
		       nof_rows,
		       row_height,
		       first_col_shown,
		       cols_shown
		      }).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_remove_marks, {sender}).
			  




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_update_hbtns, {sender,
			  parent_width,
			  parent_height,
			  first_col_shown,
			  cols_shown
			 }).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_update_vbtns, {sender,
			  color_list,
			  first_row_shown,
			  nof_rows_shown,
			  blinking_enabled
			 }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_key_info, {sender,
		      list_of_keys
		     }).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_new_colwidth, {sender,
			  real_col,
			  virtual_col,
			  xdiff
			 }).





%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_col_marked, {sender,
			col_marked,    % 'true' or 'false'
			real_col,
			virtual_col
		       }).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_row_marked, {sender,
			row_marked,    % 'true' or 'false'
			real_row,
			virtual_row
		       }).




%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       
%%======================================================================


-record(pb_set_sort_col, {sender,
			  virtual_col
			 }).




