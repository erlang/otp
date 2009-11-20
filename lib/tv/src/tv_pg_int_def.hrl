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


-define(GRIDFUNCS, tv_pg_gridfcns).



-define(DEFAULT_COLFRAME_HEIGHT, 870).
-define(DEFAULT_COLWIDTH, 100).
-define(DEFAULT_GRID_BGCOLOR, {255, 255, 255}).  % white
-define(DEFAULT_GRID_FGCOLOR, {0, 0, 0}).        % black
-define(GRID_MARK_COLOR, {200, 255, 255}).
-define(GRID_FONT, {courier, 12}).



-define(DEFAULT_BG_COLOR, {217, 217, 217}).
-define(DEFAULT_ROW_COLOR, {178, 34, 34}).   % Firebrick!
-define(DEFAULT_GRID_COLOR, {0, 0, 0}). 
-define(LIGHT_GRAY, {226, 226, 226}).
-define(DARK_VIOLET, {148, 0, 211}).
-define(FIREBRICK, {178, 34, 34}).
-define(ANTIQUE_WHITE, {255, 255, 235}).




-record(grid_params, {bg_frame,
		      fg_frame,
		      grid_width,
		      grid_height       = ?DEFAULT_COLFRAME_HEIGHT, % Actual height,
		                                                    % not the height
		                                                    % shown!
		      grid_xpos,
		      grid_ypos,
		      bg_color          = ?DEFAULT_GRID_BGCOLOR,
		      fg_color          = ?DEFAULT_GRID_FGCOLOR,
		      nof_cols          = 10,
		      nof_rows,
		      nof_rows_shown,
		      row_height,
		      col_width         = ?DEFAULT_COLWIDTH,
		      first_col_shown   = 1,
		      first_row_shown   = 1,
		      max_col_width     = 1200,
		      min_col_width     = 5,
		      col_widths        = [],
		      cols_shown        = [],
		      col_frame_ids     = [],
		      col_ids           = [],
		      row_ids           = [],
		      row_data_list     = [],
		      current_max_value,
		      lists_as_strings   = true
		     }).



-record(mark_params, {cell_id, 
		      virtual_col,
		      virtual_row
		     }).
			



-record(process_variables, {parent_pid,
			    grid_parent_id,
			    grid_params = #grid_params{},
			    mark_params = #mark_params{}
			   }).
