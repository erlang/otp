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
%%%   Description:      Include file for the pc parts of the table tool.
%%%
%%%*********************************************************************


-define(HEAD_FILE, pc).
-define(GRAPH_FUNC_FILE, tv_pc_graph_ctrl).
-define(MENU_FUNC_FILE, tv_pc_menu_handling).



-define(APPLICATION_NAME, "Table Visualizer").
-define(DEFAULT_WINDOW_WIDTH, 750).
-define(DEFAULT_WINDOW_HEIGHT, 600).
-define(DEFAULT_MIN_WINDOW_WIDTH, 300).
-define(DEFAULT_MIN_WINDOW_HEIGHT, 250).


-record(window_params, {window_id,
			window_width,
			window_height
			}).


		 
-record(process_variables, {parent_pid,
			    pw_pid,
			    pd_pid,
			    dbs_pid,
			    etsread_pid,
			    current_node,
			    local_node,
			    table_id      = undefined,
			    table_type    = ets,
			    table_name,
			    table_protection,
			    marked_row,
			    marked_object,
			    marked_color,
			    lists_as_strings = true,
			    poll_interval = infinity,   % seconds or 'infinity'
			    window_params = #window_params{}
			   }).
