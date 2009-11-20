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
%%%   Description:      Internal definitions for the pw part of the table tool.
%%%
%%%*********************************************************************

-define(WIN_FUNC_FILE, tv_pw_window).



-define(DEFAULT_WINDOW_WIDTH, 1000).
-define(DEFAULT_WINDOW_HEIGHT, 800).
-define(DEFAULT_MIN_WINDOW_WIDTH, 50).
-define(DEFAULT_MIN_WINDOW_HEIGHT, 50).



-record(window_params, {window_id,
			window_title,
			window_width,
			window_height,
			min_window_width,
			min_window_height
			}).


-record(menu_params, {menubar_id,
		      shortcuts
		     }).





-record(process_variables, {master_pid,
			    window_params  = #window_params{},
			    menu_params    = #menu_params{}
			   }).
