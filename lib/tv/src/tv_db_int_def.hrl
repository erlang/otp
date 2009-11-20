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
%%%   Description:      Internal definitions for the database part of the table 
%%%                     tool.
%%%
%%%*********************************************************************

-define(WHITE,             {255, 255, 255}).
-define(MEDIUM_GRAY,       {170, 170, 170}).


-define(LIGHT_GREEN,       {  0, 255,   0}).
-define(GREEN,             { 50, 215,  50}).
-define(DARK_GREEN,        { 50, 170,  50}).
-define(FOREST_GREEN,      { 34, 139,  34}).
-define(DARK_FOREST_GREEN, { 15, 100,  15}).



-define(RED,               {255,   0,   0}).
-define(PINK,              {255, 130, 170}).
-define(LIGHT_VIOLET,      {220, 150, 225}).
-define(VIOLET,            {160,  70, 180}).
-define(DARK_VIOLET,       {100,  10, 130}).






-record(db_data, {db            = [],     % List containing all elements
		  db_size       = 0,      % Number of elements in 'db'
		  max_elem_size = 0,      % Size of largest element in db.
		  hidden        = [],     % Elements (i.e., keys) not to be shown
		  deleted       = [],     % Elements just deleted
		  subset_size,            % Size of the subset to be extracted and 
		                          % shown
		  subset_pos,             % Position in list where subset starts
		  sorting       = false,  % Tells whether sorting is used ('true' 
		                          % or 'false')
		  requested_row = 0,
		  rev_sorting   = false,  % Tells whether the sorting (if any) is
		                          % in reversed order or not ('true' or 
		                          % 'false')
		  sort_key_no,            % Element in each tuple to use as sorting
		                          % element
		  key_no,                 % Element in each tuple to use as key
		                          % (this element is used when updating the
					  % dblist, i.e., inserting, deleting a.s.o)
		  ets_type                % 'bag' or 'set'
		 }).
                      

-record(process_variables, {master_pid,
			    etsread_pid,
                            db_data      = #db_data{},
			    list_of_keys = [],
			    lists_as_strings = true
                           }).




