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
%%%   Description:      File containing all messages used internally 
%%%                     between the various table tool components.
%%%
%%%*********************************************************************


%%%*********************************************************************
%%% MESSAGES OWNED BY PC
%%%*********************************************************************



-record(pc_raise_window, {sender}).



-record(pc_menu_msg, {sender, 
		      data}).



-record(pc_win_conf, {sender, 
		      width, 
		      height}).



-record(pc_show_table_info, {sender}).



-record(pc_poll_table, {sender}).



-record(pc_select, {sender}).



-record(pc_help, {sender}).



-record(pc_set_sorting_mode, {sender,
			      sorting,         % 'true' or 'false'
			      reverse,         % 'true' or 'false',
			      sort_key_no = 1
			     }).



-record(pc_set_sorting_mode_cfm, {sender,
				  sort_key_no
				 }).



-record(pc_marked_row, {sender,
			row_no,
			object,
			color
		       }).



-record(pc_data_req, {sender, 
		      element, 
		      nof_elements
		     }).



-record(pc_resend_data, {sender}).




-record(pc_data, {sender, 
		  scale_pos,     % vertical scale
		  scale_range,   % vertical scale
		  max_elem_size,
		  list_range, 
		  elementlist,
		  marked_row,
		  list_of_keys,
		  color}).




-record(pc_list_info, {sender,
		       lists_as_strings}).



-record(pc_dead_table, {sender,
			automatic_polling}).



-record(pc_nodedown, {sender,
		      automatic_polling}).



-record(pc_search_req, {sender
		       }).




%%%*********************************************************************
%%% MESSAGES OWNED BY PD
%%%*********************************************************************



-record(pd_win_conf, {sender, 
		      width, 
		      height}).



%%======================================================================
%% Message:   
%%
%% Function:   
%%
%% Data:       sender:      Pid of the sender of the message.
%%             win:         Id of window to create canvas and scale in.
%%             win_width:   width of the window to create the canvas in.
%%             win_height:  height of the window to create the canvas in.
%%             scale:       whether a scale shall be shown or not.
%%                          Possible values: true  -- scale is shown.
%%                                           false -- scale is not shown.
%%======================================================================

-record(pd_deblock, {sender, 
		     win,
		     win_width, 
		     win_height,
		     scale = false,
		     range}).
		     



-record(pd_deblock_cfm, {sender}).



-record(pd_new_table, {sender,
		       table_type,
		       table_name,
		       record_name,   %% Only valid for Mnesia tables.
		       writable
		      }).



-record(pd_get_sort_settings, {sender,
			       sorting,
			       reverse
			      }).



-record(pd_no_sorting, {sender
		       }).




-record(pd_ignore, {sender
		   }).




-record(pd_updated_object, {sender, 
			    object,
			    old_object,
			    old_color,    %% Tells status of the object, if deleted or present.
			    obj_no
			   }).



-record(pd_new_object, {sender,       %% Used when no row is marked. 
			object        %% Note: may still be an updated object!
		       }).



-record(pd_delete_object, {sender, 
			   object,
			   color
			  }).



-record(pd_rec_edit, {sender,
		      attributes
		     }).



%%%*********************************************************************
%%% MESSAGES OWNED BY PW
%%%*********************************************************************




-record(pw_deblock, {sender, 
		     win_title, 
		     win_width,
		     win_height,
		     min_win_width,
		     min_win_height}).
		     


-record(pw_set_window_title, {sender,
			      win_title}).



-record(pw_deblock_cfm, {sender,
			 win_id}).




%%======================================================================
%% Message:    pw_create_menu.
%%
%% Function:   Order to pw to create a menu according to the content of the message.
%%
%% Data:       menutitle: string containing the name of the menu, e.g., "File".
%%             menulist:  list of tuples having the following format: 
%%                        {Text, Data}, where Text is the string that shall be 
%%                        written in each menulist item, and Data is optional data,
%%                        presumably the name of a function that is to be called 
%%                        when the corresponding menulist message is received.
%%======================================================================

-record(pw_create_menu, {sender,
			 menutitle, 
			 title_acc_pos,
			 menulist}).



-record(pw_create_menu_cfm, {sender}).



-record(pw_select_menu, {sender,
			 menu,
			 old_menus}).





%%%*********************************************************************
%%% MESSAGES OWNED BY DBS
%%%*********************************************************************



-record(dbs_deblock, {sender,
		      etsread_pid,
		      type,
		      keypos,
		      sublist_length}).



-record(dbs_deblock_cfm, {sender}).




-record(dbs_new_data, {sender,
		       data,
		       keys,
		       time_to_read_table
		      }).



-record(dbs_new_mnesia_data, {sender,
			      new_or_changed,
			      deleted,
			      keys
			     }).



-record(dbs_subset, {sender,
		     data,
		     requested_row,
		     subset_pos,
		     db_length,
		     max_elem_size,
		     list_of_keys,
		     required_time_etsread,
		     required_time_dbs}).





-record(dbs_subset_req, {sender,
			 subset_pos, 
			 subset_length
			}).




-record(dbs_sorting_mode, {sender,
			   sorting,    % 'true' or 'false'
			   reverse,    % 'true' or 'false'
			   sort_key_no
			  }).
			   


-record(dbs_marked_row, {sender,
			 row_no
			}).




-record(dbs_search_req, {sender
		       }).



-record(dbs_updated_object, {sender,
			     object,
			     old_object,
			     old_color,
			     obj_no
			    }).


-record(dbs_new_object, {sender,
			 object
			}).


-record(dbs_delete_object, {sender,
			    object,
			    color,
			    obj_no
			    }).



%%%*********************************************************************
%%% MESSAGES OWNED BY ETSREAD
%%%*********************************************************************



-record(etsread_update_object, {sender, 
				object,
				old_object,
				key_no
			       }).

-record(etsread_update_object_cfm, {sender,
				    success
				   }).



-record(etsread_new_object, {sender, 
			     object
			    }).


-record(etsread_new_object_cfm, {sender,
				 success
				}).



-record(etsread_delete_object, {sender, 
				object,
				key_no
			       }).


-record(etsread_delete_object_cfm, {sender,
				    success
				   }).



-record(etsread_deblock, {sender,
			  dbs_pid,
			  node,
			  local_node,
			  table_id,
			  table_type,       % One of 'ets' or 'mnesia'
			  poll_interval
			 }).



-record(etsread_deblock_cfm, {sender,
			      type,
			      keypos,
			      protection
			     }).



-record(etsread_set_poll_interval, {sender,
				    interval}).



-record(etsread_poll_table, {sender}).



-record(etsread_nodedown, {sender}).



%%%*********************************************************************
%%% MESSAGES OWNED BY IP
%%%*********************************************************************




-record(ip_dead_table, {sender}).


-record(ip_register_parent, {sender}).



-record(ip_update, {sender,
		    nof_elements_to_mark,
		    text}).



-record(ip_quit, {sender}).




%%%*********************************************************************
%%% MESSAGES OWNED BY INFO
%%%*********************************************************************


-record(info_update_table_info, {sender}).



-record(info_raise_window, {sender}).



-record(info_restart, {sender,
		       node,
		       table_id,
		       table_type}).



-record(info_quit, {sender}).


