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
%%%   Description:      Part of pc handling the creation of menus, as well as 
%%%                     treating the signals these menus results in, 
%%%                     when chosen.
%%%
%%%*********************************************************************


-module(tv_pc_menu_handling).



-export([create_menus/1, 
	 exit_button/1, 
	 insert_object/1,
	 delete_object/1,
	 search_object/1,
	 open_table/7, 
	 set_poll_interval/1, 
	 poll_table/1, 
	 sort_rising_order/1, 
	 sort_falling_order/1, 
	 no_sorting/1,
	 lists_as_strings/1,
	 lists_as_lists/1,
	 table_info/1, 
	 help_button/1,
	 otp_help_button/1,
	 get_window_title/4]).





-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pc_int_def.hrl").








%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************


%%    Shortcuts currently used, in alphabetical order:
%% 
%%    c  ->  "Exit"
%%    d  ->  "Delete Object"
%%    f  ->  "Sort Falling Order"
%%    h  ->  "Help"
%%    i  ->  "Table Info"
%%    n  ->  "No Sorting"
%%    o  ->  "Edit Object"
%%    p  ->  "Poll Table"
%%    r  ->  "Sort Rising Order"
%%    s  ->  "Search Object"
%%    v  ->  "Set Poll Interval"
%%    x  ->  "Exit"


create_menus(PwPid) ->
       %% Due to a bug (or some other reason), only one of the radiobuttons belonging
       %% to a specified group can be selected, even if different processes have created
       %% the radiobuttons! This means that, if we have started more than one tv_main 
       %% process, selecting one radiobutton will affect the radiobuttons in the other 
       %% tv_main process(es)!!! Since this is a highly undesirable bahaviour, we have to 
       %% create unique group names (i.e., atoms). 
       %% (We need to group the radiobuttons, since otherwise all created by one process
       %% belongs to the same group, which also is undesirable...)
    SelfStr   = pid_to_list(self()),
    SortGroup = list_to_atom("sorting" ++ SelfStr),
    ListGroup = list_to_atom("lists" ++ SelfStr),

       % Order pw to create the 'File' menu.
    ?GRAPH_FUNC_FILE:create_menu(PwPid,
				 " File ", 
				 1,
				 [{" Table Info ", normal, table_info, 7, i},
				  separator,
				  {" Close ", normal, exit_button, 1, c}
				 ]),
    ?GRAPH_FUNC_FILE:create_menu(PwPid,
				 " Edit ", 
				 1,
				 [{" Edit Object... ", normal, insert_object, 1, o},
				  {" Delete Object ", normal, delete_object, 1, d}
				 ]),
    ?GRAPH_FUNC_FILE:create_menu(PwPid,
				 " View ", 
				 1,
				 [{" Lists as Lists ",{radio,false,ListGroup},lists_as_lists,10,no_char},
				  {" Lists as Strings ",{radio,true,ListGroup},lists_as_strings,10,no_char}
				 ]),
       % Order pw to create the 'Options' menu.
    ?GRAPH_FUNC_FILE:create_menu(PwPid,
				 " Options ", 
				 1,
				 [{" Poll Table ", normal, poll_table, 1, p},
				  {" Poll Interval... ",normal,set_poll_interval,6,no_char},
				  separator,
				  {" Search Object ", normal, search_object, 1, s},
				  separator,
				  {" Sort Ascending Order ",{radio,false,SortGroup},sort_rising_order,6,no_char},
				  {" Sort Descending Order ",{radio,false,SortGroup},sort_falling_order,6,no_char},	
				  {" No Sorting ",{radio,true,SortGroup},no_sorting,1,no_char}
				 ]).
    
    
    


exit_button(_ProcVars) ->
    exit(normal).



help_button(ProcVars) ->
    WinP = ProcVars#process_variables.window_params,
    HelpFile = filename:join([code:lib_dir(tv), "doc", "html", "index.html"]),
    tool_utils:open_help(WinP#window_params.window_id, HelpFile),
    ProcVars.




otp_help_button(ProcVars) ->
    WinP = ProcVars#process_variables.window_params,
    IndexFile = filename:join([code:root_dir(), "doc", "index.html"]),
    
    tool_utils:open_help(WinP#window_params.window_id, IndexFile),
    ProcVars.




table_info(ProcVars) ->
    #process_variables{table_id     = TableId, 
		       current_node = Node,
		       local_node   = LocalNode,
		       table_type   = Type,
		       parent_pid   = ParentPid} = ProcVars,
    
    case TableId of 
	undefined ->
	    done;
	_OtherValue ->
	    ParentPid ! {tv_start_infowin, TableId, Node, LocalNode, Type}
    end,
    ProcVars.



sort_rising_order(ProcVars) ->
    request_sort_settings(ProcVars#process_variables.pd_pid, true, false), 
    ProcVars.
    

sort_falling_order(ProcVars) ->
    request_sort_settings(ProcVars#process_variables.pd_pid, true, true), 
    ProcVars.


no_sorting(ProcVars) ->
    request_sort_settings(ProcVars#process_variables.pd_pid, false, false), 
    ProcVars.    


set_poll_interval(ProcVars) ->
    #process_variables{etsread_pid   = EtsreadPid,
		       poll_interval = PollInterval}  = ProcVars,
    
    case tv_poll_dialog:start(PollInterval) of
	cancel ->
	    ProcVars;
	NewPollInterval ->
	    EtsreadPid ! #etsread_set_poll_interval{sender   = self(),
						    interval = NewPollInterval},
	    ProcVars#process_variables{poll_interval = NewPollInterval}
    end.
	    
	    
    
poll_table(ProcVars) ->
    EtsreadPid = ProcVars#process_variables.etsread_pid,
    EtsreadPid ! #etsread_poll_table{sender = self()},
    ProcVars.
    
    
search_object(ProcVars) ->
    DbsPid = ProcVars#process_variables.dbs_pid,
    DbsPid ! #dbs_search_req{sender=self()},
    ProcVars.



lists_as_strings(ProcVars) ->
    PdPid = ProcVars#process_variables.pd_pid,    
    PdPid ! #pc_list_info{sender=self(), lists_as_strings=true},
    DbsPid = ProcVars#process_variables.dbs_pid,
    DbsPid ! #pc_list_info{sender=self(), lists_as_strings=true},
    ProcVars#process_variables{lists_as_strings=true}.




lists_as_lists(ProcVars) ->
    PdPid = ProcVars#process_variables.pd_pid,
    PdPid ! #pc_list_info{sender=self(), lists_as_strings=false},
    DbsPid = ProcVars#process_variables.dbs_pid,
    DbsPid ! #pc_list_info{sender=self(), lists_as_strings=false},
    ProcVars#process_variables{lists_as_strings=false}.






insert_object(ProcVars) ->
    #process_variables{pd_pid           = PdPid,
		       current_node     = Node,
		       local_node       = LocalNode,
		       table_type       = TabType,
		       table_name       = TabName,
		       table_protection = Protection,
		       window_params    = WinP}  = ProcVars,

    case Protection of
	public ->
	    case TabType of
		mnesia ->
		    case catch tv_mnesia_rpc:table_info(Node, LocalNode, TabName, attributes) of
			nodedown ->
			    handle_error(nodedown);
			no_table ->
			    handle_error(nodedown);
			mnesia_not_started ->
			    handle_error(mnesia_not_started);
			{unexpected_error,Reason} ->
			    handle_error({unexpected_error,Reason});
			AttrList ->
			    PdPid ! #pd_rec_edit{sender      = self(),
						 attributes  = AttrList
						}
		    end;
		ets ->
		    PdPid ! #pd_rec_edit{sender     = self(),
					 attributes = [tuple]
					}
	    end;
	_OtherProtection ->
	    WinId = WinP#window_params.window_id,
	    gs:config(WinId, [beep]),
	    ErrMsg = 
		case get(error_msg_mode) of
		    normal ->
			["The table is protected and",
			 " cannot be edited."];
		    haiku ->
			["The table you see",
			 "Is cunningly protected:",
			 "You can only watch."]
		end,
	    tv_utils:notify(WinId, "TV Notification", ErrMsg)		    
    end,
    ProcVars.






delete_object(ProcVars) ->
    #process_variables{dbs_pid          = DbsPid,
		       table_protection = Protection,
		       marked_row       = MarkedRow,
		       marked_object    = MarkedObject,
		       marked_color     = MarkedColor,
		       window_params    = WinP}  = ProcVars,

    case MarkedRow of
	undefined ->
	    done;
	_AnyRow ->
	    case Protection of
		public ->
		    DbsPid ! #dbs_delete_object{sender = self(),
						object = MarkedObject,
						color  = MarkedColor,
						obj_no = MarkedRow};
		_OtherProtection ->
		    WinId = WinP#window_params.window_id,
		    gs:config(WinId, [beep]),
		    ErrMsg = 
			case get(error_msg_mode) of
			    normal ->
				["The table is protected and",
				 " cannot be edited."];
			    haiku ->
				["The table you see",
				 "Is cunningly protected:",
				 "You can only watch."]
			end,
		    tv_utils:notify(WinId, "TV Notification", ErrMsg)		    
	    end
    end,
    ProcVars.






open_table(CurrNode, LocalNode, TableId, TableType, TableName, Raise, ProcVars) ->
    #process_variables{dbs_pid       = DbsPid,
		       etsread_pid   = EtsreadPid,
		       pw_pid        = PwPid,
		       pd_pid        = PdPid,
		       poll_interval = PollInterval,
		       window_params = WinP}  = ProcVars,
    
    case Raise of
	true ->
	    gs:config(WinP#window_params.window_id, [raise]);
	false ->
	    done
    end,

    {Type, KeyPos, Protection} = init_etsread(EtsreadPid, DbsPid, CurrNode, LocalNode, TableId, 
					      TableType, PollInterval),
    WinTitle = get_window_title(TableType, CurrNode, TableId, TableName),
    PwPid ! #pw_set_window_title{sender    = self(),
				 win_title = WinTitle},
    Writable = 
	case Protection of
	    public ->
		true;
	    _Other ->
		false
	end,
    RecordName = 
	case TableType of
	    mnesia ->
		tv_mnesia_rpc:table_info(CurrNode, LocalNode, TableId, record_name);
	    ets ->
		undefined
	end,
    PdPid ! #pd_new_table{sender      = self(),
			  table_type  = TableType,
			  table_name  = TableName,
			  record_name = RecordName,
			  writable    = Writable},
    init_dbs(DbsPid, Type, KeyPos, EtsreadPid),
    ProcVars#process_variables{current_node     = CurrNode,
			       local_node       = LocalNode, 
			       table_id         = TableId,
			       table_type       = TableType,
			       table_name       = TableName,
			       table_protection = Protection}.






get_window_title(ets, Node, TableId, TableName) ->
    NameStr = lists:flatten(io_lib:write(TableName)),
    TableStr   = case TableId of
		     {TableName, _Pid} ->
			 NameStr;
		     TableName ->
			 NameStr;
		     _Other ->
			 lists:flatten(io_lib:write(TableId)) ++ "  (" ++ NameStr ++ ")"
		 end,
    
    WinTitleSuffix = "      Node:  " ++ atom_to_list(Node),
    "ETS:  " ++ TableStr ++ WinTitleSuffix;
get_window_title(mnesia, Node, _TableId, TableName) ->
    TableNameStr   = lists:flatten(io_lib:write(TableName)),
    WinTitleSuffix = "      Node:  " ++ atom_to_list(Node),
    "Mnesia:  " ++ TableNameStr ++ WinTitleSuffix.




%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************



init_etsread(EtsreadPid, DbsPid, Node, LocalNode, TableId, TableType, PollInterval) ->
    EtsreadPid ! #etsread_deblock{sender        = self(),
				  dbs_pid       = DbsPid,
				  node          = Node,
				  local_node    = LocalNode,
				  table_id      = TableId,
				  table_type    = TableType, 
				  poll_interval = PollInterval
				 },
    receive
	#etsread_deblock_cfm{type=Type, keypos=KeyPos, protection=Protection} ->
	    {Type, KeyPos, Protection}
    after 10000 ->
	    exit(error)
    end.

	


init_dbs(DbsPid, Type, KeyPos, EtsreadPid) ->
    DbsPid ! #dbs_deblock{sender         = self(),
			  etsread_pid    = EtsreadPid,
			  type           = Type,
			  keypos         = KeyPos,
			  sublist_length = ?ITEMS_TO_DISPLAY
			 },
    receive
	#dbs_deblock_cfm{} ->
	    done
    after 10000 ->
	    exit(error)
    end.






request_sort_settings(PdPid, Sorting, Reverse) ->
    PdPid ! #pd_get_sort_settings{sender  = self(),
				  sorting = Sorting,
				  reverse = Reverse
				 }.
	    
	




handle_error(mnesia_not_started) ->
    gs:window(errorwin, gs:start(), []),
    gs:config(errorwin, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(errorwin, "TV Notification", ["Mnesia not started!"]);
	haiku ->
	    tv_utils:notify(errorwin, "TV Notification", ["Mnesia is stopped.",
							  "We wish to reach all data",
							  "But we never will."])
    end,
    gs:destroy(errorwin);
handle_error(nodedown) ->
    done;   %% Main process handles this!
handle_error({unexpected_error,Cause}) ->
    gs:window(errorwin, gs:start(), []),
    io:format("Unexpected error:  ~p~n", [Cause]),
    gs:config(errorwin, [beep]),
    gs:destroy(errorwin).


