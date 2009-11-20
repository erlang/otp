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
%%%   Description:      pc part of the table tool, i.e., the process 
%%%                     controlling all other processes, and managing
%%%                     the actions to take.
%%%
%%%*********************************************************************


-module(tv_pc).



-export([pc/7,
	 send_data/2
	]).



-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pc_int_def.hrl").




%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************



%%======================================================================
%% Function:      pc.
%%
%% Return Value:  None.
%%
%% Description:   Process controlling the processes 'pd', 'pw', 'dbs' and 'etsread'.
%%                After necessary initialisations, an eternal loop is
%%                entered, where window created messages are received and 
%%                handled, as well as user input.
%%
%% Parameters:    
%%======================================================================


pc(Master, Node, LocalNode, TableId, KindOfTable, TableName, ErrMsgMode) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrMsgMode),
    ProcVars = prepare_and_open_table(Node, LocalNode, TableId, KindOfTable, TableName, 
				false, #process_variables{parent_pid=Master}),
    loop(ProcVars).







%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





prepare_and_open_table(Node, LocalNode, TabId, TabType, TabName, Raise, ProcVars) ->
    IpPid      = spawn(tv_ip, ip, [self()]),
    show_progress(IpPid, 5, "Initializing graphics..."),

    TmpProcVars = start_procs(IpPid, ProcVars),

    show_progress(IpPid, 5, "Loading table..."),
    NewProcVars = ?MENU_FUNC_FILE:open_table(Node, LocalNode, TabId, TabType, TabName, 
					     Raise, TmpProcVars),

    IpPid ! #ip_quit{sender = self()},
       % Now make window visible!
    WinP = NewProcVars#process_variables.window_params,
    gs:config(WinP#window_params.window_id, [{map, true}]),
    NewProcVars.





start_procs(IpPid, ProcVars) ->
    ErrorMsgMode = get(error_msg_mode),
    PwPid        = spawn_link(tv_pw, pw, [self()]),
    PdPid        = spawn_link(tv_pd, pd, [self(), ErrorMsgMode]),
    DbsPid       = spawn_link(tv_db, dbs, [self(), ErrorMsgMode]),
    EtsreadPid   = spawn_link(tv_etsread, etsread, [self(), ErrorMsgMode]),

    show_progress(IpPid, 5, "Initializing graphics..."),
    NewWinP = init_pw(PwPid, ProcVars),

    show_progress(IpPid, 5, "Initializing graphics..."),
    init_pd(PdPid, NewWinP),
    ProcVars#process_variables{pw_pid        = PwPid,
			       pd_pid        = PdPid,
			       dbs_pid       = DbsPid,
			       etsread_pid   = EtsreadPid,
			       current_node  = node(),  %% Will be replaced, when table opened.
			       local_node    = true,
			       window_params = NewWinP
			       }.







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


show_progress(IpPid, NofElements, Text) ->
    IpPid ! #ip_update{sender               = self(),
		       nof_elements_to_mark = NofElements,
		       text                 = Text
		      }.

    





%%======================================================================
%% Function:      loop.
%%
%% Return Value:  None.
%%
%% Description:   Eternal (well, almost) loop, receiving messages and 
%%                handling them.
%%
%% Parameters:    None.
%%======================================================================


loop(ProcVars) ->
    receive
	Msg ->
	    case Msg of 

                   % Normal messages!
		#dbs_subset{} ->
		    NewProcVars1 = send_data(Msg, ProcVars),
		    NewProcVars2 = check_time_to_poll_table(Msg, NewProcVars1),
		    loop(NewProcVars2);

		#pc_poll_table{} ->
		    TmpProcVars  = check_node(ProcVars),
		    NewProcVars = ?MENU_FUNC_FILE:poll_table(TmpProcVars),
		    loop(NewProcVars);

		#pc_search_req{} ->
		    DbsPid = ProcVars#process_variables.dbs_pid,
		    DbsPid ! #dbs_search_req{sender=self()},
		    loop(ProcVars);

		#pc_set_sorting_mode{} ->
		    set_sorting_mode(Msg, ProcVars),
		    loop(ProcVars);


                #pc_data_req{element = Pos, nof_elements = Length} ->
		    DbsPid = ProcVars#process_variables.dbs_pid,
		    DbsPid ! #dbs_subset_req{sender           = self(), 
					     subset_pos       = Pos,
					     subset_length    = Length
					    },
                    loop(ProcVars);


		#pc_marked_row{row_no=RowNo, object=Obj, color=Color} ->
		    DbsPid = ProcVars#process_variables.dbs_pid,
		    DbsPid ! #dbs_marked_row{sender = self(),
					     row_no = RowNo
					    },
		    NewProcVars = ProcVars#process_variables{marked_row    = RowNo,
							     marked_object = Obj,
							     marked_color  = Color},
		    loop(NewProcVars);
		

                #pc_menu_msg{} ->
                    Fcn = Msg#pc_menu_msg.data,
                    NewProcVars = ?MENU_FUNC_FILE:Fcn(ProcVars),
                    loop(NewProcVars);
 

		#pd_updated_object{object=Obj,old_object=OldObj,old_color=Color,obj_no=ObjNo} ->
		    DbsPid = ProcVars#process_variables.dbs_pid,
		    DbsPid ! #dbs_updated_object{sender     = self(),
						 object     = Obj,
						 old_object = OldObj,
						 old_color  = Color,
						 obj_no     = ObjNo},
		    loop(ProcVars);


		#pd_new_object{object=Obj} ->
		    DbsPid = ProcVars#process_variables.dbs_pid,
		    DbsPid ! #dbs_new_object{sender     = self(),
					     object     = Obj},
		    loop(ProcVars);


		#pc_show_table_info{} ->
		    NewProcVars = ?MENU_FUNC_FILE:table_info(ProcVars),
		    loop(NewProcVars);

                #pc_win_conf{} ->
                    NewProcVars = ?GRAPH_FUNC_FILE:win_conf(Msg, ProcVars),
                    loop(NewProcVars);
		
		#pc_help{} ->
		    NewProcVars = ?MENU_FUNC_FILE:help_button(ProcVars),
		    loop(NewProcVars);

		#pc_dead_table{automatic_polling = AutoPoll} ->
		    WinP  = ProcVars#process_variables.window_params,
		    WinId = WinP#window_params.window_id,
		    gs:config(WinId, [beep]),
		    case get(error_msg_mode) of
			normal ->
			    tv_utils:notify(WinId, "TV Notification", 
					    ["The table no longer exists!"]);
			haiku ->
			    ErrMsg1 = ["A table that big?",
				       "It might be very useful.",
				       "But now it is gone."],
			    tv_utils:notify(WinId, "TV Notification", ErrMsg1)
		    end,
		    NewProcVars =
			case AutoPoll of
			    true ->
				gs:config(WinId, [beep]),
				case get(error_msg_mode) of
				    normal ->
					tv_utils:notify(WinId, "TV Notification",
							["The automatic polling is turned off!"]);
				    haiku ->
					ErrMsg2 = ["Previously on",
						  "The polling is now idled.",
						  "That's the way it is."],
					tv_utils:notify(WinId, "TV Notification", ErrMsg2)
				end,
				ProcVars#process_variables{poll_interval = infinity};
			    false ->
				ProcVars
			end,
		    loop(NewProcVars);

		#pc_nodedown{automatic_polling = AutoPoll} ->
		    WinP = ProcVars#process_variables.window_params,
		    WinId = WinP#window_params.window_id,
		    gs:config(WinId, [beep]),
		    case get(error_msg_mode) of
			normal ->
			    tv_utils:notify(WinId, "TV Notification", 
					    ["The node is down, and the",
					     "table cannot be reached."]);
			haiku ->
			    ErrMsg1 = ["With searching comes loss",
				       "And the presence of absence:",
				       "Node is down."],
			    tv_utils:notify(WinId, "TV Notification", ErrMsg1)
		    end,
		    NewProcVars = 
			case AutoPoll of
			    true ->
				gs:config(WinId, [beep]),
				case get(error_msg_mode) of
				    normal ->
					tv_utils:notify(WinId, "TV Notification",
							["The automatic polling is turned off!"]);
				    haiku ->
					ErrMsg = ["Previously on,",
						  "The polling is now idled.",
						  "That's the way it is."],
					tv_utils:notify(WinId, "TV Notification", ErrMsg)
				end,
				ProcVars#process_variables{poll_interval = infinity};
			    false ->
				ProcVars
			end,
		    loop(NewProcVars);
		    

		{pc_edit_object, _Sender} ->
		    NewProcVars = ?MENU_FUNC_FILE:insert_object(ProcVars),
		    loop(NewProcVars);
		    

		check_node ->
		    NewProcVars  = check_node(ProcVars),
		    loop(NewProcVars);


		raise ->
		    WinP = ProcVars#process_variables.window_params,
		    gs:config(WinP#window_params.window_id, [raise]),
		    loop(ProcVars);


		{error_msg_mode, Mode} ->
		    ProcVars#process_variables.dbs_pid ! {error_msg_mode, Mode},
		    ProcVars#process_variables.etsread_pid ! {error_msg_mode, Mode},
		    ProcVars#process_variables.pd_pid ! {error_msg_mode, Mode},
		    put(error_msg_mode, Mode),
		    loop(ProcVars);

		   % Exit messages!
		{'EXIT', Sender, Reason} ->
		    exit_signals({Sender, Reason}, ProcVars);
		

		_Other ->
		    loop(ProcVars)

	    end
    end.
	





check_node(ProcVars) ->
    #process_variables{pw_pid       = PwPid,
		       current_node = OldCurrNode,
		       local_node   = LocalNode,
		       table_id     = TableId,
		       table_type   = TableType,
		       table_name   = TableName}  = ProcVars,

    HomeNode = node(),
    case net_adm:ping(OldCurrNode) of
	pong ->
	    ProcVars;
	pang when not LocalNode ->
	    ProcVars;
	pang when LocalNode ->
	    %% XXX [siri] Will this ever happen? I thought local_node
	    %% indicated if current_node was the node where tv was
	    %% started. If so, we are pinging ourselves here, and
	    %% a pang can never happen??
	    WinTitle = ?MENU_FUNC_FILE:get_window_title(TableType,HomeNode,TableId,TableName),
	    PwPid ! #pw_set_window_title{sender = self(),
					 win_title = WinTitle},
	    ProcVars#process_variables{current_node = HomeNode}
    end.







send_data(Msg, ProcVars) ->
    #process_variables{pd_pid           = PdPid,
		       parent_pid       = ParentPid,
		       table_id         = Table,
		       table_type       = Type,
		       current_node     = Node}  = ProcVars,
    
    ParentPid ! {tv_update_infowin, Table, Node, Type},

    #dbs_subset{data                  = DbData,
		subset_pos            = ScalePos,
		db_length             = DbLength,
		list_of_keys          = ListOfKeys,
		max_elem_size         = MaxElemSize, 
		requested_row         = ReqRowData}  = Msg,

    Range = case ScalePos of
		0 ->
		    {0, 0};
		_Other ->
		    {1, DbLength}
	    end,

    PdPid ! #pc_data{sender           = self(),
		     scale_pos        = ScalePos, 
		     scale_range      = Range, 
		     elementlist      = DbData,
		     list_of_keys     = ListOfKeys,
		     max_elem_size    = MaxElemSize,
		     marked_row       = ReqRowData
		    },
    
    {MarkedObject, MarkedColor} =
	case ReqRowData of
	    [] ->
		{undefined, undefined};
	    [Data] ->
		Data
	end,
    ProcVars#process_variables{marked_object = MarkedObject,
			       marked_color  = MarkedColor}.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_sorting_mode(Msg, ProcVars) ->
    #pc_set_sorting_mode{sorting     = Sorting,
			 reverse     = Reverse,
			 sort_key_no = SortKeyNo} = Msg,

    DbsPid       = ProcVars#process_variables.dbs_pid,
    PdPid        = ProcVars#process_variables.pd_pid,
    PwPid        = ProcVars#process_variables.pw_pid,
    TableType    = ProcVars#process_variables.table_type,

    NewSortKeyNo = 
	case SortKeyNo of 
	    undefined ->
		if
		    TableType =:= mnesia ->
			2;
		    true ->
			1
		end;
	    _Other ->
		SortKeyNo
	end,
    
    Menu = 
	case Sorting of
	    true ->
		case Reverse of 
		    true ->
			sort_falling_order;
		    false ->
			sort_rising_order
		end;
	    false ->
		no_sorting
	end,

    PwPid ! #pw_select_menu{sender = self(),
			    menu   = Menu},
    
    DbsPid ! #dbs_sorting_mode{sender      = self(),
			       sorting     = Sorting,
			       reverse     = Reverse,
			       sort_key_no = NewSortKeyNo
			      },

    PdPid ! #pc_set_sorting_mode_cfm{sender = self(),
				     sort_key_no = NewSortKeyNo
				    }.







%%======================================================================
%% Function:      init_pw.
%%
%% Return Value:  Tuple containing the Pid of the pw process, and the id of 
%%                the window created by the pw process.
%%
%% Description:   Starts the pw process, and orders it to create a window.
%%                (The size of the window may be given as option.)
%%
%% Parameters:    None.
%%======================================================================



init_pw(PwPid, ProcVars) ->
    #process_variables{window_params = WinP}  = ProcVars,

       % Now deblock pw, and order it to create a window!
    PwPid ! #pw_deblock{sender         = self(),
			win_title      = ?APPLICATION_NAME,
			win_width      = ?DEFAULT_WINDOW_WIDTH,
			win_height     = ?DEFAULT_WINDOW_HEIGHT,
			min_win_width  = ?DEFAULT_MIN_WINDOW_WIDTH,
			min_win_height = ?DEFAULT_MIN_WINDOW_HEIGHT
		       },
    

    receive
	#pw_deblock_cfm{win_id = WindowId} ->
	    ?MENU_FUNC_FILE:create_menus(PwPid),
	    
	       % Store the window id as well as the size of it.
	    WinP#window_params{window_id     = WindowId,
			       window_width  = ?DEFAULT_WINDOW_WIDTH,
			       window_height = ?DEFAULT_WINDOW_HEIGHT
			      }

    
    after 180000 ->      % A timeout of 1000 ms is too short, at least the first
	                % time the system is started!
	    exit(error)
    end.
	    
		    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


init_pd(PdPid, WinP) ->
    #window_params{window_id     = WindowId,
		   window_width  = WindowWidth,
		   window_height = WindowHeight} = WinP,
    
       % Now deblock pd, and order it to create a canvas and a scale!
    PdPid ! #pd_deblock{sender      = self(),
			 win        = WindowId,
			 win_width  = WindowWidth,
			 win_height = WindowHeight,
			 scale      = true
			},

    receive
	#pd_deblock_cfm{} ->
	    done
    after 180000 ->
	    exit(error)
    end.
	    
		    


    





%%======================================================================
%% Function:      exit_signals.
%%
%% Return Value:  None.
%%
%% Description:   Decides, given an error message, action to take, i.e., whether
%%                operation shall procede, any process shall be restarted, or
%%                the table tool terminated.
%%
%% Parameters:    Exit_info:  tuple containing sender of the error message, and the 
%%                reason.
%%======================================================================


exit_signals(ExitInfo, ProcVars) ->
    #process_variables{parent_pid   = ParentPid, 
		       pd_pid       = PdPid,
		       pw_pid       = PwPid,
		       dbs_pid      = DbsPid,
		       etsread_pid  = EtsreadPid,
		       table_id     = TabId,
		       table_type   = TabType,
		       table_name   = TabName,
		       current_node = Node,
		       local_node   = LocalNode
		      } = ProcVars,

    case ExitInfo of
	{ParentPid, Reason} ->
	    exit(Reason);

        {PwPid, normal} ->
            exit(normal);

	{PwPid, error} ->
	    io:format("Internal error... restarting. ~n"),
	    kill_procs(normal, [PdPid, EtsreadPid, DbsPid]),
	    NewProcVars = pc(ParentPid, Node, LocalNode, TabId, TabType, TabName, 
			     get(error_msg_mode)),
	    loop(NewProcVars);	    

	{PdPid, _Reason} ->
	    io:format("Internal error... restarting. ~n"),
	    kill_procs(normal, [PwPid, EtsreadPid, DbsPid]),
	    NewProcVars = pc(ParentPid, Node, LocalNode, TabId, TabType, TabName, 
			     get(error_msg_mode)),
	    loop(NewProcVars);
	
	{DbsPid, _Reason} ->
	    io:format("Internal error... restarting. ~n"),
	    kill_procs(normal, [PdPid, PwPid, EtsreadPid]),
	    NewProcVars = pc(ParentPid, Node, LocalNode, TabId, TabType, TabName, 
			     get(error_msg_mode)),
	    loop(NewProcVars);
	
	{EtsreadPid, _Reason} ->
	    io:format("Internal error... restarting. ~n"),
	    kill_procs(normal, [PdPid, PwPid, DbsPid]),
	    NewProcVars = pc(ParentPid, Node, LocalNode, TabId, TabType, TabName, 
			     get(error_msg_mode)),
	    loop(NewProcVars);
	
        {_Sender, _OtherReason} ->
	    loop(ProcVars)
    end.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


kill_procs(_Status, []) ->
    done;
kill_procs(Status, [Pid | Tail]) ->
    exit(Pid, Status),
    kill_procs(Status, Tail).




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


check_time_to_poll_table(Msg, ProcVars) ->
    #dbs_subset{required_time_etsread = EtsreadTime,
		required_time_dbs     = DbsTime}  = Msg,

    UserSetPollInterval = ProcVars#process_variables.poll_interval,
    WinP                = ProcVars#process_variables.window_params,
    WinId               = WinP#window_params.window_id,
    
    case too_short_pollinterval_chosen(UserSetPollInterval, EtsreadTime, DbsTime) of
	true ->
	    EtsreadPid   = ProcVars#process_variables.etsread_pid,
	    EtsreadPid ! #etsread_set_poll_interval{sender   = self(),
						    interval = infinity},
	    
	    TimeRequired = trunc(max_time_required(EtsreadTime, DbsTime) / 10 + 0.5) * 10 + 20,
	    
	    gs:config(WinId, [beep]),
	    case get(error_msg_mode) of
		normal ->
		    tv_utils:notify(WinId, "TV Notification", 
				    ["The current poll interval is too short!"]),
		    Str = "to " ++ lists:flatten(io_lib:write(TimeRequired)) ++ " seconds!",
		    tv_utils:notify(WinId, "TV Notification", ["Setting the poll interval", Str]);
		haiku ->
		    ErrMsg = ["Being way too short",
			      "The interval of polling",
			      "Is simply increased."],
		    tv_utils:notify(WinId, "TV Notification", ErrMsg)		    
	    end,
	    clear_message_buffer(),
	    EtsreadPid ! #etsread_set_poll_interval{sender   = self(),
						    interval = TimeRequired},
	    
	    ProcVars#process_variables{poll_interval = TimeRequired};
	false ->
	    ProcVars
    end.






clear_message_buffer() ->
    receive 
	#dbs_subset{} ->
	    clear_message_buffer()
    after 100 ->
	    done
    end.





max_time_required(T1, T2) when is_number(T1), is_number(T2) ->
    if
	T1 > T2 ->
	    T1;
	true ->
	    T2
    end;
max_time_required(T1, _T2) when is_number(T1) ->
    T1;
max_time_required(_T1, T2) ->
    T2.
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


too_short_pollinterval_chosen(infinity, _EtsreadTime, _DbsTime) ->
    false;
too_short_pollinterval_chosen(undefined, _EtsreadTime, _DbsTime) ->
    false;
too_short_pollinterval_chosen(PollInt, EtsreadTime, _DbsTime) when EtsreadTime >= PollInt, is_number(EtsreadTime) ->
    true;
too_short_pollinterval_chosen(PollInt, _EtsreadTime, DbsTime) when DbsTime >= PollInt, is_number(DbsTime) ->
    true;
too_short_pollinterval_chosen(_PollInt, _EtsreadTime, _DbsTime) ->
    false.
