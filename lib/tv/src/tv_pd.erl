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
%%%   Description:      Code for pd, i.e., the data displaying part of the table 
%%%                     tool.
%%%
%%%*********************************************************************


-module(tv_pd).



-export([pd/2]).




-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_pd_int_def.hrl").
-include("tv_pd_int_msg.hrl").





%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      pd.
%%
%% Return Value:  None.
%%
%% Description:   Process controlling the display part of the window,
%%                i.e., showing diagrams and handling the scale used for scrolling.
%%
%% Parameters:    None.
%%======================================================================


pd(Master, ErrMsgMode) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrMsgMode),
    PgPid = spawn_link(tv_pg, pg, [self()]),
    PbPid = spawn_link(tv_pb, pb, [self()]),

    ProcVars = #process_variables{master_pid = Master,
				  pg_pid     = PgPid,
				  pb_pid     = PbPid},
    blocked(ProcVars).







%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





%%======================================================================
%% Function:      blocked.
%%
%% Return Value:  None.
%%
%% Description:   When started or explicitly blocked, pd enters this state,
%%                where nothing is performed until the module explicitly is 
%%                deblocked.
%%
%% Parameters:    
%%======================================================================


blocked(ProcVars) ->
    receive
	Msg ->
	    case Msg of 

		#pd_deblock{} ->
		    deblock(Msg, ProcVars);
		

		{error_msg_mode, Mode} ->
		    put(error_msg_mode, Mode),
		    blocked(ProcVars);
		

		_Other ->
		    blocked(ProcVars)
	    end
    end.
    








%%======================================================================
%% Function:      deblock.
%%
%% Return Value:  None.
%%
%% Description:   When deblocked, a canvas and scale shall be created according to 
%%                specification received in pd_deblock message.
%%
%% Parameters:    Rec:  received pd_deblock message.
%%======================================================================



deblock(Msg, ProcVars) ->
    #pd_deblock{win        = WindowId, 
		win_width  = WindowWidth,
		win_height = WindowHeight}          = Msg,

    NewProcVars = ?DISP_FUNC_FILE:init_display(WindowId, WindowWidth, WindowHeight,
					       ProcVars),
    receive 

	#pg_ready{} ->    
	    Sender = Msg#pd_deblock.sender,
	    Sender ! #pd_deblock_cfm{sender = self()},
	    deblocked_loop(NewProcVars)

    end.
    
    






%%======================================================================
%% Function:      deblocked_loop.
%%
%% Return Value:  None.
%%
%% Description:   Eternal (well, almost) loop, receiving messages and 
%%                handling them.
%%
%% Parameters:    Master:  Pid to the 'pc' process.
%%                Win:     Id of the window created.
%%======================================================================



deblocked_loop(ProcVars) ->
    receive
	Msg ->
	    case Msg of

		{gs, Id, Event, Data, Args} ->
		    NewProcVars = gs_messages({Id, Event, Data, Args}, ProcVars),
		    deblocked_loop(NewProcVars);

		_Other ->
		    NewProcVars = tv_messages(Msg, ProcVars),
		    deblocked_loop(NewProcVars)
	    end
    end.
	




tv_messages(Msg, ProcVars) ->
    WinId = ProcVars#process_variables.window_id,

    case Msg of
	#pg_cell_marked{} ->
	    mark_busy(WinId),
	    NewProcVars = handle_cell_marked(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	
	#pc_data{} ->
	    mark_busy(WinId),
	    NewProcVars = show_data(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	
	#pc_list_info{} ->
	    handle_list_info(Msg, ProcVars);

	#pb_col_marked{} ->
	    mark_busy(WinId),
	    NewProcVars = handle_col_marked(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;

	#pb_row_marked{} ->
	    mark_busy(WinId),
	    NewProcVars = handle_row_marked(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	
	#pb_new_colwidth{} ->
	    mark_busy(WinId),
	    NewProcVars = resize_column(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	
	#pd_get_sort_settings{sorting = Sorting, reverse = Reverse} ->
	    mark_busy(WinId),
	    NewProcVars =
		case send_sort_info_signal(Sorting, Reverse, ProcVars) of
		    ignore ->
			ProcVars;
		    TempNewProcVars ->
			set_sort_col(Sorting, TempNewProcVars)
		end,
	    mark_nonbusy(WinId),
	    NewProcVars;

	#pd_new_table{table_type=TabType,table_name=TabName,
		      record_name=RecName,writable=Writable} ->
	    mark_busy(WinId),		    
	    ToolP = ProcVars#process_variables.toolbar_params,
	    ?DISP_FUNC_FILE:update_toolbar_label(notext, ToolP, undefined, undefined, Writable),
	    mark_nonbusy(WinId),
	    ProcVars#process_variables{table_type  = TabType,
				       table_name  = TabName,
				       record_name = RecName,
				       writable    = Writable};

	#pd_win_conf{} ->
	    mark_busy(WinId),
	    NewProcVars = resize_window(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	
	#pd_rec_edit{} ->
	    mark_busy(WinId),
	    NewProcVars = open_rec_edit(Msg, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;


	{updated_object, UpdObj} ->
	    get_updated_elem2(true, UpdObj, ProcVars),
	    ProcVars;
	
	{new_object, NewObj} ->
	    get_updated_elem2(true, NewObj, ProcVars),
	    ProcVars;

	{error_msg_mode, Mode} ->
	    put(error_msg_mode, Mode),
	    ProcVars;
	
	{'EXIT', Pid, Reason} ->
	    exit_signals({Pid, Reason}, ProcVars);
	
	_Other ->
	    ProcVars
    end.






exit_signals(ExitInfo, ProcVars) ->
    #process_variables{master_pid = MasterPid,
		       pg_pid     = PgPid,
		       pb_pid     = PbPid,
		       rec_pid    = RecPid}  = ProcVars,
    
    case ExitInfo of
	{MasterPid, _Reason} ->
	    exit(normal);
	{PgPid, _Reason} ->
	    exit(normal);
	{PbPid, _Reason} ->
	    exit(normal);
	{RecPid, _Reason} ->
	    ProcVars#process_variables{rec_pid = undefined};
	_Other ->
	    ProcVars
    end.




open_rec_edit(Msg, ProcVars) ->
    #pd_rec_edit{attributes = AttrList}  = Msg,
    
    #process_variables{rec_pid          = RecPid,
		       table_type       = TabType,
		       table_name       = TabName,
		       record_name      = RecordName,
		       lists_as_strings = ListsAsStr,
		       mark_params      = MarkP}  = ProcVars,
    
    #mark_params{marked_object = MarkedObject}  = MarkP,
    
    TabOrRecName = 
	case TabType of 
	    mnesia ->
		RecordName;
	    ets ->
		TabName
	end,

    case RecPid of
	undefined ->
	    NewRecPid = 
		case MarkedObject of
		    undefined ->
			tv_rec_edit:start(TabType, TabOrRecName, AttrList, ListsAsStr, 
					  get(error_msg_mode));
		    _Other ->
			AttrVals = 
			    case TabType of
				mnesia ->
				    tl(tuple_to_list(MarkedObject));
				ets ->
				    [MarkedObject]
			    end,
			tv_rec_edit:start(TabType, TabOrRecName, AttrList, AttrVals, ListsAsStr,
					  get(error_msg_mode))
		end,
	    ProcVars#process_variables{rec_pid = NewRecPid};
	_AnyPid ->
	    RecPid ! raise,
	    ProcVars
    end.







gs_messages(Msg, ProcVars) ->
    
    case Msg of

	{editentry, keypress, _Data, ['Tab' | _T]} ->
	    gs:config(editentry, [{select, {0,100000000}}]),
	    ProcVars;

	{editentry, keypress, _Data, ['Return' | _T]} ->
	    get_updated_elem(ProcVars),
	    ProcVars;

	{Id, enter, {toolbar, Btn, Str}, _} ->
	    gs:config(Id, [{motion, true}]),
	    NewProcVars = handle_toolbar_buttons(Id, Btn, Str, false, 0, 0, 
						 ProcVars),
	    NewProcVars;


	{_Id, buttonpress, _Data, [3 | _Rest]} ->
	    ProcVars;


	{_Id, buttonpress, vscale, [MouseBtn | _Tail]} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = ?DISP_FUNC_FILE:scroll_vertically(MouseBtn, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	

	   % The order of messages from gs ought to be 
	   %   1. 'buttonpress' 
	   %   2. 'click'  and
	   %   3. 'buttonrelease'
	   % However, quite often the 'click' message comes last, meaning we have
	   % to check for this.  :-(

	{_Id, click, vscale, [NewScalePos | _Tail]} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = ?DISP_FUNC_FILE:perform_vertical_scroll(NewScalePos, 
								  ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	

	{_Id, buttonpress, hscale, [MouseBtn | _Tail]} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = ?DISP_FUNC_FILE:scroll_horizontally(MouseBtn, ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	

	{_Id, click, hscale, [NewScalePos | _Tail]} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = ?DISP_FUNC_FILE:perform_horizontal_scroll(NewScalePos, 
								    ProcVars),
	    mark_nonbusy(WinId),
	    NewProcVars;
	

	{_Id, click, {toolbar, poll_table, _Str}, _Arg} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_poll_table{sender = self()},
	    mark_nonbusy(WinId),
	    ProcVars;


	{_Id, click, {toolbar, select_browser, _Str}, _Arg} ->
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_select{sender = self()},
	    ProcVars;


	{_Id, click, {toolbar, help_button, _Str}, _Arg} ->
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_help{sender = self()},
	    ProcVars;



	{_Id, click, {toolbar, insert_object, _Str}, _Arg} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! {pc_edit_object, self()},
	    mark_nonbusy(WinId),
	    ProcVars;


	{_Id, click, {toolbar, search_object, _Str}, _Arg} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_search_req{sender = self()},
	    mark_nonbusy(WinId),
	    ProcVars;


	{_Id, click, {toolbar, sort_rising_order, _Str}, _Arg} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = case send_sort_info_signal(true, false, ProcVars) of
			      ignore ->
				  ProcVars;
			      TempNewProcVars ->
				  set_sort_col(true, TempNewProcVars)
			  end,
	    mark_nonbusy(WinId),
	    NewProcVars;


	{_Id, click, {toolbar, sort_falling_order, _Str}, _Arg} ->
	    WinId = ProcVars#process_variables.window_id,
	    mark_busy(WinId),
	    NewProcVars = case send_sort_info_signal(true, true, ProcVars) of
			      ignore ->
				  ProcVars;
			      TempNewProcVars ->
				  set_sort_col(true, TempNewProcVars)
			  end,
	    mark_nonbusy(WinId),
	    NewProcVars;


	{_Id, click, {toolbar, no_sorting, _Str}, _Arg} ->
	    NewProcVars = case send_sort_info_signal(false, false, ProcVars) of
			      ignore ->
				  ProcVars;
			      TempNewProcVars ->
				  set_sort_col(false, TempNewProcVars)
			  end,
	    NewProcVars;
	

	{Id, click, {toolbar, table_info, _Str}, _Arg} ->
	    ToolP = ProcVars#process_variables.toolbar_params,
	    F     = ToolP#toolbar_params.pop_up_frame_id,
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_show_table_info{sender = self()},
	    ProcVars;
	

	{Id, click, {labelbtn, pop_up}, _Arg} ->
	    gs:config(Id, [{data, {labelbtn, pop_down}}]),
	    NewProcVars = ?DISP_FUNC_FILE:show_toolbar_editor(ProcVars),
	    NewProcVars;


	{Id, click, {labelbtn, pop_down}, _Arg} ->
	    gs:config(Id, [{data, {labelbtn, pop_up}}]),
	    NewProcVars = ?DISP_FUNC_FILE:hide_toolbar_editor(ProcVars),
	    NewProcVars;


	_OtherMessage ->
	    ProcVars

    end.
	    




get_updated_elem(ProcVars) ->
    EditedStr = gs:read(editentry, text),
    case tv_db_search:string_to_term(EditedStr) of
	{error, {_Reason, Msg}} ->
	    gs:config(editentry, [beep]),
	    gs:window(pdwin, gs:start(), []),
	    tv_utils:notify(pdwin, "TV Notification", Msg),
	    gs:destroy(pdwin),
	    ProcVars;
	{ok, NewTerm} ->
	    get_updated_elem2(false, NewTerm, ProcVars)
    end.





get_updated_elem2(FromRecEdit, NewTerm, ProcVars) ->
    #process_variables{table_type  = TableType,
		       record_name = RecordName,
		       mark_params = MarkP,
		       master_pid  = PcPid}  = ProcVars,

    #mark_params{marked_object  = ObjToUpdate,
		 marked_color   = ObjColor,
		 virtual_row_no = VirtualRow,
		 cell_col_no    = VirtualCol}  = MarkP,

	case ObjToUpdate of
	    undefined ->
		case new_object_ok(TableType, RecordName, NewTerm) of
		    true ->
			PcPid ! #pd_new_object{sender = self(),
					       object = NewTerm},
			ProcVars;
		    {false, Msg} ->
			gs:window(pdwin, gs:start(), []),
			tv_utils:notify(pdwin, "TV Notification", Msg),
			gs:destroy(pdwin),
			ProcVars
		end;
	    _AnyObj ->
		   %% We need to know if the object has been deleted!
		NewObj = 
		    case VirtualCol of 
			undefined ->
			    NewTerm;
			_AnyCol when FromRecEdit ->
			    NewTerm;
			_AnyCol ->
			    if 
				is_tuple(ObjToUpdate) ->
				    erlang:setelement(VirtualCol, ObjToUpdate, NewTerm);
				true ->
				    NewTerm
			    end
		    end,
		   %% Is the update OK?
		case update_ok(TableType, ObjToUpdate, NewObj) of
		    true ->
			PcPid ! #pd_updated_object{sender     = self(),
						   object     = NewObj,
						   old_object = ObjToUpdate,
						   old_color  = ObjColor,
						   obj_no     = VirtualRow},
			ProcVars;
		    false ->
			gs:window(pdwin, gs:start(), []),
			case get(error_msg_mode) of
			    normal ->
				tv_utils:notify(pdwin, "TV Notification", 
						["The record name cannot be changed!"]);
			    haiku ->
				tv_utils:notify(pdwin, "TV Notification", 
						["The attempt to change",
						 "The permanent record name",
						 "Is simply ignored."])
			end,
			gs:destroy(pdwin),
			ProcVars
		end
	end.




new_object_ok(ets, _RecordName, NewTerm) when is_tuple(NewTerm) ->
    true;
new_object_ok(ets, _RecordName, _NewTerm) ->
    Msg = case get(error_msg_mode) of
	      normal ->
		  ["Object is not a tuple!"];
	      haiku ->
		  ["Yes, it is a term.",
		   "It is pretty, but it's not",
		   "A proper tuple."]
	  end,
    {false, Msg};    
new_object_ok(mnesia, RecordName, NewTerm) when is_tuple(NewTerm) ->
    NewRecName = element(1, NewTerm),
    case NewRecName of
	RecordName ->
	    true;
	_OtherName ->
	    Msg = case get(error_msg_mode) of
		      normal ->
			  ["Erroneous record name!"];
		      haiku ->
			  ["The attempt to use",
			   "An invalid record name",
			   "Is simply ignored."]
		  end,
	    {false, Msg}
    end;
new_object_ok(mnesia, _RecordName, _NewTerm) ->
    Msg = case get(error_msg_mode) of
	      normal ->
		  ["Object is not a record!"];
	      haiku ->
		  ["Yes, it is a term.",
		   "It is pretty, but it's not",
		   "The proper record."]
	  end,
    {false, Msg}.




update_ok(ets, _ObjectToUpdate, _NewObject) ->
    true;
update_ok(mnesia, ObjectToUpdate, NewObject) ->
    OldRecName = element(1, ObjectToUpdate),
    NewRecName = element(1, NewObject),
    case NewRecName of
	OldRecName ->
	    true;
	_Other ->
	    false
    end.




handle_toolbar_buttons(Id, Btn, Str, LabelShown, X, Y, ProcVars) ->
    WinId = ProcVars#process_variables.window_id,
    ToolP = ProcVars#process_variables.toolbar_params,
    F     = ToolP#toolbar_params.pop_up_frame_id,

    receive

	{gs, Id, motion, _Data, [NewX, NewY | _]} ->
	    handle_toolbar_buttons(Id, Btn, Str, LabelShown, NewX, NewY, 
				   ProcVars);

	{gs, editentry, keypress, _Data, ['Tab' | _T]} ->
	    gs:config(editentry, [{select, {0,100000000}}]),
	    handle_toolbar_buttons(Id, Btn, Str, LabelShown, X, Y, ProcVars);

	{gs, editentry, keypress, _Data, ['Return' | _T]} ->
	    get_updated_elem(ProcVars),
	    handle_toolbar_buttons(Id, Btn, Str, LabelShown, X, Y, ProcVars);

	{gs, Id, leave, {toolbar, Btn, Str}, _Arg} ->
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    ProcVars;
	
	{gs, Id, click, {toolbar, poll_table, _Str}, _Arg} ->
	    mark_busy(WinId),
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_poll_table{sender = self()},
	    mark_nonbusy(WinId),
	    ProcVars;

	{gs, Id, click, {toolbar, select_browser, _Str}, _Arg} ->
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_select{sender = self()},
	    ProcVars;

	{gs, Id, click, {toolbar, help_button, _Str}, _Arg} ->
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_help{sender = self()},
	    ProcVars;

	{gs, Id, click, {toolbar, insert_object, _Str}, _Arg} ->
	    mark_busy(WinId),
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! {pc_edit_object, self()},
	    mark_nonbusy(WinId),
	    ProcVars;


	{gs, Id, click, {toolbar, search_object, _Str}, _Arg} ->
	    mark_busy(WinId),
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_search_req{sender = self()},
	    mark_nonbusy(WinId),
	    ProcVars;

	{gs, Id, click, {toolbar, sort_rising_order, _Str}, _Arg} ->
	    mark_busy(WinId),
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    NewProcVars =
		case send_sort_info_signal(true, false, ProcVars) of
		    ignore ->
			ProcVars;
		    TempNewProcVars ->
			set_sort_col(true, TempNewProcVars)
		end,
	    mark_nonbusy(WinId),
	    NewProcVars;

	{gs, Id, click, {toolbar, sort_falling_order, _Str}, _Arg} ->
	    mark_busy(WinId),
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    NewProcVars =
		case send_sort_info_signal(true, true, ProcVars) of
		    ignore ->
			ProcVars;
		    TempNewProcVars ->
			set_sort_col(true, TempNewProcVars)
		end,
	    mark_nonbusy(WinId),
	    NewProcVars;

	{gs, Id, click, {toolbar, no_sorting, _Str}, _Arg} ->
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    NewProcVars =
		case send_sort_info_signal(false, false, ProcVars) of
		    ignore ->
			ProcVars;
		    TempNewProcVars ->
			set_sort_col(false, TempNewProcVars)
		end,
	    NewProcVars;

	{gs, Id, click, {toolbar, table_info, _Str}, _Arg} ->
	    gs:config(F, [{y, -30}]),
	    gs:config(Id, [{motion, false}]),
	    PcPid = ProcVars#process_variables.master_pid,
	    PcPid ! #pc_show_table_info{sender = self()},
	    ProcVars;

	{'EXIT', Pid, Reason} ->
	    exit_signals({Pid, Reason}, ProcVars),
	    handle_toolbar_buttons(Id, Btn, Str, LabelShown, X, Y, ProcVars);

	OtherMsg ->
	    NewProcVars = tv_messages(OtherMsg, ProcVars),
	    handle_toolbar_buttons(Id, Btn, Str, LabelShown, X, Y, NewProcVars)

    after 600 ->
	    case LabelShown of
		false ->
		    FrameP = ProcVars#process_variables.frame_params,
		    L = ToolP#toolbar_params.pop_up_label_id,
		    
		    #frame_params{toolbar_frame_width  = TWidth,
				  toolbar_frame_height = THeight} = FrameP,

		    BtnHeight         = gs:read(Id, height),
		    BtnXpos           = gs:read(Id, x),
		    BtnYpos           = gs:read(Id, y),
		    FrameHeight       = gs:read(F, height),
		    FontUsed          = gs:read(L, font),
		    {StringWidth, _H} = gs:read(L, {font_wh, {FontUsed, Str}}),

		    Width   = StringWidth + 6,
		    Xpos    = BtnXpos + X,
		    LblXpos = if
				  Xpos + Width > TWidth ->
				      Xpos - Width;
				  true ->
				      Xpos
			      end,
		  % Ypos    = BtnYpos + Y + 15,
		    Ypos    = BtnYpos + BtnHeight + 6,
		    LblYpos = if
				  Ypos + FrameHeight > THeight ->
				      Ypos - FrameHeight - 25;
				  true ->
				      Ypos
			      end,
		    gs:config(L, [{width, Width - 2},
				  {label, {text, Str}}]),
		    gs:config(F, [{width, Width},
				  {x, LblXpos},
				  {y, LblYpos}
				 ]);
		true ->
		    done
	    end,
	    handle_toolbar_buttons(Id, Btn, Str, true, X, Y, ProcVars)
    end.



	    


set_sort_col(SortingOn, ProcVars) ->
    #process_variables{pb_pid      = PbPid,
		       mark_params = MarkP} = ProcVars,
    
    SortCol = case SortingOn of
		  true ->
		      MarkP#mark_params.col_no;
		  false ->
		      undefined
	      end,
    PbPid ! #pb_set_sort_col{sender      = self(),
			     virtual_col = SortCol
			    },
    remove_all_marks(SortCol, ProcVars).





send_sort_info_signal(Sorting, Reverse, ProcVars) ->
    #process_variables{master_pid  = PcPid,
		       mark_params = MarkP}  = ProcVars,

    SortColNo = MarkP#mark_params.col_no,

    PcPid ! #pc_set_sorting_mode{sender      = self(),
				 sorting     = Sorting,
				 reverse     = Reverse,
				 sort_key_no = SortColNo
				},
    receive
	#pc_set_sorting_mode_cfm{sort_key_no = FinalSortColNo} ->
	    NewMarkP = MarkP#mark_params{col_no = FinalSortColNo},
	    ProcVars#process_variables{mark_params = NewMarkP};

	#pd_ignore{} ->
	    ignore

    end.
	    




show_data(Msg, ProcVars) ->
    #pc_data{scale_pos        = Pos,
	     scale_range      = Range, 
	     list_range       = MaxValue, 
	     elementlist      = List,
	     list_of_keys     = KeyList,
	     max_elem_size    = MaxElemSize,
	     marked_row       = MarkedRowData}     = Msg,
    
    ?DISP_FUNC_FILE:display_data(Pos, Range, MaxValue, List, KeyList, MaxElemSize, 
				 MarkedRowData, ProcVars).






handle_list_info(Msg, ProcVars) ->
    ListAsStr = Msg#pc_list_info.lists_as_strings,
    PgPid     = ProcVars#process_variables.pg_pid,
    PgPid ! #pg_list_info{sender           = self(),
			  lists_as_strings = ListAsStr},
    ProcVars#process_variables{lists_as_strings = ListAsStr}.
    




handle_col_marked(Msg, ProcVars) ->
    #pb_col_marked{col_marked  = ColMarked,
		   virtual_col = VirtualCol}  = Msg,

    #process_variables{master_pid     = MasterPid,
		       pg_pid         = PgPid,
		       rec_pid        = RecPid,
		       writable       = Writable,
		       toolbar_params = ToolP,
		       mark_params    = MarkP}  = ProcVars,
    SortCol = MarkP#mark_params.sort_col_no,
    
    PgPid ! #pg_remove_marks{sender = self()},

    case ColMarked of
	true ->
	    PgPid ! #pg_col_marked{sender      = self(),
				   virtual_col = VirtualCol};
	false ->
	    done
    end,

    MasterPid ! #pc_marked_row{sender = self(),
			       row_no = undefined,
			       object = undefined,
			       color  = undefined
			      },

    ?DISP_FUNC_FILE:update_toolbar_label(notext, ToolP, undefined, undefined, Writable),
    send_to_rec_edit(RecPid, insert_mode),
    
    NewMarkP = 
	if
	    ColMarked ->
		MarkP#mark_params{col_no = VirtualCol};
	    true ->
		if 
		    SortCol =:= undefined ->
			MarkP;
		    true ->
			MarkP#mark_params{col_no = SortCol}
		end
	end,
    ProcVars#process_variables{mark_params = NewMarkP}.
		      
    




remove_all_marks(SortCol, ProcVars) ->
    #process_variables{master_pid     = MasterPid,
		       pb_pid         = PbPid,
		       pg_pid         = PgPid,
		       toolbar_params = ToolP}  = ProcVars,
    
    PgPid ! #pg_remove_marks{sender = self()},
    PbPid ! #pb_remove_marks{sender = self()},
    MasterPid ! #pc_marked_row{sender = self(),
			       row_no = undefined,
			       object = undefined,
			       color  = undefined
			      },
%%    ?DISP_FUNC_FILE:update_toolbar_label(notext, ToolP, undefined, undefined, Writable),
    ?DISP_FUNC_FILE:update_toolbar_editor(ToolP#toolbar_params.editor_id, notext),
%%    send_to_rec_edit(RecPid, insert_mode),
    ProcVars#process_variables{mark_params = #mark_params{sort_col_no    = SortCol,
							  cell_col_no    = undefined,
							  row_no         = undefined,
							  virtual_row_no = undefined,
							  marked_object  = undefined,
							  marked_color   = undefined}
			      }.






handle_row_marked(Msg, ProcVars) ->
    #pb_row_marked{row_marked  = RowMarked,
		   virtual_row = VirtualRow,
		   real_row    = RealRow}  = Msg,

    #process_variables{master_pid     = MasterPid,
		       rec_pid        = RecPid,
		       pg_pid         = PgPid,
		       data_list      = DataList,
		       color_list     = ColorList,
		       writable       = Writable,
		       toolbar_params = ToolP,
		       mark_params    = MarkP}  = ProcVars,
		       
    PgPid ! #pg_remove_marks{sender = self()},

    case RowMarked of
	true ->
	    PgPid ! #pg_row_marked{sender      = self(),
				   virtual_row = VirtualRow};
	false ->
	    done
    end,

    {DataElement, NewMarkP} = 
	if
	    RowMarked ->
		{MarkedRowOrCol, RowObj} = 
		    ?DISP_FUNC_FILE:get_data_element(row, DataList, RealRow, undefined),
		
		MarkedRowColor =
		    case MarkedRowOrCol of
			notext ->
			    undefined;
			_OtherObject ->
			    lists:nth(RealRow, ColorList)
		    end,
		MasterPid ! #pc_marked_row{sender = self(),
					   row_no = VirtualRow,
					   object = RowObj,
					   color  = MarkedRowColor
					  },
		send_to_rec_edit(RecPid, {update_mode,RowObj}),
		{MarkedRowOrCol, MarkP#mark_params{virtual_row_no = VirtualRow,
						   row_no         = RealRow,
						   cell_col_no    = undefined,
						   col_no         = undefined,
						   marked_object  = RowObj,
						   marked_color   = MarkedRowColor}};
	    true ->
		 MasterPid ! #pc_marked_row{sender = self(),
					    row_no = undefined,
					    object = undefined,
					    color  = undefined
					   },
		send_to_rec_edit(RecPid, insert_mode),
		{notext, MarkP#mark_params{virtual_row_no = undefined,
					   row_no         = undefined,
					   cell_col_no    = undefined,
					   col_no         = undefined,
					   marked_object  = undefined,
					   marked_color   = undefined}}
	end,
    
    ?DISP_FUNC_FILE:update_toolbar_label(DataElement, ToolP, VirtualRow, 
					 undefined, Writable),
    ProcVars#process_variables{mark_params = NewMarkP}.





handle_cell_marked(Msg, ProcVars) ->
    #pg_cell_marked{cell_marked = CellMarked,
		    virtual_col = VirtualCol,
		    real_row    = RealRow,
		    virtual_row = VirtualRow}    = Msg,

       % We are interested in the real row number, since we only have a sublist 
       % stored in pd.
    ?DISP_FUNC_FILE:marked_cell(CellMarked, VirtualCol, RealRow, VirtualRow,
				ProcVars).
				      



resize_window(Msg, ProcVars) ->
    #pd_win_conf{width = NewWindowWidth, 
		 height = NewWindowHeight} = Msg,

    ?DISP_FUNC_FILE:resize_display(NewWindowWidth, NewWindowHeight, ProcVars).




resize_column(Msg, ProcVars) ->
    #pb_new_colwidth{real_col    = RealCol,
		     virtual_col = VirtualCol,
		     xdiff       = Xdiff}       = Msg,
    
    ?DISP_FUNC_FILE:resize_column(RealCol, VirtualCol, Xdiff, ProcVars).




mark_busy(Id) ->
    gs:config(Id, [{cursor, busy}]).
    



mark_nonbusy(Id) ->
    gs:config(Id, [{cursor, arrow}]).




send_to_rec_edit(undefined, _Msg) ->
    done;
send_to_rec_edit(RecPid, Msg) ->
    RecPid ! Msg.

