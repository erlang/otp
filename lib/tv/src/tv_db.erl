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
%%%   Description:      Module handling the internal database in the table tool.
%%%
%%%*********************************************************************

-module(tv_db).



-export([dbs/2]).



-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_db_int_def.hrl").







%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




dbs(Master, ErrMsgMode) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrMsgMode),
    ProcVars = #process_variables{master_pid = Master},
    blocked(ProcVars).






%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************



blocked(ProcVars) ->
    receive
        Msg ->
            case Msg of 

                #dbs_deblock{} ->
                    deblock(Msg, ProcVars, false);

		{error_msg_mode, Mode} ->
		    put(error_msg_mode, Mode),
		    blocked(ProcVars);

                {'EXIT', Pid, Reason} ->
                    MasterPid = ProcVars#process_variables.master_pid,
                    exit_signals({Pid, Reason}, MasterPid),
                    blocked(ProcVars);

                _Other ->
                    blocked(ProcVars)
            end
    end.







deblock(Msg, ProcVars, SearchWinCreated) ->
    #dbs_deblock{sender         = Sender,
		 etsread_pid    = EtsreadPid,
		 type           = Type,
		 keypos         = KeyPos,
		 sublist_length = SublistLength} = Msg,

    NewDbData = #db_data{subset_size = SublistLength,
			 subset_pos  = 1,
			 key_no      = KeyPos,
			 ets_type    = Type
			},
    NewProcVars = ProcVars#process_variables{db_data     = NewDbData,
					     etsread_pid = EtsreadPid},
    Sender ! #dbs_deblock_cfm{sender = self()},
    deblocked_loop(NewProcVars, SearchWinCreated, [], undefined).
    







deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp) ->
    receive
	Msg ->
	    case Msg of
		
		{gs,entry,keypress,_Data,['Return' | _T]} ->
		    NewSearchData = search_object(ProcVars, RegExp),
		    deblocked_loop(ProcVars, SearchWinCreated, NewSearchData, RegExp);

		{gs,entry,keypress,_Data,['Tab' | _T]} ->
		    gs:config(entry, [{select, {0,1000}}]),
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp);
		
		{gs,entry,keypress,_Data,_Args} ->
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp);

		{gs,expr_term,click,_Data,_Args} ->
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, false);

		{gs,expr_regexp,click,_Data,_Args} ->
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, true);

		{gs,search,click,_Data,_Args} ->
		    NewSearchData = search_object(ProcVars, RegExp),
		    deblocked_loop(ProcVars, SearchWinCreated, NewSearchData, RegExp);
		
		{gs,cancel,click,cancel,_Args} ->
		    tv_db_search:destroy_window(SearchWinCreated),
		    deblocked_loop(ProcVars, false, [], RegExp);
		
		{gs,listbox,click,_LbData,[Idx | _T]} when SearchData =/= [] ->
		    tv_db_search:mark_busy(SearchWinCreated),
		    {Row,_Obj} = lists:nth(Idx+1, SearchData),
		    DbData     = ProcVars#process_variables.db_data,
		       %% Never allow 'subset_pos' to have zero as value! 
		       %% No list can begin with the 0:th element!!! 
		       %% Has to be at least 1!
		    NewDbData  = DbData#db_data{subset_pos=?COMM_FUNC_FILE:max(1,
									       Row),
						subset_size=?ITEMS_TO_DISPLAY},
		    NewProcVars = ProcVars#process_variables{db_data=NewDbData},
		    send_subset(NewProcVars, undefined, undefined),
		    tv_db_search:mark_nonbusy(SearchWinCreated),
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		{gs,win,configure,_Data,_Args} ->
		    tv_db_search:resize_window(SearchWinCreated),
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp);

		{gs,win,destroy,_Data,_Args} ->
		    deblocked_loop(ProcVars, false, [], RegExp);


		#dbs_new_data{data = NewData, keys = ListOfKeys, 
			      time_to_read_table = ElapsedTimeEtsread} ->
		    tv_db_search:reset_window(SearchWinCreated),
		    T1 = time(),
		    NewProcVars = update_db(NewData, ListOfKeys, ProcVars),
		    T2 = time(),
		    ElapsedTimeDbs = compute_elapsed_seconds(T1, T2),
		    send_subset(NewProcVars, ElapsedTimeEtsread, ElapsedTimeDbs),
		    deblocked_loop(NewProcVars, SearchWinCreated, [], RegExp);

		#dbs_subset_req{subset_pos = Pos,subset_length = Length} ->
		    DbData    = ProcVars#process_variables.db_data,
		       %% Never allow 'subset_pos' to have zero as value! 
		       %% No list can begin with the 0:th element!!! 
		       %% Has to be at least 1!
		    NewDbData = DbData#db_data{subset_pos=?COMM_FUNC_FILE:max(1, 
									      Pos),
					       subset_size=Length},
		    NewProcVars = ProcVars#process_variables{db_data = NewDbData},
		    send_subset(NewProcVars, undefined, undefined),
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		#dbs_marked_row{row_no = RowNo} ->
		    DbData      = ProcVars#process_variables.db_data,
		    NewDbData   = DbData#db_data{requested_row = RowNo},
		    NewProcVars = ProcVars#process_variables{db_data = NewDbData},
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);
		    
		#dbs_search_req{} ->
		    tv_db_search:create_window(SearchWinCreated),
		    deblocked_loop(ProcVars, true, SearchData, false);

		#dbs_sorting_mode{} ->
		    {NewProcVars, NewSearchData} = 
			update_sorting_mode(Msg, ProcVars, 
					    SearchWinCreated, SearchData, RegExp),
		    deblocked_loop(NewProcVars, SearchWinCreated, NewSearchData, RegExp);

		#dbs_deblock{} ->
		    tv_db_search:reset_window(SearchWinCreated),
		    deblock(Msg, ProcVars, SearchWinCreated);

		#dbs_updated_object{object=Obj,old_object=OldObj,old_color=Color,obj_no=ObjNo} ->
		    {Success, NewProcVars} = update_object(Obj, OldObj, Color, ObjNo, ProcVars),
		    case Success of
			true ->
			    tv_db_search:reset_window(SearchWinCreated),
			    send_subset(NewProcVars, undefined, undefined);
			false ->
			    done
		    end,
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		#dbs_new_object{object=Obj} ->
		    {Success, NewProcVars} = new_object(Obj, ProcVars),
		    case Success of
			true ->
			    tv_db_search:reset_window(SearchWinCreated),
			    send_subset(NewProcVars, undefined, undefined);
			false ->
			    done
		    end,
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		#dbs_delete_object{object=Obj, color=Color, obj_no=ObjNo} ->
		    {Success, NewProcVars} = delete_object(Obj, Color, ObjNo, ProcVars),
		    case Success of
			true ->
			    tv_db_search:reset_window(SearchWinCreated), 
			    send_subset(NewProcVars, undefined, undefined);
			false ->
			    done
		    end,
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		#pc_list_info{lists_as_strings=ListAsStr} ->
		    NewProcVars = ProcVars#process_variables{lists_as_strings=ListAsStr},
		    deblocked_loop(NewProcVars, SearchWinCreated, SearchData, RegExp);

		{error_msg_mode, Mode} ->
		    put(error_msg_mode, Mode),
		    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp);

                {'EXIT', Pid, Reason} ->
                    MasterPid = ProcVars#process_variables.master_pid,
                    exit_signals({Pid, Reason}, MasterPid),
                    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp);

                _Other ->
                       %% io:format("Received message: ~w ~n", [_Other]),
                    deblocked_loop(ProcVars, SearchWinCreated, SearchData, RegExp)
	    end
    end.







search_object(ProcVars, RegExp) ->
    DbData    = ProcVars#process_variables.db_data,
    DbList    = dblist2list(DbData#db_data.db),
    ListAsStr = ProcVars#process_variables.lists_as_strings,
    case catch tv_db_search:get_input_and_search(DbList, RegExp, ListAsStr) of
	{'EXIT', _Reason} ->
	    tv_db_search:reset_window(true),
	    [];
	List ->
	    List
    end.






update_sorting_mode(Msg, ProcVars, SearchWinCreated, OldSearchData, RegExp) ->
    #dbs_sorting_mode{sorting     = Sorting,
		      reverse     = Reverse,
		      sort_key_no = SortKeyNo} = Msg,

    DbData = ProcVars#process_variables.db_data,

    #db_data{db          = DbList,
	     sorting     = OldSorting,
	     rev_sorting = OldReverse,
	     sort_key_no = OldSortKeyNo} = DbData,
    

    NewDbList = sort_db_list(DbList, Sorting, OldSorting, Reverse, OldReverse, 
			     SortKeyNo, OldSortKeyNo),
    
    NewDbData = DbData#db_data{db          = NewDbList,
			       sorting     = Sorting,
			       rev_sorting = Reverse,
			       sort_key_no = SortKeyNo
			      },

    NewProcVars = ProcVars#process_variables{db_data = NewDbData},
    send_subset(NewProcVars, undefined, undefined),

    SearchData = 
	case Sorting of
	    false ->
		OldSearchData;
	    OldSorting when Reverse =:= OldReverse,
			    SortKeyNo =:= OldSortKeyNo ->
		[];
	    OldSorting when Reverse =:= OldReverse,
			    OldSortKeyNo =:= undefined->
		[];
	    _Other ->
		ListAsStr = ProcVars#process_variables.lists_as_strings,
		case catch tv_db_search:update_search(SearchWinCreated, 
						      NewDbList, RegExp,
						      ListAsStr) of
		    {'EXIT', _Reason} ->
			tv_db_search:reset_window(true),
			[];
		    List ->
			List
		end
	end,

    {NewProcVars, SearchData}.








sort_db_list(DbList, Sort, Sort, Rev, Rev, KeyNo, KeyNo) ->
       % Already sorted!
    DbList;
sort_db_list(DbList, false, _OldSort, _Rev, _OldRev, _KeyNo, _OldKeyNo) ->
       % No sorting, i.e., the old list order suffices!
    DbList;
sort_db_list(DbList, _Sort, _OldSort, Rev, _OldRev, KeyNo, _OldKeyNo) ->
    tv_db_sort:mergesort(KeyNo, DbList, Rev).
    






send_subset(ProcVars, EtsreadTime, DbsTime) ->
    #process_variables{master_pid    = MasterPid,
		       db_data       = DbData,
		       list_of_keys  = ListOfKeys}   = ProcVars,
    
    #db_data{subset_size   = SubsetSize,
	     subset_pos    = SubsetPos,
	     requested_row = RowNo,
	     db_size       = DbSize,
	     db            = DbList,
	     max_elem_size = MaxElemSize}  = DbData,
    

    RowData = get_requested_row_data(RowNo, DbList),

    if 
	DbSize > 0 ->
	    Pos = ?COMM_FUNC_FILE:min(SubsetPos, DbSize),
	       % Requested_data may be shorter than requested, but that's OK,
               % pd handles that correctly!
	    Subset = lists:sublist(DbList, Pos, SubsetSize),
	    MasterPid ! #dbs_subset{sender                = self(),
				    data                  = Subset,
				    subset_pos            = Pos,
				    db_length             = DbSize,
				    list_of_keys          = ListOfKeys,
				    max_elem_size         = MaxElemSize,
				    requested_row         = RowData,
				    required_time_etsread = EtsreadTime,
				    required_time_dbs     = DbsTime
				   };
	true ->
	    MasterPid ! #dbs_subset{sender                = self(),
				    data                  = [],
				    subset_pos            = 1,
				    db_length             = 0,
				    list_of_keys          = ListOfKeys,
				    max_elem_size         = MaxElemSize,
				    requested_row         = RowData,
				    required_time_etsread = EtsreadTime,
				    required_time_dbs     = DbsTime
				   }
    end.
    
    



get_requested_row_data(undefined, _DbList) ->
    [];
get_requested_row_data(_RowNo, []) ->
    [];
get_requested_row_data(RowNo, DbList) ->
    case catch lists:nth(RowNo, DbList) of
	{'EXIT', _Reason} ->
	    [];
	RowData ->
	    [RowData]
    end.




exit_signals(ExitInfo, MasterPid) ->
    case ExitInfo of
        {MasterPid, _Reason} ->
               % When from master, just quit!
            exit(normal);
        _Other ->
            done
    end.




update_db(NewList, ListOfKeys, ProcVars) ->
    DbData = ProcVars#process_variables.db_data,
    #db_data{db            = OldDbList,
	     max_elem_size = MaxElemSize,
	     deleted       = DelList,
	     ets_type      = EtsType,
	     sorting       = Sorting,
	     rev_sorting   = RevSorting,
	     sort_key_no   = SortKeyNo,
	     key_no        = KeyNo} = DbData,
    
    DbList   = update_colors(OldDbList -- DelList),
    OldList  = dblist2list(DbList),
    InsOrUpd = (NewList -- OldList),
    DelOrUpd = (OldList -- NewList),
    
    {Inserted, Deleted, Updated} = group_difflists(basetype(EtsType), KeyNo, 
						   InsOrUpd, 
						   DelOrUpd),
    DelMarked = mark_deleted(KeyNo, Deleted, DbList),
    Replaced  = replace_elements(KeyNo, Updated, DelMarked),
    NewDbList = add_elements(KeyNo, Inserted, Replaced, Sorting, RevSorting, 
			     SortKeyNo),

    NewMaxSize = ?COMM_FUNC_FILE:max(MaxElemSize, 
				     ?COMM_FUNC_FILE:max(max_size(Replaced), 
							 max_size(Inserted))),

    NewDbData = DbData#db_data{db            = NewDbList,
			       db_size       = length(NewDbList),
			       max_elem_size = NewMaxSize,
			       deleted       = list2dblist(Deleted, ?BLACK)
			      },

    ProcVars#process_variables{db_data = NewDbData,
			       list_of_keys = ListOfKeys
			      }.
    





update_object(Obj, OldObj, OldColor, ObjNo, ProcVars) ->
    #process_variables{db_data     = DbData,
		       etsread_pid = EtsreadPid}  = ProcVars,
    
    #db_data{key_no        = KeyNo} = DbData,

       %% Don't update if there are no changes!
    case OldObj of
	Obj when OldColor =/= ?BLACK ->   %% Allow deleted objects to be inserted!
	    gs:window(dbwin, gs:start(), []),
	    case get(error_msg_mode) of
		normal ->
		    tv_utils:notify(dbwin, "TV Notification", ["The object is unchanged!"]);
		haiku ->
		    tv_utils:notify(dbwin, "TV Notification", 
				    ["Stay the patient course,",
				     "Of little worth is your ire:",
				     "The object's unchanged." ])
	    end,
	    gs:destroy(dbwin),
	    {false, ProcVars};
	_Other ->
	       %% Before we try to update the internal database, we have to check to see
	       %% whether the ETS/Mnesia update is allowed!
	    Result = 
		case OldColor of
		    ?BLACK ->
			EtsreadPid ! #etsread_new_object{sender     = self(),
							 object     = Obj}, 
			receive 
			    #etsread_new_object_cfm{success = Success} ->
				Success
			after 
			    60000 ->
				exit(etsread_not_responding)
			end;
		    _OtherColor ->
			EtsreadPid ! #etsread_update_object{sender     = self(),
							    key_no     = KeyNo,
							    object     = Obj,
							    old_object = OldObj}, 
			receive 
			    #etsread_update_object_cfm{success = Success} ->
				Success
			after 
			    60000 ->
				exit(etsread_not_responding)
			end
		end,
	    case Result of
		false ->
		    gs:window(dbwin, gs:start(), [beep]),
		    case get(error_msg_mode) of
			normal ->
			    tv_utils:notify(dbwin, "TV Notification", 
					    ["Couldn't update table!"]);
			haiku ->
			    tv_utils:notify(dbwin, "TV Notification", 
					    ["Three things are certain:",
					     "Death, taxes, and lost updates.",
					     "Guess which has occurred."])
		    end,
		    gs:destroy(dbwin),
		    {false, ProcVars};
		true ->
		    {true, update_object2(Obj, OldObj, OldColor, ObjNo, ProcVars)}
	    end
    end.





update_object2(Obj, OldObj, OldColor, ObjNo, ProcVars) ->
    #process_variables{db_data = DbData}  = ProcVars,
    
    #db_data{db            = DbList,
	     ets_type      = EtsType,     %% 'bag', 'set', 'ordered_set' or 
					  %% 'duplicate_bag'
	     max_elem_size = MaxElemSize,
	     sorting       = Sorting,
	     rev_sorting   = RevSorting,
	     sort_key_no   = SortKeyNo,
	     key_no        = KeyNo} = DbData,

       %% Replace the old element...
    Key    = element(KeyNo, Obj),
    OldKey = element(KeyNo, OldObj),
       %% If Key == OldKey, the old object shall only be replaced!
       %% Otherwise the updated object shall be treated as a new 
       %% object when inserting it in the list!
       %% In that latter case, we also have to check for duplicates!

    Fun = 
	case basetype(EtsType) of
	    set ->
		case Key of
		    OldKey ->
			fun({Data,Color}, {Replaced,AccDb}) when element(KeyNo,Data) =/= Key ->
				{Replaced, [{Data,Color} | AccDb]};
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =:= ?BLACK,
								 Color =:= ?BLACK ->
				{true, [{Obj,?RED1} | AccDb]};
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =/= ?BLACK,
								 Color =/= ?BLACK ->
				{true, [{Obj,?GREEN1} | AccDb]};
			   ({_Data,_Color}, {Replaced,AccDb}) ->
				{Replaced, AccDb}
			end;
		    _NewKey ->
			fun({Data,Color}, {Replaced,AccDb}) ->
				ElemKey = element(KeyNo,Data),
				case ElemKey of
				    OldKey when not Replaced,
						OldColor =:= ?BLACK,
						Color    =:= ?BLACK ->
					{true, [{Obj,?RED1} | AccDb]};
				    OldKey when not Replaced,
						OldColor =/= ?BLACK,
						Color =/= ?BLACK ->
					{true, [{Obj,?GREEN1} | AccDb]};
				    OldKey ->
					{Replaced, AccDb};
				    Key ->
					{Replaced, AccDb};
				    _OtherKey ->
					{Replaced, [{Data,Color} | AccDb]}
				end
			end
		end;

	    bag ->
		case Key of
		    OldKey ->
			fun({Data,_Color}, {Replaced,AccDb}) when Data =:= Obj ->
				{Replaced, AccDb};
			   ({Data,Color}, {Replaced,AccDb}) when Data =/= OldObj ->
				{Replaced, [{Data,Color} | AccDb]};
			      %% Clauses when Data =:= OldObj.
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =:= ?BLACK,
								 Color =:= ?BLACK ->
				{true, [{Obj,?RED1} | AccDb]};
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =/= ?BLACK,
								 Color =/= ?BLACK ->
				{true, [{Obj,Color} | AccDb]};
			   ({_Data,_Color}, {Replaced,AccDb}) ->
				{Replaced, AccDb}
			end;
		    _NewKey ->
			fun({Data,Color}, {Replaced,AccDb}) when Data =:= OldObj,
								 not Replaced,
								 OldColor =:= ?BLACK,
								 Color =:= ?BLACK ->
				{true, [{Obj,?RED1} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) when Data =:= OldObj,
								 not Replaced,
								 OldColor =/= ?BLACK,
								 Color =/= ?BLACK ->
				{true, [{Obj,?GREEN1} | AccDb]};
			   ({Data,_Color}, {Replaced,AccDb}) when Data =:= OldObj ->
				{Replaced, AccDb};
			   ({Data,_Color}, {Replaced,AccDb}) when Data =:= Obj ->
				{Replaced, AccDb};
			   ({Data,Color}, {Replaced,AccDb}) ->
				{Replaced, [{Data,Color} | AccDb]}
			end
		end;

	    duplicate_bag ->
		   %% Multiple identical objects allowed, meaning that we shall not
		   %% remove anything, just replace one element.
		case Key of
		    OldKey ->
			fun({Data,Color}, {Replaced,AccDb}) when Data =:= Obj ->
				{Replaced, [{Data,Color} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) when Data =/= OldObj ->
				{Replaced, [{Data,Color} | AccDb]};
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =:= ?BLACK,
								 Color =:= ?BLACK ->
				{true, [{Obj,?RED1} | AccDb]};
			   ({_Data,Color}, {Replaced,AccDb}) when not Replaced,
								 OldColor =/= ?BLACK,
								 Color =/= ?BLACK ->
				{true, [{Obj,Color} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) ->
				{Replaced, [{Data,Color} | AccDb]}
			end;
		    _NewKey ->
			fun({Data,Color}, {Replaced,AccDb}) when Data =:= OldObj,
								 not Replaced,
								 OldColor =:= ?BLACK,
								 Color =:= ?BLACK ->
				{true, [{Obj,?RED1} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) when Data =:= OldObj,
								 not Replaced,
								 OldColor =/= ?BLACK,
								 Color =/= ?BLACK ->
				{true, [{Obj,?GREEN1} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) when Data =:= OldObj ->
				{Replaced, [{Data,Color} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) when Data =:= Obj ->
				{Replaced, [{Data,Color} | AccDb]};
			   ({Data,Color}, {Replaced,AccDb}) ->
				{Replaced, [{Data,Color} | AccDb]}
			end
		end
	end,
    
    FilterFun = fun(Acc0, L) ->
			lists:foldl(Fun, Acc0, L)
		end,
    
    
    {Repl, TmpList} =
	case split(ObjNo, DbList) of
	    {L1, [{OldObj,OldColor} | T]} when OldColor =/= ?BLACK ->
		{true, 
		 lists:reverse(element(2, FilterFun({true,[]}, L1))) ++ 
		 [{Obj,?GREEN1} | lists:reverse(element(2, FilterFun({true,[]},T)))]};
	    {L1, [{OldObj,OldColor} | T]} ->
		{true, 
		 lists:reverse(element(2, FilterFun({true,[]}, L1))) ++ 
		 [{Obj,?RED1} | lists:reverse(element(2, FilterFun({true,[]}, T)))]};
	    {L1, L2} ->
		{R1, NewL1} = FilterFun({false,[]}, L1),
		{R2, NewL2} = FilterFun({false,[]}, L2),
		{R1 or R2, lists:reverse(NewL1) ++ lists:reverse(NewL2)}
	end,

    NewDbList = 
	case Repl of
	    true when not Sorting ->
		TmpList;
	    true ->
		tv_db_sort:mergesort(SortKeyNo, TmpList, RevSorting);
	    false ->
		TmpList2 = 
		    case Key of
			OldKey ->
			    lists:reverse(element(2, FilterFun({false,[]}, TmpList)));
			_OtherKey ->
			    lists:reverse(element(2, FilterFun({true,[]}, TmpList))) ++ 
				[{Obj,?RED1}]
		    end,
		case Sorting of
		    false ->
			TmpList2;
		    true ->
			tv_db_sort:mergesort(SortKeyNo, TmpList2, RevSorting)
		end
	end,
    NewMaxSize = ?COMM_FUNC_FILE:max(MaxElemSize, max_size([Obj])),
    NewDbData = DbData#db_data{db            = NewDbList,
			       db_size       = length(NewDbList),
			       max_elem_size = NewMaxSize
			      },
    ProcVars#process_variables{db_data = NewDbData}.





delete_object(_Obj, ?BLACK, _ObjNo, ProcVars) ->
       %% Don't delete already deleted objects!!!
    {false, ProcVars};
delete_object(undefined, undefined, _ObjNo, ProcVars) ->
    {false, ProcVars};
delete_object(Obj, _ObjColor, ObjNo, ProcVars) ->
    #process_variables{db_data     = DbData,
		       etsread_pid = EtsreadPid}  = ProcVars,
    
    #db_data{db            = DbList,
	     deleted       = OldDeleted} = DbData,
    
       %% Before we try to update the internal database, we have to check to see
       %% whether the ETS/Mnesia update is allowed!
    EtsreadPid ! #etsread_delete_object{sender     = self(),
					object     = Obj}, 
    Result = 
	receive 
	    #etsread_delete_object_cfm{success = Success} ->
		Success
	after
	    60000 ->
		exit(etsread_not_responding)
	end,
    
    case Result of
	false ->
	    gs:window(dbwin, gs:start(), [beep]),
	    case get(error_msg_mode) of
		normal ->
		    tv_utils:notify(dbwin, "TV Notification", 
				    ["Couldn't update table!"]);
		haiku ->
		    tv_utils:notify(dbwin, "TV Notification", 
				    ["Three things are certain:",
				     "Death, taxes, and lost updates.",
				     "Guess which has occurred."])
	    end,
	    gs:destroy(dbwin),
	    {false, ProcVars};
	true ->
	       %% Replace the old element...
	       %% Have to beware of duplicate_bag tables,
	       %% i.e., the same object may occur more than
	       %% once, but we only want to remove it once!
	    {Repl, TmpList} =
		case split(ObjNo, DbList) of
		    {L1, [{Obj,_Color} | T]} ->
			{true, L1 ++ [{Obj,?BLACK} | T]};
		    {L1, L2} ->
			{false, L1 ++ L2}
		end,
	    NewDbList = 
		case Repl of
		    true ->
			TmpList;
		    false ->
			Fun = fun({Data,TmpColor}, 
				  {Removed,AccDb}) when Data =/= Obj ->
				      {Removed, [{Data,TmpColor} | AccDb]};
				 ({_Data,TmpColor}, 
				  {Removed,AccDb}) when not Removed, TmpColor =/= ?BLACK ->
				      {true, [{Obj,?BLACK} | AccDb]};
				 ({Data,TmpColor}, 
				  {Removed,AccDb}) ->
				      {Removed, [{Data,TmpColor} | AccDb]}
			      end,
			lists:reverse(element(2, lists:foldl(Fun, {false,[]}, DbList)))
		end,
	    NewDbData = DbData#db_data{db      = NewDbList,
				       db_size = length(NewDbList),
				       deleted = [{Obj,?BLACK} | OldDeleted]},
	    {true, ProcVars#process_variables{db_data = NewDbData}}
    end.





new_object(Obj, ProcVars) ->
    #process_variables{db_data     = DbData,
		       etsread_pid = EtsreadPid}  = ProcVars,
    
    #db_data{db            = DbList,
	     max_elem_size = MaxElemSize,
	     ets_type      = EtsType,     %% 'bag', 'set' or 'duplicate_bag'
	     sorting       = Sorting,
	     rev_sorting   = RevSorting,
	     sort_key_no   = SortKeyNo,
	     key_no        = KeyNo} = DbData,

       %% Before we try to update the internal database, we have to check to see
       %% whether the ETS/Mnesia update is allowed!
    EtsreadPid ! #etsread_new_object{sender     = self(),
				     object     = Obj}, 
    Result = 
	receive 
	    #etsread_new_object_cfm{success = Success} ->
		Success
	after
	    60000 ->
		exit(etsread_not_responding)
	end,
    
    case Result of
	false ->
	    gs:window(dbwin, gs:start(), [beep]),
	    case get(error_msg_mode) of
		normal ->
		    tv_utils:notify(dbwin, "TV Notification", 
				    ["Couldn't update table!"]);
		haiku ->
		    tv_utils:notify(dbwin, "TV Notification", 
				    ["Three things are certain:",
				     "Death, taxes, and lost updates.",
				     "Guess which has occurred."])
	    end,
	    gs:destroy(dbwin),
	    {false, ProcVars};
	true ->
	    Key = element(KeyNo, Obj),
	    NewDbList = insert_new_object(EtsType, Key, KeyNo, Obj, DbList, Sorting, 
					  RevSorting, SortKeyNo),
	    NewMaxSize = ?COMM_FUNC_FILE:max(MaxElemSize, max_size([Obj])),
	    NewDbData = DbData#db_data{db            = NewDbList,
				       db_size       = length(NewDbList),
				       max_elem_size = NewMaxSize
				      },
	    {true, ProcVars#process_variables{db_data = NewDbData}}
    end.





insert_new_object(EtsType,Key,KeyNo,Obj,DbList,Sorting,RevSorting,SortKeyNo) ->
       %% Remove elements from the list that ought not to be there,
       %% according to the table type!

    Fun = 
	case basetype(EtsType) of
	    set ->
		fun({Data,Color}, {Replaced,AccDb}) when element(KeyNo,Data) =/= Key ->
			{Replaced, [{Data,Color} | AccDb]};
		   ({Data,Color}, {Replaced,AccDb}) when not Replaced, 
							 Color =/= ?BLACK,
							 Data =/= Obj->
			{true, [{Obj,?GREEN1} | AccDb]};
		   ({_Data,Color}, {Replaced,AccDb}) when not Replaced, 
							 Color =/= ?BLACK ->
			{true, [{Obj,Color} | AccDb]};
		   ({_Data,Color}, {Replaced,AccDb}) when not Replaced, 
							 Color =:= ?BLACK ->
			{true, [{Obj, ?RED1} | AccDb]};
		   ({_Data,Color}, {Replaced,AccDb}) when Replaced, 
							 Color =:= ?BLACK ->
			{false, AccDb};
		   ({_Data,_Color}, {Replaced,AccDb}) ->
			{Replaced, AccDb}
		end;
	    bag ->
		fun({Data,Color}, {Replaced,AccDb}) when Data =/= Obj ->
			{Replaced, [{Data,Color} | AccDb]};
		   ({_Data,Color}, {Replaced,AccDb}) when not Replaced, 
							 Color =/= ?BLACK ->
			{true, [{Obj,Color} | AccDb]};
		   ({_Data,Color}, {Replaced,AccDb}) when Replaced, 
							 Color =/= ?BLACK ->
			{true, AccDb};
		   ({_Data,Color}, {Replaced,AccDb}) when Replaced, 
							 Color =:= ?BLACK ->
			{true, AccDb};
		   ({_Data,Color}, {Replaced,AccDb}) when not Replaced, 
							 Color =:= ?BLACK ->
			{true, [{Obj, ?RED1} | AccDb]};
		   ({_Data,_Color}, {Replaced,AccDb}) ->
			{Replaced, AccDb}
		end;
	    duplicate_bag ->
		   %% The fun is never called if the type is duplicate_bag,
		   %% because all we have to do with new elements is to insert 
		   %% them (multiple identical objects allowed). 
		not_used
	end,
    
    FilterFun = fun(Acc0, L) ->
			lists:foldl(Fun, Acc0, L)
		end,
    
    {_Replaced, TmpDbList} = 
	case EtsType of
	    duplicate_bag ->
		{false, DbList};
	    _OtherType ->
		{R,L} = FilterFun({false,[]}, DbList),
		{R, lists:reverse(L)}
	end,

    case Sorting of
	false ->
	    TmpDbList ++ [{Obj,?RED1}];
	true ->
	       %% The original list is already sorted!
	       %% Just merge the two lists together!
	    tv_db_sort:merge(SortKeyNo, TmpDbList, [{Obj,?RED1}], RevSorting)
    end.
    





max_size([]) ->    
    0;
max_size(L) ->
    max_size(L, 0).



max_size([], CurrMax) ->
    CurrMax;
max_size([H | T], CurrMax) when is_tuple(H) ->
    Size = size(H),
    if
	Size >= CurrMax ->
	    max_size(T, Size);
	true ->
	    max_size(T, CurrMax)
    end;
max_size([_H | T], CurrMax) ->
    Size = 1,
    if
	Size >= CurrMax ->
	    max_size(T, Size);
	true ->
	    max_size(T, CurrMax)
    end.





add_elements(_KeyNo, Inserted, List, false, _RevSorting, _SortKeyNo) ->
       % Remember that the order of the original list has to be preserved!
    List ++ list2dblist(Inserted, ?RED1);
add_elements(_KeyNo, Inserted, List, _Sorting, RevSorting, SortKeyNo) ->
       % The original list is already sorted - sort the new elements, and
       % just merge the two lists together!
    SortedInsertedList = tv_db_sort:mergesort(SortKeyNo, 
					       list2dblist(Inserted, ?RED1), 
					       RevSorting),
    tv_db_sort:merge(SortKeyNo, List, SortedInsertedList, RevSorting).
    




   %% We assume the list already has been sorted, i.e., since the order won't 
   %% be changed by marking an element deleted, we DON'T have to sort the list
   %% once again!

mark_deleted(_KeyNo, [], List) ->
    List;
mark_deleted(KeyNo, [Data | T], List) ->
    KeyValue = tv_db_sort:get_compare_value(KeyNo, Data),
    NewList  = mark_one_element_deleted(KeyNo, KeyValue, Data, List, []),
    mark_deleted(KeyNo, T, NewList).
  
  






mark_one_element_deleted(_KeyNo, _KeyValue, _Data, [], Acc) ->
    Acc;
mark_one_element_deleted(KeyNo, {tuple, KeyValue}, 
			 Data, [{DataTuple, Color} | Tail], Acc) ->
    OldKeyValue = tv_db_sort:get_compare_value(KeyNo, DataTuple),
       % Remember that the order of the original list has to be preserved!
    if
	OldKeyValue =:= {tuple, KeyValue} ->
	    Acc ++ [{Data, ?BLACK}] ++ Tail;
	true ->
	    mark_one_element_deleted(KeyNo, {tuple, KeyValue}, Data, Tail, 
				     Acc ++ [{DataTuple, Color}])
    end;
mark_one_element_deleted(KeyNo, _KeyValue, Data, [{DataTuple, Color} | Tail], Acc) ->
    if
	Data =:= DataTuple ->
	    Acc ++ [{Data, ?BLACK}] ++ Tail;
	true ->
	    mark_one_element_deleted(KeyNo, _KeyValue, Data, Tail, 
				     Acc ++ [{DataTuple, Color}])
    end.
  
  
  
  



   %% We assume the list already has been sorted, i.e., since the order won't 
   %% be changed by marking an element updated, we DON'T have to sort the list
   %% once again!

replace_elements(_KeyNo, [], List) ->
    List;
replace_elements(KeyNo, [Data | T], List) ->
    KeyValue = tv_db_sort:get_compare_value(KeyNo, Data),
    NewList  = replace_one_element(KeyNo, KeyValue, Data, List, []),
    replace_elements(KeyNo, T, NewList).





    
    
replace_one_element(_KeyNo, _Key, _Data, [], Acc) ->
    Acc;
replace_one_element(KeyNo, {tuple, Key1}, Data, [{DataTuple, Color} | Tail], Acc) ->
    Key2 = tv_db_sort:get_compare_value(KeyNo, DataTuple),
    % Remember that the order of the original list has to be preserved!
    if
	Key2 =:= {tuple, Key1} ->
	    Acc ++ [{Data, ?GREEN1}] ++ Tail;
	true ->
	    replace_one_element(KeyNo, {tuple, Key1}, Data, Tail, 
				Acc ++ [{DataTuple, Color}])
    end;
replace_one_element(_KeyNo, _KeyValue, _Data, [{DataTuple, Color} | Tail], Acc) ->
       % Can't replace an element with no key!
    Acc ++ [{DataTuple, Color} | Tail].
    







group_difflists(bag, _KeyNo, Inserted, Deleted) ->	     
       %% Since the ETS table is of bag type, no element can be updated, i.e.,
       %% it can only be deleted and re-inserted, otherwise a new element will be added.
    {Inserted, Deleted, []};
group_difflists(duplicate_bag, _KeyNo, Inserted, Deleted) ->	     
       %% Since the ETS table is of duplicate_bag type, no element can be updated, i.e.,
       %% it can only be deleted and re-inserted, otherwise a new element will be added.
    {Inserted, Deleted, []};
group_difflists(set, _KeyNo, [], Deleted) ->
       %% Updated elements have to be present in both lists, i.e., if one list is empty,
       %% the other contains no updated elements - they are either inserted or deleted!
    {[], Deleted, []};
group_difflists(set, _KeyNo, Inserted, []) ->
    {Inserted, [], []};
group_difflists(set, KeyNo, InsOrUpd, DelOrUpd) ->
    match_difflists(KeyNo, InsOrUpd, DelOrUpd, [], []).    






match_difflists(_KeyNo, [], Deleted, Inserted, Updated) ->
    {Inserted, Deleted, Updated};
match_difflists(KeyNo, [Data | T], DelOrUpd, InsAcc, UpdAcc) ->
    % This function is only called in case of a 'set' ETS table.
    % 'Set' type of ETS table means there are unique keys. If two elements in 
    % InsOrUpd and DelOrUpd have the same key, that element has been updated, 
    % and is added to the Updated list, and removed from the original two lists.
    % After the two lists have been traversed in this way, the remaining elements
    % in DelOrUpd forms the new Deleted list (analogous for InsOrUpd).
    % If we want to improve the performance, we could check which list is the 
    % shortest, since the traversing time depends on this. 
    Key = element(KeyNo, Data),
    case searchdelete(Key, KeyNo, DelOrUpd) of
	{true, NewDelOrUpd} ->
	    match_difflists(KeyNo, T, NewDelOrUpd, InsAcc, [Data | UpdAcc]);
	{false, SameDelOrUpd} ->
	    match_difflists(KeyNo, T, SameDelOrUpd, [Data | InsAcc], UpdAcc)
    end.




searchdelete(_Key, _ElemNo, []) ->
    {false, []};
searchdelete(Key, ElemNo, List) ->
    searchdelete(Key, ElemNo, List, []).





searchdelete(_Key, _ElemNo, [], Acc) ->
    {false, Acc};
searchdelete(Key, ElemNo, [Tuple | Tail], Acc) ->
    % We don't use standard libraries, 'cause we want to make an 'atomic'
    % operation, i.e., we will not search the list two times...
    case (element(ElemNo, Tuple) =:= Key) of
	true ->
	    {true, Acc ++ Tail};   % Return the list without the matching element
	_Other ->
	    searchdelete(Key, ElemNo, Tail, [Tuple | Acc])
    end.
	
    





dblist2list([]) ->
    [];
dblist2list([{Data, _Color} | T]) ->
    [Data | dblist2list(T)].
    
    
    




list2dblist([], _Color) ->    
    [];
list2dblist([Data | T], Color) ->
    [{Data, Color} | list2dblist(T, Color)].








update_colors([]) ->
    [];
update_colors([{Data, Color} | T]) ->
    [{Data, new_color(Color)} | update_colors(T)].
    







new_color(?GREEN1) ->
    ?GREEN2;
new_color(?GREEN2) ->
    ?GREEN3;
new_color(?GREEN3) ->
    ?GREEN4;
new_color(?GREEN4) ->
    ?GREEN5;
new_color(?GREEN5) ->
    ?DEFAULT_BTN_COLOR;
new_color(?RED1) ->
    ?RED2;
new_color(?RED2) ->
    ?RED3;
new_color(?RED3) ->
    ?RED4;
new_color(?RED4) ->
    ?RED5;
new_color(?RED5) ->
    ?DEFAULT_BTN_COLOR;
new_color(_Other) ->
    ?DEFAULT_BTN_COLOR.                  % Default shall be gray.








compute_elapsed_seconds({H1, M1, S1}, {H2, M2, S2}) ->
    ElapsedHours   = get_time_diff(hours, H1, H2),
    ElapsedMinutes = get_time_diff(minutes, M1, M2),
    ElapsedSeconds = get_time_diff(seconds, S1, S2),
    (ElapsedHours * 3600) + (ElapsedMinutes * 60) + ElapsedSeconds + 1.







get_time_diff(_Type, T1, T2) when T1 =< T2 ->
    T2 - T1;
get_time_diff(hours, T1, T2) ->
    T2 + 24 - T1;
get_time_diff(minutes, T1, T2) ->
    T2 + 60 - T1;
get_time_diff(seconds, T1, T2) ->
    T2 + 60 - T1.




split(_N, []) ->
    {[], []};
split(0, List) ->
    {[], List};
split(N, List) ->
    split2(0, N - 1, [], List).



split2(Ctr, N, Acc, [H | T]) when Ctr < N ->
    split2(Ctr + 1, N, [H | Acc], T);
split2(_Ctr, _N, Acc, []) ->
    {lists:reverse(Acc), []};
split2(_Ctr, _N, Acc, List) ->
    {lists:reverse(Acc), List}.

basetype(ordered_set) ->	    
    set;
basetype(Any) ->
    Any.
