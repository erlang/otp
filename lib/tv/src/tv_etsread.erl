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
%%%   Description:      Module containing the interface towards ETS tables,
%%%                     i.e., handling the polling and thereafter sending the 
%%%                     result to the database part of the table tool.
%%%
%%%*********************************************************************


-module(tv_etsread).



-export([etsread/2]).



-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").






%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




etsread(MasterPid, ErrorMsgMode) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrorMsgMode),
    blocked(MasterPid).





%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************



blocked(MasterPid) ->
    receive
        Msg ->
            case Msg of 

                #etsread_deblock{} ->
                    deblock(Msg, MasterPid);

		{'EXIT', Pid, Reason} ->
	            exit_signals({Pid, Reason}, MasterPid),
		    blocked(MasterPid);

		{error_msg_mode, Mode} ->
		    put(error_msg_mode, Mode),
		    blocked(MasterPid);

                _Other ->
		    %% io:format("Received signal ~p~n", [_Other]),
                    blocked(MasterPid)
            end
    end.
    






deblock(Msg, MasterPid) ->
    #etsread_deblock{dbs_pid       = DbsPid,
		     table_type    = KindOfTable,
		     node          = Node,
		     local_node    = LocalNode,
		     table_id      = TableId,
		     poll_interval = PollInt}  = Msg,
    PollInterval = case PollInt of 
		       infinity ->
			   PollInt;
		       _Other ->
			   PollInt * 1000
		   end,
       %% Get table info!
    case catch get_table_info(Node, LocalNode, TableId, KindOfTable) of
	nodedown ->
	    MasterPid ! #pc_nodedown{sender            = self(),
				     automatic_polling = false},
	    blocked(MasterPid);
	no_table ->
	    MasterPid ! #pc_dead_table{sender = self(),
				       automatic_polling = false},
	    blocked(MasterPid);
	mnesia_not_started ->
	    MasterPid ! #pc_dead_table{sender = self(),
				       automatic_polling = false},
	    blocked(MasterPid);
	{unexpected_error,_Reason} ->
	    MasterPid ! #pc_dead_table{sender = self(),
				       automatic_polling = false},
	    blocked(MasterPid);
	{Type, Pos, Protection} ->
	    MasterPid ! #etsread_deblock_cfm{sender     = self(),
					     type       = Type,
					     keypos     = Pos,
					     protection = Protection
					    },
	    
	    timer:sleep(500),
	    case catch read_table(Node, LocalNode, TableId, KindOfTable, DbsPid) of
		nodedown ->
		    MasterPid ! #pc_nodedown{sender            = self(),
					     automatic_polling = false},
		    blocked(MasterPid);
		no_table ->
		    MasterPid ! #pc_dead_table{sender = self(),
					       automatic_polling = false},
		    blocked(MasterPid);
		mnesia_not_started ->
		    MasterPid ! #pc_dead_table{sender = self(),
					       automatic_polling = false},
		    blocked(MasterPid);
		{unexpected_error,_Reason} ->
		    MasterPid ! #pc_dead_table{sender = self(),
					       automatic_polling = false},
		    blocked(MasterPid);
		_ElapsedTime ->
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, 
				   KindOfTable, PollInterval)
	    end
    end.


		    
	    
					     

get_table_info(Node, LocalNode, TableId, KindOfTable) ->
    case KindOfTable of
	ets ->
	       % Check whether table is 'bag' or 'set' type.
	    Type       = tv_ets_rpc:info(Node, LocalNode, TableId, type),
	       % Get position for the key.
	    Pos        = tv_ets_rpc:info(Node, LocalNode, TableId, keypos),
	    Protection = tv_ets_rpc:info(Node, LocalNode, TableId, protection),
	    {Type, Pos, Protection};
	mnesia ->
	    Type = tv_mnesia_rpc:table_info(Node, LocalNode, TableId, type),
	    Pos  = 2,
	       %% All Mnesia tables are regarded as being public!
	    {Type, Pos, public}
    end.
	    



    

deblocked_loop(MasterPid,DbsPid,Node,LocalNode,TableId,KindOfTable,PollInterval) ->
    receive
	Msg ->

	    case Msg of 
		
		#etsread_poll_table{} ->
		    case catch read_table(Node, LocalNode, TableId, KindOfTable, DbsPid) of
			   %% No automatic polling here!
			nodedown ->
			    MasterPid ! #pc_nodedown{sender            = self(),
						     automatic_polling = false};
			no_table ->
			    MasterPid ! #pc_dead_table{sender = self(),
						       automatic_polling = false};
			mnesia_not_started ->
			    MasterPid ! #pc_dead_table{sender = self(),
						       automatic_polling = false};
			{unexpected_error,_Reason} ->
			    MasterPid ! #pc_dead_table{sender = self(),
						       automatic_polling = false};
			_ElapsedTime ->
			    done
		    end,
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode,
				   TableId, KindOfTable, PollInterval);
		    

		#etsread_set_poll_interval{interval = PollInt} ->
		    NewPollInterval = case PollInt of 
					  infinity ->
					      PollInt;
					  _Other ->
					      PollInt * 1000
				      end,
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode,
				   TableId, KindOfTable, NewPollInterval);


		#etsread_deblock{} ->
		    deblock(Msg, MasterPid);
		

		#etsread_update_object{key_no=KeyNo, object=Obj, old_object=OldObj} ->
		    update_object(KindOfTable, Node, LocalNode, TableId, DbsPid, 
				  KeyNo, Obj, OldObj, MasterPid, PollInterval),
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, KindOfTable, 
				   PollInterval);


		#etsread_new_object{object=Obj} ->
		    new_object(KindOfTable, Node, LocalNode, TableId, DbsPid, 
				  Obj, MasterPid, PollInterval),
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, KindOfTable, 
				   PollInterval);


		#etsread_delete_object{object=Obj} ->
		    delete_object(KindOfTable, Node, LocalNode, TableId, DbsPid, 
				  Obj, MasterPid, PollInterval),
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, KindOfTable, 
				   PollInterval);


		#ip_dead_table{} ->
		    AutoPoll = case PollInterval of
				   infinity ->
				       false;
				   _Other ->
				       true
			       end,
		    MasterPid ! #pc_dead_table{sender            = self(),
					       automatic_polling = AutoPoll},
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, 
				   KindOfTable, infinity);
		    

		#etsread_nodedown{} ->
		    AutoPoll = case PollInterval of
				   infinity ->
				       false;
				   _Other ->
				       true
			       end,
		    MasterPid ! #pc_nodedown{sender            = self(),
					     automatic_polling = AutoPoll},
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, 
				   KindOfTable, infinity);


		{error_msg_mode, Mode} ->
		    put(error_msg_mode, Mode),
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode, TableId, KindOfTable, 
				   PollInterval);
		

		{'EXIT', Pid, Reason} ->
	            exit_signals({Pid, Reason}, MasterPid),
		    deblocked_loop(MasterPid, DbsPid, Node, LocalNode,
				   TableId, KindOfTable, PollInterval)
	    end
    
    after PollInterval ->
	       %% Automatic polling must be on, otherwise these 
	       %% lines would never be executed!
	    NewPollInterval =
		case catch read_table(Node,LocalNode,TableId,KindOfTable,DbsPid) of
		    nodedown ->
			MasterPid ! #pc_nodedown{sender            = self(),
						 automatic_polling = true},
			infinity;
		    no_table ->
			MasterPid ! #pc_dead_table{sender = self(),
						   automatic_polling = true},
			infinity;
		    mnesia_not_started ->
			MasterPid ! #pc_dead_table{sender = self(),
						   automatic_polling = true},
			infinity;
		    {unexpected_error,_Reason} ->
			MasterPid ! #pc_dead_table{sender = self(),
						   automatic_polling = true},
			infinity;
		    ElapsedMilliseconds ->
			if
			    (ElapsedMilliseconds * 1000) >= PollInterval ->
				infinity;
			    true ->
				PollInterval
			end
		end,
	    deblocked_loop(MasterPid, DbsPid, Node, LocalNode,
			   TableId, KindOfTable, NewPollInterval)
    end.





exit_signals(ExitInfo, MasterPid) ->
    case ExitInfo of
        {MasterPid, _Reason} ->
            exit(normal);
        _Other ->
	    done
    end.




update_object(KindOfTable, Node, LocalNode, TableId, DbsPid, KeyNo, Obj, OldObj, MasterPid, PollInterval) ->
    AutoPoll = 
	case PollInterval of
	    infinity ->
		false;
	    _Other ->
		true
	end,
    case check_record_format(KindOfTable, Node, LocalNode, TableId, Obj) of
	bad_format ->
	    DbsPid ! #etsread_update_object_cfm{sender   = self(),
						success  = false};
	ok ->
	       %% Check that we are allowed to edit the table!
	    case catch update_object2(KindOfTable, Node, LocalNode, TableId, DbsPid, KeyNo, 
				      Obj, OldObj) of
		
		nodedown ->
		    DbsPid ! #etsread_update_object_cfm{sender  = self(),
							success = false},
		    MasterPid ! #pc_nodedown{sender             = self(),
					     automatic_polling  = AutoPoll};
		
		no_table ->
		    DbsPid ! #etsread_update_object_cfm{sender   = self(),
							success  = false},
		    MasterPid ! #pc_dead_table{sender = self(),
					       automatic_polling = AutoPoll};
		
		mnesia_not_started ->
		    DbsPid ! #etsread_update_object_cfm{sender   = self(),
							success  = false},
		    MasterPid ! #pc_dead_table{sender            = self(),
					       automatic_polling = AutoPoll};
		
		
		{unexpected_error,_Reason} ->
		    DbsPid ! #etsread_update_object_cfm{sender   = self(),
							success  = false},
		    MasterPid ! #pc_dead_table{sender            = self(),
					       automatic_polling = AutoPoll};
		
		ok ->
		    DbsPid ! #etsread_update_object_cfm{sender  = self(),
							success = true}
	    end
    end.





update_object2(ets, Node, LocalNode, Tab, _DbsPid, KeyNo, Obj, OldObj) ->
       %% We shall update a specific object! If the table is a 'set' table,
       %% it is just to insert the altered object. However, if the table
       %% is a 'bag', or a 'duplicate_bag', we first have to remove the
       %% old object, and then insert the altered one. 
       %% But, we aren't finished with that... we also want to preserve
       %% the time order, meaning we have to delete *ALL* objects having the
       %% very same key, and then insert them again! (Actually we would have 
       %% to do this anyhow, due to limitations in the interface functions,
       %% but this remark has to be noted!)
    OldKey = element(KeyNo, OldObj),
    InsertList = 
	case tv_ets_rpc:info(Node, LocalNode, Tab, type) of
	    set ->
		   %% Have to remove old object, because the key may be what's changed.
		tv_ets_rpc:delete(Node, LocalNode, Tab, OldKey),
		[Obj];
	    ordered_set ->
		   %% Have to remove old object, because the key may be what's changed.
		tv_ets_rpc:delete(Node, LocalNode, Tab, OldKey),
		[Obj];
	    _Other ->  %% 'bag' or 'duplicate_bag'
		OldList = tv_ets_rpc:lookup(Node, LocalNode, Tab, OldKey),
		tv_ets_rpc:delete(Node, LocalNode, Tab, OldKey),
		   %% Have to beware of duplicate_bag tables,
		   %% i.e., the same object may occur more than
		   %% once, but we only want to replace it once!
		{_Replaced, TmpList} =
		    lists:foldl(
		      fun(Data, {Replaced,Acc}) when Data =/= OldObj ->
			      {Replaced, [Data | Acc]};
			 (_Data, {Replaced,Acc}) when not Replaced ->
			      {true, [Obj | Acc]};
			 (Data, {Replaced,Acc}) ->
			      {Replaced, [Data | Acc]}
		      end,
		      {false, []},
		      OldList),
		lists:reverse(TmpList)
	end,
    lists:foreach(fun(H) ->
			  tv_ets_rpc:insert(Node, LocalNode, Tab, H)
		  end,
		  InsertList),
    ok;
update_object2(mnesia, Node, LocalNode, Tab, _DbsPid, KeyNo, Obj, OldObj) ->
    OldKey = element(KeyNo, OldObj),
    InsertList = 
	case tv_mnesia_rpc:table_info(Node, LocalNode, Tab, type) of
	    set ->
		tv_mnesia_rpc:transaction(
		  Node,
		  LocalNode,
		  fun() ->
			  mnesia:delete(Tab,OldKey,write)
		  end),
		[Obj];
	    ordered_set ->
		tv_mnesia_rpc:transaction(
		  Node,
		  LocalNode,
		  fun() ->
			  mnesia:delete(Tab,OldKey,write)
		  end),
		[Obj];
	    _Other ->  %% 'bag' or 'duplicate_bag'
		{atomic, OldList} = 
		    tv_mnesia_rpc:transaction(
		      Node, 
		      LocalNode,
		      fun() -> 
			      mnesia:read(Tab,OldKey,read)
		      end),
		   %% We can't use mnesia:delete_object here, because
		   %% time order wouldn't be preserved then!!!
		tv_mnesia_rpc:transaction(
		  Node,
		  LocalNode,
		  fun() ->
			  mnesia:delete(Tab,OldKey,write)
		  end),
		ChangeFun =
		    fun(H) when H =:= OldObj ->
			    Obj;
		       (H) ->
			    H
		    end,
		[ChangeFun(X) || X <- OldList]
	end,
    lists:foreach(fun(H) ->
			  tv_mnesia_rpc:transaction(
			    Node, 
			    LocalNode,
			    fun() ->
				       %% This mnesia call shall not be distributed,
				       %% since the transaction sees to that it is
				       %% executed on the right node!!!
				    mnesia:write(Tab,H,write)
			    end)
		  end,
		  InsertList),
    ok.

		  




delete_object(KindOfTable, Node, LocalNode, TableId, DbsPid, Obj, MasterPid, PollInterval) ->
    AutoPoll = 
	case PollInterval of
	    infinity ->
		false;
	    _Other ->
		true
	end,
    case catch delete_object2(KindOfTable, Node, LocalNode, TableId, DbsPid, Obj) of

	nodedown ->
	    DbsPid ! #etsread_delete_object_cfm{sender  = self(),
						success = false},
	    MasterPid ! #pc_nodedown{sender             = self(),
				     automatic_polling  = AutoPoll};

	no_table ->
	    DbsPid ! #etsread_delete_object_cfm{sender   = self(),
						success  = false},
	    MasterPid ! #pc_dead_table{sender = self(),
				       automatic_polling = AutoPoll};
	
	mnesia_not_started ->
	    DbsPid ! #etsread_delete_object_cfm{sender   = self(),
						success  = false},
	    MasterPid ! #pc_dead_table{sender            = self(),
				       automatic_polling = AutoPoll};

	{unexpected_error,_Reason} ->
	    DbsPid ! #etsread_delete_object_cfm{sender   = self(),
						success  = false},
	    MasterPid ! #pc_dead_table{sender            = self(),
				       automatic_polling = AutoPoll};

	ok ->
	    DbsPid ! #etsread_delete_object_cfm{sender  = self(),
						success = true}
    end.




delete_object2(ets, Node, LocalNode, Tab, _DbsPid, Obj) ->
    KeyNo = tv_ets_rpc:info(Node, LocalNode, Tab, keypos),
    Key   = element(KeyNo, Obj),
    InsertList = 
	case tv_ets_rpc:info(Node, LocalNode, Tab, type) of
	    set ->
		   %% Have to remove old object, because the key may be what's changed.
		tv_ets_rpc:delete(Node, LocalNode, Tab, Key),
		[];
	    ordered_set ->
		   %% Have to remove old object, because the key may be what's changed.
		tv_ets_rpc:delete(Node, LocalNode, Tab, Key),
		[];
	    _Other ->  %% 'bag' or 'duplicate_bag'
		OldList = tv_ets_rpc:lookup(Node, LocalNode, Tab, Key),
		tv_ets_rpc:delete(Node, LocalNode, Tab, Key),
		OldList -- [Obj]
	end,

    lists:foreach(fun(H) ->
			  tv_ets_rpc:insert(Node, LocalNode, Tab, H)
		  end,
		  InsertList),
    ok;
delete_object2(mnesia, Node, LocalNode, Tab, _DbsPid, Obj) ->
    tv_mnesia_rpc:transaction(
      Node, 
      LocalNode,
      fun() ->
	         %% This mnesia call shall not be distributed,
	         %% since the transaction sees to that it is
	         %% executed on the right node!!!
	      mnesia:delete_object(Tab,Obj,write)
      end),
    ok.





new_object(KindOfTable, Node, LocalNode, TableId, DbsPid, Obj, MasterPid, PollInterval) ->
    AutoPoll = 
	case PollInterval of
	    infinity ->
		false;
	    _Other ->
		true
	end,
    case check_record_format(KindOfTable, Node, LocalNode, TableId, Obj) of
	bad_format ->
	    DbsPid ! #etsread_new_object_cfm{sender   = self(),
						success  = false};
	ok ->
	       %% Check that we are allowed to edit the table!
	    case catch new_object2(KindOfTable, Node, LocalNode, TableId, DbsPid, Obj) of
		
		nodedown ->
		    DbsPid ! #etsread_new_object_cfm{sender  = self(),
						     success = false},
		    MasterPid ! #pc_nodedown{sender             = self(),
					     automatic_polling  = AutoPoll};
		
		no_table ->
		    DbsPid ! #etsread_new_object_cfm{sender   = self(),
						     success  = false},
		    MasterPid ! #pc_dead_table{sender = self(),
					       automatic_polling = AutoPoll};
		
		mnesia_not_started ->
		    DbsPid ! #etsread_new_object_cfm{sender   = self(),
						     success  = false},
		    MasterPid ! #pc_dead_table{sender            = self(),
					       automatic_polling = AutoPoll};
		
		{unexpected_error,_Reason} ->
		    DbsPid ! #etsread_new_object_cfm{sender   = self(),
						     success  = false},
		    MasterPid ! #pc_dead_table{sender            = self(),
					       automatic_polling = AutoPoll};
		
		ok ->
		    DbsPid ! #etsread_new_object_cfm{sender  = self(),
						     success = true}
	    end
    end.





new_object2(ets, Node, LocalNode, Tab, _DbsPid, Obj) ->
    tv_ets_rpc:insert(Node, LocalNode, Tab, Obj),
    ok;
new_object2(mnesia, Node, LocalNode, Tab, _DbsPid, Obj) ->
    tv_mnesia_rpc:transaction(
      Node, 
      LocalNode,
      fun() ->
	         %% This mnesia call shall not be distributed,
	         %% since the transaction sees to that it is
	         %% executed on the right node!!!
	      mnesia:write(Tab,Obj,write)
      end),
    ok.
    
		  



check_record_format(mnesia, Node, LocalNode, Tab, Obj) ->
    Arity = tv_mnesia_rpc:table_info(Node, LocalNode, Tab, arity),
    case size(Obj) of
	Arity ->
	    ok;
	_Other ->
	    gs:window(etsreadwin, gs:start(), []),
	    case get(error_msg_mode) of
		normal ->
		    tv_utils:notify(etsreadwin, "TV Notification", 
				    ["The record is not complete,",
				     "too few fields are specified!"]);
		haiku ->
		    tv_utils:notify(etsreadwin, "TV Notification", 
				    ["The attempt to change",
				     "The specified record size",
				     "Is simply ignored."])
	    end,
	    gs:destroy(etsreadwin),
	    bad_format
    end;
check_record_format(ets, _Node, _LocalNode, _Tab, _Obj) ->
    ok.
	    
	    
		      




read_table(Node, LocalNode, Tab, KindOfTable, DbsPid) ->
    T1 = time(),

    {TableContent, ListOfKeys} = 
	case KindOfTable of
	    ets ->
		{tv_ets_rpc:tab2list(Node, LocalNode, Tab), 
		 [tv_ets_rpc:info(Node, LocalNode, Tab, keypos)]
		};
	    mnesia ->
		   %% It may be tempting to use Mnesia event subscription,
		   %% but will this really save the day? The main drawback
		   %% is that we will then have to update the table copy we
		   %% store internally in two different ways: one for the 
		   %% Mnesia tables, and one for the ETS tables. Also, if
		   %% the Mnesia tables are frequently updated, this will 
		   %% cause TV to work all the time too (either updating the
		   %% table copy for each inserted/deleted object, or storing
		   %% these objects until polling is ordered). To make this 
		   %% work smoothly requires a bit of work...
		   %% The second drawback is that it doesn't seem clear in all
		   %% circumstances how the subscription actually works - i.e.,
		   %% if we only use subscriptions, can we actually be sure that
		   %% the *real* state of the table is the same as the one kept
		   %% in TV? For example, imagine the scenario that Mnesia is
		   %% stopped, all Mnesia directories are removed (from the UNIX 
		   %% shell), and then Mnesia once again is started. The first 
		   %% problem is that we have to check for start/stop of Mnesia,
		   %% the second is that we then have to rescan the actual table.
		   %% The logic for this may require som effort to write!
		   %% Also, what will happen if the table is killed/dies? 
		   %% Will we get messages for each element in the table?
		   %% (I havent't checked this last issue, this is just som thoughts.)
		   %% And generally, there is always a risk that a message is lost,
		   %% which will result in TV showing an erroneous table content.
		   %%
		   %% All in all, using Mnesia subscriptions *may* be a sub-optimization.
		   %% The current solution works fine, is also easy to control, and is 
		   %% mainly the same for both ETS and Mnesia tables.
		   %% My suggestion is that it is used until someone actually complains
		   %% about the polling time being too long for huge tables!  :-)
		   %% (However, it shall be emphasized that it is this module that
		   %% actually polls the Mnesia/ETS tables, meaning that it is
		   %% mainly this module that has to be modified, should the usage of
		   %% subscriptions be desired. The other module that has to be modified
		   %% is the one maintaining the internal copy of the table.)
                WildPattern = tv_mnesia_rpc:table_info(Node,LocalNode,Tab,wild_pattern),
                {atomic, Content} = 
                    tv_mnesia_rpc:transaction(
		      Node,
		      LocalNode,
		      fun() ->
			         %% This mnesia call shall not be distributed,
			         %% since the transaction sees to that it is
			         %% executed on the right node!!!
			      mnesia:match_object(Tab, WildPattern, read)
		      end),
                {Content, [2 | tv_mnesia_rpc:table_info(Node, LocalNode,Tab, index)]}
	end,
    
    T2 = time(),

    ElapsedTime = compute_elapsed_seconds(T1, T2),

    DbsPid ! #dbs_new_data{sender             = self(),
			   data               = TableContent,
			   keys               = ListOfKeys,
			   time_to_read_table = ElapsedTime
			  },

    ElapsedTime.







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
