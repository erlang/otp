%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-module(observer_pro_wx).

-behaviour(wx_object).

-export([start_link/3]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("etop.hrl").
-include("observer_defs.hrl").
-include("etop_defs.hrl").

%% Defines
-define(COL_PID,  0).
-define(COL_NAME, ?COL_PID+1).
%%-define(COL_TIME, 2).
-define(COL_REDS, ?COL_NAME+1).
-define(COL_MEM,  ?COL_REDS+1).
-define(COL_MSG,  ?COL_MEM+1).
-define(COL_FUN,  ?COL_MSG+1).

-define(ID_KILL, 201).
-define(ID_PROC, 202).
-define(ID_REFRESH, 203).
-define(ID_REFRESH_INTERVAL, 204).
-define(ID_DUMP_TO_FILE, 205).
-define(ID_TRACE_PIDS, 206).
-define(ID_TRACE_NAMES, 207).
-define(ID_TRACE_NEW, 208).
-define(ID_TRACE_ALL, 209).
-define(ID_ACCUMULATE, 210).
-define(ID_GARBAGE_COLLECT, 211).

-define(TRACE_PIDS_STR, "Trace selected process identifiers").
-define(TRACE_NAMES_STR, "Trace selected processes, "
	"if a process have a registered name "
	"processes with same name will be traced on all nodes").


%% Records

-record(sort,
	{
	  sort_key=?COL_REDS,
	  sort_incr=false
	}).

-record(holder, {parent,
		 info,
                 next=[],
		 sort=#sort{},
		 accum=[],
                 next_accum=[],
		 attrs,
		 node,
		 backend_pid,
                 old_backend=false
		}).

-record(state, {parent,
		grid,
		panel,
		popup_menu,
		parent_notebook,
		timer,
		procinfo_menu_pids=[],
		sel={[], []},
		right_clicked_pid,
		holder}).

start_link(Notebook, Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent, Config]) ->
    Attrs = observer_lib:create_attrs(),
    Self = self(),
    Acc = maps:get(acc, Config, false),
    Holder = spawn_link(fun() -> init_table_holder(Self, Acc, Attrs) end),
    {ProPanel, State} = setup(Notebook, Parent, Holder, Config),
    {ProPanel, State#state{holder=Holder}}.

setup(Notebook, Parent, Holder, Config) ->
    ProPanel = wxPanel:new(Notebook, []),

    Grid  = create_list_box(ProPanel, Holder),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),

    wxWindow:setSizer(ProPanel, Sizer),

    State = #state{parent=Parent,
		   grid=Grid,
		   panel=ProPanel,
		   parent_notebook=Notebook,
		   holder=Holder,
		   timer=Config
		   },
    {ProPanel, State}.


%% UI-creation

create_pro_menu(Parent, Holder) ->
    MenuEntries = [{"File",
		    [#create_menu{id=?ID_DUMP_TO_FILE, text="Dump to file"}]},
		   {"View",
		    [#create_menu{id=?ID_ACCUMULATE, text="Accumulate",
				  type=check,
				  check=call(Holder, {get_accum, self()})},
		     separator,
		     #create_menu{id=?ID_REFRESH, text="Refresh\tCtrl-R"},
		     #create_menu{id=?ID_REFRESH_INTERVAL, text="Refresh Interval"}]},
		   {"Trace",
		    [#create_menu{id=?ID_TRACE_PIDS, text="Trace processes"},
		     #create_menu{id=?ID_TRACE_NAMES, text="Trace named processes (all nodes)"},
		     #create_menu{id=?ID_TRACE_NEW, text="Trace new processes"}
		     %% , #create_menu{id=?ID_TRACE_ALL_MENU, text="Trace all processes"}
		    ]}
		  ],
    observer_wx:create_menus(Parent, MenuEntries).

create_list_box(Panel, Holder) ->
    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_HRULES,
    ListCtrl = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Row, Col) ->
					       safe_call(Holder, {get_row, self(), Row, Col})
				       end},
				      {onGetItemAttr,
				       fun(_, Item) ->
					       safe_call(Holder, {get_attr, self(), Item})
				       end}
				     ]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(ListCtrl, Col, Li),
			   wxListCtrl:setColumnWidth(ListCtrl, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Pid", ?wxLIST_FORMAT_CENTRE,  120},
		 {"Name or Initial Func", ?wxLIST_FORMAT_LEFT, 200},
%%		 {"Time", ?wxLIST_FORMAT_CENTRE, 50},
		 {"Reds", ?wxLIST_FORMAT_RIGHT, 100},
		 {"Memory", ?wxLIST_FORMAT_RIGHT, 100},
		 {"MsgQ",  ?wxLIST_FORMAT_RIGHT, 50},
		 {"Current Function", ?wxLIST_FORMAT_LEFT,  200}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:setItemCount(ListCtrl, 1),
    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    %% Use focused instead of selected, selected doesn't generate events
    %% for all multiple selections on Linux
    wxListCtrl:connect(ListCtrl, command_list_item_focused),
    ListCtrl.

dump_to_file(Parent, FileName, Holder) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    %% Holder closes the file when it's done
	    Holder ! {dump, Fd};
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    MD = wxMessageDialog:new(Parent, FailMsg),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD)
    end.

start_procinfo(undefined, _Frame, Opened) ->
    Opened;
start_procinfo(Pid, Frame, Opened) ->
    case lists:keyfind(Pid, 1, Opened) of
	false ->
	    case observer_procinfo:start(Pid, Frame, self()) of
		{error, _} -> Opened;
		PI -> [{Pid, PI} | Opened]
	    end;
	{_, PI} ->
	    wxFrame:raise(PI),
	    Opened
    end.


safe_call(Holder, What) ->
    case call(Holder, What, 2000) of
        Res when is_atom(Res) -> "";
        Res -> Res
    end.

call(Holder, What) ->
    call(Holder, What, infinity).

call(Holder, What, TMO) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! What,
    receive
	{'DOWN', Ref, _, _, _} -> holder_dead;
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    after TMO ->
	    timeout
    end.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, State0=#state{grid=Grid}) ->
    State = update_selection(State0),

    wxListCtrl:setItemCount(Grid, Count),
    Count > 0 andalso wxListCtrl:refreshItems(Grid, 0, Count-1),
    observer_wx:set_status(io_lib:format("Number of Processes: ~w", [Count])),
    {noreply, State};

handle_info(refresh_interval, #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_info({procinfo_menu_closed, Pid},
	    #state{procinfo_menu_pids=Opened}=State) ->
    NewPids = lists:keydelete(Pid, 1, Opened),
    {noreply, State#state{procinfo_menu_pids=NewPids}};

handle_info({procinfo_open, Pid},
	    #state{panel=Panel, procinfo_menu_pids=Opened}=State) ->
    Opened2 = start_procinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_info({active, Node},
	    #state{holder=Holder, timer=Timer, parent=Parent}=State) ->
    create_pro_menu(Parent, Holder),
    Holder ! {change_node, Node},
    {noreply, State#state{timer=observer_lib:start_timer(Timer, 10)}};

handle_info(not_active, #state{timer=Timer0}=State) ->
    Timer = observer_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info(Info, State) ->
    io:format("~p:~p, Unexpected info: ~tp~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, #state{holder=Holder}) ->
    Holder ! stop,
    etop:stop(),
    ok.

code_change(_, _, State) ->
    {ok, State}.

handle_call(get_config, _, #state{holder=Holder, timer=Timer}=State) ->
    Conf = observer_lib:timer_config(Timer),
    Accum = case safe_call(Holder, {get_accum, self()}) of
                Bool when is_boolean(Bool) -> Bool;
                _ -> false
            end,
    {reply, Conf#{acc=>Accum}, State};

handle_call(Msg, _From, State) ->
    io:format("~p:~p: Unhandled call ~tp~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p:~p: Unhandled cast ~tp~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=?ID_DUMP_TO_FILE}, #state{panel=Panel, holder=Holder}=State) ->
    FD  =  wxFileDialog:new(Panel,
			    [{style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    wxDialog:destroy(FD),
	    dump_to_file(Panel, Path, Holder);
	_ ->
	    wxDialog:destroy(FD)
    end,
    {noreply, State};

handle_event(#wx{id=?ID_ACCUMULATE,
		 event=#wxCommand{type=command_menu_selected, commandInt=CmdInt}},
	     #state{holder=Holder}=State) ->
    Holder ! {accum, CmdInt =:= 1},
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH, event=#wxCommand{type=command_menu_selected}},
	     #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     #state{panel=Panel, timer=Timer0}=State) ->
    Timer = observer_lib:interval_dialog(Panel, Timer0, 1, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(#wx{id=?ID_KILL}, #state{right_clicked_pid=Pid, sel=Sel0}=State) ->
    exit(Pid, kill),
    Sel = rm_selected(Pid,Sel0),
    {noreply, State#state{sel=Sel}};

handle_event(#wx{id=?ID_GARBAGE_COLLECT}, #state{sel={_, Pids}}=State) ->
    _ = [rpc:call(node(Pid), erlang, garbage_collect, [Pid]) || Pid <- Pids],
    {noreply, State};

handle_event(#wx{id=?ID_PROC},
	     #state{panel=Panel, right_clicked_pid=Pid, procinfo_menu_pids=Opened}=State) ->
    Opened2 = start_procinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(#wx{id=?ID_TRACE_PIDS}, #state{sel={_, Pids}, panel=Panel}=State)  ->
    case Pids of
	[] ->
	    observer_wx:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Pids ->
	    observer_trace_wx:add_processes(Pids),
	    {noreply,  State}
    end;

handle_event(#wx{id=?ID_TRACE_NAMES}, #state{sel={SelIds,_Pids}, holder=Holder, panel=Panel}=State)  ->
    case SelIds of
	[] ->
	    observer_wx:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	_ ->
	    PidsOrReg = call(Holder, {get_name_or_pid, self(), SelIds}),
	    observer_trace_wx:add_processes(PidsOrReg),
	    {noreply,  State}
    end;

handle_event(#wx{id=?ID_TRACE_NEW, event=#wxCommand{type=command_menu_selected}}, State) ->
    observer_trace_wx:add_processes([new_processes]),
    {noreply,  State};

handle_event(#wx{event=#wxSize{size={W,_}}},
	     #state{grid=Grid}=State) ->
    observer_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_right_click,
			       itemIndex=Row}},
	     #state{panel=Panel, holder=Holder}=State) ->

    Pid =
	case call(Holder, {get_row, self(), Row, pid}) of
	    {error, undefined} ->
		undefined;
	    {ok, P} ->
		Menu = wxMenu:new(),
		wxMenu:append(Menu, ?ID_PROC,
			      "Process info for " ++ pid_to_list(P)),
		wxMenu:append(Menu, ?ID_TRACE_PIDS,
			      "Trace selected processes",
			      [{help, ?TRACE_PIDS_STR}]),
		wxMenu:append(Menu, ?ID_TRACE_NAMES,
			      "Trace selected processes by name (all nodes)",
			      [{help, ?TRACE_NAMES_STR}]),
		wxMenu:append(Menu, ?ID_GARBAGE_COLLECT, "Garbage collect processes"),
		wxMenu:append(Menu, ?ID_KILL, "Kill process " ++ pid_to_list(P)),
		wxWindow:popupMenu(Panel, Menu),
		wxMenu:destroy(Menu),
		P
	end,
    {noreply, State#state{right_clicked_pid=Pid}};

handle_event(#wx{event=#wxList{type=command_list_item_focused,
			       itemIndex=Row}},
	     #state{grid=Grid,holder=Holder} = State) ->
    case Row >= 0 of
	true ->
	    SelIds = [Row|lists:delete(Row, get_selected_items(Grid))],
	    Pids = call(Holder, {get_pids, self(), SelIds}),
	    {noreply, State#state{sel={SelIds, Pids}}};
	false ->
	    {noreply, State}
    end;

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     #state{holder=Holder}=State) ->
    Holder !  {change_sort, Col},
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated}},
	     #state{panel=Panel, procinfo_menu_pids=Opened,
		    sel={_, [Pid|_]}}=State) ->
    Opened2 = start_procinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(Event, State) ->
    io:format("~p:~p: handle event ~tp\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_selection(State=#state{holder=Holder, grid=Grid,
			      sel={SelIds0, SelPids0}}) ->
    Sel = {SelIds,_SelPids} = call(Holder, {get_rows_from_pids, self(), SelPids0}),
    set_focus(SelIds0, SelIds, Grid),
    case SelIds =:= SelIds0 of
	true -> ok;
	false ->
	    wx:batch(fun() ->
			     [wxListCtrl:setItemState(Grid, I, 0, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds0],
			     [wxListCtrl:setItemState(Grid, I, 16#FFFF, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds]
		     end)
    end,
    %%io:format("Update ~p -> ~p~n",[{SelIds0, SelPids0}, Sel]),
    State#state{sel=Sel}.

get_selected_items(Grid) ->
    get_selected_items(Grid, -1, []).

get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index, [{geometry, ?wxLIST_NEXT_ALL},
						{state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item | ItemAcc])
    end.

set_focus([], [], _Grid) -> ok;
set_focus([Same|_], [Same|_], _Grid) -> ok;
set_focus([], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED),
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED).

rm_selected(Pid, {Ids, Pids}) ->
    rm_selected(Pid, Ids, Pids, [], []).

rm_selected(Pid, [_Id|Ids], [Pid|Pids], AccIds, AccPids) ->
    {lists:reverse(AccIds)++Ids,lists:reverse(AccPids)++Pids};
rm_selected(Pid, [Id|Ids], [OtherPid|Pids], AccIds, AccPids) ->
    rm_selected(Pid, Ids, Pids, [Id|AccIds], [OtherPid|AccPids]);
rm_selected(_, [], [], AccIds, AccPids) ->
    {lists:reverse(AccIds), lists:reverse(AccPids)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%TABLE HOLDER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table_holder(Parent, Accum0, Attrs) ->
    process_flag(trap_exit, true),
    Backend = spawn_link(node(), observer_backend, procs_info, [self()]),
    Accum = case Accum0 of
                true -> true;
                _ -> []
            end,
    table_holder(#holder{parent=Parent,
			 info=array:new(),
			 node=node(),
			 backend_pid=Backend,
			 attrs=Attrs,
                         accum=Accum
			}).

table_holder(#holder{info=Info, attrs=Attrs,
		     node=Node, backend_pid=Backend, old_backend=Old}=S0) ->
    receive
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, Info),
	    table_holder(S0);
	{get_attr, From, Row} ->
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
        {procs_info, Backend, Procs} ->
	    State = handle_update(Procs, S0),
	    table_holder(State);
        {'EXIT', Backend, normal} when Old =:= false ->
            S1 = update_complete(S0),
            table_holder(S1#holder{backend_pid=undefined});
	{Backend, EtopInfo=#etop_info{}} ->
	    State = handle_update_old(EtopInfo, S0),
	    table_holder(State#holder{backend_pid=undefined});
	refresh when is_pid(Backend)->
	    table_holder(S0); %% Already updating
	refresh ->
            Pid = case Old of
                      true ->
                          spawn_link(Node, observer_backend, etop_collect, [self()]);
                      false ->
                          spawn_link(Node, observer_backend, procs_info, [self()])
                  end,
            table_holder(S0#holder{backend_pid=Pid});
	{change_sort, Col} ->
	    State = change_sort(Col, S0),
	    table_holder(State);
	{get_pids, From, Indices} ->
	    get_pids(From, Indices, Info),
	    table_holder(S0);
	{get_rows_from_pids, From, Pids} ->
	    get_rows_from_pids(From, Pids, Info),
	    table_holder(S0);
	{get_name_or_pid, From, Indices} ->
	    get_name_or_pid(From, Indices, Info),
	    table_holder(S0);
	{get_node, From} ->
	    From ! {self(), Node},
	    table_holder(S0);
	{change_node, NewNode} ->
	    case Node == NewNode of
		true ->
		    table_holder(S0);
		false ->
                    _ = rpc:call(NewNode, code, ensure_loaded, [observer_backend]),
                    case rpc:call(NewNode, erlang, function_exported,
                                  [observer_backend,procs_info, 1]) of
                        true ->
                            self() ! refresh,
                            table_holder(S0#holder{node=NewNode, old_backend=false});
                        false ->
                            self() ! refresh,
                            table_holder(S0#holder{node=NewNode, old_backend=true});
                        _ ->
                            table_holder(S0)
                    end
            end;
	{accum, Bool} ->
	    table_holder(change_accum(Bool,S0));
	{get_accum, From} ->
	    From ! {self(), S0#holder.accum == true},
	    table_holder(S0);
	{dump, Fd} ->
            Collector = spawn_link(Node, observer_backend, etop_collect,[self()]),
            receive
                {Collector, EtopInfo=#etop_info{}} ->
                    etop_txt:do_update(Fd, EtopInfo, #etop_info{}, #opts{node=Node}),
                    file:close(Fd),
                    table_holder(S0);
                {'EXIT', Collector, _} ->
                    table_holder(S0)
            end;
	stop ->
	    ok;
        {'EXIT', Backend, normal} ->
            table_holder(S0);
        {'EXIT', Backend, _Reason} ->
            %% Node crashed will be noticed soon..
            table_holder(S0#holder{backend_pid=undefined});
	_What ->
            %% io:format("~p: Table holder got ~tp~n",[?MODULE, _What]),
	    table_holder(S0)
    end.

change_sort(Col, S0=#holder{parent=Parent, info=Data, sort=Sort0}) ->
    {Sort, ProcInfo}=sort(Col, Sort0, Data),
    Parent ! {holder_updated, array:size(Data)},
    S0#holder{info=array:from_list(ProcInfo), sort=Sort}.

change_accum(true, S0) ->
    S0#holder{accum=true};
change_accum(false, S0=#holder{info=Info}) ->
    self() ! refresh,
    Accum = [{Pid, Reds} || #etop_proc_info{pid=Pid, reds=Reds} <- array:to_list(Info)],
    S0#holder{accum=lists:sort(Accum)}.

handle_update_old(#etop_info{procinfo=ProcInfo0},
                  S0=#holder{parent=Parent, sort=Sort=#sort{sort_key=KeyField}}) ->
    {ProcInfo1, Accum} = accum(ProcInfo0, S0),
    {_SO, ProcInfo} = sort(KeyField, Sort#sort{sort_key=undefined}, ProcInfo1),
    Info = array:from_list(ProcInfo),
    Parent ! {holder_updated, array:size(Info)},
    S0#holder{info=Info, accum=Accum}.

handle_update(ProcInfo0, S0=#holder{next=Next, sort=#sort{sort_key=KeyField}}) ->
    {ProcInfo1, Accum} = accum(ProcInfo0, S0),
    Sort = sort_fun(KeyField, true),
    Merge = merge_fun(KeyField),
    Merged = Merge(Sort(ProcInfo1), Next),
    case Accum of
        true ->  S0#holder{next=Merged};
        _List -> S0#holder{next=Merged, next_accum=Accum}
    end.

update_complete(#holder{parent=Parent, sort=#sort{sort_incr=Incr},
                        next=ProcInfo, accum=Accum, next_accum=NextAccum}=S0) ->
    Info = case Incr of
               true -> array:from_list(ProcInfo);
               false -> array:from_list(lists:reverse(ProcInfo))
           end,
    Parent ! {holder_updated, array:size(Info)},
    S0#holder{info=Info, accum= Accum =:= true orelse NextAccum,
              next=[], next_accum=[]}.

accum(ProcInfo, #holder{accum=true}) ->
    {ProcInfo, true};
accum(ProcInfo0, #holder{accum=Previous, next_accum=Next}) ->
    Accum = [{Pid, Reds} || #etop_proc_info{pid=Pid, reds=Reds} <- ProcInfo0],
    ProcInfo = lists:sort(ProcInfo0),
    {accum2(ProcInfo,Previous,[]), lists:merge(lists:sort(Accum), Next)}.

accum2([PI=#etop_proc_info{pid=Pid, reds=Reds}|PIs],
       [{Pid, OldReds}|Old], Acc) ->
    accum2(PIs, Old, [PI#etop_proc_info{reds=Reds-OldReds}|Acc]);
accum2(PIs=[#etop_proc_info{pid=Pid}|_], [{OldPid,_}|Old], Acc)
  when Pid > OldPid ->
    accum2(PIs, Old, Acc);
accum2([PI|PIs], Old, Acc) ->
    accum2(PIs, Old, [PI|Acc]);
accum2([], _, Acc) -> Acc.

sort(Col, Opt, Table)
  when not is_list(Table) ->
    sort(Col,Opt,array:to_list(Table));
sort(Col, Opt=#sort{sort_key=Col, sort_incr=Bool}, Table) ->
    {Opt#sort{sort_incr=not Bool},lists:reverse(Table)};
sort(Col, S=#sort{sort_incr=Incr}, Table) ->
    Sort = sort_fun(Col, Incr),
    {S#sort{sort_key=Col}, Sort(Table)}.

sort_fun(?COL_NAME, true) ->
    fun(Table) -> lists:sort(fun sort_name/2, Table) end;
sort_fun(?COL_NAME, false) ->
    fun(Table) -> lists:sort(fun sort_name_rev/2, Table) end;
sort_fun(Col, true) ->
    N = col_to_element(Col),
    fun(Table) -> lists:keysort(N, Table) end;
sort_fun(Col, false) ->
    N = col_to_element(Col),
    fun(Table) -> lists:reverse(lists:keysort(N, Table)) end.

merge_fun(?COL_NAME) ->
    fun(A,B) -> lists:merge(fun sort_name/2, A, B) end;
merge_fun(Col) ->
    KeyField = col_to_element(Col),
    fun(A,B) -> lists:keymerge(KeyField, A, B) end.


sort_name(#etop_proc_info{name={_,_,_}=A}, #etop_proc_info{name={_,_,_}=B}) ->
    A =< B;
sort_name(#etop_proc_info{name=A}, #etop_proc_info{name=B})
  when is_atom(A), is_atom(B) ->
    A =< B;
sort_name(#etop_proc_info{name=Reg}, #etop_proc_info{name={M,_F,_A}})
  when is_atom(Reg) ->
    Reg < M;
sort_name(#etop_proc_info{name={M,_,_}}, #etop_proc_info{name=Reg})
  when is_atom(Reg) ->
    M < Reg.

sort_name_rev(#etop_proc_info{name={_,_,_}=A}, #etop_proc_info{name={_,_,_}=B}) ->
    A >= B;
sort_name_rev(#etop_proc_info{name=A}, #etop_proc_info{name=B})
  when is_atom(A), is_atom(B) ->
    A >= B;
sort_name_rev(#etop_proc_info{name=Reg}, #etop_proc_info{name={M,_F,_A}})
  when is_atom(Reg) ->
    Reg >= M;
sort_name_rev(#etop_proc_info{name={M,_,_}}, #etop_proc_info{name=Reg})
  when is_atom(Reg) ->
    M >= Reg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_procinfo_data(Col, Info) ->
    element(col_to_element(Col), Info).
col_to_element(?COL_PID)  -> #etop_proc_info.pid;
col_to_element(?COL_NAME) -> #etop_proc_info.name;
col_to_element(?COL_MEM)  -> #etop_proc_info.mem;
%%col_to_element(?COL_TIME) -> #etop_proc_info.runtime;
col_to_element(?COL_REDS) -> #etop_proc_info.reds;
col_to_element(?COL_FUN)  -> #etop_proc_info.cf;
col_to_element(?COL_MSG)  -> #etop_proc_info.mq.

get_pids(From, Indices, ProcInfo) ->
    Processes = [(array:get(I, ProcInfo))#etop_proc_info.pid || I <- Indices],
    From ! {self(), Processes}.

get_name_or_pid(From, Indices, ProcInfo) ->
    Get = fun(#etop_proc_info{name=Name}) when is_atom(Name) -> Name;
	     (#etop_proc_info{pid=Pid}) -> Pid
	  end,
    Processes = [Get(array:get(I, ProcInfo)) || I <- Indices],
    From ! {self(), Processes}.

get_row(From, Row, pid, Info) ->
    Pid = case Row =:= -1 of
	      true ->  {error, undefined};
	      false -> {ok, get_procinfo_data(?COL_PID, array:get(Row, Info))}
	  end,
    From ! {self(), Pid};
get_row(From, Row, Col, Info) ->
    Data = case Row >= array:size(Info) of
	       true ->
		   "";
	       false ->
		   ProcInfo = array:get(Row, Info),
		   get_procinfo_data(Col, ProcInfo)
	   end,
    From ! {self(), observer_lib:to_str(Data)}.

get_rows_from_pids(From, Pids0, Info) ->
    Search = fun(Idx, #etop_proc_info{pid=Pid}, Acc0={Pick0, {Idxs, Pids}}) ->
		     case ordsets:is_element(Pid, Pick0) of
			 true ->
			     Acc = {[Idx|Idxs],[Pid|Pids]},
			     Pick = ordsets:del_element(Pid, Pick0),
			     case Pick =:= [] of
				 true -> throw(Acc);
				 false -> {Pick, Acc}
			     end;
			 false -> Acc0
		     end
	     end,
    Res = try
	      {_, R} = array:foldl(Search, {ordsets:from_list(Pids0), {[],[]}}, Info),
	      R
	  catch R0 -> R0
	  end,
    From ! {self(), Res}.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->  Attrs#attrs.even;
		    false -> Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.
