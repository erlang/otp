%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(observer_alloc_wx).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-record(state,
	{
	  offset = 0.0,
	  active = false,
	  parent,
	  windows,
	  data = {0, queue:new()},
	  panel,
	  paint,
	  appmon,
	  async
	}).

-define(ALLOC_W,  1).
-define(UTIL_W,  2).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

init([Notebook, Parent]) ->
    try
	Panel = wxPanel:new(Notebook),
	Main  = wxBoxSizer:new(?wxVERTICAL),
	Style = ?wxFULL_REPAINT_ON_RESIZE bor ?wxCLIP_CHILDREN,
	Carrier = wxPanel:new(Panel, [{winid, ?ALLOC_W}, {style,Style}]),
	Utilz = wxPanel:new(Panel, [{winid, ?UTIL_W}, {style,Style}]),
	BorderFlags = ?wxLEFT bor ?wxRIGHT,
	wxSizer:add(Main, Carrier, [{flag, ?wxEXPAND bor BorderFlags bor ?wxTOP},
				    {proportion, 1}, {border, 5}]),

	wxSizer:add(Main, Utilz, [{flag, ?wxEXPAND bor BorderFlags},
				  {proportion, 1}, {border, 5}]),

	MemWin = {MemPanel,_} = create_mem_info(Panel),
	wxSizer:add(Main, MemPanel, [{flag, ?wxEXPAND bor BorderFlags bor ?wxBOTTOM},
				     {proportion, 1}, {border, 5}]),
	wxWindow:setSizer(Panel, Main),

	PaintInfo = observer_perf_wx:setup_graph_drawing([Carrier, Utilz]),
	{Panel, #state{parent=Parent,
		       panel =Panel,
		       windows = {Carrier, Utilz, MemWin},
		       paint=PaintInfo}
	}
    catch _:Err ->
	    io:format("~p crashed ~p: ~p~n",[?MODULE, Err, erlang:get_stacktrace()]),
	    {stop, Err}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event=#wxCommand{type=command_menu_selected}},
	     State = #state{}) ->
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

%%%%%%%%%%
handle_sync_event(#wx{obj=Panel, event = #wxPaint{}},_,
		  #state{active=Active, offset=Offset, paint=Paint,
			 windows=Windows, data=Data}) ->
    %% Sigh workaround bug on MacOSX (Id in paint event is always 0)
    Id = if Panel =:= element(?ALLOC_W, Windows)  -> alloc;
	    Panel =:= element(?UTIL_W, Windows)  -> utilz
	 end,
    observer_perf_wx:refresh_panel(Panel, Id, Offset, Data, Active, Paint),
    ok.
%%%%%%%%%%
handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).
%%%%%%%%%%

handle_info({Key, {promise_reply, {badrpc, _}}}, #state{async=Key} = State) ->
    {noreply, State#state{active=false, appmon=undefined}};

handle_info({Key, {promise_reply, SysInfo}}, #state{async=Key, data=Data} = State) ->
    Info = alloc_info(SysInfo),
    update_alloc(State, Info),
    {noreply, State#state{offset=0.0, data = add_data(Info, Data), async=undefined}};

handle_info({refresh, Seq, Freq, Node}, #state{panel=Panel, appmon=Node, async=Key} = State) ->
    wxWindow:refresh(Panel),
    Next = Seq+1,
    if
	Next > Freq, Key =:= undefined ->
	    erlang:send_after(trunc(1000 / Freq), self(), {refresh, 1, Freq, Node}),
	    Req = rpc:async_call(Node, observer_backend, sys_info, []),
	    {noreply, State#state{offset=Seq/Freq, async=Req}};
       true ->
	    erlang:send_after(trunc(1000 / Freq), self(), {refresh, Next, Freq, Node}),
	    {noreply, State#state{offset=Seq/Freq}}
    end;
handle_info({refresh, _Seq, _Freq, _Node}, State) ->
    {noreply, State};

handle_info({active, Node}, State = #state{parent=Parent, panel=Panel, appmon=Old}) ->
    create_menus(Parent, []),
    try
	Node = Old,
	wxWindow:refresh(Panel),
	{noreply, State#state{active=true}}
    catch _:_ ->
	    SysInfo = observer_wx:try_rpc(Node, observer_backend, sys_info, []),
	    Info = alloc_info(SysInfo),
	    Freq = 6,
	    erlang:send_after(trunc(1000 / Freq), self(), {refresh, 1, Freq, Node}),
	    wxWindow:refresh(Panel),
	    {noreply, State#state{active=true, appmon=Node, offset=0.0,
				  data = add_data(Info, {0, queue:new()})}}
    end;

handle_info(not_active, State = #state{appmon=_Pid}) ->
    {noreply, State#state{active=false}};

handle_info({'EXIT', Old, _}, State = #state{appmon=Old}) ->
    {noreply, State#state{active=false, appmon=undefined}};

handle_info(_Event, State) ->
    %% io:format("~p:~p: ~p~n",[?MODULE,?LINE,_Event]),
    {noreply, State}.

terminate(_Event, #state{}) ->
    ok.
code_change(_, _, State) ->
    State.

%%%%%%%%%%

add_data(Stats, {N, Q}) when N > 60 ->
    {N, queue:drop(queue:in(Stats, Q))};
add_data(Stats, {N, Q}) ->
    {N+1, queue:in(Stats, Q)}.

update_alloc(#state{windows={_, _, {_, Grid}}}, Fields) ->
    Max = wxListCtrl:getItemCount(Grid),
    Update = fun({Name, BS, CS}, Row) ->
		     (Row >= Max) andalso wxListCtrl:insertItem(Grid, Row, ""),
		     wxListCtrl:setItem(Grid, Row, 0, observer_lib:to_str(Name)),
		     wxListCtrl:setItem(Grid, Row, 1, observer_lib:to_str(BS div 1024)),
		     wxListCtrl:setItem(Grid, Row, 2, observer_lib:to_str(CS div 1024)),
		     Row + 1
	     end,
    lists:foldl(Update, 0, Fields),
    Fields.

alloc_info(SysInfo) ->
    AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
    alloc_info(AllocInfo, [], 0, 0, true).

alloc_info([{Type,Instances}|Allocators],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    {BS,CS,NewTotalBS,NewTotalCS,NewIncludeTotal} =
	sum_alloc_instances(Instances,0,0,TotalBS,TotalCS),
    alloc_info(Allocators,[{Type,BS,CS}|TypeAcc],NewTotalBS,NewTotalCS,
	       IncludeTotal andalso NewIncludeTotal);
alloc_info([],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    Types = [X || X={_,BS,CS} <- TypeAcc, (BS>0 orelse CS>0)],
    case IncludeTotal of
	true ->
	    [{total,TotalBS,TotalCS} | lists:reverse(Types)];
	false ->
	    lists:reverse(Types)
    end.

sum_alloc_instances(false,BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,false};
sum_alloc_instances([{_,_,Data}|Instances],BS,CS,TotalBS,TotalCS) ->
    {NewBS,NewCS,NewTotalBS,NewTotalCS} =
	sum_alloc_one_instance(Data,BS,CS,TotalBS,TotalCS),
    sum_alloc_instances(Instances,NewBS,NewCS,NewTotalBS,NewTotalCS);
sum_alloc_instances([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,true}.

sum_alloc_one_instance([{sbmbcs,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS,TotalCS);
sum_alloc_one_instance([{_,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([{_,[{blocks_size,BS},{carriers_size,CS}]}|
			Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([_|Rest],BS,CS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,BS,CS,TotalBS,TotalCS);
sum_alloc_one_instance([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_mem_info(Parent) ->
    Panel = wxPanel:new(Parent),
    wxWindow:setBackgroundColour(Panel, {255,255,255}),
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES bor ?wxLC_VRULES,
    Grid = wxListCtrl:new(Panel, [{style, Style}]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Allocator Type",  ?wxLIST_FORMAT_LEFT,  200},
		 {"Block size (kB)",  ?wxLIST_FORMAT_RIGHT, 150},
		 {"Carrier size (kB)",?wxLIST_FORMAT_RIGHT, 150}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT},
			      {border, 5}, {proportion, 1}]),
    wxWindow:setSizerAndFit(Panel, Sizer),
    {Panel, Grid}.


create_menus(Parent, _) ->
    MenuEntries =
	[{"File",
	  [
	  ]}
	],
    observer_wx:create_menus(Parent, MenuEntries).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
