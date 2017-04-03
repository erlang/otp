%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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

-export([start_link/3]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-record(state,
	{
	  time = #ti{},
	  active = false,
	  parent,
	  wins,
	  mem,
	  samples,
          max,
	  panel,
	  paint,
	  appmon,
	  async
	}).

-define(ID_REFRESH_INTERVAL, 102).

-import(observer_perf_wx,
	[make_win/4, setup_graph_drawing/1, refresh_panel/4, interval_dialog/2,
	 add_data/5, precalc/4]).

start_link(Notebook, Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

init([Notebook, Parent, Config]) ->
    try
	TopP  = wxPanel:new(Notebook),
	Main  = wxBoxSizer:new(?wxVERTICAL),
	Panel = wxPanel:new(TopP),
	GSzr  = wxBoxSizer:new(?wxVERTICAL),
	BorderFlags = ?wxLEFT bor ?wxRIGHT,
	Carrier = make_win(alloc, Panel, GSzr, BorderFlags bor ?wxTOP),
	Utilz = make_win(utilz, Panel, GSzr, BorderFlags),
	wxWindow:setSizer(Panel, GSzr),
	wxSizer:add(Main, Panel, [{flag, ?wxEXPAND},{proportion,2}]),

	MemWin = create_mem_info(TopP),
	wxSizer:add(Main, MemWin, [{flag, ?wxEXPAND bor BorderFlags bor ?wxBOTTOM},
				   {proportion, 1}, {border, 5}]),
	wxWindow:setSizer(TopP, Main),
	Windows = [Carrier, Utilz],
	PaintInfo = setup_graph_drawing(Windows),
	{TopP, #state{parent= Parent,
		      panel = Panel,
		      wins  = Windows,
		      mem   = MemWin,
		      paint = PaintInfo,
		      time  = setup_time(Config),
                      max   = #{}
		     }
	}
    catch _:Err ->
	    io:format("~p crashed ~p: ~p~n",[?MODULE, Err, erlang:get_stacktrace()]),
	    {stop, Err}
    end.

setup_time(Config) ->
    Freq = maps:get(fetch, Config, 1),
    #ti{disp=?DISP_FREQ/Freq,
        fetch=Freq,
        secs=maps:get(secs, Config, ?DISP_SECONDS)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{id=?ID_REFRESH_INTERVAL, event=#wxCommand{type=command_menu_selected}},
	     #state{active=Active, panel=Panel, appmon=Old, wins=Wins0, time=#ti{fetch=F0} = Ti0} = State) ->
    case interval_dialog(Panel, Ti0) of
	Ti0 -> {noreply, State};
	#ti{fetch=F0} = Ti -> %% Same fetch interval force refresh
	    Wins = [W#win{max=undefined} || W <- Wins0],
	    {noreply, precalc(State#state{time=Ti, wins=Wins})};
	Ti when not Active ->
	    {noreply, State#state{time=Ti}};
	Ti -> %% Changed fetch interval, drop all data
	    {noreply, restart_fetcher(Old, State#state{time=Ti})}
    end;
handle_event(#wx{event=#wxCommand{type=command_menu_selected}},
	     State = #state{}) ->
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

%%%%%%%%%%
handle_sync_event(#wx{obj=Panel, event = #wxPaint{}},_,
		  #state{active=Active, time=Ti, paint=Paint,
			 wins = Windows}) ->
    %% Sigh workaround bug on MacOSX (Id in paint event is always 0)
    Win = lists:keyfind(Panel, #win.panel, Windows),
    refresh_panel(Active, Win, Ti, Paint),
    ok.
%%%%%%%%%%
handle_call(get_config, _, #state{time=Ti}=State) ->
    #ti{fetch=Fetch, secs=Range} = Ti,
    {reply, #{fetch=>Fetch, secs=>Range}, State};

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).
%%%%%%%%%%

handle_info({Key, {promise_reply, {badrpc, _}}}, #state{async=Key} = State) ->
    {noreply, State#state{active=false, appmon=undefined}};

handle_info({Key, {promise_reply, SysInfo}},
	    #state{async=Key, samples=Data, max=Max0,
                   active=Active, wins=Wins0, time=#ti{tick=Tick, disp=Disp0}=Ti} = S0) ->
    Disp = trunc(Disp0),
    Next = max(Tick - Disp, 0),
    erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, Next}),
    Info = alloc_info(SysInfo),
    Max = lists:foldl(fun calc_max/2, Max0, Info),
    {Wins, Samples} = add_data(Info, Data, Wins0, Ti, Active),
    S1 = S0#state{time=Ti#ti{tick=Next}, wins=Wins, samples=Samples, max=Max, async=undefined},
    if Active ->
	    update_alloc(S0, Info, Max),
	    State = precalc(S1),
	    {noreply, State};
       true ->
	    {noreply, S1}
    end;

handle_info({refresh, Seq},
	    State = #state{panel=Panel, appmon=Node, time=#ti{tick=Seq, disp=DispF}=Ti})
  when (Seq+1) < (DispF*1.5) ->
    Next = Seq+1,
    State#state.active andalso (catch wxWindow:refresh(Panel)),
    erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, Next}),
    if Seq =:= (trunc(DispF)-1) ->
	    Req = rpc:async_call(Node, observer_backend, sys_info, []),
	    {noreply, State#state{time=Ti#ti{tick=Next}, async=Req}};
       true ->
	    {noreply, State#state{time=Ti#ti{tick=Next}}}
    end;
handle_info({refresh, _S}, #state{}=State) ->
    {noreply, State};

handle_info({active, Node}, State = #state{parent=Parent, panel=Panel, appmon=Old}) ->
    create_menus(Parent, []),
    try
	Node = Old,
	wxWindow:refresh(Panel),
	{noreply, precalc(State#state{active=true})}
    catch _:_ ->
	    {noreply, restart_fetcher(Node, State)}
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

restart_fetcher(Node, #state{panel=Panel, wins=Wins0, time=Ti} = State) ->
    SysInfo = observer_wx:try_rpc(Node, observer_backend, sys_info, []),
    Info = alloc_info(SysInfo),
    Max = lists:foldl(fun calc_max/2, #{}, Info),
    {Wins, Samples} = add_data(Info, {0, queue:new()}, Wins0, Ti, true),
    erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, 0}),
    wxWindow:refresh(Panel),
    precalc(State#state{active=true, appmon=Node, time=Ti#ti{tick=0},
			wins=Wins, samples=Samples, max=Max}).

precalc(#state{samples=Data0, paint=Paint, time=Ti, wins=Wins0}=State) ->
    Wins = [precalc(Ti, Data0, Paint, Win) || Win <- Wins0],
    State#state{wins=Wins}.

calc_max({Name, _, Cs}, Max0) ->
    case maps:get(Name, Max0, 0) of
        Value when Value < Cs ->
            Max0#{Name=>Cs};
        _V ->
            Max0
    end.

update_alloc(#state{mem=Grid}, Fields, Max) ->
    wxWindow:freeze(Grid),
    Last = wxListCtrl:getItemCount(Grid),
    Update = fun({Name, BS, CS}, Row) ->
		     (Row >= Last) andalso wxListCtrl:insertItem(Grid, Row, ""),
                     MaxV = maps:get(Name, Max, CS),
		     wxListCtrl:setItem(Grid, Row, 0, observer_lib:to_str(Name)),
		     wxListCtrl:setItem(Grid, Row, 1, observer_lib:to_str(BS div 1024)),
		     wxListCtrl:setItem(Grid, Row, 2, observer_lib:to_str(CS div 1024)),
                     wxListCtrl:setItem(Grid, Row, 3, observer_lib:to_str(MaxV div 1024)),
		     Row + 1
	     end,
    wx:foldl(Update, 0, Fields),
    wxWindow:thaw(Grid),
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
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES bor ?wxLC_VRULES,
    Grid = wxListCtrl:new(Parent, [{style, Style}]),

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
		 {"Carrier size (kB)",?wxLIST_FORMAT_RIGHT, 150},
                 {"Max Carrier size (kB)",?wxLIST_FORMAT_RIGHT, 150}
                ],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    Grid.

create_menus(Parent, _) ->
    View = {"View", [#create_menu{id = ?ID_REFRESH_INTERVAL, text = "Graph Settings"}]},
    observer_wx:create_menus(Parent, [{"File", []}, View]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
