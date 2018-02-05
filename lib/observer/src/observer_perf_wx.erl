%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
-module(observer_perf_wx).

-export([start_link/3]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

%% Drawing wrappers for DC and GC areas
-export([make_win/4, setup_graph_drawing/1,
	 refresh_panel/4, precalc/4, add_data/5, interval_dialog/2,
	 haveGC/0, make_gc/2, destroy_gc/1,
	 setPen/2, setFont/3, setBrush/2,
	 strokeLine/5, strokeLines/2, drawRoundedRectangle/6,
	 drawText/4, getTextExtent/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(ID_REFRESH_INTERVAL, 102).

-define(BW, 5).
-define(BH, 5).

-record(state,
	{
	  time = #ti{},
	  active = false,
	  parent,
	  samples,        %% Orig data store
	  wins=[],     %% per window content
	  panel,
	  paint,
	  appmon
	}).

-define(wxGC, wxGraphicsContext).

-record(paint, {font, small, pen, pen2, pens, dot_pens, usegc = false}).

start_link(Notebook, Parent, Config) ->
    wx_object:start_link(?MODULE, [Notebook, Parent, Config], []).

init([Notebook, Parent, Config]) ->
    try
	Panel = wxPanel:new(Notebook),
	Main  = wxBoxSizer:new(?wxVERTICAL),
	MemIO = wxBoxSizer:new(?wxHORIZONTAL),

	CPU = make_win(runq, Panel, Main, ?wxALL),
	MEM = make_win(memory, Panel, MemIO, ?wxLEFT),
	IO  = make_win(io, Panel, MemIO, ?wxLEFT bor ?wxRIGHT),

	wxSizer:add(Main, MemIO, [{flag, ?wxEXPAND bor ?wxDOWN},
				  {proportion, 1}, {border, 5}]),
	wxWindow:setSizer(Panel, Main),
	Windows = [CPU, MEM, IO],
	PaintInfo = setup_graph_drawing(Windows),

	process_flag(trap_exit, true),
	State0 = #state{parent=Parent,
			panel =Panel,
			wins = Windows,
			paint=PaintInfo,
			samples=reset_data(),
                        time=#ti{fetch=maps:get(fetch, Config, ?FETCH_DATA),
                                 secs=maps:get(secs, Config, ?DISP_SECONDS)}
		       },
	{Panel, State0}
    catch _:Err:Stacktrace ->
	    io:format("~p crashed ~tp: ~tp~n",[?MODULE, Err, Stacktrace]),
	    {stop, Err}
    end.

make_win(Name, Parent, Sizer, Border) ->
    Style = ?wxFULL_REPAINT_ON_RESIZE bor ?wxCLIP_CHILDREN,
    Panel = wxPanel:new(Parent, [{style,Style}]),
    Opts = [{flag, ?wxEXPAND bor Border}, {proportion, 1}, {border, 5}],
    wxSizer:add(Sizer, Panel, Opts),
    #win{name=Name, panel=Panel}.

setup_graph_drawing(Panels) ->
    IsWindows = element(1, os:type()) =:= win32,
    IgnoreCB = {callback, fun(_,_) -> ok end},
    Do = fun(#win{panel=Panel}) ->
		 wxWindow:setBackgroundStyle(Panel, ?wxBG_STYLE_SYSTEM),
		 wxPanel:connect(Panel, paint, [callback]),
		 IsWindows andalso
		     wxPanel:connect(Panel, erase_background, [IgnoreCB])
	 end,
    _ = [Do(Panel) || Panel <- Panels],
    UseGC = haveGC(),
    Version28 = ?wxMAJOR_VERSION =:= 2 andalso ?wxMINOR_VERSION =:= 8,
    {Font, SmallFont}
	= if UseGC, Version28 ->
		  %% Def font is really small when using Graphics contexts in 2.8
		  %% Hardcode it
		  F = wxFont:new(12,?wxFONTFAMILY_DECORATIVE,?wxFONTSTYLE_NORMAL,?wxFONTWEIGHT_BOLD),
		  SF = wxFont:new(10, ?wxFONTFAMILY_DECORATIVE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
		  {F, SF};
	     true ->
		  DefFont = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
		  DefSize = wxFont:getPointSize(DefFont),
		  DefFamily = wxFont:getFamily(DefFont),
		  F = wxFont:new(DefSize-1, DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
		  SF = wxFont:new(DefSize-2, DefFamily, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
		  {F, SF}
	  end,
    BlackPen = wxPen:new({0,0,0}, [{width, 1}]),
    Pens = [wxPen:new(Col, [{width, 1}, {style, ?wxSOLID}])
            || Col <- tuple_to_list(colors())],
    DotPens = [wxPen:new(Col, [{width, 1}, {style, ?wxDOT}])
               || Col <- tuple_to_list(colors())],
    #paint{usegc = UseGC,
	   font  = Font,
	   small = SmallFont,
	   pen   = ?wxGREY_PEN,
	   pen2  = BlackPen,
	   pens  = list_to_tuple(Pens),
           dot_pens = list_to_tuple(DotPens)
	  }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{id=?ID_REFRESH_INTERVAL, event=#wxCommand{type=command_menu_selected}},
	     #state{panel=Panel, appmon=Old, wins=Wins0, time=#ti{fetch=F0} = Ti0} = State) ->
    case interval_dialog(Panel, Ti0) of
	Ti0 -> {noreply, State};
	#ti{fetch=F0} = Ti -> %% Same fetch interval force refresh
	    Wins = [W#win{max=undefined} || W <- Wins0],
	    {noreply, precalc(State#state{time=Ti, wins=Wins})};
	Ti when Old =:= undefined ->
	    {noreply, State#state{time=Ti}};
	Ti -> %% Changed fetch interval, drop all data
	    {noreply, restart_fetcher(node(Old), State#state{time=Ti})}
    end;
handle_event(#wx{event=#wxCommand{type=command_menu_selected}},
	     State = #state{}) ->
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

%%%%%%%%%%
handle_sync_event(#wx{obj=Panel, event = #wxPaint{}},_,
		  #state{active=Active, time=Ti, paint=Paint, wins=Windows}) ->
    %% Sigh workaround bug on MacOSX (Id in paint event is always 0)
    %% Panel = element(Id, Windows),
    Win = lists:keyfind(Panel, #win.panel, Windows),
    refresh_panel(Active, Win, Ti, Paint),
    ok.

refresh_panel(Active, #win{name=_Id, panel=Panel}=Win, Ti, #paint{usegc=UseGC}=Paint) ->
    %% PaintDC must be created in a callback to work on windows.
    %% Nothing is drawn until wxPaintDC is destroyed.
    GC = make_gc(Panel, UseGC),
    if Active -> draw_win(GC, Win, Ti, Paint);
       true ->   ignore
    end,
    destroy_gc(GC).

%%%%%%%%%%
handle_call(get_config, _, #state{time=Ti}=State) ->
    #ti{fetch=Fetch, secs=Range} = Ti,
    {reply, #{fetch=>Fetch, secs=>Range}, State};

handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).
%%%%%%%%%%
handle_info({stats, 1, _, _, _} = Stats,
	    #state{panel=Panel, samples=Data, active=Active, wins=Wins0,
                   appmon=Node, time=#ti{tick=Tick, disp=Disp0}=Ti} = State0) ->
    if Active ->
	    Disp = trunc(Disp0),
	    Next = max(Tick - Disp, 0),
	    erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, Next}),
	    {Wins, Samples} = add_data(Stats, Data, Wins0, Ti, Active, Node),
	    State = precalc(State0#state{time=Ti#ti{tick=Next}, wins=Wins, samples=Samples}),
	    wxWindow:refresh(Panel),
	    {noreply, State};
       true ->
	    {Wins1, Samples} = add_data(Stats, Data, Wins0, Ti, Active, Node),
	    Wins = [W#win{max=undefined} || W <- Wins1],
	    {noreply, State0#state{samples=Samples, wins=Wins, time=Ti#ti{tick=0}}}
    end;

handle_info({refresh, Seq}, #state{panel=Panel, time=#ti{tick=Seq, disp=DispF}=Ti} = State0)
  when (Seq+1) < (DispF*1.5) ->
    Next = Seq+1,
    erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, Next}),
    State = precalc(State0#state{time=Ti#ti{tick=Next}}),
    catch wxWindow:refresh(Panel),
    {noreply, State};
handle_info({refresh, _}, State) ->
    {noreply, State};

handle_info({active, Node}, #state{parent=Parent, panel=Panel, appmon=Old} = State) ->
    create_menus(Parent, []),
    try
	Node = node(Old),
	wxWindow:refresh(Panel),
	erlang:send_after(1000 div ?DISP_FREQ, self(), {refresh, 0}),
	{noreply, State#state{active=true}}
    catch _:_ ->
	    {noreply,restart_fetcher(Node, State)}
    end;

handle_info(not_active, State = #state{appmon=_Pid}) ->
    %% Pid ! exit,
    {noreply, State#state{active=false}};

handle_info({'EXIT', Old, _}, State = #state{appmon=Old}) ->
    {noreply, State#state{active=false, appmon=undefined}};

handle_info(_Event, State) ->
    %% io:format("~p:~p: ~tp~n",[?MODULE,?LINE,_Event]),
    {noreply, State}.

%%%%%%%%%%
terminate(_Event, #state{appmon=Pid}) ->
    catch Pid ! exit,
    ok.
code_change(_, _, State) ->
    State.

restart_fetcher(Node, #state{appmon=Old, panel=Panel, time=#ti{fetch=Freq}=Ti, wins=Wins0}=State) ->
    catch Old ! exit,
    Me = self(),
    Pid = spawn_link(Node, observer_backend, fetch_stats, [Me, round(1000/Freq)]),
    wxWindow:refresh(Panel),
    Wins = [W#win{state=undefined} || W <- Wins0],
    precalc(State#state{active=true, appmon=Pid, samples=reset_data(),
			wins=Wins, time=Ti#ti{tick=0}}).

reset_data() ->
    {0, queue:new()}.

add_data(Stats, Q, Wins, Ti, Active) ->
    add_data(Stats, Q, Wins, Ti, Active, ignore).

add_data(Stats, {N, Q0}, Wins, #ti{fetch=Fetch, secs=Secs}, Active, Node)
  when N > (Secs*Fetch+1) ->
    {{value, Drop}, Q} = queue:out(Q0),
    add_data_1(Wins, Stats, N, {Drop,Q}, Active, Node);
add_data(Stats, {N, Q}, Wins, _, Active, Node) ->
    add_data_1(Wins, Stats, N+1, {empty, Q}, Active, Node).

add_data_1([#win{state={_,St}}|_]=Wins0, Last, N, {Drop, Q}, Active, Node)
  when St /= undefined ->
    try
	{Wins, Stat} =
	    lists:mapfoldl(fun(Win0, Entry) ->
				   {Win1,Stat} = add_data_2(Win0, Last, Entry),
				   case Active of
				       true ->
					   Win = add_data_3(Win1, N, Drop, Stat, Q),
					   {Win, Stat};
				       false ->
					   {Win1, Stat}
				   end
			   end, #{}, Wins0),
	{Wins, {N,queue:in(Stat#{}, Q)}}
    catch no_scheduler_change ->
	    {[Win#win{state=init_data(Id, Last), info=info(Id, Last, Node)}
	      || #win{name=Id}=Win <- Wins0], {0,queue:new()}}
    end;

add_data_1(Wins, Stats, 1, {_, Q}, _, Node) ->
    {[Win#win{state=init_data(Id, Stats), info=info(Id, Stats, Node)}
      || #win{name=Id}=Win <- Wins], {0,Q}}.

add_data_2(#win{name=Id, state=S0}=Win, Stats, Map) ->
    {V1, S1} = collect_data(Id, Stats, S0),
    {Win#win{state=S1}, Map#{Id=>V1}}.

add_data_3(#win{name=Id, max={{OldMax, OldEntry},_,_,_},
		geom=#{scale:={WS,HS}}, state={Max,_},
		graphs=Graphs}=Win,
	   N, Drop0, Last, Q1)
  when N > 3 ->
    Drop = case Drop0 of
	       #{Id:=D} -> D;
	       _ -> Drop0
	   end,
    case {max_value(Max), Drop =:= OldEntry} of
	{OldMax, false} ->
	    #{Id:=V4} = Last,
	    {{value, #{Id:=V3}},Q2} = queue:out_r(Q1),
	    {{value, #{Id:=V2}},Q3} = queue:out_r(Q2),
	    {{value, #{Id:=V1}},_}  = queue:out_r(Q3),
	    Vals = [V1,V2,V3,V4],
	    Gs = tuple_size(V1),
	    Info = lists:zip(lists:seq(Gs, 1, -1), Graphs),
	    Lines = [add_lines(Vals, Drop, Prev, I, WS, HS) || {I, Prev} <- Info],
	    Win#win{graphs=Lines, no_samples=N};
	_W -> %% Max changed Trigger complete recalc
	    Win#win{max=undefined}
    end;
add_data_3(Win, _, _, _,_) ->
    %% Trigger complete recalc
    Win#win{max=undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent, _) ->
    View = {"View", [#create_menu{id = ?ID_REFRESH_INTERVAL, text = "Graph Settings"}]},
    observer_wx:create_menus(Parent, [{"File", []}, View]).

interval_dialog(Parent0, #ti{fetch=Fetch0, secs=Secs0}=Ti) ->
    Parent = observer_lib:get_wx_parent(Parent0),
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Load Chart Settings",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor
				?wxRESIZE_BORDER}]),
    {Sl1,FetchSl} = slider(Dialog, "Sample (ms)", trunc(1000 / Fetch0), 100, 10000),
    {Sl2, SecsSl} = slider(Dialog, "Length (min)", Secs0 div 60, 1, 10),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    Flags = [{flag, ?wxEXPAND bor ?wxTOP bor ?wxLEFT bor ?wxRIGHT},
	     {border, 5}, {proportion, 1}],
    wxSizer:add(TopSizer, Sl1, Flags),
    wxSizer:add(TopSizer, Sl2, Flags),
    wxSizer:add(TopSizer, wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL), Flags),
    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),
    Res = case wxDialog:showModal(Dialog) of
	      ?wxID_OK ->
		  Fetch = 1000 / wxSlider:getValue(FetchSl),
		  Secs = wxSlider:getValue(SecsSl) * 60,
		  Ti#ti{fetch=Fetch, secs=Secs, disp=?DISP_FREQ/Fetch};
	      ?wxID_CANCEL ->
		  Ti
	  end,
    wxDialog:destroy(Dialog),
    Res.

slider(Parent, Str, Value, Min, Max) ->
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Center = [{flag, ?wxALIGN_CENTER_VERTICAL}],
    wxSizer:add(Sz, wxStaticText:new(Parent, ?wxID_ANY, Str), [{proportion, 1}|Center]),
    Opt = [{style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS}],
    Slider = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, Opt),
    wxSizer:add(Sz, Slider, [{proportion, 2}|Center]),
    case Min > 1 of
	false ->
	    {Sz, Slider};
	true ->
	    CB = fun(#wx{event=Ev},_) -> step(Ev, Slider, Min) end,
	    wxSlider:connect(Slider, scroll_thumbtrack, [{callback, CB}]),
	    wxSlider:connect(Slider, scroll_changed, [{callback, CB}]),
	    {Sz, Slider}
    end.

step(_Ev = #wxScroll{commandInt=Value}, Slider, Min) ->
    Val = Min * round(Value / Min),
    wxSlider:setValue(Slider, Val),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_max() -> {0, undefined}.
max_value({Max,_}) -> Max.
%% max_data({_,Data}) -> Data. matched in function head

lmax(MState, Tuple, Tuple) when is_tuple(Tuple) ->
    lmax(MState, tuple_to_list(Tuple), Tuple);
lmax(MState, Values, State) ->
    Max = max_value(MState),
    New = lists:max([Max|Values]),
    case New >= Max of
	false -> MState;
	true -> {New, State}
    end.

init_data(runq, {stats, _, T0, _, _}) -> {mk_max(),lists:sort(T0)};
init_data(io,   {stats, _, _, {{_,In0}, {_,Out0}}, _}) -> {mk_max(), {In0,Out0}};
init_data(memory, _) -> {mk_max(), info(memory, undefined, undefined)};
init_data(alloc, _) -> {mk_max(), unused};
init_data(utilz, _) -> {mk_max(), unused}.

info(runq, {stats, _, T0, _, _}, Node) ->
    Dirty = get_dirty_cpu(Node),
    {lists:seq(1, length(T0)-Dirty), Dirty};
info(memory, _, _) -> [total, processes, atom, binary, code, ets];
info(io, _, _) -> [input, output];
info(alloc, First, _) -> [Type || {Type, _, _} <- First];
info(utilz, First, _) -> [Type || {Type, _, _} <- First];
info(_, [], _) -> [].

get_dirty_cpu(Node) ->
    case rpc:call(node(Node), erlang, system_info, [dirty_cpu_schedulers]) of
        {badrpc,_R} -> 0;
        N -> N
    end.

collect_data(runq, {stats, _, T0, _, _}, {Max,S0}) ->
    S1 = lists:sort(T0),
    Delta = calc_delta(S1, S0),
    Sample = list_to_tuple(Delta),
    {Sample, {lmax(Max,Delta,Sample), S1}};
collect_data(io, {stats, _, _, {{_,In0}, {_,Out0}}, _}, {Max,{PIn,POut}}) ->
    In = In0-PIn,
    Out = Out0-POut,
    Sample = {In, Out},
    {Sample, {lmax(Max, [In,Out], Sample), {In0, Out0}}};
collect_data(memory, {stats, _, _, _, MemInfo}, {Max, MemTypes}) ->
    Vs = [Value || {Type,Value} <- MemInfo, lists:member(Type, MemTypes)],
    Sample = list_to_tuple(Vs),
    {Sample, {lmax(Max, Vs, Sample),MemTypes}};
collect_data(alloc, MemInfo, Max) ->
    Vs = [Carrier || {_Type,_Block,Carrier} <- MemInfo],
    Sample = list_to_tuple(Vs),
    {Sample, {lmax(Max, Vs, Sample),unused}};
collect_data(utilz, MemInfo, Max) ->
    Vs = [round(100*Block/Carrier) || {_Type,Block,Carrier} <- MemInfo],
    Sample = list_to_tuple(Vs),
    {Sample, {lmax(Max,Vs,Sample),unused}}.

calc_delta([{Id, WN, TN}|Ss], [{Id, WP, TP}|Ps]) ->
    [100*(WN-WP) div (TN-TP)|calc_delta(Ss, Ps)];
calc_delta([], []) -> [];
calc_delta(_, _) -> throw(no_scheduler_change).

precalc(#state{samples=Data0, paint=Paint, time=Ti, wins=Wins0}=State) ->
    Wins = [precalc(Ti, Data0, Paint, Win) || Win <- Wins0],
    State#state{wins=Wins}.

precalc(Ti, {NoSamples,Q}, Paint, #win{name=Id, panel=Panel}=Win) ->
    Size = wxWindow:getClientSize(Panel),
    case Win of
	#win{max=Max, no_samples=NoSamples, size=Size} when is_tuple(Max) ->
	    Win;
	_SomeThingChanged ->
	    Hs = [Vals || #{Id:=Vals} <- queue:to_list(Q)],
	    Max = lists:foldl(fun(Vals,Max) -> lmax(Max, Vals, Vals) end,
			      mk_max(), Hs),
	    MaxDisp = calc_max(Id, Max),
	    #{scale:={WS,HS}} = Props = window_geom(Size, MaxDisp, Ti, Panel, Paint),
	    NoGraphs = try tuple_size(hd(Hs)) catch _:_ -> 0 end,
	    Graphs = [make_lines(Hs, I, WS, HS) || I <- lists:seq(NoGraphs, 1, -1)],
	    State = case Win#win.state of
			undefined -> {Max, undefined};
			{_, St} -> {Max, St}
		    end,
	    Win#win{geom=Props, size=Size,
		    max=MaxDisp,
		    graphs=Graphs,
		    no_samples=NoSamples,
		    state=State}
    end.

window_geom({W,H}, {_, Max, _Unit, MaxUnit},
	    #ti{secs=Secs, fetch=FetchFreq},
	    Panel, #paint{font=Font}) ->
    Str1 = observer_lib:to_str(MaxUnit),
    Str2 = observer_lib:to_str(MaxUnit div 2),
    Str3 = observer_lib:to_str(0),
    {TW,TH,_,_} = wxWindow:getTextExtent(Panel, Str1, [{theFont, Font}]),
    {SpaceW, _,_,_} = wxWindow:getTextExtent(Panel, "W", [{theFont, Font}]),
    X0 = ?BW+TW+?BW,
    X1 = W-?BW*4,
    MaxTextY = TH+?BH,
    BottomTextY = H-?BH-TH,
    Y0 = MaxTextY + (TH / 2),
    Y1 = BottomTextY - TH - ?BH,

    ScaleW = (X1-X0-1)/(Secs*FetchFreq),
    ScaleH = (Y1-Y0-1) / Max,
    #{p0=>{X0,Y0}, p1=>{X1,Y1}, scale=>{ScaleW, ScaleH},
      txsz=>{TW,TH,SpaceW}, txt=>{BottomTextY, MaxTextY}, strs=>{Str1,Str2,Str3}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw_win(DC, #win{name=Name, no_samples=Samples, geom=#{scale:={WS,HS}},
                  graphs=Graphs, max={_,Max,_,_}, info=Info}=Win,
	 #ti{tick=Tick, fetch=FetchFreq, secs=Secs, disp=DispFreq}=Ti,
	 Paint=#paint{pens=Pens, dot_pens=Dots}) when Samples >= 2, Graphs =/= [] ->
    %% Draw graphs
    {X0,Y0,DrawBs} = draw_borders(DC, Ti, Win, Paint),
    Offset = Tick / DispFreq,
    Full = case Samples > (1+Secs*FetchFreq) of
	       true -> 1;
	       false -> 2
	   end,
    Start = X0 + (max(Secs*FetchFreq+Full-Samples, 0) - Offset)*WS,
    Last = Secs*FetchFreq*WS+X0,
    Dirty = case {Name, Info} of
                {runq, {_, DCpu}} -> DCpu;
                _ -> 0
            end,
    NoGraphs = length(Graphs),
    NoCpu = NoGraphs - Dirty,
    Draw = fun(Lines0, N) ->
                   case Dirty > 0 andalso N > NoCpu of
                       true  -> setPen(DC, element(1+ ((N-NoCpu-1) rem tuple_size(Dots)), Dots));
                       false -> setPen(DC, element(1+ ((N-1) rem tuple_size(Pens)), Pens))
                   end,
		   Order = lists:reverse(Lines0),
		   [{_,Y}|Lines] = translate(Order, {Start, Y0}, 0, WS, {X0,Max*HS,Last}, []),
		   strokeLines(DC, [{Last,Y}|Lines]),
		   N-1
	   end,
    lists:foldl(Draw, NoGraphs, Graphs),
    DrawBs(),
    ok;

draw_win(DC, #win{no_samples=Samples} = Win,Ti, #paint{small=Small}=Paint) ->
    %% Draw Error Msg
    try draw_borders(DC, Ti, Win, Paint) of
	{X0,_Y0,DrawBs} ->
	    Text = case Samples =< 1 of
		       true  -> "Waiting for data";
		       false -> "Information not available"
		   end,
	    setFont(DC, Small, {0,0,0}),
	    {_,WW} = getSize(DC),
	    drawText(DC, Text, X0 + 100, WW div 2),
	    DrawBs(),
	    ok
    catch _:_ -> %% Early redraws fail
	    ok
    end.

translate([{X0,Y}|Rest], {Sx,Sy}=Start, N, WS, {Cx,Cy,Cw}=Clip, Acc) ->
    X = min((N-X0)*WS+Sx,Cw),
    Next = if X0 > 0 -> N; true -> N+1 end,
    case X =< Cx of
	true ->
	    translate(Rest, Start, Next, WS, Clip, [{Cx,Sy-min(Cy,Y)}]);
	false ->
	    translate(Rest, Start, Next, WS, Clip, [{X,Sy-min(Cy,Y)}|Acc])
    end;
translate([], _, _, _, _, Acc) ->
    Acc.

add_lines(Vals, Drop, OldLines, I, WS, HS) ->
    Lines = strip(OldLines, Drop, 2),
    New = make_lines(Vals, I, WS, HS),
    New ++ Lines.

strip([{X,_}|Rest], Drop, N) when X > 0.0001, N > 0 ->
    strip(Rest, Drop, N);
strip([_|Rest], Drop, N) when N > 0 ->
    strip(Rest, Drop, N-1);
strip(List, empty, _) -> List;
strip(List, _, _) ->
    lists:reverse(strip(lists:reverse(List), empty, 1)).

make_lines(Ds = [Data|_], N, WS, HS) ->
    Y = element(N,Data),
    make_lines(Ds, N, WS, HS, Y, []).

make_lines([D1 | Ds = [D2|Rest]], N, WS, HS, Y0, Acc0) ->
    Y1 = element(N,D1),
    Y2 = element(N,D2),
    Y3 = case Rest of
	     [D3|_] -> element(N,D3);
	     [] -> Y2
	 end,
    This = {0, Y1*HS},
    Acc = if (abs(Y1-Y2) * HS) < 3.0 -> [This|Acc0];
	     WS < 3.0 -> [This|Acc0];
	     true -> make_splines(Y0,Y1,Y2,Y3,WS,HS,[This|Acc0])
	  end,
    make_lines(Ds, N, WS, HS, Y1, Acc);
make_lines([_D1], _N, _WS, _HS, _Y0, Acc) ->
    Acc.

make_splines(Y00,Y10,Y20,Y30,WS,HS,Acc) ->
    Y1 = Y10*HS,
    Y2 = Y20*HS,
    Steps = min(abs(Y1-Y2), WS/2),
    if Steps > 2 ->
	    Y0 = Y00*HS,
	    Y3 = Y30*HS,
	    Tan = spline_tan(Y0,Y1,Y2,Y3),
	    Delta = 1/Steps,
	    splines(Steps-1, 0.0, Delta, Tan, Y1,Y2, Acc);
       true ->
	    Acc
    end.

splines(N, XD, XD0, Tan, Y1,Y2, Acc) when N > 0 ->
    Delta = XD+XD0,
    Y = max(0, spline(Delta, Tan, Y1,Y2)),
    splines(N-1, Delta, XD0, Tan, Y1, Y2, [{1.0-Delta, Y}|Acc]);
splines(_N, _XD, _XD0, _Tan, _Y1,_Y2, Acc) ->
    Acc.

spline(T, {M1, M2}, Y1, Y2) ->
    %% Hermite Basis Funcs
    T2 = T*T,  T3 = T*T*T,
    H1 = 2*T3-3*T2+1,
    H2 = -2*T3+3*T2,
    H3 = T3-2*T2+T,
    H4 = T3-T2,
    %% Result
    M1*H3 + Y1*H1 + Y2*H2 + M2*H4.

spline_tan(Y0, Y1, Y2, Y3) ->
    S = 1.0,
    C = 0.5,
    %% Calc tangent values
    M1 = S*C*(Y2-Y0),
    M2 = S*C*(Y3-Y1),
    {M1,M2}.

draw_borders(DC, #ti{secs=Secs, fetch=FetchFreq},
	     #win{name=Type, geom=Geom, info=Info, max={_,_,Unit,_}},
	     #paint{pen=Pen, pen2=Pen2, font=Font, small=Small}) ->
    #{p0:={GraphX0, GraphY0}, p1:={GraphX1,GraphY1}, scale:={ScaleW0,_},
      txsz:={TW,TH,SpaceW}, txt:={BottomTextY, MaxTextY}, strs:={Str1,Str2,Str3}} = Geom,

    ScaleW = ScaleW0*FetchFreq,
    TopTextX = ?BW*3+TW,
    SecondsY = BottomTextY - TH,

    GraphY25 = GraphY0 + (GraphY1 - GraphY0) / 4,
    GraphY50 = GraphY0 + (GraphY1 - GraphY0) / 2,
    GraphY75 = GraphY0 + 3*(GraphY1 - GraphY0) / 4,

    setFont(DC, Small, {0,0,0}),
    Align = fun(Str, Y) ->
		    {StrW, _} = getTextExtent(DC, Str),
		    drawText(DC, Str, GraphX0 - StrW - ?BW, Y)
	    end,
    Align(Str1, MaxTextY),
    Align(Str2, GraphY50 - (TH / 2)),
    Align(Str3, GraphY1 - (TH / 2) + 1),

    setPen(DC, Pen),
    DrawSecs = fun(Sec, {Pos, Prev}) ->
		       Str = observer_lib:to_str(Sec) ++ "s",
		       X = GraphX0+Pos,
		       strokeLine(DC, X, GraphY0, X, GraphY1+5),
		       TxtX = X-SpaceW,
		       case TxtX > Prev of
			   true ->
			       drawText(DC, Str,  TxtX, SecondsY),
			       TxtW = SpaceW*length(Str),
			       {Pos + 10*ScaleW, TxtX+TxtW};
			   false ->
			       {Pos + 10*ScaleW, Prev}
		       end
	       end,
    lists:foldl(DrawSecs, {0, 0}, lists:seq(Secs,0, -10)),

    strokeLine(DC, GraphX0-3, GraphY25, GraphX1, GraphY25),
    strokeLine(DC, GraphX0-3, GraphY50, GraphX1, GraphY50),
    strokeLine(DC, GraphX0-3, GraphY75, GraphX1, GraphY75),

    setFont(DC, Font, {0,0,0}),

    Text = fun(X,Y, Str, PenId) ->
		   if PenId == 0 ->
			   setFont(DC, Font, {0,0,0});
		      PenId > 0 ->
			   Id = 1 + ((PenId-1) rem tuple_size(colors())),
			   setFont(DC, Font, element(Id, colors()))
		   end,
		   drawText(DC, Str, X, Y),
		   {StrW, _} = getTextExtent(DC, Str),
		   StrW + X + ?BW*2
	   end,

    case Type of
	runq ->
            {TextInfo, DirtyCpus} = Info,
	    drawText(DC, "Scheduler Utilization (%) ", TopTextX, ?BH),
	    TN0 = Text(TopTextX, BottomTextY, "Scheduler: ", 0),
            Id = fun(Id, Pos0) ->
                         Text(Pos0, BottomTextY, integer_to_list(Id), Id)
                 end,
	    TN1 = lists:foldl(Id, TN0, TextInfo),
            TN2 = Text(TN1, BottomTextY, "Dirty cpu: ", 0),
	    TN3 = lists:foldl(Id, TN2, lists:seq(1, DirtyCpus)),
            _ = Text(TN3, BottomTextY, "(dotted)", 0),
            ok;
	memory ->
	    drawText(DC, "Memory Usage " ++ Unit, TopTextX,?BH),
	    lists:foldl(fun(MType, {PenId, Pos0}) ->
				Str = to_string(MType),
				Pos = Text(Pos0, BottomTextY, Str, PenId),
				{PenId+1, Pos}
			end, {1, TopTextX}, Info);
	io ->
	    drawText(DC, "IO Usage " ++ Unit, TopTextX,?BH),
	    lists:foldl(fun(MType, {PenId, Pos0}) ->
				Str = to_string(MType),
				Pos = Text(Pos0, BottomTextY, Str, PenId),
				{PenId+1, Pos}
			end, {1, TopTextX}, Info);
	alloc ->
	    drawText(DC, "Carrier Size " ++ Unit, TopTextX,?BH);
	utilz ->
	    drawText(DC, "Carrier Utilization (%)" ++ Unit, TopTextX,?BH),
	    lists:foldl(fun(MType, {PenId, Pos0}) ->
				Str = to_string(MType),
				Pos = Text(Pos0, BottomTextY, Str, PenId),
				{PenId+1, Pos}
			end, {1, TopTextX}, Info)
    end,
    DrawBorder = fun() ->
			 setPen(DC, Pen2),
			 strokeLines(DC, [{GraphX0, GraphY0-1}, {GraphX0, GraphY1+1},
					  {GraphX1, GraphY1+1}, {GraphX1, GraphY0-1},
					  {GraphX0, GraphY0-1}])
		 end,
    {GraphX0+1, GraphY1, DrawBorder}.

to_string(Atom) ->
    Name = atom_to_list(Atom),
    case lists:reverse(Name) of
	"colla_" ++ Rev ->
	    uppercase(lists:reverse(Rev));
	_ ->
	    uppercase(Name)
    end.

uppercase([C|Rest]) ->
    [C-$a+$A|Rest].

calc_max(Type, Max) ->
    bytes(Type, Max).

bytes(runq, Max) ->
    Upper = calc_max1(max_value(Max)),
    {Max, Upper, "", Upper};
bytes(utilz, Max) ->
    Upper = calc_max1(max_value(Max)),
    {Max, Upper, "", Upper};
bytes(_, Max) ->
    B = max_value(Max),
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
	GB > 10 ->
	    Upper = calc_max1(GB),
	    {Max, Upper*1024*1024*1024, "(GB)", Upper};
	MB > 10 ->
	    Upper = calc_max1(MB),
	    {Max, Upper*1024*1024, "(MB)", Upper};
	KB >  0 ->
	    Upper = calc_max1(KB),
	    {Max, Upper*1024, "(KB)", Upper};
	true  ->
	    Upper = calc_max1(B),
	    {Max, Upper, "(B)", Upper}
    end.

calc_max1(Max) when Max < 10 ->
    10;
calc_max1(Max) ->
    case Max div 10 of
	X when X < 10 ->
	    case Max rem 10 of
		0 -> Max;
		_ ->
		    (X+1)*10
	    end;
	X ->
	    10*calc_max1(X)
    end.

colors() ->
    {{240, 100, 100}, {0, 128, 0},     {25, 45, 170},  {255, 165, 0},
     {220, 220, 40},  {100, 240, 240},{240, 100, 240}, {160, 40,  40},
     {100, 100, 240}, {140, 140, 0},  {25, 200, 100},  {120, 25, 240},
     {255, 140, 163}, {25, 120, 120}, {120, 25, 120},  {110, 90, 60}
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wxDC and ?wxGC wrappers

make_gc(Panel,UseGC) ->
    DC = case os:type() of
	     {win32, _} ->
		 %% Ugly hack to avoid flickering on windows, works on windows only
		 %% But the other platforms are doublebuffered by default
		 DC0 = wx:typeCast(wxBufferedPaintDC:new(Panel), wxPaintDC),
		 wxDC:clear(DC0),
		 DC0;
	     _ ->
		 wxPaintDC:new(Panel)
	 end,
    if UseGC -> {?wxGC:create(DC), DC};
       true ->  {false, DC}
    end.

destroy_gc({GC, DC}) ->
    (GC =/= false) andalso ?wxGC:destroy(GC),
    case DC =/= false andalso wx:getObjectType(DC) of
	false -> ok;
	Type -> Type:destroy(DC)
    end.

haveGC() ->
    try
	wxGraphicsRenderer:getDefaultRenderer(),
	true
    catch _:_  -> false
    end.

getSize({_, DC}) ->
    wxDC:getSize(DC).

setPen({false, DC}, Pen) ->
    wxDC:setPen(DC, Pen);
setPen({GC, _}, Pen) ->
    ?wxGC:setPen(GC, Pen).

setFont({false, DC}, Font, Color) ->
    wxDC:setTextForeground(DC, Color),
    wxDC:setFont(DC, Font);
setFont({GC, _}, Font, Color) ->
    ?wxGC:setFont(GC, Font, Color).

setBrush({false, DC}, Brush) ->
    wxDC:setBrush(DC, Brush);
setBrush({GC, _}, Brush) ->
    ?wxGC:setBrush(GC, Brush).

strokeLine({false, DC}, X0, Y0, X1, Y1) ->
    wxDC:drawLine(DC, {round(X0), round(Y0)}, {round(X1), round(Y1)});
strokeLine({GC, _}, X0, Y0, X1, Y1) ->
    ?wxGC:strokeLine(GC, X0, Y0, X1, Y1).

strokeLines(_, [_]) -> ok;
strokeLines({false, DC}, Lines) ->
    wxDC:drawLines(DC, [{round(X), round(Y)} || {X,Y} <- Lines]);
strokeLines({GC, _}, Lines) ->
    ?wxGC:strokeLines(GC, Lines).

drawRoundedRectangle({false, DC}, X0, Y0, X1, Y1, R) ->
    wxDC:drawRoundedRectangle(DC, {round(X0), round(Y0)}, {round(X1), round(Y1)}, round(R));
drawRoundedRectangle({GC, _}, X0, Y0, X1, Y1, R) ->
    ?wxGC:drawRoundedRectangle(GC, X0, Y0, X1, Y1, R).

drawText({false, DC}, Str, X, Y) ->
    wxDC:drawText(DC, Str, {round(X),round(Y)});
drawText({GC, _}, Str, X, Y) ->
    ?wxGC:drawText(GC, Str, X, Y).

getTextExtent({false, DC}, Str) ->
    wxDC:getTextExtent(DC, Str);
getTextExtent({GC, _}, Str) ->
    {W,H,_,_} = ?wxGC:getTextExtent(GC, Str),
    {W,H}.
