%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
-module(observer_perf_wx).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_sync_event/3, handle_cast/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-compile(export_all).

-record(state,
	{
	  offset = 0.0,
	  active = false,
	  parent,
	  windows,
	  data = {0, queue:new()},
	  panel,
	  paint,
	  appmon
	}).

-record(paint, {font, pen, pens}).

-define(RQ_W,  1).
-define(MEM_W, 2).
-define(IO_W,  3).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

init([Notebook, Parent]) ->
 try
    Panel = wxPanel:new(Notebook),
    %% wxWindow:setBackgroundColour(Panel, {222,222,222}),
    Main  = wxBoxSizer:new(?wxVERTICAL),

    CPU = wxPanel:new(Panel, [{winid, ?RQ_W}, {style,?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setBackgroundColour(CPU, ?wxWHITE),
    wxSizer:add(Main, CPU, [{flag, ?wxEXPAND bor ?wxALL},
				 {proportion, 1}, {border, 5}]),
    MemIO = wxBoxSizer:new(?wxHORIZONTAL),
    MEM = wxPanel:new(Panel, [{winid, ?MEM_W}, {style,?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setBackgroundColour(MEM, ?wxWHITE),
    IO  = wxPanel:new(Panel, [{winid, ?IO_W}, {style,?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:setBackgroundColour(IO, ?wxWHITE),
    wxSizer:add(MemIO, MEM, [{flag, ?wxEXPAND bor ?wxLEFT},
			     {proportion, 1}, {border, 5}]),
    wxSizer:add(MemIO, IO,  [{flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT},
			     {proportion, 1}, {border, 5}]),
    wxSizer:add(Main, MemIO, [{flag, ?wxEXPAND bor ?wxDOWN},
			      {proportion, 1}, {border, 5}]),
    wxWindow:setSizer(Panel, Main),

    wxPanel:connect(CPU, paint, [callback]),
    wxPanel:connect(IO, paint, [callback]),
    wxPanel:connect(MEM, paint, [callback]),
    % wxPanel:connect(DrawingArea, size, [{skip, true}]),

    DefFont  = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Pens = [wxPen:new(Col, [{width, 2}]) || Col <- tuple_to_list(colors())],
    %%  GC = wxGraphicsContext:create(DrawingArea),
    %%  _Font = wxGraphicsContext:createFont(GC, DefFont),
    {Panel, #state{parent=Parent,
		   panel =Panel,
		   windows = {CPU, MEM, IO},
		   paint=#paint{font= DefFont,
				pen = ?wxBLACK_PEN,
				pens = list_to_tuple(Pens)
			       }
		  }}
   catch _:Err ->
	   io:format("~p crashed ~p: ~p~n",[?MODULE, Err, erlang:get_stacktrace()]),
	   {error, Err}
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event=#wxCommand{type=command_menu_selected}},
	     State = #state{}) ->
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

%%%%%%%%%%
handle_sync_event(#wx{id=Id, event = #wxPaint{}},_,
		  #state{offset=Offset, paint=Paint, windows=Windows, data=Data}) ->
    %% PaintDC must be created in a callback to work on windows.
    Panel = element(Id, Windows),
    DC = wxPaintDC:new(Panel),
    %% Nothing is drawn until wxPaintDC is destroyed.
    try draw(Offset, Id, DC, Panel, Paint, Data)
    catch _:Err ->
	    io:format("Crash ~p ~p~n",[Err, erlang:get_stacktrace()])
    end,
    wxPaintDC:destroy(DC),
    ok.
%%%%%%%%%%
handle_call(Event, From, _State) ->
    error({unhandled_call, Event, From}).

handle_cast(Event, _State) ->
    error({unhandled_cast, Event}).
%%%%%%%%%%
handle_info(Stats = {stats, 1, _, _, _},
	    State = #state{panel=Panel, data=Data, active=Active}) ->
    if Active ->
	    wxWindow:refresh(Panel),
	    Freq = 6,
	    erlang:send_after(trunc(1000 / Freq), self(), {refresh, 1, Freq});
       true -> ignore
    end,
    {noreply, State#state{offset=0.0, data = add_data(Stats, Data)}};

handle_info({refresh, Seq, Freq}, State = #state{panel=Panel, offset=Prev}) ->
    wxWindow:refresh(Panel),
    Next = Seq+1,
    if Seq > 1, Prev =:= 0.0 ->
	    %% We didn't have time to handle the refresh
	    {noreply, State};
       Next < Freq ->
	    erlang:send_after(trunc(1000 / Freq), self(), {refresh, Next, Freq}),
	    {noreply, State#state{offset=Seq/Freq}};
       true ->
	    {noreply, State#state{offset=Seq/Freq}}
    end;

handle_info({active, Node}, State = #state{parent=Parent, appmon=Old}) ->
    create_menus(Parent, []),
    try
	Node = node(Old),
	{noreply, State#state{active=true}}
    catch _:_ ->
	    catch Old ! exit,
	    Me = self(),
	    Pid = spawn_link(Node, fun() -> fetch_stats(Me) end),
	    {noreply, State#state{active=true, appmon=Pid, data={0, queue:new()}}}
    end;

handle_info(not_active, State = #state{appmon=_Pid}) ->
    %% Pid ! exit,
    {noreply, State#state{active=false}};

handle_info(_Event, State) ->
    io:format("~p:~p: ~p~n",[?MODULE,?LINE,_Event]),
    {noreply, State}.

%%%%%%%%%%
terminate(_Event, #state{appmon=Pid}) ->
    Pid ! exit,
    ok.
code_change(_, _, State) ->
    State.

add_data(Stats, {N, Q}) when N > 60 ->
    {N, queue:drop(queue:in(Stats, Q))};
add_data(Stats, {N, Q}) ->
    {N+1, queue:in(Stats, Q)}.

fetch_stats(Parent) ->
    receive
	exit -> normal
    after 1000 ->
	    _M = Parent ! {stats, 1,
			   erlang:statistics(run_queues),
			   erlang:statistics(io),
			   erlang:memory()},
	    %% io:format("IO ~p~n",[element(4,_M)]),
	    fetch_stats(Parent)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_menus(Parent, _) ->
    MenuEntries =
	[{"File",
	  [
	  ]}
	],
    observer_wx:create_menus(Parent, MenuEntries).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_data(?RQ_W, {N, Q}) ->
    Data = [RQ || {stats, _Ver, RQ, _IO, _Mem} <- queue:to_list(Q)],
    {N, lmax(Data), Data};
collect_data(?MEM_W, {N, Q}) ->
    MemT = mem_types(),
    Data = [list_to_tuple([Value || {Type,Value} <- MemInfo,
				    lists:member(Type, MemT)])
	    || {stats, _Ver, _RQ, _IO, MemInfo} <- queue:to_list(Q)],
    {N, lmax(Data), Data};
collect_data(?IO_W, {N, Q}) ->
    case queue:to_list(Q) of
	[] ->  {0, 0, []};
	[_] -> {0, 0, []};
	[{stats, _Ver, _RQ, {{_,In0}, {_,Out0}}, _Mem}|Data0] ->
	    [_,_|Data=[First|_]] =
		lists:foldl(fun({stats, _, _, {{_,In}, {_,Out}}, _}, [PIn,Pout|Acc]) ->
				    [In,Out,{In-PIn,Out-Pout}|Acc]
			    end, [In0,Out0], Data0),
	    {N, lmax(Data), lists:reverse([First|Data])}
    end.

mem_types() ->
    [total, processes, system, atom, binary, code, ets].

lmax([]) -> 0;
lmax(List) ->
    lists:max([lists:max(tuple_to_list(T)) || T <- List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw(Offset, Id, DC, Panel, Paint=#paint{pens=Pens}, Data) ->
    {Len, Max, Hs} = collect_data(Id, Data),
    NoGraphs = try tuple_size(hd(Hs)) catch _:_ -> 0 end,
    Size = wxWindow:getClientSize(Panel),
    {X0,Y0,WS,HS} = draw_borders(Id, NoGraphs, DC, Size, Max, Paint),
    Start = max(61-Len, 0)*WS+X0 - Offset*WS,
    case Hs of
	[] -> ignore;
	_ ->
	    Draw = fun(N) ->
			   Lines = make_lines(Hs, Start, N, X0, Y0, WS, HS),
			   wxDC:setPen(DC, element(1+ (N-1 rem tuple_size(Pens)) , Pens)),
			   wxDC:drawLines(DC, Lines),
			   N+1
		   end,
	    [Draw(I) || I <- lists:seq(NoGraphs, 1, -1)]
    end,
    ok.

make_lines(Ds = [Data|_], PX, N, MinX, MaxY, WS, HS) ->
    Y = element(N,Data),
    make_lines(Ds, PX, N, MinX, MaxY, WS, HS, Y, []).

make_lines([D1 | Ds = [D2|Rest]], PX, N, MinX, MaxY, WS, HS, Y0, Acc0) ->
    Y1 = element(N,D1),
    Y2 = element(N,D2),
    Y3 = case Rest of
	     [D3|_] -> element(N,D3);
	     [] -> Y2
	 end,
    This = {max(MinX, round(PX)),MaxY-round(Y1*HS)},
    Acc = make_splines(Y0,Y1,Y2,Y3,PX,MinX,MaxY,WS,HS,[This|Acc0]),
    make_lines(Ds, PX+WS, N, MinX, MaxY, WS, HS, Y1, Acc);
make_lines([D1],  PX, N, _MinX, MaxY, _WS, HS, _Y0, Acc) ->
    Y1 = element(N,D1),
    [{round(PX),MaxY-round(Y1*HS)}|Acc].

make_splines(_Y0,Y1,Y2,_Y3, _PX, _MinX,_MaxY, _WS,HS, Acc)
  when (abs(Y1-Y2) * HS) < 3.0  ->
    Acc;
make_splines(_Y0,_Y1,_Y2,_Y3, _PX, _MinX,_MaxY, WS,_HS, Acc)
  when WS < 3.0 ->
    Acc;
make_splines(Y00,Y10,Y20,Y30,PX,MinX,MaxY,WS,HS,Acc) ->
    Y1 = Y10*HS,
    Y2 = Y20*HS,
    Steps = round(min(abs(Y1-Y2), WS)),
    if Steps > 2 ->
	    Y0 = Y00*HS,
	    Y3 = Y30*HS,
	    Tan = spline_tan(Y0,Y1,Y2,Y3),
	    Delta = 1/Steps,
	    splines(Steps-1, 0.0, Delta, Tan, Y1,Y2, PX, MinX,MaxY, Delta*WS, Acc);
       true ->
	    Acc
    end.

splines(N, XD, XD0, Tan, Y1,Y2, PX0, MinX,MaxY, WS, Acc) when N > 0 ->
    PX = PX0+WS,
    Delta = XD+XD0,
    if PX < MinX ->
	    splines(N-1, Delta, XD0, Tan, Y1, Y2, PX, MinX,MaxY, WS, Acc);
       true ->
	    Y = max(0,round(spline(Delta, Tan, Y1,Y2))),
	    %% io:format("Y1:~p Y(~p):~p Y2:~p~n",[round(Y1),round(X),round(Y),round(Y2)]),
	    splines(N-1, Delta, XD0, Tan, Y1, Y2, PX, MinX,MaxY, WS, [{round(PX), MaxY-Y}|Acc])
    end;
splines(_N, _XD, _XD0, _Tan, _Y1,_Y2, _PX, _MinX,_MaxY, _WS, Acc) -> Acc.

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

-define(BW, 5).
-define(BH, 5).

draw_borders(Type, NoGraphs, DC, {W,H}, Max0,
	     #paint{pen=Pen, font=Font}) ->
    Max = calc_max(Max0),
    wxDC:setPen(DC, Pen),
    wxDC:setFont(DC, Font),
    {Unit, MaxUnit} = bytes(Type, Max),
    Str1 = observer_lib:to_str(MaxUnit),
    Str2 = observer_lib:to_str(MaxUnit div 2),
    Str3 = observer_lib:to_str(0),
    {TW,TH} = wxDC:getTextExtent(DC, Str1),

    GraphX0 = ?BW+TW+?BW,
    GraphX1 = W-?BW*4,
    TopTextX = ?BW+TW+?BW,
    MaxTextY = ?BH+TH+?BH,
    BottomTextY = H-?BH-TH,
    SecondsY = BottomTextY - ?BH - TH,
    GraphY0 = MaxTextY + (TH div 2),
    GraphY1 = SecondsY - ?BH,
    GraphW = max(GraphX1-GraphX0-1, 60),
    GraphH = max(GraphY1-GraphY0-1, 100),
    ScaleW = GraphW / 60,
    ScaleH = calc_scale(GraphH, Max),

    case Type of
	?RQ_W ->  wxDC:drawText(DC, "CPU History - Run queue length", {TopTextX,?BH});
	?MEM_W -> wxDC:drawText(DC, "Memory Usage " ++ Unit, {TopTextX,?BH});
	?IO_W ->  wxDC:drawText(DC, "IO Usage " ++ Unit, {TopTextX,?BH})
    end,

    Align = fun(Str, Y) ->
		    {StrW, _} = wxDC:getTextExtent(DC, Str),
		    wxDC:drawText(DC, Str, {GraphX0 - StrW - ?BW, Y})
	    end,
    Align(Str1, MaxTextY),
    Align(Str2, MaxTextY - (TW div 2) + (GraphY1 - MaxTextY) div 2),
    Align(Str3, GraphY1 - (TH div 2) + 1),

    DrawSecs = fun(Secs, Pos) ->
		       Str = [observer_lib:to_str(Secs)|" s"],
		       wxDC:drawText(DC, Str,  {round(GraphX0-?BH+Pos), SecondsY}),
		       Pos + 10*ScaleW
	       end,
    lists:foldl(DrawSecs, 0, lists:seq(60,0, -10)),

    wxDC:drawLines(DC, [{GraphX0, GraphY0}, {GraphX0, GraphY1},
			{GraphX1, GraphY1}, {GraphX1, GraphY0}, {GraphX0, GraphY0}]),

    {SpaceW, _} = wxDC:getTextExtent(DC, " "),
    Text = fun(X,Y, Str, PenId) ->
		   if PenId == 0 ->
			   wxDC:setTextForeground(DC, {0,0,0});
		      PenId > 0 ->
			   wxDC:setTextForeground(DC, element(PenId, colors()))
		   end,
		   wxDC:drawText(DC, Str, {X, Y}),
		   {StrW, _} = wxDC:getTextExtent(DC, Str),
		   StrW + X + SpaceW
	   end,
    case Type of
	?RQ_W ->
	    TN0 = Text(?BW, BottomTextY, "Scheduler: ", 0),
	    lists:foldl(fun(Id, Pos0) ->
				Text(Pos0, BottomTextY, integer_to_list(Id), Id)
			end, TN0, lists:seq(1, NoGraphs));
	?MEM_W ->
	    lists:foldl(fun(MType, {PenId, Pos0}) ->
				Str = uppercase(atom_to_list(MType)),
				Pos = Text(Pos0, BottomTextY, Str, PenId),
				{PenId+1, Pos}
			end, {1, ?BW}, mem_types());
	?IO_W ->
	    TN0 = Text(?BW, BottomTextY, "Input", 1),
	    Text(TN0, BottomTextY, "Output", 2)
    end,
    {GraphX0+1, GraphY1, ScaleW, ScaleH}.

div2({Type, Int}) -> {Type, Int div 2};
div2(Int) -> Int div 2.

uppercase([C|Rest]) ->
    [C-$a+$A|Rest].

calc_max(Max) when Max < 10 -> 10;
calc_max(Max) -> calc_max1(Max).

calc_max1(Max) ->
    case Max div 10 of
	X when X < 10 ->
	    (X+1)*10;
	X ->
	    10*calc_max1(X)
    end.

bytes(?RQ_W, Val) -> {"", Val};
bytes(_, B) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
	GB > 10 -> {"(GB)", GB};
	MB > 10 -> {"(MB)", MB};
	KB >  0 -> {"(KB)", KB};
	true    -> {"(B)", B}
    end.

calc_scale(H, {_Type, Max}) -> calc_scale(H,Max);
calc_scale(Height, Max) when Height > Max ->
    Height / Max;
calc_scale(Height, Max) ->
    Height / Max.

colors() ->
    {{220, 50, 50}, {50, 220, 50}, {50, 50, 220},
     {220, 220, 50}, {50, 220, 220}, {220, 50, 220},
     {220, 100, 100}, {220, 100, 220},
     {100, 220, 220}, {100, 220, 100}
    }.
