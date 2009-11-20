%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%
-module(etop_gui).
-author('siri@erix.ericsson.se').

-export([init/1,stop/1]).
-export([formatmfa/1,to_list/1]).% For etop_txt

-include("etop.hrl").
-include("etop_defs.hrl").

-import(etop, [loadinfo/1, meminfo/2, getopt/2]).

%% Heights
-define(BarH, 28).      % height of menubar
-define(LabelH, 90).    % height of label with system info
-define(GridLineH, 21). % height of one line in the table (grid)

%% Column numbers for grid - click to sort
-define(TimeCol, 3).
-define(RedsCol, 4).
-define(MemCol, 5).
-define(MsgQCol, 6).

%% Font
-define(Normal, {screen,12}).
-define(Bold, {screen,bold,12}).


%% -----------------------------------------------------------------------------
stop(_) -> ok.

init(Config) ->
    S = gs:start(),
    Width  = getopt(width, Config),
    TotLines = getopt(lines,Config)+1,

    %% Max number of processes shown in window at startup is 10
    %% If less than 10 lines is specified, window size fits number of lines
    WinH = if TotLines > 11 -> 11*?GridLineH + ?BarH + ?LabelH;
	      true -> TotLines*?GridLineH + ?BarH + ?LabelH
	   end,
    Win    = gs:create(window, S, 
		       [{title, "Erlang Top"},
			{map, true}, %% While debugging
			{configure, true},
			{width, Width}, {height, WinH}]),
    Bar  = gs:create(menubar, Win, []),
        
    FileButt    = gs:create(menubutton, Bar, [{label,{text, " File "}}]),
    OptionsButt = gs:create(menubutton, Bar, [{label,{text, " Options "}}]),
    File        = gs:create(menu, FileButt, []),
    Options     = gs:create(menu, OptionsButt, []),
    gse:named_menuitem(refresh, File,
		       [{label,{text," Refresh "}}]),
    gse:named_menuitem(dump, File,
		       [{label,{text," Dump to file "}}]),
    gse:named_menuitem(exit, File,
		       [{label,{text," Exit "}}]),
    
    gse:named_menuitem(accum, Options,
		       [{label,{text, " Accumulate "}}, 
			{itemtype, check}]),
    gse:named_menuitem(intv, Options,
    		       [{label,{text, " Update Interval "}}]),
    gse:named_menuitem(lines, Options,
    		       [{label,{text, " Number of Lines "}}]),
    Sort = gse:named_menuitem(sort, Options,
			      [{label,{text, " Sort "}},
			       {itemtype,cascade}]),
    SortMenu = gse:create(menu, Sort, []),
    gse:named_menuitem(runtime, SortMenu,
    		       [{label,{text, " Time "}},
			{itemtype,radio},{group,gr1}]),
    gse:named_menuitem(memory, SortMenu,
    		       [{label,{text, " Memory "}},
			{itemtype,radio},{group,gr1}]),
    gse:named_menuitem(reductions, SortMenu,
    		       [{label,{text, " Reductions "}},
			{itemtype,radio},{group,gr1}]),
    gse:named_menuitem(msg_q, SortMenu,
    		       [{label,{text, " Message Queue "}},
			{itemtype,radio},{group,gr1}]),
    
    SysInfo = gs:create(label,Win,[{x, 0}, {y, ?BarH},{align,sw},
				   {width, Width},{height,?LabelH}]),

    {GridH,VScroll} = calc_grid_h(WinH,TotLines),
    Grid = gse:grid(Win, 
		    [{x, 0}, {y, ?BarH+?LabelH},
		     {width, Width},
		     {height, GridH},
		     {hscroll, false},
		     {vscroll, VScroll},
		     {columnwidths, calc_column_w(Width)},
		     {rows, {1, TotLines}},
		     {font,?Normal}]),
    
    %% Header line
    GL1  = gse:gridline(Grid, [{{text, 1}, "PID"},
			       {{text, 2}, "Name or Initial Function"},
			       {{text, ?TimeCol}, "Time(us)"},
			       {{text, ?RedsCol}, "Reds"},
			       {{text, ?MemCol}, "Memory"},
			       {{text, ?MsgQCol}, "MsgQ"},
			       {{text, 7}, "Current Function"},
			       {bg, lightblue},
			       {row, 1},
			       {click, true}]),

    config_sort(GL1,getopt(sort,Config)),
    Info = do_update(Grid, SysInfo, Config),
    
    get_event(Info, Win, Grid, GL1, SysInfo, Config).

calc_column_w(W) ->
    %% W = [2x, 3x, 1x, 1x, 1x, 1x, 3x] = 12x
    RW = W-9, % just to make nice small margins on each side of grid
    X =  RW div 12,
    [2*X, 3*X, X, X, X, X, 3*X + (RW - 12*X)].   

config_sort(GL1,Sort) ->
    gs:config(Sort,[{select,true}]),
    lists:foreach(fun(S) -> 
			  gs:config(GL1,[{{font,S},?Normal}])
		  end,
		  [?TimeCol,?MemCol,?RedsCol,?MsgQCol]),
    case Sort of
	runtime -> gs:config(GL1,{{font,?TimeCol},?Bold});
	memory -> gs:config(GL1,{{font,?MemCol},?Bold});
	reductions -> gs:config(GL1,{{font,?RedsCol},?Bold});
	msg_q -> gs:config(GL1,{{font,?MsgQCol},?Bold})
    end.

config_lines(Win,Grid,TotLines) ->
    OldGridH = gs:read(Grid,height),
    NewLinesH = TotLines*?GridLineH,
    if  NewLinesH =< OldGridH -> 
	    gs:config(Win,[{height,NewLinesH+?BarH+?LabelH}]),
	    gs:config(Grid,[{rows,{1,TotLines}},
			    {height,NewLinesH},
			    {vscroll,false}]);
	true ->
	    gs:config(Grid,[{rows,{1,TotLines}},{vscroll,right}])
    end.

calc_grid_h(WinH,TotLines) ->
    LeftInWin = WinH - ?BarH - ?LabelH,
    TotGrid = TotLines * ?GridLineH,
    if LeftInWin >= TotGrid ->
	    {TotGrid,false};
       true ->
	    {LeftInWin,right}
    end.

set_win_h(Win,OrigH,TotLines) ->
    TotH = TotLines*?GridLineH + ?BarH + ?LabelH,
    if TotH >= OrigH -> OrigH;
       true -> gs:config(Win,[{height,TotH}]),
	       TotH
    end.

get_event(Info, Win, Grid, GL1, SysInfo, Config) ->
    receive 
	{gs, Win, configure,[],[W,H,_,_]} ->
	    TotLines = getopt(lines,Config)+1,
	    %% Will not make window higher than total number of lines
	    RealWinH = set_win_h(Win,H,TotLines),
	    {GridH,VScroll} = calc_grid_h(RealWinH,TotLines),
	    gs:config(Grid, [{width, W},
			     {columnwidths, calc_column_w(W)},
			     {height,GridH}, {vscroll,VScroll}]),
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);	
	{gs, refresh, _, _, _} ->
	    Info1 = do_update(Grid, SysInfo, Config),
	    get_event(Info1, Win, Grid, GL1, SysInfo, Config);
	{gs, dump, _, _, _} ->
	    case pop(Win,dump) of
		{ok,File} -> etop:dump(File);
		{error,cancel} -> ok
	    end,
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);
	{gs, Win, destroy, _, _} ->
	    normal;
	{gs, exit, _, _, _} ->
	    ok;
	{gs, accum, _, _, _} ->
	    Old = getopt(accum,Config),
	    etop:config(accumulate,not Old),
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);	
	{gs,intv,_,_,_} ->
	    case pop(Win,interval) of
		{ok,Intv} -> etop:config(interval,list_to_integer(Intv));
		{error,cancel} -> ok
	    end,
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);
	{gs,lines,_,_,_} ->
	    case pop(Win,lines) of
		{ok,Lines} -> etop:config(lines,list_to_integer(Lines));
		{error,cancel} -> ok
	    end,
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);
	{gs,Sort,_,_,_} when Sort=:=runtime; 
			     Sort=:=memory; 
			     Sort=:=reductions; 
			     Sort=:=msg_q ->
	    etop:config(sort,Sort),
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);	
	{gs,GL1,click,_,[Col,1,_]} -> 
	    case Col of 
		?TimeCol -> etop:config(sort, runtime); 
		?MemCol -> etop:config(sort, memory); 
		?RedsCol -> etop:config(sort, reductions); 
		?MsgQCol -> etop:config(sort, msg_q);
		_other -> ignore
	    end,
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);	
	{config,{Key,Value},Config1} ->
	    case Key of
		lines -> config_lines(Win,Grid,Value+1);
		sort -> config_sort(GL1,Value);
		accumulate -> gs:config(accum,[{select,Value}]);
		_ -> ok
	    end,
	    Info1 = do_update(Grid, SysInfo, Config1),
	    get_event(Info1, Win, Grid, GL1, SysInfo, Config1);
	{dump,Fd} -> 
	    etop_txt:do_update(Fd,Info,Config),
	    get_event(Info, Win, Grid, GL1, SysInfo, Config);
	Msg ->
	    io:format("~p got unexpected msg ~p~n", [?MODULE, Msg]),
	    get_event(Info, Win, Grid, GL1, SysInfo, Config)
    after getopt(intv,Config) ->
	    Info1 = do_update(Grid, SysInfo, Config),
	    get_event(Info1, Win, Grid, GL1, SysInfo, Config)
    end.

do_update(Grid, SysInfo, Config) ->
    Info = etop:update(Config),
    Lines = makegridlines(Info#etop_info.procinfo, Grid, 2),
    clear_lines(Lines, getopt(lines,Config) + 1, Grid),
    makesysinfo(getopt(node,Config),Info,SysInfo),
    Info.

%clear_lines(From, To, _Grid) when From > To -> ok;
clear_lines(From, To, Grid) ->
    case gs:read(Grid, {obj_at_row, From}) of
	undefined ->
	    ok;
	GridLine ->
	    gs:destroy(GridLine),
	    clear_lines(From + 1, To, Grid)
    end.
    
formatmfa({M, F, A}) ->
    io_lib:format("~w:~w/~w",[M, F, A]).

makegridlines([#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T], Grid, Count) ->
    update_gl(Grid, Count, [{{text, 1}, pid_to_list(Pid)},
			    {{text, 2}, to_list(Name)},
			    {{text, ?TimeCol}, 
			     if is_integer(Time)->integer_to_list(Time);
				true -> Time
			     end},
			    {{text, ?RedsCol}, integer_to_list(Reds)},
			    {{text, ?MemCol}, integer_to_list(Mem)},
			    {{text, ?MsgQCol}, integer_to_list(MQ)},
			    {{text, 7}, formatmfa(MFA)},
			    {row, Count}, {click, false}]), 
     makegridlines(T, Grid, Count + 1);
makegridlines([],_Grid,Count) ->
    Count.

update_gl(Grid, Row, GL) ->
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    gse:gridline(Grid,[{row, Row}|GL]);
	GridLine ->
	    gs:config(GridLine,GL)
    end.

to_list(Name) when is_atom(Name) -> atom_to_list(Name);
to_list({_M,_F,_A}=MFA) -> formatmfa(MFA).


makesysinfo(Node,Info,SysInfo) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info),
    case Info#etop_info.memi of
	undefined ->
	    Str = "No memory information is available.";
	Memi ->
	    [Tot,Procs,Atom,Bin,Code,Ets] = 
		meminfo(Memi, [total,processes,atom,binary,code,ets]),
	    Str = io_lib:fwrite(?SYSFORM,
				[Node,Clock,
				 Cpu,Tot,Bin,
				 NProcs,Procs,Code,
				 RQ,Atom,Ets])
    end,
    gs:config(SysInfo,[{label,{text,Str}},{font,?Normal}]).


pop(Win,Key) ->
    Pop = gs:create(window,Win,[{title,"Config"},
			     {width,160},{height,100}]),
    gs:create(label,Pop,[{label,{text,txt(Key)}},
			 {width,160}]),
    gs:create(entry,entry,Pop,[{x,10},{y,30},{width,130},
			       {keypress,true}]),
    gs:create(button,ok,Pop,[{width,45},{y,60},{x,10},
			     {label,{text,"Ok"}}]),
    gs:create(button,cancel,Pop,[{width,60},{y,60},{x,80},
				 {label,{text,"Cancel"}}]),
    gs:config(Pop,{map,true}),
    pop_loop(Pop).

pop_loop(Pop) ->
    receive
	{gs,entry,keypress,_,['Return'|_]} ->
	    Str = gs:read(entry,text),
	    gs:destroy(Pop),
	    {ok,Str};
	{gs,entry,keypress,_,_} -> % all other keypresses
	    pop_loop(Pop);
	{gs,ok,click,_,_} ->
	    Str = gs:read(entry,text),
	    gs:destroy(Pop),
	    {ok,Str};
	{gs,cancel,click,_,_} ->
	    gs:destroy(Pop),
	    {error,cancel};
	X ->
	    io:format("Got X=~w~n",[X]),
	    pop_loop(Pop)
    end.

txt(interval) -> "Enter new interval:";
txt(lines) -> "Enter number of lines:";
txt(dump) -> "Enter file name:".
