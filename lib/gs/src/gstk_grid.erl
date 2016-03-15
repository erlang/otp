%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%%

%%
-module(gstk_grid).
-compile([{nowarn_deprecated_function,{gs,val,2}}]).

-export([event/5,create/3,config/3,option/5,read/3,delete/2,destroy/2,
	 mk_create_opts_for_child/4,read_option/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%% 			    GRID OPTIONS
%%
%%	rows		{ViewFrom, ViewTo}
%%	columnwidths	[CW1, CW2, ..., CWn]
%%	vscroll		Bool | left | right
%%	hscroll		Bool | top | bottom
%%	x		Coord
%%	y		Coord
%%	width		Int
%%	height		Int
%%	fg		Color  (lines and default line color)
%%	bg		Color
%%-----------------------------------------------------------------------------

-record(state,{canvas,ncols,max_range,cell_id, cell_pos,ids,db,tkcanvas}).
-record(item,{text_id,rect_id,line_id}).

%%======================================================================
%% Interfaces
%%======================================================================

event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_gridline:event(DB, Gstkid, Etype, Edata, Args).

create(DB, Gstkid, Options) ->
    WinParent=Gstkid#gstkid.parent,
    {OtherOpts,CanvasOpts} = parse_opts(Options,[],[]),
    %% Why this (secret) hack? Performance reasons.
    %% if we ".canvas bind all" once and for all, then we can
    %% create lines twice as fast since we don't have to bind each line.
    C = make_ref(),
    gstk:create_impl(DB,{a_grid, {canvas,C,WinParent,
				 [{secret_hack_gridit, Gstkid}
				  | CanvasOpts]}}),
    CanvasGstkid = gstk_db:lookup_gstkid(DB, C),
    Wid = CanvasGstkid#gstkid.widget,
    SO = CanvasGstkid#gstkid.widget_data,
    TkCanvas = SO#so.object,
    CI=ets:new(gstk_grid_cellid,[private,set]),
    CP=ets:new(gstk_grid_cellpos,[private,set]),
    IDs=ets:new(gstk_grid_id,[private,set]),
    S=#state{db=DB,ncols=length(gs:val(columnwidths,OtherOpts)),
	     canvas=C,cell_id=CI,tkcanvas=TkCanvas,cell_pos=CP,ids=IDs},
    Ngstkid = Gstkid#gstkid{widget=Wid,widget_data=S},
    gstk_db:insert_opts(DB,Ngstkid,OtherOpts),
    gstk_db:insert_widget(DB,Ngstkid),
    gstk_generic:mk_cmd_and_exec(lists:keydelete(columnwidths,1,OtherOpts),
				Ngstkid, TkCanvas,"","", DB,nop).

config(DB, Gstkid, Options) ->
    #gstkid{widget=TkW,widget_data=State}=Gstkid,
    {OtherOpts,CanvasOpts} = parse_opts(Options,[],[]),
    case gstk:config_impl(DB,State#state.canvas,CanvasOpts) of
	ok ->
	    SimplePreCmd = "nyi?",
	    PlacePreCmd = [";place ", TkW],
	    gstk_generic:mk_cmd_and_exec(OtherOpts,Gstkid,TkW,
					SimplePreCmd,PlacePreCmd,DB,State);
	Err -> Err
    end.
    

option(Option, Gstkid, _TkW, DB,State) ->
    case Option of
	{rows,{From,To}} ->
	    Ngstkid = reconfig_rows(From,To,Gstkid),
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    gstk_db:update_widget(DB,Ngstkid),
	    {none,Ngstkid};
	{fg,_Color} ->
	    reconfig_grid(DB,Option,State),
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    none;
	{bg,_Color} ->
	    reconfig_grid(DB,Option,State),
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    none;
	{font,_Font} ->
	    reconfig_grid(DB,Option,State),
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    none;
	{columnwidths,ColWs} ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    Rows = gstk_db:opt(DB,Gstkid,rows),
	    CellHeight = gstk_db:opt(DB,Gstkid,cellheight),
	    gstk:config_impl(DB,State#state.canvas,
			    [calc_scrollregion(Rows,ColWs,CellHeight)]),
	    %% Crash upon an error msg (so we know WHY)
	    {result,_} = gstk:call(["resize_grid_cols ",State#state.tkcanvas,
				   " [list ",asc_tcl_colw(ColWs),"]"]),
	    none;
	{cellheight,_Height} ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    none;	
	_ ->
	    invalid_option
    end.

reconfig_grid(_,_,nop) -> done;
reconfig_grid(DB,Option,#state{tkcanvas=TkW,cell_pos=CP,
			       ncols=Ncols,max_range={From,To}}) ->
    reconfig_grid(DB,TkW,Option,From,To,CP,Ncols).
    
reconfig_grid(DB,TkW,Opt,Row,MaxRow,CellPos,Ncols) when Row =< MaxRow ->
    [{_,Item}] = ets:lookup(CellPos,{1,Row}),
    case Item#item.line_id of
	free -> empty_cell_config(DB,TkW,Row,1,Ncols,CellPos,Opt);
	GridLine ->
	    gstk_gridline:config(DB,gstk_db:lookup_gstkid(DB,GridLine),
				[Opt])
    end,
    reconfig_grid(DB,TkW,Opt,Row+1,MaxRow,CellPos,Ncols);
reconfig_grid(_,_,_,_,_,_,_) -> done.

%%----------------------------------------------------------------------
%% Purpose: Config an empty cell (i.e. has no gridline)
%%----------------------------------------------------------------------
empty_cell_config(DB,TkW,Row,Col,Ncols,CellPos,Opt) when Col =< Ncols ->
    [{_,Item}] = ets:lookup(CellPos,{Col,Row}),
    empty_cell_config(DB,TkW,Item,Opt),
    empty_cell_config(DB,TkW,Row,Col+1,Ncols,CellPos,Opt);
empty_cell_config(_,_,_,_,_,_,_) -> done.

empty_cell_config(_,TkW,#item{rect_id=Rid},{bg,Color}) ->
    gstk:exec([TkW," itemconf ",gstk:to_ascii(Rid)," -f ",gstk:to_color(Color)]);
empty_cell_config(_,TkW,#item{rect_id=Rid,text_id=Tid},{fg,Color}) ->
    Acolor = gstk:to_color(Color),
    Pre = [TkW," itemconf "],
    RectStr = [Pre, gstk:to_ascii(Rid)," -outline ",Acolor],
    TexdStr = [Pre,  gstk:to_ascii(Tid)," -fi ",Acolor],
    gstk:exec([RectStr,$;,TexdStr]);
empty_cell_config(DB,TkW,#item{text_id=Tid},{font,Font}) ->
    gstk:exec([TkW," itemconf ",gstk:to_ascii(Tid)," -font ",
	      gstk_font:choose_ascii(DB,Font)]);
empty_cell_config(_,_,_,_) -> done.



reconfig_rows(From, To, Gstkid) ->
    #gstkid{widget_data=State,id=Id} = Gstkid,
    #state{tkcanvas=TkCanvas,cell_pos=CP,cell_id=CI,
	   canvas=C,db=DB,max_range=Range}=State,
    NewRange =
	if Range == undefined ->
		mkgrid(DB,CP,CI,TkCanvas,Id,From,To),
		{From,To};
	   true ->
		{Top,Bot} = Range,
		if
		    From < Top -> % we need more rects above
			mkgrid(DB,CP,CI,TkCanvas,Id,From,Top-1);
		    true -> true
		end,
		if
		    To > Bot ->   % we need more rects below
			mkgrid(DB,CP,CI,TkCanvas,Id,Bot+1,To);
		    true -> true
		end,
		{lists:min([Top, From]), lists:max([Bot, To])}
	end,
    gstk:config_impl(DB,C,[calc_scrollregion({From,To},
					    gstk_db:opt(DB,Id,columnwidths),
					    gstk_db:opt(DB,Id,cellheight))]),
    S2 = State#state{max_range=NewRange},
    Gstkid#gstkid{widget_data=S2}.

read(DB,Gstkid,Opt) ->
    State = Gstkid#gstkid.widget_data,
    case lists:member(Opt,[x,y,width,height,hscroll,vscroll]) of
	true -> gstk:read_impl(DB,State#state.canvas,Opt);
	false ->
	    gstk_generic:read_option(DB, Gstkid, Opt,State)
      end.

read_option(Option,Gstkid,_TkW,DB,State) -> 
    case Option of
	{obj_at_row,Row} ->
	    case ets:lookup(State#state.cell_pos,{1,Row}) of
		[{_pos,Item}] ->
		    case Item#item.line_id of
			free -> undefined;
			GridLine ->
			    gstk:make_extern_id(GridLine, DB)
		    end;
		_ -> undefined
	    end;
	Opt -> gstk_db:opt(DB,Gstkid#gstkid.id,Opt,undefined)
    end.


%%----------------------------------------------------------------------
%% Is always called.
%% Clean-up my specific side-effect stuff.
%%----------------------------------------------------------------------
delete(DB, Gstkid) ->
    gstk_db:delete_widget(DB, Gstkid),
    State = Gstkid#gstkid.widget_data,
    #state{canvas=C,cell_pos=CP,cell_id=CIs, ids=IDs} = State,
    ets:delete(CP),
    ets:delete(CIs),
    ets:delete(IDs),
    {Gstkid#gstkid.parent, Gstkid#gstkid.id, gstk_grid, [C]}.

%%----------------------------------------------------------------------
%% Is called iff my parent is not also destroyed.
%%----------------------------------------------------------------------
destroy(DB, Canvas) ->
    gstk:destroy_impl(DB,gstk_db:lookup_gstkid(DB,Canvas)).

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

mkgrid(DB,CellPos,CellIds,TkCanvas,Id,From,To) ->
    ColWs = gstk_db:opt(DB,Id,columnwidths),
    AscColW = ["[list ",asc_tcl_colw(ColWs),"]"],
    Font = gstk_font:choose_ascii(DB,gstk_db:opt(DB,Id,font)),
    Fg = gstk:to_color(gstk_db:opt(DB,Id,fg)),
    Bg = gstk:to_color(gstk_db:opt(DB,Id,bg)),
    Objs = tcl2erl:ret_list(["mkgrid ",TkCanvas," ",AscColW," ",
			     gstk:to_ascii(From)," ",
			     gstk:to_ascii(To)," ",
			     gstk:to_ascii(gstk_db:opt(DB,Id,cellheight))," ",
			     Font," ",Fg," ",Bg]),
    insert_objs(CellPos,CellIds,From,To,1,length(ColWs)+1,Objs).

insert_objs(_,_,_,_,_,_,[]) -> done;
insert_objs(CP,CI,Row,T,MaxCol,MaxCol,Objs) ->
    insert_objs(CP,CI,Row+1,T,1,MaxCol,Objs);
insert_objs(CellPos,CellIds,Row,To,Col,Ncols,[RectId,TextId|Objs]) ->
    ets:insert(CellPos,{{Col,Row},
		   #item{text_id=TextId,rect_id=RectId,line_id=free}}),
    ets:insert(CellIds,{RectId,{Col,Row}}),
    ets:insert(CellIds,{TextId,{Col,Row}}),
    insert_objs(CellPos,CellIds,Row,To,Col+1,Ncols,Objs).

asc_tcl_colw([]) -> "";
asc_tcl_colw([Int|T]) -> [gstk:to_ascii(Int)," "|asc_tcl_colw(T)].

%%----------------------------------------------------------------------
%% Args: Cols  list of column sizes (measured in n-chars)
%%----------------------------------------------------------------------
calc_scrollregion({From, To}, Cols, Height) ->
    {scrollregion, {0, ((From-1) * Height) + From,
		    lists:sum(Cols)+length(Cols)+1, (To * Height)+ To+1}}.

parse_opts([],OtherOpts,CanvasOpts) -> {OtherOpts,CanvasOpts};
parse_opts([{Key,Val}|Opts],OtherOpts,CanvasOpts) ->
    case lists:member(Key,[x,y,width,height,vscroll,hscroll]) of
	true -> parse_opts(Opts,OtherOpts,[{Key,Val}|CanvasOpts]);
	false -> parse_opts(Opts,[{Key,Val}|OtherOpts],CanvasOpts)
    end;
parse_opts([Opt|Opts],OtherOpts,CanvasOpts) ->
    parse_opts(Opts,[Opt|OtherOpts],CanvasOpts).

