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
-module(gstk_gridline).
-compile([{nowarn_deprecated_function,{gs,val,2}},
          {nowarn_deprecated_function,{gs,val,3}}]).

-export([event/5,create/3,config/3,option/5,read/3,delete/2,destroy/3,
	read_option/5]).

-include("gstk.hrl").
-record(state,{canvas,ncols,max_range,cell_id, cell_pos,ids,db,tkcanvas}).
-record(item,{text_id,rect_id,line_id}).

%%-----------------------------------------------------------------------------
%% 			    GRIDLINE OPTIONS
%%
%%	text		Text
%%	row             Row
%%	data		Data
%%	fg		Color (default is the same as grid fg)
%%	click         	Bool
%%
%%-----------------------------------------------------------------------------

create(DB, Gstkid, Options) ->
    Pgstkid = gstk_db:lookup_gstkid(DB,Gstkid#gstkid.parent),
    Id = Gstkid#gstkid.id,
    #gstkid{widget_data=State} = Pgstkid,
    #state{cell_pos=CP,tkcanvas=TkW,ncols=Ncols} = State,
    Row = gs:val(row,Options),
    case check_row(CP,Row) of
	{error,Reason} -> {error,Reason};
	ok ->
	    Ngstkid = Gstkid#gstkid{widget=TkW},
	    gstk_db:insert_opts(DB,Id,[{data,[]},{row,Row}]),
	    update_cp_db(Ncols,Row,Id,CP),
	    config_line(DB,Pgstkid,Ngstkid,Row,Options),
	    Ngstkid
    end.

%%----------------------------------------------------------------------
%% Returns: ok|false
%%----------------------------------------------------------------------
check_row(_CellPos,undefined) ->
    {error,{gridline,{row,undefined}}};
check_row(CellPos,Row) ->
    case ets:lookup(CellPos,{1,Row}) of
	[] ->
	    {error,{gridline,row_outside_range,Row}};
	[{_,Item}] ->
	    case Item#item.line_id of
		free -> ok;
		_    ->
		    {error,{gridline,row_is_occupied,Row}}
	    end
    end.

%%----------------------------------------------------------------------
%% s => text item
%% p => rect item
%%----------------------------------------------------------------------
option(Option, _Gstkid, _TkW, DB,_) ->
    case Option of
	{{bg,_Item}, Color} -> {p,[" -f ", gstk:to_color(Color)]};
	{{text,_Item},Text} -> {s, [" -te ", gstk:to_ascii(Text)]};
	{{fg,_Item},Color} -> {sp,{[" -fi ", gstk:to_color(Color)],
				  [" -outline ", gstk:to_color(Color)]}};
	{{font,_Item},Font} -> {s,[" -font ",gstk_font:choose_ascii(DB,Font)]};
	_ -> invalid_option
    end.

%%----------------------------------------------------------------------
%% Is always called.
%% Clean-up my specific side-effect stuff.
%%----------------------------------------------------------------------
delete(DB, Gstkid) ->
    Row = gstk_db:opt(DB,Gstkid,row),
    gstk_db:delete_widget(DB, Gstkid),    
    {Gstkid#gstkid.parent, Gstkid#gstkid.id, gstk_gridline,[Gstkid, Row]}.

%%----------------------------------------------------------------------
%% Is called iff my parent is not also destroyed.
%%----------------------------------------------------------------------
destroy(DB, Lgstkid, Row) ->
    Ggstkid = gstk_db:lookup_gstkid(DB,Lgstkid#gstkid.parent),
    #gstkid{widget_data=State} = Ggstkid,
    config_line(DB,Ggstkid,Lgstkid,Row,
		[{bg,gstk_db:opt(DB,Ggstkid,bg)},
		 {fg,gstk_db:opt(DB,Ggstkid,fg)},{text,""}]),
    Ncols = State#state.ncols,
    update_cp_db(Ncols,Row,free,State#state.cell_pos).


config(DB, Gstkid, Opts) ->
    Pgstkid = gstk_db:lookup_gstkid(DB,Gstkid#gstkid.parent),
    case {gs:val(row,Opts,missing),gstk_db:opt(DB,Gstkid,row)} of
	{Row,Row} -> % stay here...
	    config_line(DB,Pgstkid,Gstkid,Row,Opts);
	{missing,Row} ->  % stay here...
	    config_line(DB,Pgstkid,Gstkid,Row,Opts);
	{NewRow,OldRow} ->
	    config_line(DB,Pgstkid,Gstkid,OldRow,Opts),
	    Ngstkid = gstk_db:lookup_gstkid(DB,Gstkid#gstkid.id),
	    case move_line(NewRow,OldRow,DB,Pgstkid#gstkid.widget_data,Ngstkid) of
		true -> 
		    gstk_db:insert_opt(DB,Ngstkid,{row,NewRow}),
		    ok;
		{error,_Reason} -> ok
	    end
    end,
    ok.

%%----------------------------------------------------------------------
%% Returns: true|false depending on if operation succeeded
%%----------------------------------------------------------------------
move_line(NewRow,OldRow,_DB,State,_Ngstkid) ->
    case ets:lookup(State#state.cell_pos,{1,NewRow}) of
	[] ->
	    {error,{gridline,row_outside_grid,NewRow}};
	[{_,#item{line_id=Lid}}] when Lid =/= free->
	    {error,{gridline,new_row_occupied,NewRow}};
	[{_,_NewItem}] ->
	    #state{tkcanvas=TkW,ncols=Ncols,cell_pos=CP} = State,
	    swap_lines(TkW,OldRow,NewRow,1,Ncols,CP),
	    true
    end.

%%----------------------------------------------------------------------
%% Purpose: swaps an empty newrow with a (oldrow) gridline
%%----------------------------------------------------------------------
swap_lines(TkW,OldRow,NewRow,Col,MaxCol,CellPos) when Col =< MaxCol ->
    [{_,NewItem}] = ets:lookup(CellPos,{Col,NewRow}),
    [{_,OldItem}] = ets:lookup(CellPos,{Col,OldRow}),
    swap_cells(TkW,NewItem,OldItem),
    ets:insert(CellPos,{{Col,NewRow},OldItem}),
    ets:insert(CellPos,{{Col,OldRow},NewItem}),
    swap_lines(TkW,OldRow,NewRow,Col+1,MaxCol,CellPos);
swap_lines(_,_,_,_,_,_) -> done.

swap_cells(TkW,#item{rect_id=NewRectId,text_id=NewTextId},
	   #item{rect_id=OldRectId,text_id=OldTextId}) ->
    Aorid = gstk:to_ascii(OldRectId),
    Aotid = gstk:to_ascii(OldTextId),
    Anrid = gstk:to_ascii(NewRectId),
    Antid = gstk:to_ascii(NewTextId),
    Pre = [TkW," coords "],
    OldRectCoords = tcl2erl:ret_str([Pre,Aorid]),
    OldTextCoords = tcl2erl:ret_str([Pre,Aotid]),
    NewRectCoords = tcl2erl:ret_str([Pre,Anrid]),
    NewTextCoords = tcl2erl:ret_str([Pre,Antid]),
    gstk:exec([Pre,Aotid," ",NewTextCoords]),
    gstk:exec([Pre,Antid," ",OldTextCoords]),
    gstk:exec([Pre,Aorid," ",NewRectCoords]),
    gstk:exec([Pre,Anrid," ",OldRectCoords]).

%%----------------------------------------------------------------------
%% Pre: {row,Row} option is taken care of.
%%----------------------------------------------------------------------
config_line(DB,Pgstkid,Lgstkid,Row,Opts) ->
    #gstkid{widget_data=State, widget=TkW} = Pgstkid,
    #state{cell_pos=CP,ncols=Ncols} = State,
    Ropts = transform_opts(Opts,Ncols),
    RestOpts = config_gridline(DB,CP,Lgstkid,Ncols,Row,Ropts),
    gstk_generic:mk_cmd_and_exec(RestOpts,Lgstkid,TkW,"","",DB).

%%----------------------------------------------------------------------
%% Returns: non-processed options
%%----------------------------------------------------------------------
config_gridline(_DB,_CP,_Gstkid,0,_Row,Opts) ->
    Opts;
config_gridline(DB,CP,Gstkid,Col,Row,Opts) ->
    {ColOpts,OtherOpts} = opts_for_col(Col,Opts,[],[]),
    if
	ColOpts==[] -> done;
	true ->
	    [{_pos,Item}] =  ets:lookup(CP,{Col,Row}),
	    TkW = Gstkid#gstkid.widget,
	    TextPre = [TkW," itemconf ",gstk:to_ascii(Item#item.text_id)],
	    RectPre = [$;,TkW," itemconf ",gstk:to_ascii(Item#item.rect_id)],
	    case gstk_generic:make_command(ColOpts,Gstkid,TkW,
					  TextPre,RectPre,DB) of
		[] -> ok;
		{error,_Reason} -> ok;
		Cmd -> gstk:exec(Cmd)
	    end
    end,
    config_gridline(DB,CP,Gstkid,Col-1,Row,OtherOpts).

opts_for_col(Col,[{{Key,Col},Val}|Opts],ColOpts,RestOpts) ->
    opts_for_col(Col,Opts,[{{Key,Col},Val}|ColOpts],RestOpts);
opts_for_col(Col,[Opt|Opts],ColOpts,RestOpts) ->
    opts_for_col(Col,Opts,ColOpts,[Opt|RestOpts]);
opts_for_col(_Col,[],ColOpts,RestOpts) -> {ColOpts,RestOpts}.

%%----------------------------------------------------------------------
%% {Key,{Col,Val}} becomes {{Key,Col},Val}
%% {Key,Val} becomes {{Key,1},Val}...{{Key,Ncol},Val}
%%----------------------------------------------------------------------
transform_opts([], _Ncols) -> [];
transform_opts([{{Key,Col},Val} | Opts],Ncols) ->
    [{{Key,Col},Val}|transform_opts(Opts,Ncols)];
transform_opts([{Key,{Col,Val}}|Opts],Ncols) when is_integer(Col) ->
    [{{Key,Col},Val}|transform_opts(Opts,Ncols)];
transform_opts([{Key,Val}|Opts],Ncols) ->
    case lists:member(Key,[fg,bg,text,font]) of
	true -> 
	    lists:append(expand_to_all_cols(Key,Val,Ncols),
			 transform_opts(Opts,Ncols));
	false ->
	    case lists:member(Key,[click,doubleclick,row]) of
		true ->
		    [{keep_opt,{Key,Val}}|transform_opts(Opts,Ncols)];
		false ->
		    [{Key,Val}|transform_opts(Opts,Ncols)]
	    end
    end;
transform_opts([Opt|Opts],Ncols) ->
    [Opt|transform_opts(Opts,Ncols)].

expand_to_all_cols(Key,Val,1) ->
    [{{Key,1},Val}];
expand_to_all_cols(Key,Val,Col) ->
    [{{Key,Col},Val}|expand_to_all_cols(Key,Val,Col-1)].
		     

read(DB, Gstkid, Opt) ->
    Pgstkid = gstk_db:lookup_gstkid(DB,Gstkid#gstkid.parent),
    gstk_generic:read_option(DB, Gstkid, Opt,Pgstkid).

read_option({font,Column},Gstkid, _TkW,DB,Pgstkid) -> 
    case gstk_db:opt_or_not(DB,Gstkid,{font,Column}) of
	false -> gstk_db:opt(DB,Pgstkid,font);
	{value,V} -> V
    end;
read_option({Opt,Column},Gstkid, TkW,DB,#gstkid{widget_data=State}) -> 
    Row = gstk_db:opt(DB,Gstkid,row),
    [{_pos,Item}] = ets:lookup(State#state.cell_pos,{Column,Row}),
    Rid = gstk:to_ascii(Item#item.rect_id),
    Tid = gstk:to_ascii(Item#item.text_id),
    Pre = [TkW," itemcg "],
    case Opt of
	bg -> tcl2erl:ret_color([Pre,Rid," -f"]);
	fg -> tcl2erl:ret_color([Pre,Tid," -fi"]);
	text -> tcl2erl:ret_str([Pre,Tid," -te"]);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, {Opt,Column}}}
    end;
read_option(Option,Gstkid,TkW,DB,Pgstkid) -> 
    case lists:member(Option,[bg,fg,text]) of
	      true -> read_option({Option,1},Gstkid,TkW,DB,Pgstkid);
	      false -> gstk_db:opt(DB,Gstkid,Option,undefined)
    end.

update_cp_db(0,_Row,_,_) -> ok;
update_cp_db(Col,Row,ID,CP) ->
    [{_,Item}] = ets:lookup(CP,{Col,Row}),
    ets:insert(CP,{{Col,Row},Item#item{line_id = ID}}),
    update_cp_db(Col-1,Row,ID,CP).


event(DB, GridGstkid, Etype, _Edata, [CanItem]) ->
    State = GridGstkid#gstkid.widget_data,
    #state{cell_pos=CP,cell_id=CIs,tkcanvas=TkW} = State,
    case ets:lookup(CIs,CanItem) of 
	[{_id,{Col,Row}}] ->
	    [{_pos,Item}] =  ets:lookup(CP,{Col,Row}),
	    case Item#item.line_id of
		free -> ok;
		Id   ->
		    Lgstkid = gstk_db:lookup_gstkid(DB,Id),
		    case gstk_db:opt_or_not(DB,Lgstkid,Etype) of
			{value,true}  ->
			    Txt = read_option({text,Col},Lgstkid,TkW,
					      DB,GridGstkid),
			    gstk_generic:event(DB,Lgstkid,Etype,dummy,
					      [Col,Row,Txt]);
			_ -> ok
		    end
	    end;
	_ -> ok
    end;
event(_DB, _Gstkid, _Etype, _Edata, _Args) ->
    ok.
