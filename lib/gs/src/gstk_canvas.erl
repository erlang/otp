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
%% ------------------------------------------------------------
%% Basic Canvas Type
%% ------------------------------------------------------------

-module(gstk_canvas).
-compile([{nowarn_deprecated_function,{gs,pair,2}},
          {nowarn_deprecated_function,{gs,val,2}}]).

%%-----------------------------------------------------------------------------
%% 			    CANVAS OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	scrollregion		{X1, Y1, X2, Y2}
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	vscroll			Bool | left | right
%%	width			Int
%%	x			Int
%%	y			Int
%%
%%
%%  Commands:
%%	find			{X, Y}	 =>    Item at pos X,Y or false
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	configure		[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
%%	focus			[Bool | {Bool, Data}]
%%	keypress		[Bool | {Bool, Data}]
%%	keyrelease		[Bool | {Bool, Data}]
%%	leave			[Bool | {Bool, Data}]
%%	motion			[Bool | {Bool, Data}]
%%
%%  Read Options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	fg			Color
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).
-export([make_command/5,make_command/6,pickout_coords/4, coords/1,
	item_config/3,mk_create_opts_for_child/4,
	upd_gstkid/3,item_delete_impl/2,mk_cmd_and_exec/6,mk_cmd_and_call/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gstkid, Opts) ->
    MainW = gstk_generic:mk_tkw_child(DB,Gstkid),
    Canvas = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gstk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Canvas,
		  hscroll=Hscroll, vscroll=Vscroll},
    NGstkid=Gstkid#gstkid{widget=MainW, widget_data=WidgetD},
    MandatoryCmd = ["so_create canvas ", MainW],
    case gstk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    gstk_db:insert_opt(DB,Gstkid,gs:pair(scrollregion,Opts)),
	    case gstk_generic:make_command(NewOpts, NGstkid, MainW,
			       SimplePreCmd, PlacePreCmd, DB,Canvas) of
		{error,Reason} -> {error,Reason};
		Cmd when is_list(Cmd) ->
		    gstk:exec(Cmd),
		    gstk:exec([MainW,".sy conf -rel sunken -bo 2;",
			      MainW,".pad.sx conf -rel sunken -bo 2;"]),
		    NGstkid
	    end;
	Bad_Result ->
	    {bad_result, Bad_Result}
    end.

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Options) ->
    SO = Gstkid#gstkid.widget_data,
    MainW = Gstkid#gstkid.widget,
    Canvas = SO#so.object,
    NewOpts = gstk_generic:parse_scrolls(Gstkid, Options),
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gstk_generic:mk_cmd_and_exec(NewOpts, Gstkid, MainW,
				SimplePreCmd, PlacePreCmd, DB,Canvas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(DB, Gstkid, Opt) ->
    SO = Gstkid#gstkid.widget_data,
    gstk_generic:read_option(DB, Gstkid, Opt,SO#so.object).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%
%% Return 	: TkWidget to destroy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gstkid) ->
    gstk_db:delete_widget(DB, Gstkid),
    Gstkid#gstkid.widget.

event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gstkid   - The gstkid of the widget
%%		  MainW   - The main tk-widget
%%		  Canvas  - The canvas tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option,Gstkid,_MainW,DB,Canvas) ->
    case Option of
	{scrollregion, {X1, Y1, X2, Y2}} ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
    	    {c, [Canvas, " conf -scrollr {",
		 gstk:to_ascii(X1), " ", gstk:to_ascii(Y1), " ",
		 gstk:to_ascii(X2), " ", gstk:to_ascii(Y2),"}"]};
	{yscrollpos, Y} ->
	    {_,Ymin,_,Ymax} = gstk_db:opt(DB,Gstkid,scrollregion),
	    K = 1/(Ymax-Ymin),
	    M = -K*Ymin,
	    PercentOffViewTop = K*Y+M,
	    {c, [Canvas," yvi mo ",gstk:to_ascii(PercentOffViewTop)]};
	{xscrollpos, X} ->
	    {Xmin,_,Xmax,_} = gstk_db:opt(DB,Gstkid,scrollregion),
	    K = 1/(Xmax-Xmin),
	    M = -K*Xmin,
	    PercentOffViewLeft = K*X+M,
	    {c, [Canvas," xvi mo ",gstk:to_ascii(PercentOffViewLeft)]};
	{buttonpress,    On} -> bind(DB, Gstkid, Canvas, buttonpress, On);
	{buttonrelease,  On} -> bind(DB, Gstkid, Canvas, buttonrelease, On);
	{configure,      On} -> bind(DB, Gstkid, Canvas, configure, On);
	{destroy,        On} -> bind(DB, Gstkid, Canvas, destroy, On);
	{enter,          On} -> bind(DB, Gstkid, Canvas, enter, On);
	{focus,          On} -> bind(DB, Gstkid, Canvas, focus, On);
	{keypress,       On} -> bind(DB, Gstkid, Canvas, keypress, On);
	{keyrelease,     On} -> bind(DB, Gstkid, Canvas, keyrelease, On);
	{leave,          On} -> bind(DB, Gstkid, Canvas, leave, On);
	{motion,         On} -> bind(DB, Gstkid, Canvas, motion, On);

	{secret_hack_gridit, GridGstkid} ->
	    CRef = gstk_db:insert_event(DB, GridGstkid, click, []),
	    ClickCmd = [Canvas, " bind all <ButtonRelease-1> {erlsend ", CRef, 
			" [",Canvas, " find withtag current]};"],
	    DRef = gstk_db:insert_event(DB, GridGstkid, doubleclick, []),
	    DclickCmd = [Canvas," bind all <Double-ButtonRelease-1> {erlsend ",
			 DRef," [",Canvas, " find withtag current]}"],
	    %% bind all at once for preformance reasons.
	    {c, [ClickCmd,DclickCmd]};
	{secret_forwarded_grid_event, {Event,On},GridGstkid} ->
	    bind(DB,GridGstkid,Canvas,Event,On);
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gstkid,_MainW,DB,Canvas) ->
    case Option of
	scrollregion  -> gstk_db:opt(DB,Gstkid,scrollregion);
	{hit, {X,Y}} ->
	    hit(DB,Canvas,X,Y,X,Y);
	{hit, [{X1,Y1},{X2,Y2}]} ->
	    hit(DB,Canvas,X1,Y1,X2,Y2);
	% {% hidden above, % of total area that is visible + % hidden above}
	yscrollpos ->
	    {PercentOffViewTop,_} = tcl2erl:ret_tuple([Canvas," yvi"]),
	    {_,Ymin,_,Ymax} = gstk_db:opt(DB,Gstkid,scrollregion),
	    K = 1/(Ymax-Ymin),
	    M = -K*Ymin,
	    _Y = round((PercentOffViewTop - M)/K);
	xscrollpos ->
	    {PercentOffViewLeft,_} = tcl2erl:ret_tuple([Canvas," xvi"]),
	    {Xmin,_,Xmax,_} = gstk_db:opt(DB,Gstkid,scrollregion),
	    K = 1/(Xmax-Xmin),
	    M = -K*Xmin,
	    _X = round((PercentOffViewLeft-M)/K);
	buttonpress   -> gstk_db:is_inserted(DB, Gstkid, buttonpress);
	buttonrelease -> gstk_db:is_inserted(DB, Gstkid, buttonrelease);
	configure     -> gstk_db:is_inserted(DB, Gstkid, configure);
	destroy       -> gstk_db:is_inserted(DB, Gstkid, destroy);
	enter         -> gstk_db:is_inserted(DB, Gstkid, enter);
	focus         -> gstk_db:is_inserted(DB, Gstkid, focus);
	keypress      -> gstk_db:is_inserted(DB, Gstkid, keypress);
	keyrelease    -> gstk_db:is_inserted(DB, Gstkid, keyrelease);
	leave         -> gstk_db:is_inserted(DB, Gstkid, leave);   
	motion        -> gstk_db:is_inserted(DB, Gstkid, motion);

	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

hit(DB,Canvas,X1,Y1,X2,Y2) ->
    Ax1 = gstk:to_ascii(X1),
    Ay1 = gstk:to_ascii(Y1),
    Ax2 = gstk:to_ascii(X2),
    Ay2 = gstk:to_ascii(Y2),
    case tcl2erl:ret_list([Canvas," find overlapping ",
			   Ax1,$ ,Ay1,$ ,Ax2,$ ,Ay2]) of
	Items when is_list(Items) ->
	    [{_,Node}] = ets:lookup(DB,frontend_node),
	    fix_ids(Items,DB,Canvas,Node);
	Other ->
	    {bad_result, Other}
    end.

fix_ids([Item|Items],DB,Canvas,Node) ->
    [{gstk_db:lookup_item(DB,Canvas,Item),Node}|fix_ids(Items,DB,Canvas,Node)];
fix_ids([],_,_,_) -> [].

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

%%
%% Event bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for c widgets
%%
bind(DB, Gstkid, TkW, Etype, On) ->
    case bind(DB, Gstkid, TkW, Etype, On, "") of
	invalid_option -> invalid_option;
	Cmd -> {c, Cmd}
    end.

bind(DB, Gstkid, TkW, Etype, On, WS) ->
    case On of
	true  -> ebind(DB, Gstkid, TkW, Etype, WS, "");
	false -> eunbind(DB, Gstkid, TkW, Etype, WS, "");
	{true, Edata} -> ebind(DB, Gstkid, TkW, Etype, WS, Edata);
	{false, Edata} -> eunbind(DB, Gstkid, TkW, Etype, WS, Edata);
	_     -> invalid_option
    end.


%%
%% Event bind on
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for complex widgets
%%
ebind(DB, Gstkid, TkW, Etype, WS, Edata) ->
    Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion -> [P, " <Motion> {erlsend ", Eref, " [",
			 TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      keypress ->
		  [P, " <Key> {erlsend ", Eref," %K %N 0 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Shift-Key> {erlsend ", Eref, " %K %N 1 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-Key> {erlsend ", Eref, " %K %N 0 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-Shift-Key> {erlsend ", Eref," %K %N 1 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]}"];
	      keyrelease ->
		  [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		  P," <Control-Shift-KeyRelease> {erlsend ",Eref," %K %N 1 1[",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]}"];
	      buttonpress ->
		  [P, " <Button> {erlsend ", Eref, " %b [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {erlsend ", Eref, " %b [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      leave -> [P, " <Leave> {erlsend ", Eref, "}"];
	      enter -> [P, " <Enter> {erlsend ", Eref, "}"];
	      destroy ->
		  [P, " <Destroy> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, "}}"];
	      focus ->
		  [P, " <FocusIn> {erlsend ", Eref, " true};" ,
		   P, " <FocusOut> {erlsend ", Eref, " false}"];
	      configure ->
		  [P, " <Configure> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, " %w %h %x %y}}"]
	  end,
    Cmd.
		  

%%
%% Unbind event
%%
%% Should return a list of tcl commands
%% Already checked for validation in bind/5
%%
%% WS = Widget suffix for complex widgets
%%
eunbind(DB, Gstkid, TkW, Etype, WS, _Edata) ->
    gstk_db:delete_event(DB, Gstkid, Etype),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion ->
		  [P, " <Motion> {}"];
	      keypress -> 
		   [P, " <KeyRelease> {};",
		    P, " <Shift-KeyRelease> {};",
		    P, " <Control-KeyRelease> {};",
		    P, " <Control-Shift-KeyRelease> {}"];
	      keyrelease -> 
		   [P, " <KeyRelease> {};",
		    P, " <Shift-KeyRelease> {};",
		    P, " <Control-KeyRelease> {};",
		    P, " <Control-Shift-KeyRelease> {}"];
	      buttonpress ->
		  [P, " <ButtonPress> {}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {}"];
	      leave ->
		  [P, " <Leave> {}"];
	      enter ->
		  [P, " <Enter> {}"];
	      destroy ->
		  [P, " <Destroy> {}"];
	      focus ->
		  [P, " <FocusIn> {};",
		   P, " <FocusOut> {}"];
	      configure ->
		  [P, " <Configure> {}"]
	  end,
    Cmd.

%%======================================================================
%% Item library
%%======================================================================

mk_cmd_and_exec(Options, Gstkid, Canvas, AItem, SCmd, DB) ->
    case make_command(Options, Gstkid, Canvas, AItem, SCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(Cmd)
    end.

mk_cmd_and_call(Opts,Gstkid, CanvasTkW, MCmd, DB) ->
    case make_command(Opts,Gstkid, CanvasTkW, MCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    case tcl2erl:ret_int(Cmd) of
		Item when is_integer(Item) ->
		    G2 = gstk_db:lookup_gstkid(DB,Gstkid#gstkid.id), % buu, not nice
		    NewGstkid = G2#gstkid{widget_data=Item},
		    NewGstkid;
		Bad_result ->
		    {error,Bad_result}
	    end
    end.
	    

%%----------------------------------------------------------------------
%% MCmd = Mandatory command
%% Comment: The problem: Create everything in one async command and
%%          get the canvas obj integer id no back then.
%% The trick is to do:
%% set w [canvas create rectangle x1 y1 x2 y2 -Option Value ...];
%% canvas Action $w ;$w
%% Comment: no placer options (we don't have to consider all permutations)
%%----------------------------------------------------------------------
make_command(Options, Gstkid, Canvas, AItem, SCmd, DB) ->
    case gstk_generic:out_opts(Options,Gstkid,Canvas,DB,AItem, [],[],[]) of
	{[], [], []} -> [];
	{Si, [], []} -> [SCmd, Si];
	{[], [], Co} -> Co;
	{Si, [], Co} -> [SCmd, Si, $;, Co];
	{error,Reason} -> {error,Reason}
    end.

make_command(Options, Gstkid, Canvas, MCmd, DB) ->
    case gstk_generic:out_opts(Options,Gstkid,Canvas,DB,"$w",[],[],[]) of
	{[], [], []} -> MCmd;
	{Si, [], []} -> [MCmd, Si];
	{[], [], Co} -> ["set w [", MCmd, "];", Co, "set d $w"];
	{Si, [], Co} -> ["set w [", MCmd, Si, "];", Co, "set d $w"];
	{error,Reason} -> {error,Reason}
    end.

item_config(DB, Gstkid, Opts) ->
    #gstkid{widget=Canvas,widget_data=Item}=Gstkid,
    AItem = gstk:to_ascii(Item),
    SCmd = [Canvas, " itemconf ", AItem],
    case make_command(Opts, Gstkid, Canvas, AItem, SCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(Cmd)
    end.

pickout_coords([{coords,Coords} | Rest], Opts, ObjType, NbrOfCoords) 
  when length(Coords) == NbrOfCoords ->
    case coords(Coords) of
	invalid ->
	    {error, io_lib:format("A ~w must have ~w coordinates",
				  [ObjType,NbrOfCoords])};
	RealCoords ->
	    {RealCoords, lists:append(Rest, Opts)}
    end;
pickout_coords([Opt | Rest], Opts, ObjType, NbrOfCoords) ->
    pickout_coords(Rest, [Opt|Opts], ObjType, NbrOfCoords);
pickout_coords([], _Opts, ObjType, NbrOfCoords) ->
    {error, io_lib:format("A ~w must have ~w coordinates",
			  [ObjType,NbrOfCoords])}.

coords([{X,Y} | R]) when is_number(X),is_number(Y) ->
    [gstk:to_ascii(X), " ", gstk:to_ascii(Y), " ", coords(R)];
coords([_]) -> %% not a pair
    invalid;
coords([]) ->
    [].

item_delete_impl(DB,Gstkid) ->
    gstk_db:delete_widget(DB, Gstkid),
    #gstkid{widget=Canvas,widget_data=Item,parent=P,id=ID,objtype=Type}=Gstkid,
    {P,ID,gstk_widgets:type2mod(Type), [Canvas, Item]}.


upd_gstkid(DB, Gstkid, Opts) ->
    #gstkid{parent=Parent,owner=Owner}=Gstkid,
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent, Owner),
    SO = Pgstkid#gstkid.widget_data,
    CanvasTkW = SO#so.object,
    gstk_db:insert_opt(DB,Gstkid,{coords,gs:val(coords,Opts)}),
    gstk_db:update_widget(DB,Gstkid#gstkid{widget=CanvasTkW,widget_data=no_item}).


%%% ----- Done -----


