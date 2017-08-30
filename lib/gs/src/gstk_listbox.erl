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
%% -----------------------------------------------------------
%% Basic Listbox Type
%% ------------------------------------------------------------

-module(gstk_listbox).

%%-----------------------------------------------------------------------------
%% 			    LISTBOX OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	fg			Color
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	items			[String, String, ... String]
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	selection		Index | clear
%%	selectmode		single|browse|multiple|extended
%%	vscroll			Bool | left | right
%%	width			Int
%%	x			Int
%%	xselection		Bool	(Good name?????)
%%	y			Int
%%
%%  Commands:
%%	add			{Index, String} | String
%%	change			{Index, String}
%%	clear
%%	del			Index | {FromIdx, ToIdx}
%%	get			Index
%%	see			Index
%%	selection			=> [Idx1,Idx2,Idx3...]
%%	setfocus		Bool
%%	size			Int
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
%%	configure		[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%	doubleclick		[Bool | {Bool, Data}]
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

-export([create/3,config/3,read/3,delete/2,event/5,wid_event/5,option/5,
	read_option/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    MainW = gstk_generic:mk_tkw_child(DB,GstkId),
    Listbox = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gstk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Listbox,
		  hscroll=Hscroll, vscroll=Vscroll},
    Gstkid=GstkId#gstkid{widget=MainW, widget_data=WidgetD},
    MandatoryCmd = ["so_create listbox ", MainW],
    case gstk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    case gstk_generic:make_command(NewOpts, Gstkid, MainW,SimplePreCmd,
					  PlacePreCmd, DB,Listbox) of
		{error,Reason} -> {error,Reason};
		Cmd when is_list(Cmd) ->
		    gstk:exec(Cmd),
		    gstk:exec([MainW,".sy conf -rel sunken -bo 2;",
			      MainW,".pad.sx conf -rel sunken -bo 2;",Listbox,
			 " conf -bo 2 -relief sunken -highlightth 2 -expo 0;"]),
		    Gstkid
	    end;
	Bad_Result ->
	    {error, Bad_Result}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Options) ->
    SO = Gstkid#gstkid.widget_data,
    MainW = Gstkid#gstkid.widget,
    Listbox = SO#so.object,
    NewOpts = gstk_generic:parse_scrolls(Gstkid, Options),
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gstk_generic:mk_cmd_and_exec(NewOpts, Gstkid, MainW,
				SimplePreCmd, PlacePreCmd, DB,Listbox).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gstkid, click, Edata, Args) ->
    wid_event(DB, Gstkid, click, Edata, Args);
event(DB, Gstkid, doubleclick, Edata, Args) ->
    wid_event(DB, Gstkid, doubleclick, Edata, Args);
event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).


%% widget specific events
wid_event(DB, Gstkid, Etype, Edata, _Args) ->
    SO = Gstkid#gstkid.widget_data,
    TkW = SO#so.object,
    CurIdx = tcl2erl:ret_int([TkW," index active;"]),
    CurTxt = tcl2erl:ret_str([TkW," get active;"]),
    CurSel = tcl2erl:ret_list([TkW," curselection;"]),
    Arg2 = [CurIdx,CurTxt,lists:member(CurIdx,CurSel)],
    gstk_generic:event(DB, Gstkid, Etype, Edata, Arg2).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gstkid   - The gstkid of the widget
%%		  MainW   - The main tk-widget
%%		  Listbox  - The listbox tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, MainW,DB, Listbox) ->
    case Option of
	{items,          Items} when is_list(Items) ->
	    {c, [Listbox," del 0 end ;", Listbox," ins 0 ",item_list(Items)]};
	{selection, {From, To}} when is_integer(From),is_integer(To) ->
	    {c,[Listbox," sel set ",gstk:to_ascii(From)," " ,gstk:to_ascii(To)]};
	{font,            Font} when is_tuple(Font) ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    {c, [Listbox," conf -font ",gstk_font:choose_ascii(DB,Font)]};
	{selection,   clear} ->
	    {c, [Listbox," sel clear 0 end"]};
	{selection,     Idx} when is_integer(Idx) ->
	    {c, [Listbox, " select set ", gstk:to_ascii(Idx)]};
	{selectmode,   Mode} ->
	    {c, [Listbox, " conf -selectm ", gstk:to_ascii(Mode)]};
	{xselection,   Bool} ->
	    {c, [Listbox, " conf -exportse ", gstk:to_ascii(Bool)]};
	{fg,          Color} ->
	    {c, [Listbox, " conf -fg ", gstk:to_color(Color)]};
	
	{del,   {From, To}} ->
	    {c, [Listbox, " del ", integer_to_list(From), " ",
		 integer_to_list(To)]};
	{del,          Idx} ->
	    {c, [Listbox, " del ", integer_to_list(Idx)]};
	clear               -> {c, [Listbox," del 0 end"]};
	{add,   {Idx, Str}} ->
	    {c, [Listbox, " ins ", integer_to_list(Idx), " ",
		 gstk:to_ascii(Str)]};
	{add,          Str} ->
	    {c, [Listbox," ins end ",gstk:to_ascii(Str)]};
	{change, {Idx, Str}} ->
	    {c, [Listbox, " del ", integer_to_list(Idx), $;,
		 Listbox, " ins ", integer_to_list(Idx), " " ,
		 gstk:to_ascii(Str)]};
	{see,        Idx} ->   
	    {c, [Listbox," see ",gstk:to_ascii(Idx)]};
	
	{setfocus,    true} -> {c, ["focus ", MainW]};
	{setfocus,   false} -> {c, ["focus ."]};
	
	{click,         On} -> cbind(DB, Gstkid, Listbox, click, On);
	{doubleclick,   On} -> cbind(DB, Gstkid, Listbox, doubleclick, On);
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/3
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GstkId,_MainW,DB,Listbox) ->
    case Option of
	fg            -> tcl2erl:ret_color([Listbox," cg -fg"]);
	font          -> gstk_db:opt(DB,GstkId,font,undefined);
	selection     -> tcl2erl:ret_list([Listbox, " curselection"]);
	setfocus      -> tcl2erl:ret_focus(Listbox, "focus");

	items         -> tcl2erl:ret_str_list([Listbox, " get 0 end"]);
	selectmode    -> tcl2erl:ret_atom([Listbox, " cg -selectmode"]);
	size          -> tcl2erl:ret_int([Listbox, " size"]);
	xselection    -> tcl2erl:ret_bool([Listbox, " cg -exportsel"]);
	{get, Idx}    -> tcl2erl:ret_str([Listbox, " get ",gstk:to_ascii(Idx)]);
        click         -> gstk_db:is_inserted(DB, GstkId, click);
        doubleclick   -> gstk_db:is_inserted(DB, GstkId, doubleclick);
	
	_ -> {bad_result, {GstkId#gstkid.objtype, invalid_option, Option}}
    end.


%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

item_list([H|T]) ->
    [gstk:to_ascii(H),$ |item_list(T)];
item_list([]) ->
    [].

cbind(DB, Gstkid, Listbox, Etype, {true, Edata}) ->
    Button = case Etype of
                 click       -> " <ButtonRelease-1> ";
                 doubleclick -> " <Double-ButtonRelease-1> "
             end,
    Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
    {c, ["bind " ,Listbox, Button, "{erlsend ", Eref," }"]};
 
cbind(DB, Gstkid, Listbox, Etype, true) ->    
    cbind(DB, Gstkid, Listbox, Etype, {true, []});
 
cbind(DB, Gstkid, Listbox, Etype, _On) ->    
    Button = case Etype of
                 click       -> " <Button-1> {}";
                 doubleclick -> " <Double-Button-1> {}"
             end,
    gstk_db:delete_event(DB, Gstkid, Etype),
    {c, ["bind ",Listbox, Button]}.


%%% ----- Done -----
