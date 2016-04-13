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
%% Basic Editor Type
%% ------------------------------------------------------------

-module(gstk_editor).
-compile([{nowarn_deprecated_function,{gs,assq,2}},
          {nowarn_deprecated_function,{gs,error,2}},
          {nowarn_deprecated_function,{gs,val,2}}]).

%%------------------------------------------------------------------------------
%% 			    CANVAS OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	fg			Color
%%      font                    Font
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	insertbg		Color
%%	insertbw		Wth
%%      insertpos               {Row,Col}|'end'  (Row: 1..Max, Col: 0..Max)
%%	justify			left|right|center
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	vscroll			Bool | left | right
%%	width			Int
%%	wrap			none | char | word
%%	x			Int
%%	y			Int
%%
%%
%%  Commands:
%%	clear
%%	del			{FromIdx, ToIdx} 
%%	enable			Bool
%%	file			String
%%	get			{FromIdx, ToIdx} => Text
%%	insert			{Index, Text}Index = [insert,{Row,lineend},end,{Row,Col}]
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
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

%.t tag names 2.7 -> red blue (blue is the colour)
%.t tag add blue 2.1 2.10    tag the text
%.t tag configure blue -foregr blue create tag
% .t index end -> MaxRows.cols
% .t yview moveto (Row-1)/MaxRows

-export([create/3, config/3, read/3, delete/2,event/5,option/5,read_option/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gstkid, Opts) ->
    MainW = gstk_generic:mk_tkw_child(DB,Gstkid),
    Editor = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gstk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Editor,
		 hscroll=Hscroll, vscroll=Vscroll,misc=[{1,white}]},
    NGstkid=Gstkid#gstkid{widget=MainW, widget_data=WidgetD},
    gstk_db:insert_widget(DB,NGstkid),
    MandatoryCmd = ["so_create text ", MainW],
    case gstk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    case gstk_generic:make_command(NewOpts, NGstkid, MainW, SimplePreCmd,
					  PlacePreCmd, DB,Editor) of
		{error,Reason} -> {error,Reason};
		Cmd ->
		    gstk:exec(Cmd),
		    gstk:exec(
		      [Editor," conf -bo 2 -relief sunken -highlightth 2;",
		       MainW,".sy conf -rel sunken -bo 2;",
		       MainW,".pad.sx conf -rel sunken -bo 2;",
		       Editor, " tag co c1 -for white;"]),
		    ok
	    end
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
    Editor = SO#so.object,
    NewOpts =
	case {gs:assq(vscroll,Options),gs:assq(hscroll,Options)} of
	    {false,false} -> Options;
	    _ -> gstk_generic:parse_scrolls(Gstkid, Options)
	end,
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gstk_generic:mk_cmd_and_exec(NewOpts, Gstkid, MainW, SimplePreCmd,
				PlacePreCmd, DB, Editor).


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
%%		  Editor  - The Editor tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, _MainW, DB, Editor) ->
    case Option of
	{font,Font} when is_tuple(Font) ->   
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    {c, [Editor, " conf -font ", gstk_font:choose_ascii(DB,Font)]};
	{font_style, {{Start,End},Font}} -> % should be only style
	    {Tag,Ngstkid} = get_style_tag(DB,Editor,Font,Gstkid),
	    gstk_db:update_widget(DB,Ngstkid),
	    {c, Ngstkid, [Editor, " tag ad ", Tag, " ", p_index(Start), " ",
			 p_index(End)]};
	{fg, {{Start,End},Color}} ->
	    {Tag,Ngstkid} = get_color_tag(Editor,Color,Gstkid),
	    gstk_db:update_widget(DB,Ngstkid),
	    {c, Ngstkid, [Editor, " tag ad ", Tag, " ", p_index(Start), " ",
			 p_index(End)]};
	{padx,          Pad} -> {c, [Editor," conf -padx ",gstk:to_ascii(Pad)]};
	{pady,          Pad} -> {c, [Editor," conf -pady ",gstk:to_ascii(Pad)]};
	{selection, {From, To}} ->
	    {c, [Editor," tag ad sel ",p_index(From)," ", p_index(To)]};
	{vscrollpos, Row} ->
	    {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
	    {c, [Editor, " yv mo ",gstk:to_ascii(Row/MaxRow)]};
	{wrap,          How} ->
	    {c, [Editor, " conf -wrap ", gstk:to_ascii(How)]};
	{fg,          Color} ->
	    {c, [Editor, " conf -fg ", gstk:to_color(Color)]};
	{insertbw,      Wth} ->
	    {c, [Editor, " conf -insertbo ", gstk:to_ascii(Wth)]};
	{insertbg,    Color} ->
	    {c, [Editor, " conf -insertba ", gstk:to_color(Color)]};
	{insertpos,    Index} ->
	    {c, [Editor, " m s insert ", p_index(Index)]};
	{insert, {Index, Text}} ->
	    {c, [Editor, " ins ", p_index(Index), " ", gstk:to_ascii(Text)]};
	{del,      {From, To}} ->
	    {c, [Editor, " del ", p_index(From), " ", p_index(To)]};
	{overwrite, {Index, Text}} ->
	    AI = p_index(Index),
	    Len = gstk:to_ascii(lists:flatlength(Text)),
	    {c, [Editor, " del ",AI," \"",AI,"+",Len,"c\";",
		 Editor, " ins ",AI," ", gstk:to_ascii(Text)]};
	clear       -> {c, [Editor, " delete 1.0 end"]};
	{load,        File} ->
	    F2 = re:replace(File, [92,92], "/", [global,{return,list}]),
	    case gstk:call(["ed_load ", Editor, " ", gstk:to_ascii(F2)]) of
		{result,    _} -> none;
		{bad_result,Re} -> 
		    {error,{no_such_file,editor,load,F2,Re}}
	    end;
	{save, File} ->
	    F2 = re:replace(File, [92,92], "/", [global,{return,list}]),
	    case gstk:call(["ed_save ",Editor," ",gstk:to_ascii(F2)]) of
		{result,    _} -> none;
		{bad_result,Re} -> 
		    {error,{no_such_file,editor,save,F2,Re}}
	    end;
	{enable,      true} -> {c, [Editor, " conf -state normal"]};
	{enable,     false} -> {c, [Editor, " conf -state disabled"]};
	
	{setfocus,     true} -> {c, ["focus ", Editor]};
	{setfocus,    false} -> {c, ["focus ."]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GstkId,_MainW,DB,Editor) ->
    case Option of
	font -> gstk_db:opt(DB,GstkId,font,undefined);
	padx          -> tcl2erl:ret_atom([Editor," cg -padx"]);
	pady          -> tcl2erl:ret_atom([Editor," cg -pady"]);
	enable        -> tcl2erl:ret_enable([Editor," cg -st"]);
	fg            -> tcl2erl:ret_color([Editor," cg -fg"]);
	{fg, Pos} ->
	    L=tcl2erl:ret_list([Editor," tag nam ", p_index(Pos)]),
	    SO = GstkId#gstkid.widget_data,
	    case last_tag_val(undefined, $c, L, SO#so.misc) of
		undefined -> tcl2erl:ret_color([Editor," cg -fg"]);
		Color -> Color
	    end;
	{font_style, Pos} ->
	    L=tcl2erl:ret_list([Editor," tag nam ", p_index(Pos)]),
	    SO = GstkId#gstkid.widget_data,
	    case last_tag_val(undefined, $f, L, SO#so.misc) of
		undefined -> 'my style? nyi';
		Style -> Style
	    end;
	selection -> ret_ed_indexes([Editor," tag ne sel 1.0"]);
	char_height   -> tcl2erl:ret_int([Editor, " cg -he"]);
	char_width   -> tcl2erl:ret_int([Editor, " cg -wi"]);
	insertbg      -> tcl2erl:ret_color([Editor," cg -insertba"]);
	insertbw      -> tcl2erl:ret_int([Editor," cg -insertbo"]);
	insertpos     -> ret_ed_index([Editor, " ind insert"]);
	setfocus      -> tcl2erl:ret_focus(Editor, "focus");
	wrap          -> tcl2erl:ret_atom([Editor," cg -wrap"]);
	size          -> {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
			 MaxRow-1;
	vscrollpos       ->
	    {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
	    [Top,_Bot] = tcl2erl:ret_list([Editor," yvi"]),
	    round(Top*(MaxRow-1))+1;
	{get, {From, To}} ->
	    tcl2erl:ret_str([Editor, " get ", p_index(From), " ", p_index(To)]);
	_ -> {bad_result, {GstkId#gstkid.objtype, invalid_option, Option}}
    end.


%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

p_index({Line, lineend}) -> [$",gstk:to_ascii(Line), ".1 lineend",$"];
p_index({Line, Char}) -> [gstk:to_ascii(Line), $., gstk:to_ascii(Char)];
p_index(insert)       -> "insert";
p_index('end')        -> "end";
p_index(Idx)          -> gs:error("bad index in editor: ~w~n",[Idx]),0.

ret_ed_index(Cmd) ->
    case gstk:call(Cmd) of
	{result, Val} ->
	    case io_lib:fread("~d.~d", Val) of
		{ok, [Row,Col], []} -> {Row, Col};
		Other -> {bad_result, Other}
	    end;
	Bad_result -> Bad_result
    end.

ret_ed_indexes(Cmd) ->
    case gstk:call(Cmd) of
	{result, ""} -> undefined;
	{result, Val} ->
	    case io_lib:fread("~d.~d ~d.~d", Val) of
		{ok, [Row1,Col1,Row2,Col2], []} -> {{Row1, Col1}, {Row2,Col2}};
		Other -> {bad_result, Other}
	    end;
	Bad_result -> Bad_result
    end.


%%----------------------------------------------------------------------
%% Returns: {Tag text(), NewGstkId}
%%----------------------------------------------------------------------
%% The misc field of the so record is a list of {ColorNo, Color|Font|...}
get_color_tag(Editor,Color,Gstkid) ->
    SO = Gstkid#gstkid.widget_data,
    Tags = SO#so.misc,
    case lists:keysearch(Color, 2, Tags) of
%	{value, {No, _}} -> {["c",gstk:to_ascii(No)], Gstkid};
%	false -> % don't reuse tags, priority order spoils that
	_Any ->
	    {No,_} = lists:max(Tags),
	    N=No+1,
	    SO2 = SO#so{misc=[{N,Color}|Tags]},
	    TagStr=["c",gstk:to_ascii(N)],
	    gstk:exec([Editor," tag co ",TagStr," -for ", gstk:to_color(Color)]),
	    {TagStr,Gstkid#gstkid{widget_data=SO2}}
    end.

get_style_tag(DB,Editor,Style,Gstkid) ->
    SO = Gstkid#gstkid.widget_data,
    Tags = SO#so.misc,
    case lists:keysearch(Style, 2, Tags) of
%	{value, {No, _}} -> {["f",gstk:to_ascii(No)], Gstkid};
%	false -> % don't reuse tags, priority order spoils that
	_Any -> 
	    {No,_} = lists:max(Tags),
	    N=No+1,
	    SO2 = SO#so{misc=[{N,Style}|Tags]},
	    TagStr=["f",gstk:to_ascii(N)],
	    gstk:exec([Editor," tag co ",TagStr," -font ",
		      gstk_font:choose_ascii(DB,Style)]), % should be style only
	    {TagStr,Gstkid#gstkid{widget_data=SO2}}
    end.

%%----------------------------------------------------------------------
%% Purpose: Given a list of tags for a char, return its visible color
%% (that is that last color tag in the list).
%%----------------------------------------------------------------------
last_tag_val(TagVal, _Chr, [], _TagDict) -> TagVal;
last_tag_val(TagVal, Chr, [Tag|Ts],TagDict) ->
    case atom_to_list(Tag) of
	[Chr|ANo] ->
	    No = list_to_integer(ANo),
	    last_tag_val(gs:val(No, TagDict),Chr,Ts,TagDict);
	_NoAcolor ->
	    last_tag_val(TagVal,Chr, Ts,TagDict)
    end.
    
%%% ----- Done -----
