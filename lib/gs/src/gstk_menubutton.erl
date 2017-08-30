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
%% Basic Menubutton Type
%% ------------------------------------------------------------

-module(gstk_menubutton).

%%------------------------------------------------------------------------------
%% 			    MENUBUTTON OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	activefg		Color
%%	align			n,w,s,e,nw,se,ne,sw,center
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	disabledfg		Color
%%	fg			Color
%%      font                    Font
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	justify			left|right|center	(multiline text only)
%%	label			{text, String} | {image, BitmapFile}
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief	[flat|raised| sunken | ridge | groove]
%%	side			left | right	(valid only in menubars)
%%	underline		Int
%%	width			Int
%%	wraplength		Int
%%	x			Int	(not valid in menubars)
%%	y			Int	(not valid in menubars)
%%
%%  Commands:
%%	enable			Bool
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
%%	activate		?????? (kontra enable, true)
%%	state			??????
%%	cursor			??????
%%	image			??????
%%	focus			?????? (-takefocus)
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5,
	 mk_create_opts_for_child/4]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,GstkId),
    NGstkId=GstkId#gstkid{widget=TkW},
    PlacePreCmd = [";place ", TkW],
    case gstk_generic:make_command(Opts, NGstkId, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(["menubutton ", TkW," -padx 4 -pady 3",Cmd]),
	    NGstkId
    end.

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Opts) ->
    TkW = Gstkid#gstkid.widget,
    SimplePreCmd = [TkW, " conf"],
    PlacePreCmd = [";place ", TkW],
    gstk_generic:mk_cmd_and_exec(Opts,Gstkid,TkW,SimplePreCmd,PlacePreCmd,DB).

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
    gstk_generic:read_option(DB, Gstkid, Opt).


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
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gstkid   - The gstkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, TkW, DB,_) ->
    case Option of
	{anchor,        How} -> fix_anchor(How, Gstkid, TkW, DB);
	{disabledfg,  Color} -> {s, [" -disabledf ", gstk:to_color(Color)]};
	{height,     Height} -> {s, [" -he ", gstk:to_ascii(Height)]};
	{side,         Side} -> fix_side(Side, Gstkid, TkW, DB);
	{underline,     Int} -> {s, [" -und ", gstk:to_ascii(Int)]};
	{width,       Width} -> {s, [" -wi ", gstk:to_ascii(Width)]};
	{wraplength,    Int} -> {s, [" -wr ", gstk:to_ascii(Int)]};
	{x,               X} -> fix_placement(x, X, Gstkid, TkW, DB);
	{y,               Y} -> fix_placement(y, Y, Gstkid, TkW, DB);
	_                    -> invalid_option
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
read_option(Option,GstkId,TkW,_DB,_) ->
    case Option of
	anchor        -> tcl2erl:ret_place(anchor, TkW);
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledfo"]);
	height        -> tcl2erl:ret_int([TkW," cg -he"]);
	side          -> tcl2erl:ret_pack(side, TkW);
	underline     -> tcl2erl:ret_int([TkW," cg -underl"]);
	width         -> tcl2erl:ret_int([TkW," cg -wi"]);
	wraplength    -> tcl2erl:ret_int([TkW," cg -wr"]);
	x             -> tcl2erl:ret_place(x, TkW);
	y             -> tcl2erl:ret_place(y, TkW);
	_ -> {error,{invalid_option,Option, GstkId#gstkid.objtype}}
    end.

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

fix_placement(Attr, Value, Gstkid, _TkW, DB) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Gstkid#gstkid.parent),
    case Pgstkid#gstkid.objtype of
	menubar -> invalid_option;
	_       -> {p, [" -", atom_to_list(Attr), " ", gstk:to_ascii(Value)]}
    end.

	
fix_anchor(How, Gstkid, TkW, DB) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Gstkid#gstkid.parent),
    case Pgstkid#gstkid.objtype of
	menubar -> {c, ["pack ", TkW, " -an ", gstk:to_ascii(How)]};
	_       -> {p,   [" -anch ", gstk:to_ascii(How)]}
    end.


fix_side(Side, Gstkid, TkW, DB) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Gstkid#gstkid.parent),
    case Pgstkid#gstkid.objtype of
	menubar -> {c, ["pack ", TkW, " -fill y -si ", gstk:to_ascii(Side)]};
        _       -> none
    end.


%% ----- Done -----

