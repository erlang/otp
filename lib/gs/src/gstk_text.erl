%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%% Basic Text Type
%% ------------------------------------------------------------

-module(gstk_text).

%%-----------------------------------------------------------------------------
%% 			    TEXT OPTIONS
%%
%%  Attributes:
%%	anchor			n|w|e|s|nw|sw|ne|se|center
%%	coords			[{X,Y}]
%%	data			Data
%%	fg			Color
%%	font			Font
%%	justify			left | center | right
%%	stipple			Bool
%%	text			String
%%	width			Int	(line length in characters)
%%
%%  Commands:
%%	lower
%%	move			{Dx, Dy}
%%	raise
%%	scale			{Xo, Yo, Sx, Sy}
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
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
%%	fontfamily		?????? Family
%%	fontsize		?????? Size
%%	style			?????? [bold,italic]
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	 option/5,read_option/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%----------------------------------------------------------------------------
create(DB, Gstkid, Opts) ->
    case gstk_canvas:pickout_coords(Opts, [],text,1) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    Ngstkid=gstk_canvas:upd_gstkid(DB, Gstkid, Opts),
	    #gstkid{widget=CanvasTkW}=Ngstkid,
	    MCmd = [CanvasTkW, " create te ", Coords],
	    gstk_canvas:mk_cmd_and_call(NewOpts,Ngstkid,CanvasTkW, MCmd, DB)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Opts) ->
    gstk_canvas:item_config(DB, Gstkid, Opts).


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
    Item = Gstkid#gstkid.widget_data,
    gstk_generic:read_option(DB,Gstkid,Opt,[gstk:to_ascii(Item)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%
%% Return 	: TkWidget to destroy | {Parent, Objmod, Args}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gstkid) ->
    gstk_canvas:item_delete_impl(DB,Gstkid).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: DB	  - The Database
%%		  Canvas  - The canvas tk widget
%%		  Item    - The item number to destroy
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(_DB, Canvas, Item) ->
    gstk:exec([Canvas, " delete ", gstk:to_ascii(Item)]).


event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  MainW   - The main tk-widget
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, _Canvas, DB, _AItem) ->
    case Option of
	{anchor,         How} -> {s, [" -anchor ", gstk:to_ascii(How)]};
	{fg,          Color} -> {s, [" -fi ", gstk:to_color(Color)]};
	{font, Font} when is_tuple(Font) ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    {s, [" -fo ", gstk_font:choose_ascii(DB,Font)]};
	{justify,       How} -> {s, [" -j ", gstk:to_ascii(How)]};
	{text,         Text} -> {s, [" -te ", gstk:to_ascii(Text)]};
	{width,       Width} -> {s, [" -w ", gstk:to_ascii(Width)]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gstkid, Canvas, DB, AItem) ->
    case Option of
	anchor   -> tcl2erl:ret_atom([Canvas, " itemcg ", AItem, " -anchor"]);
	fg       -> tcl2erl:ret_color([Canvas, " itemcg ", AItem, " -fi"]);
	font     -> gstk_db:opt(DB,Gstkid,font,undefined);
	justify  -> tcl2erl:ret_atom([Canvas, " itemcg ", AItem, " -j"]);
	stipple  -> tcl2erl:ret_stipple([Canvas," itemcg ",AItem," -stipple"]);
	text     -> tcl2erl:ret_str([Canvas, " itemcg ", AItem, " -te"]);
	width    -> tcl2erl:ret_int([Canvas, " itemcg ", AItem, " -w"]);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.


%% ----- Done -----
