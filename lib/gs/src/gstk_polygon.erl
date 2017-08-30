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
%% Basic Polygon Type
%% ------------------------------------------------------------

-module(gstk_polygon).


%%-----------------------------------------------------------------------------
%% 			    POLYGON OPTIONS
%%
%%  Attributes:
%%	bw			Int
%%	coords			[{X1,Y1}, {X2,Y2} | {Xn,Yn}]
%%	data			Data
%%	fg			Color
%%	fill			Color
%%	smooth			Bool
%%	splinesteps		Int
%%	stipple			Bool
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

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	 option/5,read_option/5]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gstkid, Opts) ->
    case pickout_coords(Opts, []) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    Ngstkid=gstk_canvas:upd_gstkid(DB, Gstkid, Opts),
	    #gstkid{widget=CanvasTkW}=Ngstkid,
	    MCmd = [CanvasTkW, " create po ", Coords],
	    gstk_canvas:mk_cmd_and_call(NewOpts, Ngstkid,CanvasTkW, MCmd, DB)
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
%%		  Gstkid   - The gstkid of the widget
%%		  MainW   - The main tk-widget
%%		  Canvas  - The canvas tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, _Gstkid, _Canvas, _DB, _AItem) ->
    case Option of
	{fg,          Color} -> {s, [" -outline ", gstk:to_color(Color)]};
	{bw,            Int} -> {s, [" -w ", gstk:to_ascii(Int)]};
	{smooth,       Bool} -> {s, [" -sm ", gstk:to_ascii(Bool)]};
	{splinesteps,   Int} -> {s, [" -sp ", gstk:to_ascii(Int)]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gstkid, Canvas, _DB, AItem) ->
    case Option of
	bw            -> tcl2erl:ret_int([Canvas, " itemcg ", AItem, " -w"]);
	fg            ->
	    tcl2erl:ret_color([Canvas, " itemcg ", AItem, " -outline"]);
	smooth        -> tcl2erl:ret_bool([Canvas, " itemcg ", AItem, " -sm"]);
	splinesteps   -> tcl2erl:ret_int([Canvas, " itemcg ", AItem, " -sp"]);
	stipple       ->
	    tcl2erl:ret_stipple([Canvas," itemcg ",AItem," -stipple"]);

	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

pickout_coords([{coords,Coords} | Rest], Opts) when length(Coords) >= 2 ->
    case gstk_canvas:coords(Coords) of
	invalid ->
	    {error, "A polygon must have at least four coordinates"};
	RealCoords ->
	    {RealCoords, lists:append(Rest, Opts)}
    end;
pickout_coords([Opt | Rest], Opts) ->
    pickout_coords(Rest, [Opt|Opts]);
pickout_coords([], _Opts) ->
    {error, "A polygon must have at least four coordinates"}.
%% ----- Done -----

