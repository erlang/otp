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
%% Basic Image Type
%% ------------------------------------------------------------

-module(gstk_image).
-compile([{nowarn_deprecated_function,{gs,pair,2}}]).

%%-----------------------------------------------------------------------------
%% 			    BITMAP OPTIONS
%%
%%  Attributes:
%%	anchor			n|w|e|s|nw|sw|ne|se|center
%%	bg			Color
%%	bitmap			String
%%	coords			[{X,Y}]
%%	data			Data
%%	fg			Color
%%
%% Attributes for gifs only:
%%      pix_val                 {{X,Y},Color}|{{{X1,Y1},{X2,Y2}},Color]
%%      save                    String
%%      refresh
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
%%      pix_val                 {X,Y}
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	 option/5,read_option/5]).

-include("gstk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Objmod  - An atom, this module
%%		  Objtype - An atom, the logical widget type
%%		  Owner   - Pid of the creator
%%		  Name    - An atom naming the widget
%%		  Parent  - Gsid of the parent
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gstkid, Opts) ->
    case pickout_type(Opts) of
	bitmap ->
	    create(bitmap,DB, Gstkid, Opts);
	_gif ->  %%Default gif
	    create(gif,DB, Gstkid, Opts)
    end.

create(gif,DB, Gstkid, Opts) ->
    case pickout_coords(Opts, []) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    CCmd = "image create photo",
	    case tcl2erl:ret_atom(CCmd) of
		Photo_item when is_atom(Photo_item) ->
		    #gstkid{parent=Parent,owner=Owner,id=Id}=Gstkid,
		    Pgstkid = gstk_db:lookup_gstkid(DB, Parent, Owner),
		    SO = Pgstkid#gstkid.widget_data,
		    CanvasTkW = SO#so.object,
		    Photo_item_s = atom_to_list(Photo_item),
		    gstk_db:insert_opt(DB,Id,gs:pair(coords,Opts)),
		    Ngstkid=Gstkid#gstkid{widget=CanvasTkW,
				       widget_data={Photo_item_s,unknown}},
		    gstk_db:update_widget(DB,Ngstkid),
		    MCmd = [CanvasTkW," create image ",Coords," -image ", 
			   Photo_item_s," -anchor nw"],
		    case gstk_canvas:make_command(NewOpts, Ngstkid,
						 CanvasTkW, MCmd, DB) of
			{error,Reason} -> {error,Reason};
			Cmd when is_list(Cmd) ->
			    case tcl2erl:ret_int(Cmd) of
				Item when is_integer(Item) ->
				    %% buu, not nice
				    G2 = gstk_db:lookup_gstkid(DB,Id),
				    NewWidget = {Photo_item_s,Item},
				    NewGstkid = G2#gstkid{widget_data=NewWidget},
				    gstk_db:insert_widget(DB, NewGstkid),
				    NewGstkid;
				Bad_result ->
				    {error,Bad_result}
			    end
		    end;
		Bad_result ->
		    {error,Bad_result}
	    end
    end;

create(bitmap,DB, Gstkid, Opts) ->
    case pickout_coords(Opts, []) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    #gstkid{parent=Parent,owner=Owner,id=Id}=Gstkid,
	    Pgstkid = gstk_db:lookup_gstkid(DB, Parent, Owner),
	    SO = Pgstkid#gstkid.widget_data,
	    CanvasTkW = SO#so.object,
	    gstk_db:insert_opt(DB,Id,gs:pair(coords,Opts)),
	    Ngstkid=Gstkid#gstkid{widget=CanvasTkW, widget_data=no_item},
	    gstk_db:update_widget(DB,Ngstkid),
	    MCmd = [CanvasTkW," create bi ", Coords],
	    gstk_canvas:mk_cmd_and_call(NewOpts,Ngstkid, CanvasTkW, MCmd,DB)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Opts) ->
    {Canvas, Item} = get_widget(Gstkid),
    AItem = gstk:to_ascii(Item),
    SCmd = [Canvas, " itemconf ", AItem],
    gstk_canvas:mk_cmd_and_exec(Opts, Gstkid, Canvas, AItem, SCmd, DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(DB, Gstkid, Opt) ->
    {_, Item} = get_widget(Gstkid),
    gstk_generic:read_option(DB,Gstkid,Opt,[gstk:to_ascii(Item)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%
%% Return 	: TkWidget to destroy | {Parent, Objmod, Args}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gstkid) ->
    gstk_db:delete_widget(DB, Gstkid),
    #gstkid{parent=P,id=ID}=Gstkid,
    {Canvas, Item} = get_widget(Gstkid),
    {P, ID, gstk_image, [Canvas, Item]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: DB	  - The Database
%%		  Canvas  - The canvas tk widget
%%		  Item    - The item number to destroy
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(_DB, Canvas, Item) ->
    gstk:exec([Canvas, " delete ", gstk:to_ascii(Item)]).


event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).

%%------------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gstkid   - The gstkid of the widget
%%		  MainW   - The main tk-widget
%%		  Canvas  - The canvas tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, _Canvas, _DB, _AItem) ->
    case Option of
	{bitmap,     Bitmap} ->
	    BF = re:replace(Bitmap, [92,92], "/", [global,{return,list}]),
	    {s, [" -bi @", BF]};
	{load_gif,       File} -> 
	    F2 = re:replace(File, [92,92], "/", [global,{return,list}]),
	    {Photo_item, _item} = Gstkid#gstkid.widget_data,
	    {c,[Photo_item, " configure -file ", gstk:to_ascii(F2)]};
	{pix_val,  {Coords,Color}} ->
	    {Photo_item, _item} = Gstkid#gstkid.widget_data,
	    {c, [Photo_item, " put ", gstk:to_color(Color), " -to ", 
		 coords(Coords)]};
	{save_gif, Name} ->
	    {Photo_item, _item} = Gstkid#gstkid.widget_data,
	    {c, [Photo_item, " write ", gstk:to_ascii(Name)]};
	{fg,          Color} -> {s, [" -fo ", gstk:to_color(Color)]};
	{bg,          Color} -> {s, [" -ba ", gstk:to_color(Color)]};
	{anchor,         How} -> {s, [" -anchor ", gstk:to_ascii(How)]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gstkid, Canvas, _DB, AItem) ->
    case Option of
	anchor   -> tcl2erl:ret_atom([Canvas," itemcget ",AItem," -anchor"]);
	bg       -> tcl2erl:ret_color([Canvas, " itemcget ", AItem, " -ba"]);
	bitmap   -> tcl2erl:ret_file([Canvas, " itemcget ", AItem, " -bi"]);
	fg       -> tcl2erl:ret_color([Canvas, " itemcget ", AItem, " -fo"]);
	{pix_val,{X,Y}} ->
	    {Photo_item, _item} = Gstkid#gstkid.widget_data,
	    ret_photo_color([Photo_item," get ",coords({X,Y})]);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

ret_photo_color(Cmd) ->
    case gstk:call(Cmd) of
	{result,Str} ->
	    {ok, [R,G,B],[]} = io_lib:fread("~d ~d ~d", Str),
	    {R,G,B};
	Bad_result -> Bad_result
    end.


%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------
get_widget(#gstkid{widget=Canvas,widget_data={_Photo_item,Item}}) ->
    {Canvas,Item};
get_widget(#gstkid{widget=Canvas,widget_data=Item}) ->
    {Canvas,Item}.

pickout_coords([{coords,Coords} | Rest], Opts) when length(Coords) == 1 ->
    case coords(Coords) of
	invalid ->
	    {error, "An image must have two coordinates"};
	RealCoords ->
	    {RealCoords, lists:append(Rest, Opts)}
    end;
pickout_coords([Opt | Rest], Opts) ->
    pickout_coords(Rest, [Opt|Opts]);
pickout_coords([], _Opts) ->
    {error, "An image must have two coordinates"}.

coords({X,Y}) when is_number(X),is_number(Y) ->
    [gstk:to_ascii(X), " ", gstk:to_ascii(Y), " "];
coords([{X,Y} | R]) when is_number(X),is_number(Y) ->
    [gstk:to_ascii(X), " ", gstk:to_ascii(Y), " ", coords(R)];
coords({{X1,Y1},{X2,Y2}}) when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2) ->
    [gstk:to_ascii(X1), " ", gstk:to_ascii(Y1)," ",
     gstk:to_ascii(X2), " ", gstk:to_ascii(Y2)];
coords([_]) -> %% not a pair
    invalid;
coords([]) ->
    [].


pickout_type([{bitmap,_Str}|_Options]) ->
    bitmap;
pickout_type([{gif,_Str}|_Options])  ->
    gif;
pickout_type([]) ->
    none;
pickout_type([_|Tail]) ->
    pickout_type(Tail).

%% ----- Done -----

