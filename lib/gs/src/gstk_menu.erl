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
%%-----------------------------------------------------------------------------
%%                         BASIC MENU TYPE
%%------------------------------------------------------------------------------

-module(gstk_menu).
-compile([{nowarn_deprecated_function,{gs,error,2}}]).

%%------------------------------------------------------------------------------
%% 			    MENU OPTIONS
%%
%%  Attribute:
%%	activebg		Color
%%	activebw		Int
%%	activefg		Color
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	disabledfg		Color
%%	fg			Color
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	selectcolor		Color
%%
%%  Commands:
%%	setfocus		[Bool | {Bool, Data}]
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
%%	post			{X,Y}
%%	unpost
%%	align			n,w,s,e,nw,se,ne,sw,center
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	cursor			??????
%%	focus			?????? (-takefocus)
%%	height			Int
%%	justify			left|right|center	(multiline text only)
%%	width			Int
%%	x			Int	(valid only for popup menus)
%%	y			Int	(valid only for popup menus)
%%

-export([create/3, config/3, read/3, delete/2, event/5,option/5,read_option/5]).
-export([delete_menuitem/3, insert_menuitem/4, lookup_menuitem_pos/3,
	 mk_create_opts_for_child/4]).

-include("gstk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    #gstkid{parent=Parent,owner=Owner,objtype=Objtype}=GstkId,
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent, Owner),
    Oref = gstk_db:counter(DB, Objtype),
    PF = gstk_widgets:suffix(Objtype),
    case Pgstkid#gstkid.objtype of
	menuitem ->
	    PMenu = Pgstkid#gstkid.parent,
	    PMgstkid = gstk_db:lookup_gstkid(DB, PMenu, Owner),
	    PMW = PMgstkid#gstkid.widget,
	    Index = gstk_menu:lookup_menuitem_pos(DB, PMgstkid, Pgstkid#gstkid.id),
	    TkW = lists:concat([PMW, PF, Oref]),
	    Gstkid=GstkId#gstkid{widget=TkW, widget_data=[]},
	    MPreCmd = ["menu ", TkW, " -tearoff 0 -relief raised -bo 2"],
	    MPostCmd = [$;,PMW," entryco ",gstk:to_ascii(Index)," -menu ",TkW],
	    case gstk_generic:make_command(Opts, Gstkid, TkW, "", "", DB) of
		{error,Reason} -> {error,Reason};
		Cmd when is_list(Cmd) ->
		    gstk:exec([MPreCmd,Cmd,MPostCmd]),
		    Gstkid
	    end;
	OtherParent ->
	    true = lists:member(OtherParent,
				%% grid+canvas har skumma coord system
				[menubutton,window,frame]),
	    PW = Pgstkid#gstkid.widget,
	    TkW = lists:concat([PW, PF, Oref]),
	    Gstkid=GstkId#gstkid{widget=TkW, widget_data=[]},
	    MPreCmd = ["menu ", TkW, " -tearoff 0 -relief raised -bo 2 "],
	    MPostCmd = if OtherParent == menubutton ->
			       [$;, PW, " conf -menu ", TkW];
			  true -> []
		       end,
	    case gstk_generic:make_command(Opts, Gstkid, TkW, "","", DB) of
		{error,Reason} -> {error,Reason};
		Cmd when is_list(Cmd) ->
		    gstk:exec([MPreCmd,Cmd,MPostCmd]),
		    Gstkid
	    end
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
config(DB, Gstkid, Opts) ->
    TkW = Gstkid#gstkid.widget,
    PreCmd = [TkW, " conf"],
    gstk_generic:mk_cmd_and_exec(Opts, Gstkid, TkW, PreCmd, "", DB).

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
    gstk_generic:read_option(DB, Gstkid, Opt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%
%% Return 	: TkWidget to destroy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gstkid) ->
    gstk_db:delete_widget(DB, Gstkid),
    Gstkid#gstkid.widget.

event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).

%%------------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gstkid   - The gstkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, TkW, DB,_) ->
    case Option of
	{activebw,      Int} -> {s, [" -activebo ", gstk:to_ascii(Int)]};
	{disabledfg,  Color} -> {s, [" -disabledf ", gstk:to_color(Color)]};
	{selectcolor, Color} -> {s, [" -selectc ", gstk:to_color(Color)]};
	{post_at,     {X,Y}} -> post_at(X,Y,Gstkid,TkW,DB);
	_                    -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gstkid, TkW, _DB, _AItem) ->
    case Option of
	activebw      -> tcl2erl:ret_int([TkW," cg -activebo"]);
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledfo"]);
	selectcolor   -> tcl2erl:ret_color([TkW," cg -selectc"]);
	_ -> {error,{invalid_option,Option, Gstkid#gstkid.objtype}}
    end.

post_at(X,Y,Gstkid,TkW,DB) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Gstkid#gstkid.parent),
    PtkW = Pgstkid#gstkid.widget,
    RootX = tcl2erl:ret_int(["winfo rootx ",PtkW]),
    RootY = tcl2erl:ret_int(["winfo rooty ",PtkW]),
    {c,[" tk_popup ",TkW," ",gstk:to_ascii(RootX+X)," ",gstk:to_ascii(RootY+Y)]}.


%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------
%%----------------------------------------------------------------------
%% gstk_db functions for menuitem handling
%% Tk menuitems are numbered from 0, thus we have to recalc the position.
%%----------------------------------------------------------------------
insert_menuitem(DB, MenuId, ItemId, Pos) ->
    Mgstkid = gstk_db:lookup_gstkid(DB, MenuId),
    Items = Mgstkid#gstkid.widget_data,
    NewItems = insert_at(ItemId, Pos+1, Items),
    gstk_db:update_widget(DB, Mgstkid#gstkid{widget_data=NewItems}).


delete_menuitem(DB, MenuId, ItemId) ->
    Mgstkid = gstk_db:lookup_gstkid(DB, MenuId),
    Items = Mgstkid#gstkid.widget_data,
    NewItems = lists:delete(ItemId, Items),
    gstk_db:insert_widget(DB, Mgstkid#gstkid{widget_data=NewItems}).


lookup_menuitem_pos(_DB, Mgstkid, ItemId) ->
    Items = Mgstkid#gstkid.widget_data,
    find_pos(ItemId, Items) - 1.

%%----------------------------------------------------------------------
%% Generic list processing
%%----------------------------------------------------------------------
find_pos(ItemId, Items) ->
    find_pos(ItemId, Items, 1).

find_pos(_ItemId, [], _N) -> gs:error("Couldn't find item in menu~n", []);
find_pos(ItemId, [ItemId|_Items], N) -> N;
find_pos(ItemId, [_|Items], N) ->
    find_pos(ItemId, Items, N + 1).

insert_at(Elem, 1, L) -> [Elem | L];
insert_at(Elem, N, [H|T]) ->
    [H|insert_at(Elem, N-1, T)].

%% ----- Done -----
