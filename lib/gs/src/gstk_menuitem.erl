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
%% Basic Menuitem Type
%% ------------------------------------------------------------

-module(gstk_menuitem).
-compile([{nowarn_deprecated_function,{gs,error,2}}]).

%%-----------------------------------------------------------------------------
%% 			    MENUITEM OPTIONS
%%
%%  Attribute:
%%	accelerator		String
%%	activebg		Color
%%	activefg		Color
%%	bg			Color
%%	color			Color	(same as fg)
%%	data			Data
%%	fg			Color
%%      font                    Font
%%	group			Atom	(valid only for radio type)
%%	index			Int
%%	itemtype		normal|check|radio|separator|cascade (|tearoff)
%%	label			{text, String} | {image, BitmapFile}
%%	menu			Menu	(valid only for cascade type)
%%	selectbg		Color
%%	underline		Int
%%	value			Atom
%%
%%  Commands:
%%	activate
%%	enable			Bool
%%	invoke
%%
%%  Events:
%%	click			[Bool | {Bool, Data}]
%%
%%  Read Options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	font			Font
%%	read menu on cascades
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	option/5,read_option/5,mk_create_opts_for_child/4]).
-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    #gstkid{parent=Parent,owner=Owner,id=Id}=GstkId,
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent),
    TkMenu = Pgstkid#gstkid.widget,
    Widget = "",
    {Index, Type, Options} = parse_opts(Opts, TkMenu),
    PreCmd = [TkMenu, " insert ", gstk:to_ascii(Index)],
    InsertArgs = [DB, Parent,Id, Index],
    case Type of
	check ->
	    {G, GID, NOpts} = fix_group(Options, DB, Owner),
	    TypeCmd = " ch",
	    Ngstkid=GstkId#gstkid{widget=Widget,widget_data={Type, G, GID}},
	    GenArgs = [NOpts,Ngstkid,TkMenu,"","",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngstkid);
	radio ->
	    {G, GID, V, NOpts} = fix_group_and_value(Options, DB, Owner),
	    Ngstkid=GstkId#gstkid{widget=Widget, widget_data={Type,G,GID,V}},
	    TypeCmd = " ra",
	    GenArgs = [NOpts,Ngstkid,TkMenu,"", "",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngstkid);
	_ ->
	    Ngstkid=GstkId#gstkid{widget=Widget, widget_data=Type},
	    TypeCmd = case Type of
			  normal    -> " co";
			  separator -> " se";
			  cascade   -> " ca"
		      end,
	    GenArgs = [Options,Ngstkid,TkMenu,"","",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngstkid)
    end.

mk_it(GenArgs,CallArgs,InsertArgs,Ngstkid) ->
    case apply(gstk_generic,make_command,GenArgs) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    case apply(gstk,call,[[CallArgs|Cmd]]) of
		{result,_} ->
		    apply(gstk_menu,insert_menuitem,InsertArgs),
		    Ngstkid;
		Bad_Result -> {error,Bad_Result}
	    end
    end.

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Options - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FIXME: Could we really trust Index? If we create a menu and put one
% entry in the middle of the meny, don't the entrys after that one
% renumber?

config(DB, Gstkid, Options) ->
    Parent = Gstkid#gstkid.parent,
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent),
    TkMenu = Pgstkid#gstkid.widget,
    case Gstkid#gstkid.widget_data of
	{Type, _, _, _} ->
	    Owner = Gstkid#gstkid.owner,
	    {NOpts, NGstkid} = fix_group_and_value(Options, DB, Owner, Gstkid),
	    Index = gstk_menu:lookup_menuitem_pos(DB, Pgstkid, NGstkid#gstkid.id),
	    PreCmd = [TkMenu, " entryco ", gstk:to_ascii(Index)],
	    gstk_generic:mk_cmd_and_exec(NOpts,NGstkid,TkMenu,PreCmd,"",DB,
					{Type,Index});
	{Type, _, _} ->
	    Owner = Gstkid#gstkid.owner,
	    {NOpts, NGstkid} = fix_group(Options, DB, Owner, Gstkid),
	    Index = gstk_menu:lookup_menuitem_pos(DB, Pgstkid, NGstkid#gstkid.id),
	    PreCmd = [TkMenu, " entryco ", gstk:to_ascii(Index)],
	    gstk_generic:mk_cmd_and_exec(NOpts,NGstkid,TkMenu,PreCmd,"",DB,
					{Type,Index});
	Type ->
	    Index = gstk_menu:lookup_menuitem_pos(DB, Pgstkid, Gstkid#gstkid.id),
	    PreCmd = [TkMenu, " entryco ", gstk:to_ascii(Index)],
	    gstk_generic:mk_cmd_and_exec(Options,Gstkid,TkMenu,PreCmd,"",
					DB, {Type,Index})
    end.

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
    Parent = Gstkid#gstkid.parent,
    Id = Gstkid#gstkid.id,
    gstk_db:delete_widget(DB, Gstkid),
    case Gstkid#gstkid.widget_data of
	{radio, _, Gid, _} -> gstk_db:delete_bgrp(DB, Gid);
	{check, _, Gid}    -> gstk_db:delete_bgrp(DB, Gid);
	_Other              -> true
    end,
   {Parent, Id, gstk_menuitem, [Id, Parent]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: Menu    - The menu tk widget
%%		  Item    - The index of the menuitem to destroy
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(DB, Id, Parent) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent),
    PW = Pgstkid#gstkid.widget,    
    Idx = gstk_menu:lookup_menuitem_pos(DB, Pgstkid, Id),
    gstk_menu:delete_menuitem(DB, Parent, Id),
    gstk:exec([PW, " delete ", gstk:to_ascii(Idx)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gstkid, Etype, Edata, Args) ->
    Arg2 = 
	case Gstkid#gstkid.widget_data of
	    {radio, G, _GID, V} ->
		[_Grp, Text, Idx | Args1] = Args,
		[Text, Idx, G, V | Args1];
	    {check, G, _Gid} ->
		[Bool, Text, Idx | Args1] = Args,
		RBool = case Bool of
			    0 -> false;
			    1 -> true
			end,
		[Text, Idx, G, RBool | Args1];
	    _Other2 ->
		Args
	end,
    gstk_generic:event(DB, Gstkid, Etype, Edata, Arg2).



%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  TkW     - The  tk-widget
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option({click,true}, _Gstkid, _TkW, _DB, {separator,_Index}) ->
    none;  % workaround to be able to have {click,true} as default.
option(_Option, _Gstkid, _TkW, _DB, {separator,_Index}) ->
    invalid_option;

option({menu,{Menu,_RestOfExternalId}}, _Gstkid, _TkW, DB, {cascade,_Index}) ->
    Mgstkid = gstk_db:lookup_gstkid(DB, Menu),
    MenuW = Mgstkid#gstkid.widget,
    {s, [" -menu ", MenuW]};

option({select,false}, _Gstkid, TkW, _DB, {check,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gstk:to_ascii(Index),
	 " -var];global $x;set $x 0"]};
option({select,true}, _Gstkid, TkW, _DB, {check,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gstk:to_ascii(Index),
	 " -var];global $x;set $x 1"]};

option({value,Val}, _Gstkid, _TkW, _DB, {radio,_Index}) ->
    {s, [" -val ", gstk:to_ascii(Val)]};
option({select,false}, _Gstkid, TkW, _DB, {radio,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gstk:to_ascii(Index),
	 " -var];global $x;set $x {}"]};
option({select,true}, _Gstkid, TkW, _DB, {radio,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gstk:to_ascii(Index),
	 " -var]; set y [", TkW, " entrycg ", gstk:to_ascii(Index),
	 " -val]; global $x; set $x $y"]};

option(Option, Gstkid, TkW, DB, {Kind,Index}) ->
    case Option of
	activate  -> {c, [TkW, " act ", gstk:to_ascii(Index)]};
	invoke    -> {c, [TkW, " inv ", gstk:to_ascii(Index)]};
	{accelerator,   Acc} -> {s, [" -acc ", gstk:to_ascii(Acc)]};
	{click,          On} -> cbind(On, Gstkid, TkW, Index, Kind, DB);
	{font, Font} when is_tuple(Font) ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    {s, [" -font ", gstk_font:choose_ascii(DB,Font)]};
	{label, {image,Img}} -> {s, [" -bitm @", Img, " -lab {}"]};
	% FIXME: insert -command here.....
	% FIXME: how to get value from image entry???
	{label, {text,Text}} -> {s, [" -lab ",gstk:to_ascii(Text)," -bitm {}"]};
	{underline,     Int} -> {s, [" -underl ", gstk:to_ascii(Int)]};
        {activebg,    Color} -> {s, [" -activeba ", gstk:to_color(Color)]};
        {activefg,    Color} -> {s, [" -activefo ", gstk:to_color(Color)]};
        {bg,          Color} -> {s, [" -backg ", gstk:to_color(Color)]};
        {enable,       true} -> {s, " -st normal"};
        {enable,      false} -> {s, " -st disabled"};
        {fg,          Color} -> {s, [" -foreg ", gstk:to_color(Color)]};
	_Other -> 
	    case lists:member(Kind,[radio,check]) of
		true -> 
		    case Option of
			{group,Group} -> {s, [" -var ", gstk:to_ascii(Group)]};
			{selectbg,Col} -> {s,[" -selectc ",gstk:to_color(Col)]};
			_ -> invalid_option
		    end;
		_ -> invalid_option
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GstkId,_TkW,DB,_) ->
    ItemId = GstkId#gstkid.id,
    MenuId = GstkId#gstkid.parent,
    MenuGstkid = gstk_db:lookup_gstkid(DB, MenuId),
    MenuW = MenuGstkid#gstkid.widget,
    Idx = gstk_menu:lookup_menuitem_pos(DB, MenuGstkid, ItemId),
    PreCmd = [MenuW, " entrycg ", gstk:to_ascii(Idx)],
    case Option of
	accelerator   -> tcl2erl:ret_str([PreCmd, " -acc"]);
	activebg      -> tcl2erl:ret_color([PreCmd, " -activeba"]);
	activefg      -> tcl2erl:ret_color([PreCmd, " -activefo"]);
	bg            -> tcl2erl:ret_color([PreCmd, " -backg"]);
	fg            -> tcl2erl:ret_color([PreCmd, " -foreg"]);
	group         -> read_group(GstkId, Option);
	groupid       -> read_groupid(GstkId, Option);
	index         -> Idx;
	itemtype      -> case GstkId#gstkid.widget_data of
			     {Type, _, _, _} -> Type;
			     {Type, _, _} -> Type;
			     Type -> Type
			 end;
	enable        -> tcl2erl:ret_enable([PreCmd, " -st"]);
	font -> gstk_db:opt(DB,GstkId,font,undefined);
	label         -> tcl2erl:ret_label(["list [", PreCmd, " -lab] [",
					    PreCmd, " -bit]"]);
	selectbg      -> tcl2erl:ret_color([PreCmd, " -selectco"]);
	underline     -> tcl2erl:ret_int([PreCmd, " -underl"]);
	value         -> tcl2erl:ret_atom([PreCmd, " -val"]);
	select        -> read_select(MenuW, Idx, GstkId);
	click         -> gstk_db:is_inserted(DB, GstkId, click);
	_ -> {bad_result, {GstkId#gstkid.objtype, invalid_option, Option}}
    end.

read_group(Gstkid, Option) ->
    case Gstkid#gstkid.widget_data of
	{_, G, _, _} -> G;
	{_, G, _}    -> G;
	_Other -> {bad_result,{Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

read_groupid(Gstkid, Option) ->
    case Gstkid#gstkid.widget_data of
	{_, _, Gid, _} -> Gid;
	{_, _, Gid}    -> Gid;
	_Other -> {bad_result,{Gstkid#gstkid.objtype, invalid_option, Option}}
    end.




read_select(TkMenu, Idx, Gstkid) ->
    case Gstkid#gstkid.widget_data of
	{radio, _, _, _} ->
	    Cmd = ["list [set x [", TkMenu, " entrycg ", gstk:to_ascii(Idx),
		   " -var];global $x;set $x] [", TkMenu,
		   " entrycg ", gstk:to_ascii(Idx)," -val]"],
	    case tcl2erl:ret_tuple(Cmd) of
		{X, X} -> true;
		_Other  -> false
	    end;
	{check, _, _} ->
	    Cmd = ["set x [", TkMenu, " entrycg ", gstk:to_ascii(Idx),
		   " -var];global $x;set $x"],
	    tcl2erl:ret_bool(Cmd);
	_Other ->
	    {error,{invalid_option,menuitem,select}}
    end.



%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

%% create version
fix_group_and_value(Opts, DB, Owner) ->
    {G, GID, V, NOpts} = fgav(Opts, erlNIL, erlNIL, erlNIL, []),
    RV = case V of
	     erlNIL ->
		 list_to_atom(lists:concat([v,gstk_db:counter(DB,value)]));
	     Other0 -> Other0
	 end,
    NG = case G of
	       erlNIL -> mrb;
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {mrbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gstk_db:insert_bgrp(DB, RGID),
    {NG, RGID, RV, [{group, RG}, {value, RV} | NOpts]}.
    
%% config version
fix_group_and_value(Opts, DB, Owner, Gstkid) ->
    {Type, RG, RGID, RV} = Gstkid#gstkid.widget_data,
    {G, GID, V, NOpts} = fgav(Opts, RG, RGID, RV, []),
    case {G, GID, V} of
	{RG, RGID, RV} ->
	    {NOpts, Gstkid};
	{NG, RGID, RV} ->
	    NGID = {rbgrp, NG, Owner},
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,NG,NGID,RV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid};
	{RG, RGID, NRV} ->
	    NGstkid = Gstkid#gstkid{widget_data={Type,RG,RGID,NRV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{value,NRV} | NOpts], NGstkid};
	{_, NGID, RV} when NGID =/= RGID ->
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,RG,NGID,RV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid};
	{_, NGID, NRV} when NGID =/= RGID ->
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,RG,NGID,NRV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGstkid};
	{NG, RGID, NRV} ->
	    NGID = {rbgrp, NG, Owner},
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,NG,NGID,NRV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGstkid}
    end.



fgav([{group, G} | Opts], _, GID, V, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([{groupid, GID} | Opts], G, _, V, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([{value, V} | Opts], G, GID, _, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([Opt | Opts], G, GID, V, Nopts) ->
    fgav(Opts, G, GID, V, [Opt | Nopts]);

fgav([], Group, GID, Value, Opts) ->
    {Group, GID, Value, Opts}.


%% check button version
%% create version
fix_group(Opts, DB, Owner) ->
    {G, GID, NOpts} = fg(Opts, erlNIL, erlNIL, []),
    NG = case G of
	       erlNIL ->
		 Vref = gstk_db:counter(DB, variable),
		 list_to_atom(lists:flatten(["mcb", gstk:to_ascii(Vref)]));
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {mcbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gstk_db:insert_bgrp(DB, RGID),
    {NG, RGID, [{group, RG} | NOpts]}.
    
%% config version
fix_group(Opts, DB, Owner, Gstkid) ->
    {Type, RG, RGID} = Gstkid#gstkid.widget_data,
    {G, GID, NOpts} = fg(Opts, RG, RGID, []),
    case {G, GID} of
	{RG, RGID} ->
	    {NOpts, Gstkid};
	{NG, RGID} ->
	    NGID = {cbgrp, NG, Owner},
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,NG,NGID}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid};
	{_, NGID} when NGID =/= RGID ->
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={Type,RG,NGID}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid}
    end.



fg([{group, G} | Opts], _, GID, Nopts) ->
    fg(Opts, G, GID, Nopts);

fg([{groupid, GID} | Opts], G, _, Nopts) ->
    fg(Opts, G, GID, Nopts);

fg([Opt | Opts], G, GID, Nopts) ->
    fg(Opts, G, GID, [Opt | Nopts]);

fg([], Group, GID, Opts) ->
    {Group, GID, Opts}.



parse_opts(Opts, TkMenu) ->
    parse_opts(Opts, TkMenu, none, none, []).


parse_opts([Option | Rest], TkMenu, Idx, Type, Options) ->
    case Option of
	{index,    I} -> parse_opts(Rest, TkMenu, I, Type, Options);
	{itemtype, T} -> parse_opts(Rest, TkMenu, Idx, T, Options);
	_Other         -> parse_opts(Rest, TkMenu, Idx, Type,[Option | Options])
    end;
parse_opts([], TkMenu, Index, Type, Options) ->
    RealIdx =
	case Index of
	    Idx when is_integer(Idx) -> Idx;
	    last  -> find_last_index(TkMenu);
	    Other -> gs:error("Invalid index ~p~n",[Other])
	end,
    {RealIdx, Type, Options}.

find_last_index(TkMenu) ->
    case tcl2erl:ret_int([TkMenu, " index last"]) of
	Last when is_integer(Last) -> Last+1;
	none  -> 0;
	Other -> gs:error("Couldn't find index ~p~n",[Other])
    end.

cbind({true, Edata}, Gstkid, TkMenu, Index, Type, DB) ->
    Eref = gstk_db:insert_event(DB, Gstkid, click, Edata),
    IdxStr = gstk:to_ascii(Index),
    case Type of
	normal ->
	    Cmd = [" -command {erlsend ", Eref,
		   " \\\"[",TkMenu," entrycg ",IdxStr," -label]\\\" ",
		   IdxStr,"}"],
	    {s, Cmd};
	check ->
	    Cmd = [" -command {erlsend ", Eref,
		   " \[expr \$[", TkMenu, " entrycg ",IdxStr," -var]\] \\\"[",
		   TkMenu, " entrycg ",IdxStr," -label]\\\" ",IdxStr,"}"],
	    {s, Cmd};
	radio ->
	    Cmd = [" -command {erlsend ", Eref,
		   " [", TkMenu, " entrycg ",IdxStr," -var] \\\"[",
		   TkMenu, " entrycg ",IdxStr," -label]\\\" ",IdxStr,"}"],
	    {s, Cmd};
	_Other ->
	    none
    end;

cbind({false, _}, Gstkid, _TkMenu, _Index, _Type, DB) ->
    gstk_db:delete_event(DB, Gstkid, click),
    none;

cbind(On, Gstkid, TkMenu, Index, Type, DB) when is_atom(On) ->
    cbind({On, []}, Gstkid, TkMenu, Index, Type, DB).


%%% ----- Done -----

