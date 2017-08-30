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
%% Basic Radiobutton Type
%% ------------------------------------------------------------

-module(gstk_radiobutton).

%%------------------------------------------------------------------------------
%% 			    RADIOBUTTON OPTIONS
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
%%	enable			Bool
%%	fg			Color
%%	group			Atom
%%	groupid			Groupid
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	justify			left|right|center
%%	label			{text, String} | {image, BitmapFile}
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	selectbg		Color
%%	underline		Int
%%	value			Atom
%%	width			Int
%%	wraplength		Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	flash
%%	invoke			
%%	select			Bool
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
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
%%	cursor			??????
%%	focus			?????? (-takefocus)
%%	font			??????
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).

-include("gstk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,GstkId),
    {G, GID, V, NOpts} = fix_group_and_value(Opts, DB, GstkId#gstkid.owner),
    NGstkId=GstkId#gstkid{widget=TkW,widget_data={G, GID, V}},
    PlacePreCmd = [";place ", TkW],
    case gstk_generic:make_command(NOpts, NGstkId, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(["radiobutton ", TkW," -bo 2 -indi true ",Cmd]),
	    NGstkId
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
    TkW = Gstkid#gstkid.widget,
    {NOpts, NGstkid} = fix_group_and_value(Opts, DB, Gstkid#gstkid.owner, Gstkid),
    SimplePreCmd = [TkW, " conf"],
    PlacePreCmd = [";place ", TkW],
    gstk_generic:mk_cmd_and_exec(NOpts,NGstkid,TkW,SimplePreCmd,PlacePreCmd,DB).

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
    {_, Gid, _} = Gstkid#gstkid.widget_data,
    gstk_db:delete_bgrp(DB, Gid),
    Gstkid#gstkid.widget.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gstkid, Etype, Edata, Args) ->
    Arg2 = case Etype of
	      click ->
		  [Text, _Grp | Rest] = Args,
		  {G, _Gid, V} = Gstkid#gstkid.widget_data,
		  [Text, G, V | Rest];
	      _Other ->
		  Args
	  end,
    gstk_generic:event(DB, Gstkid, Etype, Edata, Arg2).



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
	{disabledfg,  Color} -> {s, [" -disabledforegr ", gstk:to_color(Color)]};
	{group,       Group} -> {s, [" -var ", gstk:to_ascii(Group)]};
	{selectbg,    Color} -> {s, [" -selectc ", gstk:to_color(Color)]};
	{underline,     Int} -> {s, [" -un ", gstk:to_ascii(Int)]};
	{value,           V} -> {s, [" -val ", gstk:to_ascii(V)]};
	{wraplength,    Int} -> {s, [" -wr ", gstk:to_ascii(Int)]};
	flash                -> {c, [TkW, " f;"]};
	invoke               -> {c, [TkW, " i;"]};
	{select,       true} -> {c, [TkW, " se;"]};
	{select,      false} -> {c, [TkW, " des;"]};
	{click,          On} -> cbind(DB, Gstkid, click, On);
	_                    -> invalid_option
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/4
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gstkid, TkW,DB,_) -> 
    case Option of
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledforegr"]);
	group         -> {G, _, _} = Gstkid#gstkid.widget_data, G;
	groupid       -> {_, Gid, _} = Gstkid#gstkid.widget_data, Gid;
	selectbg      -> tcl2erl:ret_color([TkW," cg -selectc"]);
	underline     -> tcl2erl:ret_int([TkW," cg -un"]);
	value         -> {_, _, V} = Gstkid#gstkid.widget_data, V;
	wraplength    -> tcl2erl:ret_int([TkW," cg -wr"]);

	select        ->
	    Cmd = ["list [set x [",TkW," cg -var];global $x;set $x] [",
		   TkW," cg -val]"],
	    case tcl2erl:ret_tuple(Cmd) of
		{X, X} -> true;
		_Other  -> false
	    end;

	click         -> gstk_db:is_inserted(DB, Gstkid, click);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

%% create version
fix_group_and_value(Opts, DB, Owner) ->
    {G, GID, V, NOpts} = fgav(Opts, erlNIL, erlNIL, erlNIL, []),
    RV = case V of
	     erlNIL -> list_to_atom(lists:concat([v,gstk_db:counter(DB,value)]));
	     Other0 -> Other0
	 end,
    NG = case G of
	       erlNIL -> rb;
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {rbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gstk_db:insert_bgrp(DB, RGID),
    {NG, RGID, RV, [{group, RG}, {value, RV} | NOpts]}.
    
%% config version
fix_group_and_value(Opts, DB, Owner, Gstkid) ->
    {RG, RGID, RV} = Gstkid#gstkid.widget_data,
    {G, GID, V, NOpts} = fgav(Opts, RG, RGID, RV, []),
    case {G, GID, V} of
	{RG, RGID, RV} ->
	    {NOpts, Gstkid};
	{NG, RGID, RV} ->
	    NGID = {rbgrp, NG, Owner},
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={NG,NGID,RV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid};
	{RG, RGID, NRV} ->
	    NGstkid = Gstkid#gstkid{widget_data={RG,RGID,NRV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{value,NRV} | NOpts], NGstkid};
	{_, NGID, RV} when NGID =/= RGID ->
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={RG,NGID,RV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG} | NOpts], NGstkid};
	{_, NGID, NRV} when NGID =/= RGID ->
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={RG,NGID,NRV}},
	    gstk_db:insert_widget(DB, NGstkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGstkid};
	{NG, RGID, NRV} ->
	    NGID = {rbgrp, NG, Owner},
	    gstk_db:delete_bgrp(DB, RGID),
	    NRG = gstk_db:insert_bgrp(DB, NGID),
	    NGstkid = Gstkid#gstkid{widget_data={NG,NGID,NRV}},
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

%%
%% Config bind
%%
cbind(DB, Gstkid, Etype, On) ->
    TkW = Gstkid#gstkid.widget,
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
		  [" -command {erlsend ", Eref,
		   " \\\"[", TkW, " cg -text]\\\" [", TkW, " cg -var]}"];
	      true ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, ""),
		  [" -command {erlsend ", Eref,
		   " \\\"[", TkW, " cg -text]\\\" [", TkW, " cg -var]}"];
	      _Other ->
		  gstk_db:delete_event(DB, Gstkid, Etype),
		  " -command {}"
	  end,
    {s, Cmd}.

%% ----- Done -----

