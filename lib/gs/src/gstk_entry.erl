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
%% Basic Entry Type
%% ------------------------------------------------------------

-module(gstk_entry).
-compile([{nowarn_deprecated_function,{gs,error,2}}]).

%%------------------------------------------------------------------------------
%% 			    ENTRY OPTIONS
%%
%%  Attributes:
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	fg			Color
%%      font                    Font
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int	(Pixels)
%%	highlightfg		Color
%%	insertbg		Color
%%	insertbw		Int	(0 or 1 Pixels ???)
%%	justify			left|right|center
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	selectbg		Color
%%	selectbw		Int	(Pixels)
%%	selectfg		Color
%%	text			String
%%	width			Int
%%	x			Int
%%	xselection		Bool
%%	y			Int
%%
%%  Commands:
%%	delete			Index | {From, To}
%%	enable			Bool
%%	insert			{index,String}
%%	select			{From, To} | clear
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
%%  Read options:
%%	children
%%	id
%%	index			Index	   => Int
%%	parent
%%	type
%%
%%
%%  Not Implemented:
%%	cursor			??????
%%	focus			?????? (-takefocus)
%%	font			??????
%%	hscroll			??????
%%	show			??????
%%	state			??????
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).

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
    PlacePreCmd = [";place ", TkW],
    Ngstkid = GstkId#gstkid{widget=TkW},
    case gstk_generic:make_command(Opts,Ngstkid,TkW,"", PlacePreCmd,DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    case gstk:call(["entry ", TkW,Cmd]) of
		{result, _} ->
		    gstk:exec(
		      [TkW," conf -bo 2 -relief sunken -highlightth 2;"]),
		    Ngstkid;
		Bad_Result ->
		    {error, Bad_Result}
	    end
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
    SimplePreCmd = [TkW, " conf"],
    PlacePreCmd = [";place ", TkW],
    gstk_generic:mk_cmd_and_exec(Opts,Gstkid,TkW,SimplePreCmd,PlacePreCmd,DB).

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
	{font,    Font} ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    {s, [" -font ", gstk_font:choose_ascii(DB,Font)]};
	{insertbg,    Color} -> {s, [" -insertba ", gstk:to_color(Color)]};
	{insertbw,    Width} -> {s, [" -insertbo ", gstk:to_ascii(Width)]};
	{justify,       How} -> {s, [" -ju ", gstk:to_ascii(How)]};
	{text,          Str} ->
	    {c, [TkW," del 0 end; ",TkW," ins 0 ", gstk:to_ascii(Str)]};
	{xselection,   Bool} -> {s, [" -exportse ", gstk:to_ascii(Bool)]};

	{delete, {From, To}} ->
	    {c, [TkW, " del ", p_index(From), $ , p_index(To)]};
	{delete,      Index} -> {c, [TkW, " de ", p_index(Index)]};
	{insert, {Idx, Str}} ->
	    {c, [TkW, " ins ", gstk:to_ascii(Idx),$ , gstk:to_ascii(Str)]};
	{select,      clear} -> {c, [TkW, " sel clear"]};
	{select, {From, To}} ->
	    {c, [TkW, " sel range ", p_index(From), $ , p_index(To)]};
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gstkid,TkW,DB,_) -> 
    case Option of
	insertbg      -> tcl2erl:ret_color([TkW," cg -insertba"]);
	insertbw      -> tcl2erl:ret_int([TkW," cg -insertbo"]);
	font -> gstk_db:opt(DB,Gstkid,font,undefined);
	justify       -> tcl2erl:ret_atom([TkW," cg -jus"]);
	text          -> tcl2erl:ret_str([TkW," get"]);
	xselection    -> tcl2erl:ret_bool([TkW," cg -exports"]);
	{index, Idx}  -> tcl2erl:ret_int([TkW, "cg ind ", p_index(Idx)]);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------
p_index(Index) when is_integer(Index) -> gstk:to_ascii(Index);
p_index(insert) -> "insert";
p_index(last)   -> "end";
p_index(Idx)    -> gs:error("Bad index in entry: ~w~n",[Idx]),0.


%%% ----- Done -----
