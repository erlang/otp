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
%% Basic Scale Type
%% ------------------------------------------------------------

-module(gstk_scale).

%%-------------------------------------------------------------------------
%% 			    SCALE OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	fg			Color
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	orient			vertical | horizontal
%%	range			{From, To}
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%      showvalue               Bool
%%	text			String
%%	width			Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	enable			Bool
%%	pos			Int
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
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

-export([create/3,config/3,read/3,delete/2,event/5,
	 option/5,read_option/5]).

-include("gstk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,GstkId),
    PlacePreCmd = [";place ", TkW],
    Ngstkid = GstkId#gstkid{widget=TkW},
    case gstk_generic:make_command(Opts, Ngstkid, TkW,"", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(["scale ", TkW,Cmd,$;,TkW,
		      " conf -bo 2 -sliderrelief raised -highlightth 2"]),
	    Ngstkid
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%		  TkW     - The  tk-widget
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, TkW, DB,_) ->
    case Option of
	{activebg,    Color} -> {s, [" -activeb ", gstk:to_color(Color)]};
	{orient,        How} -> {s, [" -or ", gstk:to_ascii(How)]};
	{range,  {From, To}} -> {s, [" -fr ", gstk:to_ascii(From),
				     " -to ", gstk:to_ascii(To)]};
        {relief,     Relief} -> {s, [" -rel ", gstk:to_ascii(Relief)]};
        {bw,            Wth} -> {s, [" -bd ", gstk:to_ascii(Wth)]};
	{text,       String} -> {s, [" -la ",gstk:to_ascii(String)]};
	{showvalue,    Bool} -> {s, [" -showvalue ",gstk:to_ascii(Bool)]};
	{pos,           Pos} -> {c, [TkW, " set ", gstk:to_ascii(Pos)]};
	{click,          On} -> cbind(DB, Gstkid, click, On);
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
	activebg      -> tcl2erl:ret_color([TkW," cg -activeb"]);
	orient        -> tcl2erl:ret_atom([TkW," cg -ori"]);
	range         ->
	    tcl2erl:ret_tuple(["list [",TkW," cg -fr] [",TkW," cg -to]"]);
        bw            -> tcl2erl:ret_int([TkW," cg -bd"]);
        relief        -> tcl2erl:ret_atom([TkW, " cg -reli"]);
	text          -> tcl2erl:ret_str([TkW," cg -lab"]);
	showvalue     -> tcl2erl:ret_bool([TkW," cg -showvalue"]);
	pos           -> tcl2erl:ret_int([TkW," get"]);
	click         -> gstk_db:is_inserted(DB, Gstkid, click);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------


%%
%% Config bind
%%
cbind(DB, Gstkid, Etype, On) ->
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
		  [" -command {erlsend ", Eref, "}"];
	      true ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, ""),
		  [" -command {erlsend ", Eref, "}"];
	      _Other ->
		  gstk_db:delete_event(DB, Gstkid, Etype),
		  " -command {}"
	  end,
    {s, Cmd}.

%% ----- Done -----
