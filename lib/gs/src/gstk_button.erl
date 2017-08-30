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
%% Basic Button Type
%% ------------------------------------------------------------

-module(gstk_button).

%%------------------------------------------------------------------------------
%% 			    BUTTON OPTIONS
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
%%	justify			left|right|center
%%	label			{text, String} | {image, BitmapFile}
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief  [flat|raised|sunken|ridge|groove]
%%	underline		Int
%%	width			Int
%%	wraplength		Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	enable			Bool
%%	flash			
%%	invoke			
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
%%	font			??????
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).

-include("gstk.hrl").

%%---------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%---------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,GstkId),
    NGstkId=GstkId#gstkid{widget=TkW},
    PlacePreCmd = [";place ", TkW],
    case gstk_generic:make_command(Opts,NGstkId,TkW,"",PlacePreCmd,DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(["button ", TkW," -rel raised -bo 2 ",Cmd]),
	    NGstkId
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
config(DB, Gstkid, Opts) ->
    TkW = Gstkid#gstkid.widget,
    SimplePreCmd = [TkW, " conf"],
    gstk_generic:mk_cmd_and_exec(Opts,Gstkid,SimplePreCmd,DB).

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
	{bitmap,     Bitmap} -> {s, [" -bi @", Bitmap]};
	{disabledfg,  Color} -> {s, [" -disabledf ", gstk:to_color(Color)]};
	{underline,     Int} -> {s, [" -un ", gstk:to_ascii(Int)]};
	{wraplength,    Int} -> {s, [" -wr ", gstk:to_ascii(Int)]};
	invoke               -> {c, [TkW, " i;"]};
	flash                -> {c, [TkW, " f;"]};
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
	disabledfg    -> tcl2erl:ret_color([TkW, " cg -disabledf"]);
	underline     -> tcl2erl:ret_int([TkW, " cg -un"]);
	wraplength    -> tcl2erl:ret_int([TkW, " cg -wr"]);

	click         -> gstk_db:is_inserted(DB, Gstkid, click);

	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

%%
%% Config bind
%%
cbind(DB, Gstkid, Etype, On) ->
    TkW = Gstkid#gstkid.widget,
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
		  [" -command {erlsend ", Eref, " \\\"[", TkW, " cg -text]\\\"}"];
	      true ->
		  Eref = gstk_db:insert_event(DB, Gstkid, Etype, ""),
		  [" -command {erlsend ", Eref, " \\\"[", TkW, " cg -text]\\\"}"];
	      _Other ->
		  gstk_db:delete_event(DB, Gstkid, Etype),
		  " -command {}"
	  end,
    {s, Cmd}.

%% ----- Done -----

