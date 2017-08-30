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
%% Basic Menubar Type
%% ------------------------------------------------------------

-module(gstk_menubar).

%%------------------------------------------------------------------------------
%% 			   MENUBAR OPTIONS
%%
%%  Attributes:
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
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
%%	align			How
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
    MPreCmd = ["frame ", TkW],
    PlaceCmd = [";place ", TkW],
    Ngstkid = GstkId#gstkid{widget=TkW},
    case gstk_generic:make_command(Opts, Ngstkid,TkW, MPreCmd, PlaceCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec([Cmd,";pack ", TkW, " -side top -fill x;"]),
	    Ngstkid
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
    PlacePreCmd = ["place ", TkW],
    gstk_generic:mk_cmd_and_exec(Opts,Gstkid,TkW,SimplePreCmd,PlacePreCmd,DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: Opt     - An option to read
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

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) 
when Cgstkid#gstkid.objtype==menubutton ->
    case gstk_db:lookup_def(Pgstkid,menubutton,bg) of
	false ->
	    MbarTkW=Pgstkid#gstkid.widget,
	    Color=tcl2erl:ret_color([MbarTkW," cg -bg"]),
	    gstk_db:insert_def(Pgstkid,menubutton,{bg,Color});
	_ -> done
    end,
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).


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
option(Option,_Gstkid,_TkW,_DB,_) ->
    case Option of
	{bg,          Color} -> {s, [" -bg ", gstk:to_color(Color)]};
	{height,     Height} -> {s, [" -height ", gstk:to_ascii(Height)]};
	_                    -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GstkId,TkW,_DB,_) ->
    case Option of
	bg            -> tcl2erl:ret_color([TkW," cg -bg"]);
	height        -> tcl2erl:ret_int(["update idletasks;winfo he ",TkW]);
	_ -> {bad_result, {GstkId#gstkid.objtype, invalid_option, Option}}
    end.


%% ----- Done -----


