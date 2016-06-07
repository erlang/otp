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
%% Basic Window Type.
%% ------------------------------------------------------------

-module(gstk_window).
-compile([{nowarn_deprecated_function,{gs,destroy,1}}]).

%%------------------------------------------------------------------------------
%% 			    WINDOW OPTIONS
%%
%%  Attributes:
%%	x			Int
%%	y			Int
%%	width			Int
%%	height			Int
%%	bg			Color
%%	bw			Int
%%	relief			Relief  [flat|raised|sunken|ridge|groove]
%%	highlightbw		Int
%%	highlightbg		Color
%%	highlightfg		Color
%%	map			Bool
%%	iconify 		Bool
%%	title			String
%%	iconname		String	
%%      iconbitmap      	Bitmap
%%      iconmask        	Bitmap
%%	data			Data
%%      cursor                  arrow|busy|cross|hand|help|resize|text
%%
%%  Commands:
%%      raise			
%%      lower			
%%	setfocus		Bool
%%
%%  Events:
%%      configure		[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
%%	leave			[Bool | {Bool, Data}]
%%	motion			[Bool | {Bool, Data}]
%%	keypress		[Bool | {Bool, Data}]
%%	keyrelease		[Bool | {Bool, Data}]
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	focus			[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%
%%  Read options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	screen			?????????
%%	map			
%%	unmap			
%%	iconify
%%	deiconify
%%	focusmodel		[active|passive] (wm focusmodel)
%%

-export([create/3, config/3, read/3, delete/2, event/5,destroy_win/1]).
-export([option/5,read_option/5,mk_create_opts_for_child/4]).

-include("gstk.hrl").
% bind . <1> {puts "x: [expr %X - [winfo rootx .]] y: [expr %Y - [wi rooty .]]"}

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gstkid, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,Gstkid),
    NGstkid=Gstkid#gstkid{widget=TkW},
    case gstk_generic:make_command(transform_geometry_opts(Opts),
				  NGstkid, TkW, "", ";", DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    BindCmd = gstk_generic:bind(DB, Gstkid, TkW, configure, true),
%	    io:format("\nWINDOW1: ~p\n",[TkW]),
%	    io:format("\nWINDOW1: ~p\n",[Cmd]),
%	    io:format("\nWINDOW1: ~p\n",[BindCmd]),
	    gstk:exec(["toplevel ", TkW,Cmd,$;,BindCmd]),
	    NGstkid
    end.

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gstkid, Opts) ->
    TkW = Gstkid#gstkid.widget,
    SimplePreCmd = [TkW, " conf"],
    gstk_generic:mk_cmd_and_exec(transform_geometry_opts(Opts),
				Gstkid,TkW,SimplePreCmd,"",DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
event(DB, Gstkid, configure, Edata, Args) ->
    [W,H|_] = Args,
    gstk_db:insert_opt(DB,Gstkid,{width,W}),
    gstk_db:insert_opt(DB,Gstkid,{height,H}),
    case gstk_db:opt(DB,Gstkid,configure) of
	true ->
	    apply(gstk_generic,event,[DB,Gstkid,configure,Edata,Args]);
	false ->
	    ok
    end;
event(DB, Gstkid, destroy, Edata, Args) ->
    spawn(gstk_window,destroy_win,[gstk:make_extern_id(Gstkid#gstkid.id,DB)]),
    gstk_generic:event(DB, Gstkid, destroy, Edata, Args);
event(DB, Gstkid, Etype, Edata, Args) ->
    gstk_generic:event(DB, Gstkid, Etype, Edata, Args).

destroy_win(ID) ->
    gs:destroy(ID).
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
%-define(REGEXP,"regexp {(\\d+)x(\\d+)\\+?(-?\\d+)\\+?(-?\\d+)} ").
% FIXME: Is this ok? Always positive?
-define(REGEXP,"regexp {(\\d+)x(\\d+)\\+(\\d+)\\+(\\d+)} ").

option(Option, Gstkid, TkW, DB,_) ->
    case Option of
%% Bug in tcl/tk complicates setting of a single x,y,width,height.
	{x,               X} -> 
	    {c, 
	    [?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
	     " ${w}x$h",signed(X),"+$y;update idletasks"]};
	{y,               Y} -> 
	    {c,[?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
		" ${w}x$h+$x",signed(Y),"; update idletasks"]};
	{width,       Width} when Width >= 0 ->	% FIXME: Needed test?
	    case gstk_db:opt_or_not(DB,Gstkid,width) of
		{value,Width} -> none;
		_Q ->
		    gstk_db:insert_opt(DB,Gstkid,{width,Width}),
		    {c,[?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW," ",
			gstk:to_ascii(Width),"x$h+$x+$y;update idletasks"]}
	    end;
    	{height,     Height} when Height >= 0 -> % FIXME: Needed test?
	    case gstk_db:opt_or_not(DB,Gstkid,height) of
		{value,Height} -> none;
		_Q ->				% FIXME: Why different?
		    gstk_db:insert_opt(DB,Gstkid,{height,Height}),
		    {c,
		     ["wm ge ",TkW,
		      " [winfo w ", TkW, "]x",gstk:to_ascii(Height),
		      ";update idletasks"]}
	    end;
	{width_height, {W,H}} when W >= 0, H >= 0 ->
	    case {gstk_db:opt_or_not(DB,Gstkid,width),
		  gstk_db:opt_or_not(DB,Gstkid,height)} of
		{{value,W},{value,H}} ->
		    none;
		_OtherSize -> 
		    gstk_db:insert_opt(DB,Gstkid,{height,H}),
		    gstk_db:insert_opt(DB,Gstkid,{width,W}),
		    {c, ["update idletasks;wm ge ", TkW, " ",
			 gstk:to_ascii(W),"x",gstk:to_ascii(H),
			 ";update idletasks"]}
	    end;
	{xy,             {X,Y}} -> 
	    {c, [?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
		 " ${w}x$h", signed(X),signed(Y),
		 ";update idletasks"]};
	{bg,          Color} -> {s, [" -bg ", gstk:to_color(Color)]};
	{map,          true} -> {c, ["wm deiconify ", TkW]};
	{map,         false} -> {c, ["wm withdraw ", TkW]};
	{configure,      On} ->
	    gstk_db:insert_opt(DB,Gstkid,{configure,On}),
	    none;
	{iconify,      true} -> {c, ["wm iconify ", TkW]};
	{iconify,     false} -> {c, ["wm deiconify ", TkW]};
	{title,       Title} -> {c, ["wm title ", TkW, " " , 
					   gstk:to_ascii(Title)]};
	{iconname,     Name} -> {c, ["wm iconn ",TkW, " ",
					   gstk:to_ascii(Name)]};
	{iconbitmap, Bitmap} -> {c, ["wm iconb ",TkW, " ",
					   gstk:to_ascii(Bitmap)]};
	{iconmask,   Bitmap} -> {c, ["wm iconm ",TkW, " ",
					   gstk:to_ascii(Bitmap)]};
	raise		     -> {c, ["raise ", TkW]};
	lower		     -> {c, ["lower ", TkW]};
	{setfocus,     true} -> {c, ["focus ", TkW]};
	{setfocus,    false} -> {c, ["focus {}"]};
	{buttonpress,    On} ->
	    Eref = mk_eref(On, DB, Gstkid, buttonpress),
	    {c,["bind ",TkW," <ButtonPress> ",
	       event_onoff(["{erlsend ",Eref," %b ",xy_abs_str(TkW),"};"],On)]};
	{buttonrelease,  On} ->
	    Eref = mk_eref(On, DB, Gstkid, buttonrelease),
	    {c,["bind ",TkW," <ButtonRelease> ",
	       event_onoff(["{erlsend ",Eref," %b ",xy_abs_str(TkW),"};"],On)]};
	{motion,         On} ->
	    Eref = mk_eref(On, DB, Gstkid, motion),
	    {c,["bind ",TkW," <Motion> ",
	       event_onoff(["{erlsend ",Eref," ",xy_abs_str(TkW),"};"],On)]};
	_                    -> invalid_option
    end.

xy_abs_str(TkW) ->
    ["[expr %X-[winfo rootx ",TkW,"]] [expr %Y-[winfo rooty ",TkW,"]]"].

event_onoff(Str, true) -> Str;
event_onoff(_,false) -> "{}".

mk_eref(false, DB, Gstkid, Etype) ->
    gstk_db:delete_event(DB, Gstkid, Etype),
    dummy;
mk_eref(true,DB,Gstkid,Etype) ->
    gstk_db:insert_event(DB, Gstkid, Etype, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/3
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gstkid, TkW, DB,_) ->
    case Option of
	x             -> tcl2erl:ret_x(geo_str(TkW));
	y             -> tcl2erl:ret_y(geo_str(TkW));
	width         -> tcl2erl:ret_width(geo_str(TkW));
	height        -> tcl2erl:ret_height(geo_str(TkW));
	configure     -> gstk_db:opt(DB,Gstkid,configure);
	bg            -> tcl2erl:ret_color([TkW," cg -bg"]);
	map           -> tcl2erl:ret_mapped(["winfo is ", TkW]);
	iconify       -> tcl2erl:ret_iconified(["wm st ", TkW]);
	title         -> tcl2erl:ret_str(["wm ti ", TkW]);
	iconname      -> tcl2erl:ret_str(["wm iconn ", TkW]);
	iconbitmap    -> tcl2erl:ret_str(["wm iconb ", TkW]);
	iconmask      -> tcl2erl:ret_str(["wm iconm ", TkW]);
	setfocus      -> tcl2erl:ret_focus(TkW, "focus");
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

geo_str(TkW) ->
    ["update idletasks;",?REGEXP,"[wm geometry ", TkW,
     "] g w h x y;set tmp \"$w $h $x $y\""].



%%----------------------------------------------------------------------
%%			       PRIMITIVES
%%----------------------------------------------------------------------

%% Return {+,-}Int  to be used in a geometry option
signed(X) when X>=0 ->
    [$+,integer_to_list(X)];
signed(X) when X<0 ->
    integer_to_list(X).

%%----------------------------------------------------------------------
%% Purpose: tcl/tk: wm .window geo sets WxH+x+y at one time.
%%          flushing every time is expensive. Do (almost) as much as
%%          possible in one operation.
%%----------------------------------------------------------------------
transform_geometry_opts(Opts) ->
    {Geo,RestOpts} = collect_geo_opts(Opts,[],[]),
    Geo2 = make_atomic(lists:sort(Geo)),
    lists:append(Geo2,RestOpts).

make_atomic([{height,H},{width,W},{x,X},{y,Y}]) ->
    [{width_height,{W,H}},{xy,{X,Y}}];
make_atomic([{height,H},{width,W}|XY]) ->
    [{width_height,{W,H}}|XY];
make_atomic([WH,{x,X},{y,Y}]) ->
    [WH,{xy,{X,Y}}];
make_atomic(L) -> L.

%%----------------------------------------------------------------------
%% Returns: {(list of x,y,width,height options),list of other opts}
%%----------------------------------------------------------------------
collect_geo_opts([{x,X}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{x,X}|Geo],Rest);
collect_geo_opts([{y,Y}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{y,Y}|Geo],Rest);
collect_geo_opts([{height,H}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{height,H}|Geo],Rest);
collect_geo_opts([{width,W}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{width,W}|Geo],Rest);
collect_geo_opts([Opt|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,Geo,[Opt|Rest]);
collect_geo_opts([],Geo,Rest) -> {Geo,Rest}.
    
%%% ----- Done -----
