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
%% Basic Frame Type.
%% ------------------------------------------------------------

-module(gstk_frame).

%%-----------------------------------------------------------------------------
%% 			    FRAME OPTIONS
%%
%%  Attributes:
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	width			Int
%%	x			Int
%%	y			Int
%%      cursor                  arrow|busy|cross|hand|help|resize|text
%%
%%  Commands:
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
%%  Read Options:
%%	children
%%	id
%%	parent
%%	type
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5,
	mk_create_opts_for_child/4]).

-include("gstk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GstkId, Opts) ->
    TkW = gstk_generic:mk_tkw_child(DB,GstkId),
    NGstkid=GstkId#gstkid{widget=TkW},
    PlacePreCmd = [";place ", TkW],
    case gstk_generic:make_command(Opts, NGstkid, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(["frame ", TkW,
		      " -relief raised -bo 0",Cmd]),
	    NGstkid
    end.

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

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
    PlacePreCmd = [";place ", TkW],
    Opts2 = atomic_width_height(false,false,Opts),
    gstk_generic:mk_cmd_and_exec(Opts2,Gstkid,TkW,SimplePreCmd,PlacePreCmd,DB).

atomic_width_height(false,false,[]) ->
    [];
atomic_width_height(false,Width,[]) ->
    [{width,Width}];
atomic_width_height(Height,false,[]) ->
    [{height,Height}];
atomic_width_height(H,W,[]) ->
    [{width_height,{W,H}}];
atomic_width_height(_,W,[{height,H}|Opts]) ->
    atomic_width_height(H,W,Opts);
atomic_width_height(H,_,[{width,W}|Opts]) ->
    atomic_width_height(H,W,Opts);
atomic_width_height(H,W,[Opt|Opts]) ->
    [Opt|atomic_width_height(H,W,Opts)].

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
%%		  Gstkid   - The gstkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gstkid, _TkW, DB,_) ->
    case Option of
	{bg,          Color} -> {s, [" -bg ", gstk:to_color(Color)]};
	{packer_x, _Pack} ->
            gstk_db:insert_opt(DB,Gstkid,Option),
	    none;
	{packer_y, _Pack} ->
	    gstk_db:insert_opt(DB,Gstkid,Option),
	    none;
	{width, W} ->
	    execute_pack_cmds(DB,xpack(W,DB,Gstkid)),
	    {s,[" -wi ", gstk:to_ascii(W)]};
	{height, H} ->
	    execute_pack_cmds(DB,ypack(H,DB,Gstkid)),
	    {s,[" -he ", gstk:to_ascii(H)]};
	{width_height,{W,H}} ->
	    execute_pack_cmds(DB, merge_pack_cmds(xpack(W,DB,Gstkid),
						  ypack(H,DB,Gstkid))),
	    {s,[" -he ", gstk:to_ascii(H)," -wi ", gstk:to_ascii(W)]};
	_  -> invalid_option
    end.

xpack(W,DB,Gstkid) ->
    gstk_db:insert_opt(DB,Gstkid,{width,W}),
    case gstk_db:opt_or_not(DB,Gstkid,packer_x) of
	{value,Pack} when is_list(Pack) ->
	    ColSiz = gs_packer:pack(W,Pack),
	    pack_children(pack_x,x,width,DB,
			  gstk_db:lookup_kids(DB,Gstkid#gstkid.id),
			  ColSiz);
	_Else -> []
    end.

ypack(H,DB,Gstkid) ->
    gstk_db:insert_opt(DB,Gstkid,{height,H}),
    case gstk_db:opt_or_not(DB,Gstkid,packer_y) of
	{value,Pack} when is_list(Pack) ->
	    ColSiz = gs_packer:pack(H,Pack),
	    pack_children(pack_y,y,height,DB,
			  gstk_db:lookup_kids(DB,Gstkid#gstkid.id),
			  ColSiz);
	_Else -> []
    end.

merge_pack_cmds([{Id,Opts1}|Cmds1],[{Id,Opts2}|Cmds2]) ->
    [{Id,Opts1++Opts2}|merge_pack_cmds(Cmds1,Cmds2)];
merge_pack_cmds(L1,L2) ->
    L1++L2.

execute_pack_cmds(DB,[{Id,Opts}|Cmds]) ->
    gstk:config_impl(DB,Id,Opts),
    execute_pack_cmds(DB,Cmds);
execute_pack_cmds(_,[]) ->
    ok.

%%----------------------------------------------------------------------
%% Returns: list of {Id,Opts} to be executed (or merged with other first)
%%----------------------------------------------------------------------
pack_children(PackOpt,PosOpt,SizOpt,DB,Kids,Sizes) ->
    Schildren = keep_packed(Kids,PackOpt,DB),
    pack_children2(PackOpt,PosOpt,SizOpt,Schildren,Sizes).

pack_children2(PackOpt,PosOpt,SizOpt,[{StartStop,Id}|Childs],Sizes) ->
    [pack_child(Id,StartStop,SizOpt,PosOpt,Sizes)
     | pack_children2(PackOpt,PosOpt,SizOpt,Childs,Sizes)];
pack_children2(_,_,_,[],_) ->
    [].

pack_child(Id,{StartPos,StopPos},SizOpt,PosOpt,Sizes) ->
    {Pos,Size} = find_pos(StartPos,StopPos,1,0,0,Sizes),
    {Id,[{PosOpt,Pos},{SizOpt,Size}]}.

%%----------------------------------------------------------------------
%% Returns: {PixelPos,PixelSize}
%%----------------------------------------------------------------------
find_pos(_StartPos,Pos,Pos,AccPixelPos,AccPixelSize,[Size|_]) ->
    {AccPixelPos,Size+AccPixelSize};
find_pos(StartPos,StopPos,Pos,AccPixelPos,0,[Size|Sizes])
  when Pos < StartPos ->
    find_pos(StartPos,StopPos,Pos+1,Size+AccPixelPos,0,Sizes);
find_pos(_StartPos,StopPos,Pos,AccPixelPos,AccPixelSize,[Size|Sizes])
  when Pos < StopPos ->
    find_pos(Pos,StopPos,Pos+1,AccPixelPos,Size+AccPixelSize,Sizes).

    

keep_packed([Id|Ids],PackOpt,DB) ->
    case gstk:read_impl(DB,Id,PackOpt) of
	undefined ->
	    keep_packed(Ids,PackOpt,DB);
	StartStop ->
	    [{StartStop,Id} | keep_packed(Ids,PackOpt,DB)]
    end;
keep_packed([],_,_) ->
    [].
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/3
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gstkid   - The gstkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gstkid,TkW,_DB,_) -> 
    case Option of
	bg            -> tcl2erl:ret_color([TkW," cg -bg"]);
	_ -> {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}
    end.

%% ----- Done -----
