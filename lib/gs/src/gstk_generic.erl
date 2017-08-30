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

-module(gstk_generic).
-compile([{nowarn_deprecated_function,{gs,assq,2}}]).

-export([out_opts/8,
	 read_option/5,
	 mk_tkw_child/2,
	 merge_default_options/3,
	 merge_default_options/2,
	 opts_for_child/3,
	 mk_cmd_and_exec/4,
	 mk_cmd_and_exec/5,
	 mk_cmd_and_exec/6,
	 mk_cmd_and_exec/7,
	 make_command/5,
	 make_command/6,
	 make_command/7,
	 read_option/4,
	 handle_external_opt_call/9,
	 handle_external_read/1,
	 gen_anchor/9,
	 gen_anchor/5,
	 gen_height/9,
	 gen_height/5,
	 gen_width/9,
	 gen_width/5,
	 gen_x/9,
	 gen_x/5,
	 gen_y/9,
	 gen_y/5,
	 gen_raise/9,
	 gen_raise/5,
	 gen_lower/9,
	 gen_lower/5,
	 gen_enable/9,
	 gen_enable/5,
	 gen_align/9,
	 gen_align/5,
	 gen_justify/9,
	 gen_justify/5,
	 gen_padx/9,
	 gen_padx/5,
	 gen_pady/9,
	 gen_pady/5,
	 gen_font/9,
	 gen_font/5,
	 gen_label/9,
	 gen_label/5,
	 gen_activebg/9,
	 gen_activebg/5,
	 gen_activefg/9,
	 gen_activefg/5,
	 gen_default/9,
	 gen_relief/9,
	 gen_relief/5,
	 gen_bw/9,
	 gen_bw/5,
	 gen_font_wh/5,
	 gen_choose_font/5,
	 gen_data/9,
	 gen_data/5,
	 gen_pack_x/9,
	 gen_pack_x/5,
	 gen_pack_y/9,
	 gen_pack_y/5,
	 gen_pack_xy/9,
	 gen_flush/9,
	 gen_flush/5,
	 gen_keep_opt/9,
	 gen_children/5,
	 make_extern_id/2,
	 gen_id/5,
	 gen_parent/5,
	 gen_type/5,
	 gen_beep/9,
	 gen_setfocus/9,
	 gen_setfocus/5,
	 gen_buttonpress/9,
	 gen_buttonpress/5,
	 gen_buttonrelease/9,
	 gen_buttonrelease/5,
	 gen_configure/9,
	 gen_configure/5,
	 gen_destroy/9,
	 gen_destroy/5,
	 gen_enter/9,
	 gen_enter/5,
	 gen_focus_ev/9,
	 gen_focus_ev/5,
	 gen_keypress/9,
	 gen_keypress/5,
	 gen_keyrelease/9,
	 gen_keyrelease/5,
	 gen_leave/9,
	 gen_leave/5,
	 gen_motion/9,
	 gen_motion/5,
	 gen_highlightbw/9,
	 gen_highlightbw/5,
	 gen_highlightbg/9,
	 gen_highlightbg/5,
	 gen_highlightfg/9,
	 gen_highlightfg/5,
	 gen_selectbw/9,
	 gen_selectbw/5,
	 gen_selectfg/9,
	 gen_selectfg/5,
	 gen_selectbg/9,
	 gen_selectbg/5,
	 gen_fg/9,
	 gen_fg/5,
	 gen_bg/9,
	 gen_bg/5,
	 gen_so_activebg/9,
	 gen_so_activebg/5,
	 gen_so_bc/9,
	 gen_so_bc/5,
	 gen_so_scrollfg/9,
	 gen_so_scrollfg/5,
	 gen_so_scrollbg/9,
	 gen_so_scrollbg/5,
	 obj/1,
	 gen_so_bg/9,
	 gen_so_bg/5,
	 gen_so_selectbw/9,
	 gen_so_selectbw/5,
	 gen_so_selectfg/9,
	 gen_so_selectfg/5,
	 gen_so_selectbg/9,
	 gen_so_selectbg/5,
	 gen_so_scrolls/9,
	 gen_so_hscroll/5,
	 gen_so_vscroll/5,
	 cursors/0,
	 gen_cursor/9,
	 gen_cursor/5,
	 gen_citem_coords/9,
	 gen_citem_coords/5,
	 gen_citem_fill/9,
	 gen_citem_fill/5,
	 gen_citem_lower/9,
	 gen_citem_raise/9,
	 gen_citem_move/9,
	 move_coords/3,
	 add_to_coords/3,
	 gen_citem_setfocus/9,
	 gen_citem_setfocus/5,
	 gen_citem_buttonpress/9,
	 gen_citem_buttonrelease/9,
	 gen_citem_enter/9,
	 gen_citem_keypress/9,
	 gen_citem_keyrelease/9,
	 gen_citem_leave/9,
	 gen_citem_motion/9,
	 scrolls_vh/3,
	 parse_scrolls/1,
	 parse_scrolls/2,
	 parse_scrolls/4,
	 bind/5,
	 bind/6,
	 ebind/6,
	 eunbind/6,
	 item_bind/6,
	 item_ebind/6,
	 item_eunbind/5,
	 event/5,
	 read_option/3,
	 make_command/4,
	 mk_create_opts_for_child/4]).

-include("gstk.hrl").
-include("gstk_generic.hrl").

%%----------------------------------------------------------------------
%% Returns: a new unique TkWidget (string())
%%----------------------------------------------------------------------
mk_tkw_child(DB,#gstkid{parent=P,objtype=Ot}) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, P),
    PW = Pgstkid#gstkid.widget,
    Oref = gstk_db:counter(DB, Ot),
    PF = gstk_widgets:suffix(Ot),
    _TkW = lists:concat([PW, PF, Oref]).

%%----------------------------------------------------------------------
%% Purpose: Merges options. Opts have higher priority than BuiltIn
%%          (and ParentOpts have higher than BuiltIn)
%% Returns: A list of new options.
%%----------------------------------------------------------------------
merge_default_options(ParOpts, BuildInOpts, Opts) ->
    %% parents options first
    Tmp=merge_default_options(ParOpts, lists:sort(Opts)), 
    merge_default_options(BuildInOpts,Tmp).

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) < element(1,Opt) ->
    [Def | merge_default_options(Ds,[Opt|Os])];

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) > element(1,Opt) ->
    [Opt | merge_default_options([Def|Ds],Os)];

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) == element(1,Opt) ->
    [Opt | merge_default_options(Ds,Os)];

merge_default_options(Defs,[Opt|Os]) ->
    [Opt | merge_default_options(Defs,Os)];

merge_default_options([],Opts) -> Opts;
merge_default_options(Defs,[]) -> Defs.

opts_for_child(DB,Childtype,ParId) ->
    case gs_widgets:container(Childtype) of
	true -> 
	    gstk_db:default_container_opts(DB,ParId,Childtype);
	false ->
	    gstk_db:default_opts(DB,ParId,Childtype)
    end.

mk_create_opts_for_child(DB,#gstkid{objtype=ChildType}, Pgstkid, Opts) ->
    merge_default_options(
      opts_for_child(DB,ChildType,Pgstkid#gstkid.id),
      gs_widgets:default_options(ChildType),
      Opts).

mk_cmd_and_exec(Opts,Gstkid,Scmd,DB) ->
    TkW = Gstkid#gstkid.widget,
    mk_cmd_and_exec(Opts,Gstkid,TkW,Scmd,[";place ", TkW],DB,dummy).
mk_cmd_and_exec(Opts,Gstkid,Scmd,Pcmd,DB) ->
    mk_cmd_and_exec(Opts,Gstkid,Gstkid#gstkid.widget,Scmd,Pcmd,DB,dummy).
mk_cmd_and_exec(Options, Gstkid, TkW, SCmd, PCmd, DB) ->
    mk_cmd_and_exec(Options, Gstkid, TkW, SCmd, PCmd, DB,dummy).
mk_cmd_and_exec(Options, Gstkid, TkW, SCmd, PCmd, DB,ExtraArg) ->
    case gstk_generic:make_command(Options,Gstkid,TkW,SCmd,PCmd,DB,ExtraArg) of
	{error,Reason} -> {error,Reason};
	Cmd when is_list(Cmd) ->
	    gstk:exec(Cmd)
    end.

%%----------------------------------------------------------------------
%% SCmd: SimplePreCommand - prepended to simple (s) options
%% PCmd: PlacePreCommand - prepended to placer (p) options
%%       (should start with ';' (at least if preceeded with simple cmds))
%% Comment: If some function changes the gstkid,
%%          it's responsible for storing it in the DB.
%%----------------------------------------------------------------------
make_command(Opts,Gstkid,Scmd,DB) ->
    TkW = Gstkid#gstkid.widget,
    make_command(Opts,Gstkid,TkW,Scmd,[";place ", TkW],DB,dummy).
make_command(Opts,Gstkid,Scmd,Pcmd,DB) ->
    make_command(Opts,Gstkid,Gstkid#gstkid.widget,Scmd,Pcmd,DB,dummy).
make_command(Options, Gstkid, TkW, SCmd, PCmd, DB) ->
    make_command(Options, Gstkid, TkW, SCmd, PCmd, DB,dummy).
make_command(Options, Gstkid, TkW, SCmd, PCmd, DB,ExtraArg) ->
    case out_opts(Options, Gstkid, TkW, DB, ExtraArg, [], [], []) of
	{[], [], []} -> [];
	{Si, [], []} -> [SCmd, Si,$;];
	{[], Pl, []} -> [PCmd, Pl,$;];
	{[], [], Co} -> [$;,Co];
	{[], Pl, Co} -> [PCmd, Pl, $;, Co];
	{Si, [], Co} -> [SCmd, Si, $;, Co];
	{Si, Pl, []} -> [SCmd, Si, PCmd, Pl, $;];
	{Si, Pl, Co} -> [SCmd, Si, PCmd, Pl, $;, Co];
	{error,Reason} -> {error,Reason}
    end.

read_option(DB,Gstkid,Opt) ->
    read_option(DB,Gstkid,Gstkid#gstkid.widget,Opt,dummy).
read_option(DB,Gstkid,Opt,ExtraArg) ->
    read_option(DB,Gstkid,Gstkid#gstkid.widget,Opt,ExtraArg).

%%----------------------------------------------------------------------
%% Args: Args is [Gstkid, TkW, DB, ExtraArg]
%% Comment: An optimization:don't reconstruct the arg list for apply each time.
%%          This is the option-engine so we should optimize.
%%----------------------------------------------------------------------
handle_external_opt_call([Opt|Options],Gstkid,TkW,DB,ExtraArg,ExtRes,S,P,C) ->
    case ExtRes of
	{s, Cmd} ->
	    out_opts(Options,Gstkid, TkW,DB, ExtraArg, [Cmd|S], P, C);
	{p, Cmd} ->
	    out_opts(Options, Gstkid,TkW,DB, ExtraArg, S, [Cmd|P], C);
	{c, Cmd} ->
	    out_opts(Options, Gstkid,TkW,DB, ExtraArg,S, P, [Cmd,$;|C]);
	none ->
	    out_opts(Options, Gstkid,TkW,DB,ExtraArg, S, P, C);
						%	{s, NGstkid, Cmd} ->
						%	    out_opts(Options,NGstkid,TkW,DB,ExtraArg, [Cmd|S], P, C);
						%	{p, NGstkid, Cmd} ->
						%	    out_opts(Options,NGstkid,TkW,DB,ExtraArg, S, [Cmd|P], C);
	{c, NGstkid, Cmd} ->
	    out_opts(Options,NGstkid,TkW,DB, ExtraArg,S,P,[Cmd,$;|C]);
	{none, NGstkid} ->
	    out_opts(Options,NGstkid,TkW,DB, ExtraArg, S, P, C);
	{sp,{Scmd,Pcmd}} ->
	    out_opts(Options,Gstkid,TkW,DB,ExtraArg,[Scmd|S],[Pcmd|P],C);
	invalid_option ->
	    {error,{invalid_option,Gstkid#gstkid.objtype,Opt}};
	break -> % a hack. it is possible to abort generic option handling at
	    %% any time (without even inserting the gstkid inte to DB (for
	    %% performance reasons)).
	    {S, P, C}
    end.

handle_external_read(Res) ->
    %% We have removed dead code here that attempted to translate
    %% a bad return value from {bad_result,{A,B,C}} to {error,{A,B,C}}.
    %% Since the gs application is deprecated, we don't want to introduce
    %% a potential incompatibility; thus we have removed the dead code
    %% instead of correcting it.
    Res.

%%----------------------------------------------------------------------
%% Generic options
%%----------------------------------------------------------------------

gen_anchor(How,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,[" -anc ", gstk:to_ascii(How)|P],C).
gen_anchor(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_place(anchor, TkW).

gen_height(Height,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{height,Height}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,
	     [" -he ", gstk:to_ascii(Height)|P],C).
gen_height(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,height).

gen_width(Width,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{width,Width}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,
	     [" -wi ", gstk:to_ascii(Width)|P],C).
gen_width(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,width).

gen_x(X,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{x,X}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,
	     [" -x ", gstk:to_ascii(X)|P],C).
gen_x(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,x).

gen_y(Y,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{y,Y}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,
	     [" -y ", gstk:to_ascii(Y)|P],C).
gen_y(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,y).

gen_raise(_,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,["raise ", TkW,$;|C]).
gen_raise(_Opt,_Gstkid,_TkW,_DB,_ExtraArg) ->
    undefined.

gen_lower(_,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,["lower ", TkW,$;|C]).
gen_lower(_Opt,_Gstkid,_TkW,_DB,_ExtraArg) ->
    undefined.

gen_enable(true,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -st normal"|S],P,C);
gen_enable(false,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -st disabled"|S],P,C).
gen_enable(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_enable([TkW, " cg -st"]).

gen_align(How,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -an ", gstk:to_ascii(How)|S],P,C).
gen_align(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -anch"]).

gen_justify(How,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -ju ", gstk:to_ascii(How)|S],P,C).
gen_justify(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -ju"]).

gen_padx(Pad,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -padx ", gstk:to_ascii(Pad)|S],P,C).
gen_padx(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -padx"]).

gen_pady(Pad,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -pady ", gstk:to_ascii(Pad)|S],P,C).
gen_pady(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -pady"]).


gen_font(Font,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{font,Font}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,
	     [" -font ", gstk_font:choose_ascii(DB,Font)|S],P,C).
gen_font(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,font,undefined).

gen_label({text,Text},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -text ", gstk:to_ascii(Text), " -bi {}"|S],P,C);
gen_label({image,Img},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    I2 = re:replace(Img, [92,92], "/", [global,{return,list}]),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -bi \"@", I2, "\" -text {}"|S],P,C).
gen_label(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    case gstk:call([TkW, " cg -bit"]) of
	{result, [$@|Image]} -> {image,Image};
	_Nope ->
	    case gstk:call([TkW, " cg -text"]) of
		{result, Txt} -> {text, Txt};
		Bad_Result -> Bad_Result
	    end
    end.

gen_activebg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -activeba ", gstk:to_color(Color)|S],P,C).
gen_activebg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -activeba"]).

gen_activefg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -activef ", gstk:to_color(Color)|S],P,C).
gen_activefg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -activef"]).


gen_default(Opt,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    case Opt of
	{all, {font, Font}} ->
	    C2 = ["option a *",tl(TkW), % have to remove preceeding dot
		  "*font ",gstk_font:choose_ascii(DB, Font)],
	    gstk_db:insert_def(Gstkid,grid,{font,Font}),
	    gstk_db:insert_def(Gstkid,text,{font,Font}),
	    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]);
	{buttons, {font, Font}} ->
	    C2 = ["option a *",tl(TkW), % have to remove preceeding dot
		  ".Button.font ",gstk_font:choose_ascii(DB, Font)],
	    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]);
	{buttons,{Key,Val}} ->
	    gstk_db:insert_def(Gstkid,button,{Key,Val}),
	    gstk_db:insert_def(Gstkid,checkbutton,{Key,Val}),
	    gstk_db:insert_def(Gstkid,radiobutton,{Key,Val}),
	    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
	{ObjType, {Key,Val}} ->
	    gstk_db:insert_def(Gstkid,ObjType,{Key,Val}),
	    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)
    end.


gen_relief(Relief,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -reli ",gstk:to_ascii(Relief)|S],P,C).
gen_relief(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -reli"]).

gen_bw(Wth,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -bd ", gstk:to_ascii(Wth)|S],P,C).
gen_bw(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_int([TkW, " cg -bd"]).



gen_font_wh({font_wh,{Font, Txt}},_Gstkid,_TkW,DB,_) ->
    gstk_font:width_height(DB, gstk_font:choose(DB,Font), Txt).

gen_choose_font({choose_font,Font},_Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_font:choose(DB,Font).

gen_data(Data,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{data,Data}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).
gen_data(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,data).

gen_pack_x({Start,Stop},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,{Start,Stop}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_x(Col,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) when is_integer(Col) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,{Col,Col}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).
gen_pack_x(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,pack_x, undefined).

gen_pack_y({Start,Stop},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_y,{Start,Stop}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_y(Row,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) when is_integer(Row) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).
gen_pack_y(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid,pack_y, undefined).

gen_pack_xy({Col,Row},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)
  when is_integer(Col), is_integer(Row) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,{Col,Col}}),
    gstk_db:insert_opt(DB,Gstkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({Col,{StartRow,StopRow}},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)
  when is_integer(Col) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,{Col,Col}}),
    gstk_db:insert_opt(DB,Gstkid,{pack_y,{StartRow,StopRow}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({{StartCol,StopCol},Row},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)
  when is_integer(Row) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,{StartCol,StopCol}}),
    gstk_db:insert_opt(DB,Gstkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({Col,Row},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{pack_x,Col}),
    gstk_db:insert_opt(DB,Gstkid,{pack_y,Row}),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).


gen_flush(_Opt,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)  -> 
    tcl2erl:ret_int(["update idletasks;expr 1+1"]),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).
gen_flush(_Opt,_Gstkid,_TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_int(["update idletasks;expr 1+1"]).

						% a hidden impl option.
gen_keep_opt(Opt,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C)  -> 
    gstk_db:insert_opt(DB,Gstkid,Opt),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,C).

gen_children(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    make_extern_id(gstk_db:lookup_kids(DB, Gstkid#gstkid.id), DB).

make_extern_id([Id|Ids], DB) ->
    [gstk:make_extern_id(Id, DB) | make_extern_id(Ids, DB)];
make_extern_id([], _) -> [].

gen_id(_Opt,#gstkid{id=Id},_TkW,DB,_ExtraArg) ->
    gstk:make_extern_id(Id, DB).

gen_parent(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk:make_extern_id(Gstkid#gstkid.parent, DB).

gen_type(_Opt,Gstkid,_TkW,_DB,_ExtraArg) ->
    Gstkid#gstkid.objtype.

gen_beep(_,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,["bell;",$;|C]).

gen_setfocus(true,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,["focus ", TkW,$;|C]);
gen_setfocus(false,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,["focus .",$;|C]).

gen_setfocus(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_focus(TkW, "focus").

gen_buttonpress(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, buttonpress, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_buttonpress(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB, Gstkid, buttonpress).

gen_buttonrelease(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, buttonrelease, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_buttonrelease(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,buttonrelease).

gen_configure(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, configure, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_configure(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,configure).

gen_destroy(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, destroy, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_destroy(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,destroy).

gen_enter(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, enter, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_enter(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,enter).

gen_focus_ev(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, focus, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_focus_ev(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,focus).

gen_keypress(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, keypress, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_keypress(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,keypress).

gen_keyrelease(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, keyrelease, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_keyrelease(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,keyrelease).

gen_leave(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, leave, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_leave(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,leave).

gen_motion(On,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gstkid, TkW, motion, On),
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_motion(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:is_inserted(DB,Gstkid,motion).

gen_highlightbw(Wth,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -highlightt ", gstk:to_ascii(Wth)|S],P,C).
gen_highlightbw(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_int([TkW, " cg -highlightt"]).

gen_highlightbg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -highlightb ", gstk:to_color(Color)|S],P,C).
gen_highlightbg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -highlightb"]).

gen_highlightfg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -highlightc ", gstk:to_color(Color)|S],P,C).
gen_highlightfg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW,  " cg -highlightc"]).


gen_selectbw(Width,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectbo ", gstk:to_ascii(Width),$;|C]).
gen_selectbw(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_int([TkW," cg -selectbo"]).

gen_selectfg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectfo ", gstk:to_color(Color),$;|C]).
gen_selectfg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -selectfo"]).

gen_selectbg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectba ", gstk:to_color(Color),$;|C]).
gen_selectbg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -selectba"]).

gen_fg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -fg ", gstk:to_color(Color)|S],P,C).
gen_fg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -fg"]).

gen_bg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -bg ", gstk:to_color(Color)|S],P,C).
gen_bg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -bg"]).

%%----------------------------------------------------------------------
%% Generic functions for scrolled objects
%%----------------------------------------------------------------------
gen_so_activebg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gstk:to_color(Color),
    C2 = [TkW, ".sy conf -activeba ", Col,$;,
	  TkW, ".pad.sx conf -activeba ", Col],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_activebg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -activeba"]).

gen_so_bc(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gstk:to_color(Color),
    C2= [TkW, " conf -bg ", Col,$;,
	 TkW, ".sy conf -highlightba ", Col,$;,
	 TkW, ".pad.it conf -bg ", Col,$;,
	 TkW, ".pad.sx conf -highlightba ", Col],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_bc(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -bg"]).

gen_so_scrollfg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gstk:to_color(Color),
    C2=[TkW, ".sy conf -bg ", Col,$;,
	TkW, ".pad.sx conf -bg ", Col],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_scrollfg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -bg"]).


gen_so_scrollbg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gstk:to_color(Color),
    C2 = [TkW, ".sy conf -troughc ", Col, $;,
	  TkW, ".pad.sx conf -troughc ", Col],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).

gen_so_scrollbg(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -troughc"]).

obj(#gstkid{widget_data=SO}) ->
    SO#so.object.

gen_so_bg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    C2= [obj(Gstkid), " conf -bg ", gstk:to_color(Color)],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_bg(_Opt,Gstkid,_TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([obj(Gstkid)," cg -bg"]).

gen_so_selectbw(Width,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gstkid), " conf -selectbo ", gstk:to_ascii(Width)],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectbw(_Opt,Gstkid,_TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_int([obj(Gstkid)," cg -selectbo"]).

gen_so_selectfg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gstkid), " conf -selectfo ", gstk:to_color(Color)],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectfg(_Opt,Gstkid,_TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([obj(Gstkid)," cg -selectfo"]).

gen_so_selectbg(Color,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gstkid), " conf -selectba ", gstk:to_color(Color)],
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectbg(_Opt,Gstkid,_TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_color([obj(Gstkid)," cg -selectba"]).

gen_so_scrolls({Vscroll, Hscroll},Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    SO = Gstkid#gstkid.widget_data,
    NewSO = SO#so{hscroll=Hscroll, vscroll=Vscroll},
    C2 = scrolls_vh(TkW, Vscroll, Hscroll),
    Ngstkid = Gstkid#gstkid{widget_data=NewSO},
    gstk_db:update_widget(DB,Ngstkid),
    out_opts(Opts,Ngstkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).

						% read-only
gen_so_hscroll(_Opt,#gstkid{widget_data=SO},_TkW,_DB,_) ->
    SO#so.hscroll.

						% read-only
gen_so_vscroll(_Opt,#gstkid{widget_data=SO},_TkW,_DB,_) ->
    SO#so.vscroll.

cursors() -> [{arrow,"top_left_arrow"},{busy,"watch"},{cross,"X_cursor"},
	      {hand,"hand2"},{help,"question_arrow"},{resize,"fleur"},
	      {text,"xterm"}].

gen_cursor(parent,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -cur {}"|S],P,C);
gen_cursor(Cur,Opts,Gstkid,TkW,DB,ExtraArg,S,P,C) ->
    case gs:assq(Cur,cursors()) of
	{value, TxtCur} ->
	    out_opts(Opts,Gstkid,TkW,DB,ExtraArg,[" -cur ",TxtCur|S],P,C);
	_ ->
	    {error,{invalid_cursor,Gstkid#gstkid.objtype,Cur}}
    end.
gen_cursor(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    case tcl2erl:ret_str([TkW," cg -cur"]) of
	"" -> parent;
	Txt when is_list(Txt) ->
	    case lists:keysearch(Txt,2,cursors()) of
		{value,{Cur,_}} -> Cur;
		_ -> {bad_result, read_cursor}
	    end;
	Bad_Result -> Bad_Result
    end.

gen_citem_coords(Coords,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    gstk_db:insert_opt(DB,Gstkid,{coords,Coords}),
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " coords ", AItem," ",gstk_canvas:coords(Coords),$;|C]).
gen_citem_coords(_Opt,Gstkid,_TkW,DB,_ExtraArg) ->
    gstk_db:opt(DB,Gstkid, coords).

gen_citem_fill(none,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,[" -f {}"|S],P,C);
gen_citem_fill(Color,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,[" -f ",gstk:to_color(Color)|S],P,C).
gen_citem_fill(_Opt,_Gstkid,TkW,_DB,AItem) ->
    tcl2erl:ret_color([TkW, " itemcg ", AItem, " -f"]).

gen_citem_lower(_,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " lower ", AItem,$;|C]).

gen_citem_raise(_,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " raise ", AItem,$;|C]).

gen_citem_move({Dx,Dy},Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    NewCoords = move_coords(Dx,Dy,gstk_db:opt(DB,Gstkid,coords)),
    gstk_db:insert_opt(DB,Gstkid,NewCoords),
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " move ", AItem, " ",
	      gstk:to_ascii(Dx), " ", gstk:to_ascii(Dy),$;|C]).

move_coords(Dx,Dy,Coords) ->
    Coords2 = add_to_coords(Dx,Dy, Coords),
    {coords,Coords2}.

add_to_coords(Dx,Dy,[{X,Y}|Coords]) ->
    [{X+Dx,Y+Dy}|add_to_coords(Dx,Dy,Coords)];
add_to_coords(_,_,[]) -> [].


gen_citem_setfocus(true,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " focus ", AItem,$;|C]);
gen_citem_setfocus(false,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [TkW, " focus {}",$;|C]).
gen_citem_setfocus(_Opt,_Gstkid,TkW,_DB,_ExtraArg) ->
    tcl2erl:ret_focus(gstk:to_ascii(bug_aitem),[TkW, " focus"]).

gen_citem_buttonpress(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem,buttonpress, On),$;|C]).
gen_citem_buttonrelease(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB,Gstkid,TkW,AItem,buttonrelease, On),$;|C]).
gen_citem_enter(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem, enter, On),$;|C]).

gen_citem_keypress(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem, keypress, On),$;|C]).
gen_citem_keyrelease(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem, keyrelease, On),$;|C]).

gen_citem_leave(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem, leave, On),$;|C]).
gen_citem_motion(On,Opts,Gstkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gstkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gstkid, TkW, AItem, motion, On),$;|C]).


scrolls_vh(W, V,       true) -> scrolls_vh(W, V, bottom);
scrolls_vh(W, true,       H) -> scrolls_vh(W, left, H);
scrolls_vh(W, left,  bottom) -> ["so_bottom_left ",W];
scrolls_vh(W, left,     top) -> ["so_top_left ",W];
scrolls_vh(W, left,       _) -> ["so_left ",W];
scrolls_vh(W, right, bottom) -> ["so_bottom_right ",W];
scrolls_vh(W, right,    top) -> ["so_top_right ",W];
scrolls_vh(W, right,      _) -> ["so_right ",W];
scrolls_vh(W, _,     bottom) -> ["so_bottom ",W];
scrolls_vh(W, _,        top) -> ["so_top ",W];
scrolls_vh(W, _,          _) -> ["so_plain ",W].

%% create version
parse_scrolls(Opts) ->
    {Vscroll, Hscroll, NewOpts} = parse_scrolls(Opts, false, false, []),
    {Vscroll, Hscroll, [{scrolls, {Vscroll, Hscroll}} | NewOpts]}.

%% config version
parse_scrolls(Gstkid, Opts) ->
    SO = Gstkid#gstkid.widget_data,
    Vscroll = SO#so.vscroll,
    Hscroll = SO#so.hscroll,
    case parse_scrolls(Opts, Vscroll, Hscroll, []) of
	{Vscroll, Hscroll, Opts} -> Opts;
	{NewVscroll, NewHscroll, NewOpts} -> 
	    [{scrolls, {NewVscroll, NewHscroll}} | NewOpts]
    end.


parse_scrolls([Option | Rest], Vscroll, Hscroll, Opts) when is_tuple(Option) ->
    case element(1, Option) of
	vscroll ->
	    parse_scrolls(Rest, element(2, Option), Hscroll, Opts);
	hscroll ->
	    parse_scrolls(Rest, Vscroll, element(2, Option), Opts);
	_ ->
	    parse_scrolls(Rest, Vscroll, Hscroll, [Option | Opts])
    end;

parse_scrolls([Option | Rest], Vscroll, Hscroll, Opts) ->
    parse_scrolls(Rest, Vscroll, Hscroll, [Option | Opts]);

parse_scrolls([], Vscroll, Hscroll, Opts) ->
    {Vscroll, Hscroll, Opts}.


%%
%% Event bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for complex widgets
%%
bind(DB, Gstkid, TkW, Etype, On) ->
    WD = Gstkid#gstkid.widget_data,
    TkW2 = if is_record(WD, so) ->
		   WD#so.object;
	      true -> TkW
	   end,
    case bind(DB, Gstkid, TkW2, Etype, On, "") of
	invalid_option -> invalid_option;
	Cmd ->
	    Cmd
    end.

bind(DB, Gstkid, TkW, Etype, On, WS) ->
    case On of
	true  -> ebind(DB, Gstkid, TkW, Etype, WS, "");
	false -> eunbind(DB, Gstkid, TkW, Etype, WS, "");
	{true, Edata} -> ebind(DB, Gstkid, TkW, Etype, WS, Edata);
	{false, Edata} -> eunbind(DB, Gstkid, TkW, Etype, WS, Edata);
	_     -> invalid_option
    end.


%%
%% Event bind on
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for complex widgets
%%
ebind(DB, Gstkid, TkW, Etype, WS, Edata) ->
    Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion -> [P, " <Motion> {erlsend ", Eref, " %x %y}"];
	      keypress ->
		  [P, " <KeyPress> {erlsend ", Eref," %K %N 0 0};",
		   P, " <Shift-KeyPress> {erlsend ", Eref, " %K %N 1 0};",
		   P, " <Control-KeyPress> {erlsend ", Eref, " %K %N 0 1};",
		   P," <Control-Shift-KeyPress> {erlsend ", Eref," %K %N 1 1}"];
	      keyrelease ->
		  [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0};",
		   P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0};",
		   P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1};",
		   P," <Control-Shift-KeyRelease> {erlsend ",Eref," %K %N 1 1}"];
	      buttonpress ->
		  [P, " <ButtonPress> {erlsend ", Eref, " %b %x %y}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {erlsend ", Eref, " %b %x %y}"];
	      leave -> [P, " <Leave> {erlsend ", Eref, "}"];
	      enter -> [P, " <Enter> {erlsend ", Eref, "}"];
	      destroy ->
		  [P, " <Destroy> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, "}}"];
	      focus ->
		  [P, " <FocusIn> {erlsend ", Eref, " 1};" ,
		   P, " <FocusOut> {erlsend ", Eref, " 0}"];
	      configure ->
		  [P, " <Configure> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, " %w %h %x %y}}"]
	  end,
    Cmd.


%%
%% Unbind event
%%
%% Should return a list of tcl commands
%% Already checked for validation in bind/5
%%
%% WS = Widget suffix for complex widgets
%%
eunbind(DB, Gstkid, TkW, Etype, WS, _Edata) ->
    gstk_db:delete_event(DB, Gstkid, Etype),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion ->
		  [P, " <Motion> {}"];
	      keypress -> 
		  [P, " <KeyPress> {};",
		   P, " <Shift-KeyPress> {};",
		   P, " <Control-KeyPress> {};",
		   P, " <Control-Shift-KeyPress> {}"];
	      keyrelease -> 
		  [P, " <KeyRelease> {};",
		   P, " <Shift-KeyRelease> {};",
		   P, " <Control-KeyRelease> {};",
		   P, " <Control-Shift-KeyRelease> {}"];
	      buttonpress ->
		  [P, " <ButtonPress> {}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {}"];
	      leave ->
		  [P, " <Leave> {}"];
	      enter ->
		  [P, " <Enter> {}"];
	      destroy ->
		  [P, " <Destroy> {}"];
	      focus ->
		  [P, " <FocusIn> {};",
		   P, " <FocusOut> {}"];
	      configure ->
		  [P, " <Configure> {}"]
	  end,
    Cmd.


%%
%% Event item bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
item_bind(DB, Gstkid, Canvas, Item, Etype, On) ->
    case On of
	true          -> item_ebind(DB, Gstkid, Canvas, Item, Etype, "");
	{true, Edata} -> item_ebind(DB, Gstkid, Canvas, Item, Etype, Edata);
	_Other         -> item_eunbind(DB, Gstkid, Canvas, Item, Etype)
    end.

%%
%% Event bind on
%%
%% Should return a list of tcl commands or invalid_option
%%
item_ebind(DB, Gstkid, Canvas, Item, Etype, Edata) ->
    Eref = gstk_db:insert_event(DB, Gstkid, Etype, Edata),
    P = [Canvas, " bind ", Item],
    case Etype of
	enter  -> [P, " <Enter> {erlsend ", Eref, "}"];
	leave  -> [P, " <Leave> {erlsend ", Eref, "}"];
	motion -> [P, " <Motion> {erlsend ", Eref, " [",
		   Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"];
	keypress ->
	    [P, " <Key> {erlsend ", Eref," %K %N 0 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Shift-Key> {erlsend ", Eref, " %K %N 1 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Key> {erlsend ", Eref, " %K %N 0 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Shift-Key> {erlsend ", Eref," %K %N 1 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]}"];
	keyrelease ->
	    [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Shift-KeyRelease> {erlsend ", Eref," %K %N 1 1[",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]}"];
	buttonpress ->
	    [P, " <Button> {erlsend ", Eref, " %b [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"];
	buttonrelease ->
	    [P, " <ButtonRelease> {erlsend ", Eref, " %b [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"]
    end.


%%
%% Unbind event
%%
%% Should return a list of tcl commands
%% Already checked for validation in bind/5
%%
item_eunbind(DB, Gstkid, Canvas, Item, Etype) ->
    gstk_db:delete_event(DB, Gstkid, Etype),
    P = [Canvas, " bind ", Item],
    Cmd = case Etype of
	      enter         -> [P, " <Enter> {}"];
	      leave         -> [P, " <Leave> {}"];
	      motion        -> [P, " <Motion> {}"];
	      keypress -> 
		  [P, " <KeyPress> {};",
		   P, " <Shift-KeyPress> {};",
		   P, " <Control-KeyPress> {};",
		   P, " <Control-Shift-KeyPress> {}"];
	      keyrelease -> 
		  [P, " <KeyRelease> {};",
		   P, " <Shift-KeyRelease> {};",
		   P, " <Control-KeyRelease> {};",
		   P, " <Control-Shift-KeyRelease> {}"];
	      buttonpress   -> [P, " <Button> {}"];
	      buttonrelease -> [P, " <ButtonRelease> {}"]
	  end,
    Cmd.



event(DB, Gstkid, Etype, _Edata, Args) ->
    #gstkid{owner=Ow,id=Id} = Gstkid,
    Data = gstk_db:opt(DB,Gstkid,data),
    gs_frontend:event(get(gs_frontend),Ow,{gs,Id,Etype,Data,Args}).
