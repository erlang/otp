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
%%% Purpose : The font model

%% ###########################################################################
%%
%% This module handle fonts. It was changed for Tcl 8.2 but it could
%% probably be simplified more.
%%
%% In Tcl 8.2 we can use named fonts. So the whe get a font request we
%% first check if it already exists and if not we name it and insert it
%% into the database.
%%
%% The font naming is also changedin Tcl 8.2.
%%
%% In Tcl 8.2 there is a way to find out the width of a string in
%% a specified font.
%%
%% ###########################################################################

-module(gstk_font).

%-compile(export_all).

-export([init/0,choose_ascii/2,choose/2,width_height/3]).


-ifndef(NEW_WIDTH_HEIGHT).
init() ->
    %% hack. the only way to find the size of a text seems to be to put
    %% it into a label in an unmappen window (DummyFontWindow)
    gstk:exec("toplevel .dfw;wm withdraw .dfw;" %deiconify
	     "label .dfw.l -text dummyinittxt -padx 0 -pady 0 -borderwidth 0;"
	     "pack .dfw.l").
-else.
init() -> true.
-endif.

%%----------------------------------------------------------------------
%% Returns: undefined if font doesn't exist
%%          {WidthPixels, HeightPixels}
%%----------------------------------------------------------------------
-ifndef(NEW_WIDTH_HEIGHT).
width_height(_DB, FontSpec, Txt) ->
    FontSpecStr = tk_font_spec(norm_font_spec(FontSpec)),
    case gstk:call([".dfw.l co -font {", FontSpecStr,"}",
		   " -text ", gstk:to_ascii(Txt)]) of
	{result, _} ->
	    Width = tcl2erl:ret_int("update idletasks;winfo w .dfw.l"),
	    Height = tcl2erl:ret_int("winfo h .dfw.l"),
%	    io:format("width_height(~p,~p) =>\n~p\n\n",[FontSpec,Txt,{Width,Height}]),
	    {Width,Height};
	_Bad_Result ->
%	    io:format("width_height(~p,~p) =>\nundefined\n\n",[FontSpec,Txt]),
	    undefined
    end.
-else.
%% This code should work but does't. Tk gives incorrect
%% values if asking to fast or something /kent
width_height(DB, FontSpec, Txt) when tuple(FontSpec) ->
    NormFontSpec = norm_font_spec(FontSpec),
    FontSpecStr = tk_font_spec(NormFontSpec),
    {Family,_,Size} = NormFontSpec,
    LineHeight =
	case cached_line_height(DB, {Family,Size}) of
	    undefined ->
		LineH = tcl2erl:ret_int(
			  ["font metrics {",FontSpecStr,"} -linespace"]),
		cache_line_height(DB, {Family,Size}, LineH),
		LineH;
	    LineH ->
		LineH
	end,
    EscapedText = gstk:to_ascii(Txt),
    Width = tcl2erl:ret_int(
	      ["font measure {",FontSpecStr,"} ",EscapedText]),
    Height = LineHeight * line_count(Txt),
    {Width,Height};

width_height(_DB, FontSpec, Txt) when list(FontSpec) ->
    EscapedText = gstk:to_ascii(Txt),
    Width =
	tcl2erl:ret_int(["font measure {",FontSpec,"} ",EscapedText]),
    LineHeight =
	tcl2erl:ret_int(["font metrics {",FontSpec,"} -linespace"]),
    Height = LineHeight * line_count(Txt),
    {Width,Height}.

cached_line_height(DB,FontSpec) ->
    gstk_db:lookup(DB, {cached_line_height,FontSpec}).

cache_line_height(DB,FontSpec,Size) ->
    gstk_db:insert(DB, {cached_line_height,FontSpec}, Size).

line_count(Line) ->
    line_count(Line, 1).

line_count([H | T], Count) ->
    Count + line_count(H, 0) + line_count(T, 0);
line_count($\n, Count) -> Count + 1;
line_count(Char, Count) when integer(Char) -> Count;
line_count([], Count) -> Count.
-endif.
    
% "expr [font metrics ",FSpec," -linespace] * \
% [regsub -all \\n ",Txt," {} ignore]"

%%----------------------------------------------------------------------
%% Returns: Font specification string in Tk format
%%
%% The input is {Family,Size} or {Family,Style,Size} where Family and
%% Style are atoms ?! FIXME true???
%%----------------------------------------------------------------------
choose_ascii(DB, Font) ->
    {Fam,Styl,Siz} = choose(DB, Font),
    {variable,V} =gstk_db:lookup(DB,{font,Fam,Styl,Siz}),
%    io:format("choose_ascii(~p) =>\n~p\n\n",[Font,V]),
    V.

%% DB contains: {font,Fam,Style,Size} -> {replaced_by,{font,Fam,Style,Size}} or
%%                            {variable, TkVariableStrInclDollar}

%% ###########################################################################
%%
%% We create a new font name on the other side and store the name in the
%% database. We reorder the options so that they have a predefined order.
%% 
%% ###########################################################################

choose(DB, FontSpec) ->
    choose_font(DB, norm_font_spec(FontSpec)).

choose_font(DB, {Fam,Styl,Siz}) ->
    Fam0 = map_family(Fam),
    case gstk_db:lookup(DB,{font,Fam0,Styl,Siz}) of
	{variable,_OwnFontName} -> true;
	undefined -> 
	    N = gstk_db:counter(DB,font),   % FIXME: Can use "font create"
					    % without name to get unique name
	    NewName=["f",gstk:to_ascii(N)],
%	    io:format("~s\n\n",
%		      [lists:flatten(["font create ",NewName," ",
%				      tk_font_spec({Fam0,Styl,Siz})])]),
	    gstk:exec(["font create ",NewName," ",
		       tk_font_spec({Fam0,Styl,Siz})]),
	    %% should us variable syntax gs(f1) instead
	    %% have to recompile erlcall to define this global gs var
	    V2 = {variable,NewName},
	    gstk_db:insert(DB,{font,Fam0,Styl,Siz},V2),
	    true
    end,
%   io:format("choose(~p,~p,~p) =>\n~p\n\n",[Fam,Styl,Siz,{Fam0,Styl,Siz}]),
    {Fam0,Styl,Siz}.


%% ----- The Font Model -----

%%  Guaranteed system fonts to exists in Tk 8.2 are:
%%
%%    Windows   : system systemfixed ansi ansifixed device oemfixed
%%    Unix      : fixed
%%
%%  Times, Courier and Helvetica always exists. Tk try to substitute
%%  others with the best matchin font.

%%  We map GS font style and names to something we know Tk 8 have.
%%  We know Tk have 'times', 'courier', 'helvetica' and 'fixed'.
%% 
%%  GS style specification is 'bold' or 'italic'.
%%  GS family is a typeface of type 'times', 'courier', 'helvetica',
%%  'symbol', 'new_century_schoolbook', or 'screen' (which is a suitable
%%  screen font).
%%
%%  Note that 'symbol' may not be present and this is not handled.
%%
%%  The X/Tk8 font handling don't work very well. The fonts are
%%  scaled "tk scaling", we can display a 9 and 10 point helvetica
%%  but "font actual {helvetica 9}" will return 10 points....

map_family(new_century_schoolbook) ->
    times;
map_family(Fam) ->
    Fam.

% Normalize so can make the coding easier and compare font
% specifications stored in database with new ones. We ignore invalid
% entries in the list.

norm_font_spec({Family,Size}) ->
    {Family,[],Size};
norm_font_spec({Family,Style,Size}) ->
    {Family,norm_style(Style),Size}.

norm_style(bold) ->
    [bold];
norm_style(italic) ->
    [italic];
norm_style([italic]) ->
    [italic];
norm_style([bold]) ->
    [bold];
norm_style([bold,italic] = Style) ->
    Style;
norm_style([italic,bold]) ->
    [bold,italic];
norm_style(List) when is_list(List) -> % not well formed list, ignore garbage
    case {lists:member(bold, List),lists:member(italic, List)} of
	{true,true} ->
	    [bold,italic];
	{true,_} ->
	    [bold];
	{_,true} ->
	    [italic];
	_ ->
	    []			   % ignore garbage
    end;
norm_style(_Any) ->		   % ignore garbage
    [].


% Create a tcl string from a normalized font specification  
% The style list is normalized.

tk_font_spec({Fam,Style,Size}) ->
    ["-family ",gstk:to_ascii(Fam),
     " -size ",gstk:to_ascii(-Size),
     tk_font_spec_style(Style)].

tk_font_spec_style([]) ->
    "";
tk_font_spec_style([bold]) ->
    " -weight bold";
tk_font_spec_style([italic]) ->
    " -slant italic";
tk_font_spec_style([bold,italic]) ->
    " -weight bold -slant italic".
