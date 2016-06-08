%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(dbg_wx_code).

-export([code_area/1, 
	 load_code/2, unload_code/1, 
	 add_break_to_code/3, del_break_from_code/2,
	 find/4,
	 goto_pos/2, current_pos/1,
	 mark_line/3, get_no_lines/1, goto_line/2]).

-include_lib("wx/include/wx.hrl").

-define(stc, wxStyledTextCtrl).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

code_area(Parent) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    %%Ed = wxStyledTextCtrl:new(Parent, [{size, {700, 500}}]),
    Ed = wxStyledTextCtrl:new(Parent),

    ?stc:styleClearAll(Ed),
    ?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    ?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    ?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
    ?stc:setMarginWidth(Ed, 0, LW),

    ?stc:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    %%?stc:hideSelection(Ed, true),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
	       %% Optional 2.9 stuff
	       {?wxSTC_ERLANG_COMMENT_FUNCTION, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_MODULE, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC, {160,53,35}},
	       {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160,53,35}},
	       {?wxSTC_ERLANG_ATOM_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_MACRO_QUOTED, {40,144,170}},
	       {?wxSTC_ERLANG_RECORD_QUOTED, {40,100,20}},
	       {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0,0,0}},
	       {?wxSTC_ERLANG_BIFS, {130,40,172}},
	       {?wxSTC_ERLANG_MODULES, {64,102,244}},
	       {?wxSTC_ERLANG_MODULES_ATT, {64,102,244}}
	      ],

    SetStyle = fun({Style, Color}) ->
		       ?stc:styleSetFont(Ed, Style, FixedFont),
		       ?stc:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    ?stc:setKeyWords(Ed, 0, keyWords()),

    %% Margins Markers
    %% Breakpoint Should be a pixmap?
    ?stc:markerDefine(Ed, 0, ?wxSTC_MARK_CIRCLE, [{foreground, {170,20,20}}]),    
    ?stc:markerDefine(Ed, 0, ?wxSTC_MARK_CIRCLE, [{background, {200,120,120}}]),    
    %% Disabled Breakpoint 
    ?stc:markerDefine(Ed, 1, ?wxSTC_MARK_CIRCLE, [{foreground, {20,20,170}}]),
    ?stc:markerDefine(Ed, 1, ?wxSTC_MARK_CIRCLE, [{background, {120,120,200}}]),
    
    %% Current Line
    ?stc:markerDefine(Ed, 2, ?wxSTC_MARK_ARROW,  [{foreground, {20,170,20}}]),
    ?stc:markerDefine(Ed, 2, ?wxSTC_MARK_ARROW,  [{background, {200,255,200}}]),
    ?stc:markerDefine(Ed, 3, ?wxSTC_MARK_BACKGROUND, [{background, {200,255,200}}]),

    %% Scrolling
    Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
    ?stc:setYCaretPolicy(Ed, Policy, 3),
    ?stc:setVisiblePolicy(Ed, Policy, 3),

    ?stc:connect(Ed, stc_doubleclick),
    ?stc:setReadOnly(Ed, true),
    Ed.

load_code(Ed, Code) ->
    ?stc:setReadOnly(Ed, false),
    ?stc:setTextRaw(Ed, Code),
    Lines = ?stc:getLineCount(Ed),
    Sz = trunc(math:log10(Lines))+1,
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Sz, $9)),
    %%io:format("~p ~p ~p~n", [Lines, Sz, LW]),
    ?stc:setMarginWidth(Ed, 0, LW+5),
    ?stc:setReadOnly(Ed, true),
    ok.

unload_code(Ed) ->
    ?stc:setReadOnly(Ed, false),
    ?stc:setTextRaw(Ed, <<0:8>>),
    ?stc:setReadOnly(Ed, true),
    ok.

add_break_to_code(Ed, Line, active) ->
    ?stc:markerDelete(Ed, Line-1, 1),
    ?stc:markerAdd(Ed, Line-1, 0),
    ok;
add_break_to_code(Ed, Line, inactive) ->
    ?stc:markerDelete(Ed, Line-1, 0),
    ?stc:markerAdd(Ed, Line-1, 1),
    ok.

del_break_from_code(Ed,Line) ->
    ?stc:markerDelete(Ed, Line-1, 0),
    ?stc:markerDelete(Ed, Line-1, 1).

mark_line(Ed,Prev,Line) ->
    goto_line(Ed, Line),
    ?stc:markerDelete(Ed, Prev-1, 2), 
    ?stc:markerAdd(Ed, Line-1, 2),
    ?stc:markerDelete(Ed, Prev-1, 3), 
    ?stc:markerAdd(Ed, Line-1, 3).

get_no_lines(Ed) ->
    ?stc:getLineCount(Ed).
    
goto_line(_Ed,0) -> ignore;
goto_line(Ed,Line) ->
    ?stc:gotoLine(Ed, Line-1).

current_pos(Ed) ->
    ?stc:getCurrentPos(Ed).
    
goto_pos(Ed,Pos) ->
    ?stc:gotoPos(Ed, Pos).

find(Ed, Str, Case, Next) ->
    ?stc:searchAnchor(Ed),
    Flag = 
	if  Case -> ?wxSTC_FIND_MATCHCASE;
	    true -> 0
	end,
    Res = 
	if 
	    Next -> ?stc:searchNext(Ed, Flag, Str);
	    true -> ?stc:searchPrev(Ed, Flag, Str)
	end,
    case Res >= 0 of	
	true -> 
	    %% io:format("Found ~p ~n",[Res]),
	    ?stc:scrollToLine(Ed,?stc:lineFromPosition(Ed,Res) - 3),
	    true;
	false ->
	    false
    end.
   
keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).
