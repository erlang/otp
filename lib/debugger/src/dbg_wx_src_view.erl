%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(dbg_wx_src_view).

-export([code_area/2]).
-include_lib("wx/include/wx.hrl").

-define(stc, wxStyledTextCtrl).

code_area(Parent, Sizer) ->
    FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    Ed = wxStyledTextCtrl:new(Parent, [{size, {400, 500}}]),
    ?stc:styleClearAll(Ed),
    ?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    ?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    ?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
    LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "999"),
    ?stc:setMarginWidth(Ed, 0, LW),
    
    ?stc:setReadOnly(Ed, true),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {222,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {170,110,50}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {238,80,239}},
	       {?wxSTC_ERLANG_STRING,   {236,155,172}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {92,194,241}},
	       {?wxSTC_ERLANG_RECORD,   {60,150,40}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}],
    SetStyle = fun({Style, Color}) ->
		       ?stc:styleSetFont(Ed, Style, FixedFont),
		       ?stc:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    ?stc:setKeyWords(Ed, 0, keyWords()),
    _ = wxSizer:add(Sizer, Ed, [{proportion,1}, {flag, ?wxEXPAND}]),
    Ed.


keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).
