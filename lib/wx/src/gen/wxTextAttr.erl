%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html">wxTextAttr</a>.
%% @type wxTextAttr().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxTextAttr).
-include("wxe.hrl").
-export([destroy/1,getAlignment/1,getBackgroundColour/1,getFlags/1,getFont/1,
  getLeftIndent/1,getLeftSubIndent/1,getRightIndent/1,getTabs/1,getTextColour/1,
  hasBackgroundColour/1,hasFont/1,hasTextColour/1,isDefault/1,new/0,
  new/1,new/2,setAlignment/2,setBackgroundColour/2,setFlags/2,setFont/2,
  setFont/3,setLeftIndent/2,setLeftIndent/3,setRightIndent/2,setTabs/2,
  setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxTextAttr()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
new() ->
  wxe_util:construct(?wxTextAttr_new_0,
  <<>>).

%% @spec (ColText::wx:colour()) -> wxTextAttr()
%% @equiv new(ColText, [])
new(ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  new(ColText, []).

%% @spec (ColText::wx:colour(), [Option]) -> wxTextAttr()
%% Option = {colBack, wx:colour()} | {font, wxFont:wxFont()} | {alignment, WxTextAttrAlignment}
%% WxTextAttrAlignment = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
%%<br /> WxTextAttrAlignment is one of ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
new(ColText, Options)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4,is_list(Options) ->
  MOpts = fun({colBack, ColBack}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary,0:32>>|Acc];
          ({font, #wx_ref{type=FontT,ref=FontRef}}, Acc) ->   ?CLASS(FontT,wxFont),[<<2:32/?UI,FontRef:32/?UI>>|Acc];
          ({alignment, Alignment}, Acc) -> [<<3:32/?UI,Alignment:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxTextAttr_new_2,
  <<(wxe_util:colour_bin(ColText)):16/binary, BinOpt/binary>>).

%% @spec (This::wxTextAttr()) -> WxTextAttrAlignment
%% WxTextAttrAlignment = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetalignment">external documentation</a>.
%%<br /> WxTextAttrAlignment is one of ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
getAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetAlignment,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetbackgroundcolour">external documentation</a>.
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetleftindent">external documentation</a>.
getLeftIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetLeftIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetleftsubindent">external documentation</a>.
getLeftSubIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetLeftSubIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetrightindent">external documentation</a>.
getRightIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetRightIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> [integer()]
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgettabs">external documentation</a>.
getTabs(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetTabs,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgettextcolour">external documentation</a>.
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrhasbackgroundcolour">external documentation</a>.
hasBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_HasBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrhasfont">external documentation</a>.
hasFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_HasFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrhastextcolour">external documentation</a>.
hasTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_HasTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrgetflags">external documentation</a>.
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_GetFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrisdefault">external documentation</a>.
isDefault(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:call(?wxTextAttr_IsDefault,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTextAttr(), Alignment::WxTextAttrAlignment) -> ok
%% WxTextAttrAlignment = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetalignment">external documentation</a>.
%%<br /> WxTextAttrAlignment is one of ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
setAlignment(#wx_ref{type=ThisT,ref=ThisRef},Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetAlignment,
  <<ThisRef:32/?UI,Alignment:32/?UI>>).

%% @spec (This::wxTextAttr(), ColBack::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetbackgroundcolour">external documentation</a>.
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @spec (This::wxTextAttr(), Flags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetflags">external documentation</a>.
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @spec (This::wxTextAttr(), Font::wxFont:wxFont()) -> ok
%% @equiv setFont(This,Font, [])
setFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  setFont(This,Font, []).

%% @spec (This::wxTextAttr(), Font::wxFont:wxFont(), [Option]) -> ok
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  ?CLASS(FontT,wxFont),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTextAttr_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxTextAttr(), Indent::integer()) -> ok
%% @equiv setLeftIndent(This,Indent, [])
setLeftIndent(This,Indent)
 when is_record(This, wx_ref),is_integer(Indent) ->
  setLeftIndent(This,Indent, []).

%% @spec (This::wxTextAttr(), Indent::integer(), [Option]) -> ok
%% Option = {subIndent, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetleftindent">external documentation</a>.
setLeftIndent(#wx_ref{type=ThisT,ref=ThisRef},Indent, Options)
 when is_integer(Indent),is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  MOpts = fun({subIndent, SubIndent}, Acc) -> [<<1:32/?UI,SubIndent:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTextAttr_SetLeftIndent,
  <<ThisRef:32/?UI,Indent:32/?UI, BinOpt/binary>>).

%% @spec (This::wxTextAttr(), Indent::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsetrightindent">external documentation</a>.
setRightIndent(#wx_ref{type=ThisT,ref=ThisRef},Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetRightIndent,
  <<ThisRef:32/?UI,Indent:32/?UI>>).

%% @spec (This::wxTextAttr(), Tabs::[integer()]) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsettabs">external documentation</a>.
setTabs(#wx_ref{type=ThisT,ref=ThisRef},Tabs)
 when is_list(Tabs) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetTabs,
  <<ThisRef:32/?UI,(length(Tabs)):32/?UI,
        (<< <<C:32/?I>> || C <- Tabs>>)/binary, 0:(((0+length(Tabs)) rem 2)*32)>>).

%% @spec (This::wxTextAttr(), ColText::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtextattr.html#wxtextattrsettextcolour">external documentation</a>.
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:cast(?wxTextAttr_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @spec (This::wxTextAttr()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextAttr),
  wxe_util:destroy(?wxTextAttr_destroy,Obj),
  ok.
