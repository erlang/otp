%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

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

-type wxTextAttr() :: wx:wx_object().
-export_type([wxTextAttr/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
-spec new() -> wxTextAttr().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTextAttr_new_0),
  wxe_util:rec(?wxTextAttr_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
%% <br /> Also:<br />
%% new(Attr) -> wxTextAttr() when<br />
%% 	Attr::wxTextAttr().<br />
%% 
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec new(ColText) -> wxTextAttr() when
	ColText::wx:wx_colour();
      (Attr) -> wxTextAttr() when
	Attr::wxTextAttr().

new(ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  new(ColText, []);
new(#wx_ref{type=AttrT}=Attr) ->
  ?CLASS(AttrT,wxTextAttr),
  wxe_util:queue_cmd(Attr,?get_env(),?wxTextAttr_new_1),
  wxe_util:rec(?wxTextAttr_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec new(ColText, [Option]) -> wxTextAttr() when
	ColText::wx:wx_colour(),
	Option :: {'colBack', wx:wx_colour()}
		 | {'font', wxFont:wxFont()}
		 | {'alignment', wx:wx_enum()}.
new(ColText, Options)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4,is_list(Options) ->
  MOpts = fun({colBack, ColBack}) -> {colBack,wxe_util:color(ColBack)};
          ({font, #wx_ref{type=FontT}} = Arg) ->   ?CLASS(FontT,wxFont),Arg;
          ({alignment, _alignment} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wxe_util:color(ColText), Opts,?get_env(),?wxTextAttr_new_2),
  wxe_util:rec(?wxTextAttr_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetalignment">external documentation</a>.
%%<br /> Res = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec getAlignment(This) -> wx:wx_enum() when
	This::wxTextAttr().
getAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetAlignment),
  wxe_util:rec(?wxTextAttr_GetAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxTextAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetBackgroundColour),
  wxe_util:rec(?wxTextAttr_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxTextAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFont),
  wxe_util:rec(?wxTextAttr_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetleftindent">external documentation</a>.
-spec getLeftIndent(This) -> integer() when
	This::wxTextAttr().
getLeftIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetLeftIndent),
  wxe_util:rec(?wxTextAttr_GetLeftIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetleftsubindent">external documentation</a>.
-spec getLeftSubIndent(This) -> integer() when
	This::wxTextAttr().
getLeftSubIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetLeftSubIndent),
  wxe_util:rec(?wxTextAttr_GetLeftSubIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetrightindent">external documentation</a>.
-spec getRightIndent(This) -> integer() when
	This::wxTextAttr().
getRightIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetRightIndent),
  wxe_util:rec(?wxTextAttr_GetRightIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgettabs">external documentation</a>.
-spec getTabs(This) -> [integer()] when
	This::wxTextAttr().
getTabs(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetTabs),
  wxe_util:rec(?wxTextAttr_GetTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxTextAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetTextColour),
  wxe_util:rec(?wxTextAttr_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhasbackgroundcolour">external documentation</a>.
-spec hasBackgroundColour(This) -> boolean() when
	This::wxTextAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasBackgroundColour),
  wxe_util:rec(?wxTextAttr_HasBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhasfont">external documentation</a>.
-spec hasFont(This) -> boolean() when
	This::wxTextAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasFont),
  wxe_util:rec(?wxTextAttr_HasFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhastextcolour">external documentation</a>.
-spec hasTextColour(This) -> boolean() when
	This::wxTextAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasTextColour),
  wxe_util:rec(?wxTextAttr_HasTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxTextAttr().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFlags),
  wxe_util:rec(?wxTextAttr_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrisdefault">external documentation</a>.
-spec isDefault(This) -> boolean() when
	This::wxTextAttr().
isDefault(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_IsDefault),
  wxe_util:rec(?wxTextAttr_IsDefault).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetalignment">external documentation</a>.
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec setAlignment(This, Alignment) -> 'ok' when
	This::wxTextAttr(), Alignment::wx:wx_enum().
setAlignment(#wx_ref{type=ThisT}=This,Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Alignment,?get_env(),?wxTextAttr_SetAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxTextAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxTextAttr_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxTextAttr(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxTextAttr_SetFlags).

%% @equiv setFont(This,Font, [])
-spec setFont(This, Font) -> 'ok' when
	This::wxTextAttr(), Font::wxFont:wxFont().

setFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  setFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfont">external documentation</a>.
-spec setFont(This, Font, [Option]) -> 'ok' when
	This::wxTextAttr(), Font::wxFont:wxFont(),
	Option :: {'flags', integer()}.
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  ?CLASS(FontT,wxFont),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Font, Opts,?get_env(),?wxTextAttr_SetFont).

%% @equiv setLeftIndent(This,Indent, [])
-spec setLeftIndent(This, Indent) -> 'ok' when
	This::wxTextAttr(), Indent::integer().

setLeftIndent(This,Indent)
 when is_record(This, wx_ref),is_integer(Indent) ->
  setLeftIndent(This,Indent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetleftindent">external documentation</a>.
-spec setLeftIndent(This, Indent, [Option]) -> 'ok' when
	This::wxTextAttr(), Indent::integer(),
	Option :: {'subIndent', integer()}.
setLeftIndent(#wx_ref{type=ThisT}=This,Indent, Options)
 when is_integer(Indent),is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  MOpts = fun({subIndent, _subIndent} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Indent, Opts,?get_env(),?wxTextAttr_SetLeftIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetrightindent">external documentation</a>.
-spec setRightIndent(This, Indent) -> 'ok' when
	This::wxTextAttr(), Indent::integer().
setRightIndent(#wx_ref{type=ThisT}=This,Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Indent,?get_env(),?wxTextAttr_SetRightIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsettabs">external documentation</a>.
-spec setTabs(This, Tabs) -> 'ok' when
	This::wxTextAttr(), Tabs::[integer()].
setTabs(#wx_ref{type=ThisT}=This,Tabs)
 when is_list(Tabs) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Tabs,?get_env(),?wxTextAttr_SetTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxTextAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxTextAttr_SetTextColour).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTextAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxTextAttr_destroy),
  ok.
