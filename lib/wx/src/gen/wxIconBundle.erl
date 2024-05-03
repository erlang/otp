%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxIconBundle).
-moduledoc """
Functions for wxIconBundle class

This class contains multiple copies of an icon in different sizes. It is
typically used in `wxDialog::SetIcons` (not implemented in wx) and
`wxTopLevelWindow:setIcons/2`.

Predefined objects (include wx.hrl): ?wxNullIconBundle

wxWidgets docs:
[wxIconBundle](https://docs.wxwidgets.org/3.1/classwx_icon_bundle.html)
""".
-include("wxe.hrl").
-export([addIcon/2,addIcon/3,destroy/1,getIcon/1,getIcon/2,getIcon/3,new/0,new/1,
  new/2]).

%% inherited exports
-export([parent_class/1]).

-type wxIconBundle() :: wx:wx_object().
-export_type([wxIconBundle/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-doc "Default ctor.".
-spec new() -> wxIconBundle().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxIconBundle_new_0),
  wxe_util:rec(?wxIconBundle_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
%% <br /> Also:<br />
%% new(File) -> wxIconBundle() when<br />
%% 	File::unicode:chardata().<br />
%% 
-doc "Initializes the bundle with the icon(s) found in the file.".
-spec new(Ic) -> wxIconBundle() when
	Ic::wxIconBundle:wxIconBundle() | wxIcon:wxIcon();
      (File) -> wxIconBundle() when
	File::unicode:chardata().
new(#wx_ref{type=IcT}=Ic) ->
  IswxIconBundle = ?CLASS_T(IcT,wxIconBundle),
  IswxIcon = ?CLASS_T(IcT,wxIcon),
  IcType = if
    IswxIconBundle ->   wxIconBundle;
    IswxIcon ->   wxIcon;
    true -> error({badarg, IcT})
  end,
  wxe_util:queue_cmd(wx:typeCast(Ic, IcType),?get_env(),?wxIconBundle_new_1_0),
  wxe_util:rec(?wxIconBundle_new_1_0);
new(File)
 when ?is_chardata(File) ->
  File_UC = unicode:characters_to_binary(File),
  wxe_util:queue_cmd(File_UC,?get_env(),?wxIconBundle_new_1_1),
  wxe_util:rec(?wxIconBundle_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(File, Type) -> wxIconBundle() when
	File::unicode:chardata(), Type::wx:wx_enum().
new(File,Type)
 when ?is_chardata(File),is_integer(Type) ->
  File_UC = unicode:characters_to_binary(File),
  wxe_util:queue_cmd(File_UC,Type,?get_env(),?wxIconBundle_new_2),
  wxe_util:rec(?wxIconBundle_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
%% <br /> Also:<br />
%% addIcon(This, Icon) -> 'ok' when<br />
%% 	This::wxIconBundle(), Icon::wxIcon:wxIcon().<br />
%% 
-doc """
Adds the icon to the collection; if the collection already contains an icon with
the same width and height, it is replaced by the new one.
""".
-spec addIcon(This, File) -> 'ok' when
	This::wxIconBundle(), File::unicode:chardata();
      (This, Icon) -> 'ok' when
	This::wxIconBundle(), Icon::wxIcon:wxIcon().
addIcon(#wx_ref{type=ThisT}=This,File)
 when ?is_chardata(File) ->
  ?CLASS(ThisT,wxIconBundle),
  File_UC = unicode:characters_to_binary(File),
  wxe_util:queue_cmd(This,File_UC,?get_env(),?wxIconBundle_AddIcon_1_0);
addIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxIconBundle),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxIconBundle_AddIcon_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec addIcon(This, File, Type) -> 'ok' when
	This::wxIconBundle(), File::unicode:chardata(), Type::wx:wx_enum().
addIcon(#wx_ref{type=ThisT}=This,File,Type)
 when ?is_chardata(File),is_integer(Type) ->
  ?CLASS(ThisT,wxIconBundle),
  File_UC = unicode:characters_to_binary(File),
  wxe_util:queue_cmd(This,File_UC,Type,?get_env(),?wxIconBundle_AddIcon_2).

%% @equiv getIcon(This, [])
-spec getIcon(This) -> wxIcon:wxIcon() when
	This::wxIconBundle().

getIcon(This)
 when is_record(This, wx_ref) ->
  getIcon(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlegeticon">external documentation</a>.
%% <br /> Also:<br />
%% getIcon(This, [Option]) -> wxIcon:wxIcon() when<br />
%% 	This::wxIconBundle(),<br />
%% 	Option :: {'size', integer()}<br />
%% 		 | {'flags', integer()}.<br />
%% 
-doc """
Same as.

.
""".
-spec getIcon(This, Size) -> wxIcon:wxIcon() when
	This::wxIconBundle(), Size::{W::integer(), H::integer()};
      (This, [Option]) -> wxIcon:wxIcon() when
	This::wxIconBundle(),
	Option :: {'size', integer()}
		 | {'flags', integer()}.

getIcon(This,{SizeW,SizeH} = Size)
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH) ->
  getIcon(This,Size, []);
getIcon(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIconBundle),
  MOpts = fun({size, _size} = Arg) -> Arg;
          ({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxIconBundle_GetIcon_1),
  wxe_util:rec(?wxIconBundle_GetIcon_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlegeticon">external documentation</a>.
-doc """
Returns the icon with the given size.

If `size` is ?wxDefaultSize, it is interpreted as the standard system icon size,
i.e. the size returned by `wxSystemSettings:getMetric/2` for `wxSYS_ICON_X` and
`wxSYS_ICON_Y`.

If the bundle contains an icon with exactly the requested size, it's always
returned. Otherwise, the behaviour depends on the flags. If only
`wxIconBundle::FALLBACK_NONE` (not implemented in wx) is given, the function
returns an invalid icon. If `wxIconBundle::FALLBACK_SYSTEM` (not implemented in
wx) is given, it tries to find the icon of standard system size, regardless of
the size passed as parameter. Otherwise, or if the icon system size is not found
neither, but `wxIconBundle::FALLBACK_NEAREST_LARGER` (not implemented in wx)
flag is specified, the function returns the smallest icon of the size larger
than the requested one or, if this fails too, just the icon closest to the
specified size.

The `flags` parameter is available only since wxWidgets 2.9.4.
""".
-spec getIcon(This, Size, [Option]) -> wxIcon:wxIcon() when
	This::wxIconBundle(), Size::{W::integer(), H::integer()},
	Option :: {'flags', integer()}.
getIcon(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size, Options)
 when is_integer(SizeW),is_integer(SizeH),is_list(Options) ->
  ?CLASS(ThisT,wxIconBundle),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Size, Opts,?get_env(),?wxIconBundle_GetIcon_2),
  wxe_util:rec(?wxIconBundle_GetIcon_2).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxIconBundle()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIconBundle),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxIconBundle_destruct),
  ok.
