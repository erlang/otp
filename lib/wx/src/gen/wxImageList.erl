%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxImageList).
-moduledoc """
A `m:wxImageList` contains a list of images, which are stored in an unspecified form.

Images can have masks for transparent drawing, and can be made from a variety of sources
including bitmaps and icons.

`m:wxImageList` is used principally in conjunction with `m:wxTreeCtrl` and `m:wxListCtrl` classes.

See:
* `m:wxTreeCtrl`

* `m:wxListCtrl`

wxWidgets docs: [wxImageList](https://docs.wxwidgets.org/3.2/classwx_image_list.html)
""".
-include("wxe.hrl").
-export([add/2,add/3,create/3,create/4,destroy/1,draw/5,draw/6,getBitmap/2,getIcon/2,
  getImageCount/1,getSize/2,new/0,new/2,new/3,remove/2,removeAll/1,replace/3,
  replace/4]).

%% inherited exports
-export([parent_class/1]).

-type wxImageList() :: wx:wx_object().
-export_type([wxImageList/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxImageList().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxImageList_new_0),
  wxe_util:rec(?wxImageList_new_0).

-doc(#{equiv => new(Width,Height, [])}).
-spec new(Width, Height) -> wxImageList() when
	Width::integer(), Height::integer().

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []).

-doc """
Constructor specifying the image size, whether image masks should be created, and the
initial size of the list.

See: `create/4`
""".
-spec new(Width, Height, [Option]) -> wxImageList() when
	Width::integer(), Height::integer(),
	Option :: {'mask', boolean()}
		 | {'initialCount', integer()}.
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({mask, _mask} = Arg) -> Arg;
          ({initialCount, _initialCount} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height, Opts,?get_env(),?wxImageList_new_3),
  wxe_util:rec(?wxImageList_new_3).

-doc """
Adds a new image using an icon.

Return: The new zero-based image index.

Remark: The original bitmap or icon is not affected by the `add/3` operation, and can be deleted
afterwards. If the bitmap is wider than the images in the list, then the bitmap will
automatically be split into smaller images, each matching the dimensions of the image
list. This does not apply when adding icons.

Only for:wxmsw,wxosx
""".
-spec add(This, Icon) -> integer() when
	This::wxImageList(), Icon::wxIcon:wxIcon() | wxBitmap:wxBitmap().
add(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxImageList),
  IswxIcon = ?CLASS_T(IconT,wxIcon),
  IswxBitmap = ?CLASS_T(IconT,wxBitmap),
  IconType = if
    IswxIcon ->   wxIcon;
    IswxBitmap ->   wxBitmap;
    true -> error({badarg, IconT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Icon, IconType),?get_env(),?wxImageList_Add_1),
  wxe_util:rec(?wxImageList_Add_1).

-doc """
Adds a new image or images using a bitmap and mask colour.

Return: The new zero-based image index.

Remark: The original bitmap or icon is not affected by the `add/3` operation, and can be deleted
afterwards. If the bitmap is wider than the images in the list, then the bitmap will
automatically be split into smaller images, each matching the dimensions of the image
list. This does not apply when adding icons.
""".
-spec add(This, Bitmap, Mask) -> integer() when
	This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap();
      (This, Bitmap, MaskColour) -> integer() when
	This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), MaskColour::wx:wx_colour().
add(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap,#wx_ref{type=MaskT}=Mask) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,Mask,?get_env(),?wxImageList_Add_2_0),
  wxe_util:rec(?wxImageList_Add_2_0);
add(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap,MaskColour)
 when ?is_colordata(MaskColour) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,wxe_util:color(MaskColour),?get_env(),?wxImageList_Add_2_1),
  wxe_util:rec(?wxImageList_Add_2_1).

-doc(#{equiv => create(This,Width,Height, [])}).
-spec create(This, Width, Height) -> boolean() when
	This::wxImageList(), Width::integer(), Height::integer().

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

-doc """
Initializes the list.

See `new/3` for details.
""".
-spec create(This, Width, Height, [Option]) -> boolean() when
	This::wxImageList(), Width::integer(), Height::integer(),
	Option :: {'mask', boolean()}
		 | {'initialCount', integer()}.
create(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  MOpts = fun({mask, _mask} = Arg) -> Arg;
          ({initialCount, _initialCount} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImageList_Create),
  wxe_util:rec(?wxImageList_Create).

-doc(#{equiv => draw(This,Index,Dc,X,Y, [])}).
-spec draw(This, Index, Dc, X, Y) -> boolean() when
	This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer().

draw(This,Index,Dc,X,Y)
 when is_record(This, wx_ref),is_integer(Index),is_record(Dc, wx_ref),is_integer(X),is_integer(Y) ->
  draw(This,Index,Dc,X,Y, []).

-doc "Draws a specified image onto a device context.".
-spec draw(This, Index, Dc, X, Y, [Option]) -> boolean() when
	This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer(),
	Option :: {'flags', integer()}
		 | {'solidBackground', boolean()}.
draw(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=DcT}=Dc,X,Y, Options)
 when is_integer(Index),is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(DcT,wxDC),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({solidBackground, _solidBackground} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index,Dc,X,Y, Opts,?get_env(),?wxImageList_Draw),
  wxe_util:rec(?wxImageList_Draw).

-doc "Returns the bitmap corresponding to the given index.".
-spec getBitmap(This, Index) -> wxBitmap:wxBitmap() when
	This::wxImageList(), Index::integer().
getBitmap(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxImageList_GetBitmap),
  wxe_util:rec(?wxImageList_GetBitmap).

-doc "Returns the icon corresponding to the given index.".
-spec getIcon(This, Index) -> wxIcon:wxIcon() when
	This::wxImageList(), Index::integer().
getIcon(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxImageList_GetIcon),
  wxe_util:rec(?wxImageList_GetIcon).

-doc "Returns the number of images in the list.".
-spec getImageCount(This) -> integer() when
	This::wxImageList().
getImageCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,?get_env(),?wxImageList_GetImageCount),
  wxe_util:rec(?wxImageList_GetImageCount).

-doc """
Retrieves the size of the images in the list.

Currently, the `index` parameter is ignored as all images in the list have the same size.

Return: true if the function succeeded, false if it failed (for example, if the image
list was not yet initialized).
""".
-spec getSize(This, Index) -> Result when
	Result ::{Res ::boolean(), Width::integer(), Height::integer()},
	This::wxImageList(), Index::integer().
getSize(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxImageList_GetSize),
  wxe_util:rec(?wxImageList_GetSize).

-doc "Removes the image at the given position.".
-spec remove(This, Index) -> boolean() when
	This::wxImageList(), Index::integer().
remove(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxImageList_Remove),
  wxe_util:rec(?wxImageList_Remove).

-doc "Removes all the images in the list.".
-spec removeAll(This) -> boolean() when
	This::wxImageList().
removeAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:queue_cmd(This,?get_env(),?wxImageList_RemoveAll),
  wxe_util:rec(?wxImageList_RemoveAll).

-doc """
Replaces the existing image with the new image.

Return: true if the replacement was successful, false otherwise.

Remark: The original bitmap or icon is not affected by the `replace/4` operation, and can be deleted afterwards.

Only for:wxmsw,wxosx
""".
-spec replace(This, Index, Icon) -> boolean() when
	This::wxImageList(), Index::integer(), Icon::wxIcon:wxIcon() | wxBitmap:wxBitmap().
replace(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=IconT}=Icon)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  IswxIcon = ?CLASS_T(IconT,wxIcon),
  IswxBitmap = ?CLASS_T(IconT,wxBitmap),
  IconType = if
    IswxIcon ->   wxIcon;
    IswxBitmap ->   wxBitmap;
    true -> error({badarg, IconT})
  end,
  wxe_util:queue_cmd(This,Index,wx:typeCast(Icon, IconType),?get_env(),?wxImageList_Replace_2),
  wxe_util:rec(?wxImageList_Replace_2).

-doc """
Replaces the existing image with the new image.

Windows only.

Return: true if the replacement was successful, false otherwise.

Remark: The original bitmap or icon is not affected by the `replace/4` operation, and can be deleted
afterwards.
""".
-spec replace(This, Index, Bitmap, Mask) -> boolean() when
	This::wxImageList(), Index::integer(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap().
replace(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=BitmapT}=Bitmap,#wx_ref{type=MaskT}=Mask)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:queue_cmd(This,Index,Bitmap,Mask,?get_env(),?wxImageList_Replace_3),
  wxe_util:rec(?wxImageList_Replace_3).

-doc "Destroys the object".
-spec destroy(This::wxImageList()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImageList),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
