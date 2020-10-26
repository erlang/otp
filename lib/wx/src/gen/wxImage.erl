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

%%
%% All (default) image handlers are initialized.

%%
-module(wxImage).
-include("wxe.hrl").
-export(['Destroy'/1,blur/2,blurHorizontal/2,blurVertical/2,convertAlphaToMask/1,
  convertAlphaToMask/2,convertAlphaToMask/4,convertAlphaToMask/5,convertToGreyscale/1,
  convertToGreyscale/4,convertToMono/4,copy/1,create/2,create/3,create/4,
  create/5,create/6,destroy/1,findFirstUnusedColour/1,findFirstUnusedColour/2,
  getAlpha/1,getAlpha/3,getBlue/3,getData/1,getGreen/3,getHeight/1,getImageCount/1,
  getImageCount/2,getImageExtWildcard/0,getMaskBlue/1,getMaskGreen/1,
  getMaskRed/1,getOption/2,getOptionInt/2,getOrFindMaskColour/1,getPalette/1,
  getRed/3,getSubImage/2,getWidth/1,hasAlpha/1,hasMask/1,hasOption/2,
  initAlpha/1,initStandardHandlers/0,isOk/1,isTransparent/3,isTransparent/4,
  loadFile/2,loadFile/3,loadFile/4,mirror/1,mirror/2,new/0,new/1,new/2,
  new/3,new/4,new/5,ok/1,removeHandler/1,replace/7,rescale/3,rescale/4,
  resize/3,resize/4,rotate/3,rotate/4,rotate90/1,rotate90/2,rotateHue/2,
  saveFile/2,saveFile/3,scale/3,scale/4,setAlpha/2,setAlpha/3,setAlpha/4,
  setData/2,setData/3,setData/4,setData/5,setMask/1,setMask/2,setMaskColour/4,
  setMaskFromImage/5,setOption/3,setPalette/2,setRGB/5,setRGB/6,size/3,
  size/4]).

%% inherited exports
-export([parent_class/1]).

-type wxImage() :: wx:wx_object().
-export_type([wxImage/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
-spec new() -> wxImage().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_new_0),
  wxe_util:rec(?wxImage_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Also:<br />
%% new(Sz) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()}.<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Name) -> wxImage() when
	Name::unicode:chardata();
      (Sz) -> wxImage() when
	Sz::{W::integer(), H::integer()}.

new(Name)
 when ?is_chardata(Name) ->
  new(Name, []);

new({SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  new(Sz, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Also:<br />
%% new(Sz, Data) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()}, Data::binary();<br />
%%       (Name, [Option]) -> wxImage() when<br />
%% 	Name::unicode:chardata(),<br />
%% 	Option :: {'type', wx:wx_enum()}<br />
%% 		 | {'index', integer()};<br />
%%       (Sz, [Option]) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()},<br />
%% 	Option :: {'clear', boolean()}.<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec new(Width, Height) -> wxImage() when
	Width::integer(), Height::integer();
      (Sz, Data) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary();
      (Name, [Option]) -> wxImage() when
	Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'index', integer()};
      (Sz, [Option]) -> wxImage() when
	Sz::{W::integer(), H::integer()},
	Option :: {'clear', boolean()}.

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);

new({SzW,SzH} = Sz,Data)
 when is_integer(SzW),is_integer(SzH),is_binary(Data) ->
  new(Sz,Data, []);
new(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxImage_new_2_0),
  wxe_util:rec(?wxImage_new_2_0);
new({SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Sz, Opts,?get_env(),?wxImage_new_2_1),
  wxe_util:rec(?wxImage_new_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Also:<br />
%% new(Sz, Data, Alpha) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary();<br />
%%       (Width, Height, [Option]) -> wxImage() when<br />
%% 	Width::integer(), Height::integer(),<br />
%% 	Option :: {'clear', boolean()};<br />
%%       (Name, Mimetype, [Option]) -> wxImage() when<br />
%% 	Name::unicode:chardata(), Mimetype::unicode:chardata(),<br />
%% 	Option :: {'index', integer()};<br />
%%       (Sz, Data, [Option]) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()}, Data::binary(),<br />
%% 	Option :: {'static_data', boolean()}.<br />
%% 
-spec new(Width, Height, Data) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary();
      (Sz, Data, Alpha) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary();
      (Width, Height, [Option]) -> wxImage() when
	Width::integer(), Height::integer(),
	Option :: {'clear', boolean()};
      (Name, Mimetype, [Option]) -> wxImage() when
	Name::unicode:chardata(), Mimetype::unicode:chardata(),
	Option :: {'index', integer()};
      (Sz, Data, [Option]) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary(),
	Option :: {'static_data', boolean()}.

new(Width,Height,Data)
 when is_integer(Width),is_integer(Height),is_binary(Data) ->
  new(Width,Height,Data, []);

new({SzW,SzH} = Sz,Data,Alpha)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha) ->
  new(Sz,Data,Alpha, []);
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height, Opts,?get_env(),?wxImage_new_3_0),
  wxe_util:rec(?wxImage_new_3_0);
new(Name,Mimetype, Options)
 when ?is_chardata(Name),?is_chardata(Mimetype),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  MOpts = fun({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC,Mimetype_UC, Opts,?get_env(),?wxImage_new_3_1),
  wxe_util:rec(?wxImage_new_3_1);
new({SzW,SzH} = Sz,Data, Options)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_list(Options) ->
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Sz,Data, Opts,?get_env(),?wxImage_new_3_2),
  wxe_util:rec(?wxImage_new_3_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Also:<br />
%% new(Width, Height, Data, [Option]) -> wxImage() when<br />
%% 	Width::integer(), Height::integer(), Data::binary(),<br />
%% 	Option :: {'static_data', boolean()};<br />
%%       (Sz, Data, Alpha, [Option]) -> wxImage() when<br />
%% 	Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary(),<br />
%% 	Option :: {'static_data', boolean()}.<br />
%% 
-spec new(Width, Height, Data, Alpha) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary(), Alpha::binary();
      (Width, Height, Data, [Option]) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary(),
	Option :: {'static_data', boolean()};
      (Sz, Data, Alpha, [Option]) -> wxImage() when
	Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary(),
	Option :: {'static_data', boolean()}.

new(Width,Height,Data,Alpha)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  new(Width,Height,Data,Alpha, []);
new(Width,Height,Data, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_list(Options) ->
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height,Data, Opts,?get_env(),?wxImage_new_4_0),
  wxe_util:rec(?wxImage_new_4_0);
new({SzW,SzH} = Sz,Data,Alpha, Options)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Sz,Data,Alpha, Opts,?get_env(),?wxImage_new_4_1),
  wxe_util:rec(?wxImage_new_4_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagewximage">external documentation</a>.
-spec new(Width, Height, Data, Alpha, [Option]) -> wxImage() when
	Width::integer(), Height::integer(), Data::binary(), Alpha::binary(),
	Option :: {'static_data', boolean()}.
new(Width,Height,Data,Alpha, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height,Data,Alpha, Opts,?get_env(),?wxImage_new_5),
  wxe_util:rec(?wxImage_new_5).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageblur">external documentation</a>.
-spec blur(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blur(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_Blur),
  wxe_util:rec(?wxImage_Blur).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageblurhorizontal">external documentation</a>.
-spec blurHorizontal(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blurHorizontal(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_BlurHorizontal),
  wxe_util:rec(?wxImage_BlurHorizontal).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageblurvertical">external documentation</a>.
-spec blurVertical(This, BlurRadius) -> wxImage() when
	This::wxImage(), BlurRadius::integer().
blurVertical(#wx_ref{type=ThisT}=This,BlurRadius)
 when is_integer(BlurRadius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,BlurRadius,?get_env(),?wxImage_BlurVertical),
  wxe_util:rec(?wxImage_BlurVertical).

%% @equiv convertAlphaToMask(This, [])
-spec convertAlphaToMask(This) -> boolean() when
	This::wxImage().

convertAlphaToMask(This)
 when is_record(This, wx_ref) ->
  convertAlphaToMask(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageconvertalphatomask">external documentation</a>.
-spec convertAlphaToMask(This, [Option]) -> boolean() when
	This::wxImage(),
	Option :: {'threshold', integer()}.
convertAlphaToMask(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_ConvertAlphaToMask_1),
  wxe_util:rec(?wxImage_ConvertAlphaToMask_1).

%% @equiv convertAlphaToMask(This,Mr,Mg,Mb, [])
-spec convertAlphaToMask(This, Mr, Mg, Mb) -> boolean() when
	This::wxImage(), Mr::integer(), Mg::integer(), Mb::integer().

convertAlphaToMask(This,Mr,Mg,Mb)
 when is_record(This, wx_ref),is_integer(Mr),is_integer(Mg),is_integer(Mb) ->
  convertAlphaToMask(This,Mr,Mg,Mb, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageconvertalphatomask">external documentation</a>.
-spec convertAlphaToMask(This, Mr, Mg, Mb, [Option]) -> boolean() when
	This::wxImage(), Mr::integer(), Mg::integer(), Mb::integer(),
	Option :: {'threshold', integer()}.
convertAlphaToMask(#wx_ref{type=ThisT}=This,Mr,Mg,Mb, Options)
 when is_integer(Mr),is_integer(Mg),is_integer(Mb),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Mr,Mg,Mb, Opts,?get_env(),?wxImage_ConvertAlphaToMask_4),
  wxe_util:rec(?wxImage_ConvertAlphaToMask_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageconverttogreyscale">external documentation</a>.
-spec convertToGreyscale(This) -> wxImage() when
	This::wxImage().
convertToGreyscale(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_ConvertToGreyscale_0),
  wxe_util:rec(?wxImage_ConvertToGreyscale_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageconverttogreyscale">external documentation</a>.
-spec convertToGreyscale(This, Weight_r, Weight_g, Weight_b) -> wxImage() when
	This::wxImage(), Weight_r::number(), Weight_g::number(), Weight_b::number().
convertToGreyscale(#wx_ref{type=ThisT}=This,Weight_r,Weight_g,Weight_b)
 when is_number(Weight_r),is_number(Weight_g),is_number(Weight_b) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Weight_r,Weight_g,Weight_b,?get_env(),?wxImage_ConvertToGreyscale_3),
  wxe_util:rec(?wxImage_ConvertToGreyscale_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageconverttomono">external documentation</a>.
-spec convertToMono(This, R, G, B) -> wxImage() when
	This::wxImage(), R::integer(), G::integer(), B::integer().
convertToMono(#wx_ref{type=ThisT}=This,R,G,B)
 when is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,R,G,B,?get_env(),?wxImage_ConvertToMono),
  wxe_util:rec(?wxImage_ConvertToMono).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagecopy">external documentation</a>.
-spec copy(This) -> wxImage() when
	This::wxImage().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_Copy),
  wxe_util:rec(?wxImage_Copy).

%% @equiv create(This,Sz, [])
-spec create(This, Sz) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}.

create(This,{SzW,SzH} = Sz)
 when is_record(This, wx_ref),is_integer(SzW),is_integer(SzH) ->
  create(This,Sz, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagecreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Sz, Data) -> boolean() when<br />
%% 	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary();<br />
%%       (This, Sz, [Option]) -> boolean() when<br />
%% 	This::wxImage(), Sz::{W::integer(), H::integer()},<br />
%% 	Option :: {'clear', boolean()}.<br />
%% 
-spec create(This, Width, Height) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer();
      (This, Sz, Data) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary();
      (This, Sz, [Option]) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()},
	Option :: {'clear', boolean()}.

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []);

create(This,{SzW,SzH} = Sz,Data)
 when is_record(This, wx_ref),is_integer(SzW),is_integer(SzH),is_binary(Data) ->
  create(This,Sz,Data, []);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz, Options)
 when is_integer(SzW),is_integer(SzH),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sz, Opts,?get_env(),?wxImage_Create_2),
  wxe_util:rec(?wxImage_Create_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagecreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Sz, Data, Alpha) -> boolean() when<br />
%% 	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary();<br />
%%       (This, Width, Height, [Option]) -> boolean() when<br />
%% 	This::wxImage(), Width::integer(), Height::integer(),<br />
%% 	Option :: {'clear', boolean()};<br />
%%       (This, Sz, Data, [Option]) -> boolean() when<br />
%% 	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(),<br />
%% 	Option :: {'static_data', boolean()}.<br />
%% 
-spec create(This, Width, Height, Data) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary();
      (This, Sz, Data, Alpha) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary();
      (This, Width, Height, [Option]) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'clear', boolean()};
      (This, Sz, Data, [Option]) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(),
	Option :: {'static_data', boolean()}.

create(This,Width,Height,Data)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height),is_binary(Data) ->
  create(This,Width,Height,Data, []);

create(This,{SzW,SzH} = Sz,Data,Alpha)
 when is_record(This, wx_ref),is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha) ->
  create(This,Sz,Data,Alpha, []);
create(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clear, _clear} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Create_3_0),
  wxe_util:rec(?wxImage_Create_3_0);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz,Data, Options)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sz,Data, Opts,?get_env(),?wxImage_Create_3_1),
  wxe_util:rec(?wxImage_Create_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagecreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Width, Height, Data, [Option]) -> boolean() when<br />
%% 	This::wxImage(), Width::integer(), Height::integer(), Data::binary(),<br />
%% 	Option :: {'static_data', boolean()};<br />
%%       (This, Sz, Data, Alpha, [Option]) -> boolean() when<br />
%% 	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary(),<br />
%% 	Option :: {'static_data', boolean()}.<br />
%% 
-spec create(This, Width, Height, Data, Alpha) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary(), Alpha::binary();
      (This, Width, Height, Data, [Option]) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary(),
	Option :: {'static_data', boolean()};
      (This, Sz, Data, Alpha, [Option]) -> boolean() when
	This::wxImage(), Sz::{W::integer(), H::integer()}, Data::binary(), Alpha::binary(),
	Option :: {'static_data', boolean()}.

create(This,Width,Height,Data,Alpha)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  create(This,Width,Height,Data,Alpha, []);
create(#wx_ref{type=ThisT}=This,Width,Height,Data, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height,Data, Opts,?get_env(),?wxImage_Create_4_0),
  wxe_util:rec(?wxImage_Create_4_0);
create(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz,Data,Alpha, Options)
 when is_integer(SzW),is_integer(SzH),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sz,Data,Alpha, Opts,?get_env(),?wxImage_Create_4_1),
  wxe_util:rec(?wxImage_Create_4_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagecreate">external documentation</a>.
-spec create(This, Width, Height, Data, Alpha, [Option]) -> boolean() when
	This::wxImage(), Width::integer(), Height::integer(), Data::binary(), Alpha::binary(),
	Option :: {'static_data', boolean()}.
create(#wx_ref{type=ThisT}=This,Width,Height,Data,Alpha, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height,Data,Alpha, Opts,?get_env(),?wxImage_Create_5),
  wxe_util:rec(?wxImage_Create_5).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagedestroy">external documentation</a>.
-spec 'Destroy'(This) -> 'ok' when
	This::wxImage().
'Destroy'(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_Destroy).

%% @equiv findFirstUnusedColour(This, [])
-spec findFirstUnusedColour(This) -> Result when
	Result ::{Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage().

findFirstUnusedColour(This)
 when is_record(This, wx_ref) ->
  findFirstUnusedColour(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagefindfirstunusedcolour">external documentation</a>.
-spec findFirstUnusedColour(This, [Option]) -> Result when
	Result :: {Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage(),
	Option :: {'startR', integer()}
		 | {'startG', integer()}
		 | {'startB', integer()}.
findFirstUnusedColour(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({startR, _startR} = Arg) -> Arg;
          ({startG, _startG} = Arg) -> Arg;
          ({startB, _startB} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_FindFirstUnusedColour),
  wxe_util:rec(?wxImage_FindFirstUnusedColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetimageextwildcard">external documentation</a>.
-spec getImageExtWildcard() -> unicode:charlist().
getImageExtWildcard() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_GetImageExtWildcard),
  wxe_util:rec(?wxImage_GetImageExtWildcard).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetalpha">external documentation</a>.
-spec getAlpha(This) -> binary() when
	This::wxImage().
getAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetAlpha_0),
  wxe_util:rec(?wxImage_GetAlpha_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetalpha">external documentation</a>.
-spec getAlpha(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getAlpha(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetAlpha_2),
  wxe_util:rec(?wxImage_GetAlpha_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetblue">external documentation</a>.
-spec getBlue(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getBlue(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetBlue),
  wxe_util:rec(?wxImage_GetBlue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetdata">external documentation</a>.
-spec getData(This) -> binary() when
	This::wxImage().
getData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetData),
  wxe_util:rec(?wxImage_GetData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetgreen">external documentation</a>.
-spec getGreen(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getGreen(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetGreen),
  wxe_util:rec(?wxImage_GetGreen).

%% @equiv getImageCount(Filename, [])
-spec getImageCount(Filename) -> integer() when
	Filename::unicode:chardata().

getImageCount(Filename)
 when ?is_chardata(Filename) ->
  getImageCount(Filename, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetimagecount">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec getImageCount(Filename, [Option]) -> integer() when
	Filename::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}.
getImageCount(Filename, Options)
 when ?is_chardata(Filename),is_list(Options) ->
  Filename_UC = unicode:characters_to_binary(Filename),
  MOpts = fun({type, _type} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Filename_UC, Opts,?get_env(),?wxImage_GetImageCount),
  wxe_util:rec(?wxImage_GetImageCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetheight">external documentation</a>.
-spec getHeight(This) -> integer() when
	This::wxImage().
getHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetHeight),
  wxe_util:rec(?wxImage_GetHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetmaskblue">external documentation</a>.
-spec getMaskBlue(This) -> integer() when
	This::wxImage().
getMaskBlue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskBlue),
  wxe_util:rec(?wxImage_GetMaskBlue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetmaskgreen">external documentation</a>.
-spec getMaskGreen(This) -> integer() when
	This::wxImage().
getMaskGreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskGreen),
  wxe_util:rec(?wxImage_GetMaskGreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetmaskred">external documentation</a>.
-spec getMaskRed(This) -> integer() when
	This::wxImage().
getMaskRed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetMaskRed),
  wxe_util:rec(?wxImage_GetMaskRed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetorfindmaskcolour">external documentation</a>.
-spec getOrFindMaskColour(This) -> Result when
	Result ::{Res ::boolean(), R::integer(), G::integer(), B::integer()},
	This::wxImage().
getOrFindMaskColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetOrFindMaskColour),
  wxe_util:rec(?wxImage_GetOrFindMaskColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetpalette">external documentation</a>.
-spec getPalette(This) -> wxPalette:wxPalette() when
	This::wxImage().
getPalette(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetPalette),
  wxe_util:rec(?wxImage_GetPalette).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetred">external documentation</a>.
-spec getRed(This, X, Y) -> integer() when
	This::wxImage(), X::integer(), Y::integer().
getRed(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxImage_GetRed),
  wxe_util:rec(?wxImage_GetRed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetsubimage">external documentation</a>.
-spec getSubImage(This, Rect) -> wxImage() when
	This::wxImage(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
getSubImage(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxImage_GetSubImage),
  wxe_util:rec(?wxImage_GetSubImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxImage().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_GetWidth),
  wxe_util:rec(?wxImage_GetWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagehasalpha">external documentation</a>.
-spec hasAlpha(This) -> boolean() when
	This::wxImage().
hasAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_HasAlpha),
  wxe_util:rec(?wxImage_HasAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagehasmask">external documentation</a>.
-spec hasMask(This) -> boolean() when
	This::wxImage().
hasMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_HasMask),
  wxe_util:rec(?wxImage_HasMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetoption">external documentation</a>.
-spec getOption(This, Name) -> unicode:charlist() when
	This::wxImage(), Name::unicode:chardata().
getOption(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_GetOption),
  wxe_util:rec(?wxImage_GetOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagegetoptionint">external documentation</a>.
-spec getOptionInt(This, Name) -> integer() when
	This::wxImage(), Name::unicode:chardata().
getOptionInt(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_GetOptionInt),
  wxe_util:rec(?wxImage_GetOptionInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagehasoption">external documentation</a>.
-spec hasOption(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().
hasOption(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_HasOption),
  wxe_util:rec(?wxImage_HasOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageinitalpha">external documentation</a>.
-spec initAlpha(This) -> 'ok' when
	This::wxImage().
initAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_InitAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageinitstandardhandlers">external documentation</a>.
-spec initStandardHandlers() -> 'ok'.
initStandardHandlers() ->
  wxe_util:queue_cmd(?get_env(), ?wxImage_InitStandardHandlers).

%% @equiv isTransparent(This,X,Y, [])
-spec isTransparent(This, X, Y) -> boolean() when
	This::wxImage(), X::integer(), Y::integer().

isTransparent(This,X,Y)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y) ->
  isTransparent(This,X,Y, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageistransparent">external documentation</a>.
-spec isTransparent(This, X, Y, [Option]) -> boolean() when
	This::wxImage(), X::integer(), Y::integer(),
	Option :: {'threshold', integer()}.
isTransparent(#wx_ref{type=ThisT}=This,X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, _threshold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y, Opts,?get_env(),?wxImage_IsTransparent),
  wxe_util:rec(?wxImage_IsTransparent).

%% @equiv loadFile(This,Name, [])
-spec loadFile(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().

loadFile(This,Name)
 when is_record(This, wx_ref),?is_chardata(Name) ->
  loadFile(This,Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageloadfile">external documentation</a>.
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec loadFile(This, Name, [Option]) -> boolean() when
	This::wxImage(), Name::unicode:chardata(),
	Option :: {'type', wx:wx_enum()}
		 | {'index', integer()}.
loadFile(#wx_ref{type=ThisT}=This,Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({type, _type} = Arg) -> Arg;
          ({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC, Opts,?get_env(),?wxImage_LoadFile_2),
  wxe_util:rec(?wxImage_LoadFile_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageloadfile">external documentation</a>.
-spec loadFile(This, Name, Mimetype, [Option]) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Mimetype::unicode:chardata(),
	Option :: {'index', integer()}.
loadFile(#wx_ref{type=ThisT}=This,Name,Mimetype, Options)
 when ?is_chardata(Name),?is_chardata(Mimetype),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  MOpts = fun({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC,Mimetype_UC, Opts,?get_env(),?wxImage_LoadFile_3),
  wxe_util:rec(?wxImage_LoadFile_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageisok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxImage().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxImage().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,?get_env(),?wxImage_IsOk),
  wxe_util:rec(?wxImage_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageremovehandler">external documentation</a>.
-spec removeHandler(Name) -> boolean() when
	Name::unicode:chardata().
removeHandler(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxImage_RemoveHandler),
  wxe_util:rec(?wxImage_RemoveHandler).

%% @equiv mirror(This, [])
-spec mirror(This) -> wxImage() when
	This::wxImage().

mirror(This)
 when is_record(This, wx_ref) ->
  mirror(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagemirror">external documentation</a>.
-spec mirror(This, [Option]) -> wxImage() when
	This::wxImage(),
	Option :: {'horizontally', boolean()}.
mirror(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({horizontally, _horizontally} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_Mirror),
  wxe_util:rec(?wxImage_Mirror).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagereplace">external documentation</a>.
-spec replace(This, R1, G1, B1, R2, G2, B2) -> 'ok' when
	This::wxImage(), R1::integer(), G1::integer(), B1::integer(), R2::integer(), G2::integer(), B2::integer().
replace(#wx_ref{type=ThisT}=This,R1,G1,B1,R2,G2,B2)
 when is_integer(R1),is_integer(G1),is_integer(B1),is_integer(R2),is_integer(G2),is_integer(B2) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,R1,G1,B1,R2,G2,B2,?get_env(),?wxImage_Replace).

%% @equiv rescale(This,Width,Height, [])
-spec rescale(This, Width, Height) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer().

rescale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  rescale(This,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagerescale">external documentation</a>.
%%<br /> Quality = ?wxIMAGE_QUALITY_NEAREST | ?wxIMAGE_QUALITY_BILINEAR | ?wxIMAGE_QUALITY_BICUBIC | ?wxIMAGE_QUALITY_BOX_AVERAGE | ?wxIMAGE_QUALITY_NORMAL | ?wxIMAGE_QUALITY_HIGH
-spec rescale(This, Width, Height, [Option]) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'quality', wx:wx_enum()}.
rescale(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, _quality} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Rescale),
  wxe_util:rec(?wxImage_Rescale).

%% @equiv resize(This,Size,Pos, [])
-spec resize(This, Size, Pos) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()}.

resize(This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos)
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  resize(This,Size,Pos, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximageresize">external documentation</a>.
-spec resize(This, Size, Pos, [Option]) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()},
	Option :: {'r', integer()}
		 | {'g', integer()}
		 | {'b', integer()}.
resize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, _r} = Arg) -> Arg;
          ({g, _g} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Size,Pos, Opts,?get_env(),?wxImage_Resize),
  wxe_util:rec(?wxImage_Resize).

%% @equiv rotate(This,Angle,RotationCentre, [])
-spec rotate(This, Angle, RotationCentre) -> wxImage() when
	This::wxImage(), Angle::number(), RotationCentre::{X::integer(), Y::integer()}.

rotate(This,Angle,{RotationCentreX,RotationCentreY} = RotationCentre)
 when is_record(This, wx_ref),is_number(Angle),is_integer(RotationCentreX),is_integer(RotationCentreY) ->
  rotate(This,Angle,RotationCentre, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagerotate">external documentation</a>.
-spec rotate(This, Angle, RotationCentre, [Option]) -> wxImage() when
	This::wxImage(), Angle::number(), RotationCentre::{X::integer(), Y::integer()},
	Option :: {'interpolating', boolean()}
		 | {'offset_after_rotation', {X::integer(), Y::integer()}}.
rotate(#wx_ref{type=ThisT}=This,Angle,{RotationCentreX,RotationCentreY} = RotationCentre, Options)
 when is_number(Angle),is_integer(RotationCentreX),is_integer(RotationCentreY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({interpolating, _interpolating} = Arg) -> Arg;
          ({offset_after_rotation, {_offset_after_rotationX,_offset_after_rotationY}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Angle,RotationCentre, Opts,?get_env(),?wxImage_Rotate),
  wxe_util:rec(?wxImage_Rotate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagerotatehue">external documentation</a>.
-spec rotateHue(This, Angle) -> 'ok' when
	This::wxImage(), Angle::number().
rotateHue(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxImage_RotateHue).

%% @equiv rotate90(This, [])
-spec rotate90(This) -> wxImage() when
	This::wxImage().

rotate90(This)
 when is_record(This, wx_ref) ->
  rotate90(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagerotate90">external documentation</a>.
-spec rotate90(This, [Option]) -> wxImage() when
	This::wxImage(),
	Option :: {'clockwise', boolean()}.
rotate90(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clockwise, _clockwise} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_Rotate90),
  wxe_util:rec(?wxImage_Rotate90).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesavefile">external documentation</a>.
-spec saveFile(This, Name) -> boolean() when
	This::wxImage(), Name::unicode:chardata().
saveFile(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxImage_SaveFile_1),
  wxe_util:rec(?wxImage_SaveFile_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesavefile">external documentation</a>.
%% <br /> Also:<br />
%% saveFile(This, Name, Mimetype) -> boolean() when<br />
%% 	This::wxImage(), Name::unicode:chardata(), Mimetype::unicode:chardata().<br />
%% 
%%<br /> Type = ?wxBITMAP_TYPE_INVALID | ?wxBITMAP_TYPE_BMP | ?wxBITMAP_TYPE_BMP_RESOURCE | ?wxBITMAP_TYPE_RESOURCE | ?wxBITMAP_TYPE_ICO | ?wxBITMAP_TYPE_ICO_RESOURCE | ?wxBITMAP_TYPE_CUR | ?wxBITMAP_TYPE_CUR_RESOURCE | ?wxBITMAP_TYPE_XBM | ?wxBITMAP_TYPE_XBM_DATA | ?wxBITMAP_TYPE_XPM | ?wxBITMAP_TYPE_XPM_DATA | ?wxBITMAP_TYPE_TIFF | ?wxBITMAP_TYPE_TIF | ?wxBITMAP_TYPE_TIFF_RESOURCE | ?wxBITMAP_TYPE_TIF_RESOURCE | ?wxBITMAP_TYPE_GIF | ?wxBITMAP_TYPE_GIF_RESOURCE | ?wxBITMAP_TYPE_PNG | ?wxBITMAP_TYPE_PNG_RESOURCE | ?wxBITMAP_TYPE_JPEG | ?wxBITMAP_TYPE_JPEG_RESOURCE | ?wxBITMAP_TYPE_PNM | ?wxBITMAP_TYPE_PNM_RESOURCE | ?wxBITMAP_TYPE_PCX | ?wxBITMAP_TYPE_PCX_RESOURCE | ?wxBITMAP_TYPE_PICT | ?wxBITMAP_TYPE_PICT_RESOURCE | ?wxBITMAP_TYPE_ICON | ?wxBITMAP_TYPE_ICON_RESOURCE | ?wxBITMAP_TYPE_ANI | ?wxBITMAP_TYPE_IFF | ?wxBITMAP_TYPE_TGA | ?wxBITMAP_TYPE_MACCURSOR | ?wxBITMAP_TYPE_MACCURSOR_RESOURCE | ?wxBITMAP_TYPE_ANY
-spec saveFile(This, Name, Type) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Type::wx:wx_enum();
      (This, Name, Mimetype) -> boolean() when
	This::wxImage(), Name::unicode:chardata(), Mimetype::unicode:chardata().
saveFile(#wx_ref{type=ThisT}=This,Name,Type)
 when ?is_chardata(Name),is_integer(Type) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,Type,?get_env(),?wxImage_SaveFile_2_0),
  wxe_util:rec(?wxImage_SaveFile_2_0);
saveFile(#wx_ref{type=ThisT}=This,Name,Mimetype)
 when ?is_chardata(Name),?is_chardata(Mimetype) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Mimetype_UC = unicode:characters_to_binary(Mimetype),
  wxe_util:queue_cmd(This,Name_UC,Mimetype_UC,?get_env(),?wxImage_SaveFile_2_1),
  wxe_util:rec(?wxImage_SaveFile_2_1).

%% @equiv scale(This,Width,Height, [])
-spec scale(This, Width, Height) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer().

scale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  scale(This,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagescale">external documentation</a>.
%%<br /> Quality = ?wxIMAGE_QUALITY_NEAREST | ?wxIMAGE_QUALITY_BILINEAR | ?wxIMAGE_QUALITY_BICUBIC | ?wxIMAGE_QUALITY_BOX_AVERAGE | ?wxIMAGE_QUALITY_NORMAL | ?wxIMAGE_QUALITY_HIGH
-spec scale(This, Width, Height, [Option]) -> wxImage() when
	This::wxImage(), Width::integer(), Height::integer(),
	Option :: {'quality', wx:wx_enum()}.
scale(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, _quality} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxImage_Scale),
  wxe_util:rec(?wxImage_Scale).

%% @equiv size(This,Size,Pos, [])
-spec size(This, Size, Pos) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()}.

size(This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos)
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  size(This,Size,Pos, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesize">external documentation</a>.
-spec size(This, Size, Pos, [Option]) -> wxImage() when
	This::wxImage(), Size::{W::integer(), H::integer()}, Pos::{X::integer(), Y::integer()},
	Option :: {'r', integer()}
		 | {'g', integer()}
		 | {'b', integer()}.
size(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size,{PosX,PosY} = Pos, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, _r} = Arg) -> Arg;
          ({g, _g} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Size,Pos, Opts,?get_env(),?wxImage_Size),
  wxe_util:rec(?wxImage_Size).

%% @equiv setAlpha(This,Alpha, [])
-spec setAlpha(This, Alpha) -> 'ok' when
	This::wxImage(), Alpha::binary().

setAlpha(This,Alpha)
 when is_record(This, wx_ref),is_binary(Alpha) ->
  setAlpha(This,Alpha, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetalpha">external documentation</a>.
-spec setAlpha(This, Alpha, [Option]) -> 'ok' when
	This::wxImage(), Alpha::binary(),
	Option :: {'static_data', boolean()}.
setAlpha(#wx_ref{type=ThisT}=This,Alpha, Options)
 when is_binary(Alpha),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Alpha, Opts,?get_env(),?wxImage_SetAlpha_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetalpha">external documentation</a>.
-spec setAlpha(This, X, Y, Alpha) -> 'ok' when
	This::wxImage(), X::integer(), Y::integer(), Alpha::integer().
setAlpha(#wx_ref{type=ThisT}=This,X,Y,Alpha)
 when is_integer(X),is_integer(Y),is_integer(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,Alpha,?get_env(),?wxImage_SetAlpha_3).

%% @equiv setData(This,Data, [])
-spec setData(This, Data) -> 'ok' when
	This::wxImage(), Data::binary().

setData(This,Data)
 when is_record(This, wx_ref),is_binary(Data) ->
  setData(This,Data, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetdata">external documentation</a>.
-spec setData(This, Data, [Option]) -> 'ok' when
	This::wxImage(), Data::binary(),
	Option :: {'static_data', boolean()}.
setData(#wx_ref{type=ThisT}=This,Data, Options)
 when is_binary(Data),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Data, Opts,?get_env(),?wxImage_SetData_2).

%% @equiv setData(This,Data,New_width,New_height, [])
-spec setData(This, Data, New_width, New_height) -> 'ok' when
	This::wxImage(), Data::binary(), New_width::integer(), New_height::integer().

setData(This,Data,New_width,New_height)
 when is_record(This, wx_ref),is_binary(Data),is_integer(New_width),is_integer(New_height) ->
  setData(This,Data,New_width,New_height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetdata">external documentation</a>.
-spec setData(This, Data, New_width, New_height, [Option]) -> 'ok' when
	This::wxImage(), Data::binary(), New_width::integer(), New_height::integer(),
	Option :: {'static_data', boolean()}.
setData(#wx_ref{type=ThisT}=This,Data,New_width,New_height, Options)
 when is_binary(Data),is_integer(New_width),is_integer(New_height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({static_data, _static_data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Data,New_width,New_height, Opts,?get_env(),?wxImage_SetData_4).

%% @equiv setMask(This, [])
-spec setMask(This) -> 'ok' when
	This::wxImage().

setMask(This)
 when is_record(This, wx_ref) ->
  setMask(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetmask">external documentation</a>.
-spec setMask(This, [Option]) -> 'ok' when
	This::wxImage(),
	Option :: {'mask', boolean()}.
setMask(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({mask, _mask} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxImage_SetMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetmaskcolour">external documentation</a>.
-spec setMaskColour(This, Red, Green, Blue) -> 'ok' when
	This::wxImage(), Red::integer(), Green::integer(), Blue::integer().
setMaskColour(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxImage_SetMaskColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetmaskfromimage">external documentation</a>.
-spec setMaskFromImage(This, Mask, Mr, Mg, Mb) -> boolean() when
	This::wxImage(), Mask::wxImage(), Mr::integer(), Mg::integer(), Mb::integer().
setMaskFromImage(#wx_ref{type=ThisT}=This,#wx_ref{type=MaskT}=Mask,Mr,Mg,Mb)
 when is_integer(Mr),is_integer(Mg),is_integer(Mb) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(MaskT,wxImage),
  wxe_util:queue_cmd(This,Mask,Mr,Mg,Mb,?get_env(),?wxImage_SetMaskFromImage),
  wxe_util:rec(?wxImage_SetMaskFromImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetoption">external documentation</a>.
%% <br /> Also:<br />
%% setOption(This, Name, Value) -> 'ok' when<br />
%% 	This::wxImage(), Name::unicode:chardata(), Value::unicode:chardata().<br />
%% 
-spec setOption(This, Name, Value) -> 'ok' when
	This::wxImage(), Name::unicode:chardata(), Value::integer();
      (This, Name, Value) -> 'ok' when
	This::wxImage(), Name::unicode:chardata(), Value::unicode:chardata().
setOption(#wx_ref{type=ThisT}=This,Name,Value)
 when ?is_chardata(Name),is_integer(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,Value,?get_env(),?wxImage_SetOption_2_0);
setOption(#wx_ref{type=ThisT}=This,Name,Value)
 when ?is_chardata(Name),?is_chardata(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary(Name),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Name_UC,Value_UC,?get_env(),?wxImage_SetOption_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetpalette">external documentation</a>.
-spec setPalette(This, Palette) -> 'ok' when
	This::wxImage(), Palette::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PaletteT}=Palette) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:queue_cmd(This,Palette,?get_env(),?wxImage_SetPalette).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetrgb">external documentation</a>.
-spec setRGB(This, Rect, Red, Green, Blue) -> 'ok' when
	This::wxImage(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}, Red::integer(), Green::integer(), Blue::integer().
setRGB(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect,Red,Green,Blue)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,Rect,Red,Green,Blue,?get_env(),?wxImage_SetRGB_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximage.html#wximagesetrgb">external documentation</a>.
-spec setRGB(This, X, Y, R, G, B) -> 'ok' when
	This::wxImage(), X::integer(), Y::integer(), R::integer(), G::integer(), B::integer().
setRGB(#wx_ref{type=ThisT}=This,X,Y,R,G,B)
 when is_integer(X),is_integer(Y),is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:queue_cmd(This,X,Y,R,G,B,?get_env(),?wxImage_SetRGB_5).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxImage()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImage),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
