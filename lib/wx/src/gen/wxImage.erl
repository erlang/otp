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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html">wxImage</a>.
%%
%% All (default) image handlers are initialized.

%%
%% @type wxImage().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxImage).
-include("wxe.hrl").
-export(['Destroy'/1,blur/2,blurHorizontal/2,blurVertical/2,convertAlphaToMask/1,
  convertAlphaToMask/2,convertToGreyscale/1,convertToGreyscale/2,convertToMono/4,
  copy/1,create/3,create/4,create/5,create/6,destroy/1,findFirstUnusedColour/1,
  findFirstUnusedColour/2,getAlpha/1,getAlpha/3,getBlue/3,getData/1,
  getGreen/3,getHeight/1,getImageCount/1,getImageCount/2,getImageExtWildcard/0,
  getMaskBlue/1,getMaskGreen/1,getMaskRed/1,getOption/2,getOptionInt/2,
  getOrFindMaskColour/1,getPalette/1,getRed/3,getSubImage/2,getWidth/1,
  hasAlpha/1,hasMask/1,hasOption/2,initAlpha/1,initStandardHandlers/0,
  isTransparent/3,isTransparent/4,loadFile/2,loadFile/3,loadFile/4,mirror/1,
  mirror/2,new/0,new/1,new/2,new/3,new/4,new/5,ok/1,removeHandler/1,replace/7,
  rescale/3,rescale/4,resize/3,resize/4,rotate/3,rotate/4,rotate90/1,rotate90/2,
  rotateHue/2,saveFile/2,saveFile/3,scale/3,scale/4,setAlpha/2,setAlpha/3,
  setAlpha/4,setData/2,setData/3,setData/4,setData/5,setMask/1,setMask/2,
  setMaskColour/4,setMaskFromImage/5,setOption/3,setPalette/2,setRGB/5,
  setRGB/6,size/3,size/4]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagewximage">external documentation</a>.
new() ->
  wxe_util:construct(?wxImage_new_0,
  <<>>).

%% @spec (Name::string()) -> wxImage()
%% @equiv new(Name, [])
new(Name)
 when is_list(Name) ->
  new(Name, []).

%% @spec (X::integer()|string(),X::integer()|term()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Width::integer(), Height::integer()) -> new(Width,Height, []) </c></p>
%% <p><c>
%% new(Name::string(), [Option]) -> wxImage() </c>
%%<br /> Option = {type, integer()} | {index, integer()}
%% </p>

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(Name, Options)
 when is_list(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          ({index, Index}, Acc) -> [<<2:32/?UI,Index:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImage_new_2,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (X::integer()|string(),X::integer()|string(),X::binary()|term()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Width::integer(), Height::integer(), Data::binary()) -> new(Width,Height,Data, []) </c></p>
%% <p><c>
%% new(Width::integer(), Height::integer(), [Option]) -> wxImage() </c>
%%<br /> Option = {clear, bool()}
%% </p>
%% <p><c>
%% new(Name::string(), Mimetype::string(), [Option]) -> wxImage() </c>
%%<br /> Option = {index, integer()}
%% </p>

new(Width,Height,Data)
 when is_integer(Width),is_integer(Height),is_binary(Data) ->
  new(Width,Height,Data, []);
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({clear, Clear}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Clear)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImage_new_3_0,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>);
new(Name,Mimetype, Options)
 when is_list(Name),is_list(Mimetype),is_list(Options) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  Mimetype_UC = unicode:characters_to_binary([Mimetype,0]),
  MOpts = fun({index, Index}, Acc) -> [<<1:32/?UI,Index:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImage_new_3_1,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Mimetype_UC)):32/?UI,(Mimetype_UC)/binary, 0:(((8- ((4+byte_size(Mimetype_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (Width::integer(),Height::integer(),Data::binary(),X::binary()|term()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagewximage">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Width::integer(), Height::integer(), Data::binary(), Alpha::binary()) -> new(Width,Height,Data,Alpha, []) </c></p>
%% <p><c>
%% new(Width::integer(), Height::integer(), Data::binary(), [Option]) -> wxImage() </c>
%%<br /> Option = {static_data, bool()}
%% </p>

new(Width,Height,Data,Alpha)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  new(Width,Height,Data,Alpha, []);
new(Width,Height,Data, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_list(Options) ->
  wxe_util:send_bin(Data),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImage_new_4,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (Width::integer(), Height::integer(), Data::binary(), Alpha::binary(), [Option]) -> wxImage()
%% Option = {static_data, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagewximage">external documentation</a>.
new(Width,Height,Data,Alpha, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  wxe_util:send_bin(Data),
  wxe_util:send_bin(Alpha),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImage_new_5,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (This::wxImage(), Radius::integer()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageblur">external documentation</a>.
blur(#wx_ref{type=ThisT,ref=ThisRef},Radius)
 when is_integer(Radius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_Blur,
  <<ThisRef:32/?UI,Radius:32/?UI>>).

%% @spec (This::wxImage(), Radius::integer()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageblurhorizontal">external documentation</a>.
blurHorizontal(#wx_ref{type=ThisT,ref=ThisRef},Radius)
 when is_integer(Radius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_BlurHorizontal,
  <<ThisRef:32/?UI,Radius:32/?UI>>).

%% @spec (This::wxImage(), Radius::integer()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageblurvertical">external documentation</a>.
blurVertical(#wx_ref{type=ThisT,ref=ThisRef},Radius)
 when is_integer(Radius) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_BlurVertical,
  <<ThisRef:32/?UI,Radius:32/?UI>>).

%% @spec (This::wxImage()) -> bool()
%% @equiv convertAlphaToMask(This, [])
convertAlphaToMask(This)
 when is_record(This, wx_ref) ->
  convertAlphaToMask(This, []).

%% @spec (This::wxImage(), [Option]) -> bool()
%% Option = {threshold, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageconvertalphatomask">external documentation</a>.
convertAlphaToMask(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, Threshold}, Acc) -> [<<1:32/?UI,Threshold:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_ConvertAlphaToMask,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage()) -> wxImage()
%% @equiv convertToGreyscale(This, [])
convertToGreyscale(This)
 when is_record(This, wx_ref) ->
  convertToGreyscale(This, []).

%% @spec (This::wxImage(), [Option]) -> wxImage()
%% Option = {lr, float()} | {lg, float()} | {lb, float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageconverttogreyscale">external documentation</a>.
convertToGreyscale(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({lr, Lr}, Acc) -> [<<1:32/?UI,0:32,Lr:64/?F>>|Acc];
          ({lg, Lg}, Acc) -> [<<2:32/?UI,0:32,Lg:64/?F>>|Acc];
          ({lb, Lb}, Acc) -> [<<3:32/?UI,0:32,Lb:64/?F>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_ConvertToGreyscale,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), R::integer(), G::integer(), B::integer()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageconverttomono">external documentation</a>.
convertToMono(#wx_ref{type=ThisT,ref=ThisRef},R,G,B)
 when is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_ConvertToMono,
  <<ThisRef:32/?UI,R:32/?UI,G:32/?UI,B:32/?UI>>).

%% @spec (This::wxImage()) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagecopy">external documentation</a>.
copy(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_Copy,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage(), Width::integer(), Height::integer()) -> bool()
%% @equiv create(This,Width,Height, [])
create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

%% @spec (This::wxImage(),Width::integer(),Height::integer(),X::binary()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagecreate">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% create(This::wxImage(), Width::integer(), Height::integer(), Data::binary()) -> create(This,Width,Height,Data, []) </c></p>
%% <p><c>
%% create(This::wxImage(), Width::integer(), Height::integer(), [Option]) -> bool() </c>
%%<br /> Option = {clear, bool()}
%% </p>

create(This,Width,Height,Data)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height),is_binary(Data) ->
  create(This,Width,Height,Data, []);
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clear, Clear}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Clear)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Create_3,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(),Width::integer(),Height::integer(),Data::binary(),X::binary()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagecreate">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% create(This::wxImage(), Width::integer(), Height::integer(), Data::binary(), Alpha::binary()) -> create(This,Width,Height,Data,Alpha, []) </c></p>
%% <p><c>
%% create(This::wxImage(), Width::integer(), Height::integer(), Data::binary(), [Option]) -> bool() </c>
%%<br /> Option = {static_data, bool()}
%% </p>

create(This,Width,Height,Data,Alpha)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha) ->
  create(This,Width,Height,Data,Alpha, []);
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height,Data, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:send_bin(Data),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Create_4,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Width::integer(), Height::integer(), Data::binary(), Alpha::binary(), [Option]) -> bool()
%% Option = {static_data, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagecreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height,Data,Alpha, Options)
 when is_integer(Width),is_integer(Height),is_binary(Data),is_binary(Alpha),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:send_bin(Data),
  wxe_util:send_bin(Alpha),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Create_5,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagedestroy">external documentation</a>.
'Destroy'(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_Destroy,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> {bool(),R::integer(),G::integer(),B::integer()}
%% @equiv findFirstUnusedColour(This, [])
findFirstUnusedColour(This)
 when is_record(This, wx_ref) ->
  findFirstUnusedColour(This, []).

%% @spec (This::wxImage(), [Option]) -> {bool(),R::integer(),G::integer(),B::integer()}
%% Option = {startR, integer()} | {startG, integer()} | {startB, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagefindfirstunusedcolour">external documentation</a>.
findFirstUnusedColour(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({startR, StartR}, Acc) -> [<<1:32/?UI,StartR:32/?UI>>|Acc];
          ({startG, StartG}, Acc) -> [<<2:32/?UI,StartG:32/?UI>>|Acc];
          ({startB, StartB}, Acc) -> [<<3:32/?UI,StartB:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_FindFirstUnusedColour,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec () -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetimageextwildcard">external documentation</a>.
getImageExtWildcard() ->
  wxe_util:call(?wxImage_GetImageExtWildcard,
  <<>>).

%% @spec (This::wxImage()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetalpha">external documentation</a>.
getAlpha(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetAlpha_0,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage(), X::integer(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetalpha">external documentation</a>.
getAlpha(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetAlpha_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxImage(), X::integer(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetblue">external documentation</a>.
getBlue(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetBlue,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxImage()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetdata">external documentation</a>.
getData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage(), X::integer(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetgreen">external documentation</a>.
getGreen(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetGreen,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (Name::string()) -> integer()
%% @equiv getImageCount(Name, [])
getImageCount(Name)
 when is_list(Name) ->
  getImageCount(Name, []).

%% @spec (Name::string(), [Option]) -> integer()
%% Option = {type, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetimagecount">external documentation</a>.
getImageCount(Name, Options)
 when is_list(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_GetImageCount,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxImage()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetheight">external documentation</a>.
getHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetHeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetmaskblue">external documentation</a>.
getMaskBlue(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetMaskBlue,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetmaskgreen">external documentation</a>.
getMaskGreen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetMaskGreen,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetmaskred">external documentation</a>.
getMaskRed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetMaskRed,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> {bool(),R::integer(),G::integer(),B::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetorfindmaskcolour">external documentation</a>.
getOrFindMaskColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetOrFindMaskColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> wxPalette:wxPalette()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetpalette">external documentation</a>.
getPalette(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetPalette,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage(), X::integer(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetred">external documentation</a>.
getRed(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetRed,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxImage(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> wxImage()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetsubimage">external documentation</a>.
getSubImage(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetSubImage,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxImage()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetwidth">external documentation</a>.
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_GetWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagehasalpha">external documentation</a>.
hasAlpha(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_HasAlpha,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagehasmask">external documentation</a>.
hasMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_HasMask,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImage(), Name::string()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetoption">external documentation</a>.
getOption(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_GetOption,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage(), Name::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagegetoptionint">external documentation</a>.
getOptionInt(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_GetOptionInt,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage(), Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagehasoption">external documentation</a>.
hasOption(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_HasOption,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageinitalpha">external documentation</a>.
initAlpha(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_InitAlpha,
  <<ThisRef:32/?UI>>).

%% @spec () -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageinitstandardhandlers">external documentation</a>.
initStandardHandlers() ->
  wxe_util:cast(?wxImage_InitStandardHandlers,
  <<>>).

%% @spec (This::wxImage(), X::integer(), Y::integer()) -> bool()
%% @equiv isTransparent(This,X,Y, [])
isTransparent(This,X,Y)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y) ->
  isTransparent(This,X,Y, []).

%% @spec (This::wxImage(), X::integer(), Y::integer(), [Option]) -> bool()
%% Option = {threshold, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageistransparent">external documentation</a>.
isTransparent(#wx_ref{type=ThisT,ref=ThisRef},X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({threshold, Threshold}, Acc) -> [<<1:32/?UI,Threshold:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_IsTransparent,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Name::string()) -> bool()
%% @equiv loadFile(This,Name, [])
loadFile(This,Name)
 when is_record(This, wx_ref),is_list(Name) ->
  loadFile(This,Name, []).

%% @spec (This::wxImage(), Name::string(), [Option]) -> bool()
%% Option = {type, integer()} | {index, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageloadfile">external documentation</a>.
loadFile(#wx_ref{type=ThisT,ref=ThisRef},Name, Options)
 when is_list(Name),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({type, Type}, Acc) -> [<<1:32/?UI,Type:32/?UI>>|Acc];
          ({index, Index}, Acc) -> [<<2:32/?UI,Index:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_LoadFile_2,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxImage(), Name::string(), Mimetype::string(), [Option]) -> bool()
%% Option = {index, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageloadfile">external documentation</a>.
loadFile(#wx_ref{type=ThisT,ref=ThisRef},Name,Mimetype, Options)
 when is_list(Name),is_list(Mimetype),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  Mimetype_UC = unicode:characters_to_binary([Mimetype,0]),
  MOpts = fun({index, Index}, Acc) -> [<<1:32/?UI,Index:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_LoadFile_3,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Mimetype_UC)):32/?UI,(Mimetype_UC)/binary, 0:(((8- ((4+byte_size(Mimetype_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxImage()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageok">external documentation</a>.
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:call(?wxImage_Ok,
  <<ThisRef:32/?UI>>).

%% @spec (Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageremovehandler">external documentation</a>.
removeHandler(Name)
 when is_list(Name) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_RemoveHandler,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage()) -> wxImage()
%% @equiv mirror(This, [])
mirror(This)
 when is_record(This, wx_ref) ->
  mirror(This, []).

%% @spec (This::wxImage(), [Option]) -> wxImage()
%% Option = {horizontally, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagemirror">external documentation</a>.
mirror(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({horizontally, Horizontally}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Horizontally)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Mirror,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), R1::integer(), G1::integer(), B1::integer(), R2::integer(), G2::integer(), B2::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagereplace">external documentation</a>.
replace(#wx_ref{type=ThisT,ref=ThisRef},R1,G1,B1,R2,G2,B2)
 when is_integer(R1),is_integer(G1),is_integer(B1),is_integer(R2),is_integer(G2),is_integer(B2) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_Replace,
  <<ThisRef:32/?UI,R1:32/?UI,G1:32/?UI,B1:32/?UI,R2:32/?UI,G2:32/?UI,B2:32/?UI>>).

%% @spec (This::wxImage(), Width::integer(), Height::integer()) -> wxImage()
%% @equiv rescale(This,Width,Height, [])
rescale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  rescale(This,Width,Height, []).

%% @spec (This::wxImage(), Width::integer(), Height::integer(), [Option]) -> wxImage()
%% Option = {quality, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagerescale">external documentation</a>.
rescale(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, Quality}, Acc) -> [<<1:32/?UI,Quality:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Rescale,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Size::{W::integer(),H::integer()}, Pos::{X::integer(),Y::integer()}) -> wxImage()
%% @equiv resize(This,Size,Pos, [])
resize(This,Size={SizeW,SizeH},Pos={PosX,PosY})
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  resize(This,Size,Pos, []).

%% @spec (This::wxImage(), Size::{W::integer(),H::integer()}, Pos::{X::integer(),Y::integer()}, [Option]) -> wxImage()
%% Option = {r, integer()} | {g, integer()} | {b, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximageresize">external documentation</a>.
resize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH},{PosX,PosY}, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, R}, Acc) -> [<<1:32/?UI,R:32/?UI>>|Acc];
          ({g, G}, Acc) -> [<<2:32/?UI,G:32/?UI>>|Acc];
          ({b, B}, Acc) -> [<<3:32/?UI,B:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Resize,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI,PosX:32/?UI,PosY:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Angle::float(), Centre_of_rotation::{X::integer(),Y::integer()}) -> wxImage()
%% @equiv rotate(This,Angle,Centre_of_rotation, [])
rotate(This,Angle,Centre_of_rotation={Centre_of_rotationX,Centre_of_rotationY})
 when is_record(This, wx_ref),is_float(Angle),is_integer(Centre_of_rotationX),is_integer(Centre_of_rotationY) ->
  rotate(This,Angle,Centre_of_rotation, []).

%% @spec (This::wxImage(), Angle::float(), Centre_of_rotation::{X::integer(),Y::integer()}, [Option]) -> wxImage()
%% Option = {interpolating, bool()} | {offset_after_rotation, {X::integer(),Y::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagerotate">external documentation</a>.
rotate(#wx_ref{type=ThisT,ref=ThisRef},Angle,{Centre_of_rotationX,Centre_of_rotationY}, Options)
 when is_float(Angle),is_integer(Centre_of_rotationX),is_integer(Centre_of_rotationY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({interpolating, Interpolating}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Interpolating)):32/?UI>>|Acc];
          ({offset_after_rotation, {Offset_after_rotationX,Offset_after_rotationY}}, Acc) -> [<<2:32/?UI,Offset_after_rotationX:32/?UI,Offset_after_rotationY:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Rotate,
  <<ThisRef:32/?UI,0:32,Angle:64/?F,Centre_of_rotationX:32/?UI,Centre_of_rotationY:32/?UI, BinOpt/binary>>).

%% @spec (This::wxImage(), Angle::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagerotatehue">external documentation</a>.
rotateHue(#wx_ref{type=ThisT,ref=ThisRef},Angle)
 when is_float(Angle) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_RotateHue,
  <<ThisRef:32/?UI,0:32,Angle:64/?F>>).

%% @spec (This::wxImage()) -> wxImage()
%% @equiv rotate90(This, [])
rotate90(This)
 when is_record(This, wx_ref) ->
  rotate90(This, []).

%% @spec (This::wxImage(), [Option]) -> wxImage()
%% Option = {clockwise, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagerotate90">external documentation</a>.
rotate90(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({clockwise, Clockwise}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Clockwise)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Rotate90,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesavefile">external documentation</a>.
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_SaveFile_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage(),Name::string(),X::integer()|string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesavefile">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% saveFile(This::wxImage(), Name::string(), Type::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% saveFile(This::wxImage(), Name::string(), Mimetype::string()) -> bool() </c>
%% </p>
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Name,Type)
 when is_list(Name),is_integer(Type) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxImage_SaveFile_2_0,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,Type:32/?UI>>);
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Name,Mimetype)
 when is_list(Name),is_list(Mimetype) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  Mimetype_UC = unicode:characters_to_binary([Mimetype,0]),
  wxe_util:call(?wxImage_SaveFile_2_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Mimetype_UC)):32/?UI,(Mimetype_UC)/binary, 0:(((8- ((4+byte_size(Mimetype_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage(), Width::integer(), Height::integer()) -> wxImage()
%% @equiv scale(This,Width,Height, [])
scale(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  scale(This,Width,Height, []).

%% @spec (This::wxImage(), Width::integer(), Height::integer(), [Option]) -> wxImage()
%% Option = {quality, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagescale">external documentation</a>.
scale(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({quality, Quality}, Acc) -> [<<1:32/?UI,Quality:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Scale,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Size::{W::integer(),H::integer()}, Pos::{X::integer(),Y::integer()}) -> wxImage()
%% @equiv size(This,Size,Pos, [])
size(This,Size={SizeW,SizeH},Pos={PosX,PosY})
 when is_record(This, wx_ref),is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY) ->
  size(This,Size,Pos, []).

%% @spec (This::wxImage(), Size::{W::integer(),H::integer()}, Pos::{X::integer(),Y::integer()}, [Option]) -> wxImage()
%% Option = {r, integer()} | {g, integer()} | {b, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesize">external documentation</a>.
size(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH},{PosX,PosY}, Options)
 when is_integer(SizeW),is_integer(SizeH),is_integer(PosX),is_integer(PosY),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({r, R}, Acc) -> [<<1:32/?UI,R:32/?UI>>|Acc];
          ({g, G}, Acc) -> [<<2:32/?UI,G:32/?UI>>|Acc];
          ({b, B}, Acc) -> [<<3:32/?UI,B:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImage_Size,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI,PosX:32/?UI,PosY:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Alpha::binary()) -> ok
%% @equiv setAlpha(This,Alpha, [])
setAlpha(This,Alpha)
 when is_record(This, wx_ref),is_binary(Alpha) ->
  setAlpha(This,Alpha, []).

%% @spec (This::wxImage(), Alpha::binary(), [Option]) -> ok
%% Option = {static_data, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetalpha">external documentation</a>.
setAlpha(#wx_ref{type=ThisT,ref=ThisRef},Alpha, Options)
 when is_binary(Alpha),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:send_bin(Alpha),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxImage_SetAlpha_2,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), X::integer(), Y::integer(), Alpha::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetalpha">external documentation</a>.
setAlpha(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Alpha)
 when is_integer(X),is_integer(Y),is_integer(Alpha) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_SetAlpha_3,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Alpha:32/?UI>>).

%% @spec (This::wxImage(), Data::binary()) -> ok
%% @equiv setData(This,Data, [])
setData(This,Data)
 when is_record(This, wx_ref),is_binary(Data) ->
  setData(This,Data, []).

%% @spec (This::wxImage(), Data::binary(), [Option]) -> ok
%% Option = {static_data, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetdata">external documentation</a>.
setData(#wx_ref{type=ThisT,ref=ThisRef},Data, Options)
 when is_binary(Data),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:send_bin(Data),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxImage_SetData_2,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), Data::binary(), New_width::integer(), New_height::integer()) -> ok
%% @equiv setData(This,Data,New_width,New_height, [])
setData(This,Data,New_width,New_height)
 when is_record(This, wx_ref),is_binary(Data),is_integer(New_width),is_integer(New_height) ->
  setData(This,Data,New_width,New_height, []).

%% @spec (This::wxImage(), Data::binary(), New_width::integer(), New_height::integer(), [Option]) -> ok
%% Option = {static_data, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetdata">external documentation</a>.
setData(#wx_ref{type=ThisT,ref=ThisRef},Data,New_width,New_height, Options)
 when is_binary(Data),is_integer(New_width),is_integer(New_height),is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:send_bin(Data),
  MOpts = fun({static_data, Static_data}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Static_data)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxImage_SetData_4,
  <<ThisRef:32/?UI,New_width:32/?UI,New_height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage()) -> ok
%% @equiv setMask(This, [])
setMask(This)
 when is_record(This, wx_ref) ->
  setMask(This, []).

%% @spec (This::wxImage(), [Option]) -> ok
%% Option = {mask, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetmask">external documentation</a>.
setMask(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxImage),
  MOpts = fun({mask, Mask}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Mask)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxImage_SetMask,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImage(), R::integer(), G::integer(), B::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetmaskcolour">external documentation</a>.
setMaskColour(#wx_ref{type=ThisT,ref=ThisRef},R,G,B)
 when is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_SetMaskColour,
  <<ThisRef:32/?UI,R:32/?UI,G:32/?UI,B:32/?UI>>).

%% @spec (This::wxImage(), Mask::wxImage(), Mr::integer(), Mg::integer(), Mb::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetmaskfromimage">external documentation</a>.
setMaskFromImage(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MaskT,ref=MaskRef},Mr,Mg,Mb)
 when is_integer(Mr),is_integer(Mg),is_integer(Mb) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(MaskT,wxImage),
  wxe_util:call(?wxImage_SetMaskFromImage,
  <<ThisRef:32/?UI,MaskRef:32/?UI,Mr:32/?UI,Mg:32/?UI,Mb:32/?UI>>).

%% @spec (This::wxImage(),Name::string(),X::integer()|string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetoption">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setOption(This::wxImage(), Name::string(), Value::integer()) -> ok </c>
%% </p>
%% <p><c>
%% setOption(This::wxImage(), Name::string(), Value::string()) -> ok </c>
%% </p>
setOption(#wx_ref{type=ThisT,ref=ThisRef},Name,Value)
 when is_list(Name),is_integer(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:cast(?wxImage_SetOption_2_0,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,Value:32/?UI>>);
setOption(#wx_ref{type=ThisT,ref=ThisRef},Name,Value)
 when is_list(Name),is_list(Value) ->
  ?CLASS(ThisT,wxImage),
  Name_UC = unicode:characters_to_binary([Name,0]),
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:cast(?wxImage_SetOption_2_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((4+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxImage(), Palette::wxPalette:wxPalette()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetpalette">external documentation</a>.
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaletteT,ref=PaletteRef}) ->
  ?CLASS(ThisT,wxImage),
  ?CLASS(PaletteT,wxPalette),
  wxe_util:cast(?wxImage_SetPalette,
  <<ThisRef:32/?UI,PaletteRef:32/?UI>>).

%% @spec (This::wxImage(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, R::integer(), G::integer(), B::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetrgb">external documentation</a>.
setRGB(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH},R,G,B)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_SetRGB_4,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,R:32/?UI,G:32/?UI,B:32/?UI>>).

%% @spec (This::wxImage(), X::integer(), Y::integer(), R::integer(), G::integer(), B::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximage.html#wximagesetrgb">external documentation</a>.
setRGB(#wx_ref{type=ThisT,ref=ThisRef},X,Y,R,G,B)
 when is_integer(X),is_integer(Y),is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxImage),
  wxe_util:cast(?wxImage_SetRGB_5,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,R:32/?UI,G:32/?UI,B:32/?UI>>).

%% @spec (This::wxImage()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImage),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
