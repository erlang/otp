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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html">wxRegion</a>.
%% @type wxRegion().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxRegion).
-include("wxe.hrl").
-export(['Xor'/2,'Xor'/5,clear/1,contains/2,contains/3,contains/5,convertToBitmap/1,
  destroy/1,getBox/1,intersect/2,intersect/5,isEmpty/1,new/0,new/1,new/2,
  new/4,offset/2,offset/3,subtract/2,subtract/5,union/2,union/3,union/4,
  union/5]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxRegion()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionwxregion">external documentation</a>.
new() ->
  wxe_util:construct(?wxRegion_new_0,
  <<>>).

%% @spec (X::term()) -> wxRegion()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionwxregion">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Bmp::wxBitmap:wxBitmap()) -> wxRegion() </c>
%% </p>
%% <p><c>
%% new(Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> wxRegion() </c>
%% </p>
new(#wx_ref{type=BmpT,ref=BmpRef}) ->
  ?CLASS(BmpT,wxBitmap),
  wxe_util:construct(?wxRegion_new_1_0,
  <<BmpRef:32/?UI>>);
new({RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  wxe_util:construct(?wxRegion_new_1_1,
  <<RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (TopLeft::{X::integer(),Y::integer()}, BottomRight::{X::integer(),Y::integer()}) -> wxRegion()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionwxregion">external documentation</a>.
new({TopLeftX,TopLeftY},{BottomRightX,BottomRightY})
 when is_integer(TopLeftX),is_integer(TopLeftY),is_integer(BottomRightX),is_integer(BottomRightY) ->
  wxe_util:construct(?wxRegion_new_2,
  <<TopLeftX:32/?UI,TopLeftY:32/?UI,BottomRightX:32/?UI,BottomRightY:32/?UI>>).

%% @spec (X::integer(), Y::integer(), W::integer(), H::integer()) -> wxRegion()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionwxregion">external documentation</a>.
new(X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  wxe_util:construct(?wxRegion_new_4,
  <<X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionclear">external documentation</a>.
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:cast(?wxRegion_Clear,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxRegion(),X::term()) -> WxRegionContain
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregioncontains">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% contains(This::wxRegion(), Pt::{X::integer(),Y::integer()}) -> WxRegionContain </c>
%%<br /> WxRegionContain = integer()
%%<br /> WxRegionContain is one of ?wxOutRegion | ?wxPartRegion | ?wxInRegion
%% </p>
%% <p><c>
%% contains(This::wxRegion(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> WxRegionContain </c>
%%<br /> WxRegionContain = integer()
%%<br /> WxRegionContain is one of ?wxOutRegion | ?wxPartRegion | ?wxInRegion
%% </p>
contains(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Contains_1_0,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>);
contains(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Contains_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer()) -> WxRegionContain
%% WxRegionContain = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> WxRegionContain is one of ?wxOutRegion | ?wxPartRegion | ?wxInRegion
contains(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Contains_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer()) -> WxRegionContain
%% WxRegionContain = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> WxRegionContain is one of ?wxOutRegion | ?wxPartRegion | ?wxInRegion
contains(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Contains_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion()) -> wxBitmap:wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionconverttobitmap">external documentation</a>.
convertToBitmap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_ConvertToBitmap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxRegion()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregiongetbox">external documentation</a>.
getBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_GetBox,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxRegion(),X::wxRegion()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionintersect">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% intersect(This::wxRegion(), Region::wxRegion()) -> bool() </c>
%% </p>
%% <p><c>
%% intersect(This::wxRegion(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool() </c>
%% </p>
intersect(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:call(?wxRegion_Intersect_1_0,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>);
intersect(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Intersect_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionintersect">external documentation</a>.
intersect(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Intersect_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionisempty">external documentation</a>.
isEmpty(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_IsEmpty,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxRegion(),X::wxRegion()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionsubtract">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% subtract(This::wxRegion(), Region::wxRegion()) -> bool() </c>
%% </p>
%% <p><c>
%% subtract(This::wxRegion(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool() </c>
%% </p>
subtract(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:call(?wxRegion_Subtract_1_0,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>);
subtract(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Subtract_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionsubtract">external documentation</a>.
subtract(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Subtract_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion(), Pt::{X::integer(),Y::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionoffset">external documentation</a>.
offset(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Offset_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionoffset">external documentation</a>.
offset(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Offset_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxRegion(),X::term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionunion">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% union(This::wxRegion(), Region::wxRegion() | wxBitmap:wxBitmap()) -> bool() </c>
%% </p>
%% <p><c>
%% union(This::wxRegion(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool() </c>
%% </p>
union(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxRegion),
  RegionOP = case ?CLASS_T(RegionT,wxRegion) of
     true ->
       ?wxRegion_Union_1_1;
     _ -> ?CLASS(RegionT,wxBitmap),
       ?wxRegion_Union_1_0
     end,
  wxe_util:call(RegionOP,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>);
union(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Union_1_2,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxRegion(), Bmp::wxBitmap:wxBitmap(), Transp::wx:colour()) -> bool()
%% @equiv union(This,Bmp,Transp, [])
union(This,Bmp,Transp)
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),tuple_size(Transp) =:= 3; tuple_size(Transp) =:= 4 ->
  union(This,Bmp,Transp, []).

%% @spec (This::wxRegion(), Bmp::wxBitmap:wxBitmap(), Transp::wx:colour(), [Option]) -> bool()
%% Option = {tolerance, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionunion">external documentation</a>.
union(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BmpT,ref=BmpRef},Transp, Options)
 when tuple_size(Transp) =:= 3; tuple_size(Transp) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({tolerance, Tolerance}, Acc) -> [<<1:32/?UI,Tolerance:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxRegion_Union_3,
  <<ThisRef:32/?UI,BmpRef:32/?UI,(wxe_util:colour_bin(Transp)):16/binary, BinOpt/binary>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionunion">external documentation</a>.
union(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Union_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion(),X::wxRegion()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionxor">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% 'Xor'(This::wxRegion(), Region::wxRegion()) -> bool() </c>
%% </p>
%% <p><c>
%% 'Xor'(This::wxRegion(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool() </c>
%% </p>
'Xor'(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RegionT,ref=RegionRef}) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:call(?wxRegion_Xor_1_0,
  <<ThisRef:32/?UI,RegionRef:32/?UI>>);
'Xor'(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Xor_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxregion.html#wxregionxor">external documentation</a>.
'Xor'(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:call(?wxRegion_Xor_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxRegion()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxRegion),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
