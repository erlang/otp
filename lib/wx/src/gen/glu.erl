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

%% OPENGL UTILITY API

%% This file is generated DO NOT EDIT

%% @doc  A part of the standard OpenGL Utility api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(glu).
-compile(inline).
-type vertex() :: {float(), float(), float()}.
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl or glu.hrl
-type m12() :: {f(),f(),f(),f(),
                   f(),f(),f(),f(),
                   f(),f(),f(),f()}.
-type m16() :: {f(),f(),f(),f(),
                   f(),f(),f(),f(),
                   f(),f(),f(),f(),
                   f(),f(),f(),f()}.
-type matrix() :: m12() | m16().
-type mem() :: binary() | tuple().   %% Memory block
-type f() :: float().
-type i() :: integer().

-export([tesselate/2,build1DMipmapLevels/9,build1DMipmaps/6,build2DMipmapLevels/10,
  build2DMipmaps/7,build3DMipmapLevels/11,build3DMipmaps/8,checkExtension/2,
  cylinder/6,deleteQuadric/1,disk/5,errorString/1,getString/1,lookAt/9,
  newQuadric/0,ortho2D/4,partialDisk/7,perspective/4,pickMatrix/5,project/6,
  quadricDrawStyle/2,quadricNormals/2,quadricOrientation/2,quadricTexture/2,
  scaleImage/9,sphere/4,unProject/6,unProject4/9]).

-import(gl, [get_interface/0, rec/1]).

%% API

%% @doc General purpose polygon triangulation.
%% The first argument is the normal and the second a list of
%% vertex positions. Returned is a list of indecies of the vertices
%% and a binary (64bit native float) containing an array of
%% vertex positions, it starts with the vertices in Vs and
%% may contain newly created vertices in the end.
-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}
                  when Normal :: vertex(), Vs :: vertex(),
                  Triangles :: [integer()], VertexPos :: binary().
tesselate(Normal, Vs) ->
  IF = get_interface(),
  IF:queue_cmd(Normal,Vs,5009),
  rec(5009).

-spec build1DMipmapLevels(Target, InternalFormat, Width, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data,5010),
  rec(5010).

-spec build1DMipmaps(Target, InternalFormat, Width, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Format::enum(), Type::enum(), Data::binary().
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Data,5011),
  rec(5011).

-spec build2DMipmapLevels(Target, InternalFormat, Width, Height, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data,5012),
  rec(5012).

-spec build2DMipmaps(Target, InternalFormat, Width, Height, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Data::binary().
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Data,5013),
  rec(5013).

-spec build3DMipmapLevels(Target, InternalFormat, Width, Height, Depth, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data,5014),
  rec(5014).

-spec build3DMipmaps(Target, InternalFormat, Width, Height, Depth, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Data::binary().
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Data,5015),
  rec(5015).

-spec checkExtension(ExtName::string(), ExtString::string()) -> 0|1.
checkExtension(ExtName,ExtString) when is_list(ExtName),is_list(ExtString) ->
  IF = get_interface(),
  ExtNameBin = unicode:characters_to_binary([ExtName|[0]]),
  ExtStringBin = unicode:characters_to_binary([ExtString|[0]]),
  IF:queue_cmd(ExtNameBin,ExtStringBin,5016),
  rec(5016).

-spec cylinder(Quad::i(), Base::f(), Top::f(), Height::f(), Slices::i(), Stacks::i()) -> 'ok'.
cylinder(Quad,Base,Top,Height,Slices,Stacks) when is_integer(Quad),is_float(Base),is_float(Top),is_float(Height),is_integer(Slices),is_integer(Stacks) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Base,Top,Height,Slices,Stacks,5017),
  ok.

-spec deleteQuadric(Quad::i()) -> 'ok'.
deleteQuadric(Quad) when is_integer(Quad) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,5018),
  ok.

-spec disk(Quad::i(), Inner::f(), Outer::f(), Slices::i(), Loops::i()) -> 'ok'.
disk(Quad,Inner,Outer,Slices,Loops) when is_integer(Quad),is_float(Inner),is_float(Outer),is_integer(Slices),is_integer(Loops) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,5019),
  ok.

-spec errorString(Error::enum()) -> string().
errorString(Error) when is_integer(Error) ->
  IF = get_interface(),
  IF:queue_cmd(Error,5020),
  rec(5020).

-spec getString(Name::enum()) -> string().
getString(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5021),
  rec(5021).

-spec lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) -> 'ok'
    when EyeX::f(), EyeY::f(), EyeZ::f(), CenterX::f(), CenterY::f(), CenterZ::f(), UpX::f(), UpY::f(), UpZ::f().
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) when is_float(EyeX),is_float(EyeY),is_float(EyeZ),is_float(CenterX),is_float(CenterY),is_float(CenterZ),is_float(UpX),is_float(UpY),is_float(UpZ) ->
  IF = get_interface(),
  IF:queue_cmd(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ,5022),
  ok.

-spec newQuadric() -> i().
newQuadric()  ->
  IF = get_interface(),
  IF:queue_cmd(5023),
  rec(5023).

-spec ortho2D(Left::f(), Right::f(), Bottom::f(), Top::f()) -> 'ok'.
ortho2D(Left,Right,Bottom,Top) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,5024),
  ok.

-spec partialDisk(Quad, Inner, Outer, Slices, Loops, Start, Sweep) -> 'ok'
    when Quad::i(), Inner::f(), Outer::f(), Slices::i(), Loops::i(), Start::f(), Sweep::f().
partialDisk(Quad,Inner,Outer,Slices,Loops,Start,Sweep) when is_integer(Quad),is_float(Inner),is_float(Outer),is_integer(Slices),is_integer(Loops),is_float(Start),is_float(Sweep) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,Start,Sweep,5025),
  ok.

-spec perspective(Fovy::f(), Aspect::f(), ZNear::f(), ZFar::f()) -> 'ok'.
perspective(Fovy,Aspect,ZNear,ZFar) when is_float(Fovy),is_float(Aspect),is_float(ZNear),is_float(ZFar) ->
  IF = get_interface(),
  IF:queue_cmd(Fovy,Aspect,ZNear,ZFar,5026),
  ok.

-spec pickMatrix(X::f(), Y::f(), DelX::f(), DelY::f(), Viewport::{i(),i(),i(),i()}) -> 'ok'.
pickMatrix(X,Y,DelX,DelY,Viewport) when is_float(X),is_float(Y),is_float(DelX),is_float(DelY),tuple_size(Viewport) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,DelX,DelY,Viewport,5027),
  ok.

-spec project(ObjX, ObjY, ObjZ, Model, Proj, View) -> {i(),WinX::f(),WinY::f(),WinZ::f()}
    when ObjX::f(), ObjY::f(), ObjZ::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}.
project(ObjX,ObjY,ObjZ,Model,Proj,View) when is_float(ObjX),is_float(ObjY),is_float(ObjZ),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(ObjX,ObjY,ObjZ,Model,Proj,View,5028),
  rec(5028).

-spec quadricDrawStyle(Quad::i(), Draw::enum()) -> 'ok'.
quadricDrawStyle(Quad,Draw) when is_integer(Quad),is_integer(Draw) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Draw,5029),
  ok.

-spec quadricNormals(Quad::i(), Normal::enum()) -> 'ok'.
quadricNormals(Quad,Normal) when is_integer(Quad),is_integer(Normal) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Normal,5030),
  ok.

-spec quadricOrientation(Quad::i(), Orientation::enum()) -> 'ok'.
quadricOrientation(Quad,Orientation) when is_integer(Quad),is_integer(Orientation) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Orientation,5031),
  ok.

-spec quadricTexture(Quad::i(), Texture::0|1) -> 'ok'.
quadricTexture(Quad,Texture) when is_integer(Quad),(0 =:= Texture) orelse (1 =:= Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Texture,5032),
  ok.

-spec scaleImage(Format, WIn, HIn, TypeIn, DataIn, WOut, HOut, TypeOut, DataOut) -> i()
    when Format::enum(), WIn::i(), HIn::i(), TypeIn::enum(), DataIn::binary(), WOut::i(), HOut::i(), TypeOut::enum(), DataOut::mem().
scaleImage(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut) when is_integer(Format),is_integer(WIn),is_integer(HIn),is_integer(TypeIn),is_binary(DataIn),is_integer(WOut),is_integer(HOut),is_integer(TypeOut),is_tuple(DataOut) orelse is_binary(DataOut) ->
  IF = get_interface(),
  IF:queue_cmd(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut,5033),
  rec(5033).

-spec sphere(Quad::i(), Radius::f(), Slices::i(), Stacks::i()) -> 'ok'.
sphere(Quad,Radius,Slices,Stacks) when is_integer(Quad),is_float(Radius),is_integer(Slices),is_integer(Stacks) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Radius,Slices,Stacks,5034),
  ok.

-spec unProject(WinX, WinY, WinZ, Model, Proj, View) -> {i(),ObjX::f(),ObjY::f(),ObjZ::f()}
    when WinX::f(), WinY::f(), WinZ::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}.
unProject(WinX,WinY,WinZ,Model,Proj,View) when is_float(WinX),is_float(WinY),is_float(WinZ),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(WinX,WinY,WinZ,Model,Proj,View,5035),
  rec(5035).

-spec unProject4(WinX, WinY, WinZ, ClipW, Model, Proj, View, NearVal, FarVal) -> {i(),ObjX::f(),ObjY::f(),ObjZ::f(),ObjW::f()}
    when WinX::f(), WinY::f(), WinZ::f(), ClipW::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}, NearVal::f(), FarVal::f().
unProject4(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal) when is_float(WinX),is_float(WinY),is_float(WinZ),is_float(ClipW),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4,is_float(NearVal),is_float(FarVal) ->
  IF = get_interface(),
  IF:queue_cmd(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal,5036),
  rec(5036).

