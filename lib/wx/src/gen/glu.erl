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

%% OPENGL UTILITY API

%% This file is generated DO NOT EDIT

%% @doc  A part of the standard OpenGL Utility api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(glu).
-moduledoc """
Erlang wrapper functions for OpenGL

Standard OpenGL API

This documents the functions as a brief version of the complete
[OpenGL reference pages.](https://www.khronos.org/registry/OpenGL-Refpages/)
""".
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
%% vertex positions. Returned is a list of indices of the vertices
%% and a binary (64bit native float) containing an array of
%% vertex positions, it starts with the vertices in Vs and
%% may contain newly created vertices in the end.
-doc """
Triangulates a polygon, the polygon is specified by a `Normal` and `Vs` a list
of vertex positions.

The function returns a list of indices of the vertices and a binary (64bit
native float) containing an array of vertex positions, it starts with the
vertices in `Vs` and may contain newly created vertices in the end.
""".
-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}
                  when Normal :: vertex(), Vs :: vertex(),
                  Triangles :: [integer()], VertexPos :: binary().
tesselate(Normal, Vs) ->
  IF = get_interface(),
  IF:queue_cmd(Normal,Vs,5009),
  rec(5009).

-doc """
[`glu:build1DMipmapLevels/9`](`build1DMipmapLevels/9`) builds a subset of
prefiltered one-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild1DMipmapLevels.xml)
""".
-spec build1DMipmapLevels(Target, InternalFormat, Width, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data,5010),
  rec(5010).

-doc """
[`glu:build1DMipmaps/6`](`build1DMipmaps/6`) builds a series of prefiltered
one-dimensional texture maps of decreasing resolutions called a mipmap. This is
used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild1DMipmaps.xml)
""".
-spec build1DMipmaps(Target, InternalFormat, Width, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Format::enum(), Type::enum(), Data::binary().
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Data,5011),
  rec(5011).

-doc """
[`glu:build2DMipmapLevels/10`](`build2DMipmapLevels/10`) builds a subset of
prefiltered two-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild2DMipmapLevels.xml)
""".
-spec build2DMipmapLevels(Target, InternalFormat, Width, Height, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data,5012),
  rec(5012).

-doc """
[`glu:build2DMipmaps/7`](`build2DMipmaps/7`) builds a series of prefiltered
two-dimensional texture maps of decreasing resolutions called a mipmap. This is
used for the antialiasing of texture-mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild2DMipmaps.xml)
""".
-spec build2DMipmaps(Target, InternalFormat, Width, Height, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Data::binary().
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Data,5013),
  rec(5013).

-doc """
[`glu:build3DMipmapLevels/11`](`build3DMipmapLevels/11`) builds a subset of
prefiltered three-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild3DMipmapLevels.xml)
""".
-spec build3DMipmapLevels(Target, InternalFormat, Width, Height, Depth, Format, Type, Level, Base, Max, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Level::i(), Base::i(), Max::i(), Data::binary().
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Level),is_integer(Base),is_integer(Max),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data,5014),
  rec(5014).

-doc """
[`glu:build3DMipmaps/8`](`build3DMipmaps/8`) builds a series of prefiltered
three-dimensional texture maps of decreasing resolutions called a mipmap. This
is used for the antialiasing of texture-mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild3DMipmaps.xml)
""".
-spec build3DMipmaps(Target, InternalFormat, Width, Height, Depth, Format, Type, Data) -> i()
    when Target::enum(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Data::binary().
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) when is_integer(Target),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Data,5015),
  rec(5015).

-doc """
[`glu:checkExtension/2`](`checkExtension/2`) returns `?GLU_TRUE` if `ExtName` is
supported otherwise `?GLU_FALSE` is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluCheckExtension.xml)
""".
-spec checkExtension(ExtName::string(), ExtString::string()) -> 0|1.
checkExtension(ExtName,ExtString) when is_list(ExtName),is_list(ExtString) ->
  IF = get_interface(),
  ExtNameBin = unicode:characters_to_binary([ExtName|[0]]),
  ExtStringBin = unicode:characters_to_binary([ExtString|[0]]),
  IF:queue_cmd(ExtNameBin,ExtStringBin,5016),
  rec(5016).

-doc """
[`glu:cylinder/6`](`cylinder/6`) draws a cylinder oriented along the `z` axis.
The base of the cylinder is placed at `z` = 0 and the top at z=height. Like a
sphere, a cylinder is subdivided around the `z` axis into slices and along the
`z` axis into stacks.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluCylinder.xml)
""".
-spec cylinder(Quad::i(), Base::f(), Top::f(), Height::f(), Slices::i(), Stacks::i()) -> 'ok'.
cylinder(Quad,Base,Top,Height,Slices,Stacks) when is_integer(Quad),is_float(Base),is_float(Top),is_float(Height),is_integer(Slices),is_integer(Stacks) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Base,Top,Height,Slices,Stacks,5017),
  ok.

-doc """
[`glu:deleteQuadric/1`](`deleteQuadric/1`) destroys the quadrics object (created
with [`glu:newQuadric/0`](`newQuadric/0`)) and frees any memory it uses. Once
[`glu:deleteQuadric/1`](`deleteQuadric/1`) has been called, `Quad` cannot be
used again.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluDeleteQuadric.xml)
""".
-spec deleteQuadric(Quad::i()) -> 'ok'.
deleteQuadric(Quad) when is_integer(Quad) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,5018),
  ok.

-doc """
[`glu:disk/5`](`disk/5`) renders a disk on the `z` = 0 plane. The disk has a
radius of `Outer` and contains a concentric circular hole with a radius of
`Inner`. If `Inner` is 0, then no hole is generated. The disk is subdivided
around the `z` axis into slices (like pizza slices) and also about the `z` axis
into rings (as specified by `Slices` and `Loops`, respectively).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluDisk.xml)
""".
-spec disk(Quad::i(), Inner::f(), Outer::f(), Slices::i(), Loops::i()) -> 'ok'.
disk(Quad,Inner,Outer,Slices,Loops) when is_integer(Quad),is_float(Inner),is_float(Outer),is_integer(Slices),is_integer(Loops) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,5019),
  ok.

-doc """
[`glu:errorString/1`](`errorString/1`) produces an error string from a GL or GLU
error code. The string is in ISO Latin 1 format. For example,
[`glu:errorString/1`](`errorString/1`)(`?GLU_OUT_OF_MEMORY`) returns the string
`out of memory`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluErrorString.xml)
""".
-spec errorString(Error::enum()) -> string().
errorString(Error) when is_integer(Error) ->
  IF = get_interface(),
  IF:queue_cmd(Error,5020),
  rec(5020).

-doc """
[`glu:getString/1`](`getString/1`) returns a pointer to a static string
describing the GLU version or the GLU extensions that are supported.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluGetString.xml)
""".
-spec getString(Name::enum()) -> string().
getString(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5021),
  rec(5021).

-doc """
[`glu:lookAt/9`](`lookAt/9`) creates a viewing matrix derived from an eye point,
a reference point indicating the center of the scene, and an `UP` vector.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluLookAt.xml)
""".
-spec lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) -> 'ok'
    when EyeX::f(), EyeY::f(), EyeZ::f(), CenterX::f(), CenterY::f(), CenterZ::f(), UpX::f(), UpY::f(), UpZ::f().
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) when is_float(EyeX),is_float(EyeY),is_float(EyeZ),is_float(CenterX),is_float(CenterY),is_float(CenterZ),is_float(UpX),is_float(UpY),is_float(UpZ) ->
  IF = get_interface(),
  IF:queue_cmd(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ,5022),
  ok.

-doc """
[`glu:newQuadric/0`](`newQuadric/0`) creates and returns a pointer to a new
quadrics object. This object must be referred to when calling quadrics rendering
and control functions. A return value of 0 means that there is not enough memory
to allocate the object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluNewQuadric.xml)
""".
-spec newQuadric() -> i().
newQuadric()  ->
  IF = get_interface(),
  IF:queue_cmd(5023),
  rec(5023).

-doc """
[`glu:ortho2D/4`](`ortho2D/4`) sets up a two-dimensional orthographic viewing
region. This is equivalent to calling `gl:ortho/6` with near=-1 and far=1.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluOrtho2D.xml)
""".
-spec ortho2D(Left::f(), Right::f(), Bottom::f(), Top::f()) -> 'ok'.
ortho2D(Left,Right,Bottom,Top) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,5024),
  ok.

-doc """
[`glu:partialDisk/7`](`partialDisk/7`) renders a partial disk on the z=0 plane.
A partial disk is similar to a full disk, except that only the subset of the
disk from `Start` through `Start` \+ `Sweep` is included (where 0 degrees is
along the +f2yf axis, 90 degrees along the +`x` axis, 180 degrees along the -`y`
axis, and 270 degrees along the -`x` axis).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPartialDisk.xml)
""".
-spec partialDisk(Quad, Inner, Outer, Slices, Loops, Start, Sweep) -> 'ok'
    when Quad::i(), Inner::f(), Outer::f(), Slices::i(), Loops::i(), Start::f(), Sweep::f().
partialDisk(Quad,Inner,Outer,Slices,Loops,Start,Sweep) when is_integer(Quad),is_float(Inner),is_float(Outer),is_integer(Slices),is_integer(Loops),is_float(Start),is_float(Sweep) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,Start,Sweep,5025),
  ok.

-doc """
[`glu:perspective/4`](`perspective/4`) specifies a viewing frustum into the
world coordinate system. In general, the aspect ratio in
[`glu:perspective/4`](`perspective/4`) should match the aspect ratio of the
associated viewport. For example, aspect=2.0 means the viewer's angle of view is
twice as wide in `x` as it is in `y`. If the viewport is twice as wide as it is
tall, it displays the image without distortion.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml)
""".
-spec perspective(Fovy::f(), Aspect::f(), ZNear::f(), ZFar::f()) -> 'ok'.
perspective(Fovy,Aspect,ZNear,ZFar) when is_float(Fovy),is_float(Aspect),is_float(ZNear),is_float(ZFar) ->
  IF = get_interface(),
  IF:queue_cmd(Fovy,Aspect,ZNear,ZFar,5026),
  ok.

-doc """
[`glu:pickMatrix/5`](`pickMatrix/5`) creates a projection matrix that can be
used to restrict drawing to a small region of the viewport. This is typically
useful to determine what objects are being drawn near the cursor. Use
[`glu:pickMatrix/5`](`pickMatrix/5`) to restrict drawing to a small region
around the cursor. Then, enter selection mode (with `gl:renderMode/1`) and
rerender the scene. All primitives that would have been drawn near the cursor
are identified and stored in the selection buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPickMatrix.xml)
""".
-spec pickMatrix(X::f(), Y::f(), DelX::f(), DelY::f(), Viewport::{i(),i(),i(),i()}) -> 'ok'.
pickMatrix(X,Y,DelX,DelY,Viewport) when is_float(X),is_float(Y),is_float(DelX),is_float(DelY),tuple_size(Viewport) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,DelX,DelY,Viewport,5027),
  ok.

-doc """
[`glu:project/6`](`project/6`) transforms the specified object coordinates into
window coordinates using `Model`, `Proj`, and `View`. The result is stored in
`WinX`, `WinY`, and `WinZ`. A return value of `?GLU_TRUE` indicates success, a
return value of `?GLU_FALSE` indicates failure.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluProject.xml)
""".
-spec project(ObjX, ObjY, ObjZ, Model, Proj, View) -> {i(),WinX::f(),WinY::f(),WinZ::f()}
    when ObjX::f(), ObjY::f(), ObjZ::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}.
project(ObjX,ObjY,ObjZ,Model,Proj,View) when is_float(ObjX),is_float(ObjY),is_float(ObjZ),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(ObjX,ObjY,ObjZ,Model,Proj,View,5028),
  rec(5028).

-doc """
[`glu:quadricDrawStyle/2`](`quadricDrawStyle/2`) specifies the draw style for
quadrics rendered with `Quad`. The legal values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricDrawStyle.xml)
""".
-spec quadricDrawStyle(Quad::i(), Draw::enum()) -> 'ok'.
quadricDrawStyle(Quad,Draw) when is_integer(Quad),is_integer(Draw) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Draw,5029),
  ok.

-doc """
[`glu:quadricNormals/2`](`quadricNormals/2`) specifies what kind of normals are
desired for quadrics rendered with `Quad`. The legal values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricNormals.xml)
""".
-spec quadricNormals(Quad::i(), Normal::enum()) -> 'ok'.
quadricNormals(Quad,Normal) when is_integer(Quad),is_integer(Normal) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Normal,5030),
  ok.

-doc """
[`glu:quadricOrientation/2`](`quadricOrientation/2`) specifies what kind of
orientation is desired for quadrics rendered with `Quad`. The `Orientation`
values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricOrientation.xml)
""".
-spec quadricOrientation(Quad::i(), Orientation::enum()) -> 'ok'.
quadricOrientation(Quad,Orientation) when is_integer(Quad),is_integer(Orientation) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Orientation,5031),
  ok.

-doc """
[`glu:quadricTexture/2`](`quadricTexture/2`) specifies if texture coordinates
should be generated for quadrics rendered with `Quad`. If the value of `Texture`
is `?GLU_TRUE`, then texture coordinates are generated, and if `Texture` is
`?GLU_FALSE`, they are not. The initial value is `?GLU_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricTexture.xml)
""".
-spec quadricTexture(Quad::i(), Texture::0|1) -> 'ok'.
quadricTexture(Quad,Texture) when is_integer(Quad),(0 =:= Texture) orelse (1 =:= Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Texture,5032),
  ok.

-doc """
[`glu:scaleImage/9`](`scaleImage/9`) scales a pixel image using the appropriate
pixel store modes to unpack data from the source image and pack data into the
destination image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluScaleImage.xml)
""".
-spec scaleImage(Format, WIn, HIn, TypeIn, DataIn, WOut, HOut, TypeOut, DataOut) -> i()
    when Format::enum(), WIn::i(), HIn::i(), TypeIn::enum(), DataIn::binary(), WOut::i(), HOut::i(), TypeOut::enum(), DataOut::mem().
scaleImage(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut) when is_integer(Format),is_integer(WIn),is_integer(HIn),is_integer(TypeIn),is_binary(DataIn),is_integer(WOut),is_integer(HOut),is_integer(TypeOut),is_tuple(DataOut) orelse is_binary(DataOut) ->
  IF = get_interface(),
  IF:queue_cmd(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut,5033),
  rec(5033).

-doc """
[`glu:sphere/4`](`sphere/4`) draws a sphere of the given radius centered around
the origin. The sphere is subdivided around the `z` axis into slices and along
the `z` axis into stacks (similar to lines of longitude and latitude).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluSphere.xml)
""".
-spec sphere(Quad::i(), Radius::f(), Slices::i(), Stacks::i()) -> 'ok'.
sphere(Quad,Radius,Slices,Stacks) when is_integer(Quad),is_float(Radius),is_integer(Slices),is_integer(Stacks) ->
  IF = get_interface(),
  IF:queue_cmd(Quad,Radius,Slices,Stacks,5034),
  ok.

-doc(#{equiv => unProject4/9}).
-spec unProject(WinX, WinY, WinZ, Model, Proj, View) -> {i(),ObjX::f(),ObjY::f(),ObjZ::f()}
    when WinX::f(), WinY::f(), WinZ::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}.
unProject(WinX,WinY,WinZ,Model,Proj,View) when is_float(WinX),is_float(WinY),is_float(WinZ),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(WinX,WinY,WinZ,Model,Proj,View,5035),
  rec(5035).

-doc """
[`glu:unProject/6`](`unProject/6`) maps the specified window coordinates into
object coordinates using `Model`, `Proj`, and `View`. The result is stored in
`ObjX`, `ObjY`, and `ObjZ`. A return value of `?GLU_TRUE` indicates success; a
return value of `?GLU_FALSE` indicates failure.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluUnProject.xml)
""".
-spec unProject4(WinX, WinY, WinZ, ClipW, Model, Proj, View, NearVal, FarVal) -> {i(),ObjX::f(),ObjY::f(),ObjZ::f(),ObjW::f()}
    when WinX::f(), WinY::f(), WinZ::f(), ClipW::f(), Model::matrix(), Proj::matrix(), View::{i(),i(),i(),i()}, NearVal::f(), FarVal::f().
unProject4(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal) when is_float(WinX),is_float(WinY),is_float(WinZ),is_float(ClipW),tuple_size(Model) =:= 16; tuple_size(Model) =:= 12,tuple_size(Proj) =:= 16; tuple_size(Proj) =:= 12,tuple_size(View) =:= 4,is_float(NearVal),is_float(FarVal) ->
  IF = get_interface(),
  IF:queue_cmd(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal,5036),
  rec(5036).

