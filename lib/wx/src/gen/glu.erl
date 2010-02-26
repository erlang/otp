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

%% OPENGL UTILITY API

%% This file is generated DO NOT EDIT

%% @doc  A part of the standard OpenGL Utility api.
%% See <a href="http://www.opengl.org/sdk/docs/man/">www.opengl.org</a>
%%
%% Booleans are represented by integers 0 and 1.

%% @type wx_mem(). see wx.erl on memory allocation functions
%% @type enum().   An integer defined in gl.hrl
%% @type offset(). An integer which is an offset in an array
%% @type clamp().  A float clamped between 0.0 - 1.0

-module(glu).
-compile(inline).
-include("wxe.hrl").
-define(GLenum,32/native-unsigned).
-define(GLboolean,8/native-unsigned).
-define(GLbitfield,32/native-unsigned).
-define(GLbyte,8/native-signed).
-define(GLshort,16/native-signed).
-define(GLint,32/native-signed).
-define(GLubyte,8/native-unsigned).
-define(GLushort,16/native-unsigned).
-define(GLuint,32/native-unsigned).
-define(GLsizei,32/native-signed).
-define(GLfloat,32/native-float).
-define(GLclampf,32/native-float).
-define(GLdouble,64/native-float).
-define(GLclampd,64/native-float).
-define(GLsizeiptr,64/native-unsigned).
-define(GLintptr,64/native-unsigned).
-define(GLUquadric,64/native-unsigned).
-define(GLhandleARB,64/native-unsigned).

-export([tesselate/2,build1DMipmapLevels/9,build1DMipmaps/6,build2DMipmapLevels/10,
  build2DMipmaps/7,build3DMipmapLevels/11,build3DMipmaps/8,checkExtension/2,
  cylinder/6,deleteQuadric/1,disk/5,errorString/1,getString/1,lookAt/9,
  newQuadric/0,ortho2D/4,partialDisk/7,perspective/4,pickMatrix/5,project/6,
  quadricDrawStyle/2,quadricNormals/2,quadricOrientation/2,quadricTexture/2,
  scaleImage/9,sphere/4,unProject/6,unProject4/9]).


%% API

%% @spec (Vec3, [Vec3]) -> {Triangles, VertexPos}
%%  Vec3 = {float(),float(),float()}
%%  Triangles = [VertexIndex::integer()]
%%  VertexPos  = binary()
%% @doc General purpose polygon triangulation.
%% The first argument is the normal and the second a list of
%% vertex positions. Returned is a list of indecies of the vertices
%% and a binary (64bit native float) containing an array of
%% vertex positions, it starts with the vertices in Vs and
%% may contain newly created vertices in the end.
tesselate({Nx,Ny,Nz}, Vs) ->
  wxe_util:call(5000, <<(length(Vs)):32/native,0:32,
    Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble,
    (<< <<Vx:?GLdouble,Vy:?GLdouble,Vz:?GLdouble >>
        || {Vx,Vy,Vz} <- Vs>>)/binary >>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmapLevels.xml">external</a> documentation.
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5010, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmaps.xml">external</a> documentation.
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5011, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmapLevels.xml">external</a> documentation.
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5012, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmaps.xml">external</a> documentation.
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5013, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmapLevels.xml">external</a> documentation.
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5014, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmaps.xml">external</a> documentation.
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5015, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (ExtName::[integer()],ExtString::[integer()]) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluCheckExtension.xml">external</a> documentation.
checkExtension(ExtName,ExtString) ->
  wxe_util:call(5016, <<(length(ExtName)):?GLuint,
        (<< <<C:?GLubyte>> || C <- ExtName>>)/binary,0:((8-((length(ExtName)+ 4) rem 8)) rem 8),(length(ExtString)):?GLuint,
        (<< <<C:?GLubyte>> || C <- ExtString>>)/binary,0:((8-((length(ExtString)+ 4) rem 8)) rem 8)>>).

%% @spec (Quad::integer(),Base::float(),Top::float(),Height::float(),Slices::integer(),Stacks::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluCylinder.xml">external</a> documentation.
cylinder(Quad,Base,Top,Height,Slices,Stacks) ->
  wxe_util:cast(5017, <<Quad:?GLUquadric,Base:?GLdouble,Top:?GLdouble,Height:?GLdouble,Slices:?GLint,Stacks:?GLint>>).

%% @spec (Quad::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluDeleteQuadric.xml">external</a> documentation.
deleteQuadric(Quad) ->
  wxe_util:cast(5018, <<Quad:?GLUquadric>>).

%% @spec (Quad::integer(),Inner::float(),Outer::float(),Slices::integer(),Loops::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluDisk.xml">external</a> documentation.
disk(Quad,Inner,Outer,Slices,Loops) ->
  wxe_util:cast(5019, <<Quad:?GLUquadric,Inner:?GLdouble,Outer:?GLdouble,Slices:?GLint,Loops:?GLint>>).

%% @spec (Error::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluErrorString.xml">external</a> documentation.
errorString(Error) ->
  wxe_util:call(5020, <<Error:?GLenum>>).

%% @spec (Name::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluGetString.xml">external</a> documentation.
getString(Name) ->
  wxe_util:call(5021, <<Name:?GLenum>>).

%% @spec (EyeX::float(),EyeY::float(),EyeZ::float(),CenterX::float(),CenterY::float(),CenterZ::float(),UpX::float(),UpY::float(),UpZ::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluLookAt.xml">external</a> documentation.
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) ->
  wxe_util:cast(5022, <<EyeX:?GLdouble,EyeY:?GLdouble,EyeZ:?GLdouble,CenterX:?GLdouble,CenterY:?GLdouble,CenterZ:?GLdouble,UpX:?GLdouble,UpY:?GLdouble,UpZ:?GLdouble>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluNewQuadric.xml">external</a> documentation.
newQuadric() ->
  wxe_util:call(5023, <<>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluOrtho2D.xml">external</a> documentation.
ortho2D(Left,Right,Bottom,Top) ->
  wxe_util:cast(5024, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble>>).

%% @spec (Quad::integer(),Inner::float(),Outer::float(),Slices::integer(),Loops::integer(),Start::float(),Sweep::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPartialDisk.xml">external</a> documentation.
partialDisk(Quad,Inner,Outer,Slices,Loops,Start,Sweep) ->
  wxe_util:cast(5025, <<Quad:?GLUquadric,Inner:?GLdouble,Outer:?GLdouble,Slices:?GLint,Loops:?GLint,Start:?GLdouble,Sweep:?GLdouble>>).

%% @spec (Fovy::float(),Aspect::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPerspective.xml">external</a> documentation.
perspective(Fovy,Aspect,ZNear,ZFar) ->
  wxe_util:cast(5026, <<Fovy:?GLdouble,Aspect:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (X::float(),Y::float(),DelX::float(),DelY::float(),Viewport::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPickMatrix.xml">external</a> documentation.
pickMatrix(X,Y,DelX,DelY,{V1,V2,V3,V4}) ->
  wxe_util:cast(5027, <<X:?GLdouble,Y:?GLdouble,DelX:?GLdouble,DelY:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (ObjX::float(),ObjY::float(),ObjZ::float(),Model::{float()},Proj::{float()},View::{integer()}) -> {integer(),WinX::float(),WinY::float(),WinZ::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluProject.xml">external</a> documentation.
project(ObjX,ObjY,ObjZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  wxe_util:call(5028, <<ObjX:?GLdouble,ObjY:?GLdouble,ObjZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>);
project(ObjX,ObjY,ObjZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4}) ->
  wxe_util:call(5028, <<ObjX:?GLdouble,ObjY:?GLdouble,ObjZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (Quad::integer(),Draw::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricDrawStyle.xml">external</a> documentation.
quadricDrawStyle(Quad,Draw) ->
  wxe_util:cast(5029, <<Quad:?GLUquadric,Draw:?GLenum>>).

%% @spec (Quad::integer(),Normal::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricNormals.xml">external</a> documentation.
quadricNormals(Quad,Normal) ->
  wxe_util:cast(5030, <<Quad:?GLUquadric,Normal:?GLenum>>).

%% @spec (Quad::integer(),Orientation::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricOrientation.xml">external</a> documentation.
quadricOrientation(Quad,Orientation) ->
  wxe_util:cast(5031, <<Quad:?GLUquadric,Orientation:?GLenum>>).

%% @spec (Quad::integer(),Texture::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricTexture.xml">external</a> documentation.
quadricTexture(Quad,Texture) ->
  wxe_util:cast(5032, <<Quad:?GLUquadric,Texture:?GLboolean>>).

%% @spec (Format::enum(),WIn::integer(),HIn::integer(),TypeIn::enum(),DataIn::binary(),WOut::integer(),HOut::integer(),TypeOut::enum(),DataOut::wx:wx_mem()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluScaleImage.xml">external</a> documentation.
scaleImage(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut) ->
  wxe_util:send_bin(DataIn),
  wxe_util:send_bin(DataOut#wx_mem.bin),
  wxe_util:call(5033, <<Format:?GLenum,WIn:?GLsizei,HIn:?GLsizei,TypeIn:?GLenum,WOut:?GLsizei,HOut:?GLsizei,TypeOut:?GLenum>>).

%% @spec (Quad::integer(),Radius::float(),Slices::integer(),Stacks::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluSphere.xml">external</a> documentation.
sphere(Quad,Radius,Slices,Stacks) ->
  wxe_util:cast(5034, <<Quad:?GLUquadric,Radius:?GLdouble,Slices:?GLint,Stacks:?GLint>>).

%% @spec (WinX::float(),WinY::float(),WinZ::float(),Model::{float()},Proj::{float()},View::{integer()}) -> {integer(),ObjX::float(),ObjY::float(),ObjZ::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluUnProject.xml">external</a> documentation.
unProject(WinX,WinY,WinZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  wxe_util:call(5035, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>);
unProject(WinX,WinY,WinZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4}) ->
  wxe_util:call(5035, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (WinX::float(),WinY::float(),WinZ::float(),ClipW::float(),Model::{float()},Proj::{float()},View::{integer()},NearVal::float(),FarVal::float()) -> {integer(),ObjX::float(),ObjY::float(),ObjZ::float(),ObjW::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluUnProject.xml">external</a> documentation.
unProject4(WinX,WinY,WinZ,ClipW,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4},NearVal,FarVal) ->
  wxe_util:call(5036, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,ClipW:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint,NearVal:?GLdouble,FarVal:?GLdouble>>);
unProject4(WinX,WinY,WinZ,ClipW,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4},NearVal,FarVal) ->
  wxe_util:call(5036, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,ClipW:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint,NearVal:?GLdouble,FarVal:?GLdouble>>).

