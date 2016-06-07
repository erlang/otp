%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/">www.opengl.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(glu).
-compile(inline).
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
-define(GLsync,64/native-unsigned).
-define(GLuint64,64/native-unsigned).
-define(GLint64,64/native-signed).
-type vertex() :: {float(), float(), float()}.
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl or glu.hrl
-type matrix12() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix16() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix() :: matrix12() | matrix16().
-type mem() :: binary() | tuple().   %% Memory block

-export([tesselate/2,build1DMipmapLevels/9,build1DMipmaps/6,build2DMipmapLevels/10,
  build2DMipmaps/7,build3DMipmapLevels/11,build3DMipmaps/8,checkExtension/2,
  cylinder/6,deleteQuadric/1,disk/5,errorString/1,getString/1,lookAt/9,
  newQuadric/0,ortho2D/4,partialDisk/7,perspective/4,pickMatrix/5,project/6,
  quadricDrawStyle/2,quadricNormals/2,quadricOrientation/2,quadricTexture/2,
  scaleImage/9,sphere/4,unProject/6,unProject4/9]).

-import(gl, [call/2,cast/2,send_bin/1]).
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
tesselate({Nx,Ny,Nz}, Vs) ->
  call(5000, <<(length(Vs)):32/native,0:32,
    Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble,
    (<< <<Vx:?GLdouble,Vy:?GLdouble,Vz:?GLdouble >>
        || {Vx,Vy,Vz} <- Vs>>)/binary >>).

%% @doc Builds a subset of one-dimensional mipmap levels
%%
%% ``glu:build1DMipmapLevels'' builds a subset of prefiltered one-dimensional texture maps
%% of decreasing resolutions called a mipmap. This is used for the antialiasing of texture
%% mapped primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  A series of mipmap levels from  `Base'  to  `Max'  is built by decimating   `Data' 
%%  in half  until size   1×1 is reached. At each level, each texel in the halved mipmap
%% level is an average of the corresponding two texels in the larger mipmap level.   {@link gl:texImage1D/8} 
%%  is called to load these mipmap levels from  `Base'  to  `Max' . If  `Max'  is
%% larger than the highest mipmap level for the texture of the specified size, then a GLU
%% error code is returned (see  {@link glu:errorString/1} ) and nothing is loaded. 
%%
%%  For example, if  `Level'  is 2 and  `Width'  is 16, the following levels are possible:
%%   16×1,   8×1,   4×1,  2×1,   1×1. These correspond to levels 2 through 6 respectively.
%% If  `Base'  is 3 and  `Max'  is 5, then only mipmap levels   8×1,  4×1 and   2×1
%% are loaded. However, if  `Max'  is 7, then an error is returned and nothing is loaded
%% since  `Max'  is larger than the highest mipmap level which is, in  this case, 6. 
%%
%%  The highest mipmap level can be derived from the formula  log 2(width×2 level). 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for  `Type'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for  `Level'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmapLevels.xml">external</a> documentation.
-spec build1DMipmapLevels(Target, InternalFormat, Width, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) ->
  send_bin(Data),
  call(5010, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @doc Builds a one-dimensional mipmap
%%
%% ``glu:build1DMipmaps'' builds a series of prefiltered one-dimensional texture maps of
%% decreasing resolutions called a mipmap. This is used for the antialiasing of texture mapped
%% primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  Initially, the  `Width'  of  `Data'  is checked to see if it is a power of 2. If
%% not, a copy of  `Data'  is scaled up or down to the nearest power of 2. (If  `Width' 
%%  is exactly between powers of 2, then the copy of  `Data'  will scale upwards.) This
%% copy will be used for subsequent mipmapping operations described below.  For example, if  `Width' 
%%  is 57, then a copy of  `Data'  will scale up to 64 before mipmapping takes place. 
%%
%%  Then, proxy textures (see  {@link gl:texImage1D/8} ) are used to determine if the implementation
%% can fit the requested texture. If not,  `Width'  is continually halved until it fits. 
%%
%%  Next, a series of mipmap levels is built by decimating a copy of  `Data'  in half
%% until size   1×1 is reached. At each level, each texel in the halved mipmap level is an
%% average of the corresponding two texels in the larger mipmap level. 
%%
%%  {@link gl:texImage1D/8}  is called to load each of these mipmap levels. Level 0 is a copy
%% of  `Data' .  The highest level is  (log 2)(width). For example, if  `Width'  is 64 and the implementation
%% can store a texture of this size, the following mipmap levels are built:   64×1,   32×1,
%%   16×1,   8×1,  4×1,   2×1, and   1×1. These correspond to  levels 0 through 6, respectively.
%% 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for the  `Type'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for the  `Data'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmaps.xml">external</a> documentation.
-spec build1DMipmaps(Target, InternalFormat, Width, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) ->
  send_bin(Data),
  call(5011, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Builds a subset of two-dimensional mipmap levels
%%
%% ``glu:build2DMipmapLevels'' builds a subset of prefiltered two-dimensional texture maps
%% of decreasing resolutions called a mipmap. This is used for the antialiasing of texture
%% mapped primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  A series of mipmap levels from  `Base'  to  `Max'  is built by decimating   `Data' 
%%  in half along both dimensions until size   1×1 is reached. At each level, each texel
%% in the halved mipmap level is an average of the corresponding four texels in the larger
%% mipmap level. (In the case of rectangular images, the decimation will ultimately  reach
%% an   N×1 or   1×N configuration. Here, two texels are averaged instead.)  {@link gl:texImage2D/9} 
%%  is called to load these mipmap levels from  `Base'  to  `Max' . If  `Max'  is
%% larger than the highest mipmap level for the texture of the specified size, then a GLU
%% error code is returned (see  {@link glu:errorString/1} ) and nothing is loaded. 
%%
%%  For example, if  `Level'  is 2 and  `Width'  is 16 and  `Height'  is 8, the
%% following levels are possible:   16×8,   8×4,   4×2,  2×1,   1×1. These correspond to
%% levels 2 through 6 respectively. If  `Base'  is 3 and  `Max'  is 5, then only mipmap
%% levels  8×4,   4×2, and   2×1 are loaded. However, if  `Max'  is 7, then an error is
%% returned and nothing is loaded since  `Max'  is larger than the highest mipmap level
%% which is, in this case, 6. 
%%
%%  The highest mipmap level can be derived from the formula  log 2(max(width height)×2 level). 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for  `Format'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for  `Type'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmapLevels.xml">external</a> documentation.
-spec build2DMipmapLevels(Target, InternalFormat, Width, Height, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) ->
  send_bin(Data),
  call(5012, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @doc Builds a two-dimensional mipmap
%%
%% ``glu:build2DMipmaps'' builds a series of prefiltered two-dimensional texture maps of
%% decreasing resolutions called a mipmap. This is used for the antialiasing of texture-mapped
%% primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  Initially, the  `Width'  and  `Height'  of  `Data'  are checked to see if they
%% are a power of 2. If not, a copy of  `Data'  (not  `Data' ), is scaled up or down
%% to the nearest power of 2. This copy will be used for subsequent mipmapping operations
%% described below. (If  `Width'  or  `Height'  is exactly between powers of 2, then
%% the copy of  `Data'  will scale upwards.) For example, if  `Width'  is 57 and  `Height' 
%%  is 23, then a copy of  `Data'  will scale up to 64 in  `Width'  and down to 16
%% in depth, before mipmapping takes place. 
%%
%%  Then, proxy textures (see  {@link gl:texImage2D/9} ) are used to determine if the implementation
%% can fit the requested texture. If not, both dimensions are continually halved until it
%% fits. (If the OpenGL version is (&lt;= 1.0, both maximum texture dimensions are clamped
%% to the value returned by  {@link gl:getBooleanv/1}  with the argument `?GLU_MAX_TEXTURE_SIZE'
%% .) 
%%
%%  Next, a series of mipmap levels is built by decimating a copy of  `Data'  in half
%% along both dimensions until size   1×1 is reached. At each level, each texel in the halved
%% mipmap level is an average of the corresponding four texels in the larger mipmap level.
%% (In the case of rectangular images, the decimation will ultimately reach an   N×1 or  1×N
%% configuration. Here, two texels are averaged instead.) 
%%
%%  {@link gl:texImage2D/9}  is called to load each of these mipmap levels. Level 0 is a copy
%% of  `Data' . The highest level is (log 2)(max(width height)). For example, if  `Width'  is 64 and  `Height' 
%%  is 16 and the implementation can store a texture of this size, the following mipmap levels
%% are built:   64×16,   32×8,   16×4,  8×2,   4×1,   2×1, and   1×1 These correspond to
%% levels 0 through 6, respectively. 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for  `Format'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for  `Type'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmaps.xml">external</a> documentation.
-spec build2DMipmaps(Target, InternalFormat, Width, Height, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) ->
  send_bin(Data),
  call(5013, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Builds a subset of three-dimensional mipmap levels
%%
%% ``glu:build3DMipmapLevels'' builds a subset of prefiltered three-dimensional texture
%% maps of decreasing resolutions called a mipmap. This is used for the antialiasing of texture
%% mapped primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  A series of mipmap levels from  `Base'  to  `Max'  is built by decimating  `Data' 
%%  in half along both dimensions until size  1×1×1 is reached. At each level, each texel
%% in the halved mipmap level is an average of the corresponding eight texels in the larger
%% mipmap level. (If exactly one of the dimensions is 1, four texels are averaged. If exactly
%% two of the dimensions are 1, two texels are averaged.)  {@link gl:texImage3D/10}  is called
%% to load these mipmap levels from  `Base'  to  `Max' . If  `Max'  is larger than
%% the highest mipmap level for the texture of the specified size, then a GLU error code
%% is returned (see  {@link glu:errorString/1} ) and nothing is loaded. 
%%
%%  For example, if  `Level'  is 2 and  `Width'  is 16,  `Height'  is 8 and  `Depth' 
%%  is 4, the following levels are possible:   16×8×4,   8×4×2,  4×2×1,   2×1×1,  1×1×1.
%% These correspond to levels 2 through 6 respectively. If  `Base'  is 3 and  `Max' 
%% is 5, then only mipmap levels   8×4×2,  4×2×1, and   2×1×1 are loaded. However, if  `Max' 
%%  is 7, then an error is returned and nothing is loaded, since  `Max'  is larger than
%% the highest mipmap level which is, in this case, 6. 
%%
%%  The highest mipmap level can be derived from the formula  log 2(max(width height depth)×2 level). 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for  `Format'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for  `Type'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmapLevels.xml">external</a> documentation.
-spec build3DMipmapLevels(Target, InternalFormat, Width, Height, Depth, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) ->
  send_bin(Data),
  call(5014, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @doc Builds a three-dimensional mipmap
%%
%% ``glu:build3DMipmaps'' builds a series of prefiltered three-dimensional texture maps
%% of decreasing resolutions called a mipmap. This is used for the antialiasing of texture-mapped
%% primitives. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  Initially, the  `Width' ,  `Height'  and  `Depth'  of  `Data'  are checked
%% to see if they are a power of 2. If not, a copy of  `Data'  is made and scaled up or
%% down to the nearest power of 2. (If  `Width' ,  `Height' , or  `Depth'  is exactly
%% between powers of 2, then the copy of  `Data'  will scale upwards.) This copy will
%% be used for subsequent mipmapping operations described below. For example, if  `Width' 
%% is 57,  `Height'  is 23, and  `Depth'  is 24, then a copy of  `Data'  will scale
%% up to 64 in width, down to 16 in height, and up to 32 in depth before mipmapping takes
%% place. 
%%
%%  Then, proxy textures (see  {@link gl:texImage3D/10} ) are used to determine if the implementation
%% can fit the requested texture. If not, all three dimensions are continually halved until
%% it fits.  
%%
%%  Next, a series of mipmap levels is built by decimating a copy of  `Data'  in half
%% along all three dimensions until size   1×1×1 is reached. At each level, each texel in
%% the halved mipmap level is an average of the corresponding eight texels in the larger
%% mipmap level. (If exactly one of the dimensions is 1, four texels are averaged. If exactly
%% two of the dimensions are 1, two texels are averaged.) 
%%
%%  {@link gl:texImage3D/10}  is called to load each of these mipmap levels. Level 0 is a copy
%% of  `Data' . The highest level is (log 2)(max(width height depth)). For example, if  `Width'  is 64,  `Height' 
%% is 16, and  `Depth'  is 32, and the implementation can store a texture of this size,
%% the following mipmap levels are built:   64×16×32,  32×8×16,   16×4×8,  8×2×4,   4×1×2, 
%% 2×1×1, and   1×1×1. These correspond to levels 0 through 6, respectively. 
%%
%%  See the  {@link gl:texImage1D/8}  reference page for a description of the acceptable values
%% for  `Format'  parameter. See the  {@link gl:drawPixels/5}   reference page for a description
%% of the acceptable values  for  `Type'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmaps.xml">external</a> documentation.
-spec build3DMipmaps(Target, InternalFormat, Width, Height, Depth, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) ->
  send_bin(Data),
  call(5015, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Determines if an extension name is supported
%%
%% ``glu:checkExtension'' returns `?GLU_TRUE' if  `ExtName'  is supported otherwise
%%  `?GLU_FALSE' is returned. 
%%
%%  This is used to check for the presence for OpenGL, GLU, or GLX extension names by passing
%% the extension strings returned by  {@link gl:getString/1} ,   {@link glu:getString/1} , see `glXGetClientString'
%% , see `glXQueryExtensionsString', or see `glXQueryServerString', respectively,
%% as  `ExtString' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluCheckExtension.xml">external</a> documentation.
-spec checkExtension(ExtName, ExtString) -> 0|1 when ExtName :: string(),ExtString :: string().
checkExtension(ExtName,ExtString) ->
  call(5016, <<(list_to_binary([ExtName|[0]]))/binary,0:((8-((length(ExtName)+ 1) rem 8)) rem 8),(list_to_binary([ExtString|[0]]))/binary,0:((8-((length(ExtString)+ 1) rem 8)) rem 8)>>).

%% @doc Draw a cylinder
%%
%% ``glu:cylinder'' draws a cylinder oriented along the `z' axis. The base of the
%% cylinder is placed at `z' = 0 and the top at   z=height. Like a sphere, a cylinder
%% is subdivided around the `z' axis into slices and along the  `z' axis into stacks.
%% 
%%
%%  Note that if  `Top'  is set to 0.0, this routine generates a cone. 
%%
%%  If the orientation is set to `?GLU_OUTSIDE'  (with  {@link glu:quadricOrientation/2} ),
%% then any generated normals point away from the `z' axis. Otherwise, they point toward
%% the  `z' axis. 
%%
%%  If texturing is turned on (with  {@link glu:quadricTexture/2} ), then texture  coordinates
%% are generated so that `t' ranges linearly from 0.0  at `z' = 0 to 1.0 at `z'
%%  =  `Height' , and `s'  ranges from 0.0 at the +`y' axis, to 0.25 at the +`x'
%%  axis,  to 0.5 at the -`y' axis, to 0.75 at the -`x' axis,  and back to 1.0
%% at the +`y' axis. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluCylinder.xml">external</a> documentation.
-spec cylinder(Quad, Base, Top, Height, Slices, Stacks) -> 'ok' when Quad :: integer(),Base :: float(),Top :: float(),Height :: float(),Slices :: integer(),Stacks :: integer().
cylinder(Quad,Base,Top,Height,Slices,Stacks) ->
  cast(5017, <<Quad:?GLUquadric,Base:?GLdouble,Top:?GLdouble,Height:?GLdouble,Slices:?GLint,Stacks:?GLint>>).

%% @doc Destroy a quadrics object
%%
%% ``glu:deleteQuadric'' destroys the quadrics object (created with  {@link glu:newQuadric/0} )
%% and frees any memory it uses.  Once ``glu:deleteQuadric'' has been called,  `Quad' 
%% cannot be used again. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluDeleteQuadric.xml">external</a> documentation.
-spec deleteQuadric(Quad) -> 'ok' when Quad :: integer().
deleteQuadric(Quad) ->
  cast(5018, <<Quad:?GLUquadric>>).

%% @doc Draw a disk
%%
%% ``glu:disk'' renders a disk on the `z' = 0 plane. The disk has a radius of   `Outer' 
%%  and contains a concentric circular hole with a radius  of  `Inner' . If  `Inner' 
%% is 0, then no hole is generated. The disk is subdivided around the `z' axis into
%% slices (like pizza slices) and also about the `z' axis into rings  (as specified by  `Slices' 
%%  and  `Loops' , respectively). 
%%
%%  With respect to orientation, the +`z' side of the disk is considered to be  ``outside''
%% (see  {@link glu:quadricOrientation/2} ). This means that if the orientation is set to `?GLU_OUTSIDE'
%% , then any normals generated  point along the +`z' axis. Otherwise, they point along
%% the -`z'  axis. 
%%
%%  If texturing has been turned on (with  {@link glu:quadricTexture/2} ),  texture coordinates
%% are generated linearly such that where   r=outer, the value at (`r', 0, 0) is  (1,
%% 0.5), at (0, `r', 0) it is (0.5, 1), at (-`r', 0, 0)  it is (0, 0.5), and  at
%% (0, -`r', 0) it is (0.5, 0). 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluDisk.xml">external</a> documentation.
-spec disk(Quad, Inner, Outer, Slices, Loops) -> 'ok' when Quad :: integer(),Inner :: float(),Outer :: float(),Slices :: integer(),Loops :: integer().
disk(Quad,Inner,Outer,Slices,Loops) ->
  cast(5019, <<Quad:?GLUquadric,Inner:?GLdouble,Outer:?GLdouble,Slices:?GLint,Loops:?GLint>>).

%% @doc Produce an error string from a GL or GLU error code
%%
%% ``glu:errorString'' produces an error string from a GL or GLU error code. The string
%% is in ISO Latin 1 format. For example, ``glu:errorString''(`?GLU_OUT_OF_MEMORY')
%% returns the string  `out of memory'. 
%%
%%  The standard GLU error codes are `?GLU_INVALID_ENUM',  `?GLU_INVALID_VALUE',
%% and `?GLU_OUT_OF_MEMORY'. Certain other GLU functions can return specialized error
%% codes through callbacks. See the  {@link gl:getError/0}  reference page for the list of 
%% GL error codes. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluErrorString.xml">external</a> documentation.
-spec errorString(Error) -> string() when Error :: enum().
errorString(Error) ->
  call(5020, <<Error:?GLenum>>).

%% @doc Return a string describing the GLU version or GLU extensions 
%%
%% ``glu:getString'' returns a pointer to a static string describing the  GLU version or
%% the GLU extensions that are supported. 
%%
%%  The version number is one of the following forms:  
%%
%% `major_number.minor_number'`major_number.minor_number.release_number'.  
%%
%%  The version string is of the following form:  
%%
%% `version number&lt;space&gt;vendor-specific information'
%%
%%  Vendor-specific information is optional. Its format and contents depend on the implementation.
%% 
%%
%%  The standard GLU contains a basic set of features and capabilities. If a company or group
%% of companies wish to support other features, these may be included as extensions to the
%% GLU.  If  `Name'  is  `?GLU_EXTENSIONS', then ``glu:getString'' returns a space-separated
%% list of names of supported GLU extensions. (Extension names never contain spaces.) 
%%
%%  All strings are null-terminated. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluGetString.xml">external</a> documentation.
-spec getString(Name) -> string() when Name :: enum().
getString(Name) ->
  call(5021, <<Name:?GLenum>>).

%% @doc Define a viewing transformation
%%
%% ``glu:lookAt'' creates a viewing matrix derived from an eye point, a reference point
%% indicating the center of the scene, and an `UP' vector.  
%%
%%  The matrix maps the reference point to the negative `z' axis and the eye point to
%% the origin. When a typical projection matrix is used, the center of the scene therefore
%% maps to the center of the viewport. Similarly, the direction described by the `UP'
%% vector projected onto the viewing plane is mapped to the positive `y'  axis so that
%% it points upward in the viewport. The `UP' vector must not be parallel to the line
%% of sight from the eye point to the reference point. 
%%
%%  Let  
%%
%%  F=(centerX-eyeX centerY-eyeY centerZ-eyeZ)
%%
%%  Let `UP' be the vector  (upX upY upZ).  
%%
%%  Then normalize as follows:   f=F/(||F||)
%%
%%  UP"=UP/(||UP||)
%%
%%  Finally, let   s=f×UP", and   u=s×f. 
%%
%%  M is then constructed as follows:  M=(s[0] s[1] s[2] 0 u[0] u[1] u[2] 0-f[0]-f[1]-f[2] 0 0 0 0 1)
%%
%%  and ``glu:lookAt'' is equivalent to   glMultMatrixf(M); glTranslated(-eyex, -eyey,
%% -eyez); 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluLookAt.xml">external</a> documentation.
-spec lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) -> 'ok' when EyeX :: float(),EyeY :: float(),EyeZ :: float(),CenterX :: float(),CenterY :: float(),CenterZ :: float(),UpX :: float(),UpY :: float(),UpZ :: float().
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) ->
  cast(5022, <<EyeX:?GLdouble,EyeY:?GLdouble,EyeZ:?GLdouble,CenterX:?GLdouble,CenterY:?GLdouble,CenterZ:?GLdouble,UpX:?GLdouble,UpY:?GLdouble,UpZ:?GLdouble>>).

%% @doc Create a quadrics object
%%
%% ``glu:newQuadric'' creates and returns a pointer to a new quadrics object. This object
%% must be referred to when calling quadrics rendering and control functions. A return value
%% of 0 means that there is not enough memory to allocate the object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluNewQuadric.xml">external</a> documentation.
-spec newQuadric() -> integer().
newQuadric() ->
  call(5023, <<>>).

%% @doc Define a 2D orthographic projection matrix
%%
%% ``glu:ortho2D'' sets up a two-dimensional orthographic viewing region.  This is equivalent
%% to calling  {@link gl:ortho/6}  with   near=-1 and   far=1. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluOrtho2D.xml">external</a> documentation.
-spec ortho2D(Left, Right, Bottom, Top) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float().
ortho2D(Left,Right,Bottom,Top) ->
  cast(5024, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble>>).

%% @doc Draw an arc of a disk
%%
%% ``glu:partialDisk'' renders a partial disk on the   z=0 plane. A partial disk is  similar
%% to a full disk, except that only the subset of the disk from  `Start'  through  `Start' 
%%  +  `Sweep'  is included (where 0 degrees is along the  +f2yf axis, 90 degrees along
%% the +`x' axis, 180 degrees along the -`y' axis, and  270 degrees along the -`x'
%%  axis). 
%%
%%  The partial disk has a radius of   `Outer'  and contains a concentric circular hole
%% with a radius  of  `Inner' . If  `Inner'  is 0, then no hole is generated. The partial
%% disk is subdivided around the `z' axis into slices (like pizza slices) and also about
%% the `z' axis into rings  (as specified by  `Slices'  and  `Loops' , respectively).
%% 
%%
%%  With respect to orientation, the +`z'  side of the partial disk is considered to
%%  be outside (see  {@link glu:quadricOrientation/2} ). This means that if the  orientation
%% is set to `?GLU_OUTSIDE', then any normals generated  point along the +`z' axis.
%% Otherwise, they point along the -`z'  axis. 
%%
%%  If texturing is turned on (with  {@link glu:quadricTexture/2} ), texture coordinates are
%% generated linearly such that where   r=outer, the value at (`r', 0, 0) is  (1.0,
%% 0.5), at (0, `r', 0) it is (0.5, 1.0), at (-`r', 0, 0)  it is (0.0, 0.5), and
%%  at (0, -`r', 0) it is (0.5, 0.0). 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPartialDisk.xml">external</a> documentation.
-spec partialDisk(Quad, Inner, Outer, Slices, Loops, Start, Sweep) -> 'ok' when Quad :: integer(),Inner :: float(),Outer :: float(),Slices :: integer(),Loops :: integer(),Start :: float(),Sweep :: float().
partialDisk(Quad,Inner,Outer,Slices,Loops,Start,Sweep) ->
  cast(5025, <<Quad:?GLUquadric,Inner:?GLdouble,Outer:?GLdouble,Slices:?GLint,Loops:?GLint,Start:?GLdouble,Sweep:?GLdouble>>).

%% @doc Set up a perspective projection matrix
%%
%% ``glu:perspective'' specifies a viewing frustum into the world coordinate system. In
%% general, the aspect ratio in ``glu:perspective'' should match the aspect ratio of the
%% associated viewport. For example,   aspect=2.0 means  the viewer's angle of view is twice
%% as wide in `x' as it is in `y'. If the viewport is twice as wide as it is tall,
%% it displays the image without distortion. 
%%
%%  The matrix generated by ``glu:perspective'' is multipled by the current matrix, just
%% as if  {@link gl:multMatrixd/1}  were called with the generated matrix. To load the perspective
%% matrix onto the current matrix stack instead, precede the call to ``glu:perspective''
%% with a call to  {@link gl:loadIdentity/0} . 
%%
%%  Given `f' defined as follows: 
%%
%%  f=cotangent(fovy/2) The generated matrix is 
%%
%% (f/aspect 0 0 0 0 f 0 0 0 0(zFar+zNear)/(zNear-zFar)(2×zFar×zNear)/(zNear-zFar) 0 0 -1 0)
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPerspective.xml">external</a> documentation.
-spec perspective(Fovy, Aspect, ZNear, ZFar) -> 'ok' when Fovy :: float(),Aspect :: float(),ZNear :: float(),ZFar :: float().
perspective(Fovy,Aspect,ZNear,ZFar) ->
  cast(5026, <<Fovy:?GLdouble,Aspect:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @doc Define a picking region
%%
%% ``glu:pickMatrix'' creates a projection matrix that can be used to restrict drawing
%% to a small region of the viewport. This is typically useful to determine what objects
%% are being drawn near the cursor. Use ``glu:pickMatrix'' to restrict drawing to a small
%% region around the cursor. Then, enter selection mode (with  {@link gl:renderMode/1} ) and
%% rerender the scene. All primitives that would have been drawn near the cursor are identified
%% and stored in the selection buffer. 
%%
%%  The matrix created by ``glu:pickMatrix'' is multiplied by the current matrix just as
%% if  {@link gl:multMatrixd/1}  is called with the generated matrix. To effectively use the
%% generated pick matrix for picking, first call  {@link gl:loadIdentity/0}  to load an identity
%% matrix onto the perspective matrix stack. Then call ``glu:pickMatrix'', and, finally,
%% call a command (such as  {@link glu:perspective/4} ) to multiply the perspective matrix by
%% the pick matrix. 
%%
%%  When using ``glu:pickMatrix'' to pick NURBS, be careful to turn off the NURBS  property
%% `?GLU_AUTO_LOAD_MATRIX'. If `?GLU_AUTO_LOAD_MATRIX' is not turned off, then
%% any NURBS surface rendered is subdivided differently with the pick matrix than the way
%% it was subdivided without the pick matrix. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPickMatrix.xml">external</a> documentation.
-spec pickMatrix(X, Y, DelX, DelY, Viewport) -> 'ok' when X :: float(),Y :: float(),DelX :: float(),DelY :: float(),Viewport :: {integer(),integer(),integer(),integer()}.
pickMatrix(X,Y,DelX,DelY,{V1,V2,V3,V4}) ->
  cast(5027, <<X:?GLdouble,Y:?GLdouble,DelX:?GLdouble,DelY:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc Map object coordinates to window coordinates
%%
%% ``glu:project'' transforms the specified object coordinates into window coordinates
%% using  `Model' ,  `Proj' , and  `View' . The result is stored  in  `WinX' ,  `WinY' 
%% , and  `WinZ' . A return value of  `?GLU_TRUE' indicates success, a return value
%% of `?GLU_FALSE' indicates failure. 
%%
%%  To compute the coordinates, let   v=(objX objY objZ 1.0) represented as a matrix with 4 rows and 1 column.
%% Then ``glu:project'' computes   v" as follows:  
%%
%%  v"=P×M×v
%%
%%  where   P is the current projection matrix  `Proj'  and   M is the current modelview
%% matrix  `Model'  (both represented as  4×4 matrices in column-major order). 
%%
%%  The window coordinates are then computed as follows:  
%%
%%  winX=view(0)+view(2)×(v"(0)+1)/2
%%
%%  winY=view(1)+view(3)×(v"(1)+1)/2
%%
%%  winZ=(v"(2)+1)/2
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluProject.xml">external</a> documentation.
-spec project(ObjX, ObjY, ObjZ, Model, Proj, View) -> {integer(),WinX :: float(),WinY :: float(),WinZ :: float()} when ObjX :: float(),ObjY :: float(),ObjZ :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()}.
project(ObjX,ObjY,ObjZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  call(5028, <<ObjX:?GLdouble,ObjY:?GLdouble,ObjZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>);
project(ObjX,ObjY,ObjZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4}) ->
  call(5028, <<ObjX:?GLdouble,ObjY:?GLdouble,ObjZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc Specify the draw style desired for quadrics
%%
%% ``glu:quadricDrawStyle'' specifies the draw style for quadrics rendered with  `Quad' .
%% The legal values are as follows: 
%%
%% `?GLU_FILL':  Quadrics are rendered with polygon primitives. The polygons  are drawn
%% in a counterclockwise fashion with respect to their normals (as defined with  {@link glu:quadricOrientation/2} 
%% ). 
%%
%% `?GLU_LINE':  Quadrics are rendered as a set of lines. 
%%
%% `?GLU_SILHOUETTE':  Quadrics are rendered as a set of lines, except that edges separating
%% coplanar faces will not be drawn. 
%%
%% `?GLU_POINT':  Quadrics are rendered as a set of points. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricDrawStyle.xml">external</a> documentation.
-spec quadricDrawStyle(Quad, Draw) -> 'ok' when Quad :: integer(),Draw :: enum().
quadricDrawStyle(Quad,Draw) ->
  cast(5029, <<Quad:?GLUquadric,Draw:?GLenum>>).

%% @doc Specify what kind of normals are desired for quadrics
%%
%% ``glu:quadricNormals'' specifies what kind of normals are desired for quadrics rendered
%% with  `Quad' . The legal values are as follows: 
%%
%% `?GLU_NONE':  No normals are generated. 
%%
%% `?GLU_FLAT':  One normal is generated for every facet of a quadric. 
%%
%% `?GLU_SMOOTH':  One normal is generated for every vertex of a quadric. This is the
%% initial value. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricNormals.xml">external</a> documentation.
-spec quadricNormals(Quad, Normal) -> 'ok' when Quad :: integer(),Normal :: enum().
quadricNormals(Quad,Normal) ->
  cast(5030, <<Quad:?GLUquadric,Normal:?GLenum>>).

%% @doc Specify inside/outside orientation for quadrics
%%
%% ``glu:quadricOrientation'' specifies what kind of orientation is desired for quadrics
%% rendered  with  `Quad' . The  `Orientation'  values are as follows: 
%%
%% `?GLU_OUTSIDE':  Quadrics are drawn with normals pointing outward (the initial value).
%% 
%%
%% `?GLU_INSIDE':  Quadrics are drawn with normals pointing inward. 
%%
%%  Note that the interpretation of `outward' and `inward' depends on the quadric
%% being drawn. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricOrientation.xml">external</a> documentation.
-spec quadricOrientation(Quad, Orientation) -> 'ok' when Quad :: integer(),Orientation :: enum().
quadricOrientation(Quad,Orientation) ->
  cast(5031, <<Quad:?GLUquadric,Orientation:?GLenum>>).

%% @doc Specify if texturing is desired for quadrics
%%
%% ``glu:quadricTexture'' specifies if texture coordinates should be generated for quadrics
%% rendered with  `Quad' . If the value of  `Texture'  is `?GLU_TRUE', then texture
%% coordinates  are generated, and if  `Texture'  is `?GLU_FALSE', they are not.
%% The initial value is `?GLU_FALSE'. 
%%
%%  The manner in which texture coordinates are generated depends  upon the specific quadric
%% rendered. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluQuadricTexture.xml">external</a> documentation.
-spec quadricTexture(Quad, Texture) -> 'ok' when Quad :: integer(),Texture :: 0|1.
quadricTexture(Quad,Texture) ->
  cast(5032, <<Quad:?GLUquadric,Texture:?GLboolean>>).

%% @doc Scale an image to an arbitrary size
%%
%% ``glu:scaleImage'' scales a pixel image using the appropriate pixel store modes to 
%% unpack data from the source image and pack data into the destination image. 
%%
%%  When shrinking an image, ``glu:scaleImage'' uses a box filter to sample the source
%% image and create pixels for the destination image. When magnifying an image, the pixels
%% from the source image are linearly interpolated to create the destination image. 
%%
%%  A return value of zero indicates success, otherwise a GLU error code is returned (see  {@link glu:errorString/1} 
%% ). 
%%
%%  See the  {@link gl:readPixels/7}  reference page for a description of the acceptable values
%% for the  `Format' ,  `TypeIn' , and  `TypeOut'  parameters. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluScaleImage.xml">external</a> documentation.
-spec scaleImage(Format, WIn, HIn, TypeIn, DataIn, WOut, HOut, TypeOut, DataOut) -> integer() when Format :: enum(),WIn :: integer(),HIn :: integer(),TypeIn :: enum(),DataIn :: binary(),WOut :: integer(),HOut :: integer(),TypeOut :: enum(),DataOut :: mem().
scaleImage(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut) ->
  send_bin(DataIn),
  send_bin(DataOut),
  call(5033, <<Format:?GLenum,WIn:?GLsizei,HIn:?GLsizei,TypeIn:?GLenum,WOut:?GLsizei,HOut:?GLsizei,TypeOut:?GLenum>>).

%% @doc Draw a sphere
%%
%% ``glu:sphere'' draws a sphere of the given radius centered around the origin. The sphere
%% is subdivided around the `z' axis into slices and along the  `z' axis  into
%% stacks (similar to lines of longitude and latitude). 
%%
%%  If the orientation is set to `?GLU_OUTSIDE'  (with  {@link glu:quadricOrientation/2} ),
%% then any normals generated  point away from the center of the sphere. Otherwise, they
%% point toward the center of the sphere. 
%%
%%  If texturing is turned on (with  {@link glu:quadricTexture/2} ), then texture  coordinates
%% are  generated so that `t' ranges from 0.0 at   z=-radius to 1.0 at   z=radius (`t'
%%  increases linearly along longitudinal lines), and `s' ranges from 0.0 at the +`y'
%%  axis, to 0.25 at the  +`x' axis,  to 0.5 at the -`y' axis, to 0.75 at the -`x'
%%  axis, and back to 1.0  at the +`y' axis. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluSphere.xml">external</a> documentation.
-spec sphere(Quad, Radius, Slices, Stacks) -> 'ok' when Quad :: integer(),Radius :: float(),Slices :: integer(),Stacks :: integer().
sphere(Quad,Radius,Slices,Stacks) ->
  cast(5034, <<Quad:?GLUquadric,Radius:?GLdouble,Slices:?GLint,Stacks:?GLint>>).

%% @doc Map window coordinates to object coordinates
%%
%% ``glu:unProject'' maps the specified window coordinates into object  coordinates using  `Model' 
%% ,  `Proj' , and  `View' . The result is stored in  `ObjX' ,  `ObjY' , and  `ObjZ' 
%% . A return value of  `?GLU_TRUE' indicates success; a return value of `?GLU_FALSE'
%%  indicates failure. 
%%
%%  To compute the coordinates  (objX objY objZ), ``glu:unProject'' multiplies the normalized device coordinates
%% by the inverse of  `Model'  *  `Proj'  as follows: 
%%
%% (objX objY objZ W)=INV(P  M) ((2(winX-view[0]))/(view[2])-1(2(winY-view[1]))/(view[3])-1 2(winZ)-1 1) INV denotes matrix inversion.  W is an unused variable, included for consistent
%% matrix notation. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluUnProject.xml">external</a> documentation.
-spec unProject(WinX, WinY, WinZ, Model, Proj, View) -> {integer(),ObjX :: float(),ObjY :: float(),ObjZ :: float()} when WinX :: float(),WinY :: float(),WinZ :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()}.
unProject(WinX,WinY,WinZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  call(5035, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>);
unProject(WinX,WinY,WinZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4}) ->
  call(5035, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc 
%% See {@link unProject/6}
-spec unProject4(WinX, WinY, WinZ, ClipW, Model, Proj, View, NearVal, FarVal) -> {integer(),ObjX :: float(),ObjY :: float(),ObjZ :: float(),ObjW :: float()} when WinX :: float(),WinY :: float(),WinZ :: float(),ClipW :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()},NearVal :: float(),FarVal :: float().
unProject4(WinX,WinY,WinZ,ClipW,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4},NearVal,FarVal) ->
  call(5036, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,ClipW:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint,NearVal:?GLdouble,FarVal:?GLdouble>>);
unProject4(WinX,WinY,WinZ,ClipW,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12},{V1,V2,V3,V4},NearVal,FarVal) ->
  call(5036, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,ClipW:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,0:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,0:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,0:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,1:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint,NearVal:?GLdouble,FarVal:?GLdouble>>).

