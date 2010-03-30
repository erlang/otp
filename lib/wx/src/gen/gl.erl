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

%% OPENGL API

%% This file is generated DO NOT EDIT

%% @doc  Standard OpenGL api.
%% See <a href="http://www.opengl.org/sdk/docs/man/">www.opengl.org</a>
%%
%% Booleans are represented by integers 0 and 1.

%% @type wx_mem(). see wx.erl on memory allocation functions
%% @type enum().   An integer defined in gl.hrl
%% @type offset(). An integer which is an offset in an array
%% @type clamp().  A float clamped between 0.0 - 1.0
-module(gl).

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

-export([accum/2,alphaFunc/2,areTexturesResident/1,arrayElement/1,'begin'/1,
  bindTexture/2,bitmap/7,blendFunc/2,callList/1,callLists/1,clear/1,clearAccum/4,
  clearColor/4,clearDepth/1,clearIndex/1,clearStencil/1,clipPlane/2,
  color3b/3,color3bv/1,color3d/3,color3dv/1,color3f/3,color3fv/1,color3i/3,
  color3iv/1,color3s/3,color3sv/1,color3ub/3,color3ubv/1,color3ui/3,color3uiv/1,
  color3us/3,color3usv/1,color4b/4,color4bv/1,color4d/4,color4dv/1,color4f/4,
  color4fv/1,color4i/4,color4iv/1,color4s/4,color4sv/1,color4ub/4,color4ubv/1,
  color4ui/4,color4uiv/1,color4us/4,color4usv/1,colorMask/4,colorMaterial/2,
  colorPointer/4,copyPixels/5,copyTexImage1D/7,copyTexImage2D/8,copyTexSubImage1D/6,
  copyTexSubImage2D/8,cullFace/1,deleteLists/2,deleteTextures/1,depthFunc/1,
  depthMask/1,depthRange/2,disable/1,disableClientState/1,drawArrays/3,
  drawBuffer/1,drawElements/4,drawPixels/5,edgeFlag/1,edgeFlagPointer/2,
  edgeFlagv/1,enable/1,enableClientState/1,'end'/0,endList/0,evalCoord1d/1,
  evalCoord1dv/1,evalCoord1f/1,evalCoord1fv/1,evalCoord2d/2,evalCoord2dv/1,
  evalCoord2f/2,evalCoord2fv/1,evalMesh1/3,evalMesh2/5,evalPoint1/1,
  evalPoint2/2,feedbackBuffer/3,finish/0,flush/0,fogf/2,fogfv/2,fogi/2,
  fogiv/2,frontFace/1,frustum/6,genLists/1,genTextures/1,getBooleanv/1,
  getClipPlane/1,getDoublev/1,getError/0,getFloatv/1,getIntegerv/1,getLightfv/2,
  getLightiv/2,getMapdv/3,getMapfv/3,getMapiv/3,getMaterialfv/2,getMaterialiv/2,
  getPixelMapfv/2,getPixelMapuiv/2,getPixelMapusv/2,getPolygonStipple/0,
  getString/1,getTexEnvfv/2,getTexEnviv/2,getTexGendv/2,getTexGenfv/2,
  getTexGeniv/2,getTexImage/5,getTexLevelParameterfv/3,getTexLevelParameteriv/3,
  getTexParameterfv/2,getTexParameteriv/2,hint/2,indexMask/1,indexPointer/3,
  indexd/1,indexdv/1,indexf/1,indexfv/1,indexi/1,indexiv/1,indexs/1,indexsv/1,
  indexub/1,indexubv/1,initNames/0,interleavedArrays/3,isEnabled/1,isList/1,
  isTexture/1,lightModelf/2,lightModelfv/2,lightModeli/2,lightModeliv/2,
  lightf/3,lightfv/3,lighti/3,lightiv/3,lineStipple/2,lineWidth/1,listBase/1,
  loadIdentity/0,loadMatrixd/1,loadMatrixf/1,loadName/1,logicOp/1,map1d/6,
  map1f/6,map2d/10,map2f/10,mapGrid1d/3,mapGrid1f/3,mapGrid2d/6,mapGrid2f/6,
  materialf/3,materialfv/3,materiali/3,materialiv/3,matrixMode/1,multMatrixd/1,
  multMatrixf/1,newList/2,normal3b/3,normal3bv/1,normal3d/3,normal3dv/1,
  normal3f/3,normal3fv/1,normal3i/3,normal3iv/1,normal3s/3,normal3sv/1,
  normalPointer/3,ortho/6,passThrough/1,pixelMapfv/3,pixelMapuiv/3,pixelMapusv/3,
  pixelStoref/2,pixelStorei/2,pixelTransferf/2,pixelTransferi/2,pixelZoom/2,
  pointSize/1,polygonMode/2,polygonOffset/2,polygonStipple/1,popAttrib/0,
  popClientAttrib/0,popMatrix/0,popName/0,prioritizeTextures/2,pushAttrib/1,
  pushClientAttrib/1,pushMatrix/0,pushName/1,rasterPos2d/2,rasterPos2dv/1,
  rasterPos2f/2,rasterPos2fv/1,rasterPos2i/2,rasterPos2iv/1,rasterPos2s/2,
  rasterPos2sv/1,rasterPos3d/3,rasterPos3dv/1,rasterPos3f/3,rasterPos3fv/1,
  rasterPos3i/3,rasterPos3iv/1,rasterPos3s/3,rasterPos3sv/1,rasterPos4d/4,
  rasterPos4dv/1,rasterPos4f/4,rasterPos4fv/1,rasterPos4i/4,rasterPos4iv/1,
  rasterPos4s/4,rasterPos4sv/1,readBuffer/1,readPixels/7,rectd/4,rectdv/2,
  rectf/4,rectfv/2,recti/4,rectiv/2,rects/4,rectsv/2,renderMode/1,rotated/4,
  rotatef/4,scaled/3,scalef/3,scissor/4,selectBuffer/2,shadeModel/1,stencilFunc/3,
  stencilMask/1,stencilOp/3,texCoord1d/1,texCoord1dv/1,texCoord1f/1,
  texCoord1fv/1,texCoord1i/1,texCoord1iv/1,texCoord1s/1,texCoord1sv/1,
  texCoord2d/2,texCoord2dv/1,texCoord2f/2,texCoord2fv/1,texCoord2i/2,
  texCoord2iv/1,texCoord2s/2,texCoord2sv/1,texCoord3d/3,texCoord3dv/1,
  texCoord3f/3,texCoord3fv/1,texCoord3i/3,texCoord3iv/1,texCoord3s/3,
  texCoord3sv/1,texCoord4d/4,texCoord4dv/1,texCoord4f/4,texCoord4fv/1,
  texCoord4i/4,texCoord4iv/1,texCoord4s/4,texCoord4sv/1,texCoordPointer/4,
  texEnvf/3,texEnvfv/3,texEnvi/3,texEnviv/3,texGend/3,texGendv/3,texGenf/3,
  texGenfv/3,texGeni/3,texGeniv/3,texImage1D/8,texImage2D/9,texParameterf/3,
  texParameterfv/3,texParameteri/3,texParameteriv/3,texSubImage1D/7,
  texSubImage2D/9,translated/3,translatef/3,vertex2d/2,vertex2dv/1,vertex2f/2,
  vertex2fv/1,vertex2i/2,vertex2iv/1,vertex2s/2,vertex2sv/1,vertex3d/3,
  vertex3dv/1,vertex3f/3,vertex3fv/1,vertex3i/3,vertex3iv/1,vertex3s/3,
  vertex3sv/1,vertex4d/4,vertex4dv/1,vertex4f/4,vertex4fv/1,vertex4i/4,
  vertex4iv/1,vertex4s/4,vertex4sv/1,vertexPointer/4,viewport/4,blendColor/4,
  blendEquation/1,drawRangeElements/6,texImage3D/10,texSubImage3D/11,
  copyTexSubImage3D/9,colorTable/6,colorTableParameterfv/3,colorTableParameteriv/3,
  copyColorTable/5,getColorTable/4,getColorTableParameterfv/2,getColorTableParameteriv/2,
  colorSubTable/6,copyColorSubTable/5,convolutionFilter1D/6,convolutionFilter2D/7,
  convolutionParameterf/3,convolutionParameterfv/3,convolutionParameteri/3,
  convolutionParameteriv/3,copyConvolutionFilter1D/5,copyConvolutionFilter2D/6,
  getConvolutionFilter/4,getConvolutionParameterfv/2,getConvolutionParameteriv/2,
  separableFilter2D/8,getHistogram/5,getHistogramParameterfv/2,getHistogramParameteriv/2,
  getMinmax/5,getMinmaxParameterfv/2,getMinmaxParameteriv/2,histogram/4,
  minmax/3,resetHistogram/1,resetMinmax/1,activeTexture/1,sampleCoverage/2,
  compressedTexImage3D/9,compressedTexImage2D/8,compressedTexImage1D/7,
  compressedTexSubImage3D/11,compressedTexSubImage2D/9,compressedTexSubImage1D/7,
  getCompressedTexImage/3,clientActiveTexture/1,multiTexCoord1d/2,
  multiTexCoord1dv/2,multiTexCoord1f/2,multiTexCoord1fv/2,multiTexCoord1i/2,
  multiTexCoord1iv/2,multiTexCoord1s/2,multiTexCoord1sv/2,multiTexCoord2d/3,
  multiTexCoord2dv/2,multiTexCoord2f/3,multiTexCoord2fv/2,multiTexCoord2i/3,
  multiTexCoord2iv/2,multiTexCoord2s/3,multiTexCoord2sv/2,multiTexCoord3d/4,
  multiTexCoord3dv/2,multiTexCoord3f/4,multiTexCoord3fv/2,multiTexCoord3i/4,
  multiTexCoord3iv/2,multiTexCoord3s/4,multiTexCoord3sv/2,multiTexCoord4d/5,
  multiTexCoord4dv/2,multiTexCoord4f/5,multiTexCoord4fv/2,multiTexCoord4i/5,
  multiTexCoord4iv/2,multiTexCoord4s/5,multiTexCoord4sv/2,loadTransposeMatrixf/1,
  loadTransposeMatrixd/1,multTransposeMatrixf/1,multTransposeMatrixd/1,
  blendFuncSeparate/4,multiDrawArrays/3,pointParameterf/2,pointParameterfv/2,
  pointParameteri/2,pointParameteriv/2,fogCoordf/1,fogCoordfv/1,fogCoordd/1,
  fogCoorddv/1,fogCoordPointer/3,secondaryColor3b/3,secondaryColor3bv/1,
  secondaryColor3d/3,secondaryColor3dv/1,secondaryColor3f/3,secondaryColor3fv/1,
  secondaryColor3i/3,secondaryColor3iv/1,secondaryColor3s/3,secondaryColor3sv/1,
  secondaryColor3ub/3,secondaryColor3ubv/1,secondaryColor3ui/3,secondaryColor3uiv/1,
  secondaryColor3us/3,secondaryColor3usv/1,secondaryColorPointer/4,
  windowPos2d/2,windowPos2dv/1,windowPos2f/2,windowPos2fv/1,windowPos2i/2,
  windowPos2iv/1,windowPos2s/2,windowPos2sv/1,windowPos3d/3,windowPos3dv/1,
  windowPos3f/3,windowPos3fv/1,windowPos3i/3,windowPos3iv/1,windowPos3s/3,
  windowPos3sv/1,genQueries/1,deleteQueries/1,isQuery/1,beginQuery/2,
  endQuery/1,getQueryiv/2,getQueryObjectiv/2,getQueryObjectuiv/2,bindBuffer/2,
  deleteBuffers/1,genBuffers/1,isBuffer/1,bufferData/4,bufferSubData/4,
  getBufferSubData/4,getBufferParameteriv/2,blendEquationSeparate/2,
  drawBuffers/1,stencilOpSeparate/4,stencilFuncSeparate/4,stencilMaskSeparate/2,
  attachShader/2,bindAttribLocation/3,compileShader/1,createProgram/0,
  createShader/1,deleteProgram/1,deleteShader/1,detachShader/2,disableVertexAttribArray/1,
  enableVertexAttribArray/1,getActiveAttrib/3,getActiveUniform/3,getAttachedShaders/2,
  getAttribLocation/2,getProgramiv/2,getProgramInfoLog/2,getShaderiv/2,
  getShaderInfoLog/2,getShaderSource/2,getUniformLocation/2,getUniformfv/2,
  getUniformiv/2,getVertexAttribdv/2,getVertexAttribfv/2,getVertexAttribiv/2,
  isProgram/1,isShader/1,linkProgram/1,shaderSource/2,useProgram/1,uniform1f/2,
  uniform2f/3,uniform3f/4,uniform4f/5,uniform1i/2,uniform2i/3,uniform3i/4,
  uniform4i/5,uniform1fv/2,uniform2fv/2,uniform3fv/2,uniform4fv/2,uniform1iv/2,
  uniform2iv/2,uniform3iv/2,uniform4iv/2,uniformMatrix2fv/3,uniformMatrix3fv/3,
  uniformMatrix4fv/3,validateProgram/1,vertexAttrib1d/2,vertexAttrib1dv/2,
  vertexAttrib1f/2,vertexAttrib1fv/2,vertexAttrib1s/2,vertexAttrib1sv/2,
  vertexAttrib2d/3,vertexAttrib2dv/2,vertexAttrib2f/3,vertexAttrib2fv/2,
  vertexAttrib2s/3,vertexAttrib2sv/2,vertexAttrib3d/4,vertexAttrib3dv/2,
  vertexAttrib3f/4,vertexAttrib3fv/2,vertexAttrib3s/4,vertexAttrib3sv/2,
  vertexAttrib4Nbv/2,vertexAttrib4Niv/2,vertexAttrib4Nsv/2,vertexAttrib4Nub/5,
  vertexAttrib4Nubv/2,vertexAttrib4Nuiv/2,vertexAttrib4Nusv/2,vertexAttrib4bv/2,
  vertexAttrib4d/5,vertexAttrib4dv/2,vertexAttrib4f/5,vertexAttrib4fv/2,
  vertexAttrib4iv/2,vertexAttrib4s/5,vertexAttrib4sv/2,vertexAttrib4ubv/2,
  vertexAttrib4uiv/2,vertexAttrib4usv/2,vertexAttribPointer/6,uniformMatrix2x3fv/3,
  uniformMatrix3x2fv/3,uniformMatrix2x4fv/3,uniformMatrix4x2fv/3,uniformMatrix3x4fv/3,
  uniformMatrix4x3fv/3,colorMaski/5,getBooleani_v/2,getIntegeri_v/2,
  enablei/2,disablei/2,isEnabledi/2,beginTransformFeedback/1,endTransformFeedback/0,
  bindBufferRange/5,bindBufferBase/3,transformFeedbackVaryings/3,getTransformFeedbackVarying/3,
  clampColor/2,beginConditionalRender/2,endConditionalRender/0,vertexAttribIPointer/5,
  getVertexAttribIiv/2,getVertexAttribIuiv/2,getUniformuiv/2,bindFragDataLocation/3,
  getFragDataLocation/2,uniform1ui/2,uniform2ui/3,uniform3ui/4,uniform4ui/5,
  uniform1uiv/2,uniform2uiv/2,uniform3uiv/2,uniform4uiv/2,texParameterIiv/3,
  texParameterIuiv/3,getTexParameterIiv/2,getTexParameterIuiv/2,clearBufferiv/3,
  clearBufferuiv/3,clearBufferfv/3,clearBufferfi/4,getStringi/2,vertexAttribI1i/2,
  vertexAttribI2i/3,vertexAttribI3i/4,vertexAttribI4i/5,vertexAttribI1ui/2,
  vertexAttribI2ui/3,vertexAttribI3ui/4,vertexAttribI4ui/5,vertexAttribI1iv/2,
  vertexAttribI2iv/2,vertexAttribI3iv/2,vertexAttribI4iv/2,vertexAttribI1uiv/2,
  vertexAttribI2uiv/2,vertexAttribI3uiv/2,vertexAttribI4uiv/2,vertexAttribI4bv/2,
  vertexAttribI4sv/2,vertexAttribI4ubv/2,vertexAttribI4usv/2,drawArraysInstanced/4,
  drawElementsInstanced/5,texBuffer/3,primitiveRestartIndex/1,loadTransposeMatrixfARB/1,
  loadTransposeMatrixdARB/1,multTransposeMatrixfARB/1,multTransposeMatrixdARB/1,
  weightbvARB/1,weightsvARB/1,weightivARB/1,weightfvARB/1,weightdvARB/1,
  weightubvARB/1,weightusvARB/1,weightuivARB/1,vertexBlendARB/1,currentPaletteMatrixARB/1,
  matrixIndexubvARB/1,matrixIndexusvARB/1,matrixIndexuivARB/1,programStringARB/3,
  bindProgramARB/2,deleteProgramsARB/1,genProgramsARB/1,programEnvParameter4dARB/6,
  programEnvParameter4dvARB/3,programEnvParameter4fARB/6,programEnvParameter4fvARB/3,
  programLocalParameter4dARB/6,programLocalParameter4dvARB/3,programLocalParameter4fARB/6,
  programLocalParameter4fvARB/3,getProgramEnvParameterdvARB/2,getProgramEnvParameterfvARB/2,
  getProgramLocalParameterdvARB/2,getProgramLocalParameterfvARB/2,
  getProgramStringARB/3,deleteObjectARB/1,getHandleARB/1,detachObjectARB/2,
  createShaderObjectARB/1,shaderSourceARB/2,compileShaderARB/1,createProgramObjectARB/0,
  attachObjectARB/2,linkProgramARB/1,useProgramObjectARB/1,validateProgramARB/1,
  getObjectParameterfvARB/2,getObjectParameterivARB/2,getInfoLogARB/2,
  getAttachedObjectsARB/2,getUniformLocationARB/2,getActiveUniformARB/3,
  getUniformfvARB/2,getUniformivARB/2,getShaderSourceARB/2,bindAttribLocationARB/3,
  getActiveAttribARB/3,getAttribLocationARB/2,isRenderbuffer/1,bindRenderbuffer/2,
  deleteRenderbuffers/1,genRenderbuffers/1,renderbufferStorage/4,getRenderbufferParameteriv/2,
  isFramebuffer/1,bindFramebuffer/2,deleteFramebuffers/1,genFramebuffers/1,
  checkFramebufferStatus/1,framebufferTexture1D/5,framebufferTexture2D/5,
  framebufferTexture3D/6,framebufferRenderbuffer/4,getFramebufferAttachmentParameteriv/3,
  generateMipmap/1,blitFramebuffer/10,renderbufferStorageMultisample/5,
  framebufferTextureLayer/5,programParameteriARB/3,framebufferTextureARB/4,
  framebufferTextureFaceARB/5,vertexAttribDivisorARB/2,flushMappedBufferRange/3,
  bindVertexArray/1,deleteVertexArrays/1,genVertexArrays/1,isVertexArray/1,
  getUniformIndices/2,getActiveUniformsiv/3,getActiveUniformName/3,
  getUniformBlockIndex/2,getActiveUniformBlockiv/4,getActiveUniformBlockName/3,
  uniformBlockBinding/3,copyBufferSubData/5,resizeBuffersMESA/0,windowPos4dMESA/4,
  windowPos4dvMESA/1,windowPos4fMESA/4,windowPos4fvMESA/1,windowPos4iMESA/4,
  windowPos4ivMESA/1,windowPos4sMESA/4,windowPos4svMESA/1,depthBoundsEXT/2,
  stencilClearTagEXT/2]).


%% API

%% @spec (Op::enum(),Value::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAccum.xml">external</a> documentation.
accum(Op,Value) ->
  wxe_util:cast(5037, <<Op:?GLenum,Value:?GLfloat>>).

%% @spec (Func::enum(),Ref::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAlphaFunc.xml">external</a> documentation.
alphaFunc(Func,Ref) ->
  wxe_util:cast(5038, <<Func:?GLenum,Ref:?GLclampf>>).

%% @spec (Textures::[integer()]) -> {0|1,Residences::[0|1]}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAreTexturesResident.xml">external</a> documentation.
areTexturesResident(Textures) ->
  wxe_util:call(5039, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

%% @spec (I::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glArrayElement.xml">external</a> documentation.
arrayElement(I) ->
  wxe_util:cast(5040, <<I:?GLint>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBegin.xml">external</a> documentation.
'begin'(Mode) ->
  wxe_util:cast(5041, <<Mode:?GLenum>>).

%% @spec (Target::enum(),Texture::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindTexture.xml">external</a> documentation.
bindTexture(Target,Texture) ->
  wxe_util:cast(5042, <<Target:?GLenum,Texture:?GLuint>>).

%% @spec (Width::integer(),Height::integer(),Xorig::float(),Yorig::float(),Xmove::float(),Ymove::float(),Bitmap::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBitmap.xml">external</a> documentation.
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when  is_integer(Bitmap) ->
  wxe_util:cast(5043, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat,Bitmap:?GLuint>>);
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) ->
  wxe_util:send_bin(Bitmap),
  wxe_util:cast(5044, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat>>).

%% @spec (Sfactor::enum(),Dfactor::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunc.xml">external</a> documentation.
blendFunc(Sfactor,Dfactor) ->
  wxe_util:cast(5045, <<Sfactor:?GLenum,Dfactor:?GLenum>>).

%% @spec (List::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallList.xml">external</a> documentation.
callList(List) ->
  wxe_util:cast(5046, <<List:?GLuint>>).

%% @spec (Lists::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallLists.xml">external</a> documentation.
callLists(Lists) ->
  wxe_util:cast(5047, <<(length(Lists)):?GLuint,
        (<< <<C:?GLuint>> || C <- Lists>>)/binary,0:(((1+length(Lists)) rem 2)*32)>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClear.xml">external</a> documentation.
clear(Mask) ->
  wxe_util:cast(5048, <<Mask:?GLbitfield>>).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearAccum.xml">external</a> documentation.
clearAccum(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5049, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @spec (Red::clamp(),Green::clamp(),Blue::clamp(),Alpha::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearColor.xml">external</a> documentation.
clearColor(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5050, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @spec (Depth::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearDepth.xml">external</a> documentation.
clearDepth(Depth) ->
  wxe_util:cast(5051, <<Depth:?GLclampd>>).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearIndex.xml">external</a> documentation.
clearIndex(C) ->
  wxe_util:cast(5052, <<C:?GLfloat>>).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearStencil.xml">external</a> documentation.
clearStencil(S) ->
  wxe_util:cast(5053, <<S:?GLint>>).

%% @spec (Plane::enum(),Equation::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClipPlane.xml">external</a> documentation.
clipPlane(Plane,{E1,E2,E3,E4}) ->
  wxe_util:cast(5054, <<Plane:?GLenum,0:32,E1:?GLdouble,E2:?GLdouble,E3:?GLdouble,E4:?GLdouble>>).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3b(Red,Green,Blue) ->
  wxe_util:cast(5055, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3b(Red,Green,Blue)
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3d(Red,Green,Blue) ->
  wxe_util:cast(5056, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3d(Red,Green,Blue)
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3f(Red,Green,Blue) ->
  wxe_util:cast(5057, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3f(Red,Green,Blue)
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3i(Red,Green,Blue) ->
  wxe_util:cast(5058, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3i(Red,Green,Blue)
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3s(Red,Green,Blue) ->
  wxe_util:cast(5059, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3s(Red,Green,Blue)
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3ub(Red,Green,Blue) ->
  wxe_util:cast(5060, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3ub(Red,Green,Blue)
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3ui(Red,Green,Blue) ->
  wxe_util:cast(5061, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3ui(Red,Green,Blue)
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color3us(Red,Green,Blue) ->
  wxe_util:cast(5062, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv color3us(Red,Green,Blue)
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4b(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5063, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte,Alpha:?GLbyte>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4b(Red,Green,Blue,Alpha)
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4d(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5064, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble,Alpha:?GLdouble>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4d(Red,Green,Blue,Alpha)
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).

%% @spec (Red::float(),Green::float(),Blue::float(),Alpha::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4f(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5065, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4f(Red,Green,Blue,Alpha)
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4i(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5066, <<Red:?GLint,Green:?GLint,Blue:?GLint,Alpha:?GLint>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4i(Red,Green,Blue,Alpha)
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4s(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5067, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort,Alpha:?GLshort>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4s(Red,Green,Blue,Alpha)
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4ub(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5068, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte,Alpha:?GLubyte>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4ub(Red,Green,Blue,Alpha)
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4ui(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5069, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint,Alpha:?GLuint>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4ui(Red,Green,Blue,Alpha)
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).

%% @spec (Red::integer(),Green::integer(),Blue::integer(),Alpha::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
color4us(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5070, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort,Alpha:?GLushort>>).

%% @spec ({Red,Green,Blue,Alpha}) -> ok
%% @equiv color4us(Red,Green,Blue,Alpha)
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).

%% @spec (Red::0|1,Green::0|1,Blue::0|1,Alpha::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMask.xml">external</a> documentation.
colorMask(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5071, <<Red:?GLboolean,Green:?GLboolean,Blue:?GLboolean,Alpha:?GLboolean>>).

%% @spec (Face::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaterial.xml">external</a> documentation.
colorMaterial(Face,Mode) ->
  wxe_util:cast(5072, <<Face:?GLenum,Mode:?GLenum>>).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorPointer.xml">external</a> documentation.
colorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5073, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
colorPointer(Size,Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5074, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer(),Type::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyPixels.xml">external</a> documentation.
copyPixels(X,Y,Width,Height,Type) ->
  wxe_util:cast(5075, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),InternalFormat::enum(),X::integer(),Y::integer(),Width::integer(),Border::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage1D.xml">external</a> documentation.
copyTexImage1D(Target,Level,InternalFormat,X,Y,Width,Border) ->
  wxe_util:cast(5076, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Border:?GLint>>).

%% @spec (Target::enum(),Level::integer(),InternalFormat::enum(),X::integer(),Y::integer(),Width::integer(),Height::integer(),Border::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage2D.xml">external</a> documentation.
copyTexImage2D(Target,Level,InternalFormat,X,Y,Width,Height,Border) ->
  wxe_util:cast(5077, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage1D.xml">external</a> documentation.
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) ->
  wxe_util:cast(5078, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage2D.xml">external</a> documentation.
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) ->
  wxe_util:cast(5079, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCullFace.xml">external</a> documentation.
cullFace(Mode) ->
  wxe_util:cast(5080, <<Mode:?GLenum>>).

%% @spec (List::integer(),Range::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteLists.xml">external</a> documentation.
deleteLists(List,Range) ->
  wxe_util:cast(5081, <<List:?GLuint,Range:?GLsizei>>).

%% @spec (Textures::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTextures.xml">external</a> documentation.
deleteTextures(Textures) ->
  wxe_util:cast(5082, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

%% @spec (Func::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthFunc.xml">external</a> documentation.
depthFunc(Func) ->
  wxe_util:cast(5083, <<Func:?GLenum>>).

%% @spec (Flag::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthMask.xml">external</a> documentation.
depthMask(Flag) ->
  wxe_util:cast(5084, <<Flag:?GLboolean>>).

%% @spec (ZNear::clamp(),ZFar::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRange.xml">external</a> documentation.
depthRange(ZNear,ZFar) ->
  wxe_util:cast(5085, <<ZNear:?GLclampd,ZFar:?GLclampd>>).

%% @spec (Cap::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisable.xml">external</a> documentation.
disable(Cap) ->
  wxe_util:cast(5086, <<Cap:?GLenum>>).

%% @spec (Array::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisableClientState.xml">external</a> documentation.
disableClientState(Array) ->
  wxe_util:cast(5087, <<Array:?GLenum>>).

%% @spec (Mode::enum(),First::integer(),Count::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArrays.xml">external</a> documentation.
drawArrays(Mode,First,Count) ->
  wxe_util:cast(5088, <<Mode:?GLenum,First:?GLint,Count:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffer.xml">external</a> documentation.
drawBuffer(Mode) ->
  wxe_util:cast(5089, <<Mode:?GLenum>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElements.xml">external</a> documentation.
drawElements(Mode,Count,Type,Indices) when  is_integer(Indices) ->
  wxe_util:cast(5090, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawElements(Mode,Count,Type,Indices) ->
  wxe_util:send_bin(Indices),
  wxe_util:cast(5091, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum>>).

%% @spec (Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawPixels.xml">external</a> documentation.
drawPixels(Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5092, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
drawPixels(Width,Height,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5093, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Flag::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlag.xml">external</a> documentation.
edgeFlag(Flag) ->
  wxe_util:cast(5094, <<Flag:?GLboolean>>).

%% @spec (Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlagPointer.xml">external</a> documentation.
edgeFlagPointer(Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5095, <<Stride:?GLsizei,Pointer:?GLuint>>);
edgeFlagPointer(Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5096, <<Stride:?GLsizei>>).

%% @spec ({Flag}) -> ok
%% @equiv edgeFlag(Flag)
edgeFlagv({Flag}) ->  edgeFlag(Flag).

%% @spec (Cap::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml">external</a> documentation.
enable(Cap) ->
  wxe_util:cast(5097, <<Cap:?GLenum>>).

%% @spec (Array::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableClientState.xml">external</a> documentation.
enableClientState(Array) ->
  wxe_util:cast(5098, <<Array:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnd.xml">external</a> documentation.
'end'() ->
  wxe_util:cast(5099, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndList.xml">external</a> documentation.
endList() ->
  wxe_util:cast(5100, <<>>).

%% @spec (U::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
evalCoord1d(U) ->
  wxe_util:cast(5101, <<U:?GLdouble>>).

%% @spec ({U}) -> ok
%% @equiv evalCoord1d(U)
evalCoord1dv({U}) ->  evalCoord1d(U).

%% @spec (U::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
evalCoord1f(U) ->
  wxe_util:cast(5102, <<U:?GLfloat>>).

%% @spec ({U}) -> ok
%% @equiv evalCoord1f(U)
evalCoord1fv({U}) ->  evalCoord1f(U).

%% @spec (U::float(),V::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
evalCoord2d(U,V) ->
  wxe_util:cast(5103, <<U:?GLdouble,V:?GLdouble>>).

%% @spec ({U,V}) -> ok
%% @equiv evalCoord2d(U,V)
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).

%% @spec (U::float(),V::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
evalCoord2f(U,V) ->
  wxe_util:cast(5104, <<U:?GLfloat,V:?GLfloat>>).

%% @spec ({U,V}) -> ok
%% @equiv evalCoord2f(U,V)
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).

%% @spec (Mode::enum(),I1::integer(),I2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml">external</a> documentation.
evalMesh1(Mode,I1,I2) ->
  wxe_util:cast(5105, <<Mode:?GLenum,I1:?GLint,I2:?GLint>>).

%% @spec (Mode::enum(),I1::integer(),I2::integer(),J1::integer(),J2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml">external</a> documentation.
evalMesh2(Mode,I1,I2,J1,J2) ->
  wxe_util:cast(5106, <<Mode:?GLenum,I1:?GLint,I2:?GLint,J1:?GLint,J2:?GLint>>).

%% @spec (I::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml">external</a> documentation.
evalPoint1(I) ->
  wxe_util:cast(5107, <<I:?GLint>>).

%% @spec (I::integer(),J::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml">external</a> documentation.
evalPoint2(I,J) ->
  wxe_util:cast(5108, <<I:?GLint,J:?GLint>>).

%% @spec (Size::integer(),Type::enum(),Buffer::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFeedbackBuffer.xml">external</a> documentation.
feedbackBuffer(Size,Type,Buffer) ->
  wxe_util:send_bin(Buffer#wx_mem.bin),
  wxe_util:call(5109, <<Size:?GLsizei,Type:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFinish.xml">external</a> documentation.
finish() ->
  wxe_util:cast(5110, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlush.xml">external</a> documentation.
flush() ->
  wxe_util:cast(5111, <<>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
fogf(Pname,Param) ->
  wxe_util:cast(5112, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
fogfv(Pname,Params) ->
  wxe_util:cast(5113, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
fogi(Pname,Param) ->
  wxe_util:cast(5114, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
fogiv(Pname,Params) ->
  wxe_util:cast(5115, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrontFace.xml">external</a> documentation.
frontFace(Mode) ->
  wxe_util:cast(5116, <<Mode:?GLenum>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrustum.xml">external</a> documentation.
frustum(Left,Right,Bottom,Top,ZNear,ZFar) ->
  wxe_util:cast(5117, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (Range::integer()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenLists.xml">external</a> documentation.
genLists(Range) ->
  wxe_util:call(5118, <<Range:?GLsizei>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenTextures.xml">external</a> documentation.
genTextures(N) ->
  wxe_util:call(5119, <<N:?GLsizei>>).

%% @spec (Pname::enum()) -> [0|1]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBooleanv.xml">external</a> documentation.
getBooleanv(Pname) ->
  wxe_util:call(5120, <<Pname:?GLenum>>).

%% @spec (Plane::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetClipPlane.xml">external</a> documentation.
getClipPlane(Plane) ->
  wxe_util:call(5121, <<Plane:?GLenum>>).

%% @spec (Pname::enum()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetDoublev.xml">external</a> documentation.
getDoublev(Pname) ->
  wxe_util:call(5122, <<Pname:?GLenum>>).

%% @spec () -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetError.xml">external</a> documentation.
getError() ->
  wxe_util:call(5123, <<>>).

%% @spec (Pname::enum()) -> [float()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFloatv.xml">external</a> documentation.
getFloatv(Pname) ->
  wxe_util:call(5124, <<Pname:?GLenum>>).

%% @spec (Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetIntegerv.xml">external</a> documentation.
getIntegerv(Pname) ->
  wxe_util:call(5125, <<Pname:?GLenum>>).

%% @spec (Light::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml">external</a> documentation.
getLightfv(Light,Pname) ->
  wxe_util:call(5126, <<Light:?GLenum,Pname:?GLenum>>).

%% @spec (Light::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml">external</a> documentation.
getLightiv(Light,Pname) ->
  wxe_util:call(5127, <<Light:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
getMapdv(Target,Query,V) ->
  wxe_util:send_bin(V#wx_mem.bin),
  wxe_util:call(5128, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
getMapfv(Target,Query,V) ->
  wxe_util:send_bin(V#wx_mem.bin),
  wxe_util:call(5129, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Target::enum(),Query::enum(),V::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
getMapiv(Target,Query,V) ->
  wxe_util:send_bin(V#wx_mem.bin),
  wxe_util:call(5130, <<Target:?GLenum,Query:?GLenum>>).

%% @spec (Face::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml">external</a> documentation.
getMaterialfv(Face,Pname) ->
  wxe_util:call(5131, <<Face:?GLenum,Pname:?GLenum>>).

%% @spec (Face::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml">external</a> documentation.
getMaterialiv(Face,Pname) ->
  wxe_util:call(5132, <<Face:?GLenum,Pname:?GLenum>>).

%% @spec (Map::enum(),Values::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
getPixelMapfv(Map,Values) ->
  wxe_util:send_bin(Values#wx_mem.bin),
  wxe_util:call(5133, <<Map:?GLenum>>).

%% @spec (Map::enum(),Values::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
getPixelMapuiv(Map,Values) ->
  wxe_util:send_bin(Values#wx_mem.bin),
  wxe_util:call(5134, <<Map:?GLenum>>).

%% @spec (Map::enum(),Values::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
getPixelMapusv(Map,Values) ->
  wxe_util:send_bin(Values#wx_mem.bin),
  wxe_util:call(5135, <<Map:?GLenum>>).

%% @spec () -> binary()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPolygonStipple.xml">external</a> documentation.
getPolygonStipple() ->
  wxe_util:call(5136, <<>>).

%% @spec (Name::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml">external</a> documentation.
getString(Name) ->
  wxe_util:call(5137, <<Name:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexEnv.xml">external</a> documentation.
getTexEnvfv(Target,Pname) ->
  wxe_util:call(5138, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexEnv.xml">external</a> documentation.
getTexEnviv(Target,Pname) ->
  wxe_util:call(5139, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
getTexGendv(Coord,Pname) ->
  wxe_util:call(5140, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
getTexGenfv(Coord,Pname) ->
  wxe_util:call(5141, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Coord::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
getTexGeniv(Coord,Pname) ->
  wxe_util:call(5142, <<Coord:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Format::enum(),Type::enum(),Pixels::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexImage.xml">external</a> documentation.
getTexImage(Target,Level,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels#wx_mem.bin),
  wxe_util:call(5143, <<Target:?GLenum,Level:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml">external</a> documentation.
getTexLevelParameterfv(Target,Level,Pname) ->
  wxe_util:call(5144, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml">external</a> documentation.
getTexLevelParameteriv(Target,Level,Pname) ->
  wxe_util:call(5145, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml">external</a> documentation.
getTexParameterfv(Target,Pname) ->
  wxe_util:call(5146, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml">external</a> documentation.
getTexParameteriv(Target,Pname) ->
  wxe_util:call(5147, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHint.xml">external</a> documentation.
hint(Target,Mode) ->
  wxe_util:cast(5148, <<Target:?GLenum,Mode:?GLenum>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexMask.xml">external</a> documentation.
indexMask(Mask) ->
  wxe_util:cast(5149, <<Mask:?GLuint>>).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexPointer.xml">external</a> documentation.
indexPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5150, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
indexPointer(Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5151, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
indexd(C) ->
  wxe_util:cast(5152, <<C:?GLdouble>>).

%% @spec ({C}) -> ok
%% @equiv indexd(C)
indexdv({C}) ->  indexd(C).

%% @spec (C::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
indexf(C) ->
  wxe_util:cast(5153, <<C:?GLfloat>>).

%% @spec ({C}) -> ok
%% @equiv indexf(C)
indexfv({C}) ->  indexf(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
indexi(C) ->
  wxe_util:cast(5154, <<C:?GLint>>).

%% @spec ({C}) -> ok
%% @equiv indexi(C)
indexiv({C}) ->  indexi(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
indexs(C) ->
  wxe_util:cast(5155, <<C:?GLshort>>).

%% @spec ({C}) -> ok
%% @equiv indexs(C)
indexsv({C}) ->  indexs(C).

%% @spec (C::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
indexub(C) ->
  wxe_util:cast(5156, <<C:?GLubyte>>).

%% @spec ({C}) -> ok
%% @equiv indexub(C)
indexubv({C}) ->  indexub(C).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInitNames.xml">external</a> documentation.
initNames() ->
  wxe_util:cast(5157, <<>>).

%% @spec (Format::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInterleavedArrays.xml">external</a> documentation.
interleavedArrays(Format,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5158, <<Format:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
interleavedArrays(Format,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5159, <<Format:?GLenum,Stride:?GLsizei>>).

%% @spec (Cap::enum()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabled.xml">external</a> documentation.
isEnabled(Cap) ->
  wxe_util:call(5160, <<Cap:?GLenum>>).

%% @spec (List::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsList.xml">external</a> documentation.
isList(List) ->
  wxe_util:call(5161, <<List:?GLuint>>).

%% @spec (Texture::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsTexture.xml">external</a> documentation.
isTexture(Texture) ->
  wxe_util:call(5162, <<Texture:?GLuint>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
lightModelf(Pname,Param) ->
  wxe_util:cast(5163, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
lightModelfv(Pname,Params) ->
  wxe_util:cast(5164, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
lightModeli(Pname,Param) ->
  wxe_util:cast(5165, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
lightModeliv(Pname,Params) ->
  wxe_util:cast(5166, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Light::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
lightf(Light,Pname,Param) ->
  wxe_util:cast(5167, <<Light:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Light::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
lightfv(Light,Pname,Params) ->
  wxe_util:cast(5168, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Light::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
lighti(Light,Pname,Param) ->
  wxe_util:cast(5169, <<Light:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Light::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
lightiv(Light,Pname,Params) ->
  wxe_util:cast(5170, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Factor::integer(),Pattern::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineStipple.xml">external</a> documentation.
lineStipple(Factor,Pattern) ->
  wxe_util:cast(5171, <<Factor:?GLint,Pattern:?GLushort>>).

%% @spec (Width::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineWidth.xml">external</a> documentation.
lineWidth(Width) ->
  wxe_util:cast(5172, <<Width:?GLfloat>>).

%% @spec (Base::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glListBase.xml">external</a> documentation.
listBase(Base) ->
  wxe_util:cast(5173, <<Base:?GLuint>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadIdentity.xml">external</a> documentation.
loadIdentity() ->
  wxe_util:cast(5174, <<>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml">external</a> documentation.
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5175, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5175, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml">external</a> documentation.
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5176, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5176, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (Name::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadName.xml">external</a> documentation.
loadName(Name) ->
  wxe_util:cast(5177, <<Name:?GLuint>>).

%% @spec (Opcode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLogicOp.xml">external</a> documentation.
logicOp(Opcode) ->
  wxe_util:cast(5178, <<Opcode:?GLenum>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Stride::integer(),Order::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
map1d(Target,U1,U2,Stride,Order,Points) ->
  wxe_util:send_bin(Points),
  wxe_util:cast(5179, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Stride:?GLint,Order:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Stride::integer(),Order::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
map1f(Target,U1,U2,Stride,Order,Points) ->
  wxe_util:send_bin(Points),
  wxe_util:cast(5180, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Stride:?GLint,Order:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Ustride::integer(),Uorder::integer(),V1::float(),V2::float(),Vstride::integer(),Vorder::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  wxe_util:send_bin(Points),
  wxe_util:cast(5181, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Ustride:?GLint,Uorder:?GLint,V1:?GLdouble,V2:?GLdouble,Vstride:?GLint,Vorder:?GLint>>).

%% @spec (Target::enum(),U1::float(),U2::float(),Ustride::integer(),Uorder::integer(),V1::float(),V2::float(),Vstride::integer(),Vorder::integer(),Points::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  wxe_util:send_bin(Points),
  wxe_util:cast(5182, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Ustride:?GLint,Uorder:?GLint,V1:?GLfloat,V2:?GLfloat,Vstride:?GLint,Vorder:?GLint>>).

%% @spec (Un::integer(),U1::float(),U2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
mapGrid1d(Un,U1,U2) ->
  wxe_util:cast(5183, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble>>).

%% @spec (Un::integer(),U1::float(),U2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
mapGrid1f(Un,U1,U2) ->
  wxe_util:cast(5184, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat>>).

%% @spec (Un::integer(),U1::float(),U2::float(),Vn::integer(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
mapGrid2d(Un,U1,U2,Vn,V1,V2) ->
  wxe_util:cast(5185, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble,Vn:?GLint,0:32,V1:?GLdouble,V2:?GLdouble>>).

%% @spec (Un::integer(),U1::float(),U2::float(),Vn::integer(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
mapGrid2f(Un,U1,U2,Vn,V1,V2) ->
  wxe_util:cast(5186, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat,Vn:?GLint,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (Face::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
materialf(Face,Pname,Param) ->
  wxe_util:cast(5187, <<Face:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Face::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
materialfv(Face,Pname,Params) ->
  wxe_util:cast(5188, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Face::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
materiali(Face,Pname,Param) ->
  wxe_util:cast(5189, <<Face:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Face::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
materialiv(Face,Pname,Params) ->
  wxe_util:cast(5190, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixMode.xml">external</a> documentation.
matrixMode(Mode) ->
  wxe_util:cast(5191, <<Mode:?GLenum>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml">external</a> documentation.
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5192, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5192, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml">external</a> documentation.
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5193, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5193, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (List::integer(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNewList.xml">external</a> documentation.
newList(List,Mode) ->
  wxe_util:cast(5194, <<List:?GLuint,Mode:?GLenum>>).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
normal3b(Nx,Ny,Nz) ->
  wxe_util:cast(5195, <<Nx:?GLbyte,Ny:?GLbyte,Nz:?GLbyte>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3b(Nx,Ny,Nz)
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).

%% @spec (Nx::float(),Ny::float(),Nz::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
normal3d(Nx,Ny,Nz) ->
  wxe_util:cast(5196, <<Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3d(Nx,Ny,Nz)
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).

%% @spec (Nx::float(),Ny::float(),Nz::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
normal3f(Nx,Ny,Nz) ->
  wxe_util:cast(5197, <<Nx:?GLfloat,Ny:?GLfloat,Nz:?GLfloat>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3f(Nx,Ny,Nz)
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
normal3i(Nx,Ny,Nz) ->
  wxe_util:cast(5198, <<Nx:?GLint,Ny:?GLint,Nz:?GLint>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3i(Nx,Ny,Nz)
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).

%% @spec (Nx::integer(),Ny::integer(),Nz::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
normal3s(Nx,Ny,Nz) ->
  wxe_util:cast(5199, <<Nx:?GLshort,Ny:?GLshort,Nz:?GLshort>>).

%% @spec ({Nx,Ny,Nz}) -> ok
%% @equiv normal3s(Nx,Ny,Nz)
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormalPointer.xml">external</a> documentation.
normalPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5200, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
normalPointer(Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5201, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml">external</a> documentation.
ortho(Left,Right,Bottom,Top,ZNear,ZFar) ->
  wxe_util:cast(5202, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (Token::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPassThrough.xml">external</a> documentation.
passThrough(Token) ->
  wxe_util:cast(5203, <<Token:?GLfloat>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
pixelMapfv(Map,Mapsize,Values) ->
  wxe_util:send_bin(Values),
  wxe_util:cast(5204, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
pixelMapuiv(Map,Mapsize,Values) ->
  wxe_util:send_bin(Values),
  wxe_util:cast(5205, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Map::enum(),Mapsize::integer(),Values::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
pixelMapusv(Map,Mapsize,Values) ->
  wxe_util:send_bin(Values),
  wxe_util:cast(5206, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml">external</a> documentation.
pixelStoref(Pname,Param) ->
  wxe_util:cast(5207, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml">external</a> documentation.
pixelStorei(Pname,Param) ->
  wxe_util:cast(5208, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml">external</a> documentation.
pixelTransferf(Pname,Param) ->
  wxe_util:cast(5209, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml">external</a> documentation.
pixelTransferi(Pname,Param) ->
  wxe_util:cast(5210, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Xfactor::float(),Yfactor::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelZoom.xml">external</a> documentation.
pixelZoom(Xfactor,Yfactor) ->
  wxe_util:cast(5211, <<Xfactor:?GLfloat,Yfactor:?GLfloat>>).

%% @spec (Size::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointSize.xml">external</a> documentation.
pointSize(Size) ->
  wxe_util:cast(5212, <<Size:?GLfloat>>).

%% @spec (Face::enum(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonMode.xml">external</a> documentation.
polygonMode(Face,Mode) ->
  wxe_util:cast(5213, <<Face:?GLenum,Mode:?GLenum>>).

%% @spec (Factor::float(),Units::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml">external</a> documentation.
polygonOffset(Factor,Units) ->
  wxe_util:cast(5214, <<Factor:?GLfloat,Units:?GLfloat>>).

%% @spec (Mask::binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonStipple.xml">external</a> documentation.
polygonStipple(Mask) ->
  wxe_util:send_bin(Mask),
  wxe_util:cast(5215, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopAttrib.xml">external</a> documentation.
popAttrib() ->
  wxe_util:cast(5216, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopClientAttrib.xml">external</a> documentation.
popClientAttrib() ->
  wxe_util:cast(5217, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopMatrix.xml">external</a> documentation.
popMatrix() ->
  wxe_util:cast(5218, <<>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPopName.xml">external</a> documentation.
popName() ->
  wxe_util:cast(5219, <<>>).

%% @spec (Textures::[integer()],Priorities::[clamp()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrioritizeTextures.xml">external</a> documentation.
prioritizeTextures(Textures,Priorities) ->
  wxe_util:cast(5220, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32),(length(Priorities)):?GLuint,
        (<< <<C:?GLclampf>> || C <- Priorities>>)/binary,0:(((1+length(Priorities)) rem 2)*32)>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushAttrib.xml">external</a> documentation.
pushAttrib(Mask) ->
  wxe_util:cast(5221, <<Mask:?GLbitfield>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushClientAttrib.xml">external</a> documentation.
pushClientAttrib(Mask) ->
  wxe_util:cast(5222, <<Mask:?GLbitfield>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushMatrix.xml">external</a> documentation.
pushMatrix() ->
  wxe_util:cast(5223, <<>>).

%% @spec (Name::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushName.xml">external</a> documentation.
pushName(Name) ->
  wxe_util:cast(5224, <<Name:?GLuint>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos2d(X,Y) ->
  wxe_util:cast(5225, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2d(X,Y)
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos2f(X,Y) ->
  wxe_util:cast(5226, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2f(X,Y)
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos2i(X,Y) ->
  wxe_util:cast(5227, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2i(X,Y)
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos2s(X,Y) ->
  wxe_util:cast(5228, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv rasterPos2s(X,Y)
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos3d(X,Y,Z) ->
  wxe_util:cast(5229, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3d(X,Y,Z)
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos3f(X,Y,Z) ->
  wxe_util:cast(5230, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3f(X,Y,Z)
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos3i(X,Y,Z) ->
  wxe_util:cast(5231, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3i(X,Y,Z)
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos3s(X,Y,Z) ->
  wxe_util:cast(5232, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv rasterPos3s(X,Y,Z)
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos4d(X,Y,Z,W) ->
  wxe_util:cast(5233, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4d(X,Y,Z,W)
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos4f(X,Y,Z,W) ->
  wxe_util:cast(5234, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4f(X,Y,Z,W)
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos4i(X,Y,Z,W) ->
  wxe_util:cast(5235, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4i(X,Y,Z,W)
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
rasterPos4s(X,Y,Z,W) ->
  wxe_util:cast(5236, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv rasterPos4s(X,Y,Z,W)
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadBuffer.xml">external</a> documentation.
readBuffer(Mode) ->
  wxe_util:cast(5237, <<Mode:?GLenum>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadPixels.xml">external</a> documentation.
readPixels(X,Y,Width,Height,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels#wx_mem.bin),
  wxe_util:call(5238, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (X1::float(),Y1::float(),X2::float(),Y2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectd(X1,Y1,X2,Y2) ->
  wxe_util:cast(5239, <<X1:?GLdouble,Y1:?GLdouble,X2:?GLdouble,Y2:?GLdouble>>).

%% @spec (V1::{float()},V2::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectdv({V1,V2},{V1,V2}) ->
  wxe_util:cast(5240, <<V1:?GLdouble,V2:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @spec (X1::float(),Y1::float(),X2::float(),Y2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectf(X1,Y1,X2,Y2) ->
  wxe_util:cast(5241, <<X1:?GLfloat,Y1:?GLfloat,X2:?GLfloat,Y2:?GLfloat>>).

%% @spec (V1::{float()},V2::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectfv({V1,V2},{V1,V2}) ->
  wxe_util:cast(5242, <<V1:?GLfloat,V2:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (X1::integer(),Y1::integer(),X2::integer(),Y2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
recti(X1,Y1,X2,Y2) ->
  wxe_util:cast(5243, <<X1:?GLint,Y1:?GLint,X2:?GLint,Y2:?GLint>>).

%% @spec (V1::{integer()},V2::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectiv({V1,V2},{V1,V2}) ->
  wxe_util:cast(5244, <<V1:?GLint,V2:?GLint,V1:?GLint,V2:?GLint>>).

%% @spec (X1::integer(),Y1::integer(),X2::integer(),Y2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rects(X1,Y1,X2,Y2) ->
  wxe_util:cast(5245, <<X1:?GLshort,Y1:?GLshort,X2:?GLshort,Y2:?GLshort>>).

%% @spec (V1::{integer()},V2::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
rectsv({V1,V2},{V1,V2}) ->
  wxe_util:cast(5246, <<V1:?GLshort,V2:?GLshort,V1:?GLshort,V2:?GLshort>>).

%% @spec (Mode::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderMode.xml">external</a> documentation.
renderMode(Mode) ->
  wxe_util:call(5247, <<Mode:?GLenum>>).

%% @spec (Angle::float(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml">external</a> documentation.
rotated(Angle,X,Y,Z) ->
  wxe_util:cast(5248, <<Angle:?GLdouble,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Angle::float(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml">external</a> documentation.
rotatef(Angle,X,Y,Z) ->
  wxe_util:cast(5249, <<Angle:?GLfloat,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml">external</a> documentation.
scaled(X,Y,Z) ->
  wxe_util:cast(5250, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml">external</a> documentation.
scalef(X,Y,Z) ->
  wxe_util:cast(5251, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml">external</a> documentation.
scissor(X,Y,Width,Height) ->
  wxe_util:cast(5252, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Size::integer(),Buffer::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSelectBuffer.xml">external</a> documentation.
selectBuffer(Size,Buffer) ->
  wxe_util:send_bin(Buffer#wx_mem.bin),
  wxe_util:call(5253, <<Size:?GLsizei>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShadeModel.xml">external</a> documentation.
shadeModel(Mode) ->
  wxe_util:cast(5254, <<Mode:?GLenum>>).

%% @spec (Func::enum(),Ref::integer(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFunc.xml">external</a> documentation.
stencilFunc(Func,Ref,Mask) ->
  wxe_util:cast(5255, <<Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @spec (Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMask.xml">external</a> documentation.
stencilMask(Mask) ->
  wxe_util:cast(5256, <<Mask:?GLuint>>).

%% @spec (Fail::enum(),Zfail::enum(),Zpass::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOp.xml">external</a> documentation.
stencilOp(Fail,Zfail,Zpass) ->
  wxe_util:cast(5257, <<Fail:?GLenum,Zfail:?GLenum,Zpass:?GLenum>>).

%% @spec (S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord1d(S) ->
  wxe_util:cast(5258, <<S:?GLdouble>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1d(S)
texCoord1dv({S}) ->  texCoord1d(S).

%% @spec (S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord1f(S) ->
  wxe_util:cast(5259, <<S:?GLfloat>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1f(S)
texCoord1fv({S}) ->  texCoord1f(S).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord1i(S) ->
  wxe_util:cast(5260, <<S:?GLint>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1i(S)
texCoord1iv({S}) ->  texCoord1i(S).

%% @spec (S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord1s(S) ->
  wxe_util:cast(5261, <<S:?GLshort>>).

%% @spec ({S}) -> ok
%% @equiv texCoord1s(S)
texCoord1sv({S}) ->  texCoord1s(S).

%% @spec (S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord2d(S,T) ->
  wxe_util:cast(5262, <<S:?GLdouble,T:?GLdouble>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2d(S,T)
texCoord2dv({S,T}) ->  texCoord2d(S,T).

%% @spec (S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord2f(S,T) ->
  wxe_util:cast(5263, <<S:?GLfloat,T:?GLfloat>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2f(S,T)
texCoord2fv({S,T}) ->  texCoord2f(S,T).

%% @spec (S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord2i(S,T) ->
  wxe_util:cast(5264, <<S:?GLint,T:?GLint>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2i(S,T)
texCoord2iv({S,T}) ->  texCoord2i(S,T).

%% @spec (S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord2s(S,T) ->
  wxe_util:cast(5265, <<S:?GLshort,T:?GLshort>>).

%% @spec ({S,T}) -> ok
%% @equiv texCoord2s(S,T)
texCoord2sv({S,T}) ->  texCoord2s(S,T).

%% @spec (S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord3d(S,T,R) ->
  wxe_util:cast(5266, <<S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3d(S,T,R)
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).

%% @spec (S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord3f(S,T,R) ->
  wxe_util:cast(5267, <<S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3f(S,T,R)
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).

%% @spec (S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord3i(S,T,R) ->
  wxe_util:cast(5268, <<S:?GLint,T:?GLint,R:?GLint>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3i(S,T,R)
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).

%% @spec (S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord3s(S,T,R) ->
  wxe_util:cast(5269, <<S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @spec ({S,T,R}) -> ok
%% @equiv texCoord3s(S,T,R)
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).

%% @spec (S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord4d(S,T,R,Q) ->
  wxe_util:cast(5270, <<S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4d(S,T,R,Q)
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).

%% @spec (S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord4f(S,T,R,Q) ->
  wxe_util:cast(5271, <<S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4f(S,T,R,Q)
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).

%% @spec (S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord4i(S,T,R,Q) ->
  wxe_util:cast(5272, <<S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4i(S,T,R,Q)
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).

%% @spec (S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
texCoord4s(S,T,R,Q) ->
  wxe_util:cast(5273, <<S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @spec ({S,T,R,Q}) -> ok
%% @equiv texCoord4s(S,T,R,Q)
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoordPointer.xml">external</a> documentation.
texCoordPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5274, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
texCoordPointer(Size,Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5275, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Target::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvf.xml">external</a> documentation.
texEnvf(Target,Pname,Param) ->
  wxe_util:cast(5276, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml">external</a> documentation.
texEnvfv(Target,Pname,Params) ->
  wxe_util:cast(5277, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvi.xml">external</a> documentation.
texEnvi(Target,Pname,Param) ->
  wxe_util:cast(5278, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml">external</a> documentation.
texEnviv(Target,Pname,Params) ->
  wxe_util:cast(5279, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Coord::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGend(Coord,Pname,Param) ->
  wxe_util:cast(5280, <<Coord:?GLenum,Pname:?GLenum,Param:?GLdouble>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGendv(Coord,Pname,Params) ->
  wxe_util:cast(5281, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,0:32,
      (<< <<C:?GLdouble>> ||C <- tuple_to_list(Params)>>)/binary>>).

%% @spec (Coord::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGenf(Coord,Pname,Param) ->
  wxe_util:cast(5282, <<Coord:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGenfv(Coord,Pname,Params) ->
  wxe_util:cast(5283, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Coord::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGeni(Coord,Pname,Param) ->
  wxe_util:cast(5284, <<Coord:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Coord::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
texGeniv(Coord,Pname,Params) ->
  wxe_util:cast(5285, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage1D.xml">external</a> documentation.
texImage1D(Target,Level,Internalformat,Width,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5286, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage1D(Target,Level,Internalformat,Width,Border,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5287, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2D.xml">external</a> documentation.
texImage2D(Target,Level,Internalformat,Width,Height,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5288, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage2D(Target,Level,Internalformat,Width,Height,Border,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5289, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
texParameterf(Target,Pname,Param) ->
  wxe_util:cast(5290, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
texParameterfv(Target,Pname,Params) ->
  wxe_util:cast(5291, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
texParameteri(Target,Pname,Param) ->
  wxe_util:cast(5292, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
texParameteriv(Target,Pname,Params) ->
  wxe_util:cast(5293, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Width::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage1D.xml">external</a> documentation.
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5294, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5295, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage2D.xml">external</a> documentation.
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5296, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5297, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml">external</a> documentation.
translated(X,Y,Z) ->
  wxe_util:cast(5298, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml">external</a> documentation.
translatef(X,Y,Z) ->
  wxe_util:cast(5299, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex2d(X,Y) ->
  wxe_util:cast(5300, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2d(X,Y)
vertex2dv({X,Y}) ->  vertex2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex2f(X,Y) ->
  wxe_util:cast(5301, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2f(X,Y)
vertex2fv({X,Y}) ->  vertex2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex2i(X,Y) ->
  wxe_util:cast(5302, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2i(X,Y)
vertex2iv({X,Y}) ->  vertex2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex2s(X,Y) ->
  wxe_util:cast(5303, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv vertex2s(X,Y)
vertex2sv({X,Y}) ->  vertex2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex3d(X,Y,Z) ->
  wxe_util:cast(5304, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3d(X,Y,Z)
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex3f(X,Y,Z) ->
  wxe_util:cast(5305, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3f(X,Y,Z)
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex3i(X,Y,Z) ->
  wxe_util:cast(5306, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3i(X,Y,Z)
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex3s(X,Y,Z) ->
  wxe_util:cast(5307, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv vertex3s(X,Y,Z)
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex4d(X,Y,Z,W) ->
  wxe_util:cast(5308, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4d(X,Y,Z,W)
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex4f(X,Y,Z,W) ->
  wxe_util:cast(5309, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4f(X,Y,Z,W)
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex4i(X,Y,Z,W) ->
  wxe_util:cast(5310, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4i(X,Y,Z,W)
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
vertex4s(X,Y,Z,W) ->
  wxe_util:cast(5311, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv vertex4s(X,Y,Z,W)
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexPointer.xml">external</a> documentation.
vertexPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5312, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexPointer(Size,Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5313, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewport.xml">external</a> documentation.
viewport(X,Y,Width,Height) ->
  wxe_util:cast(5314, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Red::clamp(),Green::clamp(),Blue::clamp(),Alpha::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendColor.xml">external</a> documentation.
blendColor(Red,Green,Blue,Alpha) ->
  wxe_util:cast(5315, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @spec (Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquation.xml">external</a> documentation.
blendEquation(Mode) ->
  wxe_util:cast(5316, <<Mode:?GLenum>>).

%% @spec (Mode::enum(),Start::integer(),End::integer(),Count::integer(),Type::enum(),Indices::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawRangeElements.xml">external</a> documentation.
drawRangeElements(Mode,Start,End,Count,Type,Indices) when  is_integer(Indices) ->
  wxe_util:cast(5317, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawRangeElements(Mode,Start,End,Count,Type,Indices) ->
  wxe_util:send_bin(Indices),
  wxe_util:cast(5318, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::integer(),Width::integer(),Height::integer(),Depth::integer(),Border::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3D.xml">external</a> documentation.
texImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5319, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5320, <<Target:?GLenum,Level:?GLint,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Pixels::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage3D.xml">external</a> documentation.
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when  is_integer(Pixels) ->
  wxe_util:cast(5321, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) ->
  wxe_util:send_bin(Pixels),
  wxe_util:cast(5322, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage3D.xml">external</a> documentation.
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) ->
  wxe_util:cast(5323, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Format::enum(),Type::enum(),Table::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTable.xml">external</a> documentation.
colorTable(Target,Internalformat,Width,Format,Type,Table) when  is_integer(Table) ->
  wxe_util:cast(5324, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Table:?GLuint>>);
colorTable(Target,Internalformat,Width,Format,Type,Table) ->
  wxe_util:send_bin(Table),
  wxe_util:cast(5325, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTableParameter.xml">external</a> documentation.
colorTableParameterfv(Target,Pname,{P1,P2,P3,P4}) ->
  wxe_util:cast(5326, <<Target:?GLenum,Pname:?GLenum,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTableParameter.xml">external</a> documentation.
colorTableParameteriv(Target,Pname,{P1,P2,P3,P4}) ->
  wxe_util:cast(5327, <<Target:?GLenum,Pname:?GLenum,P1:?GLint,P2:?GLint,P3:?GLint,P4:?GLint>>).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorTable.xml">external</a> documentation.
copyColorTable(Target,Internalformat,X,Y,Width) ->
  wxe_util:cast(5328, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Format::enum(),Type::enum(),Table::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTable.xml">external</a> documentation.
getColorTable(Target,Format,Type,Table) ->
  wxe_util:send_bin(Table#wx_mem.bin),
  wxe_util:call(5329, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTableParameter.xml">external</a> documentation.
getColorTableParameterfv(Target,Pname) ->
  wxe_util:call(5330, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTableParameter.xml">external</a> documentation.
getColorTableParameteriv(Target,Pname) ->
  wxe_util:call(5331, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Start::integer(),Count::integer(),Format::enum(),Type::enum(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorSubTable.xml">external</a> documentation.
colorSubTable(Target,Start,Count,Format,Type,Data) when  is_integer(Data) ->
  wxe_util:cast(5332, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum,Data:?GLuint>>);
colorSubTable(Target,Start,Count,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5333, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Start::integer(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorSubTable.xml">external</a> documentation.
copyColorSubTable(Target,Start,X,Y,Width) ->
  wxe_util:cast(5334, <<Target:?GLenum,Start:?GLsizei,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Format::enum(),Type::enum(),Image::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter1D.xml">external</a> documentation.
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when  is_integer(Image) ->
  wxe_util:cast(5335, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) ->
  wxe_util:send_bin(Image),
  wxe_util:cast(5336, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Image::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter2D.xml">external</a> documentation.
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when  is_integer(Image) ->
  wxe_util:cast(5337, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) ->
  wxe_util:send_bin(Image),
  wxe_util:cast(5338, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionParameter.xml">external</a> documentation.
convolutionParameterf(Target,Pname,Params) ->
  wxe_util:cast(5339, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target,Pname,{Params}) -> ok
%% @equiv convolutionParameterf(Target,Pname,Params)
convolutionParameterfv(Target,Pname,{Params}) ->  convolutionParameterf(Target,Pname,Params).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionParameter.xml">external</a> documentation.
convolutionParameteri(Target,Pname,Params) ->
  wxe_util:cast(5340, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target,Pname,{Params}) -> ok
%% @equiv convolutionParameteri(Target,Pname,Params)
convolutionParameteriv(Target,Pname,{Params}) ->  convolutionParameteri(Target,Pname,Params).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter1D.xml">external</a> documentation.
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) ->
  wxe_util:cast(5341, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),X::integer(),Y::integer(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter2D.xml">external</a> documentation.
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) ->
  wxe_util:cast(5342, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Format::enum(),Type::enum(),Image::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionFilter.xml">external</a> documentation.
getConvolutionFilter(Target,Format,Type,Image) ->
  wxe_util:send_bin(Image#wx_mem.bin),
  wxe_util:call(5343, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
getConvolutionParameterfv(Target,Pname) ->
  wxe_util:call(5344, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
getConvolutionParameteriv(Target,Pname) ->
  wxe_util:call(5345, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Row::offset()|binary(),Column::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSeparableFilter2D.xml">external</a> documentation.
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when  is_integer(Row), is_integer(Column) ->
  wxe_util:cast(5346, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Row:?GLuint,Column:?GLuint>>);
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) ->
  wxe_util:send_bin(Row),
  wxe_util:send_bin(Column),
  wxe_util:cast(5347, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Reset::0|1,Format::enum(),Type::enum(),Values::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogram.xml">external</a> documentation.
getHistogram(Target,Reset,Format,Type,Values) ->
  wxe_util:send_bin(Values#wx_mem.bin),
  wxe_util:call(5348, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogramParameter.xml">external</a> documentation.
getHistogramParameterfv(Target,Pname) ->
  wxe_util:call(5349, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogramParameter.xml">external</a> documentation.
getHistogramParameteriv(Target,Pname) ->
  wxe_util:call(5350, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Reset::0|1,Format::enum(),Type::enum(),Values::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmax.xml">external</a> documentation.
getMinmax(Target,Reset,Format,Type,Values) ->
  wxe_util:send_bin(Values#wx_mem.bin),
  wxe_util:call(5351, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
getMinmaxParameterfv(Target,Pname) ->
  wxe_util:call(5352, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
getMinmaxParameteriv(Target,Pname) ->
  wxe_util:call(5353, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Width::integer(),Internalformat::enum(),Sink::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHistogram.xml">external</a> documentation.
histogram(Target,Width,Internalformat,Sink) ->
  wxe_util:cast(5354, <<Target:?GLenum,Width:?GLsizei,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @spec (Target::enum(),Internalformat::enum(),Sink::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMinmax.xml">external</a> documentation.
minmax(Target,Internalformat,Sink) ->
  wxe_util:cast(5355, <<Target:?GLenum,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetHistogram.xml">external</a> documentation.
resetHistogram(Target) ->
  wxe_util:cast(5356, <<Target:?GLenum>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetMinmax.xml">external</a> documentation.
resetMinmax(Target) ->
  wxe_util:cast(5357, <<Target:?GLenum>>).

%% @spec (Texture::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glActiveTexture.xml">external</a> documentation.
activeTexture(Texture) ->
  wxe_util:cast(5358, <<Texture:?GLenum>>).

%% @spec (Value::clamp(),Invert::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSampleCoverage.xml">external</a> documentation.
sampleCoverage(Value,Invert) ->
  wxe_util:cast(5359, <<Value:?GLclampf,Invert:?GLboolean>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Height::integer(),Depth::integer(),Border::integer(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage3D.xml">external</a> documentation.
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5360, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5361, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Height::integer(),Border::integer(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage2D.xml">external</a> documentation.
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5362, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5363, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Internalformat::enum(),Width::integer(),Border::integer(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage1D.xml">external</a> documentation.
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5364, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5365, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Zoffset::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage3D.xml">external</a> documentation.
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5366, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5367, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Yoffset::integer(),Width::integer(),Height::integer(),Format::enum(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage2D.xml">external</a> documentation.
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5368, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5369, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Xoffset::integer(),Width::integer(),Format::enum(),ImageSize::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage1D.xml">external</a> documentation.
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when  is_integer(Data) ->
  wxe_util:cast(5370, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5371, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @spec (Target::enum(),Level::integer(),Img::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetCompressedTexImage.xml">external</a> documentation.
getCompressedTexImage(Target,Level,Img) ->
  wxe_util:send_bin(Img#wx_mem.bin),
  wxe_util:call(5372, <<Target:?GLenum,Level:?GLint>>).

%% @spec (Texture::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClientActiveTexture.xml">external</a> documentation.
clientActiveTexture(Texture) ->
  wxe_util:cast(5373, <<Texture:?GLenum>>).

%% @spec (Target::enum(),S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord1d(Target,S) ->
  wxe_util:cast(5374, <<Target:?GLenum,0:32,S:?GLdouble>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1d(Target,S)
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).

%% @spec (Target::enum(),S::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord1f(Target,S) ->
  wxe_util:cast(5375, <<Target:?GLenum,S:?GLfloat>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1f(Target,S)
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).

%% @spec (Target::enum(),S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord1i(Target,S) ->
  wxe_util:cast(5376, <<Target:?GLenum,S:?GLint>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1i(Target,S)
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).

%% @spec (Target::enum(),S::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord1s(Target,S) ->
  wxe_util:cast(5377, <<Target:?GLenum,S:?GLshort>>).

%% @spec (Target,{S}) -> ok
%% @equiv multiTexCoord1s(Target,S)
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).

%% @spec (Target::enum(),S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord2d(Target,S,T) ->
  wxe_util:cast(5378, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2d(Target,S,T)
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).

%% @spec (Target::enum(),S::float(),T::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord2f(Target,S,T) ->
  wxe_util:cast(5379, <<Target:?GLenum,S:?GLfloat,T:?GLfloat>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2f(Target,S,T)
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).

%% @spec (Target::enum(),S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord2i(Target,S,T) ->
  wxe_util:cast(5380, <<Target:?GLenum,S:?GLint,T:?GLint>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2i(Target,S,T)
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).

%% @spec (Target::enum(),S::integer(),T::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord2s(Target,S,T) ->
  wxe_util:cast(5381, <<Target:?GLenum,S:?GLshort,T:?GLshort>>).

%% @spec (Target,{S,T}) -> ok
%% @equiv multiTexCoord2s(Target,S,T)
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).

%% @spec (Target::enum(),S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord3d(Target,S,T,R) ->
  wxe_util:cast(5382, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3d(Target,S,T,R)
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).

%% @spec (Target::enum(),S::float(),T::float(),R::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord3f(Target,S,T,R) ->
  wxe_util:cast(5383, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3f(Target,S,T,R)
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord3i(Target,S,T,R) ->
  wxe_util:cast(5384, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3i(Target,S,T,R)
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord3s(Target,S,T,R) ->
  wxe_util:cast(5385, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @spec (Target,{S,T,R}) -> ok
%% @equiv multiTexCoord3s(Target,S,T,R)
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).

%% @spec (Target::enum(),S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord4d(Target,S,T,R,Q) ->
  wxe_util:cast(5386, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4d(Target,S,T,R,Q)
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).

%% @spec (Target::enum(),S::float(),T::float(),R::float(),Q::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord4f(Target,S,T,R,Q) ->
  wxe_util:cast(5387, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4f(Target,S,T,R,Q)
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord4i(Target,S,T,R,Q) ->
  wxe_util:cast(5388, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4i(Target,S,T,R,Q)
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).

%% @spec (Target::enum(),S::integer(),T::integer(),R::integer(),Q::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
multiTexCoord4s(Target,S,T,R,Q) ->
  wxe_util:cast(5389, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @spec (Target,{S,T,R,Q}) -> ok
%% @equiv multiTexCoord4s(Target,S,T,R,Q)
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (SfactorRGB::enum(),DfactorRGB::enum(),SfactorAlpha::enum(),DfactorAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFuncSeparate.xml">external</a> documentation.
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) ->
  wxe_util:cast(5394, <<SfactorRGB:?GLenum,DfactorRGB:?GLenum,SfactorAlpha:?GLenum,DfactorAlpha:?GLenum>>).

%% @spec (Mode::enum(),First::[integer()],Count::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiDrawArrays.xml">external</a> documentation.
multiDrawArrays(Mode,First,Count) ->
  wxe_util:cast(5395, <<Mode:?GLenum,(length(First)):?GLuint,
        (<< <<C:?GLint>> || C <- First>>)/binary,0:(((length(First)) rem 2)*32),(length(Count)):?GLuint,
        (<< <<C:?GLsizei>> || C <- Count>>)/binary,0:(((1+length(Count)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
pointParameterf(Pname,Param) ->
  wxe_util:cast(5396, <<Pname:?GLenum,Param:?GLfloat>>).

%% @spec (Pname::enum(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
pointParameterfv(Pname,Params) ->
  wxe_util:cast(5397, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Pname::enum(),Param::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
pointParameteri(Pname,Param) ->
  wxe_util:cast(5398, <<Pname:?GLenum,Param:?GLint>>).

%% @spec (Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
pointParameteriv(Pname,Params) ->
  wxe_util:cast(5399, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @spec (Coord::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoord.xml">external</a> documentation.
fogCoordf(Coord) ->
  wxe_util:cast(5400, <<Coord:?GLfloat>>).

%% @spec ({Coord}) -> ok
%% @equiv fogCoordf(Coord)
fogCoordfv({Coord}) ->  fogCoordf(Coord).

%% @spec (Coord::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoord.xml">external</a> documentation.
fogCoordd(Coord) ->
  wxe_util:cast(5401, <<Coord:?GLdouble>>).

%% @spec ({Coord}) -> ok
%% @equiv fogCoordd(Coord)
fogCoorddv({Coord}) ->  fogCoordd(Coord).

%% @spec (Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoordPointer.xml">external</a> documentation.
fogCoordPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5402, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
fogCoordPointer(Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5403, <<Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3b(Red,Green,Blue) ->
  wxe_util:cast(5404, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3b(Red,Green,Blue)
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3d(Red,Green,Blue) ->
  wxe_util:cast(5405, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3d(Red,Green,Blue)
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).

%% @spec (Red::float(),Green::float(),Blue::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3f(Red,Green,Blue) ->
  wxe_util:cast(5406, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3f(Red,Green,Blue)
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3i(Red,Green,Blue) ->
  wxe_util:cast(5407, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3i(Red,Green,Blue)
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3s(Red,Green,Blue) ->
  wxe_util:cast(5408, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3s(Red,Green,Blue)
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3ub(Red,Green,Blue) ->
  wxe_util:cast(5409, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3ub(Red,Green,Blue)
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3ui(Red,Green,Blue) ->
  wxe_util:cast(5410, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3ui(Red,Green,Blue)
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).

%% @spec (Red::integer(),Green::integer(),Blue::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
secondaryColor3us(Red,Green,Blue) ->
  wxe_util:cast(5411, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @spec ({Red,Green,Blue}) -> ok
%% @equiv secondaryColor3us(Red,Green,Blue)
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).

%% @spec (Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColorPointer.xml">external</a> documentation.
secondaryColorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5412, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
secondaryColorPointer(Size,Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5413, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos2d(X,Y) ->
  wxe_util:cast(5414, <<X:?GLdouble,Y:?GLdouble>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2d(X,Y)
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).

%% @spec (X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos2f(X,Y) ->
  wxe_util:cast(5415, <<X:?GLfloat,Y:?GLfloat>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2f(X,Y)
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos2i(X,Y) ->
  wxe_util:cast(5416, <<X:?GLint,Y:?GLint>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2i(X,Y)
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).

%% @spec (X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos2s(X,Y) ->
  wxe_util:cast(5417, <<X:?GLshort,Y:?GLshort>>).

%% @spec ({X,Y}) -> ok
%% @equiv windowPos2s(X,Y)
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos3d(X,Y,Z) ->
  wxe_util:cast(5418, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3d(X,Y,Z)
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).

%% @spec (X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos3f(X,Y,Z) ->
  wxe_util:cast(5419, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3f(X,Y,Z)
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos3i(X,Y,Z) ->
  wxe_util:cast(5420, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3i(X,Y,Z)
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).

%% @spec (X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
windowPos3s(X,Y,Z) ->
  wxe_util:cast(5421, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec ({X,Y,Z}) -> ok
%% @equiv windowPos3s(X,Y,Z)
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenQueries.xml">external</a> documentation.
genQueries(N) ->
  wxe_util:call(5422, <<N:?GLsizei>>).

%% @spec (Ids::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteQueries.xml">external</a> documentation.
deleteQueries(Ids) ->
  wxe_util:cast(5423, <<(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+length(Ids)) rem 2)*32)>>).

%% @spec (Id::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsQuery.xml">external</a> documentation.
isQuery(Id) ->
  wxe_util:call(5424, <<Id:?GLuint>>).

%% @spec (Target::enum(),Id::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQuery.xml">external</a> documentation.
beginQuery(Target,Id) ->
  wxe_util:cast(5425, <<Target:?GLenum,Id:?GLuint>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndQuery.xml">external</a> documentation.
endQuery(Target) ->
  wxe_util:cast(5426, <<Target:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQuery.xml">external</a> documentation.
getQueryiv(Target,Pname) ->
  wxe_util:call(5427, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObject.xml">external</a> documentation.
getQueryObjectiv(Id,Pname) ->
  wxe_util:call(5428, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Id::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObject.xml">external</a> documentation.
getQueryObjectuiv(Id,Pname) ->
  wxe_util:call(5429, <<Id:?GLuint,Pname:?GLenum>>).

%% @spec (Target::enum(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBuffer.xml">external</a> documentation.
bindBuffer(Target,Buffer) ->
  wxe_util:cast(5430, <<Target:?GLenum,Buffer:?GLuint>>).

%% @spec (Buffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteBuffers.xml">external</a> documentation.
deleteBuffers(Buffers) ->
  wxe_util:cast(5431, <<(length(Buffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Buffers>>)/binary,0:(((1+length(Buffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenBuffers.xml">external</a> documentation.
genBuffers(N) ->
  wxe_util:call(5432, <<N:?GLsizei>>).

%% @spec (Buffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsBuffer.xml">external</a> documentation.
isBuffer(Buffer) ->
  wxe_util:call(5433, <<Buffer:?GLuint>>).

%% @spec (Target::enum(),Size::integer(),Data::offset()|binary(),Usage::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferData.xml">external</a> documentation.
bufferData(Target,Size,Data,Usage) when  is_integer(Data) ->
  wxe_util:cast(5434, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Data:?GLuint,Usage:?GLenum>>);
bufferData(Target,Size,Data,Usage) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5435, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Usage:?GLenum>>).

%% @spec (Target::enum(),Offset::integer(),Size::integer(),Data::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferSubData.xml">external</a> documentation.
bufferSubData(Target,Offset,Size,Data) when  is_integer(Data) ->
  wxe_util:cast(5436, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr,Data:?GLuint>>);
bufferSubData(Target,Offset,Size,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:cast(5437, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Offset::integer(),Size::integer(),Data::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferSubData.xml">external</a> documentation.
getBufferSubData(Target,Offset,Size,Data) ->
  wxe_util:send_bin(Data#wx_mem.bin),
  wxe_util:call(5438, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameteriv.xml">external</a> documentation.
getBufferParameteriv(Target,Pname) ->
  wxe_util:call(5439, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (ModeRGB::enum(),ModeAlpha::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquationSeparate.xml">external</a> documentation.
blendEquationSeparate(ModeRGB,ModeAlpha) ->
  wxe_util:cast(5440, <<ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @spec (Bufs::[enum()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffers.xml">external</a> documentation.
drawBuffers(Bufs) ->
  wxe_util:cast(5441, <<(length(Bufs)):?GLuint,
        (<< <<C:?GLenum>> || C <- Bufs>>)/binary,0:(((1+length(Bufs)) rem 2)*32)>>).

%% @spec (Face::enum(),Sfail::enum(),Dpfail::enum(),Dppass::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOpSeparate.xml">external</a> documentation.
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) ->
  wxe_util:cast(5442, <<Face:?GLenum,Sfail:?GLenum,Dpfail:?GLenum,Dppass:?GLenum>>).

%% @spec (Frontfunc::enum(),Backfunc::enum(),Ref::integer(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFuncSeparate.xml">external</a> documentation.
stencilFuncSeparate(Frontfunc,Backfunc,Ref,Mask) ->
  wxe_util:cast(5443, <<Frontfunc:?GLenum,Backfunc:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @spec (Face::enum(),Mask::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMaskSeparate.xml">external</a> documentation.
stencilMaskSeparate(Face,Mask) ->
  wxe_util:cast(5444, <<Face:?GLenum,Mask:?GLuint>>).

%% @spec (Program::integer(),Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachShader.xml">external</a> documentation.
attachShader(Program,Shader) ->
  wxe_util:cast(5445, <<Program:?GLuint,Shader:?GLuint>>).

%% @spec (Program::integer(),Index::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocation.xml">external</a> documentation.
bindAttribLocation(Program,Index,Name) ->
  wxe_util:cast(5446, <<Program:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShader.xml">external</a> documentation.
compileShader(Shader) ->
  wxe_util:cast(5447, <<Shader:?GLuint>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml">external</a> documentation.
createProgram() ->
  wxe_util:call(5448, <<>>).

%% @spec (Type::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml">external</a> documentation.
createShader(Type) ->
  wxe_util:call(5449, <<Type:?GLenum>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgram.xml">external</a> documentation.
deleteProgram(Program) ->
  wxe_util:cast(5450, <<Program:?GLuint>>).

%% @spec (Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteShader.xml">external</a> documentation.
deleteShader(Shader) ->
  wxe_util:cast(5451, <<Shader:?GLuint>>).

%% @spec (Program::integer(),Shader::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachShader.xml">external</a> documentation.
detachShader(Program,Shader) ->
  wxe_util:cast(5452, <<Program:?GLuint,Shader:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisableVertexAttribArray.xml">external</a> documentation.
disableVertexAttribArray(Index) ->
  wxe_util:cast(5453, <<Index:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml">external</a> documentation.
enableVertexAttribArray(Index) ->
  wxe_util:cast(5454, <<Index:?GLuint>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttrib.xml">external</a> documentation.
getActiveAttrib(Program,Index,BufSize) ->
  wxe_util:call(5455, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniform.xml">external</a> documentation.
getActiveUniform(Program,Index,BufSize) ->
  wxe_util:call(5456, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),MaxCount::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedShaders.xml">external</a> documentation.
getAttachedShaders(Program,MaxCount) ->
  wxe_util:call(5457, <<Program:?GLuint,MaxCount:?GLsizei>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocation.xml">external</a> documentation.
getAttribLocation(Program,Name) ->
  wxe_util:call(5458, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgram.xml">external</a> documentation.
getProgramiv(Program,Pname) ->
  wxe_util:call(5459, <<Program:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramInfoLog.xml">external</a> documentation.
getProgramInfoLog(Program,BufSize) ->
  wxe_util:call(5460, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @spec (Shader::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShader.xml">external</a> documentation.
getShaderiv(Shader,Pname) ->
  wxe_util:call(5461, <<Shader:?GLuint,Pname:?GLenum>>).

%% @spec (Shader::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderInfoLog.xml">external</a> documentation.
getShaderInfoLog(Shader,BufSize) ->
  wxe_util:call(5462, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @spec (Shader::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSource.xml">external</a> documentation.
getShaderSource(Shader,BufSize) ->
  wxe_util:call(5463, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocation.xml">external</a> documentation.
getUniformLocation(Program,Name) ->
  wxe_util:call(5464, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Location::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
getUniformfv(Program,Location) ->
  wxe_util:call(5465, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Program::integer(),Location::integer()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
getUniformiv(Program,Location) ->
  wxe_util:call(5466, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Index::integer(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
getVertexAttribdv(Index,Pname) ->
  wxe_util:call(5467, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
getVertexAttribfv(Index,Pname) ->
  wxe_util:call(5468, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
getVertexAttribiv(Index,Pname) ->
  wxe_util:call(5469, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsProgram.xml">external</a> documentation.
isProgram(Program) ->
  wxe_util:call(5470, <<Program:?GLuint>>).

%% @spec (Shader::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsShader.xml">external</a> documentation.
isShader(Shader) ->
  wxe_util:call(5471, <<Shader:?GLuint>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgram.xml">external</a> documentation.
linkProgram(Program) ->
  wxe_util:cast(5472, <<Program:?GLuint>>).

%% @spec (Shader::integer(),String::[string()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSource.xml">external</a> documentation.
shaderSource(Shader,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  wxe_util:cast(5473, <<Shader:?GLuint,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+0) rem 8)) rem 8)>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgram.xml">external</a> documentation.
useProgram(Program) ->
  wxe_util:cast(5474, <<Program:?GLuint>>).

%% @spec (Location::integer(),V0::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1f(Location,V0) ->
  wxe_util:cast(5475, <<Location:?GLint,V0:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2f(Location,V0,V1) ->
  wxe_util:cast(5476, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float(),V2::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3f(Location,V0,V1,V2) ->
  wxe_util:cast(5477, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @spec (Location::integer(),V0::float(),V1::float(),V2::float(),V3::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4f(Location,V0,V1,V2,V3) ->
  wxe_util:cast(5478, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @spec (Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1i(Location,V0) ->
  wxe_util:cast(5479, <<Location:?GLint,V0:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2i(Location,V0,V1) ->
  wxe_util:cast(5480, <<Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3i(Location,V0,V1,V2) ->
  wxe_util:cast(5481, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4i(Location,V0,V1,V2,V3) ->
  wxe_util:cast(5482, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @spec (Location::integer(),Value::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1fv(Location,Value) ->
  wxe_util:cast(5483, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2fv(Location,Value) ->
  wxe_util:cast(5484, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3fv(Location,Value) ->
  wxe_util:cast(5485, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4fv(Location,Value) ->
  wxe_util:cast(5486, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1iv(Location,Value) ->
  wxe_util:cast(5487, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2iv(Location,Value) ->
  wxe_util:cast(5488, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3iv(Location,Value) ->
  wxe_util:cast(5489, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4iv(Location,Value) ->
  wxe_util:cast(5490, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
uniformMatrix2fv(Location,Transpose,Value) ->
  wxe_util:cast(5491, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
uniformMatrix3fv(Location,Transpose,Value) ->
  wxe_util:cast(5492, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix.xml">external</a> documentation.
uniformMatrix4fv(Location,Transpose,Value) ->
  wxe_util:cast(5493, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @spec (Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgram.xml">external</a> documentation.
validateProgram(Program) ->
  wxe_util:cast(5494, <<Program:?GLuint>>).

%% @spec (Index::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib1d(Index,X) ->
  wxe_util:cast(5495, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1d(Index,X)
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).

%% @spec (Index::integer(),X::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib1f(Index,X) ->
  wxe_util:cast(5496, <<Index:?GLuint,X:?GLfloat>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1f(Index,X)
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib1s(Index,X) ->
  wxe_util:cast(5497, <<Index:?GLuint,X:?GLshort>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttrib1s(Index,X)
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).

%% @spec (Index::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib2d(Index,X,Y) ->
  wxe_util:cast(5498, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2d(Index,X,Y)
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).

%% @spec (Index::integer(),X::float(),Y::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib2f(Index,X,Y) ->
  wxe_util:cast(5499, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2f(Index,X,Y)
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib2s(Index,X,Y) ->
  wxe_util:cast(5500, <<Index:?GLuint,X:?GLshort,Y:?GLshort>>).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttrib2s(Index,X,Y)
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib3d(Index,X,Y,Z) ->
  wxe_util:cast(5501, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3d(Index,X,Y,Z)
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib3f(Index,X,Y,Z) ->
  wxe_util:cast(5502, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3f(Index,X,Y,Z)
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib3s(Index,X,Y,Z) ->
  wxe_util:cast(5503, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttrib3s(Index,X,Y,Z)
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Nbv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5504, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Niv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5505, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Nsv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5506, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Nub(Index,X,Y,Z,W) ->
  wxe_util:cast(5507, <<Index:?GLuint,X:?GLubyte,Y:?GLubyte,Z:?GLubyte,W:?GLubyte>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Nuiv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5508, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4Nusv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5509, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4bv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5510, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4d(Index,X,Y,Z,W) ->
  wxe_util:cast(5511, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).

%% @spec (Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4f(Index,X,Y,Z,W) ->
  wxe_util:cast(5512, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4iv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5513, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4s(Index,X,Y,Z,W) ->
  wxe_util:cast(5514, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4ubv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5515, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4uiv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5516, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
vertexAttrib4usv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5517, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Index::integer(),Size::integer(),Type::enum(),Normalized::0|1,Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml">external</a> documentation.
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5518, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5519, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
uniformMatrix2x3fv(Location,Transpose,Value) ->
  wxe_util:cast(5520, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
uniformMatrix3x2fv(Location,Transpose,Value) ->
  wxe_util:cast(5521, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix2x.xml">external</a> documentation.
uniformMatrix2x4fv(Location,Transpose,Value) ->
  wxe_util:cast(5522, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
uniformMatrix4x2fv(Location,Transpose,Value) ->
  wxe_util:cast(5523, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix3x.xml">external</a> documentation.
uniformMatrix3x4fv(Location,Transpose,Value) ->
  wxe_util:cast(5524, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Location::integer(),Transpose::0|1,Value::[{float()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformMatrix4x.xml">external</a> documentation.
uniformMatrix4x3fv(Location,Transpose,Value) ->
  wxe_util:cast(5525, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @spec (Index::integer(),R::0|1,G::0|1,B::0|1,A::0|1) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaski.xml">external</a> documentation.
colorMaski(Index,R,G,B,A) ->
  wxe_util:cast(5526, <<Index:?GLuint,R:?GLboolean,G:?GLboolean,B:?GLboolean,A:?GLboolean>>).

%% @spec (Target::enum(),Index::integer()) -> [0|1]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBooleani_v.xml">external</a> documentation.
getBooleani_v(Target,Index) ->
  wxe_util:call(5527, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetIntegeri_v.xml">external</a> documentation.
getIntegeri_v(Target,Index) ->
  wxe_util:call(5528, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml">external</a> documentation.
enablei(Target,Index) ->
  wxe_util:cast(5529, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDisable.xml">external</a> documentation.
disablei(Target,Index) ->
  wxe_util:cast(5530, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabledi.xml">external</a> documentation.
isEnabledi(Target,Index) ->
  wxe_util:call(5531, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (PrimitiveMode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginTransformFeedback.xml">external</a> documentation.
beginTransformFeedback(PrimitiveMode) ->
  wxe_util:cast(5532, <<PrimitiveMode:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndTransformFeedback.xml">external</a> documentation.
endTransformFeedback() ->
  wxe_util:cast(5533, <<>>).

%% @spec (Target::enum(),Index::integer(),Buffer::integer(),Offset::integer(),Size::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferRange.xml">external</a> documentation.
bindBufferRange(Target,Index,Buffer,Offset,Size) ->
  wxe_util:cast(5534, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec (Target::enum(),Index::integer(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferBase.xml">external</a> documentation.
bindBufferBase(Target,Index,Buffer) ->
  wxe_util:cast(5535, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint>>).

%% @spec (Program::integer(),Varyings::[string()],BufferMode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTransformFeedbackVaryings.xml">external</a> documentation.
transformFeedbackVaryings(Program,Varyings,BufferMode) ->
 VaryingsTemp = list_to_binary([[Str|[0]] || Str <- Varyings ]),
  wxe_util:cast(5536, <<Program:?GLuint,(length(Varyings)):?GLuint,(size(VaryingsTemp)):?GLuint,(VaryingsTemp)/binary,0:((8-((size(VaryingsTemp)+0) rem 8)) rem 8),BufferMode:?GLenum>>).

%% @spec (Program::integer(),Index::integer(),BufSize::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTransformFeedbackVarying.xml">external</a> documentation.
getTransformFeedbackVarying(Program,Index,BufSize) ->
  wxe_util:call(5537, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @spec (Target::enum(),Clamp::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClampColor.xml">external</a> documentation.
clampColor(Target,Clamp) ->
  wxe_util:cast(5538, <<Target:?GLenum,Clamp:?GLenum>>).

%% @spec (Id::integer(),Mode::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginConditionalRender.xml">external</a> documentation.
beginConditionalRender(Id,Mode) ->
  wxe_util:cast(5539, <<Id:?GLuint,Mode:?GLenum>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEndConditionalRender.xml">external</a> documentation.
endConditionalRender() ->
  wxe_util:cast(5540, <<>>).

%% @spec (Index::integer(),Size::integer(),Type::enum(),Stride::integer(),Pointer::offset()|binary()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribIPointer.xml">external</a> documentation.
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  wxe_util:cast(5541, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) ->
  wxe_util:send_bin(Pointer),
  wxe_util:cast(5542, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribI.xml">external</a> documentation.
getVertexAttribIiv(Index,Pname) ->
  wxe_util:call(5543, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Index::integer(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribI.xml">external</a> documentation.
getVertexAttribIuiv(Index,Pname) ->
  wxe_util:call(5544, <<Index:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),Location::integer()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
getUniformuiv(Program,Location) ->
  wxe_util:call(5545, <<Program:?GLuint,Location:?GLint>>).

%% @spec (Program::integer(),Color::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFragDataLocation.xml">external</a> documentation.
bindFragDataLocation(Program,Color,Name) ->
  wxe_util:cast(5546, <<Program:?GLuint,Color:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Program::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFragDataLocation.xml">external</a> documentation.
getFragDataLocation(Program,Name) ->
  wxe_util:call(5547, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (Location::integer(),V0::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1ui(Location,V0) ->
  wxe_util:cast(5548, <<Location:?GLint,V0:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2ui(Location,V0,V1) ->
  wxe_util:cast(5549, <<Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3ui(Location,V0,V1,V2) ->
  wxe_util:cast(5550, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @spec (Location::integer(),V0::integer(),V1::integer(),V2::integer(),V3::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4ui(Location,V0,V1,V2,V3) ->
  wxe_util:cast(5551, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @spec (Location::integer(),Value::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform1uiv(Location,Value) ->
  wxe_util:cast(5552, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform2uiv(Location,Value) ->
  wxe_util:cast(5553, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform3uiv(Location,Value) ->
  wxe_util:cast(5554, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @spec (Location::integer(),Value::[{integer()}]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
uniform4uiv(Location,Value) ->
  wxe_util:cast(5555, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameterI.xml">external</a> documentation.
texParameterIiv(Target,Pname,Params) ->
  wxe_util:cast(5556, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum(),Params::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameterI.xml">external</a> documentation.
texParameterIuiv(Target,Pname,Params) ->
  wxe_util:cast(5557, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameterI.xml">external</a> documentation.
getTexParameterIiv(Target,Pname) ->
  wxe_util:call(5558, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum(),Pname::enum()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameterI.xml">external</a> documentation.
getTexParameterIuiv(Target,Pname) ->
  wxe_util:call(5559, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
clearBufferiv(Buffer,Drawbuffer,Value) ->
  wxe_util:cast(5560, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
clearBufferuiv(Buffer,Drawbuffer,Value) ->
  wxe_util:cast(5561, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Value::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
clearBufferfv(Buffer,Drawbuffer,Value) ->
  wxe_util:cast(5562, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @spec (Buffer::enum(),Drawbuffer::integer(),Depth::float(),Stencil::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBufferfi.xml">external</a> documentation.
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) ->
  wxe_util:cast(5563, <<Buffer:?GLenum,Drawbuffer:?GLint,Depth:?GLfloat,Stencil:?GLint>>).

%% @spec (Name::enum(),Index::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml">external</a> documentation.
getStringi(Name,Index) ->
  wxe_util:call(5564, <<Name:?GLenum,Index:?GLuint>>).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI1i(Index,X) ->
  wxe_util:cast(5565, <<Index:?GLuint,X:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI2i(Index,X,Y) ->
  wxe_util:cast(5566, <<Index:?GLuint,X:?GLint,Y:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI3i(Index,X,Y,Z) ->
  wxe_util:cast(5567, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4i(Index,X,Y,Z,W) ->
  wxe_util:cast(5568, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec (Index::integer(),X::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI1ui(Index,X) ->
  wxe_util:cast(5569, <<Index:?GLuint,X:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI2ui(Index,X,Y) ->
  wxe_util:cast(5570, <<Index:?GLuint,X:?GLuint,Y:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI3ui(Index,X,Y,Z) ->
  wxe_util:cast(5571, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint>>).

%% @spec (Index::integer(),X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4ui(Index,X,Y,Z,W) ->
  wxe_util:cast(5572, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint,W:?GLuint>>).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttribI1i(Index,X)
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttribI2i(Index,X,Y)
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttribI3i(Index,X,Y,Z)
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttribI4i(Index,X,Y,Z,W)
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).

%% @spec (Index,{X}) -> ok
%% @equiv vertexAttribI1ui(Index,X)
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).

%% @spec (Index,{X,Y}) -> ok
%% @equiv vertexAttribI2ui(Index,X,Y)
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).

%% @spec (Index,{X,Y,Z}) -> ok
%% @equiv vertexAttribI3ui(Index,X,Y,Z)
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).

%% @spec (Index,{X,Y,Z,W}) -> ok
%% @equiv vertexAttribI4ui(Index,X,Y,Z,W)
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4bv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5573, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4sv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5574, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4ubv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5575, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @spec (Index::integer(),V::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribI.xml">external</a> documentation.
vertexAttribI4usv(Index,{V1,V2,V3,V4}) ->
  wxe_util:cast(5576, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @spec (Mode::enum(),First::integer(),Count::integer(),Primcount::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysInstance.xml">external</a> documentation.
drawArraysInstanced(Mode,First,Count,Primcount) ->
  wxe_util:cast(5577, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei>>).

%% @spec (Mode::enum(),Count::integer(),Type::enum(),Indices::offset()|binary(),Primcount::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstance.xml">external</a> documentation.
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) when  is_integer(Indices) ->
  wxe_util:cast(5578, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei>>);
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) ->
  wxe_util:send_bin(Indices),
  wxe_util:cast(5579, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Buffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexBuffer.xml">external</a> documentation.
texBuffer(Target,Internalformat,Buffer) ->
  wxe_util:cast(5580, <<Target:?GLenum,Internalformat:?GLenum,Buffer:?GLuint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrimitiveRestartIndex.xml">external</a> documentation.
primitiveRestartIndex(Index) ->
  wxe_util:cast(5581, <<Index:?GLuint>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5582, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5582, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5583, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5583, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5584, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5584, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @spec (M::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  wxe_util:cast(5585, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  wxe_util:cast(5585, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightbvARB(Weights) ->
  wxe_util:cast(5586, <<(length(Weights)):?GLuint,
        (<< <<C:?GLbyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightsvARB(Weights) ->
  wxe_util:cast(5587, <<(length(Weights)):?GLuint,
        (<< <<C:?GLshort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightivARB(Weights) ->
  wxe_util:cast(5588, <<(length(Weights)):?GLuint,
        (<< <<C:?GLint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Weights::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightfvARB(Weights) ->
  wxe_util:cast(5589, <<(length(Weights)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Weights::[float()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightdvARB(Weights) ->
  wxe_util:cast(5590, <<(length(Weights)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Weights>>)/binary>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightubvARB(Weights) ->
  wxe_util:cast(5591, <<(length(Weights)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightusvARB(Weights) ->
  wxe_util:cast(5592, <<(length(Weights)):?GLuint,
        (<< <<C:?GLushort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Weights::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
weightuivARB(Weights) ->
  wxe_util:cast(5593, <<(length(Weights)):?GLuint,
        (<< <<C:?GLuint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @spec (Count::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexBlenARB.xml">external</a> documentation.
vertexBlendARB(Count) ->
  wxe_util:cast(5594, <<Count:?GLint>>).

%% @spec (Index::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCurrentPaletteMatrixARB.xml">external</a> documentation.
currentPaletteMatrixARB(Index) ->
  wxe_util:cast(5595, <<Index:?GLint>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
matrixIndexubvARB(Indices) ->
  wxe_util:cast(5596, <<(length(Indices)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Indices>>)/binary,0:((8-((length(Indices)+ 4) rem 8)) rem 8)>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
matrixIndexusvARB(Indices) ->
  wxe_util:cast(5597, <<(length(Indices)):?GLuint,
        (<< <<C:?GLushort>> || C <- Indices>>)/binary,0:((8-((length(Indices)*2+ 4) rem 8)) rem 8)>>).

%% @spec (Indices::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
matrixIndexuivARB(Indices) ->
  wxe_util:cast(5598, <<(length(Indices)):?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((1+length(Indices)) rem 2)*32)>>).

%% @spec (Target::enum(),Format::enum(),String::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramStringARB.xml">external</a> documentation.
programStringARB(Target,Format,String) ->
  wxe_util:cast(5599, <<Target:?GLenum,Format:?GLenum,(list_to_binary([String|[0]]))/binary,0:((8-((length(String)+ 1) rem 8)) rem 8)>>).

%% @spec (Target::enum(),Program::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindProgramARB.xml">external</a> documentation.
bindProgramARB(Target,Program) ->
  wxe_util:cast(5600, <<Target:?GLenum,Program:?GLuint>>).

%% @spec (Programs::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgramsARB.xml">external</a> documentation.
deleteProgramsARB(Programs) ->
  wxe_util:cast(5601, <<(length(Programs)):?GLuint,
        (<< <<C:?GLuint>> || C <- Programs>>)/binary,0:(((1+length(Programs)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenProgramsARB.xml">external</a> documentation.
genProgramsARB(N) ->
  wxe_util:call(5602, <<N:?GLsizei>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
programEnvParameter4dARB(Target,Index,X,Y,Z,W) ->
  wxe_util:cast(5603, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
programEnvParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  wxe_util:cast(5604, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
programEnvParameter4fARB(Target,Index,X,Y,Z,W) ->
  wxe_util:cast(5605, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
programEnvParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  wxe_util:cast(5606, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
programLocalParameter4dARB(Target,Index,X,Y,Z,W) ->
  wxe_util:cast(5607, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
programLocalParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  wxe_util:cast(5608, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @spec (Target::enum(),Index::integer(),X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
programLocalParameter4fARB(Target,Index,X,Y,Z,W) ->
  wxe_util:cast(5609, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec (Target::enum(),Index::integer(),Params::{float()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
programLocalParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  wxe_util:cast(5610, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @spec (Target::enum(),Index::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
getProgramEnvParameterdvARB(Target,Index) ->
  wxe_util:call(5611, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
getProgramEnvParameterfvARB(Target,Index) ->
  wxe_util:call(5612, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
getProgramLocalParameterdvARB(Target,Index) ->
  wxe_util:call(5613, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Index::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
getProgramLocalParameterfvARB(Target,Index) ->
  wxe_util:call(5614, <<Target:?GLenum,Index:?GLuint>>).

%% @spec (Target::enum(),Pname::enum(),String::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramStringARB.xml">external</a> documentation.
getProgramStringARB(Target,Pname,String) ->
  wxe_util:send_bin(String#wx_mem.bin),
  wxe_util:call(5615, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Obj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteObjectARB.xml">external</a> documentation.
deleteObjectARB(Obj) ->
  wxe_util:cast(5616, <<Obj:?GLhandleARB>>).

%% @spec (Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHandleARB.xml">external</a> documentation.
getHandleARB(Pname) ->
  wxe_util:call(5617, <<Pname:?GLenum>>).

%% @spec (ContainerObj::integer(),AttachedObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachObjectARB.xml">external</a> documentation.
detachObjectARB(ContainerObj,AttachedObj) ->
  wxe_util:cast(5618, <<ContainerObj:?GLhandleARB,AttachedObj:?GLhandleARB>>).

%% @spec (ShaderType::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShaderObjectARB.xml">external</a> documentation.
createShaderObjectARB(ShaderType) ->
  wxe_util:call(5619, <<ShaderType:?GLenum>>).

%% @spec (ShaderObj::integer(),String::[string()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSourceARB.xml">external</a> documentation.
shaderSourceARB(ShaderObj,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  wxe_util:cast(5620, <<ShaderObj:?GLhandleARB,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+4) rem 8)) rem 8)>>).

%% @spec (ShaderObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShaderARB.xml">external</a> documentation.
compileShaderARB(ShaderObj) ->
  wxe_util:cast(5621, <<ShaderObj:?GLhandleARB>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgramObjectARB.xml">external</a> documentation.
createProgramObjectARB() ->
  wxe_util:call(5622, <<>>).

%% @spec (ContainerObj::integer(),Obj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachObjectARB.xml">external</a> documentation.
attachObjectARB(ContainerObj,Obj) ->
  wxe_util:cast(5623, <<ContainerObj:?GLhandleARB,Obj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgramARB.xml">external</a> documentation.
linkProgramARB(ProgramObj) ->
  wxe_util:cast(5624, <<ProgramObj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgramObjectARB.xml">external</a> documentation.
useProgramObjectARB(ProgramObj) ->
  wxe_util:cast(5625, <<ProgramObj:?GLhandleARB>>).

%% @spec (ProgramObj::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgramARB.xml">external</a> documentation.
validateProgramARB(ProgramObj) ->
  wxe_util:cast(5626, <<ProgramObj:?GLhandleARB>>).

%% @spec (Obj::integer(),Pname::enum()) -> float()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
getObjectParameterfvARB(Obj,Pname) ->
  wxe_util:call(5627, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @spec (Obj::integer(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
getObjectParameterivARB(Obj,Pname) ->
  wxe_util:call(5628, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @spec (Obj::integer(),MaxLength::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInfoLogARB.xml">external</a> documentation.
getInfoLogARB(Obj,MaxLength) ->
  wxe_util:call(5629, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @spec (ContainerObj::integer(),MaxCount::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedObjectsARB.xml">external</a> documentation.
getAttachedObjectsARB(ContainerObj,MaxCount) ->
  wxe_util:call(5630, <<ContainerObj:?GLhandleARB,MaxCount:?GLsizei>>).

%% @spec (ProgramObj::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocationARB.xml">external</a> documentation.
getUniformLocationARB(ProgramObj,Name) ->
  wxe_util:call(5631, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (ProgramObj::integer(),Index::integer(),MaxLength::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformARB.xml">external</a> documentation.
getActiveUniformARB(ProgramObj,Index,MaxLength) ->
  wxe_util:call(5632, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Location::integer()) -> {float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
getUniformfvARB(ProgramObj,Location) ->
  wxe_util:call(5633, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @spec (ProgramObj::integer(),Location::integer()) -> {integer()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
getUniformivARB(ProgramObj,Location) ->
  wxe_util:call(5634, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @spec (Obj::integer(),MaxLength::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSourceARB.xml">external</a> documentation.
getShaderSourceARB(Obj,MaxLength) ->
  wxe_util:call(5635, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Index::integer(),Name::string()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocationARB.xml">external</a> documentation.
bindAttribLocationARB(ProgramObj,Index,Name) ->
  wxe_util:cast(5636, <<ProgramObj:?GLhandleARB,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @spec (ProgramObj::integer(),Index::integer(),MaxLength::integer()) -> {Size::integer(),Type::enum(),Name::string()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttribARB.xml">external</a> documentation.
getActiveAttribARB(ProgramObj,Index,MaxLength) ->
  wxe_util:call(5637, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @spec (ProgramObj::integer(),Name::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocationARB.xml">external</a> documentation.
getAttribLocationARB(ProgramObj,Name) ->
  wxe_util:call(5638, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @spec (Renderbuffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsRenderbuffer.xml">external</a> documentation.
isRenderbuffer(Renderbuffer) ->
  wxe_util:call(5639, <<Renderbuffer:?GLuint>>).

%% @spec (Target::enum(),Renderbuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindRenderbuffer.xml">external</a> documentation.
bindRenderbuffer(Target,Renderbuffer) ->
  wxe_util:cast(5640, <<Target:?GLenum,Renderbuffer:?GLuint>>).

%% @spec (Renderbuffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteRenderbuffers.xml">external</a> documentation.
deleteRenderbuffers(Renderbuffers) ->
  wxe_util:cast(5641, <<(length(Renderbuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Renderbuffers>>)/binary,0:(((1+length(Renderbuffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenRenderbuffers.xml">external</a> documentation.
genRenderbuffers(N) ->
  wxe_util:call(5642, <<N:?GLsizei>>).

%% @spec (Target::enum(),Internalformat::enum(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorage.xml">external</a> documentation.
renderbufferStorage(Target,Internalformat,Width,Height) ->
  wxe_util:cast(5643, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetRenderbufferParameter.xml">external</a> documentation.
getRenderbufferParameteriv(Target,Pname) ->
  wxe_util:call(5644, <<Target:?GLenum,Pname:?GLenum>>).

%% @spec (Framebuffer::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsFramebuffer.xml">external</a> documentation.
isFramebuffer(Framebuffer) ->
  wxe_util:call(5645, <<Framebuffer:?GLuint>>).

%% @spec (Target::enum(),Framebuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFramebuffer.xml">external</a> documentation.
bindFramebuffer(Target,Framebuffer) ->
  wxe_util:cast(5646, <<Target:?GLenum,Framebuffer:?GLuint>>).

%% @spec (Framebuffers::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteFramebuffers.xml">external</a> documentation.
deleteFramebuffers(Framebuffers) ->
  wxe_util:cast(5647, <<(length(Framebuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Framebuffers>>)/binary,0:(((1+length(Framebuffers)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenFramebuffers.xml">external</a> documentation.
genFramebuffers(N) ->
  wxe_util:call(5648, <<N:?GLsizei>>).

%% @spec (Target::enum()) -> enum()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCheckFramebufferStatus.xml">external</a> documentation.
checkFramebufferStatus(Target) ->
  wxe_util:call(5649, <<Target:?GLenum>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture1D.xml">external</a> documentation.
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) ->
  wxe_util:cast(5650, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture2D.xml">external</a> documentation.
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) ->
  wxe_util:cast(5651, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Textarget::enum(),Texture::integer(),Level::integer(),Zoffset::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture3D.xml">external</a> documentation.
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) ->
  wxe_util:cast(5652, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint,Zoffset:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Renderbuffertarget::enum(),Renderbuffer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferRenderbuffer.xml">external</a> documentation.
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) ->
  wxe_util:cast(5653, <<Target:?GLenum,Attachment:?GLenum,Renderbuffertarget:?GLenum,Renderbuffer:?GLuint>>).

%% @spec (Target::enum(),Attachment::enum(),Pname::enum()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFramebufferAttachmentParameter.xml">external</a> documentation.
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) ->
  wxe_util:call(5654, <<Target:?GLenum,Attachment:?GLenum,Pname:?GLenum>>).

%% @spec (Target::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenerateMipmap.xml">external</a> documentation.
generateMipmap(Target) ->
  wxe_util:cast(5655, <<Target:?GLenum>>).

%% @spec (SrcX0::integer(),SrcY0::integer(),SrcX1::integer(),SrcY1::integer(),DstX0::integer(),DstY0::integer(),DstX1::integer(),DstY1::integer(),Mask::integer(),Filter::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlitFramebuffer.xml">external</a> documentation.
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) ->
  wxe_util:cast(5656, <<SrcX0:?GLint,SrcY0:?GLint,SrcX1:?GLint,SrcY1:?GLint,DstX0:?GLint,DstY0:?GLint,DstX1:?GLint,DstY1:?GLint,Mask:?GLbitfield,Filter:?GLenum>>).

%% @spec (Target::enum(),Samples::integer(),Internalformat::enum(),Width::integer(),Height::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorageMultisample.xml">external</a> documentation.
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) ->
  wxe_util:cast(5657, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer(),Layer::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTextureLayer.xml">external</a> documentation.
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) ->
  wxe_util:cast(5658, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Layer:?GLint>>).

%% @spec (Program::integer(),Pname::enum(),Value::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramParameterARB.xml">external</a> documentation.
programParameteriARB(Program,Pname,Value) ->
  wxe_util:cast(5659, <<Program:?GLuint,Pname:?GLenum,Value:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTextureARB.xml">external</a> documentation.
framebufferTextureARB(Target,Attachment,Texture,Level) ->
  wxe_util:cast(5660, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @spec (Target::enum(),Attachment::enum(),Texture::integer(),Level::integer(),Face::enum()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTextureFaceARB.xml">external</a> documentation.
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) ->
  wxe_util:cast(5661, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Face:?GLenum>>).

%% @spec (Index::integer(),Divisor::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribDivisorARB.xml">external</a> documentation.
vertexAttribDivisorARB(Index,Divisor) ->
  wxe_util:cast(5662, <<Index:?GLuint,Divisor:?GLuint>>).

%% @spec (Target::enum(),Offset::integer(),Length::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlushMappedBufferRange.xml">external</a> documentation.
flushMappedBufferRange(Target,Offset,Length) ->
  wxe_util:cast(5663, <<Target:?GLenum,0:32,Offset:?GLintptr,Length:?GLsizeiptr>>).

%% @spec (Array::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindVertexArray.xml">external</a> documentation.
bindVertexArray(Array) ->
  wxe_util:cast(5664, <<Array:?GLuint>>).

%% @spec (Arrays::[integer()]) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteVertexArrays.xml">external</a> documentation.
deleteVertexArrays(Arrays) ->
  wxe_util:cast(5665, <<(length(Arrays)):?GLuint,
        (<< <<C:?GLuint>> || C <- Arrays>>)/binary,0:(((1+length(Arrays)) rem 2)*32)>>).

%% @spec (N::integer()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenVertexArrays.xml">external</a> documentation.
genVertexArrays(N) ->
  wxe_util:call(5666, <<N:?GLsizei>>).

%% @spec (Array::integer()) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsVertexArray.xml">external</a> documentation.
isVertexArray(Array) ->
  wxe_util:call(5667, <<Array:?GLuint>>).

%% @spec (Program::integer(),UniformNames::[string()]) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformIndices.xml">external</a> documentation.
getUniformIndices(Program,UniformNames) ->
 UniformNamesTemp = list_to_binary([[Str|[0]] || Str <- UniformNames ]),
  wxe_util:call(5668, <<Program:?GLuint,(length(UniformNames)):?GLuint,(size(UniformNamesTemp)):?GLuint,(UniformNamesTemp)/binary,0:((8-((size(UniformNamesTemp)+0) rem 8)) rem 8)>>).

%% @spec (Program::integer(),UniformIndices::[integer()],Pname::enum()) -> [integer()]
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniforms.xml">external</a> documentation.
getActiveUniformsiv(Program,UniformIndices,Pname) ->
  wxe_util:call(5669, <<Program:?GLuint,(length(UniformIndices)):?GLuint,
        (<< <<C:?GLuint>> || C <- UniformIndices>>)/binary,0:(((length(UniformIndices)) rem 2)*32),Pname:?GLenum>>).

%% @spec (Program::integer(),UniformIndex::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformName.xml">external</a> documentation.
getActiveUniformName(Program,UniformIndex,BufSize) ->
  wxe_util:call(5670, <<Program:?GLuint,UniformIndex:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),UniformBlockName::string()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformBlockIndex.xml">external</a> documentation.
getUniformBlockIndex(Program,UniformBlockName) ->
  wxe_util:call(5671, <<Program:?GLuint,(list_to_binary([UniformBlockName|[0]]))/binary,0:((8-((length(UniformBlockName)+ 5) rem 8)) rem 8)>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),Pname::enum(),Params::wx:wx_mem()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlock.xml">external</a> documentation.
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) ->
  wxe_util:send_bin(Params#wx_mem.bin),
  wxe_util:call(5672, <<Program:?GLuint,UniformBlockIndex:?GLuint,Pname:?GLenum>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),BufSize::integer()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlockName.xml">external</a> documentation.
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) ->
  wxe_util:call(5673, <<Program:?GLuint,UniformBlockIndex:?GLuint,BufSize:?GLsizei>>).

%% @spec (Program::integer(),UniformBlockIndex::integer(),UniformBlockBinding::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformBlockBinding.xml">external</a> documentation.
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) ->
  wxe_util:cast(5674, <<Program:?GLuint,UniformBlockIndex:?GLuint,UniformBlockBinding:?GLuint>>).

%% @spec (ReadTarget::enum(),WriteTarget::enum(),ReadOffset::integer(),WriteOffset::integer(),Size::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyBufferSubData.xml">external</a> documentation.
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) ->
  wxe_util:cast(5675, <<ReadTarget:?GLenum,WriteTarget:?GLenum,ReadOffset:?GLintptr,WriteOffset:?GLintptr,Size:?GLsizeiptr>>).

%% @spec () -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResizeBuffersMESA.xml">external</a> documentation.
resizeBuffersMESA() ->
  wxe_util:cast(5676, <<>>).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4dMESA.xml">external</a> documentation.
windowPos4dMESA(X,Y,Z,W) ->
  wxe_util:cast(5677, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4dMESA(X,Y,Z,W)
windowPos4dvMESA({X,Y,Z,W}) ->  windowPos4dMESA(X,Y,Z,W).

%% @spec (X::float(),Y::float(),Z::float(),W::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4fMESA.xml">external</a> documentation.
windowPos4fMESA(X,Y,Z,W) ->
  wxe_util:cast(5678, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4fMESA(X,Y,Z,W)
windowPos4fvMESA({X,Y,Z,W}) ->  windowPos4fMESA(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4iMESA.xml">external</a> documentation.
windowPos4iMESA(X,Y,Z,W) ->
  wxe_util:cast(5679, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4iMESA(X,Y,Z,W)
windowPos4ivMESA({X,Y,Z,W}) ->  windowPos4iMESA(X,Y,Z,W).

%% @spec (X::integer(),Y::integer(),Z::integer(),W::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos4sMESA.xml">external</a> documentation.
windowPos4sMESA(X,Y,Z,W) ->
  wxe_util:cast(5680, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @spec ({X,Y,Z,W}) -> ok
%% @equiv windowPos4sMESA(X,Y,Z,W)
windowPos4svMESA({X,Y,Z,W}) ->  windowPos4sMESA(X,Y,Z,W).

%% @spec (Zmin::clamp(),Zmax::clamp()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthBoundsEXT.xml">external</a> documentation.
depthBoundsEXT(Zmin,Zmax) ->
  wxe_util:cast(5681, <<Zmin:?GLclampd,Zmax:?GLclampd>>).

%% @spec (StencilTagBits::integer(),StencilClearTag::integer()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilClearTagEXT.xml">external</a> documentation.
stencilClearTagEXT(StencilTagBits,StencilClearTag) ->
  wxe_util:cast(5682, <<StencilTagBits:?GLsizei,StencilClearTag:?GLuint>>).

