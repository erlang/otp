%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

%% OPENGL API

%% This file is generated DO NOT EDIT

%% @doc  Standard OpenGL api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(gl).

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
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl
-type clamp() :: float().    %% 0.0..1.0
-type offset() :: non_neg_integer(). %% Offset in memory block
-type matrix12() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix16() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix() :: matrix12() | matrix16().
-type mem() :: binary() | tuple().   %% Memory block

-export([clearIndex/1,clearColor/4,clear/1,indexMask/1,colorMask/4,alphaFunc/2,
  blendFunc/2,logicOp/1,cullFace/1,frontFace/1,pointSize/1,lineWidth/1,
  lineStipple/2,polygonMode/2,polygonOffset/2,polygonStipple/1,getPolygonStipple/0,
  edgeFlag/1,edgeFlagv/1,scissor/4,clipPlane/2,getClipPlane/1,drawBuffer/1,
  readBuffer/1,enable/1,disable/1,isEnabled/1,enableClientState/1,disableClientState/1,
  getBooleanv/1,getDoublev/1,getFloatv/1,getIntegerv/1,pushAttrib/1,
  popAttrib/0,pushClientAttrib/1,popClientAttrib/0,renderMode/1,getError/0,
  getString/1,finish/0,flush/0,hint/2,clearDepth/1,depthFunc/1,depthMask/1,
  depthRange/2,clearAccum/4,accum/2,matrixMode/1,ortho/6,frustum/6,viewport/4,
  pushMatrix/0,popMatrix/0,loadIdentity/0,loadMatrixd/1,loadMatrixf/1,
  multMatrixd/1,multMatrixf/1,rotated/4,rotatef/4,scaled/3,scalef/3,translated/3,
  translatef/3,isList/1,deleteLists/2,genLists/1,newList/2,endList/0,
  callList/1,callLists/1,listBase/1,'begin'/1,'end'/0,vertex2d/2,vertex2f/2,
  vertex2i/2,vertex2s/2,vertex3d/3,vertex3f/3,vertex3i/3,vertex3s/3,vertex4d/4,
  vertex4f/4,vertex4i/4,vertex4s/4,vertex2dv/1,vertex2fv/1,vertex2iv/1,
  vertex2sv/1,vertex3dv/1,vertex3fv/1,vertex3iv/1,vertex3sv/1,vertex4dv/1,
  vertex4fv/1,vertex4iv/1,vertex4sv/1,normal3b/3,normal3d/3,normal3f/3,
  normal3i/3,normal3s/3,normal3bv/1,normal3dv/1,normal3fv/1,normal3iv/1,
  normal3sv/1,indexd/1,indexf/1,indexi/1,indexs/1,indexub/1,indexdv/1,
  indexfv/1,indexiv/1,indexsv/1,indexubv/1,color3b/3,color3d/3,color3f/3,
  color3i/3,color3s/3,color3ub/3,color3ui/3,color3us/3,color4b/4,color4d/4,
  color4f/4,color4i/4,color4s/4,color4ub/4,color4ui/4,color4us/4,color3bv/1,
  color3dv/1,color3fv/1,color3iv/1,color3sv/1,color3ubv/1,color3uiv/1,
  color3usv/1,color4bv/1,color4dv/1,color4fv/1,color4iv/1,color4sv/1,
  color4ubv/1,color4uiv/1,color4usv/1,texCoord1d/1,texCoord1f/1,texCoord1i/1,
  texCoord1s/1,texCoord2d/2,texCoord2f/2,texCoord2i/2,texCoord2s/2,texCoord3d/3,
  texCoord3f/3,texCoord3i/3,texCoord3s/3,texCoord4d/4,texCoord4f/4,texCoord4i/4,
  texCoord4s/4,texCoord1dv/1,texCoord1fv/1,texCoord1iv/1,texCoord1sv/1,
  texCoord2dv/1,texCoord2fv/1,texCoord2iv/1,texCoord2sv/1,texCoord3dv/1,
  texCoord3fv/1,texCoord3iv/1,texCoord3sv/1,texCoord4dv/1,texCoord4fv/1,
  texCoord4iv/1,texCoord4sv/1,rasterPos2d/2,rasterPos2f/2,rasterPos2i/2,
  rasterPos2s/2,rasterPos3d/3,rasterPos3f/3,rasterPos3i/3,rasterPos3s/3,
  rasterPos4d/4,rasterPos4f/4,rasterPos4i/4,rasterPos4s/4,rasterPos2dv/1,
  rasterPos2fv/1,rasterPos2iv/1,rasterPos2sv/1,rasterPos3dv/1,rasterPos3fv/1,
  rasterPos3iv/1,rasterPos3sv/1,rasterPos4dv/1,rasterPos4fv/1,rasterPos4iv/1,
  rasterPos4sv/1,rectd/4,rectf/4,recti/4,rects/4,rectdv/2,rectfv/2,rectiv/2,
  rectsv/2,vertexPointer/4,normalPointer/3,colorPointer/4,indexPointer/3,
  texCoordPointer/4,edgeFlagPointer/2,arrayElement/1,drawArrays/3,drawElements/4,
  interleavedArrays/3,shadeModel/1,lightf/3,lighti/3,lightfv/3,lightiv/3,
  getLightfv/2,getLightiv/2,lightModelf/2,lightModeli/2,lightModelfv/2,
  lightModeliv/2,materialf/3,materiali/3,materialfv/3,materialiv/3,getMaterialfv/2,
  getMaterialiv/2,colorMaterial/2,pixelZoom/2,pixelStoref/2,pixelStorei/2,
  pixelTransferf/2,pixelTransferi/2,pixelMapfv/3,pixelMapuiv/3,pixelMapusv/3,
  getPixelMapfv/2,getPixelMapuiv/2,getPixelMapusv/2,bitmap/7,readPixels/7,
  drawPixels/5,copyPixels/5,stencilFunc/3,stencilMask/1,stencilOp/3,
  clearStencil/1,texGend/3,texGenf/3,texGeni/3,texGendv/3,texGenfv/3,
  texGeniv/3,getTexGendv/2,getTexGenfv/2,getTexGeniv/2,texEnvf/3,texEnvi/3,
  texEnvfv/3,texEnviv/3,getTexEnvfv/2,getTexEnviv/2,texParameterf/3,
  texParameteri/3,texParameterfv/3,texParameteriv/3,getTexParameterfv/2,
  getTexParameteriv/2,getTexLevelParameterfv/3,getTexLevelParameteriv/3,
  texImage1D/8,texImage2D/9,getTexImage/5,genTextures/1,deleteTextures/1,
  bindTexture/2,prioritizeTextures/2,areTexturesResident/1,isTexture/1,
  texSubImage1D/7,texSubImage2D/9,copyTexImage1D/7,copyTexImage2D/8,
  copyTexSubImage1D/6,copyTexSubImage2D/8,map1d/6,map1f/6,map2d/10,map2f/10,
  getMapdv/3,getMapfv/3,getMapiv/3,evalCoord1d/1,evalCoord1f/1,evalCoord1dv/1,
  evalCoord1fv/1,evalCoord2d/2,evalCoord2f/2,evalCoord2dv/1,evalCoord2fv/1,
  mapGrid1d/3,mapGrid1f/3,mapGrid2d/6,mapGrid2f/6,evalPoint1/1,evalPoint2/2,
  evalMesh1/3,evalMesh2/5,fogf/2,fogi/2,fogfv/2,fogiv/2,feedbackBuffer/3,
  passThrough/1,selectBuffer/2,initNames/0,loadName/1,pushName/1,popName/0,
  blendColor/4,blendEquation/1,drawRangeElements/6,texImage3D/10,texSubImage3D/11,
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
  getVertexAttribIiv/2,getVertexAttribIuiv/2,vertexAttribI1i/2,vertexAttribI2i/3,
  vertexAttribI3i/4,vertexAttribI4i/5,vertexAttribI1ui/2,vertexAttribI2ui/3,
  vertexAttribI3ui/4,vertexAttribI4ui/5,vertexAttribI1iv/2,vertexAttribI2iv/2,
  vertexAttribI3iv/2,vertexAttribI4iv/2,vertexAttribI1uiv/2,vertexAttribI2uiv/2,
  vertexAttribI3uiv/2,vertexAttribI4uiv/2,vertexAttribI4bv/2,vertexAttribI4sv/2,
  vertexAttribI4ubv/2,vertexAttribI4usv/2,getUniformuiv/2,bindFragDataLocation/3,
  getFragDataLocation/2,uniform1ui/2,uniform2ui/3,uniform3ui/4,uniform4ui/5,
  uniform1uiv/2,uniform2uiv/2,uniform3uiv/2,uniform4uiv/2,texParameterIiv/3,
  texParameterIuiv/3,getTexParameterIiv/2,getTexParameterIuiv/2,clearBufferiv/3,
  clearBufferuiv/3,clearBufferfv/3,clearBufferfi/4,getStringi/2,drawArraysInstanced/4,
  drawElementsInstanced/5,texBuffer/3,primitiveRestartIndex/1,getInteger64i_v/2,
  getBufferParameteri64v/2,framebufferTexture/4,vertexAttribDivisor/2,
  minSampleShading/1,blendEquationi/2,blendEquationSeparatei/3,blendFunci/3,
  blendFuncSeparatei/5,loadTransposeMatrixfARB/1,loadTransposeMatrixdARB/1,
  multTransposeMatrixfARB/1,multTransposeMatrixdARB/1,weightbvARB/1,
  weightsvARB/1,weightivARB/1,weightfvARB/1,weightdvARB/1,weightubvARB/1,
  weightusvARB/1,weightuivARB/1,vertexBlendARB/1,currentPaletteMatrixARB/1,
  matrixIndexubvARB/1,matrixIndexusvARB/1,matrixIndexuivARB/1,programStringARB/3,
  bindProgramARB/2,deleteProgramsARB/1,genProgramsARB/1,programEnvParameter4dARB/6,
  programEnvParameter4dvARB/3,programEnvParameter4fARB/6,programEnvParameter4fvARB/3,
  programLocalParameter4dARB/6,programLocalParameter4dvARB/3,programLocalParameter4fARB/6,
  programLocalParameter4fvARB/3,getProgramEnvParameterdvARB/2,getProgramEnvParameterfvARB/2,
  getProgramLocalParameterdvARB/2,getProgramLocalParameterfvARB/2,
  getProgramStringARB/3,getBufferParameterivARB/2,deleteObjectARB/1,
  getHandleARB/1,detachObjectARB/2,createShaderObjectARB/1,shaderSourceARB/2,
  compileShaderARB/1,createProgramObjectARB/0,attachObjectARB/2,linkProgramARB/1,
  useProgramObjectARB/1,validateProgramARB/1,getObjectParameterfvARB/2,
  getObjectParameterivARB/2,getInfoLogARB/2,getAttachedObjectsARB/2,
  getUniformLocationARB/2,getActiveUniformARB/3,getUniformfvARB/2,
  getUniformivARB/2,getShaderSourceARB/2,bindAttribLocationARB/3,getActiveAttribARB/3,
  getAttribLocationARB/2,isRenderbuffer/1,bindRenderbuffer/2,deleteRenderbuffers/1,
  genRenderbuffers/1,renderbufferStorage/4,getRenderbufferParameteriv/2,
  isFramebuffer/1,bindFramebuffer/2,deleteFramebuffers/1,genFramebuffers/1,
  checkFramebufferStatus/1,framebufferTexture1D/5,framebufferTexture2D/5,
  framebufferTexture3D/6,framebufferRenderbuffer/4,getFramebufferAttachmentParameteriv/3,
  generateMipmap/1,blitFramebuffer/10,renderbufferStorageMultisample/5,
  framebufferTextureLayer/5,framebufferTextureFaceARB/5,flushMappedBufferRange/3,
  bindVertexArray/1,deleteVertexArrays/1,genVertexArrays/1,isVertexArray/1,
  getUniformIndices/2,getActiveUniformsiv/3,getActiveUniformName/3,
  getUniformBlockIndex/2,getActiveUniformBlockiv/4,getActiveUniformBlockName/3,
  uniformBlockBinding/3,copyBufferSubData/5,drawElementsBaseVertex/5,
  drawRangeElementsBaseVertex/7,drawElementsInstancedBaseVertex/6,
  provokingVertex/1,fenceSync/2,isSync/1,deleteSync/1,clientWaitSync/3,
  waitSync/3,getInteger64v/1,getSynciv/3,texImage2DMultisample/6,texImage3DMultisample/7,
  getMultisamplefv/2,sampleMaski/2,namedStringARB/3,deleteNamedStringARB/1,
  compileShaderIncludeARB/2,isNamedStringARB/1,getNamedStringARB/2,
  getNamedStringivARB/2,bindFragDataLocationIndexed/4,getFragDataIndex/2,
  genSamplers/1,deleteSamplers/1,isSampler/1,bindSampler/2,samplerParameteri/3,
  samplerParameteriv/3,samplerParameterf/3,samplerParameterfv/3,samplerParameterIiv/3,
  samplerParameterIuiv/3,getSamplerParameteriv/2,getSamplerParameterIiv/2,
  getSamplerParameterfv/2,getSamplerParameterIuiv/2,queryCounter/2,
  getQueryObjecti64v/2,getQueryObjectui64v/2,drawArraysIndirect/2,
  drawElementsIndirect/3,uniform1d/2,uniform2d/3,uniform3d/4,uniform4d/5,
  uniform1dv/2,uniform2dv/2,uniform3dv/2,uniform4dv/2,uniformMatrix2dv/3,
  uniformMatrix3dv/3,uniformMatrix4dv/3,uniformMatrix2x3dv/3,uniformMatrix2x4dv/3,
  uniformMatrix3x2dv/3,uniformMatrix3x4dv/3,uniformMatrix4x2dv/3,uniformMatrix4x3dv/3,
  getUniformdv/2,getSubroutineUniformLocation/3,getSubroutineIndex/3,
  getActiveSubroutineUniformName/4,getActiveSubroutineName/4,uniformSubroutinesuiv/2,
  getUniformSubroutineuiv/2,getProgramStageiv/3,patchParameteri/2,
  patchParameterfv/2,bindTransformFeedback/2,deleteTransformFeedbacks/1,
  genTransformFeedbacks/1,isTransformFeedback/1,pauseTransformFeedback/0,
  resumeTransformFeedback/0,drawTransformFeedback/2,drawTransformFeedbackStream/3,
  beginQueryIndexed/3,endQueryIndexed/2,getQueryIndexediv/3,releaseShaderCompiler/0,
  shaderBinary/3,getShaderPrecisionFormat/2,depthRangef/2,clearDepthf/1,
  getProgramBinary/2,programBinary/3,programParameteri/3,useProgramStages/3,
  activeShaderProgram/2,createShaderProgramv/2,bindProgramPipeline/1,
  deleteProgramPipelines/1,genProgramPipelines/1,isProgramPipeline/1,
  getProgramPipelineiv/2,programUniform1i/3,programUniform1iv/3,programUniform1f/3,
  programUniform1fv/3,programUniform1d/3,programUniform1dv/3,programUniform1ui/3,
  programUniform1uiv/3,programUniform2i/4,programUniform2iv/3,programUniform2f/4,
  programUniform2fv/3,programUniform2d/4,programUniform2dv/3,programUniform2ui/4,
  programUniform2uiv/3,programUniform3i/5,programUniform3iv/3,programUniform3f/5,
  programUniform3fv/3,programUniform3d/5,programUniform3dv/3,programUniform3ui/5,
  programUniform3uiv/3,programUniform4i/6,programUniform4iv/3,programUniform4f/6,
  programUniform4fv/3,programUniform4d/6,programUniform4dv/3,programUniform4ui/6,
  programUniform4uiv/3,programUniformMatrix2fv/4,programUniformMatrix3fv/4,
  programUniformMatrix4fv/4,programUniformMatrix2dv/4,programUniformMatrix3dv/4,
  programUniformMatrix4dv/4,programUniformMatrix2x3fv/4,programUniformMatrix3x2fv/4,
  programUniformMatrix2x4fv/4,programUniformMatrix4x2fv/4,programUniformMatrix3x4fv/4,
  programUniformMatrix4x3fv/4,programUniformMatrix2x3dv/4,programUniformMatrix3x2dv/4,
  programUniformMatrix2x4dv/4,programUniformMatrix4x2dv/4,programUniformMatrix3x4dv/4,
  programUniformMatrix4x3dv/4,validateProgramPipeline/1,getProgramPipelineInfoLog/2,
  vertexAttribL1d/2,vertexAttribL2d/3,vertexAttribL3d/4,vertexAttribL4d/5,
  vertexAttribL1dv/2,vertexAttribL2dv/2,vertexAttribL3dv/2,vertexAttribL4dv/2,
  vertexAttribLPointer/5,getVertexAttribLdv/2,viewportArrayv/2,viewportIndexedf/5,
  viewportIndexedfv/2,scissorArrayv/2,scissorIndexed/5,scissorIndexedv/2,
  depthRangeArrayv/2,depthRangeIndexed/3,getFloati_v/2,getDoublei_v/2,
  debugMessageControlARB/5,debugMessageInsertARB/5,getDebugMessageLogARB/2,
  getGraphicsResetStatusARB/0,drawArraysInstancedBaseInstance/5,drawElementsInstancedBaseInstance/6,
  drawElementsInstancedBaseVertexBaseInstance/7,drawTransformFeedbackInstanced/3,
  drawTransformFeedbackStreamInstanced/4,getInternalformativ/4,bindImageTexture/7,
  memoryBarrier/1,texStorage1D/4,texStorage2D/5,texStorage3D/6,depthBoundsEXT/2,
  stencilClearTagEXT/2]).

-export([call/2, cast/2, send_bin/1]).
%% @hidden
call(Op, Args) ->
    Port = get(opengl_port), 
    _ = erlang:port_control(Port,Op,Args),
    rec(Op).
    
%% @hidden
cast(Op, Args) ->
    Port = get(opengl_port), 
    _ = erlang:port_control(Port,Op,Args),
    ok.
    
%% @hidden
rec(Op) ->
    receive
        {'_egl_result_', Res} -> Res;
        {'_egl_error_',  Op, Res} -> error({error,Res,Op});
        {'_egl_error_', Other, Res} ->
                Err = io_lib:format("~p in op: ~p", [Res, Other]),
               error_logger:error_report([{gl, error}, {message, lists:flatten(Err)}]),
               rec(Op)
    end.

%% @hidden
send_bin(Bin) when is_binary(Bin) ->
    Port = get(opengl_port), 
    erlang:port_command(Port,Bin);
send_bin(Tuple) when is_tuple(Tuple) ->
    Port = get(opengl_port), 
    case element(2, Tuple) of
        Bin when is_binary(Bin) ->
            erlang:port_command(Port,Bin)
    end.


%% API

%% @doc Specify the clear value for the color index buffers
%%
%% ``gl:clearIndex'' specifies the index used by  {@link gl:clear/1}  to clear the color index
%% buffers.  `C'  is not clamped. Rather,  `C'  is converted to a fixed-point value
%% with unspecified precision to the right of the binary point. The integer part of this
%% value is then masked with   2 m-1, where   m is the number of bits in a color index stored
%% in the frame buffer. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearIndex.xml">external</a> documentation.
-spec clearIndex(C) -> 'ok' when C :: float().
clearIndex(C) ->
  cast(5037, <<C:?GLfloat>>).

%% @doc Specify clear values for the color buffers
%%
%% ``gl:clearColor'' specifies the red, green, blue, and alpha values used by  {@link gl:clear/1} 
%%  to clear the color buffers. Values specified by ``gl:clearColor'' are clamped to the
%% range  [0 1]. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearColor.xhtml">external</a> documentation.
-spec clearColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
clearColor(Red,Green,Blue,Alpha) ->
  cast(5038, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @doc Clear buffers to preset values
%%
%% ``gl:clear'' sets the bitplane area of the window to values previously selected by ``gl:clearColor''
%% , ``gl:clearDepth'', and ``gl:clearStencil''. Multiple color buffers can be cleared
%% simultaneously by selecting more than one buffer at a time using  {@link gl:drawBuffer/1} . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClear.xhtml">external</a> documentation.
-spec clear(Mask) -> 'ok' when Mask :: integer().
clear(Mask) ->
  cast(5039, <<Mask:?GLbitfield>>).

%% @doc Control the writing of individual bits in the color index buffers
%%
%% ``gl:indexMask'' controls the writing of individual bits in the color index buffers.
%% The least significant   n bits of  `Mask' , where   n is the number of bits in a color
%% index buffer, specify a mask. Where a 1 (one) appears in the mask, it's possible to write
%% to the corresponding bit in the color index buffer (or buffers). Where a 0 (zero) appears,
%% the corresponding bit is write-protected. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexMask.xml">external</a> documentation.
-spec indexMask(Mask) -> 'ok' when Mask :: integer().
indexMask(Mask) ->
  cast(5040, <<Mask:?GLuint>>).

%% @doc Enable and disable writing of frame buffer color components
%%
%% ``gl:colorMask'' and ``gl:colorMaski'' specify whether the individual color components
%% in the frame buffer can or cannot be written. ``gl:colorMaski'' sets the mask for a
%% specific draw buffer, whereas ``gl:colorMask'' sets the mask for all draw buffers. If  `Red' 
%%  is `?GL_FALSE', for example, no change is made to the red component of any pixel
%% in any of the color buffers, regardless of the drawing operation attempted. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glColorMask.xhtml">external</a> documentation.
-spec colorMask(Red, Green, Blue, Alpha) -> 'ok' when Red :: 0|1,Green :: 0|1,Blue :: 0|1,Alpha :: 0|1.
colorMask(Red,Green,Blue,Alpha) ->
  cast(5041, <<Red:?GLboolean,Green:?GLboolean,Blue:?GLboolean,Alpha:?GLboolean>>).

%% @doc Specify the alpha test function
%%
%%  The alpha test discards fragments depending on the outcome of a comparison between an
%% incoming fragment's alpha value and a constant reference value. ``gl:alphaFunc'' specifies
%% the reference value and the comparison function. The comparison is performed only if alpha
%% testing is enabled. By default, it is not enabled. (See  {@link gl:enable/1}  and  {@link gl:enable/1} 
%%  of `?GL_ALPHA_TEST'.) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAlphaFunc.xml">external</a> documentation.
-spec alphaFunc(Func, Ref) -> 'ok' when Func :: enum(),Ref :: clamp().
alphaFunc(Func,Ref) ->
  cast(5042, <<Func:?GLenum,Ref:?GLclampf>>).

%% @doc Specify pixel arithmetic
%%
%%  Pixels can be drawn using a function that blends the incoming (source) RGBA values with
%% the RGBA values that are already in the frame buffer (the destination values). Blending
%% is initially disabled. Use  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_BLEND'
%%  to enable and disable blending. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml">external</a> documentation.
-spec blendFunc(Sfactor, Dfactor) -> 'ok' when Sfactor :: enum(),Dfactor :: enum().
blendFunc(Sfactor,Dfactor) ->
  cast(5043, <<Sfactor:?GLenum,Dfactor:?GLenum>>).

%% @doc Specify a logical pixel operation for rendering
%%
%% ``gl:logicOp'' specifies a logical operation that, when enabled, is applied between
%% the incoming RGBA color and the RGBA color at the corresponding location in the frame
%% buffer. To enable or disable the logical operation, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%%  using the symbolic constant `?GL_COLOR_LOGIC_OP'. The initial value is disabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLogicOp.xhtml">external</a> documentation.
-spec logicOp(Opcode) -> 'ok' when Opcode :: enum().
logicOp(Opcode) ->
  cast(5044, <<Opcode:?GLenum>>).

%% @doc Specify whether front- or back-facing facets can be culled
%%
%% ``gl:cullFace'' specifies whether front- or back-facing facets are culled (as specified
%% by `mode') when facet culling is enabled. Facet culling is initially disabled. To
%% enable and disable facet culling, call the  {@link gl:enable/1}  and  {@link gl:enable/1} 
%% commands with the argument `?GL_CULL_FACE'. Facets include triangles, quadrilaterals,
%% polygons, and rectangles. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCullFace.xhtml">external</a> documentation.
-spec cullFace(Mode) -> 'ok' when Mode :: enum().
cullFace(Mode) ->
  cast(5045, <<Mode:?GLenum>>).

%% @doc Define front- and back-facing polygons
%%
%%  In a scene composed entirely of opaque closed surfaces, back-facing polygons are never
%% visible. Eliminating these invisible polygons has the obvious benefit of speeding up the
%% rendering of the image. To enable and disable elimination of back-facing polygons, call  {@link gl:enable/1} 
%%  and  {@link gl:enable/1}  with argument `?GL_CULL_FACE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFrontFace.xhtml">external</a> documentation.
-spec frontFace(Mode) -> 'ok' when Mode :: enum().
frontFace(Mode) ->
  cast(5046, <<Mode:?GLenum>>).

%% @doc Specify the diameter of rasterized points
%%
%% ``gl:pointSize'' specifies the rasterized diameter of points. If point size mode is
%% disabled (see  {@link gl:enable/1}  with parameter `?GL_PROGRAM_POINT_SIZE'), this value
%% will be used to rasterize points. Otherwise, the value written to the shading language
%% built-in variable gl_PointSize will be used. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointSize.xhtml">external</a> documentation.
-spec pointSize(Size) -> 'ok' when Size :: float().
pointSize(Size) ->
  cast(5047, <<Size:?GLfloat>>).

%% @doc Specify the width of rasterized lines
%%
%% ``gl:lineWidth'' specifies the rasterized width of both aliased and antialiased lines.
%% Using a line width other than 1 has different effects, depending on whether line antialiasing
%% is enabled. To enable and disable line antialiasing, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%%  with argument `?GL_LINE_SMOOTH'. Line antialiasing is initially disabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLineWidth.xhtml">external</a> documentation.
-spec lineWidth(Width) -> 'ok' when Width :: float().
lineWidth(Width) ->
  cast(5048, <<Width:?GLfloat>>).

%% @doc Specify the line stipple pattern
%%
%%  Line stippling masks out certain fragments produced by rasterization; those fragments
%% will not be drawn. The masking is achieved by using three parameters: the 16-bit line
%% stipple pattern  `Pattern' , the repeat count  `Factor' , and an integer stipple
%% counter   s. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLineStipple.xml">external</a> documentation.
-spec lineStipple(Factor, Pattern) -> 'ok' when Factor :: integer(),Pattern :: integer().
lineStipple(Factor,Pattern) ->
  cast(5049, <<Factor:?GLint,Pattern:?GLushort>>).

%% @doc Select a polygon rasterization mode
%%
%% ``gl:polygonMode'' controls the interpretation of polygons for rasterization.  `Face' 
%%  describes which polygons  `Mode'  applies to: both front and back-facing polygons (`?GL_FRONT_AND_BACK'
%% ). The polygon mode affects only the final rasterization of polygons. In particular, a
%% polygon's vertices are lit and the polygon is clipped and possibly culled before these
%% modes are applied. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonMode.xhtml">external</a> documentation.
-spec polygonMode(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
polygonMode(Face,Mode) ->
  cast(5050, <<Face:?GLenum,Mode:?GLenum>>).

%% @doc Set the scale and units used to calculate depth values
%%
%%  When `?GL_POLYGON_OFFSET_FILL', `?GL_POLYGON_OFFSET_LINE', or `?GL_POLYGON_OFFSET_POINT'
%%  is enabled, each fragment's `depth' value will be offset after it is interpolated
%% from the `depth' values of the appropriate vertices. The value of the offset is  
%% factor×DZ+r×units, where   DZ is a measurement of the change in depth relative to the
%% screen area of the polygon, and   r is the smallest value that is guaranteed to produce
%% a resolvable offset for a given implementation. The offset is added before the depth test
%% is performed and before the value is written into the depth buffer. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonOffset.xhtml">external</a> documentation.
-spec polygonOffset(Factor, Units) -> 'ok' when Factor :: float(),Units :: float().
polygonOffset(Factor,Units) ->
  cast(5051, <<Factor:?GLfloat,Units:?GLfloat>>).

%% @doc Set the polygon stippling pattern
%%
%%  Polygon stippling, like line stippling (see  {@link gl:lineStipple/2} ), masks out certain
%% fragments produced by rasterization, creating a pattern. Stippling is independent of polygon
%% antialiasing. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPolygonStipple.xml">external</a> documentation.
-spec polygonStipple(Mask) -> 'ok' when Mask :: binary().
polygonStipple(Mask) ->
  send_bin(Mask),
  cast(5052, <<>>).

%% @doc Return the polygon stipple pattern
%%
%% ``gl:getPolygonStipple'' returns to  `Pattern'  a   32×32 polygon stipple pattern.
%% The pattern is packed into memory as if  {@link gl:readPixels/7}  with both `height'
%% and `width' of 32, `type' of `?GL_BITMAP', and `format' of `?GL_COLOR_INDEX'
%%  were called, and the stipple pattern were stored in an internal   32×32 color index buffer.
%% Unlike  {@link gl:readPixels/7} , however, pixel transfer operations (shift, offset, pixel
%% map) are not applied to the returned stipple image. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPolygonStipple.xml">external</a> documentation.
-spec getPolygonStipple() -> binary().
getPolygonStipple() ->
  call(5053, <<>>).

%% @doc Flag edges as either boundary or nonboundary
%%
%%  Each vertex of a polygon, separate triangle, or separate quadrilateral specified between
%% a  {@link gl:'begin'/1} / {@link gl:'begin'/1}  pair is marked as the start of either a boundary or
%% nonboundary edge. If the current edge flag is true when the vertex is specified, the vertex
%% is marked as the start of a boundary edge. Otherwise, the vertex is marked as the start
%% of a nonboundary edge. ``gl:edgeFlag'' sets the edge flag bit to `?GL_TRUE' if  `Flag' 
%%  is `?GL_TRUE' and to `?GL_FALSE' otherwise. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlag.xml">external</a> documentation.
-spec edgeFlag(Flag) -> 'ok' when Flag :: 0|1.
edgeFlag(Flag) ->
  cast(5054, <<Flag:?GLboolean>>).

%% @equiv edgeFlag(Flag)
-spec edgeFlagv(Flag) -> 'ok' when Flag :: {Flag :: 0|1}.
edgeFlagv({Flag}) ->  edgeFlag(Flag).

%% @doc Define the scissor box
%%
%% ``gl:scissor'' defines a rectangle, called the scissor box, in window coordinates. The
%% first two arguments,  `X'  and  `Y' , specify the lower left corner of the box.  `Width' 
%%  and  `Height'  specify the width and height of the box. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissor.xhtml">external</a> documentation.
-spec scissor(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
scissor(X,Y,Width,Height) ->
  cast(5055, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Specify a plane against which all geometry is clipped
%%
%%  Geometry is always clipped against the boundaries of a six-plane frustum in `x', `y'
%% , and `z'. ``gl:clipPlane'' allows the specification of additional planes, not
%% necessarily perpendicular to the `x', `y', or `z' axis, against which all
%% geometry is clipped. To determine the maximum number of additional clipping planes, call  {@link gl:getBooleanv/1} 
%%  with argument `?GL_MAX_CLIP_PLANES'. All implementations support at least six such
%% clipping planes. Because the resulting clipping region is the intersection of the defined
%% half-spaces, it is always convex. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClipPlane.xml">external</a> documentation.
-spec clipPlane(Plane, Equation) -> 'ok' when Plane :: enum(),Equation :: {float(),float(),float(),float()}.
clipPlane(Plane,{E1,E2,E3,E4}) ->
  cast(5056, <<Plane:?GLenum,0:32,E1:?GLdouble,E2:?GLdouble,E3:?GLdouble,E4:?GLdouble>>).

%% @doc Return the coefficients of the specified clipping plane
%%
%% ``gl:getClipPlane'' returns in  `Equation'  the four coefficients of the plane equation
%% for  `Plane' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetClipPlane.xml">external</a> documentation.
-spec getClipPlane(Plane) -> {float(),float(),float(),float()} when Plane :: enum().
getClipPlane(Plane) ->
  call(5057, <<Plane:?GLenum>>).

%% @doc Specify which color buffers are to be drawn into
%%
%%  When colors are written to the frame buffer, they are written into the color buffers
%% specified by ``gl:drawBuffer''. The specifications are as follows: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffer.xhtml">external</a> documentation.
-spec drawBuffer(Mode) -> 'ok' when Mode :: enum().
drawBuffer(Mode) ->
  cast(5058, <<Mode:?GLenum>>).

%% @doc Select a color buffer source for pixels
%%
%% ``gl:readBuffer'' specifies a color buffer as the source for subsequent  {@link gl:readPixels/7} 
%% ,  {@link gl:copyTexImage1D/7} ,  {@link gl:copyTexImage2D/8} ,  {@link gl:copyTexSubImage1D/6} ,  {@link gl:copyTexSubImage2D/8} 
%% , and  {@link gl:copyTexSubImage3D/9}  commands.  `Mode'  accepts one of twelve or more
%% predefined values. In a fully configured system, `?GL_FRONT', `?GL_LEFT', and `?GL_FRONT_LEFT'
%%  all name the front left buffer, `?GL_FRONT_RIGHT' and `?GL_RIGHT' name the
%% front right buffer, and `?GL_BACK_LEFT' and `?GL_BACK' name the back left buffer.
%% Further more, the constants `?GL_COLOR_ATTACHMENT'`i' may be used to indicate
%% the `i'th color attachment where `i' ranges from zero to the value of `?GL_MAX_COLOR_ATTACHMENTS'
%%  minus one. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadBuffer.xhtml">external</a> documentation.
-spec readBuffer(Mode) -> 'ok' when Mode :: enum().
readBuffer(Mode) ->
  cast(5059, <<Mode:?GLenum>>).

%% @doc Enable or disable server-side GL capabilities
%%
%% ``gl:enable'' and  {@link gl:enable/1}  enable and disable various capabilities. Use  {@link gl:isEnabled/1} 
%%  or  {@link gl:getBooleanv/1}  to determine the current setting of any capability. The initial value
%% for each capability with the exception of `?GL_DITHER' and `?GL_MULTISAMPLE'
%% is `?GL_FALSE'. The initial value for `?GL_DITHER' and `?GL_MULTISAMPLE'
%% is `?GL_TRUE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnable.xhtml">external</a> documentation.
-spec enable(Cap) -> 'ok' when Cap :: enum().
enable(Cap) ->
  cast(5060, <<Cap:?GLenum>>).

%% @doc 
%% See {@link enable/1}
-spec disable(Cap) -> 'ok' when Cap :: enum().
disable(Cap) ->
  cast(5061, <<Cap:?GLenum>>).

%% @doc Test whether a capability is enabled
%%
%% ``gl:isEnabled'' returns `?GL_TRUE' if  `Cap'  is an enabled capability and
%% returns `?GL_FALSE' otherwise. Boolean states that are indexed may be tested with ``gl:isEnabledi''
%% . For ``gl:isEnabledi'',  `Index'  specifies the index of the capability to test.  `Index' 
%%  must be between zero and the count of indexed capabilities for  `Cap' . Initially
%% all capabilities except `?GL_DITHER' are disabled; `?GL_DITHER' is initially
%% enabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsEnabled.xhtml">external</a> documentation.
-spec isEnabled(Cap) -> 0|1 when Cap :: enum().
isEnabled(Cap) ->
  call(5062, <<Cap:?GLenum>>).

%% @doc Enable or disable client-side capability
%%
%% ``gl:enableClientState'' and  {@link gl:enableClientState/1}  enable or disable individual
%% client-side capabilities. By default, all client-side capabilities are disabled. Both ``gl:enableClientState''
%%  and  {@link gl:enableClientState/1}  take a single argument,  `Cap' , which can assume
%% one of the following values: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEnableClientState.xml">external</a> documentation.
-spec enableClientState(Cap) -> 'ok' when Cap :: enum().
enableClientState(Cap) ->
  cast(5063, <<Cap:?GLenum>>).

%% @doc 
%% See {@link enableClientState/1}
-spec disableClientState(Cap) -> 'ok' when Cap :: enum().
disableClientState(Cap) ->
  cast(5064, <<Cap:?GLenum>>).

%% @doc Return the value or values of a selected parameter
%%
%%  These four commands return values for simple state variables in GL.  `Pname'  is a
%% symbolic constant indicating the state variable to be returned, and  `Params'  is a
%% pointer to an array of the indicated type in which to place the returned data. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGet.xhtml">external</a> documentation.
-spec getBooleanv(Pname) -> [0|1] when Pname :: enum().
getBooleanv(Pname) ->
  call(5065, <<Pname:?GLenum>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getDoublev(Pname) -> [float()] when Pname :: enum().
getDoublev(Pname) ->
  call(5066, <<Pname:?GLenum>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getFloatv(Pname) -> [float()] when Pname :: enum().
getFloatv(Pname) ->
  call(5067, <<Pname:?GLenum>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getIntegerv(Pname) -> [integer()] when Pname :: enum().
getIntegerv(Pname) ->
  call(5068, <<Pname:?GLenum>>).

%% @doc Push and pop the server attribute stack
%%
%% ``gl:pushAttrib'' takes one argument, a mask that indicates which groups of state variables
%% to save on the attribute stack. Symbolic constants are used to set bits in the mask.  `Mask' 
%%  is typically constructed by specifying the bitwise-or of several  of these constants
%% together. The special mask `?GL_ALL_ATTRIB_BITS' can be used to save all stackable
%% states. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushAttrib.xml">external</a> documentation.
-spec pushAttrib(Mask) -> 'ok' when Mask :: integer().
pushAttrib(Mask) ->
  cast(5069, <<Mask:?GLbitfield>>).

%% @doc 
%% See {@link pushAttrib/1}
-spec popAttrib() -> 'ok'.
popAttrib() ->
  cast(5070, <<>>).

%% @doc Push and pop the client attribute stack
%%
%% ``gl:pushClientAttrib'' takes one argument, a mask that indicates which groups of client-state
%% variables to save on the client attribute stack. Symbolic constants are used to set bits
%% in the mask.  `Mask'  is typically constructed by specifying the bitwise-or of several
%%   of these constants together. The special mask `?GL_CLIENT_ALL_ATTRIB_BITS' can
%% be used to save all stackable client state. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushClientAttrib.xml">external</a> documentation.
-spec pushClientAttrib(Mask) -> 'ok' when Mask :: integer().
pushClientAttrib(Mask) ->
  cast(5071, <<Mask:?GLbitfield>>).

%% @doc 
%% See {@link pushClientAttrib/1}
-spec popClientAttrib() -> 'ok'.
popClientAttrib() ->
  cast(5072, <<>>).

%% @doc Set rasterization mode
%%
%% ``gl:renderMode'' sets the rasterization mode. It takes one argument,  `Mode' , which
%% can assume one of three predefined values: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRenderMode.xml">external</a> documentation.
-spec renderMode(Mode) -> integer() when Mode :: enum().
renderMode(Mode) ->
  call(5073, <<Mode:?GLenum>>).

%% @doc Return error information
%%
%% ``gl:getError'' returns the value of the error flag. Each detectable error is assigned
%% a numeric code and symbolic name. When an error occurs, the error flag is set to the appropriate
%% error code value. No other errors are recorded until ``gl:getError'' is called, the
%% error code is returned, and the flag is reset to `?GL_NO_ERROR'. If a call to ``gl:getError''
%%  returns `?GL_NO_ERROR', there has been no detectable error since the last call to ``gl:getError''
%% , or since the GL was initialized. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetError.xhtml">external</a> documentation.
-spec getError() -> enum().
getError() ->
  call(5074, <<>>).

%% @doc Return a string describing the current GL connection
%%
%% ``gl:getString'' returns a pointer to a static string describing some aspect of the
%% current GL connection.  `Name'  can be one of the following: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetString.xhtml">external</a> documentation.
-spec getString(Name) -> string() when Name :: enum().
getString(Name) ->
  call(5075, <<Name:?GLenum>>).

%% @doc Block until all GL execution is complete
%%
%% ``gl:finish'' does not return until the effects of all previously called GL commands
%% are complete. Such effects include all changes to GL state, all changes to connection
%% state, and all changes to the frame buffer contents. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFinish.xhtml">external</a> documentation.
-spec finish() -> 'ok'.
finish() ->
  cast(5076, <<>>).

%% @doc Force execution of GL commands in finite time
%%
%%  Different GL implementations buffer commands in several different locations, including
%% network buffers and the graphics accelerator itself. ``gl:flush'' empties all of these
%% buffers, causing all issued commands to be executed as quickly as they are accepted by
%% the actual rendering engine. Though this execution may not be completed in any particular
%% time period, it does complete in finite time. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlush.xhtml">external</a> documentation.
-spec flush() -> 'ok'.
flush() ->
  cast(5077, <<>>).

%% @doc Specify implementation-specific hints
%%
%%  Certain aspects of GL behavior, when there is room for interpretation, can be controlled
%% with hints. A hint is specified with two arguments.  `Target'  is a symbolic constant
%% indicating the behavior to be controlled, and  `Mode'  is another symbolic constant
%% indicating the desired behavior. The initial value for each  `Target'  is `?GL_DONT_CARE'
%% .  `Mode'  can be one of the following: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glHint.xhtml">external</a> documentation.
-spec hint(Target, Mode) -> 'ok' when Target :: enum(),Mode :: enum().
hint(Target,Mode) ->
  cast(5078, <<Target:?GLenum,Mode:?GLenum>>).

%% @doc Specify the clear value for the depth buffer
%%
%% ``gl:clearDepth'' specifies the depth value used by  {@link gl:clear/1}  to clear the depth
%% buffer. Values specified by ``gl:clearDepth'' are clamped to the range  [0 1]. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearDepth.xhtml">external</a> documentation.
-spec clearDepth(Depth) -> 'ok' when Depth :: clamp().
clearDepth(Depth) ->
  cast(5079, <<Depth:?GLclampd>>).

%% @doc Specify the value used for depth buffer comparisons
%%
%% ``gl:depthFunc'' specifies the function used to compare each incoming pixel depth value
%% with the depth value present in the depth buffer. The comparison is performed only if
%% depth testing is enabled. (See  {@link gl:enable/1}  and  {@link gl:enable/1}  of `?GL_DEPTH_TEST'
%% .) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthFunc.xhtml">external</a> documentation.
-spec depthFunc(Func) -> 'ok' when Func :: enum().
depthFunc(Func) ->
  cast(5080, <<Func:?GLenum>>).

%% @doc Enable or disable writing into the depth buffer
%%
%% ``gl:depthMask'' specifies whether the depth buffer is enabled for writing. If  `Flag' 
%%  is `?GL_FALSE', depth buffer writing is disabled. Otherwise, it is enabled. Initially,
%% depth buffer writing is enabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthMask.xhtml">external</a> documentation.
-spec depthMask(Flag) -> 'ok' when Flag :: 0|1.
depthMask(Flag) ->
  cast(5081, <<Flag:?GLboolean>>).

%% @doc Specify mapping of depth values from normalized device coordinates to window coordinates
%%
%%  After clipping and division by `w', depth coordinates range from   -1 to 1, corresponding
%% to the near and far clipping planes. ``gl:depthRange'' specifies a linear mapping of
%% the normalized depth coordinates in this range to window depth coordinates. Regardless
%% of the actual depth buffer implementation, window coordinate depth values are treated
%% as though they range from 0 through 1 (like color components). Thus, the values accepted
%% by ``gl:depthRange'' are both clamped to this range before they are accepted. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRange.xhtml">external</a> documentation.
-spec depthRange(Near_val, Far_val) -> 'ok' when Near_val :: clamp(),Far_val :: clamp().
depthRange(Near_val,Far_val) ->
  cast(5082, <<Near_val:?GLclampd,Far_val:?GLclampd>>).

%% @doc Specify clear values for the accumulation buffer
%%
%% ``gl:clearAccum'' specifies the red, green, blue, and alpha values used by  {@link gl:clear/1} 
%%  to clear the accumulation buffer. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearAccum.xml">external</a> documentation.
-spec clearAccum(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
clearAccum(Red,Green,Blue,Alpha) ->
  cast(5083, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @doc Operate on the accumulation buffer
%%
%%  The accumulation buffer is an extended-range color buffer. Images are not rendered into
%% it. Rather, images rendered into one of the color buffers are added to the contents of
%% the accumulation buffer after rendering. Effects such as antialiasing (of points, lines,
%% and polygons), motion blur, and depth of field can be created by accumulating images generated
%% with different transformation matrices. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAccum.xml">external</a> documentation.
-spec accum(Op, Value) -> 'ok' when Op :: enum(),Value :: float().
accum(Op,Value) ->
  cast(5084, <<Op:?GLenum,Value:?GLfloat>>).

%% @doc Specify which matrix is the current matrix
%%
%% ``gl:matrixMode'' sets the current matrix mode.  `Mode'  can assume one of four values:
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMatrixMode.xml">external</a> documentation.
-spec matrixMode(Mode) -> 'ok' when Mode :: enum().
matrixMode(Mode) ->
  cast(5085, <<Mode:?GLenum>>).

%% @doc Multiply the current matrix with an orthographic matrix
%%
%% ``gl:ortho'' describes a transformation that produces a parallel projection. The current
%% matrix (see  {@link gl:matrixMode/1} ) is multiplied by this matrix and the result replaces
%% the current matrix, as if  {@link gl:multMatrixd/1}  were called with the following matrix
%% as its argument: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glOrtho.xml">external</a> documentation.
-spec ortho(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
ortho(Left,Right,Bottom,Top,Near_val,Far_val) ->
  cast(5086, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,Near_val:?GLdouble,Far_val:?GLdouble>>).

%% @doc Multiply the current matrix by a perspective matrix
%%
%% ``gl:frustum'' describes a perspective matrix that produces a perspective projection.
%% The current matrix (see  {@link gl:matrixMode/1} ) is multiplied by this matrix and the result
%% replaces the current matrix, as if  {@link gl:multMatrixd/1}  were called with the following
%% matrix as its argument: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFrustum.xml">external</a> documentation.
-spec frustum(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
frustum(Left,Right,Bottom,Top,Near_val,Far_val) ->
  cast(5087, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,Near_val:?GLdouble,Far_val:?GLdouble>>).

%% @doc Set the viewport
%%
%% ``gl:viewport'' specifies the affine transformation of   x and   y from normalized device
%% coordinates to window coordinates. Let  (x nd y nd) be normalized device coordinates. Then the window
%% coordinates  (x w y w) are computed as follows: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewport.xhtml">external</a> documentation.
-spec viewport(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
viewport(X,Y,Width,Height) ->
  cast(5088, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Push and pop the current matrix stack
%%
%%  There is a stack of matrices for each of the matrix modes. In `?GL_MODELVIEW' mode,
%% the stack depth is at least 32. In the other modes, `?GL_COLOR', `?GL_PROJECTION'
%% , and `?GL_TEXTURE', the depth is at least 2. The current matrix in any mode is the
%% matrix on the top of the stack for that mode. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushMatrix.xml">external</a> documentation.
-spec pushMatrix() -> 'ok'.
pushMatrix() ->
  cast(5089, <<>>).

%% @doc 
%% See {@link pushMatrix/0}
-spec popMatrix() -> 'ok'.
popMatrix() ->
  cast(5090, <<>>).

%% @doc Replace the current matrix with the identity matrix
%%
%% ``gl:loadIdentity'' replaces the current matrix with the identity matrix. It is semantically
%% equivalent to calling  {@link gl:loadMatrixd/1}  with the identity matrix 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadIdentity.xml">external</a> documentation.
-spec loadIdentity() -> 'ok'.
loadIdentity() ->
  cast(5091, <<>>).

%% @doc Replace the current matrix with the specified matrix
%%
%% ``gl:loadMatrix'' replaces the current matrix with the one whose elements are specified
%% by  `M' . The current matrix is the projection matrix, modelview matrix, or texture
%% matrix, depending on the current matrix mode (see  {@link gl:matrixMode/1} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadMatrix.xml">external</a> documentation.
-spec loadMatrixd(M) -> 'ok' when M :: matrix().
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5092, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5092, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc 
%% See {@link loadMatrixd/1}
-spec loadMatrixf(M) -> 'ok' when M :: matrix().
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5093, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5093, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc Multiply the current matrix with the specified matrix
%%
%% ``gl:multMatrix'' multiplies the current matrix with the one specified using  `M' ,
%% and replaces the current matrix with the product. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultMatrix.xml">external</a> documentation.
-spec multMatrixd(M) -> 'ok' when M :: matrix().
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5094, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5094, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc 
%% See {@link multMatrixd/1}
-spec multMatrixf(M) -> 'ok' when M :: matrix().
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5095, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5095, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc Multiply the current matrix by a rotation matrix
%%
%% ``gl:rotate'' produces a rotation of  `Angle'  degrees around the vector  (x y z). The current
%% matrix (see  {@link gl:matrixMode/1} ) is multiplied by a rotation matrix with the product
%% replacing the current matrix, as if  {@link gl:multMatrixd/1}  were called with the following
%% matrix as its argument: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRotate.xml">external</a> documentation.
-spec rotated(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotated(Angle,X,Y,Z) ->
  cast(5096, <<Angle:?GLdouble,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link rotated/4}
-spec rotatef(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotatef(Angle,X,Y,Z) ->
  cast(5097, <<Angle:?GLfloat,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @doc Multiply the current matrix by a general scaling matrix
%%
%% ``gl:scale'' produces a nonuniform scaling along the `x', `y', and `z'
%% axes. The three parameters indicate the desired scale factor along each of the three axes.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glScale.xml">external</a> documentation.
-spec scaled(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scaled(X,Y,Z) ->
  cast(5098, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link scaled/3}
-spec scalef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scalef(X,Y,Z) ->
  cast(5099, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @doc Multiply the current matrix by a translation matrix
%%
%% ``gl:translate'' produces a translation by (x y z). The current matrix (see  {@link gl:matrixMode/1} 
%% ) is multiplied by this translation matrix, with the product replacing the current matrix,
%% as if  {@link gl:multMatrixd/1}  were called with the following matrix for its argument: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTranslate.xml">external</a> documentation.
-spec translated(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translated(X,Y,Z) ->
  cast(5100, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link translated/3}
-spec translatef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translatef(X,Y,Z) ->
  cast(5101, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @doc Determine if a name corresponds to a display list
%%
%% ``gl:isList'' returns `?GL_TRUE' if  `List'  is the name of a display list and
%% returns `?GL_FALSE' if it is not, or if an error occurs. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIsList.xml">external</a> documentation.
-spec isList(List) -> 0|1 when List :: integer().
isList(List) ->
  call(5102, <<List:?GLuint>>).

%% @doc Delete a contiguous group of display lists
%%
%% ``gl:deleteLists'' causes a contiguous group of display lists to be deleted.  `List' 
%% is the name of the first display list to be deleted, and  `Range'  is the number of
%% display lists to delete. All display lists   d with   list&lt;= d&lt;= list+range-1 are
%% deleted. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDeleteLists.xml">external</a> documentation.
-spec deleteLists(List, Range) -> 'ok' when List :: integer(),Range :: integer().
deleteLists(List,Range) ->
  cast(5103, <<List:?GLuint,Range:?GLsizei>>).

%% @doc Generate a contiguous set of empty display lists
%%
%% ``gl:genLists'' has one argument,  `Range' . It returns an integer `n' such
%% that  `Range'  contiguous empty display lists, named   n,   n+1,   ...,   n+range-1,
%% are created. If  `Range'  is 0, if there is no group of  `Range'  contiguous names
%% available, or if any error is generated, no display lists are generated, and 0 is returned.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenLists.xml">external</a> documentation.
-spec genLists(Range) -> integer() when Range :: integer().
genLists(Range) ->
  call(5104, <<Range:?GLsizei>>).

%% @doc Create or replace a display list
%%
%%  Display lists are groups of GL commands that have been stored for subsequent execution.
%% Display lists are created with ``gl:newList''. All subsequent commands are placed in
%% the display list, in the order issued, until  {@link gl:endList/0}  is called. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNewList.xml">external</a> documentation.
-spec newList(List, Mode) -> 'ok' when List :: integer(),Mode :: enum().
newList(List,Mode) ->
  cast(5105, <<List:?GLuint,Mode:?GLenum>>).

%% @doc glBeginList
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec endList() -> 'ok'.
endList() ->
  cast(5106, <<>>).

%% @doc Execute a display list
%%
%% ``gl:callList'' causes the named display list to be executed. The commands saved in
%% the display list are executed in order, just as if they were called without using a display
%% list. If  `List'  has not been defined as a display list, ``gl:callList'' is ignored.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallList.xml">external</a> documentation.
-spec callList(List) -> 'ok' when List :: integer().
callList(List) ->
  cast(5107, <<List:?GLuint>>).

%% @doc Execute a list of display lists
%%
%% ``gl:callLists'' causes each display list in the list of names passed as  `Lists' 
%% to be executed. As a result, the commands saved in each display list are executed in order,
%% just as if they were called without using a display list. Names of display lists that
%% have not been defined are ignored. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallLists.xml">external</a> documentation.
-spec callLists(Lists) -> 'ok' when Lists :: [integer()].
callLists(Lists) ->
  ListsLen = length(Lists),
  cast(5108, <<ListsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Lists>>)/binary,0:(((1+ListsLen) rem 2)*32)>>).

%% @doc set the display-list base for 
%%
%%  {@link gl:callLists/1} 
%%
%%  {@link gl:callLists/1}  specifies an array of offsets. Display-list names are generated
%% by adding  `Base'  to each offset. Names that reference valid display lists are executed;
%% the others are ignored. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glListBase.xml">external</a> documentation.
-spec listBase(Base) -> 'ok' when Base :: integer().
listBase(Base) ->
  cast(5109, <<Base:?GLuint>>).

%% @doc Delimit the vertices of a primitive or a group of like primitives
%%
%% ``gl:'begin''' and  {@link gl:'begin'/1}  delimit the vertices that define a primitive or a group
%% of like primitives. ``gl:'begin''' accepts a single argument that specifies in which of
%% ten ways the vertices are interpreted. Taking   n as an integer count starting at one,
%% and   N as the total number of vertices specified, the interpretations are as follows: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBegin.xml">external</a> documentation.
-spec 'begin'(Mode) -> 'ok' when Mode :: enum().
'begin'(Mode) ->
  cast(5110, <<Mode:?GLenum>>).

%% @doc 
%% See {@link 'begin'/1}
-spec 'end'() -> 'ok'.
'end'() ->
  cast(5111, <<>>).

%% @doc Specify a vertex
%%
%% ``gl:vertex'' commands are used within  {@link gl:'begin'/1} / {@link gl:'begin'/1}  pairs to specify
%% point, line, and polygon vertices. The current color, normal, texture coordinates, and
%% fog coordinate are associated with the vertex when ``gl:vertex'' is called. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertex.xml">external</a> documentation.
-spec vertex2d(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2d(X,Y) ->
  cast(5112, <<X:?GLdouble,Y:?GLdouble>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex2f(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2f(X,Y) ->
  cast(5113, <<X:?GLfloat,Y:?GLfloat>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2i(X,Y) ->
  cast(5114, <<X:?GLint,Y:?GLint>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2s(X,Y) ->
  cast(5115, <<X:?GLshort,Y:?GLshort>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3d(X,Y,Z) ->
  cast(5116, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3f(X,Y,Z) ->
  cast(5117, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3i(X,Y,Z) ->
  cast(5118, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3s(X,Y,Z) ->
  cast(5119, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4d(X,Y,Z,W) ->
  cast(5120, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4f(X,Y,Z,W) ->
  cast(5121, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4i(X,Y,Z,W) ->
  cast(5122, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @doc 
%% See {@link vertex2d/2}
-spec vertex4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4s(X,Y,Z,W) ->
  cast(5123, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @equiv vertex2d(X,Y)
-spec vertex2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertex2dv({X,Y}) ->  vertex2d(X,Y).

%% @equiv vertex2f(X,Y)
-spec vertex2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertex2fv({X,Y}) ->  vertex2f(X,Y).

%% @equiv vertex2i(X,Y)
-spec vertex2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertex2iv({X,Y}) ->  vertex2i(X,Y).

%% @equiv vertex2s(X,Y)
-spec vertex2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertex2sv({X,Y}) ->  vertex2s(X,Y).

%% @equiv vertex3d(X,Y,Z)
-spec vertex3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).

%% @equiv vertex3f(X,Y,Z)
-spec vertex3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).

%% @equiv vertex3i(X,Y,Z)
-spec vertex3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).

%% @equiv vertex3s(X,Y,Z)
-spec vertex3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).

%% @equiv vertex4d(X,Y,Z,W)
-spec vertex4dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).

%% @equiv vertex4f(X,Y,Z,W)
-spec vertex4fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).

%% @equiv vertex4i(X,Y,Z,W)
-spec vertex4iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).

%% @equiv vertex4s(X,Y,Z,W)
-spec vertex4sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).

%% @doc Set the current normal vector
%%
%%  The current normal is set to the given coordinates whenever ``gl:normal'' is issued.
%% Byte, short, or integer arguments are converted to floating-point format with a linear
%% mapping that maps the most positive representable integer value to 1.0 and the most negative
%% representable integer value to   -1.0. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormal.xml">external</a> documentation.
-spec normal3b(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3b(Nx,Ny,Nz) ->
  cast(5124, <<Nx:?GLbyte,Ny:?GLbyte,Nz:?GLbyte>>).

%% @doc 
%% See {@link normal3b/3}
-spec normal3d(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3d(Nx,Ny,Nz) ->
  cast(5125, <<Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble>>).

%% @doc 
%% See {@link normal3b/3}
-spec normal3f(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3f(Nx,Ny,Nz) ->
  cast(5126, <<Nx:?GLfloat,Ny:?GLfloat,Nz:?GLfloat>>).

%% @doc 
%% See {@link normal3b/3}
-spec normal3i(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3i(Nx,Ny,Nz) ->
  cast(5127, <<Nx:?GLint,Ny:?GLint,Nz:?GLint>>).

%% @doc 
%% See {@link normal3b/3}
-spec normal3s(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3s(Nx,Ny,Nz) ->
  cast(5128, <<Nx:?GLshort,Ny:?GLshort,Nz:?GLshort>>).

%% @equiv normal3b(Nx,Ny,Nz)
-spec normal3bv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).

%% @equiv normal3d(Nx,Ny,Nz)
-spec normal3dv(V) -> 'ok' when V :: {Nx :: float(),Ny :: float(),Nz :: float()}.
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).

%% @equiv normal3f(Nx,Ny,Nz)
-spec normal3fv(V) -> 'ok' when V :: {Nx :: float(),Ny :: float(),Nz :: float()}.
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).

%% @equiv normal3i(Nx,Ny,Nz)
-spec normal3iv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).

%% @equiv normal3s(Nx,Ny,Nz)
-spec normal3sv(V) -> 'ok' when V :: {Nx :: integer(),Ny :: integer(),Nz :: integer()}.
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).

%% @doc Set the current color index
%%
%% ``gl:index'' updates the current (single-valued) color index. It takes one argument,
%% the new value for the current color index. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndex.xml">external</a> documentation.
-spec indexd(C) -> 'ok' when C :: float().
indexd(C) ->
  cast(5129, <<C:?GLdouble>>).

%% @doc 
%% See {@link indexd/1}
-spec indexf(C) -> 'ok' when C :: float().
indexf(C) ->
  cast(5130, <<C:?GLfloat>>).

%% @doc 
%% See {@link indexd/1}
-spec indexi(C) -> 'ok' when C :: integer().
indexi(C) ->
  cast(5131, <<C:?GLint>>).

%% @doc 
%% See {@link indexd/1}
-spec indexs(C) -> 'ok' when C :: integer().
indexs(C) ->
  cast(5132, <<C:?GLshort>>).

%% @doc 
%% See {@link indexd/1}
-spec indexub(C) -> 'ok' when C :: integer().
indexub(C) ->
  cast(5133, <<C:?GLubyte>>).

%% @equiv indexd(C)
-spec indexdv(C) -> 'ok' when C :: {C :: float()}.
indexdv({C}) ->  indexd(C).

%% @equiv indexf(C)
-spec indexfv(C) -> 'ok' when C :: {C :: float()}.
indexfv({C}) ->  indexf(C).

%% @equiv indexi(C)
-spec indexiv(C) -> 'ok' when C :: {C :: integer()}.
indexiv({C}) ->  indexi(C).

%% @equiv indexs(C)
-spec indexsv(C) -> 'ok' when C :: {C :: integer()}.
indexsv({C}) ->  indexs(C).

%% @equiv indexub(C)
-spec indexubv(C) -> 'ok' when C :: {C :: integer()}.
indexubv({C}) ->  indexub(C).

%% @doc Set the current color
%%
%%  The GL stores both a current single-valued color index and a current four-valued RGBA
%% color. ``gl:color'' sets a new four-valued RGBA color. ``gl:color'' has two major
%% variants: ``gl:color3'' and ``gl:color4''. ``gl:color3'' variants specify new red,
%% green, and blue values explicitly and set the current alpha value to 1.0 (full intensity)
%% implicitly. ``gl:color4'' variants specify all four color components explicitly. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColor.xml">external</a> documentation.
-spec color3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3b(Red,Green,Blue) ->
  cast(5134, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @doc 
%% See {@link color3b/3}
-spec color3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3d(Red,Green,Blue) ->
  cast(5135, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @doc 
%% See {@link color3b/3}
-spec color3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3f(Red,Green,Blue) ->
  cast(5136, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @doc 
%% See {@link color3b/3}
-spec color3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3i(Red,Green,Blue) ->
  cast(5137, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @doc 
%% See {@link color3b/3}
-spec color3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3s(Red,Green,Blue) ->
  cast(5138, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @doc 
%% See {@link color3b/3}
-spec color3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ub(Red,Green,Blue) ->
  cast(5139, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @doc 
%% See {@link color3b/3}
-spec color3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ui(Red,Green,Blue) ->
  cast(5140, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @doc 
%% See {@link color3b/3}
-spec color3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3us(Red,Green,Blue) ->
  cast(5141, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @doc 
%% See {@link color3b/3}
-spec color4b(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4b(Red,Green,Blue,Alpha) ->
  cast(5142, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte,Alpha:?GLbyte>>).

%% @doc 
%% See {@link color3b/3}
-spec color4d(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4d(Red,Green,Blue,Alpha) ->
  cast(5143, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble,Alpha:?GLdouble>>).

%% @doc 
%% See {@link color3b/3}
-spec color4f(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4f(Red,Green,Blue,Alpha) ->
  cast(5144, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat,Alpha:?GLfloat>>).

%% @doc 
%% See {@link color3b/3}
-spec color4i(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4i(Red,Green,Blue,Alpha) ->
  cast(5145, <<Red:?GLint,Green:?GLint,Blue:?GLint,Alpha:?GLint>>).

%% @doc 
%% See {@link color3b/3}
-spec color4s(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4s(Red,Green,Blue,Alpha) ->
  cast(5146, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort,Alpha:?GLshort>>).

%% @doc 
%% See {@link color3b/3}
-spec color4ub(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ub(Red,Green,Blue,Alpha) ->
  cast(5147, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte,Alpha:?GLubyte>>).

%% @doc 
%% See {@link color3b/3}
-spec color4ui(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ui(Red,Green,Blue,Alpha) ->
  cast(5148, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint,Alpha:?GLuint>>).

%% @doc 
%% See {@link color3b/3}
-spec color4us(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4us(Red,Green,Blue,Alpha) ->
  cast(5149, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort,Alpha:?GLushort>>).

%% @equiv color3b(Red,Green,Blue)
-spec color3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).

%% @equiv color3d(Red,Green,Blue)
-spec color3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).

%% @equiv color3f(Red,Green,Blue)
-spec color3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).

%% @equiv color3i(Red,Green,Blue)
-spec color3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).

%% @equiv color3s(Red,Green,Blue)
-spec color3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).

%% @equiv color3ub(Red,Green,Blue)
-spec color3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).

%% @equiv color3ui(Red,Green,Blue)
-spec color3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).

%% @equiv color3us(Red,Green,Blue)
-spec color3usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).

%% @equiv color4b(Red,Green,Blue,Alpha)
-spec color4bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).

%% @equiv color4d(Red,Green,Blue,Alpha)
-spec color4dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float(),Alpha :: float()}.
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).

%% @equiv color4f(Red,Green,Blue,Alpha)
-spec color4fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float(),Alpha :: float()}.
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).

%% @equiv color4i(Red,Green,Blue,Alpha)
-spec color4iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).

%% @equiv color4s(Red,Green,Blue,Alpha)
-spec color4sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).

%% @equiv color4ub(Red,Green,Blue,Alpha)
-spec color4ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).

%% @equiv color4ui(Red,Green,Blue,Alpha)
-spec color4uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).

%% @equiv color4us(Red,Green,Blue,Alpha)
-spec color4usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer()}.
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).

%% @doc Set the current texture coordinates
%%
%% ``gl:texCoord'' specifies texture coordinates in one, two, three, or four dimensions. ``gl:texCoord1''
%%  sets the current texture coordinates to (s 0 0 1); a call to ``gl:texCoord2'' sets them to (s t
%% 0 1).
%% Similarly, ``gl:texCoord3'' specifies the texture coordinates as (s t r 1), and ``gl:texCoord4''
%%  defines all four components explicitly as (s t r q). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoord.xml">external</a> documentation.
-spec texCoord1d(S) -> 'ok' when S :: float().
texCoord1d(S) ->
  cast(5150, <<S:?GLdouble>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord1f(S) -> 'ok' when S :: float().
texCoord1f(S) ->
  cast(5151, <<S:?GLfloat>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord1i(S) -> 'ok' when S :: integer().
texCoord1i(S) ->
  cast(5152, <<S:?GLint>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord1s(S) -> 'ok' when S :: integer().
texCoord1s(S) ->
  cast(5153, <<S:?GLshort>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord2d(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2d(S,T) ->
  cast(5154, <<S:?GLdouble,T:?GLdouble>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord2f(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2f(S,T) ->
  cast(5155, <<S:?GLfloat,T:?GLfloat>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord2i(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2i(S,T) ->
  cast(5156, <<S:?GLint,T:?GLint>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord2s(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2s(S,T) ->
  cast(5157, <<S:?GLshort,T:?GLshort>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord3d(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3d(S,T,R) ->
  cast(5158, <<S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord3f(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3f(S,T,R) ->
  cast(5159, <<S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord3i(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3i(S,T,R) ->
  cast(5160, <<S:?GLint,T:?GLint,R:?GLint>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord3s(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3s(S,T,R) ->
  cast(5161, <<S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord4d(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4d(S,T,R,Q) ->
  cast(5162, <<S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord4f(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4f(S,T,R,Q) ->
  cast(5163, <<S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord4i(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4i(S,T,R,Q) ->
  cast(5164, <<S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @doc 
%% See {@link texCoord1d/1}
-spec texCoord4s(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4s(S,T,R,Q) ->
  cast(5165, <<S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @equiv texCoord1d(S)
-spec texCoord1dv(V) -> 'ok' when V :: {S :: float()}.
texCoord1dv({S}) ->  texCoord1d(S).

%% @equiv texCoord1f(S)
-spec texCoord1fv(V) -> 'ok' when V :: {S :: float()}.
texCoord1fv({S}) ->  texCoord1f(S).

%% @equiv texCoord1i(S)
-spec texCoord1iv(V) -> 'ok' when V :: {S :: integer()}.
texCoord1iv({S}) ->  texCoord1i(S).

%% @equiv texCoord1s(S)
-spec texCoord1sv(V) -> 'ok' when V :: {S :: integer()}.
texCoord1sv({S}) ->  texCoord1s(S).

%% @equiv texCoord2d(S,T)
-spec texCoord2dv(V) -> 'ok' when V :: {S :: float(),T :: float()}.
texCoord2dv({S,T}) ->  texCoord2d(S,T).

%% @equiv texCoord2f(S,T)
-spec texCoord2fv(V) -> 'ok' when V :: {S :: float(),T :: float()}.
texCoord2fv({S,T}) ->  texCoord2f(S,T).

%% @equiv texCoord2i(S,T)
-spec texCoord2iv(V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
texCoord2iv({S,T}) ->  texCoord2i(S,T).

%% @equiv texCoord2s(S,T)
-spec texCoord2sv(V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
texCoord2sv({S,T}) ->  texCoord2s(S,T).

%% @equiv texCoord3d(S,T,R)
-spec texCoord3dv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).

%% @equiv texCoord3f(S,T,R)
-spec texCoord3fv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).

%% @equiv texCoord3i(S,T,R)
-spec texCoord3iv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).

%% @equiv texCoord3s(S,T,R)
-spec texCoord3sv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).

%% @equiv texCoord4d(S,T,R,Q)
-spec texCoord4dv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).

%% @equiv texCoord4f(S,T,R,Q)
-spec texCoord4fv(V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).

%% @equiv texCoord4i(S,T,R,Q)
-spec texCoord4iv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).

%% @equiv texCoord4s(S,T,R,Q)
-spec texCoord4sv(V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).

%% @doc Specify the raster position for pixel operations
%%
%%  The GL maintains a 3D position in window coordinates. This position, called the raster
%% position, is used to position pixel and bitmap write operations. It is maintained with
%% subpixel accuracy. See  {@link gl:bitmap/7} ,  {@link gl:drawPixels/5} , and  {@link gl:copyPixels/5} 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRasterPos.xml">external</a> documentation.
-spec rasterPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2d(X,Y) ->
  cast(5166, <<X:?GLdouble,Y:?GLdouble>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2f(X,Y) ->
  cast(5167, <<X:?GLfloat,Y:?GLfloat>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2i(X,Y) ->
  cast(5168, <<X:?GLint,Y:?GLint>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2s(X,Y) ->
  cast(5169, <<X:?GLshort,Y:?GLshort>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3d(X,Y,Z) ->
  cast(5170, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3f(X,Y,Z) ->
  cast(5171, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3i(X,Y,Z) ->
  cast(5172, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3s(X,Y,Z) ->
  cast(5173, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4d(X,Y,Z,W) ->
  cast(5174, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4f(X,Y,Z,W) ->
  cast(5175, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4i(X,Y,Z,W) ->
  cast(5176, <<X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @doc 
%% See {@link rasterPos2d/2}
-spec rasterPos4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4s(X,Y,Z,W) ->
  cast(5177, <<X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @equiv rasterPos2d(X,Y)
-spec rasterPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).

%% @equiv rasterPos2f(X,Y)
-spec rasterPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).

%% @equiv rasterPos2i(X,Y)
-spec rasterPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).

%% @equiv rasterPos2s(X,Y)
-spec rasterPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).

%% @equiv rasterPos3d(X,Y,Z)
-spec rasterPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).

%% @equiv rasterPos3f(X,Y,Z)
-spec rasterPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).

%% @equiv rasterPos3i(X,Y,Z)
-spec rasterPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).

%% @equiv rasterPos3s(X,Y,Z)
-spec rasterPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).

%% @equiv rasterPos4d(X,Y,Z,W)
-spec rasterPos4dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).

%% @equiv rasterPos4f(X,Y,Z,W)
-spec rasterPos4fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).

%% @equiv rasterPos4i(X,Y,Z,W)
-spec rasterPos4iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).

%% @equiv rasterPos4s(X,Y,Z,W)
-spec rasterPos4sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).

%% @doc Draw a rectangle
%%
%% ``gl:rect'' supports efficient specification of rectangles as two corner points. Each
%% rectangle command takes four arguments, organized either as two consecutive pairs of  (x y)
%% coordinates or as two pointers to arrays, each containing an  (x y) pair. The resulting rectangle
%% is defined in the   z=0 plane. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRect.xml">external</a> documentation.
-spec rectd(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectd(X1,Y1,X2,Y2) ->
  cast(5178, <<X1:?GLdouble,Y1:?GLdouble,X2:?GLdouble,Y2:?GLdouble>>).

%% @doc 
%% See {@link rectd/4}
-spec rectf(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectf(X1,Y1,X2,Y2) ->
  cast(5179, <<X1:?GLfloat,Y1:?GLfloat,X2:?GLfloat,Y2:?GLfloat>>).

%% @doc 
%% See {@link rectd/4}
-spec recti(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
recti(X1,Y1,X2,Y2) ->
  cast(5180, <<X1:?GLint,Y1:?GLint,X2:?GLint,Y2:?GLint>>).

%% @doc 
%% See {@link rectd/4}
-spec rects(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
rects(X1,Y1,X2,Y2) ->
  cast(5181, <<X1:?GLshort,Y1:?GLshort,X2:?GLshort,Y2:?GLshort>>).

%% @doc 
%% See {@link rectd/4}
-spec rectdv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectdv({V1,V2},{V1,V2}) ->
  cast(5182, <<V1:?GLdouble,V2:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @doc 
%% See {@link rectd/4}
-spec rectfv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectfv({V1,V2},{V1,V2}) ->
  cast(5183, <<V1:?GLfloat,V2:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @doc 
%% See {@link rectd/4}
-spec rectiv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectiv({V1,V2},{V1,V2}) ->
  cast(5184, <<V1:?GLint,V2:?GLint,V1:?GLint,V2:?GLint>>).

%% @doc 
%% See {@link rectd/4}
-spec rectsv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectsv({V1,V2},{V1,V2}) ->
  cast(5185, <<V1:?GLshort,V2:?GLshort,V1:?GLshort,V2:?GLshort>>).

%% @doc Define an array of vertex data
%%
%% ``gl:vertexPointer'' specifies the location and data format of an array of vertex coordinates
%% to use when rendering.  `Size'  specifies the number of coordinates per vertex, and
%% must be 2, 3, or 4.  `Type'  specifies the data type of each coordinate, and  `Stride' 
%%  specifies the byte stride from one vertex to the next, allowing vertices and attributes
%% to be packed into a single array or stored in separate arrays. (Single-array storage may
%% be more efficient on some implementations; see  {@link gl:interleavedArrays/3} .) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertexPointer.xml">external</a> documentation.
-spec vertexPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
vertexPointer(Size,Type,Stride,Ptr) when  is_integer(Ptr) ->
  cast(5186, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Ptr:?GLuint>>);
vertexPointer(Size,Type,Stride,Ptr) ->
  send_bin(Ptr),
  cast(5187, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc Define an array of normals
%%
%% ``gl:normalPointer'' specifies the location and data format of an array of normals to
%% use when rendering.  `Type'  specifies the data type of each normal coordinate, and  `Stride' 
%%  specifies the byte stride from one normal to the next, allowing vertices and attributes
%% to be packed into a single array or stored in separate arrays. (Single-array storage may
%% be more efficient on some implementations; see  {@link gl:interleavedArrays/3} .) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormalPointer.xml">external</a> documentation.
-spec normalPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
normalPointer(Type,Stride,Ptr) when  is_integer(Ptr) ->
  cast(5188, <<Type:?GLenum,Stride:?GLsizei,Ptr:?GLuint>>);
normalPointer(Type,Stride,Ptr) ->
  send_bin(Ptr),
  cast(5189, <<Type:?GLenum,Stride:?GLsizei>>).

%% @doc Define an array of colors
%%
%% ``gl:colorPointer'' specifies the location and data format of an array of color components
%% to use when rendering.  `Size'  specifies the number of components per color, and must
%% be 3 or 4.  `Type'  specifies the data type of each color component, and  `Stride' 
%% specifies the byte stride from one color to the next, allowing vertices and attributes
%% to be packed into a single array or stored in separate arrays. (Single-array storage may
%% be more efficient on some implementations; see  {@link gl:interleavedArrays/3} .) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorPointer.xml">external</a> documentation.
-spec colorPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
colorPointer(Size,Type,Stride,Ptr) when  is_integer(Ptr) ->
  cast(5190, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Ptr:?GLuint>>);
colorPointer(Size,Type,Stride,Ptr) ->
  send_bin(Ptr),
  cast(5191, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc Define an array of color indexes
%%
%% ``gl:indexPointer'' specifies the location and data format of an array of color indexes
%% to use when rendering.  `Type'  specifies the data type of each color index and  `Stride' 
%%  specifies the byte stride from one color index to the next, allowing vertices and attributes
%% to be packed into a single array or stored in separate arrays. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexPointer.xml">external</a> documentation.
-spec indexPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
indexPointer(Type,Stride,Ptr) when  is_integer(Ptr) ->
  cast(5192, <<Type:?GLenum,Stride:?GLsizei,Ptr:?GLuint>>);
indexPointer(Type,Stride,Ptr) ->
  send_bin(Ptr),
  cast(5193, <<Type:?GLenum,Stride:?GLsizei>>).

%% @doc Define an array of texture coordinates
%%
%% ``gl:texCoordPointer'' specifies the location and data format of an array of texture
%% coordinates to use when rendering.   `Size'  specifies the number of coordinates per
%% texture coordinate set, and must be 1, 2, 3, or 4.  `Type'  specifies the data type
%% of each texture coordinate, and  `Stride'  specifies the byte stride from one texture
%% coordinate set to the next, allowing vertices and attributes to be packed into a single
%% array or stored in separate arrays. (Single-array storage may be more efficient on some
%% implementations; see  {@link gl:interleavedArrays/3} .) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoordPointer.xml">external</a> documentation.
-spec texCoordPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
texCoordPointer(Size,Type,Stride,Ptr) when  is_integer(Ptr) ->
  cast(5194, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Ptr:?GLuint>>);
texCoordPointer(Size,Type,Stride,Ptr) ->
  send_bin(Ptr),
  cast(5195, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc Define an array of edge flags
%%
%% ``gl:edgeFlagPointer'' specifies the location and data format of an array of boolean
%% edge flags to use when rendering.  `Stride'  specifies the byte stride from one edge
%% flag to the next, allowing vertices and attributes to be packed into a single array or
%% stored in separate arrays. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlagPointer.xml">external</a> documentation.
-spec edgeFlagPointer(Stride, Ptr) -> 'ok' when Stride :: integer(),Ptr :: offset()|mem().
edgeFlagPointer(Stride,Ptr) when  is_integer(Ptr) ->
  cast(5196, <<Stride:?GLsizei,Ptr:?GLuint>>);
edgeFlagPointer(Stride,Ptr) ->
  send_bin(Ptr),
  cast(5197, <<Stride:?GLsizei>>).

%% @doc Render a vertex using the specified vertex array element
%%
%% ``gl:arrayElement'' commands are used within  {@link gl:'begin'/1} / {@link gl:'begin'/1}  pairs
%% to specify vertex and attribute data for point, line, and polygon primitives. If `?GL_VERTEX_ARRAY'
%%  is enabled when ``gl:arrayElement'' is called, a single vertex is drawn, using vertex
%% and attribute data taken from location  `I'  of the enabled arrays. If `?GL_VERTEX_ARRAY'
%%  is not enabled, no drawing occurs but the attributes corresponding to the enabled arrays
%% are modified. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glArrayElement.xml">external</a> documentation.
-spec arrayElement(I) -> 'ok' when I :: integer().
arrayElement(I) ->
  cast(5198, <<I:?GLint>>).

%% @doc Render primitives from array data
%%
%% ``gl:drawArrays'' specifies multiple geometric primitives with very few subroutine calls.
%% Instead of calling a GL procedure to pass each individual vertex, normal, texture coordinate,
%% edge flag, or color, you can prespecify separate arrays of vertices, normals, and colors
%% and use them to construct a sequence of primitives with a single call to ``gl:drawArrays''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml">external</a> documentation.
-spec drawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer().
drawArrays(Mode,First,Count) ->
  cast(5199, <<Mode:?GLenum,First:?GLint,Count:?GLsizei>>).

%% @doc Render primitives from array data
%%
%% ``gl:drawElements'' specifies multiple geometric primitives with very few subroutine
%% calls. Instead of calling a GL function to pass each individual vertex, normal, texture
%% coordinate, edge flag, or color, you can prespecify separate arrays of vertices, normals,
%% and so on, and use them to construct a sequence of primitives with a single call to ``gl:drawElements''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElements.xhtml">external</a> documentation.
-spec drawElements(Mode, Count, Type, Indices) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawElements(Mode,Count,Type,Indices) when  is_integer(Indices) ->
  cast(5200, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawElements(Mode,Count,Type,Indices) ->
  send_bin(Indices),
  cast(5201, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum>>).

%% @doc Simultaneously specify and enable several interleaved arrays
%%
%% ``gl:interleavedArrays'' lets you specify and enable individual color, normal, texture
%% and vertex arrays whose elements are part of a larger aggregate array element. For some
%% implementations, this is more efficient than specifying the arrays separately. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInterleavedArrays.xml">external</a> documentation.
-spec interleavedArrays(Format, Stride, Pointer) -> 'ok' when Format :: enum(),Stride :: integer(),Pointer :: offset()|mem().
interleavedArrays(Format,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5202, <<Format:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
interleavedArrays(Format,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5203, <<Format:?GLenum,Stride:?GLsizei>>).

%% @doc Select flat or smooth shading
%%
%%  GL primitives can have either flat or smooth shading. Smooth shading, the default, causes
%% the computed colors of vertices to be interpolated as the primitive is rasterized, typically
%% assigning different colors to each resulting pixel fragment. Flat shading selects the
%% computed color of just one vertex and assigns it to all the pixel fragments generated
%% by rasterizing a single primitive. In either case, the computed color of a vertex is the
%% result of lighting if lighting is enabled, or it is the current color at the time the
%% vertex was specified if lighting is disabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glShadeModel.xml">external</a> documentation.
-spec shadeModel(Mode) -> 'ok' when Mode :: enum().
shadeModel(Mode) ->
  cast(5204, <<Mode:?GLenum>>).

%% @doc Set light source parameters
%%
%% ``gl:light'' sets the values of individual light source parameters.  `Light'  names
%% the light and is a symbolic name of the form `?GL_LIGHT' i, where i ranges from 0
%% to the value of `?GL_MAX_LIGHTS' - 1.  `Pname'  specifies one of ten light source
%% parameters, again by symbolic name.  `Params'  is either a single value or a pointer
%% to an array that contains the new values. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLight.xml">external</a> documentation.
-spec lightf(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: float().
lightf(Light,Pname,Param) ->
  cast(5205, <<Light:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link lightf/3}
-spec lighti(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: integer().
lighti(Light,Pname,Param) ->
  cast(5206, <<Light:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link lightf/3}
-spec lightfv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightfv(Light,Pname,Params) ->
  cast(5207, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link lightf/3}
-spec lightiv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightiv(Light,Pname,Params) ->
  cast(5208, <<Light:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc Return light source parameter values
%%
%% ``gl:getLight'' returns in  `Params'  the value or values of a light source parameter.
%%  `Light'  names the light and is a symbolic name of the form `?GL_LIGHT' i where
%% i ranges from 0 to the value of `?GL_MAX_LIGHTS' - 1. `?GL_MAX_LIGHTS' is an
%% implementation dependent constant that is greater than or equal to eight.  `Pname' 
%% specifies one of ten light source parameters, again by symbolic name. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetLight.xml">external</a> documentation.
-spec getLightfv(Light, Pname) -> {float(),float(),float(),float()} when Light :: enum(),Pname :: enum().
getLightfv(Light,Pname) ->
  call(5209, <<Light:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getLightfv/2}
-spec getLightiv(Light, Pname) -> {integer(),integer(),integer(),integer()} when Light :: enum(),Pname :: enum().
getLightiv(Light,Pname) ->
  call(5210, <<Light:?GLenum,Pname:?GLenum>>).

%% @doc Set the lighting model parameters
%%
%% ``gl:lightModel'' sets the lighting model parameter.  `Pname'  names a parameter
%% and  `Params'  gives the new value. There are three lighting model parameters: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLightModel.xml">external</a> documentation.
-spec lightModelf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
lightModelf(Pname,Param) ->
  cast(5211, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link lightModelf/2}
-spec lightModeli(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
lightModeli(Pname,Param) ->
  cast(5212, <<Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link lightModelf/2}
-spec lightModelfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModelfv(Pname,Params) ->
  cast(5213, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link lightModelf/2}
-spec lightModeliv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModeliv(Pname,Params) ->
  cast(5214, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc Specify material parameters for the lighting model
%%
%% ``gl:material'' assigns values to material parameters. There are two matched sets of
%% material parameters. One, the `front-facing' set, is used to shade points, lines,
%% bitmaps, and all polygons (when two-sided lighting is disabled), or just front-facing
%% polygons (when two-sided lighting is enabled). The other set, `back-facing', is used
%% to shade back-facing polygons only when two-sided lighting is enabled. Refer to the  {@link gl:lightModelf/2} 
%%  reference page for details concerning one- and two-sided lighting calculations. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMaterial.xml">external</a> documentation.
-spec materialf(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: float().
materialf(Face,Pname,Param) ->
  cast(5215, <<Face:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link materialf/3}
-spec materiali(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: integer().
materiali(Face,Pname,Param) ->
  cast(5216, <<Face:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link materialf/3}
-spec materialfv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialfv(Face,Pname,Params) ->
  cast(5217, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link materialf/3}
-spec materialiv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialiv(Face,Pname,Params) ->
  cast(5218, <<Face:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc Return material parameters
%%
%% ``gl:getMaterial'' returns in  `Params'  the value or values of parameter  `Pname' 
%%  of material  `Face' . Six parameters are defined: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMaterial.xml">external</a> documentation.
-spec getMaterialfv(Face, Pname) -> {float(),float(),float(),float()} when Face :: enum(),Pname :: enum().
getMaterialfv(Face,Pname) ->
  call(5219, <<Face:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getMaterialfv/2}
-spec getMaterialiv(Face, Pname) -> {integer(),integer(),integer(),integer()} when Face :: enum(),Pname :: enum().
getMaterialiv(Face,Pname) ->
  call(5220, <<Face:?GLenum,Pname:?GLenum>>).

%% @doc Cause a material color to track the current color
%%
%% ``gl:colorMaterial'' specifies which material parameters track the current color. When `?GL_COLOR_MATERIAL'
%%  is enabled, the material parameter or parameters specified by  `Mode' , of the material
%% or materials specified by  `Face' , track the current color at all times. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorMaterial.xml">external</a> documentation.
-spec colorMaterial(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
colorMaterial(Face,Mode) ->
  cast(5221, <<Face:?GLenum,Mode:?GLenum>>).

%% @doc Specify the pixel zoom factors
%%
%% ``gl:pixelZoom'' specifies values for the   x and   y zoom factors. During the execution
%% of  {@link gl:drawPixels/5}  or  {@link gl:copyPixels/5} , if  ( xr,   yr) is the current raster
%% position, and a given element is in the   mth row and   nth column of the pixel rectangle,
%% then pixels whose centers are in the rectangle with corners at 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelZoom.xml">external</a> documentation.
-spec pixelZoom(Xfactor, Yfactor) -> 'ok' when Xfactor :: float(),Yfactor :: float().
pixelZoom(Xfactor,Yfactor) ->
  cast(5222, <<Xfactor:?GLfloat,Yfactor:?GLfloat>>).

%% @doc Set pixel storage modes
%%
%% ``gl:pixelStore'' sets pixel storage modes that affect the operation of subsequent  {@link gl:readPixels/7} 
%%  as well as the unpacking of texture patterns (see  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} 
%% ,  {@link gl:texImage3D/10} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} 
%% ),  {@link gl:compressedTexImage1D/7} ,  {@link gl:compressedTexImage2D/8} ,  {@link gl:compressedTexImage3D/9} 
%% ,  {@link gl:compressedTexSubImage1D/7} ,  {@link gl:compressedTexSubImage2D/9}  or  {@link gl:compressedTexSubImage1D/7} 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPixelStore.xhtml">external</a> documentation.
-spec pixelStoref(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelStoref(Pname,Param) ->
  cast(5223, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link pixelStoref/2}
-spec pixelStorei(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelStorei(Pname,Param) ->
  cast(5224, <<Pname:?GLenum,Param:?GLint>>).

%% @doc Set pixel transfer modes
%%
%% ``gl:pixelTransfer'' sets pixel transfer modes that affect the operation of subsequent  {@link gl:copyPixels/5} 
%% ,  {@link gl:copyTexImage1D/7} ,  {@link gl:copyTexImage2D/8} ,  {@link gl:copyTexSubImage1D/6} ,  {@link gl:copyTexSubImage2D/8} 
%% ,  {@link gl:copyTexSubImage3D/9} ,  {@link gl:drawPixels/5} ,  {@link gl:readPixels/7} ,  {@link gl:texImage1D/8} 
%% ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} 
%% , and  {@link gl:texSubImage1D/7}  commands. Additionally, if the ARB_imaging subset is supported,
%% the routines  {@link gl:colorTable/6} ,  {@link gl:colorSubTable/6} ,  {@link gl:convolutionFilter1D/6} 
%% ,  {@link gl:convolutionFilter2D/7} ,  {@link gl:histogram/4} ,  {@link gl:minmax/3} , and  {@link gl:separableFilter2D/8} 
%%  are also affected. The algorithms that are specified by pixel transfer modes operate
%% on pixels after they are read from the frame buffer ( {@link gl:copyPixels/5}  {@link gl:copyTexImage1D/7} 
%% ,  {@link gl:copyTexImage2D/8} ,  {@link gl:copyTexSubImage1D/6} ,  {@link gl:copyTexSubImage2D/8} ,
%%  {@link gl:copyTexSubImage3D/9} , and  {@link gl:readPixels/7} ), or unpacked from client memory
%% ( {@link gl:drawPixels/5} ,  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} 
%% ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} , and  {@link gl:texSubImage1D/7} ).
%% Pixel transfer operations happen in the same order, and in the same manner, regardless
%% of the command that resulted in the pixel operation. Pixel storage modes (see  {@link gl:pixelStoref/2} 
%% ) control the unpacking of pixels being read from client memory and the packing of pixels
%% being written back into client memory. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelTransfer.xml">external</a> documentation.
-spec pixelTransferf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelTransferf(Pname,Param) ->
  cast(5225, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link pixelTransferf/2}
-spec pixelTransferi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelTransferi(Pname,Param) ->
  cast(5226, <<Pname:?GLenum,Param:?GLint>>).

%% @doc Set up pixel transfer maps
%%
%% ``gl:pixelMap'' sets up translation tables, or `maps', used by  {@link gl:copyPixels/5} 
%% ,  {@link gl:copyTexImage1D/7} ,  {@link gl:copyTexImage2D/8} ,  {@link gl:copyTexSubImage1D/6} ,  {@link gl:copyTexSubImage2D/8} 
%% ,  {@link gl:copyTexSubImage3D/9} ,  {@link gl:drawPixels/5} ,  {@link gl:readPixels/7} ,  {@link gl:texImage1D/8} 
%% ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} 
%% , and  {@link gl:texSubImage1D/7} . Additionally, if the ARB_imaging subset is supported,
%% the routines  {@link gl:colorTable/6} ,  {@link gl:colorSubTable/6} ,  {@link gl:convolutionFilter1D/6} 
%% ,  {@link gl:convolutionFilter2D/7} ,  {@link gl:histogram/4} ,  {@link gl:minmax/3} , and  {@link gl:separableFilter2D/8} 
%% . Use of these maps is described completely in the  {@link gl:pixelTransferf/2}  reference
%% page, and partly in the reference pages for the pixel and texture image commands. Only
%% the specification of the maps is described in this reference page. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelMap.xml">external</a> documentation.
-spec pixelMapfv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapfv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5227, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @doc 
%% See {@link pixelMapfv/3}
-spec pixelMapuiv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapuiv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5228, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @doc 
%% See {@link pixelMapfv/3}
-spec pixelMapusv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapusv(Map,Mapsize,Values) ->
  send_bin(Values),
  cast(5229, <<Map:?GLenum,Mapsize:?GLsizei>>).

%% @doc Return the specified pixel map
%%
%%  See the  {@link gl:pixelMapfv/3}  reference page for a description of the acceptable values
%% for the  `Map'  parameter. ``gl:getPixelMap'' returns in  `Data'  the contents
%% of the pixel map specified in  `Map' . Pixel maps are used during the execution of  {@link gl:readPixels/7} 
%% ,  {@link gl:drawPixels/5} ,  {@link gl:copyPixels/5} ,  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} 
%% ,  {@link gl:texImage3D/10} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} 
%% ,  {@link gl:copyTexImage1D/7} ,  {@link gl:copyTexImage2D/8} ,  {@link gl:copyTexSubImage1D/6} ,  {@link gl:copyTexSubImage2D/8} 
%% , and  {@link gl:copyTexSubImage3D/9} . to map color indices, stencil indices, color components,
%% and depth components to other values. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPixelMap.xml">external</a> documentation.
-spec getPixelMapfv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapfv(Map,Values) ->
  send_bin(Values),
  call(5230, <<Map:?GLenum>>).

%% @doc 
%% See {@link getPixelMapfv/2}
-spec getPixelMapuiv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapuiv(Map,Values) ->
  send_bin(Values),
  call(5231, <<Map:?GLenum>>).

%% @doc 
%% See {@link getPixelMapfv/2}
-spec getPixelMapusv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapusv(Map,Values) ->
  send_bin(Values),
  call(5232, <<Map:?GLenum>>).

%% @doc Draw a bitmap
%%
%%  A bitmap is a binary image. When drawn, the bitmap is positioned relative to the current
%% raster position, and frame buffer pixels corresponding to 1's in the bitmap are written
%% using the current raster color or index. Frame buffer pixels corresponding to 0's in the
%% bitmap are not modified. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBitmap.xml">external</a> documentation.
-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 'ok' when Width :: integer(),Height :: integer(),Xorig :: float(),Yorig :: float(),Xmove :: float(),Ymove :: float(),Bitmap :: offset()|mem().
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when  is_integer(Bitmap) ->
  cast(5233, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat,Bitmap:?GLuint>>);
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) ->
  send_bin(Bitmap),
  cast(5234, <<Width:?GLsizei,Height:?GLsizei,Xorig:?GLfloat,Yorig:?GLfloat,Xmove:?GLfloat,Ymove:?GLfloat>>).

%% @doc Read a block of pixels from the frame buffer
%%
%% ``gl:readPixels'' returns pixel data from the frame buffer, starting with the pixel
%% whose lower left corner is at location ( `X' ,  `Y' ), into client memory starting
%% at location  `Data' . Several parameters control the processing of the pixel data before
%% it is placed into client memory. These parameters are set with  {@link gl:pixelStoref/2} .
%% This reference page describes the effects on ``gl:readPixels'' of most, but not all
%% of the parameters specified by these three commands. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml">external</a> documentation.
-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
readPixels(X,Y,Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  call(5235, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Write a block of pixels to the frame buffer
%%
%% ``gl:drawPixels'' reads pixel data from memory and writes it into the frame buffer relative
%% to the current raster position, provided that the raster position is valid. Use  {@link gl:rasterPos2d/2} 
%%  or  {@link gl:windowPos2d/2}  to set the current raster position; use  {@link gl:getBooleanv/1}  with
%% argument `?GL_CURRENT_RASTER_POSITION_VALID' to determine if the specified raster
%% position is valid, and  {@link gl:getBooleanv/1}  with argument `?GL_CURRENT_RASTER_POSITION'
%% to query the raster position. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDrawPixels.xml">external</a> documentation.
-spec drawPixels(Width, Height, Format, Type, Pixels) -> 'ok' when Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
drawPixels(Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5236, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
drawPixels(Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5237, <<Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Copy pixels in the frame buffer
%%
%% ``gl:copyPixels'' copies a screen-aligned rectangle of pixels from the specified frame
%% buffer location to a region relative to the current raster position. Its operation is
%% well defined only if the entire pixel source region is within the exposed portion of the
%% window. Results of copies from outside the window, or from regions of the window that
%% are not exposed, are hardware dependent and undefined. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyPixels.xml">external</a> documentation.
-spec copyPixels(X, Y, Width, Height, Type) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Type :: enum().
copyPixels(X,Y,Width,Height,Type) ->
  cast(5238, <<X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Type:?GLenum>>).

%% @doc Set front and back function and reference value for stencil testing
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% Stencil planes are first drawn into using GL drawing primitives, then geometry and images
%% are rendered using the stencil planes to mask out portions of the screen. Stenciling is
%% typically used in multipass rendering algorithms to achieve special effects, such as decals,
%% outlining, and constructive solid geometry rendering. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFunc.xhtml">external</a> documentation.
-spec stencilFunc(Func, Ref, Mask) -> 'ok' when Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFunc(Func,Ref,Mask) ->
  cast(5239, <<Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @doc Control the front and back writing of individual bits in the stencil planes
%%
%% ``gl:stencilMask'' controls the writing of individual bits in the stencil planes. The
%% least significant   n bits of  `Mask' , where   n is the number of bits in the stencil
%% buffer, specify a mask. Where a 1 appears in the mask, it's possible to write to the corresponding
%% bit in the stencil buffer. Where a 0 appears, the corresponding bit is write-protected.
%% Initially, all bits are enabled for writing. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMask.xhtml">external</a> documentation.
-spec stencilMask(Mask) -> 'ok' when Mask :: integer().
stencilMask(Mask) ->
  cast(5240, <<Mask:?GLuint>>).

%% @doc Set front and back stencil test actions
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% You draw into the stencil planes using GL drawing primitives, then render geometry and
%% images, using the stencil planes to mask out portions of the screen. Stenciling is typically
%% used in multipass rendering algorithms to achieve special effects, such as decals, outlining,
%% and constructive solid geometry rendering. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOp.xhtml">external</a> documentation.
-spec stencilOp(Fail, Zfail, Zpass) -> 'ok' when Fail :: enum(),Zfail :: enum(),Zpass :: enum().
stencilOp(Fail,Zfail,Zpass) ->
  cast(5241, <<Fail:?GLenum,Zfail:?GLenum,Zpass:?GLenum>>).

%% @doc Specify the clear value for the stencil buffer
%%
%% ``gl:clearStencil'' specifies the index used by  {@link gl:clear/1}  to clear the stencil
%% buffer.  `S'  is masked with   2 m-1, where   m is the number of bits in the stencil
%% buffer. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearStencil.xhtml">external</a> documentation.
-spec clearStencil(S) -> 'ok' when S :: integer().
clearStencil(S) ->
  cast(5242, <<S:?GLint>>).

%% @doc Control the generation of texture coordinates
%%
%% ``gl:texGen'' selects a texture-coordinate generation function or supplies coefficients
%% for one of the functions.  `Coord'  names one of the (`s', `t', `r', `q'
%% ) texture coordinates; it must be one of the symbols `?GL_S', `?GL_T', `?GL_R'
%% , or `?GL_Q'.  `Pname'  must be one of three symbolic constants: `?GL_TEXTURE_GEN_MODE'
%% , `?GL_OBJECT_PLANE', or `?GL_EYE_PLANE'. If  `Pname'  is `?GL_TEXTURE_GEN_MODE'
%% , then  `Params'  chooses a mode, one of `?GL_OBJECT_LINEAR', `?GL_EYE_LINEAR'
%% , `?GL_SPHERE_MAP', `?GL_NORMAL_MAP', or `?GL_REFLECTION_MAP'. If  `Pname' 
%%  is either `?GL_OBJECT_PLANE' or `?GL_EYE_PLANE',  `Params'  contains coefficients
%% for the corresponding texture generation function. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexGen.xml">external</a> documentation.
-spec texGend(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGend(Coord,Pname,Param) ->
  cast(5243, <<Coord:?GLenum,Pname:?GLenum,Param:?GLdouble>>).

%% @doc 
%% See {@link texGend/3}
-spec texGenf(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGenf(Coord,Pname,Param) ->
  cast(5244, <<Coord:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link texGend/3}
-spec texGeni(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: integer().
texGeni(Coord,Pname,Param) ->
  cast(5245, <<Coord:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link texGend/3}
-spec texGendv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGendv(Coord,Pname,Params) ->
  cast(5246, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,0:32,
      (<< <<C:?GLdouble>> ||C <- tuple_to_list(Params)>>)/binary>>).

%% @doc 
%% See {@link texGend/3}
-spec texGenfv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGenfv(Coord,Pname,Params) ->
  cast(5247, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link texGend/3}
-spec texGeniv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGeniv(Coord,Pname,Params) ->
  cast(5248, <<Coord:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc Return texture coordinate generation parameters
%%
%% ``gl:getTexGen'' returns in  `Params'  selected parameters of a texture coordinate
%% generation function that was specified using  {@link gl:texGend/3} .  `Coord'  names one
%% of the (`s', `t', `r', `q') texture coordinates, using the symbolic
%% constant `?GL_S', `?GL_T', `?GL_R', or `?GL_Q'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexGen.xml">external</a> documentation.
-spec getTexGendv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGendv(Coord,Pname) ->
  call(5249, <<Coord:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getTexGendv/2}
-spec getTexGenfv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGenfv(Coord,Pname) ->
  call(5250, <<Coord:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getTexGendv/2}
-spec getTexGeniv(Coord, Pname) -> {integer(),integer(),integer(),integer()} when Coord :: enum(),Pname :: enum().
getTexGeniv(Coord,Pname) ->
  call(5251, <<Coord:?GLenum,Pname:?GLenum>>).

%% @doc glTexEnvf
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnvf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texEnvf(Target,Pname,Param) ->
  cast(5252, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc glTexEnvi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texEnvi(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texEnvi(Target,Pname,Param) ->
  cast(5253, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @doc Set texture environment parameters
%%
%%  A texture environment specifies how texture values are interpreted when a fragment is
%% textured. When  `Target'  is `?GL_TEXTURE_FILTER_CONTROL',  `Pname'  must be `?GL_TEXTURE_LOD_BIAS'
%% . When  `Target'  is `?GL_TEXTURE_ENV',  `Pname'  can be `?GL_TEXTURE_ENV_MODE'
%% , `?GL_TEXTURE_ENV_COLOR', `?GL_COMBINE_RGB', `?GL_COMBINE_ALPHA', `?GL_RGB_SCALE'
%% , `?GL_ALPHA_SCALE', `?GL_SRC0_RGB', `?GL_SRC1_RGB', `?GL_SRC2_RGB', `?GL_SRC0_ALPHA'
%% , `?GL_SRC1_ALPHA', or `?GL_SRC2_ALPHA'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexEnv.xml">external</a> documentation.
-spec texEnvfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnvfv(Target,Pname,Params) ->
  cast(5254, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link texEnvfv/3}
-spec texEnviv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnviv(Target,Pname,Params) ->
  cast(5255, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc Return texture environment parameters
%%
%% ``gl:getTexEnv'' returns in  `Params'  selected values of a texture environment that
%% was specified with  {@link gl:texEnvfv/3} .  `Target'  specifies a texture environment. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexEnv.xml">external</a> documentation.
-spec getTexEnvfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexEnvfv(Target,Pname) ->
  call(5256, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getTexEnvfv/2}
-spec getTexEnviv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexEnviv(Target,Pname) ->
  call(5257, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Set texture parameters
%%
%% ``gl:texParameter'' assigns the value or values in  `Params'  to the texture parameter
%% specified as  `Pname' .  `Target'  defines the target texture, either `?GL_TEXTURE_1D'
%% , `?GL_TEXTURE_2D', `?GL_TEXTURE_1D_ARRAY', `?GL_TEXTURE_2D_ARRAY', `?GL_TEXTURE_RECTANGLE'
%% , or `?GL_TEXTURE_3D'. The following symbols are accepted in  `Pname' : 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml">external</a> documentation.
-spec texParameterf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texParameterf(Target,Pname,Param) ->
  cast(5258, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link texParameterf/3}
-spec texParameteri(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texParameteri(Target,Pname,Param) ->
  cast(5259, <<Target:?GLenum,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link texParameterf/3}
-spec texParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterfv(Target,Pname,Params) ->
  cast(5260, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link texParameterf/3}
-spec texParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameteriv(Target,Pname,Params) ->
  cast(5261, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc Return texture parameter values
%%
%% ``gl:getTexParameter'' returns in  `Params'  the value or values of the texture parameter
%% specified as  `Pname' .  `Target'  defines the target texture. `?GL_TEXTURE_1D',
%% `?GL_TEXTURE_2D', `?GL_TEXTURE_3D', `?GL_TEXTURE_1D_ARRAY', `?GL_TEXTURE_2D_ARRAY'
%% , `?GL_TEXTURE_RECTANGLE', `?GL_TEXTURE_CUBE_MAP', `?GL_TEXTURE_CUBE_MAP_ARRAY'
%%  specify one-, two-, or three-dimensional, one-dimensional array, two-dimensional array,
%% rectangle, cube-mapped or cube-mapped array texturing, respectively.  `Pname'  accepts
%% the same symbols as  {@link gl:texParameterf/3} , with the same interpretations: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexParameter.xhtml">external</a> documentation.
-spec getTexParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexParameterfv(Target,Pname) ->
  call(5262, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getTexParameterfv/2}
-spec getTexParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameteriv(Target,Pname) ->
  call(5263, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Return texture parameter values for a specific level of detail
%%
%% ``gl:getTexLevelParameter'' returns in  `Params'  texture parameter values for a
%% specific level-of-detail value, specified as  `Level' .  `Target'  defines the target
%% texture, either `?GL_TEXTURE_1D', `?GL_TEXTURE_2D', `?GL_TEXTURE_3D', `?GL_PROXY_TEXTURE_1D'
%% , `?GL_PROXY_TEXTURE_2D', `?GL_PROXY_TEXTURE_3D', `?GL_TEXTURE_CUBE_MAP_POSITIVE_X'
%% , `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Y', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y'
%% , `?GL_TEXTURE_CUBE_MAP_POSITIVE_Z', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z', or `?GL_PROXY_TEXTURE_CUBE_MAP'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexLevelParameter.xhtml">external</a> documentation.
-spec getTexLevelParameterfv(Target, Level, Pname) -> {float()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameterfv(Target,Level,Pname) ->
  call(5264, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @doc 
%% See {@link getTexLevelParameterfv/3}
-spec getTexLevelParameteriv(Target, Level, Pname) -> {integer()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameteriv(Target,Level,Pname) ->
  call(5265, <<Target:?GLenum,Level:?GLint,Pname:?GLenum>>).

%% @doc Specify a one-dimensional texture image
%%
%%  Texturing maps a portion of a specified texture image onto each graphical primitive for
%% which texturing is enabled. To enable and disable one-dimensional texturing, call  {@link gl:enable/1} 
%%  and  {@link gl:enable/1}  with argument `?GL_TEXTURE_1D'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml">external</a> documentation.
-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5266, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5267, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @doc Specify a two-dimensional texture image
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml">external</a> documentation.
-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5268, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5269, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @doc Return a texture image
%%
%% ``gl:getTexImage'' returns a texture image into  `Img' .  `Target'  specifies
%% whether the desired texture image is one specified by  {@link gl:texImage1D/8}  (`?GL_TEXTURE_1D'
%% ),  {@link gl:texImage2D/9}  (`?GL_TEXTURE_1D_ARRAY', `?GL_TEXTURE_RECTANGLE', `?GL_TEXTURE_2D'
%%  or any of `?GL_TEXTURE_CUBE_MAP_*'), or  {@link gl:texImage3D/10}  (`?GL_TEXTURE_2D_ARRAY'
%% , `?GL_TEXTURE_3D').  `Level'  specifies the level-of-detail number of the desired
%% image.  `Format'  and  `Type'  specify the format and type of the desired image
%% array. See the reference page for  {@link gl:texImage1D/8}  for a description of the acceptable
%% values for the  `Format'  and  `Type'  parameters, respectively. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexImage.xhtml">external</a> documentation.
-spec getTexImage(Target, Level, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
getTexImage(Target,Level,Format,Type,Pixels) ->
  send_bin(Pixels),
  call(5270, <<Target:?GLenum,Level:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @doc Generate texture names
%%
%% ``gl:genTextures'' returns  `N'  texture names in  `Textures' . There is no guarantee
%% that the names form a contiguous set of integers; however, it is guaranteed that none
%% of the returned names was in use immediately before the call to ``gl:genTextures''. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTextures.xhtml">external</a> documentation.
-spec genTextures(N) -> [integer()] when N :: integer().
genTextures(N) ->
  call(5271, <<N:?GLsizei>>).

%% @doc Delete named textures
%%
%% ``gl:deleteTextures'' deletes  `N'  textures named by the elements of the array  `Textures' 
%% . After a texture is deleted, it has no contents or dimensionality, and its name is free
%% for reuse (for example by  {@link gl:genTextures/1} ). If a texture that is currently bound
%% is deleted, the binding reverts to 0 (the default texture). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTextures.xhtml">external</a> documentation.
-spec deleteTextures(Textures) -> 'ok' when Textures :: [integer()].
deleteTextures(Textures) ->
  TexturesLen = length(Textures),
  cast(5272, <<TexturesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+TexturesLen) rem 2)*32)>>).

%% @doc Bind a named texture to a texturing target
%%
%% ``gl:bindTexture'' lets you create or use a named texture. Calling ``gl:bindTexture''
%% with  `Target'  set to `?GL_TEXTURE_1D', `?GL_TEXTURE_2D', `?GL_TEXTURE_3D'
%% , or `?GL_TEXTURE_1D_ARRAY', `?GL_TEXTURE_2D_ARRAY', `?GL_TEXTURE_RECTANGLE'
%% , `?GL_TEXTURE_CUBE_MAP', `?GL_TEXTURE_2D_MULTISAMPLE' or `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY'
%%  and  `Texture'  set to the name of the new texture binds the texture name to the target.
%% When a texture is bound to a target, the previous binding for that target is automatically
%% broken. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTexture.xhtml">external</a> documentation.
-spec bindTexture(Target, Texture) -> 'ok' when Target :: enum(),Texture :: integer().
bindTexture(Target,Texture) ->
  cast(5273, <<Target:?GLenum,Texture:?GLuint>>).

%% @doc Set texture residence priority
%%
%% ``gl:prioritizeTextures'' assigns the  `N'  texture priorities given in  `Priorities' 
%%  to the  `N'  textures named in  `Textures' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPrioritizeTextures.xml">external</a> documentation.
-spec prioritizeTextures(Textures, Priorities) -> 'ok' when Textures :: [integer()],Priorities :: [clamp()].
prioritizeTextures(Textures,Priorities) ->
  TexturesLen = length(Textures),
  PrioritiesLen = length(Priorities),
  cast(5274, <<TexturesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+TexturesLen) rem 2)*32),PrioritiesLen:?GLuint,
        (<< <<C:?GLclampf>> || C <- Priorities>>)/binary,0:(((1+PrioritiesLen) rem 2)*32)>>).

%% @doc Determine if textures are loaded in texture memory
%%
%%  GL establishes a ``working set'' of textures that are resident in texture memory. These
%% textures can be bound to a texture target much more efficiently than textures that are
%% not resident. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAreTexturesResident.xml">external</a> documentation.
-spec areTexturesResident(Textures) -> {0|1,Residences :: [0|1]} when Textures :: [integer()].
areTexturesResident(Textures) ->
  TexturesLen = length(Textures),
  call(5275, <<TexturesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+TexturesLen) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a texture
%%
%% ``gl:isTexture'' returns `?GL_TRUE' if  `Texture'  is currently the name of
%% a texture. If  `Texture'  is zero, or is a non-zero value that is not currently the
%% name of a texture, or if an error occurs, ``gl:isTexture'' returns `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTexture.xhtml">external</a> documentation.
-spec isTexture(Texture) -> 0|1 when Texture :: integer().
isTexture(Texture) ->
  call(5276, <<Texture:?GLuint>>).

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5277, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5278, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5279, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5280, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Copy pixels into a 1D texture image
%%
%% ``gl:copyTexImage1D'' defines a one-dimensional texture image with pixels from the current
%% `?GL_READ_BUFFER'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage1D.xhtml">external</a> documentation.
-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Border :: integer().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) ->
  cast(5281, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Border:?GLint>>).

%% @doc Copy pixels into a 2D texture image
%%
%% ``gl:copyTexImage2D'' defines a two-dimensional texture image, or cube-map texture image
%% with pixels from the current `?GL_READ_BUFFER'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage2D.xhtml">external</a> documentation.
-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Border :: integer().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) ->
  cast(5282, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint>>).

%% @doc Copy a one-dimensional texture subimage
%%
%% ``gl:copyTexSubImage1D'' replaces a portion of a one-dimensional texture image with
%% pixels from the current `?GL_READ_BUFFER' (rather than from main memory, as is the
%% case for  {@link gl:texSubImage1D/7} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage1D.xhtml">external</a> documentation.
-spec copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) ->
  cast(5283, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Copy a two-dimensional texture subimage
%%
%% ``gl:copyTexSubImage2D'' replaces a rectangular portion of a two-dimensional texture
%% image or cube-map texture image with pixels from the current `?GL_READ_BUFFER' (rather
%% than from main memory, as is the case for  {@link gl:texSubImage1D/7} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage2D.xhtml">external</a> documentation.
-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) ->
  cast(5284, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map1d(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1d(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5285, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Stride:?GLint,Order:?GLint>>).

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map1f(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1f(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5286, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Stride:?GLint,Order:?GLint>>).

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  send_bin(Points),
  cast(5287, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Ustride:?GLint,Uorder:?GLint,V1:?GLdouble,V2:?GLdouble,Vstride:?GLint,Vorder:?GLint>>).

%% @doc glMap
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  send_bin(Points),
  cast(5288, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Ustride:?GLint,Uorder:?GLint,V1:?GLfloat,V2:?GLfloat,Vstride:?GLint,Vorder:?GLint>>).

%% @doc Return evaluator parameters
%%
%%  {@link gl:map1d/6}  and  {@link gl:map1d/6}  define evaluators. ``gl:getMap'' returns evaluator
%% parameters.  `Target'  chooses a map,  `Query'  selects a specific parameter, and  `V' 
%%  points to storage where the values will be returned. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMap.xml">external</a> documentation.
-spec getMapdv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapdv(Target,Query,V) ->
  send_bin(V),
  call(5289, <<Target:?GLenum,Query:?GLenum>>).

%% @doc 
%% See {@link getMapdv/3}
-spec getMapfv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapfv(Target,Query,V) ->
  send_bin(V),
  call(5290, <<Target:?GLenum,Query:?GLenum>>).

%% @doc 
%% See {@link getMapdv/3}
-spec getMapiv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapiv(Target,Query,V) ->
  send_bin(V),
  call(5291, <<Target:?GLenum,Query:?GLenum>>).

%% @doc Evaluate enabled one- and two-dimensional maps
%%
%% ``gl:evalCoord1'' evaluates enabled one-dimensional maps at argument  `U' . ``gl:evalCoord2''
%%  does the same for two-dimensional maps using two domain values,  `U'  and  `V' .
%% To define a map, call  {@link gl:map1d/6}  and  {@link gl:map1d/6} ; to enable and disable it,
%% call  {@link gl:enable/1}  and  {@link gl:enable/1} . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalCoord.xml">external</a> documentation.
-spec evalCoord1d(U) -> 'ok' when U :: float().
evalCoord1d(U) ->
  cast(5292, <<U:?GLdouble>>).

%% @doc 
%% See {@link evalCoord1d/1}
-spec evalCoord1f(U) -> 'ok' when U :: float().
evalCoord1f(U) ->
  cast(5293, <<U:?GLfloat>>).

%% @equiv evalCoord1d(U)
-spec evalCoord1dv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1dv({U}) ->  evalCoord1d(U).

%% @equiv evalCoord1f(U)
-spec evalCoord1fv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1fv({U}) ->  evalCoord1f(U).

%% @doc 
%% See {@link evalCoord1d/1}
-spec evalCoord2d(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2d(U,V) ->
  cast(5294, <<U:?GLdouble,V:?GLdouble>>).

%% @doc 
%% See {@link evalCoord1d/1}
-spec evalCoord2f(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2f(U,V) ->
  cast(5295, <<U:?GLfloat,V:?GLfloat>>).

%% @equiv evalCoord2d(U,V)
-spec evalCoord2dv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).

%% @equiv evalCoord2f(U,V)
-spec evalCoord2fv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).

%% @doc Define a one- or two-dimensional mesh
%%
%% ``gl:mapGrid'' and  {@link gl:evalMesh1/3}  are used together to efficiently generate and
%% evaluate a series of evenly-spaced map domain values.  {@link gl:evalMesh1/3}  steps through
%% the integer domain of a one- or two-dimensional grid, whose range is the domain of the
%% evaluation maps specified by  {@link gl:map1d/6}  and  {@link gl:map1d/6} . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMapGrid.xml">external</a> documentation.
-spec mapGrid1d(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1d(Un,U1,U2) ->
  cast(5296, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble>>).

%% @doc 
%% See {@link mapGrid1d/3}
-spec mapGrid1f(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1f(Un,U1,U2) ->
  cast(5297, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat>>).

%% @doc 
%% See {@link mapGrid1d/3}
-spec mapGrid2d(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2d(Un,U1,U2,Vn,V1,V2) ->
  cast(5298, <<Un:?GLint,0:32,U1:?GLdouble,U2:?GLdouble,Vn:?GLint,0:32,V1:?GLdouble,V2:?GLdouble>>).

%% @doc 
%% See {@link mapGrid1d/3}
-spec mapGrid2f(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2f(Un,U1,U2,Vn,V1,V2) ->
  cast(5299, <<Un:?GLint,U1:?GLfloat,U2:?GLfloat,Vn:?GLint,V1:?GLfloat,V2:?GLfloat>>).

%% @doc Generate and evaluate a single point in a mesh
%%
%%  {@link gl:mapGrid1d/3}  and  {@link gl:evalMesh1/3}  are used in tandem to efficiently generate
%% and evaluate a series of evenly spaced map domain values. ``gl:evalPoint'' can be used
%% to evaluate a single grid point in the same gridspace that is traversed by  {@link gl:evalMesh1/3} 
%% . Calling ``gl:evalPoint1'' is equivalent to calling  glEvalCoord1(  i.&amp;Delta; u+u
%% 1 );  where &amp;Delta; u=(u 2-u 1)/n
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalPoint.xml">external</a> documentation.
-spec evalPoint1(I) -> 'ok' when I :: integer().
evalPoint1(I) ->
  cast(5300, <<I:?GLint>>).

%% @doc 
%% See {@link evalPoint1/1}
-spec evalPoint2(I, J) -> 'ok' when I :: integer(),J :: integer().
evalPoint2(I,J) ->
  cast(5301, <<I:?GLint,J:?GLint>>).

%% @doc Compute a one- or two-dimensional grid of points or lines
%%
%%  {@link gl:mapGrid1d/3}  and ``gl:evalMesh'' are used in tandem to efficiently generate and
%% evaluate a series of evenly-spaced map domain values. ``gl:evalMesh'' steps through
%% the integer domain of a one- or two-dimensional grid, whose range is the domain of the
%% evaluation maps specified by  {@link gl:map1d/6}  and  {@link gl:map1d/6} .  `Mode'  determines
%% whether the resulting vertices are connected as points, lines, or filled polygons. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalMesh.xml">external</a> documentation.
-spec evalMesh1(Mode, I1, I2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer().
evalMesh1(Mode,I1,I2) ->
  cast(5302, <<Mode:?GLenum,I1:?GLint,I2:?GLint>>).

%% @doc 
%% See {@link evalMesh1/3}
-spec evalMesh2(Mode, I1, I2, J1, J2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer(),J1 :: integer(),J2 :: integer().
evalMesh2(Mode,I1,I2,J1,J2) ->
  cast(5303, <<Mode:?GLenum,I1:?GLint,I2:?GLint,J1:?GLint,J2:?GLint>>).

%% @doc Specify fog parameters
%%
%%  Fog is initially disabled. While enabled, fog affects rasterized geometry, bitmaps, and
%% pixel blocks, but not buffer clear operations. To enable and disable fog, call  {@link gl:enable/1} 
%%  and  {@link gl:enable/1}  with argument `?GL_FOG'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFog.xml">external</a> documentation.
-spec fogf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
fogf(Pname,Param) ->
  cast(5304, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link fogf/2}
-spec fogi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
fogi(Pname,Param) ->
  cast(5305, <<Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link fogf/2}
-spec fogfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogfv(Pname,Params) ->
  cast(5306, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link fogf/2}
-spec fogiv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogiv(Pname,Params) ->
  cast(5307, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc Controls feedback mode
%%
%%  The ``gl:feedbackBuffer'' function controls feedback. Feedback, like selection, is
%% a GL mode. The mode is selected by calling  {@link gl:renderMode/1}  with `?GL_FEEDBACK'.
%% When the GL is in feedback mode, no pixels are produced by rasterization. Instead, information
%% about primitives that would have been rasterized is fed back to the application using
%% the GL. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFeedbackBuffer.xml">external</a> documentation.
-spec feedbackBuffer(Size, Type, Buffer) -> 'ok' when Size :: integer(),Type :: enum(),Buffer :: mem().
feedbackBuffer(Size,Type,Buffer) ->
  send_bin(Buffer),
  call(5308, <<Size:?GLsizei,Type:?GLenum>>).

%% @doc Place a marker in the feedback buffer
%%
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPassThrough.xml">external</a> documentation.
-spec passThrough(Token) -> 'ok' when Token :: float().
passThrough(Token) ->
  cast(5309, <<Token:?GLfloat>>).

%% @doc Establish a buffer for selection mode values
%%
%% ``gl:selectBuffer'' has two arguments:  `Buffer'  is a pointer to an array of unsigned
%% integers, and  `Size'  indicates the size of the array.  `Buffer'  returns values
%% from the name stack (see  {@link gl:initNames/0} ,  {@link gl:loadName/1} ,  {@link gl:pushName/1} )
%% when the rendering mode is `?GL_SELECT' (see  {@link gl:renderMode/1} ). ``gl:selectBuffer''
%%  must be issued before selection mode is enabled, and it must not be issued while the
%% rendering mode is `?GL_SELECT'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSelectBuffer.xml">external</a> documentation.
-spec selectBuffer(Size, Buffer) -> 'ok' when Size :: integer(),Buffer :: mem().
selectBuffer(Size,Buffer) ->
  send_bin(Buffer),
  call(5310, <<Size:?GLsizei>>).

%% @doc Initialize the name stack
%%
%%  The name stack is used during selection mode to allow sets of rendering commands to be
%% uniquely identified. It consists of an ordered set of unsigned integers. ``gl:initNames''
%%  causes the name stack to be initialized to its default empty state. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInitNames.xml">external</a> documentation.
-spec initNames() -> 'ok'.
initNames() ->
  cast(5311, <<>>).

%% @doc Load a name onto the name stack
%%
%%  The name stack is used during selection mode to allow sets of rendering commands to be
%% uniquely identified. It consists of an ordered set of unsigned integers and is initially
%% empty. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadName.xml">external</a> documentation.
-spec loadName(Name) -> 'ok' when Name :: integer().
loadName(Name) ->
  cast(5312, <<Name:?GLuint>>).

%% @doc Push and pop the name stack
%%
%%  The name stack is used during selection mode to allow sets of rendering commands to be
%% uniquely identified. It consists of an ordered set of unsigned integers and is initially
%% empty. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushName.xml">external</a> documentation.
-spec pushName(Name) -> 'ok' when Name :: integer().
pushName(Name) ->
  cast(5313, <<Name:?GLuint>>).

%% @doc 
%% See {@link pushName/1}
-spec popName() -> 'ok'.
popName() ->
  cast(5314, <<>>).

%% @doc Set the blend color
%%
%%  The `?GL_BLEND_COLOR' may be used to calculate the source and destination blending
%% factors. The color components are clamped to the range  [0 1] before being stored. See  {@link gl:blendFunc/2} 
%%  for a complete description of the blending operations. Initially the `?GL_BLEND_COLOR'
%%  is set to (0, 0, 0, 0). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendColor.xhtml">external</a> documentation.
-spec blendColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
blendColor(Red,Green,Blue,Alpha) ->
  cast(5315, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @doc Specify the equation used for both the RGB blend equation and the Alpha blend equation
%%
%%  The blend equations determine how a new pixel (the ''source'' color) is combined with
%% a pixel already in the framebuffer (the ''destination'' color). This function sets both
%% the RGB blend equation and the alpha  blend equation to a single equation. ``gl:blendEquationi''
%%  specifies the blend equation for a single draw buffer whereas ``gl:blendEquation''
%% sets the blend equation for all draw buffers. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquation.xhtml">external</a> documentation.
-spec blendEquation(Mode) -> 'ok' when Mode :: enum().
blendEquation(Mode) ->
  cast(5316, <<Mode:?GLenum>>).

%% @doc Render primitives from array data
%%
%% ``gl:drawRangeElements'' is a restricted form of  {@link gl:drawElements/4} .  `Mode' ,
%%  `Start' ,  `End' , and  `Count'  match the corresponding arguments to  {@link gl:drawElements/4} 
%% , with the additional constraint that all values in the arrays  `Count'  must lie between
%%  `Start'  and  `End' , inclusive. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElements.xhtml">external</a> documentation.
-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawRangeElements(Mode,Start,End,Count,Type,Indices) when  is_integer(Indices) ->
  cast(5317, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint>>);
drawRangeElements(Mode,Start,End,Count,Type,Indices) ->
  send_bin(Indices),
  cast(5318, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum>>).

%% @doc Specify a three-dimensional texture image
%%
%%  Texturing maps a portion of a specified texture image onto each graphical primitive for
%% which texturing is enabled. To enable and disable three-dimensional texturing, call  {@link gl:enable/1} 
%%  and  {@link gl:enable/1}  with argument `?GL_TEXTURE_3D'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3D.xhtml">external</a> documentation.
-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5319, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5320, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @doc glTexSubImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5321, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5322, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Copy a three-dimensional texture subimage
%%
%% ``gl:copyTexSubImage3D'' replaces a rectangular portion of a three-dimensional texture
%% image with pixels from the current `?GL_READ_BUFFER' (rather than from main memory,
%% as is the case for  {@link gl:texSubImage1D/7} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage3D.xhtml">external</a> documentation.
-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) ->
  cast(5323, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Define a color lookup table
%%
%% ``gl:colorTable'' may be used in two ways: to test the actual size and color resolution
%% of a lookup table given a particular set of parameters, or to load the contents of a color
%% lookup table. Use the targets `?GL_PROXY_*' for the first case and the other targets
%% for the second case. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTable.xml">external</a> documentation.
-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Table :: offset()|mem().
colorTable(Target,Internalformat,Width,Format,Type,Table) when  is_integer(Table) ->
  cast(5324, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Table:?GLuint>>);
colorTable(Target,Internalformat,Width,Format,Type,Table) ->
  send_bin(Table),
  cast(5325, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Set color lookup table parameters
%%
%% ``gl:colorTableParameter'' is used to specify the scale factors and bias terms applied
%% to color components when they are loaded into a color table.  `Target'  indicates which
%% color table the scale and bias terms apply to; it must be set to `?GL_COLOR_TABLE', `?GL_POST_CONVOLUTION_COLOR_TABLE'
%% , or `?GL_POST_COLOR_MATRIX_COLOR_TABLE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTableParameter.xml">external</a> documentation.
-spec colorTableParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {float(),float(),float(),float()}.
colorTableParameterfv(Target,Pname,{P1,P2,P3,P4}) ->
  cast(5326, <<Target:?GLenum,Pname:?GLenum,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @doc 
%% See {@link colorTableParameterfv/3}
-spec colorTableParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {integer(),integer(),integer(),integer()}.
colorTableParameteriv(Target,Pname,{P1,P2,P3,P4}) ->
  cast(5327, <<Target:?GLenum,Pname:?GLenum,P1:?GLint,P2:?GLint,P3:?GLint,P4:?GLint>>).

%% @doc Copy pixels into a color table
%%
%% ``gl:copyColorTable'' loads a color table with pixels from the current `?GL_READ_BUFFER'
%%  (rather than from main memory, as is the case for  {@link gl:colorTable/6} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorTable.xml">external</a> documentation.
-spec copyColorTable(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyColorTable(Target,Internalformat,X,Y,Width) ->
  cast(5328, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Retrieve contents of a color lookup table
%%
%% ``gl:getColorTable'' returns in  `Table'  the contents of the color table specified
%% by  `Target' . No pixel transfer operations are performed, but pixel storage modes
%% that are applicable to  {@link gl:readPixels/7}  are performed. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTable.xml">external</a> documentation.
-spec getColorTable(Target, Format, Type, Table) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Table :: mem().
getColorTable(Target,Format,Type,Table) ->
  send_bin(Table),
  call(5329, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @doc Get color lookup table parameters
%%
%%  Returns parameters specific to color table  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTableParameter.xml">external</a> documentation.
-spec getColorTableParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getColorTableParameterfv(Target,Pname) ->
  call(5330, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getColorTableParameterfv/2}
-spec getColorTableParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getColorTableParameteriv(Target,Pname) ->
  call(5331, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Respecify a portion of a color table
%%
%% ``gl:colorSubTable'' is used to respecify a contiguous portion of a color table previously
%% defined using  {@link gl:colorTable/6} . The pixels referenced by  `Data'  replace the
%% portion of the existing table from indices  `Start'  to  start+count-1, inclusive.
%% This region may not include any entries outside the range of the color table as it was
%% originally specified. It is not an error to specify a subtexture with width of 0, but
%% such a specification has no effect. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorSubTable.xml">external</a> documentation.
-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> 'ok' when Target :: enum(),Start :: integer(),Count :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
colorSubTable(Target,Start,Count,Format,Type,Data) when  is_integer(Data) ->
  cast(5332, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum,Data:?GLuint>>);
colorSubTable(Target,Start,Count,Format,Type,Data) ->
  send_bin(Data),
  cast(5333, <<Target:?GLenum,Start:?GLsizei,Count:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Respecify a portion of a color table
%%
%% ``gl:copyColorSubTable'' is used to respecify a contiguous portion of a color table
%% previously defined using  {@link gl:colorTable/6} . The pixels copied from the framebuffer
%% replace the portion of the existing table from indices  `Start'  to  start+x-1, inclusive.
%% This region may not include any entries outside the range of the color table, as was originally
%% specified. It is not an error to specify a subtexture with width of 0, but such a specification
%% has no effect. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorSubTable.xml">external</a> documentation.
-spec copyColorSubTable(Target, Start, X, Y, Width) -> 'ok' when Target :: enum(),Start :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyColorSubTable(Target,Start,X,Y,Width) ->
  cast(5334, <<Target:?GLenum,Start:?GLsizei,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Define a one-dimensional convolution filter
%%
%% ``gl:convolutionFilter1D'' builds a one-dimensional convolution filter kernel from an
%% array of pixels. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter1D.xml">external</a> documentation.
-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when  is_integer(Image) ->
  cast(5335, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) ->
  send_bin(Image),
  cast(5336, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Define a two-dimensional convolution filter
%%
%% ``gl:convolutionFilter2D'' builds a two-dimensional convolution filter kernel from an
%% array of pixels. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter2D.xml">external</a> documentation.
-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when  is_integer(Image) ->
  cast(5337, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Image:?GLuint>>);
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) ->
  send_bin(Image),
  cast(5338, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Set convolution parameters
%%
%% ``gl:convolutionParameter'' sets the value of a convolution parameter. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionParameter.xml">external</a> documentation.
-spec convolutionParameterf(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameterf(Target,Pname,Params) ->
  cast(5339, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @equiv convolutionParameterf(Target,Pname,Params)
-spec convolutionParameterfv(Target :: enum(),Pname :: enum(),Params) -> 'ok' when Params :: {Params :: tuple()}.
convolutionParameterfv(Target,Pname,{Params}) ->  convolutionParameterf(Target,Pname,Params).

%% @doc 
%% See {@link convolutionParameterf/3}
-spec convolutionParameteri(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameteri(Target,Pname,Params) ->
  cast(5340, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @equiv convolutionParameteri(Target,Pname,Params)
-spec convolutionParameteriv(Target :: enum(),Pname :: enum(),Params) -> 'ok' when Params :: {Params :: tuple()}.
convolutionParameteriv(Target,Pname,{Params}) ->  convolutionParameteri(Target,Pname,Params).

%% @doc Copy pixels into a one-dimensional convolution filter
%%
%% ``gl:copyConvolutionFilter1D'' defines a one-dimensional convolution filter kernel with
%% pixels from the current `?GL_READ_BUFFER' (rather than from main memory, as is the
%% case for  {@link gl:convolutionFilter1D/6} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter1D.xml">external</a> documentation.
-spec copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) ->
  cast(5341, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Copy pixels into a two-dimensional convolution filter
%%
%% ``gl:copyConvolutionFilter2D'' defines a two-dimensional convolution filter kernel with
%% pixels from the current `?GL_READ_BUFFER' (rather than from main memory, as is the
%% case for  {@link gl:convolutionFilter2D/7} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter2D.xml">external</a> documentation.
-spec copyConvolutionFilter2D(Target, Internalformat, X, Y, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) ->
  cast(5342, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Get current 1D or 2D convolution filter kernel
%%
%% ``gl:getConvolutionFilter'' returns the current 1D or 2D convolution filter kernel as
%% an image. The one- or two-dimensional image is placed in  `Image'  according to the
%% specifications in  `Format'  and  `Type' . No pixel transfer operations are performed
%% on this image, but the relevant pixel storage modes are applied. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionFilter.xml">external</a> documentation.
-spec getConvolutionFilter(Target, Format, Type, Image) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Image :: mem().
getConvolutionFilter(Target,Format,Type,Image) ->
  send_bin(Image),
  call(5343, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @doc Get convolution parameters
%%
%% ``gl:getConvolutionParameter'' retrieves convolution parameters.  `Target'  determines
%% which convolution filter is queried.  `Pname'  determines which parameter is returned:
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
-spec getConvolutionParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getConvolutionParameterfv(Target,Pname) ->
  call(5344, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getConvolutionParameterfv/2}
-spec getConvolutionParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getConvolutionParameteriv(Target,Pname) ->
  call(5345, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Define a separable two-dimensional convolution filter
%%
%% ``gl:separableFilter2D'' builds a two-dimensional separable convolution filter kernel
%% from two arrays of pixels. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSeparableFilter2D.xml">external</a> documentation.
-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Row :: offset()|mem(),Column :: offset()|mem().
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when  is_integer(Row), is_integer(Column) ->
  cast(5346, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Row:?GLuint,Column:?GLuint>>);
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) ->
  send_bin(Row),
  send_bin(Column),
  cast(5347, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc Get histogram table
%%
%% ``gl:getHistogram'' returns the current histogram table as a one-dimensional image with
%% the same width as the histogram. No pixel transfer operations are performed on this image,
%% but pixel storage modes that are applicable to 1D images are honored. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogram.xml">external</a> documentation.
-spec getHistogram(Target, Reset, Format, Type, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Type :: enum(),Values :: mem().
getHistogram(Target,Reset,Format,Type,Values) ->
  send_bin(Values),
  call(5348, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Type:?GLenum>>).

%% @doc Get histogram parameters
%%
%% ``gl:getHistogramParameter'' is used to query parameter values for the current histogram
%% or for a proxy. The histogram state information may be queried by calling ``gl:getHistogramParameter''
%%  with a  `Target'  of `?GL_HISTOGRAM' (to obtain information for the current histogram
%% table) or `?GL_PROXY_HISTOGRAM' (to obtain information from the most recent proxy
%% request) and one of the following values for the  `Pname'  argument: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogramParameter.xml">external</a> documentation.
-spec getHistogramParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getHistogramParameterfv(Target,Pname) ->
  call(5349, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getHistogramParameterfv/2}
-spec getHistogramParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getHistogramParameteriv(Target,Pname) ->
  call(5350, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Get minimum and maximum pixel values
%%
%% ``gl:getMinmax'' returns the accumulated minimum and maximum pixel values (computed
%% on a per-component basis) in a one-dimensional image of width 2. The first set of return
%% values are the minima, and the second set of return values are the maxima. The format
%% of the return values is determined by  `Format' , and their type is determined by  `Types' 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmax.xml">external</a> documentation.
-spec getMinmax(Target, Reset, Format, Types, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Types :: enum(),Values :: mem().
getMinmax(Target,Reset,Format,Types,Values) ->
  send_bin(Values),
  call(5351, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Types:?GLenum>>).

%% @doc Get minmax parameters
%%
%% ``gl:getMinmaxParameter'' retrieves parameters for the current minmax table by setting  `Pname' 
%%  to one of the following values: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
-spec getMinmaxParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getMinmaxParameterfv(Target,Pname) ->
  call(5352, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc 
%% See {@link getMinmaxParameterfv/2}
-spec getMinmaxParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getMinmaxParameteriv(Target,Pname) ->
  call(5353, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Define histogram table
%%
%%  When `?GL_HISTOGRAM' is enabled, RGBA color components are converted to histogram
%% table indices by clamping to the range [0,1], multiplying by the width of the histogram
%% table, and rounding to the nearest integer. The table entries selected by the RGBA indices
%% are then incremented. (If the internal format of the histogram table includes luminance,
%% then the index derived from the R color component determines the luminance table entry
%% to be incremented.) If a histogram table entry is incremented beyond its maximum value,
%% then its value becomes undefined. (This is not an error.) 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glHistogram.xml">external</a> documentation.
-spec histogram(Target, Width, Internalformat, Sink) -> 'ok' when Target :: enum(),Width :: integer(),Internalformat :: enum(),Sink :: 0|1.
histogram(Target,Width,Internalformat,Sink) ->
  cast(5354, <<Target:?GLenum,Width:?GLsizei,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @doc Define minmax table
%%
%%  When `?GL_MINMAX' is enabled, the RGBA components of incoming pixels are compared
%% to the minimum and maximum values for each component, which are stored in the two-element
%% minmax table. (The first element stores the minima, and the second element stores the
%% maxima.) If a pixel component is greater than the corresponding component in the maximum
%% element, then the maximum element is updated with the pixel component value. If a pixel
%% component is less than the corresponding component in the minimum element, then the minimum
%% element is updated with the pixel component value. (In both cases, if the internal format
%% of the minmax table includes luminance, then the R color component of incoming pixels
%% is used for comparison.) The contents of the minmax table may be retrieved at a later
%% time by calling  {@link gl:getMinmax/5} . The minmax operation is enabled or disabled by
%% calling  {@link gl:enable/1}  or  {@link gl:enable/1} , respectively, with an argument of `?GL_MINMAX'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMinmax.xml">external</a> documentation.
-spec minmax(Target, Internalformat, Sink) -> 'ok' when Target :: enum(),Internalformat :: enum(),Sink :: 0|1.
minmax(Target,Internalformat,Sink) ->
  cast(5355, <<Target:?GLenum,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @doc Reset histogram table entries to zero
%%
%% ``gl:resetHistogram'' resets all the elements of the current histogram table to zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetHistogram.xml">external</a> documentation.
-spec resetHistogram(Target) -> 'ok' when Target :: enum().
resetHistogram(Target) ->
  cast(5356, <<Target:?GLenum>>).

%% @doc Reset minmax table entries to initial values
%%
%% ``gl:resetMinmax'' resets the elements of the current minmax table to their initial
%% values: the ``maximum'' element receives the minimum possible component values, and the
%% ``minimum'' element receives the maximum possible component values. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetMinmax.xml">external</a> documentation.
-spec resetMinmax(Target) -> 'ok' when Target :: enum().
resetMinmax(Target) ->
  cast(5357, <<Target:?GLenum>>).

%% @doc Select active texture unit
%%
%% ``gl:activeTexture'' selects which texture unit subsequent texture state calls will
%% affect. The number of texture units an implementation supports is implementation dependent,
%% but must be at least 80. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveTexture.xhtml">external</a> documentation.
-spec activeTexture(Texture) -> 'ok' when Texture :: enum().
activeTexture(Texture) ->
  cast(5358, <<Texture:?GLenum>>).

%% @doc Specify multisample coverage parameters
%%
%%  Multisampling samples a pixel multiple times at various implementation-dependent subpixel
%% locations to generate antialiasing effects. Multisampling transparently antialiases points,
%% lines, polygons, and images if it is enabled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleCoverage.xhtml">external</a> documentation.
-spec sampleCoverage(Value, Invert) -> 'ok' when Value :: clamp(),Invert :: 0|1.
sampleCoverage(Value,Invert) ->
  cast(5359, <<Value:?GLclampf,Invert:?GLboolean>>).

%% @doc Specify a three-dimensional texture image in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage3D.xhtml">external</a> documentation.
-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5360, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5361, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @doc Specify a two-dimensional texture image in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage2D.xhtml">external</a> documentation.
-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5362, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5363, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @doc Specify a one-dimensional texture image in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage1D.xhtml">external</a> documentation.
-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when  is_integer(Data) ->
  cast(5364, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) ->
  send_bin(Data),
  cast(5365, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,Width:?GLsizei,Border:?GLint,ImageSize:?GLsizei>>).

%% @doc Specify a three-dimensional texture subimage in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage3D.xhtml">external</a> documentation.
-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5366, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5367, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Zoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @doc Specify a two-dimensional texture subimage in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage2D.xhtml">external</a> documentation.
-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5368, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5369, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @doc Specify a one-dimensional texture subimage in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage1D.xhtml">external</a> documentation.
-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when  is_integer(Data) ->
  cast(5370, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei,Data:?GLuint>>);
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) ->
  send_bin(Data),
  cast(5371, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,ImageSize:?GLsizei>>).

%% @doc Return a compressed texture image
%%
%% ``gl:getCompressedTexImage'' returns the compressed texture image associated with  `Target' 
%%  and  `Lod'  into  `Img' .  `Img'  should be an array of `?GL_TEXTURE_COMPRESSED_IMAGE_SIZE'
%%  bytes.  `Target'  specifies whether the desired texture image was one specified by  {@link gl:texImage1D/8} 
%%  (`?GL_TEXTURE_1D'),  {@link gl:texImage2D/9}  (`?GL_TEXTURE_2D' or any of `?GL_TEXTURE_CUBE_MAP_*'
%% ), or  {@link gl:texImage3D/10}  (`?GL_TEXTURE_3D').  `Lod'  specifies the level-of-detail
%% number of the desired image. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetCompressedTexImage.xhtml">external</a> documentation.
-spec getCompressedTexImage(Target, Lod, Img) -> 'ok' when Target :: enum(),Lod :: integer(),Img :: mem().
getCompressedTexImage(Target,Lod,Img) ->
  send_bin(Img),
  call(5372, <<Target:?GLenum,Lod:?GLint>>).

%% @doc Select active texture unit
%%
%% ``gl:clientActiveTexture'' selects the vertex array client state parameters to be modified
%% by  {@link gl:texCoordPointer/4} , and enabled or disabled with  {@link gl:enableClientState/1} 
%% or  {@link gl:enableClientState/1} , respectively, when called with a parameter of `?GL_TEXTURE_COORD_ARRAY'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClientActiveTexture.xml">external</a> documentation.
-spec clientActiveTexture(Texture) -> 'ok' when Texture :: enum().
clientActiveTexture(Texture) ->
  cast(5373, <<Texture:?GLenum>>).

%% @doc Set the current texture coordinates
%%
%% ``gl:multiTexCoord'' specifies texture coordinates in one, two, three, or four dimensions.
%% ``gl:multiTexCoord1'' sets the current texture coordinates to  (s 0 0 1); a call to ``gl:multiTexCoord2''
%%  sets them to  (s t 0 1). Similarly, ``gl:multiTexCoord3'' specifies the texture coordinates as (s
%%  t r 1),
%% and ``gl:multiTexCoord4'' defines all four components explicitly as (s t r q). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultiTexCoord.xml">external</a> documentation.
-spec multiTexCoord1d(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1d(Target,S) ->
  cast(5374, <<Target:?GLenum,0:32,S:?GLdouble>>).

%% @equiv multiTexCoord1d(Target,S)
-spec multiTexCoord1dv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord1f(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1f(Target,S) ->
  cast(5375, <<Target:?GLenum,S:?GLfloat>>).

%% @equiv multiTexCoord1f(Target,S)
-spec multiTexCoord1fv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord1i(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1i(Target,S) ->
  cast(5376, <<Target:?GLenum,S:?GLint>>).

%% @equiv multiTexCoord1i(Target,S)
-spec multiTexCoord1iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord1s(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1s(Target,S) ->
  cast(5377, <<Target:?GLenum,S:?GLshort>>).

%% @equiv multiTexCoord1s(Target,S)
-spec multiTexCoord1sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord2d(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2d(Target,S,T) ->
  cast(5378, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble>>).

%% @equiv multiTexCoord2d(Target,S,T)
-spec multiTexCoord2dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord2f(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2f(Target,S,T) ->
  cast(5379, <<Target:?GLenum,S:?GLfloat,T:?GLfloat>>).

%% @equiv multiTexCoord2f(Target,S,T)
-spec multiTexCoord2fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord2i(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2i(Target,S,T) ->
  cast(5380, <<Target:?GLenum,S:?GLint,T:?GLint>>).

%% @equiv multiTexCoord2i(Target,S,T)
-spec multiTexCoord2iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord2s(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2s(Target,S,T) ->
  cast(5381, <<Target:?GLenum,S:?GLshort,T:?GLshort>>).

%% @equiv multiTexCoord2s(Target,S,T)
-spec multiTexCoord2sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord3d(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3d(Target,S,T,R) ->
  cast(5382, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble>>).

%% @equiv multiTexCoord3d(Target,S,T,R)
-spec multiTexCoord3dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord3f(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3f(Target,S,T,R) ->
  cast(5383, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat>>).

%% @equiv multiTexCoord3f(Target,S,T,R)
-spec multiTexCoord3fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord3i(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3i(Target,S,T,R) ->
  cast(5384, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint>>).

%% @equiv multiTexCoord3i(Target,S,T,R)
-spec multiTexCoord3iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord3s(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3s(Target,S,T,R) ->
  cast(5385, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort>>).

%% @equiv multiTexCoord3s(Target,S,T,R)
-spec multiTexCoord3sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord4d(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4d(Target,S,T,R,Q) ->
  cast(5386, <<Target:?GLenum,0:32,S:?GLdouble,T:?GLdouble,R:?GLdouble,Q:?GLdouble>>).

%% @equiv multiTexCoord4d(Target,S,T,R,Q)
-spec multiTexCoord4dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord4f(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4f(Target,S,T,R,Q) ->
  cast(5387, <<Target:?GLenum,S:?GLfloat,T:?GLfloat,R:?GLfloat,Q:?GLfloat>>).

%% @equiv multiTexCoord4f(Target,S,T,R,Q)
-spec multiTexCoord4fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord4i(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4i(Target,S,T,R,Q) ->
  cast(5388, <<Target:?GLenum,S:?GLint,T:?GLint,R:?GLint,Q:?GLint>>).

%% @equiv multiTexCoord4i(Target,S,T,R,Q)
-spec multiTexCoord4iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).

%% @doc 
%% See {@link multiTexCoord1d/2}
-spec multiTexCoord4s(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4s(Target,S,T,R,Q) ->
  cast(5389, <<Target:?GLenum,S:?GLshort,T:?GLshort,R:?GLshort,Q:?GLshort>>).

%% @equiv multiTexCoord4s(Target,S,T,R,Q)
-spec multiTexCoord4sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).

%% @doc Replace the current matrix with the specified row-major ordered matrix
%%
%% ``gl:loadTransposeMatrix'' replaces the current matrix with the one whose elements are
%% specified by  `M' . The current matrix is the projection matrix, modelview matrix,
%% or texture matrix, depending on the current matrix mode (see  {@link gl:matrixMode/1} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
-spec loadTransposeMatrixf(M) -> 'ok' when M :: matrix().
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5390, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc 
%% See {@link loadTransposeMatrixf/1}
-spec loadTransposeMatrixd(M) -> 'ok' when M :: matrix().
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5391, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc Multiply the current matrix with the specified row-major ordered matrix
%%
%% ``gl:multTransposeMatrix'' multiplies the current matrix with the one specified using  `M' 
%% , and replaces the current matrix with the product. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
-spec multTransposeMatrixf(M) -> 'ok' when M :: matrix().
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixf({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5392, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc 
%% See {@link multTransposeMatrixf/1}
-spec multTransposeMatrixd(M) -> 'ok' when M :: matrix().
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixd({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5393, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc Specify pixel arithmetic for RGB and alpha components separately
%%
%%  Pixels can be drawn using a function that blends the incoming (source) RGBA values with
%% the RGBA values that are already in the frame buffer (the destination values). Blending
%% is initially disabled. Use  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_BLEND'
%%  to enable and disable blending. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFuncSeparate.xhtml">external</a> documentation.
-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 'ok' when SfactorRGB :: enum(),DfactorRGB :: enum(),SfactorAlpha :: enum(),DfactorAlpha :: enum().
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) ->
  cast(5394, <<SfactorRGB:?GLenum,DfactorRGB:?GLenum,SfactorAlpha:?GLenum,DfactorAlpha:?GLenum>>).

%% @doc Render multiple sets of primitives from array data
%%
%% ``gl:multiDrawArrays'' specifies multiple sets of geometric primitives with very few
%% subroutine calls. Instead of calling a GL procedure to pass each individual vertex, normal,
%% texture coordinate, edge flag, or color, you can prespecify separate arrays of vertices,
%% normals, and colors and use them to construct a sequence of primitives with a single call
%% to ``gl:multiDrawArrays''. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawArrays.xhtml">external</a> documentation.
-spec multiDrawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: [integer()]|mem(),Count :: [integer()]|mem().
multiDrawArrays(Mode,First,Count) when  is_list(First), is_list(Count) ->
  FirstLen = length(First),
  CountLen = length(Count),
  cast(5395, <<Mode:?GLenum,FirstLen:?GLuint,
        (<< <<C:?GLint>> || C <- First>>)/binary,0:(((FirstLen) rem 2)*32),CountLen:?GLuint,
        (<< <<C:?GLsizei>> || C <- Count>>)/binary,0:(((1+CountLen) rem 2)*32)>>);
multiDrawArrays(Mode,First,Count) ->
  send_bin(First),
  FirstLen = byte_size(if is_binary(First) -> First; is_tuple(First) -> element(2, First) end) div 4,
  send_bin(Count),
  CountLen = byte_size(if is_binary(Count) -> Count; is_tuple(Count) -> element(2, Count) end) div 4,
  cast(5396, <<Mode:?GLenum,FirstLen:?GLint,CountLen:?GLsizei>>).

%% @doc Specify point parameters
%%
%%  The following values are accepted for  `Pname' : 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointParameter.xhtml">external</a> documentation.
-spec pointParameterf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pointParameterf(Pname,Param) ->
  cast(5397, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameterfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameterfv(Pname,Params) ->
  cast(5398, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameteri(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pointParameteri(Pname,Param) ->
  cast(5399, <<Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameteriv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameteriv(Pname,Params) ->
  cast(5400, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc Set the current fog coordinates
%%
%% ``gl:fogCoord'' specifies the fog coordinate that is associated with each vertex and
%% the current raster position. The value specified is interpolated and used in computing
%% the fog color (see  {@link gl:fogf/2} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoord.xml">external</a> documentation.
-spec fogCoordf(Coord) -> 'ok' when Coord :: float().
fogCoordf(Coord) ->
  cast(5401, <<Coord:?GLfloat>>).

%% @equiv fogCoordf(Coord)
-spec fogCoordfv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoordfv({Coord}) ->  fogCoordf(Coord).

%% @doc 
%% See {@link fogCoordf/1}
-spec fogCoordd(Coord) -> 'ok' when Coord :: float().
fogCoordd(Coord) ->
  cast(5402, <<Coord:?GLdouble>>).

%% @equiv fogCoordd(Coord)
-spec fogCoorddv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoorddv({Coord}) ->  fogCoordd(Coord).

%% @doc Define an array of fog coordinates
%%
%% ``gl:fogCoordPointer'' specifies the location and data format of an array of fog coordinates
%% to use when rendering.  `Type'  specifies the data type of each fog coordinate, and  `Stride' 
%%  specifies the byte stride from one fog coordinate to the next, allowing vertices and
%% attributes to be packed into a single array or stored in separate arrays. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoordPointer.xml">external</a> documentation.
-spec fogCoordPointer(Type, Stride, Pointer) -> 'ok' when Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
fogCoordPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5403, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
fogCoordPointer(Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5404, <<Type:?GLenum,Stride:?GLsizei>>).

%% @doc Set the current secondary color
%%
%%  The GL stores both a primary four-valued RGBA color and a secondary four-valued RGBA
%% color (where alpha is always set to 0.0) that is associated with every vertex. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3b(Red,Green,Blue) ->
  cast(5405, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @equiv secondaryColor3b(Red,Green,Blue)
-spec secondaryColor3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3d(Red,Green,Blue) ->
  cast(5406, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @equiv secondaryColor3d(Red,Green,Blue)
-spec secondaryColor3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3f(Red,Green,Blue) ->
  cast(5407, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @equiv secondaryColor3f(Red,Green,Blue)
-spec secondaryColor3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3i(Red,Green,Blue) ->
  cast(5408, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @equiv secondaryColor3i(Red,Green,Blue)
-spec secondaryColor3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3s(Red,Green,Blue) ->
  cast(5409, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @equiv secondaryColor3s(Red,Green,Blue)
-spec secondaryColor3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ub(Red,Green,Blue) ->
  cast(5410, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @equiv secondaryColor3ub(Red,Green,Blue)
-spec secondaryColor3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ui(Red,Green,Blue) ->
  cast(5411, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @equiv secondaryColor3ui(Red,Green,Blue)
-spec secondaryColor3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3us(Red,Green,Blue) ->
  cast(5412, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

%% @equiv secondaryColor3us(Red,Green,Blue)
-spec secondaryColor3usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).

%% @doc Define an array of secondary colors
%%
%% ``gl:secondaryColorPointer'' specifies the location and data format of an array of color
%% components to use when rendering.  `Size'  specifies the number of components per color,
%% and must be 3.  `Type'  specifies the data type of each color component, and  `Stride' 
%%  specifies the byte stride from one color to the next, allowing vertices and attributes
%% to be packed into a single array or stored in separate arrays. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColorPointer.xml">external</a> documentation.
-spec secondaryColorPointer(Size, Type, Stride, Pointer) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
secondaryColorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5413, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
secondaryColorPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5414, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc Specify the raster position in window coordinates for pixel operations
%%
%%  The GL maintains a 3D position in window coordinates. This position, called the raster
%% position, is used to position pixel and bitmap write operations. It is maintained with
%% subpixel accuracy. See  {@link gl:bitmap/7} ,  {@link gl:drawPixels/5} , and  {@link gl:copyPixels/5} 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2d(X,Y) ->
  cast(5415, <<X:?GLdouble,Y:?GLdouble>>).

%% @equiv windowPos2d(X,Y)
-spec windowPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2f(X,Y) ->
  cast(5416, <<X:?GLfloat,Y:?GLfloat>>).

%% @equiv windowPos2f(X,Y)
-spec windowPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2i(X,Y) ->
  cast(5417, <<X:?GLint,Y:?GLint>>).

%% @equiv windowPos2i(X,Y)
-spec windowPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2s(X,Y) ->
  cast(5418, <<X:?GLshort,Y:?GLshort>>).

%% @equiv windowPos2s(X,Y)
-spec windowPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3d(X,Y,Z) ->
  cast(5419, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @equiv windowPos3d(X,Y,Z)
-spec windowPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3f(X,Y,Z) ->
  cast(5420, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @equiv windowPos3f(X,Y,Z)
-spec windowPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3i(X,Y,Z) ->
  cast(5421, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @equiv windowPos3i(X,Y,Z)
-spec windowPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3s(X,Y,Z) ->
  cast(5422, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @equiv windowPos3s(X,Y,Z)
-spec windowPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).

%% @doc Generate query object names
%%
%% ``gl:genQueries'' returns  `N'  query object names in  `Ids' . There is no guarantee
%% that the names form a contiguous set of integers; however, it is guaranteed that none
%% of the returned names was in use immediately before the call to ``gl:genQueries''. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenQueries.xhtml">external</a> documentation.
-spec genQueries(N) -> [integer()] when N :: integer().
genQueries(N) ->
  call(5423, <<N:?GLsizei>>).

%% @doc Delete named query objects
%%
%% ``gl:deleteQueries'' deletes  `N'  query objects named by the elements of the array  `Ids' 
%% . After a query object is deleted, it has no contents, and its name is free for reuse
%% (for example by  {@link gl:genQueries/1} ). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteQueries.xhtml">external</a> documentation.
-spec deleteQueries(Ids) -> 'ok' when Ids :: [integer()].
deleteQueries(Ids) ->
  IdsLen = length(Ids),
  cast(5424, <<IdsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+IdsLen) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a query object
%%
%% ``gl:isQuery'' returns `?GL_TRUE' if  `Id'  is currently the name of a query
%% object. If  `Id'  is zero, or is a non-zero value that is not currently the name of
%% a query object, or if an error occurs, ``gl:isQuery'' returns `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsQuery.xhtml">external</a> documentation.
-spec isQuery(Id) -> 0|1 when Id :: integer().
isQuery(Id) ->
  call(5425, <<Id:?GLuint>>).

%% @doc Delimit the boundaries of a query object
%%
%% ``gl:beginQuery'' and  {@link gl:beginQuery/2}  delimit the boundaries of a query object.  `Query' 
%%  must be a name previously returned from a call to  {@link gl:genQueries/1} . If a query
%% object with name  `Id'  does not yet exist it is created with the type determined by  `Target' 
%% .  `Target'  must be one of `?GL_SAMPLES_PASSED', `?GL_ANY_SAMPLES_PASSED', `?GL_PRIMITIVES_GENERATED'
%% , `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN', or `?GL_TIME_ELAPSED'. The behavior
%% of the query object depends on its type and is as follows. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQuery.xhtml">external</a> documentation.
-spec beginQuery(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
beginQuery(Target,Id) ->
  cast(5426, <<Target:?GLenum,Id:?GLuint>>).

%% @doc 
%% See {@link beginQuery/2}
-spec endQuery(Target) -> 'ok' when Target :: enum().
endQuery(Target) ->
  cast(5427, <<Target:?GLenum>>).

%% @doc glGetQuery
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryiv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getQueryiv(Target,Pname) ->
  call(5428, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Return parameters of a query object
%%
%% ``gl:getQueryObject'' returns in  `Params'  a selected parameter of the query object
%% specified by  `Id' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryObject.xhtml">external</a> documentation.
-spec getQueryObjectiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectiv(Id,Pname) ->
  call(5429, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getQueryObjectiv/2}
-spec getQueryObjectuiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectuiv(Id,Pname) ->
  call(5430, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc Bind a named buffer object
%%
%% ``gl:bindBuffer'' binds a buffer object to the specified buffer binding point. Calling ``gl:bindBuffer''
%%  with  `Target'  set to one of the accepted symbolic constants and  `Buffer'  set
%% to the name of a buffer object binds that buffer object name to the target. If no buffer
%% object with name  `Buffer'  exists, one is created with that name. When a buffer object
%% is bound to a target, the previous binding for that target is automatically broken. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml">external</a> documentation.
-spec bindBuffer(Target, Buffer) -> 'ok' when Target :: enum(),Buffer :: integer().
bindBuffer(Target,Buffer) ->
  cast(5431, <<Target:?GLenum,Buffer:?GLuint>>).

%% @doc Delete named buffer objects
%%
%% ``gl:deleteBuffers'' deletes  `N'  buffer objects named by the elements of the array
%%  `Buffers' . After a buffer object is deleted, it has no contents, and its name is
%% free for reuse (for example by  {@link gl:genBuffers/1} ). If a buffer object that is currently
%% bound is deleted, the binding reverts to 0 (the absence of any buffer object). 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteBuffers.xhtml">external</a> documentation.
-spec deleteBuffers(Buffers) -> 'ok' when Buffers :: [integer()].
deleteBuffers(Buffers) ->
  BuffersLen = length(Buffers),
  cast(5432, <<BuffersLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Buffers>>)/binary,0:(((1+BuffersLen) rem 2)*32)>>).

%% @doc Generate buffer object names
%%
%% ``gl:genBuffers'' returns  `N'  buffer object names in  `Buffers' . There is no
%% guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genBuffers''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenBuffers.xhtml">external</a> documentation.
-spec genBuffers(N) -> [integer()] when N :: integer().
genBuffers(N) ->
  call(5433, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a buffer object
%%
%% ``gl:isBuffer'' returns `?GL_TRUE' if  `Buffer'  is currently the name of a
%% buffer object. If  `Buffer'  is zero, or is a non-zero value that is not currently
%% the name of a buffer object, or if an error occurs, ``gl:isBuffer'' returns `?GL_FALSE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsBuffer.xhtml">external</a> documentation.
-spec isBuffer(Buffer) -> 0|1 when Buffer :: integer().
isBuffer(Buffer) ->
  call(5434, <<Buffer:?GLuint>>).

%% @doc Creates and initializes a buffer object's data store
%%
%% ``gl:bufferData'' creates a new data store for the buffer object currently bound to  `Target' 
%% . Any pre-existing data store is deleted. The new data store is created with the specified
%%  `Size'  in bytes and  `Usage' . If  `Data'  is not `?NULL', the data store
%% is initialized with data from this pointer. In its initial  state, the new data store
%% is not mapped, it has a `?NULL' mapped pointer, and its mapped access  is `?GL_READ_WRITE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml">external</a> documentation.
-spec bufferData(Target, Size, Data, Usage) -> 'ok' when Target :: enum(),Size :: integer(),Data :: offset()|mem(),Usage :: enum().
bufferData(Target,Size,Data,Usage) when  is_integer(Data) ->
  cast(5435, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Data:?GLuint,Usage:?GLenum>>);
bufferData(Target,Size,Data,Usage) ->
  send_bin(Data),
  cast(5436, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Usage:?GLenum>>).

%% @doc Updates a subset of a buffer object's data store
%%
%% ``gl:bufferSubData'' redefines some or all of the data store for the buffer object currently
%%  bound to  `Target' . Data starting at byte offset  `Offset'  and extending for  `Size' 
%%  bytes is copied to the data store from the memory pointed to by  `Data' . An error
%% is thrown if  `Offset'  and  `Size'  together define a range beyond the bounds of
%% the buffer object's data store. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml">external</a> documentation.
-spec bufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: offset()|mem().
bufferSubData(Target,Offset,Size,Data) when  is_integer(Data) ->
  cast(5437, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr,Data:?GLuint>>);
bufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  cast(5438, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Returns a subset of a buffer object's data store
%%
%% ``gl:getBufferSubData'' returns some or all of the data from the buffer object currently
%%  bound to  `Target' . Data starting at byte offset  `Offset'  and extending for  `Size' 
%%  bytes is copied from the data store to the memory pointed to by  `Data' . An error
%% is thrown if the buffer object is currently mapped, or if  `Offset'  and  `Size' 
%% together define a range beyond the bounds  of the buffer object's data store. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetBufferSubData.xhtml">external</a> documentation.
-spec getBufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: mem().
getBufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  call(5439, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Return parameters of a buffer object
%%
%% ``gl:getBufferParameteriv'' returns in  `Data'  a selected parameter of the buffer
%% object specified by  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetBufferParameteriv.xml">external</a> documentation.
-spec getBufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getBufferParameteriv(Target,Pname) ->
  call(5440, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Set the RGB blend equation and the alpha blend equation separately
%%
%%  The blend equations determines how a new pixel (the ''source'' color) is combined with
%% a pixel already in the framebuffer (the ''destination'' color). These functions specifie
%% one blend equation for the RGB-color  components and one blend equation for the alpha
%% component. ``gl:blendEquationSeparatei'' specifies the blend equations for a single
%% draw buffer whereas ``gl:blendEquationSeparate'' sets the blend equations for all draw
%% buffers. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquationSeparate.xhtml">external</a> documentation.
-spec blendEquationSeparate(ModeRGB, ModeAlpha) -> 'ok' when ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparate(ModeRGB,ModeAlpha) ->
  cast(5441, <<ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @doc Specifies a list of color buffers to be drawn into
%%
%% ``gl:drawBuffers'' defines an array of buffers into which outputs from the fragment
%% shader data will be written. If a fragment shader writes a value to one or more user defined
%% output variables, then the value of each variable will be written into the buffer specified
%% at a location within  `Bufs'  corresponding to the location assigned to that user defined
%% output. The draw buffer used for user defined outputs assigned to locations greater than
%% or equal to  `N'  is implicitly set to `?GL_NONE' and any data written to such
%% an output is discarded.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffers.xhtml">external</a> documentation.
-spec drawBuffers(Bufs) -> 'ok' when Bufs :: [enum()].
drawBuffers(Bufs) ->
  BufsLen = length(Bufs),
  cast(5442, <<BufsLen:?GLuint,
        (<< <<C:?GLenum>> || C <- Bufs>>)/binary,0:(((1+BufsLen) rem 2)*32)>>).

%% @doc Set front and/or back stencil test actions
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% You draw into the stencil planes using GL drawing primitives, then render geometry and
%% images, using the stencil planes to mask out portions of the screen. Stenciling is typically
%% used in multipass rendering algorithms to achieve special effects, such as decals, outlining,
%% and constructive solid geometry rendering. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOpSeparate.xhtml">external</a> documentation.
-spec stencilOpSeparate(Face, Sfail, Dpfail, Dppass) -> 'ok' when Face :: enum(),Sfail :: enum(),Dpfail :: enum(),Dppass :: enum().
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) ->
  cast(5443, <<Face:?GLenum,Sfail:?GLenum,Dpfail:?GLenum,Dppass:?GLenum>>).

%% @doc Set front and/or back function and reference value for stencil testing
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% You draw into the stencil planes using GL drawing primitives, then render geometry and
%% images, using the stencil planes to mask out portions of the screen. Stenciling is typically
%% used in multipass rendering algorithms to achieve special effects, such as decals, outlining,
%% and constructive solid geometry rendering. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFuncSeparate.xhtml">external</a> documentation.
-spec stencilFuncSeparate(Face, Func, Ref, Mask) -> 'ok' when Face :: enum(),Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFuncSeparate(Face,Func,Ref,Mask) ->
  cast(5444, <<Face:?GLenum,Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @doc Control the front and/or back writing of individual bits in the stencil planes
%%
%% ``gl:stencilMaskSeparate'' controls the writing of individual bits in the stencil planes.
%% The least significant   n bits of  `Mask' , where   n is the number of bits in the
%% stencil buffer, specify a mask. Where a 1 appears in the mask, it's possible to write
%% to the corresponding bit in the stencil buffer. Where a 0 appears, the corresponding bit
%% is write-protected. Initially, all bits are enabled for writing. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMaskSeparate.xhtml">external</a> documentation.
-spec stencilMaskSeparate(Face, Mask) -> 'ok' when Face :: enum(),Mask :: integer().
stencilMaskSeparate(Face,Mask) ->
  cast(5445, <<Face:?GLenum,Mask:?GLuint>>).

%% @doc Attaches a shader object to a program object
%%
%% In order to create a complete shader program, there must be a way to  specify the list
%% of things that will be linked together. Program  objects provide this mechanism. Shaders
%% that are to be linked  together in a program object must first be attached to that  program
%% object. ``gl:attachShader'' attaches the  shader object specified by  `Shader'  to
%% the  program object specified by  `Program' . This  indicates that  `Shader'  will
%% be included in  link operations that will be performed on   `Program' .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glAttachShader.xhtml">external</a> documentation.
-spec attachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
attachShader(Program,Shader) ->
  cast(5446, <<Program:?GLuint,Shader:?GLuint>>).

%% @doc Associates a generic vertex attribute index with a named attribute variable
%%
%% ``gl:bindAttribLocation'' is used to associate a user-defined attribute variable in
%% the program object specified by  `Program'  with a generic vertex attribute index.
%% The name of the user-defined attribute variable is passed as a null terminated string in  `Name' 
%% . The generic vertex attribute index to be bound to this variable is specified by  `Index' 
%% . When  `Program'  is made part of current state, values provided via the generic vertex
%% attribute  `Index'  will modify the value of the user-defined attribute variable specified
%% by  `Name' .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindAttribLocation.xhtml">external</a> documentation.
-spec bindAttribLocation(Program, Index, Name) -> 'ok' when Program :: integer(),Index :: integer(),Name :: string().
bindAttribLocation(Program,Index,Name) ->
  NameLen = length(Name),
  cast(5447, <<Program:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc Compiles a shader object
%%
%% ``gl:compileShader'' compiles the source code strings that have been stored in the shader
%% object specified by  `Shader' .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompileShader.xhtml">external</a> documentation.
-spec compileShader(Shader) -> 'ok' when Shader :: integer().
compileShader(Shader) ->
  cast(5448, <<Shader:?GLuint>>).

%% @doc Creates a program object
%%
%% ``gl:createProgram'' creates an empty  program object and returns a non-zero value by
%% which it can be  referenced. A program object is an object to which shader  objects can
%% be attached. This provides a mechanism to specify  the shader objects that will be linked
%% to create a program. It  also provides a means for checking the compatibility of the 
%% shaders that will be used to create a program (for instance,  checking the compatibility
%% between a vertex shader and a  fragment shader). When no longer needed as part of a program
%%  object, shader objects can be detached.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateProgram.xhtml">external</a> documentation.
-spec createProgram() -> integer().
createProgram() ->
  call(5449, <<>>).

%% @doc Creates a shader object
%%
%% ``gl:createShader'' creates an empty  shader object and returns a non-zero value by
%% which it can be  referenced. A shader object is used to maintain the source code  strings
%% that define a shader.  `ShaderType'   indicates the type of shader to be created. Five
%% types of shader  are supported. A shader of type  `?GL_VERTEX_SHADER' is a shader
%% that is  intended to run on the programmable vertex processor.  A shader of type `?GL_TESS_CONTROL_SHADER'
%%  is a shader that  is intended to run on the programmable tessellation processor in the
%% control stage.  A shader of type `?GL_TESS_EVALUATION_SHADER' is a shader that  is
%% intended to run on the programmable tessellation processor in the evaluation stage.  A
%% shader of type  `?GL_GEOMETRY_SHADER' is a shader that is intended to  run on the
%% programmable geometry processor. A shader of  type `?GL_FRAGMENT_SHADER' is a shader
%% that is  intended to run on the programmable fragment processor.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateShader.xhtml">external</a> documentation.
-spec createShader(Type) -> integer() when Type :: enum().
createShader(Type) ->
  call(5450, <<Type:?GLenum>>).

%% @doc Deletes a program object
%%
%% ``gl:deleteProgram'' frees the memory and invalidates the name associated with the program
%% object specified by  `Program.'  This command effectively undoes the effects of a call
%% to  {@link gl:createProgram/0} .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgram.xhtml">external</a> documentation.
-spec deleteProgram(Program) -> 'ok' when Program :: integer().
deleteProgram(Program) ->
  cast(5451, <<Program:?GLuint>>).

%% @doc Deletes a shader object
%%
%% ``gl:deleteShader'' frees the memory and invalidates the name associated with the shader
%% object specified by  `Shader' . This command effectively undoes the effects of a call
%% to  {@link gl:createShader/1} .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteShader.xhtml">external</a> documentation.
-spec deleteShader(Shader) -> 'ok' when Shader :: integer().
deleteShader(Shader) ->
  cast(5452, <<Shader:?GLuint>>).

%% @doc Detaches a shader object from a program object to which it is attached
%%
%% ``gl:detachShader'' detaches the shader object specified by  `Shader'  from the program
%% object specified by  `Program' . This command can be used to undo the effect of the
%% command  {@link gl:attachShader/2} .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDetachShader.xhtml">external</a> documentation.
-spec detachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
detachShader(Program,Shader) ->
  cast(5453, <<Program:?GLuint,Shader:?GLuint>>).

%% @doc Enable or disable a generic vertex attribute array
%%
%% ``gl:enableVertexAttribArray'' enables the generic vertex attribute array specified by  `Index' 
%% . ``gl:disableVertexAttribArray'' disables the generic vertex attribute array specified
%% by  `Index' . By default, all client-side capabilities are disabled, including all
%% generic vertex attribute arrays. If enabled, the values in the generic vertex attribute
%% array will be accessed and used for rendering when calls are made to vertex array commands
%% such as  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} ,  {@link gl:drawRangeElements/6} , see `glMultiDrawElements'
%% , or  {@link gl:multiDrawArrays/3} .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml">external</a> documentation.
-spec disableVertexAttribArray(Index) -> 'ok' when Index :: integer().
disableVertexAttribArray(Index) ->
  cast(5454, <<Index:?GLuint>>).

%% @doc 
%% See {@link disableVertexAttribArray/1}
-spec enableVertexAttribArray(Index) -> 'ok' when Index :: integer().
enableVertexAttribArray(Index) ->
  cast(5455, <<Index:?GLuint>>).

%% @doc Returns information about an active attribute variable for the specified program object
%%
%% ``gl:getActiveAttrib'' returns information about an active attribute variable in the
%% program object specified by  `Program' . The number of active attributes can be obtained
%% by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_ATTRIBUTES'. A value
%% of 0 for  `Index'  selects the first active attribute variable. Permissible values
%% for  `Index'  range from 0 to the number of active attribute variables minus 1.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveAttrib.xhtml">external</a> documentation.
-spec getActiveAttrib(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveAttrib(Program,Index,BufSize) ->
  call(5456, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns information about an active uniform variable for the specified program object
%%
%% ``gl:getActiveUniform'' returns information about an active uniform variable in the
%% program object specified by  `Program' . The number of active uniform variables can
%% be obtained by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_UNIFORMS'.
%% A value of 0 for  `Index'  selects the first active uniform variable. Permissible values
%% for  `Index'  range from 0 to the number of active uniform variables minus 1.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniform.xhtml">external</a> documentation.
-spec getActiveUniform(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveUniform(Program,Index,BufSize) ->
  call(5457, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns the handles of the shader objects attached to a program object
%%
%% ``gl:getAttachedShaders'' returns the names of the shader objects attached to  `Program' 
%% . The names of shader objects that are attached to  `Program'  will be returned in  `Shaders.' 
%%  The actual number of shader names written into  `Shaders'  is returned in  `Count.' 
%%  If no shader objects are attached to  `Program' ,  `Count'  is set to 0. The maximum
%% number of shader names that may be returned in  `Shaders'  is specified by  `MaxCount' 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttachedShaders.xhtml">external</a> documentation.
-spec getAttachedShaders(Program, MaxCount) -> [integer()] when Program :: integer(),MaxCount :: integer().
getAttachedShaders(Program,MaxCount) ->
  call(5458, <<Program:?GLuint,MaxCount:?GLsizei>>).

%% @doc Returns the location of an attribute variable
%%
%% ``gl:getAttribLocation'' queries the previously linked program object specified by  `Program' 
%%  for the attribute variable specified by  `Name'  and returns the index of the generic
%% vertex attribute that is bound to that attribute variable. If  `Name'  is a matrix
%% attribute variable, the index of the first column of the matrix is returned. If the named
%% attribute variable is not an active attribute in the specified program object or if  `Name' 
%%  starts with the reserved prefix "gl_", a value of -1 is returned.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttribLocation.xhtml">external</a> documentation.
-spec getAttribLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getAttribLocation(Program,Name) ->
  NameLen = length(Name),
  call(5459, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc Returns a parameter from a program object
%%
%% ``gl:getProgram'' returns in  `Params'  the value of a parameter for a specific program
%% object. The following parameters are defined:
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml">external</a> documentation.
-spec getProgramiv(Program, Pname) -> integer() when Program :: integer(),Pname :: enum().
getProgramiv(Program,Pname) ->
  call(5460, <<Program:?GLuint,Pname:?GLenum>>).

%% @doc Returns the information log for a program object
%%
%% ``gl:getProgramInfoLog'' returns the  information log for the specified program object.
%% The  information log for a program object is modified when the  program object is linked
%% or validated. The string that is  returned will be null terminated.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramInfoLog.xhtml">external</a> documentation.
-spec getProgramInfoLog(Program, BufSize) -> string() when Program :: integer(),BufSize :: integer().
getProgramInfoLog(Program,BufSize) ->
  call(5461, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns a parameter from a shader object
%%
%% ``gl:getShader''  returns in  `Params'   the value of a parameter for a specific
%% shader object. The  following parameters are defined:
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShader.xhtml">external</a> documentation.
-spec getShaderiv(Shader, Pname) -> integer() when Shader :: integer(),Pname :: enum().
getShaderiv(Shader,Pname) ->
  call(5462, <<Shader:?GLuint,Pname:?GLenum>>).

%% @doc Returns the information log for a shader object
%%
%% ``gl:getShaderInfoLog'' returns the  information log for the specified shader object.
%% The information  log for a shader object is modified when the shader is compiled.  The
%% string that is returned will be null terminated.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderInfoLog.xhtml">external</a> documentation.
-spec getShaderInfoLog(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderInfoLog(Shader,BufSize) ->
  call(5463, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns the source code string from a shader object
%%
%% ``gl:getShaderSource'' returns the concatenation of the source code strings from the
%% shader object specified by  `Shader' . The source code strings for a shader object
%% are the result of a previous call to  {@link gl:shaderSource/2} . The string returned by
%% the function will be null terminated.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderSource.xhtml">external</a> documentation.
-spec getShaderSource(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderSource(Shader,BufSize) ->
  call(5464, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns the location of a uniform variable
%%
%% ``gl:getUniformLocation '' returns an integer that represents the location of a specific
%% uniform variable within a program object.  `Name'  must be a null terminated string
%% that contains no white space.  `Name'  must be an active uniform variable name in  `Program' 
%%  that is not a structure, an array of structures, or a subcomponent of a vector or a matrix.
%% This function returns -1 if  `Name'  does not correspond to an active uniform variable
%% in  `Program' , if  `Name'  starts with the reserved prefix "gl_", or if  `Name' 
%% is associated with an atomic counter or a named uniform block.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformLocation.xhtml">external</a> documentation.
-spec getUniformLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getUniformLocation(Program,Name) ->
  NameLen = length(Name),
  call(5465, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc Returns the value of a uniform variable
%%
%% ``gl:getUniform'' returns in  `Params'  the value(s) of the specified uniform variable.
%% The type of the uniform variable specified by  `Location'  determines the number of
%% values returned. If the uniform variable is defined in the shader as a boolean, int, or
%% float, a single value will be returned. If it is defined as a vec2, ivec2, or bvec2, two
%% values will be returned. If it is defined as a vec3, ivec3, or bvec3, three values will
%% be returned, and so on. To query values stored in uniform variables declared as arrays,
%% call ``gl:getUniform'' for each element of the array. To query values stored in uniform
%% variables declared as structures, call ``gl:getUniform'' for each field in the structure.
%% The values for uniform variables declared as a matrix will be returned in column major
%% order.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniform.xhtml">external</a> documentation.
-spec getUniformfv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformfv(Program,Location) ->
  call(5466, <<Program:?GLuint,Location:?GLint>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformiv(Program,Location) ->
  call(5467, <<Program:?GLuint,Location:?GLint>>).

%% @doc Return a generic vertex attribute parameter
%%
%% ``gl:getVertexAttrib'' returns in  `Params'  the value of a generic vertex attribute
%% parameter. The generic vertex attribute to be queried is specified by  `Index' , and
%% the parameter to be queried is specified by  `Pname' .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetVertexAttrib.xhtml">external</a> documentation.
-spec getVertexAttribdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribdv(Index,Pname) ->
  call(5468, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribfv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribfv(Index,Pname) ->
  call(5469, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribiv(Index,Pname) ->
  call(5470, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc Determines if a name corresponds to a program object
%%
%% ``gl:isProgram'' returns `?GL_TRUE' if  `Program'  is the name of a program
%% object previously created with   {@link gl:createProgram/0}   and not yet deleted with  {@link gl:deleteProgram/1} 
%% . If  `Program'  is zero or a non-zero value that is not the name of a program object,
%% or if an error occurs,  ``gl:isProgram'' returns `?GL_FALSE'.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgram.xhtml">external</a> documentation.
-spec isProgram(Program) -> 0|1 when Program :: integer().
isProgram(Program) ->
  call(5471, <<Program:?GLuint>>).

%% @doc Determines if a name corresponds to a shader object
%%
%% ``gl:isShader'' returns `?GL_TRUE' if  `Shader'  is the name of a shader object
%% previously created with   {@link gl:createShader/1}   and not yet deleted with  {@link gl:deleteShader/1} 
%% .  If  `Shader'  is zero or a non-zero value that is not the name of a shader object,
%% or if an error occurs, ``gl:isShader '' returns `?GL_FALSE'.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsShader.xhtml">external</a> documentation.
-spec isShader(Shader) -> 0|1 when Shader :: integer().
isShader(Shader) ->
  call(5472, <<Shader:?GLuint>>).

%% @doc Links a program object
%%
%% ``gl:linkProgram'' links the program object specified by  `Program' . If any shader
%% objects of type `?GL_VERTEX_SHADER' are attached to  `Program' , they will be
%% used to create an executable that will run on the programmable vertex processor. If any
%% shader objects of type `?GL_GEOMETRY_SHADER' are attached to  `Program' , they
%% will be used to create an executable that will run on the programmable geometry processor.
%% If any shader objects of type `?GL_FRAGMENT_SHADER' are attached to  `Program' ,
%% they will be used to create an executable that will run on the programmable fragment processor.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLinkProgram.xhtml">external</a> documentation.
-spec linkProgram(Program) -> 'ok' when Program :: integer().
linkProgram(Program) ->
  cast(5473, <<Program:?GLuint>>).

%% @doc Replaces the source code in a shader object
%%
%% ``gl:shaderSource'' sets the source code  in  `Shader'  to the source code in the
%% array  of strings specified by  `String' . Any  source code previously stored in the
%% shader object is completely  replaced. The number of strings in the array is specified
%% by   `Count' . If  `Length'   is `?NULL', each string is assumed to be null
%%  terminated. If  `Length'  is a value other  than `?NULL', it points to an array
%% containing  a string length for each of the corresponding elements of   `String' .
%% Each element in the   `Length'  array may contain the length of  the corresponding
%% string (the null character is not counted as  part of the string length) or a value less
%% than 0 to indicate  that the string is null terminated. The source code strings are  not
%% scanned or parsed at this time; they are simply copied into  the specified shader object.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderSource.xhtml">external</a> documentation.
-spec shaderSource(Shader, String) -> 'ok' when Shader :: integer(),String :: iolist().
shaderSource(Shader,String) ->
  StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  StringLen = length(String),
  cast(5474, <<Shader:?GLuint,StringLen:?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+0) rem 8)) rem 8)>>).

%% @doc Installs a program object as part of current rendering state
%%
%% ``gl:useProgram'' installs the program  object specified by  `Program'  as part of
%%  current rendering state. One or more executables are created in  a program object by
%% successfully attaching shader objects to it  with   {@link gl:attachShader/2} ,  successfully
%% compiling the shader objects with   {@link gl:compileShader/1} ,  and successfully linking
%% the program object with   {@link gl:linkProgram/1} .  
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgram.xhtml">external</a> documentation.
-spec useProgram(Program) -> 'ok' when Program :: integer().
useProgram(Program) ->
  cast(5475, <<Program:?GLuint>>).

%% @doc Specify the value of a uniform variable for the current program object
%%
%% ``gl:uniform'' modifies the value of a uniform variable or a uniform variable array.
%% The location of the uniform variable to be modified is specified by  `Location' , which
%% should be a value returned by  {@link gl:getUniformLocation/2} . ``gl:uniform'' operates
%% on the program object that was made part of current state by calling  {@link gl:useProgram/1} 
%% .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml">external</a> documentation.
-spec uniform1f(Location, V0) -> 'ok' when Location :: integer(),V0 :: float().
uniform1f(Location,V0) ->
  cast(5476, <<Location:?GLint,V0:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2f(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float().
uniform2f(Location,V0,V1) ->
  cast(5477, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3f(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
uniform3f(Location,V0,V1,V2) ->
  cast(5478, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4f(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
uniform4f(Location,V0,V1,V2,V3) ->
  cast(5479, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1i(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1i(Location,V0) ->
  cast(5480, <<Location:?GLint,V0:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2i(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2i(Location,V0,V1) ->
  cast(5481, <<Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3i(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3i(Location,V0,V1,V2) ->
  cast(5482, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4i(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4i(Location,V0,V1,V2,V3) ->
  cast(5483, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1fv(Location,Value) ->
  ValueLen = length(Value),
  cast(5484, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2fv(Location,Value) ->
  ValueLen = length(Value),
  cast(5485, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3fv(Location,Value) ->
  ValueLen = length(Value),
  cast(5486, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4fv(Location,Value) ->
  ValueLen = length(Value),
  cast(5487, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1iv(Location,Value) ->
  ValueLen = length(Value),
  cast(5488, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2iv(Location,Value) ->
  ValueLen = length(Value),
  cast(5489, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3iv(Location,Value) ->
  ValueLen = length(Value),
  cast(5490, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4iv(Location,Value) ->
  ValueLen = length(Value),
  cast(5491, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5492, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5493, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5494, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc Validates a program object
%%
%% ``gl:validateProgram'' checks to see  whether the executables contained in   `Program' 
%%  can execute given the current  OpenGL state. The information generated by the validation
%%  process will be stored in  `Program' 's  information log. The validation information
%% may consist of an  empty string, or it may be a string containing information about  how
%% the current program object interacts with the rest of  current OpenGL state. This provides
%% a way for OpenGL  implementers to convey more information about why the current  program
%% is inefficient, suboptimal, failing to execute, and so  on.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgram.xhtml">external</a> documentation.
-spec validateProgram(Program) -> 'ok' when Program :: integer().
validateProgram(Program) ->
  cast(5495, <<Program:?GLuint>>).

%% @doc Specifies the value of a generic vertex attribute
%%
%% The ``gl:vertexAttrib'' family of entry points allows an application to pass generic
%% vertex attributes in numbered locations.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttrib.xhtml">external</a> documentation.
-spec vertexAttrib1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1d(Index,X) ->
  cast(5496, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @equiv vertexAttrib1d(Index,X)
-spec vertexAttrib1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib1f(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1f(Index,X) ->
  cast(5497, <<Index:?GLuint,X:?GLfloat>>).

%% @equiv vertexAttrib1f(Index,X)
-spec vertexAttrib1fv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib1s(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttrib1s(Index,X) ->
  cast(5498, <<Index:?GLuint,X:?GLshort>>).

%% @equiv vertexAttrib1s(Index,X)
-spec vertexAttrib1sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2d(Index,X,Y) ->
  cast(5499, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @equiv vertexAttrib2d(Index,X,Y)
-spec vertexAttrib2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2f(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2f(Index,X,Y) ->
  cast(5500, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat>>).

%% @equiv vertexAttrib2f(Index,X,Y)
-spec vertexAttrib2fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2s(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttrib2s(Index,X,Y) ->
  cast(5501, <<Index:?GLuint,X:?GLshort,Y:?GLshort>>).

%% @equiv vertexAttrib2s(Index,X,Y)
-spec vertexAttrib2sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3d(Index,X,Y,Z) ->
  cast(5502, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @equiv vertexAttrib3d(Index,X,Y,Z)
-spec vertexAttrib3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3f(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3f(Index,X,Y,Z) ->
  cast(5503, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @equiv vertexAttrib3f(Index,X,Y,Z)
-spec vertexAttrib3fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3s(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttrib3s(Index,X,Y,Z) ->
  cast(5504, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @equiv vertexAttrib3s(Index,X,Y,Z)
-spec vertexAttrib3sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nbv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nbv(Index,{V1,V2,V3,V4}) ->
  cast(5505, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Niv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Niv(Index,{V1,V2,V3,V4}) ->
  cast(5506, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nsv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nsv(Index,{V1,V2,V3,V4}) ->
  cast(5507, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nub(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4Nub(Index,X,Y,Z,W) ->
  cast(5508, <<Index:?GLuint,X:?GLubyte,Y:?GLubyte,Z:?GLubyte,W:?GLubyte>>).

%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
-spec vertexAttrib4Nubv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nuiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nuiv(Index,{V1,V2,V3,V4}) ->
  cast(5509, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nusv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nusv(Index,{V1,V2,V3,V4}) ->
  cast(5510, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4bv(Index,{V1,V2,V3,V4}) ->
  cast(5511, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4d(Index,X,Y,Z,W) ->
  cast(5512, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
-spec vertexAttrib4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4f(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4f(Index,X,Y,Z,W) ->
  cast(5513, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
-spec vertexAttrib4fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4iv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4iv(Index,{V1,V2,V3,V4}) ->
  cast(5514, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4s(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4s(Index,X,Y,Z,W) ->
  cast(5515, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
-spec vertexAttrib4sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5516, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4uiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4uiv(Index,{V1,V2,V3,V4}) ->
  cast(5517, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4usv(Index,{V1,V2,V3,V4}) ->
  cast(5518, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc Define an array of generic vertex attribute data
%%
%% ``gl:vertexAttribPointer'', ``gl:vertexAttribIPointer'' and ``gl:vertexAttribLPointer''
%%  specify the location and data format of the array of generic vertex attributes at index  `Index' 
%%  to use when rendering.  `Size'  specifies the number of components per attribute and
%% must be 1, 2, 3, 4, or `?GL_BGRA'.  `Type'  specifies the data type of each component,
%% and  `Stride'  specifies the byte stride from one attribute to the next, allowing vertices
%% and attributes to be packed into a single array or stored in separate arrays. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribPointer.xhtml">external</a> documentation.
-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Stride :: integer(),Pointer :: offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5519, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5520, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5521, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5522, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5523, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5524, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5525, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3fv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5526, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc glColorMaski
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec colorMaski(Index, R, G, B, A) -> 'ok' when Index :: integer(),R :: 0|1,G :: 0|1,B :: 0|1,A :: 0|1.
colorMaski(Index,R,G,B,A) ->
  cast(5527, <<Index:?GLuint,R:?GLboolean,G:?GLboolean,B:?GLboolean,A:?GLboolean>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getBooleani_v(Target, Index) -> [0|1] when Target :: enum(),Index :: integer().
getBooleani_v(Target,Index) ->
  call(5528, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getIntegeri_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getIntegeri_v(Target,Index) ->
  call(5529, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link enable/1}
-spec enablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
enablei(Target,Index) ->
  cast(5530, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glEnablei
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
disablei(Target,Index) ->
  cast(5531, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glIsEnabledi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isEnabledi(Target, Index) -> 0|1 when Target :: enum(),Index :: integer().
isEnabledi(Target,Index) ->
  call(5532, <<Target:?GLenum,Index:?GLuint>>).

%% @doc Start transform feedback operation
%%
%%  Transform feedback mode captures the values of varying variables written by the vertex
%% shader (or, if active, the geometry shader). Transform feedback is said to be active after
%% a call to ``gl:beginTransformFeedback'' until a subsequent call to  {@link gl:beginTransformFeedback/1} 
%% . Transform feedback commands must be paired. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginTransformFeedback.xhtml">external</a> documentation.
-spec beginTransformFeedback(PrimitiveMode) -> 'ok' when PrimitiveMode :: enum().
beginTransformFeedback(PrimitiveMode) ->
  cast(5533, <<PrimitiveMode:?GLenum>>).

%% @doc 
%% See {@link beginTransformFeedback/1}
-spec endTransformFeedback() -> 'ok'.
endTransformFeedback() ->
  cast(5534, <<>>).

%% @doc Bind a range within a buffer object to an indexed buffer target
%%
%% ``gl:bindBufferRange'' binds a range the buffer object  `Buffer'  represented by  `Offset' 
%%  and  `Size'  to the binding point at index  `Index'  of the array of targets specified
%% by  `Target' . Each  `Target'  represents an indexed array of buffer binding points,
%% as well as a single general binding point that can be used by other buffer manipulation
%% functions such as  {@link gl:bindBuffer/2}  or see `glMapBuffer'. In addition to binding
%% a range of  `Buffer'  to the indexed buffer binding target, ``gl:bindBufferBase''
%% also binds the range to the generic buffer binding point specified by  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferRange.xhtml">external</a> documentation.
-spec bindBufferRange(Target, Index, Buffer, Offset, Size) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer(),Offset :: integer(),Size :: integer().
bindBufferRange(Target,Index,Buffer,Offset,Size) ->
  cast(5535, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Bind a buffer object to an indexed buffer target
%%
%% ``gl:bindBufferBase'' binds the buffer object  `Buffer'  to the binding point at
%% index  `Index'  of the array of targets specified by  `Target' . Each  `Target' 
%% represents an indexed array of buffer binding points, as well as a single general binding
%% point that can be used by other buffer manipulation functions such as  {@link gl:bindBuffer/2} 
%%  or see `glMapBuffer'. In addition to binding  `Buffer'  to the indexed buffer
%% binding target, ``gl:bindBufferBase'' also binds  `Buffer'  to the generic buffer
%% binding point specified by  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferBase.xhtml">external</a> documentation.
-spec bindBufferBase(Target, Index, Buffer) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer().
bindBufferBase(Target,Index,Buffer) ->
  cast(5536, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint>>).

%% @doc Specify values to record in transform feedback buffers
%%
%%  The names of the vertex or geometry shader outputs to be recorded in transform feedback
%% mode are specified using ``gl:transformFeedbackVaryings''. When a geometry shader is
%% active, transform feedback records the values of selected geometry shader output variables
%% from the emitted vertices. Otherwise, the values of the selected vertex shader outputs
%% are recorded. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackVaryings.xhtml">external</a> documentation.
-spec transformFeedbackVaryings(Program, Varyings, BufferMode) -> 'ok' when Program :: integer(),Varyings :: iolist(),BufferMode :: enum().
transformFeedbackVaryings(Program,Varyings,BufferMode) ->
  VaryingsTemp = list_to_binary([[Str|[0]] || Str <- Varyings ]),
  VaryingsLen = length(Varyings),
  cast(5537, <<Program:?GLuint,VaryingsLen:?GLuint,(size(VaryingsTemp)):?GLuint,(VaryingsTemp)/binary,0:((8-((size(VaryingsTemp)+0) rem 8)) rem 8),BufferMode:?GLenum>>).

%% @doc Retrieve information about varying variables selected for transform feedback
%%
%%  Information about the set of varying variables in a linked program that will be captured
%% during transform feedback may be retrieved by calling ``gl:getTransformFeedbackVarying''.
%% ``gl:getTransformFeedbackVarying'' provides information about the varying variable selected
%% by  `Index' . An  `Index'  of 0 selects the first varying variable specified in
%% the  `Varyings'  array passed to  {@link gl:transformFeedbackVaryings/3} , and an  `Index' 
%%  of `?GL_TRANSFORM_FEEDBACK_VARYINGS-1' selects the last such variable. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTransformFeedbackVarying.xhtml">external</a> documentation.
-spec getTransformFeedbackVarying(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getTransformFeedbackVarying(Program,Index,BufSize) ->
  call(5538, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @doc specify whether data read via 
%%
%%  {@link gl:readPixels/7}  should be clamped
%%
%% ``gl:clampColor'' controls color clamping that is performed during  {@link gl:readPixels/7} 
%% .  `Target'  must be `?GL_CLAMP_READ_COLOR'. If  `Clamp'  is `?GL_TRUE',
%% read color clamping is enabled; if  `Clamp'  is `?GL_FALSE', read color clamping
%% is disabled. If  `Clamp'  is `?GL_FIXED_ONLY', read color clamping is enabled
%% only if the selected read buffer has fixed point components and disabled otherwise. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClampColor.xhtml">external</a> documentation.
-spec clampColor(Target, Clamp) -> 'ok' when Target :: enum(),Clamp :: enum().
clampColor(Target,Clamp) ->
  cast(5539, <<Target:?GLenum,Clamp:?GLenum>>).

%% @doc Start conditional rendering
%%
%%  Conditional rendering is started using ``gl:beginConditionalRender'' and ended using ``gl:endConditionalRender''
%% . During conditional rendering, all vertex array commands, as well as  {@link gl:clear/1} 
%% and  {@link gl:clearBufferiv/3}  have no effect if the (`?GL_SAMPLES_PASSED') result of
%% the query object  `Id'  is zero, or if the (`?GL_ANY_SAMPLES_PASSED') result is `?GL_FALSE'
%% . The results of commands setting the current vertex state, such as  {@link gl:vertexAttrib1d/2} 
%%  are undefined. If the (`?GL_SAMPLES_PASSED') result is non-zero or if the (`?GL_ANY_SAMPLES_PASSED'
%% ) result is `?GL_TRUE', such commands are not discarded. The  `Id'  parameter to ``gl:beginConditionalRender''
%%  must be the name of a query object previously returned from a call to  {@link gl:genQueries/1} 
%% .  `Mode'  specifies how the results of the query object are to be interpreted. If  `Mode' 
%%  is `?GL_QUERY_WAIT', the GL waits for the results of the query to be available and
%% then uses the results to determine if subsequent rendering commands are discarded. If  `Mode' 
%%  is `?GL_QUERY_NO_WAIT', the GL may choose to unconditionally execute the subsequent
%% rendering commands without waiting for the query to complete. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginConditionalRender.xhtml">external</a> documentation.
-spec beginConditionalRender(Id, Mode) -> 'ok' when Id :: integer(),Mode :: enum().
beginConditionalRender(Id,Mode) ->
  cast(5540, <<Id:?GLuint,Mode:?GLenum>>).

%% @doc 
%% See {@link beginConditionalRender/2}
-spec endConditionalRender() -> 'ok'.
endConditionalRender() ->
  cast(5541, <<>>).

%% @doc glVertexAttribIPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribIPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5542, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5543, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribIiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIiv(Index,Pname) ->
  call(5544, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc glGetVertexAttribI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribIuiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIuiv(Index,Pname) ->
  call(5545, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI1i(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1i(Index,X) ->
  cast(5546, <<Index:?GLuint,X:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI2i(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2i(Index,X,Y) ->
  cast(5547, <<Index:?GLuint,X:?GLint,Y:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI3i(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3i(Index,X,Y,Z) ->
  cast(5548, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4i(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4i(Index,X,Y,Z,W) ->
  cast(5549, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI1ui(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1ui(Index,X) ->
  cast(5550, <<Index:?GLuint,X:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI2ui(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2ui(Index,X,Y) ->
  cast(5551, <<Index:?GLuint,X:?GLuint,Y:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI3ui(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3ui(Index,X,Y,Z) ->
  cast(5552, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4ui(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4ui(Index,X,Y,Z,W) ->
  cast(5553, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint,W:?GLuint>>).

%% @equiv vertexAttribI1i(Index,X)
-spec vertexAttribI1iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).

%% @equiv vertexAttribI2i(Index,X,Y)
-spec vertexAttribI2iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).

%% @equiv vertexAttribI3i(Index,X,Y,Z)
-spec vertexAttribI3iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).

%% @equiv vertexAttribI4i(Index,X,Y,Z,W)
-spec vertexAttribI4iv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).

%% @equiv vertexAttribI1ui(Index,X)
-spec vertexAttribI1uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).

%% @equiv vertexAttribI2ui(Index,X,Y)
-spec vertexAttribI2uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).

%% @equiv vertexAttribI3ui(Index,X,Y,Z)
-spec vertexAttribI3uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).

%% @equiv vertexAttribI4ui(Index,X,Y,Z,W)
-spec vertexAttribI4uiv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4bv(Index,{V1,V2,V3,V4}) ->
  cast(5554, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4sv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4sv(Index,{V1,V2,V3,V4}) ->
  cast(5555, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5556, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4usv(Index,{V1,V2,V3,V4}) ->
  cast(5557, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformuiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformuiv(Program,Location) ->
  call(5558, <<Program:?GLuint,Location:?GLint>>).

%% @doc Bind a user-defined varying out variable to a fragment shader color number
%%
%% ``gl:bindFragDataLocation'' explicitly specifies the binding of the user-defined varying
%% out variable  `Name'  to fragment shader color number  `ColorNumber'  for program  `Program' 
%% . If  `Name'  was bound previously, its assigned binding is replaced with  `ColorNumber' 
%% .  `Name'  must be a null-terminated string.  `ColorNumber'  must be less than `?GL_MAX_DRAW_BUFFERS'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFragDataLocation.xhtml">external</a> documentation.
-spec bindFragDataLocation(Program, Color, Name) -> 'ok' when Program :: integer(),Color :: integer(),Name :: string().
bindFragDataLocation(Program,Color,Name) ->
  NameLen = length(Name),
  cast(5559, <<Program:?GLuint,Color:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc Query the bindings of color numbers to user-defined varying out variables
%%
%% ``gl:getFragDataLocation'' retrieves the assigned color number binding for the user-defined
%% varying out variable  `Name'  for program  `Program' .  `Program'  must have
%% previously been linked.  `Name'  must be a null-terminated string. If  `Name'  is
%% not the name of an active user-defined varying out fragment shader variable within  `Program' 
%% , -1 will be returned. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataLocation.xhtml">external</a> documentation.
-spec getFragDataLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataLocation(Program,Name) ->
  NameLen = length(Name),
  call(5560, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1ui(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1ui(Location,V0) ->
  cast(5561, <<Location:?GLint,V0:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2ui(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2ui(Location,V0,V1) ->
  cast(5562, <<Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3ui(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3ui(Location,V0,V1,V2) ->
  cast(5563, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4ui(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4ui(Location,V0,V1,V2,V3) ->
  cast(5564, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1uiv(Location,Value) ->
  ValueLen = length(Value),
  cast(5565, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2uiv(Location,Value) ->
  ValueLen = length(Value),
  cast(5566, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3uiv(Location,Value) ->
  ValueLen = length(Value),
  cast(5567, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4uiv(Location,Value) ->
  ValueLen = length(Value),
  cast(5568, <<Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link texParameterf/3}
-spec texParameterIiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIiv(Target,Pname,Params) ->
  cast(5569, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc glTexParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec texParameterIuiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIuiv(Target,Pname,Params) ->
  cast(5570, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link getTexParameterfv/2}
-spec getTexParameterIiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIiv(Target,Pname) ->
  call(5571, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glGetTexParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getTexParameterIuiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIuiv(Target,Pname) ->
  call(5572, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Clear individual buffers of the currently bound draw framebuffer
%%
%% ``gl:clearBuffer*'' clears the specified buffer to the specified value(s). If  `Buffer' 
%%  is `?GL_COLOR', a particular draw buffer `?GL_DRAWBUFFER' `I'  is specified
%% by passing  `I'  as  `DrawBuffer' . In this case,  `Value'  points to a four-element
%% vector specifying the R, G, B and A color to clear that draw buffer to. If  `Buffer' 
%% is one of `?GL_FRONT', `?GL_BACK', `?GL_LEFT', `?GL_RIGHT', or `?GL_FRONT_AND_BACK'
%% , identifying multiple buffers, each selected buffer is cleared to the same value. Clamping
%% and conversion for fixed-point color buffers are performed in the same fashion as  {@link gl:clearColor/4} 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearBuffer.xhtml">external</a> documentation.
-spec clearBufferiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferiv(Buffer,Drawbuffer,Value) ->
  cast(5573, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link clearBufferiv/3}
-spec clearBufferuiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferuiv(Buffer,Drawbuffer,Value) ->
  cast(5574, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link clearBufferiv/3}
-spec clearBufferfv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferfv(Buffer,Drawbuffer,Value) ->
  cast(5575, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc glClearBufferfi
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearBufferfi(Buffer, Drawbuffer, Depth, Stencil) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Depth :: float(),Stencil :: integer().
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) ->
  cast(5576, <<Buffer:?GLenum,Drawbuffer:?GLint,Depth:?GLfloat,Stencil:?GLint>>).

%% @doc 
%% See {@link getString/1}
-spec getStringi(Name, Index) -> string() when Name :: enum(),Index :: integer().
getStringi(Name,Index) ->
  call(5577, <<Name:?GLenum,Index:?GLuint>>).

%% @doc glDrawArraysInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawArraysInstanced(Mode, First, Count, Primcount) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer().
drawArraysInstanced(Mode,First,Count,Primcount) ->
  cast(5578, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei>>).

%% @doc glDrawElementsInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawElementsInstanced(Mode, Count, Type, Indices, Primcount) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer().
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) when  is_integer(Indices) ->
  cast(5579, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei>>);
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) ->
  send_bin(Indices),
  cast(5580, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei>>).

%% @doc Attach the storage for a buffer object to the active buffer texture
%%
%% ``gl:texBuffer'' attaches the storage for the buffer object named  `Buffer'  to the
%% active buffer texture, and specifies the internal format for the texel array found in
%% the attached buffer object. If  `Buffer'  is zero, any buffer object attached to the
%% buffer texture is detached and no new buffer object is attached. If  `Buffer'  is non-zero,
%% it must be the name of an existing buffer object.  `Target'  must be `?GL_TEXTURE_BUFFER'
%% .  `Internalformat'  specifies the storage format, and must be one of the following
%% sized internal formats: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexBuffer.xhtml">external</a> documentation.
-spec texBuffer(Target, Internalformat, Buffer) -> 'ok' when Target :: enum(),Internalformat :: enum(),Buffer :: integer().
texBuffer(Target,Internalformat,Buffer) ->
  cast(5581, <<Target:?GLenum,Internalformat:?GLenum,Buffer:?GLuint>>).

%% @doc Specify the primitive restart index
%%
%% ``gl:primitiveRestartIndex'' specifies a vertex array element that is treated specially
%% when primitive restarting is enabled. This is known as the primitive restart index. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPrimitiveRestartIndex.xhtml">external</a> documentation.
-spec primitiveRestartIndex(Index) -> 'ok' when Index :: integer().
primitiveRestartIndex(Index) ->
  cast(5582, <<Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getInteger64i_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getInteger64i_v(Target,Index) ->
  call(5583, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetBufferParameteri64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferParameteri64v(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameteri64v(Target,Pname) ->
  call(5584, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Attach a level of a texture object as a logical buffer to the currently bound framebuffer object
%%
%% ``gl:framebufferTexture'', ``gl:framebufferTexture1D'', ``gl:framebufferTexture2D'',
%% and ``gl:framebufferTexture'' attach a selected mipmap level or image of a texture object
%% as one of the logical buffers of the framebuffer object currently bound to  `Target' .
%%  `Target'  must be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER', or `?GL_FRAMEBUFFER'
%% . `?GL_FRAMEBUFFER' is equivalent to `?GL_DRAW_FRAMEBUFFER'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferTexture.xhtml">external</a> documentation.
-spec framebufferTexture(Target, Attachment, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture(Target,Attachment,Texture,Level) ->
  cast(5585, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc Modify the rate at which generic vertex attributes advance during instanced rendering
%%
%% ``gl:vertexAttribDivisor'' modifies the rate at which generic vertex attributes advance
%% when rendering multiple instances of primitives in a single draw call. If  `Divisor' 
%% is zero, the attribute at slot  `Index'  advances once per vertex. If  `Divisor' 
%% is non-zero, the attribute advances once per  `Divisor'  instances of the set(s) of
%% vertices being rendered. An attribute is referred to as instanced if its `?GL_VERTEX_ATTRIB_ARRAY_DIVISOR'
%%  value is non-zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribDivisor.xhtml">external</a> documentation.
-spec vertexAttribDivisor(Index, Divisor) -> 'ok' when Index :: integer(),Divisor :: integer().
vertexAttribDivisor(Index,Divisor) ->
  cast(5586, <<Index:?GLuint,Divisor:?GLuint>>).

%% @doc Specifies minimum rate at which sample shaing takes place
%%
%% ``gl:minSampleShading'' specifies the rate at which samples are shaded within a covered
%% pixel. Sample-rate shading is enabled by calling  {@link gl:enable/1}  with the parameter `?GL_SAMPLE_SHADING'
%% . If `?GL_MULTISAMPLE' or `?GL_SAMPLE_SHADING' is disabled, sample shading has
%% no effect. Otherwise, an implementation must provide at least as many unique color values
%% for each covered fragment as specified by  `Value'  times  `Samples'  where  `Samples' 
%%  is the value of `?GL_SAMPLES' for the current framebuffer. At least 1 sample for
%% each covered fragment is generated. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMinSampleShading.xhtml">external</a> documentation.
-spec minSampleShading(Value) -> 'ok' when Value :: clamp().
minSampleShading(Value) ->
  cast(5587, <<Value:?GLclampf>>).

%% @doc 
%% See {@link blendEquation/1}
-spec blendEquationi(Buf, Mode) -> 'ok' when Buf :: integer(),Mode :: enum().
blendEquationi(Buf,Mode) ->
  cast(5588, <<Buf:?GLuint,Mode:?GLenum>>).

%% @doc 
%% See {@link blendEquationSeparate/2}
-spec blendEquationSeparatei(Buf, ModeRGB, ModeAlpha) -> 'ok' when Buf :: integer(),ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) ->
  cast(5589, <<Buf:?GLuint,ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @doc glBlendFunci
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec blendFunci(Buf, Src, Dst) -> 'ok' when Buf :: integer(),Src :: enum(),Dst :: enum().
blendFunci(Buf,Src,Dst) ->
  cast(5590, <<Buf:?GLuint,Src:?GLenum,Dst:?GLenum>>).

%% @doc 
%% See {@link blendFuncSeparate/4}
-spec blendFuncSeparatei(Buf, SrcRGB, DstRGB, SrcAlpha, DstAlpha) -> 'ok' when Buf :: integer(),SrcRGB :: enum(),DstRGB :: enum(),SrcAlpha :: enum(),DstAlpha :: enum().
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) ->
  cast(5591, <<Buf:?GLuint,SrcRGB:?GLenum,DstRGB:?GLenum,SrcAlpha:?GLenum,DstAlpha:?GLenum>>).

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5592, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5592, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec loadTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5593, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5593, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5594, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5594, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec multTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5595, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5595, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightbvARB(Weights) -> 'ok' when Weights :: [integer()].
weightbvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5596, <<WeightsLen:?GLuint,
        (<< <<C:?GLbyte>> || C <- Weights>>)/binary,0:((8-((WeightsLen+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightsvARB(Weights) -> 'ok' when Weights :: [integer()].
weightsvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5597, <<WeightsLen:?GLuint,
        (<< <<C:?GLshort>> || C <- Weights>>)/binary,0:((8-((WeightsLen*2+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightivARB(Weights) -> 'ok' when Weights :: [integer()].
weightivARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5598, <<WeightsLen:?GLuint,
        (<< <<C:?GLint>> || C <- Weights>>)/binary,0:(((1+WeightsLen) rem 2)*32)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightfvARB(Weights) -> 'ok' when Weights :: [float()].
weightfvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5599, <<WeightsLen:?GLuint,
        (<< <<C:?GLfloat>> || C <- Weights>>)/binary,0:(((1+WeightsLen) rem 2)*32)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightdvARB(Weights) -> 'ok' when Weights :: [float()].
weightdvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5600, <<WeightsLen:?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Weights>>)/binary>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightubvARB(Weights) -> 'ok' when Weights :: [integer()].
weightubvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5601, <<WeightsLen:?GLuint,
        (<< <<C:?GLubyte>> || C <- Weights>>)/binary,0:((8-((WeightsLen+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightusvARB(Weights) -> 'ok' when Weights :: [integer()].
weightusvARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5602, <<WeightsLen:?GLuint,
        (<< <<C:?GLushort>> || C <- Weights>>)/binary,0:((8-((WeightsLen*2+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec weightuivARB(Weights) -> 'ok' when Weights :: [integer()].
weightuivARB(Weights) ->
  WeightsLen = length(Weights),
  cast(5603, <<WeightsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Weights>>)/binary,0:(((1+WeightsLen) rem 2)*32)>>).

%% @doc glVertexBlenARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexBlendARB(Count) -> 'ok' when Count :: integer().
vertexBlendARB(Count) ->
  cast(5604, <<Count:?GLint>>).

%% @doc glCurrentPaletteMatrixARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec currentPaletteMatrixARB(Index) -> 'ok' when Index :: integer().
currentPaletteMatrixARB(Index) ->
  cast(5605, <<Index:?GLint>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexubvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexubvARB(Indices) ->
  IndicesLen = length(Indices),
  cast(5606, <<IndicesLen:?GLuint,
        (<< <<C:?GLubyte>> || C <- Indices>>)/binary,0:((8-((IndicesLen+ 4) rem 8)) rem 8)>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexusvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexusvARB(Indices) ->
  IndicesLen = length(Indices),
  cast(5607, <<IndicesLen:?GLuint,
        (<< <<C:?GLushort>> || C <- Indices>>)/binary,0:((8-((IndicesLen*2+ 4) rem 8)) rem 8)>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec matrixIndexuivARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexuivARB(Indices) ->
  IndicesLen = length(Indices),
  cast(5608, <<IndicesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((1+IndicesLen) rem 2)*32)>>).

%% @doc glProgramStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programStringARB(Target, Format, String) -> 'ok' when Target :: enum(),Format :: enum(),String :: string().
programStringARB(Target,Format,String) ->
  StringLen = length(String),
  cast(5609, <<Target:?GLenum,Format:?GLenum,(list_to_binary([String|[0]]))/binary,0:((8-((StringLen+ 1) rem 8)) rem 8)>>).

%% @doc glBindProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindProgramARB(Target, Program) -> 'ok' when Target :: enum(),Program :: integer().
bindProgramARB(Target,Program) ->
  cast(5610, <<Target:?GLenum,Program:?GLuint>>).

%% @doc glDeleteProgramsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteProgramsARB(Programs) -> 'ok' when Programs :: [integer()].
deleteProgramsARB(Programs) ->
  ProgramsLen = length(Programs),
  cast(5611, <<ProgramsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Programs>>)/binary,0:(((1+ProgramsLen) rem 2)*32)>>).

%% @doc glGenProgramsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec genProgramsARB(N) -> [integer()] when N :: integer().
genProgramsARB(N) ->
  call(5612, <<N:?GLsizei>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5613, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5614, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5615, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programEnvParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5616, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5617, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5618, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5619, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec programLocalParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5620, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramEnvParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterdvARB(Target,Index) ->
  call(5621, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramEnvParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterfvARB(Target,Index) ->
  call(5622, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramLocalParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterdvARB(Target,Index) ->
  call(5623, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramLocalParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterfvARB(Target,Index) ->
  call(5624, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getProgramStringARB(Target, Pname, String) -> 'ok' when Target :: enum(),Pname :: enum(),String :: mem().
getProgramStringARB(Target,Pname,String) ->
  send_bin(String),
  call(5625, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glGetBufferParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getBufferParameterivARB(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameterivARB(Target,Pname) ->
  call(5626, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glDeleteObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteObjectARB(Obj) -> 'ok' when Obj :: integer().
deleteObjectARB(Obj) ->
  cast(5627, <<Obj:?GLhandleARB>>).

%% @doc glGetHandleARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getHandleARB(Pname) -> integer() when Pname :: enum().
getHandleARB(Pname) ->
  call(5628, <<Pname:?GLenum>>).

%% @doc glDetachObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec detachObjectARB(ContainerObj, AttachedObj) -> 'ok' when ContainerObj :: integer(),AttachedObj :: integer().
detachObjectARB(ContainerObj,AttachedObj) ->
  cast(5629, <<ContainerObj:?GLhandleARB,AttachedObj:?GLhandleARB>>).

%% @doc glCreateShaderObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createShaderObjectARB(ShaderType) -> integer() when ShaderType :: enum().
createShaderObjectARB(ShaderType) ->
  call(5630, <<ShaderType:?GLenum>>).

%% @doc glShaderSourceARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec shaderSourceARB(ShaderObj, String) -> 'ok' when ShaderObj :: integer(),String :: iolist().
shaderSourceARB(ShaderObj,String) ->
  StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  StringLen = length(String),
  cast(5631, <<ShaderObj:?GLhandleARB,StringLen:?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+4) rem 8)) rem 8)>>).

%% @doc glCompileShaderARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compileShaderARB(ShaderObj) -> 'ok' when ShaderObj :: integer().
compileShaderARB(ShaderObj) ->
  cast(5632, <<ShaderObj:?GLhandleARB>>).

%% @doc glCreateProgramObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createProgramObjectARB() -> integer().
createProgramObjectARB() ->
  call(5633, <<>>).

%% @doc glAttachObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec attachObjectARB(ContainerObj, Obj) -> 'ok' when ContainerObj :: integer(),Obj :: integer().
attachObjectARB(ContainerObj,Obj) ->
  cast(5634, <<ContainerObj:?GLhandleARB,Obj:?GLhandleARB>>).

%% @doc glLinkProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec linkProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
linkProgramARB(ProgramObj) ->
  cast(5635, <<ProgramObj:?GLhandleARB>>).

%% @doc glUseProgramObjectARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec useProgramObjectARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
useProgramObjectARB(ProgramObj) ->
  cast(5636, <<ProgramObj:?GLhandleARB>>).

%% @doc glValidateProgramARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec validateProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
validateProgramARB(ProgramObj) ->
  cast(5637, <<ProgramObj:?GLhandleARB>>).

%% @doc glGetObjectParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getObjectParameterfvARB(Obj, Pname) -> float() when Obj :: integer(),Pname :: enum().
getObjectParameterfvARB(Obj,Pname) ->
  call(5638, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @doc glGetObjectParameterARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getObjectParameterivARB(Obj, Pname) -> integer() when Obj :: integer(),Pname :: enum().
getObjectParameterivARB(Obj,Pname) ->
  call(5639, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @doc glGetInfoLogARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInfoLogARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getInfoLogARB(Obj,MaxLength) ->
  call(5640, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @doc glGetAttachedObjectsARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttachedObjectsARB(ContainerObj, MaxCount) -> [integer()] when ContainerObj :: integer(),MaxCount :: integer().
getAttachedObjectsARB(ContainerObj,MaxCount) ->
  call(5641, <<ContainerObj:?GLhandleARB,MaxCount:?GLsizei>>).

%% @doc glGetUniformLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getUniformLocationARB(ProgramObj,Name) ->
  NameLen = length(Name),
  call(5642, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc glGetActiveUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveUniformARB(ProgramObj,Index,MaxLength) ->
  call(5643, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @doc glGetUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformfvARB(ProgramObj, Location) -> matrix() when ProgramObj :: integer(),Location :: integer().
getUniformfvARB(ProgramObj,Location) ->
  call(5644, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @doc glGetUniformARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getUniformivARB(ProgramObj, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when ProgramObj :: integer(),Location :: integer().
getUniformivARB(ProgramObj,Location) ->
  call(5645, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @doc glGetShaderSourceARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getShaderSourceARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getShaderSourceARB(Obj,MaxLength) ->
  call(5646, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @doc glBindAttribLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindAttribLocationARB(ProgramObj, Index, Name) -> 'ok' when ProgramObj :: integer(),Index :: integer(),Name :: string().
bindAttribLocationARB(ProgramObj,Index,Name) ->
  NameLen = length(Name),
  cast(5647, <<ProgramObj:?GLhandleARB,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc glGetActiveAttribARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveAttribARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveAttribARB(ProgramObj,Index,MaxLength) ->
  call(5648, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @doc glGetAttribLocationARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getAttribLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getAttribLocationARB(ProgramObj,Name) ->
  NameLen = length(Name),
  call(5649, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc Determine if a name corresponds to a renderbuffer object
%%
%% ``gl:isRenderbuffer'' returns `?GL_TRUE' if  `Renderbuffer'  is currently the
%% name of a renderbuffer object. If  `Renderbuffer'  is zero, or if  `Renderbuffer' 
%% is not the name of a renderbuffer object, or if an error occurs, ``gl:isRenderbuffer''
%% returns `?GL_FALSE'. If  `Renderbuffer'  is a name returned by  {@link gl:genRenderbuffers/1} 
%% , by that has not yet been bound through a call to  {@link gl:bindRenderbuffer/2}  or  {@link gl:framebufferRenderbuffer/4} 
%% , then the name is not a renderbuffer object and ``gl:isRenderbuffer'' returns `?GL_FALSE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsRenderbuffer.xhtml">external</a> documentation.
-spec isRenderbuffer(Renderbuffer) -> 0|1 when Renderbuffer :: integer().
isRenderbuffer(Renderbuffer) ->
  call(5650, <<Renderbuffer:?GLuint>>).

%% @doc Bind a renderbuffer to a renderbuffer target
%%
%% ``gl:bindRenderbuffer'' binds the renderbuffer object with name  `Renderbuffer' 
%% to the renderbuffer target specified by  `Target' .  `Target'  must be `?GL_RENDERBUFFER'
%% .  `Renderbuffer'  is the name of a renderbuffer object previously returned from a
%% call to  {@link gl:genRenderbuffers/1} , or zero to break the existing binding of a renderbuffer
%% object to  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindRenderbuffer.xhtml">external</a> documentation.
-spec bindRenderbuffer(Target, Renderbuffer) -> 'ok' when Target :: enum(),Renderbuffer :: integer().
bindRenderbuffer(Target,Renderbuffer) ->
  cast(5651, <<Target:?GLenum,Renderbuffer:?GLuint>>).

%% @doc Delete renderbuffer objects
%%
%% ``gl:deleteRenderbuffers'' deletes the  `N'  renderbuffer objects whose names are
%% stored in the array addressed by  `Renderbuffers' . The name zero is reserved by the
%% GL and is silently ignored, should it occur in  `Renderbuffers' , as are other unused
%% names. Once a renderbuffer object is deleted, its name is again unused and it has no contents.
%% If a renderbuffer that is currently bound to the target `?GL_RENDERBUFFER' is deleted,
%% it is as though  {@link gl:bindRenderbuffer/2}  had been executed with a  `Target'  of `?GL_RENDERBUFFER'
%%  and a  `Name'  of zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteRenderbuffers.xhtml">external</a> documentation.
-spec deleteRenderbuffers(Renderbuffers) -> 'ok' when Renderbuffers :: [integer()].
deleteRenderbuffers(Renderbuffers) ->
  RenderbuffersLen = length(Renderbuffers),
  cast(5652, <<RenderbuffersLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Renderbuffers>>)/binary,0:(((1+RenderbuffersLen) rem 2)*32)>>).

%% @doc Generate renderbuffer object names
%%
%% ``gl:genRenderbuffers'' returns  `N'  renderbuffer object names in  `Renderbuffers' 
%% . There is no guarantee that the names form a contiguous set of integers; however, it
%% is guaranteed that none of the returned names was in use immediately before the call to ``gl:genRenderbuffers''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenRenderbuffers.xhtml">external</a> documentation.
-spec genRenderbuffers(N) -> [integer()] when N :: integer().
genRenderbuffers(N) ->
  call(5653, <<N:?GLsizei>>).

%% @doc Establish data storage, format and dimensions of a renderbuffer object's image
%%
%% ``gl:renderbufferStorage'' is equivalent to calling  {@link gl:renderbufferStorageMultisample/5} 
%%  with the  `Samples'  set to zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorage.xhtml">external</a> documentation.
-spec renderbufferStorage(Target, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorage(Target,Internalformat,Width,Height) ->
  cast(5654, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Retrieve information about a bound renderbuffer object
%%
%% ``gl:getRenderbufferParameteriv'' retrieves information about a bound renderbuffer object.
%%  `Target'  specifies the target of the query operation and must be `?GL_RENDERBUFFER'
%% .  `Pname'  specifies the parameter whose value to query and must be one of `?GL_RENDERBUFFER_WIDTH'
%% , `?GL_RENDERBUFFER_HEIGHT', `?GL_RENDERBUFFER_INTERNAL_FORMAT', `?GL_RENDERBUFFER_RED_SIZE'
%% , `?GL_RENDERBUFFER_GREEN_SIZE', `?GL_RENDERBUFFER_BLUE_SIZE', `?GL_RENDERBUFFER_ALPHA_SIZE'
%% , `?GL_RENDERBUFFER_DEPTH_SIZE', `?GL_RENDERBUFFER_DEPTH_SIZE', `?GL_RENDERBUFFER_STENCIL_SIZE'
%% , or `?GL_RENDERBUFFER_SAMPLES'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetRenderbufferParameter.xhtml">external</a> documentation.
-spec getRenderbufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getRenderbufferParameteriv(Target,Pname) ->
  call(5655, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Determine if a name corresponds to a framebuffer object
%%
%% ``gl:isFramebuffer'' returns `?GL_TRUE' if  `Framebuffer'  is currently the
%% name of a framebuffer object. If  `Framebuffer'  is zero, or if `?framebuffer'
%% is not the name of a framebuffer object, or if an error occurs, ``gl:isFramebuffer''
%% returns `?GL_FALSE'. If  `Framebuffer'  is a name returned by  {@link gl:genFramebuffers/1} 
%% , by that has not yet been bound through a call to  {@link gl:bindFramebuffer/2} , then the
%% name is not a framebuffer object and ``gl:isFramebuffer'' returns `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsFramebuffer.xhtml">external</a> documentation.
-spec isFramebuffer(Framebuffer) -> 0|1 when Framebuffer :: integer().
isFramebuffer(Framebuffer) ->
  call(5656, <<Framebuffer:?GLuint>>).

%% @doc Bind a framebuffer to a framebuffer target
%%
%% ``gl:bindFramebuffer'' binds the framebuffer object with name  `Framebuffer'  to
%% the framebuffer target specified by  `Target' .  `Target'  must be either `?GL_DRAW_FRAMEBUFFER'
%% , `?GL_READ_FRAMEBUFFER' or `?GL_FRAMEBUFFER'. If a framebuffer object is bound
%% to `?GL_DRAW_FRAMEBUFFER' or `?GL_READ_FRAMEBUFFER', it becomes the target for
%% rendering or readback operations, respectively, until it is deleted or another framebuffer
%% is bound to the corresponding bind point. Calling ``gl:bindFramebuffer'' with  `Target' 
%%  set to `?GL_FRAMEBUFFER' binds  `Framebuffer'  to both the read and draw framebuffer
%% targets.  `Framebuffer'  is the name of a framebuffer object previously returned from
%% a call to  {@link gl:genFramebuffers/1} , or zero to break the existing binding of a framebuffer
%% object to  `Target' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFramebuffer.xhtml">external</a> documentation.
-spec bindFramebuffer(Target, Framebuffer) -> 'ok' when Target :: enum(),Framebuffer :: integer().
bindFramebuffer(Target,Framebuffer) ->
  cast(5657, <<Target:?GLenum,Framebuffer:?GLuint>>).

%% @doc Delete framebuffer objects
%%
%% ``gl:deleteFramebuffers'' deletes the  `N'  framebuffer objects whose names are stored
%% in the array addressed by  `Framebuffers' . The name zero is reserved by the GL and
%% is silently ignored, should it occur in  `Framebuffers' , as are other unused names.
%% Once a framebuffer object is deleted, its name is again unused and it has no attachments.
%% If a framebuffer that is currently bound to one or more of the targets `?GL_DRAW_FRAMEBUFFER'
%%  or `?GL_READ_FRAMEBUFFER' is deleted, it is as though  {@link gl:bindFramebuffer/2} 
%% had been executed with the corresponding  `Target'  and  `Framebuffer'  zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteFramebuffers.xhtml">external</a> documentation.
-spec deleteFramebuffers(Framebuffers) -> 'ok' when Framebuffers :: [integer()].
deleteFramebuffers(Framebuffers) ->
  FramebuffersLen = length(Framebuffers),
  cast(5658, <<FramebuffersLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Framebuffers>>)/binary,0:(((1+FramebuffersLen) rem 2)*32)>>).

%% @doc Generate framebuffer object names
%%
%% ``gl:genFramebuffers'' returns  `N'  framebuffer object names in  `Ids' . There
%% is no guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genFramebuffers''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenFramebuffers.xhtml">external</a> documentation.
-spec genFramebuffers(N) -> [integer()] when N :: integer().
genFramebuffers(N) ->
  call(5659, <<N:?GLsizei>>).

%% @doc Check the completeness status of a framebuffer
%%
%% ``gl:checkFramebufferStatus'' queries the completeness status of the framebuffer object
%% currently bound to  `Target' .  `Target'  must be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER'
%%  or `?GL_FRAMEBUFFER'. `?GL_FRAMEBUFFER' is equivalent to `?GL_DRAW_FRAMEBUFFER'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCheckFramebufferStatus.xhtml">external</a> documentation.
-spec checkFramebufferStatus(Target) -> enum() when Target :: enum().
checkFramebufferStatus(Target) ->
  call(5660, <<Target:?GLenum>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture1D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5661, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture2D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5662, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer(),Zoffset :: integer().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) ->
  cast(5663, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint,Zoffset:?GLint>>).

%% @doc Attach a renderbuffer as a logical buffer to the currently bound framebuffer object
%%
%% ``gl:framebufferRenderbuffer'' attaches a renderbuffer as one of the logical buffers
%% of the currently bound framebuffer object.  `Renderbuffer'  is the name of the renderbuffer
%% object to attach and must be either zero, or the name of an existing renderbuffer object
%% of type  `Renderbuffertarget' . If  `Renderbuffer'  is not zero and if ``gl:framebufferRenderbuffer''
%%  is successful, then the renderbuffer name  `Renderbuffer'  will be used as the logical
%% buffer identified by  `Attachment'  of the framebuffer currently bound to  `Target' .
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferRenderbuffer.xhtml">external</a> documentation.
-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok' when Target :: enum(),Attachment :: enum(),Renderbuffertarget :: enum(),Renderbuffer :: integer().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) ->
  cast(5664, <<Target:?GLenum,Attachment:?GLenum,Renderbuffertarget:?GLenum,Renderbuffer:?GLuint>>).

%% @doc Retrieve information about attachments of a bound framebuffer object
%%
%% ``gl:getFramebufferAttachmentParameter'' returns information about attachments of a
%% bound framebuffer object.  `Target'  specifies the framebuffer binding point and must
%% be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER' or `?GL_FRAMEBUFFER'. `?GL_FRAMEBUFFER'
%%  is equivalent to `?GL_DRAW_FRAMEBUFFER'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFramebufferAttachmentParameter.xhtml">external</a> documentation.
-spec getFramebufferAttachmentParameteriv(Target, Attachment, Pname) -> integer() when Target :: enum(),Attachment :: enum(),Pname :: enum().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) ->
  call(5665, <<Target:?GLenum,Attachment:?GLenum,Pname:?GLenum>>).

%% @doc Generate mipmaps for a specified texture target
%%
%% ``gl:generateMipmap'' generates mipmaps for the texture attached to  `Target'  of
%% the active texture unit. For cube map textures, a `?GL_INVALID_OPERATION' error is
%% generated if the texture attached to  `Target'  is not cube complete. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenerateMipmap.xhtml">external</a> documentation.
-spec generateMipmap(Target) -> 'ok' when Target :: enum().
generateMipmap(Target) ->
  cast(5666, <<Target:?GLenum>>).

%% @doc Copy a block of pixels from the read framebuffer to the draw framebuffer
%%
%% ``gl:blitFramebuffer'' transfers a rectangle of pixel values from one region of the
%% read framebuffer to another region in the draw framebuffer.  `Mask'  is the bitwise
%% OR of a number of values indicating which buffers are to be copied. The values are `?GL_COLOR_BUFFER_BIT'
%% , `?GL_DEPTH_BUFFER_BIT', and `?GL_STENCIL_BUFFER_BIT'. The pixels corresponding
%% to these buffers are copied from the source rectangle bounded by the locations ( `SrcX0' 
%% ;  `SrcY0' ) and ( `SrcX1' ;  `SrcY1' ) to the destination rectangle bounded
%% by the locations ( `DstX0' ;  `DstY0' ) and ( `DstX1' ;  `DstY1' ). The lower
%% bounds of the rectangle are inclusive, while the upper bounds are exclusive. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlitFramebuffer.xhtml">external</a> documentation.
-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok' when SrcX0 :: integer(),SrcY0 :: integer(),SrcX1 :: integer(),SrcY1 :: integer(),DstX0 :: integer(),DstY0 :: integer(),DstX1 :: integer(),DstY1 :: integer(),Mask :: integer(),Filter :: enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) ->
  cast(5667, <<SrcX0:?GLint,SrcY0:?GLint,SrcX1:?GLint,SrcY1:?GLint,DstX0:?GLint,DstY0:?GLint,DstX1:?GLint,DstY1:?GLint,Mask:?GLbitfield,Filter:?GLenum>>).

%% @doc Establish data storage, format, dimensions and sample count of a renderbuffer object's image
%%
%% ``gl:renderbufferStorageMultisample'' establishes the data storage, format, dimensions
%% and number of samples of a renderbuffer object's image. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorageMultisample.xhtml">external</a> documentation.
-spec renderbufferStorageMultisample(Target, Samples, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) ->
  cast(5668, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTextureLayer(Target, Attachment, Texture, Level, Layer) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Layer :: integer().
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) ->
  cast(5669, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Layer:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTextureFaceARB(Target, Attachment, Texture, Level, Face) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Face :: enum().
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) ->
  cast(5670, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Face:?GLenum>>).

%% @doc Indicate modifications to a range of a mapped buffer
%%
%% ``gl:flushMappedBufferRange'' indicates that modifications have been made to a range
%% of a mapped buffer. The buffer must previously have been mapped with the `?GL_MAP_FLUSH_EXPLICIT'
%%  flag.  `Offset'  and  `Length'  indicate the modified subrange of the mapping,
%% in basic units. The specified subrange to flush is relative to the start of the currently
%% mapped range of the buffer. ``gl:flushMappedBufferRange'' may be called multiple times
%% to indicate distinct subranges of the mapping which require flushing. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlushMappedBufferRange.xhtml">external</a> documentation.
-spec flushMappedBufferRange(Target, Offset, Length) -> 'ok' when Target :: enum(),Offset :: integer(),Length :: integer().
flushMappedBufferRange(Target,Offset,Length) ->
  cast(5671, <<Target:?GLenum,0:32,Offset:?GLintptr,Length:?GLsizeiptr>>).

%% @doc Bind a vertex array object
%%
%% ``gl:bindVertexArray'' binds the vertex array object with name  `Array' .  `Array' 
%%  is the name of a vertex array object previously returned from a call to  {@link gl:genVertexArrays/1} 
%% , or zero to break the existing vertex array object binding. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexArray.xhtml">external</a> documentation.
-spec bindVertexArray(Array) -> 'ok' when Array :: integer().
bindVertexArray(Array) ->
  cast(5672, <<Array:?GLuint>>).

%% @doc Delete vertex array objects
%%
%% ``gl:deleteVertexArrays'' deletes  `N'  vertex array objects whose names are stored
%% in the array addressed by  `Arrays' . Once a vertex array object is deleted it has
%% no contents and its name is again unused. If a vertex array object that is currently bound
%% is deleted, the binding for that object reverts to zero and the default vertex array becomes
%% current. Unused names in  `Arrays'  are silently ignored, as is the value zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteVertexArrays.xhtml">external</a> documentation.
-spec deleteVertexArrays(Arrays) -> 'ok' when Arrays :: [integer()].
deleteVertexArrays(Arrays) ->
  ArraysLen = length(Arrays),
  cast(5673, <<ArraysLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Arrays>>)/binary,0:(((1+ArraysLen) rem 2)*32)>>).

%% @doc Generate vertex array object names
%%
%% ``gl:genVertexArrays'' returns  `N'  vertex array object names in  `Arrays' .
%% There is no guarantee that the names form a contiguous set of integers; however, it is
%% guaranteed that none of the returned names was in use immediately before the call to ``gl:genVertexArrays''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenVertexArrays.xhtml">external</a> documentation.
-spec genVertexArrays(N) -> [integer()] when N :: integer().
genVertexArrays(N) ->
  call(5674, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a vertex array object
%%
%% ``gl:isVertexArray'' returns `?GL_TRUE' if  `Array'  is currently the name of
%% a renderbuffer object. If  `Renderbuffer'  is zero, or if  `Array'  is not the name
%% of a renderbuffer object, or if an error occurs, ``gl:isVertexArray'' returns `?GL_FALSE'
%% . If  `Array'  is a name returned by  {@link gl:genVertexArrays/1} , by that has not yet
%% been bound through a call to  {@link gl:bindVertexArray/1} , then the name is not a vertex
%% array object and ``gl:isVertexArray'' returns `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsVertexArray.xhtml">external</a> documentation.
-spec isVertexArray(Array) -> 0|1 when Array :: integer().
isVertexArray(Array) ->
  call(5675, <<Array:?GLuint>>).

%% @doc Retrieve the index of a named uniform block
%%
%% ``gl:getUniformIndices'' retrieves the indices of a number of uniforms within  `Program' 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformIndices.xhtml">external</a> documentation.
-spec getUniformIndices(Program, UniformNames) -> [integer()] when Program :: integer(),UniformNames :: iolist().
getUniformIndices(Program,UniformNames) ->
  UniformNamesTemp = list_to_binary([[Str|[0]] || Str <- UniformNames ]),
  UniformNamesLen = length(UniformNames),
  call(5676, <<Program:?GLuint,UniformNamesLen:?GLuint,(size(UniformNamesTemp)):?GLuint,(UniformNamesTemp)/binary,0:((8-((size(UniformNamesTemp)+0) rem 8)) rem 8)>>).

%% @doc glGetActiveUniforms
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getActiveUniformsiv(Program, UniformIndices, Pname) -> [integer()] when Program :: integer(),UniformIndices :: [integer()],Pname :: enum().
getActiveUniformsiv(Program,UniformIndices,Pname) ->
  UniformIndicesLen = length(UniformIndices),
  call(5677, <<Program:?GLuint,UniformIndicesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- UniformIndices>>)/binary,0:(((UniformIndicesLen) rem 2)*32),Pname:?GLenum>>).

%% @doc Query the name of an active uniform
%%
%% ``gl:getActiveUniformName'' returns the name of the active uniform at  `UniformIndex' 
%%  within  `Program' . If  `UniformName'  is not NULL, up to  `BufSize'  characters
%% (including a nul-terminator) will be written into the array whose address is specified
%% by  `UniformName' . If  `Length'  is not NULL, the number of characters that were
%% (or would have been) written into  `UniformName'  (not including the nul-terminator)
%% will be placed in the variable whose address is specified in  `Length' . If  `Length' 
%%  is NULL, no length is returned. The length of the longest uniform name in  `Program' 
%% is given by the value of `?GL_ACTIVE_UNIFORM_MAX_LENGTH', which can be queried with  {@link gl:getProgramiv/2} 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformName.xhtml">external</a> documentation.
-spec getActiveUniformName(Program, UniformIndex, BufSize) -> string() when Program :: integer(),UniformIndex :: integer(),BufSize :: integer().
getActiveUniformName(Program,UniformIndex,BufSize) ->
  call(5678, <<Program:?GLuint,UniformIndex:?GLuint,BufSize:?GLsizei>>).

%% @doc Retrieve the index of a named uniform block
%%
%% ``gl:getUniformBlockIndex'' retrieves the index of a uniform block within  `Program' .
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformBlockIndex.xhtml">external</a> documentation.
-spec getUniformBlockIndex(Program, UniformBlockName) -> integer() when Program :: integer(),UniformBlockName :: string().
getUniformBlockIndex(Program,UniformBlockName) ->
  UniformBlockNameLen = length(UniformBlockName),
  call(5679, <<Program:?GLuint,(list_to_binary([UniformBlockName|[0]]))/binary,0:((8-((UniformBlockNameLen+ 5) rem 8)) rem 8)>>).

%% @doc Query information about an active uniform block
%%
%% ``gl:getActiveUniformBlockiv'' retrieves information about an active uniform block within
%%  `Program' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlock.xhtml">external</a> documentation.
-spec getActiveUniformBlockiv(Program, UniformBlockIndex, Pname, Params) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),Pname :: enum(),Params :: mem().
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) ->
  send_bin(Params),
  call(5680, <<Program:?GLuint,UniformBlockIndex:?GLuint,Pname:?GLenum>>).

%% @doc Retrieve the name of an active uniform block
%%
%% ``gl:getActiveUniformBlockName'' retrieves the name of the active uniform block at  `UniformBlockIndex' 
%%  within  `Program' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlockName.xhtml">external</a> documentation.
-spec getActiveUniformBlockName(Program, UniformBlockIndex, BufSize) -> string() when Program :: integer(),UniformBlockIndex :: integer(),BufSize :: integer().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) ->
  call(5681, <<Program:?GLuint,UniformBlockIndex:?GLuint,BufSize:?GLsizei>>).

%% @doc Assign a binding point to an active uniform block
%%
%%  Binding points for active uniform blocks are assigned using ``gl:uniformBlockBinding''.
%% Each of a program's active uniform blocks has a corresponding uniform buffer binding point.
%%  `Program'  is the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  has been issued in the past. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformBlockBinding.xhtml">external</a> documentation.
-spec uniformBlockBinding(Program, UniformBlockIndex, UniformBlockBinding) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),UniformBlockBinding :: integer().
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) ->
  cast(5682, <<Program:?GLuint,UniformBlockIndex:?GLuint,UniformBlockBinding:?GLuint>>).

%% @doc Copy part of the data store of a buffer object to the data store of another buffer object
%%
%% ``gl:copyBufferSubData'' copies part of the data store attached to  `Readtarget' 
%% to the data store attached to  `Writetarget' . The number of basic machine units indicated
%% by  `Size'  is copied from the source, at offset  `Readoffset'  to the destination
%% at  `Writeoffset' , also in basic machine units. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyBufferSubData.xhtml">external</a> documentation.
-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok' when ReadTarget :: enum(),WriteTarget :: enum(),ReadOffset :: integer(),WriteOffset :: integer(),Size :: integer().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) ->
  cast(5683, <<ReadTarget:?GLenum,WriteTarget:?GLenum,ReadOffset:?GLintptr,WriteOffset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Render primitives from array data with a per-element offset
%%
%% ``gl:drawElementsBaseVertex'' behaves identically to  {@link gl:drawElements/4}  except
%% that the `i'th element transferred by the corresponding draw call will be taken from
%% element  `Indices' [i] +  `Basevertex'  of each enabled array. If the resulting
%% value is larger than the maximum value representable by  `Type' , it is as if the calculation
%% were upconverted to 32-bit unsigned integers (with wrapping on overflow conditions). The
%% operation is undefined if the sum would be negative. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsBaseVertex.xhtml">external</a> documentation.
-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5684, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5685, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

%% @doc Render primitives from array data with a per-element offset
%%
%% ``gl:drawRangeElementsBaseVertex'' is a restricted form of  {@link gl:drawElementsBaseVertex/5} 
%% .  `Mode' ,  `Start' ,  `End' ,  `Count'  and  `Basevertex'  match the
%% corresponding arguments to  {@link gl:drawElementsBaseVertex/5} , with the additional constraint
%% that all values in the array  `Indices'  must lie between  `Start'  and  `End' ,
%% inclusive, prior to adding  `Basevertex' . Index values lying outside the range [ `Start' 
%% ,  `End' ] are treated in the same way as  {@link gl:drawElementsBaseVertex/5} . The `i'
%% th element transferred by the corresponding draw call will be taken from element  `Indices' 
%% [i] +  `Basevertex'  of each enabled array. If the resulting value is larger than the
%% maximum value representable by  `Type' , it is as if the calculation were upconverted
%% to 32-bit unsigned integers (with wrapping on overflow conditions). The operation is undefined
%% if the sum would be negative. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElementsBaseVertex.xhtml">external</a> documentation.
-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5686, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5687, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

%% @doc Render multiple instances of a set of primitives from array data with a per-element offset
%%
%% ``gl:drawElementsInstancedBaseVertex'' behaves identically to  {@link gl:drawElementsInstanced/5} 
%%  except that the `i'th element transferred by the corresponding draw call will be
%% taken from element  `Indices' [i] +  `Basevertex'  of each enabled array. If the
%% resulting value is larger than the maximum value representable by  `Type' , it is as
%% if the calculation were upconverted to 32-bit unsigned integers (with wrapping on overflow
%% conditions). The operation is undefined if the sum would be negative. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertex.xhtml">external</a> documentation.
-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Primcount, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) when  is_integer(Indices) ->
  cast(5688, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Basevertex:?GLint>>);
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) ->
  send_bin(Indices),
  cast(5689, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Basevertex:?GLint>>).

%% @doc Specifiy the vertex to be used as the source of data for flat shaded varyings
%%
%% `Flatshading' a vertex shader varying output means to assign all vetices of the primitive
%% the same value for that output. The vertex from which these values is derived is known
%% as the `provoking vertex' and ``gl:provokingVertex'' specifies which vertex is
%% to be used as the source of data for flat shaded varyings. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProvokingVertex.xhtml">external</a> documentation.
-spec provokingVertex(Mode) -> 'ok' when Mode :: enum().
provokingVertex(Mode) ->
  cast(5690, <<Mode:?GLenum>>).

%% @doc Create a new sync object and insert it into the GL command stream
%%
%% ``gl:fenceSync'' creates a new fence sync object, inserts a fence command into the GL
%% command stream and associates it with that sync object, and returns a non-zero name corresponding
%% to the sync object. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFenceSync.xhtml">external</a> documentation.
-spec fenceSync(Condition, Flags) -> integer() when Condition :: enum(),Flags :: integer().
fenceSync(Condition,Flags) ->
  call(5691, <<Condition:?GLenum,Flags:?GLbitfield>>).

%% @doc Determine if a name corresponds to a sync object
%%
%% ``gl:isSync'' returns `?GL_TRUE' if  `Sync'  is currently the name of a sync
%% object. If  `Sync'  is not the name of a sync object, or if an error occurs, ``gl:isSync''
%%  returns `?GL_FALSE'. Note that zero is not the name of a sync object. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSync.xhtml">external</a> documentation.
-spec isSync(Sync) -> 0|1 when Sync :: integer().
isSync(Sync) ->
  call(5692, <<Sync:?GLsync>>).

%% @doc Delete a sync object
%%
%% ``gl:deleteSync'' deletes the sync object specified by  `Sync' . If the fence command
%% corresponding to the specified sync object has completed, or if no  {@link gl:waitSync/3} 
%% or  {@link gl:clientWaitSync/3}  commands are blocking on  `Sync' , the object is deleted
%% immediately. Otherwise,  `Sync'  is flagged for deletion and will be deleted when it
%% is no longer associated with any fence command and is no longer blocking any  {@link gl:waitSync/3} 
%%  or  {@link gl:clientWaitSync/3}  command. In either case, after ``gl:deleteSync'' returns,
%% the name  `Sync'  is invalid and can no longer be used to refer to the sync object. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSync.xhtml">external</a> documentation.
-spec deleteSync(Sync) -> 'ok' when Sync :: integer().
deleteSync(Sync) ->
  cast(5693, <<Sync:?GLsync>>).

%% @doc Block and wait for a sync object to become signaled
%%
%% ``gl:clientWaitSync'' causes the client to block and wait for the sync object specified
%% by  `Sync'  to become signaled. If  `Sync'  is signaled when ``gl:clientWaitSync''
%% is called, ``gl:clientWaitSync'' returns immediately, otherwise it will block and wait
%% for up to  `Timeout'  nanoseconds for  `Sync'  to become signaled. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClientWaitSync.xhtml">external</a> documentation.
-spec clientWaitSync(Sync, Flags, Timeout) -> enum() when Sync :: integer(),Flags :: integer(),Timeout :: integer().
clientWaitSync(Sync,Flags,Timeout) ->
  call(5694, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @doc Instruct the GL server to block until the specified sync object becomes signaled
%%
%% ``gl:waitSync'' causes the GL server to block and wait until  `Sync'  becomes signaled.
%%  `Sync'  is the name of an existing sync object upon which to wait.  `Flags'  and  `Timeout' 
%%  are currently not used and must be set to zero and the special value `?GL_TIMEOUT_IGNORED'
%% , respectively
%%
%%  `Flags'  and  `Timeout'  are placeholders for anticipated future extensions of
%% sync object capabilities. They must have these reserved values in order that existing
%% code calling ``gl:waitSync'' operate properly in the presence of such extensions.
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glWaitSync.xhtml">external</a> documentation.
-spec waitSync(Sync, Flags, Timeout) -> 'ok' when Sync :: integer(),Flags :: integer(),Timeout :: integer().
waitSync(Sync,Flags,Timeout) ->
  cast(5695, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getInteger64v(Pname) -> [integer()] when Pname :: enum().
getInteger64v(Pname) ->
  call(5696, <<Pname:?GLenum>>).

%% @doc Query the properties of a sync object
%%
%% ``gl:getSynciv'' retrieves properties of a sync object.  `Sync'  specifies the name
%% of the sync object whose properties to retrieve. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSync.xhtml">external</a> documentation.
-spec getSynciv(Sync, Pname, BufSize) -> [integer()] when Sync :: integer(),Pname :: enum(),BufSize :: integer().
getSynciv(Sync,Pname,BufSize) ->
  call(5697, <<Sync:?GLsync,Pname:?GLenum,BufSize:?GLsizei>>).

%% @doc Establish the data storage, format, dimensions, and number of samples of a multisample texture's image
%%
%% ``gl:texImage2DMultisample'' establishes the data storage, format, dimensions and number
%% of samples of a multisample texture's image. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2DMultisample.xhtml">external</a> documentation.
-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Fixedsamplelocations :: 0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) ->
  cast(5698, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Fixedsamplelocations:?GLboolean>>).

%% @doc Establish the data storage, format, dimensions, and number of samples of a multisample texture's image
%%
%% ``gl:texImage3DMultisample'' establishes the data storage, format, dimensions and number
%% of samples of a multisample texture's image. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3DMultisample.xhtml">external</a> documentation.
-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Fixedsamplelocations :: 0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) ->
  cast(5699, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Fixedsamplelocations:?GLboolean>>).

%% @doc Retrieve the location of a sample
%%
%% ``gl:getMultisamplefv'' queries the location of a given sample.  `Pname'  specifies
%% the sample parameter to retrieve and must be `?GL_SAMPLE_POSITION'.  `Index' 
%% corresponds to the sample for which the location should be returned. The sample location
%% is returned as two floating-point values in  `Val[0]'  and  `Val[1]' , each between
%% 0 and 1, corresponding to the  `X'  and  `Y'  locations respectively in the GL pixel
%% space of that sample. (0.5, 0.5) this corresponds to the pixel center.  `Index'  must
%% be between zero and the value of `?GL_SAMPLES' - 1. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetMultisample.xhtml">external</a> documentation.
-spec getMultisamplefv(Pname, Index) -> {float(),float()} when Pname :: enum(),Index :: integer().
getMultisamplefv(Pname,Index) ->
  call(5700, <<Pname:?GLenum,Index:?GLuint>>).

%% @doc Set the value of a sub-word of the sample mask
%%
%% ``gl:sampleMaski'' sets one 32-bit sub-word of the multi-word sample mask, `?GL_SAMPLE_MASK_VALUE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleMaski.xhtml">external</a> documentation.
-spec sampleMaski(Index, Mask) -> 'ok' when Index :: integer(),Mask :: integer().
sampleMaski(Index,Mask) ->
  cast(5701, <<Index:?GLuint,Mask:?GLbitfield>>).

%% @doc glNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec namedStringARB(Type, Name, String) -> 'ok' when Type :: enum(),Name :: string(),String :: string().
namedStringARB(Type,Name,String) ->
  NameLen = length(Name),
  StringLen = length(String),
  cast(5702, <<Type:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8),(list_to_binary([String|[0]]))/binary,0:((8-((StringLen+ 1) rem 8)) rem 8)>>).

%% @doc glDeleteNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteNamedStringARB(Name) -> 'ok' when Name :: string().
deleteNamedStringARB(Name) ->
  NameLen = length(Name),
  cast(5703, <<(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc glCompileShaderIncludeARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec compileShaderIncludeARB(Shader, Path) -> 'ok' when Shader :: integer(),Path :: iolist().
compileShaderIncludeARB(Shader,Path) ->
  PathTemp = list_to_binary([[Str|[0]] || Str <- Path ]),
  PathLen = length(Path),
  cast(5704, <<Shader:?GLuint,PathLen:?GLuint,(size(PathTemp)):?GLuint,(PathTemp)/binary,0:((8-((size(PathTemp)+0) rem 8)) rem 8)>>).

%% @doc glIsNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec isNamedStringARB(Name) -> 0|1 when Name :: string().
isNamedStringARB(Name) ->
  NameLen = length(Name),
  call(5705, <<(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc glGetNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getNamedStringARB(Name, BufSize) -> string() when Name :: string(),BufSize :: integer().
getNamedStringARB(Name,BufSize) ->
  NameLen = length(Name),
  call(5706, <<(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8),BufSize:?GLsizei>>).

%% @doc glGetNamedStringARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getNamedStringivARB(Name, Pname) -> integer() when Name :: string(),Pname :: enum().
getNamedStringivARB(Name,Pname) ->
  NameLen = length(Name),
  call(5707, <<(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8),Pname:?GLenum>>).

%% @doc glBindFragDataLocationIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec bindFragDataLocationIndexed(Program, ColorNumber, Index, Name) -> 'ok' when Program :: integer(),ColorNumber :: integer(),Index :: integer(),Name :: string().
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) ->
  NameLen = length(Name),
  cast(5708, <<Program:?GLuint,ColorNumber:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc Query the bindings of color indices to user-defined varying out variables
%%
%% ``gl:getFragDataIndex'' returns the index of the fragment color to which the variable  `Name' 
%%  was bound when the program object  `Program'  was last linked. If  `Name'  is not
%% a varying out variable of  `Program' , or if an error occurs, -1 will be returned. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataIndex.xhtml">external</a> documentation.
-spec getFragDataIndex(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataIndex(Program,Name) ->
  NameLen = length(Name),
  call(5709, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 5) rem 8)) rem 8)>>).

%% @doc Generate sampler object names
%%
%% ``gl:genSamplers'' returns  `N'  sampler object names in  `Samplers' . There is
%% no guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genSamplers''
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenSamplers.xhtml">external</a> documentation.
-spec genSamplers(Count) -> [integer()] when Count :: integer().
genSamplers(Count) ->
  call(5710, <<Count:?GLsizei>>).

%% @doc Delete named sampler objects
%%
%% ``gl:deleteSamplers'' deletes  `N'  sampler objects named by the elements of the
%% array  `Ids' . After a sampler object is deleted, its name is again unused. If a sampler
%% object that is currently bound to a sampler unit is deleted, it is as though  {@link gl:bindSampler/2} 
%%  is called with unit set to the unit the sampler is bound to and sampler zero. Unused
%% names in samplers are silently ignored, as is the reserved name zero. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSamplers.xhtml">external</a> documentation.
-spec deleteSamplers(Samplers) -> 'ok' when Samplers :: [integer()].
deleteSamplers(Samplers) ->
  SamplersLen = length(Samplers),
  cast(5711, <<SamplersLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Samplers>>)/binary,0:(((1+SamplersLen) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a sampler object
%%
%% ``gl:isSampler'' returns `?GL_TRUE' if  `Id'  is currently the name of a sampler
%% object. If  `Id'  is zero, or is a non-zero value that is not currently the name of
%% a sampler object, or if an error occurs, ``gl:isSampler'' returns `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSampler.xhtml">external</a> documentation.
-spec isSampler(Sampler) -> 0|1 when Sampler :: integer().
isSampler(Sampler) ->
  call(5712, <<Sampler:?GLuint>>).

%% @doc Bind a named sampler to a texturing target
%%
%% ``gl:bindSampler'' binds  `Sampler'  to the texture unit at index  `Unit' .  `Sampler' 
%%  must be zero or the name of a sampler object previously returned from a call to  {@link gl:genSamplers/1} 
%% .  `Unit'  must be less than the value of `?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS'.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindSampler.xhtml">external</a> documentation.
-spec bindSampler(Unit, Sampler) -> 'ok' when Unit :: integer(),Sampler :: integer().
bindSampler(Unit,Sampler) ->
  cast(5713, <<Unit:?GLuint,Sampler:?GLuint>>).

%% @doc Set sampler parameters
%%
%% ``gl:samplerParameter'' assigns the value or values in  `Params'  to the sampler
%% parameter specified as  `Pname' .  `Sampler'  specifies the sampler object to be
%% modified, and must be the name of a sampler object previously returned from a call to  {@link gl:genSamplers/1} 
%% . The following symbols are accepted in  `Pname' : 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSamplerParameter.xhtml">external</a> documentation.
-spec samplerParameteri(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: integer().
samplerParameteri(Sampler,Pname,Param) ->
  cast(5714, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameteriv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameteriv(Sampler,Pname,Param) ->
  ParamLen = length(Param),
  cast(5715, <<Sampler:?GLuint,Pname:?GLenum,ParamLen:?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+ParamLen) rem 2)*32)>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterf(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: float().
samplerParameterf(Sampler,Pname,Param) ->
  cast(5716, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterfv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [float()].
samplerParameterfv(Sampler,Pname,Param) ->
  ParamLen = length(Param),
  cast(5717, <<Sampler:?GLuint,Pname:?GLenum,ParamLen:?GLuint,
        (<< <<C:?GLfloat>> || C <- Param>>)/binary,0:(((1+ParamLen) rem 2)*32)>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterIiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIiv(Sampler,Pname,Param) ->
  ParamLen = length(Param),
  cast(5718, <<Sampler:?GLuint,Pname:?GLenum,ParamLen:?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+ParamLen) rem 2)*32)>>).

%% @doc glSamplerParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec samplerParameterIuiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIuiv(Sampler,Pname,Param) ->
  ParamLen = length(Param),
  cast(5719, <<Sampler:?GLuint,Pname:?GLenum,ParamLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Param>>)/binary,0:(((1+ParamLen) rem 2)*32)>>).

%% @doc Return sampler parameter values
%%
%% ``gl:getSamplerParameter'' returns in  `Params'  the value or values of the sampler
%% parameter specified as  `Pname' .  `Sampler'  defines the target sampler, and must
%% be the name of an existing sampler object, returned from a previous call to  {@link gl:genSamplers/1} 
%% .  `Pname'  accepts the same symbols as  {@link gl:samplerParameteri/3} , with the same
%% interpretations: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSamplerParameter.xhtml">external</a> documentation.
-spec getSamplerParameteriv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameteriv(Sampler,Pname) ->
  call(5720, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getSamplerParameteriv/2}
-spec getSamplerParameterIiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIiv(Sampler,Pname) ->
  call(5721, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getSamplerParameteriv/2}
-spec getSamplerParameterfv(Sampler, Pname) -> [float()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterfv(Sampler,Pname) ->
  call(5722, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc glGetSamplerParameterI
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getSamplerParameterIuiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIuiv(Sampler,Pname) ->
  call(5723, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc Record the GL time into a query object after all previous commands have reached the GL server but have not yet necessarily executed.
%%
%% ``gl:queryCounter'' causes the GL to record the current time into the query object named
%%  `Id' .  `Target'  must be `?GL_TIMESTAMP'. The time is recorded after all
%% previous commands on the GL client and server state and the framebuffer have been fully
%% realized. When the time is recorded, the query result for that object is marked available.
%% ``gl:queryCounter'' timer queries can be used within a  {@link gl:beginQuery/2}  /  {@link gl:beginQuery/2} 
%%  block where the target is `?GL_TIME_ELAPSED' and it does not affect the result of
%% that query object. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glQueryCounter.xhtml">external</a> documentation.
-spec queryCounter(Id, Target) -> 'ok' when Id :: integer(),Target :: enum().
queryCounter(Id,Target) ->
  cast(5724, <<Id:?GLuint,Target:?GLenum>>).

%% @doc glGetQueryObjecti64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjecti64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjecti64v(Id,Pname) ->
  call(5725, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc glGetQueryObjectui64v
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getQueryObjectui64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectui64v(Id,Pname) ->
  call(5726, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc Render primitives from array data, taking parameters from memory
%%
%% ``gl:drawArraysIndirect'' specifies multiple geometric primitives with very few subroutine
%% calls. ``gl:drawArraysIndirect'' behaves similarly to  {@link gl:drawArraysInstancedBaseInstance/5} 
%% , execept that the parameters to  {@link gl:drawArraysInstancedBaseInstance/5}  are stored
%% in memory at the address given by  `Indirect' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysIndirect.xhtml">external</a> documentation.
-spec drawArraysIndirect(Mode, Indirect) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem().
drawArraysIndirect(Mode,Indirect) when  is_integer(Indirect) ->
  cast(5727, <<Mode:?GLenum,Indirect:?GLuint>>);
drawArraysIndirect(Mode,Indirect) ->
  send_bin(Indirect),
  cast(5728, <<Mode:?GLenum>>).

%% @doc Render indexed primitives from array data, taking parameters from memory
%%
%% ``gl:drawElementsIndirect'' specifies multiple indexed geometric primitives with very
%% few subroutine calls. ``gl:drawElementsIndirect'' behaves similarly to  {@link gl:drawElementsInstancedBaseVertexBaseInstance/7} 
%% , execpt that the parameters to  {@link gl:drawElementsInstancedBaseVertexBaseInstance/7} 
%% are stored in memory at the address given by  `Indirect' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsIndirect.xhtml">external</a> documentation.
-spec drawElementsIndirect(Mode, Type, Indirect) -> 'ok' when Mode :: enum(),Type :: enum(),Indirect :: offset()|mem().
drawElementsIndirect(Mode,Type,Indirect) when  is_integer(Indirect) ->
  cast(5729, <<Mode:?GLenum,Type:?GLenum,Indirect:?GLuint>>);
drawElementsIndirect(Mode,Type,Indirect) ->
  send_bin(Indirect),
  cast(5730, <<Mode:?GLenum,Type:?GLenum>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1d(Location, X) -> 'ok' when Location :: integer(),X :: float().
uniform1d(Location,X) ->
  cast(5731, <<Location:?GLint,0:32,X:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2d(Location, X, Y) -> 'ok' when Location :: integer(),X :: float(),Y :: float().
uniform2d(Location,X,Y) ->
  cast(5732, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3d(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float().
uniform3d(Location,X,Y,Z) ->
  cast(5733, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4d(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
uniform4d(Location,X,Y,Z,W) ->
  cast(5734, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1dv(Location,Value) ->
  ValueLen = length(Value),
  cast(5735, <<Location:?GLint,0:32,ValueLen:?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2dv(Location,Value) ->
  ValueLen = length(Value),
  cast(5736, <<Location:?GLint,0:32,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3dv(Location,Value) ->
  ValueLen = length(Value),
  cast(5737, <<Location:?GLint,0:32,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4dv(Location,Value) ->
  ValueLen = length(Value),
  cast(5738, <<Location:?GLint,0:32,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5739, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5740, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5741, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5742, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5743, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5744, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5745, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5746, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3dv(Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5747, <<Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformdv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformdv(Program,Location) ->
  call(5748, <<Program:?GLuint,Location:?GLint>>).

%% @doc Retrieve the location of a subroutine uniform of a given shader stage within a program
%%
%% ``gl:getSubroutineUniformLocation'' returns the location of the subroutine uniform variable
%%  `Name'  in the shader stage of type  `Shadertype'  attached to  `Program' ,
%% with behavior otherwise identical to  {@link gl:getUniformLocation/2} . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineUniformLocation.xhtml">external</a> documentation.
-spec getSubroutineUniformLocation(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineUniformLocation(Program,Shadertype,Name) ->
  NameLen = length(Name),
  call(5749, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc Retrieve the index of a subroutine uniform of a given shader stage within a program
%%
%% ``gl:getSubroutineIndex'' returns the index of a subroutine uniform within a shader
%% stage attached to a program object.  `Program'  contains the name of the program to
%% which the shader is attached.  `Shadertype'  specifies the stage from which to query
%% shader subroutine index.  `Name'  contains the null-terminated name of the subroutine
%% uniform whose name to query. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineIndex.xhtml">external</a> documentation.
-spec getSubroutineIndex(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineIndex(Program,Shadertype,Name) ->
  NameLen = length(Name),
  call(5750, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((NameLen+ 1) rem 8)) rem 8)>>).

%% @doc Query the name of an active shader subroutine uniform
%%
%% ``gl:getActiveSubroutineUniformName'' retrieves the name of an active shader subroutine
%% uniform.  `Program'  contains the name of the program containing the uniform.  `Shadertype' 
%%  specifies the stage for which which the uniform location, given by  `Index' , is valid.
%%  `Index'  must be between zero and the value of `?GL_ACTIVE_SUBROUTINE_UNIFORMS'
%% minus one for the shader stage. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineUniformName.xhtml">external</a> documentation.
-spec getActiveSubroutineUniformName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) ->
  call(5751, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @doc Query the name of an active shader subroutine
%%
%% ``gl:getActiveSubroutineName'' queries the name of an active shader subroutine uniform
%% from the program object given in  `Program' .  `Index'  specifies the index of the
%% shader subroutine uniform within the shader stage given by  `Stage' , and must between
%% zero and the value of `?GL_ACTIVE_SUBROUTINES' minus one for the shader stage. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineName.xhtml">external</a> documentation.
-spec getActiveSubroutineName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) ->
  call(5752, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @doc Load active subroutine uniforms
%%
%% ``gl:uniformSubroutines'' loads all active subroutine uniforms for shader stage  `Shadertype' 
%%  of the current program with subroutine indices from  `Indices' , storing  `Indices[i]' 
%%  into the uniform at location  `I' .  `Count'  must be equal to the value of `?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS'
%%  for the program currently in use at shader stage  `Shadertype' . Furthermore, all
%% values in  `Indices'  must be less than the value of `?GL_ACTIVE_SUBROUTINES'
%% for the shader stage. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformSubroutines.xhtml">external</a> documentation.
-spec uniformSubroutinesuiv(Shadertype, Indices) -> 'ok' when Shadertype :: enum(),Indices :: [integer()].
uniformSubroutinesuiv(Shadertype,Indices) ->
  IndicesLen = length(Indices),
  cast(5753, <<Shadertype:?GLenum,IndicesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((IndicesLen) rem 2)*32)>>).

%% @doc Retrieve the value of a subroutine uniform of a given shader stage of the current program
%%
%% ``gl:getUniformSubroutine'' retrieves the value of the subroutine uniform at location  `Location' 
%%  for shader stage  `Shadertype'  of the current program.  `Location'  must be less
%% than the value of `?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS' for the shader currently
%% in use at shader stage  `Shadertype' . The value of the subroutine uniform is returned
%% in  `Values' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformSubroutine.xhtml">external</a> documentation.
-spec getUniformSubroutineuiv(Shadertype, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Shadertype :: enum(),Location :: integer().
getUniformSubroutineuiv(Shadertype,Location) ->
  call(5754, <<Shadertype:?GLenum,Location:?GLint>>).

%% @doc Retrieve properties of a program object corresponding to a specified shader stage
%%
%% ``gl:getProgramStage'' queries a parameter of a shader stage attached to a program object.
%%  `Program'  contains the name of the program to which the shader is attached.  `Shadertype' 
%%  specifies the stage from which to query the parameter.  `Pname'  specifies which parameter
%% should be queried. The value or values of the parameter to be queried is returned in the
%% variable whose address is given in  `Values' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramStage.xhtml">external</a> documentation.
-spec getProgramStageiv(Program, Shadertype, Pname) -> integer() when Program :: integer(),Shadertype :: enum(),Pname :: enum().
getProgramStageiv(Program,Shadertype,Pname) ->
  call(5755, <<Program:?GLuint,Shadertype:?GLenum,Pname:?GLenum>>).

%% @doc Specifies the parameters for patch primitives
%%
%% ``gl:patchParameter'' specifies the parameters that will be used for patch primitives.  `Pname' 
%%  specifies the parameter to modify and must be either `?GL_PATCH_VERTICES', `?GL_PATCH_DEFAULT_OUTER_LEVEL'
%%  or `?GL_PATCH_DEFAULT_INNER_LEVEL'. For ``gl:patchParameteri'',  `Value'  specifies
%% the new value for the parameter specified by  `Pname' . For ``gl:patchParameterfv'',
%%  `Values'  specifies the address of an array containing the new values for the parameter
%% specified by  `Pname' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPatchParameter.xhtml">external</a> documentation.
-spec patchParameteri(Pname, Value) -> 'ok' when Pname :: enum(),Value :: integer().
patchParameteri(Pname,Value) ->
  cast(5756, <<Pname:?GLenum,Value:?GLint>>).

%% @doc 
%% See {@link patchParameteri/2}
-spec patchParameterfv(Pname, Values) -> 'ok' when Pname :: enum(),Values :: [float()].
patchParameterfv(Pname,Values) ->
  ValuesLen = length(Values),
  cast(5757, <<Pname:?GLenum,ValuesLen:?GLuint,
        (<< <<C:?GLfloat>> || C <- Values>>)/binary,0:(((ValuesLen) rem 2)*32)>>).

%% @doc Bind a transform feedback object
%%
%% ``gl:bindTransformFeedback'' binds the transform feedback object with name  `Id' 
%% to the current GL state.  `Id'  must be a name previously returned from a call to  {@link gl:genTransformFeedbacks/1} 
%% . If  `Id'  has not previously been bound, a new transform feedback object with name  `Id' 
%%  and initialized with with the default transform state vector is created. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTransformFeedback.xhtml">external</a> documentation.
-spec bindTransformFeedback(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
bindTransformFeedback(Target,Id) ->
  cast(5758, <<Target:?GLenum,Id:?GLuint>>).

%% @doc Delete transform feedback objects
%%
%% ``gl:deleteTransformFeedbacks'' deletes the  `N'  transform feedback objects whose
%% names are stored in the array  `Ids' . Unused names in  `Ids'  are ignored, as is
%% the name zero. After a transform feedback object is deleted, its name is again unused
%% and it has no contents. If an active transform feedback object is deleted, its name immediately
%% becomes unused, but the underlying object is not deleted until it is no longer active. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTransformFeedbacks.xhtml">external</a> documentation.
-spec deleteTransformFeedbacks(Ids) -> 'ok' when Ids :: [integer()].
deleteTransformFeedbacks(Ids) ->
  IdsLen = length(Ids),
  cast(5759, <<IdsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+IdsLen) rem 2)*32)>>).

%% @doc Reserve transform feedback object names
%%
%% ``gl:genTransformFeedbacks'' returns  `N'  previously unused transform feedback object
%% names in  `Ids' . These names are marked as used, for the purposes of ``gl:genTransformFeedbacks''
%%  only, but they acquire transform feedback state only when they are first bound. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTransformFeedbacks.xhtml">external</a> documentation.
-spec genTransformFeedbacks(N) -> [integer()] when N :: integer().
genTransformFeedbacks(N) ->
  call(5760, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a transform feedback object
%%
%% ``gl:isTransformFeedback'' returns `?GL_TRUE' if  `Id'  is currently the name
%% of a transform feedback object. If  `Id'  is zero, or if `?id' is not the name
%% of a transform feedback object, or if an error occurs, ``gl:isTransformFeedback'' returns
%% `?GL_FALSE'. If  `Id'  is a name returned by  {@link gl:genTransformFeedbacks/1} ,
%% but that has not yet been bound through a call to  {@link gl:bindTransformFeedback/2} , then
%% the name is not a transform feedback object and ``gl:isTransformFeedback'' returns `?GL_FALSE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTransformFeedback.xhtml">external</a> documentation.
-spec isTransformFeedback(Id) -> 0|1 when Id :: integer().
isTransformFeedback(Id) ->
  call(5761, <<Id:?GLuint>>).

%% @doc Pause transform feedback operations
%%
%% ``gl:pauseTransformFeedback'' pauses transform feedback operations on the currently
%% active transform feedback object. When transform feedback operations are paused, transform
%% feedback is still considered active and changing most transform feedback state related
%% to the object results in an error. However, a new transform feedback object may be bound
%% while transform feedback is paused. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPauseTransformFeedback.xhtml">external</a> documentation.
-spec pauseTransformFeedback() -> 'ok'.
pauseTransformFeedback() ->
  cast(5762, <<>>).

%% @doc Resume transform feedback operations
%%
%% ``gl:resumeTransformFeedback'' resumes transform feedback operations on the currently
%% active transform feedback object. When transform feedback operations are paused, transform
%% feedback is still considered active and changing most transform feedback state related
%% to the object results in an error. However, a new transform feedback object may be bound
%% while transform feedback is paused. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glResumeTransformFeedback.xhtml">external</a> documentation.
-spec resumeTransformFeedback() -> 'ok'.
resumeTransformFeedback() ->
  cast(5763, <<>>).

%% @doc Render primitives using a count derived from a transform feedback object
%%
%% ``gl:drawTransformFeedback'' draws primitives of a type specified by  `Mode'  using
%% a count retrieved from the transform feedback specified by  `Id' . Calling ``gl:drawTransformFeedback''
%%  is equivalent to calling  {@link gl:drawArrays/3}  with  `Mode'  as specified,  `First' 
%%  set to zero, and  `Count'  set to the number of vertices captured on vertex stream
%% zero the last time transform feedback was active on the transform feedback object named
%% by  `Id' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedback.xhtml">external</a> documentation.
-spec drawTransformFeedback(Mode, Id) -> 'ok' when Mode :: enum(),Id :: integer().
drawTransformFeedback(Mode,Id) ->
  cast(5764, <<Mode:?GLenum,Id:?GLuint>>).

%% @doc Render primitives using a count derived from a specifed stream of a transform feedback object
%%
%% ``gl:drawTransformFeedbackStream'' draws primitives of a type specified by  `Mode' 
%% using a count retrieved from the transform feedback stream specified by  `Stream' 
%% of the transform feedback object specified by  `Id' . Calling ``gl:drawTransformFeedbackStream''
%%  is equivalent to calling  {@link gl:drawArrays/3}  with  `Mode'  as specified,  `First' 
%%  set to zero, and  `Count'  set to the number of vertices captured on vertex stream  `Stream' 
%%  the last time transform feedback was active on the transform feedback object named by  `Id' 
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackStream.xhtml">external</a> documentation.
-spec drawTransformFeedbackStream(Mode, Id, Stream) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer().
drawTransformFeedbackStream(Mode,Id,Stream) ->
  cast(5765, <<Mode:?GLenum,Id:?GLuint,Stream:?GLuint>>).

%% @doc glBeginQueryIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec beginQueryIndexed(Target, Index, Id) -> 'ok' when Target :: enum(),Index :: integer(),Id :: integer().
beginQueryIndexed(Target,Index,Id) ->
  cast(5766, <<Target:?GLenum,Index:?GLuint,Id:?GLuint>>).

%% @doc Delimit the boundaries of a query object on an indexed target
%%
%% ``gl:beginQueryIndexed'' and  {@link gl:endQueryIndexed/2}  delimit the boundaries of a
%% query object.  `Query'  must be a name previously returned from a call to  {@link gl:genQueries/1} 
%% . If a query object with name  `Id'  does not yet exist it is created with the type
%% determined by  `Target' .  `Target'  must be one of `?GL_SAMPLES_PASSED', `?GL_ANY_SAMPLES_PASSED'
%% , `?GL_PRIMITIVES_GENERATED', `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN', or `?GL_TIME_ELAPSED'
%% . The behavior of the query object depends on its type and is as follows. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQueryIndexed.xhtml">external</a> documentation.
-spec endQueryIndexed(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
endQueryIndexed(Target,Index) ->
  cast(5767, <<Target:?GLenum,Index:?GLuint>>).

%% @doc Return parameters of an indexed query object target
%%
%% ``gl:getQueryIndexediv'' returns in  `Params'  a selected parameter of the indexed
%% query object target specified by  `Target'  and  `Index' .  `Index'  specifies
%% the index of the query object target and must be between zero and a target-specific maxiumum.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryIndexed.xhtml">external</a> documentation.
-spec getQueryIndexediv(Target, Index, Pname) -> integer() when Target :: enum(),Index :: integer(),Pname :: enum().
getQueryIndexediv(Target,Index,Pname) ->
  call(5768, <<Target:?GLenum,Index:?GLuint,Pname:?GLenum>>).

%% @doc Release resources consumed by the implementation's shader compiler
%%
%% ``gl:releaseShaderCompiler'' provides a hint to the implementation that it may free
%% internal resources associated with its shader compiler.  {@link gl:compileShader/1}  may
%% subsequently be called and the implementation may at that time reallocate resources previously
%% freed by the call to ``gl:releaseShaderCompiler''. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReleaseShaderCompiler.xhtml">external</a> documentation.
-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler() ->
  cast(5769, <<>>).

%% @doc Load pre-compiled shader binaries
%%
%% ``gl:shaderBinary'' loads pre-compiled shader binary code into the  `Count'  shader
%% objects whose handles are given in  `Shaders' .  `Binary'  points to  `Length' 
%% bytes of binary shader code stored in client memory.  `BinaryFormat'  specifies the
%% format of the pre-compiled code. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderBinary.xhtml">external</a> documentation.
-spec shaderBinary(Shaders, Binaryformat, Binary) -> 'ok' when Shaders :: [integer()],Binaryformat :: enum(),Binary :: binary().
shaderBinary(Shaders,Binaryformat,Binary) ->
  ShadersLen = length(Shaders),
  send_bin(Binary),
  cast(5770, <<ShadersLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Shaders>>)/binary,0:(((1+ShadersLen) rem 2)*32),Binaryformat:?GLenum>>).

%% @doc Retrieve the range and precision for numeric formats supported by the shader compiler
%%
%% ``gl:getShaderPrecisionFormat'' retrieves the numeric range and precision for the implementation's
%% representation of quantities in different numeric formats in specified shader type.  `ShaderType' 
%%  specifies the type of shader for which the numeric precision and range is to be retrieved
%% and must be one of `?GL_VERTEX_SHADER' or `?GL_FRAGMENT_SHADER'.  `PrecisionType' 
%%  specifies the numeric format to query and must be one of `?GL_LOW_FLOAT', `?GL_MEDIUM_FLOAT'
%% `?GL_HIGH_FLOAT', `?GL_LOW_INT', `?GL_MEDIUM_INT', or `?GL_HIGH_INT'.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderPrecisionFormat.xhtml">external</a> documentation.
-spec getShaderPrecisionFormat(Shadertype, Precisiontype) -> {Range :: {integer(),integer()},Precision :: integer()} when Shadertype :: enum(),Precisiontype :: enum().
getShaderPrecisionFormat(Shadertype,Precisiontype) ->
  call(5771, <<Shadertype:?GLenum,Precisiontype:?GLenum>>).

%% @doc 
%% See {@link depthRange/2}
-spec depthRangef(N, F) -> 'ok' when N :: clamp(),F :: clamp().
depthRangef(N,F) ->
  cast(5772, <<N:?GLclampf,F:?GLclampf>>).

%% @doc glClearDepthf
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec clearDepthf(D) -> 'ok' when D :: clamp().
clearDepthf(D) ->
  cast(5773, <<D:?GLclampf>>).

%% @doc Return a binary representation of a program object's compiled and linked executable source
%%
%% ``gl:getProgramBinary'' returns a binary representation of the compiled and linked executable
%% for  `Program'  into the array of bytes whose address is specified in  `Binary' .
%% The maximum number of bytes that may be written into  `Binary'  is specified by  `BufSize' 
%% . If the program binary is greater in size than  `BufSize'  bytes, then an error is
%% generated, otherwise the actual number of bytes written into  `Binary'  is returned
%% in the variable whose address is given by  `Length' . If  `Length'  is `?NULL',
%% then no length is returned. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramBinary.xhtml">external</a> documentation.
-spec getProgramBinary(Program, BufSize) -> {BinaryFormat :: enum(),Binary :: binary()} when Program :: integer(),BufSize :: integer().
getProgramBinary(Program,BufSize) ->
  call(5774, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @doc Load a program object with a program binary
%%
%% ``gl:programBinary'' loads a program object with a program binary previously returned
%% from  {@link gl:getProgramBinary/2} .  `BinaryFormat'  and  `Binary'  must be those
%% returned by a previous call to  {@link gl:getProgramBinary/2} , and  `Length'  must be
%% the length returned by  {@link gl:getProgramBinary/2} , or by  {@link gl:getProgramiv/2}  when
%% called with  `Pname'  set to `?GL_PROGRAM_BINARY_LENGTH'. If these conditions
%% are not met, loading the program binary will fail and  `Program' 's `?GL_LINK_STATUS'
%%  will be set to `?GL_FALSE'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramBinary.xhtml">external</a> documentation.
-spec programBinary(Program, BinaryFormat, Binary) -> 'ok' when Program :: integer(),BinaryFormat :: enum(),Binary :: binary().
programBinary(Program,BinaryFormat,Binary) ->
  send_bin(Binary),
  cast(5775, <<Program:?GLuint,BinaryFormat:?GLenum>>).

%% @doc Specify a parameter for a program object
%%
%% ``gl:programParameter'' specifies a new value for the parameter nameed by  `Pname' 
%% for the program object  `Program' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramParameter.xhtml">external</a> documentation.
-spec programParameteri(Program, Pname, Value) -> 'ok' when Program :: integer(),Pname :: enum(),Value :: integer().
programParameteri(Program,Pname,Value) ->
  cast(5776, <<Program:?GLuint,Pname:?GLenum,Value:?GLint>>).

%% @doc Bind stages of a program object to a program pipeline
%%
%% ``gl:useProgramStages'' binds executables from a program object associated with a specified
%% set of shader stages to the program pipeline object given by  `Pipeline' .  `Pipeline' 
%%  specifies the program pipeline object to which to bind the executables.  `Stages' 
%% contains a logical combination of bits indicating the shader stages to use within  `Program' 
%%  with the program pipeline object  `Pipeline' .  `Stages'  must be a logical combination
%% of `?GL_VERTEX_SHADER_BIT', `?GL_TESS_CONTROL_SHADER_BIT', `?GL_TESS_EVALUATION_SHADER_BIT'
%% , `?GL_GEOMETRY_SHADER_BIT', and `?GL_FRAGMENT_SHADER_BIT'. Additionally, the
%% special value `?GL_ALL_SHADER_BITS' may be specified to indicate that all executables
%% contained in  `Program'  should be installed in  `Pipeline' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgramStages.xhtml">external</a> documentation.
-spec useProgramStages(Pipeline, Stages, Program) -> 'ok' when Pipeline :: integer(),Stages :: integer(),Program :: integer().
useProgramStages(Pipeline,Stages,Program) ->
  cast(5777, <<Pipeline:?GLuint,Stages:?GLbitfield,Program:?GLuint>>).

%% @doc Set the active program object for a program pipeline object
%%
%% ``gl:activeShaderProgram'' sets the linked program named by  `Program'  to be the
%% active program for the program pipeline object  `Pipeline' . The active program in
%% the active program pipeline object is the target of calls to  {@link gl:uniform1f/2}  when
%% no program has been made current through a call to  {@link gl:useProgram/1} . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveShaderProgram.xhtml">external</a> documentation.
-spec activeShaderProgram(Pipeline, Program) -> 'ok' when Pipeline :: integer(),Program :: integer().
activeShaderProgram(Pipeline,Program) ->
  cast(5778, <<Pipeline:?GLuint,Program:?GLuint>>).

%% @doc glCreateShaderProgramv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec createShaderProgramv(Type, Strings) -> integer() when Type :: enum(),Strings :: iolist().
createShaderProgramv(Type,Strings) ->
  StringsTemp = list_to_binary([[Str|[0]] || Str <- Strings ]),
  StringsLen = length(Strings),
  call(5779, <<Type:?GLenum,StringsLen:?GLuint,(size(StringsTemp)):?GLuint,(StringsTemp)/binary,0:((8-((size(StringsTemp)+0) rem 8)) rem 8)>>).

%% @doc Bind a program pipeline to the current context
%%
%% ``gl:bindProgramPipeline'' binds a program pipeline object to the current context.  `Pipeline' 
%%  must be a name previously returned from a call to  {@link gl:genProgramPipelines/1} . If
%% no program pipeline exists with name  `Pipeline'  then a new pipeline object is created
%% with that name and initialized to the default state vector. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindProgramPipeline.xhtml">external</a> documentation.
-spec bindProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
bindProgramPipeline(Pipeline) ->
  cast(5780, <<Pipeline:?GLuint>>).

%% @doc Delete program pipeline objects
%%
%% ``gl:deleteProgramPipelines'' deletes the  `N'  program pipeline objects whose names
%% are stored in the array  `Pipelines' . Unused names in  `Pipelines'  are ignored,
%% as is the name zero. After a program pipeline object is deleted, its name is again unused
%% and it has no contents. If program pipeline object that is currently bound is deleted,
%% the binding for that object reverts to zero and no program pipeline object becomes current.
%% 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgramPipelines.xhtml">external</a> documentation.
-spec deleteProgramPipelines(Pipelines) -> 'ok' when Pipelines :: [integer()].
deleteProgramPipelines(Pipelines) ->
  PipelinesLen = length(Pipelines),
  cast(5781, <<PipelinesLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Pipelines>>)/binary,0:(((1+PipelinesLen) rem 2)*32)>>).

%% @doc Reserve program pipeline object names
%%
%% ``gl:genProgramPipelines'' returns  `N'  previously unused program pipeline object
%% names in  `Pipelines' . These names are marked as used, for the purposes of ``gl:genProgramPipelines''
%%  only, but they acquire program pipeline state only when they are first bound. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenProgramPipelines.xhtml">external</a> documentation.
-spec genProgramPipelines(N) -> [integer()] when N :: integer().
genProgramPipelines(N) ->
  call(5782, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a program pipeline object
%%
%% ``gl:isProgramPipeline'' returns `?GL_TRUE' if  `Pipeline'  is currently the
%% name of a program pipeline object. If  `Pipeline'  is zero, or if `?pipeline'
%% is not the name of a program pipeline object, or if an error occurs, ``gl:isProgramPipeline''
%%  returns `?GL_FALSE'. If  `Pipeline'  is a name returned by  {@link gl:genProgramPipelines/1} 
%% , but that has not yet been bound through a call to  {@link gl:bindProgramPipeline/1} , then
%% the name is not a program pipeline object and ``gl:isProgramPipeline'' returns `?GL_FALSE'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgramPipeline.xhtml">external</a> documentation.
-spec isProgramPipeline(Pipeline) -> 0|1 when Pipeline :: integer().
isProgramPipeline(Pipeline) ->
  call(5783, <<Pipeline:?GLuint>>).

%% @doc Retrieve properties of a program pipeline object
%%
%% ``gl:getProgramPipelineiv'' retrieves the value of a property of the program pipeline
%% object  `Pipeline' .  `Pname'  specifies the name of the parameter whose value to
%% retrieve. The value of the parameter is written to the variable whose address is given
%% by  `Params' . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipeline.xhtml">external</a> documentation.
-spec getProgramPipelineiv(Pipeline, Pname) -> integer() when Pipeline :: integer(),Pname :: enum().
getProgramPipelineiv(Pipeline,Pname) ->
  call(5784, <<Pipeline:?GLuint,Pname:?GLenum>>).

%% @doc Specify the value of a uniform variable for a specified program object
%%
%% ``gl:programUniform'' modifies the value of a uniform variable or a uniform variable
%% array. The location of the uniform variable to be modified is specified by  `Location' ,
%% which should be a value returned by  {@link gl:getUniformLocation/2} . ``gl:programUniform''
%%  operates on the program object specified by  `Program' .
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramUniform.xhtml">external</a> documentation.
-spec programUniform1i(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1i(Program,Location,V0) ->
  cast(5785, <<Program:?GLuint,Location:?GLint,V0:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1iv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5786, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((1+ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1f(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1f(Program,Location,V0) ->
  cast(5787, <<Program:?GLuint,Location:?GLint,V0:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1fv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5788, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((1+ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1d(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1d(Program,Location,V0) ->
  cast(5789, <<Program:?GLuint,Location:?GLint,V0:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1dv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5790, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1ui(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1ui(Program,Location,V0) ->
  cast(5791, <<Program:?GLuint,Location:?GLint,V0:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1uiv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5792, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((1+ValueLen) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2i(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2i(Program,Location,V0,V1) ->
  cast(5793, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2iv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5794, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2f(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2f(Program,Location,V0,V1) ->
  cast(5795, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2fv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5796, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2d(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2d(Program,Location,V0,V1) ->
  cast(5797, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2dv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5798, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2ui(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2ui(Program,Location,V0,V1) ->
  cast(5799, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2uiv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5800, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3i(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3i(Program,Location,V0,V1,V2) ->
  cast(5801, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3iv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5802, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3f(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3f(Program,Location,V0,V1,V2) ->
  cast(5803, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3fv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5804, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3d(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3d(Program,Location,V0,V1,V2) ->
  cast(5805, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3dv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5806, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3ui(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3ui(Program,Location,V0,V1,V2) ->
  cast(5807, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3uiv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5808, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4i(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4i(Program,Location,V0,V1,V2,V3) ->
  cast(5809, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4iv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5810, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4f(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4f(Program,Location,V0,V1,V2,V3) ->
  cast(5811, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4fv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5812, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4d(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4d(Program,Location,V0,V1,V2,V3) ->
  cast(5813, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4dv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5814, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4ui(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4ui(Program,Location,V0,V1,V2,V3) ->
  cast(5815, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4uiv(Program,Location,Value) ->
  ValueLen = length(Value),
  cast(5816, <<Program:?GLuint,Location:?GLint,ValueLen:?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5817, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5818, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5819, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5820, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5821, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5822, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5823, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5824, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5825, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5826, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5827, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5828, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,ValueLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5829, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5830, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5831, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5832, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5833, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) ->
  ValueLen = length(Value),
  cast(5834, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,ValueLen:?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc Validate a program pipeline object against current GL state
%%
%% ``gl:validateProgramPipeline'' instructs the implementation to validate the shader executables
%% contained in  `Pipeline'  against the current GL state. The implementation may use
%% this as an opportunity to perform any internal shader modifications that may be required
%% to ensure correct operation of the installed shaders given the current GL state. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgramPipeline.xhtml">external</a> documentation.
-spec validateProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
validateProgramPipeline(Pipeline) ->
  cast(5835, <<Pipeline:?GLuint>>).

%% @doc Retrieve the info log string from a program pipeline object
%%
%% ``gl:getProgramPipelineInfoLog'' retrieves the info log for the program pipeline object
%%  `Pipeline' . The info log, including its null terminator, is written into the array
%% of characters whose address is given by  `InfoLog' . The maximum number of characters
%% that may be written into  `InfoLog'  is given by  `BufSize' , and the actual number
%% of characters written into  `InfoLog'  is returned in the integer whose address is
%% given by  `Length' . If  `Length'  is `?NULL', no length is returned. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipelineInfoLog.xhtml">external</a> documentation.
-spec getProgramPipelineInfoLog(Pipeline, BufSize) -> string() when Pipeline :: integer(),BufSize :: integer().
getProgramPipelineInfoLog(Pipeline,BufSize) ->
  call(5836, <<Pipeline:?GLuint,BufSize:?GLsizei>>).

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttribL1d(Index,X) ->
  cast(5837, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttribL2d(Index,X,Y) ->
  cast(5838, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttribL3d(Index,X,Y,Z) ->
  cast(5839, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribL4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttribL4d(Index,X,Y,Z,W) ->
  cast(5840, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @equiv vertexAttribL1d(Index,X)
-spec vertexAttribL1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttribL1dv(Index,{X}) ->  vertexAttribL1d(Index,X).

%% @equiv vertexAttribL2d(Index,X,Y)
-spec vertexAttribL2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttribL2dv(Index,{X,Y}) ->  vertexAttribL2d(Index,X,Y).

%% @equiv vertexAttribL3d(Index,X,Y,Z)
-spec vertexAttribL3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttribL3dv(Index,{X,Y,Z}) ->  vertexAttribL3d(Index,X,Y,Z).

%% @equiv vertexAttribL4d(Index,X,Y,Z,W)
-spec vertexAttribL4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttribL4dv(Index,{X,Y,Z,W}) ->  vertexAttribL4d(Index,X,Y,Z,W).

%% @doc glVertexAttribLPointer
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec vertexAttribLPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5841, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5842, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc glGetVertexAttribL
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getVertexAttribLdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribLdv(Index,Pname) ->
  call(5843, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc glViewportArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec viewportArrayv(First, V) -> 'ok' when First :: integer(),V :: [{float(),float(),float(),float()}].
viewportArrayv(First,V) ->
  VLen = length(V),
  cast(5844, <<First:?GLuint,VLen:?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- V>>)/binary>>).

%% @doc Set a specified viewport
%%
%% ``gl:viewportIndexedf'' and ``gl:viewportIndexedfv'' specify the parameters for a
%% single viewport.  `Index'  specifies the index of the viewport to modify.  `Index' 
%% must be less than the value of `?GL_MAX_VIEWPORTS'. For ``gl:viewportIndexedf'',  `X' 
%% ,  `Y' ,  `W' , and  `H'  specify the left, bottom, width and height of the viewport
%% in pixels, respectively. For ``gl:viewportIndexedfv'',  `V'  contains the address
%% of an array of floating point values specifying the left ( x), bottom ( y), width ( w),
%% and height ( h) of each viewport, in that order.  x and  y give the location of the viewport's
%% lower left corner, and  w and  h give the width and height of the viewport, respectively.
%% The viewport specifies the affine transformation of   x and   y from normalized device
%% coordinates to window coordinates. Let  (x nd y nd) be normalized device coordinates. Then the window
%% coordinates  (x w y w) are computed as follows: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewportIndexed.xhtml">external</a> documentation.
-spec viewportIndexedf(Index, X, Y, W, H) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),W :: float(),H :: float().
viewportIndexedf(Index,X,Y,W,H) ->
  cast(5845, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,W:?GLfloat,H:?GLfloat>>).

%% @doc 
%% See {@link viewportIndexedf/5}
-spec viewportIndexedfv(Index, V) -> 'ok' when Index :: integer(),V :: {float(),float(),float(),float()}.
viewportIndexedfv(Index,{V1,V2,V3,V4}) ->
  cast(5846, <<Index:?GLuint,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>>).

%% @doc glScissorArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorArrayv(First, V) -> 'ok' when First :: integer(),V :: [{integer(),integer(),integer(),integer()}].
scissorArrayv(First,V) ->
  VLen = length(V),
  cast(5847, <<First:?GLuint,VLen:?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- V>>)/binary>>).

%% @doc glScissorIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorIndexed(Index, Left, Bottom, Width, Height) -> 'ok' when Index :: integer(),Left :: integer(),Bottom :: integer(),Width :: integer(),Height :: integer().
scissorIndexed(Index,Left,Bottom,Width,Height) ->
  cast(5848, <<Index:?GLuint,Left:?GLint,Bottom:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc glScissorIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scissorIndexedv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
scissorIndexedv(Index,{V1,V2,V3,V4}) ->
  cast(5849, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc glDepthRangeArrayv
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRangeArrayv(First, V) -> 'ok' when First :: integer(),V :: [{clamp(),clamp()}].
depthRangeArrayv(First,V) ->
  VLen = length(V),
  cast(5850, <<First:?GLuint,0:32,VLen:?GLuint,0:32,
        (<< <<V1:?GLclampd,V2:?GLclampd>> || {V1,V2} <- V>>)/binary>>).

%% @doc glDepthRangeIndexe
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthRangeIndexed(Index, N, F) -> 'ok' when Index :: integer(),N :: clamp(),F :: clamp().
depthRangeIndexed(Index,N,F) ->
  cast(5851, <<Index:?GLuint,0:32,N:?GLclampd,F:?GLclampd>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getFloati_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getFloati_v(Target,Index) ->
  call(5852, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getDoublei_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getDoublei_v(Target,Index) ->
  call(5853, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glDebugMessageControlARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec debugMessageControlARB(Source, Type, Severity, Ids, Enabled) -> 'ok' when Source :: enum(),Type :: enum(),Severity :: enum(),Ids :: [integer()],Enabled :: 0|1.
debugMessageControlARB(Source,Type,Severity,Ids,Enabled) ->
  IdsLen = length(Ids),
  cast(5854, <<Source:?GLenum,Type:?GLenum,Severity:?GLenum,IdsLen:?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((IdsLen) rem 2)*32),Enabled:?GLboolean>>).

%% @doc glDebugMessageInsertARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec debugMessageInsertARB(Source, Type, Id, Severity, Buf) -> 'ok' when Source :: enum(),Type :: enum(),Id :: integer(),Severity :: enum(),Buf :: string().
debugMessageInsertARB(Source,Type,Id,Severity,Buf) ->
  BufLen = length(Buf),
  cast(5855, <<Source:?GLenum,Type:?GLenum,Id:?GLuint,Severity:?GLenum,(list_to_binary([Buf|[0]]))/binary,0:((8-((BufLen+ 1) rem 8)) rem 8)>>).

%% @doc glGetDebugMessageLogARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getDebugMessageLogARB(Count, Bufsize) -> {integer(),Sources :: [enum()],Types :: [enum()],Ids :: [integer()],Severities :: [enum()],MessageLog :: [string()]} when Count :: integer(),Bufsize :: integer().
getDebugMessageLogARB(Count,Bufsize) ->
  call(5856, <<Count:?GLuint,Bufsize:?GLsizei>>).

%% @doc glGetGraphicsResetStatusARB
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getGraphicsResetStatusARB() -> enum().
getGraphicsResetStatusARB() ->
  call(5857, <<>>).

%% @doc Draw multiple instances of a range of elements with offset applied to instanced attributes
%%
%% ``gl:drawArraysInstancedBaseInstance'' behaves identically to  {@link gl:drawArrays/3} 
%% except that  `Primcount'  instances of the range of elements are executed and the value
%% of the internal counter  `InstanceID'  advances for each iteration.  `InstanceID' 
%% is an internal 32-bit integer counter that may be read by a vertex shader as `?gl_InstanceID'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysInstancedBaseInstance.xhtml">external</a> documentation.
-spec drawArraysInstancedBaseInstance(Mode, First, Count, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer(),Baseinstance :: integer().
drawArraysInstancedBaseInstance(Mode,First,Count,Primcount,Baseinstance) ->
  cast(5858, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei,Baseinstance:?GLuint>>).

%% @doc Draw multiple instances of a set of elements with offset applied to instanced attributes
%%
%% ``gl:drawElementsInstancedBaseInstance'' behaves identically to  {@link gl:drawElements/4} 
%% except that  `Primcount'  instances of the set of elements are executed and the value
%% of the internal counter  `InstanceID'  advances for each iteration.  `InstanceID' 
%% is an internal 32-bit integer counter that may be read by a vertex shader as `?gl_InstanceID'
%% . 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseInstance.xhtml">external</a> documentation.
-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Primcount,Baseinstance) when  is_integer(Indices) ->
  cast(5859, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Baseinstance:?GLuint>>);
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Primcount,Baseinstance) ->
  send_bin(Indices),
  cast(5860, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Baseinstance:?GLuint>>).

%% @doc Render multiple instances of a set of primitives from array data with a per-element offset
%%
%% ``gl:drawElementsInstancedBaseVertexBaseInstance'' behaves identically to  {@link gl:drawElementsInstanced/5} 
%%  except that the `i'th element transferred by the corresponding draw call will be
%% taken from element  `Indices' [i] +  `Basevertex'  of each enabled array. If the
%% resulting value is larger than the maximum value representable by  `Type' , it is as
%% if the calculation were upconverted to 32-bit unsigned integers (with wrapping on overflow
%% conditions). The operation is undefined if the sum would be negative. The  `Basevertex' 
%%  has no effect on the shader-visible value of `?gl_VertexID'. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml">external</a> documentation.
-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Primcount, Basevertex, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance) when  is_integer(Indices) ->
  cast(5861, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Basevertex:?GLint,Baseinstance:?GLuint>>);
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance) ->
  send_bin(Indices),
  cast(5862, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Basevertex:?GLint,Baseinstance:?GLuint>>).

%% @doc glDrawTransformFeedbackInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedbackInstanced(Mode, Id, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Primcount :: integer().
drawTransformFeedbackInstanced(Mode,Id,Primcount) ->
  cast(5863, <<Mode:?GLenum,Id:?GLuint,Primcount:?GLsizei>>).

%% @doc glDrawTransformFeedbackStreamInstance
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec drawTransformFeedbackStreamInstanced(Mode, Id, Stream, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer(),Primcount :: integer().
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Primcount) ->
  cast(5864, <<Mode:?GLenum,Id:?GLuint,Stream:?GLuint,Primcount:?GLsizei>>).

%% @doc glGetInternalformat
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getInternalformativ(Target, Internalformat, Pname, BufSize) -> [integer()] when Target :: enum(),Internalformat :: enum(),Pname :: enum(),BufSize :: integer().
getInternalformativ(Target,Internalformat,Pname,BufSize) ->
  call(5865, <<Target:?GLenum,Internalformat:?GLenum,Pname:?GLenum,BufSize:?GLsizei>>).

%% @doc Bind a level of a texture to an image unit
%%
%% ``gl:bindImageTexture'' binds a single level of a texture to an image unit for the purpose
%% of reading and writing it from shaders.  `Unit'  specifies the zero-based index of
%% the image unit to which to bind the texture level.  `Texture'  specifies the name of
%% an existing texture object to bind to the image unit. If  `Texture'  is zero, then
%% any existing binding to the image unit is broken.  `Level'  specifies the level of
%% the texture to bind to the image unit. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindImageTexture.xhtml">external</a> documentation.
-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok' when Unit :: integer(),Texture :: integer(),Level :: integer(),Layered :: 0|1,Layer :: integer(),Access :: enum(),Format :: enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) ->
  cast(5866, <<Unit:?GLuint,Texture:?GLuint,Level:?GLint,Layered:?GLboolean,0:24,Layer:?GLint,Access:?GLenum,Format:?GLenum>>).

%% @doc Defines a barrier ordering memory transactions
%%
%% ``gl:memoryBarrier'' defines a barrier ordering the memory transactions issued prior
%% to the command relative to those issued after the barrier. For the purposes of this ordering,
%% memory transactions performed by shaders are considered to be issued by the rendering
%% command that triggered the execution of the shader.  `Barriers'  is a bitfield indicating
%% the set of operations that are synchronized with shader stores; the bits used in  `Barriers' 
%%  are as follows: 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMemoryBarrier.xhtml">external</a> documentation.
-spec memoryBarrier(Barriers) -> 'ok' when Barriers :: integer().
memoryBarrier(Barriers) ->
  cast(5867, <<Barriers:?GLbitfield>>).

%% @doc Simultaneously specify storage for all levels of a one-dimensional texture
%%
%% ``gl:texStorage1D'' specifies the storage requirements for all levels of a one-dimensional
%% texture simultaneously. Once a texture is specified with this command, the format and
%% dimensions of all levels become immutable unless it is a proxy texture. The contents of
%% the image may still be modified, however, its storage requirements may not change. Such
%% a texture is referred to as an `immutable-format' texture. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage1D.xhtml">external</a> documentation.
-spec texStorage1D(Target, Levels, Internalformat, Width) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer().
texStorage1D(Target,Levels,Internalformat,Width) ->
  cast(5868, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei>>).

%% @doc Simultaneously specify storage for all levels of a two-dimensional or one-dimensional array texture
%%
%% ``gl:texStorage2D'' specifies the storage requirements for all levels of a two-dimensional
%% texture or one-dimensional texture array simultaneously. Once a texture is specified with
%% this command, the format and dimensions of all levels become immutable unless it is a
%% proxy texture. The contents of the image may still be modified, however, its storage requirements
%% may not change. Such a texture is referred to as an `immutable-format' texture. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage2D.xhtml">external</a> documentation.
-spec texStorage2D(Target, Levels, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
texStorage2D(Target,Levels,Internalformat,Width,Height) ->
  cast(5869, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Simultaneously specify storage for all levels of a three-dimensional, two-dimensional array or cube-map array texture
%%
%% ``gl:texStorage3D'' specifies the storage requirements for all levels of a three-dimensional,
%% two-dimensional array or cube-map array texture simultaneously. Once a texture is specified
%% with this command, the format and dimensions of all levels become immutable unless it
%% is a proxy texture. The contents of the image may still be modified, however, its storage
%% requirements may not change. Such a texture is referred to as an `immutable-format'
%% texture. 
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage3D.xhtml">external</a> documentation.
-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) ->
  cast(5870, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei>>).

%% @doc glDepthBoundsEXT
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec depthBoundsEXT(Zmin, Zmax) -> 'ok' when Zmin :: clamp(),Zmax :: clamp().
depthBoundsEXT(Zmin,Zmax) ->
  cast(5871, <<Zmin:?GLclampd,Zmax:?GLclampd>>).

%% @doc glStencilClearTagEXT
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec stencilClearTagEXT(StencilTagBits, StencilClearTag) -> 'ok' when StencilTagBits :: integer(),StencilClearTag :: integer().
stencilClearTagEXT(StencilTagBits,StencilClearTag) ->
  cast(5872, <<StencilTagBits:?GLsizei,StencilClearTag:?GLuint>>).

