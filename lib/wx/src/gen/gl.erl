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

%% OPENGL API

%% This file is generated DO NOT EDIT

%% @doc  Standard OpenGL api.
%% See <a href="http://www.opengl.org/sdk/docs/man/">www.opengl.org</a>
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
    rec().
    
%% @hidden
cast(Op, Args) ->
    Port = get(opengl_port), 
    _ = erlang:port_control(Port,Op,Args),
    ok.
    
%% @hidden
rec() ->
    receive 
        {'_egl_result_', Res} -> Res;
        {'_egl_error_',  Op, Res} -> error({error,Res,Op})
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearIndex.xml">external</a> documentation.
-spec clearIndex(C) -> 'ok' when C :: float().
clearIndex(C) ->
  cast(5037, <<C:?GLfloat>>).

%% @doc Specify clear values for the color buffers
%%
%% ``gl:clearColor'' specifies the red, green, blue, and alpha values used by  {@link gl:clear/1} 
%%  to clear the color buffers. Values specified by ``gl:clearColor'' are clamped to the
%% range  [0 1]. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearColor.xml">external</a> documentation.
-spec clearColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
clearColor(Red,Green,Blue,Alpha) ->
  cast(5038, <<Red:?GLclampf,Green:?GLclampf,Blue:?GLclampf,Alpha:?GLclampf>>).

%% @doc Clear buffers to preset values
%%
%% ``gl:clear'' sets the bitplane area of the window to values previously selected by ``gl:clearColor''
%% , ``gl:clearDepth'', and ``gl:clearStencil''. Multiple color buffers can be cleared
%% simultaneously by selecting more than one buffer at a time using  {@link gl:drawBuffer/1} . 
%%
%%  The pixel ownership test, the scissor test, dithering, and the buffer writemasks affect
%% the operation of ``gl:clear''. The scissor box bounds the cleared region. Alpha function,
%% blend function, logical operation, stenciling, texture mapping, and depth-buffering are
%% ignored by ``gl:clear''. 
%%
%% ``gl:clear'' takes a single argument that is the bitwise OR of several values indicating
%% which buffer is to be cleared. 
%%
%%  The values are as follows: 
%%
%% `?GL_COLOR_BUFFER_BIT':  Indicates the buffers currently enabled for color writing. 
%%
%% `?GL_DEPTH_BUFFER_BIT':  Indicates the depth buffer. 
%%
%% `?GL_STENCIL_BUFFER_BIT':  Indicates the stencil buffer. 
%%
%%  The value to which each buffer is cleared depends on the setting of the clear value for
%% that buffer. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClear.xml">external</a> documentation.
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
%%  This mask is used only in color index mode, and it affects only the buffers currently
%% selected for writing (see  {@link gl:drawBuffer/1} ). Initially, all bits are enabled for
%% writing. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexMask.xml">external</a> documentation.
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
%%  Changes to individual bits of components cannot be controlled. Rather, changes are either
%% enabled or disabled for entire color components. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMask.xml">external</a> documentation.
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
%%  `Func'  and  `Ref'  specify the conditions under which the pixel is drawn. The
%% incoming alpha value is compared to  `Ref'  using the function specified by  `Func' .
%% If the value passes the comparison, the incoming fragment is drawn if it also passes subsequent
%% stencil and depth buffer tests. If the value fails the comparison, no change is made to
%% the frame buffer at that pixel location. The comparison functions are as follows: 
%%
%% `?GL_NEVER':  Never passes. 
%%
%% `?GL_LESS':  Passes if the incoming alpha value is less than the reference value. 
%%
%% `?GL_EQUAL':  Passes if the incoming alpha value is equal to the reference value. 
%%
%% `?GL_LEQUAL':  Passes if the incoming alpha value is less than or equal to the reference
%% value. 
%%
%% `?GL_GREATER':  Passes if the incoming alpha value is greater than the reference
%% value. 
%%
%% `?GL_NOTEQUAL':  Passes if the incoming alpha value is not equal to the reference
%% value. 
%%
%% `?GL_GEQUAL':  Passes if the incoming alpha value is greater than or equal to the
%% reference value. 
%%
%% `?GL_ALWAYS':  Always passes (initial value). 
%%
%% ``gl:alphaFunc'' operates on all pixel write operations, including those resulting from
%% the scan conversion of points, lines, polygons, and bitmaps, and from pixel draw and copy
%% operations. ``gl:alphaFunc'' does not affect screen clear operations. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAlphaFunc.xml">external</a> documentation.
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
%% ``gl:blendFunc'' defines the operation of blending for all draw buffers when it is enabled.
%% ``gl:blendFunci'' defines the operation of blending for a single draw buffer specified
%% by  `Buf'  when enabled for that draw buffer.  `Sfactor'  specifies which method
%% is used to scale the source color components.  `Dfactor'  specifies which method is
%% used to scale the destination color components. Both parameters must be one of the following
%% symbolic constants: `?GL_ZERO', `?GL_ONE', `?GL_SRC_COLOR', `?GL_ONE_MINUS_SRC_COLOR'
%% , `?GL_DST_COLOR', `?GL_ONE_MINUS_DST_COLOR', `?GL_SRC_ALPHA', `?GL_ONE_MINUS_SRC_ALPHA'
%% , `?GL_DST_ALPHA', `?GL_ONE_MINUS_DST_ALPHA', `?GL_CONSTANT_COLOR', `?GL_ONE_MINUS_CONSTANT_COLOR'
%% , `?GL_CONSTANT_ALPHA', `?GL_ONE_MINUS_CONSTANT_ALPHA', `?GL_SRC_ALPHA_SATURATE'
%% , `?GL_SRC1_COLOR', `?GL_ONE_MINUS_SRC1_COLOR', `?GL_SRC1_ALPHA', and `?GL_ONE_MINUS_SRC1_ALPHA'
%% . The possible methods are described in the following table. Each method defines four
%% scale factors, one each for red, green, blue, and alpha. In the table and in subsequent
%% equations, first source, second source and destination color components are referred to
%% as (R s0 G s0 B s0 A s0), (R s1 G s1 B s1 A s1) and (R d G d B d A d), respectively. The color specified by  {@link gl:blendColor/4}  is referred to
%% as (R c G c B c A c). They are understood to have integer values between 0 and (k R k G k B k A), where 
%%
%%  k c=2(m c)-1
%%
%%  and (m R m G m B m A) is the number of red, green, blue, and alpha bitplanes. 
%%
%%  Source and destination scale factors are referred to as (s R s G s B s A) and (d R d G d B d A). The scale factors described
%% in the table, denoted  (f R f G f B f A), represent either source or destination factors. All scale factors
%% have range  [0 1]. 
%%
%% <table><tbody><tr><td>` Parameter '</td><td>(f R f G f B f A)</td></tr></tbody><tbody><tr><td>`?GL_ZERO'
%% </td><td>(0 0 0 0)</td></tr><tr><td>`?GL_ONE'</td><td>(1 1 1 1)</td></tr><tr><td>`?GL_SRC_COLOR'</td>
%% <td>(R s0 k/R G s0 k/G B s0 k/B A s0 k/A)</td></tr><tr><td>`?GL_ONE_MINUS_SRC_COLOR'</td><td>(1 1 1 1)-(R s0 k/R G s0 k/G B s0 k/B
%% A s0 k/A)</td></tr><tr><td>`?GL_DST_COLOR'
%% </td><td>(R d k/R G d k/G B d k/B A d k/A)</td></tr><tr><td>`?GL_ONE_MINUS_DST_COLOR'</td><td>(1 1 1 1)-(R d k/R G d k/G B d k/B
%%  A d k/A)</td></tr><tr><td>`?GL_SRC_ALPHA'
%% </td><td>(A s0 k/A A s0 k/A A s0 k/A A s0 k/A)</td></tr><tr><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td>(1 1 1 1)-(A s0 k/A A s0 k/A A s0
%% k/A A s0 k/A)</td></tr><tr><td>`?GL_DST_ALPHA'
%% </td><td>(A d k/A A d k/A A d k/A A d k/A)</td></tr><tr><td>`?GL_ONE_MINUS_DST_ALPHA'</td><td>(1 1 1 1)-(A d k/A A d k/A A d k/A
%%  A d k/A)</td></tr><tr><td>`?GL_CONSTANT_COLOR'
%% </td><td>(R c G c B c A c)</td></tr><tr><td>`?GL_ONE_MINUS_CONSTANT_COLOR'</td><td>(1 1 1 1)-(R c G c B c A c)</td></tr><tr><td>
%% `?GL_CONSTANT_ALPHA'</td><td>(A c A c A c A c)</td></tr><tr><td>`?GL_ONE_MINUS_CONSTANT_ALPHA'</td>
%% <td>(1 1 1 1)-(A c A c A c A c)</td></tr><tr><td>`?GL_SRC_ALPHA_SATURATE'</td><td>(i i i 1)</td></tr><tr><td>`?GL_SRC1_COLOR'
%% </td><td>(R s1 k/R G s1 k/G B s1 k/B A s1 k/A)</td></tr><tr><td>`?GL_ONE_MINUS_SRC1_COLOR'</td><td>(1 1 1 1)-(R s1 k/R G s1 k/G B
%% s1 k/B A s1 k/A)</td></tr><tr><td>`?GL_SRC1_ALPHA'
%% </td><td>(A s1 k/A A s1 k/A A s1 k/A A s1 k/A)</td></tr><tr><td>`?GL_ONE_MINUS_SRC1_ALPHA'</td><td>(1 1 1 1)-(A s1 k/A A s1 k/A A
%% s1 k/A A s1 k/A)</td></tr></tbody></table>
%% 
%%
%%  In the table, 
%%
%%  i=min(A s k A-A d) k/A
%%
%%  To determine the blended RGBA values of a pixel, the system uses the following equations:
%% 
%%
%%  R d=min(k R R s  s R+R d  d R) G d=min(k G G s  s G+G d  d G) B d=min(k B B s  s B+B d  d B) A d=min(k A A s  s A+A d  d A)
%%
%%  Despite the apparent precision of the above equations, blending arithmetic is not exactly
%% specified, because blending operates with imprecise integer color values. However, a blend
%% factor that should be equal to 1 is guaranteed not to modify its multiplicand, and a blend
%% factor equal to 0 reduces its multiplicand to 0. For example, when  `Sfactor'  is `?GL_SRC_ALPHA'
%% ,  `Dfactor'  is `?GL_ONE_MINUS_SRC_ALPHA', and   A s is equal to   k A, the equations
%% reduce to simple replacement: 
%%
%%  R d=R s G d=G s B d=B s A d=A s
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunc.xml">external</a> documentation.
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
%% <table><tbody><tr><td>` Opcode '</td><td>` Resulting Operation '</td></tr></tbody>
%% <tbody><tr><td>`?GL_CLEAR'</td><td> 0 </td></tr><tr><td>`?GL_SET'</td><td> 1 </td>
%% </tr><tr><td>`?GL_COPY'</td><td> s </td></tr><tr><td>`?GL_COPY_INVERTED'</td><td>
%%  ~s </td></tr><tr><td>`?GL_NOOP'</td><td> d </td></tr><tr><td>`?GL_INVERT'</td><td>
%%  ~d </td></tr><tr><td>`?GL_AND'</td><td> s &amp; d </td></tr><tr><td>`?GL_NAND'</td>
%% <td> ~(s &amp; d) </td></tr><tr><td>`?GL_OR'</td><td> s | d </td></tr><tr><td>`?GL_NOR'
%% </td><td> ~(s | d) </td></tr><tr><td>`?GL_XOR'</td><td> s ^ d </td></tr><tr><td>`?GL_EQUIV'
%% </td><td> ~(s ^ d) </td></tr><tr><td>`?GL_AND_REVERSE'</td><td> s &amp; ~d </td></tr>
%% <tr><td>`?GL_AND_INVERTED'</td><td> ~s &amp; d </td></tr><tr><td>`?GL_OR_REVERSE'
%% </td><td> s | ~d </td></tr><tr><td>`?GL_OR_INVERTED'</td><td> ~s | d </td></tr></tbody>
%% </table>
%%
%%  `Opcode'  is a symbolic constant chosen from the list above. In the explanation of
%% the logical operations, `s' represents the incoming color and `d' represents
%% the color in the frame buffer. Standard C-language operators are used. As these bitwise
%% operators suggest, the logical operation is applied independently to each bit pair of
%% the source and destination colors. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLogicOp.xml">external</a> documentation.
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
%%  {@link gl:frontFace/1}  specifies which of the clockwise and counterclockwise facets are
%% front-facing and back-facing. See  {@link gl:frontFace/1} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCullFace.xml">external</a> documentation.
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
%%  The projection of a polygon to window coordinates is said to have clockwise winding if
%% an imaginary object following the path from its first vertex, its second vertex, and so
%% on, to its last vertex, and finally back to its first vertex, moves in a clockwise direction
%% about the interior of the polygon. The polygon's winding is said to be counterclockwise
%% if the imaginary object following the same path moves in a counterclockwise direction
%% about the interior of the polygon. ``gl:frontFace'' specifies whether polygons with
%% clockwise winding in window coordinates, or counterclockwise winding in window coordinates,
%% are taken to be front-facing. Passing `?GL_CCW' to  `Mode'  selects counterclockwise
%% polygons as front-facing; `?GL_CW' selects clockwise polygons as front-facing. By
%% default, counterclockwise polygons are taken to be front-facing. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrontFace.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointSize.xml">external</a> documentation.
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
%%  If line antialiasing is disabled, the actual width is determined by rounding the supplied
%% width to the nearest integer. (If the rounding results in the value 0, it is as if the
%% line width were 1.) If |&amp;Delta; x|&gt;=|&amp;Delta; y|, `i' pixels are filled in each column that is rasterized,
%% where `i' is the rounded value of  `Width' . Otherwise, `i' pixels are filled
%% in each row that is rasterized. 
%%
%%  If antialiasing is enabled, line rasterization produces a fragment for each pixel square
%% that intersects the region lying within the rectangle having width equal to the current
%% line width, length equal to the actual length of the line, and centered on the mathematical
%% line segment. The coverage value for each fragment is the window coordinate area of the
%% intersection of the rectangular region with the corresponding pixel square. This value
%% is saved and used in the final rasterization step. 
%%
%%  Not all widths can be supported when line antialiasing is enabled. If an unsupported
%% width is requested, the nearest supported width is used. Only width 1 is guaranteed to
%% be supported; others depend on the implementation. Likewise, there is a range for aliased
%% line widths as well. To query the range of supported widths and the size difference between
%% supported widths within the range, call  {@link gl:getBooleanv/1}  with arguments `?GL_ALIASED_LINE_WIDTH_RANGE'
%% , `?GL_SMOOTH_LINE_WIDTH_RANGE', and `?GL_SMOOTH_LINE_WIDTH_GRANULARITY'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineWidth.xml">external</a> documentation.
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
%%  Counter   s is reset to 0 whenever  {@link gl:'begin'/1}  is called and before each line segment
%% of a  {@link gl:'begin'/1} (`?GL_LINES')/ {@link gl:'begin'/1}  sequence is generated. It is
%% incremented after each fragment of a unit width aliased line segment is generated or after
%% each   i fragments of an   i width line segment are generated. The   i fragments associated
%% with count   s are masked out if 
%%
%%  `Pattern'  bit  (s/factor)% 16
%%
%%  is 0, otherwise these fragments are sent to the frame buffer. Bit zero of  `Pattern' 
%% is the least significant bit. 
%%
%%  Antialiased lines are treated as a sequence of   1×width rectangles for purposes of stippling.
%% Whether rectangle   s is rasterized or not depends on the fragment rule described for
%% aliased lines, counting rectangles rather than groups of fragments. 
%%
%%  To enable and disable line stippling, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%% with argument `?GL_LINE_STIPPLE'. When enabled, the line stipple pattern is applied
%% as described above. When disabled, it is as if the pattern were all 1's. Initially, line
%% stippling is disabled. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLineStipple.xml">external</a> documentation.
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
%%  Three modes are defined and can be specified in  `Mode' : 
%%
%% `?GL_POINT':  Polygon vertices that are marked as the start of a boundary edge are
%% drawn as points. Point attributes such as `?GL_POINT_SIZE' and `?GL_POINT_SMOOTH'
%%  control the rasterization of the points. Polygon rasterization attributes other than `?GL_POLYGON_MODE'
%%  have no effect. 
%%
%% `?GL_LINE':  Boundary edges of the polygon are drawn as line segments. Line attributes
%% such as `?GL_LINE_WIDTH' and `?GL_LINE_SMOOTH' control the rasterization of
%% the lines. Polygon rasterization attributes other than `?GL_POLYGON_MODE' have no
%% effect. 
%%
%% `?GL_FILL':  The interior of the polygon is filled. Polygon attributes such as `?GL_POLYGON_SMOOTH'
%%  control the rasterization of the polygon. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonMode.xml">external</a> documentation.
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
%% ``gl:polygonOffset'' is useful for rendering hidden-line images, for applying decals
%% to surfaces, and for rendering solids with highlighted edges. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonOffset.xml">external</a> documentation.
-spec polygonOffset(Factor, Units) -> 'ok' when Factor :: float(),Units :: float().
polygonOffset(Factor,Units) ->
  cast(5051, <<Factor:?GLfloat,Units:?GLfloat>>).

%% @doc Set the polygon stippling pattern
%%
%%  Polygon stippling, like line stippling (see  {@link gl:lineStipple/2} ), masks out certain
%% fragments produced by rasterization, creating a pattern. Stippling is independent of polygon
%% antialiasing. 
%%
%%  `Pattern'  is a pointer to a   32×32 stipple pattern that is stored in memory just
%% like the pixel data supplied to a  {@link gl:drawPixels/5}  call with  height and `width'
%%  both equal to 32, a pixel format of `?GL_COLOR_INDEX', and data type of `?GL_BITMAP'
%% . That is, the stipple pattern is represented as a   32×32 array of 1-bit color indices
%% packed in unsigned bytes.  {@link gl:pixelStoref/2}  parameters like `?GL_UNPACK_SWAP_BYTES'
%%  and `?GL_UNPACK_LSB_FIRST' affect the assembling of the bits into a stipple pattern.
%% Pixel transfer operations (shift, offset, pixel map) are not applied to the stipple image,
%% however. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a stipple pattern is specified,  `Pattern'  is
%% treated as a byte offset into the buffer object's data store. 
%%
%%  To enable and disable polygon stippling, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%% with argument `?GL_POLYGON_STIPPLE'. Polygon stippling is initially disabled. If
%% it's enabled, a rasterized polygon fragment with window coordinates   x w and   y w is
%% sent to the next stage of the GL if and only if the ( x w% 32)th bit in the  ( y w% 32)th
%% row of the stipple pattern is 1 (one). When polygon stippling is disabled, it is as if
%% the stipple pattern consists of all 1's. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPolygonStipple.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a polygon stipple pattern is requested,  `Pattern' 
%% is treated as a byte offset into the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPolygonStipple.xml">external</a> documentation.
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
%%  The vertices of connected triangles and connected quadrilaterals are always marked as
%% boundary, regardless of the value of the edge flag. 
%%
%%  Boundary and nonboundary edge flags on vertices are significant only if `?GL_POLYGON_MODE'
%%  is set to `?GL_POINT' or `?GL_LINE'. See  {@link gl:polygonMode/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlag.xml">external</a> documentation.
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
%%  To enable and disable the scissor test, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%% with argument `?GL_SCISSOR_TEST'. The test is initially disabled. While the test
%% is enabled, only pixels that lie within the scissor box can be modified by drawing commands.
%% Window coordinates have integer values at the shared corners of frame buffer pixels. glScissor(0,0,1,1)
%%  allows modification of only the lower left pixel in the window, and glScissor(0,0,0,0)
%% doesn't allow modification of any pixels in the window. 
%%
%%  When the scissor test is disabled, it is as though the scissor box includes the entire
%% window. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissor.xml">external</a> documentation.
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
%% ``gl:clipPlane'' specifies a half-space using a four-component plane equation. When ``gl:clipPlane''
%%  is called,  `Equation'  is transformed by the inverse of the modelview matrix and
%% stored in the resulting eye coordinates. Subsequent changes to the modelview matrix have
%% no effect on the stored plane-equation components. If the dot product of the eye coordinates
%% of a vertex with the stored plane equation components is positive or zero, the vertex is `in'
%%  with respect to that clipping plane. Otherwise, it is `out'. 
%%
%%  To enable and disable clipping planes, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%% with the argument `?GL_CLIP_PLANE'`i', where `i' is the plane number. 
%%
%%  All clipping planes are initially defined as (0, 0, 0, 0) in eye coordinates and are
%% disabled. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClipPlane.xml">external</a> documentation.
-spec clipPlane(Plane, Equation) -> 'ok' when Plane :: enum(),Equation :: {float(),float(),float(),float()}.
clipPlane(Plane,{E1,E2,E3,E4}) ->
  cast(5056, <<Plane:?GLenum,0:32,E1:?GLdouble,E2:?GLdouble,E3:?GLdouble,E4:?GLdouble>>).

%% @doc Return the coefficients of the specified clipping plane
%%
%% ``gl:getClipPlane'' returns in  `Equation'  the four coefficients of the plane equation
%% for  `Plane' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetClipPlane.xml">external</a> documentation.
-spec getClipPlane(Plane) -> {float(),float(),float(),float()} when Plane :: enum().
getClipPlane(Plane) ->
  call(5057, <<Plane:?GLenum>>).

%% @doc Specify which color buffers are to be drawn into
%%
%%  When colors are written to the frame buffer, they are written into the color buffers
%% specified by ``gl:drawBuffer''. The specifications are as follows: 
%%
%% `?GL_NONE':  No color buffers are written. 
%%
%% `?GL_FRONT_LEFT':  Only the front left color buffer is written. 
%%
%% `?GL_FRONT_RIGHT':  Only the front right color buffer is written. 
%%
%% `?GL_BACK_LEFT':  Only the back left color buffer is written. 
%%
%% `?GL_BACK_RIGHT':  Only the back right color buffer is written. 
%%
%% `?GL_FRONT':  Only the front left and front right color buffers are written. If there
%% is no front right color buffer, only the front left color buffer is written. 
%%
%% `?GL_BACK':  Only the back left and back right color buffers are written. If there
%% is no back right color buffer, only the back left color buffer is written. 
%%
%% `?GL_LEFT':  Only the front left and back left color buffers are written. If there
%% is no back left color buffer, only the front left color buffer is written. 
%%
%% `?GL_RIGHT':  Only the front right and back right color buffers are written. If there
%% is no back right color buffer, only the front right color buffer is written. 
%%
%% `?GL_FRONT_AND_BACK':  All the front and back color buffers (front left, front right,
%% back left, back right) are written. If there are no back color buffers, only the front
%% left and front right color buffers are written. If there are no right color buffers, only
%% the front left and back left color buffers are written. If there are no right or back
%% color buffers, only the front left color buffer is written. 
%%
%%  If more than one color buffer is selected for drawing, then blending or logical operations
%% are computed and applied independently for each color buffer and can produce different
%% results in each buffer. 
%%
%%  Monoscopic contexts include only `left' buffers, and stereoscopic contexts include
%% both `left' and `right' buffers. Likewise, single-buffered contexts include
%% only `front' buffers, and double-buffered contexts include both `front' and `back'
%%  buffers. The context is selected at GL initialization. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffer.xml">external</a> documentation.
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
%%  Nonstereo double-buffered configurations have only a front left and a back left buffer.
%% Single-buffered configurations have a front left and a front right buffer if stereo, and
%% only a front left buffer if nonstereo. It is an error to specify a nonexistent buffer to ``gl:readBuffer''
%% . 
%%
%%  `Mode'  is initially `?GL_FRONT' in single-buffered configurations and `?GL_BACK'
%%  in double-buffered configurations. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadBuffer.xml">external</a> documentation.
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
%%  Both ``gl:enable'' and  {@link gl:enable/1}  take a single argument,  `Cap' , which
%% can assume one of the following values: 
%%
%%  Some of the GL's capabilities are indexed. ``gl:enablei'' and ``gl:disablei'' enable
%% and disable indexed capabilities. 
%%
%% `?GL_BLEND':  If enabled, blend the computed fragment color values with the values
%% in the color buffers. See  {@link gl:blendFunc/2} . 
%%
%% `?GL_CLIP_DISTANCE'`i':  If enabled, clip geometry against user-defined half
%% space `i'. 
%%
%% `?GL_COLOR_LOGIC_OP':  If enabled, apply the currently selected logical operation
%% to the computed fragment color and color buffer values. See  {@link gl:logicOp/1} . 
%%
%% `?GL_CULL_FACE':  If enabled, cull polygons based on their winding in window coordinates.
%% See  {@link gl:cullFace/1} . 
%%
%% `?GL_DEPTH_CLAMP':  If enabled, the -w c&amp;le; z c&amp;le; w c plane equation is
%% ignored by view volume clipping (effectively, there is no near or far plane clipping).
%% See  {@link gl:depthRange/2} . 
%%
%% `?GL_DEPTH_TEST':  If enabled, do depth comparisons and update the depth buffer.
%% Note that even if the depth buffer exists and the depth mask is non-zero, the depth buffer
%% is not updated if the depth test is disabled. See  {@link gl:depthFunc/1}  and  {@link gl:depthRange/2} 
%% . 
%%
%% `?GL_DITHER':  If enabled, dither color components or indices before they are written
%% to the color buffer. 
%%
%% `?GL_FRAMEBUFFER_SRGB':  If enabled and the value of `?GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING'
%%  for the framebuffer attachment corresponding to the destination buffer is `?GL_SRGB',
%% the R, G, and B destination color values (after conversion from fixed-point to floating-point)
%% are considered to be encoded for the sRGB color space and hence are linearized prior to
%% their use in blending. 
%%
%% `?GL_LINE_SMOOTH':  If enabled, draw lines with correct filtering. Otherwise, draw
%% aliased lines. See  {@link gl:lineWidth/1} . 
%%
%% `?GL_MULTISAMPLE':  If enabled, use multiple fragment samples in computing the final
%% color of a pixel. See  {@link gl:sampleCoverage/2} . 
%%
%% `?GL_POLYGON_OFFSET_FILL':  If enabled, and if the polygon is rendered in `?GL_FILL'
%%  mode, an offset is added to depth values of a polygon's fragments before the depth comparison
%% is performed. See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_OFFSET_LINE':  If enabled, and if the polygon is rendered in `?GL_LINE'
%%  mode, an offset is added to depth values of a polygon's fragments before the depth comparison
%% is performed. See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_OFFSET_POINT':  If enabled, an offset is added to depth values of a
%% polygon's fragments before the depth comparison is performed, if the polygon is rendered
%% in `?GL_POINT' mode. See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_SMOOTH':  If enabled, draw polygons with proper filtering. Otherwise,
%% draw aliased polygons. For correct antialiased polygons, an alpha buffer is needed and
%% the polygons must be sorted front to back. 
%%
%% `?GL_PRIMITIVE_RESTART':  Enables primitive restarting. If enabled, any one of the
%% draw commands  which transfers a set of generic attribute array elements to the GL will
%% restart  the primitive when the index of the vertex is equal to the primitive restart
%% index. See  {@link gl:primitiveRestartIndex/1} . 
%%
%% `?GL_SAMPLE_ALPHA_TO_COVERAGE':  If enabled, compute a temporary coverage value where
%% each bit is determined by the alpha value at the corresponding sample location. The temporary
%% coverage value is then ANDed with the fragment coverage value. 
%%
%% `?GL_SAMPLE_ALPHA_TO_ONE':  If enabled, each sample alpha value is replaced by the
%% maximum representable alpha value. 
%%
%% `?GL_SAMPLE_COVERAGE':  If enabled, the fragment's coverage is ANDed with the temporary
%% coverage value. If `?GL_SAMPLE_COVERAGE_INVERT' is set to `?GL_TRUE', invert
%% the coverage value. See  {@link gl:sampleCoverage/2} . 
%%
%% `?GL_SAMPLE_SHADING':  If enabled, the active fragment shader is run once for each
%% covered sample, or at fraction of this rate as determined by the current value of `?GL_MIN_SAMPLE_SHADING_VALUE'
%% . See  {@link gl:minSampleShading/1} . 
%%
%% `?GL_SAMPLE_MASK':  If enabled, the sample coverage mask generated for a fragment
%% during rasterization will be ANDed with the value of `?GL_SAMPLE_MASK_VALUE' before
%% shading occurs. See  {@link gl:sampleMaski/2} . 
%%
%% `?GL_SCISSOR_TEST':  If enabled, discard fragments that are outside the scissor rectangle.
%% See  {@link gl:scissor/4} . 
%%
%% `?GL_STENCIL_TEST':  If enabled, do stencil testing and update the stencil buffer.
%% See  {@link gl:stencilFunc/3}  and  {@link gl:stencilOp/3} . 
%%
%% `?GL_TEXTURE_CUBE_MAP_SEAMLESS':  If enabled, cubemap textures are sampled such that
%% when linearly sampling from the border between two adjacent faces, texels from both faces
%% are used to generate the final sample value. When disabled, texels from only a single
%% face are used to construct the final sample value. 
%%
%% `?GL_PROGRAM_POINT_SIZE':  If enabled and a vertex or geometry shader is active,
%% then the derived point size is taken from the (potentially clipped) shader builtin `?gl_PointSize'
%%  and clamped to the implementation-dependent point size range. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnable.xml">external</a> documentation.
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
%%  The following capabilities are accepted for  `Cap' : <table><tbody><tr><td>` Constant '
%% </td><td>` See '</td></tr></tbody><tbody><tr><td>`?GL_BLEND'</td><td> {@link gl:blendFunc/2} 
%% ,  {@link gl:logicOp/1} </td></tr><tr><td>`?GL_CLIP_DISTANCE'`i'</td><td> {@link gl:enable/1} 
%% </td></tr><tr><td>`?GL_COLOR_LOGIC_OP'</td><td> {@link gl:logicOp/1} </td></tr><tr><td>`?GL_CULL_FACE'
%% </td><td> {@link gl:cullFace/1} </td></tr><tr><td>`?GL_DEPTH_CLAMP'</td><td> {@link gl:enable/1} 
%% </td></tr><tr><td>`?GL_DEPTH_TEST'</td><td> {@link gl:depthFunc/1} ,  {@link gl:depthRange/2} 
%% </td></tr><tr><td>`?GL_DITHER'</td><td> {@link gl:enable/1} </td></tr><tr><td>`?GL_FRAMEBUFFER_SRGB'
%% </td><td> {@link gl:enable/1} </td></tr><tr><td>`?GL_LINE_SMOOTH'</td><td> {@link gl:lineWidth/1} 
%% </td></tr><tr><td>`?GL_MULTISAMPLE'</td><td> {@link gl:sampleCoverage/2} </td></tr><tr><td>
%% `?GL_POLYGON_SMOOTH'</td><td> {@link gl:polygonMode/2} </td></tr><tr><td>`?GL_POLYGON_OFFSET_FILL'
%% </td><td> {@link gl:polygonOffset/2} </td></tr><tr><td>`?GL_POLYGON_OFFSET_LINE'</td><td>
%%  {@link gl:polygonOffset/2} </td></tr><tr><td>`?GL_POLYGON_OFFSET_POINT'</td><td> {@link gl:polygonOffset/2} 
%% </td></tr><tr><td>`?GL_PROGRAM_POINT_SIZE'</td><td> {@link gl:enable/1} </td></tr><tr><td>
%% `?GL_PRIMITIVE_RESTART'</td><td> {@link gl:enable/1} ,  {@link gl:primitiveRestartIndex/1} </td>
%% </tr><tr><td>`?GL_SAMPLE_ALPHA_TO_COVERAGE'</td><td> {@link gl:sampleCoverage/2} </td></tr>
%% <tr><td>`?GL_SAMPLE_ALPHA_TO_ONE'</td><td> {@link gl:sampleCoverage/2} </td></tr><tr><td>
%% `?GL_SAMPLE_COVERAGE'</td><td> {@link gl:sampleCoverage/2} </td></tr><tr><td>`?GL_SAMPLE_MASK'
%% </td><td> {@link gl:enable/1} </td></tr><tr><td>`?GL_SCISSOR_TEST'</td><td> {@link gl:scissor/4} 
%% </td></tr><tr><td>`?GL_STENCIL_TEST'</td><td> {@link gl:stencilFunc/3} ,  {@link gl:stencilOp/3} 
%% </td></tr><tr><td>`?GL_TEXTURE_CUBEMAP_SEAMLESS'</td><td> {@link gl:enable/1} </td></tr>
%% </tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabled.xml">external</a> documentation.
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
%% `?GL_COLOR_ARRAY':  If enabled, the color array is enabled for writing and used during
%% rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} ,
%%  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%% is called. See  {@link gl:colorPointer/4} . 
%%
%% `?GL_EDGE_FLAG_ARRAY':  If enabled, the edge flag array is enabled for writing and
%% used during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:edgeFlagPointer/2} . 
%%
%% `?GL_FOG_COORD_ARRAY':  If enabled, the fog coordinate array is enabled for writing
%% and used during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:fogCoordPointer/3} . 
%%
%% `?GL_INDEX_ARRAY':  If enabled, the index array is enabled for writing and used during
%% rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} ,
%%  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%% is called. See  {@link gl:indexPointer/3} . 
%%
%% `?GL_NORMAL_ARRAY':  If enabled, the normal array is enabled for writing and used
%% during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:normalPointer/3} . 
%%
%% `?GL_SECONDARY_COLOR_ARRAY':  If enabled, the secondary color array is enabled for
%% writing and used during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:colorPointer/4} . 
%%
%% `?GL_TEXTURE_COORD_ARRAY':  If enabled, the texture coordinate array is enabled for
%% writing and used during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:texCoordPointer/4} . 
%%
%% `?GL_VERTEX_ARRAY':  If enabled, the vertex array is enabled for writing and used
%% during rendering when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:drawElements/4} 
%% ,  {@link gl:drawRangeElements/6}  {@link gl:multiDrawArrays/3} , or see `glMultiDrawElements'
%%  is called. See  {@link gl:vertexPointer/4} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableClientState.xml">external</a> documentation.
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
%%  Type conversion is performed if  `Params'  has a different type than the state variable
%% value being requested. If ``gl:getBooleanv'' is called, a floating-point (or integer)
%% value is converted to `?GL_FALSE' if and only if it is 0.0 (or 0). Otherwise, it
%% is converted to `?GL_TRUE'. If ``gl:getIntegerv'' is called, boolean values are
%% returned as `?GL_TRUE' or `?GL_FALSE', and most floating-point values are rounded
%% to the nearest integer value. Floating-point colors and normals, however, are returned
%% with a linear mapping that maps 1.0 to the most positive representable integer value and
%%   -1.0 to the most negative representable integer value. If ``gl:getFloatv'' or ``gl:getDoublev''
%%  is called, boolean values are returned as `?GL_TRUE' or `?GL_FALSE', and integer
%% values are converted to floating-point values. 
%%
%%  The following symbolic constants are accepted by  `Pname' : 
%%
%% `?GL_ACTIVE_TEXTURE':  `Params'  returns a single value indicating the active
%% multitexture unit. The initial value is `?GL_TEXTURE0'. See  {@link gl:activeTexture/1} .
%% 
%%
%% `?GL_ALIASED_LINE_WIDTH_RANGE':  `Params'  returns a pair of values indicating
%% the range of widths supported for aliased lines. See  {@link gl:lineWidth/1} . 
%%
%% `?GL_ARRAY_BUFFER_BINDING':  `Params'  returns a single value, the name of the
%% buffer object currently bound to the target `?GL_ARRAY_BUFFER'. If no buffer object
%% is bound to this target, 0 is returned. The initial value is 0. See  {@link gl:bindBuffer/2} .
%% 
%%
%% `?GL_BLEND':  `Params'  returns a single boolean value indicating whether blending
%% is enabled. The initial value is `?GL_FALSE'. See  {@link gl:blendFunc/2} . 
%%
%% `?GL_BLEND_COLOR':  `Params'  returns four values, the red, green, blue, and alpha
%% values which are the components of the blend color. See  {@link gl:blendColor/4} . 
%%
%% `?GL_BLEND_DST_ALPHA':  `Params'  returns one value, the symbolic constant identifying
%% the alpha destination blend function. The initial value is `?GL_ZERO'. See  {@link gl:blendFunc/2} 
%%  and  {@link gl:blendFuncSeparate/4} . 
%%
%% `?GL_BLEND_DST_RGB':  `Params'  returns one value, the symbolic constant identifying
%% the RGB destination blend function. The initial value is `?GL_ZERO'. See  {@link gl:blendFunc/2} 
%%  and  {@link gl:blendFuncSeparate/4} . 
%%
%% `?GL_BLEND_EQUATION_RGB':  `Params'  returns one value, a symbolic constant indicating
%% whether the RGB blend equation is `?GL_FUNC_ADD', `?GL_FUNC_SUBTRACT',  `?GL_FUNC_REVERSE_SUBTRACT'
%% , `?GL_MIN' or `?GL_MAX'. See  {@link gl:blendEquationSeparate/2} . 
%%
%% `?GL_BLEND_EQUATION_ALPHA':  `Params'  returns one value, a symbolic constant
%% indicating whether the Alpha blend equation is `?GL_FUNC_ADD', `?GL_FUNC_SUBTRACT'
%% ,  `?GL_FUNC_REVERSE_SUBTRACT', `?GL_MIN' or `?GL_MAX'. See  {@link gl:blendEquationSeparate/2} 
%% . 
%%
%% `?GL_BLEND_SRC_ALPHA':  `Params'  returns one value, the symbolic constant identifying
%% the alpha source blend function. The initial value is `?GL_ONE'. See  {@link gl:blendFunc/2} 
%%  and  {@link gl:blendFuncSeparate/4} . 
%%
%% `?GL_BLEND_SRC_RGB':  `Params'  returns one value, the symbolic constant identifying
%% the RGB source blend function. The initial value is `?GL_ONE'. See  {@link gl:blendFunc/2} 
%%  and  {@link gl:blendFuncSeparate/4} . 
%%
%% `?GL_COLOR_CLEAR_VALUE':  `Params'  returns four values: the red, green, blue,
%% and alpha values used to clear the color buffers. Integer values, if requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 returns the most
%% positive representable integer value, and   -1.0 returns the most negative representable
%% integer value. The initial value is (0, 0, 0, 0). See  {@link gl:clearColor/4} . 
%%
%% `?GL_COLOR_LOGIC_OP':  `Params'  returns a single boolean value indicating whether
%% a fragment's RGBA color values are merged into the framebuffer using a logical operation.
%% The initial value is `?GL_FALSE'. See  {@link gl:logicOp/1} . 
%%
%% `?GL_COLOR_WRITEMASK':  `Params'  returns four boolean values: the red, green,
%% blue, and alpha write enables for the color buffers. The initial value is (`?GL_TRUE',
%% `?GL_TRUE', `?GL_TRUE', `?GL_TRUE'). See  {@link gl:colorMask/4} . 
%%
%% `?GL_COMPRESSED_TEXTURE_FORMATS':  `Params'  returns a list of symbolic constants
%% of length `?GL_NUM_COMPRESSED_TEXTURE_FORMATS'  indicating which compressed texture
%% formats are available. See  {@link gl:compressedTexImage2D/8} . 
%%
%% `?GL_CONTEXT_FLAGS':  `Params'  returns one value, the flags with which the context
%% was created (such as debugging functionality). 
%%
%% `?GL_CULL_FACE':  `Params'  returns a single boolean value indicating whether
%% polygon culling is enabled. The initial value is `?GL_FALSE'. See  {@link gl:cullFace/1} 
%% . 
%%
%% `?GL_CURRENT_PROGRAM':  `Params'  returns one value, the name of the program object
%% that is currently active, or 0 if no program object is active. See  {@link gl:useProgram/1} .
%% 
%%
%% `?GL_DEPTH_CLEAR_VALUE':  `Params'  returns one value, the value that is used
%% to clear the depth buffer. Integer values, if requested, are linearly mapped from the
%% internal floating-point representation such that 1.0 returns the most positive representable
%% integer value, and   -1.0 returns the most negative representable integer value. The initial
%% value is 1. See  {@link gl:clearDepth/1} . 
%%
%% `?GL_DEPTH_FUNC':  `Params'  returns one value, the symbolic constant that indicates
%% the depth comparison function. The initial value is `?GL_LESS'. See  {@link gl:depthFunc/1} 
%% . 
%%
%% `?GL_DEPTH_RANGE':  `Params'  returns two values: the near and far mapping limits
%% for the depth buffer. Integer values, if requested, are linearly mapped from the internal
%% floating-point representation such that 1.0 returns the most positive representable integer
%% value, and   -1.0 returns the most negative representable integer value. The initial value
%% is (0, 1). See  {@link gl:depthRange/2} . 
%%
%% `?GL_DEPTH_TEST':  `Params'  returns a single boolean value indicating whether
%% depth testing of fragments is enabled. The initial value is `?GL_FALSE'. See  {@link gl:depthFunc/1} 
%%  and  {@link gl:depthRange/2} . 
%%
%% `?GL_DEPTH_WRITEMASK':  `Params'  returns a single boolean value indicating if
%% the depth buffer is enabled for writing. The initial value is `?GL_TRUE'. See  {@link gl:depthMask/1} 
%% . 
%%
%% `?GL_DITHER':  `Params'  returns a single boolean value indicating whether dithering
%% of fragment colors and indices is enabled. The initial value is `?GL_TRUE'. 
%%
%% `?GL_DOUBLEBUFFER':  `Params'  returns a single boolean value indicating whether
%% double buffering is supported. 
%%
%% `?GL_DRAW_BUFFER':  `Params'  returns one value, a symbolic constant indicating
%% which buffers are being drawn to. See  {@link gl:drawBuffer/1} . The initial value is `?GL_BACK'
%%  if there are back buffers, otherwise it is `?GL_FRONT'. 
%%
%% `?GL_DRAW_BUFFER'`i':  `Params'  returns one value, a symbolic constant indicating
%% which buffers are being drawn to by the corresponding output color. See  {@link gl:drawBuffers/1} 
%% .  The initial value of `?GL_DRAW_BUFFER0' is `?GL_BACK' if there are back buffers,
%% otherwise it is `?GL_FRONT'. The initial values of draw buffers for all other output
%% colors is `?GL_NONE'. 
%%
%% `?GL_DRAW_FRAMEBUFFER_BINDING':  `Params'  returns one value, the name of the
%% framebuffer object currently bound to the `?GL_DRAW_FRAMEBUFFER' target. If the default
%% framebuffer is bound, this value will be zero. The initial value is zero. See  {@link gl:bindFramebuffer/2} 
%% . 
%%
%% `?GL_READ_FRAMEBUFFER_BINDING':  `Params'  returns one value, the name of the
%% framebuffer object currently bound to the `?GL_READ_FRAMEBUFFER' target. If the default
%% framebuffer is bound, this value will be zero. The initial value is zero. See  {@link gl:bindFramebuffer/2} 
%% . 
%%
%% `?GL_ELEMENT_ARRAY_BUFFER_BINDING':  `Params'  returns a single value, the name
%% of the buffer object currently bound to the target `?GL_ELEMENT_ARRAY_BUFFER'. If
%% no buffer object is bound to this target, 0 is returned. The initial value is 0. See  {@link gl:bindBuffer/2} 
%% . 
%%
%% `?GL_FRAGMENT_SHADER_DERIVATIVE_HINT':  `Params'  returns one value, a symbolic
%% constant indicating the mode of the derivative accuracy hint  for fragment shaders. The
%% initial value is `?GL_DONT_CARE'. See  {@link gl:hint/2} . 
%%
%% `?GL_IMPLEMENTATION_COLOR_READ_FORMAT':  `Params'  returns a single GLenum value
%% indicating the implementation's preferred pixel data format. See  {@link gl:readPixels/7} . 
%%
%% `?GL_IMPLEMENTATION_COLOR_READ_TYPE':  `Params'  returns a single GLenum value
%% indicating the implementation's preferred pixel data type. See  {@link gl:readPixels/7} . 
%%
%% `?GL_LINE_SMOOTH':  `Params'  returns a single boolean value indicating whether
%% antialiasing of lines is enabled. The initial value is `?GL_FALSE'. See  {@link gl:lineWidth/1} 
%% . 
%%
%% `?GL_LINE_SMOOTH_HINT':  `Params'  returns one value, a symbolic constant indicating
%% the mode of the line antialiasing hint. The initial value is `?GL_DONT_CARE'. See  {@link gl:hint/2} 
%% . 
%%
%% `?GL_LINE_WIDTH':  `Params'  returns one value, the line width as specified with  {@link gl:lineWidth/1} 
%% . The initial value is 1. 
%%
%% `?GL_LAYER_PROVOKING_VERTEX':  `Params'  returns one value, the implementation
%% dependent specifc vertex of a primitive that is used to select the rendering layer.  If
%% the value returned is equivalent to `?GL_PROVOKING_VERTEX', then the vertex  selection
%% follows the convention specified by  {@link gl:provokingVertex/1} . If the value returned
%% is equivalent to `?GL_FIRST_VERTEX_CONVENTION', then the  selection is always taken
%% from the first vertex in the primitive. If the value returned is equivalent to `?GL_LAST_VERTEX_CONVENTION'
%% , then the  selection is always taken from the last vertex in the primitive. If the value
%% returned is equivalent to `?GL_UNDEFINED_VERTEX', then the  selection is not guaranteed
%% to be taken from any specific vertex in the primitive. 
%%
%% `?GL_LINE_WIDTH_GRANULARITY':  `Params'  returns one value, the width difference
%% between adjacent supported widths for antialiased lines. See  {@link gl:lineWidth/1} . 
%%
%% `?GL_LINE_WIDTH_RANGE':  `Params'  returns two values: the smallest and largest
%% supported widths for antialiased lines. See  {@link gl:lineWidth/1} . 
%%
%% `?GL_LOGIC_OP_MODE':  `Params'  returns one value, a symbolic constant indicating
%% the selected logic operation mode. The initial value is `?GL_COPY'. See  {@link gl:logicOp/1} 
%% . 
%%
%% `?GL_MAJOR_VERSION':  `Params'  returns one value, the major version number of
%% the OpenGL API supported by the current context. 
%%
%% `?GL_MAX_3D_TEXTURE_SIZE':  `Params'  returns one value, a rough estimate of the
%% largest 3D texture that the GL can handle. The value must be at least 64. Use `?GL_PROXY_TEXTURE_3D'
%%  to determine if a texture is too large. See  {@link gl:texImage3D/10} . 
%%
%% `?GL_MAX_ARRAY_TEXTURE_LAYERS':  `Params'  returns one value. The value indicates
%% the maximum number of layers allowed in an array texture, and must be at least 256. See  {@link gl:texImage2D/9} 
%% . 
%%
%% `?GL_MAX_CLIP_DISTANCES':  `Params'  returns one value, the maximum number of
%% application-defined clipping distances. The value must be at least 8. 
%%
%% `?GL_MAX_COLOR_TEXTURE_SAMPLES':  `Params'  returns one value, the maximum number
%% of samples in a color multisample texture. 
%%
%% `?GL_MAX_COMBINED_ATOMIC_COUNTERS':  `Params'  returns a single value, the maximum
%% number of atomic counters available to all active shaders. 
%%
%% `?GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS':  `Params'  returns one value,
%% the number of words for fragment shader uniform variables in all uniform blocks (including
%% default). The value must be at least 1. See  {@link gl:uniform1f/2} . 
%%
%% `?GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS':  `Params'  returns one value,
%% the number of words for geometry shader uniform variables in all uniform blocks (including
%% default). The value must be at least 1. See  {@link gl:uniform1f/2} . 
%%
%% `?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS':  `Params'  returns one value, the maximum
%% supported texture image units that  can be used to access texture maps from the vertex
%% shader and the fragment processor combined.  If both the vertex shader and the fragment
%% processing stage access the same texture image unit, then that counts as using two texture
%% image units against this limit. The value must be at least 48. See  {@link gl:activeTexture/1} 
%% . 
%%
%% `?GL_MAX_COMBINED_UNIFORM_BLOCKS':  `Params'  returns one value, the maximum number
%% of uniform blocks per program. The value must be at least 36. See  {@link gl:uniformBlockBinding/3} 
%% . 
%%
%% `?GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS':  `Params'  returns one value, the
%% number of words for vertex shader uniform variables in all uniform blocks (including default).
%% The value must be at least 1. See  {@link gl:uniform1f/2} . 
%%
%% `?GL_MAX_CUBE_MAP_TEXTURE_SIZE':  `Params'  returns one value. The value gives
%% a rough estimate of the largest cube-map texture that the GL can handle. The value must
%% be at least 1024. Use `?GL_PROXY_TEXTURE_CUBE_MAP' to determine if a texture is too
%% large. See  {@link gl:texImage2D/9} . 
%%
%% `?GL_MAX_DEPTH_TEXTURE_SAMPLES':  `Params'  returns one value, the maximum number
%% of samples in a multisample depth or depth-stencil texture. 
%%
%% `?GL_MAX_DRAW_BUFFERS':  `Params'  returns one value, the maximum number of simultaneous
%% outputs that may be written in a fragment shader. The value must be at least 8. See  {@link gl:drawBuffers/1} 
%% . 
%%
%% `?GL_MAX_DUALSOURCE_DRAW_BUFFERS':  `Params'  returns one value, the maximum number
%% of active draw buffers when using dual-source blending. The value must be at least 1.
%% See  {@link gl:blendFunc/2}  and   {@link gl:blendFuncSeparate/4} . 
%%
%% `?GL_MAX_ELEMENTS_INDICES':  `Params'  returns one value, the recommended maximum
%% number of vertex array indices. See  {@link gl:drawRangeElements/6} . 
%%
%% `?GL_MAX_ELEMENTS_VERTICES':  `Params'  returns one value, the recommended maximum
%% number of vertex array vertices. See  {@link gl:drawRangeElements/6} . 
%%
%% `?GL_MAX_FRAGMENT_ATOMIC_COUNTERS':  `Params'  returns a single value, the maximum
%% number of atomic counters available to fragment shaders. 
%%
%% `?GL_MAX_FRAGMENT_INPUT_COMPONENTS':  `Params'  returns one value, the maximum
%% number of components of the inputs read by the fragment shader, which must be at least
%% 128. 
%%
%% `?GL_MAX_FRAGMENT_UNIFORM_COMPONENTS':  `Params'  returns one value, the maximum
%% number of individual floating-point, integer, or boolean values that can be held  in uniform
%% variable storage for a fragment shader. The value must be at least 1024. See  {@link gl:uniform1f/2} 
%% . 
%%
%% `?GL_MAX_FRAGMENT_UNIFORM_VECTORS':  `Params'  returns one value, the maximum
%% number of individual 4-vectors of floating-point, integer, or boolean values that can
%% be held in uniform variable storage for a fragment shader. The value is equal to the value
%% of `?GL_MAX_FRAGMENT_UNIFORM_COMPONENTS' divided by 4 and must be at least 256. See  {@link gl:uniform1f/2} 
%% . 
%%
%% `?GL_MAX_FRAGMENT_UNIFORM_BLOCKS':  `Params'  returns one value, the maximum number
%% of uniform blocks per fragment shader. The value must be at least 12. See  {@link gl:uniformBlockBinding/3} 
%% . 
%%
%% `?GL_MAX_GEOMETRY_ATOMIC_COUNTERS':  `Params'  returns a single value, the maximum
%% number of atomic counters available to geometry shaders. 
%%
%% `?GL_MAX_GEOMETRY_INPUT_COMPONENTS':  `Params'  returns one value, the maximum
%% number of components of inputs read by a geometry shader, which must be at least 64. 
%%
%% `?GL_MAX_GEOMETRY_OUTPUT_COMPONENTS':  `Params'  returns one value, the maximum
%% number of components of outputs written by a geometry shader, which must be at least 128.
%% 
%%
%% `?GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS':  `Params'  returns one value, the maximum
%% supported texture image units that  can be used to access texture maps from the geometry
%% shader. The value must be at least 16. See  {@link gl:activeTexture/1} . 
%%
%% `?GL_MAX_GEOMETRY_UNIFORM_BLOCKS':  `Params'  returns one value, the maximum number
%% of uniform blocks per geometry shader. The value must be at least 12. See  {@link gl:uniformBlockBinding/3} 
%% . 
%%
%% `?GL_MAX_GEOMETRY_UNIFORM_COMPONENTS':  `Params'  returns one value, the maximum
%% number of individual floating-point, integer, or boolean values that can be held  in uniform
%% variable storage for a geometry shader. The value must be at least 1024. See  {@link gl:uniform1f/2} 
%% . 
%%
%% `?GL_MAX_INTEGER_SAMPLES':  `Params'  returns one value, the maximum number of
%% samples supported in integer format multisample buffers. 
%%
%% `?GL_MIN_MAP_BUFFER_ALIGNMENT':  `Params'  returns one value, the minimum alignment
%% in basic machine units of pointers returned fromsee `glMapBuffer' and see `glMapBufferRange'
%% . This value must be a power of two and must be at least 64. 
%%
%% `?GL_MAX_PROGRAM_TEXEL_OFFSET':  `Params'  returns one value, the maximum texel
%% offset allowed in a texture lookup, which must be at least 7. 
%%
%% `?GL_MIN_PROGRAM_TEXEL_OFFSET':  `Params'  returns one value, the minimum texel
%% offset allowed in a texture lookup, which must be at most -8. 
%%
%% `?GL_MAX_RECTANGLE_TEXTURE_SIZE':  `Params'  returns one value. The value gives
%% a rough estimate of the largest rectangular texture that the GL can handle. The value
%% must be at least 1024. Use `?GL_PROXY_RECTANGLE_TEXTURE' to determine if a texture
%% is too large. See  {@link gl:texImage2D/9} . 
%%
%% `?GL_MAX_RENDERBUFFER_SIZE':  `Params'  returns one value. The value indicates
%% the maximum supported size for renderbuffers. See  {@link gl:framebufferRenderbuffer/4} . 
%%
%% `?GL_MAX_SAMPLE_MASK_WORDS':  `Params'  returns one value, the maximum number
%% of sample mask words. 
%%
%% `?GL_MAX_SERVER_WAIT_TIMEOUT':  `Params'  returns one value, the maximum  {@link gl:waitSync/3} 
%%  timeout interval. 
%%
%% `?GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS':  `Params'  returns a single value, the
%% maximum number of atomic counters available to tessellation control shaders. 
%%
%% `?GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS':  `Params'  returns a single value,
%% the maximum number of atomic counters available to tessellation evaluation shaders. 
%%
%% `?GL_MAX_TEXTURE_BUFFER_SIZE':  `Params'  returns one value. The value gives the
%% maximum number of texels allowed in the texel array of a texture buffer object. Value
%% must be at least 65536. 
%%
%% `?GL_MAX_TEXTURE_IMAGE_UNITS':  `Params'  returns one value, the maximum supported
%% texture image units that  can be used to access texture maps from the fragment shader.
%%  The value must be at least 16. See  {@link gl:activeTexture/1} . 
%%
%% `?GL_MAX_TEXTURE_LOD_BIAS':  `Params'  returns one value, the maximum, absolute
%% value of the texture level-of-detail bias. The value must be at least 2.0. 
%%
%% `?GL_MAX_TEXTURE_SIZE':  `Params'  returns one value. The value gives a rough
%% estimate of the largest texture that the GL can handle. The value must be at least 1024.
%% Use a proxy texture target such as `?GL_PROXY_TEXTURE_1D' or `?GL_PROXY_TEXTURE_2D'
%%  to determine if a texture is too large. See  {@link gl:texImage1D/8}  and  {@link gl:texImage2D/9} 
%% . 
%%
%% `?GL_MAX_UNIFORM_BUFFER_BINDINGS':  `Params'  returns one value, the maximum number
%% of uniform buffer binding points on the context, which must be at least 36. 
%%
%% `?GL_MAX_UNIFORM_BLOCK_SIZE':  `Params'  returns one value, the maximum size in
%% basic machine units of a uniform block, which must be at least 16384. 
%%
%% `?GL_MAX_VARYING_COMPONENTS':  `Params'  returns one value, the number components
%% for varying variables, which must be at least 60. 
%%
%% `?GL_MAX_VARYING_VECTORS':  `Params'  returns one value, the number 4-vectors
%% for varying variables, which is equal to the value of `?GL_MAX_VARYING_COMPONENTS'
%% and must be at least 15. 
%%
%% `?GL_MAX_VARYING_FLOATS':  `Params'  returns one value, the maximum number of
%% interpolators available for processing varying variables used by vertex and fragment shaders.
%% This value represents the number of individual floating-point  values that can be interpolated;
%% varying variables declared as vectors, matrices, and arrays  will all consume multiple
%% interpolators. The value must be at least 32. 
%%
%% `?GL_MAX_VERTEX_ATOMIC_COUNTERS':  `Params'  returns a single value, the maximum
%% number of atomic counters available to vertex shaders. 
%%
%% `?GL_MAX_VERTEX_ATTRIBS':  `Params'  returns one value, the maximum number of
%% 4-component generic vertex attributes accessible to a vertex shader.  The value must be
%% at least 16. See  {@link gl:vertexAttrib1d/2} . 
%%
%% `?GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS':  `Params'  returns one value, the maximum
%% supported texture image units that  can be used to access texture maps from the vertex
%% shader. The value may be at least 16. See  {@link gl:activeTexture/1} . 
%%
%% `?GL_MAX_VERTEX_UNIFORM_COMPONENTS':  `Params'  returns one value, the maximum
%% number of individual floating-point, integer, or boolean values that can be held  in uniform
%% variable storage for a vertex shader. The value must be at least 1024. See  {@link gl:uniform1f/2} 
%% . 
%%
%% `?GL_MAX_VERTEX_UNIFORM_VECTORS':  `Params'  returns one value, the maximum number
%% of 4-vectors that may be held in uniform variable storage for the vertex shader. The value
%% of `?GL_MAX_VERTEX_UNIFORM_VECTORS' is equal to the value of `?GL_MAX_VERTEX_UNIFORM_COMPONENTS'
%%  and must be at least 256. 
%%
%% `?GL_MAX_VERTEX_OUTPUT_COMPONENTS':  `Params'  returns one value, the maximum
%% number of components of output written by a vertex shader, which must be at least 64. 
%%
%% `?GL_MAX_VERTEX_UNIFORM_BLOCKS':  `Params'  returns one value, the maximum number
%% of uniform blocks per vertex shader. The value must be at least 12. See  {@link gl:uniformBlockBinding/3} 
%% . 
%%
%% `?GL_MAX_VIEWPORT_DIMS':  `Params'  returns two values: the maximum supported
%% width and height of the viewport. These must be at least as large as the visible dimensions
%% of the display being rendered to. See  {@link gl:viewport/4} . 
%%
%% `?GL_MAX_VIEWPORTS':  `Params'  returns one value, the maximum number of simultaneous
%% viewports that are supported. The value must be at least 16. See  {@link gl:viewportIndexedf/5} 
%% . 
%%
%% `?GL_MINOR_VERSION':  `Params'  returns one value, the minor version number of
%% the OpenGL API supported by the current context. 
%%
%% `?GL_NUM_COMPRESSED_TEXTURE_FORMATS':  `Params'  returns a single integer value
%% indicating the number of available compressed texture formats. The minimum value is 4.
%% See  {@link gl:compressedTexImage2D/8} . 
%%
%% `?GL_NUM_EXTENSIONS':  `Params'  returns one value, the number of extensions supported
%% by the GL implementation for the current context. See  {@link gl:getString/1} . 
%%
%% `?GL_NUM_PROGRAM_BINARY_FORMATS':  `Params'  returns one value, the number of
%% program binary formats supported by the implementation. 
%%
%% `?GL_NUM_SHADER_BINARY_FORMATS':  `Params'  returns one value, the number of binary
%% shader formats supported by the implementation. If this value is greater than zero, then
%% the implementation supports loading binary shaders. If it is zero, then the loading of
%% binary shaders by the implementation is not supported. 
%%
%% `?GL_PACK_ALIGNMENT':  `Params'  returns one value, the byte alignment used for
%% writing pixel data to memory. The initial value is 4. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_PACK_IMAGE_HEIGHT':  `Params'  returns one value, the image height used for
%% writing pixel data to memory. The initial value is 0. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_PACK_LSB_FIRST':  `Params'  returns a single boolean value indicating whether
%% single-bit pixels being written to memory are written first to the least significant bit
%% of each unsigned byte. The initial value is `?GL_FALSE'. See  {@link gl:pixelStoref/2} .
%% 
%%
%% `?GL_PACK_ROW_LENGTH':  `Params'  returns one value, the row length used for writing
%% pixel data to memory. The initial value is 0. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_PACK_SKIP_IMAGES':  `Params'  returns one value, the number of pixel images
%% skipped before the first pixel is written into memory. The initial value is 0. See  {@link gl:pixelStoref/2} 
%% . 
%%
%% `?GL_PACK_SKIP_PIXELS':  `Params'  returns one value, the number of pixel locations
%% skipped before the first pixel is written into memory. The initial value is 0. See  {@link gl:pixelStoref/2} 
%% . 
%%
%% `?GL_PACK_SKIP_ROWS':  `Params'  returns one value, the number of rows of pixel
%% locations skipped before the first pixel is written into memory. The initial value is
%% 0. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_PACK_SWAP_BYTES':  `Params'  returns a single boolean value indicating whether
%% the bytes of two-byte and four-byte pixel indices and components are swapped before being
%% written to memory. The initial value is `?GL_FALSE'. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_PIXEL_PACK_BUFFER_BINDING':  `Params'  returns a single value, the name of
%% the buffer object currently bound to the target `?GL_PIXEL_PACK_BUFFER'. If no buffer
%% object is bound to this target, 0 is returned. The initial value is 0. See  {@link gl:bindBuffer/2} 
%% . 
%%
%% `?GL_PIXEL_UNPACK_BUFFER_BINDING':  `Params'  returns a single value, the name
%% of the buffer object currently bound to the target `?GL_PIXEL_UNPACK_BUFFER'. If
%% no buffer object is bound to this target, 0 is returned. The initial value is 0. See  {@link gl:bindBuffer/2} 
%% . 
%%
%% `?GL_POINT_FADE_THRESHOLD_SIZE':  `Params'  returns one value, the point size
%% threshold for determining the point size. See  {@link gl:pointParameterf/2} . 
%%
%% `?GL_PRIMITIVE_RESTART_INDEX':  `Params'  returns one value, the current primitive
%% restart index. The initial value is 0. See  {@link gl:primitiveRestartIndex/1} . 
%%
%% `?GL_PROGRAM_BINARY_FORMATS':  `Params'  an array of `?GL_NUM_PROGRAM_BINARY_FORMATS'
%%  values, indicating the proram binary formats supported by the implementation. 
%%
%% `?GL_PROGRAM_PIPELINE_BINDING':  `Params'  a single value, the name of the currently
%% bound program pipeline object, or zero if no program pipeline object is bound. See  {@link gl:bindProgramPipeline/1} 
%% . 
%%
%% `?GL_PROVOKING_VERTEX':  `Params'  returns one value, the currently selected provoking
%% vertex convention. The initial value is `?GL_LAST_VERTEX_CONVENTION'. See  {@link gl:provokingVertex/1} 
%% . 
%%
%% `?GL_POINT_SIZE':  `Params'  returns one value, the point size as specified by  {@link gl:pointSize/1} 
%% . The initial value is 1. 
%%
%% `?GL_POINT_SIZE_GRANULARITY':  `Params'  returns one value, the size difference
%% between adjacent supported sizes for antialiased points. See  {@link gl:pointSize/1} . 
%%
%% `?GL_POINT_SIZE_RANGE':  `Params'  returns two values: the smallest and largest
%% supported sizes for antialiased points. The smallest size must be at most 1, and the largest
%% size must be at least 1. See  {@link gl:pointSize/1} . 
%%
%% `?GL_POLYGON_OFFSET_FACTOR':  `Params'  returns one value, the scaling factor
%% used to determine the variable offset that is added to the depth value of each fragment
%% generated when a polygon is rasterized. The initial value is 0. See  {@link gl:polygonOffset/2} 
%% . 
%%
%% `?GL_POLYGON_OFFSET_UNITS':  `Params'  returns one value. This value is multiplied
%% by an implementation-specific value and then added to the depth value of each fragment
%% generated when a polygon is rasterized. The initial value is 0. See  {@link gl:polygonOffset/2} 
%% . 
%%
%% `?GL_POLYGON_OFFSET_FILL':  `Params'  returns a single boolean value indicating
%% whether polygon offset is enabled for polygons in fill mode. The initial value is `?GL_FALSE'
%% . See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_OFFSET_LINE':  `Params'  returns a single boolean value indicating
%% whether polygon offset is enabled for polygons in line mode. The initial value is `?GL_FALSE'
%% . See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_OFFSET_POINT':  `Params'  returns a single boolean value indicating
%% whether polygon offset is enabled for polygons in point mode. The initial value is `?GL_FALSE'
%% . See  {@link gl:polygonOffset/2} . 
%%
%% `?GL_POLYGON_SMOOTH':  `Params'  returns a single boolean value indicating whether
%% antialiasing of polygons is enabled. The initial value is `?GL_FALSE'. See  {@link gl:polygonMode/2} 
%% . 
%%
%% `?GL_POLYGON_SMOOTH_HINT':  `Params'  returns one value, a symbolic constant indicating
%% the mode of the polygon antialiasing hint. The initial value is `?GL_DONT_CARE'.
%% See  {@link gl:hint/2} . 
%%
%% `?GL_READ_BUFFER':  `Params'  returns one value, a symbolic constant indicating
%% which color buffer is selected for reading. The initial value is `?GL_BACK' if there
%% is a back buffer, otherwise it is `?GL_FRONT'. See  {@link gl:readPixels/7} . 
%%
%% `?GL_RENDERBUFFER_BINDING':  `Params'  returns a single value, the name of the
%% renderbuffer object currently bound to the target `?GL_RENDERBUFFER'. If no renderbuffer
%% object is bound to this target, 0 is returned. The initial value is 0. See  {@link gl:bindRenderbuffer/2} 
%% . 
%%
%% `?GL_SAMPLE_BUFFERS':  `Params'  returns a single integer value indicating the
%% number of sample buffers associated with the framebuffer. See  {@link gl:sampleCoverage/2} .
%% 
%%
%% `?GL_SAMPLE_COVERAGE_VALUE':  `Params'  returns a single positive floating-point
%% value indicating the current sample coverage value. See  {@link gl:sampleCoverage/2} . 
%%
%% `?GL_SAMPLE_COVERAGE_INVERT':  `Params'  returns a single boolean value indicating
%% if the temporary coverage value should be inverted. See  {@link gl:sampleCoverage/2} . 
%%
%% `?GL_SAMPLER_BINDING':  `Params'  returns a single value, the name of the sampler
%% object currently bound to the active texture unit. The initial value is 0. See  {@link gl:bindSampler/2} 
%% . 
%%
%% `?GL_SAMPLES':  `Params'  returns a single integer value indicating the coverage
%% mask size. See  {@link gl:sampleCoverage/2} . 
%%
%% `?GL_SCISSOR_BOX':  `Params'  returns four values: the   x and   y window coordinates
%% of the scissor box, followed by its width and height. Initially the   x and   y window
%% coordinates are both 0 and the width and height are set to the size of the window. See  {@link gl:scissor/4} 
%% . 
%%
%% `?GL_SCISSOR_TEST':  `Params'  returns a single boolean value indicating whether
%% scissoring is enabled. The initial value is `?GL_FALSE'. See  {@link gl:scissor/4} . 
%%
%% `?GL_SHADER_COMPILER':  `Params'  returns a single boolean value indicating whether
%% an online shader compiler is present in the implementation. All desktop OpenGL implementations
%% must support online shader compilations, and therefore the value of `?GL_SHADER_COMPILER'
%%  will always be `?GL_TRUE'. 
%%
%% `?GL_SMOOTH_LINE_WIDTH_RANGE':  `Params'  returns a pair of values indicating
%% the range of widths supported for smooth (antialiased) lines. See  {@link gl:lineWidth/1} . 
%%
%% `?GL_SMOOTH_LINE_WIDTH_GRANULARITY':  `Params'  returns a single value indicating
%% the level of quantization applied to smooth line width parameters. 
%%
%% `?GL_STENCIL_BACK_FAIL':  `Params'  returns one value, a symbolic constant indicating
%% what action is taken for back-facing polygons when the stencil test fails. The initial
%% value is `?GL_KEEP'. See  {@link gl:stencilOpSeparate/4} . 
%%
%% `?GL_STENCIL_BACK_FUNC':  `Params'  returns one value, a symbolic constant indicating
%% what function is used for back-facing polygons to compare the stencil reference value
%% with the stencil buffer value. The initial value is `?GL_ALWAYS'. See  {@link gl:stencilFuncSeparate/4} 
%% . 
%%
%% `?GL_STENCIL_BACK_PASS_DEPTH_FAIL':  `Params'  returns one value, a symbolic constant
%% indicating what action is taken for back-facing polygons when the stencil test passes,
%% but the depth test fails. The initial value is `?GL_KEEP'. See  {@link gl:stencilOpSeparate/4} 
%% . 
%%
%% `?GL_STENCIL_BACK_PASS_DEPTH_PASS':  `Params'  returns one value, a symbolic constant
%% indicating what action is taken for back-facing polygons when the stencil test passes
%% and the depth test passes. The initial value is `?GL_KEEP'. See  {@link gl:stencilOpSeparate/4} 
%% . 
%%
%% `?GL_STENCIL_BACK_REF':  `Params'  returns one value, the reference value that
%% is compared with the contents of the stencil buffer for back-facing polygons. The initial
%% value is 0. See  {@link gl:stencilFuncSeparate/4} . 
%%
%% `?GL_STENCIL_BACK_VALUE_MASK':  `Params'  returns one value, the mask that is
%% used for back-facing polygons to mask both the stencil reference value and the stencil
%% buffer value before they are compared. The initial value is all 1's. See  {@link gl:stencilFuncSeparate/4} 
%% . 
%%
%% `?GL_STENCIL_BACK_WRITEMASK':  `Params'  returns one value, the mask that controls
%% writing of the stencil bitplanes for back-facing polygons. The initial value is all 1's.
%% See  {@link gl:stencilMaskSeparate/2} . 
%%
%% `?GL_STENCIL_CLEAR_VALUE':  `Params'  returns one value, the index to which the
%% stencil bitplanes are cleared. The initial value is 0. See  {@link gl:clearStencil/1} . 
%%
%% `?GL_STENCIL_FAIL':  `Params'  returns one value, a symbolic constant indicating
%% what action is taken when the stencil test fails. The initial value is `?GL_KEEP'.
%% See  {@link gl:stencilOp/3} . This stencil state only affects non-polygons and front-facing
%% polygons. Back-facing polygons use separate stencil state. See  {@link gl:stencilOpSeparate/4} 
%% . 
%%
%% `?GL_STENCIL_FUNC':  `Params'  returns one value, a symbolic constant indicating
%% what function is used to compare the stencil reference value with the stencil buffer value.
%% The initial value is `?GL_ALWAYS'. See  {@link gl:stencilFunc/3} . This stencil state
%% only affects non-polygons and front-facing polygons. Back-facing polygons use separate
%% stencil state. See  {@link gl:stencilFuncSeparate/4} . 
%%
%% `?GL_STENCIL_PASS_DEPTH_FAIL':  `Params'  returns one value, a symbolic constant
%% indicating what action is taken when the stencil test passes, but the depth test fails.
%% The initial value is `?GL_KEEP'. See  {@link gl:stencilOp/3} . This stencil state only
%% affects non-polygons and front-facing polygons. Back-facing polygons use separate stencil
%% state. See  {@link gl:stencilOpSeparate/4} . 
%%
%% `?GL_STENCIL_PASS_DEPTH_PASS':  `Params'  returns one value, a symbolic constant
%% indicating what action is taken when the stencil test passes and the depth test passes.
%% The initial value is `?GL_KEEP'. See  {@link gl:stencilOp/3} . This stencil state only
%% affects non-polygons and front-facing polygons. Back-facing polygons use separate stencil
%% state. See  {@link gl:stencilOpSeparate/4} . 
%%
%% `?GL_STENCIL_REF':  `Params'  returns one value, the reference value that is compared
%% with the contents of the stencil buffer. The initial value is 0. See  {@link gl:stencilFunc/3} 
%% . This stencil state only affects non-polygons and front-facing polygons. Back-facing
%% polygons use separate stencil state. See  {@link gl:stencilFuncSeparate/4} . 
%%
%% `?GL_STENCIL_TEST':  `Params'  returns a single boolean value indicating whether
%% stencil testing of fragments is enabled. The initial value is `?GL_FALSE'. See  {@link gl:stencilFunc/3} 
%%  and  {@link gl:stencilOp/3} . 
%%
%% `?GL_STENCIL_VALUE_MASK':  `Params'  returns one value, the mask that is used
%% to mask both the stencil reference value and the stencil buffer value before they are
%% compared. The initial value is all 1's. See  {@link gl:stencilFunc/3} . This stencil state
%% only affects non-polygons and front-facing polygons. Back-facing polygons use separate
%% stencil state. See  {@link gl:stencilFuncSeparate/4} . 
%%
%% `?GL_STENCIL_WRITEMASK':  `Params'  returns one value, the mask that controls
%% writing of the stencil bitplanes. The initial value is all 1's. See  {@link gl:stencilMask/1} 
%% . This stencil state only affects non-polygons and front-facing polygons. Back-facing
%% polygons use separate stencil state. See  {@link gl:stencilMaskSeparate/2} . 
%%
%% `?GL_STEREO':  `Params'  returns a single boolean value indicating whether stereo
%% buffers (left and right) are supported. 
%%
%% `?GL_SUBPIXEL_BITS':  `Params'  returns one value, an estimate of the number of
%% bits of subpixel resolution that are used to position rasterized geometry in window coordinates.
%% The value must be at least 4. 
%%
%% `?GL_TEXTURE_BINDING_1D':  `Params'  returns a single value, the name of the texture
%% currently bound to the target `?GL_TEXTURE_1D'. The initial value is 0. See  {@link gl:bindTexture/2} 
%% . 
%%
%% `?GL_TEXTURE_BINDING_1D_ARRAY':  `Params'  returns a single value, the name of
%% the texture currently bound to the target `?GL_TEXTURE_1D_ARRAY'. The initial value
%% is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_2D':  `Params'  returns a single value, the name of the texture
%% currently bound to the target `?GL_TEXTURE_2D'. The initial value is 0. See  {@link gl:bindTexture/2} 
%% . 
%%
%% `?GL_TEXTURE_BINDING_2D_ARRAY':  `Params'  returns a single value, the name of
%% the texture currently bound to the target `?GL_TEXTURE_2D_ARRAY'. The initial value
%% is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_2D_MULTISAMPLE':  `Params'  returns a single value, the name
%% of the texture currently bound to the target `?GL_TEXTURE_2D_MULTISAMPLE'. The initial
%% value is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY':  `Params'  returns a single value,
%% the name of the texture currently bound to the target `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY'
%% . The initial value is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_3D':  `Params'  returns a single value, the name of the texture
%% currently bound to the target `?GL_TEXTURE_3D'. The initial value is 0. See  {@link gl:bindTexture/2} 
%% . 
%%
%% `?GL_TEXTURE_BINDING_BUFFER':  `Params'  returns a single value, the name of the
%% texture currently bound to the target `?GL_TEXTURE_BUFFER'. The initial value is
%% 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_CUBE_MAP':  `Params'  returns a single value, the name of
%% the texture currently bound to the target `?GL_TEXTURE_CUBE_MAP'. The initial value
%% is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_BINDING_RECTANGLE':  `Params'  returns a single value, the name of
%% the texture currently bound to the target `?GL_TEXTURE_RECTANGLE'. The initial value
%% is 0. See  {@link gl:bindTexture/2} . 
%%
%% `?GL_TEXTURE_COMPRESSION_HINT':  `Params'  returns a single value indicating the
%% mode of the texture compression hint. The initial value is `?GL_DONT_CARE'. 
%%
%% `?GL_TEXTURE_BUFFER_BINDING':  `Params'  returns a single value, the name of the
%% texture buffer object currently bound. The initial value is 0. See  {@link gl:bindBuffer/2} .
%% 
%%
%% `?GL_TIMESTAMP':  `Params'  returns a single value, the 64-bit value of the current
%% GL time. See  {@link gl:queryCounter/2} . 
%%
%% `?GL_TRANSFORM_FEEDBACK_BUFFER_BINDING':  When used with non-indexed variants of ``gl:get''
%%  (such as ``gl:getIntegerv''),  `Params'  returns a single value, the name of the
%% buffer object currently bound to the target `?GL_TRANSFORM_FEEDBACK_BUFFER'. If no
%% buffer object is bound to this target, 0 is returned. When used with indexed variants of ``gl:get''
%%  (such as ``gl:getIntegeri_v''),  `Params'  returns a single value, the name of the
%% buffer object bound to the indexed transform feedback attribute stream. The initial value
%% is 0 for all targets. See  {@link gl:bindBuffer/2} ,  {@link gl:bindBufferBase/3} , and  {@link gl:bindBufferRange/5} 
%% . 
%%
%% `?GL_TRANSFORM_FEEDBACK_BUFFER_START':  When used with indexed variants of ``gl:get''
%%  (such as ``gl:getInteger64i_v''),  `Params'  returns a single value, the start offset
%% of the binding range for each transform feedback attribute stream. The initial value is
%% 0 for all streams. See  {@link gl:bindBufferRange/5} . 
%%
%% `?GL_TRANSFORM_FEEDBACK_BUFFER_SIZE':  When used with indexed variants of ``gl:get''
%%  (such as ``gl:getInteger64i_v''),  `Params'  returns a single value, the size of
%% the binding range for each transform feedback attribute stream. The initial value is 0
%% for all streams. See  {@link gl:bindBufferRange/5} . 
%%
%% `?GL_UNIFORM_BUFFER_BINDING':  When used with non-indexed variants of ``gl:get''
%% (such as ``gl:getIntegerv''),  `Params'  returns a single value, the name of the
%% buffer object currently bound to the target `?GL_UNIFORM_BUFFER'. If no buffer object
%% is bound to this target, 0 is returned. When used with indexed variants of ``gl:get''
%% (such as ``gl:getIntegeri_v''),  `Params'  returns a single value, the name of the
%% buffer object bound to the indexed uniform buffer binding point. The initial value is
%% 0 for all targets. See  {@link gl:bindBuffer/2} ,  {@link gl:bindBufferBase/3} , and  {@link gl:bindBufferRange/5} 
%% . 
%%
%% `?GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT':  `Params'  returns a single value, the
%% minimum required alignment for uniform buffer sizes and offset. The initial value is 1.
%% See  {@link gl:uniformBlockBinding/3} . 
%%
%% `?GL_UNIFORM_BUFFER_SIZE':  When used with indexed variants of ``gl:get'' (such
%% as ``gl:getInteger64i_v''),  `Params'  returns a single value, the size of the binding
%% range for each indexed uniform buffer binding. The initial value is 0 for all bindings.
%% See  {@link gl:bindBufferRange/5} . 
%%
%% `?GL_UNIFORM_BUFFER_START':  When used with indexed variants of ``gl:get'' (such
%% as ``gl:getInteger64i_v''),  `Params'  returns a single value, the start offset of
%% the binding range for each indexed uniform buffer binding. The initial value is 0 for
%% all bindings. See  {@link gl:bindBufferRange/5} . 
%%
%% `?GL_UNPACK_ALIGNMENT':  `Params'  returns one value, the byte alignment used
%% for reading pixel data from memory. The initial value is 4. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_UNPACK_IMAGE_HEIGHT':  `Params'  returns one value, the image height used
%% for reading pixel data from memory. The initial is 0. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_UNPACK_LSB_FIRST':  `Params'  returns a single boolean value indicating whether
%% single-bit pixels being read from memory are read first from the least significant bit
%% of each unsigned byte. The initial value is `?GL_FALSE'. See  {@link gl:pixelStoref/2} .
%% 
%%
%% `?GL_UNPACK_ROW_LENGTH':  `Params'  returns one value, the row length used for
%% reading pixel data from memory. The initial value is 0. See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_UNPACK_SKIP_IMAGES':  `Params'  returns one value, the number of pixel images
%% skipped before the first pixel is read from memory. The initial value is 0. See  {@link gl:pixelStoref/2} 
%% . 
%%
%% `?GL_UNPACK_SKIP_PIXELS':  `Params'  returns one value, the number of pixel locations
%% skipped before the first pixel is read from memory. The initial value is 0. See  {@link gl:pixelStoref/2} 
%% . 
%%
%% `?GL_UNPACK_SKIP_ROWS':  `Params'  returns one value, the number of rows of pixel
%% locations skipped before the first pixel is read from memory. The initial value is 0.
%% See  {@link gl:pixelStoref/2} . 
%%
%% `?GL_UNPACK_SWAP_BYTES':  `Params'  returns a single boolean value indicating
%% whether the bytes of two-byte and four-byte pixel indices and components are swapped after
%% being read from memory. The initial value is `?GL_FALSE'. See  {@link gl:pixelStoref/2} .
%% 
%%
%% `?GL_VERTEX_PROGRAM_POINT_SIZE':  `Params'  returns a single boolean value indicating
%% whether vertex program point size mode is enabled. If enabled, and a vertex shader is
%% active, then the point size is taken from the shader built-in gl_PointSize. If disabled,
%% and a vertex shader is active, then the point size is taken from the point state as specified
%% by  {@link gl:pointSize/1} . The initial value is `?GL_FALSE'. 
%%
%% `?GL_VIEWPORT':  When used with non-indexed variants of ``gl:get'' (such as ``gl:getIntegerv''
%% ),  `Params'  returns four values: the   x and   y window coordinates of the viewport,
%% followed by its width and height. Initially the   x and   y window coordinates are both
%% set to 0, and the width and height are set to the width and height of the window into
%% which the GL will do its rendering. See  {@link gl:viewport/4} .  When used with indexed
%% variants of ``gl:get'' (such as ``gl:getIntegeri_v''),  `Params'  returns four
%% values: the   x and   y window coordinates of the indexed viewport, followed by its width
%% and height. Initially the   x and   y window coordinates are both set to 0, and the width
%% and height are set to the width and height of the window into which the GL will do its
%% rendering. See  {@link gl:viewportIndexedf/5} . 
%%
%% `?GL_VIEWPORT_BOUNDS_RANGE':  `Params'  returns two values, the minimum and maximum
%% viewport bounds range. The minimum range should be at least [-32768, 32767]. 
%%
%% `?GL_VIEWPORT_INDEX_PROVOKING_VERTEX':  `Params'  returns one value, the implementation
%% dependent specifc vertex of a primitive that is used to select the viewport index.  If
%% the value returned is equivalent to `?GL_PROVOKING_VERTEX', then the vertex  selection
%% follows the convention specified by  {@link gl:provokingVertex/1} . If the value returned
%% is equivalent to `?GL_FIRST_VERTEX_CONVENTION', then the  selection is always taken
%% from the first vertex in the primitive. If the value returned is equivalent to `?GL_LAST_VERTEX_CONVENTION'
%% , then the  selection is always taken from the last vertex in the primitive. If the value
%% returned is equivalent to `?GL_UNDEFINED_VERTEX', then the  selection is not guaranteed
%% to be taken from any specific vertex in the primitive. 
%%
%% `?GL_VIEWPORT_SUBPIXEL_BITS':  `Params'  returns a single value, the number of
%% bits of sub-pixel precision which the GL  uses to interpret the floating point viewport
%% bounds. The minimum value is 0. 
%%
%%  Many of the boolean parameters can also be queried more easily using  {@link gl:isEnabled/1} 
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGet.xml">external</a> documentation.
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
%%  The symbolic mask constants and their associated GL state are as follows (the second
%% column lists which attributes are saved): 
%%
%% <table><tbody><tr><td>`?GL_ACCUM_BUFFER_BIT'</td><td> Accumulation buffer clear value
%% </td></tr><tr><td>`?GL_COLOR_BUFFER_BIT'</td><td>`?GL_ALPHA_TEST' enable bit </td>
%% </tr><tr><td></td><td> Alpha test function and reference value </td></tr><tr><td></td><td>
%% `?GL_BLEND' enable bit </td></tr><tr><td></td><td> Blending source and destination
%% functions </td></tr><tr><td></td><td> Constant blend color </td></tr><tr><td></td><td>
%% Blending equation </td></tr><tr><td></td><td>`?GL_DITHER' enable bit </td></tr><tr><td>
%% </td><td>`?GL_DRAW_BUFFER' setting </td></tr><tr><td></td><td>`?GL_COLOR_LOGIC_OP'
%%  enable bit </td></tr><tr><td></td><td>`?GL_INDEX_LOGIC_OP' enable bit </td></tr><tr>
%% <td></td><td> Logic op function </td></tr><tr><td></td><td> Color mode and index mode
%% clear values </td></tr><tr><td></td><td> Color mode and index mode writemasks </td></tr><tr>
%% <td>`?GL_CURRENT_BIT'</td><td> Current RGBA color </td></tr><tr><td></td><td> Current
%% color index </td></tr><tr><td></td><td> Current normal vector </td></tr><tr><td></td><td>
%% Current texture coordinates </td></tr><tr><td></td><td> Current raster position </td></tr>
%% <tr><td></td><td>`?GL_CURRENT_RASTER_POSITION_VALID' flag </td></tr><tr><td></td><td>
%%  RGBA color associated with current raster position </td></tr><tr><td></td><td> Color
%% index associated with current raster position </td></tr><tr><td></td><td> Texture coordinates
%% associated with current raster position </td></tr><tr><td></td><td>`?GL_EDGE_FLAG'
%% flag </td></tr><tr><td>`?GL_DEPTH_BUFFER_BIT'</td><td>`?GL_DEPTH_TEST' enable
%% bit </td></tr><tr><td></td><td> Depth buffer test function </td></tr><tr><td></td><td>
%% Depth buffer clear value </td></tr><tr><td></td><td>`?GL_DEPTH_WRITEMASK' enable
%% bit </td></tr><tr><td>`?GL_ENABLE_BIT'</td><td>`?GL_ALPHA_TEST' flag </td></tr><tr>
%% <td></td><td>`?GL_AUTO_NORMAL' flag </td></tr><tr><td></td><td>`?GL_BLEND' flag
%% </td></tr><tr><td></td><td> Enable bits for the user-definable clipping planes </td></tr><tr>
%% <td></td><td>`?GL_COLOR_MATERIAL'</td></tr><tr><td></td><td>`?GL_CULL_FACE'
%% flag </td></tr><tr><td></td><td>`?GL_DEPTH_TEST' flag </td></tr><tr><td></td><td>`?GL_DITHER'
%%  flag </td></tr><tr><td></td><td>`?GL_FOG' flag </td></tr><tr><td></td><td>`?GL_LIGHT'
%% `i'    where     `?0' &lt;=       `i' &lt;     `?GL_MAX_LIGHTS'</td></tr>
%% <tr><td></td><td>`?GL_LIGHTING' flag </td></tr><tr><td></td><td>`?GL_LINE_SMOOTH'
%%  flag </td></tr><tr><td></td><td>`?GL_LINE_STIPPLE' flag </td></tr><tr><td></td><td>`?GL_COLOR_LOGIC_OP'
%%  flag </td></tr><tr><td></td><td>`?GL_INDEX_LOGIC_OP' flag </td></tr><tr><td></td><td>
%% `?GL_MAP1_'`x' where `x' is a map type </td></tr><tr><td></td><td>`?GL_MAP2_'
%% `x' where `x' is a map type </td></tr><tr><td></td><td>`?GL_MULTISAMPLE'
%% flag </td></tr><tr><td></td><td>`?GL_NORMALIZE' flag </td></tr><tr><td></td><td>`?GL_POINT_SMOOTH'
%%  flag </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_LINE' flag </td></tr><tr><td></td>
%% <td>`?GL_POLYGON_OFFSET_FILL' flag </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_POINT'
%%  flag </td></tr><tr><td></td><td>`?GL_POLYGON_SMOOTH' flag </td></tr><tr><td></td><td>
%% `?GL_POLYGON_STIPPLE' flag </td></tr><tr><td></td><td>`?GL_SAMPLE_ALPHA_TO_COVERAGE'
%%  flag </td></tr><tr><td></td><td>`?GL_SAMPLE_ALPHA_TO_ONE' flag </td></tr><tr><td></td>
%% <td>`?GL_SAMPLE_COVERAGE' flag </td></tr><tr><td></td><td>`?GL_SCISSOR_TEST'
%% flag </td></tr><tr><td></td><td>`?GL_STENCIL_TEST' flag </td></tr><tr><td></td><td>`?GL_TEXTURE_1D'
%%  flag </td></tr><tr><td></td><td>`?GL_TEXTURE_2D' flag </td></tr><tr><td></td><td>`?GL_TEXTURE_3D'
%%  flag </td></tr><tr><td></td><td> Flags `?GL_TEXTURE_GEN_'`x' where `x'
%% is S, T, R, or Q </td></tr><tr><td>`?GL_EVAL_BIT'</td><td>`?GL_MAP1_'`x'
%% enable bits, where `x' is a map type </td></tr><tr><td></td><td>`?GL_MAP2_'`x'
%%  enable bits, where `x' is a map type </td></tr><tr><td></td><td> 1D grid endpoints
%% and divisions </td></tr><tr><td></td><td> 2D grid endpoints and divisions </td></tr><tr><td>
%% </td><td>`?GL_AUTO_NORMAL' enable bit </td></tr><tr><td>`?GL_FOG_BIT'</td><td>`?GL_FOG'
%%  enable bit </td></tr><tr><td></td><td> Fog color </td></tr><tr><td></td><td> Fog density
%% </td></tr><tr><td></td><td> Linear fog start </td></tr><tr><td></td><td> Linear fog end </td>
%% </tr><tr><td></td><td> Fog index </td></tr><tr><td></td><td>`?GL_FOG_MODE' value </td>
%% </tr><tr><td>`?GL_HINT_BIT'</td><td>`?GL_PERSPECTIVE_CORRECTION_HINT' setting </td>
%% </tr><tr><td></td><td>`?GL_POINT_SMOOTH_HINT' setting </td></tr><tr><td></td><td>`?GL_LINE_SMOOTH_HINT'
%%  setting </td></tr><tr><td></td><td>`?GL_POLYGON_SMOOTH_HINT' setting </td></tr><tr><td>
%% </td><td>`?GL_FOG_HINT' setting </td></tr><tr><td></td><td>`?GL_GENERATE_MIPMAP_HINT'
%%  setting </td></tr><tr><td></td><td>`?GL_TEXTURE_COMPRESSION_HINT' setting </td></tr>
%% <tr><td>`?GL_LIGHTING_BIT'</td><td>`?GL_COLOR_MATERIAL' enable bit </td></tr><tr>
%% <td></td><td>`?GL_COLOR_MATERIAL_FACE' value </td></tr><tr><td></td><td> Color material
%% parameters that are tracking the current color </td></tr><tr><td></td><td> Ambient scene
%% color </td></tr><tr><td></td><td>`?GL_LIGHT_MODEL_LOCAL_VIEWER' value </td></tr><tr><td>
%% </td><td>`?GL_LIGHT_MODEL_TWO_SIDE' setting </td></tr><tr><td></td><td>`?GL_LIGHTING'
%%  enable bit </td></tr><tr><td></td><td> Enable bit for each light </td></tr><tr><td></td><td>
%%  Ambient, diffuse, and specular intensity for each light </td></tr><tr><td></td><td> Direction,
%% position, exponent, and cutoff angle for each light </td></tr><tr><td></td><td> Constant,
%% linear, and quadratic attenuation factors for each light </td></tr><tr><td></td><td> Ambient,
%% diffuse, specular, and emissive color for each material </td></tr><tr><td></td><td> Ambient,
%% diffuse, and specular color indices for each material </td></tr><tr><td></td><td> Specular
%% exponent for each material </td></tr><tr><td></td><td>`?GL_SHADE_MODEL' setting </td>
%% </tr><tr><td>`?GL_LINE_BIT'</td><td>`?GL_LINE_SMOOTH' flag </td></tr><tr><td></td>
%% <td>`?GL_LINE_STIPPLE' enable bit </td></tr><tr><td></td><td> Line stipple pattern
%% and repeat counter </td></tr><tr><td></td><td> Line width </td></tr><tr><td>`?GL_LIST_BIT'
%% </td><td>`?GL_LIST_BASE' setting </td></tr><tr><td>`?GL_MULTISAMPLE_BIT'</td><td>
%% `?GL_MULTISAMPLE' flag </td></tr><tr><td></td><td>`?GL_SAMPLE_ALPHA_TO_COVERAGE'
%% flag </td></tr><tr><td></td><td>`?GL_SAMPLE_ALPHA_TO_ONE' flag </td></tr><tr><td></td>
%% <td>`?GL_SAMPLE_COVERAGE' flag </td></tr><tr><td></td><td>`?GL_SAMPLE_COVERAGE_VALUE'
%%  value </td></tr><tr><td></td><td>`?GL_SAMPLE_COVERAGE_INVERT' value </td></tr><tr><td>
%% `?GL_PIXEL_MODE_BIT'</td><td>`?GL_RED_BIAS' and `?GL_RED_SCALE' settings </td>
%% </tr><tr><td></td><td>`?GL_GREEN_BIAS' and `?GL_GREEN_SCALE' values </td></tr><tr>
%% <td></td><td>`?GL_BLUE_BIAS' and `?GL_BLUE_SCALE'</td></tr><tr><td></td><td>`?GL_ALPHA_BIAS'
%%  and `?GL_ALPHA_SCALE'</td></tr><tr><td></td><td>`?GL_DEPTH_BIAS' and `?GL_DEPTH_SCALE'
%% </td></tr><tr><td></td><td>`?GL_INDEX_OFFSET' and `?GL_INDEX_SHIFT' values </td>
%% </tr><tr><td></td><td>`?GL_MAP_COLOR' and `?GL_MAP_STENCIL' flags </td></tr><tr>
%% <td></td><td>`?GL_ZOOM_X' and `?GL_ZOOM_Y' factors </td></tr><tr><td></td><td>`?GL_READ_BUFFER'
%%  setting </td></tr><tr><td>`?GL_POINT_BIT'</td><td>`?GL_POINT_SMOOTH' flag </td>
%% </tr><tr><td></td><td> Point size </td></tr><tr><td>`?GL_POLYGON_BIT'</td><td>`?GL_CULL_FACE'
%%  enable bit </td></tr><tr><td></td><td>`?GL_CULL_FACE_MODE' value </td></tr><tr><td></td>
%% <td>`?GL_FRONT_FACE' indicator </td></tr><tr><td></td><td>`?GL_POLYGON_MODE'
%% setting </td></tr><tr><td></td><td>`?GL_POLYGON_SMOOTH' flag </td></tr><tr><td></td><td>
%% `?GL_POLYGON_STIPPLE' enable bit </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_FILL'
%%  flag </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_LINE' flag </td></tr><tr><td></td>
%% <td>`?GL_POLYGON_OFFSET_POINT' flag </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_FACTOR'
%% </td></tr><tr><td></td><td>`?GL_POLYGON_OFFSET_UNITS'</td></tr><tr><td>`?GL_POLYGON_STIPPLE_BIT'
%% </td><td> Polygon stipple image </td></tr><tr><td>`?GL_SCISSOR_BIT'</td><td>`?GL_SCISSOR_TEST'
%%  flag </td></tr><tr><td></td><td> Scissor box </td></tr><tr><td>`?GL_STENCIL_BUFFER_BIT'
%% </td><td>`?GL_STENCIL_TEST' enable bit </td></tr><tr><td></td><td> Stencil function
%% and reference value </td></tr><tr><td></td><td> Stencil value mask </td></tr><tr><td></td>
%% <td> Stencil fail, pass, and depth buffer pass actions </td></tr><tr><td></td><td> Stencil
%% buffer clear value </td></tr><tr><td></td><td> Stencil buffer writemask </td></tr><tr><td>
%% `?GL_TEXTURE_BIT'</td><td> Enable bits for the four texture coordinates </td></tr><tr>
%% <td></td><td> Border color for each texture image </td></tr><tr><td></td><td> Minification
%% function for each texture image </td></tr><tr><td></td><td> Magnification function for
%% each texture image </td></tr><tr><td></td><td> Texture coordinates and wrap mode for each
%% texture image </td></tr><tr><td></td><td> Color and mode for each texture environment </td>
%% </tr><tr><td></td><td> Enable bits `?GL_TEXTURE_GEN_'`x', `x' is S, T,
%% R, and Q </td></tr><tr><td></td><td>`?GL_TEXTURE_GEN_MODE' setting for S, T, R, and
%% Q </td></tr><tr><td></td><td> {@link gl:texGend/3}  plane equations for S, T, R, and Q </td></tr>
%% <tr><td></td><td> Current texture bindings (for example, `?GL_TEXTURE_BINDING_2D') </td>
%% </tr><tr><td>`?GL_TRANSFORM_BIT'</td><td> Coefficients of the six clipping planes </td>
%% </tr><tr><td></td><td> Enable bits for the user-definable clipping planes </td></tr><tr><td>
%% </td><td>`?GL_MATRIX_MODE' value </td></tr><tr><td></td><td>`?GL_NORMALIZE'
%% flag </td></tr><tr><td></td><td>`?GL_RESCALE_NORMAL' flag </td></tr><tr><td>`?GL_VIEWPORT_BIT'
%% </td><td> Depth range (near and far) </td></tr><tr><td></td><td> Viewport origin and extent
%% </td></tr></tbody></table>
%%
%%  {@link gl:pushAttrib/1}  restores the values of the state variables saved with the last ``gl:pushAttrib''
%%  command. Those not saved are left unchanged. 
%%
%%  It is an error to push attributes onto a full stack or to pop attributes off an empty
%% stack. In either case, the error flag is set and no other change is made to GL state. 
%%
%%  Initially, the attribute stack is empty. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushAttrib.xml">external</a> documentation.
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
%%  The symbolic mask constants and their associated GL client state are as follows (the
%% second column lists which attributes are saved): 
%%
%% `?GL_CLIENT_PIXEL_STORE_BIT' Pixel storage modes `?GL_CLIENT_VERTEX_ARRAY_BIT'
%% Vertex arrays (and enables) 
%%
%%  {@link gl:pushClientAttrib/1}  restores the values of the client-state variables saved with
%% the last ``gl:pushClientAttrib''. Those not saved are left unchanged. 
%%
%%  It is an error to push attributes onto a full client attribute stack or to pop attributes
%% off an empty stack. In either case, the error flag is set, and no other change is made
%% to GL state. 
%%
%%  Initially, the client attribute stack is empty. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushClientAttrib.xml">external</a> documentation.
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
%% `?GL_RENDER':  Render mode. Primitives are rasterized, producing pixel fragments,
%% which are written into the frame buffer. This is the normal mode and also the default
%% mode. 
%%
%% `?GL_SELECT':  Selection mode. No pixel fragments are produced, and no change to
%% the frame buffer contents is made. Instead, a record of the names of primitives that would
%% have been drawn if the render mode had been `?GL_RENDER' is returned in a select
%% buffer, which must be created (see  {@link gl:selectBuffer/2} ) before selection mode is
%% entered. 
%%
%% `?GL_FEEDBACK':  Feedback mode. No pixel fragments are produced, and no change to
%% the frame buffer contents is made. Instead, the coordinates and attributes of vertices
%% that would have been drawn if the render mode had been `?GL_RENDER' is returned in
%% a feedback buffer, which must be created (see  {@link gl:feedbackBuffer/3} ) before feedback
%% mode is entered. 
%%
%%  The return value of ``gl:renderMode'' is determined by the render mode at the time ``gl:renderMode''
%%  is called, rather than by  `Mode' . The values returned for the three render modes
%% are as follows: 
%%
%% `?GL_RENDER':  0. 
%%
%% `?GL_SELECT':  The number of hit records transferred to the select buffer. 
%%
%% `?GL_FEEDBACK':  The number of values (not vertices) transferred to the feedback
%% buffer. 
%%
%%  See the  {@link gl:selectBuffer/2}  and  {@link gl:feedbackBuffer/3}  reference pages for more
%% details concerning selection and feedback operation. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderMode.xml">external</a> documentation.
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
%%  To allow for distributed implementations, there may be several error flags. If any single
%% error flag has recorded an error, the value of that flag is returned and that flag is
%% reset to `?GL_NO_ERROR' when ``gl:getError'' is called. If more than one flag has
%% recorded an error, ``gl:getError'' returns and clears an arbitrary error flag value.
%% Thus, ``gl:getError'' should always be called in a loop, until it returns `?GL_NO_ERROR'
%% , if all error flags are to be reset. 
%%
%%  Initially, all error flags are set to `?GL_NO_ERROR'. 
%%
%%  The following errors are currently defined: 
%%
%% `?GL_NO_ERROR':  No error has been recorded. The value of this symbolic constant
%% is guaranteed to be 0. 
%%
%% `?GL_INVALID_ENUM':  An unacceptable value is specified for an enumerated argument.
%% The offending command is ignored and has no other side effect than to set the error flag.
%% 
%%
%% `?GL_INVALID_VALUE':  A numeric argument is out of range. The offending command is
%% ignored and has no other side effect than to set the error flag. 
%%
%% `?GL_INVALID_OPERATION':  The specified operation is not allowed in the current state.
%% The offending command is ignored and has no other side effect than to set the error flag.
%% 
%%
%% `?GL_INVALID_FRAMEBUFFER_OPERATION':  The framebuffer object is not complete. The
%% offending command is ignored and has no other side effect than to set the error flag. 
%%
%% `?GL_OUT_OF_MEMORY':  There is not enough memory left to execute the command. The
%% state of the GL is undefined, except for the state of the error flags, after this error
%% is recorded. 
%%
%%  When an error flag is set, results of a GL operation are undefined only if `?GL_OUT_OF_MEMORY'
%%  has occurred. In all other cases, the command generating the error is ignored and has
%% no effect on the GL state or frame buffer contents. If the generating command returns
%% a value, it returns 0. If ``gl:getError'' itself generates an error, it returns 0. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetError.xml">external</a> documentation.
-spec getError() -> enum().
getError() ->
  call(5074, <<>>).

%% @doc Return a string describing the current GL connection
%%
%% ``gl:getString'' returns a pointer to a static string describing some aspect of the
%% current GL connection.  `Name'  can be one of the following: 
%%
%% `?GL_VENDOR':  Returns the company responsible for this GL implementation. This name
%% does not change from release to release. 
%%
%% `?GL_RENDERER':  Returns the name of the renderer. This name is typically specific
%% to a particular configuration of a hardware platform. It does not change from release
%% to release. 
%%
%% `?GL_VERSION':  Returns a version or release number. 
%%
%% `?GL_SHADING_LANGUAGE_VERSION':  Returns a version or release number for the shading
%% language. 
%%
%% ``gl:getStringi'' returns a pointer to a static string indexed by  `Index' .  `Name' 
%%  can be one of the following: 
%%
%% `?GL_EXTENSIONS':  For ``gl:getStringi'' only, returns the extension string supported
%% by the implementation at  `Index' . 
%%
%%  Strings `?GL_VENDOR' and `?GL_RENDERER' together uniquely specify a platform.
%% They do not change from release to release and should be used by platform-recognition
%% algorithms. 
%%
%%  The `?GL_VERSION' and `?GL_SHADING_LANGUAGE_VERSION' strings begin with a version
%% number. The version number uses one of these forms: 
%%
%% `major_number.minor_number'`major_number.minor_number.release_number'
%%
%%  Vendor-specific information may follow the version number. Its format depends on the
%% implementation, but a space always separates the version number and the vendor-specific
%% information. 
%%
%%  All strings are null-terminated. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetString.xml">external</a> documentation.
-spec getString(Name) -> string() when Name :: enum().
getString(Name) ->
  call(5075, <<Name:?GLenum>>).

%% @doc Block until all GL execution is complete
%%
%% ``gl:finish'' does not return until the effects of all previously called GL commands
%% are complete. Such effects include all changes to GL state, all changes to connection
%% state, and all changes to the frame buffer contents. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFinish.xml">external</a> documentation.
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
%%  Because any GL program might be executed over a network, or on an accelerator that buffers
%% commands, all programs should call ``gl:flush'' whenever they count on having all of
%% their previously issued commands completed. For example, call ``gl:flush'' before waiting
%% for user input that depends on the generated image. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlush.xml">external</a> documentation.
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
%% `?GL_FASTEST':  The most efficient option should be chosen. 
%%
%% `?GL_NICEST':  The most correct, or highest quality, option should be chosen. 
%%
%% `?GL_DONT_CARE':  No preference. 
%%
%%  Though the implementation aspects that can be hinted are well defined, the interpretation
%% of the hints depends on the implementation. The hint aspects that can be specified with  `Target' 
%% , along with suggested semantics, are as follows: 
%%
%% `?GL_FRAGMENT_SHADER_DERIVATIVE_HINT':  Indicates the accuracy of the derivative
%% calculation for the GL shading language fragment processing built-in functions: `?dFdx'
%% , `?dFdy', and `?fwidth'. 
%%
%% `?GL_LINE_SMOOTH_HINT':  Indicates the sampling quality of antialiased lines. If
%% a larger filter function is applied, hinting `?GL_NICEST' can result in more pixel
%% fragments being generated during rasterization. 
%%
%% `?GL_POLYGON_SMOOTH_HINT':  Indicates the sampling quality of antialiased polygons.
%% Hinting `?GL_NICEST' can result in more pixel fragments being generated during rasterization,
%% if a larger filter function is applied. 
%%
%% `?GL_TEXTURE_COMPRESSION_HINT':  Indicates the quality and performance of the compressing
%% texture images. Hinting `?GL_FASTEST' indicates that texture images should be compressed
%% as quickly as possible, while `?GL_NICEST' indicates that texture images should be
%% compressed with as little image quality loss as possible. `?GL_NICEST' should be
%% selected if the texture is to be retrieved by  {@link gl:getCompressedTexImage/3}  for reuse.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHint.xml">external</a> documentation.
-spec hint(Target, Mode) -> 'ok' when Target :: enum(),Mode :: enum().
hint(Target,Mode) ->
  cast(5078, <<Target:?GLenum,Mode:?GLenum>>).

%% @doc Specify the clear value for the depth buffer
%%
%% ``gl:clearDepth'' specifies the depth value used by  {@link gl:clear/1}  to clear the depth
%% buffer. Values specified by ``gl:clearDepth'' are clamped to the range  [0 1]. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearDepth.xml">external</a> documentation.
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
%%  `Func'  specifies the conditions under which the pixel will be drawn. The comparison
%% functions are as follows: 
%%
%% `?GL_NEVER':  Never passes. 
%%
%% `?GL_LESS':  Passes if the incoming depth value is less than the stored depth value.
%% 
%%
%% `?GL_EQUAL':  Passes if the incoming depth value is equal to the stored depth value.
%% 
%%
%% `?GL_LEQUAL':  Passes if the incoming depth value is less than or equal to the stored
%% depth value. 
%%
%% `?GL_GREATER':  Passes if the incoming depth value is greater than the stored depth
%% value. 
%%
%% `?GL_NOTEQUAL':  Passes if the incoming depth value is not equal to the stored depth
%% value. 
%%
%% `?GL_GEQUAL':  Passes if the incoming depth value is greater than or equal to the
%% stored depth value. 
%%
%% `?GL_ALWAYS':  Always passes. 
%%
%%  The initial value of  `Func'  is `?GL_LESS'. Initially, depth testing is disabled.
%% If depth testing is disabled or if no depth buffer exists, it is as if the depth test
%% always passes. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthFunc.xml">external</a> documentation.
-spec depthFunc(Func) -> 'ok' when Func :: enum().
depthFunc(Func) ->
  cast(5080, <<Func:?GLenum>>).

%% @doc Enable or disable writing into the depth buffer
%%
%% ``gl:depthMask'' specifies whether the depth buffer is enabled for writing. If  `Flag' 
%%  is `?GL_FALSE', depth buffer writing is disabled. Otherwise, it is enabled. Initially,
%% depth buffer writing is enabled. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthMask.xml">external</a> documentation.
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
%%  The setting of (0,1) maps the near plane to 0 and the far plane to 1. With this mapping,
%% the depth buffer range is fully utilized. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRange.xml">external</a> documentation.
-spec depthRange(Near_val, Far_val) -> 'ok' when Near_val :: clamp(),Far_val :: clamp().
depthRange(Near_val,Far_val) ->
  cast(5082, <<Near_val:?GLclampd,Far_val:?GLclampd>>).

%% @doc Specify clear values for the accumulation buffer
%%
%% ``gl:clearAccum'' specifies the red, green, blue, and alpha values used by  {@link gl:clear/1} 
%%  to clear the accumulation buffer. 
%%
%%  Values specified by ``gl:clearAccum'' are clamped to the range  [-1 1]. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearAccum.xml">external</a> documentation.
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
%%  Each pixel in the accumulation buffer consists of red, green, blue, and alpha values.
%% The number of bits per component in the accumulation buffer depends on the implementation.
%% You can examine this number by calling  {@link gl:getBooleanv/1}  four times, with arguments
%% `?GL_ACCUM_RED_BITS', `?GL_ACCUM_GREEN_BITS', `?GL_ACCUM_BLUE_BITS', and `?GL_ACCUM_ALPHA_BITS'
%% . Regardless of the number of bits per component, the range of values stored by each component
%% is  [-1 1]. The accumulation buffer pixels are mapped one-to-one with frame buffer pixels. 
%%
%% ``gl:accum'' operates on the accumulation buffer. The first argument,  `Op' , is
%% a symbolic constant that selects an accumulation buffer operation. The second argument,  `Value' 
%% , is a floating-point value to be used in that operation. Five operations are specified: `?GL_ACCUM'
%% , `?GL_LOAD', `?GL_ADD', `?GL_MULT', and `?GL_RETURN'. 
%%
%%  All accumulation buffer operations are limited to the area of the current scissor box
%% and applied identically to the red, green, blue, and alpha components of each pixel. If
%% a ``gl:accum'' operation results in a value outside the range  [-1 1], the contents of an
%% accumulation buffer pixel component are undefined. 
%%
%%  The operations are as follows: 
%%
%% `?GL_ACCUM':  Obtains R, G, B, and A values from the buffer currently selected for
%% reading (see  {@link gl:readBuffer/1} ). Each component value is divided by   2 n-1, where  
%% n is the number of bits allocated to each color component in the currently selected buffer.
%% The result is a floating-point value in the range  [0 1], which is multiplied by  `Value' 
%% and added to the corresponding pixel component in the accumulation buffer, thereby updating
%% the accumulation buffer. 
%%
%% `?GL_LOAD':  Similar to `?GL_ACCUM', except that the current value in the accumulation
%% buffer is not used in the calculation of the new value. That is, the R, G, B, and A values
%% from the currently selected buffer are divided by   2 n-1, multiplied by  `Value' ,
%% and then stored in the corresponding accumulation buffer cell, overwriting the current
%% value. 
%%
%% `?GL_ADD':  Adds  `Value'  to each R, G, B, and A in the accumulation buffer. 
%%
%% `?GL_MULT':  Multiplies each R, G, B, and A in the accumulation buffer by  `Value' 
%%  and returns the scaled component to its corresponding accumulation buffer location. 
%%
%% `?GL_RETURN':  Transfers accumulation buffer values to the color buffer or buffers
%% currently selected for writing. Each R, G, B, and A component is multiplied by  `Value' 
%% , then multiplied by   2 n-1, clamped to the range  [0 2 n-1], and stored in the corresponding
%% display buffer cell. The only fragment operations that are applied to this transfer are
%% pixel ownership, scissor, dithering, and color writemasks. 
%%
%%  To clear the accumulation buffer, call  {@link gl:clearAccum/4}  with R, G, B, and A values
%% to set it to, then call  {@link gl:clear/1}  with the accumulation buffer enabled. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAccum.xml">external</a> documentation.
-spec accum(Op, Value) -> 'ok' when Op :: enum(),Value :: float().
accum(Op,Value) ->
  cast(5084, <<Op:?GLenum,Value:?GLfloat>>).

%% @doc Specify which matrix is the current matrix
%%
%% ``gl:matrixMode'' sets the current matrix mode.  `Mode'  can assume one of four values:
%% 
%%
%% `?GL_MODELVIEW':  Applies subsequent matrix operations to the modelview matrix stack.
%% 
%%
%% `?GL_PROJECTION':  Applies subsequent matrix operations to the projection matrix
%% stack. 
%%
%% `?GL_TEXTURE':  Applies subsequent matrix operations to the texture matrix stack. 
%%
%% `?GL_COLOR':  Applies subsequent matrix operations to the color matrix stack. 
%%
%%  To find out which matrix stack is currently the target of all matrix operations, call  {@link gl:getBooleanv/1} 
%%  with argument `?GL_MATRIX_MODE'. The initial value is `?GL_MODELVIEW'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixMode.xml">external</a> documentation.
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
%% ((2/(right-left)) 0 0(t x) 0(2/(top-bottom)) 0(t y) 0 0(-2/(farVal-nearVal))(t z) 0 0 0 1)
%%
%%  where  t x=-((right+left)/(right-left)) t y=-((top+bottom)/(top-bottom)) t z=-((farVal+nearVal)/(farVal-nearVal))
%%
%%  Typically, the matrix mode is `?GL_PROJECTION', and (left bottom-nearVal) and  (right top-nearVal) specify the points on
%% the near clipping plane that are mapped to the lower left and upper right corners of the
%% window, respectively, assuming that the eye is located at (0, 0, 0). -farVal specifies
%% the location of the far clipping plane. Both  `NearVal'  and  `FarVal'  can be either
%% positive or negative. 
%%
%%  Use  {@link gl:pushMatrix/0}  and  {@link gl:pushMatrix/0}  to save and restore the current
%% matrix stack. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml">external</a> documentation.
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
%% [((2  nearVal)/(right-left)) 0 A 0 0((2  nearVal)/(top-bottom)) B 0 0 0 C D 0 0 -1 0]
%%
%%  A=(right+left)/(right-left)
%%
%%  B=(top+bottom)/(top-bottom)
%%
%%  C=-((farVal+nearVal)/(farVal-nearVal))
%%
%%  D=-((2  farVal  nearVal)/(farVal-nearVal))
%%
%%  Typically, the matrix mode is `?GL_PROJECTION', and (left bottom-nearVal) and  (right top-nearVal) specify the points on
%% the near clipping plane that are mapped to the lower left and upper right corners of the
%% window, assuming that the eye is located at (0, 0, 0). -farVal specifies the location
%% of the far clipping plane. Both  `NearVal'  and  `FarVal'  must be positive. 
%%
%%  Use  {@link gl:pushMatrix/0}  and  {@link gl:pushMatrix/0}  to save and restore the current
%% matrix stack. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFrustum.xml">external</a> documentation.
-spec frustum(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
frustum(Left,Right,Bottom,Top,Near_val,Far_val) ->
  cast(5087, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble,Near_val:?GLdouble,Far_val:?GLdouble>>).

%% @doc Set the viewport
%%
%% ``gl:viewport'' specifies the affine transformation of   x and   y from normalized device
%% coordinates to window coordinates. Let  (x nd y nd) be normalized device coordinates. Then the window
%% coordinates  (x w y w) are computed as follows: 
%%
%%  x w=(x nd+1) (width/2)+x
%%
%%  y w=(y nd+1) (height/2)+y
%%
%%  Viewport width and height are silently clamped to a range that depends on the implementation.
%% To query this range, call  {@link gl:getBooleanv/1}  with argument `?GL_MAX_VIEWPORT_DIMS'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewport.xml">external</a> documentation.
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
%% ``gl:pushMatrix'' pushes the current matrix stack down by one, duplicating the current
%% matrix. That is, after a ``gl:pushMatrix'' call, the matrix on top of the stack is identical
%% to the one below it. 
%%
%%  {@link gl:pushMatrix/0}  pops the current matrix stack, replacing the current matrix with
%% the one below it on the stack. 
%%
%%  Initially, each of the stacks contains one matrix, an identity matrix. 
%%
%%  It is an error to push a full matrix stack or to pop a matrix stack that contains only
%% a single matrix. In either case, the error flag is set and no other change is made to
%% GL state. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushMatrix.xml">external</a> documentation.
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
%% ((1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))
%%
%%  but in some cases it is more efficient. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadIdentity.xml">external</a> documentation.
-spec loadIdentity() -> 'ok'.
loadIdentity() ->
  cast(5091, <<>>).

%% @doc Replace the current matrix with the specified matrix
%%
%% ``gl:loadMatrix'' replaces the current matrix with the one whose elements are specified
%% by  `M' . The current matrix is the projection matrix, modelview matrix, or texture
%% matrix, depending on the current matrix mode (see  {@link gl:matrixMode/1} ). 
%%
%%  The current matrix, M, defines a transformation of coordinates. For instance, assume
%% M refers to the modelview matrix. If   v=(v[0] v[1] v[2] v[3]) is the set of object coordinates of a vertex,
%% and  `M'  points to an array of   16 single- or double-precision floating-point values
%%   m={m[0] m[1] ... m[15]}, then the modelview transformation   M(v) does the following: 
%%
%%  M(v)=(m[0] m[4] m[8] m[12] m[1] m[5] m[9] m[13] m[2] m[6] m[10] m[14] m[3] m[7] m[11] m[15])×(v[0] v[1] v[2] v[3])
%%
%%  Projection and texture transformations are similarly defined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadMatrix.xml">external</a> documentation.
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
%%  The current matrix is determined by the current matrix mode (see  {@link gl:matrixMode/1} ).
%% It is either the projection matrix, modelview matrix, or the texture matrix. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultMatrix.xml">external</a> documentation.
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
%% (x 2(1-c)+c x  y(1-c)-z  s x  z(1-c)+y  s 0 y  x(1-c)+z  s y 2(1-c)+c y  z(1-c)-x  s 0 x  z(1-c)-y  s y  z(1-c)+x  s z 2(1-c)+c 0 0 0 0
%% 1)
%%
%%  Where   c=cos(angle),   s=sin(angle), and ||(x y z)||=1 (if not, the GL will normalize this vector). 
%%
%%  If the matrix mode is either `?GL_MODELVIEW' or `?GL_PROJECTION', all objects
%% drawn after ``gl:rotate'' is called are rotated. Use  {@link gl:pushMatrix/0}  and  {@link gl:pushMatrix/0} 
%%  to save and restore the unrotated coordinate system. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRotate.xml">external</a> documentation.
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
%%  The current matrix (see  {@link gl:matrixMode/1} ) is multiplied by this scale matrix, and
%% the product replaces the current matrix as if  {@link gl:multMatrixd/1}  were called with
%% the following matrix as its argument: 
%%
%% (x 0 0 0 0 y 0 0 0 0 z 0 0 0 0 1)
%%
%%  If the matrix mode is either `?GL_MODELVIEW' or `?GL_PROJECTION', all objects
%% drawn after ``gl:scale'' is called are scaled. 
%%
%%  Use  {@link gl:pushMatrix/0}  and  {@link gl:pushMatrix/0}  to save and restore the unscaled
%% coordinate system. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScale.xml">external</a> documentation.
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
%% (1 0 0 x 0 1 0 y 0 0 1 z 0 0 0 1)
%%
%%  If the matrix mode is either `?GL_MODELVIEW' or `?GL_PROJECTION', all objects
%% drawn after a call to ``gl:translate'' are translated. 
%%
%%  Use  {@link gl:pushMatrix/0}  and  {@link gl:pushMatrix/0}  to save and restore the untranslated
%% coordinate system. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTranslate.xml">external</a> documentation.
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
%%  A name returned by  {@link gl:genLists/1} , but not yet associated with a display list by
%% calling  {@link gl:newList/2} , is not the name of a display list. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsList.xml">external</a> documentation.
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
%%  All storage locations allocated to the specified display lists are freed, and the names
%% are available for reuse at a later time. Names within the range that do not have an associated
%% display list are ignored. If  `Range'  is 0, nothing happens. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteLists.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenLists.xml">external</a> documentation.
-spec genLists(Range) -> integer() when Range :: integer().
genLists(Range) ->
  call(5104, <<Range:?GLsizei>>).

%% @doc Create or replace a display list
%%
%%  Display lists are groups of GL commands that have been stored for subsequent execution.
%% Display lists are created with ``gl:newList''. All subsequent commands are placed in
%% the display list, in the order issued, until  {@link gl:endList/0}  is called. 
%%
%% ``gl:newList'' has two arguments. The first argument,  `List' , is a positive integer
%% that becomes the unique name for the display list. Names can be created and reserved with
%%  {@link gl:genLists/1}  and tested for uniqueness with  {@link gl:isList/1} . The second argument,
%%  `Mode' , is a symbolic constant that can assume one of two values: 
%%
%% `?GL_COMPILE':  Commands are merely compiled. 
%%
%% `?GL_COMPILE_AND_EXECUTE':  Commands are executed as they are compiled into the display
%% list. 
%%
%%  Certain commands are not compiled into the display list but are executed immediately,
%% regardless of the display-list mode. These commands are  {@link gl:areTexturesResident/1} ,  {@link gl:colorPointer/4} 
%% ,  {@link gl:deleteLists/2} ,  {@link gl:deleteTextures/1} ,  {@link gl:enableClientState/1} ,  {@link gl:edgeFlagPointer/2} 
%% ,  {@link gl:enableClientState/1} ,  {@link gl:feedbackBuffer/3} ,  {@link gl:finish/0} ,  {@link gl:flush/0} 
%% ,  {@link gl:genLists/1} ,  {@link gl:genTextures/1} ,  {@link gl:indexPointer/3} ,  {@link gl:interleavedArrays/3} 
%% ,  {@link gl:isEnabled/1} ,  {@link gl:isList/1} ,  {@link gl:isTexture/1} ,  {@link gl:normalPointer/3} 
%% ,  {@link gl:pushClientAttrib/1} ,  {@link gl:pixelStoref/2} ,  {@link gl:pushClientAttrib/1} ,  {@link gl:readPixels/7} 
%% ,  {@link gl:renderMode/1} ,  {@link gl:selectBuffer/2} ,  {@link gl:texCoordPointer/4} ,  {@link gl:vertexPointer/4} 
%% , and all of the  {@link gl:getBooleanv/1}  commands. 
%%
%%  Similarly,  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} , and  {@link gl:texImage3D/10} 
%% are executed immediately and not compiled into the display list when their first argument
%% is `?GL_PROXY_TEXTURE_1D', `?GL_PROXY_TEXTURE_1D', or `?GL_PROXY_TEXTURE_3D'
%% , respectively. 
%%
%%  When the ARB_imaging extension is supported,  {@link gl:histogram/4}  executes immediately
%% when its argument is `?GL_PROXY_HISTOGRAM'. Similarly,  {@link gl:colorTable/6}  executes
%% immediately when its first argument is `?GL_PROXY_COLOR_TABLE', `?GL_PROXY_POST_CONVOLUTION_COLOR_TABLE'
%% , or `?GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE'. 
%%
%%  For OpenGL versions 1.3 and greater, or when the ARB_multitexture extension is supported,
%%  {@link gl:clientActiveTexture/1}  is not compiled into display lists, but executed immediately.
%% 
%%
%%  When  {@link gl:endList/0}  is encountered, the display-list definition is completed by
%% associating the list with the unique name  `List'  (specified in the ``gl:newList''
%% command). If a display list with name  `List'  already exists, it is replaced only
%% when  {@link gl:endList/0}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNewList.xml">external</a> documentation.
-spec newList(List, Mode) -> 'ok' when List :: integer(),Mode :: enum().
newList(List,Mode) ->
  cast(5105, <<List:?GLuint,Mode:?GLenum>>).

%% @doc glBeginList
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginList.xml">external</a> documentation.
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
%% ``gl:callList'' can appear inside a display list. To avoid the possibility of infinite
%% recursion resulting from display lists calling one another, a limit is placed on the nesting
%% level of display lists during display-list execution. This limit is at least 64, and it
%% depends on the implementation. 
%%
%%  GL state is not saved and restored across a call to ``gl:callList''. Thus, changes
%% made to GL state during the execution of a display list remain after execution of the
%% display list is completed. Use  {@link gl:pushAttrib/1} ,  {@link gl:pushAttrib/1} ,  {@link gl:pushMatrix/0} 
%% , and  {@link gl:pushMatrix/0}  to preserve GL state across ``gl:callList'' calls. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallList.xml">external</a> documentation.
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
%% ``gl:callLists'' provides an efficient means for executing more than one display list.  `Type' 
%%  allows lists with various name formats to be accepted. The formats are as follows: 
%%
%% `?GL_BYTE':  `Lists'  is treated as an array of signed bytes, each in the range  
%% -128 through 127. 
%%
%% `?GL_UNSIGNED_BYTE':  `Lists'  is treated as an array of unsigned bytes, each
%% in the range 0 through 255. 
%%
%% `?GL_SHORT':  `Lists'  is treated as an array of signed two-byte integers, each
%% in the range   -32768 through 32767. 
%%
%% `?GL_UNSIGNED_SHORT':  `Lists'  is treated as an array of unsigned two-byte integers,
%% each in the range 0 through 65535. 
%%
%% `?GL_INT':  `Lists'  is treated as an array of signed four-byte integers. 
%%
%% `?GL_UNSIGNED_INT':  `Lists'  is treated as an array of unsigned four-byte integers.
%% 
%%
%% `?GL_FLOAT':  `Lists'  is treated as an array of four-byte floating-point values.
%% 
%%
%% `?GL_2_BYTES':  `Lists'  is treated as an array of unsigned bytes. Each pair of
%% bytes specifies a single display-list name. The value of the pair is computed as 256 times
%% the unsigned value of the first byte plus the unsigned value of the second byte. 
%%
%% `?GL_3_BYTES':  `Lists'  is treated as an array of unsigned bytes. Each triplet
%% of bytes specifies a single display-list name. The value of the triplet is computed as
%% 65536 times the unsigned value of the first byte, plus 256 times the unsigned value of
%% the second byte, plus the unsigned value of the third byte. 
%%
%% `?GL_4_BYTES':  `Lists'  is treated as an array of unsigned bytes. Each quadruplet
%% of bytes specifies a single display-list name. The value of the quadruplet is computed
%% as 16777216 times the unsigned value of the first byte, plus 65536 times the unsigned
%% value of the second byte, plus 256 times the unsigned value of the third byte, plus the
%% unsigned value of the fourth byte. 
%%
%%  The list of display-list names is not null-terminated. Rather,  `N'  specifies how
%% many names are to be taken from  `Lists' . 
%%
%%  An additional level of indirection is made available with the  {@link gl:listBase/1}  command,
%% which specifies an unsigned offset that is added to each display-list name specified in  `Lists' 
%%  before that display list is executed. 
%%
%% ``gl:callLists'' can appear inside a display list. To avoid the possibility of infinite
%% recursion resulting from display lists calling one another, a limit is placed on the nesting
%% level of display lists during display-list execution. This limit must be at least 64,
%% and it depends on the implementation. 
%%
%%  GL state is not saved and restored across a call to ``gl:callLists''. Thus, changes
%% made to GL state during the execution of the display lists remain after execution is completed.
%% Use  {@link gl:pushAttrib/1} ,  {@link gl:pushAttrib/1} ,  {@link gl:pushMatrix/0} , and  {@link gl:pushMatrix/0} 
%%  to preserve GL state across ``gl:callLists'' calls. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCallLists.xml">external</a> documentation.
-spec callLists(Lists) -> 'ok' when Lists :: [integer()].
callLists(Lists) ->
  cast(5108, <<(length(Lists)):?GLuint,
        (<< <<C:?GLuint>> || C <- Lists>>)/binary,0:(((1+length(Lists)) rem 2)*32)>>).

%% @doc set the display-list base for 
%%
%%  {@link gl:callLists/1} 
%%
%%  {@link gl:callLists/1}  specifies an array of offsets. Display-list names are generated
%% by adding  `Base'  to each offset. Names that reference valid display lists are executed;
%% the others are ignored. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glListBase.xml">external</a> documentation.
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
%% `?GL_POINTS':  Treats each vertex as a single point. Vertex   n defines point   n. 
%% N points are drawn. 
%%
%% `?GL_LINES':  Treats each pair of vertices as an independent line segment. Vertices
%%   2  n-1 and   2  n define line   n.  N/2 lines are drawn. 
%%
%% `?GL_LINE_STRIP':  Draws a connected group of line segments from the first vertex
%% to the last. Vertices   n and   n+1 define line   n.  N-1 lines are drawn. 
%%
%% `?GL_LINE_LOOP':  Draws a connected group of line segments from the first vertex
%% to the last, then back to the first. Vertices   n and   n+1 define line   n. The last
%% line, however, is defined by vertices   N and   1.  N lines are drawn. 
%%
%% `?GL_TRIANGLES':  Treats each triplet of vertices as an independent triangle. Vertices
%%   3  n-2,  3  n-1, and   3  n define triangle   n.  N/3 triangles are drawn. 
%%
%% `?GL_TRIANGLE_STRIP':  Draws a connected group of triangles. One triangle is defined
%% for each vertex presented after the first two vertices. For odd   n, vertices  n,  n+1,
%% and   n+2 define triangle   n. For even   n, vertices  n+1,  n, and   n+2 define triangle
%%   n.  N-2 triangles are drawn. 
%%
%% `?GL_TRIANGLE_FAN':  Draws a connected group of triangles. One triangle is defined
%% for each vertex presented after the first two vertices. Vertices   1,  n+1, and   n+2
%% define triangle   n.  N-2 triangles are drawn. 
%%
%% `?GL_QUADS':  Treats each group of four vertices as an independent quadrilateral.
%% Vertices   4  n-3,  4  n-2,  4  n-1, and   4  n define quadrilateral   n.  N/4 quadrilaterals
%% are drawn. 
%%
%% `?GL_QUAD_STRIP':  Draws a connected group of quadrilaterals. One quadrilateral is
%% defined for each pair of vertices presented after the first pair. Vertices   2  n-1,  2 
%% n,  2  n+2, and   2  n+1 define quadrilateral   n.  N/2-1 quadrilaterals are drawn. Note
%% that the order in which vertices are used to construct a quadrilateral from strip data
%% is different from that used with independent data. 
%%
%% `?GL_POLYGON':  Draws a single, convex polygon. Vertices   1 through   N define this
%% polygon. 
%%
%%  Only a subset of GL commands can be used between ``gl:'begin''' and  {@link gl:'begin'/1} .
%% The commands are  {@link gl:vertex2d/2} ,  {@link gl:color3b/3} ,  {@link gl:secondaryColor3b/3} ,  {@link gl:indexd/1} 
%% ,  {@link gl:normal3b/3} ,  {@link gl:fogCoordf/1} ,  {@link gl:texCoord1d/1} ,  {@link gl:multiTexCoord1d/2} 
%% ,  {@link gl:vertexAttrib1d/2} ,  {@link gl:evalCoord1d/1} ,  {@link gl:evalPoint1/1} ,  {@link gl:arrayElement/1} 
%% ,  {@link gl:materialf/3} , and  {@link gl:edgeFlag/1} . Also, it is acceptable to use  {@link gl:callList/1} 
%%  or  {@link gl:callLists/1}  to execute display lists that include only the preceding commands.
%% If any other GL command is executed between ``gl:'begin''' and  {@link gl:'begin'/1} , the error
%% flag is set and the command is ignored. 
%%
%%  Regardless of the value chosen for  `Mode' , there is no limit to the number of vertices
%% that can be defined between ``gl:'begin''' and  {@link gl:'begin'/1} . Lines, triangles, quadrilaterals,
%% and polygons that are incompletely specified are not drawn. Incomplete specification results
%% when either too few vertices are provided to specify even a single primitive or when an
%% incorrect multiple of vertices is specified. The incomplete primitive is ignored; the
%% rest are drawn. 
%%
%%  The minimum specification of vertices for each primitive is as follows: 1 for a point,
%% 2 for a line, 3 for a triangle, 4 for a quadrilateral, and 3 for a polygon. Modes that
%% require a certain multiple of vertices are `?GL_LINES' (2), `?GL_TRIANGLES'
%% (3), `?GL_QUADS' (4), and `?GL_QUAD_STRIP' (2). 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBegin.xml">external</a> documentation.
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
%%  When only   x and   y are specified,   z defaults to 0 and  w defaults to 1. When   x, 
%% y, and   z are specified,   w defaults to 1. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertex.xml">external</a> documentation.
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
%%  Normals specified with ``gl:normal'' need not have unit length. If `?GL_NORMALIZE'
%% is enabled, then normals of any length specified with ``gl:normal'' are normalized after
%% transformation. If `?GL_RESCALE_NORMAL' is enabled, normals are scaled by a scaling
%% factor derived from the modelview matrix. `?GL_RESCALE_NORMAL' requires that the
%% originally specified normals were of unit length, and that the modelview matrix contain
%% only uniform scales for proper results. To enable and disable normalization, call  {@link gl:enable/1} 
%%  and  {@link gl:enable/1}  with either `?GL_NORMALIZE' or `?GL_RESCALE_NORMAL'.
%% Normalization is initially disabled. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormal.xml">external</a> documentation.
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
%%  The current index is stored as a floating-point value. Integer values are converted directly
%% to floating-point values, with no special mapping. The initial value is 1. 
%%
%%  Index values outside the representable range of the color index buffer are not clamped.
%% However, before an index is dithered (if enabled) and written to the frame buffer, it
%% is converted to fixed-point format. Any bits in the integer portion of the resulting fixed-point
%% value that do not correspond to bits in the frame buffer are masked out. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndex.xml">external</a> documentation.
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
%% ``gl:color3b'', ``gl:color4b'', ``gl:color3s'', ``gl:color4s'', ``gl:color3i'',
%% and ``gl:color4i'' take three or four signed byte, short, or long integers as arguments.
%% When `v' is appended to the name, the color commands can take a pointer to an array
%% of such values. 
%%
%%  Current color values are stored in floating-point format, with unspecified mantissa and
%% exponent sizes. Unsigned integer color components, when specified, are linearly mapped
%% to floating-point values such that the largest representable value maps to 1.0 (full intensity),
%% and 0 maps to 0.0 (zero intensity). Signed integer color components, when specified, are
%% linearly mapped to floating-point values such that the most positive representable value
%% maps to 1.0, and the most negative representable value maps to   -1.0. (Note that this
%% mapping does not convert 0 precisely to 0.0.) Floating-point values are mapped directly. 
%%
%%  Neither floating-point nor signed integer values are clamped to the range  [0 1] before the
%% current color is updated. However, color components are clamped to this range before they
%% are interpolated or written into a color buffer. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColor.xml">external</a> documentation.
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
%%  The current texture coordinates are part of the data that is associated with each vertex
%% and with the current raster position. Initially, the values for `s', `t', `r'
%% , and `q' are (0, 0, 0, 1). 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoord.xml">external</a> documentation.
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
%%  The current raster position consists of three window coordinates ( x,  y,  z), a clip
%% coordinate value  ( w), an eye coordinate distance, a valid bit, and associated color
%% data and texture coordinates. The   w coordinate is a clip coordinate, because   w is
%% not projected to window coordinates. ``gl:rasterPos4'' specifies object coordinates   x,
%%  y,  z, and   w explicitly. ``gl:rasterPos3'' specifies object coordinate   x,  y, and
%%   z explicitly, while   w is implicitly set to 1. ``gl:rasterPos2'' uses the argument
%% values for   x and   y while implicitly setting   z and   w to 0 and 1. 
%%
%%  The object coordinates presented by ``gl:rasterPos'' are treated just like those of a  {@link gl:vertex2d/2} 
%%  command: They are transformed by the current modelview and projection matrices and passed
%% to the clipping stage. If the vertex is not culled, then it is projected and scaled to
%% window coordinates, which become the new current raster position, and the `?GL_CURRENT_RASTER_POSITION_VALID'
%%  flag is set. If the vertex `is' culled, then the valid bit is cleared and the current
%% raster position and associated color and texture coordinates are undefined. 
%%
%%  The current raster position also includes some associated color data and texture coordinates.
%% If lighting is enabled, then `?GL_CURRENT_RASTER_COLOR' (in RGBA mode) or `?GL_CURRENT_RASTER_INDEX'
%%  (in color index mode) is set to the color produced by the lighting calculation (see  {@link gl:lightf/3} 
%% ,  {@link gl:lightModelf/2} , and  {@link gl:shadeModel/1} ). If lighting is disabled, current
%% color (in RGBA mode, state variable `?GL_CURRENT_COLOR') or color index (in color
%% index mode, state variable `?GL_CURRENT_INDEX') is used to update the current raster
%% color. `?GL_CURRENT_RASTER_SECONDARY_COLOR' (in RGBA mode) is likewise updated. 
%%
%%  Likewise, `?GL_CURRENT_RASTER_TEXTURE_COORDS' is updated as a function of `?GL_CURRENT_TEXTURE_COORDS'
%% , based on the texture matrix and the texture generation functions (see  {@link gl:texGend/3} ).
%% Finally, the distance from the origin of the eye coordinate system to the vertex as transformed
%% by only the modelview matrix replaces `?GL_CURRENT_RASTER_DISTANCE'. 
%%
%%  Initially, the current raster position is (0, 0, 0, 1), the current raster distance is
%% 0, the valid bit is set, the associated RGBA color is (1, 1, 1, 1), the associated color
%% index is 1, and the associated texture coordinates are (0, 0, 0, 1). In RGBA mode, `?GL_CURRENT_RASTER_INDEX'
%%  is always 1; in color index mode, the current raster RGBA color always maintains its
%% initial value. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRasterPos.xml">external</a> documentation.
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
%% ``gl:rect''( `X1' ,  `Y1' ,  `X2' ,  `Y2' ) is exactly equivalent to the
%% following sequence:  glBegin(`?GL_POLYGON'); glVertex2( `X1' ,  `Y1' ); glVertex2(
%%  `X2' ,  `Y1' ); glVertex2( `X2' ,  `Y2' ); glVertex2( `X1' ,  `Y2' );
%% glEnd();  Note that if the second vertex is above and to the right of the first vertex,
%% the rectangle is constructed with a counterclockwise winding. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRect.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a vertex array is specified,  `Pointer'  is treated as a byte offset into the
%% buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as vertex array client-side state (`?GL_VERTEX_ARRAY_BUFFER_BINDING'). 
%%
%%  When a vertex array is specified,  `Size' ,  `Type' ,  `Stride' , and  `Pointer' 
%%  are saved as client-side state, in addition to the current vertex array buffer object
%% binding. 
%%
%%  To enable and disable the vertex array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_VERTEX_ARRAY'. If enabled, the vertex array is used when  {@link gl:arrayElement/1} 
%% ,  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements'
%% , or  {@link gl:drawRangeElements/6}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexPointer.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a normal array is specified,  `Pointer'  is treated as a byte offset into the
%% buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as normal vertex array client-side state (`?GL_NORMAL_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When a normal array is specified,  `Type' ,  `Stride' , and  `Pointer'  are
%% saved as client-side state, in addition to the current vertex array buffer object binding.
%% 
%%
%%  To enable and disable the normal array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_NORMAL_ARRAY'. If enabled, the normal array is used when  {@link gl:drawArrays/3} 
%% ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements',  {@link gl:drawRangeElements/6} 
%% , or  {@link gl:arrayElement/1}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNormalPointer.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a color array is specified,  `Pointer'  is treated as a byte offset into the
%% buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as color vertex array client-side state (`?GL_COLOR_ARRAY_BUFFER_BINDING').
%% 
%%
%%  When a color array is specified,  `Size' ,  `Type' ,  `Stride' , and  `Pointer' 
%%  are saved as client-side state, in addition to the current vertex array buffer object
%% binding. 
%%
%%  To enable and disable the color array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_COLOR_ARRAY'. If enabled, the color array is used when  {@link gl:drawArrays/3} 
%% ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements',  {@link gl:drawRangeElements/6} 
%% , or  {@link gl:arrayElement/1}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorPointer.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a color index array is specified,  `Pointer'  is treated as a byte offset into
%% the buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as color index vertex array client-side state (`?GL_INDEX_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When a color index array is specified,  `Type' ,  `Stride' , and  `Pointer' 
%% are saved as client-side state, in addition to the current vertex array buffer object
%% binding. 
%%
%%  To enable and disable the color index array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_INDEX_ARRAY'. If enabled, the color index array is used when
%%  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements'
%% ,  {@link gl:drawRangeElements/6} , or  {@link gl:arrayElement/1}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIndexPointer.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a texture coordinate array is specified,  `Pointer'  is treated as a byte offset
%% into the buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as texture coordinate vertex array client-side state (`?GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When a texture coordinate array is specified,  `Size' ,  `Type' ,  `Stride' ,
%% and  `Pointer'  are saved as client-side state, in addition to the current vertex array
%% buffer object binding. 
%%
%%  To enable and disable a texture coordinate array, call  {@link gl:enableClientState/1} 
%% and  {@link gl:enableClientState/1}  with the argument `?GL_TEXTURE_COORD_ARRAY'. If
%% enabled, the texture coordinate array is used when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} 
%% ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements',
%% or  {@link gl:drawRangeElements/6}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexCoordPointer.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while an edge flag array is specified,  `Pointer'  is treated as a byte offset into
%% the buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as edge flag vertex array client-side state (`?GL_EDGE_FLAG_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When an edge flag array is specified,  `Stride'  and  `Pointer'  are saved as client-side
%% state, in addition to the current vertex array buffer object binding. 
%%
%%  To enable and disable the edge flag array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_EDGE_FLAG_ARRAY'. If enabled, the edge flag array is used
%% when  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements'
%% ,  {@link gl:drawRangeElements/6} , or  {@link gl:arrayElement/1}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEdgeFlagPointer.xml">external</a> documentation.
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
%%  Use ``gl:arrayElement'' to construct primitives by indexing vertex data, rather than
%% by streaming through arrays of data in first-to-last order. Because each call specifies
%% only a single vertex, it is possible to explicitly specify per-primitive attributes such
%% as a single normal for each triangle. 
%%
%%  Changes made to array data between the execution of  {@link gl:'begin'/1}  and the corresponding
%% execution of  {@link gl:'begin'/1}  may affect calls to ``gl:arrayElement'' that are made within
%% the same  {@link gl:'begin'/1} / {@link gl:'begin'/1}  period in nonsequential ways. That is, a call
%% to ``gl:arrayElement'' that precedes a change to array data may access the changed data,
%% and a call that follows a change to array data may access original data. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glArrayElement.xml">external</a> documentation.
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
%%  When ``gl:drawArrays'' is called, it uses  `Count'  sequential elements from each
%% enabled array to construct a sequence of geometric primitives, beginning with element  `First' 
%% .  `Mode'  specifies what kind of primitives are constructed and how the array elements
%% construct those primitives. 
%%
%%  Vertex attributes that are modified by ``gl:drawArrays'' have an unspecified value
%% after ``gl:drawArrays'' returns. Attributes that aren't modified remain well defined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArrays.xml">external</a> documentation.
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
%%  When ``gl:drawElements'' is called, it uses  `Count'  sequential elements from an
%% enabled array, starting at  `Indices'  to construct a sequence of geometric primitives.
%%  `Mode'  specifies what kind of primitives are constructed and how the array elements
%% construct these primitives. If more than one array is enabled, each is used. 
%%
%%  Vertex attributes that are modified by ``gl:drawElements'' have an unspecified value
%% after ``gl:drawElements'' returns. Attributes that aren't modified maintain their previous
%% values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElements.xml">external</a> documentation.
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
%%  If  `Stride'  is 0, the aggregate elements are stored consecutively. Otherwise,  `Stride' 
%%  bytes occur between the beginning of one aggregate array element and the beginning of
%% the next aggregate array element. 
%%
%%  `Format'  serves as a ``key'' describing the extraction of individual arrays from
%% the aggregate array. If  `Format'  contains a T, then texture coordinates are extracted
%% from the interleaved array. If C is present, color values are extracted. If N is present,
%% normal coordinates are extracted. Vertex coordinates are always extracted. 
%%
%%  The digits 2, 3, and 4 denote how many values are extracted. F indicates that values
%% are extracted as floating-point values. Colors may also be extracted as 4 unsigned bytes
%% if 4UB follows the C. If a color is extracted as 4 unsigned bytes, the vertex array element
%% which follows is located at the first possible floating-point aligned address. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInterleavedArrays.xml">external</a> documentation.
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
%%  Flat and smooth shading are indistinguishable for points. Starting when  {@link gl:'begin'/1} 
%% is issued and counting vertices and primitives from 1, the GL gives each flat-shaded line
%% segment   i the computed color of vertex   i+1, its second vertex. Counting similarly
%% from 1, the GL gives each flat-shaded polygon the computed color of the vertex listed
%% in the following table. This is the last vertex to specify the polygon in all cases except
%% single polygons, where the first vertex specifies the flat-shaded color. 
%%
%% <table><tbody><tr><td>` Primitive Type of Polygon  ' i</td><td>` Vertex '</td></tr>
%% </tbody><tbody><tr><td> Single polygon  ( i== 1) </td><td> 1 </td></tr><tr><td> Triangle
%% strip </td><td> i+2</td></tr><tr><td> Triangle fan </td><td> i+2</td></tr><tr><td> Independent
%% triangle </td><td> 3  i</td></tr><tr><td> Quad strip </td><td> 2  i+2</td></tr><tr><td>
%% Independent quad  </td><td> 4  i</td></tr></tbody></table>
%%
%%  Flat and smooth shading are specified by ``gl:shadeModel'' with  `Mode'  set to `?GL_FLAT'
%%  and `?GL_SMOOTH', respectively. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShadeModel.xml">external</a> documentation.
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
%%  To enable and disable lighting calculation, call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%%  with argument `?GL_LIGHTING'. Lighting is initially disabled. When it is enabled,
%% light sources that are enabled contribute to the lighting calculation. Light source   i
%% is enabled and disabled using  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_LIGHT'
%%  i. 
%%
%%  The ten light parameters are as follows: 
%%
%% `?GL_AMBIENT':  `Params'  contains four integer or floating-point values that
%% specify the ambient RGBA intensity of the light. Integer values are mapped linearly such
%% that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial ambient light intensity is (0, 0, 0, 1). 
%%
%% `?GL_DIFFUSE':  `Params'  contains four integer or floating-point values that
%% specify the diffuse RGBA intensity of the light. Integer values are mapped linearly such
%% that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial value for `?GL_LIGHT0' is (1, 1, 1, 1); for other
%% lights, the initial value is (0, 0, 0, 1). 
%%
%% `?GL_SPECULAR':  `Params'  contains four integer or floating-point values that
%% specify the specular RGBA intensity of the light. Integer values are mapped linearly such
%% that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial value for `?GL_LIGHT0' is (1, 1, 1, 1); for other
%% lights, the initial value is (0, 0, 0, 1). 
%%
%% `?GL_POSITION':  `Params'  contains four integer or floating-point values that
%% specify the position of the light in homogeneous object coordinates. Both integer and
%% floating-point values are mapped directly. Neither integer nor floating-point values are
%% clamped. 
%%
%%  The position is transformed by the modelview matrix when ``gl:light'' is called (just
%% as if it were a point), and it is stored in eye coordinates. If the   w component of the
%% position is 0, the light is treated as a directional source. Diffuse and specular lighting
%% calculations take the light's direction, but not its actual position, into account, and
%% attenuation is disabled. Otherwise, diffuse and specular lighting calculations are based
%% on the actual location of the light in eye coordinates, and attenuation is enabled. The
%% initial position is (0, 0, 1, 0); thus, the initial light source is directional, parallel
%% to, and in the direction of the  -z axis. 
%%
%% `?GL_SPOT_DIRECTION':  `Params'  contains three integer or floating-point values
%% that specify the direction of the light in homogeneous object coordinates. Both integer
%% and floating-point values are mapped directly. Neither integer nor floating-point values
%% are clamped. 
%%
%%  The spot direction is transformed by the upper 3x3 of the modelview matrix when ``gl:light''
%%  is called, and it is stored in eye coordinates. It is significant only when `?GL_SPOT_CUTOFF'
%%  is not 180, which it is initially. The initial direction is  (0 0 -1). 
%%
%% `?GL_SPOT_EXPONENT':  `Params'  is a single integer or floating-point value that
%% specifies the intensity distribution of the light. Integer and floating-point values are
%% mapped directly. Only values in the range  [0 128] are accepted. 
%%
%%  Effective light intensity is attenuated by the cosine of the angle between the direction
%% of the light and the direction from the light to the vertex being lighted, raised to the
%% power of the spot exponent. Thus, higher spot exponents result in a more focused light
%% source, regardless of the spot cutoff angle (see `?GL_SPOT_CUTOFF', next paragraph).
%% The initial spot exponent is 0, resulting in uniform light distribution. 
%%
%% `?GL_SPOT_CUTOFF':  `Params'  is a single integer or floating-point value that
%% specifies the maximum spread angle of a light source. Integer and floating-point values
%% are mapped directly. Only values in the range  [0 90] and the special value 180 are accepted.
%% If the angle between the direction of the light and the direction from the light to the
%% vertex being lighted is greater than the spot cutoff angle, the light is completely masked.
%% Otherwise, its intensity is controlled by the spot exponent and the attenuation factors.
%% The initial spot cutoff is 180, resulting in uniform light distribution. 
%%
%% `?GL_CONSTANT_ATTENUATION'
%%
%% `?GL_LINEAR_ATTENUATION'
%%
%% `?GL_QUADRATIC_ATTENUATION':  `Params'  is a single integer or floating-point
%% value that specifies one of the three light attenuation factors. Integer and floating-point
%% values are mapped directly. Only nonnegative values are accepted. If the light is positional,
%% rather than directional, its intensity is attenuated by the reciprocal of the sum of the
%% constant factor, the linear factor times the distance between the light and the vertex
%% being lighted, and the quadratic factor times the square of the same distance. The initial
%% attenuation factors are (1, 0, 0), resulting in no attenuation. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLight.xml">external</a> documentation.
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
%%  The following parameters are defined: 
%%
%% `?GL_AMBIENT':  `Params'  returns four integer or floating-point values representing
%% the ambient intensity of the light source. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value is (0, 0, 0, 1). 
%%
%% `?GL_DIFFUSE':  `Params'  returns four integer or floating-point values representing
%% the diffuse intensity of the light source. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value for `?GL_LIGHT0' is (1, 1, 1, 1); for
%% other lights, the initial value is (0, 0, 0, 0). 
%%
%% `?GL_SPECULAR':  `Params'  returns four integer or floating-point values representing
%% the specular intensity of the light source. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value for `?GL_LIGHT0' is (1, 1, 1, 1); for
%% other lights, the initial value is (0, 0, 0, 0). 
%%
%% `?GL_POSITION':  `Params'  returns four integer or floating-point values representing
%% the position of the light source. Integer values, when requested, are computed by rounding
%% the internal floating-point values to the nearest integer value. The returned values are
%% those maintained in eye coordinates. They will not be equal to the values specified using
%%  {@link gl:lightf/3} , unless the modelview matrix was identity at the time  {@link gl:lightf/3} 
%% was called. The initial value is (0, 0, 1, 0). 
%%
%% `?GL_SPOT_DIRECTION':  `Params'  returns three integer or floating-point values
%% representing the direction of the light source. Integer values, when requested, are computed
%% by rounding the internal floating-point values to the nearest integer value. The returned
%% values are those maintained in eye coordinates. They will not be equal to the values specified
%% using  {@link gl:lightf/3} , unless the modelview matrix was identity at the time  {@link gl:lightf/3} 
%%  was called. Although spot direction is normalized before being used in the lighting equation,
%% the returned values are the transformed versions of the specified values prior to normalization.
%% The initial value is  (0 0 -1). 
%%
%% `?GL_SPOT_EXPONENT':  `Params'  returns a single integer or floating-point value
%% representing the spot exponent of the light. An integer value, when requested, is computed
%% by rounding the internal floating-point representation to the nearest integer. The initial
%% value is 0. 
%%
%% `?GL_SPOT_CUTOFF':  `Params'  returns a single integer or floating-point value
%% representing the spot cutoff angle of the light. An integer value, when requested, is
%% computed by rounding the internal floating-point representation to the nearest integer.
%% The initial value is 180. 
%%
%% `?GL_CONSTANT_ATTENUATION':  `Params'  returns a single integer or floating-point
%% value representing the constant (not distance-related) attenuation of the light. An integer
%% value, when requested, is computed by rounding the internal floating-point representation
%% to the nearest integer. The initial value is 1. 
%%
%% `?GL_LINEAR_ATTENUATION':  `Params'  returns a single integer or floating-point
%% value representing the linear attenuation of the light. An integer value, when requested,
%% is computed by rounding the internal floating-point representation to the nearest integer.
%% The initial value is 0. 
%%
%% `?GL_QUADRATIC_ATTENUATION':  `Params'  returns a single integer or floating-point
%% value representing the quadratic attenuation of the light. An integer value, when requested,
%% is computed by rounding the internal floating-point representation to the nearest integer.
%% The initial value is 0. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetLight.xml">external</a> documentation.
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
%% `?GL_LIGHT_MODEL_AMBIENT':  `Params'  contains four integer or floating-point
%% values that specify the ambient RGBA intensity of the entire scene. Integer values are
%% mapped linearly such that the most positive representable value maps to 1.0, and the most
%% negative representable value maps to   -1.0. Floating-point values are mapped directly.
%% Neither integer nor floating-point values are clamped. The initial ambient scene intensity
%% is (0.2, 0.2, 0.2, 1.0). 
%%
%% `?GL_LIGHT_MODEL_COLOR_CONTROL':  `Params'  must be either `?GL_SEPARATE_SPECULAR_COLOR'
%%  or `?GL_SINGLE_COLOR'. `?GL_SINGLE_COLOR' specifies that a single color is
%% generated from the lighting computation for a vertex. `?GL_SEPARATE_SPECULAR_COLOR'
%% specifies that the specular color computation of lighting be stored separately from the
%% remainder of the lighting computation. The specular color is summed into the generated
%% fragment's color after the application of texture mapping (if enabled). The initial value
%% is `?GL_SINGLE_COLOR'. 
%%
%% `?GL_LIGHT_MODEL_LOCAL_VIEWER':  `Params'  is a single integer or floating-point
%% value that specifies how specular reflection angles are computed. If  `Params'  is
%% 0 (or 0.0), specular reflection angles take the view direction to be parallel to and in
%% the direction of the -`z' axis, regardless of the location of the vertex in eye coordinates.
%% Otherwise, specular reflections are computed from the origin of the eye coordinate system.
%% The initial value is 0. 
%%
%% `?GL_LIGHT_MODEL_TWO_SIDE':  `Params'  is a single integer or floating-point value
%% that specifies whether one- or two-sided lighting calculations are done for polygons.
%% It has no effect on the lighting calculations for points, lines, or bitmaps. If  `Params' 
%%  is 0 (or 0.0), one-sided lighting is specified, and only the `front' material parameters
%% are used in the lighting equation. Otherwise, two-sided lighting is specified. In this
%% case, vertices of back-facing polygons are lighted using the `back' material parameters
%% and have their normals reversed before the lighting equation is evaluated. Vertices of
%% front-facing polygons are always lighted using the `front' material parameters, with
%% no change to their normals. The initial value is 0. 
%%
%%  In RGBA mode, the lighted color of a vertex is the sum of the material emission intensity,
%% the product of the material ambient reflectance and the lighting model full-scene ambient
%% intensity, and the contribution of each enabled light source. Each light source contributes
%% the sum of three terms: ambient, diffuse, and specular. The ambient light source contribution
%% is the product of the material ambient reflectance and the light's ambient intensity.
%% The diffuse light source contribution is the product of the material diffuse reflectance,
%% the light's diffuse intensity, and the dot product of the vertex's normal with the normalized
%% vector from the vertex to the light source. The specular light source contribution is
%% the product of the material specular reflectance, the light's specular intensity, and
%% the dot product of the normalized vertex-to-eye and vertex-to-light vectors, raised to
%% the power of the shininess of the material. All three light source contributions are attenuated
%% equally based on the distance from the vertex to the light source and on light source
%% direction, spread exponent, and spread cutoff angle. All dot products are replaced with
%% 0 if they evaluate to a negative value. 
%%
%%  The alpha component of the resulting lighted color is set to the alpha value of the material
%% diffuse reflectance. 
%%
%%  In color index mode, the value of the lighted index of a vertex ranges from the ambient
%% to the specular values passed to  {@link gl:materialf/3}  using `?GL_COLOR_INDEXES'.
%% Diffuse and specular coefficients, computed with a (.30, .59, .11) weighting of the lights'
%% colors, the shininess of the material, and the same reflection and attenuation equations
%% as in the RGBA case, determine how much above ambient the resulting index is. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLightModel.xml">external</a> documentation.
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
%% ``gl:material'' takes three arguments. The first,  `Face' , specifies whether the `?GL_FRONT'
%%  materials, the `?GL_BACK' materials, or both `?GL_FRONT_AND_BACK' materials
%% will be modified. The second,  `Pname' , specifies which of several parameters in one
%% or both sets will be modified. The third,  `Params' , specifies what value or values
%% will be assigned to the specified parameter. 
%%
%%  Material parameters are used in the lighting equation that is optionally applied to each
%% vertex. The equation is discussed in the  {@link gl:lightModelf/2}  reference page. The parameters
%% that can be specified using ``gl:material'', and their interpretations by the lighting
%% equation, are as follows: 
%%
%% `?GL_AMBIENT':  `Params'  contains four integer or floating-point values that
%% specify the ambient RGBA reflectance of the material. Integer values are mapped linearly
%% such that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial ambient reflectance for both front- and back-facing materials
%% is (0.2, 0.2, 0.2, 1.0). 
%%
%% `?GL_DIFFUSE':  `Params'  contains four integer or floating-point values that
%% specify the diffuse RGBA reflectance of the material. Integer values are mapped linearly
%% such that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial diffuse reflectance for both front- and back-facing materials
%% is (0.8, 0.8, 0.8, 1.0). 
%%
%% `?GL_SPECULAR':  `Params'  contains four integer or floating-point values that
%% specify the specular RGBA reflectance of the material. Integer values are mapped linearly
%% such that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial specular reflectance for both front- and back-facing materials
%% is (0, 0, 0, 1). 
%%
%% `?GL_EMISSION':  `Params'  contains four integer or floating-point values that
%% specify the RGBA emitted light intensity of the material. Integer values are mapped linearly
%% such that the most positive representable value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Floating-point values are mapped directly. Neither integer nor floating-point
%% values are clamped. The initial emission intensity for both front- and back-facing materials
%% is (0, 0, 0, 1). 
%%
%% `?GL_SHININESS':  `Params'  is a single integer or floating-point value that specifies
%% the RGBA specular exponent of the material. Integer and floating-point values are mapped
%% directly. Only values in the range  [0 128] are accepted. The initial specular exponent for both
%% front- and back-facing materials is 0. 
%%
%% `?GL_AMBIENT_AND_DIFFUSE':  Equivalent to calling ``gl:material'' twice with the
%% same parameter values, once with `?GL_AMBIENT' and once with `?GL_DIFFUSE'. 
%%
%% `?GL_COLOR_INDEXES':  `Params'  contains three integer or floating-point values
%% specifying the color indices for ambient, diffuse, and specular lighting. These three
%% values, and `?GL_SHININESS', are the only material values used by the color index
%% mode lighting equation. Refer to the  {@link gl:lightModelf/2}  reference page for a discussion
%% of color index lighting. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMaterial.xml">external</a> documentation.
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
%% `?GL_AMBIENT':  `Params'  returns four integer or floating-point values representing
%% the ambient reflectance of the material. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value is (0.2, 0.2, 0.2, 1.0) 
%%
%% `?GL_DIFFUSE':  `Params'  returns four integer or floating-point values representing
%% the diffuse reflectance of the material. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value is (0.8, 0.8, 0.8, 1.0). 
%%
%% `?GL_SPECULAR':  `Params'  returns four integer or floating-point values representing
%% the specular reflectance of the material. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value is (0, 0, 0, 1). 
%%
%% `?GL_EMISSION':  `Params'  returns four integer or floating-point values representing
%% the emitted light intensity of the material. Integer values, when requested, are linearly
%% mapped from the internal floating-point representation such that 1.0 maps to the most
%% positive representable integer value, and   -1.0 maps to the most negative representable
%% integer value. If the internal value is outside the range  [-1 1], the corresponding integer
%% return value is undefined. The initial value is (0, 0, 0, 1). 
%%
%% `?GL_SHININESS':  `Params'  returns one integer or floating-point value representing
%% the specular exponent of the material. Integer values, when requested, are computed by
%% rounding the internal floating-point value to the nearest integer value. The initial value
%% is 0. 
%%
%% `?GL_COLOR_INDEXES':  `Params'  returns three integer or floating-point values
%% representing the ambient, diffuse, and specular indices of the material. These indices
%% are used only for color index lighting. (All the other parameters are used only for RGBA
%% lighting.) Integer values, when requested, are computed by rounding the internal floating-point
%% values to the nearest integer values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMaterial.xml">external</a> documentation.
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
%%  To enable and disable `?GL_COLOR_MATERIAL', call  {@link gl:enable/1}  and  {@link gl:enable/1} 
%%  with argument `?GL_COLOR_MATERIAL'. `?GL_COLOR_MATERIAL' is initially disabled.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaterial.xml">external</a> documentation.
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
%%  ( xr+n. xfactor,   yr+m. yfactor) 
%%
%%  ( xr+(n+1). xfactor,   yr+(m+1). yfactor) 
%%
%%  are candidates for replacement. Any pixel whose center lies on the bottom or left edge
%% of this rectangular region is also modified. 
%%
%%  Pixel zoom factors are not limited to positive values. Negative zoom factors reflect
%% the resulting image about the current raster position. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelZoom.xml">external</a> documentation.
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
%%  `Pname'  is a symbolic constant indicating the parameter to be set, and  `Param' 
%% is the new value. Six of the twelve storage parameters affect how pixel data is returned
%% to client memory. They are as follows: 
%%
%% `?GL_PACK_SWAP_BYTES':  If true, byte ordering for multibyte color components, depth
%% components, or stencil indices is reversed. That is, if a four-byte component consists
%% of bytes  b 0,  b 1,  b 2,  b 3, it is stored in memory as  b 3,  b 2,  b 1,  b 0 if `?GL_PACK_SWAP_BYTES'
%%  is true. `?GL_PACK_SWAP_BYTES' has no effect on the memory order of components within
%% a pixel, only on the order of bytes within components or indices. For example, the three
%% components of a `?GL_RGB' format pixel are always stored with red first, green second,
%% and blue third, regardless of the value of `?GL_PACK_SWAP_BYTES'. 
%%
%% `?GL_PACK_LSB_FIRST':  If true, bits are ordered within a byte from least significant
%% to most significant; otherwise, the first bit in each byte is the most significant one. 
%%
%% `?GL_PACK_ROW_LENGTH':  If greater than 0, `?GL_PACK_ROW_LENGTH' defines the
%% number of pixels in a row. If the first pixel of a row is placed at location   p in memory,
%% then the location of the first pixel of the next row is obtained by skipping 
%%
%%  k={n  l(a/s) |(s  n  l)/a|  s&gt;= a s&lt; a)
%%
%%  components or indices, where   n is the number of components or indices in a pixel,  l
%% is the number of pixels in a row (`?GL_PACK_ROW_LENGTH' if it is greater than 0,
%% the   width argument to the pixel routine otherwise),  a is the value of `?GL_PACK_ALIGNMENT'
%% , and  s is the size, in bytes, of a single component (if   a&lt; s, then it is as if   a=
%% s). In the case of 1-bit values, the location of the next row is obtained by skipping 
%%
%%  k=8  a |(n  l)/(8  a)|
%%
%%  components or indices. 
%%
%%  The word `component' in this description refers to the nonindex values red, green,
%% blue, alpha, and depth. Storage format `?GL_RGB', for example, has three components
%% per pixel: first red, then green, and finally blue. 
%%
%% `?GL_PACK_IMAGE_HEIGHT':  If greater than 0, `?GL_PACK_IMAGE_HEIGHT' defines
%% the number of pixels in an image three-dimensional texture volume, where ``image'' is
%% defined by all pixels sharing the same third dimension index. If the first pixel of a
%% row is placed at location   p in memory, then the location of the first pixel of the next
%% row is obtained by skipping 
%%
%%  k={n  l  h(a/s) |(s  n  l  h)/a|  s&gt;= a s&lt; a)
%%
%%  components or indices, where   n is the number of components or indices in a pixel,   l
%% is the number of pixels in a row (`?GL_PACK_ROW_LENGTH' if it is greater than 0,
%% the  width argument to  {@link gl:texImage3D/10}  otherwise),   h is the number of rows in
%% a pixel image (`?GL_PACK_IMAGE_HEIGHT' if it is greater than 0, the   height argument
%% to the  {@link gl:texImage3D/10}  routine otherwise),  a is the value of `?GL_PACK_ALIGNMENT'
%% , and   s is the size, in bytes, of a single component (if   a&lt; s, then it is as if  
%% a=s). 
%%
%%  The word `component' in this description refers to the nonindex values red, green,
%% blue, alpha, and depth. Storage format `?GL_RGB', for example, has three components
%% per pixel: first red, then green, and finally blue. 
%%
%% `?GL_PACK_SKIP_PIXELS', `?GL_PACK_SKIP_ROWS', and `?GL_PACK_SKIP_IMAGES'
%%
%%  These values are provided as a convenience to the programmer; they provide no functionality
%% that cannot be duplicated simply by incrementing the pointer passed to  {@link gl:readPixels/7} 
%% . Setting `?GL_PACK_SKIP_PIXELS' to   i is equivalent to incrementing the pointer
%% by   i  n components or indices, where   n is the number of components or indices in each
%% pixel. Setting `?GL_PACK_SKIP_ROWS' to   j is equivalent to incrementing the pointer
%% by   j  m components or indices, where   m is the number of components or indices per
%% row, as just computed in the `?GL_PACK_ROW_LENGTH' section. Setting `?GL_PACK_SKIP_IMAGES'
%%  to   k is equivalent to incrementing the pointer by   k  p, where   p is the number of
%% components or indices per image, as computed in the `?GL_PACK_IMAGE_HEIGHT' section.
%% 
%%
%% `?GL_PACK_ALIGNMENT':  Specifies the alignment requirements for the start of each
%% pixel row in memory. The allowable values are 1 (byte-alignment), 2 (rows aligned to even-numbered
%% bytes), 4 (word-alignment), and 8 (rows start on double-word boundaries). 
%%
%%  The other six of the twelve storage parameters affect how pixel data is read from client
%% memory. These values are significant for  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} 
%% ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} , and  {@link gl:texSubImage1D/7} 
%%
%%  They are as follows: 
%%
%% `?GL_UNPACK_SWAP_BYTES':  If true, byte ordering for multibyte color components,
%% depth components, or stencil indices is reversed. That is, if a four-byte component consists
%% of bytes  b 0,  b 1,  b 2,  b 3, it is taken from memory as  b 3,  b 2,  b 1,  b 0 if `?GL_UNPACK_SWAP_BYTES'
%%  is true. `?GL_UNPACK_SWAP_BYTES' has no effect on the memory order of components
%% within a pixel, only on the order of bytes within components or indices. For example,
%% the three components of a `?GL_RGB' format pixel are always stored with red first,
%% green second, and blue third, regardless of the value of `?GL_UNPACK_SWAP_BYTES'. 
%%
%% `?GL_UNPACK_LSB_FIRST':  If true, bits are ordered within a byte from least significant
%% to most significant; otherwise, the first bit in each byte is the most significant one. 
%%
%% `?GL_UNPACK_ROW_LENGTH':  If greater than 0, `?GL_UNPACK_ROW_LENGTH' defines
%% the number of pixels in a row. If the first pixel of a row is placed at location   p in
%% memory, then the location of the first pixel of the next row is obtained by skipping 
%%
%%  k={n  l(a/s) |(s  n  l)/a|  s&gt;= a s&lt; a)
%%
%%  components or indices, where   n is the number of components or indices in a pixel,  l
%% is the number of pixels in a row (`?GL_UNPACK_ROW_LENGTH' if it is greater than 0,
%% the   width argument to the pixel routine otherwise),  a is the value of `?GL_UNPACK_ALIGNMENT'
%% , and  s is the size, in bytes, of a single component (if   a&lt; s, then it is as if   a=
%% s). In the case of 1-bit values, the location of the next row is obtained by skipping 
%%
%%  k=8  a |(n  l)/(8  a)|
%%
%%  components or indices. 
%%
%%  The word `component' in this description refers to the nonindex values red, green,
%% blue, alpha, and depth. Storage format `?GL_RGB', for example, has three components
%% per pixel: first red, then green, and finally blue. 
%%
%% `?GL_UNPACK_IMAGE_HEIGHT':  If greater than 0, `?GL_UNPACK_IMAGE_HEIGHT' defines
%% the number of pixels in an image of a three-dimensional texture volume. Where ``image''
%% is defined by all pixel sharing the same third dimension index. If the first pixel of
%% a row is placed at location   p in memory, then the location of the first pixel of the
%% next row is obtained by skipping 
%%
%%  k={n  l  h(a/s) |(s  n  l  h)/a|  s&gt;= a s&lt; a)
%%
%%  components or indices, where   n is the number of components or indices in a pixel,  l
%% is the number of pixels in a row (`?GL_UNPACK_ROW_LENGTH' if it is greater than 0,
%% the   width argument to  {@link gl:texImage3D/10}  otherwise),  h is the number of rows in
%% an image (`?GL_UNPACK_IMAGE_HEIGHT' if it is greater than 0, the   height argument
%% to  {@link gl:texImage3D/10}  otherwise),  a is the value of `?GL_UNPACK_ALIGNMENT',
%% and  s is the size, in bytes, of a single component (if   a&lt; s, then it is as if   a=s).
%% 
%%
%%  The word `component' in this description refers to the nonindex values red, green,
%% blue, alpha, and depth. Storage format `?GL_RGB', for example, has three components
%% per pixel: first red, then green, and finally blue. 
%%
%% `?GL_UNPACK_SKIP_PIXELS' and `?GL_UNPACK_SKIP_ROWS'
%%
%%  These values are provided as a convenience to the programmer; they provide no functionality
%% that cannot be duplicated by incrementing the pointer passed to  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} 
%% ,  {@link gl:texSubImage1D/7}  or  {@link gl:texSubImage1D/7} . Setting `?GL_UNPACK_SKIP_PIXELS'
%%  to   i is equivalent to incrementing the pointer by   i  n components or indices, where
%%   n is the number of components or indices in each pixel. Setting `?GL_UNPACK_SKIP_ROWS'
%%  to   j is equivalent to incrementing the pointer by   j  k components or indices, where
%%   k is the number of components or indices per row, as just computed in the `?GL_UNPACK_ROW_LENGTH'
%%  section. 
%%
%% `?GL_UNPACK_ALIGNMENT':  Specifies the alignment requirements for the start of each
%% pixel row in memory. The allowable values are 1 (byte-alignment), 2 (rows aligned to even-numbered
%% bytes), 4 (word-alignment), and 8 (rows start on double-word boundaries). 
%%
%%  The following table gives the type, initial value, and range of valid values for each
%% storage parameter that can be set with ``gl:pixelStore''. 
%%
%% <table><tbody><tr><td> `Pname' </td><td>` Type '</td><td>` Initial Value '</td>
%% <td>` Valid Range '</td></tr></tbody><tbody><tr><td>`?GL_PACK_SWAP_BYTES'</td><td>
%%  boolean </td><td> false </td><td> true or false </td></tr><tr><td>`?GL_PACK_LSB_FIRST'
%% </td><td> boolean </td><td> false </td><td> true or false </td></tr><tr><td>`?GL_PACK_ROW_LENGTH'
%% </td><td> integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_PACK_IMAGE_HEIGHT'</td>
%% <td> integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_PACK_SKIP_ROWS'</td><td>
%% integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_PACK_SKIP_PIXELS'</td><td> integer
%% </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_PACK_SKIP_IMAGES'</td><td> integer </td><td>
%%  0 </td><td>[0)</td></tr><tr><td>`?GL_PACK_ALIGNMENT'</td><td> integer </td><td> 4 </td>
%% <td> 1, 2, 4, or 8 </td></tr><tr><td>`?GL_UNPACK_SWAP_BYTES'</td><td> boolean </td><td>
%%  false </td><td> true or false </td></tr><tr><td>`?GL_UNPACK_LSB_FIRST'</td><td>
%% boolean </td><td> false </td><td> true or false </td></tr><tr><td>`?GL_UNPACK_ROW_LENGTH'
%% </td><td> integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_UNPACK_IMAGE_HEIGHT'</td>
%% <td> integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_UNPACK_SKIP_ROWS'</td><td>
%% integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_UNPACK_SKIP_PIXELS'</td><td>
%% integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_UNPACK_SKIP_IMAGES'</td><td>
%% integer </td><td> 0 </td><td>[0)</td></tr><tr><td>`?GL_UNPACK_ALIGNMENT'</td><td> integer
%% </td><td> 4 </td><td> 1, 2, 4, or 8 </td></tr></tbody></table>
%%
%% ``gl:pixelStoref'' can be used to set any pixel store parameter. If the parameter type
%% is boolean, then if  `Param'  is 0, the parameter is false; otherwise it is set to
%% true. If  `Pname'  is a integer type parameter,  `Param'  is rounded to the nearest
%% integer. 
%%
%%  Likewise, ``gl:pixelStorei'' can also be used to set any of the pixel store parameters.
%% Boolean parameters are set to false if  `Param'  is 0 and true otherwise. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelStore.xml">external</a> documentation.
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
%%  Pixel transfer operations handle four fundamental pixel types: `color', `color index'
%% , `depth', and `stencil'. `Color' pixels consist of four floating-point
%% values with unspecified mantissa and exponent sizes, scaled such that 0 represents zero
%% intensity and 1 represents full intensity. `Color indices' comprise a single fixed-point
%% value, with unspecified precision to the right of the binary point. `Depth' pixels
%% comprise a single floating-point value, with unspecified mantissa and exponent sizes,
%% scaled such that 0.0 represents the minimum depth buffer value, and 1.0 represents the
%% maximum depth buffer value. Finally, `stencil' pixels comprise a single fixed-point
%% value, with unspecified precision to the right of the binary point. 
%%
%%  The pixel transfer operations performed on the four basic pixel types are as follows: 
%%
%% `Color':  Each of the four color components is multiplied by a scale factor, then
%% added to a bias factor. That is, the red component is multiplied by `?GL_RED_SCALE',
%% then added to `?GL_RED_BIAS'; the green component is multiplied by `?GL_GREEN_SCALE'
%% , then added to `?GL_GREEN_BIAS'; the blue component is multiplied by `?GL_BLUE_SCALE'
%% , then added to `?GL_BLUE_BIAS'; and the alpha component is multiplied by `?GL_ALPHA_SCALE'
%% , then added to `?GL_ALPHA_BIAS'. After all four color components are scaled and
%% biased, each is clamped to the range  [0 1]. All color, scale, and bias values are specified
%% with ``gl:pixelTransfer''. 
%%
%%  If `?GL_MAP_COLOR' is true, each color component is scaled by the size of the corresponding
%% color-to-color map, then replaced by the contents of that map indexed by the scaled component.
%% That is, the red component is scaled by `?GL_PIXEL_MAP_R_TO_R_SIZE', then replaced
%% by the contents of `?GL_PIXEL_MAP_R_TO_R' indexed by itself. The green component
%% is scaled by `?GL_PIXEL_MAP_G_TO_G_SIZE', then replaced by the contents of `?GL_PIXEL_MAP_G_TO_G'
%%  indexed by itself. The blue component is scaled by `?GL_PIXEL_MAP_B_TO_B_SIZE',
%% then replaced by the contents of `?GL_PIXEL_MAP_B_TO_B' indexed by itself. And the
%% alpha component is scaled by `?GL_PIXEL_MAP_A_TO_A_SIZE', then replaced by the contents
%% of `?GL_PIXEL_MAP_A_TO_A' indexed by itself. All components taken from the maps are
%% then clamped to the range  [0 1]. `?GL_MAP_COLOR' is specified with ``gl:pixelTransfer''.
%% The contents of the various maps are specified with  {@link gl:pixelMapfv/3} . 
%%
%%  If the ARB_imaging extension is supported, each of the four color components may be scaled
%% and biased after transformation by the color matrix. That is, the red component is multiplied
%% by `?GL_POST_COLOR_MATRIX_RED_SCALE', then added to `?GL_POST_COLOR_MATRIX_RED_BIAS'
%% ; the green component is multiplied by `?GL_POST_COLOR_MATRIX_GREEN_SCALE', then
%% added to `?GL_POST_COLOR_MATRIX_GREEN_BIAS'; the blue component is multiplied by `?GL_POST_COLOR_MATRIX_BLUE_SCALE'
%% , then added to `?GL_POST_COLOR_MATRIX_BLUE_BIAS'; and the alpha component is multiplied
%% by `?GL_POST_COLOR_MATRIX_ALPHA_SCALE', then added to `?GL_POST_COLOR_MATRIX_ALPHA_BIAS'
%% . After all four color components are scaled and biased, each is clamped to the range  [0
%% 1]. 
%%
%%  Similarly, if the ARB_imaging extension is supported, each of the four color components
%% may be scaled and biased after processing by the enabled convolution filter. That is,
%% the red component is multiplied by `?GL_POST_CONVOLUTION_RED_SCALE', then added to `?GL_POST_CONVOLUTION_RED_BIAS'
%% ; the green component is multiplied by `?GL_POST_CONVOLUTION_GREEN_SCALE', then added
%% to `?GL_POST_CONVOLUTION_GREEN_BIAS'; the blue component is multiplied by `?GL_POST_CONVOLUTION_BLUE_SCALE'
%% , then added to `?GL_POST_CONVOLUTION_BLUE_BIAS'; and the alpha component is multiplied
%% by `?GL_POST_CONVOLUTION_ALPHA_SCALE', then added to `?GL_POST_CONVOLUTION_ALPHA_BIAS'
%% . After all four color components are scaled and biased, each is clamped to the range  [0
%% 1]. 
%%
%% `Color index':  Each color index is shifted left by `?GL_INDEX_SHIFT' bits;
%% any bits beyond the number of fraction bits carried by the fixed-point index are filled
%% with zeros. If `?GL_INDEX_SHIFT' is negative, the shift is to the right, again zero
%% filled. Then `?GL_INDEX_OFFSET' is added to the index. `?GL_INDEX_SHIFT' and `?GL_INDEX_OFFSET'
%%  are specified with ``gl:pixelTransfer''. 
%%
%%  From this point, operation diverges depending on the required format of the resulting
%% pixels. If the resulting pixels are to be written to a color index buffer, or if they
%% are being read back to client memory in `?GL_COLOR_INDEX' format, the pixels continue
%% to be treated as indices. If `?GL_MAP_COLOR' is true, each index is masked by   2 n-1
%% , where   n is `?GL_PIXEL_MAP_I_TO_I_SIZE', then replaced by the contents of `?GL_PIXEL_MAP_I_TO_I'
%%  indexed by the masked value. `?GL_MAP_COLOR' is specified with ``gl:pixelTransfer''
%% . The contents of the index map is specified with  {@link gl:pixelMapfv/3} . 
%%
%%  If the resulting pixels are to be written to an RGBA color buffer, or if they are read
%% back to client memory in a format other than `?GL_COLOR_INDEX', the pixels are converted
%% from indices to colors by referencing the four maps `?GL_PIXEL_MAP_I_TO_R', `?GL_PIXEL_MAP_I_TO_G'
%% , `?GL_PIXEL_MAP_I_TO_B', and `?GL_PIXEL_MAP_I_TO_A'. Before being dereferenced,
%% the index is masked by   2 n-1, where   n is `?GL_PIXEL_MAP_I_TO_R_SIZE' for the
%% red map, `?GL_PIXEL_MAP_I_TO_G_SIZE' for the green map, `?GL_PIXEL_MAP_I_TO_B_SIZE'
%%  for the blue map, and `?GL_PIXEL_MAP_I_TO_A_SIZE' for the alpha map. All components
%% taken from the maps are then clamped to the range  [0 1]. The contents of the four maps is
%% specified with  {@link gl:pixelMapfv/3} . 
%%
%% `Depth':  Each depth value is multiplied by `?GL_DEPTH_SCALE', added to `?GL_DEPTH_BIAS'
%% , then clamped to the range  [0 1]. 
%%
%% `Stencil':  Each index is shifted `?GL_INDEX_SHIFT' bits just as a color index
%% is, then added to `?GL_INDEX_OFFSET'. If `?GL_MAP_STENCIL' is true, each index
%% is masked by   2 n-1, where   n is `?GL_PIXEL_MAP_S_TO_S_SIZE', then replaced by
%% the contents of `?GL_PIXEL_MAP_S_TO_S' indexed by the masked value. 
%%
%%  The following table gives the type, initial value, and range of valid values for each
%% of the pixel transfer parameters that are set with ``gl:pixelTransfer''. 
%%
%% <table><tbody><tr><td> `Pname' </td><td>` Type '</td><td>` Initial Value '</td>
%% <td>` Valid Range '</td></tr></tbody><tbody><tr><td>`?GL_MAP_COLOR'</td><td>
%% boolean </td><td> false </td><td> true/false </td></tr><tr><td>`?GL_MAP_STENCIL'</td>
%% <td> boolean </td><td> false </td><td> true/false </td></tr><tr><td>`?GL_INDEX_SHIFT'</td>
%% <td> integer </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_INDEX_OFFSET'</td><td> integer
%% </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_RED_SCALE'</td><td> float </td><td> 1 </td>
%% <td>(-)</td></tr><tr><td>`?GL_GREEN_SCALE'</td><td> float </td><td> 1 </td><td>(-)</td></tr>
%% <tr><td>`?GL_BLUE_SCALE'</td><td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_ALPHA_SCALE'
%% </td><td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_DEPTH_SCALE'</td><td>
%% float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_RED_BIAS'</td><td> float </td><td>
%% 0 </td><td>(-)</td></tr><tr><td>`?GL_GREEN_BIAS'</td><td> float </td><td> 0 </td><td>(-)</td>
%% </tr><tr><td>`?GL_BLUE_BIAS'</td><td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_ALPHA_BIAS'
%% </td><td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_DEPTH_BIAS'</td><td>
%% float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_RED_SCALE'</td><td>
%%  float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_GREEN_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_BLUE_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_ALPHA_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_RED_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_GREEN_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_BLUE_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_COLOR_MATRIX_ALPHA_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_RED_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_GREEN_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_BLUE_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_ALPHA_SCALE'</td>
%% <td> float </td><td> 1 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_RED_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_GREEN_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_BLUE_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr><tr><td>`?GL_POST_CONVOLUTION_ALPHA_BIAS'</td>
%% <td> float </td><td> 0 </td><td>(-)</td></tr></tbody></table>
%%
%% ``gl:pixelTransferf'' can be used to set any pixel transfer parameter. If the parameter
%% type is boolean, 0 implies false and any other value implies true. If  `Pname'  is
%% an integer parameter,  `Param'  is rounded to the nearest integer. 
%%
%%  Likewise, ``gl:pixelTransferi'' can be used to set any of the pixel transfer parameters.
%% Boolean parameters are set to false if  `Param'  is 0 and to true otherwise.  `Param' 
%%  is converted to floating point before being assigned to real-valued parameters. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelTransfer.xml">external</a> documentation.
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
%%  `Map'  is a symbolic map name, indicating one of ten maps to set.  `Mapsize'  specifies
%% the number of entries in the map, and  `Values'  is a pointer to an array of  `Mapsize' 
%%  map values. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a pixel transfer map is specified,  `Values'  is
%% treated as a byte offset into the buffer object's data store. 
%%
%%  The ten maps are as follows: 
%%
%% `?GL_PIXEL_MAP_I_TO_I':  Maps color indices to color indices. 
%%
%% `?GL_PIXEL_MAP_S_TO_S':  Maps stencil indices to stencil indices. 
%%
%% `?GL_PIXEL_MAP_I_TO_R':  Maps color indices to red components. 
%%
%% `?GL_PIXEL_MAP_I_TO_G':  Maps color indices to green components. 
%%
%% `?GL_PIXEL_MAP_I_TO_B':  Maps color indices to blue components. 
%%
%% `?GL_PIXEL_MAP_I_TO_A':  Maps color indices to alpha components. 
%%
%% `?GL_PIXEL_MAP_R_TO_R':  Maps red components to red components. 
%%
%% `?GL_PIXEL_MAP_G_TO_G':  Maps green components to green components. 
%%
%% `?GL_PIXEL_MAP_B_TO_B':  Maps blue components to blue components. 
%%
%% `?GL_PIXEL_MAP_A_TO_A':  Maps alpha components to alpha components. 
%%
%%  The entries in a map can be specified as single-precision floating-point numbers, unsigned
%% short integers, or unsigned int integers. Maps that store color component values (all
%% but `?GL_PIXEL_MAP_I_TO_I' and `?GL_PIXEL_MAP_S_TO_S') retain their values in
%% floating-point format, with unspecified mantissa and exponent sizes. Floating-point values
%% specified by ``gl:pixelMapfv'' are converted directly to the internal floating-point
%% format of these maps, then clamped to the range [0,1]. Unsigned integer values specified
%% by ``gl:pixelMapusv'' and ``gl:pixelMapuiv'' are converted linearly such that the
%% largest representable integer maps to 1.0, and 0 maps to 0.0. 
%%
%%  Maps that store indices, `?GL_PIXEL_MAP_I_TO_I' and `?GL_PIXEL_MAP_S_TO_S',
%% retain their values in fixed-point format, with an unspecified number of bits to the right
%% of the binary point. Floating-point values specified by ``gl:pixelMapfv'' are converted
%% directly to the internal fixed-point format of these maps. Unsigned integer values specified
%% by ``gl:pixelMapusv'' and ``gl:pixelMapuiv'' specify integer values, with all 0's
%% to the right of the binary point. 
%%
%%  The following table shows the initial sizes and values for each of the maps. Maps that
%% are indexed by either color or stencil indices must have  `Mapsize'  =   2 n for some
%%   n or the results are undefined. The maximum allowable size for each map depends on the
%% implementation and can be determined by calling  {@link gl:getBooleanv/1}  with argument `?GL_MAX_PIXEL_MAP_TABLE'
%% . The single maximum applies to all maps; it is at least 32. <table><tbody><tr><td> `Map' 
%% </td><td>` Lookup Index '</td><td>` Lookup Value '</td><td>` Initial Size '</td>
%% <td>` Initial Value '</td></tr></tbody><tbody><tr><td>`?GL_PIXEL_MAP_I_TO_I'</td>
%% <td> color index </td><td> color index </td><td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_S_TO_S'
%% </td><td> stencil index  </td><td> stencil index  </td><td> 1 </td><td> 0 </td></tr><tr><td>
%% `?GL_PIXEL_MAP_I_TO_R'</td><td> color index  </td><td> R </td><td> 1 </td><td> 0 </td>
%% </tr><tr><td>`?GL_PIXEL_MAP_I_TO_G'</td><td> color index  </td><td> G </td><td> 1 </td>
%% <td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_I_TO_B'</td><td> color index </td><td> B </td>
%% <td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_I_TO_A'</td><td> color index </td>
%% <td> A </td><td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_R_TO_R'</td><td> R </td>
%% <td> R </td><td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_G_TO_G'</td><td> G </td>
%% <td> G </td><td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_B_TO_B'</td><td> B </td>
%% <td> B </td><td> 1 </td><td> 0 </td></tr><tr><td>`?GL_PIXEL_MAP_A_TO_A'</td><td> A </td>
%% <td> A </td><td> 1 </td><td> 0 </td></tr></tbody></table>
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPixelMap.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a pixel map is requested,  `Data'  is treated as
%% a byte offset into the buffer object's data store. 
%%
%%  Unsigned integer values, if requested, are linearly mapped from the internal fixed or
%% floating-point representation such that 1.0 maps to the largest representable integer
%% value, and 0.0 maps to 0. Return unsigned integer values are undefined if the map value
%% was not in the range [0,1]. 
%%
%%  To determine the required size of  `Map' , call  {@link gl:getBooleanv/1}  with the appropriate
%% symbolic constant. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetPixelMap.xml">external</a> documentation.
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
%% ``gl:bitmap'' takes seven arguments. The first pair specifies the width and height of
%% the bitmap image. The second pair specifies the location of the bitmap origin relative
%% to the lower left corner of the bitmap image. The third pair of arguments specifies `x'
%%  and `y' offsets to be added to the current raster position after the bitmap has
%% been drawn. The final argument is a pointer to the bitmap image itself. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a bitmap image is specified,  `Bitmap'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  The bitmap image is interpreted like image data for the  {@link gl:drawPixels/5}  command,
%% with  `Width'  and  `Height'  corresponding to the width and height arguments of
%% that command, and with `type' set to `?GL_BITMAP' and `format' set to `?GL_COLOR_INDEX'
%% . Modes specified using  {@link gl:pixelStoref/2}  affect the interpretation of bitmap image
%% data; modes specified using  {@link gl:pixelTransferf/2}  do not. 
%%
%%  If the current raster position is invalid, ``gl:bitmap'' is ignored. Otherwise, the
%% lower left corner of the bitmap image is positioned at the window coordinates 
%%
%%  x w=|x r-x o|
%%
%%  y w=|y r-y o|
%%
%%  where  (x r y r) is the raster position and  (x o y o) is the bitmap origin. Fragments are then generated
%% for each pixel corresponding to a 1 (one) in the bitmap image. These fragments are generated
%% using the current raster `z' coordinate, color or color index, and current raster
%% texture coordinates. They are then treated just as if they had been generated by a point,
%% line, or polygon, including texture mapping, fogging, and all per-fragment operations
%% such as alpha and depth testing. 
%%
%%  After the bitmap has been drawn, the `x' and `y' coordinates of the current
%% raster position are offset by  `Xmove'  and  `Ymove' . No change is made to the `z'
%%  coordinate of the current raster position, or to the current raster color, texture coordinates,
%% or index. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBitmap.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a block of pixels is requested,  `Data'  is treated
%% as a byte offset into the buffer object's data store rather than a pointer to client memory.
%% 
%%
%% ``gl:readPixels'' returns values from each pixel with lower left corner at (x+i y+j) for   0&lt;=
%%  i&lt; width and  0&lt;= j&lt; height. This pixel is said to be the   ith pixel in the  
%% jth row. Pixels are returned in row order from the lowest to the highest row, left to
%% right in each row. 
%%
%%  `Format'  specifies the format for the returned pixel values; accepted values are: 
%%
%% `?GL_STENCIL_INDEX':  Stencil values are read from the stencil buffer. Each index
%% is converted to fixed point, shifted left or right depending on the value and sign of `?GL_INDEX_SHIFT'
%% , and added to `?GL_INDEX_OFFSET'. If `?GL_MAP_STENCIL' is `?GL_TRUE',
%% indices are replaced by their mappings in the table `?GL_PIXEL_MAP_S_TO_S'. 
%%
%% `?GL_DEPTH_COMPONENT':  Depth values are read from the depth buffer. Each component
%% is converted to floating point such that the minimum depth value maps to 0 and the maximum
%% value maps to 1. Each component is then multiplied by `?GL_DEPTH_SCALE', added to `?GL_DEPTH_BIAS'
%% , and finally clamped to the range  [0 1]. 
%%
%% `?GL_DEPTH_STENCIL':  Values are taken from both the depth and stencil buffers. The  `Type' 
%%  parameter must be `?GL_UNSIGNED_INT_24_8' or `?GL_FLOAT_32_UNSIGNED_INT_24_8_REV'
%% . 
%%
%% `?GL_RED'
%%
%% `?GL_GREEN'
%%
%% `?GL_BLUE'
%%
%% `?GL_RGB'
%%
%% `?GL_BGR'
%%
%% `?GL_RGBA'
%%
%% `?GL_BGRA':  Finally, the indices or components are converted to the proper format,
%% as specified by  `Type' . If  `Format'  is `?GL_STENCIL_INDEX' and  `Type' 
%% is not `?GL_FLOAT', each index is masked with the mask value given in the following
%% table. If  `Type'  is `?GL_FLOAT', then each integer index is converted to single-precision
%% floating-point format. 
%%
%%  If  `Format'  is `?GL_RED', `?GL_GREEN', `?GL_BLUE', `?GL_RGB', `?GL_BGR'
%% , `?GL_RGBA', or `?GL_BGRA' and  `Type'  is not `?GL_FLOAT', each component
%% is multiplied by the multiplier shown in the following table. If type is `?GL_FLOAT',
%% then each component is passed as is (or converted to the client's single-precision floating-point
%% format if it is different from the one used by the GL). 
%%
%% <table><tbody><tr><td> `Type' </td><td>` Index Mask '</td><td>` Component Conversion '
%% </td></tr></tbody><tbody><tr><td>`?GL_UNSIGNED_BYTE'</td><td> 2 8-1</td><td>(2 8-1)  c</td></tr>
%% <tr><td>`?GL_BYTE'</td><td> 2 7-1</td><td>((2 8-1)  c-1)/2</td></tr><tr><td>`?GL_UNSIGNED_SHORT'
%% </td><td> 2 16-1</td><td>(2 16-1)  c</td></tr><tr><td>`?GL_SHORT'</td><td> 2 15-1</td><td>((2
%% 16-1) 
%% c-1)/2</td>
%% </tr><tr><td>`?GL_UNSIGNED_INT'</td><td> 2 32-1</td><td>(2 32-1)  c</td></tr><tr><td>`?GL_INT'
%% </td><td> 2 31-1</td><td>((2 32-1)  c-1)/2</td></tr><tr><td>`?GL_HALF_FLOAT'</td><td> none </td><td>
%%  c</td></tr><tr><td>`?GL_FLOAT'</td><td> none </td><td> c</td></tr><tr><td>`?GL_UNSIGNED_BYTE_3_3_2'
%% </td><td> 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_BYTE_2_3_3_REV'</td><td>
%% 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_6_5'</td><td> 2 N-1</td><td>
%% (2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_6_5_REV'</td><td> 2 N-1</td><td>(2 N-1)  c</td></tr>
%% <tr><td>`?GL_UNSIGNED_SHORT_4_4_4_4'</td><td> 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_SHORT_4_4_4_4_REV'
%% </td><td> 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_5_5_1'</td><td> 2
%% N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_SHORT_1_5_5_5_REV'</td><td> 2 N-1</td>
%% <td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_INT_8_8_8_8'</td><td> 2 N-1</td><td>(2 N-1)  c</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_8_8_8_8_REV'</td><td> 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_INT_10_10_10_2'
%% </td><td> 2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_INT_2_10_10_10_REV'</td><td>
%%  2 N-1</td><td>(2 N-1)  c</td></tr><tr><td>`?GL_UNSIGNED_INT_24_8'</td><td> 2 N-1</td><td>(2
%% N-1) 
%% c</td></tr><tr><td>`?GL_UNSIGNED_INT_10F_11F_11F_REV'</td><td> -- </td><td> Special </td>
%% </tr><tr><td>`?GL_UNSIGNED_INT_5_9_9_9_REV'</td><td> -- </td><td> Special </td></tr><tr>
%% <td>`?GL_FLOAT_32_UNSIGNED_INT_24_8_REV'</td><td> none </td><td> c (Depth Only) </td>
%% </tr></tbody></table>
%%
%%  Return values are placed in memory as follows. If  `Format'  is `?GL_STENCIL_INDEX'
%% , `?GL_DEPTH_COMPONENT', `?GL_RED', `?GL_GREEN', or `?GL_BLUE', a
%% single value is returned and the data for the   ith pixel in the   jth row is placed in
%% location  (j)  width+i. `?GL_RGB' and `?GL_BGR' return three values, `?GL_RGBA'
%%  and `?GL_BGRA' return four values for each pixel, with all values corresponding
%% to a single pixel occupying contiguous space in  `Data' . Storage parameters set by  {@link gl:pixelStoref/2} 
%% , such as `?GL_PACK_LSB_FIRST' and `?GL_PACK_SWAP_BYTES', affect the way that
%% data is written into memory. See  {@link gl:pixelStoref/2}  for a description. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReadPixels.xml">external</a> documentation.
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
%%  Several parameters define the encoding of pixel data in memory and control the processing
%% of the pixel data before it is placed in the frame buffer. These parameters are set with
%% four commands:  {@link gl:pixelStoref/2} ,  {@link gl:pixelTransferf/2} ,  {@link gl:pixelMapfv/3} ,
%% and  {@link gl:pixelZoom/2} . This reference page describes the effects on ``gl:drawPixels''
%%  of many, but not all, of the parameters specified by these four commands. 
%%
%%  Data is read from  `Data'  as a sequence of signed or unsigned bytes, signed or unsigned
%% shorts, signed or unsigned integers, or single-precision floating-point values, depending
%% on  `Type' . When  `Type'  is one of `?GL_UNSIGNED_BYTE', `?GL_BYTE', `?GL_UNSIGNED_SHORT'
%% , `?GL_SHORT', `?GL_UNSIGNED_INT', `?GL_INT', or `?GL_FLOAT' each
%% of these bytes, shorts, integers, or floating-point values is interpreted as one color
%% or depth component, or one index, depending on  `Format' . When  `Type'  is one of `?GL_UNSIGNED_BYTE_3_3_2'
%% , `?GL_UNSIGNED_SHORT_5_6_5', `?GL_UNSIGNED_SHORT_4_4_4_4', `?GL_UNSIGNED_SHORT_5_5_5_1'
%% , `?GL_UNSIGNED_INT_8_8_8_8', or `?GL_UNSIGNED_INT_10_10_10_2', each unsigned
%% value is interpreted as containing all the components for a single pixel, with the color
%% components arranged according to  `Format' . When  `Type'  is one of `?GL_UNSIGNED_BYTE_2_3_3_REV'
%% , `?GL_UNSIGNED_SHORT_5_6_5_REV', `?GL_UNSIGNED_SHORT_4_4_4_4_REV', `?GL_UNSIGNED_SHORT_1_5_5_5_REV'
%% , `?GL_UNSIGNED_INT_8_8_8_8_REV', or `?GL_UNSIGNED_INT_2_10_10_10_REV', each
%% unsigned value is interpreted as containing all color components, specified by  `Format' 
%% , for a single pixel in a reversed order. Indices are always treated individually. Color
%% components are treated as groups of one, two, three, or four values, again based on  `Format' 
%% . Both individual indices and groups of components are referred to as pixels. If  `Type' 
%%  is `?GL_BITMAP', the data must be unsigned bytes, and  `Format'  must be either `?GL_COLOR_INDEX'
%%  or `?GL_STENCIL_INDEX'. Each unsigned byte is treated as eight 1-bit pixels, with
%% bit ordering determined by `?GL_UNPACK_LSB_FIRST' (see  {@link gl:pixelStoref/2} ). 
%%
%%  width×height pixels are read from memory, starting at location  `Data' . By default,
%% these pixels are taken from adjacent memory locations, except that after all  `Width' 
%% pixels are read, the read pointer is advanced to the next four-byte boundary. The four-byte
%% row alignment is specified by  {@link gl:pixelStoref/2}  with argument `?GL_UNPACK_ALIGNMENT'
%% , and it can be set to one, two, four, or eight bytes. Other pixel store parameters specify
%% different read pointer advancements, both before the first pixel is read and after all  `Width' 
%%  pixels are read. See the  {@link gl:pixelStoref/2}  reference page for details on these options.
%% 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a block of pixels is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  The   width×height pixels that are read from memory are each operated on in the same
%% way, based on the values of several parameters specified by  {@link gl:pixelTransferf/2} 
%% and  {@link gl:pixelMapfv/3} . The details of these operations, as well as the target buffer
%% into which the pixels are drawn, are specific to the format of the pixels, as specified
%% by  `Format' .  `Format'  can assume one of 13 symbolic values: 
%%
%% `?GL_COLOR_INDEX':  Each pixel is a single value, a color index. It is converted
%% to fixed-point format, with an unspecified number of bits to the right of the binary point,
%% regardless of the memory data type. Floating-point values convert to true fixed-point
%% values. Signed and unsigned integer data is converted with all fraction bits set to 0.
%% Bitmap data convert to either 0 or 1. 
%%
%%  Each fixed-point index is then shifted left by `?GL_INDEX_SHIFT' bits and added to `?GL_INDEX_OFFSET'
%% . If `?GL_INDEX_SHIFT' is negative, the shift is to the right. In either case, zero
%% bits fill otherwise unspecified bit locations in the result. 
%%
%%  If the GL is in RGBA mode, the resulting index is converted to an RGBA pixel with the
%% help of the `?GL_PIXEL_MAP_I_TO_R', `?GL_PIXEL_MAP_I_TO_G', `?GL_PIXEL_MAP_I_TO_B'
%% , and `?GL_PIXEL_MAP_I_TO_A' tables. If the GL is in color index mode, and if `?GL_MAP_COLOR'
%%  is true, the index is replaced with the value that it references in lookup table `?GL_PIXEL_MAP_I_TO_I'
%% . Whether the lookup replacement of the index is done or not, the integer part of the
%% index is then ANDed with   2 b-1, where   b is the number of bits in a color index buffer.
%% 
%%
%%  The GL then converts the resulting indices or RGBA colors to fragments by attaching the
%% current raster position `z' coordinate and texture coordinates to each pixel, then
%% assigning   x and   y window coordinates to the   nth fragment such that  x n=x r+n% width
%% 
%%
%%  y n=y r+|n/width|
%%
%%  where  (x r y r) is the current raster position. These pixel fragments are then treated just like
%% the fragments generated by rasterizing points, lines, or polygons. Texture mapping, fog,
%% and all the fragment operations are applied before the fragments are written to the frame
%% buffer. 
%%
%% `?GL_STENCIL_INDEX':  Each pixel is a single value, a stencil index. It is converted
%% to fixed-point format, with an unspecified number of bits to the right of the binary point,
%% regardless of the memory data type. Floating-point values convert to true fixed-point
%% values. Signed and unsigned integer data is converted with all fraction bits set to 0.
%% Bitmap data convert to either 0 or 1. 
%%
%%  Each fixed-point index is then shifted left by `?GL_INDEX_SHIFT' bits, and added
%% to `?GL_INDEX_OFFSET'. If `?GL_INDEX_SHIFT' is negative, the shift is to the
%% right. In either case, zero bits fill otherwise unspecified bit locations in the result.
%% If `?GL_MAP_STENCIL' is true, the index is replaced with the value that it references
%% in lookup table `?GL_PIXEL_MAP_S_TO_S'. Whether the lookup replacement of the index
%% is done or not, the integer part of the index is then ANDed with   2 b-1, where   b is
%% the number of bits in the stencil buffer. The resulting stencil indices are then written
%% to the stencil buffer such that the   nth index is written to location 
%%
%%  x n=x r+n% width
%%
%%  y n=y r+|n/width|
%%
%%  where  (x r y r) is the current raster position. Only the pixel ownership test, the scissor test,
%% and the stencil writemask affect these write operations. 
%%
%% `?GL_DEPTH_COMPONENT':  Each pixel is a single-depth component. Floating-point data
%% is converted directly to an internal floating-point format with unspecified precision.
%% Signed integer data is mapped linearly to the internal floating-point format such that
%% the most positive representable integer value maps to 1.0, and the most negative representable
%% value maps to   -1.0. Unsigned integer data is mapped similarly: the largest integer value
%% maps to 1.0, and 0 maps to 0.0. The resulting floating-point depth value is then multiplied
%% by `?GL_DEPTH_SCALE' and added to `?GL_DEPTH_BIAS'. The result is clamped to
%% the range  [0 1]. 
%%
%%  The GL then converts the resulting depth components to fragments by attaching the current
%% raster position color or color index and texture coordinates to each pixel, then assigning
%%   x and   y window coordinates to the   nth fragment such that 
%%
%%  x n=x r+n% width
%%
%%  y n=y r+|n/width|
%%
%%  where  (x r y r) is the current raster position. These pixel fragments are then treated just like
%% the fragments generated by rasterizing points, lines, or polygons. Texture mapping, fog,
%% and all the fragment operations are applied before the fragments are written to the frame
%% buffer. 
%%
%% `?GL_RGBA'
%%
%% `?GL_BGRA':  Each pixel is a four-component group: For `?GL_RGBA', the red component
%% is first, followed by green, followed by blue, followed by alpha; for `?GL_BGRA'
%% the order is blue, green, red and then alpha. Floating-point values are converted directly
%% to an internal floating-point format with unspecified precision. Signed integer values
%% are mapped linearly to the internal floating-point format such that the most positive
%% representable integer value maps to 1.0, and the most negative representable value maps
%% to   -1.0. (Note that this mapping does not convert 0 precisely to 0.0.) Unsigned integer
%% data is mapped similarly: The largest integer value maps to 1.0, and 0 maps to 0.0. The
%% resulting floating-point color values are then multiplied by `?GL_c_SCALE' and added
%% to `?GL_c_BIAS', where `c' is RED, GREEN, BLUE, and ALPHA for the respective
%% color components. The results are clamped to the range  [0 1]. 
%%
%%  If `?GL_MAP_COLOR' is true, each color component is scaled by the size of lookup
%% table `?GL_PIXEL_MAP_c_TO_c', then replaced by the value that it references in that
%% table. `c' is R, G, B, or A respectively. 
%%
%%  The GL then converts the resulting RGBA colors to fragments by attaching the current
%% raster position `z' coordinate and texture coordinates to each pixel, then assigning
%%   x and   y window coordinates to the   nth fragment such that 
%%
%%  x n=x r+n% width
%%
%%  y n=y r+|n/width|
%%
%%  where  (x r y r) is the current raster position. These pixel fragments are then treated just like
%% the fragments generated by rasterizing points, lines, or polygons. Texture mapping, fog,
%% and all the fragment operations are applied before the fragments are written to the frame
%% buffer. 
%%
%% `?GL_RED':  Each pixel is a single red component. This component is converted to
%% the internal floating-point format in the same way the red component of an RGBA pixel
%% is. It is then converted to an RGBA pixel with green and blue set to 0, and alpha set
%% to 1. After this conversion, the pixel is treated as if it had been read as an RGBA pixel.
%% 
%%
%% `?GL_GREEN':  Each pixel is a single green component. This component is converted
%% to the internal floating-point format in the same way the green component of an RGBA pixel
%% is. It is then converted to an RGBA pixel with red and blue set to 0, and alpha set to
%% 1. After this conversion, the pixel is treated as if it had been read as an RGBA pixel. 
%%
%% `?GL_BLUE':  Each pixel is a single blue component. This component is converted to
%% the internal floating-point format in the same way the blue component of an RGBA pixel
%% is. It is then converted to an RGBA pixel with red and green set to 0, and alpha set to
%% 1. After this conversion, the pixel is treated as if it had been read as an RGBA pixel. 
%%
%% `?GL_ALPHA':  Each pixel is a single alpha component. This component is converted
%% to the internal floating-point format in the same way the alpha component of an RGBA pixel
%% is. It is then converted to an RGBA pixel with red, green, and blue set to 0. After this
%% conversion, the pixel is treated as if it had been read as an RGBA pixel. 
%%
%% `?GL_RGB'
%%
%% `?GL_BGR':  Each pixel is a three-component group: red first, followed by green,
%% followed by blue; for `?GL_BGR', the first component is blue, followed by green and
%% then red. Each component is converted to the internal floating-point format in the same
%% way the red, green, and blue components of an RGBA pixel are. The color triple is converted
%% to an RGBA pixel with alpha set to 1. After this conversion, the pixel is treated as if
%% it had been read as an RGBA pixel. 
%%
%% `?GL_LUMINANCE':  Each pixel is a single luminance component. This component is converted
%% to the internal floating-point format in the same way the red component of an RGBA pixel
%% is. It is then converted to an RGBA pixel with red, green, and blue set to the converted
%% luminance value, and alpha set to 1. After this conversion, the pixel is treated as if
%% it had been read as an RGBA pixel. 
%%
%% `?GL_LUMINANCE_ALPHA':  Each pixel is a two-component group: luminance first, followed
%% by alpha. The two components are converted to the internal floating-point format in the
%% same way the red component of an RGBA pixel is. They are then converted to an RGBA pixel
%% with red, green, and blue set to the converted luminance value, and alpha set to the converted
%% alpha value. After this conversion, the pixel is treated as if it had been read as an
%% RGBA pixel.  
%%
%%  The following table summarizes the meaning of the valid constants for the `type'
%% parameter:    
%%
%% <table><tbody><tr><td>` Type '</td><td>` Corresponding Type '</td></tr></tbody><tbody>
%% <tr><td>`?GL_UNSIGNED_BYTE'</td><td> unsigned 8-bit integer </td></tr><tr><td>`?GL_BYTE'
%% </td><td> signed 8-bit integer </td></tr><tr><td>`?GL_BITMAP'</td><td> single bits
%% in unsigned 8-bit integers </td></tr><tr><td>`?GL_UNSIGNED_SHORT'</td><td> unsigned
%% 16-bit integer </td></tr><tr><td>`?GL_SHORT'</td><td> signed 16-bit integer </td></tr>
%% <tr><td>`?GL_UNSIGNED_INT'</td><td> unsigned 32-bit integer </td></tr><tr><td>`?GL_INT'
%% </td><td> 32-bit integer </td></tr><tr><td>`?GL_FLOAT'</td><td> single-precision
%% floating-point </td></tr><tr><td>`?GL_UNSIGNED_BYTE_3_3_2'</td><td> unsigned 8-bit
%% integer  </td></tr><tr><td>`?GL_UNSIGNED_BYTE_2_3_3_REV'</td><td> unsigned 8-bit
%% integer with reversed component ordering </td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_6_5'</td>
%% <td> unsigned 16-bit integer </td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_6_5_REV'</td><td>
%%  unsigned 16-bit integer with reversed component ordering </td></tr><tr><td>`?GL_UNSIGNED_SHORT_4_4_4_4'
%% </td><td> unsigned 16-bit integer </td></tr><tr><td>`?GL_UNSIGNED_SHORT_4_4_4_4_REV'</td>
%% <td> unsigned 16-bit integer with reversed component ordering </td></tr><tr><td>`?GL_UNSIGNED_SHORT_5_5_5_1'
%% </td><td> unsigned 16-bit integer </td></tr><tr><td>`?GL_UNSIGNED_SHORT_1_5_5_5_REV'</td>
%% <td> unsigned 16-bit integer with reversed component ordering </td></tr><tr><td>`?GL_UNSIGNED_INT_8_8_8_8'
%% </td><td> unsigned 32-bit integer </td></tr><tr><td>`?GL_UNSIGNED_INT_8_8_8_8_REV'</td>
%% <td> unsigned 32-bit integer with reversed component ordering </td></tr><tr><td>`?GL_UNSIGNED_INT_10_10_10_2'
%% </td><td> unsigned 32-bit integer </td></tr><tr><td>`?GL_UNSIGNED_INT_2_10_10_10_REV'</td>
%% <td> unsigned 32-bit integer with reversed component ordering </td></tr></tbody></table>
%%
%%  The rasterization described so far assumes pixel zoom factors of 1. If  {@link gl:pixelZoom/2} 
%%  is used to change the   x and   y pixel zoom factors, pixels are converted to fragments
%% as follows. If  (x r y r) is the current raster position, and a given pixel is in the   nth column
%% and   mth row of the pixel rectangle, then fragments are generated for pixels whose centers
%% are in the rectangle with corners at 
%%
%% (x r+(zoom x)  n y r+(zoom y)  m)
%%
%% (x r+(zoom x)(n+1) y r+(zoom y)(m+1))
%%
%%  where   zoom x is the value of `?GL_ZOOM_X' and  zoom y is the value of `?GL_ZOOM_Y'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawPixels.xml">external</a> documentation.
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
%%  `X'  and  `Y'  specify the window coordinates of the lower left corner of the rectangular
%% region to be copied.  `Width'  and  `Height'  specify the dimensions of the rectangular
%% region to be copied. Both  `Width'  and  `Height'  must not be negative. 
%%
%%  Several parameters control the processing of the pixel data while it is being copied.
%% These parameters are set with three commands:  {@link gl:pixelTransferf/2} ,  {@link gl:pixelMapfv/3} 
%% , and  {@link gl:pixelZoom/2} . This reference page describes the effects on ``gl:copyPixels''
%%  of most, but not all, of the parameters specified by these three commands. 
%%
%% ``gl:copyPixels'' copies values from each pixel with the lower left-hand corner at (x+i
%% y+j)
%% for   0&lt;= i&lt; width and   0&lt;= j&lt; height. This pixel is said to be the   ith
%% pixel in the   jth row. Pixels are copied in row order from the lowest to the highest
%% row, left to right in each row. 
%%
%%  `Type'  specifies whether color, depth, or stencil data is to be copied. The details
%% of the transfer for each data type are as follows: 
%%
%% `?GL_COLOR':  Indices or RGBA colors are read from the buffer currently specified
%% as the read source buffer (see  {@link gl:readBuffer/1} ). If the GL is in color index mode,
%% each index that is read from this buffer is converted to a fixed-point format with an
%% unspecified number of bits to the right of the binary point. Each index is then shifted
%% left by `?GL_INDEX_SHIFT' bits, and added to `?GL_INDEX_OFFSET'. If `?GL_INDEX_SHIFT'
%%  is negative, the shift is to the right. In either case, zero bits fill otherwise unspecified
%% bit locations in the result. If `?GL_MAP_COLOR' is true, the index is replaced with
%% the value that it references in lookup table `?GL_PIXEL_MAP_I_TO_I'. Whether the
%% lookup replacement of the index is done or not, the integer part of the index is then
%% ANDed with   2 b-1, where   b is the number of bits in a color index buffer. 
%%
%%  If the GL is in RGBA mode, the red, green, blue, and alpha components of each pixel that
%% is read are converted to an internal floating-point format with unspecified precision.
%% The conversion maps the largest representable component value to 1.0, and component value
%% 0 to 0.0. The resulting floating-point color values are then multiplied by `?GL_c_SCALE'
%%  and added to `?GL_c_BIAS', where `c' is RED, GREEN, BLUE, and ALPHA for the
%% respective color components. The results are clamped to the range [0,1]. If `?GL_MAP_COLOR'
%%  is true, each color component is scaled by the size of lookup table `?GL_PIXEL_MAP_c_TO_c'
%% , then replaced by the value that it references in that table. `c' is R, G, B, or
%% A. 
%%
%%  If the ARB_imaging extension is supported, the color values may be additionally processed
%% by color-table lookups, color-matrix transformations, and convolution filters. 
%%
%%  The GL then converts the resulting indices or RGBA colors to fragments by attaching the
%% current raster position `z' coordinate and texture coordinates to each pixel, then
%% assigning window coordinates (x r+i y r+j), where  (x r y r) is the current raster position, and the pixel was
%% the   ith pixel in the   jth row. These pixel fragments are then treated just like the
%% fragments generated by rasterizing points, lines, or polygons. Texture mapping, fog, and
%% all the fragment operations are applied before the fragments are written to the frame
%% buffer. 
%%
%% `?GL_DEPTH':  Depth values are read from the depth buffer and converted directly
%% to an internal floating-point format with unspecified precision. The resulting floating-point
%% depth value is then multiplied by `?GL_DEPTH_SCALE' and added to `?GL_DEPTH_BIAS'
%% . The result is clamped to the range [0,1]. 
%%
%%  The GL then converts the resulting depth components to fragments by attaching the current
%% raster position color or color index and texture coordinates to each pixel, then assigning
%% window coordinates (x r+i y r+j), where  (x r y r) is the current raster position, and the pixel was the   ith
%% pixel in the   jth row. These pixel fragments are then treated just like the fragments
%% generated by rasterizing points, lines, or polygons. Texture mapping, fog, and all the
%% fragment operations are applied before the fragments are written to the frame buffer. 
%%
%% `?GL_STENCIL':  Stencil indices are read from the stencil buffer and converted to
%% an internal fixed-point format with an unspecified number of bits to the right of the
%% binary point. Each fixed-point index is then shifted left by `?GL_INDEX_SHIFT' bits,
%% and added to `?GL_INDEX_OFFSET'. If `?GL_INDEX_SHIFT' is negative, the shift
%% is to the right. In either case, zero bits fill otherwise unspecified bit locations in
%% the result. If `?GL_MAP_STENCIL' is true, the index is replaced with the value that
%% it references in lookup table `?GL_PIXEL_MAP_S_TO_S'. Whether the lookup replacement
%% of the index is done or not, the integer part of the index is then ANDed with   2 b-1,
%% where   b is the number of bits in the stencil buffer. The resulting stencil indices are
%% then written to the stencil buffer such that the index read from the   ith location of
%% the   jth row is written to location (x r+i y r+j), where  (x r y r) is the current raster position. Only the
%% pixel ownership test, the scissor test, and the stencil writemask affect these write operations.
%% 
%%
%%  The rasterization described thus far assumes pixel zoom factors of 1.0. If  {@link gl:pixelZoom/2} 
%%  is used to change the   x and   y pixel zoom factors, pixels are converted to fragments
%% as follows. If  (x r y r) is the current raster position, and a given pixel is in the   ith location
%% in the   jth row of the source pixel rectangle, then fragments are generated for pixels
%% whose centers are in the rectangle with corners at 
%%
%% (x r+(zoom x)  i y r+(zoom y)  j)
%%
%%  and 
%%
%% (x r+(zoom x)(i+1) y r+(zoom y)(j+1))
%%
%%  where   zoom x is the value of `?GL_ZOOM_X' and  zoom y is the value of `?GL_ZOOM_Y'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyPixels.xml">external</a> documentation.
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
%%  The stencil test conditionally eliminates a pixel based on the outcome of a comparison
%% between the reference value and the value in the stencil buffer. To enable and disable
%% the test, call  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_STENCIL_TEST'
%% . To specify actions based on the outcome of the stencil test, call  {@link gl:stencilOp/3} 
%% or   {@link gl:stencilOpSeparate/4} . 
%%
%%  There can be two separate sets of  `Func' ,  `Ref' , and   `Mask'  parameters;
%% one affects back-facing polygons, and the other affects front-facing polygons as well
%% as other non-polygon primitives.   {@link gl:stencilFunc/3}  sets both front and back stencil
%% state to the same values. Use  {@link gl:stencilFuncSeparate/4}  to set front and back stencil
%% state to different values. 
%%
%%  `Func'  is a symbolic constant that determines the stencil comparison function. It
%% accepts one of eight values, shown in the following list.  `Ref'  is an integer reference
%% value that is used in the stencil comparison. It is clamped to the range  [0 2 n-1], where   n
%% is the number of bitplanes in the stencil buffer.  `Mask'  is bitwise ANDed with both
%% the reference value and the stored stencil value, with the ANDed values participating
%% in the comparison. 
%%
%%  If `stencil' represents the value stored in the corresponding stencil buffer location,
%% the following list shows the effect of each comparison function that can be specified by  `Func' 
%% . Only if the comparison succeeds is the pixel passed through to the next stage in the
%% rasterization process (see  {@link gl:stencilOp/3} ). All tests treat `stencil' values
%% as unsigned integers in the range [0 2 n-1], where   n is the number of bitplanes in the stencil
%% buffer. 
%%
%%  The following values are accepted by  `Func' : 
%%
%% `?GL_NEVER':  Always fails. 
%%
%% `?GL_LESS':  Passes if (  `Ref'  &amp;  `Mask'  ) &lt; ( `stencil' &amp;  `Mask' 
%%  ). 
%%
%% `?GL_LEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) &lt;= ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_GREATER':  Passes if (  `Ref'  &amp;  `Mask'  ) &gt; ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_GEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) &gt;= ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_EQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) = ( `stencil' &amp;  `Mask' 
%%  ). 
%%
%% `?GL_NOTEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) != ( `stencil' &amp;
%%  `Mask'  ). 
%%
%% `?GL_ALWAYS':  Always passes. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFunc.xml">external</a> documentation.
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
%%  There can be two separate  `Mask'  writemasks; one affects back-facing polygons, and
%% the other affects front-facing polygons as well as other non-polygon primitives.   {@link gl:stencilMask/1} 
%%  sets both front and back stencil writemasks to the same values. Use  {@link gl:stencilMaskSeparate/2} 
%%  to set front and back stencil writemasks to different values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMask.xml">external</a> documentation.
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
%%  The stencil test conditionally eliminates a pixel based on the outcome of a comparison
%% between the value in the stencil buffer and a reference value. To enable and disable the
%% test, call  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_STENCIL_TEST'
%% ; to control it, call   {@link gl:stencilFunc/3}  or   {@link gl:stencilFuncSeparate/4} . 
%%
%%  There can be two separate sets of  `Sfail' ,  `Dpfail' , and   `Dppass'  parameters;
%% one affects back-facing polygons, and the other affects front-facing polygons as well
%% as other non-polygon primitives.   {@link gl:stencilOp/3}  sets both front and back stencil
%% state to the same values. Use  {@link gl:stencilOpSeparate/4}  to set front and back stencil
%% state to different values. 
%%
%% ``gl:stencilOp'' takes three arguments that indicate what happens to the stored stencil
%% value while stenciling is enabled. If the stencil test fails, no change is made to the
%% pixel's color or depth buffers, and  `Sfail'  specifies what happens to the stencil
%% buffer contents. The following eight actions are possible. 
%%
%% `?GL_KEEP':  Keeps the current value. 
%%
%% `?GL_ZERO':  Sets the stencil buffer value to 0. 
%%
%% `?GL_REPLACE':  Sets the stencil buffer value to `ref', as specified by  {@link gl:stencilFunc/3} 
%% . 
%%
%% `?GL_INCR':  Increments the current stencil buffer value. Clamps to the maximum representable
%% unsigned value. 
%%
%% `?GL_INCR_WRAP':  Increments the current stencil buffer value. Wraps stencil buffer
%% value to zero when incrementing the maximum representable unsigned value. 
%%
%% `?GL_DECR':  Decrements the current stencil buffer value. Clamps to 0. 
%%
%% `?GL_DECR_WRAP':  Decrements the current stencil buffer value. Wraps stencil buffer
%% value to the maximum representable unsigned value when decrementing a stencil buffer value
%% of zero. 
%%
%% `?GL_INVERT':  Bitwise inverts the current stencil buffer value. 
%%
%%  Stencil buffer values are treated as unsigned integers. When incremented and decremented,
%% values are clamped to 0 and   2 n-1, where   n is the value returned by querying `?GL_STENCIL_BITS'
%% . 
%%
%%  The other two arguments to ``gl:stencilOp'' specify stencil buffer actions that depend
%% on whether subsequent depth buffer tests succeed ( `Dppass' ) or fail ( `Dpfail' )
%% (see  {@link gl:depthFunc/1} ). The actions are specified using the same eight symbolic constants
%% as  `Sfail' . Note that  `Dpfail'  is ignored when there is no depth buffer, or
%% when the depth buffer is not enabled. In these cases,  `Sfail'  and  `Dppass'  specify
%% stencil action when the stencil test fails and passes, respectively. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOp.xml">external</a> documentation.
-spec stencilOp(Fail, Zfail, Zpass) -> 'ok' when Fail :: enum(),Zfail :: enum(),Zpass :: enum().
stencilOp(Fail,Zfail,Zpass) ->
  cast(5241, <<Fail:?GLenum,Zfail:?GLenum,Zpass:?GLenum>>).

%% @doc Specify the clear value for the stencil buffer
%%
%% ``gl:clearStencil'' specifies the index used by  {@link gl:clear/1}  to clear the stencil
%% buffer.  `S'  is masked with   2 m-1, where   m is the number of bits in the stencil
%% buffer. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearStencil.xml">external</a> documentation.
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
%%  If the texture generation function is `?GL_OBJECT_LINEAR', the function 
%%
%%  g=p 1×x o+p 2×y o+p 3×z o+p 4×w o
%%
%%  is used, where   g is the value computed for the coordinate named in  `Coord' ,  p 1,
%%  p 2,  p 3, and  p 4 are the four values supplied in  `Params' , and  x o,  y o,  z o,
%% and  w o are the object coordinates of the vertex. This function can be used, for example,
%% to texture-map terrain using sea level as a reference plane (defined by   p 1,   p 2,   p
%% 3, and   p 4). The altitude of a terrain vertex is computed by the `?GL_OBJECT_LINEAR'
%%  coordinate generation function as its distance from sea level; that altitude can then
%% be used to index the texture image to map white snow onto peaks and green grass onto foothills.
%% 
%%
%%  If the texture generation function is `?GL_EYE_LINEAR', the function 
%%
%%  g=(p 1)"×x e+(p 2)"×y e+(p 3)"×z e+(p 4)"×w e
%%
%%  is used, where 
%%
%% ((p 1)" (p 2)" (p 3)" (p 4)")=(p 1  p 2  p 3  p 4)  M -1
%%
%%  and  x e,  y e,  z e, and  w e are the eye coordinates of the vertex,  p 1,  p 2,  p 3,
%% and  p 4 are the values supplied in  `Params' , and  M is the modelview matrix when ``gl:texGen''
%%  is invoked. If   M is poorly conditioned or singular, texture coordinates generated by
%% the resulting function may be inaccurate or undefined. 
%%
%%  Note that the values in  `Params'  define a reference plane in eye coordinates. The
%% modelview matrix that is applied to them may not be the same one in effect when the polygon
%% vertices are transformed. This function establishes a field of texture coordinates that
%% can produce dynamic contour lines on moving objects. 
%%
%%  If the texture generation function is `?GL_SPHERE_MAP' and  `Coord'  is either `?GL_S'
%%  or `?GL_T',  s and   t texture coordinates are generated as follows. Let `u'
%% be the unit vector pointing from the origin to the polygon vertex (in eye coordinates).
%% Let `n' sup prime be the current normal, after transformation to eye coordinates.
%% Let 
%%
%%  f=(f x  f y  f z) T be the reflection vector such that 
%%
%%  f=u-2  n" (n") T  u
%%
%%  Finally, let   m=2 ((f x) 2+(f y) 2+(f z+1) 2). Then the values assigned to the   s and   t texture coordinates
%% are 
%%
%%  s=f x/m+1/2
%%
%%  t=f y/m+1/2
%%
%%  To enable or disable a texture-coordinate generation function, call  {@link gl:enable/1} 
%% or  {@link gl:enable/1}  with one of the symbolic texture-coordinate names (`?GL_TEXTURE_GEN_S'
%% , `?GL_TEXTURE_GEN_T', `?GL_TEXTURE_GEN_R', or `?GL_TEXTURE_GEN_Q') as
%% the argument. When enabled, the specified texture coordinate is computed according to
%% the generating function associated with that coordinate. When disabled, subsequent vertices
%% take the specified texture coordinate from the current set of texture coordinates. Initially,
%% all texture generation functions are set to `?GL_EYE_LINEAR' and are disabled. Both
%%   s plane equations are (1, 0, 0, 0), both   t plane equations are (0, 1, 0, 0), and all
%%   r and   q plane equations are (0, 0, 0, 0). 
%%
%%  When the ARB_multitexture extension is supported, ``gl:texGen'' sets the texture generation
%% parameters for the currently active texture unit, selected with  {@link gl:activeTexture/1} .
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexGen.xml">external</a> documentation.
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
%%  `Pname'  specifies one of three symbolic names: 
%%
%% `?GL_TEXTURE_GEN_MODE':  `Params'  returns the single-valued texture generation
%% function, a symbolic constant. The initial value is `?GL_EYE_LINEAR'. 
%%
%% `?GL_OBJECT_PLANE':  `Params'  returns the four plane equation coefficients that
%% specify object linear-coordinate generation. Integer values, when requested, are mapped
%% directly from the internal floating-point representation. 
%%
%% `?GL_EYE_PLANE':  `Params'  returns the four plane equation coefficients that
%% specify eye linear-coordinate generation. Integer values, when requested, are mapped directly
%% from the internal floating-point representation. The returned values are those maintained
%% in eye coordinates. They are not equal to the values specified using  {@link gl:texGend/3} ,
%% unless the modelview matrix was identity when  {@link gl:texGend/3}  was called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexGen.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvf.xml">external</a> documentation.
-spec texEnvf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texEnvf(Target,Pname,Param) ->
  cast(5252, <<Target:?GLenum,Pname:?GLenum,Param:?GLfloat>>).

%% @doc glTexEnvi
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnvi.xml">external</a> documentation.
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
%%  If  `Pname'  is `?GL_TEXTURE_ENV_MODE', then  `Params'  is (or points to)
%% the symbolic name of a texture function. Six texture functions may be specified: `?GL_ADD'
%% , `?GL_MODULATE', `?GL_DECAL', `?GL_BLEND', `?GL_REPLACE', or `?GL_COMBINE'
%% . 
%%
%%  The following table shows the correspondence of filtered texture values  R t,  G t,  B t,
%%  A t,  L t,  I t to texture source components.  C s and  A s are used by the texture functions
%% described below. 
%%
%% <table><tbody><tr><td> Texture Base Internal Format </td><td> C s</td><td> A s</td></tr></tbody>
%% <tbody><tr><td>`?GL_ALPHA'</td><td> (0, 0, 0) </td><td> A t</td></tr><tr><td>`?GL_LUMINANCE'
%% </td><td> (  L t,  L t,  L t ) </td><td> 1 </td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td>
%% <td> (  L t,  L t,  L t ) </td><td> A t</td></tr><tr><td>`?GL_INTENSITY'</td><td> ( 
%% I t,  I t,  I t ) </td><td> I t</td></tr><tr><td>`?GL_RGB'</td><td> (  R t,  G t,  B
%% t ) </td><td> 1 </td></tr><tr><td>`?GL_RGBA'</td><td> (  R t,  G t,  B t ) </td><td>
%% A t</td></tr></tbody></table>
%%
%%  A texture function acts on the fragment to be textured using the texture image value
%% that applies to the fragment (see  {@link gl:texParameterf/3} ) and produces an RGBA color
%% for that fragment. The following table shows how the RGBA color is produced for each of
%% the first five texture functions that can be chosen.  C is a triple of color values (RGB)
%% and  A is the associated alpha value. RGBA values extracted from a texture image are in
%% the range [0,1]. The subscript  p refers to the color computed from the previous texture
%% stage (or the incoming fragment if processing texture stage 0), the subscript  s to the
%% texture source color, the subscript  c to the texture environment color, and the subscript
%%  v indicates a value produced by the texture function. 
%%
%% <table><tbody><tr><td> Texture Base Internal Format </td><td>`?Value'</td><td>`?GL_REPLACE'
%%  Function </td><td>`?GL_MODULATE' Function </td><td>`?GL_DECAL' Function </td><td>
%% `?GL_BLEND' Function </td><td>`?GL_ADD' Function </td></tr></tbody><tbody><tr><td>
%% `?GL_ALPHA'</td><td> C v=</td><td> C p</td><td> C p</td><td> undefined </td><td> C p</td>
%% <td> C p</td></tr><tr><td></td><td> A v=</td><td> A s</td><td> A p  A s</td><td></td><td>
%% A v=A p  A s</td><td> A p  A s</td></tr><tr><td>`?GL_LUMINANCE'</td><td> C v=</td><td>
%%  C s</td><td> C p  C s</td><td> undefined </td><td> C p (1-C s)+C c  C s</td><td> C p+C s</td></tr>
%% <tr><td> (or 1) </td><td> A v=</td><td> A p</td><td> A p</td><td></td><td> A p</td><td> A
%% p</td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td> C v=</td><td> C s</td><td> C p  C
%% s</td><td> undefined </td><td> C p (1-C s)+C c  C s</td><td> C p+C s</td></tr><tr><td> (or 2) </td>
%% <td> A v=</td><td> A s</td><td> A p  A s</td><td></td><td> A p  A s</td><td> A p  A s</td>
%% </tr><tr><td>`?GL_INTENSITY'</td><td> C v=</td><td> C s</td><td> C p  C s</td><td>
%% undefined </td><td> C p (1-C s)+C c  C s</td><td> C p+C s</td></tr><tr><td></td><td> A v=</td><td>
%%  A s</td><td> A p  A s</td><td></td><td> A p (1-A s)+A c  A s</td><td> A p+A s</td></tr><tr><td>`?GL_RGB'
%% </td><td> C v=</td><td> C s</td><td> C p  C s</td><td> C s</td><td> C p (1-C s)+C c  C s</td><td>
%%  C p+C s</td></tr><tr><td> (or 3) </td><td> A v=</td><td> A p</td><td> A p</td><td> A p</td>
%% <td> A p</td><td> A p</td></tr><tr><td>`?GL_RGBA'</td><td> C v=</td><td> C s</td><td>
%%  C p  C s</td><td> C p (1-A s)+C s  A s</td><td> C p (1-C s)+C c  C s</td><td> C p+C s</td></tr><tr><td>
%%  (or 4) </td><td> A v=</td><td> A s</td><td> A p  A s</td><td> A p</td><td> A p  A s</td><td>
%%  A p  A s</td></tr></tbody></table>
%%
%%  If  `Pname'  is `?GL_TEXTURE_ENV_MODE', and  `Params'  is `?GL_COMBINE',
%% the form of the texture function depends on the values of `?GL_COMBINE_RGB' and `?GL_COMBINE_ALPHA'
%% . 
%%
%%  The following describes how the texture sources, as specified by `?GL_SRC0_RGB', `?GL_SRC1_RGB'
%% , `?GL_SRC2_RGB', `?GL_SRC0_ALPHA', `?GL_SRC1_ALPHA', and `?GL_SRC2_ALPHA'
%% , are combined to produce a final texture color. In the following tables, `?GL_SRC0_c'
%%  is represented by  Arg0, `?GL_SRC1_c' is represented by  Arg1, and `?GL_SRC2_c'
%% is represented by  Arg2. 
%%
%% `?GL_COMBINE_RGB' accepts any of `?GL_REPLACE', `?GL_MODULATE', `?GL_ADD'
%% , `?GL_ADD_SIGNED', `?GL_INTERPOLATE', `?GL_SUBTRACT', `?GL_DOT3_RGB',
%% or `?GL_DOT3_RGBA'. 
%%
%% <table><tbody><tr><td>`?GL_COMBINE_RGB'</td><td>` Texture Function '</td></tr></tbody>
%% <tbody><tr><td>`?GL_REPLACE'</td><td> Arg0</td></tr><tr><td>`?GL_MODULATE'</td><td>
%%  Arg0×Arg1</td></tr><tr><td>`?GL_ADD'</td><td> Arg0+Arg1</td></tr><tr><td>`?GL_ADD_SIGNED'
%% </td><td> Arg0+Arg1-0.5</td></tr><tr><td>`?GL_INTERPOLATE'</td><td> Arg0×Arg2+Arg1×(1-
%% Arg2)</td>
%% </tr><tr><td>`?GL_SUBTRACT'</td><td> Arg0-Arg1</td></tr><tr><td>`?GL_DOT3_RGB'
%% or `?GL_DOT3_RGBA'</td><td> 4×((((Arg0 r)-0.5)×((Arg1 r)-0.5))+(((Arg0 g)-0.5)×((Arg1 g)-0.5))+(((Arg0 b)-0.5)×((Arg1 b)-0.5)))</td></tr></tbody></table>
%%
%%  The scalar results for `?GL_DOT3_RGB' and `?GL_DOT3_RGBA' are placed into each
%% of the 3 (RGB) or 4 (RGBA) components on output. 
%%
%%  Likewise, `?GL_COMBINE_ALPHA' accepts any of `?GL_REPLACE', `?GL_MODULATE',
%% `?GL_ADD', `?GL_ADD_SIGNED', `?GL_INTERPOLATE', or `?GL_SUBTRACT'.
%% The following table describes how alpha values are combined: 
%%
%% <table><tbody><tr><td>`?GL_COMBINE_ALPHA'</td><td>` Texture Function '</td></tr>
%% </tbody><tbody><tr><td>`?GL_REPLACE'</td><td> Arg0</td></tr><tr><td>`?GL_MODULATE'
%% </td><td> Arg0×Arg1</td></tr><tr><td>`?GL_ADD'</td><td> Arg0+Arg1</td></tr><tr><td>`?GL_ADD_SIGNED'
%% </td><td> Arg0+Arg1-0.5</td></tr><tr><td>`?GL_INTERPOLATE'</td><td> Arg0×Arg2+Arg1×(1-
%% Arg2)</td>
%% </tr><tr><td>`?GL_SUBTRACT'</td><td> Arg0-Arg1</td></tr></tbody></table>
%%
%%  In the following tables, the value  C s represents the color sampled from the currently
%% bound texture,  C c represents the constant texture-environment color,  C f represents
%% the primary color of the incoming fragment, and  C p represents the color computed from
%% the previous texture stage or  C f if processing texture stage 0. Likewise,  A s,  A c, 
%% A f, and  A p represent the respective alpha values. 
%%
%%  The following table describes the values assigned to  Arg0,  Arg1, and  Arg2 based upon
%% the RGB sources and operands: 
%%
%% <table><tbody><tr><td>`?GL_SRCn_RGB'</td><td>`?GL_OPERANDn_RGB'</td><td>` Argument Value '
%% </td></tr></tbody><tbody><tr><td>`?GL_TEXTURE'</td><td>`?GL_SRC_COLOR'</td><td>(C
%%  s)</td>
%% </tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_COLOR'</td><td> 1-(C s)</td></tr><tr><td></td><td>
%% `?GL_SRC_ALPHA'</td><td>(A s)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'</td>
%% <td> 1-(A s)</td></tr><tr><td>`?GL_TEXTUREn'</td><td>`?GL_SRC_COLOR'</td><td>(C s)</td></tr>
%% <tr><td></td><td>`?GL_ONE_MINUS_SRC_COLOR'</td><td> 1-(C s)</td></tr><tr><td></td><td>`?GL_SRC_ALPHA'
%% </td><td>(A s)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A s)</td></tr><tr>
%% <td>`?GL_CONSTANT'</td><td>`?GL_SRC_COLOR'</td><td>(C c)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_COLOR'
%% </td><td> 1-(C c)</td></tr><tr><td></td><td>`?GL_SRC_ALPHA'</td><td>(A c)</td></tr><tr><td></td>
%% <td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A c)</td></tr><tr><td>`?GL_PRIMARY_COLOR'</td>
%% <td>`?GL_SRC_COLOR'</td><td>(C f)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_COLOR'</td>
%% <td> 1-(C f)</td></tr><tr><td></td><td>`?GL_SRC_ALPHA'</td><td>(A f)</td></tr><tr><td></td><td>
%% `?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A f)</td></tr><tr><td>`?GL_PREVIOUS'</td><td>`?GL_SRC_COLOR'
%% </td><td>(C p)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_COLOR'</td><td> 1-(C p)</td></tr><tr>
%% <td></td><td>`?GL_SRC_ALPHA'</td><td>(A p)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'
%% </td><td> 1-(A p)</td></tr></tbody></table>
%%
%%  For `?GL_TEXTUREn' sources,  C s and  A s represent the color and alpha, respectively,
%% produced from texture stage  n. 
%%
%%  The follow table describes the values assigned to  Arg0,  Arg1, and  Arg2 based upon
%% the alpha sources and operands: 
%%
%% <table><tbody><tr><td>`?GL_SRCn_ALPHA'</td><td>`?GL_OPERANDn_ALPHA'</td><td>` Argument Value '
%% </td></tr></tbody><tbody><tr><td>`?GL_TEXTURE'</td><td>`?GL_SRC_ALPHA'</td><td>(A
%%  s)</td>
%% </tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A s)</td></tr><tr><td>`?GL_TEXTUREn'
%% </td><td>`?GL_SRC_ALPHA'</td><td>(A s)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'
%% </td><td> 1-(A s)</td></tr><tr><td>`?GL_CONSTANT'</td><td>`?GL_SRC_ALPHA'</td><td>(A
%% c)</td>
%% </tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A c)</td></tr><tr><td>`?GL_PRIMARY_COLOR'
%% </td><td>`?GL_SRC_ALPHA'</td><td>(A f)</td></tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'
%% </td><td> 1-(A f)</td></tr><tr><td>`?GL_PREVIOUS'</td><td>`?GL_SRC_ALPHA'</td><td>(A
%% p)</td>
%% </tr><tr><td></td><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td> 1-(A p)</td></tr></tbody></table>
%% 
%%
%%  The RGB and alpha results of the texture function are multipled by the values of `?GL_RGB_SCALE'
%%  and `?GL_ALPHA_SCALE', respectively, and clamped to the range [0 1]. 
%%
%%  If  `Pname'  is `?GL_TEXTURE_ENV_COLOR',  `Params'  is a pointer to an array
%% that holds an RGBA color consisting of four values. Integer color components are interpreted
%% linearly such that the most positive integer maps to 1.0, and the most negative integer
%% maps to -1.0. The values are clamped to the range [0,1] when they are specified.  C c
%% takes these four values. 
%%
%%  If  `Pname'  is `?GL_TEXTURE_LOD_BIAS', the value specified is added to the texture
%% level-of-detail parameter, that selects which mipmap, or mipmaps depending upon the selected
%% `?GL_TEXTURE_MIN_FILTER', will be sampled. 
%%
%% `?GL_TEXTURE_ENV_MODE' defaults to `?GL_MODULATE' and `?GL_TEXTURE_ENV_COLOR'
%%  defaults to (0, 0, 0, 0). 
%%
%%  If  `Target'  is `?GL_POINT_SPRITE' and  `Pname'  is `?GL_COORD_REPLACE',
%% the boolean value specified is used to either enable or disable point sprite texture coordinate
%% replacement. The default value is `?GL_FALSE'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexEnv.xml">external</a> documentation.
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
%%  When  `Target'  is `?GL_TEXTURE_FILTER_CONTROL',  `Pname'  must be `?GL_TEXTURE_LOD_BIAS'
%% .  When  `Target'  is `?GL_POINT_SPRITE',  `Pname'  must be `?GL_COORD_REPLACE'
%% . When  `Target'  is `?GL_TEXTURE_ENV',  `Pname'  can be `?GL_TEXTURE_ENV_MODE'
%% , `?GL_TEXTURE_ENV_COLOR', `?GL_COMBINE_RGB', `?GL_COMBINE_ALPHA', `?GL_RGB_SCALE'
%% , `?GL_ALPHA_SCALE',  `?GL_SRC0_RGB', `?GL_SRC1_RGB', `?GL_SRC2_RGB',
%% `?GL_SRC0_ALPHA', `?GL_SRC1_ALPHA', or `?GL_SRC2_ALPHA'. 
%%
%%  `Pname'  names a specific texture environment parameter, as follows: 
%%
%% `?GL_TEXTURE_ENV_MODE':  `Params'  returns the single-valued texture environment
%% mode, a symbolic constant. The initial value is `?GL_MODULATE'. 
%%
%% `?GL_TEXTURE_ENV_COLOR':  `Params'  returns four integer or floating-point values
%% that are the texture environment color. Integer values, when requested, are linearly mapped
%% from the internal floating-point representation such that 1.0 maps to the most positive
%% representable integer, and   -1.0 maps to the most negative representable integer. The
%% initial value is (0, 0, 0, 0). 
%%
%% `?GL_TEXTURE_LOD_BIAS':  `Params'  returns a single floating-point value that
%% is the texture level-of-detail bias. The initial value is 0. 
%%
%% `?GL_COMBINE_RGB':  `Params'  returns a single symbolic constant value representing
%% the current RGB combine mode. The initial value is `?GL_MODULATE'. 
%%
%% `?GL_COMBINE_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the current alpha combine mode. The initial value is `?GL_MODULATE'. 
%%
%% `?GL_SRC0_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner zero's RGB source. The initial value is `?GL_TEXTURE'. 
%%
%% `?GL_SRC1_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner one's RGB source. The initial value is `?GL_PREVIOUS'. 
%%
%% `?GL_SRC2_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner two's RGB source. The initial value is `?GL_CONSTANT'. 
%%
%% `?GL_SRC0_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner zero's alpha source. The initial value is `?GL_TEXTURE'. 
%%
%% `?GL_SRC1_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner one's alpha source. The initial value is `?GL_PREVIOUS'. 
%%
%% `?GL_SRC2_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner two's alpha source. The initial value is `?GL_CONSTANT'. 
%%
%% `?GL_OPERAND0_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner zero's RGB operand. The initial value is `?GL_SRC_COLOR'. 
%%
%% `?GL_OPERAND1_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner one's RGB operand. The initial value is `?GL_SRC_COLOR'. 
%%
%% `?GL_OPERAND2_RGB':  `Params'  returns a single symbolic constant value representing
%% the texture combiner two's RGB operand. The initial value is `?GL_SRC_ALPHA'. 
%%
%% `?GL_OPERAND0_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner zero's alpha operand. The initial value is `?GL_SRC_ALPHA'. 
%%
%% `?GL_OPERAND1_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner one's alpha operand. The initial value is `?GL_SRC_ALPHA'. 
%%
%% `?GL_OPERAND2_ALPHA':  `Params'  returns a single symbolic constant value representing
%% the texture combiner two's alpha operand. The initial value is `?GL_SRC_ALPHA'. 
%%
%% `?GL_RGB_SCALE':  `Params'  returns a single floating-point value representing
%% the current RGB texture combiner scaling factor. The initial value is 1.0. 
%%
%% `?GL_ALPHA_SCALE':  `Params'  returns a single floating-point value representing
%% the current alpha texture combiner scaling factor. The initial value is 1.0. 
%%
%% `?GL_COORD_REPLACE':  `Params'  returns a single boolean value representing the
%% current point sprite texture coordinate replacement enable state. The initial value is `?GL_FALSE'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexEnv.xml">external</a> documentation.
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
%% `?GL_TEXTURE_BASE_LEVEL':  Specifies the index of the lowest defined mipmap level.
%% This is an integer value. The initial value is 0. 
%%
%% 
%%
%% `?GL_TEXTURE_BORDER_COLOR':  The data in  `Params'  specifies four values that
%% define the border values that should be used for border texels. If a texel is sampled
%% from the border of the texture, the values of `?GL_TEXTURE_BORDER_COLOR' are interpreted
%% as an RGBA color to match the texture's internal format and substituted for the non-existent
%% texel data. If the texture contains depth components, the first component of `?GL_TEXTURE_BORDER_COLOR'
%%  is interpreted as a depth value. The initial value is ( 0.0, 0.0, 0.0, 0.0 ). 
%%
%%  If the values for `?GL_TEXTURE_BORDER_COLOR' are specified with ``gl:texParameterIiv''
%%  or ``gl:texParameterIuiv'', the values are stored unmodified with an internal data
%% type of integer. If specified with ``gl:texParameteriv'', they are converted to floating
%% point with the following equation:  f=2 c+1 2 b-/1. If specified with ``gl:texParameterfv''
%% , they are stored unmodified as floating-point values. 
%%
%% `?GL_TEXTURE_COMPARE_FUNC':  Specifies the comparison operator used when `?GL_TEXTURE_COMPARE_MODE'
%%  is set to `?GL_COMPARE_REF_TO_TEXTURE'. Permissible values are: <table><tbody><tr><td>
%% ` Texture Comparison Function '</td><td>` Computed result  '</td></tr></tbody><tbody>
%% <tr><td>`?GL_LEQUAL'</td><td> result={1.0 0.0    r&lt;=(D t) r&gt;(D t))</td></tr><tr><td>`?GL_GEQUAL'</td><td>
%% result={1.0 0.0    r&gt;=(D t) r&lt;(D t))</td></tr><tr><td>`?GL_LESS'</td><td> result={1.0 0.0    r&lt;(D t) r&gt;=(D t))</td></tr><tr><td>`?GL_GREATER'
%% </td><td> result={1.0 0.0    r&gt;(D t) r&lt;=(D t))</td></tr><tr><td>`?GL_EQUAL'</td><td> result={1.0 0.0    r=(D t) r&amp;ne;
%% (D t))</td></tr><tr><td>`?GL_NOTEQUAL'
%% </td><td> result={1.0 0.0    r&amp;ne;(D t) r=(D t))</td></tr><tr><td>`?GL_ALWAYS'</td><td> result=1.0</td></tr><tr><td>
%% `?GL_NEVER'</td><td> result=0.0</td></tr></tbody></table> where  r is the current
%% interpolated texture coordinate, and   D t is the depth texture value sampled from the
%% currently bound depth texture.  result is assigned to the the red channel. 
%%
%% `?GL_TEXTURE_COMPARE_MODE':  Specifies the texture comparison mode for currently
%% bound depth textures. That is, a texture whose internal format is `?GL_DEPTH_COMPONENT_*'
%% ; see  {@link gl:texImage2D/9} ) Permissible values are: 
%%
%% `?GL_COMPARE_REF_TO_TEXTURE':  Specifies that the interpolated and clamped   r texture
%% coordinate should be compared to the value in the currently bound depth texture. See the
%% discussion of `?GL_TEXTURE_COMPARE_FUNC' for details of how the comparison is evaluated.
%% The result of the comparison is assigned to the red channel. 
%%
%% `?GL_NONE':  Specifies that the red channel should be assigned the appropriate value
%% from the currently bound depth texture. 
%%
%% `?GL_TEXTURE_LOD_BIAS':  `Params'  specifies a fixed bias value that is to be
%% added to the level-of-detail parameter for the texture before texture sampling. The specified
%% value is added to the shader-supplied bias value (if any) and subsequently clamped into
%% the implementation-defined range [( -  bias max)(bias max)], where  bias max is the value of the implementation
%% defined constant `?GL_MAX_TEXTURE_LOD_BIAS'. The initial value is 0.0. 
%%
%% `?GL_TEXTURE_MIN_FILTER':  The texture minifying function is used whenever the level-of-detail
%% function used when sampling from the texture determines that the texture should be minified.
%% There are six defined minifying functions. Two of them use either the nearest texture
%% elements or a weighted average of multiple texture elements to compute the texture value.
%% The other four use mipmaps. 
%%
%%  A mipmap is an ordered set of arrays representing the same image at progressively lower
%% resolutions. If the texture has dimensions   2 n×2 m, there are  max(n m)+1 mipmaps. The first
%% mipmap is the original texture, with dimensions   2 n×2 m. Each subsequent mipmap has
%% dimensions   2(k-1)×2(l-1), where   2 k×2 l are the dimensions of the previous mipmap, until either
%%   k=0 or   l=0. At that point, subsequent mipmaps have dimension   1×2(l-1) or   2(k-1)×1 until
%% the final mipmap, which has dimension   1×1. To define the mipmaps, call  {@link gl:texImage1D/8} 
%% ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} ,  {@link gl:copyTexImage1D/7} , or  {@link gl:copyTexImage2D/8} 
%%  with the `level' argument indicating the order of the mipmaps. Level 0 is the original
%% texture; level   max(n m) is the final   1×1 mipmap. 
%%
%%  `Params'  supplies a function for minifying the texture as one of the following: 
%%
%% `?GL_NEAREST':  Returns the value of the texture element that is nearest (in Manhattan
%% distance) to the specified texture coordinates. 
%%
%% `?GL_LINEAR':  Returns the weighted average of the four texture elements that are
%% closest to the specified texture coordinates. These can include items wrapped or repeated
%% from other parts of a texture, depending on the values of `?GL_TEXTURE_WRAP_S' and `?GL_TEXTURE_WRAP_T'
%% , and on the exact mapping. 
%%
%% `?GL_NEAREST_MIPMAP_NEAREST':  Chooses the mipmap that most closely matches the size
%% of the pixel being textured and uses the `?GL_NEAREST' criterion (the texture element
%% closest to the specified texture coordinates) to produce a texture value. 
%%
%% `?GL_LINEAR_MIPMAP_NEAREST':  Chooses the mipmap that most closely matches the size
%% of the pixel being textured and uses the `?GL_LINEAR' criterion (a weighted average
%% of the four texture elements that are closest to the specified texture coordinates) to
%% produce a texture value. 
%%
%% `?GL_NEAREST_MIPMAP_LINEAR':  Chooses the two mipmaps that most closely match the
%% size of the pixel being textured and uses the `?GL_NEAREST' criterion (the texture
%% element closest to the specified texture coordinates ) to produce a texture value from
%% each mipmap. The final texture value is a weighted average of those two values. 
%%
%% `?GL_LINEAR_MIPMAP_LINEAR':  Chooses the two mipmaps that most closely match the
%% size of the pixel being textured and uses the `?GL_LINEAR' criterion (a weighted
%% average of the texture elements that are closest to the specified texture coordinates)
%% to produce a texture value from each mipmap. The final texture value is a weighted average
%% of those two values. 
%%
%%  As more texture elements are sampled in the minification process, fewer aliasing artifacts
%% will be apparent. While the `?GL_NEAREST' and `?GL_LINEAR' minification functions
%% can be faster than the other four, they sample only one or multiple texture elements to
%% determine the texture value of the pixel being rendered and can produce moire patterns
%% or ragged transitions. The initial value of `?GL_TEXTURE_MIN_FILTER' is `?GL_NEAREST_MIPMAP_LINEAR'
%% . 
%%
%% 
%%
%% `?GL_TEXTURE_MAG_FILTER':  The texture magnification function is used whenever the
%% level-of-detail function used when sampling from the texture determines that the texture
%% should be magified. It sets the texture magnification function to either `?GL_NEAREST'
%%  or `?GL_LINEAR' (see below). `?GL_NEAREST' is generally faster than `?GL_LINEAR'
%% , but it can produce textured images with sharper edges because the transition between
%% texture elements is not as smooth. The initial value of `?GL_TEXTURE_MAG_FILTER' is `?GL_LINEAR'
%% . 
%%
%% `?GL_NEAREST':  Returns the value of the texture element that is nearest (in Manhattan
%% distance) to the specified texture coordinates. 
%%
%% `?GL_LINEAR':  Returns the weighted average of the texture elements that are closest
%% to the specified texture coordinates. These can include items wrapped or repeated from
%% other parts of a texture, depending on the values of `?GL_TEXTURE_WRAP_S' and `?GL_TEXTURE_WRAP_T'
%% , and on the exact mapping. 
%%
%% 
%%
%% `?GL_TEXTURE_MIN_LOD':  Sets the minimum level-of-detail parameter. This floating-point
%% value limits the selection of highest resolution mipmap (lowest mipmap level). The initial
%% value is -1000. 
%%
%% 
%%
%% `?GL_TEXTURE_MAX_LOD':  Sets the maximum level-of-detail parameter. This floating-point
%% value limits the selection of the lowest resolution mipmap (highest mipmap level). The
%% initial value is 1000. 
%%
%% 
%%
%% `?GL_TEXTURE_MAX_LEVEL':  Sets the index of the highest defined mipmap level. This
%% is an integer value. The initial value is 1000. 
%%
%% 
%%
%% `?GL_TEXTURE_SWIZZLE_R':  Sets the swizzle that will be applied to the  r component
%% of a texel before it is returned to the shader. Valid values for  `Param'  are `?GL_RED'
%% , `?GL_GREEN', `?GL_BLUE', `?GL_ALPHA', `?GL_ZERO' and `?GL_ONE'.
%% If `?GL_TEXTURE_SWIZZLE_R' is `?GL_RED', the value for  r will be taken from
%% the first channel of the fetched texel. If `?GL_TEXTURE_SWIZZLE_R' is `?GL_GREEN'
%% , the value for  r will be taken from the second channel of the fetched texel. If `?GL_TEXTURE_SWIZZLE_R'
%%  is `?GL_BLUE', the value for  r will be taken from the third channel of the fetched
%% texel. If `?GL_TEXTURE_SWIZZLE_R' is `?GL_ALPHA', the value for  r will be taken
%% from the fourth channel of the fetched texel. If `?GL_TEXTURE_SWIZZLE_R' is `?GL_ZERO'
%% , the value for  r will be subtituted with  0.0. If `?GL_TEXTURE_SWIZZLE_R' is `?GL_ONE'
%% , the value for  r will be subtituted with  1.0. The initial value is `?GL_RED'. 
%%
%% 
%%
%% `?GL_TEXTURE_SWIZZLE_G':  Sets the swizzle that will be applied to the  g component
%% of a texel before it is returned to the shader. Valid values for  `Param'  and their
%% effects are similar to those of `?GL_TEXTURE_SWIZZLE_R'. The initial value is `?GL_GREEN'
%% . 
%%
%% 
%%
%% `?GL_TEXTURE_SWIZZLE_B':  Sets the swizzle that will be applied to the  b component
%% of a texel before it is returned to the shader. Valid values for  `Param'  and their
%% effects are similar to those of `?GL_TEXTURE_SWIZZLE_R'. The initial value is `?GL_BLUE'
%% . 
%%
%% 
%%
%% `?GL_TEXTURE_SWIZZLE_A':  Sets the swizzle that will be applied to the  a component
%% of a texel before it is returned to the shader. Valid values for  `Param'  and their
%% effects are similar to those of `?GL_TEXTURE_SWIZZLE_R'. The initial value is `?GL_ALPHA'
%% . 
%%
%% 
%%
%% `?GL_TEXTURE_SWIZZLE_RGBA':  Sets the swizzles that will be applied to the  r,  g, 
%% b, and  a components of a texel before they are returned to the shader. Valid values for  `Params' 
%%  and their effects are similar to those of `?GL_TEXTURE_SWIZZLE_R', except that all
%% channels are specified simultaneously. Setting the value of `?GL_TEXTURE_SWIZZLE_RGBA'
%%  is equivalent (assuming no errors are generated) to setting the parameters of each of `?GL_TEXTURE_SWIZZLE_R'
%% , `?GL_TEXTURE_SWIZZLE_G', `?GL_TEXTURE_SWIZZLE_B', and `?GL_TEXTURE_SWIZZLE_A'
%%  successively. 
%%
%% 
%%
%% `?GL_TEXTURE_WRAP_S':  Sets the wrap parameter for texture coordinate   s to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_CLAMP_TO_BORDER', `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. `?GL_CLAMP_TO_EDGE'
%%  causes   s coordinates to be clamped to the range  [(1 2/N) 1-(1 2/N)], where   N is the size of the texture
%% in the direction of clamping. `?GL_CLAMP_TO_BORDER' evaluates  s coordinates in a
%% similar manner to `?GL_CLAMP_TO_EDGE'. However, in cases where clamping would have
%% occurred in `?GL_CLAMP_TO_EDGE' mode, the fetched texel data is substituted with
%% the values specified by `?GL_TEXTURE_BORDER_COLOR'. `?GL_REPEAT' causes the
%% integer part of the   s coordinate to be ignored; the GL uses only the fractional part,
%% thereby creating a repeating pattern. `?GL_MIRRORED_REPEAT' causes the   s coordinate
%% to be set to the fractional part of the texture coordinate if the integer part of   s
%% is even; if the integer part of   s is odd, then the   s texture coordinate is set to   1-
%% frac(s), where   frac(s) represents the fractional part of  s. Initially, `?GL_TEXTURE_WRAP_S'
%%  is set to `?GL_REPEAT'. 
%%
%% 
%%
%% `?GL_TEXTURE_WRAP_T':  Sets the wrap parameter for texture coordinate   t to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_CLAMP_TO_BORDER', `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. See the
%% discussion under `?GL_TEXTURE_WRAP_S'. Initially, `?GL_TEXTURE_WRAP_T' is set
%% to `?GL_REPEAT'. 
%%
%% 
%%
%% `?GL_TEXTURE_WRAP_R':  Sets the wrap parameter for texture coordinate   r to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_CLAMP_TO_BORDER', `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. See the
%% discussion under `?GL_TEXTURE_WRAP_S'. Initially, `?GL_TEXTURE_WRAP_R' is set
%% to `?GL_REPEAT'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml">external</a> documentation.
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
%% `?GL_TEXTURE_MAG_FILTER':  Returns the single-valued texture magnification filter,
%% a symbolic constant. The initial value is `?GL_LINEAR'. 
%%
%% `?GL_TEXTURE_MIN_FILTER':  Returns the single-valued texture minification filter,
%% a symbolic constant. The initial value is `?GL_NEAREST_MIPMAP_LINEAR'. 
%%
%% `?GL_TEXTURE_MIN_LOD':  Returns the single-valued texture minimum level-of-detail
%% value. The initial value is   -1000. 
%%
%% `?GL_TEXTURE_MAX_LOD':  Returns the single-valued texture maximum level-of-detail
%% value. The initial value is 1000. 
%%
%% `?GL_TEXTURE_BASE_LEVEL':  Returns the single-valued base texture mipmap level. The
%% initial value is 0. 
%%
%% `?GL_TEXTURE_MAX_LEVEL':  Returns the single-valued maximum texture mipmap array
%% level. The initial value is 1000. 
%%
%% `?GL_TEXTURE_SWIZZLE_R':  Returns the red component swizzle. The initial value is `?GL_RED'
%% . 
%%
%% `?GL_TEXTURE_SWIZZLE_G':  Returns the green component swizzle. The initial value is `?GL_GREEN'
%% . 
%%
%% `?GL_TEXTURE_SWIZZLE_B':  Returns the blue component swizzle. The initial value is `?GL_BLUE'
%% . 
%%
%% `?GL_TEXTURE_SWIZZLE_A':  Returns the alpha component swizzle. The initial value is `?GL_ALPHA'
%% . 
%%
%% `?GL_TEXTURE_SWIZZLE_RGBA':  Returns the component swizzle for all channels in a
%% single query. 
%%
%% `?GL_TEXTURE_WRAP_S':  Returns the single-valued wrapping function for texture coordinate
%%   s, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_WRAP_T':  Returns the single-valued wrapping function for texture coordinate
%%   t, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_WRAP_R':  Returns the single-valued wrapping function for texture coordinate
%%   r, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_BORDER_COLOR':  Returns four integer or floating-point numbers that
%% comprise the RGBA color of the texture border. Floating-point values are returned in the
%% range  [0 1]. Integer values are returned as a linear mapping of the internal floating-point
%% representation such that 1.0 maps to the most positive representable integer and   -1.0
%% maps to the most negative representable integer. The initial value is (0, 0, 0, 0). 
%%
%% `?GL_TEXTURE_COMPARE_MODE':  Returns a single-valued texture comparison mode, a symbolic
%% constant. The initial value is `?GL_NONE'. See  {@link gl:texParameterf/3} . 
%%
%% `?GL_TEXTURE_COMPARE_FUNC':  Returns a single-valued texture comparison function,
%% a symbolic constant. The initial value is `?GL_LEQUAL'. See  {@link gl:texParameterf/3} .
%% 
%%
%%  In addition to the parameters that may be set with  {@link gl:texParameterf/3} , ``gl:getTexParameter''
%%  accepts the following read-only parameters: 
%%
%% `?GL_TEXTURE_IMMUTABLE_FORMAT':  Returns non-zero if the texture has an immutable
%% format. Textures become immutable if their storage is specified with  {@link gl:texStorage1D/4} 
%% ,  {@link gl:texStorage2D/5}  or  {@link gl:texStorage3D/6} . The initial value is `?GL_FALSE'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameter.xml">external</a> documentation.
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
%% `?GL_MAX_TEXTURE_SIZE', and `?GL_MAX_3D_TEXTURE_SIZE' are not really descriptive
%% enough. It has to report the largest square texture image that can be accommodated with
%% mipmaps and borders, but a long skinny texture, or a texture without mipmaps and borders,
%% may easily fit in texture memory. The proxy targets allow the user to more accurately
%% query whether the GL can accommodate a texture of a given configuration. If the texture
%% cannot be accommodated, the texture state variables, which may be queried with ``gl:getTexLevelParameter''
%% , are set to 0. If the texture can be accommodated, the texture state values will be set
%% as they would be set for a non-proxy target. 
%%
%%  `Pname'  specifies the texture parameter whose value or values will be returned. 
%%
%%  The accepted parameter names are as follows: 
%%
%% `?GL_TEXTURE_WIDTH':  `Params'  returns a single value, the width of the texture
%% image. This value includes the border of the texture image. The initial value is 0. 
%%
%% `?GL_TEXTURE_HEIGHT':  `Params'  returns a single value, the height of the texture
%% image. This value includes the border of the texture image. The initial value is 0. 
%%
%% `?GL_TEXTURE_DEPTH':  `Params'  returns a single value, the depth of the texture
%% image. This value includes the border of the texture image. The initial value is 0. 
%%
%% `?GL_TEXTURE_INTERNAL_FORMAT':  `Params'  returns a single value, the internal
%% format of the texture image. 
%%
%% `?GL_TEXTURE_RED_TYPE',
%%
%% `?GL_TEXTURE_GREEN_TYPE',
%%
%% `?GL_TEXTURE_BLUE_TYPE',
%%
%% `?GL_TEXTURE_ALPHA_TYPE',
%%
%% `?GL_TEXTURE_DEPTH_TYPE':  The data type used to store the component. The types `?GL_NONE'
%% , `?GL_SIGNED_NORMALIZED', `?GL_UNSIGNED_NORMALIZED', `?GL_FLOAT', `?GL_INT'
%% , and `?GL_UNSIGNED_INT' may be returned to indicate signed normalized fixed-point,
%% unsigned normalized fixed-point, floating-point, integer unnormalized, and unsigned integer
%% unnormalized components, respectively. 
%%
%% `?GL_TEXTURE_RED_SIZE',
%%
%% `?GL_TEXTURE_GREEN_SIZE',
%%
%% `?GL_TEXTURE_BLUE_SIZE',
%%
%% `?GL_TEXTURE_ALPHA_SIZE',
%%
%% `?GL_TEXTURE_DEPTH_SIZE':  The internal storage resolution of an individual component.
%% The resolution chosen by the GL will be a close match for the resolution requested by
%% the user with the component argument of  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} 
%% ,  {@link gl:copyTexImage1D/7} , and  {@link gl:copyTexImage2D/8} . The initial value is 0. 
%%
%% `?GL_TEXTURE_COMPRESSED':  `Params'  returns a single boolean value indicating
%% if the texture image is stored in a compressed internal format. The initiali value is `?GL_FALSE'
%% . 
%%
%% `?GL_TEXTURE_COMPRESSED_IMAGE_SIZE':  `Params'  returns a single integer value,
%% the number of unsigned bytes of the compressed texture image that would be returned from  {@link gl:getCompressedTexImage/3} 
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexLevelParameter.xml">external</a> documentation.
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
%%  Texture images are defined with ``gl:texImage1D''. The arguments describe the parameters
%% of the texture image, such as width, width of the border, level-of-detail number (see  {@link gl:texParameterf/3} 
%% ), and the internal resolution and format used to store the image. The last three arguments
%% describe how the image is represented in memory. 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_1D', no data is read from  `Data' , but
%% all of the texture image state is recalculated, checked for consistency, and checked against
%% the implementation's capabilities. If the implementation cannot handle a texture of the
%% requested texture size, it sets all of the image state to 0, but does not generate an
%% error (see  {@link gl:getError/0} ). To query for an entire mipmap array, use an image array
%% level greater than or equal to 1. 
%%
%%  If  `Target'  is `?GL_TEXTURE_1D', data is read from  `Data'  as a sequence
%% of signed or unsigned bytes, shorts, or longs, or single-precision floating-point values,
%% depending on  `Type' . These values are grouped into sets of one, two, three, or four
%% values, depending on  `Format' , to form elements. Each data byte is treated as eight
%% 1-bit elements, with bit ordering determined by `?GL_UNPACK_LSB_FIRST' (see  {@link gl:pixelStoref/2} 
%% ). 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  The first element corresponds to the left end of the texture array. Subsequent elements
%% progress left-to-right through the remaining texels in the texture array. The final element
%% corresponds to the right end of the texture array. 
%%
%%  `Format'  determines the composition of each element in  `Data' . It can assume
%% one of these symbolic values: 
%%
%% `?GL_RED':  Each element is a single red component. The GL converts it to floating
%% point and assembles it into an RGBA element by attaching 0 for green and blue, and 1 for
%% alpha. Each component is then multiplied by the signed scale factor `?GL_c_SCALE',
%% added to the signed bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RG':  Each element is a single red/green double The GL converts it to floating
%% point and assembles it into an RGBA element by attaching 0 for blue, and 1 for alpha.
%% Each component is then multiplied by the signed scale factor `?GL_c_SCALE', added
%% to the signed bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RGB'
%%
%% `?GL_BGR':  Each element is an RGB triple. The GL converts it to floating point and
%% assembles it into an RGBA element by attaching 1 for alpha. Each component is then multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%% `?GL_RGBA'
%%
%% `?GL_BGRA':  Each element contains all four components. Each component is multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%% `?GL_DEPTH_COMPONENT':  Each element is a single depth value. The GL converts it
%% to floating point, multiplies by the signed scale factor `?GL_DEPTH_SCALE', adds
%% the signed bias `?GL_DEPTH_BIAS', and clamps to the range [0,1]. 
%%
%%  If an application wants to store the texture at a certain resolution or in a certain
%% format, it can request the resolution and format with  `InternalFormat' . The GL will
%% choose an internal representation that closely approximates that requested by  `InternalFormat' 
%% , but it may not match exactly. (The representations specified by `?GL_RED', `?GL_RG'
%% , `?GL_RGB' and `?GL_RGBA' must match exactly.) 
%%
%%  `InternalFormat'  may be one of the base internal formats shown in Table 1, below 
%%
%%  `InternalFormat'  may also be one of the sized internal formats shown in Table 2,
%% below 
%%
%%  Finally,  `InternalFormat'  may also be one of the generic or compressed compressed
%% texture formats shown in Table 3 below 
%%
%%   If the  `InternalFormat'  parameter is one of the generic compressed formats,  `?GL_COMPRESSED_RED'
%% , `?GL_COMPRESSED_RG',  `?GL_COMPRESSED_RGB', or  `?GL_COMPRESSED_RGBA',
%% the GL will replace the internal format with the symbolic constant for a specific internal
%% format and compress the texture before storage. If no corresponding internal format is
%% available, or the GL can not compress that image for any reason, the internal format is
%% instead replaced with a corresponding base internal format. 
%%
%%  If the  `InternalFormat'  parameter is     `?GL_SRGB',    `?GL_SRGB8',    `?GL_SRGB_ALPHA'
%% or    `?GL_SRGB8_ALPHA8', the texture is treated as if the red, green, or blue components
%% are encoded in the sRGB color space. Any alpha component is left unchanged. The conversion
%% from the sRGB encoded component      c s    to a linear component      c l is:   
%%
%%  c l={ c s/12.92if c s&amp;le; 0.04045( c s+0.055/1.055) 2.4if c s&gt; 0.04045
%%
%%     Assume        c s    is the sRGB component in the range [0,1]. 
%%
%%  Use the `?GL_PROXY_TEXTURE_1D' target to try out a resolution and format. The implementation
%% will update and recompute its best match for the requested storage resolution and format.
%% To then query this state, call  {@link gl:getTexLevelParameterfv/3} . If the texture cannot
%% be accommodated, texture state is set to 0. 
%%
%%  A one-component texture image uses only the red component of the RGBA color from  `Data' 
%% . A two-component image uses the R and A values. A three-component image uses the R, G,
%% and B values. A four-component image uses all of the RGBA components. 
%%
%%  Image-based shadowing can be enabled by comparing texture r coordinates to depth texture
%% values to generate a boolean result. See  {@link gl:texParameterf/3}  for details on texture
%% comparison. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage1D.xml">external</a> documentation.
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
%%  To define texture images, call ``gl:texImage2D''. The arguments describe the parameters
%% of the texture image, such as height, width, width of the border, level-of-detail number
%% (see  {@link gl:texParameterf/3} ), and number of color components provided. The last three
%% arguments describe how the image is represented in memory. 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_2D', `?GL_PROXY_TEXTURE_1D_ARRAY', `?GL_PROXY_TEXTURE_CUBE_MAP'
%% , or `?GL_PROXY_TEXTURE_RECTANGLE', no data is read from  `Data' , but all of
%% the texture image state is recalculated, checked for consistency, and checked against
%% the implementation's capabilities. If the implementation cannot handle a texture of the
%% requested texture size, it sets all of the image state to 0, but does not generate an
%% error (see  {@link gl:getError/0} ). To query for an entire mipmap array, use an image array
%% level greater than or equal to 1. 
%%
%%  If  `Target'  is `?GL_TEXTURE_2D', `?GL_TEXTURE_RECTANGLE' or one of the `?GL_TEXTURE_CUBE_MAP'
%%  targets, data is read from  `Data'  as a sequence of signed or unsigned bytes, shorts,
%% or longs, or single-precision floating-point values, depending on  `Type' . These values
%% are grouped into sets of one, two, three, or four values, depending on  `Format' ,
%% to form elements. Each data byte is treated as eight 1-bit elements, with bit ordering
%% determined by `?GL_UNPACK_LSB_FIRST' (see  {@link gl:pixelStoref/2} ). 
%%
%%  If  `Target'  is `?GL_TEXTURE_1D_ARRAY', data is interpreted as an array of one-dimensional
%% images. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  The first element corresponds to the lower left corner of the texture image. Subsequent
%% elements progress left-to-right through the remaining texels in the lowest row of the
%% texture image, and then in successively higher rows of the texture image. The final element
%% corresponds to the upper right corner of the texture image. 
%%
%%  `Format'  determines the composition of each element in  `Data' . It can assume
%% one of these symbolic values: 
%%
%% `?GL_RED':  Each element is a single red component. The GL converts it to floating
%% point and assembles it into an RGBA element by attaching 0 for green and blue, and 1 for
%% alpha. Each component is then multiplied by the signed scale factor `?GL_c_SCALE',
%% added to the signed bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RG':  Each element is a red/green double. The GL converts it to floating point
%% and assembles it into an RGBA element by attaching 0 for blue, and 1 for alpha. Each component
%% is then multiplied by the signed scale factor `?GL_c_SCALE', added to the signed
%% bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RGB'
%%
%% `?GL_BGR':  Each element is an RGB triple. The GL converts it to floating point and
%% assembles it into an RGBA element by attaching 1 for alpha. Each component is then multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%% `?GL_RGBA'
%%
%% `?GL_BGRA':  Each element contains all four components. Each component is multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%% `?GL_DEPTH_COMPONENT':  Each element is a single depth value. The GL converts it
%% to floating point, multiplies by the signed scale factor `?GL_DEPTH_SCALE', adds
%% the signed bias `?GL_DEPTH_BIAS', and clamps to the range [0,1]. 
%%
%% `?GL_DEPTH_STENCIL':  Each element is a pair of depth and stencil values. The depth
%% component of the pair is interpreted as in `?GL_DEPTH_COMPONENT'. The stencil component
%% is interpreted based on specified the depth + stencil internal format. 
%%
%%  If an application wants to store the texture at a certain resolution or in a certain
%% format, it can request the resolution and format with  `InternalFormat' . The GL will
%% choose an internal representation that closely approximates that requested by  `InternalFormat' 
%% , but it may not match exactly. (The representations specified by `?GL_RED', `?GL_RG'
%% , `?GL_RGB', and `?GL_RGBA' must match exactly.) 
%%
%%  `InternalFormat'  may be one of the base internal formats shown in Table 1, below 
%%
%%  `InternalFormat'  may also be one of the sized internal formats shown in Table 2,
%% below 
%%
%%  Finally,  `InternalFormat'  may also be one of the generic or compressed compressed
%% texture formats shown in Table 3 below 
%%
%%  If the  `InternalFormat'  parameter is one of the generic compressed formats, `?GL_COMPRESSED_RED'
%% , `?GL_COMPRESSED_RG', `?GL_COMPRESSED_RGB', or `?GL_COMPRESSED_RGBA',
%% the GL will replace the internal format with the symbolic constant for a specific internal
%% format and compress the texture before storage. If no corresponding internal format is
%% available, or the GL can not compress that image for any reason, the internal format is
%% instead replaced with a corresponding base internal format. 
%%
%%  If the  `InternalFormat'  parameter is  `?GL_SRGB', `?GL_SRGB8', `?GL_SRGB_ALPHA'
%% , or `?GL_SRGB8_ALPHA8', the texture is treated as if the red, green, or blue components
%% are encoded in the sRGB color space. Any alpha component is left unchanged. The conversion
%% from the sRGB encoded component   c s to a linear component   c l is: 
%%
%%  c l={ c s/12.92if c s&amp;le; 0.04045( c s+0.055/1.055) 2.4if c s&gt; 0.04045
%%
%%  Assume   c s is the sRGB component in the range [0,1]. 
%%
%%  Use the `?GL_PROXY_TEXTURE_2D', `?GL_PROXY_TEXTURE_1D_ARRAY', `?GL_PROXY_TEXTURE_RECTANGLE'
%% , or `?GL_PROXY_TEXTURE_CUBE_MAP' target to try out a resolution and format. The
%% implementation will update and recompute its best match for the requested storage resolution
%% and format. To then query this state, call  {@link gl:getTexLevelParameterfv/3} . If the texture
%% cannot be accommodated, texture state is set to 0. 
%%
%%  A one-component texture image uses only the red component of the RGBA color extracted
%% from  `Data' . A two-component image uses the R and G values. A three-component image
%% uses the R, G, and B values. A four-component image uses all of the RGBA components. 
%%
%%  Image-based shadowing can be enabled by comparing texture r coordinates to depth texture
%% values to generate a boolean result. See  {@link gl:texParameterf/3}  for details on texture
%% comparison. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2D.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is requested,  `Img'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  To understand the operation of ``gl:getTexImage'', consider the selected internal four-component
%% texture image to be an RGBA color buffer the size of the image. The semantics of ``gl:getTexImage''
%%  are then identical to those of  {@link gl:readPixels/7} , with the exception that no pixel
%% transfer operations are performed, when called with the same  `Format'  and  `Type' ,
%% with `x' and `y' set to 0, `width' set to the width of the texture image
%% and `height' set to 1 for 1D images, or to the height of the texture image for 2D
%% images. 
%%
%%  If the selected texture image does not contain four components, the following mappings
%% are applied. Single-component textures are treated as RGBA buffers with red set to the
%% single-component value, green set to 0, blue set to 0, and alpha set to 1. Two-component
%% textures are treated as RGBA buffers with red set to the value of component zero, alpha
%% set to the value of component one, and green and blue set to 0. Finally, three-component
%% textures are treated as RGBA buffers with red set to component zero, green set to component
%% one, blue set to component two, and alpha set to 1. 
%%
%%  To determine the required size of  `Img' , use  {@link gl:getTexLevelParameterfv/3}  to
%% determine the dimensions of the internal texture image, then scale the required number
%% of pixels by the storage required for each pixel, based on  `Format'  and  `Type' .
%% Be sure to take the pixel storage parameters into account, especially `?GL_PACK_ALIGNMENT'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexImage.xml">external</a> documentation.
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
%%  The generated textures have no dimensionality; they assume the dimensionality of the
%% texture target to which they are first bound (see  {@link gl:bindTexture/2} ). 
%%
%%  Texture names returned by a call to ``gl:genTextures'' are not returned by subsequent
%% calls, unless they are first deleted with  {@link gl:deleteTextures/1} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenTextures.xml">external</a> documentation.
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
%% ``gl:deleteTextures'' silently ignores 0's and names that do not correspond to existing
%% textures. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTextures.xml">external</a> documentation.
-spec deleteTextures(Textures) -> 'ok' when Textures :: [integer()].
deleteTextures(Textures) ->
  cast(5272, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

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
%%  Texture names are unsigned integers. The value zero is reserved to represent the default
%% texture for each texture target. Texture names and the corresponding texture contents
%% are local to the shared object space of the current GL rendering context; two rendering
%% contexts share texture names only if they explicitly enable sharing between contexts through
%% the appropriate GL windows interfaces functions. 
%%
%%  You must use  {@link gl:genTextures/1}  to generate a set of new texture names. 
%%
%%  When a texture is first bound, it assumes the specified target: A texture first bound
%% to `?GL_TEXTURE_1D' becomes one-dimensional texture, a texture first bound to `?GL_TEXTURE_2D'
%%  becomes two-dimensional texture, a texture first bound to `?GL_TEXTURE_3D' becomes
%% three-dimensional texture, a texture first bound to `?GL_TEXTURE_1D_ARRAY' becomes
%% one-dimensional array texture, a texture first bound to `?GL_TEXTURE_2D_ARRAY' becomes
%% two-dimensional arary texture, a texture first bound to `?GL_TEXTURE_RECTANGLE' becomes
%% rectangle texture, a, texture first bound to `?GL_TEXTURE_CUBE_MAP' becomes a cube-mapped
%% texture, a texture first bound to `?GL_TEXTURE_2D_MULTISAMPLE' becomes a two-dimensional
%% multisampled texture, and a texture first bound to `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY'
%% becomes a two-dimensional multisampled array texture. The state of a one-dimensional texture
%% immediately after it is first bound is equivalent to the state of the default `?GL_TEXTURE_1D'
%%  at GL initialization, and similarly for the other texture types. 
%%
%%  While a texture is bound, GL operations on the target to which it is bound affect the
%% bound texture, and queries of the target to which it is bound return state from the bound
%% texture.  In effect, the texture targets become aliases for the textures currently bound
%% to them, and the texture name zero refers to the default textures that were bound to them
%% at initialization. 
%%
%%  A texture binding created with ``gl:bindTexture'' remains active until a different
%% texture is bound to the same target, or until the bound texture is deleted with  {@link gl:deleteTextures/1} 
%% . 
%%
%%  Once created, a named texture may be re-bound to its same original target as often as
%% needed. It is usually much faster to use ``gl:bindTexture'' to bind an existing named
%% texture to one of the texture targets than it is to reload the texture image using  {@link gl:texImage1D/8} 
%% ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10}  or another similar function. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindTexture.xml">external</a> documentation.
-spec bindTexture(Target, Texture) -> 'ok' when Target :: enum(),Texture :: integer().
bindTexture(Target,Texture) ->
  cast(5273, <<Target:?GLenum,Texture:?GLuint>>).

%% @doc Set texture residence priority
%%
%% ``gl:prioritizeTextures'' assigns the  `N'  texture priorities given in  `Priorities' 
%%  to the  `N'  textures named in  `Textures' . 
%%
%%  The GL establishes a ``working set'' of textures that are resident in texture memory.
%% These textures may be bound to a texture target much more efficiently than textures that
%% are not resident. By specifying a priority for each texture, ``gl:prioritizeTextures''
%% allows applications to guide the GL implementation in determining which textures should
%% be resident. 
%%
%%  The priorities given in  `Priorities'  are clamped to the range  [0 1] before they are
%% assigned. 0 indicates the lowest priority; textures with priority 0 are least likely to
%% be resident. 1 indicates the highest priority; textures with priority 1 are most likely
%% to be resident. However, textures are not guaranteed to be resident until they are used. 
%%
%% ``gl:prioritizeTextures'' silently ignores attempts to prioritize texture 0 or any texture
%% name that does not correspond to an existing texture. 
%%
%% ``gl:prioritizeTextures'' does not require that any of the textures named by  `Textures' 
%%  be bound to a texture target.  {@link gl:texParameterf/3}  may also be used to set a texture's
%% priority, but only if the texture is currently bound. This is the only way to set the
%% priority of a default texture. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrioritizeTextures.xml">external</a> documentation.
-spec prioritizeTextures(Textures, Priorities) -> 'ok' when Textures :: [integer()],Priorities :: [clamp()].
prioritizeTextures(Textures,Priorities) ->
  cast(5274, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32),(length(Priorities)):?GLuint,
        (<< <<C:?GLclampf>> || C <- Priorities>>)/binary,0:(((1+length(Priorities)) rem 2)*32)>>).

%% @doc Determine if textures are loaded in texture memory
%%
%%  GL establishes a ``working set'' of textures that are resident in texture memory. These
%% textures can be bound to a texture target much more efficiently than textures that are
%% not resident. 
%%
%% ``gl:areTexturesResident'' queries the texture residence status of the  `N'  textures
%% named by the elements of  `Textures' . If all the named textures are resident, ``gl:areTexturesResident''
%%  returns `?GL_TRUE', and the contents of  `Residences'  are undisturbed. If not
%% all the named textures are resident, ``gl:areTexturesResident'' returns `?GL_FALSE',
%% and detailed status is returned in the  `N'  elements of  `Residences' . If an element
%% of  `Residences'  is `?GL_TRUE', then the texture named by the corresponding element
%% of  `Textures'  is resident. 
%%
%%  The residence status of a single bound texture may also be queried by calling  {@link gl:getTexParameterfv/2} 
%%  with the `target' argument set to the target to which the texture is bound, and
%% the `pname' argument set to `?GL_TEXTURE_RESIDENT'. This is the only way that
%% the residence status of a default texture can be queried. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAreTexturesResident.xml">external</a> documentation.
-spec areTexturesResident(Textures) -> {0|1,Residences :: [0|1]} when Textures :: [integer()].
areTexturesResident(Textures) ->
  call(5275, <<(length(Textures)):?GLuint,
        (<< <<C:?GLuint>> || C <- Textures>>)/binary,0:(((1+length(Textures)) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a texture
%%
%% ``gl:isTexture'' returns `?GL_TRUE' if  `Texture'  is currently the name of
%% a texture. If  `Texture'  is zero, or is a non-zero value that is not currently the
%% name of a texture, or if an error occurs, ``gl:isTexture'' returns `?GL_FALSE'. 
%%
%%  A name returned by  {@link gl:genTextures/1} , but not yet associated with a texture by
%% calling  {@link gl:bindTexture/2} , is not the name of a texture. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsTexture.xml">external</a> documentation.
-spec isTexture(Texture) -> 0|1 when Texture :: integer().
isTexture(Texture) ->
  call(5276, <<Texture:?GLuint>>).

%% @doc glTexSubImage
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage.xml">external</a> documentation.
-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5277, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5278, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @doc glTexSubImage
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage.xml">external</a> documentation.
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
%%  The screen-aligned pixel row with left corner at  (x y) and with a length of   width+2(border) defines
%% the texture array at the mipmap level specified by  `Level' .  `Internalformat' 
%% specifies the internal format of the texture array. 
%%
%%  The pixels in the row are processed exactly as if  {@link gl:readPixels/7}  had been called,
%% but the process stops just before final conversion. At this point all pixel component
%% values are clamped to the range  [0 1] and then converted to the texture's internal format
%% for storage in the texel array. 
%%
%%  Pixel ordering is such that lower   x screen coordinates correspond to lower texture
%% coordinates. 
%%
%%  If any of the pixels within the specified row of the current `?GL_READ_BUFFER' are
%% outside the window associated with the current rendering context, then the values obtained
%% for those pixels are undefined. 
%%
%% ``gl:copyTexImage1D'' defines a one-dimensional texture image with pixels from the current
%% `?GL_READ_BUFFER'. 
%%
%%  When  `Internalformat'  is one of the sRGB types, the GL does not automatically convert
%% the source pixels to the sRGB color space. In this case, the ``gl:pixelMap'' function
%% can be used to accomplish the conversion. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage1D.xml">external</a> documentation.
-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Border :: integer().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) ->
  cast(5281, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Border:?GLint>>).

%% @doc Copy pixels into a 2D texture image
%%
%% ``gl:copyTexImage2D'' defines a two-dimensional texture image, or cube-map texture image
%% with pixels from the current `?GL_READ_BUFFER'. 
%%
%%  The screen-aligned pixel rectangle with lower left corner at ( `X' ,  `Y' ) and
%% with a width of   width+2(border) and a height of  height+2(border) defines the texture array at the mipmap
%% level specified by  `Level' .  `Internalformat'  specifies the internal format of
%% the texture array. 
%%
%%  The pixels in the rectangle are processed exactly as if  {@link gl:readPixels/7}  had been
%% called, but the process stops just before final conversion. At this point all pixel component
%% values are clamped to the range  [0 1] and then converted to the texture's internal format
%% for storage in the texel array. 
%%
%%  Pixel ordering is such that lower   x and   y screen coordinates correspond to lower   s
%% and   t texture coordinates. 
%%
%%  If any of the pixels within the specified rectangle of the current `?GL_READ_BUFFER'
%% are outside the window associated with the current rendering context, then the values
%% obtained for those pixels are undefined. 
%%
%%  When  `Internalformat'  is one of the sRGB types, the GL does not automatically convert
%% the source pixels to the sRGB color space. In this case, the ``gl:pixelMap'' function
%% can be used to accomplish the conversion. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexImage2D.xml">external</a> documentation.
-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Border :: integer().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) ->
  cast(5282, <<Target:?GLenum,Level:?GLint,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei,Border:?GLint>>).

%% @doc Copy a one-dimensional texture subimage
%%
%% ``gl:copyTexSubImage1D'' replaces a portion of a one-dimensional texture image with
%% pixels from the current `?GL_READ_BUFFER' (rather than from main memory, as is the
%% case for  {@link gl:texSubImage1D/7} ). 
%%
%%  The screen-aligned pixel row with left corner at ( `X' ,  `Y' ), and with length  `Width' 
%%  replaces the portion of the texture array with x indices  `Xoffset'  through   xoffset
%% +width-1, inclusive. The destination in the texture array may not include any texels outside
%% the texture array as it was originally specified. 
%%
%%  The pixels in the row are processed exactly as if  {@link gl:readPixels/7}  had been called,
%% but the process stops just before final conversion. At this point, all pixel component
%% values are clamped to the range  [0 1] and then converted to the texture's internal format
%% for storage in the texel array. 
%%
%%  It is not an error to specify a subtexture with zero width, but such a specification
%% has no effect. If any of the pixels within the specified row of the current `?GL_READ_BUFFER'
%%  are outside the read window associated with the current rendering context, then the values
%% obtained for those pixels are undefined. 
%%
%%  No change is made to the `internalformat', `width', or `border' parameters
%% of the specified texture array or to texel values outside the specified subregion. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage1D.xml">external</a> documentation.
-spec copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) ->
  cast(5283, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Copy a two-dimensional texture subimage
%%
%% ``gl:copyTexSubImage2D'' replaces a rectangular portion of a two-dimensional texture
%% image or cube-map texture image with pixels from the current `?GL_READ_BUFFER' (rather
%% than from main memory, as is the case for  {@link gl:texSubImage1D/7} ). 
%%
%%  The screen-aligned pixel rectangle with lower left corner at (x y) and with width  `Width' 
%% and height  `Height'  replaces the portion of the texture array with x indices  `Xoffset' 
%%  through   xoffset+width-1, inclusive, and y indices  `Yoffset'  through   yoffset+height
%% -1, inclusive, at the mipmap level specified by  `Level' . 
%%
%%  The pixels in the rectangle are processed exactly as if  {@link gl:readPixels/7}  had been
%% called, but the process stops just before final conversion. At this point, all pixel component
%% values are clamped to the range  [0 1] and then converted to the texture's internal format
%% for storage in the texel array. 
%%
%%  The destination rectangle in the texture array may not include any texels outside the
%% texture array as it was originally specified. It is not an error to specify a subtexture
%% with zero width or height, but such a specification has no effect. 
%%
%%  If any of the pixels within the specified rectangle of the current `?GL_READ_BUFFER'
%% are outside the read window associated with the current rendering context, then the values
%% obtained for those pixels are undefined. 
%%
%%  No change is made to the `internalformat', `width', `height', or `border'
%%  parameters of the specified texture array or to texel values outside the specified subregion.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage2D.xml">external</a> documentation.
-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) ->
  cast(5284, <<Target:?GLenum,Level:?GLint,Xoffset:?GLint,Yoffset:?GLint,X:?GLint,Y:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc glMap
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map1d(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1d(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5285, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Stride:?GLint,Order:?GLint>>).

%% @doc glMap
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map1f(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1f(Target,U1,U2,Stride,Order,Points) ->
  send_bin(Points),
  cast(5286, <<Target:?GLenum,U1:?GLfloat,U2:?GLfloat,Stride:?GLint,Order:?GLint>>).

%% @doc glMap
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) ->
  send_bin(Points),
  cast(5287, <<Target:?GLenum,0:32,U1:?GLdouble,U2:?GLdouble,Ustride:?GLint,Uorder:?GLint,V1:?GLdouble,V2:?GLdouble,Vstride:?GLint,Vorder:?GLint>>).

%% @doc glMap
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMap.xml">external</a> documentation.
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
%%  The acceptable values for the  `Target'  parameter are described in the  {@link gl:map1d/6} 
%%  and  {@link gl:map1d/6}  reference pages. 
%%
%%  `Query'  can assume the following values: 
%%
%% `?GL_COEFF':  `V'  returns the control points for the evaluator function. One-dimensional
%% evaluators return   order control points, and two-dimensional evaluators return   uorder×vorder
%%  control points. Each control point consists of one, two, three, or four integer, single-precision
%% floating-point, or double-precision floating-point values, depending on the type of the
%% evaluator. The GL returns two-dimensional control points in row-major order, incrementing
%% the   uorder index quickly and the   vorder index after each row. Integer values, when
%% requested, are computed by rounding the internal floating-point values to the nearest
%% integer values. 
%%
%% `?GL_ORDER':  `V'  returns the order of the evaluator function. One-dimensional
%% evaluators return a single value,  order. The initial value is 1. Two-dimensional evaluators
%% return two values,  uorder and   vorder. The initial value is 1,1. 
%%
%% `?GL_DOMAIN':  `V'  returns the linear   u and   v mapping parameters. One-dimensional
%% evaluators return two values,  u1 and   u2, as specified by  {@link gl:map1d/6} . Two-dimensional
%% evaluators return four values ( u1,   u2,   v1, and   v2) as specified by  {@link gl:map1d/6} .
%% Integer values, when requested, are computed by rounding the internal floating-point values
%% to the nearest integer values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMap.xml">external</a> documentation.
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
%%  When one of the ``gl:evalCoord'' commands is issued, all currently enabled maps of
%% the indicated dimension are evaluated. Then, for each enabled map, it is as if the corresponding
%% GL command had been issued with the computed value. That is, if `?GL_MAP1_INDEX' or `?GL_MAP2_INDEX'
%%  is enabled, a  {@link gl:indexd/1}  command is simulated. If `?GL_MAP1_COLOR_4' or `?GL_MAP2_COLOR_4'
%%  is enabled, a  {@link gl:color3b/3}  command is simulated. If `?GL_MAP1_NORMAL' or `?GL_MAP2_NORMAL'
%%  is enabled, a normal vector is produced, and if any of `?GL_MAP1_TEXTURE_COORD_1', `?GL_MAP1_TEXTURE_COORD_2'
%% , `?GL_MAP1_TEXTURE_COORD_3', `?GL_MAP1_TEXTURE_COORD_4', `?GL_MAP2_TEXTURE_COORD_1'
%% , `?GL_MAP2_TEXTURE_COORD_2', `?GL_MAP2_TEXTURE_COORD_3', or `?GL_MAP2_TEXTURE_COORD_4'
%%  is enabled, then an appropriate  {@link gl:texCoord1d/1}  command is simulated. 
%%
%%  For color, color index, normal, and texture coordinates the GL uses evaluated values
%% instead of current values for those evaluations that are enabled, and current values otherwise,
%% However, the evaluated values do not update the current values. Thus, if  {@link gl:vertex2d/2} 
%%  commands are interspersed with ``gl:evalCoord'' commands, the color, normal, and texture
%% coordinates associated with the  {@link gl:vertex2d/2}  commands are not affected by the values
%% generated by the ``gl:evalCoord'' commands, but only by the most recent  {@link gl:color3b/3} 
%% ,  {@link gl:indexd/1} ,  {@link gl:normal3b/3} , and  {@link gl:texCoord1d/1}  commands. 
%%
%%  No commands are issued for maps that are not enabled. If more than one texture evaluation
%% is enabled for a particular dimension (for example, `?GL_MAP2_TEXTURE_COORD_1' and `?GL_MAP2_TEXTURE_COORD_2'
%% ), then only the evaluation of the map that produces the larger number of coordinates
%% (in this case, `?GL_MAP2_TEXTURE_COORD_2') is carried out. `?GL_MAP1_VERTEX_4'
%% overrides `?GL_MAP1_VERTEX_3', and `?GL_MAP2_VERTEX_4' overrides `?GL_MAP2_VERTEX_3'
%% , in the same manner. If neither a three- nor a four-component vertex map is enabled for
%% the specified dimension, the ``gl:evalCoord'' command is ignored. 
%%
%%  If you have enabled automatic normal generation, by calling  {@link gl:enable/1}  with argument
%% `?GL_AUTO_NORMAL', ``gl:evalCoord2'' generates surface normals analytically, regardless
%% of the contents or enabling of the `?GL_MAP2_NORMAL' map. Let 
%%
%%  m=((&amp;PartialD; p)/(&amp;PartialD; u))×((&amp;PartialD; p)/(&amp;PartialD; v))
%%
%%  Then the generated normal   n is  n=m/(||m||)
%%
%%  If automatic normal generation is disabled, the corresponding normal map `?GL_MAP2_NORMAL'
%% , if enabled, is used to produce a normal. If neither automatic normal generation nor
%% a normal map is enabled, no normal is generated for ``gl:evalCoord2'' commands. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalCoord.xml">external</a> documentation.
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
%% ``gl:mapGrid1'' and ``gl:mapGrid2'' specify the linear grid mappings between the   i
%% (or   i and   j) integer grid coordinates, to the   u (or   u and   v) floating-point
%% evaluation map coordinates. See  {@link gl:map1d/6}  and  {@link gl:map1d/6}  for details of how
%%   u and   v coordinates are evaluated. 
%%
%% ``gl:mapGrid1'' specifies a single linear mapping such that integer grid coordinate
%% 0 maps exactly to  `U1' , and integer grid coordinate  `Un'  maps exactly to  `U2' 
%% . All other integer grid coordinates   i are mapped so that 
%%
%%  u=i(u2-u1)/un+u1
%%
%% ``gl:mapGrid2'' specifies two such linear mappings. One maps integer grid coordinate  
%% i=0 exactly to  `U1' , and integer grid coordinate   i=un exactly to  `U2' . The
%% other maps integer grid coordinate   j=0 exactly to  `V1' , and integer grid coordinate
%%   j=vn exactly to  `V2' . Other integer grid coordinates   i and   j are mapped such
%% that 
%%
%%  u=i(u2-u1)/un+u1
%%
%%  v=j(v2-v1)/vn+v1
%%
%%  The mappings specified by ``gl:mapGrid'' are used identically by  {@link gl:evalMesh1/3} 
%% and  {@link gl:evalPoint1/1} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMapGrid.xml">external</a> documentation.
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
%%  and   n,   u 1, and   u 2 are the arguments to the most recent  {@link gl:mapGrid1d/3}  command.
%% The one absolute numeric requirement is that if   i=n, then the value computed from  i.&amp;Delta;
%%  u+u 1 is exactly   u 2. 
%%
%%  In the two-dimensional case, ``gl:evalPoint2'', let 
%%
%% &amp;Delta; u=(u 2-u 1)/n
%%
%% &amp;Delta; v=(v 2-v 1)/m
%%
%%  where   n,   u 1,   u 2,   m,   v 1, and   v 2 are the arguments to the most recent  {@link gl:mapGrid1d/3} 
%%  command. Then the ``gl:evalPoint2'' command is equivalent to calling  glEvalCoord2(  i.
%% &amp;Delta; u+u 1, j.&amp;Delta; v+v 1 );  The only absolute numeric requirements are
%% that if   i=n, then the value computed from  i.&amp;Delta; u+u 1 is exactly   u 2, and
%% if   j=m, then the value computed from  j.&amp;Delta; v+v 1 is exactly   v 2. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalPoint.xml">external</a> documentation.
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
%%  In the one-dimensional case, ``gl:evalMesh1'', the mesh is generated as if the following
%% code fragment were executed: 
%%
%%  glBegin(  `Type'  ); for ( i =  `I1' ; i &lt;=  `I2' ; i += 1 ) glEvalCoord1( 
%% i.&amp;Delta; u+u 1 ); glEnd();  where 
%%
%% &amp;Delta; u=(u 2-u 1)/n
%%
%%  and   n,   u 1, and   u 2 are the arguments to the most recent  {@link gl:mapGrid1d/3}  command.
%% `type' is `?GL_POINTS' if  `Mode'  is `?GL_POINT', or `?GL_LINES'
%% if  `Mode'  is `?GL_LINE'. 
%%
%%  The one absolute numeric requirement is that if   i=n, then the value computed from   i.&amp;Delta;
%%  u+u 1 is exactly   u 2. 
%%
%%  In the two-dimensional case, ``gl:evalMesh2'', let .cp &amp;Delta; u=(u 2-u 1)/n
%%
%% &amp;Delta; v=(v 2-v 1)/m
%%
%%  where   n,   u 1,   u 2,   m,   v 1, and   v 2 are the arguments to the most recent  {@link gl:mapGrid1d/3} 
%%  command. Then, if  `Mode'  is `?GL_FILL', the ``gl:evalMesh2'' command is equivalent
%% to: 
%%
%%  for ( j =  `J1' ; j &lt;  `J2' ; j += 1 ) { glBegin( GL_QUAD_STRIP ); for ( i =  `I1' 
%% ; i &lt;=  `I2' ; i += 1 ) { glEvalCoord2(  i.&amp;Delta; u+u 1, j.&amp;Delta; v+v 1
%% ); glEvalCoord2(  i.&amp;Delta; u+u 1,(j+1).&amp;Delta; v+v 1 ); } glEnd(); } 
%%
%%  If  `Mode'  is `?GL_LINE', then a call to ``gl:evalMesh2'' is equivalent to: 
%%
%%  for ( j =  `J1' ; j &lt;=  `J2' ; j += 1 ) { glBegin( GL_LINE_STRIP ); for ( i =  `I1' 
%% ; i &lt;=  `I2' ; i += 1 ) glEvalCoord2(  i.&amp;Delta; u+u 1, j.&amp;Delta; v+v 1
%% ); glEnd(); }  for ( i =  `I1' ; i &lt;=  `I2' ; i += 1 ) { glBegin( GL_LINE_STRIP
%% ); for ( j =  `J1' ; j &lt;=  `J1' ; j += 1 ) glEvalCoord2(  i.&amp;Delta; u+u 1, j.
%% &amp;Delta; v+v 1 ); glEnd(); } 
%%
%%  And finally, if  `Mode'  is `?GL_POINT', then a call to ``gl:evalMesh2'' is
%% equivalent to: 
%%
%%  glBegin( GL_POINTS ); for ( j =  `J1' ; j &lt;=  `J2' ; j += 1 ) for ( i =  `I1' 
%% ; i &lt;=  `I2' ; i += 1 ) glEvalCoord2(  i.&amp;Delta; u+u 1, j.&amp;Delta; v+v 1
%% ); glEnd(); 
%%
%%  In all three cases, the only absolute numeric requirements are that if   i=n, then the
%% value computed from   i.&amp;Delta; u+u 1 is exactly  u 2, and if   j=m, then the value
%% computed from  j.&amp;Delta; v+v 1 is exactly   v 2. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEvalMesh.xml">external</a> documentation.
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
%% ``gl:fog'' assigns the value or values in  `Params'  to the fog parameter specified
%% by  `Pname' . The following values are accepted for  `Pname' : 
%%
%% `?GL_FOG_MODE':  `Params'  is a single integer or floating-point value that specifies
%% the equation to be used to compute the fog blend factor,   f. Three symbolic constants
%% are accepted: `?GL_LINEAR', `?GL_EXP', and `?GL_EXP2'. The equations corresponding
%% to these symbolic constants are defined below. The initial fog mode is `?GL_EXP'. 
%%
%% `?GL_FOG_DENSITY':  `Params'  is a single integer or floating-point value that
%% specifies   density, the fog density used in both exponential fog equations. Only nonnegative
%% densities are accepted. The initial fog density is 1. 
%%
%% `?GL_FOG_START':  `Params'  is a single integer or floating-point value that specifies
%%   start, the near distance used in the linear fog equation. The initial near distance
%% is 0. 
%%
%% `?GL_FOG_END':  `Params'  is a single integer or floating-point value that specifies
%%   end, the far distance used in the linear fog equation. The initial far distance is 1. 
%%
%% `?GL_FOG_INDEX':  `Params'  is a single integer or floating-point value that specifies
%%  i f, the fog color index. The initial fog index is 0. 
%%
%% `?GL_FOG_COLOR':  `Params'  contains four integer or floating-point values that
%% specify  C f, the fog color. Integer values are mapped linearly such that the most positive
%% representable value maps to 1.0, and the most negative representable value maps to   -1.0.
%% Floating-point values are mapped directly. After conversion, all color components are
%% clamped to the range  [0 1]. The initial fog color is (0, 0, 0, 0). 
%%
%% `?GL_FOG_COORD_SRC':  `Params'  contains either of the following symbolic constants:
%% `?GL_FOG_COORD' or `?GL_FRAGMENT_DEPTH'. `?GL_FOG_COORD' specifies that
%% the current fog coordinate should be used as distance value in the fog color computation.
%% `?GL_FRAGMENT_DEPTH' specifies that the current fragment depth should be used as
%% distance value in the fog computation. 
%%
%%  Fog blends a fog color with each rasterized pixel fragment's post-texturing color using
%% a blending factor   f. Factor   f is computed in one of three ways, depending on the fog
%% mode. Let   c be either the distance in eye coordinate from the origin (in the case that
%% the `?GL_FOG_COORD_SRC' is `?GL_FRAGMENT_DEPTH') or the current fog coordinate
%% (in the case that `?GL_FOG_COORD_SRC' is `?GL_FOG_COORD'). The equation for `?GL_LINEAR'
%%  fog is  f=(end-c)/(end-start)
%%
%%  The equation for `?GL_EXP' fog is  f=e(-(density. c))
%%
%%  The equation for `?GL_EXP2' fog is  f=e(-(density. c)) 2
%%
%%  Regardless of the fog mode,  f is clamped to the range  [0 1] after it is computed. Then,
%% if the GL is in RGBA color mode, the fragment's red, green, and blue colors, represented
%% by   C r, are replaced by 
%%
%% (C r)"=f×C r+(1-f)×C f
%%
%%  Fog does not affect a fragment's alpha component. 
%%
%%  In color index mode, the fragment's color index   i r is replaced by 
%%
%% (i r)"=i r+(1-f)×i f
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFog.xml">external</a> documentation.
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
%% ``gl:feedbackBuffer'' has three arguments:  `Buffer'  is a pointer to an array of
%% floating-point values into which feedback information is placed.  `Size'  indicates
%% the size of the array.  `Type'  is a symbolic constant describing the information that
%% is fed back for each vertex. ``gl:feedbackBuffer'' must be issued before feedback mode
%% is enabled (by calling  {@link gl:renderMode/1}  with argument `?GL_FEEDBACK'). Setting
%% `?GL_FEEDBACK' without establishing the feedback buffer, or calling ``gl:feedbackBuffer''
%%  while the GL is in feedback mode, is an error. 
%%
%%  When  {@link gl:renderMode/1}  is called while in feedback mode, it returns the number of
%% entries placed in the feedback array and resets the feedback array pointer to the base
%% of the feedback buffer. The returned value never exceeds  `Size' . If the feedback
%% data required more room than was available in  `Buffer' ,  {@link gl:renderMode/1}  returns
%% a negative value. To take the GL out of feedback mode, call  {@link gl:renderMode/1}  with
%% a parameter value other than `?GL_FEEDBACK'. 
%%
%%  While in feedback mode, each primitive, bitmap, or pixel rectangle that would be rasterized
%% generates a block of values that are copied into the feedback array. If doing so would
%% cause the number of entries to exceed the maximum, the block is partially written so as
%% to fill the array (if there is any room left at all), and an overflow flag is set. Each
%% block begins with a code indicating the primitive type, followed by values that describe
%% the primitive's vertices and associated data. Entries are also written for bitmaps and
%% pixel rectangles. Feedback occurs after polygon culling and  {@link gl:polygonMode/2}  interpretation
%% of polygons has taken place, so polygons that are culled are not returned in the feedback
%% buffer. It can also occur after polygons with more than three edges are broken up into
%% triangles, if the GL implementation renders polygons by performing this decomposition. 
%%
%%  The  {@link gl:passThrough/1}  command can be used to insert a marker into the feedback
%% buffer. See  {@link gl:passThrough/1} . 
%%
%%  Following is the grammar for the blocks of values written into the feedback buffer. Each
%% primitive is indicated with a unique identifying value followed by some number of vertices.
%% Polygon entries include an integer value indicating how many vertices follow. A vertex
%% is fed back as some number of floating-point values, as determined by  `Type' . Colors
%% are fed back as four values in RGBA mode and one value in color index mode. 
%%
%%  feedbackList  ← feedbackItem feedbackList | feedbackItem 
%%
%%  feedbackItem  ← point | lineSegment | polygon | bitmap | pixelRectangle | passThru 
%%
%%  point  ←`?GL_POINT_TOKEN' vertex 
%%
%%  lineSegment  ←`?GL_LINE_TOKEN' vertex vertex | `?GL_LINE_RESET_TOKEN' vertex
%% vertex 
%%
%%  polygon  ←`?GL_POLYGON_TOKEN' n polySpec 
%%
%%  polySpec  ← polySpec vertex | vertex vertex vertex 
%%
%%  bitmap  ←`?GL_BITMAP_TOKEN' vertex 
%%
%%  pixelRectangle  ←`?GL_DRAW_PIXEL_TOKEN' vertex | `?GL_COPY_PIXEL_TOKEN' vertex
%% 
%%
%%  passThru  ←`?GL_PASS_THROUGH_TOKEN' value 
%%
%%  vertex  ← 2d | 3d | 3dColor | 3dColorTexture | 4dColorTexture 
%%
%%  2d  ← value value 
%%
%%  3d  ← value value value 
%%
%%  3dColor  ← value value value color 
%%
%%  3dColorTexture  ← value value value color tex 
%%
%%  4dColorTexture  ← value value value value color tex 
%%
%%  color  ← rgba | index 
%%
%%  rgba  ← value value value value 
%%
%%  index  ← value 
%%
%%  tex  ← value value value value 
%%
%% `value' is a floating-point number, and `n' is a floating-point integer giving
%% the number of vertices in the polygon. `?GL_POINT_TOKEN', `?GL_LINE_TOKEN', `?GL_LINE_RESET_TOKEN'
%% , `?GL_POLYGON_TOKEN', `?GL_BITMAP_TOKEN', `?GL_DRAW_PIXEL_TOKEN', `?GL_COPY_PIXEL_TOKEN'
%%  and `?GL_PASS_THROUGH_TOKEN' are symbolic floating-point constants. `?GL_LINE_RESET_TOKEN'
%%  is returned whenever the line stipple pattern is reset. The data returned as a vertex
%% depends on the feedback  `Type' . 
%%
%%  The following table gives the correspondence between  `Type'  and the number of values
%% per vertex. `k' is 1 in color index mode and 4 in RGBA mode. 
%%
%% <table><tbody><tr><td>` Type '</td><td>` Coordinates '</td><td>` Color '</td>
%% <td>` Texture '</td><td>` Total Number of Values '</td></tr></tbody><tbody><tr><td>
%% `?GL_2D'</td><td>`x', `y'</td><td></td><td></td><td> 2 </td></tr><tr><td>`?GL_3D'
%% </td><td>`x', `y', `z'</td><td></td><td></td><td> 3 </td></tr><tr><td>`?GL_3D_COLOR'
%% </td><td>`x', `y', `z'</td><td> k</td><td></td><td> 3+k</td></tr><tr><td>`?GL_3D_COLOR_TEXTURE'
%% </td><td>`x', `y', `z'</td><td> k</td><td> 4 </td><td> 7+k</td></tr><tr><td>
%% `?GL_4D_COLOR_TEXTURE'</td><td>`x', `y', `z', `w'</td><td> k</td>
%% <td> 4 </td><td> 8+k</td></tr></tbody></table>
%%
%%  Feedback vertex coordinates are in window coordinates, except `w', which is in clip
%% coordinates. Feedback colors are lighted, if lighting is enabled. Feedback texture coordinates
%% are generated, if texture coordinate generation is enabled. They are always transformed
%% by the texture matrix. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFeedbackBuffer.xml">external</a> documentation.
-spec feedbackBuffer(Size, Type, Buffer) -> 'ok' when Size :: integer(),Type :: enum(),Buffer :: mem().
feedbackBuffer(Size,Type,Buffer) ->
  send_bin(Buffer),
  call(5308, <<Size:?GLsizei,Type:?GLenum>>).

%% @doc Place a marker in the feedback buffer
%%
%%  Feedback is a GL render mode. The mode is selected by calling  {@link gl:renderMode/1} 
%% with `?GL_FEEDBACK'. When the GL is in feedback mode, no pixels are produced by rasterization.
%% Instead, information about primitives that would have been rasterized is fed back to the
%% application using the GL. See the  {@link gl:feedbackBuffer/3}  reference page for a description
%% of the feedback buffer and the values in it. 
%%
%% ``gl:passThrough'' inserts a user-defined marker in the feedback buffer when it is executed
%% in feedback mode.  `Token'  is returned as if it were a primitive; it is indicated
%% with its own unique identifying value: `?GL_PASS_THROUGH_TOKEN'. The order of ``gl:passThrough''
%%  commands with respect to the specification of graphics primitives is maintained. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPassThrough.xml">external</a> documentation.
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
%%  A programmer can use selection to determine which primitives are drawn into some region
%% of a window. The region is defined by the current modelview and perspective matrices. 
%%
%%  In selection mode, no pixel fragments are produced from rasterization. Instead, if a
%% primitive or a raster position intersects the clipping volume defined by the viewing frustum
%% and the user-defined clipping planes, this primitive causes a selection hit. (With polygons,
%% no hit occurs if the polygon is culled.) When a change is made to the name stack, or when
%%  {@link gl:renderMode/1}  is called, a hit record is copied to  `Buffer'  if any hits
%% have occurred since the last such event (name stack change or  {@link gl:renderMode/1}  call).
%% The hit record consists of the number of names in the name stack at the time of the event,
%% followed by the minimum and maximum depth values of all vertices that hit since the previous
%% event, followed by the name stack contents, bottom name first. 
%%
%%  Depth values (which are in the range [0,1]) are multiplied by   2 32-1, before being
%% placed in the hit record. 
%%
%%  An internal index into  `Buffer'  is reset to 0 whenever selection mode is entered.
%% Each time a hit record is copied into  `Buffer' , the index is incremented to point
%% to the cell just past the end of the block of names(emthat is, to the next available cell
%% If the hit record is larger than the number of remaining locations in  `Buffer' , as
%% much data as can fit is copied, and the overflow flag is set. If the name stack is empty
%% when a hit record is copied, that record consists of 0 followed by the minimum and maximum
%% depth values. 
%%
%%  To exit selection mode, call  {@link gl:renderMode/1}  with an argument other than `?GL_SELECT'
%% . Whenever  {@link gl:renderMode/1}  is called while the render mode is `?GL_SELECT',
%% it returns the number of hit records copied to  `Buffer' , resets the overflow flag
%% and the selection buffer pointer, and initializes the name stack to be empty. If the overflow
%% bit was set when  {@link gl:renderMode/1}  was called, a negative hit record count is returned.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSelectBuffer.xml">external</a> documentation.
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
%%  The name stack is always empty while the render mode is not `?GL_SELECT'. Calls to ``gl:initNames''
%%  while the render mode is not `?GL_SELECT' are ignored. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glInitNames.xml">external</a> documentation.
-spec initNames() -> 'ok'.
initNames() ->
  cast(5311, <<>>).

%% @doc Load a name onto the name stack
%%
%%  The name stack is used during selection mode to allow sets of rendering commands to be
%% uniquely identified. It consists of an ordered set of unsigned integers and is initially
%% empty. 
%%
%% ``gl:loadName'' causes  `Name'  to replace the value on the top of the name stack. 
%%
%%  The name stack is always empty while the render mode is not `?GL_SELECT'. Calls to ``gl:loadName''
%%  while the render mode is not `?GL_SELECT' are ignored. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadName.xml">external</a> documentation.
-spec loadName(Name) -> 'ok' when Name :: integer().
loadName(Name) ->
  cast(5312, <<Name:?GLuint>>).

%% @doc Push and pop the name stack
%%
%%  The name stack is used during selection mode to allow sets of rendering commands to be
%% uniquely identified. It consists of an ordered set of unsigned integers and is initially
%% empty. 
%%
%% ``gl:pushName'' causes  `Name'  to be pushed onto the name stack.  {@link gl:pushName/1} 
%% pops one name off the top of the stack. 
%%
%%  The maximum name stack depth is implementation-dependent; call `?GL_MAX_NAME_STACK_DEPTH'
%%  to find out the value for a particular implementation. It is an error to push a name
%% onto a full stack or to pop a name off an empty stack. It is also an error to manipulate
%% the name stack between the execution of  {@link gl:'begin'/1}  and the corresponding execution
%% of  {@link gl:'begin'/1} . In any of these cases, the error flag is set and no other change is
%% made to GL state. 
%%
%%  The name stack is always empty while the render mode is not `?GL_SELECT'. Calls to ``gl:pushName''
%%  or  {@link gl:pushName/1}  while the render mode is not `?GL_SELECT' are ignored. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPushName.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendColor.xml">external</a> documentation.
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
%%  These equations use the source and destination blend factors specified by either  {@link gl:blendFunc/2} 
%%  or  {@link gl:blendFuncSeparate/4} . See  {@link gl:blendFunc/2}  or  {@link gl:blendFuncSeparate/4} 
%%  for a description of the various blend factors. 
%%
%%  In the equations that follow, source and destination color components are referred to
%% as (R s G s B s A s) and (R d G d B d A d), respectively. The result color is referred to as (R r G r B r A r). The source and destination
%% blend factors are denoted (s R s G s B s A) and (d R d G d B d A), respectively. For these equations all color components
%% are understood to have values in the range  [0 1].  <table><tbody><tr><td>` Mode '</td><td>
%% ` RGB Components '</td><td>` Alpha Component '</td></tr></tbody><tbody><tr><td>`?GL_FUNC_ADD'
%% </td><td> Rr=R s  s R+R d  d R Gr=G s  s G+G d  d G Br=B s  s B+B d  d B</td><td> Ar=A s 
%% s A+A d  d A</td></tr><tr><td>`?GL_FUNC_SUBTRACT'</td><td> Rr=R s  s R-R d  d R Gr=G
%% s  s G-G d  d G Br=B s  s B-B d  d B</td><td> Ar=A s  s A-A d  d A</td></tr><tr><td>`?GL_FUNC_REVERSE_SUBTRACT'
%% </td><td> Rr=R d  d R-R s  s R Gr=G d  d G-G s  s G Br=B d  d B-B s  s B</td><td> Ar=A d 
%% d A-A s  s A</td></tr><tr><td>`?GL_MIN'</td><td> Rr=min(R s R d) Gr=min(G s G d) Br=min(B s B d)</td><td> Ar=min
%% (A s A d)</td></tr><tr><td>`?GL_MAX'</td><td> Rr=max(R s R d) Gr=max(G s G d) Br=max(B s B d)</td><td> Ar=max(A s A d)</td></tr></tbody>
%% </table>
%%
%%  The results of these equations are clamped to the range  [0 1]. 
%%
%%  The `?GL_MIN' and `?GL_MAX' equations are useful for applications that analyze
%% image data (image thresholding against a constant color, for example). The `?GL_FUNC_ADD'
%%  equation is useful for antialiasing and transparency, among other things. 
%%
%%  Initially, both the RGB blend equation and the alpha blend equation are set to `?GL_FUNC_ADD'
%% . 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquation.xml">external</a> documentation.
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
%%  Implementations denote recommended maximum amounts of vertex and index data, which may
%% be queried by calling  {@link gl:getBooleanv/1}  with argument `?GL_MAX_ELEMENTS_VERTICES' and `?GL_MAX_ELEMENTS_INDICES'
%% . If   end-start+1 is greater than the value of `?GL_MAX_ELEMENTS_VERTICES', or if  `Count' 
%%  is greater than the value of `?GL_MAX_ELEMENTS_INDICES', then the call may operate
%% at reduced performance. There is no requirement that all vertices in the range [start end] be referenced.
%% However, the implementation may partially process unused vertices, reducing performance
%% from what could be achieved with an optimal index set. 
%%
%%  When ``gl:drawRangeElements'' is called, it uses  `Count'  sequential elements from
%% an enabled array, starting at  `Start'  to construct a sequence of geometric primitives.
%%  `Mode'  specifies what kind of primitives are constructed, and how the array elements
%% construct these primitives. If more than one array is enabled, each is used.  
%%
%%  Vertex attributes that are modified by ``gl:drawRangeElements'' have an unspecified
%% value after ``gl:drawRangeElements'' returns. Attributes that aren't modified maintain
%% their previous values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawRangeElements.xml">external</a> documentation.
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
%%  To define texture images, call ``gl:texImage3D''. The arguments describe the parameters
%% of the texture image, such as height, width, depth, width of the border, level-of-detail
%% number (see  {@link gl:texParameterf/3} ), and number of color components provided. The last
%% three arguments describe how the image is represented in memory. 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_3D', no data is read from  `Data' , but
%% all of the texture image state is recalculated, checked for consistency, and checked against
%% the implementation's capabilities. If the implementation cannot handle a texture of the
%% requested texture size, it sets all of the image state to 0, but does not generate an
%% error (see  {@link gl:getError/0} ). To query for an entire mipmap array, use an image array
%% level greater than or equal to 1. 
%%
%%  If  `Target'  is `?GL_TEXTURE_3D', data is read from  `Data'  as a sequence
%% of signed or unsigned bytes, shorts, or longs, or single-precision floating-point values,
%% depending on  `Type' . These values are grouped into sets of one, two, three, or four
%% values, depending on  `Format' , to form elements. Each data byte is treated as eight
%% 1-bit elements, with bit ordering determined by `?GL_UNPACK_LSB_FIRST' (see  {@link gl:pixelStoref/2} 
%% ). 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  The first element corresponds to the lower left corner of the texture image. Subsequent
%% elements progress left-to-right through the remaining texels in the lowest row of the
%% texture image, and then in successively higher rows of the texture image. The final element
%% corresponds to the upper right corner of the texture image. 
%%
%%  `Format'  determines the composition of each element in  `Data' . It can assume
%% one of these symbolic values: 
%%
%% `?GL_RED':  Each element is a single red component. The GL converts it to floating
%% point and assembles it into an RGBA element by attaching 0 for green and blue, and 1 for
%% alpha. Each component is then multiplied by the signed scale factor `?GL_c_SCALE',
%% added to the signed bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RG':  Each element is a red and green pair. The GL converts each to floating
%% point and assembles it into an RGBA element by attaching 0 for blue, and 1 for alpha.
%% Each component is then multiplied by the signed scale factor `?GL_c_SCALE', added
%% to the signed bias `?GL_c_BIAS', and clamped to the range [0,1]. 
%%
%% `?GL_RGB'
%%
%% `?GL_BGR':  Each element is an RGB triple. The GL converts it to floating point and
%% assembles it into an RGBA element by attaching 1 for alpha. Each component is then multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%% `?GL_RGBA'
%%
%% `?GL_BGRA':  Each element contains all four components. Each component is multiplied
%% by the signed scale factor `?GL_c_SCALE', added to the signed bias `?GL_c_BIAS',
%% and clamped to the range [0,1]. 
%%
%%  If an application wants to store the texture at a certain resolution or in a certain
%% format, it can request the resolution and format with  `InternalFormat' . The GL will
%% choose an internal representation that closely approximates that requested by  `InternalFormat' 
%% , but it may not match exactly. (The representations specified by `?GL_RED', `?GL_RG'
%% , `?GL_RGB', and `?GL_RGBA' must match exactly.) 
%%
%%  `InternalFormat'  may be one of the base internal formats shown in Table 1, below 
%%
%%  `InternalFormat'  may also be one of the sized internal formats shown in Table 2,
%% below 
%%
%%  Finally,  `InternalFormat'  may also be one of the generic or compressed compressed
%% texture formats shown in Table 3 below 
%%
%%   If the  `InternalFormat'  parameter is one of the generic compressed formats,  `?GL_COMPRESSED_RED'
%% , `?GL_COMPRESSED_RG',  `?GL_COMPRESSED_RGB', or  `?GL_COMPRESSED_RGBA',
%% the GL will replace the internal format with the symbolic constant for a specific internal
%% format and compress the texture before storage. If no corresponding internal format is
%% available, or the GL can not compress that image for any reason, the internal format is
%% instead replaced with a corresponding base internal format. 
%%
%%  If the  `InternalFormat'  parameter is     `?GL_SRGB',    `?GL_SRGB8',    `?GL_SRGB_ALPHA'
%% , or    `?GL_SRGB8_ALPHA8', the texture is treated as if the red, green, blue, or
%% luminance components are encoded in the sRGB color space. Any alpha component is left
%% unchanged. The conversion from the sRGB encoded component      c s    to a linear component
%%      c l is:   
%%
%%  c l={ c s/12.92if c s&amp;le; 0.04045( c s+0.055/1.055) 2.4if c s&gt; 0.04045
%%
%%     Assume        c s    is the sRGB component in the range [0,1]. 
%%
%%  Use the `?GL_PROXY_TEXTURE_3D' target to try out a resolution and format. The implementation
%% will update and recompute its best match for the requested storage resolution and format.
%% To then query this state, call  {@link gl:getTexLevelParameterfv/3} . If the texture cannot
%% be accommodated, texture state is set to 0. 
%%
%%  A one-component texture image uses only the red component of the RGBA color extracted
%% from  `Data' . A two-component image uses the R and A values. A three-component image
%% uses the R, G, and B values. A four-component image uses all of the RGBA components. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3D.xml">external</a> documentation.
-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when  is_integer(Pixels) ->
  cast(5319, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum,Pixels:?GLuint>>);
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) ->
  send_bin(Pixels),
  cast(5320, <<Target:?GLenum,Level:?GLint,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Border:?GLint,Format:?GLenum,Type:?GLenum>>).

%% @doc glTexSubImage
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexSubImage.xml">external</a> documentation.
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
%%  The screen-aligned pixel rectangle with lower left corner at ( `X' ,  `Y' ) and
%% with width  `Width'  and height  `Height'  replaces the portion of the texture array
%% with x indices  `Xoffset'  through   xoffset+width-1, inclusive, and y indices  `Yoffset' 
%%  through   yoffset+height-1, inclusive, at z index  `Zoffset'  and at the mipmap level
%% specified by  `Level' . 
%%
%%  The pixels in the rectangle are processed exactly as if  {@link gl:readPixels/7}  had been
%% called, but the process stops just before final conversion. At this point, all pixel component
%% values are clamped to the range  [0 1] and then converted to the texture's internal format
%% for storage in the texel array. 
%%
%%  The destination rectangle in the texture array may not include any texels outside the
%% texture array as it was originally specified. It is not an error to specify a subtexture
%% with zero width or height, but such a specification has no effect. 
%%
%%  If any of the pixels within the specified rectangle of the current `?GL_READ_BUFFER'
%% are outside the read window associated with the current rendering context, then the values
%% obtained for those pixels are undefined. 
%%
%%  No change is made to the `internalformat', `width', `height', `depth',
%% or `border' parameters of the specified texture array or to texel values outside
%% the specified subregion. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyTexSubImage3D.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a color table is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  If  `Target'  is `?GL_COLOR_TABLE', `?GL_POST_CONVOLUTION_COLOR_TABLE', or `?GL_POST_COLOR_MATRIX_COLOR_TABLE'
%% , ``gl:colorTable'' builds a color lookup table from an array of pixels. The pixel array
%% specified by  `Width' ,  `Format' ,  `Type' , and  `Data'  is extracted from
%% memory and processed just as if  {@link gl:drawPixels/5}  were called, but processing stops
%% after the final expansion to RGBA is completed. 
%%
%%  The four scale parameters and the four bias parameters that are defined for the table
%% are then used to scale and bias the R, G, B, and A components of each pixel. (Use ``gl:colorTableParameter''
%%  to set these scale and bias parameters.) 
%%
%%  Next, the R, G, B, and A values are clamped to the range  [0 1]. Each pixel is then converted
%% to the internal format specified by  `Internalformat' . This conversion simply maps
%% the component values of the pixel (R, G, B, and A) to the values included in the internal
%% format (red, green, blue, alpha, luminance, and intensity). The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  Finally, the red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in the color table. They form a one-dimensional table with indices in
%% the range [0 width-1]. 
%%
%%  If  `Target'  is `?GL_PROXY_*', ``gl:colorTable'' recomputes and stores the
%% values of the proxy color table's state variables `?GL_COLOR_TABLE_FORMAT', `?GL_COLOR_TABLE_WIDTH'
%% , `?GL_COLOR_TABLE_RED_SIZE', `?GL_COLOR_TABLE_GREEN_SIZE', `?GL_COLOR_TABLE_BLUE_SIZE'
%% , `?GL_COLOR_TABLE_ALPHA_SIZE', `?GL_COLOR_TABLE_LUMINANCE_SIZE', and `?GL_COLOR_TABLE_INTENSITY_SIZE'
%% . There is no effect on the image or state of any actual color table. If the specified
%% color table is too large to be supported, then all the proxy state variables listed above
%% are set to zero. Otherwise, the color table could be supported by ``gl:colorTable''
%% using the corresponding non-proxy target, and the proxy state variables are set as if
%% that target were being defined. 
%%
%%  The proxy state variables can be retrieved by calling  {@link gl:getColorTableParameterfv/2} 
%% with a target of `?GL_PROXY_*'. This allows the application to decide if a particular
%% ``gl:colorTable'' command would succeed, and to determine what the resulting color table
%% attributes would be. 
%%
%%  If a color table is enabled, and its width is non-zero, then its contents are used to
%% replace a subset of the components of each RGBA pixel group, based on the internal format
%% of the table. 
%%
%%  Each pixel group has color components (R, G, B, A) that are in the range  [0.0 1.0]. The color
%% components are rescaled to the size of the color lookup table to form an index. Then a
%% subset of the components based on the internal format of the table are replaced by the
%% table entry selected by that index. If the color components and contents of the table
%% are represented as follows: 
%%
%% <table><tbody><tr><td>` Representation '</td><td>` Meaning '</td></tr></tbody><tbody>
%% <tr><td>r</td><td> Table index computed from R</td></tr><tr><td>g</td><td> Table index
%% computed from G</td></tr><tr><td>b</td><td> Table index computed from B</td></tr><tr><td>a
%% </td><td> Table index computed from A</td></tr><tr><td>L[i]</td><td> Luminance value at
%% table index i</td></tr><tr><td>I[i]</td><td> Intensity value at table index i</td></tr><tr>
%% <td>R[i]</td><td> Red value at table index i</td></tr><tr><td>G[i]</td><td> Green value
%% at table index i</td></tr><tr><td>B[i]</td><td> Blue value at table index i</td></tr><tr><td>
%% A[i]</td><td> Alpha value at table index i</td></tr></tbody></table>
%%
%%  then the result of color table lookup is as follows: 
%%
%% <table><tbody><tr><td></td><td>` Resulting Texture Components '</td></tr><tr><td>` Table Internal Format '
%% </td><td>` R '</td><td>` G '</td><td>` B '</td><td>` A '</td></tr></tbody>
%% <tbody><tr><td>`?GL_ALPHA'</td><td>R</td><td>G</td><td>B</td><td>A[a]</td></tr><tr><td>
%% `?GL_LUMINANCE'</td><td>L[r]</td><td>L[g]</td><td>L[b]</td><td>At</td></tr><tr><td>`?GL_LUMINANCE_ALPHA'
%% </td><td>L[r]</td><td>L[g]</td><td>L[b]</td><td>A[a]</td></tr><tr><td>`?GL_INTENSITY'</td>
%% <td>I[r]</td><td>I[g]</td><td>I[b]</td><td>I[a]</td></tr><tr><td>`?GL_RGB'</td><td>R[r]
%% </td><td>G[g]</td><td>B[b]</td><td>A</td></tr><tr><td>`?GL_RGBA'</td><td>R[r]</td><td>
%% G[g]</td><td>B[b]</td><td>A[a]</td></tr></tbody></table>
%%
%%  When `?GL_COLOR_TABLE' is enabled, the colors resulting from the pixel map operation
%% (if it is enabled) are mapped by the color lookup table before being passed to the convolution
%% operation. The colors resulting from the convolution operation are modified by the post
%% convolution color lookup table when `?GL_POST_CONVOLUTION_COLOR_TABLE' is enabled.
%% These modified colors are then sent to the color matrix operation. Finally, if `?GL_POST_COLOR_MATRIX_COLOR_TABLE'
%%  is enabled, the colors resulting from the color matrix operation are mapped by the post
%% color matrix color lookup table before being used by the histogram operation. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTable.xml">external</a> documentation.
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
%%  `Pname'  must be `?GL_COLOR_TABLE_SCALE' to set the scale factors. In this case,
%%  `Params'  points to an array of four values, which are the scale factors for red,
%% green, blue, and alpha, in that order. 
%%
%%  `Pname'  must be `?GL_COLOR_TABLE_BIAS' to set the bias terms. In this case,  `Params' 
%%  points to an array of four values, which are the bias terms for red, green, blue, and
%% alpha, in that order. 
%%
%%  The color tables themselves are specified by calling  {@link gl:colorTable/6} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorTableParameter.xml">external</a> documentation.
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
%%  The screen-aligned pixel rectangle with lower-left corner at ( `X' ,  `Y' ) having
%% width  `Width'  and height 1 is loaded into the color table. If any pixels within this
%% region are outside the window that is associated with the GL context, the values obtained
%% for those pixels are undefined. 
%%
%%  The pixels in the rectangle are processed just as if  {@link gl:readPixels/7}  were called,
%% with  `Internalformat'  set to RGBA, but processing stops after the final conversion
%% to RGBA. 
%%
%%  The four scale parameters and the four bias parameters that are defined for the table
%% are then used to scale and bias the R, G, B, and A components of each pixel. The scale
%% and bias parameters are set by calling  {@link gl:colorTableParameterfv/3} . 
%%
%%  Next, the R, G, B, and A values are clamped to the range  [0 1]. Each pixel is then converted
%% to the internal format specified by  `Internalformat' . This conversion simply maps
%% the component values of the pixel (R, G, B, and A) to the values included in the internal
%% format (red, green, blue, alpha, luminance, and intensity). The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  Finally, the red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in the color table. They form a one-dimensional table with indices in
%% the range [0 width-1]. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorTable.xml">external</a> documentation.
-spec copyColorTable(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyColorTable(Target,Internalformat,X,Y,Width) ->
  cast(5328, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Retrieve contents of a color lookup table
%%
%% ``gl:getColorTable'' returns in  `Table'  the contents of the color table specified
%% by  `Target' . No pixel transfer operations are performed, but pixel storage modes
%% that are applicable to  {@link gl:readPixels/7}  are performed. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a histogram table is requested,  `Table'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  Color components that are requested in the specified  `Format' , but which are not
%% included in the internal format of the color lookup table, are returned as zero. The assignments
%% of internal color components to the components requested by  `Format'  are <table><tbody>
%% <tr><td>` Internal Component '</td><td>` Resulting Component '</td></tr></tbody>
%% <tbody><tr><td> Red </td><td> Red </td></tr><tr><td> Green </td><td> Green </td></tr><tr><td>
%%  Blue </td><td> Blue </td></tr><tr><td> Alpha </td><td> Alpha </td></tr><tr><td> Luminance
%% </td><td> Red </td></tr><tr><td> Intensity </td><td> Red </td></tr></tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTable.xml">external</a> documentation.
-spec getColorTable(Target, Format, Type, Table) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Table :: mem().
getColorTable(Target,Format,Type,Table) ->
  send_bin(Table),
  call(5329, <<Target:?GLenum,Format:?GLenum,Type:?GLenum>>).

%% @doc Get color lookup table parameters
%%
%%  Returns parameters specific to color table  `Target' . 
%%
%%  When  `Pname'  is set to `?GL_COLOR_TABLE_SCALE' or `?GL_COLOR_TABLE_BIAS',
%% ``gl:getColorTableParameter'' returns the color table scale or bias parameters for the
%% table specified by  `Target' . For these queries,  `Target'  must be set to `?GL_COLOR_TABLE'
%% , `?GL_POST_CONVOLUTION_COLOR_TABLE', or `?GL_POST_COLOR_MATRIX_COLOR_TABLE'
%% and  `Params'  points to an array of four elements, which receive the scale or bias
%% factors for red, green, blue, and alpha, in that order. 
%%
%% ``gl:getColorTableParameter'' can also be used to retrieve the format and size parameters
%% for a color table. For these queries, set  `Target'  to either the color table target
%% or the proxy color table target. The format and size parameters are set by  {@link gl:colorTable/6} 
%% . 
%%
%%  The following table lists the format and size parameters that may be queried. For each
%% symbolic constant listed below for  `Pname' ,  `Params'  must point to an array
%% of the given length and receive the values indicated. 
%%
%% <table><tbody><tr><td>` Parameter '</td><td>` N '</td><td>` Meaning '</td></tr>
%% </tbody><tbody><tr><td>`?GL_COLOR_TABLE_FORMAT'</td><td> 1 </td><td> Internal format
%% (e.g., `?GL_RGBA') </td></tr><tr><td>`?GL_COLOR_TABLE_WIDTH'</td><td> 1 </td><td>
%%  Number of elements in table </td></tr><tr><td>`?GL_COLOR_TABLE_RED_SIZE'</td><td>
%% 1 </td><td> Size of red component, in bits </td></tr><tr><td>`?GL_COLOR_TABLE_GREEN_SIZE'
%% </td><td> 1 </td><td> Size of green component </td></tr><tr><td>`?GL_COLOR_TABLE_BLUE_SIZE'
%% </td><td> 1 </td><td> Size of blue component </td></tr><tr><td>`?GL_COLOR_TABLE_ALPHA_SIZE'
%% </td><td> 1 </td><td> Size of alpha component </td></tr><tr><td>`?GL_COLOR_TABLE_LUMINANCE_SIZE'
%% </td><td> 1 </td><td> Size of luminance component </td></tr><tr><td>`?GL_COLOR_TABLE_INTENSITY_SIZE'
%% </td><td> 1 </td><td> Size of intensity component </td></tr></tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetColorTableParameter.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a portion of a color table is respecified,  `Data' 
%% is treated as a byte offset into the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorSubTable.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyColorSubTable.xml">external</a> documentation.
-spec copyColorSubTable(Target, Start, X, Y, Width) -> 'ok' when Target :: enum(),Start :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyColorSubTable(Target,Start,X,Y,Width) ->
  cast(5334, <<Target:?GLenum,Start:?GLsizei,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Define a one-dimensional convolution filter
%%
%% ``gl:convolutionFilter1D'' builds a one-dimensional convolution filter kernel from an
%% array of pixels. 
%%
%%  The pixel array specified by  `Width' ,  `Format' ,  `Type' , and  `Data' 
%% is extracted from memory and processed just as if  {@link gl:drawPixels/5}  were called,
%% but processing stops after the final expansion to RGBA is completed. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a convolution filter is specified,  `Data'  is
%% treated as a byte offset into the buffer object's data store. 
%%
%%  The R, G, B, and A components of each pixel are next scaled by the four 1D `?GL_CONVOLUTION_FILTER_SCALE'
%%  parameters and biased by the four 1D `?GL_CONVOLUTION_FILTER_BIAS' parameters. (The
%% scale and bias parameters are set by  {@link gl:convolutionParameterf/3}  using the `?GL_CONVOLUTION_1D'
%%  target and the names `?GL_CONVOLUTION_FILTER_SCALE' and `?GL_CONVOLUTION_FILTER_BIAS'
%% . The parameters themselves are vectors of four values that are applied to red, green,
%% blue, and alpha, in that order.) The R, G, B, and A values are not clamped to [0,1] at
%% any time during this process. 
%%
%%  Each pixel is then converted to the internal format specified by  `Internalformat' .
%% This conversion simply maps the component values of the pixel (R, G, B, and A) to the
%% values included in the internal format (red, green, blue, alpha, luminance, and intensity).
%% The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  The red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in floating-point rather than integer format. They form a one-dimensional
%% filter kernel image indexed with coordinate `i' such that `i' starts at 0 and
%% increases from left to right. Kernel location `i' is derived from the `i'th
%% pixel, counting from 0. 
%%
%%  Note that after a convolution is performed, the resulting color components are also scaled
%% by their corresponding `?GL_POST_CONVOLUTION_c_SCALE' parameters and biased by their
%% corresponding `?GL_POST_CONVOLUTION_c_BIAS' parameters (where `c' takes on the
%% values `RED', `GREEN', `BLUE', and `ALPHA'). These parameters are
%% set by  {@link gl:pixelTransferf/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter1D.xml">external</a> documentation.
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
%%  The pixel array specified by  `Width' ,  `Height' ,  `Format' ,  `Type' ,
%% and  `Data'  is extracted from memory and processed just as if  {@link gl:drawPixels/5} 
%% were called, but processing stops after the final expansion to RGBA is completed. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a convolution filter is specified,  `Data'  is
%% treated as a byte offset into the buffer object's data store. 
%%
%%  The R, G, B, and A components of each pixel are next scaled by the four 2D `?GL_CONVOLUTION_FILTER_SCALE'
%%  parameters and biased by the four 2D `?GL_CONVOLUTION_FILTER_BIAS' parameters. (The
%% scale and bias parameters are set by  {@link gl:convolutionParameterf/3}  using the `?GL_CONVOLUTION_2D'
%%  target and the names `?GL_CONVOLUTION_FILTER_SCALE' and `?GL_CONVOLUTION_FILTER_BIAS'
%% . The parameters themselves are vectors of four values that are applied to red, green,
%% blue, and alpha, in that order.) The R, G, B, and A values are not clamped to [0,1] at
%% any time during this process. 
%%
%%  Each pixel is then converted to the internal format specified by  `Internalformat' .
%% This conversion simply maps the component values of the pixel (R, G, B, and A) to the
%% values included in the internal format (red, green, blue, alpha, luminance, and intensity).
%% The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  The red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in floating-point rather than integer format. They form a two-dimensional
%% filter kernel image indexed with coordinates `i' and `j' such that `i'
%% starts at zero and increases from left to right, and `j' starts at zero and increases
%% from bottom to top. Kernel location `i,j' is derived from the `N'th pixel, where
%% `N' is `i'+`j'* `Width' . 
%%
%%  Note that after a convolution is performed, the resulting color components are also scaled
%% by their corresponding `?GL_POST_CONVOLUTION_c_SCALE' parameters and biased by their
%% corresponding `?GL_POST_CONVOLUTION_c_BIAS' parameters (where `c' takes on the
%% values `RED', `GREEN', `BLUE', and `ALPHA'). These parameters are
%% set by  {@link gl:pixelTransferf/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionFilter2D.xml">external</a> documentation.
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
%%  `Target'  selects the convolution filter to be affected: `?GL_CONVOLUTION_1D', `?GL_CONVOLUTION_2D'
%% , or `?GL_SEPARABLE_2D' for the 1D, 2D, or separable 2D filter, respectively. 
%%
%%  `Pname'  selects the parameter to be changed. `?GL_CONVOLUTION_FILTER_SCALE'
%% and `?GL_CONVOLUTION_FILTER_BIAS' affect the definition of the convolution filter
%% kernel; see  {@link gl:convolutionFilter1D/6} ,  {@link gl:convolutionFilter2D/7} , and  {@link gl:separableFilter2D/8} 
%%  for details. In these cases,  `Params' v is an array of four values to be applied
%% to red, green, blue, and alpha values, respectively. The initial value for `?GL_CONVOLUTION_FILTER_SCALE'
%%  is (1, 1, 1, 1), and the initial value for `?GL_CONVOLUTION_FILTER_BIAS' is (0,
%% 0, 0, 0). 
%%
%%  A  `Pname'  value of `?GL_CONVOLUTION_BORDER_MODE' controls the convolution border
%% mode. The accepted modes are: 
%%
%% `?GL_REDUCE':  The image resulting from convolution is smaller than the source image.
%% If the filter width is   Wf and height is   Hf, and the source image width is   Ws and
%% height is   Hs, then the convolved image width will be   Ws-Wf+1 and height will be   Hs-Hf
%% +1. (If this reduction would generate an image with zero or negative width and/or height,
%% the output is simply null, with no error generated.) The coordinates of the image resulting
%% from convolution are zero through   Ws-Wf in width and zero through   Hs-Hf in height. 
%%
%% `?GL_CONSTANT_BORDER':  The image resulting from convolution is the same size as
%% the source image, and processed as if the source image were surrounded by pixels with
%% their color specified by the `?GL_CONVOLUTION_BORDER_COLOR'. 
%%
%% `?GL_REPLICATE_BORDER':  The image resulting from convolution is the same size as
%% the source image, and processed as if the outermost pixel on the border of the source
%% image were replicated. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glConvolutionParameter.xml">external</a> documentation.
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
%%  The screen-aligned pixel rectangle with lower-left corner at ( `X' ,  `Y' ), width
%%  `Width'  and height 1 is used to define the convolution filter. If any pixels within
%% this region are outside the window that is associated with the GL context, the values
%% obtained for those pixels are undefined. 
%%
%%  The pixels in the rectangle are processed exactly as if  {@link gl:readPixels/7}  had been
%% called with `format' set to RGBA, but the process stops just before final conversion.
%% The R, G, B, and A components of each pixel are next scaled by the four 1D `?GL_CONVOLUTION_FILTER_SCALE'
%%  parameters and biased by the four 1D `?GL_CONVOLUTION_FILTER_BIAS' parameters. (The
%% scale and bias parameters are set by  {@link gl:convolutionParameterf/3}  using the `?GL_CONVOLUTION_1D'
%%  target and the names `?GL_CONVOLUTION_FILTER_SCALE' and `?GL_CONVOLUTION_FILTER_BIAS'
%% . The parameters themselves are vectors of four values that are applied to red, green,
%% blue, and alpha, in that order.) The R, G, B, and A values are not clamped to [0,1] at
%% any time during this process. 
%%
%%  Each pixel is then converted to the internal format specified by  `Internalformat' .
%% This conversion simply maps the component values of the pixel (R, G, B, and A) to the
%% values included in the internal format (red, green, blue, alpha, luminance, and intensity).
%% The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  The red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in floating-point rather than integer format. 
%%
%%  Pixel ordering is such that lower x screen coordinates correspond to lower `i' filter
%% image coordinates. 
%%
%%  Note that after a convolution is performed, the resulting color components are also scaled
%% by their corresponding `?GL_POST_CONVOLUTION_c_SCALE' parameters and biased by their
%% corresponding `?GL_POST_CONVOLUTION_c_BIAS' parameters (where `c' takes on the
%% values `RED', `GREEN', `BLUE', and `ALPHA'). These parameters are
%% set by  {@link gl:pixelTransferf/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter1D.xml">external</a> documentation.
-spec copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) ->
  cast(5341, <<Target:?GLenum,Internalformat:?GLenum,X:?GLint,Y:?GLint,Width:?GLsizei>>).

%% @doc Copy pixels into a two-dimensional convolution filter
%%
%% ``gl:copyConvolutionFilter2D'' defines a two-dimensional convolution filter kernel with
%% pixels from the current `?GL_READ_BUFFER' (rather than from main memory, as is the
%% case for  {@link gl:convolutionFilter2D/7} ). 
%%
%%  The screen-aligned pixel rectangle with lower-left corner at ( `X' ,  `Y' ), width
%%  `Width'  and height  `Height'  is used to define the convolution filter. If any
%% pixels within this region are outside the window that is associated with the GL context,
%% the values obtained for those pixels are undefined. 
%%
%%  The pixels in the rectangle are processed exactly as if  {@link gl:readPixels/7}  had been
%% called with `format' set to RGBA, but the process stops just before final conversion.
%% The R, G, B, and A components of each pixel are next scaled by the four 2D `?GL_CONVOLUTION_FILTER_SCALE'
%%  parameters and biased by the four 2D `?GL_CONVOLUTION_FILTER_BIAS' parameters. (The
%% scale and bias parameters are set by  {@link gl:convolutionParameterf/3}  using the `?GL_CONVOLUTION_2D'
%%  target and the names `?GL_CONVOLUTION_FILTER_SCALE' and `?GL_CONVOLUTION_FILTER_BIAS'
%% . The parameters themselves are vectors of four values that are applied to red, green,
%% blue, and alpha, in that order.) The R, G, B, and A values are not clamped to [0,1] at
%% any time during this process. 
%%
%%  Each pixel is then converted to the internal format specified by  `Internalformat' .
%% This conversion simply maps the component values of the pixel (R, G, B, and A) to the
%% values included in the internal format (red, green, blue, alpha, luminance, and intensity).
%% The mapping is as follows: 
%%
%% <table><tbody><tr><td>` Internal Format '</td><td>` Red '</td><td>` Green '</td>
%% <td>` Blue '</td><td>` Alpha '</td><td>` Luminance '</td><td>` Intensity '
%% </td></tr></tbody><tbody><tr><td>`?GL_ALPHA'</td><td></td><td></td><td></td><td> A </td>
%% <td></td><td></td></tr><tr><td>`?GL_LUMINANCE'</td><td></td><td></td><td></td><td></td>
%% <td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td><td></td><td></td><td></td>
%% <td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'</td><td></td><td></td><td>
%% </td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td><td> R </td><td> G </td>
%% <td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'</td><td> R </td><td>
%% G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%%
%%  The red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in floating-point rather than integer format. 
%%
%%  Pixel ordering is such that lower x screen coordinates correspond to lower `i' filter
%% image coordinates, and lower y screen coordinates correspond to lower `j' filter
%% image coordinates. 
%%
%%  Note that after a convolution is performed, the resulting color components are also scaled
%% by their corresponding `?GL_POST_CONVOLUTION_c_SCALE' parameters and biased by their
%% corresponding `?GL_POST_CONVOLUTION_c_BIAS' parameters (where `c' takes on the
%% values `RED', `GREEN', `BLUE', and `ALPHA'). These parameters are
%% set by  {@link gl:pixelTransferf/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyConvolutionFilter2D.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a convolution filter is requested,  `Image'  is
%% treated as a byte offset into the buffer object's data store. 
%%
%%  Color components that are present in  `Format'  but not included in the internal format
%% of the filter are returned as zero. The assignments of internal color components to the
%% components of  `Format'  are as follows. <table><tbody><tr><td>` Internal Component '
%% </td><td>` Resulting Component '</td></tr></tbody><tbody><tr><td> Red </td><td> Red </td>
%% </tr><tr><td> Green </td><td> Green </td></tr><tr><td> Blue </td><td> Blue </td></tr><tr><td>
%%  Alpha </td><td> Alpha </td></tr><tr><td> Luminance </td><td> Red </td></tr><tr><td> Intensity
%% </td><td> Red </td></tr></tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionFilter.xml">external</a> documentation.
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
%% `?GL_CONVOLUTION_BORDER_MODE':  The convolution border mode. See  {@link gl:convolutionParameterf/3} 
%%  for a list of border modes. 
%%
%% `?GL_CONVOLUTION_BORDER_COLOR':  The current convolution border color.  `Params' 
%% must be a pointer to an array of four elements, which will receive the red, green, blue,
%% and alpha border colors. 
%%
%% `?GL_CONVOLUTION_FILTER_SCALE':  The current filter scale factors.  `Params' 
%% must be a pointer to an array of four elements, which will receive the red, green, blue,
%% and alpha filter scale factors in that order. 
%%
%% `?GL_CONVOLUTION_FILTER_BIAS':  The current filter bias factors.  `Params'  must
%% be a pointer to an array of four elements, which will receive the red, green, blue, and
%% alpha filter bias terms in that order. 
%%
%% `?GL_CONVOLUTION_FORMAT':  The current internal format. See  {@link gl:convolutionFilter1D/6} 
%% ,  {@link gl:convolutionFilter2D/7} , and  {@link gl:separableFilter2D/8}  for lists of allowable
%% formats. 
%%
%% `?GL_CONVOLUTION_WIDTH':  The current filter image width. 
%%
%% `?GL_CONVOLUTION_HEIGHT':  The current filter image height. 
%%
%% `?GL_MAX_CONVOLUTION_WIDTH':  The maximum acceptable filter image width. 
%%
%% `?GL_MAX_CONVOLUTION_HEIGHT':  The maximum acceptable filter image height. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetConvolutionParameter.xml">external</a> documentation.
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
%%  The pixel arrays specified by ( `Width' ,  `Format' ,  `Type' ,  `Row' )
%% and ( `Height' ,  `Format' ,  `Type' ,  `Column' ) are processed just as if
%% they had been passed to  {@link gl:drawPixels/5} , but processing stops after the final expansion
%% to RGBA is completed. 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a convolution filter is specified,  `Row'  and  `Column' 
%%  are treated as byte offsets into the buffer object's data store. 
%%
%%  Next, the R, G, B, and A components of all pixels in both arrays are scaled by the four
%% separable 2D `?GL_CONVOLUTION_FILTER_SCALE' parameters and biased by the four separable
%% 2D `?GL_CONVOLUTION_FILTER_BIAS' parameters. (The scale and bias parameters are set
%% by  {@link gl:convolutionParameterf/3}  using the `?GL_SEPARABLE_2D' target and the names
%% `?GL_CONVOLUTION_FILTER_SCALE' and `?GL_CONVOLUTION_FILTER_BIAS'. The parameters
%% themselves are vectors of four values that are applied to red, green, blue, and alpha,
%% in that order.) The R, G, B, and A values are not clamped to [0,1] at any time during
%% this process. 
%%
%%  Each pixel is then converted to the internal format specified by  `Internalformat' .
%% This conversion simply maps the component values of the pixel (R, G, B, and A) to the
%% values included in the internal format (red, green, blue, alpha, luminance, and intensity).
%% The mapping is as follows: <table><tbody><tr><td>` Internal Format '</td><td>` Red '
%% </td><td>` Green '</td><td>` Blue '</td><td>` Alpha '</td><td>` Luminance '
%% </td><td>` Intensity '</td></tr></tbody><tbody><tr><td>`?GL_LUMINANCE'</td><td></td>
%% <td></td><td></td><td></td><td> R </td><td></td></tr><tr><td>`?GL_LUMINANCE_ALPHA'</td>
%% <td></td><td></td><td></td><td> A </td><td> R </td><td></td></tr><tr><td>`?GL_INTENSITY'
%% </td><td></td><td></td><td></td><td></td><td></td><td> R </td></tr><tr><td>`?GL_RGB'</td>
%% <td> R </td><td> G </td><td> B </td><td></td><td></td><td></td></tr><tr><td>`?GL_RGBA'
%% </td><td> R </td><td> G </td><td> B </td><td> A </td><td></td><td></td></tr></tbody></table>
%% 
%%
%%  The red, green, blue, alpha, luminance, and/or intensity components of the resulting
%% pixels are stored in floating-point rather than integer format. They form two one-dimensional
%% filter kernel images. The row image is indexed by coordinate `i' starting at zero
%% and increasing from left to right. Each location in the row image is derived from element
%% `i' of  `Row' . The column image is indexed by coordinate `j' starting at
%% zero and increasing from bottom to top. Each location in the column image is derived from
%% element `j' of  `Column' . 
%%
%%  Note that after a convolution is performed, the resulting color components are also scaled
%% by their corresponding `?GL_POST_CONVOLUTION_c_SCALE' parameters and biased by their
%% corresponding `?GL_POST_CONVOLUTION_c_BIAS' parameters (where `c' takes on the
%% values `RED', `GREEN', `BLUE', and `ALPHA'). These parameters are
%% set by  {@link gl:pixelTransferf/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSeparableFilter2D.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a histogram table is requested,  `Values'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  Color components that are requested in the specified  `Format' , but which are not
%% included in the internal format of the histogram, are returned as zero. The assignments
%% of internal color components to the components requested by  `Format'  are: <table><tbody>
%% <tr><td>` Internal Component '</td><td>` Resulting Component '</td></tr></tbody>
%% <tbody><tr><td> Red </td><td> Red </td></tr><tr><td> Green </td><td> Green </td></tr><tr><td>
%%  Blue </td><td> Blue </td></tr><tr><td> Alpha </td><td> Alpha </td></tr><tr><td> Luminance
%% </td><td> Red </td></tr></tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogram.xml">external</a> documentation.
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
%% <table><tbody><tr><td>` Parameter '</td><td>` Description '</td></tr></tbody><tbody>
%% <tr><td>`?GL_HISTOGRAM_WIDTH'</td><td> Histogram table width </td></tr><tr><td>`?GL_HISTOGRAM_FORMAT'
%% </td><td> Internal format </td></tr><tr><td>`?GL_HISTOGRAM_RED_SIZE'</td><td> Red
%% component counter size, in bits </td></tr><tr><td>`?GL_HISTOGRAM_GREEN_SIZE'</td><td>
%%  Green component counter size, in bits </td></tr><tr><td>`?GL_HISTOGRAM_BLUE_SIZE'</td>
%% <td> Blue component counter size, in bits </td></tr><tr><td>`?GL_HISTOGRAM_ALPHA_SIZE'
%% </td><td> Alpha component counter size, in bits </td></tr><tr><td>`?GL_HISTOGRAM_LUMINANCE_SIZE'
%% </td><td> Luminance component counter size, in bits </td></tr><tr><td>`?GL_HISTOGRAM_SINK'
%% </td><td> Value of the `sink' parameter </td></tr></tbody></table>
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHistogramParameter.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while minimum and maximum pixel values are requested,  `Values' 
%%  is treated as a byte offset into the buffer object's data store. 
%%
%%  No pixel transfer operations are performed on the return values, but pixel storage modes
%% that are applicable to one-dimensional images are performed. Color components that are
%% requested in the specified  `Format' , but that are not included in the internal format
%% of the minmax table, are returned as zero. The assignment of internal color components
%% to the components requested by  `Format'  are as follows: 
%%
%% <table><tbody><tr><td>` Internal Component '</td><td>` Resulting Component '</td>
%% </tr></tbody><tbody><tr><td> Red </td><td> Red </td></tr><tr><td> Green </td><td> Green </td>
%% </tr><tr><td> Blue </td><td> Blue </td></tr><tr><td> Alpha </td><td> Alpha </td></tr><tr><td>
%%  Luminance </td><td> Red </td></tr></tbody></table>
%%
%%  If  `Reset'  is `?GL_TRUE', the minmax table entries corresponding to the return
%% values are reset to their initial values. Minimum and maximum values that are not returned
%% are not modified, even if  `Reset'  is `?GL_TRUE'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmax.xml">external</a> documentation.
-spec getMinmax(Target, Reset, Format, Types, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Types :: enum(),Values :: mem().
getMinmax(Target,Reset,Format,Types,Values) ->
  send_bin(Values),
  call(5351, <<Target:?GLenum,Reset:?GLboolean,0:24,Format:?GLenum,Types:?GLenum>>).

%% @doc Get minmax parameters
%%
%% ``gl:getMinmaxParameter'' retrieves parameters for the current minmax table by setting  `Pname' 
%%  to one of the following values: 
%%
%% <table><tbody><tr><td>` Parameter '</td><td>` Description '</td></tr></tbody><tbody>
%% <tr><td>`?GL_MINMAX_FORMAT'</td><td> Internal format of minmax table </td></tr><tr><td>
%% `?GL_MINMAX_SINK'</td><td> Value of the `sink' parameter </td></tr></tbody></table>
%% 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMinmaxParameter.xml">external</a> documentation.
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
%%  Histogramming is performed only for RGBA pixels (though these may be specified originally
%% as color indices and converted to RGBA by index table lookup). Histogramming is enabled
%% with  {@link gl:enable/1}  and disabled with  {@link gl:enable/1} . 
%%
%%  When  `Target'  is `?GL_HISTOGRAM', ``gl:histogram'' redefines the current
%% histogram table to have  `Width'  entries of the format specified by  `Internalformat' 
%% . The entries are indexed 0 through   width-1, and all entries are initialized to zero.
%% The values in the previous histogram table, if any, are lost. If  `Sink'  is `?GL_TRUE'
%% , then pixels are discarded after histogramming; no further processing of the pixels takes
%% place, and no drawing, texture loading, or pixel readback will result. 
%%
%%  When  `Target'  is `?GL_PROXY_HISTOGRAM', ``gl:histogram'' computes all state
%% information as if the histogram table were to be redefined, but does not actually define
%% the new table. If the requested histogram table is too large to be supported, then the
%% state information will be set to zero. This provides a way to determine if a histogram
%% table with the given parameters can be supported. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glHistogram.xml">external</a> documentation.
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
%% ``gl:minmax'' redefines the current minmax table to have entries of the format specified
%% by  `Internalformat' . The maximum element is initialized with the smallest possible
%% component values, and the minimum element is initialized with the largest possible component
%% values. The values in the previous minmax table, if any, are lost. If  `Sink'  is `?GL_TRUE'
%% , then pixels are discarded after minmax; no further processing of the pixels takes place,
%% and no drawing, texture loading, or pixel readback will result. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMinmax.xml">external</a> documentation.
-spec minmax(Target, Internalformat, Sink) -> 'ok' when Target :: enum(),Internalformat :: enum(),Sink :: 0|1.
minmax(Target,Internalformat,Sink) ->
  cast(5355, <<Target:?GLenum,Internalformat:?GLenum,Sink:?GLboolean>>).

%% @doc Reset histogram table entries to zero
%%
%% ``gl:resetHistogram'' resets all the elements of the current histogram table to zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetHistogram.xml">external</a> documentation.
-spec resetHistogram(Target) -> 'ok' when Target :: enum().
resetHistogram(Target) ->
  cast(5356, <<Target:?GLenum>>).

%% @doc Reset minmax table entries to initial values
%%
%% ``gl:resetMinmax'' resets the elements of the current minmax table to their initial
%% values: the ``maximum'' element receives the minimum possible component values, and the
%% ``minimum'' element receives the maximum possible component values. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResetMinmax.xml">external</a> documentation.
-spec resetMinmax(Target) -> 'ok' when Target :: enum().
resetMinmax(Target) ->
  cast(5357, <<Target:?GLenum>>).

%% @doc Select active texture unit
%%
%% ``gl:activeTexture'' selects which texture unit subsequent texture state calls will
%% affect. The number of texture units an implementation supports is implementation dependent,
%% but must be at least 80. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glActiveTexture.xml">external</a> documentation.
-spec activeTexture(Texture) -> 'ok' when Texture :: enum().
activeTexture(Texture) ->
  cast(5358, <<Texture:?GLenum>>).

%% @doc Specify multisample coverage parameters
%%
%%  Multisampling samples a pixel multiple times at various implementation-dependent subpixel
%% locations to generate antialiasing effects. Multisampling transparently antialiases points,
%% lines, polygons, and images if it is enabled. 
%%
%%  `Value'  is used in constructing a temporary mask used in determining which samples
%% will be used in resolving the final fragment color. This mask is bitwise-anded with the
%% coverage mask generated from the multisampling computation. If the  `Invert'  flag
%% is set, the temporary mask is inverted (all bits flipped) and then the bitwise-and is
%% computed. 
%%
%%  If an implementation does not have any multisample buffers available, or multisampling
%% is disabled, rasterization occurs with only a single sample computing a pixel's final
%% RGB color. 
%%
%%  Provided an implementation supports multisample buffers, and multisampling is enabled,
%% then a pixel's final color is generated by combining several samples per pixel. Each sample
%% contains color, depth, and stencil information, allowing those operations to be performed
%% on each sample. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSampleCoverage.xml">external</a> documentation.
-spec sampleCoverage(Value, Invert) -> 'ok' when Value :: clamp(),Invert :: 0|1.
sampleCoverage(Value,Invert) ->
  cast(5359, <<Value:?GLclampf,Invert:?GLboolean>>).

%% @doc Specify a three-dimensional texture image in a compressed format
%%
%%  Texturing allows elements of an image array to be read by shaders. 
%%
%% ``gl:compressedTexImage3D'' loads a previously defined, and retrieved, compressed three-dimensional
%% texture image if  `Target'  is `?GL_TEXTURE_3D' (see  {@link gl:texImage3D/10} ). 
%%
%%  If  `Target'  is `?GL_TEXTURE_2D_ARRAY',  `Data'  is treated as an array of
%% compressed 2D textures. 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_3D' or `?GL_PROXY_TEXTURE_2D_ARRAY',
%% no data is read from  `Data' , but all of the texture image state is recalculated,
%% checked for consistency, and checked against the implementation's capabilities. If the
%% implementation cannot handle a texture of the requested texture size, it sets all of the
%% image state to 0, but does not generate an error (see  {@link gl:getError/0} ). To query
%% for an entire mipmap array, use an image array level greater than or equal to 1. 
%%
%%  `Internalformat'  must be a known compressed image format (such as `?GL_RGTC')
%% or an extension-specified compressed-texture format. When a texture is loaded with  {@link gl:texImage2D/9} 
%%  using a generic compressed texture format (e.g., `?GL_COMPRESSED_RGB'), the GL selects
%% from one of its extensions supporting compressed textures. In order to load the compressed
%% texture image using ``gl:compressedTexImage3D'', query the compressed texture image's
%% size and format using  {@link gl:getTexLevelParameterfv/3} . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  If the compressed data are arranged into fixed-size blocks of texels, the pixel storage
%% modes can be used to select a sub-rectangle from a larger containing rectangle. These
%% pixel storage modes operate in the same way as they do for  {@link gl:texImage1D/8} . In
%% the following description, denote by  b s,  b w,  b h, and  b d, the values of pixel storage
%% modes `?GL_UNPACK_COMPRESSED_BLOCK_SIZE', `?GL_UNPACK_COMPRESSED_BLOCK_WIDTH', `?GL_UNPACK_COMPRESSED_BLOCK_HEIGHT'
%% , and `?GL_UNPACK_COMPRESSED_BLOCK_DEPTH', respectively.  b s is the compressed block
%% size in bytes;  b w,  b h, and  b d are the compressed block width, height, and depth
%% in pixels. 
%%
%%  By default the pixel storage modes `?GL_UNPACK_ROW_LENGTH', `?GL_UNPACK_SKIP_ROWS'
%% , `?GL_UNPACK_SKIP_PIXELS', `?GL_UNPACK_IMAGE_HEIGHT' and `?GL_UNPACK_SKIP_IMAGES'
%%  are ignored for compressed images. To enable `?GL_UNPACK_SKIP_PIXELS' and `?GL_UNPACK_ROW_LENGTH'
%% ,  b s and  b w must both be non-zero. To also enable `?GL_UNPACK_SKIP_ROWS' and `?GL_UNPACK_IMAGE_HEIGHT'
%% ,  b h must be non-zero. To also enable `?GL_UNPACK_SKIP_IMAGES',  b d must be non-zero.
%% All parameters must be consistent with the compressed format to produce the desired results.
%% 
%%
%%  When selecting a sub-rectangle from a compressed image: the value of `?GL_UNPACK_SKIP_PIXELS'
%%  must be a multiple of  b w;the value of `?GL_UNPACK_SKIP_ROWS' must be a multiple
%% of  b w;the value of `?GL_UNPACK_SKIP_IMAGES' must be a multiple of  b w.
%%
%%  `ImageSize'  must be equal to: 
%%
%%  b s×|width b/w|×|height b/h|×|depth b/d|
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage3D.xml">external</a> documentation.
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
%% ``gl:compressedTexImage2D'' loads a previously defined, and retrieved, compressed two-dimensional
%% texture image if  `Target'  is `?GL_TEXTURE_2D', or one of the cube map faces
%% such as `?GL_TEXTURE_CUBE_MAP_POSITIVE_X'. (see  {@link gl:texImage2D/9} ). 
%%
%%  If  `Target'  is `?GL_TEXTURE_1D_ARRAY',  `Data'  is treated as an array of
%% compressed 1D textures. 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_2D', `?GL_PROXY_TEXTURE_1D_ARRAY' or `?GL_PROXY_CUBE_MAP'
%% , no data is read from  `Data' , but all of the texture image state is recalculated,
%% checked for consistency, and checked against the implementation's capabilities. If the
%% implementation cannot handle a texture of the requested texture size, it sets all of the
%% image state to 0, but does not generate an error (see  {@link gl:getError/0} ). To query
%% for an entire mipmap array, use an image array level greater than or equal to 1. 
%%
%%  `Internalformat'  must be a known compressed image format (such as `?GL_RGTC')
%% or an extension-specified compressed-texture format. When a texture is loaded with  {@link gl:texImage2D/9} 
%%  using a generic compressed texture format (e.g., `?GL_COMPRESSED_RGB'), the GL selects
%% from one of its extensions supporting compressed textures. In order to load the compressed
%% texture image using ``gl:compressedTexImage2D'', query the compressed texture image's
%% size and format using  {@link gl:getTexLevelParameterfv/3} . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  If the compressed data are arranged into fixed-size blocks of texels, the pixel storage
%% modes can be used to select a sub-rectangle from a larger containing rectangle. These
%% pixel storage modes operate in the same way as they do for  {@link gl:texImage2D/9} . In
%% the following description, denote by  b s,  b w,  b h, and  b d, the values of pixel storage
%% modes `?GL_UNPACK_COMPRESSED_BLOCK_SIZE', `?GL_UNPACK_COMPRESSED_BLOCK_WIDTH', `?GL_UNPACK_COMPRESSED_BLOCK_HEIGHT'
%% , and `?GL_UNPACK_COMPRESSED_BLOCK_DEPTH', respectively.  b s is the compressed block
%% size in bytes;  b w,  b h, and  b d are the compressed block width, height, and depth
%% in pixels. 
%%
%%  By default the pixel storage modes `?GL_UNPACK_ROW_LENGTH', `?GL_UNPACK_SKIP_ROWS'
%% , `?GL_UNPACK_SKIP_PIXELS', `?GL_UNPACK_IMAGE_HEIGHT' and `?GL_UNPACK_SKIP_IMAGES'
%%  are ignored for compressed images. To enable `?GL_UNPACK_SKIP_PIXELS' and `?GL_UNPACK_ROW_LENGTH'
%% ,  b s and  b w must both be non-zero. To also enable `?GL_UNPACK_SKIP_ROWS' and `?GL_UNPACK_IMAGE_HEIGHT'
%% ,  b h must be non-zero. To also enable `?GL_UNPACK_SKIP_IMAGES',  b d must be non-zero.
%% All parameters must be consistent with the compressed format to produce the desired results.
%% 
%%
%%  When selecting a sub-rectangle from a compressed image: the value of `?GL_UNPACK_SKIP_PIXELS'
%%  must be a multiple of  b w;the value of `?GL_UNPACK_SKIP_ROWS' must be a multiple
%% of  b w.
%%
%%  `ImageSize'  must be equal to: 
%%
%%  b s×|width b/w|×|height b/h|
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage2D.xml">external</a> documentation.
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
%% ``gl:compressedTexImage1D'' loads a previously defined, and retrieved, compressed one-dimensional
%% texture image if  `Target'  is `?GL_TEXTURE_1D' (see  {@link gl:texImage1D/8} ). 
%%
%%  If  `Target'  is `?GL_PROXY_TEXTURE_1D', no data is read from  `Data' , but
%% all of the texture image state is recalculated, checked for consistency, and checked against
%% the implementation's capabilities. If the implementation cannot handle a texture of the
%% requested texture size, it sets all of the image state to 0, but does not generate an
%% error (see  {@link gl:getError/0} ). To query for an entire mipmap array, use an image array
%% level greater than or equal to 1. 
%%
%%  `Internalformat'  must be an extension-specified compressed-texture format. When a
%% texture is loaded with  {@link gl:texImage1D/8}  using a generic compressed texture format
%% (e.g., `?GL_COMPRESSED_RGB') the GL selects from one of its extensions supporting
%% compressed textures. In order to load the compressed texture image using ``gl:compressedTexImage1D''
%% , query the compressed texture image's size and format using  {@link gl:getTexLevelParameterfv/3} 
%% . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  If the compressed data are arranged into fixed-size blocks of texels, the pixel storage
%% modes can be used to select a sub-rectangle from a larger containing rectangle. These
%% pixel storage modes operate in the same way as they do for  {@link gl:texImage1D/8} . In
%% the following description, denote by  b s,  b w,  b h, and  b d, the values of pixel storage
%% modes `?GL_UNPACK_COMPRESSED_BLOCK_SIZE', `?GL_UNPACK_COMPRESSED_BLOCK_WIDTH', `?GL_UNPACK_COMPRESSED_BLOCK_HEIGHT'
%% , and `?GL_UNPACK_COMPRESSED_BLOCK_DEPTH', respectively.  b s is the compressed block
%% size in bytes;  b w,  b h, and  b d are the compressed block width, height, and depth
%% in pixels. 
%%
%%  By default the pixel storage modes `?GL_UNPACK_ROW_LENGTH', `?GL_UNPACK_SKIP_ROWS'
%% , `?GL_UNPACK_SKIP_PIXELS', `?GL_UNPACK_IMAGE_HEIGHT' and `?GL_UNPACK_SKIP_IMAGES'
%%  are ignored for compressed images. To enable `?GL_UNPACK_SKIP_PIXELS' and `?GL_UNPACK_ROW_LENGTH'
%% ,  b s and  b w must both be non-zero. To also enable `?GL_UNPACK_SKIP_ROWS' and `?GL_UNPACK_IMAGE_HEIGHT'
%% ,  b h must be non-zero. To also enable `?GL_UNPACK_SKIP_IMAGES',  b d must be non-zero.
%% All parameters must be consistent with the compressed format to produce the desired results.
%% 
%%
%%  When selecting a sub-rectangle from a compressed image: the value of `?GL_UNPACK_SKIP_PIXELS'
%%  must be a multiple of  b w;
%%
%%  `ImageSize'  must be equal to: 
%%
%%  b s×|width b/w|
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexImage1D.xml">external</a> documentation.
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
%% ``gl:compressedTexSubImage3D'' redefines a contiguous subregion of an existing three-dimensional
%% texture image. The texels referenced by  `Data'  replace the portion of the existing
%% texture array with x indices  `Xoffset'  and  xoffset+width-1, and the y indices  `Yoffset' 
%%  and  yoffset+height-1, and the z indices  `Zoffset'  and  zoffset+depth-1, inclusive.
%% This region may not include any texels outside the range of the texture array as it was
%% originally specified. It is not an error to specify a subtexture with width of 0, but
%% such a specification has no effect. 
%%
%%  `Internalformat'  must be a known compressed image format (such as `?GL_RGTC')
%% or an extension-specified compressed-texture format. The  `Format'  of the compressed
%% texture image is selected by the GL implementation that compressed it (see  {@link gl:texImage3D/10} 
%% ) and should be queried at the time the texture was compressed with  {@link gl:getTexLevelParameterfv/3} 
%% . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage3D.xml">external</a> documentation.
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
%% ``gl:compressedTexSubImage2D'' redefines a contiguous subregion of an existing two-dimensional
%% texture image. The texels referenced by  `Data'  replace the portion of the existing
%% texture array with x indices  `Xoffset'  and   xoffset+width-1, and the y indices  `Yoffset' 
%%  and   yoffset+height-1, inclusive.  This region may not include any texels outside the
%% range of the texture array as it was originally specified. It is not an error to specify
%% a subtexture with width of 0, but such a specification has no effect. 
%%
%%  `Internalformat'  must be a known compressed image format (such as `?GL_RGTC')
%% or an extension-specified compressed-texture format. The  `Format'  of the compressed
%% texture image is selected by the GL implementation that compressed it (see  {@link gl:texImage2D/9} 
%% ) and should be queried at the time the texture was compressed with  {@link gl:getTexLevelParameterfv/3} 
%% . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage2D.xml">external</a> documentation.
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
%% ``gl:compressedTexSubImage1D'' redefines a contiguous subregion of an existing one-dimensional
%% texture image. The texels referenced by  `Data'  replace the portion of the existing
%% texture array with x indices  `Xoffset'  and   xoffset+width-1, inclusive. This region
%% may not include any texels outside the range of the texture array as it was originally
%% specified. It is not an error to specify a subtexture with width of 0, but such a specification
%% has no effect. 
%%
%%  `Internalformat'  must be a known compressed image format (such as `?GL_RGTC')
%% or an extension-specified compressed-texture format. The  `Format'  of the compressed
%% texture image is selected by the GL implementation that compressed it (see  {@link gl:texImage1D/8} 
%% ), and should be queried at the time the texture was compressed with  {@link gl:getTexLevelParameterfv/3} 
%% . 
%%
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is specified,  `Data'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompressedTexSubImage1D.xml">external</a> documentation.
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
%%  If a non-zero named buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target
%%  (see  {@link gl:bindBuffer/2} ) while a texture image is requested,  `Img'  is treated
%% as a byte offset into the buffer object's data store. 
%%
%%  To minimize errors, first verify that the texture is compressed by calling  {@link gl:getTexLevelParameterfv/3} 
%%  with argument `?GL_TEXTURE_COMPRESSED'. If the texture is compressed, then determine
%% the amount of memory required to store the compressed texture by calling  {@link gl:getTexLevelParameterfv/3} 
%%  with argument `?GL_TEXTURE_COMPRESSED_IMAGE_SIZE'. Finally, retrieve the internal
%% format of the texture by calling  {@link gl:getTexLevelParameterfv/3}  with argument `?GL_TEXTURE_INTERNAL_FORMAT'
%% . To store the texture for later use, associate the internal format and size with the
%% retrieved texture image. These data can be used by the respective texture or subtexture
%% loading routine used for loading  `Target'  textures. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetCompressedTexImage.xml">external</a> documentation.
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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClientActiveTexture.xml">external</a> documentation.
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
%%  The current texture coordinates are part of the data that is associated with each vertex
%% and with the current raster position. Initially, the values for (s t r q) are  (0 0 0 1). 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiTexCoord.xml">external</a> documentation.
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
%%  The current matrix, M, defines a transformation of coordinates. For instance, assume
%% M refers to the modelview matrix. If   v=(v[0] v[1] v[2] v[3]) is the set of object coordinates of a vertex,
%% and  `M'  points to an array of   16 single- or double-precision floating-point values
%%   m={m[0] m[1] ... m[15]}, then the modelview transformation   M(v) does the following: 
%%
%%  M(v)=(m[0] m[1] m[2] m[3] m[4] m[5] m[6] m[7] m[8] m[9] m[10] m[11] m[12] m[13] m[14] m[15])×(v[0] v[1] v[2] v[3])
%%
%%  Projection and texture transformations are similarly defined. 
%%
%%  Calling ``gl:loadTransposeMatrix'' with matrix   M is identical in operation to  {@link gl:loadMatrixd/1} 
%%  with   M T, where   T represents the transpose. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrix.xml">external</a> documentation.
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
%%  The current matrix is determined by the current matrix mode (see  {@link gl:matrixMode/1} ).
%% It is either the projection matrix, modelview matrix, or the texture matrix. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrix.xml">external</a> documentation.
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
%% ``gl:blendFuncSeparate'' defines the operation of blending for all draw buffers when
%% it is enabled. ``gl:blendFuncSeparatei'' defines the operation of blending for a single
%% draw buffer specified by  `Buf'  when enabled for that draw buffer.  `SrcRGB'  specifies
%% which method is used to scale the source RGB-color components.  `DstRGB'  specifies
%% which method is used to scale the destination RGB-color components. Likewise,  `SrcAlpha' 
%%  specifies which method is used to scale the source alpha color component, and  `DstAlpha' 
%%  specifies which method is used to scale the destination alpha component. The possible
%% methods are described in the following table. Each method defines four scale factors,
%% one each for red, green, blue, and alpha. 
%%
%%  In the table and in subsequent equations, first source, second source and destination
%% color components are referred to as (R s0 G s0 B s0 A s0), (R s1 G s1 B s1 A s1), and (R d G d B d A d), respectively. The color specified by  {@link gl:blendColor/4} 
%%  is referred to as (R c G c B c A c). They are understood to have integer values between 0 and (k R k G k B
%% k A), where 
%%
%%  k c=2(m c)-1
%%
%%  and (m R m G m B m A) is the number of red, green, blue, and alpha bitplanes. 
%%
%%  Source and destination scale factors are referred to as (s R s G s B s A) and (d R d G d B d A). All scale factors have
%% range  [0 1]. 
%%
%% <table><tbody><tr><td>` Parameter '</td><td>` RGB Factor '</td><td>` Alpha Factor '
%% </td></tr></tbody><tbody><tr><td>`?GL_ZERO'</td><td>(0 0 0)</td><td> 0</td></tr><tr><td>`?GL_ONE'
%% </td><td>(1 1 1)</td><td> 1</td></tr><tr><td>`?GL_SRC_COLOR'</td><td>(R s0 k/R G s0 k/G B s0
%% k/B)</td><td> A s0 k/A</td>
%% </tr><tr><td>`?GL_ONE_MINUS_SRC_COLOR'</td><td>(1 1 1 1)-(R s0 k/R G s0 k/G B s0 k/B)</td><td> 1-A s0 k/A</td></tr><tr><td>
%% `?GL_DST_COLOR'</td><td>(R d k/R G d k/G B d k/B)</td><td> A d k/A</td></tr><tr><td>`?GL_ONE_MINUS_DST_COLOR'
%% </td><td>(1 1 1)-(R d k/R G d k/G B d k/B)</td><td> 1-A d k/A</td></tr><tr><td>`?GL_SRC_ALPHA'</td><td>(A s0 k/A A s0
%% k/A A s0 k/A)</td><td> A
%% s0 k/A</td></tr><tr><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td>(1 1 1)-(A s0 k/A A s0 k/A A s0 k/A
%% )</td><td> 1-A s0 k/A</td></tr>
%% <tr><td>`?GL_DST_ALPHA'</td><td>(A d k/A A d k/A A d k/A)</td><td> A d k/A</td></tr><tr><td>`?GL_ONE_MINUS_DST_ALPHA'
%% </td><td>(1 1 1)-(A d k/A A d k/A A d k/A)</td><td> 1-A d k/A</td></tr><tr><td>`?GL_CONSTANT_COLOR'</td><td>(R c G c
%% B c)</td><td>
%%  A c</td></tr><tr><td>`?GL_ONE_MINUS_CONSTANT_COLOR'</td><td>(1 1 1)-(R c G c B c)</td><td> 1-A c</td></tr>
%% <tr><td>`?GL_CONSTANT_ALPHA'</td><td>(A c A c A c)</td><td> A c</td></tr><tr><td>`?GL_ONE_MINUS_CONSTANT_ALPHA'
%% </td><td>(1 1 1)-(A c A c A c)</td><td> 1-A c</td></tr><tr><td>`?GL_SRC_ALPHA_SATURATE'</td><td>(i i i)</td><td>
%%  1</td></tr><tr><td>`?GL_SRC1_COLOR'</td><td>(R s1 k/R G s1 k/G B s1 k/B)</td><td> A s1 k/A</td></tr><tr><td>`?GL_ONE_MINUS_SRC_COLOR'
%% </td><td>(1 1 1 1)-(R s1 k/R G s1 k/G B s1 k/B)</td><td> 1-A s1 k/A</td></tr><tr><td>`?GL_SRC1_ALPHA'</td><td>(A s1 k/A A
%% s1 k/A A s1 k/A)</td><td> A
%% s1 k/A</td></tr><tr><td>`?GL_ONE_MINUS_SRC_ALPHA'</td><td>(1 1 1)-(A s1 k/A A s1 k/A A s1 k/A
%% )</td><td> 1-A s1 k/A</td></tr>
%% </tbody></table>
%%
%%  In the table, 
%%
%%  i=min(A s 1-(A d))
%%
%%  To determine the blended RGBA values of a pixel, the system uses the following equations:
%% 
%%
%%  R d=min(k R R s  s R+R d  d R) G d=min(k G G s  s G+G d  d G) B d=min(k B B s  s B+B d  d B) A d=min(k A A s  s A+A d  d A)
%%
%%  Despite the apparent precision of the above equations, blending arithmetic is not exactly
%% specified, because blending operates with imprecise integer color values. However, a blend
%% factor that should be equal to 1 is guaranteed not to modify its multiplicand, and a blend
%% factor equal to 0 reduces its multiplicand to 0. For example, when  `SrcRGB'  is `?GL_SRC_ALPHA'
%% ,  `DstRGB'  is `?GL_ONE_MINUS_SRC_ALPHA', and   A s is equal to   k A, the equations
%% reduce to simple replacement: 
%%
%%  R d=R s G d=G s B d=B s A d=A s
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFuncSeparate.xml">external</a> documentation.
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
%% ``gl:multiDrawArrays'' behaves identically to  {@link gl:drawArrays/3}  except that  `Primcount' 
%%  separate ranges of elements are specified instead. 
%%
%%  When ``gl:multiDrawArrays'' is called, it uses  `Count'  sequential elements from
%% each enabled array to construct a sequence of geometric primitives, beginning with element
%%  `First' .  `Mode'  specifies what kind of primitives are constructed, and how the
%% array elements construct those primitives. 
%%
%%  Vertex attributes that are modified by ``gl:multiDrawArrays'' have an unspecified value
%% after ``gl:multiDrawArrays'' returns. Attributes that aren't modified remain well defined.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultiDrawArrays.xml">external</a> documentation.
-spec multiDrawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: [integer()],Count :: [integer()].
multiDrawArrays(Mode,First,Count) ->
  cast(5395, <<Mode:?GLenum,(length(First)):?GLuint,
        (<< <<C:?GLint>> || C <- First>>)/binary,0:(((length(First)) rem 2)*32),(length(Count)):?GLuint,
        (<< <<C:?GLsizei>> || C <- Count>>)/binary,0:(((1+length(Count)) rem 2)*32)>>).

%% @doc Specify point parameters
%%
%%  The following values are accepted for  `Pname' : 
%%
%% `?GL_POINT_FADE_THRESHOLD_SIZE':  `Params'  is a single floating-point value that
%% specifies the threshold value to which point sizes are clamped if they exceed the specified
%% value. The default value is 1.0. 
%%
%% `?GL_POINT_SPRITE_COORD_ORIGIN':  `Params'  is a single enum specifying the point
%% sprite texture coordinate origin, either `?GL_LOWER_LEFT' or `?GL_UPPER_LEFT'.
%%  The default value is `?GL_UPPER_LEFT'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPointParameter.xml">external</a> documentation.
-spec pointParameterf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pointParameterf(Pname,Param) ->
  cast(5396, <<Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameterfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameterfv(Pname,Params) ->
  cast(5397, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameteri(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pointParameteri(Pname,Param) ->
  cast(5398, <<Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link pointParameterf/2}
-spec pointParameteriv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameteriv(Pname,Params) ->
  cast(5399, <<Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((0+size(Params)) rem 2)*32)>>).

%% @doc Set the current fog coordinates
%%
%% ``gl:fogCoord'' specifies the fog coordinate that is associated with each vertex and
%% the current raster position. The value specified is interpolated and used in computing
%% the fog color (see  {@link gl:fogf/2} ). 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoord.xml">external</a> documentation.
-spec fogCoordf(Coord) -> 'ok' when Coord :: float().
fogCoordf(Coord) ->
  cast(5400, <<Coord:?GLfloat>>).

%% @equiv fogCoordf(Coord)
-spec fogCoordfv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoordfv({Coord}) ->  fogCoordf(Coord).

%% @doc 
%% See {@link fogCoordf/1}
-spec fogCoordd(Coord) -> 'ok' when Coord :: float().
fogCoordd(Coord) ->
  cast(5401, <<Coord:?GLdouble>>).

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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a fog coordinate array is specified,  `Pointer'  is treated as a byte offset
%% into the buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as fog coordinate vertex array client-side state (`?GL_FOG_COORD_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When a fog coordinate array is specified,  `Type' ,  `Stride' , and  `Pointer' 
%% are saved as client-side state, in addition to the current vertex array buffer object
%% binding. 
%%
%%  To enable and disable the fog coordinate array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_FOG_COORD_ARRAY'. If enabled, the fog coordinate array is
%% used when  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} , see `glMultiDrawElements'
%% ,  {@link gl:drawRangeElements/6} , or  {@link gl:arrayElement/1}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFogCoordPointer.xml">external</a> documentation.
-spec fogCoordPointer(Type, Stride, Pointer) -> 'ok' when Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
fogCoordPointer(Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5402, <<Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
fogCoordPointer(Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5403, <<Type:?GLenum,Stride:?GLsizei>>).

%% @doc Set the current secondary color
%%
%%  The GL stores both a primary four-valued RGBA color and a secondary four-valued RGBA
%% color (where alpha is always set to 0.0) that is associated with every vertex. 
%%
%%  The secondary color is interpolated and applied to each fragment during rasterization
%% when `?GL_COLOR_SUM' is enabled. When lighting is enabled, and `?GL_SEPARATE_SPECULAR_COLOR'
%%  is specified, the value of the secondary color is assigned the value computed from the
%% specular term of the lighting computation. Both the primary and secondary current colors
%% are applied to each fragment, regardless of the state of `?GL_COLOR_SUM', under such
%% conditions. When `?GL_SEPARATE_SPECULAR_COLOR' is specified, the value returned from
%% querying the current secondary color is undefined. 
%%
%% ``gl:secondaryColor3b'', ``gl:secondaryColor3s'', and ``gl:secondaryColor3i'' take
%% three signed byte, short, or long integers as arguments. When `v' is appended to
%% the name, the color commands can take a pointer to an array of such values. 
%%
%%  Color values are stored in floating-point format, with unspecified mantissa and exponent
%% sizes. Unsigned integer color components, when specified, are linearly mapped to floating-point
%% values such that the largest representable value maps to 1.0 (full intensity), and 0 maps
%% to 0.0 (zero intensity). Signed integer color components, when specified, are linearly
%% mapped to floating-point values such that the most positive representable value maps to
%% 1.0, and the most negative representable value maps to   -1.0. (Note that this mapping
%% does not convert 0 precisely to 0.0). Floating-point values are mapped directly. 
%%
%%  Neither floating-point nor signed integer values are clamped to the range [0 1] before the
%% current color is updated. However, color components are clamped to this range before they
%% are interpolated or written into a color buffer. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColor.xml">external</a> documentation.
-spec secondaryColor3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3b(Red,Green,Blue) ->
  cast(5404, <<Red:?GLbyte,Green:?GLbyte,Blue:?GLbyte>>).

%% @equiv secondaryColor3b(Red,Green,Blue)
-spec secondaryColor3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3d(Red,Green,Blue) ->
  cast(5405, <<Red:?GLdouble,Green:?GLdouble,Blue:?GLdouble>>).

%% @equiv secondaryColor3d(Red,Green,Blue)
-spec secondaryColor3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3f(Red,Green,Blue) ->
  cast(5406, <<Red:?GLfloat,Green:?GLfloat,Blue:?GLfloat>>).

%% @equiv secondaryColor3f(Red,Green,Blue)
-spec secondaryColor3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3i(Red,Green,Blue) ->
  cast(5407, <<Red:?GLint,Green:?GLint,Blue:?GLint>>).

%% @equiv secondaryColor3i(Red,Green,Blue)
-spec secondaryColor3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3s(Red,Green,Blue) ->
  cast(5408, <<Red:?GLshort,Green:?GLshort,Blue:?GLshort>>).

%% @equiv secondaryColor3s(Red,Green,Blue)
-spec secondaryColor3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ub(Red,Green,Blue) ->
  cast(5409, <<Red:?GLubyte,Green:?GLubyte,Blue:?GLubyte>>).

%% @equiv secondaryColor3ub(Red,Green,Blue)
-spec secondaryColor3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ui(Red,Green,Blue) ->
  cast(5410, <<Red:?GLuint,Green:?GLuint,Blue:?GLuint>>).

%% @equiv secondaryColor3ui(Red,Green,Blue)
-spec secondaryColor3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).

%% @doc 
%% See {@link secondaryColor3b/3}
-spec secondaryColor3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3us(Red,Green,Blue) ->
  cast(5411, <<Red:?GLushort,Green:?GLushort,Blue:?GLushort>>).

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
%%  If a non-zero named buffer object is bound to the `?GL_ARRAY_BUFFER' target  (see  {@link gl:bindBuffer/2} 
%% ) while a secondary color array is specified,  `Pointer'  is treated as a byte offset
%% into the buffer object's data store. Also, the buffer object binding (`?GL_ARRAY_BUFFER_BINDING'
%% ) is saved as secondary color vertex array client-side state (`?GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING'
%% ). 
%%
%%  When a secondary color array is specified,  `Size' ,  `Type' ,  `Stride' , and  `Pointer' 
%%  are saved as client-side state, in addition to the current vertex array buffer object
%% binding. 
%%
%%  To enable and disable the secondary color array, call  {@link gl:enableClientState/1}  and  {@link gl:enableClientState/1} 
%%  with the argument `?GL_SECONDARY_COLOR_ARRAY'. If enabled, the secondary color array
%% is used when  {@link gl:arrayElement/1} ,  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,
%%  {@link gl:drawElements/4} , see `glMultiDrawElements', or  {@link gl:drawRangeElements/6} 
%% is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSecondaryColorPointer.xml">external</a> documentation.
-spec secondaryColorPointer(Size, Type, Stride, Pointer) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
secondaryColorPointer(Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5412, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
secondaryColorPointer(Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5413, <<Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc Specify the raster position in window coordinates for pixel operations
%%
%%  The GL maintains a 3D position in window coordinates. This position, called the raster
%% position, is used to position pixel and bitmap write operations. It is maintained with
%% subpixel accuracy. See  {@link gl:bitmap/7} ,  {@link gl:drawPixels/5} , and  {@link gl:copyPixels/5} 
%% . 
%%
%% ``gl:windowPos2'' specifies the   x and   y coordinates, while   z is implicitly set
%% to 0. ``gl:windowPos3'' specifies all three coordinates. The   w coordinate of the current
%% raster position is always set to 1.0. 
%%
%% ``gl:windowPos'' directly updates the   x and   y coordinates of the current raster
%% position with the values specified. That is, the values are neither transformed by the
%% current modelview and projection matrices, nor by the viewport-to-window transform. The  
%% z coordinate of the current raster position is updated in the following manner: 
%%
%%  z={n f(n+z×(f-n))  if  z&lt;= 0 if  z&gt;= 1(otherwise))
%%
%%  where   n is `?GL_DEPTH_RANGE''s near value, and   f is `?GL_DEPTH_RANGE''s
%% far value. See  {@link gl:depthRange/2} . 
%%
%%  The specified coordinates are not clip-tested, causing the raster position to always
%% be valid. 
%%
%%  The current raster position also includes some associated color data and texture coordinates.
%% If lighting is enabled, then `?GL_CURRENT_RASTER_COLOR' (in RGBA mode) or `?GL_CURRENT_RASTER_INDEX'
%%  (in color index mode) is set to the color produced by the lighting calculation (see  {@link gl:lightf/3} 
%% ,  {@link gl:lightModelf/2} , and  {@link gl:shadeModel/1} ). If lighting is disabled, current
%% color (in RGBA mode, state variable `?GL_CURRENT_COLOR') or color index (in color
%% index mode, state variable `?GL_CURRENT_INDEX') is used to update the current raster
%% color. `?GL_CURRENT_RASTER_SECONDARY_COLOR' (in RGBA mode) is likewise updated. 
%%
%%  Likewise, `?GL_CURRENT_RASTER_TEXTURE_COORDS' is updated as a function of `?GL_CURRENT_TEXTURE_COORDS'
%% , based on the texture matrix and the texture generation functions (see  {@link gl:texGend/3} ).
%% The `?GL_CURRENT_RASTER_DISTANCE' is set to the `?GL_CURRENT_FOG_COORD'. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWindowPos.xml">external</a> documentation.
-spec windowPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2d(X,Y) ->
  cast(5414, <<X:?GLdouble,Y:?GLdouble>>).

%% @equiv windowPos2d(X,Y)
-spec windowPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2f(X,Y) ->
  cast(5415, <<X:?GLfloat,Y:?GLfloat>>).

%% @equiv windowPos2f(X,Y)
-spec windowPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2i(X,Y) ->
  cast(5416, <<X:?GLint,Y:?GLint>>).

%% @equiv windowPos2i(X,Y)
-spec windowPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2s(X,Y) ->
  cast(5417, <<X:?GLshort,Y:?GLshort>>).

%% @equiv windowPos2s(X,Y)
-spec windowPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3d(X,Y,Z) ->
  cast(5418, <<X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @equiv windowPos3d(X,Y,Z)
-spec windowPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3f(X,Y,Z) ->
  cast(5419, <<X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @equiv windowPos3f(X,Y,Z)
-spec windowPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3i(X,Y,Z) ->
  cast(5420, <<X:?GLint,Y:?GLint,Z:?GLint>>).

%% @equiv windowPos3i(X,Y,Z)
-spec windowPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).

%% @doc 
%% See {@link windowPos2d/2}
-spec windowPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3s(X,Y,Z) ->
  cast(5421, <<X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @equiv windowPos3s(X,Y,Z)
-spec windowPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).

%% @doc Generate query object names
%%
%% ``gl:genQueries'' returns  `N'  query object names in  `Ids' . There is no guarantee
%% that the names form a contiguous set of integers; however, it is guaranteed that none
%% of the returned names was in use immediately before the call to ``gl:genQueries''. 
%%
%%  Query object names returned by a call to ``gl:genQueries'' are not returned by subsequent
%% calls, unless they are first deleted with  {@link gl:deleteQueries/1} .  
%%
%%  No query objects are associated with the returned query object names until they are first
%% used by calling  {@link gl:beginQuery/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenQueries.xml">external</a> documentation.
-spec genQueries(N) -> [integer()] when N :: integer().
genQueries(N) ->
  call(5422, <<N:?GLsizei>>).

%% @doc Delete named query objects
%%
%% ``gl:deleteQueries'' deletes  `N'  query objects named by the elements of the array  `Ids' 
%% . After a query object is deleted, it has no contents, and its name is free for reuse
%% (for example by  {@link gl:genQueries/1} ). 
%%
%% ``gl:deleteQueries'' silently ignores 0's and names that do not correspond to existing
%% query objects. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteQueries.xml">external</a> documentation.
-spec deleteQueries(Ids) -> 'ok' when Ids :: [integer()].
deleteQueries(Ids) ->
  cast(5423, <<(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+length(Ids)) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a query object
%%
%% ``gl:isQuery'' returns `?GL_TRUE' if  `Id'  is currently the name of a query
%% object. If  `Id'  is zero, or is a non-zero value that is not currently the name of
%% a query object, or if an error occurs, ``gl:isQuery'' returns `?GL_FALSE'. 
%%
%%  A name returned by  {@link gl:genQueries/1} , but not yet associated with a query object
%% by calling  {@link gl:beginQuery/2} , is not the name of a query object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsQuery.xml">external</a> documentation.
-spec isQuery(Id) -> 0|1 when Id :: integer().
isQuery(Id) ->
  call(5424, <<Id:?GLuint>>).

%% @doc Delimit the boundaries of a query object
%%
%% ``gl:beginQuery'' and  {@link gl:beginQuery/2}  delimit the boundaries of a query object.  `Query' 
%%  must be a name previously returned from a call to  {@link gl:genQueries/1} . If a query
%% object with name  `Id'  does not yet exist it is created with the type determined by  `Target' 
%% .  `Target'  must be one of `?GL_SAMPLES_PASSED', `?GL_ANY_SAMPLES_PASSED', `?GL_PRIMITIVES_GENERATED'
%% , `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN', or `?GL_TIME_ELAPSED'. The behavior
%% of the query object depends on its type and is as follows. 
%%
%%  If  `Target'  is `?GL_SAMPLES_PASSED',  `Id'  must be an unused name, or the
%% name of an existing occlusion query object. When ``gl:beginQuery'' is executed, the
%% query object's samples-passed counter is reset to 0. Subsequent rendering will increment
%% the counter for every sample that passes the depth test. If the value of `?GL_SAMPLE_BUFFERS'
%%  is 0, then the samples-passed count is incremented by 1 for each fragment. If the value
%% of `?GL_SAMPLE_BUFFERS' is 1, then the samples-passed count is incremented by the
%% number of samples whose coverage bit is set. However, implementations, at their discression
%% may instead increase the samples-passed count by the value of `?GL_SAMPLES' if any
%% sample in the fragment is covered. When ``gl:endQuery'' is executed, the samples-passed
%% counter is assigned to the query object's result value. This value can be queried by calling
%%  {@link gl:getQueryObjectiv/2}  with  `Pname' `?GL_QUERY_RESULT'. 
%%
%%  If  `Target'  is `?GL_ANY_SAMPLES_PASSED',  `Id'  must be an unused name,
%% or the name of an existing boolean occlusion query object. When ``gl:beginQuery'' is
%% executed, the query object's samples-passed flag is reset to `?GL_FALSE'. Subsequent
%% rendering causes the flag to be set to `?GL_TRUE' if any sample passes the depth
%% test. When ``gl:endQuery'' is executed, the samples-passed flag is assigned to the query
%% object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%% with  `Pname' `?GL_QUERY_RESULT'. 
%%
%%  If  `Target'  is `?GL_PRIMITIVES_GENERATED',  `Id'  must be an unused name,
%% or the name of an existing primitive query object previously bound to the `?GL_PRIMITIVES_GENERATED'
%%  query binding. When ``gl:beginQuery'' is executed, the query object's primitives-generated
%% counter is reset to 0. Subsequent rendering will increment the counter once for every
%% vertex that is emitted from the geometry shader, or from the vertex shader if no geometry
%% shader is present. When ``gl:endQuery'' is executed, the primitives-generated counter
%% is assigned to the query object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%%  with  `Pname' `?GL_QUERY_RESULT'. 
%%
%%  If  `Target'  is `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN',  `Id'  must
%% be an unused name, or the name of an existing primitive query object previously bound
%% to the `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN' query binding. When ``gl:beginQuery''
%%  is executed, the query object's primitives-written counter is reset to 0. Subsequent
%% rendering will increment the counter once for every vertex that is written into the bound
%% transform feedback buffer(s). If transform feedback mode is not activated between the
%% call to ``gl:beginQuery'' and ``gl:endQuery'', the counter will not be incremented.
%% When ``gl:endQuery'' is executed, the primitives-written counter is assigned to the
%% query object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%%  with  `Pname' `?GL_QUERY_RESULT'. 
%%
%%  If  `Target'  is `?GL_TIME_ELAPSED',  `Id'  must be an unused name, or the
%% name of an existing timer query object previously bound to the `?GL_TIME_ELAPSED'
%% query binding. When ``gl:beginQuery'' is executed, the query object's time counter is
%% reset to 0. When ``gl:endQuery'' is executed, the elapsed server time that has passed
%% since the call to ``gl:beginQuery'' is written into the query object's time counter.
%% This value can be queried by calling  {@link gl:getQueryObjectiv/2}  with  `Pname' `?GL_QUERY_RESULT'
%% . 
%%
%%  Querying the `?GL_QUERY_RESULT' implicitly flushes the GL pipeline until the rendering
%% delimited by the query object has completed and the result is available. `?GL_QUERY_RESULT_AVAILABLE'
%%  can be queried to determine if the result is immediately available or if the rendering
%% is not yet complete. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQuery.xml">external</a> documentation.
-spec beginQuery(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
beginQuery(Target,Id) ->
  cast(5425, <<Target:?GLenum,Id:?GLuint>>).

%% @doc 
%% See {@link beginQuery/2}
-spec endQuery(Target) -> 'ok' when Target :: enum().
endQuery(Target) ->
  cast(5426, <<Target:?GLenum>>).

%% @doc glGetQuery
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQuery.xml">external</a> documentation.
-spec getQueryiv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getQueryiv(Target,Pname) ->
  call(5427, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Return parameters of a query object
%%
%% ``gl:getQueryObject'' returns in  `Params'  a selected parameter of the query object
%% specified by  `Id' . 
%%
%%  `Pname'  names a specific query object parameter.  `Pname'  can be as follows: 
%%
%% `?GL_QUERY_RESULT':  `Params'  returns the value of the query object's passed
%% samples counter.  The initial value is 0. 
%%
%% `?GL_QUERY_RESULT_AVAILABLE':  `Params'  returns whether the passed samples counter
%% is immediately available. If a delay would occur waiting for the query result, `?GL_FALSE'
%%  is returned.  Otherwise, `?GL_TRUE' is returned, which also indicates that the results
%% of all previous queries are available as well. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObject.xml">external</a> documentation.
-spec getQueryObjectiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectiv(Id,Pname) ->
  call(5428, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getQueryObjectiv/2}
-spec getQueryObjectuiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectuiv(Id,Pname) ->
  call(5429, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc Bind a named buffer object
%%
%% ``gl:bindBuffer'' binds a buffer object to the specified buffer binding point. Calling ``gl:bindBuffer''
%%  with  `Target'  set to one of the accepted symbolic constants and  `Buffer'  set
%% to the name of a buffer object binds that buffer object name to the target. If no buffer
%% object with name  `Buffer'  exists, one is created with that name. When a buffer object
%% is bound to a target, the previous binding for that target is automatically broken. 
%%
%%  Buffer object names are unsigned integers. The value zero is reserved, but there is no
%% default buffer object for each buffer object target. Instead,  `Buffer'  set to zero
%% effectively unbinds any buffer object previously bound, and restores client memory usage
%% for that buffer object target (if supported for that target). Buffer object names and
%% the corresponding buffer object contents are local to the shared object space of the current
%% GL rendering context; two rendering contexts share buffer object names only if they explicitly
%% enable sharing between contexts through the appropriate GL windows interfaces functions. 
%%
%%  {@link gl:genBuffers/1}  must be used to generate a set of unused buffer object names. 
%%
%%  The state of a buffer object immediately after it is first bound is an unmapped zero-sized
%% memory buffer with `?GL_READ_WRITE' access and `?GL_STATIC_DRAW' usage. 
%%
%%  While a non-zero buffer object name is bound, GL operations on the target to which it
%% is bound affect the bound buffer object, and queries of the target to which it is bound
%% return state  from the bound buffer object. While buffer object name zero is bound, as
%% in the initial state, attempts to modify or query state on the target to which it is bound
%% generates an  `?GL_INVALID_OPERATION' error. 
%%
%%  When a non-zero buffer object is bound to the `?GL_ARRAY_BUFFER' target,  the vertex
%% array pointer parameter is interpreted as an offset within the buffer object measured
%% in basic machine units. 
%%
%%  When a non-zero buffer object is bound to the `?GL_DRAW_INDIRECT_BUFFER' target,
%% parameters for draws issued through  {@link gl:drawArraysIndirect/2}  and  {@link gl:drawElementsIndirect/3} 
%%  are sourced from that buffer object. 
%%
%%  While a non-zero buffer object is bound to the `?GL_ELEMENT_ARRAY_BUFFER' target,
%%  the indices parameter of  {@link gl:drawElements/4} ,   {@link gl:drawElementsInstanced/5} ,  {@link gl:drawElementsBaseVertex/5} 
%% ,   {@link gl:drawRangeElements/6} ,   {@link gl:drawRangeElementsBaseVertex/7} , see `glMultiDrawElements'
%% , or  see `glMultiDrawElementsBaseVertex' is interpreted as an  offset within the
%% buffer object measured in basic machine units. 
%%
%%  While a non-zero buffer object is bound to the `?GL_PIXEL_PACK_BUFFER' target, 
%% the following commands are affected:  {@link gl:getCompressedTexImage/3} ,   {@link gl:getTexImage/5} 
%% , and   {@link gl:readPixels/7} . The pointer parameter is interpreted as an offset within
%% the buffer object measured in basic machine units. 
%%
%%  While a non-zero buffer object is bound to the `?GL_PIXEL_UNPACK_BUFFER' target,
%%  the following commands are affected:   {@link gl:compressedTexImage1D/7} ,  {@link gl:compressedTexImage2D/8} 
%% ,  {@link gl:compressedTexImage3D/9} ,  {@link gl:compressedTexSubImage1D/7} ,  {@link gl:compressedTexSubImage2D/9} 
%% ,  {@link gl:compressedTexSubImage3D/11} ,  {@link gl:texImage1D/8} ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} 
%% ,  {@link gl:texSubImage1D/7} ,  {@link gl:texSubImage1D/7} , and   {@link gl:texSubImage1D/7} .
%% The pointer parameter is  interpreted as an offset within the buffer object measured in
%% basic machine units. 
%%
%%  The buffer targets `?GL_COPY_READ_BUFFER' and `?GL_COPY_WRITE_BUFFER' are provided
%% to allow  {@link gl:copyBufferSubData/5}  to be used without disturbing the state of other
%% bindings. However,  {@link gl:copyBufferSubData/5}  may be used with any pair of buffer binding
%% points. 
%%
%%  The `?GL_TRANSFORM_FEEDBACK_BUFFER' buffer binding point may be passed to ``gl:bindBuffer''
%% , but will not directly affect transform feedback state. Instead, the indexed `?GL_TRANSFORM_FEEDBACK_BUFFER'
%%  bindings must be used through a call to  {@link gl:bindBufferBase/3}  or  {@link gl:bindBufferRange/5} 
%% . This will affect the generic `?GL_TRANSFORM_FEEDABCK_BUFFER' binding. 
%%
%%  Likewise, the `?GL_UNIFORM_BUFFER' and `?GL_ATOMIC_COUNTER_BUFFER' buffer binding
%% points may be used, but do not directly affect uniform buffer or atomic counter buffer
%% state, respectively.  {@link gl:bindBufferBase/3}  or  {@link gl:bindBufferRange/5}  must be
%% used to bind a buffer to an indexed uniform buffer or atomic counter buffer binding point.
%% 
%%
%%  A buffer object binding created with ``gl:bindBuffer'' remains active until a different
%% buffer object name is bound to the same target, or until the bound buffer object is deleted
%% with  {@link gl:deleteBuffers/1} . 
%%
%%  Once created, a named buffer object may be re-bound to any target as often as needed.
%% However, the GL implementation may make choices about how to optimize the storage of a
%% buffer object based on its initial binding target. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBuffer.xml">external</a> documentation.
-spec bindBuffer(Target, Buffer) -> 'ok' when Target :: enum(),Buffer :: integer().
bindBuffer(Target,Buffer) ->
  cast(5430, <<Target:?GLenum,Buffer:?GLuint>>).

%% @doc Delete named buffer objects
%%
%% ``gl:deleteBuffers'' deletes  `N'  buffer objects named by the elements of the array
%%  `Buffers' . After a buffer object is deleted, it has no contents, and its name is
%% free for reuse (for example by  {@link gl:genBuffers/1} ). If a buffer object that is currently
%% bound is deleted, the binding reverts to 0 (the absence of any buffer object). 
%%
%% ``gl:deleteBuffers'' silently ignores 0's and names that do not correspond to existing
%% buffer objects. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteBuffers.xml">external</a> documentation.
-spec deleteBuffers(Buffers) -> 'ok' when Buffers :: [integer()].
deleteBuffers(Buffers) ->
  cast(5431, <<(length(Buffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Buffers>>)/binary,0:(((1+length(Buffers)) rem 2)*32)>>).

%% @doc Generate buffer object names
%%
%% ``gl:genBuffers'' returns  `N'  buffer object names in  `Buffers' . There is no
%% guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genBuffers''
%% . 
%%
%%  Buffer object names returned by a call to ``gl:genBuffers'' are not returned by subsequent
%% calls, unless they are first deleted with  {@link gl:deleteBuffers/1} . 
%%
%%  No buffer objects are associated with the returned buffer object names until they are
%% first bound by calling  {@link gl:bindBuffer/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenBuffers.xml">external</a> documentation.
-spec genBuffers(N) -> [integer()] when N :: integer().
genBuffers(N) ->
  call(5432, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a buffer object
%%
%% ``gl:isBuffer'' returns `?GL_TRUE' if  `Buffer'  is currently the name of a
%% buffer object. If  `Buffer'  is zero, or is a non-zero value that is not currently
%% the name of a buffer object, or if an error occurs, ``gl:isBuffer'' returns `?GL_FALSE'
%% . 
%%
%%  A name returned by  {@link gl:genBuffers/1} , but not yet associated with a buffer object
%% by calling  {@link gl:bindBuffer/2} , is not the name of a buffer object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsBuffer.xml">external</a> documentation.
-spec isBuffer(Buffer) -> 0|1 when Buffer :: integer().
isBuffer(Buffer) ->
  call(5433, <<Buffer:?GLuint>>).

%% @doc Creates and initializes a buffer object's data store
%%
%% ``gl:bufferData'' creates a new data store for the buffer object currently bound to  `Target' 
%% . Any pre-existing data store is deleted. The new data store is created with the specified
%%  `Size'  in bytes and  `Usage' . If  `Data'  is not `?NULL', the data store
%% is initialized with data from this pointer. In its initial  state, the new data store
%% is not mapped, it has a `?NULL' mapped pointer, and its mapped access  is `?GL_READ_WRITE'
%% . 
%%
%%  `Usage'  is a hint to the GL implementation as to how a buffer object's data store
%% will be  accessed. This enables the GL implementation to make more intelligent decisions
%% that may significantly  impact buffer object performance. It does not, however, constrain
%% the actual usage of the data store.  `Usage'  can be broken down into two parts: first,
%% the frequency of access (modification  and usage), and second, the nature of that access.
%% The frequency of access may be one of these: 
%%
%% STREAM:  The data store contents will be modified once and used at most a few times. 
%%
%% STATIC:  The data store contents will be modified once and used many times. 
%%
%% DYNAMIC:  The data store contents will be modified repeatedly and used many times. 
%%
%%  The nature of access may be one of these: 
%%
%% DRAW:  The data store contents are modified by the application, and used as the source
%% for GL drawing and image specification commands. 
%%
%% READ:  The data store contents are modified by reading data from the GL, and used to return
%% that data  when queried by the application. 
%%
%% COPY:  The data store contents are modified by reading data from the GL, and used as the
%% source for GL drawing and image specification commands. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferData.xml">external</a> documentation.
-spec bufferData(Target, Size, Data, Usage) -> 'ok' when Target :: enum(),Size :: integer(),Data :: offset()|mem(),Usage :: enum().
bufferData(Target,Size,Data,Usage) when  is_integer(Data) ->
  cast(5434, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Data:?GLuint,Usage:?GLenum>>);
bufferData(Target,Size,Data,Usage) ->
  send_bin(Data),
  cast(5435, <<Target:?GLenum,0:32,Size:?GLsizeiptr,Usage:?GLenum>>).

%% @doc Updates a subset of a buffer object's data store
%%
%% ``gl:bufferSubData'' redefines some or all of the data store for the buffer object currently
%%  bound to  `Target' . Data starting at byte offset  `Offset'  and extending for  `Size' 
%%  bytes is copied to the data store from the memory pointed to by  `Data' . An error
%% is thrown if  `Offset'  and  `Size'  together define a range beyond the bounds of
%% the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBufferSubData.xml">external</a> documentation.
-spec bufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: offset()|mem().
bufferSubData(Target,Offset,Size,Data) when  is_integer(Data) ->
  cast(5436, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr,Data:?GLuint>>);
bufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  cast(5437, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Returns a subset of a buffer object's data store
%%
%% ``gl:getBufferSubData'' returns some or all of the data from the buffer object currently
%%  bound to  `Target' . Data starting at byte offset  `Offset'  and extending for  `Size' 
%%  bytes is copied from the data store to the memory pointed to by  `Data' . An error
%% is thrown if the buffer object is currently mapped, or if  `Offset'  and  `Size' 
%% together define a range beyond the bounds  of the buffer object's data store. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferSubData.xml">external</a> documentation.
-spec getBufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: mem().
getBufferSubData(Target,Offset,Size,Data) ->
  send_bin(Data),
  call(5438, <<Target:?GLenum,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Return parameters of a buffer object
%%
%% ``gl:getBufferParameteriv'' returns in  `Data'  a selected parameter of the buffer
%% object specified by  `Target' . 
%%
%%  `Value'  names a specific buffer object parameter, as follows: 
%%
%% `?GL_BUFFER_ACCESS':  `Params'  returns the access policy set while mapping the
%% buffer object.  The initial value is `?GL_READ_WRITE'. 
%%
%% `?GL_BUFFER_MAPPED':  `Params'  returns a flag indicating whether the buffer object
%% is currently  mapped. The initial value is `?GL_FALSE'. 
%%
%% `?GL_BUFFER_SIZE':  `Params'  returns the size of the buffer object, measured
%% in bytes.  The initial value is 0. 
%%
%% `?GL_BUFFER_USAGE':  `Params'  returns the buffer object's usage pattern. The
%% initial value is `?GL_STATIC_DRAW'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameteriv.xml">external</a> documentation.
-spec getBufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getBufferParameteriv(Target,Pname) ->
  call(5439, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Set the RGB blend equation and the alpha blend equation separately
%%
%%  The blend equations determines how a new pixel (the ''source'' color) is combined with
%% a pixel already in the framebuffer (the ''destination'' color). These functions specifie
%% one blend equation for the RGB-color  components and one blend equation for the alpha
%% component. ``gl:blendEquationSeparatei'' specifies the blend equations for a single
%% draw buffer whereas ``gl:blendEquationSeparate'' sets the blend equations for all draw
%% buffers. 
%%
%%  The blend equations use the source and destination blend factors specified by either  {@link gl:blendFunc/2} 
%%  or  {@link gl:blendFuncSeparate/4} . See  {@link gl:blendFunc/2}  or  {@link gl:blendFuncSeparate/4} 
%%  for a description of the various blend factors. 
%%
%%  In the equations that follow, source and destination color components are referred to
%% as (R s G s B s A s) and (R d G d B d A d), respectively. The result color is referred to as (R r G r B r A r). The source and destination
%% blend factors are denoted (s R s G s B s A) and (d R d G d B d A), respectively. For these equations all color components
%% are understood to have values in the range  [0 1].  <table><tbody><tr><td>` Mode '</td><td>
%% ` RGB Components '</td><td>` Alpha Component '</td></tr></tbody><tbody><tr><td>`?GL_FUNC_ADD'
%% </td><td> Rr=R s  s R+R d  d R Gr=G s  s G+G d  d G Br=B s  s B+B d  d B</td><td> Ar=A s 
%% s A+A d  d A</td></tr><tr><td>`?GL_FUNC_SUBTRACT'</td><td> Rr=R s  s R-R d  d R Gr=G
%% s  s G-G d  d G Br=B s  s B-B d  d B</td><td> Ar=A s  s A-A d  d A</td></tr><tr><td>`?GL_FUNC_REVERSE_SUBTRACT'
%% </td><td> Rr=R d  d R-R s  s R Gr=G d  d G-G s  s G Br=B d  d B-B s  s B</td><td> Ar=A d 
%% d A-A s  s A</td></tr><tr><td>`?GL_MIN'</td><td> Rr=min(R s R d) Gr=min(G s G d) Br=min(B s B d)</td><td> Ar=min
%% (A s A d)</td></tr><tr><td>`?GL_MAX'</td><td> Rr=max(R s R d) Gr=max(G s G d) Br=max(B s B d)</td><td> Ar=max(A s A d)</td></tr></tbody>
%% </table>
%%
%%  The results of these equations are clamped to the range  [0 1]. 
%%
%%  The `?GL_MIN' and `?GL_MAX' equations are useful for applications that analyze
%% image data (image thresholding against a constant color, for example). The `?GL_FUNC_ADD'
%%  equation is useful for antialiasing and transparency, among other things. 
%%
%%  Initially, both the RGB blend equation and the alpha blend equation are set to `?GL_FUNC_ADD'
%% . 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendEquationSeparate.xml">external</a> documentation.
-spec blendEquationSeparate(ModeRGB, ModeAlpha) -> 'ok' when ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparate(ModeRGB,ModeAlpha) ->
  cast(5440, <<ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

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
%% The symbolic constants contained in  `Bufs'  may be any of the following:
%%
%% `?GL_NONE': The fragment shader output value is not written into any color buffer.
%%
%% `?GL_FRONT_LEFT': The fragment shader output value is written into the front left
%% color buffer.
%%
%% `?GL_FRONT_RIGHT': The fragment shader output value is written into the front right
%% color buffer.
%%
%% `?GL_BACK_LEFT': The fragment shader output value is written into the back left color
%% buffer.
%%
%% `?GL_BACK_RIGHT': The fragment shader output value is written into the back right
%% color buffer.
%%
%% `?GL_COLOR_ATTACHMENT'`n': The fragment shader output value is written into
%% the `n'th color attachment of the current framebuffer. `n' may range from 0
%% to the value of `?GL_MAX_COLOR_ATTACHMENTS'.
%%
%% Except for `?GL_NONE', the preceding symbolic constants may not appear more than
%% once in  `Bufs' . The maximum number of draw buffers supported is implementation dependent
%% and can be queried by calling  {@link gl:getBooleanv/1}  with the argument `?GL_MAX_DRAW_BUFFERS'
%% .
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawBuffers.xml">external</a> documentation.
-spec drawBuffers(Bufs) -> 'ok' when Bufs :: [enum()].
drawBuffers(Bufs) ->
  cast(5441, <<(length(Bufs)):?GLuint,
        (<< <<C:?GLenum>> || C <- Bufs>>)/binary,0:(((1+length(Bufs)) rem 2)*32)>>).

%% @doc Set front and/or back stencil test actions
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% You draw into the stencil planes using GL drawing primitives, then render geometry and
%% images, using the stencil planes to mask out portions of the screen. Stenciling is typically
%% used in multipass rendering algorithms to achieve special effects, such as decals, outlining,
%% and constructive solid geometry rendering. 
%%
%%  The stencil test conditionally eliminates a pixel based on the outcome of a comparison
%% between the value in the stencil buffer and a reference value. To enable and disable the
%% test, call  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_STENCIL_TEST'
%% ; to control it, call   {@link gl:stencilFunc/3}  or   {@link gl:stencilFuncSeparate/4} . 
%%
%%  There can be two separate sets of  `Sfail' ,  `Dpfail' , and   `Dppass'  parameters;
%% one affects back-facing polygons, and the other affects front-facing polygons as well
%% as other non-polygon primitives.   {@link gl:stencilOp/3}  sets both front and back stencil
%% state to the same values, as if   {@link gl:stencilOpSeparate/4}  were called with  `Face' 
%%  set to `?GL_FRONT_AND_BACK'. 
%%
%% ``gl:stencilOpSeparate'' takes three arguments that indicate what happens to the stored
%% stencil value while stenciling is enabled. If the stencil test fails, no change is made
%% to the pixel's color or depth buffers, and  `Sfail'  specifies what happens to the
%% stencil buffer contents. The following eight actions are possible. 
%%
%% `?GL_KEEP':  Keeps the current value. 
%%
%% `?GL_ZERO':  Sets the stencil buffer value to 0. 
%%
%% `?GL_REPLACE':  Sets the stencil buffer value to `ref', as specified by  {@link gl:stencilFunc/3} 
%% . 
%%
%% `?GL_INCR':  Increments the current stencil buffer value. Clamps to the maximum representable
%% unsigned value. 
%%
%% `?GL_INCR_WRAP':  Increments the current stencil buffer value. Wraps stencil buffer
%% value to zero when incrementing the maximum representable unsigned value. 
%%
%% `?GL_DECR':  Decrements the current stencil buffer value. Clamps to 0. 
%%
%% `?GL_DECR_WRAP':  Decrements the current stencil buffer value. Wraps stencil buffer
%% value to the maximum representable unsigned value when decrementing a stencil buffer value
%% of zero. 
%%
%% `?GL_INVERT':  Bitwise inverts the current stencil buffer value. 
%%
%%  Stencil buffer values are treated as unsigned integers. When incremented and decremented,
%% values are clamped to 0 and   2 n-1, where   n is the value returned by querying `?GL_STENCIL_BITS'
%% . 
%%
%%  The other two arguments to ``gl:stencilOpSeparate'' specify stencil buffer actions
%% that depend on whether subsequent depth buffer tests succeed ( `Dppass' ) or fail ( `Dpfail' 
%% ) (see  {@link gl:depthFunc/1} ). The actions are specified using the same eight symbolic
%% constants as  `Sfail' . Note that  `Dpfail'  is ignored when there is no depth buffer,
%% or when the depth buffer is not enabled. In these cases,  `Sfail'  and  `Dppass' 
%% specify stencil action when the stencil test fails and passes, respectively. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilOpSeparate.xml">external</a> documentation.
-spec stencilOpSeparate(Face, Sfail, Dpfail, Dppass) -> 'ok' when Face :: enum(),Sfail :: enum(),Dpfail :: enum(),Dppass :: enum().
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) ->
  cast(5442, <<Face:?GLenum,Sfail:?GLenum,Dpfail:?GLenum,Dppass:?GLenum>>).

%% @doc Set front and/or back function and reference value for stencil testing
%%
%%  Stenciling, like depth-buffering, enables and disables drawing on a per-pixel basis.
%% You draw into the stencil planes using GL drawing primitives, then render geometry and
%% images, using the stencil planes to mask out portions of the screen. Stenciling is typically
%% used in multipass rendering algorithms to achieve special effects, such as decals, outlining,
%% and constructive solid geometry rendering. 
%%
%%  The stencil test conditionally eliminates a pixel based on the outcome of a comparison
%% between the reference value and the value in the stencil buffer. To enable and disable
%% the test, call  {@link gl:enable/1}  and  {@link gl:enable/1}  with argument `?GL_STENCIL_TEST'
%% . To specify actions based on the outcome of the stencil test, call  {@link gl:stencilOp/3} 
%% or   {@link gl:stencilOpSeparate/4} . 
%%
%%  There can be two separate sets of  `Func' ,  `Ref' , and   `Mask'  parameters;
%% one affects back-facing polygons, and the other affects front-facing polygons as well
%% as other non-polygon primitives.   {@link gl:stencilFunc/3}  sets both front and back stencil
%% state to the same values, as if   {@link gl:stencilFuncSeparate/4}  were called with  `Face' 
%%  set to `?GL_FRONT_AND_BACK'. 
%%
%%  `Func'  is a symbolic constant that determines the stencil comparison function. It
%% accepts one of eight values, shown in the following list.  `Ref'  is an integer reference
%% value that is used in the stencil comparison. It is clamped to the range  [0 2 n-1], where   n
%% is the number of bitplanes in the stencil buffer.  `Mask'  is bitwise ANDed with both
%% the reference value and the stored stencil value, with the ANDed values participating
%% in the comparison. 
%%
%%  If `stencil' represents the value stored in the corresponding stencil buffer location,
%% the following list shows the effect of each comparison function that can be specified by  `Func' 
%% . Only if the comparison succeeds is the pixel passed through to the next stage in the
%% rasterization process (see  {@link gl:stencilOp/3} ). All tests treat `stencil' values
%% as unsigned integers in the range [0 2 n-1], where   n is the number of bitplanes in the stencil
%% buffer. 
%%
%%  The following values are accepted by  `Func' : 
%%
%% `?GL_NEVER':  Always fails. 
%%
%% `?GL_LESS':  Passes if (  `Ref'  &amp;  `Mask'  ) &lt; ( `stencil' &amp;  `Mask' 
%%  ). 
%%
%% `?GL_LEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) &lt;= ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_GREATER':  Passes if (  `Ref'  &amp;  `Mask'  ) &gt; ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_GEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) &gt;= ( `stencil'
%% &amp;  `Mask'  ). 
%%
%% `?GL_EQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) = ( `stencil' &amp;  `Mask' 
%%  ). 
%%
%% `?GL_NOTEQUAL':  Passes if (  `Ref'  &amp;  `Mask'  ) != ( `stencil' &amp;
%%  `Mask'  ). 
%%
%% `?GL_ALWAYS':  Always passes. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilFuncSeparate.xml">external</a> documentation.
-spec stencilFuncSeparate(Face, Func, Ref, Mask) -> 'ok' when Face :: enum(),Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFuncSeparate(Face,Func,Ref,Mask) ->
  cast(5443, <<Face:?GLenum,Func:?GLenum,Ref:?GLint,Mask:?GLuint>>).

%% @doc Control the front and/or back writing of individual bits in the stencil planes
%%
%% ``gl:stencilMaskSeparate'' controls the writing of individual bits in the stencil planes.
%% The least significant   n bits of  `Mask' , where   n is the number of bits in the
%% stencil buffer, specify a mask. Where a 1 appears in the mask, it's possible to write
%% to the corresponding bit in the stencil buffer. Where a 0 appears, the corresponding bit
%% is write-protected. Initially, all bits are enabled for writing. 
%%
%%  There can be two separate  `Mask'  writemasks; one affects back-facing polygons, and
%% the other affects front-facing polygons as well as other non-polygon primitives.   {@link gl:stencilMask/1} 
%%  sets both front and back stencil writemasks to the same values, as if  {@link gl:stencilMaskSeparate/2} 
%%  were called with  `Face'  set to `?GL_FRONT_AND_BACK'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilMaskSeparate.xml">external</a> documentation.
-spec stencilMaskSeparate(Face, Mask) -> 'ok' when Face :: enum(),Mask :: integer().
stencilMaskSeparate(Face,Mask) ->
  cast(5444, <<Face:?GLenum,Mask:?GLuint>>).

%% @doc Attaches a shader object to a program object
%%
%% In order to create a complete shader program, there must be a way to  specify the list
%% of things that will be linked together. Program  objects provide this mechanism. Shaders
%% that are to be linked  together in a program object must first be attached to that  program
%% object. ``gl:attachShader'' attaches the  shader object specified by  `Shader'  to
%% the  program object specified by  `Program' . This  indicates that  `Shader'  will
%% be included in  link operations that will be performed on   `Program' .
%%
%% All operations that can be performed on a shader object  are valid whether or not the
%% shader object is attached to a  program object. It is permissible to attach a shader object
%% to a  program object before source code has been loaded into the  shader object or before
%% the shader object has been compiled. It  is permissible to attach multiple shader objects
%% of the same  type because each may contain a portion of the complete shader.  It is also
%% permissible to attach a shader object to more than  one program object. If a shader object
%% is deleted while it is  attached to a program object, it will be flagged for deletion,
%%  and deletion will not occur until   {@link gl:detachShader/2}   is called to detach it from
%% all program objects to which it is  attached.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachShader.xml">external</a> documentation.
-spec attachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
attachShader(Program,Shader) ->
  cast(5445, <<Program:?GLuint,Shader:?GLuint>>).

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
%% If  `Name'  refers to a matrix attribute variable,  `Index'  refers to the first
%% column of the matrix. Other matrix columns are then automatically bound to locations  `Index+1' 
%%  for a matrix of type `mat2';  `Index+1'  and  `Index+2'  for a matrix of type
%% `mat3'; and  `Index+1' ,  `Index+2' , and  `Index+3'  for a matrix of type `mat4'
%% .
%%
%% This command makes it possible for vertex shaders to use descriptive names for attribute
%% variables rather than generic variables that are numbered from 0 to `?GL_MAX_VERTEX_ATTRIBS'
%%  -1. The values sent to each generic attribute index are part of current state. If a different
%% program object is made current by calling  {@link gl:useProgram/1} , the generic vertex attributes
%% are tracked in such a way that the same values will be observed by attributes in the new
%% program object that are also bound to  `Index' .
%%
%% Attribute variable name-to-generic attribute index bindings for a program object can be
%% explicitly assigned at any time by calling ``gl:bindAttribLocation''. Attribute bindings
%% do not go into effect until  {@link gl:linkProgram/1}  is called. After a program object
%% has been linked successfully, the index values for generic attributes remain fixed (and
%% their values can be queried) until the next link command occurs.
%%
%% Any attribute binding that occurs after the program object has been linked will not take
%% effect until the next time the program object is linked.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocation.xml">external</a> documentation.
-spec bindAttribLocation(Program, Index, Name) -> 'ok' when Program :: integer(),Index :: integer(),Name :: string().
bindAttribLocation(Program,Index,Name) ->
  cast(5446, <<Program:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc Compiles a shader object
%%
%% ``gl:compileShader'' compiles the source code strings that have been stored in the shader
%% object specified by  `Shader' .
%%
%% The compilation status will be stored as part of the shader object's state. This value
%% will be set to `?GL_TRUE' if the shader was compiled without errors and is ready
%% for use, and `?GL_FALSE' otherwise. It can be queried by calling  {@link gl:getShaderiv/2} 
%%  with arguments  `Shader'  and `?GL_COMPILE_STATUS'.
%%
%% Compilation of a shader can fail for a number of reasons as specified by the OpenGL Shading
%% Language Specification. Whether or not the compilation was successful, information about
%% the compilation can be obtained from the shader object's information log by calling  {@link gl:getShaderInfoLog/2} 
%% .
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShader.xml">external</a> documentation.
-spec compileShader(Shader) -> 'ok' when Shader :: integer().
compileShader(Shader) ->
  cast(5447, <<Shader:?GLuint>>).

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
%% One or more executables are created in a program object by  successfully attaching shader
%% objects to it with   {@link gl:attachShader/2} ,  successfully compiling the shader objects
%% with   {@link gl:compileShader/1} ,  and successfully linking the program object with   {@link gl:linkProgram/1} 
%% .  These executables are made part of current state when   {@link gl:useProgram/1}   is called.
%% Program objects can be deleted by calling   {@link gl:deleteProgram/1} .  The memory associated
%% with the program object will be deleted  when it is no longer part of current rendering
%% state for any  context.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgram.xml">external</a> documentation.
-spec createProgram() -> integer().
createProgram() ->
  call(5448, <<>>).

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
%% When created, a shader object's  `?GL_SHADER_TYPE' parameter is set to either  `?GL_VERTEX_SHADER'
%% , `?GL_TESS_CONTROL_SHADER',  `?GL_TESS_EVALUATION_SHADER', `?GL_GEOMETRY_SHADER'
%%   or `?GL_FRAGMENT_SHADER', depending on the value  of  `ShaderType' .
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShader.xml">external</a> documentation.
-spec createShader(Type) -> integer() when Type :: enum().
createShader(Type) ->
  call(5449, <<Type:?GLenum>>).

%% @doc Deletes a program object
%%
%% ``gl:deleteProgram'' frees the memory and invalidates the name associated with the program
%% object specified by  `Program.'  This command effectively undoes the effects of a call
%% to  {@link gl:createProgram/0} .
%%
%% If a program object is in use as part of current rendering state, it will be flagged for
%% deletion, but it will not be deleted until it is no longer part of current state for any
%% rendering context. If a program object to be deleted has shader objects attached to it,
%% those shader objects will be automatically detached but not deleted unless they have already
%% been flagged for deletion by a previous call to  {@link gl:deleteShader/1} . A value of 0
%% for  `Program'  will be silently ignored.
%%
%% To determine whether a program object has been flagged for deletion, call  {@link gl:getProgramiv/2} 
%%  with arguments  `Program'  and `?GL_DELETE_STATUS'.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgram.xml">external</a> documentation.
-spec deleteProgram(Program) -> 'ok' when Program :: integer().
deleteProgram(Program) ->
  cast(5450, <<Program:?GLuint>>).

%% @doc Deletes a shader object
%%
%% ``gl:deleteShader'' frees the memory and invalidates the name associated with the shader
%% object specified by  `Shader' . This command effectively undoes the effects of a call
%% to  {@link gl:createShader/1} .
%%
%% If a shader object to be deleted is attached to a program object, it will be flagged for
%% deletion, but it will not be deleted until it is no longer attached to any program object,
%% for any rendering context (i.e., it must be detached from wherever it was attached before
%% it will be deleted). A value of 0 for  `Shader'  will be silently ignored.
%%
%% To determine whether an object has been flagged for deletion, call  {@link gl:getShaderiv/2} 
%% with arguments  `Shader'  and `?GL_DELETE_STATUS'.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteShader.xml">external</a> documentation.
-spec deleteShader(Shader) -> 'ok' when Shader :: integer().
deleteShader(Shader) ->
  cast(5451, <<Shader:?GLuint>>).

%% @doc Detaches a shader object from a program object to which it is attached
%%
%% ``gl:detachShader'' detaches the shader object specified by  `Shader'  from the program
%% object specified by  `Program' . This command can be used to undo the effect of the
%% command  {@link gl:attachShader/2} .
%%
%% If  `Shader'  has already been flagged for deletion by a call to  {@link gl:deleteShader/1} 
%%  and it is not attached to any other program object, it will be deleted after it has been
%% detached.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachShader.xml">external</a> documentation.
-spec detachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
detachShader(Program,Shader) ->
  cast(5452, <<Program:?GLuint,Shader:?GLuint>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnableVertexAttribArray.xml">external</a> documentation.
-spec disableVertexAttribArray(Index) -> 'ok' when Index :: integer().
disableVertexAttribArray(Index) ->
  cast(5453, <<Index:?GLuint>>).

%% @doc 
%% See {@link disableVertexAttribArray/1}
-spec enableVertexAttribArray(Index) -> 'ok' when Index :: integer().
enableVertexAttribArray(Index) ->
  cast(5454, <<Index:?GLuint>>).

%% @doc Returns information about an active attribute variable for the specified program object
%%
%% ``gl:getActiveAttrib'' returns information about an active attribute variable in the
%% program object specified by  `Program' . The number of active attributes can be obtained
%% by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_ATTRIBUTES'. A value
%% of 0 for  `Index'  selects the first active attribute variable. Permissible values
%% for  `Index'  range from 0 to the number of active attribute variables minus 1.
%%
%% A vertex shader may use either built-in attribute variables, user-defined attribute variables,
%% or both. Built-in attribute variables have a prefix of "gl_" and reference conventional
%% OpenGL vertex attribtes (e.g.,  `Gl_Vertex' ,  `Gl_Normal' , etc., see the OpenGL
%% Shading Language specification for a complete list.) User-defined attribute variables
%% have arbitrary names and obtain their values through numbered generic vertex attributes.
%% An attribute variable (either built-in or user-defined) is considered active if it is
%% determined during the link operation that it may be accessed during program execution.
%% Therefore,  `Program'  should have previously been the target of a call to  {@link gl:linkProgram/1} 
%% , but it is not necessary for it to have been linked successfully.
%%
%% The size of the character buffer required to store the longest attribute variable name
%% in  `Program'  can be obtained by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_ATTRIBUTE_MAX_LENGTH'
%% . This value should be used to allocate a buffer of sufficient size to store the returned
%% attribute name. The size of this character buffer is passed in  `BufSize' , and a pointer
%% to this character buffer is passed in  `Name' .
%%
%% ``gl:getActiveAttrib'' returns the name of the attribute variable indicated by  `Index' 
%% , storing it in the character buffer specified by  `Name' . The string returned will
%% be null terminated. The actual number of characters written into this buffer is returned
%% in  `Length' , and this count does not include the null termination character. If the
%% length of the returned string is not required, a value of `?NULL' can be passed in
%% the  `Length'  argument.
%%
%% The  `Type'  argument specifies a pointer to a variable into which the attribute variable's
%% data type will be written. The symbolic constants `?GL_FLOAT', `?GL_FLOAT_VEC2',
%% `?GL_FLOAT_VEC3', `?GL_FLOAT_VEC4', `?GL_FLOAT_MAT2', `?GL_FLOAT_MAT3',
%% `?GL_FLOAT_MAT4', `?GL_FLOAT_MAT2x3', `?GL_FLOAT_MAT2x4', `?GL_FLOAT_MAT3x2'
%% , `?GL_FLOAT_MAT3x4', `?GL_FLOAT_MAT4x2', `?GL_FLOAT_MAT4x3', `?GL_INT'
%% , `?GL_INT_VEC2', `?GL_INT_VEC3', `?GL_INT_VEC4', `?GL_UNSIGNED_INT_VEC'
%% , `?GL_UNSIGNED_INT_VEC2', `?GL_UNSIGNED_INT_VEC3', `?GL_UNSIGNED_INT_VEC4',
%% `?DOUBLE', `?DOUBLE_VEC2', `?DOUBLE_VEC3', `?DOUBLE_VEC4', `?DOUBLE_MAT2'
%% , `?DOUBLE_MAT3', `?DOUBLE_MAT4', `?DOUBLE_MAT2x3', `?DOUBLE_MAT2x4',
%% `?DOUBLE_MAT3x2', `?DOUBLE_MAT3x4', `?DOUBLE_MAT4x2', or `?DOUBLE_MAT4x3'
%%  may be returned. The  `Size'  argument will return the size of the attribute, in units
%% of the type returned in  `Type' .
%%
%% The list of active attribute variables may include both built-in attribute variables (which
%% begin with the prefix "gl_") as well as user-defined attribute variable names.
%%
%% This function will return as much information as it can about the specified active attribute
%% variable. If no information is available,  `Length'  will be 0, and  `Name'  will
%% be an empty string. This situation could occur if this function is called after a link
%% operation that failed. If an error occurs, the return values  `Length' ,  `Size' ,  `Type' 
%% , and  `Name'  will be unmodified.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttrib.xml">external</a> documentation.
-spec getActiveAttrib(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveAttrib(Program,Index,BufSize) ->
  call(5455, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns information about an active uniform variable for the specified program object
%%
%% ``gl:getActiveUniform'' returns information about an active uniform variable in the
%% program object specified by  `Program' . The number of active uniform variables can
%% be obtained by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_UNIFORMS'.
%% A value of 0 for  `Index'  selects the first active uniform variable. Permissible values
%% for  `Index'  range from 0 to the number of active uniform variables minus 1.
%%
%% Shaders may use either built-in uniform variables, user-defined uniform variables, or
%% both. Built-in uniform variables have a prefix of "gl_" and reference existing OpenGL
%% state or values derived from such state (e.g.,  `Gl_DepthRangeParameters' , see the
%% OpenGL Shading Language specification for a complete list.) User-defined uniform variables
%% have arbitrary names and obtain their values from the application through calls to  {@link gl:uniform1f/2} 
%% . A uniform variable (either built-in or user-defined) is considered active if it is determined
%% during the link operation that it may be accessed during program execution. Therefore,  `Program' 
%%  should have previously been the target of a call to  {@link gl:linkProgram/1} , but it is
%% not necessary for it to have been linked successfully.
%%
%% The size of the character buffer required to store the longest uniform variable name in  `Program' 
%%  can be obtained by calling  {@link gl:getProgramiv/2}  with the value `?GL_ACTIVE_UNIFORM_MAX_LENGTH'
%% . This value should be used to allocate a buffer of sufficient size to store the returned
%% uniform variable name. The size of this character buffer is passed in  `BufSize' ,
%% and a pointer to this character buffer is passed in  `Name.' 
%%
%% ``gl:getActiveUniform'' returns the name of the uniform variable indicated by  `Index' 
%% , storing it in the character buffer specified by  `Name' . The string returned will
%% be null terminated. The actual number of characters written into this buffer is returned
%% in  `Length' , and this count does not include the null termination character. If the
%% length of the returned string is not required, a value of `?NULL' can be passed in
%% the  `Length'  argument.
%%
%% The  `Type'  argument will return a pointer to the uniform variable's data type. The
%% symbolic constants returned for uniform types are shown in the table below. <table><tbody>
%% <tr><td>` Returned Symbolic Contant '</td><td>` Shader Uniform Type '</td></tr></tbody>
%% <tbody><tr><td>`?GL_FLOAT'</td><td>`?float'</td></tr><tr><td>`?GL_FLOAT_VEC2'
%% </td><td>`?vec2'</td></tr><tr><td>`?GL_FLOAT_VEC3'</td><td>`?vec3'</td></tr>
%% <tr><td>`?GL_FLOAT_VEC4'</td><td>`?vec4'</td></tr><tr><td>`?GL_DOUBLE'</td>
%% <td>`?double'</td></tr><tr><td>`?GL_DOUBLE_VEC2'</td><td>`?dvec2'</td></tr>
%% <tr><td>`?GL_DOUBLE_VEC3'</td><td>`?dvec3'</td></tr><tr><td>`?GL_DOUBLE_VEC4'
%% </td><td>`?dvec4'</td></tr><tr><td>`?GL_INT'</td><td>`?int'</td></tr><tr><td>
%% `?GL_INT_VEC2'</td><td>`?ivec2'</td></tr><tr><td>`?GL_INT_VEC3'</td><td>`?ivec3'
%% </td></tr><tr><td>`?GL_INT_VEC4'</td><td>`?ivec4'</td></tr><tr><td>`?GL_UNSIGNED_INT'
%% </td><td>`?unsigned int'</td></tr><tr><td>`?GL_UNSIGNED_INT_VEC2'</td><td>`?uvec2'
%% </td></tr><tr><td>`?GL_UNSIGNED_INT_VEC3'</td><td>`?uvec3'</td></tr><tr><td>`?GL_UNSIGNED_INT_VEC4'
%% </td><td>`?uvec4'</td></tr><tr><td>`?GL_BOOL'</td><td>`?bool'</td></tr><tr>
%% <td>`?GL_BOOL_VEC2'</td><td>`?bvec2'</td></tr><tr><td>`?GL_BOOL_VEC3'</td><td>
%% `?bvec3'</td></tr><tr><td>`?GL_BOOL_VEC4'</td><td>`?bvec4'</td></tr><tr><td>
%% `?GL_FLOAT_MAT2'</td><td>`?mat2'</td></tr><tr><td>`?GL_FLOAT_MAT3'</td><td>
%% `?mat3'</td></tr><tr><td>`?GL_FLOAT_MAT4'</td><td>`?mat4'</td></tr><tr><td>
%% `?GL_FLOAT_MAT2x3'</td><td>`?mat2x3'</td></tr><tr><td>`?GL_FLOAT_MAT2x4'</td>
%% <td>`?mat2x4'</td></tr><tr><td>`?GL_FLOAT_MAT3x2'</td><td>`?mat3x2'</td></tr>
%% <tr><td>`?GL_FLOAT_MAT3x4'</td><td>`?mat3x4'</td></tr><tr><td>`?GL_FLOAT_MAT4x2'
%% </td><td>`?mat4x2'</td></tr><tr><td>`?GL_FLOAT_MAT4x3'</td><td>`?mat4x3'</td>
%% </tr><tr><td>`?GL_DOUBLE_MAT2'</td><td>`?dmat2'</td></tr><tr><td>`?GL_DOUBLE_MAT3'
%% </td><td>`?dmat3'</td></tr><tr><td>`?GL_DOUBLE_MAT4'</td><td>`?dmat4'</td></tr>
%% <tr><td>`?GL_DOUBLE_MAT2x3'</td><td>`?dmat2x3'</td></tr><tr><td>`?GL_DOUBLE_MAT2x4'
%% </td><td>`?dmat2x4'</td></tr><tr><td>`?GL_DOUBLE_MAT3x2'</td><td>`?dmat3x2'</td>
%% </tr><tr><td>`?GL_DOUBLE_MAT3x4'</td><td>`?dmat3x4'</td></tr><tr><td>`?GL_DOUBLE_MAT4x2'
%% </td><td>`?dmat4x2'</td></tr><tr><td>`?GL_DOUBLE_MAT4x3'</td><td>`?dmat4x3'</td>
%% </tr><tr><td>`?GL_SAMPLER_1D'</td><td>`?sampler1D'</td></tr><tr><td>`?GL_SAMPLER_2D'
%% </td><td>`?sampler2D'</td></tr><tr><td>`?GL_SAMPLER_3D'</td><td>`?sampler3D'
%% </td></tr><tr><td>`?GL_SAMPLER_CUBE'</td><td>`?samplerCube'</td></tr><tr><td>`?GL_SAMPLER_1D_SHADOW'
%% </td><td>`?sampler1DShadow'</td></tr><tr><td>`?GL_SAMPLER_2D_SHADOW'</td><td>`?sampler2DShadow'
%% </td></tr><tr><td>`?GL_SAMPLER_1D_ARRAY'</td><td>`?sampler1DArray'</td></tr><tr>
%% <td>`?GL_SAMPLER_2D_ARRAY'</td><td>`?sampler2DArray'</td></tr><tr><td>`?GL_SAMPLER_1D_ARRAY_SHADOW'
%% </td><td>`?sampler1DArrayShadow'</td></tr><tr><td>`?GL_SAMPLER_2D_ARRAY_SHADOW'</td>
%% <td>`?sampler2DArrayShadow'</td></tr><tr><td>`?GL_SAMPLER_2D_MULTISAMPLE'</td><td>
%% `?sampler2DMS'</td></tr><tr><td>`?GL_SAMPLER_2D_MULTISAMPLE_ARRAY'</td><td>`?sampler2DMSArray'
%% </td></tr><tr><td>`?GL_SAMPLER_CUBE_SHADOW'</td><td>`?samplerCubeShadow'</td></tr>
%% <tr><td>`?GL_SAMPLER_BUFFER'</td><td>`?samplerBuffer'</td></tr><tr><td>`?GL_SAMPLER_2D_RECT'
%% </td><td>`?sampler2DRect'</td></tr><tr><td>`?GL_SAMPLER_2D_RECT_SHADOW'</td><td>
%% `?sampler2DRectShadow'</td></tr><tr><td>`?GL_INT_SAMPLER_1D'</td><td>`?isampler1D'
%% </td></tr><tr><td>`?GL_INT_SAMPLER_2D'</td><td>`?isampler2D'</td></tr><tr><td>`?GL_INT_SAMPLER_3D'
%% </td><td>`?isampler3D'</td></tr><tr><td>`?GL_INT_SAMPLER_CUBE'</td><td>`?isamplerCube'
%% </td></tr><tr><td>`?GL_INT_SAMPLER_1D_ARRAY'</td><td>`?isampler1DArray'</td></tr>
%% <tr><td>`?GL_INT_SAMPLER_2D_ARRAY'</td><td>`?isampler2DArray'</td></tr><tr><td>`?GL_INT_SAMPLER_2D_MULTISAMPLE'
%% </td><td>`?isampler2DMS'</td></tr><tr><td>`?GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY'</td>
%% <td>`?isampler2DMSArray'</td></tr><tr><td>`?GL_INT_SAMPLER_BUFFER'</td><td>`?isamplerBuffer'
%% </td></tr><tr><td>`?GL_INT_SAMPLER_2D_RECT'</td><td>`?isampler2DRect'</td></tr><tr>
%% <td>`?GL_UNSIGNED_INT_SAMPLER_1D'</td><td>`?usampler1D'</td></tr><tr><td>`?GL_UNSIGNED_INT_SAMPLER_2D'
%% </td><td>`?usampler2D'</td></tr><tr><td>`?GL_UNSIGNED_INT_SAMPLER_3D'</td><td>`?usampler3D'
%% </td></tr><tr><td>`?GL_UNSIGNED_INT_SAMPLER_CUBE'</td><td>`?usamplerCube'</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_SAMPLER_1D_ARRAY'</td><td>`?usampler2DArray'</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_SAMPLER_2D_ARRAY'</td><td>`?usampler2DArray'</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE'</td><td>`?usampler2DMS'</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY'</td><td>`?usampler2DMSArray'
%% </td></tr><tr><td>`?GL_UNSIGNED_INT_SAMPLER_BUFFER'</td><td>`?usamplerBuffer'</td>
%% </tr><tr><td>`?GL_UNSIGNED_INT_SAMPLER_2D_RECT'</td><td>`?usampler2DRect'</td></tr>
%% <tr><td>`?GL_IMAGE_1D'</td><td>`?image1D'</td></tr><tr><td>`?GL_IMAGE_2D'</td>
%% <td>`?image2D'</td></tr><tr><td>`?GL_IMAGE_3D'</td><td>`?image3D'</td></tr>
%% <tr><td>`?GL_IMAGE_2D_RECT'</td><td>`?image2DRect'</td></tr><tr><td>`?GL_IMAGE_CUBE'
%% </td><td>`?imageCube'</td></tr><tr><td>`?GL_IMAGE_BUFFER'</td><td>`?imageBuffer'
%% </td></tr><tr><td>`?GL_IMAGE_1D_ARRAY'</td><td>`?image1DArray'</td></tr><tr><td>
%% `?GL_IMAGE_2D_ARRAY'</td><td>`?image2DArray'</td></tr><tr><td>`?GL_IMAGE_2D_MULTISAMPLE'
%% </td><td>`?image2DMS'</td></tr><tr><td>`?GL_IMAGE_2D_MULTISAMPLE_ARRAY'</td><td>
%% `?image2DMSArray'</td></tr><tr><td>`?GL_INT_IMAGE_1D'</td><td>`?iimage1D'</td>
%% </tr><tr><td>`?GL_INT_IMAGE_2D'</td><td>`?iimage2D'</td></tr><tr><td>`?GL_INT_IMAGE_3D'
%% </td><td>`?iimage3D'</td></tr><tr><td>`?GL_INT_IMAGE_2D_RECT'</td><td>`?iimage2DRect'
%% </td></tr><tr><td>`?GL_INT_IMAGE_CUBE'</td><td>`?iimageCube'</td></tr><tr><td>`?GL_INT_IMAGE_BUFFER'
%% </td><td>`?iimageBuffer'</td></tr><tr><td>`?GL_INT_IMAGE_1D_ARRAY'</td><td>`?iimage1DArray'
%% </td></tr><tr><td>`?GL_INT_IMAGE_2D_ARRAY'</td><td>`?iimage2DArray'</td></tr><tr>
%% <td>`?GL_INT_IMAGE_2D_MULTISAMPLE'</td><td>`?iimage2DMS'</td></tr><tr><td>`?GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY'
%% </td><td>`?iimage2DMSArray'</td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_1D'</td><td>
%% `?uimage1D'</td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_2D'</td><td>`?uimage2D'
%% </td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_3D'</td><td>`?uimage3D'</td></tr><tr><td>
%% `?GL_UNSIGNED_INT_IMAGE_2D_RECT'</td><td>`?uimage2DRect'</td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_CUBE'
%% </td><td>`?uimageCube'</td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_BUFFER'</td><td>
%% `?uimageBuffer'</td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_1D_ARRAY'</td><td>`?uimage1DArray'
%% </td></tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_2D_ARRAY'</td><td>`?uimage2DArray'</td>
%% </tr><tr><td>`?GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE'</td><td>`?uimage2DMS'</td></tr>
%% <tr><td>`?GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY'</td><td>`?uimage2DMSArray'</td>
%% </tr><tr><td>`?GL_UNSIGNED_INT_ATOMIC_COUNTER'</td><td>`?atomic_uint'</td></tr></tbody>
%% </table>
%%
%% If one or more elements of an array are active, the name of the array is returned in  `Name' 
%% , the type is returned in  `Type' , and the  `Size'  parameter returns the highest
%% array element index used, plus one, as determined by the compiler and/or linker. Only
%% one active uniform variable will be reported for a uniform array.
%%
%% Uniform variables that are declared as structures or arrays of structures will not be
%% returned directly by this function. Instead, each of these uniform variables will be reduced
%% to its fundamental components containing the "." and "[]" operators such that each of
%% the names is valid as an argument to  {@link gl:getUniformLocation/2} . Each of these reduced
%% uniform variables is counted as one active uniform variable and is assigned an index.
%% A valid name cannot be a structure, an array of structures, or a subcomponent of a vector
%% or matrix.
%%
%% The size of the uniform variable will be returned in  `Size' . Uniform variables other
%% than arrays will have a size of 1. Structures and arrays of structures will be reduced
%% as described earlier, such that each of the names returned will be a data type in the
%% earlier list. If this reduction results in an array, the size returned will be as described
%% for uniform arrays; otherwise, the size returned will be 1.
%%
%% The list of active uniform variables may include both built-in uniform variables (which
%% begin with the prefix "gl_") as well as user-defined uniform variable names.
%%
%% This function will return as much information as it can about the specified active uniform
%% variable. If no information is available,  `Length'  will be 0, and  `Name'  will
%% be an empty string. This situation could occur if this function is called after a link
%% operation that failed. If an error occurs, the return values  `Length' ,  `Size' ,  `Type' 
%% , and  `Name'  will be unmodified.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniform.xml">external</a> documentation.
-spec getActiveUniform(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveUniform(Program,Index,BufSize) ->
  call(5456, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns the handles of the shader objects attached to a program object
%%
%% ``gl:getAttachedShaders'' returns the names of the shader objects attached to  `Program' 
%% . The names of shader objects that are attached to  `Program'  will be returned in  `Shaders.' 
%%  The actual number of shader names written into  `Shaders'  is returned in  `Count.' 
%%  If no shader objects are attached to  `Program' ,  `Count'  is set to 0. The maximum
%% number of shader names that may be returned in  `Shaders'  is specified by  `MaxCount' 
%% . 
%%
%% If the number of names actually returned is not required (for instance, if it has just
%% been obtained by calling  {@link gl:getProgramiv/2} ), a value of `?NULL' may be passed
%% for count. If no shader objects are attached to  `Program' , a value of 0 will be returned
%% in  `Count' . The actual number of attached shaders can be obtained by calling  {@link gl:getProgramiv/2} 
%%  with the value `?GL_ATTACHED_SHADERS'.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedShaders.xml">external</a> documentation.
-spec getAttachedShaders(Program, MaxCount) -> [integer()] when Program :: integer(),MaxCount :: integer().
getAttachedShaders(Program,MaxCount) ->
  call(5457, <<Program:?GLuint,MaxCount:?GLsizei>>).

%% @doc Returns the location of an attribute variable
%%
%% ``gl:getAttribLocation'' queries the previously linked program object specified by  `Program' 
%%  for the attribute variable specified by  `Name'  and returns the index of the generic
%% vertex attribute that is bound to that attribute variable. If  `Name'  is a matrix
%% attribute variable, the index of the first column of the matrix is returned. If the named
%% attribute variable is not an active attribute in the specified program object or if  `Name' 
%%  starts with the reserved prefix "gl_", a value of -1 is returned.
%%
%% The association between an attribute variable name and a generic attribute index can be
%% specified at any time by calling  {@link gl:bindAttribLocation/3} . Attribute bindings do
%% not go into effect until  {@link gl:linkProgram/1}  is called. After a program object has
%% been linked successfully, the index values for attribute variables remain fixed until
%% the next link command occurs. The attribute values can only be queried after a link if
%% the link was successful. ``gl:getAttribLocation'' returns the binding that actually
%% went into effect the last time  {@link gl:linkProgram/1}  was called for the specified program
%% object. Attribute bindings that have been specified since the last link operation are
%% not returned by ``gl:getAttribLocation''.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocation.xml">external</a> documentation.
-spec getAttribLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getAttribLocation(Program,Name) ->
  call(5458, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @doc Returns a parameter from a program object
%%
%% ``gl:getProgram'' returns in  `Params'  the value of a parameter for a specific program
%% object. The following parameters are defined:
%%
%% `?GL_DELETE_STATUS':  `Params'  returns `?GL_TRUE' if  `Program'  is currently
%% flagged for deletion, and `?GL_FALSE' otherwise.
%%
%% `?GL_LINK_STATUS':  `Params'  returns `?GL_TRUE' if the last link operation
%% on  `Program'  was successful, and `?GL_FALSE' otherwise.
%%
%% `?GL_VALIDATE_STATUS':  `Params'  returns `?GL_TRUE' or if the last validation
%% operation on  `Program'  was successful, and `?GL_FALSE' otherwise.
%%
%% `?GL_INFO_LOG_LENGTH':  `Params'  returns the number of characters in the information
%% log for  `Program'  including the null termination character (i.e., the size of the
%% character buffer required to store the information log). If  `Program'  has no information
%% log, a value of 0 is returned.
%%
%% `?GL_ATTACHED_SHADERS':  `Params'  returns the number of shader objects attached
%% to  `Program' .
%%
%% `?GL_ACTIVE_ATOMIC_COUNTER_BUFFERS':  `Params'  returns the number of active attribute
%% atomic counter buffers used by  `Program' .
%%
%% `?GL_ACTIVE_ATTRIBUTES':  `Params'  returns the number of active attribute variables
%% for  `Program' .
%%
%% `?GL_ACTIVE_ATTRIBUTE_MAX_LENGTH':  `Params'  returns the length of the longest
%% active attribute name for  `Program' , including the null termination character (i.e.,
%% the size of the character buffer required to store the longest attribute name). If no
%% active attributes exist, 0 is returned.
%%
%% `?GL_ACTIVE_UNIFORMS':  `Params'  returns the number of active uniform variables
%% for  `Program' .
%%
%% `?GL_ACTIVE_UNIFORM_MAX_LENGTH':  `Params'  returns the length of the longest
%% active uniform variable name for  `Program' , including the null termination character
%% (i.e., the size of the character buffer required to store the longest uniform variable
%% name). If no active uniform variables exist, 0 is returned.
%%
%% `?GL_PROGRAM_BINARY_LENGTH':  `Params'  returns the length of the program binary,
%% in bytes that will be returned by a call to  {@link gl:getProgramBinary/2} . When a progam's
%% `?GL_LINK_STATUS' is `?GL_FALSE', its program binary length is zero. 
%%
%% `?GL_TRANSFORM_FEEDBACK_BUFFER_MODE':  `Params'  returns a symbolic constant indicating
%% the buffer mode used when transform feedback is active. This may be `?GL_SEPARATE_ATTRIBS'
%%  or `?GL_INTERLEAVED_ATTRIBS'.
%%
%% `?GL_TRANSFORM_FEEDBACK_VARYINGS':  `Params'  returns the number of varying variables
%% to capture in transform feedback mode for the program.
%%
%% `?GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH':  `Params'  returns the length of
%% the longest variable name to be used for transform feedback, including the null-terminator.
%% 
%%
%% `?GL_GEOMETRY_VERTICES_OUT':  `Params'  returns the maximum number of vertices
%% that the geometry shader in  `Program'  will output.
%%
%% `?GL_GEOMETRY_INPUT_TYPE':  `Params'  returns a symbolic constant indicating the
%% primitive type accepted as input to the geometry shader contained in  `Program' .
%%
%% `?GL_GEOMETRY_OUTPUT_TYPE':  `Params'  returns a symbolic constant indicating
%% the primitive type that will be output by the geometry shader contained in  `Program' .
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgram.xml">external</a> documentation.
-spec getProgramiv(Program, Pname) -> integer() when Program :: integer(),Pname :: enum().
getProgramiv(Program,Pname) ->
  call(5459, <<Program:?GLuint,Pname:?GLenum>>).

%% @doc Returns the information log for a program object
%%
%% ``gl:getProgramInfoLog'' returns the  information log for the specified program object.
%% The  information log for a program object is modified when the  program object is linked
%% or validated. The string that is  returned will be null terminated.
%%
%% ``gl:getProgramInfoLog'' returns in   `InfoLog'  as much of the information log as
%%  it can, up to a maximum of  `MaxLength'   characters. The number of characters actually
%% returned,  excluding the null termination character, is specified by   `Length' . If
%% the length of the returned  string is not required, a value of `?NULL' can  be passed
%% in the  `Length'  argument. The  size of the buffer required to store the returned
%% information  log can be obtained by calling   {@link gl:getProgramiv/2}   with the value `?GL_INFO_LOG_LENGTH'
%% . 
%%
%% The information log for a program object is either an  empty string, or a string containing
%% information about the last  link operation, or a string containing information about the
%%  last validation operation. It may contain diagnostic messages,  warning messages, and
%% other information. When a program object  is created, its information log will be a string
%% of length  0.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramInfoLog.xml">external</a> documentation.
-spec getProgramInfoLog(Program, BufSize) -> string() when Program :: integer(),BufSize :: integer().
getProgramInfoLog(Program,BufSize) ->
  call(5460, <<Program:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns a parameter from a shader object
%%
%% ``gl:getShader''  returns in  `Params'   the value of a parameter for a specific
%% shader object. The  following parameters are defined:
%%
%% `?GL_SHADER_TYPE':  `Params'  returns   `?GL_VERTEX_SHADER' if    `Shader' 
%%  is a vertex shader   object, `?GL_GEOMETRY_SHADER' if  `Shader'    is a geometry
%% shader object, and `?GL_FRAGMENT_SHADER'   if  `Shader'  is a fragment   shader
%% object.
%%
%% `?GL_DELETE_STATUS':  `Params'  returns   `?GL_TRUE' if    `Shader'  is
%% currently flagged   for deletion, and `?GL_FALSE'   otherwise.
%%
%% `?GL_COMPILE_STATUS':  `Params'  returns   `?GL_TRUE' if the last compile
%%   operation on  `Shader'  was   successful, and `?GL_FALSE'   otherwise.
%%
%% `?GL_INFO_LOG_LENGTH':  `Params'  returns the   number of characters in the information
%% log for    `Shader'  including the null   termination character (i.e., the size of
%% the   character buffer required to store the information   log). If  `Shader'  has
%% no   information log, a value of 0 is returned.
%%
%% `?GL_SHADER_SOURCE_LENGTH':  `Params'  returns the   length of the concatenation
%% of the source strings   that make up the shader source for the    `Shader' , including
%% the null   termination character. (i.e., the size of the   character buffer required to
%% store the shader   source). If no source code exists, 0 is   returned.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShader.xml">external</a> documentation.
-spec getShaderiv(Shader, Pname) -> integer() when Shader :: integer(),Pname :: enum().
getShaderiv(Shader,Pname) ->
  call(5461, <<Shader:?GLuint,Pname:?GLenum>>).

%% @doc Returns the information log for a shader object
%%
%% ``gl:getShaderInfoLog'' returns the  information log for the specified shader object.
%% The information  log for a shader object is modified when the shader is compiled.  The
%% string that is returned will be null terminated.
%%
%% ``gl:getShaderInfoLog'' returns in   `InfoLog'  as much of the information log as
%%  it can, up to a maximum of  `MaxLength'   characters. The number of characters actually
%% returned,  excluding the null termination character, is specified by   `Length' . If
%% the length of the returned  string is not required, a value of `?NULL' can  be passed
%% in the  `Length'  argument. The  size of the buffer required to store the returned
%% information  log can be obtained by calling   {@link gl:getShaderiv/2}   with the value `?GL_INFO_LOG_LENGTH'
%% .
%%
%% The information log for a shader object is a string that  may contain diagnostic messages,
%% warning messages, and other  information about the last compile operation. When a shader
%%  object is created, its information log will be a string of  length 0.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderInfoLog.xml">external</a> documentation.
-spec getShaderInfoLog(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderInfoLog(Shader,BufSize) ->
  call(5462, <<Shader:?GLuint,BufSize:?GLsizei>>).

%% @doc Returns the source code string from a shader object
%%
%% ``gl:getShaderSource'' returns the concatenation of the source code strings from the
%% shader object specified by  `Shader' . The source code strings for a shader object
%% are the result of a previous call to  {@link gl:shaderSource/2} . The string returned by
%% the function will be null terminated.
%%
%% ``gl:getShaderSource'' returns in  `Source'  as much of the source code string as
%% it can, up to a maximum of  `BufSize'  characters. The number of characters actually
%% returned, excluding the null termination character, is specified by  `Length' . If
%% the length of the returned string is not required, a value of `?NULL' can be passed
%% in the  `Length'  argument. The size of the buffer required to store the returned source
%% code string can be obtained by calling  {@link gl:getShaderiv/2}  with the value `?GL_SHADER_SOURCE_LENGTH'
%% .
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSource.xml">external</a> documentation.
-spec getShaderSource(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderSource(Shader,BufSize) ->
  call(5463, <<Shader:?GLuint,BufSize:?GLsizei>>).

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
%% Uniform variables that are structures or arrays of structures may be queried by calling ``gl:getUniformLocation''
%%  for each field within the structure. The array element operator "[]" and the structure
%% field operator "." may be used in  `Name'  in order to select elements within an array
%% or fields within a structure. The result of using these operators is not allowed to be
%% another structure, an array of structures, or a subcomponent of a vector or a matrix.
%% Except if the last part of  `Name'  indicates a uniform variable array, the location
%% of the first element of an array can be retrieved by using the name of the array, or by
%% using the name appended by "[0]".
%%
%% The actual locations assigned to uniform variables are not known until the program object
%% is linked successfully. After linking has occurred, the command ``gl:getUniformLocation''
%%  can be used to obtain the location of a uniform variable. This location value can then
%% be passed to  {@link gl:uniform1f/2}  to set the value of the uniform variable or to  {@link gl:getUniformfv/2} 
%%  in order to query the current value of the uniform variable. After a program object has
%% been linked successfully, the index values for uniform variables remain fixed until the
%% next link command occurs. Uniform variable locations and values can only be queried after
%% a link if the link was successful.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocation.xml">external</a> documentation.
-spec getUniformLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getUniformLocation(Program,Name) ->
  call(5464, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

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
%% The locations assigned to uniform variables are not known until the program object is
%% linked. After linking has occurred, the command  {@link gl:getUniformLocation/2}  can be
%% used to obtain the location of a uniform variable. This location value can then be passed
%% to ``gl:getUniform'' in order to query the current value of the uniform variable. After
%% a program object has been linked successfully, the index values for uniform variables
%% remain fixed until the next link command occurs. The uniform variable values can only
%% be queried after a link if the link was successful.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniform.xml">external</a> documentation.
-spec getUniformfv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformfv(Program,Location) ->
  call(5465, <<Program:?GLuint,Location:?GLint>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformiv(Program,Location) ->
  call(5466, <<Program:?GLuint,Location:?GLint>>).

%% @doc Return a generic vertex attribute parameter
%%
%% ``gl:getVertexAttrib'' returns in  `Params'  the value of a generic vertex attribute
%% parameter. The generic vertex attribute to be queried is specified by  `Index' , and
%% the parameter to be queried is specified by  `Pname' .
%%
%% The accepted parameter names are as follows:
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING':  `Params'  returns a single value, the
%% name of the buffer object currently bound to the binding point corresponding to generic
%% vertex attribute array   `Index' . If no buffer object is bound,  0 is returned. The
%% initial value is 0.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_ENABLED':  `Params'  returns a single value that is non-zero
%% (true) if the vertex attribute array for  `Index'  is enabled and 0 (false) if it is
%% disabled. The initial value is `?GL_FALSE'.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_SIZE':  `Params'  returns a single value, the size of
%% the vertex attribute array for  `Index' . The size is the number of values for each
%% element of the vertex attribute array, and it will be 1, 2, 3, or 4. The initial value
%% is 4.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_STRIDE':  `Params'  returns a single value, the array
%% stride for (number of bytes between successive elements in) the vertex attribute array
%% for  `Index' . A value of 0 indicates that the array elements are stored sequentially
%% in memory. The initial value is 0.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_TYPE':  `Params'  returns a single value, a symbolic
%% constant indicating the array type for the vertex attribute array for  `Index' . Possible
%% values are `?GL_BYTE', `?GL_UNSIGNED_BYTE', `?GL_SHORT', `?GL_UNSIGNED_SHORT'
%% , `?GL_INT', `?GL_UNSIGNED_INT', `?GL_FLOAT', and `?GL_DOUBLE'. The
%% initial value is `?GL_FLOAT'.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_NORMALIZED':  `Params'  returns a single value that is
%% non-zero (true) if fixed-point data types for the vertex attribute array indicated by  `Index' 
%%  are normalized when they are converted to floating point, and 0 (false) otherwise. The
%% initial value is `?GL_FALSE'.
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_INTEGER':  `Params'  returns a single value that is non-zero
%% (true) if fixed-point data types for the vertex attribute array indicated by  `Index' 
%% have integer data types, and 0 (false) otherwise. The initial value is 0 (`?GL_FALSE').
%% 
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_DIVISOR':  `Params'  returns a single value that is the
%% frequency divisor used for instanced rendering. See  {@link gl:vertexAttribDivisor/2} . The
%% initial value is 0.
%%
%% `?GL_CURRENT_VERTEX_ATTRIB':  `Params'  returns four values that represent the
%% current value for the generic vertex attribute specified by index. Generic vertex attribute
%% 0 is unique in that it has no current state, so an error will be generated if  `Index' 
%% is 0. The initial value for all other generic vertex attributes is (0,0,0,1).
%%
%% ``gl:getVertexAttribdv'' and ``gl:getVertexAttribfv'' return the current attribute
%% values as four single-precision floating-point values; ``gl:getVertexAttribiv'' reads
%% them as floating-point values and converts them to four integer values; ``gl:getVertexAttribIiv''
%%  and ``gl:getVertexAttribIuiv'' read and return them as signed or unsigned integer values,
%% respectively; ``gl:getVertexAttribLdv'' reads and returns them as four double-precision
%% floating-point values. 
%%
%% All of the parameters except `?GL_CURRENT_VERTEX_ATTRIB' represent state stored in
%% the currently bound vertex array object.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttrib.xml">external</a> documentation.
-spec getVertexAttribdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribdv(Index,Pname) ->
  call(5467, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribfv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribfv(Index,Pname) ->
  call(5468, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribiv(Index,Pname) ->
  call(5469, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc Determines if a name corresponds to a program object
%%
%% ``gl:isProgram'' returns `?GL_TRUE' if  `Program'  is the name of a program
%% object previously created with   {@link gl:createProgram/0}   and not yet deleted with  {@link gl:deleteProgram/1} 
%% . If  `Program'  is zero or a non-zero value that is not the name of a program object,
%% or if an error occurs,  ``gl:isProgram'' returns `?GL_FALSE'.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsProgram.xml">external</a> documentation.
-spec isProgram(Program) -> 0|1 when Program :: integer().
isProgram(Program) ->
  call(5470, <<Program:?GLuint>>).

%% @doc Determines if a name corresponds to a shader object
%%
%% ``gl:isShader'' returns `?GL_TRUE' if  `Shader'  is the name of a shader object
%% previously created with   {@link gl:createShader/1}   and not yet deleted with  {@link gl:deleteShader/1} 
%% .  If  `Shader'  is zero or a non-zero value that is not the name of a shader object,
%% or if an error occurs, ``gl:isShader '' returns `?GL_FALSE'.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsShader.xml">external</a> documentation.
-spec isShader(Shader) -> 0|1 when Shader :: integer().
isShader(Shader) ->
  call(5471, <<Shader:?GLuint>>).

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
%% The status of the link operation will be stored as part of the program object's state.
%% This value will be set to `?GL_TRUE' if the program object was linked without errors
%% and is ready for use, and `?GL_FALSE' otherwise. It can be queried by calling  {@link gl:getProgramiv/2} 
%%  with arguments  `Program'  and `?GL_LINK_STATUS'.
%%
%% As a result of a successful link operation, all active user-defined uniform variables
%% belonging to  `Program'  will be initialized to 0, and each of the program object's
%% active uniform variables will be assigned a location that can be queried by calling  {@link gl:getUniformLocation/2} 
%% . Also, any active user-defined attribute variables that have not been bound to a generic
%% vertex attribute index will be bound to one at this time.
%%
%% Linking of a program object can fail for a number of reasons as specified in the `OpenGL Shading Language Specification'
%% . The following lists some of the conditions that will cause a link error.
%%
%% The number of active attribute variables supported by the implementation has been exceeded.
%% 
%%
%% The storage limit for uniform variables has been exceeded.
%%
%% The number of active uniform variables supported by the implementation has been exceeded.
%%
%% The `main' function is missing for the vertex, geometry or fragment shader.
%%
%% A varying variable actually used in the fragment shader is not declared in the same way
%% (or is not declared at all) in the vertex shader, or geometry shader shader if present.
%%
%% A reference to a function or variable name is unresolved.
%%
%% A shared global is declared with two different types or two different initial values.
%%
%% One or more of the attached shader objects has not been successfully compiled.
%%
%% Binding a generic attribute matrix caused some rows of the matrix to fall outside the
%% allowed maximum of `?GL_MAX_VERTEX_ATTRIBS'.
%%
%% Not enough contiguous vertex attribute slots could be found to bind attribute matrices.
%%
%% The program object contains objects to form a fragment shader but does not contain objects
%% to form a vertex shader.
%%
%% The program object contains objects to form a geometry shader but does not contain objects
%% to form a vertex shader.
%%
%% The program object contains objects to form a geometry shader and the input primitive
%% type, output primitive type, or maximum output vertex count is not specified in any compiled
%% geometry shader object.
%%
%% The program object contains objects to form a geometry shader and the input primitive
%% type, output primitive type, or maximum output vertex count is specified differently in
%% multiple geometry shader objects.
%%
%% The number of active outputs in the fragment shader is greater than the value of `?GL_MAX_DRAW_BUFFERS'
%% .
%%
%% The program has an active output assigned to a location greater than or equal to the value
%% of `?GL_MAX_DUAL_SOURCE_DRAW_BUFFERS' and has an active output assigned an index
%% greater than or equal to one.
%%
%% More than one varying out variable is bound to the same number and index.
%%
%% The explicit binding assigments do not leave enough space for the linker to automatically
%% assign a location for a varying out array, which requires multiple contiguous locations.
%%
%% The  `Count'  specified by  {@link gl:transformFeedbackVaryings/3}  is non-zero, but the
%% program object has no vertex or geometry shader.
%%
%% Any variable name specified to  {@link gl:transformFeedbackVaryings/3}  in the  `Varyings' 
%%  array is not declared as an output in the vertex shader (or the geometry shader, if active).
%% 
%%
%% Any two entries in the  `Varyings'  array given  {@link gl:transformFeedbackVaryings/3} 
%% specify the same varying variable.
%%
%% The total number of components to capture in any transform feedback varying variable is
%% greater than the constant `?GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS' and the
%% buffer mode is `?SEPARATE_ATTRIBS'.
%%
%% When a program object has been successfully linked, the program object can be made part
%% of current state by calling  {@link gl:useProgram/1} . Whether or not the link operation
%% was successful, the program object's information log will be overwritten. The information
%% log can be retrieved by calling  {@link gl:getProgramInfoLog/2} .
%%
%% ``gl:linkProgram'' will also install the generated executables as part of the current
%% rendering state if the link operation was successful and the specified program object
%% is already currently in use as a result of a previous call to  {@link gl:useProgram/1} .
%% If the program object currently in use is relinked unsuccessfully, its link status will
%% be set to `?GL_FALSE' , but the executables and associated state will remain part
%% of the current state until a subsequent call to ``gl:useProgram'' removes it from use.
%% After it is removed from use, it cannot be made part of current state until it has been
%% successfully relinked.
%%
%% If  `Program'  contains shader objects of type `?GL_VERTEX_SHADER', and optionally
%% of type `?GL_GEOMETRY_SHADER', but does not contain shader objects of type `?GL_FRAGMENT_SHADER'
%% , the vertex shader executable will be installed on the programmable vertex processor,
%% the geometry shader executable, if present, will be installed on the programmable geometry
%% processor, but no executable will be installed on the fragment processor. The results
%% of rasterizing primitives with such a program will be undefined.
%%
%% The program object's information log is updated and the program is generated at the time
%% of the link operation. After the link operation, applications are free to modify attached
%% shader objects, compile attached shader objects, detach shader objects, delete shader
%% objects, and attach additional shader objects. None of these operations affects the information
%% log or the program that is part of the program object.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgram.xml">external</a> documentation.
-spec linkProgram(Program) -> 'ok' when Program :: integer().
linkProgram(Program) ->
  cast(5472, <<Program:?GLuint>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSource.xml">external</a> documentation.
-spec shaderSource(Shader, String) -> 'ok' when Shader :: integer(),String :: iolist().
shaderSource(Shader,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  cast(5473, <<Shader:?GLuint,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+0) rem 8)) rem 8)>>).

%% @doc Installs a program object as part of current rendering state
%%
%% ``gl:useProgram'' installs the program  object specified by  `Program'  as part of
%%  current rendering state. One or more executables are created in  a program object by
%% successfully attaching shader objects to it  with   {@link gl:attachShader/2} ,  successfully
%% compiling the shader objects with   {@link gl:compileShader/1} ,  and successfully linking
%% the program object with   {@link gl:linkProgram/1} .  
%%
%% A program object will contain an executable that will run  on the vertex processor if
%% it contains one or more shader  objects of type `?GL_VERTEX_SHADER' that have  been
%% successfully compiled and linked. A program object will contain an  executable that will
%% run on the geometry processor if it contains one or  more shader objects of type `?GL_GEOMETRY_SHADER'
%%  that  have been successfully compiled and linked.  Similarly, a program object will contain
%% an executable that will run on the  fragment processor if it contains one or more shader
%% objects of type  `?GL_FRAGMENT_SHADER' that have been  successfully compiled and
%% linked.
%%
%% While a program object is in use, applications are free to  modify attached shader objects,
%% compile attached shader objects,  attach additional shader objects, and detach or delete
%% shader  objects. None of these operations will affect the executables  that are part of
%% the current state. However, relinking the  program object that is currently in use will
%% install the program  object as part of the current rendering state if the link  operation
%% was successful (see   {@link gl:linkProgram/1}   ). If the program object currently in use
%% is relinked  unsuccessfully, its link status will be set to  `?GL_FALSE', but the
%% executables and  associated state will remain part of the current state until a  subsequent
%% call to ``gl:useProgram'' removes it  from use. After it is removed from use, it cannot
%% be made part  of current state until it has been successfully relinked.
%%
%% If  `Program'  is zero, then the current rendering  state refers to an `invalid'
%% program object and the  results of shader execution are undefined. However, this is not
%% an error.
%%
%% If  `Program'  does not  contain shader objects of type `?GL_FRAGMENT_SHADER',
%% an  executable will be installed on the vertex, and possibly geometry processors,  but
%% the results of fragment shader execution will be undefined.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgram.xml">external</a> documentation.
-spec useProgram(Program) -> 'ok' when Program :: integer().
useProgram(Program) ->
  cast(5474, <<Program:?GLuint>>).

%% @doc Specify the value of a uniform variable for the current program object
%%
%% ``gl:uniform'' modifies the value of a uniform variable or a uniform variable array.
%% The location of the uniform variable to be modified is specified by  `Location' , which
%% should be a value returned by  {@link gl:getUniformLocation/2} . ``gl:uniform'' operates
%% on the program object that was made part of current state by calling  {@link gl:useProgram/1} 
%% .
%%
%% The commands ``gl:uniform{1|2|3|4}{f|i|ui}'' are used to change the value of the uniform
%% variable specified by  `Location'  using the values passed as arguments. The number
%% specified in the command should match the number of components in the data type of the
%% specified uniform variable (e.g., `1' for float, int, unsigned int, bool; `2'
%% for vec2, ivec2, uvec2, bvec2, etc.). The suffix `f' indicates that floating-point
%% values are being passed; the suffix `i' indicates that integer values are being passed;
%% the suffix `ui' indicates that unsigned integer values are being passed, and this
%% type should also match the data type of the specified uniform variable. The `i' variants
%% of this function should be used to provide values for uniform variables defined as int, ivec2
%% , ivec3, ivec4, or arrays of these. The `ui' variants of this function should be
%% used to provide values for uniform variables defined as unsigned int, uvec2, uvec3, uvec4,
%% or arrays of these. The `f' variants should be used to provide values for uniform
%% variables of type float, vec2, vec3, vec4, or arrays of these. Either the `i', `ui'
%%  or `f' variants may be used to provide values for uniform variables of type bool, bvec2
%% , bvec3, bvec4, or arrays of these. The uniform variable will be set to false if the input
%% value is 0 or 0.0f, and it will be set to true otherwise.
%%
%% All active uniform variables defined in a program object are initialized to 0 when the
%% program object is linked successfully. They retain the values assigned to them by a call
%% to ``gl:uniform '' until the next successful link operation occurs on the program object,
%% when they are once again initialized to 0.
%%
%% The commands ``gl:uniform{1|2|3|4}{f|i|ui}v'' can be used to modify a single uniform
%% variable or a uniform variable array. These commands pass a count and a pointer to the
%% values to be loaded into a uniform variable or a uniform variable array. A count of 1
%% should be used if modifying the value of a single uniform variable, and a count of 1 or
%% greater can be used to modify an entire array or part of an array. When loading `n'
%% elements starting at an arbitrary position `m' in a uniform variable array, elements
%% `m' + `n' - 1 in the array will be replaced with the new values. If  `M'  +  `N' 
%%  - 1 is larger than the size of the uniform variable array, values for all array elements
%% beyond the end of the array will be ignored. The number specified in the name of the command
%% indicates the number of components for each element in  `Value' , and it should match
%% the number of components in the data type of the specified uniform variable (e.g., `1'
%%  for float, int, bool; `2' for vec2, ivec2, bvec2, etc.). The data type specified
%% in the name of the command must match the data type for the specified uniform variable
%% as described previously for ``gl:uniform{1|2|3|4}{f|i|ui}''.
%%
%% For uniform variable arrays, each element of the array is considered to be of the type
%% indicated in the name of the command (e.g., ``gl:uniform3f'' or ``gl:uniform3fv''
%% can be used to load a uniform variable array of type vec3). The number of elements of
%% the uniform variable array to be modified is specified by  `Count' 
%%
%% The commands ``gl:uniformMatrix{2|3|4|2x3|3x2|2x4|4x2|3x4|4x3}fv''  are used to modify
%% a matrix or an array of matrices. The numbers in the command name are interpreted as the
%% dimensionality of the matrix. The number `2' indicates a 2 × 2 matrix (i.e., 4 values),
%% the number `3' indicates a 3 × 3 matrix (i.e., 9 values), and the number `4'
%% indicates a 4 × 4 matrix (i.e., 16 values). Non-square matrix dimensionality is explicit,
%% with the first number representing the number of columns and the second number representing
%% the number of rows. For example,  `2x4' indicates a 2 × 4 matrix with 2 columns and
%% 4 rows (i.e., 8 values). If  `Transpose'  is `?GL_FALSE', each matrix is assumed
%% to be supplied in column major order. If  `Transpose'  is `?GL_TRUE', each matrix
%% is assumed to be supplied in row major order. The  `Count'  argument indicates the
%% number of matrices to be passed. A count of 1 should be used if modifying the value of
%% a single matrix, and a count greater than 1 can be used to modify an array of matrices.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniform.xml">external</a> documentation.
-spec uniform1f(Location, V0) -> 'ok' when Location :: integer(),V0 :: float().
uniform1f(Location,V0) ->
  cast(5475, <<Location:?GLint,V0:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2f(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float().
uniform2f(Location,V0,V1) ->
  cast(5476, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3f(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
uniform3f(Location,V0,V1,V2) ->
  cast(5477, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4f(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
uniform4f(Location,V0,V1,V2,V3) ->
  cast(5478, <<Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1i(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1i(Location,V0) ->
  cast(5479, <<Location:?GLint,V0:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2i(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2i(Location,V0,V1) ->
  cast(5480, <<Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3i(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3i(Location,V0,V1,V2) ->
  cast(5481, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4i(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4i(Location,V0,V1,V2,V3) ->
  cast(5482, <<Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1fv(Location,Value) ->
  cast(5483, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2fv(Location,Value) ->
  cast(5484, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3fv(Location,Value) ->
  cast(5485, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4fv(Location,Value) ->
  cast(5486, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1iv(Location,Value) ->
  cast(5487, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2iv(Location,Value) ->
  cast(5488, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3iv(Location,Value) ->
  cast(5489, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4iv(Location,Value) ->
  cast(5490, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2fv(Location,Transpose,Value) ->
  cast(5491, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3fv(Location,Transpose,Value) ->
  cast(5492, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4fv(Location,Transpose,Value) ->
  cast(5493, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
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
%% The status of the validation operation will be stored as  part of the program object's
%% state. This value will be set to  `?GL_TRUE' if the validation succeeded, and  `?GL_FALSE'
%%  otherwise. It can be queried by  calling   {@link gl:getProgramiv/2}   with arguments  `Program' 
%%  and  `?GL_VALIDATE_STATUS'. If validation is  successful,  `Program'  is guaranteed
%% to  execute given the current state. Otherwise,   `Program'  is guaranteed to not execute.
%% 
%%
%% This function is typically useful only during application  development. The informational
%% string stored in the information  log is completely implementation dependent; therefore,
%% an  application should not expect different OpenGL implementations  to produce identical
%% information strings.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgram.xml">external</a> documentation.
-spec validateProgram(Program) -> 'ok' when Program :: integer().
validateProgram(Program) ->
  cast(5494, <<Program:?GLuint>>).

%% @doc Specifies the value of a generic vertex attribute
%%
%% The ``gl:vertexAttrib'' family of entry points allows an application to pass generic
%% vertex attributes in numbered locations.
%%
%% Generic attributes are defined as four-component values that are organized into an array.
%% The first entry of this array is numbered 0, and the size of the array is specified by
%% the implementation-dependent constant `?GL_MAX_VERTEX_ATTRIBS'. Individual elements
%% of this array can be modified with a ``gl:vertexAttrib'' call that specifies the index
%% of the element to be modified and a value for that element.
%%
%% These commands can be used to specify one, two, three, or all four components of the generic
%% vertex attribute specified by  `Index' . A `1' in the name of the command indicates
%% that only one value is passed, and it will be used to modify the first component of the
%% generic vertex attribute. The second and third components will be set to 0, and the fourth
%% component will be set to 1. Similarly, a `2' in the name of the command indicates
%% that values are provided for the first two components, the third component will be set
%% to 0, and the fourth component will be set to 1. A `3' in the name of the command
%% indicates that values are provided for the first three components and the fourth component
%% will be set to 1, whereas a `4' in the name indicates that values are provided for
%% all four components.
%%
%% The letters `s', `f', `i', `d', `ub', `us', and `ui'
%% indicate whether the arguments are of type short, float, int, double, unsigned byte, unsigned
%% short, or unsigned int. When `v' is appended to the name, the commands can take a
%% pointer to an array of such values.
%%
%% Additional capitalized letters can indicate further alterations to the default behavior
%% of the glVertexAttrib function:
%%
%%  The commands containing `N' indicate that  the arguments will be passed as fixed-point
%% values that are  scaled to a normalized range according to the component  conversion rules
%% defined by the OpenGL specification. Signed values are understood to represent fixed-point
%% values in the range [-1,1], and unsigned values are understood to represent fixed-point
%% values in the range [0,1]. 
%%
%%  The commands containing `I' indicate that  the arguments are extended to full signed
%% or unsigned integers. 
%%
%%  The commands containing `P' indicate that  the arguments are stored as packed components
%% within a larger natural type. 
%%
%%  The commands containing `L' indicate that the arguments are full 64-bit quantities
%% and should be passed directly to shader inputs declared as 64-bit double precision types.
%% 
%%
%% OpenGL Shading Language attribute variables are allowed to be of type mat2, mat3, or mat4.
%% Attributes of these types may be loaded using the ``gl:vertexAttrib'' entry points.
%% Matrices must be loaded into successive generic attribute slots in column major order,
%% with one column of the matrix in each generic attribute slot.
%%
%% A user-defined attribute variable declared in a vertex shader can be bound to a generic
%% attribute index by calling  {@link gl:bindAttribLocation/3} . This allows an application
%% to use more descriptive variable names in a vertex shader. A subsequent change to the
%% specified generic vertex attribute will be immediately reflected as a change to the corresponding
%% attribute variable in the vertex shader.
%%
%% The binding between a generic vertex attribute index and a user-defined attribute variable
%% in a vertex shader is part of the state of a program object, but the current value of
%% the generic vertex attribute is not. The value of each generic vertex attribute is part
%% of current state, just like standard vertex attributes, and it is maintained even if a
%% different program object is used.
%%
%% An application may freely modify generic vertex attributes that are not bound to a named
%% vertex shader attribute variable. These values are simply maintained as part of current
%% state and will not be accessed by the vertex shader. If a generic vertex attribute bound
%% to an attribute variable in a vertex shader is not updated while the vertex shader is
%% executing, the vertex shader will repeatedly use the current value for the generic vertex
%% attribute.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttrib.xml">external</a> documentation.
-spec vertexAttrib1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1d(Index,X) ->
  cast(5495, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @equiv vertexAttrib1d(Index,X)
-spec vertexAttrib1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib1f(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1f(Index,X) ->
  cast(5496, <<Index:?GLuint,X:?GLfloat>>).

%% @equiv vertexAttrib1f(Index,X)
-spec vertexAttrib1fv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib1s(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttrib1s(Index,X) ->
  cast(5497, <<Index:?GLuint,X:?GLshort>>).

%% @equiv vertexAttrib1s(Index,X)
-spec vertexAttrib1sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2d(Index,X,Y) ->
  cast(5498, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @equiv vertexAttrib2d(Index,X,Y)
-spec vertexAttrib2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2f(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2f(Index,X,Y) ->
  cast(5499, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat>>).

%% @equiv vertexAttrib2f(Index,X,Y)
-spec vertexAttrib2fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib2s(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttrib2s(Index,X,Y) ->
  cast(5500, <<Index:?GLuint,X:?GLshort,Y:?GLshort>>).

%% @equiv vertexAttrib2s(Index,X,Y)
-spec vertexAttrib2sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3d(Index,X,Y,Z) ->
  cast(5501, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @equiv vertexAttrib3d(Index,X,Y,Z)
-spec vertexAttrib3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3f(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3f(Index,X,Y,Z) ->
  cast(5502, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat>>).

%% @equiv vertexAttrib3f(Index,X,Y,Z)
-spec vertexAttrib3fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib3s(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttrib3s(Index,X,Y,Z) ->
  cast(5503, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort>>).

%% @equiv vertexAttrib3s(Index,X,Y,Z)
-spec vertexAttrib3sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nbv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nbv(Index,{V1,V2,V3,V4}) ->
  cast(5504, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Niv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Niv(Index,{V1,V2,V3,V4}) ->
  cast(5505, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nsv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nsv(Index,{V1,V2,V3,V4}) ->
  cast(5506, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nub(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4Nub(Index,X,Y,Z,W) ->
  cast(5507, <<Index:?GLuint,X:?GLubyte,Y:?GLubyte,Z:?GLubyte,W:?GLubyte>>).

%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
-spec vertexAttrib4Nubv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nuiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nuiv(Index,{V1,V2,V3,V4}) ->
  cast(5508, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4Nusv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nusv(Index,{V1,V2,V3,V4}) ->
  cast(5509, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4bv(Index,{V1,V2,V3,V4}) ->
  cast(5510, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4d(Index,X,Y,Z,W) ->
  cast(5511, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
-spec vertexAttrib4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4f(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4f(Index,X,Y,Z,W) ->
  cast(5512, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
-spec vertexAttrib4fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4iv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4iv(Index,{V1,V2,V3,V4}) ->
  cast(5513, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4s(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4s(Index,X,Y,Z,W) ->
  cast(5514, <<Index:?GLuint,X:?GLshort,Y:?GLshort,Z:?GLshort,W:?GLshort>>).

%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
-spec vertexAttrib4sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5515, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4uiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4uiv(Index,{V1,V2,V3,V4}) ->
  cast(5516, <<Index:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttrib4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4usv(Index,{V1,V2,V3,V4}) ->
  cast(5517, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc Define an array of generic vertex attribute data
%%
%% ``gl:vertexAttribPointer'', ``gl:vertexAttribIPointer'' and ``gl:vertexAttribLPointer''
%%  specify the location and data format of the array of generic vertex attributes at index  `Index' 
%%  to use when rendering.  `Size'  specifies the number of components per attribute and
%% must be 1, 2, 3, 4, or `?GL_BGRA'.  `Type'  specifies the data type of each component,
%% and  `Stride'  specifies the byte stride from one attribute to the next, allowing vertices
%% and attributes to be packed into a single array or stored in separate arrays. 
%%
%%  For ``gl:vertexAttribPointer'', if  `Normalized'  is set to `?GL_TRUE', it
%% indicates that values stored in an integer format are to be mapped to the range [-1,1]
%% (for signed values) or [0,1] (for unsigned values) when they are accessed and converted
%% to floating point. Otherwise, values will be converted to floats directly without normalization.
%% 
%%
%%  For ``gl:vertexAttribIPointer'', only the integer types `?GL_BYTE', `?GL_UNSIGNED_BYTE'
%% , `?GL_SHORT', `?GL_UNSIGNED_SHORT', `?GL_INT', `?GL_UNSIGNED_INT'
%% are accepted. Values are always left as integer values. 
%%
%% ``gl:vertexAttribLPointer'' specifies state for a generic vertex attribute array associated
%% with a shader attribute variable declared with 64-bit double precision components.  `Type' 
%%  must be `?GL_DOUBLE'.  `Index' ,  `Size' , and  `Stride'  behave as described
%% for ``gl:vertexAttribPointer'' and ``gl:vertexAttribIPointer''. 
%%
%%  If  `Pointer'  is not NULL, a non-zero named buffer object must be bound to the `?GL_ARRAY_BUFFER'
%%  target (see  {@link gl:bindBuffer/2} ), otherwise an error is generated.  `Pointer' 
%% is treated as a byte offset into the buffer object's data store. The buffer object binding
%% (`?GL_ARRAY_BUFFER_BINDING') is saved as generic vertex attribute array state (`?GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING'
%% ) for index  `Index' . 
%%
%%  When a generic vertex attribute array is specified,  `Size' ,  `Type' ,  `Normalized' 
%% ,  `Stride' , and  `Pointer'  are saved as vertex array state, in addition to the
%% current vertex array buffer object binding. 
%%
%%  To enable and disable a generic vertex attribute array, call  {@link gl:disableVertexAttribArray/1} 
%%  and  {@link gl:disableVertexAttribArray/1}  with  `Index' . If enabled, the generic vertex
%% attribute array is used when  {@link gl:drawArrays/3} ,  {@link gl:multiDrawArrays/3} ,  {@link gl:drawElements/4} 
%% , see `glMultiDrawElements', or  {@link gl:drawRangeElements/6}  is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribPointer.xml">external</a> documentation.
-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Stride :: integer(),Pointer :: offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5518, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5519, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Normalized:?GLboolean,0:24,Stride:?GLsizei>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3fv(Location,Transpose,Value) ->
  cast(5520, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2fv(Location,Transpose,Value) ->
  cast(5521, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4fv(Location,Transpose,Value) ->
  cast(5522, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2fv(Location,Transpose,Value) ->
  cast(5523, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4fv(Location,Transpose,Value) ->
  cast(5524, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3fv(Location,Transpose,Value) ->
  cast(5525, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc glColorMaski
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glColorMaski.xml">external</a> documentation.
-spec colorMaski(Index, R, G, B, A) -> 'ok' when Index :: integer(),R :: 0|1,G :: 0|1,B :: 0|1,A :: 0|1.
colorMaski(Index,R,G,B,A) ->
  cast(5526, <<Index:?GLuint,R:?GLboolean,G:?GLboolean,B:?GLboolean,A:?GLboolean>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getBooleani_v(Target, Index) -> [0|1] when Target :: enum(),Index :: integer().
getBooleani_v(Target,Index) ->
  call(5527, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getIntegeri_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getIntegeri_v(Target,Index) ->
  call(5528, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link enable/1}
-spec enablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
enablei(Target,Index) ->
  cast(5529, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glEnablei
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glEnablei.xml">external</a> documentation.
-spec disablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
disablei(Target,Index) ->
  cast(5530, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glIsEnabledi
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsEnabledi.xml">external</a> documentation.
-spec isEnabledi(Target, Index) -> 0|1 when Target :: enum(),Index :: integer().
isEnabledi(Target,Index) ->
  call(5531, <<Target:?GLenum,Index:?GLuint>>).

%% @doc Start transform feedback operation
%%
%%  Transform feedback mode captures the values of varying variables written by the vertex
%% shader (or, if active, the geometry shader). Transform feedback is said to be active after
%% a call to ``gl:beginTransformFeedback'' until a subsequent call to  {@link gl:beginTransformFeedback/1} 
%% . Transform feedback commands must be paired. 
%%
%%  If no geometry shader is present, while transform feedback is active the  `Mode' 
%% parameter to  {@link gl:drawArrays/3}  must match those specified in the following table: <table>
%% <tbody><tr><td>` Transform Feedback ' `PrimitiveMode' </td><td>` Allowed Render Primitive '
%%  `Modes' </td></tr></tbody><tbody><tr><td>`?GL_POINTS'</td><td>`?GL_POINTS'</td>
%% </tr><tr><td>`?GL_LINES'</td><td>`?GL_LINES', `?GL_LINE_LOOP', `?GL_LINE_STRIP'
%% , `?GL_LINES_ADJACENCY', `?GL_LINE_STRIP_ADJACENCY'</td></tr><tr><td>`?GL_TRIANGLES'
%% </td><td>`?GL_TRIANGLES', `?GL_TRIANGLE_STRIP', `?GL_TRIANGLE_FAN', `?GL_TRIANGLES_ADJACENCY'
%% , `?GL_TRIANGLE_STRIP_ADJACENCY'</td></tr></tbody></table>
%%
%%  If a geometry shader is present, the output primitive type from the geometry shader must
%% match those provided in the following table: <table><tbody><tr><td>` Transform Feedback '
%%  `PrimitiveMode' </td><td>` Allowed Geometry Shader Output Primitive Type '</td></tr>
%% </tbody><tbody><tr><td>`?GL_POINTS'</td><td>`?points'</td></tr><tr><td>`?GL_LINES'
%% </td><td>`?line_strip'</td></tr><tr><td>`?GL_TRIANGLES'</td><td>`?triangle_strip'
%% </td></tr></tbody></table>
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginTransformFeedback.xml">external</a> documentation.
-spec beginTransformFeedback(PrimitiveMode) -> 'ok' when PrimitiveMode :: enum().
beginTransformFeedback(PrimitiveMode) ->
  cast(5532, <<PrimitiveMode:?GLenum>>).

%% @doc 
%% See {@link beginTransformFeedback/1}
-spec endTransformFeedback() -> 'ok'.
endTransformFeedback() ->
  cast(5533, <<>>).

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
%%  `Offset'  specifies the offset in basic machine units into the buffer object  `Buffer' 
%%  and  `Size'  specifies the amount of data that can be read from the buffer object
%% while used as an indexed target. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferRange.xml">external</a> documentation.
-spec bindBufferRange(Target, Index, Buffer, Offset, Size) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer(),Offset :: integer(),Size :: integer().
bindBufferRange(Target,Index,Buffer,Offset,Size) ->
  cast(5534, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint,0:32,Offset:?GLintptr,Size:?GLsizeiptr>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindBufferBase.xml">external</a> documentation.
-spec bindBufferBase(Target, Index, Buffer) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer().
bindBufferBase(Target,Index,Buffer) ->
  cast(5535, <<Target:?GLenum,Index:?GLuint,Buffer:?GLuint>>).

%% @doc Specify values to record in transform feedback buffers
%%
%%  The names of the vertex or geometry shader outputs to be recorded in transform feedback
%% mode are specified using ``gl:transformFeedbackVaryings''. When a geometry shader is
%% active, transform feedback records the values of selected geometry shader output variables
%% from the emitted vertices. Otherwise, the values of the selected vertex shader outputs
%% are recorded. 
%%
%%  The state set by ``gl:tranformFeedbackVaryings'' is stored and takes effect next time  {@link gl:linkProgram/1} 
%%  is called on  `Program' . When  {@link gl:linkProgram/1}  is called,  `Program'  is
%% linked so that the values of the specified varying variables for the vertices of each
%% primitive generated by the GL are written to a single buffer object if  `BufferMode' 
%% is `?GL_INTERLEAVED_ATTRIBS' or multiple buffer objects if  `BufferMode'  is `?GL_SEPARATE_ATTRIBS'
%% . 
%%
%%  In addition to the errors generated by ``gl:transformFeedbackVaryings'', the program  `Program' 
%%  will fail to link if: 
%%
%%  The count specified by ``gl:transformFeedbackVaryings'' is non-zero, but the program
%% object has no vertex or geometry shader. 
%%
%%  Any variable name specified in the  `Varyings'  array is not declared as an output
%% in the vertex shader (or the geometry shader, if active). 
%%
%%  Any two entries in the  `Varyings'  array specify the same varying variable. 
%%
%%  The total number of components to capture in any varying variable in  `Varyings' 
%% is greater than the constant `?GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS' and
%% the buffer mode is `?GL_SEPARATE_ATTRIBS'. 
%%
%%  The total number of components to capture is greater than the constant `?GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS'
%%  and the buffer mode is `?GL_INTERLEAVED_ATTRIBS'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTransformFeedbackVaryings.xml">external</a> documentation.
-spec transformFeedbackVaryings(Program, Varyings, BufferMode) -> 'ok' when Program :: integer(),Varyings :: iolist(),BufferMode :: enum().
transformFeedbackVaryings(Program,Varyings,BufferMode) ->
 VaryingsTemp = list_to_binary([[Str|[0]] || Str <- Varyings ]),
  cast(5536, <<Program:?GLuint,(length(Varyings)):?GLuint,(size(VaryingsTemp)):?GLuint,(VaryingsTemp)/binary,0:((8-((size(VaryingsTemp)+0) rem 8)) rem 8),BufferMode:?GLenum>>).

%% @doc Retrieve information about varying variables selected for transform feedback
%%
%%  Information about the set of varying variables in a linked program that will be captured
%% during transform feedback may be retrieved by calling ``gl:getTransformFeedbackVarying''.
%% ``gl:getTransformFeedbackVarying'' provides information about the varying variable selected
%% by  `Index' . An  `Index'  of 0 selects the first varying variable specified in
%% the  `Varyings'  array passed to  {@link gl:transformFeedbackVaryings/3} , and an  `Index' 
%%  of `?GL_TRANSFORM_FEEDBACK_VARYINGS-1' selects the last such variable. 
%%
%%  The name of the selected varying is returned as a null-terminated string in  `Name' .
%% The actual number of characters written into  `Name' , excluding the null terminator,
%% is returned in  `Length' . If  `Length'  is NULL, no length is returned. The maximum
%% number of characters that may be written into  `Name' , including the null terminator,
%% is specified by  `BufSize' . 
%%
%%  The length of the longest varying name in program is given by `?GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH'
%% , which can be queried with  {@link gl:getProgramiv/2} . 
%%
%%  For the selected varying variable, its type is returned into  `Type' . The size of
%% the varying is returned into  `Size' . The value in  `Size'  is in units of the
%% type returned in  `Type' . The type returned can be any of the scalar, vector, or matrix
%% attribute types returned by  {@link gl:getActiveAttrib/3} . If an error occurred, the return
%% parameters  `Length' ,  `Size' ,  `Type'  and  `Name'  will be unmodified.
%% This command will return as much information about the varying variables as possible.
%% If no information is available,  `Length'  will be set to zero and  `Name'  will
%% be an empty string. This situation could arise if ``gl:getTransformFeedbackVarying''
%% is called after a failed link. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTransformFeedbackVarying.xml">external</a> documentation.
-spec getTransformFeedbackVarying(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getTransformFeedbackVarying(Program,Index,BufSize) ->
  call(5537, <<Program:?GLuint,Index:?GLuint,BufSize:?GLsizei>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClampColor.xml">external</a> documentation.
-spec clampColor(Target, Clamp) -> 'ok' when Target :: enum(),Clamp :: enum().
clampColor(Target,Clamp) ->
  cast(5538, <<Target:?GLenum,Clamp:?GLenum>>).

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
%%  If  `Mode'  is `?GL_QUERY_BY_REGION_WAIT', the GL will also wait for occlusion
%% query results and discard rendering commands if the result of the occlusion query is zero.
%% If the query result is non-zero, subsequent rendering commands are executed, but the GL
%% may discard the results of the commands for any region of the framebuffer that did not
%% contribute to the sample count in the specified occlusion query. Any such discarding is
%% done in an implementation-dependent manner, but the rendering command results may not
%% be discarded for any samples that contributed to the occlusion query sample count. If  `Mode' 
%%  is `?GL_QUERY_BY_REGION_NO_WAIT', the GL operates as in `?GL_QUERY_BY_REGION_WAIT'
%% , but may choose to unconditionally execute the subsequent rendering commands without
%% waiting for the query to complete. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginConditionalRender.xml">external</a> documentation.
-spec beginConditionalRender(Id, Mode) -> 'ok' when Id :: integer(),Mode :: enum().
beginConditionalRender(Id,Mode) ->
  cast(5539, <<Id:?GLuint,Mode:?GLenum>>).

%% @doc 
%% See {@link beginConditionalRender/2}
-spec endConditionalRender() -> 'ok'.
endConditionalRender() ->
  cast(5540, <<>>).

%% @doc glVertexAttribIPointer
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribIPointer.xml">external</a> documentation.
-spec vertexAttribIPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5541, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5542, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc 
%% See {@link getVertexAttribdv/2}
-spec getVertexAttribIiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIiv(Index,Pname) ->
  call(5543, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc glGetVertexAttribI
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribI.xml">external</a> documentation.
-spec getVertexAttribIuiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIuiv(Index,Pname) ->
  call(5544, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI1i(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1i(Index,X) ->
  cast(5545, <<Index:?GLuint,X:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI2i(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2i(Index,X,Y) ->
  cast(5546, <<Index:?GLuint,X:?GLint,Y:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI3i(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3i(Index,X,Y,Z) ->
  cast(5547, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4i(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4i(Index,X,Y,Z,W) ->
  cast(5548, <<Index:?GLuint,X:?GLint,Y:?GLint,Z:?GLint,W:?GLint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI1ui(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1ui(Index,X) ->
  cast(5549, <<Index:?GLuint,X:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI2ui(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2ui(Index,X,Y) ->
  cast(5550, <<Index:?GLuint,X:?GLuint,Y:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI3ui(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3ui(Index,X,Y,Z) ->
  cast(5551, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4ui(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4ui(Index,X,Y,Z,W) ->
  cast(5552, <<Index:?GLuint,X:?GLuint,Y:?GLuint,Z:?GLuint,W:?GLuint>>).

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
  cast(5553, <<Index:?GLuint,V1:?GLbyte,V2:?GLbyte,V3:?GLbyte,V4:?GLbyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4sv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4sv(Index,{V1,V2,V3,V4}) ->
  cast(5554, <<Index:?GLuint,V1:?GLshort,V2:?GLshort,V3:?GLshort,V4:?GLshort>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4ubv(Index,{V1,V2,V3,V4}) ->
  cast(5555, <<Index:?GLuint,V1:?GLubyte,V2:?GLubyte,V3:?GLubyte,V4:?GLubyte>>).

%% @doc 
%% See {@link vertexAttrib1d/2}
-spec vertexAttribI4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4usv(Index,{V1,V2,V3,V4}) ->
  cast(5556, <<Index:?GLuint,V1:?GLushort,V2:?GLushort,V3:?GLushort,V4:?GLushort>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformuiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformuiv(Program,Location) ->
  call(5557, <<Program:?GLuint,Location:?GLint>>).

%% @doc Bind a user-defined varying out variable to a fragment shader color number
%%
%% ``gl:bindFragDataLocation'' explicitly specifies the binding of the user-defined varying
%% out variable  `Name'  to fragment shader color number  `ColorNumber'  for program  `Program' 
%% . If  `Name'  was bound previously, its assigned binding is replaced with  `ColorNumber' 
%% .  `Name'  must be a null-terminated string.  `ColorNumber'  must be less than `?GL_MAX_DRAW_BUFFERS'
%% . 
%%
%%  The bindings specified by ``gl:bindFragDataLocation'' have no effect until  `Program' 
%%  is next linked. Bindings may be specified at any time after  `Program'  has been created.
%% Specifically, they may be specified before shader objects are attached to the program.
%% Therefore, any name may be specified in  `Name' , including a name that is never used
%% as a varying out variable in any fragment shader object. Names beginning with `?gl_'
%% are reserved by the GL. 
%%
%%  In addition to the errors generated by ``gl:bindFragDataLocation'', the program  `Program' 
%%  will fail to link if: 
%%
%%  The number of active outputs is greater than the value `?GL_MAX_DRAW_BUFFERS'. 
%%
%%  More than one varying out variable is bound to the same color number. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFragDataLocation.xml">external</a> documentation.
-spec bindFragDataLocation(Program, Color, Name) -> 'ok' when Program :: integer(),Color :: integer(),Name :: string().
bindFragDataLocation(Program,Color,Name) ->
  cast(5558, <<Program:?GLuint,Color:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc Query the bindings of color numbers to user-defined varying out variables
%%
%% ``gl:getFragDataLocation'' retrieves the assigned color number binding for the user-defined
%% varying out variable  `Name'  for program  `Program' .  `Program'  must have
%% previously been linked.  `Name'  must be a null-terminated string. If  `Name'  is
%% not the name of an active user-defined varying out fragment shader variable within  `Program' 
%% , -1 will be returned. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFragDataLocation.xml">external</a> documentation.
-spec getFragDataLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataLocation(Program,Name) ->
  call(5559, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1ui(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1ui(Location,V0) ->
  cast(5560, <<Location:?GLint,V0:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2ui(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2ui(Location,V0,V1) ->
  cast(5561, <<Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3ui(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3ui(Location,V0,V1,V2) ->
  cast(5562, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4ui(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4ui(Location,V0,V1,V2,V3) ->
  cast(5563, <<Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1uiv(Location,Value) ->
  cast(5564, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2uiv(Location,Value) ->
  cast(5565, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3uiv(Location,Value) ->
  cast(5566, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4uiv(Location,Value) ->
  cast(5567, <<Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link texParameterf/3}
-spec texParameterIiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIiv(Target,Pname,Params) ->
  cast(5568, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc glTexParameterI
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexParameterI.xml">external</a> documentation.
-spec texParameterIuiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIuiv(Target,Pname,Params) ->
  cast(5569, <<Target:?GLenum,Pname:?GLenum,(size(Params)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Params)>>)/binary,0:(((1+size(Params)) rem 2)*32)>>).

%% @doc 
%% See {@link getTexParameterfv/2}
-spec getTexParameterIiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIiv(Target,Pname) ->
  call(5570, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glGetTexParameterI
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetTexParameterI.xml">external</a> documentation.
-spec getTexParameterIuiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIuiv(Target,Pname) ->
  call(5571, <<Target:?GLenum,Pname:?GLenum>>).

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
%%  If  `Buffer'  is `?GL_DEPTH',  `DrawBuffer'  must be zero, and  `Value' 
%% points to a single value to clear the depth buffer to. Only ``gl:clearBufferfv'' should
%% be used to clear depth buffers. Clamping and conversion for fixed-point depth buffers
%% are performed in the same fashion as  {@link gl:clearDepth/1} . 
%%
%%  If  `Buffer'  is `?GL_STENCIL',  `DrawBuffer'  must be zero, and  `Value' 
%% points to a single value to clear the stencil buffer to. Only ``gl:clearBufferiv'' should
%% be used to clear stencil buffers. Masing and type conversion are performed in the same
%% fashion as  {@link gl:clearStencil/1} . 
%%
%% ``gl:clearBufferfi'' may be used to clear the depth and stencil buffers.  `Buffer' 
%% must be `?GL_DEPTH_STENCIL' and  `DrawBuffer'  must be zero.  `Depth'  and   `Stencil' 
%%  are the depth and stencil values, respectively. 
%%
%%  The result of ``gl:clearBuffer'' is undefined if no conversion between the type of  `Value' 
%%  and the buffer being cleared is defined. However, this is not an error. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBuffer.xml">external</a> documentation.
-spec clearBufferiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferiv(Buffer,Drawbuffer,Value) ->
  cast(5572, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link clearBufferiv/3}
-spec clearBufferuiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferuiv(Buffer,Drawbuffer,Value) ->
  cast(5573, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLuint>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link clearBufferiv/3}
-spec clearBufferfv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferfv(Buffer,Drawbuffer,Value) ->
  cast(5574, <<Buffer:?GLenum,Drawbuffer:?GLint,(size(Value)):?GLuint,
      (<< <<C:?GLfloat>> ||C <- tuple_to_list(Value)>>)/binary,0:(((1+size(Value)) rem 2)*32)>>).

%% @doc glClearBufferfi
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearBufferfi.xml">external</a> documentation.
-spec clearBufferfi(Buffer, Drawbuffer, Depth, Stencil) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Depth :: float(),Stencil :: integer().
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) ->
  cast(5575, <<Buffer:?GLenum,Drawbuffer:?GLint,Depth:?GLfloat,Stencil:?GLint>>).

%% @doc 
%% See {@link getString/1}
-spec getStringi(Name, Index) -> string() when Name :: enum(),Index :: integer().
getStringi(Name,Index) ->
  call(5576, <<Name:?GLenum,Index:?GLuint>>).

%% @doc glDrawArraysInstance
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysInstance.xml">external</a> documentation.
-spec drawArraysInstanced(Mode, First, Count, Primcount) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer().
drawArraysInstanced(Mode,First,Count,Primcount) ->
  cast(5577, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei>>).

%% @doc glDrawElementsInstance
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstance.xml">external</a> documentation.
-spec drawElementsInstanced(Mode, Count, Type, Indices, Primcount) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer().
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) when  is_integer(Indices) ->
  cast(5578, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei>>);
drawElementsInstanced(Mode,Count,Type,Indices,Primcount) ->
  send_bin(Indices),
  cast(5579, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei>>).

%% @doc Attach the storage for a buffer object to the active buffer texture
%%
%% ``gl:texBuffer'' attaches the storage for the buffer object named  `Buffer'  to the
%% active buffer texture, and specifies the internal format for the texel array found in
%% the attached buffer object. If  `Buffer'  is zero, any buffer object attached to the
%% buffer texture is detached and no new buffer object is attached. If  `Buffer'  is non-zero,
%% it must be the name of an existing buffer object.  `Target'  must be `?GL_TEXTURE_BUFFER'
%% .  `Internalformat'  specifies the storage format, and must be one of the following
%% sized internal formats: <table><tbody><tr><td></td><td></td><td></td><td></td><td>` Component '
%% </td></tr></tbody><tbody><tr><td>`Sized Internal Format'</td><td>`Base Type'</td>
%% <td>`Components'</td><td>`Norm'</td><td>0</td><td>1</td><td>2</td><td>3</td></tr>
%% <tr><td>`?GL_R8'</td><td>ubyte</td><td>1</td><td>YES</td><td>R</td><td>0</td><td>0</td>
%% <td>1</td></tr><tr><td>`?GL_R16'</td><td>ushort</td><td>1</td><td>YES</td><td>R</td><td>
%% 0</td><td>0</td><td>1</td></tr><tr><td>`?GL_R16F'</td><td>half</td><td>1</td><td>NO</td>
%% <td>R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>`?GL_R32F'</td><td>float</td><td>
%% 1</td><td>NO</td><td>R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>`?GL_R8I'</td><td>
%% byte</td><td>1</td><td>NO</td><td>R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>`?GL_R16I'
%% </td><td>short</td><td>1</td><td>NO</td><td>R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>
%% `?GL_R32I'</td><td>int</td><td>1</td><td>NO</td><td>R</td><td>0</td><td>0</td><td>1</td>
%% </tr><tr><td>`?GL_R8UI'</td><td>ubyte</td><td>1</td><td>NO</td><td>R</td><td>0</td><td>
%% 0</td><td>1</td></tr><tr><td>`?GL_R16UI'</td><td>ushort</td><td>1</td><td>NO</td><td>
%% R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>`?GL_R32UI'</td><td>uint</td><td>1</td>
%% <td>NO</td><td>R</td><td>0</td><td>0</td><td>1</td></tr><tr><td>`?GL_RG8'</td><td>ubyte
%% </td><td>2</td><td>YES</td><td>R</td><td>G</td><td>0</td><td>1</td></tr><tr><td>`?GL_RG16'
%% </td><td>ushort</td><td>2</td><td>YES</td><td>R</td><td>G</td><td>0</td><td>1</td></tr><tr>
%% <td>`?GL_RG16F'</td><td>half</td><td>2</td><td>NO</td><td>R</td><td>G</td><td>0</td><td>
%% 1</td></tr><tr><td>`?GL_RG32F'</td><td>float</td><td>2</td><td>NO</td><td>R</td><td>G
%% </td><td>0</td><td>1</td></tr><tr><td>`?GL_RG8I'</td><td>byte</td><td>2</td><td>NO</td>
%% <td>R</td><td>G</td><td>0</td><td>1</td></tr><tr><td>`?GL_RG16I'</td><td>short</td><td>
%% 2</td><td>NO</td><td>R</td><td>G</td><td>0</td><td>1</td></tr><tr><td>`?GL_RG32I'</td>
%% <td>int</td><td>2</td><td>NO</td><td>R</td><td>G</td><td>0</td><td>1</td></tr><tr><td>`?GL_RG8UI'
%% </td><td>ubyte</td><td>2</td><td>NO</td><td>R</td><td>G</td><td>0</td><td>1</td></tr><tr><td>
%% `?GL_RG16UI'</td><td>ushort</td><td>2</td><td>NO</td><td>R</td><td>G</td><td>0</td><td>
%% 1</td></tr><tr><td>`?GL_RG32UI'</td><td>uint</td><td>2</td><td>NO</td><td>R</td><td>G
%% </td><td>0</td><td>1</td></tr><tr><td>`?GL_RGB32F'</td><td>float</td><td>3</td><td>NO
%% </td><td>R</td><td>G</td><td>B</td><td>1</td></tr><tr><td>`?GL_RGB32I'</td><td>int</td>
%% <td>3</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>1</td></tr><tr><td>`?GL_RGB32UI'
%% </td><td>uint</td><td>3</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>1</td></tr><tr><td>
%% `?GL_RGBA8'</td><td>uint</td><td>4</td><td>YES</td><td>R</td><td>G</td><td>B</td><td>
%% A</td></tr><tr><td>`?GL_RGBA16'</td><td>short</td><td>4</td><td>YES</td><td>R</td><td>
%% G</td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA16F'</td><td>half</td><td>4</td><td>NO
%% </td><td>R</td><td>G</td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA32F'</td><td>float
%% </td><td>4</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA8I'
%% </td><td>byte</td><td>4</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>A</td></tr><tr><td>
%% `?GL_RGBA16I'</td><td>short</td><td>4</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>
%% A</td></tr><tr><td>`?GL_RGBA32I'</td><td>int</td><td>4</td><td>NO</td><td>R</td><td>G
%% </td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA8UI'</td><td>ubyte</td><td>4</td><td>NO
%% </td><td>R</td><td>G</td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA16UI'</td><td>ushort
%% </td><td>4</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>A</td></tr><tr><td>`?GL_RGBA32UI'
%% </td><td>uint</td><td>4</td><td>NO</td><td>R</td><td>G</td><td>B</td><td>A</td></tr></tbody>
%% </table>
%%
%%  When a buffer object is attached to a buffer texture, the buffer object's data store
%% is taken as the texture's texel array. The number of texels in the buffer texture's texel
%% array is given by  buffer_size components×sizeof( base_type/)
%%
%%  where `buffer_size' is the size of the buffer object, in basic machine units and
%% components and base type are the element count and base data type for elements, as specified
%% in the table above. The number of texels in the texel array is then clamped to the implementation-dependent
%% limit `?GL_MAX_TEXTURE_BUFFER_SIZE'. When a buffer texture is accessed in a shader,
%% the results of a texel fetch are undefined if the specified texel coordinate is negative,
%% or greater than or equal to the clamped number of texels in the texel array. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexBuffer.xml">external</a> documentation.
-spec texBuffer(Target, Internalformat, Buffer) -> 'ok' when Target :: enum(),Internalformat :: enum(),Buffer :: integer().
texBuffer(Target,Internalformat,Buffer) ->
  cast(5580, <<Target:?GLenum,Internalformat:?GLenum,Buffer:?GLuint>>).

%% @doc Specify the primitive restart index
%%
%% ``gl:primitiveRestartIndex'' specifies a vertex array element that is treated specially
%% when primitive restarting is enabled. This is known as the primitive restart index. 
%%
%%  When one of the `Draw*' commands transfers a set of generic attribute array elements
%% to the GL, if the index within the vertex arrays corresponding to that set is equal to
%% the primitive restart index, then the GL does not process those elements as a vertex.
%% Instead, it is as if the drawing command ended with the immediately preceding transfer,
%% and another drawing command is immediately started with the same parameters, but only
%% transferring the immediately following element through the end of the originally specified
%% elements. 
%%
%%  When either  {@link gl:drawElementsBaseVertex/5} ,  {@link gl:drawElementsInstancedBaseVertex/6} 
%%  or see `glMultiDrawElementsBaseVertex' is used, the primitive restart comparison
%% occurs before the basevertex offset is added to the array index. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPrimitiveRestartIndex.xml">external</a> documentation.
-spec primitiveRestartIndex(Index) -> 'ok' when Index :: integer().
primitiveRestartIndex(Index) ->
  cast(5581, <<Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getInteger64i_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getInteger64i_v(Target,Index) ->
  call(5582, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetBufferParameteri64v
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameteri64v.xml">external</a> documentation.
-spec getBufferParameteri64v(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameteri64v(Target,Pname) ->
  call(5583, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Attach a level of a texture object as a logical buffer to the currently bound framebuffer object
%%
%% ``gl:framebufferTexture'', ``gl:framebufferTexture1D'', ``gl:framebufferTexture2D'',
%% and ``gl:framebufferTexture'' attach a selected mipmap level or image of a texture object
%% as one of the logical buffers of the framebuffer object currently bound to  `Target' .
%%  `Target'  must be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER', or `?GL_FRAMEBUFFER'
%% . `?GL_FRAMEBUFFER' is equivalent to `?GL_DRAW_FRAMEBUFFER'. 
%%
%%  `Attachment'  specifies the logical attachment of the framebuffer and must be `?GL_COLOR_ATTACHMENT'
%% `i', `?GL_DEPTH_ATTACHMENT', `?GL_STENCIL_ATTACHMENT' or `?GL_DEPTH_STENCIL_ATTACHMMENT'
%% . `i' in `?GL_COLOR_ATTACHMENT'`i' may range from zero to the value of `?GL_MAX_COLOR_ATTACHMENTS'
%%  - 1. Attaching a level of a texture to `?GL_DEPTH_STENCIL_ATTACHMENT' is equivalent
%% to attaching that level to both the `?GL_DEPTH_ATTACHMENT'`and' the `?GL_STENCIL_ATTACHMENT'
%%  attachment points simultaneously. 
%%
%%  `Textarget'  specifies what type of texture is named by  `Texture' , and for cube
%% map textures, specifies the face that is to be attached. If  `Texture'  is not zero,
%% it must be the name of an existing texture with type  `Textarget' , unless it is a
%% cube map texture, in which case  `Textarget'  must be `?GL_TEXTURE_CUBE_MAP_POSITIVE_X'
%% `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Y', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y'
%% , `?GL_TEXTURE_CUBE_MAP_POSITIVE_Z', or `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z'. 
%%
%%  If  `Texture'  is non-zero, the specified  `Level'  of the texture object named  `Texture' 
%%  is attached to the framebfufer attachment point named by  `Attachment' . For ``gl:framebufferTexture1D''
%% , ``gl:framebufferTexture2D'', and ``gl:framebufferTexture3D'',  `Texture'  must
%% be zero or the name of an existing texture with a target of  `Textarget' , or  `Texture' 
%%  must be the name of an existing cube-map texture and  `Textarget'  must be one of `?GL_TEXTURE_CUBE_MAP_POSITIVE_X'
%% , `?GL_TEXTURE_CUBE_MAP_POSITIVE_Y', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Z', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X'
%% , `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y', or `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z'. 
%%
%%  If  `Textarget'  is `?GL_TEXTURE_RECTANGLE', `?GL_TEXTURE_2D_MULTISAMPLE',
%% or `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY', then  `Level'  must be zero. If  `Textarget' 
%%  is `?GL_TEXTURE_3D', then level must be greater than or equal to zero and less than
%% or equal to log2 of the value of `?GL_MAX_3D_TEXTURE_SIZE'. If  `Textarget'  is
%% one of `?GL_TEXTURE_CUBE_MAP_POSITIVE_X', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Y', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Z'
%% , `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y', or `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z'
%% , then  `Level'  must be greater than or equal to zero and less than or equal to log2
%% of the value of `?GL_MAX_CUBE_MAP_TEXTURE_SIZE'. For all other values of  `Textarget' 
%% ,  `Level'  must be greater than or equal to zero and no larger than log2 of the value
%% of `?GL_MAX_TEXTURE_SIZE'. 
%%
%%  `Layer'  specifies the layer of a 2-dimensional image within a 3-dimensional texture.
%% 
%%
%%  For ``gl:framebufferTexture1D'', if  `Texture'  is not zero, then  `Textarget' 
%% must be `?GL_TEXTURE_1D'. For ``gl:framebufferTexture2D'', if  `Texture'  is
%% not zero,  `Textarget'  must be one of `?GL_TEXTURE_2D', `?GL_TEXTURE_RECTANGLE'
%% , `?GL_TEXTURE_CUBE_MAP_POSITIVE_X', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Y', `?GL_TEXTURE_CUBE_MAP_POSITIVE_Z'
%% , `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y', `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z'
%% , or `?GL_TEXTURE_2D_MULTISAMPLE'. For ``gl:framebufferTexture3D'', if  `Texture' 
%%  is not zero, then  `Textarget'  must be `?GL_TEXTURE_3D'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferTexture.xml">external</a> documentation.
-spec framebufferTexture(Target, Attachment, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture(Target,Attachment,Texture,Level) ->
  cast(5584, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc Modify the rate at which generic vertex attributes advance during instanced rendering
%%
%% ``gl:vertexAttribDivisor'' modifies the rate at which generic vertex attributes advance
%% when rendering multiple instances of primitives in a single draw call. If  `Divisor' 
%% is zero, the attribute at slot  `Index'  advances once per vertex. If  `Divisor' 
%% is non-zero, the attribute advances once per  `Divisor'  instances of the set(s) of
%% vertices being rendered. An attribute is referred to as instanced if its `?GL_VERTEX_ATTRIB_ARRAY_DIVISOR'
%%  value is non-zero. 
%%
%%  `Index'  must be less than the value of `?GL_MAX_VERTEX_ATTRIBUTES'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribDivisor.xml">external</a> documentation.
-spec vertexAttribDivisor(Index, Divisor) -> 'ok' when Index :: integer(),Divisor :: integer().
vertexAttribDivisor(Index,Divisor) ->
  cast(5585, <<Index:?GLuint,Divisor:?GLuint>>).

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
%%  A  `Value'  of 1.0 indicates that each sample in the framebuffer should be indpendently
%% shaded. A  `Value'  of 0.0 effectively allows the GL to ignore sample rate shading.
%% Any value between 0.0 and 1.0 allows the GL to shade only a subset of the total samples
%% within each covered fragment. Which samples are shaded and the algorithm used to select
%% that subset of the fragment's samples is implementation dependent. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMinSampleShading.xml">external</a> documentation.
-spec minSampleShading(Value) -> 'ok' when Value :: clamp().
minSampleShading(Value) ->
  cast(5586, <<Value:?GLclampf>>).

%% @doc 
%% See {@link blendEquation/1}
-spec blendEquationi(Buf, Mode) -> 'ok' when Buf :: integer(),Mode :: enum().
blendEquationi(Buf,Mode) ->
  cast(5587, <<Buf:?GLuint,Mode:?GLenum>>).

%% @doc 
%% See {@link blendEquationSeparate/2}
-spec blendEquationSeparatei(Buf, ModeRGB, ModeAlpha) -> 'ok' when Buf :: integer(),ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) ->
  cast(5588, <<Buf:?GLuint,ModeRGB:?GLenum,ModeAlpha:?GLenum>>).

%% @doc glBlendFunci
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlendFunci.xml">external</a> documentation.
-spec blendFunci(Buf, Src, Dst) -> 'ok' when Buf :: integer(),Src :: enum(),Dst :: enum().
blendFunci(Buf,Src,Dst) ->
  cast(5589, <<Buf:?GLuint,Src:?GLenum,Dst:?GLenum>>).

%% @doc 
%% See {@link blendFuncSeparate/4}
-spec blendFuncSeparatei(Buf, SrcRGB, DstRGB, SrcAlpha, DstAlpha) -> 'ok' when Buf :: integer(),SrcRGB :: enum(),DstRGB :: enum(),SrcAlpha :: enum(),DstAlpha :: enum().
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) ->
  cast(5590, <<Buf:?GLuint,SrcRGB:?GLenum,DstRGB:?GLenum,SrcAlpha:?GLenum,DstAlpha:?GLenum>>).

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
-spec loadTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5591, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
loadTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5591, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc glLoadTransposeMatrixARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLoadTransposeMatrixARB.xml">external</a> documentation.
-spec loadTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5592, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
loadTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5592, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
-spec multTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5593, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,M13:?GLfloat,M14:?GLfloat,M15:?GLfloat,M16:?GLfloat>>);
multTransposeMatrixfARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5593, <<M1:?GLfloat,M2:?GLfloat,M3:?GLfloat,0:?GLfloat,M4:?GLfloat,M5:?GLfloat,M6:?GLfloat,0:?GLfloat,M7:?GLfloat,M8:?GLfloat,M9:?GLfloat,0:?GLfloat,M10:?GLfloat,M11:?GLfloat,M12:?GLfloat,1:?GLfloat>>).

%% @doc glMultTransposeMatrixARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMultTransposeMatrixARB.xml">external</a> documentation.
-spec multTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16}) ->
  cast(5594, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble>>);
multTransposeMatrixdARB({M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12}) ->
  cast(5594, <<M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,0:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,0:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,0:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,1:?GLdouble>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightbvARB(Weights) -> 'ok' when Weights :: [integer()].
weightbvARB(Weights) ->
  cast(5595, <<(length(Weights)):?GLuint,
        (<< <<C:?GLbyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightsvARB(Weights) -> 'ok' when Weights :: [integer()].
weightsvARB(Weights) ->
  cast(5596, <<(length(Weights)):?GLuint,
        (<< <<C:?GLshort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightivARB(Weights) -> 'ok' when Weights :: [integer()].
weightivARB(Weights) ->
  cast(5597, <<(length(Weights)):?GLuint,
        (<< <<C:?GLint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightfvARB(Weights) -> 'ok' when Weights :: [float()].
weightfvARB(Weights) ->
  cast(5598, <<(length(Weights)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightdvARB(Weights) -> 'ok' when Weights :: [float()].
weightdvARB(Weights) ->
  cast(5599, <<(length(Weights)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Weights>>)/binary>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightubvARB(Weights) -> 'ok' when Weights :: [integer()].
weightubvARB(Weights) ->
  cast(5600, <<(length(Weights)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Weights>>)/binary,0:((8-((length(Weights)+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightusvARB(Weights) -> 'ok' when Weights :: [integer()].
weightusvARB(Weights) ->
  cast(5601, <<(length(Weights)):?GLuint,
        (<< <<C:?GLushort>> || C <- Weights>>)/binary,0:((8-((length(Weights)*2+ 4) rem 8)) rem 8)>>).

%% @doc glWeightARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWeightARB.xml">external</a> documentation.
-spec weightuivARB(Weights) -> 'ok' when Weights :: [integer()].
weightuivARB(Weights) ->
  cast(5602, <<(length(Weights)):?GLuint,
        (<< <<C:?GLuint>> || C <- Weights>>)/binary,0:(((1+length(Weights)) rem 2)*32)>>).

%% @doc glVertexBlenARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexBlenARB.xml">external</a> documentation.
-spec vertexBlendARB(Count) -> 'ok' when Count :: integer().
vertexBlendARB(Count) ->
  cast(5603, <<Count:?GLint>>).

%% @doc glCurrentPaletteMatrixARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCurrentPaletteMatrixARB.xml">external</a> documentation.
-spec currentPaletteMatrixARB(Index) -> 'ok' when Index :: integer().
currentPaletteMatrixARB(Index) ->
  cast(5604, <<Index:?GLint>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexubvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexubvARB(Indices) ->
  cast(5605, <<(length(Indices)):?GLuint,
        (<< <<C:?GLubyte>> || C <- Indices>>)/binary,0:((8-((length(Indices)+ 4) rem 8)) rem 8)>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexusvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexusvARB(Indices) ->
  cast(5606, <<(length(Indices)):?GLuint,
        (<< <<C:?GLushort>> || C <- Indices>>)/binary,0:((8-((length(Indices)*2+ 4) rem 8)) rem 8)>>).

%% @doc glMatrixIndexARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMatrixIndexARB.xml">external</a> documentation.
-spec matrixIndexuivARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexuivARB(Indices) ->
  cast(5607, <<(length(Indices)):?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((1+length(Indices)) rem 2)*32)>>).

%% @doc glProgramStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramStringARB.xml">external</a> documentation.
-spec programStringARB(Target, Format, String) -> 'ok' when Target :: enum(),Format :: enum(),String :: string().
programStringARB(Target,Format,String) ->
  cast(5608, <<Target:?GLenum,Format:?GLenum,(list_to_binary([String|[0]]))/binary,0:((8-((length(String)+ 1) rem 8)) rem 8)>>).

%% @doc glBindProgramARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindProgramARB.xml">external</a> documentation.
-spec bindProgramARB(Target, Program) -> 'ok' when Target :: enum(),Program :: integer().
bindProgramARB(Target,Program) ->
  cast(5609, <<Target:?GLenum,Program:?GLuint>>).

%% @doc glDeleteProgramsARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgramsARB.xml">external</a> documentation.
-spec deleteProgramsARB(Programs) -> 'ok' when Programs :: [integer()].
deleteProgramsARB(Programs) ->
  cast(5610, <<(length(Programs)):?GLuint,
        (<< <<C:?GLuint>> || C <- Programs>>)/binary,0:(((1+length(Programs)) rem 2)*32)>>).

%% @doc glGenProgramsARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenProgramsARB.xml">external</a> documentation.
-spec genProgramsARB(N) -> [integer()] when N :: integer().
genProgramsARB(N) ->
  call(5611, <<N:?GLsizei>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5612, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5613, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5614, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc glProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramEnvParameterARB.xml">external</a> documentation.
-spec programEnvParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5615, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4dARB(Target,Index,X,Y,Z,W) ->
  cast(5616, <<Target:?GLenum,Index:?GLuint,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4dvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5617, <<Target:?GLenum,Index:?GLuint,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4fARB(Target,Index,X,Y,Z,W) ->
  cast(5618, <<Target:?GLenum,Index:?GLuint,X:?GLfloat,Y:?GLfloat,Z:?GLfloat,W:?GLfloat>>).

%% @doc glProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramLocalParameterARB.xml">external</a> documentation.
-spec programLocalParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4fvARB(Target,Index,{P1,P2,P3,P4}) ->
  cast(5619, <<Target:?GLenum,Index:?GLuint,P1:?GLfloat,P2:?GLfloat,P3:?GLfloat,P4:?GLfloat>>).

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
-spec getProgramEnvParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterdvARB(Target,Index) ->
  call(5620, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramEnvParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramEnvParameterARB.xml">external</a> documentation.
-spec getProgramEnvParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterfvARB(Target,Index) ->
  call(5621, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
-spec getProgramLocalParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterdvARB(Target,Index) ->
  call(5622, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramLocalParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramLocalParameterARB.xml">external</a> documentation.
-spec getProgramLocalParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterfvARB(Target,Index) ->
  call(5623, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glGetProgramStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramStringARB.xml">external</a> documentation.
-spec getProgramStringARB(Target, Pname, String) -> 'ok' when Target :: enum(),Pname :: enum(),String :: mem().
getProgramStringARB(Target,Pname,String) ->
  send_bin(String),
  call(5624, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glGetBufferParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetBufferParameterARB.xml">external</a> documentation.
-spec getBufferParameterivARB(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameterivARB(Target,Pname) ->
  call(5625, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc glDeleteObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteObjectARB.xml">external</a> documentation.
-spec deleteObjectARB(Obj) -> 'ok' when Obj :: integer().
deleteObjectARB(Obj) ->
  cast(5626, <<Obj:?GLhandleARB>>).

%% @doc glGetHandleARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetHandleARB.xml">external</a> documentation.
-spec getHandleARB(Pname) -> integer() when Pname :: enum().
getHandleARB(Pname) ->
  call(5627, <<Pname:?GLenum>>).

%% @doc glDetachObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDetachObjectARB.xml">external</a> documentation.
-spec detachObjectARB(ContainerObj, AttachedObj) -> 'ok' when ContainerObj :: integer(),AttachedObj :: integer().
detachObjectARB(ContainerObj,AttachedObj) ->
  cast(5628, <<ContainerObj:?GLhandleARB,AttachedObj:?GLhandleARB>>).

%% @doc glCreateShaderObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShaderObjectARB.xml">external</a> documentation.
-spec createShaderObjectARB(ShaderType) -> integer() when ShaderType :: enum().
createShaderObjectARB(ShaderType) ->
  call(5629, <<ShaderType:?GLenum>>).

%% @doc glShaderSourceARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderSourceARB.xml">external</a> documentation.
-spec shaderSourceARB(ShaderObj, String) -> 'ok' when ShaderObj :: integer(),String :: iolist().
shaderSourceARB(ShaderObj,String) ->
 StringTemp = list_to_binary([[Str|[0]] || Str <- String ]),
  cast(5630, <<ShaderObj:?GLhandleARB,(length(String)):?GLuint,(size(StringTemp)):?GLuint,(StringTemp)/binary,0:((8-((size(StringTemp)+4) rem 8)) rem 8)>>).

%% @doc glCompileShaderARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShaderARB.xml">external</a> documentation.
-spec compileShaderARB(ShaderObj) -> 'ok' when ShaderObj :: integer().
compileShaderARB(ShaderObj) ->
  cast(5631, <<ShaderObj:?GLhandleARB>>).

%% @doc glCreateProgramObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateProgramObjectARB.xml">external</a> documentation.
-spec createProgramObjectARB() -> integer().
createProgramObjectARB() ->
  call(5632, <<>>).

%% @doc glAttachObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glAttachObjectARB.xml">external</a> documentation.
-spec attachObjectARB(ContainerObj, Obj) -> 'ok' when ContainerObj :: integer(),Obj :: integer().
attachObjectARB(ContainerObj,Obj) ->
  cast(5633, <<ContainerObj:?GLhandleARB,Obj:?GLhandleARB>>).

%% @doc glLinkProgramARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glLinkProgramARB.xml">external</a> documentation.
-spec linkProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
linkProgramARB(ProgramObj) ->
  cast(5634, <<ProgramObj:?GLhandleARB>>).

%% @doc glUseProgramObjectARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgramObjectARB.xml">external</a> documentation.
-spec useProgramObjectARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
useProgramObjectARB(ProgramObj) ->
  cast(5635, <<ProgramObj:?GLhandleARB>>).

%% @doc glValidateProgramARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgramARB.xml">external</a> documentation.
-spec validateProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
validateProgramARB(ProgramObj) ->
  cast(5636, <<ProgramObj:?GLhandleARB>>).

%% @doc glGetObjectParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
-spec getObjectParameterfvARB(Obj, Pname) -> float() when Obj :: integer(),Pname :: enum().
getObjectParameterfvARB(Obj,Pname) ->
  call(5637, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @doc glGetObjectParameterARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetObjectParameterARB.xml">external</a> documentation.
-spec getObjectParameterivARB(Obj, Pname) -> integer() when Obj :: integer(),Pname :: enum().
getObjectParameterivARB(Obj,Pname) ->
  call(5638, <<Obj:?GLhandleARB,Pname:?GLenum>>).

%% @doc glGetInfoLogARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInfoLogARB.xml">external</a> documentation.
-spec getInfoLogARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getInfoLogARB(Obj,MaxLength) ->
  call(5639, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @doc glGetAttachedObjectsARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttachedObjectsARB.xml">external</a> documentation.
-spec getAttachedObjectsARB(ContainerObj, MaxCount) -> [integer()] when ContainerObj :: integer(),MaxCount :: integer().
getAttachedObjectsARB(ContainerObj,MaxCount) ->
  call(5640, <<ContainerObj:?GLhandleARB,MaxCount:?GLsizei>>).

%% @doc glGetUniformLocationARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformLocationARB.xml">external</a> documentation.
-spec getUniformLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getUniformLocationARB(ProgramObj,Name) ->
  call(5641, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc glGetActiveUniformARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformARB.xml">external</a> documentation.
-spec getActiveUniformARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveUniformARB(ProgramObj,Index,MaxLength) ->
  call(5642, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @doc glGetUniformARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
-spec getUniformfvARB(ProgramObj, Location) -> matrix() when ProgramObj :: integer(),Location :: integer().
getUniformfvARB(ProgramObj,Location) ->
  call(5643, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @doc glGetUniformARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformARB.xml">external</a> documentation.
-spec getUniformivARB(ProgramObj, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when ProgramObj :: integer(),Location :: integer().
getUniformivARB(ProgramObj,Location) ->
  call(5644, <<ProgramObj:?GLhandleARB,Location:?GLint>>).

%% @doc glGetShaderSourceARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderSourceARB.xml">external</a> documentation.
-spec getShaderSourceARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getShaderSourceARB(Obj,MaxLength) ->
  call(5645, <<Obj:?GLhandleARB,MaxLength:?GLsizei>>).

%% @doc glBindAttribLocationARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindAttribLocationARB.xml">external</a> documentation.
-spec bindAttribLocationARB(ProgramObj, Index, Name) -> 'ok' when ProgramObj :: integer(),Index :: integer(),Name :: string().
bindAttribLocationARB(ProgramObj,Index,Name) ->
  cast(5646, <<ProgramObj:?GLhandleARB,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @doc glGetActiveAttribARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveAttribARB.xml">external</a> documentation.
-spec getActiveAttribARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveAttribARB(ProgramObj,Index,MaxLength) ->
  call(5647, <<ProgramObj:?GLhandleARB,Index:?GLuint,MaxLength:?GLsizei>>).

%% @doc glGetAttribLocationARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetAttribLocationARB.xml">external</a> documentation.
-spec getAttribLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getAttribLocationARB(ProgramObj,Name) ->
  call(5648, <<ProgramObj:?GLhandleARB,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsRenderbuffer.xml">external</a> documentation.
-spec isRenderbuffer(Renderbuffer) -> 0|1 when Renderbuffer :: integer().
isRenderbuffer(Renderbuffer) ->
  call(5649, <<Renderbuffer:?GLuint>>).

%% @doc Bind a renderbuffer to a renderbuffer target
%%
%% ``gl:bindRenderbuffer'' binds the renderbuffer object with name  `Renderbuffer' 
%% to the renderbuffer target specified by  `Target' .  `Target'  must be `?GL_RENDERBUFFER'
%% .  `Renderbuffer'  is the name of a renderbuffer object previously returned from a
%% call to  {@link gl:genRenderbuffers/1} , or zero to break the existing binding of a renderbuffer
%% object to  `Target' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindRenderbuffer.xml">external</a> documentation.
-spec bindRenderbuffer(Target, Renderbuffer) -> 'ok' when Target :: enum(),Renderbuffer :: integer().
bindRenderbuffer(Target,Renderbuffer) ->
  cast(5650, <<Target:?GLenum,Renderbuffer:?GLuint>>).

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
%%  If a renderbuffer object is attached to one or more attachment points in the currently
%% bound framebuffer, then it as if  {@link gl:framebufferRenderbuffer/4}  had been called,
%% with a  `Renderbuffer'  of zero for each attachment point to which this image was attached
%% in the currently bound framebuffer. In other words, this renderbuffer object is first
%% detached from all attachment ponits in the currently bound framebuffer. Note that the
%% renderbuffer image is specifically `not' detached from any non-bound framebuffers. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteRenderbuffers.xml">external</a> documentation.
-spec deleteRenderbuffers(Renderbuffers) -> 'ok' when Renderbuffers :: [integer()].
deleteRenderbuffers(Renderbuffers) ->
  cast(5651, <<(length(Renderbuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Renderbuffers>>)/binary,0:(((1+length(Renderbuffers)) rem 2)*32)>>).

%% @doc Generate renderbuffer object names
%%
%% ``gl:genRenderbuffers'' returns  `N'  renderbuffer object names in  `Renderbuffers' 
%% . There is no guarantee that the names form a contiguous set of integers; however, it
%% is guaranteed that none of the returned names was in use immediately before the call to ``gl:genRenderbuffers''
%% . 
%%
%%  Renderbuffer object names returned by a call to ``gl:genRenderbuffers'' are not returned
%% by subsequent calls, unless they are first deleted with  {@link gl:deleteRenderbuffers/1} . 
%%
%%  The names returned in  `Renderbuffers'  are marked as used, for the purposes of ``gl:genRenderbuffers''
%%  only, but they acquire state and type only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenRenderbuffers.xml">external</a> documentation.
-spec genRenderbuffers(N) -> [integer()] when N :: integer().
genRenderbuffers(N) ->
  call(5652, <<N:?GLsizei>>).

%% @doc Establish data storage, format and dimensions of a renderbuffer object's image
%%
%% ``gl:renderbufferStorage'' is equivalent to calling  {@link gl:renderbufferStorageMultisample/5} 
%%  with the  `Samples'  set to zero. 
%%
%%  The target of the operation, specified by  `Target'  must be `?GL_RENDERBUFFER'.
%%  `Internalformat'  specifies the internal format to be used for the renderbuffer object's
%% storage and must be a color-renderable, depth-renderable, or stencil-renderable format.  `Width' 
%%  and  `Height'  are the dimensions, in pixels, of the renderbuffer. Both  `Width' 
%% and  `Height'  must be less than or equal to the value of `?GL_MAX_RENDERBUFFER_SIZE'
%% . 
%%
%%  Upon success, ``gl:renderbufferStorage'' deletes any existing data store for the renderbuffer
%% image and the contents of the data store after calling ``gl:renderbufferStorage'' are
%% undefined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorage.xml">external</a> documentation.
-spec renderbufferStorage(Target, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorage(Target,Internalformat,Width,Height) ->
  cast(5653, <<Target:?GLenum,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

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
%%  Upon a successful return from ``gl:getRenderbufferParameteriv'', if  `Pname'  is `?GL_RENDERBUFFER_WIDTH'
%% , `?GL_RENDERBUFFER_HEIGHT', `?GL_RENDERBUFFER_INTERNAL_FORMAT', or `?GL_RENDERBUFFER_SAMPLES'
%% , then  `Params'  will contain the width in pixels, the height in pixels, the internal
%% format, or the number of samples, respectively, of the image of the renderbuffer currently
%% bound to  `Target' . 
%%
%%  If  `Pname'  is `?GL_RENDERBUFFER_RED_SIZE', `?GL_RENDERBUFFER_GREEN_SIZE',
%% `?GL_RENDERBUFFER_BLUE_SIZE', `?GL_RENDERBUFFER_ALPHA_SIZE', `?GL_RENDERBUFFER_DEPTH_SIZE'
%% , or `?GL_RENDERBUFFER_STENCIL_SIZE', then  `Params'  will contain the actual
%% resolutions (not the resolutions specified when the image array was defined) for the red,
%% green, blue, alpha depth, or stencil components, respectively, of the image of the renderbuffer
%% currently bound to  `Target' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetRenderbufferParameter.xml">external</a> documentation.
-spec getRenderbufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getRenderbufferParameteriv(Target,Pname) ->
  call(5654, <<Target:?GLenum,Pname:?GLenum>>).

%% @doc Determine if a name corresponds to a framebuffer object
%%
%% ``gl:isFramebuffer'' returns `?GL_TRUE' if  `Framebuffer'  is currently the
%% name of a framebuffer object. If  `Framebuffer'  is zero, or if `?framebuffer'
%% is not the name of a framebuffer object, or if an error occurs, ``gl:isFramebuffer''
%% returns `?GL_FALSE'. If  `Framebuffer'  is a name returned by  {@link gl:genFramebuffers/1} 
%% , by that has not yet been bound through a call to  {@link gl:bindFramebuffer/2} , then the
%% name is not a framebuffer object and ``gl:isFramebuffer'' returns `?GL_FALSE'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsFramebuffer.xml">external</a> documentation.
-spec isFramebuffer(Framebuffer) -> 0|1 when Framebuffer :: integer().
isFramebuffer(Framebuffer) ->
  call(5655, <<Framebuffer:?GLuint>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFramebuffer.xml">external</a> documentation.
-spec bindFramebuffer(Target, Framebuffer) -> 'ok' when Target :: enum(),Framebuffer :: integer().
bindFramebuffer(Target,Framebuffer) ->
  cast(5656, <<Target:?GLenum,Framebuffer:?GLuint>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteFramebuffers.xml">external</a> documentation.
-spec deleteFramebuffers(Framebuffers) -> 'ok' when Framebuffers :: [integer()].
deleteFramebuffers(Framebuffers) ->
  cast(5657, <<(length(Framebuffers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Framebuffers>>)/binary,0:(((1+length(Framebuffers)) rem 2)*32)>>).

%% @doc Generate framebuffer object names
%%
%% ``gl:genFramebuffers'' returns  `N'  framebuffer object names in  `Ids' . There
%% is no guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genFramebuffers''
%% . 
%%
%%  Framebuffer object names returned by a call to ``gl:genFramebuffers'' are not returned
%% by subsequent calls, unless they are first deleted with  {@link gl:deleteFramebuffers/1} . 
%%
%%  The names returned in  `Ids'  are marked as used, for the purposes of ``gl:genFramebuffers''
%%  only, but they acquire state and type only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenFramebuffers.xml">external</a> documentation.
-spec genFramebuffers(N) -> [integer()] when N :: integer().
genFramebuffers(N) ->
  call(5658, <<N:?GLsizei>>).

%% @doc Check the completeness status of a framebuffer
%%
%% ``gl:checkFramebufferStatus'' queries the completeness status of the framebuffer object
%% currently bound to  `Target' .  `Target'  must be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER'
%%  or `?GL_FRAMEBUFFER'. `?GL_FRAMEBUFFER' is equivalent to `?GL_DRAW_FRAMEBUFFER'
%% . 
%%
%%  The return value is `?GL_FRAMEBUFFER_COMPLETE' if the framebuffer bound to  `Target' 
%%  is complete. Otherwise, the return value is determined as follows: 
%%
%% `?GL_FRAMEBUFFER_UNDEFINED' is returned if  `Target'  is the default framebuffer,
%% but the default framebuffer does not exist. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT' is returned if any of the framebuffer attachment
%% points are framebuffer incomplete. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT' is returned if the framebuffer does
%% not have at least one image attached to it. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER' is returned if the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE'
%%  is `?GL_NONE' for any color attachment point(s) named by `?GL_DRAWBUFFERi'. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER' is returned if `?GL_READ_BUFFER' is
%% not `?GL_NONE' and the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' is `?GL_NONE'
%%  for the color attachment point named by `?GL_READ_BUFFER'. 
%%
%% `?GL_FRAMEBUFFER_UNSUPPORTED' is returned if the combination of internal formats
%% of the attached images violates an implementation-dependent set of restrictions. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE' is returned if the value of `?GL_RENDERBUFFER_SAMPLES'
%%  is not the same for all attached renderbuffers; if the value of `?GL_TEXTURE_SAMPLES'
%%  is the not same for all attached textures; or, if the attached images are a mix of renderbuffers
%% and textures, the value of `?GL_RENDERBUFFER_SAMPLES' does not match the value of `?GL_TEXTURE_SAMPLES'
%% . 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE' is also returned if the value of `?GL_TEXTURE_FIXED_SAMPLE_LOCATIONS'
%%  is not the same for all attached textures; or, if the attached images are a mix of renderbuffers
%% and textures, the value of `?GL_TEXTURE_FIXED_SAMPLE_LOCATIONS' is not `?GL_TRUE'
%%  for all attached textures. 
%%
%% `?GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS' is returned if any framebuffer attachment
%% is layered, and any populated attachment is not layered, or if all populated color attachments
%% are not from textures of the same target. 
%%
%%  Additionally, if an error occurs, zero is returned. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCheckFramebufferStatus.xml">external</a> documentation.
-spec checkFramebufferStatus(Target) -> enum() when Target :: enum().
checkFramebufferStatus(Target) ->
  call(5659, <<Target:?GLenum>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture1D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5660, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture2D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) ->
  cast(5661, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer(),Zoffset :: integer().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) ->
  cast(5662, <<Target:?GLenum,Attachment:?GLenum,Textarget:?GLenum,Texture:?GLuint,Level:?GLint,Zoffset:?GLint>>).

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
%%  The value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' for the specified attachment
%% point is set to `?GL_RENDERBUFFER' and the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME'
%%  is set to  `Renderbuffer' . All other state values of the attachment point specified
%% by  `Attachment'  are set to their default values. No change is made to the state of
%% the renderbuuffer object and any previous attachment to the  `Attachment'  logical
%% buffer of the framebuffer  `Target'  is broken. 
%%
%%  Calling ``gl:framebufferRenderbuffer'' with the renderbuffer name zero will detach
%% the image, if any, identified by  `Attachment' , in the framebuffer currently bound
%% to  `Target' . All state values of the attachment point specified by attachment in
%% the object bound to target are set to their default values. 
%%
%%  Setting  `Attachment'  to the value `?GL_DEPTH_STENCIL_ATTACHMENT' is a special
%% case causing both the depth and stencil attachments of the framebuffer object to be set
%% to  `Renderbuffer' , which should have the base internal format `?GL_DEPTH_STENCIL'
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFramebufferRenderbuffer.xml">external</a> documentation.
-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok' when Target :: enum(),Attachment :: enum(),Renderbuffertarget :: enum(),Renderbuffer :: integer().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) ->
  cast(5663, <<Target:?GLenum,Attachment:?GLenum,Renderbuffertarget:?GLenum,Renderbuffer:?GLuint>>).

%% @doc Retrieve information about attachments of a bound framebuffer object
%%
%% ``gl:getFramebufferAttachmentParameter'' returns information about attachments of a
%% bound framebuffer object.  `Target'  specifies the framebuffer binding point and must
%% be `?GL_DRAW_FRAMEBUFFER', `?GL_READ_FRAMEBUFFER' or `?GL_FRAMEBUFFER'. `?GL_FRAMEBUFFER'
%%  is equivalent to `?GL_DRAW_FRAMEBUFFER'. 
%%
%%  If the default framebuffer is bound to  `Target'  then  `Attachment'  must be one
%% of `?GL_FRONT_LEFT', `?GL_FRONT_RIGHT', `?GL_BACK_LEFT', or `?GL_BACK_RIGHT'
%% , identifying a color buffer, `?GL_DEPTH', identifying the depth buffer, or `?GL_STENCIL'
%% , identifying the stencil buffer. 
%%
%%  If a framebuffer object is bound, then  `Attachment'  must be one of `?GL_COLOR_ATTACHMENT'
%% `i', `?GL_DEPTH_ATTACHMENT', `?GL_STENCIL_ATTACHMENT', or `?GL_DEPTH_STENCIL_ATTACHMENT'
%% . `i' in `?GL_COLOR_ATTACHMENT'`i' must be in the range zero to the value
%% of `?GL_MAX_COLOR_ATTACHMENTS' - 1. 
%%
%%  If  `Attachment'  is `?GL_DEPTH_STENCIL_ATTACHMENT' and different objects are
%% bound to the depth and stencil attachment points of  `Target'  the query will fail.
%% If the same object is bound to both attachment points, information about that object will
%% be returned. 
%%
%%  Upon successful return from ``gl:getFramebufferAttachmentParameteriv'', if  `Pname' 
%% is `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE', then  `Params'  will contain one of `?GL_NONE'
%% , `?GL_FRAMEBUFFER_DEFAULT', `?GL_TEXTURE', or `?GL_RENDERBUFFER', identifying
%% the type of object which contains the attached image. Other values accepted for  `Pname' 
%%  depend on the type of object, as described below. 
%%
%%  If the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' is `?GL_NONE', no
%% framebuffer is bound to  `Target' . In this case querying  `Pname' `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME'
%%  will return zero, and all other queries will generate an error. 
%%
%%  If the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' is not `?GL_NONE',
%% these queries apply to all other framebuffer types: 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE', `?GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE'
%% , `?GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE', `?GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE'
%% , `?GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE', or `?GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE'
%% , then  `Params'  will contain the number of bits in the corresponding red, green,
%% blue, alpha, depth, or stencil component of the specified attachment. Zero is returned
%% if the requested component is not present in  `Attachment' . 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE',  `Params'  will
%% contain the format of components of the specified attachment, one of `?GL_FLOAT',  `GL_INT' 
%% ,  `GL_UNSIGNED_INT' ,  `GL_SIGNED_NORMALIZED' , or  `GL_UNSIGNED_NORMALIZED' 
%% for floating-point, signed integer, unsigned integer, signed normalized fixed-point, or
%% unsigned normalized fixed-point components respectively. Only color buffers may have integer
%% components. 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING',  `Param'  will
%% contain the encoding of components of the specified attachment, one of `?GL_LINEAR'
%% or `?GL_SRGB' for linear or sRGB-encoded components, respectively. Only color buffer
%% components may be sRGB-encoded; such components are treated as described in sections 4.1.7
%% and 4.1.8. For the default framebuffer, color encoding is determined by the implementation.
%% For framebuffer objects, components are sRGB-encoded if the internal format of a color
%% attachment is one of the color-renderable SRGB formats. 
%%
%%  If the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' is `?GL_RENDERBUFFER',
%% then: 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME',  `Params'  will
%% contain the name of the renderbuffer object which contains the attached image. 
%%
%%  If the value of `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE' is `?GL_TEXTURE',
%% then: 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME', then  `Params' 
%% will contain the name of the texture object which contains the attached image. 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL', then  `Params' 
%% will contain the mipmap level of the texture object which contains the attached image. 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE' and the texture
%% object named `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME' is a cube map texture, then  `Params' 
%%  will contain the cube map face of the cubemap texture object which contains the attached
%% image. Otherwise  `Params'  will contain the value zero. 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER' and the texture object
%% named `?GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME' is a layer of a three-dimensional
%% texture or a one-or two-dimensional array texture, then  `Params'  will contain the
%% number of the texture layer which contains the attached image. Otherwise  `Params' 
%% will contain the value zero. 
%%
%%  If  `Pname'  is `?GL_FRAMEBUFFER_ATTACHMENT_LAYERED', then  `Params'  will
%% contain `?GL_TRUE' if an entire level of a three-dimesional texture, cube map texture,
%% or one-or two-dimensional array texture is attached. Otherwise,  `Params'  will contain
%% `?GL_FALSE'. 
%%
%%  Any combinations of framebuffer type and  `Pname'  not described above will generate
%% an error. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFramebufferAttachmentParameter.xml">external</a> documentation.
-spec getFramebufferAttachmentParameteriv(Target, Attachment, Pname) -> integer() when Target :: enum(),Attachment :: enum(),Pname :: enum().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) ->
  call(5664, <<Target:?GLenum,Attachment:?GLenum,Pname:?GLenum>>).

%% @doc Generate mipmaps for a specified texture target
%%
%% ``gl:generateMipmap'' generates mipmaps for the texture attached to  `Target'  of
%% the active texture unit. For cube map textures, a `?GL_INVALID_OPERATION' error is
%% generated if the texture attached to  `Target'  is not cube complete. 
%%
%%  Mipmap generation replaces texel array levels  level base+1 through  q with arrays derived
%% from the  level base array, regardless of their previous contents. All other mimap arrays,
%% including the  level base array, are left unchanged by this computation. 
%%
%%  The internal formats of the derived mipmap arrays all match those of the  level base
%% array. The contents of the derived arrays are computed by repeated, filtered reduction
%% of the  level base array. For one- and two-dimensional texture arrays, each layer is filtered
%% independently. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenerateMipmap.xml">external</a> documentation.
-spec generateMipmap(Target) -> 'ok' when Target :: enum().
generateMipmap(Target) ->
  cast(5665, <<Target:?GLenum>>).

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
%%  The actual region taken from the read framebuffer is limited to the intersection of the
%% source buffers being transferred, which may include the color buffer selected by the read
%% buffer, the depth buffer, and/or the stencil buffer depending on mask. The actual region
%% written to the draw framebuffer is limited to the intersection of the destination buffers
%% being written, which may include multiple draw buffers, the depth buffer, and/or the stencil
%% buffer depending on mask. Whether or not the source or destination regions are altered
%% due to these limits, the scaling and offset applied to pixels being transferred is performed
%% as though no such limits were present. 
%%
%%  If the sizes of the source and destination rectangles are not equal,  `Filter'  specifies
%% the interpolation method that will be applied to resize the source image , and must be `?GL_NEAREST'
%%  or `?GL_LINEAR'. `?GL_LINEAR' is only a valid interpolation method for the
%% color buffer. If  `Filter'  is not `?GL_NEAREST' and  `Mask'  includes `?GL_DEPTH_BUFFER_BIT'
%%  or `?GL_STENCIL_BUFFER_BIT', no data is transferred and a `?GL_INVALID_OPERATION'
%%  error is generated. 
%%
%%  If  `Filter'  is `?GL_LINEAR' and the source rectangle would require sampling
%% outside the bounds of the source framebuffer, values are read as if the `?GL_CLAMP_TO_EDGE'
%%  texture wrapping mode were applied. 
%%
%%  When the color buffer is transferred, values are taken from the read buffer of the read
%% framebuffer and written to each of the draw buffers of the draw framebuffer. 
%%
%%  If the source and destination rectangles overlap or are the same, and the read and draw
%% buffers are the same, the result of the operation is undefined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBlitFramebuffer.xml">external</a> documentation.
-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok' when SrcX0 :: integer(),SrcY0 :: integer(),SrcX1 :: integer(),SrcY1 :: integer(),DstX0 :: integer(),DstY0 :: integer(),DstX1 :: integer(),DstY1 :: integer(),Mask :: integer(),Filter :: enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) ->
  cast(5666, <<SrcX0:?GLint,SrcY0:?GLint,SrcX1:?GLint,SrcY1:?GLint,DstX0:?GLint,DstY0:?GLint,DstX1:?GLint,DstY1:?GLint,Mask:?GLbitfield,Filter:?GLenum>>).

%% @doc Establish data storage, format, dimensions and sample count of a renderbuffer object's image
%%
%% ``gl:renderbufferStorageMultisample'' establishes the data storage, format, dimensions
%% and number of samples of a renderbuffer object's image. 
%%
%%  The target of the operation, specified by  `Target'  must be `?GL_RENDERBUFFER'.
%%  `Internalformat'  specifies the internal format to be used for the renderbuffer object's
%% storage and must be a color-renderable, depth-renderable, or stencil-renderable format.  `Width' 
%%  and  `Height'  are the dimensions, in pixels, of the renderbuffer. Both  `Width' 
%% and  `Height'  must be less than or equal to the value of `?GL_MAX_RENDERBUFFER_SIZE'
%% .  `Samples'  specifies the number of samples to be used for the renderbuffer object's
%% image, and must be less than or equal to the value of `?GL_MAX_SAMPLES'. If  `Internalformat' 
%%  is a signed or unsigned integer format then  `Samples'  must be less than or equal
%% to the value of `?GL_MAX_INTEGER_SAMPLES'. 
%%
%%  Upon success, ``gl:renderbufferStorageMultisample'' deletes any existing data store
%% for the renderbuffer image and the contents of the data store after calling ``gl:renderbufferStorageMultisample''
%%  are undefined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glRenderbufferStorageMultisample.xml">external</a> documentation.
-spec renderbufferStorageMultisample(Target, Samples, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) ->
  cast(5667, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTextureLayer(Target, Attachment, Texture, Level, Layer) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Layer :: integer().
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) ->
  cast(5668, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Layer:?GLint>>).

%% @doc 
%% See {@link framebufferTexture/4}
-spec framebufferTextureFaceARB(Target, Attachment, Texture, Level, Face) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Face :: enum().
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) ->
  cast(5669, <<Target:?GLenum,Attachment:?GLenum,Texture:?GLuint,Level:?GLint,Face:?GLenum>>).

%% @doc Indicate modifications to a range of a mapped buffer
%%
%% ``gl:flushMappedBufferRange'' indicates that modifications have been made to a range
%% of a mapped buffer. The buffer must previously have been mapped with the `?GL_MAP_FLUSH_EXPLICIT'
%%  flag.  `Offset'  and  `Length'  indicate the modified subrange of the mapping,
%% in basic units. The specified subrange to flush is relative to the start of the currently
%% mapped range of the buffer. ``gl:flushMappedBufferRange'' may be called multiple times
%% to indicate distinct subranges of the mapping which require flushing. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFlushMappedBufferRange.xml">external</a> documentation.
-spec flushMappedBufferRange(Target, Offset, Length) -> 'ok' when Target :: enum(),Offset :: integer(),Length :: integer().
flushMappedBufferRange(Target,Offset,Length) ->
  cast(5670, <<Target:?GLenum,0:32,Offset:?GLintptr,Length:?GLsizeiptr>>).

%% @doc Bind a vertex array object
%%
%% ``gl:bindVertexArray'' binds the vertex array object with name  `Array' .  `Array' 
%%  is the name of a vertex array object previously returned from a call to  {@link gl:genVertexArrays/1} 
%% , or zero to break the existing vertex array object binding. 
%%
%%  If no vertex array object with name  `Array'  exists, one is created when  `Array' 
%% is first bound. If the bind is successful no change is made to the state of the vertex
%% array object, and any previous vertex array object binding is broken. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindVertexArray.xml">external</a> documentation.
-spec bindVertexArray(Array) -> 'ok' when Array :: integer().
bindVertexArray(Array) ->
  cast(5671, <<Array:?GLuint>>).

%% @doc Delete vertex array objects
%%
%% ``gl:deleteVertexArrays'' deletes  `N'  vertex array objects whose names are stored
%% in the array addressed by  `Arrays' . Once a vertex array object is deleted it has
%% no contents and its name is again unused. If a vertex array object that is currently bound
%% is deleted, the binding for that object reverts to zero and the default vertex array becomes
%% current. Unused names in  `Arrays'  are silently ignored, as is the value zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteVertexArrays.xml">external</a> documentation.
-spec deleteVertexArrays(Arrays) -> 'ok' when Arrays :: [integer()].
deleteVertexArrays(Arrays) ->
  cast(5672, <<(length(Arrays)):?GLuint,
        (<< <<C:?GLuint>> || C <- Arrays>>)/binary,0:(((1+length(Arrays)) rem 2)*32)>>).

%% @doc Generate vertex array object names
%%
%% ``gl:genVertexArrays'' returns  `N'  vertex array object names in  `Arrays' .
%% There is no guarantee that the names form a contiguous set of integers; however, it is
%% guaranteed that none of the returned names was in use immediately before the call to ``gl:genVertexArrays''
%% . 
%%
%%  Vertex array object names returned by a call to ``gl:genVertexArrays'' are not returned
%% by subsequent calls, unless they are first deleted with  {@link gl:deleteVertexArrays/1} . 
%%
%%  The names returned in  `Arrays'  are marked as used, for the purposes of ``gl:genVertexArrays''
%%  only, but they acquire state and type only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenVertexArrays.xml">external</a> documentation.
-spec genVertexArrays(N) -> [integer()] when N :: integer().
genVertexArrays(N) ->
  call(5673, <<N:?GLsizei>>).

%% @doc Determine if a name corresponds to a vertex array object
%%
%% ``gl:isVertexArray'' returns `?GL_TRUE' if  `Array'  is currently the name of
%% a renderbuffer object. If  `Renderbuffer'  is zero, or if  `Array'  is not the name
%% of a renderbuffer object, or if an error occurs, ``gl:isVertexArray'' returns `?GL_FALSE'
%% . If  `Array'  is a name returned by  {@link gl:genVertexArrays/1} , by that has not yet
%% been bound through a call to  {@link gl:bindVertexArray/1} , then the name is not a vertex
%% array object and ``gl:isVertexArray'' returns `?GL_FALSE'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsVertexArray.xml">external</a> documentation.
-spec isVertexArray(Array) -> 0|1 when Array :: integer().
isVertexArray(Array) ->
  call(5674, <<Array:?GLuint>>).

%% @doc Retrieve the index of a named uniform block
%%
%% ``gl:getUniformIndices'' retrieves the indices of a number of uniforms within  `Program' 
%% . 
%%
%%  `Program'  must be the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  must have been called in the past, although it is not required that  {@link gl:linkProgram/1} 
%%  must have succeeded. The link could have failed because the number of active uniforms
%% exceeded the limit. 
%%
%%  `UniformCount'  indicates both the number of elements in the array of names  `UniformNames' 
%%  and the number of indices that may be written to  `UniformIndices' . 
%%
%%  `UniformNames'  contains a list of  `UniformCount'  name strings identifying the
%% uniform names to be queried for indices. For each name string in  `UniformNames' ,
%% the index assigned to the active uniform of that name will be written to the corresponding
%% element of  `UniformIndices' . If a string in  `UniformNames'  is not the name of
%% an active uniform, the special value `?GL_INVALID_INDEX' will be written to the corresponding
%% element of  `UniformIndices' . 
%%
%%  If an error occurs, nothing is written to  `UniformIndices' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformIndices.xml">external</a> documentation.
-spec getUniformIndices(Program, UniformNames) -> [integer()] when Program :: integer(),UniformNames :: iolist().
getUniformIndices(Program,UniformNames) ->
 UniformNamesTemp = list_to_binary([[Str|[0]] || Str <- UniformNames ]),
  call(5675, <<Program:?GLuint,(length(UniformNames)):?GLuint,(size(UniformNamesTemp)):?GLuint,(UniformNamesTemp)/binary,0:((8-((size(UniformNamesTemp)+0) rem 8)) rem 8)>>).

%% @doc glGetActiveUniforms
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniforms.xml">external</a> documentation.
-spec getActiveUniformsiv(Program, UniformIndices, Pname) -> [integer()] when Program :: integer(),UniformIndices :: [integer()],Pname :: enum().
getActiveUniformsiv(Program,UniformIndices,Pname) ->
  call(5676, <<Program:?GLuint,(length(UniformIndices)):?GLuint,
        (<< <<C:?GLuint>> || C <- UniformIndices>>)/binary,0:(((length(UniformIndices)) rem 2)*32),Pname:?GLenum>>).

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
%%  If ``gl:getActiveUniformName'' is not successful, nothing is written to  `Length' 
%% or  `UniformName' . 
%%
%%  `Program'  must be the name of a program for which the command  {@link gl:linkProgram/1} 
%% has been issued in the past. It is not necessary for  `Program'  to have been linked
%% successfully. The link could have failed because the number of active uniforms exceeded
%% the limit. 
%%
%%  `UniformIndex'  must be an active uniform index of the program  `Program' , in
%% the range zero to `?GL_ACTIVE_UNIFORMS' - 1. The value of `?GL_ACTIVE_UNIFORMS'
%% can be queried with  {@link gl:getProgramiv/2} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformName.xml">external</a> documentation.
-spec getActiveUniformName(Program, UniformIndex, BufSize) -> string() when Program :: integer(),UniformIndex :: integer(),BufSize :: integer().
getActiveUniformName(Program,UniformIndex,BufSize) ->
  call(5677, <<Program:?GLuint,UniformIndex:?GLuint,BufSize:?GLsizei>>).

%% @doc Retrieve the index of a named uniform block
%%
%% ``gl:getUniformBlockIndex'' retrieves the index of a uniform block within  `Program' .
%% 
%%
%%  `Program'  must be the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  must have been called in the past, although it is not required that  {@link gl:linkProgram/1} 
%%  must have succeeded. The link could have failed because the number of active uniforms
%% exceeded the limit. 
%%
%%  `UniformBlockName'  must contain a nul-terminated string specifying the name of the
%% uniform block. 
%%
%% ``gl:getUniformBlockIndex'' returns the uniform block index for the uniform block named
%%  `UniformBlockName'  of  `Program' . If  `UniformBlockName'  does not identify
%% an active uniform block of  `Program' , ``gl:getUniformBlockIndex'' returns the special
%% identifier, `?GL_INVALID_INDEX'. Indices of the active uniform blocks of a program
%% are assigned in consecutive order, beginning with zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformBlockIndex.xml">external</a> documentation.
-spec getUniformBlockIndex(Program, UniformBlockName) -> integer() when Program :: integer(),UniformBlockName :: string().
getUniformBlockIndex(Program,UniformBlockName) ->
  call(5678, <<Program:?GLuint,(list_to_binary([UniformBlockName|[0]]))/binary,0:((8-((length(UniformBlockName)+ 5) rem 8)) rem 8)>>).

%% @doc Query information about an active uniform block
%%
%% ``gl:getActiveUniformBlockiv'' retrieves information about an active uniform block within
%%  `Program' . 
%%
%%  `Program'  must be the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  must have been called in the past, although it is not required that  {@link gl:linkProgram/1} 
%%  must have succeeded. The link could have failed because the number of active uniforms
%% exceeded the limit. 
%%
%%  `UniformBlockIndex'  is an active uniform block index of  `Program' , and must
%% be less than the value of `?GL_ACTIVE_UNIFORM_BLOCKS'. 
%%
%%  Upon success, the uniform block parameter(s) specified by  `Pname'  are returned in  `Params' 
%% . If an error occurs, nothing will be written to  `Params' . 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_BINDING', then the index of the uniform buffer
%% binding point last selected by the uniform block specified by  `UniformBlockIndex' 
%% for  `Program'  is returned. If no uniform block has been previously specified, zero
%% is returned. 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_DATA_SIZE', then the implementation-dependent
%% minimum total buffer object size, in basic machine units, required to hold all active
%% uniforms in the uniform block identified by  `UniformBlockIndex'  is returned. It is
%% neither guaranteed nor expected that a given implementation will arrange uniform values
%% as tightly packed in a buffer object. The exception to this is the `std140 uniform block layout'
%% , which guarantees specific packing behavior and does not require the application to query
%% for offsets and strides. In this case the minimum size may still be queried, even though
%% it is determined in advance based only on the uniform block declaration. 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_NAME_LENGTH', then the total length (including
%% the nul terminator) of the name of the uniform block identified by  `UniformBlockIndex' 
%%  is returned. 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS', then the number of active
%% uniforms in the uniform block identified by  `UniformBlockIndex'  is returned. 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES', then a list of the
%% active uniform indices for the uniform block identified by  `UniformBlockIndex'  is
%% returned. The number of elements that will be written to  `Params'  is the value of `?GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS'
%%  for  `UniformBlockIndex' . 
%%
%%  If  `Pname'  is `?GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER', `?GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER'
%% , or `?GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER', then a boolean value indicating
%% whether the uniform block identified by  `UniformBlockIndex'  is referenced by the
%% vertex, geometry, or fragment programming stages of program, respectively, is returned. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlock.xml">external</a> documentation.
-spec getActiveUniformBlockiv(Program, UniformBlockIndex, Pname, Params) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),Pname :: enum(),Params :: mem().
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) ->
  send_bin(Params),
  call(5679, <<Program:?GLuint,UniformBlockIndex:?GLuint,Pname:?GLenum>>).

%% @doc Retrieve the name of an active uniform block
%%
%% ``gl:getActiveUniformBlockName'' retrieves the name of the active uniform block at  `UniformBlockIndex' 
%%  within  `Program' . 
%%
%%  `Program'  must be the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  must have been called in the past, although it is not required that  {@link gl:linkProgram/1} 
%%  must have succeeded. The link could have failed because the number of active uniforms
%% exceeded the limit. 
%%
%%  `UniformBlockIndex'  is an active uniform block index of  `Program' , and must
%% be less than the value of `?GL_ACTIVE_UNIFORM_BLOCKS'. 
%%
%%  Upon success, the name of the uniform block identified by  `UnifomBlockIndex'  is
%% returned into  `UniformBlockName' . The name is nul-terminated. The actual number of
%% characters written into  `UniformBlockName' , excluding the nul terminator, is returned
%% in  `Length' . If  `Length'  is NULL, no length is returned. 
%%
%%  `BufSize'  contains the maximum number of characters (including the nul terminator)
%% that will be written into  `UniformBlockName' . 
%%
%%  If an error occurs, nothing will be written to  `UniformBlockName'  or  `Length' .
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveUniformBlockName.xml">external</a> documentation.
-spec getActiveUniformBlockName(Program, UniformBlockIndex, BufSize) -> string() when Program :: integer(),UniformBlockIndex :: integer(),BufSize :: integer().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) ->
  call(5680, <<Program:?GLuint,UniformBlockIndex:?GLuint,BufSize:?GLsizei>>).

%% @doc Assign a binding point to an active uniform block
%%
%%  Binding points for active uniform blocks are assigned using ``gl:uniformBlockBinding''.
%% Each of a program's active uniform blocks has a corresponding uniform buffer binding point.
%%  `Program'  is the name of a program object for which the command  {@link gl:linkProgram/1} 
%%  has been issued in the past. 
%%
%%  If successful, ``gl:uniformBlockBinding'' specifies that  `Program'  will use the
%% data store of the buffer object bound to the binding point  `UniformBlockBinding' 
%% to extract the values of the uniforms in the uniform block identified by  `UniformBlockIndex' 
%% . 
%%
%%  When a program object is linked or re-linked, the uniform buffer object binding point
%% assigned to each of its active uniform blocks is reset to zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformBlockBinding.xml">external</a> documentation.
-spec uniformBlockBinding(Program, UniformBlockIndex, UniformBlockBinding) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),UniformBlockBinding :: integer().
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) ->
  cast(5681, <<Program:?GLuint,UniformBlockIndex:?GLuint,UniformBlockBinding:?GLuint>>).

%% @doc Copy part of the data store of a buffer object to the data store of another buffer object
%%
%% ``gl:copyBufferSubData'' copies part of the data store attached to  `Readtarget' 
%% to the data store attached to  `Writetarget' . The number of basic machine units indicated
%% by  `Size'  is copied from the source, at offset  `Readoffset'  to the destination
%% at  `Writeoffset' , also in basic machine units. 
%%
%%  `Readtarget'  and  `Writetarget'  must be `?GL_ARRAY_BUFFER', `?GL_COPY_READ_BUFFER'
%% , `?GL_COPY_WRITE_BUFFER', `?GL_ELEMENT_ARRAY_BUFFER', `?GL_PIXEL_PACK_BUFFER'
%% , `?GL_PIXEL_UNPACK_BUFFER', `?GL_TEXTURE_BUFFER', `?GL_TRANSFORM_FEEDBACK_BUFFER'
%%  or `?GL_UNIFORM_BUFFER'. Any of these targets may be used, although the targets `?GL_COPY_READ_BUFFER'
%%  and `?GL_COPY_WRITE_BUFFER' are provided specifically to allow copies between buffers
%% without disturbing other GL state. 
%%
%%  `Readoffset' ,  `Writeoffset'  and  `Size'  must all be greater than or equal
%% to zero. Furthermore,  `Readoffset'  +  `Size'  must not exceeed the size of the
%% buffer object bound to  `Readtarget' , and  `Readoffset'  +  `Size'  must not
%% exceeed the size of the buffer bound to  `Writetarget' . If the same buffer object
%% is bound to both  `Readtarget'  and  `Writetarget' , then the ranges specified by  `Readoffset' 
%% ,  `Writeoffset'  and  `Size'  must not overlap. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCopyBufferSubData.xml">external</a> documentation.
-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok' when ReadTarget :: enum(),WriteTarget :: enum(),ReadOffset :: integer(),WriteOffset :: integer(),Size :: integer().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) ->
  cast(5682, <<ReadTarget:?GLenum,WriteTarget:?GLenum,ReadOffset:?GLintptr,WriteOffset:?GLintptr,Size:?GLsizeiptr>>).

%% @doc Render primitives from array data with a per-element offset
%%
%% ``gl:drawElementsBaseVertex'' behaves identically to  {@link gl:drawElements/4}  except
%% that the `i'th element transferred by the corresponding draw call will be taken from
%% element  `Indices' [i] +  `Basevertex'  of each enabled array. If the resulting
%% value is larger than the maximum value representable by  `Type' , it is as if the calculation
%% were upconverted to 32-bit unsigned integers (with wrapping on overflow conditions). The
%% operation is undefined if the sum would be negative. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsBaseVertex.xml">external</a> documentation.
-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5683, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5684, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawRangeElementsBaseVertex.xml">external</a> documentation.
-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when  is_integer(Indices) ->
  cast(5685, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Basevertex:?GLint>>);
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) ->
  send_bin(Indices),
  cast(5686, <<Mode:?GLenum,Start:?GLuint,End:?GLuint,Count:?GLsizei,Type:?GLenum,Basevertex:?GLint>>).

%% @doc Render multiple instances of a set of primitives from array data with a per-element offset
%%
%% ``gl:drawElementsInstancedBaseVertex'' behaves identically to  {@link gl:drawElementsInstanced/5} 
%%  except that the `i'th element transferred by the corresponding draw call will be
%% taken from element  `Indices' [i] +  `Basevertex'  of each enabled array. If the
%% resulting value is larger than the maximum value representable by  `Type' , it is as
%% if the calculation were upconverted to 32-bit unsigned integers (with wrapping on overflow
%% conditions). The operation is undefined if the sum would be negative. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstancedBaseVertex.xml">external</a> documentation.
-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Primcount, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) when  is_integer(Indices) ->
  cast(5687, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Basevertex:?GLint>>);
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Primcount,Basevertex) ->
  send_bin(Indices),
  cast(5688, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Basevertex:?GLint>>).

%% @doc Specifiy the vertex to be used as the source of data for flat shaded varyings
%%
%% `Flatshading' a vertex shader varying output means to assign all vetices of the primitive
%% the same value for that output. The vertex from which these values is derived is known
%% as the `provoking vertex' and ``gl:provokingVertex'' specifies which vertex is
%% to be used as the source of data for flat shaded varyings. 
%%
%%  `ProvokeMode'  must be either `?GL_FIRST_VERTEX_CONVENTION' or `?GL_LAST_VERTEX_CONVENTION'
%% , and controls the selection of the vertex whose values are assigned to flatshaded varying
%% outputs. The interpretation of these values for the supported primitive types is: <table><tbody>
%% <tr><td>` Primitive Type of Polygon '`i'</td><td>` First Vertex Convention '
%% </td><td>` Last Vertex Convention '</td></tr><tr><td> point </td><td>`i'</td><td>
%% `i'</td></tr><tr><td> independent line </td><td> 2`i' - 1 </td><td> 2`i'</td>
%% </tr><tr><td> line loop </td><td>`i'</td><td>
%%
%% `i' + 1, if `i' &lt; `n'
%%
%%  1, if `i' = `n'</td></tr><tr><td> line strip </td><td>`i'</td><td>`i'
%% + 1 </td></tr><tr><td> independent triangle </td><td> 3`i' - 2 </td><td> 3`i'</td>
%% </tr><tr><td> triangle strip </td><td>`i'</td><td>`i' + 2 </td></tr><tr><td>
%% triangle fan </td><td>`i' + 1 </td><td>`i' + 2 </td></tr><tr><td> line adjacency
%% </td><td> 4`i' - 2 </td><td> 4`i' - 1 </td></tr><tr><td> line strip adjacency </td>
%% <td>`i' + 1 </td><td>`i' + 2 </td></tr><tr><td> triangle adjacency </td><td> 6`i'
%%  - 5 </td><td> 6`i' - 1 </td></tr><tr><td> triangle strip adjacency </td><td> 2`i'
%%  - 1 </td><td> 2`i' + 3 </td></tr></tbody></table>
%%
%%  If a vertex or geometry shader is active, user-defined varying outputs may be flatshaded
%% by using the flat qualifier when declaring the output. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProvokingVertex.xml">external</a> documentation.
-spec provokingVertex(Mode) -> 'ok' when Mode :: enum().
provokingVertex(Mode) ->
  cast(5689, <<Mode:?GLenum>>).

%% @doc Create a new sync object and insert it into the GL command stream
%%
%% ``gl:fenceSync'' creates a new fence sync object, inserts a fence command into the GL
%% command stream and associates it with that sync object, and returns a non-zero name corresponding
%% to the sync object. 
%%
%%  When the specified  `Condition'  of the sync object is satisfied by the fence command,
%% the sync object is signaled by the GL, causing any  {@link gl:waitSync/3} ,  {@link gl:clientWaitSync/3} 
%%  commands blocking in  `Sync'  to `unblock'. No other state is affected by ``gl:fenceSync''
%%  or by the execution of the associated fence command. 
%%
%%  `Condition'  must be `?GL_SYNC_GPU_COMMANDS_COMPLETE'. This condition is satisfied
%% by  completion of the fence command corresponding to the sync object and all preceding
%% commands in the same command stream. The sync object will not be signaled until all effects
%% from these commands on GL client and server state and the framebuffer are fully realized.
%% Note that completion of the fence command occurs once the state of the corresponding sync
%% object has been changed, but commands waiting on that sync object may not be unblocked
%% until after the fence command completes. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glFenceSync.xml">external</a> documentation.
-spec fenceSync(Condition, Flags) -> integer() when Condition :: enum(),Flags :: integer().
fenceSync(Condition,Flags) ->
  call(5690, <<Condition:?GLenum,Flags:?GLbitfield>>).

%% @doc Determine if a name corresponds to a sync object
%%
%% ``gl:isSync'' returns `?GL_TRUE' if  `Sync'  is currently the name of a sync
%% object. If  `Sync'  is not the name of a sync object, or if an error occurs, ``gl:isSync''
%%  returns `?GL_FALSE'. Note that zero is not the name of a sync object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsSync.xml">external</a> documentation.
-spec isSync(Sync) -> 0|1 when Sync :: integer().
isSync(Sync) ->
  call(5691, <<Sync:?GLsync>>).

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
%% ``gl:deleteSync'' will silently ignore a  `Sync'  value of zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteSync.xml">external</a> documentation.
-spec deleteSync(Sync) -> 'ok' when Sync :: integer().
deleteSync(Sync) ->
  cast(5692, <<Sync:?GLsync>>).

%% @doc Block and wait for a sync object to become signaled
%%
%% ``gl:clientWaitSync'' causes the client to block and wait for the sync object specified
%% by  `Sync'  to become signaled. If  `Sync'  is signaled when ``gl:clientWaitSync''
%% is called, ``gl:clientWaitSync'' returns immediately, otherwise it will block and wait
%% for up to  `Timeout'  nanoseconds for  `Sync'  to become signaled. 
%%
%%  The return value is one of four status values: 
%%
%% `?GL_ALREADY_SIGNALED' indicates that  `Sync'  was signaled at the time that ``gl:clientWaitSync''
%%  was called. 
%%
%% `?GL_TIMEOUT_EXPIRED' indicates that at least  `Timeout'  nanoseconds passed and  `Sync' 
%%  did not become signaled. 
%%
%% `?GL_CONDITION_SATISFIED' indicates that  `Sync'  was signaled before the timeout
%% expired. 
%%
%% `?GL_WAIT_FAILED' indicates that an error occurred. Additionally, an OpenGL error
%% will be generated. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClientWaitSync.xml">external</a> documentation.
-spec clientWaitSync(Sync, Flags, Timeout) -> enum() when Sync :: integer(),Flags :: integer(),Timeout :: integer().
clientWaitSync(Sync,Flags,Timeout) ->
  call(5693, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @doc Instruct the GL server to block until the specified sync object becomes signaled
%%
%% ``gl:waitSync'' causes the GL server to block and wait until  `Sync'  becomes signaled.
%%  `Sync'  is the name of an existing sync object upon which to wait.  `Flags'  and  `Timeout' 
%%  are currently not used and must be set to zero and the special value `?GL_TIMEOUT_IGNORED'
%% , respectively
%%
%%  `Flags'  and  `Timeout'  are placeholders for anticipated future extensions of
%% sync object capabilities. They must have these reserved values in order that existing
%% code calling ``gl:waitSync'' operate properly in the presence of such extensions.. ``gl:waitSync''
%%  will always wait no longer than an implementation-dependent timeout. The duration of
%% this timeout in nanoseconds may be queried by calling  {@link gl:getBooleanv/1}  with the parameter `?GL_MAX_SERVER_WAIT_TIMEOUT'
%% . There is currently no way to determine whether ``gl:waitSync'' unblocked because the
%% timeout expired or because the sync object being waited on was signaled. 
%%
%%  If an error occurs, ``gl:waitSync'' does not cause the GL server to block. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glWaitSync.xml">external</a> documentation.
-spec waitSync(Sync, Flags, Timeout) -> 'ok' when Sync :: integer(),Flags :: integer(),Timeout :: integer().
waitSync(Sync,Flags,Timeout) ->
  cast(5694, <<Sync:?GLsync,Flags:?GLbitfield,0:32,Timeout:?GLuint64>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getInteger64v(Pname) -> [integer()] when Pname :: enum().
getInteger64v(Pname) ->
  call(5695, <<Pname:?GLenum>>).

%% @doc Query the properties of a sync object
%%
%% ``gl:getSynciv'' retrieves properties of a sync object.  `Sync'  specifies the name
%% of the sync object whose properties to retrieve. 
%%
%%  On success, ``gl:getSynciv'' replaces up to  `BufSize'  integers in  `Values' 
%% with the corresponding property values of the object being queried. The actual number
%% of integers replaced is returned in the variable whose address is specified in  `Length' 
%% . If  `Length'  is NULL, no length is returned. 
%%
%%  If  `Pname'  is `?GL_OBJECT_TYPE', a single value representing the specific type
%% of the sync object is placed in  `Values' . The only type supported is `?GL_SYNC_FENCE'
%% . 
%%
%%  If  `Pname'  is `?GL_SYNC_STATUS', a single value representing the status of
%% the sync object (`?GL_SIGNALED' or `?GL_UNSIGNALED') is placed in  `Values' .
%% 
%%
%%  If  `Pname'  is `?GL_SYNC_CONDITION', a single value representing the condition
%% of the sync object is placed in  `Values' . The only condition supported is `?GL_SYNC_GPU_COMMANDS_COMPLETE'
%% . 
%%
%%  If  `Pname'  is `?GL_SYNC_FLAGS', a single value representing the flags with
%% which the sync object was created is placed in  `Values' . No flags are currently supported
%% 
%%
%%  `Flags'  is expected to be used in future extensions to the sync objects.. 
%%
%%  If an error occurs, nothing will be written to  `Values'  or  `Length' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSync.xml">external</a> documentation.
-spec getSynciv(Sync, Pname, BufSize) -> [integer()] when Sync :: integer(),Pname :: enum(),BufSize :: integer().
getSynciv(Sync,Pname,BufSize) ->
  call(5696, <<Sync:?GLsync,Pname:?GLenum,BufSize:?GLsizei>>).

%% @doc Establish the data storage, format, dimensions, and number of samples of a multisample texture's image
%%
%% ``gl:texImage2DMultisample'' establishes the data storage, format, dimensions and number
%% of samples of a multisample texture's image. 
%%
%%  `Target'  must be `?GL_TEXTURE_2D_MULTISAMPLE' or `?GL_PROXY_TEXTURE_2D_MULTISAMPLE'
%% .  `Width'  and  `Height'  are the dimensions in texels of the texture, and must
%% be in the range zero to `?GL_MAX_TEXTURE_SIZE' - 1.  `Samples'  specifies the
%% number of samples in the image and must be in the range zero to `?GL_MAX_SAMPLES'
%% - 1. 
%%
%%  `Internalformat'  must be a color-renderable, depth-renderable, or stencil-renderable
%% format. 
%%
%%  If  `Fixedsamplelocations'  is `?GL_TRUE', the image will use identical sample
%% locations and the same number of samples for all texels in the image, and the sample locations
%% will not depend on the internal format or size of the image. 
%%
%%  When a multisample texture is accessed in a shader, the access takes one vector of integers
%% describing which texel to fetch and an integer corresponding to the sample numbers describing
%% which sample within the texel to fetch. No standard sampling instructions are allowed
%% on the multisample texture targets. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage2DMultisample.xml">external</a> documentation.
-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Fixedsamplelocations :: 0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) ->
  cast(5697, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Fixedsamplelocations:?GLboolean>>).

%% @doc Establish the data storage, format, dimensions, and number of samples of a multisample texture's image
%%
%% ``gl:texImage3DMultisample'' establishes the data storage, format, dimensions and number
%% of samples of a multisample texture's image. 
%%
%%  `Target'  must be `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY' or `?GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY'
%% .  `Width'  and  `Height' are the dimensions in texels of the texture, and must
%% be in the range zero to `?GL_MAX_TEXTURE_SIZE' - 1.  `Depth'  is the number of
%% array slices in the array texture's image.  `Samples'  specifies the number of samples
%% in the image and must be in the range zero to `?GL_MAX_SAMPLES' - 1. 
%%
%%  `Internalformat'  must be a color-renderable, depth-renderable, or stencil-renderable
%% format. 
%%
%%  If  `Fixedsamplelocations'  is `?GL_TRUE', the image will use identical sample
%% locations and the same number of samples for all texels in the image, and the sample locations
%% will not depend on the internal format or size of the image. 
%%
%%  When a multisample texture is accessed in a shader, the access takes one vector of integers
%% describing which texel to fetch and an integer corresponding to the sample numbers describing
%% which sample within the texel to fetch. No standard sampling instructions are allowed
%% on the multisample texture targets. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexImage3DMultisample.xml">external</a> documentation.
-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Fixedsamplelocations :: 0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) ->
  cast(5698, <<Target:?GLenum,Samples:?GLsizei,Internalformat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Fixedsamplelocations:?GLboolean>>).

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
%%  If the multisample mode does not have fixed sample locations, the returned values may
%% only reflect the locations of samples within some pixels. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetMultisample.xml">external</a> documentation.
-spec getMultisamplefv(Pname, Index) -> {float(),float()} when Pname :: enum(),Index :: integer().
getMultisamplefv(Pname,Index) ->
  call(5699, <<Pname:?GLenum,Index:?GLuint>>).

%% @doc Set the value of a sub-word of the sample mask
%%
%% ``gl:sampleMaski'' sets one 32-bit sub-word of the multi-word sample mask, `?GL_SAMPLE_MASK_VALUE'
%% . 
%%
%%  `MaskIndex'  specifies which 32-bit sub-word of the sample mask to update, and  `Mask' 
%%  specifies the new value to use for that sub-word.  `MaskIndex'  must be less than
%% the value of `?GL_MAX_SAMPLE_MASK_WORDS'. Bit `B' of mask word `M' corresponds
%% to sample 32 x `M' + `B'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSampleMaski.xml">external</a> documentation.
-spec sampleMaski(Index, Mask) -> 'ok' when Index :: integer(),Mask :: integer().
sampleMaski(Index,Mask) ->
  cast(5700, <<Index:?GLuint,Mask:?GLbitfield>>).

%% @doc glNamedStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glNamedStringARB.xml">external</a> documentation.
-spec namedStringARB(Type, Name, String) -> 'ok' when Type :: enum(),Name :: string(),String :: string().
namedStringARB(Type,Name,String) ->
  cast(5701, <<Type:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8),(list_to_binary([String|[0]]))/binary,0:((8-((length(String)+ 1) rem 8)) rem 8)>>).

%% @doc glDeleteNamedStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteNamedStringARB.xml">external</a> documentation.
-spec deleteNamedStringARB(Name) -> 'ok' when Name :: string().
deleteNamedStringARB(Name) ->
  cast(5702, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc glCompileShaderIncludeARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCompileShaderIncludeARB.xml">external</a> documentation.
-spec compileShaderIncludeARB(Shader, Path) -> 'ok' when Shader :: integer(),Path :: iolist().
compileShaderIncludeARB(Shader,Path) ->
 PathTemp = list_to_binary([[Str|[0]] || Str <- Path ]),
  cast(5703, <<Shader:?GLuint,(length(Path)):?GLuint,(size(PathTemp)):?GLuint,(PathTemp)/binary,0:((8-((size(PathTemp)+0) rem 8)) rem 8)>>).

%% @doc glIsNamedStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsNamedStringARB.xml">external</a> documentation.
-spec isNamedStringARB(Name) -> 0|1 when Name :: string().
isNamedStringARB(Name) ->
  call(5704, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc glGetNamedStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetNamedStringARB.xml">external</a> documentation.
-spec getNamedStringARB(Name, BufSize) -> string() when Name :: string(),BufSize :: integer().
getNamedStringARB(Name,BufSize) ->
  call(5705, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8),BufSize:?GLsizei>>).

%% @doc glGetNamedStringARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetNamedStringARB.xml">external</a> documentation.
-spec getNamedStringivARB(Name, Pname) -> integer() when Name :: string(),Pname :: enum().
getNamedStringivARB(Name,Pname) ->
  call(5706, <<(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8),Pname:?GLenum>>).

%% @doc glBindFragDataLocationIndexe
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindFragDataLocationIndexe.xml">external</a> documentation.
-spec bindFragDataLocationIndexed(Program, ColorNumber, Index, Name) -> 'ok' when Program :: integer(),ColorNumber :: integer(),Index :: integer(),Name :: string().
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) ->
  cast(5707, <<Program:?GLuint,ColorNumber:?GLuint,Index:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @doc Query the bindings of color indices to user-defined varying out variables
%%
%% ``gl:getFragDataIndex'' returns the index of the fragment color to which the variable  `Name' 
%%  was bound when the program object  `Program'  was last linked. If  `Name'  is not
%% a varying out variable of  `Program' , or if an error occurs, -1 will be returned. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetFragDataIndex.xml">external</a> documentation.
-spec getFragDataIndex(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataIndex(Program,Name) ->
  call(5708, <<Program:?GLuint,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 5) rem 8)) rem 8)>>).

%% @doc Generate sampler object names
%%
%% ``gl:genSamplers'' returns  `N'  sampler object names in  `Samplers' . There is
%% no guarantee that the names form a contiguous set of integers; however, it is guaranteed
%% that none of the returned names was in use immediately before the call to ``gl:genSamplers''
%% . 
%%
%%  Sampler object names returned by a call to ``gl:genSamplers'' are not returned by subsequent
%% calls, unless they are first deleted with  {@link gl:deleteSamplers/1} . 
%%
%%  The names returned in  `Samplers'  are marked as used, for the purposes of ``gl:genSamplers''
%%  only, but they acquire state and type only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenSamplers.xml">external</a> documentation.
-spec genSamplers(Count) -> [integer()] when Count :: integer().
genSamplers(Count) ->
  call(5709, <<Count:?GLsizei>>).

%% @doc Delete named sampler objects
%%
%% ``gl:deleteSamplers'' deletes  `N'  sampler objects named by the elements of the
%% array  `Ids' . After a sampler object is deleted, its name is again unused. If a sampler
%% object that is currently bound to a sampler unit is deleted, it is as though  {@link gl:bindSampler/2} 
%%  is called with unit set to the unit the sampler is bound to and sampler zero. Unused
%% names in samplers are silently ignored, as is the reserved name zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteSamplers.xml">external</a> documentation.
-spec deleteSamplers(Samplers) -> 'ok' when Samplers :: [integer()].
deleteSamplers(Samplers) ->
  cast(5710, <<(length(Samplers)):?GLuint,
        (<< <<C:?GLuint>> || C <- Samplers>>)/binary,0:(((1+length(Samplers)) rem 2)*32)>>).

%% @doc Determine if a name corresponds to a sampler object
%%
%% ``gl:isSampler'' returns `?GL_TRUE' if  `Id'  is currently the name of a sampler
%% object. If  `Id'  is zero, or is a non-zero value that is not currently the name of
%% a sampler object, or if an error occurs, ``gl:isSampler'' returns `?GL_FALSE'. 
%%
%%  A name returned by  {@link gl:genSamplers/1} , is the name of a sampler object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsSampler.xml">external</a> documentation.
-spec isSampler(Sampler) -> 0|1 when Sampler :: integer().
isSampler(Sampler) ->
  call(5711, <<Sampler:?GLuint>>).

%% @doc Bind a named sampler to a texturing target
%%
%% ``gl:bindSampler'' binds  `Sampler'  to the texture unit at index  `Unit' .  `Sampler' 
%%  must be zero or the name of a sampler object previously returned from a call to  {@link gl:genSamplers/1} 
%% .  `Unit'  must be less than the value of `?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS'.
%% 
%%
%%  When a sampler object is bound to a texture unit, its state supersedes that of the texture
%% object bound to that texture unit. If the sampler name zero is bound to a texture unit,
%% the currently bound texture's sampler state becomes active. A single sampler object may
%% be bound to multiple texture units simultaneously. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindSampler.xml">external</a> documentation.
-spec bindSampler(Unit, Sampler) -> 'ok' when Unit :: integer(),Sampler :: integer().
bindSampler(Unit,Sampler) ->
  cast(5712, <<Unit:?GLuint,Sampler:?GLuint>>).

%% @doc Set sampler parameters
%%
%% ``gl:samplerParameter'' assigns the value or values in  `Params'  to the sampler
%% parameter specified as  `Pname' .  `Sampler'  specifies the sampler object to be
%% modified, and must be the name of a sampler object previously returned from a call to  {@link gl:genSamplers/1} 
%% . The following symbols are accepted in  `Pname' : 
%%
%% `?GL_TEXTURE_MIN_FILTER':  The texture minifying function is used whenever the pixel
%% being textured maps to an area greater than one texture element. There are six defined
%% minifying functions. Two of them use the nearest one or nearest four texture elements
%% to compute the texture value. The other four use mipmaps. 
%%
%%  A mipmap is an ordered set of arrays representing the same image at progressively lower
%% resolutions. If the texture has dimensions   2 n×2 m, there are  max(n m)+1 mipmaps. The first
%% mipmap is the original texture, with dimensions   2 n×2 m. Each subsequent mipmap has
%% dimensions   2(k-1)×2(l-1), where   2 k×2 l are the dimensions of the previous mipmap, until either
%%   k=0 or   l=0. At that point, subsequent mipmaps have dimension   1×2(l-1) or   2(k-1)×1 until
%% the final mipmap, which has dimension   1×1. To define the mipmaps, call  {@link gl:texImage1D/8} 
%% ,  {@link gl:texImage2D/9} ,  {@link gl:texImage3D/10} ,  {@link gl:copyTexImage1D/7} , or  {@link gl:copyTexImage2D/8} 
%%  with the `level' argument indicating the order of the mipmaps. Level 0 is the original
%% texture; level   max(n m) is the final   1×1 mipmap. 
%%
%%  `Params'  supplies a function for minifying the texture as one of the following: 
%%
%% `?GL_NEAREST':  Returns the value of the texture element that is nearest (in Manhattan
%% distance) to the center of the pixel being textured. 
%%
%% `?GL_LINEAR':  Returns the weighted average of the four texture elements that are
%% closest to the center of the pixel being textured. These can include border texture elements,
%% depending on the values of `?GL_TEXTURE_WRAP_S' and `?GL_TEXTURE_WRAP_T', and
%% on the exact mapping. 
%%
%% `?GL_NEAREST_MIPMAP_NEAREST':  Chooses the mipmap that most closely matches the size
%% of the pixel being textured and uses the `?GL_NEAREST' criterion (the texture element
%% nearest to the center of the pixel) to produce a texture value. 
%%
%% `?GL_LINEAR_MIPMAP_NEAREST':  Chooses the mipmap that most closely matches the size
%% of the pixel being textured and uses the `?GL_LINEAR' criterion (a weighted average
%% of the four texture elements that are closest to the center of the pixel) to produce a
%% texture value. 
%%
%% `?GL_NEAREST_MIPMAP_LINEAR':  Chooses the two mipmaps that most closely match the
%% size of the pixel being textured and uses the `?GL_NEAREST' criterion (the texture
%% element nearest to the center of the pixel) to produce a texture value from each mipmap.
%% The final texture value is a weighted average of those two values. 
%%
%% `?GL_LINEAR_MIPMAP_LINEAR':  Chooses the two mipmaps that most closely match the
%% size of the pixel being textured and uses the `?GL_LINEAR' criterion (a weighted
%% average of the four texture elements that are closest to the center of the pixel) to produce
%% a texture value from each mipmap. The final texture value is a weighted average of those
%% two values. 
%%
%%  As more texture elements are sampled in the minification process, fewer aliasing artifacts
%% will be apparent. While the `?GL_NEAREST' and `?GL_LINEAR' minification functions
%% can be faster than the other four, they sample only one or four texture elements to determine
%% the texture value of the pixel being rendered and can produce moire patterns or ragged
%% transitions. The initial value of `?GL_TEXTURE_MIN_FILTER' is `?GL_NEAREST_MIPMAP_LINEAR'
%% . 
%%
%% `?GL_TEXTURE_MAG_FILTER':  The texture magnification function is used when the pixel
%% being textured maps to an area less than or equal to one texture element. It sets the
%% texture magnification function to either `?GL_NEAREST' or `?GL_LINEAR' (see
%% below). `?GL_NEAREST' is generally faster than `?GL_LINEAR', but it can produce
%% textured images with sharper edges because the transition between texture elements is
%% not as smooth. The initial value of `?GL_TEXTURE_MAG_FILTER' is `?GL_LINEAR'. 
%%
%% `?GL_NEAREST':  Returns the value of the texture element that is nearest (in Manhattan
%% distance) to the center of the pixel being textured. 
%%
%% `?GL_LINEAR':  Returns the weighted average of the four texture elements that are
%% closest to the center of the pixel being textured. These can include border texture elements,
%% depending on the values of `?GL_TEXTURE_WRAP_S' and `?GL_TEXTURE_WRAP_T', and
%% on the exact mapping. 
%%
%% 
%%
%% `?GL_TEXTURE_MIN_LOD':  Sets the minimum level-of-detail parameter. This floating-point
%% value limits the selection of highest resolution mipmap (lowest mipmap level). The initial
%% value is -1000. 
%%
%% 
%%
%% `?GL_TEXTURE_MAX_LOD':  Sets the maximum level-of-detail parameter. This floating-point
%% value limits the selection of the lowest resolution mipmap (highest mipmap level). The
%% initial value is 1000. 
%%
%% 
%%
%% `?GL_TEXTURE_WRAP_S':  Sets the wrap parameter for texture coordinate   s to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. `?GL_CLAMP_TO_BORDER' causes
%% the   s coordinate to be clamped to the range [(-1 2/N) 1+(1 2/N)], where   N is the size of the texture in
%% the direction of clamping.`?GL_CLAMP_TO_EDGE' causes   s coordinates to be clamped
%% to the range  [(1 2/N) 1-(1 2/N)], where   N is the size of the texture in the direction of clamping. `?GL_REPEAT'
%%  causes the integer part of the   s coordinate to be ignored; the GL uses only the fractional
%% part, thereby creating a repeating pattern. `?GL_MIRRORED_REPEAT' causes the   s
%% coordinate to be set to the fractional part of the texture coordinate if the integer part
%% of   s is even; if the integer part of   s is odd, then the   s texture coordinate is
%% set to   1-frac(s), where   frac(s) represents the fractional part of  s. Initially, `?GL_TEXTURE_WRAP_S'
%%  is set to `?GL_REPEAT'. 
%%
%% 
%%
%% `?GL_TEXTURE_WRAP_T':  Sets the wrap parameter for texture coordinate   t to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. See the discussion under `?GL_TEXTURE_WRAP_S'
%% . Initially, `?GL_TEXTURE_WRAP_T' is set to `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_WRAP_R':  Sets the wrap parameter for texture coordinate   r to either `?GL_CLAMP_TO_EDGE'
%% , `?GL_MIRRORED_REPEAT', or `?GL_REPEAT'. See the discussion under `?GL_TEXTURE_WRAP_S'
%% . Initially, `?GL_TEXTURE_WRAP_R' is set to `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_BORDER_COLOR':  The data in  `Params'  specifies four values that
%% define the border values that should be used for border texels. If a texel is sampled
%% from the border of the texture, the values of `?GL_TEXTURE_BORDER_COLOR' are interpreted
%% as an RGBA color to match the texture's internal format and substituted for the non-existent
%% texel data. If the texture contains depth components, the first component of `?GL_TEXTURE_BORDER_COLOR'
%%  is interpreted as a depth value. The initial value is (0.0,  0.0,  0.0,  0.0). 
%%
%% `?GL_TEXTURE_COMPARE_MODE':  Specifies the texture comparison mode for currently
%% bound textures. That is, a texture whose internal format is `?GL_DEPTH_COMPONENT_*';
%% see  {@link gl:texImage2D/9} ) Permissible values are: 
%%
%% `?GL_COMPARE_REF_TO_TEXTURE':  Specifies that the interpolated and clamped   r texture
%% coordinate should be compared to the value in the currently bound texture. See the discussion
%% of `?GL_TEXTURE_COMPARE_FUNC' for details of how the comparison is evaluated. The
%% result of the comparison is assigned to the red channel. 
%%
%% `?GL_NONE':  Specifies that the red channel should be assigned the appropriate value
%% from the currently bound texture. 
%%
%% `?GL_TEXTURE_COMPARE_FUNC':  Specifies the comparison operator used when `?GL_TEXTURE_COMPARE_MODE'
%%  is set to `?GL_COMPARE_REF_TO_TEXTURE'. Permissible values are: <table><tbody><tr><td>
%% ` Texture Comparison Function '</td><td>` Computed result  '</td></tr></tbody><tbody>
%% <tr><td>`?GL_LEQUAL'</td><td> result={1.0 0.0    r&lt;=(D t) r&gt;(D t))</td></tr><tr><td>`?GL_GEQUAL'</td><td>
%% result={1.0 0.0    r&gt;=(D t) r&lt;(D t))</td></tr><tr><td>`?GL_LESS'</td><td> result={1.0 0.0    r&lt;(D t) r&gt;=(D t))</td></tr><tr><td>`?GL_GREATER'
%% </td><td> result={1.0 0.0    r&gt;(D t) r&lt;=(D t))</td></tr><tr><td>`?GL_EQUAL'</td><td> result={1.0 0.0    r=(D t) r&amp;ne;
%% (D t))</td></tr><tr><td>`?GL_NOTEQUAL'
%% </td><td> result={1.0 0.0    r&amp;ne;(D t) r=(D t))</td></tr><tr><td>`?GL_ALWAYS'</td><td> result=1.0</td></tr><tr><td>
%% `?GL_NEVER'</td><td> result=0.0</td></tr></tbody></table> where  r is the current
%% interpolated texture coordinate, and   D t is the texture value sampled from the currently
%% bound texture.  result is assigned to  R t. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameter.xml">external</a> documentation.
-spec samplerParameteri(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: integer().
samplerParameteri(Sampler,Pname,Param) ->
  cast(5713, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLint>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameteriv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameteriv(Sampler,Pname,Param) ->
  cast(5714, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterf(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: float().
samplerParameterf(Sampler,Pname,Param) ->
  cast(5715, <<Sampler:?GLuint,Pname:?GLenum,Param:?GLfloat>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterfv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [float()].
samplerParameterfv(Sampler,Pname,Param) ->
  cast(5716, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @doc 
%% See {@link samplerParameteri/3}
-spec samplerParameterIiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIiv(Sampler,Pname,Param) ->
  cast(5717, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @doc glSamplerParameterI
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glSamplerParameterI.xml">external</a> documentation.
-spec samplerParameterIuiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIuiv(Sampler,Pname,Param) ->
  cast(5718, <<Sampler:?GLuint,Pname:?GLenum,(length(Param)):?GLuint,
        (<< <<C:?GLuint>> || C <- Param>>)/binary,0:(((1+length(Param)) rem 2)*32)>>).

%% @doc Return sampler parameter values
%%
%% ``gl:getSamplerParameter'' returns in  `Params'  the value or values of the sampler
%% parameter specified as  `Pname' .  `Sampler'  defines the target sampler, and must
%% be the name of an existing sampler object, returned from a previous call to  {@link gl:genSamplers/1} 
%% .  `Pname'  accepts the same symbols as  {@link gl:samplerParameteri/3} , with the same
%% interpretations: 
%%
%% `?GL_TEXTURE_MAG_FILTER':  Returns the single-valued texture magnification filter,
%% a symbolic constant. The initial value is `?GL_LINEAR'. 
%%
%% `?GL_TEXTURE_MIN_FILTER':  Returns the single-valued texture minification filter,
%% a symbolic constant. The initial value is `?GL_NEAREST_MIPMAP_LINEAR'. 
%%
%% `?GL_TEXTURE_MIN_LOD':  Returns the single-valued texture minimum level-of-detail
%% value. The initial value is   -1000. 
%%
%% `?GL_TEXTURE_MAX_LOD':  Returns the single-valued texture maximum level-of-detail
%% value. The initial value is 1000. 
%%
%% `?GL_TEXTURE_WRAP_S':  Returns the single-valued wrapping function for texture coordinate
%%   s, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_WRAP_T':  Returns the single-valued wrapping function for texture coordinate
%%   t, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_WRAP_R':  Returns the single-valued wrapping function for texture coordinate
%%   r, a symbolic constant. The initial value is `?GL_REPEAT'. 
%%
%% `?GL_TEXTURE_BORDER_COLOR':  Returns four integer or floating-point numbers that
%% comprise the RGBA color of the texture border. Floating-point values are returned in the
%% range  [0 1]. Integer values are returned as a linear mapping of the internal floating-point
%% representation such that 1.0 maps to the most positive representable integer and   -1.0
%% maps to the most negative representable integer. The initial value is (0, 0, 0, 0). 
%%
%% `?GL_TEXTURE_COMPARE_MODE':  Returns a single-valued texture comparison mode, a symbolic
%% constant. The initial value is `?GL_NONE'. See  {@link gl:samplerParameteri/3} . 
%%
%% `?GL_TEXTURE_COMPARE_FUNC':  Returns a single-valued texture comparison function,
%% a symbolic constant. The initial value is `?GL_LEQUAL'. See  {@link gl:samplerParameteri/3} 
%% . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameter.xml">external</a> documentation.
-spec getSamplerParameteriv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameteriv(Sampler,Pname) ->
  call(5719, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getSamplerParameteriv/2}
-spec getSamplerParameterIiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIiv(Sampler,Pname) ->
  call(5720, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc 
%% See {@link getSamplerParameteriv/2}
-spec getSamplerParameterfv(Sampler, Pname) -> [float()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterfv(Sampler,Pname) ->
  call(5721, <<Sampler:?GLuint,Pname:?GLenum>>).

%% @doc glGetSamplerParameterI
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSamplerParameterI.xml">external</a> documentation.
-spec getSamplerParameterIuiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIuiv(Sampler,Pname) ->
  call(5722, <<Sampler:?GLuint,Pname:?GLenum>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glQueryCounter.xml">external</a> documentation.
-spec queryCounter(Id, Target) -> 'ok' when Id :: integer(),Target :: enum().
queryCounter(Id,Target) ->
  cast(5723, <<Id:?GLuint,Target:?GLenum>>).

%% @doc glGetQueryObjecti64v
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObjecti64v.xml">external</a> documentation.
-spec getQueryObjecti64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjecti64v(Id,Pname) ->
  call(5724, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc glGetQueryObjectui64v
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryObjectui64v.xml">external</a> documentation.
-spec getQueryObjectui64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectui64v(Id,Pname) ->
  call(5725, <<Id:?GLuint,Pname:?GLenum>>).

%% @doc Render primitives from array data, taking parameters from memory
%%
%% ``gl:drawArraysIndirect'' specifies multiple geometric primitives with very few subroutine
%% calls. ``gl:drawArraysIndirect'' behaves similarly to  {@link gl:drawArraysInstancedBaseInstance/5} 
%% , execept that the parameters to  {@link gl:drawArraysInstancedBaseInstance/5}  are stored
%% in memory at the address given by  `Indirect' . 
%%
%%  The parameters addressed by  `Indirect'  are packed into a structure that takes the
%% form (in C):  typedef struct { uint count; uint primCount; uint first; uint baseInstance;
%% } DrawArraysIndirectCommand;  const DrawArraysIndirectCommand *cmd = (const DrawArraysIndirectCommand
%% *)indirect; glDrawArraysInstancedBaseInstance(mode, cmd-&gt;first, cmd-&gt;count, cmd-&gt;primCount,
%% cmd-&gt;baseInstance);
%%
%%  If a buffer is bound to the `?GL_DRAW_INDIRECT_BUFFER' binding at the time of a
%% call to ``gl:drawArraysIndirect'',  `Indirect'  is interpreted as an offset, in basic
%% machine units, into that buffer and the parameter data is read from the buffer rather
%% than from client memory. 
%%
%%  In contrast to  {@link gl:drawArraysInstancedBaseInstance/5} , the first member of the parameter
%% structure is unsigned, and out-of-range indices do not generate an error. 
%%
%%  Vertex attributes that are modified by ``gl:drawArraysIndirect'' have an unspecified
%% value after ``gl:drawArraysIndirect'' returns. Attributes that aren't modified remain
%% well defined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysIndirect.xml">external</a> documentation.
-spec drawArraysIndirect(Mode, Indirect) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem().
drawArraysIndirect(Mode,Indirect) when  is_integer(Indirect) ->
  cast(5726, <<Mode:?GLenum,Indirect:?GLuint>>);
drawArraysIndirect(Mode,Indirect) ->
  send_bin(Indirect),
  cast(5727, <<Mode:?GLenum>>).

%% @doc Render indexed primitives from array data, taking parameters from memory
%%
%% ``gl:drawElementsIndirect'' specifies multiple indexed geometric primitives with very
%% few subroutine calls. ``gl:drawElementsIndirect'' behaves similarly to  {@link gl:drawElementsInstancedBaseVertexBaseInstance/7} 
%% , execpt that the parameters to  {@link gl:drawElementsInstancedBaseVertexBaseInstance/7} 
%% are stored in memory at the address given by  `Indirect' . 
%%
%%  The parameters addressed by  `Indirect'  are packed into a structure that takes the
%% form (in C):  typedef struct { uint count; uint primCount; uint firstIndex; uint baseVertex;
%% uint baseInstance; } DrawElementsIndirectCommand;
%%
%% ``gl:drawElementsIndirect'' is equivalent to:  void glDrawElementsIndirect(GLenum mode,
%% GLenum type, const void * indirect) { const DrawElementsIndirectCommand *cmd = (const
%% DrawElementsIndirectCommand *)indirect; glDrawElementsInstancedBaseVertexBaseInstance(mode,
%% cmd-&gt;count, type, cmd-&gt;firstIndex + size-of-type, cmd-&gt;primCount, cmd-&gt;baseVertex,
%% cmd-&gt;baseInstance); }
%%
%%  If a buffer is bound to the `?GL_DRAW_INDIRECT_BUFFER' binding at the time of a
%% call to ``gl:drawElementsIndirect'',  `Indirect'  is interpreted as an offset, in
%% basic machine units, into that buffer and the parameter data is read from the buffer rather
%% than from client memory. 
%%
%%  Note that indices stored in client memory are not supported. If no buffer is bound to
%% the `?GL_ELEMENT_ARRAY_BUFFER' binding, an error will be generated. 
%%
%%  The results of the operation are undefined if the reservedMustBeZero member of the parameter
%% structure is non-zero. However, no error is generated in this case. 
%%
%%  Vertex attributes that are modified by ``gl:drawElementsIndirect'' have an unspecified
%% value after ``gl:drawElementsIndirect'' returns. Attributes that aren't modified remain
%% well defined. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsIndirect.xml">external</a> documentation.
-spec drawElementsIndirect(Mode, Type, Indirect) -> 'ok' when Mode :: enum(),Type :: enum(),Indirect :: offset()|mem().
drawElementsIndirect(Mode,Type,Indirect) when  is_integer(Indirect) ->
  cast(5728, <<Mode:?GLenum,Type:?GLenum,Indirect:?GLuint>>);
drawElementsIndirect(Mode,Type,Indirect) ->
  send_bin(Indirect),
  cast(5729, <<Mode:?GLenum,Type:?GLenum>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1d(Location, X) -> 'ok' when Location :: integer(),X :: float().
uniform1d(Location,X) ->
  cast(5730, <<Location:?GLint,0:32,X:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2d(Location, X, Y) -> 'ok' when Location :: integer(),X :: float(),Y :: float().
uniform2d(Location,X,Y) ->
  cast(5731, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3d(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float().
uniform3d(Location,X,Y,Z) ->
  cast(5732, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4d(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
uniform4d(Location,X,Y,Z,W) ->
  cast(5733, <<Location:?GLint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform1dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1dv(Location,Value) ->
  cast(5734, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform2dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2dv(Location,Value) ->
  cast(5735, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform3dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3dv(Location,Value) ->
  cast(5736, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniform4dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4dv(Location,Value) ->
  cast(5737, <<Location:?GLint,0:32,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2dv(Location,Transpose,Value) ->
  cast(5738, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3dv(Location,Transpose,Value) ->
  cast(5739, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4dv(Location,Transpose,Value) ->
  cast(5740, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3dv(Location,Transpose,Value) ->
  cast(5741, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix2x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4dv(Location,Transpose,Value) ->
  cast(5742, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2dv(Location,Transpose,Value) ->
  cast(5743, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4dv(Location,Transpose,Value) ->
  cast(5744, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2dv(Location,Transpose,Value) ->
  cast(5745, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link uniform1f/2}
-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3dv(Location,Transpose,Value) ->
  cast(5746, <<Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link getUniformfv/2}
-spec getUniformdv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformdv(Program,Location) ->
  call(5747, <<Program:?GLuint,Location:?GLint>>).

%% @doc Retrieve the location of a subroutine uniform of a given shader stage within a program
%%
%% ``gl:getSubroutineUniformLocation'' returns the location of the subroutine uniform variable
%%  `Name'  in the shader stage of type  `Shadertype'  attached to  `Program' ,
%% with behavior otherwise identical to  {@link gl:getUniformLocation/2} . 
%%
%%  If  `Name'  is not the name of a subroutine uniform in the shader stage, -1 is returned,
%% but no error is generated. If  `Name'  is the name of a subroutine uniform in the shader
%% stage, a value between zero and the value of `?GL_ACTIVE_SUBROUTINE_LOCATIONS' minus
%% one will be returned. Subroutine locations are assigned using consecutive integers in
%% the range from zero to the value of `?GL_ACTIVE_SUBROUTINE_LOCATIONS' minus one for
%% the shader stage. For active subroutine uniforms declared as arrays, the declared array
%% elements are assigned consecutive locations. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSubroutineUniformLocation.xml">external</a> documentation.
-spec getSubroutineUniformLocation(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineUniformLocation(Program,Shadertype,Name) ->
  call(5748, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc Retrieve the index of a subroutine uniform of a given shader stage within a program
%%
%% ``gl:getSubroutineIndex'' returns the index of a subroutine uniform within a shader
%% stage attached to a program object.  `Program'  contains the name of the program to
%% which the shader is attached.  `Shadertype'  specifies the stage from which to query
%% shader subroutine index.  `Name'  contains the null-terminated name of the subroutine
%% uniform whose name to query. 
%%
%%  If  `Name'  is not the name of a subroutine uniform in the shader stage, `?GL_INVALID_INDEX'
%%  is returned, but no error is generated. If  `Name'  is the name of a subroutine uniform
%% in the shader stage, a value between zero and the value of `?GL_ACTIVE_SUBROUTINES'
%% minus one will be returned. Subroutine indices are assigned using consecutive integers
%% in the range from zero to the value of `?GL_ACTIVE_SUBROUTINES' minus one for the
%% shader stage. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetSubroutineIndex.xml">external</a> documentation.
-spec getSubroutineIndex(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineIndex(Program,Shadertype,Name) ->
  call(5749, <<Program:?GLuint,Shadertype:?GLenum,(list_to_binary([Name|[0]]))/binary,0:((8-((length(Name)+ 1) rem 8)) rem 8)>>).

%% @doc Query the name of an active shader subroutine uniform
%%
%% ``gl:getActiveSubroutineUniformName'' retrieves the name of an active shader subroutine
%% uniform.  `Program'  contains the name of the program containing the uniform.  `Shadertype' 
%%  specifies the stage for which which the uniform location, given by  `Index' , is valid.
%%  `Index'  must be between zero and the value of `?GL_ACTIVE_SUBROUTINE_UNIFORMS'
%% minus one for the shader stage. 
%%
%%  The uniform name is returned as a null-terminated string in  `Name' . The actual number
%% of characters written into  `Name' , excluding the null terminator is returned in  `Length' 
%% . If  `Length'  is `?NULL', no length is returned. The maximum number of characters
%% that may be written into  `Name' , including the null terminator, is specified by  `Bufsize' 
%% . The length of the longest subroutine uniform name in  `Program'  and  `Shadertype' 
%%  is given by the value of `?GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH', which can be
%% queried with  {@link gl:getProgramStageiv/3} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveSubroutineUniformName.xml">external</a> documentation.
-spec getActiveSubroutineUniformName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) ->
  call(5750, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @doc Query the name of an active shader subroutine
%%
%% ``gl:getActiveSubroutineName'' queries the name of an active shader subroutine uniform
%% from the program object given in  `Program' .  `Index'  specifies the index of the
%% shader subroutine uniform within the shader stage given by  `Stage' , and must between
%% zero and the value of `?GL_ACTIVE_SUBROUTINES' minus one for the shader stage. 
%%
%%  The name of the selected subroutine is returned as a null-terminated string in  `Name' 
%% . The actual number of characters written into  `Name' , not including the null-terminator,
%% is is returned in  `Length' . If  `Length'  is `?NULL', no length is returned.
%% The maximum number of characters that may be written into  `Name' , including the null-terminator,
%% is given in  `Bufsize' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetActiveSubroutineName.xml">external</a> documentation.
-spec getActiveSubroutineName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) ->
  call(5751, <<Program:?GLuint,Shadertype:?GLenum,Index:?GLuint,Bufsize:?GLsizei>>).

%% @doc Load active subroutine uniforms
%%
%% ``gl:uniformSubroutines'' loads all active subroutine uniforms for shader stage  `Shadertype' 
%%  of the current program with subroutine indices from  `Indices' , storing  `Indices[i]' 
%%  into the uniform at location  `I' .  `Count'  must be equal to the value of `?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS'
%%  for the program currently in use at shader stage  `Shadertype' . Furthermore, all
%% values in  `Indices'  must be less than the value of `?GL_ACTIVE_SUBROUTINES'
%% for the shader stage. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUniformSubroutines.xml">external</a> documentation.
-spec uniformSubroutinesuiv(Shadertype, Indices) -> 'ok' when Shadertype :: enum(),Indices :: [integer()].
uniformSubroutinesuiv(Shadertype,Indices) ->
  cast(5752, <<Shadertype:?GLenum,(length(Indices)):?GLuint,
        (<< <<C:?GLuint>> || C <- Indices>>)/binary,0:(((length(Indices)) rem 2)*32)>>).

%% @doc Retrieve the value of a subroutine uniform of a given shader stage of the current program
%%
%% ``gl:getUniformSubroutine'' retrieves the value of the subroutine uniform at location  `Location' 
%%  for shader stage  `Shadertype'  of the current program.  `Location'  must be less
%% than the value of `?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS' for the shader currently
%% in use at shader stage  `Shadertype' . The value of the subroutine uniform is returned
%% in  `Values' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetUniformSubroutine.xml">external</a> documentation.
-spec getUniformSubroutineuiv(Shadertype, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Shadertype :: enum(),Location :: integer().
getUniformSubroutineuiv(Shadertype,Location) ->
  call(5753, <<Shadertype:?GLenum,Location:?GLint>>).

%% @doc Retrieve properties of a program object corresponding to a specified shader stage
%%
%% ``gl:getProgramStage'' queries a parameter of a shader stage attached to a program object.
%%  `Program'  contains the name of the program to which the shader is attached.  `Shadertype' 
%%  specifies the stage from which to query the parameter.  `Pname'  specifies which parameter
%% should be queried. The value or values of the parameter to be queried is returned in the
%% variable whose address is given in  `Values' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_SUBROUTINE_UNIFORMS', the number of active subroutine
%% variables in the stage is returned in  `Values' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS', the number of active
%% subroutine variable locations in the stage is returned in  `Values' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_SUBROUTINES', the number of active subroutines in
%% the stage is returned in  `Values' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH', the length of the
%% longest subroutine uniform for the stage is returned in  `Values' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_SUBROUTINE_MAX_LENGTH', the length of the longest
%% subroutine name for the stage is returned in  `Values' . The returned name length includes
%% space for the null-terminator. 
%%
%%  If there is no shader present of type  `Shadertype' , the returned value will be consistent
%% with a shader containing no subroutines or subroutine uniforms. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramStage.xml">external</a> documentation.
-spec getProgramStageiv(Program, Shadertype, Pname) -> integer() when Program :: integer(),Shadertype :: enum(),Pname :: enum().
getProgramStageiv(Program,Shadertype,Pname) ->
  call(5754, <<Program:?GLuint,Shadertype:?GLenum,Pname:?GLenum>>).

%% @doc Specifies the parameters for patch primitives
%%
%% ``gl:patchParameter'' specifies the parameters that will be used for patch primitives.  `Pname' 
%%  specifies the parameter to modify and must be either `?GL_PATCH_VERTICES', `?GL_PATCH_DEFAULT_OUTER_LEVEL'
%%  or `?GL_PATCH_DEFAULT_INNER_LEVEL'. For ``gl:patchParameteri'',  `Value'  specifies
%% the new value for the parameter specified by  `Pname' . For ``gl:patchParameterfv'',
%%  `Values'  specifies the address of an array containing the new values for the parameter
%% specified by  `Pname' . 
%%
%%  When  `Pname'  is `?GL_PATCH_VERTICES',  `Value'  specifies the number of
%% vertices that will be used to make up a single patch primitive. Patch primitives are consumed
%% by the tessellation control shader (if present) and subsequently used for tessellation.
%% When primitives are specified using  {@link gl:drawArrays/3}  or a similar function, each
%% patch will be made from  `Parameter'  control points, each represented by a vertex
%% taken from the enabeld vertex arrays.  `Parameter'  must be greater than zero, and
%% less than or equal to the value of `?GL_MAX_PATCH_VERTICES'. 
%%
%%  When  `Pname'  is `?GL_PATCH_DEFAULT_OUTER_LEVEL' or `?GL_PATCH_DEFAULT_INNER_LEVEL'
%% ,  `Values'  contains the address of an array contiaining the default outer or inner
%% tessellation levels, respectively, to be used when no tessellation control shader is present.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPatchParameter.xml">external</a> documentation.
-spec patchParameteri(Pname, Value) -> 'ok' when Pname :: enum(),Value :: integer().
patchParameteri(Pname,Value) ->
  cast(5755, <<Pname:?GLenum,Value:?GLint>>).

%% @doc 
%% See {@link patchParameteri/2}
-spec patchParameterfv(Pname, Values) -> 'ok' when Pname :: enum(),Values :: [float()].
patchParameterfv(Pname,Values) ->
  cast(5756, <<Pname:?GLenum,(length(Values)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Values>>)/binary,0:(((length(Values)) rem 2)*32)>>).

%% @doc Bind a transform feedback object
%%
%% ``gl:bindTransformFeedback'' binds the transform feedback object with name  `Id' 
%% to the current GL state.  `Id'  must be a name previously returned from a call to  {@link gl:genTransformFeedbacks/1} 
%% . If  `Id'  has not previously been bound, a new transform feedback object with name  `Id' 
%%  and initialized with with the default transform state vector is created. 
%%
%%  In the initial state, a default transform feedback object is bound and treated as a transform
%% feedback object with a name of zero. If the name zero is subsequently bound, the default
%% transform feedback object is again bound to the GL state. 
%%
%%  While a transform feedback buffer object is bound, GL operations on the target to which
%% it is bound affect the bound transform feedback object, and queries of the target to which
%% a transform feedback object is bound return state from the bound object. When buffer objects
%% are bound for transform feedback, they are attached to the currently bound transform feedback
%% object. Buffer objects are used for trans- form feedback only if they are attached to
%% the currently bound transform feedback object. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindTransformFeedback.xml">external</a> documentation.
-spec bindTransformFeedback(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
bindTransformFeedback(Target,Id) ->
  cast(5757, <<Target:?GLenum,Id:?GLuint>>).

%% @doc Delete transform feedback objects
%%
%% ``gl:deleteTransformFeedbacks'' deletes the  `N'  transform feedback objects whose
%% names are stored in the array  `Ids' . Unused names in  `Ids'  are ignored, as is
%% the name zero. After a transform feedback object is deleted, its name is again unused
%% and it has no contents. If an active transform feedback object is deleted, its name immediately
%% becomes unused, but the underlying object is not deleted until it is no longer active. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteTransformFeedbacks.xml">external</a> documentation.
-spec deleteTransformFeedbacks(Ids) -> 'ok' when Ids :: [integer()].
deleteTransformFeedbacks(Ids) ->
  cast(5758, <<(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((1+length(Ids)) rem 2)*32)>>).

%% @doc Reserve transform feedback object names
%%
%% ``gl:genTransformFeedbacks'' returns  `N'  previously unused transform feedback object
%% names in  `Ids' . These names are marked as used, for the purposes of ``gl:genTransformFeedbacks''
%%  only, but they acquire transform feedback state only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenTransformFeedbacks.xml">external</a> documentation.
-spec genTransformFeedbacks(N) -> [integer()] when N :: integer().
genTransformFeedbacks(N) ->
  call(5759, <<N:?GLsizei>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsTransformFeedback.xml">external</a> documentation.
-spec isTransformFeedback(Id) -> 0|1 when Id :: integer().
isTransformFeedback(Id) ->
  call(5760, <<Id:?GLuint>>).

%% @doc Pause transform feedback operations
%%
%% ``gl:pauseTransformFeedback'' pauses transform feedback operations on the currently
%% active transform feedback object. When transform feedback operations are paused, transform
%% feedback is still considered active and changing most transform feedback state related
%% to the object results in an error. However, a new transform feedback object may be bound
%% while transform feedback is paused. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glPauseTransformFeedback.xml">external</a> documentation.
-spec pauseTransformFeedback() -> 'ok'.
pauseTransformFeedback() ->
  cast(5761, <<>>).

%% @doc Resume transform feedback operations
%%
%% ``gl:resumeTransformFeedback'' resumes transform feedback operations on the currently
%% active transform feedback object. When transform feedback operations are paused, transform
%% feedback is still considered active and changing most transform feedback state related
%% to the object results in an error. However, a new transform feedback object may be bound
%% while transform feedback is paused. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glResumeTransformFeedback.xml">external</a> documentation.
-spec resumeTransformFeedback() -> 'ok'.
resumeTransformFeedback() ->
  cast(5762, <<>>).

%% @doc Render primitives using a count derived from a transform feedback object
%%
%% ``gl:drawTransformFeedback'' draws primitives of a type specified by  `Mode'  using
%% a count retrieved from the transform feedback specified by  `Id' . Calling ``gl:drawTransformFeedback''
%%  is equivalent to calling  {@link gl:drawArrays/3}  with  `Mode'  as specified,  `First' 
%%  set to zero, and  `Count'  set to the number of vertices captured on vertex stream
%% zero the last time transform feedback was active on the transform feedback object named
%% by  `Id' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedback.xml">external</a> documentation.
-spec drawTransformFeedback(Mode, Id) -> 'ok' when Mode :: enum(),Id :: integer().
drawTransformFeedback(Mode,Id) ->
  cast(5763, <<Mode:?GLenum,Id:?GLuint>>).

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
%%  Calling  {@link gl:drawTransformFeedback/2}  is equivalent to calling ``gl:drawTransformFeedbackStream''
%%  with  `Stream'  set to zero. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedbackStream.xml">external</a> documentation.
-spec drawTransformFeedbackStream(Mode, Id, Stream) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer().
drawTransformFeedbackStream(Mode,Id,Stream) ->
  cast(5764, <<Mode:?GLenum,Id:?GLuint,Stream:?GLuint>>).

%% @doc glBeginQueryIndexe
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQueryIndexe.xml">external</a> documentation.
-spec beginQueryIndexed(Target, Index, Id) -> 'ok' when Target :: enum(),Index :: integer(),Id :: integer().
beginQueryIndexed(Target,Index,Id) ->
  cast(5765, <<Target:?GLenum,Index:?GLuint,Id:?GLuint>>).

%% @doc Delimit the boundaries of a query object on an indexed target
%%
%% ``gl:beginQueryIndexed'' and  {@link gl:endQueryIndexed/2}  delimit the boundaries of a
%% query object.  `Query'  must be a name previously returned from a call to  {@link gl:genQueries/1} 
%% . If a query object with name  `Id'  does not yet exist it is created with the type
%% determined by  `Target' .  `Target'  must be one of `?GL_SAMPLES_PASSED', `?GL_ANY_SAMPLES_PASSED'
%% , `?GL_PRIMITIVES_GENERATED', `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN', or `?GL_TIME_ELAPSED'
%% . The behavior of the query object depends on its type and is as follows. 
%%
%%  `Index'  specifies the index of the query target and must be between a  `Target' -specific
%% maximum. 
%%
%%  If  `Target'  is `?GL_SAMPLES_PASSED',  `Id'  must be an unused name, or the
%% name of an existing occlusion query object. When ``gl:beginQueryIndexed'' is executed,
%% the query object's samples-passed counter is reset to 0. Subsequent rendering will increment
%% the counter for every sample that passes the depth test. If the value of `?GL_SAMPLE_BUFFERS'
%%  is 0, then the samples-passed count is incremented by 1 for each fragment. If the value
%% of `?GL_SAMPLE_BUFFERS' is 1, then the samples-passed count is incremented by the
%% number of samples whose coverage bit is set. However, implementations, at their discression
%% may instead increase the samples-passed count by the value of `?GL_SAMPLES' if any
%% sample in the fragment is covered. When ``gl:endQueryIndexed'' is executed, the samples-passed
%% counter is assigned to the query object's result value. This value can be queried by calling
%%  {@link gl:getQueryObjectiv/2}  with  `Pname' `?GL_QUERY_RESULT'. When  `Target' 
%% is `?GL_SAMPLES_PASSED',  `Index'  must be zero. 
%%
%%  If  `Target'  is `?GL_ANY_SAMPLES_PASSED',  `Id'  must be an unused name,
%% or the name of an existing boolean occlusion query object. When ``gl:beginQueryIndexed''
%% is executed, the query object's samples-passed flag is reset to `?GL_FALSE'. Subsequent
%% rendering causes the flag to be set to `?GL_TRUE' if any sample passes the depth
%% test. When ``gl:endQueryIndexed'' is executed, the samples-passed flag is assigned to
%% the query object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%%  with  `Pname' `?GL_QUERY_RESULT'. When  `Target'  is `?GL_ANY_SAMPLES_PASSED'
%% ,  `Index'  must be zero. 
%%
%%  If  `Target'  is `?GL_PRIMITIVES_GENERATED',  `Id'  must be an unused name,
%% or the name of an existing primitive query object previously bound to the `?GL_PRIMITIVES_GENERATED'
%%  query binding. When ``gl:beginQueryIndexed'' is executed, the query object's primitives-generated
%% counter is reset to 0. Subsequent rendering will increment the counter once for every
%% vertex that is emitted from the geometry shader to the stream given by  `Index' , or
%% from the vertex shader if  `Index'  is zero and no geometry shader is present. When ``gl:endQueryIndexed''
%%  is executed, the primitives-generated counter for stream  `Index'  is assigned to
%% the query object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%%  with  `Pname' `?GL_QUERY_RESULT'. When  `Target'  is `?GL_PRIMITIVES_GENERATED'
%% ,  `Index'  must be less than the value of `?GL_MAX_VERTEX_STREAMS'. 
%%
%%  If  `Target'  is `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN',  `Id'  must
%% be an unused name, or the name of an existing primitive query object previously bound
%% to the `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN' query binding. When ``gl:beginQueryIndexed''
%%  is executed, the query object's primitives-written counter for the stream specified by  `Index' 
%%  is reset to 0. Subsequent rendering will increment the counter once for every vertex
%% that is written into the bound transform feedback buffer(s) for stream  `Index' . If
%% transform feedback mode is not activated between the call to ``gl:beginQueryIndexed''
%% and ``gl:endQueryIndexed'', the counter will not be incremented. When ``gl:endQueryIndexed''
%%  is executed, the primitives-written counter for stream  `Index'  is assigned to the
%% query object's result value. This value can be queried by calling  {@link gl:getQueryObjectiv/2} 
%%  with  `Pname' `?GL_QUERY_RESULT'. When  `Target'  is `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN'
%% ,  `Index'  must be less than the value of `?GL_MAX_VERTEX_STREAMS'. 
%%
%%  If  `Target'  is `?GL_TIME_ELAPSED',  `Id'  must be an unused name, or the
%% name of an existing timer query object previously bound to the `?GL_TIME_ELAPSED'
%% query binding. When ``gl:beginQueryIndexed'' is executed, the query object's time counter
%% is reset to 0. When ``gl:endQueryIndexed'' is executed, the elapsed server time that
%% has passed since the call to ``gl:beginQueryIndexed'' is written into the query object's
%% time counter. This value can be queried by calling  {@link gl:getQueryObjectiv/2}  with  `Pname' 
%% `?GL_QUERY_RESULT'. When  `Target'  is `?GL_TIME_ELAPSED',  `Index'  must
%% be zero. 
%%
%%  Querying the `?GL_QUERY_RESULT' implicitly flushes the GL pipeline until the rendering
%% delimited by the query object has completed and the result is available. `?GL_QUERY_RESULT_AVAILABLE'
%%  can be queried to determine if the result is immediately available or if the rendering
%% is not yet complete. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBeginQueryIndexed.xml">external</a> documentation.
-spec endQueryIndexed(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
endQueryIndexed(Target,Index) ->
  cast(5766, <<Target:?GLenum,Index:?GLuint>>).

%% @doc Return parameters of an indexed query object target
%%
%% ``gl:getQueryIndexediv'' returns in  `Params'  a selected parameter of the indexed
%% query object target specified by  `Target'  and  `Index' .  `Index'  specifies
%% the index of the query object target and must be between zero and a target-specific maxiumum.
%% 
%%
%%  `Pname'  names a specific query object target parameter. When  `Pname'  is `?GL_CURRENT_QUERY'
%% , the name of the currently active query for the specified  `Index'  of  `Target' ,
%% or zero if no query is active, will be placed in  `Params' . If  `Pname'  is `?GL_QUERY_COUNTER_BITS'
%% , the implementation-dependent number of bits used to hold the result of queries for  `Target' 
%%  is returned in  `Params' . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetQueryIndexed.xml">external</a> documentation.
-spec getQueryIndexediv(Target, Index, Pname) -> integer() when Target :: enum(),Index :: integer(),Pname :: enum().
getQueryIndexediv(Target,Index,Pname) ->
  call(5767, <<Target:?GLenum,Index:?GLuint,Pname:?GLenum>>).

%% @doc Release resources consumed by the implementation's shader compiler
%%
%% ``gl:releaseShaderCompiler'' provides a hint to the implementation that it may free
%% internal resources associated with its shader compiler.  {@link gl:compileShader/1}  may
%% subsequently be called and the implementation may at that time reallocate resources previously
%% freed by the call to ``gl:releaseShaderCompiler''. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glReleaseShaderCompiler.xml">external</a> documentation.
-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler() ->
  cast(5768, <<>>).

%% @doc Load pre-compiled shader binaries
%%
%% ``gl:shaderBinary'' loads pre-compiled shader binary code into the  `Count'  shader
%% objects whose handles are given in  `Shaders' .  `Binary'  points to  `Length' 
%% bytes of binary shader code stored in client memory.  `BinaryFormat'  specifies the
%% format of the pre-compiled code. 
%%
%%  The binary image contained in  `Binary'  will be decoded according to the extension
%% specification defining the specified  `BinaryFormat'  token. OpenGL does not define
%% any specific binary formats, but it does provide a mechanism to obtain token vaues for
%% such formats provided by such extensions. 
%%
%%  Depending on the types of the shader objects in  `Shaders' , ``gl:shaderBinary''
%% will individually load binary vertex or fragment shaders, or load an executable binary
%% that contains an optimized pair of vertex and fragment shaders stored in the same binary.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glShaderBinary.xml">external</a> documentation.
-spec shaderBinary(Shaders, Binaryformat, Binary) -> 'ok' when Shaders :: [integer()],Binaryformat :: enum(),Binary :: binary().
shaderBinary(Shaders,Binaryformat,Binary) ->
  send_bin(Binary),
  cast(5769, <<(length(Shaders)):?GLuint,
        (<< <<C:?GLuint>> || C <- Shaders>>)/binary,0:(((1+length(Shaders)) rem 2)*32),Binaryformat:?GLenum>>).

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
%%  `Range'  points to an array of two integers into which the format's numeric range
%% will be returned. If min and max are the smallest values representable in the format,
%% then the values returned are defined to be:  `Range' [0] = floor(log2(|min|)) and  `Range' 
%% [1] = floor(log2(|max|)). 
%%
%%  `Precision'  specifies the address of an integer into which will be written the log2
%% value of the number of bits of precision of the format. If the smallest representable
%% value greater than 1 is 1 + `eps', then the integer addressed by  `Precision' 
%% will contain floor(-log2(eps)). 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetShaderPrecisionFormat.xml">external</a> documentation.
-spec getShaderPrecisionFormat(Shadertype, Precisiontype) -> {Range :: {integer(),integer()},Precision :: integer()} when Shadertype :: enum(),Precisiontype :: enum().
getShaderPrecisionFormat(Shadertype,Precisiontype) ->
  call(5770, <<Shadertype:?GLenum,Precisiontype:?GLenum>>).

%% @doc 
%% See {@link depthRange/2}
-spec depthRangef(N, F) -> 'ok' when N :: clamp(),F :: clamp().
depthRangef(N,F) ->
  cast(5771, <<N:?GLclampf,F:?GLclampf>>).

%% @doc glClearDepthf
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glClearDepthf.xml">external</a> documentation.
-spec clearDepthf(D) -> 'ok' when D :: clamp().
clearDepthf(D) ->
  cast(5772, <<D:?GLclampf>>).

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
%%  The format of the program binary written into  `Binary'  is returned in the variable
%% whose address is given by  `BinaryFormat' , and may be implementation dependent. The
%% binary produced by the GL may subsequently be returned to the GL by calling  {@link gl:programBinary/3} 
%% , with  `BinaryFormat'  and  `Length'  set to the values returned by ``gl:getProgramBinary''
%% , and passing the returned binary data in the  `Binary'  parameter. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramBinary.xml">external</a> documentation.
-spec getProgramBinary(Program, BufSize) -> {BinaryFormat :: enum(),Binary :: binary()} when Program :: integer(),BufSize :: integer().
getProgramBinary(Program,BufSize) ->
  call(5773, <<Program:?GLuint,BufSize:?GLsizei>>).

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
%%  A program object's program binary is replaced by calls to  {@link gl:linkProgram/1}  or ``gl:programBinary''
%% . When linking success or failure is concerned, ``gl:programBinary'' can be considered
%% to perform an implicit linking operation.  {@link gl:linkProgram/1}  and ``gl:programBinary''
%%  both set the program object's `?GL_LINK_STATUS' to `?GL_TRUE' or `?GL_FALSE'
%% . 
%%
%%  A successful call to ``gl:programBinary'' will reset all uniform variables to their
%% initial values. The initial value is either the value of the variable's initializer as
%% specified in the original shader source, or zero if no initializer was present. Additionally,
%% all vertex shader input and fragment shader output assignments that were in effect when
%% the program was linked before saving are restored with ``gl:programBinary'' is called. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramBinary.xml">external</a> documentation.
-spec programBinary(Program, BinaryFormat, Binary) -> 'ok' when Program :: integer(),BinaryFormat :: enum(),Binary :: binary().
programBinary(Program,BinaryFormat,Binary) ->
  send_bin(Binary),
  cast(5774, <<Program:?GLuint,BinaryFormat:?GLenum>>).

%% @doc Specify a parameter for a program object
%%
%% ``gl:programParameter'' specifies a new value for the parameter nameed by  `Pname' 
%% for the program object  `Program' . 
%%
%%  If  `Pname'  is `?GL_PROGRAM_BINARY_RETRIEVABLE_HINT',  `Value'  should be `?GL_FALSE'
%%  or `?GL_TRUE' to indicate to the implementation the intention of the application
%% to retrieve the program's binary representation with  {@link gl:getProgramBinary/2} . The
%% implementation may use this information to store information that may be useful for a
%% future query of the program's binary. It is recommended to set `?GL_PROGRAM_BINARY_RETRIEVABLE_HINT'
%%  for the program to `?GL_TRUE' before calling  {@link gl:linkProgram/1} , and using
%% the program at run-time if the binary is to be retrieved later. 
%%
%%  If  `Pname'  is `?GL_PROGRAM_SEPARABLE',  `Value'  must be `?GL_TRUE'
%% or `?GL_FALSE' and indicates whether  `Program'  can be bound to individual pipeline
%% stages via  {@link gl:useProgramStages/3} . A program's `?GL_PROGRAM_SEPARABLE' parameter
%% must be set to `?GL_TRUE'`before' {@link gl:linkProgram/1}  is called in order
%% for it to be usable with a program pipeline object. The initial state of `?GL_PROGRAM_SEPARABLE'
%%  is `?GL_FALSE'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramParameter.xml">external</a> documentation.
-spec programParameteri(Program, Pname, Value) -> 'ok' when Program :: integer(),Pname :: enum(),Value :: integer().
programParameteri(Program,Pname,Value) ->
  cast(5775, <<Program:?GLuint,Pname:?GLenum,Value:?GLint>>).

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
%%  If  `Program'  refers to a program object with a valid shader attached for an indicated
%% shader stage, ``gl:useProgramStages'' installs the executable code for that stage in
%% the indicated program pipeline object  `Pipeline' . If  `Program'  is zero, or refers
%% to a program object with no valid shader executable for a given stage, it is as if the
%% pipeline object has no programmable stage configured for the indicated shader stages. If  `Stages' 
%%  contains bits other than those listed above, and is not equal to `?GL_ALL_SHADER_BITS'
%% , an error is generated. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glUseProgramStages.xml">external</a> documentation.
-spec useProgramStages(Pipeline, Stages, Program) -> 'ok' when Pipeline :: integer(),Stages :: integer(),Program :: integer().
useProgramStages(Pipeline,Stages,Program) ->
  cast(5776, <<Pipeline:?GLuint,Stages:?GLbitfield,Program:?GLuint>>).

%% @doc Set the active program object for a program pipeline object
%%
%% ``gl:activeShaderProgram'' sets the linked program named by  `Program'  to be the
%% active program for the program pipeline object  `Pipeline' . The active program in
%% the active program pipeline object is the target of calls to  {@link gl:uniform1f/2}  when
%% no program has been made current through a call to  {@link gl:useProgram/1} . 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glActiveShaderProgram.xml">external</a> documentation.
-spec activeShaderProgram(Pipeline, Program) -> 'ok' when Pipeline :: integer(),Program :: integer().
activeShaderProgram(Pipeline,Program) ->
  cast(5777, <<Pipeline:?GLuint,Program:?GLuint>>).

%% @doc glCreateShaderProgramv
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glCreateShaderProgramv.xml">external</a> documentation.
-spec createShaderProgramv(Type, Strings) -> integer() when Type :: enum(),Strings :: iolist().
createShaderProgramv(Type,Strings) ->
 StringsTemp = list_to_binary([[Str|[0]] || Str <- Strings ]),
  call(5778, <<Type:?GLenum,(length(Strings)):?GLuint,(size(StringsTemp)):?GLuint,(StringsTemp)/binary,0:((8-((size(StringsTemp)+0) rem 8)) rem 8)>>).

%% @doc Bind a program pipeline to the current context
%%
%% ``gl:bindProgramPipeline'' binds a program pipeline object to the current context.  `Pipeline' 
%%  must be a name previously returned from a call to  {@link gl:genProgramPipelines/1} . If
%% no program pipeline exists with name  `Pipeline'  then a new pipeline object is created
%% with that name and initialized to the default state vector. 
%%
%%  When a program pipeline object is bound using ``gl:bindProgramPipeline'', any previous
%% binding is broken and is replaced with a binding to the specified pipeline object. If  `Pipeline' 
%%  is zero, the previous binding is broken and is not replaced, leaving no pipeline object
%% bound. If no current program object has been established by  {@link gl:useProgram/1} , the
%% program objects used for each stage and for uniform updates are taken from the bound program
%% pipeline object, if any. If there is a current program object established by  {@link gl:useProgram/1} 
%% , the bound program pipeline object has no effect on rendering or uniform updates. When
%% a bound program pipeline object is used for rendering, individual shader executables are
%% taken from its program objects. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindProgramPipeline.xml">external</a> documentation.
-spec bindProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
bindProgramPipeline(Pipeline) ->
  cast(5779, <<Pipeline:?GLuint>>).

%% @doc Delete program pipeline objects
%%
%% ``gl:deleteProgramPipelines'' deletes the  `N'  program pipeline objects whose names
%% are stored in the array  `Pipelines' . Unused names in  `Pipelines'  are ignored,
%% as is the name zero. After a program pipeline object is deleted, its name is again unused
%% and it has no contents. If program pipeline object that is currently bound is deleted,
%% the binding for that object reverts to zero and no program pipeline object becomes current.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDeleteProgramPipelines.xml">external</a> documentation.
-spec deleteProgramPipelines(Pipelines) -> 'ok' when Pipelines :: [integer()].
deleteProgramPipelines(Pipelines) ->
  cast(5780, <<(length(Pipelines)):?GLuint,
        (<< <<C:?GLuint>> || C <- Pipelines>>)/binary,0:(((1+length(Pipelines)) rem 2)*32)>>).

%% @doc Reserve program pipeline object names
%%
%% ``gl:genProgramPipelines'' returns  `N'  previously unused program pipeline object
%% names in  `Pipelines' . These names are marked as used, for the purposes of ``gl:genProgramPipelines''
%%  only, but they acquire program pipeline state only when they are first bound. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGenProgramPipelines.xml">external</a> documentation.
-spec genProgramPipelines(N) -> [integer()] when N :: integer().
genProgramPipelines(N) ->
  call(5781, <<N:?GLsizei>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glIsProgramPipeline.xml">external</a> documentation.
-spec isProgramPipeline(Pipeline) -> 0|1 when Pipeline :: integer().
isProgramPipeline(Pipeline) ->
  call(5782, <<Pipeline:?GLuint>>).

%% @doc Retrieve properties of a program pipeline object
%%
%% ``gl:getProgramPipelineiv'' retrieves the value of a property of the program pipeline
%% object  `Pipeline' .  `Pname'  specifies the name of the parameter whose value to
%% retrieve. The value of the parameter is written to the variable whose address is given
%% by  `Params' . 
%%
%%  If  `Pname'  is `?GL_ACTIVE_PROGRAM', the name of the active program object of
%% the program pipeline object is returned in  `Params' . 
%%
%%  If  `Pname'  is `?GL_VERTEX_SHADER', the name of the current program object for
%% the vertex shader type of the program pipeline object is returned in  `Params' . 
%%
%%  If  `Pname'  is `?GL_TESS_CONTROL_SHADER', the name of the current program object
%% for the tessellation control shader type of the program pipeline object is returned in  `Params' 
%% . 
%%
%%  If  `Pname'  is `?GL_TESS_EVALUATION_SHADER', the name of the current program
%% object for the tessellation evaluation shader type of the program pipeline object is returned
%% in  `Params' . 
%%
%%  If  `Pname'  is `?GL_GEOMETRY_SHADER', the name of the current program object
%% for the geometry shader type of the program pipeline object is returned in  `Params' .
%% 
%%
%%  If  `Pname'  is `?GL_FRAGMENT_SHADER', the name of the current program object
%% for the fragment shader type of the program pipeline object is returned in  `Params' .
%% 
%%
%%  If  `Pname'  is `?GL_INFO_LOG_LENGTH', the length of the info log, including
%% the null terminator, is returned in  `Params' . If there is no info log, zero is returned.
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramPipeline.xml">external</a> documentation.
-spec getProgramPipelineiv(Pipeline, Pname) -> integer() when Pipeline :: integer(),Pname :: enum().
getProgramPipelineiv(Pipeline,Pname) ->
  call(5783, <<Pipeline:?GLuint,Pname:?GLenum>>).

%% @doc Specify the value of a uniform variable for a specified program object
%%
%% ``gl:programUniform'' modifies the value of a uniform variable or a uniform variable
%% array. The location of the uniform variable to be modified is specified by  `Location' ,
%% which should be a value returned by  {@link gl:getUniformLocation/2} . ``gl:programUniform''
%%  operates on the program object specified by  `Program' .
%%
%% The commands ``gl:programUniform{1|2|3|4}{f|i|ui}'' are used to change the value of
%% the uniform variable specified by  `Location'  using the values passed as arguments.
%% The number specified in the command should match the number of components in the data
%% type of the specified uniform variable (e.g., `1' for float, int, unsigned int, bool;
%% `2' for vec2, ivec2, uvec2, bvec2, etc.). The suffix `f' indicates that floating-point
%% values are being passed; the suffix `i' indicates that integer values are being passed;
%% the suffix `ui' indicates that unsigned integer values are being passed, and this
%% type should also match the data type of the specified uniform variable. The `i' variants
%% of this function should be used to provide values for uniform variables defined as int, ivec2
%% , ivec3, ivec4, or arrays of these. The `ui' variants of this function should be
%% used to provide values for uniform variables defined as unsigned int, uvec2, uvec3, uvec4,
%% or arrays of these. The `f' variants should be used to provide values for uniform
%% variables of type float, vec2, vec3, vec4, or arrays of these. Either the `i', `ui'
%%  or `f' variants may be used to provide values for uniform variables of type bool, bvec2
%% , bvec3, bvec4, or arrays of these. The uniform variable will be set to false if the input
%% value is 0 or 0.0f, and it will be set to true otherwise.
%%
%% All active uniform variables defined in a program object are initialized to 0 when the
%% program object is linked successfully. They retain the values assigned to them by a call
%% to ``gl:programUniform'' until the next successful link operation occurs on the program
%% object, when they are once again initialized to 0.
%%
%% The commands ``gl:programUniform{1|2|3|4}{f|i|ui}v'' can be used to modify a single
%% uniform variable or a uniform variable array. These commands pass a count and a pointer
%% to the values to be loaded into a uniform variable or a uniform variable array. A count
%% of 1 should be used if modifying the value of a single uniform variable, and a count of
%% 1 or greater can be used to modify an entire array or part of an array. When loading `n'
%%  elements starting at an arbitrary position `m' in a uniform variable array, elements
%% `m' + `n' - 1 in the array will be replaced with the new values. If  `M'  +  `N' 
%%  - 1 is larger than the size of the uniform variable array, values for all array elements
%% beyond the end of the array will be ignored. The number specified in the name of the command
%% indicates the number of components for each element in  `Value' , and it should match
%% the number of components in the data type of the specified uniform variable (e.g., `1'
%%  for float, int, bool; `2' for vec2, ivec2, bvec2, etc.). The data type specified
%% in the name of the command must match the data type for the specified uniform variable
%% as described previously for ``gl:programUniform{1|2|3|4}{f|i|ui}''.
%%
%% For uniform variable arrays, each element of the array is considered to be of the type
%% indicated in the name of the command (e.g., ``gl:programUniform3f'' or ``gl:programUniform3fv''
%%  can be used to load a uniform variable array of type vec3). The number of elements of
%% the uniform variable array to be modified is specified by  `Count' 
%%
%% The commands ``gl:programUniformMatrix{2|3|4|2x3|3x2|2x4|4x2|3x4|4x3}fv''  are used
%% to modify a matrix or an array of matrices. The numbers in the command name are interpreted
%% as the dimensionality of the matrix. The number `2' indicates a 2 × 2 matrix (i.e.,
%% 4 values), the number `3' indicates a 3 × 3 matrix (i.e., 9 values), and the number `4'
%%  indicates a 4 × 4 matrix (i.e., 16 values). Non-square matrix dimensionality is explicit,
%% with the first number representing the number of columns and the second number representing
%% the number of rows. For example,  `2x4' indicates a 2 × 4 matrix with 2 columns and
%% 4 rows (i.e., 8 values). If  `Transpose'  is `?GL_FALSE', each matrix is assumed
%% to be supplied in column major order. If  `Transpose'  is `?GL_TRUE', each matrix
%% is assumed to be supplied in row major order. The  `Count'  argument indicates the
%% number of matrices to be passed. A count of 1 should be used if modifying the value of
%% a single matrix, and a count greater than 1 can be used to modify an array of matrices.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glProgramUniform.xml">external</a> documentation.
-spec programUniform1i(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1i(Program,Location,V0) ->
  cast(5784, <<Program:?GLuint,Location:?GLint,V0:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1iv(Program,Location,Value) ->
  cast(5785, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLint>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1f(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1f(Program,Location,V0) ->
  cast(5786, <<Program:?GLuint,Location:?GLint,V0:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1fv(Program,Location,Value) ->
  cast(5787, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLfloat>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1d(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1d(Program,Location,V0) ->
  cast(5788, <<Program:?GLuint,Location:?GLint,V0:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1dv(Program,Location,Value) ->
  cast(5789, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<C:?GLdouble>> || C <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1ui(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1ui(Program,Location,V0) ->
  cast(5790, <<Program:?GLuint,Location:?GLint,V0:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform1uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1uiv(Program,Location,Value) ->
  cast(5791, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<C:?GLuint>> || C <- Value>>)/binary,0:(((1+length(Value)) rem 2)*32)>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2i(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2i(Program,Location,V0,V1) ->
  cast(5792, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2iv(Program,Location,Value) ->
  cast(5793, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2f(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2f(Program,Location,V0,V1) ->
  cast(5794, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2fv(Program,Location,Value) ->
  cast(5795, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2d(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2d(Program,Location,V0,V1) ->
  cast(5796, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2dv(Program,Location,Value) ->
  cast(5797, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2ui(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2ui(Program,Location,V0,V1) ->
  cast(5798, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform2uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2uiv(Program,Location,Value) ->
  cast(5799, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint>> || {V1,V2} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3i(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3i(Program,Location,V0,V1,V2) ->
  cast(5800, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3iv(Program,Location,Value) ->
  cast(5801, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3f(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3f(Program,Location,V0,V1,V2) ->
  cast(5802, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3fv(Program,Location,Value) ->
  cast(5803, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3d(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3d(Program,Location,V0,V1,V2) ->
  cast(5804, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3dv(Program,Location,Value) ->
  cast(5805, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3ui(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3ui(Program,Location,V0,V1,V2) ->
  cast(5806, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform3uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3uiv(Program,Location,Value) ->
  cast(5807, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint>> || {V1,V2,V3} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4i(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4i(Program,Location,V0,V1,V2,V3) ->
  cast(5808, <<Program:?GLuint,Location:?GLint,V0:?GLint,V1:?GLint,V2:?GLint,V3:?GLint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4iv(Program,Location,Value) ->
  cast(5809, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4f(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4f(Program,Location,V0,V1,V2,V3) ->
  cast(5810, <<Program:?GLuint,Location:?GLint,V0:?GLfloat,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4fv(Program,Location,Value) ->
  cast(5811, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4d(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4d(Program,Location,V0,V1,V2,V3) ->
  cast(5812, <<Program:?GLuint,Location:?GLint,V0:?GLdouble,V1:?GLdouble,V2:?GLdouble,V3:?GLdouble>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4dv(Program,Location,Value) ->
  cast(5813, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4ui(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4ui(Program,Location,V0,V1,V2,V3) ->
  cast(5814, <<Program:?GLuint,Location:?GLint,V0:?GLuint,V1:?GLuint,V2:?GLuint,V3:?GLuint>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniform4uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4uiv(Program,Location,Value) ->
  cast(5815, <<Program:?GLuint,Location:?GLint,(length(Value)):?GLuint,
        (<< <<V1:?GLuint,V2:?GLuint,V3:?GLuint,V4:?GLuint>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2fv(Program,Location,Transpose,Value) ->
  cast(5816, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) ->
  cast(5817, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) ->
  cast(5818, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat,V13:?GLfloat,V14:?GLfloat,V15:?GLfloat,V16:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2dv(Program,Location,Transpose,Value) ->
  cast(5819, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble>> || {V1,V2,V3,V4} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) ->
  cast(5820, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) ->
  cast(5821, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble,V13:?GLdouble,V14:?GLdouble,V15:?GLdouble,V16:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3fv(Program,Location,Transpose,Value) ->
  cast(5822, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2fv(Program,Location,Transpose,Value) ->
  cast(5823, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) ->
  cast(5824, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) ->
  cast(5825, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) ->
  cast(5826, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) ->
  cast(5827, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:24,(length(Value)):?GLuint,
        (<< <<V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat,V5:?GLfloat,V6:?GLfloat,V7:?GLfloat,V8:?GLfloat,V9:?GLfloat,V10:?GLfloat,V11:?GLfloat,V12:?GLfloat>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3dv(Program,Location,Transpose,Value) ->
  cast(5828, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2dv(Program,Location,Transpose,Value) ->
  cast(5829, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble>> || {V1,V2,V3,V4,V5,V6} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) ->
  cast(5830, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) ->
  cast(5831, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) ->
  cast(5832, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc 
%% See {@link programUniform1i/3}
-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) ->
  cast(5833, <<Program:?GLuint,Location:?GLint,Transpose:?GLboolean,0:56,(length(Value)):?GLuint,0:32,
        (<< <<V1:?GLdouble,V2:?GLdouble,V3:?GLdouble,V4:?GLdouble,V5:?GLdouble,V6:?GLdouble,V7:?GLdouble,V8:?GLdouble,V9:?GLdouble,V10:?GLdouble,V11:?GLdouble,V12:?GLdouble>> || {V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12} <- Value>>)/binary>>).

%% @doc Validate a program pipeline object against current GL state
%%
%% ``gl:validateProgramPipeline'' instructs the implementation to validate the shader executables
%% contained in  `Pipeline'  against the current GL state. The implementation may use
%% this as an opportunity to perform any internal shader modifications that may be required
%% to ensure correct operation of the installed shaders given the current GL state. 
%%
%%  After a program pipeline has been validated, its validation status is set to `?GL_TRUE'
%% . The validation status of a program pipeline object may be queried by calling  {@link gl:getProgramPipelineiv/2} 
%%  with parameter `?GL_VALIDATE_STATUS'. 
%%
%%  If  `Pipeline'  is a name previously returned from a call to  {@link gl:genProgramPipelines/1} 
%%  but that has not yet been bound by a call to  {@link gl:bindProgramPipeline/1} , a new program
%% pipeline object is created with name  `Pipeline'  and the default state vector. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glValidateProgramPipeline.xml">external</a> documentation.
-spec validateProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
validateProgramPipeline(Pipeline) ->
  cast(5834, <<Pipeline:?GLuint>>).

%% @doc Retrieve the info log string from a program pipeline object
%%
%% ``gl:getProgramPipelineInfoLog'' retrieves the info log for the program pipeline object
%%  `Pipeline' . The info log, including its null terminator, is written into the array
%% of characters whose address is given by  `InfoLog' . The maximum number of characters
%% that may be written into  `InfoLog'  is given by  `BufSize' , and the actual number
%% of characters written into  `InfoLog'  is returned in the integer whose address is
%% given by  `Length' . If  `Length'  is `?NULL', no length is returned. 
%%
%%  The actual length of the info log for the program pipeline may be determined by calling  {@link gl:getProgramPipelineiv/2} 
%%  with  `Pname'  set to `?GL_INFO_LOG_LENGTH'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetProgramPipelineInfoLog.xml">external</a> documentation.
-spec getProgramPipelineInfoLog(Pipeline, BufSize) -> string() when Pipeline :: integer(),BufSize :: integer().
getProgramPipelineInfoLog(Pipeline,BufSize) ->
  call(5835, <<Pipeline:?GLuint,BufSize:?GLsizei>>).

%% @doc glVertexAttribL
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttribL1d(Index,X) ->
  cast(5836, <<Index:?GLuint,0:32,X:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttribL2d(Index,X,Y) ->
  cast(5837, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttribL3d(Index,X,Y,Z) ->
  cast(5838, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble>>).

%% @doc glVertexAttribL
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribL.xml">external</a> documentation.
-spec vertexAttribL4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttribL4d(Index,X,Y,Z,W) ->
  cast(5839, <<Index:?GLuint,0:32,X:?GLdouble,Y:?GLdouble,Z:?GLdouble,W:?GLdouble>>).

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
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glVertexAttribLPointer.xml">external</a> documentation.
-spec vertexAttribLPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when  is_integer(Pointer) ->
  cast(5840, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei,Pointer:?GLuint>>);
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) ->
  send_bin(Pointer),
  cast(5841, <<Index:?GLuint,Size:?GLint,Type:?GLenum,Stride:?GLsizei>>).

%% @doc glGetVertexAttribL
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetVertexAttribL.xml">external</a> documentation.
-spec getVertexAttribLdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribLdv(Index,Pname) ->
  call(5842, <<Index:?GLuint,Pname:?GLenum>>).

%% @doc glViewportArrayv
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewportArrayv.xml">external</a> documentation.
-spec viewportArrayv(First, V) -> 'ok' when First :: integer(),V :: [{float(),float(),float(),float()}].
viewportArrayv(First,V) ->
  cast(5843, <<First:?GLuint,(length(V)):?GLuint,
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
%%  x w=(x nd+1) (width/2)+x
%%
%%  y w=(y nd+1) (height/2)+y
%%
%%  The location of the viewport's bottom left corner, given by ( x,  y) is clamped to be
%% within the implementaiton-dependent viewport bounds range. The viewport bounds range [
%% min,  max] can be determined by calling  {@link gl:getBooleanv/1}  with argument `?GL_VIEWPORT_BOUNDS_RANGE'
%% . Viewport width and height are silently clamped to a range that depends on the implementation.
%% To query this range, call  {@link gl:getBooleanv/1}  with argument `?GL_MAX_VIEWPORT_DIMS'. 
%%
%%  The precision with which the GL interprets the floating point viewport bounds is implementation-dependent
%% and may be determined by querying the impementation-defined constant `?GL_VIEWPORT_SUBPIXEL_BITS'
%% . 
%%
%%  Calling ``gl:viewportIndexedfv'' is equivalent to calling see `glViewportArray'
%% with  `First'  set to  `Index' ,  `Count'  set to 1 and  `V'  passsed directly.
%% ``gl:viewportIndexedf'' is equivalent to:  void glViewportIndexedf(GLuint index, GLfloat
%% x, GLfloat y, GLfloat w, GLfloat h) { const float v[4] = { x, y, w, h }; glViewportArrayv(index,
%% 1, v); }
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glViewportIndexed.xml">external</a> documentation.
-spec viewportIndexedf(Index, X, Y, W, H) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),W :: float(),H :: float().
viewportIndexedf(Index,X,Y,W,H) ->
  cast(5844, <<Index:?GLuint,X:?GLfloat,Y:?GLfloat,W:?GLfloat,H:?GLfloat>>).

%% @doc 
%% See {@link viewportIndexedf/5}
-spec viewportIndexedfv(Index, V) -> 'ok' when Index :: integer(),V :: {float(),float(),float(),float()}.
viewportIndexedfv(Index,{V1,V2,V3,V4}) ->
  cast(5845, <<Index:?GLuint,V1:?GLfloat,V2:?GLfloat,V3:?GLfloat,V4:?GLfloat>>).

%% @doc glScissorArrayv
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorArrayv.xml">external</a> documentation.
-spec scissorArrayv(First, V) -> 'ok' when First :: integer(),V :: [{integer(),integer(),integer(),integer()}].
scissorArrayv(First,V) ->
  cast(5846, <<First:?GLuint,(length(V)):?GLuint,
        (<< <<V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>> || {V1,V2,V3,V4} <- V>>)/binary>>).

%% @doc glScissorIndexe
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorIndexe.xml">external</a> documentation.
-spec scissorIndexed(Index, Left, Bottom, Width, Height) -> 'ok' when Index :: integer(),Left :: integer(),Bottom :: integer(),Width :: integer(),Height :: integer().
scissorIndexed(Index,Left,Bottom,Width,Height) ->
  cast(5847, <<Index:?GLuint,Left:?GLint,Bottom:?GLint,Width:?GLsizei,Height:?GLsizei>>).

%% @doc glScissorIndexe
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glScissorIndexe.xml">external</a> documentation.
-spec scissorIndexedv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
scissorIndexedv(Index,{V1,V2,V3,V4}) ->
  cast(5848, <<Index:?GLuint,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @doc glDepthRangeArrayv
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRangeArrayv.xml">external</a> documentation.
-spec depthRangeArrayv(First, V) -> 'ok' when First :: integer(),V :: [{clamp(),clamp()}].
depthRangeArrayv(First,V) ->
  cast(5849, <<First:?GLuint,0:32,(length(V)):?GLuint,0:32,
        (<< <<V1:?GLclampd,V2:?GLclampd>> || {V1,V2} <- V>>)/binary>>).

%% @doc glDepthRangeIndexe
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthRangeIndexe.xml">external</a> documentation.
-spec depthRangeIndexed(Index, N, F) -> 'ok' when Index :: integer(),N :: clamp(),F :: clamp().
depthRangeIndexed(Index,N,F) ->
  cast(5850, <<Index:?GLuint,0:32,N:?GLclampd,F:?GLclampd>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getFloati_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getFloati_v(Target,Index) ->
  call(5851, <<Target:?GLenum,Index:?GLuint>>).

%% @doc 
%% See {@link getBooleanv/1}
-spec getDoublei_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getDoublei_v(Target,Index) ->
  call(5852, <<Target:?GLenum,Index:?GLuint>>).

%% @doc glDebugMessageControlARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDebugMessageControlARB.xml">external</a> documentation.
-spec debugMessageControlARB(Source, Type, Severity, Ids, Enabled) -> 'ok' when Source :: enum(),Type :: enum(),Severity :: enum(),Ids :: [integer()],Enabled :: 0|1.
debugMessageControlARB(Source,Type,Severity,Ids,Enabled) ->
  cast(5853, <<Source:?GLenum,Type:?GLenum,Severity:?GLenum,(length(Ids)):?GLuint,
        (<< <<C:?GLuint>> || C <- Ids>>)/binary,0:(((length(Ids)) rem 2)*32),Enabled:?GLboolean>>).

%% @doc glDebugMessageInsertARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDebugMessageInsertARB.xml">external</a> documentation.
-spec debugMessageInsertARB(Source, Type, Id, Severity, Buf) -> 'ok' when Source :: enum(),Type :: enum(),Id :: integer(),Severity :: enum(),Buf :: string().
debugMessageInsertARB(Source,Type,Id,Severity,Buf) ->
  cast(5854, <<Source:?GLenum,Type:?GLenum,Id:?GLuint,Severity:?GLenum,(list_to_binary([Buf|[0]]))/binary,0:((8-((length(Buf)+ 1) rem 8)) rem 8)>>).

%% @doc glGetDebugMessageLogARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetDebugMessageLogARB.xml">external</a> documentation.
-spec getDebugMessageLogARB(Count, Bufsize) -> {integer(),Sources :: [enum()],Types :: [enum()],Ids :: [integer()],Severities :: [enum()],MessageLog :: [string()]} when Count :: integer(),Bufsize :: integer().
getDebugMessageLogARB(Count,Bufsize) ->
  call(5855, <<Count:?GLuint,Bufsize:?GLsizei>>).

%% @doc glGetGraphicsResetStatusARB
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetGraphicsResetStatusARB.xml">external</a> documentation.
-spec getGraphicsResetStatusARB() -> enum().
getGraphicsResetStatusARB() ->
  call(5856, <<>>).

%% @doc Draw multiple instances of a range of elements with offset applied to instanced attributes
%%
%% ``gl:drawArraysInstancedBaseInstance'' behaves identically to  {@link gl:drawArrays/3} 
%% except that  `Primcount'  instances of the range of elements are executed and the value
%% of the internal counter  `InstanceID'  advances for each iteration.  `InstanceID' 
%% is an internal 32-bit integer counter that may be read by a vertex shader as `?gl_InstanceID'
%% . 
%%
%% ``gl:drawArraysInstancedBaseInstance'' has the same effect as:  if ( mode or count is
%% invalid ) generate appropriate error else { for (int i = 0; i &lt; primcount ; i++) {
%% instanceID = i; glDrawArrays(mode, first, count); } instanceID = 0; }
%%
%%  Specific vertex attributes may be classified as `instanced' through the use of  {@link gl:vertexAttribDivisor/2} 
%% . Instanced vertex attributes supply per-instance vertex data to the vertex shader. The
%% index of the vertex fetched from the enabled instanced vertex attribute arrays is calculated
%% as: |gl_ InstanceID/divisor|&amp;plus; baseInstance. Note that  `Baseinstance'  does not affect the shader-visible
%% value of `?gl_InstanceID'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawArraysInstancedBaseInstance.xml">external</a> documentation.
-spec drawArraysInstancedBaseInstance(Mode, First, Count, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Primcount :: integer(),Baseinstance :: integer().
drawArraysInstancedBaseInstance(Mode,First,Count,Primcount,Baseinstance) ->
  cast(5857, <<Mode:?GLenum,First:?GLint,Count:?GLsizei,Primcount:?GLsizei,Baseinstance:?GLuint>>).

%% @doc Draw multiple instances of a set of elements with offset applied to instanced attributes
%%
%% ``gl:drawElementsInstancedBaseInstance'' behaves identically to  {@link gl:drawElements/4} 
%% except that  `Primcount'  instances of the set of elements are executed and the value
%% of the internal counter  `InstanceID'  advances for each iteration.  `InstanceID' 
%% is an internal 32-bit integer counter that may be read by a vertex shader as `?gl_InstanceID'
%% . 
%%
%% ``gl:drawElementsInstancedBaseInstance'' has the same effect as:  if (mode, count, or
%% type is invalid ) generate appropriate error else { for (int i = 0; i &lt; primcount ;
%% i++) { instanceID = i; glDrawElements(mode, count, type, indices); } instanceID = 0; }
%%
%%  Specific vertex attributes may be classified as `instanced' through the use of  {@link gl:vertexAttribDivisor/2} 
%% . Instanced vertex attributes supply per-instance vertex data to the vertex shader. The
%% index of the vertex fetched from the enabled instanced vertex attribute arrays is calculated
%% as |gl_ InstanceID/divisor|&amp;plus; baseInstance. Note that  `Baseinstance'  does not affect the shader-visible
%% value of `?gl_InstanceID'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstancedBaseInstance.xml">external</a> documentation.
-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Primcount, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Primcount,Baseinstance) when  is_integer(Indices) ->
  cast(5858, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Baseinstance:?GLuint>>);
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Primcount,Baseinstance) ->
  send_bin(Indices),
  cast(5859, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Baseinstance:?GLuint>>).

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
%%  Specific vertex attributes may be classified as `instanced' through the use of  {@link gl:vertexAttribDivisor/2} 
%% . Instanced vertex attributes supply per-instance vertex data to the vertex shader. The
%% index of the vertex fetched from the enabled instanced vertex attribute arrays is calculated
%% as |gl_ InstanceID/divisor|&amp;plus; baseInstance. Note that  `Baseinstance'  does not affect the shader-visible
%% value of `?gl_InstanceID'. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawElementsInstancedBaseVertexBaseInstance.xml">external</a> documentation.
-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Primcount, Basevertex, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Primcount :: integer(),Basevertex :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance) when  is_integer(Indices) ->
  cast(5860, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Indices:?GLuint,Primcount:?GLsizei,Basevertex:?GLint,Baseinstance:?GLuint>>);
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Primcount,Basevertex,Baseinstance) ->
  send_bin(Indices),
  cast(5861, <<Mode:?GLenum,Count:?GLsizei,Type:?GLenum,Primcount:?GLsizei,Basevertex:?GLint,Baseinstance:?GLuint>>).

%% @doc glDrawTransformFeedbackInstance
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedbackInstance.xml">external</a> documentation.
-spec drawTransformFeedbackInstanced(Mode, Id, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Primcount :: integer().
drawTransformFeedbackInstanced(Mode,Id,Primcount) ->
  cast(5862, <<Mode:?GLenum,Id:?GLuint,Primcount:?GLsizei>>).

%% @doc glDrawTransformFeedbackStreamInstance
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDrawTransformFeedbackStreamInstance.xml">external</a> documentation.
-spec drawTransformFeedbackStreamInstanced(Mode, Id, Stream, Primcount) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer(),Primcount :: integer().
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Primcount) ->
  cast(5863, <<Mode:?GLenum,Id:?GLuint,Stream:?GLuint,Primcount:?GLsizei>>).

%% @doc glGetInternalformat
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glGetInternalformat.xml">external</a> documentation.
-spec getInternalformativ(Target, Internalformat, Pname, BufSize) -> [integer()] when Target :: enum(),Internalformat :: enum(),Pname :: enum(),BufSize :: integer().
getInternalformativ(Target,Internalformat,Pname,BufSize) ->
  call(5864, <<Target:?GLenum,Internalformat:?GLenum,Pname:?GLenum,BufSize:?GLsizei>>).

%% @doc Bind a level of a texture to an image unit
%%
%% ``gl:bindImageTexture'' binds a single level of a texture to an image unit for the purpose
%% of reading and writing it from shaders.  `Unit'  specifies the zero-based index of
%% the image unit to which to bind the texture level.  `Texture'  specifies the name of
%% an existing texture object to bind to the image unit. If  `Texture'  is zero, then
%% any existing binding to the image unit is broken.  `Level'  specifies the level of
%% the texture to bind to the image unit. 
%%
%%  If  `Texture'  is the name of a one-, two-, or three-dimensional array texture, a
%% cube map or cube map array texture, or a two-dimensional multisample array texture, then
%% it is possible to bind either the entire array, or only a single layer of the array to
%% the image unit. In such cases, if  `Layered'  is `?GL_TRUE', the entire array
%% is attached to the image unit and  `Layer'  is ignored. However, if  `Layered'  is `?GL_FALSE'
%%  then  `Layer'  specifies the layer of the array to attach to the image unit. 
%%
%%  `Access'  specifies the access types to be performed by shaders and may be set to `?GL_READ_ONLY'
%% , `?GL_WRITE_ONLY', or `?GL_READ_WRITE' to indicate read-only, write-only or
%% read-write access, respectively. Violation of the access type specified in  `Access' 
%% (for example, if a shader writes to an image bound with  `Access'  set to `?GL_READ_ONLY'
%% ) will lead to undefined results, possibly including program termination. 
%%
%%  `Format'  specifies the format that is to be used when performing formatted stores
%% into the image from shaders.  `Format'  must be compatible with the texture's internal
%% format and must be one of the formats listed in the following table. 
%%
%% <table><tbody><tr><td>` Image Unit Format '</td><td>` Format Qualifier '</td></tr>
%% </tbody><tbody><tr><td>`?GL_RGBA32F'</td><td>rgba32f</td></tr><tr><td>`?GL_RGBA16F'
%% </td><td>rgba16f</td></tr><tr><td>`?GL_RG32F'</td><td>rg32f</td></tr><tr><td>`?GL_RG16F'
%% </td><td>rg16f</td></tr><tr><td>`?GL_R11F_G11F_B10F'</td><td>r11f_g11f_b10f</td></tr>
%% <tr><td>`?GL_R32F'</td><td>r32f</td></tr><tr><td>`?GL_R16F'</td><td>r16f</td></tr>
%% <tr><td>`?GL_RGBA32UI'</td><td>rgba32ui</td></tr><tr><td>`?GL_RGBA16UI'</td><td>
%% rgba16ui</td></tr><tr><td>`?GL_RGB10_A2UI'</td><td>rgb10_a2ui</td></tr><tr><td>`?GL_RGBA8UI'
%% </td><td>rgba8ui</td></tr><tr><td>`?GL_RG32UI'</td><td>rg32ui</td></tr><tr><td>`?GL_RG16UI'
%% </td><td>rg16ui</td></tr><tr><td>`?GL_RG8UI'</td><td>rg8ui</td></tr><tr><td>`?GL_R32UI'
%% </td><td>r32ui</td></tr><tr><td>`?GL_R16UI'</td><td>r16ui</td></tr><tr><td>`?GL_R8UI'
%% </td><td>r8ui</td></tr><tr><td>`?GL_RGBA32I'</td><td>rgba32i</td></tr><tr><td>`?GL_RGBA16I'
%% </td><td>rgba16i</td></tr><tr><td>`?GL_RGBA8I'</td><td>rgba8i</td></tr><tr><td>`?GL_RG32I'
%% </td><td>rg32i</td></tr><tr><td>`?GL_RG16I'</td><td>rg16i</td></tr><tr><td>`?GL_RG8I'
%% </td><td>rg8i</td></tr><tr><td>`?GL_R32I'</td><td>r32i</td></tr><tr><td>`?GL_R16I'
%% </td><td>r16i</td></tr><tr><td>`?GL_R8I'</td><td>r8i</td></tr><tr><td>`?GL_RGBA16'
%% </td><td>rgba16</td></tr><tr><td>`?GL_RGB10_A2'</td><td>rgb10_a2</td></tr><tr><td>`?GL_RGBA8'
%% </td><td>rgba8</td></tr><tr><td>`?GL_RG16'</td><td>rg16</td></tr><tr><td>`?GL_RG8'
%% </td><td>rg8</td></tr><tr><td>`?GL_R16'</td><td>r16</td></tr><tr><td>`?GL_R8'</td>
%% <td>r8</td></tr><tr><td>`?GL_RGBA16_SNORM'</td><td>rgba16_snorm</td></tr><tr><td>`?GL_RGBA8_SNORM'
%% </td><td>rgba8_snorm</td></tr><tr><td>`?GL_RG16_SNORM'</td><td>rg16_snorm</td></tr><tr>
%% <td>`?GL_RG8_SNORM'</td><td>rg8_snorm</td></tr><tr><td>`?GL_R16_SNORM'</td><td>r16_snorm
%% </td></tr><tr><td>`?GL_R8_SNORM'</td><td>r8_snorm</td></tr></tbody></table>
%%
%%  When a texture is bound to an image unit, the  `Format'  parameter for the image unit
%% need not exactly match the texture internal format as long as the formats are considered
%% compatible as defined in the OpenGL Specification. The matching criterion used for a given
%% texture may be determined by calling  {@link gl:getTexParameterfv/2}  with  `Value'  set
%% to `?GL_IMAGE_FORMAT_COMPATIBILITY_TYPE', with return values of `?GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE'
%%  and `?GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS', specifying matches by size and class,
%% respectively. 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glBindImageTexture.xml">external</a> documentation.
-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok' when Unit :: integer(),Texture :: integer(),Level :: integer(),Layered :: 0|1,Layer :: integer(),Access :: enum(),Format :: enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) ->
  cast(5865, <<Unit:?GLuint,Texture:?GLuint,Level:?GLint,Layered:?GLboolean,0:24,Layer:?GLint,Access:?GLenum,Format:?GLenum>>).

%% @doc Defines a barrier ordering memory transactions
%%
%% ``gl:memoryBarrier'' defines a barrier ordering the memory transactions issued prior
%% to the command relative to those issued after the barrier. For the purposes of this ordering,
%% memory transactions performed by shaders are considered to be issued by the rendering
%% command that triggered the execution of the shader.  `Barriers'  is a bitfield indicating
%% the set of operations that are synchronized with shader stores; the bits used in  `Barriers' 
%%  are as follows: 
%%
%% 
%%
%% `?GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT':  If set, vertex data sourced from buffer objects
%% after the barrier will reflect data written by shaders prior to the barrier. The set of
%% buffer objects affected by this bit is derived from the buffer object bindings used for
%% generic vertex attributes derived from the `?GL_VERTEX_ATTRIB_ARRAY_BUFFER' bindings.
%% 
%%
%% `?GL_ELEMENT_ARRAY_BARRIER_BIT':  If set, vertex array indices sourced from buffer
%% objects after the barrier will reflect data written by shaders prior to the barrier. The
%% buffer objects affected by this bit are derived from the `?GL_ELEMENT_ARRAY_BUFFER'
%% binding. 
%%
%% `?GL_UNIFORM_BARRIER_BIT':  Shader uniforms sourced from buffer objects after the
%% barrier will reflect data written by shaders prior to the barrier. 
%%
%% `?GL_TEXTURE_FETCH_BARRIER_BIT':  Texture fetches from shaders, including fetches
%% from buffer object memory via buffer textures, after the barrier will reflect data written
%% by shaders prior to the barrier. 
%%
%% `?GL_SHADER_IMAGE_ACCESS_BARRIER_BIT':  Memory accesses using shader image load,
%% store, and atomic built-in functions issued after the barrier will reflect data written
%% by shaders prior to the barrier. Additionally, image stores and atomics issued after the
%% barrier will not execute until all memory accesses (e.g., loads, stores, texture fetches,
%% vertex fetches) initiated prior to the barrier complete. 
%%
%% `?GL_COMMAND_BARRIER_BIT':  Command data sourced from buffer objects by Draw*Indirect
%% commands after the barrier will reflect data written by shaders prior to the barrier.
%% The buffer objects affected by this bit are derived from the `?GL_DRAW_INDIRECT_BUFFER'
%%  binding. 
%%
%% `?GL_PIXEL_BUFFER_BARRIER_BIT':  Reads and writes of buffer objects via the `?GL_PIXEL_PACK_BUFFER'
%%  and `?GL_PIXEL_UNPACK_BUFFER' bindings (via  {@link gl:readPixels/7} ,  {@link gl:texSubImage1D/7} 
%% , etc.) after the barrier will reflect data written by shaders prior to the barrier. Additionally,
%% buffer object writes issued after the barrier will wait on the completion of all shader
%% writes initiated prior to the barrier. 
%%
%% `?GL_TEXTURE_UPDATE_BARRIER_BIT':  Writes to a texture via ``gl:tex(Sub)Image*'', ``gl:copyTex(Sub)Image*''
%% , ``gl:compressedTex(Sub)Image*'', and reads via  {@link gl:getTexImage/5}  after the barrier
%% will reflect data written by shaders prior to the barrier. Additionally, texture writes
%% from these commands issued after the barrier will not execute until all shader writes
%% initiated prior to the barrier complete. 
%%
%% `?GL_BUFFER_UPDATE_BARRIER_BIT':  Reads or writes via  {@link gl:bufferSubData/4} ,  {@link gl:copyBufferSubData/5} 
%% , or  {@link gl:getBufferSubData/4} , or to buffer object memory mapped by see `glMapBuffer'
%%  or see `glMapBufferRange' after the barrier will reflect data written by shaders
%% prior to the barrier. Additionally, writes via these commands issued after the barrier
%% will wait on the completion of any shader writes to the same memory initiated prior to
%% the barrier. 
%%
%% `?GL_FRAMEBUFFER_BARRIER_BIT':  Reads and writes via framebuffer object attachments
%% after the barrier will reflect data written by shaders prior to the barrier. Additionally,
%% framebuffer writes issued after the barrier will wait on the completion of all shader
%% writes issued prior to the barrier. 
%%
%% `?GL_TRANSFORM_FEEDBACK_BARRIER_BIT':  Writes via transform feedback bindings after
%% the barrier will reflect data written by shaders prior to the barrier. Additionally, transform
%% feedback writes issued after the barrier will wait on the completion of all shader writes
%% issued prior to the barrier. 
%%
%% `?GL_ATOMIC_COUNTER_BARRIER_BIT':  Accesses to atomic counters after the barrier
%% will reflect writes prior to the barrier. 
%%
%%  If  `Barriers'  is `?GL_ALL_BARRIER_BITS', shader memory accesses will be synchronized
%% relative to all the operations described above. 
%%
%%  Implementations may cache buffer object and texture image memory that could be written
%% by shaders in multiple caches; for example, there may be separate caches for texture,
%% vertex fetching, and one or more caches for shader memory accesses. Implementations are
%% not required to keep these caches coherent with shader memory writes. Stores issued by
%% one invocation may not be immediately observable by other pipeline stages or other shader
%% invocations because the value stored may remain in a cache local to the processor executing
%% the store, or because data overwritten by the store is still in a cache elsewhere in the
%% system. When ``gl:memoryBarrier'' is called, the GL flushes and/or invalidates any caches
%% relevant to the operations specified by the  `Barriers'  parameter to ensure consistent
%% ordering of operations across the barrier. 
%%
%%  To allow for independent shader invocations to communicate by reads and writes to a common
%% memory address, image variables in the OpenGL Shading Language may be declared as "coherent".
%% Buffer object or texture image memory accessed through such variables may be cached only
%% if caches are automatically updated due to stores issued by any other shader invocation.
%% If the same address is accessed using both coherent and non-coherent variables, the accesses
%% using variables declared as coherent will observe the results stored using coherent variables
%% in other invocations. Using variables declared as "coherent" guarantees only that the
%% results of stores will be immediately visible to shader invocations using similarly-declared
%% variables; calling ``gl:memoryBarrier'' is required to ensure that the stores are visible
%% to other operations. 
%%
%%  The following guidelines may be helpful in choosing when to use coherent memory accesses
%% and when to use barriers. 
%%
%% Data that are read-only or constant may be accessed without using coherent variables or
%% calling MemoryBarrier(). Updates to the read-only data via API calls such as BufferSubData
%% will invalidate shader caches implicitly as required.
%%
%% Data that are shared between shader invocations at a fine granularity (e.g., written by
%% one invocation, consumed by another invocation) should use coherent variables to read
%% and write the shared data.
%%
%% Data written by one shader invocation and consumed by other shader invocations launched
%% as a result of its execution ("dependent invocations") should use coherent variables in
%% the producing shader invocation and call memoryBarrier() after the last write. The consuming
%% shader invocation should also use coherent variables.
%%
%% Data written to image variables in one rendering pass and read by the shader in a later
%% pass need not use coherent variables or memoryBarrier(). Calling MemoryBarrier() with
%% the SHADER_IMAGE_ACCESS_BARRIER_BIT set in  `Barriers'  between passes is necessary.
%%
%% Data written by the shader in one rendering pass and read by another mechanism (e.g.,
%% vertex or index buffer pulling) in a later pass need not use coherent variables or memoryBarrier().
%% Calling ``gl:memoryBarrier'' with the appropriate bits set in  `Barriers'  between
%% passes is necessary.
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glMemoryBarrier.xml">external</a> documentation.
-spec memoryBarrier(Barriers) -> 'ok' when Barriers :: integer().
memoryBarrier(Barriers) ->
  cast(5866, <<Barriers:?GLbitfield>>).

%% @doc Simultaneously specify storage for all levels of a one-dimensional texture
%%
%% ``gl:texStorage1D'' specifies the storage requirements for all levels of a one-dimensional
%% texture simultaneously. Once a texture is specified with this command, the format and
%% dimensions of all levels become immutable unless it is a proxy texture. The contents of
%% the image may still be modified, however, its storage requirements may not change. Such
%% a texture is referred to as an `immutable-format' texture. 
%%
%%  Calling ``gl:texStorage1D'' is equivalent, assuming no errors are generated, to executing
%% the following pseudo-code:  for (i = 0; i &lt; levels; i++) { glTexImage1D(target, i,
%% internalformat, width, 0, format, type, NULL); width = max(1, (width / 2)); }
%%
%%  Since no texture data is actually provided, the values used in the pseudo-code for  `Format' 
%%  and  `Type'  are irrelevant and may be considered to be any values that are legal
%% for the chosen  `Internalformat'  enumerant.  `Internalformat'  must be one of the
%% sized internal formats given in Table 1 below, one of the sized depth-component formats `?GL_DEPTH_COMPONENT32F'
%% , `?GL_DEPTH_COMPONENT24', or `?GL_DEPTH_COMPONENT16', or one of the combined
%% depth-stencil formats, `?GL_DEPTH32F_STENCIL8', or `?GL_DEPTH24_STENCIL8'. Upon
%% success, the value of `?GL_TEXTURE_IMMUTABLE_FORMAT' becomes `?GL_TRUE'. The
%% value of `?GL_TEXTURE_IMMUTABLE_FORMAT' may be discovered by calling  {@link gl:getTexParameterfv/2} 
%%  with  `Pname'  set to `?GL_TEXTURE_IMMUTABLE_FORMAT'. No further changes to the
%% dimensions or format of the texture object may be made. Using any command that might alter
%% the dimensions or format of the texture object (such as  {@link gl:texImage1D/8}  or another
%% call to ``gl:texStorage1D'') will result in the generation of a `?GL_INVALID_OPERATION'
%%  error, even if it would not, in fact, alter the dimensions or format of the object. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexStorage1D.xml">external</a> documentation.
-spec texStorage1D(Target, Levels, Internalformat, Width) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer().
texStorage1D(Target,Levels,Internalformat,Width) ->
  cast(5867, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei>>).

%% @doc Simultaneously specify storage for all levels of a two-dimensional or one-dimensional array texture
%%
%% ``gl:texStorage2D'' specifies the storage requirements for all levels of a two-dimensional
%% texture or one-dimensional texture array simultaneously. Once a texture is specified with
%% this command, the format and dimensions of all levels become immutable unless it is a
%% proxy texture. The contents of the image may still be modified, however, its storage requirements
%% may not change. Such a texture is referred to as an `immutable-format' texture. 
%%
%%  The behavior of ``gl:texStorage2D'' depends on the  `Target'  parameter. When  `Target' 
%%  is `?GL_TEXTURE_2D', `?GL_PROXY_TEXTURE_2D', `?GL_TEXTURE_RECTANGLE', `?GL_PROXY_TEXTURE_RECTANGLE'
%%  or `?GL_PROXY_TEXTURE_CUBE_MAP', calling ``gl:texStorage2D'' is equivalent, assuming
%% no errors are generated, to executing the following pseudo-code:  for (i = 0; i &lt; levels;
%% i++) { glTexImage2D(target, i, internalformat, width, height, 0, format, type, NULL);
%% width = max(1, (width / 2)); height = max(1, (height / 2)); }
%%
%%  When  `Target'  is `?GL_TEXTURE_CUBE_MAP', ``gl:texStorage2D'' is equivalent
%% to:  for (i = 0; i &lt; levels; i++) { for (face in (+X, -X, +Y, -Y, +Z, -Z)) { glTexImage2D(face,
%% i, internalformat, width, height, 0, format, type, NULL); } width = max(1, (width / 2));
%% height = max(1, (height / 2)); }
%%
%%  When  `Target'  is `?GL_TEXTURE_1D' or `?GL_TEXTURE_1D_ARRAY', ``gl:texStorage2D''
%%  is equivalent to:  for (i = 0; i &lt; levels; i++) { glTexImage2D(target, i, internalformat,
%% width, height, 0, format, type, NULL); width = max(1, (width / 2)); }
%%
%%  Since no texture data is actually provided, the values used in the pseudo-code for  `Format' 
%%  and  `Type'  are irrelevant and may be considered to be any values that are legal
%% for the chosen  `Internalformat'  enumerant.  `Internalformat'  must be one of the
%% sized internal formats given in Table 1 below, one of the sized depth-component formats `?GL_DEPTH_COMPONENT32F'
%% , `?GL_DEPTH_COMPONENT24', or `?GL_DEPTH_COMPONENT16', or one of the combined
%% depth-stencil formats, `?GL_DEPTH32F_STENCIL8', or `?GL_DEPTH24_STENCIL8'. Upon
%% success, the value of `?GL_TEXTURE_IMMUTABLE_FORMAT' becomes `?GL_TRUE'. The
%% value of `?GL_TEXTURE_IMMUTABLE_FORMAT' may be discovered by calling  {@link gl:getTexParameterfv/2} 
%%  with  `Pname'  set to `?GL_TEXTURE_IMMUTABLE_FORMAT'. No further changes to the
%% dimensions or format of the texture object may be made. Using any command that might alter
%% the dimensions or format of the texture object (such as  {@link gl:texImage2D/9}  or another
%% call to ``gl:texStorage2D'') will result in the generation of a `?GL_INVALID_OPERATION'
%%  error, even if it would not, in fact, alter the dimensions or format of the object. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexStorage2D.xml">external</a> documentation.
-spec texStorage2D(Target, Levels, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
texStorage2D(Target,Levels,Internalformat,Width,Height) ->
  cast(5868, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei>>).

%% @doc Simultaneously specify storage for all levels of a three-dimensional, two-dimensional array or cube-map array texture
%%
%% ``gl:texStorage3D'' specifies the storage requirements for all levels of a three-dimensional,
%% two-dimensional array or cube-map array texture simultaneously. Once a texture is specified
%% with this command, the format and dimensions of all levels become immutable unless it
%% is a proxy texture. The contents of the image may still be modified, however, its storage
%% requirements may not change. Such a texture is referred to as an `immutable-format'
%% texture. 
%%
%%  The behavior of ``gl:texStorage3D'' depends on the  `Target'  parameter. When  `Target' 
%%  is `?GL_TEXTURE_3D', or `?GL_PROXY_TEXTURE_3D', calling ``gl:texStorage3D''
%% is equivalent, assuming no errors are generated, to executing the following pseudo-code: 
%% for (i = 0; i &lt; levels; i++) { glTexImage3D(target, i, internalformat, width, height,
%% depth, 0, format, type, NULL); width = max(1, (width / 2)); height = max(1, (height /
%% 2)); depth = max(1, (depth / 2)); }
%%
%%  When  `Target'  is `?GL_TEXTURE_2D_ARRAY', `?GL_PROXY_TEXTURE_2D_ARRAY', `?GL_TEXTURE_CUBE_MAP_ARRAY'
%% , or `?GL_PROXY_TEXTURE_CUBE_MAP_ARRAY', ``gl:texStorage3D'' is equivalent to: 
%% for (i = 0; i &lt; levels; i++) { glTexImage3D(target, i, internalformat, width, height,
%% depth, 0, format, type, NULL); width = max(1, (width / 2)); height = max(1, (height /
%% 2)); }
%%
%%  Since no texture data is actually provided, the values used in the pseudo-code for  `Format' 
%%  and  `Type'  are irrelevant and may be considered to be any values that are legal
%% for the chosen  `Internalformat'  enumerant.  `Internalformat'  must be one of the
%% sized internal formats given in Table 1 below, one of the sized depth-component formats `?GL_DEPTH_COMPONENT32F'
%% , `?GL_DEPTH_COMPONENT24', or `?GL_DEPTH_COMPONENT16', or one of the combined
%% depth-stencil formats, `?GL_DEPTH32F_STENCIL8', or `?GL_DEPTH24_STENCIL8'. Upon
%% success, the value of `?GL_TEXTURE_IMMUTABLE_FORMAT' becomes `?GL_TRUE'. The
%% value of `?GL_TEXTURE_IMMUTABLE_FORMAT' may be discovered by calling  {@link gl:getTexParameterfv/2} 
%%  with  `Pname'  set to `?GL_TEXTURE_IMMUTABLE_FORMAT'. No further changes to the
%% dimensions or format of the texture object may be made. Using any command that might alter
%% the dimensions or format of the texture object (such as  {@link gl:texImage3D/10}  or another
%% call to ``gl:texStorage3D'') will result in the generation of a `?GL_INVALID_OPERATION'
%%  error, even if it would not, in fact, alter the dimensions or format of the object. 
%%
%% 
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glTexStorage3D.xml">external</a> documentation.
-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) ->
  cast(5869, <<Target:?GLenum,Levels:?GLsizei,Internalformat:?GLenum,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei>>).

%% @doc glDepthBoundsEXT
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glDepthBoundsEXT.xml">external</a> documentation.
-spec depthBoundsEXT(Zmin, Zmax) -> 'ok' when Zmin :: clamp(),Zmax :: clamp().
depthBoundsEXT(Zmin,Zmax) ->
  cast(5870, <<Zmin:?GLclampd,Zmax:?GLclampd>>).

%% @doc glStencilClearTagEXT
%%
%% See <a href="http://www.opengl.org/sdk/docs/man/xhtml/glStencilClearTagEXT.xml">external</a> documentation.
-spec stencilClearTagEXT(StencilTagBits, StencilClearTag) -> 'ok' when StencilTagBits :: integer(),StencilClearTag :: integer().
stencilClearTagEXT(StencilTagBits,StencilClearTag) ->
  cast(5871, <<StencilTagBits:?GLsizei,StencilClearTag:?GLuint>>).

