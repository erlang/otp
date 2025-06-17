%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
-moduledoc """
Erlang wrapper functions for OpenGL

Standard OpenGL API

This documents the functions as a brief version of the complete
[OpenGL reference pages.](https://www.khronos.org/registry/OpenGL-Refpages/)
""".

-compile([{inline, get_interface/0}]).
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl
-type clamp() :: float().    %% 0.0..1.0
-type offset() :: non_neg_integer(). %% Offset in memory block
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
-ifdef(CAN_BUILD_DRIVER).
-on_load(init_nif/0).
-else.
-export([init_nif/0]).
-endif.

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
  drawRangeElements/6,texImage3D/10,texSubImage3D/11,copyTexSubImage3D/9,
  activeTexture/1,sampleCoverage/2,compressedTexImage3D/9,compressedTexImage2D/8,
  compressedTexImage1D/7,compressedTexSubImage3D/11,compressedTexSubImage2D/9,
  compressedTexSubImage1D/7,getCompressedTexImage/3,clientActiveTexture/1,
  multiTexCoord1d/2,multiTexCoord1dv/2,multiTexCoord1f/2,multiTexCoord1fv/2,
  multiTexCoord1i/2,multiTexCoord1iv/2,multiTexCoord1s/2,multiTexCoord1sv/2,
  multiTexCoord2d/3,multiTexCoord2dv/2,multiTexCoord2f/3,multiTexCoord2fv/2,
  multiTexCoord2i/3,multiTexCoord2iv/2,multiTexCoord2s/3,multiTexCoord2sv/2,
  multiTexCoord3d/4,multiTexCoord3dv/2,multiTexCoord3f/4,multiTexCoord3fv/2,
  multiTexCoord3i/4,multiTexCoord3iv/2,multiTexCoord3s/4,multiTexCoord3sv/2,
  multiTexCoord4d/5,multiTexCoord4dv/2,multiTexCoord4f/5,multiTexCoord4fv/2,
  multiTexCoord4i/5,multiTexCoord4iv/2,multiTexCoord4s/5,multiTexCoord4sv/2,
  loadTransposeMatrixf/1,loadTransposeMatrixd/1,multTransposeMatrixf/1,
  multTransposeMatrixd/1,blendFuncSeparate/4,multiDrawArrays/3,pointParameterf/2,
  pointParameterfv/2,pointParameteri/2,pointParameteriv/2,fogCoordf/1,
  fogCoordfv/1,fogCoordd/1,fogCoorddv/1,fogCoordPointer/3,secondaryColor3b/3,
  secondaryColor3bv/1,secondaryColor3d/3,secondaryColor3dv/1,secondaryColor3f/3,
  secondaryColor3fv/1,secondaryColor3i/3,secondaryColor3iv/1,secondaryColor3s/3,
  secondaryColor3sv/1,secondaryColor3ub/3,secondaryColor3ubv/1,secondaryColor3ui/3,
  secondaryColor3uiv/1,secondaryColor3us/3,secondaryColor3usv/1,secondaryColorPointer/4,
  windowPos2d/2,windowPos2dv/1,windowPos2f/2,windowPos2fv/1,windowPos2i/2,
  windowPos2iv/1,windowPos2s/2,windowPos2sv/1,windowPos3d/3,windowPos3dv/1,
  windowPos3f/3,windowPos3fv/1,windowPos3i/3,windowPos3iv/1,windowPos3s/3,
  windowPos3sv/1,blendColor/4,blendEquation/1,genQueries/1,deleteQueries/1,
  isQuery/1,beginQuery/2,endQuery/1,getQueryiv/2,getQueryObjectiv/2,
  getQueryObjectuiv/2,bindBuffer/2,deleteBuffers/1,genBuffers/1,isBuffer/1,
  bufferData/4,bufferSubData/4,getBufferSubData/4,getBufferParameteriv/2,
  blendEquationSeparate/2,drawBuffers/1,stencilOpSeparate/4,stencilFuncSeparate/4,
  stencilMaskSeparate/2,attachShader/2,bindAttribLocation/3,compileShader/1,
  createProgram/0,createShader/1,deleteProgram/1,deleteShader/1,detachShader/2,
  disableVertexAttribArray/1,enableVertexAttribArray/1,getActiveAttrib/3,
  getActiveUniform/3,getAttachedShaders/2,getAttribLocation/2,getProgramiv/2,
  getProgramInfoLog/2,getShaderiv/2,getShaderInfoLog/2,getShaderSource/2,
  getUniformLocation/2,getUniformfv/2,getUniformiv/2,getVertexAttribdv/2,
  getVertexAttribfv/2,getVertexAttribiv/2,isProgram/1,isShader/1,linkProgram/1,
  shaderSource/2,useProgram/1,uniform1f/2,uniform2f/3,uniform3f/4,uniform4f/5,
  uniform1i/2,uniform2i/3,uniform3i/4,uniform4i/5,uniform1fv/2,uniform2fv/2,
  uniform3fv/2,uniform4fv/2,uniform1iv/2,uniform2iv/2,uniform3iv/2,uniform4iv/2,
  uniformMatrix2fv/3,uniformMatrix3fv/3,uniformMatrix4fv/3,validateProgram/1,
  vertexAttrib1d/2,vertexAttrib1dv/2,vertexAttrib1f/2,vertexAttrib1fv/2,
  vertexAttrib1s/2,vertexAttrib1sv/2,vertexAttrib2d/3,vertexAttrib2dv/2,
  vertexAttrib2f/3,vertexAttrib2fv/2,vertexAttrib2s/3,vertexAttrib2sv/2,
  vertexAttrib3d/4,vertexAttrib3dv/2,vertexAttrib3f/4,vertexAttrib3fv/2,
  vertexAttrib3s/4,vertexAttrib3sv/2,vertexAttrib4Nbv/2,vertexAttrib4Niv/2,
  vertexAttrib4Nsv/2,vertexAttrib4Nub/5,vertexAttrib4Nubv/2,vertexAttrib4Nuiv/2,
  vertexAttrib4Nusv/2,vertexAttrib4bv/2,vertexAttrib4d/5,vertexAttrib4dv/2,
  vertexAttrib4f/5,vertexAttrib4fv/2,vertexAttrib4iv/2,vertexAttrib4s/5,
  vertexAttrib4sv/2,vertexAttrib4ubv/2,vertexAttrib4uiv/2,vertexAttrib4usv/2,
  vertexAttribPointer/6,uniformMatrix2x3fv/3,uniformMatrix3x2fv/3,
  uniformMatrix2x4fv/3,uniformMatrix4x2fv/3,uniformMatrix3x4fv/3,uniformMatrix4x3fv/3,
  colorMaski/5,getBooleani_v/2,getIntegeri_v/2,enablei/2,disablei/2,
  isEnabledi/2,beginTransformFeedback/1,endTransformFeedback/0,bindBufferRange/5,
  bindBufferBase/3,transformFeedbackVaryings/3,getTransformFeedbackVarying/3,
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
  clearBufferuiv/3,clearBufferfv/3,clearBufferfi/4,getStringi/2,isRenderbuffer/1,
  bindRenderbuffer/2,deleteRenderbuffers/1,genRenderbuffers/1,renderbufferStorage/4,
  getRenderbufferParameteriv/2,isFramebuffer/1,bindFramebuffer/2,deleteFramebuffers/1,
  genFramebuffers/1,checkFramebufferStatus/1,framebufferTexture1D/5,
  framebufferTexture2D/5,framebufferTexture3D/6,framebufferRenderbuffer/4,
  getFramebufferAttachmentParameteriv/3,generateMipmap/1,blitFramebuffer/10,
  renderbufferStorageMultisample/5,framebufferTextureLayer/5,flushMappedBufferRange/3,
  bindVertexArray/1,deleteVertexArrays/1,genVertexArrays/1,isVertexArray/1,
  drawArraysInstanced/4,drawElementsInstanced/5,texBuffer/3,primitiveRestartIndex/1,
  copyBufferSubData/5,getUniformIndices/2,getActiveUniformsiv/3,getActiveUniformName/3,
  getUniformBlockIndex/2,getActiveUniformBlockiv/4,getActiveUniformBlockName/3,
  uniformBlockBinding/3,drawElementsBaseVertex/5,drawRangeElementsBaseVertex/7,
  drawElementsInstancedBaseVertex/6,provokingVertex/1,fenceSync/2,
  isSync/1,deleteSync/1,clientWaitSync/3,waitSync/3,getInteger64v/1,
  getSynciv/3,getInteger64i_v/2,getBufferParameteri64v/2,framebufferTexture/4,
  texImage2DMultisample/6,texImage3DMultisample/7,getMultisamplefv/2,
  sampleMaski/2,bindFragDataLocationIndexed/4,getFragDataIndex/2,genSamplers/1,
  deleteSamplers/1,isSampler/1,bindSampler/2,samplerParameteri/3,samplerParameteriv/3,
  samplerParameterf/3,samplerParameterfv/3,samplerParameterIiv/3,samplerParameterIuiv/3,
  getSamplerParameteriv/2,getSamplerParameterIiv/2,getSamplerParameterfv/2,
  getSamplerParameterIuiv/2,queryCounter/2,getQueryObjecti64v/2,getQueryObjectui64v/2,
  vertexAttribDivisor/2,minSampleShading/1,blendEquationi/2,blendEquationSeparatei/3,
  blendFunci/3,blendFuncSeparatei/5,drawArraysIndirect/2,drawElementsIndirect/3,
  uniform1d/2,uniform2d/3,uniform3d/4,uniform4d/5,uniform1dv/2,uniform2dv/2,
  uniform3dv/2,uniform4dv/2,uniformMatrix2dv/3,uniformMatrix3dv/3,uniformMatrix4dv/3,
  uniformMatrix2x3dv/3,uniformMatrix2x4dv/3,uniformMatrix3x2dv/3,uniformMatrix3x4dv/3,
  uniformMatrix4x2dv/3,uniformMatrix4x3dv/3,getUniformdv/2,getSubroutineUniformLocation/3,
  getSubroutineIndex/3,getActiveSubroutineUniformName/4,getActiveSubroutineName/4,
  uniformSubroutinesuiv/2,getUniformSubroutineuiv/2,getProgramStageiv/3,
  patchParameteri/2,patchParameterfv/2,bindTransformFeedback/2,deleteTransformFeedbacks/1,
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
  drawArraysInstancedBaseInstance/5,drawElementsInstancedBaseInstance/6,
  drawElementsInstancedBaseVertexBaseInstance/7,getInternalformativ/4,
  bindImageTexture/7,memoryBarrier/1,texStorage1D/4,texStorage2D/5,
  texStorage3D/6,drawTransformFeedbackInstanced/3,drawTransformFeedbackStreamInstanced/4,
  clearBufferData/5,clearBufferSubData/7,dispatchCompute/3,dispatchComputeIndirect/1,
  copyImageSubData/15,framebufferParameteri/3,getFramebufferParameteriv/2,
  getInternalformati64v/4,invalidateTexSubImage/8,invalidateTexImage/2,
  invalidateBufferSubData/3,invalidateBufferData/1,invalidateFramebuffer/2,
  invalidateSubFramebuffer/6,multiDrawArraysIndirect/4,getProgramInterfaceiv/3,
  getProgramResourceIndex/3,getProgramResourceName/4,getProgramResourceLocation/3,
  getProgramResourceLocationIndex/3,shaderStorageBlockBinding/3,texBufferRange/5,
  texStorage2DMultisample/6,texStorage3DMultisample/7,textureView/8,
  bindVertexBuffer/4,vertexAttribFormat/5,vertexAttribIFormat/4,vertexAttribLFormat/4,
  vertexAttribBinding/2,vertexBindingDivisor/2,debugMessageControl/5,
  debugMessageInsert/5,getDebugMessageLog/2,pushDebugGroup/4,popDebugGroup/0,
  objectPtrLabel/3,bufferStorage/4,clearTexImage/5,clearTexSubImage/11,
  bindBuffersBase/3,bindBuffersRange/5,bindTextures/2,bindSamplers/2,
  bindImageTextures/2,bindVertexBuffers/4,clipControl/2,createTransformFeedbacks/1,
  transformFeedbackBufferBase/3,transformFeedbackBufferRange/5,createBuffers/1,
  flushMappedNamedBufferRange/3,createFramebuffers/1,createRenderbuffers/1,
  createTextures/2,textureBuffer/3,textureBufferRange/5,compressedTextureSubImage1D/7,
  compressedTextureSubImage2D/9,compressedTextureSubImage3D/11,generateTextureMipmap/1,
  bindTextureUnit/2,createVertexArrays/1,disableVertexArrayAttrib/2,
  enableVertexArrayAttrib/2,vertexArrayElementBuffer/2,vertexArrayVertexBuffer/5,
  vertexArrayVertexBuffers/5,vertexArrayAttribBinding/3,vertexArrayAttribFormat/6,
  vertexArrayAttribIFormat/5,vertexArrayAttribLFormat/5,vertexArrayBindingDivisor/3,
  createSamplers/1,createProgramPipelines/1,createQueries/2,getQueryBufferObjecti64v/4,
  getQueryBufferObjectiv/4,getQueryBufferObjectui64v/4,getQueryBufferObjectuiv/4,
  memoryBarrierByRegion/1,getGraphicsResetStatus/0,textureBarrier/0,
  multiDrawArraysIndirectCount/5,polygonOffsetClamp/3,primitiveBoundingBoxARB/8,
  makeTextureHandleResidentARB/1,makeTextureHandleNonResidentARB/1,
  getImageHandleARB/5,makeImageHandleResidentARB/2,makeImageHandleNonResidentARB/1,
  uniformHandleui64ARB/2,programUniformHandleui64ARB/3,isTextureHandleResidentARB/1,
  isImageHandleResidentARB/1,dispatchComputeGroupSizeARB/6,programStringARB/3,
  bindProgramARB/2,deleteProgramsARB/1,genProgramsARB/1,programEnvParameter4dARB/6,
  programEnvParameter4dvARB/3,programEnvParameter4fARB/6,programEnvParameter4fvARB/3,
  programLocalParameter4dARB/6,programLocalParameter4dvARB/3,programLocalParameter4fARB/6,
  programLocalParameter4fvARB/3,getProgramEnvParameterdvARB/2,getProgramEnvParameterfvARB/2,
  getProgramLocalParameterdvARB/2,getProgramLocalParameterfvARB/2,
  getProgramStringARB/3,framebufferTextureFaceARB/5,uniform1i64ARB/2,
  uniform2i64ARB/3,uniform3i64ARB/4,uniform4i64ARB/5,uniform1i64vARB/2,
  uniform2i64vARB/2,uniform3i64vARB/2,uniform4i64vARB/2,uniform1ui64ARB/2,
  uniform2ui64ARB/3,uniform3ui64ARB/4,uniform4ui64ARB/5,uniform1ui64vARB/2,
  uniform2ui64vARB/2,uniform3ui64vARB/2,uniform4ui64vARB/2,getUniformi64vARB/2,
  getUniformui64vARB/2,programUniform1i64ARB/3,programUniform2i64ARB/4,
  programUniform3i64ARB/5,programUniform4i64ARB/6,programUniform1i64vARB/3,
  programUniform2i64vARB/3,programUniform3i64vARB/3,programUniform4i64vARB/3,
  programUniform1ui64ARB/3,programUniform2ui64ARB/4,programUniform3ui64ARB/5,
  programUniform4ui64ARB/6,programUniform1ui64vARB/3,programUniform2ui64vARB/3,
  programUniform3ui64vARB/3,programUniform4ui64vARB/3,colorTable/6,
  colorTableParameterfv/3,colorTableParameteriv/3,copyColorTable/5,
  getColorTable/4,getColorTableParameterfv/2,getColorTableParameteriv/2,
  colorSubTable/6,copyColorSubTable/5,convolutionFilter1D/6,convolutionFilter2D/7,
  convolutionParameterf/3,convolutionParameterfv/3,convolutionParameteri/3,
  convolutionParameteriv/3,copyConvolutionFilter1D/5,copyConvolutionFilter2D/6,
  getConvolutionFilter/4,getConvolutionParameterfv/2,getConvolutionParameteriv/2,
  separableFilter2D/8,getHistogram/5,getHistogramParameterfv/2,getHistogramParameteriv/2,
  getMinmax/5,getMinmaxParameterfv/2,getMinmaxParameteriv/2,histogram/4,
  minmax/3,resetHistogram/1,resetMinmax/1,currentPaletteMatrixARB/1,
  matrixIndexubvARB/1,matrixIndexusvARB/1,matrixIndexuivARB/1,sampleCoverageARB/2,
  maxShaderCompilerThreadsARB/1,evaluateDepthValuesARB/0,deleteObjectARB/1,
  getHandleARB/1,detachObjectARB/2,createShaderObjectARB/1,shaderSourceARB/2,
  compileShaderARB/1,createProgramObjectARB/0,attachObjectARB/2,linkProgramARB/1,
  useProgramObjectARB/1,validateProgramARB/1,getObjectParameterfvARB/2,
  getObjectParameterivARB/2,getInfoLogARB/2,getAttachedObjectsARB/2,
  getUniformLocationARB/2,getActiveUniformARB/3,getUniformfvARB/2,
  getUniformivARB/2,getShaderSourceARB/2,deleteNamedStringARB/1,compileShaderIncludeARB/2,
  isNamedStringARB/1,bufferPageCommitmentARB/4,texPageCommitmentARB/9,
  getCompressedTexImageARB/3,loadTransposeMatrixfARB/1,loadTransposeMatrixdARB/1,
  multTransposeMatrixfARB/1,multTransposeMatrixdARB/1,weightbvARB/1,
  weightsvARB/1,weightivARB/1,weightfvARB/1,weightdvARB/1,weightubvARB/1,
  weightusvARB/1,weightuivARB/1,vertexBlendARB/1,getBufferParameterivARB/2,
  bindAttribLocationARB/3,getActiveAttribARB/3,getAttribLocationARB/2,
  blendBarrierKHR/0,maxShaderCompilerThreadsKHR/1,depthBoundsEXT/2]).

-export([get_interface/0, rec/1, lookup_func/1]).
-nifs([lookup_func_nif/1]).
-define(nif_stub,nif_stub_error(?LINE)).
%% @hidden
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% @hidden
-doc false.
init_nif() ->
  Base = "erl_gl",
  Priv = code:priv_dir(wx),
  SrcTree = filename:join(Priv,erlang:system_info(system_architecture)),
  NifFile = case filelib:is_dir(SrcTree) of
                true -> filename:absname(filename:join(SrcTree, Base));
                false -> filename:absname(filename:join(Priv, Base))
            end,
  erlang:load_nif(NifFile, 0).

%% @hidden
-doc false.
get_interface() ->
    wxe_util.  %% temporary

%% @hidden
-doc false.
rec(Op) ->
    receive
        {'_egl_result_', Res} -> Res;
        {'_egl_error_',  Op, Res} -> error({error,Res,Op});
        {'_egl_error_', Other, Res} ->
             Err = io_lib:format("~p in op: ~p", [Res, Other]),
            error_logger:error_report([{gl, error}, {message, lists:flatten(Err)}]),
            rec(Op)
    end.

-doc false.
lookup_func(functions) -> lookup_func_nif(1);
lookup_func(function_names) -> lookup_func_nif(2).

lookup_func_nif(_Func) -> ?nif_stub.




%% API

-doc """
[`gl:clearIndex/1`](`clearIndex/1`) specifies the index used by
[`gl:clear/1`](`clear/1`) to clear the color index buffers. `C` is not clamped.
Rather, `C` is converted to a fixed-point value with unspecified precision to
the right of the binary point. The integer part of this value is then masked
with 2 m-1, where m is the number of bits in a color index stored in the frame
buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearIndex.xml)
""".
-spec clearIndex(C::f()) -> 'ok'.
clearIndex(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5037),
  ok.

-doc """
[`gl:clearColor/4`](`clearColor/4`) specifies the red, green, blue, and alpha
values used by [`gl:clear/1`](`clear/1`) to clear the color buffers. Values
specified by [`gl:clearColor/4`](`clearColor/4`) are clamped to the range \[0
1].

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearColor.xhtml)
""".
-spec clearColor(Red::clamp(), Green::clamp(), Blue::clamp(), Alpha::clamp()) -> 'ok'.
clearColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5038),
  ok.

-doc """
[`gl:clear/1`](`clear/1`) sets the bitplane area of the window to values
previously selected by [`gl:clearColor/4`](`clearColor/4`),
[`gl:clearDepth/1`](`clearDepth/1`), and
[`gl:clearStencil/1`](`clearStencil/1`). Multiple color buffers can be cleared
simultaneously by selecting more than one buffer at a time using
[`gl:drawBuffer/1`](`drawBuffer/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClear.xhtml)
""".
-spec clear(Mask::i()) -> 'ok'.
clear(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5039),
  ok.

-doc """
[`gl:indexMask/1`](`indexMask/1`) controls the writing of individual bits in the
color index buffers. The least significant n bits of `Mask`, where n is the
number of bits in a color index buffer, specify a mask. Where a 1 (one) appears
in the mask, it's possible to write to the corresponding bit in the color index
buffer (or buffers). Where a 0 (zero) appears, the corresponding bit is
write-protected.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexMask.xml)
""".
-spec indexMask(Mask::i()) -> 'ok'.
indexMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5040),
  ok.

-doc(#{equiv => colorMaski/5}).
-spec colorMask(Red::0|1, Green::0|1, Blue::0|1, Alpha::0|1) -> 'ok'.
colorMask(Red,Green,Blue,Alpha) when (0 =:= Red) orelse (1 =:= Red),(0 =:= Green) orelse (1 =:= Green),(0 =:= Blue) orelse (1 =:= Blue),(0 =:= Alpha) orelse (1 =:= Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5041),
  ok.

-doc """
The alpha test discards fragments depending on the outcome of a comparison
between an incoming fragment's alpha value and a constant reference value.
[`gl:alphaFunc/2`](`alphaFunc/2`) specifies the reference value and the
comparison function. The comparison is performed only if alpha testing is
enabled. By default, it is not enabled. (See [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) of `?GL_ALPHA_TEST`.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAlphaFunc.xml)
""".
-spec alphaFunc(Func::enum(), Ref::clamp()) -> 'ok'.
alphaFunc(Func,Ref) when is_integer(Func),is_float(Ref) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,5042),
  ok.

-doc(#{equiv => blendFunci/3}).
-spec blendFunc(Sfactor::enum(), Dfactor::enum()) -> 'ok'.
blendFunc(Sfactor,Dfactor) when is_integer(Sfactor),is_integer(Dfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Sfactor,Dfactor,5043),
  ok.

-doc """
[`gl:logicOp/1`](`logicOp/1`) specifies a logical operation that, when enabled,
is applied between the incoming RGBA color and the RGBA color at the
corresponding location in the frame buffer. To enable or disable the logical
operation, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
using the symbolic constant `?GL_COLOR_LOGIC_OP`. The initial value is disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLogicOp.xhtml)
""".
-spec logicOp(Opcode::enum()) -> 'ok'.
logicOp(Opcode) when is_integer(Opcode) ->
  IF = get_interface(),
  IF:queue_cmd(Opcode,5044),
  ok.

-doc """
[`gl:cullFace/1`](`cullFace/1`) specifies whether front- or back-facing facets
are culled (as specified by `mode`) when facet culling is enabled. Facet culling
is initially disabled. To enable and disable facet culling, call the
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) commands with the
argument `?GL_CULL_FACE`. Facets include triangles, quadrilaterals, polygons,
and rectangles.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCullFace.xhtml)
""".
-spec cullFace(Mode::enum()) -> 'ok'.
cullFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5045),
  ok.

-doc """
In a scene composed entirely of opaque closed surfaces, back-facing polygons are
never visible. Eliminating these invisible polygons has the obvious benefit of
speeding up the rendering of the image. To enable and disable elimination of
back-facing polygons, call [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) with argument `?GL_CULL_FACE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFrontFace.xhtml)
""".
-spec frontFace(Mode::enum()) -> 'ok'.
frontFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5046),
  ok.

-doc """
[`gl:pointSize/1`](`pointSize/1`) specifies the rasterized diameter of points.
If point size mode is disabled (see [`gl:enable/1`](`enable/1`) with parameter
`?GL_PROGRAM_POINT_SIZE`), this value will be used to rasterize points.
Otherwise, the value written to the shading language built-in variable
gl_PointSize will be used.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointSize.xhtml)
""".
-spec pointSize(Size::f()) -> 'ok'.
pointSize(Size) when is_float(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Size,5047),
  ok.

-doc """
[`gl:lineWidth/1`](`lineWidth/1`) specifies the rasterized width of both aliased
and antialiased lines. Using a line width other than 1 has different effects,
depending on whether line antialiasing is enabled. To enable and disable line
antialiasing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_LINE_SMOOTH`. Line antialiasing is initially disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLineWidth.xhtml)
""".
-spec lineWidth(Width::f()) -> 'ok'.
lineWidth(Width) when is_float(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Width,5048),
  ok.

-doc """
Line stippling masks out certain fragments produced by rasterization; those
fragments will not be drawn. The masking is achieved by using three parameters:
the 16-bit line stipple pattern `Pattern`, the repeat count `Factor`, and an
integer stipple counter s.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLineStipple.xml)
""".
-spec lineStipple(Factor::i(), Pattern::i()) -> 'ok'.
lineStipple(Factor,Pattern) when is_integer(Factor),is_integer(Pattern) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Pattern,5049),
  ok.

-doc """
[`gl:polygonMode/2`](`polygonMode/2`) controls the interpretation of polygons
for rasterization. `Face` describes which polygons `Mode` applies to: both front
and back-facing polygons (`?GL_FRONT_AND_BACK`). The polygon mode affects only
the final rasterization of polygons. In particular, a polygon's vertices are lit
and the polygon is clipped and possibly culled before these modes are applied.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonMode.xhtml)
""".
-spec polygonMode(Face::enum(), Mode::enum()) -> 'ok'.
polygonMode(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5050),
  ok.

-doc """
When `?GL_POLYGON_OFFSET_FILL`, `?GL_POLYGON_OFFSET_LINE`, or
`?GL_POLYGON_OFFSET_POINT` is enabled, each fragment's `depth` value will be
offset after it is interpolated from the `depth` values of the appropriate
vertices. The value of the offset is factor×DZ+r×units, where DZ is a
measurement of the change in depth relative to the screen area of the polygon,
and r is the smallest value that is guaranteed to produce a resolvable offset
for a given implementation. The offset is added before the depth test is
performed and before the value is written into the depth buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonOffset.xhtml)
""".
-spec polygonOffset(Factor::f(), Units::f()) -> 'ok'.
polygonOffset(Factor,Units) when is_float(Factor),is_float(Units) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,5051),
  ok.

-doc """
Polygon stippling, like line stippling (see
[`gl:lineStipple/2`](`lineStipple/2`)), masks out certain fragments produced by
rasterization, creating a pattern. Stippling is independent of polygon
antialiasing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPolygonStipple.xml)
""".
-spec polygonStipple(Mask::binary()) -> 'ok'.
polygonStipple(Mask) when is_binary(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5052),
  ok.

-doc """
[`gl:getPolygonStipple/0`](`getPolygonStipple/0`) returns to `Pattern` a 32×32
polygon stipple pattern. The pattern is packed into memory as if
[`gl:readPixels/7`](`readPixels/7`) with both `height` and `width` of 32, `type`
of `?GL_BITMAP`, and `format` of `?GL_COLOR_INDEX` were called, and the stipple
pattern were stored in an internal 32×32 color index buffer. Unlike
[`gl:readPixels/7`](`readPixels/7`), however, pixel transfer operations (shift,
offset, pixel map) are not applied to the returned stipple image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPolygonStipple.xml)
""".
-spec getPolygonStipple() -> binary().
getPolygonStipple()  ->
  IF = get_interface(),
  IF:queue_cmd(5053),
  rec(5053).

-doc(#{equiv => edgeFlagv/1}).
-spec edgeFlag(Flag::0|1) -> 'ok'.
edgeFlag(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5054),
  ok.

-doc """
Each vertex of a polygon, separate triangle, or separate quadrilateral specified
between a [`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pair is
marked as the start of either a boundary or nonboundary edge. If the current
edge flag is true when the vertex is specified, the vertex is marked as the
start of a boundary edge. Otherwise, the vertex is marked as the start of a
nonboundary edge. [`gl:edgeFlag/1`](`edgeFlag/1`) sets the edge flag bit to
`?GL_TRUE` if `Flag` is `?GL_TRUE` and to `?GL_FALSE` otherwise.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlag.xml)
""".
-spec edgeFlagv({Flag::0|1}) -> 'ok'.
edgeFlagv({Flag}) ->  edgeFlag(Flag).
-doc """
[`gl:scissor/4`](`scissor/4`) defines a rectangle, called the scissor box, in
window coordinates. The first two arguments, `X` and `Y`, specify the lower left
corner of the box. `Width` and `Height` specify the width and height of the box.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissor.xhtml)
""".
-spec scissor(X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
scissor(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5055),
  ok.

-doc """
Geometry is always clipped against the boundaries of a six-plane frustum in `x`,
`y`, and `z`. [`gl:clipPlane/2`](`clipPlane/2`) allows the specification of
additional planes, not necessarily perpendicular to the `x`, `y`, or `z` axis,
against which all geometry is clipped. To determine the maximum number of
additional clipping planes, call [`gl:getIntegerv/1`](`getBooleanv/1`) with
argument `?GL_MAX_CLIP_PLANES`. All implementations support at least six such
clipping planes. Because the resulting clipping region is the intersection of
the defined half-spaces, it is always convex.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClipPlane.xml)
""".
-spec clipPlane(Plane::enum(), Equation::{f(),f(),f(),f()}) -> 'ok'.
clipPlane(Plane,Equation) when is_integer(Plane),tuple_size(Equation) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Plane,Equation,5056),
  ok.

-doc """
[`gl:getClipPlane/1`](`getClipPlane/1`) returns in `Equation` the four
coefficients of the plane equation for `Plane`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetClipPlane.xml)
""".
-spec getClipPlane(Plane::enum()) -> {f(),f(),f(),f()}.
getClipPlane(Plane) when is_integer(Plane) ->
  IF = get_interface(),
  IF:queue_cmd(Plane,5057),
  rec(5057).

-doc """
When colors are written to the frame buffer, they are written into the color
buffers specified by [`gl:drawBuffer/1`](`drawBuffer/1`). One of the following
values can be used for default framebuffer:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffer.xhtml)
""".
-spec drawBuffer(Mode::enum()) -> 'ok'.
drawBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5058),
  ok.

-doc """
[`gl:readBuffer/1`](`readBuffer/1`) specifies a color buffer as the source for
subsequent [`gl:readPixels/7`](`readPixels/7`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`), and
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`) commands. `Mode` accepts one
of twelve or more predefined values. In a fully configured system, `?GL_FRONT`,
`?GL_LEFT`, and `?GL_FRONT_LEFT` all name the front left buffer,
`?GL_FRONT_RIGHT` and `?GL_RIGHT` name the front right buffer, and
`?GL_BACK_LEFT` and `?GL_BACK` name the back left buffer. Further more, the
constants `?GL_COLOR_ATTACHMENT``i` may be used to indicate the `i`th color
attachment where `i` ranges from zero to the value of
`?GL_MAX_COLOR_ATTACHMENTS` minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadBuffer.xhtml)
""".
-spec readBuffer(Mode::enum()) -> 'ok'.
readBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5059),
  ok.

-doc(#{equiv => enablei/2}).
-spec enable(Cap::enum()) -> 'ok'.
enable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5060),
  ok.

-doc(#{equiv => enablei/2}).
-spec disable(Cap::enum()) -> 'ok'.
disable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5061),
  ok.

-doc(#{equiv => isEnabledi/2}).
-spec isEnabled(Cap::enum()) -> 0|1.
isEnabled(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5062),
  rec(5062).

-doc """
[`gl:enableClientState/1`](`enableClientState/1`) and
[`gl:disableClientState/1`](`enableClientState/1`) enable or disable individual
client-side capabilities. By default, all client-side capabilities are disabled.
Both [`gl:enableClientState/1`](`enableClientState/1`) and
[`gl:disableClientState/1`](`enableClientState/1`) take a single argument,
`Cap`, which can assume one of the following values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEnableClientState.xml)
""".
-spec enableClientState(Cap::enum()) -> 'ok'.
enableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5063),
  ok.

-doc(#{equiv => enableClientState/1}).
-spec disableClientState(Cap::enum()) -> 'ok'.
disableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5064),
  ok.

-doc(#{equiv => getIntegerv/1}).
-spec getBooleanv(Pname::enum()) -> [0|1].
getBooleanv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5065),
  rec(5065).

-doc(#{equiv => getIntegerv/1}).
-spec getDoublev(Pname::enum()) -> [f()].
getDoublev(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5066),
  rec(5066).

-doc(#{equiv => getIntegerv/1}).
-spec getFloatv(Pname::enum()) -> [f()].
getFloatv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5067),
  rec(5067).

-doc """
These commands return values for simple state variables in GL. `Pname` is a
symbolic constant indicating the state variable to be returned, and `Data` is a
pointer to an array of the indicated type in which to place the returned data.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGet.xhtml)
""".
-spec getIntegerv(Pname::enum()) -> [i()].
getIntegerv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5068),
  rec(5068).

-doc """
[`gl:pushAttrib/1`](`pushAttrib/1`) takes one argument, a mask that indicates
which groups of state variables to save on the attribute stack. Symbolic
constants are used to set bits in the mask. `Mask` is typically constructed by
specifying the bitwise-or of several of these constants together. The special
mask `?GL_ALL_ATTRIB_BITS` can be used to save all stackable states.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushAttrib.xml)
""".
-spec pushAttrib(Mask::i()) -> 'ok'.
pushAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5069),
  ok.

-doc(#{equiv => pushAttrib/1}).
-spec popAttrib() -> 'ok'.
popAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5070),
  ok.

-doc """
[`gl:pushClientAttrib/1`](`pushClientAttrib/1`) takes one argument, a mask that
indicates which groups of client-state variables to save on the client attribute
stack. Symbolic constants are used to set bits in the mask. `Mask` is typically
constructed by specifying the bitwise-or of several of these constants together.
The special mask `?GL_CLIENT_ALL_ATTRIB_BITS` can be used to save all stackable
client state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushClientAttrib.xml)
""".
-spec pushClientAttrib(Mask::i()) -> 'ok'.
pushClientAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5071),
  ok.

-doc(#{equiv => pushClientAttrib/1}).
-spec popClientAttrib() -> 'ok'.
popClientAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5072),
  ok.

-doc """
[`gl:renderMode/1`](`renderMode/1`) sets the rasterization mode. It takes one
argument, `Mode`, which can assume one of three predefined values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRenderMode.xml)
""".
-spec renderMode(Mode::enum()) -> i().
renderMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5073),
  rec(5073).

-doc """
[`gl:getError/0`](`getError/0`) returns the value of the error flag. Each
detectable error is assigned a numeric code and symbolic name. When an error
occurs, the error flag is set to the appropriate error code value. No other
errors are recorded until [`gl:getError/0`](`getError/0`) is called, the error
code is returned, and the flag is reset to `?GL_NO_ERROR`. If a call to
[`gl:getError/0`](`getError/0`) returns `?GL_NO_ERROR`, there has been no
detectable error since the last call to [`gl:getError/0`](`getError/0`), or
since the GL was initialized.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetError.xhtml)
""".
-spec getError() -> enum().
getError()  ->
  IF = get_interface(),
  IF:queue_cmd(5074),
  rec(5074).

-doc(#{equiv => getStringi/2}).
-spec getString(Name::enum()) -> string().
getString(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5075),
  rec(5075).

-doc """
[`gl:finish/0`](`finish/0`) does not return until the effects of all previously
called GL commands are complete. Such effects include all changes to GL state,
all changes to connection state, and all changes to the frame buffer contents.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFinish.xhtml)
""".
-spec finish() -> 'ok'.
finish()  ->
  IF = get_interface(),
  IF:queue_cmd(5076),
  ok.

-doc """
Different GL implementations buffer commands in several different locations,
including network buffers and the graphics accelerator itself.
[`gl:flush/0`](`flush/0`) empties all of these buffers, causing all issued
commands to be executed as quickly as they are accepted by the actual rendering
engine. Though this execution may not be completed in any particular time
period, it does complete in finite time.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlush.xhtml)
""".
-spec flush() -> 'ok'.
flush()  ->
  IF = get_interface(),
  IF:queue_cmd(5077),
  ok.

-doc """
Certain aspects of GL behavior, when there is room for interpretation, can be
controlled with hints. A hint is specified with two arguments. `Target` is a
symbolic constant indicating the behavior to be controlled, and `Mode` is
another symbolic constant indicating the desired behavior. The initial value for
each `Target` is `?GL_DONT_CARE`. `Mode` can be one of the following:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glHint.xhtml)
""".
-spec hint(Target::enum(), Mode::enum()) -> 'ok'.
hint(Target,Mode) when is_integer(Target),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Mode,5078),
  ok.

-doc(#{equiv => clearDepthf/1}).
-spec clearDepth(Depth::clamp()) -> 'ok'.
clearDepth(Depth) when is_float(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Depth,5079),
  ok.

-doc """
[`gl:depthFunc/1`](`depthFunc/1`) specifies the function used to compare each
incoming pixel depth value with the depth value present in the depth buffer. The
comparison is performed only if depth testing is enabled. (See
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) of
`?GL_DEPTH_TEST`.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthFunc.xhtml)
""".
-spec depthFunc(Func::enum()) -> 'ok'.
depthFunc(Func) when is_integer(Func) ->
  IF = get_interface(),
  IF:queue_cmd(Func,5080),
  ok.

-doc """
[`gl:depthMask/1`](`depthMask/1`) specifies whether the depth buffer is enabled
for writing. If `Flag` is `?GL_FALSE`, depth buffer writing is disabled.
Otherwise, it is enabled. Initially, depth buffer writing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthMask.xhtml)
""".
-spec depthMask(Flag::0|1) -> 'ok'.
depthMask(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5081),
  ok.

-doc(#{equiv => depthRangef/2}).
-spec depthRange(Near_val::clamp(), Far_val::clamp()) -> 'ok'.
depthRange(Near_val,Far_val) when is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Near_val,Far_val,5082),
  ok.

-doc """
[`gl:clearAccum/4`](`clearAccum/4`) specifies the red, green, blue, and alpha
values used by [`gl:clear/1`](`clear/1`) to clear the accumulation buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearAccum.xml)
""".
-spec clearAccum(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
clearAccum(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5083),
  ok.

-doc """
The accumulation buffer is an extended-range color buffer. Images are not
rendered into it. Rather, images rendered into one of the color buffers are
added to the contents of the accumulation buffer after rendering. Effects such
as antialiasing (of points, lines, and polygons), motion blur, and depth of
field can be created by accumulating images generated with different
transformation matrices.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAccum.xml)
""".
-spec accum(Op::enum(), Value::f()) -> 'ok'.
accum(Op,Value) when is_integer(Op),is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Op,Value,5084),
  ok.

-doc """
[`gl:matrixMode/1`](`matrixMode/1`) sets the current matrix mode. `Mode` can
assume one of four values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMatrixMode.xml)
""".
-spec matrixMode(Mode::enum()) -> 'ok'.
matrixMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5085),
  ok.

-doc """
[`gl:ortho/6`](`ortho/6`) describes a transformation that produces a parallel
projection. The current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is
multiplied by this matrix and the result replaces the current matrix, as if
[`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix as
its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glOrtho.xml)
""".
-spec ortho(Left::f(), Right::f(), Bottom::f(), Top::f(), Near_val::f(), Far_val::f()) -> 'ok'.
ortho(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5086),
  ok.

-doc """
[`gl:frustum/6`](`frustum/6`) describes a perspective matrix that produces a
perspective projection. The current matrix (see
[`gl:matrixMode/1`](`matrixMode/1`)) is multiplied by this matrix and the result
replaces the current matrix, as if [`gl:multMatrix()`](`multMatrixd/1`) were
called with the following matrix as its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFrustum.xml)
""".
-spec frustum(Left::f(), Right::f(), Bottom::f(), Top::f(), Near_val::f(), Far_val::f()) -> 'ok'.
frustum(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5087),
  ok.

-doc """
[`gl:viewport/4`](`viewport/4`) specifies the affine transformation of x and y
from normalized device coordinates to window coordinates. Let (x nd y nd) be
normalized device coordinates. Then the window coordinates (x w y w) are
computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewport.xhtml)
""".
-spec viewport(X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
viewport(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5088),
  ok.

-doc """
There is a stack of matrices for each of the matrix modes. In `?GL_MODELVIEW`
mode, the stack depth is at least 32. In the other modes, `?GL_COLOR`,
`?GL_PROJECTION`, and `?GL_TEXTURE`, the depth is at least 2. The current matrix
in any mode is the matrix on the top of the stack for that mode.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushMatrix.xml)
""".
-spec pushMatrix() -> 'ok'.
pushMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5089),
  ok.

-doc(#{equiv => pushMatrix/0}).
-spec popMatrix() -> 'ok'.
popMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5090),
  ok.

-doc """
[`gl:loadIdentity/0`](`loadIdentity/0`) replaces the current matrix with the
identity matrix. It is semantically equivalent to calling
[`gl:loadMatrix()`](`loadMatrixd/1`) with the identity matrix

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadIdentity.xml)
""".
-spec loadIdentity() -> 'ok'.
loadIdentity()  ->
  IF = get_interface(),
  IF:queue_cmd(5091),
  ok.

-doc(#{equiv => loadMatrixf/1}).
-spec loadMatrixd(M::matrix()) -> 'ok'.
loadMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5092),
  ok.

-doc """
[`gl:loadMatrix()`](`loadMatrixd/1`) replaces the current matrix with the one
whose elements are specified by `M`. The current matrix is the projection
matrix, modelview matrix, or texture matrix, depending on the current matrix
mode (see [`gl:matrixMode/1`](`matrixMode/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadMatrix.xml)
""".
-spec loadMatrixf(M::matrix()) -> 'ok'.
loadMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5093),
  ok.

-doc(#{equiv => multMatrixf/1}).
-spec multMatrixd(M::matrix()) -> 'ok'.
multMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5094),
  ok.

-doc """
[`gl:multMatrix()`](`multMatrixd/1`) multiplies the current matrix with the one
specified using `M`, and replaces the current matrix with the product.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultMatrix.xml)
""".
-spec multMatrixf(M::matrix()) -> 'ok'.
multMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5095),
  ok.

-doc(#{equiv => rotatef/4}).
-spec rotated(Angle::f(), X::f(), Y::f(), Z::f()) -> 'ok'.
rotated(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5096),
  ok.

-doc """
[`gl:rotate()`](`rotated/4`) produces a rotation of `Angle` degrees around the
vector (x y z). The current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is
multiplied by a rotation matrix with the product replacing the current matrix,
as if [`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix
as its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRotate.xml)
""".
-spec rotatef(Angle::f(), X::f(), Y::f(), Z::f()) -> 'ok'.
rotatef(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5097),
  ok.

-doc(#{equiv => scalef/3}).
-spec scaled(X::f(), Y::f(), Z::f()) -> 'ok'.
scaled(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5098),
  ok.

-doc """
[`gl:scale()`](`scaled/3`) produces a nonuniform scaling along the `x`, `y`, and
`z` axes. The three parameters indicate the desired scale factor along each of
the three axes.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glScale.xml)
""".
-spec scalef(X::f(), Y::f(), Z::f()) -> 'ok'.
scalef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5099),
  ok.

-doc(#{equiv => translatef/3}).
-spec translated(X::f(), Y::f(), Z::f()) -> 'ok'.
translated(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5100),
  ok.

-doc """
[`gl:translate()`](`translated/3`) produces a translation by (x y z). The
current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is multiplied by this
translation matrix, with the product replacing the current matrix, as if
[`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix for
its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTranslate.xml)
""".
-spec translatef(X::f(), Y::f(), Z::f()) -> 'ok'.
translatef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5101),
  ok.

-doc """
[`gl:isList/1`](`isList/1`) returns `?GL_TRUE` if `List` is the name of a
display list and returns `?GL_FALSE` if it is not, or if an error occurs.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIsList.xml)
""".
-spec isList(List::i()) -> 0|1.
isList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5102),
  rec(5102).

-doc """
[`gl:deleteLists/2`](`deleteLists/2`) causes a contiguous group of display lists
to be deleted. `List` is the name of the first display list to be deleted, and
`Range` is the number of display lists to delete. All display lists d with
list&lt;= d&lt;= list+range-1 are deleted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDeleteLists.xml)
""".
-spec deleteLists(List::i(), Range::i()) -> 'ok'.
deleteLists(List,Range) when is_integer(List),is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(List,Range,5103),
  ok.

-doc """
[`gl:genLists/1`](`genLists/1`) has one argument, `Range`. It returns an integer
`n` such that `Range` contiguous empty display lists, named n, n+1, ...,
n+range-1, are created. If `Range` is 0, if there is no group of `Range`
contiguous names available, or if any error is generated, no display lists are
generated, and 0 is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenLists.xml)
""".
-spec genLists(Range::i()) -> i().
genLists(Range) when is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(Range,5104),
  rec(5104).

-doc """
Display lists are groups of GL commands that have been stored for subsequent
execution. Display lists are created with [`gl:newList/2`](`newList/2`). All
subsequent commands are placed in the display list, in the order issued, until
[`gl:endList/0`](`newList/2`) is called.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNewList.xml)
""".
-spec newList(List::i(), Mode::enum()) -> 'ok'.
newList(List,Mode) when is_integer(List),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(List,Mode,5105),
  ok.

-doc(#{equiv => newList/2}).
-spec endList() -> 'ok'.
endList()  ->
  IF = get_interface(),
  IF:queue_cmd(5106),
  ok.

-doc """
[`gl:callList/1`](`callList/1`) causes the named display list to be executed.
The commands saved in the display list are executed in order, just as if they
were called without using a display list. If `List` has not been defined as a
display list, [`gl:callList/1`](`callList/1`) is ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallList.xml)
""".
-spec callList(List::i()) -> 'ok'.
callList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5107),
  ok.

-doc """
[`gl:callLists/1`](`callLists/1`) causes each display list in the list of names
passed as `Lists` to be executed. As a result, the commands saved in each
display list are executed in order, just as if they were called without using a
display list. Names of display lists that have not been defined are ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallLists.xml)
""".
-spec callLists(Lists::[i()]) -> 'ok'.
callLists(Lists) when is_list(Lists) ->
  IF = get_interface(),
  N = length(Lists),
  IF:queue_cmd(N,Lists,5108),
  ok.

-doc """
[`gl:callLists/1`](`callLists/1`) specifies an array of offsets. Display-list
names are generated by adding `Base` to each offset. Names that reference valid
display lists are executed; the others are ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glListBase.xml)
""".
-spec listBase(Base::i()) -> 'ok'.
listBase(Base) when is_integer(Base) ->
  IF = get_interface(),
  IF:queue_cmd(Base,5109),
  ok.

-doc(#{equiv => '\'end\''/0}).
-spec 'begin'(Mode::enum()) -> 'ok'.
'begin'(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5110),
  ok.

-doc """
[`gl:'begin'/1`](`'begin'/1`) and [`gl:'end'/0`](`'begin'/1`) delimit the
vertices that define a primitive or a group of like primitives.
[`gl:'begin'/1`](`'begin'/1`) accepts a single argument that specifies in which
of ten ways the vertices are interpreted. Taking n as an integer count starting
at one, and N as the total number of vertices specified, the interpretations are
as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBegin.xml)
""".
-spec 'end'() -> 'ok'.
'end'()  ->
  IF = get_interface(),
  IF:queue_cmd(5111),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex2d(X::f(), Y::f()) -> 'ok'.
vertex2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5112),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex2f(X::f(), Y::f()) -> 'ok'.
vertex2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5113),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex2i(X::i(), Y::i()) -> 'ok'.
vertex2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5114),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex2s(X::i(), Y::i()) -> 'ok'.
vertex2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5115),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex3d(X::f(), Y::f(), Z::f()) -> 'ok'.
vertex3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5116),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex3f(X::f(), Y::f(), Z::f()) -> 'ok'.
vertex3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5117),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex3i(X::i(), Y::i(), Z::i()) -> 'ok'.
vertex3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5118),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex3s(X::i(), Y::i(), Z::i()) -> 'ok'.
vertex3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5119),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex4d(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertex4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5120),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex4f(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertex4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5121),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex4i(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertex4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5122),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex4s(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertex4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5123),
  ok.

-doc(#{equiv => vertex4sv/1}).
-spec vertex2dv({X::f(), Y::f()}) -> 'ok'.
vertex2dv({X,Y}) ->  vertex2d(X,Y).
-doc(#{equiv => vertex4sv/1}).
-spec vertex2fv({X::f(), Y::f()}) -> 'ok'.
vertex2fv({X,Y}) ->  vertex2f(X,Y).
-doc(#{equiv => vertex4sv/1}).
-spec vertex2iv({X::i(), Y::i()}) -> 'ok'.
vertex2iv({X,Y}) ->  vertex2i(X,Y).
-doc(#{equiv => vertex4sv/1}).
-spec vertex2sv({X::i(), Y::i()}) -> 'ok'.
vertex2sv({X,Y}) ->  vertex2s(X,Y).
-doc(#{equiv => vertex4sv/1}).
-spec vertex3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).
-doc(#{equiv => vertex4sv/1}).
-spec vertex3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).
-doc(#{equiv => vertex4sv/1}).
-spec vertex3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).
-doc(#{equiv => vertex4sv/1}).
-spec vertex3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).
-doc(#{equiv => vertex4sv/1}).
-spec vertex4dv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).
-doc(#{equiv => vertex4sv/1}).
-spec vertex4fv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).
-doc(#{equiv => vertex4sv/1}).
-spec vertex4iv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).
-doc """
[`gl:vertex()`](`vertex2d/2`) commands are used within
[`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pairs to specify
point, line, and polygon vertices. The current color, normal, texture
coordinates, and fog coordinate are associated with the vertex when
[`gl:vertex()`](`vertex2d/2`) is called.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertex.xml)
""".
-spec vertex4sv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).
-doc(#{equiv => normal3sv/1}).
-spec normal3b(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3b(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5124),
  ok.

-doc(#{equiv => normal3sv/1}).
-spec normal3d(Nx::f(), Ny::f(), Nz::f()) -> 'ok'.
normal3d(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5125),
  ok.

-doc(#{equiv => normal3sv/1}).
-spec normal3f(Nx::f(), Ny::f(), Nz::f()) -> 'ok'.
normal3f(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5126),
  ok.

-doc(#{equiv => normal3sv/1}).
-spec normal3i(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3i(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5127),
  ok.

-doc(#{equiv => normal3sv/1}).
-spec normal3s(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3s(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5128),
  ok.

-doc(#{equiv => normal3sv/1}).
-spec normal3bv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).
-doc(#{equiv => normal3sv/1}).
-spec normal3dv({Nx::f(), Ny::f(), Nz::f()}) -> 'ok'.
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).
-doc(#{equiv => normal3sv/1}).
-spec normal3fv({Nx::f(), Ny::f(), Nz::f()}) -> 'ok'.
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).
-doc(#{equiv => normal3sv/1}).
-spec normal3iv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).
-doc """
The current normal is set to the given coordinates whenever
[`gl:normal()`](`normal3b/3`) is issued. Byte, short, or integer arguments are
converted to floating-point format with a linear mapping that maps the most
positive representable integer value to 1.0 and the most negative representable
integer value to -1.0.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormal.xml)
""".
-spec normal3sv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).
-doc(#{equiv => indexubv/1}).
-spec indexd(C::f()) -> 'ok'.
indexd(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5129),
  ok.

-doc(#{equiv => indexubv/1}).
-spec indexf(C::f()) -> 'ok'.
indexf(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5130),
  ok.

-doc(#{equiv => indexubv/1}).
-spec indexi(C::i()) -> 'ok'.
indexi(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5131),
  ok.

-doc(#{equiv => indexubv/1}).
-spec indexs(C::i()) -> 'ok'.
indexs(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5132),
  ok.

-doc(#{equiv => indexubv/1}).
-spec indexub(C::i()) -> 'ok'.
indexub(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5133),
  ok.

-doc(#{equiv => indexubv/1}).
-spec indexdv({C::f()}) -> 'ok'.
indexdv({C}) ->  indexd(C).
-doc(#{equiv => indexubv/1}).
-spec indexfv({C::f()}) -> 'ok'.
indexfv({C}) ->  indexf(C).
-doc(#{equiv => indexubv/1}).
-spec indexiv({C::i()}) -> 'ok'.
indexiv({C}) ->  indexi(C).
-doc(#{equiv => indexubv/1}).
-spec indexsv({C::i()}) -> 'ok'.
indexsv({C}) ->  indexs(C).
-doc """
[`gl:index()`](`indexd/1`) updates the current (single-valued) color index. It
takes one argument, the new value for the current color index.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndex.xml)
""".
-spec indexubv({C::i()}) -> 'ok'.
indexubv({C}) ->  indexub(C).
-doc(#{equiv => color4usv/1}).
-spec color3b(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5134),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3d(Red::f(), Green::f(), Blue::f()) -> 'ok'.
color3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5135),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3f(Red::f(), Green::f(), Blue::f()) -> 'ok'.
color3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5136),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3i(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5137),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3s(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5138),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3ub(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5139),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3ui(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5140),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3us(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5141),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4b(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4b(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5142),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4d(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
color4d(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5143),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4f(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
color4f(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5144),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4i(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4i(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5145),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4s(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4s(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5146),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4ub(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4ub(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5147),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4ui(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4ui(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5148),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color4us(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4us(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5149),
  ok.

-doc(#{equiv => color4usv/1}).
-spec color3bv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3dv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3fv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3iv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3sv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3ubv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3uiv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color3usv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).
-doc(#{equiv => color4usv/1}).
-spec color4bv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4dv({Red::f(), Green::f(), Blue::f(), Alpha::f()}) -> 'ok'.
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4fv({Red::f(), Green::f(), Blue::f(), Alpha::f()}) -> 'ok'.
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4iv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4sv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4ubv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).
-doc(#{equiv => color4usv/1}).
-spec color4uiv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).
-doc """
The GL stores both a current single-valued color index and a current four-valued
RGBA color. [`gl:color()`](`color3b/3`) sets a new four-valued RGBA color.
[`gl:color()`](`color3b/3`) has two major variants: [`gl:color3()`](`color3b/3`)
and [`gl:color4()`](`color3b/3`). [`gl:color3()`](`color3b/3`) variants specify
new red, green, and blue values explicitly and set the current alpha value to
1.0 (full intensity) implicitly. [`gl:color4()`](`color3b/3`) variants specify
all four color components explicitly.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColor.xml)
""".
-spec color4usv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1d(S::f()) -> 'ok'.
texCoord1d(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5150),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1f(S::f()) -> 'ok'.
texCoord1f(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5151),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1i(S::i()) -> 'ok'.
texCoord1i(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5152),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1s(S::i()) -> 'ok'.
texCoord1s(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5153),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2d(S::f(), T::f()) -> 'ok'.
texCoord2d(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5154),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2f(S::f(), T::f()) -> 'ok'.
texCoord2f(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5155),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2i(S::i(), T::i()) -> 'ok'.
texCoord2i(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5156),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2s(S::i(), T::i()) -> 'ok'.
texCoord2s(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5157),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3d(S::f(), T::f(), R::f()) -> 'ok'.
texCoord3d(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5158),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3f(S::f(), T::f(), R::f()) -> 'ok'.
texCoord3f(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5159),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3i(S::i(), T::i(), R::i()) -> 'ok'.
texCoord3i(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5160),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3s(S::i(), T::i(), R::i()) -> 'ok'.
texCoord3s(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5161),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4d(S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
texCoord4d(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5162),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4f(S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
texCoord4f(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5163),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4i(S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
texCoord4i(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5164),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4s(S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
texCoord4s(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5165),
  ok.

-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1dv({S::f()}) -> 'ok'.
texCoord1dv({S}) ->  texCoord1d(S).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1fv({S::f()}) -> 'ok'.
texCoord1fv({S}) ->  texCoord1f(S).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1iv({S::i()}) -> 'ok'.
texCoord1iv({S}) ->  texCoord1i(S).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord1sv({S::i()}) -> 'ok'.
texCoord1sv({S}) ->  texCoord1s(S).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2dv({S::f(), T::f()}) -> 'ok'.
texCoord2dv({S,T}) ->  texCoord2d(S,T).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2fv({S::f(), T::f()}) -> 'ok'.
texCoord2fv({S,T}) ->  texCoord2f(S,T).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2iv({S::i(), T::i()}) -> 'ok'.
texCoord2iv({S,T}) ->  texCoord2i(S,T).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord2sv({S::i(), T::i()}) -> 'ok'.
texCoord2sv({S,T}) ->  texCoord2s(S,T).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3dv({S::f(), T::f(), R::f()}) -> 'ok'.
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3fv({S::f(), T::f(), R::f()}) -> 'ok'.
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3iv({S::i(), T::i(), R::i()}) -> 'ok'.
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord3sv({S::i(), T::i(), R::i()}) -> 'ok'.
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4dv({S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4fv({S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).
-doc(#{equiv => texCoord4sv/1}).
-spec texCoord4iv({S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).
-doc """
[`gl:texCoord()`](`texCoord1d/1`) specifies texture coordinates in one, two,
three, or four dimensions. [`gl:texCoord1()`](`texCoord1d/1`) sets the current
texture coordinates to (s 0 0 1); a call to [`gl:texCoord2()`](`texCoord1d/1`)
sets them to (s t 0 1). Similarly, [`gl:texCoord3()`](`texCoord1d/1`) specifies
the texture coordinates as (s t r 1), and [`gl:texCoord4()`](`texCoord1d/1`)
defines all four components explicitly as (s t r q).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoord.xml)
""".
-spec texCoord4sv({S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2d(X::f(), Y::f()) -> 'ok'.
rasterPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5166),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2f(X::f(), Y::f()) -> 'ok'.
rasterPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5167),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2i(X::i(), Y::i()) -> 'ok'.
rasterPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5168),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2s(X::i(), Y::i()) -> 'ok'.
rasterPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5169),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3d(X::f(), Y::f(), Z::f()) -> 'ok'.
rasterPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5170),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3f(X::f(), Y::f(), Z::f()) -> 'ok'.
rasterPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5171),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3i(X::i(), Y::i(), Z::i()) -> 'ok'.
rasterPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5172),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3s(X::i(), Y::i(), Z::i()) -> 'ok'.
rasterPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5173),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4d(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
rasterPos4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5174),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4f(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
rasterPos4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5175),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4i(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
rasterPos4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5176),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4s(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
rasterPos4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5177),
  ok.

-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2dv({X::f(), Y::f()}) -> 'ok'.
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2fv({X::f(), Y::f()}) -> 'ok'.
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2iv({X::i(), Y::i()}) -> 'ok'.
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos2sv({X::i(), Y::i()}) -> 'ok'.
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4dv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4fv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).
-doc(#{equiv => rasterPos4sv/1}).
-spec rasterPos4iv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).
-doc """
The GL maintains a 3D position in window coordinates. This position, called the
raster position, is used to position pixel and bitmap write operations. It is
maintained with subpixel accuracy. See [`gl:bitmap/7`](`bitmap/7`),
[`gl:drawPixels/5`](`drawPixels/5`), and [`gl:copyPixels/5`](`copyPixels/5`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRasterPos.xml)
""".
-spec rasterPos4sv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).
-doc(#{equiv => rectsv/2}).
-spec rectd(X1::f(), Y1::f(), X2::f(), Y2::f()) -> 'ok'.
rectd(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5178),
  ok.

-doc(#{equiv => rectsv/2}).
-spec rectf(X1::f(), Y1::f(), X2::f(), Y2::f()) -> 'ok'.
rectf(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5179),
  ok.

-doc(#{equiv => rectsv/2}).
-spec recti(X1::i(), Y1::i(), X2::i(), Y2::i()) -> 'ok'.
recti(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5180),
  ok.

-doc(#{equiv => rectsv/2}).
-spec rects(X1::i(), Y1::i(), X2::i(), Y2::i()) -> 'ok'.
rects(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5181),
  ok.

-doc(#{equiv => rectsv/2}).
-spec rectdv(V1::{f(),f()}, V2::{f(),f()}) -> 'ok'.
rectdv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5182),
  ok.

-doc(#{equiv => rectsv/2}).
-spec rectfv(V1::{f(),f()}, V2::{f(),f()}) -> 'ok'.
rectfv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5183),
  ok.

-doc(#{equiv => rectsv/2}).
-spec rectiv(V1::{i(),i()}, V2::{i(),i()}) -> 'ok'.
rectiv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5184),
  ok.

-doc """
[`gl:rect()`](`rectd/4`) supports efficient specification of rectangles as two
corner points. Each rectangle command takes four arguments, organized either as
two consecutive pairs of (x y) coordinates or as two pointers to arrays, each
containing an (x y) pair. The resulting rectangle is defined in the z=0 plane.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRect.xml)
""".
-spec rectsv(V1::{i(),i()}, V2::{i(),i()}) -> 'ok'.
rectsv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5185),
  ok.

-doc """
[`gl:vertexPointer/4`](`vertexPointer/4`) specifies the location and data format
of an array of vertex coordinates to use when rendering. `Size` specifies the
number of coordinates per vertex, and must be 2, 3, or 4. `Type` specifies the
data type of each coordinate, and `Stride` specifies the byte stride from one
vertex to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays. (Single-array storage may be more efficient
on some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertexPointer.xml)
""".
-spec vertexPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
vertexPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5186),
  ok.

-doc """
[`gl:normalPointer/3`](`normalPointer/3`) specifies the location and data format
of an array of normals to use when rendering. `Type` specifies the data type of
each normal coordinate, and `Stride` specifies the byte stride from one normal
to the next, allowing vertices and attributes to be packed into a single array
or stored in separate arrays. (Single-array storage may be more efficient on
some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormalPointer.xml)
""".
-spec normalPointer(Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
normalPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5188),
  ok.

-doc """
[`gl:colorPointer/4`](`colorPointer/4`) specifies the location and data format
of an array of color components to use when rendering. `Size` specifies the
number of components per color, and must be 3 or 4. `Type` specifies the data
type of each color component, and `Stride` specifies the byte stride from one
color to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays. (Single-array storage may be more efficient
on some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorPointer.xml)
""".
-spec colorPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
colorPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5190),
  ok.

-doc """
[`gl:indexPointer/3`](`indexPointer/3`) specifies the location and data format
of an array of color indexes to use when rendering. `Type` specifies the data
type of each color index and `Stride` specifies the byte stride from one color
index to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexPointer.xml)
""".
-spec indexPointer(Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
indexPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5192),
  ok.

-doc """
[`gl:texCoordPointer/4`](`texCoordPointer/4`) specifies the location and data
format of an array of texture coordinates to use when rendering. `Size`
specifies the number of coordinates per texture coordinate set, and must be 1,
2, 3, or 4. `Type` specifies the data type of each texture coordinate, and
`Stride` specifies the byte stride from one texture coordinate set to the next,
allowing vertices and attributes to be packed into a single array or stored in
separate arrays. (Single-array storage may be more efficient on some
implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoordPointer.xml)
""".
-spec texCoordPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
texCoordPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5194),
  ok.

-doc """
[`gl:edgeFlagPointer/2`](`edgeFlagPointer/2`) specifies the location and data
format of an array of boolean edge flags to use when rendering. `Stride`
specifies the byte stride from one edge flag to the next, allowing vertices and
attributes to be packed into a single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlagPointer.xml)
""".
-spec edgeFlagPointer(Stride::i(), Ptr::offset()|mem()) -> 'ok'.
edgeFlagPointer(Stride,Ptr) when is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Stride,Ptr,5196),
  ok.

-doc """
[`gl:arrayElement/1`](`arrayElement/1`) commands are used within
[`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pairs to specify
vertex and attribute data for point, line, and polygon primitives. If
`?GL_VERTEX_ARRAY` is enabled when [`gl:arrayElement/1`](`arrayElement/1`) is
called, a single vertex is drawn, using vertex and attribute data taken from
location `I` of the enabled arrays. If `?GL_VERTEX_ARRAY` is not enabled, no
drawing occurs but the attributes corresponding to the enabled arrays are
modified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glArrayElement.xml)
""".
-spec arrayElement(I::i()) -> 'ok'.
arrayElement(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5198),
  ok.

-doc """
[`gl:drawArrays/3`](`drawArrays/3`) specifies multiple geometric primitives with
very few subroutine calls. Instead of calling a GL procedure to pass each
individual vertex, normal, texture coordinate, edge flag, or color, you can
prespecify separate arrays of vertices, normals, and colors and use them to
construct a sequence of primitives with a single call to
[`gl:drawArrays/3`](`drawArrays/3`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml)
""".
-spec drawArrays(Mode::enum(), First::i(), Count::i()) -> 'ok'.
drawArrays(Mode,First,Count) when is_integer(Mode),is_integer(First),is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5199),
  ok.

-doc """
[`gl:drawElements/4`](`drawElements/4`) specifies multiple geometric primitives
with very few subroutine calls. Instead of calling a GL function to pass each
individual vertex, normal, texture coordinate, edge flag, or color, you can
prespecify separate arrays of vertices, normals, and so on, and use them to
construct a sequence of primitives with a single call to
[`gl:drawElements/4`](`drawElements/4`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElements.xhtml)
""".
-spec drawElements(Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem()) -> 'ok'.
drawElements(Mode,Count,Type,Indices) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,5200),
  ok.

-doc """
[`gl:interleavedArrays/3`](`interleavedArrays/3`) lets you specify and enable
individual color, normal, texture and vertex arrays whose elements are part of a
larger aggregate array element. For some implementations, this is more efficient
than specifying the arrays separately.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInterleavedArrays.xml)
""".
-spec interleavedArrays(Format::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
interleavedArrays(Format,Stride,Pointer) when is_integer(Format),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Format,Stride,Pointer,5202),
  ok.

-doc """
GL primitives can have either flat or smooth shading. Smooth shading, the
default, causes the computed colors of vertices to be interpolated as the
primitive is rasterized, typically assigning different colors to each resulting
pixel fragment. Flat shading selects the computed color of just one vertex and
assigns it to all the pixel fragments generated by rasterizing a single
primitive. In either case, the computed color of a vertex is the result of
lighting if lighting is enabled, or it is the current color at the time the
vertex was specified if lighting is disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glShadeModel.xml)
""".
-spec shadeModel(Mode::enum()) -> 'ok'.
shadeModel(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5204),
  ok.

-doc(#{equiv => lightiv/3}).
-spec lightf(Light::enum(), Pname::enum(), Param::f()) -> 'ok'.
lightf(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5205),
  ok.

-doc(#{equiv => lightiv/3}).
-spec lighti(Light::enum(), Pname::enum(), Param::i()) -> 'ok'.
lighti(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5206),
  ok.

-doc(#{equiv => lightiv/3}).
-spec lightfv(Light::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
lightfv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5207),
  ok.

-doc """
[`gl:light()`](`lightf/3`) sets the values of individual light source
parameters. `Light` names the light and is a symbolic name of the form
`?GL_LIGHT` i, where i ranges from 0 to the value of `?GL_MAX_LIGHTS` \- 1.
`Pname` specifies one of ten light source parameters, again by symbolic name.
`Params` is either a single value or a pointer to an array that contains the new
values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLight.xml)
""".
-spec lightiv(Light::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
lightiv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5208),
  ok.

-doc(#{equiv => getLightiv/2}).
-spec getLightfv(Light::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getLightfv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5209),
  rec(5209).

-doc """
[`gl:getLight()`](`getLightfv/2`) returns in `Params` the value or values of a
light source parameter. `Light` names the light and is a symbolic name of the
form `?GL_LIGHT` i where i ranges from 0 to the value of `?GL_MAX_LIGHTS` \- 1.
`?GL_MAX_LIGHTS` is an implementation dependent constant that is greater than or
equal to eight. `Pname` specifies one of ten light source parameters, again by
symbolic name.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetLight.xml)
""".
-spec getLightiv(Light::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getLightiv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5210),
  rec(5210).

-doc(#{equiv => lightModeliv/2}).
-spec lightModelf(Pname::enum(), Param::f()) -> 'ok'.
lightModelf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5211),
  ok.

-doc(#{equiv => lightModeliv/2}).
-spec lightModeli(Pname::enum(), Param::i()) -> 'ok'.
lightModeli(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5212),
  ok.

-doc(#{equiv => lightModeliv/2}).
-spec lightModelfv(Pname::enum(), Params::tuple()) -> 'ok'.
lightModelfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5213),
  ok.

-doc """
[`gl:lightModel()`](`lightModelf/2`) sets the lighting model parameter. `Pname`
names a parameter and `Params` gives the new value. There are three lighting
model parameters:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLightModel.xml)
""".
-spec lightModeliv(Pname::enum(), Params::tuple()) -> 'ok'.
lightModeliv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5214),
  ok.

-doc(#{equiv => materialiv/3}).
-spec materialf(Face::enum(), Pname::enum(), Param::f()) -> 'ok'.
materialf(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5215),
  ok.

-doc(#{equiv => materialiv/3}).
-spec materiali(Face::enum(), Pname::enum(), Param::i()) -> 'ok'.
materiali(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5216),
  ok.

-doc(#{equiv => materialiv/3}).
-spec materialfv(Face::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
materialfv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5217),
  ok.

-doc """
[`gl:material()`](`materialf/3`) assigns values to material parameters. There
are two matched sets of material parameters. One, the `front-facing` set, is
used to shade points, lines, bitmaps, and all polygons (when two-sided lighting
is disabled), or just front-facing polygons (when two-sided lighting is
enabled). The other set, `back-facing`, is used to shade back-facing polygons
only when two-sided lighting is enabled. Refer to the
[`gl:lightModel()`](`lightModelf/2`) reference page for details concerning one-
and two-sided lighting calculations.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMaterial.xml)
""".
-spec materialiv(Face::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
materialiv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5218),
  ok.

-doc(#{equiv => getMaterialiv/2}).
-spec getMaterialfv(Face::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getMaterialfv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5219),
  rec(5219).

-doc """
[`gl:getMaterial()`](`getMaterialfv/2`) returns in `Params` the value or values
of parameter `Pname` of material `Face`. Six parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMaterial.xml)
""".
-spec getMaterialiv(Face::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getMaterialiv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5220),
  rec(5220).

-doc """
[`gl:colorMaterial/2`](`colorMaterial/2`) specifies which material parameters
track the current color. When `?GL_COLOR_MATERIAL` is enabled, the material
parameter or parameters specified by `Mode`, of the material or materials
specified by `Face`, track the current color at all times.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorMaterial.xml)
""".
-spec colorMaterial(Face::enum(), Mode::enum()) -> 'ok'.
colorMaterial(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5221),
  ok.

-doc """
[`gl:pixelZoom/2`](`pixelZoom/2`) specifies values for the x and y zoom factors.
During the execution of [`gl:drawPixels/5`](`drawPixels/5`) or
[`gl:copyPixels/5`](`copyPixels/5`), if ( xr, yr) is the current raster
position, and a given element is in the mth row and nth column of the pixel
rectangle, then pixels whose centers are in the rectangle with corners at

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelZoom.xml)
""".
-spec pixelZoom(Xfactor::f(), Yfactor::f()) -> 'ok'.
pixelZoom(Xfactor,Yfactor) when is_float(Xfactor),is_float(Yfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Xfactor,Yfactor,5222),
  ok.

-doc(#{equiv => pixelStorei/2}).
-spec pixelStoref(Pname::enum(), Param::f()) -> 'ok'.
pixelStoref(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5223),
  ok.

-doc """
[`gl:pixelStore()`](`pixelStoref/2`) sets pixel storage modes that affect the
operation of subsequent [`gl:readPixels/7`](`readPixels/7`) as well as the
unpacking of texture patterns (see [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`),
[`gl:texSubImage3D/11`](`texSubImage3D/11`)),
[`gl:compressedTexImage1D/7`](`compressedTexImage1D/7`),
[`gl:compressedTexImage2D/8`](`compressedTexImage2D/8`),
[`gl:compressedTexImage3D/9`](`compressedTexImage3D/9`),
[`gl:compressedTexSubImage1D/7`](`compressedTexSubImage1D/7`),
[`gl:compressedTexSubImage2D/9`](`compressedTexSubImage2D/9`) or
[`gl:compressedTexSubImage1D/7`](`compressedTexSubImage1D/7`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPixelStore.xhtml)
""".
-spec pixelStorei(Pname::enum(), Param::i()) -> 'ok'.
pixelStorei(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5224),
  ok.

-doc(#{equiv => pixelTransferi/2}).
-spec pixelTransferf(Pname::enum(), Param::f()) -> 'ok'.
pixelTransferf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5225),
  ok.

-doc """
[`gl:pixelTransfer()`](`pixelTransferf/2`) sets pixel transfer modes that affect
the operation of subsequent [`gl:copyPixels/5`](`copyPixels/5`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`),
[`gl:drawPixels/5`](`drawPixels/5`), [`gl:readPixels/7`](`readPixels/7`),
[`gl:texImage1D/8`](`texImage1D/8`), [`gl:texImage2D/9`](`texImage2D/9`),
[`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`) commands. Additionally, if the
ARB_imaging subset is supported, the routines
[`gl:colorTable/6`](`colorTable/6`), [`gl:colorSubTable/6`](`colorSubTable/6`),
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`),
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`),
[`gl:histogram/4`](`histogram/4`), [`gl:minmax/3`](`minmax/3`), and
[`gl:separableFilter2D/8`](`separableFilter2D/8`) are also affected. The
algorithms that are specified by pixel transfer modes operate on pixels after
they are read from the frame buffer
([`gl:copyPixels/5`](`copyPixels/5`)[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`), and
[`gl:readPixels/7`](`readPixels/7`)), or unpacked from client memory
([`gl:drawPixels/5`](`drawPixels/5`), [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`)). Pixel transfer operations happen
in the same order, and in the same manner, regardless of the command that
resulted in the pixel operation. Pixel storage modes (see
[`gl:pixelStore()`](`pixelStoref/2`)) control the unpacking of pixels being read
from client memory and the packing of pixels being written back into client
memory.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelTransfer.xml)
""".
-spec pixelTransferi(Pname::enum(), Param::i()) -> 'ok'.
pixelTransferi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5226),
  ok.

-doc(#{equiv => pixelMapusv/3}).
-spec pixelMapfv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapfv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5227),
  ok.

-doc(#{equiv => pixelMapusv/3}).
-spec pixelMapuiv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapuiv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5228),
  ok.

-doc """
[`gl:pixelMap()`](`pixelMapfv/3`) sets up translation tables, or `maps`, used by
[`gl:copyPixels/5`](`copyPixels/5`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`),
[`gl:drawPixels/5`](`drawPixels/5`), [`gl:readPixels/7`](`readPixels/7`),
[`gl:texImage1D/8`](`texImage1D/8`), [`gl:texImage2D/9`](`texImage2D/9`),
[`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`). Additionally, if the ARB_imaging
subset is supported, the routines [`gl:colorTable/6`](`colorTable/6`),
[`gl:colorSubTable/6`](`colorSubTable/6`),
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`),
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`),
[`gl:histogram/4`](`histogram/4`), [`gl:minmax/3`](`minmax/3`), and
[`gl:separableFilter2D/8`](`separableFilter2D/8`). Use of these maps is
described completely in the [`gl:pixelTransfer()`](`pixelTransferf/2`) reference
page, and partly in the reference pages for the pixel and texture image
commands. Only the specification of the maps is described in this reference
page.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelMap.xml)
""".
-spec pixelMapusv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapusv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5229),
  ok.

-doc(#{equiv => getPixelMapusv/2}).
-spec getPixelMapfv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapfv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5230),
  rec(5230).

-doc(#{equiv => getPixelMapusv/2}).
-spec getPixelMapuiv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapuiv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5231),
  rec(5231).

-doc """
See the [`gl:pixelMap()`](`pixelMapfv/3`) reference page for a description of
the acceptable values for the `Map` parameter.
[`gl:getPixelMap()`](`getPixelMapfv/2`) returns in `Data` the contents of the
pixel map specified in `Map`. Pixel maps are used during the execution of
[`gl:readPixels/7`](`readPixels/7`), [`gl:drawPixels/5`](`drawPixels/5`),
[`gl:copyPixels/5`](`copyPixels/5`), [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`),
[`gl:texSubImage3D/11`](`texSubImage3D/11`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`), and
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`). to map color indices, stencil
indices, color components, and depth components to other values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPixelMap.xml)
""".
-spec getPixelMapusv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapusv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5232),
  rec(5232).

-doc """
A bitmap is a binary image. When drawn, the bitmap is positioned relative to the
current raster position, and frame buffer pixels corresponding to 1's in the
bitmap are written using the current raster color or index. Frame buffer pixels
corresponding to 0's in the bitmap are not modified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBitmap.xml)
""".
-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 'ok'
    when Width::i(), Height::i(), Xorig::f(), Yorig::f(), Xmove::f(), Ymove::f(), Bitmap::offset()|mem().
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when is_integer(Width),is_integer(Height),is_float(Xorig),is_float(Yorig),is_float(Xmove),is_float(Ymove),is_integer(Bitmap) orelse is_tuple(Bitmap) orelse is_binary(Bitmap) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap,5233),
  ok.

-doc """
[`gl:readPixels/7`](`readPixels/7`) and `glReadnPixels` return pixel data from
the frame buffer, starting with the pixel whose lower left corner is at location
(`X`, `Y`), into client memory starting at location `Data`. Several parameters
control the processing of the pixel data before it is placed into client memory.
These parameters are set with [`gl:pixelStore()`](`pixelStoref/2`). This
reference page describes the effects on [`gl:readPixels/7`](`readPixels/7`) and
`glReadnPixels` of most, but not all of the parameters specified by these three
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml)
""".
-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> 'ok'
    when X::i(), Y::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::mem().
readPixels(X,Y,Width,Height,Format,Type,Pixels) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Format,Type,Pixels,5235),
  rec(5235).

-doc """
[`gl:drawPixels/5`](`drawPixels/5`) reads pixel data from memory and writes it
into the frame buffer relative to the current raster position, provided that the
raster position is valid. Use [`gl:rasterPos()`](`rasterPos2d/2`) or
[`gl:windowPos()`](`windowPos2d/2`) to set the current raster position; use
[`gl:get()`](`getBooleanv/1`) with argument `?GL_CURRENT_RASTER_POSITION_VALID`
to determine if the specified raster position is valid, and
[`gl:get()`](`getBooleanv/1`) with argument `?GL_CURRENT_RASTER_POSITION` to
query the raster position.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDrawPixels.xml)
""".
-spec drawPixels(Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::offset()|mem()) -> 'ok'.
drawPixels(Width,Height,Format,Type,Pixels) when is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Format,Type,Pixels,5236),
  ok.

-doc """
[`gl:copyPixels/5`](`copyPixels/5`) copies a screen-aligned rectangle of pixels
from the specified frame buffer location to a region relative to the current
raster position. Its operation is well defined only if the entire pixel source
region is within the exposed portion of the window. Results of copies from
outside the window, or from regions of the window that are not exposed, are
hardware dependent and undefined.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyPixels.xml)
""".
-spec copyPixels(X::i(), Y::i(), Width::i(), Height::i(), Type::enum()) -> 'ok'.
copyPixels(X,Y,Width,Height,Type) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Type,5238),
  ok.

-doc """
Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. Stencil planes are first drawn into using GL drawing primitives, then
geometry and images are rendered using the stencil planes to mask out portions
of the screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFunc.xhtml)
""".
-spec stencilFunc(Func::enum(), Ref::i(), Mask::i()) -> 'ok'.
stencilFunc(Func,Ref,Mask) when is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,Mask,5239),
  ok.

-doc """
[`gl:stencilMask/1`](`stencilMask/1`) controls the writing of individual bits in
the stencil planes. The least significant n bits of `Mask`, where n is the
number of bits in the stencil buffer, specify a mask. Where a 1 appears in the
mask, it's possible to write to the corresponding bit in the stencil buffer.
Where a 0 appears, the corresponding bit is write-protected. Initially, all bits
are enabled for writing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMask.xhtml)
""".
-spec stencilMask(Mask::i()) -> 'ok'.
stencilMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5240),
  ok.

-doc """
Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOp.xhtml)
""".
-spec stencilOp(Fail::enum(), Zfail::enum(), Zpass::enum()) -> 'ok'.
stencilOp(Fail,Zfail,Zpass) when is_integer(Fail),is_integer(Zfail),is_integer(Zpass) ->
  IF = get_interface(),
  IF:queue_cmd(Fail,Zfail,Zpass,5241),
  ok.

-doc """
[`gl:clearStencil/1`](`clearStencil/1`) specifies the index used by
[`gl:clear/1`](`clear/1`) to clear the stencil buffer. `S` is masked with 2 m-1,
where m is the number of bits in the stencil buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearStencil.xhtml)
""".
-spec clearStencil(S::i()) -> 'ok'.
clearStencil(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5242),
  ok.

-doc(#{equiv => texGeniv/3}).
-spec texGend(Coord::enum(), Pname::enum(), Param::f()) -> 'ok'.
texGend(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5243),
  ok.

-doc(#{equiv => texGeniv/3}).
-spec texGenf(Coord::enum(), Pname::enum(), Param::f()) -> 'ok'.
texGenf(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5244),
  ok.

-doc(#{equiv => texGeniv/3}).
-spec texGeni(Coord::enum(), Pname::enum(), Param::i()) -> 'ok'.
texGeni(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5245),
  ok.

-doc(#{equiv => texGeniv/3}).
-spec texGendv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGendv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5246),
  ok.

-doc(#{equiv => texGeniv/3}).
-spec texGenfv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGenfv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5247),
  ok.

-doc """
[`gl:texGen()`](`texGend/3`) selects a texture-coordinate generation function or
supplies coefficients for one of the functions. `Coord` names one of the (`s`,
`t`, `r`, `q`) texture coordinates; it must be one of the symbols `?GL_S`,
`?GL_T`, `?GL_R`, or `?GL_Q`. `Pname` must be one of three symbolic constants:
`?GL_TEXTURE_GEN_MODE`, `?GL_OBJECT_PLANE`, or `?GL_EYE_PLANE`. If `Pname` is
`?GL_TEXTURE_GEN_MODE`, then `Params` chooses a mode, one of
`?GL_OBJECT_LINEAR`, `?GL_EYE_LINEAR`, `?GL_SPHERE_MAP`, `?GL_NORMAL_MAP`, or
`?GL_REFLECTION_MAP`. If `Pname` is either `?GL_OBJECT_PLANE` or
`?GL_EYE_PLANE`, `Params` contains coefficients for the corresponding texture
generation function.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexGen.xml)
""".
-spec texGeniv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGeniv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5248),
  ok.

-doc(#{equiv => getTexGeniv/2}).
-spec getTexGendv(Coord::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexGendv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5249),
  rec(5249).

-doc(#{equiv => getTexGeniv/2}).
-spec getTexGenfv(Coord::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexGenfv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5250),
  rec(5250).

-doc """
[`gl:getTexGen()`](`getTexGendv/2`) returns in `Params` selected parameters of a
texture coordinate generation function that was specified using
[`gl:texGen()`](`texGend/3`). `Coord` names one of the (`s`, `t`, `r`, `q`)
texture coordinates, using the symbolic constant `?GL_S`, `?GL_T`, `?GL_R`, or
`?GL_Q`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexGen.xml)
""".
-spec getTexGeniv(Coord::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexGeniv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5251),
  rec(5251).

-doc(#{equiv => texEnviv/3}).
-spec texEnvf(Target::enum(), Pname::enum(), Param::f()) -> 'ok'.
texEnvf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5252),
  ok.

-doc(#{equiv => texEnviv/3}).
-spec texEnvi(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
texEnvi(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5253),
  ok.

-doc(#{equiv => texEnviv/3}).
-spec texEnvfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texEnvfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5254),
  ok.

-doc """
A texture environment specifies how texture values are interpreted when a
fragment is textured. When `Target` is `?GL_TEXTURE_FILTER_CONTROL`, `Pname`
must be `?GL_TEXTURE_LOD_BIAS`. When `Target` is `?GL_TEXTURE_ENV`, `Pname` can
be `?GL_TEXTURE_ENV_MODE`, `?GL_TEXTURE_ENV_COLOR`, `?GL_COMBINE_RGB`,
`?GL_COMBINE_ALPHA`, `?GL_RGB_SCALE`, `?GL_ALPHA_SCALE`, `?GL_SRC0_RGB`,
`?GL_SRC1_RGB`, `?GL_SRC2_RGB`, `?GL_SRC0_ALPHA`, `?GL_SRC1_ALPHA`, or
`?GL_SRC2_ALPHA`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexEnv.xml)
""".
-spec texEnviv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texEnviv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5255),
  ok.

-doc(#{equiv => getTexEnviv/2}).
-spec getTexEnvfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexEnvfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5256),
  rec(5256).

-doc """
[`gl:getTexEnv()`](`getTexEnvfv/2`) returns in `Params` selected values of a
texture environment that was specified with [`gl:texEnv()`](`texEnvf/3`).
`Target` specifies a texture environment.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexEnv.xml)
""".
-spec getTexEnviv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexEnviv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5257),
  rec(5257).

-doc(#{equiv => texParameteriv/3}).
-spec texParameterf(Target::enum(), Pname::enum(), Param::f()) -> 'ok'.
texParameterf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5258),
  ok.

-doc(#{equiv => texParameteriv/3}).
-spec texParameteri(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
texParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5259),
  ok.

-doc(#{equiv => texParameteriv/3}).
-spec texParameterfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5260),
  ok.

-doc """
[`gl:texParameter()`](`texParameterf/3`) and
[`gl:textureParameter()`](`texParameterf/3`) assign the value or values in
`Params` to the texture parameter specified as `Pname`. For
[`gl:texParameter()`](`texParameterf/3`), `Target` defines the target texture,
either `?GL_TEXTURE_1D`, `?GL_TEXTURE_1D_ARRAY`, `?GL_TEXTURE_2D`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_2D_MULTISAMPLE`,
`?GL_TEXTURE_2D_MULTISAMPLE_ARRAY`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, or `?GL_TEXTURE_RECTANGLE`. The following symbols
are accepted in `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml)
""".
-spec texParameteriv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5261),
  ok.

-doc(#{equiv => getTexParameteriv/2}).
-spec getTexParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5262),
  rec(5262).

-doc """
[`gl:getTexParameter()`](`getTexParameterfv/2`) and `glGetTextureParameter`
return in `Params` the value or values of the texture parameter specified as
`Pname`. `Target` defines the target texture. `?GL_TEXTURE_1D`,
`?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, `?GL_TEXTURE_2D_MULTISAMPLE`, or
`?GL_TEXTURE_2D_MULTISAMPLE_ARRAY` specify one-, two-, or three-dimensional,
one-dimensional array, two-dimensional array, rectangle, cube-mapped or
cube-mapped array, two-dimensional multisample, or two-dimensional multisample
array texturing, respectively. `Pname` accepts the same symbols as
[`gl:texParameter()`](`texParameterf/3`), with the same interpretations:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexParameter.xhtml)
""".
-spec getTexParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5263),
  rec(5263).

-doc(#{equiv => getTexLevelParameteriv/3}).
-spec getTexLevelParameterfv(Target::enum(), Level::i(), Pname::enum()) -> {f()}.
getTexLevelParameterfv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5264),
  rec(5264).

-doc """
[`gl:getTexLevelParameterfv/3`](`getTexLevelParameterfv/3`),
[`gl:getTexLevelParameteriv/3`](`getTexLevelParameterfv/3`),
`glGetTextureLevelParameterfv` and `glGetTextureLevelParameteriv` return in
`Params` texture parameter values for a specific level-of-detail value,
specified as `Level`. For the first two functions, `Target` defines the target
texture, either `?GL_TEXTURE_1D`, `?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`,
`?GL_PROXY_TEXTURE_1D`, `?GL_PROXY_TEXTURE_2D`, `?GL_PROXY_TEXTURE_3D`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_X`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_Y`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_Z`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z`, or
`?GL_PROXY_TEXTURE_CUBE_MAP`. The remaining two take a `Texture` argument which
specifies the name of the texture object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexLevelParameter.xhtml)
""".
-spec getTexLevelParameteriv(Target::enum(), Level::i(), Pname::enum()) -> {i()}.
getTexLevelParameteriv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5265),
  rec(5265).

-doc """
Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable and disable one-dimensional
texturing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_TEXTURE_1D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml)
""".
-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels,5266),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml)
""".
-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Height::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels,5268),
  ok.

-doc """
[`gl:getTexImage/5`](`getTexImage/5`), `glGetnTexImage` and `glGetTextureImage`
functions return a texture image into `Pixels`. For
[`gl:getTexImage/5`](`getTexImage/5`) and `glGetnTexImage`, `Target` specifies
whether the desired texture image is one specified by
[`gl:texImage1D/8`](`texImage1D/8`) (`?GL_TEXTURE_1D`),
[`gl:texImage2D/9`](`texImage2D/9`) (`?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_2D` or any of `?GL_TEXTURE_CUBE_MAP_*`),
or [`gl:texImage3D/10`](`texImage3D/10`) (`?GL_TEXTURE_2D_ARRAY`,
`?GL_TEXTURE_3D`, `?GL_TEXTURE_CUBE_MAP_ARRAY`). For `glGetTextureImage`,
`Texture` specifies the texture object name. In addition to types of textures
accepted by [`gl:getTexImage/5`](`getTexImage/5`) and `glGetnTexImage`, the
function also accepts cube map texture objects (with effective target
`?GL_TEXTURE_CUBE_MAP`). `Level` specifies the level-of-detail number of the
desired image. `Format` and `Type` specify the format and type of the desired
image array. See the reference page for [`gl:texImage1D/8`](`texImage1D/8`) for
a description of the acceptable values for the `Format` and `Type` parameters,
respectively. For glGetnTexImage and glGetTextureImage functions, bufSize tells
the size of the buffer to receive the retrieved pixel data. `glGetnTexImage` and
`glGetTextureImage` do not write more than `BufSize` bytes into `Pixels`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexImage.xhtml)
""".
-spec getTexImage(Target::enum(), Level::i(), Format::enum(), Type::enum(), Pixels::mem()) -> 'ok'.
getTexImage(Target,Level,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Format,Type,Pixels,5270),
  rec(5270).

-doc """
[`gl:genTextures/1`](`genTextures/1`) returns `N` texture names in `Textures`.
There is no guarantee that the names form a contiguous set of integers; however,
it is guaranteed that none of the returned names was in use immediately before
the call to [`gl:genTextures/1`](`genTextures/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTextures.xhtml)
""".
-spec genTextures(N::i()) -> [i()].
genTextures(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5271),
  rec(5271).

-doc """
[`gl:deleteTextures/1`](`deleteTextures/1`) deletes `N` textures named by the
elements of the array `Textures`. After a texture is deleted, it has no contents
or dimensionality, and its name is free for reuse (for example by
[`gl:genTextures/1`](`genTextures/1`)). If a texture that is currently bound is
deleted, the binding reverts to 0 (the default texture).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTextures.xhtml)
""".
-spec deleteTextures(Textures::[i()]) -> 'ok'.
deleteTextures(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5272),
  ok.

-doc """
[`gl:bindTexture/2`](`bindTexture/2`) lets you create or use a named texture.
Calling [`gl:bindTexture/2`](`bindTexture/2`) with `Target` set to
`?GL_TEXTURE_1D`, `?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, `?GL_TEXTURE_BUFFER`, `?GL_TEXTURE_2D_MULTISAMPLE`
or `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY` and `Texture` set to the name of the new
texture binds the texture name to the target. When a texture is bound to a
target, the previous binding for that target is automatically broken.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTexture.xhtml)
""".
-spec bindTexture(Target::enum(), Texture::i()) -> 'ok'.
bindTexture(Target,Texture) when is_integer(Target),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Texture,5273),
  ok.

-doc """
[`gl:prioritizeTextures/2`](`prioritizeTextures/2`) assigns the `N` texture
priorities given in `Priorities` to the `N` textures named in `Textures`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPrioritizeTextures.xml)
""".
-spec prioritizeTextures(Textures::[i()], Priorities::[clamp()]) -> 'ok'.
prioritizeTextures(Textures,Priorities) when is_list(Textures),is_list(Priorities) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,Priorities,5274),
  ok.

-doc """
GL establishes a \`\`working set'' of textures that are resident in texture
memory. These textures can be bound to a texture target much more efficiently
than textures that are not resident.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAreTexturesResident.xml)
""".
-spec areTexturesResident(Textures::[i()]) -> {0|1,Residences::[0|1]}.
areTexturesResident(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5275),
  rec(5275).

-doc """
[`gl:isTexture/1`](`isTexture/1`) returns `?GL_TRUE` if `Texture` is currently
the name of a texture. If `Texture` is zero, or is a non-zero value that is not
currently the name of a texture, or if an error occurs,
[`gl:isTexture/1`](`isTexture/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTexture.xhtml)
""".
-spec isTexture(Texture::i()) -> 0|1.
isTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5276),
  rec(5276).

-doc """
Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable or disable one-dimensional
texturing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_TEXTURE_1D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage1D.xhtml)
""".
-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,Type,Pixels,5277),
  ok.

-doc """
Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage2D.xhtml)
""".
-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels,5279),
  ok.

-doc """
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`) defines a one-dimensional texture
image with pixels from the current `?GL_READ_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage1D.xhtml)
""".
-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Border::i().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Border,5281),
  ok.

-doc """
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`) defines a two-dimensional texture
image, or cube-map texture image with pixels from the current `?GL_READ_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage2D.xhtml)
""".
-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Height::i(), Border::i().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Height,Border,5282),
  ok.

-doc """
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`) and `glCopyTextureSubImage1D`
replace a portion of a one-dimensional texture image with pixels from the
current `?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:texSubImage1D/7`](`texSubImage1D/7`)). For
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`), the texture object that is
bound to `Target` will be used for the process. For `glCopyTextureSubImage1D`,
`Texture` tells which texture object should be used for the purpose of the call.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage1D.xhtml)
""".
-spec copyTexSubImage1D(Target::enum(), Level::i(), Xoffset::i(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,X,Y,Width,5283),
  ok.

-doc """
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`) and `glCopyTextureSubImage2D`
replace a rectangular portion of a two-dimensional texture image, cube-map
texture image, rectangular image, or a linear portion of a number of slices of a
one-dimensional array texture with pixels from the current `?GL_READ_BUFFER`
(rather than from main memory, as is the case for
[`gl:texSubImage2D/9`](`texSubImage2D/9`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage2D.xhtml)
""".
-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), X::i(), Y::i(), Width::i(), Height::i().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,X,Y,Width,Height,5284),
  ok.

-doc(#{equiv => map1f/6}).
-spec map1d(Target::enum(), U1::f(), U2::f(), Stride::i(), Order::i(), Points::binary()) -> 'ok'.
map1d(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5285),
  ok.

-doc """
Evaluators provide a way to use polynomial or rational polynomial mapping to
produce vertices, normals, texture coordinates, and colors. The values produced
by an evaluator are sent to further stages of GL processing just as if they had
been presented using [`gl:vertex()`](`vertex2d/2`),
[`gl:normal()`](`normal3b/3`), [`gl:texCoord()`](`texCoord1d/1`), and
[`gl:color()`](`color3b/3`) commands, except that the generated values do not
update the current normal, texture coordinates, or color.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMap1.xml)
""".
-spec map1f(Target::enum(), U1::f(), U2::f(), Stride::i(), Order::i(), Points::binary()) -> 'ok'.
map1f(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5286),
  ok.

-doc(#{equiv => map2f/10}).
-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok'
    when Target::enum(), U1::f(), U2::f(), Ustride::i(), Uorder::i(), V1::f(), V2::f(), Vstride::i(), Vorder::i(), Points::binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5287),
  ok.

-doc """
Evaluators provide a way to use polynomial or rational polynomial mapping to
produce vertices, normals, texture coordinates, and colors. The values produced
by an evaluator are sent on to further stages of GL processing just as if they
had been presented using [`gl:vertex()`](`vertex2d/2`),
[`gl:normal()`](`normal3b/3`), [`gl:texCoord()`](`texCoord1d/1`), and
[`gl:color()`](`color3b/3`) commands, except that the generated values do not
update the current normal, texture coordinates, or color.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMap2.xml)
""".
-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok'
    when Target::enum(), U1::f(), U2::f(), Ustride::i(), Uorder::i(), V1::f(), V2::f(), Vstride::i(), Vorder::i(), Points::binary().
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5288),
  ok.

-doc(#{equiv => getMapiv/3}).
-spec getMapdv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapdv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5289),
  rec(5289).

-doc(#{equiv => getMapiv/3}).
-spec getMapfv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapfv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5290),
  rec(5290).

-doc """
`glMap1` and `glMap2` define evaluators. [`gl:getMap()`](`getMapdv/3`) returns
evaluator parameters. `Target` chooses a map, `Query` selects a specific
parameter, and `V` points to storage where the values will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMap.xml)
""".
-spec getMapiv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapiv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5291),
  rec(5291).

-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord1d(U::f()) -> 'ok'.
evalCoord1d(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5292),
  ok.

-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord1f(U::f()) -> 'ok'.
evalCoord1f(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5293),
  ok.

-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord1dv({U::f()}) -> 'ok'.
evalCoord1dv({U}) ->  evalCoord1d(U).
-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord1fv({U::f()}) -> 'ok'.
evalCoord1fv({U}) ->  evalCoord1f(U).
-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord2d(U::f(), V::f()) -> 'ok'.
evalCoord2d(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5294),
  ok.

-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord2f(U::f(), V::f()) -> 'ok'.
evalCoord2f(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5295),
  ok.

-doc(#{equiv => evalCoord2fv/1}).
-spec evalCoord2dv({U::f(), V::f()}) -> 'ok'.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).
-doc """
[`gl:evalCoord1()`](`evalCoord1d/1`) evaluates enabled one-dimensional maps at
argument `U`. [`gl:evalCoord2()`](`evalCoord1d/1`) does the same for
two-dimensional maps using two domain values, `U` and `V`. To define a map, call
`glMap1` and `glMap2`; to enable and disable it, call
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalCoord.xml)
""".
-spec evalCoord2fv({U::f(), V::f()}) -> 'ok'.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).
-doc(#{equiv => mapGrid2f/6}).
-spec mapGrid1d(Un::i(), U1::f(), U2::f()) -> 'ok'.
mapGrid1d(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5296),
  ok.

-doc(#{equiv => mapGrid2f/6}).
-spec mapGrid1f(Un::i(), U1::f(), U2::f()) -> 'ok'.
mapGrid1f(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5297),
  ok.

-doc(#{equiv => mapGrid2f/6}).
-spec mapGrid2d(Un::i(), U1::f(), U2::f(), Vn::i(), V1::f(), V2::f()) -> 'ok'.
mapGrid2d(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5298),
  ok.

-doc """
[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used
together to efficiently generate and evaluate a series of evenly-spaced map
domain values. [`gl:evalMesh()`](`evalMesh1/3`) steps through the integer domain
of a one- or two-dimensional grid, whose range is the domain of the evaluation
maps specified by `glMap1` and `glMap2`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMapGrid.xml)
""".
-spec mapGrid2f(Un::i(), U1::f(), U2::f(), Vn::i(), V1::f(), V2::f()) -> 'ok'.
mapGrid2f(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5299),
  ok.

-doc(#{equiv => evalPoint2/2}).
-spec evalPoint1(I::i()) -> 'ok'.
evalPoint1(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5300),
  ok.

-doc """
[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used in
tandem to efficiently generate and evaluate a series of evenly spaced map domain
values. [`gl:evalPoint()`](`evalPoint1/1`) can be used to evaluate a single grid
point in the same gridspace that is traversed by
[`gl:evalMesh()`](`evalMesh1/3`). Calling [`gl:evalPoint1/1`](`evalPoint1/1`) is
equivalent to calling glEvalCoord1( i.ð u+u 1 ); where ð u=(u 2-u 1)/n

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalPoint.xml)
""".
-spec evalPoint2(I::i(), J::i()) -> 'ok'.
evalPoint2(I,J) when is_integer(I),is_integer(J) ->
  IF = get_interface(),
  IF:queue_cmd(I,J,5301),
  ok.

-doc(#{equiv => evalMesh2/5}).
-spec evalMesh1(Mode::enum(), I1::i(), I2::i()) -> 'ok'.
evalMesh1(Mode,I1,I2) when is_integer(Mode),is_integer(I1),is_integer(I2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,5302),
  ok.

-doc """
[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used in
tandem to efficiently generate and evaluate a series of evenly-spaced map domain
values. [`gl:evalMesh()`](`evalMesh1/3`) steps through the integer domain of a
one- or two-dimensional grid, whose range is the domain of the evaluation maps
specified by `glMap1` and `glMap2`. `Mode` determines whether the resulting
vertices are connected as points, lines, or filled polygons.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalMesh.xml)
""".
-spec evalMesh2(Mode::enum(), I1::i(), I2::i(), J1::i(), J2::i()) -> 'ok'.
evalMesh2(Mode,I1,I2,J1,J2) when is_integer(Mode),is_integer(I1),is_integer(I2),is_integer(J1),is_integer(J2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,J1,J2,5303),
  ok.

-doc(#{equiv => fogiv/2}).
-spec fogf(Pname::enum(), Param::f()) -> 'ok'.
fogf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5304),
  ok.

-doc(#{equiv => fogiv/2}).
-spec fogi(Pname::enum(), Param::i()) -> 'ok'.
fogi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5305),
  ok.

-doc(#{equiv => fogiv/2}).
-spec fogfv(Pname::enum(), Params::tuple()) -> 'ok'.
fogfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5306),
  ok.

-doc """
Fog is initially disabled. While enabled, fog affects rasterized geometry,
bitmaps, and pixel blocks, but not buffer clear operations. To enable and
disable fog, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_FOG`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFog.xml)
""".
-spec fogiv(Pname::enum(), Params::tuple()) -> 'ok'.
fogiv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5307),
  ok.

-doc """
The [`gl:feedbackBuffer/3`](`feedbackBuffer/3`) function controls feedback.
Feedback, like selection, is a GL mode. The mode is selected by calling
[`gl:renderMode/1`](`renderMode/1`) with `?GL_FEEDBACK`. When the GL is in
feedback mode, no pixels are produced by rasterization. Instead, information
about primitives that would have been rasterized is fed back to the application
using the GL.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFeedbackBuffer.xml)
""".
-spec feedbackBuffer(Size::i(), Type::enum(), Buffer::mem()) -> 'ok'.
feedbackBuffer(Size,Type,Buffer) when is_integer(Size),is_integer(Type),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Buffer,5308),
  rec(5308).

-doc "[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPassThrough.xml)".
-spec passThrough(Token::f()) -> 'ok'.
passThrough(Token) when is_float(Token) ->
  IF = get_interface(),
  IF:queue_cmd(Token,5309),
  ok.

-doc """
[`gl:selectBuffer/2`](`selectBuffer/2`) has two arguments: `Buffer` is a pointer
to an array of unsigned integers, and `Size` indicates the size of the array.
`Buffer` returns values from the name stack (see
[`gl:initNames/0`](`initNames/0`), [`gl:loadName/1`](`loadName/1`),
[`gl:pushName/1`](`pushName/1`)) when the rendering mode is `?GL_SELECT` (see
[`gl:renderMode/1`](`renderMode/1`)). [`gl:selectBuffer/2`](`selectBuffer/2`)
must be issued before selection mode is enabled, and it must not be issued while
the rendering mode is `?GL_SELECT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSelectBuffer.xml)
""".
-spec selectBuffer(Size::i(), Buffer::mem()) -> 'ok'.
selectBuffer(Size,Buffer) when is_integer(Size),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Buffer,5310),
  rec(5310).

-doc """
The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers.
[`gl:initNames/0`](`initNames/0`) causes the name stack to be initialized to its
default empty state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInitNames.xml)
""".
-spec initNames() -> 'ok'.
initNames()  ->
  IF = get_interface(),
  IF:queue_cmd(5311),
  ok.

-doc """
The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers
and is initially empty.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadName.xml)
""".
-spec loadName(Name::i()) -> 'ok'.
loadName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5312),
  ok.

-doc """
The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers
and is initially empty.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushName.xml)
""".
-spec pushName(Name::i()) -> 'ok'.
pushName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5313),
  ok.

-doc(#{equiv => pushName/1}).
-spec popName() -> 'ok'.
popName()  ->
  IF = get_interface(),
  IF:queue_cmd(5314),
  ok.

-doc """
[`gl:drawRangeElements/6`](`drawRangeElements/6`) is a restricted form of
[`gl:drawElements/4`](`drawElements/4`). `Mode`, and `Count` match the
corresponding arguments to [`gl:drawElements/4`](`drawElements/4`), with the
additional constraint that all values in the arrays `Count` must lie between
`Start` and `End`, inclusive.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElements.xhtml)
""".
-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 'ok'
    when Mode::enum(), Start::i(), End::i(), Count::i(), Type::enum(), Indices::offset()|mem().
drawRangeElements(Mode,Start,End,Count,Type,Indices) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,5315),
  ok.

-doc """
Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable and disable
three-dimensional texturing, call [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) with argument `?GL_TEXTURE_3D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3D.xhtml)
""".
-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels,5317),
  ok.

-doc """
Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage3D.xhtml)
""".
-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels,5319),
  ok.

-doc """
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`) and `glCopyTextureSubImage3D`
functions replace a rectangular portion of a three-dimensional or
two-dimensional array texture image with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:texSubImage3D/11`](`texSubImage3D/11`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage3D.xhtml)
""".
-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), X::i(), Y::i(), Width::i(), Height::i().
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height,5321),
  ok.

-doc """
[`gl:activeTexture/1`](`activeTexture/1`) selects which texture unit subsequent
texture state calls will affect. The number of texture units an implementation
supports is implementation dependent, but must be at least 80.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveTexture.xhtml)
""".
-spec activeTexture(Texture::enum()) -> 'ok'.
activeTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5322),
  ok.

-doc """
Multisampling samples a pixel multiple times at various implementation-dependent
subpixel locations to generate antialiasing effects. Multisampling transparently
antialiases points, lines, polygons, and images if it is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleCoverage.xhtml)
""".
-spec sampleCoverage(Value::clamp(), Invert::0|1) -> 'ok'.
sampleCoverage(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5323),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage3D.xhtml)
""".
-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data,5324),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage2D.xhtml)
""".
-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Height::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data,5326),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage1D.xhtml)
""".
-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Border,ImageSize,Data,5328),
  ok.

-doc(#{equiv => compressedTextureSubImage3D/11}).
-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5330),
  ok.

-doc(#{equiv => compressedTextureSubImage2D/9}).
-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5332),
  ok.

-doc(#{equiv => compressedTextureSubImage1D/7}).
-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,ImageSize,Data,5334),
  ok.

-doc """
[`gl:getCompressedTexImage/3`](`getCompressedTexImage/3`) and
`glGetnCompressedTexImage` return the compressed texture image associated with
`Target` and `Lod` into `Pixels`. `glGetCompressedTextureImage` serves the same
purpose, but instead of taking a texture target, it takes the ID of the texture
object. `Pixels` should be an array of `BufSize` bytes for
`glGetnCompresedTexImage` and `glGetCompressedTextureImage` functions, and of
`?GL_TEXTURE_COMPRESSED_IMAGE_SIZE` bytes in case of
[`gl:getCompressedTexImage/3`](`getCompressedTexImage/3`). If the actual data
takes less space than `BufSize`, the remaining bytes will not be touched.
`Target` specifies the texture target, to which the texture the data the
function should extract the data from is bound to. `Lod` specifies the
level-of-detail number of the desired image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetCompressedTexImage.xhtml)
""".
-spec getCompressedTexImage(Target::enum(), Lod::i(), Img::mem()) -> 'ok'.
getCompressedTexImage(Target,Lod,Img) when is_integer(Target),is_integer(Lod),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Lod,Img,5336),
  rec(5336).

-doc """
[`gl:clientActiveTexture/1`](`clientActiveTexture/1`) selects the vertex array
client state parameters to be modified by
[`gl:texCoordPointer/4`](`texCoordPointer/4`), and enabled or disabled with
[`gl:enableClientState/1`](`enableClientState/1`) or
[`gl:disableClientState/1`](`enableClientState/1`), respectively, when called
with a parameter of `?GL_TEXTURE_COORD_ARRAY`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClientActiveTexture.xml)
""".
-spec clientActiveTexture(Texture::enum()) -> 'ok'.
clientActiveTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5337),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1d(Target::enum(), S::f()) -> 'ok'.
multiTexCoord1d(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5338),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1dv(Target::enum(), {S::f()}) -> 'ok'.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1f(Target::enum(), S::f()) -> 'ok'.
multiTexCoord1f(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5339),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1fv(Target::enum(), {S::f()}) -> 'ok'.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1i(Target::enum(), S::i()) -> 'ok'.
multiTexCoord1i(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5340),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1iv(Target::enum(), {S::i()}) -> 'ok'.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1s(Target::enum(), S::i()) -> 'ok'.
multiTexCoord1s(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5341),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord1sv(Target::enum(), {S::i()}) -> 'ok'.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2d(Target::enum(), S::f(), T::f()) -> 'ok'.
multiTexCoord2d(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5342),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2dv(Target::enum(), {S::f(), T::f()}) -> 'ok'.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2f(Target::enum(), S::f(), T::f()) -> 'ok'.
multiTexCoord2f(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5343),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2fv(Target::enum(), {S::f(), T::f()}) -> 'ok'.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2i(Target::enum(), S::i(), T::i()) -> 'ok'.
multiTexCoord2i(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5344),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2iv(Target::enum(), {S::i(), T::i()}) -> 'ok'.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2s(Target::enum(), S::i(), T::i()) -> 'ok'.
multiTexCoord2s(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5345),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord2sv(Target::enum(), {S::i(), T::i()}) -> 'ok'.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3d(Target::enum(), S::f(), T::f(), R::f()) -> 'ok'.
multiTexCoord3d(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5346),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3dv(Target::enum(), {S::f(), T::f(), R::f()}) -> 'ok'.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3f(Target::enum(), S::f(), T::f(), R::f()) -> 'ok'.
multiTexCoord3f(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5347),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3fv(Target::enum(), {S::f(), T::f(), R::f()}) -> 'ok'.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3i(Target::enum(), S::i(), T::i(), R::i()) -> 'ok'.
multiTexCoord3i(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5348),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3iv(Target::enum(), {S::i(), T::i(), R::i()}) -> 'ok'.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3s(Target::enum(), S::i(), T::i(), R::i()) -> 'ok'.
multiTexCoord3s(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5349),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord3sv(Target::enum(), {S::i(), T::i(), R::i()}) -> 'ok'.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4d(Target::enum(), S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
multiTexCoord4d(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5350),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4dv(Target::enum(), {S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4f(Target::enum(), S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
multiTexCoord4f(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5351),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4fv(Target::enum(), {S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4i(Target::enum(), S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
multiTexCoord4i(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5352),
  ok.

-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4iv(Target::enum(), {S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).
-doc(#{equiv => multiTexCoord4sv/2}).
-spec multiTexCoord4s(Target::enum(), S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
multiTexCoord4s(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5353),
  ok.

-doc """
[`gl:multiTexCoord()`](`multiTexCoord1d/2`) specifies texture coordinates in
one, two, three, or four dimensions.
[`gl:multiTexCoord1()`](`multiTexCoord1d/2`) sets the current texture
coordinates to (s 0 0 1); a call to [`gl:multiTexCoord2()`](`multiTexCoord1d/2`)
sets them to (s t 0 1). Similarly, [`gl:multiTexCoord3()`](`multiTexCoord1d/2`)
specifies the texture coordinates as (s t r 1), and
[`gl:multiTexCoord4()`](`multiTexCoord1d/2`) defines all four components
explicitly as (s t r q).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultiTexCoord.xml)
""".
-spec multiTexCoord4sv(Target::enum(), {S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).
-doc """
[`gl:loadTransposeMatrix()`](`loadTransposeMatrixf/1`) replaces the current
matrix with the one whose elements are specified by `M`. The current matrix is
the projection matrix, modelview matrix, or texture matrix, depending on the
current matrix mode (see [`gl:matrixMode/1`](`matrixMode/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadTransposeMatrix.xml)
""".
-spec loadTransposeMatrixf(M::matrix()) -> 'ok'.
loadTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5354),
  ok.

-doc(#{equiv => loadTransposeMatrixf/1}).
-spec loadTransposeMatrixd(M::matrix()) -> 'ok'.
loadTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5355),
  ok.

-doc """
[`gl:multTransposeMatrix()`](`multTransposeMatrixf/1`) multiplies the current
matrix with the one specified using `M`, and replaces the current matrix with
the product.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultTransposeMatrix.xml)
""".
-spec multTransposeMatrixf(M::matrix()) -> 'ok'.
multTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5356),
  ok.

-doc(#{equiv => multTransposeMatrixf/1}).
-spec multTransposeMatrixd(M::matrix()) -> 'ok'.
multTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5357),
  ok.

-doc(#{equiv => blendFuncSeparatei/5}).
-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 'ok'
    when SfactorRGB::enum(), DfactorRGB::enum(), SfactorAlpha::enum(), DfactorAlpha::enum().
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) when is_integer(SfactorRGB),is_integer(DfactorRGB),is_integer(SfactorAlpha),is_integer(DfactorAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha,5358),
  ok.

-doc """
[`gl:multiDrawArrays/3`](`multiDrawArrays/3`) specifies multiple sets of
geometric primitives with very few subroutine calls. Instead of calling a GL
procedure to pass each individual vertex, normal, texture coordinate, edge flag,
or color, you can prespecify separate arrays of vertices, normals, and colors
and use them to construct a sequence of primitives with a single call to
[`gl:multiDrawArrays/3`](`multiDrawArrays/3`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawArrays.xhtml)
""".
-spec multiDrawArrays(Mode::enum(), First::[integer()]|mem(), Count::[integer()]|mem()) -> 'ok'.
multiDrawArrays(Mode,First,Count) when is_integer(Mode),is_list(First) orelse is_tuple(First) orelse is_binary(First),is_list(Count) orelse is_tuple(Count) orelse is_binary(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5359),
  ok.

-doc(#{equiv => pointParameteriv/2}).
-spec pointParameterf(Pname::enum(), Param::f()) -> 'ok'.
pointParameterf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5361),
  ok.

-doc(#{equiv => pointParameteriv/2}).
-spec pointParameterfv(Pname::enum(), Params::tuple()) -> 'ok'.
pointParameterfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5362),
  ok.

-doc(#{equiv => pointParameteriv/2}).
-spec pointParameteri(Pname::enum(), Param::i()) -> 'ok'.
pointParameteri(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5363),
  ok.

-doc """
The following values are accepted for `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointParameter.xhtml)
""".
-spec pointParameteriv(Pname::enum(), Params::tuple()) -> 'ok'.
pointParameteriv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5364),
  ok.

-doc(#{equiv => fogCoordfv/1}).
-spec fogCoordf(Coord::f()) -> 'ok'.
fogCoordf(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5365),
  ok.

-doc """
[`gl:fogCoord()`](`fogCoordf/1`) specifies the fog coordinate that is associated
with each vertex and the current raster position. The value specified is
interpolated and used in computing the fog color (see [`gl:fog()`](`fogf/2`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoord.xml)
""".
-spec fogCoordfv({Coord::f()}) -> 'ok'.
fogCoordfv({Coord}) ->  fogCoordf(Coord).
-doc(#{equiv => fogCoordfv/1}).
-spec fogCoordd(Coord::f()) -> 'ok'.
fogCoordd(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5366),
  ok.

-doc(#{equiv => fogCoordfv/1}).
-spec fogCoorddv({Coord::f()}) -> 'ok'.
fogCoorddv({Coord}) ->  fogCoordd(Coord).
-doc """
[`gl:fogCoordPointer/3`](`fogCoordPointer/3`) specifies the location and data
format of an array of fog coordinates to use when rendering. `Type` specifies
the data type of each fog coordinate, and `Stride` specifies the byte stride
from one fog coordinate to the next, allowing vertices and attributes to be
packed into a single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoordPointer.xml)
""".
-spec fogCoordPointer(Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
fogCoordPointer(Type,Stride,Pointer) when is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Pointer,5367),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3b(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5369),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3bv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3d(Red::f(), Green::f(), Blue::f()) -> 'ok'.
secondaryColor3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5370),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3dv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3f(Red::f(), Green::f(), Blue::f()) -> 'ok'.
secondaryColor3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5371),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3fv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3i(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5372),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3iv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3s(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5373),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3sv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3ub(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5374),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3ubv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3ui(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5375),
  ok.

-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3uiv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).
-doc(#{equiv => secondaryColor3usv/1}).
-spec secondaryColor3us(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5376),
  ok.

-doc """
The GL stores both a primary four-valued RGBA color and a secondary four-valued
RGBA color (where alpha is always set to 0.0) that is associated with every
vertex.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColor.xml)
""".
-spec secondaryColor3usv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).
-doc """
[`gl:secondaryColorPointer/4`](`secondaryColorPointer/4`) specifies the location
and data format of an array of color components to use when rendering. `Size`
specifies the number of components per color, and must be 3. `Type` specifies
the data type of each color component, and `Stride` specifies the byte stride
from one color to the next, allowing vertices and attributes to be packed into a
single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColorPointer.xml)
""".
-spec secondaryColorPointer(Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
secondaryColorPointer(Size,Type,Stride,Pointer) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Pointer,5377),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2d(X::f(), Y::f()) -> 'ok'.
windowPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5379),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2dv({X::f(), Y::f()}) -> 'ok'.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2f(X::f(), Y::f()) -> 'ok'.
windowPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5380),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2fv({X::f(), Y::f()}) -> 'ok'.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2i(X::i(), Y::i()) -> 'ok'.
windowPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5381),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2iv({X::i(), Y::i()}) -> 'ok'.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2s(X::i(), Y::i()) -> 'ok'.
windowPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5382),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos2sv({X::i(), Y::i()}) -> 'ok'.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3d(X::f(), Y::f(), Z::f()) -> 'ok'.
windowPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5383),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3f(X::f(), Y::f(), Z::f()) -> 'ok'.
windowPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5384),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3i(X::i(), Y::i(), Z::i()) -> 'ok'.
windowPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5385),
  ok.

-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).
-doc(#{equiv => windowPos3sv/1}).
-spec windowPos3s(X::i(), Y::i(), Z::i()) -> 'ok'.
windowPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5386),
  ok.

-doc """
The GL maintains a 3D position in window coordinates. This position, called the
raster position, is used to position pixel and bitmap write operations. It is
maintained with subpixel accuracy. See [`gl:bitmap/7`](`bitmap/7`),
[`gl:drawPixels/5`](`drawPixels/5`), and [`gl:copyPixels/5`](`copyPixels/5`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glWindowPos.xml)
""".
-spec windowPos3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).
-doc """
The `?GL_BLEND_COLOR` may be used to calculate the source and destination
blending factors. The color components are clamped to the range \[0 1] before
being stored. See [`gl:blendFunc/2`](`blendFunc/2`) for a complete description
of the blending operations. Initially the `?GL_BLEND_COLOR` is set to (0, 0, 0,
0).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendColor.xhtml)
""".
-spec blendColor(Red::clamp(), Green::clamp(), Blue::clamp(), Alpha::clamp()) -> 'ok'.
blendColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5387),
  ok.

-doc(#{equiv => blendEquationi/2}).
-spec blendEquation(Mode::enum()) -> 'ok'.
blendEquation(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5388),
  ok.

-doc """
[`gl:genQueries/1`](`genQueries/1`) returns `N` query object names in `Ids`.
There is no guarantee that the names form a contiguous set of integers; however,
it is guaranteed that none of the returned names was in use immediately before
the call to [`gl:genQueries/1`](`genQueries/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenQueries.xhtml)
""".
-spec genQueries(N::i()) -> [i()].
genQueries(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5389),
  rec(5389).

-doc """
[`gl:deleteQueries/1`](`deleteQueries/1`) deletes `N` query objects named by the
elements of the array `Ids`. After a query object is deleted, it has no
contents, and its name is free for reuse (for example by
[`gl:genQueries/1`](`genQueries/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteQueries.xhtml)
""".
-spec deleteQueries(Ids::[i()]) -> 'ok'.
deleteQueries(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5390),
  ok.

-doc """
[`gl:isQuery/1`](`isQuery/1`) returns `?GL_TRUE` if `Id` is currently the name
of a query object. If `Id` is zero, or is a non-zero value that is not currently
the name of a query object, or if an error occurs, [`gl:isQuery/1`](`isQuery/1`)
returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsQuery.xhtml)
""".
-spec isQuery(Id::i()) -> 0|1.
isQuery(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5391),
  rec(5391).

-doc(#{equiv => endQuery/1}).
-spec beginQuery(Target::enum(), Id::i()) -> 'ok'.
beginQuery(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5392),
  ok.

-doc """
[`gl:beginQuery/2`](`beginQuery/2`) and [`gl:endQuery/1`](`beginQuery/2`)
delimit the boundaries of a query object. `Query` must be a name previously
returned from a call to [`gl:genQueries/1`](`genQueries/1`). If a query object
with name `Id` does not yet exist it is created with the type determined by
`Target`. `Target` must be one of `?GL_SAMPLES_PASSED`,
`?GL_ANY_SAMPLES_PASSED`, `?GL_PRIMITIVES_GENERATED`,
`?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN`, or `?GL_TIME_ELAPSED`. The behavior
of the query object depends on its type and is as follows.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQuery.xhtml)
""".
-spec endQuery(Target::enum()) -> 'ok'.
endQuery(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5393),
  ok.

-doc """
[`gl:getQueryiv/2`](`getQueryiv/2`) returns in `Params` a selected parameter of
the query object target specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryiv.xhtml)
""".
-spec getQueryiv(Target::enum(), Pname::enum()) -> i().
getQueryiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5394),
  rec(5394).

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryObjectiv(Id::i(), Pname::enum()) -> i().
getQueryObjectiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5395),
  rec(5395).

-doc """
These commands return a selected parameter of the query object specified by
`Id`. [`gl:getQueryObject()`](`getQueryObjectiv/2`) returns in `Params` a
selected parameter of the query object specified by `Id`.
[`gl:getQueryBufferObject()`](`getQueryObjectiv/2`) returns in `Buffer` a
selected parameter of the query object specified by `Id`, by writing it to
`Buffer`'s data store at the byte offset specified by `Offset`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryObject.xhtml)
""".
-spec getQueryObjectuiv(Id::i(), Pname::enum()) -> i().
getQueryObjectuiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5396),
  rec(5396).

-doc """
[`gl:bindBuffer/2`](`bindBuffer/2`) binds a buffer object to the specified
buffer binding point. Calling [`gl:bindBuffer/2`](`bindBuffer/2`) with `Target`
set to one of the accepted symbolic constants and `Buffer` set to the name of a
buffer object binds that buffer object name to the target. If no buffer object
with name `Buffer` exists, one is created with that name. When a buffer object
is bound to a target, the previous binding for that target is automatically
broken.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml)
""".
-spec bindBuffer(Target::enum(), Buffer::i()) -> 'ok'.
bindBuffer(Target,Buffer) when is_integer(Target),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Buffer,5397),
  ok.

-doc """
[`gl:deleteBuffers/1`](`deleteBuffers/1`) deletes `N` buffer objects named by
the elements of the array `Buffers`. After a buffer object is deleted, it has no
contents, and its name is free for reuse (for example by
[`gl:genBuffers/1`](`genBuffers/1`)). If a buffer object that is currently bound
is deleted, the binding reverts to 0 (the absence of any buffer object).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteBuffers.xhtml)
""".
-spec deleteBuffers(Buffers::[i()]) -> 'ok'.
deleteBuffers(Buffers) when is_list(Buffers) ->
  IF = get_interface(),
  N = length(Buffers),
  IF:queue_cmd(N,Buffers,5398),
  ok.

-doc """
[`gl:genBuffers/1`](`genBuffers/1`) returns `N` buffer object names in
`Buffers`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genBuffers/1`](`genBuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenBuffers.xhtml)
""".
-spec genBuffers(N::i()) -> [i()].
genBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5399),
  rec(5399).

-doc """
[`gl:isBuffer/1`](`isBuffer/1`) returns `?GL_TRUE` if `Buffer` is currently the
name of a buffer object. If `Buffer` is zero, or is a non-zero value that is not
currently the name of a buffer object, or if an error occurs,
[`gl:isBuffer/1`](`isBuffer/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsBuffer.xhtml)
""".
-spec isBuffer(Buffer::i()) -> 0|1.
isBuffer(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5400),
  rec(5400).

-doc """
[`gl:bufferData/4`](`bufferData/4`) and `glNamedBufferData` create a new data
store for a buffer object. In case of [`gl:bufferData/4`](`bufferData/4`), the
buffer object currently bound to `Target` is used. For `glNamedBufferData`, a
buffer object associated with ID specified by the caller in `Buffer` will be
used instead.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml)
""".
-spec bufferData(Target::enum(), Size::i(), Data::offset()|mem(), Usage::enum()) -> 'ok'.
bufferData(Target,Size,Data,Usage) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Usage) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Usage,5401),
  ok.

-doc """
[`gl:bufferSubData/4`](`bufferSubData/4`) and `glNamedBufferSubData` redefine
some or all of the data store for the specified buffer object. Data starting at
byte offset `Offset` and extending for `Size` bytes is copied to the data store
from the memory pointed to by `Data`. `Offset` and `Size` must define a range
lying entirely within the buffer object's data store.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml)
""".
-spec bufferSubData(Target::enum(), Offset::i(), Size::i(), Data::offset()|mem()) -> 'ok'.
bufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5403),
  ok.

-doc """
[`gl:getBufferSubData/4`](`getBufferSubData/4`) and `glGetNamedBufferSubData`
return some or all of the data contents of the data store of the specified
buffer object. Data starting at byte offset `Offset` and extending for `Size`
bytes is copied from the buffer object's data store to the memory pointed to by
`Data`. An error is thrown if the buffer object is currently mapped, or if
`Offset` and `Size` together define a range beyond the bounds of the buffer
object's data store.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetBufferSubData.xhtml)
""".
-spec getBufferSubData(Target::enum(), Offset::i(), Size::i(), Data::mem()) -> 'ok'.
getBufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5405),
  rec(5405).

-doc """
[`gl:getBufferParameteriv/2`](`getBufferParameteriv/2`) returns in `Data` a
selected parameter of the buffer object specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetBufferParameteriv.xml)
""".
-spec getBufferParameteriv(Target::enum(), Pname::enum()) -> i().
getBufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5406),
  rec(5406).

-doc(#{equiv => blendEquationSeparatei/3}).
-spec blendEquationSeparate(ModeRGB::enum(), ModeAlpha::enum()) -> 'ok'.
blendEquationSeparate(ModeRGB,ModeAlpha) when is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(ModeRGB,ModeAlpha,5407),
  ok.

-doc """
[`gl:drawBuffers/1`](`drawBuffers/1`) and `glNamedFramebufferDrawBuffers` define
an array of buffers into which outputs from the fragment shader data will be
written. If a fragment shader writes a value to one or more user defined output
variables, then the value of each variable will be written into the buffer
specified at a location within `Bufs` corresponding to the location assigned to
that user defined output. The draw buffer used for user defined outputs assigned
to locations greater than or equal to `N` is implicitly set to `?GL_NONE` and
any data written to such an output is discarded.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffers.xhtml)
""".
-spec drawBuffers(Bufs::[enum()]) -> 'ok'.
drawBuffers(Bufs) when is_list(Bufs) ->
  IF = get_interface(),
  N = length(Bufs),
  IF:queue_cmd(N,Bufs,5408),
  ok.

-doc """
Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOpSeparate.xhtml)
""".
-spec stencilOpSeparate(Face::enum(), Sfail::enum(), Dpfail::enum(), Dppass::enum()) -> 'ok'.
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) when is_integer(Face),is_integer(Sfail),is_integer(Dpfail),is_integer(Dppass) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Sfail,Dpfail,Dppass,5409),
  ok.

-doc """
Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFuncSeparate.xhtml)
""".
-spec stencilFuncSeparate(Face::enum(), Func::enum(), Ref::i(), Mask::i()) -> 'ok'.
stencilFuncSeparate(Face,Func,Ref,Mask) when is_integer(Face),is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Func,Ref,Mask,5410),
  ok.

-doc """
[`gl:stencilMaskSeparate/2`](`stencilMaskSeparate/2`) controls the writing of
individual bits in the stencil planes. The least significant n bits of `Mask`,
where n is the number of bits in the stencil buffer, specify a mask. Where a 1
appears in the mask, it's possible to write to the corresponding bit in the
stencil buffer. Where a 0 appears, the corresponding bit is write-protected.
Initially, all bits are enabled for writing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMaskSeparate.xhtml)
""".
-spec stencilMaskSeparate(Face::enum(), Mask::i()) -> 'ok'.
stencilMaskSeparate(Face,Mask) when is_integer(Face),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mask,5411),
  ok.

-doc """
In order to create a complete shader program, there must be a way to specify the
list of things that will be linked together. Program objects provide this
mechanism. Shaders that are to be linked together in a program object must first
be attached to that program object. [`gl:attachShader/2`](`attachShader/2`)
attaches the shader object specified by `Shader` to the program object specified
by `Program`. This indicates that `Shader` will be included in link operations
that will be performed on `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glAttachShader.xhtml)
""".
-spec attachShader(Program::i(), Shader::i()) -> 'ok'.
attachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5412),
  ok.

-doc """
[`gl:bindAttribLocation/3`](`bindAttribLocation/3`) is used to associate a
user-defined attribute variable in the program object specified by `Program`
with a generic vertex attribute index. The name of the user-defined attribute
variable is passed as a null terminated string in `Name`. The generic vertex
attribute index to be bound to this variable is specified by `Index`. When
`Program` is made part of current state, values provided via the generic vertex
attribute `Index` will modify the value of the user-defined attribute variable
specified by `Name`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindAttribLocation.xhtml)
""".
-spec bindAttribLocation(Program::i(), Index::i(), Name::string()) -> 'ok'.
bindAttribLocation(Program,Index,Name) when is_integer(Program),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Index,NameBin,5413),
  ok.

-doc """
[`gl:compileShader/1`](`compileShader/1`) compiles the source code strings that
have been stored in the shader object specified by `Shader`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompileShader.xhtml)
""".
-spec compileShader(Shader::i()) -> 'ok'.
compileShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5414),
  ok.

-doc """
[`gl:createProgram/0`](`createProgram/0`) creates an empty program object and
returns a non-zero value by which it can be referenced. A program object is an
object to which shader objects can be attached. This provides a mechanism to
specify the shader objects that will be linked to create a program. It also
provides a means for checking the compatibility of the shaders that will be used
to create a program (for instance, checking the compatibility between a vertex
shader and a fragment shader). When no longer needed as part of a program
object, shader objects can be detached.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateProgram.xhtml)
""".
-spec createProgram() -> i().
createProgram()  ->
  IF = get_interface(),
  IF:queue_cmd(5415),
  rec(5415).

-doc """
[`gl:createShader/1`](`createShader/1`) creates an empty shader object and
returns a non-zero value by which it can be referenced. A shader object is used
to maintain the source code strings that define a shader. `ShaderType` indicates
the type of shader to be created. Five types of shader are supported. A shader
of type `?GL_COMPUTE_SHADER` is a shader that is intended to run on the
programmable compute processor. A shader of type `?GL_VERTEX_SHADER` is a shader
that is intended to run on the programmable vertex processor. A shader of type
`?GL_TESS_CONTROL_SHADER` is a shader that is intended to run on the
programmable tessellation processor in the control stage. A shader of type
`?GL_TESS_EVALUATION_SHADER` is a shader that is intended to run on the
programmable tessellation processor in the evaluation stage. A shader of type
`?GL_GEOMETRY_SHADER` is a shader that is intended to run on the programmable
geometry processor. A shader of type `?GL_FRAGMENT_SHADER` is a shader that is
intended to run on the programmable fragment processor.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateShader.xhtml)
""".
-spec createShader(Type::enum()) -> i().
createShader(Type) when is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(Type,5416),
  rec(5416).

-doc """
[`gl:deleteProgram/1`](`deleteProgram/1`) frees the memory and invalidates the
name associated with the program object specified by `Program.` This command
effectively undoes the effects of a call to
[`gl:createProgram/0`](`createProgram/0`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgram.xhtml)
""".
-spec deleteProgram(Program::i()) -> 'ok'.
deleteProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5417),
  ok.

-doc """
[`gl:deleteShader/1`](`deleteShader/1`) frees the memory and invalidates the
name associated with the shader object specified by `Shader`. This command
effectively undoes the effects of a call to
[`gl:createShader/1`](`createShader/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteShader.xhtml)
""".
-spec deleteShader(Shader::i()) -> 'ok'.
deleteShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5418),
  ok.

-doc """
[`gl:detachShader/2`](`detachShader/2`) detaches the shader object specified by
`Shader` from the program object specified by `Program`. This command can be
used to undo the effect of the command [`gl:attachShader/2`](`attachShader/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDetachShader.xhtml)
""".
-spec detachShader(Program::i(), Shader::i()) -> 'ok'.
detachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5419),
  ok.

-doc(#{equiv => enableVertexAttribArray/1}).
-spec disableVertexAttribArray(Index::i()) -> 'ok'.
disableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5420),
  ok.

-doc """
[`gl:enableVertexAttribArray/1`](`enableVertexAttribArray/1`) and
[`gl:enableVertexArrayAttrib/2`](`disableVertexAttribArray/1`) enable the
generic vertex attribute array specified by `Index`.
[`gl:enableVertexAttribArray/1`](`enableVertexAttribArray/1`) uses currently
bound vertex array object for the operation, whereas
[`gl:enableVertexArrayAttrib/2`](`disableVertexAttribArray/1`) updates state of
the vertex array object with ID `Vaobj`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml)
""".
-spec enableVertexAttribArray(Index::i()) -> 'ok'.
enableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5421),
  ok.

-doc """
[`gl:getActiveAttrib/3`](`getActiveAttrib/3`) returns information about an
active attribute variable in the program object specified by `Program`. The
number of active attributes can be obtained by calling
[`gl:getProgram()`](`getProgramiv/2`) with the value `?GL_ACTIVE_ATTRIBUTES`. A
value of 0 for `Index` selects the first active attribute variable. Permissible
values for `Index` range from zero to the number of active attribute variables
minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveAttrib.xhtml)
""".
-spec getActiveAttrib(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveAttrib(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5422),
  rec(5422).

-doc """
[`gl:getActiveUniform/3`](`getActiveUniform/3`) returns information about an
active uniform variable in the program object specified by `Program`. The number
of active uniform variables can be obtained by calling
[`gl:getProgram()`](`getProgramiv/2`) with the value `?GL_ACTIVE_UNIFORMS`. A
value of 0 for `Index` selects the first active uniform variable. Permissible
values for `Index` range from zero to the number of active uniform variables
minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniform.xhtml)
""".
-spec getActiveUniform(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveUniform(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5423),
  rec(5423).

-doc """
[`gl:getAttachedShaders/2`](`getAttachedShaders/2`) returns the names of the
shader objects attached to `Program`. The names of shader objects that are
attached to `Program` will be returned in `Shaders.` The actual number of shader
names written into `Shaders` is returned in `Count.` If no shader objects are
attached to `Program`, `Count` is set to 0. The maximum number of shader names
that may be returned in `Shaders` is specified by `MaxCount`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttachedShaders.xhtml)
""".
-spec getAttachedShaders(Program::i(), MaxCount::i()) -> [i()].
getAttachedShaders(Program,MaxCount) when is_integer(Program),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(Program,MaxCount,5424),
  rec(5424).

-doc """
[`gl:getAttribLocation/2`](`getAttribLocation/2`) queries the previously linked
program object specified by `Program` for the attribute variable specified by
`Name` and returns the index of the generic vertex attribute that is bound to
that attribute variable. If `Name` is a matrix attribute variable, the index of
the first column of the matrix is returned. If the named attribute variable is
not an active attribute in the specified program object or if `Name` starts with
the reserved prefix "gl\_", a value of -1 is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttribLocation.xhtml)
""".
-spec getAttribLocation(Program::i(), Name::string()) -> i().
getAttribLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5425),
  rec(5425).

-doc """
[`gl:getProgram()`](`getProgramiv/2`) returns in `Params` the value of a
parameter for a specific program object. The following parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml)
""".
-spec getProgramiv(Program::i(), Pname::enum()) -> i().
getProgramiv(Program,Pname) when is_integer(Program),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,5426),
  rec(5426).

-doc """
[`gl:getProgramInfoLog/2`](`getProgramInfoLog/2`) returns the information log
for the specified program object. The information log for a program object is
modified when the program object is linked or validated. The string that is
returned will be null terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramInfoLog.xhtml)
""".
-spec getProgramInfoLog(Program::i(), BufSize::i()) -> string().
getProgramInfoLog(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5427),
  rec(5427).

-doc """
[`gl:getShader()`](`getShaderiv/2`) returns in `Params` the value of a parameter
for a specific shader object. The following parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShader.xhtml)
""".
-spec getShaderiv(Shader::i(), Pname::enum()) -> i().
getShaderiv(Shader,Pname) when is_integer(Shader),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,Pname,5428),
  rec(5428).

-doc """
[`gl:getShaderInfoLog/2`](`getShaderInfoLog/2`) returns the information log for
the specified shader object. The information log for a shader object is modified
when the shader is compiled. The string that is returned will be null
terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderInfoLog.xhtml)
""".
-spec getShaderInfoLog(Shader::i(), BufSize::i()) -> string().
getShaderInfoLog(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5429),
  rec(5429).

-doc """
[`gl:getShaderSource/2`](`getShaderSource/2`) returns the concatenation of the
source code strings from the shader object specified by `Shader`. The source
code strings for a shader object are the result of a previous call to
[`gl:shaderSource/2`](`shaderSource/2`). The string returned by the function
will be null terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderSource.xhtml)
""".
-spec getShaderSource(Shader::i(), BufSize::i()) -> string().
getShaderSource(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5430),
  rec(5430).

-doc """
`glGetUniformLocation `returns an integer that represents the location of a
specific uniform variable within a program object. `Name` must be a null
terminated string that contains no white space. `Name` must be an active uniform
variable name in `Program` that is not a structure, an array of structures, or a
subcomponent of a vector or a matrix. This function returns -1 if `Name` does
not correspond to an active uniform variable in `Program`, if `Name` starts with
the reserved prefix "gl\_", or if `Name` is associated with an atomic counter or
a named uniform block.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformLocation.xhtml)
""".
-spec getUniformLocation(Program::i(), Name::string()) -> i().
getUniformLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5431),
  rec(5431).

-doc(#{equiv => getUniformuiv/2}).
-spec getUniformfv(Program::i(), Location::i()) -> matrix().
getUniformfv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5432),
  rec(5432).

-doc(#{equiv => getUniformuiv/2}).
-spec getUniformiv(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5433),
  rec(5433).

-doc(#{equiv => getVertexAttribiv/2}).
-spec getVertexAttribdv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5434),
  rec(5434).

-doc(#{equiv => getVertexAttribiv/2}).
-spec getVertexAttribfv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribfv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5435),
  rec(5435).

-doc """
[`gl:getVertexAttrib()`](`getVertexAttribdv/2`) returns in `Params` the value of
a generic vertex attribute parameter. The generic vertex attribute to be queried
is specified by `Index`, and the parameter to be queried is specified by
`Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetVertexAttrib.xhtml)
""".
-spec getVertexAttribiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5436),
  rec(5436).

-doc """
[`gl:isProgram/1`](`isProgram/1`) returns `?GL_TRUE` if `Program` is the name of
a program object previously created with
[`gl:createProgram/0`](`createProgram/0`) and not yet deleted with
[`gl:deleteProgram/1`](`deleteProgram/1`). If `Program` is zero or a non-zero
value that is not the name of a program object, or if an error occurs,
[`gl:isProgram/1`](`isProgram/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgram.xhtml)
""".
-spec isProgram(Program::i()) -> 0|1.
isProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5437),
  rec(5437).

-doc """
[`gl:isShader/1`](`isShader/1`) returns `?GL_TRUE` if `Shader` is the name of a
shader object previously created with [`gl:createShader/1`](`createShader/1`)
and not yet deleted with [`gl:deleteShader/1`](`deleteShader/1`). If `Shader` is
zero or a non-zero value that is not the name of a shader object, or if an error
occurs, `glIsShader `returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsShader.xhtml)
""".
-spec isShader(Shader::i()) -> 0|1.
isShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5438),
  rec(5438).

-doc """
[`gl:linkProgram/1`](`linkProgram/1`) links the program object specified by
`Program`. If any shader objects of type `?GL_VERTEX_SHADER` are attached to
`Program`, they will be used to create an executable that will run on the
programmable vertex processor. If any shader objects of type
`?GL_GEOMETRY_SHADER` are attached to `Program`, they will be used to create an
executable that will run on the programmable geometry processor. If any shader
objects of type `?GL_FRAGMENT_SHADER` are attached to `Program`, they will be
used to create an executable that will run on the programmable fragment
processor.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLinkProgram.xhtml)
""".
-spec linkProgram(Program::i()) -> 'ok'.
linkProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5439),
  ok.

-doc """
[`gl:shaderSource/2`](`shaderSource/2`) sets the source code in `Shader` to the
source code in the array of strings specified by `String`. Any source code
previously stored in the shader object is completely replaced. The number of
strings in the array is specified by `Count`. If `Length` is `?NULL`, each
string is assumed to be null terminated. If `Length` is a value other than
`?NULL`, it points to an array containing a string length for each of the
corresponding elements of `String`. Each element in the `Length` array may
contain the length of the corresponding string (the null character is not
counted as part of the string length) or a value less than 0 to indicate that
the string is null terminated. The source code strings are not scanned or parsed
at this time; they are simply copied into the specified shader object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderSource.xhtml)
""".
-spec shaderSource(Shader::i(), String::[unicode:chardata()]) -> 'ok'.
shaderSource(Shader,String) when is_integer(Shader),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(Shader,Count,StringTemp,5440),
  ok.

-doc """
[`gl:useProgram/1`](`useProgram/1`) installs the program object specified by
`Program` as part of current rendering state. One or more executables are
created in a program object by successfully attaching shader objects to it with
[`gl:attachShader/2`](`attachShader/2`), successfully compiling the shader
objects with [`gl:compileShader/1`](`compileShader/1`), and successfully linking
the program object with [`gl:linkProgram/1`](`linkProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgram.xhtml)
""".
-spec useProgram(Program::i()) -> 'ok'.
useProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5441),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1f(Location::i(), V0::f()) -> 'ok'.
uniform1f(Location,V0) when is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5442),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2f(Location::i(), V0::f(), V1::f()) -> 'ok'.
uniform2f(Location,V0,V1) when is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5443),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3f(Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
uniform3f(Location,V0,V1,V2) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5444),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4f(Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
uniform4f(Location,V0,V1,V2,V3) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5445),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1i(Location::i(), V0::i()) -> 'ok'.
uniform1i(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5446),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2i(Location::i(), V0::i(), V1::i()) -> 'ok'.
uniform2i(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5447),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3i(Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
uniform3i(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5448),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4i(Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
uniform4i(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5449),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1fv(Location::i(), Value::[f()]) -> 'ok'.
uniform1fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5450),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2fv(Location::i(), Value::[{f(),f()}]) -> 'ok'.
uniform2fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5451),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3fv(Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
uniform3fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5452),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4fv(Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniform4fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5453),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1iv(Location::i(), Value::[i()]) -> 'ok'.
uniform1iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5454),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2iv(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5455),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3iv(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5456),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4iv(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5457),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5458),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5459),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5460),
  ok.

-doc """
[`gl:validateProgram/1`](`validateProgram/1`) checks to see whether the
executables contained in `Program` can execute given the current OpenGL state.
The information generated by the validation process will be stored in
`Program`'s information log. The validation information may consist of an empty
string, or it may be a string containing information about how the current
program object interacts with the rest of current OpenGL state. This provides a
way for OpenGL implementers to convey more information about why the current
program is inefficient, suboptimal, failing to execute, and so on.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgram.xhtml)
""".
-spec validateProgram(Program::i()) -> 'ok'.
validateProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5461),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1d(Index::i(), X::f()) -> 'ok'.
vertexAttrib1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5462),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1dv(Index::i(), {X::f()}) -> 'ok'.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1f(Index::i(), X::f()) -> 'ok'.
vertexAttrib1f(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5463),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1fv(Index::i(), {X::f()}) -> 'ok'.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1s(Index::i(), X::i()) -> 'ok'.
vertexAttrib1s(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5464),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib1sv(Index::i(), {X::i()}) -> 'ok'.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2d(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttrib2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5465),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2dv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2f(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttrib2f(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5466),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2fv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2s(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttrib2s(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5467),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib2sv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3d(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttrib3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5468),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3dv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3f(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttrib3f(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5469),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3fv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3s(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttrib3s(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5470),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib3sv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nbv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nbv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5471),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Niv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Niv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5472),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nsv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nsv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5473),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nub(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttrib4Nub(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5474),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nubv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nuiv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nuiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5475),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4Nusv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nusv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5476),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4bv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5477),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4d(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttrib4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5478),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4dv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4f(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttrib4f(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5479),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4fv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4iv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4iv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5480),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4s(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttrib4s(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5481),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4sv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4ubv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5482),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4uiv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4uiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5483),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttrib4usv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5484),
  ok.

-doc """
[`gl:vertexAttribPointer/6`](`vertexAttribPointer/6`),
[`gl:vertexAttribIPointer/5`](`vertexAttribIPointer/5`) and
[`gl:vertexAttribLPointer/5`](`vertexAttribIPointer/5`) specify the location and
data format of the array of generic vertex attributes at index `Index` to use
when rendering. `Size` specifies the number of components per attribute and must
be 1, 2, 3, 4, or `?GL_BGRA`. `Type` specifies the data type of each component,
and `Stride` specifies the byte stride from one attribute to the next, allowing
vertices and attributes to be packed into a single array or stored in separate
arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribPointer.xhtml)
""".
-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok'
    when Index::i(), Size::i(), Type::enum(), Normalized::0|1, Stride::i(), Pointer::offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Normalized,Stride,Pointer,5485),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2x3fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5487),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3x2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5488),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2x4fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5489),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix4x2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix4x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5490),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix3x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5491),
  ok.

-doc """
[`gl:uniform()`](`uniform1f/2`) modifies the value of a uniform variable or a
uniform variable array. The location of the uniform variable to be modified is
specified by `Location`, which should be a value returned by
[`gl:getUniformLocation/2`](`getUniformLocation/2`).
[`gl:uniform()`](`uniform1f/2`) operates on the program object that was made
part of current state by calling [`gl:useProgram/1`](`useProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml)
""".
-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5492),
  ok.

-doc """
[`gl:colorMask/4`](`colorMask/4`) and [`gl:colorMaski/5`](`colorMask/4`) specify
whether the individual color components in the frame buffer can or cannot be
written. [`gl:colorMaski/5`](`colorMask/4`) sets the mask for a specific draw
buffer, whereas [`gl:colorMask/4`](`colorMask/4`) sets the mask for all draw
buffers. If `Red` is `?GL_FALSE`, for example, no change is made to the red
component of any pixel in any of the color buffers, regardless of the drawing
operation attempted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glColorMask.xhtml)
""".
-spec colorMaski(Index::i(), R::0|1, G::0|1, B::0|1, A::0|1) -> 'ok'.
colorMaski(Index,R,G,B,A) when is_integer(Index),(0 =:= R) orelse (1 =:= R),(0 =:= G) orelse (1 =:= G),(0 =:= B) orelse (1 =:= B),(0 =:= A) orelse (1 =:= A) ->
  IF = get_interface(),
  IF:queue_cmd(Index,R,G,B,A,5493),
  ok.

-doc(#{equiv => getIntegerv/1}).
-spec getBooleani_v(Target::enum(), Index::i()) -> [0|1].
getBooleani_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5494),
  rec(5494).

-doc(#{equiv => getIntegerv/1}).
-spec getIntegeri_v(Target::enum(), Index::i()) -> [i()].
getIntegeri_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5495),
  rec(5495).

-doc """
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) enable and disable
various capabilities. Use [`gl:isEnabled/1`](`isEnabled/1`) or
[`gl:get()`](`getBooleanv/1`) to determine the current setting of any
capability. The initial value for each capability with the exception of
`?GL_DITHER` and `?GL_MULTISAMPLE` is `?GL_FALSE`. The initial value for
`?GL_DITHER` and `?GL_MULTISAMPLE` is `?GL_TRUE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnable.xhtml)
""".
-spec enablei(Target::enum(), Index::i()) -> 'ok'.
enablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5496),
  ok.

-doc(#{equiv => enablei/2}).
-spec disablei(Target::enum(), Index::i()) -> 'ok'.
disablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5497),
  ok.

-doc """
[`gl:isEnabled/1`](`isEnabled/1`) returns `?GL_TRUE` if `Cap` is an enabled
capability and returns `?GL_FALSE` otherwise. Boolean states that are indexed
may be tested with [`gl:isEnabledi/2`](`isEnabled/1`). For
[`gl:isEnabledi/2`](`isEnabled/1`), `Index` specifies the index of the
capability to test. `Index` must be between zero and the count of indexed
capabilities for `Cap`. Initially all capabilities except `?GL_DITHER` are
disabled; `?GL_DITHER` is initially enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsEnabled.xhtml)
""".
-spec isEnabledi(Target::enum(), Index::i()) -> 0|1.
isEnabledi(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5498),
  rec(5498).

-doc(#{equiv => endTransformFeedback/0}).
-spec beginTransformFeedback(PrimitiveMode::enum()) -> 'ok'.
beginTransformFeedback(PrimitiveMode) when is_integer(PrimitiveMode) ->
  IF = get_interface(),
  IF:queue_cmd(PrimitiveMode,5499),
  ok.

-doc """
Transform feedback mode captures the values of varying variables written by the
vertex shader (or, if active, the geometry shader). Transform feedback is said
to be active after a call to
[`gl:beginTransformFeedback/1`](`beginTransformFeedback/1`) until a subsequent
call to [`gl:endTransformFeedback/0`](`beginTransformFeedback/1`). Transform
feedback commands must be paired.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginTransformFeedback.xhtml)
""".
-spec endTransformFeedback() -> 'ok'.
endTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5500),
  ok.

-doc """
[`gl:bindBufferRange/5`](`bindBufferRange/5`) binds a range the buffer object
`Buffer` represented by `Offset` and `Size` to the binding point at index
`Index` of the array of targets specified by `Target`. Each `Target` represents
an indexed array of buffer binding points, as well as a single general binding
point that can be used by other buffer manipulation functions such as
[`gl:bindBuffer/2`](`bindBuffer/2`) or `glMapBuffer`. In addition to binding a
range of `Buffer` to the indexed buffer binding target,
[`gl:bindBufferRange/5`](`bindBufferRange/5`) also binds the range to the
generic buffer binding point specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferRange.xhtml)
""".
-spec bindBufferRange(Target::enum(), Index::i(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
bindBufferRange(Target,Index,Buffer,Offset,Size) when is_integer(Target),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,Offset,Size,5501),
  ok.

-doc """
[`gl:bindBufferBase/3`](`bindBufferBase/3`) binds the buffer object `Buffer` to
the binding point at index `Index` of the array of targets specified by
`Target`. Each `Target` represents an indexed array of buffer binding points, as
well as a single general binding point that can be used by other buffer
manipulation functions such as [`gl:bindBuffer/2`](`bindBuffer/2`) or
`glMapBuffer`. In addition to binding `Buffer` to the indexed buffer binding
target, [`gl:bindBufferBase/3`](`bindBufferBase/3`) also binds `Buffer` to the
generic buffer binding point specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferBase.xhtml)
""".
-spec bindBufferBase(Target::enum(), Index::i(), Buffer::i()) -> 'ok'.
bindBufferBase(Target,Index,Buffer) when is_integer(Target),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,5502),
  ok.

-doc """
The names of the vertex or geometry shader outputs to be recorded in transform
feedback mode are specified using
[`gl:transformFeedbackVaryings/3`](`transformFeedbackVaryings/3`). When a
geometry shader is active, transform feedback records the values of selected
geometry shader output variables from the emitted vertices. Otherwise, the
values of the selected vertex shader outputs are recorded.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackVaryings.xhtml)
""".
-spec transformFeedbackVaryings(Program::i(), Varyings::[unicode:chardata()], BufferMode::enum()) -> 'ok'.
transformFeedbackVaryings(Program,Varyings,BufferMode) when is_integer(Program),is_list(Varyings),is_integer(BufferMode) ->
  IF = get_interface(),
  VaryingsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Varyings ],
  Count = length(Varyings),
  IF:queue_cmd(Program,Count,VaryingsTemp,BufferMode,5503),
  ok.

-doc """
Information about the set of varying variables in a linked program that will be
captured during transform feedback may be retrieved by calling
[`gl:getTransformFeedbackVarying/3`](`getTransformFeedbackVarying/3`).
[`gl:getTransformFeedbackVarying/3`](`getTransformFeedbackVarying/3`) provides
information about the varying variable selected by `Index`. An `Index` of 0
selects the first varying variable specified in the `Varyings` array passed to
[`gl:transformFeedbackVaryings/3`](`transformFeedbackVaryings/3`), and an
`Index` of the value of `?GL_TRANSFORM_FEEDBACK_VARYINGS` minus one selects the
last such variable.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTransformFeedbackVarying.xhtml)
""".
-spec getTransformFeedbackVarying(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getTransformFeedbackVarying(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5504),
  rec(5504).

-doc """
[`gl:clampColor/2`](`clampColor/2`) controls color clamping that is performed
during [`gl:readPixels/7`](`readPixels/7`). `Target` must be
`?GL_CLAMP_READ_COLOR`. If `Clamp` is `?GL_TRUE`, read color clamping is
enabled; if `Clamp` is `?GL_FALSE`, read color clamping is disabled. If `Clamp`
is `?GL_FIXED_ONLY`, read color clamping is enabled only if the selected read
buffer has fixed point components and disabled otherwise.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClampColor.xhtml)
""".
-spec clampColor(Target::enum(), Clamp::enum()) -> 'ok'.
clampColor(Target,Clamp) when is_integer(Target),is_integer(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Clamp,5505),
  ok.

-doc(#{equiv => endConditionalRender/0}).
-spec beginConditionalRender(Id::i(), Mode::enum()) -> 'ok'.
beginConditionalRender(Id,Mode) when is_integer(Id),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Mode,5506),
  ok.

-doc """
Conditional rendering is started using
[`gl:beginConditionalRender/2`](`beginConditionalRender/2`) and ended using
[`gl:endConditionalRender/0`](`beginConditionalRender/2`). During conditional
rendering, all vertex array commands, as well as [`gl:clear/1`](`clear/1`) and
[`gl:clearBuffer()`](`clearBufferiv/3`) have no effect if the
(`?GL_SAMPLES_PASSED`) result of the query object `Id` is zero, or if the
(`?GL_ANY_SAMPLES_PASSED`) result is `?GL_FALSE`. The results of commands
setting the current vertex state, such as
[`gl:vertexAttrib()`](`vertexAttrib1d/2`) are undefined. If the
(`?GL_SAMPLES_PASSED`) result is non-zero or if the (`?GL_ANY_SAMPLES_PASSED`)
result is `?GL_TRUE`, such commands are not discarded. The `Id` parameter to
[`gl:beginConditionalRender/2`](`beginConditionalRender/2`) must be the name of
a query object previously returned from a call to
[`gl:genQueries/1`](`genQueries/1`). `Mode` specifies how the results of the
query object are to be interpreted. If `Mode` is `?GL_QUERY_WAIT`, the GL waits
for the results of the query to be available and then uses the results to
determine if subsequent rendering commands are discarded. If `Mode` is
`?GL_QUERY_NO_WAIT`, the GL may choose to unconditionally execute the subsequent
rendering commands without waiting for the query to complete.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginConditionalRender.xhtml)
""".
-spec endConditionalRender() -> 'ok'.
endConditionalRender()  ->
  IF = get_interface(),
  IF:queue_cmd(5507),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexAttribIPointer(Index::i(), Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5508),
  ok.

-doc(#{equiv => getVertexAttribiv/2}).
-spec getVertexAttribIiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribIiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5510),
  rec(5510).

-doc(#{equiv => getVertexAttribiv/2}).
-spec getVertexAttribIuiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribIuiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5511),
  rec(5511).

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI1i(Index::i(), X::i()) -> 'ok'.
vertexAttribI1i(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5512),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI2i(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttribI2i(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5513),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI3i(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttribI3i(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5514),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4i(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttribI4i(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5515),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI1ui(Index::i(), X::i()) -> 'ok'.
vertexAttribI1ui(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5516),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI2ui(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttribI2ui(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5517),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI3ui(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttribI3ui(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5518),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4ui(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttribI4ui(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5519),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI1iv(Index::i(), {X::i()}) -> 'ok'.
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI2iv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI3iv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4iv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI1uiv(Index::i(), {X::i()}) -> 'ok'.
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI2uiv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI3uiv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4uiv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4bv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5520),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4sv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4sv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5521),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4ubv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5522),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribI4usv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5523),
  ok.

-doc """
[`gl:getUniform()`](`getUniformfv/2`) and `glGetnUniform` return in `Params` the
value(s) of the specified uniform variable. The type of the uniform variable
specified by `Location` determines the number of values returned. If the uniform
variable is defined in the shader as a boolean, int, or float, a single value
will be returned. If it is defined as a vec2, ivec2, or bvec2, two values will
be returned. If it is defined as a vec3, ivec3, or bvec3, three values will be
returned, and so on. To query values stored in uniform variables declared as
arrays, call [`gl:getUniform()`](`getUniformfv/2`) for each element of the
array. To query values stored in uniform variables declared as structures, call
[`gl:getUniform()`](`getUniformfv/2`) for each field in the structure. The
values for uniform variables declared as a matrix will be returned in column
major order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniform.xhtml)
""".
-spec getUniformuiv(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformuiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5524),
  rec(5524).

-doc """
[`gl:bindFragDataLocation/3`](`bindFragDataLocation/3`) explicitly specifies the
binding of the user-defined varying out variable `Name` to fragment shader color
number `ColorNumber` for program `Program`. If `Name` was bound previously, its
assigned binding is replaced with `ColorNumber`. `Name` must be a
null-terminated string. `ColorNumber` must be less than `?GL_MAX_DRAW_BUFFERS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFragDataLocation.xhtml)
""".
-spec bindFragDataLocation(Program::i(), Color::i(), Name::string()) -> 'ok'.
bindFragDataLocation(Program,Color,Name) when is_integer(Program),is_integer(Color),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Color,NameBin,5525),
  ok.

-doc """
[`gl:getFragDataLocation/2`](`getFragDataLocation/2`) retrieves the assigned
color number binding for the user-defined varying out variable `Name` for
program `Program`. `Program` must have previously been linked. `Name` must be a
null-terminated string. If `Name` is not the name of an active user-defined
varying out fragment shader variable within `Program`, -1 will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataLocation.xhtml)
""".
-spec getFragDataLocation(Program::i(), Name::string()) -> i().
getFragDataLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5526),
  rec(5526).

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1ui(Location::i(), V0::i()) -> 'ok'.
uniform1ui(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5527),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2ui(Location::i(), V0::i(), V1::i()) -> 'ok'.
uniform2ui(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5528),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3ui(Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
uniform3ui(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5529),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4ui(Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
uniform4ui(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5530),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1uiv(Location::i(), Value::[i()]) -> 'ok'.
uniform1uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5531),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2uiv(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5532),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3uiv(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5533),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4uiv(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5534),
  ok.

-doc(#{equiv => texParameteriv/3}).
-spec texParameterIiv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterIiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5535),
  ok.

-doc(#{equiv => texParameteriv/3}).
-spec texParameterIuiv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterIuiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5536),
  ok.

-doc(#{equiv => getTexParameteriv/2}).
-spec getTexParameterIiv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameterIiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5537),
  rec(5537).

-doc(#{equiv => getTexParameteriv/2}).
-spec getTexParameterIuiv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameterIuiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5538),
  rec(5538).

-doc(#{equiv => clearBufferuiv/3}).
-spec clearBufferiv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5539),
  ok.

-doc """
These commands clear a specified buffer of a framebuffer to specified value(s).
For [`gl:clearBuffer*()`](`clearBufferiv/3`), the framebuffer is the currently
bound draw framebuffer object. For `glClearNamedFramebuffer*`, `Framebuffer` is
zero, indicating the default draw framebuffer, or the name of a framebuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearBuffer.xhtml)
""".
-spec clearBufferuiv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferuiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5540),
  ok.

-doc(#{equiv => clearBufferuiv/3}).
-spec clearBufferfv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferfv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5541),
  ok.

-doc(#{equiv => clearBufferuiv/3}).
-spec clearBufferfi(Buffer::enum(), Drawbuffer::i(), Depth::f(), Stencil::i()) -> 'ok'.
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) when is_integer(Buffer),is_integer(Drawbuffer),is_float(Depth),is_integer(Stencil) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Depth,Stencil,5542),
  ok.

-doc """
[`gl:getString/1`](`getString/1`) returns a pointer to a static string
describing some aspect of the current GL connection. `Name` can be one of the
following:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetString.xhtml)
""".
-spec getStringi(Name::enum(), Index::i()) -> string().
getStringi(Name,Index) when is_integer(Name),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Name,Index,5543),
  rec(5543).

-doc """
[`gl:isRenderbuffer/1`](`isRenderbuffer/1`) returns `?GL_TRUE` if `Renderbuffer`
is currently the name of a renderbuffer object. If `Renderbuffer` is zero, or if
`Renderbuffer` is not the name of a renderbuffer object, or if an error occurs,
[`gl:isRenderbuffer/1`](`isRenderbuffer/1`) returns `?GL_FALSE`. If
`Renderbuffer` is a name returned by
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`), by that has not yet been bound
through a call to [`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) or
[`gl:framebufferRenderbuffer/4`](`framebufferRenderbuffer/4`), then the name is
not a renderbuffer object and [`gl:isRenderbuffer/1`](`isRenderbuffer/1`)
returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsRenderbuffer.xhtml)
""".
-spec isRenderbuffer(Renderbuffer::i()) -> 0|1.
isRenderbuffer(Renderbuffer) when is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Renderbuffer,5544),
  rec(5544).

-doc """
[`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) binds the renderbuffer object
with name `Renderbuffer` to the renderbuffer target specified by `Target`.
`Target` must be `?GL_RENDERBUFFER`. `Renderbuffer` is the name of a
renderbuffer object previously returned from a call to
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`), or zero to break the existing
binding of a renderbuffer object to `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindRenderbuffer.xhtml)
""".
-spec bindRenderbuffer(Target::enum(), Renderbuffer::i()) -> 'ok'.
bindRenderbuffer(Target,Renderbuffer) when is_integer(Target),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Renderbuffer,5545),
  ok.

-doc """
[`gl:deleteRenderbuffers/1`](`deleteRenderbuffers/1`) deletes the `N`
renderbuffer objects whose names are stored in the array addressed by
`Renderbuffers`. The name zero is reserved by the GL and is silently ignored,
should it occur in `Renderbuffers`, as are other unused names. Once a
renderbuffer object is deleted, its name is again unused and it has no contents.
If a renderbuffer that is currently bound to the target `?GL_RENDERBUFFER` is
deleted, it is as though [`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) had
been executed with a `Target` of `?GL_RENDERBUFFER` and a `Name` of zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteRenderbuffers.xhtml)
""".
-spec deleteRenderbuffers(Renderbuffers::[i()]) -> 'ok'.
deleteRenderbuffers(Renderbuffers) when is_list(Renderbuffers) ->
  IF = get_interface(),
  N = length(Renderbuffers),
  IF:queue_cmd(N,Renderbuffers,5546),
  ok.

-doc """
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`) returns `N` renderbuffer object
names in `Renderbuffers`. There is no guarantee that the names form a contiguous
set of integers; however, it is guaranteed that none of the returned names was
in use immediately before the call to
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenRenderbuffers.xhtml)
""".
-spec genRenderbuffers(N::i()) -> [i()].
genRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5547),
  rec(5547).

-doc """
[`gl:renderbufferStorage/4`](`renderbufferStorage/4`) is equivalent to calling
[`gl:renderbufferStorageMultisample/5`](`renderbufferStorageMultisample/5`) with
the `Samples` set to zero, and `glNamedRenderbufferStorage` is equivalent to
calling `glNamedRenderbufferStorageMultisample` with the samples set to zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorage.xhtml)
""".
-spec renderbufferStorage(Target::enum(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
renderbufferStorage(Target,Internalformat,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,5548),
  ok.

-doc """
[`gl:getRenderbufferParameteriv/2`](`getRenderbufferParameteriv/2`) and
`glGetNamedRenderbufferParameteriv` query parameters of a specified renderbuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetRenderbufferParameter.xhtml)
""".
-spec getRenderbufferParameteriv(Target::enum(), Pname::enum()) -> i().
getRenderbufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5549),
  rec(5549).

-doc """
[`gl:isFramebuffer/1`](`isFramebuffer/1`) returns `?GL_TRUE` if `Framebuffer` is
currently the name of a framebuffer object. If `Framebuffer` is zero, or if
`?framebuffer` is not the name of a framebuffer object, or if an error occurs,
[`gl:isFramebuffer/1`](`isFramebuffer/1`) returns `?GL_FALSE`. If `Framebuffer`
is a name returned by [`gl:genFramebuffers/1`](`genFramebuffers/1`), by that has
not yet been bound through a call to
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`), then the name is not a
framebuffer object and [`gl:isFramebuffer/1`](`isFramebuffer/1`) returns
`?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsFramebuffer.xhtml)
""".
-spec isFramebuffer(Framebuffer::i()) -> 0|1.
isFramebuffer(Framebuffer) when is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Framebuffer,5550),
  rec(5550).

-doc """
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) binds the framebuffer object with
name `Framebuffer` to the framebuffer target specified by `Target`. `Target`
must be either `?GL_DRAW_FRAMEBUFFER`, `?GL_READ_FRAMEBUFFER` or
`?GL_FRAMEBUFFER`. If a framebuffer object is bound to `?GL_DRAW_FRAMEBUFFER` or
`?GL_READ_FRAMEBUFFER`, it becomes the target for rendering or readback
operations, respectively, until it is deleted or another framebuffer is bound to
the corresponding bind point. Calling
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) with `Target` set to
`?GL_FRAMEBUFFER` binds `Framebuffer` to both the read and draw framebuffer
targets. `Framebuffer` is the name of a framebuffer object previously returned
from a call to [`gl:genFramebuffers/1`](`genFramebuffers/1`), or zero to break
the existing binding of a framebuffer object to `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFramebuffer.xhtml)
""".
-spec bindFramebuffer(Target::enum(), Framebuffer::i()) -> 'ok'.
bindFramebuffer(Target,Framebuffer) when is_integer(Target),is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Framebuffer,5551),
  ok.

-doc """
[`gl:deleteFramebuffers/1`](`deleteFramebuffers/1`) deletes the `N` framebuffer
objects whose names are stored in the array addressed by `Framebuffers`. The
name zero is reserved by the GL and is silently ignored, should it occur in
`Framebuffers`, as are other unused names. Once a framebuffer object is deleted,
its name is again unused and it has no attachments. If a framebuffer that is
currently bound to one or more of the targets `?GL_DRAW_FRAMEBUFFER` or
`?GL_READ_FRAMEBUFFER` is deleted, it is as though
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) had been executed with the
corresponding `Target` and `Framebuffer` zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteFramebuffers.xhtml)
""".
-spec deleteFramebuffers(Framebuffers::[i()]) -> 'ok'.
deleteFramebuffers(Framebuffers) when is_list(Framebuffers) ->
  IF = get_interface(),
  N = length(Framebuffers),
  IF:queue_cmd(N,Framebuffers,5552),
  ok.

-doc """
[`gl:genFramebuffers/1`](`genFramebuffers/1`) returns `N` framebuffer object
names in `Ids`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genFramebuffers/1`](`genFramebuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenFramebuffers.xhtml)
""".
-spec genFramebuffers(N::i()) -> [i()].
genFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5553),
  rec(5553).

-doc """
[`gl:checkFramebufferStatus/1`](`checkFramebufferStatus/1`) and
`glCheckNamedFramebufferStatus` return the completeness status of a framebuffer
object when treated as a read or draw framebuffer, depending on the value of
`Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCheckFramebufferStatus.xhtml)
""".
-spec checkFramebufferStatus(Target::enum()) -> enum().
checkFramebufferStatus(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5554),
  rec(5554).

-doc(#{equiv => framebufferTextureLayer/5}).
-spec framebufferTexture1D(Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5555),
  ok.

-doc(#{equiv => framebufferTextureLayer/5}).
-spec framebufferTexture2D(Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5556),
  ok.

-doc(#{equiv => framebufferTextureLayer/5}).
-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok'
    when Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i(), Zoffset::i().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level),is_integer(Zoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,Zoffset,5557),
  ok.

-doc """
[`gl:framebufferRenderbuffer/4`](`framebufferRenderbuffer/4`) and
`glNamedFramebufferRenderbuffer` attaches a renderbuffer as one of the logical
buffers of the specified framebuffer object. Renderbuffers cannot be attached to
the default draw and read framebuffer, so they are not valid targets of these
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferRenderbuffer.xhtml)
""".
-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok'
    when Target::enum(), Attachment::enum(), Renderbuffertarget::enum(), Renderbuffer::i().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) when is_integer(Target),is_integer(Attachment),is_integer(Renderbuffertarget),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Renderbuffertarget,Renderbuffer,5558),
  ok.

-doc """
[`gl:getFramebufferAttachmentParameteriv/3`](`getFramebufferAttachmentParameteriv/3`)
and `glGetNamedFramebufferAttachmentParameteriv` return parameters of
attachments of a specified framebuffer object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFramebufferAttachmentParameter.xhtml)
""".
-spec getFramebufferAttachmentParameteriv(Target::enum(), Attachment::enum(), Pname::enum()) -> i().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) when is_integer(Target),is_integer(Attachment),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Pname,5559),
  rec(5559).

-doc(#{equiv => generateTextureMipmap/1}).
-spec generateMipmap(Target::enum()) -> 'ok'.
generateMipmap(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5560),
  ok.

-doc """
[`gl:blitFramebuffer/10`](`blitFramebuffer/10`) and `glBlitNamedFramebuffer`
transfer a rectangle of pixel values from one region of a read framebuffer to
another region of a draw framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlitFramebuffer.xhtml)
""".
-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok'
    when SrcX0::i(), SrcY0::i(), SrcX1::i(), SrcY1::i(), DstX0::i(), DstY0::i(), DstX1::i(), DstY1::i(), Mask::i(), Filter::enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) when is_integer(SrcX0),is_integer(SrcY0),is_integer(SrcX1),is_integer(SrcY1),is_integer(DstX0),is_integer(DstY0),is_integer(DstX1),is_integer(DstY1),is_integer(Mask),is_integer(Filter) ->
  IF = get_interface(),
  IF:queue_cmd(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter,5561),
  ok.

-doc """
[`gl:renderbufferStorageMultisample/5`](`renderbufferStorageMultisample/5`) and
`glNamedRenderbufferStorageMultisample` establish the data storage, format,
dimensions and number of samples of a renderbuffer object's image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorageMultisample.xhtml)
""".
-spec renderbufferStorageMultisample(Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,5562),
  ok.

-doc """
These commands attach a selected mipmap level or image of a texture object as
one of the logical buffers of the specified framebuffer object. Textures cannot
be attached to the default draw and read framebuffer, so they are not valid
targets of these commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferTexture.xhtml)
""".
-spec framebufferTextureLayer(Target::enum(), Attachment::enum(), Texture::i(), Level::i(), Layer::i()) -> 'ok'.
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Layer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Layer,5563),
  ok.

-doc(#{equiv => flushMappedNamedBufferRange/3}).
-spec flushMappedBufferRange(Target::enum(), Offset::i(), Length::i()) -> 'ok'.
flushMappedBufferRange(Target,Offset,Length) when is_integer(Target),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Length,5564),
  ok.

-doc """
[`gl:bindVertexArray/1`](`bindVertexArray/1`) binds the vertex array object with
name `Array`. `Array` is the name of a vertex array object previously returned
from a call to [`gl:genVertexArrays/1`](`genVertexArrays/1`), or zero to break
the existing vertex array object binding.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexArray.xhtml)
""".
-spec bindVertexArray(Array::i()) -> 'ok'.
bindVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5565),
  ok.

-doc """
[`gl:deleteVertexArrays/1`](`deleteVertexArrays/1`) deletes `N` vertex array
objects whose names are stored in the array addressed by `Arrays`. Once a vertex
array object is deleted it has no contents and its name is again unused. If a
vertex array object that is currently bound is deleted, the binding for that
object reverts to zero and the default vertex array becomes current. Unused
names in `Arrays` are silently ignored, as is the value zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteVertexArrays.xhtml)
""".
-spec deleteVertexArrays(Arrays::[i()]) -> 'ok'.
deleteVertexArrays(Arrays) when is_list(Arrays) ->
  IF = get_interface(),
  N = length(Arrays),
  IF:queue_cmd(N,Arrays,5566),
  ok.

-doc """
[`gl:genVertexArrays/1`](`genVertexArrays/1`) returns `N` vertex array object
names in `Arrays`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genVertexArrays/1`](`genVertexArrays/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenVertexArrays.xhtml)
""".
-spec genVertexArrays(N::i()) -> [i()].
genVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5567),
  rec(5567).

-doc """
[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_TRUE` if `Array` is
currently the name of a vertex array object. If `Array` is zero, or if `Array`
is not the name of a vertex array object, or if an error occurs,
[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_FALSE`. If `Array` is a
name returned by [`gl:genVertexArrays/1`](`genVertexArrays/1`), by that has not
yet been bound through a call to [`gl:bindVertexArray/1`](`bindVertexArray/1`),
then the name is not a vertex array object and
[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsVertexArray.xhtml)
""".
-spec isVertexArray(Array::i()) -> 0|1.
isVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5568),
  rec(5568).

-doc """
[`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`) behaves identically to
[`gl:drawArrays/3`](`drawArrays/3`) except that `Instancecount` instances of the
range of elements are executed and the value of the internal counter
`InstanceID` advances for each iteration. `InstanceID` is an internal 32-bit
integer counter that may be read by a vertex shader as `?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysInstanced.xhtml)
""".
-spec drawArraysInstanced(Mode::enum(), First::i(), Count::i(), Instancecount::i()) -> 'ok'.
drawArraysInstanced(Mode,First,Count,Instancecount) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,5569),
  ok.

-doc """
[`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`) behaves identically to
[`gl:drawElements/4`](`drawElements/4`) except that `Instancecount` instances of
the set of elements are executed and the value of the internal counter
`InstanceID` advances for each iteration. `InstanceID` is an internal 32-bit
integer counter that may be read by a vertex shader as `?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstanced.xhtml)
""".
-spec drawElementsInstanced(Mode, Count, Type, Indices, Instancecount) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i().
drawElementsInstanced(Mode,Count,Type,Indices,Instancecount) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,5570),
  ok.

-doc(#{equiv => textureBuffer/3}).
-spec texBuffer(Target::enum(), Internalformat::enum(), Buffer::i()) -> 'ok'.
texBuffer(Target,Internalformat,Buffer) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,5572),
  ok.

-doc """
[`gl:primitiveRestartIndex/1`](`primitiveRestartIndex/1`) specifies a vertex
array element that is treated specially when primitive restarting is enabled.
This is known as the primitive restart index.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPrimitiveRestartIndex.xhtml)
""".
-spec primitiveRestartIndex(Index::i()) -> 'ok'.
primitiveRestartIndex(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5573),
  ok.

-doc """
[`gl:copyBufferSubData/5`](`copyBufferSubData/5`) and `glCopyNamedBufferSubData`
copy part of the data store attached to a source buffer object to the data store
attached to a destination buffer object. The number of basic machine units
indicated by `Size` is copied from the source at offset `ReadOffset` to the
destination at `WriteOffset`. `ReadOffset`, `WriteOffset` and `Size` are in
terms of basic machine units.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyBufferSubData.xhtml)
""".
-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok'
    when ReadTarget::enum(), WriteTarget::enum(), ReadOffset::i(), WriteOffset::i(), Size::i().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) when is_integer(ReadTarget),is_integer(WriteTarget),is_integer(ReadOffset),is_integer(WriteOffset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size,5574),
  ok.

-doc """
[`gl:getUniformIndices/2`](`getUniformIndices/2`) retrieves the indices of a
number of uniforms within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformIndices.xhtml)
""".
-spec getUniformIndices(Program::i(), UniformNames::[unicode:chardata()]) -> [i()].
getUniformIndices(Program,UniformNames) when is_integer(Program),is_list(UniformNames) ->
  IF = get_interface(),
  UniformNamesTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- UniformNames ],
  UniformCount = length(UniformNames),
  IF:queue_cmd(Program,UniformCount,UniformNamesTemp,5575),
  rec(5575).

-doc """
[`gl:getActiveUniformsiv/3`](`getActiveUniformsiv/3`) queries the value of the
parameter named `Pname` for each of the uniforms within `Program` whose indices
are specified in the array of `UniformCount` unsigned integers `UniformIndices`.
Upon success, the value of the parameter for each uniform is written into the
corresponding entry in the array whose address is given in `Params`. If an error
is generated, nothing is written into `Params`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformsiv.xhtml)
""".
-spec getActiveUniformsiv(Program::i(), UniformIndices::[i()], Pname::enum()) -> [i()].
getActiveUniformsiv(Program,UniformIndices,Pname) when is_integer(Program),is_list(UniformIndices),is_integer(Pname) ->
  IF = get_interface(),
  UniformCount = length(UniformIndices),
  IF:queue_cmd(Program,UniformCount,UniformIndices,Pname,5576),
  rec(5576).

-doc """
[`gl:getActiveUniformName/3`](`getActiveUniformName/3`) returns the name of the
active uniform at `UniformIndex` within `Program`. If `UniformName` is not NULL,
up to `BufSize` characters (including a nul-terminator) will be written into the
array whose address is specified by `UniformName`. If `Length` is not NULL, the
number of characters that were (or would have been) written into `UniformName`
(not including the nul-terminator) will be placed in the variable whose address
is specified in `Length`. If `Length` is NULL, no length is returned. The length
of the longest uniform name in `Program` is given by the value of
`?GL_ACTIVE_UNIFORM_MAX_LENGTH`, which can be queried with
[`gl:getProgram()`](`getProgramiv/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformName.xhtml)
""".
-spec getActiveUniformName(Program::i(), UniformIndex::i(), BufSize::i()) -> string().
getActiveUniformName(Program,UniformIndex,BufSize) when is_integer(Program),is_integer(UniformIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformIndex,BufSize,5577),
  rec(5577).

-doc """
[`gl:getUniformBlockIndex/2`](`getUniformBlockIndex/2`) retrieves the index of a
uniform block within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformBlockIndex.xhtml)
""".
-spec getUniformBlockIndex(Program::i(), UniformBlockName::string()) -> i().
getUniformBlockIndex(Program,UniformBlockName) when is_integer(Program),is_list(UniformBlockName) ->
  IF = get_interface(),
  UniformBlockNameBin = unicode:characters_to_binary([UniformBlockName|[0]]),
  IF:queue_cmd(Program,UniformBlockNameBin,5578),
  rec(5578).

-doc """
[`gl:getActiveUniformBlockiv/4`](`getActiveUniformBlockiv/4`) retrieves
information about an active uniform block within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlock.xhtml)
""".
-spec getActiveUniformBlockiv(Program::i(), UniformBlockIndex::i(), Pname::enum(), Params::mem()) -> 'ok'.
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(Pname),is_tuple(Params) orelse is_binary(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,Pname,Params,5579),
  rec(5579).

-doc """
[`gl:getActiveUniformBlockName/3`](`getActiveUniformBlockName/3`) retrieves the
name of the active uniform block at `UniformBlockIndex` within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlockName.xhtml)
""".
-spec getActiveUniformBlockName(Program::i(), UniformBlockIndex::i(), BufSize::i()) -> string().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,BufSize,5580),
  rec(5580).

-doc """
Binding points for active uniform blocks are assigned using
[`gl:uniformBlockBinding/3`](`uniformBlockBinding/3`). Each of a program's
active uniform blocks has a corresponding uniform buffer binding point.
`Program` is the name of a program object for which the command
[`gl:linkProgram/1`](`linkProgram/1`) has been issued in the past.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformBlockBinding.xhtml)
""".
-spec uniformBlockBinding(Program::i(), UniformBlockIndex::i(), UniformBlockBinding::i()) -> 'ok'.
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(UniformBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,UniformBlockBinding,5581),
  ok.

-doc """
[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`) behaves identically
to [`gl:drawElements/4`](`drawElements/4`) except that the `i`th element
transferred by the corresponding draw call will be taken from element
`Indices`\[i] + `Basevertex` of each enabled array. If the resulting value is
larger than the maximum value representable by `Type`, it is as if the
calculation were upconverted to 32-bit unsigned integers (with wrapping on
overflow conditions). The operation is undefined if the sum would be negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsBaseVertex.xhtml)
""".
-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Basevertex::i().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Basevertex,5582),
  ok.

-doc """
[`gl:drawRangeElementsBaseVertex/7`](`drawRangeElementsBaseVertex/7`) is a
restricted form of [`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`).
`Mode`, `Count` and `Basevertex` match the corresponding arguments to
[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`), with the additional
constraint that all values in the array `Indices` must lie between `Start` and
`End`, inclusive, prior to adding `Basevertex`. Index values lying outside the
range [`Start`, `End`] are treated in the same way as
[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`). The `i`th element
transferred by the corresponding draw call will be taken from element
`Indices`\[i] + `Basevertex` of each enabled array. If the resulting value is
larger than the maximum value representable by `Type`, it is as if the
calculation were upconverted to 32-bit unsigned integers (with wrapping on
overflow conditions). The operation is undefined if the sum would be negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElementsBaseVertex.xhtml)
""".
-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok'
    when Mode::enum(), Start::i(), End::i(), Count::i(), Type::enum(), Indices::offset()|mem(), Basevertex::i().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,Basevertex,5584),
  ok.

-doc """
[`gl:drawElementsInstancedBaseVertex/6`](`drawElementsInstancedBaseVertex/6`)
behaves identically to [`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`)
except that the `i`th element transferred by the corresponding draw call will be
taken from element `Indices`\[i] + `Basevertex` of each enabled array. If the
resulting value is larger than the maximum value representable by `Type`, it is
as if the calculation were upconverted to 32-bit unsigned integers (with
wrapping on overflow conditions). The operation is undefined if the sum would be
negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertex.xhtml)
""".
-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Instancecount, Basevertex) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Basevertex::i().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Instancecount,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,5586),
  ok.

-doc """
`Flatshading` a vertex shader varying output means to assign all vetices of the
primitive the same value for that output. The vertex from which these values is
derived is known as the `provoking vertex` and
[`gl:provokingVertex/1`](`provokingVertex/1`) specifies which vertex is to be
used as the source of data for flat shaded varyings.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProvokingVertex.xhtml)
""".
-spec provokingVertex(Mode::enum()) -> 'ok'.
provokingVertex(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5588),
  ok.

-doc """
[`gl:fenceSync/2`](`fenceSync/2`) creates a new fence sync object, inserts a
fence command into the GL command stream and associates it with that sync
object, and returns a non-zero name corresponding to the sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFenceSync.xhtml)
""".
-spec fenceSync(Condition::enum(), Flags::i()) -> i().
fenceSync(Condition,Flags) when is_integer(Condition),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Condition,Flags,5589),
  rec(5589).

-doc """
[`gl:isSync/1`](`isSync/1`) returns `?GL_TRUE` if `Sync` is currently the name
of a sync object. If `Sync` is not the name of a sync object, or if an error
occurs, [`gl:isSync/1`](`isSync/1`) returns `?GL_FALSE`. Note that zero is not
the name of a sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSync.xhtml)
""".
-spec isSync(Sync::i()) -> 0|1.
isSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5590),
  rec(5590).

-doc """
[`gl:deleteSync/1`](`deleteSync/1`) deletes the sync object specified by `Sync`.
If the fence command corresponding to the specified sync object has completed,
or if no [`gl:waitSync/3`](`waitSync/3`) or
[`gl:clientWaitSync/3`](`clientWaitSync/3`) commands are blocking on `Sync`, the
object is deleted immediately. Otherwise, `Sync` is flagged for deletion and
will be deleted when it is no longer associated with any fence command and is no
longer blocking any [`gl:waitSync/3`](`waitSync/3`) or
[`gl:clientWaitSync/3`](`clientWaitSync/3`) command. In either case, after
[`gl:deleteSync/1`](`deleteSync/1`) returns, the name `Sync` is invalid and can
no longer be used to refer to the sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSync.xhtml)
""".
-spec deleteSync(Sync::i()) -> 'ok'.
deleteSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5591),
  ok.

-doc """
[`gl:clientWaitSync/3`](`clientWaitSync/3`) causes the client to block and wait
for the sync object specified by `Sync` to become signaled. If `Sync` is
signaled when [`gl:clientWaitSync/3`](`clientWaitSync/3`) is called,
[`gl:clientWaitSync/3`](`clientWaitSync/3`) returns immediately, otherwise it
will block and wait for up to `Timeout` nanoseconds for `Sync` to become
signaled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClientWaitSync.xhtml)
""".
-spec clientWaitSync(Sync::i(), Flags::i(), Timeout::i()) -> enum().
clientWaitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5592),
  rec(5592).

-doc """
[`gl:waitSync/3`](`waitSync/3`) causes the GL server to block and wait until
`Sync` becomes signaled. `Sync` is the name of an existing sync object upon
which to wait. `Flags` and `Timeout` are currently not used and must be set to
zero and the special value `?GL_TIMEOUT_IGNORED`, respectively

`Flags` and `Timeout` are placeholders for anticipated future extensions of sync
object capabilities. They must have these reserved values in order that existing
code calling [`gl:waitSync/3`](`waitSync/3`) operate properly in the presence of
such extensions.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glWaitSync.xhtml)
""".
-spec waitSync(Sync::i(), Flags::i(), Timeout::i()) -> 'ok'.
waitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5593),
  ok.

-doc(#{equiv => getIntegerv/1}).
-spec getInteger64v(Pname::enum()) -> [i()].
getInteger64v(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5594),
  rec(5594).

-doc """
[`gl:getSynciv/3`](`getSynciv/3`) retrieves properties of a sync object. `Sync`
specifies the name of the sync object whose properties to retrieve.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSync.xhtml)
""".
-spec getSynciv(Sync::i(), Pname::enum(), BufSize::i()) -> [i()].
getSynciv(Sync,Pname,BufSize) when is_integer(Sync),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Pname,BufSize,5595),
  rec(5595).

-doc(#{equiv => getIntegerv/1}).
-spec getInteger64i_v(Target::enum(), Index::i()) -> [i()].
getInteger64i_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5596),
  rec(5596).

-doc(#{equiv => getBufferParameterivARB/2}).
-spec getBufferParameteri64v(Target::enum(), Pname::enum()) -> [i()].
getBufferParameteri64v(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5597),
  rec(5597).

-doc(#{equiv => framebufferTextureLayer/5}).
-spec framebufferTexture(Target::enum(), Attachment::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture(Target,Attachment,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,5598),
  ok.

-doc """
[`gl:texImage2DMultisample/6`](`texImage2DMultisample/6`) establishes the data
storage, format, dimensions and number of samples of a multisample texture's
image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2DMultisample.xhtml)
""".
-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Fixedsamplelocations::0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5599),
  ok.

-doc """
[`gl:texImage3DMultisample/7`](`texImage3DMultisample/7`) establishes the data
storage, format, dimensions and number of samples of a multisample texture's
image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3DMultisample.xhtml)
""".
-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Fixedsamplelocations::0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5600),
  ok.

-doc """
[`gl:getMultisamplefv/2`](`getMultisamplefv/2`) queries the location of a given
sample. `Pname` specifies the sample parameter to retrieve and must be
`?GL_SAMPLE_POSITION`. `Index` corresponds to the sample for which the location
should be returned. The sample location is returned as two floating-point values
in `Val[0]` and `Val[1]`, each between 0 and 1, corresponding to the `X` and `Y`
locations respectively in the GL pixel space of that sample. (0.5, 0.5) this
corresponds to the pixel center. `Index` must be between zero and the value of
`?GL_SAMPLES` minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetMultisample.xhtml)
""".
-spec getMultisamplefv(Pname::enum(), Index::i()) -> {f(),f()}.
getMultisamplefv(Pname,Index) when is_integer(Pname),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Index,5601),
  rec(5601).

-doc """
[`gl:sampleMaski/2`](`sampleMaski/2`) sets one 32-bit sub-word of the multi-word
sample mask, `?GL_SAMPLE_MASK_VALUE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleMaski.xhtml)
""".
-spec sampleMaski(MaskNumber::i(), Mask::i()) -> 'ok'.
sampleMaski(MaskNumber,Mask) when is_integer(MaskNumber),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(MaskNumber,Mask,5602),
  ok.

-doc """
[`gl:bindFragDataLocationIndexed/4`](`bindFragDataLocationIndexed/4`) specifies
that the varying out variable `Name` in `Program` should be bound to fragment
color `ColorNumber` when the program is next linked. `Index` may be zero or one
to specify that the color be used as either the first or second color input to
the blend equation, respectively.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFragDataLocationIndexed.xhtml)
""".
-spec bindFragDataLocationIndexed(Program::i(), ColorNumber::i(), Index::i(), Name::string()) -> 'ok'.
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) when is_integer(Program),is_integer(ColorNumber),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ColorNumber,Index,NameBin,5603),
  ok.

-doc """
[`gl:getFragDataIndex/2`](`getFragDataIndex/2`) returns the index of the
fragment color to which the variable `Name` was bound when the program object
`Program` was last linked. If `Name` is not a varying out variable of `Program`,
or if an error occurs, -1 will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataIndex.xhtml)
""".
-spec getFragDataIndex(Program::i(), Name::string()) -> i().
getFragDataIndex(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5604),
  rec(5604).

-doc """
[`gl:genSamplers/1`](`genSamplers/1`) returns `N` sampler object names in
`Samplers`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genSamplers/1`](`genSamplers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenSamplers.xhtml)
""".
-spec genSamplers(Count::i()) -> [i()].
genSamplers(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5605),
  rec(5605).

-doc """
[`gl:deleteSamplers/1`](`deleteSamplers/1`) deletes `N` sampler objects named by
the elements of the array `Samplers`. After a sampler object is deleted, its
name is again unused. If a sampler object that is currently bound to a sampler
unit is deleted, it is as though [`gl:bindSampler/2`](`bindSampler/2`) is called
with unit set to the unit the sampler is bound to and sampler zero. Unused names
in samplers are silently ignored, as is the reserved name zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSamplers.xhtml)
""".
-spec deleteSamplers(Samplers::[i()]) -> 'ok'.
deleteSamplers(Samplers) when is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(Count,Samplers,5606),
  ok.

-doc """
[`gl:isSampler/1`](`isSampler/1`) returns `?GL_TRUE` if `Id` is currently the
name of a sampler object. If `Id` is zero, or is a non-zero value that is not
currently the name of a sampler object, or if an error occurs,
[`gl:isSampler/1`](`isSampler/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSampler.xhtml)
""".
-spec isSampler(Sampler::i()) -> 0|1.
isSampler(Sampler) when is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,5607),
  rec(5607).

-doc """
[`gl:bindSampler/2`](`bindSampler/2`) binds `Sampler` to the texture unit at
index `Unit`. `Sampler` must be zero or the name of a sampler object previously
returned from a call to [`gl:genSamplers/1`](`genSamplers/1`). `Unit` must be
less than the value of `?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindSampler.xhtml)
""".
-spec bindSampler(Unit::i(), Sampler::i()) -> 'ok'.
bindSampler(Unit,Sampler) when is_integer(Unit),is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Sampler,5608),
  ok.

-doc(#{equiv => samplerParameteriv/3}).
-spec samplerParameteri(Sampler::i(), Pname::enum(), Param::i()) -> 'ok'.
samplerParameteri(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5609),
  ok.

-doc """
[`gl:samplerParameter()`](`samplerParameteri/3`) assigns the value or values in
`Params` to the sampler parameter specified as `Pname`. `Sampler` specifies the
sampler object to be modified, and must be the name of a sampler object
previously returned from a call to [`gl:genSamplers/1`](`genSamplers/1`). The
following symbols are accepted in `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSamplerParameter.xhtml)
""".
-spec samplerParameteriv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameteriv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5610),
  ok.

-doc(#{equiv => samplerParameteriv/3}).
-spec samplerParameterf(Sampler::i(), Pname::enum(), Param::f()) -> 'ok'.
samplerParameterf(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5611),
  ok.

-doc(#{equiv => samplerParameteriv/3}).
-spec samplerParameterfv(Sampler::i(), Pname::enum(), Param::[f()]) -> 'ok'.
samplerParameterfv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5612),
  ok.

-doc(#{equiv => samplerParameteriv/3}).
-spec samplerParameterIiv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameterIiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5613),
  ok.

-doc(#{equiv => samplerParameteriv/3}).
-spec samplerParameterIuiv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameterIuiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5614),
  ok.

-doc """
[`gl:getSamplerParameter()`](`getSamplerParameteriv/2`) returns in `Params` the
value or values of the sampler parameter specified as `Pname`. `Sampler` defines
the target sampler, and must be the name of an existing sampler object, returned
from a previous call to [`gl:genSamplers/1`](`genSamplers/1`). `Pname` accepts
the same symbols as [`gl:samplerParameter()`](`samplerParameteri/3`), with the
same interpretations:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSamplerParameter.xhtml)
""".
-spec getSamplerParameteriv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameteriv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5615),
  rec(5615).

-doc(#{equiv => getSamplerParameteriv/2}).
-spec getSamplerParameterIiv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameterIiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5616),
  rec(5616).

-doc(#{equiv => getSamplerParameteriv/2}).
-spec getSamplerParameterfv(Sampler::i(), Pname::enum()) -> [f()].
getSamplerParameterfv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5617),
  rec(5617).

-doc(#{equiv => getSamplerParameteriv/2}).
-spec getSamplerParameterIuiv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameterIuiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5618),
  rec(5618).

-doc """
[`gl:queryCounter/2`](`queryCounter/2`) causes the GL to record the current time
into the query object named `Id`. `Target` must be `?GL_TIMESTAMP`. The time is
recorded after all previous commands on the GL client and server state and the
framebuffer have been fully realized. When the time is recorded, the query
result for that object is marked available.
[`gl:queryCounter/2`](`queryCounter/2`) timer queries can be used within a
[`gl:beginQuery/2`](`beginQuery/2`) / [`gl:endQuery/1`](`beginQuery/2`) block
where the target is `?GL_TIME_ELAPSED` and it does not affect the result of that
query object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glQueryCounter.xhtml)
""".
-spec queryCounter(Id::i(), Target::enum()) -> 'ok'.
queryCounter(Id,Target) when is_integer(Id),is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Target,5619),
  ok.

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryObjecti64v(Id::i(), Pname::enum()) -> i().
getQueryObjecti64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5620),
  rec(5620).

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryObjectui64v(Id::i(), Pname::enum()) -> i().
getQueryObjectui64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5621),
  rec(5621).

-doc """
[`gl:vertexAttribDivisor/2`](`vertexAttribDivisor/2`) modifies the rate at which
generic vertex attributes advance when rendering multiple instances of
primitives in a single draw call. If `Divisor` is zero, the attribute at slot
`Index` advances once per vertex. If `Divisor` is non-zero, the attribute
advances once per `Divisor` instances of the set(s) of vertices being rendered.
An attribute is referred to as instanced if its
`?GL_VERTEX_ATTRIB_ARRAY_DIVISOR` value is non-zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribDivisor.xhtml)
""".
-spec vertexAttribDivisor(Index::i(), Divisor::i()) -> 'ok'.
vertexAttribDivisor(Index,Divisor) when is_integer(Index),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Divisor,5622),
  ok.

-doc """
[`gl:minSampleShading/1`](`minSampleShading/1`) specifies the rate at which
samples are shaded within a covered pixel. Sample-rate shading is enabled by
calling [`gl:enable/1`](`enable/1`) with the parameter `?GL_SAMPLE_SHADING`. If
`?GL_MULTISAMPLE` or `?GL_SAMPLE_SHADING` is disabled, sample shading has no
effect. Otherwise, an implementation must provide at least as many unique color
values for each covered fragment as specified by `Value` times `Samples` where
`Samples` is the value of `?GL_SAMPLES` for the current framebuffer. At least 1
sample for each covered fragment is generated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMinSampleShading.xhtml)
""".
-spec minSampleShading(Value::f()) -> 'ok'.
minSampleShading(Value) when is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Value,5623),
  ok.

-doc """
The blend equations determine how a new pixel (the ''source'' color) is combined
with a pixel already in the framebuffer (the ''destination'' color). This
function sets both the RGB blend equation and the alpha blend equation to a
single equation. [`gl:blendEquationi/2`](`blendEquation/1`) specifies the blend
equation for a single draw buffer whereas
[`gl:blendEquation/1`](`blendEquation/1`) sets the blend equation for all draw
buffers.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquation.xhtml)
""".
-spec blendEquationi(Buf::i(), Mode::enum()) -> 'ok'.
blendEquationi(Buf,Mode) when is_integer(Buf),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Mode,5624),
  ok.

-doc """
The blend equations determines how a new pixel (the ''source'' color) is
combined with a pixel already in the framebuffer (the ''destination'' color).
These functions specify one blend equation for the RGB-color components and one
blend equation for the alpha component.
[`gl:blendEquationSeparatei/3`](`blendEquationSeparate/2`) specifies the blend
equations for a single draw buffer whereas
[`gl:blendEquationSeparate/2`](`blendEquationSeparate/2`) sets the blend
equations for all draw buffers.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquationSeparate.xhtml)
""".
-spec blendEquationSeparatei(Buf::i(), ModeRGB::enum(), ModeAlpha::enum()) -> 'ok'.
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) when is_integer(Buf),is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,ModeRGB,ModeAlpha,5625),
  ok.

-doc """
Pixels can be drawn using a function that blends the incoming (source) RGBA
values with the RGBA values that are already in the frame buffer (the
destination values). Blending is initially disabled. Use
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) with argument
`?GL_BLEND` to enable and disable blending.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml)
""".
-spec blendFunci(Buf::i(), Src::enum(), Dst::enum()) -> 'ok'.
blendFunci(Buf,Src,Dst) when is_integer(Buf),is_integer(Src),is_integer(Dst) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Src,Dst,5626),
  ok.

-doc """
Pixels can be drawn using a function that blends the incoming (source) RGBA
values with the RGBA values that are already in the frame buffer (the
destination values). Blending is initially disabled. Use
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) with argument
`?GL_BLEND` to enable and disable blending.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFuncSeparate.xhtml)
""".
-spec blendFuncSeparatei(Buf::i(), SrcRGB::enum(), DstRGB::enum(), SrcAlpha::enum(), DstAlpha::enum()) -> 'ok'.
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) when is_integer(Buf),is_integer(SrcRGB),is_integer(DstRGB),is_integer(SrcAlpha),is_integer(DstAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha,5627),
  ok.

-doc """
[`gl:drawArraysIndirect/2`](`drawArraysIndirect/2`) specifies multiple geometric
primitives with very few subroutine calls.
[`gl:drawArraysIndirect/2`](`drawArraysIndirect/2`) behaves similarly to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`),
execept that the parameters to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
are stored in memory at the address given by `Indirect`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysIndirect.xhtml)
""".
-spec drawArraysIndirect(Mode::enum(), Indirect::offset()|mem()) -> 'ok'.
drawArraysIndirect(Mode,Indirect) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,5628),
  ok.

-doc """
[`gl:drawElementsIndirect/3`](`drawElementsIndirect/3`) specifies multiple
indexed geometric primitives with very few subroutine calls.
[`gl:drawElementsIndirect/3`](`drawElementsIndirect/3`) behaves similarly to
[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`),
execpt that the parameters to
[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`)
are stored in memory at the address given by `Indirect`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsIndirect.xhtml)
""".
-spec drawElementsIndirect(Mode::enum(), Type::enum(), Indirect::offset()|mem()) -> 'ok'.
drawElementsIndirect(Mode,Type,Indirect) when is_integer(Mode),is_integer(Type),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Type,Indirect,5630),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1d(Location::i(), X::f()) -> 'ok'.
uniform1d(Location,X) when is_integer(Location),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5632),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2d(Location::i(), X::f(), Y::f()) -> 'ok'.
uniform2d(Location,X,Y) when is_integer(Location),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5633),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3d(Location::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
uniform3d(Location,X,Y,Z) when is_integer(Location),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5634),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4d(Location::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
uniform4d(Location,X,Y,Z,W) when is_integer(Location),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5635),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform1dv(Location::i(), Value::[f()]) -> 'ok'.
uniform1dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5636),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform2dv(Location::i(), Value::[{f(),f()}]) -> 'ok'.
uniform2dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5637),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform3dv(Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
uniform3dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5638),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniform4dv(Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniform4dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5639),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5640),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5641),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5642),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2x3dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5643),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix2x4dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5644),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3x2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5645),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix3x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5646),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix4x2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix4x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5647),
  ok.

-doc(#{equiv => uniformMatrix4x3fv/3}).
-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5648),
  ok.

-doc(#{equiv => getUniformuiv/2}).
-spec getUniformdv(Program::i(), Location::i()) -> matrix().
getUniformdv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5649),
  rec(5649).

-doc """
[`gl:getSubroutineUniformLocation/3`](`getSubroutineUniformLocation/3`) returns
the location of the subroutine uniform variable `Name` in the shader stage of
type `Shadertype` attached to `Program`, with behavior otherwise identical to
[`gl:getUniformLocation/2`](`getUniformLocation/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineUniformLocation.xhtml)
""".
-spec getSubroutineUniformLocation(Program::i(), Shadertype::enum(), Name::string()) -> i().
getSubroutineUniformLocation(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5650),
  rec(5650).

-doc """
[`gl:getSubroutineIndex/3`](`getSubroutineIndex/3`) returns the index of a
subroutine uniform within a shader stage attached to a program object. `Program`
contains the name of the program to which the shader is attached. `Shadertype`
specifies the stage from which to query shader subroutine index. `Name` contains
the null-terminated name of the subroutine uniform whose name to query.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineIndex.xhtml)
""".
-spec getSubroutineIndex(Program::i(), Shadertype::enum(), Name::string()) -> i().
getSubroutineIndex(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5651),
  rec(5651).

-doc """
[`gl:getActiveSubroutineUniformName/4`](`getActiveSubroutineUniformName/4`)
retrieves the name of an active shader subroutine uniform. `Program` contains
the name of the program containing the uniform. `Shadertype` specifies the stage
for which the uniform location, given by `Index`, is valid. `Index` must be
between zero and the value of `?GL_ACTIVE_SUBROUTINE_UNIFORMS` minus one for the
shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineUniformName.xhtml)
""".
-spec getActiveSubroutineUniformName(Program::i(), Shadertype::enum(), Index::i(), Bufsize::i()) -> string().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5652),
  rec(5652).

-doc """
[`gl:getActiveSubroutineName/4`](`getActiveSubroutineName/4`) queries the name
of an active shader subroutine uniform from the program object given in
`Program`. `Index` specifies the index of the shader subroutine uniform within
the shader stage given by `Stage`, and must between zero and the value of
`?GL_ACTIVE_SUBROUTINES` minus one for the shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineName.xhtml)
""".
-spec getActiveSubroutineName(Program::i(), Shadertype::enum(), Index::i(), Bufsize::i()) -> string().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5653),
  rec(5653).

-doc """
[`gl:uniformSubroutines()`](`uniformSubroutinesuiv/2`) loads all active
subroutine uniforms for shader stage `Shadertype` of the current program with
subroutine indices from `Indices`, storing `Indices[i]` into the uniform at
location `I`. `Count` must be equal to the value of
`?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS` for the program currently in use at
shader stage `Shadertype`. Furthermore, all values in `Indices` must be less
than the value of `?GL_ACTIVE_SUBROUTINES` for the shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformSubroutines.xhtml)
""".
-spec uniformSubroutinesuiv(Shadertype::enum(), Indices::[i()]) -> 'ok'.
uniformSubroutinesuiv(Shadertype,Indices) when is_integer(Shadertype),is_list(Indices) ->
  IF = get_interface(),
  Count = length(Indices),
  IF:queue_cmd(Shadertype,Count,Indices,5654),
  ok.

-doc """
[`gl:getUniformSubroutine()`](`getUniformSubroutineuiv/2`) retrieves the value
of the subroutine uniform at location `Location` for shader stage `Shadertype`
of the current program. `Location` must be less than the value of
`?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS` for the shader currently in use at
shader stage `Shadertype`. The value of the subroutine uniform is returned in
`Values`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformSubroutine.xhtml)
""".
-spec getUniformSubroutineuiv(Shadertype::enum(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformSubroutineuiv(Shadertype,Location) when is_integer(Shadertype),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Location,5655),
  rec(5655).

-doc """
[`gl:getProgramStage()`](`getProgramStageiv/3`) queries a parameter of a shader
stage attached to a program object. `Program` contains the name of the program
to which the shader is attached. `Shadertype` specifies the stage from which to
query the parameter. `Pname` specifies which parameter should be queried. The
value or values of the parameter to be queried is returned in the variable whose
address is given in `Values`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramStage.xhtml)
""".
-spec getProgramStageiv(Program::i(), Shadertype::enum(), Pname::enum()) -> i().
getProgramStageiv(Program,Shadertype,Pname) when is_integer(Program),is_integer(Shadertype),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Pname,5656),
  rec(5656).

-doc """
[`gl:patchParameter()`](`patchParameteri/2`) specifies the parameters that will
be used for patch primitives. `Pname` specifies the parameter to modify and must
be either `?GL_PATCH_VERTICES`, `?GL_PATCH_DEFAULT_OUTER_LEVEL` or
`?GL_PATCH_DEFAULT_INNER_LEVEL`. For
[`gl:patchParameteri/2`](`patchParameteri/2`), `Value` specifies the new value
for the parameter specified by `Pname`. For
[`gl:patchParameterfv/2`](`patchParameteri/2`), `Values` specifies the address
of an array containing the new values for the parameter specified by `Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPatchParameter.xhtml)
""".
-spec patchParameteri(Pname::enum(), Value::i()) -> 'ok'.
patchParameteri(Pname,Value) when is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Value,5657),
  ok.

-doc(#{equiv => patchParameteri/2}).
-spec patchParameterfv(Pname::enum(), Values::[f()]) -> 'ok'.
patchParameterfv(Pname,Values) when is_integer(Pname),is_list(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Values,5658),
  ok.

-doc """
[`gl:bindTransformFeedback/2`](`bindTransformFeedback/2`) binds the transform
feedback object with name `Id` to the current GL state. `Id` must be a name
previously returned from a call to
[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`). If `Id` has not
previously been bound, a new transform feedback object with name `Id` and
initialized with the default transform state vector is created.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTransformFeedback.xhtml)
""".
-spec bindTransformFeedback(Target::enum(), Id::i()) -> 'ok'.
bindTransformFeedback(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5659),
  ok.

-doc """
[`gl:deleteTransformFeedbacks/1`](`deleteTransformFeedbacks/1`) deletes the `N`
transform feedback objects whose names are stored in the array `Ids`. Unused
names in `Ids` are ignored, as is the name zero. After a transform feedback
object is deleted, its name is again unused and it has no contents. If an active
transform feedback object is deleted, its name immediately becomes unused, but
the underlying object is not deleted until it is no longer active.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTransformFeedbacks.xhtml)
""".
-spec deleteTransformFeedbacks(Ids::[i()]) -> 'ok'.
deleteTransformFeedbacks(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5660),
  ok.

-doc """
[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`) returns `N` previously
unused transform feedback object names in `Ids`. These names are marked as used,
for the purposes of [`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`)
only, but they acquire transform feedback state only when they are first bound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTransformFeedbacks.xhtml)
""".
-spec genTransformFeedbacks(N::i()) -> [i()].
genTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5661),
  rec(5661).

-doc """
[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_TRUE` if `Id`
is currently the name of a transform feedback object. If `Id` is zero, or if
`?id` is not the name of a transform feedback object, or if an error occurs,
[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_FALSE`. If
`Id` is a name returned by
[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`), but that has not yet
been bound through a call to
[`gl:bindTransformFeedback/2`](`bindTransformFeedback/2`), then the name is not
a transform feedback object and
[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTransformFeedback.xhtml)
""".
-spec isTransformFeedback(Id::i()) -> 0|1.
isTransformFeedback(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5662),
  rec(5662).

-doc """
[`gl:pauseTransformFeedback/0`](`pauseTransformFeedback/0`) pauses transform
feedback operations on the currently active transform feedback object. When
transform feedback operations are paused, transform feedback is still considered
active and changing most transform feedback state related to the object results
in an error. However, a new transform feedback object may be bound while
transform feedback is paused.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPauseTransformFeedback.xhtml)
""".
-spec pauseTransformFeedback() -> 'ok'.
pauseTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5663),
  ok.

-doc """
[`gl:resumeTransformFeedback/0`](`resumeTransformFeedback/0`) resumes transform
feedback operations on the currently active transform feedback object. When
transform feedback operations are paused, transform feedback is still considered
active and changing most transform feedback state related to the object results
in an error. However, a new transform feedback object may be bound while
transform feedback is paused.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glResumeTransformFeedback.xhtml)
""".
-spec resumeTransformFeedback() -> 'ok'.
resumeTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5664),
  ok.

-doc """
[`gl:drawTransformFeedback/2`](`drawTransformFeedback/2`) draws primitives of a
type specified by `Mode` using a count retrieved from the transform feedback
specified by `Id`. Calling
[`gl:drawTransformFeedback/2`](`drawTransformFeedback/2`) is equivalent to
calling [`gl:drawArrays/3`](`drawArrays/3`) with `Mode` as specified, `First`
set to zero, and `Count` set to the number of vertices captured on vertex stream
zero the last time transform feedback was active on the transform feedback
object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedback.xhtml)
""".
-spec drawTransformFeedback(Mode::enum(), Id::i()) -> 'ok'.
drawTransformFeedback(Mode,Id) when is_integer(Mode),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,5665),
  ok.

-doc """
[`gl:drawTransformFeedbackStream/3`](`drawTransformFeedbackStream/3`) draws
primitives of a type specified by `Mode` using a count retrieved from the
transform feedback stream specified by `Stream` of the transform feedback object
specified by `Id`. Calling
[`gl:drawTransformFeedbackStream/3`](`drawTransformFeedbackStream/3`) is
equivalent to calling [`gl:drawArrays/3`](`drawArrays/3`) with `Mode` as
specified, `First` set to zero, and `Count` set to the number of vertices
captured on vertex stream `Stream` the last time transform feedback was active
on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackStream.xhtml)
""".
-spec drawTransformFeedbackStream(Mode::enum(), Id::i(), Stream::i()) -> 'ok'.
drawTransformFeedbackStream(Mode,Id,Stream) when is_integer(Mode),is_integer(Id),is_integer(Stream) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,5666),
  ok.

-doc(#{equiv => endQueryIndexed/2}).
-spec beginQueryIndexed(Target::enum(), Index::i(), Id::i()) -> 'ok'.
beginQueryIndexed(Target,Index,Id) when is_integer(Target),is_integer(Index),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Id,5667),
  ok.

-doc """
[`gl:beginQueryIndexed/3`](`beginQueryIndexed/3`) and
[`gl:endQueryIndexed/2`](`beginQueryIndexed/3`) delimit the boundaries of a
query object. `Query` must be a name previously returned from a call to
[`gl:genQueries/1`](`genQueries/1`). If a query object with name `Id` does not
yet exist it is created with the type determined by `Target`. `Target` must be
one of `?GL_SAMPLES_PASSED`, `?GL_ANY_SAMPLES_PASSED`,
`?GL_PRIMITIVES_GENERATED`, `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN`, or
`?GL_TIME_ELAPSED`. The behavior of the query object depends on its type and is
as follows.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQueryIndexed.xhtml)
""".
-spec endQueryIndexed(Target::enum(), Index::i()) -> 'ok'.
endQueryIndexed(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5668),
  ok.

-doc """
[`gl:getQueryIndexediv/3`](`getQueryIndexediv/3`) returns in `Params` a selected
parameter of the indexed query object target specified by `Target` and `Index`.
`Index` specifies the index of the query object target and must be between zero
and a target-specific maxiumum.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryIndexed.xhtml)
""".
-spec getQueryIndexediv(Target::enum(), Index::i(), Pname::enum()) -> i().
getQueryIndexediv(Target,Index,Pname) when is_integer(Target),is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Pname,5669),
  rec(5669).

-doc """
[`gl:releaseShaderCompiler/0`](`releaseShaderCompiler/0`) provides a hint to the
implementation that it may free internal resources associated with its shader
compiler. [`gl:compileShader/1`](`compileShader/1`) may subsequently be called
and the implementation may at that time reallocate resources previously freed by
the call to [`gl:releaseShaderCompiler/0`](`releaseShaderCompiler/0`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReleaseShaderCompiler.xhtml)
""".
-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler()  ->
  IF = get_interface(),
  IF:queue_cmd(5670),
  ok.

-doc """
[`gl:shaderBinary/3`](`shaderBinary/3`) loads pre-compiled shader binary code
into the `Count` shader objects whose handles are given in `Shaders`. `Binary`
points to `Length` bytes of binary shader code stored in client memory.
`BinaryFormat` specifies the format of the pre-compiled code.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderBinary.xhtml)
""".
-spec shaderBinary(Shaders::[i()], Binaryformat::enum(), Binary::binary()) -> 'ok'.
shaderBinary(Shaders,Binaryformat,Binary) when is_list(Shaders),is_integer(Binaryformat),is_binary(Binary) ->
  IF = get_interface(),
  Count = length(Shaders),
  IF:queue_cmd(Count,Shaders,Binaryformat,Binary,5671),
  ok.

-doc """
[`gl:getShaderPrecisionFormat/2`](`getShaderPrecisionFormat/2`) retrieves the
numeric range and precision for the implementation's representation of
quantities in different numeric formats in specified shader type. `ShaderType`
specifies the type of shader for which the numeric precision and range is to be
retrieved and must be one of `?GL_VERTEX_SHADER` or `?GL_FRAGMENT_SHADER`.
`PrecisionType` specifies the numeric format to query and must be one of
`?GL_LOW_FLOAT`, `?GL_MEDIUM_FLOAT``?GL_HIGH_FLOAT`, `?GL_LOW_INT`,
`?GL_MEDIUM_INT`, or `?GL_HIGH_INT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderPrecisionFormat.xhtml)
""".
-spec getShaderPrecisionFormat(Shadertype::enum(), Precisiontype::enum()) -> {Range::{i(),i()},Precision::i()}.
getShaderPrecisionFormat(Shadertype,Precisiontype) when is_integer(Shadertype),is_integer(Precisiontype) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Precisiontype,5672),
  rec(5672).

-doc """
After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes.
[`gl:depthRange/2`](`depthRange/2`) specifies a linear mapping of the normalized
depth coordinates in this range to window depth coordinates. Regardless of the
actual depth buffer implementation, window coordinate depth values are treated
as though they range from 0 through 1 (like color components). Thus, the values
accepted by [`gl:depthRange/2`](`depthRange/2`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRange.xhtml)
""".
-spec depthRangef(N::f(), F::f()) -> 'ok'.
depthRangef(N,F) when is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(N,F,5673),
  ok.

-doc """
[`gl:clearDepth/1`](`clearDepth/1`) specifies the depth value used by
[`gl:clear/1`](`clear/1`) to clear the depth buffer. Values specified by
[`gl:clearDepth/1`](`clearDepth/1`) are clamped to the range \[0 1].

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearDepth.xhtml)
""".
-spec clearDepthf(D::f()) -> 'ok'.
clearDepthf(D) when is_float(D) ->
  IF = get_interface(),
  IF:queue_cmd(D,5674),
  ok.

-doc """
[`gl:getProgramBinary/2`](`getProgramBinary/2`) returns a binary representation
of the compiled and linked executable for `Program` into the array of bytes
whose address is specified in `Binary`. The maximum number of bytes that may be
written into `Binary` is specified by `BufSize`. If the program binary is
greater in size than `BufSize` bytes, then an error is generated, otherwise the
actual number of bytes written into `Binary` is returned in the variable whose
address is given by `Length`. If `Length` is `?NULL`, then no length is
returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramBinary.xhtml)
""".
-spec getProgramBinary(Program::i(), BufSize::i()) -> {BinaryFormat::enum(),Binary::binary()}.
getProgramBinary(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5675),
  rec(5675).

-doc """
[`gl:programBinary/3`](`programBinary/3`) loads a program object with a program
binary previously returned from [`gl:getProgramBinary/2`](`getProgramBinary/2`).
`BinaryFormat` and `Binary` must be those returned by a previous call to
[`gl:getProgramBinary/2`](`getProgramBinary/2`), and `Length` must be the length
returned by [`gl:getProgramBinary/2`](`getProgramBinary/2`), or by
[`gl:getProgram()`](`getProgramiv/2`) when called with `Pname` set to
`?GL_PROGRAM_BINARY_LENGTH`. If these conditions are not met, loading the
program binary will fail and `Program`'s `?GL_LINK_STATUS` will be set to
`?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramBinary.xhtml)
""".
-spec programBinary(Program::i(), BinaryFormat::enum(), Binary::binary()) -> 'ok'.
programBinary(Program,BinaryFormat,Binary) when is_integer(Program),is_integer(BinaryFormat),is_binary(Binary) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BinaryFormat,Binary,5676),
  ok.

-doc """
[`gl:programParameter()`](`programParameteri/3`) specifies a new value for the
parameter nameed by `Pname` for the program object `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramParameter.xhtml)
""".
-spec programParameteri(Program::i(), Pname::enum(), Value::i()) -> 'ok'.
programParameteri(Program,Pname,Value) when is_integer(Program),is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,Value,5677),
  ok.

-doc """
[`gl:useProgramStages/3`](`useProgramStages/3`) binds executables from a program
object associated with a specified set of shader stages to the program pipeline
object given by `Pipeline`. `Pipeline` specifies the program pipeline object to
which to bind the executables. `Stages` contains a logical combination of bits
indicating the shader stages to use within `Program` with the program pipeline
object `Pipeline`. `Stages` must be a logical combination of
`?GL_VERTEX_SHADER_BIT`, `?GL_TESS_CONTROL_SHADER_BIT`,
`?GL_TESS_EVALUATION_SHADER_BIT`, `?GL_GEOMETRY_SHADER_BIT`,
`?GL_FRAGMENT_SHADER_BIT` and `?GL_COMPUTE_SHADER_BIT`. Additionally, the
special value `?GL_ALL_SHADER_BITS` may be specified to indicate that all
executables contained in `Program` should be installed in `Pipeline`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgramStages.xhtml)
""".
-spec useProgramStages(Pipeline::i(), Stages::i(), Program::i()) -> 'ok'.
useProgramStages(Pipeline,Stages,Program) when is_integer(Pipeline),is_integer(Stages),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Stages,Program,5678),
  ok.

-doc """
[`gl:activeShaderProgram/2`](`activeShaderProgram/2`) sets the linked program
named by `Program` to be the active program for the program pipeline object
`Pipeline`. The active program in the active program pipeline object is the
target of calls to [`gl:uniform()`](`uniform1f/2`) when no program has been made
current through a call to [`gl:useProgram/1`](`useProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveShaderProgram.xhtml)
""".
-spec activeShaderProgram(Pipeline::i(), Program::i()) -> 'ok'.
activeShaderProgram(Pipeline,Program) when is_integer(Pipeline),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Program,5679),
  ok.

-doc """
[`gl:createShaderProgram()`](`createShaderProgramv/2`) creates a program object
containing compiled and linked shaders for a single stage specified by `Type`.
`Strings` refers to an array of `Count` strings from which to create the shader
executables.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateShaderProgram.xhtml)
""".
-spec createShaderProgramv(Type::enum(), Strings::[unicode:chardata()]) -> i().
createShaderProgramv(Type,Strings) when is_integer(Type),is_list(Strings) ->
  IF = get_interface(),
  StringsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Strings ],
  Count = length(Strings),
  IF:queue_cmd(Type,Count,StringsTemp,5680),
  rec(5680).

-doc """
[`gl:bindProgramPipeline/1`](`bindProgramPipeline/1`) binds a program pipeline
object to the current context. `Pipeline` must be a name previously returned
from a call to [`gl:genProgramPipelines/1`](`genProgramPipelines/1`). If no
program pipeline exists with name `Pipeline` then a new pipeline object is
created with that name and initialized to the default state vector.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindProgramPipeline.xhtml)
""".
-spec bindProgramPipeline(Pipeline::i()) -> 'ok'.
bindProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5681),
  ok.

-doc """
[`gl:deleteProgramPipelines/1`](`deleteProgramPipelines/1`) deletes the `N`
program pipeline objects whose names are stored in the array `Pipelines`. Unused
names in `Pipelines` are ignored, as is the name zero. After a program pipeline
object is deleted, its name is again unused and it has no contents. If program
pipeline object that is currently bound is deleted, the binding for that object
reverts to zero and no program pipeline object becomes current.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgramPipelines.xhtml)
""".
-spec deleteProgramPipelines(Pipelines::[i()]) -> 'ok'.
deleteProgramPipelines(Pipelines) when is_list(Pipelines) ->
  IF = get_interface(),
  N = length(Pipelines),
  IF:queue_cmd(N,Pipelines,5682),
  ok.

-doc """
[`gl:genProgramPipelines/1`](`genProgramPipelines/1`) returns `N` previously
unused program pipeline object names in `Pipelines`. These names are marked as
used, for the purposes of [`gl:genProgramPipelines/1`](`genProgramPipelines/1`)
only, but they acquire program pipeline state only when they are first bound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenProgramPipelines.xhtml)
""".
-spec genProgramPipelines(N::i()) -> [i()].
genProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5683),
  rec(5683).

-doc """
[`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns `?GL_TRUE` if
`Pipeline` is currently the name of a program pipeline object. If `Pipeline` is
zero, or if `?pipeline` is not the name of a program pipeline object, or if an
error occurs, [`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns
`?GL_FALSE`. If `Pipeline` is a name returned by
[`gl:genProgramPipelines/1`](`genProgramPipelines/1`), but that has not yet been
bound through a call to [`gl:bindProgramPipeline/1`](`bindProgramPipeline/1`),
then the name is not a program pipeline object and
[`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgramPipeline.xhtml)
""".
-spec isProgramPipeline(Pipeline::i()) -> 0|1.
isProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5684),
  rec(5684).

-doc """
[`gl:getProgramPipelineiv/2`](`getProgramPipelineiv/2`) retrieves the value of a
property of the program pipeline object `Pipeline`. `Pname` specifies the name
of the parameter whose value to retrieve. The value of the parameter is written
to the variable whose address is given by `Params`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipeline.xhtml)
""".
-spec getProgramPipelineiv(Pipeline::i(), Pname::enum()) -> i().
getProgramPipelineiv(Pipeline,Pname) when is_integer(Pipeline),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Pname,5685),
  rec(5685).

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1i(Program::i(), Location::i(), V0::i()) -> 'ok'.
programUniform1i(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5686),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1iv(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5687),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1f(Program::i(), Location::i(), V0::f()) -> 'ok'.
programUniform1f(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5688),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1fv(Program::i(), Location::i(), Value::[f()]) -> 'ok'.
programUniform1fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5689),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1d(Program::i(), Location::i(), V0::f()) -> 'ok'.
programUniform1d(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5690),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1dv(Program::i(), Location::i(), Value::[f()]) -> 'ok'.
programUniform1dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5691),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1ui(Program::i(), Location::i(), V0::i()) -> 'ok'.
programUniform1ui(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5692),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform1uiv(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5693),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2i(Program::i(), Location::i(), V0::i(), V1::i()) -> 'ok'.
programUniform2i(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5694),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2iv(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5695),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2f(Program::i(), Location::i(), V0::f(), V1::f()) -> 'ok'.
programUniform2f(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5696),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2fv(Program::i(), Location::i(), Value::[{f(),f()}]) -> 'ok'.
programUniform2fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5697),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2d(Program::i(), Location::i(), V0::f(), V1::f()) -> 'ok'.
programUniform2d(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5698),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2dv(Program::i(), Location::i(), Value::[{f(),f()}]) -> 'ok'.
programUniform2dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5699),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2ui(Program::i(), Location::i(), V0::i(), V1::i()) -> 'ok'.
programUniform2ui(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5700),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform2uiv(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5701),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3i(Program::i(), Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
programUniform3i(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5702),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3iv(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5703),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3f(Program::i(), Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
programUniform3f(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5704),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3fv(Program::i(), Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
programUniform3fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5705),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3d(Program::i(), Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
programUniform3d(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5706),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3dv(Program::i(), Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
programUniform3dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5707),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3ui(Program::i(), Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
programUniform3ui(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5708),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform3uiv(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5709),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4i(Program::i(), Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
programUniform4i(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5710),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4iv(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5711),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4f(Program::i(), Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
programUniform4f(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5712),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4fv(Program::i(), Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniform4fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5713),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4d(Program::i(), Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
programUniform4d(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5714),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4dv(Program::i(), Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniform4dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5715),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4ui(Program::i(), Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
programUniform4ui(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5716),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniform4uiv(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5717),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5718),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5719),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5720),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5721),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5722),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5723),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2x3fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5724),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3x2fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix3x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5725),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5726),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5727),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5728),
  ok.

-doc """
[`gl:programUniform()`](`programUniform1i/3`) modifies the value of a uniform
variable or a uniform variable array. The location of the uniform variable to be
modified is specified by `Location`, which should be a value returned by
[`gl:getUniformLocation/2`](`getUniformLocation/2`).
[`gl:programUniform()`](`programUniform1i/3`) operates on the program object
specified by `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramUniform.xhtml)
""".
-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5729),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2x3dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5730),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3x2dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix3x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5731),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5732),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5733),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5734),
  ok.

-doc(#{equiv => programUniformMatrix4x3fv/4}).
-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5735),
  ok.

-doc """
[`gl:validateProgramPipeline/1`](`validateProgramPipeline/1`) instructs the
implementation to validate the shader executables contained in `Pipeline`
against the current GL state. The implementation may use this as an opportunity
to perform any internal shader modifications that may be required to ensure
correct operation of the installed shaders given the current GL state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgramPipeline.xhtml)
""".
-spec validateProgramPipeline(Pipeline::i()) -> 'ok'.
validateProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5736),
  ok.

-doc """
[`gl:getProgramPipelineInfoLog/2`](`getProgramPipelineInfoLog/2`) retrieves the
info log for the program pipeline object `Pipeline`. The info log, including its
null terminator, is written into the array of characters whose address is given
by `InfoLog`. The maximum number of characters that may be written into
`InfoLog` is given by `BufSize`, and the actual number of characters written
into `InfoLog` is returned in the integer whose address is given by `Length`. If
`Length` is `?NULL`, no length is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipelineInfoLog.xhtml)
""".
-spec getProgramPipelineInfoLog(Pipeline::i(), BufSize::i()) -> string().
getProgramPipelineInfoLog(Pipeline,BufSize) when is_integer(Pipeline),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,BufSize,5737),
  rec(5737).

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL1d(Index::i(), X::f()) -> 'ok'.
vertexAttribL1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5738),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL2d(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttribL2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5739),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL3d(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttribL3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5740),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL4d(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttribL4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5741),
  ok.

-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL1dv(Index::i(), {X::f()}) -> 'ok'.
vertexAttribL1dv(Index,{X}) ->  vertexAttribL1d(Index,X).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL2dv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttribL2dv(Index,{X,Y}) ->  vertexAttribL2d(Index,X,Y).
-doc(#{equiv => vertexAttribL4dv/2}).
-spec vertexAttribL3dv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttribL3dv(Index,{X,Y,Z}) ->  vertexAttribL3d(Index,X,Y,Z).
-doc """
The [`gl:vertexAttrib()`](`vertexAttrib1d/2`) family of entry points allows an
application to pass generic vertex attributes in numbered locations.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttrib.xhtml)
""".
-spec vertexAttribL4dv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttribL4dv(Index,{X,Y,Z,W}) ->  vertexAttribL4d(Index,X,Y,Z,W).
-doc """
[`gl:vertexAttribFormat/5`](`vertexAttribFormat/5`),
[`gl:vertexAttribIFormat/4`](`vertexAttribIPointer/5`) and
[`gl:vertexAttribLFormat/4`](`vertexAttribIPointer/5`), as well as
[`gl:vertexArrayAttribFormat/6`](`vertexAttribIPointer/5`),
[`gl:vertexArrayAttribIFormat/5`](`vertexAttribIPointer/5`) and
[`gl:vertexArrayAttribLFormat/5`](`vertexAttribIPointer/5`) specify the
organization of data in vertex arrays. The first three calls operate on the
bound vertex array object, whereas the last three ones modify the state of a
vertex array object with ID `Vaobj`. `Attribindex` specifies the index of the
generic vertex attribute array whose data layout is being described, and must be
less than the value of `?GL_MAX_VERTEX_ATTRIBS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribFormat.xhtml)
""".
-spec vertexAttribLPointer(Index::i(), Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5742),
  ok.

-doc(#{equiv => getVertexAttribiv/2}).
-spec getVertexAttribLdv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribLdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5744),
  rec(5744).

-doc """
[`gl:viewportArrayv/2`](`viewportArrayv/2`) specifies the parameters for
multiple viewports simulataneously. `First` specifies the index of the first
viewport to modify and `Count` specifies the number of viewports to modify.
`First` must be less than the value of `?GL_MAX_VIEWPORTS`, and `First` \+
`Count` must be less than or equal to the value of `?GL_MAX_VIEWPORTS`.
Viewports whose indices lie outside the range [`First`, `First` \+ `Count`) are
not modified. `V` contains the address of an array of floating point values
specifying the left ( x), bottom ( y), width ( w), and height ( h) of each
viewport, in that order. x and y give the location of the viewport's lower left
corner, and w and h give the width and height of the viewport, respectively. The
viewport specifies the affine transformation of x and y from normalized device
coordinates to window coordinates. Let (x nd y nd) be normalized device
coordinates. Then the window coordinates (x w y w) are computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewportArray.xhtml)
""".
-spec viewportArrayv(First::i(), V::[{f(),f(),f(),f()}]) -> 'ok'.
viewportArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5745),
  ok.

-doc(#{equiv => viewportIndexedfv/2}).
-spec viewportIndexedf(Index::i(), X::f(), Y::f(), W::f(), H::f()) -> 'ok'.
viewportIndexedf(Index,X,Y,W,H) when is_integer(Index),is_float(X),is_float(Y),is_float(W),is_float(H) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,W,H,5746),
  ok.

-doc """
[`gl:viewportIndexedf/5`](`viewportIndexedf/5`) and
[`gl:viewportIndexedfv/2`](`viewportIndexedf/5`) specify the parameters for a
single viewport. `Index` specifies the index of the viewport to modify. `Index`
must be less than the value of `?GL_MAX_VIEWPORTS`. For
[`gl:viewportIndexedf/5`](`viewportIndexedf/5`), `X`, `Y`, `W`, and `H` specify
the left, bottom, width and height of the viewport in pixels, respectively. For
[`gl:viewportIndexedfv/2`](`viewportIndexedf/5`), `V` contains the address of an
array of floating point values specifying the left ( x), bottom ( y), width (
w), and height ( h) of each viewport, in that order. x and y give the location
of the viewport's lower left corner, and w and h give the width and height of
the viewport, respectively. The viewport specifies the affine transformation of
x and y from normalized device coordinates to window coordinates. Let (x nd y
nd) be normalized device coordinates. Then the window coordinates (x w y w) are
computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewportIndexed.xhtml)
""".
-spec viewportIndexedfv(Index::i(), V::{f(),f(),f(),f()}) -> 'ok'.
viewportIndexedfv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5747),
  ok.

-doc """
[`gl:scissorArrayv/2`](`scissorArrayv/2`) defines rectangles, called scissor
boxes, in window coordinates for each viewport. `First` specifies the index of
the first scissor box to modify and `Count` specifies the number of scissor
boxes to modify. `First` must be less than the value of `?GL_MAX_VIEWPORTS`, and
`First` \+ `Count` must be less than or equal to the value of
`?GL_MAX_VIEWPORTS`. `V` specifies the address of an array containing integers
specifying the lower left corner of the scissor boxes, and the width and height
of the scissor boxes, in that order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissorArray.xhtml)
""".
-spec scissorArrayv(First::i(), V::[{i(),i(),i(),i()}]) -> 'ok'.
scissorArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5748),
  ok.

-doc(#{equiv => scissorIndexedv/2}).
-spec scissorIndexed(Index::i(), Left::i(), Bottom::i(), Width::i(), Height::i()) -> 'ok'.
scissorIndexed(Index,Left,Bottom,Width,Height) when is_integer(Index),is_integer(Left),is_integer(Bottom),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Left,Bottom,Width,Height,5749),
  ok.

-doc """
[`gl:scissorIndexed/5`](`scissorIndexed/5`) defines the scissor box for a
specified viewport. `Index` specifies the index of scissor box to modify.
`Index` must be less than the value of `?GL_MAX_VIEWPORTS`. For
[`gl:scissorIndexed/5`](`scissorIndexed/5`), `Left`, `Bottom`, `Width` and
`Height` specify the left, bottom, width and height of the scissor box, in
pixels, respectively. For [`gl:scissorIndexedv/2`](`scissorIndexed/5`), `V`
specifies the address of an array containing integers specifying the lower left
corner of the scissor box, and the width and height of the scissor box, in that
order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissorIndexed.xhtml)
""".
-spec scissorIndexedv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
scissorIndexedv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5750),
  ok.

-doc """
After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes. Each viewport has an
independent depth range specified as a linear mapping of the normalized depth
coordinates in this range to window depth coordinates. Regardless of the actual
depth buffer implementation, window coordinate depth values are treated as
though they range from 0 through 1 (like color components).
[`gl:depthRangeArray()`](`depthRangeArrayv/2`) specifies a linear mapping of the
normalized depth coordinates in this range to window depth coordinates for each
viewport in the range [`First`, `First` \+ `Count`). Thus, the values accepted
by [`gl:depthRangeArray()`](`depthRangeArrayv/2`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRangeArray.xhtml)
""".
-spec depthRangeArrayv(First::i(), V::[{f(),f()}]) -> 'ok'.
depthRangeArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5751),
  ok.

-doc """
After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes. Each viewport has an
independent depth range specified as a linear mapping of the normalized depth
coordinates in this range to window depth coordinates. Regardless of the actual
depth buffer implementation, window coordinate depth values are treated as
though they range from 0 through 1 (like color components).
[`gl:depthRangeIndexed/3`](`depthRangeIndexed/3`) specifies a linear mapping of
the normalized depth coordinates in this range to window depth coordinates for a
specified viewport. Thus, the values accepted by
[`gl:depthRangeIndexed/3`](`depthRangeIndexed/3`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRangeIndexed.xhtml)
""".
-spec depthRangeIndexed(Index::i(), N::f(), F::f()) -> 'ok'.
depthRangeIndexed(Index,N,F) when is_integer(Index),is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(Index,N,F,5752),
  ok.

-doc(#{equiv => getIntegerv/1}).
-spec getFloati_v(Target::enum(), Index::i()) -> [f()].
getFloati_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5753),
  rec(5753).

-doc(#{equiv => getIntegerv/1}).
-spec getDoublei_v(Target::enum(), Index::i()) -> [f()].
getDoublei_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5754),
  rec(5754).

-doc """
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
behaves identically to [`gl:drawArrays/3`](`drawArrays/3`) except that
`Instancecount` instances of the range of elements are executed and the value of
the internal counter `InstanceID` advances for each iteration. `InstanceID` is
an internal 32-bit integer counter that may be read by a vertex shader as
`?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysInstancedBaseInstance.xhtml)
""".
-spec drawArraysInstancedBaseInstance(Mode::enum(), First::i(), Count::i(), Instancecount::i(), Baseinstance::i()) -> 'ok'.
drawArraysInstancedBaseInstance(Mode,First,Count,Instancecount,Baseinstance) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,Baseinstance,5755),
  ok.

-doc """
[`gl:drawElementsInstancedBaseInstance/6`](`drawElementsInstancedBaseInstance/6`)
behaves identically to [`gl:drawElements/4`](`drawElements/4`) except that
`Instancecount` instances of the set of elements are executed and the value of
the internal counter `InstanceID` advances for each iteration. `InstanceID` is
an internal 32-bit integer counter that may be read by a vertex shader as
`?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseInstance.xhtml)
""".
-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Instancecount, Baseinstance) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Baseinstance::i().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Instancecount,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Baseinstance,5756),
  ok.

-doc """
[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`)
behaves identically to [`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`)
except that the `i`th element transferred by the corresponding draw call will be
taken from element `Indices`\[i] + `Basevertex` of each enabled array. If the
resulting value is larger than the maximum value representable by `Type`, it is
as if the calculation were upconverted to 32-bit unsigned integers (with
wrapping on overflow conditions). The operation is undefined if the sum would be
negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml)
""".
-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Instancecount, Basevertex, Baseinstance) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Basevertex::i(), Baseinstance::i().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance,5758),
  ok.

-doc "No documentation available.".
-spec getInternalformativ(Target::enum(), Internalformat::enum(), Pname::enum(), BufSize::i()) -> [i()].
getInternalformativ(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5760),
  rec(5760).

-doc """
[`gl:bindImageTexture/7`](`bindImageTexture/7`) binds a single level of a
texture to an image unit for the purpose of reading and writing it from shaders.
`Unit` specifies the zero-based index of the image unit to which to bind the
texture level. `Texture` specifies the name of an existing texture object to
bind to the image unit. If `Texture` is zero, then any existing binding to the
image unit is broken. `Level` specifies the level of the texture to bind to the
image unit.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindImageTexture.xhtml)
""".
-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok'
    when Unit::i(), Texture::i(), Level::i(), Layered::0|1, Layer::i(), Access::enum(), Format::enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) when is_integer(Unit),is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Access),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,Level,Layered,Layer,Access,Format,5761),
  ok.

-doc(#{equiv => memoryBarrierByRegion/1}).
-spec memoryBarrier(Barriers::i()) -> 'ok'.
memoryBarrier(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5762),
  ok.

-doc """
[`gl:texStorage1D/4`](`texStorage1D/4`) and
[`gl:textureStorage1D()`](`texStorage1D/4`) specify the storage requirements for
all levels of a one-dimensional texture simultaneously. Once a texture is
specified with this command, the format and dimensions of all levels become
immutable unless it is a proxy texture. The contents of the image may still be
modified, however, its storage requirements may not change. Such a texture is
referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage1D.xhtml)
""".
-spec texStorage1D(Target::enum(), Levels::i(), Internalformat::enum(), Width::i()) -> 'ok'.
texStorage1D(Target,Levels,Internalformat,Width) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,5763),
  ok.

-doc """
[`gl:texStorage2D/5`](`texStorage2D/5`) and
[`gl:textureStorage2D()`](`texStorage2D/5`) specify the storage requirements for
all levels of a two-dimensional texture or one-dimensional texture array
simultaneously. Once a texture is specified with this command, the format and
dimensions of all levels become immutable unless it is a proxy texture. The
contents of the image may still be modified, however, its storage requirements
may not change. Such a texture is referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage2D.xhtml)
""".
-spec texStorage2D(Target::enum(), Levels::i(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
texStorage2D(Target,Levels,Internalformat,Width,Height) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,5764),
  ok.

-doc """
[`gl:texStorage3D/6`](`texStorage3D/6`) and
[`gl:textureStorage3D()`](`texStorage3D/6`) specify the storage requirements for
all levels of a three-dimensional, two-dimensional array or cube-map array
texture simultaneously. Once a texture is specified with this command, the
format and dimensions of all levels become immutable unless it is a proxy
texture. The contents of the image may still be modified, however, its storage
requirements may not change. Such a texture is referred to as an
`immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage3D.xhtml)
""".
-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok'
    when Target::enum(), Levels::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,Depth,5765),
  ok.

-doc """
[`gl:drawTransformFeedbackInstanced/3`](`drawTransformFeedbackInstanced/3`)
draws multiple copies of a range of primitives of a type specified by `Mode`
using a count retrieved from the transform feedback stream specified by `Stream`
of the transform feedback object specified by `Id`. Calling
[`gl:drawTransformFeedbackInstanced/3`](`drawTransformFeedbackInstanced/3`) is
equivalent to calling [`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`) with
`Mode` and `Instancecount` as specified, `First` set to zero, and `Count` set to
the number of vertices captured on vertex stream zero the last time transform
feedback was active on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackInstanced.xhtml)
""".
-spec drawTransformFeedbackInstanced(Mode::enum(), Id::i(), Instancecount::i()) -> 'ok'.
drawTransformFeedbackInstanced(Mode,Id,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Instancecount,5766),
  ok.

-doc """
[`gl:drawTransformFeedbackStreamInstanced/4`](`drawTransformFeedbackStreamInstanced/4`)
draws multiple copies of a range of primitives of a type specified by `Mode`
using a count retrieved from the transform feedback stream specified by `Stream`
of the transform feedback object specified by `Id`. Calling
[`gl:drawTransformFeedbackStreamInstanced/4`](`drawTransformFeedbackStreamInstanced/4`)
is equivalent to calling [`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`)
with `Mode` and `Instancecount` as specified, `First` set to zero, and `Count`
set to the number of vertices captured on vertex stream `Stream` the last time
transform feedback was active on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackStreamInstanced.xhtml)
""".
-spec drawTransformFeedbackStreamInstanced(Mode::enum(), Id::i(), Stream::i(), Instancecount::i()) -> 'ok'.
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Stream),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,Instancecount,5767),
  ok.

-doc(#{equiv => clearBufferuiv/3}).
-spec clearBufferData(Target, Internalformat, Format, Type, Data) -> 'ok'
    when Target::enum(), Internalformat::enum(), Format::enum(), Type::enum(), Data::offset()|mem().
clearBufferData(Target,Internalformat,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Format,Type,Data,5768),
  ok.

-doc(#{equiv => clearBufferuiv/3}).
-spec clearBufferSubData(Target, Internalformat, Offset, Size, Format, Type, Data) -> 'ok'
    when Target::enum(), Internalformat::enum(), Offset::i(), Size::i(), Format::enum(), Type::enum(), Data::offset()|mem().
clearBufferSubData(Target,Internalformat,Offset,Size,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Offset),is_integer(Size),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Offset,Size,Format,Type,Data,5770),
  ok.

-doc """
[`gl:dispatchCompute/3`](`dispatchCompute/3`) launches one or more compute work
groups. Each work group is processed by the active program object for the
compute shader stage. While the individual shader invocations within a work
group are executed as a unit, work groups are executed completely independently
and in unspecified order. `Num_groups_x`, `Num_groups_y` and `Num_groups_z`
specify the number of local work groups that will be dispatched in the X, Y and
Z dimensions, respectively.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDispatchCompute.xhtml)
""".
-spec dispatchCompute(Num_groups_x::i(), Num_groups_y::i(), Num_groups_z::i()) -> 'ok'.
dispatchCompute(Num_groups_x,Num_groups_y,Num_groups_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,5772),
  ok.

-doc """
[`gl:dispatchComputeIndirect/1`](`dispatchComputeIndirect/1`) launches one or
more compute work groups using parameters stored in the buffer object currently
bound to the `?GL_DISPATCH_INDIRECT_BUFFER` target. Each work group is processed
by the active program object for the compute shader stage. While the individual
shader invocations within a work group are executed as a unit, work groups are
executed completely independently and in unspecified order. `Indirect` contains
the offset into the data store of the buffer object bound to the
`?GL_DISPATCH_INDIRECT_BUFFER` target at which the parameters are stored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDispatchComputeIndirect.xhtml)
""".
-spec dispatchComputeIndirect(Indirect::i()) -> 'ok'.
dispatchComputeIndirect(Indirect) when is_integer(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Indirect,5773),
  ok.

-doc """
[`gl:copyImageSubData/15`](`copyImageSubData/15`) may be used to copy data from
one image (i.e. texture or renderbuffer) to another.
[`gl:copyImageSubData/15`](`copyImageSubData/15`) does not perform
general-purpose conversions such as scaling, resizing, blending, color-space, or
format conversions. It should be considered to operate in a manner similar to a
CPU memcpy. CopyImageSubData can copy between images with different internal
formats, provided the formats are compatible.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyImageSubData.xhtml)
""".
-spec copyImageSubData(SrcName, SrcTarget, SrcLevel, SrcX, SrcY, SrcZ, DstName, DstTarget, DstLevel, DstX, DstY, DstZ, SrcWidth, SrcHeight, SrcDepth) -> 'ok'
    when SrcName::i(), SrcTarget::enum(), SrcLevel::i(), SrcX::i(), SrcY::i(), SrcZ::i(), DstName::i(), DstTarget::enum(), DstLevel::i(), DstX::i(), DstY::i(), DstZ::i(), SrcWidth::i(), SrcHeight::i(), SrcDepth::i().
copyImageSubData(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth) when is_integer(SrcName),is_integer(SrcTarget),is_integer(SrcLevel),is_integer(SrcX),is_integer(SrcY),is_integer(SrcZ),is_integer(DstName),is_integer(DstTarget),is_integer(DstLevel),is_integer(DstX),is_integer(DstY),is_integer(DstZ),is_integer(SrcWidth),is_integer(SrcHeight),is_integer(SrcDepth) ->
  IF = get_interface(),
  IF:queue_cmd(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth,5774),
  ok.

-doc """
[`gl:framebufferParameteri/3`](`framebufferParameteri/3`) and
`glNamedFramebufferParameteri` modify the value of the parameter named `Pname`
in the specified framebuffer object. There are no modifiable parameters of the
default draw and read framebuffer, so they are not valid targets of these
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferParameteri.xhtml)
""".
-spec framebufferParameteri(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
framebufferParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5775),
  ok.

-doc """
[`gl:getFramebufferParameteriv/2`](`getFramebufferParameteriv/2`) and
`glGetNamedFramebufferParameteriv` query parameters of a specified framebuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFramebufferParameter.xhtml)
""".
-spec getFramebufferParameteriv(Target::enum(), Pname::enum()) -> i().
getFramebufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5776),
  rec(5776).

-doc(#{equiv => getInternalformativ/4}).
-spec getInternalformati64v(Target::enum(), Internalformat::enum(), Pname::enum(), BufSize::i()) -> [i()].
getInternalformati64v(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5777),
  rec(5777).

-doc """
[`gl:invalidateTexSubImage/8`](`invalidateTexSubImage/8`) invalidates all or
part of a texture image. `Texture` and `Level` indicated which texture image is
being invalidated. After this command, data in that subregion have undefined
values. `Xoffset`, `Yoffset`, `Zoffset`, `Width`, `Height`, and `Depth` are
interpreted as they are in [`gl:texSubImage3D/11`](`texSubImage3D/11`). For
texture targets that don't have certain dimensions, this command treats those
dimensions as having a size of 1. For example, to invalidate a portion of a two-
dimensional texture, the application would use `Zoffset` equal to zero and
`Depth` equal to one. Cube map textures are treated as an array of six slices in
the z-dimension, where a value of `Zoffset` is interpreted as specifying face
`?GL_TEXTURE_CUBE_MAP_POSITIVE_X` \+ `Zoffset`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateTexSubImage.xhtml)
""".
-spec invalidateTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i().
invalidateTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,5778),
  ok.

-doc """
[`gl:invalidateTexSubImage/8`](`invalidateTexSubImage/8`) invalidates all of a
texture image. `Texture` and `Level` indicated which texture image is being
invalidated. After this command, data in the texture image has undefined values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateTexImage.xhtml)
""".
-spec invalidateTexImage(Texture::i(), Level::i()) -> 'ok'.
invalidateTexImage(Texture,Level) when is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,5779),
  ok.

-doc """
[`gl:invalidateBufferSubData/3`](`invalidateBufferSubData/3`) invalidates all or
part of the content of the data store of a buffer object. After invalidation,
the content of the specified range of the buffer's data store becomes undefined.
The start of the range is given by `Offset` and its size is given by `Length`,
both measured in basic machine units.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateBufferSubData.xhtml)
""".
-spec invalidateBufferSubData(Buffer::i(), Offset::i(), Length::i()) -> 'ok'.
invalidateBufferSubData(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5780),
  ok.

-doc """
[`gl:invalidateBufferData/1`](`invalidateBufferData/1`) invalidates all of the
content of the data store of a buffer object. After invalidation, the content of
the buffer's data store becomes undefined.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateBufferData.xhtml)
""".
-spec invalidateBufferData(Buffer::i()) -> 'ok'.
invalidateBufferData(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5781),
  ok.

-doc """
[`gl:invalidateFramebuffer/2`](`invalidateFramebuffer/2`) and
`glInvalidateNamedFramebufferData` invalidate the entire contents of a specified
set of attachments of a framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateFramebuffer.xhtml)
""".
-spec invalidateFramebuffer(Target::enum(), Attachments::[enum()]) -> 'ok'.
invalidateFramebuffer(Target,Attachments) when is_integer(Target),is_list(Attachments) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,5782),
  ok.

-doc """
[`gl:invalidateSubFramebuffer/6`](`invalidateSubFramebuffer/6`) and
`glInvalidateNamedFramebufferSubData` invalidate the contents of a specified
region of a specified set of attachments of a framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateSubFramebuffer.xhtml)
""".
-spec invalidateSubFramebuffer(Target::enum(), Attachments::[enum()], X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
invalidateSubFramebuffer(Target,Attachments,X,Y,Width,Height) when is_integer(Target),is_list(Attachments),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,X,Y,Width,Height,5783),
  ok.

-doc """
[`gl:multiDrawArraysIndirect/4`](`multiDrawArraysIndirect/4`) specifies multiple
geometric primitives with very few subroutine calls.
[`gl:multiDrawArraysIndirect/4`](`multiDrawArraysIndirect/4`) behaves similarly
to a multitude of calls to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`),
execept that the parameters to each call to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
are stored in an array in memory at the address given by `Indirect`, separated
by the stride, in basic machine units, specified by `Stride`. If `Stride` is
zero, then the array is assumed to be tightly packed in memory.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawArraysIndirect.xhtml)
""".
-spec multiDrawArraysIndirect(Mode::enum(), Indirect::offset()|mem(), Drawcount::i(), Stride::i()) -> 'ok'.
multiDrawArraysIndirect(Mode,Indirect,Drawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Stride,5784),
  ok.

-doc """
[`gl:getProgramInterfaceiv/3`](`getProgramInterfaceiv/3`) queries the property
of the interface identifed by `ProgramInterface` in `Program`, the property name
of which is given by `Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramInterface.xhtml)
""".
-spec getProgramInterfaceiv(Program::i(), ProgramInterface::enum(), Pname::enum()) -> i().
getProgramInterfaceiv(Program,ProgramInterface,Pname) when is_integer(Program),is_integer(ProgramInterface),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Pname,5786),
  rec(5786).

-doc """
[`gl:getProgramResourceIndex/3`](`getProgramResourceIndex/3`) returns the
unsigned integer index assigned to a resource named `Name` in the interface type
`ProgramInterface` of program object `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceIndex.xhtml)
""".
-spec getProgramResourceIndex(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5787),
  rec(5787).

-doc """
[`gl:getProgramResourceName/4`](`getProgramResourceName/4`) retrieves the name
string assigned to the single active resource with an index of `Index` in the
interface `ProgramInterface` of program object `Program`. `Index` must be less
than the number of entries in the active resource list for `ProgramInterface`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceName.xhtml)
""".
-spec getProgramResourceName(Program::i(), ProgramInterface::enum(), Index::i(), BufSize::i()) -> string().
getProgramResourceName(Program,ProgramInterface,Index,BufSize) when is_integer(Program),is_integer(ProgramInterface),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Index,BufSize,5788),
  rec(5788).

-doc """
[`gl:getProgramResourceLocation/3`](`getProgramResourceLocation/3`) returns the
location assigned to the variable named `Name` in interface `ProgramInterface`
of program object `Program`. `Program` must be the name of a program that has
been linked successfully. `ProgramInterface` must be one of `?GL_UNIFORM`,
`?GL_PROGRAM_INPUT`, `?GL_PROGRAM_OUTPUT`, `?GL_VERTEX_SUBROUTINE_UNIFORM`,
`?GL_TESS_CONTROL_SUBROUTINE_UNIFORM`, `?GL_TESS_EVALUATION_SUBROUTINE_UNIFORM`,
`?GL_GEOMETRY_SUBROUTINE_UNIFORM`, `?GL_FRAGMENT_SUBROUTINE_UNIFORM`,
`?GL_COMPUTE_SUBROUTINE_UNIFORM`, or `?GL_TRANSFORM_FEEDBACK_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceLocation.xhtml)
""".
-spec getProgramResourceLocation(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceLocation(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5789),
  rec(5789).

-doc """
[`gl:getProgramResourceLocationIndex/3`](`getProgramResourceLocationIndex/3`)
returns the fragment color index assigned to the variable named `Name` in
interface `ProgramInterface` of program object `Program`. `Program` must be the
name of a program that has been linked successfully. `ProgramInterface` must be
`?GL_PROGRAM_OUTPUT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceLocationIndex.xhtml)
""".
-spec getProgramResourceLocationIndex(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceLocationIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5790),
  rec(5790).

-doc """
[`gl:shaderStorageBlockBinding/3`](`shaderStorageBlockBinding/3`), changes the
active shader storage block with an assigned index of `StorageBlockIndex` in
program object `Program`. `StorageBlockIndex` must be an active shader storage
block index in `Program`. `StorageBlockBinding` must be less than the value of
`?GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS`. If successful,
[`gl:shaderStorageBlockBinding/3`](`shaderStorageBlockBinding/3`) specifies that
`Program` will use the data store of the buffer object bound to the binding
point `StorageBlockBinding` to read and write the values of the buffer variables
in the shader storage block identified by `StorageBlockIndex`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderStorageBlockBinding.xhtml)
""".
-spec shaderStorageBlockBinding(Program::i(), StorageBlockIndex::i(), StorageBlockBinding::i()) -> 'ok'.
shaderStorageBlockBinding(Program,StorageBlockIndex,StorageBlockBinding) when is_integer(Program),is_integer(StorageBlockIndex),is_integer(StorageBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,StorageBlockIndex,StorageBlockBinding,5791),
  ok.

-doc(#{equiv => textureBufferRange/5}).
-spec texBufferRange(Target::enum(), Internalformat::enum(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
texBufferRange(Target,Internalformat,Buffer,Offset,Size) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,Offset,Size,5792),
  ok.

-doc """
[`gl:texStorage2DMultisample/6`](`texStorage2DMultisample/6`) and
[`gl:textureStorage2DMultisample()`](`texStorage2DMultisample/6`) specify the
storage requirements for a two-dimensional multisample texture. Once a texture
is specified with this command, its format and dimensions become immutable
unless it is a proxy texture. The contents of the image may still be modified,
however, its storage requirements may not change. Such a texture is referred to
as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage2DMultisample.xhtml)
""".
-spec texStorage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Fixedsamplelocations::0|1.
texStorage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5793),
  ok.

-doc """
[`gl:texStorage3DMultisample/7`](`texStorage3DMultisample/7`) and
[`gl:textureStorage3DMultisample()`](`texStorage3DMultisample/7`) specify the
storage requirements for a two-dimensional multisample array texture. Once a
texture is specified with this command, its format and dimensions become
immutable unless it is a proxy texture. The contents of the image may still be
modified, however, its storage requirements may not change. Such a texture is
referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage3DMultisample.xhtml)
""".
-spec texStorage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Fixedsamplelocations::0|1.
texStorage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5794),
  ok.

-doc """
[`gl:textureView/8`](`textureView/8`) initializes a texture object as an alias,
or view of another texture object, sharing some or all of the parent texture's
data store with the initialized texture. `Texture` specifies a name previously
reserved by a successful call to [`gl:genTextures/1`](`genTextures/1`) but that
has not yet been bound or given a target. `Target` specifies the target for the
newly initialized texture and must be compatible with the target of the parent
texture, given in `Origtexture` as specified in the following table:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTextureView.xhtml)
""".
-spec textureView(Texture, Target, Origtexture, Internalformat, Minlevel, Numlevels, Minlayer, Numlayers) -> 'ok'
    when Texture::i(), Target::enum(), Origtexture::i(), Internalformat::enum(), Minlevel::i(), Numlevels::i(), Minlayer::i(), Numlayers::i().
textureView(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers) when is_integer(Texture),is_integer(Target),is_integer(Origtexture),is_integer(Internalformat),is_integer(Minlevel),is_integer(Numlevels),is_integer(Minlayer),is_integer(Numlayers) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers,5795),
  ok.

-doc(#{equiv => vertexArrayVertexBuffer/5}).
-spec bindVertexBuffer(Bindingindex::i(), Buffer::i(), Offset::i(), Stride::i()) -> 'ok'.
bindVertexBuffer(Bindingindex,Buffer,Offset,Stride) when is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Buffer,Offset,Stride,5796),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexAttribFormat(Attribindex::i(), Size::i(), Type::enum(), Normalized::0|1, Relativeoffset::i()) -> 'ok'.
vertexAttribFormat(Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Normalized,Relativeoffset,5797),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexAttribIFormat(Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexAttribIFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5798),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexAttribLFormat(Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexAttribLFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5799),
  ok.

-doc """
[`gl:vertexAttribBinding/2`](`vertexAttribBinding/2`) and
[`gl:vertexArrayAttribBinding/3`](`vertexAttribBinding/2`) establishes an
association between the generic vertex attribute of a vertex array object whose
index is given by `Attribindex`, and a vertex buffer binding whose index is
given by `Bindingindex`. For
[`gl:vertexAttribBinding/2`](`vertexAttribBinding/2`), the vertex array object
affected is that currently bound. For
[`gl:vertexArrayAttribBinding/3`](`vertexAttribBinding/2`), `Vaobj` is the name
of the vertex array object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribBinding.xhtml)
""".
-spec vertexAttribBinding(Attribindex::i(), Bindingindex::i()) -> 'ok'.
vertexAttribBinding(Attribindex,Bindingindex) when is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Bindingindex,5800),
  ok.

-doc """
[`gl:vertexBindingDivisor/2`](`vertexBindingDivisor/2`) and
[`gl:vertexArrayBindingDivisor/3`](`vertexBindingDivisor/2`) modify the rate at
which generic vertex attributes advance when rendering multiple instances of
primitives in a single draw command. If `Divisor` is zero, the attributes using
the buffer bound to `Bindingindex` advance once per vertex. If `Divisor` is
non-zero, the attributes advance once per `Divisor` instances of the set(s) of
vertices being rendered. An attribute is referred to as `instanced` if the
corresponding `Divisor` value is non-zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexBindingDivisor.xhtml)
""".
-spec vertexBindingDivisor(Bindingindex::i(), Divisor::i()) -> 'ok'.
vertexBindingDivisor(Bindingindex,Divisor) when is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Divisor,5801),
  ok.

-doc """
[`gl:debugMessageControl/5`](`debugMessageControl/5`) controls the reporting of
debug messages generated by a debug context. The parameters `Source`, `Type` and
`Severity` form a filter to select messages from the pool of potential messages
generated by the GL.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDebugMessageControl.xhtml)
""".
-spec debugMessageControl(Source::enum(), Type::enum(), Severity::enum(), Ids::[i()], Enabled::0|1) -> 'ok'.
debugMessageControl(Source,Type,Severity,Ids,Enabled) when is_integer(Source),is_integer(Type),is_integer(Severity),is_list(Ids),(0 =:= Enabled) orelse (1 =:= Enabled) ->
  IF = get_interface(),
  Count = length(Ids),
  IF:queue_cmd(Source,Type,Severity,Count,Ids,Enabled,5802),
  ok.

-doc """
[`gl:debugMessageInsert/5`](`debugMessageInsert/5`) inserts a user-supplied
message into the debug output queue. `Source` specifies the source that will be
used to classify the message and must be `?GL_DEBUG_SOURCE_APPLICATION` or
`?GL_DEBUG_SOURCE_THIRD_PARTY`. All other sources are reserved for use by the GL
implementation. `Type` indicates the type of the message to be inserted and may
be one of `?GL_DEBUG_TYPE_ERROR`, `?GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR`,
`?GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR`, `?GL_DEBUG_TYPE_PORTABILITY`,
`?GL_DEBUG_TYPE_PERFORMANCE`, `?GL_DEBUG_TYPE_MARKER`,
`?GL_DEBUG_TYPE_PUSH_GROUP`, `?GL_DEBUG_TYPE_POP_GROUP`, or
`?GL_DEBUG_TYPE_OTHER`. `Severity` indicates the severity of the message and may
be `?GL_DEBUG_SEVERITY_LOW`, `?GL_DEBUG_SEVERITY_MEDIUM`,
`?GL_DEBUG_SEVERITY_HIGH` or `?GL_DEBUG_SEVERITY_NOTIFICATION`. `Id` is
available for application defined use and may be any value. This value will be
recorded and used to identify the message.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDebugMessageInsert.xhtml)
""".
-spec debugMessageInsert(Source::enum(), Type::enum(), Id::i(), Severity::enum(), Buf::string()) -> 'ok'.
debugMessageInsert(Source,Type,Id,Severity,Buf) when is_integer(Source),is_integer(Type),is_integer(Id),is_integer(Severity),is_list(Buf) ->
  IF = get_interface(),
  BufBin = unicode:characters_to_binary([Buf|[0]]),
  IF:queue_cmd(Source,Type,Id,Severity,BufBin,5803),
  ok.

-doc """
[`gl:getDebugMessageLog/2`](`getDebugMessageLog/2`) retrieves messages from the
debug message log. A maximum of `Count` messages are retrieved from the log. If
`Sources` is not NULL then the source of each message is written into up to
`Count` elements of the array. If `Types` is not NULL then the type of each
message is written into up to `Count` elements of the array. If `Id` is not NULL
then the identifier of each message is written into up to `Count` elements of
the array. If `Severities` is not NULL then the severity of each message is
written into up to `Count` elements of the array. If `Lengths` is not NULL then
the length of each message is written into up to `Count` elements of the array.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetDebugMessageLog.xhtml)
""".
-spec getDebugMessageLog(Count::i(), BufSize::i()) -> {i(),Sources::[enum()],Types::[enum()],Ids::[i()],Severities::[enum()],MessageLog::[string()]}.
getDebugMessageLog(Count,BufSize) when is_integer(Count),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Count,BufSize,5804),
  rec(5804).

-doc """
[`gl:pushDebugGroup/4`](`pushDebugGroup/4`) pushes a debug group described by
the string `Message` into the command stream. The value of `Id` specifies the ID
of messages generated. The parameter `Length` contains the number of characters
in `Message`. If `Length` is negative, it is implied that `Message` contains a
null terminated string. The message has the specified `Source` and `Id`, the
`Type``?GL_DEBUG_TYPE_PUSH_GROUP`, and
`Severity``?GL_DEBUG_SEVERITY_NOTIFICATION`. The GL will put a new debug group
on top of the debug group stack which inherits the control of the volume of
debug output of the debug group previously residing on the top of the debug
group stack. Because debug groups are strictly hierarchical, any additional
control of the debug output volume will only apply within the active debug group
and the debug groups pushed on top of the active debug group.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPushDebugGroup.xhtml)
""".
-spec pushDebugGroup(Source::enum(), Id::i(), Length::i(), Message::string()) -> 'ok'.
pushDebugGroup(Source,Id,Length,Message) when is_integer(Source),is_integer(Id),is_integer(Length),is_list(Message) ->
  IF = get_interface(),
  MessageBin = unicode:characters_to_binary([Message|[0]]),
  IF:queue_cmd(Source,Id,Length,MessageBin,5805),
  ok.

-doc(#{equiv => pushDebugGroup/4}).
-spec popDebugGroup() -> 'ok'.
popDebugGroup()  ->
  IF = get_interface(),
  IF:queue_cmd(5806),
  ok.

-doc """
[`gl:objectPtrLabel/3`](`objectPtrLabel/3`) labels the sync object identified by
`Ptr`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glObjectPtrLabel.xhtml)
""".
-spec objectPtrLabel(Ptr::offset()|mem(), Length::i(), Label::string()) -> 'ok'.
objectPtrLabel(Ptr,Length,Label) when is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr),is_integer(Length),is_list(Label) ->
  IF = get_interface(),
  LabelBin = unicode:characters_to_binary([Label|[0]]),
  IF:queue_cmd(Ptr,Length,LabelBin,5807),
  ok.

-doc """
[`gl:bufferStorage/4`](`bufferStorage/4`) and `glNamedBufferStorage` create a
new immutable data store. For [`gl:bufferStorage/4`](`bufferStorage/4`), the
buffer object currently bound to `Target` will be initialized. For
`glNamedBufferStorage`, `Buffer` is the name of the buffer object that will be
configured. The size of the data store is specified by `Size`. If an initial
data is available, its address may be supplied in `Data`. Otherwise, to create
an uninitialized data store, `Data` should be `?NULL`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferStorage.xhtml)
""".
-spec bufferStorage(Target::enum(), Size::i(), Data::offset()|mem(), Flags::i()) -> 'ok'.
bufferStorage(Target,Size,Data,Flags) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Flags,5809),
  ok.

-doc """
[`gl:clearTexImage/5`](`clearTexImage/5`) fills all an image contained in a
texture with an application supplied value. `Texture` must be the name of an
existing texture. Further, `Texture` may not be the name of a buffer texture,
nor may its internal format be compressed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearTexImage.xhtml)
""".
-spec clearTexImage(Texture::i(), Level::i(), Format::enum(), Type::enum(), Data::offset()|mem()) -> 'ok'.
clearTexImage(Texture,Level,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Format,Type,Data,5811),
  ok.

-doc """
[`gl:clearTexSubImage/11`](`clearTexSubImage/11`) fills all or part of an image
contained in a texture with an application supplied value. `Texture` must be the
name of an existing texture. Further, `Texture` may not be the name of a buffer
texture, nor may its internal format be compressed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearTexSubImage.xhtml)
""".
-spec clearTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Data::offset()|mem().
clearTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data,5813),
  ok.

-doc """
[`gl:bindBuffersBase/3`](`bindBuffersBase/3`) binds a set of `Count` buffer
objects whose names are given in the array `Buffers` to the `Count` consecutive
binding points starting from index `First` of the array of targets specified by
`Target`. If `Buffers` is `?NULL` then
[`gl:bindBuffersBase/3`](`bindBuffersBase/3`) unbinds any buffers that are
currently bound to the referenced binding points. Assuming no errors are
generated, it is equivalent to the following pseudo-code, which calls
[`gl:bindBufferBase/3`](`bindBufferBase/3`), with the exception that the
non-indexed `Target` is not changed by
[`gl:bindBuffersBase/3`](`bindBuffersBase/3`):

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffersBase.xhtml)
""".
-spec bindBuffersBase(Target::enum(), First::i(), Buffers::[i()]) -> 'ok'.
bindBuffersBase(Target,First,Buffers) when is_integer(Target),is_integer(First),is_list(Buffers) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,5815),
  ok.

-doc """
[`gl:bindBuffersRange/5`](`bindBuffersRange/5`) binds a set of `Count` ranges
from buffer objects whose names are given in the array `Buffers` to the `Count`
consecutive binding points starting from index `First` of the array of targets
specified by `Target`. `Offsets` specifies the address of an array containing
`Count` starting offsets within the buffers, and `Sizes` specifies the address
of an array of `Count` sizes of the ranges. If `Buffers` is `?NULL` then
`Offsets` and `Sizes` are ignored and
[`gl:bindBuffersRange/5`](`bindBuffersRange/5`) unbinds any buffers that are
currently bound to the referenced binding points. Assuming no errors are
generated, it is equivalent to the following pseudo-code, which calls
[`gl:bindBufferRange/5`](`bindBufferRange/5`), with the exception that the
non-indexed `Target` is not changed by
[`gl:bindBuffersRange/5`](`bindBuffersRange/5`):

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffersRange.xhtml)
""".
-spec bindBuffersRange(Target::enum(), First::i(), Buffers::[i()], Offsets::[i()], Sizes::[i()]) -> 'ok'.
bindBuffersRange(Target,First,Buffers,Offsets,Sizes) when is_integer(Target),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Sizes) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,Offsets,Sizes,5816),
  ok.

-doc """
[`gl:bindTextures/2`](`bindTextures/2`) binds an array of existing texture
objects to a specified number of consecutive texture units. `Count` specifies
the number of texture objects whose names are stored in the array `Textures`.
That number of texture names are read from the array and bound to the `Count`
consecutive texture units starting from `First`. The target, or type of texture
is deduced from the texture object and each texture is bound to the
corresponding target of the texture unit. If the name zero appears in the
`Textures` array, any existing binding to any target of the texture unit is
reset and the default texture for that target is bound in its place. Any
non-zero entry in `Textures` must be the name of an existing texture object. If
`Textures` is `?NULL` then it is as if an appropriately sized array containing
only zeros had been specified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTextures.xhtml)
""".
-spec bindTextures(First::i(), Textures::[i()]) -> 'ok'.
bindTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5817),
  ok.

-doc """
[`gl:bindSamplers/2`](`bindSamplers/2`) binds samplers from an array of existing
sampler objects to a specified number of consecutive sampler units. `Count`
specifies the number of sampler objects whose names are stored in the array
`Samplers`. That number of sampler names is read from the array and bound to the
`Count` consecutive sampler units starting from `First`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindSamplers.xhtml)
""".
-spec bindSamplers(First::i(), Samplers::[i()]) -> 'ok'.
bindSamplers(First,Samplers) when is_integer(First),is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(First,Count,Samplers,5818),
  ok.

-doc """
[`gl:bindImageTextures/2`](`bindImageTextures/2`) binds images from an array of
existing texture objects to a specified number of consecutive image units.
`Count` specifies the number of texture objects whose names are stored in the
array `Textures`. That number of texture names are read from the array and bound
to the `Count` consecutive texture units starting from `First`. If the name zero
appears in the `Textures` array, any existing binding to the image unit is
reset. Any non-zero entry in `Textures` must be the name of an existing texture
object. When a non-zero entry in `Textures` is present, the image at level zero
is bound, the binding is considered layered, with the first layer set to zero,
and the image is bound for read-write access. The image unit format parameter is
taken from the internal format of the image at level zero of the texture object.
For cube map textures, the internal format of the positive X image of level zero
is used. If `Textures` is `?NULL` then it is as if an appropriately sized array
containing only zeros had been specified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindImageTextures.xhtml)
""".
-spec bindImageTextures(First::i(), Textures::[i()]) -> 'ok'.
bindImageTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5819),
  ok.

-doc(#{equiv => vertexArrayVertexBuffers/5}).
-spec bindVertexBuffers(First::i(), Buffers::[i()], Offsets::[i()], Strides::[i()]) -> 'ok'.
bindVertexBuffers(First,Buffers,Offsets,Strides) when is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(First,Count,Buffers,Offsets,Strides,5820),
  ok.

-doc """
[`gl:clipControl/2`](`clipControl/2`) controls the clipping volume behavior and
the clip coordinate to window coordinate transformation behavior.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClipControl.xhtml)
""".
-spec clipControl(Origin::enum(), Depth::enum()) -> 'ok'.
clipControl(Origin,Depth) when is_integer(Origin),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Origin,Depth,5821),
  ok.

-doc """
[`gl:createTransformFeedbacks/1`](`createTransformFeedbacks/1`) returns `N`
previously unused transform feedback object names in `Ids`, each representing a
new transform feedback object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateTransformFeedbacks.xhtml)
""".
-spec createTransformFeedbacks(N::i()) -> [i()].
createTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5822),
  rec(5822).

-doc """
[`gl:transformFeedbackBufferBase/3`](`transformFeedbackBufferBase/3`) binds the
buffer object `Buffer` to the binding point at index `Index` of the transform
feedback object `Xfb`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackBufferBase.xhtml)
""".
-spec transformFeedbackBufferBase(Xfb::i(), Index::i(), Buffer::i()) -> 'ok'.
transformFeedbackBufferBase(Xfb,Index,Buffer) when is_integer(Xfb),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,5823),
  ok.

-doc """
[`gl:transformFeedbackBufferRange/5`](`transformFeedbackBufferRange/5`) binds a
range of the buffer object `Buffer` represented by `Offset` and `Size` to the
binding point at index `Index` of the transform feedback object `Xfb`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackBufferRange.xhtml)
""".
-spec transformFeedbackBufferRange(Xfb::i(), Index::i(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
transformFeedbackBufferRange(Xfb,Index,Buffer,Offset,Size) when is_integer(Xfb),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,Offset,Size,5824),
  ok.

-doc """
[`gl:createBuffers/1`](`createBuffers/1`) returns `N` previously unused buffer
names in `Buffers`, each representing a new buffer object initialized as if it
had been bound to an unspecified target.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateBuffers.xhtml)
""".
-spec createBuffers(N::i()) -> [i()].
createBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5825),
  rec(5825).

-doc """
[`gl:flushMappedBufferRange/3`](`flushMappedBufferRange/3`) indicates that
modifications have been made to a range of a mapped buffer object. The buffer
object must previously have been mapped with the `?GL_MAP_FLUSH_EXPLICIT_BIT`
flag.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlushMappedBufferRange.xhtml)
""".
-spec flushMappedNamedBufferRange(Buffer::i(), Offset::i(), Length::i()) -> 'ok'.
flushMappedNamedBufferRange(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5826),
  ok.

-doc """
[`gl:createFramebuffers/1`](`createFramebuffers/1`) returns `N` previously
unused framebuffer names in `Framebuffers`, each representing a new framebuffer
object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateFramebuffers.xhtml)
""".
-spec createFramebuffers(N::i()) -> [i()].
createFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5827),
  rec(5827).

-doc """
[`gl:createRenderbuffers/1`](`createRenderbuffers/1`) returns `N` previously
unused renderbuffer object names in `Renderbuffers`, each representing a new
renderbuffer object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateRenderbuffers.xhtml)
""".
-spec createRenderbuffers(N::i()) -> [i()].
createRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5828),
  rec(5828).

-doc """
[`gl:createTextures/2`](`createTextures/2`) returns `N` previously unused
texture names in `Textures`, each representing a new texture object of the
dimensionality and type specified by `Target` and initialized to the default
values for that texture type.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateTextures.xhtml)
""".
-spec createTextures(Target::enum(), N::i()) -> [i()].
createTextures(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5829),
  rec(5829).

-doc """
[`gl:texBuffer/3`](`texBuffer/3`) and [`gl:textureBuffer/3`](`texBuffer/3`)
attaches the data store of a specified buffer object to a specified texture
object, and specify the storage format for the texture image found in the buffer
object. The texture object must be a buffer texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexBuffer.xhtml)
""".
-spec textureBuffer(Texture::i(), Internalformat::enum(), Buffer::i()) -> 'ok'.
textureBuffer(Texture,Internalformat,Buffer) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,5830),
  ok.

-doc """
[`gl:texBufferRange/5`](`texBufferRange/5`) and
[`gl:textureBufferRange/5`](`texBufferRange/5`) attach a range of the data store
of a specified buffer object to a specified texture object, and specify the
storage format for the texture image found in the buffer object. The texture
object must be a buffer texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexBufferRange.xhtml)
""".
-spec textureBufferRange(Texture::i(), Internalformat::enum(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
textureBufferRange(Texture,Internalformat,Buffer,Offset,Size) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,Offset,Size,5831),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage1D.xhtml)
""".
-spec compressedTextureSubImage1D(Texture, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage1D(Texture,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Width,Format,ImageSize,Data,5832),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage2D.xhtml)
""".
-spec compressedTextureSubImage2D(Texture, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage2D(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5834),
  ok.

-doc """
Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage3D.xhtml)
""".
-spec compressedTextureSubImage3D(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage3D(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5836),
  ok.

-doc """
[`gl:generateMipmap/1`](`generateMipmap/1`) and
[`gl:generateTextureMipmap/1`](`generateMipmap/1`) generates mipmaps for the
specified texture object. For [`gl:generateMipmap/1`](`generateMipmap/1`), the
texture object that is bound to `Target`. For
[`gl:generateTextureMipmap/1`](`generateMipmap/1`), `Texture` is the name of the
texture object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenerateMipmap.xhtml)
""".
-spec generateTextureMipmap(Texture::i()) -> 'ok'.
generateTextureMipmap(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5838),
  ok.

-doc """
[`gl:bindTextureUnit/2`](`bindTextureUnit/2`) binds an existing texture object
to the texture unit numbered `Unit`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTextureUnit.xhtml)
""".
-spec bindTextureUnit(Unit::i(), Texture::i()) -> 'ok'.
bindTextureUnit(Unit,Texture) when is_integer(Unit),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,5839),
  ok.

-doc """
[`gl:createVertexArrays/1`](`createVertexArrays/1`) returns `N` previously
unused vertex array object names in `Arrays`, each representing a new vertex
array object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateVertexArrays.xhtml)
""".
-spec createVertexArrays(N::i()) -> [i()].
createVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5840),
  rec(5840).

-doc(#{equiv => enableVertexAttribArray/1}).
-spec disableVertexArrayAttrib(Vaobj::i(), Index::i()) -> 'ok'.
disableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5841),
  ok.

-doc(#{equiv => enableVertexAttribArray/1}).
-spec enableVertexArrayAttrib(Vaobj::i(), Index::i()) -> 'ok'.
enableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5842),
  ok.

-doc """
[`gl:vertexArrayElementBuffer/2`](`vertexArrayElementBuffer/2`) binds a buffer
object with id `Buffer` to the element array buffer bind point of a vertex array
object with id `Vaobj`. If `Buffer` is zero, any existing element array buffer
binding to `Vaobj` is removed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexArrayElementBuffer.xhtml)
""".
-spec vertexArrayElementBuffer(Vaobj::i(), Buffer::i()) -> 'ok'.
vertexArrayElementBuffer(Vaobj,Buffer) when is_integer(Vaobj),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Buffer,5843),
  ok.

-doc """
[`gl:bindVertexBuffer/4`](`bindVertexBuffer/4`) and
[`gl:vertexArrayVertexBuffer/5`](`bindVertexBuffer/4`) bind the buffer named
`Buffer` to the vertex buffer binding point whose index is given by
`Bindingindex`. [`gl:bindVertexBuffer/4`](`bindVertexBuffer/4`) modifies the
binding of the currently bound vertex array object, whereas
[`gl:vertexArrayVertexBuffer/5`](`bindVertexBuffer/4`) allows the caller to
specify ID of the vertex array object with an argument named `Vaobj`, for which
the binding should be modified. `Offset` and `Stride` specify the offset of the
first element within the buffer and the distance between elements within the
buffer, respectively, and are both measured in basic machine units.
`Bindingindex` must be less than the value of `?GL_MAX_VERTEX_ATTRIB_BINDINGS`.
`Offset` and `Stride` must be greater than or equal to zero. If `Buffer` is
zero, then any buffer currently bound to the specified binding point is unbound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexBuffer.xhtml)
""".
-spec vertexArrayVertexBuffer(Vaobj::i(), Bindingindex::i(), Buffer::i(), Offset::i(), Stride::i()) -> 'ok'.
vertexArrayVertexBuffer(Vaobj,Bindingindex,Buffer,Offset,Stride) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Buffer,Offset,Stride,5844),
  ok.

-doc """
[`gl:bindVertexBuffers/4`](`bindVertexBuffers/4`) and
[`gl:vertexArrayVertexBuffers/5`](`bindVertexBuffers/4`) bind storage from an
array of existing buffer objects to a specified number of consecutive vertex
buffer binding points units in a vertex array object. For
[`gl:bindVertexBuffers/4`](`bindVertexBuffers/4`), the vertex array object is
the currently bound vertex array object. For
[`gl:vertexArrayVertexBuffers/5`](`bindVertexBuffers/4`), `Vaobj` is the name of
the vertex array object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexBuffers.xhtml)
""".
-spec vertexArrayVertexBuffers(Vaobj::i(), First::i(), Buffers::[i()], Offsets::[i()], Strides::[i()]) -> 'ok'.
vertexArrayVertexBuffers(Vaobj,First,Buffers,Offsets,Strides) when is_integer(Vaobj),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Vaobj,First,Count,Buffers,Offsets,Strides,5845),
  ok.

-doc(#{equiv => vertexAttribBinding/2}).
-spec vertexArrayAttribBinding(Vaobj::i(), Attribindex::i(), Bindingindex::i()) -> 'ok'.
vertexArrayAttribBinding(Vaobj,Attribindex,Bindingindex) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Bindingindex,5846),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexArrayAttribFormat(Vaobj, Attribindex, Size, Type, Normalized, Relativeoffset) -> 'ok'
    when Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Normalized::0|1, Relativeoffset::i().
vertexArrayAttribFormat(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset,5847),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexArrayAttribIFormat(Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexArrayAttribIFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5848),
  ok.

-doc(#{equiv => vertexAttribLPointer/5}).
-spec vertexArrayAttribLFormat(Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexArrayAttribLFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5849),
  ok.

-doc(#{equiv => vertexBindingDivisor/2}).
-spec vertexArrayBindingDivisor(Vaobj::i(), Bindingindex::i(), Divisor::i()) -> 'ok'.
vertexArrayBindingDivisor(Vaobj,Bindingindex,Divisor) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Divisor,5850),
  ok.

-doc """
[`gl:createSamplers/1`](`createSamplers/1`) returns `N` previously unused
sampler names in `Samplers`, each representing a new sampler object initialized
to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateSamplers.xhtml)
""".
-spec createSamplers(N::i()) -> [i()].
createSamplers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5851),
  rec(5851).

-doc """
[`gl:createProgramPipelines/1`](`createProgramPipelines/1`) returns `N`
previously unused program pipeline names in `Pipelines`, each representing a new
program pipeline object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateProgramPipelines.xhtml)
""".
-spec createProgramPipelines(N::i()) -> [i()].
createProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5852),
  rec(5852).

-doc """
[`gl:createQueries/2`](`createQueries/2`) returns `N` previously unused query
object names in `Ids`, each representing a new query object with the specified
`Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateQueries.xhtml)
""".
-spec createQueries(Target::enum(), N::i()) -> [i()].
createQueries(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5853),
  rec(5853).

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryBufferObjecti64v(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjecti64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5854),
  ok.

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryBufferObjectiv(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5855),
  ok.

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryBufferObjectui64v(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectui64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5856),
  ok.

-doc(#{equiv => getQueryObjectuiv/2}).
-spec getQueryBufferObjectuiv(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectuiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5857),
  ok.

-doc """
[`gl:memoryBarrier/1`](`memoryBarrier/1`) defines a barrier ordering the memory
transactions issued prior to the command relative to those issued after the
barrier. For the purposes of this ordering, memory transactions performed by
shaders are considered to be issued by the rendering command that triggered the
execution of the shader. `Barriers` is a bitfield indicating the set of
operations that are synchronized with shader stores; the bits used in `Barriers`
are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMemoryBarrier.xhtml)
""".
-spec memoryBarrierByRegion(Barriers::i()) -> 'ok'.
memoryBarrierByRegion(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5858),
  ok.

-doc """
Certain events can result in a reset of the GL context. Such a reset causes all
context state to be lost and requires the application to recreate all objects in
the affected context.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetGraphicsResetStatus.xhtml)
""".
-spec getGraphicsResetStatus() -> enum().
getGraphicsResetStatus()  ->
  IF = get_interface(),
  IF:queue_cmd(5859),
  rec(5859).

-doc """
The values of rendered fragments are undefined when a shader stage fetches
texels and the same texels are written via fragment shader outputs, even if the
reads and writes are not in the same drawing command. To safely read the result
of a written texel via a texel fetch in a subsequent drawing command, call
[`gl:textureBarrier/0`](`textureBarrier/0`) between the two drawing commands to
guarantee that writes have completed and caches have been invalidated before
subsequent drawing commands are executed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTextureBarrier.xhtml)
""".
-spec textureBarrier() -> 'ok'.
textureBarrier()  ->
  IF = get_interface(),
  IF:queue_cmd(5860),
  ok.

-doc "No documentation available.".
-spec multiDrawArraysIndirectCount(Mode, Indirect, Drawcount, Maxdrawcount, Stride) -> 'ok'
    when Mode::enum(), Indirect::offset()|mem(), Drawcount::i(), Maxdrawcount::i(), Stride::i().
multiDrawArraysIndirectCount(Mode,Indirect,Drawcount,Maxdrawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Maxdrawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Maxdrawcount,Stride,5861),
  ok.

-doc "No documentation available.".
-spec polygonOffsetClamp(Factor::f(), Units::f(), Clamp::f()) -> 'ok'.
polygonOffsetClamp(Factor,Units,Clamp) when is_float(Factor),is_float(Units),is_float(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,Clamp,5863),
  ok.

-doc false.
-spec primitiveBoundingBoxARB(MinX, MinY, MinZ, MinW, MaxX, MaxY, MaxZ, MaxW) -> 'ok'
    when MinX::f(), MinY::f(), MinZ::f(), MinW::f(), MaxX::f(), MaxY::f(), MaxZ::f(), MaxW::f().
primitiveBoundingBoxARB(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW) when is_float(MinX),is_float(MinY),is_float(MinZ),is_float(MinW),is_float(MaxX),is_float(MaxY),is_float(MaxZ),is_float(MaxW) ->
  IF = get_interface(),
  IF:queue_cmd(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW,5864),
  ok.

-doc false.
-spec makeTextureHandleResidentARB(Handle::i()) -> 'ok'.
makeTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5865),
  ok.

-doc false.
-spec makeTextureHandleNonResidentARB(Handle::i()) -> 'ok'.
makeTextureHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5866),
  ok.

-doc false.
-spec getImageHandleARB(Texture::i(), Level::i(), Layered::0|1, Layer::i(), Format::enum()) -> i().
getImageHandleARB(Texture,Level,Layered,Layer,Format) when is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Layered,Layer,Format,5867),
  rec(5867).

-doc false.
-spec makeImageHandleResidentARB(Handle::i(), Access::enum()) -> 'ok'.
makeImageHandleResidentARB(Handle,Access) when is_integer(Handle),is_integer(Access) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,Access,5868),
  ok.

-doc false.
-spec makeImageHandleNonResidentARB(Handle::i()) -> 'ok'.
makeImageHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5869),
  ok.

-doc false.
-spec uniformHandleui64ARB(Location::i(), Value::i()) -> 'ok'.
uniformHandleui64ARB(Location,Value) when is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Location,Value,5870),
  ok.

-doc false.
-spec programUniformHandleui64ARB(Program::i(), Location::i(), Value::i()) -> 'ok'.
programUniformHandleui64ARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,Value,5871),
  ok.

-doc false.
-spec isTextureHandleResidentARB(Handle::i()) -> 0|1.
isTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5872),
  rec(5872).

-doc false.
-spec isImageHandleResidentARB(Handle::i()) -> 0|1.
isImageHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5873),
  rec(5873).

-doc false.
-spec dispatchComputeGroupSizeARB(Num_groups_x, Num_groups_y, Num_groups_z, Group_size_x, Group_size_y, Group_size_z) -> 'ok'
    when Num_groups_x::i(), Num_groups_y::i(), Num_groups_z::i(), Group_size_x::i(), Group_size_y::i(), Group_size_z::i().
dispatchComputeGroupSizeARB(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z),is_integer(Group_size_x),is_integer(Group_size_y),is_integer(Group_size_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z,5874),
  ok.

-doc false.
-spec programStringARB(Target::enum(), Format::enum(), String::string()) -> 'ok'.
programStringARB(Target,Format,String) when is_integer(Target),is_integer(Format),is_list(String) ->
  IF = get_interface(),
  StringBin = unicode:characters_to_binary([String|[0]]),
  IF:queue_cmd(Target,Format,StringBin,5875),
  ok.

-doc false.
-spec bindProgramARB(Target::enum(), Program::i()) -> 'ok'.
bindProgramARB(Target,Program) when is_integer(Target),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Program,5876),
  ok.

-doc false.
-spec deleteProgramsARB(Programs::[i()]) -> 'ok'.
deleteProgramsARB(Programs) when is_list(Programs) ->
  IF = get_interface(),
  N = length(Programs),
  IF:queue_cmd(N,Programs,5877),
  ok.

-doc false.
-spec genProgramsARB(N::i()) -> [i()].
genProgramsARB(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5878),
  rec(5878).

-doc false.
-spec programEnvParameter4dARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programEnvParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5879),
  ok.

-doc false.
-spec programEnvParameter4dvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programEnvParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5880),
  ok.

-doc false.
-spec programEnvParameter4fARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programEnvParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5881),
  ok.

-doc false.
-spec programEnvParameter4fvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programEnvParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5882),
  ok.

-doc false.
-spec programLocalParameter4dARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programLocalParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5883),
  ok.

-doc false.
-spec programLocalParameter4dvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programLocalParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5884),
  ok.

-doc false.
-spec programLocalParameter4fARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programLocalParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5885),
  ok.

-doc false.
-spec programLocalParameter4fvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programLocalParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5886),
  ok.

-doc false.
-spec getProgramEnvParameterdvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramEnvParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5887),
  rec(5887).

-doc false.
-spec getProgramEnvParameterfvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramEnvParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5888),
  rec(5888).

-doc false.
-spec getProgramLocalParameterdvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramLocalParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5889),
  rec(5889).

-doc false.
-spec getProgramLocalParameterfvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramLocalParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5890),
  rec(5890).

-doc false.
-spec getProgramStringARB(Target::enum(), Pname::enum(), String::mem()) -> 'ok'.
getProgramStringARB(Target,Pname,String) when is_integer(Target),is_integer(Pname),is_tuple(String) orelse is_binary(String) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,String,5891),
  rec(5891).

-doc(#{equiv => framebufferTextureLayer/5}).
-spec framebufferTextureFaceARB(Target::enum(), Attachment::enum(), Texture::i(), Level::i(), Face::enum()) -> 'ok'.
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Face) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Face,5892),
  ok.

-doc false.
-spec uniform1i64ARB(Location::i(), X::i()) -> 'ok'.
uniform1i64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5893),
  ok.

-doc false.
-spec uniform2i64ARB(Location::i(), X::i(), Y::i()) -> 'ok'.
uniform2i64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5894),
  ok.

-doc false.
-spec uniform3i64ARB(Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
uniform3i64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5895),
  ok.

-doc false.
-spec uniform4i64ARB(Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
uniform4i64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5896),
  ok.

-doc false.
-spec uniform1i64vARB(Location::i(), Value::[i()]) -> 'ok'.
uniform1i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5897),
  ok.

-doc false.
-spec uniform2i64vARB(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5898),
  ok.

-doc false.
-spec uniform3i64vARB(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5899),
  ok.

-doc false.
-spec uniform4i64vARB(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5900),
  ok.

-doc false.
-spec uniform1ui64ARB(Location::i(), X::i()) -> 'ok'.
uniform1ui64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5901),
  ok.

-doc false.
-spec uniform2ui64ARB(Location::i(), X::i(), Y::i()) -> 'ok'.
uniform2ui64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5902),
  ok.

-doc false.
-spec uniform3ui64ARB(Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
uniform3ui64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5903),
  ok.

-doc false.
-spec uniform4ui64ARB(Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
uniform4ui64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5904),
  ok.

-doc false.
-spec uniform1ui64vARB(Location::i(), Value::[i()]) -> 'ok'.
uniform1ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5905),
  ok.

-doc false.
-spec uniform2ui64vARB(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5906),
  ok.

-doc false.
-spec uniform3ui64vARB(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5907),
  ok.

-doc false.
-spec uniform4ui64vARB(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5908),
  ok.

-doc false.
-spec getUniformi64vARB(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformi64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5909),
  rec(5909).

-doc false.
-spec getUniformui64vARB(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformui64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5910),
  rec(5910).

-doc false.
-spec programUniform1i64ARB(Program::i(), Location::i(), X::i()) -> 'ok'.
programUniform1i64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5911),
  ok.

-doc false.
-spec programUniform2i64ARB(Program::i(), Location::i(), X::i(), Y::i()) -> 'ok'.
programUniform2i64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5912),
  ok.

-doc false.
-spec programUniform3i64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
programUniform3i64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5913),
  ok.

-doc false.
-spec programUniform4i64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
programUniform4i64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5914),
  ok.

-doc false.
-spec programUniform1i64vARB(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5915),
  ok.

-doc false.
-spec programUniform2i64vARB(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5916),
  ok.

-doc false.
-spec programUniform3i64vARB(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5917),
  ok.

-doc false.
-spec programUniform4i64vARB(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5918),
  ok.

-doc false.
-spec programUniform1ui64ARB(Program::i(), Location::i(), X::i()) -> 'ok'.
programUniform1ui64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5919),
  ok.

-doc false.
-spec programUniform2ui64ARB(Program::i(), Location::i(), X::i(), Y::i()) -> 'ok'.
programUniform2ui64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5920),
  ok.

-doc false.
-spec programUniform3ui64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
programUniform3ui64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5921),
  ok.

-doc false.
-spec programUniform4ui64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
programUniform4ui64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5922),
  ok.

-doc false.
-spec programUniform1ui64vARB(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5923),
  ok.

-doc false.
-spec programUniform2ui64vARB(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5924),
  ok.

-doc false.
-spec programUniform3ui64vARB(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5925),
  ok.

-doc false.
-spec programUniform4ui64vARB(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5926),
  ok.

-doc """
[`gl:colorTable/6`](`colorTable/6`) may be used in two ways: to test the actual
size and color resolution of a lookup table given a particular set of
parameters, or to load the contents of a color lookup table. Use the targets
`?GL_PROXY_*` for the first case and the other targets for the second case.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTable.xml)
""".
-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Format::enum(), Type::enum(), Table::offset()|mem().
colorTable(Target,Internalformat,Width,Format,Type,Table) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Table) orelse is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Table,5927),
  ok.

-doc(#{equiv => colorTableParameteriv/3}).
-spec colorTableParameterfv(Target::enum(), Pname::enum(), Params::{f(),f(),f(),f()}) -> 'ok'.
colorTableParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5929),
  ok.

-doc """
[`gl:colorTableParameter()`](`colorTableParameterfv/3`) is used to specify the
scale factors and bias terms applied to color components when they are loaded
into a color table. `Target` indicates which color table the scale and bias
terms apply to; it must be set to `?GL_COLOR_TABLE`,
`?GL_POST_CONVOLUTION_COLOR_TABLE`, or `?GL_POST_COLOR_MATRIX_COLOR_TABLE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTableParameter.xml)
""".
-spec colorTableParameteriv(Target::enum(), Pname::enum(), Params::{i(),i(),i(),i()}) -> 'ok'.
colorTableParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5930),
  ok.

-doc """
[`gl:copyColorTable/5`](`copyColorTable/5`) loads a color table with pixels from
the current `?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:colorTable/6`](`colorTable/6`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorTable.xml)
""".
-spec copyColorTable(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyColorTable(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5931),
  ok.

-doc """
[`gl:getColorTable/4`](`getColorTable/4`) returns in `Table` the contents of the
color table specified by `Target`. No pixel transfer operations are performed,
but pixel storage modes that are applicable to
[`gl:readPixels/7`](`readPixels/7`) are performed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTable.xml)
""".
-spec getColorTable(Target::enum(), Format::enum(), Type::enum(), Table::mem()) -> 'ok'.
getColorTable(Target,Format,Type,Table) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Table,5932),
  rec(5932).

-doc(#{equiv => getColorTableParameteriv/2}).
-spec getColorTableParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getColorTableParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5933),
  rec(5933).

-doc """
Returns parameters specific to color table `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTableParameter.xml)
""".
-spec getColorTableParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getColorTableParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5934),
  rec(5934).

-doc """
[`gl:colorSubTable/6`](`colorSubTable/6`) is used to respecify a contiguous
portion of a color table previously defined using
[`gl:colorTable/6`](`colorTable/6`). The pixels referenced by `Data` replace the
portion of the existing table from indices `Start` to start+count-1, inclusive.
This region may not include any entries outside the range of the color table as
it was originally specified. It is not an error to specify a subtexture with
width of 0, but such a specification has no effect.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorSubTable.xml)
""".
-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> 'ok'
    when Target::enum(), Start::i(), Count::i(), Format::enum(), Type::enum(), Data::offset()|mem().
colorSubTable(Target,Start,Count,Format,Type,Data) when is_integer(Target),is_integer(Start),is_integer(Count),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,Count,Format,Type,Data,5935),
  ok.

-doc """
[`gl:copyColorSubTable/5`](`copyColorSubTable/5`) is used to respecify a
contiguous portion of a color table previously defined using
[`gl:colorTable/6`](`colorTable/6`). The pixels copied from the framebuffer
replace the portion of the existing table from indices `Start` to start+x-1,
inclusive. This region may not include any entries outside the range of the
color table, as was originally specified. It is not an error to specify a
subtexture with width of 0, but such a specification has no effect.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorSubTable.xml)
""".
-spec copyColorSubTable(Target::enum(), Start::i(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyColorSubTable(Target,Start,X,Y,Width) when is_integer(Target),is_integer(Start),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,X,Y,Width,5937),
  ok.

-doc """
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`) builds a one-dimensional
convolution filter kernel from an array of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter1D.xml)
""".
-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Format::enum(), Type::enum(), Image::offset()|mem().
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Image,5938),
  ok.

-doc """
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`) builds a two-dimensional
convolution filter kernel from an array of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter2D.xml)
""".
-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Height::i(), Format::enum(), Type::enum(), Image::offset()|mem().
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Image,5940),
  ok.

-doc(#{equiv => convolutionParameteriv/3}).
-spec convolutionParameterf(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameterf(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5942),
  ok.

-doc(#{equiv => convolutionParameteriv/3}).
-spec convolutionParameterfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5943),
  ok.

-doc(#{equiv => convolutionParameteriv/3}).
-spec convolutionParameteri(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameteri(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5944),
  ok.

-doc """
[`gl:convolutionParameter()`](`convolutionParameterf/3`) sets the value of a
convolution parameter.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionParameter.xml)
""".
-spec convolutionParameteriv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5945),
  ok.

-doc """
[`gl:copyConvolutionFilter1D/5`](`copyConvolutionFilter1D/5`) defines a
one-dimensional convolution filter kernel with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter1D.xml)
""".
-spec copyConvolutionFilter1D(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5946),
  ok.

-doc """
[`gl:copyConvolutionFilter2D/6`](`copyConvolutionFilter2D/6`) defines a
two-dimensional convolution filter kernel with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter2D.xml)
""".
-spec copyConvolutionFilter2D(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,Height,5947),
  ok.

-doc """
[`gl:getConvolutionFilter/4`](`getConvolutionFilter/4`) returns the current 1D
or 2D convolution filter kernel as an image. The one- or two-dimensional image
is placed in `Image` according to the specifications in `Format` and `Type`. No
pixel transfer operations are performed on this image, but the relevant pixel
storage modes are applied.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionFilter.xml)
""".
-spec getConvolutionFilter(Target::enum(), Format::enum(), Type::enum(), Image::mem()) -> 'ok'.
getConvolutionFilter(Target,Format,Type,Image) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Image,5948),
  rec(5948).

-doc(#{equiv => getConvolutionParameteriv/2}).
-spec getConvolutionParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getConvolutionParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5949),
  rec(5949).

-doc """
[`gl:getConvolutionParameter()`](`getConvolutionParameterfv/2`) retrieves
convolution parameters. `Target` determines which convolution filter is queried.
`Pname` determines which parameter is returned:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionParameter.xml)
""".
-spec getConvolutionParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getConvolutionParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5950),
  rec(5950).

-doc """
[`gl:separableFilter2D/8`](`separableFilter2D/8`) builds a two-dimensional
separable convolution filter kernel from two arrays of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSeparableFilter2D.xml)
""".
-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Height::i(), Format::enum(), Type::enum(), Row::offset()|mem(), Column::offset()|mem().
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Row) orelse is_tuple(Row) orelse is_binary(Row),is_integer(Column) orelse is_tuple(Column) orelse is_binary(Column) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Row,Column,5951),
  ok.

-doc """
[`gl:getHistogram/5`](`getHistogram/5`) returns the current histogram table as a
one-dimensional image with the same width as the histogram. No pixel transfer
operations are performed on this image, but pixel storage modes that are
applicable to 1D images are honored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogram.xml)
""".
-spec getHistogram(Target::enum(), Reset::0|1, Format::enum(), Type::enum(), Values::mem()) -> 'ok'.
getHistogram(Target,Reset,Format,Type,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Type),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Type,Values,5953),
  rec(5953).

-doc(#{equiv => getHistogramParameteriv/2}).
-spec getHistogramParameterfv(Target::enum(), Pname::enum()) -> {f()}.
getHistogramParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5954),
  rec(5954).

-doc """
[`gl:getHistogramParameter()`](`getHistogramParameterfv/2`) is used to query
parameter values for the current histogram or for a proxy. The histogram state
information may be queried by calling
[`gl:getHistogramParameter()`](`getHistogramParameterfv/2`) with a `Target` of
`?GL_HISTOGRAM` (to obtain information for the current histogram table) or
`?GL_PROXY_HISTOGRAM` (to obtain information from the most recent proxy request)
and one of the following values for the `Pname` argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogramParameter.xml)
""".
-spec getHistogramParameteriv(Target::enum(), Pname::enum()) -> {i()}.
getHistogramParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5955),
  rec(5955).

-doc """
[`gl:getMinmax/5`](`getMinmax/5`) returns the accumulated minimum and maximum
pixel values (computed on a per-component basis) in a one-dimensional image of
width 2. The first set of return values are the minima, and the second set of
return values are the maxima. The format of the return values is determined by
`Format`, and their type is determined by `Types`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmax.xml)
""".
-spec getMinmax(Target::enum(), Reset::0|1, Format::enum(), Types::enum(), Values::mem()) -> 'ok'.
getMinmax(Target,Reset,Format,Types,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Types),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Types,Values,5956),
  rec(5956).

-doc(#{equiv => getMinmaxParameteriv/2}).
-spec getMinmaxParameterfv(Target::enum(), Pname::enum()) -> {f()}.
getMinmaxParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5957),
  rec(5957).

-doc """
[`gl:getMinmaxParameter()`](`getMinmaxParameterfv/2`) retrieves parameters for
the current minmax table by setting `Pname` to one of the following values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmaxParameter.xml)
""".
-spec getMinmaxParameteriv(Target::enum(), Pname::enum()) -> {i()}.
getMinmaxParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5958),
  rec(5958).

-doc """
When `?GL_HISTOGRAM` is enabled, RGBA color components are converted to
histogram table indices by clamping to the range \[0,1], multiplying by the
width of the histogram table, and rounding to the nearest integer. The table
entries selected by the RGBA indices are then incremented. (If the internal
format of the histogram table includes luminance, then the index derived from
the R color component determines the luminance table entry to be incremented.)
If a histogram table entry is incremented beyond its maximum value, then its
value becomes undefined. (This is not an error.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glHistogram.xml)
""".
-spec histogram(Target::enum(), Width::i(), Internalformat::enum(), Sink::0|1) -> 'ok'.
histogram(Target,Width,Internalformat,Sink) when is_integer(Target),is_integer(Width),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Width,Internalformat,Sink,5959),
  ok.

-doc """
When `?GL_MINMAX` is enabled, the RGBA components of incoming pixels are
compared to the minimum and maximum values for each component, which are stored
in the two-element minmax table. (The first element stores the minima, and the
second element stores the maxima.) If a pixel component is greater than the
corresponding component in the maximum element, then the maximum element is
updated with the pixel component value. If a pixel component is less than the
corresponding component in the minimum element, then the minimum element is
updated with the pixel component value. (In both cases, if the internal format
of the minmax table includes luminance, then the R color component of incoming
pixels is used for comparison.) The contents of the minmax table may be
retrieved at a later time by calling [`gl:getMinmax/5`](`getMinmax/5`). The
minmax operation is enabled or disabled by calling [`gl:enable/1`](`enable/1`)
or [`gl:disable/1`](`enable/1`), respectively, with an argument of `?GL_MINMAX`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMinmax.xml)
""".
-spec minmax(Target::enum(), Internalformat::enum(), Sink::0|1) -> 'ok'.
minmax(Target,Internalformat,Sink) when is_integer(Target),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Sink,5960),
  ok.

-doc """
[`gl:resetHistogram/1`](`resetHistogram/1`) resets all the elements of the
current histogram table to zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetHistogram.xml)
""".
-spec resetHistogram(Target::enum()) -> 'ok'.
resetHistogram(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5961),
  ok.

-doc """
[`gl:resetMinmax/1`](`resetMinmax/1`) resets the elements of the current minmax
table to their initial values: the \`\`maximum'' element receives the minimum
possible component values, and the \`\`minimum'' element receives the maximum
possible component values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetMinmax.xml)
""".
-spec resetMinmax(Target::enum()) -> 'ok'.
resetMinmax(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5962),
  ok.

-doc false.
-spec currentPaletteMatrixARB(Index::i()) -> 'ok'.
currentPaletteMatrixARB(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5963),
  ok.

-doc false.
-spec matrixIndexubvARB(Indices::[i()]) -> 'ok'.
matrixIndexubvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5964),
  ok.

-doc false.
-spec matrixIndexusvARB(Indices::[i()]) -> 'ok'.
matrixIndexusvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5965),
  ok.

-doc false.
-spec matrixIndexuivARB(Indices::[i()]) -> 'ok'.
matrixIndexuivARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5966),
  ok.

-doc false.
-spec sampleCoverageARB(Value::f(), Invert::0|1) -> 'ok'.
sampleCoverageARB(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5967),
  ok.

-doc false.
-spec maxShaderCompilerThreadsARB(Count::i()) -> 'ok'.
maxShaderCompilerThreadsARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5968),
  ok.

-doc false.
-spec evaluateDepthValuesARB() -> 'ok'.
evaluateDepthValuesARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5969),
  ok.

-doc false.
-spec deleteObjectARB(Obj::i()) -> 'ok'.
deleteObjectARB(Obj) when is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,5970),
  ok.

-doc false.
-spec getHandleARB(Pname::enum()) -> i().
getHandleARB(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5971),
  rec(5971).

-doc false.
-spec detachObjectARB(ContainerObj::i(), AttachedObj::i()) -> 'ok'.
detachObjectARB(ContainerObj,AttachedObj) when is_integer(ContainerObj),is_integer(AttachedObj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,AttachedObj,5972),
  ok.

-doc false.
-spec createShaderObjectARB(ShaderType::enum()) -> i().
createShaderObjectARB(ShaderType) when is_integer(ShaderType) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderType,5973),
  rec(5973).

-doc false.
-spec shaderSourceARB(ShaderObj::i(), String::[unicode:chardata()]) -> 'ok'.
shaderSourceARB(ShaderObj,String) when is_integer(ShaderObj),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(ShaderObj,Count,StringTemp,5974),
  ok.

-doc false.
-spec compileShaderARB(ShaderObj::i()) -> 'ok'.
compileShaderARB(ShaderObj) when is_integer(ShaderObj) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderObj,5975),
  ok.

-doc false.
-spec createProgramObjectARB() -> i().
createProgramObjectARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5976),
  rec(5976).

-doc false.
-spec attachObjectARB(ContainerObj::i(), Obj::i()) -> 'ok'.
attachObjectARB(ContainerObj,Obj) when is_integer(ContainerObj),is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,Obj,5977),
  ok.

-doc false.
-spec linkProgramARB(ProgramObj::i()) -> 'ok'.
linkProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5978),
  ok.

-doc false.
-spec useProgramObjectARB(ProgramObj::i()) -> 'ok'.
useProgramObjectARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5979),
  ok.

-doc false.
-spec validateProgramARB(ProgramObj::i()) -> 'ok'.
validateProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5980),
  ok.

-doc false.
-spec getObjectParameterfvARB(Obj::i(), Pname::enum()) -> f().
getObjectParameterfvARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5981),
  rec(5981).

-doc false.
-spec getObjectParameterivARB(Obj::i(), Pname::enum()) -> i().
getObjectParameterivARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5982),
  rec(5982).

-doc false.
-spec getInfoLogARB(Obj::i(), MaxLength::i()) -> string().
getInfoLogARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5983),
  rec(5983).

-doc false.
-spec getAttachedObjectsARB(ContainerObj::i(), MaxCount::i()) -> [i()].
getAttachedObjectsARB(ContainerObj,MaxCount) when is_integer(ContainerObj),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,MaxCount,5984),
  rec(5984).

-doc false.
-spec getUniformLocationARB(ProgramObj::i(), Name::string()) -> i().
getUniformLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,5985),
  rec(5985).

-doc false.
-spec getActiveUniformARB(ProgramObj::i(), Index::i(), MaxLength::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveUniformARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,5986),
  rec(5986).

-doc false.
-spec getUniformfvARB(ProgramObj::i(), Location::i()) -> matrix().
getUniformfvARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5987),
  rec(5987).

-doc false.
-spec getUniformivARB(ProgramObj::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformivARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5988),
  rec(5988).

-doc false.
-spec getShaderSourceARB(Obj::i(), MaxLength::i()) -> string().
getShaderSourceARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5989),
  rec(5989).

-doc false.
-spec deleteNamedStringARB(Name::string()) -> 'ok'.
deleteNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5990),
  ok.

-doc false.
-spec compileShaderIncludeARB(Shader::i(), Path::[unicode:chardata()]) -> 'ok'.
compileShaderIncludeARB(Shader,Path) when is_integer(Shader),is_list(Path) ->
  IF = get_interface(),
  PathTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Path ],
  Count = length(Path),
  IF:queue_cmd(Shader,Count,PathTemp,5991),
  ok.

-doc false.
-spec isNamedStringARB(Name::string()) -> 0|1.
isNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5992),
  rec(5992).

-doc false.
-spec bufferPageCommitmentARB(Target::enum(), Offset::i(), Size::i(), Commit::0|1) -> 'ok'.
bufferPageCommitmentARB(Target,Offset,Size,Commit) when is_integer(Target),is_integer(Offset),is_integer(Size),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Commit,5993),
  ok.

-doc false.
-spec texPageCommitmentARB(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Commit) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Commit::0|1.
texPageCommitmentARB(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit,5994),
  ok.

-doc false.
-spec getCompressedTexImageARB(Target::enum(), Level::i(), Img::mem()) -> 'ok'.
getCompressedTexImageARB(Target,Level,Img) when is_integer(Target),is_integer(Level),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Img,5995),
  rec(5995).

-doc false.
-spec loadTransposeMatrixfARB(M::matrix()) -> 'ok'.
loadTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5996),
  ok.

-doc false.
-spec loadTransposeMatrixdARB(M::matrix()) -> 'ok'.
loadTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5997),
  ok.

-doc false.
-spec multTransposeMatrixfARB(M::matrix()) -> 'ok'.
multTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5998),
  ok.

-doc false.
-spec multTransposeMatrixdARB(M::matrix()) -> 'ok'.
multTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5999),
  ok.

-doc false.
-spec weightbvARB(Weights::[i()]) -> 'ok'.
weightbvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6000),
  ok.

-doc false.
-spec weightsvARB(Weights::[i()]) -> 'ok'.
weightsvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6001),
  ok.

-doc false.
-spec weightivARB(Weights::[i()]) -> 'ok'.
weightivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6002),
  ok.

-doc false.
-spec weightfvARB(Weights::[f()]) -> 'ok'.
weightfvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6003),
  ok.

-doc false.
-spec weightdvARB(Weights::[f()]) -> 'ok'.
weightdvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6004),
  ok.

-doc false.
-spec weightubvARB(Weights::[i()]) -> 'ok'.
weightubvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6005),
  ok.

-doc false.
-spec weightusvARB(Weights::[i()]) -> 'ok'.
weightusvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6006),
  ok.

-doc false.
-spec weightuivARB(Weights::[i()]) -> 'ok'.
weightuivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6007),
  ok.

-doc false.
-spec vertexBlendARB(Count::i()) -> 'ok'.
vertexBlendARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6008),
  ok.

-doc """
These functions return in `Data` a selected parameter of the specified buffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetBufferParameter.xhtml)
""".
-spec getBufferParameterivARB(Target::enum(), Pname::enum()) -> [i()].
getBufferParameterivARB(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,6009),
  rec(6009).

-doc false.
-spec bindAttribLocationARB(ProgramObj::i(), Index::i(), Name::string()) -> 'ok'.
bindAttribLocationARB(ProgramObj,Index,Name) when is_integer(ProgramObj),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,Index,NameBin,6010),
  ok.

-doc false.
-spec getActiveAttribARB(ProgramObj::i(), Index::i(), MaxLength::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveAttribARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,6011),
  rec(6011).

-doc false.
-spec getAttribLocationARB(ProgramObj::i(), Name::string()) -> i().
getAttribLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,6012),
  rec(6012).

-doc false.
-spec blendBarrierKHR() -> 'ok'.
blendBarrierKHR()  ->
  IF = get_interface(),
  IF:queue_cmd(6013),
  ok.

-doc false.
-spec maxShaderCompilerThreadsKHR(Count::i()) -> 'ok'.
maxShaderCompilerThreadsKHR(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6014),
  ok.

-doc false.
-spec depthBoundsEXT(Zmin::clamp(), Zmax::clamp()) -> 'ok'.
depthBoundsEXT(Zmin,Zmax) when is_float(Zmin),is_float(Zmax) ->
  IF = get_interface(),
  IF:queue_cmd(Zmin,Zmax,6015),
  ok.

