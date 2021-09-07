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

%% OPENGL API

%% This file is generated DO NOT EDIT

%% @doc  Standard OpenGL api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(gl).

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
-on_load(init_nif/0).

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
  debugMessageInsert/6,getDebugMessageLog/2,pushDebugGroup/4,popDebugGroup/0,
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

-export([get_interface/0, rec/1, lookup_func/0]).
-define(nif_stub,nif_stub_error(?LINE)).
%% @hidden
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% @hidden
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
get_interface() ->
    wxe_util.  %% temporary

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

lookup_func() -> ?nif_stub.




%% API

-spec clearIndex(C::f()) -> 'ok'.
clearIndex(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5037),
  ok.

-spec clearColor(Red::clamp(), Green::clamp(), Blue::clamp(), Alpha::clamp()) -> 'ok'.
clearColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5038),
  ok.

-spec clear(Mask::i()) -> 'ok'.
clear(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5039),
  ok.

-spec indexMask(Mask::i()) -> 'ok'.
indexMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5040),
  ok.

-spec colorMask(Red::0|1, Green::0|1, Blue::0|1, Alpha::0|1) -> 'ok'.
colorMask(Red,Green,Blue,Alpha) when (0 =:= Red) orelse (1 =:= Red),(0 =:= Green) orelse (1 =:= Green),(0 =:= Blue) orelse (1 =:= Blue),(0 =:= Alpha) orelse (1 =:= Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5041),
  ok.

-spec alphaFunc(Func::enum(), Ref::clamp()) -> 'ok'.
alphaFunc(Func,Ref) when is_integer(Func),is_float(Ref) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,5042),
  ok.

-spec blendFunc(Sfactor::enum(), Dfactor::enum()) -> 'ok'.
blendFunc(Sfactor,Dfactor) when is_integer(Sfactor),is_integer(Dfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Sfactor,Dfactor,5043),
  ok.

-spec logicOp(Opcode::enum()) -> 'ok'.
logicOp(Opcode) when is_integer(Opcode) ->
  IF = get_interface(),
  IF:queue_cmd(Opcode,5044),
  ok.

-spec cullFace(Mode::enum()) -> 'ok'.
cullFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5045),
  ok.

-spec frontFace(Mode::enum()) -> 'ok'.
frontFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5046),
  ok.

-spec pointSize(Size::f()) -> 'ok'.
pointSize(Size) when is_float(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Size,5047),
  ok.

-spec lineWidth(Width::f()) -> 'ok'.
lineWidth(Width) when is_float(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Width,5048),
  ok.

-spec lineStipple(Factor::i(), Pattern::i()) -> 'ok'.
lineStipple(Factor,Pattern) when is_integer(Factor),is_integer(Pattern) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Pattern,5049),
  ok.

-spec polygonMode(Face::enum(), Mode::enum()) -> 'ok'.
polygonMode(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5050),
  ok.

-spec polygonOffset(Factor::f(), Units::f()) -> 'ok'.
polygonOffset(Factor,Units) when is_float(Factor),is_float(Units) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,5051),
  ok.

-spec polygonStipple(Mask::binary()) -> 'ok'.
polygonStipple(Mask) when is_binary(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5052),
  ok.

-spec getPolygonStipple() -> binary().
getPolygonStipple()  ->
  IF = get_interface(),
  IF:queue_cmd(5053),
  rec(5053).

-spec edgeFlag(Flag::0|1) -> 'ok'.
edgeFlag(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5054),
  ok.

-spec edgeFlagv({Flag::0|1}) -> 'ok'.
edgeFlagv({Flag}) ->  edgeFlag(Flag).
-spec scissor(X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
scissor(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5055),
  ok.

-spec clipPlane(Plane::enum(), Equation::{f(),f(),f(),f()}) -> 'ok'.
clipPlane(Plane,Equation) when is_integer(Plane),tuple_size(Equation) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Plane,Equation,5056),
  ok.

-spec getClipPlane(Plane::enum()) -> {f(),f(),f(),f()}.
getClipPlane(Plane) when is_integer(Plane) ->
  IF = get_interface(),
  IF:queue_cmd(Plane,5057),
  rec(5057).

-spec drawBuffer(Mode::enum()) -> 'ok'.
drawBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5058),
  ok.

-spec readBuffer(Mode::enum()) -> 'ok'.
readBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5059),
  ok.

-spec enable(Cap::enum()) -> 'ok'.
enable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5060),
  ok.

-spec disable(Cap::enum()) -> 'ok'.
disable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5061),
  ok.

-spec isEnabled(Cap::enum()) -> 0|1.
isEnabled(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5062),
  rec(5062).

-spec enableClientState(Cap::enum()) -> 'ok'.
enableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5063),
  ok.

-spec disableClientState(Cap::enum()) -> 'ok'.
disableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5064),
  ok.

-spec getBooleanv(Pname::enum()) -> [0|1].
getBooleanv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5065),
  rec(5065).

-spec getDoublev(Pname::enum()) -> [f()].
getDoublev(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5066),
  rec(5066).

-spec getFloatv(Pname::enum()) -> [f()].
getFloatv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5067),
  rec(5067).

-spec getIntegerv(Pname::enum()) -> [i()].
getIntegerv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5068),
  rec(5068).

-spec pushAttrib(Mask::i()) -> 'ok'.
pushAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5069),
  ok.

-spec popAttrib() -> 'ok'.
popAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5070),
  ok.

-spec pushClientAttrib(Mask::i()) -> 'ok'.
pushClientAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5071),
  ok.

-spec popClientAttrib() -> 'ok'.
popClientAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5072),
  ok.

-spec renderMode(Mode::enum()) -> i().
renderMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5073),
  rec(5073).

-spec getError() -> enum().
getError()  ->
  IF = get_interface(),
  IF:queue_cmd(5074),
  rec(5074).

-spec getString(Name::enum()) -> string().
getString(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5075),
  rec(5075).

-spec finish() -> 'ok'.
finish()  ->
  IF = get_interface(),
  IF:queue_cmd(5076),
  ok.

-spec flush() -> 'ok'.
flush()  ->
  IF = get_interface(),
  IF:queue_cmd(5077),
  ok.

-spec hint(Target::enum(), Mode::enum()) -> 'ok'.
hint(Target,Mode) when is_integer(Target),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Mode,5078),
  ok.

-spec clearDepth(Depth::clamp()) -> 'ok'.
clearDepth(Depth) when is_float(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Depth,5079),
  ok.

-spec depthFunc(Func::enum()) -> 'ok'.
depthFunc(Func) when is_integer(Func) ->
  IF = get_interface(),
  IF:queue_cmd(Func,5080),
  ok.

-spec depthMask(Flag::0|1) -> 'ok'.
depthMask(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5081),
  ok.

-spec depthRange(Near_val::clamp(), Far_val::clamp()) -> 'ok'.
depthRange(Near_val,Far_val) when is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Near_val,Far_val,5082),
  ok.

-spec clearAccum(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
clearAccum(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5083),
  ok.

-spec accum(Op::enum(), Value::f()) -> 'ok'.
accum(Op,Value) when is_integer(Op),is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Op,Value,5084),
  ok.

-spec matrixMode(Mode::enum()) -> 'ok'.
matrixMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5085),
  ok.

-spec ortho(Left::f(), Right::f(), Bottom::f(), Top::f(), Near_val::f(), Far_val::f()) -> 'ok'.
ortho(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5086),
  ok.

-spec frustum(Left::f(), Right::f(), Bottom::f(), Top::f(), Near_val::f(), Far_val::f()) -> 'ok'.
frustum(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5087),
  ok.

-spec viewport(X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
viewport(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5088),
  ok.

-spec pushMatrix() -> 'ok'.
pushMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5089),
  ok.

-spec popMatrix() -> 'ok'.
popMatrix()  ->
  IF = get_interface(),
  IF:queue_cmd(5090),
  ok.

-spec loadIdentity() -> 'ok'.
loadIdentity()  ->
  IF = get_interface(),
  IF:queue_cmd(5091),
  ok.

-spec loadMatrixd(M::matrix()) -> 'ok'.
loadMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5092),
  ok.

-spec loadMatrixf(M::matrix()) -> 'ok'.
loadMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5093),
  ok.

-spec multMatrixd(M::matrix()) -> 'ok'.
multMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5094),
  ok.

-spec multMatrixf(M::matrix()) -> 'ok'.
multMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5095),
  ok.

-spec rotated(Angle::f(), X::f(), Y::f(), Z::f()) -> 'ok'.
rotated(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5096),
  ok.

-spec rotatef(Angle::f(), X::f(), Y::f(), Z::f()) -> 'ok'.
rotatef(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5097),
  ok.

-spec scaled(X::f(), Y::f(), Z::f()) -> 'ok'.
scaled(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5098),
  ok.

-spec scalef(X::f(), Y::f(), Z::f()) -> 'ok'.
scalef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5099),
  ok.

-spec translated(X::f(), Y::f(), Z::f()) -> 'ok'.
translated(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5100),
  ok.

-spec translatef(X::f(), Y::f(), Z::f()) -> 'ok'.
translatef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5101),
  ok.

-spec isList(List::i()) -> 0|1.
isList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5102),
  rec(5102).

-spec deleteLists(List::i(), Range::i()) -> 'ok'.
deleteLists(List,Range) when is_integer(List),is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(List,Range,5103),
  ok.

-spec genLists(Range::i()) -> i().
genLists(Range) when is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(Range,5104),
  rec(5104).

-spec newList(List::i(), Mode::enum()) -> 'ok'.
newList(List,Mode) when is_integer(List),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(List,Mode,5105),
  ok.

-spec endList() -> 'ok'.
endList()  ->
  IF = get_interface(),
  IF:queue_cmd(5106),
  ok.

-spec callList(List::i()) -> 'ok'.
callList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5107),
  ok.

-spec callLists(Lists::[i()]) -> 'ok'.
callLists(Lists) when is_list(Lists) ->
  IF = get_interface(),
  N = length(Lists),
  IF:queue_cmd(N,Lists,5108),
  ok.

-spec listBase(Base::i()) -> 'ok'.
listBase(Base) when is_integer(Base) ->
  IF = get_interface(),
  IF:queue_cmd(Base,5109),
  ok.

-spec 'begin'(Mode::enum()) -> 'ok'.
'begin'(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5110),
  ok.

-spec 'end'() -> 'ok'.
'end'()  ->
  IF = get_interface(),
  IF:queue_cmd(5111),
  ok.

-spec vertex2d(X::f(), Y::f()) -> 'ok'.
vertex2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5112),
  ok.

-spec vertex2f(X::f(), Y::f()) -> 'ok'.
vertex2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5113),
  ok.

-spec vertex2i(X::i(), Y::i()) -> 'ok'.
vertex2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5114),
  ok.

-spec vertex2s(X::i(), Y::i()) -> 'ok'.
vertex2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5115),
  ok.

-spec vertex3d(X::f(), Y::f(), Z::f()) -> 'ok'.
vertex3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5116),
  ok.

-spec vertex3f(X::f(), Y::f(), Z::f()) -> 'ok'.
vertex3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5117),
  ok.

-spec vertex3i(X::i(), Y::i(), Z::i()) -> 'ok'.
vertex3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5118),
  ok.

-spec vertex3s(X::i(), Y::i(), Z::i()) -> 'ok'.
vertex3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5119),
  ok.

-spec vertex4d(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertex4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5120),
  ok.

-spec vertex4f(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertex4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5121),
  ok.

-spec vertex4i(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertex4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5122),
  ok.

-spec vertex4s(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertex4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5123),
  ok.

-spec vertex2dv({X::f(), Y::f()}) -> 'ok'.
vertex2dv({X,Y}) ->  vertex2d(X,Y).
-spec vertex2fv({X::f(), Y::f()}) -> 'ok'.
vertex2fv({X,Y}) ->  vertex2f(X,Y).
-spec vertex2iv({X::i(), Y::i()}) -> 'ok'.
vertex2iv({X,Y}) ->  vertex2i(X,Y).
-spec vertex2sv({X::i(), Y::i()}) -> 'ok'.
vertex2sv({X,Y}) ->  vertex2s(X,Y).
-spec vertex3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
vertex3dv({X,Y,Z}) ->  vertex3d(X,Y,Z).
-spec vertex3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
vertex3fv({X,Y,Z}) ->  vertex3f(X,Y,Z).
-spec vertex3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
vertex3iv({X,Y,Z}) ->  vertex3i(X,Y,Z).
-spec vertex3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
vertex3sv({X,Y,Z}) ->  vertex3s(X,Y,Z).
-spec vertex4dv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertex4dv({X,Y,Z,W}) ->  vertex4d(X,Y,Z,W).
-spec vertex4fv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertex4fv({X,Y,Z,W}) ->  vertex4f(X,Y,Z,W).
-spec vertex4iv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertex4iv({X,Y,Z,W}) ->  vertex4i(X,Y,Z,W).
-spec vertex4sv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertex4sv({X,Y,Z,W}) ->  vertex4s(X,Y,Z,W).
-spec normal3b(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3b(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5124),
  ok.

-spec normal3d(Nx::f(), Ny::f(), Nz::f()) -> 'ok'.
normal3d(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5125),
  ok.

-spec normal3f(Nx::f(), Ny::f(), Nz::f()) -> 'ok'.
normal3f(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5126),
  ok.

-spec normal3i(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3i(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5127),
  ok.

-spec normal3s(Nx::i(), Ny::i(), Nz::i()) -> 'ok'.
normal3s(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5128),
  ok.

-spec normal3bv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3bv({Nx,Ny,Nz}) ->  normal3b(Nx,Ny,Nz).
-spec normal3dv({Nx::f(), Ny::f(), Nz::f()}) -> 'ok'.
normal3dv({Nx,Ny,Nz}) ->  normal3d(Nx,Ny,Nz).
-spec normal3fv({Nx::f(), Ny::f(), Nz::f()}) -> 'ok'.
normal3fv({Nx,Ny,Nz}) ->  normal3f(Nx,Ny,Nz).
-spec normal3iv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3iv({Nx,Ny,Nz}) ->  normal3i(Nx,Ny,Nz).
-spec normal3sv({Nx::i(), Ny::i(), Nz::i()}) -> 'ok'.
normal3sv({Nx,Ny,Nz}) ->  normal3s(Nx,Ny,Nz).
-spec indexd(C::f()) -> 'ok'.
indexd(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5129),
  ok.

-spec indexf(C::f()) -> 'ok'.
indexf(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5130),
  ok.

-spec indexi(C::i()) -> 'ok'.
indexi(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5131),
  ok.

-spec indexs(C::i()) -> 'ok'.
indexs(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5132),
  ok.

-spec indexub(C::i()) -> 'ok'.
indexub(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5133),
  ok.

-spec indexdv({C::f()}) -> 'ok'.
indexdv({C}) ->  indexd(C).
-spec indexfv({C::f()}) -> 'ok'.
indexfv({C}) ->  indexf(C).
-spec indexiv({C::i()}) -> 'ok'.
indexiv({C}) ->  indexi(C).
-spec indexsv({C::i()}) -> 'ok'.
indexsv({C}) ->  indexs(C).
-spec indexubv({C::i()}) -> 'ok'.
indexubv({C}) ->  indexub(C).
-spec color3b(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5134),
  ok.

-spec color3d(Red::f(), Green::f(), Blue::f()) -> 'ok'.
color3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5135),
  ok.

-spec color3f(Red::f(), Green::f(), Blue::f()) -> 'ok'.
color3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5136),
  ok.

-spec color3i(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5137),
  ok.

-spec color3s(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5138),
  ok.

-spec color3ub(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5139),
  ok.

-spec color3ui(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5140),
  ok.

-spec color3us(Red::i(), Green::i(), Blue::i()) -> 'ok'.
color3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5141),
  ok.

-spec color4b(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4b(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5142),
  ok.

-spec color4d(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
color4d(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5143),
  ok.

-spec color4f(Red::f(), Green::f(), Blue::f(), Alpha::f()) -> 'ok'.
color4f(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5144),
  ok.

-spec color4i(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4i(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5145),
  ok.

-spec color4s(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4s(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5146),
  ok.

-spec color4ub(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4ub(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5147),
  ok.

-spec color4ui(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4ui(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5148),
  ok.

-spec color4us(Red::i(), Green::i(), Blue::i(), Alpha::i()) -> 'ok'.
color4us(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5149),
  ok.

-spec color3bv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3bv({Red,Green,Blue}) ->  color3b(Red,Green,Blue).
-spec color3dv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
color3dv({Red,Green,Blue}) ->  color3d(Red,Green,Blue).
-spec color3fv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
color3fv({Red,Green,Blue}) ->  color3f(Red,Green,Blue).
-spec color3iv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3iv({Red,Green,Blue}) ->  color3i(Red,Green,Blue).
-spec color3sv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3sv({Red,Green,Blue}) ->  color3s(Red,Green,Blue).
-spec color3ubv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3ubv({Red,Green,Blue}) ->  color3ub(Red,Green,Blue).
-spec color3uiv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3uiv({Red,Green,Blue}) ->  color3ui(Red,Green,Blue).
-spec color3usv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
color3usv({Red,Green,Blue}) ->  color3us(Red,Green,Blue).
-spec color4bv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4bv({Red,Green,Blue,Alpha}) ->  color4b(Red,Green,Blue,Alpha).
-spec color4dv({Red::f(), Green::f(), Blue::f(), Alpha::f()}) -> 'ok'.
color4dv({Red,Green,Blue,Alpha}) ->  color4d(Red,Green,Blue,Alpha).
-spec color4fv({Red::f(), Green::f(), Blue::f(), Alpha::f()}) -> 'ok'.
color4fv({Red,Green,Blue,Alpha}) ->  color4f(Red,Green,Blue,Alpha).
-spec color4iv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4iv({Red,Green,Blue,Alpha}) ->  color4i(Red,Green,Blue,Alpha).
-spec color4sv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4sv({Red,Green,Blue,Alpha}) ->  color4s(Red,Green,Blue,Alpha).
-spec color4ubv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4ubv({Red,Green,Blue,Alpha}) ->  color4ub(Red,Green,Blue,Alpha).
-spec color4uiv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4uiv({Red,Green,Blue,Alpha}) ->  color4ui(Red,Green,Blue,Alpha).
-spec color4usv({Red::i(), Green::i(), Blue::i(), Alpha::i()}) -> 'ok'.
color4usv({Red,Green,Blue,Alpha}) ->  color4us(Red,Green,Blue,Alpha).
-spec texCoord1d(S::f()) -> 'ok'.
texCoord1d(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5150),
  ok.

-spec texCoord1f(S::f()) -> 'ok'.
texCoord1f(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5151),
  ok.

-spec texCoord1i(S::i()) -> 'ok'.
texCoord1i(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5152),
  ok.

-spec texCoord1s(S::i()) -> 'ok'.
texCoord1s(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5153),
  ok.

-spec texCoord2d(S::f(), T::f()) -> 'ok'.
texCoord2d(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5154),
  ok.

-spec texCoord2f(S::f(), T::f()) -> 'ok'.
texCoord2f(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5155),
  ok.

-spec texCoord2i(S::i(), T::i()) -> 'ok'.
texCoord2i(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5156),
  ok.

-spec texCoord2s(S::i(), T::i()) -> 'ok'.
texCoord2s(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5157),
  ok.

-spec texCoord3d(S::f(), T::f(), R::f()) -> 'ok'.
texCoord3d(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5158),
  ok.

-spec texCoord3f(S::f(), T::f(), R::f()) -> 'ok'.
texCoord3f(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5159),
  ok.

-spec texCoord3i(S::i(), T::i(), R::i()) -> 'ok'.
texCoord3i(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5160),
  ok.

-spec texCoord3s(S::i(), T::i(), R::i()) -> 'ok'.
texCoord3s(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5161),
  ok.

-spec texCoord4d(S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
texCoord4d(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5162),
  ok.

-spec texCoord4f(S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
texCoord4f(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5163),
  ok.

-spec texCoord4i(S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
texCoord4i(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5164),
  ok.

-spec texCoord4s(S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
texCoord4s(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5165),
  ok.

-spec texCoord1dv({S::f()}) -> 'ok'.
texCoord1dv({S}) ->  texCoord1d(S).
-spec texCoord1fv({S::f()}) -> 'ok'.
texCoord1fv({S}) ->  texCoord1f(S).
-spec texCoord1iv({S::i()}) -> 'ok'.
texCoord1iv({S}) ->  texCoord1i(S).
-spec texCoord1sv({S::i()}) -> 'ok'.
texCoord1sv({S}) ->  texCoord1s(S).
-spec texCoord2dv({S::f(), T::f()}) -> 'ok'.
texCoord2dv({S,T}) ->  texCoord2d(S,T).
-spec texCoord2fv({S::f(), T::f()}) -> 'ok'.
texCoord2fv({S,T}) ->  texCoord2f(S,T).
-spec texCoord2iv({S::i(), T::i()}) -> 'ok'.
texCoord2iv({S,T}) ->  texCoord2i(S,T).
-spec texCoord2sv({S::i(), T::i()}) -> 'ok'.
texCoord2sv({S,T}) ->  texCoord2s(S,T).
-spec texCoord3dv({S::f(), T::f(), R::f()}) -> 'ok'.
texCoord3dv({S,T,R}) ->  texCoord3d(S,T,R).
-spec texCoord3fv({S::f(), T::f(), R::f()}) -> 'ok'.
texCoord3fv({S,T,R}) ->  texCoord3f(S,T,R).
-spec texCoord3iv({S::i(), T::i(), R::i()}) -> 'ok'.
texCoord3iv({S,T,R}) ->  texCoord3i(S,T,R).
-spec texCoord3sv({S::i(), T::i(), R::i()}) -> 'ok'.
texCoord3sv({S,T,R}) ->  texCoord3s(S,T,R).
-spec texCoord4dv({S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
texCoord4dv({S,T,R,Q}) ->  texCoord4d(S,T,R,Q).
-spec texCoord4fv({S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
texCoord4fv({S,T,R,Q}) ->  texCoord4f(S,T,R,Q).
-spec texCoord4iv({S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
texCoord4iv({S,T,R,Q}) ->  texCoord4i(S,T,R,Q).
-spec texCoord4sv({S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
texCoord4sv({S,T,R,Q}) ->  texCoord4s(S,T,R,Q).
-spec rasterPos2d(X::f(), Y::f()) -> 'ok'.
rasterPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5166),
  ok.

-spec rasterPos2f(X::f(), Y::f()) -> 'ok'.
rasterPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5167),
  ok.

-spec rasterPos2i(X::i(), Y::i()) -> 'ok'.
rasterPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5168),
  ok.

-spec rasterPos2s(X::i(), Y::i()) -> 'ok'.
rasterPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5169),
  ok.

-spec rasterPos3d(X::f(), Y::f(), Z::f()) -> 'ok'.
rasterPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5170),
  ok.

-spec rasterPos3f(X::f(), Y::f(), Z::f()) -> 'ok'.
rasterPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5171),
  ok.

-spec rasterPos3i(X::i(), Y::i(), Z::i()) -> 'ok'.
rasterPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5172),
  ok.

-spec rasterPos3s(X::i(), Y::i(), Z::i()) -> 'ok'.
rasterPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5173),
  ok.

-spec rasterPos4d(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
rasterPos4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5174),
  ok.

-spec rasterPos4f(X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
rasterPos4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5175),
  ok.

-spec rasterPos4i(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
rasterPos4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5176),
  ok.

-spec rasterPos4s(X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
rasterPos4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5177),
  ok.

-spec rasterPos2dv({X::f(), Y::f()}) -> 'ok'.
rasterPos2dv({X,Y}) ->  rasterPos2d(X,Y).
-spec rasterPos2fv({X::f(), Y::f()}) -> 'ok'.
rasterPos2fv({X,Y}) ->  rasterPos2f(X,Y).
-spec rasterPos2iv({X::i(), Y::i()}) -> 'ok'.
rasterPos2iv({X,Y}) ->  rasterPos2i(X,Y).
-spec rasterPos2sv({X::i(), Y::i()}) -> 'ok'.
rasterPos2sv({X,Y}) ->  rasterPos2s(X,Y).
-spec rasterPos3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
rasterPos3dv({X,Y,Z}) ->  rasterPos3d(X,Y,Z).
-spec rasterPos3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
rasterPos3fv({X,Y,Z}) ->  rasterPos3f(X,Y,Z).
-spec rasterPos3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
rasterPos3iv({X,Y,Z}) ->  rasterPos3i(X,Y,Z).
-spec rasterPos3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
rasterPos3sv({X,Y,Z}) ->  rasterPos3s(X,Y,Z).
-spec rasterPos4dv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
rasterPos4dv({X,Y,Z,W}) ->  rasterPos4d(X,Y,Z,W).
-spec rasterPos4fv({X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
rasterPos4fv({X,Y,Z,W}) ->  rasterPos4f(X,Y,Z,W).
-spec rasterPos4iv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
rasterPos4iv({X,Y,Z,W}) ->  rasterPos4i(X,Y,Z,W).
-spec rasterPos4sv({X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
rasterPos4sv({X,Y,Z,W}) ->  rasterPos4s(X,Y,Z,W).
-spec rectd(X1::f(), Y1::f(), X2::f(), Y2::f()) -> 'ok'.
rectd(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5178),
  ok.

-spec rectf(X1::f(), Y1::f(), X2::f(), Y2::f()) -> 'ok'.
rectf(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5179),
  ok.

-spec recti(X1::i(), Y1::i(), X2::i(), Y2::i()) -> 'ok'.
recti(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5180),
  ok.

-spec rects(X1::i(), Y1::i(), X2::i(), Y2::i()) -> 'ok'.
rects(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5181),
  ok.

-spec rectdv(V1::{f(),f()}, V2::{f(),f()}) -> 'ok'.
rectdv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5182),
  ok.

-spec rectfv(V1::{f(),f()}, V2::{f(),f()}) -> 'ok'.
rectfv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5183),
  ok.

-spec rectiv(V1::{i(),i()}, V2::{i(),i()}) -> 'ok'.
rectiv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5184),
  ok.

-spec rectsv(V1::{i(),i()}, V2::{i(),i()}) -> 'ok'.
rectsv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5185),
  ok.

-spec vertexPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
vertexPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5186),
  ok.

-spec normalPointer(Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
normalPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5188),
  ok.

-spec colorPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
colorPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5190),
  ok.

-spec indexPointer(Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
indexPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5192),
  ok.

-spec texCoordPointer(Size::i(), Type::enum(), Stride::i(), Ptr::offset()|mem()) -> 'ok'.
texCoordPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5194),
  ok.

-spec edgeFlagPointer(Stride::i(), Ptr::offset()|mem()) -> 'ok'.
edgeFlagPointer(Stride,Ptr) when is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Stride,Ptr,5196),
  ok.

-spec arrayElement(I::i()) -> 'ok'.
arrayElement(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5198),
  ok.

-spec drawArrays(Mode::enum(), First::i(), Count::i()) -> 'ok'.
drawArrays(Mode,First,Count) when is_integer(Mode),is_integer(First),is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5199),
  ok.

-spec drawElements(Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem()) -> 'ok'.
drawElements(Mode,Count,Type,Indices) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,5200),
  ok.

-spec interleavedArrays(Format::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
interleavedArrays(Format,Stride,Pointer) when is_integer(Format),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Format,Stride,Pointer,5202),
  ok.

-spec shadeModel(Mode::enum()) -> 'ok'.
shadeModel(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5204),
  ok.

-spec lightf(Light::enum(), Pname::enum(), Param::f()) -> 'ok'.
lightf(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5205),
  ok.

-spec lighti(Light::enum(), Pname::enum(), Param::i()) -> 'ok'.
lighti(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5206),
  ok.

-spec lightfv(Light::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
lightfv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5207),
  ok.

-spec lightiv(Light::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
lightiv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5208),
  ok.

-spec getLightfv(Light::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getLightfv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5209),
  rec(5209).

-spec getLightiv(Light::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getLightiv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5210),
  rec(5210).

-spec lightModelf(Pname::enum(), Param::f()) -> 'ok'.
lightModelf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5211),
  ok.

-spec lightModeli(Pname::enum(), Param::i()) -> 'ok'.
lightModeli(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5212),
  ok.

-spec lightModelfv(Pname::enum(), Params::tuple()) -> 'ok'.
lightModelfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5213),
  ok.

-spec lightModeliv(Pname::enum(), Params::tuple()) -> 'ok'.
lightModeliv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5214),
  ok.

-spec materialf(Face::enum(), Pname::enum(), Param::f()) -> 'ok'.
materialf(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5215),
  ok.

-spec materiali(Face::enum(), Pname::enum(), Param::i()) -> 'ok'.
materiali(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5216),
  ok.

-spec materialfv(Face::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
materialfv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5217),
  ok.

-spec materialiv(Face::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
materialiv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5218),
  ok.

-spec getMaterialfv(Face::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getMaterialfv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5219),
  rec(5219).

-spec getMaterialiv(Face::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getMaterialiv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5220),
  rec(5220).

-spec colorMaterial(Face::enum(), Mode::enum()) -> 'ok'.
colorMaterial(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5221),
  ok.

-spec pixelZoom(Xfactor::f(), Yfactor::f()) -> 'ok'.
pixelZoom(Xfactor,Yfactor) when is_float(Xfactor),is_float(Yfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Xfactor,Yfactor,5222),
  ok.

-spec pixelStoref(Pname::enum(), Param::f()) -> 'ok'.
pixelStoref(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5223),
  ok.

-spec pixelStorei(Pname::enum(), Param::i()) -> 'ok'.
pixelStorei(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5224),
  ok.

-spec pixelTransferf(Pname::enum(), Param::f()) -> 'ok'.
pixelTransferf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5225),
  ok.

-spec pixelTransferi(Pname::enum(), Param::i()) -> 'ok'.
pixelTransferi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5226),
  ok.

-spec pixelMapfv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapfv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5227),
  ok.

-spec pixelMapuiv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapuiv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5228),
  ok.

-spec pixelMapusv(Map::enum(), Mapsize::i(), Values::binary()) -> 'ok'.
pixelMapusv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5229),
  ok.

-spec getPixelMapfv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapfv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5230),
  rec(5230).

-spec getPixelMapuiv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapuiv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5231),
  rec(5231).

-spec getPixelMapusv(Map::enum(), Values::mem()) -> 'ok'.
getPixelMapusv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5232),
  rec(5232).

-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 'ok'
    when Width::i(), Height::i(), Xorig::f(), Yorig::f(), Xmove::f(), Ymove::f(), Bitmap::offset()|mem().
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when is_integer(Width),is_integer(Height),is_float(Xorig),is_float(Yorig),is_float(Xmove),is_float(Ymove),is_integer(Bitmap) orelse is_tuple(Bitmap) orelse is_binary(Bitmap) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap,5233),
  ok.

-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> 'ok'
    when X::i(), Y::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::mem().
readPixels(X,Y,Width,Height,Format,Type,Pixels) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Format,Type,Pixels,5235),
  rec(5235).

-spec drawPixels(Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::offset()|mem()) -> 'ok'.
drawPixels(Width,Height,Format,Type,Pixels) when is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Format,Type,Pixels,5236),
  ok.

-spec copyPixels(X::i(), Y::i(), Width::i(), Height::i(), Type::enum()) -> 'ok'.
copyPixels(X,Y,Width,Height,Type) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Type,5238),
  ok.

-spec stencilFunc(Func::enum(), Ref::i(), Mask::i()) -> 'ok'.
stencilFunc(Func,Ref,Mask) when is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,Mask,5239),
  ok.

-spec stencilMask(Mask::i()) -> 'ok'.
stencilMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5240),
  ok.

-spec stencilOp(Fail::enum(), Zfail::enum(), Zpass::enum()) -> 'ok'.
stencilOp(Fail,Zfail,Zpass) when is_integer(Fail),is_integer(Zfail),is_integer(Zpass) ->
  IF = get_interface(),
  IF:queue_cmd(Fail,Zfail,Zpass,5241),
  ok.

-spec clearStencil(S::i()) -> 'ok'.
clearStencil(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5242),
  ok.

-spec texGend(Coord::enum(), Pname::enum(), Param::f()) -> 'ok'.
texGend(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5243),
  ok.

-spec texGenf(Coord::enum(), Pname::enum(), Param::f()) -> 'ok'.
texGenf(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5244),
  ok.

-spec texGeni(Coord::enum(), Pname::enum(), Param::i()) -> 'ok'.
texGeni(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5245),
  ok.

-spec texGendv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGendv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5246),
  ok.

-spec texGenfv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGenfv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5247),
  ok.

-spec texGeniv(Coord::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texGeniv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5248),
  ok.

-spec getTexGendv(Coord::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexGendv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5249),
  rec(5249).

-spec getTexGenfv(Coord::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexGenfv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5250),
  rec(5250).

-spec getTexGeniv(Coord::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexGeniv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5251),
  rec(5251).

-spec texEnvf(Target::enum(), Pname::enum(), Param::f()) -> 'ok'.
texEnvf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5252),
  ok.

-spec texEnvi(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
texEnvi(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5253),
  ok.

-spec texEnvfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texEnvfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5254),
  ok.

-spec texEnviv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texEnviv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5255),
  ok.

-spec getTexEnvfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexEnvfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5256),
  rec(5256).

-spec getTexEnviv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexEnviv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5257),
  rec(5257).

-spec texParameterf(Target::enum(), Pname::enum(), Param::f()) -> 'ok'.
texParameterf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5258),
  ok.

-spec texParameteri(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
texParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5259),
  ok.

-spec texParameterfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5260),
  ok.

-spec texParameteriv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5261),
  ok.

-spec getTexParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getTexParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5262),
  rec(5262).

-spec getTexParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5263),
  rec(5263).

-spec getTexLevelParameterfv(Target::enum(), Level::i(), Pname::enum()) -> {f()}.
getTexLevelParameterfv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5264),
  rec(5264).

-spec getTexLevelParameteriv(Target::enum(), Level::i(), Pname::enum()) -> {i()}.
getTexLevelParameteriv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5265),
  rec(5265).

-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels,5266),
  ok.

-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Height::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels,5268),
  ok.

-spec getTexImage(Target::enum(), Level::i(), Format::enum(), Type::enum(), Pixels::mem()) -> 'ok'.
getTexImage(Target,Level,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Format,Type,Pixels,5270),
  rec(5270).

-spec genTextures(N::i()) -> [i()].
genTextures(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5271),
  rec(5271).

-spec deleteTextures(Textures::[i()]) -> 'ok'.
deleteTextures(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5272),
  ok.

-spec bindTexture(Target::enum(), Texture::i()) -> 'ok'.
bindTexture(Target,Texture) when is_integer(Target),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Texture,5273),
  ok.

-spec prioritizeTextures(Textures::[i()], Priorities::[clamp()]) -> 'ok'.
prioritizeTextures(Textures,Priorities) when is_list(Textures),is_list(Priorities) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,Priorities,5274),
  ok.

-spec areTexturesResident(Textures::[i()]) -> {0|1,Residences::[0|1]}.
areTexturesResident(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5275),
  rec(5275).

-spec isTexture(Texture::i()) -> 0|1.
isTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5276),
  rec(5276).

-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,Type,Pixels,5277),
  ok.

-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels,5279),
  ok.

-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Border::i().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Border,5281),
  ok.

-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Height::i(), Border::i().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Height,Border,5282),
  ok.

-spec copyTexSubImage1D(Target::enum(), Level::i(), Xoffset::i(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,X,Y,Width,5283),
  ok.

-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), X::i(), Y::i(), Width::i(), Height::i().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,X,Y,Width,Height,5284),
  ok.

-spec map1d(Target::enum(), U1::f(), U2::f(), Stride::i(), Order::i(), Points::binary()) -> 'ok'.
map1d(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5285),
  ok.

-spec map1f(Target::enum(), U1::f(), U2::f(), Stride::i(), Order::i(), Points::binary()) -> 'ok'.
map1f(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5286),
  ok.

-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok'
    when Target::enum(), U1::f(), U2::f(), Ustride::i(), Uorder::i(), V1::f(), V2::f(), Vstride::i(), Vorder::i(), Points::binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5287),
  ok.

-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok'
    when Target::enum(), U1::f(), U2::f(), Ustride::i(), Uorder::i(), V1::f(), V2::f(), Vstride::i(), Vorder::i(), Points::binary().
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5288),
  ok.

-spec getMapdv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapdv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5289),
  rec(5289).

-spec getMapfv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapfv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5290),
  rec(5290).

-spec getMapiv(Target::enum(), Query::enum(), V::mem()) -> 'ok'.
getMapiv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5291),
  rec(5291).

-spec evalCoord1d(U::f()) -> 'ok'.
evalCoord1d(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5292),
  ok.

-spec evalCoord1f(U::f()) -> 'ok'.
evalCoord1f(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5293),
  ok.

-spec evalCoord1dv({U::f()}) -> 'ok'.
evalCoord1dv({U}) ->  evalCoord1d(U).
-spec evalCoord1fv({U::f()}) -> 'ok'.
evalCoord1fv({U}) ->  evalCoord1f(U).
-spec evalCoord2d(U::f(), V::f()) -> 'ok'.
evalCoord2d(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5294),
  ok.

-spec evalCoord2f(U::f(), V::f()) -> 'ok'.
evalCoord2f(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5295),
  ok.

-spec evalCoord2dv({U::f(), V::f()}) -> 'ok'.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).
-spec evalCoord2fv({U::f(), V::f()}) -> 'ok'.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).
-spec mapGrid1d(Un::i(), U1::f(), U2::f()) -> 'ok'.
mapGrid1d(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5296),
  ok.

-spec mapGrid1f(Un::i(), U1::f(), U2::f()) -> 'ok'.
mapGrid1f(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5297),
  ok.

-spec mapGrid2d(Un::i(), U1::f(), U2::f(), Vn::i(), V1::f(), V2::f()) -> 'ok'.
mapGrid2d(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5298),
  ok.

-spec mapGrid2f(Un::i(), U1::f(), U2::f(), Vn::i(), V1::f(), V2::f()) -> 'ok'.
mapGrid2f(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5299),
  ok.

-spec evalPoint1(I::i()) -> 'ok'.
evalPoint1(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5300),
  ok.

-spec evalPoint2(I::i(), J::i()) -> 'ok'.
evalPoint2(I,J) when is_integer(I),is_integer(J) ->
  IF = get_interface(),
  IF:queue_cmd(I,J,5301),
  ok.

-spec evalMesh1(Mode::enum(), I1::i(), I2::i()) -> 'ok'.
evalMesh1(Mode,I1,I2) when is_integer(Mode),is_integer(I1),is_integer(I2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,5302),
  ok.

-spec evalMesh2(Mode::enum(), I1::i(), I2::i(), J1::i(), J2::i()) -> 'ok'.
evalMesh2(Mode,I1,I2,J1,J2) when is_integer(Mode),is_integer(I1),is_integer(I2),is_integer(J1),is_integer(J2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,J1,J2,5303),
  ok.

-spec fogf(Pname::enum(), Param::f()) -> 'ok'.
fogf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5304),
  ok.

-spec fogi(Pname::enum(), Param::i()) -> 'ok'.
fogi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5305),
  ok.

-spec fogfv(Pname::enum(), Params::tuple()) -> 'ok'.
fogfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5306),
  ok.

-spec fogiv(Pname::enum(), Params::tuple()) -> 'ok'.
fogiv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5307),
  ok.

-spec feedbackBuffer(Size::i(), Type::enum(), Buffer::mem()) -> 'ok'.
feedbackBuffer(Size,Type,Buffer) when is_integer(Size),is_integer(Type),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Buffer,5308),
  rec(5308).

-spec passThrough(Token::f()) -> 'ok'.
passThrough(Token) when is_float(Token) ->
  IF = get_interface(),
  IF:queue_cmd(Token,5309),
  ok.

-spec selectBuffer(Size::i(), Buffer::mem()) -> 'ok'.
selectBuffer(Size,Buffer) when is_integer(Size),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Buffer,5310),
  rec(5310).

-spec initNames() -> 'ok'.
initNames()  ->
  IF = get_interface(),
  IF:queue_cmd(5311),
  ok.

-spec loadName(Name::i()) -> 'ok'.
loadName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5312),
  ok.

-spec pushName(Name::i()) -> 'ok'.
pushName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5313),
  ok.

-spec popName() -> 'ok'.
popName()  ->
  IF = get_interface(),
  IF:queue_cmd(5314),
  ok.

-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 'ok'
    when Mode::enum(), Start::i(), End::i(), Count::i(), Type::enum(), Indices::offset()|mem().
drawRangeElements(Mode,Start,End,Count,Type,Indices) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,5315),
  ok.

-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), InternalFormat::i(), Width::i(), Height::i(), Depth::i(), Border::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels,5317),
  ok.

-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Pixels::offset()|mem().
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels,5319),
  ok.

-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), X::i(), Y::i(), Width::i(), Height::i().
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height,5321),
  ok.

-spec activeTexture(Texture::enum()) -> 'ok'.
activeTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5322),
  ok.

-spec sampleCoverage(Value::clamp(), Invert::0|1) -> 'ok'.
sampleCoverage(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5323),
  ok.

-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data,5324),
  ok.

-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Height::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data,5326),
  ok.

-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Internalformat::enum(), Width::i(), Border::i(), ImageSize::i(), Data::offset()|mem().
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Border,ImageSize,Data,5328),
  ok.

-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5330),
  ok.

-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5332),
  ok.

-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,ImageSize,Data,5334),
  ok.

-spec getCompressedTexImage(Target::enum(), Lod::i(), Img::mem()) -> 'ok'.
getCompressedTexImage(Target,Lod,Img) when is_integer(Target),is_integer(Lod),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Lod,Img,5336),
  rec(5336).

-spec clientActiveTexture(Texture::enum()) -> 'ok'.
clientActiveTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5337),
  ok.

-spec multiTexCoord1d(Target::enum(), S::f()) -> 'ok'.
multiTexCoord1d(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5338),
  ok.

-spec multiTexCoord1dv(Target::enum(), {S::f()}) -> 'ok'.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).
-spec multiTexCoord1f(Target::enum(), S::f()) -> 'ok'.
multiTexCoord1f(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5339),
  ok.

-spec multiTexCoord1fv(Target::enum(), {S::f()}) -> 'ok'.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).
-spec multiTexCoord1i(Target::enum(), S::i()) -> 'ok'.
multiTexCoord1i(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5340),
  ok.

-spec multiTexCoord1iv(Target::enum(), {S::i()}) -> 'ok'.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).
-spec multiTexCoord1s(Target::enum(), S::i()) -> 'ok'.
multiTexCoord1s(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5341),
  ok.

-spec multiTexCoord1sv(Target::enum(), {S::i()}) -> 'ok'.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).
-spec multiTexCoord2d(Target::enum(), S::f(), T::f()) -> 'ok'.
multiTexCoord2d(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5342),
  ok.

-spec multiTexCoord2dv(Target::enum(), {S::f(), T::f()}) -> 'ok'.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).
-spec multiTexCoord2f(Target::enum(), S::f(), T::f()) -> 'ok'.
multiTexCoord2f(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5343),
  ok.

-spec multiTexCoord2fv(Target::enum(), {S::f(), T::f()}) -> 'ok'.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).
-spec multiTexCoord2i(Target::enum(), S::i(), T::i()) -> 'ok'.
multiTexCoord2i(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5344),
  ok.

-spec multiTexCoord2iv(Target::enum(), {S::i(), T::i()}) -> 'ok'.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).
-spec multiTexCoord2s(Target::enum(), S::i(), T::i()) -> 'ok'.
multiTexCoord2s(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5345),
  ok.

-spec multiTexCoord2sv(Target::enum(), {S::i(), T::i()}) -> 'ok'.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).
-spec multiTexCoord3d(Target::enum(), S::f(), T::f(), R::f()) -> 'ok'.
multiTexCoord3d(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5346),
  ok.

-spec multiTexCoord3dv(Target::enum(), {S::f(), T::f(), R::f()}) -> 'ok'.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).
-spec multiTexCoord3f(Target::enum(), S::f(), T::f(), R::f()) -> 'ok'.
multiTexCoord3f(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5347),
  ok.

-spec multiTexCoord3fv(Target::enum(), {S::f(), T::f(), R::f()}) -> 'ok'.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).
-spec multiTexCoord3i(Target::enum(), S::i(), T::i(), R::i()) -> 'ok'.
multiTexCoord3i(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5348),
  ok.

-spec multiTexCoord3iv(Target::enum(), {S::i(), T::i(), R::i()}) -> 'ok'.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).
-spec multiTexCoord3s(Target::enum(), S::i(), T::i(), R::i()) -> 'ok'.
multiTexCoord3s(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5349),
  ok.

-spec multiTexCoord3sv(Target::enum(), {S::i(), T::i(), R::i()}) -> 'ok'.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).
-spec multiTexCoord4d(Target::enum(), S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
multiTexCoord4d(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5350),
  ok.

-spec multiTexCoord4dv(Target::enum(), {S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).
-spec multiTexCoord4f(Target::enum(), S::f(), T::f(), R::f(), Q::f()) -> 'ok'.
multiTexCoord4f(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5351),
  ok.

-spec multiTexCoord4fv(Target::enum(), {S::f(), T::f(), R::f(), Q::f()}) -> 'ok'.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).
-spec multiTexCoord4i(Target::enum(), S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
multiTexCoord4i(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5352),
  ok.

-spec multiTexCoord4iv(Target::enum(), {S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).
-spec multiTexCoord4s(Target::enum(), S::i(), T::i(), R::i(), Q::i()) -> 'ok'.
multiTexCoord4s(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5353),
  ok.

-spec multiTexCoord4sv(Target::enum(), {S::i(), T::i(), R::i(), Q::i()}) -> 'ok'.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).
-spec loadTransposeMatrixf(M::matrix()) -> 'ok'.
loadTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5354),
  ok.

-spec loadTransposeMatrixd(M::matrix()) -> 'ok'.
loadTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5355),
  ok.

-spec multTransposeMatrixf(M::matrix()) -> 'ok'.
multTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5356),
  ok.

-spec multTransposeMatrixd(M::matrix()) -> 'ok'.
multTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5357),
  ok.

-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 'ok'
    when SfactorRGB::enum(), DfactorRGB::enum(), SfactorAlpha::enum(), DfactorAlpha::enum().
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) when is_integer(SfactorRGB),is_integer(DfactorRGB),is_integer(SfactorAlpha),is_integer(DfactorAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha,5358),
  ok.

-spec multiDrawArrays(Mode::enum(), First::[integer()]|mem(), Count::[integer()]|mem()) -> 'ok'.
multiDrawArrays(Mode,First,Count) when is_integer(Mode),is_list(First) orelse is_tuple(First) orelse is_binary(First),is_list(Count) orelse is_tuple(Count) orelse is_binary(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5359),
  ok.

-spec pointParameterf(Pname::enum(), Param::f()) -> 'ok'.
pointParameterf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5361),
  ok.

-spec pointParameterfv(Pname::enum(), Params::tuple()) -> 'ok'.
pointParameterfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5362),
  ok.

-spec pointParameteri(Pname::enum(), Param::i()) -> 'ok'.
pointParameteri(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5363),
  ok.

-spec pointParameteriv(Pname::enum(), Params::tuple()) -> 'ok'.
pointParameteriv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5364),
  ok.

-spec fogCoordf(Coord::f()) -> 'ok'.
fogCoordf(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5365),
  ok.

-spec fogCoordfv({Coord::f()}) -> 'ok'.
fogCoordfv({Coord}) ->  fogCoordf(Coord).
-spec fogCoordd(Coord::f()) -> 'ok'.
fogCoordd(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5366),
  ok.

-spec fogCoorddv({Coord::f()}) -> 'ok'.
fogCoorddv({Coord}) ->  fogCoordd(Coord).
-spec fogCoordPointer(Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
fogCoordPointer(Type,Stride,Pointer) when is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Pointer,5367),
  ok.

-spec secondaryColor3b(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5369),
  ok.

-spec secondaryColor3bv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).
-spec secondaryColor3d(Red::f(), Green::f(), Blue::f()) -> 'ok'.
secondaryColor3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5370),
  ok.

-spec secondaryColor3dv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).
-spec secondaryColor3f(Red::f(), Green::f(), Blue::f()) -> 'ok'.
secondaryColor3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5371),
  ok.

-spec secondaryColor3fv({Red::f(), Green::f(), Blue::f()}) -> 'ok'.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).
-spec secondaryColor3i(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5372),
  ok.

-spec secondaryColor3iv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).
-spec secondaryColor3s(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5373),
  ok.

-spec secondaryColor3sv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).
-spec secondaryColor3ub(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5374),
  ok.

-spec secondaryColor3ubv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).
-spec secondaryColor3ui(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5375),
  ok.

-spec secondaryColor3uiv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).
-spec secondaryColor3us(Red::i(), Green::i(), Blue::i()) -> 'ok'.
secondaryColor3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5376),
  ok.

-spec secondaryColor3usv({Red::i(), Green::i(), Blue::i()}) -> 'ok'.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).
-spec secondaryColorPointer(Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
secondaryColorPointer(Size,Type,Stride,Pointer) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Pointer,5377),
  ok.

-spec windowPos2d(X::f(), Y::f()) -> 'ok'.
windowPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5379),
  ok.

-spec windowPos2dv({X::f(), Y::f()}) -> 'ok'.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).
-spec windowPos2f(X::f(), Y::f()) -> 'ok'.
windowPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5380),
  ok.

-spec windowPos2fv({X::f(), Y::f()}) -> 'ok'.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).
-spec windowPos2i(X::i(), Y::i()) -> 'ok'.
windowPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5381),
  ok.

-spec windowPos2iv({X::i(), Y::i()}) -> 'ok'.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).
-spec windowPos2s(X::i(), Y::i()) -> 'ok'.
windowPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5382),
  ok.

-spec windowPos2sv({X::i(), Y::i()}) -> 'ok'.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).
-spec windowPos3d(X::f(), Y::f(), Z::f()) -> 'ok'.
windowPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5383),
  ok.

-spec windowPos3dv({X::f(), Y::f(), Z::f()}) -> 'ok'.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).
-spec windowPos3f(X::f(), Y::f(), Z::f()) -> 'ok'.
windowPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5384),
  ok.

-spec windowPos3fv({X::f(), Y::f(), Z::f()}) -> 'ok'.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).
-spec windowPos3i(X::i(), Y::i(), Z::i()) -> 'ok'.
windowPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5385),
  ok.

-spec windowPos3iv({X::i(), Y::i(), Z::i()}) -> 'ok'.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).
-spec windowPos3s(X::i(), Y::i(), Z::i()) -> 'ok'.
windowPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5386),
  ok.

-spec windowPos3sv({X::i(), Y::i(), Z::i()}) -> 'ok'.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).
-spec blendColor(Red::clamp(), Green::clamp(), Blue::clamp(), Alpha::clamp()) -> 'ok'.
blendColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5387),
  ok.

-spec blendEquation(Mode::enum()) -> 'ok'.
blendEquation(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5388),
  ok.

-spec genQueries(N::i()) -> [i()].
genQueries(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5389),
  rec(5389).

-spec deleteQueries(Ids::[i()]) -> 'ok'.
deleteQueries(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5390),
  ok.

-spec isQuery(Id::i()) -> 0|1.
isQuery(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5391),
  rec(5391).

-spec beginQuery(Target::enum(), Id::i()) -> 'ok'.
beginQuery(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5392),
  ok.

-spec endQuery(Target::enum()) -> 'ok'.
endQuery(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5393),
  ok.

-spec getQueryiv(Target::enum(), Pname::enum()) -> i().
getQueryiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5394),
  rec(5394).

-spec getQueryObjectiv(Id::i(), Pname::enum()) -> i().
getQueryObjectiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5395),
  rec(5395).

-spec getQueryObjectuiv(Id::i(), Pname::enum()) -> i().
getQueryObjectuiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5396),
  rec(5396).

-spec bindBuffer(Target::enum(), Buffer::i()) -> 'ok'.
bindBuffer(Target,Buffer) when is_integer(Target),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Buffer,5397),
  ok.

-spec deleteBuffers(Buffers::[i()]) -> 'ok'.
deleteBuffers(Buffers) when is_list(Buffers) ->
  IF = get_interface(),
  N = length(Buffers),
  IF:queue_cmd(N,Buffers,5398),
  ok.

-spec genBuffers(N::i()) -> [i()].
genBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5399),
  rec(5399).

-spec isBuffer(Buffer::i()) -> 0|1.
isBuffer(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5400),
  rec(5400).

-spec bufferData(Target::enum(), Size::i(), Data::offset()|mem(), Usage::enum()) -> 'ok'.
bufferData(Target,Size,Data,Usage) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Usage) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Usage,5401),
  ok.

-spec bufferSubData(Target::enum(), Offset::i(), Size::i(), Data::offset()|mem()) -> 'ok'.
bufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5403),
  ok.

-spec getBufferSubData(Target::enum(), Offset::i(), Size::i(), Data::mem()) -> 'ok'.
getBufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5405),
  rec(5405).

-spec getBufferParameteriv(Target::enum(), Pname::enum()) -> i().
getBufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5406),
  rec(5406).

-spec blendEquationSeparate(ModeRGB::enum(), ModeAlpha::enum()) -> 'ok'.
blendEquationSeparate(ModeRGB,ModeAlpha) when is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(ModeRGB,ModeAlpha,5407),
  ok.

-spec drawBuffers(Bufs::[enum()]) -> 'ok'.
drawBuffers(Bufs) when is_list(Bufs) ->
  IF = get_interface(),
  N = length(Bufs),
  IF:queue_cmd(N,Bufs,5408),
  ok.

-spec stencilOpSeparate(Face::enum(), Sfail::enum(), Dpfail::enum(), Dppass::enum()) -> 'ok'.
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) when is_integer(Face),is_integer(Sfail),is_integer(Dpfail),is_integer(Dppass) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Sfail,Dpfail,Dppass,5409),
  ok.

-spec stencilFuncSeparate(Face::enum(), Func::enum(), Ref::i(), Mask::i()) -> 'ok'.
stencilFuncSeparate(Face,Func,Ref,Mask) when is_integer(Face),is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Func,Ref,Mask,5410),
  ok.

-spec stencilMaskSeparate(Face::enum(), Mask::i()) -> 'ok'.
stencilMaskSeparate(Face,Mask) when is_integer(Face),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mask,5411),
  ok.

-spec attachShader(Program::i(), Shader::i()) -> 'ok'.
attachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5412),
  ok.

-spec bindAttribLocation(Program::i(), Index::i(), Name::string()) -> 'ok'.
bindAttribLocation(Program,Index,Name) when is_integer(Program),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Index,NameBin,5413),
  ok.

-spec compileShader(Shader::i()) -> 'ok'.
compileShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5414),
  ok.

-spec createProgram() -> i().
createProgram()  ->
  IF = get_interface(),
  IF:queue_cmd(5415),
  rec(5415).

-spec createShader(Type::enum()) -> i().
createShader(Type) when is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(Type,5416),
  rec(5416).

-spec deleteProgram(Program::i()) -> 'ok'.
deleteProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5417),
  ok.

-spec deleteShader(Shader::i()) -> 'ok'.
deleteShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5418),
  ok.

-spec detachShader(Program::i(), Shader::i()) -> 'ok'.
detachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5419),
  ok.

-spec disableVertexAttribArray(Index::i()) -> 'ok'.
disableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5420),
  ok.

-spec enableVertexAttribArray(Index::i()) -> 'ok'.
enableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5421),
  ok.

-spec getActiveAttrib(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveAttrib(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5422),
  rec(5422).

-spec getActiveUniform(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveUniform(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5423),
  rec(5423).

-spec getAttachedShaders(Program::i(), MaxCount::i()) -> [i()].
getAttachedShaders(Program,MaxCount) when is_integer(Program),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(Program,MaxCount,5424),
  rec(5424).

-spec getAttribLocation(Program::i(), Name::string()) -> i().
getAttribLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5425),
  rec(5425).

-spec getProgramiv(Program::i(), Pname::enum()) -> i().
getProgramiv(Program,Pname) when is_integer(Program),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,5426),
  rec(5426).

-spec getProgramInfoLog(Program::i(), BufSize::i()) -> string().
getProgramInfoLog(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5427),
  rec(5427).

-spec getShaderiv(Shader::i(), Pname::enum()) -> i().
getShaderiv(Shader,Pname) when is_integer(Shader),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,Pname,5428),
  rec(5428).

-spec getShaderInfoLog(Shader::i(), BufSize::i()) -> string().
getShaderInfoLog(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5429),
  rec(5429).

-spec getShaderSource(Shader::i(), BufSize::i()) -> string().
getShaderSource(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5430),
  rec(5430).

-spec getUniformLocation(Program::i(), Name::string()) -> i().
getUniformLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5431),
  rec(5431).

-spec getUniformfv(Program::i(), Location::i()) -> matrix().
getUniformfv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5432),
  rec(5432).

-spec getUniformiv(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5433),
  rec(5433).

-spec getVertexAttribdv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5434),
  rec(5434).

-spec getVertexAttribfv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribfv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5435),
  rec(5435).

-spec getVertexAttribiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5436),
  rec(5436).

-spec isProgram(Program::i()) -> 0|1.
isProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5437),
  rec(5437).

-spec isShader(Shader::i()) -> 0|1.
isShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5438),
  rec(5438).

-spec linkProgram(Program::i()) -> 'ok'.
linkProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5439),
  ok.

-spec shaderSource(Shader::i(), String::[unicode:chardata()]) -> 'ok'.
shaderSource(Shader,String) when is_integer(Shader),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(Shader,Count,StringTemp,5440),
  ok.

-spec useProgram(Program::i()) -> 'ok'.
useProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5441),
  ok.

-spec uniform1f(Location::i(), V0::f()) -> 'ok'.
uniform1f(Location,V0) when is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5442),
  ok.

-spec uniform2f(Location::i(), V0::f(), V1::f()) -> 'ok'.
uniform2f(Location,V0,V1) when is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5443),
  ok.

-spec uniform3f(Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
uniform3f(Location,V0,V1,V2) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5444),
  ok.

-spec uniform4f(Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
uniform4f(Location,V0,V1,V2,V3) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5445),
  ok.

-spec uniform1i(Location::i(), V0::i()) -> 'ok'.
uniform1i(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5446),
  ok.

-spec uniform2i(Location::i(), V0::i(), V1::i()) -> 'ok'.
uniform2i(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5447),
  ok.

-spec uniform3i(Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
uniform3i(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5448),
  ok.

-spec uniform4i(Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
uniform4i(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5449),
  ok.

-spec uniform1fv(Location::i(), Value::[f()]) -> 'ok'.
uniform1fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5450),
  ok.

-spec uniform2fv(Location::i(), Value::[{f(),f()}]) -> 'ok'.
uniform2fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5451),
  ok.

-spec uniform3fv(Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
uniform3fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5452),
  ok.

-spec uniform4fv(Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniform4fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5453),
  ok.

-spec uniform1iv(Location::i(), Value::[i()]) -> 'ok'.
uniform1iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5454),
  ok.

-spec uniform2iv(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5455),
  ok.

-spec uniform3iv(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5456),
  ok.

-spec uniform4iv(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5457),
  ok.

-spec uniformMatrix2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5458),
  ok.

-spec uniformMatrix3fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5459),
  ok.

-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5460),
  ok.

-spec validateProgram(Program::i()) -> 'ok'.
validateProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5461),
  ok.

-spec vertexAttrib1d(Index::i(), X::f()) -> 'ok'.
vertexAttrib1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5462),
  ok.

-spec vertexAttrib1dv(Index::i(), {X::f()}) -> 'ok'.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).
-spec vertexAttrib1f(Index::i(), X::f()) -> 'ok'.
vertexAttrib1f(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5463),
  ok.

-spec vertexAttrib1fv(Index::i(), {X::f()}) -> 'ok'.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).
-spec vertexAttrib1s(Index::i(), X::i()) -> 'ok'.
vertexAttrib1s(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5464),
  ok.

-spec vertexAttrib1sv(Index::i(), {X::i()}) -> 'ok'.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).
-spec vertexAttrib2d(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttrib2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5465),
  ok.

-spec vertexAttrib2dv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).
-spec vertexAttrib2f(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttrib2f(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5466),
  ok.

-spec vertexAttrib2fv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).
-spec vertexAttrib2s(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttrib2s(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5467),
  ok.

-spec vertexAttrib2sv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).
-spec vertexAttrib3d(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttrib3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5468),
  ok.

-spec vertexAttrib3dv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).
-spec vertexAttrib3f(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttrib3f(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5469),
  ok.

-spec vertexAttrib3fv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).
-spec vertexAttrib3s(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttrib3s(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5470),
  ok.

-spec vertexAttrib3sv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).
-spec vertexAttrib4Nbv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nbv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5471),
  ok.

-spec vertexAttrib4Niv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Niv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5472),
  ok.

-spec vertexAttrib4Nsv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nsv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5473),
  ok.

-spec vertexAttrib4Nub(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttrib4Nub(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5474),
  ok.

-spec vertexAttrib4Nubv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).
-spec vertexAttrib4Nuiv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nuiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5475),
  ok.

-spec vertexAttrib4Nusv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4Nusv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5476),
  ok.

-spec vertexAttrib4bv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5477),
  ok.

-spec vertexAttrib4d(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttrib4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5478),
  ok.

-spec vertexAttrib4dv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).
-spec vertexAttrib4f(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttrib4f(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5479),
  ok.

-spec vertexAttrib4fv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).
-spec vertexAttrib4iv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4iv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5480),
  ok.

-spec vertexAttrib4s(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttrib4s(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5481),
  ok.

-spec vertexAttrib4sv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).
-spec vertexAttrib4ubv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5482),
  ok.

-spec vertexAttrib4uiv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4uiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5483),
  ok.

-spec vertexAttrib4usv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttrib4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5484),
  ok.

-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok'
    when Index::i(), Size::i(), Type::enum(), Normalized::0|1, Stride::i(), Pointer::offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Normalized,Stride,Pointer,5485),
  ok.

-spec uniformMatrix2x3fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5487),
  ok.

-spec uniformMatrix3x2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5488),
  ok.

-spec uniformMatrix2x4fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5489),
  ok.

-spec uniformMatrix4x2fv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix4x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5490),
  ok.

-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix3x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5491),
  ok.

-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5492),
  ok.

-spec colorMaski(Index::i(), R::0|1, G::0|1, B::0|1, A::0|1) -> 'ok'.
colorMaski(Index,R,G,B,A) when is_integer(Index),(0 =:= R) orelse (1 =:= R),(0 =:= G) orelse (1 =:= G),(0 =:= B) orelse (1 =:= B),(0 =:= A) orelse (1 =:= A) ->
  IF = get_interface(),
  IF:queue_cmd(Index,R,G,B,A,5493),
  ok.

-spec getBooleani_v(Target::enum(), Index::i()) -> [0|1].
getBooleani_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5494),
  rec(5494).

-spec getIntegeri_v(Target::enum(), Index::i()) -> [i()].
getIntegeri_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5495),
  rec(5495).

-spec enablei(Target::enum(), Index::i()) -> 'ok'.
enablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5496),
  ok.

-spec disablei(Target::enum(), Index::i()) -> 'ok'.
disablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5497),
  ok.

-spec isEnabledi(Target::enum(), Index::i()) -> 0|1.
isEnabledi(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5498),
  rec(5498).

-spec beginTransformFeedback(PrimitiveMode::enum()) -> 'ok'.
beginTransformFeedback(PrimitiveMode) when is_integer(PrimitiveMode) ->
  IF = get_interface(),
  IF:queue_cmd(PrimitiveMode,5499),
  ok.

-spec endTransformFeedback() -> 'ok'.
endTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5500),
  ok.

-spec bindBufferRange(Target::enum(), Index::i(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
bindBufferRange(Target,Index,Buffer,Offset,Size) when is_integer(Target),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,Offset,Size,5501),
  ok.

-spec bindBufferBase(Target::enum(), Index::i(), Buffer::i()) -> 'ok'.
bindBufferBase(Target,Index,Buffer) when is_integer(Target),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,5502),
  ok.

-spec transformFeedbackVaryings(Program::i(), Varyings::[unicode:chardata()], BufferMode::enum()) -> 'ok'.
transformFeedbackVaryings(Program,Varyings,BufferMode) when is_integer(Program),is_list(Varyings),is_integer(BufferMode) ->
  IF = get_interface(),
  VaryingsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Varyings ],
  Count = length(Varyings),
  IF:queue_cmd(Program,Count,VaryingsTemp,BufferMode,5503),
  ok.

-spec getTransformFeedbackVarying(Program::i(), Index::i(), BufSize::i()) -> {Size::i(),Type::enum(),Name::string()}.
getTransformFeedbackVarying(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5504),
  rec(5504).

-spec clampColor(Target::enum(), Clamp::enum()) -> 'ok'.
clampColor(Target,Clamp) when is_integer(Target),is_integer(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Clamp,5505),
  ok.

-spec beginConditionalRender(Id::i(), Mode::enum()) -> 'ok'.
beginConditionalRender(Id,Mode) when is_integer(Id),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Mode,5506),
  ok.

-spec endConditionalRender() -> 'ok'.
endConditionalRender()  ->
  IF = get_interface(),
  IF:queue_cmd(5507),
  ok.

-spec vertexAttribIPointer(Index::i(), Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5508),
  ok.

-spec getVertexAttribIiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribIiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5510),
  rec(5510).

-spec getVertexAttribIuiv(Index::i(), Pname::enum()) -> {i(),i(),i(),i()}.
getVertexAttribIuiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5511),
  rec(5511).

-spec vertexAttribI1i(Index::i(), X::i()) -> 'ok'.
vertexAttribI1i(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5512),
  ok.

-spec vertexAttribI2i(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttribI2i(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5513),
  ok.

-spec vertexAttribI3i(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttribI3i(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5514),
  ok.

-spec vertexAttribI4i(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttribI4i(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5515),
  ok.

-spec vertexAttribI1ui(Index::i(), X::i()) -> 'ok'.
vertexAttribI1ui(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5516),
  ok.

-spec vertexAttribI2ui(Index::i(), X::i(), Y::i()) -> 'ok'.
vertexAttribI2ui(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5517),
  ok.

-spec vertexAttribI3ui(Index::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
vertexAttribI3ui(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5518),
  ok.

-spec vertexAttribI4ui(Index::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
vertexAttribI4ui(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5519),
  ok.

-spec vertexAttribI1iv(Index::i(), {X::i()}) -> 'ok'.
vertexAttribI1iv(Index,{X}) ->  vertexAttribI1i(Index,X).
-spec vertexAttribI2iv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttribI2iv(Index,{X,Y}) ->  vertexAttribI2i(Index,X,Y).
-spec vertexAttribI3iv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttribI3iv(Index,{X,Y,Z}) ->  vertexAttribI3i(Index,X,Y,Z).
-spec vertexAttribI4iv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttribI4iv(Index,{X,Y,Z,W}) ->  vertexAttribI4i(Index,X,Y,Z,W).
-spec vertexAttribI1uiv(Index::i(), {X::i()}) -> 'ok'.
vertexAttribI1uiv(Index,{X}) ->  vertexAttribI1ui(Index,X).
-spec vertexAttribI2uiv(Index::i(), {X::i(), Y::i()}) -> 'ok'.
vertexAttribI2uiv(Index,{X,Y}) ->  vertexAttribI2ui(Index,X,Y).
-spec vertexAttribI3uiv(Index::i(), {X::i(), Y::i(), Z::i()}) -> 'ok'.
vertexAttribI3uiv(Index,{X,Y,Z}) ->  vertexAttribI3ui(Index,X,Y,Z).
-spec vertexAttribI4uiv(Index::i(), {X::i(), Y::i(), Z::i(), W::i()}) -> 'ok'.
vertexAttribI4uiv(Index,{X,Y,Z,W}) ->  vertexAttribI4ui(Index,X,Y,Z,W).
-spec vertexAttribI4bv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5520),
  ok.

-spec vertexAttribI4sv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4sv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5521),
  ok.

-spec vertexAttribI4ubv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5522),
  ok.

-spec vertexAttribI4usv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
vertexAttribI4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5523),
  ok.

-spec getUniformuiv(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformuiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5524),
  rec(5524).

-spec bindFragDataLocation(Program::i(), Color::i(), Name::string()) -> 'ok'.
bindFragDataLocation(Program,Color,Name) when is_integer(Program),is_integer(Color),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Color,NameBin,5525),
  ok.

-spec getFragDataLocation(Program::i(), Name::string()) -> i().
getFragDataLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5526),
  rec(5526).

-spec uniform1ui(Location::i(), V0::i()) -> 'ok'.
uniform1ui(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5527),
  ok.

-spec uniform2ui(Location::i(), V0::i(), V1::i()) -> 'ok'.
uniform2ui(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5528),
  ok.

-spec uniform3ui(Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
uniform3ui(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5529),
  ok.

-spec uniform4ui(Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
uniform4ui(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5530),
  ok.

-spec uniform1uiv(Location::i(), Value::[i()]) -> 'ok'.
uniform1uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5531),
  ok.

-spec uniform2uiv(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5532),
  ok.

-spec uniform3uiv(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5533),
  ok.

-spec uniform4uiv(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5534),
  ok.

-spec texParameterIiv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterIiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5535),
  ok.

-spec texParameterIuiv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
texParameterIuiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5536),
  ok.

-spec getTexParameterIiv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameterIiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5537),
  rec(5537).

-spec getTexParameterIuiv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getTexParameterIuiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5538),
  rec(5538).

-spec clearBufferiv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5539),
  ok.

-spec clearBufferuiv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferuiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5540),
  ok.

-spec clearBufferfv(Buffer::enum(), Drawbuffer::i(), Value::tuple()) -> 'ok'.
clearBufferfv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5541),
  ok.

-spec clearBufferfi(Buffer::enum(), Drawbuffer::i(), Depth::f(), Stencil::i()) -> 'ok'.
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) when is_integer(Buffer),is_integer(Drawbuffer),is_float(Depth),is_integer(Stencil) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Depth,Stencil,5542),
  ok.

-spec getStringi(Name::enum(), Index::i()) -> string().
getStringi(Name,Index) when is_integer(Name),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Name,Index,5543),
  rec(5543).

-spec isRenderbuffer(Renderbuffer::i()) -> 0|1.
isRenderbuffer(Renderbuffer) when is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Renderbuffer,5544),
  rec(5544).

-spec bindRenderbuffer(Target::enum(), Renderbuffer::i()) -> 'ok'.
bindRenderbuffer(Target,Renderbuffer) when is_integer(Target),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Renderbuffer,5545),
  ok.

-spec deleteRenderbuffers(Renderbuffers::[i()]) -> 'ok'.
deleteRenderbuffers(Renderbuffers) when is_list(Renderbuffers) ->
  IF = get_interface(),
  N = length(Renderbuffers),
  IF:queue_cmd(N,Renderbuffers,5546),
  ok.

-spec genRenderbuffers(N::i()) -> [i()].
genRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5547),
  rec(5547).

-spec renderbufferStorage(Target::enum(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
renderbufferStorage(Target,Internalformat,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,5548),
  ok.

-spec getRenderbufferParameteriv(Target::enum(), Pname::enum()) -> i().
getRenderbufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5549),
  rec(5549).

-spec isFramebuffer(Framebuffer::i()) -> 0|1.
isFramebuffer(Framebuffer) when is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Framebuffer,5550),
  rec(5550).

-spec bindFramebuffer(Target::enum(), Framebuffer::i()) -> 'ok'.
bindFramebuffer(Target,Framebuffer) when is_integer(Target),is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Framebuffer,5551),
  ok.

-spec deleteFramebuffers(Framebuffers::[i()]) -> 'ok'.
deleteFramebuffers(Framebuffers) when is_list(Framebuffers) ->
  IF = get_interface(),
  N = length(Framebuffers),
  IF:queue_cmd(N,Framebuffers,5552),
  ok.

-spec genFramebuffers(N::i()) -> [i()].
genFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5553),
  rec(5553).

-spec checkFramebufferStatus(Target::enum()) -> enum().
checkFramebufferStatus(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5554),
  rec(5554).

-spec framebufferTexture1D(Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5555),
  ok.

-spec framebufferTexture2D(Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5556),
  ok.

-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok'
    when Target::enum(), Attachment::enum(), Textarget::enum(), Texture::i(), Level::i(), Zoffset::i().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level),is_integer(Zoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,Zoffset,5557),
  ok.

-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok'
    when Target::enum(), Attachment::enum(), Renderbuffertarget::enum(), Renderbuffer::i().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) when is_integer(Target),is_integer(Attachment),is_integer(Renderbuffertarget),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Renderbuffertarget,Renderbuffer,5558),
  ok.

-spec getFramebufferAttachmentParameteriv(Target::enum(), Attachment::enum(), Pname::enum()) -> i().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) when is_integer(Target),is_integer(Attachment),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Pname,5559),
  rec(5559).

-spec generateMipmap(Target::enum()) -> 'ok'.
generateMipmap(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5560),
  ok.

-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok'
    when SrcX0::i(), SrcY0::i(), SrcX1::i(), SrcY1::i(), DstX0::i(), DstY0::i(), DstX1::i(), DstY1::i(), Mask::i(), Filter::enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) when is_integer(SrcX0),is_integer(SrcY0),is_integer(SrcX1),is_integer(SrcY1),is_integer(DstX0),is_integer(DstY0),is_integer(DstX1),is_integer(DstY1),is_integer(Mask),is_integer(Filter) ->
  IF = get_interface(),
  IF:queue_cmd(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter,5561),
  ok.

-spec renderbufferStorageMultisample(Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,5562),
  ok.

-spec framebufferTextureLayer(Target::enum(), Attachment::enum(), Texture::i(), Level::i(), Layer::i()) -> 'ok'.
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Layer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Layer,5563),
  ok.

-spec flushMappedBufferRange(Target::enum(), Offset::i(), Length::i()) -> 'ok'.
flushMappedBufferRange(Target,Offset,Length) when is_integer(Target),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Length,5564),
  ok.

-spec bindVertexArray(Array::i()) -> 'ok'.
bindVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5565),
  ok.

-spec deleteVertexArrays(Arrays::[i()]) -> 'ok'.
deleteVertexArrays(Arrays) when is_list(Arrays) ->
  IF = get_interface(),
  N = length(Arrays),
  IF:queue_cmd(N,Arrays,5566),
  ok.

-spec genVertexArrays(N::i()) -> [i()].
genVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5567),
  rec(5567).

-spec isVertexArray(Array::i()) -> 0|1.
isVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5568),
  rec(5568).

-spec drawArraysInstanced(Mode::enum(), First::i(), Count::i(), Instancecount::i()) -> 'ok'.
drawArraysInstanced(Mode,First,Count,Instancecount) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,5569),
  ok.

-spec drawElementsInstanced(Mode, Count, Type, Indices, Instancecount) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i().
drawElementsInstanced(Mode,Count,Type,Indices,Instancecount) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,5570),
  ok.

-spec texBuffer(Target::enum(), Internalformat::enum(), Buffer::i()) -> 'ok'.
texBuffer(Target,Internalformat,Buffer) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,5572),
  ok.

-spec primitiveRestartIndex(Index::i()) -> 'ok'.
primitiveRestartIndex(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5573),
  ok.

-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok'
    when ReadTarget::enum(), WriteTarget::enum(), ReadOffset::i(), WriteOffset::i(), Size::i().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) when is_integer(ReadTarget),is_integer(WriteTarget),is_integer(ReadOffset),is_integer(WriteOffset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size,5574),
  ok.

-spec getUniformIndices(Program::i(), UniformNames::[unicode:chardata()]) -> [i()].
getUniformIndices(Program,UniformNames) when is_integer(Program),is_list(UniformNames) ->
  IF = get_interface(),
  UniformNamesTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- UniformNames ],
  UniformCount = length(UniformNames),
  IF:queue_cmd(Program,UniformCount,UniformNamesTemp,5575),
  rec(5575).

-spec getActiveUniformsiv(Program::i(), UniformIndices::[i()], Pname::enum()) -> [i()].
getActiveUniformsiv(Program,UniformIndices,Pname) when is_integer(Program),is_list(UniformIndices),is_integer(Pname) ->
  IF = get_interface(),
  UniformCount = length(UniformIndices),
  IF:queue_cmd(Program,UniformCount,UniformIndices,Pname,5576),
  rec(5576).

-spec getActiveUniformName(Program::i(), UniformIndex::i(), BufSize::i()) -> string().
getActiveUniformName(Program,UniformIndex,BufSize) when is_integer(Program),is_integer(UniformIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformIndex,BufSize,5577),
  rec(5577).

-spec getUniformBlockIndex(Program::i(), UniformBlockName::string()) -> i().
getUniformBlockIndex(Program,UniformBlockName) when is_integer(Program),is_list(UniformBlockName) ->
  IF = get_interface(),
  UniformBlockNameBin = unicode:characters_to_binary([UniformBlockName|[0]]),
  IF:queue_cmd(Program,UniformBlockNameBin,5578),
  rec(5578).

-spec getActiveUniformBlockiv(Program::i(), UniformBlockIndex::i(), Pname::enum(), Params::mem()) -> 'ok'.
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(Pname),is_tuple(Params) orelse is_binary(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,Pname,Params,5579),
  rec(5579).

-spec getActiveUniformBlockName(Program::i(), UniformBlockIndex::i(), BufSize::i()) -> string().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,BufSize,5580),
  rec(5580).

-spec uniformBlockBinding(Program::i(), UniformBlockIndex::i(), UniformBlockBinding::i()) -> 'ok'.
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(UniformBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,UniformBlockBinding,5581),
  ok.

-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Basevertex::i().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Basevertex,5582),
  ok.

-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok'
    when Mode::enum(), Start::i(), End::i(), Count::i(), Type::enum(), Indices::offset()|mem(), Basevertex::i().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,Basevertex,5584),
  ok.

-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Instancecount, Basevertex) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Basevertex::i().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Instancecount,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,5586),
  ok.

-spec provokingVertex(Mode::enum()) -> 'ok'.
provokingVertex(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5588),
  ok.

-spec fenceSync(Condition::enum(), Flags::i()) -> i().
fenceSync(Condition,Flags) when is_integer(Condition),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Condition,Flags,5589),
  rec(5589).

-spec isSync(Sync::i()) -> 0|1.
isSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5590),
  rec(5590).

-spec deleteSync(Sync::i()) -> 'ok'.
deleteSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5591),
  ok.

-spec clientWaitSync(Sync::i(), Flags::i(), Timeout::i()) -> enum().
clientWaitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5592),
  rec(5592).

-spec waitSync(Sync::i(), Flags::i(), Timeout::i()) -> 'ok'.
waitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5593),
  ok.

-spec getInteger64v(Pname::enum()) -> [i()].
getInteger64v(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5594),
  rec(5594).

-spec getSynciv(Sync::i(), Pname::enum(), BufSize::i()) -> [i()].
getSynciv(Sync,Pname,BufSize) when is_integer(Sync),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Pname,BufSize,5595),
  rec(5595).

-spec getInteger64i_v(Target::enum(), Index::i()) -> [i()].
getInteger64i_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5596),
  rec(5596).

-spec getBufferParameteri64v(Target::enum(), Pname::enum()) -> [i()].
getBufferParameteri64v(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5597),
  rec(5597).

-spec framebufferTexture(Target::enum(), Attachment::enum(), Texture::i(), Level::i()) -> 'ok'.
framebufferTexture(Target,Attachment,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,5598),
  ok.

-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Fixedsamplelocations::0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5599),
  ok.

-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Fixedsamplelocations::0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5600),
  ok.

-spec getMultisamplefv(Pname::enum(), Index::i()) -> {f(),f()}.
getMultisamplefv(Pname,Index) when is_integer(Pname),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Index,5601),
  rec(5601).

-spec sampleMaski(MaskNumber::i(), Mask::i()) -> 'ok'.
sampleMaski(MaskNumber,Mask) when is_integer(MaskNumber),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(MaskNumber,Mask,5602),
  ok.

-spec bindFragDataLocationIndexed(Program::i(), ColorNumber::i(), Index::i(), Name::string()) -> 'ok'.
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) when is_integer(Program),is_integer(ColorNumber),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ColorNumber,Index,NameBin,5603),
  ok.

-spec getFragDataIndex(Program::i(), Name::string()) -> i().
getFragDataIndex(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5604),
  rec(5604).

-spec genSamplers(Count::i()) -> [i()].
genSamplers(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5605),
  rec(5605).

-spec deleteSamplers(Samplers::[i()]) -> 'ok'.
deleteSamplers(Samplers) when is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(Count,Samplers,5606),
  ok.

-spec isSampler(Sampler::i()) -> 0|1.
isSampler(Sampler) when is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,5607),
  rec(5607).

-spec bindSampler(Unit::i(), Sampler::i()) -> 'ok'.
bindSampler(Unit,Sampler) when is_integer(Unit),is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Sampler,5608),
  ok.

-spec samplerParameteri(Sampler::i(), Pname::enum(), Param::i()) -> 'ok'.
samplerParameteri(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5609),
  ok.

-spec samplerParameteriv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameteriv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5610),
  ok.

-spec samplerParameterf(Sampler::i(), Pname::enum(), Param::f()) -> 'ok'.
samplerParameterf(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5611),
  ok.

-spec samplerParameterfv(Sampler::i(), Pname::enum(), Param::[f()]) -> 'ok'.
samplerParameterfv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5612),
  ok.

-spec samplerParameterIiv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameterIiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5613),
  ok.

-spec samplerParameterIuiv(Sampler::i(), Pname::enum(), Param::[i()]) -> 'ok'.
samplerParameterIuiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5614),
  ok.

-spec getSamplerParameteriv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameteriv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5615),
  rec(5615).

-spec getSamplerParameterIiv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameterIiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5616),
  rec(5616).

-spec getSamplerParameterfv(Sampler::i(), Pname::enum()) -> [f()].
getSamplerParameterfv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5617),
  rec(5617).

-spec getSamplerParameterIuiv(Sampler::i(), Pname::enum()) -> [i()].
getSamplerParameterIuiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5618),
  rec(5618).

-spec queryCounter(Id::i(), Target::enum()) -> 'ok'.
queryCounter(Id,Target) when is_integer(Id),is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Target,5619),
  ok.

-spec getQueryObjecti64v(Id::i(), Pname::enum()) -> i().
getQueryObjecti64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5620),
  rec(5620).

-spec getQueryObjectui64v(Id::i(), Pname::enum()) -> i().
getQueryObjectui64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5621),
  rec(5621).

-spec vertexAttribDivisor(Index::i(), Divisor::i()) -> 'ok'.
vertexAttribDivisor(Index,Divisor) when is_integer(Index),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Divisor,5622),
  ok.

-spec minSampleShading(Value::f()) -> 'ok'.
minSampleShading(Value) when is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Value,5623),
  ok.

-spec blendEquationi(Buf::i(), Mode::enum()) -> 'ok'.
blendEquationi(Buf,Mode) when is_integer(Buf),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Mode,5624),
  ok.

-spec blendEquationSeparatei(Buf::i(), ModeRGB::enum(), ModeAlpha::enum()) -> 'ok'.
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) when is_integer(Buf),is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,ModeRGB,ModeAlpha,5625),
  ok.

-spec blendFunci(Buf::i(), Src::enum(), Dst::enum()) -> 'ok'.
blendFunci(Buf,Src,Dst) when is_integer(Buf),is_integer(Src),is_integer(Dst) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Src,Dst,5626),
  ok.

-spec blendFuncSeparatei(Buf::i(), SrcRGB::enum(), DstRGB::enum(), SrcAlpha::enum(), DstAlpha::enum()) -> 'ok'.
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) when is_integer(Buf),is_integer(SrcRGB),is_integer(DstRGB),is_integer(SrcAlpha),is_integer(DstAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha,5627),
  ok.

-spec drawArraysIndirect(Mode::enum(), Indirect::offset()|mem()) -> 'ok'.
drawArraysIndirect(Mode,Indirect) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,5628),
  ok.

-spec drawElementsIndirect(Mode::enum(), Type::enum(), Indirect::offset()|mem()) -> 'ok'.
drawElementsIndirect(Mode,Type,Indirect) when is_integer(Mode),is_integer(Type),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Type,Indirect,5630),
  ok.

-spec uniform1d(Location::i(), X::f()) -> 'ok'.
uniform1d(Location,X) when is_integer(Location),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5632),
  ok.

-spec uniform2d(Location::i(), X::f(), Y::f()) -> 'ok'.
uniform2d(Location,X,Y) when is_integer(Location),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5633),
  ok.

-spec uniform3d(Location::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
uniform3d(Location,X,Y,Z) when is_integer(Location),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5634),
  ok.

-spec uniform4d(Location::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
uniform4d(Location,X,Y,Z,W) when is_integer(Location),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5635),
  ok.

-spec uniform1dv(Location::i(), Value::[f()]) -> 'ok'.
uniform1dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5636),
  ok.

-spec uniform2dv(Location::i(), Value::[{f(),f()}]) -> 'ok'.
uniform2dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5637),
  ok.

-spec uniform3dv(Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
uniform3dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5638),
  ok.

-spec uniform4dv(Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniform4dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5639),
  ok.

-spec uniformMatrix2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5640),
  ok.

-spec uniformMatrix3dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5641),
  ok.

-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5642),
  ok.

-spec uniformMatrix2x3dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5643),
  ok.

-spec uniformMatrix2x4dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix2x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5644),
  ok.

-spec uniformMatrix3x2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix3x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5645),
  ok.

-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix3x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5646),
  ok.

-spec uniformMatrix4x2dv(Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}]) -> 'ok'.
uniformMatrix4x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5647),
  ok.

-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok'
    when Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
uniformMatrix4x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5648),
  ok.

-spec getUniformdv(Program::i(), Location::i()) -> matrix().
getUniformdv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5649),
  rec(5649).

-spec getSubroutineUniformLocation(Program::i(), Shadertype::enum(), Name::string()) -> i().
getSubroutineUniformLocation(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5650),
  rec(5650).

-spec getSubroutineIndex(Program::i(), Shadertype::enum(), Name::string()) -> i().
getSubroutineIndex(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5651),
  rec(5651).

-spec getActiveSubroutineUniformName(Program::i(), Shadertype::enum(), Index::i(), Bufsize::i()) -> string().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5652),
  rec(5652).

-spec getActiveSubroutineName(Program::i(), Shadertype::enum(), Index::i(), Bufsize::i()) -> string().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5653),
  rec(5653).

-spec uniformSubroutinesuiv(Shadertype::enum(), Indices::[i()]) -> 'ok'.
uniformSubroutinesuiv(Shadertype,Indices) when is_integer(Shadertype),is_list(Indices) ->
  IF = get_interface(),
  Count = length(Indices),
  IF:queue_cmd(Shadertype,Count,Indices,5654),
  ok.

-spec getUniformSubroutineuiv(Shadertype::enum(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformSubroutineuiv(Shadertype,Location) when is_integer(Shadertype),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Location,5655),
  rec(5655).

-spec getProgramStageiv(Program::i(), Shadertype::enum(), Pname::enum()) -> i().
getProgramStageiv(Program,Shadertype,Pname) when is_integer(Program),is_integer(Shadertype),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Pname,5656),
  rec(5656).

-spec patchParameteri(Pname::enum(), Value::i()) -> 'ok'.
patchParameteri(Pname,Value) when is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Value,5657),
  ok.

-spec patchParameterfv(Pname::enum(), Values::[f()]) -> 'ok'.
patchParameterfv(Pname,Values) when is_integer(Pname),is_list(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Values,5658),
  ok.

-spec bindTransformFeedback(Target::enum(), Id::i()) -> 'ok'.
bindTransformFeedback(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5659),
  ok.

-spec deleteTransformFeedbacks(Ids::[i()]) -> 'ok'.
deleteTransformFeedbacks(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5660),
  ok.

-spec genTransformFeedbacks(N::i()) -> [i()].
genTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5661),
  rec(5661).

-spec isTransformFeedback(Id::i()) -> 0|1.
isTransformFeedback(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5662),
  rec(5662).

-spec pauseTransformFeedback() -> 'ok'.
pauseTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5663),
  ok.

-spec resumeTransformFeedback() -> 'ok'.
resumeTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5664),
  ok.

-spec drawTransformFeedback(Mode::enum(), Id::i()) -> 'ok'.
drawTransformFeedback(Mode,Id) when is_integer(Mode),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,5665),
  ok.

-spec drawTransformFeedbackStream(Mode::enum(), Id::i(), Stream::i()) -> 'ok'.
drawTransformFeedbackStream(Mode,Id,Stream) when is_integer(Mode),is_integer(Id),is_integer(Stream) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,5666),
  ok.

-spec beginQueryIndexed(Target::enum(), Index::i(), Id::i()) -> 'ok'.
beginQueryIndexed(Target,Index,Id) when is_integer(Target),is_integer(Index),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Id,5667),
  ok.

-spec endQueryIndexed(Target::enum(), Index::i()) -> 'ok'.
endQueryIndexed(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5668),
  ok.

-spec getQueryIndexediv(Target::enum(), Index::i(), Pname::enum()) -> i().
getQueryIndexediv(Target,Index,Pname) when is_integer(Target),is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Pname,5669),
  rec(5669).

-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler()  ->
  IF = get_interface(),
  IF:queue_cmd(5670),
  ok.

-spec shaderBinary(Shaders::[i()], Binaryformat::enum(), Binary::binary()) -> 'ok'.
shaderBinary(Shaders,Binaryformat,Binary) when is_list(Shaders),is_integer(Binaryformat),is_binary(Binary) ->
  IF = get_interface(),
  Count = length(Shaders),
  IF:queue_cmd(Count,Shaders,Binaryformat,Binary,5671),
  ok.

-spec getShaderPrecisionFormat(Shadertype::enum(), Precisiontype::enum()) -> {Range::{i(),i()},Precision::i()}.
getShaderPrecisionFormat(Shadertype,Precisiontype) when is_integer(Shadertype),is_integer(Precisiontype) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Precisiontype,5672),
  rec(5672).

-spec depthRangef(N::f(), F::f()) -> 'ok'.
depthRangef(N,F) when is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(N,F,5673),
  ok.

-spec clearDepthf(D::f()) -> 'ok'.
clearDepthf(D) when is_float(D) ->
  IF = get_interface(),
  IF:queue_cmd(D,5674),
  ok.

-spec getProgramBinary(Program::i(), BufSize::i()) -> {BinaryFormat::enum(),Binary::binary()}.
getProgramBinary(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5675),
  rec(5675).

-spec programBinary(Program::i(), BinaryFormat::enum(), Binary::binary()) -> 'ok'.
programBinary(Program,BinaryFormat,Binary) when is_integer(Program),is_integer(BinaryFormat),is_binary(Binary) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BinaryFormat,Binary,5676),
  ok.

-spec programParameteri(Program::i(), Pname::enum(), Value::i()) -> 'ok'.
programParameteri(Program,Pname,Value) when is_integer(Program),is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,Value,5677),
  ok.

-spec useProgramStages(Pipeline::i(), Stages::i(), Program::i()) -> 'ok'.
useProgramStages(Pipeline,Stages,Program) when is_integer(Pipeline),is_integer(Stages),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Stages,Program,5678),
  ok.

-spec activeShaderProgram(Pipeline::i(), Program::i()) -> 'ok'.
activeShaderProgram(Pipeline,Program) when is_integer(Pipeline),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Program,5679),
  ok.

-spec createShaderProgramv(Type::enum(), Strings::[unicode:chardata()]) -> i().
createShaderProgramv(Type,Strings) when is_integer(Type),is_list(Strings) ->
  IF = get_interface(),
  StringsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Strings ],
  Count = length(Strings),
  IF:queue_cmd(Type,Count,StringsTemp,5680),
  rec(5680).

-spec bindProgramPipeline(Pipeline::i()) -> 'ok'.
bindProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5681),
  ok.

-spec deleteProgramPipelines(Pipelines::[i()]) -> 'ok'.
deleteProgramPipelines(Pipelines) when is_list(Pipelines) ->
  IF = get_interface(),
  N = length(Pipelines),
  IF:queue_cmd(N,Pipelines,5682),
  ok.

-spec genProgramPipelines(N::i()) -> [i()].
genProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5683),
  rec(5683).

-spec isProgramPipeline(Pipeline::i()) -> 0|1.
isProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5684),
  rec(5684).

-spec getProgramPipelineiv(Pipeline::i(), Pname::enum()) -> i().
getProgramPipelineiv(Pipeline,Pname) when is_integer(Pipeline),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Pname,5685),
  rec(5685).

-spec programUniform1i(Program::i(), Location::i(), V0::i()) -> 'ok'.
programUniform1i(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5686),
  ok.

-spec programUniform1iv(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5687),
  ok.

-spec programUniform1f(Program::i(), Location::i(), V0::f()) -> 'ok'.
programUniform1f(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5688),
  ok.

-spec programUniform1fv(Program::i(), Location::i(), Value::[f()]) -> 'ok'.
programUniform1fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5689),
  ok.

-spec programUniform1d(Program::i(), Location::i(), V0::f()) -> 'ok'.
programUniform1d(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5690),
  ok.

-spec programUniform1dv(Program::i(), Location::i(), Value::[f()]) -> 'ok'.
programUniform1dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5691),
  ok.

-spec programUniform1ui(Program::i(), Location::i(), V0::i()) -> 'ok'.
programUniform1ui(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5692),
  ok.

-spec programUniform1uiv(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5693),
  ok.

-spec programUniform2i(Program::i(), Location::i(), V0::i(), V1::i()) -> 'ok'.
programUniform2i(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5694),
  ok.

-spec programUniform2iv(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5695),
  ok.

-spec programUniform2f(Program::i(), Location::i(), V0::f(), V1::f()) -> 'ok'.
programUniform2f(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5696),
  ok.

-spec programUniform2fv(Program::i(), Location::i(), Value::[{f(),f()}]) -> 'ok'.
programUniform2fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5697),
  ok.

-spec programUniform2d(Program::i(), Location::i(), V0::f(), V1::f()) -> 'ok'.
programUniform2d(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5698),
  ok.

-spec programUniform2dv(Program::i(), Location::i(), Value::[{f(),f()}]) -> 'ok'.
programUniform2dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5699),
  ok.

-spec programUniform2ui(Program::i(), Location::i(), V0::i(), V1::i()) -> 'ok'.
programUniform2ui(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5700),
  ok.

-spec programUniform2uiv(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5701),
  ok.

-spec programUniform3i(Program::i(), Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
programUniform3i(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5702),
  ok.

-spec programUniform3iv(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5703),
  ok.

-spec programUniform3f(Program::i(), Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
programUniform3f(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5704),
  ok.

-spec programUniform3fv(Program::i(), Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
programUniform3fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5705),
  ok.

-spec programUniform3d(Program::i(), Location::i(), V0::f(), V1::f(), V2::f()) -> 'ok'.
programUniform3d(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5706),
  ok.

-spec programUniform3dv(Program::i(), Location::i(), Value::[{f(),f(),f()}]) -> 'ok'.
programUniform3dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5707),
  ok.

-spec programUniform3ui(Program::i(), Location::i(), V0::i(), V1::i(), V2::i()) -> 'ok'.
programUniform3ui(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5708),
  ok.

-spec programUniform3uiv(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5709),
  ok.

-spec programUniform4i(Program::i(), Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
programUniform4i(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5710),
  ok.

-spec programUniform4iv(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5711),
  ok.

-spec programUniform4f(Program::i(), Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
programUniform4f(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5712),
  ok.

-spec programUniform4fv(Program::i(), Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniform4fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5713),
  ok.

-spec programUniform4d(Program::i(), Location::i(), V0::f(), V1::f(), V2::f(), V3::f()) -> 'ok'.
programUniform4d(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5714),
  ok.

-spec programUniform4dv(Program::i(), Location::i(), Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniform4dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5715),
  ok.

-spec programUniform4ui(Program::i(), Location::i(), V0::i(), V1::i(), V2::i(), V3::i()) -> 'ok'.
programUniform4ui(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5716),
  ok.

-spec programUniform4uiv(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5717),
  ok.

-spec programUniformMatrix2fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5718),
  ok.

-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5719),
  ok.

-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5720),
  ok.

-spec programUniformMatrix2dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5721),
  ok.

-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5722),
  ok.

-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5723),
  ok.

-spec programUniformMatrix2x3fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5724),
  ok.

-spec programUniformMatrix3x2fv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix3x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5725),
  ok.

-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5726),
  ok.

-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5727),
  ok.

-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5728),
  ok.

-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5729),
  ok.

-spec programUniformMatrix2x3dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix2x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5730),
  ok.

-spec programUniformMatrix3x2dv(Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f()}]) -> 'ok'.
programUniformMatrix3x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5731),
  ok.

-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5732),
  ok.

-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5733),
  ok.

-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5734),
  ok.

-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok'
    when Program::i(), Location::i(), Transpose::0|1, Value::[{f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f(),f()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5735),
  ok.

-spec validateProgramPipeline(Pipeline::i()) -> 'ok'.
validateProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5736),
  ok.

-spec getProgramPipelineInfoLog(Pipeline::i(), BufSize::i()) -> string().
getProgramPipelineInfoLog(Pipeline,BufSize) when is_integer(Pipeline),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,BufSize,5737),
  rec(5737).

-spec vertexAttribL1d(Index::i(), X::f()) -> 'ok'.
vertexAttribL1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5738),
  ok.

-spec vertexAttribL2d(Index::i(), X::f(), Y::f()) -> 'ok'.
vertexAttribL2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5739),
  ok.

-spec vertexAttribL3d(Index::i(), X::f(), Y::f(), Z::f()) -> 'ok'.
vertexAttribL3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5740),
  ok.

-spec vertexAttribL4d(Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
vertexAttribL4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5741),
  ok.

-spec vertexAttribL1dv(Index::i(), {X::f()}) -> 'ok'.
vertexAttribL1dv(Index,{X}) ->  vertexAttribL1d(Index,X).
-spec vertexAttribL2dv(Index::i(), {X::f(), Y::f()}) -> 'ok'.
vertexAttribL2dv(Index,{X,Y}) ->  vertexAttribL2d(Index,X,Y).
-spec vertexAttribL3dv(Index::i(), {X::f(), Y::f(), Z::f()}) -> 'ok'.
vertexAttribL3dv(Index,{X,Y,Z}) ->  vertexAttribL3d(Index,X,Y,Z).
-spec vertexAttribL4dv(Index::i(), {X::f(), Y::f(), Z::f(), W::f()}) -> 'ok'.
vertexAttribL4dv(Index,{X,Y,Z,W}) ->  vertexAttribL4d(Index,X,Y,Z,W).
-spec vertexAttribLPointer(Index::i(), Size::i(), Type::enum(), Stride::i(), Pointer::offset()|mem()) -> 'ok'.
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5742),
  ok.

-spec getVertexAttribLdv(Index::i(), Pname::enum()) -> {f(),f(),f(),f()}.
getVertexAttribLdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5744),
  rec(5744).

-spec viewportArrayv(First::i(), V::[{f(),f(),f(),f()}]) -> 'ok'.
viewportArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5745),
  ok.

-spec viewportIndexedf(Index::i(), X::f(), Y::f(), W::f(), H::f()) -> 'ok'.
viewportIndexedf(Index,X,Y,W,H) when is_integer(Index),is_float(X),is_float(Y),is_float(W),is_float(H) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,W,H,5746),
  ok.

-spec viewportIndexedfv(Index::i(), V::{f(),f(),f(),f()}) -> 'ok'.
viewportIndexedfv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5747),
  ok.

-spec scissorArrayv(First::i(), V::[{i(),i(),i(),i()}]) -> 'ok'.
scissorArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5748),
  ok.

-spec scissorIndexed(Index::i(), Left::i(), Bottom::i(), Width::i(), Height::i()) -> 'ok'.
scissorIndexed(Index,Left,Bottom,Width,Height) when is_integer(Index),is_integer(Left),is_integer(Bottom),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Left,Bottom,Width,Height,5749),
  ok.

-spec scissorIndexedv(Index::i(), V::{i(),i(),i(),i()}) -> 'ok'.
scissorIndexedv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5750),
  ok.

-spec depthRangeArrayv(First::i(), V::[{f(),f()}]) -> 'ok'.
depthRangeArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5751),
  ok.

-spec depthRangeIndexed(Index::i(), N::f(), F::f()) -> 'ok'.
depthRangeIndexed(Index,N,F) when is_integer(Index),is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(Index,N,F,5752),
  ok.

-spec getFloati_v(Target::enum(), Index::i()) -> [f()].
getFloati_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5753),
  rec(5753).

-spec getDoublei_v(Target::enum(), Index::i()) -> [f()].
getDoublei_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5754),
  rec(5754).

-spec drawArraysInstancedBaseInstance(Mode::enum(), First::i(), Count::i(), Instancecount::i(), Baseinstance::i()) -> 'ok'.
drawArraysInstancedBaseInstance(Mode,First,Count,Instancecount,Baseinstance) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,Baseinstance,5755),
  ok.

-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Instancecount, Baseinstance) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Baseinstance::i().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Instancecount,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Baseinstance,5756),
  ok.

-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Instancecount, Basevertex, Baseinstance) -> 'ok'
    when Mode::enum(), Count::i(), Type::enum(), Indices::offset()|mem(), Instancecount::i(), Basevertex::i(), Baseinstance::i().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance,5758),
  ok.

-spec getInternalformativ(Target::enum(), Internalformat::enum(), Pname::enum(), BufSize::i()) -> [i()].
getInternalformativ(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5760),
  rec(5760).

-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok'
    when Unit::i(), Texture::i(), Level::i(), Layered::0|1, Layer::i(), Access::enum(), Format::enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) when is_integer(Unit),is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Access),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,Level,Layered,Layer,Access,Format,5761),
  ok.

-spec memoryBarrier(Barriers::i()) -> 'ok'.
memoryBarrier(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5762),
  ok.

-spec texStorage1D(Target::enum(), Levels::i(), Internalformat::enum(), Width::i()) -> 'ok'.
texStorage1D(Target,Levels,Internalformat,Width) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,5763),
  ok.

-spec texStorage2D(Target::enum(), Levels::i(), Internalformat::enum(), Width::i(), Height::i()) -> 'ok'.
texStorage2D(Target,Levels,Internalformat,Width,Height) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,5764),
  ok.

-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok'
    when Target::enum(), Levels::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,Depth,5765),
  ok.

-spec drawTransformFeedbackInstanced(Mode::enum(), Id::i(), Instancecount::i()) -> 'ok'.
drawTransformFeedbackInstanced(Mode,Id,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Instancecount,5766),
  ok.

-spec drawTransformFeedbackStreamInstanced(Mode::enum(), Id::i(), Stream::i(), Instancecount::i()) -> 'ok'.
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Stream),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,Instancecount,5767),
  ok.

-spec clearBufferData(Target, Internalformat, Format, Type, Data) -> 'ok'
    when Target::enum(), Internalformat::enum(), Format::enum(), Type::enum(), Data::offset()|mem().
clearBufferData(Target,Internalformat,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Format,Type,Data,5768),
  ok.

-spec clearBufferSubData(Target, Internalformat, Offset, Size, Format, Type, Data) -> 'ok'
    when Target::enum(), Internalformat::enum(), Offset::i(), Size::i(), Format::enum(), Type::enum(), Data::offset()|mem().
clearBufferSubData(Target,Internalformat,Offset,Size,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Offset),is_integer(Size),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Offset,Size,Format,Type,Data,5770),
  ok.

-spec dispatchCompute(Num_groups_x::i(), Num_groups_y::i(), Num_groups_z::i()) -> 'ok'.
dispatchCompute(Num_groups_x,Num_groups_y,Num_groups_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,5772),
  ok.

-spec dispatchComputeIndirect(Indirect::i()) -> 'ok'.
dispatchComputeIndirect(Indirect) when is_integer(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Indirect,5773),
  ok.

-spec copyImageSubData(SrcName, SrcTarget, SrcLevel, SrcX, SrcY, SrcZ, DstName, DstTarget, DstLevel, DstX, DstY, DstZ, SrcWidth, SrcHeight, SrcDepth) -> 'ok'
    when SrcName::i(), SrcTarget::enum(), SrcLevel::i(), SrcX::i(), SrcY::i(), SrcZ::i(), DstName::i(), DstTarget::enum(), DstLevel::i(), DstX::i(), DstY::i(), DstZ::i(), SrcWidth::i(), SrcHeight::i(), SrcDepth::i().
copyImageSubData(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth) when is_integer(SrcName),is_integer(SrcTarget),is_integer(SrcLevel),is_integer(SrcX),is_integer(SrcY),is_integer(SrcZ),is_integer(DstName),is_integer(DstTarget),is_integer(DstLevel),is_integer(DstX),is_integer(DstY),is_integer(DstZ),is_integer(SrcWidth),is_integer(SrcHeight),is_integer(SrcDepth) ->
  IF = get_interface(),
  IF:queue_cmd(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth,5774),
  ok.

-spec framebufferParameteri(Target::enum(), Pname::enum(), Param::i()) -> 'ok'.
framebufferParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5775),
  ok.

-spec getFramebufferParameteriv(Target::enum(), Pname::enum()) -> i().
getFramebufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5776),
  rec(5776).

-spec getInternalformati64v(Target::enum(), Internalformat::enum(), Pname::enum(), BufSize::i()) -> [i()].
getInternalformati64v(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5777),
  rec(5777).

-spec invalidateTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i().
invalidateTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,5778),
  ok.

-spec invalidateTexImage(Texture::i(), Level::i()) -> 'ok'.
invalidateTexImage(Texture,Level) when is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,5779),
  ok.

-spec invalidateBufferSubData(Buffer::i(), Offset::i(), Length::i()) -> 'ok'.
invalidateBufferSubData(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5780),
  ok.

-spec invalidateBufferData(Buffer::i()) -> 'ok'.
invalidateBufferData(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5781),
  ok.

-spec invalidateFramebuffer(Target::enum(), Attachments::[enum()]) -> 'ok'.
invalidateFramebuffer(Target,Attachments) when is_integer(Target),is_list(Attachments) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,5782),
  ok.

-spec invalidateSubFramebuffer(Target::enum(), Attachments::[enum()], X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
invalidateSubFramebuffer(Target,Attachments,X,Y,Width,Height) when is_integer(Target),is_list(Attachments),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,X,Y,Width,Height,5783),
  ok.

-spec multiDrawArraysIndirect(Mode::enum(), Indirect::offset()|mem(), Drawcount::i(), Stride::i()) -> 'ok'.
multiDrawArraysIndirect(Mode,Indirect,Drawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Stride,5784),
  ok.

-spec getProgramInterfaceiv(Program::i(), ProgramInterface::enum(), Pname::enum()) -> i().
getProgramInterfaceiv(Program,ProgramInterface,Pname) when is_integer(Program),is_integer(ProgramInterface),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Pname,5786),
  rec(5786).

-spec getProgramResourceIndex(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5787),
  rec(5787).

-spec getProgramResourceName(Program::i(), ProgramInterface::enum(), Index::i(), BufSize::i()) -> string().
getProgramResourceName(Program,ProgramInterface,Index,BufSize) when is_integer(Program),is_integer(ProgramInterface),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Index,BufSize,5788),
  rec(5788).

-spec getProgramResourceLocation(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceLocation(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5789),
  rec(5789).

-spec getProgramResourceLocationIndex(Program::i(), ProgramInterface::enum(), Name::string()) -> i().
getProgramResourceLocationIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5790),
  rec(5790).

-spec shaderStorageBlockBinding(Program::i(), StorageBlockIndex::i(), StorageBlockBinding::i()) -> 'ok'.
shaderStorageBlockBinding(Program,StorageBlockIndex,StorageBlockBinding) when is_integer(Program),is_integer(StorageBlockIndex),is_integer(StorageBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,StorageBlockIndex,StorageBlockBinding,5791),
  ok.

-spec texBufferRange(Target::enum(), Internalformat::enum(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
texBufferRange(Target,Internalformat,Buffer,Offset,Size) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,Offset,Size,5792),
  ok.

-spec texStorage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Fixedsamplelocations::0|1.
texStorage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5793),
  ok.

-spec texStorage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok'
    when Target::enum(), Samples::i(), Internalformat::enum(), Width::i(), Height::i(), Depth::i(), Fixedsamplelocations::0|1.
texStorage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5794),
  ok.

-spec textureView(Texture, Target, Origtexture, Internalformat, Minlevel, Numlevels, Minlayer, Numlayers) -> 'ok'
    when Texture::i(), Target::enum(), Origtexture::i(), Internalformat::enum(), Minlevel::i(), Numlevels::i(), Minlayer::i(), Numlayers::i().
textureView(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers) when is_integer(Texture),is_integer(Target),is_integer(Origtexture),is_integer(Internalformat),is_integer(Minlevel),is_integer(Numlevels),is_integer(Minlayer),is_integer(Numlayers) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers,5795),
  ok.

-spec bindVertexBuffer(Bindingindex::i(), Buffer::i(), Offset::i(), Stride::i()) -> 'ok'.
bindVertexBuffer(Bindingindex,Buffer,Offset,Stride) when is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Buffer,Offset,Stride,5796),
  ok.

-spec vertexAttribFormat(Attribindex::i(), Size::i(), Type::enum(), Normalized::0|1, Relativeoffset::i()) -> 'ok'.
vertexAttribFormat(Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Normalized,Relativeoffset,5797),
  ok.

-spec vertexAttribIFormat(Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexAttribIFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5798),
  ok.

-spec vertexAttribLFormat(Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexAttribLFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5799),
  ok.

-spec vertexAttribBinding(Attribindex::i(), Bindingindex::i()) -> 'ok'.
vertexAttribBinding(Attribindex,Bindingindex) when is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Bindingindex,5800),
  ok.

-spec vertexBindingDivisor(Bindingindex::i(), Divisor::i()) -> 'ok'.
vertexBindingDivisor(Bindingindex,Divisor) when is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Divisor,5801),
  ok.

-spec debugMessageControl(Source::enum(), Type::enum(), Severity::enum(), Ids::[i()], Enabled::0|1) -> 'ok'.
debugMessageControl(Source,Type,Severity,Ids,Enabled) when is_integer(Source),is_integer(Type),is_integer(Severity),is_list(Ids),(0 =:= Enabled) orelse (1 =:= Enabled) ->
  IF = get_interface(),
  Count = length(Ids),
  IF:queue_cmd(Source,Type,Severity,Count,Ids,Enabled,5802),
  ok.

-spec debugMessageInsert(Source, Type, Id, Severity, Length, Buf) -> 'ok'
    when Source::enum(), Type::enum(), Id::i(), Severity::enum(), Length::i(), Buf::string().
debugMessageInsert(Source,Type,Id,Severity,Length,Buf) when is_integer(Source),is_integer(Type),is_integer(Id),is_integer(Severity),is_integer(Length),is_list(Buf) ->
  IF = get_interface(),
  BufBin = unicode:characters_to_binary([Buf|[0]]),
  IF:queue_cmd(Source,Type,Id,Severity,Length,BufBin,5803),
  ok.

-spec getDebugMessageLog(Count::i(), BufSize::i()) -> {i(),Sources::[enum()],Types::[enum()],Ids::[i()],Severities::[enum()],MessageLog::string()}.
getDebugMessageLog(Count,BufSize) when is_integer(Count),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Count,BufSize,5804),
  rec(5804).

-spec pushDebugGroup(Source::enum(), Id::i(), Length::i(), Message::string()) -> 'ok'.
pushDebugGroup(Source,Id,Length,Message) when is_integer(Source),is_integer(Id),is_integer(Length),is_list(Message) ->
  IF = get_interface(),
  MessageBin = unicode:characters_to_binary([Message|[0]]),
  IF:queue_cmd(Source,Id,Length,MessageBin,5805),
  ok.

-spec popDebugGroup() -> 'ok'.
popDebugGroup()  ->
  IF = get_interface(),
  IF:queue_cmd(5806),
  ok.

-spec objectPtrLabel(Ptr::offset()|mem(), Length::i(), Label::string()) -> 'ok'.
objectPtrLabel(Ptr,Length,Label) when is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr),is_integer(Length),is_list(Label) ->
  IF = get_interface(),
  LabelBin = unicode:characters_to_binary([Label|[0]]),
  IF:queue_cmd(Ptr,Length,LabelBin,5807),
  ok.

-spec bufferStorage(Target::enum(), Size::i(), Data::offset()|mem(), Flags::i()) -> 'ok'.
bufferStorage(Target,Size,Data,Flags) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Flags,5809),
  ok.

-spec clearTexImage(Texture::i(), Level::i(), Format::enum(), Type::enum(), Data::offset()|mem()) -> 'ok'.
clearTexImage(Texture,Level,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Format,Type,Data,5811),
  ok.

-spec clearTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), Type::enum(), Data::offset()|mem().
clearTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data,5813),
  ok.

-spec bindBuffersBase(Target::enum(), First::i(), Buffers::[i()]) -> 'ok'.
bindBuffersBase(Target,First,Buffers) when is_integer(Target),is_integer(First),is_list(Buffers) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,5815),
  ok.

-spec bindBuffersRange(Target::enum(), First::i(), Buffers::[i()], Offsets::[i()], Sizes::[i()]) -> 'ok'.
bindBuffersRange(Target,First,Buffers,Offsets,Sizes) when is_integer(Target),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Sizes) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,Offsets,Sizes,5816),
  ok.

-spec bindTextures(First::i(), Textures::[i()]) -> 'ok'.
bindTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5817),
  ok.

-spec bindSamplers(First::i(), Samplers::[i()]) -> 'ok'.
bindSamplers(First,Samplers) when is_integer(First),is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(First,Count,Samplers,5818),
  ok.

-spec bindImageTextures(First::i(), Textures::[i()]) -> 'ok'.
bindImageTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5819),
  ok.

-spec bindVertexBuffers(First::i(), Buffers::[i()], Offsets::[i()], Strides::[i()]) -> 'ok'.
bindVertexBuffers(First,Buffers,Offsets,Strides) when is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(First,Count,Buffers,Offsets,Strides,5820),
  ok.

-spec clipControl(Origin::enum(), Depth::enum()) -> 'ok'.
clipControl(Origin,Depth) when is_integer(Origin),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Origin,Depth,5821),
  ok.

-spec createTransformFeedbacks(N::i()) -> [i()].
createTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5822),
  rec(5822).

-spec transformFeedbackBufferBase(Xfb::i(), Index::i(), Buffer::i()) -> 'ok'.
transformFeedbackBufferBase(Xfb,Index,Buffer) when is_integer(Xfb),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,5823),
  ok.

-spec transformFeedbackBufferRange(Xfb::i(), Index::i(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
transformFeedbackBufferRange(Xfb,Index,Buffer,Offset,Size) when is_integer(Xfb),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,Offset,Size,5824),
  ok.

-spec createBuffers(N::i()) -> [i()].
createBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5825),
  rec(5825).

-spec flushMappedNamedBufferRange(Buffer::i(), Offset::i(), Length::i()) -> 'ok'.
flushMappedNamedBufferRange(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5826),
  ok.

-spec createFramebuffers(N::i()) -> [i()].
createFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5827),
  rec(5827).

-spec createRenderbuffers(N::i()) -> [i()].
createRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5828),
  rec(5828).

-spec createTextures(Target::enum(), N::i()) -> [i()].
createTextures(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5829),
  rec(5829).

-spec textureBuffer(Texture::i(), Internalformat::enum(), Buffer::i()) -> 'ok'.
textureBuffer(Texture,Internalformat,Buffer) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,5830),
  ok.

-spec textureBufferRange(Texture::i(), Internalformat::enum(), Buffer::i(), Offset::i(), Size::i()) -> 'ok'.
textureBufferRange(Texture,Internalformat,Buffer,Offset,Size) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,Offset,Size,5831),
  ok.

-spec compressedTextureSubImage1D(Texture, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Width::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage1D(Texture,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Width,Format,ImageSize,Data,5832),
  ok.

-spec compressedTextureSubImage2D(Texture, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Width::i(), Height::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage2D(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5834),
  ok.

-spec compressedTextureSubImage3D(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok'
    when Texture::i(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Format::enum(), ImageSize::i(), Data::offset()|mem().
compressedTextureSubImage3D(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5836),
  ok.

-spec generateTextureMipmap(Texture::i()) -> 'ok'.
generateTextureMipmap(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5838),
  ok.

-spec bindTextureUnit(Unit::i(), Texture::i()) -> 'ok'.
bindTextureUnit(Unit,Texture) when is_integer(Unit),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,5839),
  ok.

-spec createVertexArrays(N::i()) -> [i()].
createVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5840),
  rec(5840).

-spec disableVertexArrayAttrib(Vaobj::i(), Index::i()) -> 'ok'.
disableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5841),
  ok.

-spec enableVertexArrayAttrib(Vaobj::i(), Index::i()) -> 'ok'.
enableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5842),
  ok.

-spec vertexArrayElementBuffer(Vaobj::i(), Buffer::i()) -> 'ok'.
vertexArrayElementBuffer(Vaobj,Buffer) when is_integer(Vaobj),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Buffer,5843),
  ok.

-spec vertexArrayVertexBuffer(Vaobj::i(), Bindingindex::i(), Buffer::i(), Offset::i(), Stride::i()) -> 'ok'.
vertexArrayVertexBuffer(Vaobj,Bindingindex,Buffer,Offset,Stride) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Buffer,Offset,Stride,5844),
  ok.

-spec vertexArrayVertexBuffers(Vaobj::i(), First::i(), Buffers::[i()], Offsets::[i()], Strides::[i()]) -> 'ok'.
vertexArrayVertexBuffers(Vaobj,First,Buffers,Offsets,Strides) when is_integer(Vaobj),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Vaobj,First,Count,Buffers,Offsets,Strides,5845),
  ok.

-spec vertexArrayAttribBinding(Vaobj::i(), Attribindex::i(), Bindingindex::i()) -> 'ok'.
vertexArrayAttribBinding(Vaobj,Attribindex,Bindingindex) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Bindingindex,5846),
  ok.

-spec vertexArrayAttribFormat(Vaobj, Attribindex, Size, Type, Normalized, Relativeoffset) -> 'ok'
    when Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Normalized::0|1, Relativeoffset::i().
vertexArrayAttribFormat(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset,5847),
  ok.

-spec vertexArrayAttribIFormat(Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexArrayAttribIFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5848),
  ok.

-spec vertexArrayAttribLFormat(Vaobj::i(), Attribindex::i(), Size::i(), Type::enum(), Relativeoffset::i()) -> 'ok'.
vertexArrayAttribLFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5849),
  ok.

-spec vertexArrayBindingDivisor(Vaobj::i(), Bindingindex::i(), Divisor::i()) -> 'ok'.
vertexArrayBindingDivisor(Vaobj,Bindingindex,Divisor) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Divisor,5850),
  ok.

-spec createSamplers(N::i()) -> [i()].
createSamplers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5851),
  rec(5851).

-spec createProgramPipelines(N::i()) -> [i()].
createProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5852),
  rec(5852).

-spec createQueries(Target::enum(), N::i()) -> [i()].
createQueries(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5853),
  rec(5853).

-spec getQueryBufferObjecti64v(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjecti64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5854),
  ok.

-spec getQueryBufferObjectiv(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5855),
  ok.

-spec getQueryBufferObjectui64v(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectui64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5856),
  ok.

-spec getQueryBufferObjectuiv(Id::i(), Buffer::i(), Pname::enum(), Offset::i()) -> 'ok'.
getQueryBufferObjectuiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5857),
  ok.

-spec memoryBarrierByRegion(Barriers::i()) -> 'ok'.
memoryBarrierByRegion(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5858),
  ok.

-spec getGraphicsResetStatus() -> enum().
getGraphicsResetStatus()  ->
  IF = get_interface(),
  IF:queue_cmd(5859),
  rec(5859).

-spec textureBarrier() -> 'ok'.
textureBarrier()  ->
  IF = get_interface(),
  IF:queue_cmd(5860),
  ok.

-spec multiDrawArraysIndirectCount(Mode, Indirect, Drawcount, Maxdrawcount, Stride) -> 'ok'
    when Mode::enum(), Indirect::offset()|mem(), Drawcount::i(), Maxdrawcount::i(), Stride::i().
multiDrawArraysIndirectCount(Mode,Indirect,Drawcount,Maxdrawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Maxdrawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Maxdrawcount,Stride,5861),
  ok.

-spec polygonOffsetClamp(Factor::f(), Units::f(), Clamp::f()) -> 'ok'.
polygonOffsetClamp(Factor,Units,Clamp) when is_float(Factor),is_float(Units),is_float(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,Clamp,5863),
  ok.

-spec primitiveBoundingBoxARB(MinX, MinY, MinZ, MinW, MaxX, MaxY, MaxZ, MaxW) -> 'ok'
    when MinX::f(), MinY::f(), MinZ::f(), MinW::f(), MaxX::f(), MaxY::f(), MaxZ::f(), MaxW::f().
primitiveBoundingBoxARB(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW) when is_float(MinX),is_float(MinY),is_float(MinZ),is_float(MinW),is_float(MaxX),is_float(MaxY),is_float(MaxZ),is_float(MaxW) ->
  IF = get_interface(),
  IF:queue_cmd(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW,5864),
  ok.

-spec makeTextureHandleResidentARB(Handle::i()) -> 'ok'.
makeTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5865),
  ok.

-spec makeTextureHandleNonResidentARB(Handle::i()) -> 'ok'.
makeTextureHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5866),
  ok.

-spec getImageHandleARB(Texture::i(), Level::i(), Layered::0|1, Layer::i(), Format::enum()) -> i().
getImageHandleARB(Texture,Level,Layered,Layer,Format) when is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Layered,Layer,Format,5867),
  rec(5867).

-spec makeImageHandleResidentARB(Handle::i(), Access::enum()) -> 'ok'.
makeImageHandleResidentARB(Handle,Access) when is_integer(Handle),is_integer(Access) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,Access,5868),
  ok.

-spec makeImageHandleNonResidentARB(Handle::i()) -> 'ok'.
makeImageHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5869),
  ok.

-spec uniformHandleui64ARB(Location::i(), Value::i()) -> 'ok'.
uniformHandleui64ARB(Location,Value) when is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Location,Value,5870),
  ok.

-spec programUniformHandleui64ARB(Program::i(), Location::i(), Value::i()) -> 'ok'.
programUniformHandleui64ARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,Value,5871),
  ok.

-spec isTextureHandleResidentARB(Handle::i()) -> 0|1.
isTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5872),
  rec(5872).

-spec isImageHandleResidentARB(Handle::i()) -> 0|1.
isImageHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5873),
  rec(5873).

-spec dispatchComputeGroupSizeARB(Num_groups_x, Num_groups_y, Num_groups_z, Group_size_x, Group_size_y, Group_size_z) -> 'ok'
    when Num_groups_x::i(), Num_groups_y::i(), Num_groups_z::i(), Group_size_x::i(), Group_size_y::i(), Group_size_z::i().
dispatchComputeGroupSizeARB(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z),is_integer(Group_size_x),is_integer(Group_size_y),is_integer(Group_size_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z,5874),
  ok.

-spec programStringARB(Target::enum(), Format::enum(), String::string()) -> 'ok'.
programStringARB(Target,Format,String) when is_integer(Target),is_integer(Format),is_list(String) ->
  IF = get_interface(),
  StringBin = unicode:characters_to_binary([String|[0]]),
  IF:queue_cmd(Target,Format,StringBin,5875),
  ok.

-spec bindProgramARB(Target::enum(), Program::i()) -> 'ok'.
bindProgramARB(Target,Program) when is_integer(Target),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Program,5876),
  ok.

-spec deleteProgramsARB(Programs::[i()]) -> 'ok'.
deleteProgramsARB(Programs) when is_list(Programs) ->
  IF = get_interface(),
  N = length(Programs),
  IF:queue_cmd(N,Programs,5877),
  ok.

-spec genProgramsARB(N::i()) -> [i()].
genProgramsARB(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5878),
  rec(5878).

-spec programEnvParameter4dARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programEnvParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5879),
  ok.

-spec programEnvParameter4dvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programEnvParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5880),
  ok.

-spec programEnvParameter4fARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programEnvParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5881),
  ok.

-spec programEnvParameter4fvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programEnvParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5882),
  ok.

-spec programLocalParameter4dARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programLocalParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5883),
  ok.

-spec programLocalParameter4dvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programLocalParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5884),
  ok.

-spec programLocalParameter4fARB(Target::enum(), Index::i(), X::f(), Y::f(), Z::f(), W::f()) -> 'ok'.
programLocalParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5885),
  ok.

-spec programLocalParameter4fvARB(Target::enum(), Index::i(), Params::{f(),f(),f(),f()}) -> 'ok'.
programLocalParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5886),
  ok.

-spec getProgramEnvParameterdvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramEnvParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5887),
  rec(5887).

-spec getProgramEnvParameterfvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramEnvParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5888),
  rec(5888).

-spec getProgramLocalParameterdvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramLocalParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5889),
  rec(5889).

-spec getProgramLocalParameterfvARB(Target::enum(), Index::i()) -> {f(),f(),f(),f()}.
getProgramLocalParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5890),
  rec(5890).

-spec getProgramStringARB(Target::enum(), Pname::enum(), String::mem()) -> 'ok'.
getProgramStringARB(Target,Pname,String) when is_integer(Target),is_integer(Pname),is_tuple(String) orelse is_binary(String) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,String,5891),
  rec(5891).

-spec framebufferTextureFaceARB(Target::enum(), Attachment::enum(), Texture::i(), Level::i(), Face::enum()) -> 'ok'.
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Face) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Face,5892),
  ok.

-spec uniform1i64ARB(Location::i(), X::i()) -> 'ok'.
uniform1i64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5893),
  ok.

-spec uniform2i64ARB(Location::i(), X::i(), Y::i()) -> 'ok'.
uniform2i64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5894),
  ok.

-spec uniform3i64ARB(Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
uniform3i64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5895),
  ok.

-spec uniform4i64ARB(Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
uniform4i64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5896),
  ok.

-spec uniform1i64vARB(Location::i(), Value::[i()]) -> 'ok'.
uniform1i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5897),
  ok.

-spec uniform2i64vARB(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5898),
  ok.

-spec uniform3i64vARB(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5899),
  ok.

-spec uniform4i64vARB(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5900),
  ok.

-spec uniform1ui64ARB(Location::i(), X::i()) -> 'ok'.
uniform1ui64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5901),
  ok.

-spec uniform2ui64ARB(Location::i(), X::i(), Y::i()) -> 'ok'.
uniform2ui64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5902),
  ok.

-spec uniform3ui64ARB(Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
uniform3ui64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5903),
  ok.

-spec uniform4ui64ARB(Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
uniform4ui64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5904),
  ok.

-spec uniform1ui64vARB(Location::i(), Value::[i()]) -> 'ok'.
uniform1ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5905),
  ok.

-spec uniform2ui64vARB(Location::i(), Value::[{i(),i()}]) -> 'ok'.
uniform2ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5906),
  ok.

-spec uniform3ui64vARB(Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
uniform3ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5907),
  ok.

-spec uniform4ui64vARB(Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
uniform4ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5908),
  ok.

-spec getUniformi64vARB(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformi64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5909),
  rec(5909).

-spec getUniformui64vARB(Program::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformui64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5910),
  rec(5910).

-spec programUniform1i64ARB(Program::i(), Location::i(), X::i()) -> 'ok'.
programUniform1i64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5911),
  ok.

-spec programUniform2i64ARB(Program::i(), Location::i(), X::i(), Y::i()) -> 'ok'.
programUniform2i64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5912),
  ok.

-spec programUniform3i64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
programUniform3i64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5913),
  ok.

-spec programUniform4i64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
programUniform4i64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5914),
  ok.

-spec programUniform1i64vARB(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5915),
  ok.

-spec programUniform2i64vARB(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5916),
  ok.

-spec programUniform3i64vARB(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5917),
  ok.

-spec programUniform4i64vARB(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5918),
  ok.

-spec programUniform1ui64ARB(Program::i(), Location::i(), X::i()) -> 'ok'.
programUniform1ui64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5919),
  ok.

-spec programUniform2ui64ARB(Program::i(), Location::i(), X::i(), Y::i()) -> 'ok'.
programUniform2ui64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5920),
  ok.

-spec programUniform3ui64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i()) -> 'ok'.
programUniform3ui64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5921),
  ok.

-spec programUniform4ui64ARB(Program::i(), Location::i(), X::i(), Y::i(), Z::i(), W::i()) -> 'ok'.
programUniform4ui64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5922),
  ok.

-spec programUniform1ui64vARB(Program::i(), Location::i(), Value::[i()]) -> 'ok'.
programUniform1ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5923),
  ok.

-spec programUniform2ui64vARB(Program::i(), Location::i(), Value::[{i(),i()}]) -> 'ok'.
programUniform2ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5924),
  ok.

-spec programUniform3ui64vARB(Program::i(), Location::i(), Value::[{i(),i(),i()}]) -> 'ok'.
programUniform3ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5925),
  ok.

-spec programUniform4ui64vARB(Program::i(), Location::i(), Value::[{i(),i(),i(),i()}]) -> 'ok'.
programUniform4ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5926),
  ok.

-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Format::enum(), Type::enum(), Table::offset()|mem().
colorTable(Target,Internalformat,Width,Format,Type,Table) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Table) orelse is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Table,5927),
  ok.

-spec colorTableParameterfv(Target::enum(), Pname::enum(), Params::{f(),f(),f(),f()}) -> 'ok'.
colorTableParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5929),
  ok.

-spec colorTableParameteriv(Target::enum(), Pname::enum(), Params::{i(),i(),i(),i()}) -> 'ok'.
colorTableParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5930),
  ok.

-spec copyColorTable(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyColorTable(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5931),
  ok.

-spec getColorTable(Target::enum(), Format::enum(), Type::enum(), Table::mem()) -> 'ok'.
getColorTable(Target,Format,Type,Table) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Table,5932),
  rec(5932).

-spec getColorTableParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getColorTableParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5933),
  rec(5933).

-spec getColorTableParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getColorTableParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5934),
  rec(5934).

-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> 'ok'
    when Target::enum(), Start::i(), Count::i(), Format::enum(), Type::enum(), Data::offset()|mem().
colorSubTable(Target,Start,Count,Format,Type,Data) when is_integer(Target),is_integer(Start),is_integer(Count),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,Count,Format,Type,Data,5935),
  ok.

-spec copyColorSubTable(Target::enum(), Start::i(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyColorSubTable(Target,Start,X,Y,Width) when is_integer(Target),is_integer(Start),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,X,Y,Width,5937),
  ok.

-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Format::enum(), Type::enum(), Image::offset()|mem().
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Image,5938),
  ok.

-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Height::i(), Format::enum(), Type::enum(), Image::offset()|mem().
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Image,5940),
  ok.

-spec convolutionParameterf(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameterf(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5942),
  ok.

-spec convolutionParameterfv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5943),
  ok.

-spec convolutionParameteri(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameteri(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5944),
  ok.

-spec convolutionParameteriv(Target::enum(), Pname::enum(), Params::tuple()) -> 'ok'.
convolutionParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5945),
  ok.

-spec copyConvolutionFilter1D(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i()) -> 'ok'.
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5946),
  ok.

-spec copyConvolutionFilter2D(Target::enum(), Internalformat::enum(), X::i(), Y::i(), Width::i(), Height::i()) -> 'ok'.
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,Height,5947),
  ok.

-spec getConvolutionFilter(Target::enum(), Format::enum(), Type::enum(), Image::mem()) -> 'ok'.
getConvolutionFilter(Target,Format,Type,Image) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Image,5948),
  rec(5948).

-spec getConvolutionParameterfv(Target::enum(), Pname::enum()) -> {f(),f(),f(),f()}.
getConvolutionParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5949),
  rec(5949).

-spec getConvolutionParameteriv(Target::enum(), Pname::enum()) -> {i(),i(),i(),i()}.
getConvolutionParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5950),
  rec(5950).

-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 'ok'
    when Target::enum(), Internalformat::enum(), Width::i(), Height::i(), Format::enum(), Type::enum(), Row::offset()|mem(), Column::offset()|mem().
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Row) orelse is_tuple(Row) orelse is_binary(Row),is_integer(Column) orelse is_tuple(Column) orelse is_binary(Column) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Row,Column,5951),
  ok.

-spec getHistogram(Target::enum(), Reset::0|1, Format::enum(), Type::enum(), Values::mem()) -> 'ok'.
getHistogram(Target,Reset,Format,Type,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Type),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Type,Values,5953),
  rec(5953).

-spec getHistogramParameterfv(Target::enum(), Pname::enum()) -> {f()}.
getHistogramParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5954),
  rec(5954).

-spec getHistogramParameteriv(Target::enum(), Pname::enum()) -> {i()}.
getHistogramParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5955),
  rec(5955).

-spec getMinmax(Target::enum(), Reset::0|1, Format::enum(), Types::enum(), Values::mem()) -> 'ok'.
getMinmax(Target,Reset,Format,Types,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Types),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Types,Values,5956),
  rec(5956).

-spec getMinmaxParameterfv(Target::enum(), Pname::enum()) -> {f()}.
getMinmaxParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5957),
  rec(5957).

-spec getMinmaxParameteriv(Target::enum(), Pname::enum()) -> {i()}.
getMinmaxParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5958),
  rec(5958).

-spec histogram(Target::enum(), Width::i(), Internalformat::enum(), Sink::0|1) -> 'ok'.
histogram(Target,Width,Internalformat,Sink) when is_integer(Target),is_integer(Width),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Width,Internalformat,Sink,5959),
  ok.

-spec minmax(Target::enum(), Internalformat::enum(), Sink::0|1) -> 'ok'.
minmax(Target,Internalformat,Sink) when is_integer(Target),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Sink,5960),
  ok.

-spec resetHistogram(Target::enum()) -> 'ok'.
resetHistogram(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5961),
  ok.

-spec resetMinmax(Target::enum()) -> 'ok'.
resetMinmax(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5962),
  ok.

-spec currentPaletteMatrixARB(Index::i()) -> 'ok'.
currentPaletteMatrixARB(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5963),
  ok.

-spec matrixIndexubvARB(Indices::[i()]) -> 'ok'.
matrixIndexubvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5964),
  ok.

-spec matrixIndexusvARB(Indices::[i()]) -> 'ok'.
matrixIndexusvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5965),
  ok.

-spec matrixIndexuivARB(Indices::[i()]) -> 'ok'.
matrixIndexuivARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5966),
  ok.

-spec sampleCoverageARB(Value::f(), Invert::0|1) -> 'ok'.
sampleCoverageARB(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5967),
  ok.

-spec maxShaderCompilerThreadsARB(Count::i()) -> 'ok'.
maxShaderCompilerThreadsARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5968),
  ok.

-spec evaluateDepthValuesARB() -> 'ok'.
evaluateDepthValuesARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5969),
  ok.

-spec deleteObjectARB(Obj::i()) -> 'ok'.
deleteObjectARB(Obj) when is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,5970),
  ok.

-spec getHandleARB(Pname::enum()) -> i().
getHandleARB(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5971),
  rec(5971).

-spec detachObjectARB(ContainerObj::i(), AttachedObj::i()) -> 'ok'.
detachObjectARB(ContainerObj,AttachedObj) when is_integer(ContainerObj),is_integer(AttachedObj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,AttachedObj,5972),
  ok.

-spec createShaderObjectARB(ShaderType::enum()) -> i().
createShaderObjectARB(ShaderType) when is_integer(ShaderType) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderType,5973),
  rec(5973).

-spec shaderSourceARB(ShaderObj::i(), String::[unicode:chardata()]) -> 'ok'.
shaderSourceARB(ShaderObj,String) when is_integer(ShaderObj),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(ShaderObj,Count,StringTemp,5974),
  ok.

-spec compileShaderARB(ShaderObj::i()) -> 'ok'.
compileShaderARB(ShaderObj) when is_integer(ShaderObj) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderObj,5975),
  ok.

-spec createProgramObjectARB() -> i().
createProgramObjectARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5976),
  rec(5976).

-spec attachObjectARB(ContainerObj::i(), Obj::i()) -> 'ok'.
attachObjectARB(ContainerObj,Obj) when is_integer(ContainerObj),is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,Obj,5977),
  ok.

-spec linkProgramARB(ProgramObj::i()) -> 'ok'.
linkProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5978),
  ok.

-spec useProgramObjectARB(ProgramObj::i()) -> 'ok'.
useProgramObjectARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5979),
  ok.

-spec validateProgramARB(ProgramObj::i()) -> 'ok'.
validateProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5980),
  ok.

-spec getObjectParameterfvARB(Obj::i(), Pname::enum()) -> f().
getObjectParameterfvARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5981),
  rec(5981).

-spec getObjectParameterivARB(Obj::i(), Pname::enum()) -> i().
getObjectParameterivARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5982),
  rec(5982).

-spec getInfoLogARB(Obj::i(), MaxLength::i()) -> string().
getInfoLogARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5983),
  rec(5983).

-spec getAttachedObjectsARB(ContainerObj::i(), MaxCount::i()) -> [i()].
getAttachedObjectsARB(ContainerObj,MaxCount) when is_integer(ContainerObj),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,MaxCount,5984),
  rec(5984).

-spec getUniformLocationARB(ProgramObj::i(), Name::string()) -> i().
getUniformLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,5985),
  rec(5985).

-spec getActiveUniformARB(ProgramObj::i(), Index::i(), MaxLength::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveUniformARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,5986),
  rec(5986).

-spec getUniformfvARB(ProgramObj::i(), Location::i()) -> matrix().
getUniformfvARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5987),
  rec(5987).

-spec getUniformivARB(ProgramObj::i(), Location::i()) -> {i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i(),i()}.
getUniformivARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5988),
  rec(5988).

-spec getShaderSourceARB(Obj::i(), MaxLength::i()) -> string().
getShaderSourceARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5989),
  rec(5989).

-spec deleteNamedStringARB(Name::string()) -> 'ok'.
deleteNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5990),
  ok.

-spec compileShaderIncludeARB(Shader::i(), Path::[unicode:chardata()]) -> 'ok'.
compileShaderIncludeARB(Shader,Path) when is_integer(Shader),is_list(Path) ->
  IF = get_interface(),
  PathTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Path ],
  Count = length(Path),
  IF:queue_cmd(Shader,Count,PathTemp,5991),
  ok.

-spec isNamedStringARB(Name::string()) -> 0|1.
isNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5992),
  rec(5992).

-spec bufferPageCommitmentARB(Target::enum(), Offset::i(), Size::i(), Commit::0|1) -> 'ok'.
bufferPageCommitmentARB(Target,Offset,Size,Commit) when is_integer(Target),is_integer(Offset),is_integer(Size),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Commit,5993),
  ok.

-spec texPageCommitmentARB(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Commit) -> 'ok'
    when Target::enum(), Level::i(), Xoffset::i(), Yoffset::i(), Zoffset::i(), Width::i(), Height::i(), Depth::i(), Commit::0|1.
texPageCommitmentARB(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit,5994),
  ok.

-spec getCompressedTexImageARB(Target::enum(), Level::i(), Img::mem()) -> 'ok'.
getCompressedTexImageARB(Target,Level,Img) when is_integer(Target),is_integer(Level),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Img,5995),
  rec(5995).

-spec loadTransposeMatrixfARB(M::matrix()) -> 'ok'.
loadTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5996),
  ok.

-spec loadTransposeMatrixdARB(M::matrix()) -> 'ok'.
loadTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5997),
  ok.

-spec multTransposeMatrixfARB(M::matrix()) -> 'ok'.
multTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5998),
  ok.

-spec multTransposeMatrixdARB(M::matrix()) -> 'ok'.
multTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5999),
  ok.

-spec weightbvARB(Weights::[i()]) -> 'ok'.
weightbvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6000),
  ok.

-spec weightsvARB(Weights::[i()]) -> 'ok'.
weightsvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6001),
  ok.

-spec weightivARB(Weights::[i()]) -> 'ok'.
weightivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6002),
  ok.

-spec weightfvARB(Weights::[f()]) -> 'ok'.
weightfvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6003),
  ok.

-spec weightdvARB(Weights::[f()]) -> 'ok'.
weightdvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6004),
  ok.

-spec weightubvARB(Weights::[i()]) -> 'ok'.
weightubvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6005),
  ok.

-spec weightusvARB(Weights::[i()]) -> 'ok'.
weightusvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6006),
  ok.

-spec weightuivARB(Weights::[i()]) -> 'ok'.
weightuivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6007),
  ok.

-spec vertexBlendARB(Count::i()) -> 'ok'.
vertexBlendARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6008),
  ok.

-spec getBufferParameterivARB(Target::enum(), Pname::enum()) -> [i()].
getBufferParameterivARB(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,6009),
  rec(6009).

-spec bindAttribLocationARB(ProgramObj::i(), Index::i(), Name::string()) -> 'ok'.
bindAttribLocationARB(ProgramObj,Index,Name) when is_integer(ProgramObj),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,Index,NameBin,6010),
  ok.

-spec getActiveAttribARB(ProgramObj::i(), Index::i(), MaxLength::i()) -> {Size::i(),Type::enum(),Name::string()}.
getActiveAttribARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,6011),
  rec(6011).

-spec getAttribLocationARB(ProgramObj::i(), Name::string()) -> i().
getAttribLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,6012),
  rec(6012).

-spec blendBarrierKHR() -> 'ok'.
blendBarrierKHR()  ->
  IF = get_interface(),
  IF:queue_cmd(6013),
  ok.

-spec maxShaderCompilerThreadsKHR(Count::i()) -> 'ok'.
maxShaderCompilerThreadsKHR(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6014),
  ok.

-spec depthBoundsEXT(Zmin::clamp(), Zmax::clamp()) -> 'ok'.
depthBoundsEXT(Zmin,Zmax) when is_float(Zmin),is_float(Zmax) ->
  IF = get_interface(),
  IF:queue_cmd(Zmin,Zmax,6015),
  ok.

