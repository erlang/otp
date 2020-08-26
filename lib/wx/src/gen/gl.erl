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
-type matrix12() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix16() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix() :: matrix12() | matrix16().
-type mem() :: binary() | tuple().   %% Memory block
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

-spec clearIndex(C) -> 'ok' when C :: float().
clearIndex(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5037),
  ok.

-spec clearColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
clearColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5038),
  ok.

-spec clear(Mask) -> 'ok' when Mask :: integer().
clear(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5039),
  ok.

-spec indexMask(Mask) -> 'ok' when Mask :: integer().
indexMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5040),
  ok.

-spec colorMask(Red, Green, Blue, Alpha) -> 'ok' when Red :: 0|1,Green :: 0|1,Blue :: 0|1,Alpha :: 0|1.
colorMask(Red,Green,Blue,Alpha) when (0 =:= Red) orelse (1 =:= Red),(0 =:= Green) orelse (1 =:= Green),(0 =:= Blue) orelse (1 =:= Blue),(0 =:= Alpha) orelse (1 =:= Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5041),
  ok.

-spec alphaFunc(Func, Ref) -> 'ok' when Func :: enum(),Ref :: clamp().
alphaFunc(Func,Ref) when is_integer(Func),is_float(Ref) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,5042),
  ok.

-spec blendFunc(Sfactor, Dfactor) -> 'ok' when Sfactor :: enum(),Dfactor :: enum().
blendFunc(Sfactor,Dfactor) when is_integer(Sfactor),is_integer(Dfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Sfactor,Dfactor,5043),
  ok.

-spec logicOp(Opcode) -> 'ok' when Opcode :: enum().
logicOp(Opcode) when is_integer(Opcode) ->
  IF = get_interface(),
  IF:queue_cmd(Opcode,5044),
  ok.

-spec cullFace(Mode) -> 'ok' when Mode :: enum().
cullFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5045),
  ok.

-spec frontFace(Mode) -> 'ok' when Mode :: enum().
frontFace(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5046),
  ok.

-spec pointSize(Size) -> 'ok' when Size :: float().
pointSize(Size) when is_float(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Size,5047),
  ok.

-spec lineWidth(Width) -> 'ok' when Width :: float().
lineWidth(Width) when is_float(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Width,5048),
  ok.

-spec lineStipple(Factor, Pattern) -> 'ok' when Factor :: integer(),Pattern :: integer().
lineStipple(Factor,Pattern) when is_integer(Factor),is_integer(Pattern) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Pattern,5049),
  ok.

-spec polygonMode(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
polygonMode(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5050),
  ok.

-spec polygonOffset(Factor, Units) -> 'ok' when Factor :: float(),Units :: float().
polygonOffset(Factor,Units) when is_float(Factor),is_float(Units) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,5051),
  ok.

-spec polygonStipple(Mask) -> 'ok' when Mask :: binary().
polygonStipple(Mask) when is_binary(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5052),
  ok.

-spec getPolygonStipple() -> binary().
getPolygonStipple()  ->
  IF = get_interface(),
  IF:queue_cmd(5053),
  rec(5053).

-spec edgeFlag(Flag) -> 'ok' when Flag :: 0|1.
edgeFlag(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5054),
  ok.

%% @equiv edgeFlag(Flag)
-spec edgeFlagv(Flag) -> 'ok' when Flag :: {Flag :: 0|1}.
edgeFlagv({Flag}) ->  edgeFlag(Flag).
-spec scissor(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
scissor(X,Y,Width,Height) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,5055),
  ok.

-spec clipPlane(Plane, Equation) -> 'ok' when Plane :: enum(),Equation :: {float(),float(),float(),float()}.
clipPlane(Plane,Equation) when is_integer(Plane),tuple_size(Equation) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Plane,Equation,5056),
  ok.

-spec getClipPlane(Plane) -> {float(),float(),float(),float()} when Plane :: enum().
getClipPlane(Plane) when is_integer(Plane) ->
  IF = get_interface(),
  IF:queue_cmd(Plane,5057),
  rec(5057).

-spec drawBuffer(Mode) -> 'ok' when Mode :: enum().
drawBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5058),
  ok.

-spec readBuffer(Mode) -> 'ok' when Mode :: enum().
readBuffer(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5059),
  ok.

-spec enable(Cap) -> 'ok' when Cap :: enum().
enable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5060),
  ok.

-spec disable(Cap) -> 'ok' when Cap :: enum().
disable(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5061),
  ok.

-spec isEnabled(Cap) -> 0|1 when Cap :: enum().
isEnabled(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5062),
  rec(5062).

-spec enableClientState(Cap) -> 'ok' when Cap :: enum().
enableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5063),
  ok.

-spec disableClientState(Cap) -> 'ok' when Cap :: enum().
disableClientState(Cap) when is_integer(Cap) ->
  IF = get_interface(),
  IF:queue_cmd(Cap,5064),
  ok.

-spec getBooleanv(Pname) -> [0|1] when Pname :: enum().
getBooleanv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5065),
  rec(5065).

-spec getDoublev(Pname) -> [float()] when Pname :: enum().
getDoublev(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5066),
  rec(5066).

-spec getFloatv(Pname) -> [float()] when Pname :: enum().
getFloatv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5067),
  rec(5067).

-spec getIntegerv(Pname) -> [integer()] when Pname :: enum().
getIntegerv(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5068),
  rec(5068).

-spec pushAttrib(Mask) -> 'ok' when Mask :: integer().
pushAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5069),
  ok.

-spec popAttrib() -> 'ok'.
popAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5070),
  ok.

-spec pushClientAttrib(Mask) -> 'ok' when Mask :: integer().
pushClientAttrib(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5071),
  ok.

-spec popClientAttrib() -> 'ok'.
popClientAttrib()  ->
  IF = get_interface(),
  IF:queue_cmd(5072),
  ok.

-spec renderMode(Mode) -> integer() when Mode :: enum().
renderMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5073),
  rec(5073).

-spec getError() -> enum().
getError()  ->
  IF = get_interface(),
  IF:queue_cmd(5074),
  rec(5074).

-spec getString(Name) -> string() when Name :: enum().
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

-spec hint(Target, Mode) -> 'ok' when Target :: enum(),Mode :: enum().
hint(Target,Mode) when is_integer(Target),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Mode,5078),
  ok.

-spec clearDepth(Depth) -> 'ok' when Depth :: clamp().
clearDepth(Depth) when is_float(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Depth,5079),
  ok.

-spec depthFunc(Func) -> 'ok' when Func :: enum().
depthFunc(Func) when is_integer(Func) ->
  IF = get_interface(),
  IF:queue_cmd(Func,5080),
  ok.

-spec depthMask(Flag) -> 'ok' when Flag :: 0|1.
depthMask(Flag) when (0 =:= Flag) orelse (1 =:= Flag) ->
  IF = get_interface(),
  IF:queue_cmd(Flag,5081),
  ok.

-spec depthRange(Near_val, Far_val) -> 'ok' when Near_val :: clamp(),Far_val :: clamp().
depthRange(Near_val,Far_val) when is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Near_val,Far_val,5082),
  ok.

-spec clearAccum(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
clearAccum(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5083),
  ok.

-spec accum(Op, Value) -> 'ok' when Op :: enum(),Value :: float().
accum(Op,Value) when is_integer(Op),is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Op,Value,5084),
  ok.

-spec matrixMode(Mode) -> 'ok' when Mode :: enum().
matrixMode(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5085),
  ok.

-spec ortho(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
ortho(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5086),
  ok.

-spec frustum(Left, Right, Bottom, Top, Near_val, Far_val) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float(),Near_val :: float(),Far_val :: float().
frustum(Left,Right,Bottom,Top,Near_val,Far_val) when is_float(Left),is_float(Right),is_float(Bottom),is_float(Top),is_float(Near_val),is_float(Far_val) ->
  IF = get_interface(),
  IF:queue_cmd(Left,Right,Bottom,Top,Near_val,Far_val,5087),
  ok.

-spec viewport(X, Y, Width, Height) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
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

-spec loadMatrixd(M) -> 'ok' when M :: matrix().
loadMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5092),
  ok.

-spec loadMatrixf(M) -> 'ok' when M :: matrix().
loadMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5093),
  ok.

-spec multMatrixd(M) -> 'ok' when M :: matrix().
multMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5094),
  ok.

-spec multMatrixf(M) -> 'ok' when M :: matrix().
multMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5095),
  ok.

-spec rotated(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotated(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5096),
  ok.

-spec rotatef(Angle, X, Y, Z) -> 'ok' when Angle :: float(),X :: float(),Y :: float(),Z :: float().
rotatef(Angle,X,Y,Z) when is_float(Angle),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Angle,X,Y,Z,5097),
  ok.

-spec scaled(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scaled(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5098),
  ok.

-spec scalef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
scalef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5099),
  ok.

-spec translated(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translated(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5100),
  ok.

-spec translatef(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
translatef(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5101),
  ok.

-spec isList(List) -> 0|1 when List :: integer().
isList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5102),
  rec(5102).

-spec deleteLists(List, Range) -> 'ok' when List :: integer(),Range :: integer().
deleteLists(List,Range) when is_integer(List),is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(List,Range,5103),
  ok.

-spec genLists(Range) -> integer() when Range :: integer().
genLists(Range) when is_integer(Range) ->
  IF = get_interface(),
  IF:queue_cmd(Range,5104),
  rec(5104).

-spec newList(List, Mode) -> 'ok' when List :: integer(),Mode :: enum().
newList(List,Mode) when is_integer(List),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(List,Mode,5105),
  ok.

-spec endList() -> 'ok'.
endList()  ->
  IF = get_interface(),
  IF:queue_cmd(5106),
  ok.

-spec callList(List) -> 'ok' when List :: integer().
callList(List) when is_integer(List) ->
  IF = get_interface(),
  IF:queue_cmd(List,5107),
  ok.

-spec callLists(Lists) -> 'ok' when Lists :: [integer()].
callLists(Lists) when is_list(Lists) ->
  IF = get_interface(),
  N = length(Lists),
  IF:queue_cmd(N,Lists,5108),
  ok.

-spec listBase(Base) -> 'ok' when Base :: integer().
listBase(Base) when is_integer(Base) ->
  IF = get_interface(),
  IF:queue_cmd(Base,5109),
  ok.

-spec 'begin'(Mode) -> 'ok' when Mode :: enum().
'begin'(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5110),
  ok.

-spec 'end'() -> 'ok'.
'end'()  ->
  IF = get_interface(),
  IF:queue_cmd(5111),
  ok.

-spec vertex2d(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5112),
  ok.

-spec vertex2f(X, Y) -> 'ok' when X :: float(),Y :: float().
vertex2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5113),
  ok.

-spec vertex2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5114),
  ok.

-spec vertex2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
vertex2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5115),
  ok.

-spec vertex3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5116),
  ok.

-spec vertex3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
vertex3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5117),
  ok.

-spec vertex3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5118),
  ok.

-spec vertex3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
vertex3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5119),
  ok.

-spec vertex4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5120),
  ok.

-spec vertex4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
vertex4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5121),
  ok.

-spec vertex4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5122),
  ok.

-spec vertex4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertex4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5123),
  ok.

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
-spec normal3b(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3b(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5124),
  ok.

-spec normal3d(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3d(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5125),
  ok.

-spec normal3f(Nx, Ny, Nz) -> 'ok' when Nx :: float(),Ny :: float(),Nz :: float().
normal3f(Nx,Ny,Nz) when is_float(Nx),is_float(Ny),is_float(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5126),
  ok.

-spec normal3i(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3i(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5127),
  ok.

-spec normal3s(Nx, Ny, Nz) -> 'ok' when Nx :: integer(),Ny :: integer(),Nz :: integer().
normal3s(Nx,Ny,Nz) when is_integer(Nx),is_integer(Ny),is_integer(Nz) ->
  IF = get_interface(),
  IF:queue_cmd(Nx,Ny,Nz,5128),
  ok.

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
-spec indexd(C) -> 'ok' when C :: float().
indexd(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5129),
  ok.

-spec indexf(C) -> 'ok' when C :: float().
indexf(C) when is_float(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5130),
  ok.

-spec indexi(C) -> 'ok' when C :: integer().
indexi(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5131),
  ok.

-spec indexs(C) -> 'ok' when C :: integer().
indexs(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5132),
  ok.

-spec indexub(C) -> 'ok' when C :: integer().
indexub(C) when is_integer(C) ->
  IF = get_interface(),
  IF:queue_cmd(C,5133),
  ok.

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
-spec color3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5134),
  ok.

-spec color3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5135),
  ok.

-spec color3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
color3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5136),
  ok.

-spec color3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5137),
  ok.

-spec color3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5138),
  ok.

-spec color3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5139),
  ok.

-spec color3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5140),
  ok.

-spec color3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
color3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5141),
  ok.

-spec color4b(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4b(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5142),
  ok.

-spec color4d(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4d(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5143),
  ok.

-spec color4f(Red, Green, Blue, Alpha) -> 'ok' when Red :: float(),Green :: float(),Blue :: float(),Alpha :: float().
color4f(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5144),
  ok.

-spec color4i(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4i(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5145),
  ok.

-spec color4s(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4s(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5146),
  ok.

-spec color4ub(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ub(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5147),
  ok.

-spec color4ui(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4ui(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5148),
  ok.

-spec color4us(Red, Green, Blue, Alpha) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer(),Alpha :: integer().
color4us(Red,Green,Blue,Alpha) when is_integer(Red),is_integer(Green),is_integer(Blue),is_integer(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5149),
  ok.

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
-spec texCoord1d(S) -> 'ok' when S :: float().
texCoord1d(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5150),
  ok.

-spec texCoord1f(S) -> 'ok' when S :: float().
texCoord1f(S) when is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5151),
  ok.

-spec texCoord1i(S) -> 'ok' when S :: integer().
texCoord1i(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5152),
  ok.

-spec texCoord1s(S) -> 'ok' when S :: integer().
texCoord1s(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5153),
  ok.

-spec texCoord2d(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2d(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5154),
  ok.

-spec texCoord2f(S, T) -> 'ok' when S :: float(),T :: float().
texCoord2f(S,T) when is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5155),
  ok.

-spec texCoord2i(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2i(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5156),
  ok.

-spec texCoord2s(S, T) -> 'ok' when S :: integer(),T :: integer().
texCoord2s(S,T) when is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,5157),
  ok.

-spec texCoord3d(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3d(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5158),
  ok.

-spec texCoord3f(S, T, R) -> 'ok' when S :: float(),T :: float(),R :: float().
texCoord3f(S,T,R) when is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5159),
  ok.

-spec texCoord3i(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3i(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5160),
  ok.

-spec texCoord3s(S, T, R) -> 'ok' when S :: integer(),T :: integer(),R :: integer().
texCoord3s(S,T,R) when is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,5161),
  ok.

-spec texCoord4d(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4d(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5162),
  ok.

-spec texCoord4f(S, T, R, Q) -> 'ok' when S :: float(),T :: float(),R :: float(),Q :: float().
texCoord4f(S,T,R,Q) when is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5163),
  ok.

-spec texCoord4i(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4i(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5164),
  ok.

-spec texCoord4s(S, T, R, Q) -> 'ok' when S :: integer(),T :: integer(),R :: integer(),Q :: integer().
texCoord4s(S,T,R,Q) when is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(S,T,R,Q,5165),
  ok.

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
-spec rasterPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5166),
  ok.

-spec rasterPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
rasterPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5167),
  ok.

-spec rasterPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5168),
  ok.

-spec rasterPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
rasterPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5169),
  ok.

-spec rasterPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5170),
  ok.

-spec rasterPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
rasterPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5171),
  ok.

-spec rasterPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5172),
  ok.

-spec rasterPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
rasterPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5173),
  ok.

-spec rasterPos4d(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4d(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5174),
  ok.

-spec rasterPos4f(X, Y, Z, W) -> 'ok' when X :: float(),Y :: float(),Z :: float(),W :: float().
rasterPos4f(X,Y,Z,W) when is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5175),
  ok.

-spec rasterPos4i(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4i(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5176),
  ok.

-spec rasterPos4s(X, Y, Z, W) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
rasterPos4s(X,Y,Z,W) when is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,W,5177),
  ok.

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
-spec rectd(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectd(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5178),
  ok.

-spec rectf(X1, Y1, X2, Y2) -> 'ok' when X1 :: float(),Y1 :: float(),X2 :: float(),Y2 :: float().
rectf(X1,Y1,X2,Y2) when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5179),
  ok.

-spec recti(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
recti(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5180),
  ok.

-spec rects(X1, Y1, X2, Y2) -> 'ok' when X1 :: integer(),Y1 :: integer(),X2 :: integer(),Y2 :: integer().
rects(X1,Y1,X2,Y2) when is_integer(X1),is_integer(Y1),is_integer(X2),is_integer(Y2) ->
  IF = get_interface(),
  IF:queue_cmd(X1,Y1,X2,Y2,5181),
  ok.

-spec rectdv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectdv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5182),
  ok.

-spec rectfv(V1, V2) -> 'ok' when V1 :: {float(),float()},V2 :: {float(),float()}.
rectfv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5183),
  ok.

-spec rectiv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectiv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5184),
  ok.

-spec rectsv(V1, V2) -> 'ok' when V1 :: {integer(),integer()},V2 :: {integer(),integer()}.
rectsv(V1,V2) when tuple_size(V1) =:= 2,tuple_size(V2) =:= 2 ->
  IF = get_interface(),
  IF:queue_cmd(V1,V2,5185),
  ok.

-spec vertexPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
vertexPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5186),
  ok.

-spec normalPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
normalPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5188),
  ok.

-spec colorPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
colorPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5190),
  ok.

-spec indexPointer(Type, Stride, Ptr) -> 'ok' when Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
indexPointer(Type,Stride,Ptr) when is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Ptr,5192),
  ok.

-spec texCoordPointer(Size, Type, Stride, Ptr) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Ptr :: offset()|mem().
texCoordPointer(Size,Type,Stride,Ptr) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Ptr,5194),
  ok.

-spec edgeFlagPointer(Stride, Ptr) -> 'ok' when Stride :: integer(),Ptr :: offset()|mem().
edgeFlagPointer(Stride,Ptr) when is_integer(Stride),is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr) ->
  IF = get_interface(),
  IF:queue_cmd(Stride,Ptr,5196),
  ok.

-spec arrayElement(I) -> 'ok' when I :: integer().
arrayElement(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5198),
  ok.

-spec drawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer().
drawArrays(Mode,First,Count) when is_integer(Mode),is_integer(First),is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5199),
  ok.

-spec drawElements(Mode, Count, Type, Indices) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawElements(Mode,Count,Type,Indices) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,5200),
  ok.

-spec interleavedArrays(Format, Stride, Pointer) -> 'ok' when Format :: enum(),Stride :: integer(),Pointer :: offset()|mem().
interleavedArrays(Format,Stride,Pointer) when is_integer(Format),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Format,Stride,Pointer,5202),
  ok.

-spec shadeModel(Mode) -> 'ok' when Mode :: enum().
shadeModel(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5204),
  ok.

-spec lightf(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: float().
lightf(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5205),
  ok.

-spec lighti(Light, Pname, Param) -> 'ok' when Light :: enum(),Pname :: enum(),Param :: integer().
lighti(Light,Pname,Param) when is_integer(Light),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Param,5206),
  ok.

-spec lightfv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightfv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5207),
  ok.

-spec lightiv(Light, Pname, Params) -> 'ok' when Light :: enum(),Pname :: enum(),Params :: tuple().
lightiv(Light,Pname,Params) when is_integer(Light),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,Params,5208),
  ok.

-spec getLightfv(Light, Pname) -> {float(),float(),float(),float()} when Light :: enum(),Pname :: enum().
getLightfv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5209),
  rec(5209).

-spec getLightiv(Light, Pname) -> {integer(),integer(),integer(),integer()} when Light :: enum(),Pname :: enum().
getLightiv(Light,Pname) when is_integer(Light),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Light,Pname,5210),
  rec(5210).

-spec lightModelf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
lightModelf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5211),
  ok.

-spec lightModeli(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
lightModeli(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5212),
  ok.

-spec lightModelfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModelfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5213),
  ok.

-spec lightModeliv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
lightModeliv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5214),
  ok.

-spec materialf(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: float().
materialf(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5215),
  ok.

-spec materiali(Face, Pname, Param) -> 'ok' when Face :: enum(),Pname :: enum(),Param :: integer().
materiali(Face,Pname,Param) when is_integer(Face),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Param,5216),
  ok.

-spec materialfv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialfv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5217),
  ok.

-spec materialiv(Face, Pname, Params) -> 'ok' when Face :: enum(),Pname :: enum(),Params :: tuple().
materialiv(Face,Pname,Params) when is_integer(Face),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,Params,5218),
  ok.

-spec getMaterialfv(Face, Pname) -> {float(),float(),float(),float()} when Face :: enum(),Pname :: enum().
getMaterialfv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5219),
  rec(5219).

-spec getMaterialiv(Face, Pname) -> {integer(),integer(),integer(),integer()} when Face :: enum(),Pname :: enum().
getMaterialiv(Face,Pname) when is_integer(Face),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Pname,5220),
  rec(5220).

-spec colorMaterial(Face, Mode) -> 'ok' when Face :: enum(),Mode :: enum().
colorMaterial(Face,Mode) when is_integer(Face),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mode,5221),
  ok.

-spec pixelZoom(Xfactor, Yfactor) -> 'ok' when Xfactor :: float(),Yfactor :: float().
pixelZoom(Xfactor,Yfactor) when is_float(Xfactor),is_float(Yfactor) ->
  IF = get_interface(),
  IF:queue_cmd(Xfactor,Yfactor,5222),
  ok.

-spec pixelStoref(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelStoref(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5223),
  ok.

-spec pixelStorei(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelStorei(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5224),
  ok.

-spec pixelTransferf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pixelTransferf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5225),
  ok.

-spec pixelTransferi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pixelTransferi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5226),
  ok.

-spec pixelMapfv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapfv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5227),
  ok.

-spec pixelMapuiv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapuiv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5228),
  ok.

-spec pixelMapusv(Map, Mapsize, Values) -> 'ok' when Map :: enum(),Mapsize :: integer(),Values :: binary().
pixelMapusv(Map,Mapsize,Values) when is_integer(Map),is_integer(Mapsize),is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Mapsize,Values,5229),
  ok.

-spec getPixelMapfv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapfv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5230),
  rec(5230).

-spec getPixelMapuiv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapuiv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5231),
  rec(5231).

-spec getPixelMapusv(Map, Values) -> 'ok' when Map :: enum(),Values :: mem().
getPixelMapusv(Map,Values) when is_integer(Map),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Map,Values,5232),
  rec(5232).

-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> 'ok' when Width :: integer(),Height :: integer(),Xorig :: float(),Yorig :: float(),Xmove :: float(),Ymove :: float(),Bitmap :: offset()|mem().
bitmap(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap) when is_integer(Width),is_integer(Height),is_float(Xorig),is_float(Yorig),is_float(Xmove),is_float(Ymove),is_integer(Bitmap) orelse is_tuple(Bitmap) orelse is_binary(Bitmap) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Xorig,Yorig,Xmove,Ymove,Bitmap,5233),
  ok.

-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
readPixels(X,Y,Width,Height,Format,Type,Pixels) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Format,Type,Pixels,5235),
  rec(5235).

-spec drawPixels(Width, Height, Format, Type, Pixels) -> 'ok' when Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
drawPixels(Width,Height,Format,Type,Pixels) when is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Width,Height,Format,Type,Pixels,5236),
  ok.

-spec copyPixels(X, Y, Width, Height, Type) -> 'ok' when X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Type :: enum().
copyPixels(X,Y,Width,Height,Type) when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Width,Height,Type,5238),
  ok.

-spec stencilFunc(Func, Ref, Mask) -> 'ok' when Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFunc(Func,Ref,Mask) when is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Func,Ref,Mask,5239),
  ok.

-spec stencilMask(Mask) -> 'ok' when Mask :: integer().
stencilMask(Mask) when is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Mask,5240),
  ok.

-spec stencilOp(Fail, Zfail, Zpass) -> 'ok' when Fail :: enum(),Zfail :: enum(),Zpass :: enum().
stencilOp(Fail,Zfail,Zpass) when is_integer(Fail),is_integer(Zfail),is_integer(Zpass) ->
  IF = get_interface(),
  IF:queue_cmd(Fail,Zfail,Zpass,5241),
  ok.

-spec clearStencil(S) -> 'ok' when S :: integer().
clearStencil(S) when is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(S,5242),
  ok.

-spec texGend(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGend(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5243),
  ok.

-spec texGenf(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: float().
texGenf(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5244),
  ok.

-spec texGeni(Coord, Pname, Param) -> 'ok' when Coord :: enum(),Pname :: enum(),Param :: integer().
texGeni(Coord,Pname,Param) when is_integer(Coord),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Param,5245),
  ok.

-spec texGendv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGendv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5246),
  ok.

-spec texGenfv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGenfv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5247),
  ok.

-spec texGeniv(Coord, Pname, Params) -> 'ok' when Coord :: enum(),Pname :: enum(),Params :: tuple().
texGeniv(Coord,Pname,Params) when is_integer(Coord),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,Params,5248),
  ok.

-spec getTexGendv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGendv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5249),
  rec(5249).

-spec getTexGenfv(Coord, Pname) -> {float(),float(),float(),float()} when Coord :: enum(),Pname :: enum().
getTexGenfv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5250),
  rec(5250).

-spec getTexGeniv(Coord, Pname) -> {integer(),integer(),integer(),integer()} when Coord :: enum(),Pname :: enum().
getTexGeniv(Coord,Pname) when is_integer(Coord),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,Pname,5251),
  rec(5251).

-spec texEnvf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texEnvf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5252),
  ok.

-spec texEnvi(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texEnvi(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5253),
  ok.

-spec texEnvfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnvfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5254),
  ok.

-spec texEnviv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texEnviv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5255),
  ok.

-spec getTexEnvfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexEnvfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5256),
  rec(5256).

-spec getTexEnviv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexEnviv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5257),
  rec(5257).

-spec texParameterf(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: float().
texParameterf(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5258),
  ok.

-spec texParameteri(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
texParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5259),
  ok.

-spec texParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5260),
  ok.

-spec texParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5261),
  ok.

-spec getTexParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getTexParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5262),
  rec(5262).

-spec getTexParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5263),
  rec(5263).

-spec getTexLevelParameterfv(Target, Level, Pname) -> {float()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameterfv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5264),
  rec(5264).

-spec getTexLevelParameteriv(Target, Level, Pname) -> {integer()} when Target :: enum(),Level :: integer(),Pname :: enum().
getTexLevelParameteriv(Target,Level,Pname) when is_integer(Target),is_integer(Level),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Pname,5265),
  rec(5265).

-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage1D(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Border,Format,Type,Pixels,5266),
  ok.

-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage2D(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Border,Format,Type,Pixels,5268),
  ok.

-spec getTexImage(Target, Level, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Format :: enum(),Type :: enum(),Pixels :: mem().
getTexImage(Target,Level,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Format),is_integer(Type),is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Format,Type,Pixels,5270),
  rec(5270).

-spec genTextures(N) -> [integer()] when N :: integer().
genTextures(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5271),
  rec(5271).

-spec deleteTextures(Textures) -> 'ok' when Textures :: [integer()].
deleteTextures(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5272),
  ok.

-spec bindTexture(Target, Texture) -> 'ok' when Target :: enum(),Texture :: integer().
bindTexture(Target,Texture) when is_integer(Target),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Texture,5273),
  ok.

-spec prioritizeTextures(Textures, Priorities) -> 'ok' when Textures :: [integer()],Priorities :: [clamp()].
prioritizeTextures(Textures,Priorities) when is_list(Textures),is_list(Priorities) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,Priorities,5274),
  ok.

-spec areTexturesResident(Textures) -> {0|1,Residences :: [0|1]} when Textures :: [integer()].
areTexturesResident(Textures) when is_list(Textures) ->
  IF = get_interface(),
  N = length(Textures),
  IF:queue_cmd(N,Textures,5275),
  rec(5275).

-spec isTexture(Texture) -> 0|1 when Texture :: integer().
isTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5276),
  rec(5276).

-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage1D(Target,Level,Xoffset,Width,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,Type,Pixels,5277),
  ok.

-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,Type,Pixels,5279),
  ok.

-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Border :: integer().
copyTexImage1D(Target,Level,Internalformat,X,Y,Width,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Border,5281),
  ok.

-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer(),Border :: integer().
copyTexImage2D(Target,Level,Internalformat,X,Y,Width,Height,Border) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_integer(Border) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,X,Y,Width,Height,Border,5282),
  ok.

-spec copyTexSubImage1D(Target, Level, Xoffset, X, Y, Width) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyTexSubImage1D(Target,Level,Xoffset,X,Y,Width) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,X,Y,Width,5283),
  ok.

-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage2D(Target,Level,Xoffset,Yoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,X,Y,Width,Height,5284),
  ok.

-spec map1d(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1d(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5285),
  ok.

-spec map1f(Target, U1, U2, Stride, Order, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Stride :: integer(),Order :: integer(),Points :: binary().
map1f(Target,U1,U2,Stride,Order,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Stride),is_integer(Order),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Stride,Order,Points,5286),
  ok.

-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2d(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5287),
  ok.

-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> 'ok' when Target :: enum(),U1 :: float(),U2 :: float(),Ustride :: integer(),Uorder :: integer(),V1 :: float(),V2 :: float(),Vstride :: integer(),Vorder :: integer(),Points :: binary().
map2f(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points) when is_integer(Target),is_float(U1),is_float(U2),is_integer(Ustride),is_integer(Uorder),is_float(V1),is_float(V2),is_integer(Vstride),is_integer(Vorder),is_binary(Points) ->
  IF = get_interface(),
  IF:queue_cmd(Target,U1,U2,Ustride,Uorder,V1,V2,Vstride,Vorder,Points,5288),
  ok.

-spec getMapdv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapdv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5289),
  rec(5289).

-spec getMapfv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapfv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5290),
  rec(5290).

-spec getMapiv(Target, Query, V) -> 'ok' when Target :: enum(),Query :: enum(),V :: mem().
getMapiv(Target,Query,V) when is_integer(Target),is_integer(Query),is_tuple(V) orelse is_binary(V) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Query,V,5291),
  rec(5291).

-spec evalCoord1d(U) -> 'ok' when U :: float().
evalCoord1d(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5292),
  ok.

-spec evalCoord1f(U) -> 'ok' when U :: float().
evalCoord1f(U) when is_float(U) ->
  IF = get_interface(),
  IF:queue_cmd(U,5293),
  ok.

%% @equiv evalCoord1d(U)
-spec evalCoord1dv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1dv({U}) ->  evalCoord1d(U).
%% @equiv evalCoord1f(U)
-spec evalCoord1fv(U) -> 'ok' when U :: {U :: float()}.
evalCoord1fv({U}) ->  evalCoord1f(U).
-spec evalCoord2d(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2d(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5294),
  ok.

-spec evalCoord2f(U, V) -> 'ok' when U :: float(),V :: float().
evalCoord2f(U,V) when is_float(U),is_float(V) ->
  IF = get_interface(),
  IF:queue_cmd(U,V,5295),
  ok.

%% @equiv evalCoord2d(U,V)
-spec evalCoord2dv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2dv({U,V}) ->  evalCoord2d(U,V).
%% @equiv evalCoord2f(U,V)
-spec evalCoord2fv(U) -> 'ok' when U :: {U :: float(),V :: float()}.
evalCoord2fv({U,V}) ->  evalCoord2f(U,V).
-spec mapGrid1d(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1d(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5296),
  ok.

-spec mapGrid1f(Un, U1, U2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float().
mapGrid1f(Un,U1,U2) when is_integer(Un),is_float(U1),is_float(U2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,5297),
  ok.

-spec mapGrid2d(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2d(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5298),
  ok.

-spec mapGrid2f(Un, U1, U2, Vn, V1, V2) -> 'ok' when Un :: integer(),U1 :: float(),U2 :: float(),Vn :: integer(),V1 :: float(),V2 :: float().
mapGrid2f(Un,U1,U2,Vn,V1,V2) when is_integer(Un),is_float(U1),is_float(U2),is_integer(Vn),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Un,U1,U2,Vn,V1,V2,5299),
  ok.

-spec evalPoint1(I) -> 'ok' when I :: integer().
evalPoint1(I) when is_integer(I) ->
  IF = get_interface(),
  IF:queue_cmd(I,5300),
  ok.

-spec evalPoint2(I, J) -> 'ok' when I :: integer(),J :: integer().
evalPoint2(I,J) when is_integer(I),is_integer(J) ->
  IF = get_interface(),
  IF:queue_cmd(I,J,5301),
  ok.

-spec evalMesh1(Mode, I1, I2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer().
evalMesh1(Mode,I1,I2) when is_integer(Mode),is_integer(I1),is_integer(I2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,5302),
  ok.

-spec evalMesh2(Mode, I1, I2, J1, J2) -> 'ok' when Mode :: enum(),I1 :: integer(),I2 :: integer(),J1 :: integer(),J2 :: integer().
evalMesh2(Mode,I1,I2,J1,J2) when is_integer(Mode),is_integer(I1),is_integer(I2),is_integer(J1),is_integer(J2) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,I1,I2,J1,J2,5303),
  ok.

-spec fogf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
fogf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5304),
  ok.

-spec fogi(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
fogi(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5305),
  ok.

-spec fogfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5306),
  ok.

-spec fogiv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
fogiv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5307),
  ok.

-spec feedbackBuffer(Size, Type, Buffer) -> 'ok' when Size :: integer(),Type :: enum(),Buffer :: mem().
feedbackBuffer(Size,Type,Buffer) when is_integer(Size),is_integer(Type),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Buffer,5308),
  rec(5308).

-spec passThrough(Token) -> 'ok' when Token :: float().
passThrough(Token) when is_float(Token) ->
  IF = get_interface(),
  IF:queue_cmd(Token,5309),
  ok.

-spec selectBuffer(Size, Buffer) -> 'ok' when Size :: integer(),Buffer :: mem().
selectBuffer(Size,Buffer) when is_integer(Size),is_tuple(Buffer) orelse is_binary(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Buffer,5310),
  rec(5310).

-spec initNames() -> 'ok'.
initNames()  ->
  IF = get_interface(),
  IF:queue_cmd(5311),
  ok.

-spec loadName(Name) -> 'ok' when Name :: integer().
loadName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5312),
  ok.

-spec pushName(Name) -> 'ok' when Name :: integer().
pushName(Name) when is_integer(Name) ->
  IF = get_interface(),
  IF:queue_cmd(Name,5313),
  ok.

-spec popName() -> 'ok'.
popName()  ->
  IF = get_interface(),
  IF:queue_cmd(5314),
  ok.

-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem().
drawRangeElements(Mode,Start,End,Count,Type,Indices) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,5315),
  ok.

-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texImage3D(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(InternalFormat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,InternalFormat,Width,Height,Depth,Border,Format,Type,Pixels,5317),
  ok.

-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Pixels) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Pixels :: offset()|mem().
texSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Pixels) orelse is_tuple(Pixels) orelse is_binary(Pixels) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Pixels,5319),
  ok.

-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,X,Y,Width,Height,5321),
  ok.

-spec activeTexture(Texture) -> 'ok' when Texture :: enum().
activeTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5322),
  ok.

-spec sampleCoverage(Value, Invert) -> 'ok' when Value :: clamp(),Invert :: 0|1.
sampleCoverage(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5323),
  ok.

-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage3D(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Depth,Border,ImageSize,Data,5324),
  ok.

-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage2D(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Height,Border,ImageSize,Data,5326),
  ok.

-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Internalformat :: enum(),Width :: integer(),Border :: integer(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexImage1D(Target,Level,Internalformat,Width,Border,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Internalformat),is_integer(Width),is_integer(Border),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Internalformat,Width,Border,ImageSize,Data,5328),
  ok.

-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage3D(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5330),
  ok.

-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage2D(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5332),
  ok.

-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTexSubImage1D(Target,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Width,Format,ImageSize,Data,5334),
  ok.

-spec getCompressedTexImage(Target, Lod, Img) -> 'ok' when Target :: enum(),Lod :: integer(),Img :: mem().
getCompressedTexImage(Target,Lod,Img) when is_integer(Target),is_integer(Lod),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Lod,Img,5336),
  rec(5336).

-spec clientActiveTexture(Texture) -> 'ok' when Texture :: enum().
clientActiveTexture(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5337),
  ok.

-spec multiTexCoord1d(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1d(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5338),
  ok.

%% @equiv multiTexCoord1d(Target,S)
-spec multiTexCoord1dv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1dv(Target,{S}) ->  multiTexCoord1d(Target,S).
-spec multiTexCoord1f(Target, S) -> 'ok' when Target :: enum(),S :: float().
multiTexCoord1f(Target,S) when is_integer(Target),is_float(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5339),
  ok.

%% @equiv multiTexCoord1f(Target,S)
-spec multiTexCoord1fv(Target :: enum(),V) -> 'ok' when V :: {S :: float()}.
multiTexCoord1fv(Target,{S}) ->  multiTexCoord1f(Target,S).
-spec multiTexCoord1i(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1i(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5340),
  ok.

%% @equiv multiTexCoord1i(Target,S)
-spec multiTexCoord1iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1iv(Target,{S}) ->  multiTexCoord1i(Target,S).
-spec multiTexCoord1s(Target, S) -> 'ok' when Target :: enum(),S :: integer().
multiTexCoord1s(Target,S) when is_integer(Target),is_integer(S) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,5341),
  ok.

%% @equiv multiTexCoord1s(Target,S)
-spec multiTexCoord1sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer()}.
multiTexCoord1sv(Target,{S}) ->  multiTexCoord1s(Target,S).
-spec multiTexCoord2d(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2d(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5342),
  ok.

%% @equiv multiTexCoord2d(Target,S,T)
-spec multiTexCoord2dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2dv(Target,{S,T}) ->  multiTexCoord2d(Target,S,T).
-spec multiTexCoord2f(Target, S, T) -> 'ok' when Target :: enum(),S :: float(),T :: float().
multiTexCoord2f(Target,S,T) when is_integer(Target),is_float(S),is_float(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5343),
  ok.

%% @equiv multiTexCoord2f(Target,S,T)
-spec multiTexCoord2fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float()}.
multiTexCoord2fv(Target,{S,T}) ->  multiTexCoord2f(Target,S,T).
-spec multiTexCoord2i(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2i(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5344),
  ok.

%% @equiv multiTexCoord2i(Target,S,T)
-spec multiTexCoord2iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2iv(Target,{S,T}) ->  multiTexCoord2i(Target,S,T).
-spec multiTexCoord2s(Target, S, T) -> 'ok' when Target :: enum(),S :: integer(),T :: integer().
multiTexCoord2s(Target,S,T) when is_integer(Target),is_integer(S),is_integer(T) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,5345),
  ok.

%% @equiv multiTexCoord2s(Target,S,T)
-spec multiTexCoord2sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer()}.
multiTexCoord2sv(Target,{S,T}) ->  multiTexCoord2s(Target,S,T).
-spec multiTexCoord3d(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3d(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5346),
  ok.

%% @equiv multiTexCoord3d(Target,S,T,R)
-spec multiTexCoord3dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3dv(Target,{S,T,R}) ->  multiTexCoord3d(Target,S,T,R).
-spec multiTexCoord3f(Target, S, T, R) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float().
multiTexCoord3f(Target,S,T,R) when is_integer(Target),is_float(S),is_float(T),is_float(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5347),
  ok.

%% @equiv multiTexCoord3f(Target,S,T,R)
-spec multiTexCoord3fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float()}.
multiTexCoord3fv(Target,{S,T,R}) ->  multiTexCoord3f(Target,S,T,R).
-spec multiTexCoord3i(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3i(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5348),
  ok.

%% @equiv multiTexCoord3i(Target,S,T,R)
-spec multiTexCoord3iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3iv(Target,{S,T,R}) ->  multiTexCoord3i(Target,S,T,R).
-spec multiTexCoord3s(Target, S, T, R) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer().
multiTexCoord3s(Target,S,T,R) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,5349),
  ok.

%% @equiv multiTexCoord3s(Target,S,T,R)
-spec multiTexCoord3sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer()}.
multiTexCoord3sv(Target,{S,T,R}) ->  multiTexCoord3s(Target,S,T,R).
-spec multiTexCoord4d(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4d(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5350),
  ok.

%% @equiv multiTexCoord4d(Target,S,T,R,Q)
-spec multiTexCoord4dv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4dv(Target,{S,T,R,Q}) ->  multiTexCoord4d(Target,S,T,R,Q).
-spec multiTexCoord4f(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: float(),T :: float(),R :: float(),Q :: float().
multiTexCoord4f(Target,S,T,R,Q) when is_integer(Target),is_float(S),is_float(T),is_float(R),is_float(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5351),
  ok.

%% @equiv multiTexCoord4f(Target,S,T,R,Q)
-spec multiTexCoord4fv(Target :: enum(),V) -> 'ok' when V :: {S :: float(),T :: float(),R :: float(),Q :: float()}.
multiTexCoord4fv(Target,{S,T,R,Q}) ->  multiTexCoord4f(Target,S,T,R,Q).
-spec multiTexCoord4i(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4i(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5352),
  ok.

%% @equiv multiTexCoord4i(Target,S,T,R,Q)
-spec multiTexCoord4iv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4iv(Target,{S,T,R,Q}) ->  multiTexCoord4i(Target,S,T,R,Q).
-spec multiTexCoord4s(Target, S, T, R, Q) -> 'ok' when Target :: enum(),S :: integer(),T :: integer(),R :: integer(),Q :: integer().
multiTexCoord4s(Target,S,T,R,Q) when is_integer(Target),is_integer(S),is_integer(T),is_integer(R),is_integer(Q) ->
  IF = get_interface(),
  IF:queue_cmd(Target,S,T,R,Q,5353),
  ok.

%% @equiv multiTexCoord4s(Target,S,T,R,Q)
-spec multiTexCoord4sv(Target :: enum(),V) -> 'ok' when V :: {S :: integer(),T :: integer(),R :: integer(),Q :: integer()}.
multiTexCoord4sv(Target,{S,T,R,Q}) ->  multiTexCoord4s(Target,S,T,R,Q).
-spec loadTransposeMatrixf(M) -> 'ok' when M :: matrix().
loadTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5354),
  ok.

-spec loadTransposeMatrixd(M) -> 'ok' when M :: matrix().
loadTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5355),
  ok.

-spec multTransposeMatrixf(M) -> 'ok' when M :: matrix().
multTransposeMatrixf(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5356),
  ok.

-spec multTransposeMatrixd(M) -> 'ok' when M :: matrix().
multTransposeMatrixd(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5357),
  ok.

-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> 'ok' when SfactorRGB :: enum(),DfactorRGB :: enum(),SfactorAlpha :: enum(),DfactorAlpha :: enum().
blendFuncSeparate(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha) when is_integer(SfactorRGB),is_integer(DfactorRGB),is_integer(SfactorAlpha),is_integer(DfactorAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(SfactorRGB,DfactorRGB,SfactorAlpha,DfactorAlpha,5358),
  ok.

-spec multiDrawArrays(Mode, First, Count) -> 'ok' when Mode :: enum(),First :: [integer()]|mem(),Count :: [integer()]|mem().
multiDrawArrays(Mode,First,Count) when is_integer(Mode),is_list(First) orelse is_tuple(First) orelse is_binary(First),is_list(Count) orelse is_tuple(Count) orelse is_binary(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,5359),
  ok.

-spec pointParameterf(Pname, Param) -> 'ok' when Pname :: enum(),Param :: float().
pointParameterf(Pname,Param) when is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5361),
  ok.

-spec pointParameterfv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameterfv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5362),
  ok.

-spec pointParameteri(Pname, Param) -> 'ok' when Pname :: enum(),Param :: integer().
pointParameteri(Pname,Param) when is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Param,5363),
  ok.

-spec pointParameteriv(Pname, Params) -> 'ok' when Pname :: enum(),Params :: tuple().
pointParameteriv(Pname,Params) when is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Params,5364),
  ok.

-spec fogCoordf(Coord) -> 'ok' when Coord :: float().
fogCoordf(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5365),
  ok.

%% @equiv fogCoordf(Coord)
-spec fogCoordfv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoordfv({Coord}) ->  fogCoordf(Coord).
-spec fogCoordd(Coord) -> 'ok' when Coord :: float().
fogCoordd(Coord) when is_float(Coord) ->
  IF = get_interface(),
  IF:queue_cmd(Coord,5366),
  ok.

%% @equiv fogCoordd(Coord)
-spec fogCoorddv(Coord) -> 'ok' when Coord :: {Coord :: float()}.
fogCoorddv({Coord}) ->  fogCoordd(Coord).
-spec fogCoordPointer(Type, Stride, Pointer) -> 'ok' when Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
fogCoordPointer(Type,Stride,Pointer) when is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Type,Stride,Pointer,5367),
  ok.

-spec secondaryColor3b(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3b(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5369),
  ok.

%% @equiv secondaryColor3b(Red,Green,Blue)
-spec secondaryColor3bv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3bv({Red,Green,Blue}) ->  secondaryColor3b(Red,Green,Blue).
-spec secondaryColor3d(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3d(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5370),
  ok.

%% @equiv secondaryColor3d(Red,Green,Blue)
-spec secondaryColor3dv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3dv({Red,Green,Blue}) ->  secondaryColor3d(Red,Green,Blue).
-spec secondaryColor3f(Red, Green, Blue) -> 'ok' when Red :: float(),Green :: float(),Blue :: float().
secondaryColor3f(Red,Green,Blue) when is_float(Red),is_float(Green),is_float(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5371),
  ok.

%% @equiv secondaryColor3f(Red,Green,Blue)
-spec secondaryColor3fv(V) -> 'ok' when V :: {Red :: float(),Green :: float(),Blue :: float()}.
secondaryColor3fv({Red,Green,Blue}) ->  secondaryColor3f(Red,Green,Blue).
-spec secondaryColor3i(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3i(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5372),
  ok.

%% @equiv secondaryColor3i(Red,Green,Blue)
-spec secondaryColor3iv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3iv({Red,Green,Blue}) ->  secondaryColor3i(Red,Green,Blue).
-spec secondaryColor3s(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3s(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5373),
  ok.

%% @equiv secondaryColor3s(Red,Green,Blue)
-spec secondaryColor3sv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3sv({Red,Green,Blue}) ->  secondaryColor3s(Red,Green,Blue).
-spec secondaryColor3ub(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ub(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5374),
  ok.

%% @equiv secondaryColor3ub(Red,Green,Blue)
-spec secondaryColor3ubv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3ubv({Red,Green,Blue}) ->  secondaryColor3ub(Red,Green,Blue).
-spec secondaryColor3ui(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3ui(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5375),
  ok.

%% @equiv secondaryColor3ui(Red,Green,Blue)
-spec secondaryColor3uiv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3uiv({Red,Green,Blue}) ->  secondaryColor3ui(Red,Green,Blue).
-spec secondaryColor3us(Red, Green, Blue) -> 'ok' when Red :: integer(),Green :: integer(),Blue :: integer().
secondaryColor3us(Red,Green,Blue) when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,5376),
  ok.

%% @equiv secondaryColor3us(Red,Green,Blue)
-spec secondaryColor3usv(V) -> 'ok' when V :: {Red :: integer(),Green :: integer(),Blue :: integer()}.
secondaryColor3usv({Red,Green,Blue}) ->  secondaryColor3us(Red,Green,Blue).
-spec secondaryColorPointer(Size, Type, Stride, Pointer) -> 'ok' when Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
secondaryColorPointer(Size,Type,Stride,Pointer) when is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Size,Type,Stride,Pointer,5377),
  ok.

-spec windowPos2d(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2d(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5379),
  ok.

%% @equiv windowPos2d(X,Y)
-spec windowPos2dv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2dv({X,Y}) ->  windowPos2d(X,Y).
-spec windowPos2f(X, Y) -> 'ok' when X :: float(),Y :: float().
windowPos2f(X,Y) when is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5380),
  ok.

%% @equiv windowPos2f(X,Y)
-spec windowPos2fv(V) -> 'ok' when V :: {X :: float(),Y :: float()}.
windowPos2fv({X,Y}) ->  windowPos2f(X,Y).
-spec windowPos2i(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2i(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5381),
  ok.

%% @equiv windowPos2i(X,Y)
-spec windowPos2iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2iv({X,Y}) ->  windowPos2i(X,Y).
-spec windowPos2s(X, Y) -> 'ok' when X :: integer(),Y :: integer().
windowPos2s(X,Y) when is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,5382),
  ok.

%% @equiv windowPos2s(X,Y)
-spec windowPos2sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
windowPos2sv({X,Y}) ->  windowPos2s(X,Y).
-spec windowPos3d(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3d(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5383),
  ok.

%% @equiv windowPos3d(X,Y,Z)
-spec windowPos3dv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3dv({X,Y,Z}) ->  windowPos3d(X,Y,Z).
-spec windowPos3f(X, Y, Z) -> 'ok' when X :: float(),Y :: float(),Z :: float().
windowPos3f(X,Y,Z) when is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5384),
  ok.

%% @equiv windowPos3f(X,Y,Z)
-spec windowPos3fv(V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
windowPos3fv({X,Y,Z}) ->  windowPos3f(X,Y,Z).
-spec windowPos3i(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3i(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5385),
  ok.

%% @equiv windowPos3i(X,Y,Z)
-spec windowPos3iv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3iv({X,Y,Z}) ->  windowPos3i(X,Y,Z).
-spec windowPos3s(X, Y, Z) -> 'ok' when X :: integer(),Y :: integer(),Z :: integer().
windowPos3s(X,Y,Z) when is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(X,Y,Z,5386),
  ok.

%% @equiv windowPos3s(X,Y,Z)
-spec windowPos3sv(V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
windowPos3sv({X,Y,Z}) ->  windowPos3s(X,Y,Z).
-spec blendColor(Red, Green, Blue, Alpha) -> 'ok' when Red :: clamp(),Green :: clamp(),Blue :: clamp(),Alpha :: clamp().
blendColor(Red,Green,Blue,Alpha) when is_float(Red),is_float(Green),is_float(Blue),is_float(Alpha) ->
  IF = get_interface(),
  IF:queue_cmd(Red,Green,Blue,Alpha,5387),
  ok.

-spec blendEquation(Mode) -> 'ok' when Mode :: enum().
blendEquation(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5388),
  ok.

-spec genQueries(N) -> [integer()] when N :: integer().
genQueries(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5389),
  rec(5389).

-spec deleteQueries(Ids) -> 'ok' when Ids :: [integer()].
deleteQueries(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5390),
  ok.

-spec isQuery(Id) -> 0|1 when Id :: integer().
isQuery(Id) when is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Id,5391),
  rec(5391).

-spec beginQuery(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
beginQuery(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5392),
  ok.

-spec endQuery(Target) -> 'ok' when Target :: enum().
endQuery(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5393),
  ok.

-spec getQueryiv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getQueryiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5394),
  rec(5394).

-spec getQueryObjectiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5395),
  rec(5395).

-spec getQueryObjectuiv(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectuiv(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5396),
  rec(5396).

-spec bindBuffer(Target, Buffer) -> 'ok' when Target :: enum(),Buffer :: integer().
bindBuffer(Target,Buffer) when is_integer(Target),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Buffer,5397),
  ok.

-spec deleteBuffers(Buffers) -> 'ok' when Buffers :: [integer()].
deleteBuffers(Buffers) when is_list(Buffers) ->
  IF = get_interface(),
  N = length(Buffers),
  IF:queue_cmd(N,Buffers,5398),
  ok.

-spec genBuffers(N) -> [integer()] when N :: integer().
genBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5399),
  rec(5399).

-spec isBuffer(Buffer) -> 0|1 when Buffer :: integer().
isBuffer(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5400),
  rec(5400).

-spec bufferData(Target, Size, Data, Usage) -> 'ok' when Target :: enum(),Size :: integer(),Data :: offset()|mem(),Usage :: enum().
bufferData(Target,Size,Data,Usage) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Usage) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Usage,5401),
  ok.

-spec bufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: offset()|mem().
bufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5403),
  ok.

-spec getBufferSubData(Target, Offset, Size, Data) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Data :: mem().
getBufferSubData(Target,Offset,Size,Data) when is_integer(Target),is_integer(Offset),is_integer(Size),is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Data,5405),
  rec(5405).

-spec getBufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getBufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5406),
  rec(5406).

-spec blendEquationSeparate(ModeRGB, ModeAlpha) -> 'ok' when ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparate(ModeRGB,ModeAlpha) when is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(ModeRGB,ModeAlpha,5407),
  ok.

-spec drawBuffers(Bufs) -> 'ok' when Bufs :: [enum()].
drawBuffers(Bufs) when is_list(Bufs) ->
  IF = get_interface(),
  N = length(Bufs),
  IF:queue_cmd(N,Bufs,5408),
  ok.

-spec stencilOpSeparate(Face, Sfail, Dpfail, Dppass) -> 'ok' when Face :: enum(),Sfail :: enum(),Dpfail :: enum(),Dppass :: enum().
stencilOpSeparate(Face,Sfail,Dpfail,Dppass) when is_integer(Face),is_integer(Sfail),is_integer(Dpfail),is_integer(Dppass) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Sfail,Dpfail,Dppass,5409),
  ok.

-spec stencilFuncSeparate(Face, Func, Ref, Mask) -> 'ok' when Face :: enum(),Func :: enum(),Ref :: integer(),Mask :: integer().
stencilFuncSeparate(Face,Func,Ref,Mask) when is_integer(Face),is_integer(Func),is_integer(Ref),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Func,Ref,Mask,5410),
  ok.

-spec stencilMaskSeparate(Face, Mask) -> 'ok' when Face :: enum(),Mask :: integer().
stencilMaskSeparate(Face,Mask) when is_integer(Face),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(Face,Mask,5411),
  ok.

-spec attachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
attachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5412),
  ok.

-spec bindAttribLocation(Program, Index, Name) -> 'ok' when Program :: integer(),Index :: integer(),Name :: string().
bindAttribLocation(Program,Index,Name) when is_integer(Program),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Index,NameBin,5413),
  ok.

-spec compileShader(Shader) -> 'ok' when Shader :: integer().
compileShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5414),
  ok.

-spec createProgram() -> integer().
createProgram()  ->
  IF = get_interface(),
  IF:queue_cmd(5415),
  rec(5415).

-spec createShader(Type) -> integer() when Type :: enum().
createShader(Type) when is_integer(Type) ->
  IF = get_interface(),
  IF:queue_cmd(Type,5416),
  rec(5416).

-spec deleteProgram(Program) -> 'ok' when Program :: integer().
deleteProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5417),
  ok.

-spec deleteShader(Shader) -> 'ok' when Shader :: integer().
deleteShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5418),
  ok.

-spec detachShader(Program, Shader) -> 'ok' when Program :: integer(),Shader :: integer().
detachShader(Program,Shader) when is_integer(Program),is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shader,5419),
  ok.

-spec disableVertexAttribArray(Index) -> 'ok' when Index :: integer().
disableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5420),
  ok.

-spec enableVertexAttribArray(Index) -> 'ok' when Index :: integer().
enableVertexAttribArray(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5421),
  ok.

-spec getActiveAttrib(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveAttrib(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5422),
  rec(5422).

-spec getActiveUniform(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getActiveUniform(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5423),
  rec(5423).

-spec getAttachedShaders(Program, MaxCount) -> [integer()] when Program :: integer(),MaxCount :: integer().
getAttachedShaders(Program,MaxCount) when is_integer(Program),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(Program,MaxCount,5424),
  rec(5424).

-spec getAttribLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getAttribLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5425),
  rec(5425).

-spec getProgramiv(Program, Pname) -> integer() when Program :: integer(),Pname :: enum().
getProgramiv(Program,Pname) when is_integer(Program),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,5426),
  rec(5426).

-spec getProgramInfoLog(Program, BufSize) -> string() when Program :: integer(),BufSize :: integer().
getProgramInfoLog(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5427),
  rec(5427).

-spec getShaderiv(Shader, Pname) -> integer() when Shader :: integer(),Pname :: enum().
getShaderiv(Shader,Pname) when is_integer(Shader),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,Pname,5428),
  rec(5428).

-spec getShaderInfoLog(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderInfoLog(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5429),
  rec(5429).

-spec getShaderSource(Shader, BufSize) -> string() when Shader :: integer(),BufSize :: integer().
getShaderSource(Shader,BufSize) when is_integer(Shader),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,BufSize,5430),
  rec(5430).

-spec getUniformLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getUniformLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5431),
  rec(5431).

-spec getUniformfv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformfv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5432),
  rec(5432).

-spec getUniformiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5433),
  rec(5433).

-spec getVertexAttribdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5434),
  rec(5434).

-spec getVertexAttribfv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribfv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5435),
  rec(5435).

-spec getVertexAttribiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5436),
  rec(5436).

-spec isProgram(Program) -> 0|1 when Program :: integer().
isProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5437),
  rec(5437).

-spec isShader(Shader) -> 0|1 when Shader :: integer().
isShader(Shader) when is_integer(Shader) ->
  IF = get_interface(),
  IF:queue_cmd(Shader,5438),
  rec(5438).

-spec linkProgram(Program) -> 'ok' when Program :: integer().
linkProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5439),
  ok.

-spec shaderSource(Shader, String) -> 'ok' when Shader :: integer(),String :: iolist().
shaderSource(Shader,String) when is_integer(Shader),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(Shader,Count,StringTemp,5440),
  ok.

-spec useProgram(Program) -> 'ok' when Program :: integer().
useProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5441),
  ok.

-spec uniform1f(Location, V0) -> 'ok' when Location :: integer(),V0 :: float().
uniform1f(Location,V0) when is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5442),
  ok.

-spec uniform2f(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float().
uniform2f(Location,V0,V1) when is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5443),
  ok.

-spec uniform3f(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
uniform3f(Location,V0,V1,V2) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5444),
  ok.

-spec uniform4f(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
uniform4f(Location,V0,V1,V2,V3) when is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5445),
  ok.

-spec uniform1i(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1i(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5446),
  ok.

-spec uniform2i(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2i(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5447),
  ok.

-spec uniform3i(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3i(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5448),
  ok.

-spec uniform4i(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4i(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5449),
  ok.

-spec uniform1fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5450),
  ok.

-spec uniform2fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5451),
  ok.

-spec uniform3fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5452),
  ok.

-spec uniform4fv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4fv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5453),
  ok.

-spec uniform1iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5454),
  ok.

-spec uniform2iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5455),
  ok.

-spec uniform3iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5456),
  ok.

-spec uniform4iv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4iv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5457),
  ok.

-spec uniformMatrix2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5458),
  ok.

-spec uniformMatrix3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5459),
  ok.

-spec uniformMatrix4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5460),
  ok.

-spec validateProgram(Program) -> 'ok' when Program :: integer().
validateProgram(Program) when is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Program,5461),
  ok.

-spec vertexAttrib1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5462),
  ok.

%% @equiv vertexAttrib1d(Index,X)
-spec vertexAttrib1dv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1dv(Index,{X}) ->  vertexAttrib1d(Index,X).
-spec vertexAttrib1f(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttrib1f(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5463),
  ok.

%% @equiv vertexAttrib1f(Index,X)
-spec vertexAttrib1fv(Index :: integer(),V) -> 'ok' when V :: {X :: float()}.
vertexAttrib1fv(Index,{X}) ->  vertexAttrib1f(Index,X).
-spec vertexAttrib1s(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttrib1s(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5464),
  ok.

%% @equiv vertexAttrib1s(Index,X)
-spec vertexAttrib1sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer()}.
vertexAttrib1sv(Index,{X}) ->  vertexAttrib1s(Index,X).
-spec vertexAttrib2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5465),
  ok.

%% @equiv vertexAttrib2d(Index,X,Y)
-spec vertexAttrib2dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2dv(Index,{X,Y}) ->  vertexAttrib2d(Index,X,Y).
-spec vertexAttrib2f(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttrib2f(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5466),
  ok.

%% @equiv vertexAttrib2f(Index,X,Y)
-spec vertexAttrib2fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float()}.
vertexAttrib2fv(Index,{X,Y}) ->  vertexAttrib2f(Index,X,Y).
-spec vertexAttrib2s(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttrib2s(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5467),
  ok.

%% @equiv vertexAttrib2s(Index,X,Y)
-spec vertexAttrib2sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer()}.
vertexAttrib2sv(Index,{X,Y}) ->  vertexAttrib2s(Index,X,Y).
-spec vertexAttrib3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5468),
  ok.

%% @equiv vertexAttrib3d(Index,X,Y,Z)
-spec vertexAttrib3dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3dv(Index,{X,Y,Z}) ->  vertexAttrib3d(Index,X,Y,Z).
-spec vertexAttrib3f(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttrib3f(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5469),
  ok.

%% @equiv vertexAttrib3f(Index,X,Y,Z)
-spec vertexAttrib3fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float()}.
vertexAttrib3fv(Index,{X,Y,Z}) ->  vertexAttrib3f(Index,X,Y,Z).
-spec vertexAttrib3s(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttrib3s(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5470),
  ok.

%% @equiv vertexAttrib3s(Index,X,Y,Z)
-spec vertexAttrib3sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer()}.
vertexAttrib3sv(Index,{X,Y,Z}) ->  vertexAttrib3s(Index,X,Y,Z).
-spec vertexAttrib4Nbv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nbv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5471),
  ok.

-spec vertexAttrib4Niv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Niv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5472),
  ok.

-spec vertexAttrib4Nsv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nsv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5473),
  ok.

-spec vertexAttrib4Nub(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4Nub(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5474),
  ok.

%% @equiv vertexAttrib4Nub(Index,X,Y,Z,W)
-spec vertexAttrib4Nubv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4Nubv(Index,{X,Y,Z,W}) ->  vertexAttrib4Nub(Index,X,Y,Z,W).
-spec vertexAttrib4Nuiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nuiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5475),
  ok.

-spec vertexAttrib4Nusv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4Nusv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5476),
  ok.

-spec vertexAttrib4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5477),
  ok.

-spec vertexAttrib4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5478),
  ok.

%% @equiv vertexAttrib4d(Index,X,Y,Z,W)
-spec vertexAttrib4dv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4dv(Index,{X,Y,Z,W}) ->  vertexAttrib4d(Index,X,Y,Z,W).
-spec vertexAttrib4f(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttrib4f(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5479),
  ok.

%% @equiv vertexAttrib4f(Index,X,Y,Z,W)
-spec vertexAttrib4fv(Index :: integer(),V) -> 'ok' when V :: {X :: float(),Y :: float(),Z :: float(),W :: float()}.
vertexAttrib4fv(Index,{X,Y,Z,W}) ->  vertexAttrib4f(Index,X,Y,Z,W).
-spec vertexAttrib4iv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4iv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5480),
  ok.

-spec vertexAttrib4s(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttrib4s(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5481),
  ok.

%% @equiv vertexAttrib4s(Index,X,Y,Z,W)
-spec vertexAttrib4sv(Index :: integer(),V) -> 'ok' when V :: {X :: integer(),Y :: integer(),Z :: integer(),W :: integer()}.
vertexAttrib4sv(Index,{X,Y,Z,W}) ->  vertexAttrib4s(Index,X,Y,Z,W).
-spec vertexAttrib4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5482),
  ok.

-spec vertexAttrib4uiv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4uiv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5483),
  ok.

-spec vertexAttrib4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttrib4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5484),
  ok.

-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Stride :: integer(),Pointer :: offset()|mem().
vertexAttribPointer(Index,Size,Type,Normalized,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Normalized,Stride,Pointer,5485),
  ok.

-spec uniformMatrix2x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5487),
  ok.

-spec uniformMatrix3x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5488),
  ok.

-spec uniformMatrix2x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5489),
  ok.

-spec uniformMatrix4x2fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5490),
  ok.

-spec uniformMatrix3x4fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5491),
  ok.

-spec uniformMatrix4x3fv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3fv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5492),
  ok.

-spec colorMaski(Index, R, G, B, A) -> 'ok' when Index :: integer(),R :: 0|1,G :: 0|1,B :: 0|1,A :: 0|1.
colorMaski(Index,R,G,B,A) when is_integer(Index),(0 =:= R) orelse (1 =:= R),(0 =:= G) orelse (1 =:= G),(0 =:= B) orelse (1 =:= B),(0 =:= A) orelse (1 =:= A) ->
  IF = get_interface(),
  IF:queue_cmd(Index,R,G,B,A,5493),
  ok.

-spec getBooleani_v(Target, Index) -> [0|1] when Target :: enum(),Index :: integer().
getBooleani_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5494),
  rec(5494).

-spec getIntegeri_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getIntegeri_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5495),
  rec(5495).

-spec enablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
enablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5496),
  ok.

-spec disablei(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
disablei(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5497),
  ok.

-spec isEnabledi(Target, Index) -> 0|1 when Target :: enum(),Index :: integer().
isEnabledi(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5498),
  rec(5498).

-spec beginTransformFeedback(PrimitiveMode) -> 'ok' when PrimitiveMode :: enum().
beginTransformFeedback(PrimitiveMode) when is_integer(PrimitiveMode) ->
  IF = get_interface(),
  IF:queue_cmd(PrimitiveMode,5499),
  ok.

-spec endTransformFeedback() -> 'ok'.
endTransformFeedback()  ->
  IF = get_interface(),
  IF:queue_cmd(5500),
  ok.

-spec bindBufferRange(Target, Index, Buffer, Offset, Size) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer(),Offset :: integer(),Size :: integer().
bindBufferRange(Target,Index,Buffer,Offset,Size) when is_integer(Target),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,Offset,Size,5501),
  ok.

-spec bindBufferBase(Target, Index, Buffer) -> 'ok' when Target :: enum(),Index :: integer(),Buffer :: integer().
bindBufferBase(Target,Index,Buffer) when is_integer(Target),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Buffer,5502),
  ok.

-spec transformFeedbackVaryings(Program, Varyings, BufferMode) -> 'ok' when Program :: integer(),Varyings :: iolist(),BufferMode :: enum().
transformFeedbackVaryings(Program,Varyings,BufferMode) when is_integer(Program),is_list(Varyings),is_integer(BufferMode) ->
  IF = get_interface(),
  VaryingsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Varyings ],
  Count = length(Varyings),
  IF:queue_cmd(Program,Count,VaryingsTemp,BufferMode,5503),
  ok.

-spec getTransformFeedbackVarying(Program, Index, BufSize) -> {Size :: integer(),Type :: enum(),Name :: string()} when Program :: integer(),Index :: integer(),BufSize :: integer().
getTransformFeedbackVarying(Program,Index,BufSize) when is_integer(Program),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Index,BufSize,5504),
  rec(5504).

-spec clampColor(Target, Clamp) -> 'ok' when Target :: enum(),Clamp :: enum().
clampColor(Target,Clamp) when is_integer(Target),is_integer(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Clamp,5505),
  ok.

-spec beginConditionalRender(Id, Mode) -> 'ok' when Id :: integer(),Mode :: enum().
beginConditionalRender(Id,Mode) when is_integer(Id),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Mode,5506),
  ok.

-spec endConditionalRender() -> 'ok'.
endConditionalRender()  ->
  IF = get_interface(),
  IF:queue_cmd(5507),
  ok.

-spec vertexAttribIPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribIPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5508),
  ok.

-spec getVertexAttribIiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5510),
  rec(5510).

-spec getVertexAttribIuiv(Index, Pname) -> {integer(),integer(),integer(),integer()} when Index :: integer(),Pname :: enum().
getVertexAttribIuiv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5511),
  rec(5511).

-spec vertexAttribI1i(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1i(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5512),
  ok.

-spec vertexAttribI2i(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2i(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5513),
  ok.

-spec vertexAttribI3i(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3i(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5514),
  ok.

-spec vertexAttribI4i(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4i(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5515),
  ok.

-spec vertexAttribI1ui(Index, X) -> 'ok' when Index :: integer(),X :: integer().
vertexAttribI1ui(Index,X) when is_integer(Index),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5516),
  ok.

-spec vertexAttribI2ui(Index, X, Y) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer().
vertexAttribI2ui(Index,X,Y) when is_integer(Index),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5517),
  ok.

-spec vertexAttribI3ui(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer().
vertexAttribI3ui(Index,X,Y,Z) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5518),
  ok.

-spec vertexAttribI4ui(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
vertexAttribI4ui(Index,X,Y,Z,W) when is_integer(Index),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5519),
  ok.

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
-spec vertexAttribI4bv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4bv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5520),
  ok.

-spec vertexAttribI4sv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4sv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5521),
  ok.

-spec vertexAttribI4ubv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4ubv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5522),
  ok.

-spec vertexAttribI4usv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
vertexAttribI4usv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5523),
  ok.

-spec getUniformuiv(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformuiv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5524),
  rec(5524).

-spec bindFragDataLocation(Program, Color, Name) -> 'ok' when Program :: integer(),Color :: integer(),Name :: string().
bindFragDataLocation(Program,Color,Name) when is_integer(Program),is_integer(Color),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Color,NameBin,5525),
  ok.

-spec getFragDataLocation(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataLocation(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5526),
  rec(5526).

-spec uniform1ui(Location, V0) -> 'ok' when Location :: integer(),V0 :: integer().
uniform1ui(Location,V0) when is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,5527),
  ok.

-spec uniform2ui(Location, V0, V1) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer().
uniform2ui(Location,V0,V1) when is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,5528),
  ok.

-spec uniform3ui(Location, V0, V1, V2) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
uniform3ui(Location,V0,V1,V2) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,5529),
  ok.

-spec uniform4ui(Location, V0, V1, V2, V3) -> 'ok' when Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
uniform4ui(Location,V0,V1,V2,V3) when is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Location,V0,V1,V2,V3,5530),
  ok.

-spec uniform1uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5531),
  ok.

-spec uniform2uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5532),
  ok.

-spec uniform3uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5533),
  ok.

-spec uniform4uiv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4uiv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5534),
  ok.

-spec texParameterIiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5535),
  ok.

-spec texParameterIuiv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
texParameterIuiv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5536),
  ok.

-spec getTexParameterIiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5537),
  rec(5537).

-spec getTexParameterIuiv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getTexParameterIuiv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5538),
  rec(5538).

-spec clearBufferiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5539),
  ok.

-spec clearBufferuiv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferuiv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5540),
  ok.

-spec clearBufferfv(Buffer, Drawbuffer, Value) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Value :: tuple().
clearBufferfv(Buffer,Drawbuffer,Value) when is_integer(Buffer),is_integer(Drawbuffer),is_tuple(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Value,5541),
  ok.

-spec clearBufferfi(Buffer, Drawbuffer, Depth, Stencil) -> 'ok' when Buffer :: enum(),Drawbuffer :: integer(),Depth :: float(),Stencil :: integer().
clearBufferfi(Buffer,Drawbuffer,Depth,Stencil) when is_integer(Buffer),is_integer(Drawbuffer),is_float(Depth),is_integer(Stencil) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Drawbuffer,Depth,Stencil,5542),
  ok.

-spec getStringi(Name, Index) -> string() when Name :: enum(),Index :: integer().
getStringi(Name,Index) when is_integer(Name),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Name,Index,5543),
  rec(5543).

-spec isRenderbuffer(Renderbuffer) -> 0|1 when Renderbuffer :: integer().
isRenderbuffer(Renderbuffer) when is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Renderbuffer,5544),
  rec(5544).

-spec bindRenderbuffer(Target, Renderbuffer) -> 'ok' when Target :: enum(),Renderbuffer :: integer().
bindRenderbuffer(Target,Renderbuffer) when is_integer(Target),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Renderbuffer,5545),
  ok.

-spec deleteRenderbuffers(Renderbuffers) -> 'ok' when Renderbuffers :: [integer()].
deleteRenderbuffers(Renderbuffers) when is_list(Renderbuffers) ->
  IF = get_interface(),
  N = length(Renderbuffers),
  IF:queue_cmd(N,Renderbuffers,5546),
  ok.

-spec genRenderbuffers(N) -> [integer()] when N :: integer().
genRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5547),
  rec(5547).

-spec renderbufferStorage(Target, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorage(Target,Internalformat,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,5548),
  ok.

-spec getRenderbufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getRenderbufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5549),
  rec(5549).

-spec isFramebuffer(Framebuffer) -> 0|1 when Framebuffer :: integer().
isFramebuffer(Framebuffer) when is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Framebuffer,5550),
  rec(5550).

-spec bindFramebuffer(Target, Framebuffer) -> 'ok' when Target :: enum(),Framebuffer :: integer().
bindFramebuffer(Target,Framebuffer) when is_integer(Target),is_integer(Framebuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Framebuffer,5551),
  ok.

-spec deleteFramebuffers(Framebuffers) -> 'ok' when Framebuffers :: [integer()].
deleteFramebuffers(Framebuffers) when is_list(Framebuffers) ->
  IF = get_interface(),
  N = length(Framebuffers),
  IF:queue_cmd(N,Framebuffers,5552),
  ok.

-spec genFramebuffers(N) -> [integer()] when N :: integer().
genFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5553),
  rec(5553).

-spec checkFramebufferStatus(Target) -> enum() when Target :: enum().
checkFramebufferStatus(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5554),
  rec(5554).

-spec framebufferTexture1D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture1D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5555),
  ok.

-spec framebufferTexture2D(Target, Attachment, Textarget, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture2D(Target,Attachment,Textarget,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,5556),
  ok.

-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> 'ok' when Target :: enum(),Attachment :: enum(),Textarget :: enum(),Texture :: integer(),Level :: integer(),Zoffset :: integer().
framebufferTexture3D(Target,Attachment,Textarget,Texture,Level,Zoffset) when is_integer(Target),is_integer(Attachment),is_integer(Textarget),is_integer(Texture),is_integer(Level),is_integer(Zoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Textarget,Texture,Level,Zoffset,5557),
  ok.

-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> 'ok' when Target :: enum(),Attachment :: enum(),Renderbuffertarget :: enum(),Renderbuffer :: integer().
framebufferRenderbuffer(Target,Attachment,Renderbuffertarget,Renderbuffer) when is_integer(Target),is_integer(Attachment),is_integer(Renderbuffertarget),is_integer(Renderbuffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Renderbuffertarget,Renderbuffer,5558),
  ok.

-spec getFramebufferAttachmentParameteriv(Target, Attachment, Pname) -> integer() when Target :: enum(),Attachment :: enum(),Pname :: enum().
getFramebufferAttachmentParameteriv(Target,Attachment,Pname) when is_integer(Target),is_integer(Attachment),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Pname,5559),
  rec(5559).

-spec generateMipmap(Target) -> 'ok' when Target :: enum().
generateMipmap(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5560),
  ok.

-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> 'ok' when SrcX0 :: integer(),SrcY0 :: integer(),SrcX1 :: integer(),SrcY1 :: integer(),DstX0 :: integer(),DstY0 :: integer(),DstX1 :: integer(),DstY1 :: integer(),Mask :: integer(),Filter :: enum().
blitFramebuffer(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter) when is_integer(SrcX0),is_integer(SrcY0),is_integer(SrcX1),is_integer(SrcY1),is_integer(DstX0),is_integer(DstY0),is_integer(DstX1),is_integer(DstY1),is_integer(Mask),is_integer(Filter) ->
  IF = get_interface(),
  IF:queue_cmd(SrcX0,SrcY0,SrcX1,SrcY1,DstX0,DstY0,DstX1,DstY1,Mask,Filter,5561),
  ok.

-spec renderbufferStorageMultisample(Target, Samples, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
renderbufferStorageMultisample(Target,Samples,Internalformat,Width,Height) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,5562),
  ok.

-spec framebufferTextureLayer(Target, Attachment, Texture, Level, Layer) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Layer :: integer().
framebufferTextureLayer(Target,Attachment,Texture,Level,Layer) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Layer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Layer,5563),
  ok.

-spec flushMappedBufferRange(Target, Offset, Length) -> 'ok' when Target :: enum(),Offset :: integer(),Length :: integer().
flushMappedBufferRange(Target,Offset,Length) when is_integer(Target),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Length,5564),
  ok.

-spec bindVertexArray(Array) -> 'ok' when Array :: integer().
bindVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5565),
  ok.

-spec deleteVertexArrays(Arrays) -> 'ok' when Arrays :: [integer()].
deleteVertexArrays(Arrays) when is_list(Arrays) ->
  IF = get_interface(),
  N = length(Arrays),
  IF:queue_cmd(N,Arrays,5566),
  ok.

-spec genVertexArrays(N) -> [integer()] when N :: integer().
genVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5567),
  rec(5567).

-spec isVertexArray(Array) -> 0|1 when Array :: integer().
isVertexArray(Array) when is_integer(Array) ->
  IF = get_interface(),
  IF:queue_cmd(Array,5568),
  rec(5568).

-spec drawArraysInstanced(Mode, First, Count, Instancecount) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Instancecount :: integer().
drawArraysInstanced(Mode,First,Count,Instancecount) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,5569),
  ok.

-spec drawElementsInstanced(Mode, Count, Type, Indices, Instancecount) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Instancecount :: integer().
drawElementsInstanced(Mode,Count,Type,Indices,Instancecount) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,5570),
  ok.

-spec texBuffer(Target, Internalformat, Buffer) -> 'ok' when Target :: enum(),Internalformat :: enum(),Buffer :: integer().
texBuffer(Target,Internalformat,Buffer) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,5572),
  ok.

-spec primitiveRestartIndex(Index) -> 'ok' when Index :: integer().
primitiveRestartIndex(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5573),
  ok.

-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> 'ok' when ReadTarget :: enum(),WriteTarget :: enum(),ReadOffset :: integer(),WriteOffset :: integer(),Size :: integer().
copyBufferSubData(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size) when is_integer(ReadTarget),is_integer(WriteTarget),is_integer(ReadOffset),is_integer(WriteOffset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(ReadTarget,WriteTarget,ReadOffset,WriteOffset,Size,5574),
  ok.

-spec getUniformIndices(Program, UniformNames) -> [integer()] when Program :: integer(),UniformNames :: iolist().
getUniformIndices(Program,UniformNames) when is_integer(Program),is_list(UniformNames) ->
  IF = get_interface(),
  UniformNamesTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- UniformNames ],
  UniformCount = length(UniformNames),
  IF:queue_cmd(Program,UniformCount,UniformNamesTemp,5575),
  rec(5575).

-spec getActiveUniformsiv(Program, UniformIndices, Pname) -> [integer()] when Program :: integer(),UniformIndices :: [integer()],Pname :: enum().
getActiveUniformsiv(Program,UniformIndices,Pname) when is_integer(Program),is_list(UniformIndices),is_integer(Pname) ->
  IF = get_interface(),
  UniformCount = length(UniformIndices),
  IF:queue_cmd(Program,UniformCount,UniformIndices,Pname,5576),
  rec(5576).

-spec getActiveUniformName(Program, UniformIndex, BufSize) -> string() when Program :: integer(),UniformIndex :: integer(),BufSize :: integer().
getActiveUniformName(Program,UniformIndex,BufSize) when is_integer(Program),is_integer(UniformIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformIndex,BufSize,5577),
  rec(5577).

-spec getUniformBlockIndex(Program, UniformBlockName) -> integer() when Program :: integer(),UniformBlockName :: string().
getUniformBlockIndex(Program,UniformBlockName) when is_integer(Program),is_list(UniformBlockName) ->
  IF = get_interface(),
  UniformBlockNameBin = unicode:characters_to_binary([UniformBlockName|[0]]),
  IF:queue_cmd(Program,UniformBlockNameBin,5578),
  rec(5578).

-spec getActiveUniformBlockiv(Program, UniformBlockIndex, Pname, Params) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),Pname :: enum(),Params :: mem().
getActiveUniformBlockiv(Program,UniformBlockIndex,Pname,Params) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(Pname),is_tuple(Params) orelse is_binary(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,Pname,Params,5579),
  rec(5579).

-spec getActiveUniformBlockName(Program, UniformBlockIndex, BufSize) -> string() when Program :: integer(),UniformBlockIndex :: integer(),BufSize :: integer().
getActiveUniformBlockName(Program,UniformBlockIndex,BufSize) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,BufSize,5580),
  rec(5580).

-spec uniformBlockBinding(Program, UniformBlockIndex, UniformBlockBinding) -> 'ok' when Program :: integer(),UniformBlockIndex :: integer(),UniformBlockBinding :: integer().
uniformBlockBinding(Program,UniformBlockIndex,UniformBlockBinding) when is_integer(Program),is_integer(UniformBlockIndex),is_integer(UniformBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,UniformBlockIndex,UniformBlockBinding,5581),
  ok.

-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawElementsBaseVertex(Mode,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Basevertex,5582),
  ok.

-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> 'ok' when Mode :: enum(),Start :: integer(),End :: integer(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Basevertex :: integer().
drawRangeElementsBaseVertex(Mode,Start,End,Count,Type,Indices,Basevertex) when is_integer(Mode),is_integer(Start),is_integer(End),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Start,End,Count,Type,Indices,Basevertex,5584),
  ok.

-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Instancecount, Basevertex) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Instancecount :: integer(),Basevertex :: integer().
drawElementsInstancedBaseVertex(Mode,Count,Type,Indices,Instancecount,Basevertex) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,5586),
  ok.

-spec provokingVertex(Mode) -> 'ok' when Mode :: enum().
provokingVertex(Mode) when is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,5588),
  ok.

-spec fenceSync(Condition, Flags) -> integer() when Condition :: enum(),Flags :: integer().
fenceSync(Condition,Flags) when is_integer(Condition),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Condition,Flags,5589),
  rec(5589).

-spec isSync(Sync) -> 0|1 when Sync :: integer().
isSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5590),
  rec(5590).

-spec deleteSync(Sync) -> 'ok' when Sync :: integer().
deleteSync(Sync) when is_integer(Sync) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,5591),
  ok.

-spec clientWaitSync(Sync, Flags, Timeout) -> enum() when Sync :: integer(),Flags :: integer(),Timeout :: integer().
clientWaitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5592),
  rec(5592).

-spec waitSync(Sync, Flags, Timeout) -> 'ok' when Sync :: integer(),Flags :: integer(),Timeout :: integer().
waitSync(Sync,Flags,Timeout) when is_integer(Sync),is_integer(Flags),is_integer(Timeout) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Flags,Timeout,5593),
  ok.

-spec getInteger64v(Pname) -> [integer()] when Pname :: enum().
getInteger64v(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5594),
  rec(5594).

-spec getSynciv(Sync, Pname, BufSize) -> [integer()] when Sync :: integer(),Pname :: enum(),BufSize :: integer().
getSynciv(Sync,Pname,BufSize) when is_integer(Sync),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Sync,Pname,BufSize,5595),
  rec(5595).

-spec getInteger64i_v(Target, Index) -> [integer()] when Target :: enum(),Index :: integer().
getInteger64i_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5596),
  rec(5596).

-spec getBufferParameteri64v(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameteri64v(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5597),
  rec(5597).

-spec framebufferTexture(Target, Attachment, Texture, Level) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer().
framebufferTexture(Target,Attachment,Texture,Level) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,5598),
  ok.

-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Fixedsamplelocations :: 0|1.
texImage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5599),
  ok.

-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer(),Fixedsamplelocations :: 0|1.
texImage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5600),
  ok.

-spec getMultisamplefv(Pname, Index) -> {float(),float()} when Pname :: enum(),Index :: integer().
getMultisamplefv(Pname,Index) when is_integer(Pname),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Index,5601),
  rec(5601).

-spec sampleMaski(MaskNumber, Mask) -> 'ok' when MaskNumber :: integer(),Mask :: integer().
sampleMaski(MaskNumber,Mask) when is_integer(MaskNumber),is_integer(Mask) ->
  IF = get_interface(),
  IF:queue_cmd(MaskNumber,Mask,5602),
  ok.

-spec bindFragDataLocationIndexed(Program, ColorNumber, Index, Name) -> 'ok' when Program :: integer(),ColorNumber :: integer(),Index :: integer(),Name :: string().
bindFragDataLocationIndexed(Program,ColorNumber,Index,Name) when is_integer(Program),is_integer(ColorNumber),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ColorNumber,Index,NameBin,5603),
  ok.

-spec getFragDataIndex(Program, Name) -> integer() when Program :: integer(),Name :: string().
getFragDataIndex(Program,Name) when is_integer(Program),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,NameBin,5604),
  rec(5604).

-spec genSamplers(Count) -> [integer()] when Count :: integer().
genSamplers(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5605),
  rec(5605).

-spec deleteSamplers(Samplers) -> 'ok' when Samplers :: [integer()].
deleteSamplers(Samplers) when is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(Count,Samplers,5606),
  ok.

-spec isSampler(Sampler) -> 0|1 when Sampler :: integer().
isSampler(Sampler) when is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,5607),
  rec(5607).

-spec bindSampler(Unit, Sampler) -> 'ok' when Unit :: integer(),Sampler :: integer().
bindSampler(Unit,Sampler) when is_integer(Unit),is_integer(Sampler) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Sampler,5608),
  ok.

-spec samplerParameteri(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: integer().
samplerParameteri(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5609),
  ok.

-spec samplerParameteriv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameteriv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5610),
  ok.

-spec samplerParameterf(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: float().
samplerParameterf(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_float(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5611),
  ok.

-spec samplerParameterfv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [float()].
samplerParameterfv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5612),
  ok.

-spec samplerParameterIiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5613),
  ok.

-spec samplerParameterIuiv(Sampler, Pname, Param) -> 'ok' when Sampler :: integer(),Pname :: enum(),Param :: [integer()].
samplerParameterIuiv(Sampler,Pname,Param) when is_integer(Sampler),is_integer(Pname),is_list(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,Param,5614),
  ok.

-spec getSamplerParameteriv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameteriv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5615),
  rec(5615).

-spec getSamplerParameterIiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5616),
  rec(5616).

-spec getSamplerParameterfv(Sampler, Pname) -> [float()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterfv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5617),
  rec(5617).

-spec getSamplerParameterIuiv(Sampler, Pname) -> [integer()] when Sampler :: integer(),Pname :: enum().
getSamplerParameterIuiv(Sampler,Pname) when is_integer(Sampler),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Sampler,Pname,5618),
  rec(5618).

-spec queryCounter(Id, Target) -> 'ok' when Id :: integer(),Target :: enum().
queryCounter(Id,Target) when is_integer(Id),is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Target,5619),
  ok.

-spec getQueryObjecti64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjecti64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5620),
  rec(5620).

-spec getQueryObjectui64v(Id, Pname) -> integer() when Id :: integer(),Pname :: enum().
getQueryObjectui64v(Id,Pname) when is_integer(Id),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Pname,5621),
  rec(5621).

-spec vertexAttribDivisor(Index, Divisor) -> 'ok' when Index :: integer(),Divisor :: integer().
vertexAttribDivisor(Index,Divisor) when is_integer(Index),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Divisor,5622),
  ok.

-spec minSampleShading(Value) -> 'ok' when Value :: float().
minSampleShading(Value) when is_float(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Value,5623),
  ok.

-spec blendEquationi(Buf, Mode) -> 'ok' when Buf :: integer(),Mode :: enum().
blendEquationi(Buf,Mode) when is_integer(Buf),is_integer(Mode) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Mode,5624),
  ok.

-spec blendEquationSeparatei(Buf, ModeRGB, ModeAlpha) -> 'ok' when Buf :: integer(),ModeRGB :: enum(),ModeAlpha :: enum().
blendEquationSeparatei(Buf,ModeRGB,ModeAlpha) when is_integer(Buf),is_integer(ModeRGB),is_integer(ModeAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,ModeRGB,ModeAlpha,5625),
  ok.

-spec blendFunci(Buf, Src, Dst) -> 'ok' when Buf :: integer(),Src :: enum(),Dst :: enum().
blendFunci(Buf,Src,Dst) when is_integer(Buf),is_integer(Src),is_integer(Dst) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,Src,Dst,5626),
  ok.

-spec blendFuncSeparatei(Buf, SrcRGB, DstRGB, SrcAlpha, DstAlpha) -> 'ok' when Buf :: integer(),SrcRGB :: enum(),DstRGB :: enum(),SrcAlpha :: enum(),DstAlpha :: enum().
blendFuncSeparatei(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha) when is_integer(Buf),is_integer(SrcRGB),is_integer(DstRGB),is_integer(SrcAlpha),is_integer(DstAlpha) ->
  IF = get_interface(),
  IF:queue_cmd(Buf,SrcRGB,DstRGB,SrcAlpha,DstAlpha,5627),
  ok.

-spec drawArraysIndirect(Mode, Indirect) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem().
drawArraysIndirect(Mode,Indirect) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,5628),
  ok.

-spec drawElementsIndirect(Mode, Type, Indirect) -> 'ok' when Mode :: enum(),Type :: enum(),Indirect :: offset()|mem().
drawElementsIndirect(Mode,Type,Indirect) when is_integer(Mode),is_integer(Type),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Type,Indirect,5630),
  ok.

-spec uniform1d(Location, X) -> 'ok' when Location :: integer(),X :: float().
uniform1d(Location,X) when is_integer(Location),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5632),
  ok.

-spec uniform2d(Location, X, Y) -> 'ok' when Location :: integer(),X :: float(),Y :: float().
uniform2d(Location,X,Y) when is_integer(Location),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5633),
  ok.

-spec uniform3d(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float().
uniform3d(Location,X,Y,Z) when is_integer(Location),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5634),
  ok.

-spec uniform4d(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
uniform4d(Location,X,Y,Z,W) when is_integer(Location),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5635),
  ok.

-spec uniform1dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [float()].
uniform1dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5636),
  ok.

-spec uniform2dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float()}].
uniform2dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5637),
  ok.

-spec uniform3dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float()}].
uniform3dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5638),
  ok.

-spec uniform4dv(Location, Value) -> 'ok' when Location :: integer(),Value :: [{float(),float(),float(),float()}].
uniform4dv(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5639),
  ok.

-spec uniformMatrix2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
uniformMatrix2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5640),
  ok.

-spec uniformMatrix3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5641),
  ok.

-spec uniformMatrix4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5642),
  ok.

-spec uniformMatrix2x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix2x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5643),
  ok.

-spec uniformMatrix2x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix2x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5644),
  ok.

-spec uniformMatrix3x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
uniformMatrix3x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5645),
  ok.

-spec uniformMatrix3x4dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix3x4dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5646),
  ok.

-spec uniformMatrix4x2dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x2dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5647),
  ok.

-spec uniformMatrix4x3dv(Location, Transpose, Value) -> 'ok' when Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
uniformMatrix4x3dv(Location,Transpose,Value) when is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Transpose,Value,5648),
  ok.

-spec getUniformdv(Program, Location) -> matrix() when Program :: integer(),Location :: integer().
getUniformdv(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5649),
  rec(5649).

-spec getSubroutineUniformLocation(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineUniformLocation(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5650),
  rec(5650).

-spec getSubroutineIndex(Program, Shadertype, Name) -> integer() when Program :: integer(),Shadertype :: enum(),Name :: string().
getSubroutineIndex(Program,Shadertype,Name) when is_integer(Program),is_integer(Shadertype),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,Shadertype,NameBin,5651),
  rec(5651).

-spec getActiveSubroutineUniformName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineUniformName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5652),
  rec(5652).

-spec getActiveSubroutineName(Program, Shadertype, Index, Bufsize) -> string() when Program :: integer(),Shadertype :: enum(),Index :: integer(),Bufsize :: integer().
getActiveSubroutineName(Program,Shadertype,Index,Bufsize) when is_integer(Program),is_integer(Shadertype),is_integer(Index),is_integer(Bufsize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Index,Bufsize,5653),
  rec(5653).

-spec uniformSubroutinesuiv(Shadertype, Indices) -> 'ok' when Shadertype :: enum(),Indices :: [integer()].
uniformSubroutinesuiv(Shadertype,Indices) when is_integer(Shadertype),is_list(Indices) ->
  IF = get_interface(),
  Count = length(Indices),
  IF:queue_cmd(Shadertype,Count,Indices,5654),
  ok.

-spec getUniformSubroutineuiv(Shadertype, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Shadertype :: enum(),Location :: integer().
getUniformSubroutineuiv(Shadertype,Location) when is_integer(Shadertype),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Location,5655),
  rec(5655).

-spec getProgramStageiv(Program, Shadertype, Pname) -> integer() when Program :: integer(),Shadertype :: enum(),Pname :: enum().
getProgramStageiv(Program,Shadertype,Pname) when is_integer(Program),is_integer(Shadertype),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Shadertype,Pname,5656),
  rec(5656).

-spec patchParameteri(Pname, Value) -> 'ok' when Pname :: enum(),Value :: integer().
patchParameteri(Pname,Value) when is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Value,5657),
  ok.

-spec patchParameterfv(Pname, Values) -> 'ok' when Pname :: enum(),Values :: [float()].
patchParameterfv(Pname,Values) when is_integer(Pname),is_list(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,Values,5658),
  ok.

-spec bindTransformFeedback(Target, Id) -> 'ok' when Target :: enum(),Id :: integer().
bindTransformFeedback(Target,Id) when is_integer(Target),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Id,5659),
  ok.

-spec deleteTransformFeedbacks(Ids) -> 'ok' when Ids :: [integer()].
deleteTransformFeedbacks(Ids) when is_list(Ids) ->
  IF = get_interface(),
  N = length(Ids),
  IF:queue_cmd(N,Ids,5660),
  ok.

-spec genTransformFeedbacks(N) -> [integer()] when N :: integer().
genTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5661),
  rec(5661).

-spec isTransformFeedback(Id) -> 0|1 when Id :: integer().
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

-spec drawTransformFeedback(Mode, Id) -> 'ok' when Mode :: enum(),Id :: integer().
drawTransformFeedback(Mode,Id) when is_integer(Mode),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,5665),
  ok.

-spec drawTransformFeedbackStream(Mode, Id, Stream) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer().
drawTransformFeedbackStream(Mode,Id,Stream) when is_integer(Mode),is_integer(Id),is_integer(Stream) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,5666),
  ok.

-spec beginQueryIndexed(Target, Index, Id) -> 'ok' when Target :: enum(),Index :: integer(),Id :: integer().
beginQueryIndexed(Target,Index,Id) when is_integer(Target),is_integer(Index),is_integer(Id) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Id,5667),
  ok.

-spec endQueryIndexed(Target, Index) -> 'ok' when Target :: enum(),Index :: integer().
endQueryIndexed(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5668),
  ok.

-spec getQueryIndexediv(Target, Index, Pname) -> integer() when Target :: enum(),Index :: integer(),Pname :: enum().
getQueryIndexediv(Target,Index,Pname) when is_integer(Target),is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Pname,5669),
  rec(5669).

-spec releaseShaderCompiler() -> 'ok'.
releaseShaderCompiler()  ->
  IF = get_interface(),
  IF:queue_cmd(5670),
  ok.

-spec shaderBinary(Shaders, Binaryformat, Binary) -> 'ok' when Shaders :: [integer()],Binaryformat :: enum(),Binary :: binary().
shaderBinary(Shaders,Binaryformat,Binary) when is_list(Shaders),is_integer(Binaryformat),is_binary(Binary) ->
  IF = get_interface(),
  Count = length(Shaders),
  IF:queue_cmd(Count,Shaders,Binaryformat,Binary,5671),
  ok.

-spec getShaderPrecisionFormat(Shadertype, Precisiontype) -> {Range :: {integer(),integer()},Precision :: integer()} when Shadertype :: enum(),Precisiontype :: enum().
getShaderPrecisionFormat(Shadertype,Precisiontype) when is_integer(Shadertype),is_integer(Precisiontype) ->
  IF = get_interface(),
  IF:queue_cmd(Shadertype,Precisiontype,5672),
  rec(5672).

-spec depthRangef(N, F) -> 'ok' when N :: float(),F :: float().
depthRangef(N,F) when is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(N,F,5673),
  ok.

-spec clearDepthf(D) -> 'ok' when D :: float().
clearDepthf(D) when is_float(D) ->
  IF = get_interface(),
  IF:queue_cmd(D,5674),
  ok.

-spec getProgramBinary(Program, BufSize) -> {BinaryFormat :: enum(),Binary :: binary()} when Program :: integer(),BufSize :: integer().
getProgramBinary(Program,BufSize) when is_integer(Program),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BufSize,5675),
  rec(5675).

-spec programBinary(Program, BinaryFormat, Binary) -> 'ok' when Program :: integer(),BinaryFormat :: enum(),Binary :: binary().
programBinary(Program,BinaryFormat,Binary) when is_integer(Program),is_integer(BinaryFormat),is_binary(Binary) ->
  IF = get_interface(),
  IF:queue_cmd(Program,BinaryFormat,Binary,5676),
  ok.

-spec programParameteri(Program, Pname, Value) -> 'ok' when Program :: integer(),Pname :: enum(),Value :: integer().
programParameteri(Program,Pname,Value) when is_integer(Program),is_integer(Pname),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Pname,Value,5677),
  ok.

-spec useProgramStages(Pipeline, Stages, Program) -> 'ok' when Pipeline :: integer(),Stages :: integer(),Program :: integer().
useProgramStages(Pipeline,Stages,Program) when is_integer(Pipeline),is_integer(Stages),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Stages,Program,5678),
  ok.

-spec activeShaderProgram(Pipeline, Program) -> 'ok' when Pipeline :: integer(),Program :: integer().
activeShaderProgram(Pipeline,Program) when is_integer(Pipeline),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Program,5679),
  ok.

-spec createShaderProgramv(Type, Strings) -> integer() when Type :: enum(),Strings :: iolist().
createShaderProgramv(Type,Strings) when is_integer(Type),is_list(Strings) ->
  IF = get_interface(),
  StringsTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Strings ],
  Count = length(Strings),
  IF:queue_cmd(Type,Count,StringsTemp,5680),
  rec(5680).

-spec bindProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
bindProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5681),
  ok.

-spec deleteProgramPipelines(Pipelines) -> 'ok' when Pipelines :: [integer()].
deleteProgramPipelines(Pipelines) when is_list(Pipelines) ->
  IF = get_interface(),
  N = length(Pipelines),
  IF:queue_cmd(N,Pipelines,5682),
  ok.

-spec genProgramPipelines(N) -> [integer()] when N :: integer().
genProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5683),
  rec(5683).

-spec isProgramPipeline(Pipeline) -> 0|1 when Pipeline :: integer().
isProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5684),
  rec(5684).

-spec getProgramPipelineiv(Pipeline, Pname) -> integer() when Pipeline :: integer(),Pname :: enum().
getProgramPipelineiv(Pipeline,Pname) when is_integer(Pipeline),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,Pname,5685),
  rec(5685).

-spec programUniform1i(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1i(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5686),
  ok.

-spec programUniform1iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5687),
  ok.

-spec programUniform1f(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1f(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5688),
  ok.

-spec programUniform1fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5689),
  ok.

-spec programUniform1d(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float().
programUniform1d(Program,Location,V0) when is_integer(Program),is_integer(Location),is_float(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5690),
  ok.

-spec programUniform1dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [float()].
programUniform1dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5691),
  ok.

-spec programUniform1ui(Program, Location, V0) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer().
programUniform1ui(Program,Location,V0) when is_integer(Program),is_integer(Location),is_integer(V0) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,5692),
  ok.

-spec programUniform1uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5693),
  ok.

-spec programUniform2i(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2i(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5694),
  ok.

-spec programUniform2iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5695),
  ok.

-spec programUniform2f(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2f(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5696),
  ok.

-spec programUniform2fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5697),
  ok.

-spec programUniform2d(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float().
programUniform2d(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5698),
  ok.

-spec programUniform2dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float()}].
programUniform2dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5699),
  ok.

-spec programUniform2ui(Program, Location, V0, V1) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer().
programUniform2ui(Program,Location,V0,V1) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,5700),
  ok.

-spec programUniform2uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5701),
  ok.

-spec programUniform3i(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3i(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5702),
  ok.

-spec programUniform3iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5703),
  ok.

-spec programUniform3f(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3f(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5704),
  ok.

-spec programUniform3fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5705),
  ok.

-spec programUniform3d(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float().
programUniform3d(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5706),
  ok.

-spec programUniform3dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float()}].
programUniform3dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5707),
  ok.

-spec programUniform3ui(Program, Location, V0, V1, V2) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer().
programUniform3ui(Program,Location,V0,V1,V2) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,5708),
  ok.

-spec programUniform3uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5709),
  ok.

-spec programUniform4i(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4i(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5710),
  ok.

-spec programUniform4iv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4iv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5711),
  ok.

-spec programUniform4f(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4f(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5712),
  ok.

-spec programUniform4fv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4fv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5713),
  ok.

-spec programUniform4d(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: float(),V1 :: float(),V2 :: float(),V3 :: float().
programUniform4d(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_float(V0),is_float(V1),is_float(V2),is_float(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5714),
  ok.

-spec programUniform4dv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{float(),float(),float(),float()}].
programUniform4dv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5715),
  ok.

-spec programUniform4ui(Program, Location, V0, V1, V2, V3) -> 'ok' when Program :: integer(),Location :: integer(),V0 :: integer(),V1 :: integer(),V2 :: integer(),V3 :: integer().
programUniform4ui(Program,Location,V0,V1,V2,V3) when is_integer(Program),is_integer(Location),is_integer(V0),is_integer(V1),is_integer(V2),is_integer(V3) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,V0,V1,V2,V3,5716),
  ok.

-spec programUniform4uiv(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4uiv(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5717),
  ok.

-spec programUniformMatrix2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5718),
  ok.

-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5719),
  ok.

-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5720),
  ok.

-spec programUniformMatrix2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float()}].
programUniformMatrix2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5721),
  ok.

-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5722),
  ok.

-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5723),
  ok.

-spec programUniformMatrix2x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5724),
  ok.

-spec programUniformMatrix3x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5725),
  ok.

-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5726),
  ok.

-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5727),
  ok.

-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5728),
  ok.

-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3fv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5729),
  ok.

-spec programUniformMatrix2x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5730),
  ok.

-spec programUniformMatrix3x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5731),
  ok.

-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix2x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5732),
  ok.

-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x2dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5733),
  ok.

-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix3x4dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5734),
  ok.

-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> 'ok' when Program :: integer(),Location :: integer(),Transpose :: 0|1,Value :: [{float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float(),float()}].
programUniformMatrix4x3dv(Program,Location,Transpose,Value) when is_integer(Program),is_integer(Location),(0 =:= Transpose) orelse (1 =:= Transpose),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Transpose,Value,5735),
  ok.

-spec validateProgramPipeline(Pipeline) -> 'ok' when Pipeline :: integer().
validateProgramPipeline(Pipeline) when is_integer(Pipeline) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,5736),
  ok.

-spec getProgramPipelineInfoLog(Pipeline, BufSize) -> string() when Pipeline :: integer(),BufSize :: integer().
getProgramPipelineInfoLog(Pipeline,BufSize) when is_integer(Pipeline),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Pipeline,BufSize,5737),
  rec(5737).

-spec vertexAttribL1d(Index, X) -> 'ok' when Index :: integer(),X :: float().
vertexAttribL1d(Index,X) when is_integer(Index),is_float(X) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,5738),
  ok.

-spec vertexAttribL2d(Index, X, Y) -> 'ok' when Index :: integer(),X :: float(),Y :: float().
vertexAttribL2d(Index,X,Y) when is_integer(Index),is_float(X),is_float(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,5739),
  ok.

-spec vertexAttribL3d(Index, X, Y, Z) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float().
vertexAttribL3d(Index,X,Y,Z) when is_integer(Index),is_float(X),is_float(Y),is_float(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,5740),
  ok.

-spec vertexAttribL4d(Index, X, Y, Z, W) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
vertexAttribL4d(Index,X,Y,Z,W) when is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,Z,W,5741),
  ok.

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
-spec vertexAttribLPointer(Index, Size, Type, Stride, Pointer) -> 'ok' when Index :: integer(),Size :: integer(),Type :: enum(),Stride :: integer(),Pointer :: offset()|mem().
vertexAttribLPointer(Index,Size,Type,Stride,Pointer) when is_integer(Index),is_integer(Size),is_integer(Type),is_integer(Stride),is_integer(Pointer) orelse is_tuple(Pointer) orelse is_binary(Pointer) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Size,Type,Stride,Pointer,5742),
  ok.

-spec getVertexAttribLdv(Index, Pname) -> {float(),float(),float(),float()} when Index :: integer(),Pname :: enum().
getVertexAttribLdv(Index,Pname) when is_integer(Index),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Pname,5744),
  rec(5744).

-spec viewportArrayv(First, V) -> 'ok' when First :: integer(),V :: [{float(),float(),float(),float()}].
viewportArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5745),
  ok.

-spec viewportIndexedf(Index, X, Y, W, H) -> 'ok' when Index :: integer(),X :: float(),Y :: float(),W :: float(),H :: float().
viewportIndexedf(Index,X,Y,W,H) when is_integer(Index),is_float(X),is_float(Y),is_float(W),is_float(H) ->
  IF = get_interface(),
  IF:queue_cmd(Index,X,Y,W,H,5746),
  ok.

-spec viewportIndexedfv(Index, V) -> 'ok' when Index :: integer(),V :: {float(),float(),float(),float()}.
viewportIndexedfv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5747),
  ok.

-spec scissorArrayv(First, V) -> 'ok' when First :: integer(),V :: [{integer(),integer(),integer(),integer()}].
scissorArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5748),
  ok.

-spec scissorIndexed(Index, Left, Bottom, Width, Height) -> 'ok' when Index :: integer(),Left :: integer(),Bottom :: integer(),Width :: integer(),Height :: integer().
scissorIndexed(Index,Left,Bottom,Width,Height) when is_integer(Index),is_integer(Left),is_integer(Bottom),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Index,Left,Bottom,Width,Height,5749),
  ok.

-spec scissorIndexedv(Index, V) -> 'ok' when Index :: integer(),V :: {integer(),integer(),integer(),integer()}.
scissorIndexedv(Index,V) when is_integer(Index),tuple_size(V) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Index,V,5750),
  ok.

-spec depthRangeArrayv(First, V) -> 'ok' when First :: integer(),V :: [{float(),float()}].
depthRangeArrayv(First,V) when is_integer(First),is_list(V) ->
  IF = get_interface(),
  Count = length(V),
  IF:queue_cmd(First,Count,V,5751),
  ok.

-spec depthRangeIndexed(Index, N, F) -> 'ok' when Index :: integer(),N :: float(),F :: float().
depthRangeIndexed(Index,N,F) when is_integer(Index),is_float(N),is_float(F) ->
  IF = get_interface(),
  IF:queue_cmd(Index,N,F,5752),
  ok.

-spec getFloati_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getFloati_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5753),
  rec(5753).

-spec getDoublei_v(Target, Index) -> [float()] when Target :: enum(),Index :: integer().
getDoublei_v(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5754),
  rec(5754).

-spec drawArraysInstancedBaseInstance(Mode, First, Count, Instancecount, Baseinstance) -> 'ok' when Mode :: enum(),First :: integer(),Count :: integer(),Instancecount :: integer(),Baseinstance :: integer().
drawArraysInstancedBaseInstance(Mode,First,Count,Instancecount,Baseinstance) when is_integer(Mode),is_integer(First),is_integer(Count),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,First,Count,Instancecount,Baseinstance,5755),
  ok.

-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Instancecount, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Instancecount :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseInstance(Mode,Count,Type,Indices,Instancecount,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Baseinstance,5756),
  ok.

-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Instancecount, Basevertex, Baseinstance) -> 'ok' when Mode :: enum(),Count :: integer(),Type :: enum(),Indices :: offset()|mem(),Instancecount :: integer(),Basevertex :: integer(),Baseinstance :: integer().
drawElementsInstancedBaseVertexBaseInstance(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance) when is_integer(Mode),is_integer(Count),is_integer(Type),is_integer(Indices) orelse is_tuple(Indices) orelse is_binary(Indices),is_integer(Instancecount),is_integer(Basevertex),is_integer(Baseinstance) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Count,Type,Indices,Instancecount,Basevertex,Baseinstance,5758),
  ok.

-spec getInternalformativ(Target, Internalformat, Pname, BufSize) -> [integer()] when Target :: enum(),Internalformat :: enum(),Pname :: enum(),BufSize :: integer().
getInternalformativ(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5760),
  rec(5760).

-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> 'ok' when Unit :: integer(),Texture :: integer(),Level :: integer(),Layered :: 0|1,Layer :: integer(),Access :: enum(),Format :: enum().
bindImageTexture(Unit,Texture,Level,Layered,Layer,Access,Format) when is_integer(Unit),is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Access),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,Level,Layered,Layer,Access,Format,5761),
  ok.

-spec memoryBarrier(Barriers) -> 'ok' when Barriers :: integer().
memoryBarrier(Barriers) when is_integer(Barriers) ->
  IF = get_interface(),
  IF:queue_cmd(Barriers,5762),
  ok.

-spec texStorage1D(Target, Levels, Internalformat, Width) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer().
texStorage1D(Target,Levels,Internalformat,Width) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,5763),
  ok.

-spec texStorage2D(Target, Levels, Internalformat, Width, Height) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer().
texStorage2D(Target,Levels,Internalformat,Width,Height) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,5764),
  ok.

-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> 'ok' when Target :: enum(),Levels :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer().
texStorage3D(Target,Levels,Internalformat,Width,Height,Depth) when is_integer(Target),is_integer(Levels),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Levels,Internalformat,Width,Height,Depth,5765),
  ok.

-spec drawTransformFeedbackInstanced(Mode, Id, Instancecount) -> 'ok' when Mode :: enum(),Id :: integer(),Instancecount :: integer().
drawTransformFeedbackInstanced(Mode,Id,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Instancecount,5766),
  ok.

-spec drawTransformFeedbackStreamInstanced(Mode, Id, Stream, Instancecount) -> 'ok' when Mode :: enum(),Id :: integer(),Stream :: integer(),Instancecount :: integer().
drawTransformFeedbackStreamInstanced(Mode,Id,Stream,Instancecount) when is_integer(Mode),is_integer(Id),is_integer(Stream),is_integer(Instancecount) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Id,Stream,Instancecount,5767),
  ok.

-spec clearBufferData(Target, Internalformat, Format, Type, Data) -> 'ok' when Target :: enum(),Internalformat :: enum(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
clearBufferData(Target,Internalformat,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Format,Type,Data,5768),
  ok.

-spec clearBufferSubData(Target, Internalformat, Offset, Size, Format, Type, Data) -> 'ok' when Target :: enum(),Internalformat :: enum(),Offset :: integer(),Size :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
clearBufferSubData(Target,Internalformat,Offset,Size,Format,Type,Data) when is_integer(Target),is_integer(Internalformat),is_integer(Offset),is_integer(Size),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Offset,Size,Format,Type,Data,5770),
  ok.

-spec dispatchCompute(Num_groups_x, Num_groups_y, Num_groups_z) -> 'ok' when Num_groups_x :: integer(),Num_groups_y :: integer(),Num_groups_z :: integer().
dispatchCompute(Num_groups_x,Num_groups_y,Num_groups_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,5772),
  ok.

-spec dispatchComputeIndirect(Indirect) -> 'ok' when Indirect :: integer().
dispatchComputeIndirect(Indirect) when is_integer(Indirect) ->
  IF = get_interface(),
  IF:queue_cmd(Indirect,5773),
  ok.

-spec copyImageSubData(SrcName, SrcTarget, SrcLevel, SrcX, SrcY, SrcZ, DstName, DstTarget, DstLevel, DstX, DstY, DstZ, SrcWidth, SrcHeight, SrcDepth) -> 'ok' when SrcName :: integer(),SrcTarget :: enum(),SrcLevel :: integer(),SrcX :: integer(),SrcY :: integer(),SrcZ :: integer(),DstName :: integer(),DstTarget :: enum(),DstLevel :: integer(),DstX :: integer(),DstY :: integer(),DstZ :: integer(),SrcWidth :: integer(),SrcHeight :: integer(),SrcDepth :: integer().
copyImageSubData(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth) when is_integer(SrcName),is_integer(SrcTarget),is_integer(SrcLevel),is_integer(SrcX),is_integer(SrcY),is_integer(SrcZ),is_integer(DstName),is_integer(DstTarget),is_integer(DstLevel),is_integer(DstX),is_integer(DstY),is_integer(DstZ),is_integer(SrcWidth),is_integer(SrcHeight),is_integer(SrcDepth) ->
  IF = get_interface(),
  IF:queue_cmd(SrcName,SrcTarget,SrcLevel,SrcX,SrcY,SrcZ,DstName,DstTarget,DstLevel,DstX,DstY,DstZ,SrcWidth,SrcHeight,SrcDepth,5774),
  ok.

-spec framebufferParameteri(Target, Pname, Param) -> 'ok' when Target :: enum(),Pname :: enum(),Param :: integer().
framebufferParameteri(Target,Pname,Param) when is_integer(Target),is_integer(Pname),is_integer(Param) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Param,5775),
  ok.

-spec getFramebufferParameteriv(Target, Pname) -> integer() when Target :: enum(),Pname :: enum().
getFramebufferParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5776),
  rec(5776).

-spec getInternalformati64v(Target, Internalformat, Pname, BufSize) -> [integer()] when Target :: enum(),Internalformat :: enum(),Pname :: enum(),BufSize :: integer().
getInternalformati64v(Target,Internalformat,Pname,BufSize) when is_integer(Target),is_integer(Internalformat),is_integer(Pname),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Pname,BufSize,5777),
  rec(5777).

-spec invalidateTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth) -> 'ok' when Texture :: integer(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer().
invalidateTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,5778),
  ok.

-spec invalidateTexImage(Texture, Level) -> 'ok' when Texture :: integer(),Level :: integer().
invalidateTexImage(Texture,Level) when is_integer(Texture),is_integer(Level) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,5779),
  ok.

-spec invalidateBufferSubData(Buffer, Offset, Length) -> 'ok' when Buffer :: integer(),Offset :: integer(),Length :: integer().
invalidateBufferSubData(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5780),
  ok.

-spec invalidateBufferData(Buffer) -> 'ok' when Buffer :: integer().
invalidateBufferData(Buffer) when is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,5781),
  ok.

-spec invalidateFramebuffer(Target, Attachments) -> 'ok' when Target :: enum(),Attachments :: [enum()].
invalidateFramebuffer(Target,Attachments) when is_integer(Target),is_list(Attachments) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,5782),
  ok.

-spec invalidateSubFramebuffer(Target, Attachments, X, Y, Width, Height) -> 'ok' when Target :: enum(),Attachments :: [enum()],X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
invalidateSubFramebuffer(Target,Attachments,X,Y,Width,Height) when is_integer(Target),is_list(Attachments),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  NumAttachments = length(Attachments),
  IF:queue_cmd(Target,NumAttachments,Attachments,X,Y,Width,Height,5783),
  ok.

-spec multiDrawArraysIndirect(Mode, Indirect, Drawcount, Stride) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem(),Drawcount :: integer(),Stride :: integer().
multiDrawArraysIndirect(Mode,Indirect,Drawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Stride,5784),
  ok.

-spec getProgramInterfaceiv(Program, ProgramInterface, Pname) -> integer() when Program :: integer(),ProgramInterface :: enum(),Pname :: enum().
getProgramInterfaceiv(Program,ProgramInterface,Pname) when is_integer(Program),is_integer(ProgramInterface),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Pname,5786),
  rec(5786).

-spec getProgramResourceIndex(Program, ProgramInterface, Name) -> integer() when Program :: integer(),ProgramInterface :: enum(),Name :: string().
getProgramResourceIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5787),
  rec(5787).

-spec getProgramResourceName(Program, ProgramInterface, Index, BufSize) -> string() when Program :: integer(),ProgramInterface :: enum(),Index :: integer(),BufSize :: integer().
getProgramResourceName(Program,ProgramInterface,Index,BufSize) when is_integer(Program),is_integer(ProgramInterface),is_integer(Index),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Program,ProgramInterface,Index,BufSize,5788),
  rec(5788).

-spec getProgramResourceLocation(Program, ProgramInterface, Name) -> integer() when Program :: integer(),ProgramInterface :: enum(),Name :: string().
getProgramResourceLocation(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5789),
  rec(5789).

-spec getProgramResourceLocationIndex(Program, ProgramInterface, Name) -> integer() when Program :: integer(),ProgramInterface :: enum(),Name :: string().
getProgramResourceLocationIndex(Program,ProgramInterface,Name) when is_integer(Program),is_integer(ProgramInterface),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(Program,ProgramInterface,NameBin,5790),
  rec(5790).

-spec shaderStorageBlockBinding(Program, StorageBlockIndex, StorageBlockBinding) -> 'ok' when Program :: integer(),StorageBlockIndex :: integer(),StorageBlockBinding :: integer().
shaderStorageBlockBinding(Program,StorageBlockIndex,StorageBlockBinding) when is_integer(Program),is_integer(StorageBlockIndex),is_integer(StorageBlockBinding) ->
  IF = get_interface(),
  IF:queue_cmd(Program,StorageBlockIndex,StorageBlockBinding,5791),
  ok.

-spec texBufferRange(Target, Internalformat, Buffer, Offset, Size) -> 'ok' when Target :: enum(),Internalformat :: enum(),Buffer :: integer(),Offset :: integer(),Size :: integer().
texBufferRange(Target,Internalformat,Buffer,Offset,Size) when is_integer(Target),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Buffer,Offset,Size,5792),
  ok.

-spec texStorage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Fixedsamplelocations :: 0|1.
texStorage2DMultisample(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Fixedsamplelocations,5793),
  ok.

-spec texStorage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) -> 'ok' when Target :: enum(),Samples :: integer(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Depth :: integer(),Fixedsamplelocations :: 0|1.
texStorage3DMultisample(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations) when is_integer(Target),is_integer(Samples),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Fixedsamplelocations) orelse (1 =:= Fixedsamplelocations) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Samples,Internalformat,Width,Height,Depth,Fixedsamplelocations,5794),
  ok.

-spec textureView(Texture, Target, Origtexture, Internalformat, Minlevel, Numlevels, Minlayer, Numlayers) -> 'ok' when Texture :: integer(),Target :: enum(),Origtexture :: integer(),Internalformat :: enum(),Minlevel :: integer(),Numlevels :: integer(),Minlayer :: integer(),Numlayers :: integer().
textureView(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers) when is_integer(Texture),is_integer(Target),is_integer(Origtexture),is_integer(Internalformat),is_integer(Minlevel),is_integer(Numlevels),is_integer(Minlayer),is_integer(Numlayers) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Target,Origtexture,Internalformat,Minlevel,Numlevels,Minlayer,Numlayers,5795),
  ok.

-spec bindVertexBuffer(Bindingindex, Buffer, Offset, Stride) -> 'ok' when Bindingindex :: integer(),Buffer :: integer(),Offset :: integer(),Stride :: integer().
bindVertexBuffer(Bindingindex,Buffer,Offset,Stride) when is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Buffer,Offset,Stride,5796),
  ok.

-spec vertexAttribFormat(Attribindex, Size, Type, Normalized, Relativeoffset) -> 'ok' when Attribindex :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Relativeoffset :: integer().
vertexAttribFormat(Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Normalized,Relativeoffset,5797),
  ok.

-spec vertexAttribIFormat(Attribindex, Size, Type, Relativeoffset) -> 'ok' when Attribindex :: integer(),Size :: integer(),Type :: enum(),Relativeoffset :: integer().
vertexAttribIFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5798),
  ok.

-spec vertexAttribLFormat(Attribindex, Size, Type, Relativeoffset) -> 'ok' when Attribindex :: integer(),Size :: integer(),Type :: enum(),Relativeoffset :: integer().
vertexAttribLFormat(Attribindex,Size,Type,Relativeoffset) when is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Size,Type,Relativeoffset,5799),
  ok.

-spec vertexAttribBinding(Attribindex, Bindingindex) -> 'ok' when Attribindex :: integer(),Bindingindex :: integer().
vertexAttribBinding(Attribindex,Bindingindex) when is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Attribindex,Bindingindex,5800),
  ok.

-spec vertexBindingDivisor(Bindingindex, Divisor) -> 'ok' when Bindingindex :: integer(),Divisor :: integer().
vertexBindingDivisor(Bindingindex,Divisor) when is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Bindingindex,Divisor,5801),
  ok.

-spec debugMessageControl(Source, Type, Severity, Ids, Enabled) -> 'ok' when Source :: enum(),Type :: enum(),Severity :: enum(),Ids :: [integer()],Enabled :: 0|1.
debugMessageControl(Source,Type,Severity,Ids,Enabled) when is_integer(Source),is_integer(Type),is_integer(Severity),is_list(Ids),(0 =:= Enabled) orelse (1 =:= Enabled) ->
  IF = get_interface(),
  Count = length(Ids),
  IF:queue_cmd(Source,Type,Severity,Count,Ids,Enabled,5802),
  ok.

-spec debugMessageInsert(Source, Type, Id, Severity, Length, Buf) -> 'ok' when Source :: enum(),Type :: enum(),Id :: integer(),Severity :: enum(),Length :: integer(),Buf :: string().
debugMessageInsert(Source,Type,Id,Severity,Length,Buf) when is_integer(Source),is_integer(Type),is_integer(Id),is_integer(Severity),is_integer(Length),is_list(Buf) ->
  IF = get_interface(),
  BufBin = unicode:characters_to_binary([Buf|[0]]),
  IF:queue_cmd(Source,Type,Id,Severity,Length,BufBin,5803),
  ok.

-spec getDebugMessageLog(Count, BufSize) -> {integer(),Sources :: [enum()],Types :: [enum()],Ids :: [integer()],Severities :: [enum()],MessageLog :: string()} when Count :: integer(),BufSize :: integer().
getDebugMessageLog(Count,BufSize) when is_integer(Count),is_integer(BufSize) ->
  IF = get_interface(),
  IF:queue_cmd(Count,BufSize,5804),
  rec(5804).

-spec pushDebugGroup(Source, Id, Length, Message) -> 'ok' when Source :: enum(),Id :: integer(),Length :: integer(),Message :: string().
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

-spec objectPtrLabel(Ptr, Length, Label) -> 'ok' when Ptr :: offset()|mem(),Length :: integer(),Label :: string().
objectPtrLabel(Ptr,Length,Label) when is_integer(Ptr) orelse is_tuple(Ptr) orelse is_binary(Ptr),is_integer(Length),is_list(Label) ->
  IF = get_interface(),
  LabelBin = unicode:characters_to_binary([Label|[0]]),
  IF:queue_cmd(Ptr,Length,LabelBin,5807),
  ok.

-spec bufferStorage(Target, Size, Data, Flags) -> 'ok' when Target :: enum(),Size :: integer(),Data :: offset()|mem(),Flags :: integer().
bufferStorage(Target,Size,Data,Flags) when is_integer(Target),is_integer(Size),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data),is_integer(Flags) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Size,Data,Flags,5809),
  ok.

-spec clearTexImage(Texture, Level, Format, Type, Data) -> 'ok' when Texture :: integer(),Level :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
clearTexImage(Texture,Level,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Format,Type,Data,5811),
  ok.

-spec clearTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type, Data) -> 'ok' when Texture :: integer(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
clearTexSubImage(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,Type,Data,5813),
  ok.

-spec bindBuffersBase(Target, First, Buffers) -> 'ok' when Target :: enum(),First :: integer(),Buffers :: [integer()].
bindBuffersBase(Target,First,Buffers) when is_integer(Target),is_integer(First),is_list(Buffers) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,5815),
  ok.

-spec bindBuffersRange(Target, First, Buffers, Offsets, Sizes) -> 'ok' when Target :: enum(),First :: integer(),Buffers :: [integer()],Offsets :: [integer()],Sizes :: [integer()].
bindBuffersRange(Target,First,Buffers,Offsets,Sizes) when is_integer(Target),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Sizes) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Target,First,Count,Buffers,Offsets,Sizes,5816),
  ok.

-spec bindTextures(First, Textures) -> 'ok' when First :: integer(),Textures :: [integer()].
bindTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5817),
  ok.

-spec bindSamplers(First, Samplers) -> 'ok' when First :: integer(),Samplers :: [integer()].
bindSamplers(First,Samplers) when is_integer(First),is_list(Samplers) ->
  IF = get_interface(),
  Count = length(Samplers),
  IF:queue_cmd(First,Count,Samplers,5818),
  ok.

-spec bindImageTextures(First, Textures) -> 'ok' when First :: integer(),Textures :: [integer()].
bindImageTextures(First,Textures) when is_integer(First),is_list(Textures) ->
  IF = get_interface(),
  Count = length(Textures),
  IF:queue_cmd(First,Count,Textures,5819),
  ok.

-spec bindVertexBuffers(First, Buffers, Offsets, Strides) -> 'ok' when First :: integer(),Buffers :: [integer()],Offsets :: [integer()],Strides :: [integer()].
bindVertexBuffers(First,Buffers,Offsets,Strides) when is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(First,Count,Buffers,Offsets,Strides,5820),
  ok.

-spec clipControl(Origin, Depth) -> 'ok' when Origin :: enum(),Depth :: enum().
clipControl(Origin,Depth) when is_integer(Origin),is_integer(Depth) ->
  IF = get_interface(),
  IF:queue_cmd(Origin,Depth,5821),
  ok.

-spec createTransformFeedbacks(N) -> [integer()] when N :: integer().
createTransformFeedbacks(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5822),
  rec(5822).

-spec transformFeedbackBufferBase(Xfb, Index, Buffer) -> 'ok' when Xfb :: integer(),Index :: integer(),Buffer :: integer().
transformFeedbackBufferBase(Xfb,Index,Buffer) when is_integer(Xfb),is_integer(Index),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,5823),
  ok.

-spec transformFeedbackBufferRange(Xfb, Index, Buffer, Offset, Size) -> 'ok' when Xfb :: integer(),Index :: integer(),Buffer :: integer(),Offset :: integer(),Size :: integer().
transformFeedbackBufferRange(Xfb,Index,Buffer,Offset,Size) when is_integer(Xfb),is_integer(Index),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Xfb,Index,Buffer,Offset,Size,5824),
  ok.

-spec createBuffers(N) -> [integer()] when N :: integer().
createBuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5825),
  rec(5825).

-spec flushMappedNamedBufferRange(Buffer, Offset, Length) -> 'ok' when Buffer :: integer(),Offset :: integer(),Length :: integer().
flushMappedNamedBufferRange(Buffer,Offset,Length) when is_integer(Buffer),is_integer(Offset),is_integer(Length) ->
  IF = get_interface(),
  IF:queue_cmd(Buffer,Offset,Length,5826),
  ok.

-spec createFramebuffers(N) -> [integer()] when N :: integer().
createFramebuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5827),
  rec(5827).

-spec createRenderbuffers(N) -> [integer()] when N :: integer().
createRenderbuffers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5828),
  rec(5828).

-spec createTextures(Target, N) -> [integer()] when Target :: enum(),N :: integer().
createTextures(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5829),
  rec(5829).

-spec textureBuffer(Texture, Internalformat, Buffer) -> 'ok' when Texture :: integer(),Internalformat :: enum(),Buffer :: integer().
textureBuffer(Texture,Internalformat,Buffer) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,5830),
  ok.

-spec textureBufferRange(Texture, Internalformat, Buffer, Offset, Size) -> 'ok' when Texture :: integer(),Internalformat :: enum(),Buffer :: integer(),Offset :: integer(),Size :: integer().
textureBufferRange(Texture,Internalformat,Buffer,Offset,Size) when is_integer(Texture),is_integer(Internalformat),is_integer(Buffer),is_integer(Offset),is_integer(Size) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Internalformat,Buffer,Offset,Size,5831),
  ok.

-spec compressedTextureSubImage1D(Texture, Level, Xoffset, Width, Format, ImageSize, Data) -> 'ok' when Texture :: integer(),Level :: integer(),Xoffset :: integer(),Width :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTextureSubImage1D(Texture,Level,Xoffset,Width,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Width),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Width,Format,ImageSize,Data,5832),
  ok.

-spec compressedTextureSubImage2D(Texture, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) -> 'ok' when Texture :: integer(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTextureSubImage2D(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Width,Height,Format,ImageSize,Data,5834),
  ok.

-spec compressedTextureSubImage3D(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, ImageSize, Data) -> 'ok' when Texture :: integer(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),ImageSize :: integer(),Data :: offset()|mem().
compressedTextureSubImage3D(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data) when is_integer(Texture),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),is_integer(Format),is_integer(ImageSize),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Format,ImageSize,Data,5836),
  ok.

-spec generateTextureMipmap(Texture) -> 'ok' when Texture :: integer().
generateTextureMipmap(Texture) when is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,5838),
  ok.

-spec bindTextureUnit(Unit, Texture) -> 'ok' when Unit :: integer(),Texture :: integer().
bindTextureUnit(Unit,Texture) when is_integer(Unit),is_integer(Texture) ->
  IF = get_interface(),
  IF:queue_cmd(Unit,Texture,5839),
  ok.

-spec createVertexArrays(N) -> [integer()] when N :: integer().
createVertexArrays(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5840),
  rec(5840).

-spec disableVertexArrayAttrib(Vaobj, Index) -> 'ok' when Vaobj :: integer(),Index :: integer().
disableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5841),
  ok.

-spec enableVertexArrayAttrib(Vaobj, Index) -> 'ok' when Vaobj :: integer(),Index :: integer().
enableVertexArrayAttrib(Vaobj,Index) when is_integer(Vaobj),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Index,5842),
  ok.

-spec vertexArrayElementBuffer(Vaobj, Buffer) -> 'ok' when Vaobj :: integer(),Buffer :: integer().
vertexArrayElementBuffer(Vaobj,Buffer) when is_integer(Vaobj),is_integer(Buffer) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Buffer,5843),
  ok.

-spec vertexArrayVertexBuffer(Vaobj, Bindingindex, Buffer, Offset, Stride) -> 'ok' when Vaobj :: integer(),Bindingindex :: integer(),Buffer :: integer(),Offset :: integer(),Stride :: integer().
vertexArrayVertexBuffer(Vaobj,Bindingindex,Buffer,Offset,Stride) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Buffer),is_integer(Offset),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Buffer,Offset,Stride,5844),
  ok.

-spec vertexArrayVertexBuffers(Vaobj, First, Buffers, Offsets, Strides) -> 'ok' when Vaobj :: integer(),First :: integer(),Buffers :: [integer()],Offsets :: [integer()],Strides :: [integer()].
vertexArrayVertexBuffers(Vaobj,First,Buffers,Offsets,Strides) when is_integer(Vaobj),is_integer(First),is_list(Buffers),is_list(Offsets),is_list(Strides) ->
  IF = get_interface(),
  Count = length(Buffers),
  IF:queue_cmd(Vaobj,First,Count,Buffers,Offsets,Strides,5845),
  ok.

-spec vertexArrayAttribBinding(Vaobj, Attribindex, Bindingindex) -> 'ok' when Vaobj :: integer(),Attribindex :: integer(),Bindingindex :: integer().
vertexArrayAttribBinding(Vaobj,Attribindex,Bindingindex) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Bindingindex) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Bindingindex,5846),
  ok.

-spec vertexArrayAttribFormat(Vaobj, Attribindex, Size, Type, Normalized, Relativeoffset) -> 'ok' when Vaobj :: integer(),Attribindex :: integer(),Size :: integer(),Type :: enum(),Normalized :: 0|1,Relativeoffset :: integer().
vertexArrayAttribFormat(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),(0 =:= Normalized) orelse (1 =:= Normalized),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Normalized,Relativeoffset,5847),
  ok.

-spec vertexArrayAttribIFormat(Vaobj, Attribindex, Size, Type, Relativeoffset) -> 'ok' when Vaobj :: integer(),Attribindex :: integer(),Size :: integer(),Type :: enum(),Relativeoffset :: integer().
vertexArrayAttribIFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5848),
  ok.

-spec vertexArrayAttribLFormat(Vaobj, Attribindex, Size, Type, Relativeoffset) -> 'ok' when Vaobj :: integer(),Attribindex :: integer(),Size :: integer(),Type :: enum(),Relativeoffset :: integer().
vertexArrayAttribLFormat(Vaobj,Attribindex,Size,Type,Relativeoffset) when is_integer(Vaobj),is_integer(Attribindex),is_integer(Size),is_integer(Type),is_integer(Relativeoffset) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Attribindex,Size,Type,Relativeoffset,5849),
  ok.

-spec vertexArrayBindingDivisor(Vaobj, Bindingindex, Divisor) -> 'ok' when Vaobj :: integer(),Bindingindex :: integer(),Divisor :: integer().
vertexArrayBindingDivisor(Vaobj,Bindingindex,Divisor) when is_integer(Vaobj),is_integer(Bindingindex),is_integer(Divisor) ->
  IF = get_interface(),
  IF:queue_cmd(Vaobj,Bindingindex,Divisor,5850),
  ok.

-spec createSamplers(N) -> [integer()] when N :: integer().
createSamplers(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5851),
  rec(5851).

-spec createProgramPipelines(N) -> [integer()] when N :: integer().
createProgramPipelines(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5852),
  rec(5852).

-spec createQueries(Target, N) -> [integer()] when Target :: enum(),N :: integer().
createQueries(Target,N) when is_integer(Target),is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(Target,N,5853),
  rec(5853).

-spec getQueryBufferObjecti64v(Id, Buffer, Pname, Offset) -> 'ok' when Id :: integer(),Buffer :: integer(),Pname :: enum(),Offset :: integer().
getQueryBufferObjecti64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5854),
  ok.

-spec getQueryBufferObjectiv(Id, Buffer, Pname, Offset) -> 'ok' when Id :: integer(),Buffer :: integer(),Pname :: enum(),Offset :: integer().
getQueryBufferObjectiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5855),
  ok.

-spec getQueryBufferObjectui64v(Id, Buffer, Pname, Offset) -> 'ok' when Id :: integer(),Buffer :: integer(),Pname :: enum(),Offset :: integer().
getQueryBufferObjectui64v(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5856),
  ok.

-spec getQueryBufferObjectuiv(Id, Buffer, Pname, Offset) -> 'ok' when Id :: integer(),Buffer :: integer(),Pname :: enum(),Offset :: integer().
getQueryBufferObjectuiv(Id,Buffer,Pname,Offset) when is_integer(Id),is_integer(Buffer),is_integer(Pname),is_integer(Offset) ->
  IF = get_interface(),
  IF:queue_cmd(Id,Buffer,Pname,Offset,5857),
  ok.

-spec memoryBarrierByRegion(Barriers) -> 'ok' when Barriers :: integer().
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

-spec multiDrawArraysIndirectCount(Mode, Indirect, Drawcount, Maxdrawcount, Stride) -> 'ok' when Mode :: enum(),Indirect :: offset()|mem(),Drawcount :: integer(),Maxdrawcount :: integer(),Stride :: integer().
multiDrawArraysIndirectCount(Mode,Indirect,Drawcount,Maxdrawcount,Stride) when is_integer(Mode),is_integer(Indirect) orelse is_tuple(Indirect) orelse is_binary(Indirect),is_integer(Drawcount),is_integer(Maxdrawcount),is_integer(Stride) ->
  IF = get_interface(),
  IF:queue_cmd(Mode,Indirect,Drawcount,Maxdrawcount,Stride,5861),
  ok.

-spec polygonOffsetClamp(Factor, Units, Clamp) -> 'ok' when Factor :: float(),Units :: float(),Clamp :: float().
polygonOffsetClamp(Factor,Units,Clamp) when is_float(Factor),is_float(Units),is_float(Clamp) ->
  IF = get_interface(),
  IF:queue_cmd(Factor,Units,Clamp,5863),
  ok.

-spec primitiveBoundingBoxARB(MinX, MinY, MinZ, MinW, MaxX, MaxY, MaxZ, MaxW) -> 'ok' when MinX :: float(),MinY :: float(),MinZ :: float(),MinW :: float(),MaxX :: float(),MaxY :: float(),MaxZ :: float(),MaxW :: float().
primitiveBoundingBoxARB(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW) when is_float(MinX),is_float(MinY),is_float(MinZ),is_float(MinW),is_float(MaxX),is_float(MaxY),is_float(MaxZ),is_float(MaxW) ->
  IF = get_interface(),
  IF:queue_cmd(MinX,MinY,MinZ,MinW,MaxX,MaxY,MaxZ,MaxW,5864),
  ok.

-spec makeTextureHandleResidentARB(Handle) -> 'ok' when Handle :: integer().
makeTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5865),
  ok.

-spec makeTextureHandleNonResidentARB(Handle) -> 'ok' when Handle :: integer().
makeTextureHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5866),
  ok.

-spec getImageHandleARB(Texture, Level, Layered, Layer, Format) -> integer() when Texture :: integer(),Level :: integer(),Layered :: 0|1,Layer :: integer(),Format :: enum().
getImageHandleARB(Texture,Level,Layered,Layer,Format) when is_integer(Texture),is_integer(Level),(0 =:= Layered) orelse (1 =:= Layered),is_integer(Layer),is_integer(Format) ->
  IF = get_interface(),
  IF:queue_cmd(Texture,Level,Layered,Layer,Format,5867),
  rec(5867).

-spec makeImageHandleResidentARB(Handle, Access) -> 'ok' when Handle :: integer(),Access :: enum().
makeImageHandleResidentARB(Handle,Access) when is_integer(Handle),is_integer(Access) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,Access,5868),
  ok.

-spec makeImageHandleNonResidentARB(Handle) -> 'ok' when Handle :: integer().
makeImageHandleNonResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5869),
  ok.

-spec uniformHandleui64ARB(Location, Value) -> 'ok' when Location :: integer(),Value :: integer().
uniformHandleui64ARB(Location,Value) when is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Location,Value,5870),
  ok.

-spec programUniformHandleui64ARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: integer().
programUniformHandleui64ARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_integer(Value) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,Value,5871),
  ok.

-spec isTextureHandleResidentARB(Handle) -> 0|1 when Handle :: integer().
isTextureHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5872),
  rec(5872).

-spec isImageHandleResidentARB(Handle) -> 0|1 when Handle :: integer().
isImageHandleResidentARB(Handle) when is_integer(Handle) ->
  IF = get_interface(),
  IF:queue_cmd(Handle,5873),
  rec(5873).

-spec dispatchComputeGroupSizeARB(Num_groups_x, Num_groups_y, Num_groups_z, Group_size_x, Group_size_y, Group_size_z) -> 'ok' when Num_groups_x :: integer(),Num_groups_y :: integer(),Num_groups_z :: integer(),Group_size_x :: integer(),Group_size_y :: integer(),Group_size_z :: integer().
dispatchComputeGroupSizeARB(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z) when is_integer(Num_groups_x),is_integer(Num_groups_y),is_integer(Num_groups_z),is_integer(Group_size_x),is_integer(Group_size_y),is_integer(Group_size_z) ->
  IF = get_interface(),
  IF:queue_cmd(Num_groups_x,Num_groups_y,Num_groups_z,Group_size_x,Group_size_y,Group_size_z,5874),
  ok.

-spec programStringARB(Target, Format, String) -> 'ok' when Target :: enum(),Format :: enum(),String :: string().
programStringARB(Target,Format,String) when is_integer(Target),is_integer(Format),is_list(String) ->
  IF = get_interface(),
  StringBin = unicode:characters_to_binary([String|[0]]),
  IF:queue_cmd(Target,Format,StringBin,5875),
  ok.

-spec bindProgramARB(Target, Program) -> 'ok' when Target :: enum(),Program :: integer().
bindProgramARB(Target,Program) when is_integer(Target),is_integer(Program) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Program,5876),
  ok.

-spec deleteProgramsARB(Programs) -> 'ok' when Programs :: [integer()].
deleteProgramsARB(Programs) when is_list(Programs) ->
  IF = get_interface(),
  N = length(Programs),
  IF:queue_cmd(N,Programs,5877),
  ok.

-spec genProgramsARB(N) -> [integer()] when N :: integer().
genProgramsARB(N) when is_integer(N) ->
  IF = get_interface(),
  IF:queue_cmd(N,5878),
  rec(5878).

-spec programEnvParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5879),
  ok.

-spec programEnvParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5880),
  ok.

-spec programEnvParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programEnvParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5881),
  ok.

-spec programEnvParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programEnvParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5882),
  ok.

-spec programLocalParameter4dARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4dARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5883),
  ok.

-spec programLocalParameter4dvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4dvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5884),
  ok.

-spec programLocalParameter4fARB(Target, Index, X, Y, Z, W) -> 'ok' when Target :: enum(),Index :: integer(),X :: float(),Y :: float(),Z :: float(),W :: float().
programLocalParameter4fARB(Target,Index,X,Y,Z,W) when is_integer(Target),is_integer(Index),is_float(X),is_float(Y),is_float(Z),is_float(W) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,X,Y,Z,W,5885),
  ok.

-spec programLocalParameter4fvARB(Target, Index, Params) -> 'ok' when Target :: enum(),Index :: integer(),Params :: {float(),float(),float(),float()}.
programLocalParameter4fvARB(Target,Index,Params) when is_integer(Target),is_integer(Index),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,Params,5886),
  ok.

-spec getProgramEnvParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5887),
  rec(5887).

-spec getProgramEnvParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramEnvParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5888),
  rec(5888).

-spec getProgramLocalParameterdvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterdvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5889),
  rec(5889).

-spec getProgramLocalParameterfvARB(Target, Index) -> {float(),float(),float(),float()} when Target :: enum(),Index :: integer().
getProgramLocalParameterfvARB(Target,Index) when is_integer(Target),is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Index,5890),
  rec(5890).

-spec getProgramStringARB(Target, Pname, String) -> 'ok' when Target :: enum(),Pname :: enum(),String :: mem().
getProgramStringARB(Target,Pname,String) when is_integer(Target),is_integer(Pname),is_tuple(String) orelse is_binary(String) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,String,5891),
  rec(5891).

-spec framebufferTextureFaceARB(Target, Attachment, Texture, Level, Face) -> 'ok' when Target :: enum(),Attachment :: enum(),Texture :: integer(),Level :: integer(),Face :: enum().
framebufferTextureFaceARB(Target,Attachment,Texture,Level,Face) when is_integer(Target),is_integer(Attachment),is_integer(Texture),is_integer(Level),is_integer(Face) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Attachment,Texture,Level,Face,5892),
  ok.

-spec uniform1i64ARB(Location, X) -> 'ok' when Location :: integer(),X :: integer().
uniform1i64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5893),
  ok.

-spec uniform2i64ARB(Location, X, Y) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer().
uniform2i64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5894),
  ok.

-spec uniform3i64ARB(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer(),Z :: integer().
uniform3i64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5895),
  ok.

-spec uniform4i64ARB(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
uniform4i64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5896),
  ok.

-spec uniform1i64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5897),
  ok.

-spec uniform2i64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5898),
  ok.

-spec uniform3i64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5899),
  ok.

-spec uniform4i64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4i64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5900),
  ok.

-spec uniform1ui64ARB(Location, X) -> 'ok' when Location :: integer(),X :: integer().
uniform1ui64ARB(Location,X) when is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,5901),
  ok.

-spec uniform2ui64ARB(Location, X, Y) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer().
uniform2ui64ARB(Location,X,Y) when is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,5902),
  ok.

-spec uniform3ui64ARB(Location, X, Y, Z) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer(),Z :: integer().
uniform3ui64ARB(Location,X,Y,Z) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,5903),
  ok.

-spec uniform4ui64ARB(Location, X, Y, Z, W) -> 'ok' when Location :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
uniform4ui64ARB(Location,X,Y,Z,W) when is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Location,X,Y,Z,W,5904),
  ok.

-spec uniform1ui64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [integer()].
uniform1ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5905),
  ok.

-spec uniform2ui64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer()}].
uniform2ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5906),
  ok.

-spec uniform3ui64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer()}].
uniform3ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5907),
  ok.

-spec uniform4ui64vARB(Location, Value) -> 'ok' when Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
uniform4ui64vARB(Location,Value) when is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Location,Count,Value,5908),
  ok.

-spec getUniformi64vARB(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformi64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5909),
  rec(5909).

-spec getUniformui64vARB(Program, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when Program :: integer(),Location :: integer().
getUniformui64vARB(Program,Location) when is_integer(Program),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,5910),
  rec(5910).

-spec programUniform1i64ARB(Program, Location, X) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer().
programUniform1i64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5911),
  ok.

-spec programUniform2i64ARB(Program, Location, X, Y) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer().
programUniform2i64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5912),
  ok.

-spec programUniform3i64ARB(Program, Location, X, Y, Z) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer(),Z :: integer().
programUniform3i64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5913),
  ok.

-spec programUniform4i64ARB(Program, Location, X, Y, Z, W) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
programUniform4i64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5914),
  ok.

-spec programUniform1i64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5915),
  ok.

-spec programUniform2i64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5916),
  ok.

-spec programUniform3i64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5917),
  ok.

-spec programUniform4i64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4i64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5918),
  ok.

-spec programUniform1ui64ARB(Program, Location, X) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer().
programUniform1ui64ARB(Program,Location,X) when is_integer(Program),is_integer(Location),is_integer(X) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,5919),
  ok.

-spec programUniform2ui64ARB(Program, Location, X, Y) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer().
programUniform2ui64ARB(Program,Location,X,Y) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,5920),
  ok.

-spec programUniform3ui64ARB(Program, Location, X, Y, Z) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer(),Z :: integer().
programUniform3ui64ARB(Program,Location,X,Y,Z) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,5921),
  ok.

-spec programUniform4ui64ARB(Program, Location, X, Y, Z, W) -> 'ok' when Program :: integer(),Location :: integer(),X :: integer(),Y :: integer(),Z :: integer(),W :: integer().
programUniform4ui64ARB(Program,Location,X,Y,Z,W) when is_integer(Program),is_integer(Location),is_integer(X),is_integer(Y),is_integer(Z),is_integer(W) ->
  IF = get_interface(),
  IF:queue_cmd(Program,Location,X,Y,Z,W,5922),
  ok.

-spec programUniform1ui64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [integer()].
programUniform1ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5923),
  ok.

-spec programUniform2ui64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer()}].
programUniform2ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5924),
  ok.

-spec programUniform3ui64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer()}].
programUniform3ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5925),
  ok.

-spec programUniform4ui64vARB(Program, Location, Value) -> 'ok' when Program :: integer(),Location :: integer(),Value :: [{integer(),integer(),integer(),integer()}].
programUniform4ui64vARB(Program,Location,Value) when is_integer(Program),is_integer(Location),is_list(Value) ->
  IF = get_interface(),
  Count = length(Value),
  IF:queue_cmd(Program,Location,Count,Value,5926),
  ok.

-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Table :: offset()|mem().
colorTable(Target,Internalformat,Width,Format,Type,Table) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Table) orelse is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Table,5927),
  ok.

-spec colorTableParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {float(),float(),float(),float()}.
colorTableParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5929),
  ok.

-spec colorTableParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: {integer(),integer(),integer(),integer()}.
colorTableParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),tuple_size(Params) =:= 4 ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5930),
  ok.

-spec copyColorTable(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyColorTable(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5931),
  ok.

-spec getColorTable(Target, Format, Type, Table) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Table :: mem().
getColorTable(Target,Format,Type,Table) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Table) orelse is_binary(Table) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Table,5932),
  rec(5932).

-spec getColorTableParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getColorTableParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5933),
  rec(5933).

-spec getColorTableParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getColorTableParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5934),
  rec(5934).

-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> 'ok' when Target :: enum(),Start :: integer(),Count :: integer(),Format :: enum(),Type :: enum(),Data :: offset()|mem().
colorSubTable(Target,Start,Count,Format,Type,Data) when is_integer(Target),is_integer(Start),is_integer(Count),is_integer(Format),is_integer(Type),is_integer(Data) orelse is_tuple(Data) orelse is_binary(Data) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,Count,Format,Type,Data,5935),
  ok.

-spec copyColorSubTable(Target, Start, X, Y, Width) -> 'ok' when Target :: enum(),Start :: integer(),X :: integer(),Y :: integer(),Width :: integer().
copyColorSubTable(Target,Start,X,Y,Width) when is_integer(Target),is_integer(Start),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Start,X,Y,Width,5937),
  ok.

-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter1D(Target,Internalformat,Width,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Format,Type,Image,5938),
  ok.

-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Image :: offset()|mem().
convolutionFilter2D(Target,Internalformat,Width,Height,Format,Type,Image) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Image) orelse is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Image,5940),
  ok.

-spec convolutionParameterf(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameterf(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5942),
  ok.

-spec convolutionParameterfv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameterfv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5943),
  ok.

-spec convolutionParameteri(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameteri(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5944),
  ok.

-spec convolutionParameteriv(Target, Pname, Params) -> 'ok' when Target :: enum(),Pname :: enum(),Params :: tuple().
convolutionParameteriv(Target,Pname,Params) when is_integer(Target),is_integer(Pname),is_tuple(Params) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,Params,5945),
  ok.

-spec copyConvolutionFilter1D(Target, Internalformat, X, Y, Width) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer().
copyConvolutionFilter1D(Target,Internalformat,X,Y,Width) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,5946),
  ok.

-spec copyConvolutionFilter2D(Target, Internalformat, X, Y, Width, Height) -> 'ok' when Target :: enum(),Internalformat :: enum(),X :: integer(),Y :: integer(),Width :: integer(),Height :: integer().
copyConvolutionFilter2D(Target,Internalformat,X,Y,Width,Height) when is_integer(Target),is_integer(Internalformat),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,X,Y,Width,Height,5947),
  ok.

-spec getConvolutionFilter(Target, Format, Type, Image) -> 'ok' when Target :: enum(),Format :: enum(),Type :: enum(),Image :: mem().
getConvolutionFilter(Target,Format,Type,Image) when is_integer(Target),is_integer(Format),is_integer(Type),is_tuple(Image) orelse is_binary(Image) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Format,Type,Image,5948),
  rec(5948).

-spec getConvolutionParameterfv(Target, Pname) -> {float(),float(),float(),float()} when Target :: enum(),Pname :: enum().
getConvolutionParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5949),
  rec(5949).

-spec getConvolutionParameteriv(Target, Pname) -> {integer(),integer(),integer(),integer()} when Target :: enum(),Pname :: enum().
getConvolutionParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5950),
  rec(5950).

-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> 'ok' when Target :: enum(),Internalformat :: enum(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Row :: offset()|mem(),Column :: offset()|mem().
separableFilter2D(Target,Internalformat,Width,Height,Format,Type,Row,Column) when is_integer(Target),is_integer(Internalformat),is_integer(Width),is_integer(Height),is_integer(Format),is_integer(Type),is_integer(Row) orelse is_tuple(Row) orelse is_binary(Row),is_integer(Column) orelse is_tuple(Column) orelse is_binary(Column) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Width,Height,Format,Type,Row,Column,5951),
  ok.

-spec getHistogram(Target, Reset, Format, Type, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Type :: enum(),Values :: mem().
getHistogram(Target,Reset,Format,Type,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Type),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Type,Values,5953),
  rec(5953).

-spec getHistogramParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getHistogramParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5954),
  rec(5954).

-spec getHistogramParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getHistogramParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5955),
  rec(5955).

-spec getMinmax(Target, Reset, Format, Types, Values) -> 'ok' when Target :: enum(),Reset :: 0|1,Format :: enum(),Types :: enum(),Values :: mem().
getMinmax(Target,Reset,Format,Types,Values) when is_integer(Target),(0 =:= Reset) orelse (1 =:= Reset),is_integer(Format),is_integer(Types),is_tuple(Values) orelse is_binary(Values) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Reset,Format,Types,Values,5956),
  rec(5956).

-spec getMinmaxParameterfv(Target, Pname) -> {float()} when Target :: enum(),Pname :: enum().
getMinmaxParameterfv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5957),
  rec(5957).

-spec getMinmaxParameteriv(Target, Pname) -> {integer()} when Target :: enum(),Pname :: enum().
getMinmaxParameteriv(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,5958),
  rec(5958).

-spec histogram(Target, Width, Internalformat, Sink) -> 'ok' when Target :: enum(),Width :: integer(),Internalformat :: enum(),Sink :: 0|1.
histogram(Target,Width,Internalformat,Sink) when is_integer(Target),is_integer(Width),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Width,Internalformat,Sink,5959),
  ok.

-spec minmax(Target, Internalformat, Sink) -> 'ok' when Target :: enum(),Internalformat :: enum(),Sink :: 0|1.
minmax(Target,Internalformat,Sink) when is_integer(Target),is_integer(Internalformat),(0 =:= Sink) orelse (1 =:= Sink) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Internalformat,Sink,5960),
  ok.

-spec resetHistogram(Target) -> 'ok' when Target :: enum().
resetHistogram(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5961),
  ok.

-spec resetMinmax(Target) -> 'ok' when Target :: enum().
resetMinmax(Target) when is_integer(Target) ->
  IF = get_interface(),
  IF:queue_cmd(Target,5962),
  ok.

-spec currentPaletteMatrixARB(Index) -> 'ok' when Index :: integer().
currentPaletteMatrixARB(Index) when is_integer(Index) ->
  IF = get_interface(),
  IF:queue_cmd(Index,5963),
  ok.

-spec matrixIndexubvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexubvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5964),
  ok.

-spec matrixIndexusvARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexusvARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5965),
  ok.

-spec matrixIndexuivARB(Indices) -> 'ok' when Indices :: [integer()].
matrixIndexuivARB(Indices) when is_list(Indices) ->
  IF = get_interface(),
  Size = length(Indices),
  IF:queue_cmd(Size,Indices,5966),
  ok.

-spec sampleCoverageARB(Value, Invert) -> 'ok' when Value :: float(),Invert :: 0|1.
sampleCoverageARB(Value,Invert) when is_float(Value),(0 =:= Invert) orelse (1 =:= Invert) ->
  IF = get_interface(),
  IF:queue_cmd(Value,Invert,5967),
  ok.

-spec maxShaderCompilerThreadsARB(Count) -> 'ok' when Count :: integer().
maxShaderCompilerThreadsARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,5968),
  ok.

-spec evaluateDepthValuesARB() -> 'ok'.
evaluateDepthValuesARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5969),
  ok.

-spec deleteObjectARB(Obj) -> 'ok' when Obj :: integer().
deleteObjectARB(Obj) when is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,5970),
  ok.

-spec getHandleARB(Pname) -> integer() when Pname :: enum().
getHandleARB(Pname) when is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Pname,5971),
  rec(5971).

-spec detachObjectARB(ContainerObj, AttachedObj) -> 'ok' when ContainerObj :: integer(),AttachedObj :: integer().
detachObjectARB(ContainerObj,AttachedObj) when is_integer(ContainerObj),is_integer(AttachedObj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,AttachedObj,5972),
  ok.

-spec createShaderObjectARB(ShaderType) -> integer() when ShaderType :: enum().
createShaderObjectARB(ShaderType) when is_integer(ShaderType) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderType,5973),
  rec(5973).

-spec shaderSourceARB(ShaderObj, String) -> 'ok' when ShaderObj :: integer(),String :: iolist().
shaderSourceARB(ShaderObj,String) when is_integer(ShaderObj),is_list(String) ->
  IF = get_interface(),
  StringTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- String ],
  Count = length(String),
  IF:queue_cmd(ShaderObj,Count,StringTemp,5974),
  ok.

-spec compileShaderARB(ShaderObj) -> 'ok' when ShaderObj :: integer().
compileShaderARB(ShaderObj) when is_integer(ShaderObj) ->
  IF = get_interface(),
  IF:queue_cmd(ShaderObj,5975),
  ok.

-spec createProgramObjectARB() -> integer().
createProgramObjectARB()  ->
  IF = get_interface(),
  IF:queue_cmd(5976),
  rec(5976).

-spec attachObjectARB(ContainerObj, Obj) -> 'ok' when ContainerObj :: integer(),Obj :: integer().
attachObjectARB(ContainerObj,Obj) when is_integer(ContainerObj),is_integer(Obj) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,Obj,5977),
  ok.

-spec linkProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
linkProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5978),
  ok.

-spec useProgramObjectARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
useProgramObjectARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5979),
  ok.

-spec validateProgramARB(ProgramObj) -> 'ok' when ProgramObj :: integer().
validateProgramARB(ProgramObj) when is_integer(ProgramObj) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,5980),
  ok.

-spec getObjectParameterfvARB(Obj, Pname) -> float() when Obj :: integer(),Pname :: enum().
getObjectParameterfvARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5981),
  rec(5981).

-spec getObjectParameterivARB(Obj, Pname) -> integer() when Obj :: integer(),Pname :: enum().
getObjectParameterivARB(Obj,Pname) when is_integer(Obj),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,Pname,5982),
  rec(5982).

-spec getInfoLogARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getInfoLogARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5983),
  rec(5983).

-spec getAttachedObjectsARB(ContainerObj, MaxCount) -> [integer()] when ContainerObj :: integer(),MaxCount :: integer().
getAttachedObjectsARB(ContainerObj,MaxCount) when is_integer(ContainerObj),is_integer(MaxCount) ->
  IF = get_interface(),
  IF:queue_cmd(ContainerObj,MaxCount,5984),
  rec(5984).

-spec getUniformLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
getUniformLocationARB(ProgramObj,Name) when is_integer(ProgramObj),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,NameBin,5985),
  rec(5985).

-spec getActiveUniformARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveUniformARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,5986),
  rec(5986).

-spec getUniformfvARB(ProgramObj, Location) -> matrix() when ProgramObj :: integer(),Location :: integer().
getUniformfvARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5987),
  rec(5987).

-spec getUniformivARB(ProgramObj, Location) -> {integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer(),integer()} when ProgramObj :: integer(),Location :: integer().
getUniformivARB(ProgramObj,Location) when is_integer(ProgramObj),is_integer(Location) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Location,5988),
  rec(5988).

-spec getShaderSourceARB(Obj, MaxLength) -> string() when Obj :: integer(),MaxLength :: integer().
getShaderSourceARB(Obj,MaxLength) when is_integer(Obj),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(Obj,MaxLength,5989),
  rec(5989).

-spec deleteNamedStringARB(Name) -> 'ok' when Name :: string().
deleteNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5990),
  ok.

-spec compileShaderIncludeARB(Shader, Path) -> 'ok' when Shader :: integer(),Path :: iolist().
compileShaderIncludeARB(Shader,Path) when is_integer(Shader),is_list(Path) ->
  IF = get_interface(),
  PathTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- Path ],
  Count = length(Path),
  IF:queue_cmd(Shader,Count,PathTemp,5991),
  ok.

-spec isNamedStringARB(Name) -> 0|1 when Name :: string().
isNamedStringARB(Name) when is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(NameBin,5992),
  rec(5992).

-spec bufferPageCommitmentARB(Target, Offset, Size, Commit) -> 'ok' when Target :: enum(),Offset :: integer(),Size :: integer(),Commit :: 0|1.
bufferPageCommitmentARB(Target,Offset,Size,Commit) when is_integer(Target),is_integer(Offset),is_integer(Size),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Offset,Size,Commit,5993),
  ok.

-spec texPageCommitmentARB(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Commit) -> 'ok' when Target :: enum(),Level :: integer(),Xoffset :: integer(),Yoffset :: integer(),Zoffset :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Commit :: 0|1.
texPageCommitmentARB(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit) when is_integer(Target),is_integer(Level),is_integer(Xoffset),is_integer(Yoffset),is_integer(Zoffset),is_integer(Width),is_integer(Height),is_integer(Depth),(0 =:= Commit) orelse (1 =:= Commit) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Xoffset,Yoffset,Zoffset,Width,Height,Depth,Commit,5994),
  ok.

-spec getCompressedTexImageARB(Target, Level, Img) -> 'ok' when Target :: enum(),Level :: integer(),Img :: mem().
getCompressedTexImageARB(Target,Level,Img) when is_integer(Target),is_integer(Level),is_tuple(Img) orelse is_binary(Img) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Level,Img,5995),
  rec(5995).

-spec loadTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5996),
  ok.

-spec loadTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
loadTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5997),
  ok.

-spec multTransposeMatrixfARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixfARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5998),
  ok.

-spec multTransposeMatrixdARB(M) -> 'ok' when M :: matrix().
multTransposeMatrixdARB(M) when tuple_size(M) =:= 16; tuple_size(M) =:= 12 ->
  IF = get_interface(),
  IF:queue_cmd(M,5999),
  ok.

-spec weightbvARB(Weights) -> 'ok' when Weights :: [integer()].
weightbvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6000),
  ok.

-spec weightsvARB(Weights) -> 'ok' when Weights :: [integer()].
weightsvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6001),
  ok.

-spec weightivARB(Weights) -> 'ok' when Weights :: [integer()].
weightivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6002),
  ok.

-spec weightfvARB(Weights) -> 'ok' when Weights :: [float()].
weightfvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6003),
  ok.

-spec weightdvARB(Weights) -> 'ok' when Weights :: [float()].
weightdvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6004),
  ok.

-spec weightubvARB(Weights) -> 'ok' when Weights :: [integer()].
weightubvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6005),
  ok.

-spec weightusvARB(Weights) -> 'ok' when Weights :: [integer()].
weightusvARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6006),
  ok.

-spec weightuivARB(Weights) -> 'ok' when Weights :: [integer()].
weightuivARB(Weights) when is_list(Weights) ->
  IF = get_interface(),
  Size = length(Weights),
  IF:queue_cmd(Size,Weights,6007),
  ok.

-spec vertexBlendARB(Count) -> 'ok' when Count :: integer().
vertexBlendARB(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6008),
  ok.

-spec getBufferParameterivARB(Target, Pname) -> [integer()] when Target :: enum(),Pname :: enum().
getBufferParameterivARB(Target,Pname) when is_integer(Target),is_integer(Pname) ->
  IF = get_interface(),
  IF:queue_cmd(Target,Pname,6009),
  rec(6009).

-spec bindAttribLocationARB(ProgramObj, Index, Name) -> 'ok' when ProgramObj :: integer(),Index :: integer(),Name :: string().
bindAttribLocationARB(ProgramObj,Index,Name) when is_integer(ProgramObj),is_integer(Index),is_list(Name) ->
  IF = get_interface(),
  NameBin = unicode:characters_to_binary([Name|[0]]),
  IF:queue_cmd(ProgramObj,Index,NameBin,6010),
  ok.

-spec getActiveAttribARB(ProgramObj, Index, MaxLength) -> {Size :: integer(),Type :: enum(),Name :: string()} when ProgramObj :: integer(),Index :: integer(),MaxLength :: integer().
getActiveAttribARB(ProgramObj,Index,MaxLength) when is_integer(ProgramObj),is_integer(Index),is_integer(MaxLength) ->
  IF = get_interface(),
  IF:queue_cmd(ProgramObj,Index,MaxLength,6011),
  rec(6011).

-spec getAttribLocationARB(ProgramObj, Name) -> integer() when ProgramObj :: integer(),Name :: string().
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

-spec maxShaderCompilerThreadsKHR(Count) -> 'ok' when Count :: integer().
maxShaderCompilerThreadsKHR(Count) when is_integer(Count) ->
  IF = get_interface(),
  IF:queue_cmd(Count,6014),
  ok.

-spec depthBoundsEXT(Zmin, Zmax) -> 'ok' when Zmin :: clamp(),Zmax :: clamp().
depthBoundsEXT(Zmin,Zmax) when is_float(Zmin),is_float(Zmax) ->
  IF = get_interface(),
  IF:queue_cmd(Zmin,Zmax,6015),
  ok.

